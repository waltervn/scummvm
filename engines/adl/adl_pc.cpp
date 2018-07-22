/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include "common/error.h"
#include "common/events.h"

#include "adl/adl_pc.h"
#include "adl/display.h"

namespace Adl {

class ScriptEnv_PC : public ScriptEnv {
public:
	ScriptEnv_PC(const Command &cmd, byte room, byte verb, byte noun) :
			ScriptEnv(cmd, room, verb, noun),
			_curOpType(kOpTypeCond) { checkTerm(); }

private:
	void checkTerm() {
		switch(_curOpType) {
		case kOpTypeCond:
			if (op() != 0xff)
				break;

			_curOpType = kOpTypeAct;
			++_ip;
			// Fallthrough
		case kOpTypeAct:
			if (op() != 0xff)
				break;

			_curOpType = kOpTypeDone;
			++_ip;
			break;
		case kOpTypeDone:
			// Do nothing
			break;
		}
	}

	kOpType getOpType() const {
		return _curOpType;
	}

	void next(uint numArgs) {
		_ip += numArgs + 1;
		checkTerm();
	}

	kOpType _curOpType;
};

ScriptEnv *AdlEngine_PC::createScriptEnv(const Command &cmd, byte room, byte verb, byte noun) {
	return new ScriptEnv_PC(cmd, room, verb, noun);
}

enum Key {
	kKeyBackspace = 0x08,
	kKeyReturn = 0x0d,
	kKeyEscape = 0x1b,
	kKeyF3 = 0x80,
	kKeyF4,
	kKeyF5,
	kKeyF6,
	kKeyF1,
	kKeyF2,
	kKeyScrollLock
};

byte AdlEngine_PC::convertEvent(const Common::Event &event) const {
	switch (event.kbd.keycode) {
	case Common::KEYCODE_BACKSPACE:
		return kKeyBackspace;
	case Common::KEYCODE_RETURN:
		return kKeyReturn;
	case Common::KEYCODE_ESCAPE:
		return kKeyEscape;
	case Common::KEYCODE_SCROLLOCK:
		return kKeyScrollLock;
	case Common::KEYCODE_F1:
		return kKeyF1;
	case Common::KEYCODE_F2:
		return kKeyF2;
	case Common::KEYCODE_F3:
		return kKeyF3;
	case Common::KEYCODE_F4:
		return kKeyF4;
	case Common::KEYCODE_F5:
		return kKeyF5;
	case Common::KEYCODE_F6:
		return kKeyF6;
	default:
		if (event.kbd.ascii >= 0x20 && event.kbd.ascii <= 0x7f)
			return event.kbd.ascii;
	}
	return 0;
}

void AdlEngine_PC::newLine() {
	_display->printChar('\r');
	if (_cursorPos.y != 24)
		++_cursorPos.y;
	_cursorPos.x = 2;
	_display->moveCursorTo(_cursorPos);
}

void AdlEngine_PC::printChar(char c) {
	switch (c) {
	case kKeyReturn:
		newLine();
		break;
	case kKeyBackspace:
		_display->setCharAtCursor(' ');
		if (_cursorPos.x == 2) {
			--_cursorPos.y;
			_cursorPos.x = 37;
		} else {
			--_cursorPos.x;
		}
		_display->moveCursorTo(_cursorPos);
		_display->setCharAtCursor(' ');
		break;
	default:
		_display->printChar(c);
		if (_cursorPos.x == 37)
			newLine();
		else {
			++_cursorPos.x;
			_display->moveCursorTo(_cursorPos);
		}
	}

	_display->updateTextScreen();
}

void AdlEngine_PC::printString(const Common::String &str, uint offset, uint len) {
	for (uint i = 0; i < len; ++i)
		printChar(str[offset++]);
}

Common::String AdlEngine_PC::inputString(const Common::String &prompt) {
	Common::String s, fKeyStr;

	printString(prompt);

	_linesPrinted = 0;

	while (1) {
		byte b;

		if (!fKeyStr.empty()) {
			b = fKeyStr.firstChar();
			fKeyStr.deleteChar(0);
		} else {
			_display->setCharAtCursor('_');

			b = inputKey();

			if (shouldQuit() || _isRestoring)
				return 0;

			if (b == 0)
				continue;
		}

		switch (b) {
		case kKeyReturn:
			if (!s.empty()) {
				_display->setCharAtCursor(' ');
				printChar(b);
				return s;
			}
			break;
		case kKeyBackspace:
			if (!s.empty()) {
				s.deleteLastChar();
				printChar(b);
			}
			break;
		case kKeyEscape:
			while (!s.empty()) {
				s.deleteLastChar();
				printChar(kKeyBackspace);
			}
			break;
		case kKeyF1:
			_textMode = !_textMode;
			_display->setMode(_textMode ? Display::kModeText : Display::kModeMixed);
			break;
		case kKeyF2:
			warning("Palette switch not implemented");
			break;
		case kKeyF3:
			fKeyStr = "save game";
			break;
		case kKeyF4:
			fKeyStr = "restore game";
			break;
		case kKeyF5:
			fKeyStr = "inventory";
			break;
		case kKeyF6:
			fKeyStr = "look room";
			break;
		default:
			if (0x20 <= b && b <= 0x7f) {
				if (s.size() < 32) {
					s += b;
					printChar(b);
				} else {
					// TODO: not the right frequency
					bell();
				}
			}
		}
	}
}

uint AdlEngine_PC::countLeadingSpaces(const Common::String &str, uint offset) {
	uint count = 0;

	while (offset + count < str.size() && str[offset + count] == ' ')
		++count;

	return count;
}

void AdlEngine_PC::handleTextOverflow() {
	_display->updateTextScreen();
	bell();

	while (true) {
		byte key = inputKey(false);

		if (shouldQuit())
			return;

		// Original game only accepts scroll lock
		if (key == kKeyScrollLock || key == kKeyReturn)
			break;
	}

	_linesPrinted = 0;
}

uint AdlEngine_PC::findStringWrapLen(const Common::String &str, uint offset) {
	uint maxLen = 38 - _cursorPos.x;
	const uint remSize = str.size() - offset;

	assert(remSize != 0);

	if (maxLen > remSize)
		maxLen = remSize;

	uint curLen = 0;

	while (curLen != maxLen) {
		++curLen;
		if (str[offset + curLen - 1] == '\r')
			break;
	}

	// Carriage return embedded in string, or end-of-string reached
	if (curLen != maxLen || curLen == remSize)
		return curLen;

	while (curLen > 0 && str[offset + curLen - 1] != ' ')
		--curLen;

	if (curLen == 0) {
		warning("Word wrapping failed for '%s'", str.c_str());
		return maxLen;
	}

	while (curLen > 0 && str[offset + curLen - 1] == ' ')
		--curLen;

	assert(curLen != 0);

	return curLen;
}

void AdlEngine_PC::printStringWrap(const Common::String &str, uint offset, uint size) {
	bool addReturn = false;

	if (offset + size != str.size())
		addReturn = true;

	if (str[offset + size - 1] == '\r') {
		--size;
		addReturn = true;
	}

	printString(str, offset, size);

	if (addReturn) {
		if (_textMode) {
			_linesPrinted = 0;
		} else {
			++_linesPrinted;
			if (_linesPrinted == _maxLines)
				handleTextOverflow();
		}

		printChar('\r');
	}
}

void AdlEngine_PC::printString(const Common::String &str) {
	uint offset = countLeadingSpaces(str, 0);

	while (offset < str.size()) {
		const uint lineLen = findStringWrapLen(str, offset);
		printStringWrap(str, offset, lineLen);
		offset += lineLen;
		offset += countLeadingSpaces(str, offset);
	}
}

Common::String AdlEngine_PC::formatVerbError(const Common::String &verb) const {
	Common::String err(_strings.verbError);

	for (uint i = 0; i < verb.size() && verb[i] != ' '; ++i)
		err += verb[i];

	err += "'.\r";
	return err;
}

Common::String AdlEngine_PC::readMessageString(Common::ReadStream &stream) const {
	Common::String str;
	byte size = stream.readByte();

	for (uint i = 0; i < size; ++i)
		str += stream.readByte();

	if (stream.err() || stream.eos())
		error("Failed to read string");

	return str;
}

Common::String AdlEngine_PC::loadMessage(uint idx) const {
	if (_messages[idx]) {
		const char xorString[] = { 'A', 'v', 'i', 's', ' ', 'D', 'u', 'r', 'g', 'a', 'n' };
		StreamPtr stream(_messages[idx]->createReadStream());

		Common::String str;
		int32 size = stream->size();

		for (int32 i = 0; i < size; ++i)
			str += stream->readByte() ^ xorString[i % sizeof(xorString)];

		if (stream->err() || stream->eos())
			error("Failed to read message string");

		return str;
	}

	return Common::String();
}

void AdlEngine_PC::loadItemDescriptions(Common::SeekableReadStream &stream, byte count) {
	int32 startPos = stream.pos();
	uint16 baseAddr = stream.readUint16LE();

	// This code assumes that the first pointer points to a string that
	// directly follows the pointer table
	assert(baseAddr != 0);
	baseAddr -= count * 2;

	for (uint i = 0; i < count; ++i) {
		stream.seek(startPos + i * 2);
		uint16 offset = stream.readUint16LE();

		stream.seek(startPos + offset - baseAddr);
		byte len = stream.readByte();
		Common::String desc;

		for (uint j = 0; j < len; ++j)
			desc += stream.readByte();

		_itemDesc.push_back(desc);
	}

	if (stream.eos() || stream.err())
		error("Error loading item descriptions");
}

void AdlEngine_PC::loadRegionMetaData(Common::SeekableReadStream &stream) {
	if (stream.size() < 16)
		error("No regions found");

	// Region 0 does not exist
	const uint regions = stream.size() / 8 - 1;
	stream.seek(8);

	for (uint r = 0; r < regions; ++r) {
		RegionInitDataOffset initOfs;
		initOfs.track = stream.readByte();
		initOfs.sector = stream.readByte();
		initOfs.offset = stream.readUint16LE();
		initOfs.volume = stream.readByte();

		_regionInitDataOffsets.push_back(initOfs);

		RegionLocation loc;
		loc.track = stream.readByte();
		loc.sector = stream.readByte();

		_regionLocations.push_back(loc);
		_regionRoomCounts.push_back(stream.readByte());
	}

	if (stream.eos() || stream.err())
		error("Failed to read region meta data");
}

void AdlEngine_PC::fixupDiskOffset(byte &track, byte &sector) const {
	if (_state.region == 0) {
		// Hardcoded item pic track offset
		track += 32;
		return;
	}

	sector += _regionLocations[_state.region - 1].sector;
	if (sector > 8) {
		sector -= 8;
		++track;
	}

	track += _regionLocations[_state.region - 1].track;
}

DataBlockPtr AdlEngine_PC::readDataBlockPtr(Common::ReadStream &f) const {
	byte track = f.readByte();
	byte sector = f.readByte();
	uint16 offset = f.readUint16LE();

	if (f.eos() || f.err())
		error("Error reading DataBlockPtr");

	if (track == 0 && sector == 0 && offset == 0)
		return DataBlockPtr();

	fixupDiskOffset(track, sector);

	return DataBlockPtr(new DataBlock_PC(_disk, track, sector, offset));
}

uint AdlEngine_PC::getInputWord(const Common::String &word) const {
	Common::String compareWord(word);

	while (compareWord.size() > 8)
		compareWord.deleteLastChar();

	compareWord.toUppercase();

	if (!_verbs.contains(compareWord))
		return 0;

	return _verbs[compareWord];
}

void AdlEngine_PC::getInput(uint &verb, uint &noun) {
	while (1) {
		Common::String line = inputString(_strings.enterCommand);

		if (shouldQuit() || _isRestoring)
			return;

		uint index = 0;
		Common::String verbString = getWord(line, index, 16);

		verb = getInputWord(verbString);
		if (verb == 0) {
			printString(formatVerbError(verbString));
			continue;
		}

		Common::String nounString = getWord(line, index, 16);
		noun = getInputWord(nounString);
		if (noun == 0) {
			printString(formatVerbError(nounString));
			continue;
		}

		return;
	}
}

void AdlEngine_PC::loadWords(Common::ReadStream &stream) {
	uint index = 1;

	_nouns.clear();
	_verbs.clear();
	_priNouns.clear();
	_priVerbs.clear();

	while (1) {
		byte buf[IDI_WORD_SIZE];

		buf[0] = stream.readByte();

		if (buf[0] == 0xff)
			break;
		else if (buf[0] == 0xfe) {
			++index;
			continue;
		}

		if (stream.read(buf + 1, IDI_WORD_SIZE - 1) < IDI_WORD_SIZE - 1)
			error("Error reading word list");

		Common::String word((char *)buf, IDI_WORD_SIZE);

		if (!_nouns.contains(word)) {
			_nouns[word] = index;
			_verbs[word] = index;
		}

		if (_priNouns.size() == index - 1) {
			_priNouns.push_back(word);
			_priVerbs.push_back(word);
		}
	}

	// If there are gaps in the list, we will error out here
	if (_priNouns.size() != index)
		error("Error reading word list");
}

void AdlEngine_PC::loadItems(Common::ReadStream &stream) {
	byte id;
	while ((id = stream.readByte()) != 0xff && !stream.eos() && !stream.err()) {
		Item item;

		item.id = id;
		item.noun = stream.readByte();
		item.picture = stream.readByte();
		item.region = stream.readByte();
		item.room = stream.readByte();
		item.state = stream.readByte();
		item.description = stream.readByte();
		item.position.x = stream.readByte();
		item.position.y = stream.readByte();

		// Flag to keep track of what has been drawn on the screen
		stream.readByte();

		byte picListSize = stream.readByte();

		stream.readByte(); // Struct size

		for (uint i = 0; i < picListSize; ++i)
			item.roomPictures.push_back(stream.readByte());

		_state.items.push_back(item);
	}

	if (stream.eos() || stream.err())
		error("Error loading items");
}

void AdlEngine_PC::readCommands(Common::ReadStream &stream, Commands &commands) {
	commands.clear();

	while (1) {
		Command command;
		command.room = stream.readByte();

		if (command.room == 0xff)
			return;

		command.verb = stream.readByte();
		command.noun = stream.readByte();

		byte scriptSize = stream.readByte() - 4;

		command.numCond = 0;
		command.numAct = 0;

		for (uint i = 0; i < scriptSize; ++i)
			command.script.push_back(stream.readByte());

		if (stream.eos() || stream.err())
			error("Failed to read commands");

		if (command.script[0] == 0xff && command.script[1] == IDO_ACT_SAVE) {
			_saveVerb = command.verb;
			_saveNoun = command.noun;
		}

		if (command.script[0] == 0xff && command.script[1] == IDO_ACT_LOAD) {
			_restoreVerb = command.verb;
			_restoreNoun = command.noun;
		}

		commands.push_back(command);
	}
}

void AdlEngine_PC::loadRegion(byte region) {
	if (_currentVolume != _regionInitDataOffsets[region - 1].volume) {
		insertDisk(_regionInitDataOffsets[region - 1].volume);

		// FIXME: This shouldn't be needed, but currently is, due to
		// implementation choices made earlier on for DataBlockPtr and DiskImage.
		_state.region = 0; // To avoid region offset being applied
		_itemPics.clear();
		_itemPicIndex->seek(0);
		loadItemPictures(*_itemPicIndex, _itemPicIndex->size() / 4);
	}

	_state.region = region;

	byte track = _regionInitDataOffsets[region - 1].track;
	byte sector = _regionInitDataOffsets[region - 1].sector;
	uint offset = _regionInitDataOffsets[region - 1].offset;

	fixupDiskOffset(track, sector);

	for (uint block = 0; block < 6; ++block) {
		StreamPtr stream(DataBlock_PC(_disk, track, sector, offset).createReadStream());
		const uint size = stream->size();

		switch (block) {
		case 0: {
			// Messages
			_messages.clear();
			loadMessages(*stream, size / 4);
			break;
		}
		case 1: {
			// Global pics
			_pictures.clear();
			loadPictures(*stream);
			break;
		}
		case 2:
			// There's only one word list, so we load both verbs and nouns with it
			loadWords(*stream);
			break;
		case 3: {
			// Rooms
			uint count = size / 14 - 1;
			stream->skip(14); // Skip invalid room 0

			_state.rooms.clear();
			loadRooms(*stream, count);
			break;
		}
		case 4:
			// Room commands
			readCommands(*stream, _roomCommands);
			break;
		case 5:
			// Global commands
			readCommands(*stream, _globalCommands);
		}

		offset += 2 + size;
		while (offset >= 512) {
			offset -= 511;
			++sector;
			if (sector > 8) {
				sector = 1;
				++track;
			}
		}
	}

	applyRegionWorkarounds();
	restoreVars();
}

void AdlEngine_PC::loadItemPictures(Common::ReadStream &stream, byte count) {
	for (uint i = 0; i < count; ++i)
		_itemPics.push_back(readDataBlockPtr(stream));
}

// I suspect this is a bug in the PC version and this should just be o4_isVarGT
// However, this opcode isn't used at all in hires2 (hires4 usage unknown)
int AdlEngine_PC::o_isVarLT(ScriptEnv &e) {
	OP_DEBUG_2("\t&& VARS[%d] < %d", e.arg(1), e.arg(2));

	if (getVar(e.arg(1)) < e.arg(2))
		return 2;

	return -1;
}

int AdlEngine_PC::o_sound1(ScriptEnv &e) {
	OP_DEBUG_0("\tSOUND1()");
	// Stub
	return 0;
}

int AdlEngine_PC::o_sound2(ScriptEnv &e) {
	OP_DEBUG_0("\tSOUND2()");
	// Stub
	return 0;
}

int AdlEngine_PC::o_sound3(ScriptEnv &e) {
	OP_DEBUG_0("\tSOUND3()");
	// Stub
	return 0;
}

int AdlEngine_PC::o_sound4(ScriptEnv &e) {
	OP_DEBUG_0("\tSOUND4()");
	// Stub
	return 0;
}

int AdlEngine_PC::o_dummy(ScriptEnv &e) {
	OP_DEBUG_0("\tDUMMY()");
	return 0;
}

typedef Common::Functor1Mem<ScriptEnv &, int, AdlEngine_PC> OpcodePC;
#define SetOpcodeTable(x) table = &x;
#define Opcode(x) table->push_back(new OpcodePC(this, &AdlEngine_PC::x))
#define OpcodeUnImpl() table->push_back(new OpcodePC(this, 0))

void AdlEngine_PC::setupOpcodeTables() {
	Common::Array<const Opcode *> *table = 0;

	SetOpcodeTable(_condOpcodes);
	// 0x00
	OpcodeUnImpl();
	Opcode(o2_isFirstTime);
	Opcode(o2_isRandomGT);
	Opcode(o4_isItemInRoom);
	// 0x04
	Opcode(o3_isNounNotInRoom);
	Opcode(o1_isMovesGT);
	Opcode(o1_isVarEQ);
	Opcode(o2_isCarryingSomething);
	// 0x08
	Opcode(o_isVarLT);
	Opcode(o1_isCurPicEQ);

	SetOpcodeTable(_actOpcodes);
	// 0x00
	OpcodeUnImpl();
	Opcode(o1_varAdd);
	Opcode(o1_varSub);
	Opcode(o1_varSet);
	// 0x04
	Opcode(o1_listInv);
	Opcode(o4_moveItem);
	Opcode(o1_setRoom);
	Opcode(o2_setCurPic);
	// 0x08
	Opcode(o2_setPic);
	Opcode(o1_printMsg);
	Opcode(o_sound1);
	Opcode(o_sound2);
	// 0x0c
	Opcode(o4_moveAllItems);
	Opcode(o1_quit);
	Opcode(o_dummy);
	Opcode(o4_save);
	// 0x10
	Opcode(o4_restore);
	Opcode(o4_restart);
	Opcode(o2_placeItem);
	Opcode(o_sound3);
	// 0x14
	Opcode(o1_resetPic);
	Opcode(o1_goDirection<IDI_DIR_NORTH>);
	Opcode(o1_goDirection<IDI_DIR_SOUTH>);
	Opcode(o1_goDirection<IDI_DIR_EAST>);
	// 0x18
	Opcode(o1_goDirection<IDI_DIR_WEST>);
	Opcode(o1_goDirection<IDI_DIR_UP>);
	Opcode(o1_goDirection<IDI_DIR_DOWN>);
	Opcode(o1_takeItem);
	// 0x1c
	Opcode(o1_dropItem);
	Opcode(o4_setRoomPic);
	OpcodeUnImpl();
	Opcode(o_sound4);
	// 0x20
	Opcode(o2_initDisk);
}

} // End of namespace Adl
