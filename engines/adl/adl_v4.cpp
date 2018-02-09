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

#include "adl/adl_v4.h"
#include "adl/display.h"

namespace Adl {

AdlEngine_v4::AdlEngine_v4(OSystem *syst, const AdlGameDescription *gd) :
		AdlEngine_v3(syst, gd),
		_itemPicIndex(nullptr) {

}

AdlEngine_v4::~AdlEngine_v4() {
	delete _itemPicIndex;
}

void AdlEngine_v4::gameLoop() {
	uint verb = 0, noun = 0;
	_isRestarting = false;

	if (_isRestoring) {
		// Game restored from launcher. As this version of ADL long jumps to
		// the game loop after restoring, no special action is required.
		_isRestoring = false;
	}

	showRoom();

	if (_isRestarting || shouldQuit())
		return;

	_canSaveNow = _canRestoreNow = true;
	getInput(verb, noun);
	_canSaveNow = _canRestoreNow = false;

	if (_isRestoring) {
		// Game restored from GMM. Move cursor to next line and jump to
		// start of game loop.
		_display->printAsciiString("\r");
		_isRestoring = false;
		return;
	}

	if (_isRestarting || shouldQuit())
		return;

	_linesPrinted = 0;

	checkInput(verb, noun);

	if (_isRestarting || shouldQuit())
		return;

	doAllCommands(_globalCommands, verb, noun);

	if (_isRestarting || shouldQuit())
		return;

	_state.moves++;
}

void AdlEngine_v4::loadState(Common::ReadStream &stream) {
	_state.room = stream.readByte();
	_state.region = stream.readByte();
	_state.prevRegion = stream.readByte();

	uint32 size = stream.readUint32BE();
	if (size != _state.regions.size())
		error("Region count mismatch (expected %i; found %i)", _state.regions.size(), size);

	Common::Array<Region>::iterator region;
	for (region = _state.regions.begin(); region != _state.regions.end(); ++region) {
		size = stream.readUint32BE();
		if (size != region->rooms.size())
			error("Room count mismatch (expected %i; found %i)", region->rooms.size(), size);

		Common::Array<RoomState>::iterator room;
		for (room = region->rooms.begin(); room != region->rooms.end(); ++room) {
			room->picture = stream.readByte();
			room->isFirstTime = stream.readByte();
		}

		size = stream.readUint32BE();
		if (size != region->vars.size())
			error("Variable count mismatch (expected %i; found %i)", region->vars.size(), size);

		for (uint i = 0; i < region->vars.size(); ++i)
			region->vars[i] = stream.readByte();
	}

	size = stream.readUint32BE();
	if (size != _state.items.size())
		error("Item count mismatch (expected %i; found %i)", _state.items.size(), size);

	Common::List<Item>::iterator item;
	for (item = _state.items.begin(); item != _state.items.end(); ++item) {
		item->room = stream.readByte();
		item->region = stream.readByte();
		item->state = stream.readByte();
	}

	size = stream.readUint32BE();
	const uint expectedSize = _state.vars.size() - getRegion(1).vars.size();
	if (size != expectedSize)
		error("Variable count mismatch (expected %i; found %i)", expectedSize, size);

	for (uint i = getRegion(1).vars.size(); i < _state.vars.size(); ++i)
		_state.vars[i] = stream.readByte();

	if (stream.err() || stream.eos())
		return;

	loadRegion(_state.region);
	restoreRoomState(_state.room);
	_roomOnScreen = _picOnScreen = 0;
}

void AdlEngine_v4::saveState(Common::WriteStream &stream) {
	getCurRoom().isFirstTime = false;
	backupVars();
	backupRoomState(_state.room);

	stream.writeByte(_state.room);
	stream.writeByte(_state.region);
	stream.writeByte(_state.prevRegion);

	stream.writeUint32BE(_state.regions.size());
	Common::Array<Region>::const_iterator region;
	for (region = _state.regions.begin(); region != _state.regions.end(); ++region) {
		stream.writeUint32BE(region->rooms.size());
		Common::Array<RoomState>::const_iterator room;
		for (room = region->rooms.begin(); room != region->rooms.end(); ++room) {
			stream.writeByte(room->picture);
			stream.writeByte(room->isFirstTime);
		}

		stream.writeUint32BE(region->vars.size());
		for (uint i = 0; i < region->vars.size(); ++i)
			stream.writeByte(region->vars[i]);
	}

	stream.writeUint32BE(_state.items.size());
	Common::List<Item>::const_iterator item;
	for (item = _state.items.begin(); item != _state.items.end(); ++item) {
		stream.writeByte(item->room);
		stream.writeByte(item->region);
		stream.writeByte(item->state);
	}

	stream.writeUint32BE(_state.vars.size() - getRegion(1).vars.size());
	for (uint i = getRegion(1).vars.size(); i < _state.vars.size(); ++i)
		stream.writeByte(_state.vars[i]);
}

Common::String AdlEngine_v4::loadMessage(uint idx) const {
	Common::String str = AdlEngine_v3::loadMessage(idx);

	for (uint i = 0; i < str.size(); ++i) {
		const char *xorStr = "AVISDURGAN";
		str.setChar(str[i] ^ xorStr[i % strlen(xorStr)], i);
	}

	return str;
}

Common::String AdlEngine_v4::getItemDescription(const Item &item) const {
	return _itemDesc[item.id - 1];
}

void AdlEngine_v4::loadRegionLocations(Common::ReadStream &stream, uint regions) {
	for (uint r = 0; r < regions; ++r) {
		RegionLocation loc;
		loc.track = stream.readByte();
		loc.sector = stream.readByte();

		if (stream.eos() || stream.err())
			error("Failed to read region locations");

		_regionLocations.push_back(loc);
	}
}

void AdlEngine_v4::loadRegionInitDataOffsets(Common::ReadStream &stream, uint regions) {
	for (uint r = 0; r < regions; ++r) {
		RegionInitDataOffset initOfs;
		initOfs.track = stream.readByte();
		initOfs.sector = stream.readByte();
		initOfs.offset = stream.readByte();
		initOfs.volume = stream.readByte();

		if (stream.eos() || stream.err())
			error("Failed to read region init data offsets");

		_regionInitDataOffsets.push_back(initOfs);
	}
}

void AdlEngine_v4::initRoomState(RoomState &roomState) const {
	roomState.picture = 1;
	roomState.isFirstTime = 1;
}

void AdlEngine_v4::initRegions(const byte *roomsPerRegion, uint regions) {
	_state.regions.resize(regions);

	for (uint r = 0; r < regions; ++r) {
		Region &regn = _state.regions[r];
		// Each region has 24 variables
		regn.vars.resize(24);

		regn.rooms.resize(roomsPerRegion[r]);
		for (uint rm = 0; rm < roomsPerRegion[r]; ++rm)
			initRoomState(regn.rooms[rm]);
	}
}

void AdlEngine_v4::fixupDiskOffset(byte &track, byte &sector) const {
	if (_state.region == 0)
		return;

	sector += _regionLocations[_state.region - 1].sector;
	if (sector >= 16) {
		sector -= 16;
		++track;
	}

	track += _regionLocations[_state.region - 1].track;
}

void AdlEngine_v4::adjustDataBlockPtr(byte &track, byte &sector, byte &offset, byte &size) const {
	fixupDiskOffset(track, sector);
}

AdlEngine_v4::RegionChunkType AdlEngine_v4::getRegionChunkType(const uint16 addr) const {
	switch (addr) {
		case 0x9000:
			return kRegionChunkMessages;
		case 0x4a80:
			return kRegionChunkGlobalPics;
		case 0x4000:
			return kRegionChunkVerbs;
		case 0x1800:
			return kRegionChunkNouns;
		case 0x0e00:
			return kRegionChunkRooms;
		case 0x7b00:
			return kRegionChunkRoomCmds;
		case 0x9500:
			return kRegionChunkGlobalCmds;
		default:
			return kRegionChunkUnknown;
	}
}

void AdlEngine_v4::loadRegion(byte region) {
	if (_currentVolume != _regionInitDataOffsets[region - 1].volume) {
		insertDisk(_regionInitDataOffsets[region - 1].volume);

		// FIXME: This shouldn't be needed, but currently is, due to
		// implementation choices made earlier on for DataBlockPtr and DiskImage.
		_state.region = 0; // To avoid region offset being applied
		_itemPics.clear();
		_itemPicIndex->seek(0);
		loadItemPictures(*_itemPicIndex, _itemPicIndex->size() / 5);
	}

	_state.region = region;

	byte track = _regionInitDataOffsets[region - 1].track;
	byte sector = _regionInitDataOffsets[region - 1].sector;
	uint offset = _regionInitDataOffsets[region - 1].offset;

	fixupDiskOffset(track, sector);

	for (uint block = 0; block < 7; ++block) {
		StreamPtr stream(_disk->createReadStream(track, sector, offset, 1));

		uint16 addr = stream->readUint16LE();
		uint16 size = stream->readUint16LE();

		stream.reset(_disk->createReadStream(track, sector, offset, size / 256 + 1));
		stream->skip(4);

		switch (getRegionChunkType(addr)) {
		case kRegionChunkMessages: {
			// Messages
			_messages.clear();
			uint count = size / 4;
			loadMessages(*stream, count);
			break;
		}
		case kRegionChunkGlobalPics: {
			// Global pics
			_pictures.clear();
			loadPictures(*stream);
			break;
		}
		case kRegionChunkVerbs:
			// Verbs
			loadWords(*stream, _verbs, _priVerbs);
			break;
		case kRegionChunkNouns:
			// Nouns
			loadWords(*stream, _nouns, _priNouns);
			break;
		case kRegionChunkRooms: {
			// Rooms
			uint count = size / 14 - 1;
			stream->skip(14); // Skip invalid room 0

			_state.rooms.clear();
			loadRooms(*stream, count);
			break;
		}
		case kRegionChunkRoomCmds:
			// Room commands
			readCommands(*stream, _roomCommands);
			break;
		case kRegionChunkGlobalCmds:
			// Global commands
			readCommands(*stream, _globalCommands);
			break;
		default:
			error("Unknown data block found (addr %04x; size %04x)", addr, size);
		}

		offset += 4 + size;
		while (offset >= 256) {
			offset -= 256;
			++sector;
			if (sector >= 16) {
				sector = 0;
				++track;
			}
		}
	}

	applyRegionWorkarounds();
	restoreVars();
}

void AdlEngine_v4::loadItemPicIndex(Common::ReadStream &stream, uint items) {
	_itemPicIndex = stream.readStream(items * 5);

	if (stream.eos() || stream.err())
		error("Error reading item index");
}

void AdlEngine_v4::backupRoomState(byte room) {
	RoomState &backup = getCurRegion().rooms[room - 1];

	backup.isFirstTime = getRoom(room).isFirstTime;
	backup.picture = getRoom(room).picture;
}

byte AdlEngine_v4::restoreRoomState(byte room) {
	const RoomState &backup = getCurRegion().rooms[room - 1];

	if (backup.isFirstTime != 1) {
		getRoom(room).curPicture = getRoom(room).picture = backup.picture;
		getRoom(room).isFirstTime = false;
		return 0;
	}

	return 1;
}

void AdlEngine_v4::backupVars() {
	Region &region = getCurRegion();

	for (uint i = 0; i < region.vars.size(); ++i)
		region.vars[i] = getVar(i);
}

void AdlEngine_v4::restoreVars() {
	const Region &region = getCurRegion();

	for (uint i = 0; i < region.vars.size(); ++i)
		setVar(i, region.vars[i]);
}

void AdlEngine_v4::switchRegion(byte region) {
	backupVars();
	backupRoomState(_state.room);
	_state.prevRegion = _state.region;
	_state.region = region;
	loadRegion(region);
	_state.room = 1;
	_picOnScreen = _roomOnScreen = 0;
}

void AdlEngine_v4::switchRoom(byte roomNr) {
	getCurRoom().curPicture = getCurRoom().picture;
	getCurRoom().isFirstTime = false;
	backupRoomState(_state.room);
	_state.room = roomNr;
	restoreRoomState(_state.room);
}

int AdlEngine_v4::o4_isItemInRoom(ScriptEnv &e) {
	OP_DEBUG_2("\t&& GET_ITEM_ROOM(%s) == %s", itemStr(e.arg(1)).c_str(), itemRoomStr(e.arg(2)).c_str());

	const Item &item = getItem(e.arg(1));

	if (e.arg(2) != IDI_ANY && item.region != _state.region)
		return -1;

	if (item.room == roomArg(e.arg(2)))
		return 2;

	return -1;
}

int AdlEngine_v4::o4_isVarGT(ScriptEnv &e) {
	OP_DEBUG_2("\t&& VARS[%d] > %d", e.arg(1), e.arg(2));

	if (getVar(e.arg(1)) > e.arg(2))
		return 2;

	return -1;
}

int AdlEngine_v4::o4_moveItem(ScriptEnv &e) {
	o2_moveItem(e);
	getItem(e.arg(1)).region = _state.region;
	return 2;
}

int AdlEngine_v4::o4_setRegionToPrev(ScriptEnv &e) {
	OP_DEBUG_0("\tREGION = PREV_REGION");

	switchRegion(_state.prevRegion);
	// Long jump
	_isRestarting = true;
	return -1;
}

int AdlEngine_v4::o4_moveAllItems(ScriptEnv &e) {
	OP_DEBUG_2("\tMOVE_ALL_ITEMS(%s, %s)", itemRoomStr(e.arg(1)).c_str(), itemRoomStr(e.arg(2)).c_str());

	byte room1 = roomArg(e.arg(1));

	if (room1 == _state.room)
		_picOnScreen = 0;

	byte room2 = roomArg(e.arg(2));

	Common::List<Item>::iterator item;

	for (item = _state.items.begin(); item != _state.items.end(); ++item) {
		if (room1 != item->room)
			continue;

		if (room1 != IDI_ANY) {
			if (_state.region != item->region)
				continue;
			if (room2 == IDI_ANY) {
				if (isInventoryFull())
					break;
				if (item->state == IDI_ITEM_DOESNT_MOVE)
					continue;
			}
		}

		item->room = room2;
		item->region = _state.region;

		if (room1 == IDI_ANY)
			item->state = IDI_ITEM_DROPPED;
	}

	return 2;
}

int AdlEngine_v4::o4_setRegion(ScriptEnv &e) {
	OP_DEBUG_1("\tREGION = %d", e.arg(1));

	switchRegion(e.arg(1));
	// Long jump
	_isRestarting = true;
	return -1;
}

int AdlEngine_v4::o4_save(ScriptEnv &e) {
	OP_DEBUG_0("\tSAVE_GAME()");

	_display->printString(_strings_v2.saveReplace);
	const char key = inputKey();

	if (shouldQuit())
		return -1;

	if (key != APPLECHAR('Y'))
		return 0;

	const int slot = askForSlot(_strings_v2.saveInsert);

	if (slot < 0)
		return -1;

	saveGameState(slot, "");
	return 0;
}

int AdlEngine_v4::o4_restore(ScriptEnv &e) {
	OP_DEBUG_0("\tRESTORE_GAME()");

	const int slot = askForSlot(_strings_v2.restoreInsert);

	if (slot < 0)
		return -1;

	loadGameState(slot);
	_isRestoring = false;

	_picOnScreen = 0;
	_roomOnScreen = 0;

	// Long jump
	_isRestarting = true;
	return -1;
}

int AdlEngine_v4::o4_restart(ScriptEnv &e) {
	OP_DEBUG_0("\tRESTART_GAME()");

	while (true) {
		_display->printString(_strings.playAgain);
		const Common::String input(inputString());

		if (shouldQuit())
			return -1;

		if (input.firstChar() == APPLECHAR('N')) {
			return o1_quit(e);
		} else if (input.firstChar() == APPLECHAR('Y')) {
			// The original game loads a special save game from volume 3
			initState();
			// Long jump
			_isRestarting = true;
			return -1;
		}
	}
}

int AdlEngine_v4::o4_setRegionRoom(ScriptEnv &e) {
	OP_DEBUG_2("\tSET_REGION_ROOM(%d, %d)", e.arg(1), e.arg(2));

	switchRegion(e.arg(1));
	_state.room = e.arg(2);
	// Long jump
	_isRestarting = true;
	return -1;
}

int AdlEngine_v4::o4_setRoomPic(ScriptEnv &e) {
	o1_setRoomPic(e);
	backupRoomState(e.arg(1));
	return 2;
}

void AdlEngine_v4_PC::loadItemDescriptions(Common::SeekableReadStream &stream, byte count) {
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

void AdlEngine_v4_PC::loadRegionMetaData(Common::SeekableReadStream &stream) {
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

void AdlEngine_v4_PC::fixupDiskOffset(byte &track, byte &sector) const {
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

DataBlockPtr AdlEngine_v4_PC::readDataBlockPtr(Common::ReadStream &f) const {
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

void AdlEngine_v4_PC::loadWords(Common::ReadStream &stream) {
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

void AdlEngine_v4_PC::loadItems(Common::ReadStream &stream) {
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

void AdlEngine_v4_PC::readCommands(Common::ReadStream &stream, Commands &commands) {
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

void AdlEngine_v4_PC::loadRegion(byte region) {
	if (_currentVolume != _regionInitDataOffsets[region - 1].volume) {
		insertDisk(_regionInitDataOffsets[region - 1].volume);

		// FIXME: This shouldn't be needed, but currently is, due to
		// implementation choices made earlier on for DataBlockPtr and DiskImage.
		_state.region = 0; // To avoid region offset being applied
		_itemPics.clear();
		_itemPicIndex->seek(0);
		loadItemPictures(*_itemPicIndex, _itemPicIndex->size() / 5);
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

} // End of namespace Adl
