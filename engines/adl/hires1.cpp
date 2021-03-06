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

#include "common/system.h"
#include "common/debug.h"
#include "common/error.h"
#include "common/file.h"
#include "common/stream.h"
#include "common/ptr.h"
#include "common/memstream.h"

#include "adl/adl.h"
#include "adl/graphics.h"
#include "adl/display_a2.h"

namespace Adl {

#define IDS_HR1_EXE_0    "AUTO LOAD OBJ"
#define IDS_HR1_EXE_1    "ADVENTURE"
#define IDS_HR1_MESSAGES "MESSAGES"

#define IDI_HR1_NUM_ROOMS         41
#define IDI_HR1_NUM_PICS          97
#define IDI_HR1_NUM_VARS          20
#define IDI_HR1_NUM_ITEM_OFFSETS  21
#define IDI_HR1_NUM_MESSAGES     168

// Messages used outside of scripts
#define IDI_HR1_MSG_CANT_GO_THERE      137
#define IDI_HR1_MSG_DONT_UNDERSTAND     37
#define IDI_HR1_MSG_ITEM_DOESNT_MOVE   151
#define IDI_HR1_MSG_ITEM_NOT_HERE      152
#define IDI_HR1_MSG_THANKS_FOR_PLAYING 140
#define IDI_HR1_MSG_DONT_HAVE_IT       127
#define IDI_HR1_MSG_GETTING_DARK         7

#define IDI_HR1_OFS_STR_ENTER_COMMAND   0x5bbc
#define IDI_HR1_OFS_STR_VERB_ERROR      0x5b4f
#define IDI_HR1_OFS_STR_NOUN_ERROR      0x5b8e
#define IDI_HR1_OFS_STR_PLAY_AGAIN      0x5f1e
#define IDI_HR1_OFS_STR_CANT_GO_THERE   0x6c0a
#define IDI_HR1_OFS_STR_DONT_HAVE_IT    0x6c31
#define IDI_HR1_OFS_STR_DONT_UNDERSTAND 0x6c51
#define IDI_HR1_OFS_STR_GETTING_DARK    0x6c7c
#define IDI_HR1_OFS_STR_PRESS_RETURN    0x5f68
#define IDI_HR1_OFS_STR_LINE_FEEDS      0x59d4

#define IDI_HR1_OFS_PD_TEXT_0    0x005d
#define IDI_HR1_OFS_PD_TEXT_1    0x012b
#define IDI_HR1_OFS_PD_TEXT_2    0x016d
#define IDI_HR1_OFS_PD_TEXT_3    0x0259

#define IDI_HR1_OFS_INTRO_TEXT   0x0066
#define IDI_HR1_OFS_GAME_OR_HELP 0x000f

#define IDI_HR1_OFS_LOGO_0       0x1003

#define IDI_HR1_OFS_ITEMS        0x0100
#define IDI_HR1_OFS_ROOMS        0x050a
#define IDI_HR1_OFS_PICS         0x4b03
#define IDI_HR1_OFS_CMDS_0       0x3c00
#define IDI_HR1_OFS_CMDS_1       0x3d00
#define IDI_HR1_OFS_MSGS         0x4d00

#define IDI_HR1_OFS_ITEM_OFFSETS 0x68ff
#define IDI_HR1_OFS_SHAPES       0x4f00

#define IDI_HR1_OFS_VERBS        0x3800
#define IDI_HR1_OFS_NOUNS        0x0f00

class HiRes1Engine : public AdlEngine {
public:
	HiRes1Engine(OSystem *syst, const AdlGameDescription *gd) :
			AdlEngine(syst, gd),
			_files(nullptr),
			_messageDelay(true) { }
	~HiRes1Engine() override { delete _files; }

protected:
	// AdlEngine
	void runIntro() override;
	void init() override;
	void initGameState() override;
	void restartGame();
	void printString(const Common::String &str) override;
	Common::String loadMessage(uint idx) const override;
	void printMessage(uint idx) override;
	void drawItems() override;
	void drawItem(Item &item, const Common::Point &pos) override;
	void loadRoom(byte roomNr) override;
	void showRoom() override;

	virtual void loadExeStrings();
	void showInstructions(Common::SeekableReadStream &stream, const uint pages[], bool goHome);
	void wordWrap(Common::String &str) const;

	Files *_files;
	Common::File _exe;
	Common::Array<DataBlockPtr> _corners;
	Common::Array<byte> _roomDesc;
	bool _messageDelay;

	struct {
		Common::String cantGoThere;
		Common::String dontHaveIt;
		Common::String dontUnderstand;
		Common::String gettingDark;
	} _gameStrings;
};

void HiRes1Engine::showInstructions(Common::SeekableReadStream &stream, const uint pages[], bool goHome) {
	_display->setMode(Display::kModeText);

	uint page = 0;
	while (pages[page] != 0) {
		if (goHome)
			_display->home();

		uint count = pages[page++];
		for (uint i = 0; i < count; ++i) {
			_display->printString(readString(stream));
			stream.seek(3, SEEK_CUR);
		}

		inputString();

		if (shouldQuit())
			return;

		stream.seek((goHome ? 6 : 3), SEEK_CUR);
	}
}

void HiRes1Engine::runIntro() {
	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_0));

	// Early version have no bitmap in 'AUTO LOAD OBJ'
	if (getGameVersion() >= GAME_VER_HR1_COARSE) {
		stream->seek(IDI_HR1_OFS_LOGO_0);
		_display->setMode(Display::kModeGraphics);
		static_cast<Display_A2 *>(_display)->loadFrameBuffer(*stream);
		_display->renderGraphics();

		if (getGameVersion() == GAME_VER_HR1_PD) {
			// Only the PD version shows a title screen during the load
			delay(4000);

			if (shouldQuit())
				return;
		}
	}

	Common::String str;

	// Show the PD disclaimer for the PD release
	if (getGameVersion() == GAME_VER_HR1_PD) {
		// The PD release on the Roberta Williams Anthology disc has a PDE
		// splash screen. The original HELLO file has been renamed to
		// MYSTERY.HELLO. It's unclear whether or not this splash screen
		// was present in the original PD release back in 1987.
		StreamPtr basic(_files->createReadStream("MYSTERY.HELLO"));

		_display->setMode(Display::kModeText);
		_display->home();

		str = readStringAt(*basic, IDI_HR1_OFS_PD_TEXT_0, '"');
		_display->printAsciiString(str + '\r');

		str = readStringAt(*basic, IDI_HR1_OFS_PD_TEXT_1, '"');
		_display->printAsciiString(str + "\r\r");

		str = readStringAt(*basic, IDI_HR1_OFS_PD_TEXT_2, '"');
		_display->printAsciiString(str + "\r\r");

		str = readStringAt(*basic, IDI_HR1_OFS_PD_TEXT_3, '"');
		_display->printAsciiString(str + '\r');

		inputKey();
		if (shouldQuit())
			return;
	}

	_display->setMode(Display::kModeMixed);

	str = readStringAt(*stream, IDI_HR1_OFS_GAME_OR_HELP);

	if (getGameVersion() >= GAME_VER_HR1_COARSE) {
		bool instructions = false;

		while (1) {
			_display->printString(str);
			Common::String s = inputString();

			if (shouldQuit())
				break;

			if (s.empty())
				continue;

			if (s[0] == _display->asciiToNative('I')) {
				instructions = true;
				break;
			} else if (s[0] == _display->asciiToNative('G')) {
				break;
			}
		}

		if (instructions) {
			// This version shows the last page during the loading of the game
			// We wait for a key instead (even though there's no prompt for that).
			const uint pages[] = { 6, 6, 4, 5, 8, 7, 0 };
			stream->seek(IDI_HR1_OFS_INTRO_TEXT);
			showInstructions(*stream, pages, true);
			_display->printAsciiString("\r");
		}
	} else {
		const uint pages[] = { 6, 6, 8, 6, 0 };
		stream->seek(6);
		showInstructions(*stream, pages, false);
	}

	stream.reset(_files->createReadStream(IDS_HR1_EXE_1));
	stream->seek(0x1800);
	static_cast<Display_A2 *>(_display)->loadFrameBuffer(*stream);
	_display->renderGraphics();

	_display->setMode(Display::kModeMixed);

	if (getGameVersion() == GAME_VER_HR1_SIMI) {
		// The original waits for the key after initializing the state.
		// This causes it to also wait for a key on a blank screen when
		// a game is restarted. We only wait for a key here during the
		// intro.

		// This does mean we need to push out some extra line feeds to clear the screen
		_display->printString(_strings.lineFeeds);
		inputKey();
		if (shouldQuit())
			return;
	}
}

void HiRes1Engine::loadExeStrings() {
	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_1));
	
	// Some messages have overrides inside the executable
	_gameStrings.cantGoThere = readStringAt(*stream, IDI_HR1_OFS_STR_CANT_GO_THERE);
	_gameStrings.dontHaveIt = readStringAt(*stream, IDI_HR1_OFS_STR_DONT_HAVE_IT);
	_gameStrings.dontUnderstand = readStringAt(*stream, IDI_HR1_OFS_STR_DONT_UNDERSTAND);
	_gameStrings.gettingDark = readStringAt(*stream, IDI_HR1_OFS_STR_GETTING_DARK);

	// Load other strings from executable
	_strings.enterCommand = readStringAt(*stream, IDI_HR1_OFS_STR_ENTER_COMMAND);
	_strings.verbError = readStringAt(*stream, IDI_HR1_OFS_STR_VERB_ERROR);
	_strings.nounError = readStringAt(*stream, IDI_HR1_OFS_STR_NOUN_ERROR);
	_strings.playAgain = readStringAt(*stream, IDI_HR1_OFS_STR_PLAY_AGAIN);
	_strings.pressReturn = readStringAt(*stream, IDI_HR1_OFS_STR_PRESS_RETURN);
	_strings.lineFeeds = readStringAt(*stream, IDI_HR1_OFS_STR_LINE_FEEDS);
}

void HiRes1Engine::init() {
	if (Common::File::exists("ADVENTURE")) {
		_files = new Files_Plain();
	} else {
		Files_AppleDOS *files = new Files_AppleDOS();
		// Non-PD versions have an obfuscated FAT
		if (!files->open(getDiskImageName(0), (getGameVersion() == GAME_VER_HR1_PD ? 17 : 16)))
			error("Failed to open '%s'", getDiskImageName(0).c_str());
		_files = files;
	}

	_graphics = new GraphicsMan_v1<Display_A2>(*static_cast<Display_A2 *>(_display));
	_display->moveCursorTo(Common::Point(0, 3));

	loadExeStrings();

	// Set message IDs
	_messageIds.cantGoThere = IDI_HR1_MSG_CANT_GO_THERE;
	_messageIds.dontUnderstand = IDI_HR1_MSG_DONT_UNDERSTAND;
	_messageIds.itemDoesntMove = IDI_HR1_MSG_ITEM_DOESNT_MOVE;
	_messageIds.itemNotHere = IDI_HR1_MSG_ITEM_NOT_HERE;
	_messageIds.thanksForPlaying = IDI_HR1_MSG_THANKS_FOR_PLAYING;

	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_1));

	// Load message offsets
	stream->seek(IDI_HR1_OFS_MSGS);
	for (uint i = 0; i < IDI_HR1_NUM_MESSAGES; ++i)
		_messages.push_back(_files->getDataBlock(IDS_HR1_MESSAGES, stream->readUint16LE()));

	// Load picture data from executable
	stream->seek(IDI_HR1_OFS_PICS);
	for (uint i = 1; i <= IDI_HR1_NUM_PICS; ++i) {
		byte block = stream->readByte();
		Common::String name = Common::String::format("BLOCK%i", block);
		uint16 offset = stream->readUint16LE();
		_pictures[i] = _files->getDataBlock(name, offset);
	}

	// Load commands from executable
	stream->seek(IDI_HR1_OFS_CMDS_1);
	readCommands(*stream, _roomCommands);

	stream->seek(IDI_HR1_OFS_CMDS_0);
	readCommands(*stream, _globalCommands);

	// Load dropped item offsets
	stream->seek(IDI_HR1_OFS_ITEM_OFFSETS);
	loadDroppedItemOffsets(*stream, IDI_HR1_NUM_ITEM_OFFSETS);

	// Load shapes
	stream->seek(IDI_HR1_OFS_SHAPES);
	uint16 cornersCount = stream->readUint16LE();
	for (uint i = 0; i < cornersCount; ++i)
		_corners.push_back(_files->getDataBlock(IDS_HR1_EXE_1, IDI_HR1_OFS_SHAPES + stream->readUint16LE()));

	if (stream->eos() || stream->err())
		error("Failed to read game data from '" IDS_HR1_EXE_1 "'");

	stream->seek(IDI_HR1_OFS_VERBS);
	loadWords(*stream, _verbs, _priVerbs);

	stream->seek(IDI_HR1_OFS_NOUNS);
	loadWords(*stream, _nouns, _priNouns);
}

void HiRes1Engine::initGameState() {
	_state.vars.resize(IDI_HR1_NUM_VARS);

	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_1));

	// Load room data from executable
	_roomDesc.clear();
	stream->seek(IDI_HR1_OFS_ROOMS);
	for (uint i = 0; i < IDI_HR1_NUM_ROOMS; ++i) {
		Room room;
		stream->readByte();
		_roomDesc.push_back(stream->readByte());
		for (uint j = 0; j < 6; ++j)
			room.connections[j] = stream->readByte();
		room.picture = stream->readByte();
		room.curPicture = stream->readByte();
		_state.rooms.push_back(room);
	}

	// Load item data from executable
	stream->seek(IDI_HR1_OFS_ITEMS);
	byte id;
	while ((id = stream->readByte()) != 0xff) {
		Item item;

		item.id = id;
		item.noun = stream->readByte();
		item.room = stream->readByte();
		item.picture = stream->readByte();
		item.isShape = stream->readByte();
		item.position.x = stream->readByte();
		item.position.y = stream->readByte();
		item.state = stream->readByte();
		item.description = stream->readByte();

		stream->readByte();

		byte size = stream->readByte();

		for (uint i = 0; i < size; ++i)
			item.roomPictures.push_back(stream->readByte());

		_state.items.push_back(item);
	}
}

void HiRes1Engine::restartGame() {
	_display->printString(_strings.pressReturn);
	initState();
	_display->printAsciiString(_strings.lineFeeds);
}

void HiRes1Engine::printString(const Common::String &str) {
	Common::String wrap = str;
	wordWrap(wrap);
	_display->printString(wrap);

	if (_messageDelay)
		delay(14 * 166018 / 1000);
}

Common::String HiRes1Engine::loadMessage(uint idx) const {
	const char returnChar = _display->asciiToNative('\r');
	StreamPtr stream(_messages[idx]->createReadStream());
	return readString(*stream, returnChar) + returnChar;
}

void HiRes1Engine::printMessage(uint idx) {
	// In the English version, messages with hardcoded overrides don't delay
	// after printing. It's unclear if this is a bug or not. In most cases
	// the slow drawing of the room will give the player a chance to read
	// it. This isn't the case in ScummVM however, so we add a delay after
	// these messages.

	// In the French version, messages with hardcoded overrides delay
	// based on string length. This leads to overly long delays on longer
	// strings. This might be a bug, since other messages have a fixed
	// delay (that is slightly longer than in the English version).
	// We've chosen to stick with fixed delays here as well.

	// NOTE: Later games wait for a key when the text window overflows and
	// don't use delays. It might be better to use that system for this game
	// as well.
	switch (idx) {
	case IDI_HR1_MSG_CANT_GO_THERE:
		// NOTE: This triggers word wrapping (not in the original)
		printString(_gameStrings.cantGoThere);
		return;
	case IDI_HR1_MSG_DONT_HAVE_IT:
		printString(_gameStrings.dontHaveIt);
		return;
	case IDI_HR1_MSG_DONT_UNDERSTAND:
		printString(_gameStrings.dontUnderstand);
		return;
	case IDI_HR1_MSG_GETTING_DARK:
		printString(_gameStrings.gettingDark);
		return;
	default:
		printString(loadMessage(idx));
	}
}

void HiRes1Engine::drawItems() {
	Common::List<Item>::iterator item;

	uint dropped = 0;

	for (item = _state.items.begin(); item != _state.items.end(); ++item) {
		// Skip items not in this room
		if (item->room != _state.room)
			continue;

		if (item->state == IDI_ITEM_DROPPED) {
			// Draw dropped item if in normal view
			if (getCurRoom().picture == getCurRoom().curPicture)
				drawItem(*item, _itemOffsets[dropped++]);
		} else {
			// Draw fixed item if current view is in the pic list
			Common::Array<byte>::const_iterator pic;

			for (pic = item->roomPictures.begin(); pic != item->roomPictures.end(); ++pic) {
				if (*pic == getCurRoom().curPicture) {
					drawItem(*item, item->position);
					break;
				}
			}
		}
	}
}

void HiRes1Engine::drawItem(Item &item, const Common::Point &pos) {
	if (item.isShape) {
		StreamPtr stream(_corners[item.picture - 1]->createReadStream());
		Common::Point p(pos);
		_graphics->drawShape(*stream, p);
	} else
		drawPic(item.picture, pos);
}

void HiRes1Engine::loadRoom(byte roomNr) {
	_roomData.description = loadMessage(_roomDesc[_state.room - 1]);
}

void HiRes1Engine::showRoom() {
	_state.curPicture = getCurRoom().curPicture;
	_graphics->clearScreen();
	loadRoom(_state.room);

	if (!_state.isDark) {
		drawPic(getCurRoom().curPicture);
		drawItems();
	}

	_display->renderGraphics();
	_messageDelay = false;
	printString(_roomData.description);
	_messageDelay = true;
}

void HiRes1Engine::wordWrap(Common::String &str) const {
	uint end = 39;

	const char spaceChar = _display->asciiToNative(' ');
	const char returnChar = _display->asciiToNative('\r');

	while (1) {
		if (str.size() <= end)
			return;

		while (str[end] != spaceChar)
			--end;

		str.setChar(returnChar, end);
		end += 40;
	}
}

class HiRes1Engine_Malibu : public HiRes1Engine {
public:
	HiRes1Engine_Malibu(OSystem *syst, const AdlGameDescription *gd) :
			HiRes1Engine(syst, gd) { }

protected:
	// AdlEngine
	void runIntro() override;
	void init() override;
	Common::String formatVerbError(const Common::String &verb) const override;
	Common::String formatNounError(const Common::String &verb, const Common::String &noun) const override;

	// HiRes1Engine
	void loadExeStrings() override;

	void loadCommonExeStrings();
};

void HiRes1Engine_Malibu::runIntro() {
	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_0));

	stream->seek(0x1000);
	_display->setMode(Display::kModeGraphics);
	static_cast<Display_A2 *>(_display)->loadFrameBuffer(*stream);
	_display->renderGraphics();

	// WORKAROUND: Missing in original game, causing key prompt not to show
	_display->setMode(Display::kModeMixed);

	Common::String str = readStringAt(*stream, 0xc);

	// WORKAROUND: There's not enough linefeeds to push the string into the
	// visible window on DOS 3.2, so we print the string an extra time
	_display->printString(str);

	while (1) {
		_display->printString(str);
		Common::String s = inputString();

		if (shouldQuit())
			return;

		if (s.empty())
			continue;

		if (s[0] == _display->asciiToNative('M')) {
			const uint pages[] = { 6, 4, 4, 7, 4, 7, 0 };
			stream->seek(0x63);
			showInstructions(*stream, pages, true);
			_display->printAsciiString("\r");
			return;
		} else if (s[0] == _display->asciiToNative('J')) {
			return;
		}
	}
}

void HiRes1Engine_Malibu::loadCommonExeStrings() {
	// These strings are at the same offsets in both Malibu versions

	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_1));
	
	// Some messages have overrides inside the executable
	_gameStrings.cantGoThere = readStringAt(*stream, 0x6c0a);
	_gameStrings.dontHaveIt = readStringAt(*stream, 0x6c3f);
	_gameStrings.dontUnderstand = readStringAt(*stream, 0x6c61);
	_gameStrings.gettingDark = readStringAt(*stream, 0x6c85);

	// Load other strings from executable
	_strings.playAgain = readStringAt(*stream, 0x5f1e);
	_strings.pressReturn = readStringAt(*stream, 0x5f67);
	_strings.lineFeeds = readStringAt(*stream, 0x59d4);
}

void HiRes1Engine_Malibu::loadExeStrings() {
	loadCommonExeStrings();

	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_1));
	
	_strings.enterCommand = readStringAt(*stream, 0x5bbd);
	_strings.verbError = readStringAt(*stream, 0x5b4f);
	_strings.nounError = readStringAt(*stream, 0x5b8e);
}

void HiRes1Engine_Malibu::init() {
	HiRes1Engine::init();

	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_1));

	// Load additional message offsets
	stream->seek(0x4e50);
	for (uint i = 0; i < 5; ++i)
		_messages.push_back(_files->getDataBlock(IDS_HR1_MESSAGES, stream->readUint16LE()));

	// Load the French verb list
	stream->seek(0x900);
	loadWords(*stream, _verbs, _priVerbs);
}

Common::String HiRes1Engine_Malibu::formatVerbError(const Common::String &verb) const {
	Common::String err = _strings.verbError;
	for (uint i = 0; i < verb.size(); ++i)
		err.setChar(verb[i], i + 15);
	return err;
}

Common::String HiRes1Engine_Malibu::formatNounError(const Common::String &verb, const Common::String &noun) const {
	Common::String err = _strings.nounError;
	for (uint i = 0; i < verb.size(); ++i)
		err.setChar(verb[i], i + 15);
	for (uint i = 0; i < noun.size(); ++i)
		err.setChar(noun[i], i + 31);
	return err;
}

class HiRes1Engine_Malibu_v2 : public HiRes1Engine_Malibu {
public:
	HiRes1Engine_Malibu_v2(OSystem *syst, const AdlGameDescription *gd) :
			HiRes1Engine_Malibu(syst, gd) { }

private:
	// AdlEngine
	void runIntro() override;
	void getInput(uint &verb, uint &noun) override;

	// HiRes1Engine
	void loadExeStrings() override;
};

void HiRes1Engine_Malibu_v2::getInput(uint &verb, uint &noun) {
	// This version has a modified "parser"
	while (1) {
		_display->printString(_strings.enterCommand);
		Common::String line = getLine();

		if (shouldQuit() || _isRestoring)
			return;

		uint index = 0;
		Common::String verbString = getWord(line, index);

		if (!_verbs.contains(verbString)) {
			// If the verb is not found and it looks like an imperative, try to build the infinitive
			size_t ezPos = verbString.find("\xc5\xda"); // "EZ"

			bool found = false;

			if (ezPos != verbString.npos) {
				const char *suf[] = { "\xc5\xd2", "\xc9\xd2", "\xd2\xc5", nullptr }; // "ER", "IR", "RE"

				for (uint i = 0; suf[i]; ++i) {
					verbString.replace(ezPos, 2, suf[i]);
					if (_verbs.contains(verbString)) {
						found = true;
						break;
					}
				}
			}

			if (!found) {
				_display->printString(formatVerbError(verbString));
				continue;
			}
		}

		verb = _verbs[verbString];

		while (1) {
			// Go over all nouns to find one we know. At the end of the string,
			// it will always match the empty word (which is in the noun list).

			// The original has a code path to return a noun error here, but
			// it appears to be non-functional.
			Common::String nounString = getWord(line, index);

			if (_nouns.contains(nounString)) {
				noun = _nouns[nounString];
				return;
			}
		}
	}
}

void HiRes1Engine_Malibu_v2::runIntro() {
	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_0));

	stream->seek(0x1000);

	// Title image is one padding byte short, so we first read it into a buffer
	const uint frameBufferSize = 0x2000;
	byte *frameBuffer = (byte *)malloc(frameBufferSize);

	if (stream->read(frameBuffer, frameBufferSize - 1) < frameBufferSize - 1)
		error("Failed to read title image");

	// Set missing byte
	frameBuffer[frameBufferSize - 1] = 0;
	Common::MemoryReadStream frameBufferStream(frameBuffer, frameBufferSize, DisposeAfterUse::YES);

	_display->setMode(Display::kModeGraphics);
	static_cast<Display_A2 *>(_display)->loadFrameBuffer(frameBufferStream);
	_display->renderGraphics();

	// NOTE: Missing in original game, causing key prompt not to show
	_display->setMode(Display::kModeMixed);

	Common::String str = readStringAt(*stream, 0xc);

	while (1) {
		_display->printString(str);
		char key = inputKey();

		if (shouldQuit())
			return;

		if (key == _display->asciiToNative('M')) {
			const uint pages[] = { 6, 4, 3, 7, 7, 7, 0 };
			stream->seek(0x7b);
			showInstructions(*stream, pages, true);
			_display->printAsciiString("\r");
			return;
		} else if (key == _display->asciiToNative('J')) {
			return;
		}
	}
}

void HiRes1Engine_Malibu_v2::loadExeStrings() {
	loadCommonExeStrings();

	StreamPtr stream(_files->createReadStream(IDS_HR1_EXE_1));
	
	_strings.enterCommand = readStringAt(*stream, 0x5b1b);
	_strings.verbError = readStringAt(*stream, 0x5aad);
	_strings.nounError = readStringAt(*stream, 0x5aec);
}

Engine *HiRes1Engine_create(OSystem *syst, const AdlGameDescription *gd) {
	switch (gd->version) {
	case GAME_VER_HR1_MALIBU_V1:
		return new HiRes1Engine_Malibu(syst, gd);
	case GAME_VER_HR1_MALIBU_V2:
		return new HiRes1Engine_Malibu_v2(syst, gd);
	default:
		return new HiRes1Engine(syst, gd);
	}
}

} // End of namespace Adl
