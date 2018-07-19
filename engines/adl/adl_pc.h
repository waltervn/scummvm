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

#ifndef ADL_ADL_PC_H
#define ADL_ADL_PC_H

#include "adl/adl_v4.h"

namespace Adl {

class AdlEngine_PC : public AdlEngine_v4 {
public:
	AdlEngine_PC(OSystem *syst, const AdlGameDescription *gd) :
		AdlEngine_v4(syst, gd) { }

protected:
	// AdlEngine
	virtual Common::String loadMessage(uint idx) const;
	virtual void printString(const Common::String &str);
	virtual byte convertEvent(const Common::Event &event) const;
	virtual Common::String inputString(const Common::String &prompt);
	virtual Common::String formatVerbError(const Common::String &verb) const;
	virtual void getInput(uint &verb, uint &noun);

	// AdlEngine_v2
	virtual DataBlockPtr readDataBlockPtr(Common::ReadStream &f) const;
	virtual Common::String readMessageString(Common::ReadStream &stream) const;
	virtual void handleTextOverflow();

	void loadRegion(byte region);
	void loadWords(Common::ReadStream &stream);
	void readCommands(Common::ReadStream &stream, Commands &commands);
	void loadRegionMetaData(Common::SeekableReadStream &stream);
	void loadItems(Common::ReadStream &stream);
	void loadItemDescriptions(Common::SeekableReadStream &stream, byte count);
	void fixupDiskOffset(byte &track, byte &sector) const;

	void newLine();
	void printChar(char c);
	void printString(const Common::String &str, uint offset, uint len);
	uint countLeadingSpaces(const Common::String &str, uint offset);
	uint findStringWrapLen(const Common::String &str, uint offset);
	void printStringWrap(const Common::String &str, uint offset, uint len);
	uint getInputWord(const Common::String &word) const;

	Common::Array<byte> _regionRoomCounts;
	Common::Point _cursorPos;

	struct {
		Common::String keyF3, keyF4, keyF5, keyF6;
	} _strings_PC;
};

} // End of namespace Adl

#endif
