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

#ifndef ADL_DISPLAY_H
#define ADL_DISPLAY_H

#include "common/types.h"

namespace Common {
class ReadStream;
class WriteStream;
class String;
struct Point;
}

namespace Graphics {
struct Surface;
}

namespace Adl {

#define APPLECHAR(C) ((char)((C) | 0x80))

class Display {
public:
	enum Mode {
		kModeGraphics,
		kModeText,
		kModeMixed
	};

	virtual ~Display() { }

	virtual void init() { }
	virtual void setMode(Mode mode) { }
	virtual void updateTextScreen() { }
	virtual void updateHiResScreen() { }
	virtual bool saveThumbnail(Common::WriteStream &out) { return false; }

	// Graphics
	virtual void loadFrameBuffer(Common::ReadStream &stream, byte *dst) { }
	virtual void loadFrameBuffer(Common::ReadStream &stream) { }
	virtual void putPixel(const Common::Point &p, byte color) { }
	virtual void setPixelByte(const Common::Point &p, byte color) { }
	virtual void setPixelBit(const Common::Point &p, byte color) { }
	virtual void setPixelPalette(const Common::Point &p, byte color) { }
	virtual byte getPixelByte(const Common::Point &p) const { return 0; }
	virtual bool getPixelBit(const Common::Point &p) const { return false; }
	virtual void clear(byte color) { }
	virtual uint getDisplayWidth() const { return 0; }
	virtual uint getDisplayHeight() const { return 0; }
	virtual uint getDisplayPitch() const { return 0; }
	virtual uint getDisplaySize() const { return 0; }

	// Text
	virtual void home() { }
	virtual void moveCursorTo(const Common::Point &pos) { }
	virtual void moveCursorForward() { }
	virtual void moveCursorBackward() { }
	virtual void printChar(char c) { }
	virtual void printString(const Common::String &str) { }
	virtual void printAsciiString(const Common::String &str) { }
	virtual void setCharAtCursor(byte c) { }
	virtual void showCursor(bool enable) { }
	virtual uint getTextWidth() const { return 0; }
	virtual uint getTextHeight() const { return 0; }
	virtual char asciiToNative(char c) const { return c; }
};

Display *Display_A2_create();
Display *Display_PC_create();

} // End of namespace Adl

#endif
