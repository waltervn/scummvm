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

#include "common/stream.h"
#include "common/rect.h"
#include "common/system.h"
#include "common/str.h"
#include "common/config-manager.h"

#include "graphics/surface.h"
#include "graphics/palette.h"
#include "graphics/thumbnail.h"

#include "engines/util.h"

#include "adl/display.h"
#include "adl/adl.h"

namespace Adl {

#define TEXT_WIDTH 40
#define TEXT_HEIGHT 25

#define DISPLAY_WIDTH 320
#define DISPLAY_HEIGHT 200
#define DISPLAY_PITCH DISPLAY_WIDTH
#define DISPLAY_SIZE (DISPLAY_PITCH * DISPLAY_HEIGHT)

class Display_PC : public Display {
public:
	Display_PC();
	~Display_PC();

	void init();
	void setMode(Mode mode);
	void updateTextScreen();
	void updateHiResScreen();
	bool saveThumbnail(Common::WriteStream &out);

	// Graphics
	void loadFrameBuffer(Common::ReadStream &stream, byte *dst);
	void loadFrameBuffer(Common::ReadStream &stream);
	void putPixel(const Common::Point &p, byte color);
	void setPixelByte(const Common::Point &p, byte color);
	void setPixelBit(const Common::Point &p, byte color);
	void setPixelPalette(const Common::Point &p, byte color);
	byte getPixelByte(const Common::Point &p) const;
	bool getPixelBit(const Common::Point &p) const;
	void clear(byte color);
	uint getDisplayWidth() const { return DISPLAY_WIDTH; }
	uint getDisplayHeight() const { return DISPLAY_HEIGHT; }
	uint getDisplayPitch() const { return DISPLAY_PITCH; }

	// Text
	void home();
	void moveCursorTo(const Common::Point &pos);
	void moveCursorForward();
	void moveCursorBackward();
	void printChar(char c);
	void printString(const Common::String &str);
	void printAsciiString(const Common::String &str);
	void setCharAtCursor(byte c);
	void showCursor(bool enable);
	uint getTextWidth() const { return TEXT_WIDTH; }
	uint getTextHeight() const { return TEXT_HEIGHT; }

private:
	void writeFrameBuffer(const Common::Point &p, byte color, byte mask);
	void updateHiResSurface();
	void showScanlines(bool enable);

	void updateTextSurface();
	void drawChar(byte c, int x, int y);
	void createFont();
	void scrollUp();

	Mode _mode;

	byte *_frameBuf;
	Graphics::Surface *_frameBufSurface;
	bool _scanlines;
	bool _monochrome;

	byte *_textBuf;
	Graphics::Surface *_textBufSurface;
	Graphics::Surface *_font;
	uint _cursorPos;
	bool _showCursor;
	uint32 _startMillis;
};

#define TEXT_BUF_SIZE (TEXT_WIDTH * TEXT_HEIGHT)

#define COLOR_PALETTE_ENTRIES 8
static const byte colorPalette[COLOR_PALETTE_ENTRIES * 3] = {
	0x00, 0x00, 0x00,
	0xff, 0xff, 0xff,
	0xc7, 0x34, 0xff,
	0x38, 0xcb, 0x00,
	0x00, 0x00, 0x00,
	0xff, 0xff, 0xff,
	0x0d, 0xa1, 0xff,
	0xf2, 0x5e, 0x00
};

// Opacity of the optional scanlines (percentage)
#define SCANLINE_OPACITY 75

// Corresponding color in second palette
#define PAL2(X) ((X) | 0x04)

// Alternate color for odd pixel rows (for scanlines)
#define ALTCOL(X) ((X) | 0x08)

// Green monochrome palette
#define MONO_PALETTE_ENTRIES 2
static const byte monoPalette[MONO_PALETTE_ENTRIES * 3] = {
	0x00, 0x00, 0x00,
	0x00, 0xc0, 0x01
};

// Public domain font by Marcel Sondaar (IBM)
const byte font[96][8] = {
	{ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},   // U+0020 (space)
	{ 0x18, 0x3C, 0x3C, 0x18, 0x18, 0x00, 0x18, 0x00},   // U+0021 (!)
	{ 0x36, 0x36, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},   // U+0022 (")
	{ 0x36, 0x36, 0x7F, 0x36, 0x7F, 0x36, 0x36, 0x00},   // U+0023 (#)
	{ 0x0C, 0x3E, 0x03, 0x1E, 0x30, 0x1F, 0x0C, 0x00},   // U+0024 ($)
	{ 0x00, 0x63, 0x33, 0x18, 0x0C, 0x66, 0x63, 0x00},   // U+0025 (%)
	{ 0x1C, 0x36, 0x1C, 0x6E, 0x3B, 0x33, 0x6E, 0x00},   // U+0026 (&)
	{ 0x06, 0x06, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00},   // U+0027 (')
	{ 0x18, 0x0C, 0x06, 0x06, 0x06, 0x0C, 0x18, 0x00},   // U+0028 (()
	{ 0x06, 0x0C, 0x18, 0x18, 0x18, 0x0C, 0x06, 0x00},   // U+0029 ())
	{ 0x00, 0x66, 0x3C, 0xFF, 0x3C, 0x66, 0x00, 0x00},   // U+002A (*)
	{ 0x00, 0x0C, 0x0C, 0x3F, 0x0C, 0x0C, 0x00, 0x00},   // U+002B (+)
	{ 0x00, 0x00, 0x00, 0x00, 0x00, 0x0C, 0x0C, 0x06},   // U+002C (,)
	{ 0x00, 0x00, 0x00, 0x3F, 0x00, 0x00, 0x00, 0x00},   // U+002D (-)
	{ 0x00, 0x00, 0x00, 0x00, 0x00, 0x0C, 0x0C, 0x00},   // U+002E (.)
	{ 0x60, 0x30, 0x18, 0x0C, 0x06, 0x03, 0x01, 0x00},   // U+002F (/)
	{ 0x3E, 0x63, 0x73, 0x7B, 0x6F, 0x67, 0x3E, 0x00},   // U+0030 (0)
	{ 0x0C, 0x0E, 0x0C, 0x0C, 0x0C, 0x0C, 0x3F, 0x00},   // U+0031 (1)
	{ 0x1E, 0x33, 0x30, 0x1C, 0x06, 0x33, 0x3F, 0x00},   // U+0032 (2)
	{ 0x1E, 0x33, 0x30, 0x1C, 0x30, 0x33, 0x1E, 0x00},   // U+0033 (3)
	{ 0x38, 0x3C, 0x36, 0x33, 0x7F, 0x30, 0x78, 0x00},   // U+0034 (4)
	{ 0x3F, 0x03, 0x1F, 0x30, 0x30, 0x33, 0x1E, 0x00},   // U+0035 (5)
	{ 0x1C, 0x06, 0x03, 0x1F, 0x33, 0x33, 0x1E, 0x00},   // U+0036 (6)
	{ 0x3F, 0x33, 0x30, 0x18, 0x0C, 0x0C, 0x0C, 0x00},   // U+0037 (7)
	{ 0x1E, 0x33, 0x33, 0x1E, 0x33, 0x33, 0x1E, 0x00},   // U+0038 (8)
	{ 0x1E, 0x33, 0x33, 0x3E, 0x30, 0x18, 0x0E, 0x00},   // U+0039 (9)
	{ 0x00, 0x0C, 0x0C, 0x00, 0x00, 0x0C, 0x0C, 0x00},   // U+003A (:)
	{ 0x00, 0x0C, 0x0C, 0x00, 0x00, 0x0C, 0x0C, 0x06},   // U+003B (//)
	{ 0x18, 0x0C, 0x06, 0x03, 0x06, 0x0C, 0x18, 0x00},   // U+003C (<)
	{ 0x00, 0x00, 0x3F, 0x00, 0x00, 0x3F, 0x00, 0x00},   // U+003D (=)
	{ 0x06, 0x0C, 0x18, 0x30, 0x18, 0x0C, 0x06, 0x00},   // U+003E (>)
	{ 0x1E, 0x33, 0x30, 0x18, 0x0C, 0x00, 0x0C, 0x00},   // U+003F (?)
	{ 0x3E, 0x63, 0x7B, 0x7B, 0x7B, 0x03, 0x1E, 0x00},   // U+0040 (@)
	{ 0x0C, 0x1E, 0x33, 0x33, 0x3F, 0x33, 0x33, 0x00},   // U+0041 (A)
	{ 0x3F, 0x66, 0x66, 0x3E, 0x66, 0x66, 0x3F, 0x00},   // U+0042 (B)
	{ 0x3C, 0x66, 0x03, 0x03, 0x03, 0x66, 0x3C, 0x00},   // U+0043 (C)
	{ 0x1F, 0x36, 0x66, 0x66, 0x66, 0x36, 0x1F, 0x00},   // U+0044 (D)
	{ 0x7F, 0x46, 0x16, 0x1E, 0x16, 0x46, 0x7F, 0x00},   // U+0045 (E)
	{ 0x7F, 0x46, 0x16, 0x1E, 0x16, 0x06, 0x0F, 0x00},   // U+0046 (F)
	{ 0x3C, 0x66, 0x03, 0x03, 0x73, 0x66, 0x7C, 0x00},   // U+0047 (G)
	{ 0x33, 0x33, 0x33, 0x3F, 0x33, 0x33, 0x33, 0x00},   // U+0048 (H)
	{ 0x1E, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x1E, 0x00},   // U+0049 (I)
	{ 0x78, 0x30, 0x30, 0x30, 0x33, 0x33, 0x1E, 0x00},   // U+004A (J)
	{ 0x67, 0x66, 0x36, 0x1E, 0x36, 0x66, 0x67, 0x00},   // U+004B (K)
	{ 0x0F, 0x06, 0x06, 0x06, 0x46, 0x66, 0x7F, 0x00},   // U+004C (L)
	{ 0x63, 0x77, 0x7F, 0x7F, 0x6B, 0x63, 0x63, 0x00},   // U+004D (M)
	{ 0x63, 0x67, 0x6F, 0x7B, 0x73, 0x63, 0x63, 0x00},   // U+004E (N)
	{ 0x1C, 0x36, 0x63, 0x63, 0x63, 0x36, 0x1C, 0x00},   // U+004F (O)
	{ 0x3F, 0x66, 0x66, 0x3E, 0x06, 0x06, 0x0F, 0x00},   // U+0050 (P)
	{ 0x1E, 0x33, 0x33, 0x33, 0x3B, 0x1E, 0x38, 0x00},   // U+0051 (Q)
	{ 0x3F, 0x66, 0x66, 0x3E, 0x36, 0x66, 0x67, 0x00},   // U+0052 (R)
	{ 0x1E, 0x33, 0x07, 0x0E, 0x38, 0x33, 0x1E, 0x00},   // U+0053 (S)
	{ 0x3F, 0x2D, 0x0C, 0x0C, 0x0C, 0x0C, 0x1E, 0x00},   // U+0054 (T)
	{ 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x3F, 0x00},   // U+0055 (U)
	{ 0x33, 0x33, 0x33, 0x33, 0x33, 0x1E, 0x0C, 0x00},   // U+0056 (V)
	{ 0x63, 0x63, 0x63, 0x6B, 0x7F, 0x77, 0x63, 0x00},   // U+0057 (W)
	{ 0x63, 0x63, 0x36, 0x1C, 0x1C, 0x36, 0x63, 0x00},   // U+0058 (X)
	{ 0x33, 0x33, 0x33, 0x1E, 0x0C, 0x0C, 0x1E, 0x00},   // U+0059 (Y)
	{ 0x7F, 0x63, 0x31, 0x18, 0x4C, 0x66, 0x7F, 0x00},   // U+005A (Z)
	{ 0x1E, 0x06, 0x06, 0x06, 0x06, 0x06, 0x1E, 0x00},   // U+005B ([)
	{ 0x03, 0x06, 0x0C, 0x18, 0x30, 0x60, 0x40, 0x00},   // U+005C (\)
	{ 0x1E, 0x18, 0x18, 0x18, 0x18, 0x18, 0x1E, 0x00},   // U+005D (])
	{ 0x08, 0x1C, 0x36, 0x63, 0x00, 0x00, 0x00, 0x00},   // U+005E (^)
	{ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF},   // U+005F (_)
	{ 0x0C, 0x0C, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00},   // U+0060 (`)
	{ 0x00, 0x00, 0x1E, 0x30, 0x3E, 0x33, 0x6E, 0x00},   // U+0061 (a)
	{ 0x07, 0x06, 0x06, 0x3E, 0x66, 0x66, 0x3B, 0x00},   // U+0062 (b)
	{ 0x00, 0x00, 0x1E, 0x33, 0x03, 0x33, 0x1E, 0x00},   // U+0063 (c)
	{ 0x38, 0x30, 0x30, 0x3e, 0x33, 0x33, 0x6E, 0x00},   // U+0064 (d)
	{ 0x00, 0x00, 0x1E, 0x33, 0x3f, 0x03, 0x1E, 0x00},   // U+0065 (e)
	{ 0x1C, 0x36, 0x06, 0x0f, 0x06, 0x06, 0x0F, 0x00},   // U+0066 (f)
	{ 0x00, 0x00, 0x6E, 0x33, 0x33, 0x3E, 0x30, 0x1F},   // U+0067 (g)
	{ 0x07, 0x06, 0x36, 0x6E, 0x66, 0x66, 0x67, 0x00},   // U+0068 (h)
	{ 0x0C, 0x00, 0x0E, 0x0C, 0x0C, 0x0C, 0x1E, 0x00},   // U+0069 (i)
	{ 0x30, 0x00, 0x30, 0x30, 0x30, 0x33, 0x33, 0x1E},   // U+006A (j)
	{ 0x07, 0x06, 0x66, 0x36, 0x1E, 0x36, 0x67, 0x00},   // U+006B (k)
	{ 0x0E, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x1E, 0x00},   // U+006C (l)
	{ 0x00, 0x00, 0x33, 0x7F, 0x7F, 0x6B, 0x63, 0x00},   // U+006D (m)
	{ 0x00, 0x00, 0x1F, 0x33, 0x33, 0x33, 0x33, 0x00},   // U+006E (n)
	{ 0x00, 0x00, 0x1E, 0x33, 0x33, 0x33, 0x1E, 0x00},   // U+006F (o)
	{ 0x00, 0x00, 0x3B, 0x66, 0x66, 0x3E, 0x06, 0x0F},   // U+0070 (p)
	{ 0x00, 0x00, 0x6E, 0x33, 0x33, 0x3E, 0x30, 0x78},   // U+0071 (q)
	{ 0x00, 0x00, 0x3B, 0x6E, 0x66, 0x06, 0x0F, 0x00},   // U+0072 (r)
	{ 0x00, 0x00, 0x3E, 0x03, 0x1E, 0x30, 0x1F, 0x00},   // U+0073 (s)
	{ 0x08, 0x0C, 0x3E, 0x0C, 0x0C, 0x2C, 0x18, 0x00},   // U+0074 (t)
	{ 0x00, 0x00, 0x33, 0x33, 0x33, 0x33, 0x6E, 0x00},   // U+0075 (u)
	{ 0x00, 0x00, 0x33, 0x33, 0x33, 0x1E, 0x0C, 0x00},   // U+0076 (v)
	{ 0x00, 0x00, 0x63, 0x6B, 0x7F, 0x7F, 0x36, 0x00},   // U+0077 (w)
	{ 0x00, 0x00, 0x63, 0x36, 0x1C, 0x36, 0x63, 0x00},   // U+0078 (x)
	{ 0x00, 0x00, 0x33, 0x33, 0x33, 0x3E, 0x30, 0x1F},   // U+0079 (y)
	{ 0x00, 0x00, 0x3F, 0x19, 0x0C, 0x26, 0x3F, 0x00},   // U+007A (z)
	{ 0x38, 0x0C, 0x0C, 0x07, 0x0C, 0x0C, 0x38, 0x00},   // U+007B ({)
	{ 0x18, 0x18, 0x18, 0x00, 0x18, 0x18, 0x18, 0x00},   // U+007C (|)
	{ 0x07, 0x0C, 0x0C, 0x38, 0x0C, 0x0C, 0x07, 0x00},   // U+007D (})
	{ 0x6E, 0x3B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},   // U+007E (~)
	{ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}    // U+007F
};

Display_PC::Display_PC() :
		_mode(kModeText),
		_cursorPos(0),
		_showCursor(false) {
}

Display_PC::~Display_PC() {
	delete[] _frameBuf;
	_frameBufSurface->free();
	delete _frameBufSurface;

	delete[] _textBuf;
	_textBufSurface->free();
	delete _textBufSurface;

	_font->free();
	delete _font;
}

void Display_PC::init() {
	initGraphics(DISPLAY_WIDTH, DISPLAY_HEIGHT);

	_monochrome = !ConfMan.getBool("color");
	_scanlines = ConfMan.getBool("scanlines");

	if (_monochrome)
		g_system->getPaletteManager()->setPalette(monoPalette, 0, MONO_PALETTE_ENTRIES);
	else
		g_system->getPaletteManager()->setPalette(colorPalette, 0, COLOR_PALETTE_ENTRIES);

	showScanlines(_scanlines);

	_frameBuf = new byte[DISPLAY_SIZE];
	memset(_frameBuf, 0, DISPLAY_SIZE);
	_frameBufSurface = new Graphics::Surface;
	// We need 2x scaling to properly render the half-pixel shift
	// of the second palette
	_frameBufSurface->create(DISPLAY_WIDTH, DISPLAY_HEIGHT, Graphics::PixelFormat::createFormatCLUT8());

	_textBuf = new byte[TEXT_BUF_SIZE];
	memset(_textBuf, (byte)APPLECHAR(' '), TEXT_BUF_SIZE);
	_textBufSurface = new Graphics::Surface;
	// For ease of copying, also use 2x scaling here
	_textBufSurface->create(DISPLAY_WIDTH, DISPLAY_HEIGHT, Graphics::PixelFormat::createFormatCLUT8());

	createFont();

	_startMillis = g_system->getMillis();
}

void Display_PC::setMode(Mode mode) {
	_mode = mode;

	if (_mode == kModeText || _mode == kModeMixed)
		updateTextScreen();
	if (_mode == kModeGraphics || _mode == kModeMixed)
		updateHiResScreen();
}

void Display_PC::updateTextScreen() {
	updateTextSurface();

	if (_mode == kModeText)
		g_system->copyRectToScreen(_textBufSurface->getPixels(), _textBufSurface->pitch, 0, 0, _textBufSurface->w, _textBufSurface->h);
	else if (_mode == kModeMixed)
		g_system->copyRectToScreen(_textBufSurface->getBasePtr(0, _textBufSurface->h - 4 * 8), _textBufSurface->pitch, 0, _textBufSurface->h - 4 * 8, _textBufSurface->w, 4 * 8);

	g_system->updateScreen();
}

void Display_PC::updateHiResScreen() {
}

bool Display_PC::saveThumbnail(Common::WriteStream &out) {
	return Graphics::saveThumbnail(out);
}

void Display_PC::loadFrameBuffer(Common::ReadStream &stream, byte *dst) {
}

void Display_PC::loadFrameBuffer(Common::ReadStream &stream) {
}

void Display_PC::putPixel(const Common::Point &p, byte color) {
}

void Display_PC::setPixelByte(const Common::Point &p, byte color) {
}

void Display_PC::setPixelBit(const Common::Point &p, byte color) {
}

void Display_PC::setPixelPalette(const Common::Point &p, byte color) {
}

byte Display_PC::getPixelByte(const Common::Point &p) const {
	return 0;
}

bool Display_PC::getPixelBit(const Common::Point &p) const {
	return false;
}

void Display_PC::clear(byte color) {
}

void Display_PC::home() {
	memset(_textBuf, ' ', TEXT_BUF_SIZE);
	_cursorPos = 0;
}

void Display_PC::moveCursorForward() {
	++_cursorPos;

	if (_cursorPos >= TEXT_BUF_SIZE)
		scrollUp();
}

void Display_PC::moveCursorBackward() {
	if (_cursorPos > 0)
		--_cursorPos;
}

void Display_PC::moveCursorTo(const Common::Point &pos) {
	_cursorPos = pos.y * TEXT_WIDTH + pos.x;

	if (_cursorPos >= TEXT_BUF_SIZE)
		error("Cursor position (%i, %i) out of bounds", pos.x, pos.y);
}

// FIXME: This does not currently update the surfaces
void Display_PC::printChar(char c) {
	if (c == '\r')
		_cursorPos = (_cursorPos / TEXT_WIDTH + 1) * TEXT_WIDTH;
	else if ((byte)c < 0x80) {
		setCharAtCursor(c);
		++_cursorPos;
	}

	if (_cursorPos == TEXT_BUF_SIZE)
		scrollUp();
}

void Display_PC::printString(const Common::String &str) {
	Common::String::const_iterator c;
	for (c = str.begin(); c != str.end(); ++c)
		printChar(*c);

	updateTextScreen();
}

void Display_PC::printAsciiString(const Common::String &str) {
	printString(str);
}

void Display_PC::setCharAtCursor(byte c) {
	_textBuf[_cursorPos] = c;
}

void Display_PC::showCursor(bool enable) {
	_showCursor = enable;
}

void Display_PC::writeFrameBuffer(const Common::Point &p, byte color, byte mask) {
}

void Display_PC::showScanlines(bool enable) {
}

void Display_PC::updateHiResSurface() {
}

void Display_PC::updateTextSurface() {
	for (uint row = 0; row < TEXT_HEIGHT; ++row)
		for (uint col = 0; col < TEXT_WIDTH; ++col) {
			uint charPos = row * TEXT_WIDTH + col;
			char c = _textBuf[row * TEXT_WIDTH + col];

//			if (charPos == _cursorPos && _showCursor)
//				c = (c & 0x3f) | 0x40;

			Common::Rect r(8, 8);
			r.translate((((c & 0x7f) - 0x20)  % 16) * 8, ((c & 0x7f) - 0x20) / 16 * 8);

#if 0
			if (!(c & 0x80)) {
				// Blink text. We subtract _startMillis to make this compatible
				// with the event recorder, which returns offsetted values on
				// playback.
				const uint32 millisPassed = g_system->getMillis() - _startMillis;
				if (!(c & 0x40) || ((millisPassed / 270) & 1))
					r.translate(0, 4 * 8);
			}
#endif
			_textBufSurface->copyRectToSurface(*_font, col * 8, row * 8, r);
		}
}

void Display_PC::drawChar(byte c, int x, int y) {
	byte *buf = (byte *)_font->getPixels() + y * _font->pitch + x;

	for (uint row = 0; row < 8; ++row) {
		for (uint col = 0; col < 8; ++col) {
			if (font[c][row] & (1 << col))
				buf[col] = 1;
		}

		buf += _font->pitch;
	}
}

void Display_PC::createFont() {
	_font = new Graphics::Surface;
	_font->create(16 * 8, 6 * 8 * 2, Graphics::PixelFormat::createFormatCLUT8());

	for (uint i = 0; i < 6; ++i)
		for (uint j = 0; j < 16; ++j)
			drawChar(i * 16 + j, j * 8, i * 8);

	// Create inverted font
	byte *buf = (byte *)_font->getPixels();
	byte *bufInv = buf + (_font->h / 2) * _font->pitch;
#if 0
	for (uint row = 0; row < _font->h / 2; ++row) {
		for (uint col = 0; col < _font->w; ++col)
			bufInv[col] = (buf[col] ? 0 : 1);

		buf += _font->pitch;
		bufInv += _font->pitch;
	}
#endif
}

void Display_PC::scrollUp() {
	memmove(_textBuf, _textBuf + TEXT_WIDTH, TEXT_BUF_SIZE - TEXT_WIDTH);
	memset(_textBuf + TEXT_BUF_SIZE - TEXT_WIDTH, (byte)APPLECHAR(' '), TEXT_WIDTH);
	if (_cursorPos >= TEXT_WIDTH)
		_cursorPos -= TEXT_WIDTH;
}

Display *Display_PC_create() {
	return new Display_PC();
}

} // End of namespace Adl
