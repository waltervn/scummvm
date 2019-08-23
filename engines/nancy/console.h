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

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#ifndef NANCY_CONSOLE_H
#define NANCY_CONSOLE_H

#include "gui/debugger.h"

namespace Nancy {

class NancyEngine;

class NancyConsole : public GUI::Debugger {
public:
	NancyConsole(NancyEngine *vm);
	virtual ~NancyConsole(void);

	void postEnter();

private:
	NancyEngine *_vm;
	bool Cmd_loadCal(int argc, const char **argv);
	bool Cmd_cifHexDump(int argc, const char **argv);
	bool Cmd_cifExport(int argc, const char **argv);
	bool Cmd_cifList(int argc, const char **argv);
	bool Cmd_cifInfo(int argc, const char **argv);
	bool Cmd_showImage(int argc, const char **argv);
	bool Cmd_playVideo(int argc, const char **argv);
	bool Cmd_playAudio(int argc, const char **argv);

	bool Cmd_listScreens(int argc, const char **argv);
	bool Cmd_listObjects(int argc, const char **argv);
	bool Cmd_getObject(int argc, const char **argv);
	bool Cmd_getAllObjects(int argc, const char **argv);
	bool Cmd_gotoScreen(int argc, const char **argv);
	bool Cmd_boundaries(int argc, const char **argv);

	Common::String _videoFile;
};

} // End of namespace Nancy

#endif
