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

#include "adl/adl_v4.h"
#include "adl/display.h"
#include "adl/graphics.h"
#include "adl/disk.h"

namespace Adl {

class HiRes2Engine_PC : public AdlEngine_v4_PC {
public:
	HiRes2Engine_PC(OSystem *syst, const AdlGameDescription *gd) :
		AdlEngine_v4_PC(syst, gd) { }

	// AdlEngine
	void init();
	void initGameState();
};

Engine *HiRes2Engine_PC_create(OSystem *syst, const AdlGameDescription *gd) {
	return new HiRes2Engine_PC(syst, gd);
}

void HiRes2Engine_PC::init() {

}

void HiRes2Engine_PC::initGameState() {
    
}

} // End of namespace Adl
