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

#ifndef NANCY_RESOURCE_H
#define NANCY_RESOURCE_H

namespace Common {
class String;
}

namespace Graphics {
class Surface;
}

namespace Nancy {

class NancyEngine;
class CifTree;
class Decompressor;

class ResourceManager {
public:
	enum ResType {
		kResTypeAny,
		// Type 1 seems to be obsolete
		kResTypeImage = 2,
		kResTypeData
	};

	enum ResCompression {
		kResCompressionNone = 1,
		kResCompression
	};

	struct CifInfo {
		Common::String name;
		byte type; // ResType
		byte comp; // ResCompression
		uint16 width, pitch, height;
		byte depth; // Bit depth
		uint32 compressedSize, size;
	};

	ResourceManager(NancyEngine *vm);
	~ResourceManager();

	void initialize();
	bool loadCifTree(const Common::String &name, const Common::String &ext);
	bool loadImage(const Common::String &treeName, const Common::String &name, Graphics::Surface &surf);
	byte *loadData(const Common::String &treeName, const Common::String &name, uint &size);

	// Debugger functions
	void list(const Common::String &treeName, Common::Array<Common::String> &nameList, uint type);
	byte *loadCif(const Common::String &treeName, const Common::String &name, uint &size);
	bool exportCif(const Common::String &treeName, const Common::String &name);
	Common::String getCifDescription(const Common::String &treeName, const Common::String &name);
private:
	NancyEngine *_vm;
	Decompressor *_dec;

	byte *getCifData(const Common::String &treeName, const Common::String &name, CifInfo &info, uint *size = 0);
	bool getCifInfo(const Common::String &treeName, const Common::String &name, CifInfo &info);
	const CifTree *findCifTree(const Common::String &name) const;

	Common::Array<const CifTree *> _cifTrees;
};

} // End of namespace Nancy

#endif
