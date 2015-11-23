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

#include "common/debug.h"
#include "audio/fmopl.h"
#include "audio/mididrv.h"
#include "audio/musicplugin.h"

namespace OPL {
namespace FPGA {

class OPL : public ::OPL::RealOPL {
private:
	Config::OplType _type;
	int index[2];
	MidiDriver *_midi;

	void writeOplReg(int c, int r, int v);
	MidiDriver::DeviceHandle getDeviceByName(const Common::String &name);

public:
	OPL(Config::OplType type);
	~OPL();

	bool init();
	void reset();

	void write(int a, int v);
	byte read(int a);

	void writeReg(int r, int v);
};

OPL::OPL(Config::OplType type) : _type(type), _midi(nullptr) {
}

OPL::~OPL() {
	stop();

	if (_midi->isOpen())
		_midi->close();

	delete _midi;
}

MidiDriver::DeviceHandle OPL::getDeviceByName(const Common::String &name) {
	MusicPlugin::List p = MusicMan.getPlugins();

	for (MusicPlugin::List::const_iterator m = p.begin(); m != p.end(); m++) {
		MusicDevices i = (**m)->getDevices();
		for (MusicDevices::iterator d = i.begin(); d != i.end(); d++) {
			if (d->getName().equals(name)) {
				MidiDriver::DeviceHandle hdl = d->getHandle();
				if (MidiDriver::checkDevice(hdl))
					return hdl;
			}
		}
	}

	return 0;
}

bool OPL::init() {
	// This requires that MIDI backends report device names. Currently
	// the CoreMIDI backend doesn't do this
	MidiDriver::DeviceHandle dev = getDeviceByName("OPL3 FPGA");
	if (!dev) {
		warning("Could not find OPL3 FPGA MIDI device");
		return false;
	}

	_midi = MidiDriver::createMidi(dev);
	if (!_midi || _midi->open())
		return false;

	reset();

	return true;
}

void OPL::reset() {
	index[0] = index[1] = 0;

	// Send a special sysEx to reset the OPL3
	byte sysEx[4] = { 0x7d, 0x7f, 0x7f, 0x7f };
	_midi->sysEx(sysEx, 4);

	if (_type == Config::kDualOpl2) {
		// Here we set up the OPL3 for dual OPL2 mode
		writeOplReg(1, 0x05, 0x01); // Enable OPL3

		// Ensure panning bits are set (see writeOplReg)
		for (int i = 0; i < 9; ++i)
			writeOplReg(0, 0xc0 | i, 0);
		for (int i = 0; i < 9; ++i)
			writeOplReg(1, 0xc0 | i, 0);
	}
}

void OPL::write(int port, int val) {
	val &= 0xff;
	int chip = (port & 2) >> 1;

	if (port & 1) {
		switch(_type) {
		case Config::kOpl2:
			writeOplReg(0, index[0], val);
			break;
		case Config::kDualOpl2:
			if (port & 8) {
				writeOplReg(0, index[0], val);
				writeOplReg(1, index[1], val);
			} else
				writeOplReg(chip, index[chip], val);
			break;
		case Config::kOpl3:
			writeOplReg(chip, index[chip], val);
		}
	} else {
		switch(_type) {
		case Config::kOpl2:
			index[0] = val;
			break;
		case Config::kDualOpl2:
			if (port & 8) {
				index[0] = val;
				index[1] = val;
			} else
				index[chip] = val;
			break;
		case Config::kOpl3:
			index[chip] = val;
		}
	}
}

byte OPL::read(int port) {
	return 0;
}

void OPL::writeReg(int r, int v) {
	switch (_type) {
	case Config::kOpl2:
		writeOplReg(0, r, v);
		break;
	case Config::kDualOpl2:
		writeOplReg(0, r, v);
		writeOplReg(1, r, v);
		break;
	case Config::kOpl3:
		writeOplReg(r >= 0x100, r & 0xff, v);
	}
}

void OPL::writeOplReg(int c, int r, int v) {
	// Set panning bits for dual OPL2 mode
	if (_type == Config::kDualOpl2 && (r & 0xe0) == 0xc0)
		v |= (c ? 0x20 : 0x10);

	// SysEx method
	byte sysEx[4];
	sysEx[0] = 0x7d; // Manufacturer ID (Educational Use)
	sysEx[1] = ((c & 1) << 6) | (r >> 2);
	sysEx[2] = ((r & 3) << 5) | (v >> 3);
	sysEx[3] = (v & 7) << 4;
	_midi->sysEx(sysEx, 4);

#if 0
	// NRPN method
	_midi->send(0x63b0 | ((c & 1) << 17) | ((r >> 7) << 16));
	_midi->send(0x62b0 | ((r & 0x7f) << 16));
	_midi->send(0x06b0 | ((v >> 7) << 16));
	_midi->send(0x26b0 | (v & 0x7f) << 16);
#endif
}

OPL *create(Config::OplType type) {
	return new OPL(type);
}

} // End of namespace FPGA
} // End of namespace OPL
