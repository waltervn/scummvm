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

/* Casio CT-460/CSM-1 and MT-540 driver.
 * NOTE: Push Rhythm/CH4 button before starting game!
 */

#include "sci/resource.h"
#include "sci/sound/drivers/mididriver.h"

namespace Sci {

class MidiPlayer_Casio : public MidiPlayer {
public:
	enum {
		kVoices = 4,
		kPatches = 48,
		kRhythm = 41,
		kNoteArraySize = 8
	};

	MidiPlayer_Casio(SciVersion version, int patchNr, int percPatch, int bassPatch);
	~MidiPlayer_Casio();

	int open(ResourceManager *resMan);
	void close();
	void send(uint32 b);
	bool hasRhythmChannel() const { return false; } // Depends on play flag
	byte getPlayId() const;
	int getPolyphony() const { return 10; }
	int getFirstChannel() const;
	int getLastChannel() const;
	void setVolume(byte volume);
	int getVolume();
	void playSwitch(bool play);
	void onNewSound(uint16 channels);

private:
	byte mapNote(byte voice, byte note);
	void noteOn(byte voice, byte note, byte velocity);
	void noteOff(byte voice, byte note);
	void allNotesOff(byte voice);
	void swapVoice(byte voice);
	void setPatch(byte voice, byte patch);
	void controlChange(int channel, int control, int value);

	static const uint _polyphony[kVoices];
	const int _patchNr;
	const int _percPatch;
	const int _bassPatch;
	byte _rhythmMap[kRhythm];
	byte _patchMap1[kPatches];
	byte _fixedNote1[kPatches];
	byte _patchMap2[kPatches];
	byte _fixedNote2[kPatches];
	int8 _notes[kVoices][kNoteArraySize];
	int8 _voicePatch[kVoices];
	byte _voiceForcedFixedNote[kVoices];
	int8 _voiceFixedNote[kVoices];
	int8 _voiceChannel[kVoices];
	int8 _rhythmVoice;
	int8 _bassVoice;
	int8 _prevSentCommand;
};

const uint MidiPlayer_Casio::_polyphony[MidiPlayer_Casio::kVoices] = { 6, 4, 2, 6 };

MidiPlayer_Casio::MidiPlayer_Casio(SciVersion version, int patchNr, int percPatch, int bassPatch) :
		MidiPlayer(version),
		_patchNr(patchNr),
		_percPatch(percPatch),
		_bassPatch(bassPatch),
		_rhythmMap(),
		_patchMap1(),
		_fixedNote1(),
		_patchMap2(),
		_fixedNote2(),
		_notes(),
		_voicePatch(),
		_voiceForcedFixedNote(),
		_voiceFixedNote(),
		_voiceChannel(),
		_rhythmVoice(),
		_bassVoice(),
		_prevSentCommand() {
	MidiDriver::DeviceHandle dev = MidiDriver::detectDevice(MDT_MIDI);
	_driver = MidiDriver::createMidi(dev);
}

MidiPlayer_Casio::~MidiPlayer_Casio() {
	delete _driver;
}

void MidiPlayer_Casio::onNewSound(uint16 channels) {
	debug("NEW SOUND");
	memset(_voiceChannel, MIDI_CHANNELS, sizeof(_voiceChannel));
	memset(_notes, -1, sizeof(_notes));
	memset(_voicePatch, -1, sizeof(_voicePatch));
	memset(_voiceForcedFixedNote, 0, sizeof(_voiceForcedFixedNote));
	memset(_voiceFixedNote, -1, sizeof(_voiceFixedNote));

	_rhythmVoice = -1;

	// Look for a rhythm channel first. SSCI scans for a channel that has
	// bit 7 set of its polyphony byte. We may need to do that too, if
	// there are songs with rhythm on channel != 9.
	if (channels & (1 << 9)) {
		_voiceChannel[3] = 9;
		_rhythmVoice = 3;
		// Set channel 3 to percussion
		_driver->send((_percPatch << 8) | 0xc3);
	}

	int voice = 0;

	for (uint i = 0; i < MIDI_CHANNELS; ++i) {
		// Skip rhythm channel here, as it's handled above
		if (i == 9)
			continue;

		if (!(channels & (1 << i)))
			continue;

		if (voice == _rhythmVoice)
			++voice;

		if (voice >= kVoices) {
			warning("CASIO: total number of MIDI channels exceeds %d", kVoices);
			break;
		}

		_voiceChannel[voice++] = i;
	}

	_prevSentCommand = -1;
	_bassVoice = -1;
}

byte MidiPlayer_Casio::mapNote(byte voice, byte note) {
	if (voice == _rhythmVoice) {
		note -= 35;
		if (note >= kRhythm) {
			warning("CASIO: invalid rhythm key");
			return 0;
		}
		note = _rhythmMap[note];
	} else if (voice == _bassVoice) {
		note += 24;
		if (note < 60)
			note += 12;
	} else {
		if (_voiceFixedNote[voice] != -1)
			note = _voiceFixedNote[voice];
		else if (_voiceForcedFixedNote[voice] != 0)
			note = _voiceForcedFixedNote[voice];
	}

	return note;
}

void MidiPlayer_Casio::noteOn(byte voice, byte note, byte velocity) {
	if (velocity == 0)
		return noteOff(voice, note);

	note = mapNote(voice, note);

	for (uint i = 0; i < _polyphony[voice]; ++i) {
		if (_notes[voice][i] == -1) {
			_notes[voice][i] = note;
			_driver->send((velocity << 16) | (note << 8) | 0x90 | voice);
			return;
		}
	}
}

void MidiPlayer_Casio::noteOff(byte voice, byte note) {
	note = mapNote(voice, note);

	for (uint i = 0; i < _polyphony[voice]; ++i) {
		if (_notes[voice][i] == note) {
			_notes[voice][i] = -1;
			_driver->send((0x40 << 16) | (note << 8) | 0x80 | voice);
			return;
		}
	}
}

void MidiPlayer_Casio::allNotesOff(byte voice) {
	for (uint i = 0; i < _polyphony[voice]; ++i) {
		if (_notes[voice][i] != -1) {
			_driver->send((0x40 << 16) | (_notes[voice][i] << 8) |  0x80 | voice);
			_notes[voice][i] = -1;
		}
	}	
}

void MidiPlayer_Casio::swapVoice(byte voice) {
	for (uint i = 0; i < kNoteArraySize; ++i)
		SWAP(_notes[voice][i], _notes[2][i]);
	SWAP(_voiceChannel[voice], _voiceChannel[2]);
	SWAP(_voiceFixedNote[voice], _voiceFixedNote[2]);
	_voicePatch[voice] = _voicePatch[2];
	_voiceForcedFixedNote[voice] = _voiceForcedFixedNote[2];

	// Original driver doesn't check for -1
	if (_voicePatch[voice] != -1)
		_driver->send((_voicePatch[voice] << 8) | 0xc0 | voice);
}

void MidiPlayer_Casio::setPatch(byte voice, byte patch) {
	byte cPatch;

	if (patch <= 47) {
		cPatch = _patchMap1[patch];
		_voiceFixedNote[voice] = -1;
		_voiceForcedFixedNote[voice] = _fixedNote1[patch];
	} else {
		if (patch >= 96) {
			warning("CASIO: patch number %d out of bounds", patch);
			return;
		}

		patch -= 48;
		cPatch = _patchMap2[patch];
		if (_fixedNote2[patch] != 0)
			_voiceFixedNote[voice] = _fixedNote2[patch];
		else
			_voiceFixedNote[voice] = -1;
	}

	if (cPatch == _bassPatch + 2) {
		_bassVoice = voice;
		cPatch -= 2;
	} else {
		if (_bassVoice == voice)
			_bassVoice = -1;
	}

	if (cPatch == _bassPatch && voice != 2) {
		if (_bassVoice == voice)
			_bassVoice = 2;
		swapVoice(voice);
		voice = 2;
	}

	_voicePatch[voice] = cPatch;
	_driver->send((cPatch << 8) | 0xc0 | voice);
}

void MidiPlayer_Casio::send(uint32 b) {
	byte command = b & 0xf0;
	byte channel = b & 0xf;
	byte op1 = (b >> 8) & 0x7f;
	byte op2 = (b >> 16) & 0x7f;

	int16 voice = -1;
	for (uint i = 0; i < kVoices; ++i) {
		if (_voiceChannel[i] == channel) {
			voice = i;
			break;
		}
	}

	if (voice == -1)
		return;

	switch (command) {
	case 0x80:
		noteOff(voice, op1);
		break;
	case 0x90:
		noteOn(voice, op1, op2);
		break;
	case 0xb0:
		switch (op1) {
		case 0x40:
			_driver->send((op2 << 16) | (op1 << 8) | 0xb0 | voice);
			break;
		case 0x7b:
			// Not in original driver
			allNotesOff(voice);
		};
		break;
	case 0xc0:
		if (voice != _rhythmVoice)
			setPatch(voice, op1);
	}
}

int MidiPlayer_Casio::getFirstChannel() const {
	return 0;
}

int MidiPlayer_Casio::getLastChannel() const {
	return 3;
}

void MidiPlayer_Casio::setVolume(byte volume) {
}

int MidiPlayer_Casio::getVolume() {
	return 15;
}

void MidiPlayer_Casio::playSwitch(bool play) {
}

int MidiPlayer_Casio::open(ResourceManager *resMan) {
	assert(resMan != NULL);

	int retval = _driver->open();
	if (retval != 0) {
		warning("CASIO: failed to open MIDI driver");
		return retval;
	}

	Resource *res = resMan->findResource(ResourceId(kResourceTypePatch, _patchNr), 0);
	if (!res || res->size != 233)
		error("CASIO: failed to read patch %i", _patchNr);

	byte *data = res->data;
	for (uint i = 0; i < kPatches; ++i) {
		_patchMap1[i] = *data++;
		_fixedNote1[i] = *data++;
	}
	for (uint i = 0; i < kPatches; ++i) {
		_patchMap2[i] = *data++;
		_fixedNote2[i] = *data++;
	}

	for (uint i = 0; i < kRhythm; ++i)
		_rhythmMap[i] = *data++;

	return 0;
}

void MidiPlayer_Casio::close() {
	_driver->close();
}

byte MidiPlayer_Casio::getPlayId() const {
	return 0x08;
}

MidiPlayer *MidiPlayer_CasioCT460_create(SciVersion version) {
	return new MidiPlayer_Casio(version, 7, 13, 28);
}

// Untested!
MidiPlayer *MidiPlayer_CasioMT540_create(SciVersion version) {
	return new MidiPlayer_Casio(version, 4, 8, 18);
}

} // End of namespace Sci
