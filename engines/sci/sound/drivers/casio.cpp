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

/* Casio CSM-1/CT-460 and MT-540 driver.
 * NOTE: Push Rhythm/CH4 button before starting game!
 * 
 * Probably also compatible: CT-660, MT-240
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

	MidiPlayer_Casio(SciVersion version, bool modeMt540);
	virtual ~MidiPlayer_Casio();

	// MidiPlayer
	int open(ResourceManager *resMan) override;
	void close() override;
	void send(uint32 b) override;
	void initTrack(SciSpan<const byte> &header) override;
	bool hasRhythmChannel() const override { return false; } // Depends on play flag
	byte getPlayId() const override { return 0x08; }
	int getPolyphony() const override { return (_modeMt540 ? 16 : 10); }
	void setVolume(byte volume) override;
	int getVolume() override { return _masterVolume; }
	void playSwitch(bool play) override;
	const char *reportMissingFiles() override { return _missingFiles; }

private:
	byte mapNote(byte voice, byte note);
	void noteOn(byte voice, byte note, byte velocity);
	void noteOff(byte voice, byte note);
	void allNotesOff(byte voice);
	void swapVoice(byte voice);
	void setPatch(byte voice, byte patch);
	void controlChange(int channel, int control, int value);

	bool _modeMt540;
	static const uint _polyphony[kVoices];
	byte _rhythmMap[kRhythm];
	byte _patchMap1[kPatches];
	byte _fixedNote1[kPatches];
	byte _patchMap2[kPatches];
	byte _fixedNote2[kPatches];
	int8 _notes[kVoices][kNoteArraySize];
	int8 _voicePatch[kVoices];
	byte _voiceFixedNote1[kVoices];
	int8 _voiceFixedNote2[kVoices];
	int8 _voiceChannel[kVoices];
	int8 _rhythmVoice;
	int8 _transposedVoice;
	byte _masterVolume;
	bool _playSwitch;

	const char *_missingFiles;
};

const uint MidiPlayer_Casio::_polyphony[MidiPlayer_Casio::kVoices] = { 6, 4, 2, 4 };

MidiPlayer_Casio::MidiPlayer_Casio(SciVersion version, bool modeMt540) :
		MidiPlayer(version),
		_modeMt540(modeMt540),
		_rhythmMap(),
		_patchMap1(),
		_fixedNote1(),
		_patchMap2(),
		_fixedNote2(),
		_notes(),
		_voicePatch(),
		_voiceFixedNote1(),
		_voiceFixedNote2(),
		_voiceChannel(),
		_rhythmVoice(),
		_transposedVoice(),
		_masterVolume(15),
		_playSwitch(true),
		_missingFiles(nullptr) {
	MidiDriver::DeviceHandle dev = MidiDriver::detectDevice(MDT_MIDI);
	_driver = MidiDriver::createMidi(dev);
}

MidiPlayer_Casio::~MidiPlayer_Casio() {
	close();
	delete _driver;
}

void MidiPlayer_Casio::initTrack(SciSpan<const byte> &header) {
	uint8 readPos = 0;
	uint8 caps = header.getInt8At(readPos++);
	uint numChan = (caps == 2) ? 15 : 16;
	if (caps != 0 && caps != 2)
		return;

	for (uint voice = 0; voice < kVoices; ++voice) {
		_voiceChannel[voice] = 16;

		// FIXME: There's a race condition between the previous song's all-notes-off
		// and the initTrack for the next song, which could result in hanging notes
		// in this driver. To force all-notes-off we send a bogus patch change here
		_driver->send(0xc0 | voice);

		for (uint i = 0; i < kNoteArraySize; ++i)
			_notes[voice][i] = -1;

		_voicePatch[voice] = -1;
		_voiceFixedNote1[voice] = 0;
		_voiceFixedNote2[voice] = -1;
	}

	_rhythmVoice = -1;

	// Look for a rhythm channel first
	for (uint i = 0; i < numChan; ++i) {
		byte poly = header.getUint8At(readPos++);
		byte flags = header.getUint8At(readPos++);

		// Check rhythm flag and play flag
		if ((poly & 0x80) && (flags & getPlayId())) {
			_voiceChannel[3] = i;
			_rhythmVoice = 3;

			// Set channel 3 to percussion
			const int percPatch = (_modeMt540 ? 8 : 13);
			_driver->send((percPatch << 8) | 0xc3);
		}
	}

	readPos = 1;
	int voice = 0;

	for (uint i = 0; i < numChan; ++i) {
		byte poly = header.getUint8At(readPos++);
		byte flags = header.getUint8At(readPos++);

		// Skip rhythm channel here, as it's handled above
		if ((flags & getPlayId()) && !(poly & 0x80)) {
			if (voice == _rhythmVoice)
				++voice;

			if (voice >= kVoices)
				break;

			_voiceChannel[voice++] = i;
		}
	}

	_transposedVoice = -1;
}

byte MidiPlayer_Casio::mapNote(byte voice, byte note) {
	if (voice == _rhythmVoice) {
		note -= 35;
		if (note >= kRhythm)
			return 0;
		note = _rhythmMap[note];
	} else if (voice == _transposedVoice) {
		note += 24;
		if (note < 60)
			note += 12;
	} else {
		if (_voiceFixedNote2[voice] != -1)
			note = _voiceFixedNote2[voice];
		else if (_voiceFixedNote1[voice] != 0)
			note = _voiceFixedNote1[voice];
	}

	return note;
}

void MidiPlayer_Casio::noteOn(byte voice, byte note, byte velocity) {
	if (!_playSwitch || _masterVolume == 0)
		return;

	if (velocity == 0)
		return noteOff(voice, note);

	note = mapNote(voice, note);

	// The driver goes out of its way to ensure that the maximum polyphony
	// per channel is not exceeded. What's strange is that it does this
	// by dropping extra notes that are coming in, while normally you'd
	// expect it to turn off the oldest note and then play the new one
	// (which also turns out to be the keyboard's standard behaviour,
	// so why is any of this even here...)

	for (uint i = 0; i < _polyphony[voice]; ++i) {
		if (_notes[voice][i] == -1) {
			_notes[voice][i] = note;
			_driver->send((velocity << 16) | (note << 8) | 0x90 | voice);
			return;
		}
	}
}

void MidiPlayer_Casio::noteOff(byte voice, byte note) {
	if (!_playSwitch || _masterVolume == 0)
		return;

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
	SWAP(_voiceFixedNote2[voice], _voiceFixedNote2[2]);
	_voicePatch[voice] = _voicePatch[2];
	// Why is voiceFixedNote1 not swapped?
	_voiceFixedNote1[voice] = _voiceFixedNote1[2];

	// Original driver doesn't check for -1
	if (_voicePatch[voice] != -1)
		_driver->send((_voicePatch[voice] << 8) | 0xc0 | voice);
}

void MidiPlayer_Casio::setPatch(byte voice, byte patch) {
	// NOTE: The MT-540 driver seems to assume there are only 20 tones,
	// while an additional 10 are available via MIDI. It seems likely
	// that the person writing the driver was unaware of this.

	byte cPatch;

	if (patch <= 47) {
		cPatch = _patchMap1[patch];
		_voiceFixedNote2[voice] = -1;
		_voiceFixedNote1[voice] = _fixedNote1[patch];
	} else {
		if (patch >= 96)
			return;

		patch -= 48;
		cPatch = _patchMap2[patch];
		if (_fixedNote2[patch] != 0)
			_voiceFixedNote2[voice] = _fixedNote2[patch];
		else
			// Shouldn't voiceFixedNote1 be set to 0 here?
			_voiceFixedNote2[voice] = -1;
	}

	const int bassPatch = (_modeMt540 ? 18 : 28);

	// When using non-existing patch 20/30 (but see note above), this
	// is interpreted as patch 18/28 transposed up by three octaves
	if (cPatch == bassPatch + 2) {
		// Turn on transpose and adjust patch to 18/28
		_transposedVoice = voice;
		cPatch -= 2;
	} else if (_transposedVoice == voice) {
		// When using a regular patch and transpose is on, disable transpose
		_transposedVoice = -1;
	}

	// If bass is chosen, we move that to the channel with the lowest
	// polyphony, which is channel 2
	if (cPatch == bassPatch && voice != 2) {
		if (_transposedVoice == voice)
			_transposedVoice = 2;
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

void MidiPlayer_Casio::setVolume(byte volume) {
	_masterVolume = volume;

	if (_masterVolume == 0)
		for (uint i = 0; i < kVoices; ++i)
			allNotesOff(i);
}

void MidiPlayer_Casio::playSwitch(bool play) {
	_playSwitch = play;

	if (!_playSwitch)
		for (uint i = 0; i < kVoices; ++i)
			allNotesOff(i);
}

int MidiPlayer_Casio::open(ResourceManager *resMan) {
	assert(resMan != NULL);

	if (_version > SCI_VERSION_0_LATE)
		error("MidiPlayer_Casio: This driver only works with SCI0 games");

	int retval = _driver->open();
	if (retval != 0) {
		warning("MidiPlayer_Casio: Failed to open MIDI driver");
		return retval;
	}

	const uint16 patchNr = (_modeMt540 ? 4 : 7);
	Resource *res = resMan->findResource(ResourceId(kResourceTypePatch, patchNr), 0);

	if (!res) {
		_missingFiles = (_modeMt540 ? "PATCH.004" : "PATCH.007");
		return MidiDriver::MERR_DEVICE_NOT_AVAILABLE;
	}

	if (res->size() != 233)
		error("MidiPlayer_Casio: Failed to read patch %d", patchNr);

	Common::MemoryReadStream stream(res->toStream());

	for (uint i = 0; i < kPatches; ++i) {
		_patchMap1[i] = stream.readByte();
		_fixedNote1[i] = stream.readByte();
	}

	for (uint i = 0; i < kPatches; ++i) {
		_patchMap2[i] = stream.readByte();
		_fixedNote2[i] = stream.readByte();
	}

	for (uint i = 0; i < kRhythm; ++i)
		_rhythmMap[i] = stream.readByte();

	return 0;
}

void MidiPlayer_Casio::close() {
	if (_driver) {
		_driver->setTimerCallback(NULL, NULL);
		_driver->close();
	}
}

MidiPlayer *MidiPlayer_CasioCSM1_create(SciVersion version) {
	return new MidiPlayer_Casio(version, false);
}

MidiPlayer *MidiPlayer_CasioMT540_create(SciVersion version) {
	return new MidiPlayer_Casio(version, true);
}

} // End of namespace Sci
