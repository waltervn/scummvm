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

#include "audio/softsynth/emumidi.h"
#include "sci/sound/drivers/mididriver.h"
#include "sci/resource.h"

#include "common/file.h"
#include "common/frac.h"
#include "common/memstream.h"
#include "common/system.h"
#include "common/textconsole.h"
#include "common/util.h"
#include "audio/mods/paula.h"

namespace Sci {

// FIXME: SQ3, LSL2 and HOYLE1 for Amiga don't seem to load any
// patches, even though patches are present. Later games do load
// patches, but include disabled patches with a 'd' appended to the
// filename, e.g. sound.010d. For SQ3, LSL2 and HOYLE1, we should
// probably disable patch loading. Maybe the original interpreter
// loads these disabled patches under some specific condition?

static const uint16 periodTable[] = {
	0x3bb9, 0x3ade, 0x3a05, 0x3930, 0x385e, 0x378f, 0x36c3, 0x35fa,
	0x3534, 0x3471, 0x33b0, 0x32f3, 0x3238, 0x3180, 0x30ca, 0x3017,
	0x2f66, 0x2eb8, 0x2e0d, 0x2d64, 0x2cbd, 0x2c19, 0x2b77, 0x2ad7,
	0x2a3a, 0x299f, 0x2907, 0x2870, 0x27dc, 0x274a, 0x26b9, 0x262b,
	0x259f, 0x2515, 0x248d, 0x2406, 0x2382, 0x2300, 0x227f, 0x2201,
	0x2184, 0x2109, 0x2090, 0x2019, 0x1fa3, 0x1f2f, 0x1ebc, 0x1e4b,
	0x3bb9, 0x3ade, 0x3a05, 0x3930, 0x385e, 0x378f, 0x36c3, 0x35fa,
	0x3534, 0x3471, 0x33b0, 0x32f3, 0x3238, 0x3180, 0x30ca, 0x3017,
	0x2f66, 0x2eb8, 0x2e0d, 0x2d64, 0x2cbd, 0x2c19, 0x2b77, 0x2ad7,
	0x2a3a, 0x299f, 0x2907, 0x2870, 0x27dc, 0x274a, 0x26b9, 0x262b,
	0x259f, 0x2515, 0x248d, 0x2406, 0x2382, 0x2300, 0x227f, 0x2201,
	0x2184, 0x2109, 0x2090, 0x2019, 0x1fa3, 0x1f2f, 0x1ebc, 0x1e4b,
	0x3bb9, 0x3ade, 0x3a05, 0x3930, 0x385e, 0x378f, 0x36c3, 0x35fa,
	0x3534, 0x3471, 0x33b0, 0x32f3, 0x3238, 0x3180, 0x30ca, 0x3017,
	0x2f66, 0x2eb8, 0x2e0d, 0x2d64, 0x2cbd, 0x2c19, 0x2b77, 0x2ad7,
	0x2a3a, 0x299f, 0x2907, 0x2870, 0x27dc, 0x274a, 0x26b9, 0x262b,
	0x259f, 0x2515, 0x248d, 0x2406, 0x2382, 0x2300, 0x227f, 0x2201,
	0x2184, 0x2109, 0x2090, 0x2019, 0x1fa3, 0x1f2f, 0x1ebc, 0x1e4b,
	0x1ddc, 0x1d6e, 0x1d02, 0x1c98, 0x1c2f, 0x1bc8, 0x1b62, 0x1afd,
	0x1a9a, 0x1a38, 0x19d8, 0x1979, 0x191c, 0x18c0, 0x1865, 0x180b,
	0x17b3, 0x175c, 0x1706, 0x16b1, 0x165e, 0x160c, 0x15bb, 0x156c,
	0x151d, 0x14d0, 0x1483, 0x1438, 0x13ee, 0x13a5, 0x135c, 0x1315,
	0x12cf, 0x128a, 0x1246, 0x1203, 0x11c1, 0x1180, 0x1140, 0x1100,
	0x10c2, 0x1084, 0x1048, 0x100c, 0x0fd1, 0x0f97, 0x0f5e, 0x0f26,
	0x0eee, 0x0eb7, 0x0e81, 0x0e4c, 0x0e17, 0x0de3, 0x0db1, 0x0d7e,
	0x0d4d, 0x0d1c, 0x0cec, 0x0cbd, 0x0c8e, 0x0c60, 0x0c32, 0x0c05,
	0x0bd9, 0x0bae, 0x0b83, 0x0b59, 0x0b2f, 0x0b06, 0x0add, 0x0ab5,
	0x0a8e, 0x0a67, 0x0a41, 0x0a1c, 0x09f7, 0x09d2, 0x09ae, 0x098a,
	0x0967, 0x0945, 0x0923, 0x0901, 0x08e0, 0x08c0, 0x08a0, 0x0880,
	0x0861, 0x0842, 0x0824, 0x0806, 0x07e8, 0x07cb, 0x07af, 0x0793,
	0x0777, 0x075b, 0x0740, 0x0725, 0x070b, 0x06f1, 0x06d8, 0x06bf,
	0x06a6, 0x068e, 0x0676, 0x065e, 0x0647, 0x0630, 0x0619, 0x0602,
	0x05ec, 0x05d6, 0x05c1, 0x05ac, 0x0597, 0x0583, 0x056e, 0x055b,
	0x0547, 0x0534, 0x0520, 0x050e, 0x04fb, 0x04e9, 0x04d6, 0x04c5,
	0x04b3, 0x04a2, 0x0491, 0x0480, 0x0470, 0x0460, 0x0450, 0x0440,
	0x0430, 0x0421, 0x0412, 0x0403, 0x03f4, 0x03e5, 0x03d7, 0x03c9,
	0x03bb, 0x03ad, 0x03a0, 0x0392, 0x0385, 0x0378, 0x036c, 0x035f,
	0x0353, 0x0347, 0x033b, 0x032f, 0x0323, 0x0318, 0x030c, 0x0301,
	0x02f6, 0x02eb, 0x02e0, 0x02d6, 0x02cb, 0x02c1, 0x02b7, 0x02ad,
	0x02a3, 0x0299, 0x0290, 0x0286, 0x027d, 0x0274, 0x026b, 0x0262,
	0x0259, 0x0251, 0x0248, 0x0240, 0x0238, 0x0230, 0x0228, 0x0220,
	0x0218, 0x0210, 0x0209, 0x0201, 0x01fa, 0x01f3, 0x01eb, 0x01e4,
	0x01dd, 0x01d6, 0x01cf, 0x01c9, 0x01c2, 0x01bc, 0x01b5, 0x01af,
	0x01a9, 0x01a3, 0x019d, 0x0197, 0x0191, 0x018b, 0x0186, 0x0180,
	0x017b, 0x0175, 0x0170, 0x016a, 0x0165, 0x0160, 0x015b, 0x0156,
	0x0151, 0x014c, 0x0147, 0x0143, 0x013e, 0x0139, 0x0135, 0x0130,
	0x012c, 0x0128, 0x0124, 0x0120, 0x011c, 0x0118, 0x0114, 0x0110,
	0x010c, 0x0108, 0x0104, 0x0101, 0x00fd, 0x00f9, 0x00f5, 0x00f2,
	0x00ee, 0x00eb, 0x00e7, 0x00e4, 0x00e1, 0x00de, 0x00da, 0x00d7,
	0x00d4, 0x00d1, 0x00ce, 0x00cb, 0x00c8, 0x00c5, 0x00c2, 0x00c0,
	0x00bd, 0x00ba, 0x00b7, 0x00b5, 0x00b2, 0x00af, 0x00ad, 0x00aa,
	0x00a8, 0x00a6, 0x00a3, 0x00a1, 0x009f, 0x009d, 0x009a, 0x0098,
	0x0096, 0x0094, 0x0092, 0x0090, 0x008e, 0x008c, 0x008a, 0x0088,
	0x0086, 0x0084, 0x0082, 0x0080, 0x007e
};

class MidiDriver_AmigaSci0 : public MidiDriver, public Audio::Paula {
public:
	MidiDriver_AmigaSci0(Audio::Mixer *mixer);
	virtual ~MidiDriver_AmigaSci0();

	// MidiDriver
	int open();
	void close();
	void initTrack(SciSpan<const byte> &);
	void send(uint32 b);
	MidiChannel *allocateChannel() { return NULL; }
	MidiChannel *getPercussionChannel() { return NULL; }
	void setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc);
	uint32 getBaseTempo() { return 1000000 / _baseFreq; }
	bool isOpen() const { return _isOpen; }

	// Audio::Paula
	void interrupt();

	void setVolume(byte volume);
	void playSwitch(bool play) { }

private:
	struct Instrument {
		Instrument() : name(), loop(false), fixedNote(false), seg1Size(0),
					   seg2Offset(0), seg2Size(0), seg3Offset(0), seg3Size(0),
					   samples(nullptr), transpose(0), envelope() {}

		~Instrument() { delete samples; }

		char name[31];
		bool loop;
		bool fixedNote;
		int16 seg1Size;
		uint32 seg2Offset;
		int16 seg2Size;
		uint32 seg3Offset;
		int16 seg3Size;
		const byte *samples;
		int8 transpose;

		struct Envelope {
			byte skip;
			int8 step;
			byte target;
		} envelope[4];
	};

	Common::Array<const Instrument *> _instruments;
	uint _defaultInstrument;

	struct Voice {
		Voice(MidiDriver_AmigaSci0 &driver, byte id) :
			_patch(0),
			_note(-1),
			_velocity(0),
			_pitch(0),
			_instrument(nullptr),
			_loop(false),
			_envState(0),
			_envCntDown(0),
			_envCurVel(0),
			_volume(0),
			_id(id),
			_driver(driver) {}

		void noteOn(int8 note, int8 velocity);
		void noteOff(int8 note);

		void processEnvelope();

		byte _patch;
		int8 _note;
		byte _velocity;
		uint16 _pitch;

		const Instrument *_instrument;
		bool _loop;

		byte _envState;
		byte _envCntDown;
		int8 _envCurVel;

		byte _volume;
		byte _id;
		MidiDriver_AmigaSci0 &_driver;
	};

	Audio::Mixer *_mixer;
	Audio::SoundHandle _mixerSoundHandle;
	Common::TimerManager::TimerProc _timerProc;
	void *_timerParam;
	bool _isOpen;
	int _baseFreq;
	byte _masterVolume;
	bool _playSwitch;
	bool _isEarlyDriver;

	Common::Array<Voice *> _voices;
	typedef Common::Array<Voice *>::const_iterator VoiceIt;

	Voice *_channels[MIDI_CHANNELS];

	bool loadInstruments(Common::SeekableReadStream &patch);
	void freeInstruments();

	void stopVoice(int8 voice);
	void stopVoices();
	bool calcVoiceStep(int8 voice);
	void pitchWheel(int8 voice, int16 pitch);
};


MidiDriver_AmigaSci0::MidiDriver_AmigaSci0(Audio::Mixer *mixer) :
	Audio::Paula(true, mixer->getOutputRate(), mixer->getOutputRate() / 60),
	_defaultInstrument(0),
	_mixer(mixer),
	_timerProc(nullptr),
	_timerParam(nullptr),
	_isOpen(false),
	_baseFreq(60),
	_masterVolume(0),
	_playSwitch(true),
	_channels() {

	switch (g_sci->getGameId()) {
	case GID_HOYLE1:
	case GID_LSL2:
	case GID_LSL3:
	case GID_SQ3:
	case GID_QFG1:
		_isEarlyDriver = true;
		break;
	default:
		_isEarlyDriver = false;
	}
}

void MidiDriver_AmigaSci0::Voice::processEnvelope() {
	const byte state = _envState;

	if (state == 0 || state == 3)
		return;

	if (state == 6) {
		_driver.stopVoice(_id);
		_envState = 0;
		return;
	}

	if (_envCntDown == 0) {
		const uint envIdx = (state > 3 ? state - 2 : state - 1);

		_envCntDown = _instrument->envelope[envIdx].skip;
		int8 velocity = _envCurVel;

		if (velocity <= 0) {
			_driver.stopVoice(_id);
			_envState = 0;
			return;
		}

		if (velocity > 63)
			velocity = 63;

		if (!_driver._playSwitch)
			velocity = 0;

		// Early games ignore note velocity for envelope-enabled notes
		if (_driver._isEarlyDriver)
			_driver.setChannelVolume(_id, velocity * _driver._masterVolume >> 6);
		else
			_driver.setChannelVolume(_id, (velocity * _driver._masterVolume >> 6) * _volume >> 6);

		const int8 step = _instrument->envelope[envIdx].step;
		if (step < 0) {
			_envCurVel -= step;
			if (_envCurVel > _instrument->envelope[envIdx].target) {
				_envCurVel = _instrument->envelope[envIdx].target;
				++_envState;
			}
		} else {
			_envCurVel -= step;
			if (_envCurVel < _instrument->envelope[envIdx].target) {
				_envCurVel = _instrument->envelope[envIdx].target;
				++_envState;
			}
		}
	}

	--_envCntDown;
}

MidiDriver_AmigaSci0::~MidiDriver_AmigaSci0() {
	close();
}

int MidiDriver_AmigaSci0::open() {
	Common::File file;

	if (!file.open("bank.001")) {
		warning("MidiPlayer_Amiga0: Failed to open bank.001");
		return false;
	}

	if (!loadInstruments(file)) {
		freeInstruments();
		return MidiDriver::MERR_DEVICE_NOT_AVAILABLE;
	}

	for (byte vi = 0; vi < NUM_VOICES; ++vi)
		_voices.push_back(new Voice(*this, vi));

	startPaula();
	// Enable reverse stereo to counteract Audio::Paula's reverse stereo
	_mixer->playStream(Audio::Mixer::kPlainSoundType, &_mixerSoundHandle, this, -1, _mixer->kMaxChannelVolume, 0, DisposeAfterUse::NO, false, true);
	_isOpen = true;

	return Common::kNoError;
}

void MidiDriver_AmigaSci0::close() {
	if (!_isOpen)
		return;

	_mixer->stopHandle(_mixerSoundHandle);
	stopPaula();

	for (uint ci = 0; ci < ARRAYSIZE(_channels); ++ci)
		_channels[ci] = nullptr;

	for (VoiceIt v = _voices.begin(); v != _voices.end(); ++v)
		delete *v;
	_voices.clear();

	freeInstruments();

	_isOpen = false;
}

void MidiDriver_AmigaSci0::initTrack(SciSpan<const byte>& header) {
	if (!_isOpen)
		return;

	uint8 readPos = 0;
	const uint8 caps = header.getInt8At(readPos++);

	// We only implement the MIDI functionality here, samples are
	// handled by the generic sample code
	if (caps != 0)
		return;

	uint vi = 0;

	for (uint i = 0; i < 15; ++i) {
		readPos++;
		const uint8 flags = header.getInt8At(readPos++);

		if ((flags & 0x40 /*getPlayId()*/) && (vi < NUM_VOICES))
			_channels[i] = _voices[vi++];
		else
			_channels[i] = nullptr;
	}

	_channels[15] = nullptr;

	for (uint i = 0; i < NUM_VOICES; ++i) {
		_voices[i]->_note = -1;
		_voices[i]->_pitch = 0x2000;
	}
}

void MidiDriver_AmigaSci0::setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc) {
	_timerProc = timer_proc;
	_timerParam = timer_param;
}

void MidiDriver_AmigaSci0::interrupt() {
	if (_timerProc)
		(*_timerProc)(_timerParam);

	for (VoiceIt it = _voices.begin(); it != _voices.end(); ++it)
		(*it)->processEnvelope();
}

void MidiDriver_AmigaSci0::stopVoice(int8 voice) {
	clearVoice(voice);
}

void MidiDriver_AmigaSci0::stopVoices() {
	// FIXME: Why is master volume temporarily set to 0 here?
	byte masterVolume = _masterVolume;
	_masterVolume = 0;

	for (uint i = 0; i < NUM_VOICES; ++i)
		stopVoice(i);

	_masterVolume = masterVolume;
}

bool MidiDriver_AmigaSci0::calcVoiceStep(int8 voice) {
	int8 note = _voices[voice]->_note;

	if (_voices[voice]->_instrument->fixedNote)
		note = 101;

	int16 index = (note + _voices[voice]->_instrument->transpose) * 4;

	if (_voices[voice]->_pitch >= 0x2000)
		index += (_voices[voice]->_pitch - 0x2000) / 171;
	else
		index -= (0x2000 - _voices[voice]->_pitch) / 171;

	if (index < 0 || index >= ARRAYSIZE(periodTable))
		return false;

	setChannelPeriod(voice, periodTable[index]);
	return true;
}

void MidiDriver_AmigaSci0::Voice::noteOn(int8 note, int8 velocity) {
	if (velocity == 0) {
		noteOff(note);
		return;
	}

	_instrument = _driver._instruments[_patch];

	// Default to the first instrument in the bank
	if (!_instrument)
		_instrument = _driver._instruments[_driver._defaultInstrument];

	_velocity = velocity;
	_volume = velocity >> 1;
	_loop = _instrument->loop;
	_note = note;

	if (!_driver.calcVoiceStep(_id)) {
		_note = -1;
		return;
	}

	const int8 *seg1 = (const int8 *)_instrument->samples;
	const int8 *seg2 = seg1;
	int16 seg1Size = _instrument->seg1Size; 
	seg2 += _instrument->seg2Offset & 0xfffe;
	int16 seg2Size = _instrument->seg2Size;

	if (!_loop) {
		seg1Size = seg1Size + seg2Size + _instrument->seg3Size;
		seg2 = nullptr;
		seg2Size = 0;
	}

	_envState = 0;
	if (_instrument->envelope[0].skip != 0 && _loop) {
		_envCurVel = _volume;
		_envCntDown = 0;
		_envState = 1;
	}

	_driver.setChannelData(_id, seg1, seg2, seg1Size * 2, seg2Size * 2);
	if (_driver._playSwitch)
		_driver.setChannelVolume(_id, _driver._masterVolume * _volume >> 6);
}

void MidiDriver_AmigaSci0::Voice::noteOff(int8 note) {
	if (_note == note) {
		if (_envState != 0)
			_envState = 4;
	}
}

void MidiDriver_AmigaSci0::pitchWheel(int8 voice, int16 pitch) {
	_voices[voice]->_pitch = pitch;

	if (_voices[voice]->_note != -1)
		calcVoiceStep(voice);
}

void MidiDriver_AmigaSci0::send(uint32 b) {
	byte command = b & 0xf0;
	byte channel = b & 0xf;
	byte op1 = (b >> 8) & 0xff;
	byte op2 = (b >> 16) & 0xff;

	Voice *voice = _channels[channel];

	if (!voice)
		return;

	switch(command) {
	case 0x80:
		voice->noteOff(op1);
		break;
	case 0x90:
		voice->noteOn(op1, op2);
		break;
	case 0xb0:
		// Not in original driver
		if (op1 == 0x7b)
			stopVoice(voice->_id);
		break;
	case 0xc0:
		voice->_patch = op1;
		break;
	case 0xe0:
		if (!_isEarlyDriver)
			pitchWheel(voice->_id, (op2 << 7) | op1);
		break;
	}
}

void MidiDriver_AmigaSci0::setVolume(byte volume) {
	if (volume > 15)
		volume = 15;
	_masterVolume = volume << 2;
}

bool MidiDriver_AmigaSci0::loadInstruments(Common::SeekableReadStream &patch) {
	char name[31];

	if (patch.read(name, 8) < 8 || strncmp(name, "X0iUo123", 8) != 0) {
		warning("MidiDriver_AmigaSci0: Incorrect ID string in patch bank");
		return false;
	}

	if (patch.read(name, 30) < 30) {
		warning("MidiDriver_AmigaSci0: Error reading patch bank");
		return false;
	}
	name[30] = 0;

	debugC(kDebugLevelSound, "Bank: '%s'", name);

	_instruments.resize(128);

	const uint16 instrumentCount = patch.readUint16BE();

	if (instrumentCount == 0) {
		warning("MidiDriver_AmigaSci0: No instruments found in patch bank");
		return false;
	}

	for (uint i = 0; i < instrumentCount; ++i) {
		Instrument *instrument = new Instrument();

		const uint16 patchIdx = patch.readUint16BE();
		_instruments[patchIdx] = instrument;

		if (i == 0)
			_defaultInstrument = patchIdx;

		patch.read(instrument->name, 30);
		instrument->name[30] = 0;
		const uint16 flags = patch.readUint16BE();
		instrument->loop = flags & 1;
		instrument->fixedNote = !(flags & 2);
		instrument->transpose = patch.readByte();
		instrument->seg1Size = patch.readSint16BE();
		instrument->seg2Offset = patch.readUint32BE();
		instrument->seg2Size = patch.readSint16BE();
		instrument->seg3Offset = patch.readUint32BE();
		instrument->seg3Size = patch.readSint16BE();

		for (uint stage = 0; stage < ARRAYSIZE(instrument->envelope); ++stage)
			instrument->envelope[stage].skip = patch.readByte();

		for (uint stage = 0; stage < ARRAYSIZE(instrument->envelope); ++stage)
			instrument->envelope[stage].step = patch.readByte();

		// In the original, it uses the stage 0 step as the stage 3 target,
		// but we (most likely) don't have to replicate this bug.
		for (uint stage = 0; stage < ARRAYSIZE(instrument->envelope); ++stage)
			instrument->envelope[stage].target = patch.readByte();

		int32 sampleSize = instrument->seg1Size + instrument->seg2Size + instrument->seg3Size;
		sampleSize <<= 1;
		byte *samples = new byte[sampleSize];
		patch.read(samples, sampleSize);
		instrument->samples = samples;

		if (patch.eos() || patch.err()) {
			warning("MidiDriver_AmigaSci0: Error reading patch bank");
			return false;
		}

		debugC(kDebugLevelSound, "\tInstrument[%d]: '%s'", patchIdx, instrument->name);
		debugC(kDebugLevelSound, "\t\tSegment 1: offset 0, size %d", instrument->seg1Size * 2);
		debugC(kDebugLevelSound, "\t\tSegment 2: offset %d, size %d", instrument->seg2Offset, instrument->seg2Size * 2);
		debugC(kDebugLevelSound, "\t\tSegment 3: offset %d, size %d", instrument->seg3Offset, instrument->seg3Size * 2);
		debugC(kDebugLevelSound, "\t\tTranspose = %d, Fixed note = %d, Loop = %d", instrument->transpose, instrument->fixedNote, instrument->loop);
		debugC(kDebugLevelSound, "\t\tEnvelope:");
		for (uint stage = 0; stage < ARRAYSIZE(instrument->envelope); ++stage)
			debugC(kDebugLevelSound, "\t\t\tStage %d: skip %d, step %d, target %d", stage, instrument->envelope[stage].skip, instrument->envelope[stage].step, instrument->envelope[stage].target);
	}

	return true;
}

void MidiDriver_AmigaSci0::freeInstruments() {
	for (Common::Array<const Instrument *>::iterator it = _instruments.begin(); it != _instruments.end(); ++it)
		delete *it;

	_instruments.clear();
}

class MidiPlayer_AmigaSci0 : public MidiPlayer {
public:
	MidiPlayer_AmigaSci0(SciVersion version) : MidiPlayer(version) { _driver = new MidiDriver_AmigaSci0(g_system->getMixer()); }
	~MidiPlayer_AmigaSci0() {
		delete _driver; 
	}

	byte getPlayId() const { return 0x40; }
	int getPolyphony() const { return MidiDriver_AmigaSci0::NUM_VOICES; }
	bool hasRhythmChannel() const { return false; }
	void setVolume(byte volume) { static_cast<MidiDriver_AmigaSci0 *>(_driver)->setVolume(volume); }
	void playSwitch(bool play) { static_cast<MidiDriver_AmigaSci0 *>(_driver)->playSwitch(play); }
	void initTrack(SciSpan<const byte> &trackData) { static_cast<MidiDriver_AmigaSci0 *>(_driver)->initTrack(trackData); }
};

MidiPlayer *MidiPlayer_AmigaSci0_create(SciVersion version) {
	return new MidiPlayer_AmigaSci0(version);
}

} // End of namespace Sci
