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
	0x2184, 0x2109, 0x2090, 0x2019, 0x1fa3, 0x1f2f, 0x1ebc, 0x1e4b
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
		void setPitchWheel(int16 pitch);

		void stop();
		bool calcVoiceStep();

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
	if (_envState == 0 || _envState == 3)
		return;

	if (_envState == 6) {
		stop();
		_envState = 0;
		return;
	}

	if (_envCntDown == 0) {
		const uint envIdx = (_envState > 3 ? _envState - 2 : _envState - 1);

		_envCntDown = _instrument->envelope[envIdx].skip;
		int8 velocity = _envCurVel;

		if (velocity <= 0) {
			stop();
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

	for (VoiceIt it = _voices.begin(); it != _voices.end(); ++it) {
		(*it)->_note = -1;
		(*it)->_pitch = 0x2000;
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

void MidiDriver_AmigaSci0::Voice::stop() {
	_driver.clearVoice(_id);
}

bool MidiDriver_AmigaSci0::Voice::calcVoiceStep() {
	int8 note = _note;

	if (_instrument->fixedNote)
		note = 101;

	int16 index = (note + _instrument->transpose) * 4;

	if (_pitch >= 0x2000)
		index += (_pitch - 0x2000) / 171;
	else
		index -= (0x2000 - _pitch) / 171;

	// For very high notes, the original driver reads out of bounds
	// (see e.g. SQ3 intro sequence). We compute the period for
	// these notes. The original hardware would not be able to
	// handle these very low periods, but Audio::Paula doesn't
	// seem to mind.

	while (index < 96)
		index += 48;

	index -= 96;

	uint16 period = periodTable[index % 48];
	period >>= index / 48;

	if (period == 0)
		return false;

	_driver.setChannelPeriod(_id, period);
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

	if (!calcVoiceStep()) {
		_note = -1;
		// Not in the original
		stop();
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
		if (_envState != 0) {
			_envCurVel = _instrument->envelope[1].target;
			_envState = 4;
		}
	}
}

void MidiDriver_AmigaSci0::Voice::setPitchWheel(int16 pitch) {
	_pitch = pitch;

	if (_note != -1)
		calcVoiceStep();
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
			voice->stop();
		break;
	case 0xc0:
		voice->_patch = op1;
		break;
	case 0xe0:
		if (!_isEarlyDriver)
			voice->setPitchWheel((op2 << 7) | op1);
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
