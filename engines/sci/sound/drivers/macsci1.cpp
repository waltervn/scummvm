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

#include "common/debug-channels.h"
#include "common/file.h"
#include "common/memstream.h"
#include "common/system.h"
#include "common/textconsole.h"
#include "common/util.h"

namespace Sci {

// Unsigned version of frac_t
typedef uint32 ufrac_t;
static inline ufrac_t uintToFrac(uint16 value) { return value << 16; }
static inline uint16 fracToUint(ufrac_t value) { return value >> 16; }

static const byte envSpeedToStep[32] = {
	0x40, 0x32, 0x24, 0x18, 0x14, 0x0f, 0x0d, 0x0b, 0x09, 0x08, 0x07, 0x06, 0x05, 0x0a, 0x04, 0x03,
	0x05, 0x02, 0x03, 0x0b, 0x05, 0x09, 0x09, 0x01, 0x02, 0x03, 0x07, 0x05, 0x04, 0x03, 0x03, 0x02
};

static const byte envSpeedToSkip[32] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
	0x01, 0x00, 0x01, 0x07, 0x02, 0x05, 0x07, 0x00, 0x01, 0x02, 0x08, 0x08, 0x08, 0x09, 0x0e, 0x0b
};

static const byte velocityMap[64] = {
	0x01, 0x02, 0x03, 0x03, 0x04, 0x05, 0x05, 0x06, 0x07, 0x08, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
	0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c,
	0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2a,
	0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x34, 0x35, 0x37, 0x39, 0x3a, 0x3c, 0x3e, 0x40
};

class MidiDriver_MacSci1 : public MidiDriver_Emulated {
public:
	enum {
		kVoices = 4,
		kStepTableSize = 56
	};

	enum kEnvState {
		kEnvStateAttack,
		kEnvStateDecay,
		kEnvStateSustain,
		kEnvStateRelease
	};

	MidiDriver_MacSci1(Audio::Mixer *mixer);
	virtual ~MidiDriver_MacSci1();

	// MidiDriver
	int open();
	void close();
	void send(uint32 b);
	MidiChannel *allocateChannel() { return NULL; }
	MidiChannel *getPercussionChannel() { return NULL; }

	// AudioStream
	bool isStereo() const { return false; }
	int getRate() const { return 11127; }

	// MidiDriver_Emulated
	void generateSamples(int16 *buf, int len);
	void onTimer();

	void setVolume(byte volume) { }
	void playSwitch(bool play) { }
	virtual uint32 property(int prop, uint32 param) { return 0; }

private:
	enum {
		kTimerThreshold = 16667
	};

	uint32 _timerIncrease;
	uint32 _timerCounter;

	struct Wave {
		Wave() : name(), phase1Start(0), phase1End(0), phase2Start(0), phase2End(0),
				 nativeNote(0), stepTable(nullptr), samples(nullptr), size(0) { }

		char name[9];
		uint16 phase1Start, phase1End;
		uint16 phase2Start, phase2End;
		uint16 nativeNote;
	
		const ufrac_t *stepTable;
		const byte *samples;
		uint32 size;
	};

	struct NoteRange {
		NoteRange() : startNote(0), endNote(0), wave(nullptr), transpose(0), attackSpeed(0),
					  attackTarget(0), decaySpeed(0), decayTarget(0), releaseSpeed(0), fixedNote(0),
					  loop(false) { }

		int16 startNote;
		int16 endNote;

		const Wave *wave;

		int16 transpose;
	
		byte attackSpeed;
		byte attackTarget;
		byte decaySpeed;
		byte decayTarget;
		byte releaseSpeed;

		int16 fixedNote;
		bool loop;
	};

	struct Instrument {
		Instrument() : name() { }

		char name[9];
		Common::Array<NoteRange> noteRange;
	};

	Common::Array<const Instrument *> _instruments;
	typedef Common::HashMap<uint32, const Wave *> WaveMap;
	WaveMap _waves;
	typedef Common::HashMap<uint32, const ufrac_t *> StepTableMap;
	StepTableMap _stepTables;

	bool _playSwitch;
	uint _masterVolume;

	const Wave *loadWave(Common::SeekableReadStream &stream);
	bool loadInstruments(Common::SeekableReadStream &patch);
	void freeInstruments();
	int8 findVoice(int8 channel);
	void voiceMapping(int8 channel, byte voices);
	void assignVoices(int8 channel, byte voices);
	void releaseVoices(int8 channel, byte voices);
	void donateVoices();
	void noteOn(int8 channel, int8 note, int8 velocity);
	void noteOff(int8 channel, int8 note);
	void changePatch(int8 channel, int8 patch);
	void holdPedal(int8 channel, int8 pedal);
	void setPitchWheel(int8 channel, uint16 pitch);
	void generateSampleChunk(int16 *buf, int len);

	struct Channel;
	class Voice {
	public:
		Voice(MidiDriver_MacSci1 &driver) :
			_channel(-1),
			_note(-1),
			_velocity(0),
			_isReleased(false),
			_isSustained(false),
			_ticks(0),
			_releaseTicks(0),
			_envState(kEnvStateAttack),
			_envCurVel(0),
			_envCntDown(0),
			_noteRange(nullptr),
			_wave(nullptr),
			_stepTable(nullptr),
			_pos(0),
			_step(0),
			_mixVelocity(0),
			_isOn(false),
			_driver(driver) { }

		void noteOn(int8 note, int8 velocity);
		void noteOff();

		ufrac_t calcStep(int8 note, const NoteRange *noteRange, const Wave *wave, const ufrac_t *stepTable);
		bool calcVoiceStep();
		void calcMixVelocity();
		void processEnvelope();

		int8 _channel;
		int8 _note;
		byte _velocity;
		bool _isReleased;
		bool _isSustained;
		uint16 _ticks;
		uint16 _releaseTicks;

		kEnvState _envState;
		int8 _envCurVel;
		byte _envCntDown;

		const NoteRange *_noteRange;
		const Wave *_wave;
		const ufrac_t *_stepTable;
		ufrac_t _pos;
		ufrac_t _step;
		byte _mixVelocity;
		bool _isOn;

	private:
		MidiDriver_MacSci1 &_driver;
	};

	Voice *_voice[kVoices];

	struct Channel {
		int8 patch;
		uint16 pitch;
		bool hold;
		int8 volume;
		int8 lastVoice;
		byte extraVoices;
	} _channel[MIDI_CHANNELS];
};

MidiDriver_MacSci1::MidiDriver_MacSci1(Audio::Mixer *mixer) : MidiDriver_Emulated(mixer),
	_playSwitch(true), _masterVolume(15), _timerCounter(0), _voice() {
	_timerIncrease = getBaseTempo();
}

MidiDriver_MacSci1::~MidiDriver_MacSci1() {
	close();
}

int MidiDriver_MacSci1::open() {
	if (_isOpen)
		return MERR_ALREADY_OPEN;

	for (uint v = 0; v < ARRAYSIZE(_voice); ++v)
		_voice[v] = new Voice(*this);

	for (uint ch = 0; ch < MIDI_CHANNELS; ++ch) {
		_channel[ch].patch = 0;
		_channel[ch].pitch = 0x2000;
		_channel[ch].hold = false;
		_channel[ch].volume = 63;
		_channel[ch].lastVoice = 0;
		_channel[ch].extraVoices = 0;
	}

	const Resource *patch = g_sci->getResMan()->findResource(ResourceId(kResourceTypePatch, 7), false);
	if (!patch) {
		warning("Could not open patch for Mac SCI1 sound driver");
		return MERR_DEVICE_NOT_AVAILABLE;
	}

	Common::MemoryReadStream stream(patch->toStream());
	if (!loadInstruments(stream)) {
		freeInstruments();
		return MERR_DEVICE_NOT_AVAILABLE;
	}

	MidiDriver_Emulated::open();

	_mixer->playStream(Audio::Mixer::kPlainSoundType, &_mixerSoundHandle, this, -1, _mixer->kMaxChannelVolume, 0, DisposeAfterUse::NO);

	return 0;
}

void MidiDriver_MacSci1::close() {
	if (!_isOpen)
		return;

	_mixer->stopHandle(_mixerSoundHandle);

	for (uint v = 0; v < ARRAYSIZE(_voice); ++v)
		delete _voice[v];

	freeInstruments();

	_isOpen = false;
}

void MidiDriver_MacSci1::generateSamples(int16 *data, int len) {
	while (len > 0) {
		int chunkLen = 186;
		if (len < 186)
			chunkLen = len;
		generateSampleChunk(data, chunkLen);
		data += chunkLen;
		len -= chunkLen;
	}
}

const MidiDriver_MacSci1::Wave *MidiDriver_MacSci1::loadWave(Common::SeekableReadStream &stream) {
	Wave *wave = new Wave();

	stream.read(wave->name, 8);
	wave->name[8] = 0;
	stream.skip(2); // Signedness flag, unused
	wave->phase1Start = stream.readUint16BE();
	wave->phase1End = stream.readUint16BE();
	wave->phase2Start = stream.readUint16BE();
	wave->phase2End = stream.readUint16BE();
	wave->nativeNote = stream.readUint16BE();
	const uint32 stepTableOffset = stream.readUint32BE();

	// Sanity checks of segment offsets
	if (wave->phase2End > wave->phase1End || wave->phase1Start > wave->phase1End || wave->phase2Start > wave->phase2End)
		error("MidiDriver_MacSci1: Invalid segment offsets found for wave '%s'", wave->name);

	// 1480 additional samples are present, rounded up to the next word boundary
	// This allows for a maximum step of 8 during sample generation without bounds checking
	wave->size = ((wave->phase1End + 1) + 1480 + 1) & ~1;
	byte *samples = new byte[wave->size];
	stream.read(samples, wave->size);
	wave->samples = samples;

	if (!_stepTables.contains(stepTableOffset)) {
		ufrac_t *stepTable = new ufrac_t[kStepTableSize];

		stream.seek(stepTableOffset);

		for (uint i = 0; i < kStepTableSize; ++i)
			stepTable[i] = stream.readUint32BE();

		// Two more words follow each step table. The first word appears to be
		// the sample rate. The 2nd one is possibly just for alignment.

		_stepTables[stepTableOffset] = stepTable;
	}

	wave->stepTable = _stepTables[stepTableOffset];
	return wave;
}

bool MidiDriver_MacSci1::loadInstruments(Common::SeekableReadStream &patch) {
	_instruments.resize(128);

	for (uint patchIdx = 0; patchIdx < 128; ++patchIdx) {
		patch.seek(patchIdx * 4);
		uint32 offset = patch.readUint32BE();

		if (offset == 0)
			continue;

		Instrument *instrument = new Instrument();

		patch.seek(offset);
		patch.read(instrument->name, 8);
		instrument->name[8] = 0;
		patch.skip(2); // Unknown

		debugC(kDebugLevelSound, "Instrument[%d]: '%s'", patchIdx, instrument->name);

		while (1) {
			NoteRange noteRange;

			noteRange.startNote = patch.readUint16BE();

			if (patch.err() || patch.eos())
				return false;

			if (noteRange.startNote == -1)
				break;

			noteRange.endNote = patch.readUint16BE();

			const uint32 waveOffset = patch.readUint32BE();

			noteRange.transpose = patch.readSint16BE();

			noteRange.attackSpeed = patch.readByte();
			noteRange.attackTarget = patch.readByte();
			noteRange.decaySpeed = patch.readByte();
			noteRange.decayTarget = patch.readByte();
			noteRange.releaseSpeed = patch.readByte();

			patch.skip(1); // Probably releaseTarget, unused
			noteRange.fixedNote = patch.readSint16BE();
			noteRange.loop = !patch.readUint16BE();

			int32 nextNoteRangePos = patch.pos();

			if (!_waves.contains(waveOffset)) {
				patch.seek(waveOffset);
				const Wave *wave = loadWave(patch);

				if (!wave) {
					error("MidiDriver_MacSci1: Failed to read instrument %d", patchIdx);
					delete instrument;
					return false;
				}

				_waves[waveOffset] = wave;
			}

			noteRange.wave = _waves[waveOffset];

			debugC(kDebugLevelSound, "\tNotes %d-%d", noteRange.startNote, noteRange.endNote);
			debugC(kDebugLevelSound, "\t\tWave: '%s'", noteRange.wave->name);
			debugC(kDebugLevelSound, "\t\t\tSegment 1: %d-%d", noteRange.wave->phase1Start, noteRange.wave->phase1End);
			debugC(kDebugLevelSound, "\t\t\tSegment 2: %d-%d", noteRange.wave->phase2Start, noteRange.wave->phase2End);
			debugC(kDebugLevelSound, "\t\tTranspose = %d, Fixed note = %d, Loop = %d", noteRange.transpose, noteRange.fixedNote, noteRange.loop);
			debugC(kDebugLevelSound, "\t\tAttack: %d delta, %d target", noteRange.attackSpeed, noteRange.attackTarget);
			debugC(kDebugLevelSound, "\t\tDecay: %d delta, %d target", noteRange.decaySpeed, noteRange.decayTarget);
			debugC(kDebugLevelSound, "\t\tRelease: %d delta, %d target", noteRange.releaseSpeed, 0);
			debugC(kDebugLevelSound, "\t\tRelease: %d delta, %d target", noteRange.releaseSpeed, 0);

			instrument->noteRange.push_back(noteRange);

			_instruments[patchIdx] = instrument;
			patch.seek(nextNoteRangePos);
		}
	}

	return true;
}

void MidiDriver_MacSci1::freeInstruments() {
	for (WaveMap::iterator it = _waves.begin(); it != _waves.end(); ++it)
		delete it->_value;
	_waves.clear();

	for (StepTableMap::iterator it = _stepTables.begin(); it != _stepTables.end(); ++it)
		delete it->_value;
	_stepTables.clear();

	for (Common::Array<const Instrument *>::iterator it = _instruments.begin(); it != _instruments.end(); ++it)
		delete it;
	_instruments.clear();
}

void MidiDriver_MacSci1::onTimer() {
	// This callback is 250Hz and we need 60Hz for doEnvelope()
	_timerCounter += _timerIncrease;

	if (_timerCounter <= kTimerThreshold)
		return;

	_timerCounter -= kTimerThreshold;

	for (uint v = 0; v < kVoices; ++v) {
		if (_voice[v]->_note != -1) {
			++_voice[v]->_ticks;
			if (_voice[v]->_isReleased)
				++_voice[v]->_releaseTicks;
			_voice[v]->processEnvelope();
			_voice[v]->calcMixVelocity();
		}
	}
}

int8 MidiDriver_MacSci1::findVoice(int8 channel) {
	int8 voice = _channel[channel].lastVoice;
	uint16 maxTicks = 0;
	int8 maxTicksVoice = -1;

	do {
		voice = (voice + 1) % kVoices;

		if (_voice[voice]->_channel == channel) {
			if (_voice[voice]->_note == -1) {
				_channel[channel].lastVoice = voice;
				return voice;
			}
			uint16 ticks;
			if (_voice[voice]->_releaseTicks != 0)
				ticks = _voice[voice]->_releaseTicks + 0x8000;
			else
				ticks = _voice[voice]->_ticks;

			if (ticks >= maxTicks) {
				maxTicks = ticks;
				maxTicksVoice = voice;
			}
		}
	} while (voice != _channel[channel].lastVoice);

	if (maxTicksVoice != -1) {
		_voice[maxTicksVoice]->noteOff();
		_channel[channel].lastVoice = maxTicksVoice;
		return maxTicksVoice;
	}

	return -1;
}

void MidiDriver_MacSci1::voiceMapping(int8 channel, byte voices) {
	int curVoices = 0;

	for (uint v = 0; v < kVoices; ++v)
		if (_voice[v]->_channel == channel)
			curVoices++;

	curVoices += _channel[channel].extraVoices;

	if (curVoices < voices)
		assignVoices(channel, voices - curVoices);
	else if (curVoices > voices) {
		releaseVoices(channel, curVoices - voices);
		donateVoices();
	}
}

void MidiDriver_MacSci1::assignVoices(int8 channel, byte voices) {
	for (uint v = 0; v < kVoices; ++v)
		if (_voice[v]->_channel == -1) {
			_voice[v]->_channel = channel;

			if (_voice[v]->_note != -1)
				_voice[v]->noteOff();

			if (--voices == 0)
				break;
		}

	_channel[channel].extraVoices += voices;
}

void MidiDriver_MacSci1::releaseVoices(int8 channel, byte voices) {
	if (_channel[channel].extraVoices >= voices) {
		_channel[channel].extraVoices -= voices;
		return;
	}

	voices -= _channel[channel].extraVoices;
	_channel[channel].extraVoices = 0;

	for (uint v = 0; v < kVoices; ++v) {
		if ((_voice[v]->_channel == channel) && (_voice[v]->_note == -1)) {
			_voice[v]->_channel = -1;
			if (--voices == 0)
				return;
		}
	}

	do {
		uint16 maxTicks = 0;
		int8 maxTicksVoice = 0;

		for (uint v = 0; v < kVoices; ++v) {
			if (_voice[v]->_channel == channel) {
				// The original code seems to be broken here. It reads a word value from
				// byte array _voiceSustained.
				uint16 ticks = _voice[v]->_releaseTicks;
				if (ticks > 0)
					ticks += 0x8000;
				else
					ticks = _voice[v]->_ticks;

				if (ticks >= maxTicks) {
					maxTicks = ticks;
					maxTicksVoice = v;
				}
			}
		}
		_voice[maxTicksVoice]->_isSustained = false;
		_voice[maxTicksVoice]->noteOff();
		_voice[maxTicksVoice]->_channel = -1;
	} while (--voices > 0);
}

void MidiDriver_MacSci1::donateVoices() {
	int freeVoices = 0;

	for (uint v = 0; v < kVoices; ++v)
		if (_voice[v]->_channel == -1)
			freeVoices++;

	if (freeVoices == 0)
		return;

	for (int ch = 0; ch < MIDI_CHANNELS; ++ch) {
		if (_channel[ch].extraVoices != 0) {
			if (_channel[ch].extraVoices >= freeVoices) {
				_channel[ch].extraVoices -= freeVoices;
				assignVoices(ch, freeVoices);
				return;
			} else {
				freeVoices -= _channel[ch].extraVoices;
				byte extraVoices = _channel[ch].extraVoices;
				_channel[ch].extraVoices = 0;
				assignVoices(ch, extraVoices);
			}
		}
	}
}

void MidiDriver_MacSci1::Voice::noteOn(int8 note, int8 velocity) {
	_isReleased = false;
	_envCurVel = 0;
	_envState = kEnvStateAttack;
	_envCntDown = 0;
	_ticks = 0;
	_releaseTicks = 0;

	int8 patchId = _driver._channel[_channel].patch;

	// Check for valid patch
	if (patchId < 0 || (uint)patchId >= _driver._instruments.size() || !_driver._instruments[patchId])
		return;

	const Instrument *ins = _driver._instruments[patchId];

	// Each patch links to one or more waves, where each wave is assigned to a range of notes.
	Common::Array<NoteRange>::const_iterator noteRange;
	for (noteRange = ins->noteRange.begin(); noteRange != ins->noteRange.end(); ++noteRange) {
		if (noteRange->startNote <= note && note <= noteRange->endNote)
			break;
	}

	// Abort if this note has no wave assigned to it
	if (noteRange == ins->noteRange.end())
		return;

	const Wave *wave = noteRange->wave;
	const ufrac_t *stepTable = wave->stepTable;

	_noteRange = noteRange;
	_wave = wave;
	_stepTable = stepTable;
	_pos = uintToFrac(wave->phase1Start);

	if (velocity != 0)
		velocity = velocityMap[velocity >> 1];

	_velocity = velocity;
	_note = note;

	if (!calcVoiceStep())
		_note = -1;
	else {
		_mixVelocity = 0;
		_isOn = true;
	}
}

void MidiDriver_MacSci1::Voice::noteOff() {
	_isOn = false;
	_velocity = 0;
	_note = -1;
	_isSustained = false;
	_isReleased = false;
	_envState = kEnvStateAttack;
	_envCntDown = 0;
	_ticks = 0;
	_releaseTicks = 0;
}

ufrac_t MidiDriver_MacSci1::Voice::calcStep(int8 note, const NoteRange *noteRange, const Wave *wave, const ufrac_t *stepTable) {
	uint16 noteAdj = note + 127 - wave->nativeNote;
	byte channel = _channel;
	uint16 pitch = _driver._channel[channel].pitch;
	pitch /= 170;
	noteAdj += (pitch >> 2) - 12;
	byte offset = pitch & 3;
	uint octaveRsh = 0;

	if (noteAdj < 255)
		octaveRsh = 21 - (noteAdj + 9) / 12;

	noteAdj = (noteAdj + 9) % 12;

	uint stepTableIndex = (noteAdj << 2) + offset;
	assert(stepTableIndex + 8 < kStepTableSize);
	ufrac_t step = stepTable[stepTableIndex + 4];

	int16 transpose = noteRange->transpose;
	if (transpose > 0) {
		ufrac_t delta = stepTable[stepTableIndex + 8] - step;
		delta >>= 4;
		delta >>= octaveRsh;
		delta *= transpose;
		step >>= octaveRsh;
		step += delta;
	} else if (transpose < 0) {
		ufrac_t delta = step - stepTable[stepTableIndex];
		delta >>= 4;
		delta >>= octaveRsh;
		delta *= -transpose;
		step >>= octaveRsh;
		step -= delta;
	} else {
		step >>= octaveRsh;
	}

	// This ensures that we won't step outside of the sample data
	if (step > uintToFrac(8))
		return -1;

	return step;
}

bool MidiDriver_MacSci1::Voice::calcVoiceStep() {
	int8 note = _note;
	const NoteRange *noteRange = _noteRange;
	const Wave *wave = _wave;
	const ufrac_t *stepTable = _stepTable;

	int16 fixedNote = noteRange->fixedNote;
	if (fixedNote != -1)
		note = fixedNote;

	ufrac_t step = calcStep(note, noteRange, wave, stepTable);
	if (step == -1)
		return false;

	_step = step;
	return true;
}

void MidiDriver_MacSci1::Voice::processEnvelope() {
	const NoteRange *noteRange = _noteRange;
	byte attackTarget = noteRange->attackTarget;
	byte decayTarget = noteRange->decayTarget;

	if (!noteRange->loop) {
		_envCurVel = attackTarget;
		return;
	}

	if (_isReleased)
		_envState = kEnvStateRelease;

	switch(_envState) {
	case kEnvStateAttack: {
		if (_envCntDown != 0) {
			--_envCntDown;
			return;
		}
		byte attackSpeed = noteRange->attackSpeed;
		_envCntDown = envSpeedToSkip[attackSpeed];
		_envCurVel += envSpeedToStep[attackSpeed];
		if (_envCurVel >= attackTarget) {
			_envCurVel = attackTarget;
			_envState = kEnvStateDecay;
		}
		break;
	}
	case kEnvStateDecay: {
		if (_envCntDown != 0) {
			--_envCntDown;
			return;
		}
		byte decaySpeed = noteRange->decaySpeed;
		_envCntDown = envSpeedToSkip[decaySpeed];
		_envCurVel -= envSpeedToStep[decaySpeed];
		if (_envCurVel <= decayTarget) {
			_envCurVel = decayTarget;
			_envState = kEnvStateSustain;
		}
		break;
	}
	case kEnvStateSustain:
		_envCurVel = decayTarget;
		break;
	case kEnvStateRelease: {
		if (_envCntDown != 0) {
			--_envCntDown;
			return;
		}
		byte releaseSpeed = noteRange->releaseSpeed;
		_envCntDown = envSpeedToSkip[releaseSpeed];
		_envCurVel -= envSpeedToStep[releaseSpeed];
		if (_envCurVel <= 0)
			noteOff();
	}
	}
}

void MidiDriver_MacSci1::Voice::calcMixVelocity() {
	byte chanVol = _driver._channel[_channel].volume;
	byte voiceVelocity = _velocity;

	if (chanVol != 0) {
		if (voiceVelocity != 0) {
			voiceVelocity = voiceVelocity * chanVol / 63;
			if (_envCurVel != 0) {
				voiceVelocity = voiceVelocity * _envCurVel / 63;
				if (_driver._masterVolume != 0) {
					voiceVelocity = voiceVelocity * (_driver._masterVolume << 2) / 63;
					if (voiceVelocity == 0)
						++voiceVelocity;
				} else {
					voiceVelocity = 0;
				}
			} else {
				voiceVelocity = 0;
			}
		}
	} else {
		voiceVelocity = 0;
	}

	if (!_driver._playSwitch)
		voiceVelocity = 0;

	_mixVelocity = voiceVelocity;
}

void MidiDriver_MacSci1::noteOn(int8 channel, int8 note, int8 velocity) {
	if (velocity == 0) {
		noteOff(channel, note);
		return;
	}

	for (uint v = 0; v < kVoices; ++v) {
		if (_voice[v]->_channel == channel && _voice[v]->_note == note) {
			_voice[v]->_isSustained = false;
			_voice[v]->noteOff();
			_voice[v]->noteOn(note, velocity);
			return;
		}
	}

	int8 voice = findVoice(channel);
	if (voice != -1)
		_voice[voice]->noteOn(note, velocity);
}

void MidiDriver_MacSci1::noteOff(int8 channel, int8 note) {
	for (uint v = 0; v < kVoices; ++v) {
		if (_voice[v]->_channel == channel && _voice[v]->_note == note) {
			if (_channel[channel].hold)
				_voice[v]->_isSustained = true;
			else {
				_voice[v]->_isReleased = true;
				_voice[v]->_envCntDown = 0;
			}
			return;
		}
	}
}

void MidiDriver_MacSci1::changePatch(int8 channel, int8 patch) {
	_channel[channel].patch = patch;
}

void MidiDriver_MacSci1::holdPedal(int8 channel, int8 pedal) {
	_channel[channel].hold = pedal;

	if (pedal != 0)
		return;

	for (uint voice = 0; voice < kVoices; ++voice) {
		if (_voice[voice]->_channel == channel && _voice[voice]->_isSustained) {
			_voice[voice]->_isSustained = false;
			_voice[voice]->_isReleased = true;
		}
	}
}

void MidiDriver_MacSci1::setPitchWheel(int8 channel, uint16 pitch) {
	_channel[channel].pitch = pitch;

	for (uint v = 0; v < kVoices; ++v)
		if (_voice[v]->_note != -1 && _voice[v]->_channel == channel)
			_voice[v]->calcVoiceStep();
}

void MidiDriver_MacSci1::send(uint32 b) {
	byte command = b & 0xf0;
	byte channel = b & 0xf;
	byte op1 = (b >> 8) & 0xff;
	byte op2 = (b >> 16) & 0xff;

	switch(command) {
	case 0x80:
		noteOff(channel, op1);
		break;
	case 0x90:
		noteOn(channel, op1, op2);
		break;
	case 0xb0:
		switch (op1) {
		case 0x07:
			if (op2 != 0) {
				op2 >>= 1;
				if (op2 == 0)
					++op2;
			}
			_channel[channel].volume = op2;
			break;
		case 0x40:
			holdPedal(channel, op2);
			break;
		case 0x4b:
			voiceMapping(channel, op2);
			break;
		case 0x7b:
			for (uint voice = 0; voice < kVoices; ++voice) {
				if (_voice[voice]->_channel == channel && _voice[voice]->_note != -1)
					_voice[voice]->noteOff();
			}
		}
		break;
	case 0xc0:
		changePatch(channel, op1);
		break;
	case 0xe0:
		setPitchWheel(channel, (op2 << 7) | op1);
		break;
	}
}

static int euclDivide(int x, int y) {
	// Assumes y > 0
	if (x % y < 0)
		return x / y - 1;
	else
		return x / y;
}

static byte applyVelocity(byte velocity, byte sample) {
	return euclDivide((sample - 0x80) * velocity, 63) + 0x80;
}

void MidiDriver_MacSci1::generateSampleChunk(int16 *data, int len) {
	const byte *samples[kVoices] = { };
	const byte silence = 0x80;

	ufrac_t offset[kVoices];

	assert(len > 0 && len <= 186);

	for (uint v = 0; v < kVoices; ++v) {
		if (_voice[v]->_isOn) {
			samples[v] = _voice[v]->_wave->samples;
			offset[v] = _voice[v]->_pos;

			// Sanity checks
			assert(_voice[v]->_step <= uintToFrac(8));
			const uint16 firstIndex = fracToUint(_voice[v]->_pos);
			const uint16 lastIndex = fracToUint(_voice[v]->_pos + (len - 1) * _voice[v]->_step);
			assert(lastIndex >= firstIndex && lastIndex < _voice[v]->_wave->size);
		} else {
			samples[v] = &silence;
			offset[v] = 0;
			_voice[v]->_step = 0;
		}
	}

	// Mix

	for (uint i = 0; i < len; ++i) {
		uint16 mix = 0;
		for (int v = 0; v < kVoices; ++v) {
			uint16 curOffset = fracToUint(offset[v]);
			byte sample = samples[v][curOffset];
			mix += applyVelocity(_voice[v]->_mixVelocity, sample);
			offset[v] += _voice[v]->_step;
		}

		mix = CLIP<uint16>(mix, 384, 639) - 384;

		// Convert to 16-bit signed
		data[i] = (mix << 8) - 0x8000;
	}

	// Loop

	for (uint v = 0; v < kVoices; ++v) {
		if (_voice[v]->_isOn) {
			uint16 endOffset = _voice[v]->_wave->phase2End;

			if (endOffset == 0)
				endOffset = _voice[v]->_wave->phase1End;

			if (fracToUint(offset[v]) > endOffset) {
				if (_voice[v]->_wave->phase2End != 0 && _voice[v]->_noteRange->loop) {
					uint16 loopSize = endOffset - _voice[v]->_wave->phase2Start + 1;
					do {
						offset[v] -= uintToFrac(loopSize);
					} while (fracToUint(offset[v]) > endOffset);
				} else {
					_voice[v]->noteOff();
				}
			}
		}
	}

	for (uint v = 0; v < kVoices; ++v)
		_voice[v]->_pos = offset[v];

}

class MidiPlayer_MacSci1 : public MidiPlayer {
public:
	MidiPlayer_MacSci1(SciVersion version) : MidiPlayer(version) { _driver = new MidiDriver_MacSci1(g_system->getMixer()); }
	~MidiPlayer_MacSci1() {
		delete _driver;
	}

	byte getPlayId() const;
	int getPolyphony() const { return MidiDriver_MacSci1::kVoices; }
	bool hasRhythmChannel() const { return false; }
	void setVolume(byte volume) { static_cast<MidiDriver_MacSci1 *>(_driver)->setVolume(volume); }
	void playSwitch(bool play) { static_cast<MidiDriver_MacSci1 *>(_driver)->playSwitch(play); }
};

MidiPlayer *MidiPlayer_MacSci1_create(SciVersion version) {
	return new MidiPlayer_MacSci1(version);
}

byte MidiPlayer_MacSci1::getPlayId() const {
	return 0x06;
}

} // End of namespace Sci
