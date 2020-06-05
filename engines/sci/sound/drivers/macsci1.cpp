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

#include "sci/sound/drivers/mididriver.h"
#include "sci/resource.h"

#include "audio/audiostream.h"
#include "audio/mixer.h"
#include "common/debug-channels.h"
#include "common/file.h"
#include "common/memstream.h"
#include "common/system.h"
#include "common/textconsole.h"
#include "common/util.h"

namespace Sci {

// Unsigned version of frac_t
typedef uint32 ufrac_t;
static inline ufrac_t uintToUfrac(uint16 value) { return value << 16; }
static inline uint16 ufracToUint(ufrac_t value) { return value >> 16; }

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

class MidiDriver_MacSci1 : public MidiDriver, Audio::AudioStream {
public:
	enum {
		kVoices = 4,
		kStepTableSize = 56,
		kSampleChunkSize = 186,
		kBaseFreq = 60
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
	bool isOpen() const { return _isOpen; }
	void close();
	void send(uint32 b);
	void setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc);
	uint32 getBaseTempo() { return (1000000 + kBaseFreq / 2) / kBaseFreq; }

	MidiChannel *allocateChannel() { return NULL; }
	MidiChannel *getPercussionChannel() { return NULL; }

	// AudioStream
	bool isStereo() const { return false; }
	int getRate() const { return 11127; }
	int readBuffer(int16 *data, const int numSamples);
	bool endOfData() const { return false; }

	void setVolume(byte volume) { }
	void playSwitch(bool play) { }
	virtual uint32 property(int prop, uint32 param) { return 0; }
	void onTimer();

private:
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

	Audio::Mixer *_mixer;
	Audio::SoundHandle _mixerSoundHandle;
	Common::TimerManager::TimerProc _timerProc;
	void *_timerParam;
	ufrac_t _nextTick;
	ufrac_t _samplesPerTick;
	bool _isOpen;

	const Wave *loadWave(Common::SeekableReadStream &stream);
	bool loadInstruments(Common::SeekableReadStream &patch);
	void freeInstruments();
	void donateVoices();
	void generateSamples(int16 *buf, int len);

	class Channel;
	class Voice {
	public:
		Voice(MidiDriver_MacSci1 &driver) :
			_channel(nullptr),
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

		Channel *_channel;
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

	Common::Array<Voice *> _voices;
	typedef Common::Array<Voice *>::const_iterator VoiceIt;

	class Channel {
	public:
		Channel(MidiDriver_MacSci1 &driver) :
			_patch(0),
			_pitch(0x2000),
			_hold(false),
			_volume(63),
			_lastVoiceIt(driver._voices.begin()),
			_extraVoices(0),
			_driver(driver) { }

		void noteOn(int8 note, int8 velocity);
		void noteOff(int8 note);

		Voice *findVoice();
		void voiceMapping(byte voices);
		void assignVoices(byte voices);
		void releaseVoices(byte voices);
		void changePatch(int8 patch);
		void holdPedal(int8 pedal);
		void setPitchWheel(uint16 pitch);

		int8 _patch;
		uint16 _pitch;
		bool _hold;
		int8 _volume;
		VoiceIt _lastVoiceIt;
		byte _extraVoices;

	private:
		MidiDriver_MacSci1 &_driver;
	};

	Common::Array<Channel *> _channels;
	typedef Common::Array<Channel *>::const_iterator ChanIt;
};

MidiDriver_MacSci1::MidiDriver_MacSci1(Audio::Mixer *mixer) :
	_playSwitch(true),
	_masterVolume(15),
	_mixer(mixer),
	_mixerSoundHandle(),
	_timerProc(),
	_timerParam(nullptr),
	_nextTick(0),
	_samplesPerTick(0),
	_isOpen(false) { }

MidiDriver_MacSci1::~MidiDriver_MacSci1() {
	close();
}

int MidiDriver_MacSci1::open() {
	if (_isOpen)
		return MERR_ALREADY_OPEN;

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

	_voices.resize(kVoices);
	for (Common::Array<Voice *>::iterator v = _voices.begin(); v != _voices.end(); ++v)
		*v = new Voice(*this);

	_channels.resize(MIDI_CHANNELS);
	for (Common::Array<Channel *>::iterator ch = _channels.begin(); ch != _channels.end(); ++ch)
		*ch = new Channel(*this);

	_samplesPerTick = uintToUfrac(getRate() / kBaseFreq) + uintToUfrac(getRate() % kBaseFreq) / kBaseFreq;

	_mixer->playStream(Audio::Mixer::kPlainSoundType, &_mixerSoundHandle, this, -1, _mixer->kMaxChannelVolume, 0, DisposeAfterUse::NO);

	_isOpen = true;

	return 0;
}

void MidiDriver_MacSci1::close() {
	if (!_isOpen)
		return;

	_mixer->stopHandle(_mixerSoundHandle);

	for (ChanIt c = _channels.begin(); c != _channels.end(); ++c)
		delete *c;
	_channels.clear();

	for (VoiceIt v = _voices.begin(); v != _voices.end(); ++v)
		delete *v;
	_voices.clear();

	freeInstruments();

	_isOpen = false;
}

void MidiDriver_MacSci1::setTimerCallback(void *timer_param, Common::TimerManager::TimerProc timer_proc) {
	_timerProc = timer_proc;
	_timerParam = timer_param;
}

int MidiDriver_MacSci1::readBuffer(int16 *data, const int numSamples) {
	const int stereoFactor = isStereo() ? 2 : 1;
	int len = numSamples / stereoFactor;
	int step;

	do {
		step = len;
		if (step > ufracToUint(_nextTick))
			step = ufracToUint(_nextTick);

		if (step > kSampleChunkSize)
			step = kSampleChunkSize;

		if (step > 0)
			generateSamples(data, step);

		_nextTick -= uintToUfrac(step);
		if (ufracToUint(_nextTick) == 0) {
			if (_timerProc)
				(*_timerProc)(_timerParam);

			onTimer();

			_nextTick += _samplesPerTick;
		}

		data += step * stereoFactor;
		len -= step;
	} while (len);

	return numSamples;
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
	for (VoiceIt it = _voices.begin(); it != _voices.end(); ++it) {
		Voice *v = *it;
		if (v->_note != -1) {
			++v->_ticks;
			if (v->_isReleased)
				++v->_releaseTicks;
			v->processEnvelope();
			v->calcMixVelocity();
		}
	}
}

MidiDriver_MacSci1::Voice *MidiDriver_MacSci1::Channel::findVoice() {
	assert(_lastVoiceIt != _driver._voices.end());

	VoiceIt voiceIt = _lastVoiceIt;
	uint16 maxTicks = 0;
	VoiceIt maxTicksVoiceIt = _driver._voices.end();

	do {
		++voiceIt;

		if (voiceIt == _driver._voices.end())
			voiceIt = _driver._voices.begin();

		Voice *v = *voiceIt;

		if (v->_channel == this) {
			if (v->_note == -1) {
				_lastVoiceIt = voiceIt;
				return v;
			}

			uint16 ticks;

			if (v->_releaseTicks != 0)
				ticks = v->_releaseTicks + 0x8000;
			else
				ticks = v->_ticks;

			if (ticks >= maxTicks) {
				maxTicks = ticks;
				maxTicksVoiceIt = voiceIt;
			}
		}
	} while (voiceIt != _lastVoiceIt);

	if (maxTicksVoiceIt != _driver._voices.end()) {
		(*maxTicksVoiceIt)->noteOff();
		_lastVoiceIt = maxTicksVoiceIt;
		return *maxTicksVoiceIt;
	}

	return nullptr;
}

void MidiDriver_MacSci1::Channel::voiceMapping(byte voices) {
	int curVoices = 0;

	for (VoiceIt it = _driver._voices.begin(); it != _driver._voices.end(); ++it)
		if ((*it)->_channel == this)
			curVoices++;

	curVoices += _extraVoices;

	if (curVoices < voices)
		assignVoices(voices - curVoices);
	else if (curVoices > voices) {
		releaseVoices(curVoices - voices);
		_driver.donateVoices();
	}
}

void MidiDriver_MacSci1::Channel::assignVoices(byte voices) {
	for (VoiceIt it = _driver._voices.begin(); it != _driver._voices.end(); ++it) {
		Voice *v = *it;

		if (!v->_channel) {
			v->_channel = this;

			if (v->_note != -1)
				v->noteOff();

			if (--voices == 0)
				break;
		}
	}

	_extraVoices += voices;
}

void MidiDriver_MacSci1::Channel::releaseVoices(byte voices) {
	if (_extraVoices >= voices) {
		_extraVoices -= voices;
		return;
	}

	voices -= _extraVoices;
	_extraVoices = 0;

	for (VoiceIt it = _driver._voices.begin(); it != _driver._voices.end(); ++it) {
		Voice *v = *it;

		if ((v->_channel == this) && (v->_note == -1)) {
			v->_channel = nullptr;
			if (--voices == 0)
				return;
		}
	}

	do {
		uint16 maxTicks = 0;
		Voice *maxTicksVoice;

		for (VoiceIt it = _driver._voices.begin(); it != _driver._voices.end(); ++it) {
			Voice *v = *it;

			if (v->_channel == this) {
				// The original code seems to be broken here. It reads a word value from
				// byte array _voiceSustained.
				uint16 ticks = v->_releaseTicks;
				if (ticks > 0)
					ticks += 0x8000;
				else
					ticks = v->_ticks;

				if (ticks >= maxTicks) {
					maxTicks = ticks;
					maxTicksVoice = v;
				}
			}
		}
		maxTicksVoice->_isSustained = false;
		maxTicksVoice->noteOff();
		maxTicksVoice->_channel = nullptr;
	} while (--voices > 0);
}

void MidiDriver_MacSci1::donateVoices() {
	int freeVoices = 0;

	for (VoiceIt it = _voices.begin(); it != _voices.end(); ++it)
		if (!(*it)->_channel)
			freeVoices++;

	if (freeVoices == 0)
		return;

	for (ChanIt it = _channels.begin(); it != _channels.end(); ++it) {
		Channel *channel = *it;

		if (channel->_extraVoices != 0) {
			if (channel->_extraVoices >= freeVoices) {
				channel->_extraVoices -= freeVoices;
				channel->assignVoices(freeVoices);
				return;
			} else {
				freeVoices -= channel->_extraVoices;
				byte extraVoices = channel->_extraVoices;
				channel->_extraVoices = 0;
				channel->assignVoices(extraVoices);
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

	int8 patchId = _channel->_patch;

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
	_pos = uintToUfrac(wave->phase1Start);

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
	uint16 pitch = _channel->_pitch;
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
	if (step > uintToUfrac(8))
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
	byte chanVol = _channel->_volume;
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

void MidiDriver_MacSci1::Channel::noteOn(int8 note, int8 velocity) {
	if (velocity == 0) {
		noteOff(note);
		return;
	}

	for (VoiceIt it = _driver._voices.begin(); it != _driver._voices.end(); ++it) {
		Voice *v = *it;

		if (v->_channel == this && v->_note == note) {
			v->_isSustained = false;
			v->noteOff();
			v->noteOn(note, velocity);
			return;
		}
	}

	Voice *v = findVoice();
	if (v)
		v->noteOn(note, velocity);
}

void MidiDriver_MacSci1::Channel::noteOff(int8 note) {
	for (VoiceIt it = _driver._voices.begin(); it != _driver._voices.end(); ++it) {
		Voice *v = *it;

		if (v->_channel == this && v->_note == note) {
			if (_hold)
				v->_isSustained = true;
			else {
				v->_isReleased = true;
				v->_envCntDown = 0;
			}
			return;
		}
	}
}

void MidiDriver_MacSci1::Channel::changePatch(int8 patch) {
	_patch = patch;
}

void MidiDriver_MacSci1::Channel::holdPedal(int8 pedal) {
	_hold = pedal;

	if (pedal != 0)
		return;

	for (VoiceIt it = _driver._voices.begin(); it != _driver._voices.end(); ++it) {
		Voice *v = *it;

		if (v->_channel == this && v->_isSustained) {
			v->_isSustained = false;
			v->_isReleased = true;
		}
	}
}

void MidiDriver_MacSci1::Channel::setPitchWheel(uint16 pitch) {
	_pitch = pitch;

	for (VoiceIt it = _driver._voices.begin(); it != _driver._voices.end(); ++it) {
		Voice *v = *it;

		if (v->_note != -1 && v->_channel == this)
			v->calcVoiceStep();
	}
}

void MidiDriver_MacSci1::send(uint32 b) {
	byte command = b & 0xf0;
	Channel *channel = _channels[b & 0xf];
	byte op1 = (b >> 8) & 0xff;
	byte op2 = (b >> 16) & 0xff;

	switch(command) {
	case 0x80:
		channel->noteOff(op1);
		break;
	case 0x90:
		channel->noteOn(op1, op2);
		break;
	case 0xb0:
		switch (op1) {
		case 0x07:
			if (op2 != 0) {
				op2 >>= 1;
				if (op2 == 0)
					++op2;
			}
			channel->_volume = op2;
			break;
		case 0x40:
			channel->holdPedal(op2);
			break;
		case 0x4b:
			channel->voiceMapping(op2);
			break;
		case 0x7b:
			for (VoiceIt it = _voices.begin(); it != _voices.end(); ++it) {
				Voice *v = *it;

				if (v->_channel == channel && v->_note != -1)
					v->noteOff();
			}
		}
		break;
	case 0xc0:
		channel->changePatch(op1);
		break;
	case 0xe0:
		channel->setPitchWheel((op2 << 7) | op1);
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

void MidiDriver_MacSci1::generateSamples(int16 *data, int len) {
	const byte *samples[kVoices] = { };
	const byte silence = 0x80;

	ufrac_t offset[kVoices];

	assert(len > 0 && len <= kSampleChunkSize);

	for (uint vi = 0; vi < kVoices; ++vi) {
		Voice *v = _voices[vi];

		if (v->_isOn) {
			samples[vi] = v->_wave->samples;
			offset[vi] = v->_pos;

			// Sanity checks
			assert(v->_step <= uintToUfrac(8));
			const uint16 firstIndex = ufracToUint(v->_pos);
			const uint16 lastIndex = ufracToUint(v->_pos + (len - 1) * v->_step);
			assert(lastIndex >= firstIndex && lastIndex < v->_wave->size);
		} else {
			samples[vi] = &silence;
			offset[vi] = 0;
			v->_step = 0;
		}
	}

	// Mix

	for (uint i = 0; i < len; ++i) {
		uint16 mix = 0;
		for (int vi = 0; vi < kVoices; ++vi) {
			uint16 curOffset = ufracToUint(offset[vi]);
			byte sample = samples[vi][curOffset];
			mix += applyVelocity(_voices[vi]->_mixVelocity, sample);
			offset[vi] += _voices[vi]->_step;
		}

		mix = CLIP<uint16>(mix, 384, 639) - 384;

		// Convert to 16-bit signed
		data[i] = (mix << 8) - 0x8000;
	}

	// Loop

	for (uint vi = 0; vi < kVoices; ++vi) {
		Voice *v = _voices[vi];

		if (v->_isOn) {
			uint16 endOffset = v->_wave->phase2End;

			if (endOffset == 0)
				endOffset = v->_wave->phase1End;

			if (ufracToUint(offset[vi]) > endOffset) {
				if (v->_wave->phase2End != 0 && v->_noteRange->loop) {
					uint16 loopSize = endOffset - v->_wave->phase2Start + 1;
					do {
						offset[vi] -= uintToUfrac(loopSize);
					} while (ufracToUint(offset[vi]) > endOffset);
				} else {
					v->noteOff();
				}
			}
		}
	}

	for (uint vi = 0; vi < kVoices; ++vi)
		_voices[vi]->_pos = offset[vi];

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
