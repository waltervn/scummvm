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

static const frac_t stepTable[132] = {
	0x00002000, 0x000021e7, 0x000023eb, 0x0000260e,
	0x00002851, 0x00002ab7, 0x00002d41, 0x00002ff2,
	0x000032cc, 0x000035d1, 0x00003904, 0x00003c68,
	0x00002000, 0x000021e7, 0x000023eb, 0x0000260e,
	0x00002851, 0x00002ab7, 0x00002d41, 0x00002ff2,
	0x000032cc, 0x000035d1, 0x00003904, 0x00003c68,
	0x00002000, 0x000021e7, 0x000023eb, 0x0000260e,
	0x00002851, 0x00002ab7, 0x00002d41, 0x00002ff2,
	0x000032cc, 0x000035d1, 0x00003904, 0x00003c68,
	0x00004000, 0x000043ce, 0x000047d6, 0x00004c1c,
	0x000050a3, 0x0000556e, 0x00005a82, 0x00005fe4,
	0x00006598, 0x00006ba2, 0x00007209, 0x000078d1,
	0x00008000, 0x0000879c, 0x00008fad, 0x00009838,
	0x0000a145, 0x0000aadc, 0x0000b505, 0x0000bfc9,
	0x0000cb30, 0x0000d745, 0x0000e412, 0x0000f1a2,
	0x00010000, 0x00010f39, 0x00011f5a, 0x00013070,
	0x0001428a, 0x000155b8, 0x00016a0a, 0x00017f91,
	0x00019660, 0x0001ae8a, 0x0001c824, 0x0001e343,
	0x00020000, 0x00021e72, 0x00023eb3, 0x000260e0,
	0x00028514, 0x0002ab70, 0x0002d414, 0x0002ff22,
	0x00032cc0, 0x00035d14, 0x00039048, 0x0003c687,
	0x00040000, 0x00043ce4, 0x00047d67, 0x0004c1c0,
	0x00050a29, 0x000556e0, 0x0005a828, 0x0005fe44,
	0x00065980, 0x0006ba28, 0x00072090, 0x00078d0e,
	0x00080000, 0x000879c8, 0x0008facd, 0x0009837f,
	0x000a1451, 0x000aadc1, 0x000b504f, 0x000bfc88,
	0x000cb2ff, 0x000d7450, 0x000e411f, 0x000f1a1c,
	0x00080000, 0x000879c8, 0x0008facd, 0x0009837f,
	0x000a1451, 0x000aadc1, 0x000b504f, 0x000bfc88,
	0x000cb2ff, 0x000d7450, 0x000e411f, 0x000f1a1c,
	0x00080000, 0x000879c8, 0x0008facd, 0x0009837f,
	0x000a1451, 0x000aadc1, 0x000b504f, 0x000bfc88,
	0x000cb2ff, 0x000d7450, 0x000e411f, 0x000f1a1c
};

static const byte silence[42] = {
	0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
	0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80
};

class MidiDriver_MacSci0 : public MidiDriver_Emulated {
public:
	enum {
		kVoices = 4
	};

	enum kEnvState {
		kEnvStateAttack,
		kEnvStateDecay,
		kEnvStateSustain,
		kEnvStateRelease
	};

	MidiDriver_MacSci0(Audio::Mixer *mixer);
	virtual ~MidiDriver_MacSci0() { }

	// MidiDriver
	int open();
	void close();
	void initTrack(SciSpan<const byte> &);
	void send(uint32 b);
	MidiChannel *allocateChannel() { return NULL; }
	MidiChannel *getPercussionChannel() { return NULL; }

	// AudioStream
	bool isStereo() const { return false; }
	int getRate() const { return 11127; }

	// MidiDriver_Emulated
	void generateSamples(int16 *buf, int len);
	void onTimer();

	void setVolume(byte volume);
	void playSwitch(bool play) { _playSwitch = play; }

private:
	struct Instrument {
		Instrument();
		~Instrument();

		uint16 index;
		uint16 mode;
		uint32 segSize1;
		uint32 segSize2;
		uint32 segSize3;
		uint16 transpose;
		byte attackLength;
		byte decayLength;
		byte sustainLength;
		byte releaseLength;
		int8 attackDelta;
		int8 decayDelta;
		int8 sustainDelta;
		int8 releaseDelta;
		int8 attackTarget;
		int8 decayTarget;
		int8 sustainTarget;
		int8 releaseTarget; // ???
		char name[31];
		byte *samples;
	};

	Common::Array<Instrument *> _instruments;

	uint32 _timerThreshold;
	uint32 _timerIncrease;
	uint32 _timerCounter;

	void noteOn(int8 voice, int8 note, int8 velocity);
	void noteOff(int8 voice, int8 note);
	void generateSampleChunk(int16 *buf, int len);
	void setMixVelocity(int8 voice, byte velocity);
	void doLoop();
	void doEnvelope();
	void voiceOff(int8 voice);

	int loadInstruments(Common::SeekableReadStream &patch);

	bool _playSwitch;
	byte _masterVolume;

	struct Voice {
		Voice();

		const Instrument *instrument;
		byte envState;
		byte velocity;
		byte envCntDown;
		byte envLength[4];
		int8 envVelocity[5];
		int8 envDelta[4];
		int8 note;
		frac_t offset;
		uint32 segSize1;
		uint32 segSize2;
		uint32 segSize3;
		const byte *samples;
		frac_t step;
		bool loopingDisabled;
		byte mixVelocity;
	} _voice[kVoices];

	int8 _chanVoice[MIDI_CHANNELS];
	Resource *_patch;
};

#define MODE_LOOPING (1 << 0)
#define MODE_PITCH_CHANGES (1 << 1)

MidiDriver_MacSci0::MidiDriver_MacSci0(Audio::Mixer *mixer) : MidiDriver_Emulated(mixer),
	_playSwitch(true), _masterVolume(15), _patch(0), _timerCounter(0), _timerThreshold(16667) {

	_timerIncrease = getBaseTempo();
	memset(_chanVoice, -1, sizeof(_chanVoice));

	for (uint i = 0; i < kVoices; ++i) {
		_voice[i].samples = silence;
		_voice[i].mixVelocity = 0;
		_voice[i].segSize1 = 1;
		_voice[i].segSize2 = 2;
		_voice[i].segSize3 = 1;
	}
}

int MidiDriver_MacSci0::open() {
	Resource *patch = g_sci->getResMan()->findResource(ResourceId(kResourceTypePatch, 200), false);
	if (!patch) {
		warning("Could not open patch for Mac SCI0 sound driver");
		return Common::kUnknownError;
	}

	Common::MemoryReadStream stream(patch->toStream());
	int errorCode = loadInstruments(stream);
	if (errorCode != Common::kNoError)
		return errorCode;

	MidiDriver_Emulated::open();

	_mixer->playStream(Audio::Mixer::kPlainSoundType, &_mixerSoundHandle, this, -1, _mixer->kMaxChannelVolume, 0, DisposeAfterUse::NO);

	return Common::kNoError;
}

void MidiDriver_MacSci0::close() {
	_mixer->stopHandle(_mixerSoundHandle);

	for (uint32 i = 0; i < _instruments.size(); i++)
		delete _instruments[i];
}

void MidiDriver_MacSci0::generateSamples(int16 *data, int len) {
	while (len > 0) {
		int chunkLen = 148;
		if (len < chunkLen)
			chunkLen = len;
		generateSampleChunk(data, chunkLen);
		data += chunkLen;
		len -= chunkLen;
	}
}

void MidiDriver_MacSci0::initTrack(SciSpan<const byte>& header) {
	if (!_isOpen)
		return;

	uint8 readPos = 0;
	const uint8 caps = header.getInt8At(readPos++);

	// We only implement the MIDI functionality here, samples are
	// handled by the generic sample code
	if (caps != 0)
		return;

	uint voices = 0;

	for (uint i = 0; i < 15; ++i) {
		readPos++;
		const uint8 flags = header.getInt8At(readPos++);

		if ((flags & 0x40) && (voices < kVoices))
			_chanVoice[i] = voices++;
		else
			_chanVoice[i] = -1;
	}

	_chanVoice[15] = -1;

	for (uint i = 0; i < kVoices; ++i)
		_voice[i].note = -1;
}

void MidiDriver_MacSci0::onTimer() {
	// This callback is 250Hz and we need 60Hz for doEnvelope()
	_timerCounter += _timerIncrease;

	if (_timerCounter > _timerThreshold) {
		_timerCounter -= _timerThreshold;
		doEnvelope();
	}
}

void MidiDriver_MacSci0::voiceOff(int8 voice) {
	_voice[voice].loopingDisabled = false;
	_voice[voice].segSize1 = 1;
	_voice[voice].segSize2 = 2;
	_voice[voice].samples = silence;
	_voice[voice].offset = 0;
	_voice[voice].step = 0;
	_voice[voice].envState = 0;
}

MidiDriver_MacSci0::Voice::Voice() :
	instrument(0),
	envState(0),
	velocity(0),
	envCntDown(0),
	note(-1),
	offset(0),
	segSize1(1),
	segSize2(2),
	segSize3(1),
	samples(0),
	step(0),
	loopingDisabled(false),
	mixVelocity(0) {

	for (int i = 0; i < 4; i++)
		envLength[i] = 0;
	for (int i = 0; i < 5; i++)
		envVelocity[i] = -1;
	for (int i = 0; i < 4; i++)
		envDelta[i] = -1;
}

int MidiDriver_MacSci0::loadInstruments(Common::SeekableReadStream &patch) {
	// Check the header bytes
	byte header[8];
	patch.read(header, 8);
	if (memcmp(header, "X1iUo123", 8) != 0) {
		warning("Failed to detect sound bank header");
		return Common::kUnknownError;
	}

	// Read in the bank name, just for debugging
	char bankName[33];
	patch.read(bankName, 32);
	bankName[32] = 0;
	debugC(kDebugLevelSound, "Bank Name: '%s'", bankName);
	
	_instruments.resize(128);

	for (byte i = 0; i < 128; i++) {
		patch.seek(40 + i * 4);
		uint32 offset = patch.readUint32BE();

		if (offset == 0) {
			_instruments[i] = 0;
			continue;
		}

		patch.seek(offset);

		Instrument *instrument = new Instrument();
		_instruments[i] = instrument;

		instrument->index = patch.readUint16BE();
		instrument->mode = patch.readUint16BE();
		instrument->segSize1 = patch.readUint32BE();
		instrument->segSize2 = patch.readUint32BE();
		instrument->segSize3 = patch.readUint32BE();
		instrument->transpose = patch.readUint16BE();
		instrument->attackLength = patch.readByte();
		instrument->decayLength = patch.readByte();
		instrument->sustainLength = patch.readByte();
		instrument->releaseLength = patch.readByte();
		instrument->attackDelta = patch.readSByte();
		instrument->decayDelta = patch.readSByte();
		instrument->sustainDelta = patch.readSByte();
		instrument->releaseDelta = patch.readSByte();
		instrument->attackTarget = patch.readSByte();
		instrument->decayTarget = patch.readSByte();
		instrument->sustainTarget = patch.readSByte();
		instrument->releaseTarget = patch.readSByte();
		patch.read(instrument->name, 30);
		instrument->name[30] = 0;

		// Debug the instrument
		debugC(kDebugLevelSound, "Instrument[%d]: '%s'", i, instrument->name);
		debugC(kDebugLevelSound, "\tMode = %d, Transpose = %d", instrument->mode, instrument->transpose);
		debugC(kDebugLevelSound, "\tSegment 1: %d, 2: %d, 3: %d", instrument->segSize1, instrument->segSize2, instrument->segSize3);
		debugC(kDebugLevelSound, "\tAttack: %d len, %d delta, %d target", instrument->attackLength, instrument->attackDelta, instrument->attackTarget);
		debugC(kDebugLevelSound, "\tDecay: %d len, %d delta, %d target", instrument->decayLength, instrument->decayDelta, instrument->decayTarget);
		debugC(kDebugLevelSound, "\tSustain: %d len, %d delta, %d target", instrument->sustainLength, instrument->sustainDelta, instrument->sustainTarget);
		debugC(kDebugLevelSound, "\tRelease: %d len, %d delta, %d target", instrument->releaseLength, instrument->releaseDelta, instrument->releaseTarget);

		uint32 sampleSize = instrument->segSize1 + instrument->segSize2 + instrument->segSize3;
		instrument->samples = new byte[sampleSize];
		patch.read(instrument->samples, sampleSize);
	}

	return Common::kNoError;
}

void MidiDriver_MacSci0::doLoop() {
	for (uint i = 0; i < kVoices; ++i) {
		if (_voice[i].loopingDisabled) {
			if ((uint)fracToInt(_voice[i].offset) >= _voice[i].segSize3)
				voiceOff(i);
		} else {
			if ((uint)fracToInt(_voice[i].offset) >= _voice[i].segSize2) {
				_voice[i].offset -= intToFrac(_voice[i].segSize2);
				_voice[i].offset += intToFrac(_voice[i].segSize1);
			}
		}
	}
}
void MidiDriver_MacSci0::doEnvelope() {
	for (uint i = 0; i < kVoices; ++i) {
		byte state = _voice[i].envState;
		switch (state) {
		case 0:
			continue;
		case 1:
		case 2:
			--state;
			break;
		case 3:
			continue;
		case 4:
		case 5:
			state -= 2;
			break;
		case 6:
			voiceOff(i);
			_voice[i].envState = 0;
			_voice[i].envCntDown = 0;
			continue;
		}

		if (_voice[i].envCntDown != 0) {
			--_voice[i].envCntDown;
			continue;
		}

		_voice[i].envCntDown = _voice[i].envLength[state];
		if (_voice[i].envVelocity[state] <= 0) {
			voiceOff(i);
			_voice[i].envState = 0;
			_voice[i].envCntDown = 0;
			continue;
		}

		int8 velocity = _voice[i].envVelocity[state];
		if (velocity > 63)
			velocity = 63;
		setMixVelocity(i, velocity);

		int8 delta = _voice[i].envDelta[state];
		if (delta >= 0) {
			_voice[i].envVelocity[state] -= delta;
			if (_voice[i].envVelocity[state] < _voice[i].envVelocity[state + 1])
				++_voice[i].envState;
		} else {
			_voice[i].envVelocity[state] -= delta;
			if (_voice[i].envVelocity[state] > _voice[i].envVelocity[state + 1])
				++_voice[i].envState;
		}

		--_voice[i].envCntDown;
	}
}

void MidiDriver_MacSci0::setMixVelocity(int8 voice, byte velocity) {
	// Additional SCI1 playswitch check for current interpreter
	// mute behaviour that uses the playswitch
	if (_masterVolume == 0 || !_playSwitch)
		velocity = 0;

	_voice[voice].mixVelocity = (_voice[voice].velocity * velocity) >> 6;
}

void MidiDriver_MacSci0::noteOn(int8 voice, int8 note, int8 velocity) {
	if (velocity == 0) {
		noteOff(voice, note);
		return;
	}

	const Instrument *instrument = _voice[voice].instrument;
	if (!instrument)
		return;

	_voice[voice].velocity = velocity >> 1;
	_voice[voice].envLength[0] = instrument->attackLength;
	_voice[voice].envLength[1] = instrument->decayLength;
	_voice[voice].envLength[2] = instrument->sustainLength;
	_voice[voice].envLength[3] = instrument->releaseLength;
	_voice[voice].envDelta[0] = instrument->attackDelta;
	_voice[voice].envDelta[1] = instrument->decayDelta;
	_voice[voice].envDelta[2] = instrument->sustainDelta;
	_voice[voice].envDelta[3] = instrument->releaseDelta;
	_voice[voice].envVelocity[0] = 64;
	_voice[voice].envVelocity[1] = instrument->attackTarget;
	_voice[voice].envVelocity[2] = instrument->decayTarget;
	_voice[voice].envVelocity[3] = instrument->sustainTarget;

	// The original driver (erroneously) reads the phase 4 target from the
	// phase 1 delta. We should perhaps force 0 here or use the actual phase 4
	// target from the patch file.
	_voice[voice].envVelocity[4] = _voice[voice].envDelta[0];

	_voice[voice].envCntDown = 0;

	// Another bug in the original driver, it erases three values belonging to other
	// voices. This code can probably be removed.
	for (int i = voice + 1; i < kVoices; ++i)
		_voice[i].envCntDown = 0;
	for (int i = 0; i < voice; ++i)
		_voice[i].envLength[0] = 0;

	_voice[voice].segSize1 = instrument->segSize1;
	_voice[voice].segSize2 = instrument->segSize2;
	_voice[voice].segSize3 = instrument->segSize3;
	_voice[voice].offset = 0;

	uint16 mode = instrument->mode;

	_voice[voice].samples = instrument->samples;

	int16 transpose = instrument->transpose;
	if (!(mode & MODE_PITCH_CHANGES))
		transpose += 72;
	else
		transpose += note;

	_voice[voice].step = stepTable[transpose];

	if (mode & MODE_LOOPING) {
		_voice[voice].loopingDisabled = false;
		_voice[voice].envState = 1;
	} else {
		_voice[voice].loopingDisabled = true;
		_voice[voice].envState = 0;
	}

	setMixVelocity(voice, 63);
	_voice[voice].note = note;
}

void MidiDriver_MacSci0::noteOff(int8 voice, int8 note) {
	byte state = _voice[voice].envState;
	if (_voice[voice].note == note && state != 0) {
		--state;
		_voice[voice].envVelocity[2] = _voice[voice].envVelocity[state];
		_voice[voice].envState = 4;
		_voice[voice].envCntDown = 0;
	}
}

void MidiDriver_MacSci0::send(uint32 b) {
	byte command = b & 0xf0;
	byte channel = b & 0xf;
	byte op1 = (b >> 8) & 0xff;
	byte op2 = (b >> 16) & 0xff;

	int8 voice = _chanVoice[channel];

	if (voice == -1)
		return;

	switch(command) {
	case 0x80:
		noteOff(voice, op1);
		break;
	case 0x90:
		noteOn(voice, op1, op2);
		break;
	case 0xb0:
		// Not in original driver
		if (op1 == 0x7b)
			voiceOff(voice);
		break;
	case 0xc0:
		if (op1 >= _instruments.size() || !_instruments[op1])
			_voice[voice].instrument = 0;
		else
			_voice[voice].instrument = _instruments[op1];
		break;
	}
}

void MidiDriver_MacSci0::setVolume(byte volume) {
	_masterVolume = CLIP<byte>(volume, 0, 15);
}

static int8 applyVelocity(byte velocity, byte unsignedSample) {
	int8 signedSample = unsignedSample - 0x80;

	if (velocity == 0)
		return 0;

	if (velocity == 63)
		return signedSample;

	if (signedSample >= 0)
		return (signedSample * velocity + 32) / 64;
	else
		return ~((~signedSample * velocity + 32) / 64);
}

void MidiDriver_MacSci0::generateSampleChunk(int16 *data, int len) {
	frac_t offset[kVoices];

	for (uint i = 0; i < kVoices; ++i)
		offset[i] = _voice[i].offset;

	assert(len <= 148);

	// Additional SCI1 playswitch check for current interpreter
	// mute behaviour that uses the playswitch
	byte volume = _masterVolume;
	if (!_playSwitch)
		volume = 0;

	for (int i = 0; i < len; i++) {
		int16 mix = 0;
		for (int v = 0; v < kVoices; v++)
			offset[v] += _voice[v].step;
		for (int v = 0; v < kVoices; v++) {
			uint16 curOffset = fracToInt(offset[v]);
			byte sample = _voice[v].samples[curOffset];
			mix += applyVelocity(_voice[v].mixVelocity, sample);
		}

		data[i] = CLIP<int16>(mix, -128, 127) * 256 * ((volume >> 1) + 1) / 8;
	}

	for (uint i = 0; i < kVoices; ++i)
		_voice[i].offset = offset[i];

	doLoop();
}

MidiDriver_MacSci0::Instrument::Instrument() :
	index(0),
	mode(0),
	segSize1(0),
	segSize2(0),
	segSize3(0),
	transpose(0),
	attackLength(0),
	decayLength(0),
	sustainLength(0),
	releaseLength(0),
	attackDelta(0),
	decayDelta(0),
	sustainDelta(0),
	releaseDelta(0),
	attackTarget(0),
	decayTarget(0),
	sustainTarget(0),
	name(),
	samples(0) {
}

MidiDriver_MacSci0::Instrument::~Instrument() {
	delete[] samples;
}

class MidiPlayer_MacSci0 : public MidiPlayer {
public:
	MidiPlayer_MacSci0(SciVersion version) : MidiPlayer(version) { _driver = new MidiDriver_MacSci0(g_system->getMixer()); }
	~MidiPlayer_MacSci0() {
		delete _driver;
	}

	byte getPlayId() const { return 0x40; }
	int getPolyphony() const { return MidiDriver_MacSci0::kVoices; }
	bool hasRhythmChannel() const { return false; }
	void setVolume(byte volume) { static_cast<MidiDriver_MacSci0 *>(_driver)->setVolume(volume); }
	void playSwitch(bool play) { static_cast<MidiDriver_MacSci0 *>(_driver)->playSwitch(play); }
	void initTrack(SciSpan<const byte> &trackData) { static_cast<MidiDriver_MacSci0 *>(_driver)->initTrack(trackData); }
};

class MidiPlayer_AmigaMac0 : public MidiPlayer {
public:
	enum {
		kVoices = 4,
		kBaseFreq = 60
	};

	MidiPlayer_AmigaMac0(SciVersion version, Audio::Mixer *mixer);
	virtual ~MidiPlayer_AmigaMac0();

	// MidiPlayer
	void close() override;
	void send(uint32 b) override;
	void setTimerCallback(void *timerParam, Common::TimerManager::TimerProc timerProc) override;
	uint32 getBaseTempo() override { return (1000000 + kBaseFreq / 2) / kBaseFreq; }
	byte getPlayId() const override { return 0x40; }
	int getPolyphony() const override { return kVoices; }
	bool hasRhythmChannel() const override { return false; }
	void setVolume(byte volume) override { _masterVolume = CLIP<byte>(volume, 0, 15); }
	int getVolume() override { return _masterVolume; }
	void playSwitch(bool play) override { _playSwitch = play; }
	void initTrack(SciSpan<const byte> &trackData) override;

protected:
	bool _playSwitch;
	uint _masterVolume;

	Audio::Mixer *_mixer;
	Audio::SoundHandle _mixerSoundHandle;
	Common::TimerManager::TimerProc _timerProc;
	void *_timerParam;
	bool _isOpen;

	void freeInstruments();
	void onTimer();

	struct Instrument {
		Instrument() :
			name(),
			loop(false),
			fixedNote(false),
			seg1Size(0),
			seg2Offset(0),
			seg2Size(0),
			seg3Offset(0),
			seg3Size(0),
			samples(nullptr),
			transpose(0),
			envelope() {}

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

	class Voice {
	public:
		Voice(MidiPlayer_AmigaMac0 &driver) :
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
			_driver(driver) {}

		virtual ~Voice() {}

		virtual void noteOn(int8 note, int8 velocity) = 0;
		virtual void noteOff(int8 note) = 0;
		virtual void setPitchWheel(int16 pitch) {}

		virtual void stop() = 0;
		virtual void setEnvelopeVolume(byte volume) = 0;

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

	private:
		MidiPlayer_AmigaMac0 &_driver;
	};

	Common::Array<Voice *> _voices;
	typedef Common::Array<Voice *>::const_iterator VoiceIt;

	Voice *_channels[MIDI_CHANNELS];
};

MidiPlayer_AmigaMac0::MidiPlayer_AmigaMac0(SciVersion version, Audio::Mixer *mixer) :
	MidiPlayer(version),
	_playSwitch(true),
	_masterVolume(15),
	_mixer(mixer),
	_mixerSoundHandle(),
	_timerProc(),
	_timerParam(nullptr),
	_isOpen(false),
	_channels() {}

MidiPlayer_AmigaMac0::~MidiPlayer_AmigaMac0() {
	close();
}

void MidiPlayer_AmigaMac0::close() {
	if (!_isOpen)
		return;

	_mixer->stopHandle(_mixerSoundHandle);

	for (uint ci = 0; ci < ARRAYSIZE(_channels); ++ci)
		_channels[ci] = nullptr;

	for (VoiceIt v = _voices.begin(); v != _voices.end(); ++v)
		delete *v;
	_voices.clear();

	freeInstruments();

	_isOpen = false;
}

void MidiPlayer_AmigaMac0::initTrack(SciSpan<const byte>& header) {
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

		if ((flags & getPlayId()) && (vi < kVoices))
			_channels[i] = _voices[vi++];
		else
			_channels[i] = nullptr;
	}

	_channels[15] = nullptr;

	for (VoiceIt it = _voices.begin(); it != _voices.end(); ++it) {
		Voice *voice = *it;
		voice->stop();
		voice->_note = -1;
		voice->_envState = 0;
		voice->_pitch = 0x2000;
	}
}

void MidiPlayer_AmigaMac0::freeInstruments() {
	for (Common::Array<const Instrument *>::iterator it = _instruments.begin(); it != _instruments.end(); ++it)
		delete *it;

	_instruments.clear();
}

void MidiPlayer_AmigaMac0::onTimer() {
	if (_timerProc)
		(*_timerProc)(_timerParam);

	for (VoiceIt it = _voices.begin(); it != _voices.end(); ++it)
		(*it)->processEnvelope();
}

void MidiPlayer_AmigaMac0::setTimerCallback(void *timerParam, Common::TimerManager::TimerProc timerProc) {
	_timerProc = timerProc;
	_timerParam = timerParam;
}

void MidiPlayer_AmigaMac0::send(uint32 b) {
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
		if (op1 == 0x7b && voice->_note != -1 && voice->_envState < 4)
			voice->noteOff(voice->_note);
		break;
	case 0xc0:
		voice->_patch = op1;
		break;
	case 0xe0:
		voice->setPitchWheel((op2 << 7) | op1);
		break;
	}
}

void MidiPlayer_AmigaMac0::Voice::processEnvelope() {
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

		setEnvelopeVolume(velocity);

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

class MidiPlayer_Amiga0 : public MidiPlayer_AmigaMac0, public Audio::Paula {
public:
	MidiPlayer_Amiga0(SciVersion version, Audio::Mixer *mixer);

	// MidiPlayer
	int open(ResourceManager *resMan) override;

	// MidiDriver
	void close() override;

	// Audio::Paula
	void interrupt() override { onTimer(); }

private:
	class AmigaVoice : public MidiPlayer_AmigaMac0::Voice {
	public:
		AmigaVoice(MidiPlayer_Amiga0 &driver, uint id) : MidiPlayer_AmigaMac0::Voice(driver), _id(id), _amigaDriver(driver) {}

	private:
		void noteOn(int8 note, int8 velocity) override;
		void noteOff(int8 note) override;
		void setPitchWheel(int16 pitch) override;

		void stop() override;
		void setEnvelopeVolume(byte volume) override;

		bool calcVoiceStep();

		byte _id;
		MidiPlayer_Amiga0 &_amigaDriver;
	};

	uint _defaultInstrument;
	bool _isEarlyDriver;

	bool loadInstruments(Common::SeekableReadStream &patch);
};

MidiPlayer_Amiga0::MidiPlayer_Amiga0(SciVersion version, Audio::Mixer *mixer) :
	MidiPlayer_AmigaMac0(version, mixer),
	Audio::Paula(true, mixer->getOutputRate(), mixer->getOutputRate() / kBaseFreq),
	_defaultInstrument(0),
	_isEarlyDriver(false) {}

void MidiPlayer_Amiga0::AmigaVoice::setEnvelopeVolume(byte volume) {
	// Early games ignore note velocity for envelope-enabled notes
	if (_amigaDriver._isEarlyDriver)
		_amigaDriver.setChannelVolume(_id, volume * _amigaDriver._masterVolume >> 4);
	else
		_amigaDriver.setChannelVolume(_id, (volume * _amigaDriver._masterVolume >> 4) * _volume >> 6);
}

int MidiPlayer_Amiga0::open(ResourceManager *resMan) {
	if (_isOpen)
		return MidiDriver::MERR_ALREADY_OPEN;

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
		_voices.push_back(new AmigaVoice(*this, vi));

	startPaula();
	// Enable reverse stereo to counteract Audio::Paula's reverse stereo
	_mixer->playStream(Audio::Mixer::kPlainSoundType, &_mixerSoundHandle, this, -1, _mixer->kMaxChannelVolume, 0, DisposeAfterUse::NO, false, true);
	_isOpen = true;

	return Common::kNoError;
}

void MidiPlayer_Amiga0::close() {
	MidiPlayer_AmigaMac0::close();
	clearVoices();
	stopPaula();
}

void MidiPlayer_Amiga0::AmigaVoice::stop() {
	_amigaDriver.clearVoice(_id);
}

bool MidiPlayer_Amiga0::AmigaVoice::calcVoiceStep() {
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

	_amigaDriver.setChannelPeriod(_id, period);
	return true;
}

void MidiPlayer_Amiga0::AmigaVoice::noteOn(int8 note, int8 velocity) {
	if (velocity == 0) {
		noteOff(note);
		return;
	}

	_instrument = _amigaDriver._instruments[_patch];

	// Default to the first instrument in the bank
	if (!_instrument)
		_instrument = _amigaDriver._instruments[_amigaDriver._defaultInstrument];

	_velocity = velocity;
	_volume = velocity >> 1;
	_loop = _instrument->loop;
	_note = note;

	stop();
	_envState = 0;

	if (!calcVoiceStep())
		return;

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

	if (_instrument->envelope[0].skip != 0 && _loop) {
		_envCurVel = _volume;
		_envCntDown = 0;
		_envState = 1;
	}

	_amigaDriver.setChannelData(_id, seg1, seg2, seg1Size * 2, seg2Size * 2);
	if (_amigaDriver._playSwitch)
		_amigaDriver.setChannelVolume(_id, _amigaDriver._masterVolume * _volume >> 4);
}

void MidiPlayer_Amiga0::AmigaVoice::noteOff(int8 note) {
	if (_note == note) {
		if (_envState != 0) {
			_envCurVel = _instrument->envelope[1].target;
			_envState = 4;
		}
		// Original driver doesn't reset note anywhere that I could find,
		// but this seems like a good place to do that
		_note = -1;
	}
}

void MidiPlayer_Amiga0::AmigaVoice::setPitchWheel(int16 pitch) {
	if (_amigaDriver._isEarlyDriver)
		return;

	_pitch = pitch;

	if (_note != -1)
		calcVoiceStep();
}

bool MidiPlayer_Amiga0::loadInstruments(Common::SeekableReadStream &patch) {
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

MidiPlayer *MidiPlayer_AmigaMac0_create(SciVersion version, Common::Platform platform) {
	if (platform == Common::kPlatformMacintosh)
		return new MidiPlayer_MacSci0(version);
	else
		return new MidiPlayer_Amiga0(version, g_system->getMixer());
}

} // End of namespace Sci
