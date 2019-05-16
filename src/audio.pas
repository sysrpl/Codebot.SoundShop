unit Audio;

{$i defaults.inc}

interface

{ The TWaveForm type is used to define the shape of sound waves  }

type
  TWaveForm = function(Value: Double): Double;

{ These are a few wave forms }

function WaveSine(Value: Double): Double;
function WaveSemisine(Value: Double): Double;
function WaveSaw(Value: Double): Double;
function WaveSquare(Value: Double): Double;
function WaveTriangle(Value: Double): Double;
function WavePulse(Value: Double): Double;

{ Stop playing audio }
procedure AudioStop;
{ Reume playing audio }
procedure AudioPlay;
{ Set the frequency for a channel }
procedure AudioVoice(Channel: Integer; Frequency: Double);
{ Set the wave form for all the voices }
procedure AudioWaveForm(Wave: TWaveForm);
{ The time as calculated by the audio system }
function AudioTime: Double;
{ Clear all channels and reset the time to zero }
procedure AudioReset;

function AudioInit: Boolean;
function AudioQuit: Boolean;

implementation

uses
  SDL2;

const
  SampleRate = 44100;
  Volume = 2048;

var
  Mutex: PSDL_Mutex;

function WaveSine(Value: Double): Double;
begin
  Result := Sin(Value * 2 * Pi);
end;

function WaveSemisine(Value: Double): Double;
begin
  Result := Abs(Sin(Value * 2 * Pi / 2)) * 2 - 1;
end;

function WaveSaw(Value: Double): Double;
begin
  Result := Value * 2 - 1;
end;

function WaveSquare(Value: Double): Double;
begin
  if Value < 0.5 then Result := -1 else Result := 1;
end;

function WaveTriangle(Value: Double): Double;
begin
  if Value > 0.5 then
    Value := 1 - Value;
  Result := Value * 2 - 1;
end;

function WavePulse(Value: Double): Double;
begin
  if Value < 0.5 then Result := 0 else Result := 1;
end;

type
  TAudioSample = record
    L: SmallInt;
    R: SmallInt;
  end;
  PAudioSample = ^TAudioSample;

const
  ChannelLow = 0;
  ChannelHigh = 127;

type
  TChannel = record
    { The note to be played }
    Frequency: Double;
    { The time the note was started }
    Time: Double;
    { The time the note was released }
    Drop: Double;
    { A small phase change for each channel }
    Phase: Double;
  end;

var
  AudioInitialized: Boolean;
  AudioLoaded: Boolean;
  AudioBuffer: array[0..2047] of Byte;
  AudioCounter: Int64;
  AudioChannels: array[ChannelLow..ChannelHigh] of TChannel;
  AudioWave: TWaveForm = WaveSine;

function AudioTime: Double;
begin
  if AudioLoaded then
  begin
    SDL_LockMutex(Mutex);
    try
      Result := AudioCounter / AUDIO_FREQ_CD_QUALITY;
    finally
      SDL_UnlockMutex(Mutex);
    end;
  end
  else
    Result := 0;
end;

procedure AudioReset;
begin
  if AudioLoaded then
  begin
    SDL_LockMutex(Mutex);
    try
      AudioCounter := 0;
      FillChar(AudioChannels[ChannelLow], Length(AudioChannels) * SizeOf(TChannel), 0);
    finally
      SDL_UnlockMutex(Mutex);
    end;
  end;
end;

const
  Attack = 0.1;
  Release = 0.05;
  Sustain = 5.0;
  Slice = 1 / AUDIO_FREQ_CD_QUALITY;

procedure AudioMixer(userdata: Pointer; stream: PUInt8; len: LongInt); cdecl;
var
  Time: Double;

  { Frequency is note to be played,
    Start is the time the note was started
    Drop is the time the note was released }

  procedure PlayFrequency(Frequency, Start, Drop, Phase: Double);
  var
    Sample: PAudioSample;
    Marker: Double;
    Mix: Double;
    C: Int64;
    V: Double;
    S: SmallInt;
    I: LongInt;
  begin
    FillChar(AudioBuffer, SizeOf(AudioBuffer), 0);
    Frequency := SampleRate / Frequency;
    C := AudioCounter;
    Sample := PAudioSample(@AudioBuffer);
    Marker := Time;
    for I := 1 to len div SizeOf(TAudioSample) do
    begin
      V := Frac(C / Frequency + Phase);
      if Marker - Start < Attack then
        Mix := (Marker - Start) / Attack
      else
        Mix := 1;
      if Marker - Start > Sustain then
        Mix := 0
      else
        Mix := Mix * (1 - (Marker - Start) / Sustain);
      if Drop > 0 then
        if Marker >= Drop then
          Mix := 0
        else
          Mix := Mix * (Drop - Marker) / Release;
      S := Trunc(AudioWave(V) * Volume * Mix);
      Sample.L := S;
      Sample.R := S;
      Inc(C);
      Inc(Sample);
      Marker := Marker + Slice;
    end;
    SDL_MixAudio(stream, @AudioBuffer, len, SDL_MIX_MAXVOLUME);
  end;

var
  F: Double;
  S: Double;
  I: Integer;
begin
  FillChar(stream^, len, 0);
  SDL_LockMutex(Mutex);
  try
    Time := AudioCounter / AUDIO_FREQ_CD_QUALITY;
    for I := Low(AudioChannels) to High(AudioChannels) do
    begin
      F := AudioChannels[I].Frequency;
      S := AudioChannels[I].Time;
      if (F > 10) and (F < 30000) and (S + Sustain > Time) then
        PlayFrequency(F, S, AudioChannels[I].Drop, AudioChannels[I].Phase);
    end;
    AudioCounter := AudioCounter + len div SizeOf(TAudioSample);
  finally
    SDL_UnlockMutex(Mutex);
  end;
end;

procedure AudioStop;
begin
  if AudioLoaded then
    SDL_PauseAudio(1);
end;

procedure AudioPlay;
begin
  if AudioLoaded then
    SDL_PauseAudio(0);
end;

procedure AudioVoice(Channel: Integer; Frequency: Double);
var
  Time: Double;
begin
  if Channel in [ChannelLow..ChannelHigh] then
  begin
    SDL_LockMutex(Mutex);
    try
      Time := AudioCounter / AUDIO_FREQ_CD_QUALITY;
      if Frequency = 0 then
      begin
        if Time < AudioChannels[Channel].Time + Sustain then
          AudioChannels[Channel].Drop := Time + Release
        else
          AudioChannels[Channel].Drop := 0;
      end
      else
      begin
        AudioChannels[Channel].Frequency := Frequency;
        AudioChannels[Channel].Time := Time;
        AudioChannels[Channel].Drop := 0;
        AudioChannels[Channel].Phase := Random;
      end;
    finally
      SDL_UnlockMutex(Mutex);
    end;
  end;
end;

procedure AudioWaveForm(Wave: TWaveForm);
begin
  SDL_LockMutex(Mutex);
  try
    AudioWave := Wave;
  finally
    SDL_UnlockMutex(Mutex);
  end;
end;

function AudioInit: Boolean;
var
  Spec: TSDL_AudioSpec;
begin
  if AudioInitialized then
    Exit(AudioLoaded);
  AudioInitialized := True;
  AudioLoaded := SDL_Init(SDL_INIT_AUDIO) = 0;
  if AudioLoaded then
  begin
    Mutex := SDL_CreateMutex;
    FillChar(Spec{%H-}, SizeOf(Spec), 0);
    Spec.format := AUDIO_S16;
    Spec.channels := AUDIO_CHAN_STEREO;
    Spec.freq := AUDIO_FREQ_CD_QUALITY;
    Spec.samples := AUDIO_SAMPLE_SMALL;
    Spec.callback := @AudioMixer;
    SDL_OpenAudio(@Spec, nil);
    SDL_PauseAudio(0);
  end;
  Result := AudioLoaded;
end;

function AudioQuit: Boolean;
begin
  Result := AudioLoaded;
  if Result then
  begin
    SDL_Quit;
    SDL_DestroyMutex(Mutex);
  end;
  AudioLoaded := False;
end;

end.

