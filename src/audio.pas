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
{ Set the frequency for a channel, optionally with a start time }
procedure AudioVoice(Channel: Integer; Frequency: Double; Time: Double = 0);
{ Set the wave form for all the voices }
procedure AudioWaveForm(Wave: TWaveForm);
{ The time as calculated by the audio system }
function AudioTime: Double;
{ Alter the time by increasing or decreasing the tempo }
procedure AudioTempo(TimeFactor: Double);
{ Clear all channels and reset the time to zero }
procedure AudioReset;

{ These routines can be used to save audio to a pcm wav audio file }

type
  TAudioWrite = procedure(Buffer: Pointer; Size: Integer) of object;

procedure AudioRecordingStart(Write: TAudioWrite);
procedure AudioRecordingStop;

{ Start and stop the audio system }

function AudioInit: Boolean;
function AudioQuit: Boolean;

implementation

uses
  SDL2;

const
  SampleRate = 44100;
  Volume = 4096;

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

const
  ChannelLow = 0;
  ChannelHigh = 127;

type
  TAudioSample = record
    L: SmallInt;
    R: SmallInt;
  end;
  PAudioSample = ^TAudioSample;

  TChannel = record
    { The note to be played }
    Frequency: Double;
    { The time the note was started }
    Start: Double;
    { The time the note was released }
    Drop: Double;
    { A small phase change for each channel }
    Phase: Double;
    { Culmination of sample values }
    Sample: Integer;
    { Play a channel if it is active }
    Active: Boolean;
  end;

var
  AudioInitialized: Boolean;
  AudioLoaded: Boolean;
  AudioCounter: Int64;
  AudioTimeCounter: Int64;
  AudioTimeFactor: Double;
  AudioChannels: array[ChannelLow..ChannelHigh] of TChannel;
  AudioWave: TWaveForm = WaveSine;
  AudioWrite: TAudioWrite;

function AudioTime: Double;
begin
  if AudioLoaded then
  begin
    SDL_LockMutex(Mutex);
    try
      Result := AudioTimeCounter / AUDIO_FREQ_CD_QUALITY;
    finally
      SDL_UnlockMutex(Mutex);
    end;
  end
  else
    Result := 0;
end;

procedure AudioTempo(TimeFactor: Double);
begin
  if TimeFactor < 0.1 then TimeFactor := 0.1;
  if TimeFactor > 10 then TimeFactor := 10;
  if TimeFactor = AudioTimeFactor then Exit;
  if AudioLoaded then
  begin
    SDL_LockMutex(Mutex);
    try
      AudioTimeFactor := TimeFactor;
    finally
      SDL_UnlockMutex(Mutex);
    end;
  end;
end;

procedure AudioReset;
begin
  if AudioLoaded then
  begin
    SDL_LockMutex(Mutex);
    try
      AudioCounter := 0;
      AudioTimeCounter := 0;
      FillChar(AudioChannels[0], Length(AudioChannels) * SizeOf(TChannel), 0);
    finally
      SDL_UnlockMutex(Mutex);
    end;
  end;
end;

const
  Attack = 0.1;
  Release = 0.1;
  Sustain = 5.0;
  Slice = 1 / AUDIO_FREQ_CD_QUALITY;

procedure AudioMixer(userdata: Pointer; stream: PUInt8; len: LongInt); cdecl;
var
  Time: Double;
  AttackTime: Double;
  ReleaseTime: Double;
  SustainTime: Double;
  SliceTime: Double;

  { Frequency is note to be played,
    Start is the time the note was started
    Drop is the time the note was released }

  procedure PlayChannel(var Channel: TChannel);
  const
    DeadMix = 0.05;
    Smooth = 80;
    SmoothSmall = 1 / Smooth;
    SmoothBig = 1 - SmoothSmall;
  var
    Sample: PAudioSample;
    Marker: Double;
    Mix: Double;
    C: Int64;
    F, V: Double;
    S: SmallInt;
    I: LongInt;
  begin
    if (Channel.Frequency < 10) or (Channel.Frequency > 30000) then
      F := 0
    else
      F := SampleRate / Channel.Frequency;
    Sample := PAudioSample(stream);
    Marker := Time;
    C := AudioCounter;
    for I := 1 to len div SizeOf(TAudioSample) do
    begin
      S := 0;
      if Marker < Channel.Start then
        S := 0
      else if F < 10 then
        S := 0
      else
      begin
        V := Frac(C / F + Channel.Phase);
        if Marker - Channel.Start < Attack * AudioTimeFactor then
          Mix := (Marker - Channel.Start) / AttackTime
        else
          Mix := 1;
        if Marker - Channel.Start > SustainTime then
          Mix := 0
        else
          Mix := Mix * (1 - (Marker - Channel.Start) / SustainTime);
        if Channel.Drop > 0 then
        begin
          if Marker >= Channel.Drop then
          begin
            Channel.Frequency := 0;
            Channel.Active := False;
            Mix := 0
          end
          else
            Mix := Mix * (Channel.Drop - Marker) / ReleaseTime;
        end;
        if Mix < DeadMix then
          S := 0
        else
          S := Trunc(AudioWave(V) * Volume * Mix);
      end;
      S := Trunc(Channel.Sample * SmoothBig + S * SmoothSmall);
      Inc(Sample.L, S);
      Inc(Sample.R, S);
      Channel.Sample := S;
      Inc(C);
      Inc(Sample);
      Marker := Marker + SliceTime;
    end;
  end;

var
  I: Integer;
begin
  FillChar(stream^, len, 0);
  SDL_LockMutex(Mutex);
  try
    Time := AudioTimeCounter / AUDIO_FREQ_CD_QUALITY;
    AttackTime := Attack * AudioTimeFactor;
    ReleaseTime := Release * AudioTimeFactor;
    SustainTime := Sustain * AudioTimeFactor;
    SliceTime := Slice * AudioTimeFactor;
    for I := Low(AudioChannels) to High(AudioChannels) do
      if AudioChannels[I].Active then
        PlayChannel(AudioChannels[I]);
    AudioCounter := AudioCounter + len div SizeOf(TAudioSample);
    AudioTimeCounter := Round(AudioTimeCounter + len div SizeOf(TAudioSample) * AudioTimeFactor);
    if Assigned(AudioWrite) then
      AudioWrite(stream, len);
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

procedure AudioVoice(Channel: Integer; Frequency: Double; Time: Double = 0);
const
  Delta = 0.01;
var
  Start: Double;
begin
  if Channel in [ChannelLow..ChannelHigh] then
  begin
    SDL_LockMutex(Mutex);
    try
      if Time > 0 then
        Start := Time + Delta
      else
        Start := AudioTimeCounter / AUDIO_FREQ_CD_QUALITY;
      if Frequency = 0 then
      begin
        if AudioChannels[Channel].Start + Sustain > Start then
          AudioChannels[Channel].Drop := Start + Release * AudioTimeFactor
        else
        begin
          AudioChannels[Channel].Frequency := 0;
          AudioChannels[Channel].Drop := 0;
        end;
      end
      else
      begin
        AudioChannels[Channel].Frequency := Frequency;
        AudioChannels[Channel].Start := Start;
        AudioChannels[Channel].Drop := 0;
        AudioChannels[Channel].Phase := Channel / ChannelHigh;
        AudioChannels[Channel].Active := True;
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

procedure AudioRecordingStart(Write: TAudioWrite);
begin
  SDL_LockMutex(Mutex);
  try
    AudioWrite := Write;
  finally
    SDL_UnlockMutex(Mutex);
  end;
end;

procedure AudioRecordingStop;
begin
  SDL_LockMutex(Mutex);
  try
    AudioWrite := nil;
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
    AudioTimeFactor := 1;
    Mutex := SDL_CreateMutex;
    FillChar(Spec{%H-}, SizeOf(Spec), 0);
    Spec.format := AUDIO_S16;
    Spec.channels := AUDIO_CHAN_STEREO;
    Spec.freq := AUDIO_FREQ_CD_QUALITY;
    Spec.samples := AUDIO_SAMPLE_MEDIUM;
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
    SDL_LockMutex(Mutex);
    SDL_DestroyMutex(Mutex);
  end;
  AudioLoaded := False;
end;

end.


