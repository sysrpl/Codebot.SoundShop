unit Wave;

{$i defaults.inc}

interface

uses
  Classes, SysUtils;

{ TWaveRecorder }

type
  TWaveRecorder = class
  private
    FSource: TStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure WriteMemory(Buffer: Pointer; Size: Integer);
    procedure SaveToFile(const FileName: string);
  end;

{ TWaveSamples }

  TWaveSamples = class
  private
    FData: Pointer;
    FCount: LongWord;
  public
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure Read(SampleIndex: LongWord; out L, R: SmallInt);
    procedure Reset;
    { Number of samples }
    property Count: LongWord read FCount;
    { Direct access to a sample }
    property Data: Pointer read FData;
  end;

implementation

{ TWaveRecorder }

constructor TWaveRecorder.Create;
begin
  inherited Create;
  FSource := TMemoryStream.Create;
end;

destructor TWaveRecorder.Destroy;
begin
  inherited Destroy;
  FSource.Free;
end;

procedure TWaveRecorder.Reset;
begin
  FSource.Free;
  FSource := TMemoryStream.Create;
end;

procedure TWaveRecorder.WriteMemory(Buffer: Pointer; Size: Integer);
begin
  FSource.Write(Buffer^, Size);
end;

type
  TWaveTag = array[1..4] of Char;

function WaveTag(const S: string): TWaveTag;
begin
  Result[1] := S[1];
  Result[2] := S[2];
  Result[3] := S[3];
  Result[4] := S[4];
end;

{ See this page: http://soundfile.sapp.org/doc/WaveFormat/ }

type
  TWaveHeader = packed record
    { 'RIFF' }
    ChunkID: TWaveTag;
    { SizeOf(TWaveHeader) + Size - 8 }
    ChunkSize: LongWord;
    { 'WAVE' }
    Format: TWaveTag;
    { 'fmt ' }
    Subchunk1ID: TWaveTag;
    { 16 }
    Subchunk1Size: LongWord;
    { 1 }
    AudioFormat: Word;
    { 2 }
    NumChannels: Word;
    { 44100 }
    SampleRate: LongWord;
    { 44100 * 2 * 2 }
    ByteRate: LongWord;
    { 4 }
    BlockAlign: Word;
    { 16 }
    BitsPerSample: Word;
    { 'data' }
    Subchunk2ID: TWaveTag;
    { Size }
    Subchunk2Size: LongWord;
  end;

procedure TWaveRecorder.SaveToFile(const FileName: string);
var
  Dest: TStream;
  H: TWaveHeader;
begin
  H.ChunkID := WaveTag('RIFF');
  H.ChunkSize := SizeOf(H) + FSource.Size - 8;
  H.Format := WaveTag('WAVE');
  H.Subchunk1ID := WaveTag('fmt ');
  H.Subchunk1Size := 16;
  H.AudioFormat := 1;
  H.NumChannels := 2;
  H.SampleRate := 44100;
  H.ByteRate := 44100 * 2 * 2;
  H.BlockAlign := 4;
  H.BitsPerSample := 16;
  H.Subchunk2ID := WaveTag('data');
  Dest := TFileStream.Create(FileName, fmCreate);
  try
    FSource.Seek(0, 0);
    Dest.Write(H, SizeOf(H));
    Dest.CopyFrom(FSource, FSource.Size);
  finally
    Dest.Free;
  end;
end;

{ TWaveSamples }

type
  TSample = record
    L, R: SmallInt;
  end;
  PSample = ^TSample;

{ See this page http://theremin.music.uiowa.edu/MISpiano.html  }

destructor TWaveSamples.Destroy;
begin
  Reset;
  inherited Destroy;
end;

procedure TWaveSamples.LoadFromFile(const FileName: string);
var
  Stream: TMemoryStream;
  B: PByte;
  H: TWaveHeader;
begin
  Reset;
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    B := Stream.Memory;
    Stream.Read(H, SizeOf(H));
    FCount := (Stream.Size - SizeOf(H)) div SizeOf(TSample);
    if FCount < 1 then
      Exit;
    Inc(B, SizeOf(H));
    GetMem(FData, FCount * SizeOf(TSample));
    Move(B^, FData, FCount * SizeOf(TSample));
  finally
    Stream.Free;
  end;
end;

procedure TWaveSamples.Read(SampleIndex: LongWord; out L, R: SmallInt);
var
  S: PSample;
begin
  L := 0;
  R := 0;
  if (FData = nil) or (SampleIndex > FCount - 1) then
    Exit;
  S := FData;
  Inc(S, SampleIndex);
  L := S.L;
  R := S.R;
end;

procedure TWaveSamples.Reset;
begin
  if FData <> nil then
    FreeMem(FData);
  FData := nil;
  FCount := 0;
end;

end.

