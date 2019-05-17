unit Recording;

{$i defaults.inc}

interface

uses
  Classes, SysUtils;

{ TAudioRecorder }

type
  TAudioRecorder = class
  private
    FSource: TStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure WriteMemory(Buffer: Pointer; Size: Integer);
    procedure SaveToFile(const FileName: string);
  end;

implementation

{ TAudioRecorder }

constructor TAudioRecorder.Create;
begin
  inherited Create;
  FSource := TMemoryStream.Create;
end;

destructor TAudioRecorder.Destroy;
begin
  inherited Destroy;
  FSource.Free;
end;

procedure TAudioRecorder.Reset;
begin
  FSource.Free;
  FSource := TMemoryStream.Create;
end;

procedure TAudioRecorder.WriteMemory(Buffer: Pointer; Size: Integer);
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
    Subchunk2Size: Integer;
  end;

procedure TAudioRecorder.SaveToFile(const FileName: string);
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

end.

