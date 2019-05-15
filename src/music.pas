unit Music;

{$mode delphi}

interface

{ We define notes and numbers from 0 to 11 }

type
  TNote = Integer;

const
  noteA         = 0;
  noteAsharp    = 1;
  noteB         = 2;
  noteC         = 3;
  noteCsharp    = 4;
  noteD         = 5;
  noteDsharp    = 6;
  noteE         = 7;
  noteF         = 8;
  noteFsharp    = 9;
  noteG         = 10;
  noteGsharp    = 11;

{  The 12th root of two is the half step to the next note }

  HalfStep      = 1.0594631;

{ The octave of a note is doubling or half its frequency }

  OctaveStep    = 2;

{ This is the frequency of some musical notes }

  HertzA        = 220.000;
  HertzAsharp   = 233.082;
  HertzB        = 246.942;
  HertzC        = 261.626;
  HertzCsharp   = 277.183;
  HertzD        = 293.665;
  HertzDsharp   = 311.127;
  HertzE        = 329.628;
  HertzF        = 349.228;
  HertzFsharp   = 369.994;
  HertzG        = 391.995;
  HertzGsharp   = 415.305;

{ The MusicNote function returns a frequency given a note and optional steps }
function MusicNote(Note: TNote; HalfSteps: Integer = 0; Octave: Integer = 0): Double;
{ The MusicNoteName function returns the name of a note }
function MusicNoteName(Note: TNote; HalfSteps: Integer = 0; Octave: Integer = 0): string;

implementation

function MusicNote(Note: TNote; HalfSteps: Integer = 0; Octave: Integer = 0): Double;
var
  Multiplier: Double;
begin
  Note := Note + HalfSteps + Octave * 12;
  Multiplier := 1;
  while Note < 0 do
  begin
    Note := Note + 12;
    Multiplier := Multiplier / OctaveStep;
  end;
  while Note > 11 do
  begin
    Note := Note - 12;
    Multiplier := Multiplier * OctaveStep;
  end;
  case Note of
    noteA: Result := HertzA;
    noteAsharp: Result := HertzAsharp;
    noteB: Result := HertzB;
    noteC: Result := HertzC;
    noteCsharp: Result := HertzCsharp;
    noteD: Result := HertzD;
    noteDsharp: Result := HertzDsharp;
    noteE: Result := HertzE;
    noteF: Result := HertzF;
    noteFsharp: Result := HertzFsharp;
    noteG: Result := HertzG;
  else
    { Note is noteGsharp }
    Result := HertzGsharp;
  end;
  Result := Result * Multiplier;
end;

function IntToStr(Value: Integer): string;
var
  S: string;
  I: Integer;
begin
  S := '';
  I := Abs(Value);
  repeat
    S := Chr(I mod 10 + Ord('0')) + S;
    I := I div 10;
  until I = 0;
  if Value < 0 then
    S := '-' + S;
  Result := S;
end;

function MusicNoteName(Note: TNote; HalfSteps: Integer = 0; Octave: Integer = 0): string;
begin
  Note := Note + HalfSteps + Octave * 12;
  Octave := 0;
  while Note < 0 do
  begin
    Note := Note + 12;
    Dec(Octave);
  end;
  while Note > 11 do
  begin
    Note := Note - 12;
    Inc(Octave);
  end;
  case Note of
    noteA: Result := 'A ';
    noteAsharp: Result := 'A sharp ';
    noteB: Result := 'B ';
    noteC: Result := 'C ';
    noteCsharp: Result := 'C sharp ';
    noteD: Result := 'D ';
    noteDsharp: Result := 'D sharp ';
    noteE: Result := 'E ';
    noteF: Result := 'F ';
    noteFsharp: Result := 'F sharp ';
    noteG: Result := 'G ';
  else
    { Note is G sharp }
    Result := 'G sharp ';
  end;
  Result := Result + IntToStr(Octave);
end;

end.

