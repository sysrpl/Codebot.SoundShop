unit Main;

{$i defaults.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Audio, Piano;

{ TMusicalForm }

type
  TMusicalForm = class(TForm)
    AudioTimer: TTimer;
    WaveBox: TComboBox;
    MuteBox: TCheckBox;
    procedure AudioTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure MuteBoxChange(Sender: TObject);
    procedure WaveBoxChange(Sender: TObject);
  private
    FPiano: TPianoKeyboard;
    FMouseDown: Boolean;
    FDownKey: Integer;
    procedure PianoKeyToggle(Sender: TObject; Key: Integer; Down: Boolean);
    procedure PianoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PianoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PianoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public

  end;

var
  MusicalForm: TMusicalForm;

implementation

{$R *.lfm}

type
  TInit = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TInit.Execute;
begin
  FreeOnTerminate := True;
end;

procedure ThreadsInit;
begin
  TInit.Create(False);
end;

{ TMusicalForm }

procedure TMusicalForm.FormCreate(Sender: TObject);
begin
  ThreadsInit;
  AudioInit;
  FPiano := TPianoKeyboard.Create(Self);
  FPiano.Parent := Self;
  FPiano.Align := alClient;
  FPiano.SetMargin(Rect(0, 1, 0, 0));
  FPiano.ScaleFactor := 0.75;
  FPiano.OnKeyToggle := PianoKeyToggle;
  FPiano.OnMouseDown := PianoMouseDown;
  FPiano.OnMouseMove := PianoMouseMove;
  FPiano.OnMouseUp := PianoMouseUp;
end;

procedure TMusicalForm.AudioTimerTimer(Sender: TObject);
begin
  Caption := Format('Audio time is %.3f', [AudioTime]);
end;

procedure TMusicalForm.FormDestroy(Sender: TObject);
begin
  AudioQuit;
end;

procedure TMusicalForm.PianoKeyToggle(Sender: TObject; Key: Integer; Down: Boolean);
begin
  if Down then
    AudioVoice(Key, FPiano.KeyToFrequency(Key))
  else
    AudioVoice(Key, 0);
end;

procedure TMusicalForm.PianoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FMouseDown := True;
    FDownKey := FPiano.KeyFromPoint(X, Y);
    FPiano.Key[FDownKey] := True;
  end;
end;

procedure TMusicalForm.PianoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  I: Integer;
begin
  if FMouseDown then
  begin
    I := FPiano.KeyFromPoint(X, Y);
    if I <> FDownKey then
    begin
      FPiano.Key[FDownKey] := False;
      FDownKey := I;
      FPiano.Key[FDownKey] := True;
    end;
  end;
end;

procedure TMusicalForm.PianoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FMouseDown then
  begin
    FPiano.Key[FDownKey] := False;
    FDownKey := -1;
    FMouseDown := False;
  end;
end;

const
  Notes = 'Q2W3ER5T6Y7UI9O0P';
  NoteOffset = 23;

procedure TMusicalForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  C: Char;
  I: Integer;
begin
  C := Chr(Key);
  I := Pos(C, Notes);
  if I > 0 then
    FPiano.Key[I + NoteOffset] := True;
end;

procedure TMusicalForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  C: Char;
  I: Integer;
begin
  C := Chr(Key);
  I := Pos(C, Notes);
  if I > 0 then
    FPiano.Key[I + NoteOffset] := False;
end;

procedure TMusicalForm.FormResize(Sender: TObject);
var
  Scale: Double;
  Size: TPoint;
  X, Y: Integer;
begin
  if FPiano = nil then
    Exit;
  X := FPiano.Width - 16;
  Y := FPiano.Height;
  Scale := 0;
  while Scale < 2 do
  begin
    Scale := Scale + 0.1;
    Size := FPiano.CalculatedSize(Scale);
    if (Size.X > X) or (Size.Y > Y) then
    begin
      Scale := Scale - 0.1;
      Break;
    end;
  end;
  FPiano.ScaleFactor := Scale;
end;

procedure TMusicalForm.MuteBoxChange(Sender: TObject);
begin
  if MuteBox.Checked then
    AudioStop
  else
    AudioPlay;
end;

procedure TMusicalForm.WaveBoxChange(Sender: TObject);
begin
  case WaveBox.ItemIndex of
    0: AudioWaveForm(WaveSine);
    1: AudioWaveForm(WaveSaw);
    2: AudioWaveForm(WaveTriangle);
    3: AudioWaveForm(WaveSquare);
    4: AudioWaveForm(WavePulse);
    5: AudioWaveForm(WaveSemisine);
  end;
end;

end.

