unit Main;

{$i defaults.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, Audio, Piano;

{ TMusicalForm }

type
  TMusicalForm = class(TForm)
    AudioTimer: TTimer;
    OpenDialog: TOpenDialog;
    StopButton: TSpeedButton;
    TempoBar: TTrackBar;
    TempoValueLabel: TLabel;
    WaveLabel: TLabel;
    MusicPanel: TPanel;
    MuteBox: TCheckBox;
    SongButton: TSpeedButton;
    PlayButton: TSpeedButton;
    WaveBox: TComboBox;
    TempoLabel: TLabel;
    SongLabel: TLabel;
    procedure AudioTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MuteBoxChange(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure SongButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure TempoBarChange(Sender: TObject);
    procedure TempoLabelClick(Sender: TObject);
    procedure WaveBoxChange(Sender: TObject);
  private
    FPiano: TPianoKeyboard;
    FMouseDown: Boolean;
    FDownKey: Integer;
    FTime: Double;
    FTempo: Double;
    FTempoTime: Double;
    FFileName: string;
    procedure StopMusic;
    procedure PlayMusic;
    procedure PianoKeyToggle(Sender: TObject; Key: Integer; Down: Boolean);
    procedure PianoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PianoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PianoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
  FTempo := 1;
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
var
  Time, Delta: Double;
  Minutes, Seconds: Integer;
begin
  if MuteBox.Checked then
    Exit;
  Time := AudioTime;
  Delta := (Time - FTime) * FTempo;
  FTempoTime := FTempoTime + Delta;
  FPiano.Music.Play(FTempoTime);
  if FPiano.Music.Stopped then
    StopMusic
  else
  begin
    FTime := Time;
    Minutes := Trunc(Time) div 60;
    Seconds := Trunc(Time - Minutes * 60);
    if Seconds < 10 then
      SongLabel.Caption := Format('Music:  %s / %d:0%d', [FFileName, Minutes, Seconds])
    else
      SongLabel.Caption := Format('Music: %s / %d:%d', [FFileName, Minutes, Seconds]);
  end;
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

procedure TMusicalForm.FormShow(Sender: TObject);
const
  clStyleText = clWhite;
  clStyleWindow = 5391680;
var
  C: TControl;
  I: Integer;
begin
  OnShow := nil;
  Color := clStyleWindow;
  Font.Color := clStyleText;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TControl then
    begin
      C := Components[I] as TControl;
      C.Color := clStyleWindow;
      C.Font.Color := clStyleText;
      C := Components[I] as TControl;
      if C.Parent <> MusicPanel then
        Continue;
      C.Top := (MusicPanel.Height - C.Height) div 2;
    end;
end;

procedure TMusicalForm.MuteBoxChange(Sender: TObject);
begin
  if MuteBox.Checked then
    AudioStop
  else
    AudioPlay;
end;

procedure TMusicalForm.StopMusic;
begin
  AudioTimer.Enabled := False;
  if FFileName <> '' then
  begin
    SongLabel.Caption := 'Music:  ' + FFileName + ' / stopped';
  end
  else
    SongLabel.Caption := 'Music:  <no song selected>';
  FPiano.Reset;
end;

procedure TMusicalForm.PlayMusic;
begin
  if FFileName <> '' then
  begin
    FTime := 0;
    FTempoTime := 0;
    AudioTimer.Enabled := True;
    AudioReset;
  end;
end;

procedure TMusicalForm.SongButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FPiano.Music.LoadFromFile(OpenDialog.FileName);
    FFileName := ExtractFileName(OpenDialog.FileName);
    SetLength(FFileName, Length(FFileName) - 5);
    StopMusic;
    PlayMusic;
  end;
end;

procedure TMusicalForm.PlayButtonClick(Sender: TObject);
begin
  if not AudioTimer.Enabled then
    PlayMusic;
end;

procedure TMusicalForm.StopButtonClick(Sender: TObject);
begin
  StopMusic;
end;

procedure TMusicalForm.TempoBarChange(Sender: TObject);
begin
  FTempo := TempoBar.Position / 4;
  if Frac(FTempo) = 0 then
    TempoValueLabel.Caption := IntToStr(Trunc(FTempo)) + ' x'
  else if Frac(FTempo * 2) = 0 then
    TempoValueLabel.Caption := Format('%.1f x', [FTempo])
  else
    TempoValueLabel.Caption := Format('%.2f x', [FTempo]);
end;

procedure TMusicalForm.TempoLabelClick(Sender: TObject);
begin

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

