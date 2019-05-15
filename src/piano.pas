unit Piano;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Controls, Music, Scaling;

{ TPianoKeyboard }

type
  TPianoKeyEvent = procedure(Sender: TObject; Key: Integer; Down: Boolean) of object;

  TPianoKeyboard = class(TGraphicControl)
  private
    FScaleFactor: Double;
    FCalculatedSize: TPoint;
    FNaturalSize: TPoint;
    FSharpSize: TPoint;
    FMargin: TRect;
    FNatural: TRasterImage;
    FNaturalDown: TRasterImage;
    FSharp: TRasterImage;
    FSharpDown: TRasterImage;
    FKeys: array of Boolean;
    FOnKeyToggle: TPianoKeyEvent;
    function InternalKeyToRect(Index: Integer): TRect;
    procedure RecalculateSize;
    function GetKey(Index: Integer): Boolean;
    procedure SetKey(Index: Integer; Value: Boolean);
    procedure SetKeyCount(Value: Integer);
    function GetKeyCount: Integer;
    procedure SetScaleFactor(Value: Double);
  protected
    procedure DoKeyToggle(Key: Integer; Down: Boolean); virtual;
    procedure NeedImages;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CalculatedSize(AScaleFactor: Double = 0): TPoint;
    function IsNaturalKey(const Key: Integer): Boolean;
    function KeyFromPoint(X, Y: Integer): Integer;
    function KeyToRect(Index: Integer): TRect;
    function KeyToFrequency(Index: Integer): Double;
    function KeyToNote(Index: Integer): Integer;
    function KeyToString(Index: Integer): string;
    procedure SetMargin(const Rect: TRect);
    property Key[Index: Integer]: Boolean read GetKey write SetKey;
  published
    property KeyCount: Integer read GetKeyCount write SetKeyCount;
    property ScaleFactor: Double read FScaleFactor write SetScaleFactor;
    property OnKeyToggle: TPianoKeyEvent read FOnKeyToggle write FOnKeyToggle;
    property Align;
    property Color;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

{$R piano.res}

var
  NaturalShared: TRasterImage;
  NaturalDownShared: TRasterImage;
  SharpShared: TRasterImage;
  SharpDownShared: TRasterImage;

const
  NoteOffset = 3;

{ TPianoKeyboard }

function PointInRect(const Rect: TRect; X, Y: Integer): Boolean;
begin
  Result := (X >= Rect.Left) and (Y >= Rect.Top) and (X < Rect.Right) and (Y < Rect.Bottom);
end;

procedure OffsetRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

constructor TPianoKeyboard.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  if NaturalShared = nil then
  begin
    NaturalShared := TPortableNetworkGraphic.Create;
    NaturalShared.LoadFromResourceName(HINSTANCE, 'natural');
    NaturalDownShared := TPortableNetworkGraphic.Create;
    NaturalDownShared.LoadFromResourceName(HINSTANCE, 'natural-down');
    SharpShared := TPortableNetworkGraphic.Create;
    SharpShared.LoadFromResourceName(HINSTANCE, 'sharp');
    SharpDownShared := TPortableNetworkGraphic.Create;
    SharpDownShared.LoadFromResourceName(HINSTANCE, 'sharp-down');
  end;
  ScaleFactor := 1;
  Height := 600;
  Width := 1200;
  SetLength(FKeys, 84);
  for I := Low(FKeys) to High(FKeys) do
    FKeys[I] := False;
end;

destructor TPianoKeyboard.Destroy;
begin
  FNatural.Free;
  FNaturalDown.Free;
  FSharp.Free;
  FSharpDown.Free;
  inherited Destroy;
end;

function TPianoKeyboard.InternalKeyToRect(Index: Integer): TRect;
var
  I: Integer;
begin
  Result := Classes.Rect(0, 0, 0, 0);
  if Index < Low(FKeys) then
    Exit;
  if Index > High(FKeys) then
    Exit;
  Result := Classes.Rect(0, 0, FNaturalSize.X, FNaturalSize.Y);
  if Index = 0 then
    Exit;
  for I := 2 to High(FKeys) do
    if IsNaturalKey(I) then
    begin
      OffsetRect(Result, FNaturalSize.X, 0);
      if I = Index then
        Exit;
    end;
  Result := Classes.Rect(0, 0, FSharpSize.X, FSharpSize.Y);
  OffsetRect(Result, FNaturalSize.X - FSharpSize.X div 2, 0);
  if Index = 1 then
    Exit;
  for I := 3 to High(FKeys) do
    if not IsNaturalKey(I) then
    begin
      OffsetRect(Result, FNaturalSize.X, 0);
      if I = Index then
        Exit;
    end
    else if Byte((I + NoteOffset) mod 12) in [noteB, noteE] then
      OffsetRect(Result, FNaturalSize.X, 0);
  Result := Classes.Rect(0, 0, 0, 0);
end;

procedure TPianoKeyboard.RecalculateSize;
var
  R: TRect;
begin
  R := InternalKeyToRect(High(FKeys));
  FCalculatedSize.X := R.Right;
  FCalculatedSize.Y := FNaturalSize.Y;
end;

function TPianoKeyboard.CalculatedSize(AScaleFactor: Double = 0): TPoint;
var
  P1, P2: TPoint;
  S: Double;
begin
  S := AScaleFactor;
  if S < 0.01 then
    Exit(FCalculatedSize);
  if S < 0.1 then
    S := 0.1;
  if S > 2 then
    S := 2;
  P1 := FNaturalSize;
  P2 := FSharpSize;
  FNaturalSize.X := Round(NaturalShared.Width * S / 2);
  FNaturalSize.Y := Round(NaturalShared.Height * S / 2);
  FSharpSize.X := Round(SharpShared.Width * S / 2);
  FSharpSize.Y := Round(SharpShared.Height * S / 2);
  RecalculateSize;
  Result := FCalculatedSize;
  FNaturalSize := P1;
  FSharpSize := P2;
  RecalculateSize;
end;

function TPianoKeyboard.IsNaturalKey(const Key: Integer): Boolean;
begin
  Result := Byte((Key + NoteOffset) mod 12) in [noteA, noteB, noteC, noteD, noteE, noteF, noteG];
end;

function TPianoKeyboard.KeyFromPoint(X, Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FKeys) to High(FKeys) do
    if (not IsNaturalKey(I)) and PointInRect(KeyToRect(I), X, Y) then
      Exit(I);
  for I := Low(FKeys) to High(FKeys) do
    if IsNaturalKey(I) and PointInRect(KeyToRect(I), X, Y) then
      Exit(I);
end;

function TPianoKeyboard.KeyToRect(Index: Integer): TRect;
var
  X, Y: Integer;
begin
  Result := InternalKeyToRect(Index);
  if Index < Low(FKeys) then
    Exit;
  if Index > High(FKeys) then
    Exit;
  X := (Width - FCalculatedSize.X) div 2;
  Y := (Height - FCalculatedSize.Y) div 2;
  if FMargin.Right > 0 then
    if X + FCalculatedSize.X > Width - FMargin.Right then
      X := Width - FMargin.Right - FCalculatedSize.X;
  if FMargin.Bottom > 0 then
    if Y + FCalculatedSize.Y > Height - FMargin.Bottom then
      Y := Height - FMargin.Bottom - FCalculatedSize.Y;
  if FMargin.Left > 0 then
    if X < FMargin.Left then
      X := FMargin.Left;
  if FMargin.Top > 0 then
    if Y < FMargin.Top then
      Y := FMargin.Top;
  OffsetRect(Result, X, Y);
end;

function TPianoKeyboard.KeyToFrequency(Index: Integer): Double;
begin
  if (Index < Low(FKeys)) or (Index > High(FKeys)) then
    Result := 0
  else
    Result := MusicNote(KeyToNote(Index));
end;

function TPianoKeyboard.KeyToNote(Index: Integer): Integer;
var
  I: Integer;
begin
  Result := Index + NoteOffset;
  I := Length(FKeys);
  Result := Result - (I div 24) * 12;
end;

function TPianoKeyboard.KeyToString(Index: Integer): string;
begin
  if (Index < Low(FKeys)) or (Index > High(FKeys)) then
    Result := ''
  else
    Result := MusicNoteName(KeyToNote(Index));
end;

procedure TPianoKeyboard.SetMargin(const Rect: TRect);
begin
  FMargin := Rect;
  Invalidate;
end;

procedure TPianoKeyboard.NeedImages;
var
  Size: TPoint;
begin
  Size := FNaturalSize;
  if FNatural = nil then
    FNatural := ResampleBitmap(NaturalShared, Size.X, Size.Y);
  if FNaturalDown = nil then
    FNaturalDown := ResampleBitmap(NaturalDownShared, Size.X, Size.Y);
  Size := FSharpSize;
  if FSharp = nil then
    FSharp := ResampleBitmap(SharpShared, Size.X, Size.Y);
  if FSharpDown = nil then
    FSharpDown := ResampleBitmap(SharpDownShared, Size.X, Size.Y);
end;

procedure TPianoKeyboard.Paint;
var
  R: TRect;
  I: Integer;
begin
  NeedImages;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  for I := Low(FKeys) to High(FKeys) do
    if IsNaturalKey(I) then
    begin
      R := KeyToRect(I);
      if FKeys[I] then
        Canvas.Draw(R.Left, R.Top, FNaturalDown)
      else
        Canvas.Draw(R.Left, R.Top, FNatural);
    end;
  for I := Low(FKeys) to High(FKeys) do
    if not IsNaturalKey(I) then
    begin
      R := KeyToRect(I);
      if FKeys[I] then
        Canvas.Draw(R.Left, R.Top, FSharpDown)
      else
        Canvas.Draw(R.Left, R.Top, FSharp);
    end;
end;

procedure TPianoKeyboard.DoKeyToggle(Key: Integer; Down: Boolean);
begin
  if Assigned(FOnKeyToggle) then
    FOnKeyToggle(Self, Key, Down);
end;

function TPianoKeyboard.GetKey(Index: Integer): Boolean;
begin
  Result := False;
  if Index < 0 then
    Exit;
  if Index > High(FKeys) then
    Exit;
  Result := FKeys[Index];
end;

procedure TPianoKeyboard.SetKey(Index: Integer; Value: Boolean);
begin
  if Index < 0 then
    Exit;
  if Index > High(FKeys) then
    Exit;
  if FKeys[Index] <> Value then
  begin
    FKeys[Index] := Value;
    DoKeyToggle(Index, Value);
    Invalidate;
  end;
end;

procedure TPianoKeyboard.SetKeyCount(Value: Integer);
var
  Changed: Boolean;
  I: Integer;
begin
  if Value < 0 then
    Value := 1;
  for I := Low(FKeys) to High(FKeys) do
    Key[I] := False;
  Changed := Value <> Length(FKeys);
  if Changed then
  begin
    SetLength(FKeys, Value);
    RecalculateSize;
  end;
  for I := Low(FKeys) to High(FKeys) do
    FKeys[I] := False;
  Invalidate;
end;

function TPianoKeyboard.GetKeyCount: Integer;
begin
  Result := Length(FKeys)
end;

procedure TPianoKeyboard.SetScaleFactor(Value: Double);
var
  Size: TPoint;
begin
  if Value < 0.1 then
    Value := 0.1
  else if Value > 2 then
    Value := 2;
  if FScaleFactor <> Value then
  begin
    FScaleFactor := Value;
    Size.X := Round(NaturalShared.Width * FScaleFactor / 2);
    Size.Y := Round(NaturalShared.Height * FScaleFactor / 2);
    if (FNaturalSize.X <> Size.X) or (FNaturalSize.Y <> Size.Y) then
    begin
      FNaturalSize := Size;
      FreeAndNil(FNatural);
      FreeAndNil(FNaturalDown);
    end;
    Size.X := Round(SharpShared.Width * FScaleFactor / 2);
    Size.Y := Round(SharpShared.Height * FScaleFactor / 2);
    if (FSharpSize.X <> Size.X) or (FSharpSize.Y <> Size.Y) then
    begin
      FSharpSize := Size;
      FreeAndNil(FSharp);
      FreeAndNil(FSharpDown);
    end;
    RecalculateSize;
    Invalidate;
  end;
end;

initialization
  NaturalShared := nil;
  NaturalDownShared := nil;
  SharpShared := nil;
  SharpDownShared := nil;
finalization
  NaturalShared.Free;
  NaturalDownShared.Free;
  SharpShared.Free;
  SharpDownShared.Free;
end.

