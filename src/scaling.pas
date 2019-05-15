unit Scaling;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Math;

type
  TPixel = record Blue, Green, Red, Alpha: Byte; end;
  PPixel = ^TPixel;

  TFilterFunction = function(Value: Single): Single;

  TSamplingFilter = (sfNearest, sfLinear, sfCosine, sfHermite, sfQuadratic,
    sfGaussian, sfSpline, sfLanczos, sfMitchell, sfCatmullRom);

{ Default resampling filter used for bicubic resizing }

const
  DefaultCubicFilter = sfCatmullRom;

{ Built-in filter functions }

var
  SamplingFilterFunctions : array[TSamplingFilter] of TFilterFunction;

{ Default radii of built-in filter functions }

  SamplingFilterRadii: array[TSamplingFilter] of Single;

{ Resamples rectangle in source image to rectangle in destination image
  with resampling. You can use custom sampling function and filter radius.

  Set WrapEdges to True for seamlessly tileable images }

procedure ResampleBitmap(Src: TRasterImage; SrcX, SrcY, SrcWidth, SrcHeight: LongInt;
  Dst: TRasterImage; DstX, DstY, DstWidth, DstHeight: LongInt;
  Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean); overload;
function ResampleBitmap(Bitmap: TRasterImage; Width, Height: Integer;
  Filter: TSamplingFilter = DefaultCubicFilter; WrapEdges:
  Boolean = False): TRasterImage; overload;

{ Perform a gaussian blur on a bitmap }

implementation

function NewBitmap(W, H: Integer): TRasterImage;
begin
  Result := TPortableNetworkGraphic.Create;
  Result.Width := W;
  Result.Height := H;
  Result.PixelFormat := pf32bit;
end;

{ Type of custom sampling function}

type
  TPointRec = record
    Pos: LongInt;
    Weight: Single;
  end;

  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;

var
  FullEdge: Boolean = True;

function ClampInt(Number: LongInt; Min, Max: LongInt): LongInt;
begin
  Result := Number;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

{ The following resampling code is modified and extended code from Graphics32
  library by Alex A. Denisov }

function BuildMappingTable(DstLow, DstHigh, SrcLow, SrcHigh, SrcImageWidth: LongInt;
  Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean): TMappingTable;
var
  I, J, K, N: LongInt;
  Left, Right, SrcWidth, DstWidth: LongInt;
  Weight, Scale, Center, Count: Single;
begin
  Result := nil;
  K := 0;
  SrcWidth := SrcHigh - SrcLow;
  DstWidth := DstHigh - DstLow;
  if SrcWidth = 1 then
  begin
    SetLength(Result, DstWidth);
    for I := 0 to DstWidth - 1 do
    begin
      SetLength(Result[I], 1);
      Result[I][0].Pos := 0;
      Result[I][0].Weight := 1.0;
    end;
    Exit;
  end
  else if (SrcWidth = 0) or (DstWidth = 0) then
    Exit;
  if FullEdge then
    Scale := DstWidth / SrcWidth
  else
    Scale := (DstWidth - 1) / (SrcWidth - 1);
  SetLength(Result, DstWidth);
  if Scale = 0.0 then
  begin
    Assert(Length(Result) = 1);
    SetLength(Result[0], 1);
    Result[0][0].Pos := (SrcLow + SrcHigh) div 2;
    Result[0][0].Weight := 1.0;
  end
  else if Scale < 1.0 then
  begin
    Radius := Radius / Scale;
    for I := 0 to DstWidth - 1 do
    begin
      if FullEdge then
        Center := SrcLow - 0.5 + (I + 0.5) / Scale
      else
        Center := SrcLow + I / Scale;
      Left := Round(Floor(Center - Radius));
      Right := Round(Ceil(Center + Radius));
      Count := -1.0;
      for J := Left to Right do
      begin
        Weight := Filter((Center - J) * Scale) * Scale;
        if Weight <> 0.0 then
        begin
          Count := Count + Weight;
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          Result[I][K].Pos := ClampInt(J, SrcLow, SrcHigh - 1);
          Result[I][K].Weight := Weight;
        end;
      end;
      if Length(Result[I]) = 0 then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := Round(Floor(Center));
        Result[I][0].Weight := 1.0;
      end
      else if Count <> 0.0 then
        Result[I][K div 2].Weight := Result[I][K div 2].Weight - Count;
    end;
  end
  else // if Scale > 1.0 then
  begin
    // Super-sampling - scales from smaller to bigger
    Scale := 1.0 / Scale;
    for I := 0 to DstWidth - 1 do
    begin
      if FullEdge then
        Center := SrcLow - 0.5 + (I + 0.5) * Scale
      else
        Center := SrcLow + I * Scale;
      Left := Round(Floor(Center - Radius));
      Right := Round(Ceil(Center + Radius));
      Count := -1.0;
      for J := Left to Right do
      begin
        Weight := Filter(Center - J);
        if Weight <> 0.0 then
        begin
          Count := Count + Weight;
          K := Length(Result[I]);
          SetLength(Result[I], K + 1);
          if WrapEdges then
          begin
            if J < 0 then
              N := SrcImageWidth + J
            else if J >= SrcImageWidth then
              N := J - SrcImageWidth
            else
              N := ClampInt(J, SrcLow, SrcHigh - 1);
          end
          else
            N := ClampInt(J, SrcLow, SrcHigh - 1);
          Result[I][K].Pos := N;
          Result[I][K].Weight := Weight;
        end;
      end;
      if Count <> 0.0 then
        Result[I][K div 2].Weight := Result[I][K div 2].Weight - Count;
    end;
  end;
end;

procedure FindExtremes(const Map: TMappingTable; out  MinPos, MaxPos: LongInt);
var
  I, J: LongInt;
begin
  MinPos := 0;
  MaxPos := 0;
  if Length(Map) > 0 then
  begin
    MinPos := Map[0][0].Pos;
    MaxPos := MinPos;
    for I := 0 to Length(Map) - 1 do
      for J := 0 to Length(Map[I]) - 1 do
      begin
        if MinPos > Map[I][J].Pos then
          MinPos := Map[I][J].Pos;
        if MaxPos < Map[I][J].Pos then
          MaxPos := Map[I][J].Pos;
      end;
  end;
end;

{ Filter function for nearest filtering. Also known as box filter }

function FilterNearest(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1
  else
    Result := 0;
end;

{ Filter function for linear filtering. Also known as triangle or Bartlett filter }

function FilterLinear(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

{ Cosine filter }

function FilterCosine(Value: Single): Single;
begin
  Result := 0;
  if Abs(Value) < 1 then
    Result := (Cos(Value * Pi) + 1) / 2;
end;

{ Hermite filter }

function FilterHermite(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1 then
    Result := (2 * Value - 3) * Sqr(Value) + 1
  else
    Result := 0;
end;

{ Quadratic filter. Also known as Bell }

function FilterQuadratic(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 0.5 then
    Result := 0.75 - Sqr(Value)
  else
  if Value < 1.5 then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

{ Gaussian filter }

function FilterGaussian(Value: Single): Single;
begin
  Result := Exp(-2.0 * Sqr(Value)) * Sqrt(2.0 / Pi);
end;

{ 4th order (cubic) b-spline filter }

function FilterSpline(Value: Single): Single;
var
  Temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
  begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2.0 / 3.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := 2.0 - Value;
    Result := Sqr(Value) * Value / 6.0;
  end
  else
    Result := 0.0;
end;

{ Lanczos-windowed sinc filter }

function FilterLanczos(Value: Single): Single;

  function SinC(Value: Single): Single;
  begin
    if Value <> 0.0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end
    else
      Result := 1.0;
  end;

begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 3.0 then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

{ Micthell cubic filter }

function FilterMitchell(Value: Single): Single;
const
  B = 1.0 / 3.0;
  C = 1.0 / 3.0;
var
  Temp: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  Temp := Sqr(Value);
  if Value < 1.0 then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * Temp)) +
      ((-18.0 + 12.0 * B + 6.0 * C) * Temp) +
      (6.0 - 2.0 * B));
    Result := Value / 6.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := (((-B - 6.0 * C) * (Value * Temp)) +
      ((6.0 * B + 30.0 * C) * Temp) +
      ((-12.0 * B - 48.0 * C) * Value) +
      (8.0 * B + 24.0 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

{ CatmullRom spline filter }

function FilterCatmullRom(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 0.5 * (2.0 + Sqr(Value) * (-5.0 + 3.0 * Value))
  else
  if Value < 2.0 then
    Result := 0.5 * (4.0 + Value * (-8.0 + Value * (5.0 - Value)))
  else
    Result := 0.0;
end;

var
  Init: Boolean;

procedure InitResample;
begin
  if Init then
    Exit;
  Init := True;
  SamplingFilterFunctions[sfNearest] := FilterNearest;
  SamplingFilterFunctions[sfLinear] := FilterLinear;
  SamplingFilterFunctions[sfCosine] := FilterCosine;
  SamplingFilterFunctions[sfHermite] := FilterHermite;
  SamplingFilterFunctions[sfQuadratic] := FilterQuadratic;
  SamplingFilterFunctions[sfGaussian] := FilterGaussian;
  SamplingFilterFunctions[sfSpline] := FilterSpline;
  SamplingFilterFunctions[sfLanczos] := FilterLanczos;
  SamplingFilterFunctions[sfMitchell] := FilterMitchell;
  SamplingFilterFunctions[sfCatmullRom] := FilterCatmullRom;
  SamplingFilterRadii[sfNearest] := 1.0;
  SamplingFilterRadii[sfLinear] := 1.0;
  SamplingFilterRadii[sfCosine] := 1.0;
  SamplingFilterRadii[sfHermite] := 1.0;
  SamplingFilterRadii[sfQuadratic] := 1.5;
  SamplingFilterRadii[sfGaussian] := 1.25;
  SamplingFilterRadii[sfSpline] := 2.0;
  SamplingFilterRadii[sfLanczos] := 3.0;
  SamplingFilterRadii[sfMitchell] := 2.0;
  SamplingFilterRadii[sfCatmullRom] := 2.0;
end;

procedure ResampleBitmap(Src: TRasterImage; SrcX, SrcY, SrcWidth, SrcHeight: LongInt;
  Dst: TRasterImage; DstX, DstY, DstWidth, DstHeight: LongInt;
  Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean);
type
  TBufferItem = record
    A, R, G, B: Integer;
  end;
  TByteArray = array[0..High(LongWord) div 4] of Byte;
  PByteArray = ^TByteArray;

var
  MapX, MapY: TMappingTable;
  MinX, MaxX: Integer;
  LineBufferInt: array of TBufferItem;
  ClusterX, ClusterY: TCluster;
  Speed, Weight, AccumA, AccumR, AccumG, AccumB: Integer;
  SrcColor: TPixel;
  Pixels: PPixel;
  SrcPixels: array of PByteArray;
  DstPixels: array of PByteArray;
  I, J, X, Y: Integer;
begin
  InitResample;
  if (Src.Width < 2) or (Src.Height < 2) or (Dst.Width < 2) or (Dst.Height < 2) then Exit;
  MapX := BuildMappingTable(DstX, DstX + DstWidth , SrcX, SrcX + SrcWidth , Src.Width , Filter, Radius, WrapEdges);
  MapY := BuildMappingTable(DstY, DstY + DstHeight, SrcY, SrcY + SrcHeight, Src.Height, Filter, Radius, WrapEdges);
  ClusterX := nil;
  ClusterY := nil;
  SetLength(SrcPixels, Src.Height);
  Pixels := Src.{%H-}Scanline[0];
  for I := 0 to Src.Height - 1 do
  begin
    SrcPixels[I] := PByteArray(Pixels);
    Inc(Pixels, Src.Width);
  end;
  SetLength(DstPixels, Dst.Height);
  Pixels := Dst.{%H-}ScanLine[0];
  for I := 0 to Dst.Height - 1 do
  begin
    DstPixels[I] := PByteArray(Pixels);
    Inc(Pixels, Dst.Width);
  end;
  FindExtremes(MapX, MinX, MaxX);
  SetLength(LineBufferInt, MaxX - MinX + 1);
  for J := 0 to DstHeight - 1 do
  begin
    ClusterY := MapY[J];
    for X := MinX to MaxX do
    begin
      AccumA := 0;
      AccumR := 0;
      AccumG := 0;
      AccumB := 0;
      for Y := 0 to Length(ClusterY) - 1 do
      begin
        Weight := Round(256 * ClusterY[Y].Weight);
        Speed := X * 4;
        AccumB := AccumB + SrcPixels[ClusterY[Y].Pos][Speed] * Weight;
        AccumG := AccumG + SrcPixels[ClusterY[Y].Pos][Speed + 1] * Weight;
        AccumR := AccumR + SrcPixels[ClusterY[Y].Pos][Speed + 2] * Weight;
        AccumA := AccumA + SrcPixels[ClusterY[Y].Pos][Speed + 3] * Weight;
      end;
      with LineBufferInt[X - MinX] do
      begin
        A := AccumA;
        R := AccumR;
        G := AccumG;
        B := AccumB;
      end;
    end;
    for I := 0 to DstWidth - 1 do
    begin
      ClusterX := MapX[I];
      AccumA := 0;
      AccumR := 0;
      AccumG := 0;
      AccumB := 0;
      for X := 0 to Length(ClusterX) - 1 do
      begin
        Weight := Round(256 * ClusterX[X].Weight);
        with LineBufferInt[ClusterX[X].Pos - MinX] do
        begin
          AccumB := AccumB + B * Weight;
          AccumG := AccumG + G * Weight;
          AccumR := AccumR + R * Weight;
          AccumA := AccumA + A * Weight;
        end;
      end;
      SrcColor.Blue := ClampInt(AccumB, 0, $00FF0000) shr 16;
      SrcColor.Green := ClampInt(AccumG, 0, $00FF0000) shr 16;
      SrcColor.Red := ClampInt(AccumR, 0, $00FF0000) shr 16;
      SrcColor.Alpha := ClampInt(AccumA, 0, $00FF0000) shr 16;
      PLongWord(@DstPixels[J]^[(I + DstX) * 4])^ := PLongWord(@SrcColor)^;
    end;
  end;
end;

function ResampleBitmap(Bitmap: TRasterImage; Width, Height: Integer;
  Filter: TSamplingFilter = DefaultCubicFilter; WrapEdges: Boolean = False): TRasterImage;

  function IsEmpty(B: TRasterImage): Boolean;
  begin
    Result := (B.Width < 1) or (B.Height < 1);
  end;

begin
  InitResample;
  Result := NewBitmap(Width, Height);
  if IsEmpty(Bitmap) or IsEmpty(Result) then
    Exit;
  ResampleBitmap(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height,
    Result, 0, 0, Result.Width, Result.Height,
    SamplingFilterFunctions[Filter], SamplingFilterRadii[Filter], WrapEdges);
end;

end.

