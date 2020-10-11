(**
 * Simple ASCII art images and diagrams
 *
 * (c) 2020 by Johann Glaser
 *)
Unit AsciiDiagram;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, PasGpibUtils, Math;

Type

  { TAsciiImage }

  TAsciiImage = class
    FWidth  : Integer;
    FHeight : Integer;
    FScreen : TDynStringArray;
    Constructor Create(AWidth, AHeight : Integer);
    Procedure Clear;
    Procedure Print;
    // all coordinates are 0,0 based at bottom left
    Procedure Put(X,Y:Integer;Ch:Char);
    Function  Get(X,Y:Integer):Char;
    Procedure LineHoriz(X1,X2,Y:Integer;Ch:Char);
    Procedure LineVert (X,Y1,Y2:Integer;Ch:Char);
    Procedure WriteLeft  (X, Y : Integer; St : String);  // don't use UTF-8 chars, otherwise it will mess up the screen
    Procedure WriteCenter(X, Y : Integer; St : String);
    Procedure WriteRight (X, Y : Integer; St : String);
  End;

  { TAsciiImageReal }

  TAsciiImageReal = class(TAsciiImage)
    FXScale  : Double;
    FXOffset : Double;
    FYScale  : Double;
    FYOffset : Double;
    Constructor Create(AWidth, AHeight : Integer; AXScale, AXOffset, AYScale, AYOffset : Double);
    Function  XReal2Int(X:Double)  : Integer;
    Function  XInt2Real(X:Integer) : Double;
    Function  YReal2Int(Y:Double)  : Integer;
    Function  YInt2Real(Y:Integer) : Double;
    Procedure Put(X,Y:Double;Ch:Char); overload;
    Function  Get(X,Y:Double):Char; overload;
    Procedure LineHoriz(X1,X2,Y:Double;Ch:Char); overload;
    Procedure LineVert (X,Y1,Y2:Double;Ch:Char); overload;
    Procedure WriteLeft  (X, Y : Double; St : String); overload;
    Procedure WriteCenter(X, Y : Double; St : String); overload;
    Procedure WriteRight (X, Y : Double; St : String); overload;
  End;

  { TAsciiDiagram }

  TAsciiDiagram = class(TAsciiImageReal)
    FXOrigin : Integer;
    FYOrigin : Integer;
    FXMin    : Double;
    FXMax    : Double;
    FYMin    : Double;
    FYMax    : Double;
    Constructor Create(AWidth, AHeight, AXOrigin, AYOrigin : Integer; AXMin, AXMax, AYMin, AYMax : Double);
    Constructor Create(AWidth, AHeight, AXOrigin, AYOrigin : Integer; AXData, AYData : TDynDoubleArray; AXDiv, AYDiv : Double);
    Procedure DrawAxes;
    Procedure DrawXTics(ATickDist:Double);
    Procedure DrawYTics(ATickDist:Double);
  End;

Implementation

{ TAsciiImage }

Constructor TAsciiImage.Create(AWidth, AHeight : Integer);
Var I : Integer;
Begin
  inherited Create;
  FWidth  := AWidth;
  FHeight := AHeight;
  // prepare empty screen
  SetLength(FScreen, FHeight);
  For I := 0 to FHeight-1 do
    Begin
      SetLength(FScreen[I], FWidth);
      FillChar(FScreen[I][1], FWidth, ' ');
    End;
End;

Procedure TAsciiImage.Clear;
Var I : Integer;
Begin
  For I := 0 to FHeight-1 do
    FillChar(FScreen[I][1], FWidth, ' ');
End;

Procedure TAsciiImage.Print;
Var I : Integer;
Begin
  For I := 0 to FHeight-1 do
    WriteLn(FScreen[FHeight-1-I]);
End;

Procedure TAsciiImage.Put(X, Y : Integer; Ch : Char);
Begin
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then Exit;
  FScreen[Y][X+1] := Ch;
End;

Function TAsciiImage.Get(X, Y : Integer) : Char;
Begin
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then Exit(#0);
  Result := FScreen[Y][X+1];
End;

Procedure TAsciiImage.LineHoriz(X1, X2, Y : Integer; Ch : Char);
Begin
  if (X1 < 0) and (X2 < 0) then Exit;
  if (X1 >= FWidth) and (X2 >= FWidth) then Exit;
  if (Y < 0) or (Y >= FHeight) then Exit;
  X1 := Max(0, X1);  X1 := Min(X1, FWidth-1);
  X2 := Max(0, X2);  X2 := Min(X2, FWidth-1);
  FillChar(FScreen[Y][X1+1], X2-X1+1, Ch);
End;

Procedure TAsciiImage.LineVert(X, Y1, Y2 : Integer; Ch : Char);
Var Y : Integer;
Begin
  if (X < 0) or (X >= FWidth) then Exit;
  if (Y1 < 0) and (Y2 < 0) then Exit;
  if (Y1 >= FHeight) and (Y2 >= FHeight) then Exit;
  Y1 := Max(0, Y1);  Y1 := Min(Y1, FHeight-1);
  Y2 := Max(0, Y2);  Y2 := Min(Y2, FHeight-1);
  For Y := Y1 to Y2 do
    FScreen[Y][X+1] := Ch;
End;

Procedure TAsciiImage.WriteLeft(X, Y : Integer; St : String);
Begin
  if (X+Length(St) < 0) or (X >= FWidth) then Exit;
  if (Y < 0) or (Y >= FHeight) then Exit;
  if X < 0 then
    Begin
      St := Copy(St, -X+1, Length(St));
      X := 0;
    End;
  if X+Length(St) > FWidth then
    SetLength(St, FWidth-X);
  Move(St[1], FScreen[Y][X+1], Length(St));
End;

Procedure TAsciiImage.WriteCenter(X, Y : Integer; St : String);
Begin
  WriteLeft(X - (Length(St) shr 1), Y, St);
End;

Procedure TAsciiImage.WriteRight(X, Y : Integer; St : String);
Begin
  WriteLeft(X - Length(St) + 1, Y, St);
End;

{ TAsciiImageReal }

Constructor TAsciiImageReal.Create(AWidth, AHeight : Integer; AXScale, AXOffset, AYScale, AYOffset : Double);
Begin
  inherited Create(AWidth, AHeight);
  FXScale  := AXScale;
  FXOffset := AXOffset;
  FYScale  := AYScale;
  FYOffset := AYOffset;
End;

Function TAsciiImageReal.XReal2Int(X : Double) : Integer;
Begin
  Result := Round(X*FXScale + FXOffset);
End;

Function TAsciiImageReal.XInt2Real(X : Integer) : Double;
Begin
  Result := (X-FXOffset)/FXScale;
End;

Function TAsciiImageReal.YReal2Int(Y : Double) : Integer;
Begin
  Result := Round(Y*FYScale + FYOffset);
End;

Function TAsciiImageReal.YInt2Real(Y : Integer) : Double;
Begin
  Result := (Y-FYOffset)/FYScale;
End;

Procedure TAsciiImageReal.Put(X, Y : Double; Ch : Char);
Begin
  inherited Put(XReal2Int(X), YReal2Int(Y), Ch);
End;

Function TAsciiImageReal.Get(X, Y : Double) : Char;
Begin
  Result := Get(XReal2Int(X), YReal2Int(Y));
End;

Procedure TAsciiImageReal.LineHoriz(X1, X2, Y : Double; Ch : Char);
Begin
  inherited LineHoriz(XReal2Int(X1), XReal2Int(X2), YReal2Int(Y), Ch);
End;

Procedure TAsciiImageReal.LineVert(X, Y1, Y2 : Double; Ch : Char);
Begin
  inherited LineVert(XReal2Int(X), YReal2Int(Y1), YReal2Int(Y2), Ch);
End;

Procedure TAsciiImageReal.WriteLeft(X, Y : Double; St : String);
Begin
  inherited WriteLeft(XReal2Int(X), YReal2Int(Y), St);
End;

Procedure TAsciiImageReal.WriteCenter(X, Y : Double; St : String);
Begin
  inherited WriteCenter(XReal2Int(X), YReal2Int(Y), St);
End;

Procedure TAsciiImageReal.WriteRight(X, Y : Double; St : String);
Begin
  inherited WriteRight(XReal2Int(X), YReal2Int(Y), St);
End;

{ TAsciiDiagram }

Constructor TAsciiDiagram.Create(AWidth, AHeight, AXOrigin, AYOrigin : Integer; AXMin, AXMax, AYMin, AYMax : Double);
Var XScale, XOffset, YScale, YOffset : Double;
Begin
  // X = AXMin (in real) is at AXOrigin (in integer), and analogous for Y
  XScale  := (AWidth-1- AXOrigin)/(AXMax-AXMin);
  YScale  := (AHeight-1-AYOrigin)/(AYMax-AYMin);
  XOffset := AXOrigin*1.0 - AXMin*XScale;
  YOffset := AYOrigin*1.0 - AYMin*YScale;
  inherited Create(AWidth, AHeight, XScale, XOffset, YScale, YOffset);
  FXOrigin := AXOrigin;
  FYOrigin := AYOrigin;
  FXMin    := AXMin;
  FXMax    := AXMax;
  FYMin    := AYMin;
  FYMax    := AYMax;
End;

Constructor TAsciiDiagram.Create(AWidth, AHeight, AXOrigin, AYOrigin : Integer; AXData, AYData : TDynDoubleArray; AXDiv,AYDiv:Double);
Var Min,Max : Double;
    I       : Integer;
Begin
  Min := MinValue(AYData);
  Max := MaxValue(AYData);
  Min := Min - (Max-Min)*0.05;
  Max := Max + (Max-Min)*0.05;
  Create(AWidth, AHeight, AXOrigin, AYOrigin, AXData[0], AXData[Length(AXData)-1]*1.02, Min, Max);
  DrawAxes;
  DrawXTics(AXDiv);
  DrawYTics(AYDiv);
  For I := 0 to Length(AXData)-1 do
    Put(AXData[I], AYData[I], '*');
End;

Procedure TAsciiDiagram.DrawAxes;
Begin
  // using integer methods
  LineHoriz(FXOrigin, FWidth-1, FYOrigin, '-');
  Put(FWidth-1, FYOrigin, '>');
  LineVert(FXOrigin, FYOrigin, FHeight-1, '|');
  Put(FXOrigin, FHeight-1, '^');
  Put(FXOrigin, FYOrigin, '+');
End;

Procedure TAsciiDiagram.DrawXTics(ATickDist : Double);
Var X  : Double;
    I  : Integer;
    XI : Integer;
Begin
  I := Round(ceil(FXMin/ATickDist));
  X := I * ATickDist;
  while X < FXMax do
    Begin
      XI := XReal2Int(X);
      Put(XI, FYOrigin, '+');
      WriteCenter(XI, FYOrigin-1, FloatToStrSI(X, FormatSettings, False));  // use non-unicode pure ascii version, otherwise it will mess up FScreen
      Inc(I);
      X := I * ATickDist;
    End;
End;

Procedure TAsciiDiagram.DrawYTics(ATickDist : Double);
Var Y  : Double;
    I  : Integer;
    YI : Integer;
Begin
  I := Round(ceil(FYMin/ATickDist));
  Y := I * ATickDist;
  while Y < FYMax do
    Begin
      YI := YReal2Int(Y);
      Put(FXOrigin, YI, '+');
      WriteRight(FXOrigin-2 , YI, FloatToStrSI(Y, FormatSettings));
      Inc(I);
      Y := I * ATickDist;
    End;
End;

End.

