Unit VectorialDiagram;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Math,
  FPImage, FPCanvas, FPVectorial;

Type

(*
  "Value"   : data value
  "Drawing" : drawing coordinate
*)

  { TCoordBase }

  TCoordBase = class
    FDrwWidth     : Double;
    FDrwHeight    : Double;
    Constructor Create;
    Function ValX2Drw(Val:Double):Double; virtual; abstract;
    Function ValY2Drw(Val:Double):Double; virtual; abstract;
    Function DrwX2Val(Drw:Double):Double; virtual; abstract;
    Function DrwY2Val(Drw:Double):Double; virtual; abstract;
  End;

  { TLinearCoord }

  TLinearCoord = class(TCoordBase)
    FValXMin : Double;
    FValXMax : Double;
    FValYMin : Double;
    FValYMax : Double;
    Constructor Create;
    Function ValX2Drw(Val:Double):Double; override;
    Function ValY2Drw(Val:Double):Double; override;
    Function DrwX2Val(Drw:Double):Double; override;
    Function DrwY2Val(Drw:Double):Double; override;
  End;

  { TBipolarSemiLogXCoord }

  TBipolarSemiLogXCoord = class(TCoordBase)
    FValXNegMax   : Double;   // use absolute value
    FValXNegMin   : Double;   // use absolute value
    FValXPosMin   : Double;
    FValXPosMax   : Double;
    FValXWithZero : Boolean;
    FValYMin      : Double;
    FValYMax      : Double;
    FDrwZeroWidth : Double;  // width in drawing between FValXPosMin/FValXNegMin and the zero value
    Constructor Create;
    Function ValX2Drw(Val:Double):Double; override;
    Function ValY2Drw(Val:Double):Double; override;
    Function DrwX2Val(Drw:Double):Double; override;
    Function DrwY2Val(Drw:Double):Double; override;
  End;

  TRectDouble = record
    Left,Top,Right,Bottom : Double;
  End;

  { TVectorialDiagram }

  TVectorialDiagram = class
    FCoord   : TCoordBase;
    FVecDoc  : TvVectorialDocument;
    FVecPage : TvVectorialPage;
    FDiagBox : TRectDouble;
    FWidth   : Double;
    FHeight  : Double;
    FBoxPenColor    : TFPColor;
    FBoxPenWidth    : Integer;
    FXGridPenColor  : TFPColor;
    FXGridPenWidth  : Integer;
    FXTickPenColor  : TFPColor;
    FXTickPenWidth  : Integer;
    FXTickFontName  : String;
    FXTickFontSize  : Double;
    FTickLenDrw     : Double;    // extension above and below axis in drawing units
    FXBreakPenColor : TFPColor;
    FXBreakPenWidth : Integer;
    FXBreakExtDrw   : Double;    // extension above and below axis
    FXBreakSlantDrw : Double;    // how much slanted (actually half of that)
    FXBreakSpaceDrw : Double;    // space between the two lines (actually half of that space)

    Constructor Create(ACoord : TCoordBase);
    Destructor Destroy; override;
    Procedure Resize(AWidth, AHeight:Double);
    Procedure Resize(AWidth, AHeight:Double;ACanvas:TFPCustomCanvas);
    // drawing coordinate functions
    Function  Line(X1,Y1,X2,Y2:Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer) : TPath;
    Function  Rectangle(Left, Bottom, Right, Top : Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer; BrushColor : TFPColor; BrushStyle : TFPBrushStyle) : TPath;
    Function  Rectangle(Left, Bottom, Right, Top : Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer) : TPath;
    Function  EstimateTextWidth(T : TvText) : Double;
    Procedure CenterText(AT : Array of TvText);
    Procedure SymPlus(X, Y, Size : Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
    // diagram functions
    Function  IsVisible(ValX,ValY:Double):Boolean;
    Procedure DrawBox;
    Procedure DrawLinearXAxis;
    Procedure DrawSemiLogXAxis;
    Procedure DrawYAxis;
    Procedure DrawRect(ValXMin,ValXMax,ValYMin,ValYMax:Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer; BrushColor : TFPColor; BrushStyle : TFPBrushStyle);
    Procedure DrawRect(ValXMin,ValXMax,ValYMin,ValYMax:Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer);
    Procedure DrawSymPlus(ValX, ValY, DrwSize : Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
    Procedure DrawRange(ValXMin,ValXMax,ValY,ValWidth:Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer; BrushColor : TFPColor; BrushStyle : TFPBrushStyle);
    Procedure DrawAxes;
    // output functions
    Procedure Paint(ACanvas:TFPCustomCanvas);
    Procedure WriteSVG(AFilename:String);
  End;

Implementation

{ TCoordBase }

Constructor TCoordBase.Create;
Begin
  inherited Create;
End;

{ TLinearCoord }

Constructor TLinearCoord.Create;
Begin
  inherited Create;
End;

Function TLinearCoord.ValX2Drw(Val : Double) : Double;
Begin
  Result := (Val-FValXMin) / (FValXMax-FValXMin) * FDrwWidth;
End;

Function TLinearCoord.ValY2Drw(Val : Double) : Double;
Begin
  Result := (Val-FValYMin) / (FValYMax-FValYMin) * FDrwHeight;
End;

Function TLinearCoord.DrwX2Val(Drw : Double) : Double;
Begin
  Result := Drw/FDrwWidth * (FValXMax-FValXMin) + FValXMin;
End;

Function TLinearCoord.DrwY2Val(Drw : Double) : Double;
Begin
  Result := Drw/FDrwHeight * (FValYMax-FValYMin) + FValYMin;
End;

{ TBipolarSemiLogXCoord }

Constructor TBipolarSemiLogXCoord.Create;
Begin
  inherited Create;
End;

Function TBipolarSemiLogXCoord.ValX2Drw(Val : Double) : Double;
Var LogScale : Double;
Begin
  if Val > FValXPosMax then WriteLn('Warning: Value ',Val,' is larger than the largest value ',FValXPosMax);
  if (FValXNegMax = FValXNegMin) and not FValXWithZero then
    Begin
      if Val < FValXPosMin then Begin WriteLn('Warning: Value ',Val,' is smaller than the smallest value ',FValXPosMin); Val := FValXPosMin; End;
      // only positive values
      Result := (ln(Val) - ln(FValXPosMin)) / (ln(FValXPosMax)-ln(FValXPosMin)) * FDrwWidth;
    End
  else if (FValXNegMax = FValXNegMin) and FValXWithZero then
    Begin
      if (Val <> 0.0) and (Val < FValXPosMin) then Begin WriteLn('Warning: Value ',Val,' is smaller than the smallest value ',FValXPosMin); Val := FValXPosMin; End;
      // zero and positive values
      if Val = 0.0 then Result := 0.0
      else Result := FDrwZeroWidth + (ln(Val) - ln(FValXPosMin)) / (ln(FValXPosMax)-ln(FValXPosMin)) * (FDrwWidth - FDrwZeroWidth);
    End
  else
    Begin
      if (Val < 0.0) and (abs(Val) > FValXNegMax) then       WriteLn('Warning: Value ',Val,' is larger than the largest negative value ',FValXNegMax);
      if (Val < 0.0) and (abs(Val) < FValXNegMin) then Begin WriteLn('Warning: Value ',Val,' is smaller than the smallest negative value ',FValXNegMin); Val := -FValXNegMin; End;
      if (Val > 0.0) and (    Val  < FValXPosMin) then Begin WriteLn('Warning: Value ',Val,' is smaller than the smallest positive value ',FValXNegMax); Val :=  FValXPosMin; End;
      // negative and positive values incl. zero
      LogScale := (FDrwWidth - 2.0*FDrwZeroWidth) / (ln(FValXPosMax)-ln(FValXPosMin) + ln(FValXNegMax)-ln(FValXNegMin));
      if Val < 0 then
        Result := (ln(FValXNegMax)-ln(FValXNegMin)) * LogScale -                     (ln(abs(Val)) - ln(FValXNegMin)) * LogScale
      else if Val = 0 then
        Result := (ln(FValXNegMax)-ln(FValXNegMin)) * LogScale + FDrwZeroWidth
      else
        Result := (ln(FValXNegMax)-ln(FValXNegMin)) * LogScale + 2.0*FDrwZeroWidth + (ln(    Val ) - ln(FValXPosMin)) * LogScale;
    End;
End;

Function TBipolarSemiLogXCoord.ValY2Drw(Val : Double) : Double;
Begin
  Result := (Val - FValYMin) / (FValYMax-FValYMin) * FDrwHeight;
End;

Function TBipolarSemiLogXCoord.DrwX2Val(Drw : Double) : Double;
Begin
  raise Exception.Create('TODO: implement');
End;

Function TBipolarSemiLogXCoord.DrwY2Val(Drw : Double) : Double;
Begin
  Result := Drw/FDrwHeight * (FValYMax-FValYMin) + FValYMin;
End;

{ TVectorialDiagram }

Constructor TVectorialDiagram.Create(ACoord:TCoordBase);
Begin
  inherited Create;
  FVecDoc  := TvVectorialDocument.Create;
  FVecPage := FVecDoc.AddPage;
  FCoord   := ACoord;
  FBoxPenColor    := colBlack;
  FBoxPenWidth    := 1;
  FXGridPenColor  := colLtGray;
  FXGridPenWidth  := 1;
  FXTickPenColor  := colBlack;
  FXTickPenWidth  := 1;
  FXTickFontName  := 'Arial';
  FXTickFontSize  := 10.0;
  FTickLenDrw     := 5.0;
  FXBreakPenColor := FPColor($4000, $4000, $FFFF);
  FXBreakPenWidth := 1;
  FXBreakExtDrw   := 6.0;
  FXBreakSlantDrw := 2.0;
  FXBreakSpaceDrw := 2.0;
End;

Destructor TVectorialDiagram.Destroy;
Begin
  FVecDoc.Free;
  inherited Destroy;
End;

Procedure TVectorialDiagram.Resize(AWidth, AHeight : Double);
Begin
  // image size in pixel or mm
  FWidth  := AWidth;
  FHeight := AHeight;
  // diagram box itself
  FDiagBox.Left     := 3.0;
  FDiagBox.Bottom   := FXTickFontSize*1.3 + FTickLenDrw + 3.0;
  FDiagBox.Right    := FWidth  - 3;
  FDiagBox.Top      := FHeight - 3;
  FCoord.FDrwWidth  := FDiagBox.Right-FDiagBox.Left;
  FCoord.FDrwHeight := FDiagBox.Top-FDiagBox.Bottom;
  FVecDoc.Width     := FWidth;
  FVecDoc.Height    := FHeight;
  FVecPage.Width    := FVecDoc.Width;
  FVecPage.Height   := FVecDoc.Height;
  // clear page
  FVecPage.Clear;
End;

Procedure TVectorialDiagram.Resize(AWidth, AHeight : Double; ACanvas : TFPCustomCanvas);
Begin
  Resize(AWidth, AHeight);
  // initialize renderer and provide canvas (but without drawing) so that bounding box calculations (e.g., for text width) and so on are working
  FVecPage.Render(ACanvas,
    0, Round(FHeight),
    1.0, -1 * 1.0,
    False);
End;

Function TVectorialDiagram.Line(X1, Y1, X2, Y2 : Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer) : TPath;
Begin
  FVecPage.StartPath    (X1, Y1);
  FVecPage.SetPenColor  (Color);
  FVecPage.SetPenStyle  (Style);
  FVecPage.SetPenWidth  (Width);
  FVecPage.AddLineToPath(X2, Y2);
  FVecPage.EndPath;
End;

Function TVectorialDiagram.Rectangle(Left,Bottom,Right,Top:Double;PenColor:TFPColor;PenStyle:TFPPenStyle;PenWidth:Integer;BrushColor:TFPColor;BrushStyle: TFPBrushStyle) : TPath;
Begin
  FVecPage.StartPath    (Left,  Bottom);
  FVecPage.SetPenColor  (PenColor);
  FVecPage.SetPenStyle  (PenStyle);
  FVecPage.SetPenWidth  (PenWidth);
  FVecPage.SetBrushColor(BrushColor);
  FVecPage.SetBrushStyle(BrushStyle);
  FVecPage.AddLineToPath(Right, Bottom);
  FVecPage.AddLineToPath(Right, Top);
  FVecPage.AddLineToPath(Left,  Top);
  FVecPage.AddLineToPath(Left,  Bottom);
  Result := FVecPage.EndPath;
End;

Function TVectorialDiagram.Rectangle(Left,Bottom,Right,Top:Double;PenColor:TFPColor;PenStyle:TFPPenStyle;PenWidth:Integer) : TPath;
Begin
  Result := Rectangle(Left,Bottom,Right,Top,PenColor,PenStyle,PenWidth,colBlack,bsClear);
End;

Function TVectorialDiagram.EstimateTextWidth(T:TvText) : Double;
Begin
  if assigned(FVecPage.RenderInfo.Canvas) then
    Result := T.GetWidth(FVecPage.RenderInfo) // this needs a Canvas
  else
    Result := Length(T.Value.Strings[0]) * T.Font.Size * 0.58;  // estimating approx. 58% width compared to height
End;

// center text, one or multiple concatenated items
// place each item at the X center position, they will be moved
Procedure TVectorialDiagram.CenterText(AT : Array of TvText);
Var I : Integer;
    W : Array of Double;
    S : Double;
Begin
  SetLength(W, Length(AT));
  // calculate text widths
  For I := 0 to Length(AT)-1 do
    W[I] := EstimateTextWidth(AT[I]);
  S := - Sum(W) * 0.5;
  //  move to final positions
  For I := 0 to Length(AT)-1 do
    Begin
      AT[I].X := AT[I].X + S;
      S := S + W[I];
    End;
End;

Procedure TVectorialDiagram.SymPlus(X, Y, Size : Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
Begin
  Line(X-Size, Y,      X+Size, Y,      Color, Style, Width);
  Line(X,      Y-Size, X,      Y+Size, Color, Style, Width);
End;

Function TVectorialDiagram.IsVisible(ValX, ValY : Double) : Boolean;
Begin
  ValX := FCoord.ValX2Drw(ValX);
  ValY := FCoord.ValY2Drw(ValY);
  Result := True;
  if (ValX < 0.0) or (ValX > FCoord.FDrwWidth) or
     (ValY < 0.0) or (ValY > FCoord.FDrwHeight) then Result := False;
End;

Procedure TVectorialDiagram.DrawBox;
Begin
  Rectangle(FDiagBox.Left, FDiagBox.Bottom, FDiagBox.Right, FDiagBox.Top,
    FBoxPenColor, psSolid, FBoxPenWidth);
End;

Procedure TVectorialDiagram.DrawLinearXAxis;
Var Coord    : TLinearCoord;

  Procedure DrawXTick(XDrw:Double);
  Begin
    Line(FDiagBox.Left + XDrw, FDiagBox.Bottom + FTickLenDrw, FDiagBox.Left + XDrw, FDiagBox.Bottom - FTickLenDrw,
      FXTickPenColor, psSolid, FXTickPenWidth);
  End;

  Procedure DrawXValue(XDrw:Double;St:String);
  Begin
    CenterText([FVecPage.AddText(FDiagBox.Left + XDrw, FDiagBox.Bottom-FTickLenDrw-FXTickFontSize*1.3, 0.0, FXTickFontName, FXTickFontSize, St)]);
  End;

Var ValWidth : Double;
    TickDist : Double;
    TickExp  : Double;
    TickMag  : Double;
    TickIdx  : Double;
    TickVal  : Double;
    TickDrw  : Double;
Begin
  Coord := FCoord as TLinearCoord;
  ValWidth := Coord.FValXMax - Coord.FValXMin;
  // we want between 5 and 10 ticks
  TickDist := ValWidth * 0.1;
  TickExp := Floor(Log10(TickDist));
  TickMag := TickDist / Power(10.0, TickExp);
  if TickMag <= 2.0 then TickMag := 2.0
  else if TickMag <= 5.0 then TickMag := 5.0
  else TickMag := 10.0;
  TickDist := TickMag * Power(10.0, TickExp);
  TickIdx := Ceil(Coord.FValXMin / TickDist);
  repeat
    TickVal := TickIdx*TickDist;
    if TickVal > Coord.FValXMax then break;
    TickDrw := Coord.ValX2Drw(TickVal);
    DrawXTick(TickDrw);
    DrawXValue(TickDrw, FloatToStr(TickVal));
    TickIdx := TickIdx + 1.0;
  until false;
End;

Procedure TVectorialDiagram.DrawSemiLogXAxis;
Var Coord  : TBipolarSemiLogXCoord;
    Decade : Integer;
    Step   : Integer;
    Value  : Double;
    XDrw   : Double;

  Procedure DrawXGrid(Solid:Boolean);
  Var Style : TFPPenStyle;
  Begin
    if Solid then Style := psSolid
    else          Style := psDot;
    Line(FDiagBox.Left + XDrw, FDiagBox.Bottom, FDiagBox.Left + XDrw, FDiagBox.Top,
      FXGridPenColor, Style, FXGridPenWidth);
  End;

  Procedure DrawXTick;
  Begin
    Line(FDiagBox.Left + XDrw, FDiagBox.Bottom + FTickLenDrw, FDiagBox.Left + XDrw, FDiagBox.Bottom - FTickLenDrw,
      FXTickPenColor, psSolid, FXTickPenWidth);
  End;

  Procedure DrawXValue(Base,Exponent:Integer);
{$IFDEF UseFormula}
  Var
      F      : TvFormula;
      E      : TvFormulaElement;
{$ENDIF}
  Begin
{$IFDEF UseFormula}
    F := TvFormula.Create(FVecPage);
    F.Left := FDiagBox.Left + XDrw-5.0;
    F.Top := FDiagBox.Bottom-5.0;
    E := F.AddElementWithKind(fekPower);
    E.Formula.AddElementWithKindAndText(fekVariable, '10');
    E.AdjacentFormula.AddElementWithKindAndText(fekVariable, IntToStr(Decade));
    FVecPage.AddEntity(F);
    // this doesn't raise the exponent, it is written in the same line and same height as the base
{$ELSE}
    CenterText([
      FVecPage.AddText(FDiagBox.Left + XDrw, FDiagBox.Bottom-FTickLenDrw-FXTickFontSize*1.3, 0.0, FXTickFontName, FXTickFontSize,     IntToStr(Base)),
      FVecPage.AddText(FDiagBox.Left + XDrw, FDiagBox.Bottom-FTickLenDrw-FXTickFontSize*0.8, 0.0, FXTickFontName, FXTickFontSize*0.6, IntToStr(Exponent))
    ]);
{$ENDIF}
  End;

  Procedure DrawXAxisBreak(X:Double);
  Begin
    Line(X-FXBreakSlantDrw-FXBreakSpaceDrw, FDiagBox.Bottom - FXBreakExtDrw, X+FXBreakSlantDrw-FXBreakSpaceDrw, FDiagBox.Bottom + FXBreakExtDrw,
      FXBreakPenColor, psSolid, FXBreakPenWidth);
    Line(X-FXBreakSlantDrw+FXBreakSpaceDrw, FDiagBox.Bottom - FXBreakExtDrw, X+FXBreakSlantDrw+FXBreakSpaceDrw, FDiagBox.Bottom + FXBreakExtDrw,
      FXBreakPenColor, psSolid, FXBreakPenWidth);
  End;

Begin
  Coord := FCoord as TBipolarSemiLogXCoord;
  // negative values
  if Coord.FValXNegMax <> Coord.FValXNegMin then
    Begin
      For Decade := Floor(Log10(Coord.FValXNegMin)) to Trunc(Log10(Coord.FValXNegMax)) do
        For Step := 1 to 9 do
          Begin
            Value := Power(10.0, Decade)*Step;
            if (Value*1.001 < Coord.FValXNegMin) or (Value*0.999 > Coord.FValXNegMax) then Continue;
            Value := -Value;
            XDrw := Coord.ValX2Drw(Value);
            //WriteLn('Decade = ',Decade,', Step = ',Step,', XDrw = ',XDrw:1:1);
            DrawXGrid(Step = 1);
            if Step = 1 then
              Begin
                DrawXTick;
                DrawXValue(-10, Decade);
              End;
          End;
    End;
  // zero
  if (Coord.FValXNegMax <> Coord.FValXNegMin) or Coord.FValXWithZero then
    Begin
      XDrw := FCoord.ValX2Drw(0.0);
      DrawXGrid(true);
      DrawXTick;
      CenterText([FVecPage.AddText(FDiagBox.Left + XDrw, FDiagBox.Bottom-FTickLenDrw-FXTickFontSize*1.3, 0.0, FXTickFontName, FXTickFontSize, '0')]);
      DrawXAxisBreak(FDiagBox.Left + XDrw + Coord.FDrwZeroWidth*0.5);
      if Coord.FValXNegMax <> Coord.FValXNegMin then
        DrawXAxisBreak(FDiagBox.Left + XDrw - Coord.FDrwZeroWidth*0.5);
    End;
  // positive values
  For Decade := Floor(Log10(Coord.FValXPosMin)) to Trunc(Log10(Coord.FValXPosMax)) do
    For Step := 1 to 9 do
      Begin
        Value := Power(10.0, Decade)*Step;
        if (Value*1.001 < Coord.FValXPosMin) or (Value*0.999 > Coord.FValXPosMax) then Continue;
        XDrw := FCoord.ValX2Drw(Value);
        //WriteLn('Decade = ',Decade,', Step = ',Step,', XDrw = ',XDrw:1:1);
        DrawXGrid(Step = 1);
        if Step = 1 then
          Begin
            DrawXTick;
            DrawXValue(10, Decade);
          End;
      End;
End;

Procedure TVectorialDiagram.DrawYAxis;
Begin

End;

Procedure TVectorialDiagram.DrawRect(ValXMin, ValXMax, ValYMin,
  ValYMax : Double; PenColor : TFPColor; PenStyle : TFPPenStyle;
  PenWidth : Integer; BrushColor : TFPColor; BrushStyle : TFPBrushStyle);

  Procedure Swap(Var D1,D2:Double);
  Var T : Double;
  Begin
    T  := D1;
    D1 := D2;
    D2 := T;
  End;

Var X1,X2,Y1,Y2 : Double;
    CN,CE,CS,CW : Boolean;
Begin
  // convert value to drawing coordinates
  X1 := FCoord.ValX2Drw(ValXMin);
  Y1 := FCoord.ValY2Drw(ValYMin);
  X2 := FCoord.ValX2Drw(ValXMax);
  Y2 := FCoord.ValY2Drw(ValYMax);
  // sort
  if X1 > X2 then Swap(X1, X2);
  if Y1 > Y2 then Swap(Y1, Y2);
  // check if it drawn at all
  if X1 > FCoord.FDrwWidth  then Exit;
  if X2 < 0.0               then Exit;
  if Y1 > FCoord.FDrwHeight then Exit;
  if Y2 < 0.0               then Exit;
  // check if clipping happens
  CN := False; CE := False; CS := False; CW := False;
  if X1 < 0.0               then Begin X1 := 0.0;               CW := True; End;
  if X2 > FCoord.FDrwWidth  then Begin X2 := FCoord.FDrwWidth;  CE := True; End;
  if Y1 < 0.0               then Begin Y1 := 0.0;               CS := True; End;
  if Y2 > FCoord.FDrwHeight then Begin Y2 := FCoord.FDrwHeight; CN := True; End;
  // move to diagram box
  X1 := X1 + FDiagBox.Left;   X2 := X2 + FDiagBox.Left;
  Y1 := Y1 + FDiagBox.Bottom; Y2 := Y2 + FDiagBox.Bottom;
  // draw
  if not CN and not CE and not CS and not CW then
    Begin  // no clipping, simplest case
      Rectangle(X1, Y1, X2, Y2, PenColor,PenStyle,PenWidth,BrushColor,BrushStyle);
      Exit;
    End;
  // clipping occured, first draw the filled rectangle without border
  Rectangle(X1, Y1, X2, Y2, PenColor,psClear,0,BrushColor,BrushStyle);
  // then draw the border where applicable
  if not CN and CE and not CS and not CW then
    Begin  // clipping only east -> draw 3 sides
      FVecPage.StartPath    (X2, Y2);
      FVecPage.SetPenColor  (PenColor);
      FVecPage.SetPenStyle  (PenStyle);
      FVecPage.SetPenWidth  (PenWidth);
      FVecPage.AddLineToPath(X1, Y2);
      FVecPage.AddLineToPath(X1, Y1);
      FVecPage.AddLineToPath(X2, Y1);
      FVecPage.EndPath;
    End
  else if not CN and not CE and not CS and CW then
    Begin  // clipping only west -> draw 3 sides
      FVecPage.StartPath    (X1, Y2);
      FVecPage.SetPenColor  (PenColor);
      FVecPage.SetPenStyle  (PenStyle);
      FVecPage.SetPenWidth  (PenWidth);
      FVecPage.AddLineToPath(X2, Y2);
      FVecPage.AddLineToPath(X2, Y1);
      FVecPage.AddLineToPath(X1, Y1);
      FVecPage.EndPath;
    End
  else if not CN and CE and not CS and CW then
    Begin  // clipping only east and west -> draw top and bottom
      Line(X1, Y1, X2, Y1, PenColor, PenStyle, PenWidth);
      Line(X1, Y2, X2, Y2, PenColor, PenStyle, PenWidth);
    End
  else
    WriteLn('Warning: Clipping for CN=',CN,', CE=',CE,', CS=',CS,', CW=',CW,' not yet implemented');
End;

Procedure TVectorialDiagram.DrawRect(ValXMin, ValXMax, ValYMin, ValYMax : Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer);
Begin
  DrawRect(ValXMin, ValXMax, ValYMin, ValYMax, PenColor, PenStyle, PenWidth, colBlack, bsClear);
End;

Procedure TVectorialDiagram.DrawSymPlus(ValX,ValY,DrwSize:Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
Begin
  if not IsVisible(ValX, ValY) then Exit;
  SymPlus(FDiagBox.Left + FCoord.ValX2Drw(ValX), FDiagBox.Bottom + FCoord.ValY2Drw(ValY), DrwSize,
    Color, Style, Width);
End;

Procedure TVectorialDiagram.DrawRange(ValXMin, ValXMax, ValY, ValWidth : Double;
  PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer;
  BrushColor : TFPColor; BrushStyle : TFPBrushStyle);
//Var Range    : TPath;
//    ClipPath : TPath;
Begin
  DrawRect(ValXMin, ValXMax, ValY-ValWidth*0.5, ValY+ValWidth*0.5,
           PenColor,PenStyle,PenWidth,BrushColor,BrushStyle);
// clipping path doesn't work with SVG :-(
//  Range := Rectangle(FDiagBox.Left + FCoord.ValX2Drw(ValXMin), FDiagBox.Bottom + FCoord.ValY2Drw(ValY-ValWidth*0.5),
//                     FDiagBox.Left + FCoord.ValX2Drw(ValXMax), FDiagBox.Bottom + FCoord.ValY2Drw(ValY+ValWidth*0.5),
//                     PenColor,PenStyle,PenWidth,BrushColor,BrushStyle);
//  ClipPath := TPath.Create(FVecPage);
//  ClipPath.Pen.Color := colCyan;
//  WriteLn('    Left = ',FDiagBox.Left:1:3,', Right = ',FCoord.FDrwWidth:1:3);
//  WriteLn('    Bottom = ' , FDiagBox.Bottom:1:3,', Top = ',FCoord.FDrwHeight:1:3);
//  ClipPath.AppendMoveToSegment(FDiagBox.Left,                  FDiagBox.Bottom);
//  ClipPath.AppendLineToSegment(FDiagBox.Left,                  FDiagBox.Bottom+FCoord.FDrwHeight);
//  ClipPath.AppendLineToSegment(FDiagBox.Left+FCoord.FDrwWidth, FDiagBox.Bottom+FCoord.FDrwHeight);
//  ClipPath.AppendLineToSegment(FDiagBox.Left+FCoord.FDrwWidth, FDiagBox.Bottom);
//  ClipPath.AppendLineToSegment(FDiagBox.Left,                  FDiagBox.Bottom);
//  FVecPage.AddEntity(ClipPath);
//  FVecPage.SetClipPath(ClipPath, vcmEvenOddRule);
////  Range.ClipPath := ClipPath;
////  Range.ClipMode := vcmEvenOddRule;
End;

Procedure TVectorialDiagram.DrawAxes;
Begin
  // draw empty diagram
  if FCoord is TBipolarSemiLogXCoord then
    DrawSemiLogXAxis
  else if FCoord is TLinearCoord then
    DrawLinearXAxis
  else
    raise Exception.Create('Unsupported type of FCoord = '+FCoord.ClassName);
  DrawYAxis;
  DrawBox;   // box last for nicer looking image
(*
  // debugging: draw boxes for whole image and for diagram area
  Rectangle(0, 0, FWidth, FHeight, colCyan, psSolid, 1);
  if FCoord is TBipolarSemiLogXCoord then
    with FCoord as TBipolarSemiLogXCoord do
      DrawRect(-FValXNegMax, FValXPosMax, FValYMin, FValYMax, colMagenta, psSolid, 1)
  else if FCoord is TLinearCoord then
    with FCoord as TLinearCoord do
      DrawRect(-FValXMin, FValXMax, FValYMin, FValYMax, colMagenta, psSolid, 1)
  else
    raise Exception.Create('TODO: Implement for type of FCoord '+FCoord.ClassName);
*)
End;

Procedure TVectorialDiagram.Paint(ACanvas : TFPCustomCanvas);
Begin
  ACanvas.Clear;
  ACanvas.Brush.FPColor := colWhite;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(0, 0, Round(FWidth), Round(FHeight));

  FVecPage.Render(ACanvas,
    0, Round(FHeight),
    1.0, -1 * 1.0);
End;

Procedure TVectorialDiagram.WriteSVG(AFilename : String);
Begin
  FVecDoc.WriteToFile(AFilename, vfSVG);
End;

End.

