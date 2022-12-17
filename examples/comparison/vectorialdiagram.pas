Unit VectorialDiagram;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Math,
  Graphics, FPImage, FPCanvas, ExtCtrls, FPVectorial, svgvectorialwriter;

Type

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

  { TBipolarSemiLogXBase }

  TBipolarSemiLogXBase = class(TCoordBase)
    (* TODO: would be better design, if we have a base class for any kind of
     * coordinate transformation, with virtual methods, and this class is
     * derived from it, and there could be a linear class too.
     *)
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
    FImage   : TImage;
    Constructor Create(ACoord : TCoordBase; AImage : TImage);
    Procedure Resize;
    // drawing coordinate functions
    Procedure Line(X1,Y1,X2,Y2:Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
    Procedure Rectangle(Left, Bottom, Right, Top : Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer; BrushColor : TFPColor; BrushStyle : TFPBrushStyle);
    Procedure Rectangle(Left, Bottom, Right, Top : Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer);
    Procedure SymPlus(X, Y, Size : Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
    // diagram functions
    Procedure DrawBox;
    Procedure DrawSemiLogXAxis;
    Procedure DrawYAxis;
    Procedure DrawRect(ValXMin,ValXMax,ValYMin,ValYMax:Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer; BrushColor : TFPColor; BrushStyle : TFPBrushStyle);
    Procedure DrawSymPlus(ValX, ValY, DrwSize : Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
    Procedure DrawRange(ValXMin,ValXMax,ValY,ValWidth:Double; PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer; BrushColor : TFPColor; BrushStyle : TFPBrushStyle);
    Procedure DrawAxes;
    Procedure Update;
  End;

Implementation

{ TCoordBase }

Constructor TCoordBase.Create;
Begin
  inherited Create;
End;

{ TBipolarSemiLogXBase }

(*
  "Value"  : data value
  "Dawing" : drawing coordinate
*)

Constructor TBipolarSemiLogXBase.Create;
Begin
  inherited Create;
End;

Function TBipolarSemiLogXBase.ValX2Drw(Val : Double) : Double;
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

Function TBipolarSemiLogXBase.ValY2Drw(Val : Double) : Double;
Begin
  Result := (Val - FValYMin) / (FValYMax-FValYMin) * FDrwHeight;
End;

Function TBipolarSemiLogXBase.DrwX2Val(Drw : Double) : Double;
Begin
  raise Exception.Create('TODO: implement');
End;

Function TBipolarSemiLogXBase.DrwY2Val(Drw : Double) : Double;
Begin
  Result := Drw/FDrwHeight * (FValYMax-FValYMin) + FValYMin;
End;

{ TVectorialDiagram }

Constructor TVectorialDiagram.Create(ACoord:TCoordBase; AImage : TImage);
Begin
  inherited Create;
  FVecDoc  := TvVectorialDocument.Create;
  FVecPage := FVecDoc.AddPage;
  FCoord   := ACoord;
  FImage   := AImage;
End;

Procedure TVectorialDiagram.Resize;
Begin
  FDiagBox.Left     := 20.0;
  FDiagBox.Bottom   := 20.0;
  FDiagBox.Right    := FImage.Width  - 3;
  FDiagBox.Top      := FImage.Height - 3;
  FCoord.FDrwWidth  := FDiagBox.Right-FDiagBox.Left;
  FCoord.FDrwHeight := FDiagBox.Top-FDiagBox.Bottom;
  FVecDoc.Width     := FCoord.FDrwWidth;
  FVecDoc.Height    := FCoord.FDrwHeight;
End;

Procedure TVectorialDiagram.Line(X1, Y1, X2, Y2 : Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
Begin
  FVecPage.StartPath    (X1, Y1);
  FVecPage.SetPenColor  (Color);
  FVecPage.SetPenStyle  (Style);
  FVecPage.SetPenWidth  (Width);
  FVecPage.AddLineToPath(X2, Y2);
  FVecPage.EndPath;
End;

Procedure TVectorialDiagram.Rectangle(Left,Bottom,Right,Top:Double;PenColor:TFPColor;PenStyle:TFPPenStyle;PenWidth:Integer;BrushColor:TFPColor;BrushStyle: TFPBrushStyle);
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
  FVecPage.EndPath;
End;

Procedure TVectorialDiagram.Rectangle(Left,Bottom,Right,Top:Double;PenColor:TFPColor;PenStyle:TFPPenStyle;PenWidth:Integer);
Begin
  Rectangle(Left,Bottom,Right,Top,PenColor,PenStyle,PenWidth,colBlack,bsClear);
End;

Procedure TVectorialDiagram.SymPlus(X, Y, Size : Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
Begin
  Line(X-Size, Y,      X+Size, Y,      Color, Style, Width);
  Line(X,      Y-Size, X,      Y+Size, Color, Style, Width);
End;

Procedure TVectorialDiagram.DrawBox;
Begin
  Rectangle(FDiagBox.Left, FDiagBox.Bottom, FDiagBox.Right, FDiagBox.Top,
    colBlack, psSolid, 1);
End;

Procedure TVectorialDiagram.DrawSemiLogXAxis;
Var Coord  : TBipolarSemiLogXBase;
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
      FPColor($C000, $C000, $C000), Style, 1);
  End;

  Procedure DrawXTick;
  Const DrwTickExt = 5.0;    // extension above and below axis
  Begin
    Line(FDiagBox.Left + XDrw, FDiagBox.Bottom + DrwTickExt, FDiagBox.Left + XDrw, FDiagBox.Bottom - DrwTickExt,
      colBlack, psSolid, 1);
  End;

  Procedure DrawXValue(Base,Exponent:Integer);
  Var
{$IFDEF UseFormula}
      F      : TvFormula;
      E      : TvFormulaElement;
{$ELSE}
      T1     : TvText;
      W      : Double;
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
    T1 := FVecPage.AddText(FDiagBox.Left + XDrw-9.0, FDiagBox.Bottom-18.0, 0.0, 'Arial', 10, IntToStr(Base));
    W  := T1.GetWidth(FVecPage.RenderInfo);
    FVecPage.AddText(FDiagBox.Left + XDrw-9.0+W, FDiagBox.Bottom-12.0, 0.0, 'Arial',  6, IntToStr(Exponent));
{$ENDIF}
  End;

  Procedure DrawXAxisBreak(X:Double);
  Const DrwBreakExt   = 6.0;    // extension above and below axis
        DrwBreakSlant = 2.0;    // how much slanted (actually half of that)
        DrwBreakSpace = 2.0;    // space between the two lines (actually half of that space)
  Begin
    Line(X-DrwBreakSlant-DrwBreakSpace, FDiagBox.Bottom - DrwBreakExt, X+DrwBreakSlant-DrwBreakSpace, FDiagBox.Bottom + DrwBreakExt,
      FPColor($4000, $4000, $FFFF), psSolid, 1);
    Line(X-DrwBreakSlant+DrwBreakSpace, FDiagBox.Bottom - DrwBreakExt, X+DrwBreakSlant+DrwBreakSpace, FDiagBox.Bottom + DrwBreakExt,
      FPColor($4000, $4000, $FFFF), psSolid, 1);
  End;

Begin
  Coord := TBipolarSemiLogXBase(FCoord);
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
      FVecPage.AddText(FDiagBox.Left + XDrw-4.0, FDiagBox.Bottom-18.0, 0.0, 'Arial', 10, '0');
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
Begin
  Rectangle(FDiagBox.Left + FCoord.ValX2Drw(ValXMin), FDiagBox.Bottom + FCoord.ValY2Drw(ValYMin),
            FDiagBox.Left + FCoord.ValX2Drw(ValXMax), FDiagBox.Bottom + FCoord.ValY2Drw(ValYMax),
            PenColor,PenStyle,PenWidth,BrushColor,BrushStyle);
End;

Procedure TVectorialDiagram.DrawSymPlus(ValX,ValY,DrwSize:Double; Color : TFPColor; Style : TFPPenStyle; Width : Integer);
Begin
  SymPlus(FDiagBox.Left + FCoord.ValX2Drw(ValX), FDiagBox.Bottom + FCoord.ValY2Drw(ValY), DrwSize,
    Color, Style, Width);
End;

Procedure TVectorialDiagram.DrawRange(ValXMin, ValXMax, ValY, ValWidth : Double;
  PenColor : TFPColor; PenStyle : TFPPenStyle; PenWidth : Integer;
  BrushColor : TFPColor; BrushStyle : TFPBrushStyle);
Begin
  Rectangle(FDiagBox.Left + FCoord.ValX2Drw(ValXMin), FDiagBox.Bottom + FCoord.ValY2Drw(ValY-ValWidth*0.5),
            FDiagBox.Left + FCoord.ValX2Drw(ValXMax), FDiagBox.Bottom + FCoord.ValY2Drw(ValY+ValWidth*0.5),
            PenColor,PenStyle,PenWidth,BrushColor,BrushStyle);
End;

Procedure TVectorialDiagram.DrawAxes;
Begin
  // initialize renderer without drawing so that bounding box calculations and so on are working
  FVecPage.Clear;
  FVecPage.Render(FImage.Canvas,
    0, FImage.Height,
    1.0, -1 * 1.0,
    False);
  // DrawAxes diagram
  DrawSemiLogXAxis;
  DrawYAxis;
  DrawBox;   // box last for nicer looking image
End;

Procedure TVectorialDiagram.Update;
Begin
  FImage.Canvas.Clear;
  FImage.Canvas.Brush.Color := clWhite;
  FImage.Canvas.Brush.Style := bsSolid;
  FImage.Canvas.FillRect(0, 0, FImage.Width, FImage.Height);

  FVecPage.Render(FImage.Canvas,
    0, FImage.Height,
    1.0, -1 * 1.0);
  FImage.Invalidate;
//  FVecDoc.WriteToFile('test.svg', vfSVG);
End;

End.

