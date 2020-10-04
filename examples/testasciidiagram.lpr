Program TestAsciiDiagram;

{$mode objfpc}{$H+}

Uses
  Classes, AsciiDiagram;

Var AI : TAsciiImage;
    AR : TAsciiImageReal;
    AD : TAsciiDiagram;

Begin
  WriteLn('TAsciiImage'); ////////////////////////////////////////////////////
  AI := TAsciiImage.Create(80, 25);
  // Put(X,Y:Integer;Ch:Char);
  AI.Put(  0,  0, '1');
  AI.Put( 79,  0, '2');
  AI.Put(  0, 24, '3');
  AI.Put( 79, 24, '4');
  AI.Put(100,  0, '5');
  AI.Put(-20,  0, '5');
  AI.Put(  0, 30, '6');
  AI.Put(  0,-30, '7');
  // Get(X,Y:Integer):Char;
  // TODO
  // LineHoriz(X1,X2,Y:Integer;Ch:Char);
  AI.LineHoriz( 10,  20,  1, 'a');
  AI.LineHoriz(-10,  20,  2, 'b');
  AI.LineHoriz( 10, 100,  3, 'c');
  AI.LineHoriz( 10,  20, -5, 'd');
  AI.LineHoriz( 10,  20, 40, 'e');
  // LineVert (X,Y1,Y2:Integer;Ch:Char);
  AI.LineVert( 10,  8, 14, 'A');
  AI.LineVert( 11, -5, 28, 'B');
  AI.LineVert( 12, 28, 34, 'C');
  AI.LineVert(-10,  8, 14, 'D');
  AI.LineVert(100,  8, 14, 'E');
  // WriteLeft(X, Y : Integer; St : String);
  AI.WriteLeft(  0, 14, 'WriteLeft');
  AI.WriteLeft( 30, 15, 'Write');
  AI.WriteLeft( 79, 16, 'Write');
  AI.WriteLeft( 70, 17, 'WriteWriteWriteWriteWriteWrite');
  AI.WriteLeft(-12, 18, 'WriteWriteWriteWriteWriteWrite');
  // WriteCenter(X, Y : Integer; St : String);
  AI.WriteCenter(40, 14, 'WriteCenter');
  AI.LineVert(40, 5, 13, '|');
  AI.WriteCenter(40, 12, '1');
  AI.WriteCenter(40, 11, '12');
  AI.WriteCenter(40, 10, '123');
  AI.WriteCenter(40,  9, '1234');
  AI.WriteCenter(40,  8, '12345');
  AI.WriteCenter(40,  7, '123456');
  AI.WriteCenter(40,  6, '1234567');
  // WriteRight (X, Y : Integer; St : String);
  AI.WriteRight(79, 13, 'WriteRight');
  // Print
  AI.Print;
  AI.Free;

  WriteLn('TAsciiImageReal'); ////////////////////////////////////////////////
  // -1..+1 in X and Y, i.e., origin in the middle
  AR := TAsciiImageReal.Create(80, 25, 79.0/2.0, 79.0/2.0, 24.0/2.0, 24.0/2.0);
  // Put(X,Y:Double;Ch:Char);
  AR.Put( 0.0, 0.0, '1');
  AR.Put(-1.0, 1.0, '2');
  AR.Put( 1.0,-1.0, '3');
  AR.Put( 1.0, 1.0, '4');
  AR.Put(-1.0,-1.0, '5');
  // Get(X,Y:Double):Char;
  // TODO
  // LineHoriz(X1,X2,Y:Double;Ch:Char);
  AR.LineHoriz(-0.95, +0.95, -1.0, 'a');
  AR.LineHoriz(-0.95, +0.95, +1.0, 'B');
  // LineVert (X,Y1,Y2:Double;Ch:Char);
  AR.LineVert(-1.0, -0.90, +0.90, 'A');
  AR.LineVert(+1.0, -0.90, +0.90, 'b');
  // WriteLeft(X, Y : Double; St : String);
  AR.WriteLeft(-1.0, 0.1, 'WriteLeft');
  // WriteCenter(X, Y : Double; St : String);
  AR.WriteCenter(0.0, -0.1, 'WriteCenter');
  // WriteRight (X, Y : Double; St : String);
  AR.WriteRight(1.0, -0.2, 'WriteRight');
  // Print
  AR.Print;
  AR.Free;

  WriteLn('TAsciiImageReal'); ////////////////////////////////////////////////
  AD := TAsciiDiagram.Create(80, 25, 5, 3, 10.0, 20.0, 100.0, 200.0);
  // Put(X,Y:Double;Ch:Char);
  AD.Put(10.0, 100.0, '1');
  AD.Put(20.0, 100.0, '2');
  AD.Put(20.0, 200.0, '3');
  AD.Put(10.0, 200.0, '4');
  // DrawAxes;
  AD.DrawAxes;
  // DrawXTics(ATickDist:Double);
  AD.DrawXTics(2.0);
  // DrawYTics(ATickDist:Double);
  AD.DrawYTics(20.0);
  // Print
  AD.Print;
  AD.Free;
  AD := TAsciiDiagram.Create(80, 25, 5, 3, -2.5e-3, 2.5e-3, 100.0, 200.0);
  AD.DrawAxes;
  AD.DrawXTics(500e-6);
  AD.DrawYTics(20.0);
  AD.Print;
  AD.Free;
End.

