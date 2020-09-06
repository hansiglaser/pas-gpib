Unit PasGpibUtils;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils; 

Type
  TDynByteArray   = Array of Byte;
  TDynStringArray = Array of String;
  TDynDoubleArray = Array of Double;

Function SplitStr(Delimiter:String;St:String) : TDynStringArray;
Function SplitDouble(Delimiter:Char;St:String) : TDynDoubleArray;
Function JoinStr(Delimiter:String;Arr:TDynStringArray) : String;

Function Find(Needle:String;Haystack:TDynStringArray) : Integer;
Function Count(Needle:Char;Heystack:String):Integer;

Function Select(B:Boolean;T,F:String):String;
Function Select(I : Integer; Const S:Array of String) : String;

Function SigPending(SigNo:Integer) : Boolean;

Function SelectRead(AHandle:Integer;ATimeout:Integer { in us }) : Integer;
Procedure WriteData(Filename:String;Data:TDynByteArray);

Procedure Dump(Const Buf;Size:LongInt);

Type
  TSIPrefix = (
    spYocto, spZepto, spAtto, spFemto, spPico, spNano, spMicro, spMilli, spCenti, spDeci,
    spNone,
    spDeca, spHecto, spKilo, spMega, spGiga, spTera, spPeta, spExa, spZetta, spYotta);

Const
  CSIPrefixSymbol : Array[TSiPrefix] of String[2] = (
    'y', 'z', 'a', 'f', 'p', 'n', 'µ', 'm', 'c', 'd',
    '',
    'da', 'h', 'k', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y');
  CSIPrefixText : Array[TSiPrefix] of String[5] = (
    'yocto', 'zepto', 'atto', 'femto', 'pico', 'nano', 'micro', 'milli', 'centi', 'deci',
    '',
    'deca', 'hecto', 'kilo', 'mega', 'giga', 'tera', 'peta', 'exa', 'zetta', 'yotta');
  CSIPrefixPower : Array[TSiPrefix] of Integer = (
    -24, -21, -18, -15, -12, -9, -6, -3, -2, -1,
    0,
    1, 2, 3, 6, 9, 12, 15, 18, 21, 24);
  CSIPrefixFactor : Array[TSiPrefix] of Double = (
    1E-24, 1E-21, 1E-18, 1E-15, 1E-12, 1E-9, 1E-6, 1E-3, 1E-2, 1E-1,
    1,
    1E1, 1E2, 1E3, 1E6, 1E9, 1E12, 1E15, 1E18, 1E21, 1E24);

Function GetSIPrefix(Value:Double) : TSIPrefix;
Function FloatToStrSI(Value:Double;Const FormatSettings:TFormatSettings) : String;

Implementation
Uses BaseUnix, StrUtils, Math;

Function SplitStr(Delimiter:String;St:String) : TDynStringArray;
Var S,E : Integer;
Begin
  S := 1;
  SetLength(Result,0);
  repeat
    E := NPos(Delimiter,St,Length(Result)+1);
    if E = 0 then E := Length(St)+1;
    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1] := Copy(St,S,E-S);
    S := E + Length(Delimiter);
  until E >= Length(St);
End;

Function SplitDouble(Delimiter : Char; St : String) : TDynDoubleArray;
Var C,I,P1,P2,J : Integer;
    V : String;
Begin
  P1 := 1;
  C := Count(Delimiter,St)+1;
  SetLength(Result,C);
  For I := 0 to C-1 do
    Begin
      P2 := PosEx(Delimiter,St,P1)-1; // TODO: check the string index magic here
      if I = C-1 then
        P2 := Length(St)
      else if P2 = 0 then
        raise Exception.CreateFmt('Couldn''t find delimiter ''%s'' after position %d',[Delimiter,P1]);
      V := Trim(Copy(St,P1,P2-P1+1));
      Val(V,Result[I],J);
      if J <> 0 then
        raise Exception.CreateFmt('Invalid floating point number ''%s'' at position %d',[Copy(St,P1,P2),J]);
      P1 := P2 + 2;
    End;
End;

Function JoinStr(Delimiter : String; Arr : TDynStringArray) : String;
Var I : Integer;
Begin
  Result := '';
  For I := 0 to Length(Arr)-1 do
    Begin
      if I <> 0 then Result := Result + Delimiter;
      Result := Result + Arr[I];
    End;
End;

(**
 * Find Needle in Haystack and return its position
 *
 * If it is not found, -1 is returned
 *)
Function Find(Needle:String;Haystack:TDynStringArray) : Integer;
Begin
  For Result := 0 to Length(Haystack)-1 do
    if Haystack[Result] = Needle then
      Exit;
  Result := -1;
End;

Function Count(Needle:Char;Heystack:String):Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 1 to Length(Heystack) do
    if Heystack[I] = Needle then
      Inc(Result);
End;

Function Select(B: Boolean; T, F: String): String;
Begin
  if B then Result := T
  else      Result := F;
End;

Function Select(I : Integer; Const S:Array of String) : String;
Begin
  if (I < 0) or (I >= Length(S)) then
    Exit('Invalid ('+IntToStr(I)+')');
  Result := S[I];
End;

Function SigPending(SigNo:Integer) : Boolean;
Var SigSet : TSigSet;
Begin
  FpSigEmptySet(SigSet);
  FpSigPending(SigSet);
  Result := (FpSigIsMember(SigSet,SigNo) <> 0);
End;

Function SelectRead(AHandle:Integer;ATimeout:Integer { in us }) : Integer;
Var FD : TFDSet;
    T  : Timeval;
Begin
  fpFD_ZERO(FD);
  fpFD_SET (AHandle,FD);
  T.tv_usec := ATimeout mod 1000000;
  T.tv_sec  := ATimeout div 1000000;
  Result := fpSelect(AHandle+1,@FD,Nil,Nil,@T);
End;

Procedure WriteData(Filename : String; Data : TDynByteArray);
Var F : File;
Begin
  Assign(F,Filename);
  Rewrite(F,1);
  BlockWrite(F,Data[0],Length(Data));
  Close(F);
end;

Procedure Dump(Const Buf;Size:LongInt);
Var I : Integer;
    B : Byte;
    S : String[16];
Begin
  S := '                ';
  For I := 0 to Size-1 do
    Begin
      if I and $F = 0 then Write(IntToHex(I,4),': ');
      B := PByte(PtrUInt(@Buf)+I)^;
      Write(IntToHex(B,2),' ');
      if B in [$20..$7E,$80-$FF] then
        S[(I and $F)+1] := Chr(B)
      else
        S[(I and $F)+1] := '.';
      if I and $F = $F then
        Begin
          WriteLn('  ',S);
          S := '                ';
        End;
    End;
  if I and $F <> $F then
    Begin
      WriteLn(StringOfChar(' ',3*(15-(I and $F))),'  ',S);
    End;
End;

Function GetSIPrefix(Value : Double) : TSIPrefix;
Var LogValue : Integer;
Begin
  Result := spNone;
  if Value = 0 then Exit;
  Value := Abs(Value);
  LogValue := Floor(Log10(Value)/3.0)*3;

  While (LogValue > CSIPrefixPower[Result]) and (Result < High(TSIPrefix)) do
    Result := Succ(Result);
  While (LogValue < CSIPrefixPower[Result]) and (Result > Low(TSIPrefix)) do
    Result := Pred(Result);
End;

Function FloatToStrSI(Value:Double;Const FormatSettings:TFormatSettings) : String;
Var SIPrefix : TSIPrefix;
Begin
  SIPrefix := GetSIPrefix(Value);
  Value    := Value / CSIPrefixFactor[SIPrefix];
  Result   := FloatToStr(Value,FormatSettings) + CSIPrefixSymbol[SIPrefix];
End;

End.

