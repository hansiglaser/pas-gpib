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

Function Find(Needle:String;Haystack:TDynStringArray) : Integer;
Function Count(Needle:Char;Heystack:String):Integer;

Function Select(B:Boolean;T,F:String):String;
Function Select(I : Integer; Const S:Array of String) : String;

Function SigPending(SigNo:Integer) : Boolean;

Function SelectRead(AHandle:Integer;ATimeout:Integer { in us }) : Integer;
Procedure WriteData(Filename:String;Data:TDynByteArray);

Implementation
Uses BaseUnix, StrUtils;

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

End.

