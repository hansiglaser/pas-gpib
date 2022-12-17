Unit Instrument;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  { TValueAccuracyBase }

  TValueAccuracyBase = class
  public
    FValue : Double;           ///< measurement value from instrument
    Constructor Create(AValue:Double);
    Function ToString : String; override;
  published
    property Value : Double read FValue write FValue;
  End;

  { TValueAccuracyMinMax }

  TValueAccuracyMinMax = class(TValueAccuracyBase)
  public
    FMin : Double;             ///< lower limit of true value
    FMax : Double;             ///< upper limit of true value
    Constructor Create(AValue,AMin,AMax:Double);
    Function ToString : String; override;
  published
    property Min : Double read FMin write FMin;
    property Max : Double read FMax write FMax;
  End;

  { TAccuracyBase }

  TAccuracyBase = class
    Function Apply(AValue:Double) : TValueAccuracyBase; virtual; abstract;
  End;

  { TAccuracyGainOffset }

  TAccuracyGainOffset = class(TAccuracyBase)
    FGainError   : Double;     ///< gain error relative to measurement value from instrument
    FOffsetError : Double;     ///< offset error independent of measurement value
    Constructor Create(AGainError,AOffsetError:Double);
    Function Apply(AValue:Double) : TValueAccuracyBase; override;
  End;

  { TMeasureRangeBase }

  TMeasureRangeBase = class
    FMaxValue   : Double;
    FBipolar    : Boolean;
    FResolution : Double;               // TODO: make array, e.g., for NPLC setting
    Constructor Create(AMaxValue:Double;ABipolar:Boolean;AResolution:Double);
  End;

  TQuantity = (qtDCV, qtDCI, qtACV, qtACI);

Const
  CQuantityStr : Array[TQuantity] of String = ('DCV','DCI','ACV','ACI');

Type

  { TMeasureRangeAccuracy }

  TMeasureRangeAccuracy = class(TMeasureRangeBase)
    FAccuracy : Array of TAccuracyBase;   // index is instrument-specific accuracy rating, e.g., time since last calibration
    Procedure AddAccuracy(AAccuracy:TAccuracyBase);
  End;
  TRangesArray = Array of TMeasureRangeAccuracy;
  TRangesQuantity = Array[TQuantity] of TRangesArray;

  TInstrument = class
    class Function GetRanges(AInstrument:String) : TRangesQuantity; virtual; abstract;
  End;


Implementation
Uses Math;

{ TValueAccuracyBase }

Constructor TValueAccuracyBase.Create(AValue : Double);
Begin
  FValue := AValue;
End;

Function TValueAccuracyBase.ToString : String;
Begin
  Result := FloatToStr(FValue);
End;

{ TValueAccuracyMinMax }

Constructor TValueAccuracyMinMax.Create(AValue, AMin, AMax : Double);
Begin
  inherited Create(AValue);
  FMin := AMin;
  FMax := AMax;
End;

Function TValueAccuracyMinMax.ToString : String;
Begin
  if False then
    Begin
      // absolute
      Result := FloatToStr(FValue)+' ('+FloatToStr(FMin)+' - '+FloatToStr(FMax)+')';
    End
  else
    Begin
      if SameValue(FValue-FMin, FMax-FValue) then
        // relative with same margins on both sides
        Result := FloatToStr(FValue)+' ±'+FloatToStr(FValue-FMin)
      else
        // relative
        Result := FloatToStr(FValue)+' -'+FloatToStr(FValue-FMin)+'..+'+FloatToStr(FMax-FValue);
    End;
End;

{ TAccuracyGainOffset }

Constructor TAccuracyGainOffset.Create(AGainError, AOffsetError : Double);
Begin
  inherited Create;
  FGainError   := AGainError;
  FOffsetError := AOffsetError;
End;

Function TAccuracyGainOffset.Apply(AValue : Double) : TValueAccuracyBase;
Var Error : Double;
Begin
  Error := FOffsetError + abs(AValue)*FGainError;
  Result := TValueAccuracyMinMax.Create(AValue, AValue - Error, AValue + Error);
End;

{ TMeasureRangeBase }

Constructor TMeasureRangeBase.Create(AMaxValue : Double; ABipolar : Boolean; AResolution : Double);
Begin
  FMaxValue   := AMaxValue;
  FBipolar    := ABipolar;
  FResolution := AResolution;
End;

{ TMeasureRangeAccuracy }

Procedure TMeasureRangeAccuracy.AddAccuracy(AAccuracy : TAccuracyBase);
Begin
  SetLength(FAccuracy, Length(FAccuracy)+1);
  FAccuracy[Length(FAccuracy)-1] := AAccuracy;
End;

End.

