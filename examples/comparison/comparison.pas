Unit Comparison;

{$mode ObjFPC}{$H+}

{ $ D EFINE CreateMeasureDefinitionFirstAttempt}
{ $ D EFINE TestJSONStreamer}

Interface

Uses
  Classes, SysUtils, StrUtils, Math, FGL,
{$IFDEF TestJSONStreamer}
  fpjson, TypInfo, fpjsonrtti,
{$ENDIF}
  DevCom, DevComVisa, DevComRS232, Serial, PasGpibUtils,
  Agilent34410A, KeysightU125xB, KeysightE3631xA, AgilentMSOX3000A,
  Instrument, RemoteInstrument;

Type

  // Temporary instrument definitions (TODO: make individual units) ////////////

  { TFluke177 }

  TFluke177 = class(TRemoteInstrument)
    class Function GetRanges(AInstrument:String) : TRangesQuantity; override;
  End;

  { TKeithleyDMM6500 }

  TKeithleyDMM6500 = class(TRemoteInstrument)
    class Function GetRanges(AInstrument:String) : TRangesQuantity; override;
  End;

  { TKeithley2450 }

  TKeithley2450 = class(TRemoteInstrument)
    class Function GetRanges(AInstrument:String) : TRangesQuantity; override;
  End;

  // Testpoints ////////////////////////////////////////////////////////////////

  TDynDoubleArray = Array of Double;

  { TTestPoints }

  TTestPoints = class
    FValues : TDynDoubleArray;
    Constructor Create;
    Constructor Create(AValues : TDynDoubleArray);
    Procedure AddPoint(AValue:Double);
    Procedure Sort;
    Procedure AddPoints(ARange : TMeasureRangeBase; ANumLinPoints : Integer);
    Procedure AddPoints(ATestPoints : TTestPoints);
    Function Range(AFrom,ATo:Double) : TDynDoubleArray;
    Function RangeTo(ATo:Double) : TDynDoubleArray;
    Function ToString : String; override;
  End;

  // Instrument Wrapper Params /////////////////////////////////////////////////

Type
  TInstrumentFunction = (ifMeasure,ifSource);

Const
  CInstrumentFunction : Array[TInstrumentFunction] of String = ('Measure','Source');

Type

  { TParamValueBase }

  TParamValueBase = class
    Function ToSyntax : String; virtual; abstract;
  End;

  { TParamValueGeneric }

  generic TParamValueGeneric<T> = class(TParamValueBase)
    FValue : T;
    Constructor Create(AValue:T);
    // ToSyntax has to be implemented in derived classes, because there is no
    // generic way to convert any type to a string, and explicit implementation
    // of specialized methods is not possible.
  End;

  TParamValueGenericInteger   = specialize TParamValueGeneric<Integer>;
  TParamValueGenericDouble    = specialize TParamValueGeneric<Double>;
  TParamValueGenericString    = specialize TParamValueGeneric<String>;
  TParamValueGenericFunction  = specialize TParamValueGeneric<TInstrumentFunction>;

  { TParamValueInteger }

  TParamValueInteger = class(TParamValueGenericInteger)
    Function ToSyntax : String; override;
  End;

  { TParamValueDouble }

  TParamValueDouble = class(TParamValueGenericDouble)
    Function ToSyntax : String; override;
  End;

  { TParamValueString }

  TParamValueString = class(TParamValueGenericString)
    Function ToSyntax : String; override;
  End;

  { TParamValueIdentifier }

  TParamValueIdentifier = class(TParamValueString)
    Function ToSyntax : String; override;
  End;

  { TParamValueFunction }

  TParamValueFunction = class(TParamValueGenericFunction)
    Constructor Create(AValue:TInstrumentFunction);
    Constructor Create(AValue:String);
    Function ToSyntax : String; override;
  End;

  TParamsMap = specialize TFPGMapObject<String, TParamValueBase>;

  { TInstrumentWrapperParams }

  TInstrumentWrapperParams = class
  private
    FParams : TParamsMap;
  public
    Constructor Create;
    Constructor Create(AKey:String;AValue:TParamValueBase);
    Constructor Create(Args : Array of Const);
    Destructor Destroy; override;
    Function  Get        (AKey:String) : TParamValueBase;
    Function  GetInteger (AKey:String) : Integer;
    Function  GetDouble  (AKey:String) : Double;
    Function  GetString  (AKey:String) : String;
    Function  GetFunction(AKey:String) : TInstrumentFunction;
    Procedure SetInteger (AKey:String; AValue:Integer);
    Procedure SetDouble  (AKey:String; AValue:Double);
    Procedure SetString  (AKey:String; AValue:String);
    Procedure SetFunction(AKey:String; AValue:TInstrumentFunction);
    Procedure SetOrAddInteger (AKey:String; AValue:Integer);
    Procedure SetOrAddDouble  (AKey:String; AValue:Double);
    Procedure SetOrAddString  (AKey:String; AValue:String);
    Procedure SetOrAddFunction(AKey:String; AValue:TInstrumentFunction);
    Function ToSyntax : String;
  End;

  // Instrument Wrappers ///////////////////////////////////////////////////////

  TMeasurementResultBase = class;
  TComparisonBase = class;
  TInstrumentWrapperBase = class;
  TInstrumentWrapperClass = class of TInstrumentWrapperBase;
  TInstrumentWrapperRegistry = specialize TFPGMap<String,TInstrumentWrapperClass>;
  TInstrumentTypeItem = record
    FInstrumentType : TInstrumentWrapperClass;
    FInstruments    : Array of TInstrumentWrapperBase;
  End;
  TInstrumentTypes = Array of TInstrumentTypeItem;

  { TInstrumentWrapperFactory }

  TInstrumentWrapperFactory = class
    FRegistry : TInstrumentWrapperRegistry;
    Constructor Create;
    Destructor Destroy; override;
    Procedure RegisterWrapper(AWrapperName:String;AClass:TInstrumentWrapperClass);
    Function  CreateWrapper(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams) : TInstrumentWrapperBase;
  End;

  { TInstrumentWrapperBase }

  TInstrumentWrapperBase = class   // live without a pas-gpib TRemoteInstrument
    FComparisonBase : TComparisonBase;           // back-pointer, stores Quantity, ...
    FWrapperName    : String;                    // name with which the wrapper is registered and which is used in TInstrument.GetRanges, further describes the instrument, so that we only need one wrapper for e.g. E363xA, and can then internally distinguish between 10A, 12A and 13A, same for 34461A, ...
    FName           : String;                    // given name left of '=' in file, unique, used in diagrams, log files, ...
    FParams         : TInstrumentWrapperParams;  // can be Nil if instantiated with Create, but set by CreateFromParams, and created/updated with GetParams
    FFunction       : TInstrumentFunction;       // source or measure
    FRanges         : TRangesQuantity;           // set by SetupRanges
    FTestPoints     : Array of TTestPoints;      // set by CreateTestPoints
    FIdentifier     : String;                    // result of *IDN?, set by Initialize
  private
    Constructor Create(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams;AFunction:TInstrumentFunction);
    Destructor Destroy; override;
  public
    class Procedure RegisterWrapper(AFactory:TInstrumentWrapperFactory); virtual; abstract;   // can also register the same class multiple times, e.g., for E36310A, 12A, and 13A
    class Function CreateFromParams(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams) : TInstrumentWrapperBase; virtual; abstract;
    Function GetParams : TInstrumentWrapperParams; virtual; abstract;
    Procedure SetupRanges; virtual; abstract;   // sets FRanges, e.g., by calling TInstrument.GetRanges); → the ranges (returned from TInstrument.GetRanges) have that Accuracy information included and an Apply method
    Procedure CreateTestPoints(ANumLinPoints : Integer); virtual;    // sets FTestPoints
    // connectivity functions to a physical instrument
    Procedure Initialize; virtual; abstract;    // create a device object and connect to it, initialize, ...
    Procedure Disconnect; virtual; abstract;    // close connection and free objects
    // source functions, only implemented if a source device
    Procedure SetSourceRange(ARange:TMeasureRangeBase); virtual; abstract;
    Procedure EnableOutput; virtual; abstract;
    Procedure DisableOutput; virtual; abstract;
    Procedure SetSource(AValue:Double); virtual; abstract;
    Function  GetSourceValue : TMeasurementResultBase; virtual; abstract;
    // measurement functions, only implemented if a measurement device
    Procedure SetMeasureRange(ARange:TMeasureRangeBase); virtual; abstract;
    Procedure StartMeasurement; virtual; abstract;
    Function  GetResults : TMeasurementResultBase; virtual; abstract;
  End;

  { TInstrumentWrapperFluke177 }

  TInstrumentWrapperFluke177 = class(TInstrumentWrapperBase)
  private
    // TODO: some variables for the parameters from the file
//    FInstrument : TFluke177;               // set in Initialize
    FRange      : TMeasureRangeAccuracy;   // set in SetMeasureRange
  public
    class Procedure RegisterWrapper(AFactory:TInstrumentWrapperFactory); override;
    class Function CreateFromParams(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams) : TInstrumentWrapperBase; override;
    Function GetParams : TInstrumentWrapperParams; override;
    Procedure SetupRanges; override;
    Procedure Initialize; override;
    Procedure Disconnect; override;
    // measurement functions
    Procedure SetMeasureRange(ARange:TMeasureRangeBase); override;
    Procedure StartMeasurement; override;
    Function  GetResults : TMeasurementResultBase; override;
  End;

  { TInstrumentWrapperKeysightU125xB }

  TInstrumentWrapperKeysightU125xB = class(TInstrumentWrapperBase)
  private
    FDevice     : String;
    FComm       : TRS232Communicator;
    FInstrument : TKeysightU125xB;         // set in Initialize
    FRange      : TMeasureRangeAccuracy;   // set in SetMeasureRange
  public
    class Procedure RegisterWrapper(AFactory:TInstrumentWrapperFactory); override;
    class Function CreateFromParams(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams) : TInstrumentWrapperBase; override;
    Function GetParams : TInstrumentWrapperParams; override;
    Procedure SetupRanges; override;
    Procedure Initialize; override;
    Procedure Disconnect; override;
    // measurement functions
    Procedure SetMeasureRange(ARange:TMeasureRangeBase); override;
    Procedure StartMeasurement; override;
    Function  GetResults : TMeasurementResultBase; override;
  End;

  { TInstrumentWrapperAgilent34410A }

  TInstrumentWrapperAgilent34410A = class(TInstrumentWrapperBase)
  private
    FVISA       : String;
    FComm       : IDeviceCommunicator;
    FInstrument : TAgilent34410A;          // set in Initialize
    FRange      : TMeasureRangeAccuracy;   // set in SetMeasureRange
  public
    class Procedure RegisterWrapper(AFactory:TInstrumentWrapperFactory); override;
    class Function CreateFromParams(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams) : TInstrumentWrapperBase; override;
    Function GetParams : TInstrumentWrapperParams; override;
    Procedure SetupRanges; override;
    Procedure Initialize; override;
    Procedure Disconnect; override;
    // measurement functions
    Procedure SetMeasureRange(ARange:TMeasureRangeBase); override;
    Procedure StartMeasurement; override;
    Function  GetResults : TMeasurementResultBase; override;
  End;

  { TInstrumentWrapperKeithleyDMM6500 }

  TInstrumentWrapperKeithleyDMM6500 = class(TInstrumentWrapperBase)
  private
    // TODO: some variables for the parameters from the file
    FInstrument : TKeithleyDMM6500;        // set in Initialize
    FRange      : TMeasureRangeAccuracy;   // set in SetMeasureRange
  public
    class Procedure RegisterWrapper(AFactory:TInstrumentWrapperFactory); override;
    class Function CreateFromParams(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams) : TInstrumentWrapperBase; override;
    Function GetParams : TInstrumentWrapperParams; override;
    Procedure SetupRanges; override;
    Procedure Initialize; override;
    Procedure Disconnect; override;
    // measurement functions
    Procedure SetMeasureRange(ARange:TMeasureRangeBase); override;
    Procedure StartMeasurement; override;
    Function  GetResults : TMeasurementResultBase; override;
  End;

  { TInstrumentWrapperKeithley2450 }

  TInstrumentWrapperKeithley2450 = class(TInstrumentWrapperBase)
  private
    // TODO: some variables for the parameters from the file
    FSource       : TInstrumentWrapperKeithley2450;   // first Nil, set for both as soon as the 2nd is created
    FMeasure      : TInstrumentWrapperKeithley2450;   // first Nil, set for both as soon as the 2nd is created
    FInstrument   : TKeithley2450;           // set in Initialize
    FSourceRange  : TMeasureRangeAccuracy;   // set in SetSourceRange
    FMeasureRange : TMeasureRangeAccuracy;   // set in SetMeasureRange
  public
    class Procedure RegisterWrapper(AFactory:TInstrumentWrapperFactory); override;
    class Function CreateFromParams(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams) : TInstrumentWrapperBase; override;
    Function GetParams : TInstrumentWrapperParams; override;
    Procedure SetupRanges; override;
    Procedure Initialize; override;
    Procedure Disconnect; override;
    // source functions
    Procedure SetSourceRange(ARange:TMeasureRangeBase); override;
    Procedure EnableOutput; override;
    Procedure DisableOutput; override;
    Procedure SetSource(AValue:Double); override;
    Function  GetSourceValue : TMeasurementResultBase; override;
    // measurement functions
    Procedure SetMeasureRange(ARange:TMeasureRangeBase); override;
    Procedure StartMeasurement; override;
    Function  GetResults : TMeasurementResultBase; override;
  End;

  { TInstrumentWrapperKeysightE3631xA }

  TInstrumentWrapperKeysightE3631xA = class(TInstrumentWrapperBase)
  private
    FVISA         : String;
    FChannel      : String;
    FChannelIdx   : TChannel;
    FPairMode     : TPairMode;
    FCurLim       : Double;
    FSource       : TInstrumentWrapperKeysightE3631xA;   // first Nil, set for both as soon as the 2nd is created
    FMeasure      : TInstrumentWrapperKeysightE3631xA;   // first Nil, set for both as soon as the 2nd is created
    FComm         : IDeviceCommunicator;
    FInstrument   : TKeysightE3631xA;        // set in Initialize
    FSourceRange  : TMeasureRangeAccuracy;   // set in SetSourceRange
    FMeasureRange : TMeasureRangeAccuracy;   // set in SetMeasureRange
    FSourceValue  : Double;
  public
    class Procedure RegisterWrapper(AFactory:TInstrumentWrapperFactory); override;
    class Function CreateFromParams(AComparisonBase:TComparisonBase;AWrapperName,AName:String;AParams:TInstrumentWrapperParams) : TInstrumentWrapperBase; override;
    Function GetParams : TInstrumentWrapperParams; override;
    Procedure SetupRanges; override;
    Procedure Initialize; override;
    Procedure Disconnect; override;
    // source functions
    Procedure SetSourceRange(ARange:TMeasureRangeBase); override;
    Procedure EnableOutput; override;
    Procedure DisableOutput; override;
    Procedure SetSource(AValue:Double); override;
    Function  GetSourceValue : TMeasurementResultBase; override;
    // measurement functions
    Procedure SetMeasureRange(ARange:TMeasureRangeBase); override;
    Procedure StartMeasurement; override;
    Function  GetResults : TMeasurementResultBase; override;
  End;

  // Object Factory ////////////////////////////////////////////////////////////

  TObjectClasses = Array of TClass;

  { TObjectFactory }

  TObjectFactory = class
    (* This is a generic object factory, for all classes
     * Limitation: All data fields must be plain types, i.e., no classes,
     * strings, ...)
     *)
    FRegistry : TObjectClasses;
    Constructor Create;
    Destructor  Destroy; override;
    Procedure RegisterClass(AClass:TClass);
    Function EncodeObject(Const O : TObject) : String;
    Function CreateObject(Name:String) : TObject;
    Function DecodeObject(Const St : String) : TObject;
  End;

  // Measurement Results ///////////////////////////////////////////////////////

  TMeasurementResultClass = class of TMeasurementResultBase;
  TMeasurementResultClasses = Array of TMeasurementResultClass;
  TMeasurementResults = Array of Array of TMeasurementResultBase;

  { TMeasurementResultFactory }

  TMeasurementResultFactory = class
    FRegistry : TMeasurementResultClasses;
    Constructor Create;
    Destructor  Destroy; override;
    Procedure RegisterResultClass(AClass:TMeasurementResultClass);
    Function  CreateObject(Name:String) : TMeasurementResultBase;
    Function  CreateResult(St:String) : TMeasurementResultBase;
  End;

  { TMeasurementResultBase }

  TMeasurementResultBase = class
  public
    FValue : TValueAccuracyBase;
    Constructor Create(AValue:TValueAccuracyBase);
    Destructor Destroy; override;
    Function ToString : String; override;
    Function Encode : String; virtual;
    Constructor CreateDecode(St:String); virtual;
    // can be extended with statistics, series of measurements, other measurements (current, temperature, ...), additional information, ...
  published
    property Value : TValueAccuracyBase read FValue write FValue;
  End;

  { TMeasurementResultVI }

  TMeasurementResultVI = class(TMeasurementResultBase)
  public
    FCurrent : TValueAccuracyBase;
    Constructor Create(AVoltage,ACurrent:TValueAccuracyBase);
    Destructor Destroy; override;
    Function ToString : String; override;
    Function Encode : String; override;
    Constructor CreateDecode(St:String); override;
  published
    property Current : TValueAccuracyBase read FCurrent write FCurrent;
  End;

  // Measurement Result Analyses ///////////////////////////////////////////////

  { TMeasurementAnalysis }

  TMeasurementAnalysis = class
    FResults : Array of TMeasurementResultBase;   // pointers to referenced results, index is the instrument
    FMinMin  : Double;   // lowest bound of all ranges
    FMaxMin  : Double;   // highest case of lower bound of all ranges
    FMinMax  : Double;   // lowest case of higher bound of all ranges
    FMaxMax  : Double;   // higest bound of all ranges
    FPass    : Boolean;
    Constructor Create;
    Procedure Analyze;
  End;
  TMeasurementAnalyses = array of TMeasurementAnalysis;

  // Comparision ///////////////////////////////////////////////////////////////

  TInstrumentRanges = Array of TMeasureRangeBase;

  { TComparisonSet }

  TComparisonSet = class    // holds all testpoints with same range settings of all instruments, plus the measurement results
    FComparison   : TComparisonBase;
    FPrev         : TComparisonSet;
    FRanges       : TInstrumentRanges;   // same array index and length as TComparisonBase.FInstruments, only the places with a change of the range are populated
    FMaxVal       : Double;              // maximum value of all current active ranges, as well as max. value in FTestPoints
    FTestPoints   : TTestPoints;
    FMeasurements : TMeasurementResults; // FMeasurements[InstrumentIdx][TestPointIdx]
    FAnalyses     : TMeasurementAnalyses;// FAnalyses[TestPointIdx]
    FPass         : Boolean;
    Constructor Create(AComparison:TComparisonBase; APrev:TComparisonSet);
    Destructor Destroy; override;
    Procedure PrintRanges(AWithPrev:Boolean);
    Procedure PrintMeasurementsByInstrument;
    Procedure PrintMeasurementsByTestPoint;
    Function GetAllRanges : TInstrumentRanges;
    Procedure Analyze;
  End;

  { TComparisonProcedure }

  TComparisonProcedure = class     // holds complete sequence/procedure for testpoints in all ranges
    FSets : Array of TComparisonSet;
    FPass : Boolean;
    Constructor Create;
    Destructor Destroy; override;
    Procedure AddSet(ASet:TComparisonSet);
    Procedure PrintRanges;
    Procedure PrintMeasurementsByInstrument;
    Procedure PrintMeasurementsByTestPoint;
    Function GetNumSets : Integer;
    Function GetNumTestpoints : Integer;
    Procedure Analyze;
  End;

  { TComparisonBase }

  TComparisonBase = class
    FQuantity       : TQuantity;
    FInstruments    : Array of TInstrumentWrapperBase;
    FProcedure      : TComparisonProcedure;
    Constructor Create;
    Destructor Destroy; override;
    Procedure AddInstrument(AInstrument:TInstrumentWrapperBase);
    Procedure SetupRanges;
    Procedure CreateTestPoints(ANumLinPoints : Integer);
    Procedure PrintTestPoints;
    Function GetInstrumentIndex(AName:String) : Integer;   // returns -1 if not found
    Function GetInstrumentRangeIdx(AInstrumentIdx : Integer; ARange : Double) : Integer;             // returns -1 if not founds
    Function GetInstrumentRange   (AInstrumentIdx : Integer; ARange : Double) : TMeasureRangeBase;   // returns Nil if not found
    Function GetInstrumentTypes : TInstrumentTypes;
{$IFDEF CreateMeasureDefinitionFirstAttempt}
    Procedure CreateMeasureDefinitionFirstAttempt;
{$ENDIF} // CreateMeasureDefinitionFirstAttempt
    Procedure CreateMeasureDefinition;
    Procedure Save(AFilename:String);
{$IFDEF TestJSONStreamer}
    Procedure MyBeforeStreamObject(Sender : TObject; AObject : TObject; JSON : TJSONObject);
{$ENDIF}
    Constructor Load(AFilename:String);
  End;

{$IFDEF CreateMeasureDefinitionFirstAttempt}

  { TRangeInfo }

  TRangeInfo = class
    FInstrument  : TInstrumentDescriptor;
    FQuantity    : TQuantity;
    FRangeIdx    : Integer;
    FAccuracyIdx : Integer;
    Constructor Create(AInstrument:TInstrumentDescriptor;AQuantity:TQuantity;ARangeIdx:Integer;AAccuracyIdx:Integer);
    Function GetRange : TMeasureRangeBase;
  End;

  { TRangesList }

  TRangesList = class
  public
    type TRangeInfoList = specialize TFPGObjectList<TRangeInfo>;
    var
    FRangeInfos : TRangeInfoList;
    Constructor Create;
    Procedure Add(ARangeInfo:TRangeInfo);
    Procedure Sort;
  End;

{$ENDIF} // CreateMeasureDefinitionFirstAttempt

Procedure SetupInstrumentWrapperFactory;
Procedure SetupMeasurementResultFactory;
Procedure SetupObjectFactory;
Procedure FreeInstrumentWrapperFactory;
Procedure FreeMeasurementResultFactory;
Procedure FreeObjectFactory;

Var
  InstrumentWrapperFactory : TInstrumentWrapperFactory;
  MeasurementResultFactory : TMeasurementResultFactory;
  ObjectFactory            : TObjectFactory;

Implementation
Uses yacclib, lexlib;

{ TFluke177 }

Class Function TFluke177.GetRanges(AInstrument : String) : TRangesQuantity;
Begin
  Case AInstrument of
    'Fluke177' : Begin
      SetLength(Result[qtDCV], 5);
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(   0.6, true, 100E-6); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.09*0.01, 2*100E-6));
      Result[qtDCV][1] := TMeasureRangeAccuracy.Create(   6.0, true,   1E-3); Result[qtDCV][1].AddAccuracy(TAccuracyGainOffset.Create(0.09*0.01, 2*  1E-3));
      Result[qtDCV][2] := TMeasureRangeAccuracy.Create(  60.0, true,  10E-3); Result[qtDCV][2].AddAccuracy(TAccuracyGainOffset.Create(0.09*0.01, 2* 10E-3));
      Result[qtDCV][3] := TMeasureRangeAccuracy.Create( 600.0, true, 100E-3); Result[qtDCV][3].AddAccuracy(TAccuracyGainOffset.Create(0.09*0.01, 2*100E-3));
      Result[qtDCV][4] := TMeasureRangeAccuracy.Create(1000.0, true,   1E-0); Result[qtDCV][4].AddAccuracy(TAccuracyGainOffset.Create(0.15*0.01, 2*  1E-0));
      WriteLn('Warning: TODO: Implement for other accuracy cases and for other quantities');
    End;
  Else
    raise Exception.Create('TODO: Implement instrument '''+AInstrument+'''');
  End;
End;

{ TKeithleyDMM6500 }

Class Function TKeithleyDMM6500.GetRanges(AInstrument : String) : TRangesQuantity;
Begin
  Case AInstrument of
    'KeithleyDMM6500' : Begin
      SetLength(Result[qtDCV], 5);          // 1 year within calibration
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(   0.1, true, 100E-9); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.0030*0.01, 0.0035*0.01*   0.1));
      Result[qtDCV][1] := TMeasureRangeAccuracy.Create(   1.0, true,   1E-6); Result[qtDCV][1].AddAccuracy(TAccuracyGainOffset.Create(0.0025*0.01, 0.0006*0.01*   1.0));
      Result[qtDCV][2] := TMeasureRangeAccuracy.Create(  10.0, true,  10E-6); Result[qtDCV][2].AddAccuracy(TAccuracyGainOffset.Create(0.0025*0.01, 0.0005*0.01*  10.0));
      Result[qtDCV][3] := TMeasureRangeAccuracy.Create( 100.0, true, 100E-6); Result[qtDCV][3].AddAccuracy(TAccuracyGainOffset.Create(0.0040*0.01, 0.0006*0.01* 100.0));
      Result[qtDCV][4] := TMeasureRangeAccuracy.Create(1000.0, true,   1E-3); Result[qtDCV][4].AddAccuracy(TAccuracyGainOffset.Create(0.0040*0.01, 0.0006*0.01*1000.0));
      WriteLn('Warning: TODO: Implement for other accuracy cases and for other quantities');
    End;
  Else
    raise Exception.Create('TODO: Implement instrument '''+AInstrument+'''');
  End;
End;

{ TKeithley2450 }

Class Function TKeithley2450.GetRanges(AInstrument : String) : TRangesQuantity;
Begin
  Case AInstrument of
    'Keithley2450:S' : Begin
      SetLength(Result[qtDCV], 5);          // 1 year within calibration
      // source resolution and accuracy
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(  0.02, true, 500E-9); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.100*0.01, 200E-6));
      Result[qtDCV][1] := TMeasureRangeAccuracy.Create(  0.2,  true,   5E-6); Result[qtDCV][1].AddAccuracy(TAccuracyGainOffset.Create(0.015*0.01, 200E-6));
      Result[qtDCV][2] := TMeasureRangeAccuracy.Create(  2.0,  true,  50E-6); Result[qtDCV][2].AddAccuracy(TAccuracyGainOffset.Create(0.020*0.01, 300E-6));
      Result[qtDCV][3] := TMeasureRangeAccuracy.Create( 20.0,  true, 500E-6); Result[qtDCV][3].AddAccuracy(TAccuracyGainOffset.Create(0.015*0.01, 2.4E-3));
      Result[qtDCV][4] := TMeasureRangeAccuracy.Create(200.0,  true,   5E-3); Result[qtDCV][4].AddAccuracy(TAccuracyGainOffset.Create(0.015*0.01,  24E-3));
      WriteLn('Warning: TODO: Implement for other accuracy cases and for other quantities');
    End;
    'Keithley2450:M' : Begin
      SetLength(Result[qtDCV], 5);          // 1 year within calibration
      // measure resolution and accuracy
      Result[qtDCV][0] := TMeasureRangeAccuracy.Create(  0.02, true,  10E-9); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(0.100*0.01, 150E-6));
      Result[qtDCV][1] := TMeasureRangeAccuracy.Create(  0.2,  true, 100E-9); Result[qtDCV][1].AddAccuracy(TAccuracyGainOffset.Create(0.012*0.01, 200E-6));
      Result[qtDCV][2] := TMeasureRangeAccuracy.Create(  2.0,  true,   1E-6); Result[qtDCV][2].AddAccuracy(TAccuracyGainOffset.Create(0.012*0.01, 300E-6));
      Result[qtDCV][3] := TMeasureRangeAccuracy.Create( 20.0,  true,  10E-6); Result[qtDCV][3].AddAccuracy(TAccuracyGainOffset.Create(0.015*0.01,   1E-3));
      Result[qtDCV][4] := TMeasureRangeAccuracy.Create(200.0,  true, 100E-6); Result[qtDCV][4].AddAccuracy(TAccuracyGainOffset.Create(0.015*0.01,  10E-3));
      WriteLn('Warning: TODO: Implement for other accuracy cases and for other quantities');
    End;
  Else
    raise Exception.Create('TODO: Implement instrument '''+AInstrument+'''');
  End;
End;


(*
{ TKeysightMSOX3024A }

Constructor TKeysightMSOX3024A.Create;
Begin
  inherited Create(ifMeasure);
//  SetLength(Result[qtDCV], 5);
//  // TODO: gain is relative to full scale, not to the measured value
//  Result[qtDCV][0] := TMeasureRangeAccuracy.Create(   0.1, true, 100E-9); Result[qtDCV][0].AddAccuracy(TAccuracyGainOffset.Create(2.0   *0.01, 0.0035*0.01*   0.1));
//  Result[qtDCV][1] := TMeasureRangeAccuracy.Create(   1.0, true,   1E-6); Result[qtDCV][1].AddAccuracy(TAccuracyGainOffset.Create(0.0040*0.01, 0.0007*0.01*   1.0));
//  Result[qtDCV][2] := TMeasureRangeAccuracy.Create(  10.0, true,  10E-6); Result[qtDCV][2].AddAccuracy(TAccuracyGainOffset.Create(0.0035*0.01, 0.0005*0.01*  10.0));
//  Result[qtDCV][3] := TMeasureRangeAccuracy.Create( 100.0, true, 100E-6); Result[qtDCV][3].AddAccuracy(TAccuracyGainOffset.Create(0.0045*0.01, 0.0006*0.01* 100.0));
//  Result[qtDCV][4] := TMeasureRangeAccuracy.Create(1000.0, true,   1E-3); Result[qtDCV][4].AddAccuracy(TAccuracyGainOffset.Create(0.0045*0.01, 0.0010*0.01*1000.0));
End;

*)

{ TTestPoints }

Constructor TTestPoints.Create;
Begin
  inherited Create;
  SetLength(FValues, 0);
End;

Constructor TTestPoints.Create(AValues : TDynDoubleArray);
Begin
  inherited Create;
  FValues := AValues;
End;

Procedure TTestPoints.AddPoint(AValue : Double);
Begin
  SetLength(FValues, Length(FValues)+1);
  FValues[Length(FValues)-1] := AValue;
End;

Procedure TTestPoints.Sort;
Var D : Double;
    I : Integer;
    R : TValueRelationship;
    S : Boolean;
Begin
  // simple bubble-sort with removal of same values
  repeat
    S := False;
    I := 0;
    While I < Length(FValues)-1 do
      Begin
        R := CompareValue(FValues[I], FValues[I+1], Min(abs(FValues[I]),abs(FValues[I+1]))*1E-6);
        // Above we set a dedicated delta value, because without, it would not
        // recognize 2.9999999999999999E-001 and 3.0000000000000004E-001 as the same.
        WriteLn('Comparing ',FValues[I], FValues[I+1],' --> ',R);
        if R = EqualsValue then
          Begin
            if I < Length(FValues)-1 then
              Move(FValues[I+2], FValues[I+1], SizeOf(FValues[0])*(Length(FValues)-I-2));
            SetLength(FValues, Length(FValues)-1);
            // don't increment I for the next round
          End
        else if R = GreaterThanValue then
          Begin
            D            := FValues[I];
            FValues[I]   := FValues[I+1];
            FValues[I+1] := D;
            S := True;
            Inc(I);
          End
        else
          Inc(I);
      End;
  Until S = False;
End;

Procedure TTestPoints.AddPoints(ARange : TMeasureRangeBase; ANumLinPoints:Integer);
Var I : Integer;
    D : Double;
Begin
  // distribute points linearly in range, this is useful if the ranges are scaled by e.g. x10
  For I := 1 to ANumLinPoints do
    AddPoint((ARange.FMaxValue / ANumLinPoints) * (I*1.0));
  // add points below current smallest from 10x resolution, 100x, ...
  D := 10.0*ARange.FResolution;
  while D < FValues[0] do
    Begin
      AddPoint(D);
      D := D * 10;
    End;
  // add 0.0
  AddPoint(0.0);
  Sort;
End;

Procedure TTestPoints.AddPoints(ATestPoints : TTestPoints);
Begin
  SetLength(FValues, Length(FValues) + Length(ATestPoints.FValues));
  Move(ATestPoints.FValues[0], FValues[Length(FValues) - Length(ATestPoints.FValues)], SizeOf(FValues[0])*Length(ATestPoints.FValues));
End;

Function TTestPoints.Range(AFrom, ATo : Double) : TDynDoubleArray;
Var I : Integer;
Begin
  SetLength(Result, 0);
  For I := 0 to Length(FValues)-1 do
    if (CompareValue(FValues[I], AFrom) in [EqualsValue, GreaterThanValue]) and
       (Byte(CompareValue(FValues[I], ATo))   in [EqualsValue, Byte(LessThanValue)]) then
      Begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := FValues[I];
      End;
End;

Function TTestPoints.RangeTo(ATo : Double) : TDynDoubleArray;
Var I : Integer;
Begin
  SetLength(Result, 0);
  For I := 0 to Length(FValues)-1 do
    if Byte(CompareValue(FValues[I], ATo)) in [EqualsValue, Byte(LessThanValue)] then
      Begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := FValues[I];
      End;
End;

Function TTestPoints.ToString : String;
Var I : Integer;
Begin
  Result := '';
  For I := 0 to Length(FValues)-1 do
    Result := Result + FloatToStr(FValues[I]) + ' ';
  SetLength(Result, Length(Result)-1);
End;

{ TParamValueGeneric }

Constructor TParamValueGeneric.Create(AValue : T);
Begin
  inherited Create;
  FValue := AValue;
End;

{ TParamValueInteger }

Function TParamValueInteger.ToSyntax : String;
Begin
  Result := IntToStr(FValue);
End;

{ TParamValueDouble }

Function TParamValueDouble.ToSyntax : String;
Begin
  Result := FloatToStr(FValue);
End;

{ TParamValueString }

Function TParamValueString.ToSyntax : String;
Begin
  Result := ''''+FValue+'''';
End;

{ TParamValueIdentifier }

Function TParamValueIdentifier.ToSyntax : String;
Begin
  Result := FValue;
End;

{ TParamValueFunction }

Constructor TParamValueFunction.Create(AValue : TInstrumentFunction);
Begin
  inherited Create(AValue);
End;

Constructor TParamValueFunction.Create(AValue : String);
Begin
  if      AValue = 'Measure' then FValue := ifMeasure
  else if AValue = 'Source'  then FValue := ifSource
  else
    raise Exception.Create('Invalid value '''+AValue+''' for TParamValueFunction');
  inherited Create(FValue);
End;

Function TParamValueFunction.ToSyntax : String;
Begin
  Result := IfThen(FValue=ifMeasure,'Measure','Source');
End;

{ TInstrumentWrapperParams }

Constructor TInstrumentWrapperParams.Create;
Begin
  inherited Create;
  FParams := TParamsMap.Create(True);
  FParams.Sorted := True;
  FParams.Duplicates := dupError;
End;

Constructor TInstrumentWrapperParams.Create(AKey : String; AValue : TParamValueBase);
Begin
  // when calling TInstrumentWrapperParams.Create here, then afterwards FParams is Nil again
  inherited Create;
  FParams := TParamsMap.Create(True);
  FParams.Add(AKey, AValue);
End;

Constructor TInstrumentWrapperParams.Create(Args : Array Of Const);
Var I : Integer;
    K : String;
    V : TParamValueBase;
Begin
  if Length(Args) and $0001 <> 0 then
    raise Exception.Create('TInstrumentWrapperParams.Create called with odd number of parameters');
  inherited Create;
  FParams := TParamsMap.Create(True);
  I := 0;
  While I < Length(Args) do
    Begin
      if      Args[I].VType = vtString then
        K := Args[I].VString^
      else if Args[I].VType = vtAnsiString then
        K := AnsiString(Args[I].VAnsiString)
      else
        raise Exception.Create('Argument '+IntToStr(I)+' is not a string');
      Inc(I);
      if Args[I].VType <> vtObject then
        raise Exception.Create('Argument '+IntToStr(I)+' is not an object');
      if not (Args[I].VObject is TParamValueBase) then
        raise Exception.Create('Argument '+IntToStr(I)+' is not a descendent of TParamValueBase');
      V := TParamValueBase(Args[I].VObject);
      Inc(I);
      FParams.Add(K, V);
    End;
End;

Destructor TInstrumentWrapperParams.Destroy;
Begin
  FParams.Free;
  Inherited Destroy;
End;

Function TInstrumentWrapperParams.Get(AKey : String) : TParamValueBase;
Var I : Integer;
Begin
  if not FParams.Find(AKey, I) then
    raise Exception.Create('Parameter '''+AKey+''' not defined');
  Result := FParams.Data[I];
End;

Function TInstrumentWrapperParams.GetInteger(AKey : String) : Integer;
Var V : TParamValueBase;
Begin
  V := Get(AKey);
  if not (V is TParamValueInteger) then
    raise Exception.Create('Parameter '''+AKey+''' should be an integer, but is '+V.ClassName);
  Result := TParamValueInteger(V).FValue;
End;

Function TInstrumentWrapperParams.GetDouble(AKey : String) : Double;
Var V : TParamValueBase;
Begin
  V := Get(AKey);
  if not (V is TParamValueDouble) then
    raise Exception.Create('Parameter '''+AKey+''' should be a double, but is '+V.ClassName);
  Result := TParamValueDouble(V).FValue;
End;

Function TInstrumentWrapperParams.GetString(AKey : String) : String;
Var V : TParamValueBase;
Begin
  V := Get(AKey);
  if not (V is TParamValueString) then
    raise Exception.Create('Parameter '''+AKey+''' should be a string, but is '+V.ClassName);
  Result := TParamValueString(V).FValue;
End;

Function TInstrumentWrapperParams.GetFunction(AKey : String) : TInstrumentFunction;
Var V : TParamValueBase;
Begin
  V := Get(AKey);
  if not (V is TParamValueFunction) then
    raise Exception.Create('Parameter '''+AKey+''' should be an instrument function, but is '+V.ClassName);
  Result := TParamValueFunction(V).FValue;
End;

Procedure TInstrumentWrapperParams.SetInteger(AKey : String; AValue : Integer);
Var V : TParamValueBase;
Begin
  V := Get(AKey);
  if not (V is TParamValueInteger) then
    raise Exception.Create('Parameter '''+AKey+''' should be an integer, but is '+V.ClassName);
  TParamValueInteger(V).FValue := AValue;
End;

Procedure TInstrumentWrapperParams.SetDouble(AKey : String; AValue : Double);
Var V : TParamValueBase;
Begin
  V := Get(AKey);
  if not (V is TParamValueDouble) then
    raise Exception.Create('Parameter '''+AKey+''' should be a double, but is '+V.ClassName);
  TParamValueDouble(V).FValue := AValue;
End;

Procedure TInstrumentWrapperParams.SetString(AKey : String; AValue : String);
Var V : TParamValueBase;
Begin
  V := Get(AKey);
  if not (V is TParamValueString) then
    raise Exception.Create('Parameter '''+AKey+''' should be a string, but is '+V.ClassName);
  TParamValueString(V).FValue := AValue;
End;

Procedure TInstrumentWrapperParams.SetFunction(AKey : String; AValue : TInstrumentFunction);
Var V : TParamValueBase;
Begin
  V := Get(AKey);
  if not (V is TParamValueFunction) then
    raise Exception.Create('Parameter '''+AKey+''' should be an instrument function, but is '+V.ClassName);
  TParamValueFunction(V).FValue := AValue;
End;

Procedure TInstrumentWrapperParams.SetOrAddInteger(AKey : String;AValue : Integer);
Var I : Integer;
    V : TParamValueBase;
Begin
  if FParams.Find(AKey, I) then
    Begin
      V := FParams.Data[I];
      if not (V is TParamValueInteger) then
        raise Exception.Create('Parameter '''+AKey+''' should be an integer, but is '+V.ClassName);
      TParamValueInteger(V).FValue := AValue;
    End
  else
    Begin
      V := TParamValueInteger.Create(AValue);
      FParams.Add(AKey, V);
    End;
End;

Procedure TInstrumentWrapperParams.SetOrAddDouble(AKey : String; AValue : Double);
Var I : Integer;
    V : TParamValueBase;
Begin
  if FParams.Find(AKey, I) then
    Begin
      V := FParams.Data[I];
      if not (V is TParamValueDouble) then
        raise Exception.Create('Parameter '''+AKey+''' should be a double, but is '+V.ClassName);
      TParamValueDouble(V).FValue := AValue;
    End
  else
    Begin
      V := TParamValueDouble.Create(AValue);
      FParams.Add(AKey, V);
    End;
End;

Procedure TInstrumentWrapperParams.SetOrAddString(AKey : String; AValue : String);
Var I : Integer;
    V : TParamValueBase;
Begin
  if FParams.Find(AKey, I) then
    Begin
      V := FParams.Data[I];
      if not (V is TParamValueString) then
        raise Exception.Create('Parameter '''+AKey+''' should be a string, but is '+V.ClassName);
      TParamValueString(V).FValue := AValue;
    End
  else
    Begin
      V := TParamValueString.Create(AValue);
      FParams.Add(AKey, V);
    End;
End;

Procedure TInstrumentWrapperParams.SetOrAddFunction(AKey : String;AValue : TInstrumentFunction);
Var I : Integer;
    V : TParamValueBase;
Begin
  if FParams.Find(AKey, I) then
    Begin
      V := FParams.Data[I];
      if not (V is TParamValueFunction) then
        raise Exception.Create('Parameter '''+AKey+''' should be an instrument function, but is '+V.ClassName);
      TParamValueFunction(V).FValue := AValue;
    End
  else
    Begin
      V := TParamValueFunction.Create(AValue);
      FParams.Add(AKey, V);
    End;
End;

Function TInstrumentWrapperParams.ToSyntax : String;
Var SL : TStringList;
    I  : Integer;
Begin
  SL := TStringList.Create;
  For I := 0 to FParams.Count-1 do
    SL.Add(FParams.Keys[I]+'='+FParams.Data[I].ToSyntax);
  SL.Delimiter := ',';
  Result := SL.DelimitedText; //'Function='+;
  SL.Free;
End;

{ TInstrumentWrapperFactory }

Constructor TInstrumentWrapperFactory.Create;
Begin
  inherited Create;
  FRegistry := TInstrumentWrapperRegistry.Create;
  FRegistry.Duplicates := dupError;
  FRegistry.Sorted     := True;
End;

Destructor TInstrumentWrapperFactory.Destroy;
Begin
  FRegistry.Free;
  Inherited Destroy;
End;

Procedure TInstrumentWrapperFactory.RegisterWrapper(AWrapperName : String; AClass : TInstrumentWrapperClass);
Begin
  FRegistry.Add(AWrapperName, AClass);
End;

Function TInstrumentWrapperFactory.CreateWrapper(AComparisonBase : TComparisonBase; AWrapperName, AName : String; AParams : TInstrumentWrapperParams) : TInstrumentWrapperBase;
Var BClass : TInstrumentWrapperClass;
    I      : Integer;
Begin
  if not FRegistry.Find(AWrapperName, I) then
    raise Exception.Create('No wrapper '''+AWrapperName+''' available');
  BClass := FRegistry[AWrapperName];
  Result := BClass.CreateFromParams(AComparisonBase, AWrapperName, AName, AParams);
End;

Procedure SetupInstrumentWrapperFactory;
Begin
  if assigned(InstrumentWrapperFactory) then Exit;
  InstrumentWrapperFactory := TInstrumentWrapperFactory.Create;
  TInstrumentWrapperFluke177.       RegisterWrapper(InstrumentWrapperFactory);
  TInstrumentWrapperKeysightU125xB. RegisterWrapper(InstrumentWrapperFactory);
  TInstrumentWrapperAgilent34410A.  RegisterWrapper(InstrumentWrapperFactory);
  TInstrumentWrapperKeithleyDMM6500.RegisterWrapper(InstrumentWrapperFactory);
  TInstrumentWrapperKeithley2450.   RegisterWrapper(InstrumentWrapperFactory);
  TInstrumentWrapperKeysightE3631xA.RegisterWrapper(InstrumentWrapperFactory);
End;

Procedure FreeInstrumentWrapperFactory;
Begin
  FreeAndNil(InstrumentWrapperFactory);
End;

{ TInstrumentWrapperBase }

Constructor TInstrumentWrapperBase.Create(AComparisonBase : TComparisonBase; AWrapperName, AName : String; AParams : TInstrumentWrapperParams; AFunction : TInstrumentFunction);
Begin
  inherited Create;
  FComparisonBase := AComparisonBase;
  FWrapperName    := AWrapperName;
  FName           := AName;
  FParams         := AParams;
  FFunction       := AFunction;
End;

Destructor TInstrumentWrapperBase.Destroy;
Var NP : Integer;
    NQ : TQuantity;
Begin
  if Length(FTestPoints) > 0 then
    For NP := 0 to Length(FTestPoints)-1 do
      FTestPoints[NP].Free;
  FParams.Free;
  For NQ := Low(TQuantity) to High(TQuantity) do
    For NP := 0 to Length(FRanges[NQ])-1 do
      FRanges[NQ][NP].Free;
  inherited Destroy;
End;

Procedure TInstrumentWrapperBase.CreateTestPoints(ANumLinPoints : Integer);
Var NR : Integer;
Begin
  if Length(FRanges[FComparisonBase.FQuantity]) = 0 then
    raise Exception.Create('FRanges must be set before CreateTestPoints with SetupRanges, or the instrument doesn''t support the quantity '+CQuantityStr[FComparisonBase.FQuantity]);
  // first free all existing testpoints
  if Length(FTestPoints) > 0 then
    For NR := 0 to Length(FTestPoints)-1 do
      FTestPoints[NR].Free;
  // create new testpoints
  SetLength(FTestPoints,Length(FRanges[FComparisonBase.FQuantity]));
  For NR := 0 to Length(FRanges[FComparisonBase.FQuantity])-1 do
    Begin
      FTestPoints[NR] := TTestPoints.Create;
      FTestPoints[NR].AddPoints(FRanges[FComparisonBase.FQuantity][NR], ANumLinPoints);
      if NR > 0 then
        Begin
          // add maximum of previous range
          FTestPoints[NR].AddPoint(FRanges[FComparisonBase.FQuantity][NR-1].FMaxValue);
          FTestPoints[NR].Sort;
        End;
      WriteLn('Added testpoints to ',FName,' ',FloatToStr(FRanges[FComparisonBase.FQuantity][NR].FMaxValue),': ', FTestPoints[NR].ToString);
    End;
End;

{ TInstrumentWrapperFluke177 }

Class Procedure TInstrumentWrapperFluke177.RegisterWrapper(AFactory : TInstrumentWrapperFactory);
Begin
  AFactory.RegisterWrapper('Fluke177', TInstrumentWrapperFluke177);
End;

Class Function TInstrumentWrapperFluke177.CreateFromParams(AComparisonBase : TComparisonBase; AWrapperName, AName : String; AParams : TInstrumentWrapperParams) : TInstrumentWrapperBase;
Begin
  Result := TInstrumentWrapperFluke177.Create(AComparisonBase, AWrapperName, AName, AParams, ifMeasure);
End;

Function TInstrumentWrapperFluke177.GetParams : TInstrumentWrapperParams;
Begin
  if not assigned(FParams) then
    FParams := TInstrumentWrapperParams.Create;
  // update TODO: FParams.FFunction := FFunction;
  Result := FParams;
End;

Procedure TInstrumentWrapperFluke177.SetupRanges;
Begin
  FRanges := TFluke177.GetRanges(FWrapperName);
End;

Procedure TInstrumentWrapperFluke177.Initialize;
Begin
  WriteLn(FName+'.Initialize: Please set to ',CQuantityStr[FComparisonBase.FQuantity],', enter instrument identifier (serial number),');
  Write('and confirm with Enter. ');
  ReadLn(FIdentifier);
End;

Procedure TInstrumentWrapperFluke177.Disconnect;
Begin
  WriteLn(FName+'.Disconnect');
End;

Procedure TInstrumentWrapperFluke177.SetMeasureRange(ARange : TMeasureRangeBase);
Begin
  Write(FName+'.SetMeasureRange('+FloatToStr(ARange.FMaxValue)+'): Please set the range and confirm with Enter. ');
  ReadLn;
End;

Procedure TInstrumentWrapperFluke177.StartMeasurement;
Begin
  WriteLn(FName+'.StartMeasurement');
End;

Function TInstrumentWrapperFluke177.GetResults : TMeasurementResultBase;
Var D : Double;
Begin
  Write(FName+'.GetResults: Please enter the value and press Enter: ');
  ReadLn(D);
  Result := TMeasurementResultBase.Create(FRange.FAccuracy[0].Apply(D));
  WriteLn('  ',Result.ToString);
End;

{ TInstrumentWrapperKeysightU125xB }

Class Procedure TInstrumentWrapperKeysightU125xB.RegisterWrapper(AFactory : TInstrumentWrapperFactory);
Begin
  AFactory.RegisterWrapper('KeysightU1251B', TInstrumentWrapperKeysightU125xB);
  AFactory.RegisterWrapper('KeysightU1252B', TInstrumentWrapperKeysightU125xB);
  AFactory.RegisterWrapper('KeysightU1253B', TInstrumentWrapperKeysightU125xB);
End;

Class Function TInstrumentWrapperKeysightU125xB.CreateFromParams(AComparisonBase : TComparisonBase; AWrapperName, AName : String; AParams : TInstrumentWrapperParams) : TInstrumentWrapperBase;
Begin
  Result := TInstrumentWrapperKeysightU125xB.Create(AComparisonBase, AWrapperName, AName, AParams, ifMeasure);
  TInstrumentWrapperKeysightU125xB(Result).FDevice := AParams.GetString('TTY');
End;

Function TInstrumentWrapperKeysightU125xB.GetParams : TInstrumentWrapperParams;
Begin
  if not assigned(FParams) then
    FParams := TInstrumentWrapperParams.Create;
  // update
  FParams.SetOrAddString('TTY', FDevice);
  Result := FParams;
End;

Procedure TInstrumentWrapperKeysightU125xB.SetupRanges;
Begin
  FRanges := TKeysightU125xB.GetRanges(FWrapperName);
End;

Procedure TInstrumentWrapperKeysightU125xB.Initialize;
Var MeasureStatus : TMeasureStatus;
Begin
  WriteLn(FName+'.Initialize');
  // device connector via USB to IR
  FComm := TRS232Communicator.Create(FDevice,9600,8,NoneParity,0,[]);
  FComm.SetTimeout(10000000);

  FInstrument := TKeysightU125xB.Create(FComm);
  WriteLn('Connected to device ',FInstrument.Identify);
  FIdentifier := FInstrument.Identify;

  MeasureStatus := FInstrument.GetMeasureStatus;
  WriteLn(MeasureStatus.ToString);
  WriteLn('Battery level: ',FInstrument.GetBatteryLevel:1:2,'%');
  WriteLn('Version: ',FInstrument.GetVersion);
  WriteLn('Last error: ',FInstrument.GetError);
End;

Procedure TInstrumentWrapperKeysightU125xB.Disconnect;
Begin
  WriteLn(FName+'.Disconnect');
  FreeAndNil(FInstrument);
  FreeAndNil(FComm);
End;

Procedure TInstrumentWrapperKeysightU125xB.SetMeasureRange(ARange : TMeasureRangeBase);
Var MeasureStatus : TMeasureStatus;
    RotarySwitch  : TRotarySwitch;
    Quantity      : TMeasureQuantity;
    Coupling      : TMeasureCoupling;
Begin
  WriteLn(FName+'.SetMeasureRange('+FloatToStr(ARange.FMaxValue)+')');
  FRange := ARange as TMeasureRangeAccuracy;
  // define which rotary switch position is required
  if FComparisonBase.FQuantity = qtDCV then
    Begin
      Quantity := mqVoltage;
      Coupling := mcDC;
      if ARange.FMaxValue > 2.0 then   // 2.0 as threshold is strongly simplified here
        RotarySwitch := rsVDCAC       // rsVDCAC has ranges 5, 50, 500, 1000V
      else
        RotarySwitch := rsmVDCAC;     // rsmVDCAC has ranges 50mV, 500mV, 1000mV
    End
  else
    raise Exception.Create('TODO: Implement Quantity '+CQuantityStr[FComparisonBase.FQuantity]);
  // first request the switch to be set correctly
  While true do
    Begin
      MeasureStatus := FInstrument.GetMeasureStatus;
      if MeasureStatus.RotarySwitch = RotarySwitch then Break;
      WriteLn('WARNING: Set the rotary switch to ',CRotarySwitch[RotarySwitch],' and plug in the correct terminal');
      WriteLn('Then press Enter');
      ReadLn;
    End;
  // finally set the range
  FInstrument.SetMeasureConfig(Quantity, Coupling, ARange.FMaxValue);
End;

Procedure TInstrumentWrapperKeysightU125xB.StartMeasurement;
Begin
  WriteLn(FName+'.StartMeasurement');
End;

Function TInstrumentWrapperKeysightU125xB.GetResults : TMeasurementResultBase;
Var D : Double;
Begin
  WriteLn(FName+'.GetResults');
  D := FInstrument.Measure;
  WriteLn('  V = ',FloatToStr(D));
  Result := TMeasurementResultBase.Create(FRange.FAccuracy[0].Apply(D));
  WriteLn('  ',Result.ToString);
End;

{ TInstrumentWrapperAgilent34410A }

Class Procedure TInstrumentWrapperAgilent34410A.RegisterWrapper(AFactory : TInstrumentWrapperFactory);
Begin
  AFactory.RegisterWrapper('Agilent34410A', TInstrumentWrapperAgilent34410A);
  AFactory.RegisterWrapper('Agilent34411A', TInstrumentWrapperAgilent34410A);
  AFactory.RegisterWrapper('AgilentL4411A', TInstrumentWrapperAgilent34410A);
  AFactory.RegisterWrapper('Agilent34460A', TInstrumentWrapperAgilent34410A);
  AFactory.RegisterWrapper('Agilent34461A', TInstrumentWrapperAgilent34410A);
  AFactory.RegisterWrapper('Agilent34465A', TInstrumentWrapperAgilent34410A);
  AFactory.RegisterWrapper('Agilent34470A', TInstrumentWrapperAgilent34410A);
End;

Class Function TInstrumentWrapperAgilent34410A.CreateFromParams(AComparisonBase : TComparisonBase; AWrapperName, AName : String; AParams : TInstrumentWrapperParams) : TInstrumentWrapperBase;
Var St : String;
    I  : Integer;
Begin
  Result := TInstrumentWrapperAgilent34410A.Create(AComparisonBase, AWrapperName, AName, AParams, ifMeasure);
  With TInstrumentWrapperAgilent34410A(Result) do
    Begin
      FVISA := AParams.GetString('VISA');
    End
End;

Function TInstrumentWrapperAgilent34410A.GetParams : TInstrumentWrapperParams;
Begin
  if not assigned(FParams) then
    FParams := TInstrumentWrapperParams.Create;
  FParams.SetOrAddString('VISA',   FVISA);
  Result := FParams;
End;

Procedure TInstrumentWrapperAgilent34410A.SetupRanges;
Begin
  FRanges := TAgilent34410A.GetRanges(FWrapperName);
End;

Procedure TInstrumentWrapperAgilent34410A.Initialize;
Begin
  WriteLn(FName+'.Initialize');
  FComm       := DevComOpen(FVISA);
  FInstrument := TAgilent34410A.Create(FComm);
  FComm.SetTimeout(2000000{us});
  //FComm.ErrorHandler := @USBTMCErrorHandler;
  // TODO: like in Create, check that the device is the same type as FWrapperName
  WriteLn('Connected to device ',FInstrument.Identify);
  FIdentifier := FInstrument.Identify;
  WriteLn('Reset to default settings');
  FInstrument.Reset;
  WriteLn('Disable the beeper');
  FInstrument.SetBeeper(false);
  // print input terminals setting
  WriteLn(FName,': Input terminal selection: ',CInputTerminalsSettingNice[FInstrument.GetInputTerminalsSetting]);
  // setup measurement to DC Volts
  if FComparisonBase.FQuantity = qtDCV then
    Begin
      FInstrument.SetSenseFunction(qtVoltageDC);
    End
  else
    raise Exception.Create('TODO: Implement Quantity '+CQuantityStr[FComparisonBase.FQuantity]);
  FInstrument.SetNPLC(1.0);
  FInstrument.AutoZero;
  // don't wait for any trigger condition
  FInstrument.SetTriggerSource(tsImmediate);
End;

Procedure TInstrumentWrapperAgilent34410A.Disconnect;
Begin
  WriteLn(FName+'.Disconnect');
  FreeAndNil(FInstrument);
//  TObject(FComm).Free;     // TODO: is it ok that FComm is not Free()d?
End;

Procedure TInstrumentWrapperAgilent34410A.SetMeasureRange(ARange : TMeasureRangeBase);
Begin
  WriteLn(FName+'.SetMeasureRange('+FloatToStr(ARange.FMaxValue)+')');
  FRange := ARange as TMeasureRangeAccuracy;
  FInstrument.SetRange(FRange.FMaxValue);
End;

Procedure TInstrumentWrapperAgilent34410A.StartMeasurement;
Begin
  WriteLn(FName+'.StartMeasurement');
End;

Function TInstrumentWrapperAgilent34410A.GetResults : TMeasurementResultBase;
Var D : Double;
Begin
  WriteLn(FName+'.GetResults');
  D := FInstrument.GetValue;
  WriteLn('  V = ',FloatToStr(D));
  Result := TMeasurementResultBase.Create(FRange.FAccuracy[0].Apply(D));
  WriteLn('  ',Result.ToString);
End;

{ TInstrumentWrapperKeithleyDMM6500 }

Class Procedure TInstrumentWrapperKeithleyDMM6500.RegisterWrapper(AFactory : TInstrumentWrapperFactory);
Begin
  AFactory.RegisterWrapper('KeithleyDMM6500', TInstrumentWrapperKeithleyDMM6500);
End;

Class Function TInstrumentWrapperKeithleyDMM6500.CreateFromParams(AComparisonBase : TComparisonBase; AWrapperName, AName : String; AParams : TInstrumentWrapperParams) : TInstrumentWrapperBase;
Begin
  Result := TInstrumentWrapperKeithleyDMM6500.Create(AComparisonBase, AWrapperName, AName, AParams, ifMeasure);
End;

Function TInstrumentWrapperKeithleyDMM6500.GetParams : TInstrumentWrapperParams;
Begin
  if not assigned(FParams) then
    FParams := TInstrumentWrapperParams.Create;
  // update TODO: FParams.FFunction := FFunction;
  Result := FParams;
End;

Procedure TInstrumentWrapperKeithleyDMM6500.SetupRanges;
Begin
  FRanges := TKeithleyDMM6500.GetRanges(FWrapperName);
End;

Procedure TInstrumentWrapperKeithleyDMM6500.Initialize;
Begin
  WriteLn(FName+'.Initialize');
  //FIdentifier := FInstrument.Identify;
End;

Procedure TInstrumentWrapperKeithleyDMM6500.Disconnect;
Begin
  WriteLn(FName+'.Disconnect');
End;

Procedure TInstrumentWrapperKeithleyDMM6500.SetMeasureRange(ARange : TMeasureRangeBase);
Begin
  WriteLn(FName+'.SetMeasureRange('+FloatToStr(ARange.FMaxValue)+')');
End;

Procedure TInstrumentWrapperKeithleyDMM6500.StartMeasurement;
Begin
  WriteLn(FName+'.StartMeasurement');
End;

Function TInstrumentWrapperKeithleyDMM6500.GetResults : TMeasurementResultBase;
Begin
  WriteLn(FName+'.GetResults');
End;

{ TInstrumentWrapperKeithley2450 }

Class Procedure TInstrumentWrapperKeithley2450.RegisterWrapper(AFactory : TInstrumentWrapperFactory);
Begin
  AFactory.RegisterWrapper('Keithley2450', TInstrumentWrapperKeithley2450);
End;

Class Function TInstrumentWrapperKeithley2450.CreateFromParams(AComparisonBase : TComparisonBase; AWrapperName, AName : String; AParams : TInstrumentWrapperParams) : TInstrumentWrapperBase;
Begin
  Result := TInstrumentWrapperKeithley2450.Create(AComparisonBase, AWrapperName, AName, AParams, AParams.GetFunction('Function'));
End;

Function TInstrumentWrapperKeithley2450.GetParams : TInstrumentWrapperParams;
Begin
  if not assigned(FParams) then
    FParams := TInstrumentWrapperParams.Create;
  // update
  FParams.SetOrAddFunction('Function', FFunction);
  Result := FParams;
End;

Procedure TInstrumentWrapperKeithley2450.SetupRanges;
Begin
  FRanges := TKeithley2450.GetRanges(FWrapperName+':'+IfThen(FFunction = ifMeasure, 'M', 'S'));
End;

Procedure TInstrumentWrapperKeithley2450.Initialize;
Begin
  WriteLn(FName+'.Initialize');
  //FIdentifier := FInstrument.Identify;
End;

Procedure TInstrumentWrapperKeithley2450.Disconnect;
Begin
  WriteLn(FName+'.Disconnect');
End;

Procedure TInstrumentWrapperKeithley2450.SetSourceRange(ARange : TMeasureRangeBase);
Begin
  WriteLn(FName+'.SetSourceRange('+FloatToStr(ARange.FMaxValue)+')');
  FSourceRange := ARange as TMeasureRangeAccuracy;
  if not SameValue(FSourceRange.FMaxValue, FMeasure.FMeasureRange.FMaxValue) then
    WriteLn('Error: ',FName,' source range ',FloatToStr(FSourceRange.FMaxValue),' is different from measure range ',FloatToStr(FMeasure.FMeasureRange.FMaxValue));
End;

Procedure TInstrumentWrapperKeithley2450.EnableOutput;
Begin
  WriteLn(FName+'.EnableOutput');
End;

Procedure TInstrumentWrapperKeithley2450.DisableOutput;
Begin
  WriteLn(FName+'.DisableOutput');
End;

Procedure TInstrumentWrapperKeithley2450.SetSource(AValue : Double);
Begin
  WriteLn(FName+'.SetSource('+FloatToStr(AValue)+')');
End;

Function TInstrumentWrapperKeithley2450.GetSourceValue : TMeasurementResultBase;
Begin
  WriteLn(FName+'.GetSourceValue');
End;

Procedure TInstrumentWrapperKeithley2450.SetMeasureRange(ARange : TMeasureRangeBase);
Begin
  WriteLn(FName+'.SetMeasureRange('+FloatToStr(ARange.FMaxValue)+')');
  FMeasureRange := ARange as TMeasureRangeAccuracy;
End;

Procedure TInstrumentWrapperKeithley2450.StartMeasurement;
Begin
  WriteLn(FName+'.StartMeasurement');
End;

Function TInstrumentWrapperKeithley2450.GetResults : TMeasurementResultBase;
Begin
  WriteLn(FName+'.GetResults');
End;

{ TInstrumentWrapperKeysightE3631xA }

Class Procedure TInstrumentWrapperKeysightE3631xA.RegisterWrapper(AFactory : TInstrumentWrapperFactory);
Begin
  AFactory.RegisterWrapper('KeysightE36311A', TInstrumentWrapperKeysightE3631xA);
  AFactory.RegisterWrapper('KeysightE36312A', TInstrumentWrapperKeysightE3631xA);
  AFactory.RegisterWrapper('KeysightE36313A', TInstrumentWrapperKeysightE3631xA);
End;

Class Function TInstrumentWrapperKeysightE3631xA.CreateFromParams(AComparisonBase : TComparisonBase; AWrapperName, AName : String; AParams : TInstrumentWrapperParams) : TInstrumentWrapperBase;
Var St : String;
    I  : Integer;
Begin
  Result := TInstrumentWrapperKeysightE3631xA.Create(AComparisonBase, AWrapperName, AName, AParams, AParams.GetFunction('Function'));
  if Result.FFunction = ifSource then
    With TInstrumentWrapperKeysightE3631xA(Result) do
      Begin
        FVISA     := AParams.GetString('VISA');
        FChannel  := AParams.GetString('Ch');
        FCurLim   := AParams.GetDouble('CurLim');
        FPairMode := pmOff;
        Case FChannel of
          '1','2','3'   // can't use '1'..'3', because this is equivalent to "if (FChannel >= '1') and (FChannel <= '3') then", which includes '2+3' and '2||3'
                   : FChannelIdx := TChannel(Ord(FChannel[1]) - Ord('0'));
          '2+3'    : Begin
                       FChannelIdx := 2;
                       FPairMode   := pmSeries;
                     End;
          '2||3'   : Begin
                       FChannelIdx := 2;
                       FPairMode   := pmParallel;
                     End;
        else
          raise Exception.Create('Invalid channel specification '''+FChannel+'''');
        End;
      End
  else
    With TInstrumentWrapperKeysightE3631xA(Result) do
      Begin
        St := AParams.GetString('Ref');
        I := AComparisonBase.GetInstrumentIndex(St);
        if I < 0 then
          raise Exception.Create('Unknown referenced device '''+St+'''');
        // set values of new object
        FSource          := TInstrumentWrapperKeysightE3631xA(AComparisonBase.FInstruments[I]);
        FMeasure         := TInstrumentWrapperKeysightE3631xA(Result);
        FChannel         := FSource.FChannel;
        FChannelIdx      := FSource.FChannelIdx;
        FPairMode        := FSource.FPairMode;
        // set values to reference object
        FSource.FSource  := FSource;
        FSource.FMeasure := FMeasure;
      End;
End;

Function TInstrumentWrapperKeysightE3631xA.GetParams : TInstrumentWrapperParams;
Begin
  if not assigned(FParams) then
    FParams := TInstrumentWrapperParams.Create;
  FParams.SetOrAddFunction('Function', FFunction);
  if FFunction = ifSource then
    Begin
      FParams.SetOrAddString('VISA',   FVISA);
      FParams.SetOrAddString('Ch',     FChannel);
      FParams.SetOrAddDouble('CurLim', FCurLim);
    End
  else
    Begin
      FParams.SetOrAddString('Ref',    FSource.FName);
    End;
  Result := FParams;
End;

Procedure TInstrumentWrapperKeysightE3631xA.SetupRanges;
Begin
  FRanges := TKeysightE3631xA.GetRanges(FWrapperName+':'+IfThen(FFunction = ifMeasure, 'M', 'S')+':Ch'+FChannel);
End;

Procedure TInstrumentWrapperKeysightE3631xA.Initialize;
Begin
  WriteLn(FName+'.Initialize');
  if assigned(FSource) and assigned(FMeasure) and (FFunction = ifMeasure) then
    Begin
      FInstrument := FSource.FInstrument;
      FIdentifier := FInstrument.Identify;
      Exit;  // only source is initialized
    End;
  FComm       := DevComOpen(FVISA);
  FInstrument := TKeysightE3631xA.Create(FComm);
  FComm.SetTimeout(2000000{us});
  //FComm.ErrorHandler := @USBTMCErrorHandler;
  // TODO: like in Create, check that the device is the same type as FWrapperName
  WriteLn('Connected to power supply ',FInstrument.Identify);
  FIdentifier := FInstrument.Identify;
  WriteLn('Reset to default settings');
  FInstrument.Reset;
  WriteLn('Disable the beeper');
  FInstrument.SetBeeper(false);
  FInstrument.SetPairMode(FPairMode);
  FInstrument.SetCurrentLimit([FChannelIdx], FCurLim);
End;

Procedure TInstrumentWrapperKeysightE3631xA.Disconnect;
Begin
  WriteLn(FName+'.Disconnect');
  if assigned(FSource) and assigned(FMeasure) and (FFunction = ifMeasure) then
    Exit;  // only source is doing the job
  FreeAndNil(FInstrument);
//  TObject(FComm).Free;     // TODO: is it ok that FComm is not Free()d?
End;

Procedure TInstrumentWrapperKeysightE3631xA.SetSourceRange(ARange : TMeasureRangeBase);
Begin
  WriteLn(FName+'.SetSourceRange('+FloatToStr(ARange.FMaxValue)+')');
  FSourceRange := ARange as TMeasureRangeAccuracy;
  // SetMeasureRange was called _before_ SetSourceRange
  if not SameValue(FSourceRange.FMaxValue, FMeasure.FMeasureRange.FMaxValue) then
    WriteLn('Error: ',FName,' source range ',FloatToStr(FSourceRange.FMaxValue),' is different from measure range ',FloatToStr(FMeasure.FMeasureRange.FMaxValue));
End;

Procedure TInstrumentWrapperKeysightE3631xA.EnableOutput;
Begin
  WriteLn(FName+'.EnableOutput');
  FInstrument.EnableOutput([FChannelIdx], True);
End;

Procedure TInstrumentWrapperKeysightE3631xA.DisableOutput;
Begin
  WriteLn(FName+'.DisableOutput');
  FInstrument.EnableOutput([FChannelIdx], False);
End;

Procedure TInstrumentWrapperKeysightE3631xA.SetSource(AValue : Double);
Begin
  WriteLn(FName+'.SetSource('+FloatToStr(AValue)+')');
  FInstrument.SetCurrentLimit([FChannelIdx], 0.1);  // "boost" the output voltage to charge the capacitors
  FInstrument.SetVoltage([FChannelIdx], AValue);
  Sleep(200);
  FInstrument.SetCurrentLimit([FChannelIdx], FCurLim);
  Sleep(100);
  FSourceValue := AValue;
End;

Function TInstrumentWrapperKeysightE3631xA.GetSourceValue : TMeasurementResultBase;
Begin
  WriteLn(FName+'.GetSourceValue');
  Result := TMeasurementResultBase.Create(FSourceRange.FAccuracy[0].Apply(FSourceValue));
End;

Procedure TInstrumentWrapperKeysightE3631xA.SetMeasureRange(ARange : TMeasureRangeBase);
Begin
  WriteLn(FName+'.SetMeasureRange('+FloatToStr(ARange.FMaxValue)+')');
  FMeasureRange := ARange as TMeasureRangeAccuracy;
End;

Procedure TInstrumentWrapperKeysightE3631xA.StartMeasurement;
Begin
  WriteLn(FName+'.StartMeasurement');
End;

Function TInstrumentWrapperKeysightE3631xA.GetResults : TMeasurementResultBase;
Var V, I : Double;
Begin
  WriteLn(FName+'.GetResults');
  I := FInstrument.MeasureCurrent(FChannelIdx);
  V := FInstrument.MeasureVoltage(FChannelIdx);
  //WriteLn('  V = ',V:1:5,'V, I = ',I:1:5,'A');
  Result := TMeasurementResultVI.Create(
              FMeasureRange.FAccuracy[FInstrument.AccIdxV].Apply(V),
              FMeasureRange.FAccuracy[IfThen(I > 10E-3, FInstrument.AccIdxI, FInstrument.AccIdxIlow)].Apply(I));
              // TODO: in the 6V range the limit is 20mA, in the 25V (and presumably 50V) range it is 10mA
  WriteLn('  ',Result.ToString);
End;

{ TObjectFactory }

Constructor TObjectFactory.Create;
Begin
  inherited Create;
End;

Destructor TObjectFactory.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TObjectFactory.RegisterClass(AClass : TClass);
Begin
  SetLength(FRegistry, Length(FRegistry)+1);
  FRegistry[Length(FRegistry)-1] := AClass;
End;

Function TObjectFactory.EncodeObject(Const O : TObject) : String;
Begin
  (* O^ points to the object data, where the first 32 or 64 bits are a pointer
   * to the VMT, and the rest is the actual data.
   * Here we take the actual data and encode it in hex.
   * Format: 'TMyClass(00000000)'
   *)
  Result := O.ClassName+'(';
  if O.InheritsFrom(TPersistent) then
    Result := Result + EncodeDataHex((Pointer(O)+TPersistent.InstanceSize)^, O.InstanceSize - TPersistent.InstanceSize)
  else
    Result := Result + EncodeDataHex((Pointer(O)+TObject.InstanceSize)^,     O.InstanceSize - TObject.InstanceSize);
  Result := Result + ')';
End;

Function TObjectFactory.CreateObject(Name : String) : TObject;
Var I : Integer;
Begin
  Result := Nil;
  For I := 0 to Length(FRegistry)-1 do
    if FRegistry[I].ClassName = Name then
      Begin
        Result := FRegistry[I].NewInstance;
      End;
End;

Function TObjectFactory.DecodeObject(Const St : String) : TObject;
Var C : String;
    D : String;
    I : Integer;
Begin
  // get class name
  I := Pos('(', St);
  if I = 0 then
    raise Exception('Invalid format of object string '''+St+''': missing ''(''');
  C := Copy(St, 1, I-1);
  // cut off leading class name and '('
  D := Copy(St, I+1, Length(St));
  // extract data
  I := Pos(')', D);
  if I = 0 then
    raise Exception('Invalid format of object string '''+St+''': missing '')''');
  if I < Length(D) then
    raise Exception('Invalid format of object string '''+St+''': chars after '')''');
  SetLength(D, Length(D) - 1);
  // search class and create object
  Result := CreateObject(C);
  if Result = Nil then
    raise Exception.Create('Unknown class '+C);
  //WriteLn(C,'.InstSize = ',Result.InstanceSize);
  if Result is TPersistent then
    DecodeDataHex(D, (Pointer(Result)+TPersistent.InstanceSize)^, Result.InstanceSize - TPersistent.InstanceSize)
  else
    DecodeDataHex(D, (Pointer(Result)+TObject.InstanceSize)^,     Result.InstanceSize - TObject.InstanceSize);
End;

Procedure SetupObjectFactory;
Begin
  if assigned(ObjectFactory) then Exit;
  ObjectFactory := TObjectFactory.Create;
  ObjectFactory.RegisterClass(TValueAccuracyBase);
  ObjectFactory.RegisterClass(TValueAccuracyMinMax);
End;

Procedure FreeObjectFactory;
Begin
  FreeAndNil(ObjectFactory);
End;

{ TMeasurementResultFactory }

Constructor TMeasurementResultFactory.Create;
Begin
  inherited Create;
End;

Destructor TMeasurementResultFactory.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TMeasurementResultFactory.RegisterResultClass(AClass : TMeasurementResultClass);
Begin
  SetLength(FRegistry, Length(FRegistry)+1);
  FRegistry[Length(FRegistry)-1] := AClass;
End;

Function TMeasurementResultFactory.CreateObject(Name : String) : TMeasurementResultBase;
Var I : Integer;
Begin
  Result := Nil;
  For I := 0 to Length(FRegistry)-1 do
    if FRegistry[I].ClassName = Name then
      Begin
        Result := TMeasurementResultBase(FRegistry[I].NewInstance);
        Exit;
      End;
End;

Function TMeasurementResultFactory.CreateResult(St : String) : TMeasurementResultBase;
Var I : Integer;
    C : String;
Begin
  I := Pos(':', St);
  if I = 0 then
    raise Exception.Create('Missing '':'' in result string '''+St+'''');
  C := Copy(St, 1, I-1);
  St := Copy(St, I+1, Length(St));
  For I := 0 to Length(FRegistry)-1 do
    if FRegistry[I].ClassName = C then
      Begin
        Result := FRegistry[I].CreateDecode(St);
        Exit;
      End;
  raise Exception.Create('Unknown class '+C);
End;

Procedure SetupMeasurementResultFactory;
Begin
  if assigned(MeasurementResultFactory) then Exit;
  MeasurementResultFactory := TMeasurementResultFactory.Create;
  MeasurementResultFactory.RegisterResultClass(TMeasurementResultBase);
  MeasurementResultFactory.RegisterResultClass(TMeasurementResultVI);
End;

Procedure FreeMeasurementResultFactory;
Begin
  FreeAndNil(MeasurementResultFactory);
End;

{ TMeasurementResultBase }

Constructor TMeasurementResultBase.Create(AValue : TValueAccuracyBase);
Begin
  inherited Create;
  FValue := AValue;
End;

Destructor TMeasurementResultBase.Destroy;
Begin
  FValue.Free;
  inherited Destroy;
End;

Function TMeasurementResultBase.ToString : String;
Begin
  Result := FValue.ToString;
End;

Function TMeasurementResultBase.Encode : String;
Begin
  Result := ClassName+':'+ObjectFactory.EncodeObject(FValue);
End;

Constructor TMeasurementResultBase.CreateDecode(St : String);
Begin
  inherited Create;
  FValue := ObjectFactory.DecodeObject(St) as TValueAccuracyBase;
End;

{ TMeasurementResultVI }

Constructor TMeasurementResultVI.Create(AVoltage, ACurrent : TValueAccuracyBase);
Begin
  inherited Create(AVoltage);
  FCurrent := ACurrent;
End;

Destructor TMeasurementResultVI.Destroy;
Begin
  FCurrent.Free;
  inherited Destroy;
End;

Function TMeasurementResultVI.ToString : String;
Begin
  Result := 'V = '+FValue.ToString+', I = '+FCurrent.ToString;
End;

Function TMeasurementResultVI.Encode : String;
Begin
  Result := inherited Encode;
  Result := Result + '+' + ObjectFactory.EncodeObject(FCurrent);
End;

Constructor TMeasurementResultVI.CreateDecode(St : String);
Var I : Integer;
Begin
  inherited Create(Nil);   // set FValue below
  I := Pos('+', St);
  if I = 0 then
    raise Exception.Create('Invalid format of encoding '''+St+'''');
  FValue   := ObjectFactory.DecodeObject(Copy(St, 1,   I-1))        as TValueAccuracyBase;
  FCurrent := ObjectFactory.DecodeObject(Copy(St, I+1, Length(St))) as TValueAccuracyBase;
End;

{ TMeasurementAnalysis }

Constructor TMeasurementAnalysis.Create;
Begin
  inherited Create;
  FMinMin := +MaxDouble;
  FMaxMin := -MaxDouble;
  FMinMax := +MaxDouble;
  FMaxMax := -MaxDouble;
  FPass := False;
End;

Procedure TMeasurementAnalysis.Analyze;
Var NI : Integer;
    A  : TValueAccuracyMinMax;
Begin
  FMinMin := +MaxDouble;
  FMaxMin := -MaxDouble;
  FMinMax := +MaxDouble;
  FMaxMax := -MaxDouble;
  For NI := 0 to Length(FResults)-1 do
    Begin
      A := FResults[NI].FValue as TValueAccuracyMinMax;
      FMinMin := min(FMinMin, A.FMin);   // lowest bound of all ranges
      FMaxMin := max(FMaxMin, A.FMin);   // highest case of lower bound of all ranges
      FMinMax := min(FMinMax, A.FMax);   // lowest case of higher bound of all ranges
      FMaxMax := max(FMaxMax, A.FMax);   // higest bound of all ranges
    End;
  FPass := FMaxMin < FMinMax;
End;

{ TComparisonSet }

Constructor TComparisonSet.Create(AComparison : TComparisonBase; APrev : TComparisonSet);
Begin
  inherited Create;
  FComparison := AComparison;
  FPrev       := APrev;
  SetLength(FRanges, Length(FComparison.FInstruments));
End;

Destructor TComparisonSet.Destroy;
Var NI,NP : Integer;
Begin
  For NI := 0 to Length(FAnalyses)-1 do
    FAnalyses[NI].Free;
  For NI := 0 to Length(FComparison.FInstruments)-1 do
    For NP := 0 to Length(FTestPoints.FValues)-1 do
      FMeasurements[NI][NP].Free;
  FTestPoints.Free;
  inherited Destroy;
End;

Procedure TComparisonSet.PrintRanges(AWithPrev : Boolean);
Var AllRanges  : TInstrumentRanges;
    Prev       : TComparisonSet;
    I          : Integer;
    Instrument : TInstrumentWrapperBase;
    Range      : TMeasureRangeBase;
Begin
  AllRanges := FRanges;
  SetLength(AllRanges, Length(AllRanges));   // really copy the array
  Prev := FPrev;
  if AWithPrev then
    While assigned(Prev) do
      Begin
        For I := 0 to Length(FRanges)-1 do
          if not assigned(AllRanges[I]) then
            AllRanges[I] := Prev.FRanges[I];       // this might (again) set it to Nil
        Prev := Prev.FPrev;
      End;
  For I := 0 to Length(FRanges)-1 do
    Begin
      if not assigned(AllRanges[I]) then continue;
      Instrument := FComparison.FInstruments[I];
      Range      := AllRanges[I];
      WriteLn('  ',Instrument.FName,': ',FloatToStr(Range.FMaxValue));
    End;
End;

Procedure TComparisonSet.PrintMeasurementsByInstrument;
Var AllRanges  : TInstrumentRanges;
    NI,NP      : Integer;
    Instrument : TInstrumentWrapperBase;
    Range      : TMeasureRangeBase;
Begin
  if Length(FMeasurements) <> Length(FComparison.FInstruments) then
    Begin
      WriteLn('    No measurements?');
      Exit;
    End;
  // determine all active ranges
  AllRanges := GetAllRanges;
  // print results
  For NI := 0 to Length(FRanges)-1 do
    Begin
      if not assigned(AllRanges[NI]) then continue;
      Instrument := FComparison.FInstruments[NI];
      Range      := AllRanges[NI];
      WriteLn('  ',Instrument.FName,' @ ',FloatToStr(Range.FMaxValue),':');
      if Length(FMeasurements[NI]) <> Length(FTestPoints.FValues) then
        Begin
          WriteLn('    Different number of measurements ',Length(FMeasurements[NI]),' than testpoints ',Length(FTestPoints.FValues));
          Continue;
        End;
      For NP := 0 to Length(FTestPoints.FValues)-1 do
        Begin
          if not assigned(FMeasurements[NI][NP]) then
            Begin
              WriteLn('    No measurement');
              Continue;
            End;
          WriteLn('    ',FloatToStr(FTestPoints.FValues[NP]),' -> ',FMeasurements[NI][NP].ToString);
        End;
    End;
End;

Procedure TComparisonSet.PrintMeasurementsByTestPoint;
Var AllRanges  : TInstrumentRanges;
    NI,NP      : Integer;
    Instrument : TInstrumentWrapperBase;
    Range      : TMeasureRangeBase;
Begin
  if Length(FMeasurements) <> Length(FComparison.FInstruments) then
    Begin
      WriteLn('    No measurements?');
      Exit;
    End;
  // determine all active ranges
  AllRanges := GetAllRanges;
  // print results
  For NP := 0 to Length(FTestPoints.FValues)-1 do
    Begin
      WriteLn('  Testpoint ',FloatToStr(FTestPoints.FValues[NP]));
      For NI := 0 to Length(FRanges)-1 do
        Begin
          if not assigned(AllRanges[NI]) then continue;
          if not assigned(FMeasurements[NI][NP]) then
            Begin
              WriteLn('    No measurement');
              Continue;
            End;
          if Length(FMeasurements[NI]) <> Length(FTestPoints.FValues) then
            Begin
              WriteLn('    Different number of measurements ',Length(FMeasurements[NI]),' than testpoints ',Length(FTestPoints.FValues));
              Continue;
            End;
          Instrument := FComparison.FInstruments[NI];
          Range      := AllRanges[NI];
          WriteLn('    ',Instrument.FName,' @ ',FloatToStr(Range.FMaxValue),': ',FMeasurements[NI][NP].ToString);
        End;
    End;
End;

Function TComparisonSet.GetAllRanges : TInstrumentRanges;
Var Prev : TComparisonSet;
    NI   : Integer;
Begin
  // determine all active ranges
  Result := FRanges;
  SetLength(Result, Length(Result));   // really copy the array
  Prev := FPrev;
  While assigned(Prev) Do
    Begin
      For NI := 0 To Length(FRanges) - 1 Do
        If Not assigned(Result[NI]) Then
          Result[NI] := Prev.FRanges[NI];       // this might (again) set it to Nil
      Prev := Prev.FPrev;
    End;
End;

Procedure TComparisonSet.Analyze;
Var NP, NI : Integer;
Begin
  if Length(FAnalyses) <> 0 then
    raise Exception.Create('Analyze can only be called once.');
  if Length(FMeasurements) = 0 then
    Exit;   // no measurements --> nothing to analyze
  FPass := True;
  SetLength(FAnalyses, Length(FTestPoints.FValues));
  For NP := 0 to Length(FTestPoints.FValues)-1 do
    Begin
      FAnalyses[NP] := TMeasurementAnalysis.Create;
      SetLength(FAnalyses[NP].FResults, Length(FComparison.FInstruments));
      For NI := 0 to Length(FComparison.FInstruments)-1 do
        Begin
          FAnalyses[NP].FResults[NI] := FMeasurements[NI][NP];
        End;
      FAnalyses[NP].Analyze;
      if not FAnalyses[NP].FPass then
        FPass := False;
    End;
End;

{ TComparisonProcedure }

Constructor TComparisonProcedure.Create;
Begin
  inherited Create;
End;

Destructor TComparisonProcedure.Destroy;
Var I : Integer;
Begin
  For I := 0 to Length(FSets)-1 do
    FSets[I].Free;
  inherited Destroy;
End;

Procedure TComparisonProcedure.AddSet(ASet : TComparisonSet);
Begin
  SetLength(FSets, Length(FSets)+1);
  FSets[Length(FSets)-1] := ASet;
End;

Procedure TComparisonProcedure.PrintRanges;
Var I : Integer;
Begin
  For I := 0 to Length(FSets)-1 do
    Begin
      WriteLn('--- Set #',I,' of Ranges up to ',FloatToStr(FSets[I].FMaxVal),' ---');
      FSets[I].PrintRanges(False{True});
      WriteLn('  Testpoints: ', FSets[I].FTestPoints.ToString);
    End;
End;

Procedure TComparisonProcedure.PrintMeasurementsByInstrument;
Var I : Integer;
Begin
  For I := 0 to Length(FSets)-1 do
    Begin
      WriteLn('--- Set #',I,' of Ranges up to ',FloatToStr(FSets[I].FMaxVal),' ---');
      FSets[I].PrintMeasurementsByInstrument;
    End;
End;

Procedure TComparisonProcedure.PrintMeasurementsByTestPoint;
Var I : Integer;
Begin
  For I := 0 to Length(FSets)-1 do
    Begin
      WriteLn('--- Set #',I,' of Ranges up to ',FloatToStr(FSets[I].FMaxVal),' ---');
      FSets[I].PrintMeasurementsByTestPoint;
    End;
End;

Function TComparisonProcedure.GetNumSets : Integer;
Begin
  Result := Length(FSets);
End;

Function TComparisonProcedure.GetNumTestpoints : Integer;
Var I : Integer;
Begin
  Result := 0;
  For I := 0 to Length(FSets)-1 do
    Result := Result + Length(FSets[I].FTestPoints.FValues);
End;

Procedure TComparisonProcedure.Analyze;
Var NS : Integer;
Begin
  if Length(FSets) = 0 then
    Exit;
  FPass := True;
  For NS := 0 to Length(FSets)-1 do
    Begin
      FSets[NS].Analyze;
      if not FSets[NS].FPass then
        FPass := False;
    End;
End;

{ TComparisonBase }

Constructor TComparisonBase.Create;
Begin
  inherited Create;
  FQuantity := qtDCV;
  SetLength(FInstruments, 0);
End;

Destructor TComparisonBase.Destroy;
Var I : Integer;
Begin
  FProcedure.Free;
  For I := 0 to Length(FInstruments)-1 do
    FInstruments[I].Free;
  Inherited Destroy;
End;

Procedure TComparisonBase.AddInstrument(AInstrument : TInstrumentWrapperBase);
Begin
  SetLength(FInstruments, Length(FInstruments)+1);
  FInstruments[Length(FInstruments)-1] := AInstrument;
End;

Procedure TComparisonBase.SetupRanges;
Var NI : Integer;
Begin
  For NI := 0 to Length(FInstruments)-1 do
    FInstruments[NI].SetupRanges;
End;

Procedure TComparisonBase.CreateTestPoints(ANumLinPoints:Integer);
Var NI : Integer;
Begin
  // remove measurement procedure
  FreeAndNil(FProcedure);
  // create testpoints
  For NI := 0 to Length(FInstruments)-1 do
    FInstruments[NI].CreateTestPoints(ANumLinPoints);
End;

Procedure TComparisonBase.PrintTestPoints;
Var NI,NR,NP : Integer;
    TP       : TTestPoints;
    A        : TValueAccuracyMinMax;
Begin
  // print ranges and testpoints
  For NI := 0 to Length(FInstruments)-1 do
    Begin
      WriteLn('Instrument = ',FInstruments[NI].FName+' ('+CInstrumentFunction[FInstruments[NI].FFunction]+')');
      For NR := 0 to Length(FInstruments[NI].FRanges[FQuantity])-1 do
        Begin
          WriteLn('  Range = ',FloatToStr(FInstruments[NI].FRanges[FQuantity][NR].FMaxValue));
          TP := FInstruments[NI].FTestpoints[NR];
          For NP := 0 to Length(TP.FValues)-1 do
            Begin
              A := TValueAccuracyMinMax(FInstruments[NI].FRanges[FQuantity][NR].FAccuracy[0].Apply(TP.FValues[NP]));
              WriteLn('    TestPoint = ',FloatToStr(TP.FValues[NP]),', Min = ',FloatToStr(A.FMin),', Max = ',FloatToStr(A.FMax));
              A.Free;
            End;
        End;
    End;
End;

Function TComparisonBase.GetInstrumentIndex(AName : String) : Integer;
Var NI : Integer;
Begin
  Result := -1;
  For NI := 0 to Length(FInstruments)-1 do
    if FInstruments[NI].FName = AName then
      Result := NI;
End;

Function TComparisonBase.GetInstrumentRangeIdx(AInstrumentIdx : Integer; ARange : Double) : Integer;
Var NR : Integer;
Begin
  Result := -1;
  if (AInstrumentIdx < 0) or (AInstrumentIdx >= Length(FInstruments)) then
    raise Exception.Create('Instrument index '+IntToStr(AInstrumentIdx)+' out of range');
  For NR := 0 to Length(FInstruments[AInstrumentIdx].FRanges[FQuantity])-1 do
    Begin
      if SameValue(FInstruments[AInstrumentIdx].FRanges[FQuantity][NR].FMaxValue, ARange) then
        Result := NR;
    End;
End;

Function TComparisonBase.GetInstrumentRange(AInstrumentIdx : Integer; ARange : Double) : TMeasureRangeBase;
Var NR : Integer;
Begin
  Result := Nil;
  NR := GetInstrumentRangeIdx(AInstrumentIdx, ARange);
  if NR < 0 then
    Exit;
  Result := FInstruments[AInstrumentIdx].FRanges[FQuantity][NR];
End;

Function TComparisonBase.GetInstrumentTypes : TInstrumentTypes;
Var NI,NT : Integer;
    Same  : Boolean;
Begin
  SetLength(Result, 0);
  if Length(FInstruments) = 0 then Exit;
  // first entry
  SetLength(Result, 1);
  Result[0].FInstrumentType := TInstrumentWrapperClass(FInstruments[NI].ClassType);
  SetLength(Result[0].FInstruments, 1);
  Result[0].FInstruments[0] := FInstruments[NI];
  For NI := 1 to Length(FInstruments)-1 do
    Begin
      Same := False;
      For NT := 0 to Length(Result)-1 do
        Begin
          if Result[NT].FInstrumentType = TInstrumentWrapperClass(FInstruments[NI].ClassType) then
            Begin
              // same type: add to instruments list
              SetLength(Result[NT].FInstruments, Length(Result[NT].FInstruments)+1);
              Result[NT].FInstruments[Length(Result[NT].FInstruments)-1] := FInstruments[NI];
              Same := True;
              break;
            End;
        End;
      if not Same then
        Begin
          // new type: add to types list
          SetLength(Result, Length(Result)+1);
          Result[Length(Result)-1].FInstrumentType := TInstrumentWrapperClass(FInstruments[NI].ClassType);
          SetLength(Result[Length(Result)-1].FInstruments, 1);
          Result[Length(Result)-1].FInstruments[0] := FInstruments[NI];
        End;
    End;
End;

{$IFDEF CreateMeasureDefinitionFirstAttempt}
Procedure TComparisonBase.CreateMeasureDefinitionFirstAttempt;
Var NI,NR,NP       : Integer;
    RL             : TRangesList;
    RLSet          : Set of Byte;
    IR             : Array of TRangeInfo;          // current range selected at each instrument
    B              : Boolean;
    MaxVal         : Double;
    MaxInstIdx     : Integer;
    UnusedRanges   : Boolean;
Begin
  WriteLn('########## Creating Measure Definition (Method 1) ##########');
  // summarize available ranges
  RL := TRangesList.Create;
  For NI := 0 to Length(FInstruments)-1 do
    Begin
      if FInstruments[NI].FFunction <> ifMeasure then continue;
      For NR := 0 to Length(FInstruments[NI].FRanges[qtDCV])-1 do
        Begin
          RL.Add(TRangeInfo.Create(FInstruments[NI], qtDCV, NR, 0));
        End;
    End;
  RL.Sort;
  WriteLn('--- All Ranges ---');
  For NI := 0 to RL.FRangeInfos.Count-1 do
    Begin
      With RL.FRangeInfos[NI] do
        Begin
          WriteLn(FInstrument.FName+' '+FloatToStr(GetRange.FMaxValue));
        End;
    End;
  // find range combinations
  RLSet := [];
  SetLength(IR, Length(FInstruments));
  NP := 0;
  repeat
    WriteLn('--- Searching next lowest range ---');
    For NR := 0 to RL.FRangeInfos.Count-1 do
      Begin
        if NR in RLSet then continue;    // skip ranges which were already used
        With RL.FRangeInfos[NR] do
          Begin
            WriteLn('  checking ',FInstrument.FName+' '+FloatToStr(GetRange.FMaxValue));
            // search where RL.FRangeInfos[NR].FInstrument is at which index in FInstruments
            For NI := 0 to Length(FInstruments)-1 do
              if FInstrument = FInstruments[NI] then
                Begin
                  if NP = 0 then
                    Begin
                      // initial search: populate all instruments, but don't overwrite
                      if assigned(IR[NI]) then break;
                    End;  // 2nd and later searches: overwrite instrument with next larger range
                  IR[NI] := RL.FRangeInfos[NR];
                  RLSet  := RLSet + [NR];
                  break;
                End;
          End;
        if NP = 0 then
          Begin
            // initial search: check if we have a range for every instrument
            B := True;
            For NI := 0 to Length(FInstruments)-1 do
              Begin
                if FInstruments[NI].FFunction <> ifMeasure then continue;
                if not assigned(IR[NI]) then
                  Begin
                    B := False;
                    break;
                  End;
              End;
            if B then break;
          End
        else
          Begin
            // 2nd and later searches: check if there are more instruments with the same next range
            if NR >= RL.FRangeInfos.Count-1 then break;  // last entry reached, no more instruments
            if SameValue(RL.FRangeInfos[NR+1].GetRange.FMaxValue, RL.FRangeInfos[NR].GetRange.FMaxValue) then break;
          End;
      End;
    // determine max. range and the associated instrumnt of the current set
    MaxVal := Infinity;
    For NI := 0 to Length(FInstruments)-1 do
      Begin
        if not assigned(IR[NI]) then continue;
        with IR[NI] do
          Begin
            if MaxVal > GetRange.FMaxValue then
              Begin
                MaxVal := GetRange.FMaxValue;
                MaxInstIdx := NI;
              End;
          End;
      End;
    // report current set
    WriteLn('--- Set #',NP,' of Ranges up to ',FloatToStr(MaxVal),' because of ',IR[MaxInstIdx].FInstrument.FName,' ---');
    For NI := 0 to Length(FInstruments)-1 do
      Begin
        if not assigned(IR[NI]) then continue;
        with IR[NI] do
          Begin
            WriteLn(FInstrument.FName+' '+FloatToStr(GetRange.FMaxValue));
          End;
      End;
    Inc(NP);
    // check if there are still some unused ranges
    UnusedRanges := False;
    For NR := 0 to RL.FRangeInfos.Count-1 do
      Begin
        if NR in RLSet then continue;    // skip ranges which were already used
        UnusedRanges := True;
        break;
      End;
  Until not UnusedRanges;
End;

{$ENDIF} // CreateMeasureDefinitionFirstAttempt

Procedure TComparisonBase.CreateMeasureDefinition;
Var NI,NP     : Integer;
    TP, TP2   : TTestPoints;
    ISet      : Set of Byte;
    MaxVal    : Double;
    OldMaxVal : Double;
    NextRange : Double;
    IRI       : Array of Byte;
    CS        : TComparisonSet;
Begin
  if assigned(FProcedure) then
    FProcedure.Free;
  WriteLn('########## Creating Measure Definition (Method 2) ##########');
  FProcedure := TComparisonProcedure.Create;
  CS := Nil;
  SetLength(IRI, Length(FInstruments));
  For NI := 0 to Length(FInstruments)-1 do
    IRI[NI] := 0;
  NP := 0;
  OldMaxVal := Infinity;
  ISet := [0..(Length(FInstruments)-1)];      // initially, all instruments are new
  repeat
    // search lowest current range
    MaxVal := Infinity;
    For NI := 0 to Length(FInstruments)-1 do
      Begin
        //if FInstruments[NI].FFunction <> ifMeasure then continue;
        MaxVal := Min(MaxVal, FInstruments[NI].FRanges[FQuantity][IRI[NI]].FMaxValue);
      End;
    CS := TComparisonSet.Create(Self, CS);
    // print
    WriteLn('--- Set #',NP,' of Ranges up to ',FloatToStr(MaxVal),' ---');
    TP2 := TTestPoints.Create;
    For NI := 0 to Length(FInstruments)-1 do
      Begin
        //if FInstruments[NI].FFunction <> ifMeasure then continue;
        if NI in ISet then
          Begin
            // new range for this instrument
            CS.FRanges[NI] := FInstruments[NI].FRanges[FQuantity][IRI[NI]];
            // for all new ranges, take all testpoints below the new MaxVal
            TP := TTestPoints.Create(FInstruments[NI].FTestpoints[IRI[NI]].RangeTo(MaxVal));
          End
        else
          Begin
            // unchanged range for this instrument
            // for all unchanged ranges, take all testpoints between the OldMaxVal and the new MaxVal
            TP := TTestPoints.Create(FInstruments[NI].FTestpoints[IRI[NI]].Range(OldMaxVal,MaxVal));
          End;
        WriteLn(FInstruments[NI].FName+' '+FloatToStr(FInstruments[NI].FRanges[FQuantity][IRI[NI]].FMaxValue),': ',TP.ToString,' (of ',FInstruments[NI].FTestpoints[IRI[NI]].ToString,')');
        TP2.AddPoints(TP);
        TP.Free;
      End;
    WriteLn('--- Testpoints for Set #',NP,' of Ranges ---');
    TP2.Sort;
    WriteLn(TP2.ToString);
    CS.FTestPoints := TP2;
    CS.FMaxVal     := MaxVal;
    FProcedure.AddSet(CS);

    // search lowest next range
    Inc(NP);
    OldMaxVal := MaxVal;
    WriteLn('--- Searching next lowest range ---');
    NextRange := Infinity;
    ISet := [];
    For NI := 0 to Length(FInstruments)-1 do
      Begin
        //if FInstruments[NI].FFunction <> ifMeasure then continue;
        if Length(FInstruments[NI].FRanges[FQuantity]) <= IRI[NI]+1 then continue;  // no higher range available
        NextRange := Min(NextRange, FInstruments[NI].FRanges[FQuantity][IRI[NI]+1].FMaxValue);
      End;
    if NextRange = Infinity then
      Begin
        // no range available
        WriteLn('No higher ranges available, done.');
        break;
      End;
    WriteLn('  next lowest range is ',FloatToStr(NextRange));
    // use all instruments with that new range
    For NI := 0 to Length(FInstruments)-1 do
      Begin
        //if FInstruments[NI].FFunction <> ifMeasure then continue;
        if Length(FInstruments[NI].FRanges[FQuantity]) <= IRI[NI]+1 then continue;
        if SameValue(NextRange, FInstruments[NI].FRanges[FQuantity][IRI[NI]+1].FMaxValue) then
          Begin
            Inc(IRI[NI]);
            ISet := ISet + [NI];
            WriteLn('  changing ',FInstruments[NI].FName,' to ',FloatToStr(NextRange));
          End;
      End;
  Until False;

  FProcedure.PrintRanges;
End;

{$IFDEF TestJSONStreamer}

Procedure TComparisonBase.MyBeforeStreamObject(Sender : TObject; AObject : TObject; JSON : TJSONObject);
Begin
  JSON.Add('ClassName', AObject.ClassName);
End;

Procedure MyGetObject(Sender : TObject; AObject : TObject; Info : PPropInfo; AData : TJSONObject; DataName : TJSONStringType; Var AValue : TObject);
Var C : TJSONStringType;
    I : Integer;
Begin
  C := AData.Get('ClassName', '');
  AValue := ObjectFactory.CreateObject(C);
  if AValue = Nil then
    raise Exception.Create('Unknown class '+C+' of JSON object');
  //WriteLn(C,'.InstanceSize = ',AValue.InstanceSize);
End;

{$ENDIF TestJSONStreamer}

Procedure TComparisonBase.Save(AFilename : String);
Var S           : TStringList;
    NI,NR,NS,NP : Integer;
    AllRanges   : TInstrumentRanges;
    Prev        : TComparisonSet;
    Instrument  : TInstrumentWrapperBase;
    Range       : TMeasureRangeBase;
{$IFDEF TestJSONStreamer}
    Streamer    : TJSONStreamer;
    JSONString  : String;
    JSON        : TJSONData;
    DeStreamer  : TJSONDeStreamer;
    St          : String;
    M           : TMeasurementResultBase;
{$ENDIF TestJSONStreamer}
Begin
  S := TStringList.Create;
  S.Add('# Autogenerated comparison data on '+FormatDateTime('yyyy-mm-dd hh:mm:ss', Now));
  // instruments
  S.Add('[Instruments]');
  For NI := 0 to Length(FInstruments)-1 do
    Begin
      S.Add(FInstruments[NI].FName+' = '+FInstruments[NI].FWrapperName+'('+FInstruments[NI].GetParams.ToSyntax+')');
    End;
  //S.Add('E36313A_Ch23S = E36313A(Function=Source, VISA=''USB0::0x2A8D::0x1202::MY58180345::0::INSTR'', Ch=''2+3'', CurLim=1E-3, ...)   # also is the source');
  //S.Add('E36313A_Ch23S_M = E36313A(Function=Measure, Ref=''E36313A_Ch23_S'')');
  //S.Add('Fluke177_0  = Fluke177()');
  //S.Add('U1253B_0    = U1253B(TTY=''/dev/ttyUSB0'')');
  //S.Add('K34461A_0   = K34461A(VISA=''USB0::0x2A8D::0x1301::MY57213445::0::INSTR'')');
  //S.Add('DMM6500_1   = DMM6500(VISA=...)');
  //S.Add('DMM6500_2   = DMM6500(Ref=''DMM6500_1'', TSPLinkNode=2)');
  //S.Add('DMM6500_3   = DMM6500(...)');
  //S.Add('DMM6500_4   = DMM6500(...)');
  //S.Add('K2450_0     = K2450(Function=Measure, ...)');
  //S.Add('MSOX3024A_0 = MSOX3024A(VISA=''USB0::0x0957::0x17a6::MY52491958::0::INSTR'')');

  // settings
  S.Add('[Settings]');
  S.Add('# all values (esp. ranges, testpoints, ...) always in SI units, i.e., no');
  S.Add('# prefixes, and also no units (for simplification).');
  S.Add('Quantity = '+CQuantityStr[FQuantity]);
  //S.Add('NumLinPoints = 5');
  //S.Add('# (limit ranges, polarity, ...)');
  //S.Add('...');

  // testpoints
  S.Add('[Testpoints]');
  For NI := 0 to Length(FInstruments)-1 do
    For NR := 0 to Length(FInstruments[NI].FTestPoints)-1 do
      Begin
        S.Add(FInstruments[NI].FName+'.Testpoints['+FloatToStr(FInstruments[NI].FRanges[FQuantity][NR].FMaxValue)+'] = ['+FInstruments[NI].FTestPoints[NR].ToString+']');
      End;

  // measurement procedure
  if assigned(FProcedure) then
    Begin
      S.Add('[Procedure]');
      For NS := 0 to Length(FProcedure.FSets)-1 do
        Begin
          S.Add('(Set '+IntToStr(NS)+')    # Ranges up to '+FloatToStr(FProcedure.FSets[NS].FMaxVal));
          For NR := 0 to Length(FProcedure.FSets[NS].FRanges)-1 do
            Begin
              if not assigned(FProcedure.FSets[NS].FRanges[NR]) then continue;
              S.Add(FInstruments[NR].FName+'.Range = '+FloatToStr(FProcedure.FSets[NS].FRanges[NR].FMaxValue));
            End;
          S.Add('Testpoints = ['+FProcedure.FSets[NS].FTestPoints.ToString+']');
        End;
    End;

  // results
  if assigned(FProcedure) and (Length(FProcedure.FSets[0].FMeasurements) > 0) then
    Begin
      S.Add('[Results]');
      // instrument information
      For NI := 0 to Length(FInstruments)-1 do
        Begin
          S.Add(FInstruments[NI].FName+'.Identifier = '''+FInstruments[NI].FIdentifier+'''');
        End;
      // measurement results
      For NS := 0 to Length(FProcedure.FSets)-1 do
        Begin
          // the results are in FMeasurements[InstrumentIdx][TestPointIdx]
          if Length(FProcedure.FSets[NS].FMeasurements) <> Length(FInstruments) then
            //raise Exception.Create('First dimension of FMeasurement in set '+IntToStr(NS)+' should be the same length as number of instruments '+IntToStr(Length(FInstruments))+' but has '+IntToStr(Length(FProcedure.FSets[NS].FMeasurements)));
            Begin
              WriteLn('Warning: First dimension of FMeasurement in set '+IntToStr(NS)+' should be the same length as number of instruments '+IntToStr(Length(FInstruments))+' but has '+IntToStr(Length(FProcedure.FSets[NS].FMeasurements)));
              Continue;
            End;
          if Length(FProcedure.FSets[NS].FRanges) <> Length(FInstruments) then
            raise Exception.Create('FRanges in set '+IntToStr(NS)+' should be the same length as number of instruments '+IntToStr(Length(FInstruments))+' but has '+IntToStr(Length(FProcedure.FSets[NS].FRanges)));
          // determine all active ranges
          AllRanges := FProcedure.FSets[NS].FRanges;
          Prev := FProcedure.FSets[NS].FPrev;
          While assigned(Prev) do
            Begin
              For NI := 0 to Length(FProcedure.FSets[NS].FRanges)-1 do
                if not assigned(AllRanges[NI]) then
                  AllRanges[NI] := Prev.FRanges[NI];       // this might (again) set it to Nil
              Prev := Prev.FPrev;
            End;
          S.Add('(Set '+IntToStr(NS)+')    # Ranges up to '+FloatToStr(FProcedure.FSets[NS].FMaxVal));
          For NI := 0 to Length(FProcedure.FSets[NS].FRanges)-1 do
            Begin
              if not assigned(AllRanges[NI]) then continue;
              Instrument := FInstruments[NI];
              Range      := AllRanges[NI];
              WriteLn('  ',Instrument.FName,' @ ',FloatToStr(Range.FMaxValue),':');
              if Length(FProcedure.FSets[NS].FMeasurements[NI]) <> Length(FProcedure.FSets[NS].FTestPoints.FValues) then
                Begin
                  WriteLn('    Different number of measurements ',Length(FProcedure.FSets[NS].FMeasurements[NI]),' than testpoints ',Length(FProcedure.FSets[NS].FTestPoints.FValues));
                  Continue;
                End;
              For NP := 0 to Length(FProcedure.FSets[NS].FTestPoints.FValues)-1 do
                Begin
                  if not assigned(FProcedure.FSets[NS].FMeasurements[NI][NP]) then
                    Begin
                      WriteLn('    No measurement');
                      Continue;
                    End;
                  S.Add(Instrument.FName+'.Measurements['+FloatToStr(Range.FMaxValue)+','+FloatToStr(FProcedure.FSets[NS].FTestPoints.FValues[NP])+'] = '''+FProcedure.FSets[NS].FMeasurements[NI][NP].Encode+''' # '+FProcedure.FSets[NS].FMeasurements[NI][NP].ToString);
{$IFDEF TestJSONStreamer}
                  // test with JSON
                  WriteLn('#### Measurement Result to JSON ####');
                  WriteLn('Original        : ', FProcedure.FSets[NS].FMeasurements[NI][NP].ToString);
                  Streamer := TJSONStreamer.Create(nil);
                  Streamer.BeforeStreamObject := @MyBeforeStreamObject;
                  JSON := Streamer.ObjectToJSON(FProcedure.FSets[NS].FMeasurements[NI][NP]);
                  JSONString := JSON.AsJSON;  // O.FormatJSON;
                  WriteLn(JSONString);
                  Streamer.Free;
                  WriteLn('#### JSON to Measurement Result ####');
                  DeStreamer := TJSONDeStreamer.Create(nil);
                  DeStreamer.OnGetObject := @MyGetObject;
                  St := (JSON as TJSONObject).Get('ClassName', '');
                  M := TMeasurementResultBase(MeasurementResultFactory.CreateObject(St));
                  if M = Nil then
                    raise Exception.Create('Unknown class '+St);
                  DeStreamer.JSONToObject(JSONString, M);
                  WriteLn('After unwrapping: ', M.ToString);
                  M.Free;
                  DeStreamer.Free;
                  WriteLn('#### End of JSON Tests #############');
{$ENDIF TestJSONStreamer}
                End;
            End;
        End;
    End;
  //S.Add('(Set 0) # Ranges up to 0.02');
  //S.Add('E36313A_0 = [0.0123 0.0122 0.0123 0.0122 0.0123 0.0144 ...]');
  //S.Add('Fluke177_0 = [...]');
  //S.Add('# each value corresponds to the testpoint');
  //S.Add('# alternative:');
  //S.Add('# E36313A_0(6.0, 0.0) = 0.0123 # parameters are range and testpoint');
  //S.Add('...');

  // save
  S.SaveToFile(AFilename);
  S.Free;
End;

Procedure ParseInstCmp(Filename:String); forward;

Var NewFactory    : TInstrumentWrapperFactory;
    NewComparison : TComparisonBase;
    NewSection    : String;
    NewSubSection : String;
    NewSubSecNum  : Integer;
    NewParamValue : TParamValueBase;
    TmpParamValue : TParamValueBase;
    NewParams     : TInstrumentWrapperParams;
    NewDoubles    : TDynDoubleArray;
    NewCompSet    : TComparisonSet;
    TmpInteger    : Integer;
    TmpDouble     : Double;
    TmpDouble2    : Double;
    TmpInstIdx    : Integer;
    TmpRangeIdx   : Integer;

Constructor TComparisonBase.Load(AFilename : String);
Begin
  inherited Create;
  FQuantity := qtDCV;
  SetLength(FInstruments, 0);
  // read file
  NewFactory    := InstrumentWrapperFactory;
  NewComparison := Self;
  ParseInstCmp(AFilename);  // this will fill everyting into NewComparison
  //if assigned(FProcedure) then
  //  FProcedure.PrintRanges;
  if assigned(FProcedure) then
    FProcedure.Analyze;
End;

// helper function for parser: find maximum value in an array of doubles
Function MaxDoubleValue(ADoubles:TDynDoubleArray) : Double;
Var I : Integer;
Begin
  Result := MinDouble;
  For I := 0 to length(ADoubles)-1 do
    if ADoubles[I] > Result then
      Result := ADoubles[I];
End;

// takes NewParamValue as TParamValueDouble and TParamValueInteger, raises an Exception if it is something else, and FreeAndNil()s it
Function GetParamDouble : Double;
Begin
  if      NewParamValue is TParamValueDouble then
    Result := TParamValueDouble(NewParamValue).FValue
  else if NewParamValue is TParamValueInteger then
    Result := TParamValueInteger(NewParamValue).FValue
  else
    raise Exception.Create('Invalid type '+NewParamValue.ClassName+' of value '+NewParamValue.ToSyntax);
  FreeAndNil(NewParamValue);
End;

{ $ D EFINE yydebug}
{$INCLUDE instcmp-parser.pas}
{$INCLUDE instcmp-lex.pas}

Function MyYyWrap() : Boolean;
Begin
  Close(yyinput);
  Result := true;
End;

(**
 * Parse InstCmp file
 *
 *)
Procedure ParseInstCmp(Filename:String);
Begin
  Assign(yyinput,Filename);
  {$I-} Reset(yyinput); {$I+}
  if IOResult <> 0 then
    Begin
      raise Exception.Create('Error: File '+Filename+' not found.');
    End;

  // initialization
  NewSection    := '';
  NewSubSection := '';
  NewSubSecNum  := -1;
  NewParamValue := Nil;
  TmpParamValue := Nil;
  NewParams     := Nil;
  SetLength(NewDoubles, 0);
  NewCompSet    := Nil;

  yylineno := 0;
  yyclear;
  yywrap:= @MyYyWrap;
  //try
    yyparse();
//  except
//    on E : Exception do
//      Begin
//        WriteLn(E.Message);
//{$if defined(FPC_ANSI_TEXTFILEREC) or not(defined(FPC_HAS_FEATURE_WIDESTRINGS))}
//        WriteLn(PChar(@(TextRec(yyinput).name))+':'+IntToStr(yylineno)+':'+IntToStr(yycolno)+': '+E.Message);
////        E.Message := PChar(@(TextRec(yyinput).name))+':'+IntToStr(yylineno)+':'+IntToStr(yycolno)+': '+E.Message;
//{$else}
//WriteLn(AnsiString(PUnicodeChar(@(TextRec(yyinput).name)))+':'+IntToStr(yylineno)+':'+IntToStr(yycolno)+': '+E.Message);
////        E.Message := AnsiString(PUnicodeChar(@(TextRec(yyinput).name)))+':'+IntToStr(yylineno)+':'+IntToStr(yycolno)+': '+E.Message;
//{$endif};
//        raise;
//      End;
//  End;
//  yylex_destroy();

  // yyparse() also calls yywrap(), which is a procedure variable, which
  // originally pointed to yylex_yywrap(), which closes yyinput (so we don't
  // have to close it here) and yyoutput (which initially didn't harm, but when
  // using this function twice, WriteLn doesn't have an open output handle any
  // more).
  // Therefore we make our own MyYyWrap() function which only closes yyinput
  // but not yyoutput.
End;



{$IFDEF CreateMeasureDefinitionFirstAttempt}

{ TRangeInfo }

Constructor TRangeInfo.Create(AInstrument : TInstrumentDescriptor; AQuantity : TQuantity; ARangeIdx : Integer; AAccuracyIdx : Integer);
Begin
  inherited Create;
  FInstrument  := AInstrument;
  FQuantity    := AQuantity;
  FRangeIdx    := ARangeIdx;
  FAccuracyIdx := AAccuracyIdx;
End;

Function TRangeInfo.GetRange : TMeasureRangeBase;
Begin
  Result := FInstrument.FRanges[FQuantity][FRangeIdx];
End;

{ TRangesList }

constructor TRangesList.Create;
Begin
  inherited Create;
  FRangeInfos := TRangeInfoList.Create(False);
End;

Procedure TRangesList.Add(ARangeInfo : TRangeInfo);
Begin
  FRangeInfos.Add(ARangeInfo);
End;

Function CompareRangeInfo(const Item1, Item2: TRangeInfo) : Integer;
Begin
  Result := CompareValue(
    Item1.FInstrument.FRanges[Item1.FQuantity][Item1.FRangeIdx].FMaxValue,
    Item2.FInstrument.FRanges[Item2.FQuantity][Item2.FRangeIdx].FMaxValue);
End;

Procedure TRangesList.Sort;
Begin
  FRangeInfos.Sort(@CompareRangeInfo);
End;

{$ENDIF} // CreateMeasureDefinitionFirstAttempt

End.

