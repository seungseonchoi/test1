{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   Export CAD to SWF                        }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit CADtoSWF;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
  SysUtils, Classes, Math, CADExport, CADImage,
  DXFConv,
{$IFDEF SG_FIREMONKEY}
  System.Generics.Collections,
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types, System.Devices,
{$ELSE}
  Graphics,
{$ENDIF}
  sgConsts
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF}
  ;

//{$DEFINE SG_DEBUG}

type
  // Multibyte words are stored in little (or big)-endian format with
  //   the least (or most) significant byte in a word stored first
  TsgByteOrder = (boLittleEndian, boBigEndian);

  TsgInteger = {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF};
  TsgByte = ShortInt;
  PsgByte = PShortInt;

  TsgBytesList = class
  private
    FCapacity: TsgInteger;
    FData: PsgByte;
    FSize: TsgInteger;
    procedure Grow;
    function GetItem(Index: TsgInteger): TsgByte;
    procedure SetItem(Index: TsgInteger; const Value: TsgByte);
    procedure SetSize(const Value: TsgInteger);
    procedure SetCapacity(const Value: TsgInteger);
  public
    constructor Create(const ASize: TsgInteger);
    constructor CreateEx(const ASize, ACapacity: TsgInteger);
    destructor Destroy; override;
    procedure Add(const Value: TsgByte);
    procedure Assign(ASource: TsgBytesList);
    procedure Clear;
    procedure CompessFrom(const AIndex: TsgInteger);
    procedure CopyToStream(AStream: TStream);
    procedure Cut(const AIndex, ACount: TsgInteger);
    procedure Write(const AIndex: TsgInteger; const Arr: array of TsgByte);
    procedure WriteMemory(const Ptr: Pointer; const AIndex, ACount: Integer);
    property Items[Index: TsgInteger]: TsgByte read GetItem write SetItem; default;
    property Size: TsgInteger read FSize write SetSize;
    property Capacity: TsgInteger read FCapacity write SetCapacity;
  end;

  TsgCADtoSWF = class(TsgSimpleExport)
  private
    FBeginCurrentShapePtr: Integer;
    FBitPtr: Integer;
    FByteOrder: TsgByteOrder;
    FBytesList: TsgBytesList;
    FCurrentPosition: TPoint;
    FID: Integer;
    FLayer: Integer;
    FNumberOfFillStyleBits: Integer;
    FNumberOfLineStyleBits: Integer;
    FVersion: TsgByte;
    procedure AlignToByte;
    function BeginShape: Integer;
    procedure CompressCWS;
    procedure DefineBoundingArea(const Xmin, Ymin, Xmax, Ymax: Integer);
    procedure DefineColor(const AColor: TColor);
    procedure DefineCurrentPoint(const AX, AY: Integer);
    procedure DefineEmptyTransform;//(const AOffsetX, AOffsetY: Integer);
    procedure DefineLine(const AX, AY: Integer);
    procedure DefineStyle(const AHasLine, AHasFill: Boolean;
      const AColor: TColor; const AFillType, AWeight: Integer);
    procedure DefineType(const AType, ASize: Integer);
    //function DoActions(AEntity: TsgDXFEntity): Integer;
    procedure EndShape;
    function GetSignature: AnsiString;
    procedure PlaceObject(const AObjectID, ALayer: Integer);
    procedure PolyPoints(Points: PPoint; Count: Integer; const AClosed: Boolean);
    procedure PutBits(AValue: Integer; ACount: Integer);
    procedure PutBytes(const Ptr: Pointer; Count: Integer);
    procedure PutDouble(AValue: Double);
    procedure PutFixedBits(AValue: Single; const ABitsCount, AFSize: Integer);
    procedure PutFloat(AValue: Single; const AMSize, AFSize: Integer);
    procedure PutWord(AValue: Integer; ACount: Integer);
  protected
    procedure ExpFillRgn(P: PRect; Count: Integer); override;
    procedure ExpPolyline(Points: PPoint; Count: Integer); override;
    procedure ExpPolygon(Points: PPoint; Count: Integer); override;
    procedure ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer); override;
    procedure ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer); override;
    procedure ExpSaveDC; override;
    procedure ExpRestoreDC; override;
    function GenerateID: Integer;
    function GetExportRect: TRect; override;
    procedure PageEnd(N: Integer); override;
    procedure PageStart(N: Integer); override;
    property ID: Integer read FID;
    procedure SaveToStreamCustom(S: TStream); override;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    property Version: TsgByte read FVersion write FVersion;
  end;

implementation

uses
  sgZip, sgFunction;

const
  cnstMinCapacity = 512000;
  cnstByte = 8;
  cnstDblWord = 32;
  cnstObjectSize = $3F;//= 63
  cnstMaxSize = $FFFF;
  { Standard types for Adobe Flash }
  //   Adobe Flash 1
  cnstSWFShowFrame            = 1;
  cnstSWFDefineShape          = 2;
  cnstSWFPlaceObject          = 4;
  cnstSWFRemoveObject         = 5;
  cnstSWFDefineJPEGImage      = 6;
  cnstSWFDefineButton         = 7;
  cnstSWFJPEGTables           = 8;
  cnstSWFSetBackgroundColor   = 9;
  cnstSWFDefineFont           = 10;
  cnstSWFDefineText           = 11;
  cnstSWFDoAction             = 12;
  cnstSWFFontInfo             = 13;
  cnstSWFDefineSound          = 14;
  cnstSWFStartSound           = 15;
  cnstSWFSoundStreamHead      = 18;
  cnstSWFSoundStreamBlock     = 19;
  //   Adobe Flash 2
  cnstSWFButtonSound          = 17;
  cnstSWFDefineImage          = 20;
  cnstSWFDefineJPEGImage2     = 21;
  cnstSWFDefineShape2         = 22;
  cnstSWFButtonColorTransform = 23;
  cnstSWFProtect              = 24;
  //   Adobe Flash 3
  cnstSWFFree                 = 3;
  cnstSWFPlaceObject2         = 26;
  cnstSWFRemoveObject2        = 28;
  cnstSWFDefineShape3         = 32;
  cnstSWFDefineText2          = 33;
  cnstSWFDefineButton2        = 34;
  cnstSWFDefineJPEGImage3     = 35;
  cnstSWFDefineImage2         = 36;
  cnstSWFDefineSprite         = 39;
  cnstSWFFrameLabel           = 43;
  cnstSWFSoundStreamHead2     = 45;
  cnstSWFDefineMorphShape     = 46;
  cnstSWFDefineFont2          = 48;
  //   Adobe Flash 4
  cnstSWFPathsArePostscript   = 25;
  cnstSWFDefineTextField      = 37;
  cnstSWFQuicktimeMovie       = 38;
  cnstSWFSerialNumber         = 41;
  cnstSWFDefineBitsPtr        = 1023;
  //   Adobe Flash 5
  cnstSWFExport               = 56;
  cnstSWFImport               = 57;
  cnstSWFEnableDebugger       = 58;
  //   Adobe Flash 6
  cnstSWFInitialize           = 59;
  cnstSWFDefineVideo          = 60;
  cnstSWFVideoFrame           = 61;
  cnstSWFFontInfo2            = 62;
  cnstSWFEnableDebugger2      = 64;
  //   Adobe Flash 7
  cnstSWFLimitScript          = 65;
  cnstSWFTabOrder             = 66;

  { Clip Events for Adobe Flash }
  cnstEventLoad               = 1;
  cnstEventEnterFrame         = 2;
  cnstEventUnload             = 4;
  cnstEventMouseMove          = 8;
  cnstEventMouseDown          = $10;
  cnstEventMouseUp            = $20;
  cnstEventKeyDown            = $40;   //    64
  cnstEventKeyUp              = $80;   //   128
  cnstEventData               = $100;  //   256
  cnstEventInitialize         = $200;  //   512
  cnstEventPress              = $400;  //  1024
  cnstEventRelease            = $800;  //  2048
  cnstEventReleaseOut         = $1000; //  4096
  cnstEventRollOver           = $2000; //  8192
  cnstEventRollOut            = $4000; // 16384
  cnstEventDragOver           = $8000; // 32768
  cnstEventDragOut            = $10000;// 65536
  cnstEventKeyPress           = $20000;//131072
  cnstEventConstruct          = $40000;//262144

  { DefineFunction2 flags }
  cnstPreloadParent      = 1;    // Preload "_parent" into register
  cnstPreloadRoot        = 2;    // Preload "_root" into register
  cnstSuppressSuper      = 4;    // Don’t create "super" variable
  cnstPreloadSuper       = 8;    // Preload "super" into register
  cnstSuppressArguments  = 16;   // Don’t create "arguments" variable
  cnstPreloadArguments   = 32;   // Preload "arguments" into register
  cnstSuppressThis       = 64;   // Don’t create "this" variable
  cnstPreloadThis        = 128;  // Preload "this" into register
  cnstPreloadGlobal      = 32768;// Preload "_global" into register

type
  TswfActions = (
    // simple byte-codes stack-based operations...
    aEnd, aUndef1, aUndef2, aUndef3, aNextFrame,
    aPrevFrame, aPlay, aStop, aToggleQuality, aStopSounds,
    { Flash 4 }
    aIntegerAdd, aSubtract, aMultiply, aDivide, aIntegerEquals,
    aIntegerLess, aAnd, aOr, aNot, aStringEquals, aStringLength,
    aStringExtract, aUndef22, aPop, aToInteger, aUndef25, aUndef26,
    aUndef27, aGetVariable, aSetVariable, aUndef30, aUndef31,
    aSetTarget2, aStringAdd, aGetProperty, aSetProperty, aCloneSprite,
    aRemoveSprite, aTrace, aStartDrag, aEndDrag, aStringLess,
    {//Flash 7} aThrow, aCast, aImplements, aUndef45, aUndef46, aUndef47, {\\}
    aRandomNumber, aMBStringLength, aCharToAscii, aAsciiToChar, aGetTime,
    aMBStringExtract, aMBCharToAscii, aMBAsciiToChar,
    { Flash 5 }
    aUndef56, aUndef57, aDeleteVariable, aDelete, aInitVariable,
    aExecuteFunction, aReturn, aModulo, aNamedObject, aNewVariable,
    aNewArray, aNewObject, aGetType, aGetTarget, aEnumerate, aAdd,
    aLess, aEquals, aToNumber, aToString, aDuplicate, aSwap, aGetAttribute,
    aSetAttribute, aIncrement, aDecrement, aExecuteMethod, aNewMethod,
    {// Flash 6 } aInstanceOf, aEnumerateObject, aUndef86, aUndef87, aUndef88, {\\}
    aUndef89, aUndef90, aUndef91, aUndef92, aUndef93, aUndef94, aUndef95,
    aBitwiseAnd, aBitwiseOr, aBitwiseXOr, aLogicalShiftLeft,
    aArithmeticShiftRight, aLogicalShiftRight,
    { Flash 6 }
    aStrictEquals, aGreater, aStringGreater, aExtends,
    aUndef106, aUndef107, aUndef108, aUndef109, aUndef110, aUndef111, aUndef112,
    aUndef113, aUndef114, aUndef115, aUndef116, aUndef117, aUndef118, aUndef119,
    aUndef120, aUndef121, aUndef122, aUndef123, aUndef124, aUndef125, aUndef126,
    // large number byte-codes stack-based operations...
    aUndef127, aUndef128, aGotoFrame, aUndef130, aGetUrl, aUndef132, aUndef133,
    { Flash 5}
    aUndef134, aRegisterCopy, aTable, aUndef137,
    { Flash 3}
    aWaitForFrame, aSetTarget, aGotoLabel,
    { Flash 4}
    aWaitForFrame2,
    { Flash 7 }
    aDefineFunction2, aExceptionHandler, aUndef144, aUndef145, aUndef146, aUndef147,
    { Flash 5}
    aWith, aUndef149,
    { Flash 4}
    aPush, aUndef151, aUndef152, aJump, aGetUrl2, { //Flash 5}aDefineFunction, {\\}
    aUndef156, aIf, aCall, aGotoFrame2);

  TswfButtonStates = (bsUp, bsOver, bsDown, bsActive);

  TswfPushTypes = (psString, psProperty, psNull, psUndefined, psRegisterNumber,
    psBoolean, psDouble, psInteger, psPoolIndexLess256, psPoolIndexMoore256);

  TswfPushRecord = class
  protected
    function GetValueType: TswfPushTypes; virtual;
  public
    function GetValue: Integer; //virtual;
    property ValueType: TswfPushTypes read GetValueType;
  end;

  TswfString = class(TswfPushRecord)
  private
    FValue: string;
  protected
    function GetValueType: TswfPushTypes; override;
  public
    constructor Create(const AValue: string);
    function GetValue: string; //reintroduce; overload;
  end;

  TswfProperty = class(TswfPushRecord)
  private
    FValue: Integer;
  protected
    function GetValueType: TswfPushTypes; override;
  public
    constructor Create(const AValue: Integer);
    function GetValue: Integer; //reintroduce; overload;
  end;

  TswfBoolean = class(TswfPushRecord)
  private
    FValue: Boolean;
  protected
    function GetValueType: TswfPushTypes; override;
  public
    constructor Create(const AValue: Boolean);
    function GetValue: Boolean; //reintroduce; overload;
  end;

  TswfDouble = class(TswfPushRecord)
  private
    FValue: Double;
  protected
    function GetValueType: TswfPushTypes; override;
  public
    constructor Create(const AValue: Double);
    function GetValue: Double; //reintroduce; overload;
  end;

  TswfInteger = class(TswfPushRecord)
  private
    FValue: Integer;
  protected
    function GetValueType: TswfPushTypes; override;
  public
    constructor Create(const AValue: Integer);
    function GetValue: Integer; //reintroduce; overload;
  end;

  TswfCustomList = class({$IFDEF SG_FIREMONKEY}TList<TObject>{$ELSE}TList{$ENDIF})
  public
    destructor Destroy; override;
  end;

  TswfStackList = class(TswfCustomList)
  end;

  TswfObject = class
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; virtual;
  public
    procedure Translate(ACADtoSWF: TsgCADtoSWF); virtual;
  end;

  TswfObjectsList = class(TswfCustomList)
  end;

  TswfDataObject = class(TswfObject)
  protected
    function GetType: Integer; virtual;
  public
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
  end;

  TswfRegisterVariable = class(TswfObject)
  private
    FName: string;
    FNumber: Integer;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property Name: string read FName write FName;
    property Number: Integer read FNumber write FNumber;
  end;

  TswfAction = class(TswfObject)
  private
    FActionType: TswfActions;
  protected
    function GetLength: Integer;
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    constructor Create(AAction: TswfActions);
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property ActionType: TswfActions read FActionType write FActionType;
  end;

  TswfActionsList = class(TswfCustomList)
  end;

  TswfButton = class(TswfObject)
  private
    FLayer: Integer;
    FShapeID: Integer;
    FState: TswfButtonStates;
    function GetStateCode: Integer;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    constructor Create(const AState: TswfButtonStates);
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property Layer: Integer read FLayer write FLayer;
    property ShapeID: Integer read FShapeID write FShapeID;
    property State: TswfButtonStates read FState write FState;
    property StateCode: Integer read GetStateCode;
  end;

  TswfButtonsList = class(TswfCustomList)
  end;

  TswfContainer = class
  private
    FActions: TswfActionsList;
  protected
    function GetAction(const AIndex: Integer): TswfAction;
    function GetActionsCount: Integer;
    function GetActionsSize(ACADtoSWF: TsgCADtoSWF): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAction(AAction: TswfAction);
    procedure DeleteAction(AAction: TswfAction);
    property Actions[const AIndex: Integer]: TswfAction read GetAction;
    property ActionsCount: Integer read GetActionsCount;
  end;

  TswfDoAction = class(TswfObject)
  private
    FContainer: TswfContainer;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property Container: TswfContainer read FContainer;
  end;

  TswfDefineFunction2 = class(TswfAction)
  private
    FArguments: TList{$IFDEF SG_FIREMONKEY}<TswfRegisterVariable>{$ENDIF};
    FContainer: TswfContainer;
    FFlags: Integer;
    FName: string;
    FRegisterCount: Integer;
    function GetArgument(const AIndex: Integer): TswfRegisterVariable;
    function GetArgumentsCount: Integer;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property Arguments[const AIndex: Integer]: TswfRegisterVariable read GetArgument;
    property ArgumentsCount: Integer read GetArgumentsCount;
    property Container: TswfContainer read FContainer;
    property Flags: Integer read FFlags write FFlags;
    property Name: string read FName write FName;
  end;

  TswfClipEvent = class(TswfObject)
  private
    FContainer: TswfContainer;
    FEvent: Integer;
    //FKey: Integer;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property Container: TswfContainer read FContainer;
    property Event: Integer read FEvent write FEvent;
  end;

  TswfDefinition = class(TswfDataObject)
  private
    FID: Integer;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
  end;

  TswfDefineButton = class(TswfDefinition)
  private
    FActions: TswfActionsList;
    FButtons: TswfButtonsList;
    function GetAction(const AIndex: Integer): TswfAction;
    function GetActionsCount: Integer;
    function GetButton(const AIndex: Integer): TswfButton;
    function GetButtonsCount: Integer;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
    function GetType: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAction(AAction: TswfAction);
    procedure AddButton(AButton: TswfButton);
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property Actions[const AIndex: Integer]: TswfAction read GetAction;
    property ActionsCount: Integer read GetActionsCount;
    property Buttons[const AIndex: Integer]: TswfButton read GetButton;
    property ButtonsCount: Integer read GetButtonsCount;
  end;

  TswfDefineSprite = class(TswfDefinition)
  private
    FObjects: TswfObjectsList;
    function GetFramesCount: Integer;
    function GetObject(const AIndex: Integer): TswfObject;
    function GetObjectsCount: Integer;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
    function GetType: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObject(AObject: TswfObject);
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property Objects[const AIndex: Integer]: TswfObject read GetObject;
    property ObjectsCount: Integer read GetObjectsCount;
  end;

  TswfPush = class(TswfAction)
  private
    FStack: TswfStackList;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ARecord: TswfPushRecord); overload;
    procedure Add(const AString: string); overload;
    procedure Add(const ABoolean: Boolean); overload;
    procedure Add(const ADouble: Double); overload;
    procedure Add(const AInteger: Integer); overload;
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
  end;

  TswfShowFrame = class(TswfDataObject)
  protected
    function GetType: Integer; override;
  end;

  PswfMatrix = ^TswfMatrix;
  TswfMatrix = array[0..2, 0..2] of Single;

  TswfTransformType = (ttOffset, ttRotation, ttScaling);
  TswfTransformation = class(TswfObject)
  private
    FMatrix: TswfMatrix;
    procedure Multiply(const AMat: PswfMatrix);
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
    function GetMinBitsCount(const ATransformType: TswfTransformType): Integer;
    function HasTransform(const ATransformType: TswfTransformType): Boolean;
  public
    procedure SetAngle(const AAngle: Single);
    procedure SetScale(const AX, AY: Single);
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property Matrix: TswfMatrix read FMatrix write FMatrix;
  end;  

  TswfPlaceObject2 = class(TswfObject)
  private
    FClipEvents: TList;
    FLayer: Integer;
    FFlag: Byte;
    FName: string;
    FOwnerID: Integer;
    FRatio: Single;
    FTransformation: TswfTransformation;
    function GetClipEvent(const AIndex: Integer): TswfClipEvent;
    function GetClipEventsCount: Integer;
    function HasClipEvents: Boolean;
    function HasRatio: Boolean;
    function HasTransformation: Boolean;
    function HasName: Boolean;
  protected
    function GetSize(ACADtoSWF: TsgCADtoSWF): Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Translate(ACADtoSWF: TsgCADtoSWF); override;
    property ClipEvents[const AIndex: Integer]: TswfClipEvent read GetClipEvent;
    property ClipEventsCount: Integer read GetClipEventsCount;
    property Name: string read FName write FName;
  end;

{ Base functions}

function GetMinBitsCountI(AValue: Integer; const AIsSigned: Boolean): Integer;
var
  I: Integer;
  vSignMask: Cardinal;

  procedure ApplyMask;
  begin
    while ((AValue and vSignMask) = 0) and (I > 0) do
    begin
      vSignMask := vSignMask shr 1;
      Dec(I);
    end;
    Result := I;
  end;
begin
  Result := 0;
  I := cnstDblWord;
  vSignMask := $80000000;
  if AIsSigned then
  begin
    if AValue < 0 then
      AValue := -AValue;
    ApplyMask;
    Inc(Result, Ord(I < cnstDblWord));
  end
  else
    ApplyMask;
end;

function GetMinBitsCountIByArr(const AValues: array of Integer; const AIsSigned: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(AValues) to High(AValues) do
    Result := MaxI(Result, GetMinBitsCountI(AValues[I], AIsSigned));
end;

function GetMinBitsCountF(AValue: Double): Integer;
begin
  Result := GetMinBitsCountI(Trunc(AValue* 65536.0), True);
end;

function GetMinBitsCountFByArr(const AValues: array of Double): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(AValues) to High(AValues) do
    Result := MaxI(Result, GetMinBitsCountF(AValues[I]));
end;

{ TsgCADtoSWF }

procedure TsgCADtoSWF.CompressCWS;
begin
  FBytesList.CompessFrom(8);// 'CWS' version uses ZLib algorithm for compressing data
end;

constructor TsgCADtoSWF.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  FVersion := 7;
  FLayer := 1;
  FByteOrder := boLittleEndian;
  FBytesList := TsgBytesList.CreateEx(0, cnstMinCapacity);
  FNumberOfFillStyleBits := 1;
  FNumberOfLineStyleBits := 1;
  //LayoutExportMode := lemModel;// demo works only for Model layout
end;

procedure TsgCADtoSWF.ExpFillRgn(P: PRect; Count: Integer);
begin
  while Count > 0 do
  begin
    DefineCurrentPoint(P.Left, P.Top);
    DefineLine(P.Left, P.Bottom);
    DefineLine(P.Right, P.Bottom);
    DefineLine(P.Right, P.Top);
    DefineLine(P.Left, P.Top);
    Inc(P);
    Dec(Count);
  end;
end;

procedure TsgCADtoSWF.ExpPolyline(Points: PPoint; Count: Integer);
begin
  DefineStyle(True, False, GetStrokeColor, 0, 0{LINEWEIGHT});
  PolyPoints(Points, Count, False);
end;

procedure TsgCADtoSWF.ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer);
var
  PP: PPoints;
  PC: PInteger;
  C: Integer;
begin
  DefineStyle(False{True}, True, GetFillColor, 0{SOLID}, 0);
  PP := PPoints(@Points);
  PC := Counts;
  while Count > 0 do
  begin
    C := PC^; Inc(PC);
    PolyPoints(PPoint(PP), C, True);
    Inc(PPoint(PP), C);
    Dec(Count);
  end;
end;

procedure TsgCADtoSWF.ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer);
var
  PP: PPoints;
  PC: PInteger;
  C: Integer;
begin
  DefineStyle(True, False, GetStrokeColor, 0, 0{LINEWEIGHT});
  PP := PPoints(@Points);
  PC := Counts;
  while Count > 0 do
  begin
    C := PC^; Inc(PC);
    PolyPoints(PPoint(PP), C, False);
    Inc(PPoint(PP), C);
    Dec(Count);
  end;
end;

procedure TsgCADtoSWF.ExpPolygon(Points: PPoint; Count: Integer);
begin
  DefineStyle(False{True}, True, GetFillColor, 0{SOLID}, 0);
  PolyPoints(Points, Count, True);
end;

procedure TsgCADtoSWF.ExpSaveDC;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

function TsgCADtoSWF.GenerateID: Integer;
begin
  Inc(FID);     // increments unique identifier of the object
  Result := FID;// returns new identifier
end;

function TsgCADtoSWF.GetExportRect: TRect;
begin
  Result := inherited GetExportRect;
  // Increase Drawing Rectangle
  InflateRect(Result, 1, 1);
  // Undo value increasing if negative (bug fixing)
  if Result.Left < 0 then
    Result.Left := Result.Left + 1;
  if Result.Top < 0 then
    Result.Top := Result.Top + 1;
end;

function TsgCADtoSWF.GetSignature: AnsiString;
begin
  if (FVersion > 5) then
    Result := 'CWS' // Uses ZLib algorithm for compressing data
  else
    Result := 'FWS';
end;

procedure TsgCADtoSWF.ExpRestoreDC;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCADtoSWF.AlignToByte;
begin
  FBitPtr := (FBitPtr+7) and (not 7);
end;

{ TsgCADtoSWF.BeginShape

  Begins of the adding of a Shape with new FID.              }
function TsgCADtoSWF.BeginShape: Integer;
var
  vBoundsRect: TRect;
begin
  Result := GenerateID;
  FBeginCurrentShapePtr := FBitPtr;// remembers current pointer before shape writing
  // Common data
  DefineType(cnstSWFDefineShape, cnstObjectSize);// + 48 bit = dummy shape length (is changed after)
  PutWord(Result, 2);
  vBoundsRect := GetExportRect;
  DefineBoundingArea(vBoundsRect.Left, vBoundsRect.Top, vBoundsRect.Right, vBoundsRect.Bottom);
  PutWord(1, 1);// 1 fillstyle
  PutWord(0, 1);                //... fillstyle type
  DefineColor(BackgroundColor); //... fillstyle color
  PutWord(1, 1);// 1 linestyle
  PutWord(0, 2);                //... linestyle width
  DefineColor(BackgroundColor); //... linestyle color
  // Shapes data
  PutBits(FNumberOfFillStyleBits, 4);
  PutBits(FNumberOfLineStyleBits, 4);
  // ... entities...
end;

procedure TsgCADtoSWF.EndShape;
var
  vTmpBitPtr: Integer;
  vLen: TsgInteger;
begin
  // ... entities...
  PutBits(0, 6);// end of shape
  AlignToByte;
  vLen := (FBitPtr - FBeginCurrentShapePtr - 48{previous Type&Length}) shr 3;
  if vLen <> cnstObjectSize then
  begin
    vTmpBitPtr := FBitPtr;
    FBitPtr := FBeginCurrentShapePtr;
    DefineType(cnstSWFDefineShape, vLen);// change dummy length (see BeginShape)!
    if vLen < cnstObjectSize then
    begin
      // change to small length!
      FBytesList.Cut(FBitPtr shr 3, 4);
      //FBytesList.Size := FBitPtr shr 3 + vLen;
      vTmpBitPtr := vTmpBitPtr - cnstDblWord;
    end;
    FBitPtr := vTmpBitPtr;
  end;
end;

procedure TsgCADtoSWF.DefineBoundingArea(const Xmin, Ymin, Xmax, Ymax: Integer);
var
  vBitsCnt: Integer;
begin
  vBitsCnt := GetMinBitsCountIByArr([Xmin, Ymin, Xmax, Ymax], True);
  AlignToByte;
  PutBits(vBitsCnt, 5);
  PutBits(Xmin, vBitsCnt);
  PutBits(Xmax, vBitsCnt);
  PutBits(Ymin, vBitsCnt);
  PutBits(Ymax, vBitsCnt);
  AlignToByte;
end;

procedure TsgCADtoSWF.DefineColor(const AColor: TColor);
begin
  PutWord(AColor and $000000FF, 1);         // red
  PutWord((AColor and $0000FF00) shr 8, 1); // green
  PutWord((AColor and $00FF0000) shr 16, 1);// blue
end;

procedure TsgCADtoSWF.DefineCurrentPoint(const AX, AY: Integer);
var
  vBitsCnt: Integer;
begin
  // By analogy with TsgCADtoSWF.DefineStyle but only defines offset of "cursor"
  PutBits(0, 1);
  PutBits(0, 1);
  PutBits(0, 1);
  PutBits(0, 1);
  PutBits(0, 1);
  PutBits(1, 1);
  // Code below only for one offset "cursor"
  vBitsCnt := GetMinBitsCountIByArr([AX, AY], True);
  PutBits(vBitsCnt, 5);
  PutBits(AX, vBitsCnt);
  PutBits(AY, vBitsCnt);
  FCurrentPosition := Point(AX, AY);
end;

(* procedure TsgCADtoSWF.DefineTransform(const AOffsetX, AOffsetY: Integer);
var
  vBitsCnt: Integer;
begin
  vBitsCnt := 0;
  if (AOffsetX <> 0) or (AOffsetY <> 0) then
    vBitsCnt := GetMinBitsCountByArr([AOffsetX, AOffsetY], True);
  AlignToByte;
  PutBits(0, 1);// does not contains Scale
  PutBits(0, 1);// does not contains Rotation
  PutBits(vBitsCnt, 5);
  PutBits(AOffsetX, vBitsCnt);
  PutBits(AOffsetY, vBitsCnt);
  AlignToByte;
end;                                                                         *)

procedure TsgCADtoSWF.DefineEmptyTransform;
begin
  AlignToByte;
  PutBits(0, 1);// does not contains Scale
  PutBits(0, 1);// does not contains Rotation
  PutBits(0, 5);// does not contains Offset
  PutBits(0, 0);
  PutBits(0, 0);
  AlignToByte;
end;

procedure TsgCADtoSWF.DefineLine(const AX, AY: Integer);
var
  vIsGeneral, vIsVertical: Boolean;
  vX, vY, vBitsCnt: Integer;
begin
  if (AX = FCurrentPosition.X) and (AY = FCurrentPosition.Y) then Exit;
  vX := AX - FCurrentPosition.X;
  vY := AY - FCurrentPosition.Y;
  vIsGeneral := (vX <> 0) and (vY <> 0);
  vIsVertical := vX = 0;
  vBitsCnt := GetMinBitsCountIByArr([vX, vY, 1], True);
  PutBits(1, 1);
  PutBits(1, 1);
  PutBits(vBitsCnt-2, 4);
  PutBits(Ord(vIsGeneral), 1);
  if vIsGeneral then
  begin
    PutBits(vX, vBitsCnt);
    PutBits(vY, vBitsCnt);
  end
  else
  begin
    PutBits(Ord(vIsVertical), 1);
    PutBits(Ord(not vIsVertical)*vX + Ord(vIsVertical)*vY, vBitsCnt);
  end;
  FCurrentPosition := Point(AX, AY);
end;

procedure TsgCADtoSWF.DefineStyle(const AHasLine, AHasFill: Boolean;
  const AColor: TColor; const AFillType, AWeight: Integer);
begin
  PutBits(0, 1);
  PutBits(Ord(AHasLine or AHasFill), 1);// contains styles
  PutBits(Ord(AHasLine), 1);            // contains line styles
  PutBits(0, 1);                        // not contains alt fill styles
  PutBits(1, 1);                        // contains fill styles always (including empty fill)
  PutBits(0, 1);                        // not contains offset "cursor"
  // Code below is for one fill- and/or one line-style only (without AltFill and Offset)
  PutBits(Ord(AHasFill), FNumberOfFillStyleBits);//sets fillstyle: 0 or 1 (0 for empty fill; if 0 not set SWF uses previous fillstyle)
  if AHasLine then
    PutBits(1, FNumberOfLineStyleBits);// sets first linestyle
  AlignToByte;
  // One fillstyle and one linestyle must be exported always
  PutBits(1{Ord(AHasFill)}, 8);
  PutWord(AFillType, 1);// AFillType = 0 if "SOLID"
  DefineColor(AColor);
  PutBits(1{Ord(AHasLine)}, 8);
  PutWord(AWeight, 2);
  DefineColor(AColor);
  //FNumberOfFillStyleBits := 1;
  //FNumberOfLineStyleBits := 1;
  PutBits(FNumberOfFillStyleBits, 4);
  PutBits(FNumberOfLineStyleBits, 4);
end;

procedure TsgCADtoSWF.DefineType(const AType, ASize: Integer);
begin
  if ASize < cnstObjectSize then
    PutWord((AType shl 6) or ASize, 2)
  else
  begin
    PutWord((AType shl 6) or cnstObjectSize, 2);
    PutWord(ASize, 4);
  end;
end;

destructor TsgCADtoSWF.Destroy;
begin
  FreeAndNil(FBytesList);
  inherited Destroy;
end;

(*
function TsgCADtoSWF.DoActions(AEntity: TsgDXFEntity): Integer;
var
  //vAct: TswfAction;
  vDefBtn: TswfDefineButton;
  vUpShape, vOverShape{, vDownShape}: Integer;
  vTmpColor: TColor;

  function GetButton(const AState: TswfButtonStates; const AID: Integer): TswfButton;
  begin
    Result := TswfButton.Create(AState);
    Result.Layer := FLayer;
    Result.FShapeID := AID;
    Inc(FLayer);
  end;
begin
  Result := 0;
  if (AEntity.Layer <> nil) and (AnsiCompareText(AEntity.Layer.Name, 'SWF') = 0) then
  begin
    // Shapes
    vUpShape := BeginShape;
    DrawEntity(AEntity);
    EndShape;

    vOverShape := BeginShape;
    vTmpColor := AEntity.Color;
    AEntity.Color := ((not AEntity.Color) and $00FFFFFF) + $00000055;// inverts color
    DrawEntity(AEntity);
    AEntity.Color := vTmpColor;
    EndShape;

    // Button
    vDefBtn := TswfDefineButton.Create;
    vDefBtn.FID := GenerateID;
    vDefBtn.AddButton(GetButton(bsActive, vUpShape));
    //vDefBtn.AddButton(GetButton(bsUp, vUpShape));
    vDefBtn.AddButton(GetButton(bsOver, vOverShape));
    //vDefBtn.AddButton(GetButton(bsDown, vOverShape));
    vDefBtn.Translate(Self);
    PlaceObject(vDefBtn.FID, FLayer);
    Inc(FLayer);
    vDefBtn.Free;
  end;
end;
*)

procedure TsgCADtoSWF.PageEnd(N: Integer);
var
  vShowFrame: TswfShowFrame;
begin
  EndShape;
  PlaceObject(ID, FLayer);
  // Actions test
  //CurrentLayout.Iterate(Converter, DoActions, nil);
  // Show frame //   DefineType(cnstSWFShowFrame, 0);
  vShowFrame := TswfShowFrame.Create;
  vShowFrame.Translate(Self);
  vShowFrame.Free;
end;

procedure TsgCADtoSWF.PageStart(N: Integer);
var
  xMax: Integer;
  vBoundsRect: TRect;
begin
  vBoundsRect := GetExportRect;
  xMax := MaxI(vBoundsRect.Right - vBoundsRect.Left, vBoundsRect.Bottom - vBoundsRect.Top);
  if (xMax <> cnstMaxSize) and (xMax <> 0) then
    XScale := cnstMaxSize/xMax;
  vBoundsRect := GetExportRect;
  DefineBoundingArea(vBoundsRect.Left, vBoundsRect.Top, vBoundsRect.Right, vBoundsRect.Bottom);
  // Frame rate = 1.0
  PutFloat(1.0, 1, 1);
  // Number of frames = 1
  PutWord(1, 2);
  // Background color
  DefineType(cnstSWFSetBackgroundColor, 3);
  DefineColor(BackgroundColor);
  // DefineShape
  BeginShape;
end;

{ TsgCADtoSWF.PlaceObject

  Adds object by ID and Layer.                                        }
procedure TsgCADtoSWF.PlaceObject(const AObjectID, ALayer: Integer);
begin
  DefineType(cnstSWFPlaceObject, 5 {this size correct only for empty transform!});
  PutWord(AObjectID, 2); // object ID
  PutWord(ALayer, 2);    // layer number
  DefineEmptyTransform;
end;

procedure TsgCADtoSWF.PolyPoints(Points: PPoint; Count: Integer;
  const AClosed: Boolean);
var
  X, Y: Integer;
  P: PInteger;
begin
  P := PInteger(Points);
  X := P^; Inc(P);
  Y := P^; Inc(P);
  DefineCurrentPoint(X, Y);
  Dec(Count);
  while Count > 0 do
  begin
    X := P^; Inc(P);
    Y := P^; Inc(P);
    DefineLine(X, Y);
    Dec(Count);
  end;
  if AClosed then
  begin
    P := PInteger(Points);
    X := P^; Inc(P);
    Y := P^; //Inc(P);
    DefineLine(X, Y);
  end;
end;

procedure TsgCADtoSWF.PutBits(AValue: Integer; ACount: Integer);
var
  vByte: ShortInt;// -128..127
begin
{$IFDEF SG_DEBUG}
  if (ACount < 0) or (ACount > cnstDblWord) then
    raise Exception.Create('Amount of bits should be in limits from 1 up to 32.');
{$ENDIF}
  FillChar(vByte, SizeOf(ShortInt), 0); // vByte := 0; // for fixing undefined Delphi error
  AValue := (AValue shl (cnstDblWord - ACount)) shr (FBitPtr mod cnstByte);
  vByte := FBytesList[FBitPtr shr 3];
  AValue := AValue or (vByte shl 24);
  FBytesList.Write(FBitPtr shr 3, [AValue shr 24, AValue shr 16, AValue shr 8, AValue]);

  (*// Full code for line above
  I := 24;
  while I >= 0 do
  begin
    vByte := AValue shr I;
    FStream.Write(vByte, 1);// [Ind]-element of the FStream
    Dec(I, cnstByte);
    //Inc(Ind);
  end;                *)
  Inc(FBitPtr, ACount);
  if FBitPtr > (FBytesList.Size shl 3) then
    FBitPtr := FBytesList.Size shl 3;
end;

procedure TsgCADtoSWF.PutBytes(const Ptr: Pointer; Count: Integer);
begin
  FBytesList.WriteMemory(Ptr, FBitPtr shr 3, Count);
  Inc(FBitPtr, Count shl 3);
end;

procedure TsgCADtoSWF.PutDouble(AValue: Double);
var
  vValue: Int64;
begin
  Move(AValue, vValue, 8);
  PutWord(vValue shr 32, 4);          // high
  PutWord(vValue and $FFFFFFFF, 4);   // low
end;

procedure TsgCADtoSWF.PutFixedBits(AValue: Single; const ABitsCount,
  AFSize: Integer);
begin
  PutBits(Trunc(AValue*(1 shl AFSize)), ABitsCount);
end;

procedure TsgCADtoSWF.PutFloat(AValue: Single; const AMSize, AFSize: Integer);
var
  F, M: Integer;
  vKoef: Single;
begin
  vKoef := 1 shl (AFSize shl 3);
  F := Trunc(AValue*vKoef);
  M := Trunc(AValue);
  PutWord(F, AFSize);
  PutWord(M, AMSize);
end;

procedure TsgCADtoSWF.PutWord(AValue: Integer; ACount: Integer);
var
  I, vBitsCnt: Integer;
  vByte: ShortInt;// -128..127
begin
{$IFDEF SG_DEBUG}
  if (ACount < 0) or (ACount > 4) then
    raise Exception.Create('Amount of bytes should be in limits from 1 up to 4.');
{$ENDIF}
  vBitsCnt := ACount shl 3;
  I := 0;
  if FByteOrder = boLittleEndian then
    while I < vBitsCnt do
    begin
      vByte := AValue;
      FBytesList[(FBitPtr + I) shr 3] := vByte;
      Inc(I, cnstByte);
      AValue := AValue shr cnstByte;
    end
  else
    while I < vBitsCnt do
    begin
      vByte := AValue shr (vBitsCnt-I-cnstByte);// ???
      FBytesList[(FBitPtr + I) shr 3] := vByte;
      Inc(I, cnstByte);
    end;
  Inc(FBitPtr, vBitsCnt);
end;

procedure TsgCADtoSWF.SaveToStreamCustom(S: TStream);
var
  vSignature: AnsiString;
  vPos: Integer;
{$IFDEF SG_DEBUG}
  vTickCount: Cardinal;
{$ENDIF}
begin
{$IFDEF SG_DEBUG}
  vTickCount := GetTickCount;
{$ENDIF}
  Converter.IsCrossoverMatrix := False;
  FBytesList.Clear;
  try
    vSignature := GetSignature;
    PutBytes(PsgByte(vSignature), Length(vSignature));
    PutWord(FVersion, 1);
    vPos := FBitPtr;
    PutWord(FBytesList.Size, 4);// dummy file length (is changed after)
    inherited SaveToStreamCustom(S);// S is an unessential parameter
    PutWord(0, 2);
    FBitPtr := vPos;
    PutWord(FBytesList.Size, 4);// real file length (has been saved before)
    if FVersion > 5 then
      CompressCWS;
    FBytesList.CopyToStream(S);
  finally
    Converter.IsCrossoverMatrix := True;
  end;
{$IFDEF SG_DEBUG}
  MessageBox(0, PChar('TickCount= '+IntToStr(GetTickCount - vTickCount)), 'SWF EXPORT DEBUG MODE', MB_OK or MB_ICONINFORMATION);
{$ENDIF}
end;

{ TswfContainer }

constructor TswfContainer.Create;
begin
  inherited Create;
  FActions := TswfActionsList.Create;
end;

destructor TswfContainer.Destroy;
begin
  FreeAndNil(FActions);
  inherited Destroy;
end;

procedure TswfContainer.AddAction(AAction: TswfAction);
begin
  FActions.Add(AAction);
end;

procedure TswfContainer.DeleteAction(AAction: TswfAction);
begin
  FActions.Remove(AAction);
end;

function TswfContainer.GetAction(const AIndex: Integer): TswfAction;
begin
  if AIndex < FActions.Count then
    Result := TswfAction(FActions[AIndex])
  else
    Result := nil;
end;

function TswfContainer.GetActionsCount: Integer;
begin
  Result := FActions.Count;
end;

function TswfContainer.GetActionsSize(ACADtoSWF: TsgCADtoSWF): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ActionsCount - 1 do
    Inc(Result, Actions[I].GetSize(ACADtoSWF) + Actions[I].GetLength);
end;

{ TswfObject }

function TswfObject.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
begin
  Result := 0;
end;

procedure TswfObject.Translate(ACADtoSWF: TsgCADtoSWF);
begin
  // not implements
end;

{ TswfDataObject }

function TswfDataObject.GetType: Integer;
begin
  Result := 0;
end;

procedure TswfDataObject.Translate(ACADtoSWF: TsgCADtoSWF);
begin
  inherited Translate(ACADtoSWF);
  ACADtoSWF.DefineType(GetType, GetSize(ACADtoSWF));
end;

{ TswfAction }

constructor TswfAction.Create(AAction: TswfActions);
begin
  FActionType := AAction;
end;

function TswfAction.GetLength: Integer;
begin
  Result := 1 + 2*Ord(Ord(ActionType) > 128);
end;

function TswfAction.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
begin
  Result := 0;
end;

procedure TswfAction.Translate(ACADtoSWF: TsgCADtoSWF);
begin
  ACADtoSWF.PutWord(Ord(ActionType), 1);
  if Ord(ActionType) >= 128 then
    ACADtoSWF.PutWord(GetSize(ACADtoSWF), 2);
end;

{ TswfButton }

constructor TswfButton.Create(const AState: TswfButtonStates);
begin
  FState := AState;
end;

function TswfButton.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
begin
  Result := 6;// only for empty transform!
end;

function TswfButton.GetStateCode: Integer;
begin
  case FState of
    bsUp:     Result := 1;
    bsOver:   Result := 2;
    bsDown:   Result := 4;
    bsActive: Result := 8;
  else
    Result := 0;
  end;
end;

procedure TswfButton.Translate(ACADtoSWF: TsgCADtoSWF);
begin
  ACADtoSWF.PutBits(0, 4);
  ACADtoSWF.PutBits(StateCode, 4);
  ACADtoSWF.PutWord(ShapeID, 2);
  ACADtoSWF.PutWord(Layer, 2);
  ACADtoSWF.DefineEmptyTransform;
end;

{ TswfDefineButton }

procedure TswfDefineButton.AddAction(AAction: TswfAction);
begin
  FActions.Add(AAction);
end;

procedure TswfDefineButton.AddButton(AButton: TswfButton);
begin
  FButtons.Add(AButton);
end;

constructor TswfDefineButton.Create;
begin
  FActions := TswfActionsList.Create;
  FButtons := TswfButtonsList.Create;
end;

destructor TswfDefineButton.Destroy;
begin
  FreeAndNil(FActions);
  FreeAndNil(FButtons);
  inherited Destroy;
end;

function TswfDefineButton.GetAction(const AIndex: Integer): TswfAction;
begin
  if AIndex < FActions.Count then
    Result := TswfAction(FActions[AIndex])
  else
    Result := nil;
end;

function TswfDefineButton.GetActionsCount: Integer;
begin
  Result := FActions.Count;
end;

function TswfDefineButton.GetButton(const AIndex: Integer): TswfButton;
begin
  if AIndex < FButtons.Count then
    Result := TswfButton(FButtons[AIndex])
  else
    Result := nil;
end;

function TswfDefineButton.GetButtonsCount: Integer;
begin
  Result := FButtons.Count;
end;

function TswfDefineButton.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
var
  I: Integer;
begin
  Result := inherited GetSize(ACADtoSWF);
  for I := 0 to ButtonsCount - 1 do
    Inc(Result, Buttons[I].GetSize(ACADtoSWF));
  Inc(Result);
  for I := 0 to ActionsCount - 1 do
    Inc(Result, Actions[I].GetSize(ACADtoSWF) + Actions[I].GetLength);
  Inc(Result);
end;

function TswfDefineButton.GetType: Integer;
begin
  Result := cnstSWFDefineButton;
end;

procedure TswfDefineButton.Translate(ACADtoSWF: TsgCADtoSWF);
var
  I: Integer;
begin
  inherited Translate(ACADtoSWF);
  for I := 0 to ButtonsCount - 1 do
    Buttons[I].Translate(ACADtoSWF);
  ACADtoSWF.PutWord(0, 1);
  for I := 0 to ActionsCount - 1 do
    Actions[I].Translate(ACADtoSWF);
  ACADtoSWF.PutWord(0, 1);
end;

{ TswfDefinition }

function TswfDefinition.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
begin
  Result := 2;// ID size
end;

procedure TswfDefinition.Translate(ACADtoSWF: TsgCADtoSWF);
begin
  inherited Translate(ACADtoSWF);
  ACADtoSWF.PutWord(FID{?}, 2);// object definition
end;

{ TswfTransformation }

function TswfTransformation.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
begin
  Result := 7 + 2*GetMinBitsCount(ttOffset);
  if HasTransform(ttScaling) then
    Inc(Result, 5 + 2*GetMinBitsCount(ttScaling));
  if HasTransform(ttRotation) then
    Inc(Result, 5 + 2*GetMinBitsCount(ttRotation));
  if (Result mod cnstByte) > 0 then
    Inc(Result, cnstByte - (Result mod cnstByte));
  Result := Result shr 3;
end;

function TswfTransformation.GetMinBitsCount(
  const ATransformType: TswfTransformType): Integer;
begin
  Result := 0;
  case ATransformType of
    ttOffset:
      if HasTransform(ttOffset) then
        Result := GetMinBitsCountIByArr([Trunc(FMatrix[0][2]), Trunc(FMatrix[1][2])], True);
    ttRotation:
      Result := GetMinBitsCountFByArr([FMatrix[1][0], FMatrix[0][1]]);
    ttScaling:
      if HasTransform(ttOffset) or HasTransform(ttRotation) or HasTransform(ttScaling) then
        Result := GetMinBitsCountFByArr([FMatrix[0][0], FMatrix[1][1]]);
  end;
end;

function TswfTransformation.HasTransform(
  const ATransformType: TswfTransformType): Boolean;
begin
  case ATransformType of
    ttOffset:
      Result := (FMatrix[0][2] <> 0.0) or (FMatrix[1][2] <> 0.0);
    ttRotation:
      Result := (FMatrix[1][0] <> 0.0) or (FMatrix[0][1] <> 0.0);
    ttScaling:
      Result := (FMatrix[0][0] <> 1.0) or (FMatrix[1][1] <> 1.0);
  else
    Result := False;
  end;
end;

procedure TswfTransformation.Multiply(const AMat: PswfMatrix);
var
  vTmp: TswfMatrix;
begin
  vTmp[0][0] := FMatrix[0][0] * AMat^[0][0] + FMatrix[0][1] * AMat^[1][0] + FMatrix[0][2] * AMat^[2][0];
  vTmp[0][1] := FMatrix[0][0] * AMat^[0][1] + FMatrix[0][1] * AMat^[1][1] + FMatrix[0][2] * AMat^[2][1];
  vTmp[0][2] := FMatrix[0][0] * AMat^[0][2] + FMatrix[0][1] * AMat^[1][2] + FMatrix[0][2] * AMat^[2][2];
  vTmp[1][0] := FMatrix[1][0] * AMat^[0][0] + FMatrix[1][1] * AMat^[1][0] + FMatrix[1][2] * AMat^[2][0];
  vTmp[1][1] := FMatrix[1][0] * AMat^[0][1] + FMatrix[1][1] * AMat^[1][1] + FMatrix[1][2] * AMat^[2][1];
  vTmp[1][2] := FMatrix[1][0] * AMat^[0][2] + FMatrix[1][1] * AMat^[1][2] + FMatrix[1][2] * AMat^[2][2];
  vTmp[2][0] := FMatrix[2][0] * AMat^[0][0] + FMatrix[2][1] * AMat^[1][0] + FMatrix[2][2] * AMat^[2][0];
  vTmp[2][1] := FMatrix[2][0] * AMat^[0][1] + FMatrix[2][1] * AMat^[1][1] + FMatrix[2][2] * AMat^[2][1];
  vTmp[2][2] := FMatrix[2][0] * AMat^[0][2] + FMatrix[2][1] * AMat^[1][2] + FMatrix[2][2] * AMat^[2][2];
  FMatrix := vTmp;
end;

procedure TswfTransformation.SetAngle(const AAngle: Single);
var
  vTmp: TswfMatrix;
  S, C: Extended;
begin
  FillChar(vTmp, SizeOf(TswfMatrix), 0);
  SinCos(Radian(AAngle), S, C);
  vTmp[0][0] := C;
  vTmp[0][1] := -S;
  vTmp[1][0] := S;
  vTmp[1][1] := C;
  vTmp[2][2] := 1;
  Multiply(@vTmp);
end;

procedure TswfTransformation.SetScale(const AX, AY: Single);
var
  vTmp: TswfMatrix;
begin
  FillChar(vTmp, SizeOf(TswfMatrix), 0);
  vTmp[0][0] := AX;
  vTmp[1][1] := AY;
  Multiply(@vTmp);
end;

procedure TswfTransformation.Translate(ACADtoSWF: TsgCADtoSWF);
var
  vBitsCnt: Integer;
  vFlag: Boolean;
begin
  inherited Translate(ACADtoSWF);
  ACADtoSWF.AlignToByte;
  // Scaling
  vFlag := HasTransform(ttScaling);
  ACADtoSWF.PutBits(Ord(vFlag), 1);
  if vFlag then
  begin
    vBitsCnt := GetMinBitsCount(ttScaling);
    ACADtoSWF.PutBits(vBitsCnt, 5);
    ACADtoSWF.PutFixedBits(FMatrix[0][0], vBitsCnt, 16);
    ACADtoSWF.PutFixedBits(FMatrix[1][1], vBitsCnt, 16);
  end;
  // Rotation
  vFlag := HasTransform(ttRotation);
  ACADtoSWF.PutBits(Ord(vFlag), 1);
  if vFlag then
  begin
    vBitsCnt := GetMinBitsCount(ttRotation);
    ACADtoSWF.PutBits(vBitsCnt, 5);
    ACADtoSWF.PutFixedBits(FMatrix[1][0], vBitsCnt, 16);
    ACADtoSWF.PutFixedBits(FMatrix[0][1], vBitsCnt, 16);
  end;
  // Offset
  ACADtoSWF.PutBits(Ord(HasTransform(ttOffset)), 1);
  vBitsCnt := GetMinBitsCount(ttOffset);
  ACADtoSWF.PutBits(vBitsCnt, 5);
  ACADtoSWF.PutBits(Trunc(FMatrix[0][2]), vBitsCnt);
  ACADtoSWF.PutBits(Trunc(FMatrix[1][2]), vBitsCnt);

  ACADtoSWF.AlignToByte;
end;

{ TswfDoAction }
constructor TswfDoAction.Create;
begin
  FContainer := TswfContainer.Create;
end;

destructor TswfDoAction.Destroy;
begin
  FContainer.Free;
  inherited Destroy;
end;

function TswfDoAction.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
begin
  Result := Container.GetActionsSize(ACADtoSWF);
  Inc(Result);
end;

procedure TswfDoAction.Translate(ACADtoSWF: TsgCADtoSWF);
var
  I: Integer;
begin
  ACADtoSWF.DefineType(cnstSWFDoAction, GetSize(ACADtoSWF));
  for I := 0 to Container.ActionsCount - 1 do
    Container.Actions[I].Translate(ACADtoSWF);
  ACADtoSWF.PutWord(0, 1);
end;

{ TswfClipEvent }

constructor TswfClipEvent.Create;
begin
  FContainer := TswfContainer.Create;
end;

destructor TswfClipEvent.Destroy;
begin
  FContainer.Free;
  inherited Destroy;
end;

function TswfClipEvent.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
begin
  Result := 4 + 2 + 2*Ord(ACADtoSWF.Version > 5);
  Inc(Result, Ord((FEvent and cnstEventKeyPress) <> 0));
  Inc(Result, Container.GetActionsSize(ACADtoSWF));
  Inc(Result);
end;

procedure TswfClipEvent.Translate(ACADtoSWF: TsgCADtoSWF);
var
  I, vEventSize: Integer;
begin
  if ACADtoSWF.Version > 5 then
    vEventSize := 4
  else
    vEventSize := 2;
  ACADtoSWF.PutWord(FEvent, vEventSize);
  ACADtoSWF.PutWord(1 + Container.GetActionsSize(ACADtoSWF) + Ord((FEvent and cnstEventKeyPress) <> 0), 4);
  for I := 0 to Container.ActionsCount - 1 do
    Container.Actions[I].Translate(ACADtoSWF);
  ACADtoSWF.PutWord(0, 1);
end;

{ TswfPlaceObject2 }

constructor TswfPlaceObject2.Create;
begin
  FClipEvents := TList.Create;
  FRatio := -1.0;
  FFlag := 2;// new = 2 (modify = 1, replace = 3)
end;

destructor TswfPlaceObject2.Destroy;
begin
  if FTransformation <> nil then
    FTransformation.Free;
  inherited Destroy;
end;

function TswfPlaceObject2.GetClipEvent(const AIndex: Integer): TswfClipEvent;
begin
  if AIndex < FClipEvents.Count then
    Result := FClipEvents[AIndex]
  else
    Result := nil;
end;

function TswfPlaceObject2.GetClipEventsCount: Integer;
begin
  Result := FClipEvents.Count;
end;

function TswfPlaceObject2.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
var
  I, vEventSize: Integer;
begin
  Result := 3;
  Inc(Result, 2*Ord((FFlag=2) or (FFlag=3)));
  if HasTransformation then
    Inc(Result, FTransformation.GetSize(ACADtoSWF));
  // Color Transform here
  if HasRatio then
    Inc(Result, 2);
  // Clipping Depth here
  if HasName then
    Inc(Result, Length(FName));
  if HasClipEvents then
  begin
    if ACADtoSWF.Version > 5 then
      vEventSize := 4
    else
      vEventSize := 2;
    Inc(Result, 2 + vEventSize);
    for I := 0 to ClipEventsCount - 1 do
      Inc(Result, ClipEvents[I].GetSize(ACADtoSWF));
    Inc(Result, vEventSize);
  end;
end;

function TswfPlaceObject2.HasClipEvents: Boolean;
begin
  Result := (FClipEvents.Count > 0);
end;

function TswfPlaceObject2.HasRatio: Boolean;
begin
  Result := (FRatio <> 0);
end;

function TswfPlaceObject2.HasTransformation: Boolean;
begin
  Result := (FTransformation <> nil);
end;

function TswfPlaceObject2.HasName: Boolean;
begin
  Result := (FName <> '');
end;

procedure TswfPlaceObject2.Translate(ACADtoSWF: TsgCADtoSWF);
var
  I, vEventMask, vEventSize: Integer;
begin
  inherited Translate(ACADtoSWF);
  ACADtoSWF.DefineType(cnstSWFRemoveObject2, GetSize(ACADtoSWF));
  ACADtoSWF.PutBits(Ord(HasClipEvents), 1);
  ACADtoSWF.PutBits(0, 1);// without Clipping Depth
  ACADtoSWF.PutBits(Ord(HasName), 1);
  ACADtoSWF.PutBits(Ord(HasRatio), 1);
  ACADtoSWF.PutBits(0, 1);// without Color Transform
  ACADtoSWF.PutBits(Ord(HasTransformation), 1);
  ACADtoSWF.PutBits(FFlag, 2);
  ACADtoSWF.PutWord(FLayer, 2);
  if (FFlag=2) or (FFlag=3) then
    ACADtoSWF.PutWord(FOwnerID, 2);
  if HasTransformation then
    FTransformation.Translate(ACADtoSWF);
  // Color Transform here
  if HasRatio then
    ACADtoSWF.PutWord(Trunc(FRatio* 65535.0), 2);
  if HasName then
  begin
    ACADtoSWF.PutBytes(PChar(FName), Length(FName));
    ACADtoSWF.PutWord(0, 1);
  end;
  // Clipping Depth here
  if HasClipEvents then
  begin
    if ACADtoSWF.Version > 5 then
      vEventSize := 4
    else
      vEventSize := 2;
    vEventMask := 0;
    ACADtoSWF.PutWord(0, 2);
    for I := 0 to ClipEventsCount - 1 do
      vEventMask := vEventMask or ClipEvents[I].Event;
    ACADtoSWF.PutWord(vEventMask, vEventSize);
    for I := 0 to ClipEventsCount - 1 do
      ClipEvents[I].Translate(ACADtoSWF);
    ACADtoSWF.PutWord(0, vEventSize);
  end;
end;

{ TswfPushRecord }

function TswfPushRecord.GetValue: Integer;
begin
  REsult := 0;
end;

function TswfPushRecord.GetValueType: TswfPushTypes;
begin
  Result := psUndefined;
end;

{ TswfString }

constructor TswfString.Create(const AValue: string);
begin
  FValue := AValue;
end;

function TswfString.GetValue: string;
begin
  Result := FValue;
end;

function TswfString.GetValueType: TswfPushTypes;
begin
  Result := psString;
end;

{ TswfProperty }

constructor TswfProperty.Create(const AValue: Integer);
begin
  FValue := AValue;
end;

function TswfProperty.GetValue: Integer;
begin
  Result := FValue;
end;

function TswfProperty.GetValueType: TswfPushTypes;
begin
  Result := psProperty;
end;

{ TswfBoolean }

constructor TswfBoolean.Create(const AValue: Boolean);
begin
  FValue := AValue;
end;

function TswfBoolean.GetValue: Boolean;
begin
  Result := FValue;
end;

function TswfBoolean.GetValueType: TswfPushTypes;
begin
  Result := psBoolean;
end;

{ TswfDouble }

constructor TswfDouble.Create(const AValue: Double);
begin
  FValue := AValue;
end;

function TswfDouble.GetValue: Double;
begin
  Result := FValue;
end;

function TswfDouble.GetValueType: TswfPushTypes;
begin
  Result := psDouble;
end;

{ TswfInteger }

constructor TswfInteger.Create(const AValue: Integer);
begin
  FValue := AValue;
end;

function TswfInteger.GetValue: Integer;
begin
  Result := FValue;
end;

function TswfInteger.GetValueType: TswfPushTypes;
begin
  Result := psInteger;
end;

{ TswfPush }

procedure TswfPush.Add(ARecord: TswfPushRecord);
begin
  FStack.Add(ARecord);
end;

procedure TswfPush.Add(const AString: string);
var
  vRecord: TswfString;
begin
  vRecord := TswfString.Create(AString);
  Add(vRecord);
end;

procedure TswfPush.Add(const ABoolean: Boolean);
var
  vRecord: TswfBoolean;
begin
  vRecord := TswfBoolean.Create(ABoolean);
  Add(vRecord);
end;

procedure TswfPush.Add(const ADouble: Double);
var
  vRecord: TswfDouble;
begin
  vRecord := TswfDouble.Create(ADouble);
  Add(vRecord);
end;

procedure TswfPush.Add(const AInteger: Integer);
var
  vRecord: TswfInteger;
begin
  vRecord := TswfInteger.Create(AInteger);
  Add(vRecord);
end;

constructor TswfPush.Create;
begin
  inherited Create(aPush);
  FStack := TswfStackList.Create;
end;

destructor TswfPush.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

function TswfPush.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
var
  I: Integer;
begin
  Result := inherited GetSize(ACADtoSWF);
  for I := 0 to FStack.Count - 1 do
  begin
    Inc(Result);
    case TswfPushRecord(FStack[I]).ValueType of
      psString:     Inc(Result, Length(TswfString(FStack[I]).GetValue));
      //psNull, psUndefined: // Empty (only type code above)
      psBoolean:    Inc(Result);
      psDouble:     Inc(Result, 8);
      psInteger:    Inc(Result, 4);
      psProperty:          Inc(Result, 4);
      psRegisterNumber:    Inc(Result);
      psPoolIndexLess256:  Inc(Result);
      psPoolIndexMoore256: Inc(Result, 2);
    end;
  end;
end;

procedure TswfPush.Translate(ACADtoSWF: TsgCADtoSWF);
var
  I: Integer;
  vStrValue: string;
begin
  inherited Translate(ACADtoSWF);
  for I := 0 to FStack.Count - 1 do
  begin
    ACADtoSWF.PutWord(Ord(TswfPushRecord(FStack[I]).ValueType), 1);
    case TswfPushRecord(FStack[I]).ValueType of
      psString:
        begin
          vStrValue := TswfString(FStack[I]).GetValue;
          ACADtoSWF.PutBytes(PChar(vStrValue), Length(vStrValue));
          ACADtoSWF.PutWord(0, 1);
        end;
      //psNull, psUndefined: // Empty (only type code above)
      psBoolean:
        ACADtoSWF.PutWord(Ord(TswfBoolean(FStack[I]).GetValue), 1);
      psDouble:
        ACADtoSWF.PutDouble(TswfDouble(FStack[I]).GetValue);
      psInteger:
        ACADtoSWF.PutWord(TswfInteger(FStack[I]).GetValue, 4);
      // for future versions
      psProperty:
        ACADtoSWF.PutWord(TswfPushRecord(FStack[I]).GetValue, 4);
      psRegisterNumber:
        ACADtoSWF.PutWord(TswfPushRecord(FStack[I]).GetValue, 1);
      psPoolIndexLess256:
        ACADtoSWF.PutWord(TswfPushRecord(FStack[I]).GetValue, 1);
      psPoolIndexMoore256:
        ACADtoSWF.PutWord(TswfPushRecord(FStack[I]).GetValue, 2);
    end;
  end;
end;

{ TswfRegisterVariable }

function TswfRegisterVariable.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
begin
  Result := 1 + Length(FName);
end;

procedure TswfRegisterVariable.Translate(ACADtoSWF: TsgCADtoSWF);
begin
  ACADtoSWF.PutWord(FNumber, 1);
  ACADtoSWF.PutBytes(PChar(FName), Length(FName));
  ACADtoSWF.PutWord(0, 1);
end;

{ TswfDefineFunction2 }

constructor TswfDefineFunction2.Create;
begin
  inherited Create(aDefineFunction2);
  FArguments := TList{$IFDEF SG_FIREMONKEY}<TswfRegisterVariable>{$ENDIF}.Create;
  FContainer := TswfContainer.Create;
end;

destructor TswfDefineFunction2.Destroy;
{$IFDEF SG_FIREMONKEY}
var
  I: Integer;
  vItem: TswfRegisterVariable;
{$ENDIF}
begin
  {$IFDEF SG_FIREMONKEY}
  for I := FArguments.Count - 1 downto 0 do
  begin
    vItem := FArguments[I];
    FArguments[I] := nil;
    FArguments.Delete(I);
    FreeAndNil(vItem);
  end;
  FreeAndNil(FArguments);
  {$ELSE}
  FreeList(FArguments);
  {$ENDIF}
  FContainer.Free;
  inherited Destroy;
end;

function TswfDefineFunction2.GetArgument(
  const AIndex: Integer): TswfRegisterVariable;
begin
  if AIndex < FArguments.Count then
    Result := FArguments[AIndex]
  else
    Result := nil;
end;

function TswfDefineFunction2.GetArgumentsCount: Integer;
begin
  Result := FArguments.Count;
end;

function TswfDefineFunction2.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
var
  I: Integer;
begin
  Result := inherited GetSize(ACADtoSWF);
  if FName <> '' then
    Inc(Result, Length(FName))
  else
    Inc(Result);
  Inc(Result, 5);
  for I := 0 to ArgumentsCount - 1 do
    Inc(Result, Arguments[I].GetSize(ACADtoSWF));
  Inc(Result, 2 + Container.GetActionsSize(ACADtoSWF));
end;

procedure TswfDefineFunction2.Translate(ACADtoSWF: TsgCADtoSWF);
var
  I, vActionsSize: Integer;
begin
  vActionsSize := Container.GetActionsSize(ACADtoSWF);
  ACADtoSWF.PutWord(Ord(ActionType), 1);
  ACADtoSWF.PutWord(GetSize(ACADtoSWF) - vActionsSize, 2);
  if FName <> '' then
    ACADtoSWF.PutBytes(PChar(FName), Length(FName));
  ACADtoSWF.PutWord(0, 1);
  ACADtoSWF.PutWord(ArgumentsCount, 2);
  ACADtoSWF.PutWord(FRegisterCount, 1);
  ACADtoSWF.PutWord(FFlags, 16);
  for I := 0 to ArgumentsCount - 1 do
    Arguments[I].Translate(ACADtoSWF);
  ACADtoSWF.PutWord(vActionsSize, 2);
  for I := 0 to Container.ActionsCount - 1 do
    Container.Actions[I].Translate(ACADtoSWF);
end;

{ TswfDefineSprite }

procedure TswfDefineSprite.AddObject(AObject: TswfObject);
begin
  FObjects.Add(AObject);
end;

constructor TswfDefineSprite.Create;
begin
  FObjects := TswfObjectsList.Create;
end;

destructor TswfDefineSprite.Destroy;
begin
  FObjects.Clear;//no free items
  FObjects.Free;
  inherited Destroy;
end;

function TswfDefineSprite.GetObject(const AIndex: Integer): TswfObject;
begin
  if AIndex < FObjects.Count then
    Result := TswfObject(FObjects[AIndex])
  else
    Result := nil;
end;

function TswfDefineSprite.GetObjectsCount: Integer;
begin
  Result := FObjects.Count;
end;

function TswfDefineSprite.GetSize(ACADtoSWF: TsgCADtoSWF): Integer;
var
  I, vSize: Integer;
begin
  Result := inherited GetSize(ACADtoSWF);
  Inc(Result, 4);
  for I := 0 to ObjectsCount - 1 do
  begin
    vSize := Objects[I].GetSize(ACADtoSWF);
    if vSize >= 63 then
      Inc(Result, vSize+6)
    else
      Inc(Result, vSize+2);
  end;
end;

function TswfDefineSprite.GetFramesCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ObjectsCount - 1 do
    if Objects[I] is TswfShowFrame then
      Inc(Result);
end;

function TswfDefineSprite.GetType: Integer;
begin
  Result := cnstSWFDefineSprite;
end;

procedure TswfDefineSprite.Translate(ACADtoSWF: TsgCADtoSWF);
var
  I: Integer;
begin
  inherited Translate(ACADtoSWF);
  ACADtoSWF.PutWord(GetFramesCount, 2);
  for I := 0 to ObjectsCount - 1 do
    Objects[I].Translate(ACADtoSWF);
  ACADtoSWF.PutWord(0, 2);
end;

{ TswfShowFrame }

function TswfShowFrame.GetType: Integer;
begin
  Result := cnstSWFShowFrame;
end;

{ TsgBytesList }

procedure TsgBytesList.Add(const Value: TsgByte);
begin
  if FSize >= FCapacity then
    Grow;
  Items[FSize] := Value;
end;

procedure TsgBytesList.Assign(ASource: TsgBytesList);
begin
  SetCapacity(ASource.FCapacity);
  FSize := ASource.FSize;
  CopyMemory(FData, ASource.FData, ASource.FCapacity*SizeOf(TsgByte));
end;

procedure TsgBytesList.Clear;
begin
  FillChar(FData^, FCapacity*SizeOf(TsgByte), 0);
end;

procedure TsgBytesList.CompessFrom(const AIndex: TsgInteger);
var
  vStream: TStream;
  vLen: TsgInteger;
  P: PsgByte;
begin
  P := FData;
  Inc(P, AIndex);
  vLen := FSize - AIndex;
  vStream := TMemoryStream.Create;
  vStream.Size := vLen;
  try
    // Copying data to stream for ZIPping
    vStream.Write(P^, vLen);
    Compress(vStream);
    // Back copying
    vLen := vStream.Size;
    vStream.Position := 0;
    vStream.Read(P^, vLen);
    FSize := vLen + AIndex;
  finally
    vStream.Free;
  end;
end;

procedure TsgBytesList.CopyToStream(AStream: TStream);
begin
{$IFDEF SG_FIREMONKEY}
  AStream.Position := 0;
{$ELSE}
  AStream.Seek(0, soFromBeginning);
{$ENDIF}
  AStream.Write(FData^, FSize);
  AStream.Size := FSize;
end;

constructor TsgBytesList.Create(const ASize: TsgInteger);
begin
  FCapacity := ASize;
  FSize := FCapacity;
  FData := AllocMem(FCapacity*SizeOf(TsgByte));
  Clear;
end;

constructor TsgBytesList.CreateEx(const ASize, ACapacity: TsgInteger);
begin
  SetCapacity(ACapacity);
  if ASize <= ACapacity then
    FSize := ASize
  else
    FSize := ACapacity;
end;

procedure TsgBytesList.Cut(const AIndex, ACount: TsgInteger);
var
  P, PSource: PsgByte;
begin
  if AIndex < FSize then
  begin
    if AIndex + ACount < FSize{FCapacity} then
    begin
      P := FData;
      PSource := FData;
      Inc(P, AIndex);
      Inc(PSource, AIndex + ACount);
      Move(PSource^, P^, FSize{FCapacity} - AIndex - ACount);
      Dec(FSize, ACount);
    end;
  end;
end;

destructor TsgBytesList.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

function TsgBytesList.GetItem(Index: TsgInteger): TsgByte;
var
  P: PsgByte;
begin
  if (Index >= FSize) then
    Result := 0
  else
  begin
    P := FData;
    Inc(P, Index);
    Result := P^;
  end;
end;

procedure TsgBytesList.Grow;
begin
  SetCapacity(FCapacity + cnstMinCapacity);
end;

procedure TsgBytesList.SetCapacity(const Value: TsgInteger);
begin
  //if Value < FSize then
  //  raise Exception.Create('TsgBytesList: Value < FSize');
  if Value <> FCapacity then
  begin
    ReallocMem(FData, Value * SizeOf(TsgByte));
    FCapacity := Value;
  end;
end;

procedure TsgBytesList.SetItem(Index: TsgInteger; const Value: TsgByte);
var
  P: PsgByte;
begin
  if Index >= FCapacity then
    Grow;
  if (Index >= FSize) and (Index < FCapacity) then
    FSize := Index+1;
  if (Index >= FCapacity) then
    raise Exception.Create('TsgBytesList: (Index > FCapacity)');
  P := FData;
  Inc(P, Index);
  P^ := Value;
end;

procedure TsgBytesList.SetSize(const Value: TsgInteger);
var
  P: PsgByte;
begin
  if Value > FCapacity then
    SetCapacity(Value);
  if Value > FSize then
  begin
    P := FData;
    Inc(P, FSize);
    FillChar(P, (Value - FSize) * SizeOf(TsgByte), 0)
  end
  else
    SetCapacity(Value);
  FSize := Value;
end;

procedure TsgBytesList.Write(const AIndex: TsgInteger; const Arr: array of TsgByte);
var
  P: PsgByte;
  Cnt: Integer;
begin
  Cnt := Length(Arr);
  if AIndex + Cnt >= FCapacity then
    Grow;
  P := FData;
  Inc(P, AIndex);
  CopyMemory(P, @Arr[Low(Arr)], Cnt);
  FSize := AIndex + Cnt;
end;

procedure TsgBytesList.WriteMemory(const Ptr: Pointer; const AIndex,
  ACount: Integer);
var
  P: PsgByte;
begin
  if AIndex + ACount >= FCapacity then
    Grow;
  P := FData;
  Inc(P, AIndex);
  CopyMemory(P, Ptr, ACount);
  FSize := AIndex + ACount;
end;

{ TswfCustomList }

destructor TswfCustomList.Destroy;
{$IFDEF SG_FIREMONKEY}
var
  I: Integer;
  vItem: TObject;
{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  for I := Count - 1 downto 0 do
  begin
    vItem := Items[I];
    Items[I] := nil;
    Delete(I);
    FreeAndNil(vItem);
  end;
  Clear;
{$ELSE}
  ClearList(Self);
{$ENDIF}
  inherited Destroy;
end;

end.
