{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                     SHX fonts class                        }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit SHX;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, {$IFDEF SG_NON_WIN_PLATFORM}cwstring,{$ENDIF}
{$ENDIF}
  Classes, SysUtils, sgConsts, IniFiles, sgFunction, sgLists, Types
{$IFDEF SG_FIREMONKEY}
  ,sgFMXTypes, System.IOUtils
{$ENDIF}
  ;

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

{$IFDEF SG_FM_MOBILE}
  {$DEFINE SG_CASESENSITIVE_PLATFORM}
{$ENDIF}

{$IFDEF SG_LINUX}
  {$DEFINE SG_CASESENSITIVE_PLATFORM}
{$ENDIF}

const
  cnstUndefinedSymbol = '?';
  cnstSHXFontExt = '.SHX';

type
  TsgSHXVertexList = class;
  TsgSHXPoint = TPointF;

  PsgSHXPolyData = ^TsgSHXPolyData;
  TsgSHXPolyData = record
    Poly: TsgSHXVertexList;
    Xoff, Yoff: Double;
    Scaleoff: TsgSHXPoint;
    FirstDown, LastDown:  Double;
    Top, Bottom: Double;
  end;

  PsgSHXRecord = ^TsgSHXRecord;
  TsgSHXRecord = record
    ShapeNumber: Word;   // shape number as widechar
    DefBytes: Word;     // number of data bytes in shape, 2000 max
    BytesPos: Cardinal;  // position of SapeBytes in the file
    ShapePoly:  TsgSHXPolyData;
    ShapePolyVert: TsgSHXPolyData;   // for Vertical text
    ShapeNameLen: Word; // Length of shapename start from ShapeBytes
    ShapeBytesCount: Word; // ShapeBytes count (in case it was read)
    ShapeBytes: array[0 .. 0] of Byte;
  end;

  TsgTextGeneration = (ttNormal, ttBackward, ttUpSideDown);

  TsgSymbolType = (stUndefined, stNormal, stBigFont);
  TsgSymbolsMap = AnsiString;

  PsgSHXVertex = ^TsgSHXVertex;
  TsgSHXVertex = record
    X, Y: Double;
    Bulge: Double;
    PenDown: Boolean;
  end;

//  TsgTextStyle = record
  TsgSHXTextStyle = class
  public
    FontPath: string;
    BigFontPath: string;
    Name: string;
    TextGeneration: TsgTextGeneration;
    ObliqueAngle: Double;
    Width: Double;
    Vertical: Boolean;
    UpSideDown: Boolean;
    Backwards:  Boolean;
    FontIndex: Integer;
    BigFontIndex: Integer;
    FontExists: Boolean;
  end;

  PsgFontProcParams = ^TsgFontProcParams;
  TsgFontProcParams = record
    X, Y: Double;
    Scale: TsgSHXPoint;
    Vertical, Backward, Underscore, Overscore, HasBigSymbol: Boolean;
    Poly: TsgSHXVertexList;
  end;

  PsgSHXStack = ^TsgSHXStack;
  TsgSHXStack = record
    Index: Integer;
    Value: array[0..4, 0..2] of Double
  end;

  TsgSHXVertexArray =  array[0..MaxInt div SizeOf(TsgSHXVertex) - 1] of TsgSHXVertex;
  PsgSHXVertexArray = ^TsgSHXVertexArray;

  TsgSHXVertexList = class(TsgCustomTypedList)
  private
    function GetItems(const Index: Integer): TsgSHXVertex;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetList: PsgSHXVertexArray;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetItems(const Index: Integer; const Value: TsgSHXVertex);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function CompareSHXVertex(const A, B: Pointer): Integer;
  protected
    class function GetElementTypeInfo: Pointer; override;
    procedure SetDefaultCompareProc(var AProc: TsgObjProcCompare); override;
  public
    function Add(const AItem: TsgSHXVertex): Integer; overload;
    function Add(const X, Y: Double; ABulge: Double; APenDown: Boolean): Integer; overload;
    function IndexOf(const AItem: TsgSHXVertex): Integer;
    function Remove(const AItem: TsgSHXVertex): Integer;
    property Items[const Index: Integer]: TsgSHXVertex read GetItems write SetItems; default;
    property List: PsgSHXVertexArray read GetList;
  end;

  TsgSHXKinds = (tkNormSHX, tkBigSHX, tkUnicodeSHX, tkUndefined);

  TsgSHXFont = class
  private
    FCodePage: Integer;
    FIsShape: Boolean;
    FLineFeed: Double;
    FAbove: Integer;
    FBelow: Integer;
    FName: string;
    FNumChars: Word;
    FKind: TsgSHXKinds;
    FModes: Byte;
    FEmbtype: Byte;
    FEncoding: Byte;
    FShapes: TsgList;
    FNBigFontRanges: word;
    FExtndBig : boolean;
    FCharHeight: byte;
    FCharWidth: byte;
    FBigFontRanges: array [1..64,1..2] of Word;
    FixedTable: array [1..255] of Byte;
    FUnits: Integer;
    procedure ClearFixedTable;
    procedure CleareShapes;
    function ConvertPolyToShape(var ShapeBytes: array of Byte; AShapePoly: TsgList): Word;
    function FindShape(var N: Word; var Name: AnsiString): Boolean;
    function GetName: string;
    function GetShapePolyOfSymbol(const AIndex: Integer;
      Vert, Backward: Boolean; var APenDown: Integer;
      var ARez: Boolean; const AStack: PsgSHXStack): PsgSHXPolyData;
    function UpdateShapeName(const ABytes: array of Byte): Integer;
    procedure ParseBigSHX(AStream: TStream);
    procedure ParseNormSHX(AStream: TStream);
    procedure ParseUnicodeSHX(AStream: TStream);
    procedure ReadUnicodeSHX(N: Word);
    procedure ReadNormSHX(N: Word);
    procedure ReadBigSHX(N: Word);
    procedure ReadSHX(N: Word);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PrepareShapes(AList: TsgList; FixedSize:Boolean);
    function ProcessSymb(N: Word; var InsX, InsY: Double; var Scale: TsgSHXPoint;
      var APenDown: Integer; Vert, Backward: Boolean; APoly: TsgSHXVertexList;
      const AStackGlobal: PsgSHXStack): Boolean;
    function ProcessSymbI(Index: Integer; var InsX, InsY: Double; var Scale: TsgSHXPoint;
      var APenDown: Integer; Vert, Backward: Boolean; APoly: TsgSHXVertexList;
      const AStackGlobal: PsgSHXStack): Boolean;
    function SubSymb(N: Word; var InsX, InsY: Double; var Scale: TsgSHXPoint;
      var APenDown: Integer; Vert, Backward: Boolean;
      APoly: TsgSHXVertexList; const AStackGlobal: PsgSHXStack): Boolean;
    function ExtSubSymb(N: Word; var InsX, InsY: Double; var Scale: TsgSHXPoint;
      var APenDown: Integer; const CharWidth, CharHeight: Byte; Vert, Backward: Boolean; APoly: TsgSHXVertexList;
      const AStackGlobal: PsgSHXStack): Boolean;
    function GetAbove: Integer;
    function GetLineFeed: Double;
    function GetBelow: Integer;
    function GetNumChars: Word;
    function GetLoaded: Boolean;
    function GetShapes: TsgList;
    function FindSymbol(N: word): integer;
    procedure MoveCurpos(APoly: TsgSHXVertexList;
      const XOff, YOff: Double; const Scale: TsgSHXPoint;
      const PenDown: Boolean; var CpX, Cpy: Double);
    procedure SetBulge(APoly: TsgSHXVertexList; const X, Y, B: Double;
      const PenDown: Boolean);
    procedure SetCurpos(APoly: TsgSHXVertexList; var CpX, CpY: Double);
    procedure AddSHXVertex(APoly: TsgSHXVertexList; const X, Y, B: Double;
      const D: Boolean);
    function GetSymbolI(Index: Integer; InX, InY: Double;
      const AFontParams: PsgFontProcParams; APoly: TsgSHXVertexList; var OutX,
      OutY: Double): Boolean;
    procedure UpdateScript(InX, InY: Double; const Scale: TsgSHXPoint;
      const AFontParams: PsgFontProcParams; APoly: TsgSHXVertexList; OutX, OutY: Double);
    function GetScaledSymbolI(Index: Integer; InX, InY: Double;
      const Scale: TsgSHXPoint;
      const AFontParams: PsgFontProcParams; APoly: TsgSHXVertexList; var OutX,
      OutY: Double; AAdvance: Double): Boolean;
    function SymbolCompare(const A, B: Pointer): Integer;
  protected
    function ExtractSubSymb(const N: Word; var InsX, InsY: Double; var Scale: TsgSHXPoint;
      var APenDown: Integer;
      const CharWidth, CharHeight: Byte; const Vert, Backward: Boolean;
      APoly: TsgSHXVertexList; const AStackGlobal: PsgSHXStack): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function BigFontInRange( c : byte): boolean;
    function BigFontInRangeWord(ASymbol: Word): Boolean;
    function HasPairSymbol(ASym1, ASym2: Byte): Boolean;
    function HasPairSymbolWord(ASymbol: Word): Boolean;
  public
    constructor Create(const AFontPath: string);
    destructor Destroy; override;
    procedure AddNewShape(ShapeNumber: Word; const ShapeName: AnsiString; AShapePoly: TsgList);
    procedure SaveAs(const FName: string);
    function Find(N: Word; var Index: Integer): Boolean;
    function HasSymbol(const ASymbol: WideChar): Boolean; overload;
    function HasSymbol(const ASymbol: Word): Boolean; overload;
    function GetSymbol(Symb: Word; InX, InY: Double;
      const AFontParams: PsgFontProcParams;
      APoly: TsgSHXVertexList; var OutX, OutY: Double): Boolean;
    function GetScaledSymbol(Symb: Word; InX, InY: Double; const Scale: TsgSHXPoint;
      const AFontParams: PsgFontProcParams;
      APoly: TsgSHXVertexList; var OutX, OutY: Double): Boolean;
    function Load: Boolean;
    function GetCodePage: Integer;
    property LineFeed: Double read GetLineFeed;
    property Above: Integer read GetAbove;
    property Below: Integer read GetBelow;
    property CodePage: Integer read FCodePage;
    property Loaded: Boolean read GetLoaded;
    property Name: string read GetName;
    property NumChars: Word read GetNumChars;
    property Shapes: TsgList read GetShapes;
    property Kind: TsgSHXKinds read FKind;
  end;

{$IFNDEF SGDEL_6}
  THashedStringList = class(TsgStringList);
{$ENDIF}

  TsgSHXFontList = class
  private
    FDefaultFontName: string;
    FDefaultFont:  TsgSHXFont;
    FDefaultFontPath: string;
    FDefaultBigFontPath: string;
    FDefaultBigFontName :string;
    FFonts: THashedStringList;
    FShapeFonts: TStringList;
    FStyles: TStringList;
    FSearchPath: TStringList;
    FMissedFonts: Tstringlist;
    FSubstFonts:Tstringlist;
    function GetDefaultFont: TsgSHXFont;
    function GetFonts: TStringList;
    function GetSearchPath: TStringList;
    function GetMissedFonts: TStringList;
    function GetStyles: TStringList;
    function GetDefaultFontPath: string;
    function GetDefaultBigFontPath: string;
    procedure SetDefaultFontPath(const Value: string);
  protected
    class function GetFontName(const AName: string): string;
    property DefaultFontName: string read FDefaultFontName;
  public
    constructor Create(const ASearchPath, ADefault: string;
      const ADefaultBigFont: string = '');
    destructor  Destroy; override;
    function AddStyle(const AName, AFontName, ABigFontName: string; AVertical, AUpSideDown,
      ABackwards: Boolean; AObliqueAngle, AWidth: Double): TsgSHXTextStyle;
    function IndexFontOfName(const AName: string): Integer;
    procedure SetSubstFonts(ASubstFonts: TStrings);
    property DefaultFont: TsgSHXFont read GetDefaultFont;
    property DefaultFontPath: string read GetDefaultFontPath write SetDefaultFontPath;
    property DefaultBigFontPath: string read GetDefaultBigFontPath;
    property Fonts: TStringList read GetFonts;
    property Styles: TStringList read GetStyles;
    property SearchPath: TStringList read GetSearchPath;
    property MissedFonts: TStringList read GetMissedFonts;
  end;

  TsgSHXBytes = class
  protected
    Data: TBytes;
    function Get(var Index: Integer): Byte;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

procedure GetSHXText(ASHXStyles: TsgSHXFontlist; const Style: string;
  Text: sgRawByteString; const WideText: WideString;
  CodePage: Integer; APoly: TsgSHXVertexList;  Fstyle: TmvFontStyles;
  var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;

procedure GetSHXText(ASHXStyles: TsgSHXFontlist; const Style: string;
  const AIndices: array of Word; APoly: TsgSHXVertexList; Fstyle: TmvFontStyles;
  var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;


procedure DrawSHXText(ASHXStyles: TsgSHXFontlist; const Font: string;
  Text: sgRawByteString; const WideText: WideString; CodePage: Integer;
  Vertical, Backward: Boolean; APoly: TsgSHXVertexList;
  Fstyle: TmvFontStyles; var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;
procedure DrawSHXText(ASHXStyles: TsgSHXFontlist; const Font: string;
  const AIndices: array of Word; Vertical, Backward: Boolean; APoly: TsgSHXVertexList;
  Fstyle: TmvFontStyles; var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;

procedure GetSHXTextFromStyle(ASHXStyles: TsgSHXFontlist;
   PStyle : TsgSHXTextStyle; Text: sgRawByteString; const WideText: WideString;
   CodePage: Integer; Vertical, Backward: Boolean; APoly: TsgSHXVertexList;
   Fstyle: TmvFontStyles; var Above, Below, Left, Bottom, Right, Top: Double;
   const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;
procedure GetSHXTextFromStyle(ASHXStyles: TsgSHXFontlist; PStyle: TsgSHXTextStyle;
  const AIndices: array of Word; Vertical, Backward: Boolean; APoly: TsgSHXVertexList;
  Fstyle: TmvFontStyles; var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;

procedure GetSHXTextBox(ASHXStyles: TsgSHXFontlist; const Style: string;
  const Text: sgRawByteString; const WideText: WideString; const AMap: TsgSymbolsMap;
  CodePage: Integer; Vertical: Boolean; Fstyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double); overload;
procedure GetSHXTextBox(ASHXStyles: TsgSHXFontlist; const Style: string;
  const AIndices: array of Word; const AMap: TsgSymbolsMap;
  Vertical: Boolean; Fstyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double); overload;

procedure DrawSHXTextBox(ASHXStyles: TsgSHXFontlist; const Font: string;
  const Text: sgRawByteString; const WideText: WideString;
  CodePage: Integer; Vertical: Boolean; Fstyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double; const AMap: TsgSymbolsMap = ''); overload;
procedure DrawSHXTextBox(ASHXStyles: TsgSHXFontlist; const Font: string;
  const AIndices: array of Word; Vertical: Boolean; Fstyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double; const AMap: TsgSymbolsMap); overload;

procedure GetSHXTextBoxFromStyle(ASHXStyles: TsgSHXFontlist; PStyle : TsgSHXTextStyle;
   const Text: sgRawByteString; const WideText: WideString;
   CodePage: Integer; Vertical: Boolean;  Fstyle: TmvFontStyles;
   var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
   After: Double; const AMap: TsgSymbolsMap = ''); overload;
procedure GetSHXTextBoxFromStyle(ASHXStyles: TsgSHXFontlist; PStyle : TsgSHXTextStyle;
   const AIndices: array of Word; Vertical: Boolean; Fstyle: TmvFontStyles;
   var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
   After: Double; const AMap: TsgSymbolsMap); overload;

function IsUnicodeSHX(ASHXStyles: TsgSHXFontlist; PStyle: TsgSHXTextStyle; const AFontName: string): Boolean;

procedure SHXFreePoly(var Poly: TsgSHXVertexList);

function FindShape(ASHXStyles: TsgSHXFontlist; var ShapeNumber: Word; var ShapeName, FontName: string): Boolean;

procedure RefreshHashLists;

function GetSHXKind(const AStream: TStream; var ASHXKind: TsgSHXKinds): Boolean; overload;
function GetSHXKind(const AFileName: string; var ASHXKind: TsgSHXKinds): Boolean; overload;
// for tkNormSHX only
function IsShapeSHX(const AStream: TStream): Boolean; overload;
function IsShapeSHX(const AFileName: string): Boolean; overload;

function GetSHXPolyData(const AShape: PsgSHXRecord;
  const AVertical: Boolean): PsgSHXPolyData;

function GetBigFontCodePage(const AName: string): Integer;
function GetSHXFont(const ASHXStyles: TsgSHXFontlist;
  const PStyle: TsgSHXTextStyle; const AFontName: string;
  const ABigFont: Boolean): TsgSHXFont;
function RegBigFont(const AName: string; const ACodePage: Integer): Boolean;
procedure ClearRegBigFonts;
function GetBigFontFlagsInternal(const AName: string): Word;

function sgSHXFileExists(const AFileName: string): Boolean;
procedure sgSetSHXFileExists(const AFileName: string; const AValue: Boolean);

implementation

uses
  Math, TypInfo{$IFDEF POSIX}, Posix.Unistd{$ENDIF};

const
  cnstDefaultCodePage = 0;
  cnstUndefinedCodePage = -1;
  cnstXOffsetKoef = 0.150;
  cnstYOffsetKoef = 0.200;
  MAX_SHAPE_LEN = 2000 + 1;
  MAX_FONTNAME_LEN = 129;
  cnstHeadShapes10: AnsiString = 'AutoCAD-86 shapes 1.1';
  cnstHeadShapes11: AnsiString = 'AutoCAD-86 shapes 1.0';
  cnstHeadBigFont10: AnsiString = 'AutoCAD-86 bigfont 1.0';
  cnstHeadUniForm10: AnsiString = 'AutoCAD-86 unifont 1.0';

  cnstAnsiChar = WideChar('0');
  cnstWideChar = WideChar('1');

  cnstDirectorySeparator: Char = '\';

  SHXBytesInstanceRawSize = 8 * SizeOf(Pointer);

var
  DirExistsList: TStringList = nil;
  FileExistsList: TStringList = nil;
  RegisterBigShxFonts: TStringList = nil;

  cnstObjectNoExist: TObject = nil;
  cnstObjectExist: TObject = nil;

  cnstNoStopped: Boolean = False;

type
  TsgTextIterator = class
  private
    FText: AnsiString;
    FWideText: WideString;
    FMapWideText: array of TsgSymbolType;
    FWideTextTyped: WideString;
    FWideTextTypedFonts: array of TsgSHXFont;
    FFont: TsgSHXFont;
    FBigFont: TsgSHXFont;
    FScale: TsgSHXPoint;
    FIndices: array of Word;
    FUndefinedSymbolIndex: Integer;
    FStopped: PBoolean;
    function IsStopped: Boolean;{$IFDEF SG_INLINE}inline;{$ENDIF}
    function IterateBoxI(const AFontProcParam: PsgFontProcParams; var Above,
      Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
      After: Double): Boolean;
    function GetShapeDataI(var AFont: TsgSHXFont; var Index: Integer;
      const AFontParams: PsgFontProcParams; var AScale: TsgSHXPoint): PsgSHXPolyData;
  protected
    function GetActiveFont(const ASymbol: Word; const AType: TsgSymbolType): TsgSHXFont;
    function GetModeIterate: Integer;
    function GetShapeData(AFont: TsgSHXFont; ASymbol: Word;
      const AFontParams: PsgFontProcParams): PsgSHXPolyData;
    function GetSymbol(const AFont: TsgSHXFont; const S: Word;
      const P: PsgFontProcParams; const AScaled: Boolean;
      const AUseUndefined: Boolean): Byte; overload;
    function GetSymbolI(const AFont: TsgSHXFont; Index: Integer;
      const P: PsgFontProcParams; const AScaled: Boolean;
      const AUseUndefined: Boolean): Byte; overload;
    procedure LoadFonts;
    function IterateI(const AFontProcParam: PsgFontProcParams): Boolean;
    function IterateA(const AFontProcParam: PsgFontProcParams): Boolean;
    function IterateAW(const AFontProcParam: PsgFontProcParams): Boolean;
    function IterateW(const AFontProcParam: PsgFontProcParams): Boolean;
  public
    constructor Create(const AString: AnsiString; const AWideString: WideString;
      const AMap: TsgSymbolsMap; const AFont, ABigFont: TsgSHXFont); overload;
    constructor Create(const AIndices: array of Word; const AMap: TsgSymbolsMap;
      const AFont, ABigFont: TsgSHXFont); overload;
    destructor Destroy; override;
    function Iterate(const AVertical, ABackward: Boolean;
      const AStyle: TmvFontStyles; APoly: TsgSHXVertexList; var X, Y: Double;
      var ABig: Boolean; const Stopped: PBoolean = nil): Boolean; overload;
    function Iterate(const AFontProcParam: PsgFontProcParams;
      const Stopped: PBoolean = nil): Boolean; overload;
    function IterateBox(const Vertical: Boolean;  Fstyle : TmvFontStyles;
      var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
      After: Double): Boolean; overload;
    function IterateBox(const AFontProcParam: PsgFontProcParams;
      var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
      After: Double): Boolean; overload;
  end;

function GetFileUpperCase(const APath: string): string;
{$IFNDEF SG_CASESENSITIVE_PLATFORM}
begin
  Result := AnsiUpperCase(APath);
end;
{$ELSE}
var
  vFileName: string;
begin
  Result := APath;
  if Length(Result) > 0 then
  begin
    vFileName := {$IFDEF SG_FM_MOBILE}System.IOUtils.TPath.GetFileName(APath){$ELSE}ExtractFileName(APath){$ENDIF};
    if (Length(vFileName) > 0) and (Length({$IFDEF SG_FM_MOBILE}System.IOUtils.TPath.GetExtension(vFileName){$ELSE}ExtractFileExt(vFileName){$ENDIF}) > 0) then
    begin
      SetLength(Result, Length(Result) - Length(vFileName));
      Result := Result + AnsiUpperCase(vFileName);
    end;
  end;
end;
{$ENDIF}

function GetExistObject(const AExist: Boolean): TObject;
begin
  if AExist then
    Result := cnstObjectExist
  else
    Result := cnstObjectNoExist;
end;

function GetExistsValue(const AExistObject: TObject): Boolean;
begin
  Result := AExistObject <> cnstObjectNoExist;
end;

function CreateList: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
end;

function GetSHXFileName(const AFontName: string): string;
begin
  Result := AFontName;
  if Length(Result) > 0 then
  begin
    if Length(ExtractFileExt(Result)) = 0 then
       Result := Result + cnstSHXFontExt;
  end;
end;

function GetItemExists(var AStringList: TStringList; const AName: string;
  var AResult: Boolean): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  if AStringList = nil then
  begin
    AStringList := TStringList.Create;
    AStringList.Sorted := True;
    AStringList.Duplicates := dupIgnore
  end;
  vIndex := AStringList.IndexOf(AName);
  if vIndex > -1 then
  begin
    AResult := GetExistsValue(AStringList.Objects[vIndex]);
    Result := True;
  end;
end;

function RegisterBigShxFontsIndexOf(const AName: string): Integer;
begin
  if not Assigned(RegisterBigShxFonts) then
    RegBigFont('', cnstDefaultCodePage);
  Result := RegisterBigShxFonts.IndexOf(AnsiLowerCase(AName));
end;

function GetBigFontCodePage(const AName: string): Integer;
var
  I: Integer;
begin
  Result := cnstDefaultCodePage;
  I := RegisterBigShxFontsIndexOf(AName);
  if I > -1 then
    Result := TsgObjectInt64(RegisterBigShxFonts.Objects[I]).FieldInt64 and $FFFF;
end;

function GetBigFontFlagsInternal(const AName: string): Word;
var
  I: Integer;
begin
  Result := 0;
  I := RegisterBigShxFontsIndexOf(AName);
  if I > -1 then
    Result := (Cardinal(TsgObjectInt64(RegisterBigShxFonts.Objects[I]).FieldInt64) shr 16) and $FFFF;
end;

function GetSHXKind(const AFileName: string; var ASHXKind: TsgSHXKinds): Boolean;
var
  vStream: TStream;
begin
  ASHXKind := tkUndefined;
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := GetSHXKind(vStream, ASHXKind);
  finally
    vStream.Free;
  end;
end;

function GetSHXKind(const AStream: TStream; var ASHXKind: TsgSHXKinds): Boolean;
var
  vLen: Integer;
  vHeader: array[0 .. MAX_FONTNAME_LEN] of AnsiChar;
begin
  ASHXKind := tkUndefined;
  Result := True;
  try
    AStream.Read(vHeader, MAX_FONTNAME_LEN);
    vLen := Length(cnstHeadShapes10);
    if CompareMem(@vHeader[0], Pointer(cnstHeadShapes10), vLen) or CompareMem(@vHeader[0], Pointer(cnstHeadShapes11), vLen) then
      ASHXKind := tkNormSHX
    else
      if CompareMem(@vHeader[0], Pointer(cnstHeadBigFont10), Length(cnstHeadBigFont10)) then
        ASHXKind := tkBigSHX
      else
        if CompareMem(@vHeader[0], Pointer(cnstHeadUniForm10), Length(cnstHeadUniForm10)) then
          ASHXKind := tkUnicodeSHX
        else
          Result := False;
  except
    Result := False;
  end;
end;

function GetSHXPolyData(const AShape: PsgSHXRecord; const AVertical: Boolean): PsgSHXPolyData;
begin
  if AVertical then
    Result := @AShape^.ShapePolyVert
  else
    Result := @AShape^.ShapePoly;
end;

function IsShapeSHX(const AStream: TStream): Boolean;
var
  I: Integer;
  vNumChars: Word;
  ShapeNumber: Cardinal;
begin
  Result := True;
  AStream.Position := $1C;
  AStream.Read(vNumChars, SizeOf(vNumChars));
  I := 0;
  while (I < vNumChars) and Result do
  begin
    AStream.Read(ShapeNumber, SizeOf(ShapeNumber));
    if Word(ShapeNumber) = 0 then
      Dec(Result)
    else
      Inc(I);
  end;
end;

function IsShapeSHX(const AFileName: string): Boolean;
var
  vStream: TStream;
begin
  vStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := IsShapeSHX(vStream);
  finally
    vStream.Free;
  end;
end;

function IsDirExists(const ADirectory: string): Boolean;
begin
  if not GetItemExists(DirExistsList, ADirectory, Result) then
  begin
    Result := DirectoryExists(ADirectory);
    DirExistsList.AddObject(ADirectory, GetExistObject(Result));
  end;
end;

function sgSHXFileExists(const AFileName: string): Boolean;
begin
  if not GetItemExists(FileExistsList, AFileName, Result) then
  begin
    Result := FileExists(AFileName);
    FileExistsList.AddObject(AFileName, GetExistObject(Result));
  end;
end;

procedure RefreshHashLists;
var
  I: Integer;
  vValues: TStringList;
begin
  vValues := TStringList.Create;
  try
    if DirExistsList <> nil then
    begin
      vValues.Assign(DirExistsList);
      DirExistsList.Clear;
      for I := 0 to vValues.Count - 1 do
        IsDirExists(vValues[I]);
    end;
    if FileExistsList <> nil then
    begin
      vValues.Assign(FileExistsList);
      FileExistsList.Clear;
      for I := 0 to vValues.Count - 1 do
        sgSHXFileExists(vValues[I]);
    end;
  finally
    vValues.Free;
  end;
end;

function RegBigFont(const AName: string; const ACodePage: Integer): Boolean;
const
  cnstShortJIS = 932;
  cnstGB2312 = 936;
  cnstKSC5601 = 949;
  cnstBIG5 = 950;
const
  cnstBigFonts: array[0..9] of record
    Name: string;
    CodePage: Integer;
  end = (
//system fonts
    (Name: '@extfont2.shx'; CodePage: cnstShortJIS),
    (Name: 'bigfont.shx';   CodePage: cnstShortJIS),
    (Name: 'chineset.shx';  CodePage: cnstBIG5),
    (Name: 'extfont.shx';   CodePage: cnstShortJIS),
    (Name: 'extfont2.shx';  CodePage: cnstShortJIS),
    (Name: 'gbcbig.shx';    CodePage: cnstGB2312),
    (Name: 'whgdtxt.shx';   CodePage: cnstKSC5601),
    (Name: 'whgtxt.shx';    CodePage: cnstKSC5601),
    (Name: 'whtgtxt.shx';   CodePage: cnstKSC5601),
    (Name: 'whtmtxt.shx';   CodePage: cnstKSC5601)
//user fonts
       );
var
  I: Integer;
  vName: string;
begin
  Result := False;
  if not Assigned(RegisterBigShxFonts) then
  begin
    RegisterBigShxFonts := CreateList;
    for I := Low(cnstBigFonts) to High(cnstBigFonts) do
      RegisterBigShxFonts.AddObject(cnstBigFonts[I].Name,
        TsgObjectInt64.Create(cnstBigFonts[I].CodePage or $00010000));//autocad internal shx
  end;
  if Length(AName) > 0 then
  begin
    vName := ExtractFileName(AName);
    if Length(vName) < 1 then
      vName := AName;
    if Length(vName) > 0 then
    begin
      I := RegisterBigShxFonts.Count;
      RegisterBigShxFonts.AddObject(AnsiLowerCase(vName), TsgObjectInt64.Create(ACodePage));
      Result := RegisterBigShxFonts.Count > I;
    end;
  end;
end;

procedure ClearRegBigFonts;
var
  I: Integer;
  vObj: TObject;
begin
  if Assigned(RegisterBigShxFonts) then
  begin
    for I := RegisterBigShxFonts.Count - 1 downto 0 do
    begin
      vObj := RegisterBigShxFonts.Objects[I];
      RegisterBigShxFonts.Objects[I] := nil;
      FreeAndNil(vObj);
    end;
  end;
  FreeAndNil(RegisterBigShxFonts);
end;

procedure sgSetSHXFileExists(const AFileName: string; const AValue: Boolean);
var
  vIndex: Integer;
begin
  if FileExistsList <> nil then
  begin
    vIndex := FileExistsList.IndexOf(AFileName);
    if vIndex > -1 then
      FileExistsList.Objects[vIndex] := GetExistObject(AValue);
  end;
end;

function Max(A,B: Extended): Extended;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(A,B: Extended): Extended;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Find(AStringList: TStringList; const S: string;
  var Index: Integer): Boolean;
var
  I: Integer;
  CS, TS: string;
begin
  Result := False;
  Index := -1;
  CS := GetFileUpperCase(S);
  TS := ExtractFileName(CS);
  if CS <> TS then
  begin
    for I := 0 to AStringList.Count - 1 do
      if GetFileUpperCase(AStringList[I]) = CS then
      begin
        Result := True;
        Index := I;
        Exit;
      end;
  end
  else
  begin
    CS := ChangeFileExt(CS, cnstSHXFontExt);
    for I := 0 to AStringList.Count - 1 do
      if GetFileUpperCase(ExtractFileName(AStringList[I])) = CS then
      begin
        Result := True;
        Index := I;
        Exit;
      end;
  end;
end;

procedure SHXFreePoly(var Poly: TsgSHXVertexList);
begin
  Poly.Free;
  Poly := nil;
end;

function TextToWidestring(Font, BigFont : TsgSHXFont; const Text: sgRawByteString;
  const WideText: WideString;var IsBig: Boolean): WideString;
var
  I,K: Integer;
  N: word;
begin
  Result := '';
  IsBig := False;
  if Text <> '' then
  begin 
    K := Length(Text);
    if (BigFont<>nil) and (not BigFont.FExtndBig) then
    begin
      I := 1;
      while I <= K do
      begin
        if (I < K) and BigFont.HasPairSymbol(Ord(Text[I]), Ord(Text[I+1])) then
        begin
           N := 256 * Ord(Text[I])  +  Ord(Text[I+1]);
           Result := Result + '1';
           Result := Result + WideString(WideChar(n));
           I := I + 2;
           IsBig := True;
        end
        else
        begin
           Result := Result + '0';
           Result := Result + WideString(WideChar(Text[I]));
           inc(I);
        end;
     end
    end
    else
      for I := 1 to K do
      begin
        Result := Result + '0';
        Result := Result + WideString(WideChar(Text[I]));
      end
  end
  else
  begin
    K := length(WideText);
    if  (BigFont<>nil)  and (not BigFont.FExtndBig) then
    begin
      for I := 1 to K do
      begin
        if BigFont.HasSymbol(Word(WideText[I])) then
        begin
          Result := Result + '1';
          Result := Result + WideString(WideText[I]);
          IsBig := True;
        end
        else
        begin
          Result := Result + '0';
          Result := Result + WideString(WideText[I]);
        end;
      end;
    end
    else
       for I := 1 to K do
       begin
         Result := Result + '0';
         Result := Result + WideString(WideText[I]);
       end
  end;
end;

function ChangeUnicodeSymbolsCodes(AFont, ABigFont: TsgSHXFont;
  const AText: WideString; CodePage: Integer): WideString;
{$IFDEF SGDEL_2009}
var
  I, K, res, vCodePage: Integer;
  vIsLastAnsi: Boolean;
  S: AnsiString;
  vSymbol: WideChar;
{$ENDIF}
begin
  Result := AText;
  EXIT;
{$IFDEF SGDEL_2009}{$IFNDEF _FIXINSIGHT_}
  K := Length(AText);
  if (ABigFont <> nil)  and (not ABigFont.FExtndBig) then
  begin
    Result := '';
    vCodePage := ABigFont.GetCodePage;
    if vCodePage > 0 then// bigfont from autocad
    begin
      I := 1;
      while I <= K do
      begin
        res := LocaleCharsFromUnicode(vCodePage, 0, @AText[I], 1, nil, 0, cnstUndefinedSymbol, nil);
        if res > 0 then
        begin
          SetLength(S, res);
          SetCodePage(RawByteString(S), vCodePage, False);
          LocaleCharsFromUnicode(vCodePage, 0, @AText[I], 1, @S[1], res, cnstUndefinedSymbol, nil);
        end;
        vSymbol := AText[I];
        if ABigFont.BigFontInRange(Ord(S[1])) then
          if Length(S) > 1 then
            vSymbol := ConvertToWideChar(S[1], S[2])
          else
            vSymbol := Char(Word(Ord(S[1])));
        Result := Result + vSymbol;
        inc(I);
      end;
    end
    else
    begin
      vIsLastAnsi := False;
      for I := 1 to K do
      begin
        if Word(AText[I]) > 255 then
        begin
          if vIsLastAnsi then
            Result := Result + AText[I - 1];
          vIsLastAnsi := False;
          Result := Result + AText[I];
        end
        else
        begin
          if vIsLastAnsi then
             Result := Result + ConvertToWideChar(AText[I - 1], AText[I]);
          vIsLastAnsi := not vIsLastAnsi;
        end;
      end;
    end;
  end;
{$ENDIF}{$ENDIF}
end;

function BigFontOnly(Font, BigFont : TsgSHXFont; const Text: sgRawByteString;
  const WideText: WideString): Boolean;
var
  I,K: Integer;
begin
  Result := False;
  if BigFont=nil then Exit;
  if Text <> '' then
  begin
    K := Length(Text);
    if not BigFont.FExtndBig then
    begin
      I := 1;
      while I < K do
      begin
        if BigFont.HasPairSymbol(Ord(Text[I]), Ord(Text[I + 1])) then
        begin
          Result := True;
          Exit;
        end
        else
          Inc(I);
     end
    end
  end
  else
  begin
    K := Length(WideText);
    if not BigFont.FExtndBig then
    begin
      I := 1;
      while I <= K do
      begin
        if BigFont.HasSymbol(Word(WideText[I])) then
        begin
          Result := True;
          Exit;
        end
        else
          Inc(I);
      end
    end;
  end;
end;

procedure GetSHXFonts(const ASHXStyles: TsgSHXFontlist;
  const PStyle : TsgSHXTextStyle; var AFont, ABigFont: TsgSHXFont);
begin
  ABigFont := nil;
  AFont := ASHXStyles.Fonts.Objects[Pstyle.FontIndex] as TsgSHXFont;
  if AFont = nil then
  begin
    AFont := TsgSHXFont.Create(Pstyle.FontPath);
    ASHXStyles.Fonts.AddObject(Pstyle.FontPath, AFont);
    if not AFont.Load then Exit;
  end
  else
    if not AFont.Loaded then
    begin
      AFont.FName := Pstyle.FontPath;
      if not AFont.Load then Exit;
    end;
  if Pstyle.BigFontIndex < 0 then
    ABigFont := nil
  else
  begin
    ABigFont := ASHXStyles.Fonts.Objects[Pstyle.BigFontIndex] as TsgSHXFont;
    if ABigFont = nil then
    begin
      ABigFont := TsgSHXFont.Create(Pstyle.BigFontPath);
      ASHXStyles.Fonts.AddObject(Pstyle.BigFontPath, AFont);
      if not ABigFont.Load then Exit;
    end
    else
      if not ABigFont.Loaded then
      begin
        ABigFont.FName := Pstyle.BigFontPath;
        if not ABigFont.Load then Exit;
      end;
   end;
end;

procedure SetAboveBellow(const Font, BigFont: TsgSHXFont; var Above, Below: Double);
begin
  Above := Font.FAbove;
  Below := Font.FBelow;
end;

procedure ProcTextBox(Font, BigFont : TsgSHXFont; const Text: sgRawByteString;
  const WideText: WideString; const AMap: TsgSymbolsMap; Vertical: Boolean; Fstyle : TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double);
var
  vTextIterator: TsgTextIterator;
begin
  vTextIterator := TsgTextIterator.Create(Text, WideText, AMap, Font, BigFont);
  try
    vTextIterator.IterateBox(Vertical, Fstyle, Above, Below, Width, Height,
      RealLeft, RealBottom, RealRight, RealTop,  After);
  finally
    vTextIterator.Free;
  end;
end;

procedure ProcTextBoxI(Font, BigFont: TsgSHXFont; const AIndices: array of Word;
  const AMap: TsgSymbolsMap;
  Vertical: Boolean; Fstyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double);
var
  vTextIterator: TsgTextIterator;
  vFontProcParam: TsgFontProcParams;
begin
  vTextIterator := TsgTextIterator.Create(AIndices, AMap, Font, BigFont);
  try
    FillChar(vFontProcParam, SizeOf(vFontProcParam), 0);
    vFontProcParam.Scale.X := 1;
    vFontProcParam.Scale.Y := 1;
    vFontProcParam.Vertical := Vertical;
    vFontProcParam.Underscore := fmUnderline in FStyle;
    vFontProcParam.Overscore  := fmStrikeOut in FStyle;
    vTextIterator.IterateBoxI(@vFontProcParam, Above, Below, Width, Height,
      RealLeft, RealBottom, RealRight, RealTop, After);
  finally
    vTextIterator.Free;
  end;
end;

procedure GetSHXTextBox(ASHXStyles: TsgSHXFontlist; const Style: string;
  const Text: sgRawByteString; const WideText: WideString; const AMap: TsgSymbolsMap;
  CodePage: Integer; Vertical: Boolean; FStyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double); overload;
var
  I: Integer;
  St: TsgSHXTextStyle;
begin
  if Find(ASHXStyles.Styles, Style, I) then
  begin
    St := TsgSHXTextStyle(ASHXStyles.Styles.Objects[I]);
    if St <> nil then
    begin
      GetSHXTextBoxFromStyle(ASHXStyles, St, Text, WideText, CodePage, Vertical,
        FStyle, Above, Below, Width, Height, RealLeft, RealBottom, RealRight,
        RealTop, After, AMap);
    end;
  end;
end;

procedure GetSHXTextBox(ASHXStyles: TsgSHXFontlist; const Style: string;
  const AIndices: array of Word; const AMap: TsgSymbolsMap; Vertical: Boolean; Fstyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double); overload;
var
  I: Integer;
  St: TsgSHXTextStyle;
begin
  if Find(ASHXStyles.Styles, Style, I) then
  begin
    St := TsgSHXTextStyle(ASHXStyles.Styles.Objects[I]);
    if St <> nil then
    begin
      GetSHXTextBoxFromStyle(ASHXStyles, St, AIndices, Vertical,
        FStyle, Above, Below, Width, Height, RealLeft, RealBottom, RealRight,
        RealTop, After, AMap);
    end;
  end;
end;

procedure GetSHXTextBoxFromStyle(ASHXStyles: TsgSHXFontlist; PStyle : TsgSHXTextStyle;
   const Text: sgRawByteString; const WideText: WideString;
   CodePage: Integer; Vertical: Boolean; FStyle: TmvFontStyles;
   var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
   After: Double; const AMap: TsgSymbolsMap); overload;
var
  vFnt, vBigFnt: TsgSHXFont;
  vConvString: WideString;
begin
  GetSHXFonts(ASHXStyles, PStyle, vFnt, vBigFnt);
  vConvString := ChangeUnicodeSymbolsCodes(vFnt, vBigFnt, WideText, CodePage);
  ProcTextBox(vFnt,vBigFnt, Text, vConvString,  AMap, Pstyle.Vertical,  Fstyle,
    Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop, After);
end;

procedure GetSHXTextBoxFromStyle(ASHXStyles: TsgSHXFontlist; PStyle: TsgSHXTextStyle;
   const AIndices: array of Word; Vertical: Boolean; Fstyle: TmvFontStyles;
   var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
   After: Double; const AMap: TsgSymbolsMap); overload;
var
  vFnt, vBigFnt: TsgSHXFont;
begin
  GetSHXFonts(ASHXStyles, PStyle, vFnt, vBigFnt);
  ProcTextBoxI(vFnt, vBigFnt, AIndices, AMap, Pstyle.Vertical, Fstyle, Above, Below,
    Width, Height, RealLeft, RealBottom, RealRight, RealTop, After);
end;

procedure DrawSHXTextBox(ASHXStyles: TsgSHXFontlist;  const Font: string;
  const Text: sgRawByteString; const WideText: WideString;
  CodePage: Integer; Vertical: Boolean; Fstyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double; const AMap: TsgSymbolsMap); overload;
var
  I, J: Integer;
  vFnt: TsgSHXFont;
  S: string;
  vConvString: WideString;
begin
  S := '';
  I := ASHXStyles.IndexFontOfName(Font);
  if I < 0 then
  begin
    for J := 0 to ASHXStyles.SearchPath.Count - 1 do
    begin
      if sgSHXFileExists(ASHXStyles.SearchPath[J] + GetFileUpperCase(Font)) then
      begin
        S := ASHXStyles.SearchPath[J] + GetFileUpperCase(Font);
        Break;
      end;
    end;
    if S = '' then
      S := ASHXStyles.DefaultFontPath;
    vFnt := TsgSHXFont.Create(S);
    if not vFnt.Load then
    begin
      vFnt.Free;
      Exit;
    end;
    ASHXStyles.Fonts.AddObject(GetFileUpperCase(Font), vFnt);
  end
  else
    vFnt := ASHXStyles.Fonts.Objects[I] as TsgSHXFont;
  if vFnt = nil then Exit;
  vConvString := ChangeUnicodeSymbolsCodes(vFnt, nil, WideText, CodePage);
  ProcTextBox(vFnt, nil, Text, vConvString, AMap, Vertical, Fstyle,
    Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop, After);
end;

procedure DrawSHXTextBox(ASHXStyles: TsgSHXFontlist; const Font: string;
  const AIndices: array of Word; Vertical: Boolean; Fstyle: TmvFontStyles;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double; const AMap: TsgSymbolsMap); overload;
var
  I, J: Integer;
  vFnt: TsgSHXFont;
  S: string;
begin
  S := '';
  I := ASHXStyles.IndexFontOfName(Font);
  if I < 0 then
  begin
    for J := 0 to ASHXStyles.SearchPath.Count - 1 do
    begin
      if sgSHXFileExists(ASHXStyles.SearchPath[J] + GetFileUpperCase(Font)) then
      begin
        S := ASHXStyles.SearchPath[J] + GetFileUpperCase(Font);
        Break;
      end;
    end;
    if S = '' then
      S := ASHXStyles.DefaultFontPath;
    vFnt := TsgSHXFont.Create(S);
    if not vFnt.Load then
    begin
      vFnt.Free;
      Exit;
    end;
    ASHXStyles.Fonts.AddObject(GetFileUpperCase(Font), vFnt);
  end
  else
    vFnt := ASHXStyles.Fonts.Objects[I] as TsgSHXFont;
  if vFnt = nil then Exit;
  ProcTextBoxI(vFnt, nil, AIndices, AMap, Vertical, Fstyle, Above, Below, Width,
    Height, RealLeft, RealBottom, RealRight, RealTop, After);
end;

procedure ProcText(Font, BigFont : TsgSHXFont; const Text: sgRawByteString;
  const WideText: WideString; const AMap: TsgSymbolsMap;
  Vertical: Boolean; Backward: Boolean;
  Fstyle: TmvFontStyles;
  var X, Y: Double; APoly: TsgSHXVertexList; var Fbig: boolean;
  const Stopped: PBoolean); overload;
var
  vIterator: TsgTextIterator;
begin
  vIterator := TsgTextIterator.Create(Text, WideText, AMap, Font, BigFont);
  try
    vIterator.Iterate(Vertical, Backward, Fstyle, APoly, X, Y, FBig);
  finally
    vIterator.Free;
  end;
end;

procedure ProcText(Font, BigFont : TsgSHXFont; const AIndices: array of Word;
  const AMap: TsgSymbolsMap; Vertical: Boolean; Backward: Boolean;
  Fstyle: TmvFontStyles; var X, Y: Double; APoly: TsgSHXVertexList; var Fbig: boolean;
  const Stopped: PBoolean); overload;
var
  vIterator: TsgTextIterator;
begin
  vIterator := TsgTextIterator.Create(AIndices, AMap, Font, BigFont);
  try
    vIterator.Iterate(Vertical, Backward, Fstyle, APoly, X, Y, FBig, Stopped);
  finally
    vIterator.Free;
  end;
end;

function FindShape(ASHXStyles: TsgSHXFontlist; var ShapeNumber: Word;
  var ShapeName, FontName: string): Boolean;
var
  I: Integer;
  vFnt: TsgSHXFont;
  vShapeName: AnsiString;
begin
  Result := False;
  if (ASHXStyles.FShapeFonts.Count < 0) then Exit;
  for I := 0 to ASHXStyles.FShapeFonts.Count - 1 do
    if ASHXStyles.FShapeFonts.Objects[I] <> nil then
    begin
      vFnt := ASHXStyles.FShapeFonts.Objects[I] as TsgSHXFont;
      vShapeName := AnsiString(ShapeName);
      Result := vFnt.FindShape(ShapeNumber, vShapeName);
      if Result then
      begin
        FontName := vFnt.FName;
        ShapeName := string(vShapeName);
        Break;
      end;
    end;
end;

procedure GetSHXTextFromStyle(ASHXStyles: TsgSHXFontlist;
   PStyle : TsgSHXTextStyle; Text: sgRawByteString; const WideText: WideString;
   CodePage: Integer; Vertical, Backward: Boolean; APoly: TsgSHXVertexList;
   Fstyle: TmvFontStyles; var Above, Below, Left, Bottom, Right, Top: Double;
   const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil);
var
  vFnt, vBigFnt: TsgSHXFont;
  X, Y: Double;
  Fbig: Boolean;
  vConvString: WideString;
begin
  GetSHXFonts(ASHXStyles, PStyle, vFnt, vBigFnt);
  if (Stopped <> nil) and Stopped^  then
    Exit;
  vConvString := ChangeUnicodeSymbolsCodes(vFnt, vBigFnt, WideText, CodePage);
  ProcText(vFnt, vBigFnt, Text, vConvString, AMap, PStyle.Vertical,
    (PStyle.Backwards or PStyle.UpSideDown),Fstyle, X, Y, APoly, Fbig, Stopped);
  SetAboveBellow(vFnt, vBigFnt, Above, Below);
  Left := 0;
  Bottom := -Below;
  Top := Above;
  Right := X;
end;

procedure GetSHXTextFromStyle(ASHXStyles: TsgSHXFontlist; PStyle: TsgSHXTextStyle;
  const AIndices: array of Word; Vertical, Backward: Boolean; APoly: TsgSHXVertexList;
  Fstyle: TmvFontStyles; var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;
var
  vFnt, vBigFnt: TsgSHXFont;
  X, Y: Double;
  Fbig: Boolean;
begin
  GetSHXFonts(ASHXStyles, PStyle, vFnt, vBigFnt);
  if (Stopped <> nil) and Stopped^ then
    Exit;
  ProcText(vFnt, vBigFnt, AIndices, AMap, PStyle.Vertical,
    (PStyle.Backwards or PStyle.UpSideDown),Fstyle, X, Y, APoly, Fbig, Stopped);
  SetAboveBellow(vFnt, vBigFnt, Above, Below);
  Left := 0;
  Bottom := -Below;
  Top := Above;
  Right := X;
end;

procedure GetSHXText(ASHXStyles: TsgSHXFontlist; const Style: string;
  Text: sgRawByteString; const WideText: WideString;
  CodePage: Integer; APoly: TsgSHXVertexList;  Fstyle: TmvFontStyles;
  var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;
var
  I: Integer;
  St: TsgSHXTextStyle;
begin
  I := -1;
  if not Find(ASHXStyles.Styles, Style, I) then Exit;
  St := TsgSHXTextStyle(ASHXStyles.Styles.Objects[I]);
  if St <> nil then
    GetSHXTextFromStyle(ASHXStyles, St, Text, WideText, CodePage, St.Vertical,
      St.Backwards, APoly, Fstyle, Above, Below, Left, Bottom, Right, Top, AMap, Stopped);
end;

procedure GetSHXText(ASHXStyles: TsgSHXFontlist; const Style: string;
  const AIndices: array of Word; APoly: TsgSHXVertexList; Fstyle: TmvFontStyles;
  var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;
var
  I: Integer;
  St: TsgSHXTextStyle;
begin
  I := -1;
  if not Find(ASHXStyles.Styles, Style, I) then Exit;
  St := TsgSHXTextStyle(ASHXStyles.Styles.Objects[I]);
  if St <> nil then
    GetSHXTextFromStyle(ASHXStyles, St, AIndices, St.Vertical,
      St.Backwards, APoly, Fstyle, Above, Below, Left, Bottom, Right, Top, AMap, Stopped);
end;

procedure DrawSHXText(ASHXStyles: TsgSHXFontlist; const Font: string;
  Text: sgRawByteString; const WideText: WideString; CodePage: Integer; Vertical, Backward: Boolean; APoly: TsgSHXVertexList;
  Fstyle: TmvFontStyles;
  var Above, Below, Left, Bottom, Right, Top: Double; const AMap: TsgSymbolsMap;
  const Stopped: PBoolean = nil);
var
  I, J: Integer;
  vFnt: TsgSHXFont;
  X, Y: Double;
  S: string;
  Fbig: boolean;
  vConvString: WideString;
begin
  S := '';
  I := ASHXStyles.IndexFontOfName(Font);
  if I < 0 then
  begin
    for J := 0 to ASHXStyles.SearchPath.Count - 1 do
    begin
      if sgSHXFileExists(ASHXStyles.SearchPath[J] + GetFileUpperCase(Font)) then
      begin
        S := ASHXStyles.SearchPath[J] + GetFileUpperCase(Font);
        Break;
      end;
    end;
    if S = '' then S := ASHXStyles.DefaultFontPath;
    vFnt := TsgSHXFont.Create(S);
    if not vFnt.Load then
    begin
      vFnt.Free;
      Exit;
    end;
    ASHXStyles.Fonts.AddObject(GetFileUpperCase(Font), vFnt);
  end
  else
    vFnt := ASHXStyles.Fonts.Objects[I] as TsgSHXFont;
  if (vFnt = nil) or ((Stopped <> nil) and Stopped^) then Exit;
  vConvString := ChangeUnicodeSymbolsCodes(vFnt, nil, WideText, CodePage);
  ProcText(vFnt, nil, Text, vConvString, AMap, Vertical, Backward,Fstyle, X, Y, APoly, Fbig, Stopped);
  Above := vFnt.Above;
  Below := vFnt.Below;
  Left := 0;
  Bottom := -Below;
  Top := Above;
  Right := X;
end;

procedure DrawSHXText(ASHXStyles: TsgSHXFontlist; const Font: string;
  const AIndices: array of Word; Vertical, Backward: Boolean; APoly: TsgSHXVertexList;
  Fstyle: TmvFontStyles; var Above, Below, Left, Bottom, Right, Top: Double;
  const AMap: TsgSymbolsMap; const Stopped: PBoolean = nil); overload;
var
  I, J: Integer;
  vFnt: TsgSHXFont;
  X, Y: Double;
  S: string;
  Fbig: boolean;
begin
  S := '';
  I := ASHXStyles.IndexFontOfName(Font);
  if I < 0 then
  begin
    for J := 0 to ASHXStyles.SearchPath.Count - 1 do
    begin
      if sgSHXFileExists(ASHXStyles.SearchPath[J] + GetFileUpperCase(Font)) then
      begin
        S := ASHXStyles.SearchPath[J] + GetFileUpperCase(Font);
        Break;
      end;
    end;
    if S = '' then S := ASHXStyles.DefaultFontPath;
    vFnt := TsgSHXFont.Create(S);
    if not vFnt.Load then
    begin
      vFnt.Free;
      Exit;
    end;
    ASHXStyles.Fonts.AddObject(GetFileUpperCase(Font), vFnt);
  end
  else
    vFnt := ASHXStyles.Fonts.Objects[I] as TsgSHXFont;
  if (vFnt = nil) or ((Stopped <> nil) and Stopped^) then Exit;
  ProcText(vFnt, nil, AIndices, AMap, Vertical, Backward,Fstyle, X, Y, APoly, Fbig, Stopped);
  Above := vFnt.Above;
  Below := vFnt.Below;
  Left := 0;
  Bottom := -Below;
  Top := Above;
  Right := X;
end;

function GetSHXFont(const ASHXStyles: TsgSHXFontlist; const PStyle: TsgSHXTextStyle;
  const AFontName: string; const ABigFont: Boolean): TsgSHXFont;
var
  vFontIndex, I: Integer;
  vFnt: TsgSHXFont;
  vFontPath, S, S1: string;
begin
  Result := nil;
  S := '';
  vFontIndex := 0;
  vFontPath := '';
  if PStyle <> nil then
  begin
    if ABigFont then
    begin
      vFontIndex := Pstyle.BigFontIndex;
      vFontPath := Pstyle.BigFontPath;
    end
    else
    begin
      vFontIndex := Pstyle.FontIndex;
      vFontPath := Pstyle.FontPath;
    end;
  end;
  if (PStyle <> nil) and (AFontName = '')  and (vFontIndex >= 0) and (vFontIndex < ASHXStyles.Fonts.Count) then
  begin
    vFnt := ASHXStyles.Fonts.Objects[vFontIndex] as TsgSHXFont;
    if vFnt = nil then
    begin
      vFnt := TsgSHXFont.Create(vFontPath);
      ASHXStyles.Fonts.AddObject(vFontPath, vFnt);
      if not vFnt.Load then Exit;
    end;
    Result := vFnt;
  end
  else
  begin
    I := ASHXStyles.IndexFontOfName(AFontName);
    if I < 0 then
    begin
      S1 := GetFileUpperCase(AFontName);
      for I := 0 to ASHXStyles.SearchPath.Count - 1 do
      begin
        if sgSHXFileExists(ASHXStyles.SearchPath[I] + S1) then
        begin
          S := ASHXStyles.SearchPath[I] + S1;
          Break;
        end;
      end;
      if (S = '') and (AnsiPos(cnstDirectorySeparator, AFontName )>=0)   then begin
        if sgSHXFileExists(AFontName) then S := AFontName
        else
        begin
          S1 := GetFileUpperCase(ExtractFileName(AFontName));
          for I := 0 to ASHXStyles.SearchPath.Count - 1 do
          begin
            if sgSHXFileExists(ASHXStyles.SearchPath[I] + S1) then
            begin
              S := ASHXStyles.SearchPath[I] + S1;
              Break;
            end;
          end;
        end;
      end;

      if (S = '') then
        S := ASHXStyles.DefaultFontPath;
      I := ASHXStyles.IndexFontOfName(S);
      if I<0 then  begin
        vFnt := TsgSHXFont.Create(S);
        if not vFnt.Load then
        begin
          vFnt.Free;
          Exit;
        end;
        S := GetSHXFileName(AFontName);
        ASHXStyles.Fonts.AddObject(GetFileUpperCase(S), vFnt);
      end
      else
        vFnt := ASHXStyles.Fonts.Objects[I] as TsgSHXFont;
    end
    else
      vFnt := ASHXStyles.Fonts.Objects[I] as TsgSHXFont;
    if vFnt = nil then Exit;
    Result := vFnt;
  end;
end;

function IsUnicodeSHX(ASHXStyles: TsgSHXFontlist; PStyle: TsgSHXTextStyle;
  const AFontName: string): Boolean;
var
  vFnt: TsgSHXFont;
begin
  Result := False;
  vFnt := GetSHXFont(ASHXStyles, PStyle, AFontName, False);
  if Assigned(vFnt) and (vFnt.FKind = tkUnicodeSHX) then
    Result := True;
end;

{ TsgSHXBytes }

function TsgSHXBytes.Get(var Index: Integer): Byte;
begin
  if Index <= High(Data) then
  begin
    Result := Data[Index];
    Inc(Index);
  end
  else
    Result := 0;
end;

constructor TsgSHXFontList.Create(const ASearchPath, ADefault: string;
  const ADefaultBigFont: string = '');
var
  I: Integer;
  S: string;
begin
  FFonts := THashedStringList.Create;
  FFonts.CaseSensitive:=True;
  //FFonts.Sorted:=True;
  FStyles := TStringList.Create;
  FSearchPath := TsgStringList.Create;
  FShapeFonts := TStringList.Create;
  FMissedFonts := TStringList.Create;
  FSubstFonts := TStringList.Create;
  TsgStringList(FSearchPath).LineBreak := ';';
  FSearchPath.Text := ASearchPath;
  I := FSearchPath.Count - 1;
  while I >= 0 do
  begin
    S := Trim(FSearchPath[I]);
    S := {$IFNDEF SGDEL_6}AddLastSlash{$ELSE}IncludeTrailingPathDelimiter{$ENDIF}(S);
    if not IsDirExists(S) then
      FSearchPath.Delete(I)
    else
      FSearchPath[I] := S;
    Dec(I);
  end;
  FDefaultFontName := GetFileUpperCase(ADefault);
  for I := 0 to FSearchPath.Count - 1 do
  begin
    if sgSHXFileExists(FSearchPath[I] + FDefaultFontName)  then
    begin
      FDefaultFontPath := FSearchPath[I] + FDefaultFontName;
      Break;
    end;
  end;
  FDefaultBigFontName := ADefaultBigFont;
  if Length(FDefaultBigFontName) = 0 then
    FDefaultBigFontName := sDefaultSHXBigFont;
  FDefaultBigFontName := GetFileUpperCase(FDefaultBigFontName);
  for I := 0 to FSearchPath.Count - 1 do
  begin
    if sgSHXFileExists(FSearchPath[I] + FDefaultBigFontName)  then
    begin
      FDefaultBigFontPath := FSearchPath[I] + FDefaultBigFontName;
      Break;
    end;
  end;
end;

destructor TsgSHXFontList.Destroy;
var
  I: Integer;
  vObjTextStyle: TsgSHXTextStyle;
begin
  FSearchPath.Free;
  FShapeFonts.Free;
  FMissedFonts.Free;
  FSubstFonts.Free;
  if FFonts.Count >0 then
  for I := 0 to FFonts.Count - 1 do
    FFonts.Objects[I].Free;
  FFonts.Free;
  if FStyles.Count > 0 then
  begin
    for I := 0 to FStyles.Count - 1 do
    begin
      vObjTextStyle := TsgSHXTextStyle(FStyles.Objects[I]);
      if Assigned(vObjTextStyle) then
      begin
        FStyles.Objects[I] := nil;
        vObjTextStyle.Free;
      end;
    end;
  end;
  FStyles.Free;
  inherited Destroy;
end;

function TsgSHXFontList.AddStyle(const AName, AFontName, ABigFontName: string;
  AVertical, AUpSideDown, ABackwards: Boolean; AObliqueAngle, AWidth: Double): TsgSHXTextStyle;

  function FindSHXFont(const AFileName: string; const P: TsgSHXTextStyle;
    const AIsBigFont: Boolean): Integer;
  var
    I: INteger;
    vFileName, vFilePath: string;
  begin
    Result := 0;
    vFileName := GetFileUpperCase(AFileName);
    P.FontExists := False;
    if pos(cnstDirectorySeparator, AFileName) > 0  then
    begin
      P.FontExists := sgSHXFileExists(AFileName);
      if P.FontExists then
      begin
        if AIsBigFont then
          P.BigFontPath := vFileName
        else
          P.FontPath := vFileName;
        Result := 1;
      end
      else
        vFileName := ExtractFileName(AFileName);
    end;
    if not P.FontExists then
    begin
      for I := 0 to FSearchPath.Count - 1 do
      begin
        vFilePath := SearchPath[I] + vFileName;
        P.FontExists := sgSHXFileExists(vFilePath);
        if P.FontExists then
        begin
          if AIsBigFont then
            P.BigFontPath := vFilePath
          else
            P.FontPath := vFilePath;
          Result := 2;
          Break;
        end;
      end;
    end;
  end;

var
  I: Integer;
  P: TsgSHXTextStyle;
  S, US: string;
  vFnt: TsgSHXFont;
begin
  P := TsgSHXTextStyle.Create;
  P.Name := GetFileUpperCase(AName);
  P.Vertical:= AVertical;
  P.UpSideDown:= AUpSideDown;
  P.Backwards:= ABackwards;
  P.ObliqueAngle:= AObliqueAngle;
  P.Width := AWidth;
  P.FontExists := false;
  S := GetSHXFileName(AFontName);
  if (FSubstFonts.Count > 0)  then
  begin
     US :=  GetFileUpperCase(S);
     for I := 0 to FSubstFonts.Count -1 do
       if FSubstFonts.Names[I] = US then
       begin
         S := FSubstFonts.Values[US];
         break;
       end;
  end;
  FindSHXFont(S, P, False);
  if not P.FontExists then
  begin
    P.FontPath := DefaultFontPath;
    US :=  GetFileUpperCase(S);
    if (FMissedFonts.IndexOf(US ) < 0) and
      (ExtractFileExt(US) = cnstSHXFontExt)
    then  FMissedFonts.Add(US);
  end;
  vFnt := nil;
{$IFDEF SGFPC}
  I := FFonts.IndexOf(P.FontPath);
  if I >= 0 then
{$ELSE}
  if FFonts.Find(P.FontPath, I) then
{$ENDIF}
    P.FontIndex := I
  else
  begin
    vFnt := TsgSHXFont.Create(P.FontPath);
    vFnt.Load;
    P.FontIndex := FFonts.AddObject(P.FontPath, vFnt);
  end;
  if (AName = '') and ((vFnt = nil) or vFnt.FIsShape) and (FShapeFonts.IndexOf(AFontName) < 0) then
     FShapeFonts.AddObject(AFontName, vFnt);
  S := GetSHXFileName(ABigFontName);
  if S = '' then
  begin
    P.BigFontIndex := -1;
    P.BigFontPath := '';
  end
  else
  begin
    FindSHXFont(S, P, True);
    if not P.FontExists then
    begin
      P.BigFontPath := DefaultBigFontPath;
      if not sgSHXFileExists(P.BigFontPath) then
        P.BigFontPath := DefaultFontPath;
    end;
    I := FFonts.IndexOf(ABigFontName);
    if I >= 0 then
      P.BigFontIndex := I
    else
    begin
      vFnt := TsgSHXFont.Create(P.BigFontPath);
      vFnt.Load;
      P.BigFontIndex := FFonts.AddObject(ABigFontName, vFnt);
    end;
  end;
  Styles.AddObject(AName, P);
  Result := P;
end;

function TsgSHXFontList.IndexFontOfName(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Find(FFonts, GetFontName(AName), I) then
    Result := I;
end;

constructor TsgSHXFont.Create(const AFontPath: string);
begin
  FKind := tkUndefined;
  FName := AFontPath;
  FShapes := TsgList.Create;
  FShapes.ProcCompare := SymbolCompare;
  FAbove := 0;
  FBelow := 0;
  FNumChars := 0;
  FModes := 0;
  FEmbtype := 0;
  FEncoding := 0;
  FLineFeed := 0;
  FIsShape := False;
  FCodePage := cnstUndefinedCodePage;
end;

destructor TsgSHXFont.Destroy;
begin
  CleareShapes;
  FShapes.Free;
  inherited  Destroy;
end;

function TsgSHXFont.SymbolCompare(const A, B: Pointer): Integer;
begin
  if PsgSHXRecord(A)^.ShapeNumber > PsgSHXRecord(B)^.ShapeNumber then Result := 1
  else if PsgSHXRecord(A)^.ShapeNumber = PsgSHXRecord(B)^.ShapeNumber then Result := 0
  else Result := -1;
end;

function TsgSHXFont.FindShape(var N: Word; var Name: AnsiString): Boolean;
var
  I: Integer;
  P: PsgSHXRecord;
  vShapeName, vName: string;
begin
  Result := False;
  if Name = '' then
  begin
    I := 0;
    while (I < NumChars) and not Result do
    begin
      P := PsgSHXRecord(FShapes[I]);
      if P^.ShapeNumber = N then
      begin
        Result := True;
        SetString(Name, PAnsiChar(@P^.ShapeBytes[0]), P^.ShapeNameLen);
        Exit;
      end;
      Inc(I);
    end;
  end
  else
  begin
    vName := string(Name);
    I := 0;
    while (I < NumChars) and not Result do
    begin
      P := PsgSHXRecord(FShapes[I]);
      SetString(vShapeName, PAnsiChar(@P^.ShapeBytes[0]), P^.ShapeNameLen);
      if AnsiCompareText(vName, vShapeName) = 0 then
      begin
        Result := True;
        N := P^.ShapeNumber;
        Exit;
      end;
      Inc(I);
    end;
  end;
end;

procedure TsgSHXFont.ParseNormSHX(AStream: TStream);
var
  I, J, vPos: Integer;
  P: PsgSHXRecord;
  vIsShape: Boolean;
  vSHXSymbol: PsgSHXRecord;
begin
  AStream.Position := $1C;
  AStream.Read(FNumChars, SizeOf(FNumChars));
  PrepareShapes(FShapes, True);
  for I := 0 to NumChars - 1 do
  begin
    vSHXSymbol := FShapes[I];
    AStream.Read(vSHXSymbol^.ShapeNumber, SizeOf(Word));
    AStream.Read(vSHXSymbol^.DefBytes, SizeOf(Word));
    if vSHXSymbol^.ShapeNumber in [1..255] then
      FixedTable[vSHXSymbol^.ShapeNumber]:= i;
  end;
  vPos:= AStream.Position;
  vIsShape := True;
  for I := 0 to NumChars - 1 do
  begin
    P := PsgSHXRecord(FShapes[I]);
    P^.BytesPos := vPos;
    if P^.ShapeNumber = 0 then
    begin
      vIsShape := False;
      P^.ShapeBytesCount := P^.DefBytes;
      ReallocMem(Pointer(P), SizeOf(TsgSHXRecord) + P^.ShapeBytesCount);
      FShapes[I] := P;
      AStream.Read(P^.ShapeBytes[0], P^.ShapeBytesCount);
      P^.ShapeNameLen := UpdateShapeName(Slice(P^.ShapeBytes, P^.ShapeBytesCount));
      J := P^.ShapeNameLen;
      FAbove := P^.ShapeBytes[J + 1];
      FBelow := P^.ShapeBytes[J + 2];
      FModes := P^.ShapeBytes[J + 3];
    end;
    vPos := vPos + P^.DefBytes;
  end;
  if vIsShape then
  begin
    FIsShape := vIsShape;
    for I := 0 to NumChars - 1 do
    begin
      P := PsgSHXRecord(FShapes[I]);
      AStream.Position := P^.BytesPos;
      P^.ShapeBytesCount := P^.BytesPos;
      ReallocMem(Pointer(P), SizeOf(TsgSHXRecord) + P^.ShapeBytesCount);
      FShapes[I] := P;
      AStream.Read(P^.ShapeBytes[0], P^.ShapeBytesCount);
      P^.ShapeNameLen := UpdateShapeName(Slice(P^.ShapeBytes, P^.ShapeBytesCount));
    end;
  end;
  FUnits := FAbove;
end;

procedure TsgSHXFont.AddNewShape(ShapeNumber: Word; const ShapeName: AnsiString;
  AShapePoly: TsgList);
var
  P: PsgSHXRecord;
  Shape: TBytes;
  vTotalBytes: Integer;
  L, vShapeDataBytesCount: Word;
begin
  L := Length(ShapeName);
  SetLength(Shape, 2200);
  vShapeDataBytesCount := ConvertPolyToShape(Shape, AShapePoly);
  vTotalBytes := SizeOf(TsgSHXRecord) + L + vShapeDataBytesCount;
  P := AllocMem(vTotalBytes);
  P^.ShapeNameLen := L;
  FShapes.Add(P);
  Inc(FNumChars);
  P^.ShapeNumber := ShapeNumber;
  System.Move(PPointer(ShapeName)^, P^.ShapeBytes[0], L);
  P^.ShapeBytes[L] := 0;
  System.Move(Shape[0], P^.ShapeBytes[L + 1], vShapeDataBytesCount);
  P^.DefBytes := vShapeDataBytesCount + L + 1;
  P^.ShapeBytesCount := P^.DefBytes;
  SetLength(Shape, 0);
end;

function TsgSHXFont.BigFontInRange(c: byte): boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to FNBigFontRanges do
    if c in [FBigFontRanges[I,1]..FBigFontRanges[I,2]] then exit;
  Result:=False;
end;

function TsgSHXFont.BigFontInRangeWord(ASymbol: Word): Boolean;
type
  TWordRecord = array [1..2] of Byte;
begin
  Result := BigFontInRange(TWordRecord(ASymbol)[2]);
end;

function PenDown(const APenDown: Integer): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := APenDown <> 0;
end;

function TsgSHXFont.ExtractSubSymb(const N: Word; var InsX, InsY: Double; var Scale: TsgSHXPoint;
  var APenDown: Integer;
  const CharWidth, CharHeight: Byte; const Vert, Backward: Boolean;
  APoly: TsgSHXVertexList; const AStackGlobal: PsgSHXStack): Boolean;
var
  C: Byte;
  Cw: Word;
  Xoff, Yoff: Shortint;
  VertOnly, Res, Sf: Boolean;
  I, Il, Id, Ir, Ia, Nn, O1, O2, So, Eo, J: Integer;
  CpX, CpY, Xc, Yc, X3, Y3, A1, B: Double;
  vCpX, vCpY: Double;
  Iang, Sa, Ea, Soct, Eoctrel, Soff, Eoff: Double;
  BufRaw: array[0 .. SHXBytesInstanceRawSize - 1] of Byte;
  Buf: TsgSHXBytes;
  Pfs : PsgSHXRecord;
  vShapePoly: PsgSHXPolyData;
  BaseX, BaseY, SubWidth, SubHeight: byte;
  vStack: PsgSHXStack;
  vStackCur: TsgSHXStack;
  vScale, R: TsgSHXPoint;
  vRadius: Integer;
begin
  Result := True;
  if AStackGlobal = nil then
  begin
    FillChar(vStackCur, SizeOf(vStackCur), 0);
    vStack := @vStackCur;
  end
  else
    vStack := AStackGlobal;

  Pfs := PsgSHXRecord(FShapes[N]);

  Pointer(Buf) := Pointer(@BufRaw[0]);
  TClass(Pointer(Buf)^) := TsgSHXBytes;
  Pointer(Buf.Data) := nil;
  SetLength(Buf.Data, Pfs^.ShapeBytesCount - Pfs^.ShapeNameLen);
  System.Move(Pfs^.ShapeBytes[Pfs^.ShapeNameLen + 1], Buf.Data[0], Length(Buf.Data));

  vShapePoly := GetSHXPolyData(Pfs, Vert);
  vShapePoly^.Top := -1E5;
  vShapePoly^.Bottom := 1E5;
  vShapePoly^.LastDown:= 0;

  CpX := InsX;
  CpY := InsY;
  MoveCurpos(APoly, 0, 0, Scale, False, CpX, CpY);

  VertOnly := False;

  I := 0;
  if (APenDown < 0) and ((FKind = tkBigSHX) and not FExtndBig) and (AStackGlobal = nil) then
    APenDown := 0
  else
    APenDown := 1;

  while I <= High(Buf.Data) do
  begin
    C := Buf.Get(I);
    if C < 16 then
    begin
      case C of
        0:
          Break; // finish
        1:
          if not VertOnly then
            APenDown := 1;
        2:
          if not VertOnly then
            APenDown := 0;
        3:
          begin
            C := Buf.Get(I);
            if (not VertOnly) and (C<>0) then
            begin
              Scale.X := Scale.X / C;
              Scale.Y := Scale.Y / C;
            end;
           end;
        4:
          begin
            C := Buf.Get(I);
            if not VertOnly then
            begin
              Scale.X := Scale.X * C;
              Scale.Y := Scale.Y * C;
            end;
          end;
        5:
          if not VertOnly then
          begin
            if vStack^.Index < 4 then
            begin
              Inc(vStack^.Index);
              vStack^.Value[vStack^.Index, 1] := CpX;
              vStack^.Value[vStack^.Index, 2] := CpY;
            end;
          end;
        6:
          if not VertOnly then
          begin
            if vStack^.Index > 0 then
            begin
              CpX := vStack^.Value[vStack^.Index, 1];
              CpY := vStack^.Value[vStack^.Index, 2];
              Dec(vStack^.Index);
              SetCurpos(APoly, CpX, CpY);
            end;
          end;
        7:
          begin  // subshape
            C := Buf.Get(I);
            if (FKind = tkBigSHX) and FExtndBig and (C = 0) then
            begin
              C := Buf.Get(I);
              Cw := Buf.Get(I);
              Cw := Cw + C * 256;
              BaseX := Buf.Get(I);
              BaseY := Buf.Get(I);
              SubWidth := Buf.Get(I);
              SubHeight := Buf.Get(I);

              if Find(Cw, J) then
              begin
                Cw := J;
                vScale.X := Scale.X * SubWidth / CharWidth;
                vScale.Y := Scale.Y * SubHeight / CharHeight;
                vCpX := BaseX * Scale.X;
                vCpy := BaseY * Scale.Y;
                Result := ProcessSymbI(Cw, vCpX, vCpY, vScale, APenDown, Vert, Backward, APoly, vStack);
              end
              else
              begin
                CpX := BaseX * Scale.X;
                Cpy := BaseY * Scale.Y;
                Result := ExtSubSymb(Cw, CpX, CpY, Scale, APenDown, SubWidth, SubHeight, Vert, Backward, APoly, vStack);
              end;
              if not Result then
                Exit;
            end
            else
            begin
              if (FKind = tkUnicodeSHX) {or (FKind = tkBigSHX)}then
              begin
                Cw := Buf.Get(I);
                Cw := Cw + C * 256;
              end
              else
                Cw := C;
              if not vertonly then
              begin
                Result := ProcessSymb(Cw, CpX, CpY, Scale, APenDown, Vert, Backward, APoly, AStackGlobal);
                if not Result then
                  Exit;
              end;
            end;
          end;
        8:
          begin    // X,Y displacement
            XOff := Shortint(Buf.Get(I));
            C := Buf.Get(I);
            YOff := Shortint(C);
            if not VertOnly  then
              MoveCurpos(APoly, XOff, YOff, Scale, PenDown(APenDown), CpX, CpY);
          end;
        9:
          begin
            XOff := Shortint(Buf.Get(I));
            C := Buf.Get(I);
            YOff := Shortint(C);
            while (XOff <> 0) or (YOff <> 0) do
            begin
              if not VertOnly then
                MoveCurpos(APoly, XOff, YOff, Scale, PenDown(APenDown), CpX, CpY);
              XOff := Shortint(Buf.Get(I));
              C := Buf.Get(I);
              YOff := Shortint(C);
            end;
          end;
        10:
          begin     //Octant arc
            vRadius := Buf.Get(I);
            R.X := vRadius * Scale.X;
            R.Y := vRadius * Scale.Y;
            C := Buf.Get(I);
            if not VertOnly then
            begin
              Sf:= Boolean(C and $80);
              if Sf then
                Ia := C - $80
              else
                Ia := C;
              Ia := Abs(Ia);
              O1 := Ia div 16;
              Nn := Ia mod 16;
              if Sf then
                O2 := O1 - Nn
              else
                O2 := O1 + Nn;
              Xc := CpX - R.X * Cos(O1 * Pi / 4);
              Yc := CpY - R.Y * Sin(O1 * Pi / 4);
              if Nn <> 0 then
              begin
                if Sf then
                  B := - Tan(Nn * Pi / 16)
                else
                  B := Tan(Nn * pi / 16);
                CpX := Xc + R.X * Cos(O2 * Pi / 4);
                CpY := Yc + R.Y * Sin(O2 * Pi / 4);
                SetBulge(APoly, CpX, CpY, B, PenDown(APenDown));
              end
              else
              begin         // full circle
                if Sf then
                begin
                  B := 1;
                  O2 := O1 - 4;
                end
                else
                begin
                  B := -1;
                  O2 := O1 + 4;
                end;
                X3 := CpX;
                Y3 := CpY;
                CpX := Xc + R.X * Cos(O2 * Pi / 4);
                CpY := Yc + R.Y * Sin(O2 * Pi / 4);
                SetBulge(APoly, CpX, CpY, B, PenDown(APenDown));
                CpX := X3;
                CpY := Y3;
                SetBulge(APoly, CpX, CpY, B, PenDown(APenDown));
              end;
            end;
          end;
        11:
          begin    // Fractional arc
            So := Buf.Get(I);
            Eo := Buf.Get(I);
            Ir := Buf.Get(I);
            vRadius := (Ir * 256 + Buf.Get(I));
            R.X := vRadius * Scale.X;
            R.Y := vRadius * Scale.Y;
            C := Buf.Get(I);
            if not VertOnly then
            begin
              Sf := Boolean(C and  $80);
              if Sf then
                Ia := C - $80
              else
                Ia := C;
              O1 := Ia div 16;
              O2 := Ia mod 16;
              Soct := 45 * O1;
              if O2=0 then O2 := 8;
              Eoctrel := 45.0 * ( O2 - 1 );
              A1 := So *45.0/256.0;
              Soff := Ceil(A1);
              A1 := Eo * 45.0 / 256.0;
              Eoff := Ceil(A1);
              if (Eoff < 0.9) then  eoff := 45.0;
              if Sf then
                Sa := Soct - Soff
              else
                Sa := Soct + Soff;
              Sa := Sa * Pi / 180;
              A1 := Eoctrel + Eoff;
              if Sf then
                Ea := Soct - A1
              else
                Ea := Soct + A1;
              Ea := Ea * Pi/180;
              Iang := Ea - Sa;
              Cpx := Cpx + R.X * (Cos(Ea) - Cos(Sa));
              Cpy := Cpy + R.Y * (Sin(Ea) - Sin(Sa));
              B := Tan(0.25 * Iang);
              SetBulge(APoly, CpX, CpY, B, PenDown(APenDown));
            end;
          end;
        12:
          begin // Arc with bulge
            Xoff := Shortint(Buf.Get(I));
            Yoff := Shortint(Buf.Get(I));
            C := Buf.Get(I);
            B := Shortint(C) / 127;
            if not VertOnly then
            begin
               CpX := CpX + Xoff * Scale.X;
               CpY := CpY + Yoff * Scale.Y;
               SetBulge(APoly, CpX, CpY, B, PenDown(APenDown));
             end;
          end;
         13:
           repeat //Multiple arc
             Xoff := Shortint(Buf.Get(I));
             C := Buf.Get(I);
             Yoff := Shortint(C);
             Res := (Xoff = 0) and (Yoff = 0);
             if Res then Break;

             begin
               C := Buf.Get(I);
               B := Shortint(C) / 127;
               CpX := CpX + Xoff * Scale.X;
               CpY := CpY + Yoff * Scale.Y;
               if not VertOnly then
                 SetBulge(APoly, CpX, CpY, B, PenDown(APenDown));
             end;
           until Res;
        14: // Only vertical
          VertOnly := not Vert;
      end;
      if C <> 14 then VertOnly := False;
    end
    else
    begin
      if VertOnly  then
      begin
        VertOnly := False;
        Continue;
      end;
      Il := C div 16;
      Id := C mod 16;
      case Id of
        0:
          MoveCurpos(APoly, Il, 0, Scale, PenDown(APenDown), CpX, CpY);
        1:
          MoveCurpos(APoly, Il, Il / 2, Scale, PenDown(APenDown), CpX, CpY);
        2:
          MoveCurpos(APoly, Il, Il, Scale, PenDown(APenDown), CpX, CpY);
        3:
          MoveCurpos(APoly, Il / 2, Il, Scale, PenDown(APenDown), CpX, CpY);
        4:
          MoveCurpos(APoly, 0, Il, Scale, PenDown(APenDown), CpX, CpY);
        5:
          MoveCurpos(APoly, -Il / 2, Il, Scale, PenDown(APenDown), CpX, CpY);
        6:
          MoveCurpos(APoly, -Il, Il, Scale, PenDown(APenDown), CpX, CpY);
        7:
          MoveCurpos(APoly, -Il, Il / 2, Scale, PenDown(APenDown), CpX, CpY);
        8:
          MoveCurpos(APoly, -Il, 0, Scale, PenDown(APenDown), CpX, CpY);
        9:
          MoveCurpos(APoly, -Il, -Il / 2, Scale, PenDown(APenDown), CpX, CpY);
        10:
          MoveCurpos(APoly, -Il, -Il, Scale, PenDown(APenDown), CpX, CpY);
        11:
          MoveCurpos(APoly, -Il / 2, -Il, Scale, PenDown(APenDown), CpX, CpY);
        12:
          MoveCurpos(APoly, 0, -Il, Scale, PenDown(APenDown), CpX, CpY);
        13:
          MoveCurpos(APoly, Il / 2, -Il, Scale, PenDown(APenDown), CpX, CpY);
        14:
          MoveCurpos(APoly, Il, -Il, Scale, PenDown(APenDown), CpX, CpY);
        15:
          MoveCurpos(APoly, Il, -Il / 2, Scale, PenDown(APenDown), CpX, CpY);
      end;
    end;
  end;
  InsX := CpX;
  InsY := CpY;
  SetLength(Buf.Data, 0);
  Pointer(Buf) := nil;
end;

procedure TsgSHXFont.ParseBigSHX(AStream: TStream);
var
  I, J, vIndex: Integer;
  P: PsgSHXRecord;
  iShapeNumber: Word; 	   // shape number
  iDefByt: Word;	   // number of data bytes in shape, 2000 max
  lFileOffset: Cardinal	;  // file offset to shape specification bytes
  lNextBigShapeHeader: Cardinal;
  lNumberOfShapes : Cardinal;
  lStartOfShapeDefs: Cardinal;
begin
  FExtndBig := False;
  lNumberOfShapes := 0;
  lStartOfShapeDefs := Cardinal($ffffffff);
  AStream.Position := $1B;
  AStream.Read(FNumChars, SizeOf(FNumChars));
  AStream.Read(FNBigFontRanges, SizeOf(FNBigFontRanges));
  for I := 1 to  FNBigFontRanges do
  begin
    AStream.Read( FBigFontRanges[i,1],SizeOf(word));
    AStream.Read( FBigFontRanges[i,2],SizeOf(word));
  end;
  //PrepareShapes(FShapes, False);
  while True do
  begin
    AStream.Read(iShapeNumber, SizeOf(Word));
    AStream.Read(iDefByt, SizeOf(Word));
    AStream.Read(lFileOffset, SizeOf(lFileOffset));
    if (lNumberOfShapes = 0) then
        lStartOfShapeDefs := lFileOffset;

    lNextBigShapeHeader := AStream.Position;
    if (lNextBigShapeHeader > lStartOfShapeDefs ) then Break;
    if (lNumberOfShapes > 0) and (iShapeNumber = 0) then Continue;

    P := AllocMem(SizeOf(TsgSHXRecord));
    vIndex := FShapes.Add(P);
    P^.DefBytes := iDefByt;
    P^.BytesPos := lFileOffset;
    P^.ShapeNumber := iShapeNumber;
    if iShapeNumber = 0 then
    begin
      AStream.Position := lFileOffset;
      P^.ShapeBytesCount := P^.DefBytes;
      ReallocMem(Pointer(P), SizeOf(TsgSHXRecord) + P^.ShapeBytesCount);
      FShapes[vIndex] := Pointer(P);
      AStream.Read(P^.ShapeBytes[0], P^.ShapeBytesCount);
      P^.ShapeNameLen := UpdateShapeName(Slice(P^.ShapeBytes, P^.ShapeBytesCount));
      j := P^.ShapeNameLen;
      case (iDefByt - j - 1) of
        5:
          begin
            FExtndBig := True;
            FCharHeight := Byte(P^.ShapeBytes[J + 1]);
            FModes := Byte(P^.ShapeBytes[J + 3]);
            FCharWidth :=  Byte(P^.ShapeBytes[J + 4]);
          end;
      else
        if PWord(@P^.ShapeBytes[J])^ = $0000 then
        begin
          Inc(J);
          FExtndBig := True;
          FCharHeight := Byte(P^.ShapeBytes[J + 1]) div 8;
          FModes := Byte(P^.ShapeBytes[J + 2]);
          FCharWidth := FCharHeight;
        end
        else
        begin
          FAbove := Byte(P^.ShapeBytes[J + 1]);
          FUnits := FAbove;
          FBelow := Byte(P^.ShapeBytes[J + 2]);
          FModes := Byte(P^.ShapeBytes[J + 3]);
        end;
      end;
      AStream.Position := lNextBigShapeHeader;
    end;
    Inc(lNumberOfShapes);
  end;
  FNumChars := lNumberOfShapes;
  FShapes.Sort;
end;

procedure  TsgSHXFont.ParseUnicodeSHX(AStream: TStream);
var
  I, J: Integer;
  P: PsgSHXRecord;
begin
  AStream.Position := $19;
  AStream.Read(FNumChars, SizeOf(FNumChars));
  PrepareShapes(FShapes, False);
  P := PsgSHXRecord(FShapes[0]);
  AStream.Read(P^.ShapeNumber, SizeOf(Word));
  AStream.Read(P^.DefBytes, SizeOf(Word));
  P^.ShapeBytesCount := P^.DefBytes;
  ReallocMem(Pointer(P), SizeOf(TsgSHXRecord) + P^.ShapeBytesCount);
  FShapes[0] := P;
  AStream.Read(P^.ShapeBytes[0], P^.ShapeBytesCount);
  P^.ShapeNameLen := UpdateShapeName(Slice(P^.ShapeBytes, P^.ShapeBytesCount));
  J := P^.ShapeNameLen;
  FAbove := P^.ShapeBytes[J + 1];
  FUnits := FAbove;
  FBelow := P^.ShapeBytes[J + 2];
  FModes := P^.ShapeBytes[J + 3];
  FEncoding := P^.ShapeBytes[J + 4];
  FEmbtype := P^.ShapeBytes[J + 5];
  for I := 1 to NumChars - 1 do
  begin
    P := PsgSHXRecord(FShapes[I]);
    AStream.Read(P^.ShapeNumber, SizeOf(Word));
    AStream.Read(P^.DefBytes, SizeOf(Word));
    P^.BytesPos := AStream.Position;
    AStream.Position:= P^.BytesPos + P^.DefBytes;
  end;
  FShapes.Sort;
end;

procedure TsgSHXFont.ReadSHX(N: Word);
var
  vStream: TFileStream;
  P: PsgSHXRecord;
begin
  if not SHX.sgSHXFileExists(Name) then Exit;
  if N > FShapes.Count - 1 then Exit;
  P := PsgSHXRecord(FShapes[N]);
  try
    vStream := TFileStream.Create(Name, fmOpenRead or fmShareDenyNone);
    try
      vStream.Position := P^.BytesPos;
      ReallocMem(Pointer(P), SizeOf(TsgSHXRecord) + P^.DefBytes);
      FShapes[N] := P;
      P^.ShapeBytesCount := P^.DefBytes;
      vStream.Read(P^.ShapeBytes[0], P^.ShapeBytesCount);
      P^.ShapeNameLen := UpdateShapeName(Slice(P^.ShapeBytes, P^.ShapeBytesCount));
    finally
      vStream.Free;
    end;
  except
    sgSetSHXFileExists(Name, False);
  end;
end;

procedure TsgSHXFont.ReadUnicodeSHX(N: Word);
begin
  ReadSHX(N);
end;

procedure  TsgSHXFont.ReadNormSHX(N: Word);
begin
  ReadSHX(N);
end;

procedure TsgSHXFont.ReadBigSHX(N: Word);
begin
  ReadSHX(N);
end;

function TsgSHXFont.GetScaledSymbol(Symb: Word; InX, InY: Double; const Scale: TsgSHXPoint;
  const AFontParams: PsgFontProcParams;
  APoly: TsgSHXVertexList; var OutX, OutY: Double): Boolean;
var
  TempPoly: TsgSHXVertexList;
  V: TsgSHXVertex;
  I: integer;
begin
  TempPoly := TsgSHXVertexList.Create;
  try
    Result := GetSymbol(Symb, 0, 0, AFontParams, TempPoly, OutX, OutY);
    if TempPoly.Count > 0 then
    begin
      for I := 0 to TempPoly.Count - 1 do
      begin
        V:=TempPoly[I];
        V.X := InX + V.X * Scale.X;
        V.Y := InY + V.Y * Scale.Y;
        APoly.Add(V);
      end;
      OutX := InX + OutX * Scale.X;
      Outy := InY + OutY * Scale.Y;
    end;
  finally
    TempPoly.Free;
  end;
end;

function TsgSHXFont.GetScaledSymbolI(Index: Integer; InX, InY: Double;
  const Scale: TsgSHXPoint;
  const AFontParams: PsgFontProcParams;
  APoly: TsgSHXVertexList; var OutX, OutY: Double; AAdvance: Double): Boolean;
var
  TempPoly: TsgSHXVertexList;
  V: TsgSHXVertex;
  I: integer;
begin
  TempPoly := TsgSHXVertexList.Create;
  try
    Result := GetSymbolI(Index, 0, 0, AFontParams, TempPoly, OutX, OutY);
    if TempPoly.Count > 0 then
    begin
      for I := 0 to TempPoly.Count - 1 do
      begin
        V := TempPoly[I];
        V.X := InX + V.X * Scale.X;
        V.Y := InY + V.Y * Scale.Y;
        APoly.Add(V);
      end;
      if AAdvance > 0 then
        OutX := InX + AAdvance
      else
        OutX := InX + OutX * Scale.X;
      Outy := InY + OutY * Scale.Y;
    end;
  finally
   TempPoly.Free;
  end;
end;

function TsgSHXFont.GetShapePolyOfSymbol(const AIndex: Integer;
  Vert, Backward: Boolean; var APenDown: Integer; var ARez: Boolean;
  const AStack: PsgSHXStack): PsgSHXPolyData;
var
  I, N: Integer;
  P: PsgSHXRecord;
  V, V0: TsgSHXVertex;
  NoDown: Boolean;
  PTop, PBottom, PLastDown, PFirstDown: Double;

  procedure ProcessBulge(const PV1, PV2: TsgSHXVertex);
  var
    X0, Y0, X1, Y1, X2, Y2, A, A1, BT: Double;
    I, K: Integer;
    SinA, CosA: Extended;
  begin
    if Abs(PV1.Bulge) < 0.5 then
      K := 4
    else
      K := 14;// for future versions
    A := ArcTan(PV1.Bulge) * 2;
    A1 := A / (K+1) * 2;
    SinCos(A1, SinA, CosA);

    BT := (1 / PV1.Bulge - PV1.Bulge)/2;

    X0 := ((PV1.X + PV2.X) - BT * (PV1.Y - PV2.Y)) / 2;
    Y0 := ((PV1.Y + PV2.Y) + BT * (PV1.X - PV2.X)) / 2;

    X1 := PV2.X;
    Y1 := PV2.Y;
    I := 0;
    while I <= K do
    begin
      X2 := X0 + (X1 - X0) * CosA - (Y1 - Y0) * SinA;
      Y2 := Y0 + (X1 - X0) * SinA + (Y1 - Y0) * CosA;
      PTop :=  Max(PTop, Y2);
      PBottom :=  Min(PBottom, Y2);
      PLastDown := Max(PLastDown, X2);
      PFirstDown := Min(PFirstDown, X2);
      X1 := X2;
      Y1 := Y2;
      Inc(I);
    end;
  end;

begin
  ARez := True;
  P := FShapes[AIndex];
  N := AIndex;
  Result := GetSHXPolyData(P, Vert);
  if Result.Poly = nil then
  begin
    Result^.Poly := TsgSHXVertexList.Create;
    case FKind of
      tkNormSHX:
        ReadNormSHX(N);
      tkUnicodeSHX:
        ReadUnicodeSHX(N);
      tkBigSHX:
        ReadBigSHX(N);
    end;
    P := PsgSHXRecord(FShapes[N]);
    Result := GetSHXPolyData(P, Vert);
    Result^.Xoff := 0;
    Result^.Yoff := 0;
    Result^.ScaleOff.X := 1;
    Result^.ScaleOff.Y := 1;
    if  (FKind = tkBigSHX) and FExtndBig then
      ARez := ExtSubSymb(N, Result^.Xoff, Result^.Yoff, Result^.ScaleOff, APenDown, FCharWidth, FCharHeight,Vert, False, Result^.Poly, AStack)
    else
      ARez := SubSymb(N, Result^.Xoff, Result^.Yoff, Result^.ScaleOff, APenDown, Vert, False, Result^.Poly, AStack);
    if not ARez then
      SHXFreePoly(Result^.Poly);
    if ARez and (Result^.Poly.count > 0) then
    begin
      Result^.Poly.Capacity := Result^.Poly.Count;
      PTop := -1E5;
      PBottom := 1E5;
      PLastDown:= -1E5;
      PFirstDown:= 1E5;
      V0 :=  Result^.Poly[0];
      NoDown := True;
      for I := 1 to Result^.Poly.Count -1 do
      begin
        V := Result^.Poly[I];
        if V.PenDown then
        begin
          NoDown := False;
          PLastDown := Max(PLastDown, V0.X);
          PFirstDown := Min(PFirstDown, V0.X);
          PTop :=  Max(PTop, V0.Y);
          PBottom :=  Min(PBottom, V0.Y);
          PLastDown := Max(PLastDown, V.X);
          PFirstDown := Min(PFirstDown, V.X);
          PTop :=  Max(PTop, V.Y);
          PBottom :=  Min(PBottom, V.Y);
          end;
          V0 := V;
      end;
      V0 := Result^.Poly[0];
      for I := 1 to Result^.Poly.Count - 1 do
      begin
        V := Result^.Poly[I];
        if V.PenDown and (V.Bulge <> 00) then
          ProcessBulge(V,V0);
          V0:=V;
      end;
      if NoDown then
      begin
        Result^.FirstDown := 0;
        Result^.LastDown := 0;
        Result^.Top := 0;
        Result^.Bottom := 0;
      end
      else
      begin
        Result^.FirstDown := PFirstDown;
        Result^.LastDown := PLastDown;
        Result^.Top := PTop;
        Result^.Bottom := PBottom;
      end;
    end;
    if (P^.ShapeBytesCount > 0) and not FIsShape then
    begin
      ReallocMem(Pointer(P), SizeOf(TsgSHXRecord));
      FShapes[AIndex] := P;
      P^.ShapeBytesCount := 0;
      P^.ShapeNameLen := 0;
    end;
  end;
//  if Result^.Poly.Count > 0 then
//  begin
//    V := PsgSHXVertex(Result^.Poly.Last);
//    APenDown := Integer(V.PenDown);
//  end;
  if Abs(Result^.Top) > 0.9e5 then
  begin
    Result^.Top := 0;
    Result^.Bottom := 0;
  end;
end;

function TsgSHXFont.GetShapes: TsgList;
begin
  Result := FShapes;
end;

function TsgSHXFont.GetSymbol(Symb: Word; InX, InY: Double;
  const AFontParams: PsgFontProcParams;
  APoly: TsgSHXVertexList; var OutX, OutY: Double): Boolean;
var
  X, Y: Double;
  vPenDown: Integer;
begin
  X := InX;
  Y := InY;
  vPenDown := -1;
  Result := ProcessSymb(Symb, X, Y, AFontParams^.Scale, vPenDown, AFontParams^.Vertical, AFontParams^.Backward, APoly, nil);
  if not Result then Exit;
  UpdateScript(InX, InY, AFontParams^.Scale, AFontParams, APoly, X, Y);
  OutX := X;
  OutY := Y;
end;

function TsgSHXFont.GetSymbolI(Index: Integer; InX, InY: Double;
  const AFontParams: PsgFontProcParams;
  APoly: TsgSHXVertexList; var OutX, OutY: Double): Boolean;
var
  X, Y: Double;
  vPenDown: Integer;
begin
  X := InX;
  Y := InY;
  vPenDown := -1;
  Result := ProcessSymbI(Index, X, Y, AFontParams^.Scale, vPenDown, AFontParams^.Vertical, AFontParams^.Backward, APoly, nil);
  if not Result then Exit;
  UpdateScript(InX, InY, AFontParams^.Scale, AFontParams, APoly, X, Y);
  OutX := X;
  OutY := Y;
end;

function TsgSHXFont.HasPairSymbol(ASym1, ASym2: Byte): Boolean;
begin
  Result := HasPairSymbolWord(Word(ConvertToWideChar(AnsiChar(ASym1), AnsiChar(ASym2))));
end;

function TsgSHXFont.HasPairSymbolWord(ASymbol: Word): Boolean;
begin
  Result := True;//BigFontInRangeWord(ASymbol);
  if not Result then
    Exit;
  Result := FindSymbol(ASymbol) >= 0;
end;

function TsgSHXFont.HasSymbol(const ASymbol: WideChar): Boolean;
var
  vWord: Word absolute ASymbol;
begin
  Result := HasSymbol(vWord);
end;

function TsgSHXFont.HasSymbol(const ASymbol: Word): Boolean;
begin
  Result := FindSymbol(ASymbol) > -1;
end;

function TsgSHXFont.FindSymbol(N: word): integer;
var
  L, H, I,  SH: Integer;
begin
  Result := -1;
  if N = 0 then
    Exit;
  case FKind of
    tkUndefined:
      Exit;
    tkNormSHX:
      begin
        if N in [1..255] then
        begin
          Result := FixedTable[N];
          if Result = 255 then
            Result := -1;
        end;
      end;
  else
    L := 0;
    H := NumChars - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      SH := PsgSHXRecord(FShapes[I]).ShapeNumber;
      if SH < N then
        L := I + 1
      else
      begin
        H := I - 1;
        if SH = N then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
  end;
end;

function TsgSHXFont.Find(N: Word; var Index: Integer): Boolean;
begin
  Index := FindSymbol(N);
  if Index >= FShapes.Count then Index := -1;
  Result := Index >= 0;
end;

function TsgSHXFont.ProcessSymb(N: Word; var InsX, InsY: Double; var Scale: TsgSHXPoint;
  var APenDown: Integer; Vert, Backward: Boolean; APoly: TsgSHXVertexList;
  const AStackGlobal: PsgSHXStack): Boolean;
begin
  Result := ProcessSymbI(FindSymbol(N), InsX, InsY, Scale, APenDown, Vert,
    Backward, APoly, AStackGlobal);
end;

function TsgSHXFont.ProcessSymbI(Index: Integer; var InsX, InsY: Double; var Scale: TsgSHXPoint;
  var APenDown: Integer; Vert, Backward: Boolean; APoly: TsgSHXVertexList;
  const AStackGlobal: PsgSHXStack): Boolean;
var
  I: Integer;
  V: TsgSHXVertex;
  ShapePoly: PsgSHXPolyData;
  vPoly: TsgSHXVertexList;
begin
  Result := False;
  if (Index >= 0) and (Index < FShapes.Count) then
  begin
    ShapePoly := GetShapePolyOfSymbol(Index, Vert, Backward, APenDown, Result, AStackGlobal);
    vPoly := ShapePoly^.Poly;
    if Result and Assigned(vPoly) and (vPoly.Count > 0) then
    begin
      Result := True;
      APoly.Capacity := APoly.Capacity + vPoly.Count;
      for I := 0 to vPoly.Count - 1 do
      begin
        V := vPoly[I];
        V.X := InsX + V.X * Scale.X;
        V.Y := InsY + V.Y * Scale.Y;
        APoly.Add(V);
      end;
      InsX := InsX + ShapePoly^.Xoff * Scale.X;
      InsY := InsY + ShapePoly^.Yoff * Scale.Y;
      Scale.X := Scale.X * ShapePoly^.Scaleoff.X;
      Scale.Y := Scale.Y * ShapePoly^.Scaleoff.Y;
      if FKind = tkBigSHX then
      begin
        if Vert then
          InsX := 0
        else
          InsY := 0;
      end;
    end
    else
      Result := False;
  end;
end;

procedure TsgSHXFont.AddSHXVertex(APoly: TsgSHXVertexList; const X, Y, B: Double;
  const D: Boolean);
begin
  APoly.Add(X, Y, B, D);
end;

procedure TsgSHXFont.MoveCurpos(APoly: TsgSHXVertexList;
  const XOff, YOff: Double; const Scale: TsgSHXPoint; const PenDown: Boolean; var CpX, Cpy: Double);
begin
  CpX := CpX + XOff * Scale.X;
  CpY := CpY + YOff * Scale.Y;
  AddSHXVertex(APoly, CpX, CpY, 0, PenDown);
end;

procedure TsgSHXFont.SetCurpos(APoly: TsgSHXVertexList; var CpX, CpY: Double);
begin
  AddSHXVertex(APoly, CpX, CpY, 0, False);
end;

function TsgSHXFont.UpdateShapeName(const ABytes: array of Byte): Integer;
begin
  Result := 0;
  while (ABytes[Result] <> 0) and (Result < MAX_FONTNAME_LEN) do
    Inc(Result);
end;

procedure TsgSHXFont.SetBulge(APoly: TsgSHXVertexList; const X, Y, B: Double;
  const PenDown: Boolean);
begin
  AddSHXVertex(APoly, X, Y, B, PenDown);
end;

function TsgSHXFont.ExtSubSymb(N: Word; var InsX, InsY: Double; var Scale: TsgSHXPoint;
  var APenDown: Integer; const CharWidth, CharHeight: Byte; Vert, Backward: Boolean; APoly: TsgSHXVertexList;
  const AStackGlobal: PsgSHXStack): Boolean;
begin
  if N < FShapes.Count then
    Result := ExtractSubSymb(N, InsX, InsY, Scale, APenDown, CharWidth, CharHeight, Vert, Backward, APoly, AStackGlobal)
  else
    Result := False;
end;

procedure TsgSHXFont.SaveAs(const FName: string);
var
  I: Integer;
  vStream: TFileStream;
  S: string;
  W, WFirst, WLast: Word;
  P: PsgSHXRecord;
begin
  vStream := TFileStream.Create(FName, fmCreate);
  try
    vStream.Write(Pointer(cnstHeadShapes10)^, Length(cnstHeadShapes11));
    S := #13#10#$1A;
    W := NumChars;
    WFirst := PsgSHXRecord(FShapes[0])^.ShapeNumber;
    WLast := PsgSHXRecord(FShapes[W-1])^.ShapeNumber;
    vStream.Write(S[1], 3);
    vStream.Write(WFirst, SizeOf(Word));
    vStream.Write(WLast, SizeOf(Word));
    vStream.Write(W, SizeOf(NumChars));
    P := nil;
    for I := 0 to NumChars - 1 do
    begin
      P := PsgSHXRecord(FShapes[I]);
      vStream.Write(P^.ShapeNumber, SizeOf(Word));
      vStream.Write(P^.DefBytes , SizeOf(Word));
    end;
    I := 0;
    while I < NumChars do
    begin
      vStream.Write(P^.ShapeBytes[0], P^.ShapeBytesCount);
      Inc(I);
    end;
    S := 'EOF';
    vStream.Write(S[1], 3);
  finally
    vStream.Free;
  end;
end;

function TsgSHXFont.SubSymb(N: Word; var InsX, InsY: Double; var Scale: TsgSHXPoint;
  var APenDown: Integer;
  Vert, Backward: Boolean; APoly: TsgSHXVertexList; const AStackGlobal: PsgSHXStack): Boolean;
var
  vWidth, vHeight: Byte;
begin
  if N < FShapes.Count then
  begin
    vWidth := 0;
    vHeight := 0;
    Result := ExtractSubSymb(N, InsX, InsY, Scale, APenDown, vWidth, vHeight, Vert, Backward, APoly, AStackGlobal);
  end
  else
    Result := False;
end;


procedure TsgSHXFont.UpdateScript(InX, InY: Double; const Scale: TsgSHXPoint;
  const AFontParams: PsgFontProcParams; APoly: TsgSHXVertexList; OutX, OutY: Double);
var
  P: TsgSHXVertex;
begin
  if AFontParams^.Underscore then
  begin
    P.PenDown:= False;
    P.X := InX - (OutX - InX) * cnstXOffsetKoef;
    P.Y :=  - Above * cnstYOffsetKoef;
    P.Bulge := 0;
    APoly.Add(P);
    P.PenDown := True;
    P.X := OutX - (OutX - InX) * cnstXOffsetKoef;
    P.Y := InY - Above * cnstYOffsetKoef;
    P.Bulge := 0;
    APoly.Add(P);
  end;
  if AFontParams^.Overscore then
  begin
    P.PenDown:= False;
    P.X := InX - (OutX - InX) * cnstXOffsetKoef;
    P.Y := InY + Above * (1 + cnstYOffsetKoef);
    P.Bulge := 0;
    APoly.Add(P);
    P.PenDown:= True;
    P.X := OutX - (OutX - InX) * cnstXOffsetKoef;
    P.Y := InY + Above * (1 + cnstYOffsetKoef);
    P.Bulge := 0;
    APoly.Add(P);
  end;
end;

function TsgSHXFont.GetName: string;
begin
  Result := FName;
end;

function TsgSHXFont.Load: Boolean;
var
  vStream: THandleStream;
begin
  Result := False;
  FKind := tkUndefined;
  if not sgSHXFileExists(FName) then Exit;
  try
    vStream := THandleStream.Create(FileOpen(FName, fmOpenRead or fmShareDenyNone));
    if THandle(vStream.Handle) <> THandle(INVALID_HANDLE_VALUE) then
    try
      if GetSHXKind(vStream, FKind) then
      begin
        ClearFixedTable;
        CleareShapes;
        case FKind of
          tkNormSHX:
            ParseNormSHX(vStream);
           tkBigSHX:
             ParseBigSHX(vStream);
           tkUnicodeSHX:
             ParseunicodeSHX(vStream);
        end;
        Result := True;
      end;
    finally
{$IFNDEF SG_NON_WIN_PLATFORM}
      CloseHandle(vStream.Handle);
{$ELSE}
      FileClose(vStream.Handle);
{$ENDIF}
      vStream.Free;
    end;
  except
    FKind := tkUndefined;
    sgSetSHXFileExists(FName, False);
  end;
end;

procedure TsgSHXFont.CleareShapes;
var
  I: Integer;
  P: PsgSHXRecord;
begin
  for I := 0 to FShapes.Count - 1 do
  begin
    P := PsgSHXRecord(FShapes[I]);
    P^.ShapePoly.Poly.Free;
    FreeMem(P);
  end;
  FShapes.Clear;
end;

procedure TsgSHXFont.ClearFixedTable;
begin
  FillChar(FixedTable, SizeOf(FixedTable), $FF);
end;

function TsgSHXFont.ConvertPolyToShape(var ShapeBytes: array of Byte;
  AShapePoly: TsgList): Word;
var
  Index: Integer;
  Sm, Sd, S0, XX, YY: ShortInt;
  Pendown: Boolean;
  I: Integer;
  P: PsgSHXVertex;
  X, Y, DX, DY, Dmax, R: Double;

  procedure Push(V:  Shortint);
  begin
    ShapeBytes[Index]:=Byte(V);
    Inc(Index);
  end;

  procedure CMD(C, V:  Shortint);
  begin
    ShapeBytes[Index]:=Byte(C);
    Inc(Index);
    ShapeBytes[Index]:=Byte(V);
    Inc(Index);
  end;

begin
  P := AShapePoly[0];
  X := P^.X;
  Y := P^.Y;
  Dmax := 0;
  Index := 0;
  for I := 1 to AShapePoly.Count - 1 do
  begin
    P := AShapePoly[I];
    DMax := Max(DMax, Abs(P^.X - X));
    DMax := Max(DMax, Abs(P^.Y - Y));
    X := P^.X;
    Y := P^.Y;
  end;
  if DMax > 127 then
  begin
    S0 := 127;
    DMax := DMax / S0;
    CMD(4, S0);
  end
  else
    S0 := 0;
  R := DMax / 127;
  Sm := ShortInt(Round(R * 100));
  Sd := 100;
  if S0 > 0 then
    R := R * 127;
  CMD(4, Sm);
  CMD(3, Sd);
  P := AShapePoly[0];
  X := P^.X;
  Y := P^.Y;
  Pendown := not P^.PenDown;
  for I := 1 to AShapePoly.Count - 1 do
  begin
    P := AShapePoly[I];
    if Pendown <> P^.PenDown  then
    begin
      Pendown :=  P^.PenDown;
      if Pendown then
        Push(1)
      else
        Push(2);
    end;
    DX :=  P^.X - X;
    DY :=  P^.Y - Y;
    X :=   P^.X;
    Y :=   P^.Y;
    XX := ShortInt(Round( DX / R));
    YY := ShortInt(Round (DY / R));
    if Abs(P^.Bulge) < 0.000001 then
    begin
      Push(8);
      CMD(XX, YY);
    end
    else
    begin
      Push(12);
      CMD(XX, YY);
      Push(ShortInt(Round(P^.Bulge*127)));
    end;
  end;
  Push(0);
  Result := Index;
end;

procedure TsgSHXFont.PrepareShapes(AList: TsgList; FixedSize: Boolean);
var
  I : Integer;
  P: PsgSHXRecord;
begin
  CleareShapes;
  if FixedSize then
    for I := 1 to 255 do
      FixedTable[i] := 255;
  I := 0;
  while I < FNumChars do
  begin
    P := AllocMem(SizeOf(TsgSHXRecord));
    AList.Add(P);
    Inc(I);
  end;
end;

function TsgSHXFontList.GetDefaultBigFontPath: string;
begin
  Result := FDefaultBigFontPath;
end;

function TsgSHXFontList.GetDefaultFont: TsgSHXFont;
begin
  Result := FDefaultFont;
end;

class function TsgSHXFontList.GetFontName(const AName: string): string;
begin
  Result := AName;
end;

function TsgSHXFontList.GetFonts: TStringList;
begin
  Result := FFonts;
end;

function TsgSHXFontList.GetSearchPath: TStringList;
begin
  Result := FSearchPath;
end;

function TsgSHXFontList.GetMissedFonts: TStringList;
begin
  Result := FMissedFonts;
end;
procedure TsgSHXFontList.SetSubstFonts(ASubstFonts: TStrings);
begin
   FSubstFonts.Assign(ASubstFonts);
end;


function TsgSHXFontList.GetStyles: TStringList;
begin
  Result := FStyles;
end;

function TsgSHXFontList.GetDefaultFontPath: string;
begin
  Result := FDefaultFontPath;
end;

function TsgSHXFont.GetAbove: Integer;
begin
  Result := FAbove;
end;

function TsgSHXFont.GetLineFeed: Double;
var
  A, B, W, H, L, BT, R, T, AF: Double;
begin
  Result := FLineFeed;
  if FLineFeed <> 0 then  Exit;
  ProcTextBox(Self,nil, #10, '', '', False,[], A, B, W, H, L, BT, R, T, AF);
  Result := H;
end;

function TsgSHXFont.GetBelow: Integer;
begin
  Result := FBelow;
end;

function TsgSHXFont.GetCodePage: Integer;
begin
  if FCodePage = cnstUndefinedCodePage then
    FCodePage := GetBigFontCodePage(ExtractFileName(FName));
  Result := FCodePage;
end;

function TsgSHXFont.GetNumChars: Word;
begin
  Result := FNumChars;
end;

function TsgSHXFont.GetLoaded: Boolean;
begin
  Result := FKind <> tkUndefined;
end;

procedure TsgSHXFontList.SetDefaultFontPath(const Value: string);
begin
  FDefaultFontPath := Value;
end;

{ TsgTextIterator }

constructor TsgTextIterator.Create(const AString: AnsiString;
  const AWideString: WideString; const AMap: TsgSymbolsMap;
  const AFont, ABigFont: TsgSHXFont);
var
  I, L1, L2: Integer;
begin
  inherited Create;
  FText := AString;
  FWideText := AWideString;
  L1 := Length(FWideText);
  if L1 > 0 then
  begin
    L2 := Length(AMap);
    SetLength(FMapWideText, MaxI(L1, L2));
    for I := Low(FMapWideText) to High(FMapWideText) do
    begin
      FMapWideText[I] := stUndefined;
      if I < L2 then
      begin
        if Integer(AMap[I + 1]) = 2 then
         FMapWideText[I] := stBigFont
        else
         FMapWideText[I] := stNormal;
      end;
    end;
  end;
  FFont := AFont;
  FBigFont := ABigFont;
  if (FBigFont <> nil) and FBigFont.FExtndBig then//change in future version
    FBigFont := nil;
  if (FBigFont <> nil) and (FFont <> nil) then
    if FBigFont.Above <> 0 then
    begin
      FScale.X := FFont.Above / FBigFont.Above;
      FScale.Y := FScale.X;
    end
    else
      if FBigFont.FCharHeight <> 0 then
      begin
        FScale.X := FFont.Above / FBigFont.FCharHeight;
        FScale.Y := FScale.X;
      end;
end;

constructor TsgTextIterator.Create(const AIndices: array of Word;
  const AMap: TsgSymbolsMap; const AFont, ABigFont: TsgSHXFont);
var
  I: Integer;
begin
  if Length(AMap) > 0 then
  begin
    SetLength(FMapWideText, Length(AMap));
    for I := 1 to Length(AMap) do
      FMapWideText[I-1] := TsgSymbolType(AMap[I]);
  end
  else
  begin
    SetLength(FMapWideText, Length(AIndices));
    for I := 0 to High(AIndices) do
      FMapWideText[I] := stNormal;
  end;
  FFont := AFont;
  FBigFont := ABigFont;
  SetLength(FIndices, Length(AIndices));
  if Length(AIndices) > 0 then
    Move(AIndices[0], FIndices[0], SizeOf(AIndices[0]) * Length(AIndices));
  if (FBigFont <> nil) and (FFont <> nil) then
    if FBigFont.Above <> 0 then
    begin
      FScale.X := FFont.Above / FBigFont.Above;
      FScale.Y := FScale.X;
    end
    else
      if FBigFont.FCharHeight <> 0 then
      begin
        FScale.X := FFont.Above / FBigFont.FCharHeight;
        FScale.Y := FScale.X;
      end;
  FUndefinedSymbolIndex := 0;
  if Assigned(FFont) then
  begin
    FUndefinedSymbolIndex := FFont.FindSymbol(Ord(cnstUndefinedSymbol));
    if FUndefinedSymbolIndex = -1 then
      FUndefinedSymbolIndex := FFont.FindSymbol(Ord(' '));
  end;
end;

destructor TsgTextIterator.Destroy;
begin
  FStopped := nil;
  inherited Destroy;
end;

function TsgTextIterator.GetActiveFont(const ASymbol: Word; const AType: TsgSymbolType): TsgSHXFont;
begin
  Result := nil;
  if FBigFont <> nil then
  begin
    if (AType = stNormal) then
    begin
      if FFont.HasSymbol(ASymbol) then
        Result := FFont;
    end
    else
    begin
      if FBigFont.HasSymbol(ASymbol) then// BigFontInRangeWord(ASymbol) and FBigFont.HasPairSymbolWord(ASymbol) then
        Result := FBigFont
      else
      begin
        if FFont.HasSymbol(ASymbol) then
          Result := FFont;
      end;
    end;
  end
  else
    Result := FFont;
end;

function TsgTextIterator.GetModeIterate: Integer;
begin
  if Length(FIndices) > 0 then
    Result := 3
  else
    if Length(FText) > 0 then
    begin
      if FBigFont <> nil then
        Result := 1
      else
        Result := 0;
    end
    else
      Result := 2;
end;

function TsgTextIterator.GetShapeData(AFont: TsgSHXFont; ASymbol: Word;
  const AFontParams: PsgFontProcParams): PsgSHXPolyData;
var
  J: Integer;
  vFontParams: TsgFontProcParams;
begin
  Result := nil;
  J := AFont.FindSymbol(ASymbol);
  if (J < 0) and ((AFont <> FFont) or (ASymbol <> Ord(cnstUndefinedSymbol))) then
  begin
    AFont := FFont;
    ASymbol := Ord(cnstUndefinedSymbol);
    J := AFont.FindSymbol(ASymbol);
  end;
  if J < 0 then
    Exit;
  if Result^.Poly = nil then
  begin
    vFontParams := AFontParams^;
    vFontParams.X := 0;
    vFontParams.Y := 0;
    vFontParams.Poly := TsgSHXVertexList.Create;
    try
      case GetSymbol(AFont, ASymbol, @vFontParams, False, True) of
        0:  Result := nil;
        2:
          begin
            J := AFont.FindSymbol(Ord(cnstUndefinedSymbol));
            if J < 0 then
              Result := nil
            else
              Result := @PsgSHXRecord(AFont.FShapes[J])^.ShapePoly;
          end;
      else
        Result := @PsgSHXRecord(AFont.FShapes[J])^.ShapePoly;
      end;
    finally
      SHXFreePoly(vFontParams.Poly);
    end;
  end
  else
    Result := @PsgSHXRecord(AFont.FShapes[J])^.ShapePoly;
end;

function TsgTextIterator.GetShapeDataI(var AFont: TsgSHXFont; var Index: Integer;
  const AFontParams: PsgFontProcParams; var AScale: TsgSHXPoint): PsgSHXPolyData;
var
  vFontParams: TsgFontProcParams;
begin
  Result := @PsgSHXRecord(AFont.FShapes[Index])^.ShapePoly;
  if Result^.Poly = nil then
  begin
    vFontParams := AFontParams^;
    vFontParams.X := 0;
    vFontParams.Y := 0;
    vFontParams.Poly := TsgSHXVertexList.Create;
    try
      case GetSymbolI(AFont, Index, @vFontParams, False, True) of
        0:  Result := nil;
        2:
          begin
            if AFont = FBigFont then
              AFont := FFont;
            Index := FUndefinedSymbolIndex;
            if Index <= 0 then
              Result := nil
            else
              Result := @PsgSHXRecord(AFont.FShapes[Index])^.ShapePoly;
            AScale.X := 1;
            AScale.Y := 1;
          end;
      end;
    finally
      SHXFreePoly(vFontParams.Poly);
    end;
  end;
end;

function TsgTextIterator.GetSymbol(const AFont: TsgSHXFont; const S: Word;
  const P: PsgFontProcParams; const AScaled: Boolean;
  const AUseUndefined: Boolean): Byte;
begin
  if AScaled and (FScale.X <> 0) then
    Result := Byte(AFont.GetScaledSymbol(S, P^.X, P^.Y, FScale, P, P^.Poly, P^.X, P^.Y))
  else
    Result := Byte(AFont.GetSymbol(S, P^.X, P^.Y, P, P^.Poly, P^.X, P^.Y));
  if AUseUndefined and (Result = 0) and ((AFont <> FFont) or (S <> Ord(cnstUndefinedSymbol)))  then
  begin
    if GetSymbol(FFont, Ord(cnstUndefinedSymbol), P, False, False) <> 0 then
      Result := 2;
  end;
end;

function TsgTextIterator.GetSymbolI(const AFont: TsgSHXFont; Index: Integer;
  const P: PsgFontProcParams; const AScaled, AUseUndefined: Boolean): Byte;
var
  vAdvance: Double;
begin
  if AScaled and (FScale.X <> 0) then
  begin
    if FFont.FModes = 0 then
      vAdvance := AFont.FUnits * FScale.X
    else
      vAdvance := -1;
    Result := Byte(AFont.GetScaledSymbolI(Index, P^.X, P^.Y, FScale, P, P^.Poly, P^.X, P^.Y, vAdvance));
  end
  else
    Result := Byte(AFont.GetSymbolI(Index, P^.X, P^.Y, P, P^.Poly, P^.X, P^.Y));
  if AUseUndefined and (Result = 0) and ((AFont <> FFont) or ((Index <> FUndefinedSymbolIndex) and (FUndefinedSymbolIndex <> -1))) then
  begin
    if GetSymbolI(FFont, FUndefinedSymbolIndex, P, False, False) <> 0 then
      Result := 2;
  end;
end;

function TsgTextIterator.Iterate(const AVertical, ABackward: Boolean;
  const AStyle: TmvFontStyles; APoly: TsgSHXVertexList; var X, Y: Double;
  var ABig: Boolean; const Stopped: PBoolean  = nil): Boolean;
var
  vFontProcParam: TsgFontProcParams;
begin
  FillChar(vFontProcParam, SizeOf(vFontProcParam), 0);
  vFontProcParam.Vertical := AVertical;
  vFontProcParam.Backward := ABackward;
  vFontProcParam.Underscore := fmUnderline in AStyle;
  vFontProcParam.Overscore  := fmStrikeOut in AStyle;
  vFontProcParam.Poly := APoly;
  vFontProcParam.Scale.X := 1;
  vFontProcParam.Scale.Y := 1;
  Result := Iterate(@vFontProcParam, Stopped);
  X := vFontProcParam.X;
  Y := vFontProcParam.Y;
  ABig := vFontProcParam.HasBigSymbol;
end;

function TsgTextIterator.IsStopped: Boolean;
begin
  Result := FStopped^;
end;

function TsgTextIterator.Iterate(const AFontProcParam: PsgFontProcParams;
  const Stopped: PBoolean = nil): Boolean;
begin
  FStopped := Stopped;
  if FStopped = nil then
    FStopped := @cnstNoStopped;
  case GetModeIterate of
    1:  Result := IterateAW(AFontProcParam);
    2:  Result := IterateW(AFontProcParam);
    3:  Result := IterateI(AFontProcParam);
  else
    Result := IterateA(AFontProcParam);
  end;
end;

function TsgTextIterator.IterateA(const AFontProcParam: PsgFontProcParams): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(FText) do
  begin
    if IsStopped then
      Break;
    GetSymbol(FFont, Ord(FText[I]), AFontProcParam, False, True);
  end;
end;

function TsgTextIterator.IterateAW(const AFontProcParam: PsgFontProcParams): Boolean;
var
  I, N, K: Integer;
begin
  Result := True;
  LoadFonts;
  K := Length(FText);
  I := 1;
  while I <= K do
  begin
    if IsStopped then
      Break;
    if (I < K) and FBigFont.HasPairSymbol(Ord(FText[I]), Ord(FText[I+1])) then
    begin
      if I < K then
      begin
         N := Word(ConvertToWideChar(FText[I], FText[I+1]));
         I := I + 2;
         AFontProcParam^.HasBigSymbol := True;
         GetSymbol(FBigFont, N, AFontProcParam, False, True);
      end
      else
      begin
        Inc(I);
        GetSymbol(FFont, Ord(FText[I-1]), AFontProcParam, False, True);
      end;
    end
    else
    begin
      Inc(I);
      GetSymbol(FFont, Ord(FText[I-1]), AFontProcParam, False, True);
    end
  end;
end;

function TsgTextIterator.IterateBox(const AFontProcParam: PsgFontProcParams;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double): Boolean;

  procedure SetActiveFont(const AIndex: Integer; const ASb: TsgSHXPoint;
    var AFont: TsgSHXFont; var ASn: TsgSHXPoint);
  begin
    if (FWideTextTyped[AIndex * 2 - 1] = cnstAnsiChar) or (FBigFont = nil) then
    begin
      AFont := FFont;
    end
    else
    begin
      AFont := FBigFont;
      ASn := ASb;
    end;
//    if Assigned(FWideTextTypedFonts) and (FWideTextTypedFonts.Count > 0) then
//    begin
//      AFont := FWideTextTypedFonts[AIndex];
//      if AFont = nil then
//        AFont := FFont;
//    end;
  end;

var
  I, K: Integer;
  N: Word;
  XX, XXD, DXL, DXM, DXR, SW: Double;
  Sb, Sn: TsgSHXPoint;
  vFont: TsgSHXFont;
  ShapePoly: PsgSHXPolyData;
begin
  Result := False;

  Above := 0;
  Below := 0;
  Width := 0;
  Height := 0;
  RealLeft := 0;
  RealBottom := 0;
  RealRight := 0;
  RealTop := 0;
  After := 0;
  SW := 1;
  if (FText='') and (FWideText='') then
    Exit;
  Result := True;
//  FWideTextTyped := TextToWidestring(FFont, FBigFont, FText, FWideText, FHasBigFontSymbols);
  LoadFonts;
  if FScale.X <> 0 then
    Sb := FScale
  else
  begin
    Sb.X := 1;
    Sb.Y := 1;
  end;

  K := Length(FWideTextTyped) div 2;
  XX := 0;
  XXD := 0;
  if not AFontProcParam^.Vertical then
  begin
    RealBottom := 1e5;
    RealTop := -1e5;

    for I := 1 to K do
    begin
      Sn.X := 1;
      Sn.Y := 1;
      N := Word(FWideTextTyped[I*2]);
      SetActiveFont(I, Sb, vFont, Sn);
      ShapePoly := GetShapeData(vFont, N, AFontProcParam);
      if ShapePoly = nil then
        Continue;
      if (ShapePoly^.Xoff < 0) and (Abs(ShapePoly^.Xoff) > 1E-2) then
      begin
        RealBottom := Min(RealBottom, ShapePoly^.Bottom * Sn.Y);
        if AFontProcParam^.Underscore then
          RealBottom := Min(RealBottom, (-cnstYOffsetKoef*FFont.Above)*Sn.Y);
        RealTop := Max(RealTop, ShapePoly^.Top * Sn.Y);
        if AFontProcParam^.Overscore then
          RealTop := Max(RealTop, FFont.Above * (1 + cnstYOffsetKoef)*Sn.Y);
        {if (P^.Xoff>= 0) then  XX := 1 else}
        SW := -1;
        if (N = 32) and (I = 1) then
        begin
          DXR := Max(0,ShapePoly^.Xoff) * Sn.X;
          DXL := Min(0,ShapePoly^.Xoff) * Sn.X;
          if DXR > 0  then
          begin
             RealLeft := 0;
             RealRight := DXR;
             After := RealRight;
          end
          else
            RealLeft := DXL;
        end
        else
        if (N = 32) and (I = K) then
        begin
          DXR := Max(0,ShapePoly^.Xoff) * Sn.X;
          DXL := Min(0,ShapePoly^.Xoff) * Sn.X;
          if DXR > 0  then
          begin
            RealRight := RealRight + DXR;
            After :=  RealRight;
          end
          else    RealLeft := RealLeft + DXL;
        end
        else  if I = K then
        begin
          DXM := ShapePoly^.Xoff * Sn.X;
          DXR := ShapePoly^.LastDown * Sn.X;
          if DXM>0 then
          begin
            RealRight := RealRight + DXR;
            After := RealRight +Max(DXM,DXR);
          end
          else
          begin
            DXL := ShapePoly^.FirstDown * Sn.X;
            RealLeft := RealLeft + DXL;
          end;
        end
        else
        begin
          DXM := ShapePoly^.Xoff * Sn.X;
          if DXM < 0 then RealLeft := RealLeft + DXM
          else  RealRight := RealRight + DXM;
        end;
      end
      else
      begin
        SW := 1;
        if (N = 32) and ((I = 1) or (I = K)) then
        begin
          DXR := ShapePoly^.Xoff * Sn.X;
          DXL := 0;
          DXM := DXR;
        end
        else
        begin
          DXM := ShapePoly^.LastDown * Sn.X;
          DXR := Max(ShapePoly^.Xoff * Sn.X, DXM);
          DXL := ShapePoly^.FirstDown * Sn.X;
        end;
        if I = 1 then
        begin
          RealLeft := DXL ;
          XX := DXR ;
          XXD := DXR ;
          After := XX;
          RealRight := DXM;
        end
        else if I = K then
        begin
          XX := XX + DXR ;
          XXD := XXD + DXM;
          RealRight := XXD;
          After := XX;
        end
        else
        begin
          XX := XX + DXR;
          XXD := XXD + DXR;
        end;
        RealBottom := Min(RealBottom, ShapePoly^.Bottom * Sn.Y);
        if AFontProcParam^.Underscore then
          RealBottom := Min(RealBottom, (-cnstYOffsetKoef*FFont.Above)*Sn.Y);
        RealTop := Max(RealTop, ShapePoly^.Top * Sn.Y);
        if AFontProcParam^.Overscore then
          RealTop := Max(RealTop, FFont.Above * (1 + cnstYOffsetKoef)*Sn.Y);
      end;
    end;
    if SW<0 then
      Width := (RealRight - RealLeft)*SW
    else
      Width := XXD;
    Height := RealTop - RealBottom;
  end
  else
  begin
    RealLeft := 1e05;
    RealRight := -1e05;
    for I := 1 to K do
    begin
      Sn.X := 1;
      Sn.Y := 1;
      N := Word(FWideTextTyped[I*2]);
      SetActiveFont(I, Sb, vFont, Sn);
      ShapePoly := GetShapeData(vFont, N, AFontProcParam);
      if ShapePoly <> nil then
      begin
        RealLeft := Min(RealLeft, ShapePoly^.FirstDown*Sn.X);
        RealRight := Max(RealRight, ShapePoly^.LastDown*Sn.X);
        RealBottom := RealBottom + ShapePoly^.Yoff * Sn.Y;
      end;
    end;
    Width := RealRight - RealLeft;
    Height := RealTop - RealBottom;
  end;
  SetAboveBellow(FFont, FBigFont, Above, Below);
end;

function TsgTextIterator.IterateBoxI(const AFontProcParam: PsgFontProcParams;
  var Above, Below, Width, Height, RealLeft, RealBottom, RealRight, RealTop,
  After: Double): Boolean;
var
  I, K, vIndex: Integer;
  XX, XXD, DXL, DXM, DXR, SW: Double;
  Sn, Sb: TsgSHXPoint;
  vFont: TsgSHXFont;
  ShapePoly: PsgSHXPolyData;
  vIsSpace: Boolean;
  vAdvance: Double;

  procedure SetActiveFont(AMetaIndex: Integer; const ASb: TsgSHXPoint;
    var AFont: TsgSHXFont; var Index: Integer; var ASn: TsgSHXPoint);
  begin
    vAdvance := -1;
    Index := FIndices[AMetaIndex];
    if FMapWideText[AMetaIndex] = stBigFont then
    begin
      if Assigned(FBigFont) then
      begin
        AFont := FBigFont;
        ASn := ASb;
        if FFont.FModes = 0 then
          vAdvance := AFont.FUnits * ASn.X;
      end
      else
      begin
        AFont := FFont;
        Index := FUndefinedSymbolIndex;
      end;
    end
    else
    begin
      if Index >= FFont.FShapes.Count then
        Index := FUndefinedSymbolIndex;
      AFont := FFont;
    end;
  end;

  function GetAdvance: Double;
  begin
    if vAdvance > 0 then
      Result := vAdvance
    else
      Result := ShapePoly^.Xoff * Sn.X;
  end;

begin
  Result := False;

  Above := 0;
  Below := 0;
  Width := 0;
  Height := 0;
  RealLeft := 0;
  RealBottom := 0;
  RealRight := 0;
  RealTop := 0;
  After := 0;
  SW := 1;
  if Length(FIndices) = 0 then
    Exit;
  Result := True;
//  FWideTextTyped := TextToWidestring(FFont, FBigFont, FText, FWideText, FHasBigFontSymbols);
//  LoadFonts;
  if FScale.X <> 0 then
    Sb := FScale
  else
  begin
    Sb.X := 1;
    Sb.Y := 1;
  end;

  K := High(FIndices);
  XX := 0;
  XXD := 0;
  if not AFontProcParam^.Vertical then
  begin
    RealBottom := 1e5;
    RealTop := -1e5;

    for I := 0 to High(FIndices) do
    begin
      Sn.X := 1;
      Sn.Y := 1;
      SetActiveFont(I, Sb, vFont, vIndex, Sn);
      ShapePoly := GetShapeDataI(vFont, vIndex, AFontProcParam, Sn);
      if ShapePoly = nil then
        Continue;
      vIsSpace := PsgSHXRecord(vFont.Shapes[vIndex]).ShapeNumber = 32;
      if (ShapePoly^.Xoff < 0) and (Abs(ShapePoly^.Xoff) > 1E-2) then
      begin
        RealBottom := Min(RealBottom, ShapePoly^.Bottom * Sn.Y);
        if AFontProcParam^.Underscore then
          RealBottom := Min(RealBottom, (-cnstYOffsetKoef*FFont.Above)*Sn.Y);
        RealTop := Max(RealTop, ShapePoly^.Top * Sn.Y);
        if AFontProcParam^.Overscore then
          RealTop := Max(RealTop, FFont.Above * (1 + cnstYOffsetKoef)*Sn.Y);
        {if (P^.Xoff>= 0) then  XX := 1 else}
        SW := -1;
        if vIsSpace and (I = 0) then
        begin
          DXR := Max(0,ShapePoly^.Xoff) * Sn.X;
          DXL := Min(0,ShapePoly^.Xoff) * Sn.X;
          if DXR > 0  then
          begin
             RealLeft := 0;
             RealRight := DXR;
             After := RealRight;
          end
          else
            RealLeft := DXL;
        end
        else
        if vIsSpace and (I = K) then
        begin
          DXR := Max(0,ShapePoly^.Xoff) * Sn.X;
          DXL := Min(0,ShapePoly^.Xoff) * Sn.X;
          if DXR > 0  then
          begin
            RealRight := RealRight + DXR;
            After :=  RealRight;
          end
          else    RealLeft := RealLeft + DXL;
        end
        else  if I = K then
        begin
          DXM := ShapePoly^.Xoff * Sn.X;
          DXR := ShapePoly^.LastDown * Sn.X;
          if DXM>0 then
          begin
            RealRight := RealRight + DXR;
            After := RealRight +Max(DXM,DXR);
          end
          else
          begin
            DXL := ShapePoly^.FirstDown * Sn.X;
            RealLeft := RealLeft + DXL;
          end;
        end
        else
        begin
          DXM := ShapePoly^.Xoff * Sn.X;
          if DXM < 0 then RealLeft := RealLeft + DXM
          else  RealRight := RealRight + DXM;
        end;
      end
      else
      begin
        SW := 1;
        if vIsSpace and ((I = 0) or (I = K)) then
        begin
          DXR := GetAdvance;
          DXL := 0;
          DXM := DXR;
        end
        else
        begin
          DXM := ShapePoly^.LastDown * Sn.X;
          DXR := GetAdvance;
          DXL := ShapePoly^.FirstDown * Sn.X;
        end;
        if I = 0 then
        begin
          RealLeft := DXL ;
          XX := DXR ;
          XXD := DXR ;
          After := XX;
          RealRight := DXM;
        end
        else if I = K then
        begin
          XX := XX + DXR ;
          XXD := XXD + DXM;
          RealRight := XXD;
          After := XX;
        end
        else
        begin
          XX := XX + DXR;
          XXD := XXD + DXR;
        end;
        RealBottom := Min(RealBottom, ShapePoly^.Bottom * Sn.Y);
        if AFontProcParam^.Underscore then
          RealBottom := Min(RealBottom, (-cnstYOffsetKoef*FFont.Above)*Sn.Y);
        RealTop := Max(RealTop, ShapePoly^.Top * Sn.Y);
        if AFontProcParam^.Overscore then
          RealTop := Max(RealTop, FFont.Above * (1 + cnstYOffsetKoef)*Sn.Y);
      end;
    end;
    if SW<0 then
      Width := (RealRight - RealLeft)*SW
    else
      Width := XXD;
    Height := RealTop - RealBottom;
  end
  else
  begin
    RealLeft := 1e05;
    RealRight := -1e05;
    for I := 0 to High(FIndices) do
    begin
      Sn.X := 1;
      Sn.Y := 1;
      SetActiveFont(FIndices[I], Sb, vFont, vIndex, Sn);
      ShapePoly := GetShapeDataI(vFont, vIndex, AFontProcParam, Sn);
      if ShapePoly <> nil then
      begin
        RealLeft := Min(RealLeft, ShapePoly^.FirstDown*Sn.X);
        RealRight := Max(RealRight, ShapePoly^.LastDown*Sn.X);
        RealBottom := RealBottom + ShapePoly^.Yoff * Sn.Y;
      end;
    end;
    Width := RealRight - RealLeft;
    Height := RealTop - RealBottom;
  end;
  SetAboveBellow(FFont, FBigFont, Above, Below);
end;

function TsgTextIterator.IterateI(
  const AFontProcParam: PsgFontProcParams): Boolean;
var
  I: Integer;
begin
  Result := True;
  SetLength(FWideTextTypedFonts, Length(FIndices));
  LoadFonts;
  for I := 0 to High(FIndices) do
  begin
    if IsStopped then
      Break;
    if (FMapWideText[I] = stBigFont) and Assigned(FBigFont) then
      GetSymbolI(FBigFont, FIndices[I], AFontProcParam, True, True)
    else
      if Assigned(FFont) then
        GetSymbolI(FFont, FIndices[I], AFontProcParam, False, True);
  end;
end;

function TsgTextIterator.IterateBox(const Vertical: Boolean;
  Fstyle: TmvFontStyles; var Above, Below, Width, Height, RealLeft, RealBottom,
  RealRight, RealTop, After: Double): Boolean;
var
  vFontProcParam: TsgFontProcParams;
begin
  FillChar(vFontProcParam, SizeOf(vFontProcParam), 0);
  vFontProcParam.Vertical := Vertical;
  vFontProcParam.Underscore := fmUnderline in FStyle;
  vFontProcParam.Overscore  := fmStrikeOut in FStyle;
  vFontProcParam.Scale.X := 1;
  vFontProcParam.Scale.Y := 1;
  Result := IterateBox(@vFontProcParam, Above, Below, Width, Height, RealLeft,
    RealBottom, RealRight, RealTop, After);
end;

function TsgTextIterator.IterateW(const AFontProcParam: PsgFontProcParams): Boolean;
var
  I: Integer;
  vFont: TsgSHXFont;
  N: Word;
begin
  Result := True;
  LoadFonts;
  for I := 1 to Length(FWideText) do
  begin
    N := Word(FWideText[I]);
    vFont := FWideTextTypedFonts[I];
    if Assigned(vFont) then
    begin
      case GetSymbol(vFont, N, AFontProcParam, vFont.FKind = tkBigSHX, True) of
        1:  AFontProcParam^.HasBigSymbol := AFontProcParam^.HasBigSymbol or (vFont.FKind = tkBigSHX);
        2:  FWideTextTypedFonts[I] := FFont;
      else
        FWideTextTypedFonts[I] := nil;
      end;
    end
    else
      GetSymbol(FFont, Ord(cnstUndefinedSymbol), AFontProcParam, False, False);
  end;
end;

procedure TsgTextIterator.LoadFonts;
var
  I, K: Integer;
  vFont: TsgSHXFont;
  vTextTyped: WideString;
begin
  if Length(FWideTextTypedFonts) > 0 then Exit;
  FWideTextTyped := '';
  case GetModeIterate of
    1://unicode as ansi
      begin
        I := 1;
        K := Length(FText);
        while I < K do
        begin
          if FBigFont.HasPairSymbol(Ord(FText[I]), Ord(FText[I + 1])) then
          begin
            vTextTyped := WideString(cnstWideChar) + ConvertToWideChar(FText[I], FText[I + 1]);
            Inc(I, 2);
          end
          else
          begin
            vTextTyped := cnstAnsiChar + WideString(WideChar(FText[I]));
            Inc(I);
          end;
          FWideTextTyped := FWideTextTyped + vTextTyped;
        end;
      end;
    2://unicode
      begin
        SetLength(FWideTextTypedFonts, Length(FWideText) + 1);
        FWideTextTypedFonts[0] := nil;
        for I := 1 to Length(FWideText) do
        begin
          vFont := GetActiveFont(Word(FWideText[I]), FMapWideText[I - 1]);
          FWideTextTypedFonts[I] := vFont;
          if Assigned(vFont) and (vFont.FKind = tkBigSHX) then
          begin
            vTextTyped := cnstWideChar
          end
          else
            vTextTyped := cnstAnsiChar;
          FWideTextTyped := FWideTextTyped + vTextTyped + FWideText[I];
        end;
      end;
  else//ansi
   for I := 1 to Length(FText) do
     FWideTextTyped := FWideTextTyped + cnstAnsiChar +
       WideString(WideChar(FText[I]));
  end;
end;

{ TsgSHXVertexList }

function TsgSHXVertexList.Add(const X, Y: Double; ABulge: Double; APenDown: Boolean): Integer;
var
  Item: TsgSHXVertex;
begin
  Item.X := X;
  Item.Y := Y;
  Item.Bulge := ABulge;
  Item.PenDown := APenDown;
  Result := AddBase(@Item)
end;

function TsgSHXVertexList.CompareSHXVertex(const A, B: Pointer): Integer;
var
  vDX, vDY, vLen: Extended;
begin
  vDX := PsgSHXVertex(B)^.X - PsgSHXVertex(A)^.X;
  vDY := PsgSHXVertex(B)^.Y - PsgSHXVertex(A)^.Y;
  vLen := Sqr(vDX) + Sqr(vDY);
  if vLen < fExtendedResolution * fExtendedResolution then
    Result := 0
  else
    if vDX < 0 then
      Result := -1
    else
      if vDY < 0 then
        Result := -1
      else
        Result := 1;
end;

function TsgSHXVertexList.Add(const AItem: TsgSHXVertex): Integer;
begin
  Result := AddBase(@AItem)
end;

class function TsgSHXVertexList.GetElementTypeInfo: Pointer;
{$IFNDEF HAS_UNMANAGED_TYPEINFO}
const
  TsgSHXVertex_TypeInfo:  packed record Kind: TTypeKind; Name: string[12]; RecSize: Integer;
    ManagedFldCount: Integer; end = (Kind: tkRecord; Name: 'TsgSHXVertex';
    RecSize: SizeOf(TsgSHXVertex); ManagedFldCount: 0);
{$ENDIF}
begin
  Result := {$IFDEF HAS_UNMANAGED_TYPEINFO}TypeInfo(TsgSHXVertex){$ELSE}@TsgSHXVertex_TypeInfo{$ENDIF};
end;

function TsgSHXVertexList.GetItems(const Index: Integer): TsgSHXVertex;
begin
  GetElement(Index, Result);
end;

function TsgSHXVertexList.GetList: PsgSHXVertexArray;
begin
  Result := PsgSHXVertexArray(GetItemBase(0));
end;

function TsgSHXVertexList.IndexOf(const AItem: TsgSHXVertex): Integer;
begin
  Result := IndexOfBase(@AItem);
end;

function TsgSHXVertexList.Remove(const AItem: TsgSHXVertex): Integer;
begin
  Result := RemoveBase(@AItem);
end;

procedure TsgSHXVertexList.SetDefaultCompareProc(var AProc: TsgObjProcCompare);
begin
  AProc := CompareSHXVertex;
end;

procedure TsgSHXVertexList.SetItems(const Index: Integer;
  const Value: TsgSHXVertex);
begin
  PutElement(Index, Value);
end;

var
  vSHXBytesInstanceRawSize: Integer;

initialization
  cnstNoStopped := False;
  vSHXBytesInstanceRawSize := TsgSHXBytes.InstanceSize;
  if vSHXBytesInstanceRawSize > SHXBytesInstanceRawSize then
    raise Exception.Create('Self test error: "TsgSHXBytes.InstanceSize > SHXBytesInstanceRawSize"');
{$IFDEF SG_FIREMONKEY}
  cnstDirectorySeparator := System.IOUtils.TPath.DirectorySeparatorChar;
{$ELSE}
  cnstDirectorySeparator := PathDelim;
{$ENDIF}
  FileExistsList := nil;
  DirExistsList := nil;
  cnstObjectExist := TObject.Create;

finalization
  ClearObjects(RegisterBigShxFonts);
  FreeAndNil(RegisterBigShxFonts);
  FreeAndNil(FileExistsList);
  FreeAndNil(DirExistsList);
  FreeAndNil(cnstObjectExist);

end.
