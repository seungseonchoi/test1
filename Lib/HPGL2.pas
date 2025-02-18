{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   HPGL files TGrapic class                 }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit HPGL2;
{$INCLUDE SGDXF.inc}

//{$DEFINE SG_HPGL2_DEBUG}

interface

uses
{$IFDEF MSWINDOWS}Windows,{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  {$UNDEF SG_HPGL2_DEBUG}
  FMX.Graphics, FMX.Surfaces, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics, Clipbrd,
{$ENDIF}
  SysUtils, Classes, DXFConv, CADImage, sgBitmap,
  sgFunction, sgConsts, Math, sgLines, sgLists, sgXMLParser
{$IFDEF HAS_FEATURE_ANSISTRINGS}
  , AnsiStrings
{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
  ;

const
  SHPGLImage = 'HPGL/2 image';
  HPGLSteps = 1016;
  HPMaxPens = 256;
  HPTextHeight = 162.28;

  type
  EsgHPGLError = class(Exception);
  TsgHPGLProgressEvent = procedure(Stage: TProgressStage; Done, Count: Integer) of object;

  TsgHPGLReader = class;
  TsgHPGLSymbols = set of AnsiChar;

  TsgHPPen = record
    Color: TColor;
    Width: Double;
  end;

  TsgTextPos = record
    Point: TFPoint;
    PenPos: TFPoint;
    PointX:Double;
  end;

  TsgComponentRasterData = packed record
    HorizontalResolutionForComponent: Word;
    VerticalResolutionForComponent: Word;
    //Number: Word;
    PlanesMajorSpecificationForComponent: Byte;
    ChannelIDForComponent: Byte;
  end;

  TsgConfigureRasterData = packed record
    Format: Byte;
    NumberOfComponents: Byte;
//    ComponentsMajorSpecification: Byte;
//    Reserved: Byte;
    Data: array[0..10] of TsgComponentRasterData;
  end;
  PsgConfigureRasterData = ^TsgConfigureRasterData;

  TpclProc = procedure(Cmd: AnsiChar; Rel: Boolean; Param: Integer; ParamD: Double) of object;
  TpclUnpackProc = procedure(Src,Dst: Pointer);

  TpclReader = class(TList)
  private
    FCompress: Integer;
    FHPGLReader: TsgHPGLReader;
    FMaxLen: Integer;
    FMaxLogPalette: TMaxLogPalette;
    FPaletteEntry: TPaletteEntry;
    FPCLBitmap: TsgPCLBitmap;
    FPCLRasterData: PsgConfigureRasterData;
    FPenPosition: TFPoint;
    FRasterHeight: Integer;
    FRasterWidth: Integer;
    FResolution: Integer;
    FROP: Byte;
    FRotationPage: Integer;
    FSavedPalette: Pointer;
    FScaledRasterHeight: Integer;
    FScaledRasterWidth: Integer;
    FStartGraphicMode: Integer;
    FUseGDIPlus: Boolean;
    FUseHPGLCAP: Boolean;
    //FTransparency: Boolean;
    procedure DoBits(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoCommands(AProc: TpclProc);
    procedure DoDest(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoDeviceFeatureControlCommands(ACommand: AnsiChar;
      ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoFont(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoMisc(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoPage(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoParams(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoPlanes;
    procedure DoPosition(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoRaster(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoROP(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure DoRotate(Sender: TObject);
    procedure DoUnit(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
    procedure EndData;
    procedure PopPalette;
    procedure PushPalette;
    procedure ReadDataV(ACount: Integer);
    procedure ReadDataW(ACount: Integer);
    procedure Reset;
    procedure SimpleColors(AParam: Integer);
    procedure StartData(AMode: Integer);
    function Unpack(ACopy: Boolean; P: Pointer; AProc: TpclUnpackProc): Pointer;
  public
    constructor Create(AHPGLReader: TsgHPGLReader);
    destructor Destroy; override;
    function Ampersand: Boolean;
    function Font: Boolean;
    function Process: Boolean;
  end;

  TsgHPGLConverter = class(TsgDXFConverter)
{$IFDEF SG_HPGL2_DEBUG}
  private
    FFileName: string;
    FStackCommands: TList;
    procedure SaveStackCommand;
{$ENDIF}
  protected
    FHPGLReader: TsgHPGLReader;
    procedure Initialize; override;
    procedure InitLTypes;
    procedure InitStandartStyleByText;
  public
{$IFDEF SG_HPGL2_DEBUG}
    destructor Destroy; override;
{$ENDIF}
    procedure StopLoading; override;
  end;

  TsgHPGLImage = class(TsgVectorImageWithAltWhite)
  private
    FPage: TFPoint;
    FMMWidth: Integer;
    FMMHeight: Integer;
    FUsePSInstr: Boolean;
    FPJLData: string;
    function GetMMHeight: Integer;
    function GetMMWidth: Integer;
    function GetUsePSInstruction: Boolean;
    procedure SetSizes;
    procedure SetUsePSInstruction(const AValue: Boolean);
  protected
    procedure ApplyLineWeightFactor(var ALineWeight: Double); override;
    function CreateConverter: TsgDXFConverter; override;
    procedure DoProgress(AStage: TProgressStage; ADone, ACount: Integer);
    //procedure FillBoxForCoordsCalc(ACoord: Integer; var ABox: TFRect); override;
    function GetImageType: TsgExportFormat; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetExtentsParameters(const ARect: TFRect; const AIs3DExtents: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CommonToInternalUnits(APoint: TFPoint): TFPoint; override;
    function ConvertHPGLStepsToMM(APoint: TFPoint): TFPoint;
    function ConvertMMToHPGLSteps(APoint: TFPoint): TFPoint;
    //function GetEntityColor(AEntity: TsgDXFEntity; AInsert: TsgDXFInsert): TColor; override;
    function InternalToCommonUnits(APoint: TFPoint): TFPoint; override;
    procedure LoadFromStream(S: TStream); override;
    procedure SetDefaultViewModePlotSettings; override;
    function Measurement: TsgMeasurement; override;
    property MMWidth: Integer read GetMMWidth;
    property MMHeight: Integer read GetMMHeight;
    property UsePSInstruction: Boolean read GetUsePSInstruction write SetUsePSInstruction;
    property PJLData: string read FPJLData;
  end;

  TsgHPGLReader = class
  private
    FCommandParameters: TsgDoubleList;
    FPCLReader: TpclReader;
    FDXFConverter: TsgDXFConverter;
    FCurPenNum: Integer;
{$IFNDEF SG_FIREMONKEY}
    FLast: Integer;
{$ENDIF}
    FLType: TsgDXFLineType;
    FDrawingName: string;
    FDoScaling: Boolean;
    FError: Boolean;
    FHPGLCmd: Word;
    FUpdateCarriagePoint: Boolean;
    FLblTerm: string;
    FIsVisibleLbTerm: Boolean;
    FMem: PAnsiChar;
    FOnProgress: TsgHPGLProgressEvent;
    FPage: TFPoint;
    FPenPos: TFPoint;
    FPenPos0: TFPoint;
    FTextPos: TsgTextPos;
    FPenDown: Boolean;
    FPolyBuffer: TsgObjectList;
    FPolyMode: Boolean;
    FSCCom: TFPoint;
    FPos: PAnsiChar;
    FLimit: PAnsiChar;
    FStart: PAnsiChar;
    FPSpace: TsgDXFSection;
    FScaling: Boolean;
    FScale: TFPoint;
    FRelative: Boolean;
    FRotation: Integer;
    FLowerLeftPoint: TFPoint;
    FUpperRight: TFPoint;
    FRTLRes: Integer;
    FRTLScale: Single;
    FRealP1: TFPoint;
    FRealP2: TFPoint;
    FStream: TStream;
    FTerminator: Char;
    FUserP1: TFPoint;
    FUserP2: TFPoint;
    FArcStart: TFPoint;
    FArcEnd: TFPoint;
    FLayout: TsgDXFLayout;
    FLayoutNum: Integer;
    FNextPointUp: Boolean;
    FPolyIndex: Integer;
    FHatchIndex: Integer;
    FShading: Single;
    FTotal: Integer;
    FTextWidth: Single;
    FTextHeight: Single;
    FTextRotation: Single;
    FFrame: Single;
    FSlant: Single;
    FMeasure: Double;
    FFlat: TsgFlatPoly;
    FHatch: TsgFlatHatch;
    FImgEnt: TsgDXFImageEnt;
    FInsert: TsgCADClipInsert;
    FHPPens: array[0..HPMaxPens] of TsgHPPen;
    FLayers: array[0..HPMaxPens] of TsgDXFLayer;
    FUsePSInstr: Boolean;
    FEmpty: Boolean;
    FIsPCL: Boolean;
    FIsSR: Boolean;
    FHAlign: Byte;
    FLabelOrign: Integer;
    FXMLParser: TsgParser;
    FPlotSttings: TsgDXFPlotSettings;
    FInterpretHPGLCmdNRAsNewPageOnRead: Boolean;
    procedure AddArc(ArcAbs: Boolean);
    procedure AddArcThreePoint;
    procedure AddDefineLabelTerm;
    procedure AddCircle;
    procedure AddEntity(AEntity: TsgDXFEntity);
    function AddFlat(AEntity: TsgDXFEntity): Boolean;
    procedure AddHatch;
    function AddImage(ABitmap: TsgBitmap): Double;
    procedure AddLine;
    procedure AddPoly;
    procedure AddPolyline;
    procedure AddRectangle(const ARelative: Boolean);
    procedure AddSolid(const ARelative: Boolean);
    procedure AddSpline(ARelative: Boolean);
    procedure AddText;
    function AddToInsert(AEntity: TsgDXFEntity): Boolean;
    procedure AddVertex(var AEntity: TsgDXFPolyline; var AHatch: TsgCADPolyPolygon;
      const APoint: TFPoint; const APolygonMode, APenUp: Boolean);
    procedure BeginPlot;
    procedure CharSlant;
    function CheckIndexPen(const AIndex: Integer): Boolean;
    procedure ClearPolyBuffer;
    function CommandPoint(const AIndex: Integer): TFPoint;
    procedure CommentHPGL;
    function ConvertPoint(const ARelative: Boolean;
      const vDoPoint: TFPoint; var vDestPoint: TFPoint): TFPoint;
    function CurrentColor(const ADoShading: Boolean): TColor;
    procedure DoAddText(var AText: String);
    procedure DoArc(const ArcAbs: Boolean; const APoint: TFPoint; const AAngle: Double);
    procedure DoProgress(Stage: TProgressStage);
    function Eof: Boolean;
    procedure Wedge(const AIsFill: Boolean);
    procedure FillType;
    procedure FindSymbol(const ASymbols: TsgHPGLSymbols; var ASymbol: AnsiChar);
    procedure FinishInsert;
    procedure FlushHatch;
    procedure FrameAdvance;
    function GetChar: AnsiChar;
    function GetChars: AnsiChar;
    function GetCommandParameters(const ADoScale, ARelative: Boolean): Boolean;
    function GetLayer(const AIndexLayer: Integer): TsgDXFLayer;
    function GetPlotSettings: TsgDXFPlotSettings;
    procedure GetScales;
    function GetStrCmdPar: string;
    function GetDefaultPenColor(const AIndex: Integer): Cardinal;
    procedure InitHPPens;
    procedure InitPlot;
    procedure InputPoints;
    procedure InputWindow;
    function InsertHatch(AEntity: TsgDXFEntity): Boolean;
    procedure LabelOrigin;
    procedure Loads(AEntity: TsgDXFEntity);
    procedure MergeControl;
    procedure MoveChar;
    procedure NewLayout;
    procedure NewPage;
    procedure PlotSize;
    function ProcHPGLCmd(ACmd: Word): Boolean;
    function ProcPJLCmp: Boolean;
    procedure ProcPMCommand;
    function ProcRTLCmd: Boolean;
    procedure RemoveInsert;
    procedure ReturnToSymbol(const ASymbols: TsgHPGLSymbols;
      const ASymbol: AnsiChar);
    procedure Rotate;
    procedure SelectPen;
    procedure SetLineType;
    procedure SetPenColor;
    procedure SetPenWidth;
    procedure SetScale(const AReal1, AReal2, AUser1, AUser2: TFPoint;
      var AScale: TFPoint);
    procedure SimpleColors(AParametrs: Integer);
    procedure TextHeight;
    procedure TextRelative;
    procedure TextRotation;
    function GetHWText: TFPoint;
    function GetScale: Single;
    procedure UpdatePen(ANumberPen: Integer);
    procedure UpdateClipData;
    procedure UpdateTextPos;
  public
    constructor Create(Conv: TsgDXFConverter; S: TStream);
    destructor Destroy; override;
    procedure ReadFile;
  end;

function OrdStr(const S: string): Integer;

var
  CF_HPGL: Word;
  InterpretHPGLCmdNRAsNewPageOnRead: Boolean = False;

implementation

uses
  sgZip;

const
  cnstSpaceText: String = 'W';
  cnstDefFontFamily: string = 'Courier New';   // depends on user agent
  cnstDefFileName: string = 'cour.ttf';
  cnstHPGLExt: array [0..17] of string = ('pcl', 'plt', 'hgl',
    'hg', 'hpg', 'plo', 'hp', 'hpp', 'hp1',
    'hp2', 'hp3', 'hpgl',
    'hpgl2', 'gl', 'gl2', 'prn', 'spl', 'rtl');

type
  TsgDXFConverterAccess = class(TsgDXFConverter);

  TsgDXFPlotSettingsAccess = class(TsgDXFPlotSettings);

  ThpConverter = class(TsgDXFConverter)
  end;

  ThpGroup = class(TsgDXFGroup)
  end;

  PSavedPal = ^TSavedPal;
  TSavedPal = record
    Previous: Pointer;
    Data: TsgPCLBitmap;
    Pal: TMaxLogPalette;
    Pens: array[0..HPMaxPens] of TsgHPPen;
  end;

  TsgDXFTextAccess = class (TsgDXFText);

const
  DefPageWidth: Single = 8128.0;
  DefPageHeight: Single = 10160.0;

function DoNumStr(const S: string): string;
var
  I, Len: Integer;
begin
  Result := '';
  Len := Length(S);
  for I := 1 to Len do
    if CharInSet(S[I], ['1'..'9', '0', '+', '-', '.', ',']) then
      Result := Result + S[I];
end;

function IsHatch(P1, P2: Pointer): Integer;
var
  E: TsgDXFEntity absolute P1;
begin
  Result := -Ord((E is TsgCADPolyPolygon) or (E is TsgDXFImageEnt));
  Inc(Result, Ord(Result=0));
end;

function IsNumber(const S: string): Boolean;
var
  I, Len: Integer;
begin
  Len := Length(S);
  Result := not (Len = 0);
  if Result then
    for I := 1 to Len do
      if not CharInSet(S[I], ['1'..'9', '0', '+', '-', '.', ',']) then
      begin
        Result := False;
        Break;
      end;
end;

function OrdStr(const S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    Result := Result or (Ord(S[I]) shl ((Length(S) - I) * 8));
end;

function Line2Poly(Line: TsgDXFLine): TsgDXFPolyline;
begin
  Result := TsgDXFLWPolyline.Create;
  Result.PolyPoints.AppendArray([MakeFPoint(Line.Point.X,Line.Point.Y),
    MakeFPoint(Line.Point1.X,Line.Point1.Y)]);
  Line.Free;
end;

procedure SetPackSize(const Dst, DstPointer: Pointer;
  const ACheckSrc: Boolean = False);
const
  cnstMaxInt: TsgNativeInt = MaxInt;
var
  vSrcData: TsgNativeInt;
begin
  vSrcData := TsgNativeInt(DstPointer) - TsgNativeInt(Dst) - 4;
  if vSrcData <= cnstMaxInt then
  begin
    if (not ACheckSrc) or (PInteger(Dst)^ < Integer(vSrcData)) then
      PInteger(Dst)^ := Integer(vSrcData);
  end
  else
    raise Exception.Create('Overflow range');
end;

procedure PackRLE(Src,Dst: Pointer);
{$IFNDEF SG_ASSEMBLER32}
var
  vSrcPointer,vDstPointer: {$IFDEF SGFPC}System.{$ENDIF}PByte;
  vSrcCount,vDstCount: Integer;
  vByte: Byte;
begin
  vSrcPointer := {$IFDEF SGFPC}System.{$ENDIF}PByte(Src);
  vDstPointer := {$IFDEF SGFPC}System.{$ENDIF}PByte(Dst);
  Inc(vDstPointer,4);
  vSrcCount := PInteger(vSrcPointer)^ - 1;
  Inc(vSrcPointer,4);
  while vSrcCount > 0 do
  begin
    vDstCount := Ord(vSrcPointer^) + 1;
    Inc(vSrcPointer);
    vByte := vSrcPointer^;
    Inc(vSrcPointer);
    while vDstCount > 0 do
    begin
      vDstPointer^ := vByte;
      Inc(vDstPointer);
      Dec(vDstCount);
    end;
    Dec(vSrcCount,2);
  end;
  SetPackSize(Dst, vDstPointer);
end;
{$ELSE}
asm
            PUSH    EBX
            PUSH    ESI
            PUSH    EDI
            MOV     ESI,EAX
            MOV     EDI,EDX
            ADD     EDI,4
            LODSD
            MOV     EBX,EAX
            DEC     EBX
	    JLE     @@2
@@1:        XOR     EAX,EAX
	    LODSB
	    INC     EAX
	    MOV     ECX,EAX
	    LODSB
	    REP     STOSB
	    SUB     EBX,2
	    JG      @@1
@@2:        SUB     EDI,EDX
            SUB     EDI,4
            MOV     [EDX],EDI
	    POP     EDI
            POP     ESI
	    POP     EBX
end;
{$ENDIF}

procedure PackBits(Src,Dst: Pointer);
{$IFNDEF SG_ASSEMBLER32}
var
  vSrcPointer, vDstPointer: {$IFDEF SGFPC}System.{$ENDIF}PByte;
  vSrcCount, vDstCount: Integer;
  vByte: Byte;
begin
  vSrcPointer := Src;
  vDstPointer := Dst;
  Inc(vDstPointer,4);
  vSrcCount := PInteger(vSrcPointer)^;
  Inc(vSrcPointer,4);
  while vSrcCount > 0 do begin
    vDstCount := Ord(vSrcPointer^);
    Inc(vSrcPointer);
    if vDstCount and $80 = 0 then
    begin
      Inc(vDstCount);
      Dec(vSrcCount,vDstCount);
      if vSrcCount <= 0 then
        Break;
      while vDstCount > 0 do
      begin
        vDstPointer^ := vSrcPointer^;
        Inc(vSrcPointer);
        Inc(vDstPointer);
        Dec(vDstCount);
      end;
    end
    else begin
      Dec(vSrcCount);
      if vSrcCount <= 0 then
        Break;
      if vDstCount = $80 then Continue;
      vDstCount := 257 - vDstCount;
      vByte := vSrcPointer^; Inc(vSrcPointer);
      while vDstCount > 0 do begin
        vDstPointer^ := vByte;
        Inc(vDstPointer);
        Dec(vDstCount);
      end;
    end;
    Dec(vSrcCount);
  end;
  SetPackSize(Dst, vDstPointer);
end;
{$ELSE}
asm
	    PUSH    EBX
            PUSH    ESI
            PUSH    EDI
            MOV     ESI,EAX
	    MOV     EDI,EDX
	    ADD     EDI,4
	    LODSD
	    OR      EAX,EAX
	    JZ      @@4
	    MOV     EBX,EAX
@@1:        XOR     EAX,EAX
	    LODSB
	    OR      AL,AL
	    JS      @@2
	    INC     EAX
	    SUB     EBX,EAX
	    JBE     @@4
	    MOV     ECX,EAX
	    REP     MOVSB
	    JMP     @@3
@@2:        DEC     EBX
	    JZ      @@4
	    CMP     AL,$80
	    JZ      @@1
	    MOV     ECX,257
	    SUB     ECX,EAX
	    LODSB
	    REP     STOSB
@@3:        DEC     EBX
	    JNZ     @@1
@@4:        SUB     EDI,EDX
            SUB     EDI,4
	    MOV     [EDX],EDI
            POP     EDI
	    POP     ESI
            POP     EBX
end;
{$ENDIF}

procedure PackDelta(Src,Dst: Pointer);
{$IFNDEF SG_ASSEMBLER32}
var
  vSrcPointer,vDstPointer: {$IFDEF SGFPC}System.{$ENDIF}PByte;
  vSrcData,vSrcCount,vDstCount,vDstData: Integer;
begin
  vSrcPointer := Src;
  vDstPointer := Dst;
  Inc(vDstPointer,4);
  vSrcCount := PInteger(vSrcPointer)^;
  Inc(vSrcPointer,4);
  while vSrcCount > 0 do
  begin
    vSrcData := Ord(vSrcPointer^);
    Inc(vSrcPointer);
    vDstCount := vSrcData shr 5 + 1;
    Dec(vSrcCount,vDstCount);
    if vSrcCount <= 0 then
      Break;
    vSrcData := vSrcData and 31;
    vDstData := vSrcData;
    if vSrcData = 31 then
    repeat
      vSrcData := Ord(vSrcPointer^);
      Inc(vSrcPointer); Dec(vSrcCount);
      Inc(vDstData,vSrcData);
    until vSrcData <> $FF;
    if vSrcCount <= 0 then
      Break;
    Inc(vDstPointer,vDstData);
    while vDstCount > 0 do
    begin
      vDstPointer^ := vSrcPointer^;
      Inc(vSrcPointer); Inc(vDstPointer);
      Dec(vDstCount);
    end;
    Dec(vSrcCount);
  end;
  SetPackSize(Dst, vDstPointer, True);
end;
{$ELSE}
asm
            PUSH    EBX
	    PUSH    EBP
            PUSH    ESI
            PUSH    EDI
            MOV     ESI,EAX
            MOV     EDI,EDX
            ADD     EDI,4
            LODSD
            MOV     EBX,EAX
@@1:        XOR     EAX,EAX
	    LODSB
	    MOV     ECX,EAX
	    SHR     ECX,5
	    INC     ECX
	    SUB     EBX,ECX
	    JBE     @@4
	    AND     AL,31
	    MOV     EBP,EAX
	    CMP     AL,31
	    JNZ     @@3
@@2:        LODSB
	    DEC     EBX
	    JZ      @@4
	    ADD     EBP,EAX
	    CMP     AL,$FF
	    JZ      @@2
@@3:        ADD     EDI,EBP
	    REP     MOVSB
	    DEC     EBX
	    JNZ     @@1
@@4:        SUB     EDI,EDX
	    SUB     EDI,4
	    CMP	    EDI,[EDX]
	    JBE	    @@5
	    MOV     [EDX],EDI
@@5:	    POP     EDI
            POP     ESI
            POP     EBP
            POP     EBX
end;
{$ENDIF}


(*
// modified delta row - compression mode 9
static int pcl_read_raster_9(hpgs_reader *reader,int data_len)
{
  unsigned char *data = reader->pcl_raster_data[reader->pcl_raster_plane];
  int i_in=0,i=0;

  while (i_in < data_len)
    {
      int count;
      hpgs_bool more_count;
      int offset;
      hpgs_bool more_offset;
      hpgs_bool comp;

      reader->last_byte = hpgs_getc(reader->in);
      if (reader->last_byte == EOF)
	return -1;

      if (++i_in >= data_len) break;

      comp = (reader->last_byte & 0x80) != 0;

      if (comp)
        {
          offset = (reader->last_byte >> 5) & 0x03;
          more_offset = (offset == 0x03);
          count = (reader->last_byte & 0x1f) + 1;
          more_count = (count == 0x20);
        }
      else
        {
          offset = (reader->last_byte >> 3) & 0x0f;
          more_offset = (offset == 0x0f);
          count = (reader->last_byte & 0x07) + 1;
          more_count = (count == 0x08);
        }

      while (more_offset && i_in < data_len)
        {
          reader->last_byte = hpgs_getc(reader->in);
          if (reader->last_byte == EOF)
            return -1;

          offset += reader->last_byte;
          more_offset = (reader->last_byte == 0xff);

          if (++i_in >= data_len) return 0;
        }

      while (more_count && i_in < data_len)
        {
          reader->last_byte = hpgs_getc(reader->in);
          if (reader->last_byte == EOF)
            return -1;

          count += reader->last_byte;
          more_count = (reader->last_byte == 0xff);

          if (++i_in >= data_len) return 0;
        }

      i += offset;

      if (i >= reader->pcl_raster_data_size) break;

      if (comp)
        {
          // run-length encoded replacement.
          int i_rep = 0;

          while (i_rep < count)
            {
              reader->last_byte = hpgs_getc(reader->in);
              if (reader->last_byte == EOF)
                return -1;

              if (++i_in >= data_len) return 0;

              int rep_cnt =  reader->last_byte+1;

              reader->last_byte = hpgs_getc(reader->in);
              if (reader->last_byte == EOF)
                return -1;

              ++i_in;

              int rep_val =  reader->last_byte;

              while (rep_cnt-- > 0 && i_rep < count)
                {
                  if (i < reader->pcl_raster_data_size)
                    data[i] = rep_val;

                  ++i;
                  ++i_rep;
                }
            }
        }
      else
        {
          // uncompressed replacement.
          while (count > 0 && i_in < data_len)
            {
              reader->last_byte = hpgs_getc(reader->in);
              if (reader->last_byte == EOF)
                return -1;

              if (i < reader->pcl_raster_data_size)
                data[i] = reader->last_byte;

              ++i_in;
              ++i;
              --count;
            }
        }
    }

  return 0;
}
*)

procedure PackCompDelta(Src,Dst: Pointer);
{$IFNDEF SG_ASSEMBLER32}
var
  vSrcPointer,vDstPointer: {$IFDEF SGFPC}System.{$ENDIF}PByte;
  vSrcData,vSrcCount: Integer;
  vComp, vMoreCount, vMoreOffset: Boolean;
  vOffset, vCount, I: Integer;
  // run-length encoded replacement.
  vIRep : Integer;
  vRepCnt : Integer;
  vRepVal : Integer;
  function vDec(var AInt: Integer): Integer;
  begin
    Dec(AInt);
    Result := AInt;
  end;
  function GetData(var APinter: {$IFDEF SGFPC}System.{$ENDIF}PByte): Integer;
  begin
    Result := Ord(vSrcPointer^);
    Inc(vSrcPointer);
    Dec(vSrcCount);
  end;
begin
  vSrcPointer := Src;
  vDstPointer := Dst;
  Inc(vDstPointer,4);
  vSrcCount := PInteger(vSrcPointer)^;
  Inc(vSrcPointer,4);
  I := 0;
  while vSrcCount > 0 do
  begin
    vSrcData := GetData(vSrcPointer);
    vComp := (vSrcData and $80) <> 0;
    if vComp then
    begin
      vOffset := (vSrcData shr 5) and $03;
      vMoreOffset := vOffset = $03;
      vCount := (vSrcData and $1F) + 1;
      vMoreCount := vCount = $20;
    end
    else
    begin
      vOffset := (vSrcData shr 3) and $0F;
      vMoreOffset := vOffset = $0F;
      vCount := (vSrcData and $07) + 1;
      vMoreCount := vCount = $08;
    end;

    while vMoreOffset and (vSrcCount > 0) do
    begin
      vSrcData := GetData(vSrcPointer);
      vOffset := vOffset + vSrcData;
      vMoreOffset := vSrcData = $FF;
    end;

    while vMoreCount and  (vSrcCount > 0) do
    begin
      vSrcData := GetData(vSrcPointer);
      vCount := vCount + vSrcData;
      vMoreCount := vSrcData = $FF;
    end;

    Inc(I, vOffset);
    if I > $8000 then
      Break;

    if vComp then
    begin
      // run-length encoded replacement.
      vIRep := 0;
      while vIRep < vCount do
      begin
        vSrcData := GetData(vSrcPointer);

        vRepCnt := vSrcData+1;

        vSrcData := GetData(vSrcPointer);

        vRepVal := vSrcData;

        while (vDec(vRepCnt) > 0) and  (vIRep < vCount) do
        begin
          vDstPointer^ := vRepVal;
          Inc(vDstPointer);
          Inc(vIRep);
        end;
      end;
    end
    else
    begin
          // uncompressed replacement.
          while (vCount > 0) and (vSrcCount > 0) do
          begin
            vSrcData := GetData(vSrcPointer);
            vDstPointer^ := vSrcData;
            Inc(vSrcPointer);
            Inc(vDstPointer);
            Dec(vCount);
          end;
    end;
  end;
  SetPackSize(Dst, vDstPointer, True);
end;
{$ELSE}
asm
	JMP	@@6
@@1:	PUSH	EDX
	XOR	EDX,EDX
        MOV	DL,AH
        MOV	AH,0
        AND	ECX,EAX
        AND	EBP,EDX
        CMP	EBP,EDX
        MOV	DL,AL
        JNZ	@@3
@@2:	LODSB
	DEC	EBX
        JZ	@@5
        ADD	EBP,EAX
        CMP	AL,$FF
        JZ	@@2
@@3:	CMP	ECX,EDX
	JNZ	@@5
@@4:	LODSB
	DEC	EBX
        JZ	@@5
        ADD	ECX,EAX
        CMP	AL,$FF
        JZ	@@4
@@5:	ADD	EDI,EBP
	INC	ECX
        SUB	EBX,ECX
        POP	EDX
        RET
@@6:	PUSH	EBX
        PUSH    EBP
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        ADD     EDI,4
        LODSD
        MOV     EBX,EAX
@@7:	XOR	EAX,EAX
	LODSB
        MOV	ECX,EAX
        MOV	EBP,EAX
        OR	AL,AL
        JS	@@8
        SHR	EBP,3
        MOV	AX,$F07
        CALL	@@1
        JLE	@@12
        REP	MOVSB
        JMP	@@11
@@8:	SHR	EBP,5
	MOV	AX,$31F
        CALL	@@1
        JLE	@@12
        PUSH	EBX
        MOV	EBX,ECX
	XOR	EAX,EAX
@@9:	LODSB
	DEC	EBX
        JZ	@@10
        MOV	ECX,EAX
        INC	ECX
        LODSB
        REP	STOSB
        DEC	EBX
        JNZ	@@9
@@10:   POP	EBX
@@11:	DEC	EBX
	JG	@@7
@@12:   SUB     EDI,EDX
        SUB     EDI,4
        CMP     EDI,[EDX]
        JBE     @@13
        MOV     [EDX],EDI
@@13:   POP     EDI
        POP     ESI
        POP     EBP
        POP     EBX
end;
{$ENDIF}

procedure GetNext(P: PAnsiChar; var APixel: Integer); register; {$IFDEF SG_ASSEMBLER}assembler;{$ENDIF}
{$IFNDEF SG_ASSEMBLER}
begin
  APixel := PWord(P)^;
  Inc(P, 2);
  APixel := APixel + Ord(P^) shl 16;
{$ELSE}
asm
  {$IFDEF SG_CPUX64}
    mov ax,[P]
    mov word ptr [APixel],ax
    mov al,byte ptr [P + 2]
    mov byte ptr [APixel + 2],al
  {$ELSE}
    mov cx,[P]
    mov word ptr [APixel],cx
    mov cl,byte ptr [P + 2]
    mov byte ptr [APixel + 2],cl
  {$ENDIF}
{$ENDIF}
end;

procedure DoPixel(var APDst: PAnsiChar; APixel: Integer);
{$IFNDEF SG_ASSEMBLER}
var
  vPixelData: PAnsiChar;
begin
  vPixelData := @APixel;
  PWord(APDst)^ := PWord(vPixelData)^;
  Inc(APDst,2); Inc(vPixelData,2);
  APDst^ := vPixelData^;
  Inc(APDst);
{$ELSE}
asm
  {$IFDEF SG_CPUX64}
    mov rax,[rcx]
    mov [rax],dx
    shr edx,8
    mov byte ptr [rax + 2],dh
    add [rcx],3
  {$ELSE}
    mov ecx,[eax]
    mov [ecx],dx
    shr edx,8
    mov byte ptr [ecx + 2],dh
    add dword ptr [eax],3
  {$ENDIF}
{$ENDIF}
end;

procedure PackNLL(Src,Dst: Pointer);
var
  PSrc: PAnsiChar;
  PDst,Limit: PAnsiChar;
  Count,Offset,Pixel,Last: Integer;
  Cmd: Byte;

  function GetByte: Byte;
  begin
    Result := Byte(PSrc^);
    Inc(PSrc);
  end;

  function Additional: Integer;
  var N: Byte;
  begin
    Result := 0;
    repeat
      N := GetByte;
      Inc(Result,N);
    until N <> 255;
  end;

  function Beyond(I: Integer): Boolean;
  begin
    Result := PDst - PAnsiChar(Dst) + I*3 >= $8000;
  end;

  function ASMBSwap(const value: Integer): Integer;
  begin
    PByteArray(@Result)^[0] := PByteArray(@value)^[3];
    PByteArray(@Result)^[1] := PByteArray(@value)^[2];
    PByteArray(@Result)^[2] := PByteArray(@value)^[1];
    PByteArray(@Result)^[3] := PByteArray(@value)^[0];
  end;

  procedure GetPixel(var ASrc, ADst; var APixel: Integer);
{$IFNDEF SG_ASSEMBLER}
  type
    PPix = ^TPix;
    TPix = packed record
      case Integer of
        0:(Color: Integer;);
        1:(W0,W2: SmallInt;);
        2:(B0,B1,B2,B3: Byte;);
    end;
  var
    vSrc: Pointer;
    vDst: PByteArray;
    vPix: TPix;
    vPixOut: PPix;
  begin
    vPixOut := PPix(@APixel);
    vSrc := Pointer(ASrc);
    vPix.B0 := PByte(vSrc)^;
    if vPix.B0 and $80 = 0 then
    begin
      vPix.Color := PInteger(vSrc)^;
      vPix.Color := ASMBSwap(vPix.Color);
      vPix.B0 := 0;
      vPix.Color := vPix.Color shr 7;
      vPix.Color := ASMBSwap(vPix.Color);
      vPix.Color := vPix.Color shr 8;
      Inc(PByte(vSrc), 3);
      vPixOut^.Color := vPix.Color;
      Pointer(ASrc) := vSrc;
      vPix.B0 := vPixOut^.B0 xor vPixOut^.B1;
      if vPix.B0 and $FE <> 0 then
      begin
        vPix.B0 := vPixOut^.B2 shr 7;
        vPixOut^.B2 := vPixOut^.B2 or vPix.B0;
      end
      else
      begin
        vPix.B0 := vPixOut^.B2 xor vPixOut^.B0;
        if vPix.B0 and $FE <> 0 then
        begin
          vPix.B0 := vPixOut^.B2 shr 7;
          vPixOut^.B2 := vPixOut^.B2 or vPix.B0;
        end
        else
        begin
          vPix.B0 := vPixOut^.B1 and 1;
          vPixOut^.B2 := vPixOut^.B2 or vPix.B0;
        end;
      end;
    end
    else
    begin
      vPix.W0 := PWord(vSrc)^;
      vPix.Color := Swap(vPix.Color);
      vPix.W0 := vPix.W0 + vPix.W0;
      vPix.Color := vPix.W0;
      vPix.Color := vPix.Color shl 5;
      vPix.W0 := SAR16(vPix.W0, 3);
      vPix.B0 := SAR8(vPix.B0, 2);
      Inc(PByte(vSrc), 2);
      vPix.Color := ASMBSwap(vPix.Color);
      vPix.Color := vPix.Color shr 8;
      vPixOut^.Color := vPix.Color;
      Pointer(ASrc) := vSrc;
      vDst := PByteArray(ADst);
      vPixOut^.B0 := vPixOut^.B0 + vDst^[0];
      vPixOut^.B1 := vPixOut^.B1 + vDst^[1];
      vPixOut^.B2 := vPixOut^.B2 + vDst^[2];
    end;
  end;
{$ELSE}
  asm
  {$IFDEF SG_CPUX64}
      PUSH RBX
      PUSH RSI
      MOV RSI,ASrc
      MOV   RBX,[ASrc]
      MOV   AL,[RBX]
      OR    AL,AL
      JS    @@3
      MOV   EAX,[RBX]
      BSWAP EAX
      MOV   AL,0
      SHR   EAX,7
      BSWAP EAX
      SHR   EAX,8
      ADD   RBX,3
      MOV   [APixel],EAX
      MOV   [RSI],RBX
      MOV   AL,BYTE PTR [APixel]
      XOR   AL,BYTE PTR [APixel+1]
      TEST  AL,-2
      JNZ   @@1
      MOV   AL,BYTE PTR [APixel+2]
      XOR   AL,BYTE PTR [APixel]
      TEST  AL,-2
      JNZ   @@1
      MOV   AL,BYTE PTR [APixel+1]
      AND   AL,1
      JMP   @@2
@@1:  MOV   AL,BYTE PTR [APixel+2]
      SHR   AL,7
@@2:  OR    BYTE PTR [APixel+2],AL
      JMP   @@4
@@3:  MOV   AX,[RBX]
      XCHG  AL,AH
      ADD   AX,AX
      MOVSX EAX,AX
      SHL   EAX,5
      SAR   AX,3
      SAR   AL,2
      ADD   RBX,2
      BSWAP EAX
      SHR   EAX,8
      MOV   [APixel],EAX
      MOV   [RSI],RBX
      MOV   RBX,[ADst]
      MOV   AL,[RBX]
      ADD   BYTE PTR [APixel],AL
      MOV   AL,[RBX+1]
      ADD   BYTE PTR [APixel+1],AL
      MOV   AL,[RBX+2]
      ADD   BYTE PTR [APixel+2],AL
    @@4:
      POP RSI
      POP RBX
  {$ELSE}
      PUSH EBX
      PUSH ESI
      MOV ESI,ASrc
      MOV   EBX,[ASrc]
      MOV   AL,[EBX]
      OR    AL,AL
      JS    @@3
      MOV   EAX,[EBX]
      BSWAP EAX
      MOV   AL,0
      SHR   EAX,7
      BSWAP EAX
      SHR   EAX,8
      ADD   EBX,3
      MOV   [APixel],EAX
      MOV   [ESI],EBX
      MOV   AL,BYTE PTR [APixel]
      XOR   AL,BYTE PTR [APixel+1]
      TEST  AL,-2
      JNZ   @@1
      MOV   AL,BYTE PTR [APixel+2]
      XOR   AL,BYTE PTR [APixel]
      TEST  AL,-2
      JNZ   @@1
      MOV   AL,BYTE PTR [APixel+1]
      AND   AL,1
      JMP   @@2
@@1:  MOV   AL,BYTE PTR [APixel+2]
      SHR   AL,7
@@2:  OR    BYTE PTR [APixel+2],AL
      JMP   @@4
@@3:  MOV   AX,[EBX]
      XCHG  AL,AH
      ADD   AX,AX
      MOVSX EAX,AX
      SHL   EAX,5
      SAR   AX,3
      SAR   AL,2
      ADD   EBX,2
      BSWAP EAX
      SHR   EAX,8
      MOV   [APixel],EAX
      MOV   [ESI],EBX
      MOV   EBX,[ADst]
      MOV   AL,[EBX]
      ADD   BYTE PTR [APixel],AL
      MOV   AL,[1 + EBX]
      ADD   BYTE PTR [APixel+1],AL
      MOV   AL,[2 + EBX]
      ADD   BYTE PTR [APixel+2],AL
    @@4:
      POP ESI
      POP EBX
    {$ENDIF}
  end;
{$ENDIF}

  procedure DoPixels;
  begin
    if Beyond(Count) then Exit;
    while Count > 0 do
    begin
      if Cmd and $80 = 0 then
        GetPixel(PSrc, PDst, Pixel);
      DoPixel(PDst, Pixel);
      Dec(Count);
    end;
  end;

begin
  PDst := PAnsiChar(Dst);
  Inc(PDst, 4);
  PSrc := Src;
  Limit := PSrc;
  Inc(Limit, PInteger(Src)^ + 4);
  Inc(PSrc, 4);
  Last := $FFFFFF;
  while PSrc < Limit do
  begin
    Cmd := GetByte;
    Offset := Cmd and $18 shr 3;
    if Offset = 3 then
      Offset := Additional + 3;
    if Beyond(Offset) or (PSrc > Limit) then
      Break;
    Inc(PDst, Offset * 3);
    case Cmd and $60 of
      $00: GetPixel(PSrc, PDst, Pixel);
      $20: GetNext(PDst - 3, Pixel);
      $40: GetNext(PDst + 3, Pixel);
      $60: Pixel := Last;
    end;
    if Cmd and $60 = 0 then
      Last := Pixel;
    DoPixel(PDst, Pixel);
    Count := Cmd and 7;
    if Cmd and $87 = $87 then
      Count := Additional + 7;
    if Cmd >= $80 then
      Inc(Count);
    DoPixels;
    if Cmd and $87 = 7 then
    repeat
      Count := GetByte;
      Offset := Count;
      DoPixels;
    until Offset < 255;
    if PSrc >= Limit then
      Break;
  end;
  SetPackSize(Dst, PDst, True);
end;

procedure PackEmpty(Src,Dst: Pointer);
begin//FI:W519 ignore
end;

function AppendImage(AImageEnt1, AImageEnt2: TsgDXFImageEnt): Boolean;
begin
  Result := False;
end;
//function AppendImage(AImageEnt1, AImageEnt2: TsgDXFImageEnt): Boolean;
//
//  function Area(const ARect: TFRect): Double;
//  begin
//    Result := (ARect.Right - ARect.Left) * (ARect.Top - ARect.Bottom);
//  end;
//
//  procedure Draw(ABitmapDraw: TBitmap; AImageEnt: TsgDXFImageEnt; const ARect: TFRect);
//  var
//    vBitmapRect: TRect;
//  begin
//    vBitmapRect.Left := Round(ABitmapDraw.Width * (ARect.Left - ARect.Left) / (ARect.Right - ARect.Left));
//    vBitmapRect.Top := Round(ABitmapDraw.Height * (ARect.Top - ARect.Top) / (ARect.Top - ARect.Bottom));
//    vBitmapRect.Right := Round(ABitmapDraw.Width * (ARect.Right - ARect.Left) / (ARect.Right - ARect.Left));
//    vBitmapRect.Bottom := Round(ABitmapDraw.Height * (ARect.Top - ARect.Bottom) / (ARect.Top - ARect.Bottom));
//    ABitmapDraw.Canvas.StretchDraw(vBitmapRect, AImageEnt.Picture.Graphic);
//  end;
//
//var
//  vRect0, vRect1, vRect2, vRect3: TFRect;
//  vWidth1, vWidth2, vHeight1, vHeight2: Integer;
//  vArea: Double;
//  vBitmap: TBitmap;
//begin
//  Result := False;
//  vRect0 := AImageEnt1.Box;
//  vRect1.TopLeft := AImageEnt2.Point;
//  vRect1.BottomRight := AImageEnt2.Point;
//  ExpandFRect(vRect1, MakeFPoint(AImageEnt2.Point.X + AImageEnt2.Point1.X * AImageEnt2.Point3.X, AImageEnt2.Point.Y, 0));
//  ExpandFRect(vRect1, MakeFPoint(AImageEnt2.Point.X, AImageEnt2.Point.Y + AImageEnt2.Point2.Y * AImageEnt2.Point3.Y, 0));
//  vRect2 := vRect0;
//  UnionFRect(vRect2, vRect1);
//  vRect3 := vRect0;
//  if vRect3.Left < vRect1.Left then
//    vRect3.Left := vRect1.Left;
//  if vRect3.Right > vRect1.Right then
//    vRect3.Right := vRect1.Right;
//  if vRect3.Top > vRect1.Top then
//    vRect3.Top := vRect1.Top;
//  if vRect3.Bottom < vRect1.Bottom then
//    vRect3.Bottom := vRect1.Bottom;
//  vArea := Area(vRect3);
//  if vArea < 0 then
//    vArea := 0;
//  Result := (Area(vRect0) + Area(vRect1) - vArea) / Area(vRect2) > 0.9;
//  if not Result then
//    Exit;
//  AImageEnt1.Point := MakeFPoint(vRect2.Left, vRect2.Bottom, 0);
//  AImageEnt1.UVector := MakeFPoint(vRect2.Right, vRect2.Bottom, 0);
//  AImageEnt1.VVector := MakeFPoint(vRect2.Left, vRect2.Top, 0);
//  AImageEnt1.Size := MakeFPoint(vRect2.Right, vRect2.Top, 0);
//  vWidth1 := Round(AImageEnt1.Picture.Width * (vRect2.Right - vRect2.Left) / (vRect0.Right - vRect0.Left));
//  vWidth2 := Round(AImageEnt2.Picture.Width * (vRect2.Right - vRect2.Left) / (vRect1.Right - vRect1.Left));
//  vHeight1 := Round(AImageEnt1.Picture.Height * (vRect2.Top - vRect2.Bottom) / (vRect0.Top - vRect0.Bottom));
//  vHeight2 := Round(AImageEnt2.Picture.Height * (vRect2.Top - vRect2.Bottom) / (vRect1.Top - vRect1.Bottom));
//  if vWidth1 < vWidth2 then
//    vWidth1 := vWidth2;
//  if vHeight1 < vHeight2 then
//    vHeight1 := vHeight2;
//  vBitmap := TBitmap.Create;
//  try
//    vBitmap.Width := vWidth1;
//    vBitmap.Height := vHeight1;
//    Draw(vBitmap, AImageEnt1, vRect0);
//    Draw(vBitmap, AImageEnt2, vRect1);
//    AImageEnt1.SetImage(vBitmap);
//  finally
//    {$IFNDEF SGFPC}
//    FreeMemoryContexts;
//    {$ENDIF}
//    vBitmap.Free;
//  end;
//end;

{ TpclReader }

constructor TpclReader.Create(AHPGLReader: TsgHPGLReader);
begin
  inherited Create;
  FHPGLReader := AHPGLReader;
  FResolution := 75;
  FROP := 252;
  FRotationPage := -1;
  FPCLBitmap.Encode  := 1;//  FData.Encode  := 0; { ###Encode=0 & BitsNdx=8 }
  FPCLBitmap.BitIndex := 1;//  FData.BitsNdx := 8;
  FMaxLogPalette.palVersion := $300;
  FMaxLogPalette.palPalEntry[0].peRed := $FF;
  FMaxLogPalette.palPalEntry[0].peGreen := $FF;
  FMaxLogPalette.palPalEntry[0].peBlue := $FF;
  //FTransparency := False;
end;

destructor TpclReader.Destroy;
begin
  Reset;
  while FSavedPalette <> nil do
    PopPalette;
  inherited Destroy;
end;

function TpclReader.Ampersand: Boolean;
begin
  Result := True;
  case FHPGLReader.GetChar of
    'a':
    begin
      DoCommands(DoDeviceFeatureControlCommands);
    end;
    'u': DoCommands(DoUnit);
    'l': DoCommands(DoPage);
  else
    Result := False;
  end;
end;

function TpclReader.Font: Boolean;
begin
  Result := True;
  case FHPGLReader.GetChar of
    's': DoCommands(DoFont);
  else
    Result := False;
  end;
end;

function TpclReader.Process: Boolean;
begin
  Result := True;
  case FHPGLReader.GetChar of
    'p': DoCommands(DoPosition);
    'r': DoCommands(DoMisc);
    'b': DoCommands(DoBits);
    'v': DoCommands(DoParams);
    't': DoCommands(DoDest);
    'l': DoCommands(DoROP);
    'g': DoCommands(DoRaster);
    'c': DoCommands(DoFont);
  else
    Result := False;
  end;
end;

procedure TpclReader.DoBits(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);

  procedure Skip;
  begin
    while AParam > 0 do
    begin
      Add(nil);
      Dec(AParam);
    end;
  end;

begin
  case ACommand of
    'M': FCompress := AParam;
    'Y': Skip;
    'V': ReadDataV(AParam);
    'W': ReadDataW(AParam);
  end;
end;

procedure TpclReader.DoCommands(AProc: TpclProc);
var
  vStr: string;
  vChar: AnsiChar;
begin
  {$IFNDEF SGDEL_10_SEATTLE}
  vChar := #0;
  {$ENDIF}
  repeat
    vStr := '';
    while True do
    begin
      vChar := FHPGLReader.GetChar;
      if vChar in ['A'..'Z','a'..'z'] then
        Break;
      if FHPGLReader.FPos >= FHPGLReader.FLimit then
        Exit;
      vStr := vStr + Char(vChar);
    end;
    AProc(UpCase(vChar), (vStr <> '') and CharInSet(vStr[1], ['+','-']), StrToIntDef(vStr, 0),
      StrToDouble(vStr, '.'));
  until vChar in ['A'..'Z'];
end;

procedure TpclReader.DoDest(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
begin
  case ACommand of
    'H': FScaledRasterWidth := AParam;
    'V': FScaledRasterHeight := AParam;
    'R': if AParam <> 0 then
         begin
           FHPGLReader.FRTLRes := AParam;
           //FOwner.FScaling := True;
           //FOwner.FScale.X := HPGLSteps / FOwner.FRTLRes;
           //FOwner.FScale.Y := HPGLSteps / FOwner.FRTLRes;
           //FOwner.FRTLScale := HPGLSteps / FOwner.FRTLRes;
         end;
  end;
end;

procedure TpclReader.DoDeviceFeatureControlCommands(ACommand: AnsiChar;
  ARel: Boolean; AParam: Integer; AParamD: Double);
begin
  case ACommand of
    'H':; // moves the CAP horizontally by the specified number
         // of decipoints (1 decipoint = 1/720 inch).
    'N':; // Negative Motion
  end;
end;

procedure TpclReader.DoFont(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
//var
//  vFontId: Integer;
//  vX, vY: Double;
//  vFrameAnchorPoint: Integer;
begin
  case ACommand of
    'W':
      begin
        Inc(FHPGLReader.FPos, AParam);
        if FHPGLReader.FPos^ <> #27 then
        Inc(FHPGLReader.FPos);
      end;
    'D':;//vFontId := AParam;
    'X':;//vX := AParamD;
    'Y':;// vY := AParamD;
    'T':;// vFrameAnchorPoint := AParam;
  end;

end;

procedure TpclReader.DoMisc(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
begin
  case ACommand of
    'A': StartData(AParam);
    'B','C': EndData;
    'S': FRasterWidth := AParam;
    'T': FRasterHeight := AParam;
    'U': SimpleColors(AParam);
  end;
end;

procedure TpclReader.DoPage(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
begin
  case ACommand of
    'A': ; //Paper size
    'C': ; //Vertical Motion Index (in 1/48")
    'D': ; //Vertical Spacing, Lines per Inch
    'E': ; //Text Top Margin
    'F': ; //Text Length in lines
    'H': FHPGLReader.NewLayout;//Page Eject (feed from tray #)
    'G': ; //Select output bin
    'O': ; //&l0O Portrait Orientation
           //ESC &l1OLandscape Orientation
    'P': ; //Page Length in lines
    'U': ; //Long-edge offset registration
    'Z': ; //Short-edge offset registration
  end;
end;

procedure TpclReader.DoParams(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);

  function SetColor(ARed, AGreen, ABlue: Byte): TPaletteEntry;
  begin
    Result.peRed := ARed;
    Result.peGreen := AGreen;
    Result.peBlue := ABlue;
  end;

  procedure GetData;
  var
    P: PAnsiChar;
    vCount: Integer;
  begin
    vCount := AParam;
    if vCount > 18 then
      vCount := 18;
    Dec(AParam, vCount);
    P := @FPCLBitmap;
    while vCount > 0 do
    begin
      P^ := FHPGLReader.GetChar;
      Inc(P);
      Dec(vCount);
    end;

    if FPCLBitmap.Encode <> 2 then
    begin
      case FPCLBitmap.BitIndex of
        1: begin
          FMaxLogPalette.palPalEntry[0] := SetColor(255, 255, 255);
          FMaxLogPalette.palPalEntry[1] := SetColor(0, 0, 0);
          FMaxLogPalette.palNumEntries := 2;
        end;
        2: begin
          FMaxLogPalette.palPalEntry[0] := SetColor(0, 0, 0);
          FMaxLogPalette.palPalEntry[1] := SetColor(255, 0, 0);
          FMaxLogPalette.palPalEntry[2] := SetColor(0, 255, 0);
          FMaxLogPalette.palPalEntry[3] := SetColor(255, 255, 255);
          FMaxLogPalette.palNumEntries := 4;
        end;
        else begin
          FMaxLogPalette.palPalEntry[0] := SetColor(0, 0, 0);
          FMaxLogPalette.palPalEntry[1] := SetColor(255, 0, 0);
          FMaxLogPalette.palPalEntry[2] := SetColor(0, 255, 0);
          FMaxLogPalette.palPalEntry[3] := SetColor(255, 255, 0);
          FMaxLogPalette.palPalEntry[4] := SetColor(0, 0, 255);
          FMaxLogPalette.palPalEntry[5] := SetColor(255, 0, 255);
          FMaxLogPalette.palPalEntry[6] := SetColor(0, 255, 255);
          FMaxLogPalette.palPalEntry[7] := SetColor(255, 255, 255);
          FMaxLogPalette.palNumEntries := 256;
        end;
      end;
    end;

    while AParam > 0 do
    begin
      FHPGLReader.GetChar;
      Dec(AParam);
    end;
  end;
begin
  case ACommand of
    'A': FPaletteEntry.peBlue := AParam;
    'B': FPaletteEntry.peGreen := AParam;
    'C': FPaletteEntry.peRed := AParam;
    'W': GetData;
    'I':
      begin
        if AParam and not $FF = 0 then
          FMaxLogPalette.palPalEntry[AParam] := FPaletteEntry;
        //FTransparency := False;
      end;
    'N': ;//FTransparency := AParam = 0;
  end;
end;

procedure TpclReader.DoPlanes;

  procedure Transfer(A,B: Pointer);
{$IFNDEF SG_USE_ASM32}
  var
    vA,vB: PByte;
    I,vTemp,vBData: Integer;
    vByte: Byte;
  begin
    vA := A; vB := B;
    vBData := PInteger(vB)^;
    Inc(vA,4); Inc(vB,4);
    while vBData > 0 do begin
      vTemp := vB^;
      for I:=0 to 7 do begin//FI:W528 ignore
        vTemp := vTemp + vTemp;//FI:W510 ignore
        vByte := vA^;
        vA^ := vByte + vByte + Ord(vTemp > 255);
        Inc(vA);
        vTemp := vTemp and 255;
      end;
      Inc(vB);
      Dec(vBData);
    end;
  end;
{$ELSE}
  asm
    PUSH	EBX
    MOV	EBX,EAX
    MOV	ECX,[EDX]
          JECXZ   @@3
    ADD	EDX,4
    ADD	EBX,4
    @@1:	MOV	AL,[EDX]
    MOV	AH,8
    @@2:	ADD	AL,AL
    RCL	BYTE PTR [EBX],1
    INC	EBX
    DEC	AH
    JNZ	@@2
    INC	EDX
    DEC	ECX
    JNZ	@@1
    @@3:	POP	EBX
  end;
{$ENDIF}

var
  I, J, K: Integer;
  P: Pointer;
begin
  FMaxLen := FMaxLen shl 3;
  I := 0;
  J := 0;
  while J < Count do
  begin
    P := AllocMem(FMaxLen + 4);
    PInteger(P)^ := FMaxLen;
    for K := J + FPCLBitmap.BitIndex - 1 downto J do
    begin
      if K >= Count then
        Continue;
      if Items[K] <> nil then
        Transfer(P, Items[K]);
      FreeMem(Items[K]);
    end;
    Items[I] := P;
    Inc(I);
    Inc(J, FPCLBitmap.BitIndex);
  end;
  Count := I;
end;

procedure TpclReader.DoPosition(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);

  procedure SetPos(var Pos: TsgFloat; Coef: TsgFloat);
  begin
    if not ARel then
      Pos := 0;
    Pos := Pos + AParam * Coef;
    //FOwner.FPenPos := FPenPos; //!!!
  end;

var
  SY: TsgFloat;
begin
  SY := FHPGLReader.FScale.Y;
  if FHPGLReader.FIsPCL then
    SY := -SY;
  case ACommand of
    'X': SetPos(FPenPosition.X, FHPGLReader.FScale.X);
    'Y': SetPos(FPenPosition.Y, SY);
    'P':
      if AParam = 0 then
        PushPalette
      else
        PopPalette;
  end;
end;

procedure TpclReader.DoRaster(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
 procedure GetData;
  var
    P: PAnsiChar;
    vCount: Integer;
  begin
    vCount := AParam;
    FPCLRasterData := AllocMem(vCount);
    Dec(AParam, vCount);
    P := PAnsiChar(FPCLRasterData);
    while vCount > 0 do
    begin
      P^ := FHPGLReader.GetChar;
      Inc(P);
      Dec(vCount);
    end;
    while AParam > 0 do
    begin
      FHPGLReader.GetChar;
      Dec(AParam);
    end;
  end;
begin
  { Comment this for ###Encode=0 & BitIndex=8 }
  FPCLBitmap.Encode := 0;
  FPCLBitmap.BitIndex := 8;
  case ACommand of
    'W': GetData;
  end;
//begin
//  { Comment this for ###Encode=0 & BitIndex=8 }
//  //Configure Raster Data
//  FPCLBitmap.Encode := 0;
//  FPCLBitmap.BitIndex := 8;
//  Inc(FHPGLReader.FPos, AParam);
end;

procedure TpclReader.DoROP(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
begin
  if ACommand = 'O' then
    FROP := AParam;
end;

procedure TpclReader.DoRotate(Sender: TObject);
begin
  FHPGLReader.DoProgress(psRunning);
end;

procedure TpclReader.DoUnit(ACommand: AnsiChar; ARel: Boolean; AParam: Integer; AParamD: Double);
begin
  if ACommand = 'D' then
    FHPGLReader.FMeasure := AParam;
end;

procedure SwapRB(ADst: Pointer; Count: Integer);
//var
//  A,B: Byte;
//  vBytePointer: PByte;
//begin
//  vBytePointer := ADst;
//  while Count > 0 do begin
//    A := vBytePointer^;
//    Inc(vBytePointer,2);
//    B := vBytePointer^;
//    vBytePointer^ := A;
//    Dec(vBytePointer,2);
//    vBytePointer^ := B;
//    Inc(vBytePointer,3);
//    Dec(Count);
//  end;
//end;
{$IFNDEF SG_ASSEMBLER}
  var
    A,B: Byte;
    vBytePointer: PByte;
  begin
    vBytePointer := ADst;
    while Count > 0 do begin
      A := vBytePointer^;
      Inc(vBytePointer,2);
      B := vBytePointer^;
      vBytePointer^ := A;
      Dec(vBytePointer,2);
      vBytePointer^ := B;
      Inc(vBytePointer,3);
      Dec(Count);
    end;
{$ELSE}
  asm
  @@1:
  {$IFDEF SG_CPUX64}
    MOV   EAX,[RCX]
    MOV   [RCX+2],AL
    SHR   EAX,8
    MOV   [RCX],AH
    ADD   RCX,3
  {$ELSE}
    MOV   ECX, [EAX]
    MOV   [2 + EAX], CL
    SHR   ECX, 8
    MOV   [EAX], CH
    ADD   EAX, 3
  {$ENDIF}
    DEC   EDX
    JNZ   @@1
{$ENDIF}
end;

procedure TpclReader.EndData;
const
  cnstBitmapPixelSize: array[TPixelFormat] of Byte = (1,1,4,8,16,16,24,32,32);
  cnstPalEnts: array[pfDevice..pf8bit] of Byte = (0,1,15,255);
  cnstStdPal: array[0..7] of Integer = (0, $FF, $FF00, $FFFF, $FF0000, $FF00FF, $FFFF00, $FFFFFF);

  procedure FillBitmap(var ABitmap: TsgBitmap; const APixelFormat: TPixelFormat);
  var
    N, I, vCount: Integer;
    vPData: PInteger;
  begin
    N := (ABitmap.Width * cnstBitmapPixelSize[APixelFormat] + 31) and -32 shr 3;
    for I := 0 to ABitmap.Height - 1 do
    begin
      vPData := Items[I];
      if vPData = nil then
        Continue;
      FHPGLReader.DoProgress(psRunning);
      vCount := vPData^;
      Inc(vPData);
      if vCount > N then
        vCount := N;
      FillChar(ABitmap.ScanLine[I]^, N, 0);
      System.Move(vPData^, ABitmap.ScanLine[I]^, vCount);
      if APixelFormat = pf24bit then
        SwapRB(ABitmap.ScanLine[I], ABitmap.Width);
    end;
    I := ABitmap.Height;		// rotate-independent value ;-)
    FPenPosition.Y := FPenPosition.Y + I * FHPGLReader.FScale.Y * FHPGLReader.AddImage(ABitmap);
  end;

  procedure SetPixelFormatAndPalette(var APixelFormat: TPixelFormat;
    var APPalette: Pointer);
//  var
//    I: Integer;
  begin
//    I := FPCLBitmap.Encode * FPCLBitmap.BitIndex;
    case FPCLBitmap.Encode of
      0, 1, 4:
        begin
          case FPCLBitmap.BitIndex of
            0..1: APixelFormat := pf1bit;
            2..4: APixelFormat := pf4bit;
            5..8: APixelFormat := pf8bit;
          end;
        end;
      2: APixelFormat := pf16bit;  
      3: APixelFormat := pf24bit;
    else
      APixelFormat := pf24bit;  
    end;
//    case I of
//      0..1:   APixelFormat := pf1bit;
//      2..4:   APixelFormat := pf4bit;
//      5..8:   APixelFormat := pf8bit;
//      9..15:  APixelFormat := pf15bit;
//      16:     APixelFormat := pf16bit;
//      17..24: APixelFormat := pf24bit;
//    else
//      APixelFormat := pf32bit;
//    end;
    if FPCLBitmap.Encode = 2 then
      APPalette := @cnstStdPal[0]
    else
      APPalette := @FMaxLogPalette.palPalEntry[0];
    if (FPCLBitmap.Encode and 1 = 0) and (FPCLBitmap.BitIndex > 1) { ###Encode=0 & BitIndex=8 } then
    begin
      DoPlanes;
      APixelFormat := pf8bit;
    end;
  end;

var
  vBitmap: TsgBitmap;
  vRasterWidth, vRasterHeight: Integer;
  vPPalette: Pointer;
  vPixelFormat: TPixelFormat;
begin
  //if FData.BitsNdx = 0 then FData.BitsNdx := 8;{ ###Encode=0 & BitsNdx=8 }
  SetPixelFormatAndPalette(vPixelFormat, vPPalette);
  vRasterWidth := FRasterWidth;
  vRasterHeight := FRasterHeight;
  if vRasterWidth = 0 then
    vRasterWidth := FMaxLen shl 3;
  if vRasterHeight = 0 then
    vRasterHeight := Count;
  vBitmap := TsgBitmap.Create;
  try
    vBitmap.PixelFormat := vPixelFormat;
    vBitmap.ROP := FROP;
    vBitmap.OnRotate := DoRotate;
    vBitmap.Width := Min(vRasterWidth, FMaxLen shl 3 div cnstBitmapPixelSize[vPixelFormat]);
    vBitmap.Height := Min(vRasterHeight,Count);
    if (vBitmap.Width = 0) or (vBitmap.Height = 0) then
      Exit;
    vBitmap.SetColors(vPPalette);
    FillBitmap(vBitmap, vPixelFormat);
  finally
    vBitmap.Free;
    Reset;
  end;
end;

procedure TpclReader.PopPalette;
var
  vPSavedPalette: PSavedPal;
begin
  vPSavedPalette := FSavedPalette;
  if vPSavedPalette = nil then
    Exit;
  FSavedPalette := vPSavedPalette.Previous;
  System.Move(vPSavedPalette.Data, FPCLBitmap, SizeOf(FPCLBitmap));
  System.Move(vPSavedPalette.Pal, FMaxLogPalette, SizeOf(FMaxLogPalette));
  System.Move(vPSavedPalette.Pens, FHPGLReader.FHPPens, SizeOf(FHPGLReader.FHPPens));
  Dispose(vPSavedPalette);
end;

procedure TpclReader.PushPalette;
var
  vPSavedPalette: PSavedPal;
begin
  New(vPSavedPalette);
  vPSavedPalette.Previous := FSavedPalette;
  System.Move(FPCLBitmap, vPSavedPalette.Data, SizeOf(FPCLBitmap));
  System.Move(FMaxLogPalette, vPSavedPalette.Pal, SizeOf(FMaxLogPalette));
  System.Move(FHPGLReader.FHPPens, vPSavedPalette.Pens, SizeOf(FHPGLReader.FHPPens));
  FSavedPalette := vPSavedPalette;
end;

procedure TpclReader.ReadDataV(ACount: Integer);
var
  P: PAnsiChar;
  vBase: PInteger;
  I, vPart: Integer;
begin
  if FCompress = 4 then
  begin
    vPart := 0;
    for I:=0 to 3 do//FI:W528 ignore
      vPart := vPart shl 8 + Ord(FHPGLReader.GetChar);
    Dec(ACount,4);
    vPart := (vPart + 7) shr 3;
    FCompress := 0;
    while ACount > 0 do
    begin
      if vPart > ACount then
        vPart := ACount;
      ReadDataW(vPart);
      Dec(ACount,vPart);
    end;
    FCompress := 4;
    Exit;
  end;
  if FCompress=5 then
  begin
    while ACount >= 3 do
    begin
      FCompress := Ord(FHPGLReader.GetChar);
      vPart := Ord(FHPGLReader.GetChar);
      vPart := vPart shl 8 + Ord(FHPGLReader.GetChar);
      Dec(ACount,3);
      case FCompress of
        0..3:
          begin
            ReadDataW(vPart); Dec(ACount,vPart);
          end;
        4:
          for I:=0 to vPart-1 do//FI:W528 ignore
            Add(nil);
        5:
          for I:=0 to vPart-1 do//FI:W528 ignore
            Add(Unpack(True,nil,PackEmpty));
      else
        Break;
      end;
    end;
    Inc(FHPGLReader.FPos, ACount);
    FCompress := 5;
    Exit;
  end;
  GetMem(vBase, ACount+4);
  vBase^ := ACount;
  P := PAnsiChar(vBase);
  Inc(P,4);
  while ACount > 0 do
  begin
    P^ := FHPGLReader.GetChar;
    Inc(P);
    Dec(ACount);
  end;
  case FCompress of
    1: vBase := Unpack(False,vBase,PackRLE);
    2:  vBase := Unpack(False,vBase,PackBits);
    3:  vBase := Unpack(True,vBase,PackDelta);
    9:  vBase := Unpack(True,vBase,PackCompDelta);
    10:  vBase := Unpack(True,vBase,PackNLL);
  else
    if FCompress <> 0 then
    begin
      FreeMem(vBase);
      Exit;
    end;
  end;
  if FMaxLen < vBase^ then
    FMaxLen := vBase^;
  Add(vBase);
end;

procedure TpclReader.ReadDataW(ACount: Integer);
var
  I: Integer;
begin
  if FPCLBitmap.Encode and 1 <> 0 then
    I := 0
  else
    I := (Count+1) mod FPCLBitmap.BitIndex;
  if I = 0 then
    I := FPCLBitmap.BitIndex;
  while I < FPCLBitmap.BitIndex do
  begin
    Add(nil);
    Inc(I);
  end;
  ReadDataV(ACount);
end;

procedure TpclReader.Reset;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
   FreeMem(Items[I]);
  Count := 0;
  FMaxLen := 0;
  if FPCLRasterData <> nil then
    FreeMem(FPCLRasterData);
  FPCLRasterData := nil;
end;

procedure TpclReader.SimpleColors(AParam: Integer);

  procedure MakeEntry(var Dst: TPaletteEntry; Src: TPaletteEntry);
  begin
    Dst.peRed := Src.peBlue;
    Dst.peGreen := Src.peGreen;
    Dst.peBlue := Src.peRed;
    Dst.peFlags := 0;
  end;

var
  I: Integer;
begin
  FPCLBitmap.Encode := 0;
  FPCLBitmap.BitIndex := Abs(AParam);
  FHPGLReader.SimpleColors(AParam);
  for I:=0 to 15 do
    MakeEntry(FMaxLogPalette.palPalEntry[I], TPaletteEntry(FHPGLReader.FHPPens[I].Color));
  FMaxLogPalette.palNumEntries := 16;
end;

procedure TpclReader.StartData(AMode: Integer);
begin
  Reset;
  FStartGraphicMode := AMode;
end;

function TpclReader.Unpack(ACopy: Boolean; P: Pointer; AProc: TpclUnpackProc): Pointer;
const
  cnstMaxUnpackBuffSize = $38000;
type
  PUnpackBuff = ^TUnpackBuff;
  TUnpackBuff = record
    Size: Integer;
    Raw: array[0..3] of Byte;
  end;

var
  vPrevBuff, vBuff: PUnpackBuff;
  vBuffSize: Integer;

  function CreateCopyBuff(AInput: PUnpackBuff; ANewSize: Integer): PUnpackBuff;
  begin
    GetMem(Result, ANewSize);
    Result^.Size := AInput^.Size;
    System.Move(AInput^.Raw, Result^.Raw, AInput^.Size);
  end;

begin
  vPrevBuff := nil;
  if ACopy and (Count > 0) then
    vPrevBuff := PUnpackBuff(Last);
  if vPrevBuff <> nil then
  begin
    if vPrevBuff^.Size > cnstMaxUnpackBuffSize then
      vBuffSize := vPrevBuff^.Size + SizeOf(Integer)
    else
      vBuffSize := cnstMaxUnpackBuffSize;
    vBuff := CreateCopyBuff(vPrevBuff, vBuffSize);
    FillChar(vBuff^.Raw[vPrevBuff^.Size],
      vBuffSize - vPrevBuff^.Size - SizeOf(Integer), 0);
  end
  else
    vBuff := AllocMem(cnstMaxUnpackBuffSize);
  AProc(P, vBuff);
  Result := CreateCopyBuff(vBuff, vBuff^.Size + SizeOf(Integer));
  FreeMem(vBuff);
  FreeMem(P);
end;

{ TsgHPGLConverter }

{$IFDEF SG_HPGL2_DEBUG}
destructor TsgHPGLConverter.Destroy;
begin
  FStackCommands.Free;
  inherited Destroy;
end;
{$ENDIF}

procedure TsgHPGLConverter.Initialize;
begin
  inherited Initialize;
  UseSHXFonts := False;
  InitializeSectionsBegin;
  InitLTypes;
  InitStandartStyleByText;
{$IFDEF SG_HPGL2_DEBUG}
  FStackCommands := TList.Create;
  FStackCommands.Count := 0;
  FFileName := FileName;
{$ENDIF}
end;

procedure TsgHPGLConverter.InitLTypes;

  procedure NewLType(const Ticks: array of Double);
  var
   vLineType: TsgDXFLineType;
  begin
   vLineType := TsgDXFLineType.Create;
   vLineType.Name := Format('LType_%.3d', [Sections[csLTypes].Count+1]);
   vLineType.Lines.Initialize(Ticks);
   Sections[csLTypes].AddEntity(vLineType);
   Loads(vLineType);
  end;

begin
  NewLType([2.5, -1, 0, -1, 1, -1, 0, -1]);
  NewLType([3.5, -1, 0, -1, 0, -1]);
  NewLType([2.5, -1, 1, -1, 1, -1]);
  NewLType([3.5, -1, 1, -1, 3.5]);
  NewLType([4,   -1, 0, -1, 4]);
  NewLType([3.5, -3]);
  NewLType([2.5, -5]);
  NewLType([0, -10]);
  NewLType([0, -10]);
  NewLType([0, -10]);
  NewLType([5, -5]);
  NewLType([7, -3]);
  NewLType([8, -1, 0, -1]);
  NewLType([7, -1, 1, -1]);
  NewLType([5, -1, 1, -1, 1, -1]);
  NewLType([7, -1, 0, -1, 0, -1]);
  NewLType([5, -1, 0, -1, 1, -1, 0, -1]);
end;

procedure TsgHPGLConverter.InitStandartStyleByText;
var
  vText: TsgDXFTextAccess;
  vStyle: TsgDXFStyle;
begin
  vText := TsgDXFTextAccess(TsgDXFText.Create);
  try
    Self.Loads(vText);
  finally
   vText.Free;
  end;
  vStyle := Self.StyleByName(sStandardName);
  if vStyle = nil then
  begin
    vStyle := TsgDXFStyle.Create;
    vStyle.Name := sStandardName;
    Self.Sections[csStyles].AddEntity(vStyle);
  end;
  vStyle.FontName := cnstDefFontFamily;
  vStyle.PrimaryFont := cnstDefFileName;
end;

{$IFDEF SG_HPGL2_DEBUG}
procedure TsgHPGLConverter.SaveStackCommand;
var
  I: Integer;
  vStartPos, vEndPos, vOldPos: PAnsiChar;
  vStrings: TStringList;
  vChar : AnsiChar;
  S: AnsiString;
begin
  vStrings := TStringList.Create;
  if FStackCommands.Count < 1 then
    Exit;
  vStartPos := PAnsiChar(FStackCommands.List[0]);
  vOldPos := FHPGLReader.FPos;
  try
    try
      I := 1;
      while I < FStackCommands.Count do
      begin
        FHPGLReader.FPos := vStartPos;
        vEndPos := PAnsiChar(FStackCommands.List[I]);
        Inc(I);
        while (FHPGLReader.FPos < vEndPos) or (FHPGLReader.FPos >= FHPGLReader.FLimit) do
        begin
          vChar := FHPGLReader.GetChar;
          case vChar of
            #3,#13,#10,';':;
            #27:
            begin
              S := S +'[ESC]';
            end;
          else
            S := S+ vChar;
          end;
        end;
        vStartPos := vEndPos;
        if S <> '' then
          vStrings.Add(string(S));
        S := '';
      end;
      if Length(FFileName) = 0 then
        FFileName := 'stackcommands_' + IntToHex(TsgNativeInt(Self), 8);
      vStrings.SaveToFile('d:\LogCGM\' + FFileName + '.txt');
    except
    end;
  finally
    FHPGLReader.FPos := vOldPos;
    vStrings.Free;
  end;
end;
{$ENDIF}

procedure TsgHPGLConverter.StopLoading;
begin
  inherited StopLoading;
  if FHPGLReader <> nil then FHPGLReader.FPos := FHPGLReader.FLimit;
end;

{ TsgHPGLImage }

function TsgHPGLImage.GetMMHeight: Integer;
begin
  Result := Abs(FMMHeight);// 100 * Round(AbsHeight);// for future versions
end;

function TsgHPGLImage.GetMMWidth: Integer;
begin
  Result := Abs(FMMWidth);// 100 * Round(AbsWidth);// for future versions
end;

function TsgHPGLImage.GetUsePSInstruction: Boolean;
begin
  Result := FUsePSInstr;
end;

procedure TsgHPGLImage.SetSizes;
var
  vHeight, vWidth: Double;
begin
  vWidth := PureExtents.Right - PureExtents.Left;
  vHeight := PureExtents.Top - PureExtents.Bottom;
  if FUsePSInstr then
  begin
    if (FPage.X <> 0) and (FPage.X <> DefPageWidth) then
      FMMWidth := Abs(Round(FPage.X * 1016 * 100{/ 40}))
    else
      FMMWidth := Round(vWidth {* 100 / 40});
    if (FPage.Y <> 0) and (FPage.Y <> DefPageHeight) then
      FMMHeight := Abs(Round(FPage.Y * 1016 * 100{/ 40}))
    else
      FMMHeight := Round(vHeight * 100 {/ 40});
  end
  else
  begin
    FMMWidth := Round(vWidth * 100 {/ 40});
    FMMHeight := Round(vHeight * 100 {/ 40});
  end;
end;

procedure TsgHPGLImage.SetUsePSInstruction(const AValue: Boolean);
begin
  if AValue <> FUsePSInstr then
  begin
    FUsePSInstr := AValue;
    SetSizes;
  end;
end;

function TsgHPGLImage.CreateConverter: TsgDXFConverter;
begin
  Result := TsgHPGLConverter.CreateFromFile('');
  //Result.StyleByName(sStandardName).PrimaryFont := 'simplex_.ttf';
end;

procedure TsgHPGLImage.DoProgress(AStage: TProgressStage; ADone, ACount: Integer);
const
  R: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
begin
  Progress(Self, AStage, Byte(sgMulDiv(ADone, 100, ACount)), False, R, '');
end;

{procedure TsgHPGLImage.FillBoxForCoordsCalc(ACoord: Integer; var ABox: TFRect);
begin
  ABox.Left := 0;
  ABox.Right := MMWidth / 100;
  ABox.Bottom := 0;
  ABox.Top := MMHeight / 100;
  ABox.Z1 := 0;
  ABox.Z2 := 0;
end;           }

function TsgHPGLImage.GetImageType: TsgExportFormat;
begin
  Result := efPlt;
end;


function TsgHPGLImage.GetHeight: Integer;
begin
  Result := inherited GetHeight;
end;

function TsgHPGLImage.GetWidth: Integer;
begin
  Result := inherited GetWidth;
end;

procedure TsgHPGLImage.SetDefaultViewModePlotSettings;
begin
  ShowPlotSetting := True;
  ShowPlotForModel := True;
  ShowPlotMargins := True;
end;

procedure TsgHPGLImage.SetExtentsParameters(const ARect: TFRect; const AIs3DExtents: Boolean);
begin
  inherited SetExtentsParameters(ARect, AIs3DExtents);
  SetSizes;
end;

constructor TsgHPGLImage.Create;
begin
  inherited Create;
  Converter.UseSHXFonts := False;
  FPage := MakeFPoint(DefPageWidth, DefPageHeight, 0.0);
  CBFormat := CF_HPGL;
  FMMWidth := 21000;
  FMMHeight := 29200;
//  FUsePSInstr := True;
end;

destructor TsgHPGLImage.Destroy;
begin
  inherited Destroy;
end;

procedure TsgHPGLImage.ApplyLineWeightFactor(var ALineWeight: Double);
begin
  DoScale2D(FDraw);
  ALineWeight := ALineWeight * {40.0 *} FDraw.YScale * FmmToPixelX;
end;

procedure TsgHPGLImage.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TsgHPGLImage then
  begin
    FUsePSInstr := TsgHPGLImage(Source).FUsePSInstr;
    FPage := TsgHPGLImage(Source).FPage;
    SetSizes;
  end;
end;

function TsgHPGLImage.CommonToInternalUnits(APoint: TFPoint): TFPoint;
begin
  Result := ConvertMMToHPGLSteps(APoint);
end;

function TsgHPGLImage.ConvertHPGLStepsToMM(APoint: TFPoint): TFPoint;
var
  vRect: TFRect;
  vWidth, vHeight: Double;
begin
  vRect := CurrentLayout.Box;
  vWidth := vRect.Right - vRect.Left;
  vHeight := vRect.Top - vRect.Bottom;
  if vWidth = 0 then
    vWidth := 1;
  if vHeight = 0 then
    vHeight := 1;
  Result := MakeFPoint(MMWidth / 100 * (APoint.X - vRect.Left) / vWidth,
                       MMHeight / 100 * ((APoint.Y - vRect.Bottom) / vHeight), 0);
end;

function TsgHPGLImage.ConvertMMToHPGLSteps(APoint: TFPoint): TFPoint;
var
  vRect: TFRect;
  vMMWidth, vMMHeight: Integer;
begin
  vRect := CurrentLayout.Box;
  vMMWidth := MMWidth;
  if vMMWidth = 0 then
    vMMWidth := 1;
  vMMHeight := MMHeight;
  if vMMHeight = 0 then
    vMMHeight := 1;
  Result := MakeFPoint(vRect.Left + (vRect.Right - vRect.Left) * APoint.X * 100 / vMMWidth,
    vRect.Bottom + (vRect.Top - vRect.Bottom) * APoint.Y * 100 / vMMHeight, 0);
end;

(*// For future versions
function TsgHPGLImage.GetEntityColor(AEntity: TsgDXFEntity; AInsert: TsgDXFInsert): TColor;
begin
  Result := AEntity.Color;
end;
*)

function TsgHPGLImage.InternalToCommonUnits(APoint: TFPoint): TFPoint;
begin
  Result := ConvertHPGLStepsToMM(APoint);
end;

procedure TsgHPGLImage.LoadFromStream(S: TStream);

  procedure CreateHPGLREader(AStream: TStream; AImage: TsgCADImage);
  var
    vHPGLReader: TsgHPGLReader;
    I: Integer;
  begin
    vHPGLReader := TsgHPGLReader.Create(Converter, AStream);
    try
      vHPGLReader.FUsePSInstr := Self.FUsePSInstr;
      vHPGLReader.FOnProgress := DoProgress;
      vHPGLReader.FPCLReader.FUseGDIPlus := LibraryGDIPlusExists;
      vHPGLReader.FInterpretHPGLCmdNRAsNewPageOnRead := InterpretHPGLCmdNRAsNewPageOnRead;
      vHPGLReader.ReadFile;
      Converter.Source := nil;//evg
      SetDefaultViewPort(Converter);
      {$IFNDEF SG_OPENING_IN_THEADS}
      if Loading = Self then
      {$ENDIF}
        GetExtents;
      // Update PlotSettings
      for I := 0 to Converter.LayoutsCount-1 do
      begin
        Converter.Layouts[I].PlotSettings.PlotWindowAreaMin :=
          MakeF2DPoint(Converter.Layouts[I].Box.Left,Converter.Layouts[I].Box.Bottom);
        Converter.Layouts[I].PlotSettings.PlotWindowAreaMax :=
          MakeF2DPoint(Converter.Layouts[I].Box.Right, Converter.Layouts[I].Box.Top);
        Converter.Layouts[I].PlotSettings.PaperImageOrigin :=
          MakeF2DPoint(Abs(Converter.Layouts[I].Box.Left),
          Abs(Converter.Layouts[I].Box.Bottom));
      end;
       if AImage <> nil then
        AImage.Assign(Self);
      FPage := vHPGLReader.FPage;
      //SetSizes;// see SetExtentsParameters
    finally
      vHPGLReader.Free;
    end;
  end;

var
  {$IFNDEF SG_OPENING_IN_THEADS}
  vImage: TsgCADImage;
  {$ENDIF}
  vStream: TStream;
  vDecomposeFile: TMemoryStream;
  vDecomposeFileName: string;
begin
  inherited LoadFromStream(S);
  {$IFNDEF SG_OPENING_IN_THEADS}
  vImage := Loading;
  if vImage <> nil then
    vImage.Converter.StopLoading;
  Loading := Self;
  {$ENDIF}
  try
    Converter.ClearDrawingProp;
    ThpConverter(Converter).SetLoading(True, nil);
    try
      vStream := S;
      vDecomposeFile := nil;
      try
        if IsGZPacked(vStream) then
        begin
          vDecomposeFile := TMemoryStream.Create;
          if UnPackGZStream(S, vDecomposeFile, vDecomposeFileName) then
          begin
            vDecomposeFile.Position := 0;
            vStream := vDecomposeFile;
          end;
        end;
        CreateHPGLREader(vStream, {$IFNDEF SG_OPENING_IN_THEADS}vImage{$ELSE}nil{$ENDIF});
        RotateImageEnt;
        SetDefaultPlotSettings{$IFDEF SG_HPGL_ResetPlotSettings}(True){$ENDIF};
      finally
        vDecomposeFile.Free;
      end;
    finally
      ThpConverter(Converter).SetLoading(False, nil);
    end;
  finally
    {$IFNDEF SG_OPENING_IN_THEADS}
    Loading := nil;
    {$ENDIF}
  end;
end;

function TsgHPGLImage.Measurement: TsgMeasurement;
begin
  Result.DistanceUnits := duMM;
  Result.AngularUnits := auNone;
end;

{ TsgHPGLReader }

procedure TsgHPGLReader.AddArc(ArcAbs: Boolean);
var
  A1: Double;
begin
  FError := GetCommandParameters(FScaling, not ArcAbs) or (FCommandParameters.Count < 3);
  if FError then
    Exit;
  A1 := FCommandParameters[2];
  if FScaling then
    A1 := A1 / Abs(FScale.Y);
  DoArc(ArcAbs, CommandPoint(0), A1);
end;

procedure TsgHPGLReader.AddArcThreePoint;

  function Rel(const P0, P1: TFPoint): TFPoint;
  begin
    Result.X := P0.X - P1.X;
    Result.Y := P0.Y - P1.Y;
  end;

  procedure Params(var ADest: TFPoint; const APenPos, ACommandPoint: TFPoint;
    var AX, AY: Double);
  begin
    AX := (APenPos.X + ACommandPoint.X) * 0.5;
    AY := (APenPos.Y + ACommandPoint.Y) * 0.5;
    ADest.X := AX - ACommandPoint.X;
    ADest.Y := ACommandPoint.Y - AY;
    ADest.Z := AX * ADest.X - AY * ADest.Y;
  end;

var
  vX, vY: Double;
  A, B, C: TFPoint;
begin
  FError := GetCommandParameters(FScaling,False) or (FCommandParameters.Count < 4);
  if FError then
    Exit;
  Params(A, FPenPos, CommandPoint(0), vX, vY);
  Params(B, CommandPoint(0), CommandPoint(2), vX, vY);
  C.Z := A.X * B.Y - A.Y * B.X;
  if C.Z = 0 then
    Exit;
  C.X := (A.Z * B.Y - A.Y * B.Z) / C.Z;
  C.Y := (A.Z * B.X - A.X * B.Z) / C.Z;
  C.Z := 0;
  A := Rel(CommandPoint(2), C);
  B := Rel(FPenPos,C);
  vX := ArcTan2(A.Y, A.X) - ArcTan2(B.Y, B.X);
  A := Rel(FPenPos, CommandPoint(0));
  B := Rel(CommandPoint(2), CommandPoint(0));
  if vX * (A.X * B.Y - A.Y * B.X) > 0 then
    vX := -vX;
  DoArc(True, C, vX * 180 /Pi);
end;

procedure TsgHPGLReader.AddDefineLabelTerm;
var
  vChar: AnsiChar;
  vDL: AnsiChar;
  vIsDrawDLT: Boolean;
  vNext: Integer;
begin
  vNext := 0;
  vDL := #3;
  vIsDrawDLT := False;
  try
    repeat
      vChar := GetChar;
      case vNext of
        0: begin
          if not (vChar in [#0,#5,#27,#59]) then
            vDL := vChar;
          if vChar in [#3] then
          begin
            if not (FPos^ in [',',';'])  then
              Break;
          end;
          Inc(vNext);
        end;
        1: begin
          if vChar in [','] then
            Inc(vNext);
        end;
        2: begin
          case vChar of
            '0': vIsDrawDLT := True;
            '1': vIsDrawDLT := False;
          end;
        end;
      end;
    until (AnsiChar(vChar) = ';') or Eof;
  finally
    FLblTerm := '' + vDL;
    FIsVisibleLbTerm := vIsDrawDLT;
  end;
end;

procedure TsgHPGLReader.AddCircle;
var
  vCircle: TsgDXFCircle;
  vRadius: Double;
  vCirclePoint, vTempPoint: TFPoint;
begin
  FError := GetCommandParameters(FScaling,True) or (FCommandParameters.Count = 0);
  if FError then
    Exit;
  vCircle := TsgDXFCircle.Create;
  vRadius := FCommandParameters[0];
  vCirclePoint := ConvertPoint(False, FPenPos, vTempPoint);
  vCirclePoint.Z := 0;
  vCircle.Point := vCirclePoint;
  vCircle.Radius := vRadius * 0.025;
  AddEntity(vCircle);
  FPenDown := False;
end;

procedure TsgHPGLReader.AddEntity(AEntity: TsgDXFEntity);
begin
  if FLayout = nil then
    NewLayout;
  AEntity.SrcStart := FStart;
  AEntity.SrcEnd := FPos;
  FStart := FPos;
  AEntity.ColorCAD := ConvertARGBToColorCAD(CurrentColor(AEntity is TsgCADPolyPolygon),
    @cnstColorCADHPGLNone);
  AEntity.SetLType(FLType);
  AEntity.SetLWeight(Round(FHPPens[FCurPenNum].Width * 100));
  AEntity.Layer := GetLayer(FCurPenNum);
  Loads(AEntity);
  FEmpty := False;
  if FPolyMode then
    FPolyBuffer.Add(AEntity)
  else
  begin
    if AddFlat(AEntity) then
      Exit;
    if InsertHatch(AEntity)
      then Exit;
    if not AddToInsert(AEntity) then
      FLayout.AddEntity(AEntity);
  end;
  if Assigned(FDXFConverter.OnCreate) then
    FDXFConverter.OnCreate(AEntity);
end;

function TsgHPGLReader.AddFlat(AEntity: TsgDXFEntity): Boolean;
var
  vPolyline: TsgCADBasePolyline;
begin
  Result := (AEntity is TsgCADBasePolyline) and TsgCADBasePolyline(AEntity).Lines.IsSolid
    and (TsgCADBasePolyline(AEntity).PolyPoints.Count > 0);
  if not Result then
    Exit;
  vPolyline := TsgCADBasePolyline(AEntity);
  if (FFlat = nil) or not FFlat.Accepts(vPolyline) or (FFlat.Layer <> AEntity.Layer) then
  begin
    FFlat := TsgFlatPoly.Create;
    FFlat.Layer := GetLayer(FCurPenNum);
    if not AddToInsert(FFlat) then
      FLayout.AddEntity(FFlat);
    if Assigned(FDXFConverter.OnCreate) then
      FDXFConverter.OnCreate(FFlat);
  end;
  FFlat.Add(vPolyline);
  vPolyline.Free;
end;

procedure TsgHPGLReader.AddHatch;
var
  I, J: Integer;
  vHatch: TsgCADPolyPolygon;
  vEntity: TsgDXFEntity;
  vPolyPoints: TFPointList;
begin
  if FPolyBuffer.Count = 0 then
    Exit;
  Inc(FTotal);
  vHatch := TsgCADPolyPolygon.Create;
  for I := 0 to FPolyBuffer.Count - 1 do
  begin
    vEntity := TsgDXFEntity(FPolyBuffer[I]);
    if vEntity is TsgDXFVPort then
      vHatch.Boundaries.Add(TF2DPointList.Create);
    if vEntity is TsgDXFLine then
    begin
      vEntity := Line2Poly(TsgDXFLine(vEntity));
      FPolyBuffer[I] := vEntity;
    end;
    if not (vEntity is TsgCADBasePolyline) then
      Continue;
    vPolyPoints := TsgCADBasePolyline(vEntity).PolyPoints;
    if vPolyPoints.Count = 0 then
      Continue;
    if vHatch.Boundaries.Count = 0 then
      vHatch.Boundaries.Add(TF2DPointList.Create);
    for J := 0 to vPolyPoints.Count - 1 do  // STEM-LP3.HPG
    begin
      TF2DPointList(vHatch.Boundaries.Last).Add(
        MakeF2DPointFrom3D(vPolyPoints[J]));
    end;
  end;
  if (vHatch.Boundaries.Count = 1) and (TF2DPointList(vHatch.Boundaries.Last).Count = 0) then
    vHatch.Free
  else
    AddEntity(vHatch);
end;

function TsgHPGLReader.AddImage(ABitmap: TsgBitmap): Double;

    function PreparePointIsFile(const APenPosition: TFPoint;
      const AImageRotation: Integer): TFPoint;
    begin
      Result := MakeFPoint(APenPosition.X, APenPosition.Y, 0);
      if (AImageRotation = 0) and not FIsPCL and (FPCLReader.FRotationPage >= 0) then
      begin
         if not FPCLReader.FUseHPGLCAP then
         begin
           FRotation := 270 - FPCLReader.FRotationPage;
           Result.X := Result.X - FRealP2.X + FRealP1.X;
         end
         else
         begin
           // ????? - see ...\Bugzilla_big_files\2149\
         end;
      end;
      if FIsPCL or (FPCLReader.FStartGraphicMode and 1 = 0) then
        Result.X := 0;
    end;

    function PreparePointScaled: TFPoint;
    begin
      Result.X := 1.0;
      Result.Y := 1.0;
      if FPCLReader.FStartGraphicMode and 2 <> 0 then
      begin
        Result.X := FPCLReader.FScaledRasterWidth / ABitmap.Width * FRTLRes / 720.0;
        Result.Y := FPCLReader.FScaledRasterHeight / ABitmap.Height * FRTLRes / 720.0;
        if Result.X = 0 then
          Result.X := Result.Y;
        if Result.X = 0 then
          Result.X := 1.0;
        if Result.Y = 0 then
          Result.Y := Result.X;
      end;
    end;

    procedure RotateImage(AImage: TsgDXFImageEnt; const ARotation: Integer;
      const APointScaled, AScale: TFPoint; var APoint: TFPoint);
    begin
      if (ARotation = 0) and (not FIsPCL){ and (FInsert = nil) } then
      begin
//        if FPCLReader.FStartGraphicMode >= 2 then
//          FRotation := 180
//        else
          FRotation := 90;
      end;
      AImage.Size := MakeFPoint(APointScaled.X * Abs(ABitmap.Width * AScale.X * 0.025),
        APointScaled.Y * Abs(ABitmap.Height * AScale.Y * 0.025), 0);//   AImage.Size := P;
      if Abs(FRotation) > 0 then// rotate vectors
      begin
        AImage.UVector := RotateFPoint(cnstXOrtAxis, FRotation);
        AImage.VVector := RotateFPoint(cnstYOrtAxis, FRotation);
        APoint := RotateFPoint(MakeFPoint(0, AImage.Size.Y, 0), FRotation);
        AImage.Point := MakeFPoint(AImage.Point.X - APoint.X, AImage.Point.Y - APoint.Y, 0);
      end;
    end;

var
  vImageEnt: TsgDXFImageEnt;
  vPointScaled, vScale, vPoint: TFPoint;
  vImageRotation: Integer;
{$IFDEF SG_FIREMONKEY}
  vBitmap: TBitmap;
{$ENDIF}
begin
  vImageRotation := FRotation;
  if FScaling then
    vScale := FScale
  else
    vScale := MakeFPoint(FMeasure / FRTLRes, -FMeasure / FRTLRes, 1);//  vScale := MakeFPoint(1,1,1);
  vPointScaled := ConvertPoint(False,
    PreparePointIsFile(FPCLReader.FPenPosition, vImageRotation), vPointScaled);
  //  P.X := P.X * 40;
  //  P.Y := P.Y * 40;
  vImageEnt := TsgDXFImageEnt.Create;
  vImageEnt.Point := vPointScaled;
  vPointScaled := PreparePointScaled;
  RotateImage(vImageEnt, vImageRotation, vPointScaled, vScale, vPoint);
  if FIsPCL then
    Result := -vPointScaled.Y
  else
    if FScaling then
      Result := vPointScaled.Y
    else
    begin
      Result := -vScale.Y;
      //BM.ROP := 168;// for future versions (see "FWZ1127A0_ZU0004_mitRaster.plt")
    end;
//  Result := -P.Y; //
{$IFDEF SG_FIREMONKEY}
  vBitmap := TBitmap.Create;
  try
    try
      ABitmap.AssignToBMP(vBitmap);
      vImageEnt.SetImage(TGraphic(vBitmap));
    except
      FreeAndNil(vImageEnt);
    end;
  finally
    FreeAndNil(vBitmap);
  end;
{$ELSE}
  try
    vImageEnt.SetImage(ABitmap);
  except
    FreeAndNil(vImageEnt);
  end;
{$ENDIF}
  if Assigned(vImageEnt) then
  begin
    vImageEnt.IsMonoChrome := ABitmap.PixelFormat = pf1bit;
    //vImageEnt.Transparency := FPCLReader.FTransparency;
    if (FImgEnt <> nil) and AppendImage(FImgEnt,vImageEnt) then
      vImageEnt.Free
    else
    begin
      FImgEnt := vImageEnt;
      AddEntity(vImageEnt);
      vImageEnt.TransparentColor := clWhite;
      FImgEnt := vImageEnt;
      FHatchIndex := FLayout.Count;// FConv.Counts[csEntities];
    end;
  end;
  FRotation := vImageRotation;
end;

procedure TsgHPGLReader.AddLine;
var
  vPolyline: TsgDXFPolyline;
  vVertex: TsgDXFVertex;
  I: Integer;
  vVertexPoint: TFPoint;
  vCommandParameter: Double;
  IsAddPoint: Boolean;
begin
  FError := GetCommandParameters(FScaling, FRelative);
  if FError or (FCommandParameters.Count = 0) {or ((FCmdPar.Count mod 2) <> 0)} then
    Exit;
  if FPenDown or FPolyMode then
  begin
    IsAddPoint := False;
    if not (FPolyMode and (FPolyBuffer.Count > 0) and (TObject(FPolyBuffer.Last) is TsgDXFVPort)) then
    begin
      vCommandParameter := FPenPos.Y;
      FCommandParameters.Insert(0, vCommandParameter);
      vCommandParameter := FPenPos.X;
      FCommandParameters.Insert(0, vCommandParameter);
      IsAddPoint := True;
    end;
    vPolyline := TsgDXFLWPolyline.Create;
    I := 0;
    while I < FCommandParameters.Count - 1 do
    begin
      if IsAddPoint then
        vVertexPoint := ConvertPoint(FRelative and (I > 0), CommandPoint(I), FPenPos)
      else
        vVertexPoint := ConvertPoint(FRelative, CommandPoint(I), FPenPos);
      vVertex := TsgDXFVertex.Create;
      vVertex.Point := vVertexPoint;
      vPolyline.AddEntity(vVertex);
      Inc(I, 2);
    end;
    AddEntity(vPolyline);
  end
  else
  begin
    I := 0;
    while I < FCommandParameters.Count - 1 do
    begin
      vVertexPoint := ConvertPoint(FRelative, CommandPoint(I), FPenPos);
      Inc(I, 2);
    end;
  end;
end;

procedure TsgHPGLReader.AddPoly;

  procedure Next(var APolyline: TsgDXFPolyline);
  begin
    if APolyline = nil then
      Exit;
    if APolyline.Count < 2 then
      APolyline.Free
    else
      AddEntity(APolyline);
    APolyline := nil;
  end;

var
  I,J: Integer;
  vEntity: TsgDXFEntity;
  vDottedSingPoints: TFPointList;
  vPolyline: TsgDXFPolyline;
  vVertex: TsgDXFVertex;
  vDottedSingPoint: TFPoint;
begin
  vPolyline := nil;
  for I := 0 to FPolyBuffer.Count-1 do
  begin
    vEntity := TsgDXFEntity(FPolyBuffer[I]);
    if vEntity is TsgDXFVPort then
      Next(vPolyline);
    if not (vEntity is TsgCADBasePolyline) then
      Continue;
    vDottedSingPoints := TsgCADBasePolyline(vEntity).PolyPoints;
    if vDottedSingPoints.Count = 0 then
      Continue;
    for J := 0 to vDottedSingPoints.Count - 1 do
    begin
      if vPolyline = nil then
      begin
        vPolyline := TsgDXFLWPolyline.Create;
//        if I > 0 then Continue;
      end;
      vDottedSingPoint := vDottedSingPoints[J];
      if vDottedSingPoint.Z <> 0 then
        Continue;
      vVertex := TsgDXFVertex.Create;
      vVertex.Point := vDottedSingPoint;
      vPolyline.AddEntity(vVertex);
    end;
  end;
  Next(vPolyline);
end;

procedure TsgHPGLReader.AddPolyline;

  function Done(const AChar: AnsiChar): Boolean;
  begin
    Result := (AChar = #0)  or (AChar = ';');
  end;

  function DecodeCoord(var AChar: AnsiChar; const ATerm: AnsiChar;
    var AEror: Boolean; const ANextChar: Boolean; const AShift: Integer) : integer;
  var
    vTemp, vTem1: Integer;
  begin
    vTem1 := 0;
    Result := 0;
    if ANextChar then
      AChar := GetChars;
    while AChar < ATerm do
    begin
      if Ord(AChar) < 63 then
        AEror := True;
      if AEror then
        Exit;
      vTemp := Ord(AChar) - 63;
      Result := Result + (vTemp shl vTem1);
      vTem1 := vTem1 + AShift;
      AChar := GetChars;
    end;
    vTemp := Ord(AChar) - Ord(ATerm);
    if vTem1 > 0 then
      Result := Result + (vTemp shl vTem1)
    else
      Result := vTemp;
    if Odd(Result) then
    begin
      Dec(Result);
      Result := -Result;
    end;
    Result := Result div 2;
  end;

  procedure NewPoly(var AEntity: TsgDXFPolyline;
    const AEror,  AFlag, APolygonMode: Boolean);
  begin
    FPolyMode := APolygonMode;
    if Assigned(AEntity) then
    begin
      if not AEror and (AEntity.Count > 1 - Ord(FPolyMode and AFlag)) then
        AddEntity(AEntity)
      else
      AEntity.Free;
      AEntity := nil;
    end;
    FPolyMode := APolygonMode and AFlag;
  end;

var
  vPenUp, vPolygoneMode, vRectangleMode, vIsError: Boolean;
  vX, vY: Single;
  vPenPos, vPointIsFile: TFPoint;
  vChar, vBits: AnsiChar;
  vNumberPen, vShift: Integer;
  vPolyline: TsgDXFPolyline;
  vHatch: TsgCADPolyPolygon;
begin
  vPenPos := FPenPos;
  vPenUp := False;
  vPolygoneMode := FPolyMode;
  FPolyMode := False;
  vBits := #191;
  vShift := 6;
  vRectangleMode := False;
  vIsError := False;
  vPolyline := nil;
  vHatch := nil;
  try
    repeat
      vChar := GetChar;
      if Ord(vChar) and $7F < 63 then
      case vChar of
        ':':
          begin
            vNumberPen := DecodeCoord(vChar, vBits, vIsError, True, vShift);
            if CheckIndexPen(vNumberPen) then
            begin
              NewPoly(vPolyline, vIsError, False, vPolygoneMode);
              FCurPenNum := vNumberPen;
              UpdatePen(FCurPenNum);
            end;
          end;
        '<':
          begin
            vPenUp := True;
            vRectangleMode := False;
          end;
        '>':  ;//PenUp := True;
        '=': vPenPos := MakeFPoint(0.0, 0.0, 0.0);
        '7':
          begin
            vBits := #95;
            vShift := 5;
          end;
        '9':
          begin
            vRectangleMode := True;
            vPenUp := True;
          end;
      end
      else
      begin
        DoProgress(psRunning);
        vX := DecodeCoord(vChar, vBits, vIsError, False, vShift);
        if Done(vChar) then
          Exit;
        vY := DecodeCoord(vChar, vBits, vIsError, True, vShift);
        if Done(vChar) then
          Exit;
        if FScaling then
        begin
          vX := vX * FScale.X;
          vY := vY * FScale.Y;
        end;
        vPenPos := MakeFPoint(vPenPos.X + vX, vPenPos.Y + vY, 0.0);
        if FNextPointUp then
        begin
          vPenUp := FNextPointUp;
          FNextPointUp := False;
        end;
        if (vPolygoneMode or not vPenUp) and not Assigned(vPolyline) then
          AddVertex(vPolyline, vHatch, FPenPos, vPolygoneMode, vPenUp);
        if vPenUp then
          NewPoly(vPolyline, vIsError, False, vPolygoneMode);
        if vPenUp or not vRectangleMode then
          AddVertex(vPolyline, vHatch, vPenPos, vPolygoneMode, vPenUp)
        else
        begin
          vPointIsFile := FPenPos;
          vHatch := TsgCADPolyPolygon.Create;
          vHatch.Boundaries.Add(TF2DPointList.Create);
          AddVertex(vPolyline, vHatch, MakeFPoint(vPenPos.X, vPointIsFile.Y), vPolygoneMode, vPenUp);
          AddVertex(vPolyline, vHatch, vPenPos, vPolygoneMode, vPenUp);
          AddVertex(vPolyline, vHatch, MakeFPoint(vPointIsFile.X, vPenPos.Y), vPolygoneMode, vPenUp);
          AddVertex(vPolyline, vHatch, vPointIsFile, vPolygoneMode, vPenUp);
          AddEntity(vHatch);
          vHatch := nil;
          vPenPos := vPointIsFile;
        end;
        vPenUp := not vPenUp and vRectangleMode;
      end;
    until Done(vChar);
  finally
    NewPoly(vPolyline, vIsError, True, vPolygoneMode);
  end;
end;

procedure TsgHPGLReader.AddRectangle(const ARelative: Boolean);

  procedure AddVertx(APolyline: TsgDXFPolyline; const AVertexPoint: TFPoint);
  var
    vVertex: TsgDXFVertex;
  begin
    vVertex := TsgDXFVertex.Create;
    vVertex.Point := AVertexPoint;
    APolyline.AddEntity(vVertex);
  end;

var
  vPolyline: TsgDXFPolyline;
  vCommandPoint, vRectPoint1, vRectPoint2, vRectPoint3, vRectPoint4: TFPoint;
begin
  FError := GetCommandParameters(FScaling, ARelative);
  if FError or (FCommandParameters.Count <> 2) then
    Exit;
  vPolyline := TsgDXFLWPolyline.Create;
  vPolyline.Closed := True;
  vCommandPoint := MakeFPoint(FCommandParameters[0], FCommandParameters[1], 0.0);
  vRectPoint1 := FPenPos;
  if not ARelative then
    vRectPoint3 := vCommandPoint
  else
    vRectPoint3 := MakeFPoint(vRectPoint1.X + vCommandPoint.X, vRectPoint1.Y + vCommandPoint.Y, 0.0);
  vRectPoint2 := MakeFPoint(vRectPoint3.X, vRectPoint1.Y, 0.0);
  vRectPoint4 := MakeFPoint(vRectPoint1.X, vRectPoint3.Y, 0.0);
  AddVertx(vPolyline, ConvertPoint(False, vRectPoint1, vCommandPoint));
  AddVertx(vPolyline, ConvertPoint(False, vRectPoint2, vCommandPoint));
  AddVertx(vPolyline, ConvertPoint(False, vRectPoint3, vCommandPoint));
  AddVertx(vPolyline, ConvertPoint(False, vRectPoint4, vCommandPoint));
  AddEntity(vPolyline);
end;

procedure TsgHPGLReader.AddSolid(const ARelative: Boolean);

  procedure AddPt(var ABoundaryPoint, AFirstBoundaryPoint: TFPoint; AHatch: TsgCADPolyPolygon);
  var
    vBounadryPoint: TF2DPoint;
  begin
    ABoundaryPoint := ConvertPoint(False, ABoundaryPoint, AFirstBoundaryPoint);
    vBounadryPoint.X := ABoundaryPoint.X;
    vBounadryPoint.Y := ABoundaryPoint.Y;
    TF2DPointList(AHatch.Boundaries.Last).Add(vBounadryPoint);
  end;

var
  vHatch: TsgCADPolyPolygon;
  vBoundarePoint, vBoundarePoint1, vBoundarePoint2, vBoundarePoint3,
    vBoundarePoint4: TFPoint;
begin
  FError := GetCommandParameters(FScaling,ARelative);
  if FError or (FCommandParameters.Count <> 2) then
    Exit;
  vBoundarePoint := MakeFPoint(FCommandParameters[0],
    FCommandParameters[1], 0.0);
  vBoundarePoint1 := FPenPos;
  if not ARelative then
    vBoundarePoint3 := vBoundarePoint
  else
    vBoundarePoint3 := MakeFPoint(vBoundarePoint1.X + vBoundarePoint.X,
      vBoundarePoint1.Y + vBoundarePoint.Y, 0.0);
  vBoundarePoint2 := MakeFPoint(vBoundarePoint3.X, vBoundarePoint1.Y, 0.0);
  vBoundarePoint4 := MakeFPoint(vBoundarePoint1.X, vBoundarePoint3.Y, 0.0);
  vHatch := TsgCADPolyPolygon.Create;
  vHatch.Boundaries.Add(TF2DPointList.Create);
  AddPt(vBoundarePoint1, vBoundarePoint, vHatch);
  AddPt(vBoundarePoint2, vBoundarePoint, vHatch);
  AddPt(vBoundarePoint3, vBoundarePoint, vHatch);
  AddPt(vBoundarePoint4, vBoundarePoint, vHatch);
  AddEntity(vHatch);
end;

procedure TsgHPGLReader.AddSpline(ARelative: Boolean);

  function GeneratePolyBezier(const AControlPoints: TFPointList): Boolean;
  const
    cnstAmount = 4;
  var
    vSpline: TsgDXFSpline;
    vKnot, vInc: Single;
    vNumberOfControlPoints, I, J: Integer;
  begin
    Result := False;
    if ((AControlPoints.Count - 4) mod 3) <> 0 then // Bad Bezier
      Exit;
    vSpline := TsgDXFSpline.Create;
    vNumberOfControlPoints := AControlPoints.Count - 1 + (Math.Floor(AControlPoints.Count/3));
    vInc := 1.0 / vNumberOfControlPoints;
    I := 0;
    vKnot := 0;
    while I < vNumberOfControlPoints do
    begin
      for J := 0 to cnstAmount - 1 do//FI:W528 ignore
        vSpline.Knots.Add(vKnot);
      vKnot := vKnot + vInc;
      I := I + cnstAmount;
    end;
    vKnot := 1;
    for J := 0 to cnstAmount - 1 do//FI:W528 ignore
      vSpline.Knots.Add(vKnot);
    for I := 0 to AControlPoints.Count - 1 do
    begin
      vSpline.Controls.Add(AControlPoints[I]);
      if (I mod 3 = 0) and (I <> 0) and (I <> AControlPoints.Count - 1) then
        vSpline.Controls.Add(AControlPoints[I]);
    end;
    AddEntity(vSpline);
    Result := True;
  end;

var
  vPoint: TFPoint;
  I: Integer;
  vValue: Double;
  vControlPoints: TFPointList;
  vPenPos: TFPoint;
begin
  ARelative := ARelative or FRelative;
  FError := GetCommandParameters(FScaling,ARelative);
  if FError or (FCommandParameters.Count <= 2) then
    Exit;
  vPenPos := FPenPos;
  vValue := FPenPos.Y;
  FCommandParameters.Insert(0, vValue);
  vValue := FPenPos.X;
  FCommandParameters.Insert(0, vValue);
  I := 0;
  vControlPoints := TFPointList.Create;
  try
    while I <= FCommandParameters.Count - 2 do
    begin
      if I = 0 then
        vPoint := ConvertPoint(False, CommandPoint(I), vPenPos)
      else
        vPoint := ConvertPoint(ARelative, CommandPoint(I), vPenPos);
      vControlPoints.Add(vPoint);
      if I mod 6 = 0 then
        FPenPos := vPenPos
      else
        vPenPos := FPenPos;
      Inc(I, 2);
    end;
    GeneratePolyBezier(vControlPoints);
  finally
    vControlPoints.Free;
  end;
end;

procedure TsgHPGLReader.AddText;
var
  vTextWithParams, vText: string;
  vCarriageReturn, vLineFeed: Integer;
begin
  vTextWithParams := GetStrCmdPar;
  if AnsiPos(FLblTerm, vTextWithParams) > 0 then
    if not FIsVisibleLbTerm then
      Delete(vTextWithParams, AnsiPos(FLblTerm, vTextWithParams), Length(FLblTerm));
//  P := FConv.Counts[csEntities];
//  if (P = 0) or not (FConv.Entities[P-1] is TsgDXFText) then
//    FPenPosTemp := FPenPos;
  if not IsEqualFPoints(FTextPos.PenPos, FPenPos) then
  begin
    FTextPos.PenPos := FPenPos;
    FTextPos.Point := FPenPos;
  end;
  while vTextWithParams <> '' do
  begin
    vCarriageReturn := Pos(#13, vTextWithParams);
    vLineFeed := Pos(#10, vTextWithParams);
    if vLineFeed = 0 then
      vLineFeed := vCarriageReturn;
    if (vCarriageReturn = 0) or (vCarriageReturn > vLineFeed) then
      vCarriageReturn := vLineFeed;
    if vCarriageReturn = 0 then
      vCarriageReturn := MaxInt;
    if vCarriageReturn = 1 then
    begin
      if vTextWithParams[1] = #13 then
        FPenPos.X := FTextPos.Point.X
      else
        FPenPos.Y := FTextPos.Point.Y;
      Delete(vTextWithParams, 1, 1);
      FTextPos.PenPos := FPenPos;
      Continue;
    end;
    vText := Copy(vTextWithParams, 1, vCarriageReturn - 1);
    Delete(vTextWithParams, 1, vCarriageReturn - 1);
    DoAddText(vText);
  end;
end;

function TsgHPGLReader.AddToInsert(AEntity: TsgDXFEntity): Boolean;
begin
  Result := FInsert <> nil;
  if not Result then
    Exit;
  if FInsert.Block.Count = 0 then
  begin
    FLayout.AddEntity(FInsert);
  end;
  FInsert.Block.AddEntity(AEntity);
end;

procedure TsgHPGLReader.AddVertex(var AEntity: TsgDXFPolyline; var AHatch: TsgCADPolyPolygon;
  const APoint: TFPoint; const APolygonMode, APenUp: Boolean);
var
  vVertex: TsgDXFVertex;
  vVertexPoint: TFPoint;
begin
  if not Assigned(AEntity) then
    AEntity := TsgDXFLWPolyline.Create;
  vVertexPoint := MakeFPoint(0.0, 0.0, 0.0);
  vVertex := TsgDXFVertex.Create;
  vVertex.Point := ConvertPoint(False, APoint, vVertexPoint);
  vVertexPoint := vVertex.Point;
  vVertexPoint.Z := Ord(APolygonMode and APenUp);
  vVertex.Point := vVertexPoint;
  AEntity.AddEntity(vVertex);
  FPenPos := APoint;
  if AHatch <> nil then
    TF2DPointList(AHatch.Boundaries.Last).Add(MakeF2DPointFrom3D(vVertexPoint));
end;

procedure TsgHPGLReader.BeginPlot;
var
  vDone: Boolean;
  vChar: AnsiChar;
begin
  vDone := False;
  while not vDone do
  begin
    vChar := GetChar;
    case vChar of
      '"':
        repeat
          vChar := GetChar;
          if vChar <> '"' then FDrawingName := FDrawingName + Char(vChar);
          DoProgress(psRunning);
        until vChar = '"';
    end;
    vDone := (vChar = ';') or (vChar in ['A'..'Z']);
  end;
  Dec(FPos);
  NewLayout;
end;

procedure TsgHPGLReader.CharSlant;
begin
  FError := GetCommandParameters(False,False);
  if FError then
    Exit;
  if FCommandParameters.Count = 0 then
    FSlant := 0
  else
    FSlant := ArcTan(FCommandParameters[0]) * 180 / 3.1415926;
end;

function TsgHPGLReader.CheckIndexPen(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= Low(FHPPens)) and (AIndex <= High(FHPPens))
end;

procedure TsgHPGLReader.ClearPolyBuffer;
var
  I: Integer;
begin
  if not Assigned(FPolyBuffer) then
    Exit;
  for I := 0 to FPolyBuffer.Count - 1 do
    TObject(FPolyBuffer[I]).Free;
  FPolyBuffer.Clear;
end;

function TsgHPGLReader.CommandPoint(const AIndex: Integer): TFPoint;
begin
  Result.X := FCommandParameters[AIndex];
  Result.Y := FCommandParameters[AIndex + 1];
  Result.Z := 0.0;
end;

procedure TsgHPGLReader.CommentHPGL;
var
  vChar: AnsiChar;
  vTemp: AnsiString;
begin
  vTemp := '';
  repeat
    vChar := GetChar;
  until (AnsiChar(vChar) = '"') or Eof;
  repeat
    vChar := GetChar;
    vTemp := vTemp + AnsiChar(vChar);
  until (AnsiChar(vChar) = '"') or Eof;
  Delete(vTemp, Length(vTemp), 1);
end;

function TsgHPGLReader.ConvertPoint(const ARelative: Boolean; const vDoPoint: TFPoint;
  var vDestPoint: TFPoint): TFPoint;
var
  vPoint, vPoint2: TFPoint;
begin
  vPoint := cnstFPointZero;
  if FScaling then
  begin
    vPoint.X := FRealP1.X - FUserP1.X * FScale.X;
    vPoint.Y := FRealP1.Y - FUserP1.Y * FScale.Y;
  end;
  if ARelative then
  begin
    vDestPoint.X := vDestPoint.X + vDoPoint.X;
    vDestPoint.Y := vDestPoint.Y + vDoPoint.Y;
  end
  else
    vDestPoint := vDoPoint;
  vDestPoint.Z := 0;
  Result.X := vDestPoint.X + vPoint.X;
  Result.Y := vDestPoint.Y + vPoint.Y;
  Result.Z := Ord(FPolyMode and not FPenDown);
  Result.X := Result.X * 0.025;
  Result.Y := Result.Y * 0.025;
  vPoint2 := Result;
  case FRotation of
    0:
        Result.Y := Result.Y + FFrame;
    90:
      begin
        Result.X := FFrame - vPoint2.Y;
        Result.Y := vPoint2.X;
      end;
    180:
      begin
        Result.X := -vPoint2.X;
        Result.Y := FFrame - vPoint2.Y;
      end;
    270:
      begin
        Result.X := FFrame + vPoint2.Y;
        Result.Y := -vPoint2.X;
      end;
  end;
end;

function TsgHPGLReader.CurrentColor(const ADoShading: Boolean): TColor;

  function Shade(AValue: Integer): Byte;
  begin
    Result := 255 - Round(FShading * (255 - AValue and 255));
  end;

var
  vRed, vGreen, vBlue: Byte;
begin
  Result := FHPPens[FCurPenNum].Color;
  if not ADoShading or (FShading >= 1.0) {or (Result = clNone)} then
    Exit;
  vRed := Shade(Result shr 16);
  vGreen := Shade(Result shr 8);
  vBlue := Shade(Result);
  Result := vRed shl 16 or vGreen shl 8 or vBlue;
end;

procedure TsgHPGLReader.DoAddText(var AText: String);
const
  Classes: array[0..1] of TsgDXFEntityClass = (TsgDXFText, TsgDXFMText);
var
  vEntity: TsgDXFEntity;
  vTextPoint: TFPoint;
  I: Integer;
  vTextHW: TFPoint;
  vPenPos: TFPoint;
  vBoxText: TFRect;
  vMidPoint: TFPoint;
  vK: Double;

begin
  I := 1;
  while I <= Length(AText) do
  begin
    if AText[1] >= #32 then
      Inc(I)
    else
      Delete(AText,I,1);
  end;
  if Trim(AText) = '' then
    Exit;
  vEntity := Classes[0].Create;
  vTextPoint := cnstFPointZero;
  TsgDXFText(vEntity).Point := ConvertPoint(False, FPenPos, vTextPoint);
  TsgDXFTextAccess(vEntity).ChangeByStyle(FDXFConverter.StyleByName(sStandardName));
  vTextPoint := TsgDXFText(vEntity).Point;
  vTextHW := GetHWText;
  TsgDXFText(vEntity).Point1 := vTextPoint;
  TsgDXFText(vEntity).Height := vTextHW.Y;
  TsgDXFText(vEntity).Scale := GetScale;
  TsgDXFText(vEntity).ObliqueAngle := FSlant;
  TsgDXFText(vEntity).HAlign := FHAlign;
  TsgDXFText(vEntity).VAlign := 0;
  TsgDXFText(vEntity).Text := AText;
  AddEntity(vEntity);
  vBoxText := vEntity.Box;

  //vEntity.Layer := FDXFConverter.LayerByName('LO-'+IntToStr(FLabelOrign));

  if (not FIsSR) and ( FLabelOrign in [12 , 2, 5, 15, 8 ,18, 6, 13, 16, 19] ) then
  begin
    //SI
    case FLabelOrign of
      12 , 2, 5, 15, 8 ,18 : vK := 0.5;
      6, 13, 16, 19: vK := 1.5;
      else
        vK := 0;
    end;
    vMidPoint := MakeFPoint(0, {(vBoxText.Bottom - vBoxText.Top)} -vTextHW.Y * vK);
    vMidPoint := RotateFPoint(vMidPoint, FRotation + FTextRotation);
    vTextPoint := AddFPoint(vTextPoint, vMidPoint);

    TsgDXFText(vEntity).Point := vTextPoint;
    TsgDXFText(vEntity).Point1 := vTextPoint;
  end;

  TsgDXFText(vEntity).Rotation := FRotation + FTextRotation;
  FDXFConverter.Loads(vEntity);

  vPenPos := GetPointOnCircle(FPenPos,40 *(vBoxText.Right-vBoxText.Left),FTextRotation);

  ConvertPoint(False,vPenPos,vPenPos);
  FPenPos.X := vPenPos.X;
  FTextPos.PenPos := FPenPos;
  FTextPos.Point.Y := vPenPos.Y;
end;

procedure TsgHPGLReader.DoArc(const ArcAbs: Boolean; const APoint: TFPoint;
  const AAngle: Double);

  procedure TransformPenPos(const AAngle: Integer; const AOffsetPenPos: TFPoint;
    const AIsArcEnd, AScale: Boolean);
  var
    vPenPosOffset: TFPoint;
  begin
    vPenPosOffset := AOffsetPenPos;
    if AIsArcEnd then
      vPenPosOffset := FArcEnd
    else
      vPenPosOffset := FArcStart;
    vPenPosOffset.X := vPenPosOffset.X * 40;
    vPenPosOffset.Y := vPenPosOffset.Y * 40;
    FPenPos := vPenPosOffset;
    case FRotation of
      90:
        begin
          FPenPos.X := vPenPosOffset.Y;
          FPenPos.Y := -vPenPosOffset.X;
        end;
      180:
        begin
          FPenPos.X := -vPenPosOffset.X;
          FPenPos.Y := -vPenPosOffset.Y;
        end;
      270:
        begin
          FPenPos.X := -vPenPosOffset.Y;
          FPenPos.Y := vPenPosOffset.X;
        end;
    end;
    if AScale then
    begin
      FPenPos.X := FPenPos.X - FRealP1.X + FUserP1.X * FScale.X;
      FPenPos.Y := FPenPos.Y - FRealP1.Y + FUserP1.Y * FScale.Y;
    end;
  end;

var
  vArc: TsgDXFArc;
  vPenPos, vTempPoint: TFPoint;
  vStartAngle, vAngleOffset: Double;
  vIsArcEnd: Boolean;

begin
  vAngleOffset := AAngle;
  if vAngleOffset = 0 then
    Exit;

  vArc := TsgDXFArc.Create;
  vTempPoint := FPenPos;
  vArc.Point := ConvertPoint(not ArcAbs, APoint, vTempPoint);
  vPenPos := ConvertPoint(False, FPenPos, vTempPoint);

  vTempPoint := MakeFPoint(vPenPos.X - vArc.Point.X, vPenPos.Y - vArc.Point.Y, 0);
  vArc.Radius := Sqrt(vTempPoint.X * vTempPoint.X + vTempPoint.Y * vTempPoint.Y);
  vStartAngle := 180 / Pi * ArcTan2(vTempPoint.Y, vTempPoint.X);
  if FScaling and (FScale.X * FScale.Y < 0) then
    vAngleOffset := -vAngleOffset;
  vIsArcEnd := True;
  if vAngleOffset < 0 then
  begin
    vStartAngle := vStartAngle + vAngleOffset;
    vAngleOffset := -vAngleOffset;
    vIsArcEnd := False;
  end;
  vArc.StartAngle := vStartAngle;
  vArc.EndAngle := vStartAngle + vAngleOffset;
  vPenPos := GetPointOnCircle(vArc.Point,vArc.Radius,vArc.EndAngle);
  AddEntity(vArc);

  TransformPenPos(FRotation, vPenPos, vIsArcEnd, FScaling);
end;

procedure TsgHPGLReader.DoProgress(Stage: TProgressStage);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Stage, FPos - FMem, FLimit - FMem);
end;

function TsgHPGLReader.Eof: Boolean;
begin
  Result := True;
  if not Assigned(FStream) then
    Exit;
  Result := FPos >= FLimit;
end;

procedure TsgHPGLReader.Wedge(const AIsFill: Boolean);
var
  Radius: Double;
  start_angle,sweep_angle,chord_angle: Double;
  midAngle, tempS, tempE: Double;
  vRadius: Double;
  vCirclePoint, vTempPoint: TFPoint;

  vPolyline: TsgDXFPolyline;
  vArc: TsgArcR;

  J: Integer;
  vHatch: TsgCADPolyPolygon;
  vPolyPoints: TFPointList;

  procedure AddVertx(APolyline: TsgDXFPolyline; const AVertexPoint: TFPoint);
  var
    vVertex: TsgDXFVertex;
  begin
    vVertex := TsgDXFVertex.Create;
    vVertex.Point := AVertexPoint;
    APolyline.AddEntity(vVertex);
  end;

  procedure AddBulge(APolyline: TsgDXFPolyline; const AArc: TsgArcR);
  var
    vVertexS, vVertexE: TsgDXFVertex;
  begin
    vVertexS := TsgDXFVertex.Create;
    vVertexE := TsgDXFVertex.Create;

    vVertexS.Point := GetPointOnCircle(AArc.Center, AArc.Radius, AArc.AngleS);
    vVertexE.Point := GetPointOnCircle(AArc.Center, AArc.Radius, AArc.AngleE);
    vVertexS.Bulge := GetBulgeOfArcR(vArc, vVertexS.Point);

    APolyline.AddEntity(vVertexS);
    APolyline.AddEntity(vVertexE);
  end;

begin
  FError := GetCommandParameters(FScaling,True) or (FCommandParameters.Count < 3);
  if FError then
    Exit;
  vRadius := FCommandParameters[0];
  vRadius := vRadius * 0.025;

  start_angle := FCommandParameters[1];
  sweep_angle := FCommandParameters[2];
  if Abs(start_angle) > 360 then
    start_angle := sgMod(start_angle,360);
  if Abs(sweep_angle) > 360 then
    sweep_angle := sgMod(sweep_angle,360);

  vCirclePoint := ConvertPoint(FRelative, FPenPos, vTempPoint);

  vArc.Center := vCirclePoint;
  vArc.Radius := Abs(vRadius);
  if start_angle < 0 then
    vArc.AngleS := 360 + start_angle
  else
    vArc.AngleS := start_angle;

  if sweep_angle < 0 then
  begin
    vArc.AngleE := vArc.AngleS + sweep_angle;
    SwapDoubles(vArc.AngleS, vArc.AngleE);
  end
  else
  begin
    vArc.AngleE := vArc.AngleS + sweep_angle;
  end;

  if vRadius > 0 then
  begin
    vArc.AngleS := 180 + vArc.AngleS;
    vArc.AngleE := 180 + vArc.AngleE;
  end;

  vArc.AngleS := 180 + vArc.AngleS;
  vArc.AngleE := 180 + vArc.AngleE;

  if vArc.AngleE < vArc.AngleS then
    vArc.AngleE := vArc.AngleE + 360;

  vPolyline := TsgDXFLWPolyline.Create;
  vPolyline.Closed := True;

  if (vArc.AngleE - vArc.AngleS) >= 360 then
  begin
    tempS := vArc.AngleS;
    tempE := vArc.AngleE;
    midAngle := (tempS + tempE) / 2;
    vArc.AngleS := tempS;
    vArc.AngleE := midAngle;
    sgModAngle(vArc.AngleS, vArc.AngleS);
    sgModAngle(vArc.AngleE, vArc.AngleE);
    AddBulge(vPolyline, vArc);
    vArc.AngleS := midAngle;
    vArc.AngleE := tempE;
    sgModAngle(vArc.AngleS, vArc.AngleS);
    sgModAngle(vArc.AngleE, vArc.AngleE);
    AddBulge(vPolyline, vArc);
  end
  else
  begin
    sgModAngle(vArc.AngleS, vArc.AngleS);
    sgModAngle(vArc.AngleE, vArc.AngleE);
    AddVertx(vPolyline, vCirclePoint);
    AddBulge(vPolyline, vArc);
    AddVertx(vPolyline, vCirclePoint);
  end;

  if AIsFill then
  begin
    Loads(vPolyline);
    vHatch := TsgCADPolyPolygon.Create;
    vHatch.Boundaries.Add(TF2DPointList.Create);
    vPolyPoints := TsgCADBasePolyline(vPolyline).PolyPoints;
    for J := 0 to vPolyPoints.Count - 1 do  // STEM-LP3.HPG
    begin
      TF2DPointList(vHatch.Boundaries.Last).Add(
        MakeF2DPointFrom3D(vPolyPoints[J]));
    end;
    AddEntity(vHatch);
    vPolyline.Free;
  end
  else
    AddEntity(vPolyline);
end;

procedure TsgHPGLReader.FillType;
var
  vFillTapeOptions: TFPoint;
begin
  FShading := 1.0;
  FError := GetCommandParameters(False,False);
  if FError or (FCommandParameters.Count < 2) then
    Exit;
  vFillTapeOptions := CommandPoint(0);
  if vFillTapeOptions.X <> 10 then
    Exit;
  vFillTapeOptions.Y := vFillTapeOptions.Y / 100.0;
  if vFillTapeOptions.Y < 0 then
    vFillTapeOptions.Y := 0;
  if vFillTapeOptions.Y > 1.0 then
    vFillTapeOptions.Y := 1.0;
  FShading := vFillTapeOptions.Y;
end;

procedure TsgHPGLReader.FindSymbol(const ASymbols: TsgHPGLSymbols; var ASymbol: AnsiChar);
begin
  repeat
    ASymbol := FPos^;
    Inc(FPos);
    if TsgNativeInt(FPos) and $3FF = 0 then
      DoProgress(psRunning);
  until (ASymbol in ASymbols) or (FPos >= FLimit);
end;

procedure TsgHPGLReader.FinishInsert;
var
  vBlockClipBox, vBlockBox: TFRect;
  vDoRemove: Boolean;
begin
  if FInsert = nil then
    Exit;
  vDoRemove := FInsert.Block.Count = 0;
  if not vDoRemove then
  begin
    FDXFConverter.Loads(FInsert.Block);
    FDXFConverter.Loads(FInsert);
    vBlockClipBox := FInsert.ClipBox;
    vBlockBox := FInsert.Block.Box;
    vDoRemove := (vBlockBox.Left >= vBlockClipBox.Left) and (vBlockBox.Right <= vBlockClipBox.Right) and
      (vBlockBox.Top <= vBlockClipBox.Top) and (vBlockBox.Bottom >= vBlockClipBox.Bottom);
  end;
  if vDoRemove then
    RemoveInsert;
//  if FInsert.Block.Count = 0 then FInsert.Free
//  else FConv.Loads(FInsert);
  FInsert := nil;
  FFlat := nil;
  FlushHatch;
end;

procedure TsgHPGLReader.FlushHatch;
begin
  if FHatch <> nil then
    FHatch.Counts.Add(0);
  FHatch := nil;
end;

procedure TsgHPGLReader.FrameAdvance;
var
  vFrame: Double;
begin
  GetCommandParameters(False, False);
  if FCommandParameters.Count = 0 then
    vFrame := (FRealP2.X - FRealP1.X) / 2
  else
    vFrame := FCommandParameters[0];
  FFrame := FFrame + vFrame * 0.025;
end;

function TsgHPGLReader.GetChar: AnsiChar;
begin
  if FPos >= FLimit then
    Result := #0
  else
  begin
    Result := FPos^;
    Inc(FPos);
  end;
end;

function TsgHPGLReader.GetChars: AnsiChar;
begin
  repeat
    Result := GetChar
  until not (Result in [#13,#10]);
end;

function TsgHPGLReader.GetCommandParameters(const ADoScale,ARelative: Boolean): Boolean;

  procedure Next(var S: String);
  var
    vPChar: PAnsiChar;
    vPosition: Integer;
  begin
    while (not Eof) and (FPos^ in [#32,#13,#10]) do
      Inc(FPos);
    vPChar := FPos;
    while (not Eof) and (not (FPos^ in ['A'..#255,',',';',#27,#32])) do
      Inc(FPos);
    SetString(S, vPChar, FPos - vPChar);
    while True do
    begin
      vPosition := Pos(#10, S);
      if vPosition = 0 then
        vPosition := Pos(#13, S);
      if vPosition = 0 then
        Exit;
      Delete(S, vPosition, 1);
    end;
  end;

var
  vStr: string;
  vParameter: Single;
  vDecimalSeparator: Char;
  vFlag: Boolean;
begin
  FCommandParameters.Count := 0;
  vDecimalSeparator := SetDecimalSeparator('.');
  try
    vFlag := False;
    while not Eof do
    begin
      Next(vStr);
      if (vStr = '') and (FPos^ <> ',') then
        Break;
      if FPos^ = ',' then
        Inc(FPos);
      vFlag := not vFlag;
      if vFlag and (vStr = '') and (FCommandParameters.Count = 0) then
        Continue;
      vStr := Trim(vStr);
      if vStr = '' then
        Continue;
      vParameter := StrToFloat(vStr);
      if ADoScale then
      begin
        if FCommandParameters.Count and 1 = 0 then
          vParameter := vParameter * FScale.X
        else
          vParameter := vParameter * FScale.Y;
      end;
      FCommandParameters.Add(vParameter);
    end;
  finally
    SetDecimalSeparator(vDecimalSeparator);
  end;
  if FPos^ = ';' then
    Inc(FPos);
  Result := False;
end;

function TsgHPGLReader.GetDefaultPenColor(const AIndex: Integer): Cardinal;
begin
  case AIndex of
    2: Result := clRed;
    3: Result := clGreen;
    4: Result := clYellow;
    5: Result := clBlue;
    6: Result := clPurple;
    7: Result := clAqua;
  else
    Result := clHpglNone;
  end;
end;

function TsgHPGLReader.GetHWText: TFPoint;
begin
  if FIsSR then
  begin
    //SR
    Result.Y := FTextHeight * (FRealP2.Y - FRealP1.Y);
    Result.X := FTextWidth * (FRealP2.X - FRealP1.X) { / Result.Y};
  end
  else
  begin
    //SI
    Result.Y :=  FTextHeight;
    Result.X :=  FTextWidth;
  end;
end;

function TsgHPGLReader.GetLayer(const AIndexLayer: Integer): TsgDXFLayer;
var
  vColor: Cardinal;
begin
  if FLayers[AIndexLayer] = nil then
  begin
    FLayers[AIndexLayer] := TsgDXFLayer.Create;
    FLayers[AIndexLayer].Name := Format('Pen %d', [AIndexLayer]);
    vColor :=  CurrentColor(False);
    FLayers[AIndexLayer].ColorCAD := ConvertARGBToColorCAD(vColor);  //clWhite
    Loads(FLayers[AIndexLayer]);
    FDXFConverter.Sections[csLayers].AddEntity(FLayers[AIndexLayer]);
  end;
  Result := FLayers[AIndexLayer];
end;

(*
Conversion Table| 1 mm   | 1 inch  | 1 plotter | 1 foot  |1 decipoint
                |        |         |    unit   |         |
---------------------------------------------------------------------
Millimeters     | 1      | 25.4    | 0.025     | 304.80  | 0.035
Inches          | 0.039  | 1       | 0.00098   | 12      | 1/720
Plotter units   | 40     | 1016    | 1         | 12192   | 1.4
Feet            | 0.0033 | 1/12    | 0.000082  | 1       | 0.000116
Decipoints      | 28     | 720     | 0.71      | 8640    | 1
*)

function TsgHPGLReader.GetPlotSettings: TsgDXFPlotSettings;
var
  vPlotSttingsData: TsgPlotSettingsData;
  vPaperWidth, vPaperLenght: Double;
  vAttrib: TsgNodeSample;
  vTempStr: string;
  function ConvertDicipointToMillimetr(const ADicipoint: Integer): Double;
  begin
    Result := ADicipoint*0.035;
  end;
begin
  Result := FPlotSttings;
  if Result = nil then
  begin
    FillChar(vPlotSttingsData, SizeOf(TsgPlotSettingsData), 0);
    if (FXMLParser.Root.Count = 1) and Assigned(FXMLParser.Root.Nodes[0])
      and (FXMLParser.Root.Nodes[0].AttributeNodesCount > 0) then
    begin
      //vPlotSttingsData.PrintOrConfigName := 'HPGL printer 2';
      vAttrib := FXMLParser.Root.Nodes[0].GetAttributeByName('ORIENTATION');
      if vAttrib <> nil then
      begin
        vTempStr := vAttrib.Value;
        if UpperCase(vAttrib.Value) = 'PORTRAIT' then
          vPlotSttingsData.PlotRotation := pr90DegreesCounterCW
        else
          vPlotSttingsData.PlotRotation := prNoRotation;
      end;

      vAttrib := FXMLParser.Root.Nodes[0].GetAttributeByName('TOPMARGIN');
      if vAttrib <> nil then
      begin
        vPlotSttingsData.UnprintableMargin.Top :=
          ConvertDicipointToMillimetr(StrToIntDef(vAttrib.Value, 0));
      end;
      vAttrib := FXMLParser.Root.Nodes[0].GetAttributeByName('LEFTMARGIN');
      if vAttrib <> nil then
      begin
        vPlotSttingsData.UnprintableMargin.Left :=
          ConvertDicipointToMillimetr(StrToIntDef(vAttrib.Value, 0));
      end;
      vAttrib := FXMLParser.Root.Nodes[0].GetAttributeByName('RIGHTMARGIN');
      if vAttrib <> nil then
      begin
        vPlotSttingsData.UnprintableMargin.Right :=
          ConvertDicipointToMillimetr(StrToIntDef(vAttrib.Value, 0));
      end;
      vAttrib := FXMLParser.Root.Nodes[0].GetAttributeByName('BOTTOMMARGIN');
      if vAttrib <> nil then
      begin
        vPlotSttingsData.UnprintableMargin.Bottom :=
          ConvertDicipointToMillimetr(StrToIntDef(vAttrib.Value, 0));
      end;

      vAttrib := FXMLParser.Root.Nodes[0].GetAttributeByName('RESOLUTION');
      if vAttrib <> nil then
      begin
        vPlotSttingsData.ShadePlotCustomDPI := StrToIntDef(vAttrib.Value, 300);
      end;

      vPaperLenght := -1;
      vAttrib := FXMLParser.Root.Nodes[0].GetAttributeByName('PAPERLENGTH');
      if vAttrib <> nil then
      begin
        vPaperLenght :=
          ConvertDicipointToMillimetr(StrToIntDef(vAttrib.Value, 0));
      end;
      vPaperWidth := -1;
      vAttrib := FXMLParser.Root.Nodes[0].GetAttributeByName('PAPERWIDTH');
      if vAttrib <> nil then
      begin
        vPaperWidth :=
          ConvertDicipointToMillimetr(StrToIntDef(vAttrib.Value, 0));
      end;
      if vPaperWidth * vPaperLenght > 0 then
      begin
        vPlotSttingsData.PlotPaperSize.X := sgFunction.Min(vPaperWidth, vPaperLenght);
        vPlotSttingsData.PlotPaperSize.Y :=sgFunction.Max(vPaperWidth, vPaperLenght);
      end;



      vPlotSttingsData.ShadePlotResolutionLevel := 5;
      vPlotSttingsData.PlotLayoutFlags := ConvertIntegerToPlotLayoutFlags(688);

      vPlotSttingsData.PageSetupName := '';
      vPlotSttingsData.PrintOrConfigName := cnstDefaulPrinter;
      vPlotSttingsData.PaperSize := '';//'A3';
      vPlotSttingsData.PlotViewName := '';
      //vPlotSttingsData.UnprintableMargin.Left := 5.873749732971191;
      //vPlotSttingsData.UnprintableMargin.Bottom := 5.873748779296875;
      //vPlotSttingsData.UnprintableMargin.Right := 6.12774658203125;
      //vPlotSttingsData.UnprintableMargin.Top := 5.873748779296875;
      //vPlotSttingsData.PlotPaperSize.X := 594;
      //vPlotSttingsData.PlotPaperSize.Y := 841;
//      vPlotSttingsData.PlotOrigin.X := 0.0;
//      vPlotSttingsData.PlotOrigin.Y := 0.0;
      vPlotSttingsData.PlotOrigin.X := vPlotSttingsData.UnprintableMargin.Left;
      vPlotSttingsData.PlotOrigin.Y := vPlotSttingsData.UnprintableMargin.Bottom;


      vPlotSttingsData.PlotWindowAreaMin.X := 0;
      vPlotSttingsData.PlotWindowAreaMin.Y := 0;
      vPlotSttingsData.PlotWindowAreaMax.X := 0;
      vPlotSttingsData.PlotWindowAreaMax.Y := 0;
      vPlotSttingsData.NumeratorOfCustomPrintScale := 1;
      vPlotSttingsData.DenominatorOfCustomPrintScale := 1;
      vPlotSttingsData.PlotLayoutFlags := ConvertIntegerToPlotLayoutFlags(14336);
      vPlotSttingsData.PlotPaperUnits := TsgPlotPaperUnits(1);
      //vPlotSttingsData.PlotRotation := 0;
      vPlotSttingsData.PlotType := TsgPlotType(4);//TsgPlotType(4); //Rect by code 48 49 140 141
      vPlotSttingsData.CurrentStyleSheet := '';
      vPlotSttingsData.StandardScaleType := 16;
      vPlotSttingsData.FloatingPointScaleFactor := 1.0;
      vPlotSttingsData.ShadePlotMode := 0;
      vPlotSttingsData.ShadePlotResolutionLevel := 5;
      //vPlotSttingsData.ShadePlotCustomDPI := 555;
      vPlotSttingsData.PaperImageOrigin.X := 0;
      vPlotSttingsData.PaperImageOrigin.Y := 0;

      FPlotSttings := TsgDXFPlotSettings.Create;
      Result := FPlotSttings;
      TsgDXFPlotSettingsAccess(Result).PlotData^ := vPlotSttingsData;
    end;
  end;
end;

function TsgHPGLReader.GetScale: Single;
var
  vTextHW: TFPoint;
  vEntity: TsgDXFEntity;
  vBoxText: TFRect;
begin
  vTextHW := GetHWText;
  Result := 1;
  if FIsSR then
  begin
    vEntity := TsgDXFText.Create;
    try
      TsgDXFText(vEntity).Point := cnstFPointZero;
      TsgDXFTextAccess(vEntity).ChangeByStyle(FDXFConverter.StyleByName(sStandardName));
      TsgDXFText(vEntity).Point1 := MakeFPoint(cnstFPointZero.X +
        vTextHW.X * Length(cnstSpaceText), cnstFPointZero.Y, 0.0);
      TsgDXFText(vEntity).Height := vTextHW.Y;
      TsgDXFText(vEntity).Scale := 1;
      TsgDXFText(vEntity).ObliqueAngle := FSlant;
      TsgDXFText(vEntity).HAlign := FHAlign;
      TsgDXFText(vEntity).VAlign := 0;
      TsgDXFText(vEntity).Text := cnstSpaceText;
      FDXFConverter.Loads(vEntity);
      vBoxText := vEntity.Box;
    finally
      FreeAndNil(vEntity);
    end;
    if Abs(vBoxText.Right-vBoxText.Left) > fAccuracy then
      Result := Abs(vTextHW.X / (vBoxText.Right-vBoxText.Left));
  end
  else
    Result := Abs(vTextHW.X);
  if Result < fAccuracy then
    Result := 1;
end;

procedure TsgHPGLReader.GetScales;

  function ComandParam(const AIndex: Integer): Double;
  begin
    Result := FCommandParameters[AIndex];
  end;

  procedure SetPoint(var APoint: TFPoint; const APointX, APointY: Single);
  begin
    APoint.X := APointX;
    APoint.Y := APointY;
  end;

  function SetUserPoints(const AScale: Integer): Boolean;
  begin
    Result := False;
    if AScale < 2 then
    begin
      SetPoint(FUserP1, ComandParam(0), ComandParam(2));
      SetPoint(FUserP2, ComandParam(1), ComandParam(3));
      SetPoint(FScale, 1.0, 1.0);
      SetScale(FRealP2, FRealP1, FUserP2, FUserP1, FSCCom);
      FDoScaling := True;
    end
    else
    begin
      SetPoint(FScale, 1.0, 1.0);
      FScaling := False;
      FDoScaling := False;
      if (ComandParam(1) = 1.0) and (ComandParam(3) = 1.0) then
      begin
        SetPoint(FRealP1, ComandParam(0), ComandParam(1));
        Result := True;
      end
      else
      begin
        SetPoint(FUserP1, ComandParam(0), ComandParam(2));
        SetPoint(FUserP2, FUserP1.X + 1, FUserP1.Y + 1);
        SetPoint(FRealP1, 0.0, 0.0);
        SetPoint(FRealP2, ComandParam(1), ComandParam(3));
        SetPoint(FScale, ComandParam(1), ComandParam(3));
      end;
    end;
  end;

var
  vScale: Integer;
begin
  FError := GetCommandParameters(False,True);
  FSCCom := cnstFPointSingle;
  if FCommandParameters.Count = 0 then
  begin
    FScaling := False;
    Exit;
  end;
  if FError then
    Exit;
  if (FCommandParameters.Count < 4) then
  begin
    FScaling := False;
    FDoScaling := False;
    Exit;
  end;
  if FCommandParameters.Count > 4 then
    vScale := Round(ComandParam(4))
  else
    vScale := 0;
  if SetUserPoints(vScale) then
    Exit;
  if FDoScaling or FScaling then
  begin
    SetScale(FRealP2, FRealP1, FUserP2, FUserP1, FScale);
    FDoScaling := False;
  end
  else
    FDoScaling := True;
  // Previous version
  //SetScale;
  //FDoScaling := False;
  FScaling := True;

  FPenPos.X := FPenPos.X - FRealP1.X + FUserP1.X * FScale.X;
  FPenPos.Y := FPenPos.Y - FRealP1.Y + FUserP1.Y * FScale.Y;
end;

function TsgHPGLReader.GetStrCmdPar: string;
var
  vChar: AnsiChar;
begin
  Result := '';
  repeat
    vChar := GetChar;
    Result := Result + Char(vChar);
  until (Char(vChar) = FLblTerm) or Eof;
  if vChar in [#0] then
    Delete(Result, Length(Result), 1);
end;

procedure TsgHPGLReader.InitHPPens;
var
  I: Integer;
begin
  for I := Low(FHPPens) to High(FHPPens) do
  begin
    case I of
      0..7:  FHPPens[I].Color :=  GetDefaultPenColor(I);
    else
      FHPPens[I].Color := clHpglNone;
    end;
    FHPPens[I].Width := 0.35;
  end;
end;

procedure TsgHPGLReader.InitPlot;
begin
  FError := False;
  FHPGLCmd := 0;
  FTerminator := ';';
  FLblTerm := #3;
  FIsVisibleLbTerm := False;
  FDrawingName := '';
  FPenDown := False;
  FRotation := 0;
  FRelative := False;
  FScaling := False;
  FDoScaling := False;
  FRTLScale := 1;
  FPolyMode := False;
  ClearPolyBuffer;
  FPage := MakeFPoint(DefPageWidth, DefPageHeight, 0.0);
  FPenPos := cnstFPointZero;
  FScale := cnstFPointSingle;
  FRealP1 := cnstFPointZero;
  FRealP2 := MakeFPoint(FPage.X, FPage.Y, 0.0);
  FUserP1 := FRealP1;
  FUserP2 := FRealP2;
  FShading := 1.0;
  FTextHeight := HPTextHeight / 40;
  FTextWidth := 0.4;
  FIsSR := False;
  FTextRotation := 0.0;
  InitHPPens;
  FCurPenNum := 1;
end;

procedure TsgHPGLReader.InputPoints;
var
   vRealPointX, vRealPointY: Single;
begin
  FError := GetCommandParameters(False,True);
  if FError or (FCommandParameters.Count = 1) or  (FCommandParameters.Count = 3) then
    Exit;
  case FCommandParameters.Count of
    0:
      begin
        FRealP1.X := 0;
        FRealP1.Y := 0;
        FRealP2.X := FPage.Y;   // ?????
        FRealP2.Y := FPage.X;   // ?????
        FUserP1 := FRealP1;
        FUserP2 := FRealP2;
      end;
    2:
      begin
        vRealPointX := FRealP2.X - FRealP1.X;
        vRealPointY := FRealP2.Y - FRealP1.Y;
        FRealP1.X := FCommandParameters[0];
        FRealP1.Y := FCommandParameters[1];
        FRealP2.X := FRealP1.X + vRealPointX;
        FRealP2.Y := FRealP1.Y + vRealPointY;
      end;
  else
    begin
      FRealP1.X := FCommandParameters[0];
      FRealP1.Y := FCommandParameters[1];
      FRealP2.X := FCommandParameters[2];
      FRealP2.Y := FCommandParameters[3];
    end;
  end;
  if FDoScaling or FScaling then
  begin
    FDoScaling := False;
    SetScale(FRealP2, FRealP1, FUserP2, FUserP1, FScale);
  end
  else
    FDoScaling := (FCommandParameters.Count > 0);
end;

procedure TsgHPGLReader.InputWindow;

  procedure SwapDouble(var D1, D2: TsgFloat);
  var
    vVal: TsgFloat;
  begin
    if D1 > D2 then
    begin
       vVal := D1;
       D1 := D2;
       D2 := vVal;
    end;
  end;

var
  vLowerLeftPoint, vUpperRight: TFPoint;
  vCADClipInsert: TsgCADClipInsert;
  vBlock: TsgDXFBlock;
  //vScaleX, vScaleY: TsgFloat;
begin
  FError := GetCommandParameters(False,False);
  if FError then
    Exit;
  //Exit;// change in future versions
  FinishInsert;
  FlushHatch;
  FFlat := nil;
  if FCommandParameters.Count <> 4 then
    Exit;
  vLowerLeftPoint := CommandPoint(0);
  vUpperRight := CommandPoint(2);
  if (vLowerLeftPoint.X > vUpperRight.X) and (vLowerLeftPoint.Y > vUpperRight.Y) then
  begin
    SwapFPoints(vLowerLeftPoint, vUpperRight);
  end;
  SwapDouble(vLowerLeftPoint.Y, vUpperRight.Y);
  vBlock := TsgDXFBlock.Create;
  vBlock.Name := '$' + IntToHex(TsgNativeInt(Pointer(vBlock)), 8);
  FDXFConverter.Sections[csBlocks].AddEntity(vBlock);
  vCADClipInsert := TsgCADClipInsert.Create;
  vCADClipInsert.Block := vBlock;
  FInsert := vCADClipInsert;
  FLowerLeftPoint := vLowerLeftPoint;
  FUpperRight := vUpperRight;
  UpdateClipData;
end;

function TsgHPGLReader.InsertHatch(AEntity: TsgDXFEntity): Boolean;
var
  vHatch: TsgCADPolyPolygon;
begin
  Result := AEntity is TsgCADPolyPolygon;
  if not Result then
    Exit
  else
    vHatch := TsgCADPolyPolygon(AEntity);
  if (FHatch = nil) or not FHatch.Accepts(vHatch) or (FHatch.Layer <> AEntity.Layer) then
  begin
    FlushHatch;
    FHatch := TsgFlatHatch.Create;
    FHatch.Layer := GetLayer(FCurPenNum);
    if not AddToInsert(FHatch) then
    begin
      FLayout.PaperSpaceBlock.InsertEntity(FHatchIndex, FHatch);
      Inc(FHatchIndex);
    end;
    if Assigned(FDXFConverter.OnCreate) then
      FDXFConverter.OnCreate(FHatch);
  end;
  FHatch.Add(vHatch);
  vHatch.Free;
end;

procedure TsgHPGLReader.LabelOrigin;
var
  vLabelOrign: Integer;
begin
  FError := GetCommandParameters(False,False);
  if FError then Exit;
  if FCommandParameters.Count = 0 then
    vLabelOrign := 0
  else
    vLabelOrign := Round(FCommandParameters[0]);
  FLabelOrign := vLabelOrign;
  case FLabelOrign of
    4,5,6,14,15,16:     FHAlign := 1;
    7,8,9,17,18,19:     FHAlign := 2;
  else
                        FHAlign := 0;
  end;
end;

procedure TsgHPGLReader.Loads(AEntity: TsgDXFEntity);
var
  vArc: TsgDXFArc;
begin
  if AEntity is TsgCADClipInsert then
    Exit;
  FDXFConverter.Loads(AEntity);
  if AEntity is TsgDXFArc then
  begin
    vArc := TsgDXFArc(AEntity);
    FArcStart := vArc.PolyPoints.First;
    FArcEnd := vArc.PolyPoints.Last;
  end;
end;

procedure TsgHPGLReader.MergeControl;
begin
  FError := GetCommandParameters(False,False);
  if FError then
    Exit;
  if FCommandParameters.Count < 2 then
    FPCLReader.FROP := 252
  else
    FPCLReader.FROP := Round(FCommandParameters[1]);
end;

procedure TsgHPGLReader.MoveChar;
const
  cnstSingle: Single = -1.0;
  cnstSpaceText: String = 'Q';
var
  vPenPos: TFPoint;
  vPenPosX, vPenPosY: Double;
  vSin, vCos: Extended;
  vTextHW: TFPoint;
  vFlag: Boolean;
  vEntity: TsgDXFEntity;
  vBoxText: TFRect;
  vMagikTemp: Double;
begin
  vFlag := False;
  FError := GetCommandParameters(False,False);
  if FError then
    Exit;
  vTextHW := GetHWText;
  vMagikTemp := 40;

  vEntity := TsgDXFText.Create;
  try
    TsgDXFText(vEntity).Point := cnstFPointZero;
    TsgDXFTextAccess(vEntity).ChangeByStyle(FDXFConverter.StyleByName(sStandardName));
    TsgDXFText(vEntity).Point1 := MakeFPoint(cnstFPointZero.X +
      vTextHW.X * Length(cnstSpaceText), cnstFPointZero.Y, 0.0);
    TsgDXFText(vEntity).Height := vTextHW.Y;
    TsgDXFText(vEntity).Scale := GetScale;
    TsgDXFText(vEntity).ObliqueAngle := FSlant;
    TsgDXFText(vEntity).HAlign := FHAlign;
    TsgDXFText(vEntity).VAlign := 0;
    TsgDXFText(vEntity).Text := cnstSpaceText;
    FDXFConverter.Loads(vEntity);
    vBoxText := vEntity.Box;
    vTextHW.X := abs(vBoxText.Right-vBoxText.Left);
    vTextHW.Y := abs(vBoxText.Top-vBoxText.Bottom);
  finally
    FreeAndNil(vEntity);
  end;

  if FCommandParameters.Count = 0 then
  begin
    FCommandParameters.Add(0);
    FCommandParameters.Add(cnstSingle);
    vFlag := True;
    vMagikTemp := 68;
  end;
  if FCommandParameters.Count <> 2 then
    Exit;

  vPenPos := CommandPoint(0);
  SinCos(FTextRotation * Pi / 180, vSin, vCos);
  vPenPosX := vMagikTemp * vPenPos.X * vTextHW.X;
  vPenPosY := vMagikTemp * vPenPos.Y * vTextHW.Y;
  vPenPos.X := FPenPos.X + vPenPosX * vCos - vPenPosY * vSin;
  vPenPos.Y := FPenPos.Y + vPenPosY * vCos + vPenPosX * vSin;
  ConvertPoint(False,vPenPos,FPenPos);
  if vFlag then
  begin
    FTextPos.Point.Y := FPenPos.Y;
    FTextPos.Point.X := FTextPos.PointX;
    FPenPos.X := FTextPos.Point.X;
  end;
end;

procedure TsgHPGLReader.NewLayout;
begin
  if FEmpty and (FLayout <> nil) then
    Exit;
  Inc(FLayoutNum);
  FlushHatch;
  FFlat := nil;
  if FLayoutNum > 1 then
    FinishInsert;
  FLayout := ThpConverter(FDXFConverter).Layouts[FLayoutNum-1];
  if not Assigned(FLayout) then
  begin
    FLayout := TsgDXFLayout.Create;
    FDXFConverter.AddLayout(FLayout);
  end;
  FLayout.PlotSettings := GetPlotSettings;
  FLayout.Name := Format('Page%.2d', [FLayoutNum]);
  Loads(FLayout);
  FLayout.CADSpace := cs2D;
  FEmpty := True;
  FHatchIndex := 0;
end;

procedure TsgHPGLReader.NewPage;
begin
  if not FEmpty then begin
    FinishInsert;
    FLayout := nil;
  end;
end;

procedure TsgHPGLReader.PlotSize;
begin
  GetCommandParameters(False,True);
  if not FUsePSInstr then
    Exit;
  if FCommandParameters.Count <> 2 then
    Exit;
  FPage.X := FCommandParameters[FCommandParameters.Count - 2] / HPGLSteps;
  FPage.Y := FCommandParameters[FCommandParameters.Count - 1] / HPGLSteps;
  if FPage.Y > FPage.X then
    FRotation := 90;
end;

function TsgHPGLReader.ProcHPGLCmd(ACmd: Word): Boolean;

  procedure SetUpdateCarriage(const ACmd: Word);
  begin
    case ACmd of
      $425A, $4252, $4446, $494E, $5041,
      $5045, $5044, $5052, $5055, $524F,
      $4141, $4152, $4154, $4449, $4452,
      $4350, $4C4F, $434F: FUpdateCarriagePoint := True;
    end;
  end;

begin
  Result := True;
  SetUpdateCarriage(ACmd);
  case ACmd of
    $4250://BP
      begin
        InitPlot;
        BeginPlot;
      end;
    $425A://BZ
      AddSpline(False);
    $4252://BR
      AddSpline(True);
    $4349://CI
      AddCircle;
    $4446://DF
      InitPlot;
    $4454://DT
      AddDefineLabelTerm;
    $4550://EP
      AddPoly;
    $4541://EA
      AddRectangle(False);
    $4552://ER
      AddRectangle(True);
    $494E://IN
        InitPlot;
    $4950://IP
      InputPoints;
    $4C42://LB
      begin
        if FUpdateCarriagePoint then
        begin
          FTextPos.PointX := FPenPos.X;
          FUpdateCarriagePoint := False;
        end;
        AddText;
      end;
    $5041://PA
      begin
        FRelative := False;
        AddLine;
        UpdateTextPos;
      end;
    $5043://PC
      SetPenColor;
    $5045://PE
      AddPolyline;
    $5044://PD
      begin
        FPenDown := True;
        AddLine;
        UpdateTextPos;
      end;
    $504D://PM
      ProcPMCommand;
    $5052://PR
      begin
        FRelative := True;
        AddLine;
        UpdateTextPos;
      end;
    $5053://PS
       PlotSize;
    $5055://PU
      begin
        FPenDown := False;
        AddLine;
        UpdateTextPos;
      end;
    $5057://PW
      SetPenWidth;
    $524F: //RO
      Rotate;
    $5343://SC
      GetScales;
    $5350://SP
      SelectPen;
    $4141://AA
      AddArc(True);
    $4152://AR
      AddArc(False);
    $4154://AT
      AddArcThreePoint;
    $5047://PG
      NewPage;
    $4E52://NR
     if FInterpretHPGLCmdNRAsNewPageOnRead then NewPage;
    $4650://FP
      AddHatch;
    $5241://RA
      AddSolid(False);
    $5252://RR
      AddSolid(True);
    $5349://SI
      TextHeight;
    $4449://DI
      TextRotation;
    $4452://DR
      TextRotation;
    $4350://CP
      MoveChar;
    $4C54://LT
      SetLineType;
    $4D43://MC
      MergeControl;
    $4654://FT
      FillType;
    $4957://IW
      InputWindow;
    $4652://FR
      FrameAdvance;
    $4C4F://LO
      LabelOrigin;
    $534C://SL
      CharSlant;
    $5352://SR
      TextRelative;
    $434F://CO
      CommentHPGL;
    $5747://WG
      Wedge(True);
    $4557://EW
      Wedge(False);
    else
      while not (FPos^ in ['A'..#255,';',#12,#27,#0]) do Inc(FPos);
  end;
  DoProgress(psRunning);
end;

function TsgHPGLReader.ProcPJLCmp: Boolean;
const
  PJLs: array[1..6] of string = ('PAPERWIDTH', 'PAPERLENGTH', 'RESOLUTION',
    'LANGUAGE', 'RENDERMODE', 'EOJ');
var
  vCmdParams, vCmdParams2: string;
  vChar: AnsiChar;
  I, J: Integer;
  vNameAttrib, vValueAttrib: string;
begin
  Result := False;
  vCmdParams := Char(GetChar);
  if AnsiUpperCase(vCmdParams) <> 'P' then
  begin
    FindSymbol([#27, ';'], vChar);
    ReturnToSymbol([#27], vChar);
    Exit;
  end;
  for I := 1 to 2 do//FI:W528 ignore
    vCmdParams := vCmdParams + Char(GetChar);
  if AnsiUpperCase(vCmdParams) <> 'PJL' then
    Exit;
  vCmdParams := '';
  vChar := #0;
  while (vChar <> '@') and (vChar <> #13) and (vChar <> #10) and (vChar <> #27)
    and (not Eof) do
  begin
    vChar := GetChar;
    if ( (vChar = #9) or ((vChar >= #32) and (vChar < #64)) or ((vChar > #64) and (vChar <= #126))) then
      vCmdParams := vCmdParams + Char(vChar);
      DoProgress(psRunning);
  end;
  if vChar = '@' then
    Dec(FPos);
  // Add data to XML for PJL
  J := AnsiPos('=', vCmdParams);
  vNameAttrib := Trim(Copy(vCmdParams, 1, J-1));
  if AnsiPos('SET ', vNameAttrib) = 1 then
    vNameAttrib := Copy(vNameAttrib, 5, Length(vNameAttrib) - 4);
  vValueAttrib := Trim(Copy(vCmdParams, J+1, Length(vCmdParams) - J));
  if AnsiPos(' ', vNameAttrib) = 0 then
  begin
    TsgNode(FXMLParser.ROOT.Nodes[0]).AddAttribNV(vNameAttrib, vValueAttrib);
  end;

  for I := 1 to 5 do
    if AnsiPos(PJLs[I], vCmdParams) > 0 then
    begin
      J := AnsiPos('=', vCmdParams);
      if J = 0 then
        Exit;
      Delete(vCmdParams, 1, J);
      if vCmdParams[1] = ' ' then
        while vCmdParams[1] = ' ' do
          Delete(vCmdParams, 1, 1);
      vCmdParams2 := '';
      for J := 1 to Length(vCmdParams) do
        if IsNumber(vCmdParams[J]) then
          vCmdParams2 := vCmdParams2 + vCmdParams[J];
      J := StrToIntDef(vCmdParams2, MaxInt);
      if (I = 4) and (Copy(vCmdParams, 1, 3) = 'PCL') then
        FIsPCL := True;
      if J = MaxInt then
        Exit;
      case I of
        1:
        begin
          FPage.X := J * 1.411;
          Exit;
        end;
        2:
        begin
          FPage.Y := J * 1.411;
          Exit;
        end;
        3:
        begin
          FRTLRes := J;
          FRTLScale := HPGLSteps / FRTLRes;
          Exit;
        end;
        6:
          if Assigned(FStream) then
            FPos := FLimit;
      end;
    end;
  Result := True;
end;

procedure TsgHPGLReader.ProcPMCommand;

  procedure ClosePoligon;
  var
    vEntity: TsgDXFEntity;
    vFirstPoint, vLastPoint: TFPoint;
    vPolyPoints: TFPointList;
  begin
    if FPolyBuffer.Count = 1 then
    begin
      vEntity := TsgDXFEntity(FPolyBuffer.Last);
      if vEntity is TsgDXFLine then
      begin
        vEntity := Line2Poly(TsgDXFLine(vEntity));
        FPolyBuffer[FPolyBuffer.Count - 1] := vEntity;
      end;
      if vEntity is TsgCADBasePolyline then
      begin
        vPolyPoints := TsgCADBasePolyline(vEntity).PolyPoints;
        if vPolyPoints.Count > 0 then
        begin
          vFirstPoint := vPolyPoints.First;
          vLastPoint := vPolyPoints.Last;
          if (vFirstPoint.Z = 0) and ((vFirstPoint.X <> vLastPoint.X) or
            (vFirstPoint.Y <> vLastPoint.Y)) then
          begin
            vPolyPoints.Add(vFirstPoint);
          end;
        end;
      end;
    end;
  end;

var
  vPolygonMode: Integer;
begin
  FFlat := nil;
  FError := GetCommandParameters(False,True);
  if FError then
    Exit;
  if FCommandParameters.Count = 0 then
    vPolygonMode := 0
  else
    vPolygonMode := Round(FCommandParameters[0]);
  case vPolygonMode of
    0:
      begin
        FPolyMode := True;
        ClearPolyBuffer;
        FPenPos0 := FPenPos;
      end;
    1:
      begin
        if FPolyMode then
        begin
          FPenDown := False;
          FNextPointUp := True;
          FPolyBuffer.Add(TsgDXFVPort.Create);
          //FPenPos0 := FPenPos;
        end;
      end;
    2:
      begin
        if FPolyMode then
        begin
          ClosePoligon;
          FPolyMode := False;
          FPenPos := FPenPos0;
        end;
      end;
  end;
end;

function TsgHPGLReader.ProcRTLCmd: Boolean;
const
  Symbols = [#12, #27, '@', ';'];
var
  Ch,C1: AnsiChar;
  vRotateOld: Integer;
  vPenPosOldHPGL: TFPoint;
  vStr: string;
begin
  DoProgress(psRunning);
  Result := True;
  Ch := GetChar;
  vRotateOld := FPCLReader.FRotationPage;
  FPCLReader.FRotationPage := 0;
  vPenPosOldHPGL := FPenPos;
  try
    case Ch of
      'E':
        Exit;
      '.':
        begin
          GetChar;
          Exit;
        end;
      '%':
        begin
          FindSymbol(Symbols + ['1'..'9', '0', '-'] + ['A'..'Z','a'..'z'], Ch);
          C1 := Ch;
          vStr := Char(Ch);
          if Ch in ['A'..'Z','a'..'z'] then
            vStr := ''
          else
            while True do
            begin
              Ch := GetChar;
              if FPos >= FLimit then
                Exit;
              if Ch in ['A'..'Z','a'..'z'] then
                Break;
              vStr := vStr + Char(Ch);
            end;
          FStart := FPos;
          case Ch of
            'X':
              begin
                //Universal Exit Language/Start of PJL
                if vStr = '-12345' then
                begin
                  // Read @
                  GetChar;
                  ProcPJLCmp;
                end;
              end;
            'B':
              begin
                if vStr = '' then
                  C1 := '0';
                case C1 of
                  '-':
                    begin
                      if vStr = '-1' then
                      begin
                        //For parameter value -1, the HP-GL/2
                        //context behaves as a stand-alone plotter,
                        //except that the ESCE, Reset and ESC%#A,
                        //Enter RTL Mode commands arerecognized.
                      end;
                    end;
                  '1': FPenPos := MakeFPoint(FPCLReader.FPenPosition.X{ * FRTLScale},

                    FPCLReader.FPenPosition.Y{ * FRTLScale}, 0);
                  '0': FPenPos := vPenPosOldHPGL;
                end;
                FPCLReader.FUseHPGLCAP := False;
                Exit;
              end;
            'A':
              begin
                FPCLReader.FUseHPGLCAP := False;
                if C1 = '1' then
                begin
                  FPCLReader.FPenPosition := MakeFPoint(FPenPos.X{ / FRTLScale},
                    FPenPos.Y{ / FRTLScale}, 0);
                  FPCLReader.FUseHPGLCAP := True;
                end;
                if FPos^ = #12 then
                begin
                  NewLayout;
                  //InitPlot;
                end;
              end;
          end;
          FindSymbol(Symbols, Ch);
        end;
      '*':
        if not FPCLReader.Process then FindSymbol([#27], Ch);
      '(':
        if not FPCLReader.Font then FindSymbol([#27], Ch);
      ')':
        if not FPCLReader.Font then FindSymbol([#27], Ch);
      '&':
        if not FPCLReader.Ampersand then FindSymbol([#27], Ch);
    else
      FindSymbol(Symbols, Ch);
    end;
    ReturnToSymbol(Symbols, Ch);
    FStart := FPos;
  finally
    FPCLReader.FRotationPage := vRotateOld;
  end;
end;

procedure TsgHPGLReader.RemoveInsert;
var
  I: Integer;
begin
  FDXFConverter.Sections[csBlocks].RemoveEntity(FInsert.Block);
  if FLayout <> nil then
  begin
    FLayout.PaperSpaceBlock.RemoveEntity(FInsert);
    for I := 0 to FInsert.Block.Count - 1 do
      FLayout.AddEntity(FInsert.Block.Entities[I]);
  end;
  FInsert.Block.Clear(False);
  FInsert.Block.Free;
  FInsert.Free;
end;

procedure TsgHPGLReader.ReturnToSymbol(const ASymbols: TsgHPGLSymbols;
  const ASymbol: AnsiChar);
begin
  if ASymbol in ASymbols then
    while (FPos >= FMem) and (not (FPos^ in ASymbols)) do
      Dec(FPos);
end;

procedure TsgHPGLReader.Rotate;
var
  vRotation: Integer;

  function Scale(const APoint: TFPoint): TFPoint;
  begin
    Result.X := APoint.X * FScale.X;
    Result.Y := APoint.Y * FScale.Y;
    Result.Z := 0;
  end;

begin
  FError := GetCommandParameters(False,True);
  if FError then
    Exit;
  if FCommandParameters.Count > 0 then
    vRotation := Round(FCommandParameters[0])
  else
    vRotation := 0;
  FRotation := vRotation;
  UpdateClipData;
end;

procedure TsgHPGLReader.SelectPen;
begin
  FError := GetCommandParameters(False,True);
  if FError or (FCommandParameters.Count = 0) then
    Exit;
  UpdatePen(Round(FCommandParameters[0]));
end;

procedure TsgHPGLReader.SetLineType;
var
  I: Integer;
begin
  FError := GetCommandParameters(False,False);
  if FError then
    Exit;
  if FCommandParameters.Count = 0 then
    I := -1
  else
    I := Round(FCommandParameters[0]) + 8;
  if (I < 0) or (I > 16) then
    FLType := nil
  else
    FLType := FDXFConverter.LTypes[I];
end;

procedure TsgHPGLReader.SetPenColor;
var
  vIndexPen: Integer;
  vRed, vGreen, vBlue: Byte;
begin
  FError := GetCommandParameters(False,True);
  if FError then
    Exit;
  if FCommandParameters.Count = 0 then
    Exit;
  vIndexPen := Round(FCommandParameters[0]);
  if FCommandParameters.Count = 1 then
    case vIndexPen of
      0..7: FHPPens[FCurPenNum].Color := GetDefaultPenColor(vIndexPen);
    end
  else
  begin
    if (FCommandParameters.Count < 4) or (not CheckIndexPen(vIndexPen)) then
      Exit;
    vRed := Round(FCommandParameters[1]);
    vGreen := Round(FCommandParameters[2]);
    vBlue := Round(FCommandParameters[3]);
    FHPPens[vIndexPen].Color := ConvertRGBtoColor(vRed, vGreen, vBlue);
  end;
  if FHPPens[vIndexPen].Color = 0 then
    FHPPens[vIndexPen].Color := clHpglNone;
end;

procedure TsgHPGLReader.SetPenWidth;
var
  vIndexPen: Integer;
  vPenWidth: Single;
begin
  FError := GetCommandParameters(False,True);
  if FError or (FCommandParameters.Count = 0) then
    Exit;
  if FCommandParameters.Count = 0 then
    vPenWidth := 0.35
  else
    vPenWidth := FCommandParameters[0];
  if FCommandParameters.Count = 1 then
    vIndexPen := -1
  else
    vIndexPen := Round(FCommandParameters[1]);
  if CheckIndexPen(vIndexPen) then
    FHPPens[vIndexPen].Width := vPenWidth
  else
    for vIndexPen := 0 to 255 do
      FHPPens[vIndexPen].Width := vPenWidth;
end;

procedure TsgHPGLReader.SetScale(const AReal1, AReal2, AUser1,
  AUser2: TFPoint; var AScale: TFPoint);
begin
  FScale := MakeFPoint(1.0, 1.0, 0.0);
  if (AUser2.X = AUser1.X) or (AUser2.Y = AUser1.Y) then
    Exit;
  if (AReal2.X = AReal1.X) or (AReal2.Y = AReal1.Y) then
    Exit;
  AScale := MakeFPoint((AReal2.X - AReal1.X) / (AUser2.X - AUser1.X),
    (AReal2.Y - AReal1.Y) / (AUser2.Y - AUser1.Y), 0.0);
end;

procedure TsgHPGLReader.SimpleColors(AParametrs: Integer);
const
  KCMY: array[0..15] of TColor = (clWhite,clWhite,clAqua,clAqua,clFuchsia,clFuchsia,clBlue,clBlue,
  				 clYellow,clYellow,clLime,clLime,clRed,clRed,clBlack,clBlack);
  KCMY_Black: array[0..15] of TColor = (clWhite,clBlack,clWhite,clBlack,
    clWhite,clBlack,clWhite,clBlack,
    clWhite,clBlack,clWhite,clBlack,
    clWhite,clBlack,clWhite,clBlack);
  CMY: array[0..7] of TColor = (clWhite,clAqua,clFuchsia,clBlue,clYellow,clLime,clRed,clBlack);
  K: array[0..1] of TColor = (clWhite,clBlack);
  RGB: array[0..7] of TColor = (clHpglNone,clRed,clLime,clYellow,clBlue,clFuchsia,clAqua,clWhite);

  procedure SetPal(const APalette: array of TColor);
  var
    I: Integer;
  begin
    for I := 0 to High(APalette) do
      FHPPens[I].Color := APalette[I];
  end;

begin
  case AParametrs of
    -4: SetPal(KCMY_Black);//SetPal(KCMY);
    -3: SetPal(CMY);
     1: SetPal(K);
     3: SetPal(RGB);
  end;
end;

procedure TsgHPGLReader.TextHeight;
begin
  GetCommandParameters(False,True);
  if FCommandParameters.Count < 2 then
  begin
    FTextHeight := HPTextHeight / 40;
    FTextWidth := 0;
  end
  else
  begin
    FTextHeight := FCommandParameters[1] * 10;//  1016 * 1.05 / 40 / 2.54;
    //FTextWidth := 2* Single(FCmdPar[0]) / Single(FCmdPar[1]);
    if FCommandParameters[1] <> 0 then
      FTextWidth := 1.4 * FCommandParameters[0] / FCommandParameters[1]
    else
      FTextWidth := 0;
  end;
  FIsSR := False;
end;

procedure TsgHPGLReader.TextRelative;
const
  vMagValue = 1/40;
var
  vTextWidth, vTextHeight: Single;
begin
  GetCommandParameters(False,True);
  if FCommandParameters.Count < 2 then
  begin
    vTextWidth := 0.75;
    vTextHeight := 1.5;
  end
  else
  begin
    vTextWidth := Abs(FCommandParameters[0]);
    vTextHeight := Abs(FCommandParameters[1]);
  end;
  FTextHeight := vTextHeight / 100 * vMagValue;
  FTextWidth := {fWinTextHeightFactor *} (vTextWidth / 100)* vMagValue;
  FIsSR := True;
end;

procedure TsgHPGLReader.TextRotation;
var
  vPoint: TFPoint;
begin
  GetCommandParameters(False,True);
  if FCommandParameters.Count < 2 then
    FTextRotation := 0
  else
  begin
    vPoint := CommandPoint(0);
    FTextRotation := ArcTan2(vPoint.Y, vPoint.X) * 180 / Pi;
  end;
end;

procedure TsgHPGLReader.UpdatePen(ANumberPen: Integer);
begin
  if not CheckIndexPen(ANumberPen) then
    ANumberPen := 1;
  if ANumberPen <> FCurPenNum then
  begin
    FlushHatch;
    FFlat := nil;
  end;
  FCurPenNum := ANumberPen;
end;

procedure TsgHPGLReader.UpdateClipData;
var
  vClipBox: TFRect;
  vTempPoint: TFPoint;

  function Scale(const APoint: TFPoint): TFPoint;
  begin
    Result.X := APoint.X * FScale.X;
    Result.Y := APoint.Y * FScale.Y;
    Result.Z := 0;
  end;
begin
  if Assigned(FInsert) then
  begin
    vClipBox := BadRect;
    if FScaling then
    begin
      ExpandFRect(vClipBox, ConvertPoint(False, Scale(FLowerLeftPoint), vTempPoint));
      ExpandFRect(vClipBox, ConvertPoint(False, Scale(FUpperRight), vTempPoint));
    end
    else
    begin
      ExpandFRect(vClipBox, ConvertPoint(False, (FLowerLeftPoint), vTempPoint));
      ExpandFRect(vClipBox, ConvertPoint(False, (FUpperRight), vTempPoint));
    end;
    FInsert.Block.Offset := MakeFPoint(vClipBox.Left, vClipBox.Bottom, 0);
    FInsert.Point := FInsert.Block.Offset;
    FInsert.ClipBox := vClipBox;
  end;
end;

procedure TsgHPGLReader.UpdateTextPos;
begin
  FTextPos.PenPos := FPenPos;
  FTextPos.Point := FPenPos;
  FTextPos.PointX := FPenPos.X;
end;

constructor TsgHPGLReader.Create(Conv: TsgDXFConverter; S: TStream);
var
  vPJL : TsgNode;
begin
  FDXFConverter := Conv;
  TsgHPGLConverter(FDXFConverter).FHPGLReader := Self;
  FScale := cnstFPointSingle;
  FStream := S;
  FPSpace := TsgDXFSection.Create;
  FCommandParameters := TsgDoubleList.Create(0);
  FPolyBuffer := TsgObjectList.Create;
  FPCLReader := TpclReader.Create(Self);
  FPolyIndex := -1;
  FUsePSInstr := False;
  FRTLRes := 300;
  FMeasure := 1016.0;
  FEmpty := True;
  InitPlot;
  FSCCom := cnstFPointSingle;
  FXMLParser := TsgParser.Create;
  vPJL := TsgNode.Create;
  vPJL.Name := 'PJL';
  FXMLParser.ROOT.Add(Pointer(vPJL));
  FPlotSttings := nil;
end;

destructor TsgHPGLReader.Destroy;
begin
  TsgHPGLConverter(FDXFConverter).FHPGLReader := nil;
  FPCLReader.Free;
  FPSpace.Free;
  FCommandParameters.Free;
  ClearPolyBuffer;
  FPolyBuffer.Free;

  
//  FXMLParser.SaveToFile('D:\PJL.xml');

  
  FXMLParser.Free;
  FPlotSttings.Free;
  inherited Destroy;
end;

procedure TsgHPGLReader.ReadFile;

  procedure ProcCommand;
  var
    vChar: AnsiChar;
  begin
    FStart := FPos;
    FUpdateCarriagePoint := True;
    repeat
{$IFDEF SG_HPGL2_DEBUG}
    TsgHPGLConverter(FDXFConverter).FStackCommands.Add(Pointer(FPos));
{$ENDIF}
      vChar := GetChar;
      case vChar of
      #0:
        if Eof then Break;
      #27:
        ProcRTLCmd;
      #12:
        NewLayout;
      '@':
        ProcPJLCmp;
      'A'..'Z','a'..'z':
        try
          FHPGLCmd := Ord(vChar) and not $20;
          vChar := GetChars;
          if vChar = #27 then
            Dec(FPos)
          else
            if vChar in ['A'..'Z','a'..'z'] then
            begin
              FHPGLCmd := FHPGLCmd shl 8 + Ord(vChar) and not $20;
              ProcHPGLCmd(FHPGLCmd);
            end;
        except on EConvertError do
          FError := True;
        end;
      end;
    until FPos >= FLimit;
{$IFDEF SG_HPGL2_DEBUG}
    TsgHPGLConverter(FDXFConverter).SaveStackCommand;
{$ENDIF}
  end;

  procedure CheckAndIncFPos(const ABeginStr, AEndStr: AnsiString);
  var
    vFMem: PAnsiChar;
    vPos: Integer;
  begin
    vFMem := PAnsiChar(FMem);
    vPos := StringPosA(ABeginStr, AnsiString(vFMem),1);
    if vPos = 1 then
    begin
      vPos := StringPosA(AEndStr, AnsiString(vFMem), 1);
      if vPos > 0 then
        Inc(FPos, vPos + Length(AEndStr));
    end;

  end;

  procedure SetFPos(const AMemorySream: TCustomMemoryStream);
  begin
    FPos := FMem;
    FDXFConverter.Source := AMemorySream;
    FLimit := FMem + AMemorySream.Size;
    CheckAndIncFPos('##HDI START', '##HDI END');
    CheckAndIncFPos('BEGMF', 'ENDMF');
  end;

var
  vMemorySream: TCustomMemoryStream;
  P1, P2: PAnsiChar;
  vPossiblyUnicode: Boolean;
{$IFDEF SG_NON_WIN_PLATFORM}
  vSourceStreamPos: Int64;
{$ENDIF}
begin
  if not Assigned(FStream) then
    Exit;

  FTerminator := ';';
  FDXFConverter.SetSourceStream(FStream);
  vMemorySream := FDXFConverter.Source;

  ExtractFirstFileSatisfiesCondition(FStream, vMemorySream, cnstHPGLExt);

  FMem := vMemorySream.Memory;
  try
{$IFNDEF SG_FIREMONKEY}
    FLast := GetTickCount and not $FF;
{$ENDIF}
    FPos := FMem;
    DoProgress(psStarting);
    InitPlot;
    FHPPens[0].Color := clHpglNone;
    FError := False;
    vPossiblyUnicode := False;
    if vMemorySream.Size > 4 then
    begin
      P1 := PAnsiChar(vMemorySream.Memory);
      P2 := P1;
      FLimit := FMem + vMemorySream.Size - 4 ;
      repeat
        if P1^ <> #0 then
        begin
          P2 := P1;
          Inc(P2);
          if P2^ <> #0 then
            Break;
          vPossiblyUnicode := True;
          Inc(P1,2);
          Inc(P2,2);
          Break;
        end;
        Inc(P1);
      until P1 >= FLimit;
      if vPossiblyUnicode and (P1^ = AnsiChar(#255)) or (P2^ = #0) then
      begin
        vMemorySream := TMemoryStream.Create;
{$IFNDEF SG_NON_WIN_PLATFORM}
        vMemorySream.Size := UnicodeFromLocaleChars(0, 0, FDXFConverter.Source.Memory,
          FDXFConverter.Source.Size shr 1, nil, 0);
        UnicodeFromLocaleChars(0, 0, FDXFConverter.Source.Memory,
          FDXFConverter.Source.Size shr 1, vMemorySream.Memory, vMemorySream.Size);
{$ELSE}
        vSourceStreamPos := FDXFConverter.Source.Position;
        FDXFConverter.Source.Position := 0;
        vMemorySream.CopyFrom(FDXFConverter.Source, FDXFConverter.Source.Size);
        vMemorySream.Position := 0;
        FDXFConverter.Source.Position := vSourceStreamPos;
{$ENDIF}
        FMem := vMemorySream.Memory;
        FDXFConverter.OwnSource := True;
      end;
    end
    else
    begin
      DoProgress(psEnding);
      Exit;
    end;
    SetFPos(vMemorySream);
    ProcCommand;
    FlushHatch;
    if FPCLReader.Count > 0 then
      FPCLReader.EndData;
    FinishInsert;
  finally
    FMem := nil;
    FPos := nil;
  end;
  DoProgress(psEnding);
end;

initialization
{$IFNDEF SG_NON_WIN_PLATFORM}
  CF_HPGL := RegisterClipboardFormat('SoftGold HPGL Image');
{$ENDIF}
  TPicture.RegisterClipboardFormat(CF_HPGL, TsgHPGLImage);
  TPicture.RegisterFileFormat('pcl', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('plt', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hgl', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hg', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hpg', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('plo', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hp', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hpp', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hp1', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hp2', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hp3', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hpgl', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('hpgl2', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('gl', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('gl2', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('prn', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('spl', SHPGLImage, TsgHPGLImage);
  TPicture.RegisterFileFormat('rtl', SHPGLImage, TsgHPGLImage);
{$IFDEF SG_XREF_EXTENDED}
  TsgDXFConverterAccess.RegisterXRefGraphicClass('pcl', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('plt', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hgl', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hg', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hpg', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('plo', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hp', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hpp', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hp1', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hp2', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hp3', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hpgl', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('hpgl2', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('gl', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('gl2', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('prn', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('spl', TsgHPGLImage);
  TsgDXFConverterAccess.RegisterXRefGraphicClass('rtl', TsgHPGLImage);
{$ENDIF}

finalization
  TPicture.UnRegisterGraphicClass(TsgHPGLImage);

end.
