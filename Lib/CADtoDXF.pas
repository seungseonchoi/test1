{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{               Export CAD to DXF                            }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit CADtoDXF;
{$INCLUDE SGDXF.inc}

{$IFNDEF SG_ASSEMBLER32}
  {$DEFINE NOASM}
{$ENDIF}
{$IFDEF SG_FM_MOBILE}
  {$DEFINE NOASM}
{$ENDIF}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows, ActiveX,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
  Classes, SysUtils, CADExport, CADImage, DXFConv, sgConsts, ExtData,
  sgBitmap, sgUnicode, sgLists, CADDirectExport
{$IFDEF SGDEL_6}
  , DateUtils
{$ENDIF}
{$IFDEF SG_BTI}
  , BTICommon
{$ENDIF}
{$IFDEF SG_ENCRYPT_FILES}
  , sgStmCrpt
{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF}
  ;

type
  TsgCADtoDXF = class(TsgCADDirectExport)
  private
    FNewBlockRecordList: TStringList;
    FBlockRecordsListExported: TsgList;
    FRenamedBlockRecords: TStringList;
    FContentOleByImage: Boolean;
    FHasViewPortWindow: Boolean;
    FPaperMode: Boolean;
    FStream: TStream;
    FAppIDs: TObject;
    FHandleCode: Integer;
    FXInfoOffs: Int64;
    FCounts: array[TsgCADEntities] of Integer;
    FBlockID: Integer;
    FACISEntities: TsgObjectList;
    procedure AddBlockName(const AName: string);
    procedure AddObj(const AName: string; AHandle, AOwner: UInt64);
    function AddDictRefs(const AName: string; const AHandle: UInt64;
      const AHard: Boolean = False): UInt64; overload;
    function AddDictRefs(const AEntity: TsgDXFEntity;
      const AHard: Boolean = False): UInt64; overload;
    function AddDictNamed(const AName, ASubClassName: string;
      const AHandle: UInt64; const AParent: UInt64 = 0): UInt64;
    function AddDict(const AHandle: UInt64; const AParent: UInt64 = 0): UInt64;
    function ExportDictionaryVariables(ADictionaryVariablesDictionaryHandle: UInt64): Boolean;
    function CreateBlockRecord(const ABlock: TsgDXFBlock;
      const AName: string): TsgDXFBlockRecord;
    procedure CheckBlockName(var AName: string; var AFlags: Byte);
    function GetFlagsByName(const AFlags: Integer; const AName: string) : Integer;
    function GetBlockRecordByBlock(const ABlock: TsgDXFBlock;
      const AName: string): TsgDXFBlockRecord;
    function GetBlockRecord(const ABlock: TsgDXFBlock): TsgDXFBlockRecord;
    function GetEntityLayerName(const AEntity: TsgDXFEntity): string;
    function GetHandleByEntity(AEntity: TsgDXFEntity): UInt64;
    function GetHandleByEntityNoCheck(AEntity: TsgDXFEntity): UInt64;
    procedure ProcBlockRecord(ABlock: TsgDXFBlock; const AName: string; Data: Pointer;
      ACurrPaperSpaceIndex: Integer = -1);
    procedure ProcBlock(ABlock: TsgDXFBlock; const AName: string; Data: Pointer;
      ACurrPaperSpaceIndex: Integer = -1);
    function ConvertDWGExtDataToDXF(const AExtData: TsgCADExtendedData): Boolean;
    function ExportExtearnalEED(const AEntHandle: UInt64; AFilter: TsgInt64List = nil): Boolean;
    function ExportKnownEED(const AEntity: TsgDXFEntity; AFilter: TsgInt64List = nil): Boolean;
    procedure DoExportEED(const AExtData: TsgCADExtendedData);
    function GetEntityName(const AEntity: TsgDXFEntity; const ADefaultName: string): string;
    procedure UpdateXInfo(AStream: TStream; AXInfoOffs: {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF});
    procedure ExportExtendedDictionaries;
    procedure PrepareExportText(var AText: string);
    procedure AddTransparency(const AValue: Single);
  protected
    class function ACIS_Row_Encode(const ADecoded: AnsiString): AnsiString; virtual;
    procedure Add2DFlatPoint(const Code: Integer; const Pt: TFPoint);
    procedure Add2DPoint(const Code: Integer; const Pt: TF2DPoint);
    procedure Add3DPoint(const Code: Integer; const Pt: TFPoint);
    procedure Add3DPointExtruded(const Code: Integer; const Pt, Extr: TFPoint);
    procedure Add3DPointPreExtruded(const Code: Integer; const Pt, Extr: TFPoint);
    procedure AddCode(const AValue: Integer); virtual;
    procedure AddCommonEntityGroup(AEntity: TsgDXFEntity);
    procedure AddCommonEntityGroupEx(AEntity: TsgDXFEntity; const AName, ASubName: string);
    procedure AddCommonTableGroup(AEntity: TsgDXFEntity; const ABaseName, ASub, AName: string;
      const AFlags: Integer);
    procedure AddDXFColor(const AValue: TsgColorCAD;
      const AVisible: Boolean = True; const AditionalIndex: Byte = 0);
    procedure AddEntName(AEntity: TsgDXFEntity; const AName: string;
      ALayerName: string = '');
    procedure AddExtrusion(const Pt: TFPoint);
    procedure AddFloat(const Code: Integer; Value: Double); virtual;
    procedure AddHandle(AEntity: TsgDXFEntity; const AParentHandle: UInt64;
      const AReactors: array of UInt64; const AXDictionary: UInt64 = cnstBadHandle);
    procedure AddHexInt(const Code: Integer; const Value: UInt64);
    procedure AddInt(const Code, Value: Integer); virtual;
    procedure AddLineWeight(const AValue: Double);
    procedure AddLineType(ALineType: TsgDXFLineType);
    procedure AddString(const Code: Integer; const Str: string); virtual;
    procedure AddStringBig(const ACode, ASubCode, AExtCode: Integer;
      const AString: string);
    procedure AddRawByteString(const Code: Integer;
      const ARawByteString: sgRawByteString); virtual;
    procedure AddSubclassMarker(const Str: string);
    procedure AddThickness(const AValue: Double);
    procedure BeforeDestroy(var AClearing: Boolean); override;
    procedure BeginObject(const AName: string);
    procedure BeginSection(const AName: string);
    procedure BeginTable(AGroup: TsgDXFGroup; const AName: string);
    function CorrectLineWeight(const ALineWeight: Double;
      ANeedToConvert: Boolean): Integer; override;
    procedure DoExport(const E: TsgDXFEntity; const Data: PsgDirectExportData = nil); override;
    procedure DoAddReactors(const ACode: SmallInt; const AReactorName: string;
      const AReactors: array of UInt64);
    procedure EndOfFile(APos: {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF});
    procedure EndSection;
    procedure EndTable;
    function Export3DFace(F3D: TsgDXF3DFace): Boolean;
    function ExportAcadTable(AcadTable: TsgDXFAcadTable): Boolean;
    function ExportACISEntity(ACIS: TsgBrepModAcis): Boolean;
    function ExportSurface(AEnt: TsgDXFSurface): Boolean;
    function Export3dSolid(AEnt: TsgDXF3dSolid): Boolean;
    function ExportArc(A: TsgDXFArc): Boolean;
    function ExportCircle(C: TsgDXFCircle): Boolean;
    function ExportDimension(D: TsgDXFDimension): Boolean;
    function ExportEllipse(E: TsgDXFEllipse): Boolean;
    function ExportEntityAsGroup(const E: TsgDXFEntity): Boolean;
    function ExportFlatHatch(FH: TsgFlatHatch): Boolean;
    function ExportFlatPoly(FP: TsgFlatPoly): Boolean;
    function ExportHatch(PP: TsgCADPolyPolygon): Boolean;
    function ExportCADFill(const ACADFill: TsgCADFill): Boolean;
    function ExportInsert(INS: TsgDXFInsert): Boolean;
    function ExportImageEnt(W: TsgDXFImageEnt): Boolean;
    function ExportImageEntByFile(W: TsgDXFImageEnt): Boolean;
    function ExportImageEntByOle(W: TsgDXFImageEnt): Boolean;
    function ExportLeader(L: TsgDXFLeader): Boolean;
    function ExportLine(L: TsgDXFLine): Boolean;
    function ExportMText(MT: TsgDXFMText): Boolean;
    function ExportMLeader(ML: TsgCADMultiLeader): Boolean;
    function ExportMLine(M: TsgCADMLine): Boolean;
    function ExportMesh(M: TsgDXFMesh): Boolean;
    function ExportOle2Frame(OLE2: TsgDXFOle2Frame; Data: TObject = nil): Boolean;
    function ExportPoint(P: TsgDXFPoint): Boolean;
    function ExportPolyLine(BasePL: TsgCADBasePolyline): Boolean;
{$IFDEF SG_BTI}
    function ExportPolyPolyline2D(PL2D: TsgCADPolyPolyline2D): Boolean;
{$ENDIF}
    function ExportChildEntities(AEntity: TsgDXFEntity): Boolean;
    function ExportRay(Ray: TsgDXFRay): Boolean;
    function ExportShape(S: TsgDXFShape): Boolean;
    function ExportSolid(S: TsgDXFSolid): Boolean;
    function ExportSpatial(AFilter: TsgCADSpatialFilter): Boolean;
    function ExportSpline(SPL: TsgDXFSpline): Boolean;
    function ExportText(T: TsgDXFText): Boolean;
    function ExportTolerance(T: TsgDXFTolerance): Boolean;
    function ExportViewPort(V: TsgDXFViewPort): Boolean;
    function ExportWipeOut(W: TsgCADCustomRectangle; AData: TObject = nil): Boolean;
    function ExportExtendedData(const AEntity: TsgDXFEntity): Boolean;
    procedure ExportSectionBLOCKS;
    procedure ExportSectionCLASSES;
    procedure ExportSectionENTITIES;
    function  ExportSectionHEADER: Integer;
    procedure ExportSectionOBJECTS;
    procedure ExportSectionTABLES;
    procedure ExportSectionACDSDATA;
    procedure ExportTablesAPPID;
    procedure ExportTablesBLOCK_RECORD;
    procedure ExportTablesDIMSTYLE;
    procedure ExportTablesLAYER;
    procedure ExportTablesLTYPE;
    procedure ExportTablesSTYLE;
    procedure ExportTablesVPORT;
    procedure ExportTablesVIEW;
    procedure ExportTablesUCS;
    procedure ExportXrecord(X: TsgDXFXRecord);
    function GetStringDXFVersion: string;
    function GetCorrectBlockName(const AName: string): string; override;
    procedure PrepareDrawingDatabase; override;
    procedure PrepareBlock(ABlockRecord: TsgDXFBlockRecord); override;
    procedure ProcessExport(Stream: TStream); override;
    procedure SetOleFrameVariable(const AValue: Integer); override;
    function WriteBuffer(const ABuffer; ACount: Integer): Integer; virtual;
    function WriteBytes(const ABuffer: TBytes): Integer; virtual;
    function WriteBytesArray(const ABuffer: array of Byte): Integer; virtual;
    function WriteRawByteStr(const AString: sgRawByteString): Integer; virtual;
    procedure WriteStr(const AString: string); virtual;
    function CreateNewEntity(const AEntClass: TsgDXFEntityClass; const AName: string): TsgDXFEntity;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    procedure Clear; override;
    class function ExportType: TsgExportFormat; override;
  end;

  TsgCADtoBin = class(TsgCADtoDXF)
  protected
    class function ACIS_Row_Encode(const ADecoded: AnsiString): AnsiString; override;
    procedure AddCode(const AValue: Integer); override;
    procedure AddFloat(const Code: Integer; Value: Double); override;
    procedure AddInt(const Code, Value: Integer); override;
    procedure AddString(const Code: Integer; const Str: string); override;
    function WriteRawByteStr(const AString: sgRawByteString): Integer; override;
    procedure SaveToStreamCustom(S: TStream); override;
  end;

implementation

uses
  Math, sgFunction, sgLines;

const
  cnstBufferSize = 1048576;
  // Constants
  cnstHexDigits = 16;
  sEnter = #13#10;
  // DXF Codes
  cFlag_70          = 70;
  cFloat_40         = 40;
  cHandle_5         = 5;
  cHandle_105       = 105;
  cParentHandle_330 = 330;
  cntCodeSoftOwnerHandle = 350;
  cntCodeHardOwnerHandle = 360;
  cntCodeControl = 102;
  cnstXInfoZero: UInt64 = 0;

  HandleRefCodes: array[0 .. 1] of Integer = (cntCodeSoftOwnerHandle, cntCodeHardOwnerHandle);

  sByBlock = 'ByBlock';
  sByLayer = 'ByLayer';

  { TODO: Group 1
    Uncomment 'ACAD_TABLE' and 'Table' }
  // See unit sgConsts.pas "The entities to be drawn"
  sCADEntityNames: array[TsgCADEntities] of string = ('#ENTITY', 'POINT', 'LINE',
     'SOLID', 'TRACE', 'CIRCLE', 'ARC', 'ELLIPSE', 'POLYLINE', 'LWPOLYLINE', 'SPLINE', 'SPLINE'{'HELIX'},
     'LEADER', 'INSERT', 'DIMENSION', 'TOLERANCE', 'MTEXT', 'SHAPE', 'TEXT', 'ATTDEF', 'ATTRIB',
     '3DFACE', 'IMAGE', 'VIEWPORT', 'REGION', 'BODY', '3DSOLID',
     '#PATTERN', 'HATCH', 'OLE2FRAME', 'HATCH', 'HATCH', 'HATCH', 'HATCH',
     'INSERT'{'ACAD_TABLE'}, 'LWPOLYLINE'{FlatPoly}, 'HATCH'{FlatHatch}, 'XREF', 'ACAD_PROXY_ENTITY',
     'POLYLINE', 'WIPEOUT', 'MLINE', 'MPOLYGON'{$IFDEF SG_BTI}, 'POLYLINE'{$ENDIF},
     'SURFACE', 'ENDSEC', '#ENTITY', '3DSOLID', '3DSOLID', '3DSOLID', '3DSOLID', 'POLYLINE'{FlatPoly3D},
     'RAY', 'XLINE', 'MULTILEADER', 'MESH', '3DSOLID','ENTITY3D');
  sCADEntitySubNames: array[TsgCADEntities] of string = ('#Entity', 'Point', 'Line',
     'Trace', 'Trace', 'Circle', 'Circle', 'Ellipse', '3dPolyline', 'Polyline', 'Spline', 'Spline'{'Helix'},
     'Leader', 'BlockReference', 'Dimension', 'Fcf', 'MText', 'Shape', 'Text', 'Text', 'Text',
     'Face', 'RasterImage', 'Viewport', 'ModelerGeometry', 'ModelerGeometry', 'ModelerGeometry',
     '#Pattern', 'Hatch', 'Ole2Frame', 'Hatch', 'Hatch', 'Hatch', 'Hatch',
     'BlockReference'{'Table'}, 'Polyline'{FlatPoly}, 'Hatch'{FlatHatch}, 'Xref', 'ProxyEntity',
     '3dPolyline', 'Wipeout', 'Mline', 'MPolygon' {$IFDEF SG_BTI}, '3dPolyline'{$ENDIF},
     'ModelerGeometry', 'Endsec', '#Entity', 'ModelerGeometry', 'ModelerGeometry', 'ModelerGeometry',
     'ModelerGeometry', '3dPolyline'{FlatPoly3D}, 'Ray', 'Xline', 'MLeader', 'SubDMesh',
     'ModelerGeometry','Entity3D');

  cnstDictSubClass = 'Dictionary';

type
  TsgCADImageEx = class(TsgCADImage);
  TsgCADConverter = class(TsgDXFConverter);
  Tsg2DPolylineEx = class(Tsg2DPolyline);
  Tsg2DSplineEx = class(Tsg2DSpline);
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgDXFBlockEx = class(TsgDXFBlock);
  TsgDXFInsertEx = class(TsgDXFInsert);
  TsgDXFDimensionEx = class(TsgDXFDimension);
  TsgDXfDimStyleAccess = class(TsgDXFDimensionStyle);
  TsgDXFMTextEx = class(TsgDXFMText);
  TsgDXFTextEx = class(TsgDXFText);
  TsgDXFToleranceEx = class(TsgDXFTolerance);
  TsgDXFVertexEx = class(TsgDXFVertex);
{$IFDEF SG_BTI}
  TsgAreaFigureAccess = class(TsgAreaFigure);
  TsgExtendedBlockDataEx = class(TsgExtendedBlockData);
{$ENDIF}
  TsgDXFTableAccess = class(TsgDXFTable);
  TsgDXFImageEntAccess = class(TsgDXFImageEnt);
  TsgDXFImageDefAccess = class(TsgDXFImageDef);
  TsgBitmapAccess = class(TsgBitmap);
  TsgAcadTableCellAccess = class(TsgAcadTableCell);
  TsgDXFPlotSettingsAccess = class(TsgDXFPlotSettings);
  TsgDXFBlockAccess = class(TsgDXFBlock);
  TsgDXF3dFaceAccess = class(TsgDXF3dFace);
  TsgDXFBlockRecordAccess = class(TsgDXFBlockRecord);
  TsgCADExtendedDataAccess = class(TsgCADExtendedData);
  TsgCADHatchAccess = class(TsgCADHatch);
  TsgCADMultiLeaderAccess = class(TsgCADMultiLeader);
  TsgBrepModEntityAccess = class(TsgBrepModEntity);

  TsgCADDirectExportAccess = class(TsgCADDirectExport);

  TAppIDs = class(TsgList)
  private
    FSortedList: TsgStringList;
    FSearchAppID: TsgDXFOwneredItem;
    FCADExport: TsgCADDirectExportAccess;
    function GetEnt(Index: Integer): TsgDXFEntity;{$IFDEF USE_INLINE} inline;{$ENDIF}
    procedure SetEnt(Index: Integer; const Value: TsgDXFEntity);
    function GetEntity(const AName: string): TsgDXFEntity;{$IFDEF USE_INLINE} inline;{$ENDIF}
  protected
    function CompareEntityHandles(const A, B: Pointer): Integer;
    function FindAppID(const AAppIDHandle: UInt64; var I: Integer): Boolean;
    procedure Init(AAppIDSection: TsgDXFEntity);
  public
    constructor Create(ACADExport: TsgCADDirectExport);
    destructor Destroy; override;
    property SortedList: TsgStringList read FSortedList;
    property Ent[Index: Integer]: TsgDXFEntity read GetEnt write SetEnt; default;
    property Entity[const AName: string]: TsgDXFEntity read GetEntity;
  end;

const
  cnstRawEnter = $0A0D;
  cnstMaxBinDataLineLen = 254;
  cnstMaxBinBytesPerLine = cnstMaxBinDataLineLen div 2;
  RawEnter: Word = cnstRawEnter;
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  MaxRawBufferLenInt = 11;
  RawBufferLen = 2 * MaxRawBufferLenInt + 4; // 4 = 2 * SizeOf(Word(cnstRawEnter))

var
  RawBuffer: array[0 .. RawBufferLen - 1] of AnsiChar;

function _itosA(AValue, APrecision, ABufferOffset: Integer): Pointer; register;
asm
{$IFDEF CPUX64}
  push rsi
  push rdi
  push rbx

  mov rax,rcx
  mov rsi,rcx   // rsi - AValue
  mov rbx,rdx   // RBX - APrecision
  mov rcx,r8    // RCX - ABufferOffset

  or rax,rax
  jns @positive
  neg rax

@positive:
  mov rdi,10

@divide:
  xor rdx,rdx
  div rdi
  add rdx,'0'
  dec rcx
  mov byte ptr [rcx],dl
  dec rbx
  test rax,rax
  jnz @divide

  or rsi,rsi
  jns @positive1
  dec rcx
  mov byte ptr [rcx],'-'
@positive1:

  cmp rbx,0
  jng @end
@space:
  dec rcx
  mov byte ptr [rcx],' '
  dec rbx
  cmp rbx,0
  jg @space

@end:
  mov rax,rcx

  pop rbx
  pop rdi
  pop rsi
{$ELSE}
  push esi
  push edi
  push ebx

  mov esi,eax
  mov ebx,edx

  or eax,eax
  jns @positive
  neg eax

@positive:
  mov edi,10

@divide:
  xor edx,edx
  div edi
  add edx,'0'
  dec ecx
  mov byte ptr [ecx],dl
  dec ebx
  test eax,eax
  jnz @divide

  or esi,esi
  jns @positive1
  dec ecx
  mov byte ptr [ecx],'-'
@positive1:

  cmp ebx,0
  jng @end
@space:
  dec ecx
  mov byte ptr [ecx],' '
  dec ebx
  cmp ebx,0
  jg @space

@end:
  mov eax,ecx

  pop ebx
  pop edi
  pop esi
{$ENDIF}
end;
function GetCode(ACode, APrecision: Integer): Pointer; register; assembler;
asm
{$IFDEF CPUX64}
  lea r8,RawBuffer
  add r8,RawBufferLen - 2
  mov word ptr [r8],cnstRawEnter
{$ELSE}
  lea ecx,RawBuffer
  add ecx,RawBufferLen - 2
  mov word ptr [ecx],cnstRawEnter
{$ENDIF}
  call _itosA
end;

function GetInt(ACode, AValue: Integer): Pointer; register; assembler;
asm
{$IFDEF CPUX64}
  push rcx

  lea r8,RawBuffer
  add r8,RawBufferLen

  movsx rcx,edx
  xor rdx,rdx

  sub r8,2
  mov word ptr [r8],cnstRawEnter
  call _itosA

  mov r8,rcx
  pop rcx
  mov rdx,3

  sub r8,2
  mov word ptr [r8],cnstRawEnter
  call _itosA
{$ELSE}
  push eax

  lea ecx,RawBuffer
  add ecx,RawBufferLen

  mov eax,edx
  xor edx,edx

  sub ecx,2
  mov word ptr [ecx],cnstRawEnter
  call _itosA

  mov ecx,eax
  pop eax
  mov edx,3

  sub ecx,2
  mov word ptr [ecx],cnstRawEnter
  call _itosA
{$ENDIF}
end;

function GetSize(const ABufPointer: Pointer): TsgNativeInt;
begin
  Result := SizeOf(RawBuffer) - (TsgNativeInt(ABufPointer) - TsgNativeInt(@RawBuffer[0]));
end;
{$ENDIF}

function GetDXFEntColor(const AEntity: TsgDXFEntity;
  const AVersion: TsgDWGVersion = acR2004): TsgColorCAD;
begin
  if AVersion <= acR2000 then
  begin
    if IsCADGradient(AEntity) then
      Result := TsgCADGradientPolygon(AEntity).GradientColorCAD[0]
    else
      Result := AEntity.ColorCAD
  end
  else
    Result := AEntity.ColorCAD;
end;

type
  TWriteBytes = function(const ABytes; ACount: Integer): Integer of object;

function WriteBinary(AInpBuffer: Pointer; ASize: Integer; AWriteBytes: TWriteBytes;
  ALineLen: Integer = cnstMaxBinDataLineLen): Integer; overload;
const
  cntLnBrkLen = SizeOf(cnstRawEnter);
  cnt310Len = 3;
  cntSubLen = cnt310Len + cntLnBrkLen + cntLnBrkLen;
  cntBuffLen = cnstMaxBinDataLineLen + cntSubLen;
var
  vBuff: array[0 .. cntBuffLen - 1] of Byte;
  vInp, vEnd: PByte;
  vOut, vOutEnd: PWord;
  C, vBuffLen: Integer;
begin
  Result := 0;
  if ALineLen > cnstMaxBinDataLineLen then
    ALineLen := cnstMaxBinDataLineLen;
  vBuffLen := ALineLen + cntSubLen;
  InitByteBinToHex;
  vInp := PByte(AInpBuffer);
  vEnd := PByte(TsgNativeInt(vInp) + ASize);
  PCardinal(@vBuff[0])^ := $0D303133; // '310'#13
  PByte(@vBuff[4])^ := $0A; //#10
  while TsgNativeInt(vInp) < TsgNativeInt(vEnd) do
  begin
    vOut := PWord(@vBuff[cnt310Len + cntLnBrkLen]);
    vOutEnd := PWord(@vBuff[vBuffLen - cntLnBrkLen]);
    while (TsgNativeInt(vOut) < TsgNativeInt(vOutEnd)) and
       (TsgNativeInt(vInp) < TsgNativeInt(vEnd)) do
    begin
      vOut^ := ByteBinToHex^[vInp^];
      Inc(vInp);
      Inc(vOut);
    end;
    if vOut^ <> cnstRawEnter then vOut^ := cnstRawEnter; //#13#10
    C := TsgNativeInt(vOut) - TsgNativeInt(@vBuff[0]) + cntLnBrkLen;
    Inc(Result, C);
    AWriteBytes(vBuff, C);
  end;
end;

function WriteBinary(S: TMemoryStream; AWriteBytes: TWriteBytes;
  ALineLen: Integer = cnstMaxBinDataLineLen): Integer; overload;
var
  vInp: PByte;
  vSize: Integer;
begin
  vSize := S.Size - S.Position;
  vInp := PByte(TsgNativeInt(S.Memory) + S.Position);
  Result := WriteBinary(vInp, vSize, AWriteBytes, ALineLen);
end;

function TextToDXFText(const S: string): string;
begin
  Result := S;
  StringReplace(Result, #13, '^M');
  StringReplace(Result, #10, '^J');
  StringReplace(Result, #9, '^I');
end;

{ TsgCADtoDXF }

constructor TsgCADtoDXF.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  FNewBlockRecordList := TStringList.Create;
  FNewBlockRecordList.Sorted := True;
  FBlockRecordsListExported := TsgList.Create;
  FBlockRecordsListExported.Sorted := True;
  FBlockRecordsListExported.Duplicates := dupIgnore;
  ConvertImageToOLE := True;
  FContentOleByImage := False;
  FHandleCode := 5;
  FACISEntities := TsgObjectList.Create(False);
end;

procedure TsgCADtoDXF.Add2DFlatPoint(const Code: Integer; const Pt: TFPoint);
begin
  AddFloat(Code, Pt.X);
  AddFloat(Code+10, Pt.Y);
end;

procedure TsgCADtoDXF.Add2DPoint(const Code: Integer; const Pt: TF2DPoint);
begin
  AddFloat(Code, Pt.X);
  AddFloat(Code+10, Pt.Y);
end;

procedure TsgCADtoDXF.Add3DPoint(const Code: Integer; const Pt: TFPoint);
begin
  AddFloat(Code, Pt.X);
  AddFloat(Code+10, Pt.Y);
  AddFloat(Code+20, Pt.Z);
end;

procedure TsgCADtoDXF.Add3DPointExtruded(const Code: Integer; const Pt, Extr: TFPoint);
var
  vPt: TFPoint;
begin
  if Extruded(Extr) then
  begin
    vPt := Pt;
    DoPreExtrusion(vPt, Extr);
    Add3DPoint(Code, vPt);
  end
  else
    Add3DPoint(Code, Pt);
end;

procedure TsgCADtoDXF.Add3DPointPreExtruded(const Code: Integer; const Pt, Extr: TFPoint);
var
  vPt: TFPoint;
begin
  if Extruded(Extr) then
  begin
    vPt := Pt;
    DoExtrusion(vPt, Extr);
    Add3DPoint(Code, vPt);
  end
  else
    Add3DPoint(Code, Pt);
end;

procedure TsgCADtoDXF.AddBlockName(const AName: string);
begin
  if Version > acR14 then
    AddString(2, AName)
  else
    AddString(2, AnsiUpperCase(AName));
end;

procedure TsgCADtoDXF.AddCode(const AValue: Integer);
var
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  P: Pointer;
{$ELSE}
  S: string;
{$ENDIF}
begin
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  P := GetCode(AValue, 3);
  WriteBuffer(P^, GetSize(P));
{$ELSE}
  S := Format('%3d%s', [AValue, sEnter]);
  WriteStr(S);
{$ENDIF}
end;

procedure TsgCADtoDXF.AddCommonEntityGroup(AEntity: TsgDXFEntity);
begin
  AddCommonEntityGroupEx(AEntity, sCADEntityNames[AEntity.EntType],
    sCADEntitySubNames[AEntity.EntType]);
end;

procedure TsgCADtoDXF.AddCommonEntityGroupEx(AEntity: TsgDXFEntity; const AName, ASubName: string);
begin
  AddEntName(AEntity, AName);
  if not AEntity.Visibility then
    AddInt(60, 1);
  AddLineWeight(AEntity.LineWeight);
  AddDXFColor(AEntity.ColorCAD);
  if AEntity.LineTypeScale <> 1 then
    AddFloat(48, AEntity.LineTypeScale);
  if (AEntity is TsgDXFPenLine) or (AEntity.EntType in [ceMPolygon]) then
    AddLineType(AEntity.LineType);
  AddSubclassMarker(ASubName);
  case AEntity.EntType of
    ceMPolygon: AddInt(70, 1);//the version of MPolygon
    ceArc, ceCircle, ceEllipse, ceSolid: AddExtrusion(TsgDXFPenLine(AEntity).Extrusion); // Only for entities whose coordinates are set in OCS (or 2D)
    ceCurvePolygon:  AddTransparency(TsgCADCurvePolygon(AEntity).Transparency);
  end;
end;

procedure TsgCADtoDXF.AddCommonTableGroup(AEntity: TsgDXFEntity;
  const ABaseName, ASub, AName: string; const AFlags: Integer);
begin
  BeginObject(ABaseName);
  AddHandle(AEntity, cnstBadHandle, []);
  AddSubclassMarker('SymbolTableRecord');
  AddSubclassMarker(ASub+'TableRecord');
  AddString(2, AName);
  if AFlags >= 0 then
    AddInt(cFlag_70, AFlags);
end;

procedure TsgCADtoDXF.AddDXFColor(const AValue: TsgColorCAD;
  const AVisible: Boolean = True; const AditionalIndex: Byte = 0);
const
  cnstSingColor: array [Boolean] of Integer = (-1, 1);
var
  vColorIndex, vColorRgb: Integer;
  vColorFix: TsgColorCAD;
begin
  case AValue.Active of
    acIndexColor:
      begin
        if AValue.Color > clDXFByLayer then
        begin
          vColorFix := AValue;
          vColorFix.Active := acRGBColor;
          AddDXFColor(vColorFix, AVisible, AditionalIndex);
        end
        else
          if AValue.Color <> clDXFByLayer then
            AddInt(62 + AditionalIndex, cnstSingColor[AVisible] * Integer(AValue.Color));
      end;
  else//acRGBColor
    vColorIndex := ConvertColorCADToIndexColor(AValue, True);
    AddInt(62 + AditionalIndex, cnstSingColor[AVisible] * vColorIndex);
    if Version >= acR2004 then
    begin
      vColorRgb := (AValue.Color and $FF0000 shr 16) or (AValue.Color and $FF00)
        or (AValue.Color and $FF shl 16);
      AddInt(420 + AditionalIndex, vColorRgb);
    end;
  end;
end;

procedure TsgCADtoDXF.AddEntName(AEntity: TsgDXFEntity;
  const AName: string; ALayerName: string = '');
var
  vXDictionary: UInt64;
  vFilter: TsgDXFEntity;
begin
  BeginObject(AName);
  vXDictionary := cnstBadHandle;
  if Assigned(AEntity) then
    case AEntity.EntType of
      ceInsert:
        if HasSpatialFilter(AEntity.Dictionary, vFilter) then
          vXDictionary := GetHandleByEntityNoCheck(AEntity.Dictionary);
      ceAttdef, ceAttrib:
        if TsgDXFEntityAccess(AEntity).GetExtensionDictionary(cnstAcadMLAtt) <> nil then
          vXDictionary := GetHandleByEntityNoCheck(AEntity.Dictionary);
    end;
  AddHandle(AEntity, cnstBadHandle, [], vXDictionary);
  AddSubclassMarker('Entity');
  if FPaperMode then
    AddInt(67, 1);
  if Length(ALayerName) = 0 then
    ALayerName := GetEntityLayerName(AEntity);
  AddString(8, ALayerName);
end;

procedure TsgCADtoDXF.AddExtrusion(const Pt: TFPoint);
begin
  if Extruded(Pt) then
    Add3DPoint(210, Pt);
end;

procedure TsgCADtoDXF.AddTransparency(const AValue: Single);
var
  vValue: Integer;
begin
  if (Version > acR2000) and (AValue <> 0) then
  begin
    if AValue < 0 then
    begin
      if AValue = fTransparencyByBlock then
        AddInt(440, $01000000);
    end
    else
    begin
      vValue := Ceil(2.55 * AValue);
      if vValue < 0 then
        vValue := 0;
      if vValue > 255 then
        vValue := 255;
      AddInt(440, $02000000 or (255 - vValue));
    end;
  end;
end;

procedure TsgCADtoDXF.AddFloat(const Code: Integer; Value: Double);
{$IFNDEF SG_DXF_EXPORT_OPTIMIZE}
var
  S: string;
{$ENDIF}
begin
{$IFDEF SG_CLIENT_CLNOFSFL}
  if (Code = 30) and IsZero(Value) then
    Exit;
{$ENDIF}
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  AddRawByteString(Code, sgRawByteString(FloatToStr({$IFDEF SG_CLIENT_CLNOFSFL}SimpleRoundTo(Value, -7){$ELSE}Value{$ENDIF})));
{$ELSE}
  S := Format('%3d', [Code]) + sEnter +
  {$IFDEF SG_CLIENT_CLNOFSFL}
    FloatToStr(SimpleRoundTo(Value, -7))
  {$ELSE}
    FloatToStr(Value)
  {$ENDIF} + sEnter;
  WriteStr(S);
{$ENDIF}
end;

procedure TsgCADtoDXF.AddHandle(AEntity: TsgDXFEntity; const AParentHandle: UInt64;
  const AReactors: array of UInt64; const AXDictionary: UInt64 = cnstBadHandle);
var
  H: UInt64;
begin
  if Version >= acR14 then
  begin
    H := GetHandleByEntityNoCheck(AEntity);
    if H = cnstBadHandle then
      H := DoHandle;
    AddHexInt(FHandleCode, H);
    if AXDictionary <> cnstBadHandle then
      DoAddReactors(cntCodeHardOwnerHandle, sAcadXDictionary, [AXDictionary]);
    if Length(AReactors) > 0 then
      DoAddReactors(cParentHandle_330, sAcadReactors, AReactors);
    if (Version >= acR2000) and (AParentHandle <> cnstBadHandle) then
      AddHexInt(cParentHandle_330, AParentHandle);
  end;
end;

procedure TsgCADtoDXF.AddHexInt(const Code: Integer; const Value: UInt64);
begin
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  AddRawByteString(Code, sgRawByteString(IntToHex(Value, 0)));
{$ELSE}
  AddString(Code, IntToHex(Value, 0));
{$ENDIF}
end;

procedure TsgCADtoDXF.AddInt(const Code, Value: Integer);
var
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  P: Pointer;
{$ELSE}
  S: string;
{$ENDIF}
begin
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  P := GetInt(Code, Value);
  WriteBuffer(P^, GetSize(P));
{$ELSE}
  S := Format('%3d%s%d%s', [Code, sEnter, Value, sEnter]);
  WriteStr(S);
{$ENDIF}
end;

procedure TsgCADtoDXF.AddLineWeight(const AValue: Double);
begin
  if (AValue <> 0) and (Ord(Version) >= Ord(acR2000)) then
    AddInt(370, CorrectLineWeight(AValue, False));
end;

procedure TsgCADtoDXF.AddRawByteString(const Code: Integer;
  const ARawByteString: sgRawByteString);
begin
  AddCode(Code);
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  WriteBuffer(PPointer(ARawByteString)^, Length(ARawByteString));
{$ELSE}
  WriteRawByteStr(ARawByteString);
{$ENDIF}
  WriteBuffer(RawEnter, SizeOf(RawEnter));
end;

procedure TsgCADtoDXF.AddLineType(ALineType: TsgDXFLineType);
begin
  if ALineType <> nil then
  {$IFDEF SG_CLIENT_CLNOFSFL}
    if not sgSameText(ALineType.Name, sByLayer) then
  {$ENDIF}
      AddString(6, ExtractName(ALineType.Name));
  //else AddString(6, 'ByLayer');// for future versions
end;

procedure TsgCADtoDXF.AddString(const Code: Integer; const Str: string);
{$IFNDEF SG_DXF_EXPORT_OPTIMIZE}
var
  S: string;
{$ENDIF}
begin
{$IFDEF SG_DXF_EXPORT_OPTIMIZE}
  AddCode(Code);
  WriteStr(Str);
  WriteBuffer(RawEnter, SizeOf(RawEnter));
{$ELSE}
  S := Format('%3d%s%s%s', [Code, sEnter, Str, sEnter]);
  WriteStr(S);
{$ENDIF}
end;

procedure TsgCADtoDXF.AddStringBig(const ACode, ASubCode, AExtCode: Integer;
  const AString: string);

  procedure ExportString(const AStrCode: Integer; const AStr: string);
  begin
    if AExtCode > 0 then
    begin
      AddInt(Integer_1070, AStrCode);
      AddString(AExtCode, AStr);
    end
    else
      AddString(AStrCode, AStr);
  end;

var
  vStr: string;
begin
  if Length(AString) <= cnstMaxStringLen then
    ExportString(ACode, AString)
  else
  begin
    ExportString(ACode, Copy(AString, 1, cnstMaxStringLen));
    vStr := AString;
    repeat
      Delete(vStr, 1 ,cnstMaxStringLen);
      if Length(vStr) <= cnstMaxStringLen then
        ExportString(ASubCode, vStr)
      else
        ExportString(ASubCode, Copy(vStr, 1, cnstMaxStringLen));
    until Length(vStr) <= cnstMaxStringLen;
  end;
end;

procedure TsgCADtoDXF.AddSubclassMarker(const Str: string);
begin
  if Ord(Version) >= Ord(acR14) then
    AddString(100, cntAcDb + Str);
end;

procedure TsgCADtoDXF.AddThickness(const AValue: Double);
begin
  if AValue <> 0 then
    AddFloat(39, AValue);
end;

procedure TsgCADtoDXF.CheckBlockName(var AName: string; var AFlags: Byte);
begin
 if cnstNoAnonimusSVGBlock and IsAnonymousBlock(AFlags) and IsSVGBlock(AName) then
   CheckSVGBlockFlagsAndName(AFlags, AName);
end;

procedure TsgCADtoDXF.Clear;
begin
  inherited Clear;
  if Assigned(FACISEntities) then
    FACISEntities.Clear;
end;

function TsgCADtoDXF.ConvertDWGExtDataToDXF(const AExtData: TsgCADExtendedData): Boolean;
var
  I, J: Integer;
  vByte: Byte;
  E: TsgDXFEntity;
begin
  Result := True;
  I := 0;
  while (I < AExtData.DataCount) and Result do
  begin
    case AExtData.DataType[I] of
      edtByte:
        begin
          if AExtData.DataCode[I] = 1002 then
          begin
            vByte := AExtData.DataByte[I];
            AExtData.ChangeType(I, edtString);
            case vByte of
              0: AExtData.DataString[I] := '{';
              1: AExtData.DataString[I] := '}';
            end;
          end;
        end;
      edtInt64:
        begin
          case AExtData.DataCode[I] of
            1001:
              begin
                if TAppIDs(FAppIDs).FindAppID(AExtData.DataInt64[I], J) then
                begin
                  AExtData.ChangeType(I, edtString);
                  AExtData.DataString[I] := TAppIDs(FAppIDs)[J].Name;
                end
                else
                  Result := False;
              end;
            1003:
              begin
                E := Converter.Sections[csLayers].FindEntByHandle(AExtData.DataInt64[I]);
                AExtData.ChangeType(I, edtString);
                if Assigned(E) then
                  AExtData.DataString[I] := E.Name
                else
                  AExtData.DataString[I] := '0';
              end;
          end; { case }
        end;
    end; { case }
    Inc(I);
  end;
end;

function TsgCADtoDXF.CorrectLineWeight(const ALineWeight: Double;
  ANeedToConvert: Boolean): Integer;
begin
  Result := ConvertLineWeightToDXF(ALineWeight);
end;

function TsgCADtoDXF.CreateBlockRecord(const ABlock: TsgDXFBlock;
  const AName: string): TsgDXFBlockRecord;
begin
  Result := GetBlockRecord(ABlock);
  if Result = nil then
  begin
    Result := TsgDXFBlockRecord(CreateNewEntity(TsgDXFBlockRecord, GetEntityName(ABlock, AName)));
    FNewBlockRecordList.AddObject(Result.Name, Result);
  end;
end;

function TsgCADtoDXF.CreateNewEntity(const AEntClass: TsgDXFEntityClass;
  const AName: string): TsgDXFEntity;
begin
  Result := AEntClass.Create;
  Result.Name := AName;
  Result.Handle := DoHandle;
end;

class function TsgCADtoDXF.ExportType: TsgExportFormat;
begin
  Result := efDxf;
end;

destructor TsgCADtoDXF.Destroy;
begin
  FBlockRecordsListExported.Free;
  ClearObjects(FNewBlockRecordList);
  FNewBlockRecordList.Free;
  FAppIDs.Free;
  FreeAndNil(FACISEntities);
  inherited Destroy;
end;

procedure TsgCADtoDXF.DoAddReactors(const ACode: SmallInt;
  const AReactorName: string; const AReactors: array of UInt64);
var
  I: Integer;
begin
  AddString(cntCodeControl, '{' + AReactorName);
  for I := Low(AReactors) to High(AReactors) do
    AddHexInt(ACode, AReactors[I]);
  AddString(cntCodeControl, '}');
end;

procedure TsgCADtoDXF.DoExport(const E: TsgDXFEntity; const Data: PsgDirectExportData = nil);
begin
{$IFDEF SG_CLIENT_CLNOFSFL}
  if Version = acR10 then
  begin
    case E.EntType of
      ceLine:              ExportLine(TsgDXFLine(E));
      ceArc:               ExportArc(TsgDXFArc(E));
      ceLWPolyline,
        cePolyline,
        cePath:            ExportPolyline(TsgDXFPolyline(E));
    end;
  end
  else
{$ENDIF}
  case E.EntType of
    ceLine:              ExportLine(TsgDXFLine(E));
    cePoint:             ExportPoint(TsgDXFPoint(E));
    ceSolid, ceTrace:    ExportSolid(TsgDXFSolid(E));
    ceCircle:            ExportCircle(TsgDXFCircle(E));
    ceArc:               ExportArc(TsgDXFArc(E));
    ceEllipse:           ExportEllipse(TsgDXFEllipse(E));
    ceLWPolyline,
      cePolyline,
      cePath:            ExportPolyline(TsgDXFPolyline(E));
    ceSpline, ceHelix:   ExportSpline(TsgDXFSpline(E));
    ceACADTable:         ExportAcadTable(TsgDXFAcadTable(E));
    ceInsert:            ExportInsert(TsgDXFInsert(E));
    ceAttdef,
      ceAttrib, ceText:  ExportText(TsgDXFText(E));
    ceShape:             ExportShape(TsgDXFShape(E));
    ceMText:             ExportMText(TsgDXFMText(E));
    ceTolerance:         ExportTolerance(TsgDXFTolerance(E));
    ceDimension:         ExportDimension(TsgDXFDimension(E));
    ce3dFace:            Export3DFace(TsgDXF3DFace(E));
    ceLeader:            ExportLeader(TsgDXFLeader(E));
    cePolyPolygon,
      ceGradient,
      ceGradientPolygon,
      ceCurvePolygon,
      ceHatch,
      ceMPolygon:        ExportHatch(TsgCADPolyPolygon(E));
    ceViewport:          ExportViewPort(TsgDXFViewPort(E));
    ceImageEnt:          ExportImageEnt(TsgDXFImageEnt(E));
    ceRegion, ceBody:
                         ExportACISEntity(TsgBrepModAcis(E));
    ceSurface:           ExportSurface(TsgDXFSurface(E));
    ceIges, ceStep, ceBrep, ceParasolid, ceInventor,
    ce3DSolid:           Export3dSolid(TsgDXF3dSolid(E));
    ceOle2Frame:         ExportOle2Frame(TsgDXFOle2Frame(E));
    ceProxy:             ExportChildEntities(TsgDXFEntity(E));
{
    cePattern:           ExportPattern(TsgSVGPattern(E));
}
    ceFlatPoly, ceFlatPoly3D: ExportFlatPoly(TsgFlatPoly(E));
    ceFlatHatch:         ExportFlatHatch(TsgFlatHatch(E));
    ceWipeOut{,
      ceImageEnt}:        ExportWipeOut(TsgCADWipeOut(E));
{$IFDEF SG_BTI}
    cePolyPolyline2D:    ExportPolyPolyline2D(TsgCADPolyPolyline2D(E));
{$ENDIF}
    ceMLine:             ExportMLine(TsgCADMLine(E));
    ceRay, ceXline:      ExportRay(TsgDXFRay(E));
    ceMLeader:           ExportMLeader(TsgCADMultiLeader(E));
    ceMesh:              ExportMesh(TsgDXFMesh(E));
  end;
end;

procedure TsgCADtoDXF.DoExportEED(const AExtData: TsgCADExtendedData);
var
  I: Integer;
begin
  for I := 0 to AExtData.DataCount - 1 do
    case AExtData.DataType[I] of
      edtByte:      AddInt(AExtData.DataCode[I], AExtData.DataByte[I]);
      edtInt16:     AddInt(AExtData.DataCode[I], AExtData.DataInt16[I]);
      edtInt:       AddInt(AExtData.DataCode[I], AExtData.DataInt[I]);
      edtSingle:    AddFloat(AExtData.DataCode[I], AExtData.DataSingle[I]);
      edtDouble:    AddFloat(AExtData.DataCode[I], AExtData.DataDouble[I]);
      edtString:    AddString(AExtData.DataCode[I], TextToDXFText(AExtData.DataString[I]));
      edtF2DPoint:  Add2DPoint(AExtData.DataCode[I], AExtData.DataPoint2D[I]);
      edtF3DPoint:  Add3DPoint(AExtData.DataCode[I], AExtData.DataPoint[I]);
      edtInt64:     AddHexInt(AExtData.DataCode[I], AExtData.DataInt64[I]);
      edtObject:    AddHexInt(AExtData.DataCode[I], GetHandleByEntity(TsgDXFEntity(AExtData.DataObject[I])));
      edtBinary:    AddRawByteString(AExtData.DataCode[I], AExtData.DataBinaryAsHex[I]);
    end;
end;

function TsgCADtoDXF.GetBlockRecordByBlock(const ABlock: TsgDXFBlock;
  const AName: string): TsgDXFBlockRecord;
var
  vIndex: Integer;
  vName: string;
begin
  Result := GetBlockRecord(ABlock);
  if Result = nil then
  begin
    vName := GetEntityName(ABlock, AName);
    Result := TsgDXFBlockRecord(Converter.Sections[csBlockRecords].FindEntByName(vName));
    if Result = nil then
    begin
      vIndex := FNewBlockRecordList.IndexOf(vName);
      if vIndex > -1 then
        Result := TsgDXFBlockRecord(FNewBlockRecordList.Objects[vIndex]);
    end;
  end;
end;

procedure TsgCADtoDXF.ProcBlockRecord(ABlock: TsgDXFBlock; const AName: string; Data: Pointer;
  ACurrPaperSpaceIndex: Integer = -1);
var
  vXDictionary: TsgDXFEntity;
  vBlockRecord: TsgDXFBlockRecord;
  vName: string;
  vFlags: Byte;
  Cnt: Integer;
{$IFDEF SG_BLOCK_PREVIEW}
  vMS: TMemoryStream;
{$ENDIF}
begin
  vBlockRecord := GetBlockRecordByBlock(ABlock, AName);
  if vBlockRecord = nil then
    vBlockRecord := CreateBlockRecord(ABlock, AName);
  Cnt := FBlockRecordsListExported.Add(vBlockRecord);
  if FBlockRecordsListExported.Count <> Cnt then
  begin
    vFlags := 0;
    if Assigned(ABlock) then
      vFlags := ABlock.Flags;
    vName := AName;
    CheckBlockName(vName, vFlags);
    BeginObject(cnstTableBLOCK_RECORD);
    vXDictionary := vBlockRecord.Dictionary;
    if Assigned(vXDictionary) and (vXDictionary.FindEntByName(sInternalData) = nil) then
      vXDictionary := nil;
    AddHandle(vBlockRecord, cnstBadHandle, [], GetHandleByEntityNoCheck(vXDictionary)); //AddHexInt(cHandle_5, vBlockRecord.Handle);
    AddSubclassMarker('SymbolTableRecord');
    AddSubclassMarker('BlockTableRecord');
    AddBlockName(vName);
{$IFDEF SG_BLOCK_PREVIEW}
    vMS := nil;
    if (vBlockRecord <> nil) and (vBlockRecord.Preview.Graphic <> nil) then
    begin
      if ABlock.BlockRecord.Preview.Graphic is TsgBitmap then
      begin
        vMS := TMemoryStream.Create;
        TsgBitmapAccess(vBlockRecord.Preview.Graphic).WriteImage(vMS);
        vMS.Position := 0;
      end
      else
        if vBlockRecord.Preview.Graphic is TBitmap then
        begin
          vMS := TMemoryStream.Create;
          TBitmap(vBlockRecord.Preview.Graphic).SaveToStream(vMS);
          vMS.Position := SizeOf(TBitmapFileHeader);
        end;
    end;
    try
      if Assigned(vMS) then
        WriteBinary(vMS, WriteBuffer);
    finally
      vMS.Free;
    end;
{$ENDIF}
    ExportExtendedData(vBlockRecord);
  end;
end;

procedure TsgCADtoDXF.ProcessExport(Stream: TStream);
var
  vPos: {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF};
  DS: Char;
  vMS: TStream;
{$IFDEF SG_ENCRYPT_FILES}
  vEncryptNeeded: Boolean;
{$ENDIF}
begin
    vMS := Stream;
{$IFDEF SG_ENCRYPT_FILES}
    vEncryptNeeded := Assigned(FSGBlock);
    if vEncryptNeeded then
      vMS := TMemoryStream.Create;
    try
{$ENDIF}
      FStream := TBufferedStream.Create(vMS, cnstBufferSize);
      DS := SetDecimalSeparator('.');
      try
      {$IFDEF SG_CLIENT_CLNOFSFL}
        if Version = acR10 then
        begin
          Progress(psStarting, 0, '');
          vPos := 0;
        end
        else
        begin
      {$ENDIF}
          Progress(psStarting, 0, '');
          vPos := ExportSectionHEADER;
          Progress(psRunning, 1, '');
          ExportSectionCLASSES;
          ExportSectionTABLES;
          Progress(psRunning, 9, '');
          ExportSectionBLOCKS;
      {$IFDEF SG_CLIENT_CLNOFSFL}
        end;
      {$ENDIF}
        Progress(psRunning, 34, '');
        ExportSectionENTITIES;
      {$IFDEF SG_CLIENT_CLNOFSFL}
        if Version <> acR10 then
        begin
      {$ENDIF}
          Progress(psRunning, 99, '');
          ExportSectionOBJECTS;
      {$IFDEF SG_CLIENT_CLNOFSFL}
        end;
      {$ENDIF}
        if (Version >= acR2013) and (FACISEntities.Count > 0) then
          ExportSectionACDSDATA;
        EndOfFile(vPos);
        Progress(psEnding, 100, '');
      finally
        FStream.Free;
        FStream := nil;
        UpdateXInfo(vMS, FXInfoOffs);
        SetDecimalSeparator(DS);
      end;
{$IFDEF SG_ENCRYPT_FILES}
    finally
      if vEncryptNeeded then
      begin
        sgStmCrpt.EnctyptStm(Stream, TMemoryStream(vMS));
        vMS.Free;
      end;
    end;
{$ENDIF}
end;

procedure TsgCADtoDXF.PrepareExportText(var AText: string);
begin
  ReplaceAnsi(AText, #$9, '^I');
  ReplaceAnsi(AText, #$D#$A, '\P');
  ReplaceAnsi(AText, #$A, '^J');
  ReplaceAnsi(AText, '^;', '^ ;'); // or, may be insert space after any '^'?
end;

procedure TsgCADtoDXF.PrepareBlock(ABlockRecord: TsgDXFBlockRecord);
const
  cnstAvalBlockNameSymbols = ['B', 'D' .. 'F', 'H' .. 'K', 'M', 'O', 'R'..'Z'];
var
  J, vLen: Integer;
  vBlockRecord: TsgDXFBlockRecordAccess;
  vItem: TsgNamedItem;
  vCorrectedName, S: string;
  vHasAsterisk, vIsInternalDimensionBlockName: Boolean;

  function BlockNameToAnonymous(const AName: string; const AID: Integer;
    var AIsInternalDimesionBlockName: Boolean): string;
  var
    vBlockNameSymbol: string;
  begin
    AIsInternalDimesionBlockName := False;
    if AName <> '' then
    begin
      AIsInternalDimesionBlockName := HasInternalDimensionBlockName(AName);
      if AIsInternalDimesionBlockName then
        vBlockNameSymbol := 'D'
      else
      begin
        if IsBTIBlockName(AName, False) then
        begin
          vLen := Length(AName);
          if vLen > 3 then
            vLen := 3;
          vBlockNameSymbol := Copy(AName, 1, vLen);
        end
        else
          vBlockNameSymbol := UpCase(AName[1])
      end;
    end
    else
      vBlockNameSymbol := cnstDefBlockNameSymbol;
    if Length(vBlockNameSymbol) = 1 then
    begin
      if not CharInSet(vBlockNameSymbol[1], cnstAvalBlockNameSymbols) then
        vBlockNameSymbol := cnstDefBlockNameSymbol;
    end;
    Result := cnstAsterisk + vBlockNameSymbol + IntToStr(AID);
  end;

  function CreateXrecord(const AItem: TsgNamedItem): TsgDXFDictionary;
  var
    vXrecord: TsgDXFXRecord;
  begin
    Result := TsgDXFDictionary(AItem.Ancestor.Dictionary);
    if Result = nil then
    begin
      Result := TsgDXFDictionary(CreateNewEntity(TsgDXFDictionary, cnstDictionary));
      if AItem.Ancestor.Handle = cnstBadHandle then
        AItem.Ancestor.Handle := DoHandle;
      AItem.Ancestor.Dictionary := Result;
    end;
    vXrecord := TsgDXFXRecord(Result.FindEntByName(sInternalData));
    if vXrecord = nil then
    begin
      vXrecord := TsgDXFXRecord(CreateNewEntity(TsgDXFXRecord, sInternalData));
      Result.AddEntity(vXrecord);
    end
    else
      TsgCADExtendedDataAccess(vXrecord.Data).ClearData;
    vXrecord.Data.AddInt(90, 0);
    vXrecord.Data.AddString(1, AItem.Name);
  end;
begin
  inherited PrepareBlock(ABlockRecord);
  vBlockRecord := TsgDXFBlockRecordAccess(ABlockRecord);
  vItem := TsgNamedItem(vBlockRecord.GetNamedItem);
  vCorrectedName := TrimRight(vItem.Name); // name correction
  if vCorrectedName <> vItem.Name then
  begin
    if vCorrectedName = '' then
      vCorrectedName := cnstInternalBlockPrefix + cnstDefBlockNameSymbol;
    if FRenamedBlockRecords.IndexOf(vCorrectedName) >= 0 then
    begin
      repeat
        S := vCorrectedName + '$' + IntToHex(FBlockID, 0);
        Inc(FBlockID);
      until FRenamedBlockRecords.IndexOf(S) < 0;
      Dec(FBlockID);
      vCorrectedName := S;
    end;
  end;
  if GetPaperIndex(vCorrectedName) = -1 then
  begin
    if not IsBTIBlockInternal(vCorrectedName) then
    begin
      vHasAsterisk := HasAsterisk(vCorrectedName);
      if vHasAsterisk then
      begin
        vCorrectedName := BlockNameToAnonymous(Copy(vCorrectedName, 2, MaxInt), FBlockID, vIsInternalDimensionBlockName);
        vHasAsterisk := True;
      end;
      if IsAnonymousBlock(vBlockRecord.Block.Flags) and not vHasAsterisk then
        vCorrectedName := BlockNameToAnonymous(vCorrectedName, FBlockID, vIsInternalDimensionBlockName);
    end;
  end;
  if vCorrectedName <> vItem.Name then
  begin
    FRenamedBlockRecords.AddObject(vItem.Name, vItem); // store name
    if vIsInternalDimensionBlockName or (vBlockRecord.Block.EntClass.EG = gtBTI) then
      CreateXrecord(vItem);
  end;
  vItem.Name := vCorrectedName; // do item rename
  for J := 0 to vBlockRecord.Block.Count - 1 do
    Inc(FCounts[vBlockRecord.Block[J].EntType]);
  Inc(FBlockID);
end;

procedure TsgCADtoDXF.PrepareDrawingDatabase;
begin
  FBlockID := 0;
  if not Assigned(FRenamedBlockRecords) then
  begin
    FRenamedBlockRecords := TsgStringList.Create;
    FRenamedBlockRecords.Sorted := True;
    TsgStringList(FRenamedBlockRecords).Duplicates := dupIgnore;
  end;
  inherited PrepareDrawingDatabase;
end;

procedure TsgCADtoDXF.ProcBlock(ABlock: TsgDXFBlock; const AName: string; Data: Pointer;
  ACurrPaperSpaceIndex: Integer = -1);
  procedure BeginBlock(const ABlockName: string; const ABlockFlags: Integer);
  begin
    // nil (see BLOCK_RECORDS)
    AddEntName(ABlock, 'BLOCK');
    AddSubclassMarker('BlockBegin');
    AddBlockName(ABlockName);
    AddInt(cFlag_70, ABlockFlags);
  end;
var
  J: Integer;
  S, vName: string;
  vFlags: Byte;
begin
  if ABlock <> nil then
  begin
    vName := AName;
    vFlags := TsgDXFBlockAccess(ABlock).Flags;
    vName := AName;
    CheckBlockName(vName, vFlags);
    if (vFlags and 1 <> 0) and not HasAsterisk(vName) then
      vFlags := vFlags and not 1;
    BeginBlock(vName, vFlags);
    Add3DPoint(10, ABlock.Offset);
    AddString(3, vName);
    AddString(1, ABlock.XrefPath);
    S := ABlock.Description;
    if S <> '' then
      AddString(4, TextToDXFText(S));
    ExportExtendedData(ABlock);
    for J := 0 to ABlock.Count - 1 do
      DoExport(TsgDXFEntity(ABlock.Entities[J]));
  end
  else
  begin
    BeginBlock(AName, 0);
    Add2DPoint(10, cnstF2DPointZero);
  end;
  AddEntName(nil, 'ENDBLK');
  AddSubclassMarker('BlockEnd');
end;

function TsgCADtoDXF.GetCorrectBlockName(const AName: string): string;
var
  vPos: Integer;
begin
  if HasInternalDimensionBlockName(AName, @vPos) then
  begin
    if not HasAsterisk(AName) then
      Result := cnstInternalBlockPrefix + AName
    else
      Result := AName;
  end
  else
    Result := AName
end;

procedure TsgCADtoDXF.SetOleFrameVariable(const AValue: Integer);
begin
  FContentOleByImage := True;
  Converter.OleFrame := AValue;
end;

procedure TsgCADtoDXF.UpdateXInfo(AStream: TStream; AXInfoOffs: {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF});
var
  vBinXInfo: array[0 .. SizeOf(cnstXInfoZero) shl 1 - 1] of Byte;
  vXInfo: UInt64;
  P, vBase: PByte;
begin
  if (AXInfoOffs > 0) and (AStream.Size > 0) then
  begin
    vXInfo := 0;
    AStream.Position := 0;
    GetMem(vBase, cnstBufferSize);
    try
      repeat
        P := vBase;
        vXInfo := GetHash64(P, AStream.Read(P^, cnstBufferSize), vXInfo);
      until AStream.Position >= AStream.Size;
    finally
      FreeMem(vBase);
    end;
    AStream.Position := AXInfoOffs;
    sgBinToHex(@vXInfo, @vBinXInfo[0], SizeOf(vXInfo));
    AStream.Write(vBinXInfo, SizeOf(vBinXInfo));
  end;
end;

function TsgCADtoDXF.GetBlockRecord(const ABlock: TsgDXFBlock): TsgDXFBlockRecord;
begin
  Result := nil;
  if ABlock <> nil then
    Result := ABlock.BlockRecord;
end;

function TsgCADtoDXF.WriteBuffer(const ABuffer; ACount: Integer): Integer;
begin
  Result := FStream.Write(ABuffer, ACount);
end;

function TsgCADtoDXF.WriteBytes(const ABuffer: TBytes): Integer;
begin
  Result := WriteBuffer(ABuffer[0], Length(ABuffer));
end;

function TsgCADtoDXF.WriteBytesArray(const ABuffer: array of Byte): Integer;
begin
  Result := WriteBuffer(ABuffer[0], Length(ABuffer));
end;

function TsgCADtoDXF.WriteRawByteStr(const AString: sgRawByteString): Integer;
begin
  Result := Length(AString) * SizeOf(AString[1]);
  if Result > 0 then
    Result := WriteBuffer(Pointer(AString)^, Result);
end;

procedure TsgCADtoDXF.WriteStr(const AString: string);
var
  vStr: sgRawByteString;
begin
  if Version >= acR2007 then
    vStr := {$IFDEF SGDEL_6}UTF8Encode{$ENDIF}(AString)
  else
    vStr := StringToACADPresentation(AString, CP_ACP); // AutoCAD always uses system locale (CP_ACP)
  WriteRawByteStr(vStr);
end;

procedure TsgCADtoDXF.BeforeDestroy(var AClearing: Boolean);
var
  I, C: Integer;
  vHashList: PsgHashItemsArray;
  vItem: TsgNamedItem;
  vDictionaryItem: TsgTableItem;
  vDictionary: TsgDXFEntity;
  vXrecord: TsgDXFXRecord;
  vConverter: TsgCADConverter;
begin
  inherited BeforeDestroy(AClearing);
  if Assigned(FRenamedBlockRecords) then
  begin
    for I := 0 to FRenamedBlockRecords.Count - 1 do
    begin
      vItem := TsgNamedItem(FRenamedBlockRecords.Objects[I]);
      vItem.Name := FRenamedBlockRecords[I];
    end;
    FreeAndNil(FRenamedBlockRecords);
  end;
  vConverter := TsgCADConverter(Converter);
  C := vConverter.EntityDictionaries.Count - 1;
  vHashList := vConverter.EntityDictionaries.List;
  for I := C downto 0 do
  begin
    vDictionaryItem := TsgTableItem(vHashList^[I].Data);
    vDictionary := vDictionaryItem.Item;
    vXrecord := TsgDXFXRecord(vDictionary.FindEntByName(sInternalData));
    if Assigned(vXrecord) then
    begin
      vXrecord.Owner.RemoveEntity(vXrecord);
      vXrecord.Free;
      if vDictionary.Count = 0 then
      begin
        vConverter.EntityDictionaries.Delete(I);
        if vDictionary.Owner <> nil then
          vDictionary.Owner.RemoveEntity(vDictionary);
        vDictionaryItem.Item := nil;
        vDictionaryItem.Free;
      end;
    end;
  end;
end;

procedure TsgCADtoDXF.BeginObject(const AName: string);
begin
  AddString(0, AName);
end;

procedure TsgCADtoDXF.BeginSection(const AName: string);
begin
  BeginObject('SECTION');
  AddString(2, AName);
end;

procedure TsgCADtoDXF.BeginTable(AGroup: TsgDXFGroup; const AName: string);
begin
  BeginObject('TABLE');
  AddString(2, AName);
  AddHandle(AGroup, cnstBadHandle, []);
  AddSubclassMarker('SymbolTable');
end;

procedure TsgCADtoDXF.EndOfFile(APos: {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF});
var
  vStr: string;
begin
  BeginObject('EOF');
  if APos > 0 then
  begin
    // Setting correct value of next available handle
    FStream.Position := APos;
    vStr := IntToHex(DoHandle, cnstHexDigits);
    SetLength(vStr, cnstHexDigits);
    AddString(5, vStr);
  end;
end;

procedure TsgCADtoDXF.EndSection;
begin
  BeginObject('ENDSEC');
end;

procedure TsgCADtoDXF.EndTable;
begin
  BeginObject('ENDTAB');
end;

{ Export Entities }

{ 3DFACE }
function TsgCADtoDXF.Export3DFace(F3D: TsgDXF3DFace): Boolean;
begin
  AddCommonEntityGroup(F3D);
  Add3DPoint(10, F3D.Point);
  Add3DPoint(11, F3D.Point1);
  Add3DPoint(12, F3D.Point2);
  Add3DPoint(13, F3D.Point3);
  AddInt(cFlag_70, TsgDXF3dFaceAccess(F3D).Flags);
  ExportExtendedData(F3D);
  Result := True;
end;

function TsgCADtoDXF.ExportAcadTable(AcadTable: TsgDXFAcadTable): Boolean;
var
  I, J: Integer;
  vBlockRecordHandle: UInt64;
  vCell: TsgAcadTableCellAccess;

  procedure ExportOverriden(const AIndex: Integer; ACell: TsgAcadTableCellAccess);
  begin
    if ACell.Flags and ACell.GetFlagsMask(AIndex) <> 0 then
    begin
      AddInt(AIndex + 274, CorrectLineWeight(ACell.BorderLineWeight[AIndex], False));
      AddInt(AIndex + 284, Ord(ACell.BorderVisible[AIndex]));
      AddInt(AIndex + 64, ColorToDXF(ACell.BorderColor[AIndex]));
    end;
  end;

begin
  if not Assigned(AcadTable.Style) then
    Result := ExportInsert(AcadTable)
  else
  begin
    { TODO: Group 1
      AddCommonEntityGroupEx(AcadTable, sCADEntityNames[AcadTable.EntType],
      'BlockReference'); }
    AddCommonEntityGroupEx(AcadTable, 'ACAD_TABLE', 'BlockReference');
    AddLineType(AcadTable.LineType);
    if AcadTable.Attribs.Count > 0 then
      AddInt(66, 1);
    AddBlockName(AcadTable.BlockRecord.Name);
    Add3DPoint(10, AcadTable.Point);
    if IsScaled(AcadTable.Scale) then
    begin
      AddFloat(41, AcadTable.Scale.X);
      AddFloat(42, AcadTable.Scale.Y);
      AddFloat(43, AcadTable.Scale.Z);
    end;
    if AcadTable.Angle <> 0 then
      AddFloat(50, AcadTable.Angle);
    { TODO: Group 1
      AddSubclassMarker(sCADEntitySubNames[AcadTable.EntType]); }
    AddSubclassMarker('Table');
    AddHexInt(342, AcadTable.Style.Handle);
    vBlockRecordHandle := GetHandleByEntity(AcadTable.BlockRecord);
    if vBlockRecordHandle <> cnstBadHandle then
      AddHexInt(343, vBlockRecordHandle);
    AddInt(90, AcadTable.Flags);
    AddInt(91, AcadTable.NumRows);
    AddInt(92, AcadTable.NumCols);
    // Flags for an override
    //AddInt(93, 0);
    //AddInt(94, 0);  // of border color
    //AddInt(95, 0);  // of border lineweight
    //AddInt(96, 0);  // of border visibility
    for I := 0 to AcadTable.NumRows - 1 do
      AddFloat(141, AcadTable.RowHeight[I]);
    for I := 0 to AcadTable.NumCols - 1 do
      AddFloat(142, AcadTable.ColWidth[I]);
    for I := 0 to AcadTable.NumRows - 1 do
      for J := 0 to AcadTable.NumCols - 1 do
      begin
        vCell := TsgAcadTableCellAccess(AcadTable.Cell[J, I]);
        AddInt(171, Ord(not vCell.IsTextType) + 1);
        AddInt(172, vCell.Flags);
        AddInt(173, vCell.MergedValue);
        AddInt(174, Ord(vCell.AutoFit));
        AddInt(175, vCell.MergedWidth);
        AddInt(176, vCell.MergedHeight);
        AddInt(177, vCell.OverrideFlag);
        AddInt(178, Ord(vCell.VirtualEgdeFlag));
        AddFloat(145, vCell.Rotation);
        if vCell.IsTextType then
          AddString(1, vCell.Text);
        if Assigned(vCell.TextStyle) then
          AddString(7, ExtractName(vCell.TextStyle.Name));
        if (vCell.OverrideFlag and $20 <> 0) or not vCell.HasOwnerStyle then
          AddFloat(140, vCell.TextHeight);
        if vCell.IsPropInit(cpCellAllignment) or not vCell.HasOwnerStyle then
          AddInt(170, vCell.CellAlignment);
        if vCell.IsPropInit(cpTextColor) or not vCell.HasOwnerStyle then
          AddInt(64, ColorToDXF(vCell.TextColor));
        if vCell.BkColorOn then
          AddInt(63, ColorToDXF(vCell.FillColor));
        ExportOverriden(5, vCell);
        ExportOverriden(1, vCell);
        ExportOverriden(2, vCell);
        ExportOverriden(4, vCell);
      end;
    ExportExtendedData(AcadTable);
    Result := True;
  end;
end;

function TsgCADtoDXF.ExportCADFill(const ACADFill: TsgCADFill): Boolean;
var
  vImageEnt: TsgDXFImageEnt;
begin
  Result := False;
  if not Assigned(DXFConv.LoadEntityProc) then
  begin
    vImageEnt := ACADFill.ConvertToImageEnt(Converter);
    try
      if vImageEnt <> nil then
        Result := ExportImageEnt(vImageEnt);
    finally
      vImageEnt.Free;
    end;
  end;
end;

function TsgCADtoDXF.ExportChildEntities(AEntity: TsgDXFEntity): Boolean;
var
  I: Integer;
begin
  for I := 0 to AEntity.Count-1 do
    DoExport(AEntity[I]);
  Result := True;
end;

class function TsgCADtoDXF.ACIS_Row_Encode(const ADecoded: AnsiString): AnsiString;
var
  vChar: AnsiChar;
  I: Integer;
begin
  Result := '';
  for I := cnstStrBegin to Length(ADecoded) + (cnstStrBegin - 1) do
  begin
    vChar := ACIS_Cipher(ADecoded[I]);
    if vChar < ' ' then
    begin
      Result := Result + '^';//#94
      vChar := AnsiChar(Byte(vChar) + Byte('@'));//#64
    end;
    Result := Result + vChar;
    if vChar = '^' then
      Result := Result + ' ';
  end;
end;

{ ACIS (REGION, SURFACE, BODY, 3DSOLID) }
function TsgCADtoDXF.ExportACISEntity(ACIS: TsgBrepModAcis): Boolean;
var
  vChar: AnsiChar;
  vACISRow: AnsiString;
  vSaveVer: Word;
  vMemoryStream: TMemoryStream;

  procedure WriteACISRow(const ARow: AnsiString);
  var
    vEncoded, vFirstPart: AnsiString;
  begin
    vEncoded := ACIS_Row_Encode(ARow);
    while Length(vEncoded) > 255 do
    begin
      vFirstPart := Copy(vEncoded, 1, 255);
      Delete(vEncoded, 1, 255);
      AddString(3, string(vFirstPart));
    end;
    AddString(1, string(vEncoded));
  end;

begin
  Result := False;

  if ConvertAcis then
  begin
    Result := ExportGroup(ACIS, 0, CreateExportData);
    ExportExtendedData(ACIS);
    Exit;
  end;


  if Version >= acR2013 then
  begin
    FACISEntities.Add(ACIS);
    AddCommonEntityGroup(ACIS);
    AddInt(290, 0);
    AddString(2, '{00000000-0000-0000-0000-000000000000}');
    Result := True;
    Exit;
  end;

  vMemoryStream := GetACISStream(ACIS, vSaveVer);
  if vMemoryStream = nil then
    Exit;
  AddCommonEntityGroup(ACIS);
  AddInt(70, 1);
  vMemoryStream.Position := 0;
  try
    vACISRow := '';
    while vMemoryStream.Read(vChar, 1) > 0 do
      case vChar of
      #10, #13:
        if vAcisRow <> '' then
        begin
          WriteACISRow(vACISRow);
          vACISRow := '';
        end;
      #3, #5, #9, #26:
          vACISRow := vACISRow + ' ';
      else
        vACISRow := vACISRow + vChar;
      end;
    if vACISRow <> '' then
      WriteACISRow(vACISRow);
  finally
    vMemoryStream.Free;
  end;
  Result := True;
end;

function TsgCADtoDXF.ExportSurface(AEnt: TsgDXFSurface): Boolean;
begin
  Result := ExportACISEntity(AEnt);
  if not Result then
    Exit;
  if ConvertAcis then
    Exit;
  AddSubClassMarker(cnstSurface);
  AddInt(71, AEnt.IsoCountU);
  AddInt(72, AEnt.IsoCountV);
end;

function TsgCADtoDXF.Export3dSolid(AEnt: TsgDXF3dSolid): Boolean;
begin
  Result := ExportACISEntity(AEnt);
  if not Result then
    Exit;
  if ConvertAcis then
    Exit;
  if Version >= acR2007 then
  begin
    AddSubClassMarker(cnst3dSolid);
    AddHexInt(cntCodeSoftOwnerHandle, cnstBadHandle);
  end;
end;

{ ARC }
function TsgCADtoDXF.ExportArc(A: TsgDXFArc): Boolean;
begin
  AddCommonEntityGroup(A);
  AddThickness(A.ZThick);
  Add3DPoint(10, A.Point);//  Add3DPointExtruded(10, A.Point, A.Extrusion);
  AddFloat(40, A.Radius);
  AddSubclassMarker('Arc');
  AddFloat(50, A.StartAngle);
  AddFloat(51, A.EndAngle);
  ExportExtendedData(A);
  Result := True;
end;

{ CIRCLE }
function TsgCADtoDXF.ExportCircle(C: TsgDXFCircle): Boolean;
begin
  AddCommonEntityGroup(C);
  AddThickness(C.ZThick);
  Add3DPoint(10, C.Point);//  Add3DPointExtruded(10, C.Point, C.Extrusion);
  AddFloat(40, C.Radius);
  ExportExtendedData(C);
  Result := True;
end;

{ HATCH }
function TsgCADtoDXF.ExportHatch(PP: TsgCADPolyPolygon): Boolean;
const
  cnstPatternNames: array[0 .. 2] of string = ('USER', '_SOLID', sPatternSOLID);
var
  K: Integer;
  vIsDerived: Boolean;
  vGradient: TsgCADGradientPolygon;
  vColor: TsgColorCAD;

  procedure AddPatternData;
  var
    H: TsgCADHatchAccess;
    vPatternData: TsgHatchPatternData;
    vDashLengths: TsgDoubleList;
    I, J, N: Integer;
  begin
    H := TsgCADHatchAccess(PP);
    N := IndexOfStrings(AnsiUpperCase(H.HatchName), cnstPatternNames);
    AddInt(76, Ord(N <> 0));// Hatch pattern type (1 = predefined) (not user)
    if (H.PatternData <> nil) and not (N in [1, 2]) then // (not solid)
    begin
      AddFloat(52, H.PatternAngle);// Hatch pattern angle
      AddFloat(41, H.PatternScale);// Hatch pattern scale or spacing
      AddInt(77, Ord(H.DoubleHatch));// Hatch pattern double flag (pattern fill only): 0 = not double; 1 = double
      AddInt(78, H.HatchPatternData.Count);
      for I := 0 to H.HatchPatternData.Count - 1 do
      begin
        vPatternData := PsgHatchPatternData(H.HatchPatternData[I])^;
        AddFloat(53, vPatternData.LineAngle);
        AddFloat(43, vPatternData.BaseP.X);
        AddFloat(44, vPatternData.BaseP.Y);
        AddFloat(45, vPatternData.Offset.X);
        AddFloat(46, vPatternData.Offset.Y);
        // Dash length (multiple entries)
        vDashLengths := vPatternData.Lines;
        if vDashLengths <> nil then
        begin
          AddInt(79, vDashLengths.Count);
          for J := 0 to vDashLengths.Count - 1 do
            AddFloat(49, vDashLengths[J]);
        end
        else
          AddInt(79, 0);
      end;
    end;
  end;

  procedure AddPolylineBoundary(ABoundaryList: TF2DPointList);
  var
    I: Integer;
  begin
    (*// Polyline edge type
    vHasPolyBoundary := True;
    AddInt(92, 7);
    AddInt(72, 0);
    AddInt(73, 0);
    AddInt(93, ABoundaryList.Count);
    for I := 0 to ABoundaryList.Count - 1 do
      Add2DPoint(10, PF2DPoint(ABoundaryList[I])^);
    AddInt(97, 0); *)
    // Line edge type
    AddInt(92, 1);
    AddInt(93, ABoundaryList.Count);
    for I := 1 to ABoundaryList.Count - 1 do
    begin
      AddInt(72, 1);// Edge type = Line
      Add2DPoint(10, ABoundaryList[I-1]);
      Add2DPoint(11, ABoundaryList[I]);
    end;
    // To close
    AddInt(72, 1);
    Add2DPoint(10, ABoundaryList.Last);// last point
    Add2DPoint(11, ABoundaryList.First);// first point
    AddInt(97, 0); 
  end;

  procedure AddBoundaryPathData(ABoundaryList: Tsg2DBoundaryList);
  var
    vBoundary: Tsg2DCurve;
    vPoly: Tsg2DPolylineEx;
    vSpline: Tsg2DSplineEx;
    I, J: Integer;
  begin
    AddInt(92, ABoundaryList.BoundaryType);
    vIsDerived := vIsDerived or (ABoundaryList.BoundaryType and 4 <> 0);
    if ABoundaryList.BoundaryType and 2 <> 0 then
    begin
      vPoly := Tsg2DPolylineEx(ABoundaryList[0]);
      if PP.EntType = ceMPolygon then
      begin
        AddInt(73, 1);//undefined
        AddInt(72, Ord(vPoly.BulgesCount > 0));
      end
      else
      begin
        AddInt(72, Ord(vPoly.BulgesCount > 0));
        AddInt(73, Ord(vPoly.Closed));
      end;  
      AddInt(93, vPoly.VertexesCount);
      for J := 0 to vPoly.VertexesCount - 1 do
      begin
        Add2DPoint(10, vPoly.Vertexes[J]);
        if vPoly.BulgesCount > 0 then
          AddFloat(42, vPoly.Bulges[J]);
      end;
      if PP.EntType <> ceMPolygon  then
        AddInt(97, 0);// Number of source boundary objects
      // Reference to source boundary objects
      //AddString(cParentHandle_330, '0');// default = 0 - not source
    end
    else
    begin
      AddInt(93, ABoundaryList.Count);
      for I := 0 to ABoundaryList.Count - 1 do
      begin
        vBoundary := Tsg2DCurve(ABoundaryList[I]);
        AddInt(72, vBoundary.EdgeType);
        case vBoundary.EdgeType of
          1:// Line
            begin
              Add2DPoint(10, vBoundary.StartPoint);
              Add2DPoint(11, vBoundary.EndPoint);
            end;
          2:// Circular arc
            begin
              Add2DPoint(10, Tsg2DArc(vBoundary).CenterPoint);
              AddFloat(40, Tsg2DArc(vBoundary).Radius);
              AddFloat(50, Tsg2DArc(vBoundary).StartParam);
              AddFloat(51, Tsg2DArc(vBoundary).EndParam);
              AddInt(73, Ord(Tsg2DArc(vBoundary).CounterClockWise));
            end;
          3:// Ellipse
            begin
              Add2DPoint(10, Tsg2DEllipse(vBoundary).CenterPoint);
              Add2DPoint(11, Tsg2DEllipse(vBoundary).MajorPoint);
              AddFloat(40, Tsg2DEllipse(vBoundary).Radius);
              AddFloat(50, Tsg2DEllipse(vBoundary).StartParam);
              AddFloat(51, Tsg2DEllipse(vBoundary).EndParam);
              AddInt(73, Ord(Tsg2DEllipse(vBoundary).CounterClockWise));
            end;
          4:// Spline
            begin
              vSpline := Tsg2DSplineEx(vBoundary);
              AddInt(94, vSpline.Degree);
              AddInt(73, 0);
              AddInt(74, 0);
              AddInt(95, vSpline.KnotsCount);
              AddInt(96, vSpline.ControlsCount);
              for J := 0 to vSpline.KnotsCount - 1 do
                AddFloat(40, vSpline.Knots[J]);
              for J := 0 to vSpline.ControlsCount - 1 do
                Add2DPoint(10, vSpline.Controls[J]);
              if Version >= acR2010 then
                AddInt(97, 0{FitCount});
            end;
        end;
      end;// end of for (ABoundaryList.Count)
      AddInt(97, 0);
    end;
  end;

begin
  if PP is TsgCADFill then
  begin
    Result := ExportCADFill(TsgCADFill(PP));//change in future version
    Exit;
  end;
  vIsDerived := False;
  if Version <= acR2000 then
  begin
    vColor := PP.ColorCAD;
    try
      PP.ColorCAD := GetDXFEntColor(PP, Version);
      AddCommonEntityGroup(PP);
    finally
      PP.ColorCAD := vColor;
    end;
  end
  else
    AddCommonEntityGroup(PP);
  if PP is TsgCADCurvePolygon then
  begin
    Add3DPoint(10, TsgCADCurvePolygon(PP).Elevation);
    Add3DPoint(210, TsgCADCurvePolygon(PP).Extrusion);// always to be added Extrusion
  end
  else
  begin
    Add3DPoint(10, cnstFPointZero);
    Add3DPoint(210, cnstZOrtAxis);
  end;
  AddString(2, PP.HatchName);
  if PP.EntType <> ceMPolygon then
  begin
    AddInt(cFlag_70, Integer(PP.SolidFill));
    AddInt(71, 0);//associativity flag
  end  
  else
    AddInt(71, Integer(PP.SolidFill));
  if PP is TsgCADCurvePolygon then
  begin
    AddInt(91, TsgCADCurvePolygon(PP).BoundaryDataCount);
    for K := 0 to TsgCADCurvePolygon(PP).BoundaryDataCount-1 do
      AddBoundaryPathData(TsgCADCurvePolygon(PP).BoundaryData[K]);
    if PP.EntType <> ceMPolygon then
      AddInt(75, 0);
    //if (not PP.SolidFill) and (TsgCADHatch(PP).HatchPatternData.Count > 0) then
      AddPatternData
    //else
      //AddInt(76, 1);// Hatch pattern type (1 = predefined)
  end
  else
  begin
    AddInt(91, PP.Boundaries.Count);
    for K := 0 to PP.Boundaries.Count-1 do
      AddPolylineBoundary(TF2DPointList(PP.Boundaries[K]));
    AddInt(75, 0);
    AddInt(76, 1);
  end;
  if PP.EntType = ceMPolygon then
  begin
    if TsgCADCurvePolygon(PP).FillColor.Active = acIndexColor then
    begin
      AddInt(IfThen(Ord(Version) >= Ord(acR2004), 63, 94),
       TsgCADCurvePolygon(PP).FillColor.Color);
    end
    else if TsgCADCurvePolygon(PP).FillColor.Active = acRGBColor then
    begin
      AddInt(IfThen(Ord(Version) >= Ord(acR2004), 63, 94),
        ColorToDXF(TsgCADCurvePolygon(PP).FillColor));
    end;
    Add2DFlatPoint(11, TsgCADCurvePolygon(PP).Offset);
    AddInt(99, 0);
  end
  else
  begin
    if vIsDerived then
      AddFloat(47, 0.000001);
    AddInt(98, 0);// Number of seed points (usually one point)
    //Add3DPoint(10, MakeFPoint(0,0,0));// Seed point, i.e. point inside area "for filling"
  end;  
  if Ord(Version) >= Ord(acR2004) then
  begin
    if (PP.EntType = ceMPolygon) or IsCADGradient(PP) then// Add gradient
    begin
      vGradient := TsgCADGradientPolygon(PP);
      AddInt(450, Integer(PP.EntType <> ceMPolygon));
      AddInt(451, 0);
      AddFloat(460, vGradient.GradientAngle * fPiDividedBy180);
      AddFloat(461, Integer(not vGradient.GradientUseCenter));
      AddInt(452, 1);//Single-color gradient
      AddFloat(462, 1.0);
      AddInt(453, 2);
      AddFloat(463, 0.0);
      AddInt(63, GetCADColor(vGradient.GradientColor[0], True));
      AddInt(421, BGRToRGB(vGradient.GradientColor[0]));
      AddFloat(463, 1.0);
      AddInt(63, GetCADColor(vGradient.GradientColor[1], True));
      AddInt(421, BGRToRGB(vGradient.GradientColor[1]));
      AddString(470, vGradient.GradientName);
    end;
  end;
  ExportExtendedData(PP);
  Result := True;
end;

function TsgCADtoDXF.ExportImageEntByFile(W: TsgDXFImageEnt): Boolean;
begin
  Result := ExportImageEntAsExternalFile(W, DoHandle, ExportWipeOut, nil);
end;

function TsgCADtoDXF.ExportImageEntByOle(W: TsgDXFImageEnt): Boolean;
begin
  Result := ExportImageEntAsOle(W, ExportOle2Frame, nil);
end;

function TsgCADtoDXF.ExportImageEnt(W: TsgDXFImageEnt): Boolean;
begin
  case GetExportImageIndex(W, ExportImageFileIndex) of
    cntImageExportAsImage:
      Result := ExportWipeOut(TsgCADWipeOut(W));
    cntImageExportAsOLE2Frame:
      Result := ExportImageEntByOle(W);
    cntImageExportAsFile:
      Result := ExportImageEntByFile(W);
  else
    Result := False;
  end;
end;

{ INSERT }
function TsgCADtoDXF.ExportInsert(INS: TsgDXFInsert): Boolean;
var
  I, vAttribsCount: Integer;
  MINS: TsgCADMInsert absolute INS;
  P, S: TFPoint;
  A: Double;
  vBlockName: string;
  vFlags: Byte;
begin
  Result := False;
  if INS.Block = nil then Exit;
  vBlockName := INS.Block.Name;
  if IsNameInternal(vBlockName) then
    Exit;
  if Assigned(FSGBlock) and (vBlockName = FSGBlock.Name) then
{$IFNDEF SG_ENCRYPT_FILES}
    if INS.Block <> FSGBlock then Exit;
{$ELSE}
      Exit;
{$ENDIF}
  vAttribsCount := INS.Attribs.Count;
  AddCommonEntityGroup(INS);
  AddLineType(INS.LineType);
  if vAttribsCount > 0 then
    AddInt(66, 1);
  vFlags := INS.Block.Flags;
  CheckBlockName(vBlockName, vFlags);
  AddBlockName(vBlockName);
  if INS.EntClass.EG = gtSVG then
  begin
    INS.ExtractParamsFromMatrix2D(P, S, A);
    Add3DPoint(10, P);
    AddFloat(41, S.X);
    AddFloat(42, S.Y);
    AddFloat(43, S.Z);
    if A <> 0 then
      AddFloat(50, A);
  end
  else
  begin
    Add3DPoint(10, INS.Point);//  Add3DPointExtruded(10, INS.Point, INS.Extrusion);
    AddFloat(41, INS.Scale.X);
    AddFloat(42, INS.Scale.Y);
    AddFloat(43, INS.Scale.Z);
    if INS.Angle <> 0 then
      AddFloat(50, INS.Angle);
    // Add column and raw here
    AddExtrusion(INS.Extrusion);
  end;

  if INS.ClassType = TsgCADMInsert then
  begin
    AddInt(70, MINS.NumCols);
    AddInt(71, MINS.NumRows);
    AddFloat(44, MINS.ColSpacing);
    AddFloat(45, MINS.RowSpacing);
  end;
  // Extended data before Attribs
  ExportExtendedData(INS);

  if vAttribsCount > 0 then
  begin
    for I := 0 to vAttribsCount - 1 do
      ExportText(TsgDXFAttrib(INS.Attribs[I]));
    AddEntName(nil, 'SEQEND');
  end;
  Result := True;
end;

function TsgCADtoDXF.ExportKnownEED(const AEntity: TsgDXFEntity;
  AFilter: TsgInt64List = nil): Boolean;
var
  vCount: Integer;

  procedure DoExportKnownEED(const AAppIDName: string; AFilter: TsgInt64List;
    var ACount: Integer);
  var
    vExtData: TsgCADExtendedData;
    vEnt: TsgDXFEntity;
  begin
    if Assigned(FAppIDs) then
    begin
      vEnt := TAppIDs(FAppIDs).Entity[AAppIDName];
      if Assigned(vEnt) then
      begin
        vExtData := TsgCADExtendedData.Create(Version);
        try
          if AEntity.GetExtData(vExtData, AAppIDName) and
             ConvertDWGExtDataToDXF(vExtData) then
          begin
            DoExportEED(vExtData);
            Inc(ACount);
            if Assigned(AFilter) then
              AFilter.Add(vEnt.Handle);
          end;
        finally
          vExtData.Free;
        end;
      end;
    end;
  end;

begin
  vCount := 0;
  DoExportKnownEED(sACADXDataAppName, AFilter, vCount);
  if Version < acR2007 then
    DoExportKnownEED(sACADXDataAttribAppName, AFilter, vCount);
  DoExportKnownEED(XDataAppName, AFilter, vCount);
  DoExportKnownEED(sURLXDataName, AFilter, vCount);
  Result := vCount > 0;
end;

{ DIMENSION }
function TsgCADtoDXF.ExportDimension(D: TsgDXFDimension): Boolean;
const
  cnstDimension = 'Dimension';
  cnstDimSubclasses: array [0..7] of string = ('Rotated', 'Aligned',
    '2LineAngular', 'Diametric', 'Radial', '3PointAngular', 'Ordinate', 'Arc');
var
  DimEx: TsgDXFDimensionEx absolute D;
  vIsArcDimension: Boolean;
  vDimType, vFlag: Integer;
  S: string;
begin
  Result := False;
  if D.Block = nil then Exit;
  vFlag := D.Flags;
  vDimType := vFlag and 7;
  vIsArcDimension := vDimType = 7;
  if vIsArcDimension then
    AddCommonEntityGroupEx(D, cntClassDXFARC_DIMENSION, sCADEntitySubNames[D.EntType])
  else
    AddCommonEntityGroup(D);
  if D.TextOverride <> '' then
  begin
    S := D.TextOverride;
    PrepareExportText(S);
    AddString(1, S);
  end;
  if D.Style <> nil then AddString(3, D.Style.Name);
  AddString(2, GetCorrectBlockName(D.Block.Name));
  Add3DPoint(10, D.DefPoint);
  Add3DPoint(11, D.MiddlePoint);
  AddExtrusion(D.Extrusion);
  if vIsArcDimension then
    vFlag := (vFlag and 224) or 5
  else
  begin
    if (vDimType > 6) or (vDimType < 0) then
      vDimType := 1;
    vFlag := (vFlag and 224) or vDimType;
  end;
  AddInt(cFlag_70, vFlag);
  if D.TextRotation <> 0 then
    AddFloat(53, D.TextRotation);
  case vDimType of
    7: //'Arc'
       begin
        AddSubclassMarker(cnstDimSubclasses[vDimType]+cnstDimension);
        Add3DPoint(13, D.LinDefPoint1);//Extension line 1 point 3BD 13
        Add3DPoint(14, D.LinDefPoint2);//Extension line 2 point 3BD 14
        Add3DPoint(15, D.RadDefPoint);//Arc center 3BD 15
        AddInt(cFlag_70, 0);//Is partial? B 70
        AddFloat(40, 0);//Start angle (radians) BD 40
        AddFloat(41, 0);//End angle (radians) BD 41
        AddInt(71, 0);//Has leader? B 71
        Add3DPoint(16, D.ArcDefPoint);//Leader point 1 3BD 16 cnstFPointZero??
        Add3DPoint(17, cnstFPointZero);//Leader point 2 3BD 17
       end;
    2, 5: //'2LineAngular' '3PointAngular',
      begin
        AddSubclassMarker(cnstDimSubclasses[vDimType]+cnstDimension);
        Add3DPoint(13, D.LinDefPoint1);
        Add3DPoint(14, D.LinDefPoint2);
        Add3DPoint(15, D.RadDefPoint);
        if vDimType <> 5 then
          Add3DPoint(16, D.ArcDefPoint);
      end;
    3, 4: //'Diametric' 'Radial'
      begin
        AddSubclassMarker(cnstDimSubclasses[vDimType]+cnstDimension);
        Add3DPoint(15, D.RadDefPoint);
        //AddFloat(40, 0);
      end;
    6:    //'Ordinate'
      begin
        AddSubclassMarker(cnstDimSubclasses[vDimType]+cnstDimension);
        Add3DPoint(13, D.LinDefPoint1);
        Add3DPoint(14, D.LinDefPoint2);
      end;
  else
    Add3DPointExtruded(12, D.Point, D.Extrusion);
    AddSubclassMarker(cnstDimSubclasses[1] + cnstDimension);
    Add3DPoint(13, D.LinDefPoint1);
    Add3DPoint(14, D.LinDefPoint2);
    if D.DimRot <> 0 then
      AddFloat(50, D.DimRot);
    if vDimType = 0 then
      AddSubclassMarker(cnstDimSubclasses[0] + cnstDimension);
  end;
  ExportExtendedData(D);
  Result := True;
end;

{ ELLIPSE }
function TsgCADtoDXF.ExportEllipse(E: TsgDXFEllipse): Boolean;
begin
  if Ord(Version) > Ord(acR12) then
  begin
    AddCommonEntityGroup(E);
    //AddThickness(E.ZThick);
    Add3DPointPreExtruded(10, E.Point, E.Extrusion);
    Add3DPointPreExtruded(11, E.RadPt, E.Extrusion);
    AddFloat(40, E.Ratio);
    if Abs(E.StartAngle - E.EndAngle) > fAccuracy then
    begin
      AddFloat(41, Radian(E.StartAngle));
      AddFloat(42, Radian(E.EndAngle));
    end;
  end
  else
    ExportPolyLine(E);
  Result := True;
end;

function TsgCADtoDXF.ExportEntityAsGroup(const E: TsgDXFEntity): Boolean;
begin
  Result := ExportGroup(E, 0, CreateExportData);
  ExportExtendedData(E);
end;

function TsgCADtoDXF.ExportExtearnalEED(const AEntHandle: UInt64;
  AFilter: TsgInt64List = nil): Boolean;
var
  I, J, C: Integer;
  vEEDItems: TsgObjectCollection;
  vHashItem: PsgHashItemObject;
  vEED: TsgCADExtendedData;
begin
  C := 0;
  J := EntityEEDItems.IndexOf(AEntHandle);
  if J >= 0 then
  begin
    vEEDItems := TsgObjectCollection(EntityEEDItems.List^[J].Data);
    if Assigned(vEEDItems) then
      for I := 0 to vEEDItems.Count - 1 do
      begin
        vHashItem := @vEEDItems.List^[I];
        if not Assigned(AFilter) or (AFilter.IndexOf(vHashItem^.HashCode) = -1) then
        begin
          vEED := TsgCADExtendedData.Create(Version);
          try
            vEED.AssignData(TsgCADExtendedData(vHashItem^.Data));
            if ConvertDWGExtDataToDXF(vEED) then
            begin
              DoExportEED(vEED);
              Inc(C);
              if Assigned(AFilter) then
                AFilter.Add(vHashItem^.HashCode);
            end;
          finally
            vEED.Free;
          end;
        end;
      end;
    EntityEEDItems.Delete(J);
  end;
  Result := C > 0;
end;

function TsgCADtoDXF.ExportExtendedData(const AEntity: TsgDXFEntity): Boolean;
var
  vAdded: TsgInt64List;
begin
  vAdded := TsgInt64List.Create;
  try
    vAdded.Sorted := True;
    ExportKnownEED(AEntity, vAdded);
    ExportExtearnalEED(AEntity.Handle, vAdded);
    Result := vAdded.Count > 0;
  finally
    vAdded.Free;
  end;
end;

procedure TsgCADtoDXF.ExportExtendedDictionaries;
{var
  I, C: Integer;
  vList: PsgHashItemsArray;
  vItem: TsgDictionaryItem;
  vAcadFilterDict: TsgDXFEntity;
  vFilter: TsgDXFEntity;
  vXrecord: TsgDXFXRecord;
begin
  C := TsgCADConverter(Converter).EntityDictionaries.Count;
  vList := TsgCADConverter(Converter).EntityDictionaries.List;
  for I := 0 to C - 1 do
  begin
    vItem := TsgDictionaryItem(vList^[I].Data);
    case vItem.Entity.EntType of
      ceInsert:
        begin
          vAcadFilterDict := vItem.Item.FindEntByName(sAcadFilterDictionary);
          if Assigned(vAcadFilterDict) then
          begin
            vFilter := vAcadFilterDict.FindEntByName(cnstSpatial);
            if Assigned(vFilter) then
            begin
              AddDict(vItem.Item.Handle, vItem.Entity.Handle);
              AddInt(280, 1); //Hard-owner flag
              AddInt(281, vItem.Item.Flags);
              AddDictRefs(sAcadFilterDictionary, vAcadFilterDict.Handle, True);

              AddDict(vAcadFilterDict.Handle, vItem.Item.Handle);
              AddInt(280, 1); //Hard-owner flag
              AddInt(281, vAcadFilterDict.Flags);
              AddDictRefs(cnstSpatial, vFilter.Handle, True);

              ExportSpatial(TsgCADSpatialFilter(vFilter));
            end;
          end;
        end;
      ceAttdef, ceAttrib:
        begin
          vXrecord := TsgDXFXRecord(vItem.Item.FindEntByName(cnstAcadMLAtt));
          if Assigned(vXrecord) then
          begin
            AddDict(vItem.Item.Handle, vItem.Entity.Handle);
            AddInt(280, 1); //Hard-owner flag
            AddInt(281, vItem.Item.Flags);
            AddDictRefs(cnstAcadMLAtt, vXrecord.Handle, True);

            ExportXrecord(vXrecord);
          end;
        end;
    else
      if vItem.Entity is TsgDXFBlockRecord then
      begin
        vXrecord := TsgDXFXRecord(vItem.Item.FindEntByName(sInternalData));
        if Assigned(vXrecord) then
        begin
          AddDict(vItem.Item.Handle, vItem.Entity.Handle);
          AddInt(280, 1); //Hard-owner flag
          AddInt(281, vItem.Item.Flags);
          AddDictRefs(vXrecord, True);
          ExportXrecord(vXrecord);
        end;
      end;
    end;
  end;
end;}
var
  I, J, C, vCount: Integer;
  vList: PsgHashItemsArray;
  vItem: TsgTableItem;
  vItems: array of TsgDXFEntity;
  vFilter: TsgDXFEntity;
  E: TsgDXFEntity;
begin
  C := TsgCADConverter(Converter).EntityDictionaries.Count;
  vList := TsgCADConverter(Converter).EntityDictionaries.List;
  for I := 0 to C - 1 do
  begin
    vItem := TsgTableItem(vList^[I].Data);
    SetLength(vItems, vItem.Item.Count);
    vCount := 0;
    vFilter := nil;
    for J := 0 to vItem.Item.Count - 1 do
    begin
      E := vItem.Item[J];
      if E is TsgDXFXRecord then
      begin
        vItems[vCount] := E;
        Inc(vCount);
      end
      else
        if (E.Name = sAcadFilterDictionary) and (E is TsgDXFDictionary) then
        begin
          vFilter := E.FindEntByName(cnstSpatial);
          if Assigned(vFilter) then
          begin
            vItems[vCount] := E;
            Inc(vCount);
          end;
        end;
    end;
    if vCount > 0 then
    begin
      AddDict(vItem.Item.Handle, vList^[I].HashCode);
      AddInt(280, 1); //Hard-owner flag
      AddInt(281, vItem.Item.Flags);
      for J := Low(vItems) to vCount - 1 do
        AddDictRefs(vItems[J], True);
      for J := Low(vItems) to vCount - 1 do
        if Assigned(vFilter) and (vFilter.Owner = vItems[J]) then
        begin
          AddDict(vItems[J].Handle, vItems[J].Owner.Handle);
          AddInt(280, 1); //Hard-owner flag
          AddInt(281, vItems[J].Flags);
          AddDictRefs(vFilter, True);
          ExportSpatial(TsgCADSpatialFilter(vFilter));
        end
        else
          ExportXrecord(TsgDXFXRecord(vItems[J]));
    end;
  end;
end;

{ FlatHatch entity }
function TsgCADtoDXF.ExportFlatHatch(FH: TsgFlatHatch): Boolean;
var
  I, K, vCount, vSubCount, vIndex: Integer;
begin
  vIndex := 0;
  I := 0;
  while I < FH.Counts.Count do
  begin
    vCount := Integer(FH.Counts[I]);
    Inc(I);
    //if (vIndex + vCount) > FH.PCount then Break;
    if vCount = 0 then Break;
    AddCommonEntityGroup(FH);
    Add3DPoint(10, cnstFPointZero);
    Add3DPoint(210, cnstZOrtAxis);
    AddString(2, sSOLID);
    AddInt(cFlag_70, 1);
    AddInt(71, 0);
    AddInt(91, vCount);
    (*// Polyline does not work in AutoCAD (AutoCAD bug)
    AddInt(92, 2);
    for J := 0 to vCount - 1 do
    begin
      AddInt(72, 0);
      AddInt(73, 1);
      vSubCount := Integer(FH.Counts[I]);
      Inc(I);
      AddInt(93, vSubCount);
      for K := vIndex to (vIndex + vSubCount - 1) do
        Add2DFlatPoint(10, FH.XY[K]);
      AddInt(97, 0);
      Inc(vIndex, vSubCount);
    end;  *)
    // 'Line'-boundarytype works in AutoCAD
    while vCount > 0 do
    begin
      AddInt(92, 1);
      vSubCount := Integer(FH.Counts[I]);
      Inc(I);
      AddInt(93, vSubCount);
      for K := vIndex + 1 to (vIndex + vSubCount - 1) do
      begin
        AddInt(72, 1);// Edge type = Line
        Add2DFlatPoint(10, FH.XY[K-1]);
        Add2DFlatPoint(11, FH.XY[K]);
      end;
      // To close
      AddInt(72, 1);
      Add2DFlatPoint(10, FH.XY[vIndex + vSubCount - 1]);// last point
      Add2DFlatPoint(11, FH.XY[vIndex]);// first point
      AddInt(97, 0);
      Inc(vIndex, vSubCount);
      Dec(vCount);
    end;
    AddInt(75, 0);
    AddInt(76, 1);
    AddInt(98, 0);
    Inc(I);// see TsgFlatHatch.Add below full count of points
  end;
  Result := True;
end;

{ FlatPoly entity }
function TsgCADtoDXF.ExportFlatPoly(FP: TsgFlatPoly): Boolean;
begin
  Result := ExportEntityAsGroup(FP);
end;

{ LEADER }
function TsgCADtoDXF.ExportLeader(L: TsgDXFLeader): Boolean;
var
  I: Integer;
begin
  AddCommonEntityGroup(L);
  AddString(3, L.DimStyle.Name);
  AddInt(71, Ord(L.Arrowhead));
  AddInt(72, Ord(L.IsSpline));
  AddInt(73, 3);// Created without any annotation
  AddInt(74, 0);
  AddFloat(40, 1);
  AddFloat(41, 1);
  if L.ControlCount = 0 then
  begin
    AddInt(76, 1);
    Add3DPoint(10, cnstFPointZero);
  end
  else
  begin
    AddInt(76, L.ControlCount);
    for I := 0 to L.ControlCount - 1 do
      Add3DPoint(10, L.Controls[I]);
  end;
  AddString(340, '0');
  // Dimension style
  ExportExtendedData(L);
  Result := True;
end;

{ LINE }
function TsgCADtoDXF.ExportLine(L: TsgDXFLine): Boolean;
begin
  AddCommonEntityGroup(L);
  AddThickness(L.ZThick);
  Add3DPoint(10, L.Point);//  Add3DPointExtruded(10, L.Point, L.Extrusion);
  Add3DPoint(11, L.Point1);//  Add3DPointExtruded(11, L.Point1, L.Extrusion);
  AddExtrusion(L.Extrusion);
  ExportExtendedData(L);
  Result := True;
end;

{ MESH }

function TsgCADtoDXF.ExportMesh(M: TsgDXFMesh): Boolean;
var
  I: Integer;
begin
  AddEntName(M, sCADEntityNames[M.EntType]);
  //if M.Material <> nil then AddHexInt(347, M.Material.Handle);
  AddSubclassMarker(sCADEntitySubNames[M.EntType]);
  AddInt(71, 2); // version
  AddInt(72, 0); // "Blend Crease" property: 0 = Turn off; 1 = Turn on
  AddInt(91, 0); // Number of subdivision level
  AddInt(92, M.Vertices.Count); // Vertex count of level 0
  for I := 0 to M.Vertices.Count - 1 do
    Add3DPoint(10, M.Vertices[I]); // Vertex position
  AddInt(93, M.VertexIndices.Count); //Size of face list of level 0
  for I := 0 to M.VertexIndices.Count - 1 do
    AddInt(90, M.VertexIndices[I]);
  //94: Edge count of level 0
  //90: Vertex index of each edge
  //95: Edge crease count of level 0
  //140: Edge create value
  //90: Count of sub-entity which property has been overridden
  //91: Sub-entity marker
  //92: Count of property was overridden
  //90: Property type:  0 = Color;  1 = Material;  2 = Transparency; 3 = Material mapper
  ExportExtendedData(M);
  Result := True;
end;

{MLINE}

function TsgCADtoDXF.ExportMLeader(ML: TsgCADMultiLeader): Boolean;
begin
  Result := True;
  if TsgCADMultiLeaderAccess(ML).RenderObject is TsgDXFInsert then
    Result := ExportInsert(TsgDXFInsert(TsgCADMultiLeaderAccess(ML).RenderObject));
end;

function TsgCADtoDXF.ExportMLine(M: TsgCADMLine): Boolean;
var
  vVertex: TsgMVertex;
  I, J, K, vLinesInStyle: Integer;
begin
  AddEntName(M, sCADEntityNames[M.EntType]);
  AddLineWeight(M.LineWeight);
  AddSubclassMarker(sCADEntitySubNames[M.EntType]);

  if Assigned(M.Style) then
  begin
    AddString(2, AnsiUpperCase(M.Style.Name));
    AddHexInt(340, M.Style.Handle);
  end
  else
  begin
    AddString(2, sStandardName);
    AddString(340, '0');
  end;
  AddFloat(40, M.ScaleFactor);
  AddInt(cFlag_70, M.Justify);
  Add3DPoint(10, M.StartPoint);
  Add3DPoint(210, M.Extrusion);
  AddInt(71, M.Flags);
  if M.Count > 0 then
    vLinesInStyle := TsgMVertex(M[0]).ParamsCount
  else
    vLinesInStyle := 0;
  AddInt(73, vLinesInStyle);
  AddInt(72, M.Count);
  for I := 0 to M.Count - 1 do
  begin
    vVertex := M[I];
    Add3DPoint(11, vVertex.Point);
    Add3DPoint(12, vVertex.Direction);
    Add3DPoint(13, vVertex.Miter);
    for J := 0 to vVertex.ParamsCount - 1 do
    begin
      AddInt(74, vVertex.PCounts[J]);
      for K := 0 to vVertex.PCounts[J] - 1 do
        AddFloat(41, vVertex.Params[J, K]);
      AddInt(75, 0);
    end;
  end;
  ExportExtendedData(M);
  Result := True;
end;

{ MTEXT }

function TsgCADtoDXF.ExportMText(MT: TsgDXFMText): Boolean;
var
  vCode: Integer;
  vTailLen, vLineLen: Integer;
  vTail, vLine: string;
begin
  AddCommonEntityGroup(MT);
  Add3DPoint(10, MT.Point);
  AddFloat(40, MT.Height);
  vTail := GetEntityStringValue(MT);
  PrepareExportText(vTail);
  vTailLen := Length(vTail);
  vCode := 3;
  repeat
    vLineLen := cnstMaxStringLen;
    if (vTailLen >= vLineLen) and (vTail[vLineLen] = '^') then // error if '^' is eoln
      Dec(vLineLen);
    vLine := Copy(vTail, 1, vLineLen);
    vTail := Copy(vTail, vLineLen + 1, MaxInt);
    vTailLen := Length(vTail);
    if vTailLen = 0 then
      vCode := 1;
    AddString(vCode, vLine);
  until vTailLen <= cnstMaxStringLen;
  if vTailLen > 0 then AddString(1, vTail);
  AddFloat(50, MT.Angle);
  AddFloat(41, MT.RectWidth);
  if MT.Style <> nil then
    AddString(7, MT.Style.Name);
  AddInt(71, MT.Align);
  if TsgDXFMTextEx(MT).HasSecond then
    Add3DPoint(11, MT.Point1);
  AddExtrusion(MT.Extrusion);
  AddInt(73, MT.LineSpacingStyle);
  AddFloat(44, MT.LineSpacingFactor);
  if Version >= acR2004 then
    if MT.HasBackgroundColor then
    begin
      AddInt(90, MT.BackgroundFlags);
      case MT.BackgroundFlags of
        1:
          begin
            AddInt(63, ConvertColorCADToIndexColor(MT.BackgroundColor, True));
            AddInt(421, BGRToRGB(ConvertColorCADToRGB(MT.BackgroundColor)));
            AddFloat(45, MT.BackgroundScaleFactor);
            AddInt(441, MT.BackgroundTransparency);
          end;
        3:
          begin
            AddInt(63, clDXFByLayer);
            AddFloat(45, MT.BackgroundScaleFactor);
            AddInt(441, MT.BackgroundTransparency);
          end;
      end;
    end;
  ExportExtendedData(MT);
  Result := True;
end;

{ OLE2FRAME }
const
  cnstLineSize = 71;

procedure AddHexBinData(const ABinData: AnsiString; ABuf: Pointer; ASelf: Pointer); register;
{$IFDEF NOASM}
const
  cnstDataLen = $20; // AnsiChar pairs
  cnstDataLenMask = $1F;
var
  I, vInpDataLen: Integer;
  vPInpBinDataPos: PByte;
  vPOutBinLinePos: PWord;
begin
  InitByteBinToHex;
  vInpDataLen := Length(ABinData); // always a multiple of cnstDataLen (?)
  vPOutBinLinePos := ABuf;
  vPInpBinDataPos := PPointer(@ABinData)^;
  // make string 310#13#10 ... #13#10
  PCardinal(vPOutBinLinePos)^ := $0D303133; // '310'#13
  Inc(PCardinal(vPOutBinLinePos));
  PByte(vPOutBinLinePos)^ := $0A;           // #10
  // line end #13#10
  Inc(PByte(vPOutBinLinePos), cnstDataLen shl 1 + 1);
  PWord(vPOutBinLinePos)^ := cnstRawEnter;  // #13#10
  // start data position
  Dec(vPOutBinLinePos, cnstDataLen);

  // filling the output buffer
  I := 0;
  while I < vInpDataLen do
  begin
    // put translated word to out
    vPOutBinLinePos^ := ByteBinToHex^[vPInpBinDataPos^];
    // next out position
    Inc(vPOutBinLinePos);
    // next input position
    Inc(vPInpBinDataPos);
    // is line ready
    if I and cnstDataLenMask = cnstDataLenMask then // (I + 1) mod 32 = 0
    begin
      // write output line to stream
      TsgCADtoDXF(ASelf).WriteBuffer(ABuf^, cnstLineSize);
      // start data position
      Dec(vPOutBinLinePos, cnstDataLen);
    end;
    Inc(I);
  end;
end;
{asm
    push rbx                    // save registers
    push rsi
    push rdi
    push r9                     // output line
    push r11                    // offset binary data
    push r12                    // ASelf storage

    mov r12,r8                  // save ASelf

    mov rbx,sBinToHex           // RBX - offset hexdecimal
    mov rax,ABinData            // rax - address of binary data
    movsx rsi,dword ptr [rax - 4] // rsi - length binary data in bytes
    shr rsi,5                   // RSI - 64-bytes output data lines count
    mov rcx,rax                 // RCX - address of binary data

    mov dword ptr [rdx],$0D303133 // set string
    mov byte ptr [rdx + 4],$0A    //  '310'#13#10
    mov word ptr [rdx + 69],$0A0D // #13#10
    add rdx,5                   // offset to begining of output data line
    mov r9,rdx                  // RDX - output line start
@nextline:                     // begin loop for next line
    mov rdx,r9
// ----------- fill line with hexdecimal characters ----------------------------
    mov rdi,32                  // RDI - count of bindata bytes translated to output line
@nextbyte:                      // begin loop for next data byte
    mov ah,[rcx]                // mov data byte to AH
    mov al,ah                   // copy AH to AL
    shr al,4                    // shift AL (requires higher tetrad)
    xlatb                       // translate [RBX + AL] to AL
    mov [rdx],al                // write char to line
    mov al,ah                   // copy AH to AL
    and al,$F                   // mask lower tetrad
    xlatb                       // translate [RBX + AL] to AL
    inc rdx                     // seek next char in line
    mov [rdx],al                // write char to line
    inc rdx                     // seek next char in line
    inc rcx                     // seek next data byte
    dec rdi                     // next data byte counter
    jnz @nextbyte               // if line not fill goto next data byte translate
// ----------- fill line with hexdecimal characters ----------------------------

    mov r11,rcx                 // save default offset of binary data
    mov rcx,r8                  // prepare params
    mov rdx,r9                  // for calling
    sub rdx,5
    mov rax,[rcx]               // TsgCADToDXF.WriteBuffer method
    mov r8,cnstLineSize         // line size
    call [rax + VMTOFFSET TsgCADToDXF.WriteBuffer] // write to buffer
    mov r8,r12                  // restore ASelf
    mov rcx,r11                 // restore default offset of binary data

    dec rsi                     // next line counter
    jnz @nextline               // if not end of line count goto next line fill

    pop r12
    pop r11
    pop r9
    pop rdi
    pop rsi
    pop rbx                     // restore registers
end;}
{$ELSE}
asm
// EAX - input binary data
// EDX - output line start
// ECX - TsgCADToDXF object

    pushad                    // save all registers
    mov ebp,ecx               // save ASelf

    mov ebx,sBinToHex         // EBX - offset hexdecimal
    mov esi,[eax - 4]         // esi - length binary data in bytes
    shr esi,5                 // ESI - 64-bytes output data lines count
    mov ecx,eax               // ECX - address of binary data

    mov dword ptr [edx],$0D303133 // set string
    mov byte ptr [edx + 4],$0A    //  '310'#13#10
    mov word ptr [edx + 69],$0A0D // #13#10
    add edx,5                 // offset to begining of output data line

@nextline:                    // begin loop for next line
    push edx                  // save line buffer offset
// ----------- fill line with hexdecimal characters ----------------------------
    mov edi,32                // EDI - count of bindata bytes translated to output line
@nextbyte:                    // begin loop for next data byte
    mov ah,[ecx]              // mov data byte to AH
    mov al,ah                 // copy AH to AL
    shr al,4                  // shift AL (requires higher tetrad)
{$IFNDEF SGDEL_6}
    xlat                      // translate [EBX + AL] to AL
{$ELSE}
    xlatb                     // translate [EBX + AL] to AL
{$ENDIF}
    mov [edx],al              // write char to line
    inc edx                   // seek next char in line
    mov al,ah                 // copy AH to AL
    and al,$F                 // mask lower tetrad
{$IFNDEF SGDEL_6}
    xlat                      // translate [EBX + AL] to AL
{$ELSE}
    xlatb                     // translate [EBX + AL] to AL
{$ENDIF}
    mov [edx],al              // write char to line
    inc edx                   // seek next char in line
    inc ecx                   // seek next data byte
    dec edi                   // next data byte counter
    jnz @nextbyte             // if line not fill goto next data byte translate
// ----------- fill line with hexdecimal characters ----------------------------

    push ecx                  // save default offset of binary data
    mov eax,ebp               // prepare params for calling TsgCADToDXF.WriteBuffer method
    mov edi,[eax]             // EDI = object offset
    sub edx,69                // EDX = ABuffer
    mov ecx,cnstLineSize      // ECX = count
{$IFDEF SGDEL_6}
    call [edi + VMTOFFSET TsgCADToDXF.WriteBuffer] // write to buffer
{$ELSE}
    call [edi + TsgCADToDXF.WriteBuffer]           // write to buffer
{$ENDIF}
    pop ecx                   // restore default offset of binary data

    pop edx                   // restore line buffer offset

    dec esi                   // next line counter
    jnz @nextline             // if not end of line count goto next line fill

    popad                     // restore all registers
end;
{$ENDIF}

function TsgCADtoDXF.ExportOle2Frame(OLE2: TsgDXFOle2Frame; Data: TObject = nil): Boolean;
var
  vBuf: array[0 .. cnstLineSize - 1] of Byte;
  vSelf: Pointer;
  vBinData: AnsiString;
  vOleHeader: PsgOle2FrameHeaderData;
begin
  vSelf := Self;
  AddCommonEntityGroup(OLE2);
  AddInt(70, 2);
  Add3DPoint(10, MakeFPoint(OLE2.Point.X, OLE2.Point.Y, 0.0));
  Add3DPoint(11, MakeFPoint(OLE2.Point1.X, OLE2.Point1.Y, 0.0));
  //Add3DPoint(10, OLE2.Point);
  //Add3DPoint(11, OLE2.Point1);
  AddInt(71, OLE2.OLEObjectType);
  AddInt(72, 1);
  AddInt(90, Length(OLE2.BinaryData) + 128);
  SetLength(vBinData, 128);
  vOleHeader := PsgOle2FrameHeaderData(PPointer(@vBinData)^);
  PrepareOLEHeader(OLE2, vOleHeader);
  WriteBinary(Pointer(vBinData), Length(vBinData), WriteBuffer, 128);
  if Length(OLE2.BinaryData) > 0 then
    AddHexBinData(OLE2.BinaryData, @vBuf[0], vSelf);
  AddString(1, 'OLE');
  ExportExtendedData(OLE2);
  Result := True;
end;

{ POINT }
function TsgCADtoDXF.ExportPoint(P: TsgDXFPoint): Boolean;
begin
  AddCommonEntityGroup(P);
  AddThickness(P.ZThick);
  Add3DPoint(10, P.Point);
  AddExtrusion(P.Extrusion);
  ExportExtendedData(P);
  Result := True;
end;

{ POLYLINE }
function TsgCADtoDXF.ExportPolyLine(BasePL: TsgCADBasePolyline): Boolean;
var
  PL: TsgDXFPolyline absolute BasePL;
  vFaceMesh, vPolyMesh: Boolean;
  vVertex: TsgDXFVertexEx;
  I, vCount: Integer;
  vEntType: TsgCADEntities;
  vLayer: string;

  procedure Add3dVertex(AVertex: TsgDXFVertexEx; const ALayer: string);
  var
    vPoint: TFPoint;
  begin
    AddEntName(nil, 'VERTEX', ALayer);
    if vFaceMesh and (AVertex.Flags = 128) then
      AddSubclassMarker('FaceRecord')
    else
    begin
      AddSubclassMarker('Vertex');
      if vPolyMesh and ((AVertex.Flags and 64) <> 0) then
        AddSubclassMarker('PolygonMeshVertex')
      else
        if vFaceMesh and (AVertex.Flags <> 128) then
          AddSubclassMarker('PolyFaceMeshVertex')
        else
        begin
          AddLineType(PL.LineType);
          AddSubclassMarker('3dPolylineVertex');
        end;
    end;
    AddDXFColor(PL.ColorCAD);
    vPoint := AVertex.Point;
    DoExtrusion(vPoint, PL.Extrusion);
    //Location point (in OCS when 2D, and WCS when 3D)
    Add3DPoint(10, vPoint);
    if AVertex.StartWidth <> 0 then
      AddFloat(40, AVertex.StartWidth);
    if AVertex.EndWidth <> 0 then
      AddFloat(41, AVertex.EndWidth);
    if AVertex.Bulge <> 0 then
      AddFloat(42, AVertex.Bulge);
  {$IFDEF SG_CLIENT_CLNOFSFL}
    if AVertex.Flags <> 0 then
  {$ENDIF}
      AddInt(cFlag_70, AVertex.Flags);
    //AddFloat(50, AVertex.FitTangent);
    if AVertex.FPolyFaceIndexes <> nil then
    begin
      AddInt(71, AVertex.PolyFaceVertexIndex1);
      AddInt(72, AVertex.PolyFaceVertexIndex2);
      AddInt(73, AVertex.PolyFaceVertexIndex3);
      AddInt(74, AVertex.PolyFaceVertexIndex4);
    end;
  end;

begin
  Result := (BasePL.Count > 0) or (BasePL.PolyPoints.Count > 0);
  if not Result then
    Exit;
  vEntType := BasePL.EntType;
  if (Ord(Version) < Ord(acR14)) and (vEntType = ceLWPolyline) then
    vEntType := cePolyline;
  case vEntType of
    cePolyline:
      begin
        vPolyMesh := PL.IsPolygonMesh;
        vFaceMesh := PL.IsPolyFaceMesh;
        if vPolyMesh then
          AddCommonEntityGroupEx(PL, sCADEntityNames[cePolyline], 'PolygonMesh')
        else
          if vFaceMesh then
            AddCommonEntityGroupEx(PL, sCADEntityNames[cePolyline], 'PolyFaceMesh')
          else// for correct export CIRCLE, ELLIPSE, SPLINE to POLYLINE
            AddCommonEntityGroupEx(PL, sCADEntityNames[cePolyline], '3dPolyline');
        AddInt(66, 1);
      {$IFNDEF SG_CLIENT_CLNOFSFL}
        Add3DPoint(10, MakeFPoint(0, 0, PL.Elevation));
      {$ENDIF}
        AddThickness(PL.ZThick);
        AddInt(cFlag_70, PL.Flags);
      {$IFNDEF SG_CLIENT_CLNOFSFL}
        if PL.GlobalWidth = 0 then
        begin
          AddFloat(40, PL.StartWidth);
          AddFloat(41, PL.EndWidth);
        end
        else
        begin
          AddFloat(40, PL.GlobalWidth);
          AddFloat(41, PL.GlobalWidth);
        end;
      {$ENDIF}
        if PL.MeshM > 0 then
          AddInt(71, PL.MeshM);
        if PL.MeshN > 0 then
          AddInt(72, PL.MeshN);
        ExportExtendedData(PL);
        vCount := PL.Count-1;
        vLayer := GetEntityLayerName(BasePL);
        for I := 0 to vCount do
          Add3dVertex(TsgDXFVertexEx(PL.Entities[I]), vLayer);
        AddEntName(nil, 'SEQEND');
      end;
    ceLWPolyline:
      begin
        AddCommonEntityGroup(PL);
        AddThickness(PL.ZThick);
        if PL.Count > 0 then
          AddFloat(38, PL.Elevation);// Elevation
        AddInt(90, PL.Count);
        AddInt(cFlag_70, PL.Flags);
        if PL.GlobalWidth <> 0 then
          AddFloat(43, PL.GlobalWidth);
        vCount := PL.Count-1;
        for I := 0 to vCount do
        begin
          vVertex := TsgDXFVertexEx(PL.Entities[I]);
          Add2DFlatPoint(10, vVertex.Point);
          if (vVertex.StartWidth <> 0) or (vVertex.EndWidth <> 0) then
          begin
            AddFloat(40, vVertex.StartWidth);
            AddFloat(41, vVertex.EndWidth);
          end;
          if vVertex.Bulge <> 0 then
            AddFloat(42, vVertex.Bulge);
        end;
        AddExtrusion(PL.Extrusion);
        ExportExtendedData(PL);
      end;
  else
    // for correct export CIRCLE, ELLIPSE, SPLINE to POLYLINE
    AddCommonEntityGroupEx(BasePL, sCADEntityNames[cePolyline], '3dPolyline');
    AddInt(66, 1);
    Add3DPoint(10, cnstFPointZero);
    AddThickness(BasePL.ZThick);
    AddInt(cFlag_70, BasePL.Flags or 8);// 8 = 3DPolyline
    ExportExtendedData(BasePL);
    vCount := BasePL.PolyPoints.Count - 1;
    for I := 0 to vCount do
    begin
      AddEntName(nil, 'VERTEX');
      AddSubclassMarker('Vertex');
      AddSubclassMarker('3dPolylineVertex');
      Add3DPoint(10, BasePL.PolyPoints[I]);
      AddInt(cFlag_70, 32);// 32 = 3D polyline vertex
    end;
    AddEntName(nil, 'SEQEND');
  end;
end;

function TsgCADtoDXF.ExportRay(Ray: TsgDXFRay): Boolean;
begin
  AddCommonEntityGroup(Ray);
  AddThickness(Ray.ZThick);
  Add3DPoint(10, Ray.StartPoint);
  Add3DPoint(11, Ray.Direction);
  ExportExtendedData(Ray);
  Result := True;
end;

{$IFDEF SG_BTI}
{ POLYPOLYLINE }
function TsgCADtoDXF.ExportPolyPolyline2D(PL2D: TsgCADPolyPolyline2D): Boolean;
var
  I, vCount: Integer;

  procedure Export2DLine(L: TsgLine);
  begin
    AddCommonEntityGroupEx(PL2D, 'LINE', 'Line');
    AddLineType(PL2D.LineType);// only if PL2D is not a TsgDXFPenLine, see code in AddCommonEntityGroupEx
    Add2DFlatPoint(10, L.Point1);
    Add2DFlatPoint(11, L.Point2);
  end;

  procedure Export2DArc(A: TsgArcR);
  begin
    AddCommonEntityGroupEx(PL2D, 'ARC', 'Circle');
    AddLineType(PL2D.LineType);// only if PL2D is not a TsgDXFPenLine, see code in AddCommonEntityGroupEx
    Add3DPoint(10, A.Center);
    AddFloat(40, A.Radius);
    AddSubclassMarker('Arc');
    AddFloat(50, A.AngleS);
    AddFloat(51, A.AngleE);
  end;

begin
  vCount := PL2D.CurvesCount-1;
  for I := 0 to vCount do
    if PL2D.CurveVisible[I] then
    begin
      if PL2D.IsCurveArc(I) then
        Export2DArc(PL2D.Arcs[I])
      else
        Export2DLine(PL2D.Lines[I]);
    end;
  Result := True;
end;
{$ENDIF}

{ SHAPE }
function TsgCADtoDXF.ExportShape(S: TsgDXFShape): Boolean;
begin
  AddCommonEntityGroup(S);
  Add3DPoint(10, S.Point);
  AddFloat(40, S.Height);
  AddString(2, S.ShapeName);
  AddFloat(50, S.Rotation);
  AddFloat(41, S.XScale);
  AddFloat(51, S.ObliqueAngle);
  ExportExtendedData(S);
  Result := True;
end;

{ SOLID }
function TsgCADtoDXF.ExportSolid(S: TsgDXFSolid): Boolean;
begin
  AddCommonEntityGroup(S);
  Add3DPoint(10, S.Point);
  Add3DPoint(11, S.Point1);
  Add3DPoint(12, S.Point2);
  Add3DPoint(13, S.Point3);
  AddExtrusion(S.Extrusion);
  ExportExtendedData(S);
  Result := True;
end;

function TsgCADtoDXF.ExportSpatial(AFilter: TsgCADSpatialFilter): Boolean;
var
  I, C: Integer;
  P: PFPointArray;

  procedure AddMatrix(const M: TFMatrix);
  var
    Col, Row: Integer;
  begin
    for Col := 0 to 2 do
      for Row := 0 to 3 do
        AddFloat(40, M.M[Row][Col]);
  end;

begin
  BeginObject(cnstSPATIAL_FILTER);
  AddHandle(AFilter, AFilter.Owner.Handle, []);
  AddSubclassMarker('Filter');
  AddSubclassMarker(cnstSpatialFilterMarker);
  C := AFilter.Bounds.Count;
  AddInt(70, C);
  P := AFilter.Bounds.List;
  for I := 0 to C - 1 do
    Add2DPoint(10, P^[I].Point2D);
  Add3DPoint(210, AFilter.Extrusion);
  Add3DPoint(11, AFilter.Clipbdorg);
  AddInt(71, Ord(AFilter.DisplayBounds));
  AddInt(72, Ord(AFilter.FrontClip));
  if AFilter.FrontClip then
    AddFloat(40, AFilter.FrontDist);
  AddInt(73, Ord(AFilter.BackClip));
  if AFilter.BackClip then
    AddFloat(41, AFilter.BackDist);
  AddMatrix(AFilter.InvBlkTransform);
  AddMatrix(AFilter.Clip);
  ExportExtendedData(AFilter);
  Result := True;
end;

{ SPLINE }

function TsgCADtoDXF.ExportSpline(SPL: TsgDXFSpline): Boolean;
var
  I: Integer;
begin
  if Ord(Version) > Ord(acR12) then
  begin
    AddCommonEntityGroup(SPL);
    AddInt(cFlag_70, SPL.Flags);
    AddInt(71, SPL.Degree);
    AddInt(72, SPL.Knots.Count);
    AddInt(73, SPL.ControlCount);
    AddFloat(42, SPL.KnotTol);
    AddFloat(43, SPL.CtrlTol);
    AddFloat(44, SPL.FitTol);
    if SPL.FitCount > 0 then
    begin
      AddInt(74, SPL.FitCount);
      Add3DPoint(12, SPL.BeginningTangent);
      Add3DPoint(13, SPL.EndingTangent);
    end;
    for I := 0 to SPL.Knots.Count - 1 do
      AddFloat(40, SPL.Knots[I]);
    for I := 0 to SPL.ControlCount - 1 do
      Add3DPoint(10, SPL.ControlPoints[I]);
    for I := 0 to SPL.WeightCount - 1 do
      AddFloat(41, SPL.Weight[I]);
    for I := 0 to SPL.FitCount - 1 do
      Add3DPoint(11, SPL.FitPoints[I]);
    ExportExtendedData(SPL);
  end
  else
    ExportPolyLine(SPL);
  Result := True;
end;

{ ATTDEF, ATTRIB, TEXT }
function TsgCADtoDXF.ExportText(T: TsgDXFText): Boolean;
var
  vEntType: TsgCADEntities;
  vText: string;
  vLockPosition: Integer;
begin
  vEntType := T.EntType;
  AddCommonEntityGroup(T);
  Add3DPoint(10, T.Point);
  AddFloat(40, T.Height);
  vText := GetEntityStringValue(T);
  if vEntType = ceText then
    ReplaceAnsi(vText, cnstCaret, cnstCaretAndSpace);
  AddString(1, vText);
  AddFloat(50, T.Rotation);
  AddFloat(41, T.Scale);
  AddFloat(51, T.ObliqueAngle);
  if T.Style <> nil then
    AddString(7, ExtractName(T.Style.Name));
  AddInt(71, T.Generation);
  AddInt(72, T.HAlign);
  if TsgDXFTextEx(T).HasSecond then
    Add3DPoint(11, T.Point1);
  AddExtrusion(T.Extrusion);
  if vEntType = ceText then
  begin
    AddSubclassMarker('Text');
    AddInt(73, T.VAlign);
  end
  else
  begin
    if vEntType = ceAttrib then
      AddSubclassMarker('Attribute')
    else
    begin
      AddSubclassMarker('AttributeDefinition');
      AddString(3, TsgDXFAttdef(T).Tag);
    end;
    AddString(2, TsgDXFAttrib(T).Tag);
    AddInt(cFlag_70, TsgDXFAttrib(T).Flags);
    AddInt(74, T.VAlign);
    vLockPosition := Integer(TsgDXFAttrib(T).LockPosition);
    if Version >= acR2007 then
      AddInt(Word_280, vLockPosition);
  end;
  ExportExtendedData(T);
  Result := True;
end;

{ TOLERANCE }
function TsgCADtoDXF.ExportTolerance(T: TsgDXFTolerance): Boolean;
begin
  AddCommonEntityGroup(T);
  AddString(3, T.DimStyle.Name);
  Add3DPoint(10, T.Point);
  AddString(1, T.Text);
  if TsgDXFToleranceEx(T).HasSecond then
    Add3DPoint(11, T.Point1);
  AddExtrusion(T.Extrusion);
  ExportExtendedData(T);
  Result := True;
end;

{ VIEWPORT }
function TsgCADtoDXF.ExportViewPort(V: TsgDXFViewPort): Boolean;
var
  vIndex: UInt64;
  I: Integer;
begin
  if not FHasViewPortWindow then
    FHasViewPortWindow := (V.ThisID = 1);
  AddCommonEntityGroup(V);
  Add3DPoint(10, V.PSpaceCenter);
  AddFloat(40, V.PSpaceWidth);
  AddFloat(41, V.PSpaceHeight);
  AddInt(68, V.StatusField);
  AddInt(69, V.ThisID);
  if Ord(Version) > Ord(acR14) then
  begin
    Add3DPoint(12, V.MSpaceCenter);
    Add3DPoint(16, V.ViewDirection);
    Add3DPoint(17, V.ViewTarget);
    AddFloat(42, cntViewPortPerspectiveLensLength);
    AddFloat(43, V.FrontClipPlane);
    AddFloat(44, V.BackClipPlane);
    AddFloat(45, V.MSpaceHeight);
    AddFloat(51, V.ViewTwistAngle);
    AddInt(90, V.Flags);
    if Assigned(V.FrozenLayers) then
      for I := 0 to V.FrozenLayers.Count - 1 do
        if Version <=  acR2000 then
          AddHexInt(341, V.FrozenLayers[I])
        else
          AddHexInt(331, V.FrozenLayers[I]);
    vIndex := GetHandleByEntity(V.ClippingBoundary);
    if vIndex > 0 then
      AddHexInt(340, vIndex)
    else
      AddHexInt(340, 0);
    ExportExtearnalEED(V.Handle, nil);
  end
  else
    ExportExtendedData(V);
  Result := True;
end;

function TsgCADtoDXF.ExportWipeOut(W: TsgCADCustomRectangle; AData: TObject = nil): Boolean;
var
  I, Cnt:  Integer;
  vSize, vUVector, vVVector: TFPoint;
  vColorCadOld: TsgColorCAD;
begin
  // necessary for HPGL
  vColorCadOld := W.ColorCAD;
  try
    if IsNeedImageDefHandle(W) then
      W.ColorCAD := cnstColorCADByBlackWhite;
    AddCommonEntityGroup(W);
  finally
    W.ColorCAD := vColorCadOld;
  end;
  Add3DPoint(10, W.Point);

  GetCorrectImageVectorData(TsgDXFImageEnt(W), vUVector, vVVector, vSize);
  Add3DPoint(11, vUVector);
  Add3DPoint(12, vVVector);
  Add2DFlatPoint(13, vSize);

  AddHexInt(340, GetImageDefHandle(W)); // IMAGEDEF
  AddInt(70, W.Flags);
  AddInt(280, Ord(W.UseClipping));
  AddInt(281, cntImageBrightness);
  AddInt(282, cntImageContrast);
  AddInt(283, cntImageFade);
  AddHexInt(360, 0);      // IMAGEDEF_REACTOR
  if W.ClippingBoundaryType <> 0 then
  begin
    AddInt(71, W.ClippingBoundaryType);
    AddInt(91, GetRealCountBoundaryPoints(W));
    Cnt := W.ClipPointsCount - 1;
    for I := 0 to Cnt do
      Add2DPoint(14, W.ClipPoints[I]);
    if (W.ClippingBoundaryType <> 1) and (Cnt > 0) and (not IsEqualF2DPoints(W.ClipPoints[0], W.ClipPoints[Cnt])) then
      Add2DPoint(14, W.ClipPoints[0]);
  end;
  ExportExtendedData(W);
  Result := True;
end;

procedure TsgCADtoDXF.ExportXrecord(X: TsgDXFXRecord);
begin
  BeginObject(cnstXRecord);
  AddHandle(X, X.Owner.Handle, [X.Owner.Handle]);
  AddSubclassMarker(sXrecord);
  AddInt(280, 0);//X.Flags and 1
  DoExportEED(X.Data);
end;

procedure TsgCADtoDXF.ExportTablesAPPID;
var
  I: Integer;
  vEnt: TsgDXFAppID;
begin
  BeginTable(nil, cnstTableAPPID);
  if not Assigned(FAppIDs) then
    FAppIDs := TAppIDs.Create(Self);
  // TAppIDs(FAppIDs).SortedList not need sort!!!
  for I := 0 to TAppIDs(FAppIDs).SortedList.Count - 1 do
  begin
    vEnt := TsgDXFAppID(TAppIDs(FAppIDs).SortedList.Objects[I]);
    AddCommonTableGroup(vEnt, cnstTableAPPID, 'RegApp',
      TAppIDs(FAppIDs).SortedList[I], vEnt.Flags);
  end;
  // do sort for export known EED
  TAppIDs(FAppIDs).SortedList.Sorted := True;
  EndTable;
end;

procedure TsgCADtoDXF.ExportTablesBLOCK_RECORD;
var
  vBlock: TsgDXFBlock;
begin
  BeginTable(nil, cnstTableBLOCK_RECORD);
  // Default entry *MODEL_SPACE and *PAPER_SPACE
  ProcBlockRecord(Converter.Layouts[0].PaperSpaceBlock, GetModelSpaceName, nil);
  if GetPaperLayout <> nil then
    vBlock := GetPaperLayout.PaperSpaceBlock
  else
    vBlock := nil;
  ProcBlockRecord(vBlock, GetPaperSpaceName, nil);
  // BLOCK_RECORDs
  ApplyBlocksByProc(ProcBlockRecord);
  EndTable;
end;

procedure TsgCADtoDXF.ExportTablesDIMSTYLE;

  function ExportDimDtyle(ADimStyle: TsgDXFDimensionStyle): Boolean;

    function AddReference(const ARefCode: Integer; AEntity: TsgDXFEntity): UInt64;
    begin
      Result := 0;
      if (AEntity <> nil) then
      begin
        Result := GetHandleByEntity(AEntity);
        if Result <> cnstBadHandle then
          AddHexInt(ARefCode, Result);
//      else
//        AddString(ARefCode, '0');
      end;
    end;

    procedure ExportArrow(ADimBlk, ADimBlk1, ADimBlk2, ADimLrBlk: TsgDXFEntity);
      procedure CheckBlock(var ABlock: TsgDXFEntity);
      begin
        if (ABlock <> nil) and (ABlock.EntName = sgDimensionArrowTypeNames[datClosedfilled]) then
          ABlock := nil;
      end;
    begin
      CheckBlock(ADimBlk);
      CheckBlock(ADimBlk1);
      CheckBlock(ADimBlk2);
      CheckBlock(ADimLrBlk);
      AddReference(341, ADimLrBlk);
      AddReference(342, ADimBlk);
      AddReference(343, ADimBlk1);
      AddReference(344, ADimBlk2);
    end;


  begin
    AddCommonTableGroup(ADimStyle, cnstTableDIMSTYLE, 'DimStyle', ADimStyle.Name, 0{ADimDtyle.Flags});
    AddString(3, ADimStyle.DIMPOST);
    AddFloat(40, ADimStyle.Scale);
    AddFloat(41, ADimStyle.ArrowSize);
    AddFloat(42, ADimStyle.ExtLineOffset);
    AddFloat(44, ADimStyle.ExtLineExt);
    AddFloat(47, ADimStyle.DIMTP);
    AddFloat(48, ADimStyle.DIMTM);
    AddInt(73, Ord(ADimStyle.DIMTIH));
    AddInt(74, Ord(ADimStyle.DIMTOH));
    AddInt(75, Ord(ADimStyle.DIMSE1));
    AddInt(76, Ord(ADimStyle.DIMSE2));
    AddInt(77, Ord(ADimStyle.TextPosVert));
    AddFloat(140, ADimStyle.TextHeight);
    AddFloat(141, ADimStyle.SizeCenterMark);
    AddFloat(144, ADimStyle.DIMLFAC);
    AddFloat(147, ADimStyle.TextOffset);
    AddInt(Integer_173, Ord(ADimStyle.DIMSAH));
    AddInt(Integer_174, ADimStyle.DIMTIX);
    AddInt(Integer_176, ColorToDXF(ADimStyle.DIMCLRD));
    AddInt(177, ColorToDXF(ADimStyle.DIMCLRE));
    AddInt(178, ColorToDXF(ADimStyle.DIMCLRT));
    AddInt(Integer_277, GetDimLimitUnitsByte(ADimStyle.DIMLUNIT));
    AddInt(Integer_278, Ord(ADimStyle.DIMDSEP));
    AddInt(281, Ord(ADimStyle.DIMSD1));
    AddInt(282, Ord(ADimStyle.DIMSD2));
    AddInt(Integer_271, ADimStyle.DIMDEC);
    AddInt(Integer_276, ADimStyle.DIMFRAC);
    AddReference(340, ADimStyle.TextStyle);
    ExportArrow(TsgDXfDimStyleAccess(ADimStyle).GetBlocks(vnDIMBLK),
      TsgDXfDimStyleAccess(ADimStyle).GetBlocks(vnDIMBLK1),
      TsgDXfDimStyleAccess(ADimStyle).GetBlocks(vnDIMBLK2),
      TsgDXfDimStyleAccess(ADimStyle).GetBlocks(vnDIMLRBLK));
    AddInt(371, CorrectLineWeight(ADimStyle.DIMLWD, False));
    AddInt(372, CorrectLineWeight(ADimStyle.DIMLWE, False));
    Result := True;
  end;
var
  I: Integer;
  vDimStyles: TsgDXFTable;
begin
  vDimStyles := TsgDXFTable(Converter.Sections[csDimStyles]);
  BeginTable(vDimStyles, cnstTableDIMSTYLE);
  if Ord(Version) >= Ord(acR2000) then
  begin
    AddInt(cFlag_70, vDimStyles.Count);
    AddSubclassMarker('DimStyleTable');
    AddInt(71, 0);
  end;
  FHandleCode := 105;
  if (vDimStyles = nil) or (vDimStyles.FindEntByName(sStandardName) = nil) then
  begin// Default entry 'STANDARD'
    AddCommonTableGroup(nil, cnstTableDIMSTYLE, 'DimStyle', sStandardName, 0);
    AddString(340, '0');
  end;
  if vDimStyles <> nil then
    for I := 0 to vDimStyles.Count - 1 do
      ExportDimDtyle(TsgDXFDimensionStyle(vDimStyles.Entities[I]));
  FHandleCode := 5;
  EndTable;
end;

procedure TsgCADtoDXF.ExportTablesLAYER;
var
  I: Integer;

  function ExportLayer(ALayer: TsgDXFLayer): Boolean;
  var
    vName: string;
  begin
    Result := False;
    vName := ExtractName(ALayer.Name);
    if IsNameInternal(vName) then
      Exit;
    AddCommonTableGroup(ALayer, cnstTableLAYER, 'Layer', vName,
      GetFlagsByName(ALayer.Flags, ALayer.Name));
    // Add Color and Visible properties (with correcting for 'ByLayer' color)
    if ALayer.Color = clByLayer then
    begin
      if ALayer.Visible then
        AddInt(62, 7)
      else
        AddInt(62, -7);// Black/White
    end
    else
      AddDXFColor(ALayer.ColorCAD, ALayer.Visible);
    if ALayer.LineType <> nil then
      AddLineType(ALayer.LineType)
    else
      if Ord(Version) <= Ord(acR12) then
        AddString(6, sContinuous);
    if Ord(Version) > Ord(acR14) then
    begin
      // The 'DEFPOINTS'-layer cannot be plotted
      if AnsiUpperCase(ALayer.Name) <> sLayerDefPoints then
        AddInt(290, Ord(ALayer.IsPlotting))
      else
        AddInt(290, 0);
      AddLineWeight(ALayer.LineWeight);
      AddString(390, '0');// reference to 'ACDBPLACEHOLDER'
    end;
    Result := True;
  end;
begin
  BeginTable(Converter.Sections[csLayers], cnstTableLAYER);
  // Default entry 0
  if TsgDXFTable(Converter.Sections[csLayers]).FindEntByName('0') = nil then
  begin
    AddCommonTableGroup(nil, cnstTableLAYER, 'Layer', '0', 0);
    AddInt(62, clDXFBlackWhite);
    AddString(6, sContinuous);
    if Ord(Version) > Ord(acR14) then
    begin
      AddInt(370, -3);// Lineweight
      AddString(390, '0');// reference to 'ACDBPLACEHOLDER'
    end;
  end;
  // Layers
  if Converter.Sections[csLayers] <> nil then
    for I := 0 to Converter.Counts[csLayers] - 1 do
      ExportLayer(TsgDXFLayer(Converter.Layers[I]));
  EndTable;
end;

procedure TsgCADtoDXF.ExportTablesLTYPE;
var
  vHasByBlock, vHasByLayer: Boolean;
  vLType: TsgDXFLineType;

  procedure ExportLType(ALType: TsgDXFLineType);
  var
    J, vCount: Integer;
    vLen: Double;
    vDescriptiveText: string;
    vElement: TsgLTypeElement;
  begin
    AddCommonTableGroup(ALType, cnstTableLTYPE, 'Linetype', ExtractName(ALType.Name),
      GetFlagsByName(ALType.Flags, ALType.Name));
    if not vHasByBlock then vHasByBlock := AnsiUpperCase(ALType.Name) = AnsiUpperCase(sByBlock);
    if not vHasByLayer then vHasByLayer := AnsiUpperCase(ALType.Name) = AnsiUpperCase(sByLayer);
    vCount := ALType.Lines.ElementsCount;
    vDescriptiveText := GenLTypeDiscriptText(ALType, vLen);
    AddString(3, vDescriptiveText);
    AddInt(72, 65);
    AddInt(73, vCount);
    AddFloat(40, vLen);
    for J := 0 to vCount - 1 do
    begin
      vElement := ALType.Lines.Elements[J];
      AddFloat(49, vElement.Dash);
      if Ord(Version) > Ord(acR12) then
      begin
        AddInt(74, vElement.ComplexType and $07);// 0 = no embedded shape/text (acad 2010 exception if $08 bit set)
        if vElement.ComplexType > 0 then
        begin
          AddInt(75, vElement.ShapeNumber);
          AddHexInt(340, GetHandleByEntity(TsgDXFEntity(vElement.Style)));
          AddFloat(46, vElement.Scale);
          AddFloat(50, vElement.Rotation);
          AddFloat(44, vElement.XOffset);
          AddFloat(45, vElement.YOffset);
          if vElement.ComplexType and 2 <> 0 then
            AddString(9, vElement.Text);
        end;
      end;
    end;
  end;
var
  I: Integer;
begin
  BeginTable(Converter.Sections[csLTypes], cnstTableLTYPE);

  vLType := TsgDXFLineType(Converter.Sections[csLTypes].FindEntByName(sContinuous));
  if Assigned(vLType) then
  begin
    // first write Continuous !!!
    ExportLType(vLType);
  end;

  vHasByBlock := False;
  vHasByLayer := False;
  if Converter.Sections[csLTypes] <> nil then
    for I := 0 to Converter.Counts[csLTypes] - 1 do
    begin
      // write LTypes excluding Continuous
      if vLType <> Converter.LTypes[I] then
        ExportLType(TsgDXFLineType(Converter.LTypes[I]));
    end;
  // Default entry ByBlock, ByLayer
  vLType := TsgDXFLineType.Create;
  if not vHasByBlock then
  begin
    vLType.Name := sByBlock;
    ExportLType(vLType);
  end;
  if not vHasByLayer then
  begin
    vLType.Name := sByLayer;
    ExportLType(vLType);
  end;
  vLType.Free;
  EndTable;
end;

procedure TsgCADtoDXF.ExportTablesSTYLE;
  procedure ExportStyle(AStyle: TsgDXFStyle);
  var
    vName: string;
  begin
    if AStyle.Flags and 1 <> 0 then
      vName := ''
    else
      vName := AStyle.Name;
    AddCommonTableGroup(AStyle, cnstTableSTYLE, 'TextStyle', ExtractName(vName),
      GetFlagsByName(AStyle.Flags, vName));
    AddFloat(40, AStyle.FixedHeight);
    AddFloat(41, AStyle.WidthFactor);
    AddFloat(50, AStyle.ObliqueAngle);
    AddInt(71, AStyle.TextGenFlags);
    AddFloat(42, AStyle.LastHeightUsed);
    AddString(3, AStyle.PrimaryFont);
    AddString(4, AStyle.BigFont);
    if Version >= acR14 then
      ExportExtendedData(AStyle);
  end;
var
  I: Integer;
begin
  BeginTable(Converter.Sections[csStyles], cnstTableSTYLE);
  if (Converter.Sections[csStyles] = nil)
    or (TsgDXFTable(Converter.Sections[csStyles]).FindEntByName(sStandardName) = nil) then
  begin// Default entry 'STANDARD'
    AddCommonTableGroup(nil, cnstTableSTYLE, 'TextStyle', sStandardName, 0);
    AddFloat(40, 0);
    AddFloat(41, 1);
    AddFloat(50, 0);
    AddInt(71, 0);
    AddFloat(42, 10.0);
    AddString(3, sDefaultSHXFont);
    AddString(4, '');// no bigfont
  end;
  if Converter.Sections[csStyles] <> nil then
    for I := 0 to Converter.Counts[csStyles] - 1 do
      ExportStyle(TsgDXFStyle(Converter.Styles[I]));
  EndTable;
end;

procedure TsgCADtoDXF.ExportTablesVPORT;
  procedure ExportVPort(AVPort: TsgDXFVPort);
  var
    V: TsgDXFViewPort;
  begin
    V := CreateDefaultViewPort(DoHandle);
    try
      AddCommonTableGroup(AVPort, cnstTableVPORT, 'Viewport', sActiveVPort{AVPort.Name}, 0{AVPort.Flags});
      Add2DPoint(10, cnstF2DPointZero);
      Add2DPoint(11, MakeF2DPoint(1,1));
      if Ord(Version) <= Ord(acR12) then
        Add2DFlatPoint(12, V.MSpaceCenter)
      else
        Add3DPoint(12, V.MSpaceCenter);
      Add2DPoint(13, cnstF2DPointZero);
      Add2DPoint(14, MakeF2DPoint(1,1));
      Add2DPoint(15, MakeF2DPoint(1,1));
      Add3DPoint(16, V.ViewDirection);
      Add3DPoint(17, V.ViewTarget);
      AddFloat(40, V.PSpaceHeight);
      AddFloat(41, V.PSpaceWidth/V.PSpaceHeight);
      AddFloat(42, 50);
      AddFloat(43, V.FrontClipPlane);
      AddFloat(44, V.BackClipPlane);
      AddFloat(50, 0);
      AddFloat(51, V.ViewTwistAngle);
      AddInt(71, 0);
      AddInt(72, cntViewPortCircleZoomPercent);
      AddInt(73, 1);
      AddInt(74, 1);
      AddInt(75, 0);
      AddInt(76, 0);
      AddInt(77, 0);
      AddInt(78, 0);
    finally
      V.Free;
    end;
  end;
begin
  BeginTable(nil, cnstTableVPORT);
  ExportVPort(nil);
  EndTable;
end;

procedure TsgCADtoDXF.ExportTablesVIEW;
begin
  BeginTable(nil, cnstTableVIEW);
  EndTable;
end;

procedure TsgCADtoDXF.ExportTablesUCS;
begin
  BeginTable(nil, cnstTableUCS);
  EndTable;
end;

procedure TsgCADtoDXF.ExportSectionACDSDATA;
var
  I: Integer;
  vSaveVer: Word;
  S: TMemoryStream;
  vLineStart, vEndData: PByte;
  vRaw: sgRawByteString;
  vLinesCount: Integer;

  procedure DoSchema2;
  begin
    // TODO: use sgDataSegFile as common
    AddString(0, cnstACDSSCHEMA);
    AddInt(90, 0);
    AddString(1, cnstAcDb_Thumbnail_Schema);
    AddString(2, cnstAcDbDs_ID);
    AddInt(280, 10);
    AddInt(91, 8);
    AddString(2, cnstThumbnail_Data);
    AddInt(280, 15);
    AddInt(91, 0);
    AddString(101, cnstACDSRECORD);
    AddInt(95, 0);
    AddInt(90, 2);
    AddString(2, cnstAcDbDs_TreatedAsObjectData);
    AddInt(280, 1);
    AddInt(291, 1);
    AddString(101, cnstACDSRECORD);
    AddInt(95, 0);
    AddInt(90, 3);
    AddString(2, cnstAcDbDs_Legacy);
    AddInt(280, 1);
    AddInt(291, 1);
    AddString(101, cnstACDSRECORD);
    AddString(1, cnstAcDbDs_ID);
    AddInt(90, 4);
    AddString(2, cnstAcDs_Indexable);
    AddInt(280, 1);
    AddInt(291, 1);
    AddString(101, cnstACDSRECORD);
    AddString(1, cnstAcDbDs_ID);
    AddInt(90, 5);
    AddString(2, cnstAcDbDs_HandleAttribute);
    AddInt(280, 7);
    AddInt(282, 1);

    AddString(0, cnstACDSSCHEMA);
    AddInt(90, 1);
    AddString(1, cnstAcDb3DSolid_ASM_Data);
    AddString(2, cnstAcDbDs_ID);
    AddInt(280, 10);
    AddInt(91, 8);
    AddString(2, cnstASM_Data);
    AddInt(280, 15);
    AddInt(91, 0);
    AddString(101, cnstACDSRECORD);
    AddInt(95, 1);
    AddInt(90, 2);
    AddString(2, cnstAcDbDs_TreatedAsObjectData);
    AddInt(280, 1);
    AddInt(291, 1);
    AddString(101, cnstACDSRECORD);
    AddInt(95, 1);
    AddInt(90, 3);
    AddString(2, cnstAcDbDs_Legacy);
    AddInt(280, 1);
    AddInt(291, 1);
    AddString(101, cnstACDSRECORD);
    AddString(1, cnstAcDbDs_ID);
    AddInt(90, 4);
    AddString(2, cnstAcDs_Indexable);
    AddInt(280, 1);
    AddInt(291, 1);
    AddString(101, cnstACDSRECORD);
    AddString(1, cnstAcDbDs_ID);
    AddInt(90, 5);
    AddString(2, cnstAcDbDs_HandleAttribute);
    AddInt(280, 7);
    AddInt(282, 1);

    AddString(0, cnstACDSSCHEMA);
    AddInt(90, 2);
    AddString(1, cnstAcDbDs_TreatedAsObjectDataSchema);
    AddString(2, cnstAcDbDs_TreatedAsObjectData);
    AddInt(280, 1);
    AddInt(91, 0);

    AddString(0, cnstACDSSCHEMA);
    AddInt(90, 3);
    AddString(1, cnstAcDbDs_LegacySchema);
    AddString(2, cnstAcDbDs_Legacy);
    AddInt(280, 1);
    AddInt(91, 0);

    AddString(0, cnstACDSSCHEMA);
    AddInt(90, 4);
    AddString(1, cnstAcDbDs_IndexedPropertySchema);
    AddString(2, cnstAcDs_Indexable);
    AddInt(280, 1);
    AddInt(91, 0);

    AddString(0, cnstACDSSCHEMA);
    AddInt(90, 5);
    AddString(1, cnstAcDbDs_HandleAttributeSchema);
    AddString(2, cnstAcDbDs_HandleAttribute);
    AddInt(280, 7);
    AddInt(91, 1);
    AddInt(284, 1);
  end;

begin
  BeginSection(cnstSectionACDSDATA);
  AddInt(70, 2);
  AddInt(71, 2);

  DoSchema2;//vcl

  for I := 0 to FACISEntities.Count - 1 do
  begin
    S := GetACISStream(TsgBrepModEntity(FACISEntities[I]), vSaveVer);
    if Assigned(S) then
    try
      AddString(0, cnstACDSRECORD);
      AddInt(90, 1);
      AddString(2, cnstAcDbDs_ID);
      AddInt(280, 10);
      AddHexInt(320, TsgBrepModEntity(FACISEntities[I]).Handle);
      AddString(2, cnstASM_Data);
      AddInt(280, 15);
      AddInt(94, S.Size);
      SetLength(vRaw, cnstMaxBinDataLineLen);
      vLineStart := PByte(S.Memory);
      vEndData := vLineStart;
      vLinesCount := S.Size div cnstMaxBinBytesPerLine;
      Inc(vEndData, vLinesCount * cnstMaxBinBytesPerLine);
      while UIntPtr(vLineStart) < UIntPtr(vEndData) do
      begin
        sgBinToHex(vLineStart, Pointer(vRaw), cnstMaxBinBytesPerLine);
        AddRawByteString(310, vRaw);
        Inc(vLineStart, cnstMaxBinBytesPerLine);
      end;
      SetLength(vRaw, (S.Size - vLinesCount * cnstMaxBinBytesPerLine) shl 1);
      if Length(vRaw) > 0 then
      begin
        sgBinToHex(vLineStart, Pointer(vRaw), S.Size - vLinesCount * cnstMaxBinBytesPerLine);
        AddRawByteString(310, vRaw);
      end;
    finally
      S.Free;
    end;
  end;
  EndSection;
end;

procedure TsgCADtoDXF.ExportSectionBLOCKS;
begin
  BeginSection('BLOCKS');
  // Default entry *MODEL_SPACE and *PAPER_SPACE ("*" => "$" for R12)
  ProcBlock(nil, GetModelSpaceName, nil);
  ProcBlock(nil, GetPaperSpaceName, nil);
  // Blocks
  ApplyBlocksByProc(ProcBlock);
  EndSection;
end;

procedure TsgCADtoDXF.ExportSectionCLASSES;
var
  vInstCount: Integer;
  vDict: TsgDXFDictionary;
  vDictItem: TsgDXFOwneredItem;

  procedure AddCommonCLASSGroup(const ARecordName, AClassName, AAppName: string;
    const AFlags, AInstCount: Integer; const AIsEntity: Boolean);
  begin
    BeginObject('CLASS');
    AddString(1, ARecordName);
    AddString(2, cntAcDb + AClassName);
    AddString(3, AAppName);
    AddInt(90, AFlags);
    if Version >= acR2004 then
      AddInt(91, AInstCount);// Instance count for a custom class
    AddInt(280, 0);// Was-a-proxy flag. Set to 1 if class was not loaded when this DXF file was created, and 0 otherwise
    AddInt(281, Ord(AIsEntity));
  end;
begin
  if Ord(Version) <= Ord(acR12) then Exit;
  BeginSection('CLASSES');
  // Default classes
  AddCommonCLASSGroup('IMAGE', 'RasterImage', 'ISM', 2175{127}, 1, True);
  AddCommonCLASSGroup('IMAGEDEF', 'RasterImageDef', 'ISM', 0, 1, False);
  AddCommonCLASSGroup('WIPEOUT', 'Wipeout', '"WipeOut"', 2175{127}, 1, True);
  AddCommonCLASSGroup('WIPEOUTVARIABLES', 'WipeoutVariables', '"WipeOut"', 0, 1, False);
  AddCommonCLASSGroup('MPOLYGON', 'MPolygon', '"AcMPolygonObj15"', 3071, 1, True);
  vInstCount := Converter.Counts[csTableStyles];
  if vInstCount > 0 then
  begin
    AddCommonCLASSGroup('TABLESTYLE', 'TableStyle', cntAppNameObjectDBX, 2047,
      vInstCount, False);
    AddCommonCLASSGroup('ACAD_TABLE', 'Table', cntAppNameObjectDBX, 1025, 1, True);
  end;

  vDict := TsgDXFDictionary(Converter.Sections[csObjects].FindEntByName(cnstDictionary));
  if Assigned(vDict) then
  begin
    vDictItem := TsgDXFOwneredItem(vDict.FindEntByName(sAcadPlotSettingsDictionary));
    if Assigned(vDictItem) then
    begin
      AddCommonCLASSGroup('PLOTSETTINGS', 'PlotSettings', cntAppNameObjectDBX, 0,
        vDictItem.Count, False);
    end;


    vDictItem := TsgDXFOwneredItem(vDict.FindEntByName(sAcadImageVars));
    if Assigned(vDictItem)then
    begin
      AddCommonCLASSGroup('RASTERVARIABLES', 'RasterVariables', 'ISM', 0,
        1, False);
    end;
  end;
  if FContentOleByImage then
  begin
    AddCommonCLASSGroup('DICTIONARYVAR', 'DictionaryVar', cntAppNameObjectDBX, 0,
      1, False);
  end;
  // calc spatial filters count
  vInstCount := GetSpatialFiltersCount;
  if vInstCount > 0 then
    AddCommonCLASSGroup(cnstSPATIAL_FILTER, cnstSpatialFilterMarker,
      cntAppNameObjectDBX, 0, vInstCount, False);
  if Version in [acR13 .. acR14] then
    AddCommonCLASSGroup(cnstXRecord, sXrecord, cntAppNameObjectDBX{AutoCAD 2000?}, 0, 1, False);
  if FCounts[ceMesh] > 0 then
    AddCommonCLASSGroup('MESH', 'SubDMesh', cntAppNameObjectDBX, 0, FCounts[ceMesh], True);
  EndSection;
end;

procedure TsgCADtoDXF.ExportSectionENTITIES;

  procedure IterateLayout(ALayout: TsgDXFLayout);
  var
    I: Integer;
    V: TsgDXFViewPort;
    vInserts: TsgObjectList;
  begin
    FHasViewPortWindow := False;
    if ALayout = nil then Exit;
    FPaperMode := not ALayout.IsModel;
    for I := 0 to ALayout.Count - 1 do
      DoExport(ALayout.Entities[I]);

    vInserts := ExpSGInsert(ALayout);
    try
      if Assigned(vInserts) then
        for I := 0 to vInserts.Count - 1 do
          DoExport(TsgDXFEntity(vInserts[I]));
    finally
      TsgObjectList.FreeList(vInserts);
    end;
    if not ALayout.IsModel and not FHasViewPortWindow then
    begin
      // Add VIEWPORT as ACAD window
      V := CreateViewPortAsACADWindow(ALayout);
      try
        DoExport(V);
      finally
        V.Free;
      end;
    end;
  end;
begin
  BeginSection('ENTITIES');
  IterateLayout(Converter.Layouts[0]);
  IterateLayout(GetPaperLayout);
  FPaperMode := False;
  EndSection;
end;

function TsgCADtoDXF.ExportSectionHEADER: Integer;
var
  vCPName: string;
  vHeadVars: PsgHeadVarStruct;

  procedure AddPropName(const APropName: string);
  begin
    AddString(VarName_9, APropName);
  end;

  procedure AddStrProp(const APropName, AValue: string; const ACode: Integer);
  begin
    AddPropName(APropName);
    AddString(ACode, AValue);
  end;

  procedure AddIntProp(const APropName: string; const AValue: Integer;
    const ACode: Integer = Integer_70);
  begin
    AddPropName(APropName);
    AddInt(ACode, AValue);
  end;

  procedure AddBoolProp(const APropName: string; const AValue: Boolean;
    const ACode: Integer = 290);
  begin
    AddIntProp(APropName, Ord(AValue), ACode);
  end;

  procedure AddFltProp(const APropName: string; const AValue: Double;
    const ACode: Integer = Float_40);
  begin
    AddPropName(APropName);
    AddFloat(ACode, AValue);
  end;

  procedure Add2DPointProp(const APropName: string; const AValue: TF2DPoint;
    const ACode: Integer = XFirst_10);
  begin
    AddPropName(APropName);
    Add2DPoint(ACode, AValue);
  end;

  procedure Add3DPointProp(const APropName: string; const AValue: TFPoint;
    const ACode: Integer = XFirst_10);
  begin
    AddPropName(APropName);
    Add3DPoint(ACode, AValue);
  end;

  procedure AddTimeStamp(const APropName: string; const ATimeStamp: TTimeStamp); overload;
  var
    vJDDateTime: Double;
  begin
    vJDDateTime := ATimeStamp.Date + ATimeStamp.Time / MSecsPerDay;
    AddFltProp(APropName, vJDDateTime);
  end;

  procedure AddTimeStamp(const APropName: string; const ADateTime: TDateTime); overload;
  var
    vJDDateTime: Double;
  begin
    vJDDateTime := DateTimeToJulianDate(IncHour(ADateTime, 12));
    AddFltProp(APropName, vJDDateTime);
  end;

  procedure ExportReferens(const AName: TsgDimNameVal;
    const ArrowType: TsgDimArrowType);
  const
    cnstDimBlkVarNames: array[vnDIMBLK .. vnDIMLRBLK] of string = (
      '$DIMBLK', '$DIMBLK1', '$DIMBLK2', '$DIMLDRBLK');
  begin
    if (ArrowType = datUndefined) or (ArrowType = datClosedfilled) then
      AddStrProp(cnstDimBlkVarNames[AName], '', 1)
    else
      AddStrProp(cnstDimBlkVarNames[AName], sgDimensionArrowTypeNames[ArrowType], 1);
  end;

  procedure ExportArrows(Arrows: TsgArrows);
  begin
    if (Arrows.Blk1 = Arrows.Blk2) then
    begin
      if Arrows.Blk = datUndefined then
        Arrows.Blk := datClosedfilled;
      ExportReferens(vnDIMBLK, Arrows.Blk);
      ExportReferens(vnDIMBLK1, datClosedfilled);
      ExportReferens(vnDIMBLK2, datClosedfilled);
    end
    else
    begin
      ExportReferens(vnDIMBLK, datClosedfilled);
      ExportReferens(vnDIMBLK1, Arrows.Blk1);
      ExportReferens(vnDIMBLK2, Arrows.Blk2);
    end;
    ExportReferens(vnDIMLRBLK, Arrows.LrBlk);
  end;

begin
  Result := 0;
  vHeadVars := TsgCADConverter(Converter).PHeadVarStruct;
  if OutFileInfo then
  begin
    AddString(999, cnstExporterSoftwareInfo);
    AddString(999, TsgCADImageEx(ExportCADImage).FileInfo);
  end;
  BeginSection(cnstSectionHEADER);
  AddStrProp('$ACADVER', GetStringDXFVersion, 1);
  if vHeadVars^.CodePage = 65001 then
    vCPName := sgDXFCodePageName(LCIDToCodePage(SysLocale.DefaultLCID))
  else
    vCPName := sgDXFCodePageName(vHeadVars^.CodePage);
  AddStrProp('$DWGCODEPAGE', vCPName, 3);
  if vHeadVars^.CEColor.Active = acIndexColor then
    if vHeadVars^.CEColor.Color <> clDXFByLayer then
      AddIntProp('$CECOLOR', vHeadVars^.CEColor.Color, 62);
  AddStrProp('$CLAYER', vHeadVars^.CLayer, 8);
  AddStrProp('$CELTYPE', vHeadVars^.CELType, 6);
  AddFltProp('$CELTSCALE', vHeadVars^.CELTScale);
  AddIntProp('$CELWEIGHT', CorrectLineWeight(vHeadVars^.CELWeight, False), 370);
  AddIntProp('$DIMALT', Ord(vHeadVars^.DimProps.Alt));
  AddFltProp('$DIMALTF', vHeadVars^.DimProps.AltF);
  AddStrProp('$DIMPOST', vHeadVars^.DimProps.Post, 1);
  AddStrProp('$DIMAPOST', vHeadVars^.DimProps.APost, 1);
  AddFltProp('$DIMSCALE', vHeadVars^.DimProps.Scale);//
  AddFltProp('$DIMASZ', vHeadVars^.DimProps.Asz);
  AddIntProp('$DIMCLRD', ColorToDXF(vHeadVars^.DimProps.Clrd));
  AddIntProp('$DIMCLRE', ColorToDXF(vHeadVars^.DimProps.Clre));
  AddIntProp('$DIMCLRT', ColorToDXF(vHeadVars^.DimProps.Clrt));
  AddFltProp('$DIMEXE', vHeadVars^.DimProps.Exe);
  AddFltProp('$DIMEXO', vHeadVars^.DimProps.Exo);
  AddFltProp('$DIMGAP', vHeadVars^.DimProps.Gap);
  AddFltProp('$DIMLFAC', vHeadVars^.DimProps.LFac);
  AddIntProp('$DIMDEC', vHeadVars^.DimProps.Dec);
  AddIntProp('$DIMFRAC', vHeadVars^.DimProps.Frac);
  AddIntProp('$DIMTAD', vHeadVars^.DimProps.Tad);
  AddFltProp('$DIMCEN', vHeadVars^.DimProps.Cen);
  AddIntProp('$DIMTIH', Ord(vHeadVars^.DimProps.Tih));
  AddIntProp('$DIMTIX', vHeadVars^.DimProps.Tix);
  AddIntProp('$DIMTOH', Ord(vHeadVars^.DimProps.Toh));
  AddIntProp('$DIMSE1', Ord(vHeadVars^.DimProps.SE1));
  AddIntProp('$DIMSE2', Ord(vHeadVars^.DimProps.SE2));
  AddIntProp('$DIMSD1', Ord(vHeadVars^.DimProps.SD1));
  AddIntProp('$DIMSD2', Ord(vHeadVars^.DimProps.SD2));
  AddIntProp('$DIMLWD', CorrectLineWeight(vHeadVars^.DimProps.LwD, False));
  AddIntProp('$DIMLWE', CorrectLineWeight(vHeadVars^.DimProps.LwE, False));
  AddIntProp('$DIMSAH', Ord(vHeadVars^.DimProps.Sah));
  ExportArrows(vHeadVars^.DimProps.Arrows);
  AddStrProp('$DIMSTYLE', vHeadVars^.DimStyle, 2);
  AddFltProp('$DIMTP', vHeadVars^.DimProps.Tp);
  AddFltProp('$DIMTM', vHeadVars^.DimProps.Tm);
  AddFltProp('$DIMTXT', vHeadVars^.DimProps.Txt);
  AddIntProp('$DIMLUNIT', GetDimLimitUnitsByte(vHeadVars^.DimProps.LUnit));
  AddIntProp('$DIMDSEP', Ord(vHeadVars^.DimProps.DSep));
  AddStrProp('$DIMTXSTY', ExtractName(vHeadVars^.DimTextStyle), 7);
  AddFltProp('$TEXTSIZE', vHeadVars^.TextSize);
  AddStrProp('$TEXTSTYLE', ExtractName(vHeadVars^.TextStyle), 7);
  if Ord(Version) > Ord(acR12) then
  begin
    // Setting dummy value of next available handle
    AddPropName('$HANDSEED');
    Result := FStream.Position;// $HANDSEED position
    AddString(5, Format('%16d', [0]));//digits = cnstDigitsInHandle
  end;
  AddFltProp('$FILLETRAD', vHeadVars^.FilletRadius);
  AddIntProp('$INSUNITS', vHeadVars^.InsUnits);

  Add2DPointProp('$EXTMIN', vHeadVars^.ExtMin.Point2D);
  Add2DPointProp('$EXTMAX', vHeadVars^.ExtMax.Point2D);
  Add2DPointProp('$LIMMIN', vHeadVars^.LimMin.Point2D);
  Add2DPointProp('$LIMMAX', vHeadVars^.LimMax.Point2D);
  AddIntProp('$FILLMODE', Ord(vHeadVars^.FillMode));
  AddFltProp('$LTSCALE', Converter.LTScale);
  AddIntProp('$MEASUREMENT', Integer(vHeadVars^.Measurement));
  AddIntProp(cnstAttMode, Integer(vHeadVars^.AttMode));
  AddIntProp('$PDMODE', vHeadVars^.PointDisplayMode);
  AddFltProp('$PDSIZE', vHeadVars^.PointDisplaySize);
  if (Converter.LayoutsCount > 2) and (Converter.Layouts[1] <> nil) then
  begin
    Add3DPointProp('$PEXTMIN', vHeadVars^.PExtMin);
    Add3DPointProp('$PEXTMAX', vHeadVars^.PExtMax);
    Add2DPointProp('$PLIMMIN', vHeadVars^.PLimMin.Point2D);
    Add2DPointProp('$PLIMMAX', vHeadVars^.PLimMax.Point2D);
  end;
  AddIntProp('$TILEMODE', Ord(ExportCADImage.CurrentLayout.IsModel));
  Add3DPointProp('$UCSORG', vHeadVars^.UCSORG);
  Add3DPointProp('$UCSXDIR', vHeadVars^.UCSXDIR);
  Add3DPointProp('$UCSYDIR', vHeadVars^.UCSYDIR);
  Add3DPointProp('$INSBASE', vHeadVars^.InsBase);
  if Version >= acR2004 then
  begin
    if Version < acR2010 then
      AddBoolProp('$XCLIPFRAME', vHeadVars^.XClipFrame <> 0, 290)
    else
      AddIntProp('$XCLIPFRAME', vHeadVars^.XClipFrame, 280);
    AddIntProp('$DIMASSOC', vHeadVars^.DimAssoc, 280);
  end;
  if Version >= acR2000 then
    AddBoolProp('$LWDISPLAY', vHeadVars^.LwDisplay <> 0, 290);
  // time stamps
  AddTimeStamp('$TDCREATE', Converter.DrwPropCreatedDateTime);
  AddTimeStamp('$TDINDWG', Converter.DrwPropTotalEditingTime);
  AddTimeStamp('$TDUPDATE', Converter.DrwPropModifiedDateTime);
  EndSection;
end;

procedure TsgCADtoDXF.AddObj(const AName: string; AHandle, AOwner: UInt64);
begin
  BeginObject(AName);
  AddHexInt(FHandleCode, AHandle);
  if AOwner <> cnstBadHandle then
    AddHexInt(cParentHandle_330, AOwner);
end;

function TsgCADtoDXF.AddDictRefs(const AName: string; const AHandle: UInt64;
  const AHard: Boolean = False): UInt64;
begin
  Result := AHandle;
  AddString(3, AName);
  AddHexInt(HandleRefCodes[Ord(AHard)], Result);
end;

function TsgCADtoDXF.AddDictRefs(const AEntity: TsgDXFEntity;
  const AHard: Boolean = False): UInt64;
begin
  Result := AddDictRefs(AEntity.Name, AEntity.Handle, AHard);
end;

function TsgCADtoDXF.AddDictNamed(const AName, ASubClassName: string;
  const AHandle: UInt64; const AParent: UInt64 = 0): UInt64;
begin
  Result := AHandle;
  AddObj(AName, Result, AParent);
  AddSubclassMarker(ASubClassName);
end;

function TsgCADtoDXF.AddDict(const AHandle: UInt64; const AParent: UInt64 = 0): UInt64;
begin
  Result := AddDictNamed(cnstDictionary, cnstDictSubClass, AHandle, AParent);
end;

function TsgCADtoDXF.ExportDictionaryVariables(ADictionaryVariablesDictionaryHandle: UInt64): Boolean;
const
  cnstDictionaryVariables = 'DictionaryVariables';
type
  TDictionaryVariableItem = record
    Handle: UInt64;
    Name: string;
    Value: Integer;
  end;
var
  I: Integer;
  DictVarItems: array of TDictionaryVariableItem;

  function MakeDVItem(const AHandle: UInt64; const AName: string;
    AValue: Integer): TDictionaryVariableItem;
  begin
    Result.Handle := AHandle;
    Result.Name := AName;
    Result.Value := AValue;
  end;

  function AddDVItem(const AName: string;
    AValue: Integer): Integer;
  begin
    Result := Length(DictVarItems);
    SetLength(DictVarItems, Result + 1);
    DictVarItems[Result] := MakeDVItem(DoHandle, AName, AValue);
  end;

  procedure ExportDictVar(const AParent, AHandle: UInt64;
    const AValue: Integer);
  begin
    AddObj(cnstDictionaryVar, AHandle, AParent);
    AddString(100, cnstDictionaryVariables);
    AddInt(280, 0);//Object schema number (currently set to 0)
    AddInt(1, AValue);
  end;

begin
  AddDict(ADictionaryVariablesDictionaryHandle);
  AddInt(281, 1);
  if FContentOleByImage then
    AddDVItem(sDictionaryVarOLEFRAME, 0)
  else
  begin
    if Converter.OleFrame <> cnstOleFrameDefault then
      AddDVItem(sDictionaryVarOLEFRAME, Converter.OleFrame);
    if Converter.HeadVarStruct.XClipFrame <> cnstDefHeadVarStruct.XClipFrame then
      AddDVItem(sDictionaryVarXCLIPFRAME, Converter.HeadVarStruct.XClipFrame);
  end;
  for I := Low(DictVarItems) to High(DictVarItems) do
    AddDictRefs(DictVarItems[I].Name, DictVarItems[I].Handle);
  for I := Low(DictVarItems) to High(DictVarItems) do
    ExportDictVar(ADictionaryVariablesDictionaryHandle, DictVarItems[I].Handle, DictVarItems[I].Value);
  Result := True;
end;

procedure TsgCADtoDXF.ExportSectionOBJECTS;

  procedure ExportPlotSettings(APlotSettings: TsgDXFPlotSettings;
    AInline: Boolean = False; AIsModel: Boolean = False;
    AOwnerDictHandle: UInt64 = 0);
  var
    vPlotData: PsgPlotSettingsData;
    vPlotFlag: Integer;
  begin
    vPlotData := TsgDXFPlotSettingsAccess(APlotSettings).PlotData;
    if not AInline then
      AddObj(cnstObjectsPLOTSETTINGS, APlotSettings.Handle, AOwnerDictHandle);
    AddSubclassMarker(sPlotSettingsMarker);
    AddString(1,vPlotData^.PageSetupName);
    AddString(2,vPlotData^.PrintOrConfigName);
    AddString(4,vPlotData^.PaperSize);
    AddString(6,vPlotData^.PlotViewName);
    AddFloat(40,vPlotData^.UnprintableMargin.Left);
    AddFloat(41,vPlotData^.UnprintableMargin.Bottom);
    AddFloat(42,vPlotData^.UnprintableMargin.Right);
    AddFloat(43,vPlotData^.UnprintableMargin.Top);

    AddFloat(44,vPlotData^.PlotPaperSize.X);
    AddFloat(45,vPlotData^.PlotPaperSize.Y);
    AddFloat(46,vPlotData^.PlotOrigin.X);
    AddFloat(47,vPlotData^.PlotOrigin.Y);
    AddFloat(48,vPlotData^.PlotWindowAreaMin.X);
    AddFloat(49,vPlotData^.PlotWindowAreaMin.Y);

    vPlotFlag := ConvertPlotLayoutFlagsToInteger(vPlotData^.PlotLayoutFlags);
    if AIsModel then
      vPlotFlag := vPlotFlag or 1024;
    AddInt(70, vPlotFlag);
    AddInt(72, Integer(vPlotData^.PlotPaperUnits));
    AddInt(73, Integer(vPlotData^.PlotRotation));
    AddInt(74, Integer(vPlotData^.PlotType));
    AddInt(75, vPlotData^.StandardScaleType);
    AddInt(76, vPlotData^.ShadePlotMode);
    AddInt(77, vPlotData^.ShadePlotResolutionLevel);
    AddInt(78, vPlotData^.ShadePlotCustomDPI);

    AddFloat(140,vPlotData^.PlotWindowAreaMax.X);
    AddFloat(141,vPlotData^.PlotWindowAreaMax.Y);
    AddFloat(142,vPlotData^.NumeratorOfCustomPrintScale);
    AddFloat(143,vPlotData^.DenominatorOfCustomPrintScale);
    AddFloat(147,vPlotData^.FloatingPointScaleFactor);
    AddFloat(148,vPlotData^.PaperImageOrigin.X);
    AddFloat(149,vPlotData^.PaperImageOrigin.Y);
  end;

  procedure ExportLayout(ALayout: TsgDXFLayout; AOwnerDictionary: UInt64; AIndex: Integer);
  var
    vBox: TFRect;
  begin
    BeginObject(cntClassDXFLAYOUT);
    AddHandle(ALayout, AOwnerDictionary, [AOwnerDictionary]);
    ExportPlotSettings(ALayout.PlotSettings, True, ALayout.IsModel);
    AddSubclassMarker('Layout');
    AddString(1, ALayout.Name);
    AddInt(70, 1);
    AddInt(71, AIndex);
    vBox := ALayout.Box;
    Add2DPoint(10, MakeF2DPoint(vBox.Left, vBox.Bottom));
    Add2DPoint(11, MakeF2DPoint(vBox.Right, vBox.Top));
    Add3DPoint(12, cnstFPointZero);
    Add3DPoint(14, MakeFPoint(vBox.Left, vBox.Bottom, vBox.Z1));
    Add3DPoint(15, MakeFPoint(vBox.Right, vBox.Top, vBox.Z2));
    AddFloat(146,0);
    Add3DPoint(13, cnstFPointZero);
    Add3DPoint(16, MakeFPoint(1,0,0));
    Add3DPoint(17, MakeFPoint(0,1,0));
    AddInt(76, 0);
    AddHexInt(cParentHandle_330, GetHandleByEntity(ALayout.PaperSpaceBlock.BlockRecord));
  end;

  procedure ExportImageDef(const AEntity: TsgDXFEntity;
    const AOwnerHandle: UInt64);
  var
    vImageDef: TsgDXFImageDef;
  begin
    if not (IsEntity(AEntity) and (AEntity is TsgDXFImageDef)) then
      Exit;
    vImageDef := TsgDXFImageDef(AEntity);
    if (vImageDef.Picture = nil) then
      Exit;
    BeginObject(cntClassDXFIMAGEDEF);
    AddHandle(vImageDef, AOwnerHandle, []);
    AddSubclassMarker('RasterImageDef');
    AddInt(90, 0);
    AddString(1, vImageDef.FileName);
    if vImageDef.Picture.Graphic = nil then
    begin
      Add2DPoint(10, cnstF2DPointZero);
      Add2DPoint(11, cnstF2DPointZero);
    end
    else
    begin
      Add2DPoint(10, vImageDef.Size.Point2D);
      Add2DPoint(11, MakeF2DPoint(1 / vImageDef.Size.X, 1 / vImageDef.Size.Y));
    end;
    AddInt(280, 1);//Image-is-loaded flag. 0 = Unloaded; 1 = Loaded
    AddInt(281, 0);//Resolution units. 0 = No units; 2 = Centimeters; 5 = Inch
  end;

var
  I, K, vLayoutsCount, vTableStylesCount: Integer;
  vMLineStylesCount: Integer;
  J: TsgAcadTableCellType;
  vOwnerTblStyleHandle: UInt64;
  vOwnerPlotSettingsHandle: UInt64;
  vOwnerRasterVariablesHandle: UInt64;
  vAcDbVariableDictionaryHandle: UInt64;
  vDicHandle: UInt64;
  vGroupHandle, vMatHandle, vWipeoutHandle, vMLineHandle, vLayoutHandle: UInt64;
  vEnt: TsgDXFEntity;
  vAcadTable: TsgDXFAcadTableStyle;
  vCellStyle: TsgAcadTableCellStyle;
  vObjects: TsgDXFEntity;
  vPlotSettingsDictionary: TsgDXFEntity;
  vRasterVariablesDictionary: TsgDXFEntity;
  vAcDbVariableDictionary: TsgDXFEntity;
  vRasterVariables: TsgCADRasterVariables;
  vImageDefsHandle: UInt64;
  vInfo: TsgDXFEntity;
  vMLineStyle: TsgMLineStyle;
  vMLineEntry: TsgMLineEntry;
  vXrecord: TsgDXFXRecord;
{$IFNDEF SG_NON_WIN_PLATFORM}
  vFV: TVSFixedFileInfo;
  vLongVer: UInt64;
{$ENDIF}
begin
  vOwnerTblStyleHandle := 0;
  vLayoutHandle := 0;
  vOwnerPlotSettingsHandle := 0;
  vOwnerRasterVariablesHandle:= 0;
  vAcDbVariableDictionaryHandle := 0;
  vPlotSettingsDictionary := nil;
  vLayoutsCount := Converter.LayoutsCount;
  vTableStylesCount := Converter.Counts[csTableStyles];
  BeginSection(cnstSectionOBJECTS);
  // Default entry "NamedObject dictionary"
  vDicHandle := AddDict(DoHandle);
  vImageDefsHandle := cnstBadHandle;
  if (ImageDefList.Count + Converter.Counts[csImageDefs] > 0) then
    vImageDefsHandle := AddDictRefs(sAcadImageDict, DoHandle);
  vGroupHandle := AddDictRefs(sAcadGroupDictionary, DoHandle);
  vMatHandle := AddDictRefs(sAcadMaterialDictionary, DoHandle);
  vWipeoutHandle := AddDictRefs(sAcadWipeOutVarsDictionary, DoHandle);
  vMLineHandle := AddDictRefs(sAcadMLineStyleDictionary, DoHandle);
  vObjects := Converter.Sections[csObjects];

  if Assigned(vObjects) then
  begin
    // plotsettings
    vPlotSettingsDictionary := vObjects.FindEntByName(cnstObjectsPLOTSETTINGS);
    if Assigned(vPlotSettingsDictionary) and (vPlotSettingsDictionary.Count > 0) then
      vOwnerPlotSettingsHandle := AddDictRefs(sAcadPlotSettingsDictionary, DoHandle);
    // rastervariables
    vRasterVariablesDictionary := TsgDXFOwneredItem(vObjects.FindEntByName(sRasterVariables));
    if Assigned(vRasterVariablesDictionary)then
      vOwnerRasterVariablesHandle := AddDictRefs(sAcadImageVars, DoHandle);
    // variabledictionary
    vAcDbVariableDictionary := vObjects.FindEntByName(sAcDbVariableDictionary);
    if Assigned(vAcDbVariableDictionary) then
      vAcDbVariableDictionaryHandle := AddDictRefs(sAcDbVariableDictionary, DoHandle);
  end;

  if (FContentOleByImage) and (vAcDbVariableDictionaryHandle = cnstBadHandle) then
    vAcDbVariableDictionaryHandle := AddDictRefs(sAcDbVariableDictionary, DoHandle);

  // Add table styles only if TableStylesCount > 0 !
  if vTableStylesCount > 0 then
    vOwnerTblStyleHandle := AddDictRefs(sAcadTableStyleDictionary, DoHandle);
  // Add Layouts only if LayoutsCount > 1 !
  if (Ord(Version) > Ord(acR14)) and (vLayoutsCount >= 1) then
    vLayoutHandle := AddDictRefs(sAcadLayoutDictionary, DoHandle);

  AddDict(vGroupHandle, vDicHandle);
  AddDict(vMatHandle, vDicHandle);
  AddDictNamed(cntClassDXFWIPEOUTVARIABLES, 'WipeoutVariables', vWipeoutHandle, vDicHandle);
  AddInt(70, Ord(not Converter.HideWipeOuts));

  { MLINESTYLE }

  vMLineStylesCount := Converter.Counts[csMLineStyles];
  AddDict(vMLineHandle, vDicHandle);
  if vMLineStylesCount = 0 then
  begin
    vMLineHandle := AddDictRefs(sStandardName, DoHandle);
    AddDictNamed(AnsiUpperCase(sMLineStyle), sMLineStyle, vMLineHandle);
    AddString(2, sStandardName);
    AddInt(cFlag_70, 0);
    AddString(3, '');
    AddInt(62, clDXFByLayer);
    AddFloat(51, 90.0);
    AddFloat(52, 90.0);
    AddInt(71, 2);
    AddFloat(49, 0.5);
    AddInt(62, clDXFByLayer);
    AddString(6, sByLayer);
    AddFloat(49, -0.5);
    AddInt(62, clDXFByLayer);
    AddString(6, sByLayer);
  end
  else
  begin
    for I := 0 to vMLineStylesCount - 1 do
    begin
      vEnt := Converter.Sections[csMLineStyles][I];
      if vEnt is TsgMLineStyle then
        AddDictRefs(vEnt);
    end;
    for I := 0 to vMLineStylesCount - 1 do
    begin
      vEnt := Converter.Sections[csMLineStyles][I];
      if vEnt is TsgMLineStyle then
      begin
        vMLineStyle := TsgMLineStyle(vEnt);
        AddDictNamed(AnsiUpperCase(sMLineStyle), sMLineStyle, vEnt.Handle, vMLineHandle);
        AddString(2, vMLineStyle.Name);
        AddInt(cFlag_70, vMLineStyle.Flags);
        AddString(3, '');
        AddInt(62, ColorToDXF(vMLineStyle.ColorCAD));
        AddFloat(51, 90.0);
        AddFloat(52, 90.0);
        AddInt(71, vMLineStyle.Count);
        for K := 0 to vMLineStyle.Count-1 do
        begin
          vMLineEntry := TsgMLineEntry(vMLineStyle.Entries[K]);
          AddFloat(49, vMLineEntry.Offset);
          AddInt(62, ColorToDXF(vMLineEntry.ColorCAD));
          AddString(6, GetEntityName(vMLineEntry.LineType, ''));
        end;
      end;
    end;
  end;

  { TABLESTYLE }
  if vOwnerTblStyleHandle <> cnstBadHandle then
  begin
    AddDict(vOwnerTblStyleHandle);
    AddInt(281, 1);
    for I := 0 to vTableStylesCount - 1 do
      AddDictRefs(Converter.Sections[csTableStyles][I]);
    for I := 0 to vTableStylesCount - 1 do
    begin
      vEnt := Converter.Sections[csTableStyles][I];
      AddDictNamed(cntClassDXFTABLESTYLE, 'TableStyle', vEnt.Handle, vOwnerTblStyleHandle);
      vAcadTable := TsgDXFAcadTableStyle(vEnt);
      AddString(3, vAcadTable.Description);
      AddInt(70, vAcadTable.FlowDirection);
      AddInt(71, vAcadTable.Flags);
      AddFloat(40, vAcadTable.HorzCellMargin);
      AddFloat(41, vAcadTable.VertCellMargin);
      AddInt(280, Ord(vAcadTable.TitleSuppressed));
      AddInt(281, Ord(vAcadTable.HeaderSuppressed));
      for J := Low(TsgAcadTableCellType) to High(TsgAcadTableCellType) do
      begin
        vCellStyle := vAcadTable.CellStyle[J];
        if Assigned(vCellStyle.TextStyle) then
          AddString(7, ExtractName(vCellStyle.TextStyle.Name));
        AddFloat(140, vCellStyle.TextHeight);
        AddInt(170, vCellStyle.CellAlignment);
        AddInt(62, ColorToDXF(vCellStyle.TextColor));
        AddInt(63, ColorToDXF(vCellStyle.FillColor));
        AddInt(283, Ord(vCellStyle.BkColorOn));
        //AddInt(90, vCellStyle.DataType); // ??
        //AddInt(91, vCellStyle.UnitType); // ??
        for K := 0 to vCellStyle.BorderCount - 1 do
        begin
          AddInt(K + 274, CorrectLineWeight(vCellStyle.BorderLineWeight[K], False));
          AddInt(K + 284, Ord(vCellStyle.BorderVisible[K]));
          AddInt(K + 64, ColorToDXF(vCellStyle.BorderColor[K]));
        end;
      end;
    end;
  end;
  { LAYOUTS }
  if vLayoutHandle <> cnstBadHandle then
  begin
    AddDict(vLayoutHandle);
    AddInt(281, 1);
    for I := 0 to vLayoutsCount - 1 do
      AddDictRefs(Converter.Layouts[I]);
    for I := 0 to vLayoutsCount - 1 do
      ExportLayout(Converter.Layouts[I], vLayoutHandle, I + 1);
  end;
  {RASTERVARIABLE}
  if vOwnerRasterVariablesHandle <> cnstBadHandle then
  begin
    vRasterVariables := TsgCADRasterVariables(vObjects.FindEntByName(sRasterVariables));
    if Assigned(vRasterVariables) then
    begin
      AddDictNamed(sRasterVariables, 'RasterVariables', vOwnerRasterVariablesHandle, vDicHandle);
      AddInt(90, 0);
      AddInt(70, Ord(vRasterVariables.DisplayImageFrame));
      AddInt(71, Ord(vRasterVariables.ImageDisplayQuality));
      AddInt(72, vRasterVariables.UnitsForInsertingImages);
    end;
  end;
  {AcDbVariableDictionary}
  if vAcDbVariableDictionaryHandle <> cnstBadHandle then
  begin
    ExportDictionaryVariables(vAcDbVariableDictionaryHandle);
  end;

  {PLOT}
  if vOwnerPlotSettingsHandle <> cnstBadHandle then
  begin
    AddDict(vOwnerPlotSettingsHandle);
    AddInt(281, 1);
    for I := 0 to vPlotSettingsDictionary.Count - 1 do
      AddDictRefs(vPlotSettingsDictionary[I]);
    for I := 0 to vPlotSettingsDictionary.Count - 1 do
    begin
      ExportPlotSettings(TsgDXFPlotSettings(vPlotSettingsDictionary[I]), False,
        False,
        vOwnerPlotSettingsHandle);
    end;
  end;
  { IMAGEDEFS }
  if vImageDefsHandle <> cnstBadHandle then
  begin
    AddDict(vImageDefsHandle);
    AddInt(281, 1);
    for I := 0 to Converter.Counts[csImageDefs] - 1 do
      AddDictRefs(Converter.ImageDefs[I].EntName, Converter.ImageDefs[I].Handle);
    for I := 0 to ImageDefList.Count - 1 do
      AddDictRefs(TsgDXFImageDef(ImageDefList.List[I]).EntName, TsgDXFImageDef(ImageDefList.List[I]).Handle);
    for I := 0 to Converter.Counts[csImageDefs] - 1 do
      ExportImageDef(Converter.ImageDefs[I], vImageDefsHandle);
    for I := 0 to ImageDefList.Count - 1 do
      ExportImageDef(TsgDXFImageDef(ImageDefList.List[I]), vImageDefsHandle);
    ClearImageDefsList;
  end;
  ExportExtendedDictionaries;

  vInfo := CreateNewEntity(TsgDXFDictionary, cnstDictionary);
  try
    vXrecord := TsgDXFXRecord(CreateNewEntity(TsgDXFXRecord, 'XINFO'));
    vInfo.AddEntity(vXrecord);
    AddDict(vInfo.Handle);
    AddInt(280, 1); //Hard-owner flag
    AddDictRefs(vXrecord, True);
{$IFNDEF SG_NON_WIN_PLATFORM}
    if GetFileVerFixed(GetApplicationName, vFV) then
    begin
      vLongVer := vFV.dwFileVersionMS;
      vLongVer := vLongVer shl 32 or vFV.dwFileVersionLS;
      vXrecord.Data.AddBinary(310, SizeOf(UInt64), PByte(@vLongVer));
    end;
{$ENDIF}
    vXrecord.Data.AddBinary(311, SizeOf(cnstXInfoZero), PByte(@cnstXInfoZero));
    ExportXrecord(vXrecord);
    FXInfoOffs := FStream.Position - SizeOf(UInt64) shl 1 - Length(sEnter);
  finally
    vInfo.Free;
  end;

  EndSection;
end;

procedure TsgCADtoDXF.ExportSectionTABLES;
begin
  BeginSection('TABLES');
  ExportTablesAPPID;
  if Ord(Version) >= Ord(acR14) then
    ExportTablesBLOCK_RECORD;
  ExportTablesSTYLE;
  ExportTablesLTYPE;
  ExportTablesLAYER;
  ExportTablesVPORT;
  ExportTablesVIEW;
  ExportTablesUCS;
  if Ord(Version) >= Ord(acR14) then
    ExportTablesDIMSTYLE;
{
  ExportTablesVPORT;
  ExportTablesLTYPE;
  ExportTablesLAYER;
  ExportTablesSTYLE;
  ExportTablesVIEW;
  ExportTablesUCS;
  ExportTablesAPPID;
  ExportTablesDIMSTYLE;
  if Ord(Version) >= Ord(acR14) then
    ExportTablesBLOCK_RECORD;
}
  EndSection;
end;

function TsgCADtoDXF.GetStringDXFVersion: string;
begin
  Result := sgDWGVerToString(Version);
end;

function TsgCADtoDXF.GetHandleByEntity(AEntity: TsgDXFEntity): UInt64;
var
  vBlockRecord: TsgDXFBlockRecord;
begin
  if AEntity = nil then
    Result := cnstBadHandle
  else
  begin
    Result := AEntity.Handle;
    if AEntity is TsgDXFBlock then
    begin
      vBlockRecord := GetBlockRecordByBlock(TsgDXFBlock(AEntity), '');
      if vBlockRecord <> nil then
        Result := vBlockRecord.Handle;
    end;
    if Result = cnstBadHandle then
      raise EWriteError.CreateFmt('No handle: %s', [AEntity.EntName]);
  end;
end;

function TsgCADtoDXF.GetHandleByEntityNoCheck(AEntity: TsgDXFEntity): UInt64;
begin
  if Assigned(AEntity) then
    Result := AEntity.Handle
  else
    Result := cnstBadHandle;
end;

function TsgCADtoDXF.GetEntityLayerName(const AEntity: TsgDXFEntity): string;
begin
  if (AEntity <> nil) and (AEntity.Layer <> nil) then
    Result := ExtractName(AEntity.Layer.Name)
  else
    Result := '0';
end;

function TsgCADtoDXF.GetEntityName(const AEntity: TsgDXFEntity;
  const ADefaultName: string): string;
begin
  if Assigned(AEntity) then
    Result := AEntity.Name
  else
    Result := ADefaultName
end;

function TsgCADtoDXF.GetFlagsByName(const AFlags: Integer; const AName: string): Integer;
const
  cnstXrefFlags = 16 or 32 or 64;
begin
  Result := AFlags;
  if not IsXRefInName(AName) then
    Result := Result and (not cnstXrefFlags)
  else
    Result := Result or cnstXrefFlags;
end;

{ TsgCADtoBin }

class function TsgCADtoBin.ACIS_Row_Encode(const ADecoded: AnsiString): AnsiString;
var
  vChar: AnsiChar;
  I, vLen: Integer;
begin
  vLen := Length(ADecoded);
  SetLength(Result, vLen);
  for I := cnstStrBegin to vLen + (cnstStrBegin - 1) do
  begin
    vChar := ADecoded[I];
    if vChar <> #0 then
      Result[I] := ACIS_Cipher(vChar)
    else
      Result[I] := ' ';
  end;
end;

procedure TsgCADtoBin.AddCode(const AValue: Integer);
begin
  FStream.Write(AValue, 2);
end;

procedure TsgCADtoBin.AddFloat(const Code: Integer; Value: Double);
begin
  FStream.Write(Code, 2);
  FStream.Write(Value, 8);
end;

procedure TsgCADtoBin.AddInt(const Code, Value: Integer);
var L: Integer;
begin
  case Code of
    290..299:               L := 1;
    90..99, 450..459, 1071: L := 4;
    else                    L := 2;
  end;
  FStream.Write(Code, 2);
  FStream.Write(Value, L);
end;

procedure TsgCADtoBin.AddString(const Code: Integer; const Str: string);
begin
  if Code=999 then Exit;
  FStream.Write(Code, 2);
  WriteStr(Str);
end;

procedure TsgCADtoBin.SaveToStreamCustom(S: TStream);
const Bin: array[0..21] of AnsiChar = 'AutoCAD Binary DXF'#13#10#26#0;
begin
  S.Write(Bin, 22);
  inherited SaveToStreamCustom(S);
end;

function TsgCADtoBin.WriteRawByteStr(const AString: sgRawByteString): Integer;
begin
  Result := Length(AString) * SizeOf(AString[1]);
  if Result > 0 then
    Result := WriteBuffer(Pointer(AString)^, Result)
  else
    Result := 0;
  Inc(Result, WriteBytesArray([0]));
end;

{ TAppIDs }

constructor TAppIDs.Create(ACADExport: TsgCADDirectExport);
begin
  inherited Create;
  FCADExport := TsgCADDirectExportAccess(ACADExport);
  FSortedList := TsgStringList.Create;
  FSortedList.CaseSensitive := False;
  FSortedList.Duplicates := dupIgnore;
  ProcCompare := CompareEntityHandles;
  FSearchAppID := TsgDXFOwneredItem.Create;
  Init(FCADExport.Converter.Sections[csAppID]);
end;

destructor TAppIDs.Destroy;
var
  I: Integer;
  vEnt: TsgDXFEntityAccess;
  vItem: TsgOwneredItem;
begin
  for I := 0 to FSortedList.Count - 1 do
  begin
    vEnt := TsgDXFEntityAccess(FSortedList.Objects[I]);
    vItem := TsgOwneredItem(vEnt.GetNamedItem);
    if Assigned(vItem) and (vItem.Owner = nil) and (vItem.RefCount = 0) then
      vEnt.Free;
  end;
  FSortedList.Free;
  FSearchAppID.Free;
  inherited Destroy;
end;

function TAppIDs.CompareEntityHandles(const A, B: Pointer): Integer;
begin
  Result := TsgDXFEntity(A).Handle - TsgDXFEntity(B).Handle;
end;

function TAppIDs.FindAppID(const AAppIDHandle: UInt64; var I: Integer): Boolean;
begin
  FSearchAppID.Handle := AAppIDHandle;
  I := IndexOf(FSearchAppID);
  Result := (I >= 0) and (I < Count);
end;

function TAppIDs.GetEnt(Index: Integer): TsgDXFEntity;
begin
  Result := TsgDXFEntity(Items[Index]);
end;

function TAppIDs.GetEntity(const AName: string): TsgDXFEntity;
var
  I: Integer;
begin
  Result := nil;
  I := FSortedList.IndexOf(AName);
  if I >= 0 then
    Result := TsgDXFEntity(FSortedList.Objects[I]);
end;

procedure TAppIDs.Init(AAppIDSection: TsgDXFEntity);
var
  I: Integer;
  vEnt: TsgDXFEntity;

  function ForceAppID(const AAppIDName: string): TsgDXFEntity;
  var
    K: Integer;
  begin
    Result := nil;
    if AAppIDName <> '' then
    begin
      K := FSortedList.IndexOf(AAppIDName);
      if K < 0 then
      begin
        Result := TsgDXFAppID.Create;
        Result.Name := AAppIDName;
        Result.Handle := FCADExport.DoHandle;
        FSortedList.AddObject(Result.Name, Result);
      end;
    end;
  end;

begin
  for I := 0 to AAppIDSection.Count - 1 do
  begin
    vEnt := AAppIDSection[I];
    FSortedList.AddObject(vEnt.Name, vEnt);
  end;
  ForceAppID(sACADXDataAppName);
  if FCADExport.Version < acR2007 then
    ForceAppID(sACADXDataAttribAppName);
  ForceAppID(FCADExport.XDataAppName);
  ForceAppID(sURLXDataName);
  I := FSortedList.IndexOf(sACADXDataAppName);
  if I >= 0 then
    FSortedList.Exchange(0, I);
  // sort after table fill
  //FSortedList.Sorted := True;
  for I := 0 to FSortedList.Count - 1 do
    Add(FSortedList.Objects[I]);
  Sorted := True;
end;

procedure TAppIDs.SetEnt(Index: Integer; const Value: TsgDXFEntity);
begin
  Items[Index] := Value;
end;

end.
