{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                Export CAD to DWG                           }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit CADtoDWG;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, {$IFNDEF SG_NON_WIN_PLATFORM} sgOle,{$ENDIF}
{$ENDIF}
{$IFDEF SG_HAS_ACTIVEX}ActiveX, {$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
  SysUtils,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
{$IFNDEF SG_FIREMONKEY}
{$IFDEF SGDEL_2009}
  {$IFDEF SGDEL_XE2}Vcl.Imaging.PNGImage{$ELSE}PNGImage{$ENDIF},
{$ENDIF}
{$ENDIF}
  Classes, CADExport, CADImage, DXFConv, sgConsts,
  ExtData, sgBitmap, sgFunction, Math, sgLists, sgComparer, sgLines, sgDWGBits,
  sgDWGUtils, CADDirectExport, sgDataSegFile
{$IFDEF SGDEL_6}, DateUtils{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF}
  ;
const
  cntDWGR15SectionsCount = 6;

  sInvalidSectNumber: string = 'The section number is invalid';
  sCannotFindTable: string = 'Table (%s) (%d) cannot be found';
  sCannotFindTableCode: string = 'Table (%d) cannot be found';
  sDWGCodeInvalid: string = 'DWG code (%d) id is invalid';
  sHandleInvalid: string = 'Handle is invalid';
  sPlotStyleIsAbsent: string = 'Plot style is absent';
  sBlockIsNil: string = 'Block is nil';
  sLayerIsNil: string = 'Layer is nil';
  sLTypeIsNil: string = 'LType is nil';
  sCannotFindBlock: string = 'Cannot find block %h';
  sListsNotEqual: string = 'Lists are not equal';
  sPaperSpaceLayoutIsNil: string = '*Paper_Space layout is nil';
  sDublicatedHandle: string = 'Duplicated handle';
  sFlathHatchCreationError: string = 'FlathHatch creation error';
  sGradientIsNotSupported: string = 'Gradient is not supported';
  sUnknownSectionName: string = 'Unknown section name "%s"';
  sClassesSectionExportError: string = sAcadSectClasses + ' section number of classes is invalid';
  sGradient2ColorsIsSupported: string = 'Only 2 colors gradients are supperted';

type
  PsgDWGR15Locators = ^TsgDWGR15Locators;
  TsgDWGR15Locators = packed array[0..cntDWGR15SectionsCount - 1] of TdwgLocator;

  EDWGExportError = class(Exception);
  EDWGUnsupportedExportVersion = class(EDWGExportError);

type
  TsgDWGLinkItem = record
    Handle: UInt64;
    Name: string;
  end;

  TsgDWGLinkItems = array of TsgDWGLinkItem;

  PsgDWGCreatedElement = ^TsgDWGCreatedElement;
  TsgDWGCreatedElement = record
    Handle: UInt64;
    OwnerHandle: UInt64;
    Location: Integer;
    DWGObjCode: Word;
    Obj: TObject;
    Size: Integer;
    Name: string;
    Handles: TsgDWGHandleArray;
  end;

  TsgSortElemBy = (seHandle, seLocation, seDWGObjCode, seObject, seOwnerHandle,
    seName);

  TsgDWGCreatedElements = class
  private
    FCount: Integer;
    FElements: array of TsgDWGCreatedElement;
    FFindBeforeAdd: Boolean;
    FSortBy: TsgSortElemBy;
    FSorted: Boolean;
    procedure GotoFirstLastElem(const AElem: TsgDWGCreatedElement; var AIndex: Integer;
      AFirst: Boolean);
    function CountElem(AIndex: Integer; Elem: TsgDWGCreatedElement): Integer;
    function FindBase(const AItem: TsgDWGCreatedElement; var AIndex: Integer): Boolean;
    function Find(const Elem: TsgDWGCreatedElement; ASortBy: TsgSortElemBy;
      AGotoFirstElem: Boolean = False): Integer;
    function GetFindBeforeAdd: Boolean;
    procedure SetSortBy(const Value: TsgSortElemBy);
    procedure SetFindBeforeAdd(const Value: Boolean);
    procedure SortInternal(L, R: Integer);
    procedure Exchange(I, J: Integer);{$IFDEF SG_INLINE} inline;{$ENDIF}
  protected
    function CompareElements(const Element1, Element2: Pointer): Integer;
    function GetElement(AIndex: Integer): TsgDWGCreatedElement;
    function GetPtrElement(AIndex: Integer): PsgDWGCreatedElement;
    procedure GotoFirstElem(const AElem: TsgDWGCreatedElement; var AIndex: Integer);
    procedure GotoLastElem(const AElem: TsgDWGCreatedElement; var AIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function AddElement(const AValue: TsgDWGCreatedElement): Integer; overload;
    function AddElement(const AHandle: UInt64; ALocation: Integer; ADWGObjCode: Word;
      const AName: string = ''; AOwnerHandle: UInt64 = cnstBadHandle; AObject: TObject = nil;
      ASize: Integer = 0): Integer; overload;
    procedure Clear;
    function CountOf(const OwnerHandle: UInt64; var AIndex: Integer): Integer; overload;
    function CountOf(const ALocation: Integer; var AIndex: Integer): Integer; overload;
    function CountOf(const ADWGObjCode: Word; var AIndex: Integer): Integer; overload;
    function CountOf(const AObject: TObject; var AIndex: Integer): Integer; overload;
    function CountOf(const AName: string; var AIndex: Integer): Integer; overload;
    function IndexOf(const AHandle: UInt64; OwnerHandle: Boolean = False;
      PElem: PsgDWGCreatedElement = nil): Integer; overload;
    function IndexOf(const ALocation: Integer; PElem: PsgDWGCreatedElement = nil): Integer; overload;
    function IndexOf(const ADWGObjCode: Word; PElem: PsgDWGCreatedElement = nil): Integer; overload;
    function IndexOf(const AObject: TObject; PElem: PsgDWGCreatedElement = nil): Integer; overload;
    function IndexOf(const AName: string; PElem: PsgDWGCreatedElement = nil): Integer; overload;
    procedure Sort;
    property Count: Integer read FCount;
    property Element[AIndex: Integer]: TsgDWGCreatedElement read GetElement; default;
    property FindBeforeAdd: Boolean read GetFindBeforeAdd write SetFindBeforeAdd;
    property SortBy: TsgSortElemBy read FSortBy write SetSortBy;
  end;

  TsgDWGReadEvent = procedure(ABits: TsgDWGBits) of object;

  TsgCADtoDWG = class(TsgCADDirectExport)
  private
    FSections: TsgObjectList;
    FMaintenanceVersion: Byte;
    FOrigFileSavedVer: Byte;
    FOrigFileSavedReleaseVer: Byte;
    FClasses: TStringList;
    FObjectsMap: TsgDWGCreatedElements;
    FCreatedElements: TsgDWGCreatedElements;
    FCreatedEndBlk: TsgUInt64Pairs;
    FCreatedSeqEnd: TsgUInt64Pairs;
    FDWGCodePage: Word;
    FCodePage: Integer;
    FModelSpaceHandle: UInt64;
    FPaperSpaceHandle: UInt64;
    FScalesDictionary: TStringList;
    FVariableDictionary: TStringList;
    FEntitiesWithAddedHandles: TList;
    FAddedViewPorts: array of TsgDXFViewport;
    FSGInserts: TsgObjectList;
    FAppIDs: TsgCollection;
    FSegmentedFile: TsgSegmentedFile;
    function AddCreatedElem(const AName: string; const AClass: TsgDXFEntityClass;
      const AElements: TsgDWGCreatedElements; ADWGCode: Word;
      AHandle: UInt64 = cnstBadHandle; AOwnerHandle: UInt64 = cnstBadHandle): Integer;
    //For Objects EntMode must be 3!
    function AddCommonEntityData(const AHandle: UInt64; const AObjType: Word;
      const ABits: TsgDWGBits; const AReactors: TsgInt64List = nil; Entity: TsgDXFEntity = nil;
      EntMode: Byte = 3; PLineTypeFlags: PByte = nil; PPlotstyleFlags: PByte = nil;
      XDictionary: UInt64 = cnstBadHandle): Cardinal;
    procedure AddCommonTableItemFlags(ABits: TsgDWGBits;
      ATableItem: TsgDXFEntity; const AName: string;
      AXRefIndex: Integer; AFlags: PByte);
    function AddEED(const ABits: TsgDWGBits; const E: TsgDXFEntity;
      const AEEData: TsgCADExtendedData): Boolean;
    procedure AddEEDItems(const ABits: TsgDWGBits; const E: TsgDXFEntity);
    procedure AddHandlesUpdateAndFinish(const ABits: TsgDWGBits;
      const AHandles: array of TsgDWGHandle; const ObjHandle: UInt64;
      const ADWGCode: Word; AWriteCRC: Boolean = True);
    procedure AddHandlesData(const ABits: TsgDWGBits; const AHandles: array of TsgDWGHandle);

    procedure AddOwnerHandle(var AHandles: TsgDWGHandleArray; const AOwnerHandle: UInt64); overload;
    procedure AddOwnerHandle(var AHandles: TsgDWGHandleArray; const AOwnerHandle: TsgDWGHandle); overload;
    procedure AddResctorsHandle(var AHandles: TsgDWGHandleArray; const AReactors: TsgInt64List = nil);
    procedure AddXDictObjHandle(var AHandles: TsgDWGHandleArray; const XDictObjHandle: Uint64 = cnstBadHandle);

    procedure AddReactorsAndXDictHandles(var AHandles: TsgDWGHandleArray;
      const AReactors: TsgInt64List; const AXDictObjHandle: Uint64);

    procedure AddCommonHandleRefs(var AHandles: TsgDWGHandleArray;
      const AOwnerHandle: UInt64; const AReactors: TsgInt64List = nil;
      const AXDictionaryHandle: UInt64 = cnstBadHandle); overload;

    procedure AddCommonHandleRefs(var AHandles: TsgDWGHandleArray;
      const AOwnerHandle: TsgDWGHandle; const AReactors: TsgInt64List = nil;
      const AXDictionaryHandle: UInt64 = cnstBadHandle); overload;

    procedure AddHandleToArray(const AHandle: TsgDWGHandle;
      var AHandles: TsgDWGHandleArray);
    function AddItemToArray(const AHandle: UInt64; const AName: string;
      var AItems: TsgDWGLinkItems): Integer;
    procedure AddCommonEntityHandleData(const ObjHandle: UInt64; ABits: TsgDWGBits;
      AEntity: TsgDXFEntity; const AOwnerHandle: UInt64; DWGCode: Word; EntMode,
      LTypeFlags, PlotStyleFlags: Byte; ANeedLayer: Boolean; const APrevEntHandle,
      ANextEntHandle: UInt64; AOwner: TsgDXFEntity; AXDictionary: UInt64);
    function AddEEDDirrect(const ABits: TsgDWGBits; const AEEData: TsgCADExtendedData): Integer;
    procedure AddXRecordDirrect(const ABits: TsgDWGBits;
      const XRecords: TsgCADExtendedData);

    procedure CreateTablesRecord;
    procedure CreateDictionaryListByStrList(const DictionaryName: string;
      DWGCode: Word; AList: TStringList);
    function CheckEntHandle(AEntity: TsgDXFEntity; ADoRaise: Boolean = True): Boolean;
    function ConvertEEDToDWG(E: TsgDXFEntity; Source, Dest: TsgCADExtendedData): Boolean;
    procedure DoBeforeAddCommonEntData(ABits: TsgDWGBits; var Handle: UInt64;
      var E: TsgDXFEntity; var DWGCode: Word; OwnerDWGCode: Word;
      AOwner: TsgDXFEntity; var XDictionary: UInt64);
    procedure DoAfterAddEntHandlesData(const ObjHandle: UInt64; ABits: TsgDWGBits;
      AEntity: TsgDXFEntity; const AOwnerHandle: UInt64; DWGCode: Word;
      EntMode, LTypeFlags, PlotStyleFlags: Byte; const APrevEntHandle,
      ANextEntHandle: UInt64; AOwner: TsgDXFEntity;
      var Handles: TsgDWGHandleArray);

    procedure ClearScalesDictionary;
    procedure ExportCreatedTableRecords;
    procedure ExportViewPortEntityHeaders;
    procedure ExportSectionTables;
    procedure ExportTableAPPID;
    procedure ExportTableBLOCK_RECORD;
    procedure ExportTableSTYLE;
    procedure ExportTableLTYPE;
    procedure ExportTableLAYER;
    procedure ExportTableVPORT;
    procedure ExportTableDIMSTYLE;
    procedure ExportTableOBJECTS;
    procedure ExportSectionENTITIES;

    function ExportCADFill(CADFill: TsgCADFill; const Data: TsgDirectExportData): Boolean;
    function ExportLine(L: TsgDXFLine; ABits: TsgDWGBits): Boolean;
    function ExportRay(R: TsgDXFRay; ABits: TsgDWGBits): Boolean;
    function ExportSolid(S: TsgDXFSolid; ABits: TsgDWGBits): Boolean;
    function ExportText(T: TsgDXFText; ABits: TsgDWGBits): Boolean;
    function ExportOle2Frame(OLE2: TsgDXFOle2Frame; ExportData: TObject): Boolean;
    function ExportOle2FrameCreateOleGroup(OLE2: TsgDXFOle2Frame; ExportData: TObject): Boolean;
    function ExportPoint(P: TsgDXFPoint; ABits: TsgDWGBits): Boolean;
    function ExportCircle(C: TsgDXFCircle; ABits: TsgDWGBits): Boolean;
    function ExportPolyline(PLine: TsgDXFPolyline; DWGCode: Word; ABits: TsgDWGBits): Boolean;
    function ExportLWPolyline(LWPolyline: TsgDXFLWPolyline; ABits: TsgDWGBits): Boolean;
    function ExportSeqEnd(E: TsgDXFEntity; ABits: TsgDWGBits): Boolean;
    function ExportVertex(Owner: TsgDXFEntity; V: TsgDXFVertex; DWGCode: Word;
      ABits: TsgDWGBits): Boolean;
    function ExportSpline(Spline: TsgDXFSpline; ABits: TsgDWGBits): Boolean;
    function ExportFlatPoly(FlatPoly: TsgFlatPoly; const Data: TsgDirectExportData): Boolean;
    function ExportShape(S: TsgDXFShape; ABits: TsgDWGBits): Boolean;
    function ExportInsert(Insert: TsgDXFInsert; ABits: TsgDWGBits; EEData: TsgCADExtendedData): Boolean;
    function ExportMesh(M: TsgDXFMesh; ABits: TsgDWGBits): Boolean;
    function ExportMText(M: TsgDXFMText; ABits: TsgDWGBits): Boolean;
    function ExportTolerance(T: TsgDXFTolerance; ABits: TsgDWGBits): Boolean;
    function Export3DFace(F: TsgDXF3dFace; ABits: TsgDWGBits): Boolean;
    function ExportEllipse(E: TsgDXFEllipse; ABits: TsgDWGBits): Boolean;
    function ExportLeader(L: TsgDXFLeader; ABits: TsgDWGBits): Boolean;
    function ExportDimension(D: TsgDXFDimension; ABits: TsgDWGBits;
      DWGCode: Word): Boolean;
    function ExportHatch(H: TsgCADPolyPolygon; ABits: TsgDWGBits;
      DWGCode: Word): Boolean;
    function ExportViewPort(V: TsgDXFViewPort; ABits: TsgDWGBits): Boolean;
    function ExportWipeOut(W: TsgCADCustomRectangle; ABits: TObject): Boolean;
    function ExportWipeOutAsExternalFile(W: TsgCADCustomRectangle; AData: TObject): Boolean;
    function ExportACISEntity(AEnt: TsgBrepModAcis; ABits: TsgDWGBits): Boolean;

    {$IFDEF SG_BTI}
    function ExportPolyPolyline2D(Poly: TsgCADPolyPolyline2D; const AExportData: TsgDirectExportData): Boolean;
    {$ENDIF}
    function ExportMLine(M: TsgCADMLine; const ABits: TsgDWGBits;
      const AExportData: TsgDirectExportData): Boolean;
    function ExportUnknownObject(E: TsgDXFEntity; const AExportData: TsgDirectExportData): Boolean;

    procedure FinishObject(const ObjHandle: UInt64; const ABits: TsgDWGBits;
      const AObjSize: Cardinal; ADWGCode: Word;
      AHandleStreamBitSize: Cardinal = 0; AWriteCRC: Boolean = True);
    procedure ForceEntityHandle(AEntity: TsgDXFEntity);
    procedure ForceAppIDLinks;
    function GetSection(Index: Integer): TMemoryStream;
    function GetDWGCodePage: Word;
    function GetHandleByEntity(AEntity: TsgDXFEntity): UInt64;
    function GetBlockRecordHandle(const AName: string): UInt64;

    function GetEntityHandle(const AEntity: TsgDXFEntity; ADoNewHandle: Boolean = False;
      ADWGCode: Word = cntDWGObjCodeUNSUSED; const AName: string = ''; ADoRaise: Boolean = False;
      AFirst: Boolean = True): UInt64;
    function GetDictionaryHandle(const AName: string): UInt64;
    function GetDWGCodeByClass(const AClassName: string): Word;
    function GetRealDWGCode(const DWGCode: Word): Word;
    function GetEntHandleByIndex(Group: TsgDXFEntity; Index: Integer; AList: TsgEntitiesList;
      EntIndex: Integer; AFirst: Boolean): UInt64;
    function GetStyleHandle(Style: TsgDXFEntity; ADimStyle: Boolean): UInt64;
    function GetLineTypeHandle(LineType: TsgDXFLineType): UInt64;
    function GetAppInfoSectionIndex: Integer;
    function GetAuxHeaderSectionIndex: Integer;
    function GetClassesSectionIndex: Integer;
    function GetFileDepListSectionIndex: Integer;
    function GetHeaderVarsSectionIndex: Integer;
    function GetMeasurementSectionIndex: Integer;
    function GetObjFreeSpaceSectionIndex: Integer;
    function GetObjectMapSectionIndex: Integer;
    function GetRevHistorySectionIndex: Integer;
    function GetFileHeaderSectionIndex: Integer;
    function GetPreviewSectionIndex: Integer;
    function GetSummaryInfoSectionIndex: Integer;
    function GetObjectsSectionIndex: Integer;
    function GetSecondHeaderSectionIndex: Integer;
    function GetPrototypeSectionIndex: Integer;
    function GetDefVal(const Value, DefValue: Double): Double;
    function GetPaperSpaceBlockHandle: UInt64;
    function GetModelSpaceBlockHandle: UInt64;
    function GetDefChildTableItemsCount(const AHandles: TsgDWGHandleArray): Integer;

    function IsEntityHasNotEED(const Entity: TsgDXFEntity): Boolean;

    procedure OnWriteDimStyleControlObject(ABits: TsgDWGBits);//DimStyleCtrlObject needs additional WriteRC(0)

    function ProcAppID(const AOwnerHandle: UInt64; const AppInfo: string;
      E: TsgDXFEntity; var AHandles: TsgDWGHandleArray): UInt64;
    procedure ProcAppIDControl(const AHandle: UInt64; const AHandles: TsgDWGHandleArray);
    procedure ProcBlockControl(BlockHandles: TsgInt64List);
    procedure ProcBlock(ABlock: TsgDXFBlock; const AName: string;
      Data: Pointer; APaperSpaceIndex: Integer = -1);
    procedure ProcBlockEnt(ABlock: TsgDXFBlock; const AName: string;
      Data: Pointer; ACurrPaperSpaceIndex: Integer = -1);
    procedure ProcStyle(const AOwner: UInt64; const AStyle: TsgDXFStyle);
    procedure ProcStyleControl(const AHandle: UInt64; const StylesHandles: TsgDWGHandleArray);
    procedure ProcControlObject(const AHandle: UInt64; ACode: Word; ANumentries: Cardinal;
      const AHandles: array of TsgDWGHandle; ReadEvent: TsgDWGReadEvent = nil);
    procedure ProcLType(AOwnerHandle: UInt64; ALType: TsgDXFLineType);
    procedure ProcLTypeControlObject(const AHandle: UInt64; const ALTypesHandles: array of TsgDWGHandle);
    procedure ProcLayer(const AOwnerHandle: UInt64; ALayer: TsgDXFLayer;
      var ProcHandles: TsgDWGHandleArray; var ZeroLayerIndex: Integer);
    procedure ProcLayerControlObject(const AHandle: UInt64;
      const ProcLayerHandles: TsgDWGHandleArray);
    procedure ProcVPort(const AOwnerHandle: UInt64; AVPort: TsgDXFVPort;
      var ProcVPortHandles: TsgDWGHandleArray);
    procedure ProcVPortControlObject(const AHandle: UInt64;
      const ProcVPortHandles: TsgDWGHandleArray);
    procedure ProcDimStyle(const AOwnerHandle: UInt64;
      ADimStyle: TsgDXFDimensionStyle; var AProcHandles: TsgDWGHandleArray);
    procedure ProcDimStylesControlObject(const AHandle: UInt64;
      const ADimStyleHandles: TsgDWGHandleArray);
    procedure ProcDictionary(const Handle, OwnerHandle: UInt64; var Items: TsgDWGLinkItems;
      const AHardOwnerFlag: Byte = 0);
    procedure ProcRasterVariables(const ARasterVariables: TsgCADRasterVariables;
      const OwnerHandle: UInt64);
    procedure ProcScale(const Handle, OwnerHandle: UInt64; const Index: Integer;
      const Name: string);
    procedure ProcMLineStyle(const AStyle: TsgMLineStyle; const OwnerHandle: UInt64);
    procedure ProcWipeoutVar(const Handle, OwnerHandle: UInt64);
    procedure ProcVariableDictionary(const Handle, OwnerHandle: UInt64; const Index: Integer;
      const Name: string);
    procedure ProcDWGProps(const Handle, OwnerHandle: UInt64);
    procedure ProcLayout(ALayout: TsgDXFLayout; const Handle, OwnerHandle: UInt64;
      AIndex: Integer; const AName: string);
    procedure ProcViewPortEntHeader(const AIndex: Integer);
    procedure ProcImageDef(AImageDef: TsgDXFImageDef);
    procedure ProcSpatialFilter(const AFilter: TsgCADSpatialFilter);
    procedure ProcXRecords;
    procedure ProcXRecord(const AHandle: UInt64;
      const AData: TsgCADExtendedData; AOwnerDictionary: UInt64);
    procedure ProcXData;

    procedure SaveObjectToStream(const AHandle: UInt64; const ABits: TsgDWGBits;
      DWGCode: Word);
    procedure WriteByteToStream(const Stream: TStream; const Byte: Byte;
      const Count: Integer);
    function GetXrefHandle(Entity: TsgDXFEntity): TsgDWGHandle;
    procedure MakeBlockAlias(ABlock: TsgDXFBlock; const AName: string;
      Data: Pointer; ACurrPaperSpaceIndex: Integer = -1);
    procedure __AddDWGSGInserts(const APaperSpaceBlock: TsgDXFBlock;
      const AInserts: TsgObjectList);
    function GetAppIDs: TsgCollection;
    property AppIDs: TsgCollection read GetAppIDs;
  protected
    FSaveBTIDataDwg: Boolean;
    procedure BeforeDestroy(var AClearing: Boolean); override;
    procedure Clear; override;
    procedure CreateSections;
    function CorrectLineWeight(const ALineWeight: Double;
      ANeedToConvert: Boolean): Integer; override;
    procedure DoExport(const E: TsgDXFEntity; const Data: PsgDirectExportData = nil); override;
    procedure ExportSectionAppInfo;
    procedure ExportSectionAuxHeader(const HandsSeed: UInt64;
      const ADateCreate, ADateUpdate: TDateTime);
    procedure ExportSectionClasses(const APaddingSize: Integer); virtual;
    procedure ExportSectionFileDepList;
    procedure ExportSectionHeaderVars(const AHandSeed: UInt64;
      const ADateCreate, ADateUpdate: TDateTime);
    procedure ExportSectionMeasurement(const Measurement: Word = 1); overload;//0 = English, 1 = Metric
    procedure ExportSectionMeasurement(const Measurement: Boolean); overload;//False = English, True = Metric
    procedure ExportSectionObjects;
    procedure ExportSectionObjMap;
    procedure ExportSectionObjFreeSpace(const ADateUpdate: TDateTime;
      const ObjCount, ObjOffset: Cardinal);
    procedure ExportSectionRevHistory;
    procedure ExportSectionPreview(const ImageOffset: Cardinal; var HeaderOffsetPos, HeaderOffsetValue,
      BmpOffsetPos, BmpOffsetPosValue: Cardinal);
    procedure ExportSectionSecondHeader(const AHandSeed: UInt64;
      Offset: Cardinal; const Locators: TsgDWGR15Locators);
    procedure ExportSectionSummaryInfo(const ADateCreate, ADateUpdate: TDateTime);
    procedure ExportSectionPrototype;
    procedure FreeSections;

    function GetArrowBlockHandle(const Value: TsgDimArrowType): UInt64;
    procedure GetCreateAndUpdateDateTime(var ADateCreate, ADateUpdate: TDateTime);
    function GetModelAndPaperSpaceBlockHandle(const AIndex: Integer): UInt64;

    function GetPreviewImageHeigth: Integer;
    function GetPreviewImageWidth: Integer;
    function GetPreviewImageRect: TRect;
    function GetLocatorsCount: Integer; virtual;
    function GetSectionsCount: Integer; virtual;

    function GetMLineStyleHandle: UInt64;
    function GetBlockControlObjectHandle: UInt64;
    function GetLayerControlObjectHandle: UInt64;
    function GetStyleControlObjectHandle: UInt64;
    function GetLineTypeControlObjectHandle: UInt64;
    function GetViewControlObjectHandle: UInt64;
    function GetVPortControlObjectHandle: UInt64;
    function GetUCSontrolObjectHandle: UInt64;
    function GetAppIDControlObjectHandle: UInt64;
    function GetDimStyleControlObjectHandle: UInt64;
    function GetViewPortEntityHeaderControlObjectHandle: UInt64;
    function GetCurrentViewportEntityHeaderHandle: UInt64;
    function GetLastActiveViewportHandle(ALayout: TsgDXFLayout): UInt64;

    function GetDictionaryAcadGroupHandle: UInt64;
    function GetDictionaryAcadImageDict: UInt64;
    function GetDictionaryAcadMaterialsHandle: UInt64;
    function GetDictionaryAcadMLineStyleHandle: UInt64;
    function GetDictionaryNamedObjectHandle: UInt64;
    function GetDictionaryLayoutsHandle: UInt64;
    function GetDictionaryPlotSettingHandle: UInt64;
    function GetDictionaryPlotStyleHandle: UInt64;
    function GetPaperSpaceLayoutHandle(PIndex: PInteger = nil): UInt64;

    function GetFingerPrintGUID: string;
    function GetVersionGUID: string;

    function Get64bitof70Group(Flags: Word): Byte; //see DWG documentation
    function GetXdep(const AName: string): Byte; //see DWG documentation
    function GetBitSizePos(DWGCode: Word): Cardinal; virtual;
    function GetActiveLayerHandle(AGetZeroLayer: Boolean = False): UInt64;
    function GetObjectsFileOffset: Cardinal;
    function GetSecondHeaderOffset: Cardinal;
    function GetPreViewImageOffset: Cardinal;
    function GetHeaderVarsOffset: Cardinal;
    function GetClassesOffset: Cardinal;
    function GetObjMapOffset: Cardinal;
    function GetObjFreeSpaceOffset: Cardinal;
    function GetTemplateOffset: Cardinal;
    function GetAuxHeaderOffset: Cardinal;
    function GetExportExceptionClass: ExceptClass; override;
    function GetNextEntHandle(Group: TsgDXFEntity; Index: Integer;
      AList: TsgEntitiesList = nil): UInt64; override;
    function GetPrevEntHandle(Group: TsgDXFEntity; Index: Integer;
      AList: TsgEntitiesList = nil): UInt64; override;
    function GetExtensionDictionary(AEntity: TsgDXFEntity; const AName: string): TsgDXFEntity;

    function IsPaperSpace(const AName: string; ABlock: TsgDXFBlock): Boolean; overload;
    function IsPaperSpace(ABlock: TsgDXFBlock): Boolean; overload;

    procedure PrepareDrawingDatabase; override;
    function PrepareTextForDWG(const Text: string): string;
    procedure ProcessExport(Stream: TStream); override;

    procedure SaveSectionToStream(const ASectNum: Integer;
      const ABits: TsgDWGBits; const ABeginSentinel, AEndSentinel: array of Byte;
      ANeedToClear: Boolean = True);
    procedure SetVersion(const Value: TsgDWGVersion); override;
    function WriteDateTimeAsJulian(const ADate: TDateTime; const ABits: TsgDWGBits;
      AUseRL: Boolean; PJDate: PSGCardinal = nil; PMilSec: PSGCardinal = nil;
      AWriteMilSec: Boolean = True): Integer;
    function WriteEntityHandle(const ABits: TsgDWGBits; const Entity: TsgDXFEntity;
      const ACode: Byte; AHandle: Boolean = True): Integer;
    property ModelSpaceBlockHandle: UInt64 read GetModelSpaceBlockHandle;
    property PaperSpaceBlockHandle: UInt64 read GetPaperSpaceBlockHandle;
    property Sections[Index: Integer]: TMemoryStream read GetSection;
    property XrefHandle[Entity: TsgDXFEntity]: TsgDWGHandle read GetXrefHandle;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    class function ExportType: TsgExportFormat; override;
    property DWGCodePage: Word read GetDWGCodePage;
    property MaintenanceVersion: Byte read FMaintenanceVersion;
  end;

implementation

const
  cntDWGR15FileHeaderSize = SizeOf(TdwgFileHeader) + //File header variables
    cntDWGR15SectionsCount * SizeOf(TdwgLocator)   + //Locator records count is 6
    SizeOf(Word)                                   + //CRC
    SizeOf(TsgDWGSentinel);                          //End Sentinel
  cntSizeTempValue = $FFFFFFFF;
  cntOffsetTempValue = cntSizeTempValue;
  cntPaddingSize = $80 * SizeOf(Cardinal);
  cntDWGObjCodeTEXT = 1;
  cntDWGObjCodeATTRIB = 2;
  cntDWGObjCodeATTDEF = 3;
  cntDWGObjCodeBLOCK = 4;
  cntDWGObjCodeENDBLK = 5;
  cntDWGObjCodeSEQEND = 6;
  cntDWGObjCodeINSERT = 7;
  cntDWGObjCodeMINSERT1 = 8;
  cntDWGObjCodeMINSERT2 = 9;
  cntDWGObjCodeVERTEX2D = $0A;
  cntDWGObjCodeVERTEX3D = $0B;
  cntDWGObjCodeVERTEXMESH = $0C;
  cntDWGObjCodeVERTEXPFACE = $0D;
  cntDWGObjCodeVERTEXPFACEFACE = $0E;
  cntDWGObjCodePOLYLINE2D = $0F;
  cntDWGObjCodePOLYLINE3D = $10;
  cntDWGObjCodeARC = $11;
  cntDWGObjCodeCIRCLE = $12;
  cntDWGObjCodeLINE = $13;
  cntDWGObjCodeDIMENSIONORDINATE = $14;
  cntDWGObjCodeDIMENSIONLINEAR = $15;
  cntDWGObjCodeDIMENSIONALIGNED = $16;
  cntDWGObjCodeDIMENSIONANG3Pt = $17;
  cntDWGObjCodeDIMENSIONANG2Ln = $18;
  cntDWGObjCodeDIMENSIONRADIUS = $19;
  cntDWGObjCodeDIMENSIONDIAMETER = $1A;
  cntDWGObjCodePOINT = $1B;
  cntDWGObjCode3DFACE = $1C;
  cntDWGObjCodePOLYLINEPFACE = $1D;
  cntDWGObjCodePOLYLINEMESH = $1E;
  cntDWGObjCodeSOLID = $1F;
  cntDWGObjCodeTRACE = $20;
  cntDWGObjCodeSHAPE = $21;
  cntDWGObjCodeVIEWPORT = $22;
  cntDWGObjCodeELLIPSE = $23;
  cntDWGObjCodeSPLINE = $24;
  cntDWGObjCodeREGION = $25;
  cntDWGObjCode3DSOLID = $26;
  cntDWGObjCodeBODY = $27;
  cntDWGObjCodeSURFACE = $58;
  cntDWGObjCodeRAY = $28;
  cntDWGObjCodeXLINE = $29;
  cntDWGObjCodeDICTIONARY1 = $2A;
  cntDWGObjCodeDICTIONARY2 = $2B;
  cntDWGObjCodeMTEXT = $2C;
  cntDWGObjCodeLEADER = $2D;
  cntDWGObjCodeTOLERANCE = $2E;
  cntDWGObjCodeMLINE = $2F;
  cntDWGObjCodeBLOCKCONTROLOBJ = $30;
  cntDWGObjCodeBLOCKHEADER = $31;
  cntDWGObjCodeLAYERCONTROLOBJ = $32;
  cntDWGObjCodeLAYER = $33;
  cntDWGObjCodeSTYLECONTROLOBJ = $34;
  cntDWGObjCodeSTYLE1 = $35;
  cntDWGObjCodeSTYLE2 = $36;
  cntDWGObjCodeSTYLE3 = $37;
  cntDWGObjCodeLTYPECONTROLOBJ = $38;
  cntDWGObjCodeLTYPE1 = $39;
  cntDWGObjCodeLTYPE2 = $3A;
  cntDWGObjCodeLTYPE3 = $3B;
  cntDWGObjCodeVIEWCONTROLOBJ = $3C;
  cntDWGObjCodeVIEW = $3D;
  cntDWGObjCodeUCSCONTROLOBJ = $3E;
  cntDWGObjCodeUCS = $3F;
  cntDWGObjCodeVPORTCONTROLOBJ = $40;
  cntDWGObjCodeVPORT = $41;
  cntDWGObjCodeAPPIDCONTROLOBJ = $42;
  cntDWGObjCodeAPPID = $43;
  cntDWGObjCodeDIMSTYLECONTROLOBJ = $44;
  cntDWGObjCodeDIMSTYLE = $45;
  cntDWGObjCodeVPENTHDRCTRLOBJ = $46;
  cntDWGObjCodeVPENTHDR = $47;
  cntDWGObjCodeGROUP = $48;
  cntDWGObjCodeMLINESTYLE = $49;
  cntDWGObjCodeOLE2FRAME = $4A;
  cntDWGObjCodeDUMMY = $4B;
  cntDWGObjCodeLONGTRANSACTION = $4C;
  cntDWGObjCodeLWPOLYLINE = $4D;
  cntDWGObjCodeHATCH = $4E;
  cntDWGObjCodeXRECORD = $4F;
  cntDWGObjCodeACDBPLACEHOLDER = $50;
  cntDWGObjCodeVBA_PROJECT = $51;
  cntDWGObjCodeLAYOUT = $52;

  cntDWGObjCodeUSER = 3000; //only for internal use, codes will not be saved into DWG file
  cntDWGObjCodeMPOLYGON = cntDWGObjCodeUSER + 1;
  cntDWGObjCodeWIPEOUT = cntDWGObjCodeUSER + 2;
  cntDWGObjCodeIMAGE = cntDWGObjCodeUSER + 3;
  cntDWGObjCodeIMAGEBYFILE = cntDWGObjCodeUSER + 4;
  cntDWGObjCodeIMAGEDEF = cntDWGObjCodeUSER + 5;
  cntDWGObjCodeFLATHATCH = cntDWGObjCodeUSER + 6;
  cntDWGObjCodePROXY = cntDWGObjCodeUSER + 7;
  cntDWGObjCodePOLYPOLYLINE2D = cntDWGObjCodeUSER + 8;
  cntDWGObjCodeRASTERVARIABLES = cntDWGObjCodeUSER + 9;
  cntDWGObjCodeMESH = cntDWGObjCodeUSER + 10;
  cntDWGObjCodeARC_DIMENSION = cntDWGObjCodeUSER + 11;
  cntDWGObjCodeUNKNOWNOBJ = cntDWGObjCodeUSER + 12;

  cntEmptyDictionary = ' ';
type
  PsgDWGScale = ^TsgDWGScale;
  TsgDWGScale = record
    ScaleParam1: Double;
    ScaleParam2: Double;
    ScaleParam3: Byte;
  end;

  TsgCADImageEx = class(TsgCADImage);
  TsgDXFEntityEx = class(TsgDXFEntity);
  Tsg2DPolylineEx = class(Tsg2DPolyline);
  Tsg2DSplineEx = class(Tsg2DSpline);
  TsgDXFBlockEx = class(TsgDXFBlock);
  TsgDXFInsertEx = class(TsgDXFInsert);
  TsgDXFDimensionEx = class(TsgDXFDimension);
  TsgDXFTextEx = class(TsgDXFText);
  TsgDXFMTextEx = class(TsgDXFMText);
  TsgDXFToleranceEx = class(TsgDXFTolerance);
  TsgDXFVertexEx = class(TsgDXFVertex);
{$IFDEF SG_BTI}
  TsgAreaFigureAccess = class(TsgAreaFigure);
  TsgExtendedBlockDataEx = class(TsgExtendedBlockData);
{$ENDIF}
  TsgDXFTableAccess = class(TsgDXFTable);
  TsgDXFImageEntAccess = class(TsgDXFImageEnt);
  TsgBitmapAccess = class(TsgBitmap);
  TsgAcadTableCellAccess = class(TsgAcadTableCell);
  TsgMLineStyleAccess = class(TsgMLineStyle);
  TsgDXFPolylineAccess = class(TsgDXFPolyline);
  TsgDXF3dFaceAccess = class(TsgDXF3dFace);
  TsgDXFBlockRecordAccess = class(TsgDXFBlockRecord);
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgCADMultiLeaderAccess = class(TsgCADMultiLeader);
  TsgDXFViewportAccess = class(TsgDXFViewport);
  TsgBrepModEntityAccess = class(TsgBrepModEntity);

type

  TsgDWG2010StreamPages = array of TsgDWGR18StreamPage;
  TsgDWGR18SysPageItems = array of TsgDWGR18SysPageItem;

  TdwgR18SectionMapBuilder = class;

  TsgDWGR18PageBuilder = class
  private
    FSectionStream: TStream;
    FSectionData: Int64;
    FDest: TCustomMemoryStream;
    FSectionName: string;
    FDataSize: UInt64;
    FPages: TsgDWG2010StreamPages;
    FPrevPageID: Cardinal;
    // local
    FSrcPageBuff: PByte;
    FPageHeader: PsgDWG2004DataPageHeader;
    FPackedSize: Integer;
    FPageData: Pointer;
    FPackedAlignedSize: Integer;
    FDestStartPos: Int64;
    FOnPageAdd: TNotifyEvent;
    FCurrPageSize: Integer;
    FStartOffset: Int64;
    FSectionID: Integer;
    function GetPageSize: Integer;
    function GetCompressedProp: Integer;
    function GetEncrypted: Integer;
    function GetMapOrderID: Integer;
  protected
    procedure AddSysPage; virtual;
    procedure StorePage; virtual;
    function Compress: Integer; virtual;
    property DestStartPos: Int64 read FDestStartPos;
    property PrevPageID: Cardinal read FPrevPageID write FPrevPageID;
    property CurrPageSize: Integer read FCurrPageSize;
  public
    constructor Create(ASectionStream: TStream; ADest: TCustomMemoryStream;
      const ASectionName: string); virtual;
    class function GetCompressed(const ASectionName: string): Integer;
    class function SectionData(const ASectionName: string): Int64;
    class function GetMapID(const ASectionName: string): TsgNativeUInt;
    property Encrypted: Integer read GetEncrypted;
    property Compressed: Integer read GetCompressedProp;
    property PageSize: Integer read GetPageSize;
    procedure Build; virtual;
    property MapOrderID: Integer read GetMapOrderID;
    property SectionName: string read FSectionName;
    property DataSize: UInt64 read FDataSize;
    property Pages: TsgDWG2010StreamPages read FPages;
    property OnPageAdd: TNotifyEvent read FOnPageAdd write FOnPageAdd;
    property SectionID: Integer read FSectionID;
  end;

  TsgDWGR18PageBuilderCompressed = class(TsgDWGR18PageBuilder)
  private
    FCompressor: TsgDWGCompressor;
  protected
    function Compress: Integer; override;
    procedure StorePage; override;
  public
    constructor Create(ASectionStream: TStream; ADest: TCustomMemoryStream;
      const ASectionName: string); override;
    destructor Destroy; override;
  end;

  TdwgR18SectionMapBuilder = class
  private
    FSections: TStringList;
    FDest: TCustomMemoryStream;
    FSectionMapOffest: UInt64;
    FSectionPagesMapOffest: UInt64;
    FLastPageEndOffset: UInt64;
    FPageItems: TsgDWGR18SysPageItems;
    FSectionMapID: Cardinal;
    FSectionPageMapID: Cardinal;
    FMapOrderIDs: TsgIntegerList;
    procedure PageAdded(Sender: TObject);
    function AlignPage(const ASize: Integer): Cardinal;
    function CalcCheckSum(var ASysPageHeader: TsgDWG2010SysPageHeader;
      AData: Pointer): Cardinal;
    function MakeSysPage(const APageType: Cardinal;
      const ADecompSize: Cardinal; AData: Pointer): UInt64;
    function GetPageBuilder(Index: Integer): TsgDWGR18PageBuilder;
  public
    constructor Create(ADest: TCustomMemoryStream); virtual;
    destructor Destroy; override;
    function Add(ASectionStream: TCustomMemoryStream; const ASectionName: string): Integer;
    function Find(AMapOrderID: Integer; var I: Integer): Boolean;
    procedure MakeSysSections;
    procedure Build;
    property PageBuilder[Index: Integer]: TsgDWGR18PageBuilder read GetPageBuilder; default;
    property PageItems: TsgDWGR18SysPageItems read FPageItems;
  end;

  TsgHandleNameLinks = class(TsgCollection)
  private
    function GetName(Index: Integer): string;
    function GetNameByHandle(Handle: UInt64): string;
    function GetHandleByName(const Name: string): UInt64;
  protected
    procedure SetSorted(const Value: Boolean); override;
    function CmpEntHandles(const Item1, Item2: Pointer): Integer;
    function CmpEntNames(const Item1, Item2: Pointer): Integer;
    class function IsEqualMethods(const AProc1, AProc2: TsgObjProcCompare): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear(ClearCapacity: Boolean = False); override;
    procedure Delete(const Index: Integer; DelCount: Integer); overload; override;
    function Find(const Handle: UInt64; var Index: Integer): Boolean; overload;
    function Find(const Name: string; var Index: Integer): Boolean; overload;
    function AddItem(Handle: UInt64; const Name: string): Integer;
    procedure SortByName;
    procedure SortByHandle;
    property Name[Index: Integer]: string read GetName;
    property NameByHandle[Handle: UInt64]: string read GetNameByHandle;
    property HandleByName[const Name: string]: UInt64 read GetHandleByName;
  end;

  TsgDWGSectionSizesWriter = class
  private
    FBits: TsgDWGBits;
    FHandles: ^TsgDWGHandleArray;
  public
    constructor Create(ABits: TsgDWGBits; AHandles: PPointer = nil);
    procedure BeginSection; virtual;
    procedure EndSection; virtual;
  end;

  TsgDWGSectionSizesWriter64 = class(TsgDWGSectionSizesWriter)
  public
    procedure BeginSection; override;
    procedure EndSection; override;
  end;

var
  DWGPagesR18: TsgStringList = nil;

procedure InitDWGPages;

  function MakeProp(AMapOrderID: Byte; ACompressed: Byte; AEncrypted: Byte;
    APageSize: Integer): Int64;
  begin
    Result := APageSize;
    Result := Result or ACompressed shl 16;
    Result := Result or AEncrypted  shl 18;
    Result := Result or AMapOrderID shl 26;
  end;

begin
  DWGPagesR18 := TsgStringList.Create;
  DWGPagesR18.CaseSensitive := True;
  DWGPagesR18.Duplicates := dupIgnore;
  DWGPagesR18.AddObject('',                     TsgObjectWithField.CreateInt64(MakeProp(0,  2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectSecurity,      TsgObjectWithField.CreateInt64(MakeProp(1,  1, 0, $400)));
  DWGPagesR18.AddObject(sAcadSectFileDepList,   TsgObjectWithField.CreateInt64(MakeProp(2,  1, 2, $80)));
  DWGPagesR18.AddObject(sAcadSectVBAProject,    TsgObjectWithField.CreateInt64(MakeProp(3,  1, 0, $80)));
  DWGPagesR18.AddObject(sAcadSectAppInfo,       TsgObjectWithField.CreateInt64(MakeProp(4,  1, 0, $80)));
  DWGPagesR18.AddObject(sAcadSectPreview,       TsgObjectWithField.CreateInt64(MakeProp(5,  1, 0, $80)));  //??
  DWGPagesR18.AddObject(sAcadSectSummaryInfo,   TsgObjectWithField.CreateInt64(MakeProp(6,  1, 0, $100)));
  DWGPagesR18.AddObject(sAcadSectRevHistory,    TsgObjectWithField.CreateInt64(MakeProp(7,  2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectObjects,       TsgObjectWithField.CreateInt64(MakeProp(8,  2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectObjFreeSpace,  TsgObjectWithField.CreateInt64(MakeProp(9,  2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectTemplate,      TsgObjectWithField.CreateInt64(MakeProp(10, 2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectHandles,       TsgObjectWithField.CreateInt64(MakeProp(11, 2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectClasses,       TsgObjectWithField.CreateInt64(MakeProp(12, 2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectAuxHeader,     TsgObjectWithField.CreateInt64(MakeProp(13, 2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectHeader,        TsgObjectWithField.CreateInt64(MakeProp(14, 2, 0, $7400)));
  DWGPagesR18.AddObject(sAcadSectSignature,     TsgObjectWithField.CreateInt64(MakeProp(15, 2, 0, $7400))); //??
  DWGPagesR18.AddObject(sAcadSectPrototype,     TsgObjectWithField.CreateInt64(MakeProp(16, 2, 0, $7400))); //??

//  DWGPagesR18.AddObject('DataSectionMap',       MakeProp(16, True,  0, $7400)); // internal
//  DWGPagesR18.AddObject('SectionPageMap',       MakeProp(17, True,  0, $7400)); // internal

  DWGPagesR18.Sorted := True;
end;

function CreateDWGLinkItem(AHandle: UInt64; const AName: string): TsgDWGLinkItem;
begin
  Result.Handle := AHandle;
  Result.Name := AName;
end;

function CastEntToID(E: TsgDXFEntity): UInt64;
begin
  if Assigned(E) then
    Result := E.Handle
  else
    Result := cnstBadHandle;
end;

function CastEntToName(E: TsgDXFEntity; const ADefault: string): string;
begin
  if Assigned(E) then
    Result := E.Name
  else
    Result := ADefault;
end;

function GetDWGTrasparency(const AObjType: Integer; AEntity: TsgDXFEntity): Integer;
begin
  Result := 0;
  if ((AObjType = cntDWGObjCodeHATCH) or (AObjType = cntDWGObjCodeMPOLYGON)) and
     (AEntity is TsgCADCurvePolygon) then
  begin
    if TsgCADCurvePolygon(AEntity).Transparency = fTransparencyByBlock then
      Result := $01000000
    else
      if TsgCADCurvePolygon(AEntity).Transparency > 0 then
      begin
        Result := Ceil(2.55 * TsgCADCurvePolygon(AEntity).Transparency);
        if Result < 0 then
          Result := 0;
        if Result > 255 then
          Result := 255;
        Result := 255 - Result;
        Result := $02000000 or Result;
      end;
  end;
end;

procedure DoAddHandlesData(const ABits: TsgDWGBits;
  const AHandles: array of TsgDWGHandle);
var
  I: Integer;
begin
  for I := Low(AHandles) to High(AHandles) do
    ABits.WriteHandle(AHandles[I].Handle, AHandles[I].Code);
end;

{ TsgDWGSectionSizesWriter }

constructor TsgDWGSectionSizesWriter.Create(ABits: TsgDWGBits;
  AHandles: PPointer = nil);
begin
  FBits := ABits;
  PPointer(FHandles) := AHandles;
end;

procedure TsgDWGSectionSizesWriter.BeginSection;
begin
  FBits.WriteRL(cntSizeTempValue);// size
  if FBits.Version >= acR2007 then
    FBits.WriteRL(cntSizeTempValue);// data+strs
end;

procedure TsgDWGSectionSizesWriter.EndSection;
begin
  FBits.Complete;
  if FHandles <> nil then
    DoAddHandlesData(FBits, FHandles^);
  FBits.UpdateRL(0, FBits.Size - 4);
end;

{ TsgDWGSectionSizesWriter64 }

procedure TsgDWGSectionSizesWriter64.BeginSection;
begin
  FBits.WriteRL(cntSizeTempValue);// size
  FBits.WriteRL(cntSizeTempValue);// high 64bit size
  if FBits.Version >= acR2007 then
    FBits.WriteRL(cntSizeTempValue); // data+strs
end;

procedure TsgDWGSectionSizesWriter64.EndSection;
var
  vOffset, vHandleStreamBitPos: Cardinal;
  vSize64: UInt64;
begin
  FBits.Complete;
  vHandleStreamBitPos := FBits.Position;
  if FHandles <> nil then
    DoAddHandlesData(FBits, FHandles^);
  vSize64 := FBits.Size - 8;
  vOffset := 0;
  FBits.UpdateRL(vOffset, vSize64);
  Inc(vOffset, 32);
  FBits.UpdateRL(vOffset, Cardinal(vSize64 shr 32));
  Inc(vOffset, 32);
  if FBits.Version >= acR2007 then
    FBits.UpdateRL(vOffset, vHandleStreamBitPos - vOffset);
end;

{ TsgCADtoDWG }

function TsgCADtoDWG.AddCommonEntityData(const AHandle: UInt64; const AObjType: Word;
  const ABits: TsgDWGBits; const AReactors: TsgInt64List = nil; Entity: TsgDXFEntity = nil;
  EntMode: Byte = 3; PLineTypeFlags: PByte = nil; PPlotstyleFlags: PByte = nil;
  XDictionary: UInt64 = cnstBadHandle): Cardinal;
var
  vEntName: string;
  vFlags: Byte;
begin
  Result := 0;
  if AObjType = 0 then Exit;
  ABits.WriteOT(GetRealDWGCode(AObjType));
  Result := ABits.Position;

  case Version of
    acR2000..acR2007{,acR2010}:
      ABits.WriteRL(cntSizeTempValue); // in bytes; data+strs size in bits for R21+
  end;

  ABits.WriteEntHandle(AHandle, cntDWGObjHandleType0);

  if Assigned(Entity) and (not IsEntityHasNotEED(Entity)) then
    AddEEDItems(ABits, Entity)
  else
    ABits.WriteBS(0);

  if EntMode <> 3 then //is entity
  begin
    ABits.WriteBit(0); //proxy graphic
    ABits.WriteBB(EntMode); //entmode
  end;

  if Assigned(AReactors) then
    ABits.WriteBL(AReactors.Count) //Reactors count
  else
    ABits.WriteBL(0);

  if EntMode = 3 then
  begin
    case Version of
      acR2004..DWGVersionHigh:
        ABits.WriteBit(XDictionary = cnstBadHandle)//We write 1 (no XDictionary) If 1, no XDictionary handle is stored for this object, otherwise XDictionary handle is stored as in R2000 and earlier.
    end;
  end;

  if EntMode = 3 then
    if Version >= acR2013 then
      ABits.WriteBit(0); //Indicates whether the object has associated binary data in the data store section

  if EntMode <> 3 then //is entity
  begin
    //Nolinks
    if Version >= acR2004 then
      ABits.WriteBit(Ord(XDictionary = cnstBadHandle)) //For R2004+ this always has value 1 (links are not used)
    else
      ABits.WriteBit(0);

    if Version >= acR2013 then
      ABits.WriteBit((AObjType = cntDWGObjCode3DSOLID) or
        (AObjType = cntDWGObjCodeREGION) or
        (AObjType = cntDWGObjCodeBODY)); //Indicates whether the object has associated binary data in the data store section

    if Entity <> nil then
    begin
      ABits.WriteENC(Entity.ColorCAD, GetDWGTrasparency(AObjType, Entity)); //color
      ABits.WriteBD(Entity.LineTypeScale); // Ltype scale
    end
    else
    begin
      ABits.WriteENC(cnstColorCADByLayer, 0);
      ABits.WriteBD(1);
    end;
    vFlags := 0;
    if (Entity is TsgDXFPenLine) or (Entity is TsgDXFInsert) then
    begin
      if TsgDXFPenLine(Entity).LineType <> nil then
        vEntName := TsgDXFPenLine(Entity).LineType.Name;
      if vEntName <> '' then
      begin
        if SameText(vEntName, sByBlock) then
          vFlags := 1
        else
          if SameText(vEntName, sContinuous) then
            vFlags := 2
          else
            if not SameText(vEntName, sByLayer) then
              vFlags := 3;
      end;
    end;
    ABits.WriteBB(vFlags);//Ltype flags
    if PLineTypeFlags <> nil then
      PLineTypeFlags^ := vFlags;
    ABits.WriteBB(0);//Plotstyle flags
    if PPlotstyleFlags <> nil then
      PPlotstyleFlags^ := 0;
    if Version >= acR2007 then
    begin
      ABits.WriteBB(0); // Material flags
      ABits.WriteRC(0); // Shadow flags
      if Version >= acR2010 then
      begin
        ABits.WriteBit(0); // Has full visual style
        ABits.WriteBit(0); // Has face visual style
        ABits.WriteBit(0); // Has edge visual style
      end;
    end;
    if Entity <> nil then
    begin
      ABits.WriteBS(BoolToBit(not Entity.Visibility));//Invisibility
      ABits.WriteRC(CorrectLineWeight(Entity.LineWeight, True)); //Lineweight
    end
    else
    begin
      ABits.WriteBS(0);
      ABits.WriteRC(29);
    end;
  end;
end;

procedure TsgCADtoDWG.AddCommonEntityHandleData(const ObjHandle: UInt64;
  ABits: TsgDWGBits; AEntity: TsgDXFEntity; const AOwnerHandle: UInt64; DWGCode: Word;
  EntMode, LTypeFlags, PlotStyleFlags: Byte; ANeedLayer: Boolean; const APrevEntHandle,
  ANextEntHandle: UInt64; AOwner: TsgDXFEntity; AXDictionary: UInt64);
var
  vHandles: TsgDWGHandleArray;
  vEnt: TsgDXFEntity;

  procedure CheckNeedLayer;
  begin
    if ANeedLayer then
    begin
      if AOwner = nil then
        AddHandleToArray(CreateDWGHandle(GetActiveLayerHandle(False), cntDWGObjHandleHardPointer), vHandles)
      else
        AddHandleToArray(CreateDWGHandle(GetEntityHandle(AOwner.Layer), cntDWGObjHandleHardPointer), vHandles)
    end
    else
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles);
  end;

begin
  { TODO: AddCommonHandleRefs(); }
  if EntMode = 0 then
    AddOwnerHandle(vHandles, AOwnerHandle);
//  AddResctorsHandle(vHandles, AReactors);
  AddXDictObjHandle(vHandles, AXDictionary);

  case Version of
    acR13..acR2000:
      begin
        AddHandleToArray(CreateDWGHandle(APrevEntHandle, cntDWGObjHandleSoftPointer), vHandles);
        AddHandleToArray(CreateDWGHandle(ANextEntHandle, cntDWGObjHandleSoftPointer), vHandles);
      end;
  end;

  if AEntity <> nil then  //Layer
  begin
    if AEntity.Layer = nil then
      CheckNeedLayer
    else
      AddHandleToArray(CreateDWGHandle(GetEntityHandle(AEntity.Layer),
        cntDWGObjHandleHardPointer), vHandles);
  end
  else
    CheckNeedLayer;

  if LTypeFlags = 3 then //LType
  begin
    if AOwner <> nil then
      vEnt := AOwner
    else
      vEnt := AEntity;
    if vEnt <> nil then
      AddHandleToArray(CreateDWGHandle(GetEntityHandle(vEnt.LineType, False, 0, '',
        True), cntDWGObjHandleHardPointer), vHandles)
    else
      DoError(True, sLTypeIsNil);
  end;

  DoError(PlotStyleFlags = 3, sPlotStyleIsAbsent);

  DoAfterAddEntHandlesData(ObjHandle, ABits, AEntity, AOwnerHandle, DWGCode, EntMode,
    LTypeFlags, PlotStyleFlags, APrevEntHandle, ANextEntHandle, AOwner, vHandles);

  AddHandlesUpdateAndFinish(ABits, vHandles, ObjHandle, GetRealDWGCode(DWGCode));
end;

procedure TsgCADtoDWG.AddCommonHandleRefs(var AHandles: TsgDWGHandleArray;
  const AOwnerHandle: UInt64; const AReactors: TsgInt64List;
  const AXDictionaryHandle: UInt64);
begin
  AddOwnerHandle(AHandles, AOwnerHandle);
  AddReactorsAndXDictHandles(AHandles, AReactors, AXDictionaryHandle);
end;

procedure TsgCADtoDWG.AddCommonHandleRefs(var AHandles: TsgDWGHandleArray;
  const AOwnerHandle: TsgDWGHandle; const AReactors: TsgInt64List = nil;
  const AXDictionaryHandle: UInt64 = 0);
begin
  AddOwnerHandle(AHandles, AOwnerHandle);
  AddReactorsAndXDictHandles(AHandles, AReactors, AXDictionaryHandle);
end;

procedure TsgCADtoDWG.AddCommonTableItemFlags(ABits: TsgDWGBits;
  ATableItem: TsgDXFEntity; const AName: string; AXRefIndex: Integer; AFlags: PByte);
var
  vName: string;
  vFlags: Byte;
begin
  if (AName = '') and Assigned(ATableItem) then
    vName := ExtractName(ATableItem.Name)
  else
    vName := ExtractName(AName);
  ABits.WriteTV(vName, FCodePage); //Entry name TV 2
  if AFlags = nil then
  begin
    AFlags := @vFlags;
    if Assigned(ATableItem) then
      AFlags^ := ATableItem.Flags
    else
      AFlags^ := $40;
  end;
  AFlags^ := AFlags^ or (GetXdep(vName) shl 4);
  if Version >= acR2007 then
    ABits.WriteBS(AXRefIndex)// Was expected ABits.WriteBS(AFlags^)
  else
  begin
    ABits.WriteBit(AFlags^, $40); //The 64-bit of the 70 group
    ABits.WriteBS(AXRefIndex); //subtract one from this value when read. After that, -1 indicates that this reference did not come from an xref, otherwise this value indicates the index of the blockheader for the xref from which this came.
    ABits.WriteBit(AFlags^, $10);// Xdep B 70 block is dependent on an xref. (16 bit)
  end;
end;

function TsgCADtoDWG.AddCreatedElem(const AName: string; const AClass: TsgDXFEntityClass;
  const AElements: TsgDWGCreatedElements; ADWGCode: Word; AHandle: UInt64 = cnstBadHandle;
  AOwnerHandle: UInt64 = cnstBadHandle): Integer;
var
  vHandle: UInt64;
  vEntity: TsgDXFEntity;
begin
  vEntity := nil;
  if AClass <> nil then
  begin
    vEntity := AClass.Create;
    vEntity.Name := AName;
  end;
  vHandle := AHandle;
  if vHandle = cnstBadHandle then
    vHandle := DoHandle;
  Result := AElements.AddElement(vHandle, -1, ADWGCode, AName, AOwnerHandle, vEntity);
end;

function TsgCADtoDWG.AddEED(const ABits: TsgDWGBits; const E: TsgDXFEntity;
  const AEEData: TsgCADExtendedData): Boolean;
var
  vBits: TsgDWGBits;
  vEEData: TsgCADExtendedData;
  vEEDHandle: UInt64;
begin
  Result := False;
  vEEData := TsgCADExtendedData.Create(Version);
  try
    if ConvertEEDToDWG(E, AEEData, vEEData) then
    begin
      vEEDHandle := vEEData.DataInt64[0];
      vBits := CreateDWGBitsByVersion(Version);
      try
        AddEEDDirrect(vBits, vEEData);
        ABits.WriteBS(vBits.Size);
        ABits.WriteEntHandle(vEEDHandle, cntDWGObjHandleHardPointer);
        ABits.WriteBytes(vBits.Data, vBits.Size);
        Result := True;
      finally
        vBits.Free;
      end;
    end;
  finally
    vEEData.Free;
  end;
end;

procedure TsgCADtoDWG.AddEEDItems(const ABits: TsgDWGBits;
  const E: TsgDXFEntity);
var
  I, J: Integer;
  vEEDItems: TsgObjectCollection;
  vAdded: TsgInt64List;

  procedure DoAddEED(const AAppIDName: string; AAdded: TsgInt64List);
  var
    vExtData: TsgCADExtendedData;
  begin
    if AAppIDName <> '' then
    begin
      vExtData := TsgCADExtendedData.Create(Version);
      try
        if E.GetExtData(vExtData, AAppIDName) then
          if AddEED(ABits, E, vExtData) then
            AAdded.Add(vExtData.DataInt64[0]);
      finally
        vExtData.Free;
      end;
    end;
  end;

begin
  vAdded := TsgInt64List.Create;
  try
    vAdded.Sorted := True;
    DoAddEED(sACADXDataAppName, vAdded);
    DoAddEED(sACADXDataAttribAppName, vAdded);
    if FSaveBTIDataDwg then
      DoAddEED(XDataAppName, vAdded);
    DoAddEED(sURLXDataName, vAdded);
    J := EntityEEDItems.IndexOf(E.Handle);
    if J >= 0 then
    begin
      vEEDItems := TsgObjectCollection(EntityEEDItems.List^[J].Data);
      if Assigned(vEEDItems) then
        for I := 0 to vEEDItems.Count - 1 do
          if vAdded.IndexOf(vEEDItems[I].HashCode) = -1 then
            if AddEED(ABits, E, TsgCADExtendedData(vEEDItems[I].Data)) then
              vAdded.Add(TsgCADExtendedData(vEEDItems[I].Data).DataInt64[0]);
      if E.ClassType <> TsgDXFAppID then
        EntityEEDItems.Delete(J);
    end;
    ABits.WriteBS(0);
  finally
    vAdded.Free;
  end;
end;

procedure TsgCADtoDWG.AddHandlesData(const ABits: TsgDWGBits; const AHandles: array of TsgDWGHandle);
begin
  DoAddHandlesData(ABits, AHandles);
end;

procedure TsgCADtoDWG.AddHandlesUpdateAndFinish(const ABits: TsgDWGBits;
  const AHandles: array of TsgDWGHandle; const ObjHandle: UInt64;
  const ADWGCode: Word; AWriteCRC: Boolean = True);
var
  vHandleStreamBitPos, vHandleStreamBitSize: Cardinal;
begin
  if Version <= acR2004 then
  begin
    vHandleStreamBitSize := 0;
    ABits.UpdateRL(GetBitSizePos(ADWGCode), ABits.Position);
    AddHandlesData(ABits, AHandles);
  end
  else
  begin
    ABits.Complete;
    vHandleStreamBitPos := ABits.Position;
    AddHandlesData(ABits, AHandles);
    vHandleStreamBitSize := ABits.Size * cntBitPerByte - vHandleStreamBitPos;
    if Version in [acR2007{..acR2010}] then
      ABits.UpdateRL(GetBitSizePos(ADWGCode), vHandleStreamBitPos);
  end;
  FinishObject(ObjHandle, ABits, ABits.Size, ADWGCode, vHandleStreamBitSize, AWriteCRC);
end;

procedure TsgCADtoDWG.AddHandleToArray(const AHandle: TsgDWGHandle;
  var AHandles: TsgDWGHandleArray);
begin
  SetLength(AHandles, Length(AHandles) + 1);
  AHandles[High(AHandles)] := AHandle;
end;

function TsgCADtoDWG.AddItemToArray(const AHandle: UInt64; const AName: string;
  var AItems: TsgDWGLinkItems): Integer;
begin
  Result := Length(AItems);
  SetLength(AItems, Result + 1);
  AItems[Result].Handle := AHandle;
  AItems[Result].Name := AName;
end;

procedure TsgCADtoDWG.AddXDictObjHandle(var AHandles: TsgDWGHandleArray;
  const XDictObjHandle: UInt64 = cnstBadHandle);
begin
  if (Version < acR2004) or ((Version >= acR2004) and (XDictObjHandle <> cnstBadHandle)) then
    AddHandleToArray(CreateDWGHandle(XDictObjHandle, cntDWGObjHandleHardOwner), AHandles);
end;

procedure TsgCADtoDWG.AddOwnerHandle(var AHandles: TsgDWGHandleArray;
  const AOwnerHandle: UInt64);
begin
  AddHandleToArray(CreateDWGHandle(AOwnerHandle, cntDWGObjHandleSoftPointer), AHandles);
end;

procedure TsgCADtoDWG.AddOwnerHandle(var AHandles: TsgDWGHandleArray;
  const AOwnerHandle: TsgDWGHandle);
begin
  AddHandleToArray(AOwnerHandle, AHandles);
end;

procedure TsgCADtoDWG.AddReactorsAndXDictHandles(
  var AHandles: TsgDWGHandleArray; const AReactors: TsgInt64List;
  const AXDictObjHandle: UInt64);
begin
  AddResctorsHandle(AHandles, AReactors);
  AddXDictObjHandle(AHandles, AXDictObjHandle);
end;

procedure TsgCADtoDWG.AddResctorsHandle(var AHandles: TsgDWGHandleArray;
  const AReactors: TsgInt64List);
var
  I: Integer;
begin
  if Assigned(AReactors) then
    for I := 0 to AReactors.Count - 1 do
      AddHandleToArray(CreateDWGHandle(AReactors[I], cntDWGObjHandleSoftPointer), AHandles);
end;

procedure TsgCADtoDWG.AddXRecordDirrect(const ABits: TsgDWGBits;
  const XRecords: TsgCADExtendedData);
var
  I: Integer;
  vData: TsgExtData;
begin
  for I := 0 to XRecords.DataCount - 1 do
  begin
    vData := XRecords.Data[I];
    ABits.WriteRS(Word(vData.ECode));
    case vData.EType of
      edtByte:      ABits.WriteRC(vData.EByte);
      edtInt16:     ABits.WriteRS(vData.EInt16);
      edtInt:       ABits.WriteRL(vData.EInt);
      edtInt64:     ABits.WriteBytes(@vData.EInt64, 8, False);
      edtSingle:    ABits.WriteRD(vData.ESingle);
      edtDouble:    ABits.WriteRD(vData.EDouble);
      edtString:    ABits.WriteXRecordText(XRecords.DataString[I], FCodePage, False);
      edtF2DPoint:  ABits.Write2RD(PF2DPoint(vData.EData)^);
      edtF3DPoint:  ABits.Write3RD(PFPoint(vData.EData)^);
      edtBinary:
        begin
          ABits.WriteRC(vData.ECount);
          ABits.WriteBytes(vData.EData, vData.ECount);
        end;
    end;
  end;
end;

procedure TsgCADtoDWG.BeforeDestroy(var AClearing: Boolean);
begin
  Clear;
  FreeSections;
  FreeAndNil(FSections);
  ClearObjectObjects(FClasses);
  FreeAndNil(FClasses);
  FreeAndNil(FObjectsMap);
  FreeAndNil(FCreatedElements);
  FreeAndNil(FCreatedEndBlk);
  FreeAndNil(FCreatedSeqEnd);
  ClearScalesDictionary;
  FreeAndNil(FScalesDictionary);
  FreeAndNil(FVariableDictionary);
  FreeAndNil(FEntitiesWithAddedHandles);
//  for I := 0 to FSGInserts.Count - 1 do
//  begin
//    vList := TsgObjectList(FSGInserts[I]);
//    FSGInserts[I] := nil;
//    if Assigned(vList) then
//    begin
//      for J := 0 to vList.Count - 1 do
//      begin
//        vInsert := TsgDXFInsert(vList[J]);
//        vList[J] := nil;
//        FreeAndNil(vInsert);
//      end;
//      vList.Count := 0;
//      FreeAndNil(vList);
//    end;
//  end;
//  FSGInserts.Count := 0;
//  FreeAndNil(FSGInserts);
  TsgObjectList.FreeListOfList(FSGInserts);
  FreeAndNil(FAppIDs);
  FreeAndNil(FSegmentedFile);
  AClearing := False;
  inherited BeforeDestroy(AClearing);
end;

function TsgCADtoDWG.AddEEDDirrect(const ABits: TsgDWGBits;
  const AEEData: TsgCADExtendedData): Integer;
var
  I: Integer;
  vData: TsgExtData;
begin
  Result := 0;
  // exclude first data(AppID handle)!!!
  for I := 1 to AEEData.DataCount - 1 do
  begin
    vData := AEEData.Data[I];
    ABits.WriteRC(Byte(vData.ECode));
    case vData.EType of
      edtByte: ABits.WriteRC(vData.EByte);
      edtInt16: ABits.WriteRS(vData.EInt16);
      edtInt: ABits.WriteRL(vData.EInt);
      edtInt64: ABits.WriteBytes(@vData.EInt64, 8, True);
      edtSingle: ABits.WriteRD(vData.ESingle);
      edtDouble: ABits.WriteRD(vData.EDouble);
      edtString: ABits.WriteXRecordText(AEEData.DataString[I], FCodePage, True);
      edtF2DPoint: ABits.Write2RD(PF2DPoint(vData.EData)^);
      edtF3DPoint: ABits.Write3RD(PFPoint(vData.EData)^);
      edtBinary:
        begin
          ABits.WriteRC(vData.ECount);
          ABits.WriteBytes(vData.EData, vData.ECount);
        end;
    end;
  end;
end;

function TsgCADtoDWG.GetBitSizePos(DWGCode: Word): Cardinal;
var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    Result := vBits.WriteOT(DWGCode);
  finally
    vBits.Free;
  end;
end;

function TsgCADtoDWG.CheckEntHandle(AEntity: TsgDXFEntity; ADoRaise: Boolean = True): Boolean;
begin
  Result := False;
  if AEntity = nil then Exit;
  Result := AEntity.Handle <> cnstBadHandle;
  if (not Result) and ADoRaise then
    raise EDWGExportError.Create(sHandleInvalid);
end;

procedure TsgCADtoDWG.Clear;
  procedure ClearCreatedElements(const Elements: TsgDWGCreatedElements);
  var
    I: Integer;
    vItem: TsgDWGCreatedElement;
  begin
    for I := 0 to Elements.Count - 1 do
    begin
      vItem := Elements.Element[I];
      vItem.Obj.Free;
      vItem.Name := '';
      Finalize(vItem.Handles);
    end;
    Elements.Clear;
  end;
var
  I: Integer;
  vBlock: TsgDXFEntity;
begin
  inherited Clear;
  FreeSections;
  CreateSections;
  ClearObjectObjects(FClasses);
  ClearCreatedElements(FCreatedElements);
  ClearCreatedElements(FObjectsMap);
  FreeAndNil(FAppIDs);
  FModelSpaceHandle := cnstBadHandle;
  FPaperSpaceHandle := cnstBadHandle;
  FCreatedEndBlk.Clear;
  FCreatedSeqEnd.Clear;
  for I := 0 to FEntitiesWithAddedHandles.Count - 1 do
    TsgDXFEntity(FEntitiesWithAddedHandles[I]).Handle := cnstBadHandle;
  FEntitiesWithAddedHandles.Clear;
  for I := Low(FAddedViewPorts) to High(FAddedViewPorts) do
  begin
    vBlock := FAddedViewPorts[I].Owner;
    if Assigned(vBlock) then
      vBlock.RemoveEntity(FAddedViewPorts[I]);
    FAddedViewPorts[I].Free;
  end;
  Finalize(FAddedViewPorts);
  if Assigned(FSegmentedFile) then
    FSegmentedFile.Clear;
end;

procedure TsgCADtoDWG.ClearScalesDictionary;
var
  I: Integer;
  vObj: TsgObjectPointer;
begin
  if (not Assigned(FScalesDictionary)) or (FScalesDictionary.Count = 0) then
    Exit;
  for I := 0 to FScalesDictionary.Count - 1 do
  begin
    vObj := TsgObjectPointer(FScalesDictionary.Objects[I]);
    if Assigned(vObj) then
    begin
      FScalesDictionary.Objects[I] := nil;
      Dispose(PsgDWGScale(vObj.FieldPointer));
      vObj.Free;
    end;
  end;
  FScalesDictionary.Clear;
end;

function TsgCADtoDWG.ConvertEEDToDWG(E: TsgDXFEntity; Source, Dest: TsgCADExtendedData): Boolean;
var
  I: Integer;
  S: string;
  vDataCode: SmallInt;
begin
  Result := False;
//  if IsEntityHasNotEED(E) then Exit; //IsEntityHasNotEED calls AddCommonEntityData calling here is duplicated code
  if (Source = nil) or (Dest = nil) then Exit;
  Dest.AssignData(Source);
  I := Dest.DataCount - 1;
  while I >= 0 do
  begin
    vDataCode := Dest.DataCode[I];
    if vDataCode >= 1000 then
      vDataCode := vDataCode - 1000;
    Dest.DataCode[I] := vDataCode;
    case Dest.DataType[I] of
      edtObject:  Dest.Delete(I);
      edtString:
        case vDataCode of
          1:
            begin
              S := Source.DataString[I];
              Dest.ChangeType(I, edtInt64);
              Dest.DataInt64[I] := TsgHandleNameLinks(AppIDs).HandleByName[S];
            end;
          2:
            begin
              S := Dest.DataString[I];
              Dest.ChangeType(I, edtByte);
              Dest.DataByte[I] := Ord(S = '}');
            end;
          3:
            begin
              S := Source.DataString[I];
              Dest.ChangeType(I, edtInt64);
              Dest.DataInt64[I] := GetEntityHandle(Converter.LayerByName(S));
            end;
        end;
    end;
    Dec(I);
  end;
  Result := Dest.DataCount > 0;
end;

function TsgCADtoDWG.CorrectLineWeight(const ALineWeight: Double;
  ANeedToConvert: Boolean): Integer;
begin
  if ANeedToConvert then
  begin
    Result := ConvertLineWeightToDWG(ALineWeight);
//    if ALineWeight < 0 then
//      Result := 29;
  end
  else
  begin
    if ALineWeight < 0 then
      Result := Trunc(ALineWeight)
    else
      Result := Trunc(ALineWeight) * 100;
  end;
end;

constructor TsgCADtoDWG.Create(ACADImage: TsgCADImage);
begin
  FScalesDictionary := TStringList.Create;
  FVariableDictionary := TStringList.Create;
  inherited Create(ACADImage);
  FSaveBTIDataDwg := bSaveBTIDataDwg;
  FClasses := TStringList.Create;
  FClasses.Sorted := True;
  FClasses.Duplicates := dupError;
  FMaintenanceVersion := 6;
  FSections := TsgObjectList.Create;
  CreateSections;
  FObjectsMap := TsgDWGCreatedElements.Create;
  FObjectsMap.FindBeforeAdd := False;
  FCreatedElements := TsgDWGCreatedElements.Create;
  FCreatedEndBlk := TsgUInt64Pairs.Create;
  FCreatedSeqEnd := TsgUInt64Pairs.Create;
  FDWGCodePage := sgDWGCodePageFromCP({CP_ACP}Converter.HeadVarStruct.CodePage, True);
  FCodePage := sgCodePageFromDWG(FDWGCodePage);
  FEntitiesWithAddedHandles := TList.Create;
  FSGInserts := TsgObjectList.Create;
  FSegmentedFile := TsgSegmentedFile.Create(Converter);
end;

procedure TsgCADtoDWG.CreateSections;
var
  I: Integer;
begin
  for I := 0 to GetSectionsCount - 1 do
    FSections.Add(TMemoryStream.Create);
end;

procedure TsgCADtoDWG.CreateDictionaryListByStrList(const DictionaryName: string;
  DWGCode: Word; AList: TStringList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AddCreatedElem(AList.Values[AList.Names[I]], nil, FCreatedElements,
      DWGCode, cnstBadHandle, cnstBadHandle);
end;

procedure TsgCADtoDWG.CreateTablesRecord;
const
  cnstViewportEntHdrNames: array[Boolean] of string = ('', '1');
var
  vOwnerDictHandle: UInt64;
  I, J, K, vIndex: Integer;
  vPaperSpace, vLayout: TsgDXFLayout;
  vNullViewport: TsgDXFViewport;// only for R2000 with Handle==cnstBadHandle
  vViewport: TsgDXFViewportAccess;
  vViewports: TStringList;
  vRasterVariables: TsgCADRasterVariables;

  procedure AddPenTable(const AName: string; DWGCode: Word);
  begin
    AddCreatedElem(AName, TsgDXFOwneredItem, FCreatedElements, DWGCode);
  end;

  function AddDictionary(const AName: string; const Handle: Uint64 = cnstBadHandle): Integer;
  begin
    Result := AddCreatedElem(AName, TsgDXFDictionary, FCreatedElements,
      cntDWGObjCodeDICTIONARY1, Handle, vOwnerDictHandle);
    if AName = sNamedObjectsDictionary then
      vOwnerDictHandle := FCreatedElements[Result].Handle;
  end;

  procedure CreateVieportEntHeaderHandles(AElem: PsgDWGCreatedElement; const AViewportHandle: UInt64);
  begin
    Finalize(AElem^.Handles);
    AddCommonHandleRefs(AElem^.Handles, GetViewPortEntityHeaderControlObjectHandle); // viewport entity control (soft pointer)
    AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), AElem^.Handles); // External reference block handle (hard pointer)
    AddHandleToArray(CreateDWGHandle(AViewportHandle, cntDWGObjHandleSoftPointer), AElem^.Handles); // Corresponding viewport entity (soft owner)
    AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), AElem^.Handles); // next ViewPortEnt handle hard pointer
  end;

begin
  vOwnerDictHandle := cnstBadHandle;

  //Tables AppId, UCS, View, ViewPortEntityControl
  //AddPenTable(sAppIDTableRecord, cntDWGObjCodeAPPIDCONTROLOBJ);
  AddPenTable(sUCSTableRecord, cntDWGObjCodeUCSCONTROLOBJ);
  AddPenTable(sViewTableRecord, cntDWGObjCodeVIEWCONTROLOBJ);
  AddPenTable(sViewPortEntityControl, cntDWGObjCodeVPENTHDRCTRLOBJ);

  vRasterVariables := TsgCADRasterVariables(ExportCADImage.Converter.Sections[csObjects].FindEntByName(sRasterVariables));
  //Dictionaries
  for I := Low(cnstDWGDictionaries) to High(cnstDWGDictionaries) do
  begin
    if SameText(cnstDWGDictionaries[I], sAcadImageVars) then
    begin
      if Assigned(vRasterVariables) then
        AddDictionary(cnstDWGDictionaries[I], vRasterVariables.Handle);
    end
    else
      AddDictionary(cnstDWGDictionaries[I]);
  end;

  vPaperSpace := GetPaperLayout;
  //Paper space Layout, only if GetPaperLayout = nil
  if vPaperSpace = nil then
    AddCreatedElem(sLayout + '1', nil, FCreatedElements, cntDWGObjCodeLAYOUT,
      cnstBadHandle);
  for K := 1 to Converter.LayoutsCount - 1 do
  begin
    vLayout := TsgDXFLayout(Converter.Layouts[K]);
    vViewports := TStringList.Create;
    try
      vNullViewport := TsgDXFViewport.Create;
      try
        vNullViewport.ThisID := 0;
        vViewports.AddObject(IntToStr(vNullViewport.ThisID), vNullViewport);
        for J := 0 to vLayout.PaperSpaceBlock.Count - 1 do
          if vLayout.PaperSpaceBlock.Entities[J].EntType = ceViewport then
          begin
            vViewport := TsgDXFViewportAccess(vLayout.PaperSpaceBlock.Entities[J]);
            vViewports.AddObject(IntToStr(vViewport.ThisID), vViewport);
          end;
        if vViewports.Count = 1 then
        begin
          SetLength(FAddedViewPorts, Length(FAddedViewPorts) + 1);
          FAddedViewPorts[High(FAddedViewPorts)] := CreateViewPortAsACADWindow(vLayout, DoHandle);
          vLayout.PaperSpaceBlock.AddEntity(FAddedViewPorts[High(FAddedViewPorts)]);
          vViewports.AddObject(IntToStr(FAddedViewPorts[High(FAddedViewPorts)].ThisID), FAddedViewPorts[High(FAddedViewPorts)]);
        end;
        if (Version < acR2004) and (vLayout = vPaperSpace) then
        begin
          //R2000: Viewport entity headers for paperspace
          vIndex := vViewports.IndexOf('1');
          if vIndex > 0 then
            vViewports.Exchange(1, vIndex);
          for I := 0 to vViewports.Count - 1 do
          begin
            vViewport := TsgDXFViewportAccess(vViewports.Objects[I]);
            vIndex := AddCreatedElem(cnstViewportEntHdrNames[I=1], nil,
              FCreatedElements, cntDWGObjCodeVPENTHDR, DoHandle, vViewport.Handle);
            FCreatedElements.GetPtrElement(vIndex)^.Location := Ord(not vViewport.TurnsOff);//location is 1 bit of flags
            CreateVieportEntHeaderHandles(FCreatedElements.GetPtrElement(vIndex), vViewport.Handle);
          end;
        end;
      finally
        vNullViewport.Free;
      end;
    finally
      vViewports.Free;
    end;
  end;
end;

destructor TsgCADtoDWG.Destroy;
begin
  inherited Destroy;
end;

class function TsgCADtoDWG.ExportType: TsgExportFormat;
begin
  Result := efDwg;
end;

const
  sDWGEntityCodes: array[TsgCADEntities] of Word = (cntDWGObjCodeUNSUSED, cntDWGObjCodePOINT,
    cntDWGObjCodeLINE, cntDWGObjCodeSOLID, cntDWGObjCodeTRACE, cntDWGObjCodeCIRCLE,
    cntDWGObjCodeARC, cntDWGObjCodeELLIPSE, cntDWGObjCodeUNSUSED, cntDWGObjCodeLWPOLYLINE,
    cntDWGObjCodeSPLINE, cntDWGObjCodeSPLINE, cntDWGObjCodeLEADER, cntDWGObjCodeINSERT,
    cntDWGObjCodeUNSUSED, cntDWGObjCodeTOLERANCE, cntDWGObjCodeMTEXT, cntDWGObjCodeSHAPE,
    cntDWGObjCodeTEXT, cntDWGObjCodeATTDEF, cntDWGObjCodeATTRIB, cntDWGObjCode3DFACE,
    cntDWGObjCodeIMAGE, cntDWGObjCodeVIEWPORT, cntDWGObjCodeREGION, cntDWGObjCodeBODY,
    cntDWGObjCode3DSOLID, cntDWGObjCodeUNSUSED, cntDWGObjCodeHATCH, cntDWGObjCodeOLE2FRAME,
    cntDWGObjCodeHATCH, cntDWGObjCodeHATCH, cntDWGObjCodeHATCH, cntDWGObjCodeHATCH,
    cntDWGObjCodeINSERT, cntDWGObjCodeLWPOLYLINE, cntDWGObjCodeFLATHATCH, cntDWGObjCodeUNSUSED,
    cntDWGObjCodePROXY, cntDWGObjCodeUNSUSED, cntDWGObjCodeWIPEOUT, cntDWGObjCodeMLINE,
    cntDWGObjCodeUNSUSED{$IFDEF SG_BTI}, cntDWGObjCodePOLYPOLYLINE2D{$ENDIF}, cntDWGObjCodeBODY,
    cntDWGObjCodeUNSUSED, cntDWGObjCodeUNSUSED, cntDWGObjCode3DSOLID, cntDWGObjCode3DSOLID,
    cntDWGObjCode3DSOLID, cntDWGObjCode3DSOLID, cntDWGObjCodeLWPOLYLINE, cntDWGObjCodeRAY, cntDWGObjCodeXLINE,
    cntDWGObjCodeUNSUSED, cntDWGObjCodeMESH, cntDWGObjCode3DSOLID, cntDWGObjCodeUNSUSED);


procedure TsgCADtoDWG.DoAfterAddEntHandlesData(const ObjHandle: UInt64;
  ABits: TsgDWGBits; AEntity: TsgDXFEntity; const AOwnerHandle: UInt64;
  DWGCode: Word; EntMode, LTypeFlags, PlotStyleFlags: Byte;
  const APrevEntHandle, ANextEntHandle: UInt64; AOwner: TsgDXFEntity;
  var Handles: TsgDWGHandleArray);

  procedure AddSeqEnd;
  begin
    FCreatedSeqEnd.ActivePairValue := apFirst;
    AddHandleToArray(CreateDWGHandle(FCreatedSeqEnd[FCreatedSeqEnd.IndexOf(AEntity.Handle)].Second, cntDWGObjHandleHardOwner), Handles);
  end;

var
  I, vIndex: Integer;
  vEnt: TsgDXFEntity;
  vHandle: UInt64;
begin
  if AEntity <> nil then
  begin
    case DWGCode of
      cntDWGObjCodeSHAPE:
        begin
          if Assigned(TsgDXFText(AEntity).Style) then
            vHandle := TsgDXFText(AEntity).Style.Handle
          else
            vHandle := GetEntityHandle(Converter.StyleByName(sStandardName));
          AddHandleToArray(CreateDWGHandle(vHandle, cntDWGObjHandleHardPointer), Handles);
        end;
      cntDWGObjCodeATTRIB, cntDWGObjCodeATTDEF, cntDWGObjCodeTEXT:
        AddHandleToArray(CreateDWGHandle(GetStyleHandle(TsgDXFText(AEntity).Style, False),
          cntDWGObjHandleHardPointer), Handles);
      cntDWGObjCodeMTEXT:
        begin
          AddHandleToArray(CreateDWGHandle(GetStyleHandle(TsgDXFMText(AEntity).Style, False),
            cntDWGObjHandleHardPointer), Handles);
          if (Version >= acR2018) and not AEntity.Annotative then
            AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), Handles);
        end;
      cntDWGObjCodePOLYLINE2D, cntDWGObjCodePOLYLINEMESH,
      cntDWGObjCodePOLYLINEPFACE, cntDWGObjCodePOLYLINE3D:
        begin
          if Version < acR2004 then
          begin
            if AEntity.Count > 0 then
            begin
              AddHandleToArray(CreateDWGHandle(AEntity.Entities[0].Handle, cntDWGObjHandleSoftPointer), Handles);
              AddHandleToArray(CreateDWGHandle(AEntity.Entities[AEntity.Count - 1].Handle, cntDWGObjHandleSoftPointer), Handles);
            end
            else
            begin
              AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleSoftPointer), Handles);
              AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleSoftPointer), Handles);
            end
          end
          else
            for I := 0 to AEntity.Count - 1 do
              AddHandleToArray(CreateDWGHandle(AEntity.Entities[I].Handle, cntDWGObjHandleHardOwner), Handles);
          AddSeqEnd;
        end;
      cntDWGObjCodeINSERT, cntDWGObjCodeMINSERT1:
        begin
          if TsgDXFInsert(AEntity).BlockRecord <> nil then
            vEnt := TsgDXFInsert(AEntity).BlockRecord
          else
            vEnt := nil;
          AddHandleToArray(CreateDWGHandle(GetEntityHandle(vEnt), cntDWGObjHandleHardPointer), Handles);
          if Version < acR2004 then
          begin
            if TsgDXFInsert(AEntity).HasAttribs then
            begin
              AddHandleToArray(CreateDWGHandle(TsgDXFAttrib(TsgDXFInsert(AEntity).Attribs[0]).Handle, cntDWGObjHandleSoftPointer), Handles);
              AddHandleToArray(CreateDWGHandle(TsgDXFAttrib(TsgDXFInsert(AEntity).Attribs[TsgDXFInsert(AEntity).Attribs.Count - 1]).Handle, cntDWGObjHandleSoftPointer), Handles);
            end
          end
          else
          begin
            for I := 0 to TsgDXFInsert(AEntity).Attribs.Count - 1 do
              AddHandleToArray(CreateDWGHandle(TsgDXFAttrib(TsgDXFInsert(AEntity).Attribs[I]).Handle, cntDWGObjHandleHardOwner), Handles);
          end;
          if TsgDXFInsert(AEntity).HasAttribs then
            AddSeqEnd;
        end;
      cntDWGObjCodeTOLERANCE:
        AddHandleToArray(CreateDWGHandle(GetStyleHandle(TsgDXFTolerance(AEntity).DimStyle, True),
          cntDWGObjHandleHardPointer), Handles);
      cntDWGObjCodeLEADER:
        begin
          AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), Handles);
          AddHandleToArray(CreateDWGHandle(GetStyleHandle(TsgDXFLeader(AEntity).DimStyle, True),
            cntDWGObjHandleHardPointer), Handles);
        end;
      cntDWGObjCodeDIMENSIONALIGNED, cntDWGObjCodeDIMENSIONLINEAR,
      cntDWGObjCodeDIMENSIONANG2Ln, cntDWGObjCodeDIMENSIONRADIUS,
      cntDWGObjCodeDIMENSIONDIAMETER, cntDWGObjCodeARC_DIMENSION:
        begin
          AddHandleToArray(CreateDWGHandle(GetStyleHandle(TsgDXFDimension(AEntity).Style, True),
            cntDWGObjHandleHardPointer), Handles);
          AddHandleToArray(CreateDWGHandle(GetEntityHandle(TsgDXFDimension(AEntity).BlockRecord),
            cntDWGObjHandleHardPointer), Handles);
        end;
      cntDWGObjCodeVIEWPORT:
        begin
          if Assigned(TsgDXFViewport(AEntity).FrozenLayers) then
            for I := 0 to TsgDXFViewport(AEntity).FrozenLayers.Count - 1 do // FrozenLayers
              if Version = acR2000 then
                AddHandleToArray(CreateDWGHandle(TsgDXFViewport(AEntity).FrozenLayers[I],
                  cntDWGObjHandleHardPointer), Handles)
              else
                AddHandleToArray(CreateDWGHandle(TsgDXFViewport(AEntity).FrozenLayers[I],
                  cntDWGObjHandleSoftPointer), Handles);
          AddHandleToArray(CreateDWGHandle(GetEntityHandle(TsgDXFViewport(AEntity).ClippingBoundary), cntDWGObjHandleHardPointer), Handles); //Clip boundary handle (soft pointer) checked files shows (hard pointer)
          if Version = acR2000 then
          begin
            vHandle := cnstBadHandle;
            vIndex := FCreatedElements.IndexOf(AEntity.Handle, True);
            if vIndex >= 0 then
              vHandle := FCreatedElements[vIndex].Handle;
           AddHandleToArray(CreateDWGHandle(vHandle, cntDWGObjHandleHardPointer), Handles); // VIEWPORT ENT HEADER ((hard pointer))
          end;
          AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), Handles); // H 345 Named UCS Handle (hard pointer)
          AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), Handles); // H 346 Base UCS Handle (hard pointer)
          if Version >= acR2007 then
          begin
            AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleSoftPointer), Handles); // H 332 Background (soft pointer)
            AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), Handles); // H 348 Visual Style (hard pointer)
            AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleSoftPointer), Handles); // H 333 Shadeplot ID (soft pointer)
            AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardOwner), Handles); // H 361 Sun (hard owner)
          end;
        end;
      cntDWGObjCodeWIPEOUT, cntDWGObjCodeIMAGE:
        begin
          AddHandleToArray(CreateDWGHandle(GetImageDefHandle(TsgCADWipeout(AEntity)), cntDWGObjHandleHardPointer), Handles); // H imagedef (hard pointer)
          AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardOwner), Handles); // H imagedefreactor (hard owner)
        end;
      cntDWGObjCodeMLINE:
        AddHandleToArray(CreateDWGHandle(GetEntityHandle(TsgCADMLine(AEntity).Style), cntDWGObjHandleHardPointer), Handles); // mline style oject handle (hard pointer)
      cntDWGObjCode3DSOLID, cntDWGObjCodeREGION, cntDWGObjCodeBODY, cntDWGObjCodeSURFACE:
        begin
          if Version > acR2004 then
            AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardOwner), Handles); //History ID
        end;
    end;
  end;
end;

procedure TsgCADtoDWG.DoBeforeAddCommonEntData(ABits: TsgDWGBits; var Handle: UInt64;
  var E: TsgDXFEntity; var DWGCode: Word; OwnerDWGCode: Word; AOwner: TsgDXFEntity; var XDictionary: UInt64);
var
  vIndex: Integer;
  vPLine: TsgDXFPolyline;
  vItem: TsgDXFEntity;

  function Check2DPolyline: Boolean;
  var
    I: Integer;
    vVertex: TsgDXFVertex;
  begin
    Result := (vPLine.Flags and 8 = 0) or (not IsZero(vPLine.GlobalWidth)) or
      (not IsZero(vPLine.StartWidth)) or (not IsZero(vPLine.EndWidth)); //!!! maybe ZThick
    if not Result then
    begin
      for I := 0 to vPLine.Count do
      begin
        vVertex := TsgDXFVertex(vPLine.Entities[I]);
        if (not IsZero(vVertex.StartWidth)) or (not IsZero(vVertex.EndWidth)) or (not IsZero(vVertex.Bulge)) then //!!! maybe check Point.Z
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;

  procedure ChangeToSeqEnd;
  begin
    DWGCode := cntDWGObjCodeSEQEND;
    Handle := FCreatedSeqEnd.Second[vIndex];
    E := AOwner;
  end;

  function CheckSeqEndAndChange: Boolean;
  begin
    FCreatedSeqEnd.ActivePairValue := apFirst;
    vIndex := FCreatedSeqEnd.IndexOf(E.Handle);
    if vIndex >= 0 then //SeqEnd
    begin
      ChangeToSeqEnd;
      Result := True;
    end
    else
    begin
      FCreatedSeqEnd.AddPair(E.Handle, DoHandle);
      Result := False;
    end;
  end;

begin
  XDictionary := cnstBadHandle;
  if E is TsgDXFBlock then //ENDBLK
  begin
    FCreatedEndBlk.ActivePairValue := apFirst;
    vIndex := FCreatedEndBlk.IndexOf(TsgDXFBlock(E).BlockRecord.Handle);
    DoError(vIndex < 0, sCannotFindBlock, [TsgDXFBlock(E).BlockRecord.Handle]);
    Handle := FCreatedEndBlk[vIndex].Second;
    DWGCode := cntDWGObjCodeENDBLK;
    E := nil;
    Exit;
  end;
  if E is TsgDXFPolyline then
  begin
    vPLine := TsgDXFPolyline(E);
    case E.EntType of
      cePolyline:
        begin
          if vPLine.IsPolygonMesh then
            DWGCode := cntDWGObjCodePOLYLINEMESH
          else
            if vPLine.IsPolyFaceMesh then
              DWGCode := cntDWGObjCodePOLYLINEPFACE
            else
              if TsgDXFPolylineAccess(vPLine).Is3dPolyline then
                DWGCode := cntDWGObjCodePOLYLINE3D
              else
                DWGCode := cntDWGObjCodePOLYLINE2D;
        end;
      ceLWPolyline:
        DWGCode := cntDWGObjCodeLWPOLYLINE;
    else
      DWGCode := cntDWGObjCodePOLYLINE3D;
    end;
    if DWGCode <> cntDWGObjCodeLWPOLYLINE then
      CheckSeqEndAndChange;
    Exit;
  end;
  if E is TsgDXFVertex then
  begin
    case OwnerDWGCode of
      cntDWGObjCodePOLYLINE2D:
        DWGCode := cntDWGObjCodeVERTEX2D;//2D Vertex
      cntDWGObjCodePOLYLINE3D:
        DWGCode := cntDWGObjCodeVERTEX3D;//3D Vertex
      cntDWGObjCodePOLYLINEMESH:
        DWGCode := cntDWGObjCodeVERTEXMESH;
      else
        DWGCode := cntDWGObjCodeVERTEXPFACE;
    end;
    if TsgDXFVertexEx(E).FPolyFaceIndexes <> nil then
      DWGCode := cntDWGObjCodeVERTEXPFACEFACE;
    E := AOwner;
    Exit;
  end;
  if E.EntType = ceInsert then
  begin
    if TsgDXFInsert(E).HasAttribs then
      if CheckSeqEndAndChange then Exit;
    if E.ClassType = TsgCADMInsert then
      DWGCode := cntDWGObjCodeMINSERT1;
    vItem := GetExtensionDictionary(E, sAcadFilterDictionary);
    if Assigned(vItem) then
      XDictionary := CastEntToID(vItem.Owner);
  end;
  if E.EntType in [ceAttdef, ceAttrib] then
  begin
    vItem := GetExtensionDictionary(E, cnstAcadMLAtt);
    if Assigned(vItem) then
      XDictionary := CastEntToID(vItem.Owner);
  end;
  if E is TsgDXFDimension then
  begin
    if TsgDXFDimension(E).Flags and 7 = 7 then
      DWGCode := cntDWGObjCodeARC_DIMENSION
    else
      DWGCode := cntDWGObjCodeDIMENSIONLINEAR;
//    case DimSubclassNum(TsgDXFDimension(E).Flags) of //for future versions
//      1: DWGCode := cntDWGObjCodeDIMENSIONALIGNED;
//      2: DWGCode := cntDWGObjCodeDIMENSIONANG2Ln;
//      3: DWGCode := cntDWGObjCodeDIMENSIONDIAMETER;
//      4: DWGCode := cntDWGObjCodeDIMENSIONRADIUS;
//      5: DWGCode := cntDWGObjCodeDIMENSIONANG3Pt;
//      6: DWGCode := cntDWGObjCodeDIMENSIONORDINATE;
//    else
//      DWGCode := cntDWGObjCodeDIMENSIONLINEAR;
//    end;
    Exit;
  end;
  if E is TsgCADPolyPolygon then
  begin
    case E.EntType of
      ceMPolygon: DWGCode := cntDWGObjCodeMPOLYGON;
    else
      DWGCode := cntDWGObjCodeHATCH;
    end;
    Exit;
  end;
  if E.EntType = ceImageEnt then
  begin
    case GetExportImageIndex(TsgDXFImageEnt(E), ExportImageFileIndex) of
      cntImageExportAsImage:
        DWGCode := cntDWGObjCodeIMAGE;
      cntImageExportAsOLE2Frame:
        DWGCode := cntDWGObjCodeOLE2FRAME;
      cntImageExportAsFile:
        DWGCode := cntDWGObjCodeIMAGEBYFILE;
    end;
    Exit;
  end;
end;

procedure TsgCADtoDWG.DoExport(const E: TsgDXFEntity; const Data: PsgDirectExportData = nil);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vLTypeFlags, vPlotStyeFlags: Byte;
  vHandle: UInt64;
  vEnt: TsgDXFEntity;
  vExportData: TsgDWGExportDataClass;
  vColorCadOld: TsgColorCAD;
  vXDict: UInt64;
begin
  vBits := CreateDWGBitsByVersion(Version);
  vExportData := nil;
  try
    vCode := sDWGEntityCodes[E.EntType];
    if ((vCode = cntDWGObjCodeUNSUSED) and (Data.EntDWGCode <> cntDWGObjCodeUNSUSED)) or Data.UseEntCode then
      vCode := Data.EntDWGCode;
    vHandle := E.Handle;
    vEnt := E;
    DoBeforeAddCommonEntData(vBits, vHandle, vEnt, vCode, Data.OwnerDWGCode, Data.Owner, vXDict);
    if Data.UseEntCode then
      vCode := Data.EntDWGCode;
    if vCode = cntDWGObjCodeUNSUSED then
      vCode := cntDWGObjCodeUNKNOWNOBJ;//all unknown objects will be exported as cntDWGObjCodePOINT see GetRealDWGCode
    try
      case vCode of
        cntDWGObjCodeWIPEOUT, cntDWGObjCodeIMAGE, cntDWGObjCodeIMAGEBYFILE:
        begin
          if IsNeedImageDefHandle(TsgCADWipeOut(vEnt)) then
          begin
            vColorCadOld := vEnt.ColorCAD;
            vEnt.ColorCAD := cnstColorCADByBlackWhite;
          end;
        end;
      end;
      AddCommonEntityData(vHandle, vCode, vBits, nil, vEnt, Data.EntMode,
        @vLTypeFlags, @vPlotStyeFlags, vXDict);
    finally
      case vCode of
        cntDWGObjCodeWIPEOUT, cntDWGObjCodeIMAGE, cntDWGObjCodeIMAGEBYFILE:
        begin
          if IsNeedImageDefHandle(TsgCADWipeOut(vEnt)) then
            vEnt.ColorCAD := vColorCadOld;
        end;
      end;
    end;
    case vCode of
      cntDWGObjCodeSEQEND:
        ExportSeqEnd(nil, vBits);
      cntDWGObjCodeLINE:
        ExportLine(TsgDXFLine(E), vBits);
      cntDWGObjCodeRAY, cntDWGObjCodeXLINE:
        ExportRay(TsgDXFRay(E), vBits);
      cntDWGObjCodePOINT:
        ExportPoint(TsgDXFPoint(E), vBits);
      cntDWGObjCodeSOLID, cntDWGObjCodeTRACE:
        ExportSolid(TsgDXFSolid(E), vBits);
      cntDWGObjCodeCIRCLE, cntDWGObjCodeARC:
        ExportCircle(TsgDXFCircle(E), vBits);
      cntDWGObjCodePOLYLINEMESH, cntDWGObjCodePOLYLINEPFACE,
      cntDWGObjCodePOLYLINE2D, cntDWGObjCodePOLYLINE3D:
        ExportPolyline(TsgDXFPolyline(E), vCode, vBits);
      cntDWGObjCodeLWPOLYLINE:
        begin
          case E.EntType of
            ceFlatPoly, ceFlatPoly3D:
              begin
                ExportFlatPoly(TsgFlatPoly(E), Data^);
                Exit; //TsgFlatPoly is not exported, only members of the FlatPoly.Group property
              end;
          else
            ExportLWPolyline(TsgDXFLWPolyline(E), vBits);
          end;
        end;
      cntDWGObjCodeINSERT, cntDWGObjCodeMINSERT1:
        begin
          if (TsgDXFInsert(E).Block <> nil) and IsNameInternal(TsgDXFInsert(E).Block.Name) then
          begin
            ExportUnknownObject(E, Data^);
            Exit;
          end
          else
            ExportInsert(TsgDXFInsert(E), vBits, nil);
        end;
      cntDWGObjCodeSPLINE:
        ExportSpline(TsgDXFSpline(E), vBits);
      cntDWGObjCodeATTRIB, cntDWGObjCodeATTDEF, cntDWGObjCodeTEXT:
        ExportText(TsgDXFText(E), vBits);
      cntDWGObjCodeSHAPE:
        ExportShape(TsgDXFShape(E), vBits);
      cntDWGObjCodeOLE2FRAME:
      begin
        vExportData := TsgDWGExportDataClass.CreateEx(vBits, E, TsgDXFImageEntAccess(E).GroupGet,
          Data^);
        case E.EntType of
          ceImageEnt:
            begin
              if not TsgDXFImageEntAccess(E).GroupExists then
              begin
                vExportData.Bits := nil;
                ExportImageEntAsOle(TsgDXFImageEnt(E), ExportOle2Frame,
                  vExportData, True);
              end
              else
              begin
                ExportImageEntAsOle(TsgDXFImageEnt(E), ExportOle2FrameCreateOleGroup,
                  vExportData, True);
                if not ExportGroup(E, cntDWGObjCodePOLYLINE3D, Data^) then
                  ExportUnknownObject(E, Data^);
              end;
              Exit;
            end
        else
          ExportOle2Frame(TsgDXFOle2Frame(E), vExportData);
        end;
      end;
      cntDWGObjCodeVERTEX2D, cntDWGObjCodeVERTEX3D, cntDWGObjCodeVERTEXMESH,
      cntDWGObjCodeVERTEXPFACE, cntDWGObjCodeVERTEXPFACEFACE:
        ExportVertex(Data.Owner, TsgDXFVertex(E), vCode, vBits);
      cntDWGObjCodeMTEXT:
        ExportMText(TsgDXFMText(E), vBits);
      cntDWGObjCodeTOLERANCE:
        ExportTolerance(TsgDXFTolerance(E), vBits);
      cntDWGObjCode3DFACE:
        Export3DFace(TsgDXF3dFace(E), vBits);
      cntDWGObjCodeELLIPSE:
        ExportEllipse(TsgDXFEllipse(E), vBits);
      cntDWGObjCodeLEADER:
        ExportLeader(TsgDXFLeader(E), vBits);
      cntDWGObjCodeDIMENSIONORDINATE, cntDWGObjCodeDIMENSIONANG3Pt,
      cntDWGObjCodeDIMENSIONALIGNED, cntDWGObjCodeDIMENSIONLINEAR,
      cntDWGObjCodeDIMENSIONANG2Ln, cntDWGObjCodeDIMENSIONRADIUS,
      cntDWGObjCodeDIMENSIONDIAMETER, cntDWGObjCodeARC_DIMENSION:
        ExportDimension(TsgDXFDimension(E), vBits, vCode);
      cntDWGObjCodeHATCH, cntDWGObjCodeMPOLYGON:
        begin
          if E is TsgCADFill then
          begin
            ExportCADFill(TsgCADFill(E), Data^);
            Exit;
          end
          else
            ExportHatch(TsgCADPolyPolygon(E), vBits, vCode);
        end;
      cntDWGObjCodeVIEWPORT:
        ExportViewPort(TsgDXFViewPort(E), vBits);
      cntDWGObjCodeWIPEOUT, cntDWGObjCodeIMAGE:
        begin
          ExportWipeOut(TsgCADWipeOut(E), vBits);
          if IsNeedImageDefHandle(TsgCADWipeOut(E)) then
            ProcImageDef(TsgDXFImageEntAccess(E).ImageDef);
        end;
      cntDWGObjCodeIMAGEBYFILE:
        begin
          vExportData := TsgDWGExportDataClass.CreateEx(nil, E, nil, Data^);
          ExportImageEntAsExternalFile(TsgDXFImageEnt(E), DoHandle,
            ExportWipeOutAsExternalFile, vExportData);
//          TsgDXFImageEntAccess(E).ImageDefExport := nil; see TsgDXFImageEntAccess.Destroy ImageDefExport clears its destructor
          Exit;
        end;
      cntDWGObjCodeREGION, cntDWGObjCodeSURFACE, cntDWGObjCode3DSOLID, cntDWGObjCodeBODY:
        begin
          if ConvertAcis then
          begin
            if not ExportGroup(E, cntDWGObjCodePOLYLINE3D, Data^) then
            begin
              ExportUnknownObject(E, Data^);
            end;
            Exit;
          end
          else
          if not ExportACISEntity(TsgBrepModAcis(E), vBits) then
          begin
            ExportUnknownObject(E, Data^);
            Exit;
          end;
        end;
      cntDWGObjCodePROXY, cntDWGObjCodeFLATHATCH:
        begin
          if not ExportGroup(E, cntDWGObjCodePOLYLINE3D, Data^) then
            ExportUnknownObject(E, Data^);
           Exit;
        end;
    {$IFDEF SG_BTI}
      cntDWGObjCodePOLYPOLYLINE2D:
        begin
          ExportPolyPolyline2D(TsgCADPolyPolyline2D(E), Data^);
          Exit;
        end;
    {$ENDIF}
      cntDWGObjCodeUNKNOWNOBJ:
        begin
          ExportUnknownObject(E, Data^);
          Exit;
        end;
      cntDWGObjCodeMLINE:
         ExportMLine(TsgCADMLine(E), vBits, Data^);
      cntDWGObjCodeMESH:
        ExportMesh(TsgDXFMesh(E), vBits);
      cntDWGObjCodeUNSUSED:
        Exit;
    end;
  //      ceACADTable:         ExportAcadTable(TsgDXFAcadTable(E));
  //
    AddCommonEntityHandleData(vHandle, vBits, E, Data.OwnerHandle, vCode,
      Data.EntMode, vLTypeFlags, vPlotStyeFlags, True, Data.PrevEntHandle,
      Data.NextEntHandle, Data.Owner, vXDict);
  finally
    vBits.Free;
    vExportData.Free;
  end;
end;

procedure TsgCADtoDWG.ProcScale(const Handle, OwnerHandle: UInt64; const Index: Integer;
  const Name: string);
var
  vBits: TsgDWGBits;
  vScale: TsgDWGScale;
  vHandles: TsgDWGHandleArray;
  vCode: Word;
  vReactors: TsgInt64List;
begin
  vReactors := TsgInt64List.Create;
  vBits := CreateDWGBitsByVersion(Version);
  try
    vCode := GetDWGCodeByClass(cntClassDXFSCALE);
    vReactors.Add(OwnerHandle);
    AddCommonEntityData(Handle, vCode, vBits, vReactors);
    vScale := PsgDWGScale(TsgObjectPointer(FScalesDictionary.Objects[Index]).FieldPointer)^;
    vBits.WriteBS(0);
    vBits.WriteTV(Name, FCodePage);
    vBits.WriteBD(vScale.ScaleParam1);
    vBits.WriteBD(vScale.ScaleParam2);
    vBits.WriteBit(vScale.ScaleParam3);
    AddCommonHandleRefs(vHandles, OwnerHandle, vReactors);
    AddHandlesUpdateAndFinish(vBits, vHandles, Handle, vCode);
  finally
    vBits.Free;
    vReactors.Free;
  end;
end;

procedure TsgCADtoDWG.ProcSpatialFilter(const AFilter: TsgCADSpatialFilter);
var
  I: Integer;
  vBits: TsgDWGBits;
  vCode: Word;
  vHandles: TsgDWGHandleArray;
  vReactors: TsgInt64List;

  procedure WriteMatrix(ABits: TsgDWGBits; const AMatrix: TFMatrix);
  var
    L, K: Integer;
  begin
    for K := 0 to 2 do
      for L := 0 to 3 do
        vBits.WriteBD(AMatrix.M[L][K]);
  end;

begin
  vReactors := TsgInt64List.Create;
  vBits := CreateDWGBitsByVersion(Version);
  try
    vCode := GetDWGCodeByClass(cntClassDXFSPATIAL_FILTER);
    vReactors.Add(AFilter.Owner.Handle);
    AddCommonEntityData(AFilter.Handle, vCode, vBits, vReactors);
    vBits.WriteBS(AFilter.Bounds.Count);
    for I := 0 to AFilter.Bounds.Count - 1 do
      vBits.Write2RD(AFilter.Bounds[I].Point2D);
    vBits.Write3BD(AFilter.Extrusion);
    vBits.Write3BD(AFilter.Clipbdorg);
    vBits.WriteBS(Ord(AFilter.DisplayBounds));
    vBits.WriteBS(Ord(AFilter.FrontClip));
    if AFilter.FrontClip then
      vBits.WriteBD(AFilter.FrontDist);
    vBits.WriteBS(Ord(AFilter.BackClip));
    if AFilter.BackClip then
      vBits.WriteBD(AFilter.BackDist);
    WriteMatrix(vBits, AFilter.InvBlkTransform);
    WriteMatrix(vBits, AFilter.Clip);
    AddCommonHandleRefs(vHandles, AFilter.Owner.Handle, vReactors);
    AddHandlesUpdateAndFinish(vBits, vHandles, AFilter.Handle, vCode);
  finally
    vReactors.Free;
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcXRecords;
{var
  I, C: Integer;
  vHashList: PsgHashItemsArray;
  Item: TsgDictionaryItem;
  vAcadFilterDict: TsgDXFEntity;
  vFilter: TsgCADSpatialFilter;
  vItems: TsgDWGLinkItems;
  vXrec: TsgDXFXRecord;
begin
  C := TsgDXFConverterAccess(Converter).EntityDictionaries.Count;
  vHashList := TsgDXFConverterAccess(Converter).EntityDictionaries.List;
  SetLength(vItems, 1);
  for I := 0 to C - 1 do
  begin
    Item := vHashList^[I].Data;
    case Item.Entity.EntType of
      ceInsert:
        begin
          vAcadFilterDict := Item.Item.FindEntByName(sAcadFilterDictionary);
          if Assigned(vAcadFilterDict) then
          begin
            vFilter := TsgCADSpatialFilter(vAcadFilterDict.FindEntByName(cnstSpatial));
            if Assigned(vFilter) then
            begin
              vItems[0] := CreateDWGLinkItem(vAcadFilterDict.Handle, sAcadFilterDictionary);
              ProcDictionary(Item.Item.Handle, Item.Entity.Handle, vItems, 1);

              vItems[0] := CreateDWGLinkItem(vFilter.Handle, cnstSpatial);
              ProcDictionary(vAcadFilterDict.Handle, vAcadFilterDict.Owner.Handle, vItems, 1);

              ProcSpatialFilter(vFilter);
            end;
          end;
        end;
      ceAttdef, ceAttrib:
        begin
          vXrec := TsgDXFXRecord(Item.Item.FindEntByName(cnstAcadMLAtt));
          if Assigned(vXrec) then
          begin
            vItems[0] := CreateDWGLinkItem(vXrec.Handle, cnstAcadMLAtt);
            ProcDictionary(vXrec.Owner.Handle, Item.Entity.Handle, vItems, 1);
            ProcXRecord(vXrec.Handle, vXrec.Data, vXrec.Owner.Handle);
          end;
        end;
    end;
  end;
end;}
var
  I, J, C, vCount: Integer;
  E: TsgDXFEntity;
  vHashList: PsgHashItemsArray;
  Item: TsgTableItem;
  vFilter: TsgDXFEntity;
  vItems, vSpatialItems: TsgDWGLinkItems;
  vEntItems: array of TsgDXFEntity;
begin
  C := TsgDXFConverterAccess(Converter).EntityDictionaries.Count;
  vHashList := TsgDXFConverterAccess(Converter).EntityDictionaries.List;
  for I := 0 to C - 1 do
  begin
    Item := vHashList^[I].Data;
    SetLength(vItems, Item.Item.Count);
    SetLength(vEntItems, Item.Item.Count);
    vCount := 0;
    vFilter := nil;
    for J := 0 to Item.Item.Count - 1 do
    begin
      E := Item.Item[J];
      if E is TsgDXFXRecord then
      begin
        vItems[vCount] := CreateDWGLinkItem(E.Handle, E.Name);
        vEntItems[vCount] := E;
        Inc(vCount);
      end
      else
        if (E.Name = sAcadFilterDictionary) and (E is TsgDXFDictionary) then
        begin
          vFilter := E.FindEntByName(cnstSpatial);
          if Assigned(vFilter) then
          begin
            vItems[vCount] := CreateDWGLinkItem(E.Handle, E.Name);
            vEntItems[vCount] := E;
            Inc(vCount);
          end;
        end;
    end;
    if vCount > 0 then
    begin
      SetLength(vItems, vCount);
      ProcDictionary(Item.Item.Handle, vHashList^[I].HashCode, vItems, 1);
      for J := Low(vItems) to vCount - 1 do
        if Assigned(vFilter) and (vFilter.Owner = vEntItems[J]) then
        begin
          SetLength(vSpatialItems, 1);
          vSpatialItems[0] := CreateDWGLinkItem(vFilter.Handle, cnstSpatial);
          ProcDictionary(vFilter.Owner.Handle, Item.Item.Handle, vSpatialItems, 1);
          ProcSpatialFilter(TsgCADSpatialFilter(vFilter));
        end
        else
          ProcXRecord(vItems[J].Handle, TsgDXFXRecord(vEntItems[J]).Data, Item.Item.Handle);
    end;
  end;
end;

procedure TsgCADtoDWG.ProcXData;
var
  I: Integer;
  vItems: TsgDWGLinkItems;
  vXrecordCode: Word;
  vDictionaryElem, vXrecordElem: TsgDWGCreatedElement;
begin
  FCreatedElements.SortBy := seHandle;
  vXrecordCode := GetDWGCodeByClass(cntClassDXFXRECORD);
  I := 0;
  while I < FCreatedElements.Count do
  begin
    vDictionaryElem := FCreatedElements[I];
    if vDictionaryElem.DWGObjCode = cntDWGObjCodeDICTIONARY1 then
    begin
      Inc(I);
      if I < FCreatedElements.Count then
      begin
        SetLength(vItems, 0);
        vXrecordElem := FCreatedElements[I];
        repeat
          if (vXrecordElem.DWGObjCode = vXrecordCode) and (vXrecordElem.OwnerHandle = vDictionaryElem.Handle) then
          begin
            AddItemToArray(vXrecordElem.Handle, vXrecordElem.Name, vItems);
            ProcXRecord(vXrecordElem.Handle, TsgDXFXRecord(vXrecordElem.Obj).Data, vDictionaryElem.Handle);
          end;
          Inc(I);
          if I < FCreatedElements.Count then
            vXrecordElem := FCreatedElements[I];
        until (I >= FCreatedElements.Count) or (vXrecordElem.DWGObjCode <> vXrecordCode) or
          (vXrecordElem.OwnerHandle = vDictionaryElem.Handle);
        if Length(vItems) > 0 then
          ProcDictionary(vDictionaryElem.Handle, vDictionaryElem.OwnerHandle, vItems, 1);
        Dec(I);
      end;
    end;
    Inc(I);
  end;
end;

procedure TsgCADtoDWG.ProcXRecord(const AHandle: UInt64;
  const AData: TsgCADExtendedData; AOwnerDictionary: UInt64);
var
  vBits, vXrecordBits: TsgDWGBits;
  vXrecordCode: Word;
  vHandles: TsgDWGHandleArray;
  vReactors: TsgInt64List;
  vMS: TMemoryStream;
begin
  vReactors := TsgInt64List.Create;
  vBits := CreateDWGBitsByVersion(Version);
  try
    vXrecordCode := GetDWGCodeByClass(cntClassDXFXRECORD);
    vReactors.Add(AOwnerDictionary);
    AddCommonEntityData(AHandle, vXrecordCode, vBits, vReactors{, AXrecord, 3, nil, nil, cnstBadHandle});
    vXrecordBits := CreateDWGBitsByVersion(Version);
    try
      AddXRecordDirrect(vXrecordBits, AData);
      vBits.WriteBL(vXrecordBits.Size);
      vMS := TMemoryStream.Create;
      try
        vXrecordBits.SaveToStream(vMS);
        vBits.WriteBytes(vMS.Memory, vMS.Size);
      finally
        vMS.Free;
      end;
    finally
      vXrecordBits.Free;
    end;
    if Version >= acR2000 then
      vBits.WriteBS(0); //Cloning flag BS 280
    AddCommonHandleRefs(vHandles, AOwnerDictionary, vReactors);
    AddHandlesUpdateAndFinish(vBits, vHandles, AHandle, vXrecordCode);
  finally
    vReactors.Free;
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionAppInfo;
const
  cnstAppInfoProduct = 'CAD VCL';
  cnstAppInfoDataList = 'AppInfoDataList';
  cnstUnknownAppInfo = '4001';
  cnstRegistryVersion = '12.0';
  cnstBuildVersion = '0.0';
  cnstAppInfoVersion = cnstRegistryVersion + '.' + cnstBuildVersion;
  cnstProductInformation = '<ProductInformation name ="%s" build_version="%s" registry_version="%s" install_id_string="%s" registry_localeID="1033"/>';
  cnstComments = 'This file was last saved by an CADSoftTools™ (CST) application or an CST licensed application.';
var
  vBits: TsgDWGBits;
  vProductInformation: string;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vProductInformation := Format(cnstProductInformation, [cnstAppInfoProduct, cnstBuildVersion,
      cnstRegistryVersion, cnstCompanyCST]);
    case Version of
      acR2004:
        begin
          vBits.WriteAppInfoText(cnstAppInfoDataList);
          vBits.WriteRL(2);
          vBits.WriteAppInfoText(cnstUnknownAppInfo);
          vBits.WriteAppInfoText(vProductInformation);
          vBits.WriteAppInfoText(cnstAppInfoVersion);
        end;
      acR2010..acR2018:
        begin
          vBits.WriteRL(2); //Cardinal 4 Unknown (CAD VCL writes 2)
          vBits.WriteAppInfoText(cnstAppInfoDataList); //string 2 + 2 * n + 2 App info name
          vBits.WriteRL(3); //Cardinal 4 Unknown (CAD VCL writes 3)
          vBits.WriteBytes(0, 16); //Array of Byte 16 Version data (checksum, CAD VCL writes zeroes)
          vBits.WriteAppInfoText(cnstAppInfoVersion); //string 2 + 2 * n + 2 Version
          vBits.WriteBytes(0, 16); //Array of Byte 16 Comment data (checksum, CAD VCL writes zeroes)
          vBits.WriteAppInfoText(cnstComments); //string 2 + 2 * n + 2 Comment
          vBits.WriteBytes(0, 16); //Array of Byte 16 Product data (checksum, CAD VCL writes zeroes)
          vBits.WriteAppInfoText(vProductInformation); //string 2 + 2 * n + 2 Product
        end;
      acR2007:
        DoError(True, sUnsupportedDWGVer + ' ' + sgDWGVerToString(Version));
    end;
    SaveSectionToStream(GetAppInfoSectionIndex, vBits, [], []);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionAuxHeader(const HandsSeed: UInt64;
  const ADateCreate, ADateUpdate: TDateTime);
var
  vBits: TsgDWGBits;
  vNumSaves: Cardinal;
  vNumSaves1, vNumSaves2: Word;
  vHandSeed: Cardinal;

  procedure WriteVersion;
  begin
    vBits.WriteRS(FOrigFileSavedVer);
    vBits.WriteRS(FMaintenanceVersion);
  end;

begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vBits.WriteRC($FF);
    vBits.WriteRC($77);
    vBits.WriteRC(1);

    WriteVersion;

    vNumSaves := 1;
    vBits.WriteRL(vNumSaves);// number of saves
    vBits.WriteRL(Cardinal(-1));

    vNumSaves1 := 1;
    vNumSaves2 := 0;

    vBits.WriteRS(vNumSaves1); // number of saves part 1
    vBits.WriteRS(vNumSaves2); // number of saves part 2

    vBits.WriteRL0;

    WriteVersion;
    WriteVersion;

    vBits.WriteRS(5);
    vBits.WriteRS($893);
    vBits.WriteRS(5);
    vBits.WriteRS($893);
    vBits.WriteRS(0);
    vBits.WriteRS(1);
    vBits.WriteRL0;
    vBits.WriteRL0;
    vBits.WriteRL0;
    vBits.WriteRL0;
    vBits.WriteRL0;

    WriteDateTimeAsJulian(ADateCreate, vBits, True);

    WriteDateTimeAsJulian(ADateUpdate, vBits, True);

    if HandsSeed < $7fffffff then
      vHandSeed := HandsSeed
    else
      vHandSeed := Cardinal(-1);

    vBits.WriteRL(vHandSeed);
    vBits.WriteRL0;//RL : Educational plot stamp (default value is 0)
    vBits.WriteRS(0);
    vBits.WriteRS(vNumSaves1 - vNumSaves2);
    vBits.WriteRL0;
    vBits.WriteRL0;
    vBits.WriteRL0;
    vBits.WriteRL(vNumSaves);
    vBits.WriteRL0;
    vBits.WriteRL0;
    vBits.WriteRL0;
    vBits.WriteRL0;

    if Version >= acR2018 then
    begin
      vBits.WriteRS(0);
      vBits.WriteRS(0);
      vBits.WriteRS(0);
    end;

    SaveSectionToStream(GetAuxHeaderSectionIndex, vBits, [], []);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.MakeBlockAlias(ABlock: TsgDXFBlock;
  const AName: string; Data: Pointer; ACurrPaperSpaceIndex: Integer = -1);
var
  I: Integer;
  vFlags: Byte;
  vName: string;
  vOwnerDictionary: UInt64;
begin
  vName := AName;
  if HasInternalDimensionBlockName(vName) then
  begin
    vOwnerDictionary := DoHandle;
    AddCreatedElem(cnstDictionary, nil, FCreatedElements,
      cntDWGObjCodeDICTIONARY1, vOwnerDictionary, ABlock.BlockRecord.Handle);
    I := AddCreatedElem(sInternalData, TsgDXFXRecord, FCreatedElements,
      GetDWGCodeByClass(cntClassDXFXRECORD), cnstBadHandle, vOwnerDictionary);
    TsgDXFXRecord(FCreatedElements[I].Obj).Data.AddInt(90, 0);
    TsgDXFXRecord(FCreatedElements[I].Obj).Data.AddString(1, vName);
  end;
  vFlags := ABlock.Flags;
  if GetCorrectBlockFlagsAndName(ABlock, vFlags, vName, Data) then
  begin
    ABlock.Name := vName;
    ABlock.Flags := vFlags;
  end
  else
    ABlock.Name := AName;
  TStringList(Data).AddObject(AName, ABlock);
end;

procedure TsgCADtoDWG.ExportTableBLOCK_RECORD;
var
  vHandles: TsgInt64List;
begin
  vHandles := TsgInt64List.Create;
  try
    vHandles.Sorted := False;
    ProcBlock(nil, GetModelSpaceName, vHandles);
    ProcBlock(nil, GetPaperSpaceName, vHandles);
    ApplyBlocksByProc(ProcBlock, vHandles);
    ProcBlockControl(vHandles);
  finally
    vHandles.Free;
  end;
end;

procedure TsgCADtoDWG.ExportTableDIMSTYLE;
var
  I: Integer;
  vDimStyles: TsgDXFTable;
  vDimStyle: TsgDXFDimensionStyle;
  vHandles: TsgDWGHandleArray;
  vOwnerHandle: UInt64;
begin
  vDimStyle := nil;
  try
    vOwnerHandle := GetDimStyleControlObjectHandle;
    AddCommonHandleRefs(vHandles, cnstBadHandle);
    vDimStyles := TsgDXFTable(Converter.Sections[csDimStyles]);
    if (vDimStyles = nil) or (vDimStyles.FindEntByName(sStandardName) = nil) then
    begin
      vDimStyle := CreateDefaultDimStyle(DoHandle);
      ProcDimStyle(vOwnerHandle, vDimStyle, vHandles);
    end;
    if vDimStyles <> nil then
      for I := 0 to vDimStyles.Count - 1 do
        ProcDimStyle(vOwnerHandle, TsgDXFDimensionStyle(vDimStyles.Entities[I]),
          vHandles);
    ProcDimStylesControlObject(vOwnerHandle, vHandles);
  finally
    vDimStyle.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionClasses(const APaddingSize: Integer);

  procedure WriteClassItem(const ABits: TsgDWGBits; const Ver, ItemClassID: Word;
    const AppName, CPlusPlusClassName, ClassDXFName: string; var ClassNum: Word);
  begin
    if ABits <> nil then
    begin
      ABits.WriteBS(ClassNum);
      ABits.WriteBS(Ver);
      ABits.WriteTV(AppName, FCodePage);
      ABits.WriteTV(CPlusPlusClassName, FCodePage);
      ABits.WriteTV(ClassDXFName, FCodePage);
      ABits.WriteBit(0); //wasazombie
      ABits.WriteBS(ItemClassID);
      case Version of
        acR2004..acR2018:
          begin
            ABits.WriteBL(1); //Number of objects created of this type in the current DB (DXF 91).
            //ABits.WriteBS($16);//  Dwg Version
            //ABits.WriteBS(arDWGVersions[Version]);
            case Version of
              acR2004:
                ABits.WriteBS(arDWGVersions[Version]);
              acR2010, acR2007, acR2013, acR2018: //for DWG versions that are greater than DWG 2004 the number of the DWG Version should be equal 22
                ABits.WriteBS(22);
            end;
            ABits.WriteBS(FMaintenanceVersion);//  Maintenance release version.
            ABits.WriteBL(0);//  Unknown (normally 0L)
            ABits.WriteBL(0);//  Unknown (normally 0L)
          end;
      end;
      FClasses.AddObject(ClassDXFName, TsgObjectWithField.CreateInt(ClassNum));
    end;
    Inc(ClassNum);
  end;

  procedure ExportClasses(const ABits: TsgDWGBits; var ClassNum: Word);
  begin
    ClassNum := 500;
    WriteClassItem(ABits, 127, cntItemClassIDEntity, cntAppNameISM, cntCPlusPlusClassNameRasterImage,
      cntClassDXFIMAGE, ClassNum);
    WriteClassItem(ABits, 0, cntItemClassIDObject, cntAppNameISM, cntCPlusPlusClassNameRasterImageDef,
      cntClassDXFIMAGEDEF, ClassNum);
    WriteClassItem(ABits, 127, cntItemClassIDEntity, cntAppNameWipeOutDWG, cntCPlusPlusClassNameWipeout,
      cntClassDXFWIPEOUT, ClassNum);
    WriteClassItem(ABits, 0, cntItemClassIDObject, cntAppNameWipeOutDWG, cntCPlusPlusClassNameWipeoutVariables,
      cntClassDXFWIPEOUTVARIABLES, ClassNum);
    WriteClassItem(ABits, 3071, cntItemClassIDEntity, cntAppNameAcMPolygonObj15DWG, cntCPlusPlusClassNameMPolygon,
      cntClassDXFMPOLYGON, ClassNum);
    if Converter.Counts[csTableStyles] > 0 then
    begin
      WriteClassItem(ABits, 4095, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassTableStyle,
        cntClassDXFTABLESTYLE, ClassNum);
      WriteClassItem(ABits, 1025, cntItemClassIDEntity, cntAppNameObjectDBX, cntCPlusPlusClassTable,
        cntClassDXFACAD_TABLE, ClassNum);
      WriteClassItem(ABits, 1152, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassTableGeometry,
        cntClassDXFTABLEGEOMETRY, ClassNum);
      WriteClassItem(ABits, 1152, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassCellStyleMap,
        cntClassDXFCELLSTYLEMAP, ClassNum);
    end;
    WriteClassItem(ABits, 1153, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassScale,
      cntClassDXFSCALE, ClassNum);
    WriteClassItem(ABits, 0, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassDictionaryVar,
      cntClassDXFDICTIONARYVAR, ClassNum);
    WriteClassItem(ABits, 0, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassPlaceHolder,
      cntClassDXFACDBPLACEHOLDER, ClassNum);
    WriteClassItem(ABits, 0, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassLayout,
      cntClassDXFLAYOUT, ClassNum);
    WriteClassItem(ABits, 0, cntItemClassIDObject, cntAppNameISM, cntCPlusPlusClassRasterVariables,
      cntClassDXFRASTERVARIABLES, ClassNum);
    if GetSpatialFiltersCount > 0 then
      WriteClassItem(ABits, 0, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassSpatialFilter,
        cntClassDXFSPATIAL_FILTER, ClassNum);
    WriteClassItem(ABits, 0, cntItemClassIDObject, cntAppNameObjectDBX, cntCPlusPlusClassXrecord,
      cntClassDXFXRECORD, ClassNum);
    WriteClassItem(ABits, 1025, cntItemClassIDEntity, cntAppNameObjectDBX, cntCPlusPlusClassArcDimesion,
      cntClassDXFARC_DIMENSION, ClassNum);
    WriteClassItem(ABits, 0, cntItemClassIDEntity, cntAppNameObjectDBX, cntCPlusPlusClassMesh,
      cntClassDXFMESH, ClassNum);
  end;

var
  vBits: TsgDWGBits;
  vClassNum: Word;
  vSectionSizesWriter: TsgDWGSectionSizesWriter;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
//R2010+ (only present if the maintenance version is greater than 3!)
//RL : unknown, possibly the high 32 bits of a 64-bit size?
    if ((Version >= acR2010) and (MaintenanceVersion > 3)) or (Version >= acR2018) then
      vSectionSizesWriter := TsgDWGSectionSizesWriter64.Create(vBits, nil)
    else
      vSectionSizesWriter := TsgDWGSectionSizesWriter.Create(vBits, nil);
    vSectionSizesWriter.BeginSection;
    case Version of
      acR2004..DWGVersionHigh:
        begin
          ExportClasses(nil, vClassNum); // to get the ClassNum value
          vBits.WriteBS(vClassNum - 1);  //must write ClassNum - 1 becouse we increment the ClassNum after WriteClassItem
          vBits.WriteRC(0);
          vBits.WriteRC(0);
          vBits.WriteBit(True);
        end;
    end;
    ExportClasses(vBits, vClassNum);
    vSectionSizesWriter.EndSection;
    vBits.WriteCRC8(cnstClassesSectionCRCInit);
    SaveSectionToStream(GetClassesSectionIndex, vBits, cnstClassesSectionBeginSentinel,
      cnstClassesSectionEndSentinel);
    if APaddingSize > 0 then
      WriteByteToStream(Sections[GetClassesSectionIndex], 0, APaddingSize);
    CreateDictionaryListByStrList(sAcDbVariableDictionary, GetDWGCodeByClass(cntClassDXFDICTIONARYVAR),
      FVariableDictionary);
    CreateDictionaryListByStrList(sAcadScaleListDictionary, GetDWGCodeByClass(cntClassDXFSCALE),
      FScalesDictionary);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionENTITIES;

  procedure IterateLayout(ALayout: TsgDXFLayout);
  var
    I: Integer;
    vEntMode: Byte;
    vData: TsgDirectExportData;
    vEnt: TsgDXFEntity;
  begin
    if ALayout = nil then Exit;
    vEntMode := 2;
    if not ALayout.IsModel then
      vEntMode := 1;
    for I := 0 to ALayout.Count - 1 do
    begin
      vEnt := ALayout.Entities[I];
      if vEnt.EntType = ceMLeader then
        vEnt := TsgCADMultiLeaderAccess(vEnt).RenderObject;
      vData := CreateExportData(cnstBadHandle, vEntMode, GetPrevEntHandle(ALayout, I),
        GetNextEntHandle(ALayout, I));
      DoExport(vEnt, @vData);
    end;
  end;
begin
  IterateLayout(ExportCADImage.Layouts[0]);
  IterateLayout(GetPaperLayout);
end;

procedure TsgCADtoDWG.ExportSectionFileDepList;
const
  cnstAcad = 'Acad:';
  cnstAcadTextStyleFeature = cnstAcad + 'Text';

var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    if Version >= acR2004 then
    begin
      vBits.WriteRL(1);
      vBits.WriteFileDepListText(cnstAcadTextStyleFeature);
      vBits.WriteRL(1);
      vBits.WriteFileDepListText('txt.shx');
      vBits.WriteFileDepListText('');
      vBits.WriteFileDepListText('');
      vBits.WriteFileDepListText('');
      vBits.WriteRL(0);
      vBits.WriteRL(Cardinal(-1));
      vBits.WriteRL(0);
      vBits.WriteRS(0);
      vBits.WriteRL(1);
    end;
    SaveSectionToStream(GetFileDepListSectionIndex, vBits, [], []);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportTableSTYLE;
var
  vStyle: TsgDXFStyle;
  I: Integer;
  vOwnerHandle: UInt64;
  vHandles: TsgDWGHandleArray;
begin
  vOwnerHandle := GetStyleControlObjectHandle;
  AddCommonHandleRefs(vHandles, cnstBadHandle);
  if (Converter.Sections[csStyles] = nil)
    or (TsgDXFTable(Converter.Sections[csStyles]).FindEntByName(sStandardName) = nil) then
  begin
    vStyle := TsgDXFStyle.Create;
    try
      vStyle.Name := sStandardName;
      vStyle.Handle := DoHandle;
      vStyle.FixedHeight := 0;
      vStyle.WidthFactor := 1;
      vStyle.ObliqueAngle := 0;
      vStyle.TextGenFlags := 0;
      vStyle.LastHeightUsed := 10.0;
      vStyle.PrimaryFont := sDefaultSHXFont;
      vStyle.BigFont := '';
      ProcStyle(vOwnerHandle, vStyle);
      AddHandleToArray(CreateDWGHandle(vStyle.Handle, cntDWGObjHandleSoftOwner), vHandles);
    finally
      vStyle.Free;
    end;
  end;
  for I := 0 to Converter.Counts[csStyles] - 1 do
  begin
    vStyle := TsgDXFStyle(Converter.Styles[I]);
    ProcStyle(vOwnerHandle, vStyle);
    AddHandleToArray(CreateDWGHandle(vStyle.Handle, cntDWGObjHandleSoftOwner), vHandles);
  end;
  ProcStyleControl(vOwnerHandle, vHandles);
end;

procedure TsgCADtoDWG.ExportSectionHeaderVars(const AHandSeed: UInt64;
  const ADateCreate, ADateUpdate: TDateTime);
var
  vBits: TsgDWGBits;
  vCEPSNTYPE: Word;
  vFlags: Cardinal;
  vConv: TsgDXFConverter;
  vHeadVar: TsgHeadVarStruct;
  vHandles: TsgDWGHandleArray;
  vSectionSizesWriter: TsgDWGSectionSizesWriter;

  procedure DoWrtHndl(AHandle: UInt64; ACode: Byte);
  begin
    if Version >= acR2007 then
      AddHandleToArray(CreateDWGHandle(AHandle, ACode), vHandles)
    else
      vBits.WriteHandle(AHandle, ACode);
  end;

begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vConv := Converter;
    vHeadVar := vConv.HeadVarStruct;
    if ((Version in [acR2010, acR2013]) and (MaintenanceVersion > 3)) or (Version >= acR2018) then
      vSectionSizesWriter := TsgDWGSectionSizesWriter64.Create(vBits, @vHandles)
    else
      vSectionSizesWriter := TsgDWGSectionSizesWriter.Create(vBits, @vHandles);
    vSectionSizesWriter.BeginSection;
    if Version >= acR2013 then
      vBits.Write3Bits(0);// BLL = 0??

    vBits.WriteBD(412148564080.0);
    vBits.WriteBD(1);
    vBits.WriteBD(1);
    vBits.WriteBD(1);
    vBits.WriteTV('m', FCodePage);
    vBits.WriteTV('', FCodePage);
    vBits.WriteTV('', FCodePage);
    vBits.WriteTV('', FCodePage);
    vBits.WriteBL(24);
    vBits.WriteBL(0);

    if Version in [acR13..acR14] then
      vBits.WriteBL(0);// bs in documentation

    if Version < acR2004 then
      DoWrtHndl(GetCurrentViewportEntityHeaderHandle, cntDWGObjHandleHardPointer); //Handle of the current viewport entity header   Code 5 Handle 0

    vBits.WriteBit(1);//B : DIMASO              1
    vBits.WriteBit(1);//B : DIMSHO              1

    if Version in [acR13..acR14] then
      vBits.WriteBit(0);//B : DIMSAV Undocumented

    vBits.WriteBit(0);//B : PLINEGEN            0
    vBits.WriteBit(0);//B : ORTHOMODE           0
    vBits.WriteBit(1);//B : REGENMODE           1
    vBits.WriteBit(Ord(vHeadVar.FillMode));//B : FILLMODE            1
    vBits.WriteBit(0);//B : QTEXTMODE           0
    vBits.WriteBit(1);//B : PSLTSCALE           1
    vBits.WriteBit(0);//B : LIMCHECK            0

    if Version in [acR13..acR14] then
      vBits.WriteBit(0);//B : BLIPMODE

    if Version >= acR2004 then
      vBits.WriteBit(0);//Undocumented

    vBits.WriteBit(1);//B : USRTIMER (User timer on/off).  1
    vBits.WriteBit(0);//B : SKPOLY                         0
    vBits.WriteBit(0);//B : ANGDIR                         0
    vBits.WriteBit(0);//B : SPLFRAME                       0

    if Version in [acR13..acR14] then
    begin
      vBits.WriteBit(0);//B : ATTREQ
      vBits.WriteBit(0);//B : ATTDIA
    end;

    vBits.WriteBit(0);//B : MIRRTEXT                       0
    vBits.WriteBit(1);//B : WORLDVIEW                      1

    if Version in [acR13..acR14] then
      vBits.WriteBit(0);//B : WIREFRAME Undocumented.

    vBits.WriteBit(Byte(ExportCADImage.CurrentLayout.IsModel));//B : TILEMODE                       1
    vBits.WriteBit(0);//B : PLIMCHECK                      0
    vBits.WriteBit(1);//B : VISRETAIN                      1

    if Version in [acR13..acR14] then
      vBits.WriteBit(0);//B : DELOBJ

    vBits.WriteBit(0);//B : DISPSILH                       0
    vBits.WriteBit(0);//B : PELLIPSE (not present in DXF)  0
    vBits.WriteBS(1);//BS : PROXYGRAPHICS                  1

    if Version in [acR13..acR14] then
      vBits.WriteBS(0);//BS : DRAGMODE

    vBits.WriteBS(3020);//BS : TREEDEPTH                      3020
    vBits.WriteBS(2);//BS : LUNITS                         2
    vBits.WriteBS(4);//BS : LUPREC                         4
    vBits.WriteBS(0);//BS : AUNITS                         0
    vBits.WriteBS(0);//BS : AUPREC                         0

    if Version in [acR13..acR14] then
      vBits.WriteBS(0);//BS : OSMODE

    vBits.WriteBS(Integer(vHeadVar.AttMode));//BS : ATTMODE               1

    if Version in [acR13..acR14] then
      vBits.WriteBS(0);//BS : COORDS

    vBits.WriteBS(vHeadVar.PointDisplayMode);//BS : PDMODE                0

    if Version in [acR13..acR14] then
      vBits.WriteBS(0);//PICKSTYLE

    if Version >= acR2004 then
    begin
      vBits.WriteBL(0);//BL : Unknown
      vBits.WriteBL(0);//BL: Unknown
      vBits.WriteBL(0);//BL : Unknown
    end;

    vBits.WriteBS(0);//BS : USERI1                          0
    vBits.WriteBS(0);//BS : USERI2                          0
    vBits.WriteBS(0);//BS : USERI3                          0
    vBits.WriteBS(0);//BS : USERI4                          0
    vBits.WriteBS(0);//BS : USERI5                          0
    vBits.WriteBS(8);//BS : SPLINESEGS                      8
    vBits.WriteBS(6);//BS : SURFU                           6
    vBits.WriteBS(6);//BS : SURFV                           6
    vBits.WriteBS(6);//BS : SURFTYPE                        6
    vBits.WriteBS(6);//BS : SURFTAB1                        6
    vBits.WriteBS(6);//BS : SURFTAB2                        6
    vBits.WriteBS(6);//BS : SPLINETYPE                      6
    vBits.WriteBS(3);//BS : SHADEDGE                        3
    vBits.WriteBS(70);//BS : SHADEDIF                       70
    vBits.WriteBS(0);//BS : UNITMODE                        0
    vBits.WriteBS(64);//BS : MAXACTVP                       64
    vBits.WriteBS(4);//BS : ISOLINES                        4
    vBits.WriteBS(0);//BS : CMLJUST                         0
    vBits.WriteBS(50);//BS : TEXTQLTY                       50
    vBits.WriteBD(Converter.LTScale);//BD : LTSCALE                     1
    vBits.WriteBD(Converter.HeadVarStruct.TextSize);//BD : TEXTSIZE    2.5
    vBits.WriteBD(1);//BD : TRACEWID                        1
    vBits.WriteBD(1);//BD : SKETCHINC                       1
    vBits.WriteBD(Converter.HeadVarStruct.FilletRadius);//BD : FILLETRAD 0
    vBits.WriteBD(0);//BD : THICKNESS                       0
    vBits.WriteBD(0);//BD : ANGBASE                         0
    vBits.WriteBD(Converter.HeadVarStruct.PointDisplaySize);//BD : PDSIZE                          0
    vBits.WriteBD(0);//BD : PLINEWID                        0
    vBits.WriteBD(0);//BD : USERR1                          0
    vBits.WriteBD(0);//BD : USERR2                          0
    vBits.WriteBD(0);//BD : USERR3                          0
    vBits.WriteBD(0);//BD : USERR4                          0
    vBits.WriteBD(0);//BD : USERR5                          0
    vBits.WriteBD(0);//BD : CHAMFERA                        0
    vBits.WriteBD(0);//BD : CHAMFERB                        0
    vBits.WriteBD(0);//BD : CHAMFERC                        0
    vBits.WriteBD(0);//BD : CHAMFERD                        0
    vBits.WriteBD(0.5);//BD : FACETRES                      0.5
    vBits.WriteBD(20);//BD : CMLSCALE                       20
    vBits.WriteBD(Converter.HeadVarStruct.CELTScale);//BD : CELTSCALE                       1

    if Version in [acR13 .. DWGVersionHigh] then
      vBits.WriteTV('.', FCodePage);//TV : MENUNAME                     '.'

    //BL : TDCREATE (Julian day)                 2455958
    //BL : TDCREATE (Milliseconds into the day)  39819107
    WriteDateTimeAsJulian(ADateCreate, vBits, False);

    //BL : TDUPDATE (Julian day)                 2455958
    //BL : TDUPDATE (Milliseconds into the day)  39887280
    WriteDateTimeAsJulian(ADateUpdate, vBits, False);

    if Version >= acR2004 then
    begin
      vBits.WriteBL(0);//BL : Unknown
      vBits.WriteBL(0);//BL : Unknown
      vBits.WriteBL(0);//BL : Unknown
    end;

    //TDINDWG (Days)
    //TDINDWG (Milliseconds into the day)
    vBits.WriteJulianDateInt(Converter.DrwPropTotalEditingTime, True, True);
//    vBits.WriteBL(0);//BL : TDINDWG (Days)                          0
//    vBits.WriteBL(vMJDMillSecUpdate - vMJDMillSecCreate);//BL : TDINDWG (Milliseconds into the day)     68640
    //TDUSRTIMER (Days)                       0
    //TDUSRTIMER (Milliseconds into the day)  68640
    vBits.WriteJulianDateInt(CreateTimeStamp, True, True);
//    vBits.WriteBL(0);//BL : TDUSRTIMER (Days)                       0
//    vBits.WriteBL(0);//BL : TDUSRTIMER (Milliseconds into the day)  68640
    vBits.WriteCMC(vHeadVar.CEColor);//CMC : CECOLOR clDXFByLayer

    vBits.WriteEntHandle(AHandSeed, cntDWGObjHandleType0);//H : HANDSEED The next handle, with an 8-bit length specifier preceding the handle bytes (standard hex handle form) (code 0)
    DoWrtHndl(CastEntToID(vConv.LayerByName(vHeadVar.CLayer)), cntDWGObjHandleHardPointer); //H : CLAYER (hard pointer)       Code 5, Handle 16
    DoWrtHndl(CastEntToID(vConv.StyleByName(vHeadVar.TextStyle)), cntDWGObjHandleHardPointer);//H : TEXTSTYLE (hard pointer)    Code 5, Handle 17
    DoWrtHndl(CastEntToID(vConv.LTypeByName(vHeadVar.CELType)), cntDWGObjHandleHardPointer);//H : CELTYPE (hard pointer)      Code 5, Handle 21

    if Version >= acR2007 then
      DoWrtHndl(cnstBadHandle, cntDWGObjHandleHardPointer);//H : CMATERIAL (hard pointer)

    DoWrtHndl(CastEntToID(vConv.DimensionStyleByName(vHeadVar.DimStyle)), cntDWGObjHandleHardPointer);//H : DIMSTYLE (hard pointer)     Code 5, Handle 39
    DoWrtHndl(GetMLineStyleHandle, cntDWGObjHandleHardPointer); //H : CMLSTYLE (hard pointer)  multilyne style  Code 5, Handle 24

    if Version >= acR2000 then
      vBits.WriteBD(0);// BD : PSVPSCALE                            0

    vBits.Write3BD(cnstFPointZero);//3BD : INSBASE (PSPACE)                    0, 0, 0
    vBits.Write3BD(vHeadVar.PExtMin);//3BD : EXTMIN (PSPACE)                     0, 0, 0  ODA 1e+20, 1e+20, 1e+20
    vBits.Write3BD(vHeadVar.PExtMax);//3BD : EXTMAX (PSPACE)                     0, 0, 0  ODA -1e+20, -1e+20, -1e+20
    vBits.Write2RD(vHeadVar.PLimMin);//2RD : LIMMIN (PSPACE)                     0, 0, 0
    vBits.Write2RD(vHeadVar.PLimMax);//2RD : LIMMAX (PSPACE)                 12, 9, 0
    vBits.WriteBD(0);//BD : ELEVATION (PSPACE)                    0

    vBits.Write3BD(cnstFPointZero);//3BD : UCSORG (PSPACE)                     0, 0, 0
    vBits.Write3BD(cnstXOrtAxis);//3BD : UCSXDIR (PSPACE)                    1, 0, 0
    vBits.Write3BD(cnstYOrtAxis);//3BD : UCSYDIR (PSPACE)                    0, 1, 0
    DoWrtHndl(cnstBadHandle, cntDWGObjHandleHardPointer);//H : UCSNAME (PSPACE) (hard pointer)  Code 5, Handle 0

    if Version >= acR2000 then
    begin
      DoWrtHndl(cnstBadHandle, cntDWGObjHandleHardPointer);//H : PUCSORTHOREF (hard pointer)      Code 5, Handle 0
      vBits.WriteBS(0);//BS : PUCSORTHOVIEW                              0
      DoWrtHndl(cnstBadHandle, cntDWGObjHandleHardPointer);//H : PUCSBASE (hard pointer)          Code 5, Handle 0
      vBits.Write3BD(cnstFPointZero);//3BD : PUCSORGTOP                          0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : PUCSORGBOTTOM                       0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : PUCSORGLEFT                         0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : PUCSORGRIGHT                        0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : PUCSORGFRONT                        0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : PUCSORGBACK                         0, 0, 0
    end;

    vBits.Write3BD(vHeadVar.InsBase);//3BD : INSBASE (MSPACE)                    0, 0, 0
    vBits.Write3BD(vHeadVar.ExtMin);//3BD : EXTMIN (MSPACE)                     1239.47030326644, 1029.17491819938, 0  ODA 1e+20, 1e+20, 1e+20
    vBits.Write3BD(vHeadVar.ExtMax);//3BD : EXTMAX (MSPACE)                     1876.89217864485, 1387.20749547897, 0  ODA -1e+20, -1e+20, -1e+20
    vBits.Write2RD(vHeadVar.LimMin);//2RD : LIMMIN (MSPACE)                     0, 0, 0
    vBits.Write2RD(vHeadVar.LimMax);//2RD : LIMMAX (MSPACE)                     420, 297, 0                             ODA  12, 9, 0 !!!

    vBits.WriteBD(0);//BD : ELEVATION (MSPACE)                          0
    vBits.Write3BD(vHeadVar.UCSORG);//3BD : UCSORG (MSPACE)                   0, 0, 0
    vBits.Write3BD(vHeadVar.UCSXDir);//3BD : UCSXDIR (MSPACE)                    1, 0, 0
    vBits.Write3BD(vHeadVar.UCSYDir);//3BD : UCSYDIR (MSPACE)                    0, 1, 0
    DoWrtHndl(cnstBadHandle, cntDWGObjHandleHardPointer);//H : UCSNAME (MSPACE) (hard pointer) Code 5, Handle 0

    if Version >= acR2000 then
    begin
      DoWrtHndl(cnstBadHandle, cntDWGObjHandleHardPointer);//H : UCSORTHOREF (hard pointer)      Code 5, Handle 0
      vBits.WriteBS(0);//BS : UCSORTHOVIEW                              0
      DoWrtHndl(cnstBadHandle, cntDWGObjHandleHardPointer);//H : UCSBASE (hard pointer)          Code 5, Handle 0
      vBits.Write3BD(cnstFPointZero);//3BD : UCSORGTOP                               0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : UCSORGBOTTOM                            0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : UCSORGLEFT                              0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : UCSORGRIGHT                             0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : UCSORGFRONT                             0, 0, 0
      vBits.Write3BD(cnstFPointZero);//3BD : UCSORGBACK                              0, 0, 0
      if Version >= acR2007 then
      begin
        vBits.WriteTV(vHeadVar.DimProps.APost, FCodePage);//see procedure TdwgReader.ReadHeadVars(ASeek,ASize: Integer);
        vBits.WriteTV(vHeadVar.DimProps.Post, FCodePage);//
      end
      else
      begin
        vBits.WriteTV(vHeadVar.DimProps.Post, FCodePage);//TV : DIMPOST               ''
        vBits.WriteTV(vHeadVar.DimProps.APost, FCodePage);//TV : DIMAPOST             ''
      end;
    end;

    if Version in [acR13..acR14] then
    begin
      vBits.WriteBit(0);//B : DIMTOL
      vBits.WriteBit(0);//B : DIMLIM
      vBits.WriteBit(vHeadVar.DimProps.Tih);//B : DIMTIH
      vBits.WriteBit(vHeadVar.DimProps.Toh);//B : DIMTOH
      vBits.WriteBit(vHeadVar.DimProps.SE1);//B : DIMSE1
      vBits.WriteBit(vHeadVar.DimProps.SE1);//B : DIMSE2
      vBits.WriteBit(vHeadVar.DimProps.Alt);//B : DIMALT
      vBits.WriteBit(0);//B : DIMTOFL
      vBits.WriteBit(0);//B : DIMSAH
      vBits.WriteBit(vHeadVar.DimProps.Tix <> 0);//B : DIMTIX
      vBits.WriteBit(0);//B : DIMSOXD
      vBits.WriteRC(0);//RC : DIMALTD
      vBits.WriteRC(0);//RC : DIMZIN
      vBits.WriteBit(0);//B : DIMSD1
      vBits.WriteBit(0);//B : DIMSD2
      vBits.WriteRC(0);//RC : DIMTOLJ
      vBits.WriteRC(0);//RC : DIMJUST
      vBits.WriteRC(0);//RC : DIMFIT
      vBits.WriteBit(0);//B : DIMUPT
      vBits.WriteRC(0);//RC : DIMTZIN
      vBits.WriteRC(0);//RC : DIMALTZ
      vBits.WriteRC(0);//RC : DIMALTTZ
      vBits.WriteRC(vHeadVar.DimProps.Tad);//RC : DIMTAD
      vBits.WriteBS(0);//BS : DIMUNIT
      vBits.WriteBS(0);//BS : DIMAUNIT
      vBits.WriteBS(vHeadVar.DimProps.Dec);//BS : DIMDEC
      vBits.WriteBS(0);//BS : DIMTDEC
      vBits.WriteBS(0);//BS : DIMALTU
      vBits.WriteBS(0);//BS : DIMALTTD
      DoWrtHndl(CastEntToID(vConv.StyleByName(vHeadVar.DimTextStyle)), cntDWGObjHandleHardPointer); //H : DIMTXSTY (hard pointer)
    end;

    vBits.WriteBD(vHeadVar.DimProps.Scale);//BD : DIMSCALE              1
    vBits.WriteBD(vHeadVar.DimProps.Asz);//BD : DIMASZ                  2.5
    vBits.WriteBD(vHeadVar.DimProps.Exo);//BD : DIMEXO                  0.625
    vBits.WriteBD(3.75);//BD : DIMDLI                                   3.75 !!!
    vBits.WriteBD(vHeadVar.DimProps.Exe);//BD : DIMEXE                  1.25
    vBits.WriteBD(0);//BD : DIMRND                                      0    !!!
    vBits.WriteBD(0);//BD : DIMDLE                                      0    !!!
    vBits.WriteBD(vHeadVar.DimProps.Tp);//BD : DIMTP                    0    !!!
    vBits.WriteBD(vHeadVar.DimProps.Tm);//BD : DIMTM                                       0    !!!

    if Version >= acR2007 then
    begin
      vBits.WriteBD(0);//BD : DIMFXL
      vBits.WriteBD(0.785398);//BD : DIMJOGANG
      vBits.WriteBS(0);//BS : DIMTFILL
      vBits.WriteCMC(cnstDefaultEntityColor);//CMC : DIMTFILLCLR
    end;

    if Version >= acR2000 then
    begin
      vBits.WriteBit(0);//B : DIMTOL                                     0  !!!
      vBits.WriteBit(0);//B : DIMLIM                                     0  !!!
      vBits.WriteBit(vHeadVar.DimProps.Tih);//B : DIMTIH      0
      vBits.WriteBit(vHeadVar.DimProps.Toh);//B : DIMTOH      0
      vBits.WriteBit(vHeadVar.DimProps.SE1);//B : DIMSE1      0
      vBits.WriteBit(vHeadVar.DimProps.SE2);//B : DIMSE2      0
      vBits.WriteBS(vHeadVar.DimProps.Tad);//BS : DIMTAD                 1
      vBits.WriteBS(8);//BS : DIMZIN                                     8 !!!
      vBits.WriteBS(0);//BS : DIMAZIN                                    0 !!!
    end;

    if Version >= acR2007 then
      vBits.WriteBS(0);//BS : DIMARCSYM

    vBits.WriteBD(GetDefVal(vHeadVar.DimProps.Txt, 2.5));//BD : DIMTXT               2.5
    vBits.WriteBD(vHeadVar.DimProps.Cen);//BD : DIMCEN               2.5
    vBits.WriteBD(0);//BD : DIMTSZ                                     0  !!!
    vBits.WriteBD(vHeadVar.DimProps.AltF);//BD : DIMALTF       0.03937007874016
    vBits.WriteBD(GetDefVal(vHeadVar.DimProps.LFac, 1));//BD : DIMLFAC               1
    vBits.WriteBD(0);//BD : DIMTVP                                     0  !!!
    vBits.WriteBD(1);//BD : DIMTFAC                                    1  !!!
    vBits.WriteBD(vHeadVar.DimProps.Gap);//BD : DIMGAP                0.625

    if Version in [acR13..acR14] then
    begin
      vBits.WriteT(vHeadVar.DimProps.Post, FCodePage);//T : DIMPOST
      vBits.WriteT(vHeadVar.DimProps.APost, FCodePage);//T : DIMAPOST
      vBits.WriteT(GetNameByArrowType(Converter.HeadVarStruct.DimProps.Arrows.Blk,
        sgDimensionArrowTypeNames[Low(sgDimensionArrowTypeNames)]), FCodePage);//T : DIMBLK
      vBits.WriteT(GetNameByArrowType(Converter.HeadVarStruct.DimProps.Arrows.Blk1,
        sgDimensionArrowTypeNames[Low(sgDimensionArrowTypeNames)]), FCodePage);//T : DIMBLK1
      vBits.WriteT(GetNameByArrowType(Converter.HeadVarStruct.DimProps.Arrows.Blk2,
        sgDimensionArrowTypeNames[Low(sgDimensionArrowTypeNames)]), FCodePage);//T : DIMBLK2
    end;

    if Version >= acR2000 then
    begin
      vBits.WriteBD(0);//BD : DIMALTRND                                  0 !!!
      vBits.WriteBit(vHeadVar.DimProps.Alt);//B : DIMALT      0
      vBits.WriteBS(3);//BS : DIMALTD                                    3 !!!
      vBits.WriteBit(1);//B : DIMTOFL                                    1 !!!
      vBits.WriteBit(0);//B : DIMSAH                                     0 !!!
      vBits.WriteBit(vHeadVar.DimProps.Tix <> 0);//B : DIMTIX                 0
      vBits.WriteBit(0);//B : DIMSOXD                                    0 !!!
    end;

    vBits.WriteCMC(vHeadVar.DimProps.ClrD);//CMC : DIMCLRD         clDXFByBlock
    vBits.WriteCMC(vHeadVar.DimProps.ClrE);//CMC : DIMCLRE         clDXFByBlock
    vBits.WriteCMC(vHeadVar.DimProps.ClrT);//CMC : DIMCLRT         clDXFByBlock

    if Version >= acR2000 then
    begin
      vBits.WriteBS(0);//BS : DIMADEC                                    0 !!!
      vBits.WriteBS(vHeadVar.DimProps.Dec);//BS : DIMDEC                 2
      vBits.WriteBS(2);//BS : DIMTDEC                                    2
      vBits.WriteBS(2);//BS : DIMALTU                                    2
      vBits.WriteBS(3);//BS : DIMALTTD                                   3
      vBits.WriteBS(0);//BS : DIMAUNIT                                   0
      vBits.WriteBS(vHeadVar.DimProps.Frac);//BS : DIMFRAC               0
      vBits.WriteBS(GetDimLimitUnitsByte(vHeadVar.DimProps.LUnit));//BS : DIMLUNIT 2
      vBits.WriteBS(Ord(vHeadVar.DimProps.DSep));//BS : DIMDSEP          44
      vBits.WriteBS(0);//BS : DIMTMOVE                                   0
      vBits.WriteBS(0);//BS : DIMJUST                                    0
      vBits.WriteBit(vHeadVar.DimProps.SD1);//B : DIMSD1      0
      vBits.WriteBit(vHeadVar.DimProps.SD2);//B : DIMSD2      0
      vBits.WriteBS(0);//BS : DIMTOLJ                                    0
      vBits.WriteBS(8);//BS : DIMTZIN                                    8
      vBits.WriteBS(0);//BS : DIMALTZ                                    0
      vBits.WriteBS(0);//BS : DIMALTTZ                                   0
      vBits.WriteBit(0);//B : DIMUPT                                     0
      vBits.WriteBS(3);//BS : DIMATFIT                                   3
    end;

    if Version >= acR2007 then
      vBits.WriteBit(0);//B : DIMFXLON

    if Version >= acR2010 then
    begin
      vBits.WriteBit(0);//B : DIMTXTDIRECTION
      vBits.WriteBD(0);//BD : DIMALTMZF
      vBits.WriteT('', FCodePage);//T : DIMALTMZS
      vBits.WriteBD(0);//BD : DIMMZF
      vBits.WriteT('', FCodePage);//T : DIMMZS
    end;

    if Version >= acR2000 then
    begin
      DoWrtHndl(CastEntToID(vConv.StyleByName(vHeadVar.DimTextStyle)), cntDWGObjHandleHardPointer);//H : DIMTXSTY (hard pointer)               Code 5 Handle 17
      DoWrtHndl(GetArrowBlockHandle(vHeadVar.DimProps.Arrows.LrBlk), cntDWGObjHandleHardPointer);//H : DIMLDRBLK (hard pointer)           Code 5 Handle 0
      DoWrtHndl(GetArrowBlockHandle(vHeadVar.DimProps.Arrows.Blk), cntDWGObjHandleHardPointer);//H : DIMBLK (hard pointer)                Code 5 Handle 0
      DoWrtHndl(GetArrowBlockHandle(vHeadVar.DimProps.Arrows.Blk1), cntDWGObjHandleHardPointer);//H : DIMBLK1 (hard pointer)              Code 5 Handle 0
      DoWrtHndl(GetArrowBlockHandle(vHeadVar.DimProps.Arrows.Blk2), cntDWGObjHandleHardPointer);//H : DIMBLK2 (hard pointer)              Code 5 Handle 0
    end;

    if Version >= acR2007 then
    begin
      DoWrtHndl(cnstBadHandle, cntDWGObjHandleHardPointer);//H : DIMLTYPE (hard pointer)
      DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : DIMLTEX1 (hard pointer)
      DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : DIMLTEX2 (hard pointer)
    end;

    if Version >= acR2000 then
    begin
      vBits.WriteBS(CorrectLineWeight(vHeadVar.DimProps.LwD, False));//BS : DIMLWD                   65534
      vBits.WriteBS(CorrectLineWeight(vHeadVar.DimProps.LwE, False));//BS : DIMLWE                   65534
    end;

    DoWrtHndl(GetBlockControlObjectHandle, cntDWGObjHandleHardOwner);//H : BLOCK CONTROL OBJECT (hard owner)           Code 3 Handle 35
    DoWrtHndl(GetLayerControlObjectHandle, cntDWGObjHandleHardOwner);//H : LAYER CONTROL OBJECT (hard owner)           Code 3 Handle 11
    DoWrtHndl(GetStyleControlObjectHandle, cntDWGObjHandleHardOwner);//H : STYLE CONTROL OBJECT (hard owner)           Code 3 Handle 12
    DoWrtHndl(GetLineTypeControlObjectHandle, cntDWGObjHandleHardOwner);//H : LINETYPE CONTROL OBJECT (hard owner)     Code 3 Handle 10
    DoWrtHndl(GetViewControlObjectHandle, cntDWGObjHandleHardOwner);//H : VIEW CONTROL OBJECT (hard owner)             Code 3 Handle 42
    DoWrtHndl(GetUCSontrolObjectHandle, cntDWGObjHandleHardOwner);//H : UCS CONTROL OBJECT (hard owner)                Code 3 Handle 43
    DoWrtHndl(GetVPortControlObjectHandle, cntDWGObjHandleHardOwner);//H : VPORT CONTROL OBJECT (hard owner)           Code 3 Handle 40
    DoWrtHndl(GetAppIDControlObjectHandle, cntDWGObjHandleHardOwner);//H : APPID CONTROL OBJECT (hard owner)           Code 3 Handle 32
    DoWrtHndl(GetDimStyleControlObjectHandle, cntDWGObjHandleHardOwner);//H : DIMSTYLE CONTROL OBJECT (hard owner)     Code 3 Handle 13

    if Version in [acR13..acR2000] then
      DoWrtHndl(GetViewPortEntityHeaderControlObjectHandle, cntDWGObjHandleHardOwner);//H : VIEWPORT ENTITY HEADER CONTROL OBJECT     Code 3 Handle 497

    DoWrtHndl(GetDictionaryAcadGroupHandle, cntDWGObjHandleHardPointer);//H : DICTIONARY (ACAD_GROUP) (hard pointer)             Code 5 Handle 51
    DoWrtHndl(GetDictionaryAcadMLineStyleHandle, cntDWGObjHandleHardPointer);//H : DICTIONARY (ACAD_MLINESTYLE) (hard pointer)   Code 5 Handle 54
    DoWrtHndl(GetDictionaryNamedObjectHandle, cntDWGObjHandleHardOwner);//H : DICTIONARY (NAMED OBJECTS) (hard owner)          Code 3 Handle 50

    if Version >= acR2000 then
    begin
      vBits.WriteBS(1);//BS : TSTACKALIGN, default = 1 (not present in DXF)            1
      vBits.WriteBS(70);//BS : TSTACKSIZE, default = 70 (not present in DXF)           70
      vBits.WriteTV('', FCodePage);//TV : HYPERLINKBASE                                           ''
      vBits.WriteTV('', FCodePage);//TV : STYLESHEET                                              ''
      DoWrtHndl(GetDictionaryLayoutsHandle, cntDWGObjHandleHardPointer);//H : DICTIONARY (LAYOUTS) (hard pointer)                Code 5 Handle 57
      DoWrtHndl(GetDictionaryPlotSettingHandle, cntDWGObjHandleHardPointer);//H : DICTIONARY (PLOTSETTINGS) (hard pointer)       Code 5 Handle 0
      DoWrtHndl(GetDictionaryPlotStyleHandle, cntDWGObjHandleHardPointer);//H : DICTIONARY (PLOTSTYLES) (hard pointer)           Code 5 Handle 0
    end;

    if Version >= acR2004 then
    begin
      DoWrtHndl(GetDictionaryAcadMaterialsHandle, cntDWGObjHandleHardPointer);//H : DICTIONARY (MATERIALS) (hard pointer)
      DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : DICTIONARY (COLORS) (hard pointer)
    end;

    if Version >= acR2007 then
      DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : DICTIONARY (VISUALSTYLE) (hard pointer)

    if Version >= acR2013 then
      DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : UNKNOWN (hard pointer)

    if Version >= acR2000 then
    begin
      vFlags := $2800;//we use only CELWEIGHT flag, other flags are default values $2A00
      if vHeadVar.LwDisplay = 0 then
        vFlags := vFlags or $0200;//$2A00
      vFlags := Cardinal((CorrectLineWeight(Converter.HeadVarStruct.CELWeight, True) and $1F)) or vFlags;
      vBits.WriteBL(vFlags);//just use BL : 10781
        //Flags:
          //CELWEIGHT Flags & 0x001F
          //ENDCAPS Flags & 0x0060
          //JOINSTYLE Flags & 0x0180
          //LWDISPLAY !(Flags & 0x0200)
          //XEDIT !(Flags & 0x0400)
          //EXTNAMES Flags & 0x0800
          //PSTYLEMODE Flags & 0x2000
          //OLESTARTUP Flags & 0x4000
      vBits.WriteBS(vHeadVar.InsUnits);//BS : INSUNITS     4
      vCEPSNTYPE := 0;
      vBits.WriteBS(vCEPSNTYPE);//BS : CEPSNTYPE                    0
      if vCEPSNTYPE = 3 then
        DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : CPSNID (present only if CEPSNTYPE == 3) (hard pointer)
      vBits.WriteTV(GetFingerPrintGUID, FCodePage);//TV : FINGERPRINTGUID    {F536BED7-6F9E-468E-8820-242279AE2C2C}     ODA {E44D33DF-D2B9-48AE-8431-27F629E25DDD}
      vBits.WriteTV(GetVersionGUID, FCodePage);//TV : VERSIONGUID        {4C62D578-729E-4900-B98E-2D837BD6EAD7}     ODA {FAEB1C32-E019-11D5-929B-00C0DF256EC4}
    end;

    if Version >= acR2004 then
    begin
      vBits.WriteRC(0);//RC : SORTENTS
      vBits.WriteRC(0);//RC : INDEXCTL
      vBits.WriteRC(0);//RC : HIDETEXT
      vBits.WriteRC(vHeadVar.XClipFrame);//RC : XCLIPFRAME
      vBits.WriteRC(vHeadVar.DimAssoc);//RC : DIMASSOC
      vBits.WriteRC(0);//RC : HALOGAP
      vBits.WriteBS(0);//BS : OBSCUREDCOLOR
      vBits.WriteBS(0);//BS : INTERSECTIONCOLOR
      vBits.WriteRC(0);//RC : OBSCUREDLTYPE
      vBits.WriteRC(0);//RC : INTERSECTIONDISPLAY
      vBits.WriteTV('', FCodePage);//TV : PROJECTNAME
    end;

    DoWrtHndl(PaperSpaceBlockHandle, cntDWGObjHandleHardPointer);//H : BLOCK_RECORD (*PAPER_SPACE) (hard pointer)   Code 5 Handle 37
    DoWrtHndl(ModelSpaceBlockHandle, cntDWGObjHandleHardPointer);//H : BLOCK_RECORD (*MODEL_SPACE) (hard pointer)   Code 5 Handle 36
    DoWrtHndl(CastEntToID(vConv.LTypeByName(sByLayer)), cntDWGObjHandleHardPointer);//H : LTYPE (BYLAYER) (hard pointer)           Code 5 Handle 1
    DoWrtHndl(CastEntToID(vConv.LTypeByName(sByBlock)), cntDWGObjHandleHardPointer);//H : LTYPE (BYBLOCK) (hard pointer)           Code 5 Handle 2
    DoWrtHndl(CastEntToID(vConv.LTypeByName(sContinuous)), cntDWGObjHandleHardPointer);//H : LTYPE (CONTINUOUS) (hard pointer)     Code 5 Handle 3

    if Version >= acR2007 then
    begin
      vBits.WriteBit(0);//B : CAMERADISPLAY
      vBits.WriteBL(0);//BL : unknown
      vBits.WriteBL(0);//BL : unknown
      vBits.WriteBD(0);//BD : unknown
      vBits.WriteBD(2);//BD : STEPSPERSEC
      vBits.WriteBD(6);//BD : STEPSIZE
      vBits.WriteBD(2);//BD : 3DDWFPREC
      vBits.WriteBD(50);//BD : LENSLENGTH
      vBits.WriteBD(0);//BD : CAMERAHEIGHT
      vBits.WriteRC(0);//RC : SOLIDHIST
      vBits.WriteRC(0);//RC : SHOWHIST
      vBits.WriteBD(5);//BD : PSOLWIDTH
      vBits.WriteBD(80);//BD : PSOLHEIGHT
      vBits.WriteBD(0);//BD : LOFTANG1
      vBits.WriteBD(0);//BD : LOFTANG2
      vBits.WriteBD(0);//BD : LOFTMAG1
      vBits.WriteBD(0);//BD : LOFTMAG2
      vBits.WriteBS(0);//BS : LOFTPARAM
      vBits.WriteRC(0);//RC : LOFTNORMALS
      vBits.WriteBD(0);//BD : LATITUDE
      vBits.WriteBD(0);//BD : LONGITUDE
      vBits.WriteBD(0);//BD : NORTHDIRECTION
      vBits.WriteBL(0);//BL : TIMEZONE
      vBits.WriteRC(0);//RC : LIGHTGLYPHDISPLAY
      vBits.WriteRC(0);//RC : TILEMODELIGHTSYNCH
      vBits.WriteRC(0);//RC : DWFFRAME
      vBits.WriteRC(0);//RC : DGNFRAME
      vBits.WriteBit(0);//B : unknown
      vBits.WriteCMC(cnstDefaultEntityColor);//CMC : INTERFERECOLOR
      DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : INTERFEREOBJVS (hard pointer)
      DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : INTERFEREVPVS (hard pointer)
      DoWrtHndl(cnstBadHandle,  cntDWGObjHandleHardPointer);//H : DRAGVS (hard pointer)
      vBits.WriteRC(0);//RC : CSHADOW
      vBits.WriteBD(0);//BD SHADOWPLANELOCATION
    end;
    if Version >= acR14 then
    begin
      vBits.WriteBS($FFFF);//BS : unknown short (type 5/6 only) these do not seem to be required,  65535
      vBits.WriteBS($FFFF);//BS : unknown short (type 5/6 only) even for type 5.                   65535
      vBits.WriteBS($FFFF);//BS : unknown short (type 5/6 only)                                    65535
      vBits.WriteBS($FFFF);//BS : unknown short (type 5/6 only)                                    65535
    end;
    vSectionSizesWriter.EndSection;
    vBits.WriteCRC8(cnstHeaderVarCRCInit);//RS : CRC for the data section, starting after the sentinel. Use 0xC0C1 for the initial value.*)
    SaveSectionToStream(GetHeaderVarsSectionIndex, vBits, cnstHeaderVariablesBeginSentinel,
      cnstHeaderVariablesEndSentinel);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionMeasurement(const Measurement: Boolean);
begin
  if Measurement then
    ExportSectionMeasurement(1)
  else
    ExportSectionMeasurement(0);
end;

procedure TsgCADtoDWG.ExportSectionMeasurement(const Measurement: Word);
var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vBits.WriteRS(0);
    vBits.WriteRS(Measurement);
    SaveSectionToStream(GetMeasurementSectionIndex, vBits, [], []);
  finally
    vBits.Free
  end;
end;

procedure TsgCADtoDWG.ExportSectionObjects;
var
  vValue: Cardinal;
begin
  Sections[GetObjectsSectionIndex].Clear;
  if Version > acR2000 then
  begin
    vValue := $DCA;
    Sections[GetObjectsSectionIndex].Write(vValue, SizeOf(vValue)); //RL
  end;
end;

procedure TsgCADtoDWG.ExportSectionObjFreeSpace(const ADateUpdate: TDateTime;
  const ObjCount, ObjOffset: Cardinal);
const
  cnstSignature: AnsiString = 'CADSoftTools';
var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vBits.WriteRL0;
    vBits.WriteRL(ObjCount);
    WriteDateTimeAsJulian(ADateUpdate, vBits, True);
    vBits.WriteRL(ObjOffset); //Offset of the objects section in the stream
    if Version = acR2000 then
    begin
     //CADSoftTools signature 'CADSoftTools'
      vBits.WriteRC(4); //4 UInt64's
      vBits.WriteBytes(@cnstSignature[1], Length(cnstSignature));
      vBits.WriteBytes(0, SizeOf(UInt64) * 4 - Length(cnstSignature));
    end
    else
    begin
      vBits.WriteRC(4);
      vBits.WriteRL(50);
      vBits.WriteRL(0);
      vBits.WriteRL(100);
      vBits.WriteRL(0);
      vBits.WriteRL(0);
      vBits.WriteRL(0);
      vBits.WriteRL(Cardinal(-1));
      vBits.WriteRL(0);
    end;
    SaveSectionToStream(GetObjFreeSpaceSectionIndex, vBits, [], []);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionObjMap;
var
  I: Integer;
  vOffset, vPrevLoc: Integer;
  vPrevHandle: UInt64;
  vBits: TsgDWGBits;

  procedure Save;
  begin
    vBits.UpdateRS(0, SwapBytes(vBits.Size));
    vBits.WriteCRC8(cnstSectionObjMapCRCInit, True);
    vBits.SaveToStream(Sections[GetObjectMapSectionIndex]);
    vBits.Clear;
  end;

  procedure CheckAndSplit(AFirst, ALast: Boolean); //each section is cut off at a maximum length of 2032
  var
    vFlag: Boolean;
  begin
    vFlag := vBits.Size >= cnstSectionObjMapMaxSize;
    if AFirst or ALast or vFlag then
    begin
      if (not AFirst) and (ALast or vFlag) then Save;
      vPrevHandle := 0;
      vPrevLoc := 0;
      vBits.WriteRS(Word(cntSizeTempValue));
    end;
  end;

begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    if Version <= acR2000 then
      vOffset := GetObjectsFileOffset
    else
      vOffset := 0; //RL
    Sections[GetObjectMapSectionIndex].Clear;
    FObjectsMap.SortBy := seHandle;
    FObjectsMap.Sort;
    CheckAndSplit(True, False);
    for I := 0 to FObjectsMap.Count - 1 do
    begin
      CheckAndSplit(False, False);
      vBits.WriteMCHandle(FObjectsMap[I].Handle - vPrevHandle);
      vBits.WriteMC(FObjectsMap[I].Location + vOffset - vPrevLoc);
      vPrevHandle := FObjectsMap[I].Handle;
      vPrevLoc := FObjectsMap[I].Location + vOffset;
    end;
    CheckAndSplit(False, True);
    Save;
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionPreview(const ImageOffset: Cardinal; var HeaderOffsetPos, HeaderOffsetValue,
  BmpOffsetPos, BmpOffsetPosValue: Cardinal);
var
  vBits: TsgDWGBits;
  vBitmap: TBitmap;
  P: PByte;
  vStream: TMemoryStream;
  vHeaderSize, vBmpDataSize: Cardinal;

  function GetRealPos: Cardinal;
  begin
    Result := vBits.Position shr cntMulDiv8 + ImageOffset;
  end;

begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vBitmap := TBitmap.Create;
    try
      vBitmap.PixelFormat := pf8bit;
      vBitmap.Width := GetPreviewImageWidth;
      vBitmap.Height := GetPreviewImageHeigth;
  {$IFDEF SG_FIREMONKEY}
      try
        vBitmap.Canvas.BeginScene;
  {$ENDIF}
      vBitmap.Canvas.StretchDraw(Rect(0, 0, vBitmap.Width, vBitmap.Height),
        ExportCADImage);
  {$IFDEF SG_FIREMONKEY}
      finally
        vBitmap.Canvas.EndScene;
      end;
  {$ENDIF}
      vStream := TMemoryStream.Create;
      try
        vBitmap.SaveToStream(vStream);
        P := vStream.Memory;
        Inc(P, SizeOf(TBitmapFileHeader));
        vBits.WriteRL(cntSizeTempValue); //size will be written later
        vBits.WriteRC(2);
        vBits.WriteRC(1);
        vHeaderSize := 80;
        HeaderOffsetPos := vBits.Position;
        vBits.WriteRL(cntOffsetTempValue);//Header offset
        vBits.WriteRL(vHeaderSize);
        vBits.WriteRC(2);
        BmpOffsetPos := vBits.Position;
        vBits.WriteRL(cntOffsetTempValue);//Bitmap data offset
        vBmpDataSize := vStream.Size - SizeOf(TBitmapFileHeader);
        vBits.WriteRL(vBmpDataSize);
        vBits.UpdateRL(HeaderOffsetPos, GetRealPos);
        HeaderOffsetValue := GetRealPos - ImageOffset;
        vBits.WriteBytes(0, vHeaderSize);
        vBits.UpdateRL(BmpOffsetPos, GetRealPos);
        BmpOffsetPosValue := GetRealPos - ImageOffset;
        vBits.WriteBytes(P, vBmpDataSize);
        vBits.UpdateRL(0, vBits.Size - 4);
        SaveSectionToStream(GetPreviewSectionIndex, vBits, cnstImageDataBeginSentinel, cnstImageDataEndSentinel);

        Inc(HeaderOffsetPos, SizeOf(TsgDWGSentinel) shl cntMulDiv8);
        Inc(HeaderOffsetValue, SizeOf(TsgDWGSentinel));
        Inc(BmpOffsetPos, SizeOf(TsgDWGSentinel) shl cntMulDiv8);
        Inc(BmpOffsetPosValue, SizeOf(TsgDWGSentinel));

      finally
        vStream.Free;
      end;
    finally
      vBitmap.Free;
    end;
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionPrototype;
begin
  if Version < acR2013 then Exit;
  FSegmentedFile.WriteData(Sections[GetPrototypeSectionIndex]);
end;

procedure TsgCADtoDWG.ExportSectionRevHistory;
var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    { TODO: Full ExportSectionRevHistory; }
    vBits.WriteRL(0);
    vBits.WriteRL(0);
    if Version > acR2007 then
      vBits.WriteRL(1);
    vBits.WriteRL(0);
    SaveSectionToStream(GetRevHistorySectionIndex, vBits, [], []);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionSecondHeader(const AHandSeed: UInt64;
  Offset: Cardinal; const Locators: TsgDWGR15Locators);
var
  vBits: TsgDWGBits;
  S: AnsiString;
  I: Integer;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vBits.WriteRL(cntSizeTempValue);
    vBits.WriteBL(Offset);
    S := AnsiString(sgDWGVerToString(Version));
    vBits.WriteBytes(@S[1], Length(S));
    vBits.WriteBytes(0, 5);
    vBits.WriteRC(FMaintenanceVersion);
    vBits.WriteRC(1);
    vBits.WriteBS($617);
    vBits.WriteRS(FDWGCodePage);
    vBits.WriteBS(cntDWGR15SectionsCount);
    for I := 0 to High(Locators) do
    begin
      vBits.WriteRC(I);
      vBits.WriteBL(Locators[I].Seeker);
      vBits.WriteBL(Locators[I].Size);
    end;
    vBits.WriteBS($E);
    vBits.WriteHandleRecord(AHandSeed, 0);
    vBits.WriteHandleRecord(GetBlockControlObjectHandle, 1);
    vBits.WriteHandleRecord(GetLayerControlObjectHandle, 2);
    vBits.WriteHandleRecord(GetStyleControlObjectHandle, 3);
    vBits.WriteHandleRecord(GetLineTypeControlObjectHandle, 4);
    vBits.WriteHandleRecord(GetViewControlObjectHandle, 5);
    vBits.WriteHandleRecord(GetUCSontrolObjectHandle, 6);
    vBits.WriteHandleRecord(GetVPortControlObjectHandle, 7);
    vBits.WriteHandleRecord(GetAppIDControlObjectHandle, 8);
    vBits.WriteHandleRecord(GetDimStyleControlObjectHandle, 9);
    vBits.WriteHandleRecord(GetViewPortEntityHeaderControlObjectHandle, 10);
    vBits.WriteHandleRecord(GetDictionaryNamedObjectHandle, 11);
    vBits.WriteHandleRecord(GetDictionaryAcadMLineStyleHandle, 12);
    vBits.WriteHandleRecord(GetDictionaryAcadGroupHandle, 13);
    vBits.UpdateRL(0, vBits.Size + 14);//plus 14 RL(size) + 8 * RC (signature after CRC) + RS (CRC)
    vBits.WriteCRC8(cnstSecondFileHeaderCRCInit, False);
    vBits.WriteBytes(0, 8);
    SaveSectionToStream(GetSecondHeaderSectionIndex, vBits, cnstSecondFileHeaderBeginSentinel,
      cnstSecondFileHeaderEndSentinel);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionSummaryInfo(const ADateCreate,
  ADateUpdate: TDateTime);
const
  cnstVulesSeparator = '=';
  cnstMaxPropCustomSummaryInfo = cnstMaxWord;
var
  vBits: TsgDWGBits;
  I, vPos: Integer;
  vMaxProp: Word;
  S, vKey, vVal: string;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vBits.WriteSummaryInfoText(DrwPropTitle);
    vBits.WriteSummaryInfoText(DrwPropSubject);
    vBits.WriteSummaryInfoText(DrwPropAuthor);
    vBits.WriteSummaryInfoText(DrwPropKeywords);
    vBits.WriteSummaryInfoText(DrwPropComments);
    vBits.WriteSummaryInfoText(DrwPropSavedBy);
    vBits.WriteSummaryInfoText(DrwPropRevisionNumber);
    vBits.WriteSummaryInfoText(DrwPropHyperlinkBase);
    vBits.WriteJulianDateInt(Converter.DrwPropTotalEditingTime, False, True);
    WriteDateTimeAsJulian(ADateCreate, vBits, True);
    WriteDateTimeAsJulian(ADateUpdate, vBits, True);
    vMaxProp := Min(DrwPropCustomSummaryInfo.Count, cnstMaxPropCustomSummaryInfo);
    vBits.WriteRS(vMaxProp);
    for I := 0 to vMaxProp - 1 do
    begin
      S := DrwPropCustomSummaryInfo[I];
      vPos := Pos(cnstVulesSeparator, S);
      if vPos = 0 then
      begin
        vKey := S;
        vVal := '';
      end
      else
      begin
        vKey := Copy(S, 1, vPos - 1);
        vVal := Copy(S, vPos + 1, MaxInt);
      end;
      vBits.WriteSummaryInfoText(vKey);
      vBits.WriteSummaryInfoText(vVal);
    end;
    vBits.WriteRL0;
    vBits.WriteRL0;
    SaveSectionToStream(GetSummaryInfoSectionIndex, vBits, [], []);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ExportSectionTables;
var
  I: Integer;
  vBlockNameAliases: TStringList;
begin
  vBlockNameAliases := TStringList.Create;
  try
    vBlockNameAliases.Sorted := True;
    vBlockNameAliases.Duplicates := dupAccept;
    ApplyBlocksByProc(MakeBlockAlias, vBlockNameAliases);

    ExportSectionObjects;
    ExportTableAPPID;
    ExportCreatedTableRecords;
    ExportTableBLOCK_RECORD;
    ExportTableSTYLE;
    ExportTableLTYPE;
    ExportTableLAYER;
    ExportTableVPORT;
    ExportTableDIMSTYLE;
    ExportSectionENTITIES;
    ExportTableOBJECTS;

    for I := 0 to vBlockNameAliases.Count - 1 do
      TsgDXFEntity(vBlockNameAliases.Objects[I]).Name := vBlockNameAliases[I];

  finally
    vBlockNameAliases.Free;
  end;
end;

function TsgCADtoDWG.ExportSeqEnd(E: TsgDXFEntity; ABits: TsgDWGBits): Boolean;
begin
  Result := True;//nothing to do
end;

function TsgCADtoDWG.ExportShape(S: TsgDXFShape; ABits: TsgDWGBits): Boolean;
begin
  ABits.Write3BD(S.Point);//Ins pt 3BD 10
  ABits.WriteBD(S.Height);//Scale BD 40 Scale factor, default value 1
  ABits.WriteBD(S.Rotation / f180DividedByPi);//Rotation BD 50 Rotation in radians, default value 0
  ABits.WriteBD(S.Scale);//Width factor BD 41 Width factor, default value 1
  ABits.WriteBD(S.ObliqueAngle / f180DividedByPi);//Oblique BD 51 Oblique angle in radians, default value 0
  ABits.WriteBD(0);//Thickness BD 39
  ABits.WriteBS(S.ShapeNumber);//Shapeno BS 2
  ABits.Write3BD(S.Extrusion);//Extrusion 3BD 210
  Result := True;
end;

function TsgCADtoDWG.ExportSolid(S: TsgDXFSolid; ABits: TsgDWGBits): Boolean;
begin
  ABits.WriteBT(S.ZThick); //Thickness BT 39
  ABits.WriteBD(S.Point.Z); //Elevation BD --- Z for 10 - 13.
  ABits.Write2RD(S.Point); //1st corner 2RD 10
  ABits.Write2RD(S.Point1); //2nd corner 2RD 11
  ABits.Write2RD(S.Point2); //3rd corner 2RD 12
  ABits.Write2RD(S.Point3); //4th corner 2RD 13
  ABits.WriteBE(S.Extrusion); //Extrusion BE 210
  Result := True;
end;

function TsgCADtoDWG.ExportSpline(Spline: TsgDXFSpline; ABits: TsgDWGBits): Boolean;
var
  vScenario: Word;
  vBadSpline, vWeightFlag: Boolean;
  I: Integer;
  vFitList: TFPointList;
  vSplineFlags, vKnotParameter: Integer;
begin
  Result := True;
  vBadSpline := Spline.Knots.Count >= Spline.ControlCount + 4;
  vWeightFlag := False;
  vFitList := nil;
  if Spline.IsByAngles or vBadSpline then
  begin
    vScenario := 2;
    if Spline.Knots.Count > Spline.ControlCount + 4 then //?? vBadSpline+
      vFitList := Spline.PolyPoints
    else
      if Spline.Fit.Count = 0 then
        vScenario := 1
      else
        vFitList := Spline.Fit;
  end
  else
    vScenario := 1;
  if vScenario = 1 then
    vWeightFlag := Spline.WeightCount > 0;

  if Version >= acR2013 then
  begin
    if vScenario = 2 then
    begin
      vSplineFlags := 9;
      vKnotParameter := 0;
    end
    else
    begin
      vSplineFlags := 0;
      vKnotParameter := 15;
    end;
    ABits.WriteBS(1);
    ABits.WriteBL(vSplineFlags);
    ABits.WriteBL(vKnotParameter);
  end
  else
    ABits.WriteBS(vScenario);

  ABits.WriteBS(Spline.Degree);
  case vScenario of
    2:
      begin
        ABits.WriteBD(Spline.FitTol);
        ABits.Write3BD(Spline.BeginningTangent);
        ABits.Write3BD(Spline.EndingTangent);
        ABits.WriteBL(vFitList.Count);
        for I := 0 to vFitList.Count - 1 do
          ABits.Write3BD(vFitList[I]);
      end;
    1:
      begin
        ABits.WriteBit(Spline.Flags, 4);
        ABits.WriteBit(Spline.Closed);
        ABits.WriteBit(Spline.Flags, 1);
        ABits.WriteBD(Spline.KnotTol);
        ABits.WriteBD(Spline.CtrlTol);
        ABits.WriteBL(Spline.KnotCount);
        ABits.WriteBL(Spline.ControlCount);
        ABits.WriteBit(vWeightFlag);
        for I := 0 to Spline.KnotCount - 1 do
          ABits.WriteBD(Spline.Knot[I]);
        for I := 0 to Spline.ControlCount - 1 do
        begin
          ABits.Write3BD(Spline.ControlPoints[I]);
          if vWeightFlag then
            if I <= Spline.WeightCount - 1 then
              ABits.WriteBD(Spline.Weight[I])
            else
              ABits.WriteBD(0);
        end;
      end;
  end;
end;

procedure TsgCADtoDWG.ExportTableAPPID;
var
  I, J: Integer;
  vOwnerHandle: UInt64;
  vEEDItems: TsgObjectCollection;
  vAppIDs: TsgDXFEntity;
  vHandles: TsgDWGHandleArray;
  vExports: TsgInt64List;

  function DoProcAppID(AOwnerHandle: UInt64; const AName: string;
    var AHandles: TsgDWGHandleArray): TsgDXFEntity;
  begin
    if Assigned(vAppIDs) then
      Result := vAppIDs.FindEntByName(AName)
    else
      Result := nil;
    vExports.Add(ProcAppID(AOwnerHandle, AName, Result, AHandles));
  end;

begin
  vExports := TsgInt64List.Create;
  try
    vExports.Sorted := True;
    vAppIDs := Converter.Sections[csAppID];
    vOwnerHandle := GetAppIDControlObjectHandle;
    ForceAppIDLinks;
    AddCommonHandleRefs(vHandles, cnstBadHandle);
    DoProcAppID(vOwnerHandle, sACADXDataAppName, vHandles);
    DoProcAppID(vOwnerHandle, sACADXDataAttribAppName, vHandles);
    if XDataAppName <> '' then
      DoProcAppID(vOwnerHandle, XDataAppName, vHandles);
    DoProcAppID(vOwnerHandle, sURLXDataName, vHandles);
    for I := 0 to EntityEEDItems.Count - 1 do
    begin
      vEEDItems := TsgObjectCollection(EntityEEDItems.Objects[I]);
      for J := 0 to vEEDItems.Count - 1 do
        if vExports.IndexOf(vEEDItems[J].HashCode) = -1 then
          vExports.Add(ProcAppID(vOwnerHandle,
            TsgHandleNameLinks(AppIDs).NameByHandle[vEEDItems[J].HashCode],
            vAppIDs.FindEntByHandle(vEEDItems[J].HashCode), vHandles));
    end;
    ProcAppIDControl(vOwnerHandle, vHandles);
  finally
    vExports.Free;
  end;
end;

procedure TsgCADtoDWG.ExportTableVPORT;
var
  vOwnerHandle: UInt64;
  vVPort: TsgDXFVPort;
  vNeedsToBeDeleted: Boolean;
  vHandles: TsgDWGHandleArray;
//  I: Integer;

  function FindActiveVPort: TsgDXFVPort;
//  var
//    I: Integer;
  begin
    Result := nil;
//    if Converter.Sections[csVPorts] = nil then Exit;
//    for I := 0 to Converter.Sections[csVPorts].Count - 1 do
//      if SameText(sActiveVPort, TsgDXFVPort(Converter.Sections[csVPorts].Entities[I]).Name) then
//      begin
//        Result := TsgDXFVPort(Converter.Sections[csVPorts].Entities[I]);
//        Break;
//      end;
  end;

begin
  vOwnerHandle := GetVPortControlObjectHandle;
  vVPort := FindActiveVPort;
  vNeedsToBeDeleted := vVPort = nil;
  if vNeedsToBeDeleted then
    vVPort := CreateDefaultVPort(DoHandle);
  try
//    if Assigned(Converter.Sections[csVPorts]) then
//      for I := 0 to Converter.Sections[csVPorts].Count - 1 do
//        if not SameText(TsgDXFVPort(Converter.Sections[csVPorts].Entities[I]).Name, sActiveVPort) then
//          ProcVPort(vOwnerHandle, TsgDXFVPort(Converter.Sections[csVPorts].Entities[I]), vHandles);
    AddCommonHandleRefs(vHandles, cnstBadHandle);
    ProcVPort(vOwnerHandle, vVPort, vHandles);
    ProcVPortControlObject(vOwnerHandle, vHandles);
  finally
    if vNeedsToBeDeleted then
      vVPort.Free;
  end;
end;

function TsgCADtoDWG.ExportText(T: TsgDXFText; ABits: TsgDWGBits): Boolean;
var
  vFlags: Byte;
  vBitPos: Cardinal;
  vObliqueAngle: Double;
  vFirstPoint, vSecondPoint: TFPoint;

  procedure WriteRDAndChangeFlag(const Value: Double; Mask: Byte);
  begin
    if not IsZero(Value) then
    begin
      vFlags := vFlags or Mask;
      ABits.WriteRD(Value);
    end;
  end;

  procedure WriteBSAndChangeFlag(const Value: Word; Mask: Byte);
  begin
    if Value <> 0 then
    begin
      vFlags := vFlags or Mask;
      ABits.WriteBS(Value);
    end;
  end;

  procedure WriteFirstPointAndChangeFlag(const APoint: TFPoint; Mask: Byte);
  begin
    WriteRDAndChangeFlag(APoint.Z, Mask);//Elevation RD --- present if !(DataFlags & 0x01)
    ABits.Write2RD(APoint);//Insertion pt 2RD 10
  end;

begin
  vBitPos := ABits.Position;
  ABits.WriteRC($FF); //Flag for using to determine presence of subsquent data
  vFlags := 0;
  vFirstPoint := T.Point;
  if TsgDXFTextEx(T).HasSecond then//Alignment pt 2DD 11 present if !(DataFlags & 0x02), use 10 & 20 values for 2 default values.
  begin
    vSecondPoint := T.Point1;
//    if T.HAlign or T.VAlign <> 0 then
//      //if IsEqual(vFirstPoint.X, vSecondPoint.X) and IsEqual(vFirstPoint.Y, vSecondPoint.Y) then
//        vFirstPoint := T.StartPoint;
    WriteFirstPointAndChangeFlag(vFirstPoint, $01);
    vFlags := vFlags or $02;
    ABits.WriteDD(vSecondPoint.X, vFirstPoint.X);
    ABits.WriteDD(vSecondPoint.Y, vFirstPoint.Y);
  end
  else
    WriteFirstPointAndChangeFlag(vFirstPoint, $01);
  ABits.WriteBE(T.Extrusion);//Extrusion BE 210
  ABits.WriteBT(0);//Thickness BT 39
  vObliqueAngle := Abs(T.ObliqueAngle);
  if (vObliqueAngle > 85) and (vObliqueAngle <= 180) then
    vObliqueAngle := vObliqueAngle - 180;
  if (vObliqueAngle > 180) and (vObliqueAngle < 275) then
    vObliqueAngle := vObliqueAngle + 180;
  if T.ObliqueAngle < 0 then
    vObliqueAngle := -1 * vObliqueAngle;
  WriteRDAndChangeFlag(vObliqueAngle / f180DividedByPi, $04);//Oblique ang RD 51 present if !(DataFlags & 0x04)
  WriteRDAndChangeFlag(T.Rotation / f180DividedByPi, $08);//Rotation ang RD 50 present if !(DataFlags & 0x08)
  ABits.WriteRD(T.Height);//Height RD 40
  WriteRDAndChangeFlag(T.Scale, $10);//Width factor RD 41 present if !(DataFlags & 0x10)
  ABits.WriteTV(GetEntityStringValue(T), FCodePage);//Text value TV 1
  WriteBSAndChangeFlag(T.Generation, $20);//Generation BS 71 present if !(DataFlags & 0x20));
  WriteBSAndChangeFlag(T.HAlign, $40);//Horiz align. BS 72 present if !(DataFlags & 0x40)
  WriteBSAndChangeFlag(T.VAlign, $80);//Vert align. BS 73 present if !(DataFlags & 0x80)
  ABits.UpdateRC(vBitPos, not vFlags);
  if T.EntType <> ceText then
  begin
    if Version >= acR2010 then
      ABits.WriteRC(0); //Version RC ?
    if Version >= acR2018 then
      ABits.WriteRC(1); //sg unknown (1)
    ABits.WriteTV(TsgDXFAttrib(T).Tag, FCodePage);//Tag TV 2
    ABits.WriteBS(0);//Field length BS 73 unused
    ABits.WriteRC(T.Flags);//RC 70 NOT bit-pair-coded.
    if Version >= acR2007 then
      ABits.WriteBit(TsgDXFAttdef(T).LockPosition);
    if T.EntType = ceAttdef then
    begin
      if Version >= acR2010 then
        ABits.WriteRC(0); //Version RC ?
      ABits.WriteTV('', FCodePage);//Prompt TV 3
    end;
  end;
  Result := True;
end;

function TsgCADtoDWG.ExportTolerance(T: TsgDXFTolerance; ABits: TsgDWGBits): Boolean;
begin
  ABits.Write3BD(T.Point);//Ins pt 3BD 10
  ABits.WriteXDirection(TsgDXFToleranceEx(T).HasSecond, T.Point1);//X direction 3BD 11
  ABits.Write3BD(T.Extrusion);//Extrusion 3BD 210 etc
  ABits.WriteTV(PrepareTextForDWG(T.Text), FCodePage, True);//Text string BS 1
  Result := True;
end;

function TsgCADtoDWG.ExportUnknownObject(E: TsgDXFEntity;
  const AExportData: TsgDirectExportData): Boolean;
var
  vDummy: TsgDXFPoint;
  vData: TsgDirectExportData;
begin
  Result := True;
  if E = nil then Exit;
  vDummy := TsgDXFPoint.Create;
  try
    vData := AExportData;
    vDummy.AssignEntity(E);
    vDummy.Visible := False;
    vDummy.Visibility := False;
    if E is TsgDXFPenLine then
      vDummy.Point := TsgDXFPenLine(E).Point;
    if E is TsgDXFVertex then
      vDummy.Point := TsgDXFVertex(E).Point;
    vDummy.Handle := GetEntityHandle(E, True);
    vData.EntDWGCode := cntDWGObjCodeUNSUSED;
    DoExport(vDummy, @vData);
  finally
    vDummy.Free;
  end;
end;

function TsgCADtoDWG.ExportVertex(Owner: TsgDXFEntity; V: TsgDXFVertex;
  DWGCode: Word; ABits: TsgDWGBits): Boolean;
var
  vPoint: TFPoint;
begin
  Result := True;
  if DWGCode <> cntDWGObjCodeVERTEXPFACEFACE then
    ABits.WriteRC(V.Flags);
  case DWGCode of
    cntDWGObjCodeVERTEX2D:
      begin
        ABits.Write3BD(MakeFPoint(V.Point.X, V.Point.Y, {TsgDXFPolyline(Owner).Elevation}0));
        if (not IsZero(V.StartWidth)) and IsEqual(V.StartWidth, V.EndWidth) then
          ABits.WriteBD(-V.StartWidth)
        else
        begin
          ABits.WriteBD(V.StartWidth);
          ABits.WriteBD(V.EndWidth);
        end;
        ABits.WriteBD(V.Bulge);
        if Version >= acR2010 then
          ABits.WriteBL(0); //Vertex ID
        ABits.WriteBD(0);//V.FitTangent
      end;
    cntDWGObjCodeVERTEXPFACEFACE:
      begin
        ABits.WriteBS(V.PolyFaceVertexIndex1);
        ABits.WriteBS(V.PolyFaceVertexIndex2);
        ABits.WriteBS(V.PolyFaceVertexIndex3);
        ABits.WriteBS(V.PolyFaceVertexIndex4);
      end;
  else
    begin
      vPoint := V.Point;
      DoExtrusion(vPoint, TsgCADBasePolyline(Owner).Extrusion);
      ABits.Write3BD(vPoint);
    end;
  end;
end;

function TsgCADtoDWG.ExportViewPort(V: TsgDXFViewPort; ABits: TsgDWGBits): Boolean;

  function CorrectSize(const ASize: Double; ADefault: Double = 1.0): Double;
  begin
    if ASize < 0 then
      Result := ADefault
    else
      Result := ASize;
  end;

begin
  Result := True;
  ABits.Write3BD(V.PSpaceCenter);//Center 3BD 10
  ABits.WriteBD(CorrectSize(V.PSpaceWidth));//Width BD 40
  ABits.WriteBD(CorrectSize(V.PSpaceHeight));//Height BD 41
  ABits.Write3BD(V.ViewTarget); //View Target 3BD 17
  ABits.Write3BD(V.ViewDirection);// View Direction 3BD 16
  ABits.WriteBD(V.ViewTwistAngle / f180DividedByPi);// View Twist Angle BD 51
  ABits.WriteBD(CorrectSize(V.MSpaceHeight));// View Height BD 45
  ABits.WriteBD(cntViewPortPerspectiveLensLength);// Lens Length BD 42
  ABits.WriteBD(V.FrontClipPlane);// Front Clip Z BD 43
  ABits.WriteBD(V.BackClipPlane);// Back Clip Z BD 44
  ABits.WriteBD(0);// Snap Angle BD 50
  ABits.Write2RD(V.MSpaceCenter);// View Center 2RD 12
  ABits.Write2RD(cnstFPointZero);// Snap Base 2RD 13
  ABits.Write2RD(MakeF2DPoint(0.5, 0.5));// Snap Spacing 2RD 14
  ABits.Write2RD(cnstFPointZero);//Grid Spacing 2RD 15
  ABits.WriteBS(cntViewPortCircleZoomPercent);// Circle Zoom BS 72
  if Version >= acR2007 then
    ABits.WriteBS(0);// Grid Major BS 61
  if Assigned(V.FrozenLayers) and (V.FrozenLayers.Count > 0) then
    ABits.WriteBL(V.FrozenLayers.Count)
  else
    ABits.WriteBL(0); // Frozen Layer Count BL !!! must be V.FrozenLayers.Count
  ABits.WriteBL(V.Flags);// Status Flags BL 90
  ABits.WriteTV('', FCodePage);// Style Sheet TV 1
  ABits.WriteRC(0);// Render Mode RC 281 0 - 2D Optimized (classic 2D)
  ABits.WriteBit(0);// UCS at origin B 74
  ABits.WriteBit(1);// UCS per Viewport B 71 0 - The UCS will not change when this viewport becomes active
//                                           1 - This viewport stores its own UCS which will become the current UCS whenever the viewport is activated
  ABits.Write3BD(cnstExtrusion);// UCS Origin 3BD 110
  ABits.Write3BD(cnstXOrtAxis);// UCS X Axis 3BD 111
  ABits.Write3BD(cnstYOrtAxis);// UCS Y Axis 3BD 112
  ABits.WriteBD(0);// UCS Elevation BD 146
  ABits.WriteBS(0);// UCS Ortho View Type BS 79
  if Version >= acR2004 then
    ABits.WriteBS(0);// ShadePlot Mode BS 170 0 - As Displayed
  if Version >= acR2007 then
  begin
    ABits.WriteBit(True);// Use def. lights B 292
    ABits.WriteRC(0);// Def. lighting type RC 282 0 - One distant light
    ABits.WriteBD(50);// Brightness BD 141
    ABits.WriteBD(50);// Contrast BD 142
    ABits.WriteCMC(cnstColorCADByWhite);// Ambient light color CMC 63
  end;
end;

procedure TsgCADtoDWG.ExportViewPortEntityHeaders;
var
  I, vCount, vIndex: Integer;
begin
  vCount := FCreatedElements.CountOf(Word(cntDWGObjCodeVPENTHDR), vIndex);
  if vCount = 0 then Exit;
  for I := vIndex to vIndex + vCount - 1 do
    ProcViewPortEntHeader(I);
end;

function TsgCADtoDWG.ExportWipeOut(W: TsgCADCustomRectangle; ABits: TObject): Boolean;
var
  I, vCount: Integer;
  vBits: TsgDWGBits;
  vSize, vUVector, vVVector: TFPoint;
begin
  Result := True;
  vBits := TsgDWGBits(ABits);
  vBits.WriteBL(0);//BL 90 class version
  vBits.Write3BD(W.Point);//pt0 3BD 10 insertion point

  GetCorrectImageVectorData(TsgDXFImageEnt(W), vUVector, vVVector, vSize);
  vBits.Write3BD(vUVector);//uvec 3BD 11 u direction vector
  vBits.Write3BD(vVVector);//vvec 3BD 12 v direction vector
  vBits.Write2RD(vSize);//size 2RD 13 size of image

  vBits.WriteBS(W.Flags);//displayprops BS 70
  vBits.WriteBit(W.UseClipping);//clipping B 280 1 if on
  vBits.WriteRC(cntImageBrightness);//brightness RC 281 brightness value (0—100, default 50)
  vBits.WriteRC(cntImageContrast);//contrast RC 282 contrast value (0—100, default 50)
  vBits.WriteRC(cntImageFade);//fade RC 283 fade value (0—100, default 0)
  if Version > acR2007 then
    vBits.WriteBit(0);//Clip mode B 290 0 = outside, 1 = inside
  vBits.WriteBS(W.ClippingBoundaryType);//clipbndtype BS 71 type of clipping boundary, 1==rect, 2==polygon
  case W.ClippingBoundaryType of //!!! not sure
    1, 2:
      begin
        if W.ClippingBoundaryType > 1 then
          vBits.WriteBL(GetRealCountBoundaryPoints(W));
        vCount := W.ClipPointsCount;
        for I := 0 to vCount - 1 do
          vBits.Write2RD(W.ClipPoints[I]);
        if IsNeedAddPointForBoundary(W) then
          vBits.Write2RD(W.ClipPoints[0]);
      end;
  end;
end;

function TsgCADtoDWG.ExportWipeOutAsExternalFile(W: TsgCADCustomRectangle;
  AData: TObject): Boolean;
var
  vData: TsgDWGExportDataClass;
  vExportData: TsgDirectExportData;
begin
  Result := True;
  vData := TsgDWGExportDataClass(AData);
  vExportData := vData.ExportData;
  vExportData.EntDWGCode := cntDWGObjCodeIMAGE;
  vExportData.UseEntCode := True;
  DoExport(TsgDXFEntity(vData.OwnerObject), @vExportData);
end;

procedure TsgCADtoDWG.ExportTableLAYER;
const
  sZeroLayer = '0';
var
  I, vFirstLayerIndex, vZeroLayerIndex: Integer;
  vOwnerHandle: UInt64;
  vZeroLayer: TsgDXFLayer;
  vHandles: TsgDWGHandleArray;

  vDWGHandle1, vDWGHandle2: TsgDWGHandle;
begin
  vOwnerHandle := GetLayerControlObjectHandle;
  vZeroLayer := nil;
  try
    AddCommonHandleRefs(vHandles, cnstBadHandle);
    vZeroLayerIndex := -1;
    vFirstLayerIndex := Length(vHandles);

    if TsgDXFTable(Converter.Sections[csLayers]).FindEntByName(sZeroLayer) = nil then
    begin
      vZeroLayer := TsgDXFLayer.Create;
      vZeroLayer.Handle := DoHandle;
      vZeroLayer.ColorCAD := MakeColorCAD(acIndexColor, clDXFBlackWhite);
      vZeroLayer.Name := sZeroLayer;
      vZeroLayer.LineType := TsgDXFLineType(TsgDXFTable(Converter.Sections[csLTypes]).FindEntByName(sContinuous));
      vZeroLayer.LineWeight := 0;
      vZeroLayer.Visible := True;
      vZeroLayer.Locked := False;
      vZeroLayer.Frozen := False;
      AddCreatedElem('0', nil, FCreatedElements, cntDWGObjCodeLAYER, vZeroLayer.Handle);
    end;
    if vZeroLayer <> nil then
      ProcLayer(vOwnerHandle, vZeroLayer, vHandles, vZeroLayerIndex);
    if Converter.Sections[csLayers] <> nil then
      for I := 0 to Converter.Counts[csLayers] - 1 do
        ProcLayer(vOwnerHandle, TsgDXFLayer(Converter.Layers[I]),
          vHandles, vZeroLayerIndex);

    if (vZeroLayerIndex >= 0) and (vZeroLayerIndex <> vFirstLayerIndex) then //Layer '0' must be first
    begin
      vDWGHandle1 := vHandles[vFirstLayerIndex];
      vDWGHandle2 := vHandles[vZeroLayerIndex];
      vHandles[vFirstLayerIndex] := vDWGHandle2;
      vHandles[vZeroLayerIndex] := vDWGHandle1;
    end;

    ProcLayerControlObject(vOwnerHandle, vHandles);
  finally
    vZeroLayer.Free;
  end;
end;

procedure TsgCADtoDWG.ExportTableLTYPE;
var
  I: Integer;
  vOwnerHandle: UInt64;
  vHandles: TsgDWGHandleArray;
  vLTByBlock, vLTByLayer, vLTypes: TsgDXFEntity;

  function DoProcLType(AOwnerHandle: UInt64; ALType: TsgDXFEntity; AHandleCode: Byte;
    var AHandles: TsgDWGHandleArray; const AName: string = ''): TsgDXFEntity;
  begin
    Result := ALType;
    if Result = nil then
    begin
      Result := TsgDXFLineType.Create;
      Result.Name := AName;
    end;
    if Result.Handle = cnstBadHandle then
      Result.Handle := DoHandle;
    try
      AddHandleToArray(CreateDWGHandle(Result.Handle, AHandleCode), AHandles);
      ProcLType(AOwnerHandle, TsgDXFLineType(Result));
    finally
      if Result <> ALType then
        FreeAndNil(Result);
    end;
  end;

begin
  vOwnerHandle := GetLineTypeControlObjectHandle;
  AddCommonHandleRefs(vHandles, cnstBadHandle);
  vLTByBlock := nil;
  vLTByLayer := nil;
  vLTypes := Converter.Sections[csLTypes];
  if vLTypes <> nil then
  begin
    vLTByBlock := vLTypes.FindEntByName(sByBlock);
    vLTByLayer := vLTypes.FindEntByName(sByLayer);
    for I := 0 to vLTypes.Count - 1 do
      if (vLTypes[I] <> vLTByBlock) and (vLTypes[I] <> vLTByLayer) then
        DoProcLType(vOwnerHandle, vLTypes[I], cntDWGObjHandleSoftOwner, vHandles);
  end;
  DoProcLType(vOwnerHandle, vLTByBlock, cntDWGObjHandleHardOwner, vHandles, sByBlock);
  DoProcLType(vOwnerHandle, vLTByLayer, cntDWGObjHandleHardOwner, vHandles, sByLayer);
  ProcLTypeControlObject(vOwnerHandle, vHandles);
end;

type
  TsgProcDictByStrList = procedure(const Handle, OwnerHandle: UInt64;
    const Index: Integer; const Name: string) of object;

procedure TsgCADtoDWG.ExportTableOBJECTS;

  function GetItemForDictionary(var Items: TsgDWGLinkItems; var Handle, OwnerHandle: UInt64;
    const AName: string): Boolean;
  var
    I, vIndex, vCount: Integer;
    vElem: TsgDWGCreatedElement;
    vHandle: UInt64;
    vName: string;
    vRasterVariables: TsgCADRasterVariables;

    procedure ProcFromStrList(const DXFClass: string; const AList: TStringList;
      Proc: TsgProcDictByStrList);
    var
      I, vIndex, vCount: Integer;
      vName, vVal: string;
    begin
      vCount := FCreatedElements.CountOf(GetDWGCodeByClass(DXFClass), vIndex);
      DoError(vCount <> AList.Count, sListsNotEqual);
      for I := 0 to AList.Count - 1 do
      begin
        vName := AList.Names[I];
        vVal := AList.Values[vName];
        AddItemToArray(FCreatedElements[I + vIndex].Handle, vName, Items);
        Proc(FCreatedElements[I + vIndex].Handle, Handle, I, vVal);
      end;
    end;

  begin
    Result := True;
    SetLength(Items, 0);

    if SameText(AName, sNamedObjectsDictionary) then //main dictionary object
    begin
      OwnerHandle := 0;
      Handle := GetDictionaryNamedObjectHandle;
      vCount := FCreatedElements.CountOf(Word(cntDWGObjCodeDICTIONARY1), vIndex);
      for I := vIndex to vIndex + vCount - 1 do
      begin
        vElem := FCreatedElements[I];
        if not SameText(vElem.Name, sNamedObjectsDictionary) then
        begin
          if I = 7 then
            AddItemToArray(cnstBadHandle, cntEmptyDictionary, Items); //do not know why but DWG file needs an empty dictionary
          AddItemToArray(vElem.Handle, vElem.Name, Items);
        end;
      end;
      Exit;
    end;

    vIndex := FCreatedElements.IndexOf(AName);
    if vIndex < 0 then
    begin
      Result := False;
      Exit;
    end;

    vElem := FCreatedElements[vIndex];
    OwnerHandle := vElem.OwnerHandle;
    Handle := vElem.Handle;

    //empty dictionaries
    if SameText(AName, sAcadGroupDictionary) or SameText(AName, sAcadMaterialDictionary) or
      SameText(AName, sAcadMLeaderStyleDictionary)  then
        Exit;

    //ACAD_MLINESTYLE
    if SameText(AName, sAcadMLineStyleDictionary) then
    begin
      if Converter.Counts[csMLineStyles] > 0 then
      begin
        for I := 0 to TsgDXFTable(Converter.Sections[csMLineStyles]).Count - 1 do
        begin
          AddItemToArray(TsgDXFTable(Converter.Sections[csMLineStyles]).Entities[I].Handle,
            (TsgDXFTable(Converter.Sections[csMLineStyles]).Entities[I] as TsgMLineStyle).Name, Items);
          ProcMLineStyle(TsgMLineStyle(TsgDXFTable(Converter.Sections[csMLineStyles]).Entities[I]), Handle);
        end;
      end
      else
      begin
        AddItemToArray(GetMLineStyleHandle, sStandardName, Items);
        ProcMLineStyle(nil, Handle);
      end;
      Exit;
    end;

    //ACAD_IMAGE_DICT
    if SameText(AName, sAcadImageDict) then
    begin
      for I := 0 to Converter.Counts[csImageDefs] - 1 do
        AddItemToArray(Converter.Sections[csImageDefs].Entities[I].Handle,
          TsgDXFEntityEx(Converter.Sections[csImageDefs].Entities[I]).EntName, Items);
      for I := 0 to ImageDefList.Count - 1 do
        AddItemToArray(TsgDXFEntity(ImageDefList[I]).Handle, TsgDXFEntity(ImageDefList[I]).EntName, Items);
      Exit;
    end;

    //RASTERVARIABLES
    if SameText(AName, sAcadImageVars) then
    begin
      vRasterVariables := TsgCADRasterVariables(ExportCADImage.Converter.Sections[csObjects].FindEntByName(sRasterVariables));
      if Assigned(vRasterVariables) then
        ProcRasterVariables(vRasterVariables, OwnerHandle);
      Result := False; //not needed to add the RASTERVARIABLES dict to Distionary list see ACAD_WIPEOUT_VARS below
      Exit;
    end;

    //ACAD_SCALE
    if SameText(AName, sAcadScaleListDictionary) then
    begin
      ProcFromStrList(cntClassDXFSCALE, FScalesDictionary, ProcScale);
      Exit;
    end;

    //ACAD_WIPEOUT_VARS
    if SameText(AName, sAcadWipeOutVarsDictionary) then
    begin
      Result := False;
      ProcWipeoutVar(Handle, OwnerHandle);
      Exit;
    end;

    //AcDbVariableDictionary
    if SameText(AName, sAcDbVariableDictionary) then
    begin
      ProcFromStrList(cntClassDXFDICTIONARYVAR, FVariableDictionary,
        ProcVariableDictionary);
      Exit;
    end;

    //DWGPROPS
    if SameText(AName, sDWGPropsDictionary) then
    begin
      if Version = acR2000 then
        ProcDWGProps(Handle, OwnerHandle);
      Result := False;
      Exit;
    end;

    //ACAD_LAYOUT
    if SameText(AName, sAcadLayoutDictionary) then
    begin
      ProcLayout(ExportCADImage.Layouts[0], ExportCADImage.Layouts[0].Handle, Handle, 0, '');
      AddItemToArray(ExportCADImage.Layouts[0].Handle, sModel, Items);
      if GetPaperLayout = nil then
      begin
        vHandle := GetPaperSpaceLayoutHandle(@vIndex);
        vName := FCreatedElements[vIndex].Name;
        ProcLayout(nil, vHandle, Handle, 1, vName);
        AddItemToArray(vHandle, vName, Items);
      end
      else
        for I := 1 to ExportCADImage.LayoutsCount - 1 do
        begin
          ProcLayout(ExportCADImage.Layouts[I], ExportCADImage.Layouts[I].Handle, Handle, I, '');
          AddItemToArray(ExportCADImage.Layouts[I].Handle, ExportCADImage.Layouts[I].Name, Items);
        end;
      Result := True;
      Exit;
    end;
  end;

var
  vItems: TsgDWGLinkItems;
  vHandle, vOwnerHandle: UInt64;
  I: Integer;
begin
  for I := Low(cnstDWGDictionaries) to High(cnstDWGDictionaries) do
    if GetItemForDictionary(vItems, vHandle, vOwnerHandle, cnstDWGDictionaries[I]) then
      ProcDictionary(vHandle, vOwnerHandle, vItems);
  ProcXRecords;
  ProcXData;
end;

procedure TsgCADtoDWG.FinishObject(const ObjHandle: UInt64; const ABits: TsgDWGBits;
  const AObjSize: Cardinal; ADWGCode: Word; AHandleStreamBitSize: Cardinal = 0;
  AWriteCRC: Boolean = True);
begin
  if Version >= acR2010 then
    ABits.InsertMCHandle(0, AHandleStreamBitSize);
  ABits.InsertMS(0, AObjSize);
  if AWriteCRC then
    ABits.WriteCRC8(cnstObjectCRCInit);
  DoError(ObjHandle = cnstBadHandle, sHandleInvalid);
  SaveObjectToStream(ObjHandle, ABits, ADWGCode);
end;

procedure TsgCADtoDWG.ForceAppIDLinks;
var
  I: Integer;
  vAppIDs: TsgDXFEntity;
  vLinks: TsgHandleNameLinks;
begin
  if not Assigned(FAppIDs) then
  begin
    vLinks := TsgHandleNameLinks.Create;
    FAppIDs := vLinks;
    vAppIDs := Converter.Sections[csAppID];
    if Assigned(vAppIDs) then
    begin
      vLinks.Sorted := False;
      for I := 0 to vAppIDs.Count - 1 do
        vLinks.AddItem(GetEntityHandle(vAppIDs[I], True, cntDWGObjCodeAPPID, vAppIDs[I].Name), vAppIDs[I].Name);
      // Find force sorting
      if not vLinks.Find(sACADXDataAppName, I) then
        vLinks.AddItem(DoHandle, sACADXDataAppName);
      if not vLinks.Find(sACADXDataAttribAppName, I) then
        vLinks.AddItem(DoHandle, sACADXDataAttribAppName);
      if FSaveBTIDataDwg and (XDataAppName <> '') then
        if not vLinks.Find(XDataAppName, I) then
          vLinks.AddItem(DoHandle, XDataAppName);
      if not vLinks.Find(sURLXDataName, I) then
        vLinks.AddItem(DoHandle, sURLXDataName);
    end;
  end;
end;

procedure TsgCADtoDWG.ForceEntityHandle(AEntity: TsgDXFEntity);
begin
  if AEntity.Handle = cnstBadHandle then
  begin
    FEntitiesWithAddedHandles.Add(AEntity);
    AEntity.Handle := DoHandle;
  end;
end;

procedure TsgCADtoDWG.FreeSections;
begin
  if FSections = nil then Exit;
  TsgObjectList.ClearList(FSections);
end;

function TsgCADtoDWG.Get64bitof70Group(Flags: Word): Byte;
begin
  if Flags and 64 <> 0 then
    Result := 1
  else
    Result := 0;
end;

function TsgCADtoDWG.GetActiveLayerHandle(AGetZeroLayer: Boolean = False): UInt64;
var
  vLayer: TsgDXFLayer;
  vNameFirst, vNameSecord: string;
begin
  if AGetZeroLayer then
  begin
    vNameFirst := Converter.HeadVarStruct.CLayer;
    vNameSecord := '0';
  end
  else
  begin
    vNameFirst := '0';
    vNameSecord := Converter.HeadVarStruct.CLayer;
  end;
  vLayer := Converter.LayerByName(vNameFirst);
  if vLayer = nil then
    vLayer := Converter.LayerByName(vNameSecord);
  Result := GetEntityHandle(vLayer);
end;

function TsgCADtoDWG.GetAppIDControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(Converter.Sections[csAppID], False,
    cntDWGObjCodeAPPIDCONTROLOBJ, '', True);
end;

function TsgCADtoDWG.GetAppIDs: TsgCollection;
begin
  ForceAppIDLinks;
  Result := FAppIDs;
end;

function TsgCADtoDWG.GetAppInfoSectionIndex: Integer;
begin
  Result := GetSummaryInfoSectionIndex + 1;
end;

function TsgCADtoDWG.GetArrowBlockHandle(const Value: TsgDimArrowType): UInt64;
var
  vBlockName: string;
begin
  vBlockName := GetNameByArrowType(Value, sgDimensionArrowTypeNames[Low(sgDimensionArrowTypeNames)]);
  Result := GetBlockRecordHandle(vBlockName);
end;

function TsgCADtoDWG.GetAuxHeaderOffset: Cardinal;
begin
  Result := cntDWGR15FileHeaderSize;
end;

function TsgCADtoDWG.GetAuxHeaderSectionIndex: Integer;
begin
  Result := 5;
end;

function TsgCADtoDWG.GetBlockControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(Converter.Sections[csBlockRecords], True,
    cntDWGObjCodeBLOCKCONTROLOBJ);
end;

function TsgCADtoDWG.GetBlockRecordHandle(const AName: string): UInt64;
var
  vBlock: TsgDXFBlock;
begin
  vBlock := TsgDXFBlock(Converter.BlockByName(AName));
  if Assigned(vBlock) then
    Result := vBlock.BlockRecord.Handle
  else
    Result := cnstBadHandle;
end;

function TsgCADtoDWG.GetClassesOffset: Cardinal;
begin
  Result := GetHeaderVarsOffset + Sections[GetHeaderVarsSectionIndex].Size;
end;

function TsgCADtoDWG.GetClassesSectionIndex: Integer;
begin
  Result := 1;
end;

procedure TsgCADtoDWG.GetCreateAndUpdateDateTime(var ADateCreate,
  ADateUpdate: TDateTime);
begin
  ADateCreate := Converter.DrwPropCreatedDateTime;
  ADateUpdate := Converter.DrwPropModifiedDateTime;
end;

function TsgCADtoDWG.GetCurrentViewportEntityHeaderHandle: UInt64;
var
  vLayout: TsgDXFLayout;
  I, vIndex: Integer;
  vViewPortHandle: UInt64;
begin
  Result := GetEntityHandle(nil);
  vLayout := GetPaperLayout;
  if vLayout <> nil then
  begin
    vViewPortHandle := cnstBadHandle;
    for I := 0 to vLayout.PaperSpaceBlock.Count - 1 do
    begin
      if vLayout.PaperSpaceBlock[I].EntType = ceViewport then
      begin
        vViewPortHandle := vLayout.PaperSpaceBlock[I].Handle;
        Break;
      end;
    end;
    if vViewPortHandle <> cnstBadHandle then
    begin
      vIndex := FCreatedElements.IndexOf(vViewPortHandle, True);
      if vIndex >= 0 then
        Result := FCreatedElements[vIndex].Handle;
    end;
  end;
end;

function TsgCADtoDWG.GetDefChildTableItemsCount(const AHandles: TsgDWGHandleArray): Integer;
begin
  if Version = acR2000 then
    Result := Length(AHandles) - 2 //owner+xdict
  else
    Result := Length(AHandles) - 1; // owner
end;

function TsgCADtoDWG.GetDefVal(const Value, DefValue: Double): Double;
begin
  if IsZero(Value) then
    Result := DefValue
  else
    Result := Value;
end;

function TsgCADtoDWG.GetDictionaryAcadGroupHandle: UInt64;
begin
  Result := GetDictionaryHandle(sAcadGroupDictionary);
end;

function TsgCADtoDWG.GetDictionaryAcadImageDict: UInt64;
begin
  Result := GetDictionaryHandle(sAcadImageDict);
end;

function TsgCADtoDWG.GetDictionaryAcadMaterialsHandle: UInt64;
begin
  Result := GetDictionaryHandle(sAcadMaterialDictionary);
end;

function TsgCADtoDWG.GetDictionaryAcadMLineStyleHandle: UInt64;
begin
  Result := GetDictionaryHandle(sAcadMLineStyleDictionary);
end;

function TsgCADtoDWG.GetDictionaryHandle(const AName: string): UInt64;
begin
  Result := GetEntityHandle(nil, False, cntDWGObjCodeDICTIONARY1, AName);
end;

function TsgCADtoDWG.GetDictionaryLayoutsHandle: UInt64;
begin
  Result := GetDictionaryHandle(sAcadLayoutDictionary);
end;

function TsgCADtoDWG.GetDictionaryNamedObjectHandle: UInt64;
begin
  Result := GetDictionaryHandle(sNamedObjectsDictionary);
end;

function TsgCADtoDWG.GetDictionaryPlotSettingHandle: UInt64;
begin
  Result := GetDictionaryHandle('');//!!!
end;

function TsgCADtoDWG.GetDictionaryPlotStyleHandle: UInt64;
begin
  Result := GetDictionaryHandle('');//!!!
end;

function TsgCADtoDWG.GetDimStyleControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(Converter.Sections[csDimStyles], True,
    cntDWGObjCodeDIMSTYLECONTROLOBJ);
end;

function TsgCADtoDWG.GetDWGCodeByClass(const AClassName: string): Word;
var
  vIndex: Integer;
begin
  vIndex := FClasses.IndexOf(AClassName);
  Result := Word(TsgObjectInt64(FClasses.Objects[vIndex]).FieldInt);
end;

function TsgCADtoDWG.GetDWGCodePage: Word;
begin
  Result := FDWGCodePage;
end;

function TsgCADtoDWG.GetEntHandleByIndex(Group: TsgDXFEntity; Index: Integer; AList: TsgEntitiesList;
  EntIndex: Integer; AFirst: Boolean): UInt64;
var
  vEnt: TsgDXFEntityEx;
  vEntIndex: Integer;
begin
  Result := cnstBadHandle;
  if AList = nil then
  begin
    vEntIndex := Index;
    if (vEntIndex >= 0) and (vEntIndex < Group.Count) then
    begin
      vEnt := TsgDXFEntityEx(Group[vEntIndex]);
      Result := GetEntityHandle(vEnt, False, 0, '', False, AFirst);
    end;
  end
  else
    if (Index >= 0) and (Index <= AList.Count - 1) then
      Result := GetEntityHandle(TsgDXFEntity(AList[Index]), False, 0, '', False, AFirst);
end;

function TsgCADtoDWG.GetEntityHandle(const AEntity: TsgDXFEntity; ADoNewHandle: Boolean = False;
  ADWGCode: Word = cntDWGObjCodeUNSUSED; const AName: string = ''; ADoRaise: Boolean = False;
  AFirst: Boolean = True): UInt64;
var
  I, vCount, vIndex: Integer;

  function DerefEntHandle(const AEnt: TsgDXFEntity): UInt64;
  begin
    if AEnt.EntType = ceMLeader then
      Result := TsgCADMultiLeaderAccess(AEnt).RenderObject.Handle
    else
      Result := AEnt.Handle;
  end;

begin
  vCount := 0;
  Result := cnstBadHandle;
  if AEntity <> nil then
  begin
    if CreateGroup(AEntity) then
    begin
      if AFirst then
        Result := DerefEntHandle(TsgDXFEntityEx(AEntity).Group[0])
      else
        Result := DerefEntHandle(TsgDXFEntityEx(AEntity).Group[TsgDXFEntityEx(AEntity).Group.Count - 1])
    end
    else
      Result := DerefEntHandle(AEntity);
  end
  else
  begin
    if ADWGCode > 0 then
    begin
      vCount := FCreatedElements.CountOf(ADWGCode, vIndex);
      if vCount = 1 then
        Result := FCreatedElements[vIndex].Handle
      else
      begin
        if AName <> '' then
        begin
          for I := vIndex to vIndex + vCount - 1 do
            if SameText(AName, FCreatedElements[I].Name) then
            begin
              Result := FCreatedElements[I].Handle;
              Break;
            end;
        end;
      end;
    end;
  end;

  if (Result = cnstBadHandle) and ADoNewHandle then
  begin
    Result := DoHandle;
    if AEntity <> nil then
      AEntity.Handle := Result
    else
    begin
      if (ADWGCode > 0) and (vCount = 0) then
        FCreatedElements.AddElement(Result, 0, ADWGCode);
    end;
  end;
  if ADoRaise and (Result = cnstBadHandle) then
    raise Exception.Create(sHandleInvalid);
end;

function TsgCADtoDWG.GetExportExceptionClass: ExceptClass;
begin
  Result := EDWGExportError;
end;

function TsgCADtoDWG.GetExtensionDictionary(AEntity: TsgDXFEntity;
  const AName: string): TsgDXFEntity;
var
  vDictionary: TsgDXFDictionary;
begin
  Result := nil;
  case AEntity.EntType of
    ceAttdef, ceAttrib, ceInsert:
      begin
        vDictionary := TsgDXFDictionary(AEntity.Dictionary);
        if Assigned(vDictionary) then
          Result := vDictionary.FindEntByName(AName);
      end;
  end;
end;

function TsgCADtoDWG.GetFileDepListSectionIndex: Integer;
begin
  Result := GetAppInfoSectionIndex + 1;
end;

function TsgCADtoDWG.GetFileHeaderSectionIndex: Integer;
begin
  Result := GetLocatorsCount;
end;

function TsgCADtoDWG.GetFingerPrintGUID: string;
begin
  Result := '{E44D33DF-D2B9-48AE-8431-27F629E25DDD}'; //AutoDesc {F536BED7-6F9E-468E-8820-242279AE2C2C} ODA {E44D33DF-D2B9-48AE-8431-27F629E25DDD}
end;

function TsgCADtoDWG.GetHandleByEntity(AEntity: TsgDXFEntity): UInt64;
begin
  if AEntity = nil then
    Result := cnstBadHandle
  else
  begin
    Result := AEntity.Handle;
    DoError(Result = cnstBadHandle, sHandleInvalid);
  end;
end;

function TsgCADtoDWG.GetHeaderVarsOffset: Cardinal;
begin
  Result := GetPreViewImageOffset + Sections[GetPreviewSectionIndex].Size;
end;

function TsgCADtoDWG.GetHeaderVarsSectionIndex: Integer;
begin
  Result := 0;
end;

function TsgCADtoDWG.GetLastActiveViewportHandle(ALayout: TsgDXFLayout): UInt64;
var
  I: Integer;
begin
  Result := cnstBadHandle;
  if ALayout = nil then
    Exit
  else
    if ALayout.IsModel then Exit;
  for I := 0 to ALayout.PaperSpaceBlock.Count - 1 do
    if ALayout.PaperSpaceBlock.Entities[I] is TsgDXFViewport then
    begin
      Result := ALayout.PaperSpaceBlock.Entities[I].Handle;
      Break;
    end;
end;

function TsgCADtoDWG.GetLayerControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(Converter.Sections[csLayers], True,
    cntDWGObjCodeLAYERCONTROLOBJ);
end;

function TsgCADtoDWG.GetLineTypeHandle(LineType: TsgDXFLineType): UInt64;
var
  vLineTypeName: string;
begin
  if LineType = nil then
    vLineTypeName := sContinuous
  else
    vLineTypeName := LineType.Name;

  Result := GetEntityHandle(Converter.LTypeByName(vLineTypeName));
end;

function TsgCADtoDWG.GetLineTypeControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(Converter.Sections[csLTypes], True,
    cntDWGObjCodeLTYPECONTROLOBJ);
end;

function TsgCADtoDWG.GetLocatorsCount: Integer;
begin
  Result := cntDWGR15SectionsCount;
end;

function TsgCADtoDWG.GetMeasurementSectionIndex: Integer;
begin
  Result := 4;
end;

function TsgCADtoDWG.GetMLineStyleHandle: UInt64;
var
  vMLineStyle: TsgMLineStyle;
begin
  vMLineStyle := nil;
  if Assigned(Converter.Sections[csMLineStyles]) then
  begin
    vMLineStyle := TsgMLineStyle(Converter.Sections[csMLineStyles].FindEntByName(sStandardName));
    if not Assigned(vMLineStyle) and (Converter.Sections[csMLineStyles].Count > 0) then
      vMLineStyle := TsgMLineStyle(Converter.Sections[csMLineStyles][0]);
  end;
  Result := GetEntityHandle(vMLineStyle, True, cntDWGObjCodeMLINESTYLE, sStandardName);
end;

function TsgCADtoDWG.GetModelAndPaperSpaceBlockHandle(const AIndex: Integer): UInt64;
var
  vBlock: TsgDXFBlock;
  P: ^UInt64;
  vLayout: TsgDXFLayout;
begin
  Result := 0;
  vBlock := nil;
  case AIndex of
    0:
      begin
        vBlock := ExportCADImage.Layouts[0].PaperSpaceBlock;
        P := @FModelSpaceHandle;
      end
  else
    begin
      vLayout := GetPaperLayout;
      if vLayout <> nil then
        vBlock := vLayout.PaperSpaceBlock;
      P := @FPaperSpaceHandle;
    end;
  end;
  if P^ <> cnstBadHandle then
  begin
    Result := P^;
    Exit;
  end;
  if vBlock <> nil then
    Result := vBlock.BlockRecord.Handle;
  if Result <> cnstBadHandle then
    P^ := Result
  else
  begin
    Result := DoHandle;
    P^ := Result;
  end;
end;

function TsgCADtoDWG.GetModelSpaceBlockHandle: UInt64;
begin
  Result := GetModelAndPaperSpaceBlockHandle(0);
end;

function TsgCADtoDWG.GetNextEntHandle(Group: TsgDXFEntity; Index: Integer;
  AList: TsgEntitiesList = nil): UInt64;
begin
  Result := GetEntHandleByIndex(Group, Index + 1, AList, Index, True);
end;

function TsgCADtoDWG.GetObjectMapSectionIndex: Integer;
begin
  Result := 2;
end;

function TsgCADtoDWG.GetObjectsFileOffset: Cardinal;
begin
  Result := Sections[GetFileHeaderSectionIndex].Size +
    Sections[GetAuxHeaderSectionIndex].Size +
    Sections[GetPreviewSectionIndex].Size +
    Sections[GetHeaderVarsSectionIndex].Size +
    Sections[GetClassesSectionIndex].Size;
end;

function TsgCADtoDWG.GetObjectsSectionIndex: Integer;
begin
  Result := GetPreviewSectionIndex + 1;
end;

function TsgCADtoDWG.GetObjFreeSpaceOffset: Cardinal;
begin
  Result := GetObjMapOffset + Sections[GetObjectMapSectionIndex].Size;
end;

function TsgCADtoDWG.GetObjFreeSpaceSectionIndex: Integer;
begin
  Result := 3;
end;

function TsgCADtoDWG.GetObjMapOffset: Cardinal;
begin
  Result := GetObjectsFileOffset + Sections[GetObjectsSectionIndex].Size;
end;

function TsgCADtoDWG.GetPaperSpaceBlockHandle: UInt64;
begin
  Result := GetModelAndPaperSpaceBlockHandle(1);
end;

function TsgCADtoDWG.GetPaperSpaceLayoutHandle(PIndex: PInteger = nil): UInt64;
var
  vIndex: Integer;
  vLayout: TsgDXFLayout;
begin
  vLayout := GetPaperLayout;
  if vLayout = nil then
  begin
    vIndex := FCreatedElements.IndexOf(Word(cntDWGObjCodeLAYOUT));
    DoError(vIndex < 0, sPaperSpaceLayoutIsNil);
    Result := FCreatedElements[vIndex].Handle;
    if PIndex <> nil then
      PIndex^ := vIndex
  end
  else
    Result := vLayout.Handle;
end;

function TsgCADtoDWG.GetPrevEntHandle(Group: TsgDXFEntity; Index: Integer;
   AList: TsgEntitiesList = nil): UInt64;
begin
  Result := GetEntHandleByIndex(Group, Index - 1, AList, Index, False);
end;

function TsgCADtoDWG.GetPreviewImageHeigth: Integer;
begin
  Result := 85;
end;

function TsgCADtoDWG.GetPreViewImageOffset: Cardinal;
begin
  Result := cntDWGR15FileHeaderSize + Sections[GetAuxHeaderSectionIndex].Size;
end;

function TsgCADtoDWG.GetPreviewImageRect: TRect;
begin
  Result := Rect(0, 0, GetPreviewImageWidth, GetPreviewImageHeigth);
end;

function TsgCADtoDWG.GetPreviewImageWidth: Integer;
begin
  Result := 180;
end;

function TsgCADtoDWG.GetPreviewSectionIndex: Integer;
begin
  Result := GetFileHeaderSectionIndex + 1;
end;

function TsgCADtoDWG.GetPrototypeSectionIndex: Integer;
begin
  Result := GetRevHistorySectionIndex + 1;
end;

function TsgCADtoDWG.GetRealDWGCode(const DWGCode: Word): Word;
begin
  Result := DWGCode;
  if Result > cntDWGObjCodeUSER then
  begin
    case Result of
      cntDWGObjCodeMPOLYGON:
        Result := GetDWGCodeByClass(cntClassDXFMPOLYGON);
      cntDWGObjCodeWIPEOUT:
        Result := GetDWGCodeByClass(cntClassDXFWIPEOUT);
      cntDWGObjCodeIMAGE:
        Result := GetDWGCodeByClass(cntClassDXFIMAGE);
      cntDWGObjCodeIMAGEDEF:
        Result := GetDWGCodeByClass(cntClassDXFIMAGEDEF);
      cntDWGObjCodeFLATHATCH:
        Result := GetRealDWGCode(cntDWGObjCodeMPOLYGON); //cntDWGObjCodeHATCH;
      cntDWGObjCodeRASTERVARIABLES:
         Result := GetDWGCodeByClass(cntClassDXFRASTERVARIABLES);
      cntDWGObjCodeMESH:
        Result := GetDWGCodeByClass(cntClassDXFMESH);
      cntDWGObjCodeUNKNOWNOBJ:
        Result := cntDWGObjCodePOINT;
      cntDWGObjCodeARC_DIMENSION:
        Result := GetDWGCodeByClass(cntClassDXFARC_DIMENSION);
    end;
  end;
end;

function TsgCADtoDWG.GetRevHistorySectionIndex: Integer;
begin
  Result := GetFileDepListSectionIndex + 1;
end;

function TsgCADtoDWG.GetSecondHeaderOffset: Cardinal;
begin
  Result := GetObjectsFileOffset + Sections[GetObjectsSectionIndex].Size +
    Sections[GetObjectMapSectionIndex].Size + Sections[GetObjFreeSpaceSectionIndex].Size;
end;

function TsgCADtoDWG.GetSecondHeaderSectionIndex: Integer;
begin
  Result := GetObjectsSectionIndex + 1;
end;

function TsgCADtoDWG.GetSection(Index: Integer): TMemoryStream;
begin
  Result := TMemoryStream(FSections[Index]);
end;

function TsgCADtoDWG.GetSectionsCount: Integer;
begin
  case Version of
    acR2000:
      Result := GetLocatorsCount + 4;
    acR2004, acR2010, acR2013, acR2018:
      Result := 16;
  else
    Result := 0;
  end;
end;

function TsgCADtoDWG.GetStyleControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(Converter.Sections[csStyles], True,
    cntDWGObjCodeSTYLECONTROLOBJ);
end;

function TsgCADtoDWG.GetStyleHandle(Style: TsgDXFEntity; ADimStyle: Boolean): UInt64;
var
  vStyleName: string;
begin
  vStyleName := '';
  if Assigned(Style) then
    vStyleName := TsgDXFEntityEx(Style).GetName;
  if ADimStyle then
    Result := GetEntityHandle(Converter.DimensionStyleByName(vStyleName))
  else
    Result := GetEntityHandle(Converter.StyleByName(vStyleName));
end;

function TsgCADtoDWG.GetSummaryInfoSectionIndex: Integer;
begin
  Result := GetSecondHeaderSectionIndex + 1;
end;

function TsgCADtoDWG.GetTemplateOffset: Cardinal;
begin
  Result := GetSecondHeaderOffset + Sections[GetSecondHeaderSectionIndex].Size;
end;

function TsgCADtoDWG.GetUCSontrolObjectHandle: UInt64;
begin
  Result := GetEntityHandle(nil, False, cntDWGObjCodeUCSCONTROLOBJ);
end;

function TsgCADtoDWG.GetVersionGUID: string;
begin
  Result := '{FAEB1C32-E019-11D5-929B-00C0DF256EC4}';
end;

function TsgCADtoDWG.GetViewControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(nil, False, cntDWGObjCodeVIEWCONTROLOBJ);
end;

function TsgCADtoDWG.GetViewPortEntityHeaderControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(nil, False, cntDWGObjCodeVPENTHDRCTRLOBJ);
end;

function TsgCADtoDWG.GetVPortControlObjectHandle: UInt64;
begin
  Result := GetEntityHandle(Converter.Sections[csVPorts], True,
    cntDWGObjCodeVPORTCONTROLOBJ);
end;

function TsgCADtoDWG.GetXdep(const AName: string): Byte;
begin
  if IsXRefInName(AName) then
    Result := 1
  else
    Result := 0;
end;

function TsgCADtoDWG.GetXrefHandle(Entity: TsgDXFEntity): TsgDWGHandle;
var
  BlockRecord: TsgDXFBlockRecord;
begin
  BlockRecord := EntXrefBlock(Entity, Converter);
  Result := CreateDWGHandle(GetEntityHandle(BlockRecord){cnstBadHandle}, cntDWGObjHandleHardPointer);
end;

function TsgCADtoDWG.IsEntityHasNotEED(const Entity: TsgDXFEntity): Boolean;
begin
  Result := (Entity.EntType = ceViewport) or {(Entity is TsgSVGContainer) or}
    (Entity.EntClass.EG = gtSVG);
end;

function TsgCADtoDWG.IsPaperSpace(ABlock: TsgDXFBlock): Boolean;
var
  vLayout: TsgDXFLayout;
begin
  vLayout := GetPaperLayout;
  if vLayout = nil then
    Result := True
  else
    Result := vLayout.PaperSpaceBlock = ABlock;
end;

function TsgCADtoDWG.IsPaperSpace(const AName: string; ABlock: TsgDXFBlock): Boolean;
begin
  Result := False;
  if GetPaperIndex(AName) = 1 then
    Result := IsPaperSpace(ABlock);
end;

procedure TsgCADtoDWG.OnWriteDimStyleControlObject(ABits: TsgDWGBits);
begin
  if Version >= acR2000 then
    ABits.WriteRC(0);
end;

procedure TsgCADtoDWG.__AddDWGSGInserts(const APaperSpaceBlock: TsgDXFBlock; const AInserts: TsgObjectList);
var
  I: Integer;
  vEntity: TsgDXFEntity;
  vIsLoaded: Boolean;
begin
  vIsLoaded := APaperSpaceBlock.IsLoaded;
  APaperSpaceBlock.IsLoaded := False;
  if Assigned(AInserts) then
    for I := 0 to AInserts.Count - 1 do
    begin
      vEntity := TsgDXFEntity(AInserts[I]);
      APaperSpaceBlock.AddEntity(vEntity);
    end;
  APaperSpaceBlock.IsLoaded := vIsLoaded;
end;

procedure TsgCADtoDWG.ProcBlock(ABlock: TsgDXFBlock; const AName: string;
  Data: Pointer; APaperSpaceIndex: Integer);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vBlockRecordHandle, vOwnerHandle, vBlockEntHandle, vLayoutHandle, vDictionary: UInt64;
  vOffset: TFPoint;
  I, J, vIndex, vAttDefCount: Integer;
  vBlock: TsgDXFBlock;
  vLayout: TsgDXFLayout;
  vNeedFreeBlock, vNoNeedEntities: Boolean;
  vHandles: TsgDWGHandleArray;
  vEnt: TsgDXFEntity;
  vOwnedEntHandles: TsgInt64List;
  vInserts: TsgObjectList;
  vEntity: TsgDXFEntityEx;
  vPictureUpdate: Boolean;
  vBlockEntryName, vBlockrecordEntryName: string;
  vFlags: Byte;
{$IFDEF SG_BLOCK_PREVIEW}
  vMS: TMemoryStream;
{$ENDIF}

  procedure CheckLayer(const ABlockCur: TsgDXFBlock);
  begin
    if not Assigned(ABlockCur.Layer) then
    begin
      ABlockCur.Layer := Converter.LayerByName('0');
      if ABlockCur.Layer = nil then
        ABlockCur.Layer := Converter.LayerByName(Converter.HeadVarStruct.CLayer);
    end;
    ABlockCur.BlockRecord.Layer := ABlockCur.Layer;
  end;

  function GetEntHnd(AFirst: Boolean): UInt64;
  begin
    Result := cnstBadHandle;
    if (vBlock = nil) or (vOwnedEntHandles.Count = 0) or vNoNeedEntities then Exit;
    if AFirst then
      Result := vOwnedEntHandles[0]
    else
      Result := vOwnedEntHandles[vOwnedEntHandles.Count - 1];
  end;

  procedure AddOwnedHandle(E: TsgDXFEntity);
  begin
    if (E <> nil) and (E.Handle  <> cnstBadHandle) then
      vOwnedEntHandles.Add(E.Handle);
  end;

  function DoAddHandle(ACode: Byte; AHandle: UInt64 = cnstBadHandle): TsgDWGHandle;
  begin
    Result := CreateDWGHandle(AHandle, ACode);
    AddHandleToArray(Result, vHandles);
  end;

  procedure FindDictionary(AOwnerHandle: UInt64; var ADictionary: UInt64);
  var
    K: Integer;
    vPElem: PsgDWGCreatedElement;
  begin
    ADictionary := cnstBadHandle;
    K := 0;
    while (K < FCreatedElements.Count) and (ADictionary = cnstBadHandle) do
    begin
      vPElem := FCreatedElements.GetPtrElement(K);
      if (vPElem^.DWGObjCode = cntDWGObjCodeDICTIONARY1) and
         (vPElem^.OwnerHandle = AOwnerHandle) then
        ADictionary := vPElem^.Handle;
      Inc(K);
    end;
  end;

begin
  vBlockEntryName := AName;
  vBlockrecordEntryName := AName;
  if ABlock <> nil then
    vFlags := ABlock.Flags
  else
    vFlags := $40;
  GetCorrectBlockFlagsAndName(ABlock, vFlags, vBlockEntryName);
  vNeedFreeBlock := False;
  vLayout := nil;
  vInserts := nil;
  vBits := CreateDWGBitsByVersion(Version);
  vOwnedEntHandles := TsgInt64List.Create;
  try
    vCode := cntDWGObjCodeBLOCKHEADER;

    vOwnerHandle := GetBlockControlObjectHandle;
    vBlockRecordHandle := cnstBadHandle;
    vLayoutHandle := cnstBadHandle;

    vBlock := ABlock;

    vNoNeedEntities := False;
    if vBlock = nil then
    begin
      I := GetPaperIndex(vBlockEntryName);
      case I of
        0:
          begin
            vLayout := Converter.Layouts[0];
            vBlock := vLayout.PaperSpaceBlock;
            CheckLayer(vBlock);
            vBlockRecordHandle := ModelSpaceBlockHandle;
            vLayoutHandle := vLayout.Handle;
          end;
        1:
          if APaperSpaceIndex = -1 then
          begin
            vBlockRecordHandle := PaperSpaceBlockHandle;
            vLayout := GetPaperLayout;
            if vLayout <> nil then
              vBlock := vLayout.PaperSpaceBlock;
            if vBlock = nil then
            begin
              vBlock := TsgDXFBlock.Create;
              vBlock.Name := vBlockEntryName;
              vBlock.Handle := DoHandle;
              CheckLayer(vBlock);
              vBlock.BlockRecord.Handle := vBlockRecordHandle;
              vNeedFreeBlock := True;
            end;
          end;
      end;
      if Assigned(vLayout) then
      begin
        vInserts := ExpSGInsert(vLayout);
        __AddDWGSGInserts(vLayout.PaperSpaceBlock, vInserts);
      end;
    end
    else
    begin
      vBlockRecordHandle := vBlock.BlockRecord.Handle;
      if APaperSpaceIndex <> -1 then
      begin
        vInserts := ExpSGInsert(Converter.Layouts[APaperSpaceIndex]);
        __AddDWGSGInserts(vBlock, vInserts);
      end;
      vBlockEntryName := vBlock.Name;
    end;
    if not SplitAnonymousName(vBlockEntryName, vBlockrecordEntryName) then
      vBlockrecordEntryName := vBlockEntryName
    else
      vBlockrecordEntryName := cnstAsterisk + vBlockrecordEntryName;
    if vBlockRecordHandle = cnstBadHandle then
      vBlockRecordHandle := DoHandle;
    DoError(vBlock = nil, sBlockIsNil);
    vBlockEntHandle := vBlock.Handle;
    vIndex := FCreatedEndBlk.AddPair(vBlock.BlockRecord.Handle, DoHandle);

    FindDictionary(vBlockRecordHandle, vDictionary);
    AddCommonEntityData(vBlockRecordHandle, vCode, vBits, nil, nil, 3, nil, nil, vDictionary);

    vNoNeedEntities := Assigned(ABlock) and TsgDXFBlockEx(ABlock).IsBlockByXRef; //(vFlags and 4 <> 0) or (vFlags and 8 <> 0) or ((Assigned(ABlock) and (ABlock.XrefPath <> '')));

    vAttDefCount := 0;
    vFlags := vFlags or $02;
    vOwnedEntHandles.ProcCompare := TsgPointerTypeComparer.CmpUInt64;
    vOwnedEntHandles.Sorted := False;
    for I := 0 to vBlock.Count - 1 do
    begin
      vPictureUpdate := False;
      vEntity := TsgDXFEntityEx(vBlock[I]);
      try
        case vEntity.EntType of
          ceAttdef: Inc(vAttDefCount);
          ceImageEnt: vPictureUpdate := TsgDXFImageEntAccess(vEntity).PictureUpdate(ExportCADImage);
          ceMLeader: vEntity := TsgDXFEntityEx(TsgCADMultiLeaderAccess(vEntity).RenderObject);
        end;
        if vEntity.GroupExists and CreateGroup(vEntity) then
        begin
          for J := 0 to vEntity.Group.Count - 1 do
            if J = 0 then
              vOwnedEntHandles.Add(GetEntHandleByIndex(vEntity.Group, J, nil, J, True))
            else
              if J = vEntity.Group.Count - 1 then
                vOwnedEntHandles.Add(GetEntHandleByIndex(vEntity.Group, J, nil, J, False))
              else
                AddOwnedHandle(vEntity.Group[J]);
        end
        else
          AddOwnedHandle(vEntity);
      finally
        if vPictureUpdate then
          TsgDXFImageEntAccess(vEntity).PictureUpdate(nil);
      end;
    end;
//    vOwnedEntHandles.Sort; //!!!

    if vAttDefCount = 0 then
      vFlags := vFlags and not $02;
    AddCommonTableItemFlags(vBits, nil, vBlockrecordEntryName, 0, @vFlags);
    vBits.WriteBit(IsAnonymousBlock(vFlags));// Anonymous B 1 if this is an anonymous block (1 bit)
    vBits.WriteBit(vFlags, 2);// Hasatts B 1 if block contains attdefs (2 bit)
    vBits.WriteBit(vFlags, 4);//Blkisxref B 1 if block is xref (4 bit)
    vBits.WriteBit(vFlags, 8);//Xrefoverlaid B 1 if an overlaid xref (8 bit)
    vBits.WriteBit(0);// Loaded Bit B 0 indicates loaded for an xref  !!! maybe  if ABlock <> nil then vBits.WriteBit(BoolToBit(ABlock.XrefPath <> nil));

    if (vFlags and $C = 0) and (Version >= acR2004) then
      vBits.WriteBL(vOwnedEntHandles.Count);//Number of objects owned by this object

    if ABlock <> nil then
      vOffset := ABlock.Offset
    else
      vOffset := cnstFPointZero;

    vBits.Write3BD(vOffset);

    if ABlock <> nil then
      vBits.WriteTV(ABlock.XrefPath, FCodePage)
    else
      vBits.WriteTV('', FCodePage);

    //Insert Count RC
    //A sequence of zero or more non-zero RC’s, followed by a terminating 0 RC.
    //The total number of these indicates how many insert handles will be present.
    for I := 0 to vBlock.References.Count - 1 do
      if TsgDXFEntity(vBlock.References[I]).EntType = ceInsert then
        vBits.WriteRC(1);

    vBits.WriteRC(0);

    if ABlock <> nil then
      vBits.WriteTV(ABlock.Description, FCodePage)//Block Description TV 4 Block description. ''
    else
      vBits.WriteTV('', FCodePage);

    //Size of preview data BL Indicates number of bytes of data following.
{$IFDEF SG_BLOCK_PREVIEW}
    vMS := nil;
    if (ABlock <> nil) and (ABlock.BlockRecord.Preview.Graphic <> nil) then
    begin
      if ABlock.BlockRecord.Preview.Graphic is TsgBitmap then
      begin
        vMS := TMemoryStream.Create;
        TsgBitmapAccess(ABlock.BlockRecord.Preview.Graphic).WriteImage(vMS);
        vMS.Position := 0;
      end
      else
        if ABlock.BlockRecord.Preview.Graphic is TBitmap then
        begin
          vMS := TMemoryStream.Create;
          TBitmap(ABlock.BlockRecord.Preview.Graphic).SaveToStream(vMS);
          vMS.Position := SizeOf(TBitmapFileHeader);
        end;
    end;
    try
      if Assigned(vMS) and (vMS.Size - vMS.Position > 0) then
      begin
        vBits.WriteBL(vMS.Size - vMS.Position);
        vBits.WriteBytes(Pointer(TsgNativeInt(vMS.Memory) + vMS.Position), vMS.Size - vMS.Position);
      end
      else
{$ENDIF}
        vBits.WriteBL(0);
{$IFDEF SG_BLOCK_PREVIEW}
    finally
      vMS.Free;
    end;
{$ENDIF}

    if Version >= acR2007 then
    begin
      vBits.WriteBS(0);//Insert units BS 70;
      vBits.WriteBit(1);//Explodable B 280;
      vBits.WriteRC(0);//Block scaling RC 281
    end;

    if IsPaperLayout(vBlockEntryName) then
    begin
      if APaperSpaceIndex <> -1 then
        vLayoutHandle := Converter.Layouts[APaperSpaceIndex].Handle
      else
        vLayoutHandle := GetPaperSpaceLayoutHandle;
    end;

    //Handles
    AddCommonHandleRefs(vHandles, vOwnerHandle, nil, vDictionary);
    // NULL
    DoAddHandle(cntDWGObjHandleHardPointer);
    // block entity
    DoAddHandle(cntDWGObjHandleHardOwner, vBlockEntHandle);
    if Version < acR2004 then
    begin
      if not vNoNeedEntities then
      begin
        DoAddHandle(cntDWGObjHandleSoftPointer, GetEntHnd(True));
        DoAddHandle(cntDWGObjHandleSoftPointer, GetEntHnd(False));
      end
    end
    else
    begin
      for I := 0 to vOwnedEntHandles.Count - 1 do
        DoAddHandle(cntDWGObjHandleHardOwner, vOwnedEntHandles[I]);
    end;
    DoAddHandle(cntDWGObjHandleHardOwner, FCreatedEndBlk.Second[vIndex]);
    for I := 0 to vBlock.References.Count - 1 do
    begin
      vEnt := TsgDXFEntity(vBlock.References[I]);
      if vEnt.EntType = ceInsert then
        DoAddHandle(cntDWGObjHandleSoftPointer, vEnt.Handle);
    end;
    DoAddHandle(cntDWGObjHandleHardPointer, vLayoutHandle);

    AddHandlesUpdateAndFinish(vBits, vHandles, vBlockRecordHandle, vCode);
    ProcBlockEnt(vBlock, vBlockEntryName, @vBlockRecordHandle);
    TsgInt64List(Data).Add(vBlockRecordHandle);
  finally
    vBits.Free;
    vOwnedEntHandles.Free;
    if vNeedFreeBlock then
    begin
      TsgObjectList.FreeList(vInserts);
      vBlock.Free;
    end
    else
    begin
      if Assigned(vLayout) or (APaperSpaceIndex <> -1) then
      begin
        FSGInserts.Add(vInserts);
        vInserts := nil;
      end
      else
        TsgObjectList.FreeList(vInserts);
    end;
  end;
end;

procedure TsgCADtoDWG.ProcBlockControl(BlockHandles: TsgInt64List);
var
  I: Integer;
  vOwnerHandle: UInt64;
  vHandles: TsgDWGHandleArray;
begin
  //Handles
  vOwnerHandle := GetBlockControlObjectHandle;
  AddCommonHandleRefs(vHandles, cnstBadHandle);

  for I := 2 to BlockHandles.Count - 1 do
    AddHandleToArray(CreateDWGHandle(BlockHandles[I], cntDWGObjHandleSoftOwner),
      vHandles);

    AddHandleToArray(CreateDWGHandle(BlockHandles[0],  //Model space
      cntDWGObjHandleHardOwner), vHandles);

    AddHandleToArray(CreateDWGHandle(BlockHandles[1],  //Paper space
      cntDWGObjHandleHardOwner), vHandles);

  ProcControlObject(vOwnerHandle, cntDWGObjCodeBLOCKCONTROLOBJ,
    BlockHandles.Count - 2, vHandles);
end;

procedure TsgCADtoDWG.PrepareDrawingDatabase;

  procedure AddScale(const Name: string; const Param1, Param2: Double; Param3: Byte);
  var
    P: PsgDWGScale;
  begin
    New(P);
    P^.ScaleParam1 := Param1;
    P^.ScaleParam2 := Param2;
    P^.ScaleParam3 := Param3;
    FScalesDictionary.AddObject(Name, TsgObjectWithField.CreatePointer(P));
  end;
begin
  inherited PrepareDrawingDatabase;

  AddScale('A0=1:1', 1, 1, 1);
  AddScale('A1=1:2', 1, 2, 0);
  AddScale('A2=1:4', 1, 4, 0);
  AddScale('A3=1:5', 1, 5, 0);
  AddScale('A4=1:8', 1, 8, 0);
  AddScale('A5=1:10', 1, 10, 0);
  AddScale('A6=1:16', 1, 16, 0);
  AddScale('A7=1:20', 1, 20, 0);
  AddScale('A8=1:30', 1, 30, 0);
  AddScale('A9=1:40', 1, 40, 0);
  AddScale('B0=1:50', 1, 50, 0);
  AddScale('B1=1:100', 1, 100, 0);
  AddScale('B2=2:1', 2, 1, 0);
  AddScale('B3=4:1', 4, 1, 0);
  AddScale('B4=8:1', 8, 1, 0);
  AddScale('B5=10:1', 10, 1, 0);
  AddScale('B6=100:1', 100, 1, 0);
  AddScale('B7=1/128" = 1''-0"', 0.0078125, 12, 0);
  AddScale('B8=1/64" = 1''-0"', 0.015625, 12, 0);
  AddScale('B9=1/32" = 1''-0"', 0.03125, 12, 0);
  AddScale('C0=1/16" = 1''-0"', 0.0625, 12, 0);
  AddScale('C1=3/32" = 1''-0"', 0.09375, 12, 0);
  AddScale('C2=1/8" = 1''-0"', 0.125, 12, 0);
  AddScale('C3=3/16" = 1''-0"', 0.1875, 12, 0);
  AddScale('C4=1/4" = 1''-0"', 0.25, 12, 0);
  AddScale('C5=3/8" = 1''-0"', 0.375, 12, 0);
  AddScale('C6=1/2" = 1''-0"', 0.5, 12, 0);
  AddScale('C7=3/4" = 1''-0"', 0.75, 12, 0);
  AddScale('C8=1" = 1''-0"', 1, 12, 0);
  AddScale('C9=1-1/2" = 1''-0"', 1.5, 12, 0);
  AddScale('D0=3" = 1''-0"', 3, 12, 0);
  AddScale('D1=6" = 1''-0"', 6, 12, 0);
  AddScale('D2=1''-0" = 1''-0"', 12, 12, 0);

  FVariableDictionary.Values['CANNOSCALE'] := '1:1';
  FVariableDictionary.Values['DIMASSOC'] := IntToStr(Converter.HeadVarStruct.DimAssoc);
  FVariableDictionary.Values['HIDETEXT'] := '1';
  if Converter.OleFrame <> cnstOleFrameDefault then
    FVariableDictionary.Values[sDictionaryVarOLEFRAME] := IntToStr(Converter.OleFrame);
  if Converter.HeadVarStruct.XClipFrame <> cnstDefHeadVarStruct.XClipFrame then
    FVariableDictionary.Values[sDictionaryVarXCLIPFRAME] := IntToStr(Converter.HeadVarStruct.XClipFrame);
  if Version >= acR2007 then
  begin
    FVariableDictionary.Values['PSOLHEIGHT'] := '80.000000';
    FVariableDictionary.Values['PSOLWIDTH'] := '5.000000';
  end;
end;

function TsgCADtoDWG.PrepareTextForDWG(const Text: string): string;
begin
  Result := Text;
  ReplaceAnsi(Result, '^I', #09);
  ReplaceAnsi(Result, '^J', #$0A);
end;

procedure TsgCADtoDWG.ProcRasterVariables(const ARasterVariables: TsgCADRasterVariables;
  const OwnerHandle: UInt64);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vHandles: TsgDWGHandleArray;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vCode := GetRealDWGCode(cntDWGObjCodeRASTERVARIABLES);
    AddCommonEntityData(ARasterVariables.Handle, vCode, vBits);
    vBits.WriteBL(0);
    vBits.WriteBS(Ord(ARasterVariables.DisplayImageFrame));
    vBits.WriteBS(Ord(ARasterVariables.ImageDisplayQuality));
    vBits.WriteBS(ARasterVariables.UnitsForInsertingImages);
    AddCommonHandleRefs(vHandles, OwnerHandle);
    AddHandlesUpdateAndFinish(vBits, vHandles, ARasterVariables.Handle, vCode);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcMLineStyle(const AStyle: TsgMLineStyle; const OwnerHandle: UInt64);
var
  vStyle: TsgMLineStyle;
  vBits: TsgDWGBits;
  vCode: Word;
  I: Integer;
  vHandles: TsgDWGHandleArray;
  vMLineEntry: TsgMLineEntry;

  function DXFCodesToDWG(AFlags: SmallInt): SmallInt;
    procedure ChangeBit(Source: SmallInt; var Dest: SmallInt; si, di: Word);
    var
      val: Boolean;
    begin
      val := Word(Source) and si <> 0;
      if val then
        Dest := Dest or SmallInt(di)
      else
        Dest := Dest and not SmallInt(di);
    end;
  begin
    Result := AFlags;
    ChangeBit(AFlags, Result, 1, 2);
    ChangeBit(AFlags, Result, 2, 1);

    ChangeBit(AFlags, Result, 64, 32);
    ChangeBit(AFlags, Result, 32, 64);

    ChangeBit(AFlags, Result, 1024, 512);
    ChangeBit(AFlags, Result, 512, 1024);
  end;

  function LTypeBS(const AMLineEntry: TsgMLineEntry): Word;
  begin
    Result := $7FFF;
    if AMLineEntry.LineType <> nil then
    begin
      Result := Converter.Sections[csLTypes].IndexOfEntity(AMLineEntry.LineType);
      if Result >= 2 then
        Result := Result - 2
      else
        Result := $7FFF;
      //Result := Word(AMLineEntry.LType);
    end;
  end;

begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vStyle := AStyle;
    if vStyle = nil then
    begin
      vStyle := TsgMLineStyle.Create;
      vStyle.Name := sStandardName;
      vStyle.NewEntry(0.5, cnstColorCADByLayer, Converter.LTypeByName(sByLayer));
      vStyle.NewEntry(-0.5, cnstColorCADByLayer, Converter.LTypeByName(sByLayer));
      vStyle.Handle := GetMLineStyleHandle;
    end;
    try
    vCode := cntDWGObjCodeMLINESTYLE;
    AddCommonEntityData(vStyle.Handle, vCode, vBits);
    if Version <= acR2004 then
      vBits.WriteTV(UpperCase(Copy(vStyle.Name, 1, 31)), FCodePage) //Name TV Name of this style
    else
      vBits.WriteTV(vStyle.Name, FCodePage); //Name TV Name of this style
    vBits.WriteTV('', FCodePage); //Desc TV Description of this style
    vBits.WriteBS(DXFCodesToDWG(vStyle.Flags)); // Flags BS A short which reconstitutes the mlinestyle flags as defined in DXF. Here are the bits as they relate to DXF:
    vBits.WriteCMC(vStyle.ColorCAD); //fillcolor CMC Fill color for this style
    vBits.WriteBD(1.5707963267949); //startang BD Start angle
    vBits.WriteBD(1.5707963267949);//endang BD End angle
    vBits.WriteRC(TsgMLineStyleAccess(vStyle).GetCount); //linesinstyle RC Number of lines in this style
    for I := 0 to TsgMLineStyleAccess(vStyle).GetCount - 1 do
    begin
      vMLineEntry := vStyle.Entries[I] as TsgMLineEntry;
      vBits.WriteBD(vMLineEntry.Offset); //0.5 -0.5 Offset BD Offset of this segment
      vBits.WriteCMC(vMLineEntry.ColorCAD); //Color CMC Color of this segment
      if Version < acR2018 then
        vBits.WriteBS(LTypeBS(vMLineEntry)); //Ltindex BS Linetype index (yes, index)
    end;
    AddCommonHandleRefs(vHandles, OwnerHandle);
    if Version >= acR2018 then
      for I := 0 to TsgMLineStyleAccess(vStyle).GetCount - 1 do
      begin
        vMLineEntry := vStyle.Entries[I] as TsgMLineEntry;
        AddHandleToArray(CreateDWGHandle(CastEntToID(vMLineEntry.LineType), cntDWGObjHandleHardPointer), vHandles);
      end;
    AddHandlesUpdateAndFinish(vBits, vHandles, vStyle.Handle, vCode);
    finally
      if AStyle = nil then
        vStyle.Free;
    end;
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcBlockEnt(ABlock: TsgDXFBlock; const AName: string;
  Data: Pointer; ACurrPaperSpaceIndex: Integer = -1);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vHandle, vOwnerHandle: UInt64;
  vEntMode, vLTypeFlags: Byte;
  I: Integer;
  vData: TsgDirectExportData;
  vEnt: TsgDXFEntity;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vCode := cntDWGObjCodeBLOCK;
    CheckEntHandle(ABlock, True);
    vHandle := ABlock.Handle;
    if Data <> nil then
      vOwnerHandle := PUInt64(Data)^
    else
      vOwnerHandle := cnstBadHandle;
    vEntMode := 0;
    I := GetPaperIndex(AName);
    case I of
      0: vEntMode := 2;
      1:
        if IsPaperSpace(ABlock) then
          vEntMode := 1;
    end;
    AddCommonEntityData(vHandle, vCode, vBits, nil, ABlock, vEntMode, @vLTypeFlags);
    vBits.WriteTV(AName, FCodePage);
    AddCommonEntityHandleData(vHandle, vBits, ABlock, vOwnerHandle, vCode,
      vEntMode, vLTypeFlags, 0, True, cnstBadHandle, cnstBadHandle, nil, cnstBadHandle);
    if vEntMode = 0 then //MODEL_SPACE and PAPER_SPACE will export entites later
    begin
      for I := 0 to ABlock.Count - 1 do
      begin
        vEnt := ABlock.Entities[I];
        if vEnt.EntType = ceMLeader then
          vEnt := TsgCADMultiLeaderAccess(vEnt).RenderObject;
        vData := CreateExportData(vOwnerHandle, 0, GetPrevEntHandle(ABlock, I),
          GetNextEntHandle(ABlock, I));
        DoExport(vEnt, @vData);
      end;
    end;
    vData := CreateExportData(vOwnerHandle, vEntMode, cnstBadHandle,
      cnstBadHandle, 0, nil, cntDWGObjCodeENDBLK);
    DoExport(ABlock, @vData); //exports ENDBLK
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcControlObject(const AHandle: UInt64; ACode: Word;
  ANumentries: Cardinal; const AHandles: array of TsgDWGHandle; ReadEvent: TsgDWGReadEvent = nil);
var
  vBits: TsgDWGBits;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    DoError(AHandle = cnstBadHandle, sCannotFindTableCode, [ACode]);
    AddCommonEntityData(AHandle, ACode, vBits);
    vBits.WriteBL(ANumentries);
    if Assigned(ReadEvent) then
      ReadEvent(vBits);
   //Handles
    AddHandlesUpdateAndFinish(vBits, AHandles, AHandle, ACode);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcDictionary(const Handle, OwnerHandle: UInt64;
  var Items: TsgDWGLinkItems; const AHardOwnerFlag: Byte = 0);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vName: string;
  vHandles: TsgDWGHandleArray;
  I: Integer;
  vReactors: TsgInt64List;
begin
  vReactors := TsgInt64List.Create;
  vBits := CreateDWGBitsByVersion(Version);
  try
    vCode := cntDWGObjCodeDICTIONARY1;
    if OwnerHandle <> cnstBadHandle then //??? not sure
      vReactors.Add(OwnerHandle);
    AddCommonEntityData(Handle, vCode, vBits, vReactors);
    vBits.WriteBL(Length(Items)); //Numitems BL number of dictonary items
    vBits.WriteBS(1); //Cloning flag BS 281      1
    vBits.WriteRC(AHardOwnerFlag); //Hard Owner flag RC 280
    AddCommonHandleRefs(vHandles, OwnerHandle, vReactors);
    for I := Low(Items) to High(Items) do
    begin
      vName := Items[I].Name;
      if vName = cntEmptyDictionary then
        vName := '';
      vBits.WriteTV(vName, FCodePage); //Text TV string name of dictionary entry, numitems entries
      AddHandleToArray(CreateDWGHandle(Items[I].Handle, cntDWGObjHandleSoftOwner),
        vHandles);
    end;
    AddHandlesUpdateAndFinish(vBits, vHandles, Handle, vCode);
  finally
    vReactors.Free;
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcDimStyle(const AOwnerHandle: UInt64;
  ADimStyle: TsgDXFDimensionStyle; var AProcHandles: TsgDWGHandleArray);
var
  vCode: Word;
  vBits: TsgDWGBits;
  vHandles: TsgDWGHandleArray;

  function GetBlockEntHandle(const ABlock: TsgDXFBlock): UInt64;
  begin
    if (ABlock <> nil) and (ABlock.Name <> sgDimensionArrowTypeNames[datClosedfilled]) then
      Result := GetEntityHandle(ABlock.BlockRecord) //GetEntityHandle(ADimStyle.DIMLDRBLK)
    else
      Result := cnstBadHandle;
  end;

begin
  if ADimStyle = nil then Exit;
  vCode := cntDWGObjCodeDIMSTYLE;
  vBits := CreateDWGBitsByVersion(Version);
  try
    AddCommonEntityData(ADimStyle.Handle, vCode, vBits);
    AddCommonTableItemFlags(vBits, ADimStyle, ADimStyle.Name, 0, nil);
    vBits.WriteTV(ADimStyle.DIMPOST, FCodePage);//DIMPOST TV 3           ''
    vBits.WriteTV('', FCodePage);//DIMAPOST TV 4          ''

    vBits.WriteBD(ADimStyle.Scale); //DIMSCALE BD 40               1
    vBits.WriteBD(ADimStyle.ArrowSize); //DIMASZ BD 41             0,18
    vBits.WriteBD(ADimStyle.ExtLineOffset); //DIMEXO BD 42         0,0625
    vBits.WriteBD(0.38);//DIMDLI BD 43                             0,38
    vBits.WriteBD(ADimStyle.ExtLineExt);//DIMEXE BD 44             0,18
    vBits.WriteBD(0);//DIMRND BD 45                                0
    vBits.WriteBD(0);//DIMDLE BD 46                                0
    vBits.WriteBD(ADimStyle.DIMTP);//DIMTP BD 47                   0
    vBits.WriteBD(ADimStyle.DIMTM);//DIMTM BD 48                   0

    if Version >= acR2007 then
    begin
      vBits.WriteBD(1);//DIMFXL BD 49
      vBits.WriteBD(0.785398163397448);//DIMJOGANG BD 50
      vBits.WriteBD(0);//DIMTFILL BS 69
      vBits.WriteCMC(cnstColorCADByBlock);//DIMTFILLCLR CMC 70
    end;

    vBits.WriteBit(0);//DIMTOL B 71                                0
    vBits.WriteBit(0);//DIMLIM B 72                                0
    vBits.WriteBit(ADimStyle.DIMTIH);//DIMTIH B 73                 1
    vBits.WriteBit(ADimStyle.DIMTOH);//DIMTOH B 74                 1
    vBits.WriteBit(ADimStyle.DIMSE1);//DIMSE1 B 75                 0
    vBits.WriteBit(ADimStyle.DIMSE2);//DIMSE2 B 76                 0
    vBits.WriteBS(Ord(ADimStyle.TextPosVert));//DIMTAD BS 77      0
    vBits.WriteBS(0);//DIMZIN BS 78                                0
    vBits.WriteBS(0);//DIMAZIN BS 79                               0

    if Version >= acR2007 then
      vBits.WriteBS(0);//DIMARCSYM BS 90

    vBits.WriteBD(GetDefVal(ADimStyle.TextHeight, 2.5));//DIMTXT BD 140            0,18
    vBits.WriteBD(ADimStyle.SizeCenterMark);//DIMCEN BD 141        0
    vBits.WriteBD(0);//DIMTSZ BD 142                               0
    vBits.WriteBD(24.4);//DIMALTF BD 143                           25,4
    vBits.WriteBD(GetDefVal(ADimStyle.DIMLFAC, 1));//DIMLFAC BD 144              1
    vBits.WriteBD(0);//DIMTVP BD 145                               0
    vBits.WriteBD(1);//DIMTFAC BD 146                              1
    vBits.WriteBD(ADimStyle.TextOffset);//DIMGAP BD 147            0,0625
    vBits.WriteBD(0);//DIMALTRND BD 148                            0
    vBits.WriteBit(0);//DIMALT B 170                               0
    vBits.WriteBS(2);//DIMALTD BS 171                              2
    vBits.WriteBit(0);//DIMTOFL B 172                              0
    vBits.WriteBit(0);//DIMSAH B 173                               0
    vBits.WriteBit(ADimStyle.DIMTIX <> 0);//DIMTIX B 174                0
    vBits.WriteBit(0);//DIMSOXD B 175                              0
    vBits.WriteCMC(ADimStyle.DIMCLRD);//DIMCLRD BS 176             0
    vBits.WriteCMC(ADimStyle.DIMCLRE);//DIMCLRE BS 177             0
    vBits.WriteCMC(ADimStyle.DIMCLRT);//DIMCLRT BS 178             0
    vBits.WriteBS(0);//DIMADEC BS 179                              0
    vBits.WriteBS(ADimStyle.DIMDEC);//DIMDEC BS 271                4
    vBits.WriteBS(4);//DIMTDEC BS 272                              4
    vBits.WriteBS(2);//DIMALTU BS 273                              2
    vBits.WriteBS(2);//DIMALTTD BS 274                             2
    vBits.WriteBS(0);//DIMAUNIT BS 275                             0
    vBits.WriteBS(ADimStyle.DIMFRAC);//DIMFRAC BS 276                              0
    vBits.WriteBS(GetDimLimitUnitsByte(ADimStyle.DIMLUNIT));//DIMLUNIT BS 277   2
    vBits.WriteBS(Ord(ADimStyle.DIMDSEP));//DIMDSEP BS 278                      46
    vBits.WriteBS(0);//DIMTMOVE BS 279                             0
    vBits.WriteBS(0);//DIMJUST BS 280                              0
    vBits.WriteBit(ADimStyle.DIMSD1);//DIMSD1 B 281                0
    vBits.WriteBit(ADimStyle.DIMSD2);//DIMSD2 B 282                0
    vBits.WriteBS(1);//DIMTOLJ BS 283                              1
    vBits.WriteBS(0);//DIMTZIN BS 284                              0
    vBits.WriteBS(0);//DIMALTZ BS 285                              0
    vBits.WriteBS(0);//DIMALTTZ BS 286                             0
    vBits.WriteBit(0);//DIMUPT B 288                               0
    vBits.WriteBS(3);//DIMFIT BS 287                               3

    if Version >= acR2007 then
    begin
      vBits.WriteBit(0);//DIMFXLON B 290
      if Version >= acR2010 then
      begin
        vBits.WriteBit(0);//DIMTXTDIRECTION B 295
        vBits.WriteBD(100);//DIMALTMZF BD ?
        vBits.WriteTV('', FCodePage);//DIMALTMZS T ?
        vBits.WriteBD(100);//DIMMZF BD ?
        vBits.WriteTV('', FCodePage);//DIMMZS T ?
      end;
    end;

    vBits.WriteBS(CorrectLineWeight(ADimStyle.DIMLWD, False));//DIMLWD BS 371      65535
    vBits.WriteBS(CorrectLineWeight(ADimStyle.DIMLWE, False));//DIMLWE BS 372      65535
    vBits.WriteBit(0);//Unknown B 70 Seems to set the 0-bit (1) of the 70-group. 0

    AddCommonHandleRefs(vHandles, AOwnerHandle);

    AddHandleToArray(XrefHandle[ADimStyle], vHandles);

    AddHandleToArray(CreateDWGHandle(GetStyleHandle(ADimStyle.TextStyle, False),
      cntDWGObjHandleHardPointer), vHandles);//Text Style

    AddHandleToArray(CreateDWGHandle(GetBlockEntHandle(ADimStyle.DIMLDRBLK), //blocks
      cntDWGObjHandleHardPointer), vHandles);
    AddHandleToArray(CreateDWGHandle(GetBlockEntHandle(ADimStyle.DIMBLK),
      cntDWGObjHandleHardPointer), vHandles);
    AddHandleToArray(CreateDWGHandle(GetBlockEntHandle(ADimStyle.DIMBLK1),
      cntDWGObjHandleHardPointer), vHandles);
    AddHandleToArray(CreateDWGHandle(GetBlockEntHandle(ADimStyle.DIMBLK2),
      cntDWGObjHandleHardPointer), vHandles);

    if Version >= acR2007 then
    begin
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles);//345 dimltype (hard pointer)
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles);//346 dimltex1 (hard pointer)
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles);//347 dimltex2 (hard pointer)
    end;

    AddHandlesUpdateAndFinish(vBits, vHandles, ADimStyle.Handle, vCode);
    AddHandleToArray(CreateDWGHandle(ADimStyle.Handle, cntDWGObjHandleSoftOwner),
      AProcHandles);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcDimStylesControlObject(const AHandle: UInt64;
  const ADimStyleHandles: TsgDWGHandleArray);
begin
  ProcControlObject(AHandle, cntDWGObjCodeDIMSTYLECONTROLOBJ, GetDefChildTableItemsCount(ADimStyleHandles),
    ADimStyleHandles, OnWriteDimStyleControlObject);
end;

procedure TsgCADtoDWG.ProcDWGProps(const Handle, OwnerHandle: UInt64);
var
  vBits, vXRecordBits: TsgDWGBits;
  vCode: Word;
  vXRecord: TsgCADExtendedData;
  I: Integer;
  vKey, vVal: string;
  vHandles: TsgDWGHandleArray;
  vReactors: TsgInt64List;
begin
  vReactors := TsgInt64List.Create;
  vBits := CreateDWGBitsByVersion(Version);
  vXRecordBits := CreateDWGBitsByVersion(Version);
  vXRecord := TsgCADExtendedData.Create(Version);
  try
    vCode := cntDWGObjCodeXRECORD;
    vReactors.Add(OwnerHandle);
    AddCommonEntityData(Handle, vCode, vBits, vReactors);

    vXRecord.AddString(cntXCodeDrwPropBase, sDWGPropsXRecord);
    vXRecord.AddString(cntXCodeDrwPropTitle, DrwPropTitle);
    vXRecord.AddString(cntXCodeDrwPropSubject, DrwPropSubject);
    vXRecord.AddString(cntXCodeDrwPropAuthor, DrwPropAuthor);
    vXRecord.AddString(cntXCodeDrwPropComments, DrwPropComments);
    vXRecord.AddString(cntXCodeDrwPropKeywords, DrwPropKeywords);
    vXRecord.AddString(cntXCodeDrwPropLastSavedBy, DrwPropSavedBy);
    vXRecord.AddString(cntXCodeDrwPropRevisionNumber, DrwPropRevisionNumber);

    for I := cntXCodeDrwPropCustomSummaryInfo1 to cntXCodeDrwPropCustomSummaryInfo9 do
    begin
      vKey := '';
      vVal := '';
      if I - cntXCodeDrwPropCustomSummaryInfo1 <= DrwPropCustomSummaryInfo.Count - 1 then
      begin
        vKey := DrwPropCustomSummaryInfo.Names[I - cntXCodeDrwPropCustomSummaryInfo1];
        vVal := DrwPropCustomSummaryInfo.Values[vKey];
      end;
      vXRecord.AddString(I, vKey + '=' + vVal);
    end;
    vXRecord.AddDouble(cntXCodeDrwPropUnknown40, 0);
    vXRecord.AddDouble(cntXCodeDrwPropUnknown41, 0);
    vXRecord.AddDouble(cntXCodeDrwPropUnknown42, 0);
    vXRecord.AddString(cntXCodeDrwPropHyperlinkBase, DrwPropHyperlinkBase);
    vXRecord.AddInt(cntXCodeDrwPropCustomSummaryInfoCount, Min(DrwPropCustomSummaryInfo.Count, 10));
    AddXRecordDirrect(vXRecordBits, vXRecord);
    vBits.WriteBL(vXRecordBits.Size);
    vBits.WriteBytes(vXRecordBits.Data, vXRecordBits.Size);
    vBits.WriteBS(1);
    AddCommonHandleRefs(vHandles, OwnerHandle, vReactors);
    AddHandlesUpdateAndFinish(vBits, vHandles, Handle, vCode);
  finally
    vReactors.Free;
    vBits.Free;
    vXRecordBits.Free;
    vXRecord.Free;
  end;
end;

procedure TsgCADtoDWG.ProcessExport(Stream: TStream);
const
  arPad: array[0..6] of AnsiChar = (#0, #0, #0, #0, #0, #6, #1);
  cnstFileHeaderTailSeed = $6A15E95D;

var
  vDateCreate, vDateUpdate: TDateTime;
  vHandSeed: UInt64;
  PFileHeader: PdwgFileHeader;
  vLocators: TsgDWGR15Locators;
  vStrVer: AnsiString;
  vCRC: Word;
  vSize, vHeaderOffsetPos, vHeaderOffsetPosValue: Cardinal;
  vBmpOffsetPos, vBmpOffsetPosValue: Cardinal;
  vPaddingSize: Integer;

  vFileHeader2004: PsgDWG2004NonCryptedFileHeader;
  vEncryptedFileHeader2004: PsgDWG2004EncryptedFileHeader;
  vTail: Pointer;
  vDest: TMemoryStream;
  vSectionBuilder: TdwgR18SectionMapBuilder;
  vPreviewSectionIndex, vSummarySectionInfoIndex: Integer;
  P: PByte;
  vBits: TsgDWGBits;
  vObjOffsetForFreeSpace: Cardinal;
begin
//DWG R2000 FILE STRUCTURE
//FILE HEADER                  |
//LOCATORS                     | 81 bytes
//CRC                          |
//FILE HEADER END SENTINEL     |
//AuxHeader                    | size 123 offset 97
//Preview image                | size variable offset 220 (97 + 123)
//Classes                      | size variable offset 17318
//Unknown area, padding        | $80 (128 dec) 0 32 bit UInt (Cardinal(s))

//CLASS DEFINITIONS
//TEMPLATE (R13 only, optional)
//PADDING (R13C3 AND LATER, 200 bytes, minutes the template section above if present)
//IMAGE DATA (PRE-R13C3)
//OBJECT DATA
//All entities, table entries, dictionary entries, etc. go in this
//section.
//OBJECT MAP
//OBJECT FREE SPACE (optional)
//TEMPLATE (R14-R15, optional)
//SECOND HEADER
//IMAGE DATA (R13C3 AND LATER)

  Progress(psStarting, 0, '');
  CreateTablesRecord;
  GetCreateAndUpdateDateTime(vDateCreate, vDateUpdate);// all dates may be zeros in JD
  vStrVer := AnsiString(sgDWGVerToString(Version));
  case Version of
    acR2000:
      vPaddingSize := cntPaddingSize;
    acR2004..DWGVersionHigh:
      vPaddingSize := 8;
  else
    vPaddingSize := 0;
  end;

  ExportSectionClasses(vPaddingSize); //we have to add $80 * SizeOf(UInt32) zeros to the end of Classes section, the padding
  Progress(psRunning, 10, '');
  ExportSectionTables; //export objects must be after classes
  vHandSeed := DoHandle; //After export Objects the HandSeed is known
  FillChar(vLocators, SizeOf(vLocators), 0);
  Progress(psRunning, 70, '');
    //File Header
  WriteByteToStream(Sections[GetFileHeaderSectionIndex], 0,
    cntDWGR15FileHeaderSize - SizeOf(TsgDWGSentinel));
  Sections[GetFileHeaderSectionIndex].Write(cntFileHeaderEndSentinel,
    SizeOf(cntFileHeaderEndSentinel));
  Progress(psRunning, 80, '');
  //Aux Header
  ExportSectionAuxHeader(vHandSeed, vDateCreate, vDateUpdate);

  //Preview
  ExportSectionPreview(Sections[GetFileHeaderSectionIndex].Size +
    Sections[GetAuxHeaderSectionIndex].Size, vHeaderOffsetPos, vHeaderOffsetPosValue,
    vBmpOffsetPos, vBmpOffsetPosValue);

  //Header Vars
  ExportSectionHeaderVars(vHandSeed, vDateCreate, vDateUpdate);

  //Classes were before

  //objects see ExportSectionTables
  //ExportSectionObjects;

  //handles (Objects Map)
  ExportSectionObjMap;

  //free space aka unknown section
  if Version = acR2000 then
    vObjOffsetForFreeSpace := GetObjectsFileOffset
  else
    vObjOffsetForFreeSpace := 0;

  ExportSectionObjFreeSpace(vDateUpdate, FObjectsMap.Count + 4, vObjOffsetForFreeSpace);

  //template
  ExportSectionMeasurement(Converter.HeadVarStruct.Measurement);
  Progress(psRunning, 90, '');

  case Version of
    acR2000:
      begin
        //Prepare all known section locators
        //DWG files need the first four locators in the SecondHeader
        vLocators[0].RecNumber := 0; //HeaderVars
        vLocators[0].Seeker := GetHeaderVarsOffset;
        vLocators[0].Size := Sections[GetHeaderVarsSectionIndex].Size;

        vLocators[1].RecNumber := 1;//Classes
        vLocators[1].Seeker := GetClassesOffset;
        vLocators[1].Size := Sections[GetClassesSectionIndex].Size - cntPaddingSize;

        vLocators[2].RecNumber := 2;//ObjMap
        vLocators[2].Seeker := GetObjMapOffset;
        vLocators[2].Size := Sections[GetObjectMapSectionIndex].Size;

        vLocators[3].RecNumber := 3;//ObjFreeSpace
        vLocators[3].Seeker := GetObjFreeSpaceOffset;
        vLocators[3].Size := Sections[GetObjFreeSpaceSectionIndex].Size;

        vLocators[4].RecNumber := 4;//Template, data locator will be known later
        vLocators[4].Seeker := 0;
        vLocators[4].Size := Sections[GetMeasurementSectionIndex].Size;

        vLocators[5].RecNumber := 5;//AuxHeader
        vLocators[5].Seeker := GetAuxHeaderOffset;
        vLocators[5].Size := Sections[GetAuxHeaderSectionIndex].Size;

        //second header
        ExportSectionSecondHeader(vHandSeed, GetSecondHeaderOffset, vLocators);

        vLocators[4].RecNumber := 4;//Template
        vLocators[4].Seeker := GetTemplateOffset;
        vLocators[4].Size := Sections[GetMeasurementSectionIndex].Size;

        //prepare FileHeader
        PFileHeader := PdwgFileHeader(Sections[GetFileHeaderSectionIndex].Memory);
        Move(vStrVer[1], PFileHeader^.VersionID, SizeOf(PFileHeader^.VersionID)); //version
        Move(arPad, PFileHeader^.Pad, SizeOf(PFileHeader^.Pad)); //pad
        PFileHeader^.CodePage := FDWGCodePage;
        PFileHeader^.Unknown := 1565;
        PFileHeader^.Locators := cntDWGR15SectionsCount;
        PFileHeader^.ImageSeeker := GetPreViewImageOffset;

        Sections[GetFileHeaderSectionIndex].Position := SizeOf(TdwgFileHeader);
        Sections[GetFileHeaderSectionIndex].Write(vLocators, SizeOf(vLocators));

        vSize := Sections[GetFileHeaderSectionIndex].Position;

        vCRC := CRC8(Sections[GetFileHeaderSectionIndex].Memory, 0, vSize) xor cnstFileHeaderCRCXOR[cntDWGR15SectionsCount];
        Sections[GetFileHeaderSectionIndex].Write(vCRC, SizeOf(vCRC));

        Progress(psRunning, 95, '');
        Stream.Write(Sections[GetFileHeaderSectionIndex].Memory^, Sections[GetFileHeaderSectionIndex].Size);
        Stream.Write(Sections[GetAuxHeaderSectionIndex].Memory^, Sections[GetAuxHeaderSectionIndex].Size);
        Stream.Write(Sections[GetPreviewSectionIndex].Memory^, Sections[GetPreviewSectionIndex].Size);
        Stream.Write(Sections[GetHeaderVarsSectionIndex].Memory^, Sections[GetHeaderVarsSectionIndex].Size);
        Stream.Write(Sections[GetClassesSectionIndex].Memory^, Sections[GetClassesSectionIndex].Size);
        Stream.Write(Sections[GetObjectsSectionIndex].Memory^, Sections[GetObjectsSectionIndex].Size);
        Stream.Write(Sections[GetObjectMapSectionIndex].Memory^, Sections[GetObjectMapSectionIndex].Size);
        Stream.Write(Sections[GetObjFreeSpaceSectionIndex].Memory^, Sections[GetObjFreeSpaceSectionIndex].Size);
        Stream.Write(Sections[GetSecondHeaderSectionIndex].Memory^, Sections[GetSecondHeaderSectionIndex].Size);
        Stream.Write(Sections[GetMeasurementSectionIndex].Memory^, Sections[GetMeasurementSectionIndex].Size);
      end;
    acR2004 .. acR2018:
      begin
        ExportSectionPrototype;
        ExportSectionSummaryInfo(vDateCreate, vDateUpdate);
        ExportSectionAppInfo;
        ExportSectionFileDepList;
        ExportSectionRevHistory;
        //prepare FileHeader
        Progress(psRunning, 95, '');

        vDest := TMemoryStream.Create;
        try
          vDest.Size := $100;
          vDest.Position := vDest.Size;

          vSectionBuilder := TdwgR18SectionMapBuilder.Create(vDest);
          try
            vSectionBuilder.Add(nil, '');
            vSummarySectionInfoIndex := vSectionBuilder.Add(Sections[GetSummaryInfoSectionIndex], sAcadSectSummaryInfo);
            vPreviewSectionIndex := vSectionBuilder.Add(Sections[GetPreviewSectionIndex], sAcadSectPreview);
            //vSectionBuilder.Add(nil, sAcadSectVBAProject); // optional
            vSectionBuilder.Add(Sections[GetAppInfoSectionIndex], sAcadSectAppInfo);
            vSectionBuilder.Add(Sections[GetFileDepListSectionIndex], sAcadSectFileDepList);
            vSectionBuilder.Add(nil{Sections[GetRevHistorySectionIndex]}, sAcadSectRevHistory);
            //vSectionBuilder.Add(nil, sAcadSectSecurity);   // optional
            vSectionBuilder.Add(Sections[GetObjectsSectionIndex], sAcadSectObjects);
            vSectionBuilder.Add(Sections[GetObjFreeSpaceSectionIndex], sAcadSectObjFreeSpace);
            vSectionBuilder.Add(Sections[GetMeasurementSectionIndex], sAcadSectTemplate);
            vSectionBuilder.Add(Sections[GetObjectMapSectionIndex], sAcadSectHandles);
            vSectionBuilder.Add(Sections[GetClassesSectionIndex], sAcadSectClasses);
            vSectionBuilder.Add(Sections[GetAuxHeaderSectionIndex], sAcadSectAuxHeader);
            vSectionBuilder.Add(Sections[GetHeaderVarsSectionIndex], sAcadSectHeader);
            if (Version >= acR2013) and (Sections[GetPrototypeSectionIndex].Size > 0) then
              vSectionBuilder.Add(Sections[GetPrototypeSectionIndex], sAcadSectPrototype);
            // we do not write sAcadSectSignature

            vSectionBuilder.Build;

            vDest.Position := 0;

            vFileHeader2004 := PsgDWG2004NonCryptedFileHeader(vDest.Memory);
            FillChar(vFileHeader2004^, SizeOf(vFileHeader2004^), 0);

            Move(vStrVer[1], vFileHeader2004^.VersionID, SizeOf(vFileHeader2004^.VersionID)); //version
            FillChar(vFileHeader2004^.Padding1, SizeOf(vFileHeader2004^.Padding1), 0);
            vFileHeader2004^.ReleaseVersion := MaintenanceVersion;
            vFileHeader2004^.UnknownByte1 := 3;
            vFileHeader2004^.PreviewOffset := vSectionBuilder[vPreviewSectionIndex].DestStartPos + SizeOf(TsgDWG2004DataPageHeader);

            P := vDest.Memory;
            Inc(P, vFileHeader2004^.PreviewOffset);
            vBits := CreateDWGBitsByVersion(Version, P, vDest.Size - vFileHeader2004^.PreviewOffset);
            try
              vBits.UpdateRL(vHeaderOffsetPos, vFileHeader2004^.PreviewOffset + vHeaderOffsetPosValue);
              vBits.UpdateRL(vBmpOffsetPos, vFileHeader2004^.PreviewOffset + vBmpOffsetPosValue);
            finally
              vBits.Free;
            end;

            vFileHeader2004^.OrigFileSavedVer := FOrigFileSavedVer;
            vFileHeader2004^.OrigFileSavedReleaseVer := FOrigFileSavedReleaseVer;
            vFileHeader2004^.Codepage := DWGCodePage;
            vFileHeader2004^.OrigFileSavedVerRepeat := vFileHeader2004^.OrigFileSavedVer;
            vFileHeader2004^.OrigFileSavedReleaseVerRepeat := vFileHeader2004^.OrigFileSavedReleaseVer;

            vFileHeader2004^.SecurityFlags := 0;
            vFileHeader2004^.UnknownLong := 0;
            vFileHeader2004^.SummaryInfoAddress := vSectionBuilder[vSummarySectionInfoIndex].DestStartPos + SizeOf(TsgDWG2004DataPageHeader);
            vFileHeader2004^.VBAProjectAddress := 0;
            vFileHeader2004^.Padding3 := $00000080;
            FillChar(vFileHeader2004^.Padding4, SizeOf(vFileHeader2004^.Padding4), 0);

            vEncryptedFileHeader2004 := PsgDWG2004EncryptedFileHeader(vFileHeader2004);
            Inc(PByte(vEncryptedFileHeader2004), SizeOf(TsgDWG2004NonCryptedFileHeader));
            System.Move(Pointer(cntDWG2004FileIDStr)^, vEncryptedFileHeader2004^.ACADVer[0],
              Length(cntDWG2004FileIDStr) + 1);
            vEncryptedFileHeader2004^.Field1 := $00;
            vEncryptedFileHeader2004^.Field2 := $6C;
            vEncryptedFileHeader2004^.Field3 := $04;
            vEncryptedFileHeader2004^.RootTreeNodeGap := 0;
            vEncryptedFileHeader2004^.LowermostLeftTreeNodeGap := 0;
            vEncryptedFileHeader2004^.LowermostRightTreeNodeGap := 0;
            vEncryptedFileHeader2004^.Field4 := $01;
            vEncryptedFileHeader2004^.LastSectionPageId := vSectionBuilder.FSectionPageMapID;
            vEncryptedFileHeader2004^.LastSectionPageEndAddress := vSectionBuilder.FLastPageEndOffset - $100;
            vEncryptedFileHeader2004^.SecondHeaderAddress := vDest.Size;
            vEncryptedFileHeader2004^.GapAmount := 0;
            vEncryptedFileHeader2004^.PagesAmount := Length(vSectionBuilder.PageItems);
            vEncryptedFileHeader2004^.Field5 := $20;
            vEncryptedFileHeader2004^.Field6 := $80;
            vEncryptedFileHeader2004^.Field7 := $40;
            vEncryptedFileHeader2004^.SectionPageMapId := vSectionBuilder.FSectionPageMapID;
            vEncryptedFileHeader2004^.SectionPageMapAddress := vSectionBuilder.FSectionPagesMapOffest - $100;
            vEncryptedFileHeader2004^.SectionMapId := vSectionBuilder.FSectionMapID;
            vEncryptedFileHeader2004^.PagesMaxId := vEncryptedFileHeader2004^.SectionPageMapId;
            vEncryptedFileHeader2004^.GapArraySize := 0;
            vEncryptedFileHeader2004^.CRC32 := 0;
            vEncryptedFileHeader2004^.CRC32 := CRC32(PByte(vEncryptedFileHeader2004), 0, SizeOf(TsgDWG2004EncryptedFileHeader));//FI:W508 ignore
            sgDWG2004Encode(vEncryptedFileHeader2004, SizeOf(TsgDWG2004EncryptedFileHeader));
            // fill file header tail by magic F8 46 6A 04...
            vTail := vEncryptedFileHeader2004;
            Inc(PByte(vTail), SizeOf(TsgDWG2004EncryptedFileHeader));
            FillChar(vTail^, $14, 0);
            sgDWG2004Coding(vTail, $14, cnstFileHeaderTailSeed);
            Stream.Write(vDest.Memory^, vDest.Size);
            vDest.Position := vDest.Size;
            Stream.Write(vEncryptedFileHeader2004^, SizeOf(TsgDWG2004EncryptedFileHeader));
          finally
            vSectionBuilder.Free;
          end;
        finally
          vDest.Free;
        end;
      end;
  end;
  Progress(psEnding, 100, '');
end;

procedure TsgCADtoDWG.ProcImageDef(AImageDef: TsgDXFImageDef);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vHandles: TsgDWGHandleArray;
  vImageSize, vPixelSize: TF2DPoint;
  vAcadImageDict: UInt64;
  vReactors: TsgInt64List;
  vSize: TSize;
begin
  vReactors := TsgInt64List.Create;
  vBits := CreateDWGBitsByVersion(Version);
  try
    vAcadImageDict := GetDictionaryAcadImageDict;
    vImageSize := cnstF2DPointZero;
    vPixelSize := cnstF2DPointZero;
    if AImageDef.Picture.Graphic <> nil then
    begin
      vSize := GetSizeGraphic(AImageDef.Picture.Graphic);
      vImageSize := MakeF2DPoint(vSize.cx, vSize.cy);
      vPixelSize :=  MakeF2DPoint(1 / vSize.cx, 1 / vSize.cy);
    end;
    vCode := GetRealDWGCode(cntDWGObjCodeIMAGEDEF);
    vReactors.Add(vAcadImageDict);
    //vReactors.Add(cnstBadHandle); //IMAGEDEF_REACTOR
    AddCommonEntityData(AImageDef.Handle, vCode, vBits, vReactors);
    vBits.WriteBL(0); //Clsver BL 0 class version
    vBits.Write2RD(vImageSize); //Imgsize 2RD 10 size of image in pixels
    vBits.WriteTV(AImageDef.FileName, FCodePage); //Filepath TV 1 path to file
    vBits.WriteBit(1); //Isloaded B 280 0==no, 1==yes  1
    vBits.WriteRC(0); //Resunits RC 281 0==none, 2==centimeters, 5==inches   0
    vBits.Write2RD(vPixelSize);// Pixelsize 2RD 11 size of one pixel in AutoCAD units    0.01, 0.01, 0
    //parenthandle Reference handle (hard owner is mistake in the documentation)
    AddCommonHandleRefs(vHandles, CreateDWGHandleRef(vAcadImageDict, AImageDef.Handle), vReactors);
    AddHandlesUpdateAndFinish(vBits, vHandles, AImageDef.Handle, vCode);
  finally
    vReactors.Free;
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcLayer(const AOwnerHandle: UInt64; ALayer: TsgDXFLayer;
  var ProcHandles: TsgDWGHandleArray; var ZeroLayerIndex: Integer);
var
  vBits: TsgDWGBits;
  vFlags, vLineWeightFlag: Word;
  vHandles: TsgDWGHandleArray;
  vCode: Word;
begin
  if (ALayer = nil) or IsNameInternal(ALayer.Name) then Exit;
  CheckEntHandle(ALayer);
  vBits := CreateDWGBitsByVersion(Version);
  try
    vCode := cntDWGObjCodeLAYER;
    AddCommonEntityData(ALayer.Handle, vCode, vBits);
    AddCommonTableItemFlags(vBits, ALayer, ALayer.Name, 0, nil);
    vLineWeightFlag := CorrectLineWeight(ALayer.LineWeight, True) shl 5 and $3E0;
    vFlags := 0;
    if ALayer.Frozen then
      vFlags := vFlags or 1;
    if not ALayer.Visible then
      vFlags := vFlags or 2;
    if ALayer.IsFrozenByNewViewPort then
      vFlags := vFlags or 4;
    if ALayer.Locked then
      vFlags := vFlags or 8;
    if ALayer.IsPlotting and (not SameText(ALayer.Name, sLayerDefPoints)) then
      vFlags := vFlags or $10;
    vFlags := vFlags or vLineWeightFlag;
    vBits.WriteBS(vFlags); //Values BS 70,290,370 contains frozen (1 bit), on (2 bit), frozen by default in new viewports (4 bit), locked (8 bit), plotting flag (16 bit), and lineweight (mask with 0x03E0)
    vBits.WriteCMC(ALayer.ColorCAD);

    AddCommonHandleRefs(vHandles, AOwnerHandle);

    AddHandleToArray(XrefHandle[ALayer], vHandles); //External reference block handle
    AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles); //Plotstyle (hard pointer), by default points to PLACEHOLDER with handle 0x0f

    if Version >= acR2007 then
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles);// 347 Material

    AddHandleToArray(CreateDWGHandle(GetLineTypeHandle(ALayer.LineType),
      cntDWGObjHandleHardPointer), vHandles);
    AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles);// Unknown handle (hard pointer). Always seems to be NULL.

    AddHandlesUpdateAndFinish(vBits, vHandles, ALayer.Handle, vCode);
    AddHandleToArray(CreateDWGHandle(ALayer.Handle, cntDWGObjHandleSoftOwner), ProcHandles);
    if ALayer.Name = '0' then
      ZeroLayerIndex := High(ProcHandles);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcLayerControlObject(const AHandle: UInt64;
  const ProcLayerHandles: TsgDWGHandleArray);
begin
  ProcControlObject(AHandle, cntDWGObjCodeLAYERCONTROLOBJ, GetDefChildTableItemsCount(ProcLayerHandles),
    ProcLayerHandles);
end;

procedure TsgCADtoDWG.ProcLayout(ALayout: TsgDXFLayout; const Handle, OwnerHandle: UInt64;
  AIndex: Integer; const AName: string);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vName: string;
  vBox: TFRect;
  vHandles: TsgDWGHandleArray;
  vBlockRecordHandle: UInt64;
//  vEntity: TsgDXFEntity;
  vPlotFlags: Word;
  I: Integer;
  vViewports: TsgInt64List;
  vPlotSetting: TsgDXFPlotSettings;
  vEntity: TsgDXFEntity;
  vReactors: TsgInt64List;
  vXDict: UInt64;
  vExtMin, vExtMax, vLimMin, vLimMax: TFPoint;

  function IsPaperSpace: Boolean;
  begin
    Result := (ALayout <> nil) and (not ALayout.IsModel);// = GetPaperLayout);
  end;

  procedure SavePlotSettingToDWG(const APlotSetting: TsgDXFPlotSettings);
  var
    vPS: TsgDXFPlotSettings;
    vFlags: TsgPlotLayoutFlags;
  begin
    vPS := TsgDXFPlotSettings.Create;
    try
      if Assigned(APlotSetting) then
        vPS.AssignEntity(APlotSetting)
      else
        vPS.ResetForDefaultValue;

      vBits.WriteTV('', FCodePage); //Page setup name TV 1 plotsettings page setup name ''
      vBits.WriteTV(vPS.PrintOrConfigName, FCodePage); //Printer/Config TV 2 plotsettings printer or configuration file 'none_device'

      vPlotFlags := 688;
      if ALayout <> nil then
      begin
        if ALayout.IsModel then
          vPlotFlags := 11952;//vPlotFlags or $400;
      end;
      vFlags :=  ConvertIntegerToPlotLayoutFlags(vPlotFlags);
      if plfUseStandardScale in vPS.PlotLayoutFlags then
        vFlags := vFlags + [plfUseStandardScale]
      else
        vFlags := vFlags - [plfUseStandardScale];
      vPlotFlags := ConvertPlotLayoutFlagsToInteger(vFlags);

      vBits.WriteBS(vPlotFlags); //Plot layout flags BS 70 plotsettings plot layout flag 688
      vBits.WriteBD(vPS.UnprintableMargin.Left); //Left Margin BD 40 plotsettings left margin in millimeters
      vBits.WriteBD(vPS.UnprintableMargin.Bottom); //Bottom Margin BD 41 plotsettings bottom margin in millimeters
      vBits.WriteBD(vPS.UnprintableMargin.Right); //Right Margin BD 42 plotsettings right margin in millimeters
      vBits.WriteBD(vPS.UnprintableMargin.Top); //Top Margin BD 43 plotsettings top margin in millimeters
      vBits.WriteBD(vPS.PlotPaperSize.X); //Paper Width BD 44 plotsettings paper width in millimeters
      vBits.WriteBD(vPS.PlotPaperSize.Y); //Paper Height BD 45 plotsettings paper height in millimeters
      vBits.WriteTV(vPS.PaperSize, FCodePage); //Paper Size TV 4 plotsettings paper size
      vBits.Write2BD(vPS.PlotOrigin); //Plot origin 2BD 46,47 plotsettings origin offset in millimeters
      vBits.WriteBS(Word(vPS.PlotPaperUnits)); //Paper units BS 72 plotsettings plot paper units
      vBits.WriteBS(Word(vPS.PlotRotation)); //Plot rotation BS 73 plotsettings plot rotation
      vBits.WriteBS(Word(vPS.PlotType)); //Plot type BS 74 plotsettings plot type
      vBits.Write2BD(vPS.PlotWindowAreaMin); //Window min 2BD 48,49 plotsettings plot window area lower left
      vBits.Write2BD(vPS.PlotWindowAreaMax); //Window max 2BD 140,141 plotsettings plot window area upper right
      if Version = acR2000 then
        vBits.WriteT('', FCodePage); //Plot view name T 6 plotsettings plot view name
      vBits.WriteBD(vPS.NumeratorOfCustomPrintScale); //Real world units BD 142 plotsettings numerator of custom print scale
      vBits.WriteBD(vPS.DenominatorOfCustomPrintScale); //Drawing units BD 143 plotsettings denominator of custom print scale
      vBits.WriteTV('', FCodePage); //Current style sheet TV 7 plotsettings current style sheet
      vBits.WriteBS(0); //Scale type BS 75 plotsettings standard scale type
      vBits.WriteBD(vPS.FloatingPointScaleFactor); //Scale factor BD 147 plotsettings scale factor
      vBits.Write2BD(vPS.PaperImageOrigin); //Paper image origin 2BD 148,149 plotsettings paper image origin
    finally
      vPS.Free;
    end;
  end;

begin
  vReactors := TsgInt64List.Create;
  vBits := CreateDWGBitsByVersion(Version);
  vViewports := nil;
  try
    vCode := GetDWGCodeByClass(cntClassDXFLAYOUT);
    if ALayout = nil then
    begin
      vName := AName;
      vBlockRecordHandle := PaperSpaceBlockHandle;
      vPlotSetting := nil;
    end
    else
    begin
      if AIndex > 0 then
      begin
        vName := ALayout.Name;
        if GetPaperLayout = ALayout then
          vBlockRecordHandle := PaperSpaceBlockHandle
        else
          vBlockRecordHandle := ALayout.PaperSpaceBlock.BlockRecord.Handle;
      end
      else
      begin
        vName := sModel;
        vBlockRecordHandle := ModelSpaceBlockHandle;
      end;
      vPlotSetting := ALayout.PlotSettings;
    end;

    DoError(vBlockRecordHandle = cnstBadHandle, sHandleInvalid);
    vBox.TopLeft := cnstFPointZero;
    vBox.BottomRight := cnstFPointZero;
    if ALayout <> nil then
    begin
      vBox := ALayout.Box;
      vLimMin := ALayout.LimMin;
      vLimMax := ALayout.LimMax;
      vExtMin := ALayout.ExtMin;
      vExtMax := ALayout.ExtMax;
    end
    else
    begin
      vLimMin := cnstFPointZero;
      vLimMax := cnstFPointZero;
      vExtMin := cnstFPointZero;
      vExtMax := cnstFPointZero;
    end;
    vReactors.Add(OwnerHandle);
    vXDict := GetEntityHandle(TsgDXFConverterAccess(Converter).Dictionary[Handle]);
    AddCommonEntityData(Handle, vCode, vBits, vReactors, ALayout, 3, nil, nil, vXDict);

    SavePlotSettingToDWG(vPlotSetting);
    if Version >= acR2004 then
    begin
      vBits.WriteBS(0); //Shade plot mode BS 76
      vBits.WriteBS(0); //Shade plot res. Level BS 77
      vBits.WriteBS(0); //Shade plot custom DPI BS 78
    end;
    vBits.WriteTV(vName, FCodePage); //Layout name TV 1 layout name                                          'Model' 'Layout1'
    vBits.WriteBS(AIndex); //Tab order BS 71 layout tab order                                      0 1
    vBits.WriteBS(1); //Flag BS 70 layout flags                                               1
    vBits.Write3BD(cnstFPointZero); //Ucs origin 3BD 13 layout ucs origin                          0, 0, 0
    vBits.Write2RD(vLimMin); //Limmin 2RD 10 layout minimum limits                                  0, 0, 0
    vBits.Write2RD(vLimMax); //Limmax 2RD 11 layout maximum limits                                  0, 0, 0
    if IsPaperSpace then
      vBits.Write3BD(cnstExtrusion)
    else
      vBits.Write3BD(cnstFPointZero); //Inspoint 3BD 12 layout insertion base point                          0, 0, 0
    vBits.Write3BD(cnstXOrtAxis); //Ucs x axis 3BD 16 layout ucs x axis direction                        1, 0, 0
    vBits.Write3BD(cnstYOrtAxis); //Ucs y axis 3BD 17 layout ucs y axis direction                        0, 1, 0
    vBits.WriteBD(0); //Elevation BD 146 layout elevation                                     0
    vBits.WriteBS(0); //Orthoview type BS 76 layout orthographic view type of UCS             0
    vBits.Write3BD(vExtMin); //Extmin 3BD 14 layout extent min                                      0, 0, 0
    vBits.Write3BD(vExtMax); //Extmax 3BD 15 layout extent max                                      0, 0, 0

    if Version >= acR2004 then
    begin
      vViewports := TsgInt64List.Create;
      vEntity := Converter.Sections[csBlockRecords].FindEntByHandle(vBlockRecordHandle);
      if Assigned(vEntity) then
      begin
        vEntity := TsgDXFBlockRecordAccess(vEntity).Block;
        for I := 0 to vEntity.Count - 1 do
          if vEntity.Entities[I].EntType = ceViewport then
            vViewports.Add(vEntity.Entities[I].Handle);
      end;
      vBits.WriteBS(vViewports.Count); //RL # of viewports in this layout we write BS
    end;

    //Handles
    AddCommonHandleRefs(vHandles, OwnerHandle, vReactors, vXDict);
    if Version >= acR2004 then
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles); //plot view handle (hard pointer)
    if Version >= acR2007 then
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleSoftPointer), vHandles); //Visual Style handle (soft pointer)
    AddHandleToArray(CreateDWGHandle(vBlockRecordHandle, cntDWGObjHandleSoftPointer), vHandles);
    AddHandleToArray(CreateDWGHandle(GetLastActiveViewportHandle(ALayout), cntDWGObjHandleSoftPointer), vHandles);// last active viewport handle (soft pointer)
    AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles);// base ucs handle (hard pointer)
    AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer), vHandles);// named ucs handle (hard pointer)
    if (Version >= acR2004) and Assigned(vViewports) then
      for I := 0 to vViewports.Count - 1 do
        AddHandleToArray(CreateDWGHandle(vViewports[I], cntDWGObjHandleSoftPointer), vHandles); //Viewport handle (repeats Viewport count times) (soft pointer)
    AddHandlesUpdateAndFinish(vBits, vHandles, Handle, vCode);
  finally
    vReactors.Free;
    vBits.Free;
    vViewports.Free;
  end;
end;

procedure TsgCADtoDWG.ProcLType(AOwnerHandle: UInt64; ALType: TsgDXFLineType);
var
  I, vCount: Integer;
  vLen: Double;
  vElem: TsgLTypeElement;
  vBits: TsgDWGBits;
  vStrAreaSize, vStrByteSize: Integer;
  vStrArea: array[0..511] of Byte;
  vHandles: TsgDWGHandleArray;
  P: Pointer;
begin
  if ALType = nil then Exit;
  CheckEntHandle(ALType);
  vStrAreaSize := 0;
  FillChar(vStrArea, SizeOf(vStrArea), #0);
  vBits := CreateDWGBitsByVersion(Version);
  try
    AddCommonEntityData(ALType.Handle, cntDWGObjCodeLTYPE1, vBits);
    vCount := ALType.Lines.ElementsCount;
    AddCommonTableItemFlags(vBits, ALType, ALType.Name, 0, nil);
    vBits.WriteTV(GenLTypeDiscriptText(ALType, vLen), FCodePage); //Description TV 3
    vBits.WriteBD(vLen); //Pattern Len BD 40
    vBits.WriteRC(Byte(AnsiChar('A'))); //Alignment RC 72 Always 'A'
    vBits.WriteRC(vCount); //Numdashes RC 73 The number of repetitions of the 49...74 data.
    AddCommonHandleRefs(vHandles, AOwnerHandle);
    AddHandleToArray(XrefHandle[ALType], vHandles);
    for I := 0 to vCount - 1 do
    begin
      vElem := ALType.Lines.Elements[I];
      vBits.WriteBD(vElem.Dash);
      vBits.WriteBS(vElem.ShapeNumber);
      vBits.WriteRD(vElem.XOffset);
      vBits.WriteRD(vElem.YOffset);
      vBits.WriteBD(vElem.Scale);
      vBits.WriteBD(Radian(vElem.Rotation));
      vBits.WriteBS(vElem.ComplexType and $07);
      if (vElem.ComplexType and 2 <> 0) then //writes only 1 text to the strings area
      begin
        if Version >= acR2007 then
        begin
          P := PWideChar(WideString(vElem.Text));
          vStrAreaSize := 512;
          vStrByteSize := Length(vElem.Text) * SizeOf(WideChar);
        end
        else
        begin
          P := PAnsiChar(AnsiString(vElem.Text));
          vStrAreaSize := 256;
          vStrByteSize := Length(vElem.Text);
        end;
        if vStrByteSize > 0 then
          Move(P^, vStrArea, MinI(vStrAreaSize, vStrByteSize));
      end;
      AddHandleToArray(CreateDWGHandle(GetHandleByEntity(TsgDXFEntity(vElem.Style)), cntDWGObjHandleHardPointer), vHandles);
    end;
    if (Version <= acR2004) and (vStrAreaSize = 0) then
      vStrAreaSize := 256;
    if vStrAreaSize > 0 then
      vBits.WriteBytes(@vStrArea[0], vStrAreaSize);
    AddHandlesUpdateAndFinish(vBits, vHandles, ALType.Handle, cntDWGObjCodeLTYPE1);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcLTypeControlObject(const AHandle: UInt64;
  const ALTypesHandles: array of TsgDWGHandle);
var
  vNumEntries: Integer;
begin
  case Version of
    acR2000: vNumEntries := Length(ALTypesHandles) - 4; //owner+xdict+bylayer+byblock
  else
    vNumEntries := Length(ALTypesHandles) - 3; // owner+bylayer+byblock
  end;
  ProcControlObject(AHandle, cntDWGObjCodeLTYPECONTROLOBJ, vNumEntries, ALTypesHandles);
end;

procedure TsgCADtoDWG.ProcStyle(const AOwner: UInt64; const AStyle: TsgDXFStyle);
var
  vName: string;
  vBits: TsgDWGBits;
  vCode: Word;
  vHandles: TsgDWGHandleArray;
  vFlags: Byte;
begin
  if AStyle = nil then Exit;

  if AStyle.Flags and 1 <> 0 then
  begin
    vName := '';
    vFlags := $40;
  end
  else
  begin
    vName := AStyle.Name;
    vFlags := AStyle.Flags;
  end;
  vCode := cntDWGObjCodeSTYLE1;

  vBits := CreateDWGBitsByVersion(Version);
  try
    AddCommonEntityData(AStyle.Handle, vCode, vBits, nil, AStyle);
    AddCommonTableItemFlags(vBits, nil, vName, 0, @vFlags);
    vBits.WriteBit(AStyle.Flags, 1); //Vertical B 1 if vertical (1 bit of flag)
    vBits.WriteBit(AStyle.Flags, 8); //shape file B 1 if a shape file rather than a font (4 bit)
    vBits.WriteBD(AStyle.FixedHeight); //Fixed height BD 40
    vBits.WriteBD(AStyle.WidthFactor); //Width factor BD 41
    vBits.WriteBD(AStyle.ObliqueAngle / f180DividedByPi); //Oblique ang BD 50
    vBits.WriteRC(AStyle.TextGenFlags); //Generation RC 71 Generation flags (not bit-pair coded)
    vBits.WriteBD(AStyle.LastHeightUsed); //Last height BD 42
    vBits.WriteTV(AStyle.PrimaryFont, FCodePage); //Font name TV 3
    vBits.WriteTV(AStyle.BigFont, FCodePage); //Bigfont name TV 4

    AddCommonHandleRefs(vHandles, AOwner);
    AddHandleToArray(XrefHandle[AStyle], vHandles);
    AddHandlesUpdateAndFinish(vBits, vHandles, AStyle.Handle, vCode);
  finally
    vBits.Free;
  end;
end;

function TsgCADtoDWG.ProcAppID(const AOwnerHandle: UInt64; const AppInfo: string;
  E: TsgDXFEntity; var AHandles: TsgDWGHandleArray): UInt64;
var
  J: Integer;
  vNewHandle: Boolean;
  vBits: TsgDWGBits;
  vHandles: TsgDWGHandleArray;
  vXDict: UInt64;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vNewHandle := False;
    if Assigned(E) and (E.Handle <> cnstBadHandle) then
      Result := E.Handle
    else
    begin
      if not TsgHandleNameLinks(AppIDs).Find(AppInfo, J) then
        J := TsgHandleNameLinks(AppIDs).AddItem(DoHandle, AppInfo);
      Result := TsgHandleNameLinks(AppIDs).List^[J].HashCode;
      vNewHandle := True;
    end;
    vXDict := GetEntityHandle(TsgDXFConverterAccess(Converter).Dictionary[Result]);
    AddCommonEntityData(Result, cntDWGObjCodeAPPID, vBits, nil, E, 3, nil, nil, vXDict);
    AddCommonTableItemFlags(vBits, E, AppInfo, 0, nil);
    vBits.WriteRC(0);
    AddCommonHandleRefs(vHandles, AOwnerHandle, nil, vXDict);
    AddHandleToArray(XrefHandle[nil], vHandles);
    AddHandlesUpdateAndFinish(vBits, vHandles, Result, cntDWGObjCodeAPPID);
    AddHandleToArray(CreateDWGHandle(Result, cntDWGObjHandleSoftOwner), AHandles);
    if vNewHandle then
      AddCreatedElem('', nil, FCreatedElements, cntDWGObjCodeAPPID, Result);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcAppIDControl(const AHandle: UInt64;
  const AHandles: TsgDWGHandleArray);
begin
  ProcControlObject(AHandle, cntDWGObjCodeAPPIDCONTROLOBJ, GetDefChildTableItemsCount(AHandles),
    AHandles);
end;

procedure TsgCADtoDWG.ProcStyleControl(const AHandle: UInt64; const StylesHandles: TsgDWGHandleArray);
begin
  ProcControlObject(AHandle, cntDWGObjCodeSTYLECONTROLOBJ, GetDefChildTableItemsCount(StylesHandles),
    StylesHandles);
end;

procedure TsgCADtoDWG.ProcVariableDictionary(const Handle,
  OwnerHandle: UInt64; const Index: Integer; const Name: string);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vHandles: TsgDWGHandleArray;
  vReactors: TsgInt64List;
begin
  vReactors := TsgInt64List.Create;
  vBits := CreateDWGBitsByVersion(Version);
  try
    vCode := GetDWGCodeByClass(cntClassDXFDICTIONARYVAR);
    vReactors.Add(OwnerHandle);
    AddCommonEntityData(Handle, vCode, vBits, vReactors);
    vBits.WriteRC(0);
    vBits.WriteTV(Name, FCodePage);
    AddCommonHandleRefs(vHandles, OwnerHandle, vReactors);
    AddHandlesUpdateAndFinish(vBits, vHandles, Handle, vCode);
  finally
    vReactors.Free;
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcVPort(const AOwnerHandle: UInt64; AVPort: TsgDXFVPort;
  var ProcVPortHandles: TsgDWGHandleArray);
var
  vBits: TsgDWGBits;
  V: TsgDXFViewPort;
  vHandles: TsgDWGHandleArray;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    V := CreateDefaultViewPort(cnstBadHandle);
    try
      AddCommonEntityData(AVPort.Handle, cntDWGObjCodeVPORT, vBits);
      AddCommonTableItemFlags(vBits, AVPort, AVPort.Name, 0, nil);
      vBits.WriteBD(V.PSpaceHeight); //View height BD 40 2
      vBits.WriteBD(V.PSpaceWidth);//Aspect ratio BD 41
      vBits.Write2RD(V.MSpaceCenter); //View Center 2RD 12 DCS
      vBits.Write3BD(V.ViewTarget); //View target 3BD 17
      vBits.Write3BD(V.ViewDirection);//View dir 3BD 16
      vBits.WriteBD(V.ViewTwistAngle / f180DividedByPi); //View twist BD 51
      vBits.WriteBD(cntViewPortPerspectiveLensLength); //Lens length BD 42
      vBits.WriteBD(V.FrontClipPlane); //Front clip BD 43
      vBits.WriteBD(V.BackClipPlane); //Back clip BD 44
      vBits.WriteBB(0); //View mode X 71 2 bits
      vBits.WriteBB(0); //View mode X 71 2 bits
      vBits.WriteRC(0); //Render Mode RC 281 0 2D Optimized (classic 2D)
      if Version >= acR2007 then
      begin
        vBits.WriteBit(1);//Use default lights B 292
        vBits.WriteRC(1);//Default lighting type RC 282
        vBits.WriteBD(0);//Brightness BD 141
        vBits.WriteBD(0);//Constrast BD 142
        vBits.WriteCMC(MakeColorCAD(acRGBColor, $333333));//Ambient Color CMC 63
      end;
      vBits.Write2RD(cnstF2DPointZero); //Lower left 2RD 10 In fractions of screen width and height
      vBits.Write2RD(MakeF2DPoint(1, 1)); //Upper right 2RD 11 In fractions of screen width and height.
      vBits.WriteBit(0); //UCSFOLLOW B 71 UCSFOLLOW. Bit 3 (8) of the 71-group.
      vBits.WriteBS(cntViewPortCircleZoomPercent); //Circle zoom BS 72 Circle zoom percent.
      vBits.WriteBit(1); //Fast zoom B 73
      vBits.WriteBB(2); //UCSICON X 74 2 bits: 10
      vBits.WriteBit(0); //Grid on/off B 76
      vBits.Write2RD(MakeF2DPoint(1, 1)); //Grd spacing 2RD 15
      vBits.WriteBit(0); //Snap on/off B 75
      vBits.WriteBit(0); //Snap style B 77
      vBits.WriteBS(0); //Snap isopair BS 78
      vBits.WriteBD(0); //Snap rot BD 50
      vBits.Write2RD(cnstF2DPointZero); //Snap base 2RD 13
      vBits.Write2RD(MakeF2DPoint(1,1)); //Snp spacing 2RD 14
      vBits.WriteBit(0); //Unknown B
      vBits.WriteBit(AVPort.UCSVP); //UCS per Viewport B 71
      vBits.Write3BD(AVPort.UCSOrigin); //UCS Origin 3BD 110
      vBits.Write3BD(AVPort.UCSXDir); //UCS X Axis 3BD 111
      vBits.Write3BD(AVPort.UCSYDir); //UCS Y Axis 3BD 112
      vBits.WriteBD(0); //UCS Elevation BD 146
      vBits.WriteBS(0); //UCS Orthographic type BS 79
      if Version >= acR2007 then
      begin
        vBits.WriteBS(3); //Grid flags BS 60
        vBits.WriteBS(5); //Grid major BS 61
      end;
    finally
      V.Free;
    end;

    AddCommonHandleRefs(vHandles, AOwnerHandle);

    AddHandleToArray(XrefHandle[AVPort], vHandles);

    if Version >= acR2007 then
    begin
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleSoftPointer),
        vHandles);//Background handle H 332 soft pointer
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer),
        vHandles);//Visual Style handle H 348 hard pointer
      AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardOwner),
        vHandles);//Sun handle H 361 hard owner
    end;
    AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer),
      vHandles);
    AddHandleToArray(CreateDWGHandle(cnstBadHandle, cntDWGObjHandleHardPointer),
      vHandles);

    AddHandlesUpdateAndFinish(vBits, vHandles, AVPort.Handle, cntDWGObjCodeVPORT);
    AddHandleToArray(CreateDWGHandle(AVPort.Handle, cntDWGObjHandleSoftOwner),
      ProcVPortHandles);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcVPortControlObject(const AHandle: UInt64;
  const ProcVPortHandles: TsgDWGHandleArray);
begin
  ProcControlObject(AHandle, cntDWGObjCodeVPORTCONTROLOBJ, GetDefChildTableItemsCount(ProcVPortHandles),
    ProcVPortHandles);
end;

procedure TsgCADtoDWG.ProcViewPortEntHeader(const AIndex: Integer);
var
  vBits: TsgDWGBits;
  vElem: TsgDWGCreatedElement;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vElem := FCreatedElements[AIndex];
    AddCommonEntityData(vElem.Handle, cntDWGObjCodeVPENTHDR, vBits);
    AddCommonTableItemFlags(vBits, nil, vElem.Name, 0, nil);
    vBits.WriteBit(vElem.Location, 1); //1 flag B The 1 bit of the 70 group
    AddHandlesUpdateAndFinish(vBits, vElem.Handles, vElem.Handle, cntDWGObjCodeVPENTHDR);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.ProcWipeoutVar(const Handle, OwnerHandle: UInt64);
var
  vBits: TsgDWGBits;
  vCode: Word;
  vHandles: TsgDWGHandleArray;
begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    vCode := GetDWGCodeByClass(cntClassDXFWIPEOUTVARIABLES);
    AddCommonEntityData(Handle, vCode, vBits);
    vBits.WriteBL(Ord(not Converter.HideWipeOuts));
    AddCommonHandleRefs(vHandles, OwnerHandle);
    AddHandlesUpdateAndFinish(vBits, vHandles, Handle, vCode);
  finally
    vBits.Free;
  end;
end;

procedure TsgCADtoDWG.SaveObjectToStream(const AHandle: UInt64; const ABits: TsgDWGBits;
  DWGCode: Word);
begin
  FObjectsMap.AddElement(AHandle, Sections[GetObjectsSectionIndex].Position,
    DWGCode, '', cnstBadHandle, nil, ABits.Size);
  ABits.SaveToStream(Sections[GetObjectsSectionIndex]);
end;

procedure TsgCADtoDWG.SaveSectionToStream(const ASectNum: Integer;
  const ABits: TsgDWGBits; const ABeginSentinel, AEndSentinel: array of Byte;
  ANeedToClear: Boolean = True);
var
  vMemoryStream: TMemoryStream;

  procedure WriteSentinelToStream(const ASentinel: array of Byte);
  var
    vSentinelSize: Integer;
    P: Pointer;
  begin
    vSentinelSize := SizeOf(ASentinel);
    if vSentinelSize > 0 then
    begin
      P := @ASentinel[Low(ASentinel)];
      vMemoryStream.Write(P^, vSentinelSize);
    end;
  end;

begin
  DoError((ASectNum < 0) or (ASectNum >= FSections.Count), sInvalidSectNumber);
  vMemoryStream := Sections[ASectNum];
  if ANeedToClear then
    vMemoryStream.Clear;
  WriteSentinelToStream(ABeginSentinel);
  ABits.SaveToStream(vMemoryStream);
  WriteSentinelToStream(AEndSentinel);
end;

procedure TsgCADtoDWG.SetVersion(const Value: TsgDWGVersion);
begin
  if Version = Value then Exit;
  case Value of
    acR2000..acR2018:
      begin
        FreeSections;
        inherited SetVersion(Value);
        CreateSections;
        case Version of
          acR2000:
            FMaintenanceVersion := 6;
          acR2004:
            begin
              FMaintenanceVersion := 6;
              FOrigFileSavedVer := 29;
              FOrigFileSavedReleaseVer := 8;
            end;
          acR2010:
            begin
              FMaintenanceVersion := 6;
              FOrigFileSavedVer := 29;
              FOrigFileSavedReleaseVer := 6;
            end;
          acR2013:
            begin
//Valid values for acad maintenance version are 6 and 8. The CAD VCL currently writes value 8.
              FMaintenanceVersion := 8;
              FOrigFileSavedVer := 31;
              FOrigFileSavedReleaseVer := 8;
            end;
          acR2018:
            begin
              FMaintenanceVersion := 0;
              FOrigFileSavedVer := 33;
              FOrigFileSavedReleaseVer := 0;
            end;
        end;
      end;
  else
    DoError(True, sUnsupportedDWGVer + ' ' + sgDWGVerToString(Value));
  end;
end;

procedure TsgCADtoDWG.WriteByteToStream(const Stream: TStream; const Byte: Byte;
  const Count: Integer);
var
  P: PByte;
begin
  GetMem(P, Count);
  try
    FillChar(P^, Count, Byte);
    Stream.Write(P^, Count);
  finally
    FreeMem(P);
  end;
end;

function TsgCADtoDWG.Export3DFace(F: TsgDXF3dFace; ABits: TsgDWGBits): Boolean;
var
  vHasFlags, vZNilBit: Boolean;
  vFlags: Integer;
begin
  vFlags := TsgDXF3dFaceAccess(F).Flags and $0F;
  vHasFlags := vFlags <> 0;
  vZNilBit := IsZero(F.Point.Z);
  ABits.WriteBit(not vHasFlags);
  ABits.WriteBit(vZNilBit);
  ABits.Write2RD(F.Point);
  if not vZNilBit then
    ABits.WriteRD(F.Point.Z);
  ABits.Write3DD(F.Point1, F.Point);
  ABits.Write3DD(F.Point2, F.Point1);
  ABits.Write3DD(F.Point3, F.Point2);
  if vHasFlags then
    ABits.WriteBS(vFlags);
  Result := True;
end;

function TsgCADtoDWG.ExportACISEntity(AEnt: TsgBrepModAcis; ABits: TsgDWGBits): Boolean;
const
  cnstBlockSize = 4096;
var
  vMemoryStream: TMemoryStream;
  vSaveVer: Word;
  I: Integer;
  vSize, vWrittenSize: Integer;
  PStart, PData: PByte;
begin
  Result := False;
  vMemoryStream := GetACISStream(AEnt, vSaveVer);
  try
    if not (Assigned(vMemoryStream) and (vSaveVer in [1, 2])) then
    begin
      ABits.WriteBit(True);
      Exit;
    end;
    vMemoryStream.Position := 0;
    if (Version >= acR2013) and Assigned(AEnt) then
    begin
      FSegmentedFile.AddData(vMemoryStream, AEnt.Handle);

{       vBits.ReadBit; //false
      vBits.ReadBit; //false
      vBits.Read3BD; //26.93, 11.372, 1.505
      vBits.ReadBL; //4
      vBits.ReadBit; //false
      vBits.ReadBL; //0
      vBits.ReadBit; //true
      vBits.ReadBL; //0
      vBits.ReadBit; //false

      //GUID
      vBits.ReadBL; //0
      vBits.ReadBS; //0
      vBits.ReadBS; //0
      vBits.ReadBytes(@vByte[0], 8);
      vBits.ReadBL; //0}

      ABits.WriteBit(1); //wireframe
      ABits.WriteBit(1); //has point
      ABits.Write3BD(AEnt.Box.TopLeft);//point
      ABits.WriteBL(4);// num isolines
      ABits.WriteBit(0);// isoline data present
      ABits.WriteBL(0);// Number of ISO lines that follow.
      ABits.WriteBit(1);//??
      ABits.WriteBL(0);//Num. silhouettes
      ABits.WriteBit(0);//ACIS Empty bit

      ABits.WriteBL(0);//write GIUD
      ABits.WriteBS(0);//write GIUD
      ABits.WriteBS(0);//write GIUD
      ABits.WriteBytes(0, 8);//write GIUD

      ABits.WriteBL(0);

      Result := True;
      Exit;
    end;

    ABits.WriteBit(False); //ACIS Empty bit B X If 1, then no data follows
    ABits.WriteBit(vSaveVer = 1); //Unknown bit B X
    ABits.WriteBS(vSaveVer); //Version BS Can be 1 or 2

    vSize := vMemoryStream.Size;
    PStart := vMemoryStream.Memory;
    case vSaveVer of
    1:
      begin
        PData := PStart;
        for I := 0 to vSize - 1 do
        begin
          PData^ := ACIS_Cipher(PData^);
          Inc(PData);
        end;
        PData := PStart;
        while True do
        begin
          vWrittenSize := Min(vSize, cnstBlockSize);
          ABits.WriteBL(vWrittenSize);
          ABits.WriteBytes(PData, vWrittenSize);
          Dec(vSize, cnstBlockSize);
          Inc(PData, vWrittenSize);
          if vSize <= 0 then
            Break;
        end;
        ABits.WriteBL(0);
      end;
    2:
      begin
        PData := PStart;
        for I := 0 to vSize - 1 do
        begin
          ABits.WriteRC(PData^);
          Inc(PData);
        end;
        {vTailSize := Ceil(vSize / 1024) * 1024 - vSize;
        for I := 0 to vTailSize - 1 do
          ABits.WriteRC(0);
        }
      end;
    else
      ABits.WriteBit(True); //ACIS Empty bit B X If 1, then no data follows
      Exit;
    end;
    //if vSaveVer <> 1 then
    //  ABits.WriteBit(False); //Wireframe data present
    ABits.WriteBit(False);  //only one SAT file can be saved
    if Version >= acR2007 then
      ABits.WriteBL(0);    //num materials

    if Version > acR2010  then
    begin
      ABits.WriteBit(0);

      ABits.WriteBL(0);//write GIUD
      ABits.WriteBS(0);//write GIUD
      ABits.WriteBS(0);//write GIUD
      ABits.WriteBytes(0, 8);//write GIUD

      ABits.WriteBL(0);
    end;

  finally
    FreeAndNil(vMemoryStream);
  end;
  Result := True;
end;

{$IFDEF SG_BTI}
function TsgCADtoDWG.ExportPolyPolyline2D(Poly: TsgCADPolyPolyline2D;
  const AExportData: TsgDirectExportData): Boolean;
begin
  Result := ExportGroup(Poly, cntDWGObjCodePOLYLINE3D, AExportData);
end;
{$ENDIF}

function TsgCADtoDWG.ExportCADFill(CADFill: TsgCADFill;
  const Data: TsgDirectExportData): Boolean;
begin
  Result := ExportGroup(CADFill, cntDWGObjCodePOLYLINE3D, Data);
end;

function TsgCADtoDWG.ExportCircle(C: TsgDXFCircle; ABits: TsgDWGBits): Boolean;
begin
  Result := True;
  ABits.Write3BD(C.Point);
  ABits.WriteBD(C.Radius);
  ABits.WriteBT(C.ZThick);
  ABits.WriteBE(C.Extrusion);
  case C.EntType of
    ceArc:
      begin
        ABits.WriteBD(TsgDXFArc(C).StartAngle / f180DividedByPi);
        ABits.WriteBD(TsgDXFArc(C).EndAngle / f180DividedByPi);
      end;
  end;
end;

procedure TsgCADtoDWG.ExportCreatedTableRecords;
var
  vBits: TsgDWGBits;
  vItem: TsgDWGCreatedElement;
  I: Integer;
  vHandles: TsgDWGHandleArray;

  function GetChildrenCount(ACode: Word): Cardinal;
  var
    I, vIndex: Integer;
    vCode: Word;
  begin
    Result := 0;
    vCode := ACode + 1; //tables children DWG code are equal CONTROL_OBJECT_CODE + 1, see DWG documentation
    vIndex := FCreatedElements.IndexOf(vCode);
    AddCommonHandleRefs(vHandles, cnstBadHandle);
    if vIndex >= 0 then
    begin
      for I := vIndex to FCreatedElements.Count - 1 do
      begin
        if FCreatedElements.Element[I].DWGObjCode <> vCode then Break;
        Inc(Result);
        AddHandleToArray(CreateDWGHandle(FCreatedElements[I].Handle, cntDWGObjHandleSoftOwner), vHandles);
      end;
    end;
  end;

begin
  vBits := CreateDWGBitsByVersion(Version);
  try
    FCreatedElements.SortBy := seDWGObjCode;
    for I := 0 to FCreatedElements.Count - 1 do
    begin
      vBits.Clear;
      Finalize(vHandles);
      vItem := FCreatedElements.Element[I];
      if (vItem.Handle <> cnstBadHandle) and Assigned(vItem.Obj) and (vItem.Obj.ClassType <> TsgDXFXRecord) and
         vItem.Obj.InheritsFrom(TsgDXFOwneredItem) then
      begin
        if AddCommonEntityData(vItem.Handle, vItem.DWGObjCode, vBits) = 0 then Continue;
        vBits.WriteBL(GetChildrenCount(vItem.DWGObjCode));
        AddHandlesUpdateAndFinish(vBits, vHandles, vItem.Handle, vItem.DWGObjCode);
      end;
    end;
  finally
    vBits.Free;
  end;
  ExportViewPortEntityHeaders;
end;

function TsgCADtoDWG.ExportDimension(D: TsgDXFDimension; ABits: TsgDWGBits;
  DWGCode: Word): Boolean;
var
  vFlags: Byte;

  procedure SetBitFlag(Mask, FlagMask: Byte; SetAnyTime: Boolean = False);
  begin
    if (D.Flags and Mask = 1) or SetAnyTime then
      vFlags := vFlags or FlagMask;
  end;

begin
  if Version > acR2007 then
    ABits.WriteRC(0);
  ABits.Write3BD(D.Extrusion);//Extrusion 3BD 210
  ABits.Write2RD(D.MiddlePoint);//Text midpt 2RD 11
  ABits.WriteBD(D.Elevation); //Elevation BD 11
  vFlags := 8;//not sure
  SetBitFlag(128, 1);
  SetBitFlag(32, 2, True);//Bit 1 : Same as bit 5 (32) of the 70
  ABits.WriteRC(vFlags);//Flags 1 RC 70
  ABits.WriteTV(D.TextOverride, FCodePage);//User text TV 1
  ABits.WriteBD(0);//Text rot BD 53
  ABits.WriteBD(0);//Horiz dir BD 51
  ABits.Write3BD(D.Scale);//Ins X, Y, Z-scale
  ABits.WriteBD(D.Angle / f180DividedByPi);//Ins rotation
  if Version >= acR2000 then
  begin
    ABits.WriteBS(5);//Attachment Point BS
    ABits.WriteBS(1);//Linespacing Style BS
    ABits.WriteBD(1);//Linespacing Factor BD 41
    ABits.WriteBD(-1);//Actual Measurement BD 42
  end;
  if Version >= acR2007 then //b73
  begin
    ABits.WriteBit(0);//Unknown B 73
    ABits.WriteBit(0);//Flip arrow1 B 74
    ABits.WriteBit(0);//Flip arrow2 B 75
  end;
  ABits.Write2RD(D.Point);
  case DWGCode of
    cntDWGObjCodeARC_DIMENSION:
      begin
        ABits.Write3BD(D.DefPoint);//10-pt 3BD 10
        ABits.Write3BD(D.LinDefPoint1);//13-pt 3BD 13
        ABits.Write3BD(D.LinDefPoint2);//14-pt 3BD 14
        ABits.Write3BD(D.RadDefPoint);//15-pt 3BD 15
        ABits.WriteBit(0);//Is partial? B 70
        ABits.WriteBD(0);//Start angle (radians) BD 40
        ABits.WriteBD(0);//End angle (radians) BD 41
        ABits.WriteBit(0);//Has leader? B 71
        ABits.Write3BD(cnstFPointZero);//Leader point 1 3BD 16
        ABits.Write3BD(cnstFPointZero);//Leader point 2 3BD 17
      end;
    cntDWGObjCodeDIMENSIONORDINATE, cntDWGObjCodeDIMENSIONANG3Pt:
      begin
        ABits.Write3BD(D.DefPoint);//10-pt 3BD 10
        ABits.Write3BD(D.LinDefPoint1);//13-pt 3BD 13
        ABits.Write3BD(D.LinDefPoint2);//14-pt 3BD 14
        case DWGCode of
          cntDWGObjCodeDIMENSIONORDINATE:
            begin
              vFlags := 0;
              SetBitFlag(64, 1);
              ABits.WriteRC(vFlags);
            end;
          cntDWGObjCodeDIMENSIONANG3Pt:
            ABits.Write3BD(D.RadDefPoint);//15-pt 3BD 15
        end;
      end;
    cntDWGObjCodeDIMENSIONALIGNED, cntDWGObjCodeDIMENSIONLINEAR:
      begin
        ABits.Write3BD(D.LinDefPoint1);//13-pt 3BD 13
        ABits.Write3BD(D.LinDefPoint2);//14-pt 3BD 14
        ABits.Write3BD(D.DefPoint);//10-pt 3BD 10
        ABits.WriteBD(0);//Ext ln rot BD 52
        if DWGCode = cntDWGObjCodeDIMENSIONLINEAR then
          ABits.WriteBD(D.DimRot * fPiDividedBy180);//Dim rot BD 50
      end;
    cntDWGObjCodeDIMENSIONANG2Ln:
      begin
        ABits.Write2RD(D.ArcDefPoint);//16-pt 2RD 16
        ABits.Write3BD(D.LinDefPoint1);//13-pt 3BD 13
        ABits.Write3BD(D.LinDefPoint2);//14-pt 3BD 14
        ABits.Write3BD(D.RadDefPoint);//15-pt 3BD 15
        ABits.Write3BD(D.DefPoint);//10-pt 3BD 10
      end;
    cntDWGObjCodeDIMENSIONRADIUS, cntDWGObjCodeDIMENSIONDIAMETER:
      begin
        ABits.Write3BD(D.DefPoint);//10-pt 3BD 10
        ABits.Write3BD(D.RadDefPoint);//15-pt 3BD 15
        ABits.WriteBD(0);//Leader len D 40
      end;
  end;
  Result := True;
end;

function TsgCADtoDWG.ExportEllipse(E: TsgDXFEllipse; ABits: TsgDWGBits): Boolean;
var
  vStartAngle, vEndAngle: Double;

  function CheckAngle(const Value: Double): Double;
  begin
    Result := Value;
    if Value > 360 then
      Result := Value - 360;
  end;

begin
  ABits.Write3BDPreExtruded(E.Point, E.Extrusion);//Center 3BD 10 (WCS)
  ABits.Write3BDPreExtruded(E.RadPt, E.Extrusion);//SM axis vec 3BD 11 Semi-major axis vector (WCS)
  ABits.Write3BD(E.Extrusion);//Extrusion 3BD 210
  ABits.WriteBD(E.Ratio);//Axis ratio BD 40 Minor/major axis ratio
  vStartAngle := E.StartAngle;
  vEndAngle := E.EndAngle;
  if IsZero(vStartAngle - vEndAngle) then
  begin
    vStartAngle := 0;
    vEndAngle := 360;
  end;
  ABits.WriteBD(Radian(vStartAngle));//Beg angle BD 41 Starting angle (eccentric anomaly, radians)
  ABits.WriteBD(Radian(vEndAngle));//End angle BD 42 Ending angle (eccentric anomaly, radians)
  Result := True;
end;

function TsgCADtoDWG.ExportFlatPoly(FlatPoly: TsgFlatPoly; const Data: TsgDirectExportData): Boolean;
begin
  Result := ExportGroup(FlatPoly, cntDWGObjCodePOLYLINE3D, Data);
end;

function TsgCADtoDWG.ExportHatch(H: TsgCADPolyPolygon; ABits: TsgDWGBits;
  DWGCode: Word): Boolean;
const
  cntMPoligonsTypes = [cePolyPolygon];

var
  vDoubleVal: Double;
  vFPoint: TFPoint;
  I: Integer;
  vPixSize, vIsCADCurvePolygon, vMPolygonFillIsUsed: Boolean;

  function GetGradientParam(DXFCode: Integer; const Gradient: TsgCADGradientPolygon = nil): Variant;
  begin
    Result := Integer(0);
    case DXFCode of
      450:
        // 0 = Solid hatch
        // 1 = Gradient
        if (Gradient <> nil) and vMPolygonFillIsUsed then
          Result := 1
        else
          Result := 0;
      451:
        Result := 0; // Reserved
      452:
        // 0 = Two-color gradient
        // 1 = Single-color gradient
        Result := 0;
      453:
        if Gradient <> nil then
          Result := 2
        else
          Result := 0;
      460:
        if Gradient <> nil then
          Result := Gradient.GradientAngle * fPiDividedBy180
        else
          Result := 0.0;
      461:
        if (Gradient <> nil) and Gradient.GradientUseCenter then
          Result := 0.0
        else
          Result := 1.0;
      462:
        if Gradient <> nil then
          Result := 1.0
        else
          Result := 0.0;
      463:
        Result := 1.0;
      470:
        if Gradient <> nil then
          Result := Gradient.GradientName
        else
          Result := '';
    end;
  end;

  procedure WriteGradietParams;
  var
    vGradient: TsgCADGradientPolygon;
    I, vGradientColors: Integer;
  begin
    vGradient := nil;
    if (H.EntType = ceMPolygon) or IsCADGradient(H) then
    begin
      vGradient := TsgCADGradientPolygon(H);
      if vGradient.SolidFill and ((vGradient.GradientName = '') or ((vGradient.GradientName = 'SOLID'))) then
        vGradient := nil;
    end;

    ABits.WriteBL(GetGradientParam(450, vGradient)); //450 Non-zero indicates a gradient fill is used
    ABits.WriteBL(GetGradientParam(451)); //Reserved BL 451

    ABits.WriteBD(GetGradientParam(460, vGradient)); //Gradient Angle BD 460
    ABits.WriteBD(GetGradientParam(461, vGradient)); //Gradient Shift BD 461

    ABits.WriteBL(GetGradientParam(452, vGradient)); // Single Color Grad. BL 452
    ABits.WriteBD(GetGradientParam(462, vGradient)); //Gradient Tint BD 462

    vGradientColors := GetGradientParam(453, vGradient);
    ABits.WriteBL(vGradientColors); //# of Gradient Colors BL 453
    for I := 0 to vGradientColors - 1 do
    begin
      ABits.WriteBD(GetGradientParam(463, vGradient)); //Unknown double BD 463
      ABits.WriteColorFlags(0); // BS
      case I of
        0, 1: if vGradient <> nil then
          ABits.WriteCMCDirectly(vGradient.GradientColorCAD[I]); //BL 63,421
      else
        DoError(True, sGradient2ColorsIsSupported);
      end;
      ABits.WriteRC(0); // color byte
    end;
    ABits.WriteTV(GetGradientParam(470, vGradient), FCodePage); //Gradient Name TV 470
  end;

  procedure AddPolylineBoundary(ABoundaryList: TF2DPointList);
  var
    I: Integer;
  begin
    ABits.WriteBit(True);//closed
    ABits.WriteBit(False);//bulges
    ABits.WriteBL(ABoundaryList.Count);
    for I := 0 to ABoundaryList.Count - 1 do
      ABits.Write2RD(ABoundaryList[I]);
  end;

  procedure AddPatternData;
  var
    vHatch: TsgCADHatch absolute H;
    vPatternData: TsgHatchPatternData;
    vDashLengths: TsgDoubleList;
    I, J: Integer;
  begin
    ABits.WriteBD(vHatch.PatternAngle / f180DividedByPi);
    ABits.WriteBD(vHatch.PatternScale);
    ABits.WriteBit(vHatch.DoubleHatch);
    ABits.WriteBS(vHatch.HatchPatternData.Count);
    for I := 0 to vHatch.HatchPatternData.Count - 1 do
    begin
      vPatternData := PsgHatchPatternData(vHatch.HatchPatternData[I])^;
      ABits.WriteBD(vPatternData.LineAngle / f180DividedByPi);//angle BD 53 line angle
      ABits.Write2BD(vPatternData.BaseP);//pt0 2BD 43/44 pattern through this point (X,Y)
      ABits.Write2BD(vPatternData.Offset);//offset 2BD 45/56 pattern line offset
      // Dash length (multiple entries)
      vDashLengths := vPatternData.Lines;
      if vDashLengths <> nil then
      begin
        ABits.WriteBS(vDashLengths.Count);//numdashes BS 79 number of dash length items
        for J := 0 to vDashLengths.Count - 1 do
          ABits.WriteBD(vDashLengths[J]);
      end
      else
        ABits.WriteBS(0);
    end;
  end;

  procedure Write2DLine(Boundary: Tsg2DCurve);
  begin
    ABits.Write2RD(Boundary.StartPoint);
    ABits.Write2RD(Boundary.EndPoint);
  end;

  procedure Write2DArc(const ACircle: Boolean; Boundary: Tsg2DCurve);
  begin
    if ACircle then
      ABits.Write2RD(Tsg2DArc(Boundary).CenterPoint)
    else
    begin
      ABits.Write2RD(Tsg2DEllipse(Boundary).CenterPoint);
      ABits.Write2RD(Tsg2DEllipse(Boundary).MajorPoint);
    end;
    ABits.WriteBD(Tsg2DArc(Boundary).Radius);
    ABits.WriteBD(Tsg2DArc(Boundary).StartParam / f180DividedByPi);
    ABits.WriteBD(Tsg2DArc(Boundary).EndParam / f180DividedByPi);
    ABits.WriteBit(Tsg2DArc(Boundary).CounterClockWise);
  end;

  procedure Write2DSpline(Boundary: Tsg2DSplineEx);
  var
    I, FitPtsNum: Integer;
  begin
    ABits.WriteBL(Boundary.Degree);
    ABits.WriteBit(0);//rational
    ABits.WriteBit(0);//periodic
    ABits.WriteBL(Boundary.KnotsCount);
    ABits.WriteBL(Boundary.ControlsCount);
    for I := 0 to Boundary.KnotsCount - 1 do
      ABits.WriteBD(Boundary.Knots[I]);
    for I := 0 to Boundary.ControlsCount - 1 do
      ABits.Write2RD(Boundary.Controls[I]);
    if Version > acR2007 then
    begin
      FitPtsNum := 0;
      ABits.WriteBL(FitPtsNum); //new feature builds curve by fit points
      if FitPtsNum > 0 then
      begin
        for I := 0 to FitPtsNum - 1 do
          ABits.Write2RD(cnstF2DPointZero);
        ABits.Write2RD(cnstF2DPointZero);
        ABits.Write2RD(cnstF2DPointZero);
      end;
    end;
  end;

  procedure Write2DPolyline(Boundary: Tsg2DPolylineEx;
    APointList: TF2DPointList = nil);
  var
    I, vCount: Integer;
    vBulges, vClosed: Boolean;
  begin

    if APointList = nil then
    begin
      vBulges := Boundary.BulgesCount > 0;
      vCount := Boundary.VertexesCount;
      vClosed := Boundary.Closed;
    end
    else
    begin
      vBulges := False;
      vCount := APointList.Count;
      vClosed := True;
    end;
    ABits.WriteBit(vBulges);//bulges
    ABits.WriteBit(vClosed);//closed
    ABits.WriteBL(vCount);
    for I := 0 to vCount - 1 do
    begin
      if APointList = nil then
      begin
        ABits.Write2RD(Boundary.Vertexes[I]);
        if vBulges then
          ABits.WriteBD(Boundary.Bulges[I]);
      end
      else
        ABits.Write2RD(APointList[I]);
    end;
  end;

  procedure AddBoundaryPathData(ABoundaryList: Tsg2DBoundaryList;
    APointList: TF2DPointList = nil);
  var
    vBoundary: Tsg2DCurve;
    I: Integer;
    vBoundaryType: Byte;
  begin
    if APointList = nil then
    begin
      vBoundaryType := ABoundaryList.BoundaryType;
      vPixSize := vPixSize or (ABoundaryList.BoundaryType and 4 <> 0);
    end
    else
      vBoundaryType := 2;
    ABits.WriteBL(vBoundaryType);
    if vBoundaryType and 2 = 0 then
    begin
      ABits.WriteBL(ABoundaryList.Count);
      for I := 0 to ABoundaryList.Count - 1 do
      begin
        vBoundary := Tsg2DCurve(ABoundaryList[I]);
        ABits.WriteRC(vBoundary.EdgeType);
        case vBoundary.EdgeType of
          1: Write2DLine(vBoundary);
          2: Write2DArc(True, vBoundary);
          3: Write2DArc(False, vBoundary);
          4: Write2DSpline(Tsg2DSplineEx(vBoundary));
        end;
      end;
    end
    else
      if APointList = nil then
        Write2DPolyline(Tsg2DPolylineEx(ABoundaryList[0]), nil)
      else
        Write2DPolyline(nil, APointList);
    ABits.WriteBL(0);
  end;

  function IsMPoligonHatch: Boolean;
  begin
    Result := (DWGCode = cntDWGObjCodeMPOLYGON) or (H.EntType = cePolyPolygon);
  end;

begin
  vPixSize := False;
  vIsCADCurvePolygon := H is TsgCADCurvePolygon;
  vMPolygonFillIsUsed := True;
  if DWGCode = cntDWGObjCodeMPOLYGON then
  begin
    if not H.SolidFill and (TsgCADHatch(H).HatchPatternData.Count > 0) then
      vMPolygonFillIsUsed := False;
  end;

  if DWGCode = cntDWGObjCodeMPOLYGON then
    ABits.WriteBL(1);
  if Version >= acR2004 then
    WriteGradietParams;
//  if Version >= acR2007 then
//    ABits.WriteTV(H.HatchName, FCodePage);
  vDoubleVal := 0;
  vFPoint := cnstZOrtAxis;
  if H is TsgCADCurvePolygon then
  begin
    vDoubleVal := TsgCADCurvePolygon(H).Elevation.Z;
    vFPoint := TsgCADCurvePolygon(H).Extrusion;
  end;
  ABits.WriteBD(vDoubleVal);//Z coord BD 30 X, Y always 0.0
  ABits.Write3BD(vFPoint);//Extrusion 3BD 210
  //if Version < acR2007 then
    ABits.WriteTV(H.HatchName, FCodePage);

  if DWGCode = cntDWGObjCodeMPOLYGON then
    ABits.WriteBit(H.SolidFill)
  else
  begin
    ABits.WriteBit(H.SolidFill);
    ABits.WriteBit(0);
  end;
  //boundary data
  if DWGCode <> cntDWGObjCodeMPOLYGON then
  begin
    if vIsCADCurvePolygon then
    begin
      ABits.WriteBL(TsgCADCurvePolygon(H).BoundaryDataCount);
      for I := 0 to TsgCADCurvePolygon(H).BoundaryDataCount - 1 do
        AddBoundaryPathData(TsgCADCurvePolygon(H).BoundaryData[I]);
    end
    else
    begin
      ABits.WriteBL(H.Boundaries.Count);
      for I := 0 to H.Boundaries.Count - 1 do
        AddBoundaryPathData(nil, TF2DPointList(H.Boundaries[I]));
    end;
    ABits.WriteBS(0);//Hatch style
  end
  else
  begin
    ABits.WriteBL(H.Boundaries.Count);
    for I := 0 to H.Boundaries.Count - 1 do
      AddPolylineBoundary(TF2DPointList(H.Boundaries[I]));
  end;
  ABits.WriteBS(1);// Hatch pattern type (1 = predefined)
  if not H.SolidFill then
    AddPatternData;
  if vPixSize then
    ABits.WriteBD(0.000001);
  if DWGCode = cntDWGObjCodeMPOLYGON then
  begin
    ABits.WriteCMC(TsgCADMPolygon(H).FillColor);
    ABits.Write2RD(TsgCADMPolygon(H).Offset);
  end;
  ABits.WriteBL(0);
  Result := True;
end;

function TsgCADtoDWG.ExportInsert(Insert: TsgDXFInsert; ABits: TsgDWGBits;
  EEData: TsgCADExtendedData): Boolean;
var
  vPoint, vScale: TFPoint;
  vAngle: Double;
  vScaleFlag: Byte;
  I: Integer;
  vHasAttribs: Boolean;
  vData: TsgDirectExportData;
begin
  Result := True;

  vPoint := Insert.Point;
  vScale := Insert.Scale;
  vAngle := Insert.Angle;

  if Insert.EntClass.EG = gtSVG then
  begin
    Insert.ExtractParamsFromMatrix2D(vPoint, vScale, vAngle);
    if IsEqual(vScale.X, 0) or IsEqual(vScale.Y, 0) or IsEqual(vScale.Z, 0) then
      vScale := Insert.Scale;
  end;

  ABits.Write3BD(vPoint);// Ins pt 3BD 10

  if IsEqualFPoints(vScale, cnstFPointSingle) then
    vScaleFlag := 3
  else
    if IsEqual(vScale.X, vScale.Y) and IsEqual(vScale.X, vScale.Z) then
      vScaleFlag := 2
    else
      if IsEqual(vScale.X, 1) then
        vScaleFlag := 1
      else
        vScaleFlag := 0;
  ABits.WriteBB(vScaleFlag);
  case vScaleFlag of
    0:
      begin
        ABits.WriteRD(vScale.X);
        ABits.WriteDD(vScale.Y, vScale.X);
        ABits.WriteDD(vScale.Z, vScale.X);
      end;
    1:
      begin
        ABits.WriteDD(vScale.Y, 1);
        ABits.WriteDD(vScale.Z, 1);
      end;
    2: ABits.WriteRD(vScale.X);
  end;

  ABits.WriteBD(vAngle / f180DividedByPi);
  ABits.Write3BD(Insert.Extrusion);
  vHasAttribs := Insert.HasAttribs;
  ABits.WriteBit(vHasAttribs);

  if Version >= acR2004 then
    if vHasAttribs then
      ABits.WriteBL(Insert.Attribs.Count);

  if Insert is TsgCADMInsert then
  begin
    ABits.WriteBS(TsgCADMInsert(Insert).NumCols);//Numcols BS 70
    ABits.WriteBS(TsgCADMInsert(Insert).NumRows);//Numrows BS 71
    ABits.WriteBD(TsgCADMInsert(Insert).ColSpacing);//Col spacing BD 44
    ABits.WriteBD(TsgCADMInsert(Insert).RowSpacing);//Row spacing BD 45
  end;

  if vHasAttribs then
  begin
    vData := CreateExportData(Insert.Handle, 0, cnstBadHandle, cnstBadHandle, 0, Insert);
    DoExport(Insert, @vData);//for exporting SeqEnd
    for I := 0 to Insert.Attribs.Count - 1 do
    begin
      vData := CreateExportData(Insert.Handle, 0, GetPrevEntHandle(nil, I,
        Insert.Attribs), GetNextEntHandle(nil, I, Insert.Attribs), 0, Insert);
      DoExport(TsgDXFEntity(Insert.Attribs[I]), @vData);
    end;
  end;
end;

function TsgCADtoDWG.ExportLeader(L: TsgDXFLeader; ABits: TsgDWGBits): Boolean;
var
  I: Integer;
  vPoint: TFPoint;
begin
  ABits.WriteBit(0);//Unknown bit B
  ABits.WriteBS(3);// Annot type BS Created without any annotation
  ABits.WriteBS(Ord(L.IsSpline));//path type BS
  if L.ControlCount = 0 then
  begin
    ABits.WriteBL(1);//number of points
    ABits.Write3BD(cnstFPointZero);
  end
  else
  begin
    ABits.WriteBL(L.ControlCount);//number of points
    for I := 0 to L.ControlCount - 1 do
      ABits.Write3BD(L.Controls[I]);
  end;
  vPoint := cnstFPointZero;
  if L.ControlCount > 0 then
    vPoint := L.Controls[0]; //not sure, experimentally
  ABits.Write3BD(vPoint);//Endptproj 3BD
  ABits.Write3BD(L.Extrusion);//Extrusion 3BD
  ABits.Write3BD(cnstXOrtAxis);//x direction 3BD
  ABits.Write3BD(cnstFPointZero);//offsettoblockinspt 3BD
  ABits.Write3BD(cnstFPointZero);//Unknown 3BD
  if Version < acR2010 then
  begin
    ABits.WriteBD(0);//Box height BD 40
    ABits.WriteBD(0);//Box width BD 41
  end;
  ABits.WriteBit(0);//Hooklineonxdir B
  ABits.WriteBit(L.Arrowhead);//Arrowheadon B
  ABits.WriteBS(8);
  ABits.WriteBit(0);
  ABits.WriteBit(0);
  Result := True;
end;

function TsgCADtoDWG.ExportLine(L: TsgDXFLine; ABits: TsgDWGBits): Boolean;
var
  vNotNeedZ: Boolean;
begin
  vNotNeedZ := IsZero(L.Point.Z) and IsZero(L.Point1.Z);
  ABits.WriteBit(vNotNeedZ);
  ABits.WriteRD(L.Point.X); //Start Point x RD 10
  ABits.WriteDD(L.Point1.X, L.Point.X); //End Point x DD 11 Use 10 value for default
  ABits.WriteRD(L.Point.Y); //Start Point y RD 20
  ABits.WriteDD(L.Point1.Y, L.Point.Y); //End Point y DD 21 Use 20 value for default
  if not vNotNeedZ then
  begin
    ABits.WriteRD(L.Point.Z); //Start Point z RD 30 Present only if “Z’s are zero bit” is 0
    ABits.WriteDD(L.Point1.Z, L.Point.Z); //End Point z DD 31 Present only if “Z’s are zero bit” is 0, use 30 value for default.
  end;
  ABits.WriteBT(L.ZThick); //Thickness BT 39
  ABits.WriteBE(L.Extrusion); //Extrusion BE 210
  Result := True;
end;

function TsgCADtoDWG.ExportLWPolyline(LWPolyline: TsgDXFLWPolyline; ABits: TsgDWGBits): Boolean;
var
  vFlags: Word;
  vPoint: TFPoint;
  vBulgesCount, vWidthsCount: Cardinal;
  I: Integer;
  vFlagNoVertex: Boolean;

  procedure SetFlagsBit(const Mask: Word; const Value: Double);
  begin
    if not IsZero(Value) then
      vFlags := vFlags or Mask;
  end;

  procedure CheckBitAndWriteBD(Mask: Word; const Value: Double);
  begin
    if Mask and vFlags <> 0 then
      ABits.WriteBD(Value);
  end;

  function GetNumPoints: Cardinal;
  begin
    Result := LWPolyline.Count;
  end;

  procedure CountBulgesAndWidths;
  var
    I: Integer;
    vVertex: TsgDXFVertex;
  begin
    vBulgesCount := 0;
    vWidthsCount := 0;
    for I := 0 to LWPolyline.Count - 1 do
    begin
      vVertex := LWPolyline.Vertexes[I];
      if not IsZero(vVertex.Bulge) then
        vBulgesCount := GetNumPoints;
      if (not IsZero(vVertex.StartWidth)) or (not IsZero(vVertex.EndWidth)) then
        vWidthsCount := GetNumPoints;
      if (vBulgesCount <> 0) and (vWidthsCount <> 0) then
        Break;
    end;
  end;

begin
  Result := True;
  vFlags := 0;
  vFlagNoVertex := False;
  vPoint := cnstFPointZero;
  if LWPolyline.Count > 0 then
    vPoint := LWPolyline.Vertexes[0].Point
  else
    vFlagNoVertex := True;
  CountBulgesAndWidths;
  SetFlagsBit(4, LWPolyline.GlobalWidth);
  SetFlagsBit(8, LWPolyline.Elevation);
  SetFlagsBit(2, LWPolyline.ZThick);
  if LWPolyline.Closed then
    vFlags := vFlags or 512;
  if not IsEqualFPoints(LWPolyline.Extrusion, cnstExtrusion)  then
    vFlags := vFlags or 1;
  if vBulgesCount <> 0 then
    vFlags := vFlags or 16;
  if vWidthsCount <> 0 then
    vFlags := vFlags or 32;
  ABits.WriteBS(vFlags);
  CheckBitAndWriteBD(4, LWPolyline.GlobalWidth);
  CheckBitAndWriteBD(8, LWPolyline.Elevation);
  CheckBitAndWriteBD(2, LWPolyline.ZThick);
  if vFlags and 1 <> 0 then
    ABits.Write3BD(LWPolyline.Extrusion);
  if vFlagNoVertex then
    ABits.WriteBL(2)
  else
    ABits.WriteBL(GetNumPoints);
  if vBulgesCount <> 0 then
    ABits.WriteBL(vBulgesCount);
  if vWidthsCount <> 0 then
    ABits.WriteBL(vWidthsCount);
  ABits.Write2RD(vPoint);
  if vFlagNoVertex then
  begin
      ABits.WriteDD(0,0);
      ABits.WriteDD(0,0);
  end
  else
  begin
    for I := 1 to GetNumPoints - 1 do
    begin
      ABits.WriteDD(LWPolyline.Vertexes[I].Point.X, LWPolyline.Vertexes[I - 1].Point.X);
      ABits.WriteDD(LWPolyline.Vertexes[I].Point.Y, LWPolyline.Vertexes[I - 1].Point.Y);
    end;
  end;

  for I := 0 to vBulgesCount - 1 do
    ABits.WriteBD(LWPolyline.Vertexes[I].Bulge);
  for I := 0 to vWidthsCount - 1 do
    ABits.Write2BD(MakeF2DPoint(LWPolyline.Vertexes[I].StartWidth, LWPolyline.Vertexes[I].EndWidth));
end;

function TsgCADtoDWG.ExportMesh(M: TsgDXFMesh; ABits: TsgDWGBits): Boolean;
var
  I: Integer;
begin
  ABits.WriteBS(2);  // version
  ABits.WriteBit(0); //"Blend Crease"
  ABits.WriteBL(0); // Number of subdivision level
  ABits.WriteBL(M.Vertices.Count);
  for I := 0 to M.Vertices.Count - 1 do
    ABits.Write3BD(M.Vertices[I]);
  ABits.WriteBL(M.VertexIndices.Count);
  for I := 0 to M.VertexIndices.Count - 1 do
    ABits.WriteBL(M.VertexIndices[I]);
  ABits.WriteBL(0);// 94: edges count; [+ 90: count * (2BL)]
  ABits.WriteBL(0);// 95: Edge crease count of level 0; [+ 140: Edge create value]
  ABits.WriteBL(0);// 90 unknown count (Count of sub-entity which property has been overridden)
  //92: Count of property was overridden
  //90 Property type: 0 = Color; 1 = Material; 2 = Transparency; 3 = Material mapper
  Result := True;
end;

function TsgCADtoDWG.ExportMLine(M: TsgCADMLine; const ABits: TsgDWGBits;
  const AExportData: TsgDirectExportData): Boolean;
var
  vVertex: TsgMVertex;
  I, J, K, vLinesInStyle: Integer;
begin
  ABits.WriteBD(M.ScaleFactor);//Scale BD 40
  ABits.WriteRC(M.Justify);//Just EC top (0), bottom(2), or center(1)
  ABits.Write3BD(M.StartPoint);//Base point 3BD 10
  ABits.Write3BD(M.Extrusion);//Extrusion 3BD 210 etc.
  ABits.WriteBS(M.Flags);//Openclosed BS open (1), closed(3)
  if M.Count > 0 then
    vLinesInStyle := TsgMVertex(M[0]).ParamsCount
  else
    vLinesInStyle := 0;
  ABits.WriteRC(vLinesInStyle);//Linesinstyle RC 73
  ABits.WriteBS(M.Count); //Numverts BS 72
  for I := 0 to M.Count - 1 do
  begin
    vVertex := M[I];
    ABits.Write3BD(vVertex.Point);
    ABits.Write3BD(vVertex.Direction);
    ABits.Write3BD(vVertex.Miter);
    for J := 0 to vVertex.ParamsCount - 1 do
    begin
      ABits.WriteBS(vVertex.PCounts[J]);
      for K := 0 to vVertex.PCounts[J] - 1 do
        ABits.WriteBD(vVertex.Params[J, K]);
      ABits.WriteBS(0);
    end;
  end;
  Result := True;
end;

function TsgCADtoDWG.ExportMText(M: TsgDXFMText; ABits: TsgDWGBits): Boolean;
var
  vIsNotAnnotative: Boolean;
  vColumnType: Integer;

  procedure WriteXDir;
  begin
    if TsgDXFMTextEx(M).HasSecond then
      ABits.WriteXDirection(True, M.Point1)
    else
      ABits.WriteXDirection(M.Angle, M.Extrusion);
  end;

begin
  ABits.Write3BD(M.Point);// Insertion pt3 BD 10 First picked point
  ABits.Write3BD(M.Extrusion);//Extrusion 3BD 210
  WriteXDir; //X-axis dir 3BD 11
  ABits.WriteBD(M.RectWidth);
  if Version >= acR2007 then
    ABits.WriteBD(M.RectHeight);//R2007+: Rect height BD 46 Reference rectangle height
  ABits.WriteBD(M.Height);//Text height BD 40
  ABits.WriteBS(M.Align);//Attachment BS 71
  ABits.WriteBS(1);//Drawing dir BS 72
  ABits.WriteBD(0);//Extents ht BD --- Undocumented and not present in DXF or entget
  ABits.WriteBD(0);//Extents wid BD --- Undocumented and not present in DXF or entget
  ABits.WriteTV(PrepareTextForDWG(GetEntityStringValue(M)), FCodePage, True);//Text TV 1 All text in one long string
  ABits.WriteBS(M.LineSpacingStyle);//Linespacing Style BS 73
  ABits.WriteBD(M.LineSpacingFactor);//Linespacing Factor BD 44
  ABits.WriteBit(0);//Unknown bit
  if Version >= acR2004 then
  begin
    ABits.WriteBL(M.BackgroundFlags);
    if M.BackgroundFlags and $11 <> 0 then
    begin
      ABits.WriteBD(M.BackgroundScaleFactor);
      ABits.WriteCMC(M.BackgroundColor);
      ABits.WriteBL(M.BackgroundTransparency);
    end;

    if Version >= acR2018 then
    begin
      vIsNotAnnotative := not M.Annotative;
      ABits.WriteBit(vIsNotAnnotative);
      if vIsNotAnnotative then
      begin
        ABits.WriteBS(4);//Version BS Default 0
        ABits.WriteBit(1);//Default flag B Default true
        //BEGIN REDUNDANT FIELDS
        //ABits.WriteHandle(,cntDWGObjHandleHardPointer);//??Registered application H Hard pointer
        ABits.WriteBL(M.Align);//Attachment point
        WriteXDir; //X-axis dir 3BD 10
        ABits.Write3BD(M.Point);//Insertion point 3BD 11
        ABits.WriteBD(M.RectWidth);//Rect width BD 40
        ABits.WriteBD(M.RectHeight);//Rect height BD 41
        ABits.WriteBD(M.Box.Right - M.Box.Left);//Extents width BD 42
        ABits.WriteBD(M.Box.Top - M.Box.Bottom);//Extents height BD 43
        //END REDUNDANT FIELDS
        vColumnType := 0;
        ABits.WriteBS(vColumnType);//Column type BS 71 0 = No columns, 1 = static columns, 2 = dynamic columns
{ TODO: ExportMText if vColumnType <> 0}
//        if vColumnType <> 0 then
//        begin
//          vColumnHeightCount := ReadBL(APBit);//Column height count BL 72
//          SeekGeneral(APBit, [seBD, seBD]);//Columnn width BD 44;Gutter BD 45
//          vAutoHeight := ReadBit(APBit) = 1;//Auto height? B 73
//          SeekGeneral(APBit, [seB]);//Flow reversed? B 74
//          if not vAutoHeight and (vColumnType = 2) then
//            while vColumnHeightCount > 0 do
//            begin
//              SeekGeneral(APBit, [seBD]);
//              Dec(vColumnHeightCount);
//            end;
//        end;
      end;
    end;

  end;
  Result := True;
end;

function TsgCADtoDWG.ExportOle2Frame(OLE2: TsgDXFOle2Frame;
  ExportData: TObject): Boolean;
var
  vOleHeader: PsgOle2FrameHeaderData;
  vBits: TsgDWGBits;
  vExportData: TsgDWGExportDataClass;
begin
  vExportData := TsgDWGExportDataClass(ExportData);
  if vExportData.Bits <> nil then
    vBits := TsgDWGBits(vExportData.Bits)
  else
    vBits := nil;
  if vBits = nil then
  begin
    DoExport(OLE2, vExportData.PExportData);
    Result := True;
    Exit;
  end;
  vBits.AllocBlock := vBits.AllocBlock + Cardinal(Length(OLE2.BinaryData));
  vBits.WriteBS(OLE2.OLEObjectType);
  vBits.WriteBS(0);
  vBits.WriteBL(Length(OLE2.BinaryData) + SizeOf(TsgOle2FrameHeaderData));
  New(vOleHeader);
  try
    PrepareOLEHeader(OLE2, vOleHeader);
    vBits.WriteBytes(vOleHeader, SizeOf(TsgOle2FrameHeaderData));
  finally
    Dispose(vOleHeader);
  end;
  vBits.WriteBytes(@OLE2.BinaryData[1], Length(OLE2.BinaryData));
  vBits.WriteRC(1);
  Result := True;
end;

function TsgCADtoDWG.ExportOle2FrameCreateOleGroup(OLE2: TsgDXFOle2Frame;
  ExportData: TObject): Boolean;
var
  vData: TsgDWGExportDataClass;
  vOLE: TsgDXFOle2Frame;
  vEnt: TsgDXFEntity;
begin
  Result := True;
  vData := TsgDWGExportDataClass(ExportData);
  vOLE := TsgDXFOle2Frame.Create;
  vOLE.AssignEntity(OLE2);
  vOLE.Handle := OLE2.Handle;
  vEnt := TsgDXFGroup(vData.Group)[vData.GroupIndex];
  TsgDXFGroup(vData.Group).DeleteEntity(vData.GroupIndex);
  TsgDXFGroup(vData.Group).InsertEntity(vData.GroupIndex, vOLE);
  vEnt.Free;
end;

function TsgCADtoDWG.ExportPoint(P: TsgDXFPoint; ABits: TsgDWGBits): Boolean;
begin
  ABits.Write3BD(P.Point);
  ABits.WriteBT(P.ZThick);
  ABits.WriteBE(P.Extrusion);
  ABits.WriteBD(0);
  Result := True;
end;

function TsgCADtoDWG.ExportPolyline(PLine: TsgDXFPolyline; DWGCode: Word;
  ABits: TsgDWGBits): Boolean;
var
  vFlags: Byte;
  I: Integer;
  vData: TsgDirectExportData;
  vSplinedFlagPos: Cardinal;
  vSplined: Byte;
begin
  Result := True;
  if PLine.Closed then
    vFlags := 1
  else
    vFlags := 0;
  vSplined := 0;
  vSplinedFlagPos := $FFFFFFFF;
  case DWGCode of
    cntDWGObjCodePOLYLINE2D:
      begin
        ABits.WriteBS(vFlags);
        ABits.WriteBS(0);
        if IsZero(PLine.GlobalWidth) then
        begin
          ABits.WriteBD(PLine.StartWidth);
          ABits.WriteBD(PLine.EndWidth);
        end
        else
        begin
          ABits.WriteBD(PLine.GlobalWidth);
          ABits.WriteBD(PLine.GlobalWidth);
        end;
        ABits.WriteBT(PLine.ZThick);
        ABits.WriteBD(PLine.Elevation);
        ABits.WriteBE(PLine.Extrusion);
      end;
    cntDWGObjCodePOLYLINE3D:
      begin
        vSplinedFlagPos := ABits.Position;
        ABits.WriteRC(0);
        ABits.WriteRC(vFlags);
      end;
    cntDWGObjCodePOLYLINEPFACE:
      begin
        ABits.WriteBS(PLine.MeshM); //Numverts BS 71 Number of vertices in the mesh.
        ABits.WriteBS(PLine.MeshN); //Numfaces BS 72 Number of faces
      end;
    cntDWGObjCodePOLYLINEMESH:
      begin
        ABits.WriteBS(PLine.Flags or vFlags);//Flags BS 70
        ABits.WriteBS(0);//BS 75 Curve and smooth surface type.
        ABits.WriteBS(PLine.MeshM);//M vert count BS 71 M vertex count
        ABits.WriteBS(PLine.MeshN);//N vert count BS 72 N vertex count
        ABits.WriteBS(0);//M density BS 73 M vertex count
        ABits.WriteBS(0);//N density BS 74 N vertex count
      end;
  end;
  if Version >= acR2004 then
    ABits.WriteBL(PLine.Count);
  //EntMode = 0 for SeqEnd entity export
  vData := CreateExportData(PLine.Handle, 0, cnstBadHandle, cnstBadHandle, 0, PLine);
  for I := 0 to PLine.Count - 1 do //check to be sure all vertexes have Handle
  begin
    vSplined := vSplined or TsgDXFVertex(PLine.Entities[I]).Flags;
    ForceEntityHandle(PLine.Entities[I]);
  end;
  if DWGCode = cntDWGObjCodePOLYLINE3D then
    ABits.UpdateRC(vSplinedFlagPos, vSplined shr 3 and $02);// 2 if Splined
  DoExport(PLine, @vData);
  for I := 0 to PLine.Count - 1 do //export Vertexes EntMode = 0
  begin
    vData := CreateExportData(PLine.Handle, 0, GetPrevEntHandle(PLine, I),
      GetNextEntHandle(PLine, I), DWGCode, PLine);
    DoExport(PLine.Entities[I], @vData);
  end;
end;

function TsgCADtoDWG.ExportRay(R: TsgDXFRay; ABits: TsgDWGBits): Boolean;
begin
  ABits.Write3BD(R.StartPoint); //Point 3BD 10
  ABits.Write3BD(R.Direction); //Vector 3BD 11
  Result := True;
end;

function TsgCADtoDWG.WriteDateTimeAsJulian(const ADate: TDateTime; const ABits: TsgDWGBits;
  AUseRL: Boolean; PJDate: PSGCardinal = nil; PMilSec: PSGCardinal = nil;
  AWriteMilSec: Boolean = True): Integer;
var
  vTimeStamp: TTimeStamp;
begin
  Result := ABits.WriteJulianDateInt(ADate, not AUseRL, AWriteMilSec);
  vTimeStamp := DateTimeToJulianTimeStamp(ADate);
  if PJDate <> nil then
    PJDate^ := vTimeStamp.Date;
  if PMilSec <> nil then
    PMilSec^ := vTimeStamp.Time;
end;

function TsgCADtoDWG.WriteEntityHandle(const ABits: TsgDWGBits; const Entity: TsgDXFEntity;
  const ACode: Byte; AHandle: Boolean = True): Integer;
begin
  if AHandle then
    Result := ABits.WriteHandle(CastEntToID(Entity), ACode)
  else
    Result := ABits.WriteHandleRecord(CastEntToID(Entity), ACode);
end;

{ TsgDWGCreatedElements }

function TsgDWGCreatedElements.AddElement(const AHandle: UInt64; ALocation: Integer;
  ADWGObjCode: Word; const AName: string = ''; AOwnerHandle: UInt64 = cnstBadHandle;
  AObject: TObject = nil; ASize: Integer = 0): Integer;
var
  vVal: TsgDWGCreatedElement;
begin
  SortBy := seHandle;
//  ClearElem(vVal);
  vVal.Handle := AHandle;
  vVal.Location := ALocation;
  vVal.DWGObjCode := ADWGObjCode;
  vVal.Obj := AObject;
  vVal.OwnerHandle := AOwnerHandle;
  vVal.Size := ASize;
  vVal.Name := AName;
  Result := AddElement(vVal);
end;

function TsgDWGCreatedElements.AddElement(const AValue: TsgDWGCreatedElement): Integer;
var
  vIndex: Integer;

  function AddElem: Integer;
  begin
    if FSorted then
      FindBase(AValue, Result)
    else
      Result := FCount;
    if FCount = Length(FElements) then
      SetLength(FElements, ListGrow(Length(FElements)));
    if Result < FCount then
    begin
      System.Move(FElements[Result], FElements[Result + 1],
        (FCount - Result) * SizeOf(TsgDWGCreatedElement));
      FillChar(FElements[Result], SizeOf(TsgDWGCreatedElement), 0);
    end;
    FElements[Result] := AValue;
    Inc(FCount);
  end;

  function Find: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to Count - 1 do
      if Element[I].Handle = AValue.Handle then
      begin
        Result := I;
        Break;
      end;
  end;

begin
  SortBy := seHandle;
  if FFindBeforeAdd then
  begin
    if FSorted then
      vIndex := IndexOf(AValue.Handle)
    else
      vIndex := Find;
    if vIndex >= 0 then
      Result := vIndex
    else
      Result := AddElem;
  end
  else
    Result := AddElem;
end;

procedure TsgDWGCreatedElements.Clear;
begin
  Finalize(FElements);
  FCount := 0;
end;

function TsgDWGCreatedElements.CompareElements(const Element1,
  Element2: Pointer): Integer;
var
  vVal1, vVal2: PsgDWGCreatedElement;
begin
  vVal1 := PsgDWGCreatedElement(Element1);
  vVal2 := PsgDWGCreatedElement(Element2);
  case FSortBy of
    seLocation: Result := TsgTypeComparer.CmpInteger(vVal1^.Location, vVal2^.Location);
    seDWGObjCode: Result := TsgTypeComparer.CmpCardinal(vVal1^.DWGObjCode, vVal2^.DWGObjCode);
    seObject: Result := TsgTypeComparer.CmpPointer(vVal1^.Obj, vVal2^.Obj);
    seOwnerHandle: Result := sgCompareHandles(vVal1^.OwnerHandle, vVal2^.OwnerHandle);
    seName: Result := TsgTypeComparer.CmpStr(vVal1^.Name, vVal2^.Name, False);
  else //seHandle
    Result := sgCompareHandles(vVal1^.Handle, vVal2^.Handle);
  end;
end;

function TsgDWGCreatedElements.CountOf(const ALocation: Integer; var AIndex: Integer): Integer;
var
  vElem: TsgDWGCreatedElement;
begin
  AIndex := IndexOf(ALocation, @vElem);
  Result := CountElem(AIndex, vElem);
end;

function TsgDWGCreatedElements.CountOf(const OwnerHandle: UInt64; var AIndex: Integer): Integer;
var
  vElem: TsgDWGCreatedElement;
begin
  AIndex := IndexOf(OwnerHandle, True, @vElem);
  Result := CountElem(AIndex, vElem);
end;

function TsgDWGCreatedElements.CountOf(const ADWGObjCode: Word; var AIndex: Integer): Integer;
var
  vElem: TsgDWGCreatedElement;
begin
  AIndex := IndexOf(ADWGObjCode, @vElem);
  Result := CountElem(AIndex, vElem);
end;

function TsgDWGCreatedElements.CountOf(const AName: string; var AIndex: Integer): Integer;
var
  vElem: TsgDWGCreatedElement;
begin
  AIndex := IndexOf(AName, @vElem);
  Result := CountElem(AIndex, vElem);
end;

function TsgDWGCreatedElements.CountOf(const AObject: TObject; var AIndex: Integer): Integer;
var
  vElem: TsgDWGCreatedElement;
begin
  AIndex := IndexOf(AObject, @vElem);
  Result := CountElem(AIndex, vElem);
end;

function TsgDWGCreatedElements.CountElem(AIndex: Integer; Elem: TsgDWGCreatedElement): Integer;
var
  I: Integer;
begin
  if AIndex < 0 then
    Result := 0
  else
  begin
    Result := 1;
    for I := AIndex + 1 to Count - 1 do
      if CompareElements(@Elem, @FElements[I]) = 0 then
        Inc(Result)
      else
        Break;
  end;
end;

constructor TsgDWGCreatedElements.Create;
begin
  FSortBy := seHandle;
  FFindBeforeAdd := True;
end;

destructor TsgDWGCreatedElements.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TsgDWGCreatedElements.Exchange(I, J: Integer);
var
  Elem: array[0 .. SizeOf(TsgDWGCreatedElement) - 1] of Byte;
begin
  System.Move(FElements[I], Elem, SizeOf(TsgDWGCreatedElement));
  System.Move(FElements[J], FElements[I], SizeOf(TsgDWGCreatedElement));
  System.Move(Elem, FElements[J], SizeOf(TsgDWGCreatedElement));
end;

function TsgDWGCreatedElements.Find(const Elem: TsgDWGCreatedElement;
  ASortBy: TsgSortElemBy; AGotoFirstElem: Boolean = False): Integer;
var
  vNeedSort: Boolean;
begin
  vNeedSort := FSorted and (SortBy <> ASortBy);
  SortBy := ASortBy;
  if vNeedSort then
    Sort;
  if not FindBase(Elem, Result) then
    Result := -1;
  if AGotoFirstElem and (Result >= 0) then
    GotoFirstElem(Element[Result], Result);
end;

function TsgDWGCreatedElements.FindBase(const AItem: TsgDWGCreatedElement;
  var AIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  H := Count - 1;
  if FSorted then
  begin
    L := 0;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := CompareElements(@FElements[I], @AItem);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
            L := I;
        end;
      end;
    end;
    AIndex := L;
  end
  else
  begin
    AIndex := 0;
    while (AIndex <= H) and (CompareElements(@FElements[AIndex], @AItem) <> 0) do
      Inc(AIndex);
    if AIndex <= H  then
      Result := True;
  end;
end;

function TsgDWGCreatedElements.GetElement(AIndex: Integer): TsgDWGCreatedElement;
begin
  Result := FElements[AIndex];
end;

function TsgDWGCreatedElements.GetFindBeforeAdd: Boolean;
begin
  Result := FFindBeforeAdd;
end;

function TsgDWGCreatedElements.GetPtrElement(AIndex: Integer): PsgDWGCreatedElement;
begin
  Result := PsgDWGCreatedElement(@FElements[AIndex]);
end;

procedure TsgDWGCreatedElements.GotoFirstElem(const AElem: TsgDWGCreatedElement;
  var AIndex: Integer);
begin
  GotoFirstLastElem(AElem, AIndex, True);
end;

procedure TsgDWGCreatedElements.GotoFirstLastElem(const AElem: TsgDWGCreatedElement;
  var AIndex: Integer; AFirst: Boolean);
var
  D: Integer;
begin
  D := -2 * Ord(AFirst) + 1;
  AIndex := AIndex + D;
  while ((AIndex >= 0) and (AIndex < Count)) and (CompareElements(@FElements[AIndex], @AElem) = 0) do
    AIndex := AIndex + D;
  AIndex := AIndex - D;
end;

procedure TsgDWGCreatedElements.GotoLastElem(const AElem: TsgDWGCreatedElement;
  var AIndex: Integer);
begin
  GotoFirstLastElem(AElem, AIndex, False);
end;

function TsgDWGCreatedElements.IndexOf(const AName: string;
  PElem: PsgDWGCreatedElement = nil): Integer;
var
  vVal: TsgDWGCreatedElement;
begin
  ///ClearElem(vVal);
  vVal.Name := AName;
  Result := Find(vVal, seName, True);
  if PElem <> nil then PElem^ := vVal;
end;

function TsgDWGCreatedElements.IndexOf(const ALocation: Integer;
  PElem: PsgDWGCreatedElement = nil): Integer;
var
  vVal: TsgDWGCreatedElement;
begin
  ///ClearElem(vVal);
  vVal.Location := ALocation;
  Result := Find(vVal, seLocation, True);
  if PElem <> nil then PElem^ := vVal;
end;

function TsgDWGCreatedElements.IndexOf(const AHandle: UInt64; OwnerHandle: Boolean = False;
  PElem: PsgDWGCreatedElement = nil): Integer;
var
  vVal: TsgDWGCreatedElement;
  vSortBy: TsgSortElemBy;
  vGotoFirstElem: Boolean;
begin
  //ClearElem(vVal);
  if OwnerHandle then
  begin
    vSortBy := seOwnerHandle;
    vVal.OwnerHandle := AHandle;
    vGotoFirstElem := True;
  end
  else
  begin
    vVal.Handle := AHandle;
    vSortBy := seHandle;
    vGotoFirstElem := False;
  end;
  Result := Find(vVal, vSortBy, vGotoFirstElem);
  if PElem <> nil then PElem^ := vVal;
end;

function TsgDWGCreatedElements.IndexOf(const AObject: TObject;
  PElem: PsgDWGCreatedElement = nil): Integer;
var
  vVal: TsgDWGCreatedElement;
begin
  ///ClearElem(vVal);
  vVal.Obj := AObject;
  Result := Find(vVal, seObject, True);
  if PElem <> nil then PElem^ := vVal;
end;

function TsgDWGCreatedElements.IndexOf(const ADWGObjCode: Word;
  PElem: PsgDWGCreatedElement = nil): Integer;
var
  vVal: TsgDWGCreatedElement;
begin
  ///ClearElem(vVal);
  vVal.DWGObjCode := ADWGObjCode;
  Result := Find(vVal, seDWGObjCode, True);
  if PElem <> nil then PElem^ := vVal;
end;

procedure TsgDWGCreatedElements.SetFindBeforeAdd(const Value: Boolean);
begin
  if Value <> FFindBeforeAdd then
    FFindBeforeAdd := Value;
end;

procedure TsgDWGCreatedElements.SetSortBy(const Value: TsgSortElemBy);
begin
  if Value <> FSortBy then
  begin
    FSortBy := Value;
    if FSorted then
      Sort;
  end;
end;

procedure TsgDWGCreatedElements.Sort;
begin
  SortInternal(0, FCount - 1);
  FSorted := True;
end;

procedure TsgDWGCreatedElements.SortInternal(L, R: Integer);
var
  I, J: Integer;
  Elem: array[0 .. SizeOf(TsgDWGCreatedElement) - 1] of Byte;
begin
  if L < R then
  begin
    repeat
      if (R - L) = 1 then
      begin
        if CompareElements(@FElements[L], @FElements[R]) > 0 then
          Exchange(L, R);
        break;
      end;
      I := L;
      J := R;
      System.Move(FElements[(L + R) shr 1], Elem, SizeOf(TsgDWGCreatedElement));
      repeat
        while CompareElements(@FElements[I], @Elem) < 0 do
          Inc(I);
        while CompareElements(@FElements[J], @Elem) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
            Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if (J - L) > (R - I) then
      begin
        if I < R then
          SortInternal(I, R);
        R := J;
      end
      else
      begin
        if L < J then
          SortInternal(L, J);
        L := I;
      end;
    until L >= R;
  end;
end;

{ TsgDWGR18PageBuilder }

procedure TsgDWGR18PageBuilder.AddSysPage;
var
  Len: Cardinal;
begin
  Len := Length(FPages) + 1;
  SetLength(FPages, Len);
  FPages[High(FPages)].PageId := FPrevPageID + Len;
  FPages[High(FPages)].ComprSize := FPackedSize;
  FPages[High(FPages)].DataOffset := FStartOffset;
  if Assigned(FOnPageAdd) then
    FOnPageAdd(Self);
end;

function TsgDWGR18PageBuilder.Compress: Integer;
var
  P: PByte;
begin
  Result := FSectionStream.Read(FSrcPageBuff^, PageSize);
  if Result < PageSize then
  begin
    P := FSrcPageBuff;
    Inc(P, Result);
    FillChar(P^, PageSize - Result, 0);
    Result := PageSize;
  end;
end;

procedure TsgDWGR18PageBuilder.StorePage;
begin
  Move(FSrcPageBuff^, FPageData^, FPackedSize);
end;

procedure TsgDWGR18PageBuilder.Build;
begin
  FDestStartPos := FDest.Position;
  if FSectionName = sAcadSectRevHistory then
    FDataSize := 12;
  if (FSectionStream = nil) or (FSectionStream.Size = 0) then Exit;
  FDataSize := FSectionStream.Size;
  GetMem(FSrcPageBuff, PageSize);
  try
    repeat
      FStartOffset := FSectionStream.Position;
      FPackedSize := Compress; // 0 .. paked(PageSize)/PageSize
      FPackedAlignedSize := sgDWGAlignSize(FPackedSize); // 0 .. 31

      FCurrPageSize := FPackedSize + FPackedAlignedSize + SizeOf(TsgDWG2004DataPageHeader);
      FDest.Size := FDest.Size + FCurrPageSize;

      FPageHeader := PsgDWG2004DataPageHeader(FDest.Memory);
      Inc(PByte(FPageHeader), FDest.Position);

      FillChar(FPageHeader^, SizeOf(FPageHeader^), 0);
      FPageHeader^.SectionType := cntDWG2004SectionData;
      FPageHeader^.SectionId := SectionID;
      FPageHeader^.DataSize := FPackedSize;
      FPageHeader^.PageSize := FCurrPageSize; //?? PageSize in spec;
      FPageHeader^.StartOffset := FStartOffset;

      FPageData := FPageHeader;
      Inc(PByte(FPageData), SizeOf(TsgDWG2004DataPageHeader));
      StorePage;

      FPageHeader^.Checksum2 := Checksum(PByte(FPageData), 0, FPackedSize);
      FPageHeader^.Checksum1 := Checksum(PByte(FPageHeader), FPageHeader^.Checksum2, SizeOf(TsgDWG2004DataPageHeader));

      sgDWG2004PageHeaderEncode(FPageHeader, FDest.Position);

      if FPackedAlignedSize <> 0 then
      begin
        Inc(PByte(FPageData), FPackedSize);
        FillChar(FPageData^, FPackedAlignedSize, 0);
        sgDWG2004Encode(FPageData, FPackedAlignedSize);
      end;

      AddSysPage;

      FDest.Position := FDest.Size;

    until FSectionStream.Position = FSectionStream.Size;
  finally
    FreeMem(FSrcPageBuff, PageSize);
  end;
end;

constructor TsgDWGR18PageBuilder.Create(ASectionStream: TStream;
  ADest: TCustomMemoryStream; const ASectionName: string);
begin
  inherited Create;
  FSectionStream := ASectionStream;
  FDest := ADest;
  FSectionName := ASectionName;
  FSectionData := SectionData(FSectionName);
  FDataSize := 0;
end;

class function TsgDWGR18PageBuilder.GetCompressed(const ASectionName: string): Integer;
begin
  Result := SectionData(ASectionName) shr 16 and $3;
end;

class function TsgDWGR18PageBuilder.SectionData(const ASectionName: string): Int64;
var
  I: Integer;
begin
  I := DWGPagesR18.IndexOf(ASectionName);
  if I < 0 then
    raise EDWGExportError.CreateFmt(sUnknownSectionName, [ASectionName])
  else
    Result := TsgObjectInt64(DWGPagesR18.Objects[I]).FieldInt64;
end;

class function TsgDWGR18PageBuilder.GetMapID(const ASectionName: string): TsgNativeUInt;
begin
  Result := SectionData(ASectionName) shr 26 and $F;
end;

function TsgDWGR18PageBuilder.GetCompressedProp: Integer;
begin
  Result := FSectionData shr 16 and $3;
end;

function TsgDWGR18PageBuilder.GetEncrypted: Integer;
begin
  Result := FSectionData shr 18 and $3;
end;

function TsgDWGR18PageBuilder.GetPageSize: Integer;
begin
  Result := FSectionData and $7FFF;
  if Assigned(FSectionStream) and (Compressed <> 2) and (FSectionName <> sAcadSectAppInfo) then
    Result := FSectionStream.Size + sgDWGAlignSize(FSectionStream.Size, Result);
end;

function TsgDWGR18PageBuilder.GetMapOrderID: Integer;
begin
  Result := FSectionData shr 26 and $1F;
end;

{ TsgDWGR2004PageBuilderCompressed }

function TsgDWGR18PageBuilderCompressed.Compress: Integer;
begin
  inherited Compress;
  Result := FCompressor.Compress(FSrcPageBuff, PageSize);
end;

procedure TsgDWGR18PageBuilderCompressed.StorePage;
begin
  FCompressor.Stream.Read(FPageData^, FPackedSize);
end;

constructor TsgDWGR18PageBuilderCompressed.Create(ASectionStream: TStream;
  ADest: TCustomMemoryStream; const ASectionName: string);
begin
  inherited Create(ASectionStream, ADest, ASectionName);
  FCompressor := TsgDWG2004Compressor.Create;
end;

destructor TsgDWGR18PageBuilderCompressed.Destroy;
begin
  FCompressor.Free;
  inherited Destroy;
end;

{ TdwgR18SectionMapBuilder }

procedure TdwgR18SectionMapBuilder.PageAdded(Sender: TObject);
var
  Len: Integer;
  vPageBuilder: TsgDWGR18PageBuilder absolute Sender;
begin
  FPageItems[High(FPageItems)].Number := vPageBuilder.Pages[High(vPageBuilder.Pages)].PageId;
  FPageItems[High(FPageItems)].Size := vPageBuilder.CurrPageSize;

  Len := Length(FPageItems) + 1;
  SetLength(FPageItems, Len);
end;

function TdwgR18SectionMapBuilder.Add(ASectionStream: TCustomMemoryStream;
  const ASectionName: string): Integer;
var
  vPageBuilder: TsgDWGR18PageBuilder;

  function GetPageBuilder(ASectionStream: TStream; ADest: TCustomMemoryStream;
    const ASectionName: string): TsgDWGR18PageBuilder;
  begin
    if TsgDWGR18PageBuilder.GetCompressed(ASectionName) = 2 then
      Result := TsgDWGR18PageBuilderCompressed.Create(ASectionStream, ADest, ASectionName)
    else
      Result := TsgDWGR18PageBuilder.Create(ASectionStream, ADest, ASectionName);
  end;

begin
  Result := -1;
  if Assigned(ASectionStream) then
    ASectionStream.Position := 0;
  vPageBuilder := GetPageBuilder(ASectionStream, FDest, ASectionName);
  if Assigned(vPageBuilder) then
  begin
    vPageBuilder.OnPageAdd := Self.PageAdded;
    FMapOrderIDs.Add(vPageBuilder.MapOrderID);
    Result := FSections.AddObject(ASectionName, vPageBuilder);
  end;
end;

function TdwgR18SectionMapBuilder.AlignPage(const ASize: Integer): Cardinal;
var
  P: Pointer;
begin
  Result := sgDWGAlignSize(ASize);
  if Result > 0 then
  begin
    FDest.Size := FDest.Size + Result;
    P := FDest.Memory;
    Inc(PByte(P), FDest.Position);
    FillChar(P^, Result, 0);
    sgDWG2004Encode(P, Result);
    FDest.Position := FDest.Size;
  end;
end;

procedure TdwgR18SectionMapBuilder.Build;
var
  I, J: Integer;
  vPageBuilder: TsgDWGR18PageBuilder;
begin
  FMapOrderIDs.Sorted := True;

  for I := 0 to FMapOrderIDs.Count - 1 do
    if Find(FMapOrderIDs[I], J) then
    begin
      vPageBuilder := TsgDWGR18PageBuilder(FSections.Objects[J]);
      vPageBuilder.FSectionID := (FSections.Count - I) mod FSections.Count;
    end;

  for I := 0 to FSections.Count - 1 do
  begin
    vPageBuilder := TsgDWGR18PageBuilder(FSections.Objects[I]);
    vPageBuilder.PrevPageID := Length(FPageItems) - 1;
    vPageBuilder.Build;
  end;

  FSectionMapOffest := FDest.Position;
  MakeSysSections;
end;

function TdwgR18SectionMapBuilder.CalcCheckSum(
  var ASysPageHeader: TsgDWG2010SysPageHeader; AData: Pointer): Cardinal;
begin
  ASysPageHeader.CRC := 0;
  Result := Checksum(@ASysPageHeader, 0, SizeOf(TsgDWG2010SysPageHeader));
  Result := Checksum(AData, Result, ASysPageHeader.CompSize);
  ASysPageHeader.CRC := Result;
end;

constructor TdwgR18SectionMapBuilder.Create(ADest: TCustomMemoryStream);
begin
  inherited Create;
  FSections := TStringList.Create;
  FDest := ADest;
  SetLength(FPageItems, 1);
  FMapOrderIDs := TsgIntegerList.Create;
end;

destructor TdwgR18SectionMapBuilder.Destroy;
begin
  while FSections.Count > 0 do
  begin
    FSections.Objects[FSections.Count - 1].Free;
    FSections.Delete(FSections.Count - 1);
  end;
  FSections.Free;
  FMapOrderIDs.Free;
  inherited Destroy;
end;

function TdwgR18SectionMapBuilder.Find(AMapOrderID: Integer;
  var I: Integer): Boolean;
begin
  Result := False;
  I := FSections.Count - 1;
  while (I >= 0) and not Result do
    if PageBuilder[I].MapOrderID = AMapOrderID then
      Inc(Result)
    else
      Dec(I);
end;

function TdwgR18SectionMapBuilder.GetPageBuilder(
  Index: Integer): TsgDWGR18PageBuilder;
begin
  Result := TsgDWGR18PageBuilder(FSections.Objects[Index]);
end;

function TdwgR18SectionMapBuilder.MakeSysPage(const APageType: Cardinal;
  const ADecompSize: Cardinal; AData: Pointer): UInt64;
var
  vCmpressor: TsgDWG2004Compressor;
  vSysPageHeader: TsgDWG2010SysPageHeader;
begin
  vCmpressor := TsgDWG2004Compressor.Create;
  try
    vCmpressor.Compress(PByte(AData), ADecompSize);
    vSysPageHeader.PageType := APageType;
    vSysPageHeader.DecompSize := ADecompSize;
    vSysPageHeader.CompSize := vCmpressor.Stream.Size;
    vSysPageHeader.CompType := 2;
    vSysPageHeader.CRC := 0;
    CalcCheckSum(vSysPageHeader, vCmpressor.Stream.Memory);
    FDest.Write(vSysPageHeader, SizeOf(vSysPageHeader));
    FDest.Write(vCmpressor.Stream.Memory^, vCmpressor.Stream.Size);
    AlignPage(vCmpressor.Stream.Size + SizeOf(vSysPageHeader));
    Result := FDest.Position;
  finally
    vCmpressor.Free;
  end;
end;

procedure TdwgR18SectionMapBuilder.MakeSysSections;
var
  vPageBuffer, vSectionMapItem: PsgDWGR18Section;
  I, J, vLen: Integer;
  vSize: Cardinal;
  vPageBuilder: TsgDWGR18PageBuilder;
  vSysPageItem: PsgDWGR18SysPageItem;
  vS: AnsiString;

  vCmpressor: TsgDWG2004Compressor;
begin
  // pages + section pages page (1)
  vSize := SizeOf(TsgDWGR18SectionMapInfo);

  for I := 0 to FSections.Count - 1 do
  begin
    vPageBuilder := TsgDWGR18PageBuilder(FSections.Objects[I]);
    vLen := Length(vPageBuilder.Pages);
    Inc(vSize, SizeOf(TsgDWGR18Section));
    Inc(vSize, (vLen - 1) * SizeOf(TsgDWGR18StreamPage));
  end;

  // sections map
  GetMem(vPageBuffer, vSize);
  try
    PsgDWGR18SectionMapInfo(vPageBuffer)^.SectionsCount := FSections.Count;
    PsgDWGR18SectionMapInfo(vPageBuffer)^.Fileld1 := $02;
    PsgDWGR18SectionMapInfo(vPageBuffer)^.Fileld2 := $7400;
    PsgDWGR18SectionMapInfo(vPageBuffer)^.Fileld3 := $0;
    PsgDWGR18SectionMapInfo(vPageBuffer)^.Unknown := FSections.Count;

    vSectionMapItem := vPageBuffer;
    Inc(PByte(vSectionMapItem), SizeOf(TsgDWGR18SectionMapInfo));
    FillChar(vSectionMapItem^, vSize - SizeOf(TsgDWGR18SectionMapInfo), 0);
    for I := 0 to FMapOrderIDs.Count - 1 do
    begin
      if Find(FMapOrderIDs[I], J) then
      begin
        vPageBuilder := TsgDWGR18PageBuilder(FSections.Objects[J]);
        vSectionMapItem^.DataSize := vPageBuilder.DataSize;
        vSectionMapItem^.PagesAmount := Length(vPageBuilder.Pages);
        vSectionMapItem^.PageSize := vPageBuilder.PageSize;
        vSectionMapItem^.Unknown := 1;
        vSectionMapItem^.Compressed := vPageBuilder.Compressed;
        vSectionMapItem^.SectID := vPageBuilder.SectionID;
        vSectionMapItem^.Encrypted := vPageBuilder.Encrypted;
        vS := AnsiString(vPageBuilder.SectionName);
        System.Move(Pointer(vS)^, vSectionMapItem^.SecName[0], Length(vS));
        vSectionMapItem^.SecName[Length(vS)] := #0;
        for J := Low(vPageBuilder.Pages) to High(vPageBuilder.Pages) do
          vSectionMapItem^.Pages[J] := vPageBuilder.Pages[J];
        vLen := SizeOf(TsgDWGR18Section) + (Length(vPageBuilder.Pages) - 1) * SizeOf(TsgDWGR18StreamPage);
        Inc(PByte(vSectionMapItem), vLen);
      end
      else
        EDWGExportError.Create(sUnknownSectionName);
    end;
    FSectionPagesMapOffest := MakeSysPage(cntDWG2004SectionMap, vSize, vPageBuffer);
  finally
    FreeMem(vPageBuffer, vSize);
  end;

  // section pages map
  FSectionMapID := FPageItems[High(FPageItems) - 1].Number + 1;
  FPageItems[High(FPageItems)].Number := FSectionMapID;
  FPageItems[High(FPageItems)].Size := FSectionPagesMapOffest - FSectionMapOffest;

//---------------------- singular code -----------------------------------------
  vLen := Length(FPageItems) + 1;
  SetLength(FPageItems, vLen);
  FSectionPageMapID := FSectionMapID + 1;
  FPageItems[High(FPageItems)].Number := FSectionPageMapID;
  FPageItems[High(FPageItems)].Size := 0;
  vSysPageItem := PsgDWGR18SysPageItem(@FPageItems[Low(FPageItems)]);
  vCmpressor := TsgDWG2004Compressor.Create;
  try
    vCmpressor.Compress(PByte(vSysPageItem), vLen * SizeOf(TsgDWGR18SysPageItem));
    vSize := SizeOf(TsgDWG2010SysPageHeader) + vCmpressor.Stream.Size;
    vSize := vSize + Cardinal(sgDWGAlignSize(vSize));
    FPageItems[High(FPageItems)].Size := vSize;
  finally
    vCmpressor.Free;
  end;
//------------------------------------------------------------------------------

  MakeSysPage(cntDWG2004SectionPageMap, vLen * SizeOf(TsgDWGR18SysPageItem), vSysPageItem);
  FLastPageEndOffset := FDest.Size;
end;

{ TsgHandleNameLinks }

function TsgHandleNameLinks.AddItem(Handle: UInt64;
  const Name: string): Integer;
var
  Item: TsgHashItem;
begin
  Item.HashCode := Handle;
  Item.Data := nil;
  PString(@Item.Data)^ := Name;
  Result := inherited Add(Item);
end;

procedure TsgHandleNameLinks.Clear(ClearCapacity: Boolean);
begin
  Delete(0, Count);
  if ClearCapacity then
    Capacity := 0;
end;

function TsgHandleNameLinks.CmpEntHandles(const Item1, Item2: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpInt64(PsgHashItem(Item1)^.HashCode, PsgHashItem(Item2)^.HashCode);
end;

function TsgHandleNameLinks.CmpEntNames(const Item1, Item2: Pointer): Integer;
begin
  Result := TsgTypeComparer.CmpStr(PString(@PsgHashItem(Item1)^.Data)^, PString(@PsgHashItem(Item2)^.Data)^, False);
end;

constructor TsgHandleNameLinks.Create;
begin
  inherited Create;
  Sorted := False;
  Duplicates := dupAccept;
end;

procedure TsgHandleNameLinks.Delete(const Index: Integer; DelCount: Integer);
var
  I, C: Integer;
begin
  C := Count;
  I := Index;
  while (I < C) and (DelCount > 0) do
  begin
    Finalize(PString(@List^[I].Data)^);
    Dec(DelCount);
    Inc(I);
  end;
  inherited Delete(Index, DelCount)
end;

destructor TsgHandleNameLinks.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TsgHandleNameLinks.Find(const Name: string; var Index: Integer): Boolean;
var
  Item: TsgHashItem;
begin
  SortByName;
  Item.HashCode := 0;
  Item.Data := Pointer(Name);
  Result := FindBase(@Item, Index);
end;

function TsgHandleNameLinks.GetHandleByName(const Name: string): UInt64;
var
  I: Integer;
begin
  if Find(Name, I) then
    Result := List^[I].HashCode
  else
    Result := cnstBadHandle;
end;

function TsgHandleNameLinks.GetName(Index: Integer): string;
begin
  Result := PString(@List^[Index].Data)^;
end;

function TsgHandleNameLinks.GetNameByHandle(Handle: UInt64): string;
var
  I: Integer;
begin
  if Find(Handle, I) then
    Result := Name[I]
  else
    Result := '';
end;

class function TsgHandleNameLinks.IsEqualMethods(const AProc1,
  AProc2: TsgObjProcCompare): Boolean;
begin
  Result := (TMethod(AProc1).Code = TMethod(AProc2).Code) and
    (TMethod(AProc1).Data = TMethod(AProc2).Data);
end;

function TsgHandleNameLinks.Find(const Handle: UInt64; var Index: Integer): Boolean;
var
  Item: TsgHashItem;
begin
  SortByHandle;
  Item.HashCode := Handle;
  Item.Data := nil;
  Result := FindBase(@Item, Index);
end;

procedure TsgHandleNameLinks.SetSorted(const Value: Boolean);
begin
  if (Flags and cnstSortedBit <> 0) <> Value then
  begin
    if Value then
      Sort;
    Flags := (Flags and not cnstSortedBit) or (Ord(Value) shl 2);
  end;
end;

procedure TsgHandleNameLinks.SortByHandle;
begin
  if not IsEqualMethods(ProcCompare, CmpEntHandles) then
  begin
    ProcCompare := CmpEntHandles;
    if not Sorted then
      Sorted := True
    else
      Sort;
  end
  else
    Sorted := True;
end;

procedure TsgHandleNameLinks.SortByName;
begin
  if not IsEqualMethods(ProcCompare, CmpEntNames) then
  begin
    ProcCompare := CmpEntNames;
    if not Sorted then
      Sorted := True
    else
      Sort;
  end
  else
    Sorted := True;
end;

initialization

  InitDWGPages;

finalization

  ClearObjects(DWGPagesR18);
  DWGPagesR18.Free;

end.
