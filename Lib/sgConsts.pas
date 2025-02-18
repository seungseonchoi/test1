{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                 Basic constants and types                  }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools?                       }
{************************************************************}

unit sgConsts;
{$INCLUDE SGDXF.inc}
interface

{$IFDEF SG_AGG2D}
  {$IFNDEF SG_USE_AGG2D_AS_GDI}
    {$DEFINE USE_SG_AGG2D}
  {$ENDIF}
{$ENDIF}

uses
{$IFDEF SG_GLOBALHANDLE}
  {$IFDEF SG_FIREMONKEY}FMX.Dialogs{$ELSE}VCL.Dialogs{$ENDIF},
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
{$IFDEF MSWINDOWS}
  Winapi.Windows, {$IFDEF SG_HAS_ACTIVEX} ActiveX,{$ENDIF}
{$ENDIF}
{$ELSE}
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows, {$IFDEF SG_HAS_ACTIVEX} ActiveX,{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF SG_USEGDIPLUS}
{$IFDEF SG_WINAPI_GDIPLUS}
  Winapi.GDIPAPI, Winapi.GDIPOBJ,// Winapi.GDIPUTIL,
{$ENDIF}
{$ENDIF}
{$IFDEF SGFPC}
  LCLType, LCLIntf, LMessages,
{$ENDIF}
  SysUtils, Classes, Math, TypInfo,
{$IFDEF SG_FIREMONKEY}
  FMX.Types, FMX.Colors, FMX.Graphics, FMX.Objects, FMX.FontGlyphs,{$IFDEF HAS_FEATURE_ANSISTRINGS} AnsiStrings,{$ENDIF}
{$IFDEF SG_FM_LINUX}
  FMX.FontGlyphs.Linux,
{$ENDIF}
  System.Generics.Collections, sgFMXTypes, System.Math.Vectors
{$ELSE}
  Registry, Graphics
{$ENDIF}
{$IFDEF SGDEL_6}
  , RTLConsts
{$ENDIF}
{$IFDEF SGDEL_2009}
{$IFNDEF SGFPC}
  , Character
{$ENDIF}
{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF}
{$IFDEF SG_PLUGINSHOST}
  , XMLIntf
{$ENDIF}
  ;

{$IFNDEF SGDEL_6}
const
  {$IFDEF LINUX}
  sLineBreak = #10;
  {$ELSE}
  {.$IFDEF MSWINDOWS}
  sLineBreak = #13#10;
  {.$ENDIF}
  {$ENDIF}
{$ELSE}
const
  sLineBreak = System.sLineBreak;
{$ENDIF}
  cnstStdDPI = 96;

 cnstLoadCad2D: Boolean = False;
 cnstMode2D = '2D';
 cnstModeNew = 'New';
 cnstModeEmptyClose = 'EmptyClose';

{$IFDEF SGFPC}
  WM_CONTEXTMENU = LM_CONTEXTMENU;
  WM_MOUSELAST = LM_MOUSELAST;
  WM_MOUSEFIRST = LM_MOUSEFIRST;
  WM_KEYFIRST = LM_KEYFIRST;
  WM_KEYLAST = LM_KEYLAST;
  WM_DISPLAYCHANGE = $007E;
  WM_ACTIVATEAPP = $001C;
  CM_CURSORCHANGED = CM_BASE + 15;
{$ENDIF}

{$IFNDEF SG_HAS_ACTIVEX}
  DVASPECT_CONTENT   = 1;
  DVASPECT_ICON      = 4;
{$ELSE}

{$IFNDEF SGDEL_6}
type
  {$EXTERNALSYM IGlobalInterfaceTable}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IGlobalInterfaceTable)' }
  IGlobalInterfaceTable = interface(IUnknown)
    ['{00000146-0000-0000-C000-000000000046}']
    function RegisterInterfaceInGlobal(const pUnk: IUnknown; const riid: TIID;
                                       out dwCookie: DWORD): HResult; stdcall;
    function RevokeInterfaceFromGlobal(const dwCookie: DWORD): HResult; stdcall;
    function GetInterfaceFromGlobal(dwCookie: DWORD; const riid: TIID;
                                    out ppv): HResult; stdcall;
  end;
  {$ENDIF}
{$ENDIF}

type
  TsgExportFormat = (efAuto, efWmf, efEmf, efBitmap, efJpg, efJpeg, efTif,
    efTiff, efGif, efPng, efDxf, efDxt, efPlt, efHgl, efHg, efHpg, efPlo, efHp,
    efHp1, efHp2, efHp3, efHpgl, efHpgl2, efHpp, efGl, efGl2, efPrn, efSpl,
    efRtl, efPcl, efCgm, efSvg, efSwf, efPdf, efDwg, efStl, efGlsm, efLmts,
    efNmf, efObj, efObjf, efSmd, efIges, efStep, efBrep, efGCode, efMcp,
    efSvgZ, efSat, efSab, efSmt, efSmb, efXkt);

  TsgExportFormats = set of TsgExportFormat;

  TsgPictureState = (psDefault, psReadBegin,  psReading, psReadEnd,
     psReadException, psDestroying);

const
  cnstRASTR = 'RASTR';
  cnstHPGL = 'HPGL';
  cnstACIS = 'ACIS';
  cnstSTEP = 'STEP';
  cnstSVG = 'SVG';

  cnstRastrFormats = [efBitmap, efJpg, efJpeg, efTif, efTiff, efGif, efPng];
  cnstHPGLFormats = [efPlt, efHgl, efHg, efHp, efHpg, efPlo, efHp1, efHp2, efHp3,
    efHpgl, efHpgl2, efHpp, efGl, efGl2, efPrn, efSpl, efRtl, efPcl];
  cnstSVGFormats = [efSvg, efSvgZ];
  cnstACISFormats = [efSat, efSab];
  cnstSTEPFormats = [efStep];
  cnst3DFormats = [efStl, efGlsm, efLmts, efNmf, efObj, efObjf, efSmd, efIges,
    efStep, efBrep, efMcp, efSat, efSab, efSmt, efSmb, efXkt];
type
{$IFDEF SG_FIREMONKEY}
  TList = TList<Pointer>;
{$ENDIF}

{$IFDEF SG_FIREMONKEY}
  TsgFiremonkeyShowingStyle = (fssShading, fssWireframe , fssOutline);
  TsgFiremonkeyShowingStyles = set of TsgFiremonkeyShowingStyle;
var
  GlobalDrawingStyles: TsgFiremonkeyShowingStyles;
type
{$ENDIF}

{$IFDEF SG_HAS_ACTIVEX}
{$HPPEMIT '#if defined(__BORLANDC__)'}
{$HPPEMIT '  #if __BORLANDC__ < 0x0630'}
{$HPPEMIT 'typedef IGlobalInterfaceTable _di_IGlobalInterfaceTable;'}
{$HPPEMIT '  #endif'}
{$HPPEMIT '#endif'}
{$ENDIF}

{$IFDEF SGFPC}
  TMessage = TLMessage;
  TWMContextMenu = TLMContextMenu;
  TWMEraseBkgnd = TLMEraseBkgnd;
  TWMPaint = TLMPaint;
  TWMKeyDown = TLMKeyDown;
  TWMKey = TLMKey;
  TDWordFiller = record
{$IFDEF CPUX64}
    Filler: array[1..4] of Byte; // Pad DWORD to make it 8 bytes (4+4) [x64 only]
{$ENDIF}
  end;
  TWMActivateApp = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    Active: BOOL;
    ActiveFiller: TDWordFiller;
    ThreadId: Longint;
    TheadIdFiller: TDWordFiller;
    Result: LRESULT;
  end;
  THandle = {$IFNDEF MSWINDOWS}LCLType.{$ELSE}System.{$ENDIF}THandle;
{$ELSE}
  THandle = System.THandle;
{$ENDIF}

{$IFNDEF SGDEL_5}
  PColor = ^TColor;
{$ENDIF}
{$IFNDEF SGDEL_6}
  PWord = ^Word;
  PLongWord = ^LongWord;
{$IFNDEF PCardinal}
  PCardinal = ^Cardinal;
  {$EXTERNALSYM PCardinal}
{$ENDIF}
{$IFNDEF PPointer}
  PPointer = ^Pointer;
  {$EXTERNALSYM PPointer}
{$ENDIF}
{$IFNDEF PByte}
  //PByte = ^Byte;
  {.$EXTERNALSYM PByte}
{$ENDIF}
  TRoundToRange = -37..37;

  TSeekOrigin = (soBeginning, soCurrent, soEnd);
{$ENDIF}

{$IFNDEF SGDEL_7}
  UInt64 = Int64;
{$ENDIF}
{$IFNDEF SGDEL_2009}
  PUInt64 = ^UInt64;
{$ENDIF}

{basic types}

{$IFNDEF SGDEL_2009}
  TSysCharSet = set of AnsiChar;
{$ENDIF}
  sgRawByteString = {$IFDEF SG_HAS_CPSTRING}RawByteString{$ELSE}AnsiString{$ENDIF};
  sgUnicodeStr = {$IFDEF UNICODESTRING_TYPE_DEFINED}UnicodeString{$ELSE}WideString{$ENDIF};
  sgUnicodeChar = {$IFDEF UNICODESTRING_TYPE_DEFINED}{$IFDEF UNICODE}Char{$ELSE}UnicodeChar{$ENDIF}{$ELSE}WideChar{$ENDIF};

{$IFDEF SG_FM_MOBILE}
//  AnsiChar = UTF8Char; // move to sgFMXTypes.pas
//  PAnsiChar = PUTF8Char;
//  AnsiString = UTF8String;
//  WideString = string;
//  PBytes = ^TBytes;
{$ENDIF}

{$IFDEF BCB}
  {$IFNDEF SGDEL_6}
  PSGCardinal = sgConsts.PCardinal;
  {$ELSE}
  PSGCardinal = ^Cardinal;
  {$ENDIF}
  {$IFNDEF SGDEL_2009}
  PPointer = ^Pointer;
  {$ENDIF}
{$ELSE}
  PSGCardinal = PCardinal;
{$ENDIF}

  PPoints = ^TPoints;
  TPoints = array[Byte] of TPoint;

{$IFDEF SGFPC}
  TPointF = Types.TPointF;
  PPointF = ^TPointF;
{$ELSE}
{$IFDEF SGDEL_XE2}
  TPointF = System.Types.TPointF;
  PPointF = System.Types.PPointF;
{$ELSE}
  TPointF = ActiveX.TPointF;
  PPointF = ^TPointF;

  PRectF = ^TRectF;
  TRectF = record
  case Integer of
    0: (Left, Top, Right, Bottom: System.Single);
    1: (TopLeft, BottomRight: TPointF);
  end;

{$ENDIF}
{$ENDIF}

{$IFDEF SGFPC}
const
  PAN_FAMILY_TEXT_DISPLAY = 2;
  PAN_FAMILY_SCRIPT = 3;
  PAN_FAMILY_DECORATIVE = 4;
  PAN_FAMILY_PICTORIAL = 5;

  PAN_NO_FIT = 1;

  PAN_STRAIGHT_ARMS_HORZ = 2;
  PAN_STRAIGHT_ARMS_DOUBLE_SERIF = 6;
  PAN_BENT_ARMS_HORZ = 7;

type
  {$IFDEF SG_NON_WIN_PLATFORM}
  LCID = DWORD;
  {$ENDIF}
  PPanose = ^TPanose;
  tagPANOSE = record
    bFamilyType: Byte;
    bSerifStyle: Byte;
    bWeight: Byte;
    bProportion: Byte;
    bContrast: Byte;
    bStrokeVariation: Byte;
    bArmStyle: Byte;
    bLetterform: Byte;
    bMidline: Byte;
    bXHeight: Byte;
  end;
  TPanose = tagPANOSE;
  PANOSE = tagPANOSE;

  PRgnDataHeader = ^TRgnDataHeader;
  _RGNDATAHEADER = record
    dwSize: DWORD;
    iType: DWORD;
    nCount: DWORD;
    nRgnSize: DWORD;
    rcBound: TRect;
  end;
  TRgnDataHeader = _RGNDATAHEADER;
  RGNDATAHEADER = _RGNDATAHEADER;
{$IFDEF SG_NON_WIN_PLATFORM}
  PRgnData = ^TRgnData;
  _RGNDATA = record
    rdh: TRgnDataHeader;
    Buffer: array[0..0] of AnsiChar;
    Reserved: array[0..2] of AnsiChar;
  end;
  TRgnData = _RGNDATA;
  RGNDATA = _RGNDATA;
{$ENDIF}
  PEnhMetaHeader = ^TEnhMetaHeader;
  tagENHMETAHEADER = record
    iType: DWORD;          { Record type EMR_HEADER}
    nSize: DWORD;          { Record size in bytes.  This may be greater
                             than the sizeof(TEnhMetaHeader). }
    rclBounds: TRect;     { Inclusive-inclusive bounds in device units}
    rclFrame: TRect;      { Inclusive-inclusive Picture Frame of metafile in .01 mm units}
    dSignature: DWORD;     { Signature.  Must be ENHMETA_SIGNATURE.}
    nVersion: DWORD;       { Version number}
    nBytes: DWORD;         { Size of the metafile in bytes}
    nRecords: DWORD;       { Number of records in the metafile}
    nHandles: Word;        { Number of handles in the handle table
                             Handle index zero is reserved. }
    sReserved: Word;       { Reserved.  Must be zero.}
    nDescription: DWORD;   { Number of chars in the unicode description string
                             This is 0 if there is no description string }
    offDescription: DWORD; { Offset to the metafile description record. }
                           { This is 0 if there is no description string }
    nPalEntries: DWORD;    { Number of entries in the metafile palette.}
    szlDevice: TSize;      { Size of the reference device in pels}
    szlMillimeters: TSize; { Size of the reference device in millimeters}
    cbPixelFormat: DWORD;  { Size of TPixelFormatDescriptor information }
                           { This is 0 if no pixel format is set }
    offPixelFormat: DWORD; { Offset to TPixelFormatDescriptor }
                           { This is 0 if no pixel format is set }
    bOpenGL: DWORD;        { True if OpenGL commands are present in }
                           { the metafile, otherwise FALSE }
    szlMicrometers: TSize; { Size of the reference device in micrometers }
  end;
  TEnhMetaHeader = tagENHMETAHEADER;
  ENHMETAHEADER = tagENHMETAHEADER;

  TPenData = record
    Handle: HPen;
    Color: TColor;
    Width: Integer;
    Style: TPenStyle;
  end;

  TBrushData = record
    Handle: HBrush;
    Color: TColor;
    Bitmap: TBitmap;
    Style: TBrushStyle;
    OwnsBitmap: Boolean;
  end;

  TResData = record
    Handle: THandle;
  end;

  PResource = ^TResource;
  TResource = record
    Next: PResource;
    RefCount: Integer;
    Handle: THandle;
    HashCode: Word;
    Owner: TThreadID;
    case Integer of
      0: (Data: TResData);
      1: (Font: TFontData);
      2: (Pen: TPenData);
      3: (Brush: TBrushData);
  end;
{$ENDIF}

{$IFNDEF SGDEL_6}
type
    PBoolean      = ^Boolean;
    PByte         = ^Byte;
const
    PathDelim  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
    varInt64 = $0014; { vt_i8         20 }
    varShortInt = $0010; { vt_i1          }
    varWord     = $0012; { vt_ui2         }
    varLongWord = $0013; { vt_ui4         }
{$ELSE}
{$IFDEF SGFPC}type PBoolean = System.PBoolean;{$ENDIF}
{$ENDIF}
{$IFNDEF SGDEL_2009}{$IFNDEF SGFPC}
const
  varUInt64   = $0015; { vt_ui8         21 }
  varUString  = $0102; { Unicode string 258 } {not OLE compatible }
{$ENDIF}{$ENDIF}

type
{$IFDEF SGFPC}
TLocaleID = {$IFDEF SG_WIN_FPC}Cardinal{$ELSE}LCID{$ENDIF};
{$ELSE}
{$IFNDEF SGDEL_XE}
  TLocaleID = LCID;
{$IFDEF MACOS}
  TLocaleID = CFLocaleRef;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF SG_USEGDIPLUS}
{$IFDEF SG_WINAPI_GDIPLUS}
{$HPPEMIT 'typedef Gdiplus::Graphics TGPGraphics;'}
{$HPPEMIT 'typedef Gdiplus::Image TGPImage;'}
{$HPPEMIT 'typedef Gdiplus::ImageAttributes TGPImageAttributes;'}
{$ENDIF}
{$ENDIF}

  TsgSilentMode = (smNo, smNoSplash, smNoMain, smNoAll);

  TsgLogOperation = (loOtherAction, loOpen, loSave, loClose, loException,
    loClipboardCopy, loClipboardPaste, loCreateNew);

  TMeasureToolMode = (mtDistance, mtArea, mtPolylineLength, mtView);
  TsgMeasureMode3d = (mmNone, mmHighLighting, mmSnap, mmSnapEdge, mmOffAll,
    mmSnapPlan, mmSnapBetweenEdge, mmSnapPoint);
  TsgMeasureOperation = (moAddPoint, moStopInput, moContinue, moFinish);

  TsgDXFViewDirection = (vdDefault, vdTop, vdBottom, vdLeft, vdRight, vdFront, vdBack,
     vdSWIsometric, vdSEIsometric, vdNWIsometric, vdNEIsometric, vdDecard, vdGeodezik);

  TsgEntityOperation = (eoEquals, eoNotEquals, eoGreater, eoLess, eoSelectAll);

  TsgLogParam = record
    Operation: TsgLogOperation;
    Id: UInt64;
    Obj: TObject;
    Info: String;
  end;

  TsgLogger = class
  public
    function FileLogging(const AParam: TsgLogParam): Integer;  virtual; abstract;
  end;

  TsgObjProcToString = function(const Sender: TObject): string of object;
  TsgObjProcTranslate = function(const AKey: string): string of object;
  TCmdSymReplaceProc = function(const AText: sgUnicodeStr): sgUnicodeStr;

  TsgCustomProcCompare = function(const Value1, Value2): Integer;

  TsgTIFFCompressionType = (ctLZW, ctDeflate, ctJPEG, ctCCITT3, ctCCITT4,
    ctCCITT6, ctRle, ctAuto, ctNone);

  TsgAreaType = (arGeometrical, arContour, arUser, arCalculated);
  TsgButtType = (btNone, btCut, btEmpty, btZigZag);
  TsgGeoType = (gtByPattern, gtLine, gtArc);
  TsgFlightOfSteps = (fsFull, fsPart, fsIntermediate,
    fsDiagonalPart, fsDiagonalIntermediate);

  TbtiEntity = (btiUndefined, btiConstruction, btiDimConstruction, btiComplex,
    btiComplexBroad, btiComplexLinear, btiElement, btiElementBroad,
    btiElementLinear, btiElementCarved, btiElementModifier, btiArea,
    btiComplexArea, btiLabel, btiDimLabel, btiBlockPattern);
  TbtiEntities = set of TbtiEntity;

  TsgByteSet = set of Byte;

  TsgBTIContolType = (bctLabel, bctEdit, bctDate, bctCombobox, bctXMLCombobox, bctNone);

  TsgAnnotType = (atText, atLink, atFreeText, atLine, atSquare, atCircle,
    atPolygon, atPolyLine, atHighlight, atUnderline, atSquiggly, atStrikeOut,
    atStamp, atCaret, atInk, atPopup, atFileAttachment, atSound, atMovie,
    atWidget, atScreen, atPrinterMark, atTrapNet, atWatermark, at3D, atRedact);

  TsgColorType = (ctPen, ctBrush, ctFont, ctBrushClear);

  TsgTTFMode = (ttfAuto, ttfGDI, ttfPolyPolyline, ttfPolyPolygon, ttfPolyFillAndEdge);
  TsgTextType = (ttTextOut, ttTextOutW, ttExTextOut, ttExtTextOutW, ttExtTextOutA);

  TsgDimNameVal = (vnDIMASZ, vnDIMBLK, vnDIMBLK1, vnDIMBLK2, vnDIMLRBLK,
    vnDIMSD1, vnDIMSD2, vnDIMSE1, vnDIMSE2, vnDIMCLRD, vnDIMCLRE, vnDIMCLRT,
    vnDIMLWD, vnDIMLWE, vnDIMTIX, vnDIMLFAC, vnDIMDEC, vnDIMTXT, vnDIMTAD,
    vnDIMCEN, vnDIMTXSTY, vnDIMGAP, vnDIMEXE, vnDIMEXO, vnDIMTIH, vnDIMTOH,
    vnDIMSCALE, vnDIMENBL, vnDIMTXTALIGN, vnDIMSAH, vnDIMPOST, vnDIMTP, vnDIMTM,
    vnDIMDSEP, vnDIMLUNIT, vnDIMFRAC, vnDIMALT, vnDIMALTF, vnDIMAPOST);
  TsgDimNameVals = set of TsgDimNameVal;

  TsgDimArrowType = (datUndefined{$IFNDEF SGFPC} {$IFDEF SGDEL_6} = -1{$ENDIF}{$ENDIF},
    datClosedfilled, datClosedblank, datClosed, datDot, datArchtick, datOblique,
    datOpen, datOrigin, datOrigin2, datOpen90, datOpen30, datDotsmall, datDotblank,
    datDotsmallbank, datBoxblank, datBoxfilled, datDatumblank, datDatumfilled,
    datIntegral, datUserarrow, datNone);

  TsgDimLimitUnits = (luUndefined, luScientific, luDecimal, luEngineering,
   luArchitectural, luFractional, luWindowsDesktop);

  TsgDimensionType = (dmtRotHorVert, dmtAligned, dmtAngular, dmtDiameter,
    dmtRadius, dmtAngular3Point, dmtOrdinate, dmtArcDimension, dmtUndefined);

  TsgLayoutDetectorMode = (ldmModel, ldmAllLayouts, ldmLayoutByName,
    ldmAllPaperSpaces, ldmCurrentLayout);

  TsgPlotLayoutFlag = ( plfPlotViewportBorders, plfShowPlotStyles,
    plfPlotCentered, plfPlotHidden, plfUseStandardScale, plfPlotPlotStyles,
    plfScaleLineweights, plfPrintLineweights, plf256bit, plfDrawViewportsFirst,
    plfModelType, plfUpdatePaper, plfZoomToPaperOnUpdate,
    plfInitializing, plfPrevPlotInit );
  TsgPlotLayoutFlags = set of TsgPlotLayoutFlag;

  TsgPlotType = (ptLastScreenDisplay, ptDrawingExtents, ptDrawingLimits,
    ptViewSpecified, ptWindowSpecified, ptLayoutInformation);

  TsgPlotPaperUnits = (ppuInches, ppuMM, ppuPixels);

  TsgPlotRotation = (prNoRotation, pr90DegreesCounterCW, prUpsideDown,
    pr90DegreesCW);

  TsgPrintMode = (pntNone, pntDirectColor, pntPrint);

  // lmByPatternName disabled in Inventory
  TsgSetConstructionLayerMode = (lmCurrent, lmByPatternLayer, lmByPatternName);

  TSizeMode = (smActual, smMaximum, smCustom, smSame);

  TsgDataId = (diUndefined, diClassifiers, diSettings, diMapEntities,
    diInspectorGroups, diLockPaint, diSwitchImage, diPatterns);

  TsgExternalFilesMode = (efNoIterateChildren, efAddXRef, efAddImageDef,
{$IFDEF DATA_LINK}
    efDataLink,
{$ENDIF}
    efConvertXrefInFullPath);
  TsgExternalFilesModes = set of TsgExternalFilesMode;

{$IFDEF SG_FIREMONKEY}
  TsgLangugeID = (lgAny, lgEnglish, lgRussian, lgDeutsch, lgFrench,
    lgChina, lgJapanise, lgKorean, lgEstonian, lgItaliano, lgNederlands,
    lgBrazilian);
{$ELSE}
  TsgLangugeID = (lgAny, lgRussian, lgChina, lgJapanise, lgKorean);
{$ENDIF}
  TsgLangugeIDs = set of TsgLangugeID;


  TsgCopyMode = (cmCopy, cmAppend);

  PsgFloat = ^TsgFloat;
  TsgFloat = Double;

  PsgKnotMult = ^TsgKnotMult;
  TsgKnotMult = record
    Knot: Double;
    Mult: Integer;
  end;

  PF2DPoint = ^TF2DPoint;
  TF2DPoint = packed record
  case Integer of
    0: (X, Y: TsgFloat);
    1: (V: array[0..1] of TsgFloat);
  end;

  PFPoint = ^TFPoint;
  TFPoint = record
  case Integer of
    0: (X, Y, Z: TsgFloat);
    1: (Point2D: TF2DPoint; Elevation: TsgFloat);
    2: (V: array[0..2] of TsgFloat);
  end;

  PF4DPoint = ^TF4DPoint;
  TF4DPoint = record
  case Integer of
    0: (X, Y, Z, W: TsgFloat);
    1: (Point3D: TFPoint; Weight: TsgFloat);
    2: (V: array[0..3] of TsgFloat);
  end;

  // Extended CAD data
  PcadExtendedData = ^TcadExtendedData;
  TcadExtendedData = packed record
    Param1: Pointer;// 4 byte field
    IsDotted: LongBool;
    { Fields below may be added in future versions }
    AnsiStr: PAnsiChar;
  end;

  PcadData = ^TcadData;
  TcadData = packed record
    Tag: Word;       // classindex, DXF_LINE, DXF_SOLID etc.
    Count: Word;     // number of child entities
    TickCount: Word;
    Flags: Byte;     // Flags byte
                     // for polylines: low bit <> 0 - is closed
                     // for layers:
                     //  0 - Visible
                     //  2 - Frozen
                     //  4 - Locked
                     //  8 - IsPlotting
                     // for text: low bit = 0 - is SHX - else - TTF; 1-bit == Backward; 2-bit == UpsideDown
                     // for spline is degree
                     // for 3dface edges visibility
    Style: Byte;     // Style (pen, brush...) - for future versions
    Dimension: Integer;
    DashDots: PFPoint;
    DashDotsCount: Integer;
    Color: Integer;  // Color of entity
    Ticks: Pointer;
    Thickness: TsgFloat;// for future versions
    Rotation: TsgFloat; // Text or block rotation angle
    Layer: PWideChar;  // Layer name (only one layer for element)
    Text:  PWideChar;  // Pointer to text string
    FontName: PWideChar;
    Handle: THandle;
    Undefined1: Integer;
    Undefined2: TsgFloat;// Double
    Undefined3: TsgFloat;// Double
    CADExtendedData: PcadExtendedData;
    Point: TFPoint;   // Coordinates of the first point
    Point1: TFPoint;  // Coordinates of the second point
    Point2: TFPoint;  // Coordinates of the third point
    Point3: TFPoint;  // Coordinates of the fourth point
    case Integer of
    0: (Radius, StartAngle, EndAngle, Ratio: TsgFloat; EntityType: Byte); // for arcs (NOT in DXFEnum)
    1: (Block: THandle; Scale: TFPoint); // for Inserts (NOT in DXFEnum)
    2: (FHeight, FScale, RWidth, RHeight: TsgFloat; HAlign, VAlign: Byte); // for Text
    3: (Points: PFPoint; CountPointOfSegments: Integer);                // for polylines (in DXFEnum)
    4: (SolidPoints: PFPoint{PsgPoints4}; SolidPointsCount, SolidInternalFlags: Word);// for solid, trace, 3dface (in DXFEnum)
  end;

type
  //------------------------ cad.dll/libcad.so ------------------------
  // CADLoad content type parameter
  TsgCADLoadContentType = (lctFileName, lctMemory, lctIUnknown, lctFileHandle, lctResource, lctHGlobal);
  // events support
  TsgCADInvokeNotify = procedure(Handle: THandle; Param: Pointer); stdcall;
  TsgCADProgressNotify = procedure(Handle: THandle; Stage: Integer; Percent: Byte;
    Msg: PChar; var AContinue: Boolean; Param: Pointer); stdcall;
  //------------------------ cad.dll/libcad.so ------------------------

  PsgLineState = ^TsgLineState;
  TsgLineState = packed record
    Point1: TFPoint;
    Point2: TFPoint;
    State: Integer;
  end;

  { not used now
  PsgLineI = ^TsgLineI;
  TsgLineI = record
    Color: Integer;
    case Integer of
      0: (X1, Y1, X2, Y2: Integer);
      1: (P1, P2: TPoint);
  end;}

{$IFNDEF SGDEL_XE2}
{$IFNDEF SGFPC}
  TPolygon = array of TPointF;
{$ENDIF}
{$ENDIF}

{$IFNDEF SGDEL_2007}
{$IFNDEF SGFPC}
  TBytes = array of Byte;
  TCharArray = array of Char;
{$ENDIF}
{$ENDIF}

  TAnsiStringArray = array of AnsiString;

  PdxfPoint = ^TdxfPoint;
  TdxfPoint = record
    X,Y,Z: Single;
  end;

  PdxfData = ^TdxfData;
  TdxfData = packed record
    Tag: Word;       // classindex, DXF_LINE, DXF_SOLID etc.
    Count: Word;     // number of child entities
    TickCount: Word;
    Flags: Byte;     // Flags byte
                     // for polylines: low bit <> 0 - is closed
                     // for layers: low bit <> 0 - is invisible
    Style: Byte;     // Style (pen, brush...) - for future versions
    Dimension: Integer;
    DashDots: PdxfPoint;
    DashDotsCount: Integer;
    Color: Integer;  // Color of entity
    Ticks: Pointer;
    Thickness: Single;// for future versions
    Rotation: Single; // Text or block rotation angle
    Layer: PChar;  // Layer name (only one layer for element)
    Text:  PChar;  // Pointer to text string
    FontName: PChar;
    Handle: THandle;
    CADExtendedData: PcadExtendedData;
    Point: TdxfPoint;   // Coordinates of the first point
    Point1: TdxfPoint;  // Coordinates of the second point
    Point2: TdxfPoint;  // Coordinates of the third point
    Point3: TdxfPoint;  // Coordinates of the fourth point
    case Integer of
    0: (Radius, StartAngle, EndAngle, Ratio: Single; EntityType: Byte); // for arcs (NOT in DXFEnum)
    1: (Block: THandle; Scale: TdxfPoint); // for Inserts (NOT in DXFEnum)
    2: (FHeight, FScale, RWidth, RHeight: Single; HAlign, VAlign: Byte); // for Text
    3: (Points: PdxfPoint);                // for polylines (in DXFEnum)
  end;

  PsgCADPalett = ^TsgCADPalett;
  TsgCADPalett = array[0..255] of Cardinal;// AutoCAD 8-bit colors

  TsgEntityMode = (emCustomDraw, emAskOnDelete, emReadOnly);
  TsgEntityModes = set of TsgEntityMode;

  PsgCustomSelectMode = ^TsgCustomSelectMode;
  TsgCustomSelectMode = packed record
    Mode: TsgEntityModes;
    Color: TColor;
    LineWidth: Integer;
    PenStyle: TPenStyle;
  end;

  PF2DLine = ^TF2DLine;
  TF2DLine = packed record
    Point1: TF2DPoint;
    Point2: TF2DPoint;
  end;

  PsgArcR = ^TsgArcR;
  TsgArcR = record
    Center: TFPoint;
    Radius: Double;
    AngleS: Double;
    AngleE: Double;
  end;

  PsgPlotScale = ^TsgPlotScale;
  TsgPlotScale = record
    Numerator: Double;
    Denomenator: Double;
  end;

  PsgRastrExportParams = ^TsgRastrExportParams;
  TsgRastrExportParams = record
    ExportFormat       : TsgExportFormat;
    SizeMode           : TSizeMode;
    Width              : Integer;
    Height             : Integer;
    MaxDim             : Integer;
    Depth              : TPixelFormat;
    Compression        : TsgTIFFCompressionType;// early this record was in fSizeDialog - not kernel
    DPU                : TPoint;
    MeasureInPixels    : Boolean;
    Transparent        : Boolean;
    Quality            : Integer;//range 0..100/ default iJPEG
    SaveProportion     : Boolean;
//    CadScale           : Double;
    Size               : TF2DPoint;//in m
    DPM                : Double;
    ImageQuality       : Double;
    UserScale          : string;
    BlackWhitePic      : Boolean;
    Smooth             : Boolean;
  end;

  TmvFontStyle =  (fmBold, fmItalic, fmUnderline, fmStrikeOut, fmCondensed, fmUpward, fmDownward);
  TmvFontStyles = set of TmvFontStyle;

  TsgProgID = (piCADNavigator, piOther);

  PsgAttribVertex = ^TsgAttribVertex;
  TsgAttribVertex = record
    Index: Integer;
    Vertex: TObject;
    State: Integer;
  end;

const
{$IFDEF SG_NON_WIN_PLATFORM}
  {$EXTERNALSYM CP_ACP}
  CP_ACP = 0;
  {$EXTERNALSYM CP_OEMCP}
  CP_OEMCP = 1;
  {$EXTERNALSYM CP_MACCP}
  CP_MACCP = 2;
  {$EXTERNALSYM CP_THREAD_ACP}
  CP_THREAD_ACP = 3;
  {$EXTERNALSYM CP_SYMBOL}
  CP_SYMBOL = 42;
  {$EXTERNALSYM CP_UTF7}
  CP_UTF7 = 65000;
  {$EXTERNALSYM CP_UTF8}
  CP_UTF8 = 65001;
  {$EXTERNALSYM CP_UTF16}
  CP_UTF16 = 12000;
  {$EXTERNALSYM CP_8859_1}
  CP_8859_1 = 28591;
  {$EXTERNALSYM E_POINTER}
  E_POINTER = HRESULT($80004003);

  {$IFNDEF MSWINDOWS}
{$IFDEF SGFPC}
type
  TWMRButtonUp = TLMRButtonUp;
  TWMMButtonUp = TLMMButtonUp;
{$ENDIF}
  {$ENDIF}

const
  VK_ESCAPE = 27;
  HELP_CONTENTS = 3;
{$IFDEF SGFPC}
  WM_RBUTTONUP = LM_RBUTTONDOWN;
  WM_MBUTTONUP = LM_MBUTTONDOWN;
{$ENDIF}
{$ENDIF}

  clEmpty   = $5FFFFFFF;

{$IFDEF SG_HAS_WINAPI_INTERFACE}
   {$IFDEF SGFPC}
   VK_RETURN = 13;
   {$ENDIF}
{$ELSE}
  {$EXTERNALSYM VK_RETURN}
  VK_RETURN = {$IFDEF SG_FIREMONKEY}vkReturn{$ELSE}13{$ENDIF};
{$ENDIF}

{$IFDEF SG_FIREMONKEY}
  {$EXTERNALSYM VK_BACK}
  VK_BACK = vkBack;
{$ENDIF}

  {$IFDEF SGFPC}
  OLEMISC_ISLINKOBJECT = $40;
  {$ENDIF}

{$IFDEF SG_GLOBALHANDLE}
var
  GlobalIndex: Integer = 0;
  GlobalList: TList = nil;
  GlobalListItemInfo: TsgObjProcToString = nil;
{$ENDIF}

type
  TsgCurveProperty  = (lpVisible, lpJoin, lpArc, lpSegment, lpCut);
  TsgCurveProperties = set of TsgCurveProperty;

  TsgListType = (ltNil, ltList, ltFPoint, ltF2DPoint, ltF4DPoint, ltDouble, ltSingle,
    ltFloat, ltInt64, ltInteger, ltPointer, ltHashItem, ltPointF, ltObject,
    ltObjectList, ltHashItemObject, ltKnots);

  PF2DCurveEx = ^TF2DCurveEx;
  TF2DCurveEx = packed record
    Flags: TsgCurveProperties;
    case Integer of
      0: (Line: TF2DLine);
      1: (Arc: TsgArcR);
  end;

  PF2DRect = ^TF2DRect;
  TF2DRect = packed record
  case Integer of
    0: (Left, Top, Right, Bottom: TsgFloat);
    1: (TopLeft, BottomRight: TF2DPoint);
    2: (V: array[0 .. 1] of TF2DPoint);
  end;

  PsgAffineMatrix = ^TsgAffineMatrix;
  TsgAffineMatrix = record
  case Integer of
    0: (V1, V2, V3: array[0 .. 2] of TsgFloat);
    1: (M: array[0 .. 2, 0 .. 2] of TsgFloat);
    2: (EX, EY, EZ: TFPoint);
    3: (V: array[0 .. 2] of TFPoint);
  end;

  PFMatrix = ^TFMatrix;
  TFMatrix = record
  case Integer of
    0: (V1, V2, V3, V4: array[0 .. 2] of TsgFloat);
    1: (M: array[0 .. 3, 0 .. 2] of TsgFloat);
    2: (EX, EY, EZ, E0: TFPoint);
    3: (V: array[0 .. 3] of TFPoint);
    4: (Affine: TsgAffineMatrix);
  end;

  PFRect = ^TFRect;
  TFRect = record
  case Integer of
    0: (Left, Top, Z1, Right, Bottom, Z2: TsgFloat);
    1: (TopLeft, BottomRight: TFPoint);
    2: (V: array[0 .. 1] of TFPoint);
    3: (D: array[0 .. 5] of TsgFloat);
  end;

  //0 - reset clip
  //1 - clip by coord in extents
  //2 - clip by cad coords
  //3 - draw view rect in abviewer
  //4 - clip using in GetImage(clip rect in screen coords)
  //7 - clip actual size for CADNavigator
  TsgClipping = record
    ClipMode: Integer;
    ClipRect: TFRect;
  end;

  Psgversion = ^TsgVersion;
  TsgVersion = record
    Major: Integer;
    Minor: Integer;
  end;

  TsgBuildVersion = record
  case Byte of
    0: (Major, Minor, Release, Build: Integer);
    1: (Values: array[0..3] of Integer);
  end;

  TsgCADTracerState = (tsNone, tsActive, tsParallel, tsOrtho);

  TsgTraceRecord = record
    Point: TFPoint;
    TracerState: TsgCADTracerState;
  end;

  TMeasurePolylineOption = (mpoPolyLength, mpoSegmentsAngle, mpoSegmentLength,
    mpoSegmentAngleXY, mpoSegmentAngleByZ, mpoSegmentDX, mpoSegmentDY, mpoSegmentDZ);
  TMeasurePolylineOptions = set of TMeasurePolylineOption;

  // Modes of 3D objects intersection
  TIntersectionMode = (imNone, imPoint, imLine, imPlane);
  // Object snap modes
  TObjectSnapMode = (osInsert, osEndPt, osCenter, osNode, osMiddle, osNormal,
    osNearest, osIntersection, osExtension, osDisabled, osGrid, osOrtho,
    osPolar, osAngle, osDinamic, osTrace, osParallel, osQuadrant, osTangent);
  // Object snap state
  TObjectSnapState = set of TObjectSnapMode;

  TParseUnicodeMode = (puU, puM, puDP);
  TParseUnicodeModes = set of TParseUnicodeMode;

  TsgActivePoint = (apLeftTop, apLeftBottom, apCenter, apRightTop, apRightBottom);
  // Axes in 3-Dimensional cartesian coordinates
  TsgAxes = (axisX, axisY, axisZ);
  TsgAxisType = (atCenter, atLeft, atRight);

  PsgArcsParams = ^TsgArcsParams;
  TsgArcsParams = record
    Center: TFPoint;
    Radius1: Double;
    AngleS1: Double;
    AngleE1: Double;
    Radius2: Double;
    AngleS2: Double;
    AngleE2: Double;
  end;

  TsgDefaultView = (df2D, df3D);

  TsgFragmentAutoScreen = (fasNone, fasBMP, fasEMF);

  // CAD drawing border size
  TsgBorderSize = Double;
  // CAD drawing border type
  TsgBorderType = (btGlobal, btRatio, btPixel);
  TsgCachingRasterMode = (crmNone, crmCache, crmDoubleCache);
  // The entities to be drawn
  TsgCADEntities = (ceEntity, cePoint, ceLine, ceSolid, ceTrace, ceCircle, ceArc,
     ceEllipse, cePolyline, ceLWPolyline, ceSpline, ceHelix, ceLeader, ceInsert,
     ceDimension, ceTolerance, ceMText, ceShape, ceText, ceAttdef, ceAttrib,
     ce3dFace, ceImageEnt, ceViewport, ceRegion, ceBody, ce3DSolid, cePattern,
     ceGradient, ceOle2Frame, cePolyPolygon, ceGradientPolygon, ceCurvePolygon,
     ceHatch, ceACADTable, ceFlatPoly, ceFlatHatch, ceXRef, ceProxy, cePath,
     ceWipeOut, ceMLine,  ceMPolygon {$IFDEF SG_BTI},cePolyPolyline2D{$ENDIF},
     ceSurface, ceSeqEnd, ceSolidObject, ceIges, ceStep, ceBrep, ceParasolid,
     ceFlatPoly3D, ceRay, ceXline, ceMLeader, ceMesh, ceInventor, ceEntity3D);

  TsgCADEnities = TsgCADEntities{$IFDEF SGDEL_2005}deprecated{$IFDEF SGDEL_2009}'This type is obsolete and will be deleted in the next version. Please, use the following type - TsgCADEntities!'{$ENDIF}{$ENDIF};

  //see below GetCADEntityTypeString and add string there!!
  TsgCADEntityType = (etNone, etCustomVertex, etLine, etPolyLine, etLWPolyLine,
    etRect, etCircle, etEllipse, etMText, etLeader, etPath, etInsert, etBlock,
    etArc, etMulti, etDimAligned, etHatch, etText, etSolid, etSpline,
    etViewPort, etDimension, etSquare, etPoint, etPolyPolygon, etClipInsert,
    etACADTable, etTrace, etTolerance, etShape, etAttdef, etMLine, etWipeOut,
    etImageEnt, etHelix, etOle2Frame, etCloud, etMPolygon, et3dFace
//{$IFDEF SG_BTI}
    ,etConstruction, etDimConstruction, etComplex, etElement, etComplexBroad,
    etComplexLinear, etElementBroad, etElementLinear, etElementCarved,
    etElementModifier, etArea, etComplexArea
//{$ENDIF}
    ,etContainer, etProxy, etCADFill, etBrepEntity, etACIS, etBREP,
    etPathEvacuation, etXRef, etRay, etFlat, etEntity3D, etXline);

  TsgCADEntityTypes = set of TsgCADEntityType;

  TsgBrepEntities = (seBrepEntity, sBrepEntityTopology, seBrepEntityGeometry,
    seConeSurface, sePlaneSurface, seSphereSurface, seTorusSurface, seExactSplineSurface,
    seEllipsCurve, seExactCurve, seBs3Curve, seStraightCurve, seHelixCurve);

  // Style of the CAD-drawing space
  TsgCADSpace = (cs2D, cs3D, csUndefined);

  PsgColor = ^TsgColor;
  TsgColor = packed record
  case Integer of
    0: (R, G, B, A: Byte);
    1: (V: array [0..3] of Byte);
    2: (Color: DWORD);
  end;

  PsgParametrs = ^TsgParametrs;
  TsgParametrs = record
  case Integer of
    0: (Param1, Param2, Param3, Param4: Byte);
    1: (Params: array [0..3] of Byte);
    2: (FirstTwoParams, LastTwoParams: Word);
    3: (Parametrs: Integer);
  end;

  TsgActiveColor = (acIndexColor, acRGBColor);
  PsgColorCAD = ^TsgColorCAD;
  TsgColorCAD = packed record
    Active: TsgActiveColor;
    Color: Cardinal;
    AlbumString: string;
  end;

  PsgEntColors = ^TsgEntColors;
  TsgEntColors = record
    DrawColor: TColor;
    EntColor: TColor;
    EntColorCAD: TsgColorCAD;
  end;

  TsgArrows = record
    case Integer of
      0: (Blk, Blk1, Blk2, LrBlk: TsgDimArrowType);
      1: (Blks: array[vnDIMBLK .. vnDIMLRBLK] of TsgDimArrowType);
  end;

  PsgDimStyle = ^TsgDimStyle;
  TsgDimStyle = record
    Alt: Boolean;
    AltF: Double;
    APost: string;
    Asz: Double;
    Sah: Boolean;
    Arrows: TsgArrows;
    Cen: Double;
    ClrD: TsgColorCAD;
    ClrE: TsgColorCAD;
    ClrT: TsgColorCAD;
    SD1: Boolean;
    SD2: Boolean;
    SE1: Boolean;
    SE2: Boolean;
    Dec: Integer;
    Exe: Double;
    Exo: Double;
    Gap: Double;
    LFac: Double;
    LwD: Double;
    LwE: Double;
    Post: string;
    Scale: Double;
    Tad: Integer;
    Tih: Boolean;
    Tix: Integer;
    Toh: Boolean;
    Txt: Double;
    Tp: Double;
    Tm: Double;
    LUnit: TsgDimLimitUnits;
    DSep: Char;
    Frac: Integer;
  end;

  {$IFDEF SG_CPUX64} //for protection purposes
  TsgPrNativeUInt = NativeUInt;
  {$ELSE}
  TsgPrNativeUInt = Cardinal;
  {$ENDIF}

  // DWG, DXF distance units mode
  TsgDistUnitsMode = (umDrawing, umUser, umAltUser);
  // DWG format version
  TsgDWGVersion = (acR09, acR10, acR11, acR12, acR13, acR14, acR2000, acR2004,
    acR2007, acR2010, acR2013, acR2018);
  TsgDWGVersions = set of TsgDWGVersion;

  TsgDWGAppInfo = record
    InfoName: string;
    ID: string;
    Version: string;
    Comment: string;
    ProductXMLElement: string;
  end;

  TsgStandardLayerName = (slrnEmpty, slrnZero, slrnDefpoints, slrnHasAsterisk, slrnAny);
  TsgStandardLineTypeName = (sltnEmpty, sltnByBlock, sltnByLayer, sltnContinious, sltnAny);

  TsgColorSpace = (clspRGB, clspCMYK);

const
  DWGVersionHigh = High(TsgDWGVersion);

{$IFNDEF SGDEL_6}
const
  ctNode = 0;
  ctInteger = 1;
  ctDouble = 2;
  ctString = 4;
  ctHex = 8;
type
  TsgCodeType = Byte;
{$ELSE}
type
  TsgCodeType = (ctNode, ctInteger, ctDouble, ctString=4, ctHex=8);
{$ENDIF}
  PCodeTypes = ^TCodeTypes;
  TCodeTypes = array[-5 .. 1071] of TsgCodeType;

const
  cnstAsterisk = '*';
  cnstAutocadName = 'AutoCAD';
  cnstAutocadValue = 'AC';
  cnstSVGBlockNameBase = '*IB';
  cnstSVGBlockNameBaseLength = Length(cnstSVGBlockNameBase);
  cnstSVGBImagelockNameBase = '*IBP';

  cnstDWGVersionStr: array[TsgDWGVersion] of record
    Name: string;
    Value: string;
  end =
   ((Name: 'R09';  Value: '1004'),
    (Name: 'R10';  Value: '1006'),
    (Name: 'R11';  Value: '1007'),//!!! not sure
    (Name: 'R12';  Value: '1009'),
    (Name: 'R13';  Value: '1013'),
    (Name: 'R14';  Value: '1014'),
    (Name: '2000'; Value: '1015'),
    (Name: '2004'; Value: '1018'),
    (Name: '2007'; Value: '1021'),
    (Name: '2010'; Value: '1024'),
    (Name: '2013'; Value: '1027'),
    (Name: '2018'; Value: '1032'));

  cnstDataId: array[TsgDataId] of string = ('', 'Classifiers', 'Settings',
    'MapEntities', 'InspectorGroups', 'LockPaint', 'SwitchImage', 'Patterns');

  osAll = [osInsert, osEndPt, osCenter, osNode, osNearest, osMiddle,
    osIntersection];

  cnstAcDbDynamicBlockTrueName = 'AcDbDynamicBlockTrueName';

type

  TsvgEntity = (svgUndefined, svgContainer, svgGroup, svgGroupDim, svgEntity, svgPoint, svgCircle,
    svgEllipse, svgLine, svgLink, svgPolyline, svgRect, svgPath, svgText, svgTextPath,
    svgUse, svgImage);

  TsgEntityTypeEx = record
    case Byte of
      0: (Val: Byte);
      1: (CAD: TsgCADEntities);
      2: (BTI: TbtiEntity);
      3: (SVG: TsvgEntity);
  end;

  TsgGroupType = (gtCAD, gtBTI, gtSVG);

  TsgEntClass = record
    case Byte of
      0: (ID: Word);
      1: (EG: TsgGroupType; ET: TsgEntityTypeEx);
  end;

  PsgFontParams = ^TsgFontParams;
  TsgFontParams = packed record
    Above: Double;
    Below: Double;
    Width: Double;
    Height: Double;
    Left: Double;
    Bottom: Double;
    Right: Double;
    Top: Double;
    After: Double;
    DY: Double;
    DX: Double;
  end;

  TsgFormResult = (frCancel, frOk, frAdd, frClose, frDelete, frSelectEntities,
    frSelectPoint, frSelectLength, frSelectAngle, frSelectRect, frYes, frNo,
    frIgnore, frDeleteAll, frMerge);

  TsgGradientType = (gtNone, gtCurved, gtCylinder, gtHemiSpherical, gtLinear,
    gtSpherical, gtInvCurved, gtInvCylinder, gtInvHemiSpherical, gtInvSpherical,
    gtOther);
  // Numbers are necessary for compatibility with a TBrushStyle
  TsgHatchStyle = (hsSolid, hsPatternData, hsHorizontal, hsVertical,
    hsFDiagonal, hsBDiagonal, hsCross, hsDiagCross);

  TsgFillStyle = (fsSolid, fsClear, fsHorizontal, fsVertical,
    fsFDiagonal, fsBDiagonal, fsCross, fsDiagCross, fsCAD);

  TsgAttributeMode = (attDisable, attNormal, attEnable);

  TsgHatchSettting = (hsNoChange, hsConvertToSolid, hsDrawAsSolid, hsParserOnDraw);

  PsgHashItem = ^TsgHashItem;
  TsgHashItem = record
    HashCode:  Uint64;
    Data: Pointer;
  end;

  PsgHashItemObject = ^TsgHashItemObject;
  TsgHashItemObject = record
    HashCode:  Uint64;
    Data: TObject;
  end;

  TsgOjectHandleItem = class
    Handle: UInt64;
    Obj: TObject;
  end;

  PsgHashLnk = ^TsgHashLnk;
  TsgHashLnk = record
    Data: Pointer;
    Index: Integer;
  end;

  //--- Warning
  //--- if TsgHeadVarStruct was changed (added new fields etc)
  //--- function TsgDXFSectionHeader.ToXMLNode and TsgDXFSectionHeader.FromXMLNode
  //--- should be changed also!
  PsgHeadVarStruct = ^TsgHeadVarStruct;
  TsgHeadVarStruct = record
    AnalogyDistance: Double;
    CEColor: TsgColorCAD;
    CLayer: string;
    CELType: string;
    CELTScale: Double;
    CELWeight: Double;
    CodePage: Integer;
    DimStyle: string;
    DimTextStyle: string;
    DimProps: TsgDimStyle;
    ExtMin: TFPoint;
    ExtMax: TFPoint;
    PExtMin: TFPoint;
    PExtMax: TFPoint;
    LimMin: TFPoint;
    LimMax: TFPoint;
    PLimMin: TFPoint;
    PLimMax: TFPoint;
    TextStyle: string;
    FilletRadius: Double;
    InsUnits: Integer;
    LTScale: Double;
    Measurement: Boolean;
    PointDisplayMode: Integer;
    PointDisplaySize: Double;
    TextSize: Double;
    TileMode: Integer;
    UCSORG: TFPoint;
    UCSXDir: TFPoint;
    UCSYDir: TFPoint;
    Version: Byte;
    InsBase: TFPoint;
    AttMode: TsgAttributeMode;
    XClipFrame: Byte;
    DimAssoc: Byte;
    FillMode: Boolean; // Specifies whether hatches and fills, 2D solids, and wide polylines are filled in, default True
    LwDisplay: Byte;//0 = Lineweight is not displayed 1 = Lineweight is displayed
  end;

  TsgCADVariables = record
    HPGapTol: Double;
  end;

  PsgHTTPProxyInfo = ^TsgHTTPProxyInfo;
  TsgHTTPProxyInfo = record
    UseProxy: Boolean;
    ProxyHost: string;
    ProxyPort: Integer;
    ProxyNeedPass: Boolean;
    ProxyUser: string;
    ProxyPass: string;
  end;

{$IFDEF SGDEL_XE2}
  TsgNativeInt = NativeInt;
  TsgNativeUInt = NativeUInt;
{$ELSE}
{$IFNDEF SGFPC}
  TsgNativeInt = Integer;
  TsgNativeUInt = Cardinal;
{$ELSE}
  TsgNativeInt = NativeInt;
  TsgNativeUInt = NativeUInt;
{$ENDIF}
{$ENDIF}
  TsgNativePointer = TsgNativeInt;

  PsgNativeInt = ^TsgNativeInt;
  PsgNativeUInt = ^TsgNativeUInt;
{$IFNDEF SGFPC}
{$IFNDEF SGDEL_XE2}
  IntPtr = TsgNativeInt;
  UIntPtr = TsgNativeUInt;
  PIntPtr = ^IntPtr;
  PUIntPtr = ^UIntPtr;
{$ENDIF}
{$ENDIF}

  PsgSubProxyProps = ^TsgSubProxyProps;
  TsgSubProxyProps = packed record
    LayerIndex: Integer;// = -1;
    LTypeIndex: Integer;// = -1;
    ColorID: Integer;   // = -1;
    LWeight: Integer;
    Marker: Integer;
    LTScale: Double;
    ProxyType: Integer;
    PlotStyleIndex: Integer;
  end;

  PsgInt64 = ^TsgInt64;
  TsgInt64 = record
    Lo: TsgNativeInt;
    Hi: Integer;
  end;

  PsgArc = ^TsgArc;
  TsgArc = record
    Rect: TFRect;
    Center, Point1, Point2: TFPoint;
    Ang1, Ang2, Rx, Ry: Double;
    Valid: Boolean;
  end;

  TsgExpTextParam = record
    Height: Double;
    HKoef: Double;
    Scale: Double;
    Draw: TFMatrix;
  end;
  PsgExpTextParam = ^TsgExpTextParam;

  TsgExpAnnotParam = packed record
    Size: Word;
    Subtype: TsgAnnotType;
    Open: Boolean;
    Flags: Cardinal;
    Title: PAnsiChar;
    Rect: TRect;
  end;

  IProxyReader = interface
    function ReadDouble: Double;
    function ReadInteger: Integer;
    function ReadSmallInt: SmallInt;
    function ReadWord: Word;
    function ReadByte: Byte;
    procedure PushPosition;
    procedure PopPosition;
    procedure Seek(ASize: Integer);
    function ReadAnsiPadding: AnsiString;
    function ReadWidePadding: WideString;
    function ReadString: string;
    function ReadHandle: UInt64;
    procedure ReadBytes(var Dest; const ACount: UInt64);
    function Position: Pointer;
    function PositionAsInt64: TsgInt64;
    function GetVersion: Integer;
    procedure UpdatePosition(ANewPos: Pointer);
  end;

  TCustomProxyObjReader = class
    function ReadObject(ProxyReader: IProxyReader; obj: TObject): Boolean; virtual; abstract;
  end;

  IsgGlobalPropProvider = interface
    ['{65A98306-5E0D-4B1D-840A-21FB9FD1291C}']
    procedure Clear;
    function TryGetValue(Name: PChar; out Value: Variant): Boolean;
    function GetItem(Name: PChar): Variant;
    function RemoveItem(Name: PChar): Integer;
    procedure SetItem(Name: PChar; const Value: Variant);
    property Item[Name: PChar]: Variant read GetItem write SetItem; default;
  end;

  TsgGetGlobalPropProvider = function(AInstance: TObject; AOwnsProps: Boolean;
    out AGlobalPropProvider: IsgGlobalPropProvider): Boolean;

{$IFNDEF SGDEL_7}
type
  UInt8 = Byte;
  UInt16 = Word;

  PExtended80Rec = ^TExtended80Rec;
  TExtended80Rec = packed record
    case Integer of
    0: (Words: array [0..4] of UInt16);
    1: (Bytes: array[0..9] of UInt8);
    2: (Frac: TsgNativeUInt; Exp: Uint16;);
  end;

  PExtendedRec = PExtended80Rec;
  TExtendedRec = TExtended80Rec;
{$ENDIF}

  PsgImageProperties = ^TsgImageProperties;
  TsgImageProperties = packed record
    DPI: TF2DPoint;
    Size: TPoint;
  end;

  TsgImportMode = (imView, imImport);

  TsgInsUnits = (iuUnitless, iuInches, iuFeet, iuMiles, iuMillimeters,
    iuCentimeters, iuMeters, iuKilometers, iuMicroinches, iuMils, iuYards,
    iuAngstroms, iuNanometers, iuMicrons, iuDecimeters, iuDecameters,
    iuHectometers, iuGigameters, iuAstronomical, iuLightyears, iuParsecs,
    //supported in versions prior to AutoCAD 2017
    iuUsSurveyFeet{$IFDEF SG_iuPixel}, iuPixel{$ENDIF});

  TsgUnitsProps = record
    DisplayedUnits: TsgInsUnits;
    InitialUnits: TsgInsUnits;
    ScaleFactor: Double;
    PrecisionFactor: Integer;
  end;

//GCode
  TsgGcodeExts = (geNc, geNgc, geGcode, geNcp);
  TsgGLeadType = (gltNone, gltTangent, gltNormal, gltArc);
  TsgGRadiusComp = (grcNone, grcLeft, grcRight);
  TsgContourShape = (cshFree, cshLinear, cshCircular, cshPoint, cshPocket);

  TsgGToolParams = record
    Number: Integer;
    Radius: Double;
    Length: Double;
  end;
//
  PsgPoint = ^TPoint;

  TsgDWGSentinel = array[0..15] of Byte;
  PsgDWGHandle = ^TsgDWGHandle;
  TsgDWGHandle = record
    Handle: UInt64;
    Code: Byte;
  end;
  TsgDWGHandleArray = array of TsgDWGHandle;

  PdwgLocator = ^TdwgLocator;
  TdwgLocator = packed record
    RecNumber: Byte;
    Seeker: Integer;
    Size: Integer;
  end;

  PdwgFileHeader = ^TdwgFileHeader;
  TdwgFileHeader = packed record
    VersionID: array[0..5] of AnsiChar;
    Pad: array[0..6] of AnsiChar;
    ImageSeeker: Integer;
    Unknown: Word;
    CodePage: Word;
    Locators: Integer;
  end;

  TsgInternalTextFlag = (itfWinFont, itfInsideMText, itfUseDefaultFont,
    itfSetFontName, itfInsideDimension);
  TsgInternalTextFlags = set of TsgInternalTextFlag;


  PsgDWG2004NonCryptedFileHeader = ^TsgDWG2004NonCryptedFileHeader;
  TsgDWG2004NonCryptedFileHeader = packed record
    VersionID: array[0 .. 5] of AnsiChar;
    Padding1: array[0 .. 4] of AnsiChar;
    ReleaseVersion: Byte;
    UnknownByte1: Byte; // 0, 1, or 3 (for DWG 2004 write 3)
    PreviewOffset: Cardinal; //points to the image page + page header size (0x20).
    OrigFileSavedVer: Byte;// Acad version that writes the file
    OrigFileSavedReleaseVer: Byte;// Acad maintenance version that writes the file
    Codepage: Word;
    UnknownByte2: Byte; // 0
    OrigFileSavedVerRepeat: Byte;// The same as ApplicationDwgVersion
    OrigFileSavedReleaseVerRepeat: Byte;// Acad maintenance version that writes the file

    SecurityFlags: Cardinal;//Security flags, default value is 0 (long)
                            //0x0001 = encrypt data (used for all data sections except AcDb:Preview and AcDb:SummaryInfo)
                            //0x0002 = encrypt properties (used for sections AcDb:Preview and AcDb:SummaryInfo)
                            //0x0010 = sign data
                            //0x0020 = add timestamp
    UnknownLong: Cardinal; // 0
    SummaryInfoAddress: Cardinal; // points to summary info page + page header size (0x20)
    VBAProjectAddress: Cardinal;  // Optional, write 0 if not present
    Padding3: Cardinal;           // 0x00000080
    Padding4: array[0 .. 83] of Byte; // 0
  end;

  PsgDWG2004EncryptedFileHeader = ^TsgDWG2004EncryptedFileHeader;
  TsgDWG2004EncryptedFileHeader = packed record
  case Byte of
    0: (RawData: array[0..107] of Byte);
    1: (ACADVer: array[0..11] of AnsiChar;
        Field1: Cardinal; // $00
        Field2: Cardinal; // $6C;
        Field3: Cardinal; // $04;
        RootTreeNodeGap: Integer;
        LowermostLeftTreeNodeGap: Integer;
        LowermostRightTreeNodeGap: Integer;
        Field4: Cardinal; // $1
        LastSectionPageId: Cardinal;
        LastSectionPageEndAddress: UInt64;
        SecondHeaderAddress: UInt64; //pointing to the repeated header data at the end of the file
        GapAmount: Cardinal;
        PagesAmount: Cardinal;
        Field5: Cardinal; // $20;
        Field6: Cardinal; // $80;
        Field7: Cardinal; // $40;
        SectionPageMapId: Cardinal;
        SectionPageMapAddress: UInt64;
        SectionMapId: Cardinal;
        PagesMaxId: Cardinal;
        GapArraySize: Cardinal;
        CRC32: Cardinal); //Initial value 0
  end;

  PsgDWG2004SysPageHeader = ^TsgDWG2004SysPageHeader;
  TsgDWG2004SysPageHeader = packed record
  case Byte of
    0: (RawData: array[0..19] of Byte);
    1: (SectionType: Cardinal; //must be equal cntDWG2004SectionPageMap or cntDWG2004SectionMap
        DecompDataSize: Cardinal;
        CompDataSize: Cardinal;
        CompressionType: Cardinal; // $02
        Checkum: Cardinal);
  end;

  PsgDWG2004DataPageHeader = ^TsgDWG2004DataPageHeader;
  TsgDWG2004DataPageHeader = packed record
  case Byte of
    0: (RawData: array[0..31] of Byte);
		1: (SectionType: Cardinal;//	$4163043b	cntDWG2004SectionData
		    SectionId: Cardinal;
		    DataSize: Cardinal;
		    PageSize: Cardinal;
		    StartOffset: Int64;
		    Checksum1: Cardinal;
		    Checksum2: Cardinal);
  end;

  PsgDWGR18SectionMapInfo = ^TsgDWGR18SectionMapInfo;
  TsgDWGR18SectionMapInfo = packed record
    SectionsCount: Cardinal;
    Fileld1, Fileld2, Fileld3: Cardinal;
    Unknown: Cardinal;
  end;

  PsgDWGR18SysPageItem = ^TsgDWGR18SysPageItem;
  TsgDWGR18SysPageItem = record
    Number, Size: Cardinal;
  end;

  PsgDWGR18StreamPage = ^TsgDWGR18StreamPage;
  TsgDWGR18StreamPage = packed record
    PageId: Cardinal;
    ComprSize: Cardinal;
    DataOffset: UInt64;
  end;

  PsgDWGR18Section = ^TsgDWGR18Section;
  TsgDWGR18Section = packed record
    DataSize: UInt64;
    PagesAmount: Cardinal;
    PageSize: Cardinal;
    Unknown: Cardinal;
    Compressed: Cardinal;
    SectID: Cardinal;
    Encrypted: Cardinal;
    SecName: array[0 .. 63] of AnsiChar;
    Pages: array[0 .. 0] of TsgDWGR18StreamPage;
  end;

  { Begin DWG 2010 support }

  TsgDWG2010FileHeader = packed record
    GpsRtId: Integer;
    GpsLeftId: Integer;
    GpsRightId: Integer;
    GpsUnknown: Integer;
    LastPageId: Integer;
    LastPageEndOffset: UInt64;
    Hdr2Offset: UInt64;
    GapsAmount: Cardinal;
    PagesAmount: Cardinal;
    PagesMapId: Cardinal;
    PagesMapOffset: UInt64;
    SectionsMapId: Cardinal;
    PagesMaxId: Cardinal;
    GapsMaxId: Cardinal;
  end;

  PsgDWG2010SysPageHeader = ^TsgDWG2010SysPageHeader;
  TsgDWG2010SysPageHeader = record
    PageType: Cardinal;
    DecompSize: Cardinal;
    CompSize: Cardinal;
    CompType: Cardinal;
    CRC: Cardinal;
  end;

  PsgDWG2010StreamPage = PsgDWGR18StreamPage;
  TsgDWG2010StreamPage = TsgDWGR18StreamPage;

  { HP RTL }

  PsgPCLBitmap = ^TsgPCLBitmap;
  TsgPCLBitmap = packed record
    ColorSpace: Byte;
    Encode: Byte;
    BitIndex: Byte;
    BitRed: Byte;
    BitGrn: Byte;
    BitBlue: Byte;
    WhiteRed: Word;
    WhiteGreen: Word;
    WhiteBlue: Word;
    BlackRed: Word;
    BlackGreen: Word;
    BlackBlue: Word;
  end;

  TsgWatermarkPosition = (wpCenter, wpTopLeft, wpBottomLeft, wpTopRight, wpBottomRight);
  PsgWatermarkParams = ^TsgWatermarkParams;
  TsgWatermarkParams = packed record
    Text: string;
    Angle: Double;
    FontName: TFontName;
    Color: TColor;
    Scale: Double;
    Position: TsgWatermarkPosition;
  end;

const
  cnstAddressBit = 3;
{$IFDEF SG_CPUX64}
  cnstAddressHi: WORD = (SizeOf(NativeUInt) shl cnstAddressBit) - cnstAddressBit;
{$ELSE}
  cnstAddressHi: WORD = (SizeOf(Integer) shl cnstAddressBit) - cnstAddressBit;
{$ENDIF}
  cnstProxyTypeLWPLine = 33;

  DefWatermarkPapams: TsgWatermarkParams =(
    Text: 'Watermark';
    Angle: 45;
    FontName: 'Arial';
    Color: $FF0000;
    Scale: 1;
    Position: wpCenter;
);

//Gcode
  cnstGcodeExts: array [TsgGcodeExts] of string = ('.nc', '.ngc', '.gcode', '.ncp');
  cnstGToolsSeparator = '@';
  cnstGParamsSeparator = '/';
  cnstRCNumber = 0;
  cnstRCDiameter = 1;
  cnstRCLength = 2;
  cnstGDrawFirstPt: Boolean = True;
  cnstGDrawNodePt: Boolean = True;
  cnstGDrawArrow: Boolean = True;
  cnstGDrawLeadIn: Boolean = True;
  cnstGDrawLeadOut: Boolean = True;
  cnstGDrawTool: Boolean = True;
  cnstDrawEqRadiusCompGRBL: Boolean = True;
  cnstDefGcodeColorLegend = '65280, 12632256, 255, 32768, 16711935, 16738049, 95231';
//Gcode CNC Colors
  cnstGCommentColor = 12489328;
  cnstGServiceColor = 12489328;
  cnstGCoordXColor = 0;
  cnstGCoordYColor = 0;
  cnstGCoordZColor = 31476;
  cnstGCoordIColor  = 0;
  cnstGCoordJColor = 0;
  cnstGFeedPowerColor = 170;
  cnstGRadiusCompColor = 38400;
  cnstGToolDiamterColor = 13387839;
  cnstGMComColor = 8388736;
  cnstG0Color = 16748574;
  cnstG1Color = 16711808;
  cnstG2G3Color = 8388863;
  cnstDefCNCCodeColors = '12489328, 12489328, 0, 0, 31476, 0, 0, 13387839, 8388736, 38400, 170, 16748574, 16711808, 8388863';

  iGCommentColor1: TColor = cnstGCommentColor;
  iGServiceColor2: TColor = cnstGServiceColor;
  iGCoordXColor3: TColor = cnstGCoordXColor;
  iGCoordYColor4: TColor = cnstGCoordYColor;
  iGCoordZColor5: TColor = cnstGCoordZColor;
  iGCoordIColor6: TColor = cnstGCoordIColor;
  iGCoordJColor7: TColor = cnstGCoordJColor;
  iGFeedPowerColor8: TColor = cnstGFeedPowerColor;
  iGRadiusCompColor9: TColor = cnstGRadiusCompColor;
  iGToolDiamterColor10: TColor = cnstGToolDiamterColor;
  iGMComColor11: TColor = cnstGMComColor;
  iG0Color12: TColor = cnstG0Color;
  iG1Color13: TColor = cnstG1Color;
  iG2G3Color14: TColor = cnstG2G3Color;
//


type
  PsgLine = ^TsgLine;
  TsgLine = packed record
    Point1: TFPoint;
    Point2: TFPoint;
  end;

  PsgF2DLine = ^TsgF2DLine;
  TsgF2DLine = packed record
    Point1: TF2DPoint;
    Point2: TF2DPoint;
  end;

  PsgCurveLnk = ^TsgCurveLnk;
  TsgCurveLnk = record
    Flags: Byte;
    Link: TObject;
    P1: Pointer;
    P2: Pointer;
    case Integer of
      0: (Line: TsgLine);
      1: (Arc: TsgArcR);
  end;

  TsgMeshQuality = (mqLow, mqNormal, mqHigh);

  PsgLittleHeadVarStruct = ^TsgLittleHeadVarStruct;
  TsgLittleHeadVarStruct = packed record
//    InitFlags: LongWord;
    InitFlags: TsgDimNameVals;
    DimDec: Integer;
    DimLFac: Double;
    DimTxt: Double;
    DimTextAlign: Byte;
    DimClrT: TsgColorCAD;
    DimGap: Double;
  end;

  TsgParamType = (ptUndefined,ptPoint, ptDistance, ptAngle, ptKey, ptRect,
    ptAreaCalc{$IFNDEF SG_FM_MOBILE}, ptObject, ptObjectList{$ENDIF});

  TsgParamTypes = set of TsgParamType;

  TsgGridHeaderType = (ghUndefined, ghDescription, ghAreaValue, ghAreaType,
    ghAreaNumber, ghAttribute, ghLitter, ghRegisterNumber);

  PsgParamData = ^TsgParamData;
  TsgParamData = packed record
    PType: TsgParamType;
    case TsgParamType of
      ptUndefined: (DataUnd: Pointer);
      ptPoint:     (DataPnt: TFPoint);
      ptRect:      (DataRect: TFRect);
      ptDistance:  (DataDst: Double);
      ptAngle:     (DataAng: Double);
      ptKey:       (DataKey: Integer);
      ptAreaCalc:  (DataArea: Double);
      {$IFNDEF SG_FM_MOBILE}
      ptObject:    (DataObj: TObject);
//      ptList:      (DataLst: TList);
      {$ENDIF}
  end;

  TsgPointClassify = (pcLEFT, pcRIGHT, pcBEYOND, pcBEHIND, pcBETWEEN, pcORIGIN,
    pcDESTINATION, psUNDEFINED);
  TsgPointClasses = set of TsgPointClassify;
  TsgPointState = (psInside, psOutside, psBoundary);
  TsgEdgeState = (esTouching, esCrossing, esInessential);

  TsgPopMenuType = (pmtNone, pmtEnter, pmtCancel, pmtClose, pmtActPtMenu,
    pmtActPtLeftTop, pmtActPtLeftBottom, pmtActPtCenter, pmtActPtRightTop,
    pmtActPtRightBottom, pmtAreaMenu, pmtCreateAreaBySelect,
    pmtCreateAreaByPolyline, pmtCreateAreaByRect,
    pmtCopy, pmtPaste, pmtPasteAsBlock, pmtPasteOriginCoords, pmtCut,
    pmtDelete, pmtSelectAll, pmtUndo, pmtRedo, pmtZoomIn, pmtZoomOut,
    pmtFitToSize, pmtCreateCopy, pmtOffset, pmtDoubleOffset, pmtMove,
    pmtMirror, pmtRotate, pmtScale, pmtFindText, pmtProperties ,pmtOptions,
    pmtEditAttrib, pmtMatchSettings, pmtSendToBack, pmtBringToFront,
    pmtDrawArcs, pmtDrawLines, pmtAxisType, pmtAxisLeft, pmtAxisCenter,
    pmtAxisRight, pmtAreaProp, pmtClockOrCounterClockWise, pmtCW90, pmtCCW90,
    pmtRotate180, pmtCreateAreaByPoint, pmtFullScreen, pmtExitTemplateEditor,
    pmtExitBlockEditor, pmtBlockEditor, pmtCreateAreaByContour, pmtOLE,
    pmt3DViewSetting, pmtSmoothShading, pmtFlatShading, pmtHidenLines,
    pmtWireframe, pmt2DWireframe, pmtViewportActive, pmtViewportDeActive,
    pmtViewportZoomIn, pmtViewportZoomOut, pmtViewportFitToSize,
    pmtACADTableEdit, pmtFastReport, pmtEditHatch, pmtEditMText,
    pmtEditTextAttdef, pmtShowEntity, pmtUser, pmtMetafileToCad, pmtExplodeAll,
    pmtExplodeAllInserts, pmtCreatePolyline2D, pmtCreatePolyline3D,
    pmtWipeoutRect, pmtWipeoutPolyline, pmtWipeoutShow, pmtWipeoutHide,
    pmtViewportRect, pmtViewportPolyline, pmtViewportObject, pmtToCurves,
    pmtConvertToPolyline, pmtImplement, pmtRecreateBlockAsTemplate,
    pmtDrawFillet, pmtCopyWithBasePoint, pmtChangeDirection,
    pmtInsertMode, pmtIMBasepoint, pmtIMTopLeft, pmtIMTopRight, pmtIMCenter,
    pmtIMBottomLeft, pmtImBottomRight, pmtComplexLine, pmtComplexArc,
    pmtAddAxis, pmtRectOffset, pmtSelectContour,
    pmtEditContour, pmtAddVertex, pmtDelVertex, pmtEditVertex, pmtStartVertex,
    pmtEndVertex, pmtEditPolyline,//Edito contour

    pmtCreateAreaBySelectArea, pmtCreateAreaByCoords, pmtSizes,
    pmtSetCadastrNumber, pmtDelDuplicateVertex,
    pmtDelClipFromImage, pmtDelClipFromInsert, pmtPolyToCoordsPoints, pmtSelectOwner,
    pmtEditOwner);


  TsgPopMenuTypes = set of TsgPopMenuType;

  PsgPoints3 = ^TsgPoints3;
  TsgPoints3 = array [0..2] of TFPoint;
  PsgPoints4 = ^TsgPoints4;
  TsgPoints4 = array [0..3] of TFPoint;
  TsgPFPoints4 = array [0..3] of PFPoint;
  TsgIndexes4 = array[0..3] of Integer;
  TsgPoints5 = array [0..4] of TFPoint;

  PRectCoords = ^TsgRectCoords;
  TsgRectCoords = record
    Number: string;
    Point: TFPoint;
  end;

  TsgSizesOrientation = (soAuto, soHorizontal, soVertical);
  TsgSizeGenerate = record
    TypeText: TsgSizesOrientation;
    ColorText: TColor;
    TextHeight: Double;
    Offset: Double;
    Precision: Integer;
    Scale: Double;
  end;

  PsgPlotSettingsData = ^TsgPlotSettingsData;
  TsgPlotSettingsData = record
    PageSetupName: string;
    PrintOrConfigName: string;
    PlotLayoutFlags: TsgPlotLayoutFlags;
    PlotPaperUnits: TsgPlotPaperUnits;
    StandardScaleType: Byte;
    NumeratorOfCustomPrintScale: Double;
    DenominatorOfCustomPrintScale: Double;
    PaperSize: string;
    PlotViewName: string;
    UnprintableMargin: TF2DRect;
    PlotPaperSize: TF2DPoint;
    PlotOrigin: TF2DPoint;
    PlotWindowAreaMin: TF2DPoint;
    PlotWindowAreaMax: TF2DPoint;
    PlotRotation: TsgPlotRotation;
    PlotType: TsgPlotType;
    CurrentStyleSheet: string;
    ShadePlotMode: Integer;
    ShadePlotResolutionLevel: Integer;
    ShadePlotCustomDPI: Integer;
    FloatingPointScaleFactor: Double;
    PaperImageOrigin: TF2DPoint;
  end;

  TsgShowEntityMethod = (semProportional, semAbsolute, semFitToSize);

  TsgShowEntitySettings = record
    Method: TsgShowEntityMethod;
    Precision: Double;
  end;

  TsgPropertyType = (prtNone, prtArrowSize, prtArrowType, prtArrowType1,
    prtArrowType2, prtArrowTypeL, prtBackGroundImage, prtColor, prtDimExtLineExt,
    prtDimExtLineExo, prtDimMeasurementsScale, prtDimPrecision, prtDimScale,
    prtDimStyle, prtDimTextAlign, prtDimTextForce, prtDimTextSize,
    prtDimTextColor, prtDimTextOffset, prtDimTextPosVert, prtFilletRadius,
    prtLayer, prtLineType, prtLineTypeScale, prtLineWeight, prtPointDisplayMode,
    prtPointDisplaySize, prtTextFontName, prtTextSize, prtTextStyle,
    prtTextRotation, prtAnalogyDistance);

{$IFDEF SGDEL_2006}
  TsgRecUInt64 = record
  private
    FHi: Cardinal;
    FLo: Cardinal;
    function GetAll: Int64; inline;
    function GetHi: Cardinal; inline;
    function GetLo: Cardinal;
    procedure SetAll(const Value: Int64); inline;
    procedure SetHi(const Value: Cardinal); inline;
    procedure SetLo(const Value: Cardinal); inline;
  public
    property All: Int64 read GetAll write SetAll;
    property Hi: Cardinal read GetHi write SetHi;
    property Lo: Cardinal read GetLo write SetLo;
    class operator Add(const A,B: TsgRecUInt64): TsgRecUInt64;
    class operator BitwiseOr(const A,B: TsgRecUInt64): TsgRecUInt64;
    class operator Equal(const A,B: TsgRecUInt64): Boolean;
    class operator GreaterThan(const A,B: TsgRecUInt64): Boolean;
    class operator GreaterThanOrEqual(const A,B: TsgRecUInt64): Boolean;
    class operator Implicit(const A: TsgRecUInt64): Int64; inline;
    class operator Implicit(const A: TsgRecUInt64): Integer; inline;
    class operator Implicit(I: Int64): TsgRecUInt64; inline;
    class operator Implicit(I: Integer): TsgRecUInt64; inline;
    class operator LeftShift(const A: TsgRecUInt64; B: Integer): TsgRecUInt64;
    class operator LessThan(const A,B: TsgRecUInt64): Boolean;
    class operator LessThanOrEqual(const A,B: TsgRecUInt64): Boolean;
    class operator Negative(const A: TsgRecUInt64): TsgRecUInt64;
    class operator NotEqual(const A,B: TsgRecUInt64): Boolean;
    class operator RightShift(const A: TsgRecUInt64; B: Integer): TsgRecUInt64;
    class operator Subtract(const A,B: TsgRecUInt64): TsgRecUInt64;
  end;
{$ENDIF}

  TsgUnitsInsertTempl = (uitMilimetr, uitMetr, uitUndefined);

  TsgSVGPreserveAspectRatio = (altxMinyMin, altxMidyMin, altxMaxyMin,
    altxMinyMid, altxMidyMid, altxMaxyMid, altxMinyMax, altXmidyMax, altxMaxyMax,
    altMeet, altSlice, altNone);
  TsgSVGPreserveAspectRatios = set of TsgSVGPreserveAspectRatio;

  PsgViewBox = ^TsgViewBox;
  TsgViewBox = record
    Mode: TsgSVGPreserveAspectRatios;
    case Integer of
      0: (X, Y, Width, Height: TsgFloat);
      1: (Point, Size: TF2DPoint);
  end;

{$IFDEF SG_FIREMONKEY}
  PFMXSnapData = ^TFMXSnapData;
  TFMXSnapData = record
    SnapMode: TObjectSnapState;
    CADPoint: TFPoint;
  end;
  PPFMXSnapData = ^PFMXSnapData;

  TFMXSnapDataExt = record
    Point: TPoint3D;
    FMXEntity: TObject;
    Entity: TObject;
    Owner: TObject;
    Data: TFMXSnapData;
    Normal: TPoint3D;
  end;
{$ENDIF}

  PSnapData = ^TSnapData;
  TSnapData = record
    Entity: TObject;
    EntityIntersected: TObject;
    SnapMode: TObjectSnapState;
    SnapType: TObjectSnapMode;
    CADPoint: TFPoint;
    CADPointInUCS: TFPoint;
    Insert: TObject;
{$IFDEF SG_FIREMONKEY}
    CADNormal: TFPoint;
    FMXSnapData: TFMXSnapDataExt;
{$ENDIF}
  end;
  PPSnapData = ^PSnapData;

{$IFDEF SG_BTI}
  TsgFigureType = (ftFigure, ftQuadrangle, ftTrapezium, ftParallelogram, ftRect,
    ftSquare, ftRhomb, ftSegmentByChordAndLength, ftSegmentByChordAndHeight,
    ftTriangle, ftTriangleRightAngled, ftTriangleByHippuran, ftCircleByLength,
    ftCircleByRadius, ftSectorByAngleAndRadius, ftContour);
{$ENDIF}
  TsgOffsetGapType = (ogtExpansion, ogtFillet, ogtBevel,
    ogtExpansionOrBevel, ogtNone);

  //printer structs
  PPaperFormat = ^TPaperFormat;
  TPaperFormat = packed record
    Name: string;
    Index: Integer;
    PaperSize: TF2DPoint;
    IsXMLFormat: Boolean;
    Margins: TF2DRect;
    Units: TsgPlotPaperUnits;
  end;

  PsgOle2FrameHeaderData = ^TsgOle2FrameHeaderData;
  TsgOle2FrameHeaderData = packed record
    Unknown: Word;                // $5580 ($5581 ??)
    Point1: TFPoint;
    Point2: TFPoint;
    Point3: TFPoint;
    Point4: TFPoint;
    ExtentX: Word;                // 1 unit == 0.01mm
    ExtentY: Word;                // 1 unit == 0.01mm
    TileModeDescriptor: Integer;  // 0 - paper space; 1 - model space
    ReservedInt1: Integer;        // 256
    OLEObjectType: Integer;       // 1 = Link; 2 = Embedded; 3 = Static
    ReservedWord: Word;           // 1
    ReservedZero: Integer;        // 0
    DrawAspect: Integer;          // DVASPECT_CONTENT
    Size: Integer;                // size ole data
  end;

  TsgParamStrType = (ptArea, ptAreaOwner, ptNumber, ptNumberOwner,
    ptSubNumber, ptSubNumberOwner, ptLetNumber, ptLetNumberOwner,
    ptPerimeter, ptPerimeterOwner, ptValue, ptValueOwner, ptEmpty);

  TsgDataType = (dtUndefined,
    dtBool, dtByte, dtWord, dtInteger, dtInt64, dtColor, dtHandle,
    dtSingle, dtDouble, dtPointer, dtString, dtPoint, dtTF2DPoint, dtTFPoint,
    dtRect, dtTF2DRect, dtTFRect, dtVersion, dtColorCAD, dtTFMatrix,
//types by Description
    dtEntHandle, dtEntName, dtList, dtListItem, dtStringInternal, dtBase64,
    dtSingles, dtFPoints2DOr3D, dtProcedure, dtParameters, dtGuid, dtClass,
    dtVariant);
  TsgDataTypes = set of TsgDataType;

  TsgObjectWithOwner = class
  private
    FOwner: TObject;
  public
    constructor Create(const AOwner: TObject); virtual;
    destructor Destroy; override;
    property Owner: TObject read FOwner;
  end;

  TsgNotificationParams = class(TObject)
  end;

  TsgNotification = class(TsgObjectWithOwner)
  private
    FFlags: Word;
    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
  public
    procedure Proc(const Sender: TObject;
      const AParams: TsgNotificationParams); virtual; abstract;
    property Active: Boolean read GetActive write SetActive;
  end;

  TsgNotifications = class(TsgObjectWithOwner)
  private
    FList: TList;
  protected
    property List: TList read FList;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    function Add(const AObj: TsgNotification): Boolean;
    function Remove(const AObj: TsgNotification): Boolean;
    function RemoveAndFree(var AObj: TsgNotification): Boolean;
    procedure Proc(const AParams: TsgNotificationParams);
  end;

  TInterfacedPersistentClass = class of TInterfacedPersistent;
  TBitmapClass = class of TBitmap;

  TsgObjectWithField = class
  protected
    function GetDataType: TsgDataType; virtual; abstract;
  public
    class function CreateInt(const AValue: Integer): TsgObjectWithField;
    class function CreateInt64(const AValue: Int64): TsgObjectWithField;
    class function CreatePointer(const AValue: Pointer): TsgObjectWithField;
    class function CreateClass(const AValue: TClass): TsgObjectWithField;
    class function CreateVariant(const AValue: Variant): TsgObjectWithField;
    class function CreateColorCAD(const AValue: TsgColorCAD): TsgObjectWithField;
    class function CreateDouble(const AValue: Double): TsgObjectWithField;
    class function CreateString(const AValue: string): TsgObjectWithField;
    class function CreateFMatrix(const AValue: TFMatrix): TsgObjectWithField;
    class function CreateFPoint(const AValue: TFPoint): TsgObjectWithField;
    class function GetType(const AObjValue: TsgObjectWithField): TsgDataType;
    property DataType: TsgDataType read GetDataType;
  end;

  TsgObjectInt64 = class(TsgObjectWithField)
  protected
    function GetInt: Integer;
    procedure SetInt(const Value: Integer);
    function GetDataType: TsgDataType; override;
  public
    FieldInt64: Int64;
    constructor Create(const AValue: Int64 = 0);
    property FieldInt: Integer read GetInt write SetInt;
  end;

  TsgObjectTClass = class(TsgObjectWithField)
  protected
    function GetDataType: TsgDataType; override;
  public
    FieldClass: TClass;
    constructor Create(const AValue: TClass);
  end;

  TsgObjectPointer = class(TsgObjectWithField)
  protected
    function GetDataType: TsgDataType; override;
  public
    FieldPointer: Pointer;
    constructor Create(const AValue: Pointer = nil);
  end;

  TsgObjectVariant = class(TsgObjectWithField)
  protected
    function GetDataType: TsgDataType; override;
  public
    FieldVariant: Variant;
    constructor Create(const AValue: Variant);
  end;

  TsgObjectColorCAD = class(TsgObjectWithField)
  protected
    function GetDataType: TsgDataType; override;
  public
    FieldColor: TsgColorCAD;
    constructor Create(const AValue: TsgColorCAD); overload;
    constructor Create(const Active: TsgActiveColor; const Color: Cardinal;
      const Album: string = ''); overload;
    constructor Create(const AValue: TColor); overload;
  end;

  TsgObjectDouble = class(TsgObjectWithField)
  protected
    function GetDataType: TsgDataType; override;
  public
    FieldDouble: Double;
    constructor Create(const AValue: Double = 0);
  end;

  TsgObjectString = class(TsgObjectWithField)
  protected
    function GetDataType: TsgDataType; override;
  public
    FieldStr: string;
    constructor Create(const AValue: string = '');
  end;

  TsgObjectFMatrix = class(TsgObjectWithField)
  protected
    function GetDataType: TsgDataType; override;
  public
    FielFMatrix: TFMatrix;
    constructor Create(const AValue: TFMatrix);
  end;

  TsgObjectGUID = class(TsgObjectWithField)
  protected
    function GetDataType: TsgDataType; override;
  public
    FieldGUID: TGUID;
    constructor Create(const AValue: TGUID);
  end;

  TsgObjectFPoint = class(TsgObjectWithField)
  private
    function Get2DPoint: TF2DPoint;
    function GetPoint2D: TFPoint;
    procedure Set2DPoint(const AValue: TF2DPoint);
    procedure SetPoint2D(const AValue: TFPoint);
  protected
    function GetDataType: TsgDataType; override;
  public
    Point: TFPoint;
    constructor Create(const AValue: TFPoint);
    property FieldFPoint: TFPoint read Point write Point;
    property FieldFPoint2D: TFPoint read GetPoint2D write SetPoint2D;
    property FieldF2DPoint: TF2DPoint read Get2DPoint write Set2DPoint;
  end;

{$IFNDEF SGDEL_5}
  TListNotification = (lnAdded, lnExtracted, lnDeleted);

  TsgNotificationList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
  public
    function Add(Item: Pointer): Integer;
    procedure Delete(Index: Integer);
    function Extract(Item: Pointer): Pointer;
    procedure Insert(Index: Integer; Item: Pointer);
  end;
{$ELSE}
  TsgNotificationList = class(TList);
{$ENDIF}

{$IFNDEF SGDEL_6}
  IInterface = IUnknown;

  IStreamPersist = interface
    ['{B8CD12A3-267A-11D4-83DA-00C04F60B2DD}']
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  { TInterfaced Persistent }

  TInterfacedPersistent = class(TPersistent, IInterface)
  private
    FOwnerInterface: IInterface;
  protected
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure AfterConstruction; override;
  end;
{$ENDIF}

{$IFDEF SGFPC}
{.$DEFINE SG_HAS_STRINGBUILDER}
{$ELSE}
{$IFDEF SGDEL_2009}
{$DEFINE SG_HAS_STRINGBUILDER}
{$ENDIF}
{$ENDIF}

{$IFNDEF SG_HAS_STRINGBUILDER}
  TStringBuilder = class
  private
    function GetChars(index: Integer): WideChar;
    procedure SetChars(index: Integer; const Value: WideChar);
  protected
    FData: array of WideChar;
    FLength: Integer;
  published
  public
    constructor Create; overload;
    constructor Create(const Value: string); overload;
    constructor Create(const Value: WideString); overload;
{$IFNDEF SGDEL_7}
    class function CreateFromWideString(const Value: WideString): TStringBuilder;
    class function CreateFromAnsiString(const Value: AnsiString): TStringBuilder;
{$ENDIF}
    function ToString: sgUnicodeStr; overload; virtual;
    function ToString(StartIndex: Integer; StrLength: Integer): sgUnicodeStr; overload;
    property Length: Integer read FLength;
    property Chars[index: Integer]: WideChar read GetChars write SetChars; default;
  end;
{$ENDIF}

  TsgProcOfPointerGetPoint = function(const AObject: Pointer): TFPoint;
  TsgObjProcCompare = function(const A, B: Pointer): Integer of object;
  TsgObjProcNearestCompare = function(const A, B: Pointer): Double of object;
  TsgObjProcGetFPoint = function(const APointer: Pointer): TFPoint of object;
  TsgGetBox = function(Sender: TObject; var ABox: TFRect): Boolean of object;
  TsgMethodCompare = function(const A, B: TMethod): Integer of object;
  TsgNotifyEventCompare = function(const A, B: TNotifyEvent): Integer of object;

  TAddPt = function(const P): Integer of object;
  TNewCollection = function: TAddPt of object;

  TsgPointsProc = procedure(Points: PPoint; Count: Integer) of object;
  TsgPolyPointsProc = procedure(const Points; Counts: PInteger; Count: Integer) of object;

const
  cnstGUID_Change = '{3322123D-F8FA-4170-9040-A25C271B1887}';
  cnstGUID_CollectionBase = '{17A4E39A-668D-405F-9D2B-56019AC3CA84}';
  cnstGUID_CollectionBaseSort = '{6D1E57A5-542F-4C68-BB84-EFAE2E832F42}';
  cnstGUID_ArrayFPoints = '{F3E4590D-2987-434A-9976-BF94DC24D88F}';
  cnstGUID_ListOfEntities = '{56C91F32-4B45-4217-B790-7D97F89DEBDD}';
  cnstGUID_ArrayVertexes = '{E25320D6-D3A4-4078-A8B0-3B3409C9CF76}';
  cnstGUID_InputRectCoords = '{085B34D7-1ED9-400B-BFE3-74712FB53980}';

  cnstFailCreateGuid: TGUID = '{346ED9BE-02E0-4FC2-9831-1E07719E7796}';
  cntsErrorGuid: TGUID = '{CEBFE042-F13D-40B3-92E6-9BA3A76E8C99}';
  cnstFailNewDrawing: TGUID = '{F42F4557-D19D-433D-884B-79465EFCE73B}';
  GUID_ArrayFPoints: TGUID = '{00000000-0000-0000-0000-000000000000}';

  cnstGUID_DelayedDestroyObject = '{FD60B4E9-AB96-4C9E-943D-3A07B31CEC01}';
  cnstGUID_MainApplication =  '{334A4DDC-5237-4BD2-B236-975946B4ADF8}';
  cnstGUID_XMLIde = '{F16043B8-3469-41C2-AD53-C5E86D6CEA57}';
  cnstGUID_ResultNode = '{E52909E5-AA0D-4DD6-B36D-14B9CB335233}';
  cnstGUID_XMLOut = '{F1D6F762-FAC3-4958-93C6-51BFE1E850BD}';
  cnstGUID_PluginItem =  '{B0ECBC56-AD55-4984-BEEF-335981B142E3}';
  cnstGUID_XMLNode = '{C38E1C7C-A045-4CB8-850F-A7456BE9422D}';
  cnstGUID_Progress = '{54B436C1-C61F-4067-B802-845013588EA3}';
  cnstGUID_XMLObject = '{C3FB8046-CAE6-4A95-A784-596BCADB1778}';
  cnstGUID_XMLSubObjects = '{CD04C1D3-32B4-4421-817F-5795E02100BD}';
  cnstGuid_XMLSupportModeller = '{AD663D61-1DC3-4B1E-866C-0A10D99535D5}';
  cnstGuid_XMLSupportBuildKey = '{2BC9CE6D-CD2C-40B2-9547-B970C27D1B72}';
  cnstGuid_VisibleObject = '{29EDCEFD-890E-42F4-9D90-BAEBAE98B115}';
  cnstGuid_VisibleObjectByPath = '{80BAA757-F276-4DEE-9865-71F1A3CB29EF}';

  cnstGUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

type
{$IFNDEF HAS_OBJECT_CAST_FROM_INTERFACE}
  IInstancePersist = interface
    ['{C5F68180-A583-4830-816F-1FC82985ADBD}']
    function GetInstance: TObject;
  end;
{$ENDIF}

  IsgDelayedDestroyObject = interface(IInterface)
    [cnstGUID_DelayedDestroyObject]
    procedure FinalizeLocalReferences;
  end;

  IsgArrayFPoint = interface(IInterface)
    [cnstGUID_ArrayFPoints]
    function GetFPoint(const AIndex: Integer): TFPoint;
    function GetFPointCount: Integer;
    property FPointCount: Integer read GetFPointCount;
    property FPoints[const AIndex: Integer]: TFPoint read GetFPoint;
  end;

  IsgArrayVertexes = interface(IsgArrayFPoint)
    [cnstGUID_ArrayVertexes]
    function GetBulge(const AIndex: Integer): Double;
    property Bulges[const AIndex: Integer]: Double read GetBulge;
  end;

  IsgCollectionBase = interface(IInterface)
    [cnstGUID_CollectionBase]
    function GetCount: Integer;
    procedure Delete(const AIndex: Integer);
    property Count: Integer read GetCount;
  end;

   IsgCollectionBaseSort = interface(IsgCollectionBase)
     [cnstGUID_CollectionBaseSort]
     function GetDuplicates: TDuplicates;
     function GetSorted: Boolean;
     procedure SetDuplicates(const AValue: TDuplicates);
     procedure SetSorted(const AValue: Boolean);
     procedure SetProcCompare(const AValue: TsgObjProcCompare);
     property Sorted: Boolean read GetSorted write SetSorted;
     property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
   end;

  IsgCollectionInt = interface(IsgCollectionBaseSort)
    function GetItem(const AIndex: Integer): Integer;
    procedure SetItem(const AIndex: Integer; const AValue: Integer);
    property Items[const AIndex: Integer]: Integer read GetItem
      write SetItem; default;
  end;

  IsgCollectionInt64 = interface(IsgCollectionBaseSort)
    function GetItem(const AIndex: Integer): Int64;
    procedure SetItem(const AIndex: Integer; const AValue: Int64);
    property Items[const AIndex: Integer]: Int64 read GetItem
      write SetItem; default;
  end;

  IsgCollectionSingle = interface(IsgCollectionBaseSort)
    function GetItem(const AIndex: Integer): Single;
    procedure SetItem(const AIndex: Integer; const AValue: Single);
    property Items[const AIndex: Integer]: Single read GetItem
      write SetItem; default;
  end;

  IsgCollectionDouble = interface(IsgCollectionBaseSort)
    function GetItem(const AIndex: Integer): Double;
    procedure SetItem(const AIndex: Integer; const AValue: Double);
    property Items[const AIndex: Integer]: Double read GetItem
      write SetItem; default;
  end;

  IsgCollectionFloat = interface(IsgCollectionBaseSort)
    function GetItem(const AIndex: Integer): TsgFloat;
    procedure SetItem(const AIndex: Integer; const AValue: TsgFloat);
    property Items[const AIndex: Integer]: TsgFloat read GetItem
      write SetItem; default;
  end;

  IsgCollectionF2DPoint = interface(IsgCollectionBaseSort)
    function GetItem(const AIndex: Integer): TF2DPoint;
    procedure SetItem(const AIndex: Integer; const AValue: TF2DPoint);
    property Items[const AIndex: Integer]: TF2DPoint read GetItem
      write SetItem; default;
  end;

  IsgCollectionFPoint = interface(IsgCollectionBaseSort)
    function GetItem(const AIndex: Integer): TFPoint;
    procedure SetItem(const AIndex: Integer; const AValue: TFPoint);
    property Items[const AIndex: Integer]: TFPoint read GetItem
      write SetItem; default;
  end;

  IsgCollectionF4DPoint = interface(IsgCollectionBaseSort)
    function GetItem(const AIndex: Integer): TF4DPoint;
    procedure SetItem(const AIndex: Integer; const AValue: TF4DPoint);
    property Items[const AIndex: Integer]: TF4DPoint read GetItem
      write SetItem; default;
  end;

  IsgCollectionPointer = interface(IsgCollectionBaseSort)
    function Add(const AValue: Pointer): Integer;
    function IndexOf(const AValue: Pointer): Integer;
    function GetItem(const AIndex: Integer): Pointer;
    function GetFPointProc: TsgObjProcGetFPoint;
    function Remove(const AValue: Pointer): Integer;
    procedure SetItem(const AIndex: Integer; const AValue: Pointer);
    procedure SetFPointProc(const Value: TsgObjProcGetFPoint);
    property Items[const AIndex: Integer]: Pointer read GetItem
      write SetItem; default;
  end;

  IsgCollectionObject = interface(IsgCollectionBaseSort)
    function Add(const AValue: TObject): Integer;
    function IndexOf(const AValue: TObject): Integer;
    function GetItem(const AIndex: Integer): TObject;
    function GetFPointProc: TsgObjProcGetFPoint;
    function Remove(const AValue: TObject): Integer;
    procedure SetItem(const AIndex: Integer; const AValue: TObject);
    procedure SetFPointProc(const Value: TsgObjProcGetFPoint);
    property Items[const AIndex: Integer]: TObject read GetItem
      write SetItem; default;
  end;

  IsgObjCollection = interface
    ['{51BCC18C-7615-40ED-94BD-D46E0FAF578A}']
    function GetObj(Handle: UInt64): TObject;
  end;

  //0 - false
  //1 - true
  //2 - full
  IsgInputRectCoords = interface
    [cnstGUID_InputRectCoords]
    function OpenFile(const AFileName: string): Integer;
    function LoadPoints(const APoints: IsgArrayFPoint): Integer;
    function LoadPolygons(const APolygons: IsgCollectionObject): Integer;
    function SetPoints(const APoints: TStrings): Integer;
  end;

  TsgDrawingXMLData = record
    Guid: TGUID;
    Name: string;
    Index: Integer;
    Mode: Integer;
    State: Integer;
    Current: Boolean;
  end;

{$IFDEF SGDEL_XE2}
  TsgFileStreamTmp = class(TFileStream)
  public
    DeleteOnFree: Boolean;
    destructor Destroy; override;
  end;
{$ENDIF}

  TsgStorage = class(TPersistent)
  private
    FSaved: Boolean;
    FOwnObject: Boolean;
    FOnBeforeSaveToStream: TNotifyEvent;
    FOnAfterSaveToStream: TNotifyEvent;
    FListeners: array of TNotifyEvent;
    FMode: Integer;
    procedure SetFileName(const Value: TFileName);
    function GetFileNameFromStream(Stream: TObject): TFileName;
    function GetFileName: TFileName;
    procedure SetStream(const Value: TStream);
    function GetIsFile: Boolean;
    function GetStream: TStream;
    function GetKind: TTypeKind;{$IFDEF SG_INLINE} inline;{$ENDIF}
    function GetIsStream: Boolean;
    function GetTypeName: string;{$IFDEF SG_INLINE} inline;{$ENDIF}
    function GetConnected: Boolean;
    function MethodEqual(const A, B: TNotifyEvent): Integer;
    function MethodNotEqual(const A, B: TNotifyEvent): Integer;
    function MethodEmptyOrEqual(const A, B: TNotifyEvent): Integer;
  protected
    FData: Int64;
    FTypeInfo: Pointer;
    procedure FinalizeData;
    procedure DoBeforeSaveToStream(Sender: TObject); dynamic;
    procedure DoAfterSaveToStream(Sender: TObject); dynamic;
    procedure NotifyListeners;
    function Find(const ANotify: TNotifyEvent; var Index: Integer; ACompareMethods: TsgNotifyEventCompare): Boolean;
    property Kind: TTypeKind read GetKind;
    property TypeName: string read GetTypeName;
    property OnBeforeSaveToStream: TNotifyEvent read FOnBeforeSaveToStream write FOnBeforeSaveToStream;
    property OnAfterSaveToStream: TNotifyEvent read FOnAfterSaveToStream write FOnAfterSaveToStream;
  public
    constructor Create(AOwnObject: Boolean = True); overload; virtual;
    constructor Create(const AFileName: TFileName; AOwnObject: Boolean = True); overload; virtual;
    constructor Create(const AStream: TStream; AOwnObject: Boolean = True); overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AddListener(const ANotify: TNotifyEvent): Integer;
    function RemoveListener(const ANotify: TNotifyEvent): Integer; overload;
    function RemoveListener(ACookie: Integer): Integer; overload;
    function Save(AExportObject: TPersistent): Boolean;
    function Load(ADestObject: TPersistent): Boolean;
    procedure CopyToClipboard(AFormat: Word);
    function Delete: Boolean;
    function Close: Boolean;
    function Rename(const ANewName: TFileName): Boolean;
    property Stream: TStream read GetStream write SetStream;
    property FileName: TFileName read GetFileName write SetFileName;
    property IsFile: Boolean read GetIsFile;
    property IsStream: Boolean read GetIsStream;
    property OwnObject: Boolean read FOwnObject write FOwnObject;
    property TypeInfoPtr: Pointer read FTypeInfo;
    property Connected: Boolean read GetConnected;
    property Saved: Boolean read FSaved write FSaved;
  end;

  TsgMainUpdateMode = (mumSilent, mumFullUpdate, mumXmlInterfaceUpdate);

  IsgXMLOut = interface(IInterface)
    [cnstGUID_XMLOut]
    procedure AddToOutput(const Attribs: TStrings);
    procedure AddItemsToOutput(const AItems: TStrings);
    procedure AddToOutputNV(const AName, AValue: string; const AMessage: string = '');
    procedure AddToOutputFromNode(const ANode: TObject);
    procedure AddToError(const Attribs: TStrings);
    procedure AddToErrorNV(const AName, AValue: string; const AMessage: string = '');
  end;

  IsgMainApplication = interface(IInterface)
    [cnstGUID_MainApplication]
    procedure CreateUserMenu(const ANames: TStrings;
      const ASenders, AClicks, AOnOPopups, AImageIndexes: TList;
      const AMode: Integer; const AIconsBitmap: TBitmap);
    procedure SetRibbonVisible(const Value: Boolean);
    function OpenFile(AFileName: string; AddResent: Boolean; const AMode: string;
      const AGuid: PGUID; AASync: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStream; AExt: string; const AMode: string;
      const AGuid: PGUID): Boolean;
    function SaveFileWithXmlParams(const AXMLParams: string; AStorage: TsgStorage;
      var AFormat: Integer): Integer;
    function SaveBitmapToStream(const ASource: TBitmap;
      const AExportParams: TObject;
      const AName: string; const AStream: TStream): Boolean;
    function CreateDrawing: TGUID;
    function GetViewRect: TRect;
    function GetViewFRect: TFRect;
    procedure SetViewFRect(const ARect: TFRect);
    function ApplyXML(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    procedure SetCurrDrawingSizes(const AWidth, AHeight: Integer);
    procedure SetDrawingChange(const AGuid: TGUID; const AMode: Integer; const AChange: Boolean);
    function CloseDrawingGuid(const AGuid: TGUID): Boolean;
    function GetActualGraphic(const AExportParams: TObject): {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TObject{$ENDIF};
    function GetDrawingsCount: Integer;
    function GetDrawingGuid(const AIndex: Integer): TsgDrawingXMLData;
    function GetCurrentDrawingGuid: TGUID;
    function GetCADEditorCurrent: TObject;
    procedure WriteError(const AMessage: string);
    function RotateCurrentDrawing(const AX, AY, AZ: Double): Integer;
    procedure SetCurrentDrawingGuid(const AValue: TGUID);
    procedure MainUpdate(const AMode: TsgMainUpdateMode = mumSilent);
    function CommandReport(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandGetData(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandSetData(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandCreateBtiEntity(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandXml(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function SaveCompleate: Boolean;
    procedure UpdateLayouts(AIsSetDefault: Boolean);
    property CurrentDrawingGuid: TGUID read GetCurrentDrawingGuid
      write SetCurrentDrawingGuid;
    property DrawingGuid[const AIndex: Integer]: TsgDrawingXMLData read GetDrawingGuid;
    property DrawingsCount: Integer read GetDrawingsCount;
  end;

  TsgXMLMessage = procedure(const S: WideString) of object;

  IsgXMLIde = interface(IInterface)
    [cnstGUID_XMLIde]
    function ProcessXML(const AInput: string): string;
    function GetOnXMLMessage: TsgXMLMessage;
    procedure SetOnXMLMessage(const Value: TsgXMLMessage);
    property OnXMLMessage: TsgXMLMessage read GetOnXMLMessage write SetOnXMLMessage;
  end;

  IsgNodeFrame = interface
    [cnstGUID_XMLNode]
    function AddChild(const NodeName: string): IsgNodeFrame;
    procedure ClearChildNodes;
    function FindChildNode(const NodeName: string): IsgNodeFrame;
    function ChildNodeCount: Integer;
    function GetAttribute(const AttrName: string): OleVariant;
    function GetChildNode(const AIndex: Integer): IsgNodeFrame;
    function GetChildValue(const IndexOrName: string): OleVariant;
    function GetName: string;
    procedure SetAttribute(const AttrName: string; const Value: OleVariant);
    procedure SetChildValue(const IndexOrName: string; const Value: OleVariant);
    property Attributes[const AttrName: string]: OleVariant read GetAttribute write SetAttribute;
    property ChildValues[const IndexOrName: string]: OleVariant read GetChildValue write SetChildValue; default;
    property ChildNodes[const AIndex: Integer]: IsgNodeFrame read GetChildNode;
  end;

  IsgXMLPluginItem = interface
    [cnstGUID_PluginItem]
    function ExportToPlugins(Node: IsgNodeFrame): Integer;
    function ChangeFromPlugins(ANode: IsgNodeFrame; const APropName: string;
      const AValue: Variant): Integer;
  end;


  IsgProgress = interface
    [cnstGUID_Progress]
    procedure DoProgress(Sender: TObject; Stage:{$IFDEF SG_FIREMONKEY}sgFMXTypes.{$ENDIF}TProgressStage; PercentDone: Byte;
      RedrawNow: Boolean; const R: TRect; const Msg: string{$IFDEF SGFPC}; var Continue: Boolean{$ENDIF});
    function GetBreak: Boolean;
    procedure SetBreak(const AValue: Boolean);
    property Break: Boolean read GetBreak write SetBreak;
  end;


  TsgExternalPenMode = (epmNone, epmInternal, epmExternal);

  TsgExternalProps = class
  public
    UseExternalPen: TsgExternalPenMode;
    EntityHandle: Int64;
    Mode: TsgCustomSelectMode;
  end;

  TsgMessageParams = record
    Code: Integer;
    Msg: string;
  end;

  TsgGLGraphicCustom = class(TBitmap)
  protected
    procedure Changed(Sender: TObject); {$IFDEF SG_FIREMONKEY} virtual; {$ELSE} override; {$ENDIF}
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); {$IFDEF SG_FIREMONKEY} virtual; {$ELSE} override; {$ENDIF}
    procedure DrawTransparent(ACanvas: TCanvas; const Rect: TRect; Opacity: Byte); {$IFDEF SG_FIREMONKEY} virtual; {$ELSE}{$IFNDEF SGFPC}{$IFDEF SGDEL_2009} override; {$ENDIF}{$ENDIF}{$ENDIF}
  public
    procedure UpdateViewport; virtual;
  end;

  { MESH object helper records }

  PsgVertexIndices = ^TsgVertexIndices;
  TsgVertexIndices = array[0 .. 3] of Integer;

  PsgInlineFace = ^TsgInlineFace;
  TsgInlineFace = record
    Count: Integer;
    Indices: TsgVertexIndices;
  end;

  TsgInlineFaceIndices = array of PsgVertexIndices;
  TsgFaceIndices = array of PsgInlineFace;

const

{Plugins}
  cnstExportToXML_NO = 0;
  cnstExportToXML_OK = 1;

{bti file descriptions}
  cnstBlockShortName = 4;
  cnstBTIReservedBlock = '*R0';
  cnstBlockDescriptionkName: string = cnstBTIReservedBlock + '1';//used as name blockpattern
  cnstBlockConstantsName: string = cnstBTIReservedBlock + '2';
  cnstBlockPatternLayer = '0';
  cnstFileDescription = 'FileDescription';
  cnstFileDescriptionVersion = '1';

{ bti props id }
  cnstBti_PropBase = 1;
  cnsBti_AreaHeight = cnstBti_PropBase + 1;

{Mathematical Constants}
  cnst1DivPi = 1 / Pi;
  cnstCos30 = 0.86602540378443864676372317075294;
  cnstCos45 = 0.70710678118654752440084436210485;
  cnstCos60 = 0.5;
  cnstCrossYMat: TFMatrix = (M:((1, 0, 0), (0, -1, 0), (0, 0, 1), (0, 0, 0)));
  cnstIdentityMat: TFMatrix = (M:((1, 0, 0), (0, 1, 0), (0, 0, 1), (0, 0, 0)));
//  cnstTestMat: TFMatrix = (M:((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12)));

  cnstFract16: Double = 0.0000152587890625;//1 / 2^16
  cnstFract32: Double = 0.00000000023283064365386962890625;//1 / 2^32
  cnstPi = Pi;
  cnstPiDiv2 = cnstPi * 0.5;
  cnstPiMul2 = cnstPi * 2;
  cnstPiMul4 = cnstPi * 4;
  cnstSqrt2 = 1.4142135623730950488016887242097;
  cnstSqrt3 = 1.7320508075688772935274463415059;
  f180DividedByPi: Extended = 57.295779513082320876798154814105; // 180 / Pi
  //the value is used in the RotToView function
  fIsometricRotation = 54.735610317245345684622999669981;
  fPiDividedBy180: Extended = 0.017453292519943295769236907684886; // Pi / 180
  cnstMMPerInch = 25.4;
  cnstPlus = '+';

  cnstEmptyPoints = -1;
  cnstNoInitPoints = -2;
  cnstIndexesNoInit: TsgIndexes4 = (cnstNoInitPoints, cnstNoInitPoints,
    cnstNoInitPoints, cnstNoInitPoints);
  cnstIndexesEmpty: TsgIndexes4 = (cnstEmptyPoints, cnstEmptyPoints,
    cnstEmptyPoints, cnstEmptyPoints);
  cntBitPerByte = 8;
  cntBitPerWord = 16;
  cntLastBitInByte = SizeOf(Byte) * cntBitPerByte - 1;
  cntLastBitInWord = SizeOf(Word) * cntBitPerByte - 1;
  cntMulDiv8 = 3;

{save result}
  cnstSaveResultOk = 0;
  cnstSaveResultError = 1;
  cnstSaveResultNeedParams = 2;
  cnstSaveResultUndefinedFormat = 3;
  cnstSaveResultUnsupportedFormat = 4;
  cnstSaveResultLimitationSave = 5;

{GetLastErrorCAD command}
  cnstNoResult = '0';
  cnstTrialExpired = '1009';

{Vectorization}
  cnstDefContrast = 128;
  cnstDefTolerance = 95;
  cnstDefSkipSegment = 3;

  cnstVectrContrast: Integer = cnstDefContrast;
  cnstVectrTolerance: Integer = cnstDefTolerance;
  cnstVectrSkipSegment: Double = cnstDefSkipSegment;

{boundary max, min, bad value}

  BadSingValue: Single = -24444444; // bad value for all singles
  cnstIllegal = -$5555 * 65536.0 * 65536.0;
  cnstMaxLongWord = High(LongWord);
  cnstNan = MaxDouble;
  cnstMaxByte = 255;
  cnstMaxWord = 65535;
  cnstMaxInt32 = 2147483647;
  cnstMinInt32 = Integer($80000000);//-2147483648;
  cnstMaxUInt32 = $FFFFFFFF;

  cnstMaxSingleValue:Single =  340282346638528859811704183484516925440.0;
  cnstMinSingleValue:Single = -340282346638528859811704183484516925440.0;

  cMaxDoubleValue = 1.7e+10;
  cMinDoubleValue = - cMaxDoubleValue;
  fMaxDouble = 1E308;
  fMinDouble = -fMaxDouble;

{$IFDEF SG_FIREMONKEY}
  {$EXTERNALSYM MINCHAR}
  MINCHAR = $80;
  {$EXTERNALSYM MAXCHAR}
  MAXCHAR = 127;
  {$EXTERNALSYM MINSHORT}
  MINSHORT = $8000;
  {$EXTERNALSYM MAXSHORT}
  MAXSHORT = 32767;
  {$EXTERNALSYM MINLONG}
  MINLONG = DWORD($80000000);
  {$EXTERNALSYM MAXLONG}
  MAXLONG = $7FFFFFFF;
  {$EXTERNALSYM MAXBYTE}
  MAXBYTE = 255;
  {$EXTERNALSYM MAXWORD}
  MAXWORD = 65535;
  {$EXTERNALSYM MAXDWORD}
  MAXDWORD = DWORD($FFFFFFFF);
  cnstMultiViewPanelWidth = {$IFDEF SG_FM_MOBILE}250{$ELSE}367{$ENDIF};
  cnstSettingsMainPanelWidth = 640;
  cnstSettingsMaultiPageMargin = 24;
{$ENDIF}

{mouse wheel}
  cnstScrollStep = 16;

{settings boundaries}
  cnstAnglesByPolarSnap: array[0..8] of TsgFloat = (1, 5, 10, 15, 18, 22.5, 30, 45, 90);

{zooming constants}
  cnstScales: array [Boolean] of Double = (5/4, 4/5);
  cnstLTScale = 3/100;

{graphics constants}

  arrDXFtoRGBColors: TsgCADPalett = (   // AutoCAD 8-bit colors
    $000000, $0000FF, $00FFFF, $00FF00, $FFFF00, $FF0000, $FF00FF, $FFFFFF,
    $808080, $C0C0C0, $0000FF, $7F7FFF, $0000CC, $6666CC, $000099, $4C4C99,
    $00007F, $3F3F7F, $00004C, $26264C, $003FFF, $7F9FFF, $0033CC, $667FCC,
    $002699, $4C5F99, $001F7F, $3F4F7F, $00134C, $262F4C, $007FFF, $7FBFFF,
    $0066CC, $6699CC, $004C99, $4C7299, $003F7F, $3F5F7F, $00264C, $26394C,
    $00BFFF, $7FDFFF, $0099CC, $66B2CC, $007299, $4C8599, $005F7F, $3F6F7F,
    $00394C, $26424C, $00FFFF, $7FFFFF, $00CCCC, $66CCCC, $009999, $4C9999,
    $007F7F, $3F7F7F, $004C4C, $264C4C, $00FFBF, $7FFFDF, $00CC99, $66CCB2,
    $009972, $4C9985, $007F5F, $3F7F6F, $004C39, $264C42, $00FF7F, $7FFFBF,
    $00CC66, $66CC99, $00994C, $4C9972, $007F3F, $3F7F5F, $004C26, $264C39,
    $00FF3F, $7FFF9F, $00CC33, $66CC7F, $009926, $4C995F, $007F1F, $3F7F4F,
    $004C13, $264C2F, $00FF00, $7FFF7F, $00CC00, $66CC66, $009900, $4C994C,
    $007F00, $3F7F3F, $004C00, $264C26, $3FFF00, $9FFF7F, $33CC00, $7FCC66,
    $269900, $5F994C, $1F7F00, $4F7F3F, $134C00, $2F4C26, $7FFF00, $BFFF7F,
    $66CC00, $99CC66, $4C9900, $72994C, $3F7F00, $5F7F3F, $264C00, $394C26,
    $BFFF00, $DFFF7F, $99CC00, $B2CC66, $729900, $85994C, $5F7F00, $6F7F3F,
    $394C00, $424C26, $FFFF00, $FFFF7F, $CCCC00, $CCCC66, $999900, $99994C,
    $7F7F00, $7F7F3F, $4C4C00, $4C4C26, $FFBF00, $FFDF7F, $CC9900, $CCB266,
    $997200, $99854C, $7F5F00, $7F6F3F, $4C3900, $4C4226, $FF7F00, $FFBF7F,
    $CC6600, $CC9966, $994C00, $99724C, $7F3F00, $7F5F3F, $4C2600, $4C3926,
    $FF3F00, $FF9F7F, $CC3300, $CC7F66, $992600, $995F4C, $7F1F00, $7F4F3F,
    $4C1300, $4C2F26, $FF0000, $FF7F7F, $CC0000, $CC6666, $990000, $994C4C,
    $7F0000, $7F3F3F, $4C0000, $4C2626, $FF003F, $FF7F9F, $CC0033, $CC667F,
    $990026, $994C5F, $7F001F, $7F3F4F, $4C0013, $4C262F, $FF007F, $FF7FBF,
    $CC0066, $CC6699, $99004C, $994C72, $7F003F, $7F3F5F, $4C0026, $4C2639,
    $FF00BF, $FF7FDF, $CC0099, $CC66B2, $990072, $994C85, $7F005F, $7F3F6F,
    $4C0039, $4C2642, $FF00FF, $FF7FFF, $CC00CC, $CC66CC, $990099, $994C99,
    $7F007F, $7F3F7F, $4C004C, $4C264C, $BF00FF, $DF7FFF, $9900CC, $B266CC,
    $720099, $854C99, $5F007F, $6F3F7F, $39004C, $42264C, $7F00FF, $BF7FFF,
    $6600CC, $9966CC, $4C0099, $724C99, $3F007F, $5F3F7F, $26004C, $39264C,
    $3F00FF, $9F7FFF, $3300CC, $7F66CC, $260099, $5F4C99, $1F007F, $4F3F7F,
    $13004C, $2F264C, $333333, $5B5B5B, $848484, $ADADAD, $D6D6D6, $FFFFFF);

  clByBlock = $3FFFFFFF; // dxf/dwg color ByBlock
  clByLayer = $2FFFFFFF; // dxf/dwg color ByLayer
  clBackGroundCompare = $FEFEFE; //for only Graphics compare

  cmcByLayer = Byte($C0);
  cmcByBlock = Byte($C1);
  cmcBGR     = Byte($C2);
  cmcIndexed = Byte($C3);

  clDXFByBlock    = 0;
  clDXFRed        = 1;
  clDXFYellow     = 2;
  clDXFGreen      = 3;
  clDXFAqua       = 4;
  clDXFBlue       = 5;
  clDXFFuchsia    = 6;
  clDXFBlackWhite = 7;
  clDXFGray       = 8;
  clDXFSilver     = 9;
  clDXFAlternativeBlack = 250;// for replacement of a clDXFBlackWhite
  clDXFDKGray     = 251;// dark gray
  clDXFLTGray     = 254;// light gray
  clDXFWhite      = 255;
  clDXFByLayer    = 256;

  cnstColorOrange = $A5FF;

  cnstSnapMarkerSize = 8;

  cnstUseCustomEdgeColor: Boolean = False;
  cnstCustomEdgeColor: TColor = $0050FF;


  cnstExtractSubstrings: Boolean = True;
  cnstUseVectorizationTextStyle: Boolean = False;
  cnstDefaultDrawingMode = 0;//dmNormal
  cnstBackgroundColor = clWhite;
  cnstDefaultColor = clBlack;

  cnstMakrControlPointsFill: Boolean = True;
  cnstMakrControlPointsColor = cnstColorOrange;

  cnstGcodeModeColor = $008CFF;

  cnstAlternateWhite = False;
  cnstAlternateWhiteColor = clBlack;
  cnstAlternateBlack = False;

  bAlternateBlack: Boolean = cnstAlternateBlack;
  bAlternateWhite: Boolean = cnstAlternateWhite;
  cAlternateWhiteColor: TColor = cnstAlternateWhiteColor;

  cnstLoadEmptyXRef = False;
  bLoadEmptyXRef: Boolean = cnstLoadEmptyXRef;

  cnstPrintBackground: TColor = clBlack;
  cnstPrintDefaultColor: TColor = clWhite;

  cnstPlotWindowAreaColor = $E1E1E1;
  cnstPlotPaperAreaColor = clWhite;
  cnstPlotBackground = $C0C0C0;

  cnstColorCADByBlock: TsgColorCAD = (Active: acIndexColor; Color: clDXFByBlock; AlbumString: '');
  cnstColorCADByLayer: TsgColorCAD = (Active: acIndexColor; Color: clDXFByLayer; AlbumString: '');
  cnstColorCADByBlackWhite: TsgColorCAD = (Active: acIndexColor; Color: clDXFBlackWhite; AlbumString: '');
  cnstColorCADByWhite: TsgColorCAD = (Active: acRGBColor; Color: clWhite; AlbumString: '');
  cnstDefaultEntityColor: TsgColorCAD = (Active: acIndexColor; Color: clDXFByLayer; AlbumString: '');
  cnstColorCADNone: TsgColorCAD  = (Active: acRGBColor; Color: clNone; AlbumString: '');
  cnstColorCADRed: TsgColorCAD  = (Active: acIndexColor; Color: clDXFRed; AlbumString: '');
  cnstColorCADAqua: TsgColorCAD  = (Active: acIndexColor; Color: clDXFAqua; AlbumString: '');
  cnstColorCADBlack: TsgColorCAD = (Active: acRGBColor; Color: clBlack; AlbumString: '');
  cnstColorCADEmpty: TsgColorCAD  = (Active: acRGBColor; Color: clEmpty; AlbumString: '');

  clHpglNone = clNone;
  cnstColorCADHPGLNone: TsgColorCAD  = (Active: acIndexColor; Color: clDXFBlackWhite; AlbumString: '');


  cnstBackgroundTransparencyDefault = 11076216;// $A90278
  bBackGroundLayerNoPlotting: Boolean = False;

  //Gcode colors
  cnstDefArrowColor = $0000FF;
  cnstDefFPtColor = $00FF00;
  cnstDefNodePointsColor = $C0C0C0;
  cnstDefLeadInColor = $FF00FF;
  cnstDefLeadOutColor = $FF6701;
  cnstDefToolColor = $0173FF;
  cnstDefSelectedColor = $008000;
  cnstNodePointsColor: TColor = $C0C0C0;
  cnstArrowColor: TColor = $0000FF;
  cnstFPtColor: TColor = $00FF00;
  cnstLeadInColor: TColor = $FF00FF;
  cnstLeadOutColor: TColor = $FF6701;
  cnstToolColor: TColor = $0173FF;
  cnstSelectedColor: TColor = $008000;
  cnstEqRCompGRBL: TColor = $0000FF;

  cnstColorCA_RGB_Black: TsgColorCAD  = (Active: acRGBColor; Color: clBlack; AlbumString: '');

  cnstCADHatchSetting: TsgHatchSettting = hsNoChange;//for internal using

  cnstLimitByHatch = 1024*1024*32*4;
  cnstTime1Sec = 1000;//ticks
  cnstTime1Min = 60 * cnstTime1Sec;
{$IFDEF DEBUG}
  cnstLimitTimeByHatch = cnstTime1Min * 10; //10 minutes
{$ELSE}
  cnstLimitTimeByHatch = cnstTime1Min * 1; //1 minutes
{$ENDIF}

  cnstHatchUpdateInterval = 5 * cnstTime1Sec;
  cnstIgnoreDrawStopped: Integer = 0;
  cnstIgnoreDrawStoppedName = 'IgnoreDrawStopped';


  cnstMaxImgSize = 4096;
  cnstMaxImgSizeSqrt = cnstMaxImgSize * cnstMaxImgSize;
  cnstBadSize = $7FFF;
  cnstBad2DRect: TRect = (Left:cnstBadSize; Top:cnstBadSize;
    Right:-cnstBadSize; Bottom:-cnstBadSize);
  cnstBad2DRectExport: TRect = (Left:$7FFFFFF; Top:$7FFFFFF; Right:-$7FFFFFF; Bottom:-$7FFFFFF);
  cnstBadPoint: TFPoint = (X: cnstNan; Y: cnstNan; Z: cnstNan);
  cnstBadParam = 1E20;
  cnstBadRect: TFRect = (Left: 1E20; Top: -1E20; Z1: 1E20; Right:-1E20;
    Bottom: 1E20; Z2: -1E20);
  cnstBadRectF2D: TF2DRect = (Left: 1E20; Top: -1E20; Right:-1E20;
    Bottom: 1E20);
  cnstExtrusion: TFPoint = (X: 0; Y: 0; Z: 1);
  cnstF2DPointZero: TF2DPoint = (X: 0; Y: 0);
  cnstF2DPointSingle: TF2DPoint = (X: 1; Y: 1);
  cnstFPointIllegal: TFPoint = (X:cnstIllegal; Y:cnstIllegal; Z:0);
  cnstFPointSingle: TFPoint = (X:1; Y:1; Z:1);
  cnstFPointSingleYMinus: TFPoint = (X:1; Y:-1; Z:1);
  cnstFPointSize = SizeOf(TFPoint) div SizeOf(Pointer);
  cnstFPointZero: TFPoint = (X: 0; Y: 0; Z: 0);
  cnstPointZero: TPoint = (X: 0; Y: 0);
  cnstPointSingle: TPoint = (X: 1; Y: 1);
  cnstRectZero: TRect = (Left:0; Top:0; Right:0; Bottom:0);
  cnstXOrtAxis: TFPoint = (X: 1; Y: 0; Z: 0);
  cnstYOrtAxis: TFPoint = (X: 0; Y: 1; Z: 0);
  cnstZOrtAxis: TFPoint = (X: 0; Y: 0; Z: 1);
  cnstFRectZero: TFRect = (Left:0; Top:0; Z1: 0; Right:0; Bottom:0; Z2: 0);
  cnstF2DRectZero: TF2DRect = (Left:0; Top:0; Right:0; Bottom:0);
  cnstObjectReserve: Pointer = Pointer($1);

  cnstMaxStringLen = 250;

  cnstPathFlag = 256;
  cnstPathEvacuationFlag = 512;
  cnstPolyPointsNotSolid = 2048;
  cnstPolySignMinus = 256;
  cnstPolySignPlus = 1024;
{$IFDEF SG_EVACUATION}
  bSaveBTIDataDwg: Boolean = False;
{$ELSE}
  bSaveBTIDataDwg: Boolean = True;
{$ENDIF}

{drawing constants}
  cnstUseStackForBlockEditorOpt = 'StackBlockEditor';
  bUseStackForBlockEditor: Boolean = False;

{navigator constants}
  bMoveByRightMouse: Boolean = True;
  cnstExtendedStructureDefault = 1;
  iExtendedStructure: Byte = cnstExtendedStructureDefault;

{cadeditor constants}
  cnstConfirmUndefined = 0;
  cnstConfirmNo = 1;
  cnstConfirmYes = 2;
  cnstEntitySelectedModeDefault = 7;

{pdf load}
  bSaveNotStandartFormat: Boolean = True;

{path evacuation defaults}
  cnstColorCADByPathEvacuation: TsgColorCAD = (Active: acIndexColor; Color: 116; AlbumString: '');
  sLayerPathEvacuation: string = '';

  cnstCreateViewportInNewLayouts: Boolean = True;// for future versions

  cnstDefaulPrinter = 'DWF6 ePlot.pc3';
  cnstDefaulPaperWidth = 210;
  cnstDefaulPaperHeight = 297;
  cnstPrinterNone = 'None';
  cnstNoneDevice = 'none_device';
  cnstMargin = 3.2;
  cnstDefaultMargins: TF2DRect = (Left: cnstMargin; Top: cnstMargin; Right: cnstMargin; Bottom: cnstMargin);

  cnstUnprintableMarginEmpty: Boolean = False;
  cnstUnprintableMarginEmptyName = 'UnprintableMarginEmpty';

  cnstUnprintableMarginWidth = 5;
  cnstUnprintableMarginHeight = cnstUnprintableMarginWidth *
    cnstDefaulPaperHeight / cnstDefaulPaperWidth;
  cnstUnprintableMargins: TF2DRect = (
    Left: cnstUnprintableMarginWidth;
    Top: cnstUnprintableMarginHeight;
    Right: cnstUnprintableMarginWidth;
    Bottom: cnstUnprintableMarginHeight);

  cnstDefaultLayoutWidth = cnstDefaulPaperHeight - cnstUnprintableMarginHeight * 2;

  cnstDefaultPlotSettingsData: TsgPlotSettingsData =
    (
      PageSetupName: '';
      PrintOrConfigName:  cnstNoneDevice;
      PlotLayoutFlags: [plfUseStandardScale, plfPlotPlotStyles,
        plfPrintLineweights,plfDrawViewportsFirst];
      PlotPaperUnits: ppuMM;
      StandardScaleType: 16;
      NumeratorOfCustomPrintScale: 1;
      DenominatorOfCustomPrintScale: 1;
      PaperSize: '';
      PlotViewName: '';
      UnprintableMargin: (Left: 0; Top: 0; Right: 0; Bottom: 0);
      PlotPaperSize: (X: 0; Y: 0);
      PlotOrigin: (X: 0; Y: 0);
      PlotWindowAreaMin: (X: 0; Y: 0);
      PlotWindowAreaMax: (X: 0; Y: 0);
      PlotRotation: prNoRotation;
      PlotType: ptLayoutInformation;
      CurrentStyleSheet: '';
      ShadePlotMode: 0;
      ShadePlotResolutionLevel: 0;
      ShadePlotCustomDPI: 100;
      FloatingPointScaleFactor: 1;
      PaperImageOrigin: (X: 0; Y: 0);
    );

  sgDefaultPlotSettingsData:  TsgPlotSettingsData =
    (
      PageSetupName: '';
      PrintOrConfigName:  cnstNoneDevice;
      PlotLayoutFlags: [plfUseStandardScale];
      PlotPaperUnits: ppuMM;
      StandardScaleType: 0;
      NumeratorOfCustomPrintScale: 1;
      DenominatorOfCustomPrintScale: 1;
      PaperSize: 'ISO_A4_(297.00_x_210.00_MM)';
      PlotViewName: '';
      UnprintableMargin: (
        Left: 20;
        Top: 7.5;
        Right: 20;
        Bottom: 7.5);
      PlotPaperSize: (X: 210; Y:297);
      PlotOrigin: (X: 0; Y: 0);
      PlotWindowAreaMin: (X: 0; Y: 0);
      PlotWindowAreaMax: (X: 0; Y: 0);
      PlotRotation: pr90DegreesCounterCW;
      PlotType: ptLayoutInformation;
      CurrentStyleSheet: '';
      ShadePlotMode: 0;
      ShadePlotResolutionLevel: 0;
      ShadePlotCustomDPI: 100;
      FloatingPointScaleFactor: 1;
      PaperImageOrigin: (X: 0; Y: 0);
    );

{raster constant}
  iBitmapSizeWinNT: Integer = 300000000;
  iBitmapSizeWin98: Integer = 16777215;
{$IFDEF SG_EVACUATION}
  cnstJPEGQuality = 100;
  cnstDrawTemlatesIconsQuality: Boolean = True;
{$ELSE}
  cnstJPEGQuality = {$IFDEF SG_FIREMONKEY}100{$ELSE}60{$ENDIF};
  cnstDrawTemlatesIconsQuality: Boolean = False;
{$ENDIF}
  iMetafileSizeMin = 64;
  iMetafileSizeMax = 31000;
  iJPEGQuality: Integer = cnstJPEGQuality;

{$IFNDEF SG_FIREMONKEY}
  DefRastrExportParams: TsgRastrExportParams =(
    SizeMode: smSame;
    Width: 640;
    Height: 480;
    MaxDim: 99999999;
    Depth: pf24bit;
    Compression: ctNone;
    DPU: (X: 96; Y:96);
    MeasureInPixels: True;
    Transparent: False;
    Quality: cnstJPEGQuality;
    SaveProportion: True;
    Size: (X: 0; Y: 0);
    DPM: 0;
    ImageQuality: 1;
    UserScale: '';
    BlackWhitePic: False;
    Smooth: False;
  );
{$ENDIF}

{string XML export constants}

const
  cnstXMLFileName = 'FileName';
  cnstXMLExternalFile = 'ExternalFile';
  cnstXMLImage = 'Image';
  cnstXMLDirectoryName = 'DirName';
// Dde params
  cnstParAbviewer = '-abviewer';
  cnstParCadEditorX = '-cadeditorx';
  cnstParInventory = '-inventory';
  cnstParCadView = '-cadviewx';
  cnstParCadViewXPro = '-cadviewxpro';
  cnstParCADDll = '-caddll';
//pipes
  cnstParCADNavigator = '-cstcadnavigator';
// Clipping
  cnstXMLClipMode = 'ClipMode';
  cnstXMLClipRect = 'ClipRect';
// Rastr
  cnstXMLExportParams = 'ExportParams';
  cnstXMLFormat = 'Format';
  cnstXMLWidth = 'Width';
  cnstXMLHeight = 'Height';
  cnstXMLWidthM = 'WidthM';
  cnstXMLHeightM = 'HeightM';
  cnstXMLUserScale = 'UserScale';
  cnstXMLImageQuality = 'ImageQuality';
  cnstXMLBlackWhitePic = 'BlackWhitePic';
  cnstXMLDPM = 'DPM';
  cnstXMLMaxDim = 'MaxDim';
  cnstXMLQuality = 'Quality';
  cnstXMLMeasureInPixels = 'MeasureInPixels';
  cnstXMLSmoothing = 'Smothing';
  cnstXMLTransparent = 'Transparent';
  cnstXMLTransparentColor = 'TransparentColor';
  cnstXMLDpuX = 'DPUX';
  cnstXMLDpuY = 'DPUY';
  cnstXMLSizeMode = 'SizeMode';
  cnstXMLCompression = 'Compression';
  cnstXMLPixelFormat = 'BitPerPixel';
  cnstXMLBitPerPixel = 'BitPerPixel';
  cnstXMLSaveProportion = 'Proportional';
  cnstXMLSize = 'Size';
  cnstXMLBase64 = 'Base64';
// Cad constant
  cnstXMLVersion = 'Version';
  cnstXMLAlternateBlack = 'AlternateBlack';
  cnstXMLLineWeightScale = 'LineWeightScale';
  cnstXMLUnitSize = 'UnitSize';
  cnstXMLUse01MM = 'Use01MM';
  cnstXMLFillingEntity = 'FillingEntity';
  cnstXMLOffsetPoint = 'OffsetPoint';
  cnstXMLUseHighQuality = 'UseHighQuality';
  cnstXMLExportTexts = 'ExportTexts';
  cnstXMLSHXAnnotations = 'SHXAnnotations';
  cnstXMLUseOutlines = 'UseOutlines';
  cnstXMLUseSHXFonts = 'UseSHXFonts';
  cnstXMLTrueTypeOptimization = 'TrueTypeOptimization';

  cnstXMLPageSize = 'PageSize';
  cnstXMLPageOrientation = 'Orientation';
  cnstXMLPageWidth = 'PageWidth';
  cnstXMLPageHeight = 'PageHeight';
  cnstXMLMargin = 'Margin';
  cnstXMLDrawMode = 'DrawMode';
  cnstXMLBackgroundColor = 'BackgroundColor';
  cnstXMLDefaultColor = 'DefaultColor';
  cnstXMLNullWidth = 'NullWidth';
  cnstXMLSaveLineWeight = 'SaveLineWeight';
  cnstXMLCADQuality = 'CADQuality';
  cnstXMLLayoutExportMode = 'LayoutExportMode';
  cnstXMLLayoutNameExportMode = 'LayoutNameExportMode';
  cnstXMLTitle = 'Title';
  cnstXMLAuthor = 'Author';
  cnstXMLSubjct = 'Subjct';
  cnstXMLKeywords = 'Keywords';
  cnstXMLLineWeights = 'LineWeights';
  cnstXMLUseVectorLineWeight = 'UseVectorLineWeight';
  cnstXMLUseExtentsFromPlotSettings = 'UseExtentsFromPlotSettings';
  cnstXMLHPGLXScale = 'XScale';
  cnstXMLIsConvertImageToOLE = 'IsConvertImageToOLE';
  cnstXMLIsConvertAcisToOldVarian = 'IsConvertAcisToOldVarian';
  cnstXMLLayersMode = 'LayersMode';
  cnstXmlItem = 'Item';
  cnstXmlItems = cnstXmlItem + 's';
  cnstXMLGroups = 'Groups';
  cnstXMLGroup = 'Group';
  cnstXMLSizeAsExtents = 'SizeAsExtents';
  cnstXMLMMInCad = 'MMInCad';
  cnstXMLPlottingScale = 'PlottingScale';
  cnstXMLExtendedParams = 'ExtendedParams';
  cnstSnapFlagsAttrName = 'SnapFlags';
  cnstXMLPdfBitmapExportMode = 'BitmapExportMode';
  cnstXMLPaperSizeName = 'PaperSizeName';
  cnstXmlParams = 'Params';
  cnstXmlExecute = 'Execute';
  cnstXmlColorToLineWeight = 'ColorToLineWeight';
  cnstXmlColorToLineWeightMode = 'ColorToLineWeightMode';
  cnstXmlWriteTclColors = 'WriteTclColors';
  cnstXmlWriteAdskColors = 'WriteAdskColors';
  cnstXmlWriteAdskIndexColors = 'WriteAdskIndexColors';
  cnstXmlCreateMtl = 'XmlCreateMtl';
  cnstXmlIgnoreLineWeight = 'IgnoreLineWeight';
  cnstXmlIgnoreLayerNoPlotting = 'IgnoreLayerNoPlotting';
  cnstXmlTextAsPolygons = 'TextAsPolygons';
  cnstXmlColorSpace = 'ColorSpace';
//SVG
  cnstXmlLineTypeMode = 'LineTypeMode';
  cnstXmlNullWeightMode = 'NullWeightMode';
  cnstXmlFillBackground = 'FillBackground';
//Gcode
  cnstAbsoluteCoordIJ = 'AbsoluteCoordinatesIJ';
  cnstArcToLines = 'ArcToLines';
  cnstCodeOptizime = 'CodeOptimize';
  cnstPositioningSystem = 'PositioningSystem';
  cnstConvOnlyVisLayers = 'ConvertOnlyVisibleLayers';
  cnstMachineType = 'MachineTypeID';
  cnstPassesDirection = 'PassesDirectionID';
  cnstPrecisionDigits = 'PrecisionDigits';
  cnstPrecisionFactor = 'PrecisionFactor';
  cnstFeedOnZ = 'FeedOnZ';
  cnstFeedOnXY = 'FeedOnXY';
  cnstFeedOnXZ = 'FeedOnXZ';
  cnstSpindleSpeed = 'SpindleSpeed';
  cnstDepthOnZ = 'DepthOnZ';
  cnstDepthPass = 'DepthPass';
  cnstFullDepthPass = 'FullDepthPass';
  cnstNumbOfPasses = 'NumbOfPasses';
  cnstDepartureOnZ = 'DepartureOnZ';
  cnstLaserOnCommand = 'LaserOnCommand';
  cnstLaserOffCommand = 'LaserOffCommand';
  cnstUseLaserPower = 'UseLaserPower';
  cnstLaserPower = 'LaserPower';
  cnstMaxLaserPower = 'MaxLaserPower';
  cnstDelay = 'Delay';
  cnstForceLaserOffGRBL = 'ForceLaserOffGRBL';
  cnstMachineUnits = 'MachineUnitsID';
  cnstDrawingUnits = 'DrawingUnitsID';
  cnstAddLabelOfProgam = 'AddLabelOfProgam';
  cnstLabelOfProgram = 'LabelOfProgram';
  cnstAddNumbering = 'AddNumbering';
  cnstShowContourName = 'ContourName';
  cnstShowLayerName = 'LayerName';
  cnstShowPercent = 'ShowPercent';
  cnstStartNumber = 'StartNumber';
  cnstStepOfNumbering = 'StepOfNumbering';
  cnstTurnOffPumpBefore = 'TurnOffPumpBefore';
  cnstTurnOffPumpBeforeValue = 'TurnOffPumpBeforeValue';
  cnstDispenserOffCommand = 'DispenserOffCommand';
  cnstDispenserOnCommand = 'DispenserOnCommand';
  cnstPumpOffCommand = 'PumpOffCommand';
  cnstPumpOnCommand = 'PumpOnCommand';
  cnstStraightenContour = 'StraightenContour';
  cnstTrailingZeros = 'TrailingZeros';
  cnstReturnToZeroPt = 'cnstReturnToZeroPoint';
  cnstMergePoints = 'MergePoints';
  cnstMergePointsRadius = 'MergePointsRadius';    
  cnstWorkpieceZeroPointID = 'WorkpieceZeroPointID';
  cnstZeroPointOffsetX = 'ZeroPointOffsetX';
  cnstZeroPointOffsetY = 'ZeroPointOffsety';
  cnstRetractTool = 'RetractTool';
  cnstToolParams = 'ToolParams';
  cnstTool = 'Tool';
  cnstTools = 'Tools';
  cnstFileExit = 'FileExt';
  cnstGFirstPt = 'FirstPoint';
  cnstGArrow = 'DirectionArrow';
  sGSelContour = 'SelectedContour';
  sLeadIn = 'LeadIn';
  sLeadOut = 'LeadOut';
  cnstVisual = 'Visual';
  cnstStartFromZeroPositon = 'StartFromZeroPositon';
  cnstColorLegend = 'ColorLegend';
  cnstColorCNCCode = 'ColorCNCCode';
  cnstDrawFirstPoint = 'DrawFirstPoint';
  cnstDrawNodePoints = 'DrawNodePoints';
  cnstDrawDirectionArrow = 'DrawDirectionArrow';
  cnstDrawSelCcontour = 'DrawSelCcontour';
  cnstDrawLeadIn = 'DrawLeadIn';
  cnstDrawLeadOut = 'DrawLeadOut';
  cnstDrawTool = 'DrawTool';
  cnstDrawStateLegend = 'DrawStateLegend';
  sContourShape: array [0..Byte(High(TsgContourShape))] of string = ('Free',
    'Linear', 'Circular', 'Point', 'Pocket');

// 3d
  cnstXMLIconIndex = 'IconIndex';

// Entity names
  cnstAcadTable = 'Table';
  cnstFont = 'Font';
  cnstPolyPolygon = 'Hatch';
  cnstGradientPolygon = 'Hatch';
  cnstCurvePolygon = 'Hatch';
  cnstClipInsert = 'Clip Insert';
  cnstFlatHatch = 'Flat Hatch';
  cnstFlatEntity = 'Flat Entity';
  cnstFlatPoly = 'Flat Poly';
  cnstHatch = 'Hatch';
  cnstMPolygon = 'MPolygon';
  cnstACISEntity = 'ACIS Entity';
  cnstAttdef = 'Attribute Definition';
  cnstAttrib = 'Attributes';
  cnstBody = 'Body';
  cnstCircle = 'Circle';
  cnstCustomVertex = 'Custom Vertex';
  cnstDimension = 'Dimension';
  cnstEllipse = 'Ellipse';
  cnstEntitySample = 'Default Values';
  cnstEvacuationDefault = 'Default Values';
  cnstInventoryDefault = 'Default Values';
  cnstInsert = 'Insert';
  cnst3DFace = 'Face';
  cnstSceneElementInsp = cnst3DFace;
  cnstMInsert = 'MInsert';
  cnstMLine = 'MLine';
  cnstWipeout = 'Wipeout';
  cnstImageEnt = 'ImageEnt';
  cnstLeader = 'Leader';
  cnstLine = 'Line';
  cnstLWPolyline = 'LW Polyline';
  cnstMText = 'MText';
  cnstObjectEntity = 'Entity';
  cnstPenEntity = 'Entity';
  cnstPenLine = 'Entity';
  cnstPointEnt = 'Point';
  cnstPolyline = 'Polyline';
  cnstRay = 'Ray';
  cnstRegion = 'Region';
  cnstSurface = 'Surface';
  cnstShape = 'Shape';
  cnstSolid = 'Solid';
  cnstCompound = 'Compound';
  cnstSpline = 'Spline';
  cnstText = 'Text';
  cnstTolerance = 'Tolerance';
  cnstTrace = 'Trace';
  cnstInsp3DFace = '3dFace';
  cnst3dSolid = '3dSolid';
  cnstXline = 'Xline';
  cnstXRef = 'XRef';
  cnstViewport = 'Viewport';
  cnstVPort = 'VPort';
  cnstOle2Frame = 'OLE';
  cnstSVGPath = 'SVG Path';
  cnstSVGMText = 'SVG MText';
  cnstSVGInsert = 'SVG Insert';
  cnstSVGContainer = 'Container';
  cnstGragient = 'Gradient';
  cnstGContour = 'Contour';
  cnstArc = 'Arc';
  cnstEntity3D = cnstObjectEntity;

  cnstInspEntNames: array [0..60] of string = ('Unknown', cnstAcadTable, cnstFont,
    cnstPolyPolygon, cnstGradientPolygon, cnstCurvePolygon, cnstClipInsert, cnstFlatHatch,
    cnstFlatEntity, cnstFlatPoly, cnstHatch, cnstMPolygon, cnstACISEntity, cnstAttdef,
    cnstAttrib, cnstBody, cnstCircle, cnstCustomVertex, cnstDimension, cnstEllipse,
    cnstEntitySample, cnstEvacuationDefault, cnstInventoryDefault, cnstInsert,
    cnst3DFace, cnstSceneElementInsp, cnstMInsert, cnstMLine, cnstWipeout, cnstImageEnt,
    cnstLeader, cnstLine, cnstLWPolyline, cnstMText, cnstObjectEntity, cnstPenEntity,
    cnstPenLine, cnstPointEnt, cnstPolyline, cnstRay, cnstRegion, cnstShape, cnstSolid,
    cnstSpline, cnstText, cnstTolerance, cnstTrace, cnstInsp3DFace, cnst3dSolid, cnstXline,
    cnstXRef, cnstViewport, cnstVPort, cnstOle2Frame, cnstSVGPath, cnstSVGMText,
    cnstSVGInsert, cnstSVGContainer, cnstGragient, cnstGContour, cnstArc
  );

//
{$IFDEF SG_VECTORIZATION}
  cnstVectorizationMode = 'VectorizationMode';
{$ENDIF}
{$IFDEF SG_BTI}
  cnstUseFillStyleIndex = 'UseFillStyleIndex';
{$ENDIF}
  cnstXMLOwner = 'Owner';
  cnstTTFMode = 'TTFMode';

  cnstXMLGuid = 'Guid';
  cnstXMLExt = '.xml';

{InputRectCoord}
  cnstXmlAppId64 = 'AppInt';
  cnstXmlFormatInfo = 'FormatInfo';
  cnstXmlFormatExt = 'Ext';
  cnstXmlAttrib = 'Attribute';
  cnstXmlAttribs = 'Attributes';
  cnstXmlPoint = 'Point';
  cnstXmlPoints = 'Points';
  cnstXmlX = 'x';
  cnstXmlY = 'y';
  cnstXmlZ = 'z';
  cnstXmlFrame = 'Frame';
  cnstXmlClipX = 'ClipX';
  cnstXmlClipY = 'ClipY';

  cnstXmlAxisX = 'AxisX';
  cnstXmlAxisY = 'AxisY';
  cnstXmlAxisZ = 'AxisZ';

  cnstAxis4Names: array[0..3] of char = ('X', 'Y', 'Z', 'W');
  cnstAxis2Names: array[0..1] of char = ('U', 'V');

{string operation constants}

  cnstColor = 'Color';
  cnstColorIndex = 'Color Index';
  cnstColors = 'Colors';
  cnstByLayer = 'By Layer';
  cnstByBlock = 'By Block';
  cnstColorRed = 'Red';
  cnstColorYellow = 'Yellow';
  cnstColorGreen = 'Green';
  cnstColorLightBlue = 'LightBlue';
  cnstColorBlue = 'Blue';
  cnstColorPurple = 'Purple';
  cnstColorWhite = 'White';
  cnstColorBlackWhite = 'Black/White';
  cnstChooseColor = 'Select color...';
  cnstColorVariesValue = '*VARIES*';
  cnstEmptyFill = 'Empty fill';
  cnstBackGroundLayerName = 'background';

  //Section Header
  cnstSectionHEADER = 'HEADER';

  //Section Entities
  cnstSectionENTITIES = 'ENTITIES';

  //Section Blocks
  cnstSectionBLOCKS = 'BLOCKS';

  //Brep constants
  cnstBrepCenterMass = 'CenterMass';
  cnstBrepVolume = 'Volume';
  cnstBrepArea = 'Area';
  cnstBrepOriginalName = 'OriginalName';
  cnstBrepBlock = 'sgBrepBlock_';

  //Section Tables
  cnstSectionTABLES = 'TABLES';
  cnstTableBLOCK_RECORD = 'BLOCK_RECORD';
  cnstTableAPPID = 'APPID';
  cnstTableDIMSTYLE = 'DIMSTYLE';
  cnstTableLAYER = 'LAYER';
  cnstTableLTYPE = 'LTYPE';
  cnstTableSTYLE = 'STYLE';
  cnstTableUCS = 'UCS';
  cnstTableVIEW = 'VIEW';
  cnstTableVPORT = 'VPORT';

  //Section Objects
  cnstObjectsCOLORS = 'COLORS';
  cnstObjectsDICTIONARY = 'DICTIONARY';
  cnstObjectsDICTIONARYVAR = 'DICTIONARYVAR';
  cnstObjectsFIELDLIST = 'FIELDLIST';
  cnstObjectsIMAGEDEFS = 'IMAGEDEFS';
  cnstObjectsLAYOUTS = 'LAYOUTS';
  cnstObjectsMATERIALS = 'MATERIALS';
  cnstObjectsMLINESTYLES = 'MLINESTYLES';
  cnstObjectsPLOTSETTINGS = 'PLOTSETTINGS';
  cnstObjectsSCALELIST = 'SCALELIST';
  cnstObjectsTABLESTYLES = 'TABLESTYLES';
  cnstObjectsMLEADERSTYLES = 'MLEADERSTYLES';
  cnstObjectsGROUPS = 'GROUPS';
  cnstSectionOBJECTS = 'OBJECTS';

  //Section ACDSDATA
  cnstSectionACDSDATA = 'ACDSDATA';
  cnstACDSSCHEMA = 'ACDSSCHEMA';
  cnstACDSRECORD = 'ACDSRECORD';

  //AcDs record names
  cnstAcDbDs = 'AcDbDs::';
  cnstAcDbDs_ID = cnstAcDbDs + 'ID';
  cnstAcDbDs_TreatedAsObjectDataSchema = cnstAcDbDs + 'TreatedAsObjectDataSchema';
  cnstAcDbDs_TreatedAsObjectData = cnstAcDbDs + 'TreatedAsObjectData';
  cnstAcDbDs_LegacySchema = cnstAcDbDs + 'LegacySchema';
  cnstAcDbDs_Legacy = cnstAcDbDs + 'Legacy';
  cnstAcDbDs_IndexedPropertySchema = cnstAcDbDs + 'IndexedPropertySchema';
  cnstAcDs_Indexable = 'AcDs:Indexable';
  cnstAcDbDs_HandleAttributeSchema = cnstAcDbDs + 'HandleAttributeSchema';
  cnstAcDbDs_HandleAttribute = cnstAcDbDs + 'HandleAttribute';
  cnstAcDb3DSolid_ASM_Data = 'AcDb3DSolid_ASM_Data';
  cnstASM_Data = 'ASM_Data';
  cnstAcDb_Thumbnail_Schema = 'AcDb_Thumbnail_Schema';
  cnstThumbnail_Data = 'Thumbnail_Data';

  //Section Classes
  cnstSectionCLASSES = 'CLASSES';

  //entities 3d
  cnstXmlVisible3D = 'Visible3D';
  cnstXmlVisible = 'Visible';
  cnstXmlHandle3D = 'Handle';
  cnstXmlPathKey = 'PathKey';
  cnstXmlBox3D = 'Box3D';
  cnstXmlCanShow = 'CanShow';
  cnstXmlVolume = 'Volume';
  cnstxmlAlphaBlend = 'AlphaBlend';
  cnstXmlColorVector = 'ColorVector';

  cnstEnumValuesDelimiter = '|';
  cnstXRefNameDelimiter = '|';
  cnstHyperLinkDelimiter = '#';
  cnstDStyle = 'DSTYLE';
  cnstMView = 'MVIEW';
  cnstAttMode = '$ATTMODE';
  cnstInternalBlockPrefix = cnstAsterisk;
//  cnstBTIBlockName: string = '*B';
  cnstDefBlockNameSymbol = Char('U');
  cnstObjects = cnstSectionOBJECTS;
  cnstDictionary = cnstObjectsDICTIONARY;
  cnstDictionaryVar = cnstObjectsDICTIONARYVAR;
  cnstGradientName: array [0..8] of string = ('CURVED', 'CYLINDER',
    'HEMISPHERICAL', 'LINEAR', 'SPHERICAL', 'INVCURVED', 'INVCYLINDER',
    'INVHEMISPHERICAL', 'INVSPHERICAL');
  cnstGroupTypeName: array [TsgGroupType] of string = ('', 'BTI', 'SVG');
  cnstROOM: string = 'TEMPLATES';
  cnstROOMAREA: string = 'AREAS OF FILE';
  cnstROOMFILE: string = 'TEMPLATES OF FILE';
  cnstInCorrectBlockPattern: string = 'Incorrect blockpattern';
  cnstShowHandleAsHex = ' Handle = %X';
  cnstDeviationCoefficientDefault = 0.01;
  cnstDeviationCoefficientRealistic = 0.0002;
  cnstLess = '<';
  cnstGreater = '>';
  cnstBackSlash = '\';
  cnstForwardSlash = '/';
  {$IFDEF SG_NON_WIN_PLATFORM}
  cnstPathSlash = cnstForwardSlash; // '/'
  {$ELSE}
  cnstPathSlash = cnstBackSlash; // '\'
  {$ENDIF}
  cnstVertSlash = '|';
  cnstCommaPoint = ';';
  cnstColon = ':';
  cnstCopyRight = Char($A9); {'?}
  cnstLeftBracket = '{';
  cnstRigthBracket = '}';
  cnstRegisteredSign = Char($AE); {'?}
  cnstSpace = ' ';
  cnstCaret = '^';
  cnstStar = '*';
  cnstBackQuote = '`';
  cnstEquals = '=';
  cnstCaretAndSpace = cnstCaret + cnstSpace;
  cnstComma = ',';
  cnstDegree = Char($B0); {'?}
  cnstPlusMinusSign = Char($B1); {'?}
  cnstPoint = '.';
  cnstPercent = '%';
  cnstAmpersant = '&';
  cnstQuestionMark = '?';
  cnstQuotationMark = '"';
  cnstExclamationPoint = '!';
  cnstEuro = Char($80); {'?}
  cnstApos = '''';
  cnstLowLine = '_';
  cnstAt = '@';
  cnstApostrophe = Char($27);

  cnstNoUseChildNodes = 'NoUseChildNodes';

  {cnstCheckSymbolAutoCadName : array[0..12] of Char = (
    cnstLess, cnstGreater, cnstForwardSlash, cnstBackSlash, cnstVertSlash,
    cnstCommaPoint, cnstComma, cnstQuestionMark, cnstQuotationMark, cnstColon,
    cnstStar, cnstEquals, cnstBackQuote);}

  sInvalidSymbolAutoCadName = cnstLess + cnstGreater + cnstForwardSlash +
    cnstBackSlash + cnstVertSlash + cnstCommaPoint + cnstComma +
    cnstQuestionMark + cnstQuotationMark + cnstColon + cnstStar + cnstEquals +
    cnstBackQuote;

  cnstInvalidSymbolAutoCadName = [cnstLess, cnstGreater, cnstForwardSlash,
    cnstBackSlash, cnstVertSlash, cnstCommaPoint, cnstComma, cnstQuestionMark,
    cnstQuotationMark, cnstColon, cnstStar, cnstEquals, cnstBackQuote];

  cnstSymbolAngle = cnstLess + ' ';
  cnstSymbolDegree = cnstDegree;

  cnstBuyLinkStart = '[BuyLink]';
  cnstBuyLinkEnd = '[/BuyLink]';
  cnstAppVersionKey = 'AppEnterprise';

  sMarkupLayerName = 'markup';
  cnstBigFontSet = 'bigfont.xml';
  cnstTypeDP = '%%';
  cnstTypeM = '\M+';
  cnstTypeU = '\U+';
  cnstXRecord = 'XRECORD';
  sACADXDataAppName = 'ACAD';
  sACADXDataAttribAppName = 'AcDbAttr';
  sURLXDataName = 'PE_URL';
  cnstCSTAppID = 'CSTINVENTORY';
  cnstCSTAppIdInternal = 'CSTABVIEWER';
  cnstCstXNameValue = 'CST_NV';
{$IFDEF SG_BTI}
  cnstInventory = 'Inventory';
  cnstBlockNameOfAreaTable = '*Z';
  cnstAttdefHandle = 'Handle';
  cnstAttdefMode = 'Mode';
  cnstAttdefUp = 'Up';
  cnstAttdefDown = 'Down';
{$ENDIF}
  sActiveVPort = '*ACTIVE';
  cnstAdskConstraints = '*ADSK_CONSTRAINTS';
  sBackSlash = cnstBackSlash;
  sByBlock = 'BYBLOCK';
  sByLayer = 'BYLAYER';
  sCircle   = 'Circle';
  sContinuous = 'CONTINUOUS';
  sCorruptedDWGFileHeader: string = 'DWG file header is corrupted';
  sDecompressDWGError: string = 'Unexpected error has occurred while DWG file '+
    'decompressing';
  sDWG2007HeaderError: string = 'Error in DWG 2007 header';
  sDWGFileError: string = 'DWG is corrupted';
  sDXFConverterIsNotInitializedError: string = 'Converter is not initialized';
  sCADImageIsNotInitializedError: string = 'CADImage is not initialized';
  sXMLInterfaceVersionError: string = 'CADSoftTools XML version is not supported';
  sXMLInterfaceHatchCreatingError: string = 'Hatch was not created';
  sXMLInterfaceFileNotFoundError: string = 'File not found';
  sXMLInterfaceObjectNotFindHandle = 'Object with handle %s not found';
  sXMLInterfaceObjectsNotFind = 'Objects with handles not found';
  sXMLInterfaceInvalidMode: string = 'Invalid Mode';
  sXMLInterfaceInvalidGuid: string = 'Invalid Guid';
  sXMLInterfaceInvalidIndex: string = 'Invalid Index';
  sXMLInterfaceInvalidHandle: string = 'Invalid Handle';
  sXMLInterfaceFileNewError: string = 'File could not be created';
  sXMLInterfaceFileLoadError: string = 'File could not be loaded';
  sXMLInterfaceComFrmError: string = 'Incorrect command format';
  sXMLInterfaceMainApplicationError: string = 'MainApplication nil';
  cnstXMLInterfaceEntityNotFoundError = 'Entity not found';
  cnstXMLInterfaceEntityNotSelectError = 'Entities not selected';
  cnstXMLInterfaceEntityNotHasAttributePoint = 'Attrubute Point not found';
  cnstXMLInterfaceSetValueNotFoundError = 'Set value not found';
  cnstXMLInterfaceLimitationOnSaveMessage = 'Limitation on saving in the demo version. If you need to save your files, please send us a request for a trial key at info@cadsofttools.com';
  sXMLInterfaceEntityNotFoundError: string = cnstXMLInterfaceEntityNotFoundError;
  sXMLInterfaceEntityNotSelectError: string = cnstXMLInterfaceEntityNotSelectError;
  sXMLInterfaceEntityNotHasAttributePoint: string = cnstXMLInterfaceEntityNotHasAttributePoint;
  sXMLInterfaceLimitationOnSaveMessage: string = cnstXMLInterfaceLimitationOnSaveMessage;
  sDSFileIsNotInitializedError: string = 'Data segment file in not initialized';
  sDSFileIdexesHeaderReadError: string = 'Indexes header read error';
  sInvalidIndexError: string = 'Index out of bounds';
  sLZ77CompressionError: string = 'LZ77 compression error';
  sDSFileSegIndexError: string = 'Segment index is invalid';
  sDWGCompilerError: string = 'Unsupported compiler version for DWG %s';
  sEllipse  = 'Ellipse';
  sEncryptedByPassword: string = 'Encrypted by password';
  sSatBinCodeError: string = 'Error SAT binary code';
  sCameraStyleIsNoSupported: string = 'Selected Camera Style is not supported';
  sCannotCreateGLBitmap: string = 'Cannot create GLBitmap';
  sGroupIsNotSupported: string = 'Group is not supported';
  sTriangleCount: string = '%d triangle(s)';
  cnstCADToGLSceneErrorWrongMode = 'Export to "%s" file is possible only if 3D View mode is active.' + #13#10 +
    'Change "Display View" to any 3D mode and try again.';
  sCADToGLSceneErrorWrongMode: string = cnstCADToGLSceneErrorWrongMode;
  cnstCADToGLSceneErrorFileNameIsAbsent = 'Filename is not specified!';
  sCADToGLSceneErrorFileNameIsAbsent: string = cnstCADToGLSceneErrorFileNameIsAbsent;
  cnstDescrSTL = 'Stereolithography files';
  cnstDescrSAT = 'Standard ACIS files (*.sat; *.sab)';
  cnstDescrXKT = 'xeokit: Web Programming Toolkit for AEC Graphics (*.xkt)';
  cnstDescrSAB = 'Spatial binary files';
  cnstDescrASM = 'Autodesk Shape Manager files (*.fsat, *.smt)';
  cnstDescrIGES = 'IGES (*.igs; *.iges)';
  cnstDescrSTEP = 'STEP (*.stp; *.step; *.stpz)';
  cnstDescrSTPZ = 'Compressed STEP files';
  cnstDescrBrep = 'BREP (*.brep)';//'Boundary reprsentation'
  cnstDescrParasolid = 'Parasolid files(*.x_t; *.x_b)';
  cnstDescrInventor = 'Autodesk Inventor Part (*.ipt)';
  cnstDescrSldPrt = 'SolidWorks Part files (*.sldprt)';
  cnstDescrSldAsm = 'SolidWorks Assembly files (*.sldasm)';
  cnstDescrOBJ = 'Wave Front model file';
  cnstDescr3DS = '3D Studio Model';
  cnstDescrPLY = 'Stanford Triangle Format vector file';
  cnstDescrVRML = 'Virtual Reality Modeling Language vector file';
  cnstDescrLMTS = 'Pulsar Studio LMTS File Format';
  cnstDescrNMF = 'NormalMapper files';
  cnstDescrPDF = 'PDF';
  cnstDescrSWF = 'Adobe Flash File Format';
  cnstDescrImages = 'Image (*.png *.jpg *.jpeg *.bmp)';
  cnstKernel3dName = 'CST Modeller v1.0';
{$IFDEF SG_USE_MCP3D}
  cnstDescrMCP3D = 'MCC-MT Files';
{$ENDIF}
{$IFDEF SG_USE_IFC}
  cnstDescrIFC = 'Industry Foundation Classes (*.ifc)';
{$ENDIF}
  sEntNamesExCount = 14;
  cnstBTIEntNamesEx: array [-1..sEntNamesExCount] of string = ('UNKNOW', 'DIMCONSTRUCTION',
    'CONSTRUCTION', 'COMPLEX', 'ELEMENT','COMPLEX BROAD', 'COMPLEX LINEAR',
    'ELEMENT BROAD','ELEMENT LINEAR', 'ELEMENT CARVED', 'AREA OBJECTS',
    'AREAS UNION', 'LABEL', 'DIMLABEL', 'BLOCK PATTERN', 'ELEMENT MODIFIER');
  sEntNamesEx: array [-1..sEntNamesExCount] of string = ('', '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '');//initialization from sBTIEntNamesEx;


  cnstBlockPatternNameDefault: array [TbtiEntity] of string =
    ('', 'TsgConstruction', 'TsgDimConstruction', 'TsgComplex',
    'TsgComplexBroad', 'TsgComplexLinear', 'TsgElement', 'TsgElementBroad',
    'TsgElementLinear','TsgElementCarved', 'TsgElementModifier', 'TsgArea',
    'TsgComplexArea', 'TsgLabel', 'TsgDimLabel', 'TsgBlockPatter');

  sgDimensionArrowTypeNames: array [TsgDimArrowType] of string =('',
    '_CLOSEDFILLED', '_CLOSEDBLANK', '_CLOSED', '_DOT', '_ARCHTICK', '_OBLIQUE',
    '_OPEN', '_ORIGIN', '_ORIGIN2', '_OPEN90', '_OPEN30', '_DOTSMALL',
    '_DOTBLANK', '_SMALL', '_BOXBLANK', '_BOXFILLED', '_DATUMBLANK',
    '_DATUMFILLED', '_INTEGRAL', '_USERARROW', '_NONE');
  sHatchEntity   = 'HATCH';
  sHttp = 'http';
  sHttps = 'https';
  sInternalDimension = 'sgDim';
  sInternalACADTable = '*TsgTbl';
  sInternalData = 'XDATA';
  sLayerDefPoints = 'DEFPOINTS';
  sLayout = 'Layout';
  sLineWeightFactor = 'LineWeightDispFactor';
  sLoadEntities: string = 'Loading entities...';
  sLoadFile: string = 'Loading file...';
  sMessageAnalysing: string = 'Performing file analysing';
  sMessageCleaning: string = 'Performing file cleaning';
  sModel  = 'Model';
  sModelSpace  = '*MODEL_SPACE';
  sModelSpace12 = '$MODEL_SPACE';
  sNodeStageUndefined = 'Undefined Stage in TsgNode.Read!';
  sPage: string = 'Page';
  sPaperSpace  = '*PAPER_SPACE';
  sPaperSpace12  = '$PAPER_SPACE';
  sPatternANSI31 = 'ANSI31';// FDiagonal, BDiagonal
  sPatternANSI37 = 'ANSI37';// DiagCross
  sPatternLINE   = 'LINE';  // Horizintal, Vertical
  sPatternNET    = 'NET';   // Cross
  sPatternSOLID  = 'SOLID';
  sPolyline = 'Polyline';
  sRasterVariables = 'RASTERVARIABLES';
  sViewTableRecord = 'VIEW';
  sUCSTableRecord = 'UCS';
  sAppIDTableRecord = 'APPID';
  sBlockHeader = 'BLOCK HEADER';
  sBlockControl = 'BLOCK CONTROL';
  sShapeFileControl = 'SHAPEFILE CONTROL';
  sViewPortEntityControl = 'VIEWPORT ENTITY CONTROL';
  cntAcDb = 'AcDb';
  cntAcad = 'ACAD';
  cnstFileNameOfShapes = 'sgShapes.shx';
  cnstFileNameForbiddenChars = ['<', '>', '|', '"', '\', '/', ':', '*', '?'];
  sNamedObjectsDictionary = 'DICTIONARY (NAMED OBJECTS)'; //see DWG documentation
  sAcadXDictionary = cntAcad + '_XDICTIONARY';
  sAcadReactors = cntAcad + '_REACTORS';
  sBlkRefs = 'BLKREFS';
  sAcadGroupDictionary = cntAcad + '_GROUP';
  sAcadColor = cntAcad + '_COLOR';
  sAcadMaterialDictionary = cntAcad + '_MATERIAL';
  sAcadWipeOutVarsDictionary = cntAcad + '_WIPEOUT_VARS';
  sAcadImageDict = cntAcad + '_IMAGE_DICT';
  sAcadImageVars = cntAcad + '_IMAGE_VARS';
  sMLineStyle = 'MlineStyle';
  sPlotSettingsMarker = 'PlotSettings';
  sXrecord = 'Xrecord';
  sAcDbPlotSettings = cntAcDb + sPlotSettingsMarker;
  sAcadMLineStyleDictionary = cntAcad + '_MLINESTYLE';
  sAcadLayoutDictionary = cntAcad + '_LAYOUT';
  sAcadScaleListDictionary = cntAcad + '_SCALELIST';
  sAcDbVariableDictionary = cntAcDb + 'VariableDictionary';
  sAcadMLeaderStyleDictionary = cntAcad + '_MLEADERSTYLE';
  sAcadMLeaderVer = cntAcad + '_MLEADERVER';
  sAcadPlotSettingsDictionary = cntAcad + '_PLOTSETTINGS';
  sAcadFieldListDictionary = cntAcad + '_FIELDLIST';
  sAcadFieldDictionary = cntAcad + '_FIELD';
  sAcadTableStyleDictionary = cntAcad + '_TABLESTYLE';
  sAcadDStyle = 'DSTYLE';
  sDictionaryVarSORTENTS = 'SORTENTS';
  sDictionaryVarOLEFRAME = 'OLEFRAME';
  sDictionaryVarXCLIPFRAME = 'XCLIPFRAME';
  sDictionaryAcadDataLink = cntAcad + '_DATALINK';
  sAcadSortEnts = cntAcad + '_' + sDictionaryVarSORTENTS;
  sAcDbXrecord = cntAcDb + sXrecord;
  sAcDbColor = cntAcDb + 'Color';
  sDWGPropsDictionary = 'DWGPROPS';
  sDWGPropsXRecord = 'DWGPROPS COOKIE';
  sAcadXRecRoundTrip = sACADXDataAppName + '_XREC_ROUNDTRIP';
  sAcadFilterDictionary = cntAcad + '_FILTER';
  cnstSpatial = 'SPATIAL';
  cnstSPATIAL_FILTER = 'SPATIAL_FILTER';
  cnstSpatialFilterMarker = 'SpatialFilter';
  sAcIdBlockReference = 'ACIDBLOCKREFERENCE';
  sRedlineLayerName = 'Redline_layer';
  cnstAcadMLAtt = cntAcad + '_MLATT';
  cnstAcadMTextBBRT = 'ACAD_MTEXT_BBRT';
  cnstAcadMTextBERT = 'ACAD_MTEXT_BERT';
  cnstAcadMTextRT = 'ACAD_MTEXT_RT';
  cnstAcadMText2008RT = 'ACAD_MTEXT_2008_RT';
  cnstAcadFilter = 'ACAD_FILTER';
  cnstMTextBegin = 'MTEXTBEGIN';
  cnstMTextEnd = 'MTEXTEND';
  cnstASDK_XREC_ANNO_SCALE_INFO = 'ASDK_XREC_ANNO_SCALE_INFO';
  cnstASDK_XREC_ANNOTATION_SCALE_INFO = 'ASDK_XREC_ANNOTATION_SCALE_INFO';
  cnstADSK_XREC_SUBDVERTEXTEXCOORDS = 'ADSK_XREC_SUBDVERTEXTEXCOORDS';
  cnstAcDbAnnotationScales = 'ACDB_ANNOTATIONSCALES';
  cnstAcadAnnotative = 'AcadAnnotative';
  cnstAnnotativeData = 'AnnotativeData';
  cnstEmbeddedObject = 'Embedded Object';
  sAcDbContextDataManager = cntAcDb + 'ContextDataManager';
  sAcDbMLeader = cntAcDb + 'MLeader';

  // IRD objects constants
  cnstIrdDscDict = 'IRD_DSC_DICT';
  cnstIrdDscRecord = 'IRD_DSC_RECORD';
  cnstIrdObjRecord = 'IRD_OBJ_RECORD';
  cnstCIrdBaseRecord = 'CIrdBaseRecord';
  cnstCIrdDscRecord = 'CIrdDscRecord';
  cnstCIrdObjRecord = 'CIrdObjRecord';
  sIrdDscDictionary = 'IrdDscDictionary'; // name of dictionary
  cnst_ird_data = 'ird_data'; // do to localize for internal using

  sRSBlockSizeError: string = 'Block size error';
  sRSDecoderError: string = 'RS decoder error. DWG file is corrupted!';
  sRSMaxErrors: string = 'The number of errors cannot be more than %d';
  sExporterSoftwareInfo: string = 'The file was created in CADSoftTools software';
  sCreatedFileInfo: string = 'Created by Soft Gold Ltd. www.cadsofttools.com';
  sWebAddress: string = 'www.cadsofttools.com';
  sCopyrightCADNavigator =  'Copyright ? %s CADSoftTools. All rights reserved.';

  sSOLID = 'SOLID';
  sStandardName = 'STANDARD';
  sStringBreak = #1;
  sSlash = cnstForwardSlash;
  sSlashes = '://';
  sTempFileName = '~cs_temp_file_.';

  cnstUnknown = 'Unknown';
  sUNKNOWN: string = cnstUnknown;
  sUnsupportedNewDWGVer: string = 'Unsupported new DWG version ID:';
  sUnsupportedOldDWGVer: string = 'Unsupported old DWG version ID:';
  sUnsupportedDWGVer: string = 'Unsupported DWG version ID:';
  sInvalidVersion: string = 'Unsuported version';
  sHatchMemoryLimit: string = 'Hatch Memory Limit';
  sHatchTimeLimit: string = 'Hatch Time Limit';
  sHatchOverflow: string = 'Hatch Parameters Overflow';
  sIncorrectNameLayer: string = 'Incorrect Layer Name';
  cnstSymbolUnicode = '\U+';
  cnstLoadError = 'Loading error';
  cnstParsingError = 'Parsing error';
  cnstExporterSoftwareInfo = 'The file was created in CADSoftTools software';
  cnstCreatedFileInfo = 'Created by Soft Gold Ltd. www.cadsofttools.com';
  cnstMaxOLEStream = 1024 * 1024 * 7;
  cnstSceneElement = 'SceneElement';

  cnstVisualRound: Boolean = False;

  cnstEntClass_BlockPattern_ID = 3841;

  GUID_XMLObject: TGUID = ();
  GUID_InputRectCoords: TGUID = ();//'{00000000-0000-0000-0000-000000000000}';

{$IFDEF SG_BTI}
  cnstBTITemplateTypeOpt = 'BTITemplateType';
  cnstInfoSeporator = '%';
  cnstLeftSeporator = cnstInfoSeporator + cnstInfoSeporator;
  cnstRightSeporator = cnstInfoSeporator + cnstInfoSeporator;
  cnstActiveAxis: TsgAxisType = atLeft;
  cnstActivePoint: TsgActivePoint = apLeftTop;
  cnstAreaFigues: array [TsgFigureType] of string =
    ('Figure', 'Quadrangle', 'Trapezium', 'Parallelogram', 'Rect', 'Square',
     'Rhomb', 'Segment by chord and length', 'Segment by chord and height',
     'Triangle', 'Right triangle', 'Triangle by hippuran', 'Circle by length',
     'Circle by radius', 'Sector by angle and radius', 'Contour');

  cnstParamType: array [TsgParamStrType] of string =
   (cnstEnumValuesDelimiter + 'AREA',
    cnstEnumValuesDelimiter + 'AREAOWNER',

    cnstEnumValuesDelimiter + 'NUMBER',
    cnstEnumValuesDelimiter + 'NUMBEROWNER',

    cnstEnumValuesDelimiter + 'SUBNUMBER',
    cnstEnumValuesDelimiter + 'SUBNUMBERONWER',

    cnstEnumValuesDelimiter + 'LETNUMBER',
    cnstEnumValuesDelimiter + 'LETNUMBERONWER',

    cnstEnumValuesDelimiter + 'PERIMETER',
    cnstEnumValuesDelimiter + 'PERIMETERONWER',

    cnstEnumValuesDelimiter + 'VALUE',
    cnstEnumValuesDelimiter + 'VALUEOWNER',

    '');
  cnstParamTypeATTR: string = cnstEnumValuesDelimiter + 'ATTR_';
  cnstParamTypeATTROwner: string = cnstEnumValuesDelimiter + 'ATTROWNER_';

  cnstDefaultValueMask: string = cnstLeftSeporator  + cnstEnumValuesDelimiter +
    'NUMBER' + cnstRightSeporator;
  cnstParamTypeNumberAndLetterMask: string = cnstLeftSeporator +
    cnstEnumValuesDelimiter + 'NUMBER' + cnstRightSeporator + cnstLeftSeporator +
    cnstEnumValuesDelimiter + 'LETNUMBER' + cnstRightSeporator;

  cnstParamTypeNumberOwnerAndNumber: string = '%%|NUMBEROWNER%%-%%|NUMBER%%';
  cnstParamTypeNumber:  string = '%%|NUMBER%%';

  sBoundaryVisible = 'BoundaryVisible';
  sGeron: string = 'Geron';
  sRadin: string = 'Radian';
  sSegmentAH: string = 'SegmentAH';
  sSegmentAL: string = 'SegmentAL';
  sSin: string = 'Sin';
  sSqr: string = 'Sqr';
  sSqrt: string = 'Sqrt';

  cnstBTIExtDataVersion: TsgVersion = (Major: 1; Minor: 0);
  cnstBTICustomFieldsVersion: TsgVersion = (Major: 1; Minor: 0);

  cnstTemplateType_Base = 0;
  cnstTemplateType_Habarovs = 1;
  cnstTemplateType_Moldova = 2;
  cnstTemplateType_AllVersion = 3;
  cnstTemplateType_Valmaster = 4;
  cnstTemplateType_Germany = 5;
  cnstTemplateType_Russian = 100;

  cnstBTITemplateType: Integer =
  {$IFDEF SG_BTI_MOLDOVA}
   cnstTemplateType_Moldova
  {$ELSE}
    {$IFDEF SG_VALMASTER}
    cnstTemplateType_Valmaster
    {$ELSE}
      {$IFDEF SG_BTI_RUS}
      cnstTemplateType_Russian
      {$ELSE}
      cnstTemplateType_AllVersion
      {$ENDIF}
    {$ENDIF}
  {$ENDIF};

{$ENDIF}
//
  cntAutoCAD = 'AutoCAD';
  cnstDefaultVisualDxf10 = cntAutoCAD + ' R10 DXF (*.dxf)';//
  cnstDefaultVisualDxf2000 = cntAutoCAD + ' 2000 DXF (*.dxf)';
  cnstDefaultVisualDxf2004 = cntAutoCAD + ' 2004 DXF (*.dxf)';
  cnstDefaultVisualDxf2007 = cntAutoCAD + ' 2007 DXF (*.dxf)';
  cnstDefaultVisualDxf2018 = cntAutoCAD + ' 2018 DXF (*.dxf)';

  cnstSaveCADVersionClipboard =  acR2004;
  cnstSaveCADVersionDefault = {$IFDEF SG_CLIENT_CLNOFSFL}acR10{$ELSE}cnstSaveCADVersionClipboard{$ENDIF};
  cnstSaveCADVersionMoldova = acR2007;
  cnstCADVersionsSupported: array [Boolean] of TsgDWGVersions =//DWG = True
    ([{$IFDEF SG_CLIENT_CLNOFSFL}acR10{$ELSE}acR2000{$ENDIF}, acR2004, acR2007, acR2010, acR2013, acR2018],
     [acR2000, acR2004, acR2010, acR2013, acR2018]);

{lisp text constants}

const
  cnstLispEnterFirstPoint = 'Enter first point';
  sLispEnterFirstPoint: string = cnstLispEnterFirstPoint;
  cnstLispEnterPoint = 'Enter point';
  sLispEnterPoint: string = cnstLispEnterPoint;
  cnstLispEnterNextPoint = 'Enter next point';
  sLispEnterNextPoint: string = cnstLispEnterNextPoint;

  cnstLispErrorCastAsBool = 'Lisp casting error: AsBool';
  sLispErrorCastAsBool: string = cnstLispErrorCastAsBool;
  cnstLispErrorCastAsInteger = 'Lisp casting error: AsInteger';
  sLispErrorCastAsInteger: string = cnstLispErrorCastAsInteger;
  cnstLispErrorCastAsList = 'Lisp casting error: AsList';
  sLispErrorCastAsList: string = cnstLispErrorCastAsList;
  cnstLispErrorCastAsReal = 'Lisp casting error: AsReal';
  sLispErrorCastAsReal: string = cnstLispErrorCastAsReal;
  cnstLispErrorTooFewArguments = 'Error: too few arguments';
  sLispErrorTooFewArguments: string = cnstLispErrorTooFewArguments;
  cnstLispErrorTooManyArguments = 'Error: too many arguments';
  sLispErrorTooManyArguments: string = cnstLispErrorTooManyArguments;
  cnstLispErrorUnknownFunction = 'Error: no function definition';
  sLispErrorUnknownFunction: string = cnstLispErrorUnknownFunction;

{accuracy constants}

  cnstMinDist = 0.001;//0.01;
  cnstSqrMinDist = cnstMinDist * cnstMinDist;
  fAccuracy = 0.000001; //It is necessary for comparison floating-point numbers
  fDoubleResolution = 1E-09;//1E-15;
  fExtendedResolution = 1E-15;
  fMaxDoubleValue = 1E99;
  fMinDoubleValue = -1E99;
  fMaxResolution = 1E-100; // for invert matrix and calc matrix scale
  fDivDoubleResolution = 1/fDoubleResolution;

{$IFNDEF SGDEL_6}
  NaN         =  0.0 / 0.0;
  (*$EXTERNALSYM NaN*)
  (*$HPPEMIT 'static const Extended NaN = 0.0 / 0.0;'*)
{$ENDIF}


{Seporators}
  cnstColorLWeightDelimiter = '=';
  cnstColorLWeightValSeparator = '.';
  cnstColorLWeightValuesDelimiter = ';';
  cnstColorLWeightZero = '0' + cnstColorLWeightValSeparator + '00';
  cnstColorLWeightBadKey = TColor($EFFFFFFF);
  cnstColorCADLWeightBadKey: TsgColorCAD = (Active: acRGBColor; Color: Cardinal(cnstColorLWeightBadKey); AlbumString: '');
  cnstColorLWeightColorNewEmpty = $7FFFFFFF;//MaxInt
  cnstColorCADLWeightColorNewEmpty: TsgColorCAD = (Active: acRGBColor; Color: cnstColorLWeightColorNewEmpty; AlbumString: '');

 cnstPenWidthNull = {$IFDEF SG_FIREMONKEY}1{$ELSE}0{$ENDIF};

{Group Codes in Numerical Order}
  Info_0 = 0;
  Text_1 = 1;
  Name_2 = 2;
  Text_3 = 3;
  Text_4 = 4;
  Text_5 = 5;
  LineTypeName_6 = 6;
  TextFont_7 = 7;
  LayerName_8 = 8;
  VarName_9 = 9;
  XFirst_10 = 10;
  XOther_11 = 11;
  XOther_12 = 12;
  XOther_13 = 13;
  XOther_14 = 14;
  XOther_15 = 15;
  XOther_16 = 16;
  XOther_17 = 17;
  XOther_18 = 18;
  YFirst_20 = 20;
  YOther_21 = 21;
  YOther_22 = 22;
  YOther_23 = 23;
  YOther_24 = 24;
  YOther_25 = 25;
  YOther_26 = 26;
  YOther_27 = 27;
  YOther_28 = 28;
  ZFirst_30 = 30;
  ZOther_31 = 31;
  ZOther_32 = 32;
  ZOther_33 = 33;
  ZOther_34 = 34;
  ZOther_35 = 35;
  ZOther_36 = 36;
  ZOther_37 = 37;
  Elevation_38 = 38;
  TableOfEntity_39 = 39;
  Float_40 = 40;
  Float_41 = 41;
  Float_42 = 42;
  Float_43 = 43;
  Float_44 = 44;
  Float_45 = 45;
  Float_46 = 46;
  Float_47 = 47;
  Float_48 = 48;
  FloatRepeated_49 = 49;
  Angle_50 = 50;
  Angle_51 = 51;
  Angle_52 = 52;
  Angle_53 = 53;
  Angle_54 = 54;
  Angle_55 = 55;
  Angle_56 = 56;
  Angle_57 = 57;
  Angle_58 = 58;
  Visible_60 = 60;
  ColorNumber_62 = 62;
  Integer_63 = 63;
  Integer_64 = 64;
  Integer_65 = 65;
  UCSVP_65 = Integer_65;
  EntitysBegin_66 = 66;
  PaperSpace_67 = 67;
  Integer_68 = 68;
  Integer_69 = 69;
  Integer_70 = 70;
  Integer_71 = 71;
  Integer_72 = 72;
  Integer_73 = 73;
  Integer_74 = 74;
  Integer_75 = 75;
  Integer_76 = 76;
  Integer_77 = 77;
  Integer_78 = 78;
  Integer_79 = 79;
  Integer_90 = 90;
  Integer_91 = 91;
  Integer_92 = 92;
  Integer_93 = 93;
  Integer_94 = 94;
  Integer_97 = 97;
  Integer_99 = 99;
  String_100 = 100;
  XOther_110 = 110;
  XOther_111 = 111;
  XOther_112 = 112;
  YOther_120 = 120;
  YOther_121 = 121;
  YOther_122 = 122;
  ZOther_130 = 130;
  ZOther_131 = 131;
  ZOther_132 = 132;
  Float_140 = 140;
  Float_141 = 141;
  Float_142 = 142;
  Float_143 = 143;
  Float_144 = 144;
  Float_145 = 145;
  Float_146 = 146;
  Float_147 = 147;
  Float_148 = 148;
  Float_149 = 149;
  Integer_170 = 170;
  Integer_171 = 171;
  Integer_172 = 172;
  Integer_173 = 173;
  Integer_174 = 174;
  Integer_175 = 175;
  Integer_176 = 176;
  Integer_177 = 177;
  Integer_178 = 178;
  Float_211 = 211;
  Extrusion_210 = 210;
  Float_221 = 221;
  Extrusion_220 = 220;
  Extrusion_230 = 230;
  Float_231 = 231;
  Float_239 = 239;
  Integer_271 = 271;
  Integer_276 = 276;
  Integer_277 = 277;
  Integer_278 = 278;
  Word_280 = 280;
  Word_281 = 281;
  Word_282 = 282;
  Word_283 = 283;
  Word_284 = 284;
  Word_285 = 285;
  Word_286 = 286;
  Word_287 = 287;
  Word_288 = 288;
  Word_289 = 289;
  Integer_290 = 290;
  Integer_291 = 291;
  String_302 = 302;
  String_309 = 309;
  String_310 = 310;
  String_330 = 330;
  String_331 = 331;
  String_340 = 340;
  String_341 = 341;
  String_342 = 342;
  Integer_370 = 370;
  Integer_400 = 400;
  String_410 = 410;
  String_411 = 411;
  String_412 = 412;
  String_413 = 413;
  String_414 = 414;
  String_415 = 415;
  Integer_420 = 420;//32-bit integer value. When used with True Color;
  Integer_421 = 421;//a 32-bit integer representing a 24-bit color value:
  Integer_422 = 422;//0x00RRGGBB
  Integer_423 = 423;
  Integer_424 = 424;
  Integer_425 = 425;
  Integer_426 = 426;
  Integer_427 = 427;
  Integer_428 = 428;
  Integer_429 = 429;
  String_430 = 430;
  String_431 = 431;
  String_432 = 432;
  String_433 = 433;
  String_434 = 434;
  String_435 = 435;
  String_436 = 436;
  String_437 = 437;
  String_438 = 438;
  String_439 = 439;
  Integer_441 = 441;//440-447 32-bit integer value. When used for True Color, the transparency value
  String_1000 = 1000;
  String_1001 = 1001;
  String_1002 = 1002;
  String_1003 = 1003;
  String_1005 = 1005;
  Float_1010 = 1010;
  Float_1020 = 1020;
  Float_1030 = 1030;
  Float_1040 = 1040;
  Float_1041 = 1041;
  Float_1042 = 1042;
  Float_1043 = 1043;
  Integer_1070 = 1070;
  Integer_1071 = 1071;
  UserDigit = 23111;

  cnstEEDInternalVersion = 1;

{$IFDEF SG_BTI}
  cnstBTExtDataCode = String_438;
  cnstBTExtDataCodeCRC = Integer_99;
{$ENDIF}

{consts by export}

  cnstOutFileInfo: Boolean = True;

{consts record}
  cnstDefHPGapTol =  0.005;

  cnstDefHeadVarStruct: TsgHeadVarStruct =
    (
      AnalogyDistance: 0;
      CEColor: (Active: acIndexColor; Color: clDXFByLayer; AlbumString: '');// cnstDefaultEntityColor;
      CLayer: '0';
      CELType: sByLayer;
      CELTScale: 1;
      CELWeight: -1.0;
      CodePage: 0;
      DimStyle: sStandardName;
      DimTextStyle: sStandardName;
      DimProps: (
        Alt: False;
        AltF: 1;
        APost: '';
        Asz: 0.18;
        Sah: False;
        Arrows: (Blk: datClosedfilled; Blk1: datClosedfilled; Blk2: datClosedfilled; LrBlk: datClosedfilled);
        Cen: 0.09;
        ClrD: (Active: acIndexColor; Color: clDXFByBlock; AlbumString: '');
        ClrE: (Active: acIndexColor; Color: clDXFByBlock; AlbumString: '');
        ClrT: (Active: acIndexColor; Color: clDXFByBlock; AlbumString: '');
        SD1: False;
        SD2: False;
        SE1: False;
        SE2: False;
        Dec: 4;
        Exe: 0.18;
        Exo: 0.0625;
        Gap: 0.0625;
        LFac: 1;
        LwD: -1.0;
        LwE: -1.0;
        Post: '';
        Scale: 1.0;
        Tad: 0;
        Tih: True;
        Tix: 0;
        Toh: True;
        Txt: 0.18;
        Tp: 0;
        Tm: 0;
        LUnit: luDecimal;
        DSep: cnstPoint;
         );
      ExtMin: (X: 0; Y: 0; Z: 0);
      ExtMax: (X: 0; Y: 0; Z: 0);
      PExtMin: (X: 0; Y: 0; Z: 0);
      PExtMax: (X: 0; Y: 0; Z: 0);
      LimMin: (X: 0; Y: 0; Z: 0);
      LimMax: (X: 0; Y: 0; Z: 0);
      PLimMin: (X: 0; Y: 0; Z: 0);
      PLimMax: (X: 0; Y: 0; Z: 0);
      TextStyle: sStandardName;
      FilletRadius: 0.5;
      InsUnits: Byte(iuMillimeters);
      LTScale: 1;
      Measurement: False;
      PointDisplayMode: 0;
      PointDisplaySize: 0;
      TextSize: 0.18;
      TileMode: 1;
      UCSORG:  (X: 0; Y: 0; Z: 0);
      UCSXDir: (X: 1; Y: 0; Z: 0);
      UCSYDir: (X: 0; Y: 1; Z: 0);
      Version: Ord(acR2004);
      InsBase: (X: 0; Y: 0; Z: 0);
      AttMode: attNormal;
      XClipFrame: 2;
      DimAssoc: 2;
      FillMode: True;
      LwDisplay: 0;//
    );
{$IFDEF SGFPC}
  cnstSGHeadVarStruct: TsgHeadVarStruct = ({%H-});
{$ELSE}
  cnstSGHeadVarStruct: TsgHeadVarStruct = ();
{$ENDIF}

  cnstHPGapTolOpt = 'HPGAPTOL';
  cnstCADVariables: TsgCADVariables = (HPGapTol: cnstDefHPGapTol;);

  cnstSolidSelection: Boolean =  True;
  cnstSelectRasterMode: Integer = 0;
  bCachingRasterMode: TsgCachingRasterMode = crmCache;
  cnstBorderType: TsgBorderType = btRatio;
  cnstFontParamsZero: TsgFontParams = (Above: 0; Below: 0; Width: 0; Height: 0;
    Left: 0; Bottom: 0; Right: 0; Top: 0; After: 0; DY: 0; DX: 0);
  cnstViewBoxZero: TsgViewBox = (Mode: []; X: 0; Y: 0; Width: 0; Height: 0);
  cnstIniHeadVar: TsgLittleHeadVarStruct = (InitFlags: []; DimDec: 2;
    DimLFac: 0.001; DimTxt: 150; DimTextAlign: 0;
    DimClrT:(Active: acIndexColor; Color: clDXFByBlock; AlbumString: ''); DimGap: 100);
  cnstSnapMaskAll: TObjectSnapState =[osInsert, osEndPt, osCenter, osNode,
    osMiddle, osNearest, osIntersection, osNormal, osParallel];
  cnstSnapMaskNone: TObjectSnapState = [osDisabled];
  cnstSnapMemoryCash: Integer = 10000;
  cnstProxyInfo: TsgHTTPProxyInfo = (
    ProxyHost: '';
    ProxyPort: 0;
    ProxyNeedPass: False;
    ProxyUser: '';
    ProxyPass: '' );


{XML const DATA}
const
  cnstXML_UNSUPPORTED = -1;
  cnstXML_OK = 0;
  cnstXML_ERROR = 1;
  cnstXML_INCORRECT_PDMODE = 2;

  iRoundDigits = 9;

  cnstXMLHeader     = cnstSectionHEADER;
  cnstXMLTables     = cnstSectionTABLES;
  cnstXMLBlocks     = cnstSectionBLOCKS;
  cnstXMLEntities     = cnstSectionENTITIES;
  cnstXMLObjects     = cnstSectionOBJECTS;

  cnstXMLEntity = 'Entity';


  cnstXMLRootNode = 'cadsofttools';

  cnstXMLUnsupported = 'Unsupported';
  cnstXMLNotCorrectName= 'Not the correct name.';
  cnstXMLLineWeigthValueError = 'The lineweight value is out of range.';
  cnstXMLGlobalWidthValueError = 'The globalwidth value is out of range.';
  cnstXMLItemType: array [Boolean] of string= ('Attribute', 'Child');

type
  TsgXMLId = (xmlUndefined, xmlLineWeight, xmlFlags, xmlLineTypeScale,
  xmlLayerName, xmlLineTypeName, xmlName, xmlHandle, xmlSubEntities, xmlColor,
  xmlVisible, xmlLocked, xmlPoint, xmlBasePoint, xmlBulge, xmlStartWidth,
  xmlEndWidth, xmlPoint1, xmlLength, xmlBlockName, xmlExtrusion, xmlAngle,
  xmlScale, xmlAttribs, xmlHeight, xmlRotation, xmlObliqueAngle, xmlText,
  xmlStyleName, xmlAttribTag, xmlAttribValue, xmlArrowSize, xmlPoint2, xmlPoint3,
  xmlControlPoints, xmlCount, xmlCountU, xmlCountV, xmlKnotsPoints, xmlWeights, xmlFitPoints,
  xmlStartTangentVector, xmlEndTangentVector, xmlDegree, xmlDegreeU, xmlDegreeV, xmlKnot, xmlWeight,
  xmlRadius, xmlMajRadius, xmlMinRadius, xmlCircleLength, xmlArea, xmlEndAngle, xmlStartAngle, xmlRadPt,
  xmlRatio, xmlLineAngle, xmlOffset, xmlIsSolid, xmlLines, xmlDash,
  xmlStartPoint, xmlEndPoint, xmlCenterPoint, xmlEndParam, xmlStartParam,
  xmlMajorPoint, xmlColumnCount, xmlRowCount, xmlColSpacing, xmlRowSpacing,
  xmlRectWidth, xmlTextValue, xmlHatchName, xmlSolidFill, xmlWidth,
  xmlElevation, xmlPatternAngle, xmlPatternScale, xmlFileName,
  xmlDimScaleOverall, xmlJustify, xmlStart, xmlTurns, xmlTurnHeight,
  xmlClockwise, xmlACADVer, xmlCodePage, xmlTextStyle, xmlCLayer, xmlCELType,
  xmlCEColor, xmlCELTScale, xmlCELWeight, xmlDimStyle, xmlDimTextStyle,
  xmlDimBlk, xmlDimBlk1, xmlDimBlk2, xmlDimRBlk,
  xmlDimAlt, xmlDimAltF, xmlDimAPost, xmlDimAsz, xmlDimSah, xmlDimCen,
  xmlDimClrD, xmlDimClrE, xmlDimClrT, xmlDimSD1, xmlDimSD2, xmlDimSE1,
  xmlDimSE2, xmlDimDec, xmlDimExe, xmlDimExo, xmlDimGap, xmlDimLFac, xmlDimLwD,
  xmlDimLwE, xmlDimPost, xmlDimScale, xmlDimTad, xmlDimTih, xmlDimTix,
  xmlDimToh, xmlDimTxt, xmlDimTp, xmlDimTm, xmlDimDSep, xmlDimLUnit, xmlDimFrac,
  xmlDimLRBLK, xmlDimADEC, xmlDimDLI, xmlDimRND, xmlDimDLE, xmlDimFXL, xmlDimTVP,
  xmlDimTFAC, xmlDimALTRND, xmlDimTFILLCLR, xmlDimTOL, xmlDimLIM, xmlDimZIN,
  xmlDimAZIN,xmlDimALTD, xmlDimTOFL, xmlDimSOXD, xmlDimTDEC, xmlDimALTU,
  xmlDimALTTD, xmlDimAUNIT, xmlDimTMOVE, xmlDimJUST, xmlDimTOLJ, xmlDimTZIN,
  xmlDimALTZ, xmlDimALTTZ, xmlDimUPT, xmlDimATFIT, xmlDimFXLON, xmlDimTFILL,
  xmlDimENBL, xmlDimTXTALIGN, xmlDimLTY, xmlDimEX1LTY, xmlDimEX2LTY, xmlDimTSZ,
  xmlExtMim,
  xmlExtMax, xmlFilletRadius, xmlFillMode, xmlLwDisplay, xmlInsUnits,
  xmlLTScale, xmlMeasurement, xmlPointDisplayMode, xmlPointDisplaySize,
  xmlTextSize, xmlTileMode, xmlUCSORG, xmlUCSXDir, xmlUCSYDir, xmlInsBase,
  xmlAttributeMode, xmlHeader, xmlTables, xmlBlocks, xmlEntities, xmlObjects,
  xmlPattern, xmlPatterns, xmlBoundary, xmlBoundaries,
  xmlPSpaceCenter, xmlMSpaceCenter, xmlViewDirection, xmlViewTarget,
  xmlPSpaceWidth, xmlPSpaceHeight, xmlFrontClipPlane, xmlBackClipPlane,
  xmlMSpaceHeight, xmlViewTwistAngle, xmlStatusField, xmlThisID,
  xmlClippingBoundaryHandle, xmlExtData, xmlHandleSave, xmlCircleZoomPercent,
  xmlUCSOrigin, xmlUCSVP, xmlViewAspectRatio, xmlViewCenterPoint, xmlViewHeight,
  xmlImageDef, xmlVertex, xmlVertexes, xmlClosed, xmlPeriodic, xmlRational,
  xmlPaperSpaceBlock, xmlUVector, xmlVVector, xmlSize, xmlCalc, xmlMode,
  xmlArrowhead, xmlLeaderPathType, xmlProperties, xmlDictionaryValue,
  xmlFPointScale, xmlZThick, xmlFrozen, xmlFrozenByNewViewPort, xmlXrefLink,
  xmlIsPlotting, xmlBigFont, xmlFixedHeight, xmlFontName, xmlFontStyle,
  xmlLastHeightUsed, xmlPrimaryFont, xmlTextGenFlags, xmlWidthFactor,
  xmlGlobalWidth, xmlArrowType,
  xmlArcDefPoint, xmlDefPoint, xmlLinDefPoint1, xmlLinDefPoint2, xmlMiddlePoint, xmlRadDefPoint,
  xmlTransparency, xmlTransparentColor, xmlDescription, xmlLayoutScale, xmlModelScale,
  {$IFDEF SG_BREP_TOPOLOGY}
  xmlVolumeOCC, xmlAreaOCC, xmlCenterMassOCC, xmlOriginalNameOCC,
  {$ENDIF}
  xmlDistance, xmlAngleInXY, xmlAngleFromXY, xmlDeltaX, xmlDeltaY, xmlDeltaZ,
  xmlScaleFactor, xmlPerimeter, xmlSegmentIndex, xmlPolylineLength,
  xmlMatProp, xmlHyperLink, xmlTextOverride, xmlTextRotation, xmlGradientTypeName,
  xmlGradientAngle, xmlGradientOneColor, xmlGradientTwoColor, xmlGradientUseCenter,
  xmlBackgroundColor, xmlBackgroundFlags, xmlBoundaryPolylines, xmlBoundaryPolyline,
  xmlEedList, xmlApplication, xmlItem, xmlEedData, xmlEedType, xmlEedCode,

  xmlPageSetupName, xmlPrintOrConfigName, xmlPlotLayoutFlags, xmlPlotPaperUnits,
  xmlStandardScaleType, xmlNumeratorOfCustomPrintScale, xmlDenominatorOfCustomPrintScale,
  xmlPaperSizeName, xmlPlotViewName, xmlUnprintableMargin, xmlPlotPaperSize,
  xmlPlotOrigin, xmlPlotWindowAreaMin, xmlPlotWindowAreaMax, xmlPlotRotation,
  xmlPlotType, xmlCurrentStyleSheet, xmlShadePlotMode, xmlShadePlotResolutionLevel,
  xmlShadePlotCustomDPI, xmlFloatingPointScaleFactor, xmlPaperImageOrigin,
  xmlFillStyleIndex, xmlEntData, xmlBox, xmlDimAssoc, xmlValue,
  xmlHAlign, xmlVAlign, xmlX, xmlY, xmlZ, xmlAxisX, xmlAxisY, xmlAxisZ,
  xmlAxisU_2D, xmlAxisV_2D, xmlCenterPoint_2D, xmlClipRect, xmlTurnsOff,
  xmlPointsCount, xmlCanShow, xmlInsertsCount, xmlShapesCount, xmlSolidsCount,
  xmlPathKey, xmlSinAngle, xmlCosAngle, xmlHelixStep, xmlFocLength, xmlKx, xmlKy,
  xmlDirection, xmlNormal, xmlTangent, xmlParamT, xmlUVPoint, xmlDeflection,
  xmlInterval, xmlInterval1, xmlInterval2, xmlInterval2d, xmlInterval3d, xmlSense, xmlIsPrimary,
  xmlLinTolerance, xmlAngTolerance, xmlMeshLinPrecision, xmlMeshAngPrecision, xmlIsoLinesU, xmlIsoLinesV,
  xmlColorVector, xmlFillColor, xmlAlign, xmlVisible3D, xmlBox3D, xmlVolume,
  xmlAlphaBlend, xmlPolypoints, xmlColorCustom, xmlBinaryData,
  xmlComplexType, xmlIndex, xmlMiter,
  //MLedaer
  xmlLeaderLineType, xmlLineColor, xmlLanding, xmlLandingGap,
  xmlDogLeg, xmlDogLegLen, xmlContentType, xmlBlock,
  xmlBlockColor, xmlBlockScale, xmlBlockRotation, xmlBlockNormal,
  xmlBlockLocation, xmlBlockMatrix, xmlBlockConnectionType,
  xmlUseDefMText,  xmlTextColor, xmlTextHeight, xmlTextAngleType, xmlTextLabel,
  xmlTextFrame, xmlTextAlignment, xmlTextSwitchAlignment, xmlTextAttachmentDirection,
  xmlLeftAttachment, xmlRightAttachment, xmlTopAttachment, xmlBottomAttachment,
  xmlMLeaderOrder, xmlLeaderOrder, xmlMaxNumberOfPoints, xmlFirstSegmentAngle,
  xmlSecondSegmentAngle,  xmlStyleDescription, xmlAlwaysAlignTextLeft,
  xmlAlignSpace, xmlBlockScaleEnabled, xmlBlockRotationEnabled,  xmlBreakSize,
  xmlArrows, xmlLabels, xmlDirectionSign, xmlExtendedToText, xmlJustification,
  xmlTextAlignIPE, xmlContext, xmlContentBasePoint,
  xmlMLeaders, xmlOverallScale, xmlTextContext,
  xmlTextNormal, xmlTextLocation, xmlTextDirection, xmlLineSpacingFactor,
  xmlLineSpacingStyle, xmlBackgroundScaleFactor, xmlBackgroundTransparency,
  xmlBackground, xmlBackgroundMask, xmlTextHeightAuto, xmlColumnType,
  xmlColumnWidth, xmlColumnGutter, xmlColumnFlowReversed, xmlColumnSize,
  xmlWordBreak, xmlBaseDirection, xmlBaseVertical, xmlNormalReversed,
  xmlPoints, xmlLeaderType, xmlFlagsStr, xmlIntValues,
  xmlOwnerHandle, xmlInsertMatrix,
  xmlClassId(*xmlClassId only last*) );

  TsgXMLIds = array of TsgXMLId;

  TsgXMLType = record
    Id: TsgXMLId;
    ClassId: TClass;
  end;

  TsgXMLMode = (xmAddSubEntities, xmAddSectionEntities, xmNoSubEntitiesNode,
    xmCallHelp, xmOnlyChildNodes, xmlGetDefaultValue, xmlForViewing,
    xmlModReduced, xmlModGeometry, xmlModMeshes, xmlModPhysical, xmlModAliases,
    xmFullXRefNames, xmPathGenSup);
  TsgXMLModes = set of TsgXMLMode;

  PsgXMLParams = ^TsgXMLParams;
  TsgXMLParams = record
    Mode: TsgXMLModes;
    NodeData: TObject;
  end;

const
  cnstDefaultXMLMode = [xmAddSubEntities];

type
  TsgXMLIdItem = record
    Name: string;
    ValueType: TsgDataType;
    Description: string;
  end;

const
  cnstDataTypes: array[TsgDataType] of record
    Name: string;
    Description: string;
  end = (
    (Name: 'Undefined'; Description: 'Undefined value'),
    (Name: 'Boolean'; Description: 'Provides an enumeration of the logical True and False values'),
    (Name: 'Byte'; Description: 'Supports positive integers from 0 to 255. Occupies 8 bits of storage'),
    (Name: 'Word'; Description: 'Supports positive integers from 0 to 65535. Occupies 16 bits of storage'),
    (Name: 'Integer'; Description: 'Supports positive and negative integers. Includes 1 bit sign, and 31 bits value'),
    (Name: 'Int64'; Description: 'Supports positive and negative integers. Includes 1 bit sign, and 63 bits value'),
    (Name: 'Color'; Description: 'Represents color values in range from -$7FFFFFFF-1 to $7FFFFFFF. The last 3 bytes represents R, G and B values correspondingly'),
    (Name: 'Handle'; Description: 'Direct Handle via $'),
    (Name: 'Single'; Description: 'Supports single-precision floating-point values'),
    (Name: 'Double'; Description: 'Supports double-precision floating-point values'),
    (Name: 'Pointer'; Description: 'Provides a general use pointer to any memory based variable'),
    (Name: 'String'; Description: 'Represents a string of chars, International alphabet'),
    (Name: 'Point'; Description: 'Represents a record holding X and Y integer values'),
    (Name: 'F2DPoint'; Description: 'Represents a record holding X and Y double values'),
    (Name: 'FPoint'; Description: 'Represents a record holding X, Y and Z double values'),
    (Name: 'Rect'; Description: 'Represents a record holding 2D rectangle integer values as either 4 coordinates or 2 points'),
    (Name: 'F2DRect'; Description: 'Represents a record holding 2D rectangle double values as either 4 coordinates or 2 points'),
    (Name: 'FRect'; Description: 'Represents a record holding 3D rectangle double values as either 6 coordinates or 2 points'),
    (Name: 'Version'; Description: 'Represents a record holding major version and minor version integer numbers'),
    (Name: 'ColorCAD'; Description: 'Complex type: [0: Index | 1: RGB; Value;]'),
    (Name: 'TFMatrix'; Description: 'Represents a record holding double array values'),
    //types by Description
    (Name: 'EntHandle'; Description: '$handle or @handle'),
    (Name: 'EntName'; Description: 'String value representing an entity name'),
    (Name: 'List'; Description: 'Represents a list ob objects'),
    (Name: 'ListItem'; Description: 'Represents an object from a list'),
    (Name: 'StringInternal'; Description: 'String value for internal use'),
    (Name: 'Base64'; Description: 'Represents binary data as string'),
    (Name: 'Singles'; Description: 'Represents list of Single values'),
    (Name: 'FPoints2DOr3D'; Description: 'Represents list of F2DPoint or FPoint'),
    (Name: 'Procedure'; Description: 'A method'),
    (Name: 'Parameters'; Description: 'List of parameters'),
    (Name: cnstXMLGuid; Description: 'Represents a guid identifier'),
    (Name: 'Class'; Description: 'A Class'),
    (Name: 'Class'; Description: 'A Variant')
  );

  cnstClassIdUniqName = '{14D72A32-CDD2-40A4-BFC3-7D776CE0CA61}';
  cnstXMLNames: array[TsgXMLId] of TsgXMLIdItem = (
    (Name: ''; ValueType: dtUndefined;  Description: ''),
    (Name: 'LineWeight';             ValueType: dtDouble; Description: 'Line Weight in mm'),
    (Name: 'Flags';                  ValueType: dtInteger; Description: 'Flags are not suggested for usage. They have different meanings for particular entities, most of them are represented also directly via other attributes.'),
    (Name: 'LineTypeScale';          ValueType: dtDouble; Description: 'Scale for Line Type elements'),
    (Name: 'Layer';                  ValueType: dtEntName; Description: 'Name of a Layer'),
    (Name: 'LineType';               ValueType: dtEntName; Description: 'Name of a LineType'),
    (Name: 'Name';                   ValueType: dtString; Description: 'Name string value'),
    (Name: 'Handle';                 ValueType: dtEntHandle; Description: 'Handle value'),
    (Name: 'SubEntities';            ValueType: dtList; Description: 'Sub entities list'),
    (Name: 'Color';                  ValueType: dtColorCAD; Description: 'Color specified with TsgColorCAD'),
    (Name: cnstXmlVisible;           ValueType: dtBool; Description: 'Specifies entity visibility'),
    (Name: 'Locked';                 ValueType: dtBool; Description: 'Specifies entity editability'),
    (Name: 'Point';                  ValueType: dtTFPoint; Description: '3D Point defining entity'),
    (Name: 'BasePoint';              ValueType: dtTFPoint; Description: 'Base 3D point'),
    (Name: 'Bulge';                  ValueType: dtDouble; Description: 'Vertex bulge value'),
    (Name: 'StartWidth';             ValueType: dtDouble; Description: 'Start width for a segment'),
    (Name: 'EndWidth';               ValueType: dtDouble; Description: 'End width for a segment'),
    (Name: 'Point1';                 ValueType: dtTFPoint; Description: '3D point defining entity'),
    (Name: 'Length';                 ValueType: dtDouble; Description: 'Length value'),
    (Name: 'BlockName';              ValueType: dtEntName; Description: 'Name of a Block'),
    (Name: 'Extrusion';              ValueType: dtTFPoint; Description: 'Extrusion direction point'),
    (Name: 'Angle';                  ValueType: dtDouble; Description: 'Angle value'),
    (Name: 'Scale';                  ValueType: dtDouble; Description: 'Scale value'),
    (Name: 'Attribs';                ValueType: dtList; Description: 'List of attributes'),
    (Name: 'Height';                 ValueType: dtDouble; Description: 'Height value'),
    (Name: 'Rotation';               ValueType: dtDouble; Description: 'Rotation angle'),
    (Name: 'ObliqueAngle';           ValueType: dtDouble; Description: 'Oblique angle'),
    (Name: 'Text';                   ValueType: dtString; Description: 'Text string'),
    (Name: 'StyleName';              ValueType: dtString; Description: 'Name of a Style'),
    (Name: 'Tag';                    ValueType: dtString; Description: 'Attribute or attribute definition tag string'),
    (Name: 'AttValue';               ValueType: dtString; Description: 'Attribute or attribute definition value string'),
    (Name: 'ArrowSize';              ValueType: dtDouble; Description: 'Dimensioning arrow size'),
    (Name: 'Point2';                 ValueType: dtTFPoint; Description: '3D point defining entity'),
    (Name: 'Point3';                 ValueType: dtTFPoint; Description: '3D point defining entity'),
    (Name: 'ControlPoints';          ValueType: dtFPoints2DOr3D; Description: 'List of Controls for spline and based entities'),
    (Name: 'Count';                  ValueType: dtInteger; Description: 'Number of members in list'),
    (Name: 'CountU';                 ValueType: dtInteger; Description: 'Number of U-series of spline surface control points'),
    (Name: 'CountV';                 ValueType: dtInteger; Description: 'Number of V-series of spline surface control points'),
    (Name: 'KnotsPoints';            ValueType: dtFPoints2DOr3D; Description: 'List of Knots for spline based entities'),
    (Name: 'Weights';                ValueType: dtSingles; Description: 'List of Weights for spline based entities'),
    (Name: 'FitPoints';              ValueType: dtFPoints2DOr3D; Description: 'List of Fits for spline based entities'),
    (Name: 'StartTangentVector';     ValueType: dtTFPoint; Description: 'Start tangent direction point'),
    (Name: 'EndTangentVector';       ValueType: dtTFPoint; Description: 'End tangent direction point'),
    (Name: 'Degree';                 ValueType: dtDouble; Description: 'Degree value'),
    (Name: 'DegreeU';                ValueType: dtDouble; Description: 'U degree value of spline surface'),
    (Name: 'DegreeV';                ValueType: dtDouble; Description: 'V degree value of spline surface'),
    (Name: 'Knot';                   ValueType: dtDouble; Description: 'Knot value for spline based entities'),
    (Name: 'Weight';                 ValueType: dtDouble; Description: 'Weight value for spline based entities'),
    (Name: 'Radius';                 ValueType: dtDouble; Description: 'Radius value'),
    (Name: 'MajorRadius';            ValueType: dtDouble; Description: 'Major radius value of torus'),
    (Name: 'MinorRadius';            ValueType: dtDouble; Description: 'Minor radius value of torus'),
    (Name: 'CircleLength';           ValueType: dtDouble; Description: 'Length of a circle'),
    (Name: 'Area';                   ValueType: dtDouble; Description: 'Area value'),
    (Name: 'EndAngle';               ValueType: dtDouble; Description: 'End angle value'),
    (Name: 'StartAngle';             ValueType: dtDouble; Description: 'Start angle value'),
    (Name: 'RadPt';                  ValueType: dtTFPoint; Description: 'End point of the major axis relative to the Center point for Ellipse'),
    (Name: 'Ratio';                  ValueType: dtDouble; Description: 'Ratio of minor axis to major axis for elliptic entities or a two values ratio'),
    (Name: 'LineAngle';              ValueType: dtDouble; Description: 'Hatch line tilt angle'),
    (Name: 'Offset';                 ValueType: dtTFPoint; Description: '3D offset point'),
    (Name: 'IsSolid';                ValueType: dtBool; Description: 'Specifies if pattern of a dotted line is solid'),
    (Name: 'Lines';                  ValueType: dtList; Description: 'List of elements for dotted lines'),
    (Name: 'Dash';                   ValueType: dtDouble; Description: 'Dash element from list of elements for dotted lines'),
    (Name: 'StartPoint';             ValueType: dtTFPoint; Description: 'Start point, can be used with 2D and 3D entities'),
    (Name: 'EndPoint';               ValueType: dtTFPoint; Description: 'End point, can be used with 2D and 3D entities'),
    (Name: 'CenterPoint';            ValueType: dtTFPoint; Description: '3D center point'),
    (Name: 'EndParam';               ValueType: dtTFPoint; Description: 'End parameter for 2D Arc and inherited curves'),
    (Name: 'StartParam';             ValueType: dtTFPoint; Description: 'Start parameter for 2D Arc and inherited curves'),
    (Name: 'MajorPoint';             ValueType: dtTFPoint; Description: 'End point of the major axis relative to the Center point for 2D Ellipse'),
    (Name: 'ColumnCount';            ValueType: dtInteger; Description: 'Number of columns'),
    (Name: 'RowCount';               ValueType: dtInteger; Description: 'Number of rows'),
    (Name: 'ColSpacing';             ValueType: dtDouble; Description: 'Spacing for columns'),
    (Name: 'RowSpacing';             ValueType: dtDouble; Description: 'Spacing for rows'),
    (Name: 'RectWidth';              ValueType: dtDouble; Description: 'Width of a rectangle'),
    (Name: 'TextValue';              ValueType: dtString; Description: 'Text string value'),
    (Name: 'HatchName';              ValueType: dtString; Description: 'Name of a Hatch pattern'),
    (Name: 'SolidFill';              ValueType: dtBool; Description: 'Specifies if a Hatch have solid filling'),
    (Name: 'Width';                  ValueType: dtDouble; Description: 'Width value'),
    (Name: 'Elevation';              ValueType: dtTFPoint; Description: 'Elevation value'),
    (Name: 'PatternAngle';           ValueType: dtDouble; Description: 'Angle of lines in Hatch pattern'),
    (Name: 'PatternScale';           ValueType: dtDouble; Description: 'Scale for Hatch pattern'),
    (Name: 'FileName';               ValueType: dtString; Description: 'File name string'),
    (Name: 'DimScaleOverall';        ValueType: dtDouble; Description: 'Overall dimensioning scale factor'),
    (Name: 'Justify';                ValueType: dtInteger; Description: 'Specifies justification type'),
    (Name: 'Start';                  ValueType: dtTFPoint; Description: 'Start point for Helix'),
    (Name: 'Turns';                  ValueType: dtDouble; Description: 'Number of turns for Helix'),
    (Name: 'TurnHeight';             ValueType: dtDouble; Description: 'Turn height for Helix'),
    (Name: 'Clockwise';              ValueType: dtBool; Description: 'Specifies if Helix is twisted clockwise'),
    (Name: 'ACADVer';                ValueType: dtString; Description: 'Header variable. The AutoCAD drawing database version number'),
    (Name: 'CodePage';               ValueType: dtString; Description: 'Header variable. Drawing code page'),
    (Name: 'TextStyle';              ValueType: dtString; Description: 'Header variable. Current text style name'),
    (Name: 'CLayer';                 ValueType: dtString; Description: 'Header variable. Current layer name'),
    (Name: 'CELType';                ValueType: dtString; Description: 'Header variable. Entity linetype name, or BYBLOCK or BYLAYER'),
    (Name: 'CEColor';                ValueType: dtColorCAD; Description: 'Header variable. Current entity color'),
    (Name: 'CELTScale';              ValueType: dtDouble; Description: 'Header variable. Current entity linetype scale'),
    (Name: 'CELWeight';              ValueType: dtDouble; Description: 'Header variable. Lineweight of new objects'),
    (Name: 'DimStyle';               ValueType: dtString; Description: 'Header variable. Dimension style name'),
    (Name: 'DIMTXSTY';               ValueType: dtString; Description: 'Dimension text style'),
    (Name: 'DimBlk';                 ValueType: dtEntName; Description: 'Arrow block name'),
    (Name: 'DimBlk1';                ValueType: dtEntName; Description: 'First arrow block name'),
    (Name: 'DimBlk2';               ValueType: dtEntName; Description: 'Second arrow block name'),
    (Name: 'DimRBlk';                ValueType: dtEntName; Description: 'Arrow block name for leaders'),
    (Name: 'DimAlt';                 ValueType: dtBool; Description: 'Alternate unit dimensioning performed if nonzero'),
    (Name: 'DimAltF';                ValueType: dtDouble; Description: 'Alternate unit scale factor'),
    (Name: 'DimAPost';               ValueType: dtString; Description: 'Alternate dimensioning suffix'),
    (Name: 'DimAsz';                 ValueType: dtDouble; Description: 'Dimensioning arrow size'),
    (Name: 'DimSah';                 ValueType: dtBool; Description: 'Use separate arrow blocks if nonzero'),
    (Name: 'DimCen';                 ValueType: dtDouble; Description: 'Size of center mark/lines'),
    (Name: 'DimClrD';                ValueType: dtColorCAD; Description: 'Dimension line color'),
    (Name: 'DimClrE';                ValueType: dtColorCAD; Description: 'Dimension extension line color'),
    (Name: 'DimClrT';                ValueType: dtColorCAD; Description: 'Dimension text color'),
    (Name: 'DimSD1';                 ValueType: dtBool; Description: 'Suppression of first extension line'),
    (Name: 'DimSD2';                 ValueType: dtBool; Description: 'Suppression of second extension line'),
    (Name: 'DimSE1';                 ValueType: dtBool; Description: 'First extension line suppressed if nonzero'),
    (Name: 'DimSE2';                 ValueType: dtBool; Description: 'Second extension line suppressed if nonzero'),
    (Name: 'DimDec';                 ValueType: dtInteger; Description: 'Number of decimal places for the tolerance values of aprimary units dimension'),
    (Name: 'DimExe';                 ValueType: dtDouble; Description: 'Extension line extension'),
    (Name: 'DimExo';                 ValueType: dtDouble; Description: 'Extension line offset'),
    (Name: 'DimGap';                 ValueType: dtDouble; Description: 'Dimension line gap'),
    (Name: 'DimLFac';                ValueType: dtDouble; Description: 'Linear measurements scale factor'),
    (Name: 'DimLwD';                 ValueType: dtDouble; Description: 'Dimension line lineweight'),
    (Name: 'DimLwE';                 ValueType: dtDouble; Description: 'Extension line lineweight'),
    (Name: 'DimPost';                ValueType: dtString; Description: 'General dimensioning suffix'),
    (Name: 'DimScale';               ValueType: dtDouble; Description: 'Overall dimensioning scale factor'),
    (Name: 'DimTad';                 ValueType: dtInteger; Description: 'Text above dimension line if nonzero'),
    (Name: 'DimTih';                 ValueType: dtBool; Description: 'Text inside horizontal if nonzero'),
    (Name: 'DimTix';                 ValueType: dtInteger; Description: 'Force text inside extensions if nonzero'),
    (Name: 'DimToh';                 ValueType: dtBool; Description: 'Text outside horizontal if nonzero'),
    (Name: 'DimTxt';                 ValueType: dtDouble; Description: 'Dimensioning text height'),
    (Name: 'DimTp';                  ValueType: dtDouble; Description: 'Dimensioning plus tolerance'),
    (Name: 'DimTm';                  ValueType: dtDouble; Description: 'Dimensioning minus tolerance'),
    (Name: 'DimDSep';                ValueType: dtDouble; Description: 'Single-character decimal separator used when creating dimensions whose unit format is decimale'),
    (Name: 'DimLUnit';               ValueType: dtDouble; Description: 'Sets units for all dimension types except Angular: 1 = Scientific; 2 = Decimal; 3 = Engineering; 4 = Architectural; 5 = Fractional; 6 = Windows desktop'),
    (Name: 'DimFrac';                ValueType: dtInteger; Description: 'Sets the fraction format when the DimLUnit system variable is specified as 4 (Architectural) or 5 (Fractional): 0 = Horizontal stacking; 1 = Diagonal stacking; 2 = Not stacked'),

    (Name: 'DimLRBLK';               ValueType: dtEntName; Description: 'Arrow block name for leaders'),
    (Name: 'DimADEC';                ValueType: dtInteger; Description: 'Number of precision places displayed in angular dimensions'),
    (Name: 'DimDLI';                 ValueType: dtDouble; Description: 'Dimension line increment'),
    (Name: 'DimRND';                 ValueType: dtDouble; Description: 'Rounding value for dimension distances'),
    (Name: 'DimDLE';                 ValueType: dtDouble; Description: 'Dimension line extension'),
    (Name: 'DimFXL';                 ValueType: dtDouble; Description: 'Sets the total length of the extension lines starting from the dimension line toward the dimension origin'),
    (Name: 'DimTVP';                 ValueType: dtDouble; Description: 'Text vertical position'),
    (Name: 'DimTFAC';                ValueType: dtDouble; Description: 'Dimension tolerance display scale factor'),
    (Name: 'DimALTRND';              ValueType: dtDouble; Description: 'Determines rounding of alternate units'),
    (Name: 'DimTFILLCLR';            ValueType: dtInteger; Description: 'Color numbers are displayed in the Select Color dialog box. For BYBLOCK, enter 0. For BYLAYER, enter 256.'),
    (Name: 'DimTOL';                 ValueType: dtInteger; Description: 'Dimension tolerances generated if nonzero'),
    (Name: 'DimLIM';                 ValueType: dtInteger; Description: 'Dimension limits generated if nonzero'),
    (Name: 'DimZIN';                 ValueType: dtInteger; Description: 'Controls the suppression of zeros in the primary unit value.'),
    (Name: 'DimAZIN';                ValueType: dtInteger; Description: 'Controls suppression of zeros for angular dimensions'),
    (Name: 'DimALTD';                ValueType: dtInteger; Description: 'Alternate unit decimal places'),
    (Name: 'DimTOFL';                ValueType: dtInteger; Description: 'If text is outside extensions, force line extensions between extensions if nonzero'),
    (Name: 'DimSOXD';                ValueType: dtInteger; Description: 'Suppress outside-extensions dimension lines if nonzero'),
    (Name: 'DimTDEC';                ValueType: dtInteger; Description: 'Number of decimal places to display the tolerance values'),
    (Name: 'DimALTU';                ValueType: dtInteger; Description: 'Units format for alternate units of all dimension style family members except angular'),
    (Name: 'DimALTTD';               ValueType: dtInteger; Description: 'Number of decimal places for tolerance values of an alternate units dimension'),
    (Name: 'DimAUNIT';               ValueType: dtInteger; Description: 'Angle format for angular dimensions'),
    (Name: 'DimTMOVE';               ValueType: dtInteger; Description: 'Dimension text movement rules'),
    (Name: 'DimJUST';                ValueType: dtInteger; Description: 'Horizontal dimension text position'),
    (Name: 'DimTOLJ';                ValueType: dtInteger; Description: 'Vertical justification for tolerance values'),
    (Name: 'DimTZIN';                ValueType: dtInteger; Description: 'Controls suppression of zeros for tolerance values'),
    (Name: 'DimALTZ';                ValueType: dtInteger; Description: 'Controls suppression of zeros for alternate unit dimension values'),
    (Name: 'DimALTTZ';               ValueType: dtInteger; Description: 'Controls suppression of zeros for alternate tolerance values'),
    (Name: 'DimUPT';                 ValueType: dtInteger; Description: 'Cursor functionality for user-positioned text'),
    (Name: 'DimATFIT';               ValueType: dtInteger; Description: 'Controls dimension text and arrow placement when space is not sufficient to place both within the extension lines'),
    (Name: 'DimFXLON';               ValueType: dtInteger; Description: 'Controls whether extension lines are set to a fixed length'),
    (Name: 'DimTFILL';               ValueType: dtInteger; Description: 'Controls the background of dimension text'),
    (Name: 'DimENBL';                ValueType: dtInteger; Description: 'for internal usin'),
    (Name: 'DimTXTALIGN';            ValueType: dtInteger; Description: 'for internal using'),
    (Name: 'DimLTY';                 ValueType: dtString; Description: 'Dimension line  linetype'),
    (Name: 'DimEX1LTY';              ValueType: dtString; Description: 'Extension line 1 linetype'),
    (Name: 'DimEX2LTY';              ValueType: dtString; Description: 'Extension line 2 linetype '),
    (Name: 'DimTSZ';                 ValueType: dtDouble; Description: 'Dimensioning tick size'),

    (Name: 'EXTMIN';                 ValueType: dtTFPoint; Description: 'Header variable. X, Y, and Z drawing extents lower-left corner (in WCS)'),
    (Name: 'EXTMAX';                 ValueType: dtTFPoint; Description: 'Header variable. X, Y, and Z drawing extents upper-right corner (in WCS)'),
    (Name: 'FILLETRAD';              ValueType: dtDouble; Description: 'Header variable. Fillet radius'),
    (Name: 'FILLMODE';               ValueType: dtBool; Description: 'Specifies whether hatches and fills, 2D solids, and wide polylines are filled in, default True'),
    (Name: 'LWDISPLAY';              ValueType: dtBool; Description: 'Controls the display of lineweights'),
    (Name: 'InsUnits';               ValueType: dtInteger; Description: 'Header variable. Default drawing units for AutoCAD DesignCenter blocks'),
    (Name: 'LTScale';                ValueType: dtDouble; Description: 'Header variable. Global linetype scale'),
    (Name: 'Measurement';            ValueType: dtInteger; Description: 'Header variable. Drawing units: 0 = English; 1 = Metric'),
    (Name: 'PDMODE';                 ValueType: dtInteger; Description: 'Header variable. Point display mode'),
    (Name: 'PDSIZE';                 ValueType: dtDouble; Description: 'Header variable. Point display size'),
    (Name: 'TextSize';               ValueType: dtDouble; Description: 'Header variable. Default text height'),
    (Name: 'TileMode';               ValueType: dtInteger; Description: 'Header variable. 1 for previous release compatibility mode; 0 otherwise'),
    (Name: 'UCSORG';                 ValueType: dtTFPoint; Description: 'Header variable. Origin of current UCS (in WCS)'),
    (Name: 'UCSXDir';                ValueType: dtTFPoint; Description: 'Header variable. Direction of the current UCS X axis (in WCS)'),
    (Name: 'UCSYDir';                ValueType: dtTFPoint; Description: 'Header variable. Direction of the current UCS Y axis (in WCS)'),
    (Name: 'InsBase';                ValueType: dtTFPoint; Description: 'Header variable. Insertion base set by BASE command (in WCS)'),
    (Name: 'Attmode';                ValueType: dtInteger; Description: 'Header variable. Attribute visibility: 0 = None 1 = Normal 2 = All'),
    (Name: cnstXMLHeader;            ValueType: dtList; Description: 'Header section'),
    (Name: cnstXMLTables;            ValueType: dtList; Description: 'Tables section'),
    (Name: cnstXMLBlocks;            ValueType: dtList; Description: 'Blocks section'),
    (Name: cnstXMLEntities;          ValueType: dtList; Description: 'Entities section'),
    (Name: cnstXMLObjects;           ValueType: dtList; Description: 'Objects section'),
    (Name: 'Pattern';                ValueType: dtList; Description: 'List of elements of a Hatch pattern'),
    (Name: 'Patterns';               ValueType: dtList; Description: 'List of patterns for Hatches'),
    (Name: 'Boundary';               ValueType: dtList; Description: 'List of elements forming a boundary of a Hatch'),
    (Name: 'Boundaries';             ValueType: dtList; Description: 'List of boundaries of a Hatch'),
    (Name: 'PSpaceCenter';           ValueType: dtTFPoint; Description: 'Viewport center point on layout'),
    (Name: 'MSpaceCenter';           ValueType: dtTFPoint; Description: 'Viewport model view area center point'),
    (Name: 'ViewDirection';          ValueType: dtTFPoint; Description: 'Viewport view direction vector (in WCS)'),
    (Name: 'ViewTarget';             ValueType: dtTFPoint; Description: 'Viewport view target point (in WCS)'),
    (Name: 'PSpaceWidth';            ValueType: dtDouble; Description: 'Viewport width on layout'),
    (Name: 'PSpaceHeight';           ValueType: dtDouble; Description: 'Viewport height on layout'),
    (Name: 'FrontClipPlane';         ValueType: dtDouble; Description: 'Viewport front cut off plane'),
    (Name: 'BackClipPlane';          ValueType: dtDouble; Description: 'Viewport back cut off plane'),
    (Name: 'MSpaceHeight';           ValueType: dtDouble; Description: 'Viewport model view area height'),
    (Name: 'ViewTwistAngle';         ValueType: dtDouble; Description: 'Viewport view twist angle'),
    (Name: 'StatusField';            ValueType: dtInteger; Description: 'Viewport status field'),
    (Name: 'ThisID';                 ValueType: dtInteger; Description: 'Viewport ID'),
    (Name: 'ClippingBoundaryHandle'; ValueType: dtHandle; Description: 'Handle of an entity that serves as a clipping boundary for the Viewport'),
    (Name: 'ExtData';                ValueType: dtStringInternal; Description: 'Extended data'),
    (Name: 'HandleSave';             ValueType: dtEntHandle; Description: 'Value for alternative access to a Handle'),
    (Name: 'CircleZoomPercent';      ValueType: dtInteger; Description: 'VPort circle zoom percent'),
    (Name: 'UCSOrigin';              ValueType: dtTFPoint; Description: 'VPort UCS origin'),
    (Name: 'UCSVP';                  ValueType: dtBool; Description: 'Display UCS icon at UCS origin flag'),
    (Name: 'ViewAspectRatio';        ValueType: dtDouble; Description: 'VPort view aspect ratio'),
    (Name: 'ViewCenterPoint';        ValueType: dtTFPoint; Description: 'VPort view center point'),
    (Name: 'ViewHeight';             ValueType: dtDouble; Description: 'VPort view height'),
    (Name: 'ImageDef';               ValueType: dtHandle; Description: 'Handle of ImageDef object of Image entity'),
    (Name: 'Vertex';                 ValueType: dtListItem; Description: 'Vertex element'),
    (Name: 'Vertexes';               ValueType: dtList; Description: 'List of Vertexes'),
    (Name: 'Closed';                 ValueType: dtBool; Description: 'Specifies if entity is closed'),
    (Name: 'Periodic';               ValueType: dtBool; Description: 'Specifies if Spline based entity is periodic'),
    (Name: 'Rational';               ValueType: dtBool; Description: 'Specifies if Spline based entity is rational'),
    (Name: 'PaperSpaceBlock';        ValueType: dtEntName; Description: 'Layout name'),
    (Name: 'UVector';                ValueType: dtTFPoint; Description: 'UVector for Wipeout and inherited entities'),
    (Name: 'VVector';                ValueType: dtTFPoint; Description: 'VVector for Wipeout and inherited entities'),
    (Name: 'Size';                   ValueType: dtTFPoint; Description: 'Size of image for Wipeout/Ole2Frame and inherited entities'),
    (Name: 'Calc';                   ValueType: dtProcedure; Description: 'Alternative creation method'),
    (Name: 'Mode';                   ValueType: dtInteger; Description: 'Mode for alternative creation method'),
    (Name: 'Arrowhead';              ValueType: dtBool; Description: 'Specifies if arrowhead is present for Leader'),
    (Name: 'LeaderPathType';         ValueType: dtInteger; Description: 'Specifies if Leader path is spline'),
    (Name: 'Properties';             ValueType: dtList; Description: 'Dimension style properties'),
    (Name: 'DictValue';              ValueType: dtString; Description: 'Dictionary value'),
    (Name: 'PScale';                 ValueType: dtTFPoint; Description: 'Scale factor'),
    (Name: 'Thickness';              ValueType: dtDouble; Description: 'Thickness'),
    (Name: 'Frozen';                 ValueType: dtBool; Description: 'Specifies if Layer is frozen'),
    (Name: 'FrozenByNewViewPort';    ValueType: dtBool; Description: 'Specifies if Layer is frozen for new Viewports'),
    (Name: 'XrefLink';               ValueType: dtBool; Description: 'Specifies if entity is externally dependent on an xref'),
    (Name: 'IsPlotting';             ValueType: dtBool; Description: 'Specifies if this layer will be plotted'),
    (Name: 'BigFont';                ValueType: dtString; Description: 'Bigfont file name for Style'),
    (Name: 'FixedHeight';            ValueType: dtDouble; Description: 'Fixed text heigh for Style'),
    (Name: 'FontName';               ValueType: dtString; Description: 'Font name'),
    (Name: 'FontStyle';              ValueType: dtInteger; Description: 'Font style'),
    (Name: 'LastHeightUsed';         ValueType: dtDouble; Description: 'Last used font height for Style'),
    (Name: 'PrimaryFont';            ValueType: dtString; Description: 'Primary font file name for Style'),
    (Name: 'TextGenFlags';           ValueType: dtInteger; Description: 'Text generation flags for Style'),
    (Name: 'WidthFactor';            ValueType: dtDouble; Description: 'Width factor for Style'),
    (Name: 'GlobalWidth';            ValueType: dtDouble; Description: 'Global width value'),
    (Name: 'ArrowType';              ValueType: dtByte; Description: 'Arrow type for entities'),
    (Name: 'ArcDefPoint';            ValueType: dtTFPoint; Description: 'Point defining dimension arc for angular Dimension'),
    (Name: 'DefPoint';               ValueType: dtTFPoint; Description: 'Definition point for Dimension'),
    (Name: 'LinDefPoint1';           ValueType: dtTFPoint; Description: 'Start point of the first extension line for Dimension'),
    (Name: 'LinDefPoint2';           ValueType: dtTFPoint; Description: 'Start point of the second extension line for Dimension'),
    (Name: 'MiddlePoint';            ValueType: dtTFPoint; Description: 'Middle point of dimension text for Dimension'),
    (Name: 'RadDefPoint';            ValueType: dtTFPoint; Description: 'Definition point for diameter, radius, and angular Dimensions'),
    (Name: 'Transparency';           ValueType: dtBool; Description: 'Definition transparency for imageent'),
    (Name: 'TransparentColor';       ValueType: dtColor; Description: 'Definition transparent color for imageent'),
    (Name: 'Description';            ValueType: dtColor; Description: 'Description'),
    (Name: 'LayoutScale';            ValueType: dtColor; Description: 'Layout units scale'),
    (Name: 'ModelScale';             ValueType: dtColor; Description: 'Model units scale'),
  {$IFDEF SG_BREP_TOPOLOGY}
    (Name: 'VolumeOOC';              ValueType: dtDouble; Description: 'Volume value, for BREP topology'),
    (Name: 'AreaOOC';                ValueType: dtDouble; Description: 'Area value, for BREP topology'),
    (Name: 'CenterMassOOC';          ValueType: dtTFPoint; Description: 'Center mass point, for BREP topology'),
    (Name: 'OriginalNameOOC';        ValueType: dtString; Description: 'Original name, for BREP topology'),
  {$ENDIF}
    (Name: 'Distance';               ValueType: dtDouble; Description: 'Distance'),
    (Name: 'AngleInXY';              ValueType: dtDouble; Description: 'Angle in plane XY'),
    (Name: 'AngleFromXY';            ValueType: dtDouble; Description: 'Angle from plane XY'),
    (Name: 'DeltaX';                 ValueType: dtDouble; Description: 'Delta X'),
    (Name: 'DeltaY';                 ValueType: dtDouble; Description: 'Delta Y'),
    (Name: 'DeltaZ';                 ValueType: dtDouble; Description: 'Delta Z'),
    (Name: 'ScaleFactor';            ValueType: dtDouble; Description: 'ScaleFactor'),
    (Name: 'Perimeter';              ValueType: dtDouble; Description: 'Perimeter'),
    (Name: 'SegmentIndex';           ValueType: dtInteger; Description: 'Segment index'),
    (Name: 'PolylineLength';         ValueType: dtDouble; Description: 'Polyline length'),
    (Name: 'MatProp';                ValueType: dtDouble; Description: 'Material name'),
    (Name: 'HyperLink';              ValueType: dtString; Description: 'HyperLink'),
    (Name: 'TextOverride';           ValueType: dtString; Description: 'Dimension text explicitly entered by the user'),
    (Name: 'TextRotation';           ValueType: dtDouble; Description: 'Dimension text rotation explicitly entered by the user'),
    (Name: 'GradientTypeName';       ValueType: dtString; Description: 'Gradient type can have the following values:' +
                                                                       'CURVED, CYLINDER, HEMISPHERICAL, LINEAR, SPHERICAL' +
                                                                       'INVCURVED, INVCYLINDER, INVHEMISPHERICAL, INVSPHERICAL'),
    (Name: 'GradientAngle';          ValueType: dtDouble; Description: 'Angle in gradient'),
    (Name: 'GradientOneColor';       ValueType: dtColorCAD; Description: 'The first color for the gradient'),
    (Name: 'GradientTwoColor';       ValueType: dtColorCAD; Description: 'A second color for the gradient'),
    (Name: 'GradientUseCenter';      ValueType: dtBool; Description: 'Gradient use center'),
    (Name: 'BackgroundColor';        ValueType: dtColorCAD; Description: 'Background color'),
    (Name: 'BackgroundFlags';        ValueType: dtColorCAD; Description: 'Background fill setting: 0 = Background fill off, 1 = Use background fill color, 2 = Use drawing window color as background fill color'),
    (Name: 'BoundaryPolylines';      ValueType: dtList; Description: 'List of polylines generated by boundaries of a Hatch'),
    (Name: 'BoundaryPolyline';       ValueType: dtFPoints2DOr3D; Description: 'List of polyline points generated by boundaries of a Hatch'),

    (Name: 'EedList';                ValueType: dtProcedure; Description: ''),
    (Name: 'Application';            ValueType: dtProcedure; Description: ''),
    (Name: 'Item';                   ValueType: dtProcedure; Description: ''),
    (Name: 'Data';                   ValueType: dtString; Description: ''),
    (Name: 'Type';                   ValueType: dtString; Description: ''),
    (Name: 'Code';                   ValueType: dtString; Description: ''),

    (Name: 'plPageSetupName';            ValueType: dtString; Description: ''),
    (Name: 'plPrintOrConfigName';        ValueType: dtString; Description: ''),
    (Name: 'plLayoutFlags';              ValueType: dtInt64; Description: 'Flags'),
    (Name: 'plPaperUnits';               ValueType: dtByte; Description: 'Enum'),
    (Name: 'plStandardScaleType';        ValueType: dtByte; Description: ''),
    (Name: 'plNumerator';                ValueType: dtDouble; Description: ''),
    (Name: 'plDenominator';              ValueType: dtDouble; Description: ''),
    (Name: 'plPaperSizeName';            ValueType: dtString; Description: ''),
    (Name: 'plViewName';                 ValueType: dtString; Description: ''),
    (Name: 'plPrintableMargin';          ValueType: dtTF2DRect; Description: ''),
    (Name: 'plPaperSize';                ValueType: dtTF2DPoint; Description: ''),
    (Name: 'plOrigin';                   ValueType: dtTF2DPoint; Description: ''),
    (Name: 'plWindowAreaMin';            ValueType: dtTF2DPoint; Description: ''),
    (Name: 'plWindowAreaMax';            ValueType: dtTF2DPoint; Description: ''),
    (Name: 'plRotation';                 ValueType: dtByte; Description: 'Enum'),
    (Name: 'plType';                     ValueType: dtByte; Description: 'Enum'),
    (Name: 'plCurrentStyleSheet';        ValueType: dtString; Description: ''),
    (Name: 'plShadeMode';                ValueType: dtInteger; Description: ''),
    (Name: 'plShadeResolutionLevel';     ValueType: dtInteger; Description: ''),
    (Name: 'plShadeCustomDPI';           ValueType: dtInteger; Description: ''),
    (Name: 'plFloatingPointScaleFactor'; ValueType: dtDouble; Description: ''),
    (Name: 'plPaperImageOrigin';         ValueType: dtTF2DPoint; Description: ''),
    (Name: 'FillStyleIndex';             ValueType: dtInteger; Description: ''),
    (Name: 'EntData';                    ValueType: dtStringInternal; Description: 'Entity data'),
    (Name: 'Box';                        ValueType: dtTFRect; Description: 'Entity box'),
    (Name: 'DimAssoc';                   ValueType: dtByte; Description:  'Associativity of dimension objects'),
    (Name: 'Value';                      ValueType: dtUndefined; Description: ''),
    (Name: 'HAlign';                     ValueType: dtUndefined; Description: ''),
    (Name: 'VAlign';                     ValueType: dtUndefined; Description: ''),
    (Name: cnstXmlX;                     ValueType: dtDouble; Description: cnstXmlX),
    (Name: cnstXmlY;                     ValueType: dtDouble; Description: cnstXmlY),
    (Name: cnstXmlZ;                     ValueType: dtDouble; Description: cnstXmlZ),
    (Name: cnstXmlAxisX;                 ValueType: dtTFPoint; Description: cnstXmlAxisX),
    (Name: cnstXmlAxisY;                 ValueType: dtTFPoint; Description: cnstXmlAxisY),
    (Name: cnstXmlAxisZ;                 ValueType: dtTFPoint; Description: cnstXmlAxisZ),
    (Name: 'AxisX2D';                    ValueType: dtTF2DPoint; Description: 'Axis U vector 2D'),
    (Name: 'AxisY2D';                    ValueType: dtTF2DPoint; Description: 'Axis V vector 2D'),
    (Name: 'CenterPoint2D';              ValueType: dtTF2DPoint; Description: '2D center point'),
    (Name: 'ClipRect';                   ValueType: dtTFRect; Description: 'Entity clip rect'),
    (Name: 'TurnsOff';                   ValueType: dtBool; Description: 'Turns the viewport off'),
    (Name: 'PointsCount';                ValueType: dtInteger; Description: 'Points count'),
    (Name:  cnstXmlCanShow;                    ValueType: dtBool; Description: 'Can show'),
    (Name: 'InsertsCount';               ValueType: dtInteger; Description: 'Inserts count'),
    (Name: 'ShapesCount';                ValueType: dtInteger; Description: 'Shapes count'),
    (Name: 'SolidsCount';                ValueType: dtInteger; Description: 'Solids count'),
    (Name: cnstXmlPathKey;               ValueType: dtHandle; Description: 'Individual identifier of the visual representation of the modeler object'),
    (Name: 'SinAngle';                   ValueType: dtDouble; Description: 'Sine of the angle at the apex of the cone for cone surface or cone helix'),
    (Name: 'CosAngle';                   ValueType: dtDouble; Description: 'Cosine of the angle at the apex of the cone for cone surface or cone helix'),
    (Name: 'HelixStep';                  ValueType: dtDouble; Description: 'Distance between adjacent turns of the helix curve'),
    (Name: 'FocalLength';                ValueType: dtDouble; Description: 'Parabola focal length value'),
    (Name: 'Kx';                         ValueType: dtDouble; Description: 'Hyperbola real axis length value'),
    (Name: 'Ky';                         ValueType: dtDouble; Description: 'Hyperbola imaginary axis length value'),
    (Name: 'Direction';                  ValueType: dtTFPoint; Description: 'Direction vector 3D'),
    (Name: 'Normal';                     ValueType: dtTFPoint; Description: 'Normal vector 3D'),
    (Name: 'Tangent';                    ValueType: dtTFPoint; Description: 'Tangent vector 3D'),
    (Name: 'ParamT';                     ValueType: dtDouble; Description: 'Curve point parameter value'),
    (Name: 'UVPoint';                    ValueType: dtTF2DPoint; Description: 'UV Point'),
    (Name: 'Deflection';                 ValueType: dtDouble; Description: 'Deflection value'),
    (Name: 'Interval';                   ValueType: dtString; Description: 'Interval of curve parameters'),
    (Name: 'Interval1';                  ValueType: dtString; Description: 'Interval of parameters for first curve'),
    (Name: 'Interval2';                  ValueType: dtString; Description: 'Interval of parameters for second curve'),
    (Name: 'Interval2d';                 ValueType: dtString; Description: 'Interval of UV values'),
    (Name: 'Interval3d';                 ValueType: dtString; Description: 'Interval of XYZ values'),
    (Name: 'Sense';                      ValueType: dtString; Description: 'Topological orientation'),
    (Name: 'IsPrimary';                  ValueType: dtBool; Description: 'Edge representation is primary'),
    (Name: 'LinearTolerance';            ValueType: dtDouble; Description: 'Linear tolerance of modeller operations'),
    (Name: 'AngularTolerance';           ValueType: dtDouble; Description: 'Angular tolerance of modeller operations'),
    (Name: 'LinearPrecision';            ValueType: dtDouble; Description: 'Linear precision of meshes and polylines'),
    (Name: 'AngularPrecision';           ValueType: dtDouble; Description: 'Angular precision of meshes and polylines'),
    (Name: 'IsoLinesU';                  ValueType: dtInteger; Description: 'U IsoLines count'),
    (Name: 'IsoLinesV';                  ValueType: dtInteger; Description: 'U IsoLines count'),
    (Name: cnstXmlColorVector;           ValueType: dtTFPoint; Description: 'Color vector'),
    (Name: 'FillColor';                  ValueType: dtColorCAD; Description: 'Fill color specified with TsgColorCAD'),
    (Name: 'Align';                      ValueType: dtWord; Description: 'Align'),
    (Name: cnstXmlVisible3D;             ValueType: dtBool; Description: 'Visibility dependent on PathKey defines one of the entity visual representations'),
    (Name: cnstXmlBox3D;                 ValueType: dtTFRect; Description: 'Box 3d dependent on PathKey defines one of the entity visual representations'),
    (Name: cnstXmlVolume;                ValueType: dtDouble; Description: 'Volume 3d dependent on PathKey defines one of the entity visual representations'),
    (Name: cnstxmlAlphaBlend;            ValueType: dtInteger; Description: 'AlphaBlend dependent on PathKey defines one of the entity visual representations'),
    (Name: 'Polypoints';                 ValueType: dtFPoints2DOr3D; Description: 'List of polyline points'),
    (Name: 'CustomColor';                ValueType: dtColor; Description: 'Color'),
    (Name: 'BinaryData';                 ValueType: dtBase64; Description: ''),
    (Name: 'ComplexType';                ValueType: dtByte; Description: ''),
    (Name: 'Index';                      ValueType: dtInteger; Description: ''),
    (Name: 'Miter';                      ValueType: dtTFPoint; Description: 'Direction vector of miter at this vertex'),
     //MLeader
    (Name: 'LeaderLineType';              ValueType: dtInteger; Description: ''),
    (Name: 'LineColor';                   ValueType: dtColorCAD; Description: ''),
    (Name: 'Landing';                     ValueType: dtInteger; Description: ''),
    (Name: 'LandingGap';                  ValueType: dtDouble; Description: ''),
    (Name: 'DogLeg';                      ValueType: dtInteger; Description: ''),
    (Name: 'DogLegLen';                   ValueType: dtDouble; Description: ''),
    (Name: 'ContentType';                 ValueType: dtInteger; Description: ''),
    (Name: 'Block';                       ValueType: dtBool; Description: ''),
    (Name: 'BlockColor';                  ValueType: dtColorCAD; Description: ''),
    (Name: 'BlockScale';                  ValueType: dtTFPoint; Description: ''),
    (Name: 'BlockRotation';               ValueType: dtDouble; Description: ''),
    (Name: 'BlockNormal';                 ValueType: dtTFPoint; Description: ''),
    (Name: 'BlockLocation';               ValueType: dtTFPoint; Description: ''),
    (Name: 'BlockMatrix';                 ValueType: dtTFMatrix; Description: ''),

    (Name: 'BlockConnectionType';         ValueType: dtInteger; Description: ''),
    (Name: 'UseDefMText';                 ValueType: dtInteger; Description: ''),
    (Name: 'TxtColor';                    ValueType: dtColorCAD; Description: ''),
    (Name: 'TxtHeight';                   ValueType: dtDouble; Description: ''),
    (Name: 'TxtAngleType';                ValueType: dtInteger; Description: ''),
    (Name: 'TxtLabel';                    ValueType: dtString; Description: ''),
    (Name: 'TxtFrame';                    ValueType: dtInteger; Description: ''),
    (Name: 'TxtAlignment';                ValueType: dtInteger; Description: ''),
    (Name: 'SwitchAlignment';             ValueType: dtInteger; Description: ''),
    (Name: 'AttachmentDirection';         ValueType: dtInteger; Description: ''),
    (Name: 'LeftAttachment';              ValueType: dtInteger; Description: ''),
    (Name: 'RightAttachment';             ValueType: dtInteger; Description: ''),
    (Name: 'TopAttachment';               ValueType: dtInteger; Description: ''),
    (Name: 'BottomAttachment';            ValueType: dtInteger; Description: ''),
    (Name: 'MLeaderOrder';                ValueType: dtInteger; Description: ''),
    (Name: 'LeaderOrder';                 ValueType: dtInteger; Description: ''),
    (Name: 'MaxNumberOfPoints';           ValueType: dtInteger; Description: ''),
    (Name: 'FirstSegmentAngle';           ValueType: dtDouble; Description: ''),
    (Name: 'SecondSegmentAngle';          ValueType: dtDouble; Description: ''),
    (Name: 'StyleDescription';            ValueType: dtString; Description: ''),
    (Name: 'AlwaysAlignTextLeft';         ValueType: dtInteger; Description: ''),
    (Name: 'AlignSpace';                  ValueType: dtDouble; Description: ''),
    (Name: 'BlockScaleEnabled';           ValueType: dtInteger; Description: ''),
    (Name: 'BlockRotationEnabled';        ValueType: dtInteger; Description: ''),
    (Name: 'BreakSize';                   ValueType: dtDouble; Description: ''),
    (Name: 'Arrows';                      ValueType: dtList; Description: ''),
    (Name: 'Labels';                      ValueType: dtList; Description: ''),
    (Name: 'DirectionSign';               ValueType: dtInteger; Description: ''),
    (Name: 'ExtendedToText';              ValueType: dtInteger; Description: ''),
    (Name: 'Justification';               ValueType: dtInteger; Description: ''),
    (Name: 'TextAlignIPE';                ValueType: dtInteger; Description: ''),
    (Name: 'Context';                     ValueType: dtParameters; Description: ''),
    (Name: 'ContextBasePoint';            ValueType: dtTFPoint; Description: ''),

    (Name: 'MLeaders';                    ValueType: dtList; Description: ''),

    (Name: 'OverallScale';                ValueType: dtDouble; Description: ''),
    (Name: 'TextContext';                 ValueType: dtInteger; Description: ''),
    (Name: 'TextNormal';                  ValueType: dtTFPoint; Description: ''),
    (Name: 'TextLocation';                ValueType: dtTFPoint; Description: ''),
    (Name: 'TextDirection';               ValueType: dtTFPoint; Description: ''),
    (Name: 'LineSpacingFactor';           ValueType: dtDouble; Description: ''),
    (Name: 'LineSpacingStyle';            ValueType: dtInteger; Description: ''),
    (Name: 'BackgroundScaleFactor';       ValueType: dtDouble; Description: ''),
    (Name: 'BackgroundTransparency';      ValueType: dtInteger; Description: ''),
    (Name: 'Background';                  ValueType: dtBool; Description: ''),
    (Name: 'BackgroundMask';              ValueType: dtInteger; Description: ''),
    (Name: 'TextHeightAuto';              ValueType: dtBool; Description: ''),
    (Name: 'ColumnType';                  ValueType: dtInteger; Description: ''),
    (Name: 'ColumnWidth';                 ValueType: dtDouble; Description: ''),
    (Name: 'ColumnGutter';                ValueType: dtDouble; Description: ''),
    (Name: 'ColumnFlowReversed';          ValueType: dtBool; Description: ''),
    (Name: 'ColumnSize';                  ValueType: dtDouble; Description: ''),
    (Name: 'WordBreak';                   ValueType: dtBool; Description: ''),
    (Name: 'BaseDirection';               ValueType: dtTFPoint; Description: ''),
    (Name: 'BaseVertical';                ValueType: dtTFPoint; Description: ''),
    (Name: 'NormalReversed';              ValueType: dtBool; Description: ''),
    (Name: 'Points';                      ValueType: dtFPoints2DOr3D; Description: ''),
    (Name: 'LeaderType';                  ValueType: dtInteger; Description: ''),
    (Name: 'FlagsStr';                    ValueType: dtList; Description: ''),
    (Name: 'IntValues';                   ValueType: dtList; Description: ''),
    (Name: 'OwnerHandle';                 ValueType: dtEntHandle; Description: 'Owner handle value'),
    (Name: 'InsertMatrix';                ValueType: dtTFMatrix; Description: 'Insert matrix value is read-only'),

    (Name: cnstClassIdUniqName;      ValueType: dtUndefined; Description: '')//alwas last element!
  );

  cnstDimXmlId: array[TsgDimNameVal] of TsgXMLId =
    (xmlDimAsz, xmlDimBlk, xmlDimBlk1, xmlDimBlk2, xmlDimRBlk,
    xmlDimSD1, xmlDimSD2, xmlDimSE1, xmlDimSE2, xmlDimClrD, xmlDimClrE, xmlDimClrT,
    xmlDimLwD, xmlDimLwE, xmlDimTix, xmlDimLFac, xmlDimDec, xmlDimTxt, xmlDimTad,
    xmlDimCen, xmlDimTextStyle, xmlDimGap, xmlDimExe, xmlDimExo, xmlDimTih, xmlDimToh,
    xmlDimScale, xmlDimENBL, xmlDimTXTALIGN, xmlDimSah, xmlDimPost, xmlDIMTP, xmlDimTm,
    xmlDimDSep, xmlDimLUnit, xmlDimFrac, xmlDimAlt, xmlDimAltF, xmlDimAPost);

  cnstIngoreNames2D: array [0..3] of string = (cnstXmlIconIndex, cnstXmlCanShow,
    cnstXmlVisible3D, cnstXmlPathKey);

  cnstIngoreNames3D: array [0..6] of string = (cnstXmlIconIndex, cnstXmlCanShow,
    cnstXmlVisible, cnstXmlHandle3D, cnstXmlPathKey, cnstXmlVisible3D,
    cnstxmlAlphaBlend);//cnstXmlColorVector

{$IFDEF SG_BTI}
type
  TsgBTIXMLId = (bxmlUndefined, bxmlFlags, bxmlAlignment, bxmlCarvingMode,
    bxmlFillColor, bxmlIsCircular, bxmlLength, bxmlSegmentsCount,
    bxmlSegmentWidths, bxmlTypeSnapEntities, bxmlWidth, bxmlValueMask,
    bxmlInsUnits, bxmlLineType, bxmlParent, bxmlAttributes, bxmlAttribut,
    bxmlName, bxmlValue, bxmlType, bxmlValues, bxmlDelimiter,
    bxmlBoundaryVisible, bxmlAreaFigure, bxmlKoef, bxmlDimStyle,
    bxmlBlockPattern, bxmlRef, bxmlByteArray, bxmlIntArray,
    bxmlDoubleArray, bxmlStringArray, bxmlRefArray, bxmlItem,
    bxmlCustomFields, bxmlHandle, bxmlDescriptions, bxmlEntDescription,
    //ver 1.0
    bxmlEditorFlags, bxmlDiffWidths, bxmlSegmentsByLength, bxmlSegmentsLength,
    bxmlStairs, bxmlSegmentBegin, bxmlSegmentEnd, bxmlSegmentGap,
    bxmlFlightOfSteps, bxmlGapOffset, bxmlGapAngle, bxmlElementFlags,
    bxmlAreaFlags, bxmlLabel, bxmlSign, bxmlLayer, bxmlNumber, bxmlNumberLetter,
    bxmlNumberSub, bxmlAreaProperties, bxmlComplexProperties, bxmlElementProperties,
    bxmlPoint, bxmlComplexFlags, bxmlAccuracy, bxmlEpsilon, bxmlMarkPoints,
    bxmlFlipFlags, bxmlVertexMode, bxmlCondition, bxmlCategory
    );

  TsgBTIXMLType = record
    Id: TsgBTIXMLId;
  end;

  TsgGetXMLBTITypeProc = function(const AName: string): TsgBTIXMLType;

const
  cnstBTIExtData = 'EXTDATA';
  cnstBTIXMLNames: array[TsgBTIXMLId] of TsgXMLIdItem = (
    (Name: 'Undefined'; ValueType: dtUndefined; Description: ''),
    (Name: 'Flags'; ValueType: dtByte; Description: ''),
    (Name: 'Alignment';),
    (Name: 'CarvingMode';),
    (Name: 'FillColor';),
    (Name: 'IsCircular';),
    (Name: 'Length';),
    (Name: 'SegmentsCount';),
    (Name: 'SegmentWidths';),
    (Name: 'TypeSnapEntities';),
    (Name: 'Width';),
    (Name: 'ValueMask';),
    (Name: 'InsUnits';),
    (Name: 'LineType';),
    (Name: 'Parent';),
    (Name: 'Attributes';),
    (Name: 'Attribut';),
    (Name: 'Name';),
    (Name: 'Value';),
    (Name: 'Type';),
    (Name: 'Values';),
    (Name: 'Delimiter';),
    (Name: 'BoundaryVisible';),
    (Name: 'AreaFigure';),
    (Name: 'Koef';),
    (Name: 'DimStyle';),
    (Name: 'BlockPattern';),
    (Name: 'Ref';),
    (Name: 'ByteArray';),
    (Name: 'IntArray';),
    (Name: 'DoubleArray';),
    (Name: 'StringArray';),
    (Name: 'RefArray';),
    (Name: 'Item';),
    (Name: 'CustomFields'),
    (Name: 'Handle'),
    (Name: 'Descriptions'),
    (Name: 'EntDescription'),
    //ver 1.0
    (Name: 'EditorFlags'),
    (Name: 'DiffWidths'),
    (Name: 'SegmentsByLength'),
    (Name: 'SegmentsLength'),
    (Name: 'Stairs'),
    (Name: 'SegmentBegin'),
    (Name: 'SegmentEnd'),
    (Name: 'SegmentGap'),
    (Name: 'FlightOfSteps'),
    (Name: 'GapOffset'),
    (Name: 'GapAngle'),
    (Name: 'ElementFlags'),
    (Name: 'AreaFlags'),
    (Name: 'Label'),
    (Name: 'Sign'),
    (Name: 'Layer'),
    (Name: 'Number'),
    (Name: 'Letter'),
    (Name: 'NumberSub'),
    (Name: 'AreaProps'),
    (Name: 'ComplexProps'),
    (Name: 'ElementProps'),
    (Name: 'Point'),
    (Name: 'ComplexFlags'),
    (Name: 'Accuracy'),
    (Name: 'Epsilon'),
    (Name: 'MarkPoints'),
    (Name: 'FlipFlags'),
    (Name: 'VertexMode'),
    (Name: 'Condition'),
    (Name: 'Category')
  );
{$ENDIF}

  cnstOptionsDecimalSeparator = '.';

  cnstMaxSaveHandleXML = 200;
  cnstMaxSaveHandleXMLError = cnstMaxSaveHandleXML+1;
  cnstSignHandle = cnstAt;
  cnst3DHadnle = cnstStar;
  cnstXMLPrefix = 'cst';
  cnstXMLValue = 'Value';
  cnstXMLEnabled = 'Enabled';
  cnstXMLValues = 'Values';
  cnstXMLValuesSeparator = '|';
  cnstXMLSuccess = 'Success';
  cnstXMLError = 'Error';
  cnstXMLErrors = 'Errors';
  cnstXMLResult = 'Result';
  cnstXMLResults = 'Results';
  cnstXMLMessage = 'Message';
  cnstXMLName = 'Name';
  cnstXMLCaption = 'Caption';
  cnstXMLOutput = 'Output';
  cnstXMLCommand = 'Command';
  cnstXMLInstruction = 'Instruction';
  cnstXMLValueError = 'value error';
  cnstXMLType = 'Type';
  cnstXML3d = '3d';
  cnstXMLBox = 'Box';
  cnstXMLAreaRoot = 'AreaRoot';
  cnstXMLTypeEvent = 'event';
  cnstXMLTypeCommand = 'command';
  cnstXMLException = 'Exception';
  cnstXMLCreated = 'Created';
  cnstXMLDeleted = 'Deleted';
  cnstXMLRemoved = 'Removed';
  cnstXMLUpdated = 'Updated';
  cnstXMLPosition = 'Position';
  cnstXMLParserError = 'Parser' + cnstXMLError;
  cnstXMLIntfaceVersion: TsgVersion = (Major: 2; Minor: 0);
  cnstXMLWarning = 'Warning';
  cnstXMLWarnings = cnstXMLWarning + 's';
  cnstXMLMeasure = 'Measure';
  cnstXMLId = 'Id';
  cnstXMLHandle = 'Handle';
  cnstXMLGlobalFunction = 'GlobalFunction';
  cnstXMLSection = 'Section';
  cnstXMLInfo = 'Info';
  cnstXMLIndex = 'Index';
  cnstXMLCount = 'Count';
  cnstXMLMaxCount = 'MaxCount';
  cnstXMLPrinterName = 'Printer';
  cnstXMLSource = 'Source';
  cnstXMLDest = 'Destination';

  cnstDim3dAutoLoad: Boolean = False;
  cnstXMLDim3dFileExt = '.dim3d';
  cnstXMLDim3dNode = 'Measuring3d';
  cnstXMLDim3dScaleFactor = 'ScaleFactor';
  cnstXMLDim3dPrecisionFactor = 'PrecisionFactor';
  cnstXMLDim3dDisplayedUnits  = 'DisplayedUnits';
  cnstXMLDim3dInitialUnits = 'InitialUnits';
  cnstXMLDim3dBoxOffset = 'BoxOffset';
  cnstXMLDim3dDimDistance = 'DimDistance';
  cnstXMLDim3dDimRadius = 'DimRadius';
  cnstXMLDim3dDimPoint = 'DimPoint';
  cnstXMLDim3dDistance = 'Distance';
  cnstXMLDim3dRadius = 'Radius';
  cnstXMLDim3dCenter = 'Center';
  cnstXMLDim3dBegin = 'Start';
  cnstXMLDim3dEnd = 'End';
  cnstXMLDim3dNormal = 'Normal';
  cnstXMLDim3dPolylineType = 'Type';
  cnstXMLDim3dPolyline = 'Polyline';
  cnstXMLDim3dPolylineExt = 'PolylineExt';
  cnstXMLDim3dPosition = 'Pos';
  cnstXMLNameAccuracy = 'Accuracy';

  cnstXmlLayersBlocked = 'LayersBlocked';
  cnstXmlLayersNoSelected = 'LayersNoSelected';

   cnstXmlItemLayout = 'Layout';
   cnstXMLProperty = 'Property';

{consts}
  cnstGenIso = 'GENISO';
  cnstStandard = 'STANDARD';
  cnstGost = 'GOST';
  cnstLanguage = 'Language';
  cnstLangugesShortName: array [TsgLangugeID] of string = ('',
{$IFDEF SG_FIREMONKEY}
   'EN', 'RU', 'DE', 'FR', 'CN', 'JP', 'KR', 'ES', 'IT', 'NL', 'BR'
{$ELSE}
   'RU', 'CN', 'JP', 'KR'
{$ENDIF}
  );
  cnstDefaultLangID = 'en';

  cnstActiveStyles: array [TsgLangugeID] of string = (
{$IFDEF SG_FIREMONKEY}
    cnstStandard, cnstStandard, cnstStandard, cnstStandard, cnstStandard, cnstStandard,
    cnstStandard, cnstStandard, cnstStandard, cnstStandard, cnstStandard, cnstStandard
{$ELSE}

{$IFNDEF SG_EVACUATION}
  cnstGenIso, cnstGost,
{$ELSE}
  cnstStandard, cnstStandard,
{$ENDIF}
  cnstStandard, cnstStandard, cnstStandard
{$ENDIF}
  );

  cnstCadStyles: array [0..{$IFNDEF SG_EVACUATION}2{$ELSE}0{$ENDIF}] of record
    LangIds: TsgLangugeIDs;
    Name: string;
    Font: string;
    FFile: string;
  end = (
    (LangIds: []; Name: cnstStandard; Font: 'Arial'; FFile: 'Arial.ttf')
{$IFNDEF SG_EVACUATION}
    , (LangIds: [lgAny]; Name: cnstGenIso; Font: ''; FFile: 'Geniso.shx'),
      (LangIds: [lgRussian]; Name: cnstGost; Font: ''; FFile: 'Gost.shx')
{$ENDIF}
   );

  //bUseFastDraw: Boolean = True; //For future versions

  bAutoCapturePoint: Boolean = True;
  cnstShowLineWeightDefault: Boolean = True;
  cIPointsCnt = 8;
  cnstBadHandle = 0;
  cnstBorderSize: Double = 1 / 8;

// measure Global params
  cnstPrecisionFactorGlobal: Integer = 4;
  cnstInitialUnits: TsgInsUnits = iuUnitless;
  cnstDisplayedUnits: TsgInsUnits = iuUnitless;
  cnstLinearDimensionsFactorGlobal: Double = 1;
//

  cnstClassID_HTML = 2;
  cnstClassID_SVG = 1;
  cnstClassID_XML = 0;

//BTI constants
  cnstConstruction = 0;
  cnstComplex = 1;
  cnstElement = 2;
  cnstComplexBroad = 3;
  cnstComplexLinear = 4;
  cnstElementBroad = 5;
  cnstElementCarved = 7;
  cnstElementLinear = 6;
  cnstElementModifier = 8;
  cnstArea = 16;
  cnstComplexArea = 17;
  cnstLabel = 32;
  cnstDimLabel = 33;
  cnstDimConstruction = cnstMaxByte;

  cnstBlockPattern = MaxInt;
  cnstBlockDescription1 = cnstBlockPattern - 1;

  cnstBTIEntTypeEx: array [0..13] of Integer = (cnstConstruction, cnstComplex,
    cnstElement, cnstComplexBroad, cnstComplexLinear, cnstElementBroad,
    cnstElementCarved, cnstElementLinear, cnstElementModifier, cnstArea,
    cnstComplexArea, cnstDimConstruction, cnstLabel,  cnstDimLabel);

  cnstDefaultGapAngle = 135;
  cnstDefaultGapSegment = 200;

//{$IFDEF SG_EVACUATION} for future version
//  cnstDefaultTTFFont = 'Arial';
//{$ELSE}
//  cnstDefaultTTFFont = {$IFDEF SG_BTI_INTERFACE}'Gost'{$ELSE}'Arial'{$ENDIF};
//{$ENDIF}
{$IFDEF SG_FM_ANDROID}
  cnstDefaultTTFFont = 'Roboto';
  cnstDefaultTTFFontForBigFont = 'Roboto';
{$ELSE}
  cnstDefaultTTFFont = {$IFNDEF LINUX}'Arial'{$ELSE}'FreeSans'{$ENDIF};
  cnstDefaultTTFFontForBigFont = {$IFNDEF LINUX}'Arial Unicode MS'{$ELSE}'FreeSans'{$ENDIF};
{$ENDIF}

  cnstDefaultPenWidth = {$IFNDEF SG_FIREMONKEY}0{$ELSE}0.2{$ENDIF};

  cnstDefColor: Integer = clWhite;
  cnstLoop4: array [-3..7] of Integer = (1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3);

  cnstSizePrecision: array [0..8] of Byte = (0, 1, 2, 3, 4, 5, 6, 7, 8);
  cnstSizeScale: array [0..24] of Double = (10000, 5000, 2000, 1000, 500, 200,
    100, 50, 20, 10, 5, 2, 1, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002,
    0.001, 0.0005, 0.0002, 0.0001);
  cnstUndefined = -1;
  cnstUseProxy: Boolean = False;
  cnstSSLUseAnyCertificate: Boolean = True;
  DXF_3DFACE      = 20;
  DXF_ACIS        = 107;
  DXF_ACIS_BEGIN  = 106;
  DXF_ACIS_END  = 108;
  DXF_ARC          = 9;
  DXF_ATTDEF      = 17;
  DXF_ATTRIB      = 23;
  DXF_BEGIN_INSERT   = 102;
  DXF_BEGIN_POLYLINE = 100;
  DXF_BEGIN_VIEWPORT = 104;
  DXF_BLOCK        = 2;
  DXF_CIRCLE      = 8;
  DXF_DIMENSION    = 14;
  DXF_ELLIPSE      = 18;
  DXF_END_INSERT     = 103;
  DXF_END_POLYLINE   = 101;
  DXF_END_VIEWPORT   = 105;
  DXF_HATCH        = 21;
  DXF_IMAGE_ENT       = 22;
  DXF_INSERT      = 13;
  DXF_LAYER        = 4;
  DXF_LINE        = 6;
  DXF_LTYPE       = 3;
  DXF_LWPOLYLINE    = 11;
  DXF_MTEXT        = 16;
  DXF_POINT        = 19;
  DXF_POLYLINE     = 10;
  DXF_SEC_BLOCKS    = 1;
  DXF_SEC_ENTITIES  = 2;
  DXF_SEC_LAYERS    = 4;
  DXF_SEC_LTYPE    = 3;
  DXF_SEC_TABLES    = 0;
  DXF_SOLID        = 7;
  DXF_SPLINE      = 12;
  DXF_TABLE        = 1;
  DXF_TEXT        = 15;
  DXF_UNKNOWN       = 0;
  DXF_VERTEX      = 5;
  fDXFLineWeightByBlock: Double = -1.0;
  fDXFLineWeightByLayer: Double = -2.0;
  fDXFLineWeightDefault: Double = -3.0;
  fLineWeightByBlock: Double = -2.0;
  fLineWeightByLayer: Double = -1.0;
  fLineWeightDefault: Double = -3.0;
  fLineWeightFactor: Double = 1.0;
  cnstThicknessDefault = 0.0;
  // Windows text height factor
  fWinTextHeightFactor: Extended = 1.6000000000;
  // Windows text Y-offset factor by height
  fWinTextYOffsetFactor: Extended = 0.2700000000;
  // Default numbers of parts for parsing circle
  iDefaultNumberOfCircleParts = 48;
  // Default numbers of parts for parsing spline
  iDefaultNumberOfSplineParts = 24;
  //Min numbers of parts for parsing spline and circle
  iMinNumberOfPart = 5;
  //Max numbers of parts for parsing spline and circle
  iMaxNumberOfPart = 255;
  iCurrentNumberOfCircleParts: Integer = iDefaultNumberOfCircleParts;
  iCurrentNumberOfSplineParts: Integer = iDefaultNumberOfSplineParts;
  iCurrentDispersionFactor: Double = 0;//15;
  iMaxNumDottedLines: Integer = 8000; { Maximum count of the lines in a dotted polyline
                                        or curve (if count of lines more then
                                        iMaxNumDottedLines we draws solid-polyline) }
  iMinNumberOfCircleParts = 4;
  cnstRegenerateArcs = False;
  bRegenerateArcs: Boolean = cnstRegenerateArcs;
  cnstHighQualityRegenerateArcs: Boolean = False;
  cnstDegreeOfSpline = 3;
  sgDXFLineWeights: array [0..23] of Integer = (0, 5, 9, 13, 15, 18, 20, 25,
    30, 35, 40, 50, 53, 60, 70, 80, 90, 100, 106, 120, 140, 158, 200, 211);
{$IFDEF SG_UNITS_MM}
  fCellMargin = 6.0;
  fDoubleLineInterval = 4.5;
{$ELSE}
  fCellMargin = 0.06;
  fDoubleLineInterval = 0.045;
{$ENDIF}
  fFixToleranceInc: Double = 0.01;
  cnstFitTol = 0.0000000001;
  cnstKnotTol = 0.0000001;
  cnstCtrlTol = 0.0000001;
  iDefaultOperator: Integer = 0;

  fTransparencyByBlock: Single = -1.0;
  fTransparencyByLayer: Single = -2.0;

{$IFDEF SG_BTI}
  {$IFDEF SG_BTI_MOLDOVA}
  BTIDefaultOutlineType: Integer = 1;
  {$ELSE}
  BTIDefaultOutlineType: Integer = 0;
  {$ENDIF}
{$ENDIF}
  cnst2DCurvePoly = 0;
  cnst2DCurveLine = 1;
  cnst2DCurveArc = 2;
  cnst2DCurveEllipse = 3;
  cnst2DCurveSpline = 4;
  cnst2DCurveUndefined = 255;

  cnstSoftwareSoftGold = 'Software\SoftGold\';
  cnstCompany = 'Soft Gold';
  cnstCompanyCST = 'CADSoftTools';
  cnstCSTCADNavigator = 'CST CAD Navigator';
  cnstCSTCADNavigatorUnited = 'CST'  + #0160 + 'CAD' + #0160 + 'Navigator';

  cnstResources = 'Resources';
  cnstSHXPath = 'SHX';

  SHX_FONTTYPE = 8;
  cnstSHX_FONTTYPE: TsgNativePointer = (SHX_FONTTYPE shl 24);
  cnstSHX_FONTSHAPE: TsgNativePointer = $10000000;

  cntCRC8Table: array[Byte] of Word = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
    $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
    $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
    $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
    $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
    $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
    $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
    $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
    $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
    $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
    $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
    $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
    $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
    $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
    $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
    $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
    $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
    $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
    $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
    $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
    $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
    $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
    $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
    $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
    $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
    $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
    $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
    $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
    $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
    $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040);

  cntCRC32Table: array[Byte] of Cardinal = (
    $00000000, $77073096, $ee0e612c, $990951ba,
    $076dc419, $706af48f, $e963a535, $9e6495a3,
    $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988,
    $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
    $1db71064, $6ab020f2, $f3b97148, $84be41de,
    $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
    $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
    $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
    $3b6e20c8, $4c69105e, $d56041e4, $a2677172,
    $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
    $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940,
    $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
    $26d930ac, $51de003a, $c8d75180, $bfd06116,
    $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924,
    $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
    $76dc4190, $01db7106, $98d220bc, $efd5102a,
    $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
    $7807c9a2, $0f00f934, $9609a88e, $e10e9818,
    $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
    $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
    $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
    $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c,
    $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
    $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2,
    $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
    $4369e96a, $346ed9fc, $ad678846, $da60b8d0,
    $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086,
    $5768b525, $206f85b3, $b966d409, $ce61e49f,
    $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4,
    $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
    $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a,
    $ead54739, $9dd277af, $04db2615, $73dc1683,
    $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
    $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
    $f00f9344, $8708a3d2, $1e01f268, $6906c2fe,
    $f762575d, $806567cb, $196c3671, $6e6b06e7,
    $fed41b76, $89d32be0, $10da7a5a, $67dd4acc,
    $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
    $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252,
    $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60,
    $df60efc3, $a867df55, $316e8eef, $4669be79,
    $cb61b38c, $bc66831a, $256fd2a0, $5268e236,
    $cc0c7795, $bb0b4703, $220216b9, $5505262f,
    $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04,
    $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
    $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
    $9c0906a9, $eb0e363f, $72076785, $05005713,
    $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38,
    $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
    $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e,
    $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
    $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
    $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2,
    $a7672661, $d06016f7, $4969474d, $3e6e77db,
    $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0,
    $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
    $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6,
    $bad03605, $cdd70693, $54de5729, $23d967bf,
    $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
    $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d);

  arDWGCodePages: array[0..60] of Word = (
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, //  0..20
     0, 0, 0, 0, 0, 0, 0, //  21..27
     1250, //  28   ANSI_1250  ANSI - Central European
     1251, //  29   ANSI_1251  ANSI - Cyrillic
     1252, //  30   ANSI_1252  ANSI - Latin I
     0, //  31
     1253, //  32   ANSI_1253  ANSI - Greek
     1254, //  33   ANSI_1254  ANSI - Turkish
     1255, //  34   ANSI_1255  ANSI - Hebrew
     1256, //  35   ANSI_1256  ANSI - Arabic
     1257, //  36   ANSI_1257  ANSI - Baltic
     874,  //  37   ANSI_874   ANSI/OEM - Thai (same as 28605, ISO 8859-15)
     932,  //  38   ANSI_932   ANSI/OEM - Japanese, Shift-JIS
     936,  //  39   ANSI_936   ANSI/OEM - Simplified Chinese (PRC, Singapore)
     949,  //  40   ANSI_949   ANSI/OEM - Korean (Unified Hangul Code)
     950,  //  41   ANSI_950   ANSI/OEM - Traditional Chinese (Taiwan;
           //                  Hong Kong SAR, PRC)
     1361, //  42   ANSI_1361  Win Korean Johab
     1200, //  43   ANSI_1200  Unicode (reserved)
     1258, //  44   ANSI_1258  ANSI/OEM - Vietnamese
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  //  45..60
  );

  arDWGVersions: array[TsgDWGVersion] of Byte = (16, 17, 18, 19, 20, 21, 23,
    25, 27, 29, 31, 32); //32??

const
{$IFDEF CADDLL}
  cnstCADImageLibName = {$IFDEF MSWINDOWS}'CAD.dll'{$ELSE}{$IFDEF SG_LIBCAD_GTK2}'libcad-gtk2.so'{$ELSE}'libcad.so'{$ENDIF}{$ENDIF};
{$ELSE}
  cnstCADImageLibName = 'CADImage.dll';
{$ENDIF}

const
  cnstSizeModeName: array[TSizeMode] of string =
    ('Actual', 'Maximum', 'Custom', 'Same');
  cnstTiffCompessionName: array[TsgTIFFCompressionType] of string =
    ('LZW', 'Deflate', 'JPEG', 'CCITT3', 'CCITT4', 'CCITT6', 'Rle', 'Auto', 'None');
  cnstPixelFormat: array[TPixelFormat] of string =
    ('Device', '1bit', '4bit', '8bit', '15bit', '16bit', '24bit', '32bit', 'Custom');

  cntClassDXFIMAGE = 'IMAGE';
  cntClassDXFMPOLYGON = 'MPOLYGON';
  cntClassDXFIMAGEDEF = 'IMAGEDEF';
  cntClassDXFIMAGEDEFREACTOR = 'IMAGEDEF_REACTOR';
  cntClassDXFWIPEOUT = 'WIPEOUT';
  cntClassDXFWIPEOUTVARIABLES = 'WIPEOUTVARIABLES';
  cntClassDXFACDB_MLEADERSTYLE_CLASS = 'ACDB_MLEADERSTYLE_CLASS';
  cntClassDXFEXAC_ESW = 'EXAC_ESW';
  cntClassDXFTABLESTYLE = 'TABLESTYLE';
  cntClassDXFACAD_TABLE = 'ACAD_TABLE';
  cntClassDXFACDBDICTIONARYWDFLT = 'ACDBDICTIONARYWDFLT';
  cntClassDXFSCALE = 'SCALE';
  cntClassDXFDICTIONARYVAR = 'DICTIONARYVAR';
  cntClassDXFCELLSTYLEMAP = 'CELLSTYLEMAP';
  cntClassDXFACDBPLACEHOLDER = 'ACDBPLACEHOLDER';
  cntClassDXFRASTERVARIABLES = sRasterVariables;
  cntClassDXFTABLEGEOMETRY = 'TABLEGEOMETRY';
  cntClassDXFLAYOUT = 'LAYOUT';
  cntClassDXFSPATIAL_FILTER = 'SPATIAL_FILTER';
  cntClassDXFXRECORD = cnstXRecord;
  cntClassDXFARC_DIMENSION = 'ARC_DIMENSION';
  cntClassDXFMESH = 'MESH';

  cntImageExportAsImage = 0;
  cntImageExportAsOLE2Frame = 1;
  cntImageExportAsFile = 2;

  cnstOleFrameDefault = 2;

  cnstWipeoutVariablesDisplayImageFrame = True;
  cnstRasterVariablesDisplayImageFrame = True;

  cntCPlusPlusClassNameRasterImage = cntAcDb + 'RasterImage';
  cntCPlusPlusClassNameRasterImageDef = cntAcDb + 'RasterImageDef';
  cntCPlusPlusClassNameImageDefReactor = cntCPlusPlusClassNameRasterImageDef + 'Reactor';
  cntCPlusPlusClassNameWipeout = cntAcDb + 'Wipeout';
  cntCPlusPlusClassNameWipeoutVariables = cntAcDb + 'WipeoutVariables';
  cntCPlusPlusClassNameMPolygon = cntAcDb + 'MPolygon';
  cntCPlusPlusClassTableStyle = cntAcDb + 'TableStyle';
  cntCPlusPlusClassTable = cntAcDb + 'Table';
  cntCPlusPlusClassDictionaryWithDefault = cntAcDb + 'DictionaryWithDefault';
  cntCPlusPlusClassScale = cntAcDb + 'Scale';
  cntCPlusPlusClassDictionaryVar = cntAcDb + 'DictionaryVar';
  cntCPlusPlusClassCellStyleMap = cntAcDb + 'CellStyleMap';
  cntCPlusPlusClassPlaceHolder = cntAcDb + 'PlaceHolder';
  cntCPlusPlusClassLayout = cntAcDb + 'Layout';
  cntCPlusPlusClassRasterVariables = cntAcDb + 'RasterVariables';
  cntCPlusPlusClassTableGeometry = cntAcDb + 'TableGeometry';
  cntCPlusPlusClassSpatialFilter = cntAcDb + cnstSpatialFilterMarker;
  cntCPlusPlusClassXrecord = cntAcDb + sXrecord;
  cntCPlusPlusClassArcDimesion = cntAcDb + 'ArcDimension';
  cntCPlusPlusClassMesh = cntAcDb + 'SubDMesh';

  cntAppNameISM = 'ISM';
  cntAppNameWipeOutDXF = '"WipeOut"';
  cntAppNameWipeOutDWG = 'WipeOut|AutoCAD Express Tool|expresstools@autodesk.com';
  cntAppNameAcMPolygonObj15DXF = '"AcMPolygonObj15"';
  cntAppNameAcMPolygonObj15DWG = 'AcMPolygonObj15|Version(1.0.0.0) Product Desc:     Object enabler for the AcDbMPolygon entity|Company:          Autodesk,Inc.|WEB Address:      www.autodesk.com';
  cntAppNameObjectDBX = 'ObjectDBX Classes';

  cntItemClassIDEntity = $1F2;
  cntItemClassIDObject = $1F3;
  cntDWGObjCodeUNSUSED = 0;

  sAcadSectHeader = cntAcDb + ':Header';
  sAcadSectClasses = cntAcDb + ':Classes';
  sAcadSectHandles = cntAcDb + ':Handles';
  sAcadSectObjects = cntAcDb + ':AcDbObjects';
  sAcadSectSummaryInfo = cntAcDb + ':SummaryInfo';
  sAcadSectTemplate = cntAcDb + ':Template';
  sAcadSectPrototype = cntAcDb + ':AcDsPrototype_1b';
  sAcadSectSecurity = cntAcDb + ':Security';
  sAcadSectFileDepList = cntAcDb + ':FileDepList';
  sAcadSectVBAProject = cntAcDb + ':VBAProject';
  sAcadSectAppInfo = cntAcDb + ':AppInfo';
  sAcadSectPreview = cntAcDb + ':Preview';
  sAcadSectRevHistory = cntAcDb + ':RevHistory';
  sAcadSectObjFreeSpace = cntAcDb + ':ObjFreeSpace';
  sAcadSectAuxHeader = cntAcDb + ':AuxHeader';
  sAcadSectSignature = cntAcDb + ':Signature';

  cntDWGSectCount = 6;
  cntDWGSections: array[0 .. cntDWGSectCount] of WideString = (sAcadSectHeader,
    sAcadSectClasses, sAcadSectHandles, sAcadSectObjects, sAcadSectSummaryInfo,
    sAcadSectTemplate, sAcadSectAppInfo);

  cnstCRCInitUnknown = MaxInt;

  cnstFileHeaderCRCXOR: array[3..6] of Word = ($A598, $8101, $3CC4, $8461);
  cntFileHeaderEndSentinel: TsgDWGSentinel = ($95, $A0, $4E, $28, $99, $82,
    $1A, $E5, $5E, $41, $E0, $5F, $9D, $3A, $4D, $00);

  cnstHeaderVarCRCInit = $C0C1;
  cnstHeaderVariablesBeginSentinel: TsgDWGSentinel = ($CF, $7B, $1F,
    $23, $FD, $DE, $38, $A9, $5F, $7C, $68, $B8, $4E, $6D, $33, $5F);
  cnstHeaderVariablesEndSentinel: TsgDWGSentinel = ($30, $84, $E0, $DC,
    $02, $21, $C7, $56, $A0, $83, $97, $47, $B1, $92, $CC, $A0);

  cnstClassesSectionCRCInit = cnstHeaderVarCRCInit;
  cnstClassesSectionBeginSentinel: TsgDWGSentinel = ($8D, $A1, $C4, $B8,
    $C4, $A9, $F8, $C5, $C0, $DC, $F4, $5F, $E7, $CF, $B6, $8A);
  cnstClassesSectionEndSentinel: TsgDWGSentinel = ($72, $5E, $3B, $47,
    $3B, $56, $07, $3A, $3F, $23, $0B, $A0, $18, $30, $49, $75);

  cnstImageDataBeginSentinel: TsgDWGSentinel = ($1F, $25, $6D, $07, $D4, $36,
    $28, $28, $9D, $57, $CA, $3F, $9D, $44, $10, $2B);
  cnstImageDataEndSentinel: TsgDWGSentinel = ($E0, $DA, $92, $F8, $2B,
    $C9, $D7, $D7, $62, $A8, $35, $C0, $62, $BB, $EF, $D4);

  cnstSecondFileHeaderCRCInit = cnstHeaderVarCRCInit;
  cnstSecondFileHeaderBeginSentinel: TsgDWGSentinel = ($D4, $7B, $21, $CE,
    $28, $93, $9F, $BF, $53, $24, $40, $09, $12, $3C, $AA, $01);
  cnstSecondFileHeaderEndSentinel: TsgDWGSentinel = ($2B, $84, $DE, $31, $D7,
    $6C, $60, $40, $AC, $DB, $BF, $F6, $ED, $C3, $55, $FE);

  cnstObjectCRCInit = cnstHeaderVarCRCInit;
  cnstSectionObjMapCRCInit = cnstHeaderVarCRCInit;
  cnstSectionObjMapMaxSize = 2032;

  cntDWG2004Id: array[0..11] of AnsiChar = ('A', 'c', 'F', 's', 's', 'F', 'c', 'A',
   'J', 'M', 'B', #0);

  cntDWGObjHandleType0 = 0;
  cntDWGObjHandleType2 = 2;
  cntDWGObjHandleType3 = 3;
  cntDWGObjHandleType4 = 4;
  cntDWGObjHandleType5 = 5;
  cntDWGObjHandleType6 = 6;
  cntDWGObjHandleType8 = 8;
  cntDWGObjHandleTypeA = $A;
  cntDWGObjHandleTypeC = $C;
  cntDWGObjHandleTypeMax = cntDWGObjHandleTypeC;

  cntDWGObjHandleSoftOwner = cntDWGObjHandleType2;
  cntDWGObjHandleHardOwner = cntDWGObjHandleType3;
  cntDWGObjHandleSoftPointer = cntDWGObjHandleType4;
  cntDWGObjHandleHardPointer = cntDWGObjHandleType5;
  cntDWG2004SectionPageMap = $41630E3B;
  cntDWG2004SectionMap = $4163003B;
  cntDWG2004SectionData = $4163043B;
  cntDWG2004RSeedMul = $343FD;
  cntDWG2004RSeedPlus = $269EC3;
  cntDWG2004FileIDStr: AnsiString = 'AcFssFcAJMB';

  cntDWGSectHeaderIndex = 0;
  cntDWGSectClassesIndex = 1;
  cntDWGSectHandlesIndex = 2;
  cntDWGSectObjectsIndex = 3;
  cntDWGSectSummaryInfoIndex = 4;
  cntDWGSectTemplateIndex = 5;
  cntDWGSectAppInfoIndex = 6;
  cntDWGSectPrototypeIndex = 7;
  cntDWGSectMaxIndex = cntDWGSectPrototypeIndex;

  cnstDWGDictionaries: array[0..11] of string = (sNamedObjectsDictionary, sAcadGroupDictionary,
    sAcadMaterialDictionary, sAcadMLineStyleDictionary, sAcadLayoutDictionary,
    sAcadScaleListDictionary, sAcDbVariableDictionary, sAcadMLeaderStyleDictionary,
    sAcadWipeOutVarsDictionary, sDWGPropsDictionary, sAcadImageDict, sAcadImageVars);

  cntViewPortPerspectiveLensLength = 50;
  cntViewPortCircleZoomPercent = 100;
  cntImageBrightness = 50;
  cntImageContrast = cntImageBrightness;
  cntImageFade = 0;
  cntXCodeDrwPropBase = 1;
  cntXCodeDrwPropTitle = 2;
  cntXCodeDrwPropSubject = 3;
  cntXCodeDrwPropAuthor = 4;
  cntXCodeDrwPropComments = 6;
  cntXCodeDrwPropKeywords = 7;
  cntXCodeDrwPropLastSavedBy = 8;
  cntXCodeDrwPropRevisionNumber = 9;
  cntXCodeDrwPropCustomSummaryInfo1 = 300;
  cntXCodeDrwPropCustomSummaryInfo2 = 301;
  cntXCodeDrwPropCustomSummaryInfo3 = 303;
  cntXCodeDrwPropCustomSummaryInfo4 = 304;
  cntXCodeDrwPropCustomSummaryInfo5 = 305;
  cntXCodeDrwPropCustomSummaryInfo6 = 306;
  cntXCodeDrwPropCustomSummaryInfo7 = 307;
  cntXCodeDrwPropCustomSummaryInfo8 = 308;
  cntXCodeDrwPropCustomSummaryInfo9 = 309;
  cntXCodeDrwPropUnknown40 = 40;
  cntXCodeDrwPropUnknown41 = 41;
  cntXCodeDrwPropUnknown42 = 42;
  cntXCodeDrwPropHyperlinkBase = 1;
  cntXCodeDrwPropCustomSummaryInfoCount = 90;
  cntJulianDateHourDif: Integer = 4;
  cntDwgJulianDateHourDif = 9;
{$IFNDEF KEY_WOW64_32KEY}
  KEY_WOW64_32KEY        = $0200;
  {$EXTERNALSYM KEY_WOW64_32KEY}
{$ENDIF}
{$IFNDEF KEY_WOW64_64KEY}
  KEY_WOW64_64KEY        = $0100;
  {$EXTERNALSYM KEY_WOW64_64KEY}
{$ENDIF}

  cnstGeneratedHandlesCapacity = 1048576;

const
  //Warning message
  cnstWarning_OK = 0;
  cnstWarningMsgType_OK = 0;
  cnstWarningMsgType_InvalidVersion = 1;//invalid version bti entities
  cnstHttpTimeOut: Integer = 30000;
  cnstHttpTimeOutName: string = 'HttpTimeOut';
  cnstMaxHttpRedirects = 35;

  cnstSatBinFileBeginIndent: AnsiString = 'ACIS BinaryFile';
  cnstASMBinFileBeginIndent: AnsiString = 'ASM BinaryFile';
  cnstACISEndSignatureAutocad: AnsiString = 'End'#14#2'of'#14#4'ACIS'#13#4'data';
  cnstACISEndSignatureSpatial: AnsiString = 'End-of-ACIS-data';
  cnstACISSATDefLineBreak: AnsiString = #13#10;


{$IFDEF SG_BTI}
  cnstInvalidVersionBTIEntities = 'Invalid Version';
{$ENDIF}


  cnstNullPrecisionIndex = 3;
  cnstPrecisionList: array[0..7] of string = ('1000', '100', '10', '0', '0.0', '0.00',
    '0.000', '0.0000');

  cnstMeasureUnitsInM: array[TsgInsUnits] of Double = (// in meters
{0   Unspecified (No units)}-1,
{1   Inches}0.0254,
{2   Feet}0.3048,
{3   Miles}1609.34,
{4   Millimeters}0.001,
{5   Centimeters}0.01,
{6   Meters}1,
{7   Kilometers}1000,
{8   Microinches}0.0000000254,
{9   Mils}0.0000254,
{10  Yards}0.9144,
{11  Angstroms}1.0E-10,
{12  Nanometers}0.000000001,
{13  Microns}0.000001,
{14  Decimeters}0.1,
{15  Dekameters}10,
{16  Hectometers}100,
{17  Gigameters}1000000000,
{18  Astronomical Units}-1,//149597870691,
{19  Light Years}-1,//9460730472580800,
{20  Parsecs}-1,//30856775812799588,
{21  US Survey Feet}-1//0.3048006096-1
//last element
{$IFDEF SG_iuPixel}
, {22  Soft gold pixel - 1 inch = 96 pixels} 0.0254 / 96
{$ENDIF}
  );

  cnstMeasureUnitsKoef: array[TsgInsUnits] of Double = (-1, 1, 0.083333, 0.000015,
    25.4, 2.54,  0.0254, 0.0000254, 1000000, 1000, 0.0277778,
    -1{'Angstroms'},
    2.54e+7{Nanometers},
    25400, 0.254, 0.00254, {Microns, Decimeters, Decameters,}
    0.000254{'Hectometers'},
    2.54E-11{Gigameters},
    -1{'Astronomical units'},
    -1{'Light years'},
    -1{'Parsecs'},
    -1{'US Survey Feet'}
    {$IFDEF SG_iuPixel}
    , 96{'Pixel'}
    {$ENDIF}
    );

  cnstDefaultViewsName: array[TsgDXFViewDirection] of string = ('Initial View',
    'Top View', 'Bottom View', 'Left View', 'Right View', 'Front View', 'Back View',
    'SW Isometric', 'SE Isometric', 'NW Isometric', 'NE Isometric', '', '');

  cnstFontStylesMask: TmvFontStyles = [fmBold, fmItalic, fmUnderline,
    fmStrikeOut, fmCondensed, fmUpward, fmDownward];

const
  cnstAluminium = 'Aluminium';
  cnstPlastic = 'Plastic';
  cnstRubber = 'Rubber';
  cnstAir = 'Air';

const
  cnstLispCommandSendOk = 0;
  cnstLispCommandSendError = 1;
  cnstLispCommandSendResult = -1;

const
{$IFDEF SG_FM_MOBILE}
  cnstStrBegin = Low(string);
{$ELSE}
  cnstStrBegin = 1;
{$ENDIF}

  cnstSnapDataEmpty: TSnapData = ();

  cnstTTFModeDefault: TsgTTFMode =
    {$IFDEF SG_FIREMONKEY}
      ttfPolyFillAndEdge
    {$ELSE}
    {$IFDEF SG_AGG2D}
      ttfPolyPolygon
    {$ELSE}
      ttfAuto
    {$ENDIF}
    {$ENDIF};

var
  bTransparencyToFills: Boolean = True;
  //bUseTTFFonts: Boolean = False;
  bCADSetSHXOptionsInit: Boolean = False;
  bSearchSHXPaths: Boolean = True;
  bUseGDIPlusForRastImgRotate: Boolean = True;
  bUseSHXFonts: Boolean = True;
  bNoSelectEntitiesOnLockedLayer: Boolean = False;
  bEasyEditing: Boolean = True;
  bEasyDrawing: Boolean = True;

  iEntitiesMouseMode: Integer = 0;//1 - mouse move, 2 - mouse click
  iEntitiesMouseSize: Integer = 16;

  CADPreview: Boolean = False;
  KeepOriginal: Boolean = False; // save/load/assign original stream
  RenderThroughAcAuthEnviron: Boolean = True;
  sDefaultSHXFont: string = 'simplex.shx';//'txt.shx';
  sDefaultSHXBigFont: string = 'bigfont.shx';
  sDefaultSHXPath: string = '';
  sDefaultTTFFont: string = cnstDefaultTTFFont;
  sDefaultTTFFontForBigFont: string = cnstDefaultTTFFontForBigFont;
  sgClientRect: TRect = (Left:MaxInt; Top:MaxInt; Right:-MaxInt; Bottom:-MaxInt);
  sgClientRectForSnap: TRect;
  sgVisibleArea: TPoint; //!Snap!
  sSHXSearchPaths: string = '';
  vLogs: TList = nil;
  XMLIds: TStringList = nil;
//svg group begin
  KoefDeviceCaps: Double = -1;
  ScreenResolution: TPoint = (X: -1; Y:-1);
//svg group end
{$IFDEF SG_DEBUG_OUTPUT}
  gFileName: string = '';
  gDebugOutputPath: string = 'd:\test\';
{$ENDIF}
{$IFDEF SG_MODULE_LOG}
  LogFileName: string = '';
  Log: TStringList = nil;
{$ENDIF}
{$IFDEF SG_BTI}
  XMLBTITypeProc: TsgGetXMLBTITypeProc  = nil;

function GetXMLBTITypeEx(const AName: string): TsgBTIXMLType;
function GetBTIContolType(const AIndex: Integer): TsgBTIContolType;

function IndexOfNameXml(const AName: string; const ANames: array of TsgBTIXMLId): Integer;
{$ENDIF}
function GetActualUnits(const AIUnits: TsgInsUnits): TsgInsUnits;
function GetUnitsSize(const AIUnits: TsgInsUnits): Double;
function GetDataIdType(const AStr: string): TsgDataId;

function InsertSpace(S: string): string;
function SetNumberFromStr(var ANumber: Integer; var AStr: string): string;
function IsDigit(AChar: Char): Boolean; {$IFNDEF UNICODE} overload; {$ENDIF}
{$IFNDEF UNICODE}
function IsDigit(AChar: WideChar): Boolean; overload;
{$ENDIF}

function GetNumberOfCircleParts: Integer;
function GetNumberOfSplineParts: Integer;
function GetXMLHelp(AXMLType: TsgXMLType): string;

procedure SetNumberOfCircleParts(const ANumber: Integer);
procedure SetNumberOfSplineParts(const ANumber: Integer);

function GetDispersionFactor: Double;
function HasDispersionFactor: Boolean;
procedure SetDispersionFactor(const AValue: Double);


{convert functions}
function ConvertToMeasurePolylineOptions(const AValues: Integer): TMeasurePolylineOptions;
function ConvertFromMeasurePolylineOptions(const AValues: TMeasurePolylineOptions): Integer;

//get string for TsgCADEntityType
function GetCADEntityTypeString(ACADEntityType: TsgCADEntityType): string;
{
  function GetPaperIndex

  AName - paperspace block name

  Function return paperspace index
   -1:
    0:   AName = '[*|$]MODEL_SPACE';
    1:   AName = '[*|$]PAPER_SPACE';
    2:   AName = '[*|$]PAPER_SPACE0';
    3:   AName = '[*|$]PAPER_SPACE1';
    ...
    N+2:  [*|$]PAPER_SPACE[N];

  Note: result is not layout index!!!
}
function GetPaperIndex(const AName: string): Integer;

function SplitAnonymousName(const AName: string; var ARoot: string;
  AIndex: PInteger = nil): Boolean;

{make and create functions}

function CreatePFPoint2DProc(const AX, AY, AZ: TsgFloat): Pointer;
function CreatePFPointProc(const AX, AY, AZ: TsgFloat): Pointer;
function CreateParamData: PsgParamData;
function CreatePsgCurveLink(const AFlag: Byte): PsgCurveLnk;
function MakeArcR(const ACenter: TFPoint; const ARadius, AAngleS, AAngleE: Double): TsgArcR;
procedure MakeArcs(const AArcs: TsgArcsParams; var AArcR1, AArcR2: TsgArcR);
procedure MakeArcsParams(const AArcR1, AArcR2: TsgArcR; var AArcs: TsgArcsParams);
function MakePointFrom3D(const APoint: TFPoint; const AGDI: Boolean = False): TPoint;
function MakeDXFFromFPoint(const AFPoint: TFPoint): TdxfPoint;
function MakeDXFPoint(const AX, AY, AZ: Single): TdxfPoint;
function MakeF2DPoint(const AX, AY: TsgFloat): TF2DPoint;
function MakeF2DPointFrom3D(const APoint: TFPoint): TF2DPoint;
function MakeF2DPointFromPoint(const APoint: TPoint): TF2DPoint;
function MakeF2DRect(const ALeft, ATop, ARight, ABottom: TsgFloat): TF2DRect;
function MakeF2DRectByPoints(const ALeftTop, ARightBottom: TF2DPoint): TF2DRect;
function MakeF2dRectFromFRect(ARect: TFRect): TF2dRect;
function MakeF2DLine(const A, B: TF2DPoint): TF2DLine;
function MakeF2DLineFrom3D(const ALine: TsgLine): TF2DLine;
function MakeFRectFromF2dRect(ARect: TF2dRect): TFRect;
function MakeFRectFromRect(const ARect: TRect): TFRect;
function MakeFPoint(const AX, AY : TsgFloat; const AZ: TsgFloat = 0): TFPoint;
function MakeFPointFrom2D(const APoint: TF2DPoint): TFPoint;
function MakeFPointFromDXF(const AdxfPoint: TdxfPoint): TFPoint;
function MakeFPointFromPoint(const APoint: TPoint): TFPoint; overload;
function MakeFPointFromPoint(const APoint: TPointF): TFPoint; overload;
function MakePointF(X, Y: Single): TPointF; overload;
function MakePointF(X, Y: Integer): TPointF; overload;
function MakePointF(const P: TPoint): TPointF; overload;
function MakeFRect(const ALeft, ATop, AZ1, ARight,
  ABottom, AZ2: TsgFloat): TFRect;
function MakeFRect2D(const ALeft, ATop, ARight, ABottom: Extended): TFRect;
function MakeFRectByPoints(const ALeftTop, ARightBottom: TFPoint): TFRect;
function MakeFRectByF2DPoints(const ALeftTop, ARightBottom: TF2DPoint): TFRect;
function MakeLine(const APoint1, APoint2: TFPoint): TsgLine;
procedure MakePoints3(const AP1, AP2, AP3: TFPoint; APoints3: PDouble);
procedure MakePoints4(const AP1, AP2, AP3, AP4: TFPoint; APoints4: PDouble);
procedure MakePoints4L(const AList: TList; APoints4: PDouble);
function MakeRectFromFRect(const ARect: TFRect): TRect;
function MakeRectFromF2DRect(const ARect: TF2DRect): TRect;
function MakeRightPath(APath: string): string;
function MakeViewBox(const AX, AY, AWidth, AHeight: Double): TsgViewBox;
function MakeVersion(const AMajor, AMinor: Integer): TsgVersion;
function MakeHyperLink(const AName, AValue: string): string;
function NewFPoint(const APoint: TFPoint): PFPoint; overload;
function NewFPoint(const AX, AY : TsgFloat; const AZ: TsgFloat = 0): PFPoint; overload;
procedure SetMinMaxUInt64(const AHandle: UInt64; var AMin, AMax: UInt64);{$IFDEF SG_INLINE}inline;{$ENDIF}

function sgFloorZ(const AValue: Double): Int64;{$IFDEF SG_INLINE}inline;{$ENDIF}
function sgMod(const AValue, ABase: Double): Double;{$IFDEF SG_INLINE}inline;{$ENDIF}
function sgModAngle(const AValue: Double; var AModValue: Double;
  const ARadian: Boolean = False): Boolean;{$IFDEF SG_INLINE}inline;{$ENDIF}

{$IFNDEF  SGFPC}
{$IFNDEF SGDEL_XE2}
function RectF(Left, Top, Right, Bottom: Single): TRectF;
{$ENDIF}
{$ENDIF}

{Log procedure}
procedure FileLoging(const AParam: TsgLogParam); overload;
procedure FileLoging(const AOperation: TsgLogOperation; const AParam: Pointer;
  const AFileName: string = ''; const AId: UInt64 = 0); overload;
procedure RegisterLogger(const ALogger: TsgLogger);
function UnRegisterLoger(const ALogger: TsgLogger): Boolean;

{Supports for older versions}
{$IFNDEF SGDEL_2009}
function AnsiStrAlloc(ASize: Cardinal): PAnsiChar;
{$ENDIF}
function BadRect: TFRect;
function BadPoint: TFPoint;
function MaxTsgFloat: Extended;
function GetRectWidth(const ARect: TRect): Integer;{$IFDEF SG_INLINE}inline;{$ENDIF}
function GetRectHeight(const ARect: TRect): Integer;{$IFDEF SG_INLINE}inline;{$ENDIF}

//this functions uses in demos 
procedure ClearList(AList: TList; ClearCapacity: Boolean = True);
procedure ClearListOfList(AListOfList: TList; ClearCapacity: Boolean = True);
procedure ClearListWithNilCheck(AList: TList; ClearCapacity: Boolean = True);
procedure ClearObjects(const AStrings: TStrings; Clear: Boolean = True);
procedure ClearParamData(const APParam: PsgParamData);
procedure ClearRecordList(AList: TList; ClearCapacity: Boolean = True);
procedure ClearRecordListOfList(AListOfList: TList; ClearCapacity: Boolean = True);
procedure ClearRecordObjects(const AStrings: TStrings; Clear: Boolean = True);
procedure ClearObjectObjects(const AStrings: TStrings; Clear: Boolean = True);
procedure DisposeAndNil(var ARec);
procedure DisposeDeleteFromList(AList: TList; const AIndex: Integer); // don't use for record with strings!
procedure DisposeParamData(var AParam: PsgParamData);
procedure FreeList(var AList: TList);
procedure FreeListOfList(var AList: TList);
procedure FreeMemAndNil(var P: Pointer);
procedure FreeRecordList(var AList: TList);
procedure FreeRecordListOfList(var AListOfList: TList);
procedure FreeRecordStringList(var AList: TStrings);
procedure FreeObjectStringList(var AList: TStrings);
function IsZero(const A: Double; Epsilon: Double = 0): Boolean;
function DistanceFPoint(const AP1, AP2: TFPoint; const AResolution: Double = fDoubleResolution): Double;
function IsEqual(const A, B: Extended; AResolution: Extended = fDoubleResolution): Boolean; {$IFNDEF SGFPC}overload;{$ENDIF}
{$IFNDEF SGFPC}
function IsEqual(const A, B: TsgFloat; AResolution: Double = fDoubleResolution): Boolean; overload;
{$ENDIF}
function IsEqualFPoints(const Point1, Point2: TFPoint; Epsilon: Double = fDoubleResolution): Boolean;
function sgIsEqualIID(const AGuid1, AGuid2: TGUID): Boolean;
function SwapInts(var A,B: Cardinal): Cardinal; overload;
function SwapInts(var A,B: Integer): Integer; overload;
procedure CopyLists(const Destination, Source: TList; const AMode: TsgCopyMode = cmCopy);
function GetDecimalSeparator: Char;
function SetDecimalSeparator(const ADecimalSeparator: Char): Char;
function ListGrow(ACurrentCapacity: Integer): Integer;{$IFDEF SG_INLINE}inline;{$ENDIF}

function GetAttributeMode(const AIndex: Integer): TsgAttributeMode;
function HasObject(const AObject, AObjectCurrent: Pointer): Boolean;{$IFDEF SG_INLINE}inline;{$ENDIF}


function SGHeadVarStruct(const AMM: Boolean =
  {$IFDEF SG_UNITS_MM}True{$ELSE}False{$ENDIF}): TsgHeadVarStruct;
function SGDefaultImageSize(const AMM: Boolean =
 {$IFDEF SG_UNITS_MM}True{$ELSE}False{$ENDIF}): TFRect;

function GetArrowByte(const AType: TsgDimArrowType): Byte;
function GetArrowType(const AType: Byte): TsgDimArrowType;
function GetDimLimitUnitsByte(const AType: TsgDimLimitUnits): Integer;
function GetDimLimitUnitsType(const AType: Integer): TsgDimLimitUnits;
function GetPlotRotation(const AType: Integer): TsgPlotRotation;

procedure ClearAppInfo(var AAppInfo: TsgDWGAppInfo);
function IsAppInfoNotEmpty(const AAppInfo: TsgDWGAppInfo): Boolean;

{ Export support function }

function GetExportFormat(const AExt: string): TsgExportFormat;
function GetExportExt(const AExportFormat: TsgExportFormat; const AFileName: string = ''): string;
function GetUniqFileName(const AFileNewName: string): string;
function IsSVGBlock(const AName: string): Boolean;
function HasInternalDimensionBlockName(const AName: string; APos: PInteger = nil): Boolean;
function GetExportFormatName(const AExportFormat: TsgExportFormat): string;
function GetExportFormatByFormatName(const AExportFormatName: string): TsgExportFormat;

// return supported dwg version by first bytes of content
function GetAppID: string;
function GetAppIdInternal: string;//not cleaned - for internal using
function GetDWGVersion(const S: AnsiString; var AVersion: TsgDWGVersion): Integer;
function CompareBuilVersion(const AV1, AV2: TsgBuildVersion): Integer;
function MakeBuildVersion(const AMajor, AMinor: Integer;
  const ARelease: Integer = 0; const ABuild: Integer = 0): TsgBuildVersion;
function StrToBuildVersion(const AVersion: string): TsgBuildVersion;
function BuildVersionToStr(const AVersion: TsgBuildVersion; const AMssk: Byte = 7): string;

{$IFNDEF SGDEL_5}
function SameText(const S1, S2: string): Boolean;
{$ENDIF}
{$IFNDEF SGDEL_6}
function WideUpperCase(const S: WideString): WideString;
{$ENDIF}

function GetXMLId(const AName: string): TsgXMLId;
procedure CreateXMLIdTable;
function GetSaveResultStr(const ASaveResult: Integer): string;
function CreateXMLParams(const AMode: TsgXMLModes = cnstDefaultXMLMode): TsgXMLParams;


// Interface references convertation
{$IFNDEF SG_WINDOWS}
{$EXTERNALSYM CopyMemory}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: TsgNativeUInt);
{$ENDIF}

{$IFDEF SGFPC}
{$IFDEF SG_NON_WIN_PLATFORM}
function PolyPolyline(DC: HDC; const PointStructs; const Points; p4: DWORD): Bool; stdcall;
function PolyPolygon(DC: HDC; var Points; var nPoints; p4: Integer): BOOL; stdcall;
{$ENDIF}
{$ENDIF}

function CreateStrListByNameObjects(const ANames: array of string;
  const AObjects: array of Integer): TStringList;

function GetColorByMaterialName(const AName: string; var ACOlor: Integer): Boolean;
function GetIDByMaterialName(const AName: string; var AID: string): Boolean;
//support function
function IsGermanyEdition: Boolean;
//Get Enumenator string-ident
function GetViewDirectionName(ADirection: TsgDXFViewDirection): string;


// font styles functions
function MVFontStylesToFontStyles(const AFontStyles: TmvFontStyles): TFontStyles;
function FontStylesToMVFontStyles(const AFontStyles: TFontStyles): TmvFontStyles;
function IntegerToFontStyles(const AMask : Integer) : TmvFontStyles;
function FontStylesToInteger(const AFontStyles : TmvFontStyles) : Integer;


// IsgMainApplication helper functions
{$IFDEF MSWINDOWS}
procedure InitializeWineInfo;
{$ENDIF}
function GetMainApplication(AInstance: TPersistent; out AMainApplication: IsgMainApplication): Boolean;
{$IFNDEF SG_FIREMONKEY}
{$IFNDEF SGFPC}
function GetFileNameByHandle(Handle: THandle; var FileName: TFileName): Boolean;
{$ENDIF}
{$ENDIF}

{$IFDEF SG_FIREMONKEY}
function DefRastrExportParams: TsgRastrExportParams;
{$ENDIF}

{$IFDEF SG_HAS_ACTIVEX}
function GetGlobalInterfaceTable(reserved: Integer; out git: IGlobalInterfaceTable): HRESULT;
{$ENDIF}

var
  IsWine: Boolean = False;
  WineVer: string = '';

  CodeTypes: PCodeTypes = nil;
  CF_CADIMG: Word = 0;
  GetGlobalPropProvider: TsgGetGlobalPropProvider = nil;
  HTTPOwner: TComponent = nil;

implementation

{$IFNDEF SG_FIREMONKEY}

uses
{$IFNDEF SG_NON_WIN_PLATFORM}{$IFNDEF SGFPC}PsApi, {$ENDIF}{$ENDIF}
  {$IFNDEF SGDEL_6}ComObj, {$ELSE}Variants, {$ENDIF}{$IFNDEF SG_FIREMONKEY}ClipBrd{$ENDIF}
{$IFDEF USE_SG_AGG2D}
  , Agg_LCL, FPCanvas, agg_fpimage, agg_basics, agg_path_storage, agg_conv_gpc
{$ENDIF}
{$IFDEF SG_USE_AGG2D_AS_GDI}, sgAGG2DIntf2{$ENDIF};
{$ENDIF}

type
  TsgObjRastrExportParams = class
    Fields: TsgRastrExportParams;
  private
    class procedure InitDefValues(var AFields: TsgRastrExportParams);
  public
    constructor Create;
  end;

{$IFDEF SG_FIREMONKEY}
const
  DefRastrExportParamsObj: TsgObjRastrExportParams = nil;

function DefRastrExportParams: TsgRastrExportParams;
begin
  Result := DefRastrExportParamsObj.Fields;
end;
{$ENDIF}

const
  cnstMaterialNames: array[0..3] of string = (cnstAluminium, cnstPlastic, cnstRubber, cnstAir);
  cnstMaterialColors: array[0..3] of Integer = ($C0C0C0, clRed, $2A2AA5, clBlue);
  cnstMaterialsIDs: array[0..3] of string = ('O-1036', 'O-1053', cnstRubber, cnstAir);
  cnstMaterialIDIndexes: array[0..3] of Integer = (0, 1, 2, 3);

type
  TPersistentAccess = class(TPersistent);

var
  ColorsByMaterial: TStringList = nil;
  IDsByMaterial: TStringList = nil;

function GetNumberOfCircleParts: Integer;
begin
  Result := iCurrentNumberOfCircleParts;
  if Result < iMinNumberOfPart then
    Result := iDefaultNumberOfCircleParts;
end;

function GetNumberOfSplineParts: Integer;
begin
  Result := iCurrentNumberOfSplineParts;
  if Result < iMinNumberOfPart then
    Result := iDefaultNumberOfSplineParts;
end;

function GetDispersionFactor: Double;
begin
  Result := iCurrentDispersionFactor;
end;

function HasDispersionFactor: Boolean;
begin
  Result := iCurrentDispersionFactor > 0;
end;

function InsertSpace(S: string): string;
var
  I: Integer;
{$IFNDEF SGDEL_2009}
  function IsUpper(C: Char): Boolean;
  begin
    Result := (C >= 'A') and (C <= 'Z');
  end;
{$ENDIF}
begin
  for I := Length(S) downto 2 do
    {$IFNDEF SGDEL_XE4}
    if IsUpper(S[I]) and not IsUpper(S[I - 1])
    {$ELSE}
    if S[I].IsUpper and not S[I - 1].IsUpper
    {$ENDIF}
    then
      Insert(' ', S, I);
  Result := S;
end;

function IsDigit(AChar: Char): Boolean;{$IFNDEF UNICODE} overload;{$ENDIF}
begin
{$IFDEF SGDEL_XE4}
  Result := AChar.IsDigit;
{$ELSE}
{$IFDEF SGDEL_2009}
  Result := TCharacter.IsDigit(AChar)
{$ELSE}
  Result := AChar in ['0'..'9'];
{$ENDIF}
{$ENDIF}
end;

{$IFNDEF UNICODE}
function IsDigit(AChar: WideChar): Boolean; overload;
begin
{$IFDEF SGDEL_2009}
  Result := TCharacter.IsDigit(AChar)
{$ELSE}
  Result := AnsiChar(AChar) in ['0'..'9'];
{$ENDIF}
end;
{$ENDIF}

function SetNumberFromStr(var ANumber: Integer; var AStr: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AStr) do
  begin
    if IsDigit(AStr[I]) then
      Result := Result + AStr[I]
    else
    begin
       if (I = 1) and (AStr[I] = '-') then
         Result := Result + AStr[I]
       else
         Break;
    end;
  end;
  Delete(AStr, 1, Length(Result));
  ANumber := StrToIntDef(Result, 0);
end;

{$IFDEF SG_BTI}
function GetXMLBTITypeEx(const AName: string): TsgBTIXMLType;
begin
  if Assigned(XMLBTITypeProc) then
    Result := XMLBTITypeProc(AName)
  else
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Id := bxmlUndefined;
  end;
end;

function GetBTIContolType(const AIndex: Integer): TsgBTIContolType;
begin
  if (AIndex >= Integer(Low(TsgBTIContolType))) and (AIndex <= Integer(High(TsgBTIContolType)))then
    Result := TsgBTIContolType(AIndex)
  else
    Result := bctNone;
end;

function IndexOfNameXml(const AName: string; const ANames: array of TsgBTIXMLId): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Length(AName) > 0 then
  begin
    for I := Low(ANames) to High(ANames) do
      if SameText(AName, cnstBTIXMLNames[ANames[I]].Name) then
      begin
        Result := I;
        Break;
      end;
  end;
end;
{$ENDIF}

function GetActualUnits(const AIUnits: TsgInsUnits): TsgInsUnits;
begin
  if AIUnits = iuMeters then
    Result := iuMeters
  else
    Result := iuMillimeters;
end;


function GetUnitsSize(const AIUnits: TsgInsUnits): Double;
begin
  case GetActualUnits(AIUnits) of
    iuMeters:  Result := 1000;
  else
    Result := 1;
  end;
end;

function GetDataIdType(const AStr: string): TsgDataId;
var
  I: TsgDataId;
begin
  Result := diUndefined;
  for I := Low(cnstDataId) to High(cnstDataId) do
  begin
    if SameText(AStr, cnstDataId[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function GetXMLHelp(AXMLType: TsgXMLType): string;
begin
  Result := '';
//  Result := InsertSpace(cnstXMLNames[AXMLType.Id].Name)  + '; Type:' + cnstXMLTypes[AXMLType.Id];
end;

procedure SetNumber(const AValue, ADefault: Integer; var ANumber: Integer);
begin
  ANumber := AValue;
  if ANumber < iMinNumberOfPart then
    ANumber := ADefault
  else
    if ANumber > iMaxNumberOfPart then
      ANumber := iMaxNumberOfPart;
end;

procedure SetNumberOfCircleParts(const ANumber: Integer);
begin
  SetNumber(ANumber, iDefaultNumberOfCircleParts, iCurrentNumberOfCircleParts);
end;


function ConvertToMeasurePolylineOptions(const AValues: Integer): TMeasurePolylineOptions;
var
  I: TMeasurePolylineOption;
  J: Integer;
begin
  Result := [];
  if AValues > 0 then
  begin
    J := 0;
    for I := Low(TMeasurePolylineOption) to High(TMeasurePolylineOption) do
    begin
      if AValues and (1 shl J) <> 0 then
        Include(Result, I);
      Inc(J);
    end;
  end;
end;

function ConvertFromMeasurePolylineOptions(const AValues: TMeasurePolylineOptions): Integer;
var
  I: TMeasurePolylineOption;
  J: Integer;
begin
  Result := 0;
  J := 0;
  for I := Low(TMeasurePolylineOption) to High(TMeasurePolylineOption) do
  begin
    if I in AValues then
      Result := Result or (1 shl J);
    Inc(J);
  end;
end;

procedure SetNumberOfSplineParts(const ANumber: Integer);
begin
  SetNumber(ANumber, iDefaultNumberOfSplineParts, iCurrentNumberOfSplineParts);
end;

procedure SetDispersionFactor(const AValue: Double);
var
  vValue: Double;
begin
  vValue := Abs(AValue);
  iCurrentDispersionFactor := vValue;
  if HasDispersionFactor then
  begin
    if not sgModAngle(vValue, iCurrentDispersionFactor) then
      iCurrentDispersionFactor := vValue;
  end;
end;

function GetCADEntityTypeString(ACADEntityType: TsgCADEntityType): string;
begin
  Result := '';
  case ACADEntityType of
    etNone: Result :=  '';
    etCustomVertex: Result :=  'CustomVertex';
    etLine: Result :=  'Line';
    etPolyLine: Result :=  'PolyLine';
    etLWPolyLine: Result :=  'LWPolyLine';
    etRect: Result :=  'Rect';
    etCircle: Result :=  'Circle';
    etEllipse: Result :=  'Ellipse';
    etMText: Result :=  'MText';
    etLeader: Result :=  'Leader';
    etPath: Result :=  'Path';
    etInsert: Result :=  'Insert';
    etBlock: Result :=  'Block';
    etArc: Result :=  'Arc';
    etMulti: Result :=  'Multi';
    etDimAligned: Result :=  'DimAligned';
    etHatch: Result :=  'Hatch';
    etText: Result :=  'Text';
    etSolid: Result :=  'Solid';
    etSpline: Result :=  'Spline';
    etViewPort: Result :=  'ViewPort';
    etDimension: Result :=  'Dimension';
    etSquare: Result :=  'Square';
    etPoint: Result :=  'Point';
    etPolyPolygon: Result :=  'PolyPolygon';
    etClipInsert: Result :=  'ClipInsert';
    etACADTable: Result :=  'ACADTable';
    etTrace: Result :=  'Trace';
    etTolerance: Result :=  'Tolerance';
    etShape: Result :=  'Shape';
    etAttdef: Result :=  'Attdef';
    etMLine: Result :=  'MLine';
    etWipeOut: Result :=  'WipeOut';
    etImageEnt: Result :=  'ImageEnt';
    etHelix: Result :=  'Helix';
    etOle2Frame: Result :=  'Ole2Frame';
    etCloud: Result :=  'Cloud';
    etMPolygon: Result :=  'MPolygon';
    etConstruction: Result :=  'Construction';
    etDimConstruction: Result :=  'DimConstruction';
    etComplex: Result :=  'Complex';
    etElement: Result :=  'Element';
    etComplexBroad: Result :=  'ComplexBroad';
    etComplexLinear: Result :=  'ComplexLinear';
    etElementBroad: Result :=  'ElementBroad';
    etElementLinear: Result :=  'ElementLinear';
    etElementCarved: Result :=  'ElementCarved';
    etElementModifier: Result :=  'ElementModifier';
    etArea: Result :=  'Area';
    etComplexArea: Result :=  'ComplexArea';
    etContainer: Result :=  'Container';
    etProxy: Result :=  'Proxy';
    etPathEvacuation: Result :=  'PathEvacuation';
    etXRef: Result :=  'XRef';
  end;
end;

function SplitAnonymousName(const AName: string; var ARoot: string;
  AIndex: PInteger = nil): Boolean;

var
  I, Err: Integer;
  S: string;

  function InternalCharInSet(C: Char; const ACharSet: TSysCharSet): Boolean;
  begin
    Result := AnsiChar(C) in ACharSet;
  end;

begin
  Result := False;
  if (Length(AName) > 0) and (AName[1] = cnstAsterisk) then
  begin
    ARoot := Copy(AName, 2, MaxInt);
    I := 1;
    while (I <= Length(ARoot)) and not InternalCharInSet(ARoot[I], ['0' .. '9']) do
      Inc(I);
    if I <= Length(ARoot) then
    begin
      S := Copy(ARoot, I, MaxInt);
      Delete(ARoot, I, MaxInt);
      if AIndex = nil then AIndex := @I;
      Err := 0;
      if S = '' then
        AIndex^ := -1
      else
        Val(S, AIndex^, Err);
      Result := (ARoot <> '') and (Err = 0);
    end;
  end;
end;

function GetPaperIndex(const AName: string): Integer;
var
  vName: string;
  Err: Integer;
begin
  Result := -1;
  vName := AnsiUpperCase(AName);
  if (Length(vName) > 0) and (vName[1] = '$') then
    vName[1] := '*';
  if vName = sModelSpace then
    Result := 0
  else
    if Pos(sPaperSpace, vName) = 1 then
    begin
      Delete(vName, 1, Length(sPaperSpace));
      if Length(vName) = 0 then
        Result := 1
      else
      begin
        Val(vName, Result, Err);
        if Err > 0 then
          Result := -1
        else
          Inc(Result, 2);
      end;
    end;
end;

function CreatePFPoint2DProc(const AX, AY, AZ: TsgFloat): Pointer;
var
  vPFPoint: PFPoint;
begin
  New(vPFPoint);
  vPFPoint^ := MakeFPoint(AX, AY, 0);
  Result := vPFPoint;
end;

function CreatePFPointProc(const AX, AY, AZ: TsgFloat): Pointer;
var
  vPFPoint: PFPoint;
begin
  New(vPFPoint);
  vPFPoint^ := MakeFPoint(AX, AY, AZ);
  Result := vPFPoint;
end;

function CreateParamData: PsgParamData;
begin
  New(Result);
  FillChar(Result^, SizeOf(Result^), 0);
end;

function CreatePsgCurveLink(const AFlag: Byte): PsgCurveLnk;
begin
  New(Result);
  FillChar(Result^, Sizeof(Result^), 0);
  Result^.Flags := AFlag;
end;

function MakeArcR(const ACenter: TFPoint; const ARadius, AAngleS, AAngleE: Double): TsgArcR;
begin
  Result.Center := ACenter;
  Result.Radius := ARadius;
  Result.AngleS := AAngleS;
  Result.AngleE := AAngleE;
end;

procedure MakeArcs(const AArcs: TsgArcsParams; var AArcR1, AArcR2: TsgArcR);
begin
  AArcR1.Center := AArcs.Center;
  AArcR1.Radius := AArcs.Radius1;
  AArcR1.AngleS := AArcs.AngleS1;
  AArcR1.AngleE := AArcs.AngleE1;

  AArcR2.Center := AArcs.Center;
  AArcR2.Radius := AArcs.Radius2;
  AArcR2.AngleS := AArcs.AngleS2;
  AArcR2.AngleE := AArcs.AngleE2;
end;

procedure MakeArcsParams(const AArcR1, AArcR2: TsgArcR; var AArcs: TsgArcsParams);
begin
  AArcs.Center := AArcR1.Center;
  AArcs.Radius1 := AArcR1.Radius;
  AArcs.AngleS1 := AArcR1.AngleS;
  AArcs.AngleE1 := AArcR1.AngleE;

  AArcs.Radius2 := AArcR2.Radius;
  AArcs.AngleS2 := AArcR2.AngleS;
  AArcs.AngleE2 := AArcR2.AngleE;
end;

function MakeDXFFromFPoint(const AFPoint: TFPoint): TdxfPoint;
begin
  Result.X := AFPoint.X;
  Result.Y := AFPoint.Y;
  Result.Z := AFPoint.Z;
end;

function MakeDXFPoint(const AX, AY, AZ: Single): TdxfPoint;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function MakeF2DPoint(const AX, AY: TsgFloat): TF2DPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function MakeF2DPointFrom3D(const APoint: TFPoint): TF2DPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function MakeF2DPointFromPoint(const APoint: TPoint): TF2DPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function MakeF2DRect(const ALeft, ATop, ARight, ABottom: TsgFloat): TF2DRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function MakeF2DRectByPoints(const ALeftTop, ARightBottom: TF2DPoint): TF2DRect;
begin
  Result.TopLeft := ALeftTop;
  Result.BottomRight := ARightBottom;
end;

function MakeF2dRectFromFRect(ARect: TFRect): TF2dRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Bottom := ARect.Bottom;
  Result.Right := ARect.Right;
end;

function MakeF2DLine(const A, B: TF2DPoint): TF2DLine;
begin
  Result.Point1 := A;
  Result.Point2 := B;
end;

function MakeF2DLineFrom3D(const ALine: TsgLine): TF2DLine;
begin
  Result.Point1.X := ALine.Point1.X;
  Result.Point1.Y := ALine.Point1.Y;
  Result.Point2.X := ALine.Point2.X;
  Result.Point2.Y := ALine.Point2.Y;
end;

function MakeFRectFromF2dRect(ARect: TF2dRect): TFRect;
begin
  Result.TopLeft := MakeFPoint(ARect.TopLeft.X, ARect.TopLeft.Y, 0);
  Result.BottomRight := MakeFPoint(ARect.BottomRight.X, ARect.BottomRight.Y, 0);
end;

function MakeFRectFromRect(const ARect: TRect): TFRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Bottom := ARect.Bottom;
  Result.Right := ARect.Right;
  Result.Z1 := 0;
  Result.Z2 := 0;
end;

function MakeRectFromFRect(const ARect: TFRect): TRect;
begin
  Result := MakeRectFromF2DRect(MakeF2dRectFromFRect(ARect));
end;

function MakeRectFromF2DRect(const ARect: TF2DRect): TRect;

  function GetValue(const AValue: Double): Integer;
  begin
    if AValue >= cnstMaxInt32 then
      Result := cnstMaxInt32
    else
      if AValue <= cnstMinInt32 then
        Result := cnstMinInt32
      else
        Result := Round(AValue);
  end;

begin
  Result.Top := GetValue(ARect.Top);
  Result.Left := GetValue(ARect.Left);
  Result.Bottom := GetValue(ARect.Bottom);
  Result.Right := GetValue(ARect.Right);
end;

function MakeFPoint(const AX, AY: TsgFloat; const AZ: TsgFloat = 0): TFPoint;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function MakeFPointFrom2D(const APoint: TF2DPoint): TFPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
  Result.Z := 0;
end;

function MakeFPointFromDXF(const AdxfPoint: TdxfPoint): TFPoint;
begin
  Result.X := AdxfPoint.X;
  Result.Y := AdxfPoint.Y;
  Result.Z := AdxfPoint.Z;
end;

function MakeFPointFromPoint(const APoint: TPoint): TFPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
  Result.Z := 0;
end;

function MakeFPointFromPoint(const APoint: TPointF): TFPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
  Result.Z := 0;
end;

function MakePointF(X, Y: Single): TPointF; overload;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MakePointF(X, Y: Integer): TPointF; overload;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MakePointF(const P: TPoint): TPointF; overload;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function MakePointFrom3D(const APoint: TFPoint; const AGDI: Boolean = False): TPoint;

  procedure DoFixExtent(var AValue: Integer; const AMax: Integer);
  begin
    if AValue < -AMax then
      AValue := -AMax
    else
      if AValue > AMax then
        AValue := AMax;
  end;

begin
  Result.X := Round(APoint.X);
  Result.Y := Round(APoint.Y);
  if AGDI then
  begin
    DoFixExtent(Result.X, MaxInt);
    DoFixExtent(Result.Y, MaxInt);
  end;
end;

function MakeFRect(const ALeft, ATop, AZ1, ARight,
  ABottom, AZ2: TsgFloat): TFRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Z1 := AZ1;
  Result.Right := ARight;
  Result.Bottom := ABottom;
  Result.Z2 := AZ2;
end;

function MakeFRect2D(const ALeft, ATop, ARight, ABottom: Extended): TFRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Z1 := 0;
  Result.Right := ARight;
  Result.Bottom := ABottom;
  Result.Z2 := 0;
end;

function MakeFRectByPoints(const ALeftTop, ARightBottom: TFPoint): TFRect;
begin
  Result.TopLeft := ALeftTop;
  Result.BottomRight := ARightBottom;
end;

function MakeFRectByF2DPoints(const ALeftTop, ARightBottom: TF2DPoint): TFRect;
begin
  Result.Left := ALeftTop.X;
  Result.Top := ALeftTop.Y;
  Result.Z1 := 0;
  Result.Right := ARightBottom.X;
  Result.Bottom := ARightBottom.Y;
  Result.Z2 := 0;
end;

function MakeLine(const APoint1, APoint2: TFPoint): TsgLine;
begin
  Result.Point1 := APoint1;
  Result.Point2 := APoint2;
end;

procedure MakePoints3(const AP1, AP2, AP3: TFPoint; APoints3: PDouble);
begin
  PsgPoints3(APoints3)^[0] := AP1;
  PsgPoints3(APoints3)^[1] := AP2;
  PsgPoints3(APoints3)^[2] := AP3;
end;

procedure MakePoints4(const AP1, AP2, AP3, AP4: TFPoint; APoints4: PDouble);
begin
  PsgPoints4(APoints4)^[0] := AP1;
  PsgPoints4(APoints4)^[1] := AP2;
  PsgPoints4(APoints4)^[2] := AP3;
  PsgPoints4(APoints4)^[3] := AP4;
end;

procedure MakePoints4L(const AList: TList; APoints4: PDouble);
begin
  PsgPoints4(APoints4)^[0] := PFPoint(@AList.List{$IFDEF LIST_PTR}^{$ENDIF}[0])^;
  PsgPoints4(APoints4)^[1] := PFPoint(@AList.List{$IFDEF LIST_PTR}^{$ENDIF}[1])^;
  PsgPoints4(APoints4)^[2] := PFPoint(@AList.List{$IFDEF LIST_PTR}^{$ENDIF}[2])^;
  PsgPoints4(APoints4)^[3] := PFPoint(@AList.List{$IFDEF LIST_PTR}^{$ENDIF}[3])^;
end;

function MakeRightPath(APath: string): string;
var
  vLength: Integer;
begin
  vLength := Length(APath);
  if vLength > 0 then
    if APath[vLength] <> cnstPathSlash then
      APath := APath + cnstPathSlash;
  Result := APath;
end;

function MakeViewBox(const AX, AY, AWidth, AHeight: Double): TsgViewBox;
begin
  Result.Mode := [];
  Result.X := AX;
  Result.Y := AY;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

function MakeVersion(const AMajor, AMinor: Integer): TsgVersion;
begin
  Result.Major := AMajor;
  Result.Minor := AMinor;
end;

function MakeHyperLink(const AName, AValue: string): string;
begin
  Result := AName + cnstHyperLinkDelimiter + AValue;
end;

function NewFPoint(const AX, AY : TsgFloat; const AZ: TsgFloat = 0): PFPoint;
begin
  New(Result);
  Result^.X := AX;
  Result^.Y := AY;
  Result^.Z:= AZ;
end;

function NewFPoint(const APoint: TFPoint): PFPoint; overload;
begin
  Result := NewFPoint(APoint.X, APoint.Y, APoint.Z);
end;

procedure SetMinMaxUInt64(const AHandle: UInt64; var AMin, AMax: UInt64);{$IFDEF SG_INLINE}inline;{$ENDIF}
begin
  if AMin > AHandle then
    AMin := AHandle;
  if AMax < AHandle then
    AMax := AHandle;
end;

function sgFloorZ(const AValue: Double): Int64;
begin
  Result := Int64(Trunc(AValue));
end;

function sgMod(const AValue, ABase: Double): Double;
begin
  Result := Frac(AValue / ABase) * ABase;
end;

function sgModAngle(const AValue: Double; var AModValue: Double;
  const ARadian: Boolean = False): Boolean;
begin
  Result := Abs(AValue) < MaxInt;
  if Result then
  begin
    if ARadian then
      AModValue := sgMod(AValue, cnstPiMul2)
    else
      AModValue := sgMod(AValue, 360)
  end
  else
    AModValue := AValue;
end;

{$IFNDEF  SGFPC}
{$IFNDEF SGDEL_XE2}
function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;
{$ENDIF}
{$ENDIF}

{Log procedure}

procedure FileLoging(const AParam: TsgLogParam); overload;
var
  I: Integer;
begin
  if Assigned(vLogs) then
    for I := 0 to vLogs.Count - 1 do
      TsgLogger(vLogs[I]).FileLogging(AParam);
end;

procedure FileLoging(const AOperation: TsgLogOperation; const AParam: Pointer;
  const AFileName: string = ''; const AId: UInt64 = 0); overload;
var
  vParam: TsgLogParam;
begin
  FillChar(vParam, SizeOf(vParam), 0);
  vParam.Operation := AOperation;
  vParam.Obj := TObject(AParam);
  vParam.Info := AFileName;
  vParam.Id := AId;
  FileLoging(vParam);
end;

procedure RegisterLogger(const ALogger: TsgLogger);
begin
  if not Assigned(vLogs) then
    vLogs := TList.Create;
  vLogs.Add(ALogger);
end;

function UnRegisterLoger(const ALogger: TsgLogger): Boolean;
begin
  Result := False;
  if Assigned(vLogs) then
  begin
    Result := vLogs.Remove(ALogger) > -1;
    if vLogs.Count = 0 then
      FreeAndNil(vLogs);
  end;
end;

{$IFNDEF SGDEL_2009}
function AnsiStrAlloc(ASize: Cardinal): PAnsiChar;
begin
  Result := StrAlloc(ASize);
end;
{$ENDIF}

function BadRect: TFRect;
begin
  Result := cnstBadRect;
end;

function BadPoint: TFPoint;
begin
  Result := MakeFPoint(cnstNan, cnstNan, cnstNan);
end;

function MaxTsgFloat: Extended;
begin
  case SizeOf(TsgFloat) of
    4: Result := MaxSingle;
    8: Result := MaxDouble;
    10: Result := {$IFDEF SG_NO_EXTENDED}MaxDouble{$ELSE}MaxExtended{$ENDIF};
  else
    Result := MaxSingle;
  end;
end;

function GetRectWidth(const ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

function GetRectHeight(const ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

procedure ClearList(AList: TList; ClearCapacity: Boolean = True);
var
  I: Integer;
begin
  if AList = nil then
    Exit;
  for I := 0 to AList.Count - 1 do
    TObject(AList.List{$IFDEF LIST_PTR}^{$ENDIF}[I]).Free;
  if ClearCapacity then
    AList.Clear
  else
    AList.Count := 0;
end;

procedure ClearListOfList(AListOfList: TList; ClearCapacity: Boolean = True);
var
  I, K: Integer;
  vList: TList;
begin
  if AListOfList = nil then
    Exit;
  for I := AListOfList.Count - 1 downto 0 do
    if AListOfList.List{$IFDEF LIST_PTR}^{$ENDIF}[I] <> nil then
    begin
      vList := TList(AListOfList.List{$IFDEF LIST_PTR}^{$ENDIF}[I]);
      for K := vList.Count - 1 downto 0 do
        if vList.List{$IFDEF LIST_PTR}^{$ENDIF}[K] <> nil then
          TObject(vList.List{$IFDEF LIST_PTR}^{$ENDIF}[K]).Free;
      vList.Free;
    end;
  if ClearCapacity then
    AListOfList.Clear
  else
    AListOfList.Count := 0;
end;

procedure ClearListWithNilCheck(AList: TList; ClearCapacity: Boolean = True);
var
  I: Integer;
begin
  if AList = nil then
    Exit;
  for I := 0 to AList.Count - 1 do
    if AList.List{$IFDEF LIST_PTR}^{$ENDIF}[I] <> nil then
      TObject(AList.List{$IFDEF LIST_PTR}^{$ENDIF}[I]).Free;
  if ClearCapacity then
    AList.Clear
  else
    AList.Count := 0;
end;

procedure ClearObjects(const AStrings: TStrings; Clear: Boolean = True);
var
  I: Integer;
  vObj: TObject;
begin
  if AStrings = nil then
    Exit;
  for I := 0 to AStrings.Count - 1 do
  begin
    vObj := AStrings.Objects[I];
    AStrings.Objects[I] := nil;
    vObj.Free;
  end;
  if Clear then
    AStrings.Clear;
end;

procedure ClearParamData(const APParam: PsgParamData);
begin
  if APParam <> nil then
  begin
{$IFNDEF SG_FM_MOBILE}
    case APParam^.PType of
      ptObjectList:
        begin
          FreeAndNil(APParam^.DataObj);
        end;
    end;
{$ENDIF}
    FillChar(APParam^, SizeOf(APParam^), 0);
  end;
end;

procedure ClearRecordList(AList: TList; ClearCapacity: Boolean = True);
var
  I: Integer;
begin
  if AList = nil then
    Exit;
  for I := 0 to AList.Count - 1 do
    FreeMem(AList.List{$IFDEF LIST_PTR}^{$ENDIF}[I]);
  if ClearCapacity then
    AList.Clear
  else
    AList.Count := 0;
end;

procedure ClearRecordListOfList(AListOfList: TList; ClearCapacity: Boolean = True);
var
  I, K: Integer;
  vList: TList;
begin
  if AListOfList = nil then
    Exit;
  for I := AListOfList.Count - 1 downto 0 do
    if AListOfList.List{$IFDEF LIST_PTR}^{$ENDIF}[I] <> nil then
    begin
      vList := TList(AListOfList.List{$IFDEF LIST_PTR}^{$ENDIF}[I]);
      for K := vList.Count - 1 downto 0 do
        if vList.List{$IFDEF LIST_PTR}^{$ENDIF}[K] <> nil then
          FreeMem(vList.List{$IFDEF LIST_PTR}^{$ENDIF}[K]);
      vList.Free;
    end;
  if ClearCapacity then
    AListOfList.Clear
  else
    AListOfList.Count := 0;
end;

procedure ClearRecordObjects(const AStrings: TStrings; Clear: Boolean = True);
var
  I: Integer;
  P: Pointer;
begin
  if AStrings = nil then
    Exit;
  for I := 0 to AStrings.Count - 1 do
  begin
    P := Pointer(AStrings.Objects[I]);
    AStrings.Objects[I] := nil;
    FreeMem(P);
  end;
  if Clear then
    AStrings.Clear;
end;

procedure ClearObjectObjects(const AStrings: TStrings; Clear: Boolean = True);
var
  I: Integer;
  vObj: TObject;
begin
  if AStrings = nil then
    Exit;
  for I := 0 to AStrings.Count - 1 do
  begin
    vObj := AStrings.Objects[I];
    AStrings.Objects[I] := nil;
    vObj.Free;
  end;
  if Clear then
    AStrings.Clear;
end;

procedure DisposeAndNil(var ARec);
begin
  if Pointer(ARec) <> nil then
  begin
    FreeMem(Pointer(ARec));
    Pointer(ARec) := nil;
  end;
end;

procedure DisposeDeleteFromList(AList: TList; const AIndex: Integer);
begin
  FreeMem(AList.List{$IFDEF LIST_PTR}^{$ENDIF}[AIndex]);
  AList.Delete(AIndex);
end;

procedure DisposeParamData(var AParam: PsgParamData);
begin
  ClearParamData(AParam);
  Dispose(AParam)
end;

procedure FreeList(var AList: TList);
begin
  ClearList(AList);
  FreeAndNil(AList);
end;

procedure FreeMemAndNil(var P: Pointer);
begin
  if P <> nil then
  begin
    FreeMem(P);
    P := nil
  end;
end;

procedure FreeListOfList(var AList: TList);
begin
  ClearListOfList(AList);
  FreeAndNil(AList); 
end;

procedure FreeRecordList(var AList: TList);
begin
  ClearRecordList(AList);
  FreeAndNil(AList);
end;

procedure FreeRecordListOfList(var AListOfList: TList);
begin
  ClearRecordListOfList(AListOfList);
  FreeAndNil(AListOfList);
end;

procedure FreeRecordStringList(var AList: TStrings);
begin
  ClearRecordObjects(AList, True);
  FreeAndNil(AList);
end;

procedure FreeObjectStringList(var AList: TStrings);
begin
  ClearObjectObjects(AList, True);
  FreeAndNil(AList);
end;

function SGHeadVarStruct(const AMM: Boolean =
  {$IFDEF SG_UNITS_MM}True{$ELSE}False{$ENDIF}): TsgHeadVarStruct;
const
{$IFDEF SG_EVACUATION}
  cnstTextSizeInt = 0.5;
  cnstArrowSizeInt = 0.25;
  cnstDimExeInt = 0.25;
  cnstDimExoInt = 0.0868;
  cnstDimGapInt = 0.1;
{$ELSE}
  cnstTextSizeInt = 2.5;
  cnstArrowSizeInt = 2.5;
  cnstDimExeInt = 2.5;
  cnstDimExoInt = 0.868;
  cnstDimGapInt = 0.1;
{$ENDIF}

  cnstSGDefaultHeadVarStruct: TsgHeadVarStruct =
    (
      CEColor: (Active: acIndexColor; Color: clDXFByLayer; AlbumString: '');// cnstDefaultEntityColor;
      CLayer: '0';
      CELType: sByLayer;
      CELTScale: 1;
      CELWeight: -1.0;
      CodePage: 0;
      DimStyle: sStandardName;
      DimTextStyle: sStandardName;
      DimProps: (
        Alt: False;
        AltF: 1;
        APost: '';
        Asz: cnstArrowSizeInt;
        Sah: False;
        Arrows: (Blk: datClosedfilled; Blk1: datClosedfilled;
          Blk2: datClosedfilled; LrBlk: datClosedfilled);
        Cen: 0.09;
        ClrD: (Active: acIndexColor; Color: clDXFByBlock; AlbumString: '');
        ClrE: (Active: acIndexColor; Color: clDXFByBlock; AlbumString: '');
        ClrT: (Active: acIndexColor; Color: clDXFByBlock; AlbumString: '');
        SD1: False;
        SD2: False;
        SE1: False;
        SE2: False;
        Dec: 2;
        Exe: cnstDimExeInt;
        Exo: cnstDimExoInt;
        Gap: cnstDimGapInt;
        LFac: 1;
        LwD: -1.0;
        LwE: -1.0;
        Post: '';
        Scale: 1.0;
        Tad: 0;
        Tih: False;
        Tix: 0;
        Toh: False;
        Txt: cnstTextSizeInt;
        Tp: 0;
        Tm: 0;
        LUnit: luDecimal;
        DSep: cnstPoint;
        );
      ExtMin: (X: 0; Y: 0; Z: 0);
      ExtMax: (X: 0; Y: 0; Z: 0);
      PExtMin: (X: 0; Y: 0; Z: 0);
      PExtMax: (X: 0; Y: 0; Z: 0);
      LimMin: (X: 0; Y: 0; Z: 0);
      LimMax: (X: 0; Y: 0; Z: 0);
      PLimMin: (X: 0; Y: 0; Z: 0);
      PLimMax: (X: 0; Y: 0; Z: 0);
      TextStyle: sStandardName;
      FilletRadius: 0.5;
      InsUnits: Byte(iuMeters);
      LTScale: 1;
      Measurement: True;
      PointDisplayMode: 0;
      PointDisplaySize: 0;
      TextSize: cnstTextSizeInt;
      TileMode: 1;
      UCSORG:  (X: 0; Y: 0; Z: 0);
      UCSXDir: (X: 1; Y: 0; Z: 0);
      UCSYDir: (X: 0; Y: 1; Z: 0);
      Version: 0;
      InsBase: (X: 0; Y: 0; Z: 0);
      AttMode: attNormal;
      XClipFrame: 2;
      DimAssoc: 2;
      FillMode: True;
      LwDisplay: 1;
    );

begin
  Result := cnstSGDefaultHeadVarStruct;
  if AMM then
  begin
    Result.CELTScale := 1000;
    Result.FilletRadius := 250;
    Result.TextSize := 250;
    Result.InsUnits := Byte(iuMillimeters);
    Result.PointDisplaySize := 50;
    Result.DimProps.LFac := 0.001;
    Result.DimProps.Txt := Result.TextSize;
    Result.DimProps.Asz := 250;
    Result.DimProps.Exe := 250;
    Result.DimProps.Exo := 80;
    Result.DimProps.Gap := 80;
    Result.FillMode := True;
  end;
end;

function SGDefaultImageSize(const AMM: Boolean =
 {$IFDEF SG_UNITS_MM}True{$ELSE}False{$ENDIF}): TFRect;
const
  cnstImageSizeM: TFRect = (Left: -148.5; Top: 105; Z1: 0; Right: 148.5; Bottom: -105; Z2: 0);
  cnstImageSizeMM: TFRect = (Left: -148500; Top: 105000; Z1: 0; Right: 148500; Bottom: -105000; Z2: 0);
begin
  if AMM then
    Result := cnstImageSizeMM
  else
    Result := cnstImageSizeM;
end;

function DistanceFPoint(const AP1, AP2: TFPoint; const AResolution: Double): Double;
var
  vSqr, vX, vY, vZ: Extended;
begin
  vX := AP1.X - AP2.X;
  vY := AP1.Y - AP2.Y;
  vZ := AP1.Z - AP2.Z;
  vSqr := Sqr(vX) + Sqr(vY) + Sqr(vZ);
  if vSqr > AResolution then
    Result := Sqrt(vSqr)
  else
    Result := 0;
end;
{$IFNDEF SGFPC}
function IsEqual(const A, B: TsgFloat; AResolution: Double = fDoubleResolution): Boolean; overload;
var
  Epsilon: Double;
begin
  if AResolution > 0 then
    Epsilon := Max(Min(Abs(A), Abs(B)) * AResolution, AResolution)
  else
    Epsilon := -AResolution;
  if A > B then
    Result := (A - B) <= Epsilon
  else
    Result := (B - A) <= Epsilon;
//  Result := Abs(A - B) <= AResolution
end;
{$ENDIF}
function IsEqual(const A, B: Extended; AResolution: Extended = fDoubleResolution): Boolean;{$IFNDEF SGFPC} overload;{$ENDIF}
var
  Epsilon: Extended;
begin
  if AResolution > 0 then
    Epsilon := Max(Min(Abs(A), Abs(B)) * AResolution, AResolution)
  else
    Epsilon := -AResolution;
  if A > B then
    Result := (A - B) <= Epsilon
  else
    Result := (B - A) <= Epsilon;
//  Result := Abs(A - B) <= AResolution
end;

function IsEqualFPoints(const Point1, Point2: TFPoint;
  Epsilon: Double = fDoubleResolution): Boolean;
begin
  Result := IsZero(Point1.X - Point2.X, Epsilon) and
   IsZero(Point1.Y - Point2.Y, Epsilon) and IsZero(Point1.Z - Point2.Z, Epsilon);
end;

function IsZero(const A: Double; Epsilon: Double): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := fDoubleResolution;
  Result := Abs(A) <= Epsilon;
end;

function sgIsEqualIID(const AGuid1, AGuid2: TGUID): Boolean;
begin
{$IFDEF SG_FIREMONKEY}
  Result := AGuid1 = AGuid2;
{$ELSE}
  Result := {$IFDEF SGDEL_6}SysUtils.IsEqualGUID{$ELSE}IsEqualIID{$ENDIF}(AGuid1, AGuid2);
{$ENDIF}
end;

function SwapInts(var A,B: Cardinal): Cardinal; overload;
{$IFDEF SG_FIREMONKEY}
begin
  Result := A;
  A := B;
  B := Result;
end;
{$ELSE}
  {$IFDEF SGFPC}
  begin
    Result := A;
    A := B;
    B := Result;
  end;
  {$ELSE}
    asm
    {$IFDEF CPUX64}
      mov eax,dword ptr [rcx]
      xchg eax,dword ptr [rdx]
      mov dword ptr [rcx],eax

    {$ELSE}
      MOV	ECX,[EAX]
      XCHG	ECX,[EDX]
      MOV	[EAX],ECX
    {$ENDIF}
    end;
  {$ENDIF}
{$ENDIF}

function SwapInts(var A,B: Integer): Integer; overload;
{$IFDEF SG_FIREMONKEY}
begin
  Result := A;
  A := B;
  B := Result;
end;
{$ELSE}
  {$IFDEF SGFPC}
  begin
    Result := A;
    A := B;
    B := Result;
  end;
  {$ELSE}
  asm
  {$IFDEF CPUX64}
    mov eax,dword ptr [rcx]
    xchg eax,dword ptr [rdx]
    mov dword ptr [rcx],eax
  {$ELSE}
    MOV	ECX,[EAX]
    XCHG	ECX,[EDX]
    MOV	[EAX],ECX
  {$ENDIF}
  end;
 {$ENDIF}
{$ENDIF}

procedure CopyLists(const Destination, Source: TList;
  const AMode: TsgCopyMode = cmCopy);
var
  vIndex: Integer;
begin
  case AMode of
    cmAppend:
      begin
        vIndex := Destination.Count;
        Destination.Count := Destination.Count + Source.Count;
        if Source.Count > 0 then
           System.Move(Source.List{$IFDEF LIST_PTR}^{$ENDIF}[0], Destination.List{$IFDEF LIST_PTR}^{$ENDIF}[vIndex], Source.Count * SizeOf(Pointer));
      end;
  else
    Destination.Count := Source.Count;
    if Source.Count > 0 then
       System.Move(Source.List{$IFDEF LIST_PTR}^{$ENDIF}[0], Destination.List{$IFDEF LIST_PTR}^{$ENDIF}[0], Source.Count * SizeOf(Pointer));
  end;
end;

function GetDecimalSeparator: Char;
begin
  Result := {$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}FormatSettings.{$ENDIF}DecimalSeparator;
end;

function SetDecimalSeparator(const ADecimalSeparator: Char): Char;
begin
  Result := {$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF SG_HAS_FORMATSETTINGS_FEATURE}FormatSettings.{$ENDIF}DecimalSeparator := ADecimalSeparator;
end;

function ListGrow(ACurrentCapacity: Integer): Integer;{$IFDEF SG_INLINE}inline;{$ENDIF}
begin
  if ACurrentCapacity > 64 then
    Result := ACurrentCapacity + ACurrentCapacity div 4
  else
    if ACurrentCapacity > 8 then
      Result := ACurrentCapacity + 16
    else
      Result := ACurrentCapacity + 4;
end;

function GetAttributeMode(const AIndex: Integer): TsgAttributeMode;
begin
  case AIndex of
    0:  Result := attDisable;
    1:  Result := attNormal;
  else
    Result := attEnable;
  end;
end;

function HasObject(const AObject, AObjectCurrent: Pointer): Boolean;{$IFDEF SG_INLINE}inline;{$ENDIF}
begin
  if AObject = cnstObjectReserve then//has style
    Result := AObjectCurrent <> cnstObjectReserve
  else//has current style
    Result := AObjectCurrent = AObject;
end;

function GetArrowByte(const AType: TsgDimArrowType): Byte;
begin
  case AType of
    datUndefined: Result := cnstMaxByte;
  else
    Result := Byte(AType);
  end;
end;

function GetArrowType(const AType: Byte): TsgDimArrowType;
begin
  case AType of
    cnstMaxByte:  Result := datUndefined;
  else
    Result := TsgDimArrowType(AType);
    if AType > Byte(High(TsgDimArrowType)) then
      Result := datClosedfilled;
  end;
end;

function GetDimLimitUnitsByte(const AType: TsgDimLimitUnits): Integer;
begin
  Result := Byte(AType);
end;

function GetDimLimitUnitsType(const AType: Integer): TsgDimLimitUnits;
begin
  case AType of
    Integer(luScientific)..Integer(luWindowsDesktop): Result := TsgDimLimitUnits(AType);
  else
    Result := luUndefined;
  end;
end;

function GetPlotRotation(const AType: Integer): TsgPlotRotation;
begin
  Result := TsgPlotRotation(AType);
  if AType > Integer(High(TsgPlotRotation)) then
    Result := prNoRotation;
end;

procedure ClearAppInfo(var AAppInfo: TsgDWGAppInfo);
begin
  AAppInfo.InfoName := '';
  AAppInfo.ID := '';
  AAppInfo.Version := '';
  AAppInfo.Comment := '';
  AAppInfo.ProductXMLElement := '';
end;

function IsAppInfoNotEmpty(const AAppInfo: TsgDWGAppInfo): Boolean;
begin
  Result := (Length(AAppInfo.InfoName) > 0) or
    (Length(AAppInfo.ID) > 0) or (Length(AAppInfo.Version) > 0) or
    (Length(AAppInfo.Comment) > 0) or (Length(AAppInfo.ProductXMLElement) > 0);
end;

{$IFDEF SGDEL_2006}
{TsgUInt64}

{private}

function TsgRecUInt64.GetAll: Int64;
begin
  Result := Int64(FHi) shl 32 + FLo;
end;

function TsgRecUInt64.GetHi: Cardinal;
begin
  Result := FHi;
end;

function TsgRecUInt64.GetLo: Cardinal;
begin
  Result := FLo;
end;

procedure TsgRecUInt64.SetAll(const Value: Int64);
begin
  FLo := Value;
  FHi := Int64(Value) shr 32;
end;

procedure TsgRecUInt64.SetHi(const Value: Cardinal);
begin
  FHi := Value;
end;

procedure TsgRecUInt64.SetLo(const Value: Cardinal);
begin
  FLo := Value;
end;

{public}

class operator TsgRecUInt64.Add(const A, B: TsgRecUInt64): TsgRecUInt64;
begin
  Result.All := A.All + B.All;
end;

class operator TsgRecUInt64.BitwiseOr(const A, B: TsgRecUInt64): TsgRecUInt64;
begin
  Result.All := A.All or B.All;
end;

class operator TsgRecUInt64.Equal(const A, B: TsgRecUInt64): Boolean;
begin
  Result := (A.FHi = B.FHi) and (A.FLo = B.FLo);
end;

class operator TsgRecUInt64.GreaterThan(const A, B: TsgRecUInt64): Boolean;
begin
  Result := A.FHi > B.FHi;
  if not Result and (A.FHi = B.FHi) then Result := A.FLo > B.FLo;
end;

class operator TsgRecUInt64.GreaterThanOrEqual(const A, B: TsgRecUInt64): Boolean;
begin
  Result := not (A < B);
end;

class operator TsgRecUInt64.Implicit(const A: TsgRecUInt64): Int64;
begin
  Result := A.GetAll;
end;

class operator TsgRecUInt64.Implicit(const A: TsgRecUInt64): Integer;
begin
  Result := A.FLo;
end;

class operator TsgRecUInt64.Implicit(I: Int64): TsgRecUInt64;
begin
  Result.All := I;
end;

class operator TsgRecUInt64.Implicit(I: Integer): TsgRecUInt64;
begin
  Result.Lo := I;
  Result.Hi := 0;
end;

class operator TsgRecUInt64.LeftShift(const A: TsgRecUInt64; B: Integer): TsgRecUInt64;
begin
  Result.All := A.All shl B;
end;

class operator TsgRecUInt64.LessThan(const A, B: TsgRecUInt64): Boolean;
begin
  Result := A.FHi < B.FHi;
  if not Result and (A.FHi = B.FHi) then Result := A.FLo < B.FLo;
end;

class operator TsgRecUInt64.LessThanOrEqual(const A, B: TsgRecUInt64): Boolean;
begin
  Result := not (A > B);
end;

class operator TsgRecUInt64.Negative(const A: TsgRecUInt64): TsgRecUInt64;
begin
  Result.All := -A.All;
end;

class operator TsgRecUInt64.NotEqual(const A, B: TsgRecUInt64): Boolean;
begin
  Result := (A.FHi <> B.FHi) or (A.FLo <> B.FLo);
end;

class operator TsgRecUInt64.RightShift(const A: TsgRecUInt64; B: Integer): TsgRecUInt64;
begin
  Result.All := A.All shr B;
end;

class operator TsgRecUInt64.Subtract(const A, B: TsgRecUInt64): TsgRecUInt64;
begin
  Result.All := A.All - B.All;
end;

{$ENDIF}

var
  ExportExtensions: TStringList = nil;

procedure InitParams;
begin
  if not Assigned(ExportExtensions) then
  begin
    ExportExtensions := TStringList.Create;
{$IFDEF SGDEL_7}
    ExportExtensions.CaseSensitive := False;
{$ENDIF}
    ExportExtensions.Sorted := True;
    ExportExtensions.Duplicates := dupIgnore;
    ExportExtensions.AddObject('', TsgObjectWithField.CreateInt(Ord(efAuto)));
    ExportExtensions.AddObject('.bmp', TsgObjectWithField.CreateInt(Ord(efBitmap)));
    ExportExtensions.AddObject('.wmf', TsgObjectWithField.CreateInt(Ord(efWmf)));
    ExportExtensions.AddObject('.emf', TsgObjectWithField.CreateInt(Ord(efEmf)));
    ExportExtensions.AddObject('.jpg', TsgObjectWithField.CreateInt(Ord(efJpg)));
    ExportExtensions.AddObject('.jpeg', TsgObjectWithField.CreateInt(Ord(efJpeg)));
    ExportExtensions.AddObject('.tif', TsgObjectWithField.CreateInt(Ord(efTif)));
    ExportExtensions.AddObject('.tiff', TsgObjectWithField.CreateInt(Ord(efTiff)));
    ExportExtensions.AddObject('.gif', TsgObjectWithField.CreateInt(Ord(efGif)));
    ExportExtensions.AddObject('.png', TsgObjectWithField.CreateInt(Ord(efPng)));
    ExportExtensions.AddObject('.dxf', TsgObjectWithField.CreateInt(Ord(efDxf)));
    ExportExtensions.AddObject('.dxt', TsgObjectWithField.CreateInt(Ord(efDxt)));
    ExportExtensions.AddObject('.plt', TsgObjectWithField.CreateInt(Ord(efPlt)));
    ExportExtensions.AddObject('.hgl', TsgObjectWithField.CreateInt(Ord(efHgl)));
    ExportExtensions.AddObject('.hg', TsgObjectWithField.CreateInt(Ord(efHg)));
    ExportExtensions.AddObject('.hpg', TsgObjectWithField.CreateInt(Ord(efHpg)));
    ExportExtensions.AddObject('.plo', TsgObjectWithField.CreateInt(Ord(efPlo)));
    ExportExtensions.AddObject('.hp', TsgObjectWithField.CreateInt(Ord(efHp)));
    ExportExtensions.AddObject('.hp1', TsgObjectWithField.CreateInt(Ord(efHp1)));
    ExportExtensions.AddObject('.hp2', TsgObjectWithField.CreateInt(Ord(efHp2)));
    ExportExtensions.AddObject('.hp3', TsgObjectWithField.CreateInt(Ord(efHp3)));
    ExportExtensions.AddObject('.hpgl', TsgObjectWithField.CreateInt(Ord(efHpgl)));
    ExportExtensions.AddObject('.hpgl2', TsgObjectWithField.CreateInt(Ord(efHpgl2)));
    ExportExtensions.AddObject('.hpp', TsgObjectWithField.CreateInt(Ord(efHpp)));
    ExportExtensions.AddObject('.gl', TsgObjectWithField.CreateInt(Ord(efGl)));
    ExportExtensions.AddObject('.gl2', TsgObjectWithField.CreateInt(Ord(efGl2)));
    ExportExtensions.AddObject('.prn', TsgObjectWithField.CreateInt(Ord(efPrn)));
    ExportExtensions.AddObject('.spl', TsgObjectWithField.CreateInt(Ord(efSpl)));
    ExportExtensions.AddObject('.rtl', TsgObjectWithField.CreateInt(Ord(efRtl)));
    ExportExtensions.AddObject('.pcl', TsgObjectWithField.CreateInt(Ord(efPcl)));
    ExportExtensions.AddObject('.cgm', TsgObjectWithField.CreateInt(Ord(efCgm)));
    ExportExtensions.AddObject('.svg', TsgObjectWithField.CreateInt(Ord(efSvg)));
    ExportExtensions.AddObject('.svgz', TsgObjectWithField.CreateInt(Ord(efSvgZ)));
    ExportExtensions.AddObject('.swf', TsgObjectWithField.CreateInt(Ord(efSwf)));
    ExportExtensions.AddObject('.pdf', TsgObjectWithField.CreateInt(Ord(efPdf)));
    ExportExtensions.AddObject('.dwg', TsgObjectWithField.CreateInt(Ord(efDwg)));
    ExportExtensions.AddObject('.stl', TsgObjectWithField.CreateInt(Ord(efStl)));
    ExportExtensions.AddObject('.smd', TsgObjectWithField.CreateInt(Ord(efSmd)));
    ExportExtensions.AddObject('.glsm', TsgObjectWithField.CreateInt(Ord(efGlsm)));
    ExportExtensions.AddObject('.lmts', TsgObjectWithField.CreateInt(Ord(efLmts)));
    ExportExtensions.AddObject('.nmf', TsgObjectWithField.CreateInt(Ord(efNmf)));
    ExportExtensions.AddObject('.obj', TsgObjectWithField.CreateInt(Ord(efObj)));
    ExportExtensions.AddObject('.objf', TsgObjectWithField.CreateInt(Ord(efObjf)));
    ExportExtensions.AddObject('.igs', TsgObjectWithField.CreateInt(Ord(efIges)));
    ExportExtensions.AddObject('.iges', TsgObjectWithField.CreateInt(Ord(efIges)));
    ExportExtensions.AddObject('.step', TsgObjectWithField.CreateInt(Ord(efStep)));
    ExportExtensions.AddObject('.brep', TsgObjectWithField.CreateInt(Ord(efBrep)));
    ExportExtensions.AddObject('.nc', TsgObjectWithField.CreateInt(Ord(efGCode)));
    ExportExtensions.AddObject('.ngc', TsgObjectWithField.CreateInt(Ord(efGCode)));
    ExportExtensions.AddObject('.gcode', TsgObjectWithField.CreateInt(Ord(efGCode)));
    ExportExtensions.AddObject('.ncp', TsgObjectWithField.CreateInt(Ord(efGCode)));
    ExportExtensions.AddObject('.mcp', TsgObjectWithField.CreateInt(Ord(efMcp)));
    ExportExtensions.AddObject('.sat', TsgObjectWithField.CreateInt(Ord(efSat)));
    ExportExtensions.AddObject('.sab', TsgObjectWithField.CreateInt(Ord(efSab)));
{$IFDEF SG_XKT}
    ExportExtensions.AddObject('.xkt', TsgObjectWithField.CreateInt(Ord(efXkt)));
{$ENDIF}
  end;
end;

procedure FreeParams;
begin
  ClearObjects(ExportExtensions);
  FreeAndNil(ExportExtensions);
end;

function GetExportFormat(const AExt: string): TsgExportFormat;
var
  I: Integer;
begin
  Result := efAuto;
  InitParams;
  I := ExportExtensions.IndexOf(AExt);
  if I >= 0 then
    Result := TsgExportFormat(TsgObjectInt64(ExportExtensions.Objects[I]).FieldInt);
end;

function GetExportExt(const AExportFormat: TsgExportFormat; const AFileName: string = ''): string;
var
  I: Integer;
  vFind: Boolean;
  vDefExt: string;
begin
  Result := '';
  InitParams;
  if AExportFormat = efAuto then
  begin
    if Length(AFileName) > 0 then
      Result := ExtractFileExt(AFileName);
  end
  else
  begin
    I := 0;
    vFind := False;
    while (I < ExportExtensions.Count) and not vFind do
      if TsgExportFormat(TsgObjectInt64(ExportExtensions.Objects[I]).FieldInt) = AExportFormat then
        Inc(vFind)
      else
        Inc(I);
    if vFind then
    begin
      vDefExt := '';
      if AFileName <> '' then
        vDefExt := ExtractFileExt(AFileName);
      if vDefExt <> '' then
        if GetExportFormat(vDefExt) = AExportFormat then
        begin
          Result := vDefExt;
          vFind := False;
        end;
      if vFind then
        Result := ExportExtensions[I];
    end;
  end;
end;

function GetExportFormatName(const AExportFormat: TsgExportFormat): string;
begin
  Result := GetExportExt(AExportFormat);
  Result := Copy(Result, 2, Length(Result) - 1);
  Result := UpperCase(Result);
  if AExportFormat in cnstRastrFormats then
    Result := cnstRASTR
  else
    if AExportFormat in cnstHPGLFormats then
      Result := cnstHPGL
    else
      if AExportFormat in cnstSVGFormats then
        Result := cnstSVG
      else
         if AExportFormat in cnstACISFormats then
            Result := cnstACIS
         else
           if AExportFormat in cnstSTEPFormats then
             Result := cnstSTEP;
end;

function GetExportFormatByFormatName(const AExportFormatName: string): TsgExportFormat;
var
  vExt: string;
begin
  vExt := LowerCase(AExportFormatName);
  vExt := '.' + vExt;
  Result := GetExportFormat(vExt);
  if AExportFormatName = cnstRASTR then
    Result := efBitmap
  else
    if AExportFormatName = cnstHPGL then
      Result := efHpgl
    else
      if AExportFormatName = cnstSVG then
        Result := efSvg
      else
         if AExportFormatName = cnstACIS then
            Result := efSat
         else
           if AExportFormatName = cnstSTEP then
             Result := efStep;
end;


function GetUniqFileName(const AFileNewName: string): string;
var
  vIndex: Integer;
begin
  Result := AFileNewName;
  if FileExists(Result) then
  begin
    vIndex := 1;
    repeat
      Result := AFileNewName;
      Insert(' (' + IntToStr(vIndex) + ')', Result, Length(Result) - 3);
      Inc(vIndex);
    until not FileExists(Result);
  end;
end;

function IsSVGBlock(const AName: string): Boolean;
begin
  Result := (Length(AName) > cnstSVGBlockNameBaseLength) and
    (Copy(AName, 1, cnstSVGBlockNameBaseLength) = cnstSVGBlockNameBase);
end;

function HasInternalDimensionBlockName(const AName: string; APos: PInteger): Boolean;
var
  I: Integer;
begin
  if APos = nil then APos := @I;
  APos^ := AnsiPos(sInternalDimension, AName);
  Result := APos^ > 0;
end;

function GetAppID: string;
begin
  Result := cnstCSTAppID;
end;

function GetAppIdInternal: string;//not cleaned - for internal using
begin
  Result := cnstCSTAppIdInternal;
end;


(*function GetAppID: string;
begin
{$IFDEF SG_ABVIEWER}
  Result := 'ABViewer';
{$ELSE}
  {$IFDEF SG_BTI}
  Result := 'Inventory';
  {$ELSE}
  Result := '';
  {$ENDIF}
{$ENDIF}
end;*)

{ Function GetDWGVersion

  If result:
  - 0 - version supported;
  - 1 - version not supported;
  - else - unknown
}
function GetDWGVersion(const S: AnsiString; var AVersion: TsgDWGVersion): Integer;
{
   Version         Header             AutoCAD Version

  DWG R1.0     MC0.0            AutoCAD Release 1.0
  DWG R1.2     AC1.2            AutoCAD Release 1.2
  DWG R1.40    AC1.40           AutoCAD Release 1.40
  DWG R2.05    AC1.50           AutoCAD Release 2.05
  DWG R2.10    AC2.10           AutoCAD Release 2.10
  DWG R2.21    AC2.21           AutoCAD Release 2.21
  DWG R2.22    AC1001, AC2.22   AutoCAD Release 2.22
  DWG R2.50    AC1002           AutoCAD Release 2.50
  DWG R2.60    AC1003           AutoCAD Release 2.60
  DWG R9       AC1004           AutoCAD Release 9
  DWG R10      AC1006           AutoCAD Release 10
  DWG R11/12   AC1009           AutoCAD Release 11, AutoCAD Release 12
  DWG R13      AC1012           AutoCAD Release 13
  DWG R14      AC1014           AutoCAD Release 14
  DWG 2000     AC1015           AutoCAD 2000, AutoCAD 2000i, AutoCAD 2002
  DWG 2004     AC1018           AutoCAD 2004, AutoCAD 2005, AutoCAD 2006
  DWG 2007     AC1021           AutoCAD 2007, AutoCAD 2008, AutoCAD 2009
  DWG 2010     AC1024           AutoCAD 2010, AutoCAD 2011, AutoCAD 2012
  DWG 2013     AC1027           AutoCAD 2013
  DWG 2018     AC1032           AutoCAD 2018
}
const
  cnstAC10    = $30314341;  // 'AC10'
  cnstAC2Dot  = $2E324341;  // 'AC2.'
//  cnstAC1Dot  = $2E314341;  // 'AC1.'
//  cnstMC0Dot  = $2E304D41;  // 'MC0.'

var
  vP: PCardinal;

  function PackDCB(const ADCB: Word): Integer;
  begin
    Result := 10 * (ADCB and $F) + (ADCB shr 8) and $F;
  end;

begin
  Result := 0;
  try
    if Length(S) >= 4 then
    begin
      vP := PPointer(@S)^;
      case vP^ of
        cnstAC10:
          begin
            if Length(S) >= 6 then
            begin
              Inc(vP);
              case PackDCB(PWord(vP)^) of
                1..4:   AVersion := acR09;    // '01'..'04'
                6:      AVersion := acR10;    // '06'
                9:      AVersion := acR12;    // '09'
                12:     AVersion := acR13;    // '12'
                13..14: AVersion := acR14;    // '13'..'14'
                15:     AVersion := acR2000;  // '15'
                18:     AVersion := acR2004;  // '18'
                21:     AVersion := acR2007;  // '21'
                24:     AVersion := acR2010;  // '24'
                27:     AVersion := acR2013;  // '27'
                32:     AVersion := acR2018;  // '32'
              else
                Result := 2 + Ord(PackDCB(vP^) > 1);
              end;
            end
            else
              Result := 1;
          end;
        cnstAC2Dot:
          if Length(S) >= 6 then
          begin
            Inc(vP);
            case PackDCB(PWord(vP)^) of
              10, 21..22: AVersion := acR09; // '10', '21'..'22' (DWG R2.10/R2.21/R2.22)
            else
              Result := 2;
            end;
          end
          else
            Result := 1;
        else
          Result := 1;
      end; { case }
    end
    else
      Result := 1;
  except
    Result := 255;
  end;
end;

function CompareBuilVersion(const AV1, AV2: TsgBuildVersion): Integer;
var
  I: Integer;
begin
//Major, Minor, Release, Build
  for I := 0 to 3 do
  begin
    case Math.CompareValue(AV1.Values[I], AV2.Values[I]) of
      GreaterThanValue:
        begin
          Result := 1;
          Break;
        end;
      LessThanValue:
        begin
          Result := -1;
          Break;
        end;
    else
      Result := 0;
    end;
  end;
end;

function MakeBuildVersion(const AMajor, AMinor: Integer;
  const ARelease: Integer = 0; const ABuild: Integer = 0): TsgBuildVersion;
begin
  Result.Major := AMajor;
  Result.Minor := AMinor;
  Result.Release := ARelease;
  Result.Build := ABuild;
end;

function StrToBuildVersion(const AVersion: string): TsgBuildVersion;
var
  vVersion: TStringList;
{$IFNDEF SGDEL_2005}
  vPos: Integer;
  vVersionStr: string;
{$ENDIF}
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(AVersion) > 0 then
  begin
    vVersion := TStringList.Create;
    try
{$IFNDEF SGDEL_2005}
      vVersionStr := AVersion;
      vPos := Pos(cnstPoint, vVersionStr);
      while vPos > 0 do
      begin
        Delete(vVersionStr, vPos, Length(cnstPoint));
        Insert(sLineBreak, vVersionStr, vPos);
        vPos := Pos(cnstPoint, vVersionStr);
      end;
      vVersion.Text := vVersionStr;
{$ELSE}
      vVersion.LineBreak := cnstPoint;
      vVersion.Text := AVersion;
{$ENDIF}
      if vVersion.Count > 0 then
      begin
        Result.Major := StrToIntDef(vVersion[0], 0);
        if vVersion.Count > 1 then
        begin
          Result.Minor := StrToIntDef(vVersion[1], 0);
          if vVersion.Count > 2 then
          begin
            Result.Release := StrToIntDef(vVersion[2], 0);
            if vVersion.Count > 3 then
              Result.Build := StrToIntDef(vVersion[3], 0);
          end;
        end;
      end;
    finally
      FreeAndNil(vVersion);
    end;
  end;
end;

function BuildVersionToStr(const AVersion: TsgBuildVersion; const AMssk: Byte = 7): string;
begin
  Result := IntToStr(AVersion.Major);
  if AMssk and 1 <> 0 then
  begin
    Result := Result + cnstPoint + IntToStr(AVersion.Minor);
    if AMssk and 2 <> 0 then
    begin
      Result := Result + cnstPoint + IntToStr(AVersion.Release);
      if AMssk and 4 <> 0 then
         Result := Result + cnstPoint + IntToStr(AVersion.Build);
    end;
  end;
end;

{ TsgObjectWithOwner }

constructor TsgObjectWithOwner.Create(const AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TsgObjectWithOwner.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;


{ TsgNotification }

function TsgNotification.GetActive: Boolean;
begin
  Result := FFlags and 1 = 0;
end;

procedure TsgNotification.SetActive(const AValue: Boolean);
begin
  FFlags := (FFlags and $FFFE) or Word(not AValue);
end;

{ TsgNotifications }

function TsgNotifications.Add(const AObj: TsgNotification): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  vIndex := FList.IndexOf(AObj);
  if vIndex < 0 then
  begin
    Result := True;
    FList.Add(AObj);
  end;
end;

constructor TsgNotifications.Create(const AOwner: TObject);
begin
  inherited Create(AOwner);
  FList := TList.Create;
end;

destructor TsgNotifications.Destroy;
begin
  FreeList(FList);
  inherited Destroy;
end;

procedure TsgNotifications.Proc(const AParams: TsgNotificationParams);
var
  I: Integer;
  vNotify: TsgNotification;
begin
  for I := 0 to FList.Count - 1 do
  begin
    vNotify := TsgNotification(FList.List{$IFDEF LIST_PTR}^{$ENDIF}[I]);
    if vNotify.Active then
      vNotify.Proc(FOwner, AParams);
  end;
end;

function TsgNotifications.Remove(const AObj: TsgNotification): Boolean;
begin
  Result := FList.Remove(AObj) > -1;
end;

function TsgNotifications.RemoveAndFree(var AObj: TsgNotification): Boolean;
begin
  Result := Remove(AObj);
  if Result then
    AObj.Free
  else
    AObj.Active := False;
  AObj := nil;
end;

{$IFNDEF SGDEL_6}

{$IFNDEF SGDEL_5}

function SameText(const S1, S2: string): Boolean;
begin
  Result := CompareText(S1, S2) = 0
end;

{ TsgNotificationList }

procedure TsgNotificationList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

function TsgNotificationList.Add(Item: Pointer): Integer;
begin
  Result := inherited Add(Item);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

function TsgNotificationList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    List^[I] := nil;
    Delete(I);
    Notify(Result, lnExtracted);
  end;
end;

procedure TsgNotificationList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  Temp := Items[Index];
  inherited Delete(Index);
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

procedure TsgNotificationList.Insert(Index: Integer; Item: Pointer);
begin
  inherited Insert(Index, Item);
  if Item <> nil then
    Notify(Item, lnAdded);
end;
{$ENDIF}


function WideUpperCase(const S: WideString): WideString;
var
  vLen: Integer;
begin
  vLen := Length(S);
  SetString(Result, PWideChar(S), vLen);
  if vLen > 0 then
    CharUpperBuffW(Pointer(Result), vLen);
end;

{ TInterfacedPersistent }

procedure TInterfacedPersistent.AfterConstruction;
begin
  inherited;
  if GetOwner <> nil then
    GetOwner.GetInterface(IInterface, FOwnerInterface);
end;

function TInterfacedPersistent._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef else
    Result := -1;
end;

function TInterfacedPersistent._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release else
    Result := -1;
end;

function TInterfacedPersistent.QueryInterface(const IID: TGUID;
  out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

{$ENDIF}

procedure CreateXMLIdTable;
var
  I: TsgXMLId;
begin
  XMLIds := TStringList.Create;
  XMLIds.Duplicates := dupError;
  XMLIds.Sorted := True;
{$IFDEF SGDEL_6}
  XMLIds.CaseSensitive := True;
{$ENDIF}
  for I := Low(TsgXMLId) to High(TsgXMLId) do
  begin
    if (I <> xmlUndefined) and (I <> xmlClassId) then//not (I in [xmlUndefined, xmlClassId]) then//reserved
      XMLIds.AddObject(LowerCase(cnstXMLNames[I].Name), TsgObjectWithField.CreateInt(Ord(I)));
  end;
end;

function GetXMLId(const AName: string): TsgXMLId;
var
  vIndex: Integer;
begin
  Result := xmlUndefined;
{$IFNDEF SG_OPENING_IN_THEADS}
  if XMLIds = nil then
    CreateXMLIdTable;
{$ENDIF}
  if Length(AName) > 0 then
  begin
    vIndex := XMLIds.IndexOf(LowerCase(AName));
    if vIndex > -1 then
      Result := TsgXMLId(TsgObjectInt64(XMLIds.Objects[vIndex]).FieldInt);
  end;
end;

function CreateXMLParams(const AMode: TsgXMLModes = cnstDefaultXMLMode): TsgXMLParams;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Mode := AMode;
end;

function GetSaveResultStr(const ASaveResult: Integer): string;
begin
  case ASaveResult of
    cnstSaveResultOk:                 Result := 'OK';
    cnstSaveResultNeedParams:         Result := 'Need parametrs';
    cnstSaveResultUndefinedFormat:    Result := 'Undefined format';
    cnstSaveResultUnsupportedFormat:  Result := sInvalidVersion;
    cnstSaveResultLimitationSave:     Result := sXMLInterfaceLimitationOnSaveMessage;
  else
    Result := 'Error';
  end;
end;

{$IFNDEF SG_WINDOWS}
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: TsgNativeUInt);
begin
  Move(Source^, Destination^, Length);
end;
{$ENDIF}

{$IFDEF SGFPC}
{$IFDEF SG_NON_WIN_PLATFORM}

{$IFDEF USE_SG_AGG2D}
  procedure agg_do_poly(path: path_storage_ptr; var Pts: PPoints; var Count: PInteger);
  var
    k: Integer;
  begin
    path.move_to(Pts^[0].x + 0.5, Pts^[0].y + 0.5);
    for k := 1 to Count^ - 1 do
      path.line_to(Pts^[k].x + 0.5, Pts^[k].y + 0.5);
    Inc(PByte(Pts), SizeOf(TPoint) * Count^);
    Inc(PByte(Count), SizeOf(Integer));
  end;
{$ENDIF}

function PolyPolyline(DC: HDC; const PointStructs; const Points; p4: DWORD): Bool; stdcall;
{$IFDEF SG_USE_AGG2D_AS_GDI}
begin
  Result := sgAGG2DIntf2.sgAgg2dWidgetSet.PolyPolyline(DC, PointStructs, Points, p4);
end;
{$ELSE}
var
  I: Integer;
  P: PPoints;
  C: PInteger;
begin
  Result := True;
  P := @PointStructs;
  C := @Points;
{$IFDEF USE_SG_AGG2D}
  TAggLCLCanvas(DC).Path.m_path.remove_all;
  for I := 0 to p4 - 1 do
    agg_do_poly(@TAggLCLCanvas(DC).Path.m_path, P, C);
  TAggLCLCanvas(DC).AggDrawPath(AGG_StrokeOnly);
{$ELSE}
  for I := 0 to p4 - 1 do
  begin
    Polyline(DC, @P^[0], C^);
    Inc(PByte(P), C^ * SizeOf(TPoint));
    Inc(C);
  end;
{$ENDIF}
end;
{$ENDIF}

function PolyPolygon(DC: HDC; var Points; var nPoints; p4: Integer): BOOL; stdcall;
{$IFDEF SG_USE_AGG2D_AS_GDI}
begin
  Result := sgAGG2DIntf2.sgAgg2dWidgetSet.PolyPolygon(DC, Points, nPoints, p4);
end;
{$ELSE}
var
  P: PPoints;
  C: PInteger;
{$IFDEF USE_SG_AGG2D}
  ps1, ps2: path_storage;
  gpc: conv_gpc;
{$ELSE}
  vPolyRegion, vTmpRegion: HRGN;
  R: TRect;
{$ENDIF}
begin
  Result := True;
  if p4 > 0 then
  begin
{$IFDEF USE_SG_AGG2D}
    TAggLCLCanvas(DC).Path.m_path.remove_all;
    P := @Points;
    C := @nPoints;
    ps1.Construct;
    ps2.Construct;
    gpc.Construct(@ps1, @ps2, agg_conv_gpc.gpc_xor);
    case p4 of
      1:
        begin
          agg_do_poly(@ps1, P, C);
          ps2.move_to(MaxInt, MaxInt);
          ps2.line_rel(0, 0);
        end;
    else
      while p4 > 1 do
      begin
        agg_do_poly(@ps1, P, C);
        Dec(p4);
      end;
      agg_do_poly(@ps2, P, C);
    end;
    gpc.rewind(0);
    TAggLCLCanvas(DC).Path.m_path.concat_path(@gpc);
    TAggLCLCanvas(DC).AggDrawPath(AGG_FillOnly);
    ps1.Destruct;
    ps2.Destruct;
    gpc.Destruct;
{$ELSE}
    vPolyRegion := CreateRectRgn(0, 0, 0, 0);
    try
      P := @Points;
      C := @nPoints;
      while p4 > 0 do
      begin
        vTmpRegion := CreatePolygonRgn(@P^[0], C^, WINDING);
        CombineRgn(vPolyRegion, vPolyRegion, vTmpRegion, RGN_XOR);
        DeleteObject(vTmpRegion);
        Inc(PByte(P), C^ * SizeOf(TPoint));
        Inc(C);
        Dec(p4);
      end;
      if not (GetRgnBox(vPolyRegion, @R) in [Region_Error, NullRegion]) then
        Result := FillRgn(DC, vPolyRegion, GetCurrentObject(DC, OBJ_BRUSH));
    finally
      DeleteObject(vPolyRegion);
    end;
{$ENDIF}
  end;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
procedure IniConstants;
var
  I: Integer;
begin
  sCreatedFileInfo := cnstCreatedFileInfo;
  sExporterSoftwareInfo := cnstExporterSoftwareInfo;
  for I := Low(cnstBTIEntNamesEx) to High(cnstBTIEntNamesEx) do
    sEntNamesEx[I] := cnstBTIEntNamesEx[I];
  GUID_XMLObject := StringToGUID(cnstGUID_XMLObject);
  GUID_InputRectCoords := StringToGUID(cnstGUID_InputRectCoords);
  GUID_ArrayFPoints := StringToGUID(cnstGUID_ArrayFPoints);
{$IFDEF SG_FIREMONKEY}
  DefRastrExportParamsObj := TsgObjRastrExportParams.Create;
{$ELSE}
  TsgObjRastrExportParams.InitDefValues(DefRastrExportParams);
{$ENDIF}
end;

procedure InitCodeTypes;
var
  I: Integer;
begin
  if CodeTypes = nil then
    GetMem(CodeTypes, SizeOf(TCodeTypes));
  for I := Low(CodeTypes^) to High(CodeTypes^) do
    case I of
      60 .. 79, 90 .. 99, 170 .. 179, 270 .. 279, 280 .. 289, 290 .. 299,
      370 .. 389, 400 .. 409, 420 .. 429, 440 .. 459, 1060 .. 1071:
        begin
          CodeTypes^[I] := ctInteger;
        end;
      10 .. 59, 110 .. 149, 210 .. 239, 460 .. 469, 1010 .. 1059:
        begin
          CodeTypes^[I] := ctDouble;
        end;
      5, 105, 160..169, 320 .. 329, 330 .. 369, 390 .. 399
      {$IFNDEF SG_BTI_HANDELS}, 1005{$ENDIF}:
        begin
          CodeTypes^[I] := ctHex;
        end;
      0 .. 4, 6 .. 9, 100, 102, 300 .. 309, 410 .. 419, 430 .. 439, 470 .. 479,
      999, 1000 .. 1004, {$IFDEF SG_BTI_HANDELS}1005, {$ENDIF}1006 .. 1009:
        begin
          CodeTypes^[I] := ctString;
        end;
    else
      CodeTypes^[I] := ctNode;
    end;
end;

procedure FreeCodeTypes;
begin
  FreeMemAndNil(Pointer(CodeTypes));
end;

function CreateStrListByNameObjects(const ANames: array of string;
  const AObjects: array of Integer): TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
{$IFDEF SGDEL_7}
  Result.CaseSensitive := False;
{$ENDIF}
  for I := Low(ANames) to High(ANames) do
    Result.AddObject(ANames[I], TsgObjectWithField.CreateInt(AObjects[I]));
end;

function GetColorByMaterialName(const AName: string; var AColor: Integer): Boolean;
var
  I: Integer;
begin
  if not Assigned(ColorsByMaterial) then
    ColorsByMaterial := CreateStrListByNameObjects(cnstMaterialNames, cnstMaterialColors);
  Result := False;
  I := ColorsByMaterial.IndexOf(AName);
  if I > -1 then
  begin
    AColor := TsgObjectInt64(ColorsByMaterial.Objects[I]).FieldInt;
    Result := True;
  end;
end;

function GetIDByMaterialName(const AName: string; var AID: string): Boolean;
var
  I: Integer;
begin
  if not Assigned(IDsByMaterial) then
    IDsByMaterial := CreateStrListByNameObjects(cnstMaterialNames, cnstMaterialIDIndexes);
  Result := False;
  I := IDsByMaterial.IndexOf(AName);
  if I > -1 then
  begin
    AID := cnstMaterialsIDs[TsgObjectInt64(IDsByMaterial.Objects[I]).FieldInt];
    Result := True;
  end;
end;

function GetViewDirectionName(ADirection: TsgDXFViewDirection): string;
begin
  Result := cnstDefaultViewsName[ADirection];
end;

function IsGermanyEdition: Boolean;
begin
{$IFDEF SG_BTI}
  Result := cnstBTITemplateType = cnstTemplateType_Germany;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function IntegerToFontStyles(const AMask : Integer) : TmvFontStyles;
var
  vStyles: TmvFontStyles absolute AMask;
begin
  Result := vStyles * cnstFontStylesMask;
end;

function FontStylesToInteger(const AFontStyles : TmvFontStyles) : Integer;
var
  J: Integer;
  vStyle: TmvFontStyle;
begin
  Result := 0;
  J := 1;
  for vStyle := Low(TmvFontStyle) to High(TmvFontStyle) do
  begin
    if vStyle in AFontStyles then
     Inc(Result, J);
    J := J shl 1;
  end;
end;

function MVFontStylesToFontStyles(const AFontStyles: TmvFontStyles): TFontStyles;
begin
  Result := [];
  if fmBold in AFontStyles then Include(Result, {$IFDEF SGDEL_2009}TFontStyle.{$ENDIF}fsBold);
  if fmItalic in AFontStyles then Include(Result, {$IFDEF SGDEL_2009}TFontStyle.{$ENDIF}fsItalic);
  if fmUnderline in AFontStyles then Include(Result, {$IFDEF SGDEL_2009}TFontStyle.{$ENDIF}fsUnderline);
  if fmStrikeOut in AFontStyles then Include(Result, {$IFDEF SGDEL_2009}TFontStyle.{$ENDIF}fsStrikeOut);
end;

function FontStylesToMVFontStyles(const AFontStyles: TFontStyles): TmvFontStyles;
begin
  Result := [];
  if {$IFDEF SGDEL_2009}TFontStyle.{$ENDIF}fsBold in AFontStyles then Include(Result, fmBold);
  if {$IFDEF SGDEL_2009}TFontStyle.{$ENDIF}fsItalic in AFontStyles then Include(Result, fmItalic);
  if {$IFDEF SGDEL_2009}TFontStyle.{$ENDIF}fsUnderline in AFontStyles then Include(Result, fmUnderline);
  if {$IFDEF SGDEL_2009}TFontStyle.{$ENDIF}fsStrikeOut in AFontStyles then Include(Result, fmStrikeOut);
end;

{$IFDEF MSWINDOWS}
procedure InitializeWineInfo;
type
  TWineGetVerion = function(): PAnsiChar; cdecl;
var
  ntdll: THandle;
  WineGetVerion: TWineGetVerion;
  vWineVer: PAnsiChar;
begin
  try
    vWineVer := nil;
    ntdll := GetModuleHandle('ntdll.dll');
    if ntdll <> 0 then
    begin
      @WineGetVerion := GetProcAddress(ntdll, 'wine_get_version');
      if @WineGetVerion <> nil then
        vWineVer := WineGetVerion;
      if vWineVer <> nil then
      begin
        IsWine := True;
        WineVer := string(vWineVer);
      end;
    end;
  except
    IsWine := False;
    WineVer := '';
  end;
end;
{$ENDIF}

function GetMainApplication(AInstance: TPersistent; out AMainApplication: IsgMainApplication): Boolean;
begin
  Pointer(AMainApplication) := nil;
  Result := False;
  if Assigned(AInstance) then
    repeat
      Result := Supports(AInstance, IsgMainApplication, AMainApplication);
      if not Result then
        AInstance := TPersistentAccess(AInstance).GetOwner;
    until Result or not Assigned(AInstance);
end;

{$IFNDEF SG_FIREMONKEY}
{$IFNDEF SGFPC}
function GetFileNameByHandle(Handle: THandle; var FileName: TFileName): Boolean;
var
  vFileSizeLow, vFileSizeHigh: DWORD;
  vFileMapHandle: THandle;
  vMemory: Pointer;
  vBuffer: array[0..MAX_PATH + 1] of Char;

  function ExpandVolumeName(const AFileName: string): string;
  var
    vDrives, vTemp: array[0..MAX_PATH + 1] of Char;
    vP: PChar;
    vLen: Integer;
    vVolumeName: string;
  begin
    vLen := GetLogicalDriveStrings(MAX_PATH, vDrives);

    if vLen > 0 then
    begin
      vP := @vDrives[0];

      repeat
        (vP + 2)^ := #0;
        vLen := Integer(QueryDosDevice(vP, vTemp, MAX_PATH));

        if vLen > 0 then
        begin
          vVolumeName := vTemp;
          if Pos(vVolumeName, AFileName) > 0 then
          begin
            vLen := Length(vVolumeName);
            Result := vP + Copy(AFileName, vLen + 1, Length(AFileName) - vLen);
            Break;
          end;
        end;

        while vP^ <> #0 do
          Inc(vP);
        Inc(vP, 2);

      until vP = '';
    end;
  end;


begin
  Result := False;
  FileName := '';
  vFileSizeHigh := 0;
  vFileSizeLow := GetFileSize(Handle, @vFileSizeHigh);
  if (vFileSizeLow <> 0) or (vFileSizeHigh <> 0) then
  begin
    vFileMapHandle := CreateFileMapping(Handle, nil, PAGE_READONLY, 0, 1, nil);

    if vFileMapHandle <> ERROR_FILE_INVALID then
    begin
      try
        vMemory := MapViewOfFile(vFileMapHandle, FILE_MAP_READ, 0, 0, 1);

        if (vMemory <> nil) then
        begin
          try
            if (GetMappedFileName(GetCurrentProcess(), vMemory, vBuffer, MAX_PATH) > 0) then
            begin
              FileName := ExpandVolumeName(vBuffer);
              Result := True;
            end;
          finally
            UnmapViewOfFile(vMemory);
          end;
        end;
      finally
        CloseHandle(vFileMapHandle);
      end;
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

{ TsgObjectWithField }

class function TsgObjectWithField.CreateClass(const AValue: TClass): TsgObjectWithField;
begin
  Result := TsgObjectTClass.Create(AValue);
end;

class function TsgObjectWithField.CreateColorCAD(
  const AValue: TsgColorCAD): TsgObjectWithField;
begin
  Result := TsgObjectColorCAD.Create(AValue);
end;

class function TsgObjectWithField.CreateDouble(
  const AValue: Double): TsgObjectWithField;
begin
  Result := TsgObjectDouble.Create(AValue);
end;

class function TsgObjectWithField.CreateFMatrix(
  const AValue: TFMatrix): TsgObjectWithField;
begin
  Result := TsgObjectFMatrix.Create(AValue);
end;

class function TsgObjectWithField.CreateFPoint(
  const AValue: TFPoint): TsgObjectWithField;
begin
 Result := TsgObjectFPoint.Create(AValue);
end;

class function TsgObjectWithField.CreateInt(
  const AValue: Integer): TsgObjectWithField;
begin
  Result := TsgObjectInt64.Create(Integer(AValue));
  //TsgObjectInt64(Result).FieldInt := AValue;
end;

class function TsgObjectWithField.CreateInt64(const AValue: Int64): TsgObjectWithField;
begin
  Result := TsgObjectInt64.Create(AValue);
end;

class function TsgObjectWithField.CreatePointer(
  const AValue: Pointer): TsgObjectWithField;
begin
  Result := TsgObjectPointer.Create(AValue);
end;

class function TsgObjectWithField.CreateString(
  const AValue: string): TsgObjectWithField;
begin
  Result := TsgObjectString.Create(AValue);
end;

class function TsgObjectWithField.CreateVariant(
  const AValue: Variant): TsgObjectWithField;
begin
  Result := TsgObjectVariant.Create(AValue);
end;

class function TsgObjectWithField.GetType(const AObjValue: TsgObjectWithField): TsgDataType;
begin
  Result := dtUndefined;
  if Assigned(AObjValue) then
    Result := AObjValue.GetDataType;
end;

{ TsgObjectInt }

constructor TsgObjectInt64.Create(const AValue: Int64 = 0);
begin
  FieldInt := AValue;
end;

{ TsgObjectClass }

constructor TsgObjectTClass.Create(const AValue: TClass);
begin
  FieldClass := AValue;
end;

function TsgObjectTClass.GetDataType: TsgDataType;
begin
  Result := dtClass;
end;

{ TsgObjectPointer }

constructor TsgObjectPointer.Create(const AValue: Pointer);
begin
  FieldPointer := AValue;
end;

function TsgObjectInt64.GetDataType: TsgDataType;
begin
  Result := dtInt64;
end;

function TsgObjectInt64.GetInt: Integer;
begin
  Result := Integer(FieldInt64)
end;

procedure TsgObjectInt64.SetInt(const Value: Integer);
begin
  FieldInt64 := Value;
end;

function TsgObjectPointer.GetDataType: TsgDataType;
begin
  Result := dtPointer;
end;

{ TsgObjectVariant }

constructor TsgObjectVariant.Create(const AValue: Variant);
begin
  FieldVariant := AValue;
end;

function TsgObjectVariant.GetDataType: TsgDataType;
begin
  Result := dtVariant;
end;

{ TsgObjectColorCAD }

constructor TsgObjectColorCAD.Create(const AValue: TsgColorCAD);
begin
  FieldColor := AValue;
end;

constructor TsgObjectColorCAD.Create(const AValue: TColor);
begin
  FieldColor.Active := acRGBColor;
  FieldColor.Color := AValue;
  FieldColor.AlbumString := '';
end;

function TsgObjectColorCAD.GetDataType: TsgDataType;
begin
  Result := dtColorCAD;
end;

constructor TsgObjectColorCAD.Create(const Active: TsgActiveColor;
  const Color: Cardinal; const Album: string = '');
begin
  inherited Create;
  FieldColor.Active := Active;
  FieldColor.Color := Color;
  FieldColor.AlbumString := Album;
end;

{ TsgObjectDouble }

constructor TsgObjectDouble.Create(const AValue: Double);
begin
  inherited Create;
  FieldDouble := AValue;
end;

function TsgObjectDouble.GetDataType: TsgDataType;
begin
  Result := dtDouble;
end;

{ TsgObjectString }

constructor TsgObjectString.Create(const AValue: string);
begin
  inherited Create;
  FieldStr := AValue;
end;

function TsgObjectString.GetDataType: TsgDataType;
begin
  Result := dtString;
end;


{$IFNDEF SG_HAS_STRINGBUILDER}

{ TStringBuilder }

constructor TStringBuilder.Create;
begin
  inherited;
end;

constructor TStringBuilder.Create(const Value: string);
begin
  Create(WideString(Value));
end;

constructor TStringBuilder.Create(const Value: WideString);
begin
  Create;
  FLength := System.Length(Value);
  SetLength(FData, FLength);
  System.Move(PPointer(Value)^, FData[0], FLength * SizeOf(WideChar));
end;

{$IFNDEF SGDEL_7}
class function TStringBuilder.CreateFromWideString(const Value: WideString): TStringBuilder;
var
  I: Integer;
begin
  Result := TStringBuilder.Create;
  Result.FLength := System.Length(Value);
  SetLength(Result.FData, Result.FLength);
  for I := 0 to Result.Length - 1 do
    Result.FData[I] := Value[I + 1];
end;

class function TStringBuilder.CreateFromAnsiString(const Value: AnsiString): TStringBuilder;
var
  I: Integer;
begin
  Result := TStringBuilder.Create;
  Result.FLength := System.Length(Value);
  SetLength(Result.FData, Result.FLength);
  for I := 0 to Result.Length - 1 do
    Result.FData[I] := WideChar(Value[I + 1]);
end;
{$ENDIF}

function TStringBuilder.GetChars(index: Integer): WideChar;
begin
  Result := FData[index];
end;

procedure TStringBuilder.SetChars(index: Integer; const Value: WideChar);
begin
  FData[index] := Value;
end;

function TStringBuilder.ToString(StartIndex, StrLength: Integer): sgUnicodeStr;
begin
  if StrLength <> 0 then
  begin
    if StartIndex < 0 then
      StartIndex := 0;
    if StrLength < 0 then
      StrLength := 0;
    System.SetLength(Result, StrLength);
    Move(FData[StartIndex], Result[1], StrLength * SizeOf(sgUnicodeChar));
  end
  else
    Result := '';
end;

function TStringBuilder.ToString: sgUnicodeStr;
begin
  SetLength(Result, FLength);
  System.Move(FData[0], Result[1], SizeOf(sgUnicodeChar) * FLength);
end;

{$ENDIF}

{ TsgStorage }

{$IFDEF SGFPC}
{$DEFINE FILESTREAM_HAS_PROP_FILENAME}
{$ENDIF}
{$IFDEF SGDEL_2006}
{$DEFINE FILESTREAM_HAS_PROP_FILENAME}
{$ENDIF}

const
  cnstEmptyMethod: TMethod = (Code: nil; Data: nil);
  cnstEmptyEvent: TNotifyEvent = nil;

function IsMethodEqual(const M1, M2: TMethod): Boolean; overload;
begin
  Result := (M1.Code = M2.Code) and (M1.Data = M2.Data);
end;

function IsMethodEqual(const E1, E2: TNotifyEvent): Boolean; overload;
begin
  Result := IsMethodEqual(TMethod(E1), TMethod(E2));
end;

function TsgStorage.AddListener(const ANotify: TNotifyEvent): Integer;
begin
  if not Find(ANotify, Result, MethodEmptyOrEqual) then
  begin
    Result := Length(FListeners);
    SetLength(FListeners, Result + 1);
  end;
  FListeners[Result] := ANotify;
  Inc(Result);
end;

procedure TsgStorage.Assign(Source: TPersistent);
var
  Src: TsgStorage;
begin
  if Source is TsgStorage then
  begin
    Src := TsgStorage(Source);
    if (FData <> Src.FData) or (FTypeInfo <> Src.FTypeInfo) then
      FinalizeData;
    FTypeInfo := Src.FTypeInfo;
    //FOwnObject := Src.FOwnObject;//??
    FSaved := Src.FSaved;
    case TsgStorage(Source).Kind of
      tkString{$IFDEF SGFPC}, tkAString{$ENDIF}: PAnsiString(@FData)^ := PAnsiString(@Src.FData)^;
      tkWString: PWideString(@FData)^ := PWideString(@Src.FData)^;
{$IFDEF UNICODESTRING_TYPE_DEFINED}
      tkUString: PUnicodeString(@FData)^ := PUnicodeString(@Src.FData)^;
{$ENDIF}
      tkClass: TObject(PPointer(@FData)^) := TObject(PPointer(@Src.FData)^);
    end;
  end
  else
    if Source = nil then
      FinalizeData
    else
      inherited Assign(Source);
end;

procedure TsgStorage.CopyToClipboard(AFormat: Word);
{$IFNDEF SG_FIREMONKEY}
var
  vStream: TStream;
  vGraphic: TGraphic;
  vFormat: Word;
  vData: THandle;
  vPalette: HPALETTE;
{$ENDIF}
begin
{$IFNDEF SG_FIREMONKEY}
  vStream := nil;
  if (Kind = tkClass) and GetTypeData(PTypeInfo(FTypeInfo))^.ClassType.InheritsFrom(TStream) then
    vStream := TStream(PPointer(@FData)^);
  if Assigned(vStream) then
  begin
    vStream.Position := 0;
{$IFDEF SGFPC}
    Clipboard.SetFormat(AFormat, vStream);
{$ELSE}
    vGraphic := nil;
    case AFormat of
      CF_BITMAP: vGraphic := TBitmap.Create;
{$IFDEF SG_DELPHI_VCL}
      CF_ENHMETAFILE: vGraphic := TMetafile.Create;
{$ENDIF}
    end;
    if Assigned(vGraphic) then
    try
      vGraphic.LoadFromStream(vStream);
      vGraphic.SaveToClipboardFormat(vFormat, vData, vPalette);
{$IFNDEF SG_FIREMONKEY}
      Clipboard.SetAsHandle(vFormat, vData);
{$ENDIF}
    finally
      vGraphic.Free;
    end;
{$ENDIF}
    vStream.Position := 0;
  end;
{$ENDIF}
end;

constructor TsgStorage.Create(const AFileName: TFileName; AOwnObject: Boolean);
begin
  Create(AOwnObject);
  FTypeInfo := TypeInfo(TFileName);
  PString(@FData)^ := AFileName;
end;

constructor TsgStorage.Create(const AStream: TStream; AOwnObject: Boolean);
begin
  Create(AOwnObject);
  if Assigned(AStream) then
    FTypeInfo := AStream.ClassInfo;
  TObject(PPointer(@FData)^) := AStream;
end;

constructor TsgStorage.Create(AOwnObject: Boolean);
begin
  FOwnObject := AOwnObject;
  FMode := fmCreate;
end;

function TsgStorage.Delete: Boolean;
var
  S: TFileName;
begin
  Result := False;
  try
    if IsFile then
    begin
      S := FileName;
      if IsStream then
        FinalizeData;
      Result := DeleteFile(S);
    end;
  except
  end;
end;

function TsgStorage.RemoveListener(const ANotify: TNotifyEvent): Integer;
begin
  Result := -1;
  if Find(ANotify, Result, MethodEqual) then
    FListeners[Result] := nil;
  Inc(Result);
end;

function TsgStorage.RemoveListener(ACookie: Integer): Integer;
begin
  Result := 0;
  Dec(ACookie);
  if (ACookie >= Low(FListeners)) and (ACookie <= High(FListeners)) then
  begin
    FListeners[ACookie] := nil;
    Result := ACookie + 1;
  end;
end;

function TsgStorage.Rename(const ANewName: TFileName): Boolean;
var
  S: TFileName;
begin
  Result := False;
  if IsFile then
  begin
    S := FileName;
    Close;
    try
      Result := RenameFile(S, ANewName);
      FileName := ANewName;
    except
    end;
  end;
end;

destructor TsgStorage.Destroy;
begin
  NotifyListeners;
  FinalizeData;
  inherited Destroy;
end;

procedure TsgStorage.DoAfterSaveToStream(Sender: TObject);
begin
  if Assigned(FOnAfterSaveToStream) then
    FOnAfterSaveToStream(Sender);
end;

procedure TsgStorage.DoBeforeSaveToStream(Sender: TObject);
begin
  if Assigned(FOnBeforeSaveToStream) then
    FOnBeforeSaveToStream(Sender);
end;

function TsgStorage.Close: Boolean;
var
  S: TFileName;
begin
  Result := False;
  if IsFile then
  begin
    S := FileName;
    if IsStream then
    begin
      FinalizeData;
      Result := True;
    end;
    FileName := S;
  end
  else
    Result := True;
end;

procedure TsgStorage.FinalizeData;
var
  S: string;
begin
  case Kind of
    tkString{$IFDEF SGFPC}, tkAString{$ENDIF}: PAnsiString(@FData)^ := '';
    tkWString: PWideString(@FData)^ := '';
{$IFDEF UNICODESTRING_TYPE_DEFINED}
    tkUString: PUnicodeString(@FData)^ := '';
{$ENDIF}
    tkClass:
      begin
        S := GetFileNameFromStream(TObject(PPointer(@FData)^));
        if OwnObject then
          FreeAndNil(TObject(PPointer(@FData)^));
        if (not FSaved and (FMode = fmCreate)) and (S <> '') and FileExists(S) then
          DeleteFile(S);
      end;
  end;
end;

function TsgStorage.Find(const ANotify: TNotifyEvent;
  var Index: Integer; ACompareMethods: TsgNotifyEventCompare): Boolean;
begin
  Index := High(FListeners);
  Result := False;
  while (Index >= Low(FListeners)) and not Result do
    if ACompareMethods(FListeners[Index], ANotify) = 0 then
      Inc(Result)
    else
      Dec(Index);
end;

function TsgStorage.GetConnected: Boolean;
var
  I: Integer;
begin
  Result := Find(cnstEmptyEvent, I, MethodNotEqual);
end;

function TsgStorage.GetFileName: TFileName;
begin
  case Kind of
    tkString{$IFDEF SGFPC}, tkAString{$ENDIF}: Result := TFileName(PAnsiString(@FData)^);
    tkWString: Result := TFileName(PWideString(@FData)^);
{$IFDEF UNICODESTRING_TYPE_DEFINED}
    tkUString: Result := TFileName(PUnicodeString(@FData)^);
{$ENDIF}
    tkClass: Result := GetFileNameFromStream(TObject(PPointer(@FData)^));
  else
    Result := '';
  end;
end;

function TsgStorage.GetFileNameFromStream(Stream: TObject): TFileName;
begin
  Result := '';
  if Stream is THandleStream then
  begin
{$IFDEF FILESTREAM_HAS_PROP_FILENAME}
    if Stream is TFileStream then Result := TFileStream(Stream).FileName else
{$ENDIF}
{$IFNDEF SG_FIREMONKEY}
{$IFNDEF SGFPC}
    GetFileNameByHandle(THandleStream(Stream).Handle, Result);
{$ENDIF}
{$ENDIF}
  end;
end;

function TsgStorage.GetIsFile: Boolean;
begin
  case Kind of
    tkString{$IFDEF SGFPC}, tkAString{$ENDIF}, tkWString{$IFDEF UNICODESTRING_TYPE_DEFINED}, tkUString{$ENDIF}: Result := True;
    tkClass: Result := TObject(PPointer(@FData)^) is THandleStream;
  else
    Result := False;
  end;
end;

function TsgStorage.GetIsStream: Boolean;
begin
  Result := (Kind = tkClass) and GetTypeData(PTypeInfo(FTypeInfo))^.ClassType.InheritsFrom(TStream);
end;

function TsgStorage.GetKind: TTypeKind;
begin
  if Assigned(FTypeInfo) then
    Result := PTypeInfo(FTypeInfo)^.Kind
  else
    Result := tkUnknown;
end;

function TsgStorage.GetTypeName: string;
{$IFNDEF SGDEL_6}
  type
    UTF8String = type string;

  function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
  var
    i, count: Cardinal;
    c: Byte;
    wc: Cardinal;
  begin
    if Source = nil then
    begin
      Result := 0;
      Exit;
    end;
    Result := Cardinal(-1);
    count := 0;
    i := 0;
    if Dest <> nil then
    begin
      while (i < SourceBytes) and (count < MaxDestChars) do
      begin
        wc := Cardinal(Source[i]);
        Inc(i);
        if (wc and $80) <> 0 then
        begin
          if i >= SourceBytes then Exit;          // incomplete multibyte char
          wc := wc and $3F;
          if (wc and $20) <> 0 then
          begin
            c := Byte(Source[i]);
            Inc(i);
            if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
            if i >= SourceBytes then Exit;        // incomplete multibyte char
            wc := (wc shl 6) or (c and $3F);
          end;
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;       // malformed trail byte
 
          Dest[count] := WideChar((wc shl 6) or (c and $3F));
        end
        else
          Dest[count] := WideChar(wc);
        Inc(count);
      end;
      if count >= MaxDestChars then count := MaxDestChars-1;
      Dest[count] := #0;
    end
    else
    begin
      while (i < SourceBytes) do
      begin
        c := Byte(Source[i]);
        Inc(i);
        if (c and $80) <> 0 then
        begin
          if i >= SourceBytes then Exit;          // incomplete multibyte char
          c := c and $3F;
          if (c and $20) <> 0 then
          begin
            c := Byte(Source[i]);
            Inc(i);
            if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
            if i >= SourceBytes then Exit;        // incomplete multibyte char
          end;
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;       // malformed trail byte
        end;
        Inc(count);
      end;
    end;
    Result := count+1;
  end;

  function Utf8ToAnsi(const S: UTF8String): WideString;
  var
    L: Integer;
    Temp: WideString;
  begin
    Result := '';
    if S = '' then Exit;
    SetLength(Temp, Length(S));

    L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
    if L > 0 then
      SetLength(Temp, L-1)
    else
      Temp := '';
    Result := Temp;
  end;
{$ENDIF}
begin
  Result := '';
  if Assigned(FTypeInfo) then
{$IFDEF SG_FIREMONKEY}
    Result := PTypeInfo(FTypeInfo)^.NameFld.ToString;
{$ELSE}
    Result := Utf8ToAnsi(PTypeInfo(FTypeInfo)^.Name);
{$ENDIF}
end;

function TsgStorage.Load(ADestObject: TPersistent): Boolean;
var
  vStmPersist: IStreamPersist;
begin
  Result := False;
  FMode := fmOpenRead or fmShareDenyWrite;
{$IFNDEF SG_FIREMONKEY}
  if ADestObject is TGraphic then
  begin
    TGraphic(ADestObject).LoadFromStream(Stream);
    Result := True;
  end
  else
{$ENDIF}
    if Supports(ADestObject, IStreamPersist, vStmPersist) then
    begin
      vStmPersist.LoadFromStream(Stream);
      Result := True;
    end;
end;

function TsgStorage.MethodEmptyOrEqual(const A, B: TNotifyEvent): Integer;
begin
  Result := Ord(not (IsMethodEqual(A, cnstEmptyEvent) or IsMethodEqual(A, B)));
end;

function TsgStorage.MethodEqual(const A, B: TNotifyEvent): Integer;
begin
  Result := Ord(not IsMethodEqual(A, B));
end;

function TsgStorage.MethodNotEqual(const A, B: TNotifyEvent): Integer;
begin
  Result := Ord(IsMethodEqual(A, B));
end;

procedure TsgStorage.NotifyListeners;
var
  I: Integer;
begin
  I := High(FListeners);
  while I >= Low(FListeners) do
  begin
    if not IsMethodEqual(FListeners[I], cnstEmptyEvent) then
    begin
      FListeners[I](Self);
      FListeners[I] := nil;
    end;
    Dec(I);
  end;
end;

function TsgStorage.GetStream: TStream;
begin
  Result := nil;
  case Kind of
    tkClass: if TObject(PPointer(@FData)^) is TStream then Result := TStream(PPointer(@FData)^);
    tkString{$IFDEF SGFPC}, tkAString{$ENDIF}: Result := TFileStream.Create(string(PAnsiString(@FData)^), FMode);
    tkWString: Result := TFileStream.Create(string(PWideString(@FData)^), FMode);
{$IFDEF UNICODESTRING_TYPE_DEFINED}
    tkUString: Result := TFileStream.Create(string(PUnicodeString(@FData)^), FMode);
{$ENDIF}
    tkUnknown: Result := TMemoryStream.Create;
  end;
  if Assigned(Result) and (Result.ClassInfo <> FTypeInfo) then
  begin
    FinalizeData;
    FTypeInfo := Result.ClassInfo;
    TStream(PPointer(@FData)^) := Result;
    FSaved := False;
  end;
end;

function TsgStorage.Save(AExportObject: TPersistent): Boolean;
var
  vStmPersist: IStreamPersist;
begin
  Result := False;
{$IFNDEF SG_FIREMONKEY}
  if AExportObject is TGraphic then
  begin
    DoBeforeSaveToStream(AExportObject);
    try
      TGraphic(AExportObject).SaveToStream(Stream);
      FSaved := True;
      Result := True;
    finally
      DoAfterSaveToStream(Stream);
    end;
  end
  else
{$ENDIF}
    if Supports(AExportObject, IStreamPersist, vStmPersist) then
    begin
      DoBeforeSaveToStream(AExportObject);
      try
        vStmPersist.SaveToStream(Stream);
        FSaved := True;
        Result := True;
      finally
        DoAfterSaveToStream(Stream);
      end;
    end;
end;

procedure TsgStorage.SetFileName(const Value: TFileName);
begin
  if (FTypeInfo <> TypeInfo(TFileName)) or
     not {$IFDEF SGDEL_7}SameFileName{$ELSE}SameText{$ENDIF}(Value, GetFileName) then
  begin
    FinalizeData;
    FTypeInfo := TypeInfo(TFileName);
    TFileName(PPointer(@FData)^) := Value;
    if Value = '' then FTypeInfo := nil;
    FSaved := False;
  end;
end;

procedure TsgStorage.SetStream(const Value: TStream);
var
  vValueType: Pointer;
begin
  vValueType := nil;
  if Assigned(Value) then
    vValueType := Value.ClassInfo;
  if (FTypeInfo <> vValueType) or (Pointer(Value) <> PPointer(@FData)^) then
  begin
    FinalizeData;
    FTypeInfo := vValueType;
    TObject(PPointer(@FData)^) := Value;
    FSaved := False;
  end;
end;

{ TsgGLGraphicCustom }

procedure TsgGLGraphicCustom.Changed(Sender: TObject);
begin
{$IFNDEF SG_FIREMONKEY}
  inherited;
{$ENDIF}
end;

procedure TsgGLGraphicCustom.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
{$IFNDEF SG_FIREMONKEY}
  inherited;
{$ENDIF}
end;

procedure TsgGLGraphicCustom.DrawTransparent(ACanvas: TCanvas;
  const Rect: TRect; Opacity: Byte);
begin
{$IFNDEF SG_FIREMONKEY}
{$IFDEF SGDEL_2009}
  inherited;
{$ENDIF}
{$ENDIF}
end;

procedure TsgGLGraphicCustom.UpdateViewport;
begin
end;

{ TsgObjRastrExportParams }

constructor TsgObjRastrExportParams.Create;
begin
  inherited Create;
  InitDefValues(Fields);
end;

class procedure TsgObjRastrExportParams.InitDefValues(
  var AFields: TsgRastrExportParams);
begin
  AFields.SizeMode := smSame;
  AFields.Width  := 640;
  AFields.Height := 480;
  AFields.MaxDim := 99999999;
  AFields.Depth := pf24bit;
  AFields.Compression := ctNone;
  AFields.DPU := Point(96, 96);
  AFields.MeasureInPixels := True;
  AFields.Transparent := False;
  AFields.Quality := cnstJPEGQuality;
  AFields.SaveProportion := True;
  AFields.Size.X := 0;
  AFields.Size.Y := 0;
  AFields.DPM := 0;
  AFields.ImageQuality := 1;
  AFields.UserScale := '';
  AFields.BlackWhitePic := False;
  AFields.Smooth := False;
end;

procedure FreeGlobalList;
{$IFDEF SG_GLOBALHANDLE}
var
  I: Integer;
   vInfo: string;
   vLog: TStringList;
begin
  vLog := TStringList.Create;
  try
    if GlobalList.Count > 0 then
    begin
      vLog.Add('Leaks entities count = ' + IntToStr(GlobalList.Count));
      if Assigned(GlobalListItemInfo) then
      begin
        for I := 0 to GlobalList.Count - 1 do
        begin
          vInfo := GlobalListItemInfo(GlobalList[I]);
          vLog.Add(vInfo);
        end;
        vInfo := vLog.Text;
        ShowMessage(vInfo);
      end;
    end;
  finally
    FreeAndNil(vLog);
    FreeAndNil(GlobalList);
  end;
{$ELSE}
begin
{$ENDIF}
end;

{$IFDEF SG_HAS_ACTIVEX}
function GetGlobalInterfaceTable(reserved: Integer; out git: IGlobalInterfaceTable): HRESULT;
const
  CLSID_StdGlobalInterfaceTable : TGUID = '{00000323-0000-0000-C000-000000000046}';
begin
  Result := CoCreateInstance(CLSID_StdGlobalInterfaceTable, nil, CLSCTX_INPROC_SERVER, IGlobalInterfaceTable, git);
end;
{$ENDIF}

function NullGetGlobalPropProvider(AInstance: TObject; AOwnsProps: Boolean;
  out AGlobalPropProvider: IsgGlobalPropProvider): Boolean;
begin
  Result := False;
end;

{ TsgObjectFMatrix }

constructor TsgObjectFMatrix.Create(const AValue: TFMatrix);
begin
  inherited Create;
  FielFMatrix := AValue;
end;

function TsgObjectFMatrix.GetDataType: TsgDataType;
begin
  Result := dtVariant;
end;


{ TsgObjectGUID }

constructor TsgObjectGUID.Create(const AValue: TGUID);
begin
  inherited Create;
  FieldGUID := AValue;
end;

function TsgObjectGUID.GetDataType: TsgDataType;
begin
  Result := dtGuid;
end;

{  TsgObjectFPoint }

function TsgObjectFPoint.Get2DPoint: TF2DPoint;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
end;

function TsgObjectFPoint.GetDataType: TsgDataType;
begin
  Result := dtTFPoint;
end;

function TsgObjectFPoint.GetPoint2D: TFPoint;
begin
  Result := Point;
  Result.Z := 0;
end;

procedure TsgObjectFPoint.Set2DPoint(const AValue: TF2DPoint);
begin
  Point.X := AValue.X;
  Point.Y := AValue.Y;
  Point.Z := 0;
end;

procedure TsgObjectFPoint.SetPoint2D(const AValue: TFPoint);
begin
  Point.X := AValue.X;
  Point.Y := AValue.Y;
  Point.Z := 0;
end;

constructor TsgObjectFPoint.Create(const AValue: TFPoint);
begin
  inherited Create;
  Point := AValue;
end;

{$IFDEF SGDEL_XE2}

{ TsgFileStreamTmp }

destructor TsgFileStreamTmp.Destroy;
var
  vFileName: string;
begin
  vFileName := '';
  if DeleteOnFree then
    vFileName := FileName;
  inherited;
  if Length(vFileName) > 0 then
  begin
    try
      DeleteFile(vFileName);
    except
    end;
  end;
end;
{$ENDIF}


//procedure TestXmlName;
//var
//  T1, T2: TsgXMLId;
//  vList: TList;
//begin
//  vList := TList.Create;
//  try
//    for T1 := Low(TsgXMLId) to TsgXMLId(Ord(High(TsgXMLId))- 1) do
//    begin
//      if Length(cnstXMLNames[T1].Name) = 0 then
//        Continue;
//      for T2 := TsgXMLId(Ord(T1) + 1) to High(TsgXMLId) do
//      begin
//        if SameText(cnstXMLNames[T1].Name, cnstXMLNames[T2].Name)  then
//        begin
//          vList.Add(Pointer(T1));
//          vList.Add(Pointer(T2));
//        end;
//      end;
//    end;
//  finally
//    vList.Free;
//  end;
//end;

initialization
  if {$IFNDEF FPC_OBJFPC}@{$ENDIF}GetGlobalPropProvider = nil then
    GetGlobalPropProvider := {$IFNDEF FPC_OBJFPC}@{$ENDIF}NullGetGlobalPropProvider;
{$IFDEF SG_GLOBALHANDLE}
  GlobalIndex := 0;
  GlobalList := TList.Create;
  GlobalList.Capacity := 1000000;
{$ENDIF}
  IniConstants;
  InitCodeTypes;
  cnstSGHeadVarStruct := SGHeadVarStruct({$IFDEF SG_UNITS_MM}True{$ELSE}False{$ENDIF});
{$IFDEF SG_MODULE_LOG}
  if IsLibrary then
    LogFileName := ChangeFileExt(GetModuleName(HInstance), '.log')
  else
    LogFileName := ChangeFileExt(ParamStr(0), '.log');
  Log := TStringList.Create;
  if FileExists(LogFileName) then
    Log.LoadFromFile(LogFileName);
{$ENDIF}
  GUID_ArrayFPoints := StringToGUID(cnstGUID_ArrayFPoints);
{$IFDEF SG_OPENING_IN_THEADS}
  CreateXMLIdTable;
{$ENDIF}
  cnstBlockDescriptionkName := cnstInternalBlockPrefix + cnstBlockPatternNameDefault[btiConstruction];
  ColorsByMaterial := nil;
{$IFDEF SG_FIREMONKEY}
  GlobalDrawingStyles := [fssShading];
{$ENDIF}
//  TestXmlName;

finalization
  FreeGlobalList;
  FreeCodeTypes;
  FreeList(vLogs);
{$IFDEF SG_MODULE_LOG}
  FreeAndNil(Log);
{$ENDIF}
  ClearObjects(XMLIds);
  FreeAndNil(XMLIds);
  FreeParams;
  ClearObjects(ColorsByMaterial);
  FreeAndNil(ColorsByMaterial);
  ClearObjects(IDsByMaterial);
  FreeAndNil(IDsByMaterial);
{$IFDEF SG_FIREMONKEY}
  FreeAndNil(DefRastrExportParamsObj);
{$ENDIF}

end.



