{************************************************************}
{                                                            }
{              Borland Delphi Runtime Library                }
{                                                            }
{                   GDIPLUS interface unit                   }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}
unit GDIPLUS;
{$INCLUDE SGDXF.INC}

interface

uses
  Windows, ActiveX, Math, Graphics, SysUtils{$IFNDEF SGDEL_6}, sgConsts{$ENDIF};

{$IFDEF SGDEL_6}
  {$IFNDEF BCB}
     {$DEFINE SGFULLGDIPLUS}
  {$ELSE}
    {$IFDEF SGDEL_2007}
      {$DEFINE SGFULLGDIPLUS}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

(*$HPPEMIT '// Provides C++ Builder correct declaration structure type name'*)
(*$HPPEMIT '// EnhMetaHeader3'*)
(*$HPPEMIT 'namespace Gdiplus               '*)
(*$HPPEMIT '{                               '*)
(*$HPPEMIT 'typedef TRect RECTL;            '*)
(*$HPPEMIT '#if CompilerVersion < 23        '*)
(*$HPPEMIT 'typedef TSize SIZEL;            '*)
(*$HPPEMIT '#endif                          '*)
(*$HPPEMIT '#pragma pack(push,1)            '*)
(*$HPPEMIT 'struct ENHMETAHEADER3           '*)
(*$HPPEMIT '{                               '*)
(*$HPPEMIT '  DWORD iType;                  '*)
(*$HPPEMIT '  DWORD nSize;                  '*)
(*$HPPEMIT '  RECTL rclBounds;              '*)
(*$HPPEMIT '  RECTL rclFrame;               '*)
(*$HPPEMIT '  DWORD dSignature;             '*)
(*$HPPEMIT '  DWORD nVersion;               '*)
(*$HPPEMIT '  DWORD nBytes;                 '*)
(*$HPPEMIT '  DWORD nRecords;               '*)
(*$HPPEMIT '  WORD nHandles;                '*)
(*$HPPEMIT '  WORD sReserved;               '*)
(*$HPPEMIT '  DWORD nDescription;           '*)
(*$HPPEMIT '  DWORD offDescription;         '*)
(*$HPPEMIT '  DWORD nPalEntries;            '*)
(*$HPPEMIT '  SIZEL szlDevice;              '*)
(*$HPPEMIT '  SIZEL szlMillimeters;         '*)
(*$HPPEMIT '};                              '*)
(*$HPPEMIT '#pragma pack(pop)               '*)
(*$HPPEMIT 'typedef ENHMETAHEADER3 TENHMETAHEADER3;'*)
(*$HPPEMIT 'typedef ENHMETAHEADER3 *PENHMETAHEADER3;'*)
(*$HPPEMIT '                                '*)
(*$HPPEMIT '#pragma pack(push,1)            '*)
(*$HPPEMIT 'struct TMetafileHeaderEx        '*)
(*$HPPEMIT '{                               '*)
(*$HPPEMIT '                                '*)
(*$HPPEMIT '	union                         '*)
(*$HPPEMIT '	{                             '*)
(*$HPPEMIT '		struct EnhMetaHeader3       '*)
(*$HPPEMIT '		{                           '*)
(*$HPPEMIT '			ENHMETAHEADER3 EmfHeader; '*)
(*$HPPEMIT '		};                          '*)
(*$HPPEMIT '		struct                      '*)
(*$HPPEMIT '		{                           '*)
(*$HPPEMIT '			tagMETAHEADER WmfHeader;  '*)
(*$HPPEMIT '                                '*)
(*$HPPEMIT '		};                          '*)
(*$HPPEMIT '                                '*)
(*$HPPEMIT '	};                            '*)
(*$HPPEMIT '} ;                             '*)
(*$HPPEMIT '#pragma pack(pop)               '*)
(*$HPPEMIT '} // namespace Gdiplus          '*)

type
  INT16   = type Smallint;
  UINT16  = type Word;
  PUINT16 = ^UINT16;
  UINT32  = type Cardinal;
  TSingleDynArray = array of Single;

type
  ImageAbort = function: BOOL; stdcall;
  DrawImageAbort         = ImageAbort;
  GetThumbnailImageAbort = ImageAbort;

type
  TsgGDIPlusBase = class
  private
    FCallBack: DrawImageAbort;
    FCallBackData: Pointer;
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    property CallBack: DrawImageAbort read FCallBack write FCallBack;
    property CallBackData: Pointer read FCallBackData write FCallBackData;
  end;

type
  Status = (Ok, GenericError, InvalidParameter, OutOfMemory, ObjectBusy, InsufficientBuffer,
    NotImplemented, Win32Error, WrongState, Aborted, FileNotFound, ValueOverflow,
    AccessDenied, UnknownImageFormat, FontFamilyNotFound, FontStyleNotFound, NotTrueTypeFont,
    UnsupportedGDIPlusVersion, GDIPlusNotInitialized, PropertyNotFound, PropertyNotSupported);
  TsgGDIPStatus = Status;

  TsgGDIPlusOpimization = (opDefault, opSpeed, opQuality);

const
  FlatnessDefault = 0.25;
type
  GraphicsState     = UINT;
  GraphicsContainer = UINT;

  FillMode = (FillModeAlternate, FillModeWinding);
  TsgGDIPFillMode = FillMode;

{$IFDEF SGFULLGDIPLUS}
  QualityMode = (
    QualityModeInvalid   = -1,
    QualityModeDefault   =  0,
    QualityModeLow       =  1,
    QualityModeHigh      =  2);
  TQualityMode = QualityMode;
{$ELSE}
  QualityMode = Integer;
  const
    QualityModeInvalid   = -1;
    QualityModeDefault   =  0;
    QualityModeLow       =  1;
    QualityModeHigh      =  2;
{$ENDIF}

type
  CompositingMode = (CompositingModeSourceOver, CompositingModeSourceCopy);
  TCompositingMode = CompositingMode;

{$IFDEF SGFULLGDIPLUS}
  CompositingQuality = (
    CompositingQualityInvalid          = Ord(QualityModeInvalid),
    CompositingQualityDefault          = Ord(QualityModeDefault),
    CompositingQualityHighSpeed        = Ord(QualityModeLow),
    CompositingQualityHighQuality      = Ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear);
  TCompositingQuality = CompositingQuality;
{$ELSE}
  CompositingQuality = Integer;
  const
    CompositingQualityInvalid          = QualityModeInvalid;
    CompositingQualityDefault          = QualityModeDefault;
    CompositingQualityHighSpeed        = QualityModeLow;
    CompositingQualityHighQuality      = QualityModeHigh;
    CompositingQualityGammaCorrected   = 3;
    CompositingQualityAssumeLinear     = 4;

type
  TCompositingQuality = CompositingQuality;
{$ENDIF}

  Unit_ = Integer;
  const
    UnitWorld = 0;
    UnitDisplay = 1;
    UnitPixel = 2;
    UnitPoint = 3;
    UnitInch = 4;
    UnitDocument = 5;
    UnitMillimeter = 6;
type
  TsgGDIPUnit = Unit_;

{$IFDEF SGFULLGDIPLUS}
  MetafileFrameUnit = (
    MetafileFrameUnitPixel      = Ord(UnitPixel),
    MetafileFrameUnitPoint      = Ord(UnitPoint),
    MetafileFrameUnitInch       = Ord(UnitInch),
    MetafileFrameUnitDocument   = Ord(UnitDocument),
    MetafileFrameUnitMillimeter = Ord(UnitMillimeter),
    MetafileFrameUnitGdi);
  TMetafileFrameUnit = MetafileFrameUnit;
{$ELSE}
  MetafileFrameUnit = Integer;
  const
    MetafileFrameUnitPixel      = 2;
    MetafileFrameUnitPoint      = 3;
    MetafileFrameUnitInch       = 4;
    MetafileFrameUnitDocument   = 5;
    MetafileFrameUnitMillimeter = 6;
    MetafileFrameUnitGdi        = 7;

type
  TMetafileFrameUnit = MetafileFrameUnit;
{$ENDIF}

  CoordinateSpace = (CoordinateSpaceWorld, CoordinateSpacePage, CoordinateSpaceDevice);
  TsgGDIPCoordinateSpace = CoordinateSpace;

  WrapMode = (WrapModeTile, WrapModeTileFlipX, WrapModeTileFlipY, WrapModeTileFlipXY, WrapModeClamp);
  TsgGDIPWrapMode = WrapMode;

  HatchStyle = (
    HatchStyleHorizontal,
    HatchStyleVertical,
    HatchStyleForwardDiagonal,
    HatchStyleBackwardDiagonal,
    HatchStyleCross,
    HatchStyleDiagonalCross,
    HatchStyle05Percent,
    HatchStyle10Percent,
    HatchStyle20Percent,
    HatchStyle25Percent,
    HatchStyle30Percent,
    HatchStyle40Percent,
    HatchStyle50Percent,
    HatchStyle60Percent,
    HatchStyle70Percent,
    HatchStyle75Percent,
    HatchStyle80Percent,
    HatchStyle90Percent,
    HatchStyleLightDownwardDiagonal,
    HatchStyleLightUpwardDiagonal,
    HatchStyleDarkDownwardDiagonal,
    HatchStyleDarkUpwardDiagonal,
    HatchStyleWideDownwardDiagonal,
    HatchStyleWideUpwardDiagonal,
    HatchStyleLightVertical,
    HatchStyleLightHorizontal,
    HatchStyleNarrowVertical,
    HatchStyleNarrowHorizontal,
    HatchStyleDarkVertical,
    HatchStyleDarkHorizontal,
    HatchStyleDashedDownwardDiagonal,
    HatchStyleDashedUpwardDiagonal,
    HatchStyleDashedHorizontal,
    HatchStyleDashedVertical,
    HatchStyleSmallConfetti,
    HatchStyleLargeConfetti,
    HatchStyleZigZag,
    HatchStyleWave,
    HatchStyleDiagonalBrick,
    HatchStyleHorizontalBrick,
    HatchStyleWeave,
    HatchStylePlaid,
    HatchStyleDivot,
    HatchStyleDottedGrid,
    HatchStyleDottedDiamond,
    HatchStyleShingle,
    HatchStyleTrellis,
    HatchStyleSphere,
    HatchStyleSmallGrid,
    HatchStyleSmallCheckerBoard,
    HatchStyleLargeCheckerBoard,
    HatchStyleOutlinedDiamond,
    HatchStyleSolidDiamond,
    HatchStyleTotal);

  const
    HatchStyleLargeGrid = HatchStyleCross;
    HatchStyleMin       = HatchStyleHorizontal;
    HatchStyleMax       = HatchStyleSolidDiamond;

type
  TsgGDIPHatchStyle = HatchStyle;

  DashStyle = (DashStyleSolid, DashStyleDash, DashStyleDot,
    DashStyleDashDot, DashStyleDashDotDot, DashStyleCustom);
  TsgGDIPDashStyle = DashStyle;

{$IFDEF SGFULLGDIPLUS}
  DashCap = (
    DashCapFlat             = 0,
    DashCapRound            = 2,
    DashCapTriangle         = 3);
  TsgGDIPDashCap = DashCap;
{$ELSE}
  DashCap = Integer;
  const
    DashCapFlat             = 0;
    DashCapRound            = 2;
    DashCapTriangle         = 3;

type
  TsgGDIPDashCap = DashCap;
{$ENDIF}

{$IFDEF SGFULLGDIPLUS}
  LineCap = (
    LineCapFlat             = 0,
    LineCapSquare           = 1,
    LineCapRound            = 2,
    LineCapTriangle         = 3,
    LineCapNoAnchor         = $10,
    LineCapSquareAnchor     = $11,
    LineCapRoundAnchor      = $12,
    LineCapDiamondAnchor    = $13,
    LineCapArrowAnchor      = $14,
    LineCapCustom           = $ff,
    LineCapAnchorMask       = $f0);
  TsgGDIPLineCap = LineCap;
{$ELSE}
  LineCap = Integer;
  const
    LineCapFlat             = 0;
    LineCapSquare           = 1;
    LineCapRound            = 2;
    LineCapTriangle         = 3;
    LineCapNoAnchor         = $10;
    LineCapSquareAnchor     = $11;
    LineCapRoundAnchor      = $12;
    LineCapDiamondAnchor    = $13;
    LineCapArrowAnchor      = $14;
    LineCapCustom           = $ff;
    LineCapAnchorMask       = $f0;
type
  TsgGDIPLineCap = LineCap;
{$ENDIF}

  CustomLineCapType = (CustomLineCapTypeDefault,CustomLineCapTypeAdjustableArrow);
  TCustomLineCapType = CustomLineCapType;

  LineJoin = (LineJoinMiter, LineJoinBevel, LineJoinRound, LineJoinMiterClipped);
  TsgGDIPLineJoin = LineJoin;

{$IFDEF SGFULLGDIPLUS}
  {$Z1}
  PathPointType = (
    PathPointTypeStart           = $00,
    PathPointTypeLine            = $01,
    PathPointTypeBezier          = $03,
    PathPointTypePathTypeMask    = $07,
    PathPointTypeDashMode        = $10,
    PathPointTypePathMarker      = $20,
    PathPointTypeCloseSubpath    = $80,
    PathPointTypeBezier3         = $03);
  TPathPointType = PathPointType;
  {$Z4}
{$ELSE}
  PathPointType = Byte;
  const
    PathPointTypeStart         : Byte = $00;
    PathPointTypeLine          : Byte = $01;
    PathPointTypeBezier        : Byte = $03;
    PathPointTypePathTypeMask  : Byte = $07;
    PathPointTypeDashMode      : Byte = $10;
    PathPointTypePathMarker    : Byte = $20;
    PathPointTypeCloseSubpath  : Byte = $80;
    PathPointTypeBezier3       : Byte = $03;
type
  TPathPointType = PathPointType;
{$ENDIF}

  WarpMode = (WarpModePerspective, WarpModeBilinear);
  TWarpMode = WarpMode;

  LinearGradientMode = (LinearGradientModeHorizontal, LinearGradientModeVertical,
    LinearGradientModeForwardDiagonal, LinearGradientModeBackwardDiagonal);
  TLinearGradientMode = LinearGradientMode;

  CombineMode = (CombineModeReplace, CombineModeIntersect, CombineModeUnion,
    CombineModeXor, CombineModeExclude, CombineModeComplement);
  TCombineMode = CombineMode;

  ImageType = (ImageTypeUnknown, ImageTypeBitmap, ImageTypeMetafile);
  TImageType = ImageType;

{$IFDEF SGFULLGDIPLUS}
  InterpolationMode = (
    InterpolationModeInvalid          = Ord(QualityModeInvalid),
    InterpolationModeDefault          = Ord(QualityModeDefault),
    InterpolationModeLowQuality       = Ord(QualityModeLow),
    InterpolationModeHighQuality      = Ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic);
  TInterpolationMode = InterpolationMode;
{$ELSE}
  InterpolationMode = Integer;
  const
    InterpolationModeInvalid             = QualityModeInvalid;
    InterpolationModeDefault             = QualityModeDefault;
    InterpolationModeLowQuality          = QualityModeLow;
    InterpolationModeHighQuality         = QualityModeHigh;
    InterpolationModeBilinear            = 3;
    InterpolationModeBicubic             = 4;
    InterpolationModeNearestNeighbor     = 5;
    InterpolationModeHighQualityBilinear = 6;
    InterpolationModeHighQualityBicubic  = 7;
type
  TInterpolationMode = InterpolationMode;
{$ENDIF}

  PenAlignment = (PenAlignmentCenter,PenAlignmentInset);
  TsgGDIPPenAlignment = PenAlignment;

  BrushType = (BrushTypeSolidColor, BrushTypeHatchFill, BrushTypeTextureFill,
    BrushTypePathGradient, BrushTypeLinearGradient);
  TsgGDIPBrushType = BrushType;

{$IFDEF SGFULLGDIPLUS}
  PenType = (
    PenTypeSolidColor       =  Ord(BrushTypeSolidColor),
    PenTypeHatchFill        =  Ord(BrushTypeHatchFill),
    PenTypeTextureFill      =  Ord(BrushTypeTextureFill),
    PenTypePathGradient     =  Ord(BrushTypePathGradient),
    PenTypeLinearGradient   =  Ord(BrushTypeLinearGradient),
    PenTypeUnknown          = -1);
  TsgGDIPPenType = PenType;
{$ELSE}
  PenType = Integer;
  const
    PenTypeSolidColor       =  0;
    PenTypeHatchFill        =  1;
    PenTypeTextureFill      =  2;
    PenTypePathGradient     =  3;
    PenTypeLinearGradient   =  4;
    PenTypeUnknown          = -1;
type
  TsgGDIPPenType = PenType;
{$ENDIF}

  MatrixOrder = (MatrixOrderPrepend, MatrixOrderAppend);
  TsgGDIPMatrixOrder = MatrixOrder;

  GenericFontFamily = (GenericFontFamilySerif, GenericFontFamilySansSerif, GenericFontFamilyMonospace);
  TGenericFontFamily = GenericFontFamily;

type
  FontStyle = Integer;
  const
    FontStyleRegular    = Integer(0);
    FontStyleBold       = Integer(1);
    FontStyleItalic     = Integer(2);
    FontStyleBoldItalic = Integer(3);
    FontStyleUnderline  = Integer(4);
    FontStyleStrikeout  = Integer(8);
type
  TFontStyle = FontStyle;

{$IFDEF SGFULLGDIPLUS}
  SmoothingMode = (
    SmoothingModeInvalid     = Ord(QualityModeInvalid),
    SmoothingModeDefault     = Ord(QualityModeDefault),
    SmoothingModeHighSpeed   = Ord(QualityModeLow),
    SmoothingModeHighQuality = Ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias);
  TSmoothingMode = SmoothingMode;
{$ELSE}
  SmoothingMode = Integer;
  const
    SmoothingModeInvalid     = QualityModeInvalid;
    SmoothingModeDefault     = QualityModeDefault;
    SmoothingModeHighSpeed   = QualityModeLow;
    SmoothingModeHighQuality = QualityModeHigh;
    SmoothingModeNone        = 3;
    SmoothingModeAntiAlias   = 4;

type
  TSmoothingMode = SmoothingMode;
{$ENDIF}

{$IFDEF SGFULLGDIPLUS}
  PixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,
    PixelOffsetModeHalf);
  TPixelOffsetMode = PixelOffsetMode;
{$ELSE}
  PixelOffsetMode = Integer;
  const
    PixelOffsetModeInvalid     = QualityModeInvalid;
    PixelOffsetModeDefault     = QualityModeDefault;
    PixelOffsetModeHighSpeed   = QualityModeLow;
    PixelOffsetModeHighQuality = QualityModeHigh;
    PixelOffsetModeNone        = 3;
    PixelOffsetModeHalf        = 4;
type
  TPixelOffsetMode = PixelOffsetMode;
{$ENDIF}

  TextRenderingHint = (TextRenderingHintSystemDefault, TextRenderingHintSingleBitPerPixelGridFit,
    TextRenderingHintSingleBitPerPixel, TextRenderingHintAntiAliasGridFit,
    TextRenderingHintAntiAlias, TextRenderingHintClearTypeGridFit);
  TTextRenderingHint = TextRenderingHint;

  MetafileType = (MetafileTypeInvalid, MetafileTypeWmf, MetafileTypeWmfPlaceable,
    MetafileTypeEmf, MetafileTypeEmfPlusOnly, MetafileTypeEmfPlusDual);
  TMetafileType = MetafileType;

{$IFDEF SGFULLGDIPLUS}
  EmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual));
  TEmfType = EmfType;
{$ELSE}
  EmfType = Integer;
  const
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf);
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly);
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual);
type
  TEmfType = EmfType;
{$ENDIF}

  ObjectType = (ObjectTypeInvalid, ObjectTypeBrush, ObjectTypePen,
    ObjectTypePath, ObjectTypeRegion, ObjectTypeImage, ObjectTypeFont,
    ObjectTypeStringFormat, ObjectTypeImageAttributes, ObjectTypeCustomLineCap);
  TObjectType = ObjectType;

const
  ObjectTypeMax = ObjectTypeCustomLineCap;
  ObjectTypeMin = ObjectTypeBrush;

function ObjectTypeIsValid(type_: ObjectType): BOOL;

const
  GDIP_EMFPLUS_RECORD_BASE      = $00004000;
  GDIP_WMF_RECORD_BASE          = $00010000;

{$IFDEF SGFULLGDIPLUS}
type
  EmfPlusRecordType = (
    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetBkMode               = (META_SETBKMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapMode              = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetROP2                 = (META_SETROP2 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetRelAbs               = (META_SETRELABS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeLineTo                  = (META_LINETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeMoveTo                  = (META_MOVETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeArc                     = (META_ARC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEllipse                 = (META_ELLIPSE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFloodFill               = (META_FLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePie                     = (META_PIE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRectangle               = (META_RECTANGLE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRoundRect               = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePatBlt                  = (META_PATBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSaveDC                  = (META_SAVEDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPixel                = (META_SETPIXEL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeTextOut                 = (META_TEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeBitBlt                  = (META_BITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchBlt              = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolygon                 = (META_POLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyline                = (META_POLYLINE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEscape                  = (META_ESCAPE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRestoreDC               = (META_RESTOREDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFillRegion              = (META_FILLREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFrameRegion             = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeInvertRegion            = (META_INVERTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePaintRegion             = (META_PAINTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectObject            = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDrawText                = ($062F or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeChord                   = (META_CHORD or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchDIB              = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetLayout               = ($0149 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeResetDC                 = ($014C or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStartDoc                = ($014D or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStartPage               = ($004F or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEndPage                 = ($0050 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeAbortDoc                = ($0052 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEndDoc                  = ($005E or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrush             = ($00F8 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBitmapIndirect    = ($02FD or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBitmap            = ($06FE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateRegion            = (META_CREATEREGION or GDIP_WMF_RECORD_BASE),
    EmfRecordTypeHeader                  = EMR_HEADER,
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,
    EmfRecordTypePolygon                 = EMR_POLYGON,
    EmfRecordTypePolyline                = EMR_POLYLINE,
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,
    EmfRecordTypeEOF                     = EMR_EOF,
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,
    EmfRecordTypeSetROP2                 = EMR_SETROP2,
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,
    EmfRecordTypeSaveDC                  = EMR_SAVEDC,
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,
    EmfRecordTypeCreatePen               = EMR_CREATEPEN,
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,
    EmfRecordTypeAngleArc                = EMR_ANGLEARC,
    EmfRecordTypeEllipse                 = EMR_ELLIPSE,
    EmfRecordTypeRectangle               = EMR_RECTANGLE,
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,
    EmfRecordTypeArc                     = EMR_ARC,
    EmfRecordTypeChord                   = EMR_CHORD,
    EmfRecordTypePie                     = EMR_PIE,
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,
    EmfRecordTypeLineTo                  = EMR_LINETO,
    EmfRecordTypeArcTo                   = EMR_ARCTO,
    EmfRecordTypePolyDraw                = EMR_POLYDRAW,
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,
    EmfRecordTypeBeginPath               = EMR_BEGINPATH,
    EmfRecordTypeEndPath                 = EMR_ENDPATH,
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,
    EmfRecordTypeFillPath                = EMR_FILLPATH,
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,
    EmfRecordTypeStrokePath              = EMR_STROKEPATH,
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,
    EmfRecordTypeWidenPath               = EMR_WIDENPATH,
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,
    EmfRecordTypeAbortPath               = EMR_ABORTPATH,
    EmfRecordTypeReserved_069            = 69,
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,
    EmfRecordTypeFillRgn                 = EMR_FILLRGN,
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,
    EmfRecordTypePaintRgn                = EMR_PAINTRGN,
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,
    EmfRecordTypeBitBlt                  = EMR_BITBLT,
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,
    EmfRecordTypePlgBlt                  = EMR_PLGBLT,
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,
    EmfRecordTypePolygon16               = EMR_POLYGON16,
    EmfRecordTypePolyline16              = EMR_POLYLINE16,
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,
    EmfRecordTypeSetICMMode              = 98,
    EmfRecordTypeCreateColorSpace        = 99,
    EmfRecordTypeSetColorSpace           = 100,
    EmfRecordTypeDeleteColorSpace        = 101,
    EmfRecordTypeGLSRecord               = 102,
    EmfRecordTypeGLSBoundedRecord        = 103,
    EmfRecordTypePixelFormat             = 104,
    EmfRecordTypeDrawEscape              = 105,
    EmfRecordTypeExtEscape               = 106,
    EmfRecordTypeStartDoc                = 107,
    EmfRecordTypeSmallTextOut            = 108,
    EmfRecordTypeForceUFIMapping         = 109,
    EmfRecordTypeNamedEscape             = 110,
    EmfRecordTypeColorCorrectPalette     = 111,
    EmfRecordTypeSetICMProfileA          = 112,
    EmfRecordTypeSetICMProfileW          = 113,
    EmfRecordTypeAlphaBlend              = 114,
    EmfRecordTypeSetLayout               = 115,
    EmfRecordTypeTransparentBlt          = 116,
    EmfRecordTypeReserved_117            = 117,
    EmfRecordTypeGradientFill            = 118,
    EmfRecordTypeSetLinkedUFIs           = 119,
    EmfRecordTypeSetTextJustification    = 120,
    EmfRecordTypeColorMatchToTargetW     = 121,
    EmfRecordTypeCreateColorSpaceW       = 122,
    EmfRecordTypeMax                     = 122,
    EmfRecordTypeMin                     = 1,
    EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE,
    EmfPlusRecordTypeHeader,
    EmfPlusRecordTypeEndOfFile,
    EmfPlusRecordTypeComment,
    EmfPlusRecordTypeGetDC,
    EmfPlusRecordTypeMultiFormatStart,
    EmfPlusRecordTypeMultiFormatSection,
    EmfPlusRecordTypeMultiFormatEnd,
    EmfPlusRecordTypeObject,
    EmfPlusRecordTypeClear,
    EmfPlusRecordTypeFillRects,
    EmfPlusRecordTypeDrawRects,
    EmfPlusRecordTypeFillPolygon,
    EmfPlusRecordTypeDrawLines,
    EmfPlusRecordTypeFillEllipse,
    EmfPlusRecordTypeDrawEllipse,
    EmfPlusRecordTypeFillPie,
    EmfPlusRecordTypeDrawPie,
    EmfPlusRecordTypeDrawArc,
    EmfPlusRecordTypeFillRegion,
    EmfPlusRecordTypeFillPath,
    EmfPlusRecordTypeDrawPath,
    EmfPlusRecordTypeFillClosedCurve,
    EmfPlusRecordTypeDrawClosedCurve,
    EmfPlusRecordTypeDrawCurve,
    EmfPlusRecordTypeDrawBeziers,
    EmfPlusRecordTypeDrawImage,
    EmfPlusRecordTypeDrawImagePoints,
    EmfPlusRecordTypeDrawString,
    EmfPlusRecordTypeSetRenderingOrigin,
    EmfPlusRecordTypeSetAntiAliasMode,
    EmfPlusRecordTypeSetTextRenderingHint,
    EmfPlusRecordTypeSetTextContrast,
    EmfPlusRecordTypeSetInterpolationMode,
    EmfPlusRecordTypeSetPixelOffsetMode,
    EmfPlusRecordTypeSetCompositingMode,
    EmfPlusRecordTypeSetCompositingQuality,
    EmfPlusRecordTypeSave,
    EmfPlusRecordTypeRestore,
    EmfPlusRecordTypeBeginContainer,
    EmfPlusRecordTypeBeginContainerNoParams,
    EmfPlusRecordTypeEndContainer,
    EmfPlusRecordTypeSetWorldTransform,
    EmfPlusRecordTypeResetWorldTransform,
    EmfPlusRecordTypeMultiplyWorldTransform,
    EmfPlusRecordTypeTranslateWorldTransform,
    EmfPlusRecordTypeScaleWorldTransform,
    EmfPlusRecordTypeRotateWorldTransform,
    EmfPlusRecordTypeSetPageTransform,
    EmfPlusRecordTypeResetClip,
    EmfPlusRecordTypeSetClipRect,
    EmfPlusRecordTypeSetClipPath,
    EmfPlusRecordTypeSetClipRegion,
    EmfPlusRecordTypeOffsetClip,
    EmfPlusRecordTypeDrawDriverString,
    EmfPlusRecordTotal,
    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1,
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader);
  TEmfPlusRecordType = EmfPlusRecordType;
{$ELSE}
type
  EmfPlusRecordType = Integer;
  const
    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetBkMode               = (META_SETBKMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetMapMode              = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetROP2                 = (META_SETROP2 or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetRelAbs               = (META_SETRELABS or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeLineTo                  = (META_LINETO or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeMoveTo                  = (META_MOVETO or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeArc                     = (META_ARC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEllipse                 = (META_ELLIPSE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFloodFill               = (META_FLOODFILL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePie                     = (META_PIE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRectangle               = (META_RECTANGLE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRoundRect               = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePatBlt                  = (META_PATBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSaveDC                  = (META_SAVEDC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPixel                = (META_SETPIXEL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeTextOut                 = (META_TEXTOUT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeBitBlt                  = (META_BITBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStretchBlt              = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolygon                 = (META_POLYGON or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolyline                = (META_POLYLINE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEscape                  = (META_ESCAPE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRestoreDC               = (META_RESTOREDC or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFillRegion              = (META_FILLREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeFrameRegion             = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeInvertRegion            = (META_INVERTREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePaintRegion             = (META_PAINTREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectObject            = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDrawText                = ($062F or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeChord                   = (META_CHORD or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE);
    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStretchDIB              = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeSetLayout               = ($0149 or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeResetDC                 = ($014C or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStartDoc                = ($014D or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeStartPage               = ($004F or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEndPage                 = ($0050 or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeAbortDoc                = ($0052 or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeEndDoc                  = ($005E or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBrush             = ($00F8 or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBitmapIndirect    = ($02FD or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateBitmap            = ($06FE or GDIP_WMF_RECORD_BASE);
    WmfRecordTypeCreateRegion            = (META_CREATEREGION or GDIP_WMF_RECORD_BASE);
    EmfRecordTypeHeader                  = EMR_HEADER;
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER;
    EmfRecordTypePolygon                 = EMR_POLYGON;
    EmfRecordTypePolyline                = EMR_POLYLINE;
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO;
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO;
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE;
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON;
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX;
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX;
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX;
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX;
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX;
    EmfRecordTypeEOF                     = EMR_EOF;
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV;
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS;
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE;
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE;
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE;
    EmfRecordTypeSetROP2                 = EMR_SETROP2;
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE;
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN;
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT;
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR;
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR;
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN;
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX;
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN;
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT;
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT;
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX;
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX;
    EmfRecordTypeSaveDC                  = EMR_SAVEDC;
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC;
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM;
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM;
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT;
    EmfRecordTypeCreatePen               = EMR_CREATEPEN;
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT;
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT;
    EmfRecordTypeAngleArc                = EMR_ANGLEARC;
    EmfRecordTypeEllipse                 = EMR_ELLIPSE;
    EmfRecordTypeRectangle               = EMR_RECTANGLE;
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT;
    EmfRecordTypeArc                     = EMR_ARC;
    EmfRecordTypeChord                   = EMR_CHORD;
    EmfRecordTypePie                     = EMR_PIE;
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE;
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE;
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES;
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE;
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE;
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL;
    EmfRecordTypeLineTo                  = EMR_LINETO;
    EmfRecordTypeArcTo                   = EMR_ARCTO;
    EmfRecordTypePolyDraw                = EMR_POLYDRAW;
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION;
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT;
    EmfRecordTypeBeginPath               = EMR_BEGINPATH;
    EmfRecordTypeEndPath                 = EMR_ENDPATH;
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE;
    EmfRecordTypeFillPath                = EMR_FILLPATH;
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH;
    EmfRecordTypeStrokePath              = EMR_STROKEPATH;
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH;
    EmfRecordTypeWidenPath               = EMR_WIDENPATH;
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH;
    EmfRecordTypeAbortPath               = EMR_ABORTPATH;
    EmfRecordTypeReserved_069            = 69;
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT;
    EmfRecordTypeFillRgn                 = EMR_FILLRGN;
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN;
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN;
    EmfRecordTypePaintRgn                = EMR_PAINTRGN;
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN;
    EmfRecordTypeBitBlt                  = EMR_BITBLT;
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT;
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT;
    EmfRecordTypePlgBlt                  = EMR_PLGBLT;
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE;
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS;
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW;
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA;
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW;
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16;
    EmfRecordTypePolygon16               = EMR_POLYGON16;
    EmfRecordTypePolyline16              = EMR_POLYLINE16;
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16;
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16;
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16;
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16;
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16;
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH;
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT;
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN;
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA;
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW;
    EmfRecordTypeSetICMMode              = 98;
    EmfRecordTypeCreateColorSpace        = 99;
    EmfRecordTypeSetColorSpace           = 100;
    EmfRecordTypeDeleteColorSpace        = 101;
    EmfRecordTypeGLSRecord               = 102;
    EmfRecordTypeGLSBoundedRecord        = 103;
    EmfRecordTypePixelFormat             = 104;
    EmfRecordTypeDrawEscape              = 105;
    EmfRecordTypeExtEscape               = 106;
    EmfRecordTypeStartDoc                = 107;
    EmfRecordTypeSmallTextOut            = 108;
    EmfRecordTypeForceUFIMapping         = 109;
    EmfRecordTypeNamedEscape             = 110;
    EmfRecordTypeColorCorrectPalette     = 111;
    EmfRecordTypeSetICMProfileA          = 112;
    EmfRecordTypeSetICMProfileW          = 113;
    EmfRecordTypeAlphaBlend              = 114;
    EmfRecordTypeSetLayout               = 115;
    EmfRecordTypeTransparentBlt          = 116;
    EmfRecordTypeReserved_117            = 117;
    EmfRecordTypeGradientFill            = 118;
    EmfRecordTypeSetLinkedUFIs           = 119;
    EmfRecordTypeSetTextJustification    = 120;
    EmfRecordTypeColorMatchToTargetW     = 121;
    EmfRecordTypeCreateColorSpaceW       = 122;
    EmfRecordTypeMax                     = 122;
    EmfRecordTypeMin                     = 1;
    EmfPlusRecordTypeInvalid   = GDIP_EMFPLUS_RECORD_BASE;
    EmfPlusRecordTypeHeader    = GDIP_EMFPLUS_RECORD_BASE + 1;
    EmfPlusRecordTypeEndOfFile = GDIP_EMFPLUS_RECORD_BASE + 2;
    EmfPlusRecordTypeComment   = GDIP_EMFPLUS_RECORD_BASE + 3;
    EmfPlusRecordTypeGetDC     = GDIP_EMFPLUS_RECORD_BASE + 4;
    EmfPlusRecordTypeMultiFormatStart   = GDIP_EMFPLUS_RECORD_BASE + 5;
    EmfPlusRecordTypeMultiFormatSection = GDIP_EMFPLUS_RECORD_BASE + 6;
    EmfPlusRecordTypeMultiFormatEnd     = GDIP_EMFPLUS_RECORD_BASE + 7;
    EmfPlusRecordTypeObject = GDIP_EMFPLUS_RECORD_BASE + 8;
    EmfPlusRecordTypeClear           = GDIP_EMFPLUS_RECORD_BASE + 9;
    EmfPlusRecordTypeFillRects       = GDIP_EMFPLUS_RECORD_BASE + 10;
    EmfPlusRecordTypeDrawRects       = GDIP_EMFPLUS_RECORD_BASE + 11;
    EmfPlusRecordTypeFillPolygon     = GDIP_EMFPLUS_RECORD_BASE + 12;
    EmfPlusRecordTypeDrawLines       = GDIP_EMFPLUS_RECORD_BASE + 13;
    EmfPlusRecordTypeFillEllipse     = GDIP_EMFPLUS_RECORD_BASE + 14;
    EmfPlusRecordTypeDrawEllipse     = GDIP_EMFPLUS_RECORD_BASE + 15;
    EmfPlusRecordTypeFillPie         = GDIP_EMFPLUS_RECORD_BASE + 16;
    EmfPlusRecordTypeDrawPie         = GDIP_EMFPLUS_RECORD_BASE + 17;
    EmfPlusRecordTypeDrawArc         = GDIP_EMFPLUS_RECORD_BASE + 18;
    EmfPlusRecordTypeFillRegion      = GDIP_EMFPLUS_RECORD_BASE + 19;
    EmfPlusRecordTypeFillPath        = GDIP_EMFPLUS_RECORD_BASE + 20;
    EmfPlusRecordTypeDrawPath        = GDIP_EMFPLUS_RECORD_BASE + 21;
    EmfPlusRecordTypeFillClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 22;
    EmfPlusRecordTypeDrawClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 23;
    EmfPlusRecordTypeDrawCurve       = GDIP_EMFPLUS_RECORD_BASE + 24;
    EmfPlusRecordTypeDrawBeziers     = GDIP_EMFPLUS_RECORD_BASE + 25;
    EmfPlusRecordTypeDrawImage       = GDIP_EMFPLUS_RECORD_BASE + 26;
    EmfPlusRecordTypeDrawImagePoints = GDIP_EMFPLUS_RECORD_BASE + 27;
    EmfPlusRecordTypeDrawString      = GDIP_EMFPLUS_RECORD_BASE + 28;
    EmfPlusRecordTypeSetRenderingOrigin      = GDIP_EMFPLUS_RECORD_BASE + 29;
    EmfPlusRecordTypeSetAntiAliasMode        = GDIP_EMFPLUS_RECORD_BASE + 30;
    EmfPlusRecordTypeSetTextRenderingHint    = GDIP_EMFPLUS_RECORD_BASE + 31;
    EmfPlusRecordTypeSetTextContrast         = GDIP_EMFPLUS_RECORD_BASE + 32;
    EmfPlusRecordTypeSetInterpolationMode    = GDIP_EMFPLUS_RECORD_BASE + 33;
    EmfPlusRecordTypeSetPixelOffsetMode      = GDIP_EMFPLUS_RECORD_BASE + 34;
    EmfPlusRecordTypeSetCompositingMode      = GDIP_EMFPLUS_RECORD_BASE + 35;
    EmfPlusRecordTypeSetCompositingQuality   = GDIP_EMFPLUS_RECORD_BASE + 36;
    EmfPlusRecordTypeSave                    = GDIP_EMFPLUS_RECORD_BASE + 37;
    EmfPlusRecordTypeRestore                 = GDIP_EMFPLUS_RECORD_BASE + 38;
    EmfPlusRecordTypeBeginContainer          = GDIP_EMFPLUS_RECORD_BASE + 39;
    EmfPlusRecordTypeBeginContainerNoParams  = GDIP_EMFPLUS_RECORD_BASE + 40;
    EmfPlusRecordTypeEndContainer            = GDIP_EMFPLUS_RECORD_BASE + 41;
    EmfPlusRecordTypeSetWorldTransform       = GDIP_EMFPLUS_RECORD_BASE + 42;
    EmfPlusRecordTypeResetWorldTransform     = GDIP_EMFPLUS_RECORD_BASE + 43;
    EmfPlusRecordTypeMultiplyWorldTransform  = GDIP_EMFPLUS_RECORD_BASE + 44;
    EmfPlusRecordTypeTranslateWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 45;
    EmfPlusRecordTypeScaleWorldTransform     = GDIP_EMFPLUS_RECORD_BASE + 46;
    EmfPlusRecordTypeRotateWorldTransform    = GDIP_EMFPLUS_RECORD_BASE + 47;
    EmfPlusRecordTypeSetPageTransform        = GDIP_EMFPLUS_RECORD_BASE + 48;
    EmfPlusRecordTypeResetClip               = GDIP_EMFPLUS_RECORD_BASE + 49;
    EmfPlusRecordTypeSetClipRect             = GDIP_EMFPLUS_RECORD_BASE + 50;
    EmfPlusRecordTypeSetClipPath             = GDIP_EMFPLUS_RECORD_BASE + 51;
    EmfPlusRecordTypeSetClipRegion           = GDIP_EMFPLUS_RECORD_BASE + 52;
    EmfPlusRecordTypeOffsetClip              = GDIP_EMFPLUS_RECORD_BASE + 53;
    EmfPlusRecordTypeDrawDriverString        = GDIP_EMFPLUS_RECORD_BASE + 54;
    EmfPlusRecordTotal                       = GDIP_EMFPLUS_RECORD_BASE + 55;
    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1;
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader;

type
  TEmfPlusRecordType = EmfPlusRecordType;
{$ENDIF}
  StringFormatFlags = Integer;
  const
    StringFormatFlagsDirectionRightToLeft        = $00000001;
    StringFormatFlagsDirectionVertical           = $00000002;
    StringFormatFlagsNoFitBlackBox               = $00000004;
    StringFormatFlagsDisplayFormatControl        = $00000020;
    StringFormatFlagsNoFontFallback              = $00000400;
    StringFormatFlagsMeasureTrailingSpaces       = $00000800;
    StringFormatFlagsNoWrap                      = $00001000;
    StringFormatFlagsLineLimit                   = $00002000;
    StringFormatFlagsNoClip                      = $00004000;

type
  TStringFormatFlags = StringFormatFlags;
  StringTrimming = (StringTrimmingNone, StringTrimmingCharacter, StringTrimmingWord,
    StringTrimmingEllipsisCharacter, StringTrimmingEllipsisWord, StringTrimmingEllipsisPath);
  TStringTrimming = StringTrimming;

  StringDigitSubstitute = (StringDigitSubstituteUser, StringDigitSubstituteNone,
    StringDigitSubstituteNational, StringDigitSubstituteTraditional);
  TStringDigitSubstitute = StringDigitSubstitute;
  PStringDigitSubstitute = ^TStringDigitSubstitute;

  HotkeyPrefix = (HotkeyPrefixNone, HotkeyPrefixShow, HotkeyPrefixHide);
  THotkeyPrefix = HotkeyPrefix;

  StringAlignment = (StringAlignmentNear, StringAlignmentCenter, StringAlignmentFar);
  TStringAlignment = StringAlignment;

  DriverStringOptions = Integer;
  const
    DriverStringOptionsCmapLookup             = 1;
    DriverStringOptionsVertical               = 2;
    DriverStringOptionsRealizedAdvance        = 4;
    DriverStringOptionsLimitSubpixel          = 8;

type
  TDriverStringOptions = DriverStringOptions;

  FlushIntention = (FlushIntentionFlush, FlushIntentionSync);
  TsgGDIPFlushIntention = FlushIntention;

  EncoderParameterValueType = Integer;
  const
    EncoderParameterValueTypeByte         : Integer = 1;
    EncoderParameterValueTypeASCII        : Integer = 2;
    EncoderParameterValueTypeShort        : Integer = 3;
    EncoderParameterValueTypeLong         : Integer = 4;
    EncoderParameterValueTypeRational     : Integer = 5;
    EncoderParameterValueTypeLongRange    : Integer = 6;
    EncoderParameterValueTypeUndefined    : Integer = 7;
    EncoderParameterValueTypeRationalRange: Integer = 8;
type
  TEncoderParameterValueType = EncoderParameterValueType;

  EncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage);
  TEncoderValue = EncoderValue;

{$IFDEF SGFULLGDIPLUS}
  EmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault          = $00000000,
    EmfToWmfBitsFlagsEmbedEmf         = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip        = $00000004);
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ELSE}
  EmfToWmfBitsFlags = Integer;
  const
    EmfToWmfBitsFlagsDefault          = $00000000;
    EmfToWmfBitsFlagsEmbedEmf         = $00000001;
    EmfToWmfBitsFlagsIncludePlaceable = $00000002;
    EmfToWmfBitsFlagsNoXORClip        = $00000004;

type
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ENDIF}

  EnumerateMetafileProc = function(RecordType: EmfPlusRecordType; Flags: UINT;
    DataSize: UINT; Data: PBYTE; CallBackData: Pointer): BOOL; stdcall;

const
{$IFDEF BCB}
{$HPPEMIT '#define FLT_MAX 3.402823E+38'}
{$HPPEMIT '#define FLT_MIN 1.175494E-38'}
{$HPPEMIT '#define REAL_MAX 3.402823E+38'}
{$HPPEMIT '#define REAL_MIN 1.175494E-38'}
{$HPPEMIT '#define REAL_TOLERANCE (FLT_MIN * 100)'}
{$ELSE}
  FLT_MAX =  3.402823466e+38;
  FLT_MIN =  1.175494351e-38;
  REAL_MAX           = FLT_MAX;
  REAL_MIN           = FLT_MIN;
  REAL_TOLERANCE     = (FLT_MIN * 100);
{$ENDIF}
  REAL_EPSILON       = 1.192092896e-07;

type
  PsgGSizeF = ^TsgGSizeF;
  TsgGSizeF = packed record
    Width, Height: Single;
  end;

function MakeSizeF(Width, Height: Single): TsgGSizeF; overload;

type
  PsgGSize = ^TsgGSize;
  TsgGSize = packed record
    Width, Height: Integer;
  end;

function MakeSize(Width, Height: Integer): TsgGSize; overload;

type
  PsgGPointF = ^TsgGPointF;
  TsgGPointF = packed record
    X, Y: Single;
  end;
  TPointFDynArray = array of TsgGPointF;

function MakePointF(X, Y: Single): TsgGPointF; overload;

type
  PsgGPoint = ^TsgGPoint;
  TsgGPoint = packed record
    X, Y: Integer;
  end;
  TPointDynArray = array of TsgGPoint;

function MakePoint(X, Y: Integer): TsgGPoint; overload;
function PointFToPointI(const APoint: TsgGPointF): TsgGPoint;
function PointIToPointF(const APoint: TsgGPoint): TsgGPointF;

type
  PsgGRectF = ^TsgGRectF;
  TsgGRectF = packed record
    X, Y, Width, Height: Single;
  end;
  TRectFDynArray = array of TsgGRectF;

function MakeRectF(const X, Y, Width, Height: Single): TsgGRectF; overload;
function MakeRectF(Location: TsgGPointF; size: TsgGSizeF): TsgGRectF; overload;
function GetCenterF(const ARect: TsgGRectF): TsgGPointF; overload;
function GetTopLeftF(const ARect: TsgGRectF): TsgGPointF; overload;
function GetBottomRightF(const ARect: TsgGRectF): TsgGPointF; overload;

type
  PsgGRect = ^TsgGRect;
  TsgGRect = packed record
    X, Y, Width, Height: Integer;
  end;
  TRectDynArray = array of TsgGRect;

function MakeRect(X, Y, Width, Height: Integer): TsgGRect; overload;
function MakeRect(Location: TsgGPoint; size: TsgGSize): TsgGRect; overload;
function MakeRectI(const ARect: TRect): TsgGRect;
function GetCenter(const ARect: TRect): TsgGPoint; overload;
function GetCenter(const ARect: TsgGRect): TsgGPoint; overload;
function GetTopLeft(const ARect: TsgGRect): TsgGPoint; overload;
function GetBottomRight(const ARect: TsgGRect): TsgGPoint; overload;

type
  TsgGDIPPathData = packed class
  public
    Count: Integer;
    Points: PsgGPointF;
    Types: PBYTE;
    constructor Create;
    destructor Destroy; override;
  end;

  PCharacterRange = ^TCharacterRange;
  TCharacterRange = packed record
    First: Integer;
    Length: Integer;
  end;

function MakeCharacterRange(First, Length: Integer): TCharacterRange;

type
  DebugEventLevel = (DebugEventLevelFatal, DebugEventLevelWarning);
  TDebugEventLevel = DebugEventLevel;
  DebugEventProc = procedure(Level: DebugEventLevel; message: PChar); stdcall;
  NotificationHookProc = function(out Token: ULONG): Status; stdcall;
  NotificationUnhookProc = procedure(Token: ULONG); stdcall;

  GDIPlusStartupInput = packed record
    GDIPlusVersion          : Cardinal;
    DebugEventCallback      : DebugEventProc;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs  : BOOL;
  end;
  TGDIPlusStartupInput = GDIPlusStartupInput;
  PGDIPlusStartupInput = ^TGDIPlusStartupInput;

  GDIPlusStartupOutput = packed record
    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGDIPlusStartupOutput = GDIPlusStartupOutput;
  PGDIPlusStartupOutput = ^TGDIPlusStartupOutput;

  TsgGDIPlusStartup = function (out Token: ULONG; Input: PGDIPlusStartupInput; Output: PGDIPlusStartupOutput): Status; stdcall;
  TsgGDIPlusShutdown =  procedure (Token: ULONG); stdcall;

type
  PARGB  = ^ARGB;
  ARGB   = DWORD;
  ARGB64 = Int64;

const
  ALPHA_SHIFT = 24;
  RED_SHIFT   = 16;
  GREEN_SHIFT = 8;
  BLUE_SHIFT  = 0;
  ALPHA_MASK  = (ARGB($ff) shl ALPHA_SHIFT);

type
  PixelFormat = Integer;
  TPixelFormat = PixelFormat;

const
  PixelFormatIndexed     = $00010000;
  PixelFormatGDI         = $00020000;
  PixelFormatAlpha       = $00040000;
  PixelFormatPAlpha      = $00080000;
  PixelFormatExtended    = $00100000;
  PixelFormatCanonical   = $00200000;
  PixelFormatUndefined      = 0;
  PixelFormatDontCare       = 0;
  PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat16bppGrayScale = (4  or (16 shl 8) or PixelFormatExtended);
  PixelFormat16bppRGB555    = (5  or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppRGB565    = (6  or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  PixelFormat24bppRGB       = (8  or (24 shl 8) or PixelFormatGDI);
  PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha  or PixelFormatCanonical or PixelFormatExtended);
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha  or PixelFormatPAlpha or PixelFormatExtended);
  PixelFormatMax            = 15;

function GetPixelFormatSize(APixFmt: PixelFormat): UINT;
function GetPixelFormat(AFormat: Graphics.TPixelFormat): TPixelFormat;
function IsIndexedPixelFormat(APixFmt: PixelFormat): BOOL;
function IsAlphaPixelFormat(APixFmt: PixelFormat): BOOL;
function IsExtendedPixelFormat(APixFmt: PixelFormat): BOOL;

function IsCanonicalPixelFormat(APixFmt: PixelFormat): BOOL;

{$IFDEF SGFULLGDIPLUS}
type
  PaletteFlags = (
    PaletteFlagsHasAlpha    = $0001,
    PaletteFlagsGrayScale   = $0002,
    PaletteFlagsHalftone    = $0004
  );
  TPaletteFlags = PaletteFlags;
{$ELSE}
type
  PaletteFlags = Integer;
  const
    PaletteFlagsHasAlpha    = $0001;
    PaletteFlagsGrayScale   = $0002;
    PaletteFlagsHalftone    = $0004;

type
  TPaletteFlags = PaletteFlags;
{$ENDIF}

  ColorPalette = packed record
    Flags: UINT ;
    Count: UINT ;
    Entries: array [0..0] of ARGB ;
  end;

  TColorPalette = ColorPalette;
  PColorPalette = ^TColorPalette;

  ColorMode = (ColorModeARGB32, ColorModeARGB64);
  TColorMode = ColorMode;

  ColorChannelFlags = (ColorChannelFlagsC, ColorChannelFlagsM, ColorChannelFlagsY,
    ColorChannelFlagsK, ColorChannelFlagsLast);
  TColorChannelFlags = ColorChannelFlags;


const
  aclAliceBlue            = $FFF0F8FF;
  aclAntiqueWhite         = $FFFAEBD7;
  aclAqua                 = $FF00FFFF;
  aclAquamarine           = $FF7FFFD4;
  aclAzure                = $FFF0FFFF;
  aclBeige                = $FFF5F5DC;
  aclBisque               = $FFFFE4C4;
  aclBlack                = $FF000000;
  aclBlanchedAlmond       = $FFFFEBCD;
  aclBlue                 = $FF0000FF;
  aclBlueViolet           = $FF8A2BE2;
  aclBrown                = $FFA52A2A;
  aclBurlyWood            = $FFDEB887;
  aclCadetBlue            = $FF5F9EA0;
  aclChartreuse           = $FF7FFF00;
  aclChocolate            = $FFD2691E;
  aclCoral                = $FFFF7F50;
  aclCornflowerBlue       = $FF6495ED;
  aclCornsilk             = $FFFFF8DC;
  aclCrimson              = $FFDC143C;
  aclCyan                 = $FF00FFFF;
  aclDarkBlue             = $FF00008B;
  aclDarkCyan             = $FF008B8B;
  aclDarkGoldenrod        = $FFB8860B;
  aclDarkGray             = $FFA9A9A9;
  aclDarkGreen            = $FF006400;
  aclDarkKhaki            = $FFBDB76B;
  aclDarkMagenta          = $FF8B008B;
  aclDarkOliveGreen       = $FF556B2F;
  aclDarkOrange           = $FFFF8C00;
  aclDarkOrchid           = $FF9932CC;
  aclDarkRed              = $FF8B0000;
  aclDarkSalmon           = $FFE9967A;
  aclDarkSeaGreen         = $FF8FBC8B;
  aclDarkSlateBlue        = $FF483D8B;
  aclDarkSlateGray        = $FF2F4F4F;
  aclDarkTurquoise        = $FF00CED1;
  aclDarkViolet           = $FF9400D3;
  aclDeepPink             = $FFFF1493;
  aclDeepSkyBlue          = $FF00BFFF;
  aclDimGray              = $FF696969;
  aclDodgerBlue           = $FF1E90FF;
  aclFirebrick            = $FFB22222;
  aclFloralWhite          = $FFFFFAF0;
  aclForestGreen          = $FF228B22;
  aclFuchsia              = $FFFF00FF;
  aclGainsboro            = $FFDCDCDC;
  aclGhostWhite           = $FFF8F8FF;
  aclGold                 = $FFFFD700;
  aclGoldenrod            = $FFDAA520;
  aclGray                 = $FF808080;
  aclGreen                = $FF008000;
  aclGreenYellow          = $FFADFF2F;
  aclHoneydew             = $FFF0FFF0;
  aclHotPink              = $FFFF69B4;
  aclIndianRed            = $FFCD5C5C;
  aclIndigo               = $FF4B0082;
  aclIvory                = $FFFFFFF0;
  aclKhaki                = $FFF0E68C;
  aclLavender             = $FFE6E6FA;
  aclLavenderBlush        = $FFFFF0F5;
  aclLawnGreen            = $FF7CFC00;
  aclLemonChiffon         = $FFFFFACD;
  aclLightBlue            = $FFADD8E6;
  aclLightCoral           = $FFF08080;
  aclLightCyan            = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray            = $FFD3D3D3;
  aclLightGreen           = $FF90EE90;
  aclLightPink            = $FFFFB6C1;
  aclLightSalmon          = $FFFFA07A;
  aclLightSeaGreen        = $FF20B2AA;
  aclLightSkyBlue         = $FF87CEFA;
  aclLightSlateGray       = $FF778899;
  aclLightSteelBlue       = $FFB0C4DE;
  aclLightYellow          = $FFFFFFE0;
  aclLime                 = $FF00FF00;
  aclLimeGreen            = $FF32CD32;
  aclLinen                = $FFFAF0E6;
  aclMagenta              = $FFFF00FF;
  aclMaroon               = $FF800000;
  aclMediumAquamarine     = $FF66CDAA;
  aclMediumBlue           = $FF0000CD;
  aclMediumOrchid         = $FFBA55D3;
  aclMediumPurple         = $FF9370DB;
  aclMediumSeaGreen       = $FF3CB371;
  aclMediumSlateBlue      = $FF7B68EE;
  aclMediumSpringGreen    = $FF00FA9A;
  aclMediumTurquoise      = $FF48D1CC;
  aclMediumVioletRed      = $FFC71585;
  aclMidnightBlue         = $FF191970;
  aclMintCream            = $FFF5FFFA;
  aclMistyRose            = $FFFFE4E1;
  aclMoccasin             = $FFFFE4B5;
  aclNavajoWhite          = $FFFFDEAD;
  aclNavy                 = $FF000080;
  aclOldLace              = $FFFDF5E6;
  aclOlive                = $FF808000;
  aclOliveDrab            = $FF6B8E23;
  aclOrange               = $FFFFA500;
  aclOrangeRed            = $FFFF4500;
  aclOrchid               = $FFDA70D6;
  aclPaleGoldenrod        = $FFEEE8AA;
  aclPaleGreen            = $FF98FB98;
  aclPaleTurquoise        = $FFAFEEEE;
  aclPaleVioletRed        = $FFDB7093;
  aclPapayaWhip           = $FFFFEFD5;
  aclPeachPuff            = $FFFFDAB9;
  aclPeru                 = $FFCD853F;
  aclPink                 = $FFFFC0CB;
  aclPlum                 = $FFDDA0DD;
  aclPowderBlue           = $FFB0E0E6;
  aclPurple               = $FF800080;
  aclRed                  = $FFFF0000;
  aclRosyBrown            = $FFBC8F8F;
  aclRoyalBlue            = $FF4169E1;
  aclSaddleBrown          = $FF8B4513;
  aclSalmon               = $FFFA8072;
  aclSandyBrown           = $FFF4A460;
  aclSeaGreen             = $FF2E8B57;
  aclSeaShell             = $FFFFF5EE;
  aclSienna               = $FFA0522D;
  aclSilver               = $FFC0C0C0;
  aclSkyBlue              = $FF87CEEB;
  aclSlateBlue            = $FF6A5ACD;
  aclSlateGray            = $FF708090;
  aclSnow                 = $FFFFFAFA;
  aclSpringGreen          = $FF00FF7F;
  aclSteelBlue            = $FF4682B4;
  aclTan                  = $FFD2B48C;
  aclTeal                 = $FF008080;
  aclThistle              = $FFD8BFD8;
  aclTomato               = $FFFF6347;
  aclTransparent          = $00FFFFFF;
  aclTurquoise            = $FF40E0D0;
  aclViolet               = $FFEE82EE;
  aclWheat                = $FFF5DEB3;
  aclWhite                = $FFFFFFFF;
  aclWhiteSmoke           = $FFF5F5F5;
  aclYellow               = $FFFFFF00;
  aclYellowGreen          = $FF9ACD32;

  AlphaShift  = 24;
  RedShift    = 16;
  GreenShift  = 8;
  BlueShift   = 0;

  AlphaMask   = $ff000000;
  RedMask     = $00ff0000;
  GreenMask   = $0000ff00;
  BlueMask    = $000000ff;

type
  PsgColor = ^TsgColor;
  TsgColor = ARGB;
  TColorDynArray = array of TsgColor;

function MakeColor(R, G, B: Byte): ARGB; overload;
function MakeColor(A, R, G, B: Byte): ARGB; overload;
function GetAlpha(color: ARGB): BYTE;
function GetRed(color: ARGB): BYTE;
function GetGreen(color: ARGB): BYTE;
function GetBlue(color: ARGB): BYTE;
function ColorRefToARGB(RGB: COLORREF): ARGB;
function ARGBToColorRef(Color: ARGB): COLORREF;

type
  RECTL = Windows.TRect;
  SIZEL = Windows.TSize;

  ENHMETAHEADER3 = packed record
    iType         : DWORD;
    nSize         : DWORD;
    rclBounds     : RECTL;
    rclFrame      : RECTL;
    dSignature    : DWORD;
    nVersion      : DWORD;
    nBytes        : DWORD;
    nRecords      : DWORD;
    nHandles      : WORD;
    sReserved     : WORD;
    nDescription  : DWORD;
    offDescription: DWORD;
    nPalEntries   : DWORD;
    szlDevice     : SIZEL;
    szlMillimeters: SIZEL;
  end;
  {$EXTERNALSYM ENHMETAHEADER3}
  TENHMETAHEADER3 = ENHMETAHEADER3;
  {$EXTERNALSYM TENHMETAHEADER3}
  PENHMETAHEADER3 = ^TENHMETAHEADER3;
  {$EXTERNALSYM PENHMETAHEADER3}

  PWMFRect16 = packed record
    Left, Top, Right, Bottom: INT16;
  end;
  TPWMFRect16 = PWMFRect16;
  PPWMFRect16 = ^TPWMFRect16;

  WmfPlaceableFileHeader = packed record
    Key        : UINT32;
    Hmf        : INT16;
    BoundingBox: PWMFRect16;
    Inch       : INT16;
    Reserved   : UINT32;
    Checksum   : INT16;
  end;
  TWmfPlaceableFileHeader = WmfPlaceableFileHeader;
  PWmfPlaceableFileHeader = ^TWmfPlaceableFileHeader;

const
  GDIP_EMFPLUSFLAGS_DISPLAY      = $00000001;

type
  PMetafileHeaderEx = ^TMetafileHeaderEx;
  {$EXTERNALSYM TMetafileHeaderEx}
  TMetafileHeaderEx = packed record
    case Integer of
      0: (WmfHeader: TMETAHEADER;);
      1: (EmfHeader: TENHMETAHEADER3);
  end;

  TMetafileHeader = packed class
  public
    Type_       : TMetafileType;
    Size        : UINT;
    Version     : UINT;
    EmfPlusFlags: UINT;
    DpiX        : Single;
    DpiY        : Single;
    X           : Integer;
    Y           : Integer;
    Width       : Integer;
    Height      : Integer;
    Header      : TMetafileHeaderEx;
    EmfPlusHeaderSize: Integer;
    LogicalDpiX      : Integer;
    LogicalDpiY      : Integer;
  public
    property GetType: TMetafileType read Type_;
    property GetMetafileSize: UINT read Size;
    property GetVersion: UINT read Version;
    property GetEmfPlusFlags: UINT read EmfPlusFlags;
    property GetDpiX: Single read DpiX;
    property GetDpiY: Single read DpiY;
    procedure GetBounds(out Rect: TsgGRect);
    function IsWmf: BOOL;
    function IsWmfPlaceable: BOOL;
    function IsEmf: BOOL;
    function IsEmfOrEmfPlus: BOOL;
    function IsEmfPlus: BOOL;
    function IsEmfPlusDual: BOOL;
    function IsEmfPlusOnly: BOOL;
    function IsDisplay: BOOL;
    function GetWmfHeader: PMetaHeader;
    function GetEmfHeader: PENHMETAHEADER3;
  end;

const
  ImageFormatUndefined    : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatMemoryBMP    : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatBMP          : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatEMF          : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatWMF          : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatJPEG         : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatPNG          : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatGIF          : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatTIFF         : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatEXIF         : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  ImageFormatIcon         : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';
  FrameDimensionTime      : TGUID = '{6aedbd6d-3fb5-418a-83a6-7f45229dc872}';
  FrameDimensionResolution: TGUID = '{84236f7b-3bd3-428f-8dab-4ea1439ca315}';
  FrameDimensionPage      : TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';
  FormatIDImageInformation: TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  FormatIDJpegAppHeaders  : TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';
  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
  CodecIImageBytes        : TGUID = '{025d1823-6c7d-447b-bbdb-a3cbc3dfa2fc}';

type
  IImageBytes = Interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    function CountBytes(out pcb: UINT): HRESULT; stdcall;
    function LockBytes(cb: UINT; ulOffset: ULONG; out ppvBytes: Pointer): HRESULT; stdcall;
    function UnlockBytes(pvBytes: Pointer; cb: UINT; ulOffset: ULONG): HRESULT; stdcall;
  end;
  
  ImageCodecInfo = packed record
    Clsid            : TGUID;
    FormatID         : TGUID;
    CodecName        : PWCHAR;
    DllName          : PWCHAR;
    FormatDescription: PWCHAR;
    FilenameExtension: PWCHAR;
    MimeType         : PWCHAR;
    Flags            : DWORD;
    Version          : DWORD;
    SigCount         : DWORD;
    SigSize          : DWORD;
    SigPattern       : PBYTE;
    SigMask          : PBYTE;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;

{$IFDEF SGFULLGDIPLUS}
  ImageCodecFlags = (
    ImageCodecFlagsEncoder            = $00000001,
    ImageCodecFlagsDecoder            = $00000002,
    ImageCodecFlagsSupportBitmap      = $00000004,
    ImageCodecFlagsSupportVector      = $00000008,
    ImageCodecFlagsSeekableEncode     = $00000010,
    ImageCodecFlagsBlockingDecode     = $00000020,
    ImageCodecFlagsBuiltin            = $00010000,
    ImageCodecFlagsSystem             = $00020000,
    ImageCodecFlagsUser               = $00040000
  );
  TImageCodecFlags = ImageCodecFlags;
{$ELSE}
  ImageCodecFlags = Integer;
  const
    ImageCodecFlagsEncoder            = $00000001;
    ImageCodecFlagsDecoder            = $00000002;
    ImageCodecFlagsSupportBitmap      = $00000004;
    ImageCodecFlagsSupportVector      = $00000008;
    ImageCodecFlagsSeekableEncode     = $00000010;
    ImageCodecFlagsBlockingDecode     = $00000020;
    ImageCodecFlagsBuiltin            = $00010000;
    ImageCodecFlagsSystem             = $00020000;
    ImageCodecFlagsUser               = $00040000;

type
  TImageCodecFlags = ImageCodecFlags;
{$ENDIF}

  ImageLockMode = Integer;
  const
    ImageLockModeRead         = $0001;
    ImageLockModeWrite        = $0002;
    ImageLockModeUserInputBuf = $0004;
type
  TImageLockMode = ImageLockMode;

  BitmapData = packed record
    Width      : UINT;
    Height     : UINT;
    Stride     : Integer;
    PixelFormat: PixelFormat;
    Scan0      : Pointer;
    Reserved   : UINT;
  end;
  TBitmapData = BitmapData;
  PBitmapData = ^TBitmapData;

{$IFDEF SGFULLGDIPLUS}
  ImageFlags = (
    ImageFlagsNone                = 0,
    ImageFlagsScalable            = $0001,
    ImageFlagsHasAlpha            = $0002,
    ImageFlagsHasTranslucent      = $0004,
    ImageFlagsPartiallyScalable   = $0008,
    ImageFlagsColorSpaceRGB       = $0010,
    ImageFlagsColorSpaceCMYK      = $0020,
    ImageFlagsColorSpaceGRAY      = $0040,
    ImageFlagsColorSpaceYCBCR     = $0080,
    ImageFlagsColorSpaceYCCK      = $0100,
    ImageFlagsHasRealDPI          = $1000,
    ImageFlagsHasRealPixelSize    = $2000,
    ImageFlagsReadOnly            = $00010000,
    ImageFlagsCaching             = $00020000);
  TImageFlags = ImageFlags;
{$ELSE}
  ImageFlags = Integer;
  const
    ImageFlagsNone                = 0;
    ImageFlagsScalable            = $0001;
    ImageFlagsHasAlpha            = $0002;
    ImageFlagsHasTranslucent      = $0004;
    ImageFlagsPartiallyScalable   = $0008;
    ImageFlagsColorSpaceRGB       = $0010;
    ImageFlagsColorSpaceCMYK      = $0020;
    ImageFlagsColorSpaceGRAY      = $0040;
    ImageFlagsColorSpaceYCBCR     = $0080;
    ImageFlagsColorSpaceYCCK      = $0100;
    ImageFlagsHasRealDPI          = $1000;
    ImageFlagsHasRealPixelSize    = $2000;
    ImageFlagsReadOnly            = $00010000;
    ImageFlagsCaching             = $00020000;

type
  TImageFlags = ImageFlags;
{$ENDIF}

{$IFDEF SGFULLGDIPLUS}
  RotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,
    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,
    RotateNoneFlipY    = Rotate180FlipX,
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,
    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone);
  TRotateFlipType = RotateFlipType;
{$ELSE}

  RotateFlipType = (RotateNoneFlipNone, Rotate90FlipNone, Rotate180FlipNone,
    Rotate270FlipNone, RotateNoneFlipX, Rotate90FlipX, Rotate180FlipX, Rotate270FlipX);

  const
    RotateNoneFlipY    = Rotate180FlipX;
    Rotate90FlipY      = Rotate270FlipX;
    Rotate180FlipY     = RotateNoneFlipX;
    Rotate270FlipY     = Rotate90FlipX;
    RotateNoneFlipXY   = Rotate180FlipNone;
    Rotate90FlipXY     = Rotate270FlipNone;
    Rotate180FlipXY    = RotateNoneFlipNone;
    Rotate270FlipXY    = Rotate90FlipNone;

type
  TRotateFlipType = RotateFlipType;
{$ENDIF}

  EncoderParameter = packed record
    Guid          : TGUID;
    NumberOfValues: ULONG;
    Type_         : ULONG;
    Value         : Pointer;
  end;
  TEncoderParameter = EncoderParameter;
  PEncoderParameter = ^TEncoderParameter;

  EncoderParameters = packed record
    Count    : UINT;
    Parameter: array[0..0] of TEncoderParameter;
  end;
  TEncoderParameters = EncoderParameters;
  PEncoderParameters = ^TEncoderParameters;

  PropertyItem = record
    id      : PROPID;
    length  : ULONG;
    type_   : WORD;
    value   : Pointer;
  end;
  TPropertyItem = PropertyItem;
  PPropertyItem = ^TPropertyItem;

const
  PropertyTagTypeByte     : Integer =  1;
  PropertyTagTypeASCII    : Integer =  2;
  PropertyTagTypeShort    : Integer =  3;
  PropertyTagTypeLong     : Integer =  4;
  PropertyTagTypeRational : Integer =  5;
  PropertyTagTypeUndefined: Integer =  7;
  PropertyTagTypeSLONG    : Integer =  9;
  PropertyTagTypeSRational: Integer = 10;

  PropertyTagExifIFD                        = $8769;
  PropertyTagGpsIFD                         = $8825;
  PropertyTagNewSubfileType                 = $00FE;
  PropertyTagSubfileType                    = $00FF;
  PropertyTagImageWidth                     = $0100;
  PropertyTagImageHeight                    = $0101;
  PropertyTagBitsPerSample                  = $0102;
  PropertyTagCompression                    = $0103;
  PropertyTagPhotometricInterp              = $0106;
  PropertyTagThreshHolding                  = $0107;
  PropertyTagCellWidth                      = $0108;
  PropertyTagCellHeight                     = $0109;
  PropertyTagFillOrder                      = $010A;
  PropertyTagDocumentName                   = $010D;
  PropertyTagImageDescription               = $010E;
  PropertyTagEquipMake                      = $010F;
  PropertyTagEquipModel                     = $0110;
  PropertyTagStripOffsets                   = $0111;
  PropertyTagOrientation                    = $0112;
  PropertyTagSamplesPerPixel                = $0115;
  PropertyTagRowsPerStrip                   = $0116;
  PropertyTagStripBytesCount                = $0117;
  PropertyTagMinSampleValue                 = $0118;
  PropertyTagMaxSampleValue                 = $0119;
  PropertyTagXResolution                    = $011A;
  PropertyTagYResolution                    = $011B;
  PropertyTagPlanarConfig                   = $011C;
  PropertyTagPageName                       = $011D;
  PropertyTagXPosition                      = $011E;
  PropertyTagYPosition                      = $011F;
  PropertyTagFreeOffset                     = $0120;
  PropertyTagFreeByteCounts                 = $0121;
  PropertyTagGrayResponseUnit               = $0122;
  PropertyTagGrayResponseCurve              = $0123;
  PropertyTagT4Option                       = $0124;
  PropertyTagT6Option                       = $0125;
  PropertyTagResolutionUnit                 = $0128;
  PropertyTagPageNumber                     = $0129;
  PropertyTagTransferFuncition              = $012D;
  PropertyTagSoftwareUsed                   = $0131;
  PropertyTagDateTime                       = $0132;
  PropertyTagArtist                         = $013B;
  PropertyTagHostComputer                   = $013C;
  PropertyTagPredictor                      = $013D;
  PropertyTagWhitePoint                     = $013E;
  PropertyTagPrimaryChromaticities          = $013F;
  PropertyTagColorMap                       = $0140;
  PropertyTagHalftoneHints                  = $0141;
  PropertyTagTileWidth                      = $0142;
  PropertyTagTileLength                     = $0143;
  PropertyTagTileOffset                     = $0144;
  PropertyTagTileByteCounts                 = $0145;
  PropertyTagInkSet                         = $014C;
  PropertyTagInkNames                       = $014D;
  PropertyTagNumberOfInks                   = $014E;
  PropertyTagDotRange                       = $0150;
  PropertyTagTargetPrinter                  = $0151;
  PropertyTagExtraSamples                   = $0152;
  PropertyTagSampleFormat                   = $0153;
  PropertyTagSMinSampleValue                = $0154;
  PropertyTagSMaxSampleValue                = $0155;
  PropertyTagTransferRange                  = $0156;
  PropertyTagJPEGProc                       = $0200;
  PropertyTagJPEGInterFormat                = $0201;
  PropertyTagJPEGInterLength                = $0202;
  PropertyTagJPEGRestartInterval            = $0203;
  PropertyTagJPEGLosslessPredictors         = $0205;
  PropertyTagJPEGPointTransforms            = $0206;
  PropertyTagJPEGQTables                    = $0207;
  PropertyTagJPEGDCTables                   = $0208;
  PropertyTagJPEGACTables                   = $0209;
  PropertyTagYCbCrCoefficients              = $0211;
  PropertyTagYCbCrSubsampling               = $0212;
  PropertyTagYCbCrPositioning               = $0213;
  PropertyTagREFBlackWhite                  = $0214;
  PropertyTagICCProfile                     = $8773;
  PropertyTagGamma                          = $0301;
  PropertyTagICCProfileDescriptor           = $0302;
  PropertyTagSRGBRenderingIntent            = $0303;
  PropertyTagImageTitle                     = $0320;
  PropertyTagCopyright                      = $8298;
  PropertyTagResolutionXUnit                = $5001;
  PropertyTagResolutionYUnit                = $5002;
  PropertyTagResolutionXLengthUnit          = $5003;
  PropertyTagResolutionYLengthUnit          = $5004;
  PropertyTagPrintFlags                     = $5005;
  PropertyTagPrintFlagsVersion              = $5006;
  PropertyTagPrintFlagsCrop                 = $5007;
  PropertyTagPrintFlagsBleedWidth           = $5008;
  PropertyTagPrintFlagsBleedWidthScale      = $5009;
  PropertyTagHalftoneLPI                    = $500A;
  PropertyTagHalftoneLPIUnit                = $500B;
  PropertyTagHalftoneDegree                 = $500C;
  PropertyTagHalftoneShape                  = $500D;
  PropertyTagHalftoneMisc                   = $500E;
  PropertyTagHalftoneScreen                 = $500F;
  PropertyTagJPEGQuality                    = $5010;
  PropertyTagGridSize                       = $5011;
  PropertyTagThumbnailFormat                = $5012;
  PropertyTagThumbnailWidth                 = $5013;
  PropertyTagThumbnailHeight                = $5014;
  PropertyTagThumbnailColorDepth            = $5015;
  PropertyTagThumbnailPlanes                = $5016;
  PropertyTagThumbnailRawBytes              = $5017;
  PropertyTagThumbnailSize                  = $5018;
  PropertyTagThumbnailCompressedSize        = $5019;
  PropertyTagColorTransferFunction          = $501A;
  PropertyTagThumbnailData                  = $501B;
  PropertyTagThumbnailImageWidth            = $5020;
  PropertyTagThumbnailImageHeight           = $5021;
  PropertyTagThumbnailBitsPerSample         = $5022;
  PropertyTagThumbnailCompression           = $5023;
  PropertyTagThumbnailPhotometricInterp     = $5024;
  PropertyTagThumbnailImageDescription      = $5025;
  PropertyTagThumbnailEquipMake             = $5026;
  PropertyTagThumbnailEquipModel            = $5027;
  PropertyTagThumbnailStripOffsets          = $5028;
  PropertyTagThumbnailOrientation           = $5029;
  PropertyTagThumbnailSamplesPerPixel       = $502A;
  PropertyTagThumbnailRowsPerStrip          = $502B;
  PropertyTagThumbnailStripBytesCount       = $502C;
  PropertyTagThumbnailResolutionX           = $502D;
  PropertyTagThumbnailResolutionY           = $502E;
  PropertyTagThumbnailPlanarConfig          = $502F;
  PropertyTagThumbnailResolutionUnit        = $5030;
  PropertyTagThumbnailTransferFunction      = $5031;
  PropertyTagThumbnailSoftwareUsed          = $5032;
  PropertyTagThumbnailDateTime              = $5033;
  PropertyTagThumbnailArtist                = $5034;
  PropertyTagThumbnailWhitePoint            = $5035;
  PropertyTagThumbnailPrimaryChromaticities = $5036;
  PropertyTagThumbnailYCbCrCoefficients     = $5037;
  PropertyTagThumbnailYCbCrSubsampling      = $5038;
  PropertyTagThumbnailYCbCrPositioning      = $5039;
  PropertyTagThumbnailRefBlackWhite         = $503A;
  PropertyTagThumbnailCopyRight             = $503B;
  PropertyTagLuminanceTable                 = $5090;
  PropertyTagChrominanceTable               = $5091;
  PropertyTagFrameDelay                     = $5100;
  PropertyTagLoopCount                      = $5101;
  PropertyTagPixelUnit                      = $5110;
  PropertyTagPixelPerUnitX                  = $5111;
  PropertyTagPixelPerUnitY                  = $5112;
  PropertyTagPaletteHistogram               = $5113;
  PropertyTagExifExposureTime               = $829A;
  PropertyTagExifFNumber                    = $829D;
  PropertyTagExifExposureProg               = $8822;
  PropertyTagExifSpectralSense              = $8824;
  PropertyTagExifISOSpeed                   = $8827;
  PropertyTagExifOECF                       = $8828;
  PropertyTagExifVer                        = $9000;
  PropertyTagExifDTOrig                     = $9003;
  PropertyTagExifDTDigitized                = $9004;
  PropertyTagExifCompConfig                 = $9101;
  PropertyTagExifCompBPP                    = $9102;
  PropertyTagExifShutterSpeed               = $9201;
  PropertyTagExifAperture                   = $9202;
  PropertyTagExifBrightness                 = $9203;
  PropertyTagExifExposureBias               = $9204;
  PropertyTagExifMaxAperture                = $9205;
  PropertyTagExifSubjectDist                = $9206;
  PropertyTagExifMeteringMode               = $9207;
  PropertyTagExifLightSource                = $9208;
  PropertyTagExifFlash                      = $9209;
  PropertyTagExifFocalLength                = $920A;
  PropertyTagExifMakerNote                  = $927C;
  PropertyTagExifUserComment                = $9286;
  PropertyTagExifDTSubsec                   = $9290;
  PropertyTagExifDTOrigSS                   = $9291;
  PropertyTagExifDTDigSS                    = $9292;
  PropertyTagExifFPXVer                     = $A000;
  PropertyTagExifColorSpace                 = $A001;
  PropertyTagExifPixXDim                    = $A002;
  PropertyTagExifPixYDim                    = $A003;
  PropertyTagExifRelatedWav                 = $A004;
  PropertyTagExifInterop                    = $A005;
  PropertyTagExifFlashEnergy                = $A20B;
  PropertyTagExifSpatialFR                  = $A20C;
  PropertyTagExifFocalXRes                  = $A20E;
  PropertyTagExifFocalYRes                  = $A20F;
  PropertyTagExifFocalResUnit               = $A210;
  PropertyTagExifSubjectLoc                 = $A214;
  PropertyTagExifExposureIndex              = $A215;
  PropertyTagExifSensingMethod              = $A217;
  PropertyTagExifFileSource                 = $A300;
  PropertyTagExifSceneType                  = $A301;
  PropertyTagExifCfaPattern                 = $A302;
  PropertyTagGpsVer                         = $0000;
  PropertyTagGpsLatitudeRef                 = $0001;
  PropertyTagGpsLatitude                    = $0002;
  PropertyTagGpsLongitudeRef                = $0003;
  PropertyTagGpsLongitude                   = $0004;
  PropertyTagGpsAltitudeRef                 = $0005;
  PropertyTagGpsAltitude                    = $0006;
  PropertyTagGpsGpsTime                     = $0007;
  PropertyTagGpsGpsSatellites               = $0008;
  PropertyTagGpsGpsStatus                   = $0009;
  PropertyTagGpsGpsMeasureMode              = $00A;
  PropertyTagGpsGpsDop                      = $000B;
  PropertyTagGpsSpeedRef                    = $000C;
  PropertyTagGpsSpeed                       = $000D;
  PropertyTagGpsTrackRef                    = $000E;
  PropertyTagGpsTrack                       = $000F;
  PropertyTagGpsImgDirRef                   = $0010;
  PropertyTagGpsImgDir                      = $0011;
  PropertyTagGpsMapDatum                    = $0012;
  PropertyTagGpsDestLatRef                  = $0013;
  PropertyTagGpsDestLat                     = $0014;
  PropertyTagGpsDestLongRef                 = $0015;
  PropertyTagGpsDestLong                    = $0016;
  PropertyTagGpsDestBearRef                 = $0017;
  PropertyTagGpsDestBear                    = $0018;
  PropertyTagGpsDestDistRef                 = $0019;
  PropertyTagGpsDestDist                    = $001A;

type
  ColorMatrix = packed array[0..4, 0..4] of Single;
  TColorMatrix = ColorMatrix;
  PColorMatrix = ^TColorMatrix;

  ColorMatrixFlags = (ColorMatrixFlagsDefault, ColorMatrixFlagsSkipGrays, ColorMatrixFlagsAltGray);
  TColorMatrixFlags = ColorMatrixFlags;

  ColorAdjustType = (ColorAdjustTypeDefault, ColorAdjustTypeBitmap, ColorAdjustTypeBrush,
    ColorAdjustTypePen, ColorAdjustTypeText, ColorAdjustTypeCount, ColorAdjustTypeAny);
  TColorAdjustType = ColorAdjustType;

  ColorMap = packed record
    ColorOld: TsgColor;
    ColorNew: TsgColor;
  end;
  TColorMap = ColorMap;
  PColorMap = ^TColorMap;

  TsgGDIPGraphics = Pointer;
  TsgGDIPBrush = Pointer;
  TsgGDIPTexture = Pointer;
  TsgGDIPSolidFill = Pointer;
  TsgGDIPLineGradient = Pointer;
  TsgGDIPPathGradient = Pointer;
  TsgGDIPHatch =  Pointer;
  TsgGDIPPen = Pointer;
  TsgGDIPCustomLineCap = Pointer;
  TsgGDIPAdjustableArrowCap = Pointer;
  TsgGDIPImage = Pointer;
  TsgGDIPBitMap = Pointer;
  TsgGDIPMetaFile = Pointer;
  TsgGDIPImageAttributes = Pointer;
  TsgGDIPPath = Pointer;
  TsgGDIPRegion = Pointer;
  TsgGDIPPathIterator = Pointer;
  TsgGDIPFontFamily = Pointer;
  TsgGDIPFont = Pointer;
  TsgGDIPStringFormat = Pointer;
  TsgGDIPFontCollection = Pointer;
//  TsgGDIPCachedBitmap = Pointer;
  TsgGDIPMatrix = Pointer;

{ Classes of GDIPlus }

type
  TsgGDIPlusGraphics = class;
  TsgGDIPlusPen = class;
  TsgGDIPlusBrush = class;
  TsgGDIPlusMatrix = class;
  TsgGDIPlusMetaFile = class;
  TsgGDIPlusFontFamily = class;
  TsgGDIPlusGraphicsPath = class;
  TsgGDIPlusRegion = class;
  TsgGDIPlusImage = class;
  TsgGDIPlusHatchBrush = class;
  TsgGDIPlusSolidBrush = class;
  TsgGDIPlusLinearGradientBrush = class;
  TsgGDIPlusPathGradientBrush = class;
  TsgGDIPlusFont = class;
  TsgGDIPlusFontCollection = class;
  TsgGDIPlusInstalledFontCollection = class;
  TsgGDIPlusPrivateFontCollection = class;
  TsgGDIPlusImageAttributes = class;

{ TsgGDIPlusRegion

  Perameter Dummy in constructors for CBuilder support. }
TsgGDIPlusRegion = class(TsgGDIPlusBase)
protected
  NativeRegion: TsgGDIPRegion;
  FStatusOfFunction: TsgGDIPStatus;
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  procedure SetNativeRegion(NativeRegion: TsgGDIPRegion);
  constructor Create(NativeRegion: TsgGDIPRegion; Dummy: Integer = 0); reintroduce; overload;
public
  constructor Create; reintroduce; overload;
  constructor Create(Rect: TsgGRectF); reintroduce; overload;
  constructor Create(Rect: TsgGRect); reintroduce; overload;
  constructor Create(Path: TsgGDIPlusGraphicsPath); reintroduce; overload;
  constructor Create(RegionData: PBYTE; Size: Integer); reintroduce; overload;
  constructor Create(hRgn: HRGN); reintroduce; overload;
  function FromHRGN(hRgn: HRGN): TsgGDIPlusRegion;
  destructor Destroy; override;
  function Clone: TsgGDIPlusRegion;
  function MakeInfinite: TsgGDIPStatus;
  function MakeEmpty: TsgGDIPStatus;
  function GetDataSize: UINT;
  function GetData(Buffer: PBYTE; BufferSize: UINT; SizeFilled: PUINT = nil): TsgGDIPStatus;
  function Intersect(const Rect: TsgGRect): TsgGDIPStatus; overload;
  function Intersect(const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function Intersect(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus; overload;
  function Intersect(Region: TsgGDIPlusRegion): TsgGDIPStatus; overload;
  function Union(const Rect: TsgGRect): TsgGDIPStatus; overload;
  function Union(const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function Union(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus; overload;
  function Union(Region: TsgGDIPlusRegion): TsgGDIPStatus; overload;
  function Xor_(const Rect: TsgGRect): TsgGDIPStatus; overload;
  function Xor_(const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function Xor_(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus; overload;
  function Xor_(Region: TsgGDIPlusRegion): TsgGDIPStatus; overload;
  function Exclude(const Rect: TsgGRect): TsgGDIPStatus; overload;
  function Exclude(const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function Exclude(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus; overload;
  function Exclude(Region: TsgGDIPlusRegion): TsgGDIPStatus; overload;
  function Complement(const Rect: TsgGRect): TsgGDIPStatus; overload;
  function Complement(const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function Complement(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus; overload;
  function Complement(Region: TsgGDIPlusRegion): TsgGDIPStatus; overload;
  function Translate(Dx, Dy: Single): TsgGDIPStatus; overload;
  function Translate(Dx, Dy: Integer): TsgGDIPStatus; overload;
  function Transform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function GetBounds(out Rect: TsgGRect; Graph: TsgGDIPlusGraphics): TsgGDIPStatus; overload;
  function GetBounds(out Rect: TsgGRectF; Graph: TsgGDIPlusGraphics): TsgGDIPStatus; overload;
  function GetHRGN(Graph: TsgGDIPlusGraphics): HRGN;
  function IsEmpty(Graph: TsgGDIPlusGraphics): BOOL;
  function IsInfinite(Graph: TsgGDIPlusGraphics): BOOL ;
  function IsVisible(X, Y: Integer; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(const point: TsgGPoint; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(X, Y: Single; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(const point: TsgGPointF; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(X, Y, Width, Height: Integer; Graph: TsgGDIPlusGraphics): BOOL; overload;
  function IsVisible(const Rect: TsgGRect; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(X, Y, Width, Height: Single; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(const Rect: TsgGRectF; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function EqualsRgn(Region: TsgGDIPlusRegion; Graph: TsgGDIPlusGraphics): BOOL;
  function GetRegionScansCount(const AMatrix: TsgGDIPlusMatrix): UINT;
  function GetRegionScans(const AMatrix: TsgGDIPlusMatrix ;Rects: PsgGRectF; out Count: Integer): TsgGDIPStatus; overload;
  function GetRegionScans(const AMatrix: TsgGDIPlusMatrix; Rects: PsgGRect; out Count: Integer): TsgGDIPStatus; overload;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusFontFamily = class(TsgGDIPlusBase)
protected
  NativeFamily: TsgGDIPFontFamily;
  FStatusOfFunction: TsgGDIPStatus;
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(NativeOrig: TsgGDIPFontFamily;
    Status: TsgGDIPStatus); reintroduce; overload;
public
  constructor Create; reintroduce; overload;
  constructor Create(name: WideString; FontCollection: TsgGDIPlusFontCollection = nil); reintroduce; overload;
  destructor Destroy; override;
  class function GenericSansSerif: TsgGDIPlusFontFamily;
  class function GenericSerif: TsgGDIPlusFontFamily;
  class function GenericMonospace: TsgGDIPlusFontFamily;
  function GetFamilyName(out name: String; Language: LANGID = 0): TsgGDIPStatus;
  function Clone: TsgGDIPlusFontFamily;
  function IsAvailable: BOOL;
  function IsStyleAvailable(Style: Integer): BOOL;
  function GetEmHeight(Style: Integer): UINT16;
  function GetCellAscent(Style: Integer): UINT16;
  function GetCellDescent(Style: Integer): UINT16;
  function GetLineSpacing(Style: Integer): UINT16;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusFontCollection = class(TsgGDIPlusBase)
protected
  NativeFontCollection: TsgGDIPFontCollection;
  FStatusOfFunction: TsgGDIPStatus;
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
public
  constructor Create;
  destructor Destroy; override;
  function GetFamilyCount: Integer;
  function GetFamilies(NumSought: Integer; out AFamilies: array of TsgGDIPlusFontFamily; out NumFound: Integer): TsgGDIPStatus;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusInstalledFontCollection = class(TsgGDIPlusFontCollection)
public
  constructor Create; reintroduce;
  destructor Destroy; override;
end;

TsgGDIPlusPrivateFontCollection = class(TsgGDIPlusFontCollection)
public
  constructor Create; reintroduce;
  destructor Destroy; override;
  function AddFontFile(FileName: WideString): TsgGDIPStatus;
  function AddMemoryFont(Memory: Pointer; Length: Integer): TsgGDIPStatus;
end;

TsgGDIPlusFont = class(TsgGDIPlusBase)
protected
  NativeFont: TsgGDIPFont;
  FStatusOfFunction: TsgGDIPStatus;
  procedure SetNativeFont(Font: TsgGDIPFont);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(Font: TsgGDIPFont; Status: TsgGDIPStatus); overload;
public
  constructor Create(ADC: HDC); reintroduce; overload;
  constructor Create(ADC: HDC; ALogFont: PLogFontA); reintroduce; overload;
  constructor Create(ADC: HDC; ALogFont: PLogFontW); reintroduce; overload;
  constructor Create(ADC: HDC; AFont: HFONT); reintroduce; overload;
  constructor Create(Family: TsgGDIPlusFontFamily; ASize: Single; Style: TFontStyle = FontStyleRegular;
    Unit_: TsgGDIPUnit = UnitPoint); reintroduce; overload;
  constructor Create(FamilyName: WideString; ASize: Single; Style: TFontStyle = FontStyleRegular; Unit_: TsgGDIPUnit = UnitPoint;
    FontCollection: TsgGDIPlusFontCollection = nil); reintroduce; overload;
  function GetLogFontA(Graph: TsgGDIPlusGraphics; out LogFontA: TLogFontA): TsgGDIPStatus;
  function GetLogFontW(Graph: TsgGDIPlusGraphics; out LogFontW: TLogFontW): TsgGDIPStatus;
  function Clone: TsgGDIPlusFont;
  destructor Destroy; override;
  function IsAvailable: BOOL;
  function GetStyle: Integer;
  function GetSize: Single;
  function GetUnit: TsgGDIPUnit;
  function GetStatusOfFunction: TsgGDIPStatus;
  function GetHeight(Graphics: TsgGDIPlusGraphics): Single; overload;
  function GetHeight(Dpi: Single): Single; overload;
  function GetFamily(Family: TsgGDIPlusFontFamily): TsgGDIPStatus;
end;

TsgGDIPlusImage = class(TsgGDIPlusBase)
protected
  NativeImage: TsgGDIPImage;
  FStatusOfFunction: TsgGDIPStatus;
  procedure SetNativeImage(NativeImage: TsgGDIPImage);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(NativeImage: TsgGDIPImage; Status: TsgGDIPStatus); reintroduce; overload;
public
  constructor Create(FileName: WideString; useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
  constructor Create(Stream: IStream; useEmbeddedColorManagement: BOOL  = FALSE); reintroduce; overload;
  function FromFile(FileName: WideString; useEmbeddedColorManagement: BOOL = FALSE): TsgGDIPlusImage;
  function FromStream(Stream: IStream; useEmbeddedColorManagement: BOOL = FALSE): TsgGDIPlusImage;
  destructor Destroy; override;
  function Clone: TsgGDIPlusImage;
  function Save(FileName: WideString; const ClsIdEncoder: TGUID;
    EncoderParams: PEncoderParameters = nil): TsgGDIPStatus; overload;
  function Save(Stream: IStream; const ClsIdEncoder: TGUID;
    EncoderParams: PEncoderParameters  = nil): TsgGDIPStatus; overload;
  function SaveAdd(EncoderParams: PEncoderParameters): TsgGDIPStatus; overload;
  function SaveAdd(newImage: TsgGDIPlusImage; EncoderParams: PEncoderParameters): TsgGDIPStatus; overload;
  function GetType: TImageType;
  function GetPhysicalDimension(out Size: TsgGSizeF): TsgGDIPStatus;
  function GetBounds(out SrcRect: TsgGRectF; out SrcUnit: TsgGDIPUnit): TsgGDIPStatus;
  function GetWidth: UINT;
  function GetHeight: UINT;
  function GetHorizontalResolution: Single;
  function GetVerticalResolution: Single;
  function GetFlags: UINT;
  function GetRawFormat(out Format: TGUID): TsgGDIPStatus;
  function GetPixelFormat: TPixelFormat;
  function GetPaletteSize: Integer;
  function GetPalette(Palette: PColorPalette; Size: Integer): TsgGDIPStatus;
  function SetPalette(Palette: PColorPalette): TsgGDIPStatus;
  function GetThumbnailImage(ThumbWidth, ThumbHeight: UINT): TsgGDIPlusImage;
  function GetFrameDimensionsCount: UINT;
  function GetFrameDimensionsList(DimensionIDs: PGUID; Count: UINT): TsgGDIPStatus;
  function GetFrameCount(const DimensionID: TGUID): UINT;
  function SelectActiveFrame(const DimensionID: TGUID; FrameIndex: UINT): TsgGDIPStatus;
  function RotateFlip(rotateFlipType: TRotateFlipType): TsgGDIPStatus;
  function GetPropertyCount: UINT;
  function GetPropertyIdList(NumOfProperty: UINT; List: PPropID): TsgGDIPStatus;
  function GetPropertyItASize(PropId: PROPID): UINT;
  function GetPropertyItem(PropId: PROPID; PropSize: UINT; Buffer: PPropertyItem): TsgGDIPStatus;
  function GetPropertySize(out TotalBufferSize, NumProperties: UINT): TsgGDIPStatus;
  function GetAllPropertyItems(TotalBufferSize, NumProperties: UINT; AllItems: PPROPERTYITEM): TsgGDIPStatus;
  function RemovePropertyItem(PropId: TPROPID): TsgGDIPStatus;
  function SetPropertyItem(const Item: TPropertyItem): TsgGDIPStatus;
  function GetEncoderParameterListSize(const ClsIdEncoder: TGUID): UINT;
  function GetEncoderParameterList(const ClsIdEncoder: TGUID; Size: UINT; Buffer: PEncoderParameters): TsgGDIPStatus;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusBitmap = class(TsgGDIPlusImage)
public
  constructor Create(FileName: WideString; useEmbeddedColorManagement: BOOL = FALSE); reintroduce; overload;
  constructor Create(Stream: IStream; useEmbeddedColorManagement: BOOL  = FALSE); reintroduce; overload;
  constructor Create(Handle: HBITMAP; hPal: HPALETTE);  reintroduce; overload;
  constructor Create(Width, Height, Stride: Integer; Format: PIXELFORMAT; scan0: PBYTE); reintroduce; overload;
  constructor Create(var GDIBitmapInfo: TBitmapInfo; GDIBitmapData: Pointer); reintroduce; overload;
  constructor Create(ABitmap: Graphics.TBitmap); reintroduce; overload;
end;

TsgGDIPlusCustomLineCap = class(TsgGDIPlusBase)
protected
  NativeCap: TsgGDIPCustomLineCap;
  FStatusOfFunction: TsgGDIPStatus;
  procedure SetNativeCap(NativeCap: TsgGDIPCustomLineCap);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(NativeCap: TsgGDIPCustomLineCap; Status: TsgGDIPStatus); reintroduce; overload;
public
  constructor Create; reintroduce; overload;
  constructor Create(FillPath, StrokePath: TsgGDIPlusGraphicsPath; BaseCap: TsgGDIPLineCap = LineCapFlat;  BaseInset: Single = 0); reintroduce; overload;
  destructor Destroy; override;
  function Clone: TsgGDIPlusCustomLineCap;
  function SetStrokeCap(StrokeCap: TsgGDIPLineCap): TsgGDIPStatus;
  function SetStrokeCaps(StartCap, EndCap: TsgGDIPLineCap): TsgGDIPStatus;
  function GetStrokeCaps(out StartCap, EndCap: TsgGDIPLineCap): TsgGDIPStatus;
  function SetStrokeJoin(LineJoin: TsgGDIPLineJoin): TsgGDIPStatus;
  function GetStrokeJoin: TsgGDIPLineJoin;
  function SetBaseCap(BaseCap: TsgGDIPLineCap): TsgGDIPStatus;
  function GetBaseCap: TsgGDIPLineCap;
  function SetBaseInset(Inset: Single): TsgGDIPStatus;
  function GetBaseInset: Single;
  function SetWidthScale(WidthScale: Single): TsgGDIPStatus;
  function GetWidthScale: Single;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusImageAttributes = class(TsgGDIPlusBase)
protected
  NativeImageAttr: TsgGDIPImageAttributes;
  FStatusOfFunction: TsgGDIPStatus;
  procedure SetNativeImageAttr(NativeImageAttr: TsgGDIPImageAttributes);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(ImageAttr: TsgGDIPImageAttributes; Status: TsgGDIPStatus); reintroduce; overload;
public
  constructor Create; reintroduce; overload;
  destructor Destroy; override;
  function Clone: TsgGDIPlusImageAttributes;
  function SetToIdentity(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function Reset(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetColorMatrix(const ColorMatrix: TColorMatrix;
    Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
    Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearColorMatrix(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetColorMatrices(const AColorMatrix: TColorMatrix; const AgrayMatrix: TColorMatrix;
    Mode: TColorMatrixFlags  = ColorMatrixFlagsDefault;
    Type_: TColorAdjustType  = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearColorMatrices(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetThreshold(threshold: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearThreshold(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetGamma(gamma: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearGamma( Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetColorKey(ColorLow, ColorHigh: TsgColor; Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearColorKey(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetOutputChannel(channelFlags: TColorChannelFlags; Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearOutputChannel(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetOutputChannelColorProFile(ColorProFileFilename: WideString;
    Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearOutputChannelColorProFile(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetRemapTable(mapSize: Cardinal; map: PColorMap; Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function ClearRemapTable(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
  function SetBrushRemapTable(mapSize: Cardinal; map: PColorMap): TsgGDIPStatus;
  function ClearBrushRemapTable: TsgGDIPStatus;
  function SetWrapMode(Wrap: TsgGDIPWrapMode; Color: TsgColor = aclBlack; clamp: BOOL = FALSE): TsgGDIPStatus;
  function GetAdjustedPalette(ColorPalette: PColorPalette; ColorAdjustType: TColorAdjustType): TsgGDIPStatus;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TMatrixArray = array[0..5] of Single;

TsgGDIPlusMatrix = class(TsgGDIPlusBase)
protected
  NativeMatrix: TsgGDIPMatrix;
  FStatusOfFunction: TsgGDIPStatus ;
  procedure SetNativeMatrix(NativeMatrix: TsgGDIPMatrix);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(NativeMatrix: TsgGDIPMatrix); reintroduce; overload;
public
  constructor Create; reintroduce; overload;
  constructor Create(const M11, M12, M21, M22, Dx, Dy: Single); reintroduce; overload;
  constructor Create(const Rect: TsgGRectF; const DstPlg: TsgGPointF); reintroduce; overload;
  constructor Create(const Rect: TsgGRect; const DstPlg: TsgGPoint); reintroduce; overload;
  destructor Destroy; override;
  function Clone: TsgGDIPlusMatrix;
  function GetElements(const m: TMatrixArray): TsgGDIPStatus;
  function SetElements(M11, M12, M21, M22, Dx, Dy: Single): TsgGDIPStatus;
  function OffsetX: Single;
  function OffsetY: Single;
  function Reset: TsgGDIPStatus;
  function Multiply(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function Translate(OffsetX, OffsetY: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function Scale(ScaleX, ScaleY: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function Rotate(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function RotateAt(Angle: Single; const center: TsgGPointF; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function Shear(ShearX, ShearY: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function Invert: TsgGDIPStatus;
  function TransformPoints(Pts: PsgGPointF; Count: Integer = 1): TsgGDIPStatus; overload;
  function TransformPoints(Pts: PsgGPoint; Count: Integer = 1): TsgGDIPStatus; overload;
  function TransformVectors(Pts: PsgGPointF; Count: Integer = 1): TsgGDIPStatus; overload;
  function TransformVectors(Pts: PsgGPoint; Count: Integer = 1): TsgGDIPStatus; overload;
  function IsInvertible: BOOL;
  function IsIdentity: BOOL;
  function EqualsMat(const AMatrix: TsgGDIPlusMatrix): BOOL;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusBrush = class(TsgGDIPlusBase)
protected
  NativeBrush: TsgGDIPBrush;
  FStatusOfFunction: TsgGDIPStatus;
  procedure SetNativeBrush(NativeBrush: TsgGDIPBrush);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(NativeBrush: TsgGDIPBrush; Status: TsgGDIPStatus); overload;
public
  constructor Create; overload;
  destructor Destroy; override;
  function Clone: TsgGDIPlusBrush; virtual;
  function GetType: TsgGDIPBrushType;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusSolidBrush = class(TsgGDIPlusBrush)
public
  constructor Create(Color: TsgColor); reintroduce; overload;
  constructor Create; reintroduce; overload;
  function GetColor(out Color: TsgColor): TsgGDIPStatus;
  function SetColor(Color: TsgColor): TsgGDIPStatus;
end;

TsgGDIPlusTextureBrush = class(TsgGDIPlusBrush)
public
  constructor Create; reintroduce; overload;
  constructor Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode = WrapModeTile); reintroduce; overload;
  constructor Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode; DstRect: TsgGRectF); reintroduce; overload;
  constructor Create(Image: TsgGDIPlusImage; DstRect: TsgGRectF; ImageAttributes: TsgGDIPlusImageAttributes = nil); reintroduce; overload;
  constructor Create(Image: TsgGDIPlusImage; DstRect: TsgGRect; ImageAttributes: TsgGDIPlusImageAttributes = nil); reintroduce; overload;
  constructor Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode; DstRect: TsgGRect); reintroduce; overload;
  constructor Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode; DstX, DstY, DstWidth,
    DstHeight: Single); reintroduce; overload;
  constructor Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode; DstX, DstY, DstWidth,
    DstHeight: Integer); reintroduce; overload;
  function SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function ResetTransform: TsgGDIPStatus;
  function MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function TranslateTransform(Dx, Dy: Single; Order: MatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function SetWrapMode(WrapMode: TsgGDIPWrapMode): TsgGDIPStatus;
  function GetWrapMode: TsgGDIPWrapMode;
  function GetImage: TsgGDIPlusImage;
end;

TsgGDIPlusLinearGradientBrush = class(TsgGDIPlusBrush)
public
  constructor Create; reintroduce; overload;
  constructor Create(const point1, point2: TsgGPointF; Color1, Color2: TsgColor); reintroduce; overload;
  constructor Create(const point1, point2: TsgGPoint; Color1,  Color2: TsgColor); reintroduce; overload;
  constructor Create(Rect: TsgGRectF; Color1, Color2: TsgColor; Mode: TLinearGradientMode); reintroduce; overload;
  constructor Create(Rect: TsgGRect; Color1, Color2: TsgColor; Mode: TLinearGradientMode); reintroduce; overload;
  constructor Create(Rect: TsgGRectF; Color1, Color2: TsgColor; Angle: Single;
    isAngleScalable: BOOL = FALSE); overload;
  constructor Create(Rect: TsgGRect; Color1, Color2: TsgColor; Angle: Single;
    isAngleScalable: BOOL = FALSE); overload;
  function SetLinearColors(Color1, Color2: TsgColor): TsgGDIPStatus;
  function GetLinearColors(out Color1, Color2: TsgColor): TsgGDIPStatus;
  function GetRectangle(out Rect: TsgGRectF): TsgGDIPStatus; overload;
  function GetRectangle(out Rect: TsgGRect): TsgGDIPStatus; overload;
  function SetGammaCorRection(UseGammaCorRection: BOOL): TsgGDIPStatus;
  function GetGammaCorRection: BOOL;
  function GetBlendCount: Integer;
  function SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
  function GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
  function GetInterpolationColorCount: Integer;
  function SetInterpolationColors(APresetColors: PsgColor; BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
  function GetInterpolationColors(APresetColors: PsgColor; BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
  function SetBlendBellShape(Focus: Single; Scale: Single = 1.0): TsgGDIPStatus;
  function SetBlendTriangularShape(Focus: Single; Scale: Single = 1.0): TsgGDIPStatus;
  function SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function ResetTransform: TsgGDIPStatus;
  function MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function TranslateTransform(Dx, Dy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function SetWrapMode(WrapMode: TsgGDIPWrapMode): TsgGDIPStatus;
  function GetWrapMode: TsgGDIPWrapMode;
end;

TsgGDIPlusHatchBrush = class(TsgGDIPlusBrush)
public
  constructor Create; reintroduce; overload;
  constructor Create(HatchStyle: TsgGDIPHatchStyle; foreColor: TsgColor; backColor: TsgColor = aclBlack); reintroduce; overload;
  function GetHatchStyle: TsgGDIPHatchStyle;
  function GetForegroundColor(out Color: TsgColor): TsgGDIPStatus;
  function GetBackgroundColor(out Color: TsgColor): TsgGDIPStatus;
end;

TsgGDIPlusPen = class(TsgGDIPlusBase)
protected
  NativePen: TsgGDIPPen;
  FStatusOfFunction: TsgGDIPStatus;
  procedure SetNativePen(NativePen: TsgGDIPPen);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(NativePen: TsgGDIPPen; Status: TsgGDIPStatus); reintroduce; overload;
public
  constructor Create(Color: TsgColor; Width: Single = 1.0); reintroduce; overload;
  constructor Create(Brush: TsgGDIPlusBrush; Width: Single = 1.0); reintroduce; overload;
  destructor Destroy; override;
  function Clone: TsgGDIPlusPen;
  function SetWidth(Width: Single): TsgGDIPStatus;
  function GetWidth: Single;
  function SetLineCap(StartCap, EndCap: TsgGDIPLineCap; DashCap: TsgGDIPDashCap): TsgGDIPStatus;
  function SetStartCap(StartCap: TsgGDIPLineCap): TsgGDIPStatus;
  function SetEndCap(EndCap: TsgGDIPLineCap): TsgGDIPStatus;
  function SetDashCap(DashCap: TsgGDIPDashCap): TsgGDIPStatus;
  function GetStartCap: TsgGDIPLineCap;
  function GetEndCap: TsgGDIPLineCap;
  function GetDashCap: TsgGDIPDashCap;
  function SetLineJoin(LineJoin: TsgGDIPLineJoin): TsgGDIPStatus;
  function GetLineJoin: TsgGDIPLineJoin;
  function SetCustomStartCap(CustomCap: TsgGDIPlusCustomLineCap): TsgGDIPStatus;
  function GetCustomStartCap(CustomCap: TsgGDIPlusCustomLineCap): TsgGDIPStatus;
  function SetCustomEndCap(CustomCap: TsgGDIPlusCustomLineCap): TsgGDIPStatus;
  function GetCustomEndCap(CustomCap: TsgGDIPlusCustomLineCap): TsgGDIPStatus;
  function SetMiterLimit(MiterLimit: Single): TsgGDIPStatus;
  function GetMiterLimit: Single;
  function SetAlignment(PenAlignment: TsgGDIPPenAlignment): TsgGDIPStatus;
  function GetAlignment: TsgGDIPPenAlignment;
  function SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function ResetTransform: TsgGDIPStatus;
  function MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function TranslateTransform(Dx, Dy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function GetPenType: TsgGDIPPenType;
  function SetColor(Color: TsgColor): TsgGDIPStatus;
  function SetBrush(Brush: TsgGDIPlusBrush): TsgGDIPStatus;
  function GetColor(out Color: TsgColor): TsgGDIPStatus;
  function GetBrush: TsgGDIPlusBrush;
  function GetDashStyle: TsgGDIPDashStyle;
  function SetDashStyle(DashStyle: TsgGDIPDashStyle): TsgGDIPStatus;
  function GetDashOffset: Single;
  function SetDashOffset(DashOffset: Single): TsgGDIPStatus;
  function SetDashPattern(DashArray: PSingle; Count: Integer): TsgGDIPStatus;
  function GetDashPatternCount: Integer;
  function GetDashPattern(DashArray: PSingle; Count: Integer): TsgGDIPStatus;
  function SetCompoundArray(compoundArray: PSingle; Count: Integer): TsgGDIPStatus;
  function GetCompoundArrayCount: Integer;
  function GetCompoundArray(compoundArray: PSingle; Count: Integer): TsgGDIPStatus;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusStringFormat = class(TsgGDIPlusBase)
protected
  NativeFormat: TsgGDIPStringFormat;
  FStatusOfFunction: TsgGDIPStatus;
  function SetStatus(newStatus: TsgGDIPStatus): TsgGDIPStatus;
  procedure Assign(source: TsgGDIPlusStringFormat);
  constructor Create(ClonedStringFormat: TsgGDIPStringFormat; Status: TsgGDIPStatus); reintroduce; overload;
public
  constructor Create(FormatFlags: Integer = 0; Language: LANGID = LANG_NEUTRAL); reintroduce; overload;
  constructor Create(Format: TsgGDIPlusStringFormat); reintroduce; overload;
  destructor Destroy; override;
  class function GenericDefault: TsgGDIPlusStringFormat;
  class function GenericTypographic: TsgGDIPlusStringFormat;
  function Clone: TsgGDIPlusStringFormat;
  function SetFormatFlags(Flags: Integer): TsgGDIPStatus;
  function GetFormatFlags: Integer;
  function SetAlignment(align: TStringAlignment): TsgGDIPStatus;
  function GetAlignment: TStringAlignment;
  function SetLineAlignment(align: TStringAlignment): TsgGDIPStatus;
  function GetLineAlignment: TStringAlignment;
  function SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TsgGDIPStatus;
  function GetHotkeyPrefix: THotkeyPrefix;
  function SetTabStops(firstTabOffset: Single; Count: Integer; TabStops: PSingle): TsgGDIPStatus;
  function GetTabStopCount: Integer;
  function GetTabStops(Count: Integer; FirstTabOffset, TabStops: PSingle): TsgGDIPStatus;
  function SetDigitSubstitution(Language: LANGID; substitute: TStringDigitSubstitute): TsgGDIPStatus;
  function GetDigitSubstitutionLanguage: LANGID;
  function GetDigitSubstitutionMethod: TStringDigitSubstitute;
  function SetTrimming(trimming: TStringTrimming): TsgGDIPStatus;
  function GetTrimming: TStringTrimming;
  function SetMeasurableCharacterRanges(rangeCount: Integer; ranges: PCharacterRange): TsgGDIPStatus;
  function GetMeasurableCharacterRangeCount: Integer;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusGraphicsPath = class(TsgGDIPlusBase)
protected
  NativePath: TsgGDIPPath;
  FStatusOfFunction: TsgGDIPStatus;
  procedure SetNativePath(NativePath: TsgGDIPPath);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  constructor Create(NativePath: TsgGDIPPath); reintroduce; overload;
public
  constructor Create(Path: TsgGDIPlusGraphicsPath); reintroduce; overload;
  constructor Create(FillMode: TsgGDIPFillMode = FillModeAlternate); reintroduce; overload;
  constructor Create(Points: PsgGPointF; Types: PBYTE; Count: Integer;
    FillMode: TsgGDIPFillMode = FillModeAlternate); reintroduce; overload;
  constructor Create(Points: PsgGPoint; Types: PBYTE; Count: Integer;
    FillMode: TsgGDIPFillMode = FillModeAlternate); reintroduce; overload;
  destructor Destroy; override;
  function Clone: TsgGDIPlusGraphicsPath;
  function Reset: TsgGDIPStatus;
  function GetFillMode: TsgGDIPFillMode;
  function SetFillMode(FillMode: TsgGDIPFillMode): TsgGDIPStatus;
  function GetPathData(PathData: TsgGDIPPathData): TsgGDIPStatus;
  function StartFigure: TsgGDIPStatus;
  function CloseFigure: TsgGDIPStatus;
  function CloseAllFigures: TsgGDIPStatus;
  function SetMarker: TsgGDIPStatus;
  function ClearMarkers: TsgGDIPStatus;
  function Reverse: TsgGDIPStatus;
  function GetLastPoint(out lastPoint: TsgGPointF): TsgGDIPStatus;
  function AddLine(const pt1, pt2: TsgGPointF): TsgGDIPStatus; overload;
  function AddLine(X1, Y1, X2, Y2: Single): TsgGDIPStatus; overload;
  function AddLines(Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function AddLine(const pt1, pt2: TsgGPoint): TsgGDIPStatus; overload;
  function AddLine(X1, Y1, X2, Y2: Integer): TsgGDIPStatus; overload;
  function AddLines(Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function AddArc(Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function AddArc(X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function AddArc(Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function AddArc(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function AddBezier(pt1, pt2, pt3, pt4: TsgGPointF): TsgGDIPStatus; overload;
  function AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TsgGDIPStatus; overload;
  function AddBeziers(Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function AddBezier(pt1, pt2, pt3, pt4: TsgGPoint): TsgGDIPStatus; overload;
  function AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TsgGDIPStatus; overload;
  function AddBeziers(Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function AddCurve(Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function AddCurve(Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus; overload;
  function AddCurve(Points: PsgGPointF; Count, offset, NumberOfSegments: Integer; Tension: Single): TsgGDIPStatus; overload;
  function AddCurve(Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function AddCurve(Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus; overload;
  function AddCurve(Points: PsgGPoint; Count, offset, NumberOfSegments: Integer; Tension: Single): TsgGDIPStatus; overload;
  function AddClosedCurve(Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function AddClosedCurve(Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus; overload;
  function AddClosedCurve(Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function AddClosedCurve(Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus; overload;
  function AddRectangle(Rect: TsgGRectF): TsgGDIPStatus; overload;
  function AddRectangles(Rects: PsgGRectF; Count: Integer): TsgGDIPStatus; overload;
  function AddRectangle(Rect: TsgGRect): TsgGDIPStatus; overload;
  function AddRectangles(Rects: PsgGRect; Count: Integer): TsgGDIPStatus; overload;
  function AddEllipse(Rect: TsgGRectF): TsgGDIPStatus; overload;
  function AddEllipse(X, Y, Width, Height: Single): TsgGDIPStatus; overload;
  function AddEllipse(Rect: TsgGRect): TsgGDIPStatus; overload;
  function AddEllipse(X, Y, Width, Height: Integer): TsgGDIPStatus; overload;
  function AddPie(Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function AddPie(X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function AddPie(Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function AddPie(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function AddPolygon(Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function AddPolygon(Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function AddPath(AddingPath: TsgGDIPlusGraphicsPath; Connect: Bool): TsgGDIPStatus;
  function AddString(String_: WideString; Length: Integer; Family: TsgGDIPlusFontFamily;
    Style: Integer; ASize: Single; AOrg: TsgGPointF; Format: TsgGDIPlusStringFormat): TsgGDIPStatus; overload;
  function AddString(String_: WideString; Length: Integer; Family: TsgGDIPlusFontFamily;
    Style: Integer; ASize: Single;const ALayoutRect: TsgGRectF; Format: TsgGDIPlusStringFormat): TsgGDIPStatus; overload;
  function AddString(String_: WideString; Length: Integer; Family: TsgGDIPlusFontFamily;
    Style: Integer; ASize: Single; AOrg: TsgGPoint; Format: TsgGDIPlusStringFormat): TsgGDIPStatus; overload;
  function AddString(String_: WideString; Length: Integer; Family: TsgGDIPlusFontFamily;
    Style: Integer; ASize: Single;const ALayoutRect: TsgGRect; Format: TsgGDIPlusStringFormat): TsgGDIPStatus; overload;
  function Transform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function GetBounds(out Bounds: TsgGRectF; const AMatrix: TsgGDIPlusMatrix = nil; Pen: TsgGDIPlusPen = nil): TsgGDIPStatus; overload;
  function GetBounds(out Bounds: TsgGRect; const AMatrix: TsgGDIPlusMatrix = nil; Pen: TsgGDIPlusPen = nil): TsgGDIPStatus;  overload;
  function Flatten(const AMatrix: TsgGDIPlusMatrix = nil; Flatness: Single = FlatnessDefault): TsgGDIPStatus;
  function Widen(Pen: TsgGDIPlusPen; const AMatrix: TsgGDIPlusMatrix = nil; Flatness: Single = FlatnessDefault): TsgGDIPStatus;
  function Outline(const AMatrix: TsgGDIPlusMatrix = nil; Flatness: Single = FlatnessDefault): TsgGDIPStatus;
  function Warp(DestPoints: PsgGPointF; Count: Integer; SrcRect: TsgGRectF;
    const AMatrix: TsgGDIPlusMatrix = nil; AWarpMode: TWarpMode = WarpModePerspective;
    Flatness: Single = FlatnessDefault): TsgGDIPStatus;
  function GetPointCount: Integer;
  function GetPathTypes(Types: PBYTE; Count: Integer): TsgGDIPStatus;
  function GetPathPoints(Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function GetPathPoints(Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function GetStatusOfFunction: TsgGDIPStatus;
  function IsVisible(point: TsgGPointF; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(X, Y: Single; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(point: TsgGPoint; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsVisible(X, Y: Integer; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsOutlineVisible(point: TsgGPointF; Pen: TsgGDIPlusPen; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsOutlineVisible(X, Y: Single; Pen: TsgGDIPlusPen; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsOutlineVisible(point: TsgGPoint; Pen: TsgGDIPlusPen; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
  function IsOutlineVisible(X, Y: Integer; Pen: TsgGDIPlusPen; Graph: TsgGDIPlusGraphics = nil): BOOL; overload;
end;

TsgGDIPlusGraphicsPathIterator = class(TsgGDIPlusBase)
protected
  NativeIterator: TsgGDIPPathIterator;
  FStatusOfFunction: TsgGDIPStatus;
  procedure SetNativeIterator(NativeIterator: TsgGDIPPathIterator);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
public
  constructor Create(Path: TsgGDIPlusGraphicsPath); reintroduce;
  destructor Destroy; override;
  function NextSubpath(out StartIndex, EndIndex: Integer; out IsClosed: bool): Integer; overload;
  function NextSubpath(Path: TsgGDIPlusGraphicsPath; out IsClosed: BOOL): Integer; overload;
  function NextPathType(out PathType: TPathPointType; out StartIndex, EndIndex: Integer): Integer;
  function NextMarker(out StartIndex, EndIndex: Integer): Integer; overload;
  function NextMarker(Path: TsgGDIPlusGraphicsPath): Integer; overload;
  function GetCount: Integer;
  function GetSubpathCount: Integer;
  function HasCurve: BOOL;
  procedure Rewind;
  function ENumerate(Points: PsgGPointF; Types: PBYTE; Count: Integer): Integer;
  function CopyData(Points: PsgGPointF; Types: PBYTE; StartIndex, EndIndex: Integer): Integer;
  function GetStatusOfFunction: TsgGDIPStatus;
end;

TsgGDIPlusPathGradientBrush = class(TsgGDIPlusBrush)
public
  constructor Create(Points: PsgGPointF; Count: Integer;
    WrapMode: TsgGDIPWrapMode = WrapModeClamp); reintroduce; overload;
  constructor Create(Points: PsgGPoint; Count: Integer;
    WrapMode: TsgGDIPWrapMode = WrapModeClamp); reintroduce; overload;
  constructor Create(Path: TsgGDIPlusGraphicsPath); reintroduce; overload;
  constructor Create; reintroduce; overload;
  function GetCenterColor(out Color: TsgColor): TsgGDIPStatus;
  function SetCenterColor(Color: TsgColor): TsgGDIPStatus;
  function GetPointCount: Integer;
  function GetSurroundColorCount: Integer;
  function GetSurroundColors(Colors: PARGB; var Count: Integer): TsgGDIPStatus;
  function SetSurroundColors(Colors: PARGB; var Count: Integer): TsgGDIPStatus;
  function GetGraphicsPath(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
  function SetGraphicsPath(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
  function GetCenterPoint(out point: TsgGPointF): TsgGDIPStatus; overload;
  function GetCenterPoint(out point: TsgGPoint): TsgGDIPStatus; overload;
  function SetCenterPoint(point: TsgGPointF): TsgGDIPStatus; overload;
  function SetCenterPoint(point: TsgGPoint): TsgGDIPStatus; overload;
  function GetRectangle(out Rect: TsgGRectF): TsgGDIPStatus; overload;
  function GetRectangle(out Rect: TsgGRect): TsgGDIPStatus; overload;
  function SetGammaCorRection(UseGammaCorRection: BOOL): TsgGDIPStatus; overload;
  function GetGammaCorRection: BOOL; overload;
  function GetBlendCount: Integer;
  function GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
  function SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
  function GetInterpolationColorCount: Integer;
  function SetInterpolationColors(APresetColors: PARGB; BlendPositions: PSingle;
    Count: Integer): TsgGDIPStatus;
  function GetInterpolationColors(APresetColors: PARGB;
    BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
  function SetBlendBellShape(Focus: Single; Scale: Single = 1.0): TsgGDIPStatus;
  function SetBlendTriangularShape(Focus: Single; Scale: Single = 1.0): TsgGDIPStatus;
  function GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function ResetTransform: TsgGDIPStatus;
  function MultiplyTransform(const AMatrix: TsgGDIPlusMatrix;
    Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function TranslateTransform(Dx, Dy: Single;
    Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function ScaleTransform(Sx, Sy: Single;
    Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function RotateTransform(Angle: Single;
    Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function GetFocusScales(out XScale, YScale: Single): TsgGDIPStatus;
  function SetFocusScales(XScale, YScale: Single): TsgGDIPStatus;
  function GetWrapMode: TsgGDIPWrapMode;
  function SetWrapMode(WrapMode: TsgGDIPWrapMode): TsgGDIPStatus;
end;

{ TsgGDIPlusGraphics

  Perameter Dummy in constructors for CBuilder support. }
TsgGDIPlusGraphics = class(TsgGDIPlusBase)
protected
  NativeGraphics: TsgGDIPGraphics;
  FStatusOfFunction: TsgGDIPStatus;
  FSrcUnit: TsgGDIPUnit;
  procedure SetNativeGraphics(Graphics: TsgGDIPGraphics);
  function SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
  function GetNativeGraphics: TsgGDIPGraphics;
  function GetNativePen(Pen: TsgGDIPlusPen): TsgGDIPPen;
  constructor Create(Graphics: TsgGDIPGraphics; Dummy: Integer = 0); reintroduce; overload;
public
  function FromHDC(ADC: HDC): TsgGDIPlusGraphics; overload;
  function FromHDC(ADC: HDC; hdevice: THANDLE): TsgGDIPlusGraphics; overload;
  function FromHWND(hwnd: HWND; icm: BOOL = FALSE): TsgGDIPlusGraphics;
  function FromImage(Image: TsgGDIPlusImage): TsgGDIPlusGraphics;
  constructor Create(ADC: HDC); reintroduce; overload;
  constructor Create(ADC: HDC; hdevice: THANDLE); reintroduce; overload;
  constructor Create(hwnd: HWND; icm: BOOL{ = FALSE};
    Dummy: Integer = 0); reintroduce; overload;
  constructor Create(Image: TsgGDIPlusImage); reintroduce; overload;
  destructor Destroy; override;
  procedure Flush(Intention: TsgGDIPFlushIntention = FlushIntentionFlush);
  function GetHDC: HDC;
  procedure ReleaseHDC(ADC: HDC);
  procedure InitParams(const AOptimization: TsgGDIPlusOpimization = opSpeed);
  function SetRenderingOrigin(X, Y: Integer): TsgGDIPStatus;
  function GetRenderingOrigin(out X, Y: Integer): TsgGDIPStatus;
  function SetCompositingMode(CompositingMode: TCompositingMode): TsgGDIPStatus;
  function GetCompositingMode: TCompositingMode;
  function SetCompositingQuality(CompositingQuality: TCompositingQuality): TsgGDIPStatus;
  function GetCompositingQuality: TCompositingQuality;
  function SetTextRenderingHint(newMode: TTextRenderingHint): TsgGDIPStatus;
  function GetTextRenderingHint: TTextRenderingHint;
  function SetTextContrast(Contrast: UINT): TsgGDIPStatus;
  function GetTextContrast: UINT;
  function GetInterpolationMode: TInterpolationMode;
  function SetInterpolationMode(InterpolationMode: TInterpolationMode): TsgGDIPStatus;
  function GetSmoothingMode: TSmoothingMode;
  function SetSmoothingMode(SmoothingMode: TSmoothingMode): TsgGDIPStatus;
  function GetPixelOffsetMode: TPixelOffsetMode;
  function SetPixelOffsetMode(PixelOffsetMode: TPixelOffsetMode): TsgGDIPStatus;
  function SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function ResetTransform: TsgGDIPStatus;
  function MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function TranslateTransform(Dx, Dy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
  function GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function SetPageUnit(Unit_: TsgGDIPUnit): TsgGDIPStatus;
  function SetPageScale(Scale: Single): TsgGDIPStatus;
  function GetPageUnit: TsgGDIPUnit;
  function GetPageScale: Single;
  function GetDpiX: Single;
  function GetDpiY: Single;
  function TransformPoints(DestSpace: TsgGDIPCoordinateSpace; SrcSpace: TsgGDIPCoordinateSpace;
    Pts: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function TransformPoints(DestSpace: TsgGDIPCoordinateSpace; SrcSpace: TsgGDIPCoordinateSpace;
    Pts: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function GetNearestColor(var Color: TsgColor): TsgGDIPStatus;
  function DrawLine(Pen: TsgGDIPlusPen; X1, Y1, X2, Y2: Single): TsgGDIPStatus; overload;
  function DrawLine(Pen: TsgGDIPlusPen; const pt1, pt2: TsgGPointF): TsgGDIPStatus; overload;
  function DrawLines(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function DrawLine(Pen: TsgGDIPlusPen; X1, Y1, X2, Y2: Integer): TsgGDIPStatus; overload;
  function DrawLine(Pen: TsgGDIPlusPen; const pt1, pt2: TsgGPoint): TsgGDIPStatus; overload;
  function DrawLines(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function DrawArc(Pen: TsgGDIPlusPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function DrawArc(Pen: TsgGDIPlusPen; const Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function DrawArc(Pen: TsgGDIPlusPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function DrawArc(Pen: TsgGDIPlusPen; const Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function DrawBezier(Pen: TsgGDIPlusPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TsgGDIPStatus; overload;
  function DrawBezier(Pen: TsgGDIPlusPen; const pt1, pt2, pt3, pt4: TsgGPointF): TsgGDIPStatus; overload;
  function DrawBeziers(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function DrawBezier(Pen: TsgGDIPlusPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TsgGDIPStatus; overload;
  function DrawBezier(Pen: TsgGDIPlusPen; const pt1, pt2, pt3, pt4: TsgGPoint): TsgGDIPStatus; overload;
  function DrawBeziers(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function DrawRectangle(Pen: TsgGDIPlusPen; const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function DrawRectangle(Pen: TsgGDIPlusPen; X, Y, Width, Height: Single): TsgGDIPStatus; overload;
  function DrawRectangles(Pen: TsgGDIPlusPen; Rects: PsgGRectF; Count: Integer): TsgGDIPStatus; overload;
  function DrawRectangle(Pen: TsgGDIPlusPen; const Rect: TsgGRect): TsgGDIPStatus; overload;
  function DrawRectangle(Pen: TsgGDIPlusPen; X, Y, Width, Height: Integer): TsgGDIPStatus; overload;
  function DrawRectangles(Pen: TsgGDIPlusPen; Rects: PsgGRect; Count: Integer): TsgGDIPStatus; overload;
  function DrawEllipse(Pen: TsgGDIPlusPen; const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function DrawEllipse(Pen: TsgGDIPlusPen; X, Y, Width, Height: Single): TsgGDIPStatus; overload;
  function DrawEllipse(Pen: TsgGDIPlusPen; const Rect: TsgGRect): TsgGDIPStatus; overload;
  function DrawEllipse(Pen: TsgGDIPlusPen; X, Y, Width, Height: Integer): TsgGDIPStatus; overload;
  function DrawPie(Pen: TsgGDIPlusPen; const Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function DrawPie(Pen: TsgGDIPlusPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function DrawPie(Pen: TsgGDIPlusPen; const Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function DrawPie(Pen: TsgGDIPlusPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function DrawPolygon(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function DrawPolygon(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function DrawPath(Pen: TsgGDIPlusPen; Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
  function DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus; overload;
  function DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count, offset,
    NumberOfSegments: Integer; Tension: Single = 0.5): TsgGDIPStatus; overload;
  function DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus; overload;
  function DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count, offset, NumberOfSegments: Integer;
    Tension: Single = 0.5): TsgGDIPStatus; overload;
  function DrawClosedCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function DrawClosedCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus; overload;
  function DrawClosedCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint;  Count: Integer): TsgGDIPStatus; overload;
  function DrawClosedCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus; overload;
  function Clear(Color: TsgColor): TsgGDIPStatus;
  function FillRectangle(Brush: TsgGDIPlusBrush; const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function FillRectangle(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Single): TsgGDIPStatus; overload;
  function FillRectangles(Brush: TsgGDIPlusBrush; Rects: PsgGRectF; Count: Integer): TsgGDIPStatus; overload;
  function FillRectangle(Brush: TsgGDIPlusBrush; const Rect: TsgGRect): TsgGDIPStatus; overload;
  function FillRectangle(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Integer): TsgGDIPStatus; overload;
  function FillRectangles(Brush: TsgGDIPlusBrush; Rects: PsgGRect; Count: Integer): TsgGDIPStatus; overload;
  function FillPolygon(Brush: TsgGDIPlusBrush; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function FillPolygon(Brush: TsgGDIPlusBrush; Points: PsgGPointF; Count: Integer; FillMode: TsgGDIPFillMode): TsgGDIPStatus; overload;
  function FillPolygon(Brush: TsgGDIPlusBrush; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function FillPolygon(Brush: TsgGDIPlusBrush; Points: PsgGPoint; Count: Integer; FillMode: TsgGDIPFillMode): TsgGDIPStatus; overload;
  function FillEllipse(Brush: TsgGDIPlusBrush; const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function FillEllipse(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Single): TsgGDIPStatus; overload;
  function FillEllipse(Brush: TsgGDIPlusBrush; const Rect: TsgGRect): TsgGDIPStatus; overload;
  function FillEllipse(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Integer): TsgGDIPStatus; overload;
  function FillPie(Brush: TsgGDIPlusBrush; const Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function FillPie(Brush: TsgGDIPlusBrush; X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function FillPie(Brush: TsgGDIPlusBrush; const Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function FillPie(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus; overload;
  function FillPath(Brush: TsgGDIPlusBrush; Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
  function FillClosedCurve(Brush: TsgGDIPlusBrush; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function FillClosedCurve(Brush: TsgGDIPlusBrush; Points: PsgGPointF; Count: Integer; FillMode: TsgGDIPFillMode; Tension: Single = 0.5 ): TsgGDIPStatus; overload;
  function FillClosedCurve(Brush: TsgGDIPlusBrush; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function FillClosedCurve(Brush: TsgGDIPlusBrush; Points: PsgGPoint; Count: Integer; FillMode: TsgGDIPFillMode; Tension: Single = 0.5): TsgGDIPStatus; overload;
  function FillRegion(Brush: TsgGDIPlusBrush; Region: TsgGDIPlusRegion): TsgGDIPStatus;
  function DrawString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const ALayoutRect: TsgGRectF; stringFormat: TsgGDIPlusStringFormat; Brush: TsgGDIPlusBrush): TsgGDIPStatus; overload;
  function DrawString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const AOrg: TsgGPointF; Brush: TsgGDIPlusBrush): TsgGDIPStatus; overload;
  function DrawString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const AOrg: TsgGPointF; stringFormat: TsgGDIPlusStringFormat; Brush: TsgGDIPlusBrush): TsgGDIPStatus; overload;
  function MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const ALayoutRect: TsgGRectF; stringFormat: TsgGDIPlusStringFormat; out BoundingBox: TsgGRectF;
    codePointsFitted: PInteger = nil; linesFilled: PInteger = nil): TsgGDIPStatus; overload;
  function MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const ALayoutRectSize: TsgGSizeF; stringFormat: TsgGDIPlusStringFormat; out Size: TsgGSizeF;
    codePointsFitted: PInteger = nil; linesFilled: PInteger = nil): TsgGDIPStatus; overload;
  function MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const AOrg: TsgGPointF; stringFormat: TsgGDIPlusStringFormat;
    out BoundingBox: TsgGRectF): TsgGDIPStatus; overload;
  function MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const ALayoutRect: TsgGRectF; out BoundingBox: TsgGRectF): TsgGDIPStatus; overload;
  function MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const AOrg: TsgGPointF; out BoundingBox: TsgGRectF): TsgGDIPStatus; overload;
  function MeasureCharacterRanges(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
    const ALayoutRect: TsgGRectF; stringFormat: TsgGDIPlusStringFormat; RegionCount: Integer;
    const Regions: array of TsgGDIPlusRegion): TsgGDIPStatus; overload;
  function DrawDriverString(text: PUINT16; Length: Integer; Font: TsgGDIPlusFont;
    Brush: TsgGDIPlusBrush; Positions: PsgGPointF; Flags: Integer; const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
  function MeasureDriverString(text: PUINT16; Length: Integer; Font: TsgGDIPlusFont;
     Positions: PsgGPointF; Flags: Integer; const AMatrix: TsgGDIPlusMatrix;
     out BoundingBox: TsgGRectF): TsgGDIPStatus;
  function DrawImage(Image: TsgGDIPlusImage; const point: TsgGPointF): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; X, Y: Single): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; X, Y, Width, Height: Single): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; const point: TsgGPoint): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; X, Y: Integer): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; const Rect: TsgGRect): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; X, Y, Width, Height: Integer): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPointF; Count: Integer): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPoint; Count: Integer): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TsgGDIPUnit): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; const DestRect: TsgGRectF; SrcX, SrcY,
    SrcWidth, SrcHeight: Single; SrcUnit: TsgGDIPUnit; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPointF; Count: Integer;
    SrcX, SrcY, SrcWidth, SrcHeight: Single; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; X, Y, SrcX, SrcY, SrcWidth,
    SrcHeight: Integer; SrcUnit: TsgGDIPUnit): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; const DestRect: TsgGRect; SrcX, SrcY,
    SrcWidth, SrcHeight: Integer; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPoint;
    Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPoint;
  Count, SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit: Integer; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;

  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestPoint: TsgGPointF;
    MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestPoint: TsgGPoint;
     MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestRect: TsgGRectF;
     MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestRect: TsgGRect;
     MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; DestPoints: PsgGPointF;
     Count: Integer; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; DestPoints: PsgGPoint;
     Count: Integer; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestPoint: TsgGPointF;
     const SrcRect: TsgGRectF; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestPoint: TsgGPoint;
     const SrcRect: TsgGRect; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestRect: TsgGRectF;
     const SrcRect: TsgGRectF; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestRect, SrcRect: TsgGRect;
     MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile( MetaFile: TsgGDIPlusMetaFile; DestPoints: PsgGPointF;
      Count: Integer; const SrcRect: TsgGRectF; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; DestPoints: PsgGPoint;
      Count: Integer; const SrcRect: TsgGRect; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus; overload;
  function SetClip(Graph: TsgGDIPlusGraphics; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus; overload;
  function SetClip(Rect: TsgGRectF; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus; overload;
  function SetClip(Rect: TsgGRect; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus; overload;
  function SetClip(Path: TsgGDIPlusGraphicsPath; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus; overload;
  function SetClip(Region: TsgGDIPlusRegion; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus; overload;
  function SetClip(hRgn: HRGN; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus; overload;
  function IntersectClip(const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function IntersectClip(const Rect: TsgGRect): TsgGDIPStatus; overload;
  function IntersectClip(Region: TsgGDIPlusRegion): TsgGDIPStatus; overload;
  function ExcludeClip(const Rect: TsgGRectF): TsgGDIPStatus; overload;
  function ExcludeClip(const Rect: TsgGRect): TsgGDIPStatus; overload;
  function ExcludeClip(Region: TsgGDIPlusRegion): TsgGDIPStatus; overload;
  function ResetClip: TsgGDIPStatus;
  function TranslateClip(Dx, Dy: Single): TsgGDIPStatus; overload;
  function TranslateClip(Dx, Dy: Integer): TsgGDIPStatus; overload;
  function GetClip(Region: TsgGDIPlusRegion): TsgGDIPStatus;
  function GetClipBounds(out Rect: TsgGRectF): TsgGDIPStatus; overload;
  function GetClipBounds(out Rect: TsgGRect): TsgGDIPStatus; overload;
  function IsClipEmpty: Bool;
  function GetVisibleClipBounds(out Rect: TsgGRectF): TsgGDIPStatus; overload;
  function GetVisibleClipBounds(out Rect: TsgGRect): TsgGDIPStatus; overload;
  function IsVisibleClipEmpty: BOOL;
  function IsVisible(X, Y: Integer): BOOL; overload;
  function IsVisible(const point: TsgGPoint): BOOL; overload;
  function IsVisible(X, Y, Width, Height: Integer): BOOL; overload;
  function IsVisible(const Rect: TsgGRect): BOOL; overload;
  function IsVisible(X, Y: Single): BOOL; overload;
  function IsVisible(const point: TsgGPointF): BOOL; overload;
  function IsVisible(X, Y, Width, Height: Single): BOOL; overload;
  function IsVisible(const Rect: TsgGRectF): BOOL; overload;
  function Save: GraphicsState;
  function Restore(gState: GraphicsState): TsgGDIPStatus;
  function BeginContainer(const DstRect,SrcRect: TsgGRectF; Unit_: TsgGDIPUnit): GraphicsContainer; overload;
  function BeginContainer(const DstRect, SrcRect: TsgGRect; Unit_: TsgGDIPUnit): GraphicsContainer; overload;
  function BeginContainer: GraphicsContainer; overload;
  function EndContainer(State: GraphicsContainer): TsgGDIPStatus;
  function AddMetaFileComment(Data: PBYTE; SizeData: UINT): TsgGDIPStatus;
  function GetHalftonePalette: HPALETTE;
  function GetStatusOfFunction: TsgGDIPStatus;
  property SrcUnit: TsgGDIPUnit read FSrcUnit write FSrcUnit;
end;

TsgGDIPlusAdjustableArrowCap = class(TsgGDIPlusCustomLineCap)
public
  constructor Create(Height, Width: Single; isFilled: Bool = True);
  function SetHeight(Height: Single): TsgGDIPStatus;
  function GetHeight: Single;
  function SetWidth(Width: Single): TsgGDIPStatus;
  function GetWidth: Single;
  function SetMiddleInset(MiddleInset: Single): TsgGDIPStatus;
  function GetMiddleInset: Single;
  function SetFillState(isFilled: Bool): TsgGDIPStatus;
  function IsFilled: BOOL;
end;

TsgGDIPlusMetaFile = class(TsgGDIPlusImage)
public
  constructor Create(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
    DeleteWmf: BOOL = FALSE); overload;
  constructor Create(hEmf: HENHMETAFILE; DeleteEmf: BOOL = FALSE); overload;
  constructor Create(FileName: WideString); overload;
  constructor Create(FileName: WideString; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader); overload;
  constructor Create(Stream: IStream); overload;
  constructor Create(ReferenceHdc: HDC; Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create(ReferenceHdc: HDC; FrameRect: TsgGRectF; FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi;
    Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create(ReferenceHdc: HDC; FrameRect: TsgGRect;
    FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi;
    Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create(FileName: WideString;ReferenceHdc: HDC;
    Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create(FileName: WideString; ReferenceHdc: HDC; FrameRect: TsgGRectF;
    FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi;
    Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create( FileName: WideString; ReferenceHdc: HDC; FrameRect: TsgGRect;
    FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi;
    Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create(Stream: IStream; ReferenceHdc: HDC;
    Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create(Stream: IStream; ReferenceHdc: HDC; FrameRect: TsgGRectF;
    FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi;
    Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create(Stream: IStream; ReferenceHdc: HDC; FrameRect: TsgGRect;
   FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi;
   Type_: TEmfType = EmfTypeEmfPlusDual; description: PWCHAR = nil); overload;
  constructor Create; reintroduce; overload;
  function GetMetaFileHeader(hWmf: HMETAFILE; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
    header: TMetaFileHeader): TsgGDIPStatus; overload;
  function GetMetaFileHeader(hEmf: HENHMETAFILE; header: TMetaFileHeader): TsgGDIPStatus; overload;
  function GetMetaFileHeader(FileName: WideString; header: TMetaFileHeader): TsgGDIPStatus; overload;
  function GetMetaFileHeader(Stream: IStream; header: TMetaFileHeader): TsgGDIPStatus; overload;
  function GetMetaFileHeader(header: TMetaFileHeader): TsgGDIPStatus; overload;
  function GetHENHMETAFILE: HENHMETAFILE;
  function PlayRecOrd(RecordType: TEmfPlusRecordType; flags, DataSize: UINT; Data: PBYTE): TsgGDIPStatus;
  function SetDownLevelRasterizationLimit(MetaFileRasterizationLimitDpi: UINT): TsgGDIPStatus;
  function GetDownLevelRasterizationLimit: UINT;
  function EmfToWmfBits(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
    iMapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UINT;
end;

{$IFNDEF SG_WINAPI_GDIPLUS}
  TGPBitmap = TsgGDIPlusBitmap;
  TGPGraphics = TsgGDIPlusGraphics;
  TGPGraphicsPath = TsgGDIPlusGraphicsPath;
  TGPImage = TsgGDIPlusImage;
  TGPImageAttributes = TsgGDIPlusImageAttributes;
  TGPLinearGradientBrush = TsgGDIPlusLinearGradientBrush;
  TGPMatrix = TsgGDIPlusMatrix;
  TGPMetafile = TsgGDIPlusMetaFile;
  TGPPathGradientBrush = TsgGDIPlusPathGradientBrush;
  TGPPen = TsgGDIPlusPen;
  TGPPoint = TsgGPoint;
  TGPPointF = TsgGPointF;
  TGPRect = TsgGRect;
  TGPRectF = TsgGRectF;
  TGPSolidBrush = TsgGDIPlusSolidBrush;
  PGPPoint = PsgGPoint;
{$ENDIF}

{* End classes of GDIPlus ******************************************************}
var
  GDIPlusLoaded: Integer = -1;

procedure DoneGDIPlus;
procedure InitGDIPlus;

//Fucntion gets a CLSID for image encoder
//Format must be
//'image/bmp', 'image/jpeg', 'image/gif', 'image/tiff', 'image/png'

function GetEncoderClsid(const EncoderName: WideString; var AClsid: TGUID): Integer;

implementation

var
  GDIPlusHnd: HModule = 0;

var
  GenericSansSerifFontFamily: TsgGDIPlusFontFamily = nil;
  GenericSerifFontFamily    : TsgGDIPlusFontFamily = nil;
  GenericMonospaceFontFamily: TsgGDIPlusFontFamily = nil;
  GenericTypographicStringFormatBuffer: TsgGDIPlusStringFormat = nil;
  GenericDefaultStringFormatBuffer   : TsgGDIPlusStringFormat = nil;
  StartupInput: TGDIPlusStartupInput;
  StartupOutput: TGDIPlusStartupOutput;
  GDIPlusToken: ULONG = 0;

type
  TsgGDIPlusCreatePath = function (BrushMode: TsgGDIPFillMode; out Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreatePath2 = function (V1: PsgGPointF; V2: PBYTE; V3: Integer; V4: TsgGDIPFillMode; out Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreatePath2I = function (V1: PsgGPoint; V2: PBYTE; V3: Integer; V4: TsgGDIPFillMode; out Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusClonePath = function (Path: TsgGDIPPath;out ClonePath: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeletePath = function (Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetPath = function (Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPointCount = function (Path: TsgGDIPPath; out Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathTypes = function (Path: TsgGDIPPath; types: PBYTE; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathPoints = function (V1: TsgGDIPPath; points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathPointsI = function (V1: TsgGDIPPath; points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathFillMode = function (Path: TsgGDIPPath; var FillMode: TsgGDIPFillMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathFillMode = function (Path: TsgGDIPPath;FillMode: TsgGDIPFillMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathData = function (Path: TsgGDIPPath; PathData: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusStartPathFigure = function (Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusClosePathFigure = function (Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusClosePathFigures = function (Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathMarker = function (Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusClearPathMarkers = function (Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusReversePath = function (Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathLastPoint = function (Path: TsgGDIPPath; lastPoint: PsgGPointF): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathLine = function (Path: TsgGDIPPath; X1, Y1, X2, Y2: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathLine2 = function (Path: TsgGDIPPath; points: PsgGPointF;Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathArc = function (Path: TsgGDIPPath; X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathBezier = function (Path: TsgGDIPPath; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathBeziers = function (Path: TsgGDIPPath; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathCurve = function (Path: TsgGDIPPath; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathCurve2 = function (Path: TsgGDIPPath; Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathCurve3 = function (Path: TsgGDIPPath; Points: PsgGPointF; Count: Integer; Offset: Integer; NumberOfSegments: Integer;
    Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathClosedCurve = function (Path: TsgGDIPPath; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathClosedCurve2 = function (Path: TsgGDIPPath; Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathRectangle = function (Path: TsgGDIPPath; X, Y, Width, Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathRectangles = function (Path: TsgGDIPPath; Rects: PsgGRectF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathEllipse = function (Path: TsgGDIPPath; X, Y, Width, Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathPie = function (Path: TsgGDIPPath; X, Y, Width, Height: Single; StartAngle: Single; SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathPolygon = function (Path: TsgGDIPPath; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathPath = function (Path: TsgGDIPPath; AddingPath: TsgGDIPPath; connect: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathString = function (Path: TsgGDIPPath; String_: PWCHAR; Length: Integer; Family: TsgGDIPFontFamily; style: Integer; ASize: Single; layoutRect: PsgGRectF;
    Format: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathStringI = function (Path: TsgGDIPPath; String_: PWCHAR; Length: Integer; Family: TsgGDIPFontFamily; style: Integer; ASize: Single; layoutRect: PsgGRect;
    Format: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathLineI = function (Path: TsgGDIPPath; X1, Y1, X2, Y2: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathLine2I = function (Path: TsgGDIPPath; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathArcI = function (Path: TsgGDIPPath; X: Integer; Y: Integer; Width: Integer;
    Height: Integer; StartAngle: Single; SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathBezierI = function (Path: TsgGDIPPath; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathBeziersI = function (Path: TsgGDIPPath; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathCurveI = function (Path: TsgGDIPPath; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathCurve2I = function (Path: TsgGDIPPath; Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathCurve3I = function (Path: TsgGDIPPath; Points: PsgGPoint; Count: Integer; Offset: Integer; NumberOfSegments: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathClosedCurveI = function (Path: TsgGDIPPath; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathClosedCurve2I = function (Path: TsgGDIPPath; Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathRectangleI = function (Path: TsgGDIPPath; X, Y, Width, Height: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathRectanglesI = function (Path: TsgGDIPPath; Rects: PsgGRect; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathEllipseI = function (Path: TsgGDIPPath; X, Y, Width, Height: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathPieI = function (Path: TsgGDIPPath; X, Y, Width, Height: Integer; StartAngle: Single; SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusAddPathPolygonI = function (Path: TsgGDIPPath; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFlattenPath = function (Path: TsgGDIPPath; Matrix: TsgGDIPMatrix; flatness: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusWindingModeOutline = function (Path: TsgGDIPPath; Matrix: TsgGDIPMatrix; flatness: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusWidenPath = function (NativePath: TsgGDIPPath; pen: TsgGDIPPen; Matrix: TsgGDIPMatrix; flatness: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusWarpPath = function (Path: TsgGDIPPath; Matrix: TsgGDIPMatrix; Points: PsgGPointF; Count: Integer;
    SrcX, SrcY, SrcWidth, SrcHeight: Single; WarpMode: WARPMODE; Flatness: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusTransformPath = function (Path: TsgGDIPPath; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathWorldBounds = function (Path: TsgGDIPPath; Bounds: PsgGRectF; Matrix: TsgGDIPMatrix; Pen: TsgGDIPPen): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathWorldBoundsI = function (Path: TsgGDIPPath; Bounds: PsgGRect; Matrix: TsgGDIPMatrix; Pen: TsgGDIPPen): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisiblePathPoint = function (Path: TsgGDIPPath; X, Y: Single; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisiblePathPointI = function (Path: TsgGDIPPath; X, Y: Integer; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsOutlineVisiblePathPoint = function (Path: TsgGDIPPath; X, Y: Single; Pen: TsgGDIPPen; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsOutlineVisiblePathPointI = function (Path: TsgGDIPPath; X, Y: Integer; Pen: TsgGDIPPen; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
//Path API
  TsgGDIPlusCreatePathIter = function (out Iterator: TsgGDIPPathIterator; Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeletePathIter = function (Iterator: TsgGDIPPathIterator): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterNextSubpath = function (Iterator: TsgGDIPPathIterator; var ResultCount: Integer; var StartIndex: Integer; var EndIndex: Integer;
    out IsClosed: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterNextSubpathPath = function (Iterator: TsgGDIPPathIterator; var ResultCount: Integer; Path: TsgGDIPPath; out IsClosed: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterNextPathType = function (Iterator: TsgGDIPPathIterator; var ResultCount: Integer; PathType: PBYTE; var StartIndex: Integer;
    var EndIndex: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterNextMarker = function (Iterator: TsgGDIPPathIterator; var ResultCount: Integer; var StartIndex: Integer;
    var EndIndex: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterNextMarkerPath = function (Iterator: TsgGDIPPathIterator; var ResultCount: Integer; Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterGetCount = function (Iterator: TsgGDIPPathIterator; out Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterGetSubpathCount = function (Iterator: TsgGDIPPathIterator; out Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterIsValid = function (Iterator: TsgGDIPPathIterator; out Valid: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterHasCurve = function (Iterator: TsgGDIPPathIterator; out HasCurve: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterRewind = function (Iterator: TsgGDIPPathIterator): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterEnumerate = function (Iterator: TsgGDIPPathIterator; var ResultCount: Integer; Points: PsgGPointF; Types: PBYTE;
   Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusPathIterCopyData = function (Iterator: TsgGDIPPathIterator; var ResultCount: Integer; Points: PsgGPointF; Types: PBYTE;
    StartIndex: Integer; EndIndex: Integer): TsgGDIPStatus; stdcall;
//Matrix API
  TsgGDIPlusCreateMatrix = function (out Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateMatrix2 = function (M11, M12, M21, M22: Single; Dx, Dy: Single; out Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateMatrix3 = function (Rect: PsgGRectF; DstPlg: PsgGPointF; out Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateMatrix3I = function (Rect: PsgGRect; DstPlg: PsgGPoint; out Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneMatrix = function (Matrix: TsgGDIPMatrix; out CloneMatrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeleteMatrix = function (Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetMatrixElements = function (Matrix: TsgGDIPMatrix; M11, M12, M21, M22: Single; Dx, Dy: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusMultiplyMatrix = function (Matrix: TsgGDIPMatrix; Matrix2: TsgGDIPMatrix; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslateMatrix = function (Matrix: TsgGDIPMatrix; OffsetX, OffsetY: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusScaleMatrix = function (Matrix: TsgGDIPMatrix; ScaleX, ScaleY: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusRotateMatrix = function (Matrix: TsgGDIPMatrix; Angle: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusShearMatrix = function (Matrix: TsgGDIPMatrix; ShearX, ShearY: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusInvertMatrix = function (Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusTransformMatrixPoints = function (Matrix: TsgGDIPMatrix; Pts: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusTransformMatrixPointsI = function (Matrix: TsgGDIPMatrix; Pts: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusVectorTransformMatrixPoints = function (Matrix: TsgGDIPMatrix; Pts: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusVectorTransformMatrixPointsI = function (Matrix: TsgGDIPMatrix; Pts: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetMatrixElements = function (Matrix: TsgGDIPMatrix; MatrixOut: PSingle): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsMatrixInvertible = function (Matrix: TsgGDIPMatrix; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsMatrixIdentity = function (Matrix: TsgGDIPMatrix; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsMatrixEqual = function (Matrix: TsgGDIPMatrix; Matrix2: TsgGDIPMatrix; out Result: Bool): TsgGDIPStatus; stdcall;
//Region API
  TsgGDIPlusCreateRegion = function (out Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateRegionRect = function (Rect: PsgGRectF; out Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateRegionRectI = function (Rect: PsgGRect; out Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateRegionPath = function (Path: TsgGDIPPath; out Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateRegionRgnData = function (RegionData: PBYTE; Size: Integer; out Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateRegionHrgn = function (hRgn: HRGN; out Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneRegion = function (Region: TsgGDIPRegion; out CloneRegion: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeleteRegion = function (Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetInfinite = function (Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetEmpty = function (Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusCombineRegionRect = function (Region: TsgGDIPRegion; Rect: PsgGRectF; CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusCombineRegionRectI = function (Region: TsgGDIPRegion; Rect: PsgGRect; CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusCombineRegionPath = function (Region: TsgGDIPRegion; Path: TsgGDIPPath;CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusCombineRegionRegion = function (Region: TsgGDIPRegion; Region2: TsgGDIPRegion; CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslateRegion = function (Region: TsgGDIPRegion; Dx, Dy: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslateRegionI = function (Region: TsgGDIPRegion; Dx: Integer; Dy: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusTransformRegion = function (Region: TsgGDIPRegion; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRegionBounds = function (Region: TsgGDIPRegion; Graphics: TsgGDIPGraphics; Rect: PsgGRectF): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRegionBoundsI = function (Region: TsgGDIPRegion; Graphics: TsgGDIPGraphics; Rect: PsgGRect): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRegionHRgn = function (Region: TsgGDIPRegion; Graphics: TsgGDIPGraphics; out hRgn: HRGN): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsEmptyRegion = function (Region: TsgGDIPRegion; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsInfiniteRegion = function (Region: TsgGDIPRegion; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsEqualRegion = function (Region: TsgGDIPRegion; Region2: TsgGDIPRegion; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRegionDataSize = function (Region: TsgGDIPRegion; out BufferSize: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRegionData = function (Region: TsgGDIPRegion; buffer: PBYTE; BufferSize: UINT; SizeFilled: PUINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisibleRegionPoint = function (Region: TsgGDIPRegion; X, Y: Single; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisibleRegionPointI = function (Region: TsgGDIPRegion; X, Y: Integer; Graphics: TsgGDIPGraphics; out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisibleRegionRect = function (Region: TsgGDIPRegion; X, Y, Width, Height: Single; Graphics: TsgGDIPGraphics;
    out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisibleRegionRectI = function (Region: TsgGDIPRegion; X, Y, Width, Height: Integer; Graphics: TsgGDIPGraphics;
    out Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRegionScansCount =  function (Region: TsgGDIPRegion; out Count: UINT; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRegionScans = function (Region: TsgGDIPRegion; Rects: PsgGRectF; out Count: Integer; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRegionScansI = function (Region: TsgGDIPRegion; Rects: PsgGRect; out Count: Integer; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
//Brush API
  TsgGDIPlusCloneBrush = function (Brush: TsgGDIPBrush; out CloneBrush: TsgGDIPBrush): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeleteBrush = function (Brush: TsgGDIPBrush): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetBrushType = function (Brush: TsgGDIPBrush; out Type_: TsgGDIPBrushTYPE): TsgGDIPStatus; stdcall;
//HatchBrush API
  TsgGDIPlusCreateHatchBrush = function (HatchStyle: Integer; ForeCol: ARGB; backcol: ARGB; out Brush: TsgGDIPHatch): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetHatchStyle = function (Brush: TsgGDIPHatch; out HatchStyle: TsgGDIPHatchSTYLE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetHatchForegroundColor = function (Brush: TsgGDIPHatch; out ForeCol: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetHatchBackgroundColor = function (Brush: TsgGDIPHatch; out BackCol: ARGB): TsgGDIPStatus; stdcall;
//TextureBrush API
  TsgGDIPlusCreateTexture = function (Image: TsgGDIPImage; WrapMode: TsgGDIPWrapMode; var Texture: TsgGDIPTexture): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateTexture2 = function (Image: TsgGDIPImage; WrapMode: TsgGDIPWrapMode; X, Y, Width, Height: Single;
    out Texture: TsgGDIPTexture): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateTextureIA = function (Image: TsgGDIPImage; ImageAttributes: TsgGDIPImageAttributes; X, Y: Single; Width: Single;
    Height: Single; out Texture: TsgGDIPTexture): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateTexture2I = function (Image: TsgGDIPImage; WrapMode: TsgGDIPWrapMode; X, Y, Width, Height: Integer;
    out Texture: TsgGDIPTexture): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateTextureIAI = function (Image: TsgGDIPImage; ImageAttributes: TsgGDIPImageAttributes; X, Y: Integer; Width: Integer;
    Height: Integer; out Texture: TsgGDIPTexture): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetTextureTransform = function (Brush: TsgGDIPTexture; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetTextureTransform = function (Brush: TsgGDIPTexture; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetTextureTransform = function (Brush: TsgGDIPTexture): TsgGDIPStatus; stdcall;
  TsgGDIPlusMultiplyTextureTransform = function (Brush: TsgGDIPTexture; Matrix: TsgGDIPMatrix; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslateTextureTransform = function (Brush: TsgGDIPTexture; Dx, Dy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusScaleTextureTransform = function (Brush: TsgGDIPTexture; Sx, Sy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusRotateTextureTransform = function (Brush: TsgGDIPTexture; Angle: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetTextureWrapMode = function (Brush: TsgGDIPTexture; WrapMode: TsgGDIPWrapMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetTextureWrapMode = function (Brush: TsgGDIPTexture; var WrapMode: TsgGDIPWrapMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetTextureImage = function (Brush: TsgGDIPTexture; out Image: TsgGDIPImage): TsgGDIPStatus; stdcall;
//SolidBrush API
  TsgGDIPlusCreateSolidFill = function (Color: ARGB; out Brush: TsgGDIPSolidFill): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetSolidFillColor = function (Brush: TsgGDIPSolidFill; Color: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetSolidFillColor = function (Brush: TsgGDIPSolidFill; out Color: ARGB): TsgGDIPStatus; stdcall;
//LineBrush API
  TsgGDIPlusCreateLineBrush = function (point1: PsgGPointF; point2: PsgGPointF; Color1: ARGB; Color2: ARGB; WrapMode: TsgGDIPWrapMode;
    out LineGradient: TsgGDIPLineGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateLineBrushI = function (point1: PsgGPoint; point2: PsgGPoint; Color1: ARGB; Color2: ARGB; WrapMode: TsgGDIPWrapMode;
    out LineGradient: TsgGDIPLineGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateLineBrushFromRect = function (Rect: PsgGRectF; Color1: ARGB; Color2: ARGB; mode: LINEARGRADIENTMODE; WrapMode: TsgGDIPWrapMode;
    out LineGradient: TsgGDIPLineGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateLineBrushFromRectI = function (Rect: PsgGRect; Color1: ARGB; Color2: ARGB; mode: LINEARGRADIENTMODE; WrapMode: TsgGDIPWrapMode;
    out LineGradient: TsgGDIPLineGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateLineBrushFromRectWithAngle = function (Rect: PsgGRectF; Color1: ARGB; Color2: ARGB; Angle: Single; isAngleScalable: Bool; WrapMode: TsgGDIPWrapMode;
    out LineGradient: TsgGDIPLineGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateLineBrushFromRectWithAngleI = function (Rect: PsgGRect; Color1: ARGB; Color2: ARGB; Angle: Single; isAngleScalable: Bool; WrapMode: TsgGDIPWrapMode;
    out LineGradient: TsgGDIPLineGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetLineColors = function (Brush: TsgGDIPLineGradient; Color1: ARGB; Color2: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineColors = function (Brush: TsgGDIPLineGradient; Colors: PARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineRect = function (Brush: TsgGDIPLineGradient; Rect: PsgGRectF): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineRectI = function (Brush: TsgGDIPLineGradient; Rect: PsgGRect): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetLineGammaCorRection = function (Brush: TsgGDIPLineGradient; UseGammaCorRection: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineGammaCorRection = function (Brush: TsgGDIPLineGradient; out UseGammaCorRection: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineBlendCount = function (Brush: TsgGDIPLineGradient; out Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineBlend = function (Brush: TsgGDIPLineGradient; blend: PSingle; positions: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetLineBlend = function (Brush: TsgGDIPLineGradient; blend: PSingle; positions: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLinePresetBlendCount = function (Brush: TsgGDIPLineGradient; out Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLinePresetBlend = function (Brush: TsgGDIPLineGradient; blend: PARGB; positions: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetLinePresetBlend = function (Brush: TsgGDIPLineGradient; blend: PARGB; positions: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetLineSigmaBlend = function (Brush: TsgGDIPLineGradient; focus: Single; scale: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetLineLinearBlend = function (Brush: TsgGDIPLineGradient; focus: Single; scale: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetLineWrapMode = function (Brush: TsgGDIPLineGradient; WrapMode: TsgGDIPWrapMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineWrapMode = function (Brush: TsgGDIPLineGradient; out WrapMode: TsgGDIPWrapMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineTransform = function (Brush: TsgGDIPLineGradient; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetLineTransform = function (Brush: TsgGDIPLineGradient; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetLineTransform = function (Brush: TsgGDIPLineGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusMultiplyLineTransform = function (Brush: TsgGDIPLineGradient; Matrix: TsgGDIPMatrix; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslateLineTransform = function (Brush: TsgGDIPLineGradient; Dx, Dy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusScaleLineTransform = function (Brush: TsgGDIPLineGradient; Sx, Sy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusRotateLineTransform = function (Brush: TsgGDIPLineGradient; Angle: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
//PathGradientBrush API
  TsgGDIPlusCreatePathGradient = function (Points: PsgGPointF; Count: Integer; WrapMode: TsgGDIPWrapMode; out polyGradient: TsgGDIPPathGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreatePathGradientI = function (Points: PsgGPoint; Count: Integer; WrapMode: TsgGDIPWrapMode; out polyGradient: TsgGDIPPathGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreatePathGradientFromPath = function (Path: TsgGDIPPath; out polyGradient: TsgGDIPPathGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientCenterColor = function (Brush: TsgGDIPPathGradient; out Colors: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientCenterColor = function (Brush: TsgGDIPPathGradient; Colors: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientSurroundColorsWithCount = function (Brush: TsgGDIPPathGradient; Color: PARGB; var Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientSurroundColorsWithCount = function (Brush: TsgGDIPPathGradient; Color: PARGB; var Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientPath = function (Brush: TsgGDIPPathGradient; Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientPath = function (Brush: TsgGDIPPathGradient; Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientCenterPoint = function (Brush: TsgGDIPPathGradient; Points: PsgGPointF): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientCenterPointI = function (Brush: TsgGDIPPathGradient; Points: PsgGPoint): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientCenterPoint = function (Brush: TsgGDIPPathGradient; Points: PsgGPointF): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientCenterPointI = function (Brush: TsgGDIPPathGradient; Points: PsgGPoint): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientRect = function (Brush: TsgGDIPPathGradient; Rect: PsgGRectF): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientRectI = function (Brush: TsgGDIPPathGradient; Rect: PsgGRect): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientPointCount = function (Brush: TsgGDIPPathGradient;var Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientSurroundColorCount = function (Brush: TsgGDIPPathGradient; var Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientGammaCorRection = function (Brush: TsgGDIPPathGradient; UseGammaCorRection: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientGammaCorRection = function (Brush: TsgGDIPPathGradient; var UseGammaCorRection: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientBlendCount = function (Brush: TsgGDIPPathGradient; var Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientBlend = function (Brush: TsgGDIPPathGradient; Blend: PSingle; Positions: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientBlend = function (Brush: TsgGDIPPathGradient; Blend: PSingle; Positions: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientPresetBlendCount = function (Brush: TsgGDIPPathGradient; var Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientPresetBlend = function (Brush: TsgGDIPPathGradient; Blend: PARGB; Positions: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientPresetBlend = function (Brush: TsgGDIPPathGradient; Blend: PARGB; Positions: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientSigmaBlend = function (Brush: TsgGDIPPathGradient; Focus: Single; Scale: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientLinearBlend = function (Brush: TsgGDIPPathGradient; Focus: Single; Scale: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientWrapMode = function (Brush: TsgGDIPPathGradient; var WrapMode: TsgGDIPWrapMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientWrapMode = function (Brush: TsgGDIPPathGradient; WrapMode: TsgGDIPWrapMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientTransform = function (Brush: TsgGDIPPathGradient; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientTransform = function (Brush: TsgGDIPPathGradient; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetPathGradientTransform = function (Brush: TsgGDIPPathGradient): TsgGDIPStatus; stdcall;
  TsgGDIPlusMultiplyPathGradientTransform = function (Brush: TsgGDIPPathGradient; Matrix: TsgGDIPMatrix; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslatePathGradientTransform = function (Brush: TsgGDIPPathGradient; Dx, Dy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusScalePathGradientTransform = function (Brush: TsgGDIPPathGradient; Sx, Sy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusRotatePathGradientTransform = function (Brush: TsgGDIPPathGradient; Angle: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPathGradientFocusScales = function (Brush: TsgGDIPPathGradient; var XScale: Single; var YScale: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPathGradientFocusScales = function (Brush: TsgGDIPPathGradient; XScale, YScale: Single): TsgGDIPStatus; stdcall;
//Pen API
  TsgGDIPlusCreatePen1 = function (Color: ARGB; Width: Single; Unit_: TsgGDIPUnit; out Pen: TsgGDIPPen): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreatePen2 = function (Brush: TsgGDIPBrush; Width: Single; Unit_: TsgGDIPUnit; out Pen: TsgGDIPPen): TsgGDIPStatus; stdcall;
  TsgGDIPlusClonePen = function (Pen: TsgGDIPPen; out ClonePen: TsgGDIPPen): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeletePen = function (Pen: TsgGDIPPen): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenWidth = function (Pen: TsgGDIPPen; Width: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenWidth = function (Pen: TsgGDIPPen; out Width: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenUnit = function (Pen: TsgGDIPPen; Unit_: TsgGDIPUnit): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenUnit = function (Pen: TsgGDIPPen; var Unit_: TsgGDIPUnit): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenLineCap197819 = function (Pen: TsgGDIPPen; StartCap: TsgGDIPLineCap; EndCap: TsgGDIPLineCap; DashCap: TsgGDIPDashCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenStartCap = function (Pen: TsgGDIPPen; StartCap: TsgGDIPLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenEndCap = function (Pen: TsgGDIPPen; EndCap: TsgGDIPLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenDashCap197819 = function (Pen: TsgGDIPPen; DashCap: TsgGDIPDashCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenStartCap = function (Pen: TsgGDIPPen; out StartCap: TsgGDIPLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenEndCap = function (Pen: TsgGDIPPen; out EndCap: TsgGDIPLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenDashCap197819 = function (Pen: TsgGDIPPen; out DashCap: TsgGDIPDashCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenLineJoin = function (Pen: TsgGDIPPen; LineJoin: TsgGDIPLineJoin): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenLineJoin = function (Pen: TsgGDIPPen; var LineJoin: TsgGDIPLineJoin): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenCustomStartCap = function (Pen: TsgGDIPPen; CustomCap: TsgGDIPCustomLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenCustomStartCap = function (Pen: TsgGDIPPen; out CustomCap: TsgGDIPCustomLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenCustomEndCap = function (Pen: TsgGDIPPen; CustomCap: TsgGDIPCustomLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenCustomEndCap = function (Pen: TsgGDIPPen; out CustomCap: TsgGDIPCustomLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenMiterLimit = function (Pen: TsgGDIPPen; MiterLimit: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenMiterLimit = function (Pen: TsgGDIPPen; out MiterLimit: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenMode = function (Pen: TsgGDIPPen; PenMode: TsgGDIPPenALIGNMENT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenMode = function (Pen: TsgGDIPPen; var PenMode: TsgGDIPPenALIGNMENT): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenTransform = function (Pen: TsgGDIPPen; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenTransform = function (Pen: TsgGDIPPen; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetPenTransform = function (Pen: TsgGDIPPen): TsgGDIPStatus; stdcall;
  TsgGDIPlusMultiplyPenTransform = function (Pen: TsgGDIPPen; Matrix: TsgGDIPMatrix; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslatePenTransform = function (Pen: TsgGDIPPen; Dx, Dy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusScalePenTransform = function (Pen: TsgGDIPPen; Sx, Sy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusRotatePenTransform = function (Pen: TsgGDIPPen; Angle: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenColor = function (Pen: TsgGDIPPen; ARGB: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenColor = function (Pen: TsgGDIPPen; out ARGB: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenBrushFill = function (Pen: TsgGDIPPen; Brush: TsgGDIPBrush): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenBrushFill = function (Pen: TsgGDIPPen; out Brush: TsgGDIPBrush): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenFillType = function (Pen: TsgGDIPPen; out Type_: TsgGDIPPenTYPE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenDashStyle = function (Pen: TsgGDIPPen; out DashStyle: TsgGDIPDashStyle): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenDashStyle = function (Pen: TsgGDIPPen; DashStyle: TsgGDIPDashStyle): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenDashOffset = function (Pen: TsgGDIPPen; out Offset: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenDashOffset = function (Pen: TsgGDIPPen; Offset: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenDashCount = function (Pen: TsgGDIPPen; var Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenDashArray = function (Pen: TsgGDIPPen; Dash: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenDashArray = function (Pen: TsgGDIPPen; Dash: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenCompoundCount = function (Pen: TsgGDIPPen; out Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPenCompoundArray = function (Pen: TsgGDIPPen; Dash: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPenCompoundArray = function (Pen: TsgGDIPPen; Dash: PSingle; Count: Integer): TsgGDIPStatus; stdcall;
//CustomLineCap API
  TsgGDIPlusCreateCustomLineCap = function (fillPath: TsgGDIPPath; StrokePath: TsgGDIPPath; baseCap: TsgGDIPLineCap; baseInset: Single;
    out CustomCap: TsgGDIPCustomLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeleteCustomLineCap = function (CustomCap: TsgGDIPCustomLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneCustomLineCap = function (CustomCap: TsgGDIPCustomLineCap; out ClonedCap: TsgGDIPCustomLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCustomLineCapType = function (CustomCap: TsgGDIPCustomLineCap; var capType: CUSTOMLINECAPTYPE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetCustomLineCapStrokeCaps = function (CustomCap: TsgGDIPCustomLineCap; StartCap: TsgGDIPLineCap; EndCap: TsgGDIPLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCustomLineCapStrokeCaps = function (CustomCap: TsgGDIPCustomLineCap; var StartCap: TsgGDIPLineCap; var EndCap: TsgGDIPLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetCustomLineCapStrokeJoin = function (CustomCap: TsgGDIPCustomLineCap; LineJoin: TsgGDIPLineJoin): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCustomLineCapStrokeJoin = function (CustomCap: TsgGDIPCustomLineCap; var LineJoin: TsgGDIPLineJoin): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetCustomLineCapBaseCap = function (CustomCap: TsgGDIPCustomLineCap; BaseCap: TsgGDIPLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCustomLineCapBaseCap = function (CustomCap: TsgGDIPCustomLineCap; var BaseCap: TsgGDIPLineCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetCustomLineCapBaseInset = function (CustomCap: TsgGDIPCustomLineCap; Inset: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCustomLineCapBaseInset = function (CustomCap: TsgGDIPCustomLineCap; var Inset: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetCustomLineCapWidthScale = function (CustomCap: TsgGDIPCustomLineCap; WidthScale: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCustomLineCapWidthScale = function (CustomCap: TsgGDIPCustomLineCap; var WidthScale: Single): TsgGDIPStatus; stdcall;
// AdjustableArrowCap API
  TsgGDIPlusCreateAdjustableArrowCap = function (Height: Single; Width: Single; isFilled: Bool; out cap: TsgGDIPAdjustableArrowCap): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetAdjustableArrowCapHeight = function (Cap: TsgGDIPAdjustableArrowCap; Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetAdjustableArrowCapHeight = function (Cap: TsgGDIPAdjustableArrowCap; var Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetAdjustableArrowCapWidth = function (Cap: TsgGDIPAdjustableArrowCap; Width: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetAdjustableArrowCapWidth = function (Cap: TsgGDIPAdjustableArrowCap; var Width: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetAdjustableArrowCapMiddleInset = function (Cap: TsgGDIPAdjustableArrowCap; MiddleInset: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetAdjustableArrowCapMiddleInset = function (Cap: TsgGDIPAdjustableArrowCap; var MiddleInset: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetAdjustableArrowCapFillState = function (Cap: TsgGDIPAdjustableArrowCap; FillState: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetAdjustableArrowCapFillState = function (Cap: TsgGDIPAdjustableArrowCap; var FillState: Bool): TsgGDIPStatus; stdcall;
// Image API
  TsgGDIPlusLoadImageFromStream = function (Stream: ISTREAM; out Image: TsgGDIPImage): TsgGDIPStatus; stdcall;
  TsgGDIPlusLoadImageFromFile = function (FileName: PWCHAR; out Image: TsgGDIPImage): TsgGDIPStatus; stdcall;
  TsgGDIPlusLoadImageFromStreamICM = function (Stream: ISTREAM; out Image: TsgGDIPImage): TsgGDIPStatus; stdcall;
  TsgGDIPlusLoadImageFromFileICM = function (FileName: PWCHAR; out Image: TsgGDIPImage): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneImage = function (Image: TsgGDIPImage; out CloneImage: TsgGDIPImage): TsgGDIPStatus; stdcall;
  TsgGDIPlusDisposeImage = function (Image: TsgGDIPImage): TsgGDIPStatus; stdcall;
  TsgGDIPlusSaveImageToFile = function (Image: TsgGDIPImage; FileName: PWCHAR; ClsIdEncoder: PGUID; encoderParams: PENCODERPARAMETERS): TsgGDIPStatus; stdcall;
  TsgGDIPlusSaveImageToStream = function (Image: TsgGDIPImage; Stream: ISTREAM; ClsIdEncoder: PGUID; encoderParams: PENCODERPARAMETERS): TsgGDIPStatus; stdcall;
  TsgGDIPlusSaveAdd = function (Image: TsgGDIPImage; EncoderParams: PENCODERPARAMETERS): TsgGDIPStatus; stdcall;
  TsgGDIPlusSaveAddImage = function (Image: TsgGDIPImage; newImage: TsgGDIPImage; EncoderParams: PENCODERPARAMETERS): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageGraphicsContext = function (Image: TsgGDIPImage; out Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageBounds = function (Image: TsgGDIPImage; SrcRect: PsgGRectF; var SrcUnit: TsgGDIPUnit): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageDimension = function (Image: TsgGDIPImage; var Width: Single; var Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageType = function (Image: TsgGDIPImage; var Type_: IMAGETYPE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageWidth = function (Image: TsgGDIPImage; var Width: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageHeight = function (Image: TsgGDIPImage; var Height: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageHorizontalResolution = function (Image: TsgGDIPImage; var Resolution: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageVerticalResolution = function (Image: TsgGDIPImage; var Resolution: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageFlags = function (Image: TsgGDIPImage; var Flags: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageRawFormat = function (Image: TsgGDIPImage; Format: PGUID): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImagePixelFormat = function (Image: TsgGDIPImage; out Format: TPIXELFORMAT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageThumbnail = function (Image: TsgGDIPImage; ThumbWidth: UINT; ThumbHeight: UINT; out ThumbImage: TsgGDIPImage;
    callback: GETTHUMBNAILIMAGEABORT; CallBackData: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetEncoderParameterListSize = function (Image: TsgGDIPImage; ClsIdEncoder: PGUID; out Size: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetEncoderParameterList = function (Image: TsgGDIPImage; ClsIdEncoder: PGUID; Size: UINT; buffer: PENCODERPARAMETERS): TsgGDIPStatus; stdcall;
  TsgGDIPlusImageGetFrameDimensionsCount = function (Image: TsgGDIPImage; var Count: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusImageGetFrameDimensionsList = function (Image: TsgGDIPImage; DimensionIDs: PGUID; Count: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusImageGetFrameCount = function (Image: TsgGDIPImage; DimensionID: PGUID; var Count: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusImageSelectActiveFrame = function (Image: TsgGDIPImage; DimensionID: PGUID; FrameIndex: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusImageRotateFlip = function (Image: TsgGDIPImage; rfType: ROTATEFLIPTYPE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImagePalette = function (Image: TsgGDIPImage; Palette: PCOLORPALETTE; Size: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImagePalette = function (Image: TsgGDIPImage; Palette: PCOLORPALETTE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImagePaletteSize = function (Image: TsgGDIPImage; var Size: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPropertyCount = function (Image: TsgGDIPImage; var NumOfProperty: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPropertyIdList = function (Image: TsgGDIPImage; NumOfProperty: UINT; List: PPROPID): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPropertyItemSize = function (Image: TsgGDIPImage; PropId: PROPID; var Size: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPropertyItem = function (Image: TsgGDIPImage; PropId: PROPID; PropSize: UINT; Buffer: PPROPERTYITEM): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPropertySize = function (Image: TsgGDIPImage; var TotalBufferSize: UINT; var NumProperties: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetAllPropertyItems = function (Image: TsgGDIPImage; TotalBufferSize: UINT; NumProperties: UINT; AllItems: PPROPERTYITEM): TsgGDIPStatus; stdcall;
  TsgGDIPlusRemovePropertyItem = function (Image: TsgGDIPImage; PropId: PROPID): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPropertyItem = function (Image: TsgGDIPImage; Item: PPROPERTYITEM): TsgGDIPStatus; stdcall;
  TsgGDIPlusImageForceValidation = function (Image: TsgGDIPImage): TsgGDIPStatus; stdcall;
// Bitmap API
  TsgGDIPlusCreateBitmapFromStream = function (Stream: ISTREAM; out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromFile = function (FileName: PWCHAR; out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromStreamICM = function (Stream: ISTREAM; out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromFileICM = function (FileName: PWCHAR; var BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromScan0 = function (Width: Integer; Height: Integer; Stride: Integer; Format: PIXELFORMAT; scan0: PBYTE;
    out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromGraphics = function (Width: Integer; Height: Integer; target: TsgGDIPGraphics; out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromGdiDib = function (GDIBitmapInfo: PBitmapInfo; GDIBitmapData: Pointer; out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromHBITMAP = function (HBM: HBITMAP; hPal: HPALETTE; out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateHBITMAPFromBitmap = function (BitMap: TsgGDIPBitMap; out HBMReturn: HBITMAP; background: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromHICON = function (HIcon: HICON; out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateHICONFromBitmap = function (BitMap: TsgGDIPBitMap; out HBMReturn: HICON): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateBitmapFromResource = function (HInstance: HMODULE; lpBitmapName: PWCHAR; out BitMap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneBitmapArea = function (X, Y, Width, Height: Single; Format: PIXELFORMAT; SrcBitmap: TsgGDIPBitMap;
    out DstBitmap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneBitmapAreaI = function (X, Y, Width, Height: Integer; Format: PIXELFORMAT; SrcBitmap: TsgGDIPBitMap;
    out DstBitmap: TsgGDIPBitMap): TsgGDIPStatus; stdcall;
  TsgGDIPlusBitmapLockBits = function (BitMap: TsgGDIPBitMap; Rect: PsgGRect; Flags: UINT; Format: PIXELFORMAT; lockedBitmapData: PBITMAPDATA): TsgGDIPStatus; stdcall;
  TsgGDIPlusBitmapUnlockBits = function (BitMap: TsgGDIPBitMap; LockedBitmapData: PBITMAPDATA): TsgGDIPStatus; stdcall;
  TsgGDIPlusBitmapGetPixel = function (BitMap: TsgGDIPBitMap; X, Y: Integer; var Color: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusBitmapSetPixel = function (BitMap: TsgGDIPBitMap; X, Y: Integer; Color: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusBitmapSetResolution = function (BitMap: TsgGDIPBitMap; XDpi: Single; YDpi: Single): TsgGDIPStatus; stdcall;
// ImageAttributes API
  TsgGDIPlusCreateImageAttributes = function (out ImageAttr: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneImageAttributes = function (ImageAttr: TsgGDIPImageAttributes; out CloneImageAttr: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusDisposeImageAttributes = function (ImageAttr: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesToIdentity = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetImageAttributes = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesColorMatrix = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE; enableFlag: Bool; ColorMatrix: PCOLORMATRIX;
    grayMatrix: PCOLORMATRIX; Flags: COLORMATRIXFLAGS): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesThreshold = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE; enableFlag: Bool;
    threshold: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesGamma = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE; enableFlag: Bool; gamma: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesNoOp = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE; enableFlag: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesColorKeys = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE; enableFlag: Bool; ColorLow: ARGB;
    ColorHigh: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesOutputChannel = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE; enableFlag: Bool;
    channelFlags: COLORCHANNELFLAGS): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesOutputChannelColorProfile = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE; enableFlag: Bool;
    ColorProfileFilename: PWCHAR): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesRemapTable = function (ImageAttr: TsgGDIPImageAttributes; Type_: COLORADJUSTTYPE; enableFlag: Bool; mapSize: UINT; map: PCOLORMAP): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesWrapMode = function (ImageAttr: TsgGDIPImageAttributes; Wrap: WRAPMODE; ARGB: ARGB; clamp: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetImageAttributesICMMode = function (ImageAttr: TsgGDIPImageAttributes; on_: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageAttributesAdjustedPalette = function (ImageAttr: TsgGDIPImageAttributes; ColorPalette: PCOLORPALETTE; ColorAdjustType: COLORADJUSTTYPE): TsgGDIPStatus; stdcall;
// Graphics API
  TsgGDIPlusFlush = function (Graphics: TsgGDIPGraphics;Intention: TsgGDIPFlushIntention): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateFromHDC = function (ADC: HDC; out Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateFromHDC2 = function (ADC: HDC; hDevice: THandle; out Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateFromHWND = function (hwnd: HWND; out Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateFromHWNDICM = function (hwnd: HWND; out Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeleteGraphics = function (Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetDC = function (Graphics: TsgGDIPGraphics; var ADC: HDC): TsgGDIPStatus; stdcall;
  TsgGDIPlusReleaseDC = function (Graphics: TsgGDIPGraphics; ADC: HDC): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetCompositingMode = function (Graphics: TsgGDIPGraphics; CompositingMode: COMPOSITINGMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCompositingMode = function (Graphics: TsgGDIPGraphics; var CompositingMode: COMPOSITINGMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetRenderingOrigin = function (Graphics: TsgGDIPGraphics; X: Integer; Y: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetRenderingOrigin = function (Graphics: TsgGDIPGraphics; var X: Integer; var Y: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetCompositingQuality = function (Graphics: TsgGDIPGraphics; CompositingQuality: COMPOSITINGQUALITY): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCompositingQuality = function (Graphics: TsgGDIPGraphics; var CompositingQuality: COMPOSITINGQUALITY): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetSmoothingMode = function (Graphics: TsgGDIPGraphics; SmoothingMode: SMOOTHINGMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetSmoothingMode = function (Graphics: TsgGDIPGraphics; var SmoothingMode: SMOOTHINGMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPixelOffsetMode = function (Graphics: TsgGDIPGraphics; PixelOffsetMode: PIXELOFFSETMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPixelOffsetMode = function (Graphics: TsgGDIPGraphics; var PixelOffsetMode: PIXELOFFSETMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetTextRenderingHint = function (Graphics: TsgGDIPGraphics; Mode: TEXTRENDERINGHINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetTextRenderingHint = function (Graphics: TsgGDIPGraphics; var Mode: TEXTRENDERINGHINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetTextContrast = function (Graphics: TsgGDIPGraphics; Contrast: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetTextContrast = function (Graphics: TsgGDIPGraphics; var Contrast: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetInterpolationMode = function (Graphics: TsgGDIPGraphics; InterpolationMode: INTERPOLATIONMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetInterpolationMode = function (Graphics: TsgGDIPGraphics; var InterpolationMode: INTERPOLATIONMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetWorldTransform = function (Graphics: TsgGDIPGraphics; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetWorldTransform = function (Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusMultiplyWorldTransform = function (Graphics: TsgGDIPGraphics; Matrix: TsgGDIPMatrix; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslateWorldTransform = function (Graphics: TsgGDIPGraphics; Dx, Dy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusScaleWorldTransform = function (Graphics: TsgGDIPGraphics; Sx, Sy: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusRotateWorldTransform = function (Graphics: TsgGDIPGraphics; Angle: Single; Order: TsgGDIPMatrixOrder): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetWorldTransform = function (Graphics: TsgGDIPGraphics; Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetPageTransform = function (Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPageUnit = function (Graphics: TsgGDIPGraphics; var Unit_: TsgGDIPUnit): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetPageScale = function (Graphics: TsgGDIPGraphics; var Scale: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPageUnit = function (Graphics: TsgGDIPGraphics; Unit_: TsgGDIPUnit): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetPageScale = function (Graphics: TsgGDIPGraphics; Scale: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetDpiX = function (Graphics: TsgGDIPGraphics; var Dpi: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetDpiY = function (Graphics: TsgGDIPGraphics; var Dpi: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusTransformPoints = function (Graphics: TsgGDIPGraphics; DestSpace: TsgGDIPCoordinateSpace; SrcSpace: TsgGDIPCoordinateSpace;
    Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusTransformPointsI = function (Graphics: TsgGDIPGraphics; DestSpace: TsgGDIPCoordinateSpace; SrcSpace: TsgGDIPCoordinateSpace;
    Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetNearestColor = function (Graphics: TsgGDIPGraphics; ARGB: PARGB): TsgGDIPStatus; stdcall;

  TsgGDIPlusCreateHalftonePalette = function: HPALETTE; stdcall;
  TsgGDIPlusDrawLine = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X1: Single; Y1: Single; X2: Single; Y2: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawLineI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X1, Y1, X2, Y2: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawLines = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawLinesI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawArc = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X, Y, Width, Height: Single; StartAngle: Single;
    SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawArcI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X, Y, Width, Height: Integer; StartAngle: Single;
    SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawBezier = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X1: Single; Y1: Single; X2: Single; Y2: Single; X3: Single; Y3: Single; X4: Single;
    Y4: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawBezierI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X1, Y1, X2, Y2: Integer; X3: Integer; Y3: Integer;
    X4: Integer; Y4: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawBeziers = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawBeziersI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawRectangle = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X, Y, Width, Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawRectangleI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X, Y, Width, Height: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawRectangles = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Rects: PsgGRectF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawRectanglesI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Rects: PsgGRect;  Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawEllipse = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X, Y, Width, Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawEllipseI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X, Y, Width, Height: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawPie = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X, Y: Single; Width: Single;  Height: Single; StartAngle: Single;
    SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawPieI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; X, Y, Width, Height: Integer; StartAngle: Single;
    SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawPolygon = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawPolygonI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawPath = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawCurve = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawCurveI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawCurve2 = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawCurve2I = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawCurve3 = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPointF; Count: Integer; Offset: Integer; NumberOfSegments: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawCurve3I = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPoint; Count: Integer; Offset: Integer; NumberOfSegments: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawClosedCurve = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawClosedCurveI = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawClosedCurve2 = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawClosedCurve2I = function (Graphics: TsgGDIPGraphics; Pen: TsgGDIPPen; Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGraphicsClear = function (Graphics: TsgGDIPGraphics; Color: ARGB): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillRectangle = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; X, Y, Width, Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillRectangleI = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; X, Y, Width, Height: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillRectangles = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Rects: PsgGRectF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillRectanglesI = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Rects: PsgGRect; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillPolygon = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Points: PsgGPointF; Count: Integer; FillMode: TsgGDIPFillMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillPolygonI = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Points: PsgGPoint; Count: Integer; FillMode: TsgGDIPFillMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillPolygon2 = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillPolygon2I = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillEllipse = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; X, Y, Width, Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillEllipseI = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; X, Y, Width, Height: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillPie = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; X, Y, Width, Height: Single; StartAngle: Single; SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillPieI = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; X: Integer;  Y: Integer; Width: Integer; Height: Integer; StartAngle: Single; SweepAngle: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillPath = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Path: TsgGDIPPath): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillClosedCurve = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Points: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillClosedCurveI = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Points: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillClosedCurve2 = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Points: PsgGPointF; Count: Integer; Tension: Single;
    FillMode: TsgGDIPFillMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillClosedCurve2I = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Points: PsgGPoint; Count: Integer; Tension: Single;
    FillMode: TsgGDIPFillMode): TsgGDIPStatus; stdcall;
  TsgGDIPlusFillRegion = function (Graphics: TsgGDIPGraphics; Brush: TsgGDIPBrush; Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImage = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; X: Single; Y: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImageI = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; X: Integer; Y: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImageRect = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; X, Y, Width, Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImageRectI = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; X, Y, Width, Height: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImagePoints = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; DstPoints: PsgGPointF; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImagePointsI = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; DstPoints: PsgGPoint; Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImagePointRect = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; X, Y: Single; SrcX: Single; SrcY: Single; SrcWidth: Single;
    SrcHeight: Single; SrcUnit: TsgGDIPUnit): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImagePointRectI = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; X, Y: Integer; SrcX: Integer; SrcY: Integer; SrcWidth: Integer;
    SrcHeight: Integer; SrcUnit: TsgGDIPUnit): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImageRectRect = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; Dstx: Single; Dsty: Single; DstWidth: Single; DstHeight: Single;
    SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TsgGDIPUnit; ImageAttributes: TsgGDIPImageAttributes;
    callback: DRAWIMAGEABORT; CallBackData: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImageRectRectI =  function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; Dstx: Integer; Dsty: Integer; DstWidth: Integer; DstHeight: Integer;
    SrcX: Integer; SrcY: Integer; SrcWidth: Integer; SrcHeight: Integer; SrcUnit: TsgGDIPUnit; ImageAttributes: TsgGDIPImageAttributes;
    callback: DRAWIMAGEABORT; CallBackData: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImagePointsRect = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; Points: PsgGPointF; Count: Integer; SrcX: Single; SrcY: Single;
    SrcWidth: Single; SrcHeight: Single; SrcUnit: TsgGDIPUnit; ImageAttributes: TsgGDIPImageAttributes; callback: DRAWIMAGEABORT;
    CallBackData: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawImagePointsRectI = function (Graphics: TsgGDIPGraphics; Image: TsgGDIPImage; Points: PsgGPoint; Count: Integer; SrcX: Integer; SrcY: Integer;
    SrcWidth: Integer; SrcHeight: Integer; SrcUnit: TsgGDIPUnit; ImageAttributes: TsgGDIPImageAttributes; CallBack: DRAWIMAGEABORT;
    CallBackData: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileDestPoint = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestPoint: PsgGPointF; MCallBack: ENumerateMetaFileProc;
    CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileDestPointI = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestPoint: PsgGPoint; MCallBack: ENumerateMetaFileProc;
    CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileDestRect = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestRect: PsgGRectF; MCallBack: ENumerateMetaFileProc;
    CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileDestRectI =  function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestRect: PsgGRect; MCallBack: ENumerateMetaFileProc;
    CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileDestPoints = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestPoints: PsgGPointF; Count: Integer;
    MCallBack: ENumerateMetaFileProc; CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileDestPointsI = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestPoints: PsgGPoint; Count: Integer;
    MCallBack: ENumerateMetaFileProc; CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileSrcRectDestPoint = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestPoint: PsgGPointF; SrcRect: PsgGRectF; SrcUnit: TsgGDIPUnit;
    MCallBack: ENumerateMetaFileProc; CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileSrcRectDestPointI = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestPoint: PsgGPoint; SrcRect: PsgGRect; SrcUnit: TsgGDIPUnit;
    MCallBack: ENumerateMetaFileProc; CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileSrcRectDestRect = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestRect: PsgGRectF; SrcRect: PsgGRectF; SrcUnit: TsgGDIPUnit;
    MCallBack: ENumerateMetaFileProc; CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileSrcRectDestRectI = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestRect: PsgGRect; SrcRect: PsgGRect; SrcUnit: TsgGDIPUnit;
    MCallBack: ENumerateMetaFileProc; CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileSrcRectDestPoints = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestPoints: PsgGPointF; Count: Integer; SrcRect: PsgGRectF;
    SrcUnit: TsgGDIPUnit; MCallBack: ENumerateMetaFileProc; CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusENumerateMetafileSrcRectDestPointsI = function (Graphics: TsgGDIPGraphics; Metafile: TsgGDIPMetaFile; DestPoints: PsgGPoint; Count: Integer; SrcRect: PsgGRect;
    SrcUnit: TsgGDIPUnit; MCallBack: ENumerateMetaFileProc; CallBackData: Pointer; ImageAttributes: TsgGDIPImageAttributes): TsgGDIPStatus; stdcall;
  TsgGDIPlusPlayMetafileRecord = function (Metafile: TsgGDIPMetaFile; RecordType: EMFPLUSRECORDTYPE; Flags: UINT; DataSize: UINT;
    Data: PBYTE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetClipGraphics = function (Graphics: TsgGDIPGraphics; SrcGraphics: TsgGDIPGraphics; CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetClipRect = function (Graphics: TsgGDIPGraphics; X, Y, Width, Height: Single; CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetClipRectI = function (Graphics: TsgGDIPGraphics; X, Y, Width, Height: Integer;
    CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetClipPath = function (Graphics: TsgGDIPGraphics; Path: TsgGDIPPath; CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetClipRegion = function (Graphics: TsgGDIPGraphics; Region: TsgGDIPRegion; CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetClipHrgn = function (Graphics: TsgGDIPGraphics; hRgn: HRGN; CombineMode: COMBINEMODE): TsgGDIPStatus; stdcall;
  TsgGDIPlusResetClip = function (Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslateClip = function (Graphics: TsgGDIPGraphics; Dx, Dy: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusTranslateClipI = function (Graphics: TsgGDIPGraphics; Dx: Integer; Dy: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetClip = function (Graphics: TsgGDIPGraphics; Region: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetClipBounds = function (Graphics: TsgGDIPGraphics; Rect: PsgGRectF): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetClipBoundsI  = function (Graphics: TsgGDIPGraphics; Rect: PsgGRect): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsClipEmpty = function (Graphics: TsgGDIPGraphics; Result: PBool): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetVisibleClipBounds = function (Graphics: TsgGDIPGraphics; Rect: PsgGRectF): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetVisibleClipBoundsI = function (Graphics: TsgGDIPGraphics; Rect: PsgGRect): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisibleClipEmpty = function (Graphics: TsgGDIPGraphics; var Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisiblePoint = function (Graphics: TsgGDIPGraphics; X, Y: Single; var Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisiblePointI = function (Graphics: TsgGDIPGraphics; X, Y: Integer; var Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisibleRect = function (Graphics: TsgGDIPGraphics; X, Y, Width, Height: Single; var Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsVisibleRectI = function (Graphics: TsgGDIPGraphics; X, Y, Width, Height: Integer; var Result: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusSaveGraphics = function (Graphics: TsgGDIPGraphics; var State: GRAPHICSSTATE): TsgGDIPStatus; stdcall;
  TsgGDIPlusRestoreGraphics = function (Graphics: TsgGDIPGraphics; State: GRAPHICSSTATE): TsgGDIPStatus; stdcall;
  TsgGDIPlusBeginContainer = function (Graphics: TsgGDIPGraphics; DstRect: PsgGRectF; SrcRect: PsgGRectF; Unit_: TsgGDIPUnit;
    var State: GRAPHICSCONTAINER): TsgGDIPStatus; stdcall;
  TsgGDIPlusBeginContainerI = function (Graphics: TsgGDIPGraphics; DstRect: PsgGRect; SrcRect: PsgGRect; Unit_: TsgGDIPUnit;
    var State: GRAPHICSCONTAINER): TsgGDIPStatus; stdcall;
  TsgGDIPlusBeginContainer2 = function (Graphics: TsgGDIPGraphics; var State: GRAPHICSCONTAINER): TsgGDIPStatus; stdcall;
  TsgGDIPlusEndContainer = function (Graphics: TsgGDIPGraphics; State: GRAPHICSCONTAINER): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetMetaFileHeaderFromWmf = function (hWmf: HMETAFILE; wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    header: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetMetaFileHeaderFromEmf = function (hEmf: HENHMETAFILE; header: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetMetaFileHeaderFromFile = function (FileName: PWCHAR; header: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetMetaFileHeaderFromStream = function (Stream: ISTREAM; header: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetMetaFileHeaderFromMetaFile = function (MetaFile: TsgGDIPMetaFile; header: Pointer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetHemfFromMetaFile = function (MetaFile: TsgGDIPMetaFile; var hEmf: HENHMETAFILE): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateStreamOnFile = function (FileName: PWCHAR; Access: UINT; out Stream: ISTREAM): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateMetaFileFromWmf = function (hWmf: HMETAFILE; DeleteWmf: Bool; wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateMetaFileFromEmf = function (hEmf: HENHMETAFILE; DeleteEmf: Bool; out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateMetaFileFromFile = function (File_: PWCHAR; out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateMetaFileFromWmfFile = function (File_: PWCHAR; wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateMetaFileFromStream = function (Stream: ISTREAM; out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusRecordMetaFile = function (ReferenceHdc: HDC; Type_: EMFTYPE;FrameRect: PsgGRectF; FrameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusRecordMetaFileI = function (ReferenceHdc: HDC; Type_: EMFTYPE; FrameRect: PsgGRect; FrameUnit: METAFILEFRAMEUNIT; description: PWCHAR;
    out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusRecordMetaFileFileName = function (FileName: PWCHAR; ReferenceHdc: HDC; Type_: EMFTYPE; FrameRect: PsgGRectF; FrameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusRecordMetaFileFileNameI = function (FileName: PWCHAR; ReferenceHdc: HDC; Type_: EMFTYPE; FrameRect: PsgGRect; FrameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusRecordMetaFileStream = function (Stream: ISTREAM; ReferenceHdc: HDC; Type_: EMFTYPE; FrameRect: PsgGRectF; FrameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusRecordMetaFileStreamI = function (Stream: ISTREAM; ReferenceHdc: HDC; Type_: EMFTYPE; FrameRect: PsgGRect; FrameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out MetaFile: TsgGDIPMetaFile): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetMetaFileDownLevelRasterizationLimit = function (MetaFile: TsgGDIPMetaFile; MetaFileRasterizationLimitDpi: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetMetaFileDownLevelRasterizationLimit = function (MetaFile: TsgGDIPMetaFile; var MetaFileRasterizationLimitDpi: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageDecodersSize = function (out NumDecoders: UINT; out Size: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageDecoders = function (NumDecoders: UINT; Size: UINT; decoders: PIMAGECODECINFO): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageEncodersSize = function (out NumEncoders: UINT; out Size: UINT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetImageEncoders = function (NumEncoders: UINT; Size: UINT; Encoders: PIMAGECODECINFO): TsgGDIPStatus; stdcall;
  TsgGDIPlusComment = function (Graphics: TsgGDIPGraphics; SizeData: UINT; Data: PBYTE): TsgGDIPStatus; stdcall;
//FontFamily API
  TsgGDIPlusCreateFontFamilyFromName = function (name: PWCHAR; FontCollection: TsgGDIPFontCollection; out FontFamily: TsgGDIPFontFamily): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeleteFontFamily = function (FontFamily: TsgGDIPFontFamily): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneFontFamily = function (FontFamily: TsgGDIPFontFamily; out ClonedFontFamily: TsgGDIPFontFamily): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetGenericFontFamilySansSerif = function (out NativeFamily: TsgGDIPFontFamily): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetGenericFontFamilySerif = function (out NativeFamily: TsgGDIPFontFamily): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetGenericFontFamilyMonospace = function (out NativeFamily: TsgGDIPFontFamily): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFamilyName = function (Family: TsgGDIPFontFamily; name: PWideChar; Language: LANGID): TsgGDIPStatus; stdcall;
  TsgGDIPlusIsStyleAvailable = function (Family: TsgGDIPFontFamily; Style: Integer; var IsStyleAvailable: Bool): TsgGDIPStatus; stdcall;
  TsgGDIPlusFontCollectionENumerable = function (FontCollection: TsgGDIPFontCollection; Graphics: TsgGDIPGraphics; var NumFound: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusFontCollectionENumerate = function (FontCollection: TsgGDIPFontCollection; NumSought: Integer; gpFamilies: array of TsgGDIPFontFamily;
    var NumFound: Integer; Graphics: TsgGDIPGraphics): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetEmHeight = function (Family: TsgGDIPFontFamily; Style: Integer; out EmHeight: UINT16): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCellAscent = function (Family: TsgGDIPFontFamily; Style: Integer; var CellAscent: UINT16): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetCellDescent = function (Family: TsgGDIPFontFamily; Style: Integer; var CellDescent: UINT16): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLineSpacing = function (Family: TsgGDIPFontFamily; Style: Integer; var LineSpacing: UINT16): TsgGDIPStatus; stdcall;
//Font API
  TsgGDIPlusCreateFontFromDC = function (ADC: HDC; out Font: TsgGDIPFont): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateFontFromLogFontA = function (ADC: HDC; ALogFont: PLOGFONTA; out Font: TsgGDIPFont): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateFontFromLogFontW = function (ADC: HDC; ALogFont: PLOGFONTW; out Font: TsgGDIPFont): TsgGDIPStatus; stdcall;
  TsgGDIPlusCreateFont = function (FontFamily: TsgGDIPFontFamily; ASize: Single; Style: Integer; Unit_: Integer; out Font: TsgGDIPFont): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneFont = function (Font: TsgGDIPFont; out CloneFont: TsgGDIPFont): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeleteFont = function (Font: TsgGDIPFont): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFamily = function (Font: TsgGDIPFont; out Family: TsgGDIPFontFamily): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFontStyle = function (Font: TsgGDIPFont; var Style: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFontSize = function (Font: TsgGDIPFont; var Size: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFontUnit = function (Font: TsgGDIPFont; var Unit_: TsgGDIPUnit): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFontHeight = function (Font: TsgGDIPFont; Graphics: TsgGDIPGraphics; var Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFontHeightGivenDPI = function (Font: TsgGDIPFont; Dpi: Single; var Height: Single): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLogFontA = function (Font: TsgGDIPFont; Graphics: TsgGDIPGraphics; var LogFontA: LOGFONTA): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetLogFontW = function (Font: TsgGDIPFont; Graphics: TsgGDIPGraphics; var LogFontW: LOGFONTW): TsgGDIPStatus; stdcall;
  TsgGDIPlusNewInstalledFontCollection = function (out FontCollection: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusNewPrivateFontCollection = function (out FontCollection: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeletePrivateFontCollection = function (out FontCollection: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFontCollectionFamilyCount = function (FontCollection: TsgGDIPFontCollection; var NumFound: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetFontCollectionFamilyList = function (FontCollection: TsgGDIPFontCollection; NumSought: Integer; gpFamilies: TsgGDIPFontFamily;
    var NumFound: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusPrivateAddFontFile = function (FontCollection: TsgGDIPFontCollection; FileName: PWCHAR): TsgGDIPStatus; stdcall;
  TsgGDIPlusPrivateAddMemoryFont = function (FontCollection: TsgGDIPFontCollection; Memory: Pointer; Length: Integer): TsgGDIPStatus; stdcall;
//Text API
  TsgGDIPlusDrawString = function (Graphics: TsgGDIPGraphics; String_: PWCHAR; Length: Integer; Font: TsgGDIPFont; layoutRect: PsgGRectF;
    stringFormat: TsgGDIPFontCollection; Brush: TsgGDIPBrush): TsgGDIPStatus; stdcall;
  TsgGDIPlusMeasureString = function (Graphics: TsgGDIPGraphics; String_: PWCHAR; Length: Integer; Font: TsgGDIPFont; layoutRect: PsgGRectF;
    stringFormat: TsgGDIPFontCollection; BoundingBox: PsgGRectF; codePointsFitted: PInteger; linesFilled: PInteger): TsgGDIPStatus; stdcall;
  TsgGDIPlusMeasureCharacterRanges = function (Graphics: TsgGDIPGraphics; String_: PWCHAR; Length: Integer; Font: TsgGDIPFont; layoutRect: PsgGRectF;
    stringFormat: TsgGDIPFontCollection; RegionCount: Integer; const Regions: TsgGDIPRegion): TsgGDIPStatus; stdcall;
  TsgGDIPlusDrawDriverString = function (Graphics: TsgGDIPGraphics; const Text: PUINT16; Length: Integer; const Font: TsgGDIPFont; const Brush: TsgGDIPBrush;
    const Positions: PsgGPointF; Flags: Integer; const Matrix: TsgGDIPMatrix): TsgGDIPStatus; stdcall;
  TsgGDIPlusMeasureDriverString = function (Graphics: TsgGDIPGraphics; Text: PUINT16; Length: Integer; Font: TsgGDIPFont; Positions: PsgGPointF; Flags: Integer;
    Matrix: TsgGDIPMatrix; BoundingBox: PsgGRectF): TsgGDIPStatus; stdcall;
// String Format API
  TsgGDIPlusCreateStringFormat = function (FormatAttributes: Integer; Language: LANGID; out Format: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusStringFormatGetGenericDefault = function (out Format: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusStringFormatGetGenericTypographic = function (out Format: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusDeleteStringFormat = function (Format: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusCloneStringFormat = function (Format: TsgGDIPFontCollection; out newFormat: TsgGDIPFontCollection): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetStringFormatFlags = function (Format: TsgGDIPFontCollection; Flags: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatFlags = function (Format: TsgGDIPFontCollection; out Flags: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetStringFormatAlign = function (Format: TsgGDIPFontCollection; align: STRINGALIGNMENT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatAlign = function (Format: TsgGDIPFontCollection; out align: STRINGALIGNMENT): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetStringFormatLineAlign = function (Format: TsgGDIPFontCollection; align: STRINGALIGNMENT): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatLineAlign = function (Format: TsgGDIPFontCollection; out align: STRINGALIGNMENT): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetStringFormatTrimming = function (Format: TsgGDIPFontCollection; trimming: STRINGTRIMMING): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatTrimming = function (Format: TsgGDIPFontCollection; out trimming: STRINGTRIMMING): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetStringFormatHotkeyPrefix = function (Format: TsgGDIPFontCollection; hotkeyPrefix: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatHotkeyPrefix = function (Format: TsgGDIPFontCollection; out hotkeyPrefix: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetStringFormatTabStops = function (Format: TsgGDIPFontCollection; FirstTabOffset: Single; Count: Integer; TabStops: PSingle): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatTabStops = function (Format: TsgGDIPFontCollection; Count: Integer; FirstTabOffset: PSingle;
    TabStops: PSingle): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatTabStopCount = function (Format: TsgGDIPFontCollection; out Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetStringFormatDigitSubstitution = function (Format: TsgGDIPFontCollection; Language: LANGID; substitute: STRINGDIGITSUBSTITUTE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatDigitSubstitution = function (Format: TsgGDIPFontCollection; Language: PUINT; substitute: PSTRINGDIGITSUBSTITUTE): TsgGDIPStatus; stdcall;
  TsgGDIPlusGetStringFormatMeasurableCharacterRangeCount = function (Format: TsgGDIPFontCollection; out Count: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusSetStringFormatMeasurableCharacterRanges = function (Format: TsgGDIPFontCollection; rangeCount: Integer; ranges: PCHARACTERRANGE): TsgGDIPStatus; stdcall;
//Cached Bitmap API
//  TsgGDIPlusCreateCachedBitmap = function (BitMap: TsgGDIPBitMap; Graphics: TsgGDIPGraphics; out cachedBitmap: TsgGDIPCachedBitmap): TsgGDIPStatus; stdcall;
//  TsgGDIPlusDeleteCachedBitmap = function (cachedBitmap: TsgGDIPCachedBitmap): TsgGDIPStatus; stdcall;
//  TsgGDIPlusDrawCachedBitmap = function (Graphics: TsgGDIPGraphics; cachedBitmap: TsgGDIPCachedBitmap; X, Y: Integer): TsgGDIPStatus; stdcall;
  TsgGDIPlusEmfToWmfBits = function (hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE; iMapMode: Integer; eFlags: Integer): UINT; stdcall;
// Memory AlLocation API
  TsgGDIPlusAlloc = function (Size: ULONG): Pointer; stdcall;
  TsgGDIPlusFree = procedure (ptr: Pointer); stdcall;

var
  GDIPAlloc: TsgGDIPlusAlloc = nil;
  GDIPFree: TsgGDIPlusFree = nil;
  GDIPlusStartup: TsgGDIPlusStartup = nil;
  GDIPlusShutdown: TsgGDIPlusShutdown = nil;

  GDIPCreatePath: TsgGDIPlusCreatePath = nil;
  GDIPCreatePath2: TsgGDIPlusCreatePath2 = nil;
  GDIPCreatePath2I: TsgGDIPlusCreatePath2I = nil;
  GDIPClonePath: TsgGDIPlusClonePath = nil;
  GDIPDeletePath: TsgGDIPlusDeletePath = nil;
  GDIPResetPath: TsgGDIPlusResetPath = nil;
  GDIPGetPointCount: TsgGDIPlusGetPointCount = nil;
  GDIPGetPathTypes: TsgGDIPlusGetPathTypes = nil;
  GDIPGetPathPoints: TsgGDIPlusGetPathPoints = nil;
  GDIPGetPathPointsI: TsgGDIPlusGetPathPointsI = nil;
  GDIPGetPathFillMode: TsgGDIPlusGetPathFillMode = nil;
  GDIPSetPathFillMode: TsgGDIPlusSetPathFillMode = nil;
  GDIPGetPathData: TsgGDIPlusGetPathData = nil;
  GDIPStartPathFigure: TsgGDIPlusStartPathFigure = nil;
  GDIPClosePathFigure: TsgGDIPlusClosePathFigure = nil;
  GDIPClosePathFigures: TsgGDIPlusClosePathFigures = nil;
  GDIPSetPathMarker: TsgGDIPlusSetPathMarker = nil;
  GDIPClearPathMarkers: TsgGDIPlusClearPathMarkers = nil;
  GDIPReversePath: TsgGDIPlusReversePath = nil;
  GDIPGetPathLastPoint: TsgGDIPlusGetPathLastPoint = nil;
  GDIPAddPathLine: TsgGDIPlusAddPathLine = nil;
  GDIPAddPathLine2: TsgGDIPlusAddPathLine2 = nil;
  GDIPAddPathArc: TsgGDIPlusAddPathArc = nil;
  GDIPAddPathBezier: TsgGDIPlusAddPathBezier = nil;
  GDIPAddPathBeziers: TsgGDIPlusAddPathBeziers = nil;
  GDIPAddPathCurve: TsgGDIPlusAddPathCurve = nil;
  GDIPAddPathCurve2: TsgGDIPlusAddPathCurve2 = nil;
  GDIPAddPathCurve3: TsgGDIPlusAddPathCurve3 = nil;
  GDIPAddPathClosedCurve: TsgGDIPlusAddPathClosedCurve = nil;
  GDIPAddPathClosedCurve2: TsgGDIPlusAddPathClosedCurve2 = nil;
  GDIPAddPathRectangle: TsgGDIPlusAddPathRectangle = nil;
  GDIPAddPathRectangles: TsgGDIPlusAddPathRectangles = nil;
  GDIPAddPathEllipse: TsgGDIPlusAddPathEllipse = nil;
  GDIPAddPathPie: TsgGDIPlusAddPathPie = nil;
  GDIPAddPathPolygon: TsgGDIPlusAddPathPolygon = nil;
  GDIPAddPathPath: TsgGDIPlusAddPathPath = nil;
  GDIPAddPathString: TsgGDIPlusAddPathString = nil;
  GDIPAddPathStringI: TsgGDIPlusAddPathStringI = nil;
  GDIPAddPathLineI: TsgGDIPlusAddPathLineI = nil;
  GDIPAddPathLine2I: TsgGDIPlusAddPathLine2I = nil;
  GDIPAddPathArcI: TsgGDIPlusAddPathArcI = nil;
  GDIPAddPathBezierI: TsgGDIPlusAddPathBezierI = nil;
  GDIPAddPathBeziersI: TsgGDIPlusAddPathBeziersI = nil;
  GDIPAddPathCurveI: TsgGDIPlusAddPathCurveI = nil;
  GDIPAddPathCurve2I: TsgGDIPlusAddPathCurve2I = nil;
  GDIPAddPathCurve3I: TsgGDIPlusAddPathCurve3I = nil;
  GDIPAddPathClosedCurveI: TsgGDIPlusAddPathClosedCurveI = nil;
  GDIPAddPathClosedCurve2I: TsgGDIPlusAddPathClosedCurve2I = nil;
  GDIPAddPathRectangleI: TsgGDIPlusAddPathRectangleI = nil;
  GDIPAddPathRectanglesI: TsgGDIPlusAddPathRectanglesI = nil;
  GDIPAddPathEllipseI: TsgGDIPlusAddPathEllipseI = nil;
  GDIPAddPathPieI: TsgGDIPlusAddPathPieI = nil;
  GDIPAddPathPolygonI: TsgGDIPlusAddPathPolygonI = nil;
  GDIPFlattenPath: TsgGDIPlusFlattenPath = nil;
  GDIPWindingModeOutline: TsgGDIPlusWindingModeOutline = nil;
  GDIPWidenPath: TsgGDIPlusWidenPath = nil;
  GDIPWarpPath: TsgGDIPlusWarpPath = nil;
  GDIPTransformPath: TsgGDIPlusTransformPath = nil;
  GDIPGetPathWorldBounds: TsgGDIPlusGetPathWorldBounds = nil;
  GDIPGetPathWorldBoundsI: TsgGDIPlusGetPathWorldBoundsI = nil;
  GDIPIsVisiblePathPoint: TsgGDIPlusIsVisiblePathPoint = nil;
  GDIPIsVisiblePathPointI: TsgGDIPlusIsVisiblePathPointI = nil;
  GDIPIsOutlineVisiblePathPoint: TsgGDIPlusIsOutlineVisiblePathPoint = nil;
  GDIPIsOutlineVisiblePathPointI: TsgGDIPlusIsOutlineVisiblePathPointI = nil;
  GDIPCreatePathIter: TsgGDIPlusCreatePathIter = nil;
  GDIPDeletePathIter: TsgGDIPlusDeletePathIter = nil;
  GDIPPathIterNextSubpath: TsgGDIPlusPathIterNextSubpath = nil;
  GDIPPathIterNextSubpathPath: TsgGDIPlusPathIterNextSubpathPath = nil;
  GDIPPathIterNextPathType: TsgGDIPlusPathIterNextPathType = nil;
  GDIPPathIterNextMarker: TsgGDIPlusPathIterNextMarker = nil;
  GDIPPathIterNextMarkerPath: TsgGDIPlusPathIterNextMarkerPath = nil;
  GDIPPathIterGetCount: TsgGDIPlusPathIterGetCount = nil;
  GDIPPathIterGetSubpathCount: TsgGDIPlusPathIterGetSubpathCount = nil;
  GDIPPathIterIsValid: TsgGDIPlusPathIterIsValid = nil;
  GDIPPathIterHasCurve: TsgGDIPlusPathIterHasCurve = nil;
  GDIPPathIterRewind: TsgGDIPlusPathIterRewind = nil;
  GDIPPathIterEnumerate: TsgGDIPlusPathIterEnumerate = nil;
  GDIPPathIterCopyData: TsgGDIPlusPathIterCopyData = nil;
  GDIPCreateMatrix: TsgGDIPlusCreateMatrix = nil;
  GDIPCreateMatrix2: TsgGDIPlusCreateMatrix2 = nil;
  GDIPCreateMatrix3: TsgGDIPlusCreateMatrix3 = nil;
  GDIPCreateMatrix3I: TsgGDIPlusCreateMatrix3I = nil;
  GDIPCloneMatrix: TsgGDIPlusCloneMatrix = nil;
  GDIPDeleteMatrix: TsgGDIPlusDeleteMatrix = nil;
  GDIPSetMatrixElements: TsgGDIPlusSetMatrixElements = nil;
  GDIPMultiplyMatrix: TsgGDIPlusMultiplyMatrix = nil;
  GDIPTranslateMatrix: TsgGDIPlusTranslateMatrix = nil;
  GDIPScaleMatrix: TsgGDIPlusScaleMatrix = nil;
  GDIPRotateMatrix: TsgGDIPlusRotateMatrix = nil;
  GDIPShearMatrix: TsgGDIPlusShearMatrix = nil;
  GDIPInvertMatrix: TsgGDIPlusInvertMatrix = nil;
  GDIPTransformMatrixPoints: TsgGDIPlusTransformMatrixPoints = nil;
  GDIPTransformMatrixPointsI: TsgGDIPlusTransformMatrixPointsI = nil;
  GDIPVectorTransformMatrixPoints: TsgGDIPlusVectorTransformMatrixPoints = nil;
  GDIPVectorTransformMatrixPointsI: TsgGDIPlusVectorTransformMatrixPointsI = nil;
  GDIPGetMatrixElements: TsgGDIPlusGetMatrixElements = nil;
  GDIPIsMatrixInvertible: TsgGDIPlusIsMatrixInvertible = nil;
  GDIPIsMatrixIdentity: TsgGDIPlusIsMatrixIdentity = nil;
  GDIPIsMatrixEqual: TsgGDIPlusIsMatrixEqual = nil;
  GDIPCreateRegion: TsgGDIPlusCreateRegion = nil;
  GDIPCreateRegionRect: TsgGDIPlusCreateRegionRect = nil;
  GDIPCreateRegionRectI: TsgGDIPlusCreateRegionRectI = nil;
  GDIPCreateRegionPath: TsgGDIPlusCreateRegionPath = nil;
  GDIPCreateRegionRgnData: TsgGDIPlusCreateRegionRgnData = nil;
  GDIPCreateRegionHrgn: TsgGDIPlusCreateRegionHrgn = nil;
  GDIPCloneRegion: TsgGDIPlusCloneRegion = nil;
  GDIPDeleteRegion: TsgGDIPlusDeleteRegion = nil;
  GDIPSetInfinite: TsgGDIPlusSetInfinite = nil;
  GDIPSetEmpty: TsgGDIPlusSetEmpty = nil;
  GDIPCombineRegionRect: TsgGDIPlusCombineRegionRect = nil;
  GDIPCombineRegionRectI: TsgGDIPlusCombineRegionRectI = nil;
  GDIPCombineRegionPath: TsgGDIPlusCombineRegionPath = nil;
  GDIPCombineRegionRegion: TsgGDIPlusCombineRegionRegion = nil;
  GDIPTranslateRegion: TsgGDIPlusTranslateRegion = nil;
  GDIPTranslateRegionI: TsgGDIPlusTranslateRegionI = nil;
  GDIPTransformRegion: TsgGDIPlusTransformRegion = nil;
  GDIPGetRegionBounds: TsgGDIPlusGetRegionBounds = nil;
  GDIPGetRegionBoundsI: TsgGDIPlusGetRegionBoundsI = nil;
  GDIPGetRegionHRgn: TsgGDIPlusGetRegionHRgn = nil;
  GDIPIsEmptyRegion: TsgGDIPlusIsEmptyRegion = nil;
  GDIPIsInfiniteRegion: TsgGDIPlusIsInfiniteRegion = nil;
  GDIPIsEqualRegion: TsgGDIPlusIsEqualRegion = nil;
  GDIPGetRegionDataSize: TsgGDIPlusGetRegionDataSize = nil;
  GDIPGetRegionData: TsgGDIPlusGetRegionData = nil;
  GDIPIsVisibleRegionPoint: TsgGDIPlusIsVisibleRegionPoint = nil;
  GDIPIsVisibleRegionPointI: TsgGDIPlusIsVisibleRegionPointI = nil;
  GDIPIsVisibleRegionRect: TsgGDIPlusIsVisibleRegionRect = nil;
  GDIPIsVisibleRegionRectI: TsgGDIPlusIsVisibleRegionRectI = nil;
  GDIPGetRegionScansCount: TsgGDIPlusGetRegionScansCount = nil;
  GDIPGetRegionScans: TsgGDIPlusGetRegionScans = nil;
  GDIPGetRegionScansI: TsgGDIPlusGetRegionScansI = nil;
  GDIPCloneBrush: TsgGDIPlusCloneBrush = nil;
  GDIPDeleteBrush: TsgGDIPlusDeleteBrush = nil;
  GDIPGetBrushType: TsgGDIPlusGetBrushType = nil;
  GDIPCreateHatchBrush: TsgGDIPlusCreateHatchBrush = nil;
  GDIPGetHatchStyle: TsgGDIPlusGetHatchStyle = nil;
  GDIPGetHatchForegroundColor: TsgGDIPlusGetHatchForegroundColor = nil;
  GDIPGetHatchBackgroundColor: TsgGDIPlusGetHatchBackgroundColor = nil;
  GDIPCreateTexture: TsgGDIPlusCreateTexture = nil;
  GDIPCreateTexture2: TsgGDIPlusCreateTexture2 = nil;
  GDIPCreateTextureIA: TsgGDIPlusCreateTextureIA = nil;
  GDIPCreateTexture2I: TsgGDIPlusCreateTexture2I = nil;
  GDIPCreateTextureIAI: TsgGDIPlusCreateTextureIAI = nil;
  GDIPGetTextureTransform: TsgGDIPlusGetTextureTransform = nil;
  GDIPSetTextureTransform: TsgGDIPlusSetTextureTransform = nil;
  GDIPResetTextureTransform: TsgGDIPlusResetTextureTransform = nil;
  GDIPMultiplyTextureTransform: TsgGDIPlusMultiplyTextureTransform = nil;
  GDIPTranslateTextureTransform: TsgGDIPlusTranslateTextureTransform = nil;
  GDIPScaleTextureTransform: TsgGDIPlusScaleTextureTransform = nil;
  GDIPRotateTextureTransform: TsgGDIPlusRotateTextureTransform = nil;
  GDIPSetTextureWrapMode: TsgGDIPlusSetTextureWrapMode = nil;
  GDIPGetTextureWrapMode: TsgGDIPlusGetTextureWrapMode = nil;
  GDIPGetTextureImage: TsgGDIPlusGetTextureImage = nil;
  GDIPCreateSolidFill: TsgGDIPlusCreateSolidFill = nil;
  GDIPSetSolidFillColor: TsgGDIPlusSetSolidFillColor = nil;
  GDIPGetSolidFillColor: TsgGDIPlusGetSolidFillColor = nil;
  GDIPCreateLineBrush: TsgGDIPlusCreateLineBrush = nil;
  GDIPCreateLineBrushI: TsgGDIPlusCreateLineBrushI = nil;
  GDIPCreateLineBrushFromRect: TsgGDIPlusCreateLineBrushFromRect = nil;
  GDIPCreateLineBrushFromRectI: TsgGDIPlusCreateLineBrushFromRectI = nil;
  GDIPCreateLineBrushFromRectWithAngle: TsgGDIPlusCreateLineBrushFromRectWithAngle = nil;
  GDIPCreateLineBrushFromRectWithAngleI: TsgGDIPlusCreateLineBrushFromRectWithAngleI = nil;
  GDIPSetLineColors: TsgGDIPlusSetLineColors = nil;
  GDIPGetLineColors: TsgGDIPlusGetLineColors = nil;
  GDIPGetLineRect: TsgGDIPlusGetLineRect = nil;
  GDIPGetLineRectI: TsgGDIPlusGetLineRectI = nil;
  GDIPSetLineGammaCorrection: TsgGDIPlusSetLineGammaCorrection = nil;
  GDIPGetLineGammaCorrection: TsgGDIPlusGetLineGammaCorrection = nil;
  GDIPGetLineBlendCount: TsgGDIPlusGetLineBlendCount = nil;
  GDIPGetLineBlend: TsgGDIPlusGetLineBlend = nil;
  GDIPSetLineBlend: TsgGDIPlusSetLineBlend = nil;
  GDIPGetLinePresetBlendCount: TsgGDIPlusGetLinePresetBlendCount = nil;
  GDIPGetLinePresetBlend: TsgGDIPlusGetLinePresetBlend = nil;
  GDIPSetLinePresetBlend: TsgGDIPlusSetLinePresetBlend = nil;
  GDIPSetLineSigmaBlend: TsgGDIPlusSetLineSigmaBlend = nil;
  GDIPSetLineLinearBlend: TsgGDIPlusSetLineLinearBlend = nil;
  GDIPSetLineWrapMode: TsgGDIPlusSetLineWrapMode = nil;
  GDIPGetLineWrapMode: TsgGDIPlusGetLineWrapMode = nil;
  GDIPGetLineTransform: TsgGDIPlusGetLineTransform = nil;
  GDIPSetLineTransform: TsgGDIPlusSetLineTransform = nil;
  GDIPResetLineTransform: TsgGDIPlusResetLineTransform = nil;
  GDIPMultiplyLineTransform: TsgGDIPlusMultiplyLineTransform = nil;
  GDIPTranslateLineTransform: TsgGDIPlusTranslateLineTransform = nil;
  GDIPScaleLineTransform: TsgGDIPlusScaleLineTransform = nil;
  GDIPRotateLineTransform: TsgGDIPlusRotateLineTransform = nil;
  GDIPCreatePathGradient: TsgGDIPlusCreatePathGradient = nil;
  GDIPCreatePathGradientI: TsgGDIPlusCreatePathGradientI = nil;
  GDIPCreatePathGradientFromPath: TsgGDIPlusCreatePathGradientFromPath = nil;
  GDIPGetPathGradientCenterColor: TsgGDIPlusGetPathGradientCenterColor = nil;
  GDIPSetPathGradientCenterColor: TsgGDIPlusSetPathGradientCenterColor = nil;
  GDIPGetPathGradientSurroundColorsWithCount: TsgGDIPlusGetPathGradientSurroundColorsWithCount = nil;
  GDIPSetPathGradientSurroundColorsWithCount: TsgGDIPlusSetPathGradientSurroundColorsWithCount = nil;
  GDIPGetPathGradientPath: TsgGDIPlusGetPathGradientPath = nil;
  GDIPSetPathGradientPath: TsgGDIPlusSetPathGradientPath = nil;
  GDIPGetPathGradientCenterPoint: TsgGDIPlusGetPathGradientCenterPoint = nil;
  GDIPGetPathGradientCenterPointI: TsgGDIPlusGetPathGradientCenterPointI = nil;
  GDIPSetPathGradientCenterPoint: TsgGDIPlusSetPathGradientCenterPoint = nil;
  GDIPSetPathGradientCenterPointI: TsgGDIPlusSetPathGradientCenterPointI = nil;
  GDIPGetPathGradientRect: TsgGDIPlusGetPathGradientRect = nil;
  GDIPGetPathGradientRectI: TsgGDIPlusGetPathGradientRectI = nil;
  GDIPGetPathGradientPointCount: TsgGDIPlusGetPathGradientPointCount = nil;
  GDIPGetPathGradientSurroundColorCount: TsgGDIPlusGetPathGradientSurroundColorCount = nil;
  GDIPSetPathGradientGammaCorrection: TsgGDIPlusSetPathGradientGammaCorrection = nil;
  GDIPGetPathGradientGammaCorrection: TsgGDIPlusGetPathGradientGammaCorrection = nil;
  GDIPGetPathGradientBlendCount: TsgGDIPlusGetPathGradientBlendCount = nil;
  GDIPGetPathGradientBlend: TsgGDIPlusGetPathGradientBlend = nil;
  GDIPSetPathGradientBlend: TsgGDIPlusSetPathGradientBlend = nil;
  GDIPGetPathGradientPresetBlendCount: TsgGDIPlusGetPathGradientPresetBlendCount = nil;
  GDIPGetPathGradientPresetBlend: TsgGDIPlusGetPathGradientPresetBlend = nil;
  GDIPSetPathGradientPresetBlend: TsgGDIPlusSetPathGradientPresetBlend = nil;
  GDIPSetPathGradientSigmaBlend: TsgGDIPlusSetPathGradientSigmaBlend = nil;
  GDIPSetPathGradientLinearBlend: TsgGDIPlusSetPathGradientLinearBlend = nil;
  GDIPGetPathGradientWrapMode: TsgGDIPlusGetPathGradientWrapMode = nil;
  GDIPSetPathGradientWrapMode: TsgGDIPlusSetPathGradientWrapMode = nil;
  GDIPGetPathGradientTransform: TsgGDIPlusGetPathGradientTransform = nil;
  GDIPSetPathGradientTransform: TsgGDIPlusSetPathGradientTransform = nil;
  GDIPResetPathGradientTransform: TsgGDIPlusResetPathGradientTransform = nil;
  GDIPMultiplyPathGradientTransform: TsgGDIPlusMultiplyPathGradientTransform = nil;
  GDIPTranslatePathGradientTransform: TsgGDIPlusTranslatePathGradientTransform = nil;
  GDIPScalePathGradientTransform: TsgGDIPlusScalePathGradientTransform = nil;
  GDIPRotatePathGradientTransform: TsgGDIPlusRotatePathGradientTransform = nil;
  GDIPGetPathGradientFocusScales: TsgGDIPlusGetPathGradientFocusScales = nil;
  GDIPSetPathGradientFocusScales: TsgGDIPlusSetPathGradientFocusScales = nil;
  GDIPCreatePen1: TsgGDIPlusCreatePen1 = nil;
  GDIPCreatePen2: TsgGDIPlusCreatePen2 = nil;
  GDIPClonePen: TsgGDIPlusClonePen = nil;
  GDIPDeletePen: TsgGDIPlusDeletePen = nil;
  GDIPSetPenWidth: TsgGDIPlusSetPenWidth = nil;
  GDIPGetPenWidth: TsgGDIPlusGetPenWidth = nil;
  GDIPSetPenUnit: TsgGDIPlusSetPenUnit = nil;
  GDIPGetPenUnit: TsgGDIPlusGetPenUnit = nil;
  GDIPSetPenLineCap197819: TsgGDIPlusSetPenLineCap197819 = nil;
  GDIPSetPenStartCap: TsgGDIPlusSetPenStartCap = nil;
  GDIPSetPenEndCap: TsgGDIPlusSetPenEndCap = nil;
  GDIPSetPenDashCap197819: TsgGDIPlusSetPenDashCap197819 = nil;
  GDIPGetPenStartCap: TsgGDIPlusGetPenStartCap = nil;
  GDIPGetPenEndCap: TsgGDIPlusGetPenEndCap = nil;
  GDIPGetPenDashCap197819: TsgGDIPlusGetPenDashCap197819 = nil;
  GDIPSetPenLineJoin: TsgGDIPlusSetPenLineJoin = nil;
  GDIPGetPenLineJoin: TsgGDIPlusGetPenLineJoin = nil;
  GDIPSetPenCustomStartCap: TsgGDIPlusSetPenCustomStartCap = nil;
  GDIPGetPenCustomStartCap: TsgGDIPlusGetPenCustomStartCap = nil;
  GDIPSetPenCustomEndCap: TsgGDIPlusSetPenCustomEndCap = nil;
  GDIPGetPenCustomEndCap: TsgGDIPlusGetPenCustomEndCap = nil;
  GDIPSetPenMiterLimit: TsgGDIPlusSetPenMiterLimit = nil;
  GDIPGetPenMiterLimit: TsgGDIPlusGetPenMiterLimit = nil;
  GDIPSetPenMode: TsgGDIPlusSetPenMode = nil;
  GDIPGetPenMode: TsgGDIPlusGetPenMode = nil;
  GDIPSetPenTransform: TsgGDIPlusSetPenTransform = nil;
  GDIPGetPenTransform: TsgGDIPlusGetPenTransform = nil;
  GDIPResetPenTransform: TsgGDIPlusResetPenTransform = nil;
  GDIPMultiplyPenTransform: TsgGDIPlusMultiplyPenTransform = nil;
  GDIPTranslatePenTransform: TsgGDIPlusTranslatePenTransform = nil;
  GDIPScalePenTransform: TsgGDIPlusScalePenTransform = nil;
  GDIPRotatePenTransform: TsgGDIPlusRotatePenTransform = nil;
  GDIPSetPenColor: TsgGDIPlusSetPenColor = nil;
  GDIPGetPenColor: TsgGDIPlusGetPenColor = nil;
  GDIPSetPenBrushFill: TsgGDIPlusSetPenBrushFill = nil;
  GDIPGetPenBrushFill: TsgGDIPlusGetPenBrushFill = nil;
  GDIPGetPenFillType: TsgGDIPlusGetPenFillType = nil;
  GDIPGetPenDashStyle: TsgGDIPlusGetPenDashStyle = nil;
  GDIPSetPenDashStyle: TsgGDIPlusSetPenDashStyle = nil;
  GDIPGetPenDashOffset: TsgGDIPlusGetPenDashOffset = nil;
  GDIPSetPenDashOffset: TsgGDIPlusSetPenDashOffset = nil;
  GDIPGetPenDashCount: TsgGDIPlusGetPenDashCount = nil;
  GDIPSetPenDashArray: TsgGDIPlusSetPenDashArray = nil;
  GDIPGetPenDashArray: TsgGDIPlusGetPenDashArray = nil;
  GDIPGetPenCompoundCount: TsgGDIPlusGetPenCompoundCount = nil;
  GDIPSetPenCompoundArray: TsgGDIPlusSetPenCompoundArray = nil;
  GDIPGetPenCompoundArray: TsgGDIPlusGetPenCompoundArray = nil;
  GDIPCreateCustomLineCap: TsgGDIPlusCreateCustomLineCap = nil;
  GDIPDeleteCustomLineCap: TsgGDIPlusDeleteCustomLineCap = nil;
  GDIPCloneCustomLineCap: TsgGDIPlusCloneCustomLineCap = nil;
  GDIPGetCustomLineCapType: TsgGDIPlusGetCustomLineCapType = nil;
  GDIPSetCustomLineCapStrokeCaps: TsgGDIPlusSetCustomLineCapStrokeCaps = nil;
  GDIPGetCustomLineCapStrokeCaps: TsgGDIPlusGetCustomLineCapStrokeCaps = nil;
  GDIPSetCustomLineCapStrokeJoin: TsgGDIPlusSetCustomLineCapStrokeJoin = nil;
  GDIPGetCustomLineCapStrokeJoin: TsgGDIPlusGetCustomLineCapStrokeJoin = nil;
  GDIPSetCustomLineCapBaseCap: TsgGDIPlusSetCustomLineCapBaseCap = nil;
  GDIPGetCustomLineCapBaseCap: TsgGDIPlusGetCustomLineCapBaseCap = nil;
  GDIPSetCustomLineCapBaseInset: TsgGDIPlusSetCustomLineCapBaseInset = nil;
  GDIPGetCustomLineCapBaseInset: TsgGDIPlusGetCustomLineCapBaseInset = nil;
  GDIPSetCustomLineCapWidthScale: TsgGDIPlusSetCustomLineCapWidthScale = nil;
  GDIPGetCustomLineCapWidthScale: TsgGDIPlusGetCustomLineCapWidthScale = nil;
  GDIPCreateAdjustableArrowCap: TsgGDIPlusCreateAdjustableArrowCap = nil;
  GDIPSetAdjustableArrowCapHeight: TsgGDIPlusSetAdjustableArrowCapHeight = nil;
  GDIPGetAdjustableArrowCapHeight: TsgGDIPlusGetAdjustableArrowCapHeight = nil;
  GDIPSetAdjustableArrowCapWidth: TsgGDIPlusSetAdjustableArrowCapWidth = nil;
  GDIPGetAdjustableArrowCapWidth: TsgGDIPlusGetAdjustableArrowCapWidth = nil;
  GDIPSetAdjustableArrowCapMiddleInset: TsgGDIPlusSetAdjustableArrowCapMiddleInset = nil;
  GDIPGetAdjustableArrowCapMiddleInset: TsgGDIPlusGetAdjustableArrowCapMiddleInset = nil;
  GDIPSetAdjustableArrowCapFillState: TsgGDIPlusSetAdjustableArrowCapFillState = nil;
  GDIPGetAdjustableArrowCapFillState: TsgGDIPlusGetAdjustableArrowCapFillState = nil;
  GDIPLoadImageFromStream: TsgGDIPlusLoadImageFromStream = nil;
  GDIPLoadImageFromFile: TsgGDIPlusLoadImageFromFile = nil;
  GDIPLoadImageFromStreamICM: TsgGDIPlusLoadImageFromStreamICM = nil;
  GDIPLoadImageFromFileICM: TsgGDIPlusLoadImageFromFileICM = nil;
  GDIPCloneImage: TsgGDIPlusCloneImage = nil;
  GDIPDisposeImage: TsgGDIPlusDisposeImage = nil;
  GDIPSaveImageToFile: TsgGDIPlusSaveImageToFile = nil;
  GDIPSaveImageToStream: TsgGDIPlusSaveImageToStream = nil;
  GDIPSaveAdd: TsgGDIPlusSaveAdd = nil;
  GDIPSaveAddImage: TsgGDIPlusSaveAddImage = nil;
  GDIPGetImageGraphicsContext: TsgGDIPlusGetImageGraphicsContext = nil;
  GDIPGetImageBounds: TsgGDIPlusGetImageBounds = nil;
  GDIPGetImageDimension: TsgGDIPlusGetImageDimension = nil;
  GDIPGetImageType: TsgGDIPlusGetImageType = nil;
  GDIPGetImageWidth: TsgGDIPlusGetImageWidth = nil;
  GDIPGetImageHeight: TsgGDIPlusGetImageHeight = nil;
  GDIPGetImageHorizontalResolution: TsgGDIPlusGetImageHorizontalResolution = nil;
  GDIPGetImageVerticalResolution: TsgGDIPlusGetImageVerticalResolution = nil;
  GDIPGetImageFlags: TsgGDIPlusGetImageFlags = nil;
  GDIPGetImageRawFormat: TsgGDIPlusGetImageRawFormat = nil;
  GDIPGetImagePixelFormat: TsgGDIPlusGetImagePixelFormat = nil;
  GDIPGetImageThumbnail: TsgGDIPlusGetImageThumbnail = nil;
  GDIPGetEncoderParameterListSize: TsgGDIPlusGetEncoderParameterListSize = nil;
  GDIPGetEncoderParameterList: TsgGDIPlusGetEncoderParameterList = nil;
  GDIPImageGetFrameDimensionsCount: TsgGDIPlusImageGetFrameDimensionsCount = nil;
  GDIPImageGetFrameDimensionsList: TsgGDIPlusImageGetFrameDimensionsList = nil;
  GDIPImageGetFrameCount: TsgGDIPlusImageGetFrameCount = nil;
  GDIPImageSelectActiveFrame: TsgGDIPlusImageSelectActiveFrame = nil;
  GDIPImageRotateFlip: TsgGDIPlusImageRotateFlip = nil;
  GDIPGetImagePalette: TsgGDIPlusGetImagePalette = nil;
  GDIPSetImagePalette: TsgGDIPlusSetImagePalette = nil;
  GDIPGetImagePaletteSize: TsgGDIPlusGetImagePaletteSize = nil;
  GDIPGetPropertyCount: TsgGDIPlusGetPropertyCount = nil;
  GDIPGetPropertyIdList: TsgGDIPlusGetPropertyIdList = nil;
  GDIPGetPropertyItemSize: TsgGDIPlusGetPropertyItemSize = nil;
  GDIPGetPropertyItem: TsgGDIPlusGetPropertyItem = nil;
  GDIPGetPropertySize: TsgGDIPlusGetPropertySize = nil;
  GDIPGetAllPropertyItems: TsgGDIPlusGetAllPropertyItems = nil;
  GDIPRemovePropertyItem: TsgGDIPlusRemovePropertyItem = nil;
  GDIPSetPropertyItem: TsgGDIPlusSetPropertyItem = nil;
  GDIPImageForceValidation: TsgGDIPlusImageForceValidation = nil;
  GDIPCreateBitmapFromStream: TsgGDIPlusCreateBitmapFromStream = nil;
  GDIPCreateBitmapFromFile: TsgGDIPlusCreateBitmapFromFile = nil;
  GDIPCreateBitmapFromStreamICM: TsgGDIPlusCreateBitmapFromStreamICM = nil;
  GDIPCreateBitmapFromFileICM: TsgGDIPlusCreateBitmapFromFileICM = nil;
  GDIPCreateBitmapFromScan0: TsgGDIPlusCreateBitmapFromScan0 = nil;
  GDIPCreateBitmapFromGraphics: TsgGDIPlusCreateBitmapFromGraphics = nil;
  GDIPCreateBitmapFromGdiDib: TsgGDIPlusCreateBitmapFromGdiDib = nil;
  GDIPCreateBitmapFromHBITMAP: TsgGDIPlusCreateBitmapFromHBITMAP = nil;
  GDIPCreateHBITMAPFromBitmap: TsgGDIPlusCreateHBITMAPFromBitmap = nil;
  GDIPCreateBitmapFromHICON: TsgGDIPlusCreateBitmapFromHICON = nil;
  GDIPCreateHICONFromBitmap: TsgGDIPlusCreateHICONFromBitmap = nil;
  GDIPCreateBitmapFromResource: TsgGDIPlusCreateBitmapFromResource = nil;
  GDIPCloneBitmapArea: TsgGDIPlusCloneBitmapArea = nil;
  GDIPCloneBitmapAreaI: TsgGDIPlusCloneBitmapAreaI = nil;
  GDIPBitmapLockBits: TsgGDIPlusBitmapLockBits = nil;
  GDIPBitmapUnlockBits: TsgGDIPlusBitmapUnlockBits = nil;
  GDIPBitmapGetPixel: TsgGDIPlusBitmapGetPixel = nil;
  GDIPBitmapSetPixel: TsgGDIPlusBitmapSetPixel = nil;
  GDIPBitmapSetResolution: TsgGDIPlusBitmapSetResolution = nil;
  GDIPCreateImageAttributes: TsgGDIPlusCreateImageAttributes = nil;
  GDIPCloneImageAttributes: TsgGDIPlusCloneImageAttributes = nil;
  GDIPDisposeImageAttributes: TsgGDIPlusDisposeImageAttributes = nil;
  GDIPSetImageAttributesToIdentity: TsgGDIPlusSetImageAttributesToIdentity = nil;
  GDIPResetImageAttributes: TsgGDIPlusResetImageAttributes = nil;
  GDIPSetImageAttributesColorMatrix: TsgGDIPlusSetImageAttributesColorMatrix = nil;
  GDIPSetImageAttributesThreshold: TsgGDIPlusSetImageAttributesThreshold = nil;
  GDIPSetImageAttributesGamma: TsgGDIPlusSetImageAttributesGamma = nil;
  GDIPSetImageAttributesNoOp: TsgGDIPlusSetImageAttributesNoOp = nil;
  GDIPSetImageAttributesColorKeys: TsgGDIPlusSetImageAttributesColorKeys = nil;
  GDIPSetImageAttributesOutputChannel: TsgGDIPlusSetImageAttributesOutputChannel = nil;
  GDIPSetImageAttributesOutputChannelColorProfile: TsgGDIPlusSetImageAttributesOutputChannelColorProfile = nil;
  GDIPSetImageAttributesRemapTable: TsgGDIPlusSetImageAttributesRemapTable = nil;
  GDIPSetImageAttributesWrapMode: TsgGDIPlusSetImageAttributesWrapMode = nil;
  GDIPSetImageAttributesICMMode: TsgGDIPlusSetImageAttributesICMMode = nil;
  GDIPGetImageAttributesAdjustedPalette: TsgGDIPlusGetImageAttributesAdjustedPalette = nil;
  GDIPFlush: TsgGDIPlusFlush = nil;
  GDIPCreateFromHDC: TsgGDIPlusCreateFromHDC = nil;
  GDIPCreateFromHDC2: TsgGDIPlusCreateFromHDC2 = nil;
  GDIPCreateFromHWND: TsgGDIPlusCreateFromHWND = nil;
  GDIPCreateFromHWNDICM: TsgGDIPlusCreateFromHWNDICM = nil;
  GDIPDeleteGraphics: TsgGDIPlusDeleteGraphics = nil;
  GDIPGetDC: TsgGDIPlusGetDC = nil;
  GDIPReleaseDC: TsgGDIPlusReleaseDC = nil;
  GDIPSetCompositingMode: TsgGDIPlusSetCompositingMode = nil;
  GDIPGetCompositingMode: TsgGDIPlusGetCompositingMode = nil;
  GDIPSetRenderingOrigin: TsgGDIPlusSetRenderingOrigin = nil;
  GDIPGetRenderingOrigin: TsgGDIPlusGetRenderingOrigin = nil;
  GDIPSetCompositingQuality: TsgGDIPlusSetCompositingQuality = nil;
  GDIPGetCompositingQuality: TsgGDIPlusGetCompositingQuality = nil;
  GDIPSetSmoothingMode: TsgGDIPlusSetSmoothingMode = nil;
  GDIPGetSmoothingMode: TsgGDIPlusGetSmoothingMode = nil;
  GDIPSetPixelOffsetMode: TsgGDIPlusSetPixelOffsetMode = nil;
  GDIPGetPixelOffsetMode: TsgGDIPlusGetPixelOffsetMode = nil;
  GDIPSetTextRenderingHint: TsgGDIPlusSetTextRenderingHint = nil;
  GDIPGetTextRenderingHint: TsgGDIPlusGetTextRenderingHint = nil;
  GDIPSetTextContrast: TsgGDIPlusSetTextContrast = nil;
  GDIPGetTextContrast: TsgGDIPlusGetTextContrast = nil;
  GDIPSetInterpolationMode: TsgGDIPlusSetInterpolationMode = nil;
  GDIPGetInterpolationMode: TsgGDIPlusGetInterpolationMode = nil;
  GDIPSetWorldTransform: TsgGDIPlusSetWorldTransform = nil;
  GDIPResetWorldTransform: TsgGDIPlusResetWorldTransform = nil;
  GDIPMultiplyWorldTransform: TsgGDIPlusMultiplyWorldTransform = nil;
  GDIPTranslateWorldTransform: TsgGDIPlusTranslateWorldTransform = nil;
  GDIPScaleWorldTransform: TsgGDIPlusScaleWorldTransform = nil;
  GDIPRotateWorldTransform: TsgGDIPlusRotateWorldTransform = nil;
  GDIPGetWorldTransform: TsgGDIPlusGetWorldTransform = nil;
  GDIPResetPageTransform: TsgGDIPlusResetPageTransform = nil;
  GDIPGetPageUnit: TsgGDIPlusGetPageUnit = nil;
  GDIPGetPageScale: TsgGDIPlusGetPageScale = nil;
  GDIPSetPageUnit: TsgGDIPlusSetPageUnit = nil;
  GDIPSetPageScale: TsgGDIPlusSetPageScale = nil;
  GDIPGetDpiX: TsgGDIPlusGetDpiX = nil;
  GDIPGetDpiY: TsgGDIPlusGetDpiY = nil;
  GDIPTransformPoints: TsgGDIPlusTransformPoints = nil;
  GDIPTransformPointsI: TsgGDIPlusTransformPointsI = nil;
  GDIPGetNearestColor: TsgGDIPlusGetNearestColor = nil;
  GDIPCreateHalftonePalette: TsgGDIPlusCreateHalftonePalette = nil;
  GDIPDrawLine: TsgGDIPlusDrawLine = nil;
  GDIPDrawLineI: TsgGDIPlusDrawLineI = nil;
  GDIPDrawLines: TsgGDIPlusDrawLines = nil;
  GDIPDrawLinesI: TsgGDIPlusDrawLinesI = nil;
  GDIPDrawArc: TsgGDIPlusDrawArc = nil;
  GDIPDrawArcI: TsgGDIPlusDrawArcI = nil;
  GDIPDrawBezier: TsgGDIPlusDrawBezier = nil;
  GDIPDrawBezierI: TsgGDIPlusDrawBezierI = nil;
  GDIPDrawBeziers: TsgGDIPlusDrawBeziers = nil;
  GDIPDrawBeziersI: TsgGDIPlusDrawBeziersI = nil;
  GDIPDrawRectangle: TsgGDIPlusDrawRectangle = nil;
  GDIPDrawRectangleI: TsgGDIPlusDrawRectangleI = nil;
  GDIPDrawRectangles: TsgGDIPlusDrawRectangles = nil;
  GDIPDrawRectanglesI: TsgGDIPlusDrawRectanglesI = nil;
  GDIPDrawEllipse: TsgGDIPlusDrawEllipse = nil;
  GDIPDrawEllipseI: TsgGDIPlusDrawEllipseI = nil;
  GDIPDrawPie: TsgGDIPlusDrawPie = nil;
  GDIPDrawPieI: TsgGDIPlusDrawPieI = nil;
  GDIPDrawPolygon: TsgGDIPlusDrawPolygon = nil;
  GDIPDrawPolygonI: TsgGDIPlusDrawPolygonI = nil;
  GDIPDrawPath: TsgGDIPlusDrawPath = nil;
  GDIPDrawCurve: TsgGDIPlusDrawCurve = nil;
  GDIPDrawCurveI: TsgGDIPlusDrawCurveI = nil;
  GDIPDrawCurve2: TsgGDIPlusDrawCurve2 = nil;
  GDIPDrawCurve2I: TsgGDIPlusDrawCurve2I = nil;
  GDIPDrawCurve3: TsgGDIPlusDrawCurve3 = nil;
  GDIPDrawCurve3I: TsgGDIPlusDrawCurve3I = nil;
  GDIPDrawClosedCurve: TsgGDIPlusDrawClosedCurve = nil;
  GDIPDrawClosedCurveI: TsgGDIPlusDrawClosedCurveI = nil;
  GDIPDrawClosedCurve2: TsgGDIPlusDrawClosedCurve2 = nil;
  GDIPDrawClosedCurve2I: TsgGDIPlusDrawClosedCurve2I = nil;
  GDIPGraphicsClear: TsgGDIPlusGraphicsClear = nil;
  GDIPFillRectangle: TsgGDIPlusFillRectangle = nil;
  GDIPFillRectangleI: TsgGDIPlusFillRectangleI = nil;
  GDIPFillRectangles: TsgGDIPlusFillRectangles = nil;
  GDIPFillRectanglesI: TsgGDIPlusFillRectanglesI = nil;
  GDIPFillPolygon: TsgGDIPlusFillPolygon = nil;
  GDIPFillPolygonI: TsgGDIPlusFillPolygonI = nil;
  GDIPFillPolygon2: TsgGDIPlusFillPolygon2 = nil;
  GDIPFillPolygon2I: TsgGDIPlusFillPolygon2I = nil;
  GDIPFillEllipse: TsgGDIPlusFillEllipse = nil;
  GDIPFillEllipseI: TsgGDIPlusFillEllipseI = nil;
  GDIPFillPie: TsgGDIPlusFillPie = nil;
  GDIPFillPieI: TsgGDIPlusFillPieI = nil;
  GDIPFillPath: TsgGDIPlusFillPath = nil;
  GDIPFillClosedCurve: TsgGDIPlusFillClosedCurve = nil;
  GDIPFillClosedCurveI: TsgGDIPlusFillClosedCurveI = nil;
  GDIPFillClosedCurve2: TsgGDIPlusFillClosedCurve2 = nil;
  GDIPFillClosedCurve2I: TsgGDIPlusFillClosedCurve2I = nil;
  GDIPFillRegion: TsgGDIPlusFillRegion = nil;
  GDIPDrawImage: TsgGDIPlusDrawImage = nil;
  GDIPDrawImageI: TsgGDIPlusDrawImageI = nil;
  GDIPDrawImageRect: TsgGDIPlusDrawImageRect = nil;
  GDIPDrawImageRectI: TsgGDIPlusDrawImageRectI = nil;
  GDIPDrawImagePoints: TsgGDIPlusDrawImagePoints = nil;
  GDIPDrawImagePointsI: TsgGDIPlusDrawImagePointsI = nil;
  GDIPDrawImagePointRect: TsgGDIPlusDrawImagePointRect = nil;
  GDIPDrawImagePointRectI: TsgGDIPlusDrawImagePointRectI = nil;
  GDIPDrawImageRectRect: TsgGDIPlusDrawImageRectRect = nil;
  GDIPDrawImageRectRectI: TsgGDIPlusDrawImageRectRectI = nil;
  GDIPDrawImagePointsRect: TsgGDIPlusDrawImagePointsRect = nil;
  GDIPDrawImagePointsRectI: TsgGDIPlusDrawImagePointsRectI = nil;
  GDIPEnumerateMetafileDestPoint: TsgGDIPlusEnumerateMetafileDestPoint = nil;
  GDIPEnumerateMetafileDestPointI: TsgGDIPlusEnumerateMetafileDestPointI = nil;
  GDIPEnumerateMetafileDestRect: TsgGDIPlusEnumerateMetafileDestRect = nil;
  GDIPEnumerateMetafileDestRectI: TsgGDIPlusEnumerateMetafileDestRectI = nil;
  GDIPEnumerateMetafileDestPoints: TsgGDIPlusEnumerateMetafileDestPoints = nil;
  GDIPEnumerateMetafileDestPointsI: TsgGDIPlusEnumerateMetafileDestPointsI = nil;
  GDIPEnumerateMetafileSrcRectDestPoint: TsgGDIPlusEnumerateMetafileSrcRectDestPoint = nil;
  GDIPEnumerateMetafileSrcRectDestPointI: TsgGDIPlusEnumerateMetafileSrcRectDestPointI = nil;
  GDIPEnumerateMetafileSrcRectDestRect: TsgGDIPlusEnumerateMetafileSrcRectDestRect = nil;
  GDIPEnumerateMetafileSrcRectDestRectI: TsgGDIPlusEnumerateMetafileSrcRectDestRectI = nil;
  GDIPEnumerateMetafileSrcRectDestPoints: TsgGDIPlusEnumerateMetafileSrcRectDestPoints = nil;
  GDIPEnumerateMetafileSrcRectDestPointsI: TsgGDIPlusEnumerateMetafileSrcRectDestPointsI = nil;
  GDIPPlayMetafileRecord: TsgGDIPlusPlayMetafileRecord = nil;
  GDIPSetClipGraphics: TsgGDIPlusSetClipGraphics = nil;
  GDIPSetClipRect: TsgGDIPlusSetClipRect = nil;
  GDIPSetClipRectI: TsgGDIPlusSetClipRectI = nil;
  GDIPSetClipPath: TsgGDIPlusSetClipPath = nil;
  GDIPSetClipRegion: TsgGDIPlusSetClipRegion = nil;
  GDIPSetClipHrgn: TsgGDIPlusSetClipHrgn = nil;
  GDIPResetClip: TsgGDIPlusResetClip = nil;
  GDIPTranslateClip: TsgGDIPlusTranslateClip = nil;
  GDIPTranslateClipI: TsgGDIPlusTranslateClipI = nil;
  GDIPGetClip: TsgGDIPlusGetClip = nil;
  GDIPGetClipBounds: TsgGDIPlusGetClipBounds = nil;
  GDIPGetClipBoundsI: TsgGDIPlusGetClipBoundsI = nil;
  GDIPIsClipEmpty: TsgGDIPlusIsClipEmpty = nil;
  GDIPGetVisibleClipBounds: TsgGDIPlusGetVisibleClipBounds = nil;
  GDIPGetVisibleClipBoundsI: TsgGDIPlusGetVisibleClipBoundsI = nil;
  GDIPIsVisibleClipEmpty: TsgGDIPlusIsVisibleClipEmpty = nil;
  GDIPIsVisiblePoint: TsgGDIPlusIsVisiblePoint = nil;
  GDIPIsVisiblePointI: TsgGDIPlusIsVisiblePointI = nil;
  GDIPIsVisibleRect: TsgGDIPlusIsVisibleRect = nil;
  GDIPIsVisibleRectI: TsgGDIPlusIsVisibleRectI = nil;
  GDIPSaveGraphics: TsgGDIPlusSaveGraphics = nil;
  GDIPRestoreGraphics: TsgGDIPlusRestoreGraphics = nil;
  GDIPBeginContainer: TsgGDIPlusBeginContainer = nil;
  GDIPBeginContainerI: TsgGDIPlusBeginContainerI = nil;
  GDIPBeginContainer2: TsgGDIPlusBeginContainer2 = nil;
  GDIPEndContainer: TsgGDIPlusEndContainer = nil;
  GDIPGetMetafileHeaderFromWmf: TsgGDIPlusGetMetafileHeaderFromWmf = nil;
  GDIPGetMetafileHeaderFromEmf: TsgGDIPlusGetMetafileHeaderFromEmf = nil;
  GDIPGetMetafileHeaderFromFile: TsgGDIPlusGetMetafileHeaderFromFile = nil;
  GDIPGetMetafileHeaderFromStream: TsgGDIPlusGetMetafileHeaderFromStream = nil;
  GDIPGetMetafileHeaderFromMetafile: TsgGDIPlusGetMetafileHeaderFromMetafile = nil;
  GDIPGetHemfFromMetafile: TsgGDIPlusGetHemfFromMetafile = nil;
  GDIPCreateStreamOnFile: TsgGDIPlusCreateStreamOnFile = nil;
  GDIPCreateMetafileFromWmf: TsgGDIPlusCreateMetafileFromWmf = nil;
  GDIPCreateMetafileFromEmf: TsgGDIPlusCreateMetafileFromEmf = nil;
  GDIPCreateMetafileFromFile: TsgGDIPlusCreateMetafileFromFile = nil;
  GDIPCreateMetafileFromWmfFile: TsgGDIPlusCreateMetafileFromWmfFile = nil;
  GDIPCreateMetafileFromStream: TsgGDIPlusCreateMetafileFromStream = nil;
  GDIPRecordMetafile: TsgGDIPlusRecordMetafile = nil;
  GDIPRecordMetafileI: TsgGDIPlusRecordMetafileI = nil;
  GDIPRecordMetafileFileName: TsgGDIPlusRecordMetafileFileName = nil;
  GDIPRecordMetafileFileNameI: TsgGDIPlusRecordMetafileFileNameI = nil;
  GDIPRecordMetafileStream: TsgGDIPlusRecordMetafileStream = nil;
  GDIPRecordMetafileStreamI: TsgGDIPlusRecordMetafileStreamI = nil;
  GDIPSetMetafileDownLevelRasterizationLimit: TsgGDIPlusSetMetafileDownLevelRasterizationLimit = nil;
  GDIPGetMetafileDownLevelRasterizationLimit: TsgGDIPlusGetMetafileDownLevelRasterizationLimit = nil;
  GDIPGetImageDecodersSize: TsgGDIPlusGetImageDecodersSize = nil;
  GDIPGetImageDecoders: TsgGDIPlusGetImageDecoders = nil;
  GDIPGetImageEncodersSize: TsgGDIPlusGetImageEncodersSize = nil;
  GDIPGetImageEncoders: TsgGDIPlusGetImageEncoders = nil;
  GDIPComment: TsgGDIPlusComment = nil;
  GDIPCreateFontFamilyFromName: TsgGDIPlusCreateFontFamilyFromName = nil;
  GDIPDeleteFontFamily: TsgGDIPlusDeleteFontFamily = nil;
  GDIPCloneFontFamily: TsgGDIPlusCloneFontFamily = nil;
  GDIPGetGenericFontFamilySansSerif: TsgGDIPlusGetGenericFontFamilySansSerif = nil;
  GDIPGetGenericFontFamilySerif: TsgGDIPlusGetGenericFontFamilySerif = nil;
  GDIPGetGenericFontFamilyMonospace: TsgGDIPlusGetGenericFontFamilyMonospace = nil;
  GDIPGetFamilyName: TsgGDIPlusGetFamilyName = nil;
  GDIPIsStyleAvailable: TsgGDIPlusIsStyleAvailable = nil;
  GDIPFontCollectionEnumerable: TsgGDIPlusFontCollectionEnumerable = nil;
  GDIPFontCollectionEnumerate: TsgGDIPlusFontCollectionEnumerate = nil;
  GDIPGetEmHeight: TsgGDIPlusGetEmHeight = nil;
  GDIPGetCellAscent: TsgGDIPlusGetCellAscent = nil;
  GDIPGetCellDescent: TsgGDIPlusGetCellDescent = nil;
  GDIPGetLineSpacing: TsgGDIPlusGetLineSpacing = nil;
  GDIPCreateFontFromDC: TsgGDIPlusCreateFontFromDC = nil;
  GDIPCreateFontFromLogfontA: TsgGDIPlusCreateFontFromLogfontA = nil;
  GDIPCreateFontFromLogfontW: TsgGDIPlusCreateFontFromLogfontW = nil;
  GDIPCreateFont: TsgGDIPlusCreateFont = nil;
  GDIPCloneFont: TsgGDIPlusCloneFont = nil;
  GDIPDeleteFont: TsgGDIPlusDeleteFont = nil;
  GDIPGetFamily: TsgGDIPlusGetFamily = nil;
  GDIPGetFontStyle: TsgGDIPlusGetFontStyle = nil;
  GDIPGetFontSize: TsgGDIPlusGetFontSize = nil;
  GDIPGetFontUnit: TsgGDIPlusGetFontUnit = nil;
  GDIPGetFontHeight: TsgGDIPlusGetFontHeight = nil;
  GDIPGetFontHeightGivenDPI: TsgGDIPlusGetFontHeightGivenDPI = nil;
  GDIPGetLogFontA: TsgGDIPlusGetLogFontA = nil;
  GDIPGetLogFontW: TsgGDIPlusGetLogFontW = nil;
  GDIPNewInstalledFontCollection: TsgGDIPlusNewInstalledFontCollection = nil;
  GDIPNewPrivateFontCollection: TsgGDIPlusNewPrivateFontCollection = nil;
  GDIPDeletePrivateFontCollection: TsgGDIPlusDeletePrivateFontCollection = nil;
  GDIPGetFontCollectionFamilyCount: TsgGDIPlusGetFontCollectionFamilyCount = nil;
  GDIPGetFontCollectionFamilyList: TsgGDIPlusGetFontCollectionFamilyList = nil;
  GDIPPrivateAddFontFile: TsgGDIPlusPrivateAddFontFile = nil;
  GDIPPrivateAddMemoryFont: TsgGDIPlusPrivateAddMemoryFont = nil;
  GDIPDrawString: TsgGDIPlusDrawString = nil;
  GDIPMeasureString: TsgGDIPlusMeasureString = nil;
  GDIPMeasureCharacterRanges: TsgGDIPlusMeasureCharacterRanges = nil;
  GDIPDrawDriverString: TsgGDIPlusDrawDriverString = nil;
  GDIPMeasureDriverString: TsgGDIPlusMeasureDriverString = nil;
  GDIPCreateStringFormat: TsgGDIPlusCreateStringFormat = nil;
  GDIPStringFormatGetGenericDefault: TsgGDIPlusStringFormatGetGenericDefault = nil;
  GDIPStringFormatGetGenericTypographic: TsgGDIPlusStringFormatGetGenericTypographic = nil;
  GDIPDeleteStringFormat: TsgGDIPlusDeleteStringFormat = nil;
  GDIPCloneStringFormat: TsgGDIPlusCloneStringFormat = nil;
  GDIPSetStringFormatFlags: TsgGDIPlusSetStringFormatFlags = nil;
  GDIPGetStringFormatFlags: TsgGDIPlusGetStringFormatFlags = nil;
  GDIPSetStringFormatAlign: TsgGDIPlusSetStringFormatAlign = nil;
  GDIPGetStringFormatAlign: TsgGDIPlusGetStringFormatAlign = nil;
  GDIPSetStringFormatLineAlign: TsgGDIPlusSetStringFormatLineAlign = nil;
  GDIPGetStringFormatLineAlign: TsgGDIPlusGetStringFormatLineAlign = nil;
  GDIPSetStringFormatTrimming: TsgGDIPlusSetStringFormatTrimming = nil;
  GDIPGetStringFormatTrimming: TsgGDIPlusGetStringFormatTrimming = nil;
  GDIPSetStringFormatHotkeyPrefix: TsgGDIPlusSetStringFormatHotkeyPrefix = nil;
  GDIPGetStringFormatHotkeyPrefix: TsgGDIPlusGetStringFormatHotkeyPrefix = nil;
  GDIPSetStringFormatTabStops: TsgGDIPlusSetStringFormatTabStops = nil;
  GDIPGetStringFormatTabStops: TsgGDIPlusGetStringFormatTabStops = nil;
  GDIPGetStringFormatTabStopCount: TsgGDIPlusGetStringFormatTabStopCount = nil;
  GDIPSetStringFormatDigitSubstitution: TsgGDIPlusSetStringFormatDigitSubstitution = nil;
  GDIPGetStringFormatDigitSubstitution: TsgGDIPlusGetStringFormatDigitSubstitution = nil;
  GDIPGetStringFormatMeasurableCharacterRangeCount: TsgGDIPlusGetStringFormatMeasurableCharacterRangeCount = nil;
  GDIPSetStringFormatMeasurableCharacterRanges: TsgGDIPlusSetStringFormatMeasurableCharacterRanges = nil;
//  GDIPCreateCachedBitmap: TsgGDIPlusCreateCachedBitmap = nil;
//  GDIPDeleteCachedBitmap: TsgGDIPlusDeleteCachedBitmap = nil;
//  GDIPDrawCachedBitmap: TsgGDIPlusDrawCachedBitmap = nil;
  GDIPEmfToWmfBits: TsgGDIPlusEmfToWmfBits = nil;

function GetImageDecodersSize(out NumDecoders, Size: UINT): TsgGDIPStatus;
begin
  Result := GdipGetImageDecodersSize(NumDecoders, Size);
end;

function GetImageDecoders(NumDecoders, Size: UINT; Decoders: PImageCodecInfo): TsgGDIPStatus;
begin
  Result := GdipGetImageDecoders(NumDecoders, Size, Decoders);
end;

function GetImageEncodersSize(out NumEncoders, Size: UINT): TsgGDIPStatus;
begin
  Result := GdipGetImageEncodersSize(NumEncoders, Size);
end;

function GetImageEncoders(NumEncoders, Size: UINT; Encoders: PImageCodecInfo): TsgGDIPStatus;
begin
  Result := GdipGetImageEncoders(NumEncoders, Size, Encoders);
end;

{ Classes of GDIPlus }

constructor TsgGDIPlusImageAttributes.Create;
begin
  NativeImageAttr := nil;
  FStatusOfFunction := GdipCreateImageAttributes(NativeImageAttr);
end;

destructor TsgGDIPlusImageAttributes.Destroy;
begin
  GdipDisposeImageAttributes(NativeImageAttr);
  inherited Destroy;
end;

function TsgGDIPlusImageAttributes.Clone: TsgGDIPlusImageAttributes;
var Clone: TsgGDIPImageAttributes;
begin
  SetStatus(GdipCloneImageAttributes(NativeImageAttr, Clone));
  Result := TsgGDIPlusImageAttributes.Create(Clone, FStatusOfFunction);
end;

function TsgGDIPlusImageAttributes.SetToIdentity(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesToIdentity(NativeImageAttr, Type_));
end;

function TsgGDIPlusImageAttributes.Reset(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipResetImageAttributes(NativeImageAttr, Type_));
end;

function TsgGDIPlusImageAttributes.SetColorMatrix(const ColorMatrix: TColorMatrix;
  Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(NativeImageAttr, Type_, True, @ColorMatrix, nil, Mode));
end;

function TsgGDIPlusImageAttributes.ClearColorMatrix(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(NativeImageAttr, Type_, False, nil, nil, ColorMatrixFlagsDefault));
end;


function TsgGDIPlusImageAttributes.SetColorMatrices(const AColorMatrix: TColorMatrix;
  const AGrayMatrix: TColorMatrix; Mode: TColorMatrixFlags  = ColorMatrixFlagsDefault;
  Type_: TColorAdjustType  = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(NativeImageAttr, Type_, True, @AColorMatrix, @AGrayMatrix, Mode));
end;

function TsgGDIPlusImageAttributes.ClearColorMatrices(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(NativeImageAttr, Type_, False, nil, nil, ColorMatrixFlagsDefault));
end;

function TsgGDIPlusImageAttributes.SetThreshold(threshold: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold( NativeImageAttr, Type_, True, threshold));
end;

function TsgGDIPlusImageAttributes.ClearThreshold(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(NativeImageAttr, Type_, False, 0.0));
end;

function TsgGDIPlusImageAttributes.SetGamma(gamma: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(NativeImageAttr, Type_, True, gamma));
end;

function TsgGDIPlusImageAttributes.ClearGamma(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(NativeImageAttr, Type_, False, 0.0));
end;

function TsgGDIPlusImageAttributes.SetNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(NativeImageAttr, Type_, True));
end;

function TsgGDIPlusImageAttributes.ClearNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp( NativeImageAttr, Type_, False));
end;

function TsgGDIPlusImageAttributes.SetColorKey(ColorLow, ColorHigh: TsgColor; Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(NativeImageAttr, Type_, True, ColorLow, ColorHigh));
end;

function TsgGDIPlusImageAttributes.ClearColorKey(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(NativeImageAttr, Type_, False, 0, 0));
end;

function TsgGDIPlusImageAttributes.SetOutputChannel(ChannelFlags: TColorChannelFlags;
      Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(NativeImageAttr, Type_, True, ChannelFlags));
end;

function TsgGDIPlusImageAttributes.ClearOutputChannel(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(NativeImageAttr, Type_, False, ColorChannelFlagsLast));
end;

function TsgGDIPlusImageAttributes.SetOutputChannelColorProFile(ColorProFileFilename: WideString;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProFile(NativeImageAttr, Type_, True, PWideChar(ColorProFileFilename)));
end;

function TsgGDIPlusImageAttributes.ClearOutputChannelColorProFile(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProFile(NativeImageAttr, Type_, False, nil));
end;

function TsgGDIPlusImageAttributes.SetRemapTable(mapSize: Cardinal; map: PColorMap;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(NativeImageAttr, Type_, True, mapSize, map));
end;

function TsgGDIPlusImageAttributes.ClearRemapTable(Type_: TColorAdjustType = ColorAdjustTypeDefault): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(NativeImageAttr, Type_, False, 0, nil));
end;

function TsgGDIPlusImageAttributes.SetBrushRemapTable(mapSize: Cardinal; Map: PColorMap): TsgGDIPStatus;
begin
  Result := SetRemapTable(mapSize, Map, ColorAdjustTypeBrush);
end;

function TsgGDIPlusImageAttributes.ClearBrushRemapTable: TsgGDIPStatus;
begin
  Result := ClearRemapTable(ColorAdjustTypeBrush);
end;

function TsgGDIPlusImageAttributes.SetWrapMode(Wrap: TsgGDIPWrapMode; Color: TsgColor = aclBlack; Clamp: BOOL = False): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImageAttributesWrapMode(NativeImageAttr, Wrap, Color, Clamp));
end;

function TsgGDIPlusImageAttributes.GetAdjustedPalette(ColorPalette: PColorPalette; ColorAdjustType: TColorAdjustType): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetImageAttributesAdjustedPalette(NativeImageAttr, ColorPalette, ColorAdjustType));
end;

function TsgGDIPlusImageAttributes.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusImageAttributes.Create(ImageAttr: TsgGDIPImageAttributes; Status: TsgGDIPStatus);
begin
  SetNativeImageAttr(ImageAttr);
  FStatusOfFunction := Status;
end;

procedure TsgGDIPlusImageAttributes.SetNativeImageAttr(NativeImageAttr: TsgGDIPImageAttributes);
begin
  Self.NativeImageAttr := NativeImageAttr;
end;

function TsgGDIPlusImageAttributes.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then FStatusOfFunction := Status;
    Result := Status;
end;

constructor TsgGDIPlusMatrix.Create;
var Matrix: TsgGDIPMatrix;
begin
  Matrix := nil;
  FStatusOfFunction := GdipCreateMatrix(Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TsgGDIPlusMatrix.Create(const M11, M12, M21, M22, Dx, Dy: Single);
var Matrix: TsgGDIPMatrix;
begin
  Matrix := nil;
  FStatusOfFunction := GdipCreateMatriX2(M11, M12, M21, M22, Dx, Dy, Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TsgGDIPlusMatrix.Create(const Rect: TsgGRectF; const DstPlg: TsgGPointF);
var Matrix: TsgGDIPMatrix;
begin
  Matrix := nil;
  FStatusOfFunction := GdipCreateMatriX3(@Rect, @DstPlg, Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TsgGDIPlusMatrix.Create(const Rect: TsgGRect; const DstPlg: TsgGPoint);
var Matrix: TsgGDIPMatrix;
begin
  Matrix := nil;
  FStatusOfFunction := GdipCreateMatriX3I(@Rect, @DstPlg, Matrix);
  SetNativeMatrix(Matrix);
end;

destructor TsgGDIPlusMatrix.Destroy;
begin
  GdipDeleteMatrix(NativeMatrix);
end;

function TsgGDIPlusMatrix.Clone: TsgGDIPlusMatrix;
var CloneMatrix: TsgGDIPMatrix;
begin
  CloneMatrix := nil;
  SetStatus(GdipCloneMatrix(NativeMatrix, CloneMatrix));
  if (FStatusOfFunction <> Ok) then
  begin
    Result := nil;
    exit;
  end;
  Result := TsgGDIPlusMatrix.Create(CloneMatrix);
end;

function TsgGDIPlusMatrix.GetElements(const m: TMatrixArray): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetMatrixElements(NativeMatrix, @m));
end;

function TsgGDIPlusMatrix.SetElements(M11, M12, M21, M22, Dx, Dy: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetMatrixElements(NativeMatrix, M11, M12, M21, M22, Dx, Dy));
end;

function TsgGDIPlusMatrix.OffsetX: Single;
var Elements: TMatrixArray;
begin
  if (GetElements(Elements) = Ok) then
    Result := Elements[4]
  else
    Result := 0.0;
end;

function TsgGDIPlusMatrix.OffsetY: Single;
var Elements: TMatrixArray;
begin
  if (GetElements(Elements) = Ok) then
    Result := Elements[5]
  else
    Result := 0.0;
end;

function TsgGDIPlusMatrix.Reset: TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetMatrixElements(NativeMatrix, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0));
end;

function TsgGDIPlusMatrix.Multiply(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipMultiplyMatrix(NativeMatrix, AMatrix.NativeMatrix, Order));
end;

function TsgGDIPlusMatrix.Translate(OffsetX, OffsetY: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslateMatrix(NativeMatrix, OffsetX, OffsetY, Order));
end;

function TsgGDIPlusMatrix.Scale(ScaleX, ScaleY: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipScaleMatrix(NativeMatrix, ScaleX, ScaleY, Order));
end;

function TsgGDIPlusMatrix.Rotate(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipRotateMatrix(NativeMatrix, Angle, Order));
end;

function TsgGDIPlusMatrix.RotateAt(Angle: Single; const center: TsgGPointF; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  if(Order = MatrixOrderPrePend) then
  begin
    SetStatus(GdipTranslateMatrix(NativeMatrix, center.X, center.Y, Order));
    SetStatus(GdipRotateMatrix(NativeMatrix, Angle, Order));
    Result := SetStatus(GdipTranslateMatrix(NativeMatrix, -center.X, -center.Y, Order));
  end
  else
  begin
    SetStatus(GdipTranslateMatrix(NativeMatrix, - center.X, - center.Y, Order));
    SetStatus(GdipRotateMatrix(NativeMatrix, Angle, Order));
    Result := SetStatus(GdipTranslateMatrix(NativeMatrix, center.X, center.Y, Order));
  end;
end;

function TsgGDIPlusMatrix.Shear(ShearX, ShearY: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipShearMatrix(NativeMatrix, ShearX, ShearY, Order));
end;

function TsgGDIPlusMatrix.Invert: TsgGDIPStatus;
begin
  Result := SetStatus(GdipInvertMatrix(NativeMatrix));
end;

function TsgGDIPlusMatrix.TransformPoints(Pts: PsgGPointF; Count: Integer = 1): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTransformMatrixPoints(NativeMatrix, Pts, Count));
end;

function TsgGDIPlusMatrix.TransformPoints(Pts: PsgGPoint; Count: Integer = 1): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTransformMatrixPointsI(NativeMatrix, Pts, Count));
end;

function TsgGDIPlusMatrix.TransformVectors(Pts: PsgGPointF; Count: Integer = 1): TsgGDIPStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPoints( NativeMatrix, Pts, Count));
end;

function TsgGDIPlusMatrix.TransformVectors(Pts: PsgGPoint; Count: Integer = 1): TsgGDIPStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPointsI(NativeMatrix, Pts, Count));
end;

function TsgGDIPlusMatrix.IsInvertible: BOOL;
begin
  Result := False;
  SetStatus(GdipIsMatrixInvertible(NativeMatrix, Result));
end;

function TsgGDIPlusMatrix.IsIdentity: BOOL;
begin
  Result := False;
  SetStatus(GdipIsMatrixIdentity(NativeMatrix, Result));
end;

function TsgGDIPlusMatrix.EqualsMat(const AMatrix: TsgGDIPlusMatrix): BOOL;
begin
  Result := False;
  SetStatus(GdipIsMatrixEqual(NativeMatrix, AMatrix.NativeMatrix, Result));
end;

function TsgGDIPlusMatrix.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusMatrix.Create(NativeMatrix: TsgGDIPMatrix);
begin
  FStatusOfFunction := Ok;
  SetNativeMatrix(NativeMatrix);
end;

procedure TsgGDIPlusMatrix.SetNativeMatrix(NativeMatrix: TsgGDIPMatrix);
begin
  Self.NativeMatrix := NativeMatrix;
end;

function TsgGDIPlusMatrix.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

constructor TsgGDIPlusStringFormat.Create(FormatFlags: Integer = 0; Language: LANGID = LANG_NEUTRAL);
begin
  NativeFormat := nil;
  FStatusOfFunction := GdipCreateStringFormat(FormatFlags, Language, NativeFormat);
end;

class function TsgGDIPlusStringFormat.GenericDefault: TsgGDIPlusStringFormat;
begin
  if not Assigned(GenericDefaultStringFormatBuffer) then
  begin
    GenericDefaultStringFormatBuffer := TsgGDIPlusStringFormat.Create;
    GenericDefaultStringFormatBuffer.FStatusOfFunction := GdipStringFormatGetGenericDefault(GenericDefaultStringFormatBuffer.NativeFormat);
  end;
  Result := GenericDefaultStringFormatBuffer;
end;

class function TsgGDIPlusStringFormat.GenericTypographic: TsgGDIPlusStringFormat;
begin
  if not Assigned(GenericTypographicStringFormatBuffer) then
  begin
    GenericTypographicStringFormatBuffer := TsgGDIPlusStringFormat.Create;
    GenericTypographicStringFormatBuffer.FStatusOfFunction := GdipStringFormatGetGenericTypographic(GenericTypographicStringFormatBuffer.NativeFormat);
  end;
  Result := GenericTypographicStringFormatBuffer;
end;

constructor TsgGDIPlusStringFormat.Create(Format: TsgGDIPlusStringFormat);
var
  vStrF: TsgGDIPFontCollection;
begin
  NativeFormat := nil;
  if Assigned(Format) then
    vStrF := Format.NativeFormat
  else
    vStrF := nil;
  FStatusOfFunction := GdipCloneStringFormat(vStrF, NativeFormat);
end;

function TsgGDIPlusStringFormat.Clone: TsgGDIPlusStringFormat;
var
  ClonedStringFormat: TsgGDIPStringFormat;
begin
  ClonedStringFormat := nil;
  FStatusOfFunction := GdipCloneStringFormat(NativeFormat, ClonedStringFormat);
  if (FStatusOfFunction = Ok) then
    Result := TsgGDIPlusStringFormat.Create(ClonedStringFormat, FStatusOfFunction)
  else
    Result := nil;
end;

destructor TsgGDIPlusStringFormat.Destroy;
begin
  GdipDeleteStringFormat(NativeFormat);
end;

function TsgGDIPlusStringFormat.SetFormatFlags(Flags: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetStringFormatFlags(NativeFormat, Flags));
end;

function TsgGDIPlusStringFormat.GetFormatFlags: Integer;
begin
  SetStatus(GdipGetStringFormatFlags(NativeFormat, Result));
end;

function TsgGDIPlusStringFormat.SetAlignment(align: TStringAlignment): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetStringFormatAlign(NativeFormat, align));
end;

function TsgGDIPlusStringFormat.GetAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatAlign(NativeFormat, Result));
end;

function TsgGDIPlusStringFormat.SetLineAlignment(align: TStringAlignment): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetStringFormatLineAlign(NativeFormat, align));
end;

function TsgGDIPlusStringFormat.GetLineAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatLineAlign(NativeFormat, Result));
end;

function TsgGDIPlusStringFormat.SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetStringFormatHotkeyPrefix(NativeFormat, Integer(hotkeyPrefix)));
end;

function TsgGDIPlusStringFormat.GetHotkeyPrefix: THotkeyPrefix;
var
  HotkeyPrefix: Integer;
begin
  SetStatus(GdipGetStringFormatHotkeyPrefix(NativeFormat, HotkeyPrefix));
  Result := THotkeyPrefix(HotkeyPrefix);
end;

function TsgGDIPlusStringFormat.SetTabStops(firstTabOffset: Single; Count: Integer; TabStops: PSingle): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetStringFormatTabStops(NativeFormat, FirstTabOffset, Count, TabStops));
end;

function TsgGDIPlusStringFormat.GetTabStopCount: Integer;
begin
  SetStatus(GdipGetStringFormatTabStopCount(NativeFormat, Result));
end;

function TsgGDIPlusStringFormat.GetTabStops(Count: Integer; FirstTabOffset, TabStops: PSingle): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetStringFormatTabStops(NativeFormat, Count, FirstTabOffset, TabStops));
end;

function TsgGDIPlusStringFormat.SetDigitSubstitution(Language: LANGID; substitute: TStringDigitSubstitute): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetStringFormatDigitSubstitution(NativeFormat, Language, substitute));
end;

function TsgGDIPlusStringFormat.GetDigitSubstitutionLanguage: LANGID;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(NativeFormat, @Result, nil));
end;

function TsgGDIPlusStringFormat.GetDigitSubstitutionMethod: TStringDigitSubstitute;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(NativeFormat, nil, @Result));
end;

function TsgGDIPlusStringFormat.SetTrimming(Trimming: TStringTrimming): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetStringFormatTrimming(NativeFormat, Trimming));
end;

function TsgGDIPlusStringFormat.GetTrimming: TStringTrimming;
begin
  SetStatus(GdipGetStringFormatTrimming(NativeFormat, Result));
end;

function TsgGDIPlusStringFormat.SetMeasurableCharacterRanges(RangeCount: Integer; Ranges: PCharacterRange): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetStringFormatMeasurableCharacterRanges(NativeFormat, RangeCount, Ranges));
end;

function TsgGDIPlusStringFormat.GetMeasurableCharacterRangeCount: Integer;
begin
  SetStatus(GdipGetStringFormatMeasurableCharacterRangeCount(NativeFormat, Result));
end;

function TsgGDIPlusStringFormat.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

function TsgGDIPlusStringFormat.SetStatus(NewStatus: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (NewStatus <> Ok) then FStatusOfFunction := NewStatus;
  Result := NewStatus;
end;

procedure TsgGDIPlusStringFormat.Assign(Source: TsgGDIPlusStringFormat);
begin
  assert(Assigned(Source));
  GdipDeleteStringFormat(NativeFormat);
  FStatusOfFunction := GdipCloneStringFormat(Source.NativeFormat, NativeFormat);
end;

constructor TsgGDIPlusStringFormat.Create(ClonedStringFormat: TsgGDIPStringFormat; Status: TsgGDIPStatus);
begin
  FStatusOfFunction := Status;
  NativeFormat := ClonedStringFormat;
end;

constructor TsgGDIPlusAdjustableArrowCap.Create(Height, Width: Single; isFilled: Bool = True);
var
  Cap: TsgGDIPAdjustableArrowCap;
begin
  Cap := nil;
  FStatusOfFunction := GdipCreateAdjustableArrowCap(Height, Width, isFilled, Cap);
  SetNativeCap(Cap);
end;

function TsgGDIPlusAdjustableArrowCap.SetHeight(Height: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapHeight(TsgGDIPAdjustableArrowCap(NativeCap), Height));
end;

function TsgGDIPlusAdjustableArrowCap.GetHeight: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapHeight(TsgGDIPAdjustableArrowCap(NativeCap), Result));
end;

function TsgGDIPlusAdjustableArrowCap.SetWidth(Width: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapWidth(TsgGDIPAdjustableArrowCap(NativeCap), Width));
end;

function TsgGDIPlusAdjustableArrowCap.GetWidth: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapWidth(TsgGDIPAdjustableArrowCap(NativeCap), Result));
end;

function TsgGDIPlusAdjustableArrowCap.SetMiddleInset(MiddleInset: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapMiddleInset(TsgGDIPAdjustableArrowCap(NativeCap), MiddleInset));
end;

function TsgGDIPlusAdjustableArrowCap.GetMiddleInset: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapMiddleInset(TsgGDIPAdjustableArrowCap(NativeCap), Result));
end;

function TsgGDIPlusAdjustableArrowCap.SetFillState(IsFilled: Bool): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapFillState(TsgGDIPAdjustableArrowCap(NativeCap), IsFilled));
end;

function TsgGDIPlusAdjustableArrowCap.IsFilled: BOOL;
begin
  SetStatus(GdipGetAdjustableArrowCapFillState(TsgGDIPAdjustableArrowCap(NativeCap), Result));
end;

constructor TsgGDIPlusMetaFile.Create(hWmf: HMETAFILE;var wmfPlaceableFileHeader: TWmfPlaceableFileHeader; DeleteWmf: BOOL = False);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipCreateMetaFileFromWmf(hWmf, DeleteWmf, @wmfPlaceableFileHeader, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(hEmf: HENHMETAFILE; DeleteEmf: BOOL = False);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipCreateMetaFileFromEmf(hEmf, DeleteEmf, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(FileName: WideString);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipCreateMetaFileFromFile(PWideChar(FileName), MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(FileName: WiDestring; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipCreateMetaFileFromWmfFile(PWideChar(FileName), @wmfPlaceableFileHeader, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(Stream: IStream);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipCreateMetaFileFromStream(Stream, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(ReferenceHdc: HDC; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFile(ReferenceHdc, Type_, nil, MetaFileFrameUnitGdi,
     Description, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(ReferenceHdc: HDC; FrameRect: TsgGRectF;
   FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi;
   Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFile(ReferenceHdc, Type_, @FrameRect, FrameUnit,
    Description, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(ReferenceHdc: HDC; FrameRect: TsgGRect;
  FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi;
  Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFileI(ReferenceHdc, Type_, @FrameRect, FrameUnit, Description, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(FileName: WideString; ReferenceHdc: HDC;
  Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFileFileName(PWideChar(FileName),
    ReferenceHdc, Type_, nil, MetaFileFrameUnitGdi, Description, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(FileName: WideString; ReferenceHdc: HDC; FrameRect: TsgGRectF;
  FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFileFileName(PWideChar(FileName), ReferenceHdc, Type_, @FrameRect, FrameUnit, Description, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(FileName: WideString; ReferenceHdc: HDC; FrameRect: TsgGRect;
  FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFileFileNameI(PWideChar(FileName), ReferenceHdc, Type_, @FrameRect, FrameUnit, Description, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(Stream: IStream; ReferenceHdc: HDC;
  Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFileStream(Stream, ReferenceHdc, Type_, nil,
    MetaFileFrameUnitGdi, Description, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(Stream: IStream; ReferenceHdc: HDC; FrameRect: TsgGRectF;
  FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFileStream(Stream, ReferenceHdc, Type_, @FrameRect, FrameUnit, Description, MetaFile);
  SetNativeImage(MetaFile);
end;

constructor TsgGDIPlusMetaFile.Create(Stream: IStream; ReferenceHdc: HDC; FrameRect: TsgGRect;
  FrameUnit: TMetaFileFrameUnit = MetaFileFrameUnitGdi; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWCHAR = nil);
var
  MetaFile: TsgGDIPMetaFile;
begin
  MetaFile := nil;
  FStatusOfFunction := GdipRecordMetaFileStreamI(Stream, ReferenceHdc, Type_, @FrameRect, FrameUnit, Description, MetaFile);
  SetNativeImage(MetaFile);
end;

function TsgGDIPlusMetaFile.GetMetaFileHeader(hWmf: HMETAFILE;
  var wmfPlaceableFileHeader: TWmfPlaceableFileHeader; header: TMetaFileHeader): TsgGDIPStatus;
begin
  Result := GdipGetMetaFileHeaderFromWmf(hWmf, @wmfPlaceableFileHeader, @header.Type_);
end;

function TsgGDIPlusMetaFile.GetMetaFileHeader(hEmf: HENHMETAFILE; header: TMetaFileHeader): TsgGDIPStatus;
begin
  Result := GdipGetMetaFileHeaderFromEmf(hEmf, @header.Type_);
end;

function TsgGDIPlusMetaFile.GetMetaFileHeader(FileName: WideString; header: TMetaFileHeader): TsgGDIPStatus;
begin
  Result := GdipGetMetaFileHeaderFromFile(PWideChar(FileName), @header.Type_);
end;

function TsgGDIPlusMetaFile.GetMetaFileHeader(Stream: IStream; header: TMetaFileHeader): TsgGDIPStatus;
begin
  Result := GdipGetMetaFileHeaderFromStream(Stream, @header.Type_);
end;

function TsgGDIPlusMetaFile.GetMetaFileHeader(header: TMetaFileHeader): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetMetaFileHeaderFromMetaFile(TsgGDIPMetaFile(NativeImage),@header.Type_));
end;

function TsgGDIPlusMetaFile.GetHENHMETAFILE: HENHMETAFILE;
begin
  SetStatus(GdipGetHemfFromMetaFile(TsgGDIPMetaFile(NativeImage), Result));
end;

function TsgGDIPlusMetaFile.PlayRecOrd(RecordType: TEmfPlusRecordType; Flags, DataSize: UINT; Data: PBYTE): TsgGDIPStatus;
begin
  Result := SetStatus(GdipPlayMetaFileRecOrd(TsgGDIPMetaFile(NativeImage), recordType, Flags, dataSize, data));
end;

function TsgGDIPlusMetaFile.SetDownLevelRasterizationLimit(MetaFileRasterizationLimitDpi: UINT): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetMetaFileDownLevelRasterizationLimit(TsgGDIPMetaFile(NativeImage), MetaFileRasterizationLimitDpi));
end;

function TsgGDIPlusMetaFile.GetDownLevelRasterizationLimit: UINT;
var
  MetaFileRasterizationLimitDpi: UINT;
begin
  MetaFileRasterizationLimitDpi := 0;
  SetStatus(GdipGetMetaFileDownLevelRasterizationLimit(TsgGDIPMetaFile(NativeImage), MetaFileRasterizationLimitDpi));
  Result := MetaFileRasterizationLimitDpi;
end;

function TsgGDIPlusMetaFile.EmfToWmfBits(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
  iMapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UINT;
begin
  Result := GdipEmfToWmfBits(hemf, cbData16, pData16, iMapMode, Integer(eFlags));
end;

constructor TsgGDIPlusMetaFile.Create;
begin
  SetNativeImage(nil);
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusRegion.Create;
var
  Region: TsgGDIPRegion;
begin
  Region := nil;
  FStatusOfFunction := GdipCreateRegion(Region);
  SetNativeRegion(Region);
end;

constructor TsgGDIPlusRegion.Create(Rect: TsgGRectF);
var
  Region: TsgGDIPRegion;
begin
  Region := nil;
  FStatusOfFunction := GdipCreateRegionRect(@Rect, Region);
  SetNativeRegion(Region);
end;

constructor TsgGDIPlusRegion.Create(Rect: TsgGRect);
var
  Region: TsgGDIPRegion;
begin
  Region := nil;
  FStatusOfFunction := GdipCreateRegionRectI(@Rect, Region);
  SetNativeRegion(Region);
end;

constructor TsgGDIPlusRegion.Create(Path: TsgGDIPlusGraphicsPath);
var
  Region: TsgGDIPRegion;
begin
  Region := nil;
  FStatusOfFunction := GdipCreateRegionPath(Path.NativePath, Region);
  SetNativeRegion(Region);
end;

constructor TsgGDIPlusRegion.Create(RegionData: PBYTE; Size: Integer);
var
  Region: TsgGDIPRegion;
begin
  Region := nil;
  FStatusOfFunction := GdipCreateRegionRgnData(RegionData, Size, Region);
  SetNativeRegion(Region);
end;

constructor TsgGDIPlusRegion.Create(hRgn: HRGN);
var
  Region: TsgGDIPRegion;
begin
  Region := nil;
  FStatusOfFunction := GdipCreateRegionHrgn(hRgn, Region);
  SetNativeRegion(Region);
end;

function TsgGDIPlusRegion.FromHRGN(hRgn: HRGN): TsgGDIPlusRegion;
var
  Region: TsgGDIPRegion;
begin
  Region := nil;
  if (GdipCreateRegionHrgn(hRgn, Region) = Ok) then
  begin
      Result := TsgGDIPlusRegion.Create(Region);
      if (Result = nil) then
        GdipDeleteRegion(Region);
      Exit;
  end
  else
    Result := nil;
end;

destructor TsgGDIPlusRegion.Destroy;
begin
  GdipDeleteRegion(NativeRegion);
end;

function TsgGDIPlusRegion.Clone: TsgGDIPlusRegion;
var
  Region: TsgGDIPRegion;
begin
  Region := nil;
  SetStatus(GdipCloneRegion(NativeRegion, Region));
  Result := TsgGDIPlusRegion.Create(Region);
end;

function TsgGDIPlusRegion.MakeInfinite: TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetInfinite(NativeRegion));
end;

function TsgGDIPlusRegion.MakeEmpty: TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetEmpty(NativeRegion));
end;

function TsgGDIPlusRegion.GetDataSize: UINT;
var
  BufferSize: UINT;
begin
  BufferSize := 0;
  SetStatus(GdipGetRegionDataSize(NativeRegion, BufferSize));
  Result := BufferSize;
end;

function TsgGDIPlusRegion.GetData(Buffer: PBYTE; BufferSize: UINT; SizeFilled: PUINT = nil): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetRegionData(NativeRegion, Buffer, BufferSize, SizeFilled));
end;

function TsgGDIPlusRegion.Intersect(const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(NativeRegion, @Rect, CombineModeIntersect));
end;

function TsgGDIPlusRegion.Intersect(const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(NativeRegion, @Rect, CombineModeIntersect));
end;

function TsgGDIPlusRegion.Intersect(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(NativeRegion, path.NativePath,
    CombineModeIntersect));
end;

function TsgGDIPlusRegion.Intersect(Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(NativeRegion, Region.NativeRegion,
    CombineModeIntersect));
end;

function TsgGDIPlusRegion.Union(const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(NativeRegion, @Rect, CombineModeUnion));
end;

function TsgGDIPlusRegion.Union(const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(NativeRegion, @Rect, CombineModeUnion));
end;

function TsgGDIPlusRegion.Union(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(NativeRegion, path.NativePath, CombineModeUnion));
end;

function TsgGDIPlusRegion.Union(Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(NativeRegion, Region.NativeRegion, CombineModeUnion));
end;

function TsgGDIPlusRegion.Xor_(const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(NativeRegion, @Rect, CombineModeXor));
end;

function TsgGDIPlusRegion.Xor_(const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(NativeRegion, @Rect, CombineModeXor));
end;

function TsgGDIPlusRegion.Xor_(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(NativeRegion, path.NativePath, CombineModeXor));
end;

function TsgGDIPlusRegion.Xor_(Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(NativeRegion, Region.NativeRegion, CombineModeXor));
end;

function TsgGDIPlusRegion.Exclude(const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(NativeRegion, @Rect, CombineModeExclude));
end;

function TsgGDIPlusRegion.Exclude(const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(NativeRegion, @Rect, CombineModeExclude));
end;

function TsgGDIPlusRegion.Exclude(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(NativeRegion, path.NativePath, CombineModeExclude));
end;

function TsgGDIPlusRegion.Exclude(Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(NativeRegion, Region.NativeRegion, CombineModeExclude));
end;

function TsgGDIPlusRegion.Complement(const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(NativeRegion, @Rect, CombineModeComplement));
end;

function TsgGDIPlusRegion.Complement(const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(NativeRegion, @Rect, CombineModeComplement));
end;

function TsgGDIPlusRegion.Complement(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(NativeRegion, Path.NativePath, CombineModeComplement));
end;

function TsgGDIPlusRegion.Complement(Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(NativeRegion, Region.NativeRegion, CombineModeComplement));
end;

function TsgGDIPlusRegion.Translate(Dx, Dy: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslateRegion(NativeRegion, Dx, Dy));
end;

function TsgGDIPlusRegion.Translate(Dx, Dy: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslateRegionI(NativeRegion, Dx, Dy));
end;

function TsgGDIPlusRegion.Transform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTransformRegion(NativeRegion, AMatrix.NativeMatrix));
end;

function TsgGDIPlusRegion.GetBounds(out Rect: TsgGRect; Graph: TsgGDIPlusGraphics): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetRegionBoundsI(NativeRegion, Graph.NativeGraphics, @Rect));
end;

function TsgGDIPlusRegion.GetBounds(out Rect: TsgGRectF; Graph: TsgGDIPlusGraphics): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetRegionBounds(NativeRegion, Graph.NativeGraphics, @Rect));
end;

function TsgGDIPlusRegion.GetHRGN(Graph: TsgGDIPlusGraphics): HRGN;
begin
  SetStatus(GdipGetRegionHRgn(NativeRegion, Graph.NativeGraphics, Result));
end;

function TsgGDIPlusRegion.IsEmpty(Graph: TsgGDIPlusGraphics): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := False;
  SetStatus(GdipIsEmptyRegion(NativeRegion, Graph.NativeGraphics, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsInfinite(Graph: TsgGDIPlusGraphics): BOOL ;
var
  vBoolVal: BOOL;
begin
  vBoolVal := False;
  SetStatus(GdipIsInfiniteRegion(NativeRegion, Graph.NativeGraphics, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsVisible(X, Y: Integer; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  vGraph: TsgGDIPGraphics;
begin
  vBoolVal := False;
  if Assigned(Graph) then
    vGraph := Graph.NativeGraphics
  else
    vGraph := nil;
  SetStatus(GdipIsVisibleRegionPointI(NativeRegion, X, Y, vGraph, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsVisible(const Point: TsgGPoint; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  vGraph: TsgGDIPGraphics;
begin
  vBoolVal := False;
  if Assigned(Graph) then
    vGraph := Graph.NativeGraphics
  else
    vGraph := nil;
  SetStatus(GdipIsVisibleRegionPointI(NativeRegion, Point.X, Point.Y, vGraph, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsVisible(X, Y: Single; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  vGraph: TsgGDIPGraphics;
begin
  vBoolVal := False;
  if Assigned(Graph) then
    vGraph := Graph.NativeGraphics
  else
    vGraph := nil;
  SetStatus(GdipIsVisibleRegionPoint(NativeRegion, X, Y, vGraph, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsVisible(const Point: TsgGPointF; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  vGraph: TsgGDIPGraphics;
begin
  vBoolVal := False;
  if Assigned(Graph) then
    vGraph := Graph.NativeGraphics
  else
    vGraph := nil;
  SetStatus(GdipIsVisibleRegionPoint(NativeRegion, Point.X, Point.Y, vGraph, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsVisible(X, Y, Width, Height: Integer; Graph: TsgGDIPlusGraphics): BOOL;
var
  vBoolVal: BOOL;
  vGraph: TsgGDIPGraphics;
begin
  vBoolVal := False;
  if Assigned(Graph) then
    vGraph := Graph.NativeGraphics
  else
    vGraph := nil;
  SetStatus(GdipIsVisibleRegionRectI(NativeRegion, X, Y, Width, Height, vGraph, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsVisible(const Rect: TsgGRect; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  vGraph: TsgGDIPGraphics;
begin
  vBoolVal := False;
  if Assigned(Graph) then
    vGraph := Graph.NativeGraphics
  else
    vGraph := nil;
  SetStatus(GdipIsVisibleRegionRectI(NativeRegion, Rect.X, Rect.Y, Rect.Width, Rect.Height, vGraph, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsVisible(X, Y, Width, Height: Single; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  vGraph: TsgGDIPGraphics;
begin
  vBoolVal := False;
  if Assigned(Graph) then
    vGraph := Graph.NativeGraphics
  else
    vGraph := nil;
  SetStatus(GdipIsVisibleRegionRect(NativeRegion, X, Y, Width, Height, vGraph, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.IsVisible(const Rect: TsgGRectF; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  vGraph: TsgGDIPGraphics;
begin
  vBoolVal := False;
  if Assigned(Graph) then
    vGraph := Graph.NativeGraphics
  else
    vGraph := nil;
  SetStatus(GdipIsVisibleRegionRect(NativeRegion, Rect.X, Rect.Y, Rect.Width, Rect.Height, vGraph, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.EqualsRgn(Region: TsgGDIPlusRegion; Graph: TsgGDIPlusGraphics): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := False;
  SetStatus(GdipIsEqualRegion(NativeRegion, Region.NativeRegion, Graph.NativeGraphics, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusRegion.GetRegionScansCount(const AMatrix: TsgGDIPlusMatrix): UINT;
var
  Count: UINT;
begin
  Count := 0;
  SetStatus(GdipGetRegionScansCount(NativeRegion, Count, AMatrix.NativeMatrix));
  Result := Count;
end;

function TsgGDIPlusRegion.GetRegionScans(const AMatrix: TsgGDIPlusMatrix; Rects: PsgGRectF; out Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetRegionScans(NativeRegion, Rects, Count, AMatrix.NativeMatrix));
end;

function TsgGDIPlusRegion.GetRegionScans(const AMatrix: TsgGDIPlusMatrix; Rects: PsgGRect; out Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetRegionScansI(NativeRegion, Rects, Count, AMatrix.NativeMatrix));
end;

function TsgGDIPlusRegion.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

function TsgGDIPlusRegion.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

constructor TsgGDIPlusRegion.Create(NativeRegion: TsgGDIPRegion; Dummy: Integer);
begin
  SetNativeRegion(NativeRegion);
end;

procedure TsgGDIPlusRegion.SetNativeRegion(NativeRegion: TsgGDIPRegion);
begin
  Self.NativeRegion := NativeRegion;
end;

constructor TsgGDIPlusCustomLineCap.Create(FillPath, StrokePath: TsgGDIPlusGraphicsPath;
  BaseCap: TsgGDIPLineCap = LineCapFlat; BaseInset: Single = 0);
var
  NativeFillPath, NativeStrokePath: TsgGDIPPath;
begin
  NativeCap := nil;
  NativeFillPath := nil;
  NativeStrokePath := nil;
  if Assigned(FillPath) then
    NativeFillPath := FillPath.NativePath;
  if Assigned(StrokePath) then
    NativeStrokePath := StrokePath.NativePath;
  FStatusOfFunction := GdipCreateCustomLineCap(NativeFillPath, NativeStrokePath, BaseCap, BaseInset, NativeCap);
end;

destructor TsgGDIPlusCustomLineCap.Destroy;
begin
  GdipDeleteCustomLineCap(NativeCap);
end;

function TsgGDIPlusCustomLineCap.Clone: TsgGDIPlusCustomLineCap;
var
  NewNativeLineCap: TsgGDIPCustomLineCap;
begin
  NewNativeLineCap := nil;
  SetStatus(GdipCloneCustomLineCap(NativeCap, NewNativeLineCap));
  if (FStatusOfFunction = Ok) then
  begin
    Result := TsgGDIPlusCustomLineCap.Create(NewNativeLineCap, FStatusOfFunction);
    if (Result = nil) then
      SetStatus(GdipDeleteCustomLineCap(NewNativeLineCap));
  end
  else
    Result := nil;
end;

function TsgGDIPlusCustomLineCap.SetStrokeCap(StrokeCap: TsgGDIPLineCap): TsgGDIPStatus;
begin
  Result := SetStrokeCaps(StrokeCap, StrokeCap);
end;

function TsgGDIPlusCustomLineCap.SetStrokeCaps(StartCap, EndCap: TsgGDIPLineCap): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapStrokeCaps(NativeCap, StartCap, EndCap));
end;

function TsgGDIPlusCustomLineCap.GetStrokeCaps(out StartCap, EndCap: TsgGDIPLineCap): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetCustomLineCapStrokeCaps(NativeCap, StartCap, EndCap));
end;

function TsgGDIPlusCustomLineCap.SetStrokeJoin(LineJoin: TsgGDIPLineJoin): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapStrokeJoin(NativeCap, LineJoin));
end;

function TsgGDIPlusCustomLineCap.GetStrokeJoin: TsgGDIPLineJoin;
begin
  SetStatus(GdipGetCustomLineCapStrokeJoin(NativeCap, Result));
end;

function TsgGDIPlusCustomLineCap.SetBaseCap(BaseCap: TsgGDIPLineCap): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapBaseCap(NativeCap, BaseCap));
end;

function TsgGDIPlusCustomLineCap.GetBaseCap: TsgGDIPLineCap;
begin
  SetStatus(GdipGetCustomLineCapBaseCap(NativeCap, Result));
end;

function TsgGDIPlusCustomLineCap.SetBaseInset(Inset: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapBaseInset(NativeCap, Inset));
end;

function TsgGDIPlusCustomLineCap.GetBaseInset: Single;
begin
  SetStatus(GdipGetCustomLineCapBaseInset(NativeCap, Result));
end;

function TsgGDIPlusCustomLineCap.SetWidthScale(WidthScale: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapWidthScale(NativeCap, WidthScale));
end;

function TsgGDIPlusCustomLineCap.GetWidthScale: Single;
begin
  SetStatus(GdipGetCustomLineCapWidthScale(NativeCap, Result));
end;

function TsgGDIPlusCustomLineCap.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusCustomLineCap.Create;
begin
  NativeCap := nil;
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusCustomLineCap.Create(NativeCap: TsgGDIPCustomLineCap; Status: TsgGDIPStatus);
begin
  FStatusOfFunction := Status;
  SetNativeCap(NativeCap);
end;

procedure TsgGDIPlusCustomLineCap.SetNativeCap(NativeCap: TsgGDIPCustomLineCap);
begin
  Self.NativeCap := NativeCap;
end;

function TsgGDIPlusCustomLineCap.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

constructor TsgGDIPlusPen.Create(Color: TsgColor; Width: Single = 1.0);
var
  Unit_: TsgGDIPUnit;
begin
  Unit_ := UnitWorld;
  NativePen := nil;
  FStatusOfFunction := GdipCreatePen1(Color, Width, Unit_, NativePen);
end;

constructor TsgGDIPlusPen.Create(Brush: TsgGDIPlusBrush; Width: Single = 1.0);
var
  Unit_: TsgGDIPUnit;
begin
  Unit_ := UnitWorld;
  NativePen := nil;
  FStatusOfFunction := GdipCreatePen2(Brush.NativeBrush, Width, Unit_, NativePen);
end;

destructor TsgGDIPlusPen.Destroy;
begin
  GdipDeletePen(NativePen);
end;

function TsgGDIPlusPen.Clone: TsgGDIPlusPen;
var
  ClonePen: TsgGDIPPen;
begin
  ClonePen := nil;
  FStatusOfFunction := GdipClonePen(NativePen, ClonePen);
  Result := TsgGDIPlusPen.Create(ClonePen, FStatusOfFunction);
end;

function TsgGDIPlusPen.SetWidth(Width: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenWidth(NativePen, Width));
end;

function TsgGDIPlusPen.GetWidth: Single;
begin
  SetStatus(GdipGetPenWidth(NativePen, Result));
end;
    
function TsgGDIPlusPen.SetLineCap(StartCap, EndCap: TsgGDIPLineCap; DashCap: TsgGDIPDashCap): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenLineCap197819(NativePen, StartCap, EndCap, DashCap));
end;

function TsgGDIPlusPen.SetStartCap(StartCap: TsgGDIPLineCap): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenStartCap(NativePen, StartCap));
end;

function TsgGDIPlusPen.SetEndCap(EndCap: TsgGDIPLineCap): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenEndCap(NativePen, EndCap));
end;

function TsgGDIPlusPen.SetDashCap(DashCap: TsgGDIPDashCap): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenDashCap197819(NativePen, DashCap));
end;

function TsgGDIPlusPen.GetStartCap: TsgGDIPLineCap;
begin
  SetStatus(GdipGetPenStartCap(NativePen, Result));
end;

function TsgGDIPlusPen.GetEndCap: TsgGDIPLineCap;
begin
  SetStatus(GdipGetPenEndCap(NativePen, Result));
end;

function TsgGDIPlusPen.GetDashCap: TsgGDIPDashCap;
begin
  SetStatus(GdipGetPenDashCap197819(NativePen, Result));
end;

function TsgGDIPlusPen.SetLineJoin(LineJoin: TsgGDIPLineJoin): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenLineJoin(NativePen, LineJoin));
end;

function TsgGDIPlusPen.GetLineJoin: TsgGDIPLineJoin;
begin
  SetStatus(GdipGetPenLineJoin(NativePen, Result));
end;

function TsgGDIPlusPen.SetCustomStartCap(CustomCap: TsgGDIPlusCustomLineCap): TsgGDIPStatus;
var
  NativeCap: TsgGDIPCustomLineCap;
begin
  NativeCap := nil;
  if Assigned(CustomCap) then
    NativeCap := CustomCap.NativeCap;
  Result := SetStatus(GdipSetPenCustomStartCap(NativePen, NativeCap));
end;

function TsgGDIPlusPen.GetCustomStartCap(CustomCap: TsgGDIPlusCustomLineCap): TsgGDIPStatus;
begin
  if(CustomCap = nil) then
    Result := SetStatus(InValidParameter)
  else
    Result := SetStatus(GdipGetPenCustomStartCap(NativePen, CustomCap.NativeCap));
end;

function TsgGDIPlusPen.SetCustomEndCap(CustomCap: TsgGDIPlusCustomLineCap): TsgGDIPStatus;
var
  NativeCap: TsgGDIPCustomLineCap;
begin
  NativeCap := nil;
  if Assigned(CustomCap) then NativeCap := CustomCap.NativeCap;
  Result := SetStatus(GdipSetPenCustomEndCap(NativePen, NativeCap));
end;

function TsgGDIPlusPen.GetCustomEndCap(CustomCap: TsgGDIPlusCustomLineCap): TsgGDIPStatus;
begin
  if(CustomCap = nil) then
    Result := SetStatus(InValidParameter)
  else
    Result := SetStatus(GdipGetPenCustomEndCap(NativePen, CustomCap.NativeCap));
end;

function TsgGDIPlusPen.SetMiterLimit(MiterLimit: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenMiterLimit(NativePen, MiterLimit));
end;

function TsgGDIPlusPen.GetMiterLimit: Single;
begin
  SetStatus(GdipGetPenMiterLimit(NativePen, Result));
end;

function TsgGDIPlusPen.SetAlignment(PenAlignment: TsgGDIPPenAlignment): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenMode(NativePen, PenAlignment));
end;

function TsgGDIPlusPen.GetAlignment: TsgGDIPPenAlignment;
begin
  SetStatus(GdipGetPenMode(NativePen, Result));
end;

function TsgGDIPlusPen.SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenTransform(NativePen, AMatrix.NativeMatrix));
end;

function TsgGDIPlusPen.GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPenTransform(NativePen, AMatrix.NativeMatrix));
end;

function TsgGDIPlusPen.ResetTransform: TsgGDIPStatus;
begin
  Result := SetStatus(GdipResetPenTransform(NativePen));
end;

function TsgGDIPlusPen.MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipMultiplyPenTransform(NativePen, AMatrix.NativeMatrix, Order));
end;

function TsgGDIPlusPen.TranslateTransform(Dx, Dy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslatePenTransform(NativePen, Dx, Dy, Order));
end;

function TsgGDIPlusPen.ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipScalePenTransform(NativePen, Sx, Sy, Order));
end;

function TsgGDIPlusPen.RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipRotatePenTransform(NativePen, Angle, Order));
end;

function TsgGDIPlusPen.GetPenType: TsgGDIPPenType;
begin
  SetStatus(GdipGetPenFillType(NativePen, Result));
end;

function TsgGDIPlusPen.SetColor(Color: TsgColor): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenColor(NativePen, Color));
end;

function TsgGDIPlusPen.SetBrush(Brush: TsgGDIPlusBrush): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenBrushFill(NativePen, Brush.NativeBrush));
end;

function TsgGDIPlusPen.GetColor(out Color: TsgColor): TsgGDIPStatus;
var
  Type_: TsgGDIPPenType;
  ARGB: DWORD;
begin
  Type_ := GetPenType;
  if (Type_ <> PenTypeSolidColor) then
  begin
    Result := WrongState;
    Exit;
  end;
  SetStatus(GdipGetPenColor(NativePen, ARGB));
  if (FStatusOfFunction = Ok) then
    Color := ARGB;
  Result := FStatusOfFunction;
end;

function TsgGDIPlusPen.GetBrush: TsgGDIPlusBrush;
var
  Type_: TsgGDIPPenType;
  Brush: TsgGDIPlusBrush;
  NativeBrush: TsgGDIPBrush;
begin
  Type_ := GetPenType;
  Brush := nil;
  case Type_ of
     PenTypeSolidColor    : Brush := TsgGDIPlusSolidBrush.Create;
     PenTypeHatchFill     : Brush := TsgGDIPlusHatchBrush.Create;
     PenTypeTextureFill   : Brush := TsgGDIPlusTextureBrush.Create;
     PenTypePathGradient  : Brush := TsgGDIPlusBrush.Create;
     PenTypeLinearGradient: Brush := TsgGDIPlusLinearGradientBrush.Create;
   end;
   if (Brush <> nil) then
   begin
     SetStatus(GdipGetPenBrushFill(NativePen, NativeBrush));
     Brush.SetNativeBrush(NativeBrush);
   end;
   Result := Brush;
end;

function TsgGDIPlusPen.GetDashStyle: TsgGDIPDashStyle;
begin
  SetStatus(GdipGetPenDashStyle(NativePen, Result));
end;

function TsgGDIPlusPen.SetDashStyle(DashStyle: TsgGDIPDashStyle): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenDashStyle(NativePen, DashStyle));
end;

function TsgGDIPlusPen.GetDashOffset: Single;
begin
  SetStatus(GdipGetPenDashOffset(NativePen, Result));
end;

function TsgGDIPlusPen.SetDashOffset(DashOffset: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenDashOffset(NativePen, DashOffset));
end;
    
function TsgGDIPlusPen.SetDashPattern(DashArray: PSingle; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenDashArray(NativePen, DashArray, Count));
end;

function TsgGDIPlusPen.GetDashPatternCount: Integer;
begin
  SetStatus(GdipGetPenDashCount(NativePen, Result));
end;

function TsgGDIPlusPen.GetDashPattern(DashArray: PSingle; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPenDashArray(NativePen, DashArray, Count));
end;

function TsgGDIPlusPen.SetCompoundArray(compoundArray: PSingle; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPenCompoundArray(NativePen, compoundArray, Count));
end;

function TsgGDIPlusPen.GetCompoundArrayCount: Integer;
begin
  SetStatus(GdipGetPenCompoundCount(NativePen, Result));
end;

function TsgGDIPlusPen.GetCompoundArray(compoundArray: PSingle; Count: Integer): TsgGDIPStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InValidParameter)
  else
    Result := SetStatus(GdipGetPenCompoundArray(NativePen, compoundArray, Count));
end;

function TsgGDIPlusPen.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusPen.Create(NativePen: TsgGDIPPen; Status: TsgGDIPStatus);
begin
  FStatusOfFunction := Status;
  SetNativePen(NativePen);
end;

procedure TsgGDIPlusPen.SetNativePen(NativePen: TsgGDIPPen);
begin
  Self.NativePen := NativePen;
end;

function TsgGDIPlusPen.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

destructor TsgGDIPlusBrush.Destroy;
begin
  GdipDeleteBrush(NativeBrush);
end;

function TsgGDIPlusBrush.Clone: TsgGDIPlusBrush;
var
  Brush: TsgGDIPBrush;
  newBrush: TsgGDIPlusBrush;
begin
  Brush := nil;
  SetStatus(GdipCloneBrush(NativeBrush, Brush));
  newBrush := TsgGDIPlusBrush.Create(Brush, FStatusOfFunction);
  if (newBrush = nil) then
    GdipDeleteBrush(Brush);
  Result := newBrush;
end;

function TsgGDIPlusBrush.GetType: TsgGDIPBrushType;
var
  Type_: TsgGDIPBrushType;
begin
  Type_ := TsgGDIPBrushType(-1);
  SetStatus(GdipGetBrushType(NativeBrush, Type_));
  Result := Type_;
end;

function TsgGDIPlusBrush.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusBrush.Create;
begin
  SetStatus(NotImplemented);
end;

constructor TsgGDIPlusBrush.Create(NativeBrush: TsgGDIPBrush; Status: TsgGDIPStatus);
begin
  FStatusOfFunction := Status;
  SetNativeBrush(NativeBrush);
end;

procedure TsgGDIPlusBrush.SetNativeBrush(NativeBrush: TsgGDIPBrush);
begin
  Self.NativeBrush := NativeBrush;
end;

function TsgGDIPlusBrush.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

constructor TsgGDIPlusSolidBrush.Create(Color: TsgColor);
var
  Brush: TsgGDIPSolidFill;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreateSolidFill(Color, Brush);
  SetNativeBrush(Brush);
end;

function TsgGDIPlusSolidBrush.GetColor(out Color: TsgColor): TsgGDIPStatus;
begin
  SetStatus(GdipGetSolidFillColor(TsgGDIPSolidFill(NativeBrush), Color));
  Result := FStatusOfFunction;
end;

function TsgGDIPlusSolidBrush.SetColor(Color: TsgColor): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetSolidFillColor(TsgGDIPSolidFill(NativeBrush), Color));
end;

constructor TsgGDIPlusSolidBrush.Create;
begin
end;

constructor TsgGDIPlusTextureBrush.Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode = WrapModeTile);
var
  Texture: TsgGDIPTexture;
begin
  FStatusOfFunction := GdipCreateTexture(Image.NativeImage, WrapMode, Texture);
  SetNativeBrush(Texture);
end;

constructor TsgGDIPlusTextureBrush.Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode; DstRect: TsgGRectF);
var
  Texture: TsgGDIPTexture;
begin
  Texture := nil;
  FStatusOfFunction := GdipCreateTexture2(Image.NativeImage, WrapMode, DstRect.X, DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TsgGDIPlusTextureBrush.Create(Image: TsgGDIPlusImage; DstRect: TsgGRectF; ImageAttributes: TsgGDIPlusImageAttributes = nil);
var
  Texture: TsgGDIPTexture;
  ImgAtt: TsgGDIPImageAttributes;
begin
  Texture := nil;
  if Assigned(ImageAttributes) then
    ImgAtt := ImageAttributes.NativeImageAttr
  else ImgAtt := nil;
  FStatusOfFunction := GdipCreateTextureIA(Image.NativeImage, ImgAtt, DstRect.X, DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TsgGDIPlusTextureBrush.Create(Image: TsgGDIPlusImage; DstRect: TsgGRect; ImageAttributes: TsgGDIPlusImageAttributes = nil);
var
  Texture: TsgGDIPTexture;
  ImgAtt: TsgGDIPImageAttributes;
begin
  Texture := nil;
  if Assigned(ImageAttributes) then
    ImgAtt := ImageAttributes.NativeImageAttr
  else ImgAtt := nil;
  FStatusOfFunction := GdipCreateTextureIAI(Image.NativeImage, ImgAtt, DstRect.X, DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TsgGDIPlusTextureBrush.Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode; DstRect: TsgGRect);
var
  Texture: TsgGDIPTexture;
begin
  Texture := nil;
  FStatusOfFunction := GdipCreateTexture2I(Image.NativeImage, WrapMode, DstRect.X, DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TsgGDIPlusTextureBrush.Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode; DstX, DstY, DstWidth, DstHeight: Single);
var
  Texture: TsgGDIPTexture;
begin
  Texture := nil;
  FStatusOfFunction := GdipCreateTexture2(Image.NativeImage, WrapMode, DstX, DstY, DstWidth, DstHeight, Texture);
  SetNativeBrush(Texture);
end;

constructor TsgGDIPlusTextureBrush.Create(Image: TsgGDIPlusImage; WrapMode: TsgGDIPWrapMode; DstX, DstY, DstWidth, DstHeight: Integer);
var
  Texture: TsgGDIPTexture;
begin
  Texture := nil;
  FStatusOfFunction := GdipCreateTexture2I(Image.NativeImage, WrapMode, DstX, DstY, DstWidth, DstHeight, Texture);
  SetNativeBrush(Texture);
end;

function TsgGDIPlusTextureBrush.SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetTextureTransform(TsgGDIPTexture(NativeBrush), AMatrix.NativeMatrix));
end;

function TsgGDIPlusTextureBrush.GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetTextureTransform(TsgGDIPTexture(NativeBrush), AMatrix.NativeMatrix));
end;

function TsgGDIPlusTextureBrush.ResetTransform: TsgGDIPStatus;
begin
  Result := SetStatus(GdipResetTextureTransform(TsgGDIPTexture(NativeBrush)));
end;

function TsgGDIPlusTextureBrush.MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipMultiplyTextureTransform(TsgGDIPTexture(NativeBrush), AMatrix.NativeMatrix, Order));
end;

function TsgGDIPlusTextureBrush.TranslateTransform(Dx, Dy: Single; Order: MatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslateTextureTransform(TsgGDIPTexture(NativeBrush), Dx, Dy, Order));
end;

function TsgGDIPlusTextureBrush.ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipScaleTextureTransform(TsgGDIPTexture(NativeBrush), Sx, Sy, Order));
end;

function TsgGDIPlusTextureBrush.RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipRotateTextureTransform(TsgGDIPTexture(NativeBrush), Angle, Order));
end;

function TsgGDIPlusTextureBrush.SetWrapMode(WrapMode: TsgGDIPWrapMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetTextureWrapMode(TsgGDIPTexture(NativeBrush), WrapMode));
end;

function TsgGDIPlusTextureBrush.GetWrapMode: TsgGDIPWrapMode;
begin
  SetStatus(GdipGetTextureWrapMode(TsgGDIPTexture(NativeBrush), Result));
end;

function TsgGDIPlusTextureBrush.GetImage: TsgGDIPlusImage;
var
  Image: TsgGDIPImage;
begin
  SetStatus(GdipGetTextureImage(TsgGDIPTexture(NativeBrush), Image));
  Result := TsgGDIPlusImage.Create(Image, FStatusOfFunction);
  if (Result = nil) then
    GdipDisposeImage(Image);
end;

constructor TsgGDIPlusTextureBrush.Create;
begin
end;

constructor TsgGDIPlusLinearGradientBrush.Create(const Point1, Point2: TsgGPointF; Color1, Color2: TsgColor);
var
  Brush: TsgGDIPLineGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreateLineBrush(@Point1, @Point2, Color1, Color2, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TsgGDIPlusLinearGradientBrush.Create(const Point1, Point2: TsgGPoint; Color1, Color2: TsgColor);
var
  Brush: TsgGDIPLineGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreateLineBrushI(@Point1, @Point2, Color1, Color2, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TsgGDIPlusLinearGradientBrush.Create(Rect: TsgGRectF; Color1, Color2: TsgColor; Mode: TLinearGradientMode);
var
  Brush: TsgGDIPLineGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreateLineBrushFromRect(@Rect, Color1, Color2, Mode, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TsgGDIPlusLinearGradientBrush.Create(Rect: TsgGRect; Color1, Color2: TsgColor; Mode: TLinearGradientMode);
var
  Brush: TsgGDIPLineGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreateLineBrushFromRectI(@Rect, Color1, Color2, Mode, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TsgGDIPlusLinearGradientBrush.Create(Rect: TsgGRectF; Color1, Color2: TsgColor; Angle: Single; isAngleScalable: BOOL = False);
var
  Brush: TsgGDIPLineGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreateLineBrushFromRectWithAngle(@Rect, Color1, Color2, Angle, isAngleScalable, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TsgGDIPlusLinearGradientBrush.Create(Rect: TsgGRect; Color1, Color2: TsgColor; Angle: Single; isAngleScalable: BOOL = False);
var
  Brush: TsgGDIPLineGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreateLineBrushFromRectWithAngleI(@Rect, Color1, Color2, Angle, isAngleScalable, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

function TsgGDIPlusLinearGradientBrush.SetLinearColors(Color1, Color2: TsgColor): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetLineColors(TsgGDIPLineGradient(NativeBrush), Color1, Color2));
end;

function TsgGDIPlusLinearGradientBrush.GetLinearColors(out Color1, Color2: TsgColor): TsgGDIPStatus;
var
  Colors: array[0..1] of TsgColor;
begin
  SetStatus(GdipGetLineColors(TsgGDIPLineGradient(NativeBrush), @Colors));
  if (FStatusOfFunction = Ok) then
  begin
    Color1 := Colors[0];
    Color2 := Colors[1];
  end;
  Result := FStatusOfFunction;
end;

function TsgGDIPlusLinearGradientBrush.GetRectangle(out Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetLineRect(TsgGDIPLineGradient(NativeBrush), @Rect));
end;

function TsgGDIPlusLinearGradientBrush.GetRectangle(out Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetLineRectI(TsgGDIPLineGradient(NativeBrush), @Rect));
end;

function TsgGDIPlusLinearGradientBrush.SetGammaCorRection(UseGammaCorRection: BOOL): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetLineGammaCorRection(TsgGDIPLineGradient(NativeBrush), UseGammaCorRection));
end;

function TsgGDIPlusLinearGradientBrush.GetGammaCorRection: BOOL;
var
  UseGammaCorRection: BOOL;
begin
  SetStatus(GdipGetLineGammaCorRection(TsgGDIPLineGradient(NativeBrush), UseGammaCorRection));
  Result := UseGammaCorRection;
end;

function TsgGDIPlusLinearGradientBrush.GetBlendCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetLineBlendCount(TsgGDIPLineGradient(NativeBrush), Count));
  Result := Count;
end;

function TsgGDIPlusLinearGradientBrush.SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetLineBlend(TsgGDIPLineGradient(NativeBrush), BlendFactors, BlendPositions, Count));
end;

function TsgGDIPlusLinearGradientBrush.GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
begin
  if ((Count <= 0) or (BlendFactors = nil) or (BlendPositions = nil)) then
    Result := SetStatus(InValidParameter)
  else
    Result := SetStatus(GdipGetLineBlend(TsgGDIPLineGradient(NativeBrush), BlendFactors, BlendPositions, Count));
end;

function TsgGDIPlusLinearGradientBrush.GetInterpolationColorCount: Integer;
var
  vStatus: TsgGDIPStatus;
begin
  vStatus := SetStatus(GdipGetLinePresetBlendCount(TsgGDIPLineGradient(NativeBrush), Result));
  if vStatus <> Ok then
    Result := 0;
end;

function TsgGDIPlusLinearGradientBrush.SetInterpolationColors(APresetColors: PsgColor;
  BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
begin
  if (Count <= 0)  then
    Result := SetStatus(InValidParameter)
  else
    Result := SetStatus(GdipSetLinePresetBlend(TsgGDIPLineGradient(NativeBrush), PARGB(APresetColors), BlendPositions, Count));
end;

function TsgGDIPlusLinearGradientBrush.GetInterpolationColors(APresetColors: PsgColor; BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InValidParameter)
  else
    Result := SetStatus(GdipGetLinePresetBlend(TsgGDIPLineGradient(NativeBrush), PARGB(APresetColors), BlendPositions, Count));
end;

function TsgGDIPlusLinearGradientBrush.SetBlendBellShape(Focus: Single; Scale: Single = 1.0): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetLineSigmaBlend(TsgGDIPLineGradient(NativeBrush), Focus, Scale));
end;

function TsgGDIPlusLinearGradientBrush.SetBlendTriangularShape(Focus: Single; Scale: Single = 1.0): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetLineLinearBlend(TsgGDIPLineGradient(NativeBrush), Focus, Scale));
end;

function TsgGDIPlusLinearGradientBrush.SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetLineTransform(TsgGDIPLineGradient(NativeBrush), AMatrix.NativeMatrix));
end;

function TsgGDIPlusLinearGradientBrush.GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetLineTransform(TsgGDIPLineGradient(NativeBrush), AMatrix.NativeMatrix));
end;

function TsgGDIPlusLinearGradientBrush.ResetTransform: TsgGDIPStatus;
begin
  Result := SetStatus(GdipResetLineTransform(TsgGDIPLineGradient(NativeBrush)));
end;

function TsgGDIPlusLinearGradientBrush.MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipMultiplyLineTransform(TsgGDIPLineGradient(NativeBrush),  AMatrix.NativeMatrix, Order));
end;

function TsgGDIPlusLinearGradientBrush.TranslateTransform(Dx, Dy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslateLineTransform(TsgGDIPLineGradient(NativeBrush), Dx, Dy, Order));
end;

function TsgGDIPlusLinearGradientBrush.ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipScaleLineTransform(TsgGDIPLineGradient(NativeBrush), Sx, Sy, Order));
end;

function TsgGDIPlusLinearGradientBrush.RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipRotateLineTransform(TsgGDIPLineGradient(NativeBrush), Angle, Order));
end;

function TsgGDIPlusLinearGradientBrush.SetWrapMode(WrapMode: TsgGDIPWrapMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetLineWrapMode(TsgGDIPLineGradient(NativeBrush), WrapMode));
end;

function TsgGDIPlusLinearGradientBrush.GetWrapMode: TsgGDIPWrapMode;
begin
   SetStatus(GdipGetLineWrapMode(TsgGDIPLineGradient(NativeBrush), Result));
end;

constructor TsgGDIPlusLinearGradientBrush.Create;
begin
end;

constructor TsgGDIPlusHatchBrush.Create(HatchStyle: TsgGDIPHatchStyle; foreColor: TsgColor; backColor: TsgColor = aclBlack);
var
  Brush: TsgGDIPHatch;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreateHatchBrush(Integer(HatchStyle), foreColor, backColor, Brush);
  SetNativeBrush(Brush);
end;

function TsgGDIPlusHatchBrush.GetHatchStyle: TsgGDIPHatchStyle;
begin
  SetStatus(GdipGetHatchStyle(TsgGDIPHatch(NativeBrush), Result));
end;

function TsgGDIPlusHatchBrush.GetForegroundColor(out Color: TsgColor): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetHatchForegroundColor(TsgGDIPHatch(NativeBrush), Color));
end;

function TsgGDIPlusHatchBrush.GetBackgroundColor(out Color: TsgColor): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetHatchBackgroundColor(TsgGDIPHatch(NativeBrush), Color));
end;

constructor TsgGDIPlusHatchBrush.Create;
begin
end;

constructor TsgGDIPlusImage.Create(FileName: WideString;
                UseEmbeddedColorManagement: BOOL = False);
begin
  NativeImage := nil;
  if UseEmbeddedColorManagement then
    FStatusOfFunction := GdipLoadImageFromFileICM(PWideChar(FileName), NativeImage)
  else
    FStatusOfFunction := GdipLoadImageFromFile(PWideChar(FileName), NativeImage);
end;

constructor TsgGDIPlusImage.Create(Stream: IStream; UseEmbeddedColorManagement: BOOL  = False);
begin
  NativeImage := nil;
  if UseEmbeddedColorManagement then
    FStatusOfFunction := GdipLoadImageFromStreamICM(Stream, NativeImage)
  else
    FStatusOfFunction := GdipLoadImageFromStream(Stream, NativeImage);
end;

function TsgGDIPlusImage.FromFile(FileName: WideString; UseEmbeddedColorManagement: BOOL = False): TsgGDIPlusImage;
begin
  Result := TsgGDIPlusImage.Create(PWideChar(FileName), UseEmbeddedColorManagement);
end;

function TsgGDIPlusImage.FromStream(Stream: IStream; UseEmbeddedColorManagement: BOOL = False): TsgGDIPlusImage;
begin
  Result := TsgGDIPlusImage.Create(Stream, UseEmbeddedColorManagement);
end;

destructor TsgGDIPlusImage.Destroy;
begin
  GdipDisposeImage(NativeImage);
end;

function TsgGDIPlusImage.Clone: TsgGDIPlusImage;
var
  CloneImage: TsgGDIPImage;
begin
  CloneImage := nil;
  SetStatus(GdipCloneImage(NativeImage, CloneImage));
  Result := TsgGDIPlusImage.Create(CloneImage, FStatusOfFunction);
end;

function TsgGDIPlusImage.Save(FileName: WideString; const ClsIdEncoder: TGUID; EncoderParams: PEncoderParameters = nil): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSaveImageToFile(NativeImage, PWideChar(FileName), @ClsIdEncoder, EncoderParams));
end;

function TsgGDIPlusImage.Save(Stream: IStream; const ClsIdEncoder: TGUID; EncoderParams: PEncoderParameters  = nil): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSaveImageToStream(NativeImage, Stream, @ClsIdEncoder, EncoderParams));
end;

function TsgGDIPlusImage.SaveAdd(EncoderParams: PEncoderParameters): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSaveAdd(NativeImage, EncoderParams));
end;

function TsgGDIPlusImage.SaveAdd(NewImage: TsgGDIPlusImage; EncoderParams: PEncoderParameters): TsgGDIPStatus;
begin
  if (NewImage = nil) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  Result := SetStatus(GdipSaveAddImage(NativeImage, NewImage.NativeImage, EncoderParams));
end;

function TsgGDIPlusImage.GetType: TImageType;
begin
  SetStatus(GdipGetImageType(NativeImage, Result));
end;

function TsgGDIPlusImage.GetPhysicalDimension(out Size: TsgGSizeF): TsgGDIPStatus;
var
  Width, Height: Single;
  Status: TsgGDIPStatus;
begin
  Status := SetStatus(GdipGetImageDimension(NativeImage, Width, Height));
  Size.Width  := Width;
  Size.Height := Height;
  Result := Status;
end;

function TsgGDIPlusImage.GetBounds(out SrcRect: TsgGRectF; out SrcUnit: TsgGDIPUnit): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetImageBounds(NativeImage, @SrcRect, SrcUnit));
end;

function TsgGDIPlusImage.GetWidth: UINT;
var
  Width: UINT;
begin
  Width := 0;
  SetStatus(GdipGetImageWidth(NativeImage, Width));
  Result := Width;
end;

function TsgGDIPlusImage.GetHeight: UINT;
var
  Height: UINT;
begin
  Height := 0;
  SetStatus(GdipGetImageHeight(NativeImage, Height));
  Result := Height;
end;

function TsgGDIPlusImage.GetHorizontalResolution: Single;
var
  Resolution: Single;
begin
  Resolution := 0.0;
  SetStatus(GdipGetImageHorizontalResolution(NativeImage, Resolution));
  Result := Resolution;
end;

function TsgGDIPlusImage.GetVerticalResolution: Single;
var
  Resolution: Single;
begin
  Resolution := 0.0;
  SetStatus(GdipGetImageVerticalResolution(NativeImage, Resolution));
  Result := Resolution;
end;

function TsgGDIPlusImage.GetFlags: UINT;
var
  Flags: UINT;
begin
  Flags := 0;
  SetStatus(GdipGetImageFlags(NativeImage, Flags));
  Result := Flags;
end;

function TsgGDIPlusImage.GetRawFormat(out Format: TGUID): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetImageRawFormat(NativeImage, @Format));
end;

function TsgGDIPlusImage.GetPixelFormat: TPixelFormat;
begin
  SetStatus(GdipGetImagePixelFormat(NativeImage, Result));
end;

function TsgGDIPlusImage.GetPaletteSize: Integer;
var
  Size: Integer;
begin
  Size := 0;
  SetStatus(GdipGetImagePaletteSize(NativeImage, Size));
  Result := Size;
end;

function TsgGDIPlusImage.GetPalette(Palette: PColorPalette; Size: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetImagePalette(NativeImage, Palette, Size));
end;

function TsgGDIPlusImage.SetPalette(Palette: PColorPalette): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetImagePalette(NativeImage, Palette));
end;

function TsgGDIPlusImage.GetThumbnailImage(ThumbWidth, ThumbHeight: UINT): TsgGDIPlusImage;
var
  ThumbImage: TsgGDIPImage;
  NewImage: TsgGDIPlusImage;
begin
  ThumbImage := nil;
  SetStatus(GdipGetImageThumbnail(NativeImage, ThumbWidth, ThumbHeight, ThumbImage, FCallBack, FCallBackData));
  NewImage := TsgGDIPlusImage.Create(ThumbImage, FStatusOfFunction);
  if (NewImage = nil) then
    GdipDisposeImage(ThumbImage);
  Result := NewImage;
end;

function TsgGDIPlusImage.GetFrameDimensionsCount: UINT;
var
  Count: UINT;
begin
  Count := 0;
  SetStatus(GdipImageGetFrameDimensionsCount(NativeImage, Count));
  Result := Count;
end;

function TsgGDIPlusImage.GetFrameDimensionsList(DimensionIDs: PGUID; Count: UINT): TsgGDIPStatus;
begin
  Result := SetStatus(GdipImageGetFrameDimensionsList(NativeImage, DimensionIDs, Count));
end;

function TsgGDIPlusImage.GetFrameCount(const DimensionID: TGUID): UINT;
var
  Count: UINT;
begin
  Count := 0;
  SetStatus(GdipImageGetFrameCount(NativeImage, @DimensionID, Count));
  Result := Count;
end;

function TsgGDIPlusImage.SelectActiveFrame(const DimensionID: TGUID; FrameIndex: UINT): TsgGDIPStatus;
begin
  Result := SetStatus(GdipImageSelectActiveFrame(NativeImage, @DimensionID, FrameIndex));
end;

function TsgGDIPlusImage.RotateFlip(RotateFlipType: TRotateFlipType): TsgGDIPStatus;
begin
  Result := SetStatus(GdipImageRotateFlip(NativeImage, RotateFlipType));
end;

function TsgGDIPlusImage.GetPropertyCount: UINT;
var
  NumProperty: UINT;
begin
  NumProperty := 0;
  SetStatus(GdipGetPropertyCount(NativeImage, NumProperty));
  Result := NumProperty;
end;

function TsgGDIPlusImage.GetPropertyIdList(NumOfProperty: UINT; List: PPropID): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPropertyIdList(NativeImage, NumOfProperty, List));
end;

function TsgGDIPlusImage.GetPropertyItASize(PropId: PROPID): UINT;
var
  Size: UINT;
begin
  Size := 0;
  SetStatus(GdipGetPropertyItemSize(NativeImage, PropId, Size));
  Result := Size;
end;

function TsgGDIPlusImage.GetPropertyItem(PropId: PROPID; PropSize: UINT; Buffer: PPropertyItem): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPropertyItem(NativeImage, PropId, PropSize, Buffer));
end;

function TsgGDIPlusImage.GetPropertySize(out TotalBufferSize, NumProperties: UINT): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPropertySize(NativeImage, TotalBufferSize, NumProperties));
end;

function TsgGDIPlusImage.GetAllPropertyItems(TotalBufferSize, NumProperties: UINT; AllItems: PPROPERTYITEM): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetAllPropertyItems(NativeImage, TotalBufferSize, NumProperties, AllItems));
end;

function TsgGDIPlusImage.RemovePropertyItem(PropId: TPROPID): TsgGDIPStatus;
begin
  Result := SetStatus(GdipRemovePropertyItem(NativeImage, PropId));
end;

function TsgGDIPlusImage.SetPropertyItem(const Item: TPropertyItem): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPropertyItem(NativeImage, @Item));
end;

function TsgGDIPlusImage.GetEncoderParameterListSize(const ClsIdEncoder: TGUID): UINT;
var
  Size: UINT;
begin
  Size := 0;
  SetStatus(GdipGetEncoderParameterListSize(NativeImage, @ClsIdEncoder, Size));
  Result := Size;
end;

function TsgGDIPlusImage.GetEncoderParameterList(const ClsIdEncoder: TGUID; Size: UINT; Buffer: PEncoderParameters): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetEncoderParameterList(NativeImage, @ClsIdEncoder, Size, Buffer));
end;

function TsgGDIPlusImage.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusImage.Create(NativeImage: TsgGDIPImage; Status: TsgGDIPStatus);
begin
  SetNativeImage(NativeImage);
  FStatusOfFunction := Status;
end;

procedure TsgGDIPlusImage.SetNativeImage(NativeImage: TsgGDIPImage);
begin
  Self.NativeImage := NativeImage;
end;

function TsgGDIPlusImage.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then FStatusOfFunction := Status;
  Result := Status;
end;

function TsgGDIPlusGraphics.FromHDC(ADC: HDC): TsgGDIPlusGraphics;
begin
  Result := TsgGDIPlusGraphics.Create(ADC);
end;

function TsgGDIPlusGraphics.FromHDC(ADC: HDC; hdevice: THANDLE): TsgGDIPlusGraphics;
begin
  Result := TsgGDIPlusGraphics.Create(ADC, hdevice);
end;

function TsgGDIPlusGraphics.FromHWND(hwnd: HWND; icm: BOOL = False): TsgGDIPlusGraphics;
begin
  Result := TsgGDIPlusGraphics.Create(hwnd, icm);
end;

function TsgGDIPlusGraphics.FromImage(Image: TsgGDIPlusImage): TsgGDIPlusGraphics;
begin
  Result := TsgGDIPlusGraphics.Create(Image);
end;

constructor TsgGDIPlusGraphics.Create(ADC: HDC);
var
  Graphics: TsgGDIPGraphics;
begin
  Graphics := nil;
  FStatusOfFunction := GdipCreateFromHDC(ADC, Graphics);
  SetNativeGraphics(Graphics);
  if FStatusOfFunction = Ok then
    FStatusOfFunction := SetPageUnit(UnitPixel);
end;

constructor TsgGDIPlusGraphics.Create(ADC: HDC; hdevice: THANDLE);
var
  Graphics: TsgGDIPGraphics;
begin
  Graphics := nil;
  FStatusOfFunction := GdipCreateFromHDC2(ADC, hdevice, Graphics);
  SetNativeGraphics(Graphics);
  if FStatusOfFunction = Ok then
    FStatusOfFunction := SetPageUnit(UnitPixel);
end;

constructor TsgGDIPlusGraphics.Create(hwnd: HWND; Icm: BOOL{ = False};
  Dummy: Integer);
var
  Graphics: TsgGDIPGraphics;
begin
  Graphics := nil;
  if Icm then
    FStatusOfFunction := GdipCreateFromHWNDICM(hwnd, Graphics)
  else
    FStatusOfFunction := GdipCreateFromHWND(hwnd, Graphics);
  SetNativeGraphics(Graphics);
  if FStatusOfFunction = Ok then
    FStatusOfFunction := SetPageUnit(UnitPixel);
end;

constructor TsgGDIPlusGraphics.Create(Image: TsgGDIPlusImage);
var
  Graphics: TsgGDIPGraphics;
begin
  Graphics:= nil;
  if (Image <> nil) then
    FStatusOfFunction := GdipGetImageGraphicsContext(Image.NativeImage, Graphics);
  SetNativeGraphics(Graphics);
  if FStatusOfFunction = Ok then
    FStatusOfFunction := SetPageUnit(UnitPixel);
end;

destructor TsgGDIPlusGraphics.Destroy;
begin
  GdipDeleteGraphics(NativeGraphics);
end;

procedure TsgGDIPlusGraphics.Flush(Intention: TsgGDIPFlushIntention = FlushIntentionFlush);
begin
  GdipFlush(NativeGraphics, Intention);
end;

function TsgGDIPlusGraphics.GetHDC: HDC;
begin
  SetStatus(GdipGetDC(NativeGraphics, Result));
end;

procedure TsgGDIPlusGraphics.ReleaseHDC(ADC: HDC);
begin
  SetStatus(GdipReleaseDC(NativeGraphics, ADC));
end;

function TsgGDIPlusGraphics.SetRenderingOrigin(X, Y: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetRenderingOrigin(NativeGraphics, X, Y));
end;

function TsgGDIPlusGraphics.GetRenderingOrigin(out X, Y: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetRenderingOrigin(NativeGraphics, X, Y));
end;

function TsgGDIPlusGraphics.SetCompositingMode(CompositingMode: TCompositingMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetCompositingMode(NativeGraphics, CompositingMode));
end;

function TsgGDIPlusGraphics.GetCompositingMode: TCompositingMode;
begin
  SetStatus(GdipGetCompositingMode(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.SetCompositingQuality(CompositingQuality: TCompositingQuality): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetCompositingQuality( NativeGraphics, CompositingQuality));
end;

function TsgGDIPlusGraphics.GetCompositingQuality: TCompositingQuality;
begin
  SetStatus(GdipGetCompositingQuality(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.SetTextRenderingHint(newMode: TTextRenderingHint): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetTextRenderingHint(NativeGraphics, newMode));
end;

function TsgGDIPlusGraphics.GetTextRenderingHint: TTextRenderingHint;
begin
  SetStatus(GdipGetTextRenderingHint(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.SetTextContrast(Contrast: UINT): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetTextContrast(NativeGraphics, Contrast));
end;

function TsgGDIPlusGraphics.GetTextContrast: UINT;
begin
  SetStatus(GdipGetTextContrast(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.GetInterpolationMode: TInterpolationMode;
var
  Mode: TInterpolationMode;
begin
  Mode := InterpolationModeInValid;
  SetStatus(GdipGetInterpolationMode(NativeGraphics, Mode));
  Result := Mode;
end;

function TsgGDIPlusGraphics.SetInterpolationMode(InterpolationMode: TInterpolationMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetInterpolationMode(NativeGraphics,
                             InterpolationMode));
end;

function TsgGDIPlusGraphics.GetSmoothingMode: TSmoothingMode;
var
  SmoothingMode: TSmoothingMode;
begin
  SmoothingMode := SmoothingModeInValid;
  SetStatus(GdipGetSmoothingMode(NativeGraphics,  SmoothingMode));
  Result := SmoothingMode;
end;

function TsgGDIPlusGraphics.SetSmoothingMode(SmoothingMode: TSmoothingMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetSmoothingMode(NativeGraphics, SmoothingMode));
end;

function TsgGDIPlusGraphics.GetPixelOffsetMode: TPixelOffsetMode;
var
  PixelOffsetMode: TPixelOffsetMode;
begin
  PixelOffsetMode := PixelOffsetModeInValid;
  SetStatus(GdipGetPixelOffsetMode(NativeGraphics, PixelOffsetMode));
  Result := PixelOffsetMode;
end;

function TsgGDIPlusGraphics.SetPixelOffsetMode(PixelOffsetMode: TPixelOffsetMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPixelOffsetMode(NativeGraphics, PixelOffsetMode));
end;

function TsgGDIPlusGraphics.SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetWorldTransform(NativeGraphics, AMatrix.NativeMatrix));
end;

function TsgGDIPlusGraphics.ResetTransform: TsgGDIPStatus;
begin
  Result := SetStatus(GdipResetWorldTransform(NativeGraphics));
end;

function TsgGDIPlusGraphics.MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipMultiplyWorldTransform(NativeGraphics, AMatrix.NativeMatrix, Order));
end;

function TsgGDIPlusGraphics.TranslateTransform(Dx, Dy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslateWorldTransform(NativeGraphics, Dx, Dy, Order));
end;

function TsgGDIPlusGraphics.ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipScaleWorldTransform(NativeGraphics, Sx, Sy, Order));
end;

function TsgGDIPlusGraphics.RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipRotateWorldTransform(NativeGraphics, Angle, Order));
end;

function TsgGDIPlusGraphics.GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetWorldTransform(NativeGraphics, AMatrix.NativeMatrix));
end;

function TsgGDIPlusGraphics.SetPageUnit(Unit_: TsgGDIPUnit): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPageUnit(NativeGraphics, Unit_));
  if Result = Ok then
    FSrcUnit := Unit_;
end;

function TsgGDIPlusGraphics.SetPageScale(Scale: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPageScale(NativeGraphics, Scale));
end;

function TsgGDIPlusGraphics.GetPageUnit: TsgGDIPUnit;
begin
  SetStatus(GdipGetPageUnit(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.GetPageScale: Single;
begin
  SetStatus(GdipGetPageScale(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.GetDpiX: Single;
begin
  SetStatus(GdipGetDpiX(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.GetDpiY: Single;
begin
  SetStatus(GdipGetDpiY(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.TransformPoints(DestSpace: TsgGDIPCoordinateSpace; SrcSpace: TsgGDIPCoordinateSpace;
  Pts: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTransformPoints(NativeGraphics, DestSpace, SrcSpace, Pts, Count));
end;

function TsgGDIPlusGraphics.TransformPoints(DestSpace: TsgGDIPCoordinateSpace; SrcSpace: TsgGDIPCoordinateSpace;
  Pts: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTransformPointsI(NativeGraphics, DestSpace, SrcSpace, Pts, Count));
end;

function TsgGDIPlusGraphics.GetNearestColor(var Color: TsgColor): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetNearestColor(NativeGraphics, @Color));
end;

function TsgGDIPlusGraphics.DrawLine(Pen: TsgGDIPlusPen; X1, Y1, X2, Y2: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawLine(NativeGraphics, Pen.NativePen, X1, Y1, X2, Y2));
end;

function TsgGDIPlusGraphics.DrawLine(Pen: TsgGDIPlusPen; const pt1, pt2: TsgGPointF): TsgGDIPStatus;
begin
  Result := DrawLine(Pen, pt1.X, pt1.Y, pt2.X, pt2.Y);
end;

function TsgGDIPlusGraphics.DrawLines(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawLines(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawLine(Pen: TsgGDIPlusPen; X1, Y1, X2, Y2: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawLineI(NativeGraphics, Pen.NativePen, X1, Y1, X2, Y2));
end;

function TsgGDIPlusGraphics.DrawLine(Pen: TsgGDIPlusPen; const Pt1, Pt2: TsgGPoint): TsgGDIPStatus;
begin
  Result := DrawLine(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TsgGDIPlusGraphics.DrawLines(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawLinesI(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawArc(Pen: TsgGDIPlusPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawArc(NativeGraphics, Pen.NativePen,  X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphics.DrawArc(Pen: TsgGDIPlusPen; const Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := DrawArc(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphics.DrawArc(Pen: TsgGDIPlusPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawArcI(NativeGraphics, Pen.NativePen, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphics.DrawArc(Pen: TsgGDIPlusPen; const Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := DrawArc(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphics.DrawBezier(Pen: TsgGDIPlusPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawBezier(NativeGraphics, Pen.NativePen, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TsgGDIPlusGraphics.DrawBezier(Pen: TsgGDIPlusPen; const Pt1, Pt2, Pt3, Pt4: TsgGPointF): TsgGDIPStatus;
begin
  Result := DrawBezier(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TsgGDIPlusGraphics.DrawBeziers(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawBeziers(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawBezier(Pen: TsgGDIPlusPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawBezierI(NativeGraphics, Pen.NativePen, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TsgGDIPlusGraphics.DrawBezier(Pen: TsgGDIPlusPen; const Pt1, Pt2, Pt3, Pt4: TsgGPoint): TsgGDIPStatus;
begin
  Result := DrawBezier(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TsgGDIPlusGraphics.DrawBeziers(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawBeziersI(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawRectangle(Pen: TsgGDIPlusPen; const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := DrawRectangle(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.DrawRectangle(Pen: TsgGDIPlusPen; X, Y, Width, Height: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawRectangle(NativeGraphics, Pen.NativePen, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.DrawRectangles(Pen: TsgGDIPlusPen; Rects: PsgGRectF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawRectangles(NativeGraphics, Pen.NativePen, Rects, Count));
end;

function TsgGDIPlusGraphics.DrawRectangle(Pen: TsgGDIPlusPen; const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := DrawRectangle(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.DrawRectangle(Pen: TsgGDIPlusPen; X, Y, Width, Height: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawRectangleI(NativeGraphics, Pen.NativePen, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.DrawRectangles(Pen: TsgGDIPlusPen; Rects: PsgGRect; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawRectanglesI(NativeGraphics, Pen.NativePen, Rects, Count));
end;

function TsgGDIPlusGraphics.DrawEllipse(Pen: TsgGDIPlusPen; const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := DrawEllipse(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.DrawEllipse(Pen: TsgGDIPlusPen; X, Y, Width, Height: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawEllipse(NativeGraphics, Pen.NativePen, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.DrawEllipse(Pen: TsgGDIPlusPen; const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := DrawEllipse(Pen, Rect.X,Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.DrawEllipse(Pen: TsgGDIPlusPen; X, Y, Width, Height: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawEllipseI(NativeGraphics, Pen.NativePen, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.DrawPie(Pen: TsgGDIPlusPen; const Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := DrawPie(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphics.DrawPie(Pen: TsgGDIPlusPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawPie(NativeGraphics, Pen.NativePen, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphics.DrawPie(Pen: TsgGDIPlusPen; const Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := DrawPie(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphics.DrawPie(Pen: TsgGDIPlusPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawPieI(NativeGraphics, Pen.NativePen, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphics.DrawPolygon(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawPolygon(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawPolygon(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawPolygonI(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawPath(Pen: TsgGDIPlusPen; Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
var
  vPen: TsgGDIPPen;
  vPath: TsgGDIPPath;
begin
  if Assigned(Pen) then
    vPen := Pen.NativePen
  else
    vPen  := nil;
  if Assigned(Path) then
    vPath := Path.NativePath
  else
    vPath := nil;
  Result := SetStatus(GdipDrawPath(NativeGraphics, vPen, vPath));
end;

function TsgGDIPlusGraphics.DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawCurve(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawCurve2(NativeGraphics, Pen.NativePen, Points, Count, tension));
end;

function TsgGDIPlusGraphics.DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count, Offset,
  NumberOfSegments: Integer; Tension: Single = 0.5): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawCurve3(NativeGraphics, Pen.NativePen, Points, Count, Offset, NumberOfSegments, tension));
end;

function TsgGDIPlusGraphics.DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawCurveI(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawCurve2I(NativeGraphics, Pen.NativePen, Points, Count, Tension));
end;

function TsgGDIPlusGraphics.DrawCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count, Offset,
 NumberOfSegments: Integer; Tension: Single = 0.5): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawCurve3I(NativeGraphics, Pen.NativePen, Points, Count, Offset, NumberOfSegments, Tension));
end;

function TsgGDIPlusGraphics.DrawClosedCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawClosedCurve(Pen: TsgGDIPlusPen; Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve2(NativeGraphics, Pen.NativePen, Points, Count, Tension));
end;

function TsgGDIPlusGraphics.DrawClosedCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawClosedCurveI(NativeGraphics, Pen.NativePen, Points, Count));
end;

function TsgGDIPlusGraphics.DrawClosedCurve(Pen: TsgGDIPlusPen; Points: PsgGPoint;
  Count: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipDrawClosedCurve2I(NativeGraphics, Pen.NativePen, Points, Count, Tension));
end;

function TsgGDIPlusGraphics.Clear(Color: TsgColor): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGraphicsClear(NativeGraphics, Color));
end;

function TsgGDIPlusGraphics.FillRectangle(Brush: TsgGDIPlusBrush; const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := FillRectangle(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.FillRectangle(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillRectangle(NativeGraphics, Brush.NativeBrush, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.FillRectangles(Brush: TsgGDIPlusBrush; Rects: PsgGRectF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillRectangles(NativeGraphics, Brush.NativeBrush, Rects, Count));
end;

function TsgGDIPlusGraphics.FillRectangle(Brush: TsgGDIPlusBrush; const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := FillRectangle(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.FillRectangle(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillRectangleI(NativeGraphics, Brush.NativeBrush, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.FillRectangles(Brush: TsgGDIPlusBrush; Rects: PsgGRect; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillRectanglesI(NativeGraphics, Brush.NativeBrush, Rects, Count));
end;

function TsgGDIPlusGraphics.FillPolygon(Brush: TsgGDIPlusBrush; Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := FillPolygon(Brush, Points, Count, FillModeAlternate);
end;

function TsgGDIPlusGraphics.FillPolygon(Brush: TsgGDIPlusBrush; Points: PsgGPointF; Count: Integer; FillMode: TsgGDIPFillMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillPolygon(NativeGraphics, Brush.NativeBrush, Points, Count, FillMode));
end;

function TsgGDIPlusGraphics.FillPolygon(Brush: TsgGDIPlusBrush; Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := FillPolygon(Brush, Points, Count, FillModeAlternate);
end;

function TsgGDIPlusGraphics.FillPolygon(Brush: TsgGDIPlusBrush; Points: PsgGPoint; Count: Integer; FillMode: TsgGDIPFillMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillPolygonI(NativeGraphics, Brush.NativeBrush, Points, Count, FillMode));
end;

function TsgGDIPlusGraphics.FillEllipse(Brush: TsgGDIPlusBrush; const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := FillEllipse(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.FillEllipse(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillEllipse(NativeGraphics, Brush.NativeBrush, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.FillEllipse(Brush: TsgGDIPlusBrush; const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := FillEllipse(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.FillEllipse(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillEllipseI(NativeGraphics, Brush.NativeBrush, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.FillPie(Brush: TsgGDIPlusBrush; const Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := FillPie(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphics.FillPie(Brush: TsgGDIPlusBrush; X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillPie(NativeGraphics, Brush.NativeBrush, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphics.FillPie(Brush: TsgGDIPlusBrush; const Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := FillPie(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphics.FillPie(Brush: TsgGDIPlusBrush; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillPieI(NativeGraphics, Brush.NativeBrush, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphics.FillPath(Brush: TsgGDIPlusBrush; Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillPath(NativeGraphics, Brush.NativeBrush, Path.NativePath));
end;

function TsgGDIPlusGraphics.FillClosedCurve(Brush: TsgGDIPlusBrush; Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillClosedCurve(NativeGraphics, Brush.NativeBrush, Points, Count));
end;

function TsgGDIPlusGraphics.FillClosedCurve(Brush: TsgGDIPlusBrush; Points: PsgGPointF; Count: Integer; FillMode: TsgGDIPFillMode; Tension: Single = 0.5): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2(NativeGraphics, Brush.NativeBrush, Points, Count, Tension, FillMode));
end;

function TsgGDIPlusGraphics.FillClosedCurve(Brush: TsgGDIPlusBrush; Points:  PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillClosedCurveI(NativeGraphics, Brush.NativeBrush, Points, Count));
end;

function TsgGDIPlusGraphics.FillClosedCurve(Brush: TsgGDIPlusBrush; Points: PsgGPoint;
  Count: Integer; FillMode: TsgGDIPFillMode; Tension: Single = 0.5): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2I(NativeGraphics, Brush.NativeBrush, Points, Count, Tension, FillMode));
end;

function TsgGDIPlusGraphics.FillRegion(Brush: TsgGDIPlusBrush; Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipFillRegion(NativeGraphics, Brush.NativeBrush, Region.NativeRegion));
end;

function TsgGDIPlusGraphics.DrawString( String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const ALayoutRect: TsgGRectF; stringFormat: TsgGDIPlusStringFormat; Brush: TsgGDIPlusBrush): TsgGDIPStatus;
var
  vFont: TsgGDIPFont;
  vStringFormat: TsgGDIPStringFormat;
  vBrush: TsgGDIPBrush;
begin
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(stringFormat) then
    vStringFormat := StringFormat.NativeFormat
  else
    vStringFormat := nil;
  if Assigned(Brush) then
    vBrush := Brush.NativeBrush
  else
    vBrush := nil;
  Result := SetStatus(GdipDrawString(NativeGraphics, PWideChar(String_), Length, vFont, @ALayoutRect, vStringFormat, vBrush));
end;

function TsgGDIPlusGraphics.DrawString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const AOrg: TsgGPointF; Brush: TsgGDIPlusBrush): TsgGDIPStatus;
var
  Rect: TsgGRectF;
  vFont: TsgGDIPFont;
  vBrush: TsgGDIPBrush;
begin
  Rect.X := AOrg.X;
  Rect.Y := AOrg.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(Brush) then
    vBrush := Brush.NativeBrush
  else
    vBrush := nil;
  Result := SetStatus(GdipDrawString(NativeGraphics, PWideChar(String_), Length, vFont, @Rect, nil, vBrush));
end;

function TsgGDIPlusGraphics.DrawString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const AOrg: TsgGPointF; stringFormat: TsgGDIPlusStringFormat; Brush: TsgGDIPlusBrush): TsgGDIPStatus;
var
  Rect: TsgGRectF;
  vFont: TsgGDIPFont;
  vStringFormat: TsgGDIPStringFormat;
  vBrush: TsgGDIPBrush;
begin
  Rect.X := AOrg.X;
  Rect.Y := AOrg.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(stringFormat) then
    vStringFormat := stringFormat.NativeFormat
  else
    vStringFormat := nil;
  if Assigned(Brush) then
    vBrush := Brush.NativeBrush
  else
    vBrush := nil;
  Result := SetStatus(GdipDrawString(NativeGraphics, PWideChar(String_), Length, vFont, @Rect, vStringFormat, vBrush));
end;

function TsgGDIPlusGraphics.MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const ALayoutRect: TsgGRectF; StringFormat: TsgGDIPlusStringFormat; out BoundingBox: TsgGRectF;
  CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TsgGDIPStatus;
var
  vFont: TsgGDIPFont;
  vStringFormat: TsgGDIPStringFormat;
begin
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(StringFormat) then
    vStringFormat := StringFormat.NativeFormat
  else
    vStringFormat := nil;
  Result := SetStatus(GdipMeasureString(NativeGraphics, PWideChar(String_), Length, vFont, @ALayoutRect,
    vStringFormat, @BoundingBox, CodePointsFitted, LinesFilled));
end;


function TsgGDIPlusGraphics.MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const ALayoutRectSize: TsgGSizeF; StringFormat: TsgGDIPlusStringFormat; out Size: TsgGSizeF;
  CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TsgGDIPStatus;
var
  LayoutRect, BoundingBox: TsgGRectF;
  Status: TsgGDIPStatus;
  vFont: TsgGDIPFont;
  vStringFormat: TsgGDIPStringFormat;
begin
  LayoutRect.X := 0;
  LayoutRect.Y := 0;
  LayoutRect.Width := ALayoutRectSize.Width;
  LayoutRect.Height := ALayoutRectSize.Height;
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(StringFormat) then
    vStringFormat := StringFormat.NativeFormat
  else
    vStringFormat := nil;
  Status := SetStatus(GdipMeasureString(NativeGraphics, PWideChar(String_), Length, vFont, @LayoutRect,
    vStringFormat, @BoundingBox, CodePointsFitted, LinesFilled));
  if (Status = Ok) then
  begin
    Size.Width  := BoundingBox.Width;
    Size.Height := BoundingBox.Height;
  end;
  Result := Status;
end;

function TsgGDIPlusGraphics.MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const AOrg: TsgGPointF; StringFormat: TsgGDIPlusStringFormat; out BoundingBox: TsgGRectF): TsgGDIPStatus;
var
  Rect: TsgGRectF;
  vFont: TsgGDIPFont;
  vStringFormat: TsgGDIPStringFormat;
begin
  Rect.X := AOrg.X;
  Rect.Y := AOrg.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(StringFormat) then
    vStringFormat := StringFormat.NativeFormat
  else
    vStringFormat := nil;
  Result := SetStatus(GdipMeasureString(NativeGraphics, PWideChar(String_), Length, vFont, @Rect, vStringFormat,
    @BoundingBox, nil, nil));
end;

function TsgGDIPlusGraphics.MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const ALayoutRect: TsgGRectF; out BoundingBox: TsgGRectF): TsgGDIPStatus;
var
  vFont: TsgGDIPFont;
begin
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  Result := SetStatus(GdipMeasureString(NativeGraphics, PWideChar(String_), Length, vFont, @ALayoutRect,
    nil, @BoundingBox, nil, nil));
end;

function TsgGDIPlusGraphics.MeasureString(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const AOrg: TsgGPointF; out BoundingBox: TsgGRectF): TsgGDIPStatus;
var
  vFont: TsgGDIPFont;
  Rect: TsgGRectF;
begin
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  Rect.X := AOrg.X;
  Rect.Y := AOrg.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;
  Result := SetStatus(GdipMeasureString(NativeGraphics, PWideChar(String_), Length,
    vFont, @Rect, nil, @BoundingBox, nil, nil));
end;

function TsgGDIPlusGraphics.MeasureCharacterRanges(String_: WideString; Length: Integer; Font: TsgGDIPlusFont;
  const ALayoutRect: TsgGRectF; StringFormat: TsgGDIPlusStringFormat; RegionCount: Integer;
  const Regions: array of TsgGDIPlusRegion): TsgGDIPStatus;
var
  NativeRegions: Pointer;
  I: Integer;
  vStatus: TsgGDIPStatus;
  vFont: TsgGDIPFont;
  vStringFormat: TsgGDIPStringFormat;
type
  TArrayGpRegion = array of TsgGDIPRegion;
begin
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(StringFormat) then
    vStringFormat := StringFormat.NativeFormat
  else
    vStringFormat := nil;
  if (RegionCount <= 0) then
  begin
    Result := InValidParameter;
    Exit;
  end;
  GetMem(NativeRegions, Sizeof(TsgGDIPRegion)* RegionCount);
  for I := 0 to RegionCount - 1 do
    TArrayGpRegion(NativeRegions)[I] := Regions[I].NativeRegion;
  vStatus := SetStatus(GdipMeasureCharacterRanges(NativeGraphics, PWideChar(String_),
    Length, vFont, @ALayoutRect, vStringFormat, RegionCount, NativeRegions));
  FreeMem(NativeRegions, Sizeof(TsgGDIPRegion)* RegionCount);
  Result := vStatus;
end;

function TsgGDIPlusGraphics.DrawDriverString(text: PUINT16; Length: Integer; Font: TsgGDIPlusFont;
  Brush: TsgGDIPlusBrush; Positions: PsgGPointF; Flags: Integer; const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
var
  vFont: TsgGDIPFont;
  vBrush: TsgGDIPBrush;
  vMatrix: TsgGDIPMatrix;
begin
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(Brush) then
    vBrush := Brush.NativeBrush
  else
    vBrush := nil;
  if Assigned(AMatrix) then
    vMatrix := AMatrix.NativeMatrix
  else
    vMatrix := nil;
  Result := SetStatus(GdipDrawDriverString(NativeGraphics, Text, Length, vFont, vBrush, Positions, Flags, vMatrix));
end;

function TsgGDIPlusGraphics.MeasureDriverString(text: PUINT16; Length: Integer; Font: TsgGDIPlusFont;
  Positions: PsgGPointF; Flags: Integer; const AMatrix: TsgGDIPlusMatrix; out BoundingBox: TsgGRectF): TsgGDIPStatus;
var
  vFont: TsgGDIPFont;
  vMatrix: TsgGDIPMatrix;
begin
  if Assigned(Font) then
    vFont := Font.NativeFont
  else
    vFont := nil;
  if Assigned(AMatrix) then
    vMatrix := AMatrix.NativeMatrix
  else
    vMatrix := nil;
  Result := SetStatus(GdipMeasureDriverString(NativeGraphics, Text, Length, vFont, Positions, Flags, vMatrix, @BoundingBox));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; const Point: TsgGPointF): TsgGDIPStatus;
begin
  Result := DrawImage(Image, Point.X, Point.Y);
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; X, Y: Single): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
begin
  if Assigned(Image) then vImage := Image.NativeImage else vImage := nil;
  Result := SetStatus(GdipDrawImage(NativeGraphics, vImage, X, Y));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := DrawImage(Image, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; X, Y, Width, Height: Single): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  Result := SetStatus(GdipDrawImageRect(NativeGraphics, vImage, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; const Point: TsgGPoint): TsgGDIPStatus;
begin
  Result := DrawImage(Image, Point.X, Point.Y);
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; X, Y: Integer): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  Result := SetStatus(GdipDrawImageI(NativeGraphics, vImage, X, Y));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := DrawImage(Image, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; X, Y, Width, Height: Integer): TsgGDIPStatus;
var
 vImage: TsgGDIPImage;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  Result := SetStatus(GdipDrawImageRectI(NativeGraphics, vImage, X, Y, Width, Height));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPointF; Count: Integer): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
begin
  if (((Count <> 3) and (Count <> 4)) or (DestPoints = nil)) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  Result := SetStatus(GdipDrawImagePoints(NativeGraphics, vImage, DestPoints, Count));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPoint; Count: Integer): TsgGDIPStatus;
var
 vImage: TsgGDIPImage;
begin
  if (((Count <> 3) and (Count <> 4))or (DestPoints = nil)) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  Result := SetStatus(GdipDrawImagePointsI(NativeGraphics, vImage, DestPoints, Count));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TsgGDIPUnit): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  Result := SetStatus(GdipDrawImagePointRect(NativeGraphics, vImage, X, Y, SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; const DestRect: TsgGRectF; SrcX, SrcY, SrcWidth, SrcHeight: Single;
  SrcUnit: TsgGDIPUnit; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipDrawImageRectRect(NativeGraphics, vImage, DestRect.X, DestRect.Y, DestRect.Width,
    DestRect.Height, SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit, vImageAttributes, FCallBack, FCallBackData));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPointF; Count: Integer; SrcX, SrcY, SrcWidth, SrcHeight: Single;
  ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipDrawImagePointsRect(NativeGraphics, vImage, DestPoints, Count, SrcX, SrcY,
    SrcWidth, SrcHeight, SrcUnit, vImageAttributes, FCallBack, FCallBackData));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TsgGDIPUnit): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  Result := SetStatus(GdipDrawImagePointRectI(NativeGraphics, vImage, X, Y, SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; const DestRect: TsgGRect; SrcX, SrcY, SrcWidth,
  SrcHeight: Integer; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipDrawImageRectRectI(NativeGraphics, vImage, DestRect.X, DestRect.Y, DestRect.Width,
    DestRect.Height, SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit, vImageAttributes, FCallBack, FCallBackData));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPoint;
  Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipDrawImagePointsRectI(NativeGraphics, vImage, DestPoints, Count,
    SrcX, SrcY, SrcWidth, SrcHeight, FSrcUnit, vImageAttributes, FCallBack, FCallBackData));
end;

function TsgGDIPlusGraphics.DrawImage(Image: TsgGDIPlusImage; DestPoints: PsgGPoint;
  Count, SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit: Integer; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vImage: TsgGDIPImage;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(Image) then
    vImage := Image.NativeImage
  else
    vImage := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipDrawImagePointsRectI(NativeGraphics, vImage, DestPoints, Count,
    SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit, vImageAttributes, FCallBack, FCallBackData));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestPoint: TsgGPointF;
  MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileDestPoint(NativeGraphics, vMetaFile, @DestPoint, MCallBack, FCallBackData, vImageAttributes));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestPoint: TsgGPoint;
  MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileDestPointI(NativeGraphics, vMetaFile, @DestPoint, MCallBack, FCallBackData, vImageAttributes));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestRect: TsgGRectF;
  MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileDestRect(NativeGraphics, vMetaFile, @DestRect, MCallBack,
    FCallBackData, vImageAttributes));
end;


function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestRect: TsgGRect;
  MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileDestRectI(NativeGraphics, vMetaFile, @DestRect, MCallBack, FCallBackData, vImageAttributes));
end;


function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; DestPoints: PsgGPointF;
  Count: Integer; MCallBack: ENumerateMetaFileProc;
  ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileDestPoints(NativeGraphics, vMetaFile, DestPoints,  Count, MCallBack, FCallBackData, vImageAttributes));
end;

    
function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; DestPoints: PsgGPoint;
  Count: Integer; MCallBack: ENumerateMetaFileProc;
  ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileDestPointsI(NativeGraphics, vMetaFile, DestPoints,  Count, MCallBack, FCallBackData, vImageAttributes));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestPoint: TsgGPointF;
  const SrcRect: TsgGRectF; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileSrcRectDestPoint(NativeGraphics, vMetaFile,
    @DestPoint, @SrcRect, SrcUnit, MCallBack, FCallBackData, vImageAttributes));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestPoint: TsgGPoint;
  const SrcRect: TsgGRect; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileSrcRectDestPointI(NativeGraphics, vMetaFile,
    @DestPoint, @SrcRect, SrcUnit, MCallBack, FCallBackData, vImageAttributes));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestRect: TsgGRectF;
  const SrcRect: TsgGRectF; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileSrcRectDestRect(NativeGraphics, vMetaFile,
    @DestRect, @SrcRect, SrcUnit, MCallBack, FCallBackData, vImageAttributes));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; const DestRect, SrcRect: TsgGRect;
  MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileSrcRectDestRectI(NativeGraphics, vMetaFile,
    @DestRect, @SrcRect, SrcUnit, MCallBack, FCallBackData, vImageAttributes));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile( MetaFile: TsgGDIPlusMetaFile; DestPoints: PsgGPointF;
  Count: Integer; const SrcRect: TsgGRectF; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileSrcRectDestPoints(NativeGraphics, vMetaFile, DestPoints,
    Count, @SrcRect, SrcUnit, MCallBack, FCallBackData, vImageAttributes));
end;

function TsgGDIPlusGraphics.ENumerateMetaFile(MetaFile: TsgGDIPlusMetaFile; DestPoints: PsgGPoint;
  Count: Integer; const SrcRect: TsgGRect; MCallBack: ENumerateMetaFileProc; ImageAttributes: TsgGDIPlusImageAttributes = nil): TsgGDIPStatus;
var
  vMetaFile: TsgGDIPMetaFile;
  vImageAttributes: TsgGDIPImageAttributes;
begin
  if Assigned(MetaFile) then
    vMetaFile := TsgGDIPMetaFile(MetaFile.NativeImage)
  else
    vMetaFile := nil;
  if Assigned(ImageAttributes) then
    vImageAttributes := ImageAttributes.NativeImageAttr
  else
    vImageAttributes := nil;
  Result := SetStatus(GdipENumerateMetaFileSrcRectDestPointsI(NativeGraphics, vMetaFile,
    DestPoints, Count, @SrcRect, SrcUnit, MCallBack, FCallBackData, vImageAttributes));
end;
    
function TsgGDIPlusGraphics.SetClip(Graph: TsgGDIPlusGraphics; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipGraphics(NativeGraphics, Graph.NativeGraphics, ACombineMode));
end;

function TsgGDIPlusGraphics.SetClip(Rect: TsgGRectF; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRect(NativeGraphics, Rect.X, Rect.Y, Rect.Width, Rect.Height, ACombineMode));
end;

function TsgGDIPlusGraphics.SetClip(Rect: TsgGRect; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRectI(NativeGraphics, Rect.X, Rect.Y, Rect.Width, Rect.Height, ACombineMode));
end;

function TsgGDIPlusGraphics.SetClip(Path: TsgGDIPlusGraphicsPath; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipPath(NativeGraphics, Path.NativePath, ACombineMode));
end;

function TsgGDIPlusGraphics.SetClip(Region: TsgGDIPlusRegion; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRegion(NativeGraphics, Region.NativeRegion, ACombineMode));
end;

function TsgGDIPlusGraphics.SetClip(hRgn: HRGN; ACombineMode: TCombineMode = CombineModeReplace): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipHrgn(NativeGraphics, hRgn, ACombineMode));
end;

function TsgGDIPlusGraphics.IntersectClip(const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRect(NativeGraphics, Rect.X, Rect.Y,
    Rect.Width, Rect.Height, CombineModeIntersect));
end;

function TsgGDIPlusGraphics.IntersectClip(const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRectI(NativeGraphics, Rect.X, Rect.Y,
    Rect.Width, Rect.Height, CombineModeIntersect));
end;

procedure TsgGDIPlusGraphics.InitParams(const AOptimization: TsgGDIPlusOpimization = opSpeed);
begin
  case AOptimization of
    opSpeed:
      begin
        SetPixelOffsetMode(PixelOffsetModeHalf);
        SetInterpolationMode(InterpolationModeNearestNeighbor);
      end;
    opQuality:
      begin
        SetPixelOffsetMode(PixelOffsetModeHighQuality);
        SetInterpolationMode(InterpolationModeHighQualityBicubic);
      end;
  end;
end;

function TsgGDIPlusGraphics.IntersectClip(Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRegion(NativeGraphics, Region.NativeRegion, CombineModeIntersect));
end;

function TsgGDIPlusGraphics.ExcludeClip(const Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRect(NativeGraphics, Rect.X, Rect.Y,
    Rect.Width, Rect.Height, CombineModeExclude));
end;

function TsgGDIPlusGraphics.ExcludeClip(const Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRectI(NativeGraphics, Rect.X, Rect.Y,
    Rect.Width, Rect.Height, CombineModeExclude));
end;

function TsgGDIPlusGraphics.ExcludeClip(Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetClipRegion(NativeGraphics, Region.NativeRegion, CombineModeExclude));
end;

function TsgGDIPlusGraphics.ResetClip: TsgGDIPStatus;
begin
  Result := SetStatus(GdipResetClip(NativeGraphics));
end;

function TsgGDIPlusGraphics.TranslateClip(Dx, Dy: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslateClip(NativeGraphics, Dx, Dy));
end;

function TsgGDIPlusGraphics.TranslateClip(Dx, Dy: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslateClipI(NativeGraphics, Dx, Dy));
end;

function TsgGDIPlusGraphics.GetClip(Region: TsgGDIPlusRegion): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetClip(NativeGraphics, Region.NativeRegion));
end;

function TsgGDIPlusGraphics.GetClipBounds(out Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetClipBounds(NativeGraphics, @Rect));
end;

function TsgGDIPlusGraphics.GetClipBounds(out Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetClipBoundsI(NativeGraphics, @Rect));
end;

function TsgGDIPlusGraphics.IsClipEmpty: Bool;
var
  vBoolVal: BOOL;
begin
  vBoolVal := False;
  SetStatus(GdipIsClipEmpty(NativeGraphics, @vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.GetVisibleClipBounds(out Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBounds(NativeGraphics, @Rect));
end;

function TsgGDIPlusGraphics.GetVisibleClipBounds(out Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBoundsI(NativeGraphics, @Rect));
end;

function TsgGDIPlusGraphics.IsVisibleClipEmpty: BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := False;
  SetStatus(GdipIsVisibleClipEmpty(NativeGraphics, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.IsVisible(X, Y: Integer): BOOL;
var
  Pt: TsgGPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  Result := IsVisible(Pt);
end;

function TsgGDIPlusGraphics.IsVisible(const Point: TsgGPoint): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := False;
  SetStatus(GdipIsVisiblePointI(NativeGraphics, Point.X, Point.Y, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.IsVisible(X, Y, Width, Height: Integer): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := True;
  SetStatus(GdipIsVisibleRectI(NativeGraphics, X, Y, Width, Height, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.IsVisible(const Rect: TsgGRect): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := True;
  SetStatus(GdipIsVisibleRectI(NativeGraphics, Rect.X, Rect.Y, Rect.Width, Rect.Height, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.IsVisible(X, Y: Single): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := False;
  SetStatus(GdipIsVisiblePoint(NativeGraphics, X, Y, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.IsVisible(const Point: TsgGPointF): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := False;
  SetStatus(GdipIsVisiblePoint(NativeGraphics, Point.X, Point.Y, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.IsVisible(X, Y, Width, Height: Single): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := True;
  SetStatus(GdipIsVisibleRect(NativeGraphics, X, Y, Width, Height, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.IsVisible(const Rect: TsgGRectF): BOOL;
var
  vBoolVal: BOOL;
begin
  vBoolVal := True;
  SetStatus(GdipIsVisibleRect(NativeGraphics, Rect.X, Rect.Y, Rect.Width, Rect.Height, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphics.Save: GraphicsState;
begin
  SetStatus(GdipSaveGraphics(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.Restore(gState: GraphicsState): TsgGDIPStatus;
begin
  Result := SetStatus(GdipRestoreGraphics(NativeGraphics, gState));
end;

function TsgGDIPlusGraphics.BeginContainer(const DstRect,SrcRect: TsgGRectF; Unit_: TsgGDIPUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainer(NativeGraphics, @DstRect, @SrcRect, Unit_, Result));
end;

function TsgGDIPlusGraphics.BeginContainer(const DstRect, SrcRect: TsgGRect; Unit_: TsgGDIPUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainerI(NativeGraphics, @DstRect, @SrcRect, Unit_, Result));
end;

function TsgGDIPlusGraphics.BeginContainer: GraphicsContainer;
begin
  SetStatus(GdipBeginContainer2(NativeGraphics, Result));
end;

function TsgGDIPlusGraphics.EndContainer(State: GraphicsContainer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipEndContainer(NativeGraphics, State));
end;

function TsgGDIPlusGraphics.AddMetaFileComment(Data: PBYTE; SizeData: UINT): TsgGDIPStatus;
begin
  Result := SetStatus(GdipComment(NativeGraphics, SizeData, data));
end;

function TsgGDIPlusGraphics.GetHalftonePalette: HPALETTE;
begin
  Result := GdipCreateHalftonePalette;
end;

function TsgGDIPlusGraphics.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

constructor TsgGDIPlusGraphics.Create(Graphics: TsgGDIPGraphics; Dummy: Integer);
begin
  FStatusOfFunction := Ok;
  SetNativeGraphics(Graphics);
end;

procedure TsgGDIPlusGraphics.SetNativeGraphics(Graphics: TsgGDIPGraphics);
begin
  Self.NativeGraphics := Graphics;
end;

function TsgGDIPlusGraphics.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then FStatusOfFunction := Status;
  Result := Status;
end;

function TsgGDIPlusGraphics.GetNativeGraphics: TsgGDIPGraphics;
begin
  Result := Self.NativeGraphics;
end;

function TsgGDIPlusGraphics.GetNativePen(Pen: TsgGDIPlusPen): TsgGDIPPen;
begin
  Result := Pen.NativePen;
end;

constructor TsgGDIPlusFontFamily.Create;
begin
  NativeFamily := nil;
  FStatusOfFunction   := Ok;
end;

constructor TsgGDIPlusFontFamily.Create(Name: WideString; FontCollection: TsgGDIPlusFontCollection = nil);
var
  vFontCollection: TsgGDIPFontCollection;
begin
  NativeFamily := nil;
  if Assigned(FontCollection) then
    vFontCollection := FontCollection.NativeFontCollection
  else
    vFontCollection := nil;
  FStatusOfFunction := GdipCreateFontFamilyFromName(PWideChar(Name), vFontCollection, NativeFamily);
end;

destructor TsgGDIPlusFontFamily.Destroy;
begin
  GdipDeleteFontFamily (NativeFamily);
end;

class function TsgGDIPlusFontFamily.GenericSansSerif: TsgGDIPlusFontFamily;
var
  vFontFamily: TsgGDIPFontFamily;
begin
  if (GenericSansSerifFontFamily <> nil) then
  begin
    Result := GenericSansSerifFontFamily;
    Exit;
  end;
  GenericSansSerifFontFamily := TsgGDIPlusFontFamily.Create;
  GenericSansSerifFontFamily.FStatusOfFunction := GdipGetGenericFontFamilySansSerif(vFontFamily);
  GenericSansSerifFontFamily.NativeFamily := vFontFamily;
  Result := GenericSansSerifFontFamily;
end;

class function TsgGDIPlusFontFamily.GenericSerif: TsgGDIPlusFontFamily;
var
  vFontFamily: TsgGDIPFontFamily;
begin
  if (GenericSerifFontFamily <> nil) then
  begin
    Result := GenericSerifFontFamily;
    Exit;
  end;
  GenericSerifFontFamily := TsgGDIPlusFontFamily.Create;
  GenericSerifFontFamily.FStatusOfFunction := GdipGetGenericFontFamilySerif(vFontFamily);
  GenericSerifFontFamily.NativeFamily := vFontFamily;
  Result := GenericSerifFontFamily;
end;

class function TsgGDIPlusFontFamily.GenericMonospace: TsgGDIPlusFontFamily;
var
  vFontFamily: TsgGDIPFontFamily;
begin
  if (GenericMonospaceFontFamily <> nil) then
  begin
    Result := GenericMonospaceFontFamily;
    Exit;
  end;
  GenericMonospaceFontFamily := TsgGDIPlusFontFamily.Create;
  GenericMonospaceFontFamily.FStatusOfFunction := GdipGetGenericFontFamilyMonospace(vFontFamily);
  GenericMonospaceFontFamily.NativeFamily := vFontFamily;
  Result := GenericMonospaceFontFamily;
end;

function TsgGDIPlusFontFamily.GetFamilyName(out Name: string; Language: LANGID = 0): TsgGDIPStatus;
var
  Str: array[0..LF_FACESIZE - 1] of WideChar;
begin
  Result := SetStatus(GdipGetFamilyName(NativeFamily, @Str, Language));
  Name := Str;
end;

function TsgGDIPlusFontFamily.Clone: TsgGDIPlusFontFamily;
var
  ClonedFamily: TsgGDIPFontFamily;
begin
  ClonedFamily := nil;
  SetStatus(GdipCloneFontFamily (NativeFamily, ClonedFamily));
  Result := TsgGDIPlusFontFamily.Create(ClonedFamily, FStatusOfFunction);
end;

function TsgGDIPlusFontFamily.IsAvailable: BOOL;
begin
  Result := (NativeFamily <> nil);
end;

function TsgGDIPlusFontFamily.IsStyleAvailable(Style: Integer): BOOL;
var
  StyleAvailable: BOOL;
  Status: TsgGDIPStatus;
begin
  Status := SetStatus(GdipIsStyleAvailable(NativeFamily, Style, StyleAvailable));
  if (Status <> Ok) then
    StyleAvailable := False;
  Result := StyleAvailable;
end;

function TsgGDIPlusFontFamily.GetEmHeight(Style: Integer): UINT16;
begin
  SetStatus(GdipGetEmHeight(NativeFamily, Style, Result));
end;

function TsgGDIPlusFontFamily.GetCellAscent(Style: Integer): UINT16;
begin
  SetStatus(GdipGetCellAscent(NativeFamily, Style, Result));
end;

function TsgGDIPlusFontFamily.GetCellDescent(Style: Integer): UINT16;
begin
  SetStatus(GdipGetCellDescent(NativeFamily, Style, Result));
end;

function TsgGDIPlusFontFamily.GetLineSpacing(Style: Integer): UINT16;
begin
  SetStatus(GdipGetLineSpacing(NativeFamily, Style, Result));
end;

function TsgGDIPlusFontFamily.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

function TsgGDIPlusFontFamily.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

constructor TsgGDIPlusFontFamily.Create(NativeOrig: TsgGDIPFontFamily; Status: TsgGDIPStatus);
begin
  FStatusOfFunction  := Status;
  NativeFamily := NativeOrig;
end;

constructor TsgGDIPlusFont.Create(ADC: HDC);
var
  Font: TsgGDIPFont;
begin
  Font := nil;
  FStatusOfFunction := GdipCreateFontFromDC(ADC, Font);
  SetNativeFont(Font);
end;

constructor TsgGDIPlusFont.Create(ADC: HDC; ALogFont: PLogFontA);
var
  Font: TsgGDIPFont;
begin
  Font := nil;
  if Assigned(ALogFont) then
    FStatusOfFunction := GdipCreateFontFromLogFontA(ADC, ALogFont, Font)
  else
    FStatusOfFunction := GdipCreateFontFromDC(ADC, Font);
  SetNativeFont(Font);
end;

constructor TsgGDIPlusFont.Create(ADC: HDC; ALogFont: PLogFontW);
var
  Font: TsgGDIPFont;
begin
  Font := nil;
  if Assigned(ALogFont) then
    FStatusOfFunction := GdipCreateFontFromLogFontW(ADC, ALogFont, Font)
  else
    FStatusOfFunction := GdipCreateFontFromDC(ADC, Font);
  SetNativeFont(Font);
end;

constructor TsgGDIPlusFont.Create(ADC: HDC; AFont: HFONT);
var
  Font: TsgGDIPFont;
  lf: LOGFONTA;
begin
  Font := nil;
  if BOOL(AFont) then
  begin
    if (BOOL(GetObjectA(AFont, Sizeof(LOGFONTA), @lf))) then
      FStatusOfFunction := GdipCreateFontFromLogFontA(ADC, @lf, Font)
    else
      FStatusOfFunction := GdipCreateFontFromDC(ADC, Font);
  end
  else
    FStatusOfFunction := GdipCreateFontFromDC(ADC, Font);
  SetNativeFont(Font);
end;

constructor TsgGDIPlusFont.Create(Family: TsgGDIPlusFontFamily; ASize: Single;
    Style: TFontStyle = FontStyleRegular; Unit_: TsgGDIPUnit = UnitPoint);
var
  Font: TsgGDIPFont;
  vFontFamily: TsgGDIPFontFamily;
begin
  Font := nil;
  if Assigned(Family) then
    vFontFamily := Family.NativeFamily
  else
    vFontFamily := nil;
  FStatusOfFunction := GdipCreateFont(vFontFamily, ASize, Integer(Style), Integer(Unit_), Font);
  SetNativeFont(Font);
end;

constructor TsgGDIPlusFont.Create(FamilyName: WideString; ASize: Single;
  Style: TFontStyle = FontStyleRegular; Unit_: TsgGDIPUnit = UnitPoint; FontCollection: TsgGDIPlusFontCollection = nil);
var
  Family: TsgGDIPlusFontFamily;
  NativeFamily: TsgGDIPFontFamily;
begin
  NativeFont := nil;
  Family := TsgGDIPlusFontFamily.Create(FamilyName, FontCollection);
  NativeFamily := Family.NativeFamily;
  FStatusOfFunction := Family.GetStatusOfFunction;
  if (FStatusOfFunction <> Ok) then
  begin
    NativeFamily := TsgGDIPlusFontFamily.GenericSansSerif.NativeFamily;
    FStatusOfFunction := TsgGDIPlusFontFamily.GenericSansSerif.FStatusOfFunction;
    if (FStatusOfFunction <> Ok) then
    begin
      Family.Free;
      Exit;
    end;
  end;
  FStatusOfFunction := GdipCreateFont(NativeFamily, ASize, Integer(Style), Integer(Unit_), NativeFont);
  if (FStatusOfFunction <> Ok) then
  begin
    NativeFamily := TsgGDIPlusFontFamily.GenericSansSerif.NativeFamily;
    FStatusOfFunction := TsgGDIPlusFontFamily.GenericSansSerif.FStatusOfFunction;
    if (FStatusOfFunction <> Ok) then
    begin
      Family.Free;
      Exit;
    end;
    FStatusOfFunction := GdipCreateFont(NativeFamily, ASize, Integer(Style), Integer(Unit_), NativeFont);
  end;
  Family.Free;
end;

function TsgGDIPlusFont.GetLogFontA(Graph: TsgGDIPlusGraphics; out LogFontA: TLogFontA): TsgGDIPStatus;
var
  vGraphics: TsgGDIPGraphics;
begin
  if Assigned(Graph) then
    vGraphics := Graph.NativeGraphics
  else
    vGraphics := nil;
  Result := SetStatus(GdipGetLogFontA(NativeFont, vGraphics, LogFontA));
end;

function TsgGDIPlusFont.GetLogFontW(Graph: TsgGDIPlusGraphics; out LogFontW: TLogFontW): TsgGDIPStatus;
var
  vGraphics: TsgGDIPGraphics;
begin
  if Assigned(Graph) then
    vGraphics := Graph.NativeGraphics
  else
    vGraphics := nil;
  Result := SetStatus(GdipGetLogFontW(NativeFont, vGraphics, LogFontW));
end;

function TsgGDIPlusFont.Clone: TsgGDIPlusFont;
var
  CloneFont: TsgGDIPFont;
begin
  CloneFont := nil;
  SetStatus(GdipCloneFont(NativeFont, CloneFont));
  Result := TsgGDIPlusFont.Create(CloneFont, FStatusOfFunction);
end;

destructor TsgGDIPlusFont.Destroy;
begin
  GdipDeleteFont(NativeFont);
end;

function TsgGDIPlusFont.IsAvailable: BOOL;
begin
  Result := (NativeFont <> nil);
end;

function TsgGDIPlusFont.GetStyle: Integer;
begin
  SetStatus(GdipGetFontStyle(NativeFont, Result));
end;

function TsgGDIPlusFont.GetSize: Single;
begin
  SetStatus(GdipGetFontSize(NativeFont, Result));
end;

function TsgGDIPlusFont.GetUnit: TsgGDIPUnit;
begin
  SetStatus(GdipGetFontUnit(NativeFont, Result));
end;

function TsgGDIPlusFont.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
end;

function TsgGDIPlusFont.GetHeight(Graphics: TsgGDIPlusGraphics): Single;
var
  vGraphics: TsgGDIPGraphics;
begin
  if Assigned(Graphics) then
    vGraphics := Graphics.NativeGraphics
  else
    vGraphics := nil;
  SetStatus(GdipGetFontHeight(NativeFont, vGraphics, Result));
end;

function TsgGDIPlusFont.GetHeight(Dpi: Single): Single;
begin
  SetStatus(GdipGetFontHeightGivenDPI(NativeFont, Dpi, Result));
end;

function TsgGDIPlusFont.GetFamily(Family: TsgGDIPlusFontFamily): TsgGDIPStatus;
var
  vStatus: TsgGDIPStatus;
  vFamily: TsgGDIPFontFamily;
begin
  if (Family = nil) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  vStatus := GdipGetFamily(NativeFont, vFamily);
  Family.NativeFamily := vFamily;
  Family.SetStatus(vStatus);
  Result := SetStatus(vStatus);
end;

constructor TsgGDIPlusFont.Create(Font: TsgGDIPFont; Status: TsgGDIPStatus);
begin
  FStatusOfFunction := Status;
  SetNativeFont(Font);
end;

procedure TsgGDIPlusFont.SetNativeFont(Font: TsgGDIPFont);
begin
  NativeFont := Font;
end;

function TsgGDIPlusFont.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

constructor TsgGDIPlusFontCollection.Create;
begin
  NativeFontCollection := nil;
end;

destructor TsgGDIPlusFontCollection.Destroy;
begin
  inherited Destroy;
end;

function TsgGDIPlusFontCollection.GetFamilyCount: Integer;
var
  NumFound: Integer;
begin
  NumFound := 0;
  FStatusOfFunction := GdipGetFontCollectionFamilyCount(NativeFontCollection, NumFound);
  Result := NumFound;
end;

function TsgGDIPlusFontCollection.GetFamilies(NumSought: Integer; out AFamilies: array of TsgGDIPlusFontFamily;
    out NumFound: Integer): TsgGDIPStatus;
var
  NativeFamilyList: Pointer;
  vStatus: TsgGDIPStatus;
  I: Integer;
type
  ArrGpFontFamily = array of TsgGDIPFontFamily;
begin
  if ((NumSought <= 0) or (Length(AFamilies) = 0)) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  NumFound := 0;
  GetMem(NativeFamilyList, NumSought * SizeOf(TsgGDIPFontFamily));
  if NativeFamilyList = nil then
  begin
    Result := SetStatus(OutOfMemory);
    Exit;
  end;
  vStatus := SetStatus(GdipGetFontCollectionFamilyList( NativeFontCollection, NumSought, NativeFamilyList, NumFound));
  if (vStatus = Ok) then
    for I := 0 to NumFound - 1 do
      GdipCloneFontFamily(ArrGpFontFamily(NativeFamilyList)[I], AFamilies[I].NativeFamily);
  FreeMem(NativeFamilyList, NumSought * SizeOf(TsgGDIPFontFamily));
  Result := vStatus;
end;

function TsgGDIPlusFontCollection.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
end;

function TsgGDIPlusFontCollection.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  FStatusOfFunction := Status;
  Result := FStatusOfFunction;
end;

constructor TsgGDIPlusInstalledFontCollection.Create;
begin
  NativeFontCollection := nil;
  FStatusOfFunction := GdipNewInstalledFontCollection(NativeFontCollection);
end;

destructor TsgGDIPlusInstalledFontCollection.Destroy;
begin
  inherited Destroy;
end;

constructor TsgGDIPlusPrivateFontCollection.Create;
begin
  NativeFontCollection := nil;
  FStatusOfFunction := GdipNewPrivateFontCollection(NativeFontCollection);
end;

destructor TsgGDIPlusPrivateFontCollection.Destroy;
begin
  GdipDeletePrivateFontCollection(NativeFontCollection);
  inherited Destroy;
end;

function TsgGDIPlusPrivateFontCollection.AddFontFile(FileName: WideString): TsgGDIPStatus;
begin
  Result := SetStatus(GdipPrivateAddFontFile(NativeFontCollection, PWideChar(FileName)));
end;

function TsgGDIPlusPrivateFontCollection.AddMemoryFont(Memory: Pointer; Length: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipPrivateAddMemoryFont(NativeFontCollection, Memory, Length));
end;

constructor TsgGDIPlusGraphicsPath.Create(FillMode: TsgGDIPFillMode = FillModeAlternate);
begin
  NativePath := nil;
  FStatusOfFunction := GdipCreatePath(FillMode, NativePath);
end;

constructor TsgGDIPlusGraphicsPath.Create(Points: PsgGPointF; Types: PBYTE; Count: Integer; FillMode: TsgGDIPFillMode = FillModeAlternate);
begin
  NativePath := nil;
  FStatusOfFunction := GdipCreatePath2(Points, Types, Count, FillMode, NativePath);
end;

constructor TsgGDIPlusGraphicsPath.Create(Points: PsgGPoint; Types: PBYTE; Count: Integer; FillMode: TsgGDIPFillMode = FillModeAlternate);
begin
  NativePath := nil;
  FStatusOfFunction := GdipCreatePath2I(Points, Types, Count, FillMode, NativePath);
end;

destructor TsgGDIPlusGraphicsPath.Destroy;
begin
  GdipDeletePath(NativePath);
end;

function TsgGDIPlusGraphicsPath.Clone: TsgGDIPlusGraphicsPath;
var
  ClonePath: TsgGDIPPath;
begin
  Clonepath := nil;
  SetStatus(GdipClonePath(NativePath, Clonepath));
  Result := TsgGDIPlusGraphicsPath.Create(Clonepath);
end;

function TsgGDIPlusGraphicsPath.Reset: TsgGDIPStatus;
begin
  Result := SetStatus(GdipResetPath(NativePath));
end;

function TsgGDIPlusGraphicsPath.GetFillMode: TsgGDIPFillMode;
var
  vMode: TsgGDIPFillMode;
begin
  vMode := FillModeAlternate;
  SetStatus(GdipGetPathFillMode(NativePath, Result));
  Result := vMode;
end;

function TsgGDIPlusGraphicsPath.SetFillMode(FillMode: TsgGDIPFillMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathFillMode(NativePath, FillMode));
end;

function TsgGDIPlusGraphicsPath.GetPathData(PathData: TsgGDIPPathData): TsgGDIPStatus;
var
  Count: Integer;
begin
  Count := GetPointCount;
  if ((Count <= 0) or ((pathData.Count > 0) and (pathData.Count < Count))) then
  begin
    PathData.Count := 0;
    if Assigned(pathData.Points) then
    begin
      FreeMem(pathData.Points);
      PathData.Points := nil;
    end;
    if Assigned(pathData.Types) then
    begin
      FreeMem(pathData.Types);
      PathData.Types := nil;
    end;
    if (Count <= 0) then
    begin
      Result := FStatusOfFunction;
      Exit;
    end;
  end;
  if (pathData.Count = 0) then
  begin
    GetMem(pathData.Points, SizeOf(TsgGPointF) * Count);
    if (pathData.Points = nil) then
    begin
      Result := SetStatus(OutOfMemory);
      Exit;
    end;
    Getmem(pathData.Types, Count);
    if (pathData.Types = nil) then
    begin
      FreeMem(pathData.Points);
      PathData.Points := nil;
      Result := SetStatus(OutOfMemory);
      Exit;
    end;
    PathData.Count := Count;
  end;
  Result := SetStatus(GdipGetPathData(NativePath, @pathData.Count));
end;

function TsgGDIPlusGraphicsPath.StartFigure: TsgGDIPStatus;
begin
  Result := SetStatus(GdipStartPathFigure(NativePath));
end;

function TsgGDIPlusGraphicsPath.CloseFigure: TsgGDIPStatus;
begin
  Result := SetStatus(GdipClosePathFigure(NativePath));
end;

function TsgGDIPlusGraphicsPath.CloseAllFigures: TsgGDIPStatus;
begin
  Result := SetStatus(GdipClosePathFigures(NativePath));
end;

function TsgGDIPlusGraphicsPath.SetMarker: TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathMarker(NativePath));
end;

function TsgGDIPlusGraphicsPath.ClearMarkers: TsgGDIPStatus;
begin
  Result := SetStatus(GdipClearPathMarkers(NativePath));
end;

function TsgGDIPlusGraphicsPath.Reverse: TsgGDIPStatus;
begin
  Result := SetStatus(GdipReversePath(NativePath));
end;

function TsgGDIPlusGraphicsPath.GetLastPoint(out LastPoint: TsgGPointF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathLastPoint(NativePath, @LastPoint));
end;

function TsgGDIPlusGraphicsPath.AddLine(const Pt1, Pt2: TsgGPointF): TsgGDIPStatus;
begin
  Result := AddLine(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TsgGDIPlusGraphicsPath.AddLine(X1, Y1, X2, Y2: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathLine(NativePath, X1, Y1, X2, Y2));
end;

function TsgGDIPlusGraphicsPath.AddLines(Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathLine2(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddLine(const Pt1, Pt2: TsgGPoint): TsgGDIPStatus;
begin
  Result := AddLine(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TsgGDIPlusGraphicsPath.AddLine(X1, Y1, X2, Y2: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathLineI(NativePath, X1, Y1, X2, Y2));
end;

function TsgGDIPlusGraphicsPath.AddLines(Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathLine2I(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddArc(Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := AddArc(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphicsPath.AddArc(X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathArc(NativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphicsPath.AddArc(Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := AddArc(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphicsPath.AddArc(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathArcI(NativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphicsPath.AddBezier(Pt1, Pt2, Pt3, Pt4: TsgGPointF): TsgGDIPStatus;
begin
  Result := AddBezier(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TsgGDIPlusGraphicsPath.AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathBezier(NativePath, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TsgGDIPlusGraphicsPath.AddBeziers(Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathBeziers(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddBezier(Pt1, Pt2, Pt3, Pt4: TsgGPoint): TsgGDIPStatus;
begin
  Result := AddBezier(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TsgGDIPlusGraphicsPath.AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathBezierI(NativePath, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TsgGDIPlusGraphicsPath.AddBeziers(Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathBeziersI(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddCurve(Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathCurve(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddCurve(Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathCurve2(NativePath, Points, Count, Tension));
end;

function TsgGDIPlusGraphicsPath.AddCurve(Points: PsgGPointF; Count, Offset, NumberOfSegments: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathCurve3(NativePath, Points, Count, Offset, NumberOfSegments, Tension));
end;

function TsgGDIPlusGraphicsPath.AddCurve(Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathCurveI(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddCurve(Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathCurve2I(NativePath, Points, Count, Tension));
end;

function TsgGDIPlusGraphicsPath.AddCurve(Points: PsgGPoint; Count, Offset, NumberOfSegments: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathCurve3I(NativePath, Points, Count, Offset, NumberOfSegments, Tension));
end;

function TsgGDIPlusGraphicsPath.AddClosedCurve(Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddClosedCurve(Points: PsgGPointF; Count: Integer; Tension: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2(NativePath, Points, Count, Tension));
end;

function TsgGDIPlusGraphicsPath.AddClosedCurve(Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurveI(NativePath, Points, Count));
end;


function TsgGDIPlusGraphicsPath.AddClosedCurve(Points: PsgGPoint; Count: Integer; Tension: Single): TsgGDIPStatus;
begin
     Result := SetStatus(GdipAddPathClosedCurve2I(NativePath, Points, Count, Tension));
end;

function TsgGDIPlusGraphicsPath.AddRectangle(Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathRectangle(NativePath, Rect.X, Rect.Y, Rect.Width, Rect.Height));
end;

function TsgGDIPlusGraphicsPath.AddRectangles(Rects: PsgGRectF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathRectangles(NativePath, Rects, Count));
end;

function TsgGDIPlusGraphicsPath.AddRectangle(Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathRectangleI(NativePath,Rect.X, Rect.Y, Rect.Width, Rect.Height));
end;

function TsgGDIPlusGraphicsPath.AddRectangles(Rects: PsgGRect; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathRectanglesI(NativePath, Rects, Count));
end;

function TsgGDIPlusGraphicsPath.AddEllipse(Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := AddEllipse(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphicsPath.AddEllipse(X, Y, Width, Height: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathEllipse(NativePath, X, Y, Width, Height));
end;

function TsgGDIPlusGraphicsPath.AddEllipse(Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := AddEllipse(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TsgGDIPlusGraphicsPath.AddEllipse(X, Y, Width, Height: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathEllipseI(NativePath, X, Y, Width, Height));
end;

function TsgGDIPlusGraphicsPath.AddPie(Rect: TsgGRectF; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := AddPie(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphicsPath.AddPie(X, Y, Width, Height, StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathPie(NativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphicsPath.AddPie(Rect: TsgGRect; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := AddPie(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TsgGDIPlusGraphicsPath.AddPie(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathPieI(NativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TsgGDIPlusGraphicsPath.AddPolygon(Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathPolygon(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddPolygon(Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipAddPathPolygonI(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.AddPath(AddingPath: TsgGDIPlusGraphicsPath; Connect: Bool): TsgGDIPStatus;
var
  NativePath2: TsgGDIPPath;
begin
  NativePath2 := nil;
  if Assigned(AddingPath) then
    NativePath2 := AddingPath.NativePath;
  Result := SetStatus(GdipAddPathPath(NativePath, NativePath2, Connect));
end;

function TsgGDIPlusGraphicsPath.AddString(String_: WideString; Length: Integer; Family: TsgGDIPlusFontFamily;
  Style: Integer; ASize: Single; AOrg: TsgGPointF; Format: TsgGDIPlusStringFormat): TsgGDIPStatus;
var
  Rect: TsgGRectF;
  vFontFamily: TsgGDIPFontFamily;
  vStringFormat: TsgGDIPFontCollection;
begin
  Rect.X := AOrg.X;
  Rect.Y := AOrg.Y;
  Rect.Width := 0.0;
  Rect.Height := 0.0;
  vFontFamily := nil;
  vStringFormat := nil;
  if Assigned(Family) then
    vFontFamily := Family.NativeFamily;
  if Assigned(Format) then
    vStringFormat := Format.NativeFormat;
  Result := SetStatus(GdipAddPathString(NativePath, PWideChar(String_), Length, vFontFamily, Style, ASize, @Rect, vStringFormat));
end;

function TsgGDIPlusGraphicsPath.AddString(String_: WideString; Length: Integer; Family: TsgGDIPlusFontFamily;
  Style: Integer;  ASize: Single; const ALayoutRect: TsgGRectF; Format: TsgGDIPlusStringFormat): TsgGDIPStatus;
var
  vFontFamily: TsgGDIPFontFamily;
  vStringFormat: TsgGDIPFontCollection;
begin
  vFontFamily := nil;
  vStringFormat := nil;
  if Assigned(Family) then
    vFontFamily := Family.NativeFamily;
  if Assigned(Format) then
    vStringFormat := Format.NativeFormat;
  Result := SetStatus(GdipAddPathString( NativePath, PWideChar(String_), Length, vFontFamily, Style, ASize, @ALayoutRect, vStringFormat));
end;

function TsgGDIPlusGraphicsPath.AddString(String_: WideString; Length: Integer; Family: TsgGDIPlusFontFamily; Style : Integer;
  ASize: Single; AOrg: TsgGPoint; Format: TsgGDIPlusStringFormat): TsgGDIPStatus;
var
  Rect: TsgGRect;
  vFontFamily: TsgGDIPFontFamily;
  vStringFormat: TsgGDIPFontCollection;
begin
  Rect.X := AOrg.X;
  Rect.Y := AOrg.Y;
  Rect.Width := 0;
  Rect.Height := 0;
  vFontFamily := nil;
  vStringFormat := nil;
  if Assigned(Family) then
    vFontFamily := Family.NativeFamily;
  if Assigned(Format) then
    vStringFormat := Format.NativeFormat;
  Result := SetStatus(GdipAddPathStringI(NativePath, PWideChar(String_), Length, vFontFamily, Style, ASize, @Rect, vStringFormat));
end;

function TsgGDIPlusGraphicsPath.AddString(String_: WideString; Length: Integer; Family: TsgGDIPlusFontFamily;
  Style: Integer; ASize: Single;const ALayoutRect: TsgGRect; Format: TsgGDIPlusStringFormat): TsgGDIPStatus;
var
  vFontFamily: TsgGDIPFontFamily;
  vStringFormat: TsgGDIPFontCollection;
begin
  vFontFamily := nil;
  vStringFormat := nil;
  if Assigned(Family) then
    vFontFamily := Family.NativeFamily;
  if Assigned(Format) then
    vStringFormat := Format.NativeFormat;
  Result := SetStatus(GdipAddPathStringI( NativePath, PWideChar(String_), Length, vFontFamily, Style, ASize, @ALayoutRect, vStringFormat));
end;

function TsgGDIPlusGraphicsPath.Transform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  if Assigned(AMatrix) then
    Result := SetStatus(GdipTransformPath(NativePath, AMatrix.NativeMatrix))
  else
    Result := Ok;
end;

function TsgGDIPlusGraphicsPath.GetBounds(out Bounds: TsgGRectF; const AMatrix: TsgGDIPlusMatrix = nil; Pen: TsgGDIPlusPen = nil): TsgGDIPStatus;
var
  NativeMatrix: TsgGDIPMatrix;
  NativePen: TsgGDIPPen;
begin
  NativeMatrix := nil;
  NativePen    := nil;
  if Assigned(AMatrix) then
    NativeMatrix := AMatrix.NativeMatrix;
  if Assigned(Pen) then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipGetPathWorldBounds(NativePath, @Bounds, NativeMatrix, NativePen));
end;

function TsgGDIPlusGraphicsPath.GetBounds(out Bounds: TsgGRect; const AMatrix: TsgGDIPlusMatrix = nil; Pen: TsgGDIPlusPen = nil): TsgGDIPStatus;
var
  NativeMatrix: TsgGDIPMatrix;
  NativePen: TsgGDIPPen;
begin
  NativeMatrix := nil;
  NativePen    := nil;
  if Assigned(AMatrix) then
    NativeMatrix := AMatrix.NativeMatrix;
  if Assigned(Pen) then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipGetPathWorldBoundsI(NativePath, @Bounds, NativeMatrix, NativePen));
end;

function TsgGDIPlusGraphicsPath.Flatten(const AMatrix: TsgGDIPlusMatrix = nil; Flatness: Single = FlatnessDefault): TsgGDIPStatus;
var
  NativeMatrix: TsgGDIPMatrix;
begin
  NativeMatrix := nil;
  if Assigned(AMatrix) then
    NativeMatrix := AMatrix.NativeMatrix;
  Result := SetStatus(GdipFlattenPath(NativePath, NativeMatrix, Flatness));
end;

function TsgGDIPlusGraphicsPath.Widen(Pen: TsgGDIPlusPen; const AMatrix: TsgGDIPlusMatrix = nil; Flatness: Single = FlatnessDefault): TsgGDIPStatus;
var
  NativeMatrix: TsgGDIPMatrix;
begin
  NativeMatrix := nil;
  if Assigned(AMatrix) then
    NativeMatrix := AMatrix.NativeMatrix;
  Result := SetStatus(GdipWidenPath(NativePath, Pen.NativePen, NativeMatrix, Flatness));
end;

function TsgGDIPlusGraphicsPath.Outline(const AMatrix: TsgGDIPlusMatrix = nil; Flatness: Single = FlatnessDefault): TsgGDIPStatus;
var
  NativeMatrix: TsgGDIPMatrix;
begin
  NativeMatrix := nil;
  if Assigned(AMatrix) then
    NativeMatrix := AMatrix.NativeMatrix;
  Result := SetStatus(GdipWindingModeOutline(NativePath, NativeMatrix, Flatness));
end;

function TsgGDIPlusGraphicsPath.Warp(DestPoints: PsgGPointF; Count: Integer; SrcRect: TsgGRectF;
  const AMatrix: TsgGDIPlusMatrix = nil; AWarpMode: TWarpMode = WarpModePerspective;  Flatness: Single = FlatnessDefault): TsgGDIPStatus;
var
  NativeMatrix: TsgGDIPMatrix;
begin
  NativeMatrix := nil;
  if Assigned(AMatrix) then
    NativeMatrix := AMatrix.NativeMatrix;
  Result := SetStatus(GdipWarpPath(NativePath, NativeMatrix, DestPoints, Count, SrcRect.X, SrcRect.Y, SrcRect.Width, SrcRect.Height,
    AWarpMode, Flatness));
end;

function TsgGDIPlusGraphicsPath.GetPointCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPointCount(NativePath, Count));
  Result := Count;
end;

function TsgGDIPlusGraphicsPath.GetPathTypes(Types: PBYTE; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathTypes(NativePath, Types, Count));
end;

function TsgGDIPlusGraphicsPath.GetPathPoints(Points: PsgGPointF; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathPoints(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.GetPathPoints(Points: PsgGPoint; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathPointsI(NativePath, Points, Count));
end;

function TsgGDIPlusGraphicsPath.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

function TsgGDIPlusGraphicsPath.IsVisible(Point: TsgGPointF; Graph: TsgGDIPlusGraphics = nil): BOOL;
begin
  Result := IsVisible(Point.X, Point.Y, Graph);
end;

function TsgGDIPlusGraphicsPath.IsVisible(X, Y: Single; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  NativeGraphics: TsgGDIPGraphics;
begin
  vBoolVal := False;
  NativeGraphics := nil;
  if Assigned(Graph) then
    NativeGraphics := Graph.NativeGraphics;
  SetStatus(GdipIsVisiblePathPoint(NativePath, X, Y, NativeGraphics, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphicsPath.IsVisible(Point: TsgGPoint; Graph: TsgGDIPlusGraphics = nil): BOOL;
begin
  Result := IsVisible(Point.X, Point.Y, Graph);
end;

function TsgGDIPlusGraphicsPath.IsVisible(X, Y: Integer; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  NativeGraphics: TsgGDIPGraphics;
begin
  vBoolVal := False;
  NativeGraphics := nil;
  if Assigned(Graph) then
    NativeGraphics := Graph.NativeGraphics;
  SetStatus(GdipIsVisiblePathPointI(NativePath, X, Y, NativeGraphics, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphicsPath.IsOutlineVisible(Point: TsgGPointF; Pen: TsgGDIPlusPen; Graph: TsgGDIPlusGraphics = nil): BOOL;
begin
  Result := IsOutlineVisible(Point.X, Point.Y, Pen, Graph);
end;

function TsgGDIPlusGraphicsPath.IsOutlineVisible(X, Y: Single; Pen: TsgGDIPlusPen; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  NativeGraphics: TsgGDIPGraphics;
  NativePen: TsgGDIPPen;
begin
  vBoolVal := False;
  NativeGraphics := nil;
  NativePen := nil;
  if Assigned(Graph) then
   NativeGraphics := Graph.NativeGraphics;
  if Assigned(Pen) then
   NativePen := Pen.NativePen;
  SetStatus(GdipIsOutlineVisiblePathPoint(NativePath, X, Y, NativePen, NativeGraphics, vBoolVal));
  Result := vBoolVal;
end;

function TsgGDIPlusGraphicsPath.IsOutlineVisible(Point: TsgGPoint; Pen: TsgGDIPlusPen; Graph: TsgGDIPlusGraphics = nil): BOOL;
begin
  Result := IsOutlineVisible(Point.X, Point.Y, Pen, Graph);
end;

function TsgGDIPlusGraphicsPath.IsOutlineVisible(X, Y: Integer; Pen: TsgGDIPlusPen; Graph: TsgGDIPlusGraphics = nil): BOOL;
var
  vBoolVal: BOOL;
  NativeGraphics: TsgGDIPGraphics;
  NativePen: TsgGDIPPen;
begin
  vBoolVal := False;
  NativeGraphics := nil;
  NativePen := nil;
  if Assigned(Graph) then
    NativeGraphics := Graph.NativeGraphics;
  if Assigned(Pen) then
    NativePen := Pen.NativePen;
  SetStatus(GdipIsOutlineVisiblePathPointI(NativePath, X, Y, NativePen, NativeGraphics, vBoolVal));
  Result := vBoolVal;
end;

constructor TsgGDIPlusGraphicsPath.Create(Path: TsgGDIPlusGraphicsPath);
var
  ClonePath: TsgGDIPPath;
begin
  Clonepath := nil;
  SetStatus(GdipClonePath(path.NativePath, Clonepath));
  SetNativePath(Clonepath);
end;

constructor TsgGDIPlusGraphicsPath.Create(NativePath: TsgGDIPPath);
begin
  FStatusOfFunction := Ok;
  SetNativePath(NativePath);
end;

procedure TsgGDIPlusGraphicsPath.SetNativePath(NativePath: TsgGDIPPath);
begin
  Self.NativePath := NativePath;
end;

function TsgGDIPlusGraphicsPath.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

constructor TsgGDIPlusGraphicsPathIterator.Create(Path: TsgGDIPlusGraphicsPath);
var
  NativePath: TsgGDIPPath;
  vIterator: TsgGDIPPathIterator;
begin
  NativePath := nil;
  if Assigned(path) then
    NativePath := Path.NativePath;
  vIterator := nil;
  FStatusOfFunction := GdipCreatePathIter(vIterator, NativePath); SetNativeIterator(vIterator);
end;

destructor TsgGDIPlusGraphicsPathIterator.Destroy;
begin
  GdipDeletePathIter(NativeIterator);
end;


function TsgGDIPlusGraphicsPathIterator.NextSubpath(out StartIndex, EndIndex: Integer; out IsClosed: bool): Integer;
begin
  SetStatus(GdipPathIterNextSubpath(NativeIterator, Result, StartIndex, EndIndex, IsClosed));
end;

function TsgGDIPlusGraphicsPathIterator.NextSubpath(Path: TsgGDIPlusGraphicsPath; out IsClosed: BOOL): Integer;
var
  NativePath: TsgGDIPPath;
  ResultCount: Integer;
begin
  NativePath := nil;
  if Assigned(path) then
    NativePath := Path.NativePath;
  SetStatus(GdipPathIterNextSubpathPath(NativeIterator, ResultCount, NativePath, IsClosed));
  Result := ResultCount;
end;

function TsgGDIPlusGraphicsPathIterator.NextPathType(out PathType: TPathPointType; out StartIndex, EndIndex: Integer): Integer;
var
  ResultCount: Integer;
begin
  SetStatus(GdipPathIterNextPathType(NativeIterator, ResultCount, @PathType, StartIndex, EndIndex));
  Result := ResultCount;
end;

function TsgGDIPlusGraphicsPathIterator.NextMarker(out StartIndex, EndIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterNextMarker(NativeIterator, Result, StartIndex, EndIndex));
end;

function TsgGDIPlusGraphicsPathIterator.NextMarker(Path: TsgGDIPlusGraphicsPath): Integer;
var
  NativePath: TsgGDIPPath;
begin
  NativePath := nil;
  if Assigned(path) then
    NativePath := Path.NativePath;
  SetStatus(GdipPathIterNextMarkerPath(NativeIterator, Result, NativePath));
end;

function TsgGDIPlusGraphicsPathIterator.GetCount: Integer;
begin
  SetStatus(GdipPathIterGetCount(NativeIterator, Result));
end;

function TsgGDIPlusGraphicsPathIterator.GetSubpathCount: Integer;
begin
  SetStatus(GdipPathIterGetSubpathCount(NativeIterator, Result));
end;

function TsgGDIPlusGraphicsPathIterator.HasCurve: BOOL;
begin
  SetStatus(GdipPathIterHasCurve(NativeIterator, Result));
end;

procedure TsgGDIPlusGraphicsPathIterator.Rewind;
begin
  SetStatus(GdipPathIterRewind(NativeIterator));
end;

function TsgGDIPlusGraphicsPathIterator.ENumerate(Points: PsgGPointF; Types: PBYTE; Count: Integer): Integer;
begin
  SetStatus(GdipPathIterENumerate(NativeIterator, Result, Points, Types, Count));
end;

function TsgGDIPlusGraphicsPathIterator.CopyData(Points: PsgGPointF; Types: PBYTE; StartIndex, EndIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterCopyData(NativeIterator, Result, Points, Types, StartIndex, EndIndex));
end;

function TsgGDIPlusGraphicsPathIterator.GetStatusOfFunction: TsgGDIPStatus;
begin
  Result := FStatusOfFunction;
  FStatusOfFunction := Ok;
end;

procedure TsgGDIPlusGraphicsPathIterator.SetNativeIterator(NativeIterator: TsgGDIPPathIterator);
begin
  Self.NativeIterator := NativeIterator;
end;

function TsgGDIPlusGraphicsPathIterator.SetStatus(Status: TsgGDIPStatus): TsgGDIPStatus;
begin
  if (Status <> Ok) then
    FStatusOfFunction := Status;
  Result := Status;
end;

constructor TsgGDIPlusPathGradientBrush.Create(Points: PsgGPointF; Count: Integer; WrapMode: TsgGDIPWrapMode = WrapModeClamp);
var
  Brush: TsgGDIPPathGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreatePathGradient(Points, Count, WrapMode, Brush);
  SetNativeBrush(Brush);
end;

constructor TsgGDIPlusPathGradientBrush.Create(Points: PsgGPoint; Count: Integer; WrapMode: TsgGDIPWrapMode = WrapModeClamp);
var
  Brush: TsgGDIPPathGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreatePathGradientI(Points, Count, WrapMode, Brush);
  SetNativeBrush(Brush);
end;

constructor TsgGDIPlusPathGradientBrush.Create(Path: TsgGDIPlusGraphicsPath);
var
  Brush: TsgGDIPPathGradient;
begin
  Brush := nil;
  FStatusOfFunction := GdipCreatePathGradientFromPath(path.NativePath, Brush);
  SetNativeBrush(Brush);
end;

function TsgGDIPlusPathGradientBrush.GetCenterColor(out Color: TsgColor): TsgGDIPStatus;
begin
  SetStatus(GdipGetPathGradientCenterColor(TsgGDIPPathGradient(NativeBrush), Color));
  Result := FStatusOfFunction;
end;

function TsgGDIPlusPathGradientBrush.SetCenterColor(Color: TsgColor): TsgGDIPStatus;
begin
  SetStatus(GdipSetPathGradientCenterColor(TsgGDIPPathGradient(NativeBrush),Color));
  Result := FStatusOfFunction;
end;

function TsgGDIPlusPathGradientBrush.GetPointCount: Integer;
begin
  SetStatus(GdipGetPathGradientPointCount(TsgGDIPPathGradient(NativeBrush), Result));
end;

function TsgGDIPlusPathGradientBrush.GetSurroundColorCount: Integer;
begin
  SetStatus(GdipGetPathGradientSurroundColorCount(TsgGDIPPathGradient(NativeBrush), Result));
end;

function TsgGDIPlusPathGradientBrush.GetSurroundColors(Colors: PARGB; var Count: Integer): TsgGDIPStatus;
var
  Count1: Integer;
begin
  if not Assigned(Colors) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  SetStatus(GdipGetPathGradientSurroundColorCount(TsgGDIPPathGradient(NativeBrush), Count1));
  if(FStatusOfFunction <> Ok) then
  begin
    Result := FStatusOfFunction;
    Exit;
  end;
  if((Count < Count1) or (Count1 <= 0)) then
  begin
    Result := SetStatus(InsufficientBuffer);
    Exit;
  end;
  SetStatus(GdipGetPathGradientSurroundColorsWithCount(TsgGDIPPathGradient(NativeBrush), Colors, Count1));
  if (FStatusOfFunction = Ok) then
    Count := Count1;
  Result := FStatusOfFunction;
end;

function TsgGDIPlusPathGradientBrush.SetSurroundColors(Colors: PARGB; var Count: Integer): TsgGDIPStatus;
type
  TDynArrDWORD = array of DWORD;
var
  Count1: Integer;
begin
  if (Colors = nil) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  Count1 := GetPointCount;
  if((Count > Count1) or (Count1 <= 0)) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  Count1 := Count;
  SetStatus(GdipSetPathGradientSurroundColorsWithCount(TsgGDIPPathGradient(NativeBrush), Colors, Count1));
  if (FStatusOfFunction = Ok) then
    Count := Count1;
  Result := FStatusOfFunction;
end;

function TsgGDIPlusPathGradientBrush.GetGraphicsPath(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
begin
  if(path = nil) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  Result := SetStatus(GdipGetPathGradientPath(TsgGDIPPathGradient(NativeBrush), Path.NativePath));
end;

function TsgGDIPlusPathGradientBrush.SetGraphicsPath(Path: TsgGDIPlusGraphicsPath): TsgGDIPStatus;
begin
  if(path = nil) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  Result := SetStatus(GdipSetPathGradientPath(TsgGDIPPathGradient(NativeBrush), Path.NativePath));
end;

function TsgGDIPlusPathGradientBrush.GetCenterPoint(out Point: TsgGPointF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPoint(TsgGDIPPathGradient(NativeBrush), @Point));
end;

function TsgGDIPlusPathGradientBrush.GetCenterPoint(out Point: TsgGPoint): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPointI(TsgGDIPPathGradient(NativeBrush), @Point));
end;

function TsgGDIPlusPathGradientBrush.SetCenterPoint(Point: TsgGPointF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPoint(TsgGDIPPathGradient(NativeBrush), @Point));
end;

function TsgGDIPlusPathGradientBrush.SetCenterPoint(Point: TsgGPoint): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPointI(TsgGDIPPathGradient(NativeBrush), @Point));
end;

function TsgGDIPlusPathGradientBrush.GetRectangle(out Rect: TsgGRectF): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathGradientRect(TsgGDIPPathGradient(NativeBrush), @Rect));
end;

function TsgGDIPlusPathGradientBrush.GetRectangle(out Rect: TsgGRect): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathGradientRectI(TsgGDIPPathGradient(NativeBrush), @Rect));
end;

function TsgGDIPlusPathGradientBrush.SetGammaCorRection(UseGammaCorRection: BOOL): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientGammaCorRection(TsgGDIPPathGradient(NativeBrush), UseGammaCorRection));
end;

function TsgGDIPlusPathGradientBrush.GetGammaCorRection: BOOL;
begin
  SetStatus(GdipGetPathGradientGammaCorRection(TsgGDIPPathGradient(NativeBrush), Result));
end;

function TsgGDIPlusPathGradientBrush.GetBlendCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPathGradientBlendCount(TsgGDIPPathGradient(NativeBrush), Count));
  Result := Count;
end;

function TsgGDIPlusPathGradientBrush.GetBlend(BlendFactors, BlendPositions:PSingle; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathGradientBlend(TsgGDIPPathGradient(NativeBrush), BlendFactors, BlendPositions, Count));
end;

function TsgGDIPlusPathGradientBrush.SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientBlend(TsgGDIPPathGradient(NativeBrush), BlendFactors, BlendPositions, Count));
end;

function TsgGDIPlusPathGradientBrush.GetInterpolationColorCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPathGradientPresetBlendCount(TsgGDIPPathGradient(NativeBrush), Count));
  Result := Count;
end;

function TsgGDIPlusPathGradientBrush.SetInterpolationColors(APresetColors: PARGB; BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
var
  Status: TsgGDIPStatus;
begin
  if ((Count <= 0) or (APresetColors = nil)) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  Status := SetStatus(GdipSetPathGradientPresetBlend(TsgGDIPPathGradient(NativeBrush), APresetColors, BlendPositions, Count));
  Result := Status;
end;

function TsgGDIPlusPathGradientBrush.GetInterpolationColors(APresetColors: PARGB;
  BlendPositions: PSingle; Count: Integer): TsgGDIPStatus;
var
  Status: TsgGDIPStatus;
  I: Integer;
  ARGBs: PARGB;
begin
  if ((Count <= 0) or (APresetColors = nil)) then
  begin
    Result := SetStatus(InValidParameter);
    Exit;
  end;
  GetMem(ARGBs, Count*SizeOf(ARGB));
  if (ARGBs = nil) then
  begin
    Result := SetStatus(OutOfMemory);
    Exit;
  end;
  Status := SetStatus(GdipGetPathGradientPresetBlend(NativeBrush, ARGBs,  BlendPositions, Count));
  for I := 0 to Count - 1 do
    TColorDynArray(APresetColors)[I] := TColorDynArray(ARGBs)[I];
  FreeMem(ARGBs);
  Result := Status;
end;

function TsgGDIPlusPathGradientBrush.SetBlendBellShape(Focus: Single; Scale: Single = 1.0): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientSigmaBlend(TsgGDIPPathGradient(NativeBrush), Focus, Scale));
end;

function TsgGDIPlusPathGradientBrush.SetBlendTriangularShape(Focus: Single; Scale: Single = 1.0): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientLinearBlend(TsgGDIPPathGradient(NativeBrush), Focus, Scale));
end;

function TsgGDIPlusPathGradientBrush.GetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathGradientTransform(TsgGDIPPathGradient(NativeBrush),  AMatrix.NativeMatrix));
end;

function TsgGDIPlusPathGradientBrush.SetTransform(const AMatrix: TsgGDIPlusMatrix): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientTransform(TsgGDIPPathGradient(NativeBrush),  AMatrix.NativeMatrix));
end;

function TsgGDIPlusPathGradientBrush.ResetTransform: TsgGDIPStatus;
begin
  Result := SetStatus(GdipResetPathGradientTransform(TsgGDIPPathGradient(NativeBrush)));
end;

function TsgGDIPlusPathGradientBrush.MultiplyTransform(const AMatrix: TsgGDIPlusMatrix; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipMultiplyPathGradientTransform(TsgGDIPPathGradient(NativeBrush), AMatrix.NativeMatrix, Order));
end;

function TsgGDIPlusPathGradientBrush.TranslateTransform(Dx, Dy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipTranslatePathGradientTransform(TsgGDIPPathGradient(NativeBrush), Dx, Dy, Order));
end;

function TsgGDIPlusPathGradientBrush.ScaleTransform(Sx, Sy: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipScalePathGradientTransform(TsgGDIPPathGradient(NativeBrush), Sx, Sy, Order));
end;

function TsgGDIPlusPathGradientBrush.RotateTransform(Angle: Single; Order: TsgGDIPMatrixOrder = MatrixOrderPrePend): TsgGDIPStatus;
begin
  Result := SetStatus(GdipRotatePathGradientTransform(TsgGDIPPathGradient(NativeBrush),Angle, Order));
end;

function TsgGDIPlusPathGradientBrush.GetFocusScales(out XScale, YScale: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipGetPathGradientFocusScales(TsgGDIPPathGradient(NativeBrush), XScale, YScale));
end;

function TsgGDIPlusPathGradientBrush.SetFocusScales(XScale, YScale: Single): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientFocusScales(TsgGDIPPathGradient(NativeBrush), XScale, YScale));
end;

function TsgGDIPlusPathGradientBrush.GetWrapMode: TsgGDIPWrapMode;
begin
  SetStatus(GdipGetPathGradientWrapMode(TsgGDIPPathGradient(NativeBrush), Result));
end;

function TsgGDIPlusPathGradientBrush.SetWrapMode(WrapMode: TsgGDIPWrapMode): TsgGDIPStatus;
begin
  Result := SetStatus(GdipSetPathGradientWrapMode(TsgGDIPPathGradient(NativeBrush), WrapMode));
end;

constructor TsgGDIPlusPathGradientBrush.Create;
begin
end;

{* End classes of GDIPlus ******************************************************}

procedure FreeGDIPlus;
begin
  FreeLibrary(GDIPlusHnd);
  GDIPlusHnd := 0;
end;

procedure InitGDIPlus;

  procedure Init;
  begin
    GDIPAlloc := GetProcAddress(GDIPlusHnd, 'GdipAlloc');
    GDIPFree := GetProcAddress(GDIPlusHnd, 'GdipFree');
    GDIPlusStartup := GetProcAddress(GDIPlusHnd, 'GdiplusStartup');
    GDIPlusShutdown := GetProcAddress(GDIPlusHnd, 'GdiplusShutdown');

    GDIPCreatePath := GetProcAddress(GDIPlusHnd, 'GdipCreatePath');
    GDIPCreatePath2 := GetProcAddress(GDIPlusHnd, 'GdipCreatePath2');
    GDIPCreatePath2I := GetProcAddress(GDIPlusHnd, 'GdipCreatePath2I');
    GDIPClonePath := GetProcAddress(GDIPlusHnd, 'GdipClonePath');
    GDIPDeletePath := GetProcAddress(GDIPlusHnd, 'GdipDeletePath');
    GDIPResetPath := GetProcAddress(GDIPlusHnd, 'GdipResetPath');
    GDIPGetPointCount := GetProcAddress(GDIPlusHnd, 'GdipGetPointCount');
    GDIPGetPathTypes := GetProcAddress(GDIPlusHnd, 'GdipGetPathTypes');
    GDIPGetPathPoints := GetProcAddress(GDIPlusHnd, 'GdipGetPathPoints');
    GDIPGetPathPointsI := GetProcAddress(GDIPlusHnd, 'GdipGetPathPointsI');
    GDIPGetPathFillMode := GetProcAddress(GDIPlusHnd, 'GdipGetPathFillMode');
    GDIPSetPathFillMode := GetProcAddress(GDIPlusHnd, 'GdipSetPathFillMode');
    GDIPGetPathData := GetProcAddress(GDIPlusHnd, 'GdipGetPathData');
    GDIPStartPathFigure := GetProcAddress(GDIPlusHnd, 'GdipStartPathFigure');
    GDIPClosePathFigure := GetProcAddress(GDIPlusHnd, 'GdipClosePathFigure');
    GDIPClosePathFigures := GetProcAddress(GDIPlusHnd, 'GdipClosePathFigures');
    GDIPSetPathMarker := GetProcAddress(GDIPlusHnd, 'GdipSetPathMarker');
    GDIPClearPathMarkers := GetProcAddress(GDIPlusHnd, 'GdipClearPathMarkers');
    GDIPReversePath := GetProcAddress(GDIPlusHnd, 'GdipReversePath');
    GDIPGetPathLastPoint := GetProcAddress(GDIPlusHnd, 'GdipGetPathLastPoint');
    GDIPAddPathLine := GetProcAddress(GDIPlusHnd, 'GdipAddPathLine');
    GDIPAddPathLine2 := GetProcAddress(GDIPlusHnd, 'GdipAddPathLine2');
    GDIPAddPathArc := GetProcAddress(GDIPlusHnd, 'GdipAddPathArc');
    GDIPAddPathBezier := GetProcAddress(GDIPlusHnd, 'GdipAddPathBezier');
    GDIPAddPathBeziers := GetProcAddress(GDIPlusHnd, 'GdipAddPathBeziers');
    GDIPAddPathCurve := GetProcAddress(GDIPlusHnd, 'GdipAddPathCurve');
    GDIPAddPathCurve2 := GetProcAddress(GDIPlusHnd, 'GdipAddPathCurve2');
    GDIPAddPathCurve3 := GetProcAddress(GDIPlusHnd, 'GdipAddPathCurve3');
    GDIPAddPathClosedCurve := GetProcAddress(GDIPlusHnd, 'GdipAddPathClosedCurve');
    GDIPAddPathClosedCurve2 := GetProcAddress(GDIPlusHnd, 'GdipAddPathClosedCurve2');
    GDIPAddPathRectangle := GetProcAddress(GDIPlusHnd, 'GdipAddPathRectangle');
    GDIPAddPathRectangles := GetProcAddress(GDIPlusHnd, 'GdipAddPathRectangles');
    GDIPAddPathEllipse := GetProcAddress(GDIPlusHnd, 'GdipAddPathEllipse');
    GDIPAddPathPie := GetProcAddress(GDIPlusHnd, 'GdipAddPathPie');
    GDIPAddPathPolygon := GetProcAddress(GDIPlusHnd, 'GdipAddPathPolygon');
    GDIPAddPathPath := GetProcAddress(GDIPlusHnd, 'GdipAddPathPath');
    GDIPAddPathString := GetProcAddress(GDIPlusHnd, 'GdipAddPathString');
    GDIPAddPathStringI := GetProcAddress(GDIPlusHnd, 'GdipAddPathStringI');
    GDIPAddPathLineI := GetProcAddress(GDIPlusHnd, 'GdipAddPathLineI');
    GDIPAddPathLine2I := GetProcAddress(GDIPlusHnd, 'GdipAddPathLine2I');
    GDIPAddPathArcI := GetProcAddress(GDIPlusHnd, 'GdipAddPathArcI');
    GDIPAddPathBezierI := GetProcAddress(GDIPlusHnd, 'GdipAddPathBezierI');
    GDIPAddPathBeziersI := GetProcAddress(GDIPlusHnd, 'GdipAddPathBeziersI');
    GDIPAddPathCurveI := GetProcAddress(GDIPlusHnd, 'GdipAddPathCurveI');
    GDIPAddPathCurve2I := GetProcAddress(GDIPlusHnd, 'GdipAddPathCurve2I');
    GDIPAddPathCurve3I := GetProcAddress(GDIPlusHnd, 'GdipAddPathCurve3I');
    GDIPAddPathClosedCurveI := GetProcAddress(GDIPlusHnd, 'GdipAddPathClosedCurveI');
    GDIPAddPathClosedCurve2I := GetProcAddress(GDIPlusHnd, 'GdipAddPathClosedCurve2I');
    GDIPAddPathRectangleI := GetProcAddress(GDIPlusHnd, 'GdipAddPathRectangleI');
    GDIPAddPathRectanglesI := GetProcAddress(GDIPlusHnd, 'GdipAddPathRectanglesI');
    GDIPAddPathEllipseI := GetProcAddress(GDIPlusHnd, 'GdipAddPathEllipseI');
    GDIPAddPathPieI := GetProcAddress(GDIPlusHnd, 'GdipAddPathPieI');
    GDIPAddPathPolygonI := GetProcAddress(GDIPlusHnd, 'GdipAddPathPolygonI');
    GDIPFlattenPath := GetProcAddress(GDIPlusHnd, 'GdipFlattenPath');
    GDIPWindingModeOutline := GetProcAddress(GDIPlusHnd, 'GdipWindingModeOutline');
    GDIPWidenPath := GetProcAddress(GDIPlusHnd, 'GdipWidenPath');
    GDIPWarpPath := GetProcAddress(GDIPlusHnd, 'GdipWarpPath');
    GDIPTransformPath := GetProcAddress(GDIPlusHnd, 'GdipTransformPath');
    GDIPGetPathWorldBounds := GetProcAddress(GDIPlusHnd, 'GdipGetPathWorldBounds');
    GDIPGetPathWorldBoundsI := GetProcAddress(GDIPlusHnd, 'GdipGetPathWorldBoundsI');
    GDIPIsVisiblePathPoint := GetProcAddress(GDIPlusHnd, 'GdipIsVisiblePathPoint');
    GDIPIsVisiblePathPointI := GetProcAddress(GDIPlusHnd, 'GdipIsVisiblePathPointI');
    GDIPIsOutlineVisiblePathPoint := GetProcAddress(GDIPlusHnd, 'GdipIsOutlineVisiblePathPoint');
    GDIPIsOutlineVisiblePathPointI := GetProcAddress(GDIPlusHnd, 'GdipIsOutlineVisiblePathPointI');
    GDIPCreatePathIter := GetProcAddress(GDIPlusHnd, 'GdipCreatePathIter');
    GDIPDeletePathIter := GetProcAddress(GDIPlusHnd, 'GdipDeletePathIter');
    GDIPPathIterNextSubpath := GetProcAddress(GDIPlusHnd, 'GdipPathIterNextSubpath');
    GDIPPathIterNextSubpathPath := GetProcAddress(GDIPlusHnd, 'GdipPathIterNextSubpathPath');
    GDIPPathIterNextPathType := GetProcAddress(GDIPlusHnd, 'GdipPathIterNextPathType');
    GDIPPathIterNextMarker := GetProcAddress(GDIPlusHnd, 'GdipPathIterNextMarker');
    GDIPPathIterNextMarkerPath := GetProcAddress(GDIPlusHnd, 'GdipPathIterNextMarkerPath');
    GDIPPathIterGetCount := GetProcAddress(GDIPlusHnd, 'GdipPathIterGetCount');
    GDIPPathIterGetSubpathCount := GetProcAddress(GDIPlusHnd, 'GdipPathIterGetSubpathCount');
    GDIPPathIterIsValid := GetProcAddress(GDIPlusHnd, 'GdipPathIterIsValid');
    GDIPPathIterHasCurve := GetProcAddress(GDIPlusHnd, 'GdipPathIterHasCurve');
    GDIPPathIterRewind := GetProcAddress(GDIPlusHnd, 'GdipPathIterRewind');
    GDIPPathIterEnumerate := GetProcAddress(GDIPlusHnd, 'GdipPathIterEnumerate');
    GDIPPathIterCopyData := GetProcAddress(GDIPlusHnd, 'GdipPathIterCopyData');
    GDIPCreateMatrix := GetProcAddress(GDIPlusHnd, 'GdipCreateMatrix');
    GDIPCreateMatrix2 := GetProcAddress(GDIPlusHnd, 'GdipCreateMatrix2');
    GDIPCreateMatrix3 := GetProcAddress(GDIPlusHnd, 'GdipCreateMatrix3');
    GDIPCreateMatrix3I := GetProcAddress(GDIPlusHnd, 'GdipCreateMatrix3I');
    GDIPCloneMatrix := GetProcAddress(GDIPlusHnd, 'GdipCloneMatrix');
    GDIPDeleteMatrix := GetProcAddress(GDIPlusHnd, 'GdipDeleteMatrix');
    GDIPSetMatrixElements := GetProcAddress(GDIPlusHnd, 'GdipSetMatrixElements');
    GDIPMultiplyMatrix := GetProcAddress(GDIPlusHnd, 'GdipMultiplyMatrix');
    GDIPTranslateMatrix := GetProcAddress(GDIPlusHnd, 'GdipTranslateMatrix');
    GDIPScaleMatrix := GetProcAddress(GDIPlusHnd, 'GdipScaleMatrix');
    GDIPRotateMatrix := GetProcAddress(GDIPlusHnd, 'GdipRotateMatrix');
    GDIPShearMatrix := GetProcAddress(GDIPlusHnd, 'GdipShearMatrix');
    GDIPInvertMatrix := GetProcAddress(GDIPlusHnd, 'GdipInvertMatrix');
    GDIPTransformMatrixPoints := GetProcAddress(GDIPlusHnd, 'GdipTransformMatrixPoints');
    GDIPTransformMatrixPointsI := GetProcAddress(GDIPlusHnd, 'GdipTransformMatrixPointsI');
    GDIPVectorTransformMatrixPoints := GetProcAddress(GDIPlusHnd, 'GdipVectorTransformMatrixPoints');
    GDIPVectorTransformMatrixPointsI := GetProcAddress(GDIPlusHnd, 'GdipVectorTransformMatrixPointsI');
    GDIPGetMatrixElements := GetProcAddress(GDIPlusHnd, 'GdipGetMatrixElements');
    GDIPIsMatrixInvertible := GetProcAddress(GDIPlusHnd, 'GdipIsMatrixInvertible');
    GDIPIsMatrixIdentity := GetProcAddress(GDIPlusHnd, 'GdipIsMatrixIdentity');
    GDIPIsMatrixEqual := GetProcAddress(GDIPlusHnd, 'GdipIsMatrixEqual');
    GDIPCreateRegion := GetProcAddress(GDIPlusHnd, 'GdipCreateRegion');
    GDIPCreateRegionRect := GetProcAddress(GDIPlusHnd, 'GdipCreateRegionRect');
    GDIPCreateRegionRectI := GetProcAddress(GDIPlusHnd, 'GdipCreateRegionRectI');
    GDIPCreateRegionPath := GetProcAddress(GDIPlusHnd, 'GdipCreateRegionPath');
    GDIPCreateRegionRgnData := GetProcAddress(GDIPlusHnd, 'GdipCreateRegionRgnData');
    GDIPCreateRegionHrgn := GetProcAddress(GDIPlusHnd, 'GdipCreateRegionHrgn');
    GDIPCloneRegion := GetProcAddress(GDIPlusHnd, 'GdipCloneRegion');
    GDIPDeleteRegion := GetProcAddress(GDIPlusHnd, 'GdipDeleteRegion');
    GDIPSetInfinite := GetProcAddress(GDIPlusHnd, 'GdipSetInfinite');
    GDIPSetEmpty := GetProcAddress(GDIPlusHnd, 'GdipSetEmpty');
    GDIPCombineRegionRect := GetProcAddress(GDIPlusHnd, 'GdipCombineRegionRect');
    GDIPCombineRegionRectI := GetProcAddress(GDIPlusHnd, 'GdipCombineRegionRectI');
    GDIPCombineRegionPath := GetProcAddress(GDIPlusHnd, 'GdipCombineRegionPath');
    GDIPCombineRegionRegion := GetProcAddress(GDIPlusHnd, 'GdipCombineRegionRegion');
    GDIPTranslateRegion := GetProcAddress(GDIPlusHnd, 'GdipTranslateRegion');
    GDIPTranslateRegionI := GetProcAddress(GDIPlusHnd, 'GdipTranslateRegionI');
    GDIPTransformRegion := GetProcAddress(GDIPlusHnd, 'GdipTransformRegion');
    GDIPGetRegionBounds := GetProcAddress(GDIPlusHnd, 'GdipGetRegionBounds');
    GDIPGetRegionBoundsI := GetProcAddress(GDIPlusHnd, 'GdipGetRegionBoundsI');
    GDIPGetRegionHRgn := GetProcAddress(GDIPlusHnd, 'GdipGetRegionHRgn');
    GDIPIsEmptyRegion := GetProcAddress(GDIPlusHnd, 'GdipIsEmptyRegion');
    GDIPIsInfiniteRegion := GetProcAddress(GDIPlusHnd, 'GdipIsInfiniteRegion');
    GDIPIsEqualRegion := GetProcAddress(GDIPlusHnd, 'GdipIsEqualRegion');
    GDIPGetRegionDataSize := GetProcAddress(GDIPlusHnd, 'GdipGetRegionDataSize');
    GDIPGetRegionData := GetProcAddress(GDIPlusHnd, 'GdipGetRegionData');
    GDIPIsVisibleRegionPoint := GetProcAddress(GDIPlusHnd, 'GdipIsVisibleRegionPoint');
    GDIPIsVisibleRegionPointI := GetProcAddress(GDIPlusHnd, 'GdipIsVisibleRegionPointI');
    GDIPIsVisibleRegionRect := GetProcAddress(GDIPlusHnd, 'GdipIsVisibleRegionRect');
    GDIPIsVisibleRegionRectI := GetProcAddress(GDIPlusHnd, 'GdipIsVisibleRegionRectI');
    GDIPGetRegionScansCount := GetProcAddress(GDIPlusHnd, 'GdipGetRegionScansCount');
    GDIPGetRegionScans := GetProcAddress(GDIPlusHnd, 'GdipGetRegionScans');
    GDIPGetRegionScansI := GetProcAddress(GDIPlusHnd, 'GdipGetRegionScansI');
    GDIPCloneBrush := GetProcAddress(GDIPlusHnd, 'GdipCloneBrush');
    GDIPDeleteBrush := GetProcAddress(GDIPlusHnd, 'GdipDeleteBrush');
    GDIPGetBrushType := GetProcAddress(GDIPlusHnd, 'GdipGetBrushType');
    GDIPCreateHatchBrush := GetProcAddress(GDIPlusHnd, 'GdipCreateHatchBrush');
    GDIPGetHatchStyle := GetProcAddress(GDIPlusHnd, 'GdipGetHatchStyle');
    GDIPGetHatchForegroundColor := GetProcAddress(GDIPlusHnd, 'GdipGetHatchForegroundColor');
    GDIPGetHatchBackgroundColor := GetProcAddress(GDIPlusHnd, 'GdipGetHatchBackgroundColor');
    GDIPCreateTexture := GetProcAddress(GDIPlusHnd, 'GdipCreateTexture');
    GDIPCreateTexture2 := GetProcAddress(GDIPlusHnd, 'GdipCreateTexture2');
    GDIPCreateTextureIA := GetProcAddress(GDIPlusHnd, 'GdipCreateTextureIA');
    GDIPCreateTexture2I := GetProcAddress(GDIPlusHnd, 'GdipCreateTexture2I');
    GDIPCreateTextureIAI := GetProcAddress(GDIPlusHnd, 'GdipCreateTextureIAI');
    GDIPGetTextureTransform := GetProcAddress(GDIPlusHnd, 'GdipGetTextureTransform');
    GDIPSetTextureTransform := GetProcAddress(GDIPlusHnd, 'GdipSetTextureTransform');
    GDIPResetTextureTransform := GetProcAddress(GDIPlusHnd, 'GdipResetTextureTransform');
    GDIPMultiplyTextureTransform := GetProcAddress(GDIPlusHnd, 'GdipMultiplyTextureTransform');
    GDIPTranslateTextureTransform := GetProcAddress(GDIPlusHnd, 'GdipTranslateTextureTransform');
    GDIPScaleTextureTransform := GetProcAddress(GDIPlusHnd, 'GdipScaleTextureTransform');
    GDIPRotateTextureTransform := GetProcAddress(GDIPlusHnd, 'GdipRotateTextureTransform');
    GDIPSetTextureWrapMode := GetProcAddress(GDIPlusHnd, 'GdipSetTextureWrapMode');
    GDIPGetTextureWrapMode := GetProcAddress(GDIPlusHnd, 'GdipGetTextureWrapMode');
    GDIPGetTextureImage := GetProcAddress(GDIPlusHnd, 'GdipGetTextureImage');
    GDIPCreateSolidFill := GetProcAddress(GDIPlusHnd, 'GdipCreateSolidFill');
    GDIPSetSolidFillColor := GetProcAddress(GDIPlusHnd, 'GdipSetSolidFillColor');
    GDIPGetSolidFillColor := GetProcAddress(GDIPlusHnd, 'GdipGetSolidFillColor');
    GDIPCreateLineBrush := GetProcAddress(GDIPlusHnd, 'GdipCreateLineBrush');
    GDIPCreateLineBrushI := GetProcAddress(GDIPlusHnd, 'GdipCreateLineBrushI');
    GDIPCreateLineBrushFromRect := GetProcAddress(GDIPlusHnd, 'GdipCreateLineBrushFromRect');
    GDIPCreateLineBrushFromRectI := GetProcAddress(GDIPlusHnd, 'GdipCreateLineBrushFromRectI');
    GDIPCreateLineBrushFromRectWithAngle := GetProcAddress(GDIPlusHnd, 'GdipCreateLineBrushFromRectWithAngle');
    GDIPCreateLineBrushFromRectWithAngleI := GetProcAddress(GDIPlusHnd, 'GdipCreateLineBrushFromRectWithAngleI');
    GDIPSetLineColors := GetProcAddress(GDIPlusHnd, 'GdipSetLineColors');
    GDIPGetLineColors := GetProcAddress(GDIPlusHnd, 'GdipGetLineColors');
    GDIPGetLineRect := GetProcAddress(GDIPlusHnd, 'GdipGetLineRect');
    GDIPGetLineRectI := GetProcAddress(GDIPlusHnd, 'GdipGetLineRectI');
    GDIPSetLineGammaCorrection := GetProcAddress(GDIPlusHnd, 'GdipSetLineGammaCorrection');
    GDIPGetLineGammaCorrection := GetProcAddress(GDIPlusHnd, 'GdipGetLineGammaCorrection');
    GDIPGetLineBlendCount := GetProcAddress(GDIPlusHnd, 'GdipGetLineBlendCount');
    GDIPGetLineBlend := GetProcAddress(GDIPlusHnd, 'GdipGetLineBlend');
    GDIPSetLineBlend := GetProcAddress(GDIPlusHnd, 'GdipSetLineBlend');
    GDIPGetLinePresetBlendCount := GetProcAddress(GDIPlusHnd, 'GdipGetLinePresetBlendCount');
    GDIPGetLinePresetBlend := GetProcAddress(GDIPlusHnd, 'GdipGetLinePresetBlend');
    GDIPSetLinePresetBlend := GetProcAddress(GDIPlusHnd, 'GdipSetLinePresetBlend');
    GDIPSetLineSigmaBlend := GetProcAddress(GDIPlusHnd, 'GdipSetLineSigmaBlend');
    GDIPSetLineLinearBlend := GetProcAddress(GDIPlusHnd, 'GdipSetLineLinearBlend');
    GDIPSetLineWrapMode := GetProcAddress(GDIPlusHnd, 'GdipSetLineWrapMode');
    GDIPGetLineWrapMode := GetProcAddress(GDIPlusHnd, 'GdipGetLineWrapMode');
    GDIPGetLineTransform := GetProcAddress(GDIPlusHnd, 'GdipGetLineTransform');
    GDIPSetLineTransform := GetProcAddress(GDIPlusHnd, 'GdipSetLineTransform');
    GDIPResetLineTransform := GetProcAddress(GDIPlusHnd, 'GdipResetLineTransform');
    GDIPMultiplyLineTransform := GetProcAddress(GDIPlusHnd, 'GdipMultiplyLineTransform');
    GDIPTranslateLineTransform := GetProcAddress(GDIPlusHnd, 'GdipTranslateLineTransform');
    GDIPScaleLineTransform := GetProcAddress(GDIPlusHnd, 'GdipScaleLineTransform');
    GDIPRotateLineTransform := GetProcAddress(GDIPlusHnd, 'GdipRotateLineTransform');
    GDIPCreatePathGradient := GetProcAddress(GDIPlusHnd, 'GdipCreatePathGradient');
    GDIPCreatePathGradientI := GetProcAddress(GDIPlusHnd, 'GdipCreatePathGradientI');
    GDIPCreatePathGradientFromPath := GetProcAddress(GDIPlusHnd, 'GdipCreatePathGradientFromPath');
    GDIPGetPathGradientCenterColor := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientCenterColor');
    GDIPSetPathGradientCenterColor := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientCenterColor');
    GDIPGetPathGradientSurroundColorsWithCount := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientSurroundColorsWithCount');
    GDIPSetPathGradientSurroundColorsWithCount := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientSurroundColorsWithCount');
    GDIPGetPathGradientPath := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientPath');
    GDIPSetPathGradientPath := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientPath');
    GDIPGetPathGradientCenterPoint := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientCenterPoint');
    GDIPGetPathGradientCenterPointI := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientCenterPointI');
    GDIPSetPathGradientCenterPoint := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientCenterPoint');
    GDIPSetPathGradientCenterPointI := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientCenterPointI');
    GDIPGetPathGradientRect := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientRect');
    GDIPGetPathGradientRectI := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientRectI');
    GDIPGetPathGradientPointCount := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientPointCount');
    GDIPGetPathGradientSurroundColorCount := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientSurroundColorCount');
    GDIPSetPathGradientGammaCorrection := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientGammaCorrection');
    GDIPGetPathGradientGammaCorrection := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientGammaCorrection');
    GDIPGetPathGradientBlendCount := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientBlendCount');
    GDIPGetPathGradientBlend := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientBlend');
    GDIPSetPathGradientBlend := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientBlend');
    GDIPGetPathGradientPresetBlendCount := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientPresetBlendCount');
    GDIPGetPathGradientPresetBlend := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientPresetBlend');
    GDIPSetPathGradientPresetBlend := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientPresetBlend');
    GDIPSetPathGradientSigmaBlend := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientSigmaBlend');
    GDIPSetPathGradientLinearBlend := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientLinearBlend');
    GDIPGetPathGradientWrapMode := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientWrapMode');
    GDIPSetPathGradientWrapMode := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientWrapMode');
    GDIPGetPathGradientTransform := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientTransform');
    GDIPSetPathGradientTransform := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientTransform');
    GDIPResetPathGradientTransform := GetProcAddress(GDIPlusHnd, 'GdipResetPathGradientTransform');
    GDIPMultiplyPathGradientTransform := GetProcAddress(GDIPlusHnd, 'GdipMultiplyPathGradientTransform');
    GDIPTranslatePathGradientTransform := GetProcAddress(GDIPlusHnd, 'GdipTranslatePathGradientTransform');
    GDIPScalePathGradientTransform := GetProcAddress(GDIPlusHnd, 'GdipScalePathGradientTransform');
    GDIPRotatePathGradientTransform := GetProcAddress(GDIPlusHnd, 'GdipRotatePathGradientTransform');
    GDIPGetPathGradientFocusScales := GetProcAddress(GDIPlusHnd, 'GdipGetPathGradientFocusScales');
    GDIPSetPathGradientFocusScales := GetProcAddress(GDIPlusHnd, 'GdipSetPathGradientFocusScales');
    GDIPCreatePen1 := GetProcAddress(GDIPlusHnd, 'GdipCreatePen1');
    GDIPCreatePen2 := GetProcAddress(GDIPlusHnd, 'GdipCreatePen2');
    GDIPClonePen := GetProcAddress(GDIPlusHnd, 'GdipClonePen');
    GDIPDeletePen := GetProcAddress(GDIPlusHnd, 'GdipDeletePen');
    GDIPSetPenWidth := GetProcAddress(GDIPlusHnd, 'GdipSetPenWidth');
    GDIPGetPenWidth := GetProcAddress(GDIPlusHnd, 'GdipGetPenWidth');
    GDIPSetPenUnit := GetProcAddress(GDIPlusHnd, 'GdipSetPenUnit');
    GDIPGetPenUnit := GetProcAddress(GDIPlusHnd, 'GdipGetPenUnit');
    GDIPSetPenLineCap197819 := GetProcAddress(GDIPlusHnd, 'GdipSetPenLineCap197819');
    GDIPSetPenStartCap := GetProcAddress(GDIPlusHnd, 'GdipSetPenStartCap');
    GDIPSetPenEndCap := GetProcAddress(GDIPlusHnd, 'GdipSetPenEndCap');
    GDIPSetPenDashCap197819 := GetProcAddress(GDIPlusHnd, 'GdipSetPenDashCap197819');
    GDIPGetPenStartCap := GetProcAddress(GDIPlusHnd, 'GdipGetPenStartCap');
    GDIPGetPenEndCap := GetProcAddress(GDIPlusHnd, 'GdipGetPenEndCap');
    GDIPGetPenDashCap197819 := GetProcAddress(GDIPlusHnd, 'GdipGetPenDashCap197819');
    GDIPSetPenLineJoin := GetProcAddress(GDIPlusHnd, 'GdipSetPenLineJoin');
    GDIPGetPenLineJoin := GetProcAddress(GDIPlusHnd, 'GdipGetPenLineJoin');
    GDIPSetPenCustomStartCap := GetProcAddress(GDIPlusHnd, 'GdipSetPenCustomStartCap');
    GDIPGetPenCustomStartCap := GetProcAddress(GDIPlusHnd, 'GdipGetPenCustomStartCap');
    GDIPSetPenCustomEndCap := GetProcAddress(GDIPlusHnd, 'GdipSetPenCustomEndCap');
    GDIPGetPenCustomEndCap := GetProcAddress(GDIPlusHnd, 'GdipGetPenCustomEndCap');
    GDIPSetPenMiterLimit := GetProcAddress(GDIPlusHnd, 'GdipSetPenMiterLimit');
    GDIPGetPenMiterLimit := GetProcAddress(GDIPlusHnd, 'GdipGetPenMiterLimit');
    GDIPSetPenMode := GetProcAddress(GDIPlusHnd, 'GdipSetPenMode');
    GDIPGetPenMode := GetProcAddress(GDIPlusHnd, 'GdipGetPenMode');
    GDIPSetPenTransform := GetProcAddress(GDIPlusHnd, 'GdipSetPenTransform');
    GDIPGetPenTransform := GetProcAddress(GDIPlusHnd, 'GdipGetPenTransform');
    GDIPResetPenTransform := GetProcAddress(GDIPlusHnd, 'GdipResetPenTransform');
    GDIPMultiplyPenTransform := GetProcAddress(GDIPlusHnd, 'GdipMultiplyPenTransform');
    GDIPTranslatePenTransform := GetProcAddress(GDIPlusHnd, 'GdipTranslatePenTransform');
    GDIPScalePenTransform := GetProcAddress(GDIPlusHnd, 'GdipScalePenTransform');
    GDIPRotatePenTransform := GetProcAddress(GDIPlusHnd, 'GdipRotatePenTransform');
    GDIPSetPenColor := GetProcAddress(GDIPlusHnd, 'GdipSetPenColor');
    GDIPGetPenColor := GetProcAddress(GDIPlusHnd, 'GdipGetPenColor');
    GDIPSetPenBrushFill := GetProcAddress(GDIPlusHnd, 'GdipSetPenBrushFill');
    GDIPGetPenBrushFill := GetProcAddress(GDIPlusHnd, 'GdipGetPenBrushFill');
    GDIPGetPenFillType := GetProcAddress(GDIPlusHnd, 'GdipGetPenFillType');
    GDIPGetPenDashStyle := GetProcAddress(GDIPlusHnd, 'GdipGetPenDashStyle');
    GDIPSetPenDashStyle := GetProcAddress(GDIPlusHnd, 'GdipSetPenDashStyle');
    GDIPGetPenDashOffset := GetProcAddress(GDIPlusHnd, 'GdipGetPenDashOffset');
    GDIPSetPenDashOffset := GetProcAddress(GDIPlusHnd, 'GdipSetPenDashOffset');
    GDIPGetPenDashCount := GetProcAddress(GDIPlusHnd, 'GdipGetPenDashCount');
    GDIPSetPenDashArray := GetProcAddress(GDIPlusHnd, 'GdipSetPenDashArray');
    GDIPGetPenDashArray := GetProcAddress(GDIPlusHnd, 'GdipGetPenDashArray');
    GDIPGetPenCompoundCount := GetProcAddress(GDIPlusHnd, 'GdipGetPenCompoundCount');
    GDIPSetPenCompoundArray := GetProcAddress(GDIPlusHnd, 'GdipSetPenCompoundArray');
    GDIPGetPenCompoundArray := GetProcAddress(GDIPlusHnd, 'GdipGetPenCompoundArray');
    GDIPCreateCustomLineCap := GetProcAddress(GDIPlusHnd, 'GdipCreateCustomLineCap');
    GDIPDeleteCustomLineCap := GetProcAddress(GDIPlusHnd, 'GdipDeleteCustomLineCap');
    GDIPCloneCustomLineCap := GetProcAddress(GDIPlusHnd, 'GdipCloneCustomLineCap');
    GDIPGetCustomLineCapType := GetProcAddress(GDIPlusHnd, 'GdipGetCustomLineCapType');
    GDIPSetCustomLineCapStrokeCaps := GetProcAddress(GDIPlusHnd, 'GdipSetCustomLineCapStrokeCaps');
    GDIPGetCustomLineCapStrokeCaps := GetProcAddress(GDIPlusHnd, 'GdipGetCustomLineCapStrokeCaps');
    GDIPSetCustomLineCapStrokeJoin := GetProcAddress(GDIPlusHnd, 'GdipSetCustomLineCapStrokeJoin');
    GDIPGetCustomLineCapStrokeJoin := GetProcAddress(GDIPlusHnd, 'GdipGetCustomLineCapStrokeJoin');
    GDIPSetCustomLineCapBaseCap := GetProcAddress(GDIPlusHnd, 'GdipSetCustomLineCapBaseCap');
    GDIPGetCustomLineCapBaseCap := GetProcAddress(GDIPlusHnd, 'GdipGetCustomLineCapBaseCap');
    GDIPSetCustomLineCapBaseInset := GetProcAddress(GDIPlusHnd, 'GdipSetCustomLineCapBaseInset');
    GDIPGetCustomLineCapBaseInset := GetProcAddress(GDIPlusHnd, 'GdipGetCustomLineCapBaseInset');
    GDIPSetCustomLineCapWidthScale := GetProcAddress(GDIPlusHnd, 'GdipSetCustomLineCapWidthScale');
    GDIPGetCustomLineCapWidthScale := GetProcAddress(GDIPlusHnd, 'GdipGetCustomLineCapWidthScale');
    GDIPCreateAdjustableArrowCap := GetProcAddress(GDIPlusHnd, 'GdipCreateAdjustableArrowCap');
    GDIPSetAdjustableArrowCapHeight := GetProcAddress(GDIPlusHnd, 'GdipSetAdjustableArrowCapHeight');
    GDIPGetAdjustableArrowCapHeight := GetProcAddress(GDIPlusHnd, 'GdipGetAdjustableArrowCapHeight');
    GDIPSetAdjustableArrowCapWidth := GetProcAddress(GDIPlusHnd, 'GdipSetAdjustableArrowCapWidth');
    GDIPGetAdjustableArrowCapWidth := GetProcAddress(GDIPlusHnd, 'GdipGetAdjustableArrowCapWidth');
    GDIPSetAdjustableArrowCapMiddleInset := GetProcAddress(GDIPlusHnd, 'GdipSetAdjustableArrowCapMiddleInset');
    GDIPGetAdjustableArrowCapMiddleInset := GetProcAddress(GDIPlusHnd, 'GdipGetAdjustableArrowCapMiddleInset');
    GDIPSetAdjustableArrowCapFillState := GetProcAddress(GDIPlusHnd, 'GdipSetAdjustableArrowCapFillState');
    GDIPGetAdjustableArrowCapFillState := GetProcAddress(GDIPlusHnd, 'GdipGetAdjustableArrowCapFillState');
    GDIPLoadImageFromStream := GetProcAddress(GDIPlusHnd, 'GdipLoadImageFromStream');
    GDIPLoadImageFromFile := GetProcAddress(GDIPlusHnd, 'GdipLoadImageFromFile');
    GDIPLoadImageFromStreamICM := GetProcAddress(GDIPlusHnd, 'GdipLoadImageFromStreamICM');
    GDIPLoadImageFromFileICM := GetProcAddress(GDIPlusHnd, 'GdipLoadImageFromFileICM');
    GDIPCloneImage := GetProcAddress(GDIPlusHnd, 'GdipCloneImage');
    GDIPDisposeImage := GetProcAddress(GDIPlusHnd, 'GdipDisposeImage');
    GDIPSaveImageToFile := GetProcAddress(GDIPlusHnd, 'GdipSaveImageToFile');
    GDIPSaveImageToStream := GetProcAddress(GDIPlusHnd, 'GdipSaveImageToStream');
    GDIPSaveAdd := GetProcAddress(GDIPlusHnd, 'GdipSaveAdd');
    GDIPSaveAddImage := GetProcAddress(GDIPlusHnd, 'GdipSaveAddImage');
    GDIPGetImageGraphicsContext := GetProcAddress(GDIPlusHnd, 'GdipGetImageGraphicsContext');
    GDIPGetImageBounds := GetProcAddress(GDIPlusHnd, 'GdipGetImageBounds');
    GDIPGetImageDimension := GetProcAddress(GDIPlusHnd, 'GdipGetImageDimension');
    GDIPGetImageType := GetProcAddress(GDIPlusHnd, 'GdipGetImageType');
    GDIPGetImageWidth := GetProcAddress(GDIPlusHnd, 'GdipGetImageWidth');
    GDIPGetImageHeight := GetProcAddress(GDIPlusHnd, 'GdipGetImageHeight');
    GDIPGetImageHorizontalResolution := GetProcAddress(GDIPlusHnd, 'GdipGetImageHorizontalResolution');
    GDIPGetImageVerticalResolution := GetProcAddress(GDIPlusHnd, 'GdipGetImageVerticalResolution');
    GDIPGetImageFlags := GetProcAddress(GDIPlusHnd, 'GdipGetImageFlags');
    GDIPGetImageRawFormat := GetProcAddress(GDIPlusHnd, 'GdipGetImageRawFormat');
    GDIPGetImagePixelFormat := GetProcAddress(GDIPlusHnd, 'GdipGetImagePixelFormat');
    GDIPGetImageThumbnail := GetProcAddress(GDIPlusHnd, 'GdipGetImageThumbnail');
    GDIPGetEncoderParameterListSize := GetProcAddress(GDIPlusHnd, 'GdipGetEncoderParameterListSize');
    GDIPGetEncoderParameterList := GetProcAddress(GDIPlusHnd, 'GdipGetEncoderParameterList');
    GDIPImageGetFrameDimensionsCount := GetProcAddress(GDIPlusHnd, 'GdipImageGetFrameDimensionsCount');
    GDIPImageGetFrameDimensionsList := GetProcAddress(GDIPlusHnd, 'GdipImageGetFrameDimensionsList');
    GDIPImageGetFrameCount := GetProcAddress(GDIPlusHnd, 'GdipImageGetFrameCount');
    GDIPImageSelectActiveFrame := GetProcAddress(GDIPlusHnd, 'GdipImageSelectActiveFrame');
    GDIPImageRotateFlip := GetProcAddress(GDIPlusHnd, 'GdipImageRotateFlip');
    GDIPGetImagePalette := GetProcAddress(GDIPlusHnd, 'GdipGetImagePalette');
    GDIPSetImagePalette := GetProcAddress(GDIPlusHnd, 'GdipSetImagePalette');
    GDIPGetImagePaletteSize := GetProcAddress(GDIPlusHnd, 'GdipGetImagePaletteSize');
    GDIPGetPropertyCount := GetProcAddress(GDIPlusHnd, 'GdipGetPropertyCount');
    GDIPGetPropertyIdList := GetProcAddress(GDIPlusHnd, 'GdipGetPropertyIdList');
    GDIPGetPropertyItemSize := GetProcAddress(GDIPlusHnd, 'GdipGetPropertyItemSize');
    GDIPGetPropertyItem := GetProcAddress(GDIPlusHnd, 'GdipGetPropertyItem');
    GDIPGetPropertySize := GetProcAddress(GDIPlusHnd, 'GdipGetPropertySize');
    GDIPGetAllPropertyItems := GetProcAddress(GDIPlusHnd, 'GdipGetAllPropertyItems');
    GDIPRemovePropertyItem := GetProcAddress(GDIPlusHnd, 'GdipRemovePropertyItem');
    GDIPSetPropertyItem := GetProcAddress(GDIPlusHnd, 'GdipSetPropertyItem');
    GDIPImageForceValidation := GetProcAddress(GDIPlusHnd, 'GdipImageForceValidation');
    GDIPCreateBitmapFromStream := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromStream');
    GDIPCreateBitmapFromFile := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromFile');
    GDIPCreateBitmapFromStreamICM := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromStreamICM');
    GDIPCreateBitmapFromFileICM := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromFileICM');
    GDIPCreateBitmapFromScan0 := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromScan0');
    GDIPCreateBitmapFromGraphics := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromGraphics');
    GDIPCreateBitmapFromGdiDib := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromGdiDib');
    GDIPCreateBitmapFromHBITMAP := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromHBITMAP');
    GDIPCreateHBITMAPFromBitmap := GetProcAddress(GDIPlusHnd, 'GdipCreateHBITMAPFromBitmap');
    GDIPCreateBitmapFromHICON := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromHICON');
    GDIPCreateHICONFromBitmap := GetProcAddress(GDIPlusHnd, 'GdipCreateHICONFromBitmap');
    GDIPCreateBitmapFromResource := GetProcAddress(GDIPlusHnd, 'GdipCreateBitmapFromResource');
    GDIPCloneBitmapArea := GetProcAddress(GDIPlusHnd, 'GdipCloneBitmapArea');
    GDIPCloneBitmapAreaI := GetProcAddress(GDIPlusHnd, 'GdipCloneBitmapAreaI');
    GDIPBitmapLockBits := GetProcAddress(GDIPlusHnd, 'GdipBitmapLockBits');
    GDIPBitmapUnlockBits := GetProcAddress(GDIPlusHnd, 'GdipBitmapUnlockBits');
    GDIPBitmapGetPixel := GetProcAddress(GDIPlusHnd, 'GdipBitmapGetPixel');
    GDIPBitmapSetPixel := GetProcAddress(GDIPlusHnd, 'GdipBitmapSetPixel');
    GDIPBitmapSetResolution := GetProcAddress(GDIPlusHnd, 'GdipBitmapSetResolution');
    GDIPCreateImageAttributes := GetProcAddress(GDIPlusHnd, 'GdipCreateImageAttributes');
    GDIPCloneImageAttributes := GetProcAddress(GDIPlusHnd, 'GdipCloneImageAttributes');
    GDIPDisposeImageAttributes := GetProcAddress(GDIPlusHnd, 'GdipDisposeImageAttributes');
    GDIPSetImageAttributesToIdentity := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesToIdentity');
    GDIPResetImageAttributes := GetProcAddress(GDIPlusHnd, 'GdipResetImageAttributes');
    GDIPSetImageAttributesColorMatrix := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesColorMatrix');
    GDIPSetImageAttributesThreshold := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesThreshold');
    GDIPSetImageAttributesGamma := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesGamma');
    GDIPSetImageAttributesNoOp := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesNoOp');
    GDIPSetImageAttributesColorKeys := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesColorKeys');
    GDIPSetImageAttributesOutputChannel := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesOutputChannel');
    GDIPSetImageAttributesOutputChannelColorProfile := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesOutputChannelColorProfile');
    GDIPSetImageAttributesRemapTable := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesRemapTable');
    GDIPSetImageAttributesWrapMode := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesWrapMode');
    GDIPSetImageAttributesICMMode := GetProcAddress(GDIPlusHnd, 'GdipSetImageAttributesICMMode');
    GDIPGetImageAttributesAdjustedPalette := GetProcAddress(GDIPlusHnd, 'GdipGetImageAttributesAdjustedPalette');
    GDIPFlush := GetProcAddress(GDIPlusHnd, 'GdipFlush');
    GDIPCreateFromHDC := GetProcAddress(GDIPlusHnd, 'GdipCreateFromHDC');
    GDIPCreateFromHDC2 := GetProcAddress(GDIPlusHnd, 'GdipCreateFromHDC2');
    GDIPCreateFromHWND := GetProcAddress(GDIPlusHnd, 'GdipCreateFromHWND');
    GDIPCreateFromHWNDICM := GetProcAddress(GDIPlusHnd, 'GdipCreateFromHWNDICM');
    GDIPDeleteGraphics := GetProcAddress(GDIPlusHnd, 'GdipDeleteGraphics');
    GDIPGetDC := GetProcAddress(GDIPlusHnd, 'GdipGetDC');
    GDIPReleaseDC := GetProcAddress(GDIPlusHnd, 'GdipReleaseDC');
    GDIPSetCompositingMode := GetProcAddress(GDIPlusHnd, 'GdipSetCompositingMode');
    GDIPGetCompositingMode := GetProcAddress(GDIPlusHnd, 'GdipGetCompositingMode');
    GDIPSetRenderingOrigin := GetProcAddress(GDIPlusHnd, 'GdipSetRenderingOrigin');
    GDIPGetRenderingOrigin := GetProcAddress(GDIPlusHnd, 'GdipGetRenderingOrigin');
    GDIPSetCompositingQuality := GetProcAddress(GDIPlusHnd, 'GdipSetCompositingQuality');
    GDIPGetCompositingQuality := GetProcAddress(GDIPlusHnd, 'GdipGetCompositingQuality');
    GDIPSetSmoothingMode := GetProcAddress(GDIPlusHnd, 'GdipSetSmoothingMode');
    GDIPGetSmoothingMode := GetProcAddress(GDIPlusHnd, 'GdipGetSmoothingMode');
    GDIPSetPixelOffsetMode := GetProcAddress(GDIPlusHnd, 'GdipSetPixelOffsetMode');
    GDIPGetPixelOffsetMode := GetProcAddress(GDIPlusHnd, 'GdipGetPixelOffsetMode');
    GDIPSetTextRenderingHint := GetProcAddress(GDIPlusHnd, 'GdipSetTextRenderingHint');
    GDIPGetTextRenderingHint := GetProcAddress(GDIPlusHnd, 'GdipGetTextRenderingHint');
    GDIPSetTextContrast := GetProcAddress(GDIPlusHnd, 'GdipSetTextContrast');
    GDIPGetTextContrast := GetProcAddress(GDIPlusHnd, 'GdipGetTextContrast');
    GDIPSetInterpolationMode := GetProcAddress(GDIPlusHnd, 'GdipSetInterpolationMode');
    GDIPGetInterpolationMode := GetProcAddress(GDIPlusHnd, 'GdipGetInterpolationMode');
    GDIPSetWorldTransform := GetProcAddress(GDIPlusHnd, 'GdipSetWorldTransform');
    GDIPResetWorldTransform := GetProcAddress(GDIPlusHnd, 'GdipResetWorldTransform');
    GDIPMultiplyWorldTransform := GetProcAddress(GDIPlusHnd, 'GdipMultiplyWorldTransform');
    GDIPTranslateWorldTransform := GetProcAddress(GDIPlusHnd, 'GdipTranslateWorldTransform');
    GDIPScaleWorldTransform := GetProcAddress(GDIPlusHnd, 'GdipScaleWorldTransform');
    GDIPRotateWorldTransform := GetProcAddress(GDIPlusHnd, 'GdipRotateWorldTransform');
    GDIPGetWorldTransform := GetProcAddress(GDIPlusHnd, 'GdipGetWorldTransform');
    GDIPResetPageTransform := GetProcAddress(GDIPlusHnd, 'GdipResetPageTransform');
    GDIPGetPageUnit := GetProcAddress(GDIPlusHnd, 'GdipGetPageUnit');
    GDIPGetPageScale := GetProcAddress(GDIPlusHnd, 'GdipGetPageScale');
    GDIPSetPageUnit := GetProcAddress(GDIPlusHnd, 'GdipSetPageUnit');
    GDIPSetPageScale := GetProcAddress(GDIPlusHnd, 'GdipSetPageScale');
    GDIPGetDpiX := GetProcAddress(GDIPlusHnd, 'GdipGetDpiX');
    GDIPGetDpiY := GetProcAddress(GDIPlusHnd, 'GdipGetDpiY');
    GDIPTransformPoints := GetProcAddress(GDIPlusHnd, 'GdipTransformPoints');
    GDIPTransformPointsI := GetProcAddress(GDIPlusHnd, 'GdipTransformPointsI');
    GDIPGetNearestColor := GetProcAddress(GDIPlusHnd, 'GdipGetNearestColor');
    GDIPCreateHalftonePalette := GetProcAddress(GDIPlusHnd, 'GdipCreateHalftonePalette');
    GDIPDrawLine := GetProcAddress(GDIPlusHnd, 'GdipDrawLine');
    GDIPDrawLineI := GetProcAddress(GDIPlusHnd, 'GdipDrawLineI');
    GDIPDrawLines := GetProcAddress(GDIPlusHnd, 'GdipDrawLines');
    GDIPDrawLinesI := GetProcAddress(GDIPlusHnd, 'GdipDrawLinesI');
    GDIPDrawArc := GetProcAddress(GDIPlusHnd, 'GdipDrawArc');
    GDIPDrawArcI := GetProcAddress(GDIPlusHnd, 'GdipDrawArcI');
    GDIPDrawBezier := GetProcAddress(GDIPlusHnd, 'GdipDrawBezier');
    GDIPDrawBezierI := GetProcAddress(GDIPlusHnd, 'GdipDrawBezierI');
    GDIPDrawBeziers := GetProcAddress(GDIPlusHnd, 'GdipDrawBeziers');
    GDIPDrawBeziersI := GetProcAddress(GDIPlusHnd, 'GdipDrawBeziersI');
    GDIPDrawRectangle := GetProcAddress(GDIPlusHnd, 'GdipDrawRectangle');
    GDIPDrawRectangleI := GetProcAddress(GDIPlusHnd, 'GdipDrawRectangleI');
    GDIPDrawRectangles := GetProcAddress(GDIPlusHnd, 'GdipDrawRectangles');
    GDIPDrawRectanglesI := GetProcAddress(GDIPlusHnd, 'GdipDrawRectanglesI');
    GDIPDrawEllipse := GetProcAddress(GDIPlusHnd, 'GdipDrawEllipse');
    GDIPDrawEllipseI := GetProcAddress(GDIPlusHnd, 'GdipDrawEllipseI');
    GDIPDrawPie := GetProcAddress(GDIPlusHnd, 'GdipDrawPie');
    GDIPDrawPieI := GetProcAddress(GDIPlusHnd, 'GdipDrawPieI');
    GDIPDrawPolygon := GetProcAddress(GDIPlusHnd, 'GdipDrawPolygon');
    GDIPDrawPolygonI := GetProcAddress(GDIPlusHnd, 'GdipDrawPolygonI');
    GDIPDrawPath := GetProcAddress(GDIPlusHnd, 'GdipDrawPath');
    GDIPDrawCurve := GetProcAddress(GDIPlusHnd, 'GdipDrawCurve');
    GDIPDrawCurveI := GetProcAddress(GDIPlusHnd, 'GdipDrawCurveI');
    GDIPDrawCurve2 := GetProcAddress(GDIPlusHnd, 'GdipDrawCurve2');
    GDIPDrawCurve2I := GetProcAddress(GDIPlusHnd, 'GdipDrawCurve2I');
    GDIPDrawCurve3 := GetProcAddress(GDIPlusHnd, 'GdipDrawCurve3');
    GDIPDrawCurve3I := GetProcAddress(GDIPlusHnd, 'GdipDrawCurve3I');
    GDIPDrawClosedCurve := GetProcAddress(GDIPlusHnd, 'GdipDrawClosedCurve');
    GDIPDrawClosedCurveI := GetProcAddress(GDIPlusHnd, 'GdipDrawClosedCurveI');
    GDIPDrawClosedCurve2 := GetProcAddress(GDIPlusHnd, 'GdipDrawClosedCurve2');
    GDIPDrawClosedCurve2I := GetProcAddress(GDIPlusHnd, 'GdipDrawClosedCurve2I');
    GDIPGraphicsClear := GetProcAddress(GDIPlusHnd, 'GdipGraphicsClear');
    GDIPFillRectangle := GetProcAddress(GDIPlusHnd, 'GdipFillRectangle');
    GDIPFillRectangleI := GetProcAddress(GDIPlusHnd, 'GdipFillRectangleI');
    GDIPFillRectangles := GetProcAddress(GDIPlusHnd, 'GdipFillRectangles');
    GDIPFillRectanglesI := GetProcAddress(GDIPlusHnd, 'GdipFillRectanglesI');
    GDIPFillPolygon := GetProcAddress(GDIPlusHnd, 'GdipFillPolygon');
    GDIPFillPolygonI := GetProcAddress(GDIPlusHnd, 'GdipFillPolygonI');
    GDIPFillPolygon2 := GetProcAddress(GDIPlusHnd, 'GdipFillPolygon2');
    GDIPFillPolygon2I := GetProcAddress(GDIPlusHnd, 'GdipFillPolygon2I');
    GDIPFillEllipse := GetProcAddress(GDIPlusHnd, 'GdipFillEllipse');
    GDIPFillEllipseI := GetProcAddress(GDIPlusHnd, 'GdipFillEllipseI');
    GDIPFillPie := GetProcAddress(GDIPlusHnd, 'GdipFillPie');
    GDIPFillPieI := GetProcAddress(GDIPlusHnd, 'GdipFillPieI');
    GDIPFillPath := GetProcAddress(GDIPlusHnd, 'GdipFillPath');
    GDIPFillClosedCurve := GetProcAddress(GDIPlusHnd, 'GdipFillClosedCurve');
    GDIPFillClosedCurveI := GetProcAddress(GDIPlusHnd, 'GdipFillClosedCurveI');
    GDIPFillClosedCurve2 := GetProcAddress(GDIPlusHnd, 'GdipFillClosedCurve2');
    GDIPFillClosedCurve2I := GetProcAddress(GDIPlusHnd, 'GdipFillClosedCurve2I');
    GDIPFillRegion := GetProcAddress(GDIPlusHnd, 'GdipFillRegion');
    GDIPDrawImage := GetProcAddress(GDIPlusHnd, 'GdipDrawImage');
    GDIPDrawImageI := GetProcAddress(GDIPlusHnd, 'GdipDrawImageI');
    GDIPDrawImageRect := GetProcAddress(GDIPlusHnd, 'GdipDrawImageRect');
    GDIPDrawImageRectI := GetProcAddress(GDIPlusHnd, 'GdipDrawImageRectI');
    GDIPDrawImagePoints := GetProcAddress(GDIPlusHnd, 'GdipDrawImagePoints');
    GDIPDrawImagePointsI := GetProcAddress(GDIPlusHnd, 'GdipDrawImagePointsI');
    GDIPDrawImagePointRect := GetProcAddress(GDIPlusHnd, 'GdipDrawImagePointRect');
    GDIPDrawImagePointRectI := GetProcAddress(GDIPlusHnd, 'GdipDrawImagePointRectI');
    GDIPDrawImageRectRect := GetProcAddress(GDIPlusHnd, 'GdipDrawImageRectRect');
    GDIPDrawImageRectRectI := GetProcAddress(GDIPlusHnd, 'GdipDrawImageRectRectI');
    GDIPDrawImagePointsRect := GetProcAddress(GDIPlusHnd, 'GdipDrawImagePointsRect');
    GDIPDrawImagePointsRectI := GetProcAddress(GDIPlusHnd, 'GdipDrawImagePointsRectI');
    GDIPEnumerateMetafileDestPoint := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileDestPoint');
    GDIPEnumerateMetafileDestPointI := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileDestPointI');
    GDIPEnumerateMetafileDestRect := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileDestRect');
    GDIPEnumerateMetafileDestRectI := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileDestRectI');
    GDIPEnumerateMetafileDestPoints := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileDestPoints');
    GDIPEnumerateMetafileDestPointsI := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileDestPointsI');
    GDIPEnumerateMetafileSrcRectDestPoint := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileSrcRectDestPoint');
    GDIPEnumerateMetafileSrcRectDestPointI := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileSrcRectDestPointI');
    GDIPEnumerateMetafileSrcRectDestRect := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileSrcRectDestRect');
    GDIPEnumerateMetafileSrcRectDestRectI := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileSrcRectDestRectI');
    GDIPEnumerateMetafileSrcRectDestPoints := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileSrcRectDestPoints');
    GDIPEnumerateMetafileSrcRectDestPointsI := GetProcAddress(GDIPlusHnd, 'GdipEnumerateMetafileSrcRectDestPointsI');
    GDIPPlayMetafileRecord := GetProcAddress(GDIPlusHnd, 'GdipPlayMetafileRecord');
    GDIPSetClipGraphics := GetProcAddress(GDIPlusHnd, 'GdipSetClipGraphics');
    GDIPSetClipRect := GetProcAddress(GDIPlusHnd, 'GdipSetClipRect');
    GDIPSetClipRectI := GetProcAddress(GDIPlusHnd, 'GdipSetClipRectI');
    GDIPSetClipPath := GetProcAddress(GDIPlusHnd, 'GdipSetClipPath');
    GDIPSetClipRegion := GetProcAddress(GDIPlusHnd, 'GdipSetClipRegion');
    GDIPSetClipHrgn := GetProcAddress(GDIPlusHnd, 'GdipSetClipHrgn');
    GDIPResetClip := GetProcAddress(GDIPlusHnd, 'GdipResetClip');
    GDIPTranslateClip := GetProcAddress(GDIPlusHnd, 'GdipTranslateClip');
    GDIPTranslateClipI := GetProcAddress(GDIPlusHnd, 'GdipTranslateClipI');
    GDIPGetClip := GetProcAddress(GDIPlusHnd, 'GdipGetClip');
    GDIPGetClipBounds := GetProcAddress(GDIPlusHnd, 'GdipGetClipBounds');
    GDIPGetClipBoundsI := GetProcAddress(GDIPlusHnd, 'GdipGetClipBoundsI');
    GDIPIsClipEmpty := GetProcAddress(GDIPlusHnd, 'GdipIsClipEmpty');
    GDIPGetVisibleClipBounds := GetProcAddress(GDIPlusHnd, 'GdipGetVisibleClipBounds');
    GDIPGetVisibleClipBoundsI := GetProcAddress(GDIPlusHnd, 'GdipGetVisibleClipBoundsI');
    GDIPIsVisibleClipEmpty := GetProcAddress(GDIPlusHnd, 'GdipIsVisibleClipEmpty');
    GDIPIsVisiblePoint := GetProcAddress(GDIPlusHnd, 'GdipIsVisiblePoint');
    GDIPIsVisiblePointI := GetProcAddress(GDIPlusHnd, 'GdipIsVisiblePointI');
    GDIPIsVisibleRect := GetProcAddress(GDIPlusHnd, 'GdipIsVisibleRect');
    GDIPIsVisibleRectI := GetProcAddress(GDIPlusHnd, 'GdipIsVisibleRectI');
    GDIPSaveGraphics := GetProcAddress(GDIPlusHnd, 'GdipSaveGraphics');
    GDIPRestoreGraphics := GetProcAddress(GDIPlusHnd, 'GdipRestoreGraphics');
    GDIPBeginContainer := GetProcAddress(GDIPlusHnd, 'GdipBeginContainer');
    GDIPBeginContainerI := GetProcAddress(GDIPlusHnd, 'GdipBeginContainerI');
    GDIPBeginContainer2 := GetProcAddress(GDIPlusHnd, 'GdipBeginContainer2');
    GDIPEndContainer := GetProcAddress(GDIPlusHnd, 'GdipEndContainer');
    GDIPGetMetafileHeaderFromWmf := GetProcAddress(GDIPlusHnd, 'GdipGetMetafileHeaderFromWmf');
    GDIPGetMetafileHeaderFromEmf := GetProcAddress(GDIPlusHnd, 'GdipGetMetafileHeaderFromEmf');
    GDIPGetMetafileHeaderFromFile := GetProcAddress(GDIPlusHnd, 'GdipGetMetafileHeaderFromFile');
    GDIPGetMetafileHeaderFromStream := GetProcAddress(GDIPlusHnd, 'GdipGetMetafileHeaderFromStream');
    GDIPGetMetafileHeaderFromMetafile := GetProcAddress(GDIPlusHnd, 'GdipGetMetafileHeaderFromMetafile');
    GDIPGetHemfFromMetafile := GetProcAddress(GDIPlusHnd, 'GdipGetHemfFromMetafile');
    GDIPCreateStreamOnFile := GetProcAddress(GDIPlusHnd, 'GdipCreateStreamOnFile');
    GDIPCreateMetafileFromWmf := GetProcAddress(GDIPlusHnd, 'GdipCreateMetafileFromWmf');
    GDIPCreateMetafileFromEmf := GetProcAddress(GDIPlusHnd, 'GdipCreateMetafileFromEmf');
    GDIPCreateMetafileFromFile := GetProcAddress(GDIPlusHnd, 'GdipCreateMetafileFromFile');
    GDIPCreateMetafileFromWmfFile := GetProcAddress(GDIPlusHnd, 'GdipCreateMetafileFromWmfFile');
    GDIPCreateMetafileFromStream := GetProcAddress(GDIPlusHnd, 'GdipCreateMetafileFromStream');
    GDIPRecordMetafile := GetProcAddress(GDIPlusHnd, 'GdipRecordMetafile');
    GDIPRecordMetafileI := GetProcAddress(GDIPlusHnd, 'GdipRecordMetafileI');
    GDIPRecordMetafileFileName := GetProcAddress(GDIPlusHnd, 'GdipRecordMetafileFileName');
    GDIPRecordMetafileFileNameI := GetProcAddress(GDIPlusHnd, 'GdipRecordMetafileFileNameI');
    GDIPRecordMetafileStream := GetProcAddress(GDIPlusHnd, 'GdipRecordMetafileStream');
    GDIPRecordMetafileStreamI := GetProcAddress(GDIPlusHnd, 'GdipRecordMetafileStreamI');
    GDIPSetMetafileDownLevelRasterizationLimit := GetProcAddress(GDIPlusHnd, 'GdipSetMetafileDownLevelRasterizationLimit');
    GDIPGetMetafileDownLevelRasterizationLimit := GetProcAddress(GDIPlusHnd, 'GdipGetMetafileDownLevelRasterizationLimit');
    GDIPGetImageDecodersSize := GetProcAddress(GDIPlusHnd, 'GdipGetImageDecodersSize');
    GDIPGetImageDecoders := GetProcAddress(GDIPlusHnd, 'GdipGetImageDecoders');
    GDIPGetImageEncodersSize := GetProcAddress(GDIPlusHnd, 'GdipGetImageEncodersSize');
    GDIPGetImageEncoders := GetProcAddress(GDIPlusHnd, 'GdipGetImageEncoders');
    GDIPComment := GetProcAddress(GDIPlusHnd, 'GdipComment');
    GDIPCreateFontFamilyFromName := GetProcAddress(GDIPlusHnd, 'GdipCreateFontFamilyFromName');
    GDIPDeleteFontFamily := GetProcAddress(GDIPlusHnd, 'GdipDeleteFontFamily');
    GDIPCloneFontFamily := GetProcAddress(GDIPlusHnd, 'GdipCloneFontFamily');
    GDIPGetGenericFontFamilySansSerif := GetProcAddress(GDIPlusHnd, 'GdipGetGenericFontFamilySansSerif');
    GDIPGetGenericFontFamilySerif := GetProcAddress(GDIPlusHnd, 'GdipGetGenericFontFamilySerif');
    GDIPGetGenericFontFamilyMonospace := GetProcAddress(GDIPlusHnd, 'GdipGetGenericFontFamilyMonospace');
    GDIPGetFamilyName := GetProcAddress(GDIPlusHnd, 'GdipGetFamilyName');
    GDIPIsStyleAvailable := GetProcAddress(GDIPlusHnd, 'GdipIsStyleAvailable');
    GDIPFontCollectionEnumerable := GetProcAddress(GDIPlusHnd, 'GdipFontCollectionEnumerable');
    GDIPFontCollectionEnumerate := GetProcAddress(GDIPlusHnd, 'GdipFontCollectionEnumerate');
    GDIPGetEmHeight := GetProcAddress(GDIPlusHnd, 'GdipGetEmHeight');
    GDIPGetCellAscent := GetProcAddress(GDIPlusHnd, 'GdipGetCellAscent');
    GDIPGetCellDescent := GetProcAddress(GDIPlusHnd, 'GdipGetCellDescent');
    GDIPGetLineSpacing := GetProcAddress(GDIPlusHnd, 'GdipGetLineSpacing');
    GDIPCreateFontFromDC := GetProcAddress(GDIPlusHnd, 'GdipCreateFontFromDC');
    GDIPCreateFontFromLogfontA := GetProcAddress(GDIPlusHnd, 'GdipCreateFontFromLogfontA');
    GDIPCreateFontFromLogfontW := GetProcAddress(GDIPlusHnd, 'GdipCreateFontFromLogfontW');
    GDIPCreateFont := GetProcAddress(GDIPlusHnd, 'GdipCreateFont');
    GDIPCloneFont := GetProcAddress(GDIPlusHnd, 'GdipCloneFont');
    GDIPDeleteFont := GetProcAddress(GDIPlusHnd, 'GdipDeleteFont');
    GDIPGetFamily := GetProcAddress(GDIPlusHnd, 'GdipGetFamily');
    GDIPGetFontStyle := GetProcAddress(GDIPlusHnd, 'GdipGetFontStyle');
    GDIPGetFontSize := GetProcAddress(GDIPlusHnd, 'GdipGetFontSize');
    GDIPGetFontUnit := GetProcAddress(GDIPlusHnd, 'GdipGetFontUnit');
    GDIPGetFontHeight := GetProcAddress(GDIPlusHnd, 'GdipGetFontHeight');
    GDIPGetFontHeightGivenDPI := GetProcAddress(GDIPlusHnd, 'GdipGetFontHeightGivenDPI');
    GDIPGetLogFontA := GetProcAddress(GDIPlusHnd, 'GdipGetLogFontA');
    GDIPGetLogFontW := GetProcAddress(GDIPlusHnd, 'GdipGetLogFontW');
    GDIPNewInstalledFontCollection := GetProcAddress(GDIPlusHnd, 'GdipNewInstalledFontCollection');
    GDIPNewPrivateFontCollection := GetProcAddress(GDIPlusHnd, 'GdipNewPrivateFontCollection');
    GDIPDeletePrivateFontCollection := GetProcAddress(GDIPlusHnd, 'GdipDeletePrivateFontCollection');
    GDIPGetFontCollectionFamilyCount := GetProcAddress(GDIPlusHnd, 'GdipGetFontCollectionFamilyCount');
    GDIPGetFontCollectionFamilyList := GetProcAddress(GDIPlusHnd, 'GdipGetFontCollectionFamilyList');
    GDIPPrivateAddFontFile := GetProcAddress(GDIPlusHnd, 'GdipPrivateAddFontFile');
    GDIPPrivateAddMemoryFont := GetProcAddress(GDIPlusHnd, 'GdipPrivateAddMemoryFont');
    GDIPDrawString := GetProcAddress(GDIPlusHnd, 'GdipDrawString');
    GDIPMeasureString := GetProcAddress(GDIPlusHnd, 'GdipMeasureString');
    GDIPMeasureCharacterRanges := GetProcAddress(GDIPlusHnd, 'GdipMeasureCharacterRanges');
    GDIPDrawDriverString := GetProcAddress(GDIPlusHnd, 'GdipDrawDriverString');
    GDIPMeasureDriverString := GetProcAddress(GDIPlusHnd, 'GdipMeasureDriverString');
    GDIPCreateStringFormat := GetProcAddress(GDIPlusHnd, 'GdipCreateStringFormat');
    GDIPStringFormatGetGenericDefault := GetProcAddress(GDIPlusHnd, 'GdipStringFormatGetGenericDefault');
    GDIPStringFormatGetGenericTypographic := GetProcAddress(GDIPlusHnd, 'GdipStringFormatGetGenericTypographic');
    GDIPDeleteStringFormat := GetProcAddress(GDIPlusHnd, 'GdipDeleteStringFormat');
    GDIPCloneStringFormat := GetProcAddress(GDIPlusHnd, 'GdipCloneStringFormat');
    GDIPSetStringFormatFlags := GetProcAddress(GDIPlusHnd, 'GdipSetStringFormatFlags');
    GDIPGetStringFormatFlags := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatFlags');
    GDIPSetStringFormatAlign := GetProcAddress(GDIPlusHnd, 'GdipSetStringFormatAlign');
    GDIPGetStringFormatAlign := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatAlign');
    GDIPSetStringFormatLineAlign := GetProcAddress(GDIPlusHnd, 'GdipSetStringFormatLineAlign');
    GDIPGetStringFormatLineAlign := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatLineAlign');
    GDIPSetStringFormatTrimming := GetProcAddress(GDIPlusHnd, 'GdipSetStringFormatTrimming');
    GDIPGetStringFormatTrimming := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatTrimming');
    GDIPSetStringFormatHotkeyPrefix := GetProcAddress(GDIPlusHnd, 'GdipSetStringFormatHotkeyPrefix');
    GDIPGetStringFormatHotkeyPrefix := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatHotkeyPrefix');
    GDIPSetStringFormatTabStops := GetProcAddress(GDIPlusHnd, 'GdipSetStringFormatTabStops');
    GDIPGetStringFormatTabStops := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatTabStops');
    GDIPGetStringFormatTabStopCount := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatTabStopCount');
    GDIPSetStringFormatDigitSubstitution := GetProcAddress(GDIPlusHnd, 'GdipSetStringFormatDigitSubstitution');
    GDIPGetStringFormatDigitSubstitution := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatDigitSubstitution');
    GDIPGetStringFormatMeasurableCharacterRangeCount := GetProcAddress(GDIPlusHnd, 'GdipGetStringFormatMeasurableCharacterRangeCount');
    GDIPSetStringFormatMeasurableCharacterRanges := GetProcAddress(GDIPlusHnd, 'GdipSetStringFormatMeasurableCharacterRanges');
//    GDIPCreateCachedBitmap := GetProcAddress(GDIPlusHnd, 'GdipCreateCachedBitmap');
//    GDIPDeleteCachedBitmap := GetProcAddress(GDIPlusHnd, 'GdipDeleteCachedBitmap');
//    GDIPDrawCachedBitmap := GetProcAddress(GDIPlusHnd, 'GdipDrawCachedBitmap');
    GDIPEmfToWmfBits := GetProcAddress(GDIPlusHnd, 'GdipEmfToWmfBits');
  end;

begin
  try
    GDIPlusHnd := LoadLibrary('GdiPlus.dll');
    if GDIPlusHnd <> 0 then
      Init;
  finally
    if GDIPlusHnd <> 0 then
      GDIPlusLoaded := 0
    else
      GDIPlusLoaded := GetLastError;
  end;
  if (GDIPlusLoaded = 0) and (GDIPlusToken = 0) then
  begin
    StartupInput.DebugEventCallback := nil;
    StartupInput.SuppressBackgroundThread := True;
    StartupInput.SuppressExternalCodecs   := False;
    StartupInput.GdiplusVersion := 1;
    StartupOutput.NotificationHook := nil;
    StartupOutput.NotificationUnhook := nil;
    try
      GdiplusStartup(GDIPlusToken, @StartupInput, @StartupOutput);
      GDIPlusLoaded := 0;
    except
      if @StartupOutput = nil then
      begin
        GDIPlusLoaded := -1;
        FreeGDIPlus;
      end;
    end;
  end;
end;

function GetEncoderClsid(const EncoderName: WideString; var AClsid: TGUID): Integer;
var
   I, vNumber, vSize: Cardinal;
   pImageCodecInfoBase, pImageCodecInfoCurr: PImageCodecInfo;
   vEncoderName: WideString;
begin
   Result := -1;
   GetImageEncodersSize(vNumber, vSize);
   vEncoderName := WideUpperCase(EncoderName);

   if vSize = 0 then Exit;
   GetMem(pImageCodecInfoBase, SizeOf(TImageCodecInfo));
   try
     GetImageEncoders(vNumber, vSize, pImageCodecInfoBase);
     pImageCodecInfoCurr := pImageCodecInfoBase;
     for I := 0 to vNumber - 1 do
     begin
       if WideUpperCase(WideString(pImageCodecInfoCurr^.MimeType)) = vEncoderName then
       begin
         AClsid := pImageCodecInfoCurr^.Clsid;
         Result := I;
         Break;
       end;
       Inc(pImageCodecInfoCurr);
     end;
   finally
     FreeMem(pImageCodecInfoBase);
   end;
end;


class function TsgGDIPlusBase.NewInstance: TObject;
begin
  Result := InitInstance(GdipAlloc(ULONG(instanceSize)));
end;

procedure TsgGDIPlusBase.FreeInstance;
begin
  CleanupInstance;
  GDIPFree(Self);
end;

function ObjectTypeIsValid(Type_: ObjectType): BOOL;
begin
  Result :=  ((Type_ >= ObjectTypeMin) and (Type_ <= ObjectTypeMax));
end;

function MakePoint(X, Y: Integer): TsgGPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MakePointF(X, Y: Single): TsgGPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function PointFToPointI(const APoint: TsgGPointF): TsgGPoint;
begin
  Result.X := Round(APoint.X);
  Result.Y := Round(APoint.Y);
end;

function PointIToPointF(const APoint: TsgGPoint): TsgGPointF;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function MakeSizeF(Width, Height: Single): TsgGSizeF;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

function MakeSize(Width, Height: Integer): TsgGSize;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

function MakeCharacterRange(First, Length: Integer): TCharacterRange;
begin
  Result.First  := First;
  Result.Length := Length;
end;

function MakeRect(const ARect: TRect): TsgGRect; overload;
begin
  Result.X := ARect.Left;
  Result.Y := ARect.Top;
  Result.Width := ARect.Right - ARect.Left;
  Result.Height := ARect.Bottom - ARect.Top;
end;

function MakeRectF(const X, Y, Width, Height: Single): TsgGRectF; overload;
begin
  Result.X      := X;
  Result.Y      := Y;
  Result.Width  := Width;
  Result.Height := Height;
end;

function MakeRectF(Location: TsgGPointF; Size: TsgGSizeF): TsgGRectF; overload;
begin
  Result.X      := Location.X;
  Result.Y      := Location.Y;
  Result.Width  := Size.Width;
  Result.Height := Size.Height;
end;

function GetCenterF(const ARect: TsgGRectF): TsgGPointF; overload;
begin
  Result.X := ARect.X + ARect.Width * 0.5;
  Result.Y := ARect.Y + ARect.Height * 0.5;
end;

function GetTopLeftF(const ARect: TsgGRectF): TsgGPointF; overload;
begin
  Result.X := ARect.X;
  Result.Y := ARect.Y;
end;

function GetBottomRightF(const ARect: TsgGRectF): TsgGPointF; overload;
begin
  Result.X := ARect.X + ARect.Width;
  Result.Y := ARect.Y + ARect.Height;
end;

function MakeRect(X, Y, Width, Height: Integer): TsgGRect; overload;
begin
  Result.X      := X;
  Result.Y      := Y;
  Result.Width  := Width;
  Result.Height := Height;
end;

function MakeRect(Location: TsgGPoint; Size: TsgGSize): TsgGRect; overload;
begin
  Result.X      := Location.X;
  Result.Y      := Location.Y;
  Result.Width  := Size.Width;
  Result.Height := Size.Height;
end;

function MakeRectI(const ARect: TRect): TsgGRect;
begin
  Result.X :=  ARect.Left;
  Result.Y :=  ARect.Top;
  Result.Width := ARect.Right - Arect.Left;
  Result.Height := ARect.Bottom - Arect.Top;
end;

function GetCenter(const ARect: TRect): TsgGPoint; overload;
begin
  Result.X := (ARect.Left + ARect.Right) div 2;
  Result.Y := (ARect.Top + ARect.Bottom) div 2;
end;

function GetCenter(const ARect: TsgGRect): TsgGPoint; overload;
begin
  Result.X := ARect.X + ARect.Width div 2;
  Result.Y := ARect.Y + ARect.Height div 2;
end;

function GetTopLeft(const ARect: TsgGRect): TsgGPoint; overload;
begin
  Result.X := ARect.X;
  Result.Y := ARect.Y;
end;

function GetBottomRight(const ARect: TsgGRect): TsgGPoint; overload;
begin
  Result.X := ARect.X + ARect.Width;
  Result.Y := ARect.Y + ARect.Height;
end;

constructor TsgGDIPPathData.Create;
begin
  Count := 0;
  Points := nil;
  Types := nil;
end;

destructor TsgGDIPPathData.Destroy;
begin
  if Assigned(Points) then FreeMem(Points);
  if Assigned(Types) then FreeMem(Types);
  inherited Destroy;
end;


function GetPixelFormat(AFormat: Graphics.TPixelFormat): TPixelFormat;
begin
  case AFormat of
    pf1bit:  Result := PixelFormat1bppIndexed;
    pf4bit:  Result := PixelFormat4bppIndexed;
    pf8bit:  Result := PixelFormat8bppIndexed;
    pf15bit: Result := PixelFormat16bppRGB555;
    pf16bit: Result := PixelFormat16bppRGB565;
    pf24bit: Result := PixelFormat24bppRGB;
    pf32bit: Result := PixelFormat32bppRGB;
  else
    Result := PixelFormat32bppRGB;
  end;
end;

function GetPixelFormatSize(APixFmt: PixelFormat): UINT;
begin
  Result := (APixFmt shr 8) and $ff;
end;

function IsIndexedPixelFormat(APixFmt: PixelFormat): BOOL;
begin
  Result := (APixFmt and PixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(APixFmt: PixelFormat): BOOL;
begin
  Result := (APixFmt and PixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(APixFmt: PixelFormat): BOOL;
begin
  Result := (APixFmt and PixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(APixFmt: PixelFormat): BOOL;
begin
  Result := (APixFmt and PixelFormatCanonical) <> 0;
end;

function MakeColor(R, G, B: Byte): ARGB; overload;
begin
  Result := MakeColor(255, R, G, B);
end;

function MakeColor(A, R, G, B: Byte): ARGB; overload;
begin
  Result := ((DWORD(b) shl  BlueShift) or (DWORD(g) shl GreenShift) or
    (DWORD(r) shl   RedShift) or (DWORD(a) shl AlphaShift));
end;

function GetAlpha(Color: ARGB): BYTE;
begin
  Result := BYTE(Color shr AlphaShift);
end;

function GetRed(Color: ARGB): BYTE;
begin
  Result := BYTE(Color shr RedShift);
end;

function GetGreen(Color: ARGB): BYTE;
begin
  Result := BYTE(Color shr GreenShift);
end;

function GetBlue(Color: ARGB): BYTE;
begin
  Result := BYTE(Color shr BlueShift);
end;

function ColorRefToARGB(RGB: COLORREF): ARGB;
begin
  Result := MakeColor(255, GetRValue(RGB), GetGValue(RGB), GetBValue(RGB));
end;

function ARGBToColorRef(Color: ARGB): COLORREF;
begin
  Result := RGB(GetRed(Color), GetGreen(Color), GetBlue(Color));
end;

procedure TMetaFileHeader.GetBounds(out Rect: TsgGRect);
begin
  Rect.X      := X;
  Rect.Y      := Y;
  Rect.Width  := Width;
  Rect.Height := Height;
end;

function TMetaFileHeader.IsWmf: BOOL;
begin
  Result :=  ((Type_ = MetaFileTypeWmf) or (Type_ = MetaFileTypeWmfPlaceable));
end;

function TMetaFileHeader.IsWmfPlaceable: BOOL;
begin
  Result := (Type_ = MetaFileTypeWmfPlaceable);
end;

function TMetaFileHeader.IsEmf: BOOL;
begin
  Result := (Type_ = MetaFileTypeEmf);
end;

function TMetaFileHeader.IsEmfOrEmfPlus: BOOL;
begin
  Result := (Type_ >= MetaFileTypeEmf);
end;

function TMetaFileHeader.IsEmfPlus: BOOL;
begin
  Result := (Type_ >= MetaFileTypeEmfPlusOnly)
end;

function TMetaFileHeader.IsEmfPlusDual: BOOL;
begin
  Result := (Type_ = MetaFileTypeEmfPlusDual)
end;

function TMetaFileHeader.IsEmfPlusOnly: BOOL;
begin
  Result := (Type_ = MetaFileTypeEmfPlusOnly)
end;

function TMetaFileHeader.IsDisplay: BOOL;
begin
  Result := (IsEmfPlus and ((EmfPlusFlags and GDIP_EMFPLUSFLAGS_DISPLAY) <> 0));
end;

function TMetaFileHeader.GetWmfHeader: PMetaHeader;
begin
  if IsWmf then
    Result :=  @Header.WmfHeader
  else Result := nil;
end;

function TMetaFileHeader.GetEmfHeader: PENHMETAHEADER3;
begin
  if IsEmfOrEmfPlus then
    Result := @Header.EmfHeader
  else Result := nil;
end;

{ TsgGDIPlusBitmap }

constructor TsgGDIPlusBitmap.Create(FileName: WideString; useEmbeddedColorManagement: BOOL = FALSE);
begin
  NativeImage := nil;
  if UseEmbeddedColorManagement then
    FStatusOfFunction := GDIPCreateBitmapFromFileICM(PWideChar(FileName), NativeImage)
  else
    FStatusOfFunction := GDIPCreateBitmapFromFile(PWideChar(FileName), NativeImage);
end;

constructor TsgGDIPlusBitmap.Create(Stream: IStream; useEmbeddedColorManagement: BOOL  = FALSE);
begin
  NativeImage := nil;
  if UseEmbeddedColorManagement then
    FStatusOfFunction := GDIPCreateBitmapFromStreamICM(Stream, NativeImage)
  else
    FStatusOfFunction := GDIPCreateBitmapFromStream(Stream, NativeImage);
end;

constructor TsgGDIPlusBitmap.Create(Handle: HBITMAP; hPal: HPALETTE);
begin
  NativeImage := nil;
  FStatusOfFunction := GDIPCreateBitmapFromHBITMAP(Handle, hPal, NativeImage);
end;

constructor TsgGDIPlusBitmap.Create(Width, Height, Stride: Integer;
  Format: PIXELFORMAT; scan0: PBYTE);
begin
  NativeImage := nil;
  FStatusOfFunction := GDIPCreateBitmapFromScan0(Width, Height, Stride, Format, scan0, NativeImage);
end;

constructor TsgGDIPlusBitmap.Create(var GDIBitmapInfo: TBitmapInfo;
  GDIBitmapData: Pointer);
begin
  NativeImage := nil;
  FStatusOfFunction := GDIPCreateBitmapFromGdiDib(@GDIBitmapInfo, GDIBitmapData,
    NativeImage);
end;

constructor TsgGDIPlusBitmap.Create(ABitmap: Graphics.TBitmap);
begin
  NativeImage := nil;
  FStatusOfFunction := GdipCreateBitmapFromHBITMAP(ABitmap.Handle, ABitmap.Palette, NativeImage);
end;

procedure DoneGDIPlus;
begin
  if GDIPlusLoaded = 0 then
  begin
    if Assigned(GenericSansSerifFontFamily) then
      GenericSansSerifFontFamily.Free;
    if Assigned(GenericSerifFontFamily) then
      GenericSerifFontFamily.Free;
    if Assigned(GenericMonospaceFontFamily) then
      GenericMonospaceFontFamily.Free;
    if Assigned(GenericTypographicStringFormatBuffer) then
      GenericTypographicStringFormatBuffer.free;
    if Assigned(GenericDefaultStringFormatBuffer) then
      GenericDefaultStringFormatBuffer.Free;
    try
      try
        GdiplusShutdown(GDIPlusToken);
        GDIPlusToken := 0
      except
      end
    finally
      GDIPlusLoaded := -1;
      FreeGDIPlus;
    end
  end;
  GDIPlusLoaded := -1;
end;

initialization

{$IFNDEF SG_DLL}
  InitGDIPlus;
{$ENDIF}

finalization

  DoneGDIPlus;
  
end.
