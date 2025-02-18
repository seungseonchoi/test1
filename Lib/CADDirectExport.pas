{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{              CAD Direct Export interface                   }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit CADDirectExport;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFDEF MSWINDOWS}
  Windows, ActiveX,
{$ENDIF}
{$IFDEF MSWINDOWS}
  sgOle,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types,
{$ELSE}
  Graphics,
{$ENDIF}
  Classes, CADExport, CADImage, DXFConv, sgConsts,
  ExtData, sgFunction, sgBitmap, Math, sgLines, sgLists
{$IFNDEF SG_NO_USE_KERNEL3D}
  , sgModeller, sgModellerIO, sgModExportACIS
{$ENDIF}
{$IFDEF SG_USEGDIPLUS}
{$IFDEF SG_WINAPI_GDIPLUS}
  , Winapi.GDIPAPI, Winapi.GDIPOBJ//, Winapi.GDIPUTIL
{$ELSE}
  , GDIPlus
{$ENDIF}
{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF};

const
  cnstNoAnonimusSVGBlock: Boolean = True;

type
  TsgExportOLEEvent = function(AOLE: TsgDXFOle2Frame; Data: TObject): Boolean of object;
  TsgExportWipeoutEvent = function(AWipeout: TsgCADCustomRectangle; Data: TObject): Boolean of object;
  TsgExportImageEntEvent = TsgExportWipeoutEvent;

  TsgConvertToOle = (ctoConvertAllImageToOle, ctoConverOrtoImageToOle);

  PsgDirectExportData = ^TsgDirectExportData;
  TsgDirectExportData = record
    OwnerHandle: UInt64;
    EntMode: Byte;
    PrevEntHandle: UInt64;
    NextEntHandle: UInt64;
    OwnerDWGCode: Word;
    Owner: TsgDXFEntity;
    EntDWGCode: Word;
    UseEntCode: Boolean;
  end;


  TsgCADDirectExport = class(TsgCADExport)
  private
    FPathDir: string;
    FFileName: string;
    FConvertImageToOLE: Boolean;
    FConvertAcis: Boolean;
    FImageDefList: TList;
    FCADImage: TsgCADImage;
    FVersion: TsgDWGVersion;
    FXDataAppName: string;
    FDrwPropInfo: TStringList;
    FDrwPropCustomSummaryInfo: TStringList;
    FExportImageFileIndex: Integer;
    FOutFileInfo: Boolean;
    FGroupsToFree: TList;
    FEntityEEDItems: TsgObjectCollection;
    FMLeaders: array of TsgDXFEntity;
    function GetDrwPropHyperlinkBase: string;
    function GetDrwPropAuthor: string;
    function GetDrwPropComments: string;
    function GetDrwPropKeywords: string;
    function GetDrwPropSavedBy: string;
    function GetDrwPropSubject: string;
    function GetDrwPropTitle: string;
    function GetDrwPropRevisionNumber: string;
    procedure SetDrwPropHyperlinkBase(const Value: string);
    procedure SetDrwPropAuthor(const Value: string);
    procedure SetDrwPropComments(const Value: string);
    procedure SetDrwPropKeywords(const Value: string);
    procedure SetDrwPropSavedBy(const Value: string);
    procedure SetDrwPropSubject(const Value: string);
    procedure SetDrwPropTitle(const Value: string);
    procedure SetDrwPropRevisionNumber(const Value: string);
    function GetDrwPropCustomSummaryInfo: TStringList;
    procedure SetConvertImageToOLE(const Value: Boolean);
    procedure SetConvertAcis(const Value: Boolean);
    procedure UpdateAcisFlag(const Flag: Boolean);
  protected
    procedure AfterExport(const S: TStream); override;
    procedure BeforeExport(const S: TStream); override;
    procedure ApplyBlocksByProc(AProc: TsgApplyBlocksProc; Data: TObject = nil); virtual;
    procedure BeforeDestroy(var AClearing: Boolean); virtual;
    procedure Clear; virtual;
    procedure ClearImageDefsList; virtual;
    procedure ClearGroupsToFree; virtual;
    function CorrectLineWeight(const ALineWeight: Double; ANeedToConvert: Boolean): Integer; virtual;
    procedure CreateDefaultDrawingProp; virtual;
    function CreateDefaultViewPort(const AHandle: UInt64): TsgDXFViewport;
    function CreateViewPortAsACADWindow(ALayout: TsgDXFLayout;
      AHandle: UInt64 = cnstBadHandle): TsgDXFViewport;
    function CreateDefaultVPort(const Handle: UInt64): TsgDXFVPort;
    function CreateDefaultDimStyle(const Handle: UInt64): TsgDXFDimensionStyle;
    function CreateGroup(const E: TsgDXFEntity): Boolean;
    function DoHandle: UInt64; virtual;
    procedure DoExport(const E: TsgDXFEntity; const Data: PsgDirectExportData = nil); virtual;
    function ExtractName(const AName: string): string;
    function ExportImageEntAsOle(W: TsgDXFImageEnt; ADoOnOLEExport: TsgExportOLEEvent;
      AData: TObject; ADoAssignHandle: Boolean = False): Boolean;
    function ExportImageEntAsExternalFile(W: TsgDXFImageEnt; AHandle: UInt64;
       ADoOnImageEntExport: TsgExportImageEntEvent; AData: TObject): Boolean;
    function ExportGroup(E: TsgDXFEntity; APolyLineDWGCode: Word;
      const AExportData: TsgDirectExportData): Boolean;
    procedure GetCorrectImageVectorData(W: TsgDXFImageEnt;
      var AUVector, AVVector, ASize: TFPoint);
    function GetConverter: TsgDXFConverter; override;
    function GetCorrectBlockName(const AName: string): string; virtual;
    function GetEntityStringValue(ATextEntity: TsgDXFEntity): string;
    function GetModelSpaceName: string; virtual;
    function GetPaperSpaceName: string; virtual;
    function GetPaperLayout: TsgDXFLayout; virtual;
    function GetSpatialFiltersCount: Integer; virtual;
    function GetACISStream(const AEnt: TsgBrepModEntity; out ASaveVer: Word): TMemoryStream;
    function GenLTypeDiscriptText(const ALType: TsgDXFLineType; var Len: Double): string;
    function GetImageDefHandle(W: TsgCADCustomRectangle): UInt64;
    function GetRealCountBoundaryPoints(W: TsgCADCustomRectangle): Integer;
    function GetNextEntHandle(Group: TsgDXFEntity; Index: Integer;
      AList: TsgEntitiesList = nil): UInt64; virtual;
    function GetPrevEntHandle(Group: TsgDXFEntity; Index: Integer;
      AList: TsgEntitiesList = nil): UInt64; virtual;

    procedure InitEntityForExport(const Entity: TsgDXFEntity);
    //GetExportImageIndex returns
    //0 - export as WipeOut
    //1 - export as OLE
    //2 - export as external file
    function GetExportImageIndex(I: TsgDXFImageEnt; AIndex: Integer): Integer;
    function IsNeedAddPointForBoundary(W: TsgCADCustomRectangle): Boolean;
    function IsNeedImageDefHandle(W: TsgCADCustomRectangle): Boolean;

    function IsNameInternal(const AName: string): Boolean;

    function IsPaperLayout(const AName: string): Boolean;
    function GetCorrectBlockFlagsAndName(const ABlock: TsgDXFBlock;
      var AFlags: Byte; var AName: string; Data: Pointer = nil): Boolean;
    procedure CheckSVGBlockFlagsAndName(var AFlags: Byte; var AName: string);

    procedure PrepareDrawingDatabase; virtual;
    procedure PrepareBlock(ABlockRecord: TsgDXFBlockRecord); virtual;
    procedure PrepareOLEHeader(OLE2: TsgDXFOle2Frame;
      const POleHeader: PsgOle2FrameHeaderData);
    procedure ProcessExport(Stream: TStream); virtual;
    procedure SaveToStreamCustom(S: TStream); override;
    procedure SetVersion(const Value: TsgDWGVersion); virtual;
    procedure SetOleFrameVariable(const AValue: Integer); virtual;
    procedure UpdateTimeStamps; virtual;
    procedure UpdateHeadvarExtentsAndLimits; virtual;
    property ExportCADImage: TsgCADImage read FCADImage;
    property ExportImageFileIndex: Integer read FExportImageFileIndex;
    property FileName: string read FFileName;
    property ImageDefList: TList read FImageDefList;
    property PathDir: string read FPathDir;
    property OutFileInfo: Boolean read FOutFileInfo write FOutFileInfo;
    property EntityEEDItems: TsgObjectCollection read FEntityEEDItems;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    procedure ApplyParams(const AExportParams: TsgExportParams); override;
    procedure AddCustomSummaryInfo(const Key, Value: string); virtual;
    procedure DeleteCustomSummaryInfo(const Key: string); virtual;
    property ConvertAcis: Boolean read FConvertAcis write SetConvertAcis;
    property ConvertImageToOLE: Boolean read FConvertImageToOLE write SetConvertImageToOLE;
    property DrwPropAuthor: string read GetDrwPropAuthor write SetDrwPropAuthor;
    property DrwPropComments: string read GetDrwPropComments write SetDrwPropComments;
    property DrwPropHyperlinkBase: string read GetDrwPropHyperlinkBase write SetDrwPropHyperlinkBase;
    property DrwPropKeywords: string read GetDrwPropKeywords write SetDrwPropKeywords;
    property DrwPropRevisionNumber: string read GetDrwPropRevisionNumber write SetDrwPropRevisionNumber;
    property DrwPropSavedBy: string read GetDrwPropSavedBy write SetDrwPropSavedBy;
    property DrwPropSubject: string read GetDrwPropSubject write SetDrwPropSubject;
    property DrwPropTitle: string read GetDrwPropTitle write SetDrwPropTitle;
    property DrwPropCustomSummaryInfo: TStringList read GetDrwPropCustomSummaryInfo;
    property Version: TsgDWGVersion read FVersion write SetVersion;
    property XDataAppName: string read FXDataAppName write FXDataAppName;
  end;

  TsgDWGExportDataClass = class
  private
    FBits: TObject;
    FExportData: TsgDirectExportData;
    FOwnerObject: TObject;
    FGroup: TObject;
    FGroupIndex: Integer;
    function GetPExportData: PsgDirectExportData;
  public
    constructor Create;
    constructor CreateEx(const ABits, AOwnerObject, AGroup: TObject;
      AExportData: TsgDirectExportData);
    property Bits: TObject read FBits write FBits;
    property ExportData: TsgDirectExportData read FExportData write FExportData;
    property Group: TObject read FGroup write FGroup;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property OwnerObject: TObject read FOwnerObject write FOwnerObject;
    property PExportData: PsgDirectExportData read GetPExportData;
  end;

   TsgCADDirectExportClass = class of TsgCADDirectExport;

function CreateExportData(OwnerHandle: UInt64 = cnstBadHandle; EntMode: Byte = 2;
  PrevEntHandle: UInt64 = cnstBadHandle; NextEntHandle: UInt64 = cnstBadHandle;
  OwnerDWGCode: Word = cntDWGObjCodeUNSUSED; Owner: TsgDXFEntity = nil;
  EntDWGCode: Word = cntDWGObjCodeUNSUSED): TsgDirectExportData;

function ConvertImageEntToOle(const AImageEnt: TsgDXFImageEnt;
  const AExportCADImage: TsgCADImage;
  const ADoOnOLEExport: TsgExportOLEEvent; const AData: TObject;
  const ADoAssignHandle: Boolean; const AOleList: TsgObjectList): Boolean;

function HasAsterisk(const AName: string): Boolean;
function IsAnonymousBlock(const AFlags: Byte): Boolean;

implementation

type
  TsgDXFEntityEx = class(TsgDXFEntity);
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgCADImageAccess = class(TsgCADImage);
  TsgBitmapAccess = class(TsgBitmap);
  TsgDXFImageEntAccess = class(TsgDXFImageEnt);
  TsgDXFImageDefAccess = class(TsgDXFImageDef);
  TsgCADMultiLeaderAccess = class(TsgCADMultiLeader);
  TsgDXFBlockRecordAccess = class(TsgDXFBlockRecord);
  TsgBrepModEntityAccess = class(TsgBrepModEntity);

  TsgDXFImageDefExport = class(TsgDXFImageDef)
  end;

const
  cnstDefExportData: TsgDirectExportData = (OwnerHandle: cnstBadHandle; EntMode: 2;
    PrevEntHandle: cnstBadHandle; NextEntHandle: cnstBadHandle; OwnerDWGCode: cntDWGObjCodeUNSUSED;
    Owner: nil; EntDWGCode: cntDWGObjCodeUNSUSED);

function CreateExportData(OwnerHandle: UInt64 = cnstBadHandle; EntMode: Byte = 2;
  PrevEntHandle: UInt64 = cnstBadHandle; NextEntHandle: UInt64 = cnstBadHandle;
  OwnerDWGCode: Word = cntDWGObjCodeUNSUSED; Owner: TsgDXFEntity = nil;
  EntDWGCode: Word = cntDWGObjCodeUNSUSED): TsgDirectExportData;
begin
  Result := cnstDefExportData;
  Result.OwnerHandle := OwnerHandle;
  Result.EntMode := EntMode;
  Result.PrevEntHandle := PrevEntHandle;
  Result.NextEntHandle := NextEntHandle;
  Result.OwnerDWGCode := OwnerDWGCode;
  Result.Owner := Owner;
  Result.EntDWGCode := EntDWGCode;
  Result.UseEntCode := False;
end;

function CreateBitmapGDIPlus(const AImageEnt: TsgDXFImageEnt): TBitmap;
{$IFDEF SG_USEGDIPLUS}
var
  vGr: TGraphic;
  vBmp: TsgBitmapAccess;
  vImg: TsgDXFImageEntAccess;
  vDestRect: TGPRect;
  vGraphics: TGPGraphics;
  vAttr: TGPImageAttributes;
  vTransparentColor: Cardinal;
{$ENDIF}
begin
  Result := nil;
{$IFDEF SG_USEGDIPLUS}
  vImg := TsgDXFImageEntAccess(AImageEnt);
  if not (vImg.Picture.Graphic is TMetafile) then
  begin
    if vImg.FGDIPImage = nil then
      vImg.GDIPCreate;
    if vImg.FGDIPImage <> nil then
    begin
      vGr := vImg.Picture.Graphic;
      vBmp := nil;
      Result := TBitmap.Create;
      if vGr is TsgBitmap then
      begin
        vBmp := TsgBitmapAccess(vGr);
        Result.PixelFormat := vBmp.PixelFormat;
      end;
      SetSizeGraphic(Result, vGr.Width, vGr.Height);
      Result.Canvas.Brush.Color := clWhite;
      Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));

      vDestRect.X := 0;
      vDestRect.Y := 0;
      vDestRect.Width := Result.Width;
      vDestRect.Height :=  Result.Height;

      vGraphics := TGPGraphics.Create(Result.Canvas.Handle);
      try
        // opSpeed
        vGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
        vGraphics.SetInterpolationMode(InterpolationModeNearestNeighbor);
        try
          if Assigned(vBmp) and (vImg.Transparency or (vBmp.FROP <> SRCCOPY)) then
          begin
            vAttr := TGPImageAttributes.Create;
            try
              vTransparentColor := ColorRefToARGB(vImg.TransparentColor);
              vAttr.SetColorKey(vTransparentColor, vTransparentColor,
                ColorAdjustTypeBitmap);

              if vGraphics.DrawImage(vImg.FGDIPImage,
                   vDestRect, 0, 0, vDestRect.Width, vDestRect.Height, {$IFDEF SG_WINAPI_GDIPLUS}UnitPixel, {$ENDIF}vAttr) = OutOfMemory then
                FreeAndNil(Result);
            finally
              vAttr.Free;
            end;
          end
          else
            if vGraphics.DrawImage(vImg.FGDIPImage, vDestRect) = OutOfMemory then
              FreeAndNil(Result);
        except
          FreeAndNil(Result);
        end;
      finally
        vGraphics.Free;
      end;
    end;
  end;
{$ENDIF}
end;

function ConvertImageEntToOle(const AImageEnt: TsgDXFImageEnt;
  const AExportCADImage: TsgCADImage;
  const ADoOnOLEExport: TsgExportOLEEvent; const AData: TObject;
  const ADoAssignHandle: Boolean; const AOleList: TsgObjectList): Boolean;
{$IFDEF MSWINDOWS}
var
  W: TsgDXFImageEntAccess absolute AImageEnt;
  vOle: TsgDXFOle2Frame;
  vAngle, vAngleY: Double;
  R: Integer;
  vBitmap: TsgBitmapAccess;
  vBitmapBig, vBitmapSmall: TBitmap;
  I, J: Integer;
  vRect: TRect;
{$IFDEF SG_DELPHI_VCL}
  vMetafile: TMetafile;
  vMetafileCanvas: TMetafileCanvas;
{$ENDIF}
  vChunk, vDst: TPoint;
  vROP: Integer;
  vImageHeight, vImageWidth: TFPoint;
  vCountParts: TPoint;
  vDpi: TFPoint;
  vSingX, vSingY: Integer;
  vR: TFRect;
  vFormat: TClipFormat;
  vGr: {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TPersistent{$ENDIF};
  vPictureUpdate: Boolean;
  vBitmapGDIPlus: TBitmap;

  procedure ExportOle(const AGraphic: TPersistent; const AFormat: TClipFormat;
    ASmall, ABitmapSmall: TPoint;
    ADpi: TFPoint; ACol, ARow: Integer);
  var
    vTempPoint, vTempPoint1: TFPoint;
    vIndex: Integer;
    vDWGData: TsgDWGExportDataClass;
  begin
    vOle := TsgDXFOle2Frame.Create;
    try
      vOle.AssignEntity(W);
      if (AData <> nil) and (AData is TsgDWGExportDataClass) then
        vDWGData := TsgDWGExportDataClass(AData)
      else
        vDWGData := nil;
      if ADoAssignHandle then
      begin
        if vDWGData <> nil then
          vIndex := vDWGData.FGroupIndex
        else
          vIndex := 0;
        if W.GroupExists then
          vOle.Handle := W.Group[vIndex].Handle
        else
          vOle.Handle := W.Handle;
      end;
      vOle.Aspect := DVASPECT_CONTENT;
      vOle.BinaryData := CreateSTGMFromImage(AGraphic, AFormat);

      vTempPoint := vR.TopLeft;

      vTempPoint.X := vTempPoint.X + ACol * ASmall.X * ADpi.X;
      vTempPoint.Y := vTempPoint.Y - (ARow * ASmall.Y + ABitmapSmall.Y) * ADpi.Y;

      vTempPoint1.X := vTempPoint.X + ABitmapSmall.X * ADpi.X;
      vTempPoint1.Y := vTempPoint.Y + ABitmapSmall.Y * ADpi.Y;

      vOle.Point := vTempPoint;
      vOle.Point1 := vTempPoint1;

      if Assigned(ADoOnOLEExport) then
        Result := ADoOnOLEExport(vOle, AData);
      if vDWGData <> nil then
        Inc(vDWGData.FGroupIndex);
    finally
      if Assigned(AOleList) then
        AOleList.Add(vOle)
      else
        vOle.Free;
    end;
  end;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := True;
  vPictureUpdate := W.PictureUpdate(AExportCADImage);
  vBitmapGDIPlus := nil;
  try
    if W.Empty then Exit;

    try
      vBitmapGDIPlus := CreateBitmapGDIPlus(W);
    except
      vBitmapGDIPlus := nil;
    end;

    if Assigned(vBitmapGDIPlus) then
      vGr := vBitmapGDIPlus
    else
      vGr := W.Picture.Graphic;

    vImageWidth := SubFPoint(W.Point1, W.Point);
    vImageHeight := SubFPoint(W.Point2, W.Point);

    vAngle := GetAngleByPoints(W.Point, W.Point1,
      False, fDoubleResolution ,1, 1);
    vAngleY :=  GetAngleByPoints(W.Point, W.Point2,
      False, fDoubleResolution, 1, 1);

    if sgIsZero(vAngle - 0) then
      vAngle := 0;
    if sgIsZero(vAngleY - 90) then
      vAngleY := 90;

    vR := MakeFRectByPoints( W.Point, W.Point);
    ExpandFRect(vR, W.Point1);
    ExpandFRect(vR, W.Point2);
    ExpandFRect(vR, W.Point3);

    vSingX := 1;
    vSingY := 1;

    if vGr is TBitmap then
      vDpi := MakeFPoint(vSingX * (DistanceFVector(vImageWidth) / TBitmap(vGr).Width),
        vSingY * (DistanceFVector(vImageHeight) / TBitmap(vGr).Height))
    else
      vDpi := MakeFPoint(vSingX * (DistanceFVector(vImageWidth) / TGraphic(vGr).Width),
        vSingY * (DistanceFVector(vImageHeight) / TGraphic(vGr).Height));
{$IFDEF SG_DELPHI_VCL}
    if vGr is TMetafile then
    begin
      vFormat := CF_ENHMETAFILE;
      if CheckExportOLEAsMetafile(TMetafile(vGr).Handle) then
      begin
        vChunk := Point(vGr.Width, vGr.Height);
        ExportOle(vGr, vFormat, vChunk, vChunk, vDpi, 0, 0);
        Exit;
      end;
    end
    else
{$ENDIF}
      if (vGr.ClassType = TBitmap) or (vGr is TsgBitmap) then
        vFormat := CF_BITMAP
      else
        vFormat := CF_METAFILEPICT;

    vBitmap := TsgBitmapAccess(TsgBitmap.Create);
    try
      vBitmap.Assign(vGr);
      if IsEqual((Round(vAngle) div 90) * 90, vAngle) then
      begin
        if (vAngle = 0) and (vAngleY = 90) then
        begin
        end
        else
        begin
          R := (Round(vAngle) div 90) * 90;
          if R <> 0 then
          begin
            vBitmap.Rotate(360-R);
          end
          else
            if (vAngle = 0) and (vAngleY = 270) then
              vBitmap.Flip;
        end;
      end;

      vBitmapBig := TBitmap.Create;
      try
        vBitmapBig.PixelFormat := vBitmap.PixelFormat;
        SetSizeGraphic(vBitmapBig, vBitmap);
        vROP := vBitmap.FROP;
        try
          vBitmap.FROP := SRCCOPY;
          vBitmapBig.Canvas.StretchDraw(vBitmapBig.Canvas.ClipRect, vBitmap);
          if vFormat in [CF_ENHMETAFILE, CF_METAFILEPICT] then
          begin
            vDpi := MakeFPoint(vSingX * (DistanceFVector(vImageWidth) / vBitmapBig.Width),
              vSingY * (DistanceFVector(vImageHeight) / vBitmapBig.Height));
          end;
        finally
          vBitmap.FROP := vROP;
        end;

        vBitmapSmall := TBitmap.Create;
        try
          Result := True;
          vBitmapSmall.PixelFormat := vBitmapBig.PixelFormat;

          vCountParts := GetCountBlocksForOLEExport(vBitmapBig.Width, vBitmapBig.Height,
            vBitmapBig.PixelFormat, vChunk);

          for I := 0 to vCountParts.X do
            for J := 0 to vCountParts.Y do
            begin
              vRect.Left := I * vChunk.X;
              vRect.Top := J * vChunk.Y;
              vRect.Right := vRect.Left + vChunk.X;
              vRect.Bottom := vRect.Top + vChunk.Y;
              if I = vCountParts.X then
                vRect.Right := vRect.Left + vBitmapBig.Width - I * vChunk.X;
              if J = vCountParts.Y then
                vRect.Bottom := vRect.Top + vBitmapBig.Height - J * vChunk.Y;

              vDst.X := vRect.Right - vRect.Left;
              vDst.Y := vRect.Bottom - vRect.Top;
              SetSizeGraphic(vBitmapSmall, vDst.X, vDst.Y);
            {$IFDEF SG_FIREMONKEY}
              try
                vBitmapSmall.Canvas.Beginscene;
                vBitmapSmall.Canvas.DrawBitmap(vBitmapBig, vRect,
                  TRectF.Create(0, 0, vDst.X, vDst.Y), 1);
               finally
                 vBitmapSmall.Canvas.Endscene;
               end;
            {$ELSE}
              BitBlt(vBitmapSmall.Canvas.Handle, 0, 0, vDst.X, vDst.Y,
                vBitmapBig.Canvas.Handle, vRect.Left, vRect.Top, SRCCOPY);
            {$ENDIF}
{$IFDEF SG_DELPHI_VCL}
              if vFormat in [CF_ENHMETAFILE, CF_METAFILEPICT] then
              begin
                vMetafile := TMetafile.Create;
                try
                  SetSizeGraphic(vMetafile, vDst.X, vDst.Y);
                  vMetafileCanvas := TMetafileCanvas.Create(vMetafile, 0);
                  try
                    BitBlt(vMetafileCanvas.Handle, 0, 0, vDst.X, vDst.Y,
                      vBitmapSmall.Canvas.Handle, 0, 0, vROP);
                  finally
                    vMetafileCanvas.Free;
                  end;
                  ExportOle(vMetafile, vFormat, vChunk, vDst, vDpi, I, J);
                finally
                  vMetafile.Free;
                end;
              end
              else
{$ENDIF}
                ExportOle(vBitmapSmall, vFormat, vChunk, vDst, vDpi, I, J);
            end;
        finally
          vBitmapSmall.Free;
        end;
      finally
        vBitmapBig.Free;
      end;
    finally
      vBitmap.Free;
    end;
  finally
    if vPictureUpdate then
      W.PictureUpdate(nil);
    if Assigned(vBitmapGDIPlus) then
      vBitmapGDIPlus.Free;
  end;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function HasAsterisk(const AName: string): Boolean;
begin
  Result := (AName <> '') and (AName[1] = cnstAsterisk);
end;

function IsAnonymousBlock(const AFlags: Byte): Boolean;
begin
  Result := AFlags and 1 <> 0;
end;

{ TsgCADDirectExport }

procedure TsgCADDirectExport.AddCustomSummaryInfo(const Key, Value: string);
begin
  if (Key <> '') and (Value <> '') and (FDrwPropCustomSummaryInfo.Count < 10) then
    FDrwPropCustomSummaryInfo.Values[Key] := Value;
end;

procedure TsgCADDirectExport.UpdateAcisFlag(const Flag: Boolean);
var
  I, J, vBlockRecordsCount: Integer;
  vBlockRecords: TsgDXFBlockRecords;
  vBlockRecord: TsgDXFBlockRecordAccess;
begin
  vBlockRecords := TsgDXFBlockRecords(Converter.Sections[csBlockRecords]);
  if Assigned(vBlockRecords) then
  begin
    vBlockRecordsCount := vBlockRecords.Count;
    for I := 0 to vBlockRecordsCount - 1 do
    begin
      vBlockRecord := TsgDXFBlockRecordAccess(vBlockRecords[I]);
      for J := 0 to vBlockRecord.Block.Count - 1 do
      begin
        if vBlockRecord.Block[J] is TsgBrepModEntity then
        begin
          TsgBrepModEntityAccess(vBlockRecord.Block[J]).IsGenerateGroup := Flag;
          TsgBrepModEntityAccess(vBlockRecord.Block[J]).IsGenerate3DFaceByGroup := Flag;
        end;
      end;
    end;
  end;
end;

procedure TsgCADDirectExport.BeforeExport(const S: TStream);
begin
  if ConvertAcis then
    UpdateAcisFlag(ConvertAcis);
end;

procedure TsgCADDirectExport.AfterExport(const S: TStream);
var
  I: Integer;
  vRenderObject: TsgDXFInsert;
  vBlockRecords: TsgDXFBlockRecords;
  vBlockRecord: TsgDXFBlockRecordAccess;
begin
  inherited AfterExport(S);
  if Version < acR2018 then
    TsgDXFConverterAccess(Converter).CompositeMLAtt;
  vBlockRecords := TsgDXFBlockRecords(Converter.Sections[csBlockRecords]);
  for I := High(FMLeaders) downto 0 do
    if TsgCADMultiLeaderAccess(FMLeaders[I]).RenderObject is TsgDXFInsert then
    begin
      vRenderObject := TsgDXFInsert(TsgCADMultiLeaderAccess(FMLeaders[I]).RenderObject);
      vBlockRecord := TsgDXFBlockRecordAccess(vRenderObject.BlockRecord);
      vBlockRecords.RemoveEntity(vBlockRecord);
      vBlockRecord.SetOwner(vRenderObject);
      TsgDXFEntityEx(vBlockRecord.Block).SetOwner(vRenderObject);
    end;
  SetLength(FMLeaders, 0);
  //Invoke cleanup because the flag affects the behavior of the function
  ClearGroupsToFree;
  UpdateAcisFlag(False);
end;

procedure TsgCADDirectExport.ApplyBlocksByProc(AProc: TsgApplyBlocksProc;
  Data: TObject = nil);
var
  I: Integer;
  vBlock: TsgDXFBlock;
  vName, vNameUC: string;
begin
  for I := 0 to Converter.Counts[csBlocks] - 1 do
  begin
    vBlock := Converter.Blocks[I];
    if vBlock.Layout = nil then
    begin
      vName := GetCorrectBlockName(vBlock.Name);
      vNameUC := AnsiUpperCase(vName);//for quick names comparing
      if (vNameUC <> GetModelSpaceName) and (vNameUC <> GetPaperSpaceName) and
        (not IsNameInternal(vNameUC)) then
      begin
        if vName = '' then
          vName := IntToStr(TsgNativeUInt(vBlock));
        if not Assigned(FSGBlock) or (vNameUC <> AnsiUpperCase(FSGBlock.Name)) then
          AProc(vBlock, vName, Data, -1);
      end;
    end;
  end;
  if Assigned(FSGBlock) then
    AProc(FSGBlock, FSGBlock.Name, Data);
  for I := 1 to Converter.LayoutsCount - 1 do
    if ExportCADImage.Layouts[I] <> GetPaperLayout then
      AProc(ExportCADImage.Layouts[I].PaperSpaceBlock, sPaperSpace + IntToStr(I), Data, I);
end;

procedure TsgCADDirectExport.ApplyParams(const AExportParams: TsgExportParams);
begin
  inherited ApplyParams(AExportParams);
  Version := TsgDWGVersion(AExportParams.Version);
  DrwPropTitle := AExportParams.Title;
  DrwPropAuthor := AExportParams.Author;
  DrwPropKeywords := AExportParams.Keywords;
  DrwPropSubject := AExportParams.Subjct;
  ConvertImageToOLE := AExportParams.IsConvertImageToOLE;
  ConvertAcis := AExportParams.ConvertAcis;
end;

procedure  TsgCADDirectExport.ClearGroupsToFree;
var
  I: Integer;
begin
  for I := 0 to FGroupsToFree.Count - 1 do
    TsgDXFEntityEx(FGroupsToFree[I]).GroupClear;
  FGroupsToFree.Clear;
end;

procedure TsgCADDirectExport.Clear;
begin
  ClearImageDefsList;
  FExportImageFileIndex := 1;
  ClearGroupsToFree;
end;

procedure TsgCADDirectExport.ClearImageDefsList;
var
  I: Integer;
begin
  for I := 0 to FImageDefList.Count - 1 do
  begin
    TsgDXFImageDefAccess(FImageDefList[I]).PictureRef := nil;
    TsgDXFImageDefAccess(FImageDefList[I]).Free;
  end;
  FImageDefList.Clear;
end;

function TsgCADDirectExport.CorrectLineWeight(const ALineWeight: Double;
  ANeedToConvert: Boolean): Integer;
begin
  Result := 5;
end;

constructor TsgCADDirectExport.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  FCADImage := ACADImage;
  FImageDefList := TList.Create;
  FOutFileInfo := True;
  FConvertImageToOLE := True;
  FConvertAcis := False;
  FVersion := acR2000;
  FDrwPropInfo := TStringList.Create;
  FDrwPropCustomSummaryInfo := TStringList.Create;
  FGroupsToFree := TList.Create;
  FEntityEEDItems := TsgObjectCollection.Create;
  PrepareDrawingDatabase;
  UpdateHeadvarExtentsAndLimits;
end;

function TsgCADDirectExport.CreateDefaultDimStyle(const Handle: UInt64): TsgDXFDimensionStyle;
var
  vHeadVar: TsgHeadVarStruct;
begin
  Result := TsgDXFDimensionStyle.Create;
  vHeadVar := Converter.HeadVarStruct;
  Result.InitProps(@vHeadVar);
  Result.Handle := Handle;
  Result.DIMTIH := True;
  Result.DIMTOH := True;
  Result.DIMSE1 := False;
  Result.DIMSE2 := False;
  Result.SizeCenterMark := 0;
  Result.DIMSD1 := False;
  Result.DIMSD2 := False;
  Result.TextStyle := Converter.StyleByName(vHeadVar.TextStyle);//!!!
end;

procedure TsgCADDirectExport.CreateDefaultDrawingProp;
begin
  FDrwPropInfo.Clear;
  FDrwPropCustomSummaryInfo.Clear;
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropTitle)] := cnstCompanyCST;
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropSubject)] := '';
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropAuthor)] := '';
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropComments)] := sExporterSoftwareInfo;
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropKeywords)] := '';
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropLastSavedBy)] := TsgCADImageAccess(ExportCADImage).FileInfo;
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropRevisionNumber)] := '';
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropHyperlinkBase)] := '';
end;

function TsgCADDirectExport.CreateDefaultViewPort(const AHandle: UInt64): TsgDXFViewport;
begin
  Result := TsgCADImageAccess(ExportCADImage).CreateDefaultViewPort(AHandle);
end;

function TsgCADDirectExport.CreateDefaultVPort(const Handle: UInt64): TsgDXFVPort;
begin
  Result := TsgCADImageAccess(ExportCADImage).CreateDefaultVPort(Handle);
end;

function TsgCADDirectExport.CreateGroup(const E: TsgDXFEntity): Boolean;
var
  vEnt: TsgDXFEntityEx;
begin
  Result := False;
  vEnt := TsgDXFEntityEx(E);
  if vEnt = nil then Exit;
  InitEntityForExport(E);
  if vEnt.Group = nil then
  begin
    Result := vEnt.GroupCreate(Converter);
    if Result then
      FGroupsToFree.Add(vEnt);
  end
  else
    Result := vEnt.Group.Count > 0;
end;

function TsgCADDirectExport.CreateViewPortAsACADWindow(ALayout: TsgDXFLayout;
  AHandle: UInt64): TsgDXFViewport;
var
  vBox: TFRect;
begin
  Result := TsgDXFViewPort.Create;
  Result.Handle := AHandle;
  vBox := ALayout.Box;
  Result.PSpaceCenter := MakeFPoint(0.5 * (vBox.Right + vBox.Left), 0.5 * (vBox.Top + vBox.Bottom), 0);
  Result.PSpaceWidth := vBox.Right - vBox.Left;
  Result.PSpaceHeight := vBox.Top - vBox.Bottom;
  Result.StatusField := 1;
  Result.ThisID := 1;
  Result.MSpaceCenter := Result.PSpaceCenter;
  Result.ViewDirection := MakeFPoint(0, 0, 1);
  Result.ViewTarget := cnstFPointZero;
  Result.Flags := 32800;
  Result.FrontClipPlane := 0;
  Result.BackClipPlane := 0;
  Result.MSpaceHeight := Result.PSpaceHeight;
  Result.ViewTwistAngle := 0;
end;

procedure TsgCADDirectExport.DeleteCustomSummaryInfo(const Key: string);
begin
  FDrwPropCustomSummaryInfo.Values[Key] := '';
end;

procedure TsgCADDirectExport.BeforeDestroy(var AClearing: Boolean);
begin
end;

destructor TsgCADDirectExport.Destroy;
var
  vClear: Boolean;
begin
  vClear := True;
  BeforeDestroy(vClear);
  if vClear then
    Clear;
  FImageDefList.Free;
  FDrwPropInfo.Free;
  FDrwPropCustomSummaryInfo.Free;
  FGroupsToFree.Free;
  FEntityEEDItems.Free;
  inherited Destroy;
end;

procedure TsgCADDirectExport.DoExport(const E: TsgDXFEntity;
  const Data: PsgDirectExportData);
begin

end;

function TsgCADDirectExport.DoHandle: UInt64;
begin
  Result := cnstBadHandle;
  TsgDXFConverterAccess(Converter).CheckHandle(Result);
end;

function TsgCADDirectExport.ExportGroup(E: TsgDXFEntity; APolyLineDWGCode: Word;
  const AExportData: TsgDirectExportData): Boolean;
var
  vPrevEntHandle, vNextEntHandle, vNextEntHandleSaved: UInt64;
  I: Integer;
  vGroup: TsgDXFGroup;
  vExportData: TsgDirectExportData;
//  vVertex: TsgDXFVertex;
  vDXFPolyline: TsgDXFPolyline;
//  vBasePolyLine: TsgCADBasePolyline;
  vExpEnt: TsgDXFEntity;
begin
  Result := False;
  if E = nil then Exit;
  try
    CreateGroup(E);
    vGroup := TsgDXFEntityEx(E).Group;
    DoError(vGroup = nil, sGroupIsNotSupported);
    vNextEntHandleSaved := AExportData.NextEntHandle;
    for I := 0 to vGroup.Count - 1 do
    begin
      if I = 0 then
        vPrevEntHandle := AExportData.PrevEntHandle
      else
        vPrevEntHandle := GetPrevEntHandle(vGroup, I);
      if I < vGroup.Count - 1 then
        vNextEntHandle := GetNextEntHandle(vGroup, I)
      else
        vNextEntHandle := vNextEntHandleSaved;
      vExportData := CreateExportData(AExportData.OwnerHandle, AExportData.EntMode,
        vPrevEntHandle, vNextEntHandle, AExportData.OwnerDWGCode, AExportData.Owner,
        AExportData.EntDWGCode);
      vDXFPolyline := nil;
      vExpEnt := vGroup.Entities[I];
      DoExport(vExpEnt, @vExportData);
      vDXFPolyline.Free;
    end;
    Result := vGroup.Count > 0;
  finally
    if FGroupsToFree.IndexOf(E) < 0 then
      TsgDXFEntityEx(E).GroupClear;
  end;
end;

function TsgCADDirectExport.ExportImageEntAsExternalFile(W: TsgDXFImageEnt;
  AHandle: UInt64; ADoOnImageEntExport: TsgExportImageEntEvent;
  AData: TObject): Boolean;
const
  cnstExtSupportAutoCAD: array [0..8] of string =
    ('.BMP', '.PNG', '.JPG', '.TIF', '.JPEG', '.GIF', '.PCX', '.EMF', '.WMF');
var
  vImgDef, vImgDefStore: TsgDXFImageDefAccess;
  vFileNameImage: string;
  vDir, vPathDir, vExt: string;
begin
  Result := False;
  if W.Empty then Exit;
  //Save Image
  Inc(FExportImageFileIndex);
  vFileNameImage := FileName + '_' + IntToStr(FExportImageFileIndex);
  vPathDir := PathDir;
  if (vPathDir <> '') and (vPathDir[Length(vPathDir)] = PathDelim) then
    Delete(vPathDir, Length(vPathDir), 1);
  vDir := vPathDir + PathDelim + FileName;
  if not DirectoryExists(vDir) then
  begin
    try
      Result := CreateDir(vDir);
    except
      Exit;
    end;
  end;
  try
    if (W.Picture.Graphic.ClassType = TBitmap) or
       (W.Picture.Graphic is TsgBitmap) then
    begin
    {$IFDEF SG_FIREMONKEY}
      vExt := '.png';
    {$ELSE}
      vExt := '.BMP';
    {$ENDIF}

    end
    else
    begin
      vExt := GraphicExtension(TGraphicClass(W.Picture.Graphic.ClassType));
      if (Length(vExt) > 0) and (vExt[1] <> '.') then
        vExt := '.' + vExt;
    end;
    if Length(vExt) = 0 then
      Exit;
    if StrIndex(UpperCase(vExt), cnstExtSupportAutoCAD) > High(cnstExtSupportAutoCAD) then
    begin
      // Conver to support format.
      Exit;
    end;

    vFileNameImage := vFileNameImage+vExt;
    W.Picture.SaveToFile(vDir + PathDelim + vFileNameImage);
  except
    Exit;
  end;
  vImgDef := TsgDXFImageDefAccess(TsgDXFImageDefExport.Create);
  try
    vImgDef.FileName := '.\' + FileName + '\' + vFileNameImage;
    vImgDef.Handle := AHandle;
    vImgDef.PictureRef.Free;
    vImgDef.PictureRef := W.Picture; // PictureRef - released on ClearImageDefsList
    vImgDefStore := TsgDXFImageDefAccess(TsgDXFImageEntAccess(W).ImageDef);
    try
      TsgDXFImageEntAccess(W).ImageDefRef := vImgDef;
      if Assigned(ADoOnImageEntExport) then
        Result := ADoOnImageEntExport(W, AData);
    finally
      TsgDXFImageEntAccess(W).ImageDefRef := vImgDefStore;
    end;
  finally
    if Result then
      ImageDefList.Add(vImgDef)
    else
      vImgDef.Free;
  end;
end;

function TsgCADDirectExport.ExportImageEntAsOle(W: TsgDXFImageEnt;
  ADoOnOLEExport: TsgExportOLEEvent; AData: TObject;
  ADoAssignHandle: Boolean): Boolean;
begin
  Result := True;
  if ConvertImageEntToOle(W, ExportCADImage, ADoOnOLEExport, AData,
     ADoAssignHandle, nil) then
    SetOleFrameVariable(0);
end;

function TsgCADDirectExport.ExtractName(const AName: string): string;
const
  cnstSharp = '#';
  cnstSpace = ' ';
  cnstAsk = '?';
begin
  Result := AName;
  if Ord(Version) <= Ord(acR14) then
  begin
    if AnsiPos(cnstSharp, Result) > 0 then
      ReplaceAnsi(Result, cnstSharp, '_');
    if AnsiPos(cnstSpace, Result) > 0 then
      ReplaceAnsi(Result, cnstSpace, '_');
  end;
  if AnsiPos(cnstAsk, Result) > 0 then
    ReplaceAnsi(Result, cnstAsk, '_');
end;

function TsgCADDirectExport.GenLTypeDiscriptText(const ALType: TsgDXFLineType;
  var Len: Double): string;
var
  I, vCount: Integer;
  vThick: Double;
  vElement: TsgLTypeElement;
begin
  vCount := ALType.Lines.ElementsCount;
  Len := 0;
  Result := '';
  for I := 0 to vCount - 1 do
  begin
    vElement := ALType.Lines.Elements[I];
    vThick := vElement.Dash;
    Len := Len + Abs(vThick);
    if vThick > 0 then
      Result := Result + '_'
    else
      if vThick = 0 then
        Result := Result + '.'
      else
        Result := Result + ' ';
  end;
end;

procedure TsgCADDirectExport.GetCorrectImageVectorData(W: TsgDXFImageEnt;
      var AUVector, AVVector, ASize: TFPoint);
var
  vKoef, vSize: TFPoint;
  vMatrix: TFMatrix;
begin
  if IsNeedImageDefHandle(W) and (TsgDXFImageEntAccess(W).ImageDef.ClassType = TsgDXFImageDefExport) then
  begin
    ASize := TsgDXFImageEntAccess(W).ImageDef.Size;
    if (ASize.X = 0) or (ASize.Y = 0) then//incorrect value
      ASize := TsgDXFImageEntAccess(W).Size;//ASize := cnstFPointSingle;
    vSize := TsgDXFImageEntAccess(W).Size;
    // vKof is a ratio of visualization size to the original size
    vKoef := MakeFPoint(vSize.X/ASize.X, vSize.Y/ASize.Y, 1);
    vMatrix := StdMat(vKoef, cnstFPointZero);
    AUVector := FPointXMat(W.UVector, vMatrix);
    AVVector := FPointXMat(W.VVector, vMatrix);
  end
  else
  begin
    AUVector := W.UVector;
    AVVector := W.VVector;
    ASize := W.Size;
  end;
end;

function TsgCADDirectExport.GetACISStream(const AEnt: TsgBrepModEntity; out ASaveVer: Word): TMemoryStream;
{$IFNDEF SG_NO_USE_KERNEL3D}
var
  vBinary: Boolean;
  vAppName: AnsiString;
  vModeller: TsgModeller;
  vExporter: TsgModExportACIS;
{$ENDIF}
begin
  Result := nil; ASaveVer := 0;
{$IFNDEF SG_NO_USE_KERNEL3D}
  vModeller := Converter.QueryModeller(False);
  if vModeller = nil then
    Exit;
  Result := TMemoryStream.Create;
  vExporter := TsgModExportACIS.Create(vModeller);
  try
    vAppName := AnsiString(AppName);
    if vAppName = '' then
      vAppName := 'Unknown';
    //vExporter.AppName := vAppName;
    //vExporter.KernelName := cnstKernel3dName;
    //vExporter.Date := ACISDateTimeName(Now);
    //vExporter.MMPerUnit := Double
    //vExporter.ResAbs := Double
    //vExporter.ResNorm := Double
    //vExporter.WriteTclColors;
    vExporter.WriteAdskColors := True;
    if Version <= acR2000 then
      vExporter.WriteAdskIndexColors := True;

    case Version of
    acR09, acR10, acR11, acR12, //!! ACIS not Supported
    acR13, acR14: vExporter.ACISVersion := 106;
    acR2000:      vExporter.ACISVersion := 400;
    {acR2004:      vExporter.Version := 20800;
    acR2007:      vExporter.Version := 21200;
    acR2010:      vExporter.Version := 21500;
    acR2013:      vExporter.Version := 21800;
    acR2018:      vExporter.Version := 22300;}
    else
      vExporter.ACISVersion := 400;
    end;
    if ExportType = efDwg then
      vBinary := (FVersion >= acR2004)
    else
      vBinary := (FVersion >= acR2013);
    if vBinary then
    begin
      ASaveVer := 2;
      vExporter.WriteEntCount := False;
    end
    else
    begin
      ASaveVer := 1;
      vExporter.WriteEntCount := True;
    end;
    vExporter.IsBinary := vBinary;

    vExporter.WriteRowNumbers := False;
    //vExporter.WriteExtents3d := False;
    //vExporter.WriteExtents2d := False;

    if not vExporter.SaveToStream(AEnt.Compound, Result) then
      FreeAndNil(Result)
    else if Result.Size = 0 then
      FreeAndNil(Result);
  finally
    vExporter.DropParser;
    vExporter.Free;
  end;
{$ENDIF}
end;

function TsgCADDirectExport.GetConverter: TsgDXFConverter;
begin
  Result := FCADImage.Converter;
end;

function TsgCADDirectExport.GetCorrectBlockName(const AName: string): string;
var
  vPos: Integer;
begin
  if HasInternalDimensionBlockName(AName, @vPos) then
    Result := sInternalDimension + '0' + Copy(AName, vPos + Length(sInternalDimension), MaxInt)
  else
    Result := AName;
end;

function TsgCADDirectExport.GetDrwPropHyperlinkBase: string;
begin
  Result := FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropHyperlinkBase)];
end;

function TsgCADDirectExport.GetDrwPropAuthor: string;
begin
  Result := FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropAuthor)];
end;

function TsgCADDirectExport.GetDrwPropComments: string;
begin
  Result := FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropComments)];
end;

function TsgCADDirectExport.GetDrwPropCustomSummaryInfo: TStringList;
begin
  Result := FDrwPropCustomSummaryInfo;
end;

function TsgCADDirectExport.GetDrwPropKeywords: string;
begin
  Result := FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropKeywords)];
end;

function TsgCADDirectExport.GetDrwPropSavedBy: string;
begin
  Result := FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropLastSavedBy)];
end;

function TsgCADDirectExport.GetDrwPropSubject: string;
begin
  Result := FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropSubject)];
end;

function TsgCADDirectExport.GetDrwPropTitle: string;
begin
  Result := FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropTitle)];
end;

function TsgCADDirectExport.GetDrwPropRevisionNumber: string;
begin
  Result := FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropRevisionNumber)];
end;

function TsgCADDirectExport.GetEntityStringValue(ATextEntity: TsgDXFEntity): string;
begin
   case ATextEntity.EntType of
    ceAttdef, ceAttrib:
      Result := TsgDXFAttdef(ATextEntity).Value;
    ceText:
      Result := TsgDXFText(ATextEntity).Text;
    ceMText:
      Result := TsgDXFMText(ATextEntity).Text;
    else
      Result := '';
  end;
end;

function TsgCADDirectExport.GetExportImageIndex(I: TsgDXFImageEnt;
  AIndex: Integer): Integer;
begin
  Result := DXFConv.GetExportImageIndex(I, AIndex, ConvertImageToOLE);
end;

function TsgCADDirectExport.GetImageDefHandle(W: TsgCADCustomRectangle): UInt64;
begin
  if IsNeedImageDefHandle(W) then
    Result := TsgDXFImageEnt(W).ImageDef.Handle
  else
    Result := cnstBadHandle;
end;

function TsgCADDirectExport.GetModelSpaceName: string;
begin
  if Ord(Version) >= Ord(acR14) then
    Result := sModelSpace
  else
    Result := sModelSpace12;
end;

function TsgCADDirectExport.GetNextEntHandle(Group: TsgDXFEntity;
  Index: Integer; AList: TsgEntitiesList): UInt64;
begin
  Result := cnstBadHandle;
end;

function TsgCADDirectExport.GetPaperLayout: TsgDXFLayout;
begin
  Result := nil;
  if ExportCADImage.CurrentLayout <> ExportCADImage.Layouts[0] then
    Result := ExportCADImage.CurrentLayout
  else
    if ExportCADImage.LayoutsCount > 1 then
      Result := ExportCADImage.Layouts[1];
end;

function TsgCADDirectExport.GetPaperSpaceName: string;
begin
  if Ord(Version) >= Ord(acR14) then
    Result := sPaperSpace
  else
    Result := sPaperSpace12;
end;

function TsgCADDirectExport.GetPrevEntHandle(Group: TsgDXFEntity;
  Index: Integer; AList: TsgEntitiesList): UInt64;
begin
  Result := cnstBadHandle;
end;

function TsgCADDirectExport.GetRealCountBoundaryPoints(
  W: TsgCADCustomRectangle): Integer;
begin
  Result := W.ClipPointsCount;
  if IsNeedAddPointForBoundary(W) then Inc(Result);
end;

function TsgCADDirectExport.GetSpatialFiltersCount: Integer;
var
  I, C: Integer;
  vHashList: PsgHashItem;
  Item: TsgTableItem;
  vEnt: TsgDXFEntity;
begin
  Result := 0;
  C := TsgDXFConverterAccess(Converter).EntityDictionaries.Count;
  if C > 0 then
  begin
    vHashList := @TsgDXFConverterAccess(Converter).EntityDictionaries.List^[0];
    I := 0;
    while I < C do
    begin
      Item := vHashList^.Data;
      Inc(Result, Ord(HasSpatialFilter(Item.Item, vEnt)));
      Inc(vHashList);
      Inc(I);
    end;
  end;
end;

procedure TsgCADDirectExport.InitEntityForExport(const Entity: TsgDXFEntity);
begin
  if (Entity <> nil) and (Entity.EntType = ceImageEnt) then
  begin
    TsgDXFImageEntAccess(Entity).FDirectExportConverImageToOLE := ConvertImageToOLE;
    TsgDXFImageEntAccess(Entity).FDirectExportImageFileIndex := ExportImageFileIndex;
  end;
end;

function TsgCADDirectExport.IsNameInternal(const AName: string): Boolean;
begin
  Result := AnsiUpperCase(AName) = 'REDLINE';
end;

function TsgCADDirectExport.IsPaperLayout(const AName: string): Boolean;
begin
  Result := GetPaperIndex(AName) > 0;//Result := Pos(GetPaperSpaceName, UpperCase(AName)) > 0;
end;

function TsgCADDirectExport.GetCorrectBlockFlagsAndName(const ABlock: TsgDXFBlock;
  var AFlags: Byte; var AName: string; Data: Pointer = nil): Boolean;
var
  vLayout: TsgDXFEntity;
begin
  Result := False;
  if IsAnonymousBlock(AFlags) or (Assigned(Data) and HasAsterisk(AName)) then
  begin
    if cnstNoAnonimusSVGBlock and IsSVGBlock(AName) then
      CheckSVGBlockFlagsAndName(AFlags, AName)
    else
    begin
      vLayout := nil;
      if Assigned(ABlock) then
        vLayout := ABlock.Layout;
      case GetPaperIndex(AName) of
        0, 1: AFlags := AFlags and not 1;
        -1:
          begin
            if HasAsterisk(AName) then
              Delete(AName, 1, 1);
            case Length(AName) of
              0: AName := 'A';
            else
              if HasInternalDimensionBlockName(AName) then
                AName := 'D'
              else
              begin
                AName := UpCase(AName[1]);
                if not Assigned(vLayout) and (AName = 'P') then
                  AName := 'X';
              end;
            end;
            AName := cnstAsterisk + AName;
            AFlags := AFlags or 1;
            if Assigned(Data) then
              AName := AName + IntToStr(TStringList(Data).Count);
          end;
      else
        AFlags := AFlags and not 1;
        if Assigned(Data) then
          AName := sPaperSpace + IntToStr(TStringList(Data).Count)
        else
          if Assigned(ABlock) then
            AName := ABlock.Name
      end;
    end;
    Result := True;
  end
  else
    if IsPaperLayout(AName) then
    begin
      AName := UpperCase(GetPaperSpaceName);
      Result := True;
    end;
end;

procedure TsgCADDirectExport.CheckSVGBlockFlagsAndName(var AFlags: Byte;
  var AName: string);
begin
  if HasAsterisk(AName) then
    Delete(AName, 1, 1);
  AFlags := AFlags and $FE;
end;

function TsgCADDirectExport.IsNeedAddPointForBoundary(
  W: TsgCADCustomRectangle): Boolean;
begin
  Result := (W.ClippingBoundaryType <> 1) and (W.ClipPointsCount > 1) and
    (not IsEqualF2DPoints(W.ClipPoints[0], W.ClipPoints[W.ClipPointsCount - 1]));
end;

function TsgCADDirectExport.IsNeedImageDefHandle(W: TsgCADCustomRectangle): Boolean;
begin
  Result := (W.EntType = ceImageEnt) and (TsgDXFImageEnt(W).ImageDef <> nil);
end;

procedure TsgCADDirectExport.PrepareBlock(ABlockRecord: TsgDXFBlockRecord);
begin
end;

procedure TsgCADDirectExport.PrepareDrawingDatabase;
var
  I, J, vBlockRecordsCount: Integer;
  vBlockRecords: TsgDXFBlockRecords;
  vRenderObject: TsgDXFInsert;
  vBlockRecord, vMLeaderBlockRecord: TsgDXFBlockRecordAccess;
begin
{$IFDEF SGFPC}
{$IFDEF LINUX}
  if Converter.HeadVarStruct.CodePage = 0 then
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.CodePage := 1252;
{$ENDIF}
{$ENDIF}
  CreateDefaultDrawingProp;
  UpdateTimeStamps;
  FEntityEEDItems.Assign(TsgDXFConverterAccess(Converter).EntityEEDItems);
  if Version < acR2018 then
    TsgDXFConverterAccess(Converter).DecompositeMLAtt;
  if Converter.Sections[csAppID] = nil then
    Converter.NewNamedTable(cnstTableAPPID, TsgDXFTable);
  vBlockRecords := TsgDXFBlockRecords(Converter.Sections[csBlockRecords]);
  if Assigned(vBlockRecords) then
  begin
    SetLength(FMLeaders, 0);
    vBlockRecordsCount := vBlockRecords.Count;
    for I := 0 to vBlockRecordsCount - 1 do
    begin
      vBlockRecord := TsgDXFBlockRecordAccess(vBlockRecords[I]);
      for J := 0 to vBlockRecord.Block.Count - 1 do
      begin
        if vBlockRecord.Block[J].EntType = ceMLeader then
        begin
          SetLength(FMLeaders, Length(FMLeaders) + 1);
          FMLeaders[High(FMLeaders)] := vBlockRecord.Block[J];
        end;
      end;
      PrepareBlock(vBlockRecord);
    end;
    for I := 0 to High(FMLeaders) do
      if TsgCADMultiLeaderAccess(FMLeaders[I]).RenderObject is TsgDXFInsert then
      begin
        vRenderObject := TsgDXFInsert(TsgCADMultiLeaderAccess(FMLeaders[I]).RenderObject);
        vMLeaderBlockRecord := TsgDXFBlockRecordAccess(vRenderObject.BlockRecord);
        vBlockRecords.AddEntity(vMLeaderBlockRecord);
        if vMLeaderBlockRecord.Name = '' then
          vMLeaderBlockRecord.Name := 'MLEADER' + '$' + IntToHex(vRenderObject.Handle, 16);
        PrepareBlock(vMLeaderBlockRecord);
      end;
  end;
end;

procedure TsgCADDirectExport.PrepareOLEHeader(OLE2: TsgDXFOle2Frame;
  const POleHeader: PsgOle2FrameHeaderData);
var
  vSize: TPoint;
begin
  if POleHeader = nil then Exit;
  POleHeader^.Unknown := $5580;
  POleHeader^.Point1 := MakeFPoint(OLE2.Point.X, OLE2.Point.Y, 0.0); //OLE2.Point;
  POleHeader^.Point2 := MakeFPoint(OLE2.Point1.X, OLE2.Point.Y, 0.0);
  POleHeader^.Point3 := MakeFPoint(OLE2.Point1.X, OLE2.Point1.Y, 0.0); //OLE2.Point1;
  POleHeader^.Point4 := MakeFPoint(OLE2.Point.X, OLE2.Point1.Y, 0.0);
{$IFDEF MSWINDOWS}
  if not Assigned(OLE2.OleObject) or (OLE2.OleObject.GetExtent(OLE2.Aspect, vSize) <> S_OK) then
    vSize := Point(Round(GetSystemMetrics(SM_CXSCREEN) * 2540 / GetDeviceCap(LOGPIXELSX)),
      Round(GetSystemMetrics(SM_CYSCREEN) * 2540 / GetDeviceCap(LOGPIXELSY)));
{$ELSE}
    if Assigned(OLE2.OleBitmap) then
      vSize := Point(OLE2.OleBitmap.Width, OLE2.OleBitmap.Height)
    else
      vSize := Point(1024, 768);
{$ENDIF}
  POleHeader^.ExtentX := vSize.X;
  POleHeader^.ExtentY := vSize.Y;
  POleHeader^.TileModeDescriptor := OLE2.TileModeDescriptor xor 1;
  POleHeader^.ReservedInt1 := 256;
  POleHeader^.OLEObjectType := OLE2.OLEObjectType;//1 = Link; 2 = Embedded; 3 = Static
  POleHeader^.ReservedWord := 1;
  POleHeader^.ReservedZero := 0;
  POleHeader^.DrawAspect := OLE2.Aspect;
  POleHeader^.Size := Length(OLE2.BinaryData);
end;

procedure TsgCADDirectExport.ProcessExport(Stream: TStream);
begin
end;

procedure TsgCADDirectExport.SaveToStreamCustom(S: TStream);
var
  vMaxHandle: UInt64;
{$IFDEF SGDEL_2006}
  vTempString: string;
{$ENDIF}
begin
  vMaxHandle := Converter.MaxHandle;
  try
    Clear;
{$IFDEF SGDEL_2006}
    if (not FIgnoreFileName) and (S is TFileStream) then //Converter.FileName?
    begin
      FPathDir := ExtractFileDir(TFileStream(S).FileName);
      FFileName := ExtractFileName(TFileStream(S).FileName);
      vTempString := ExtractFileExt(FFileName);
      if Length(vTempString) <> 0 then
        SetLength(FFileName, Length(FFileName) - Length(vTempString));
      FExportImageFileIndex := 1;
      FImageDefList.Count := 0;
    end
    else
{$ENDIF}
    begin
      FPathDir := '';
      FFileName := '';
      FExportImageFileIndex := -1;
      FImageDefList.Count := 0;
    end;
    LoadSGBlock;
    ProcessExport(S);
  finally
    Converter.MaxHandle := vMaxHandle;
  end;
end;

procedure TsgCADDirectExport.SetDrwPropHyperlinkBase(const Value: string);
begin
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropHyperlinkBase)] := Value;
end;

procedure TsgCADDirectExport.SetConvertAcis(const Value: Boolean);
begin
  FConvertAcis := Value;
end;

procedure TsgCADDirectExport.SetConvertImageToOLE(const Value: Boolean);
begin
  FConvertImageToOLE := Value;
end;

procedure TsgCADDirectExport.SetDrwPropAuthor(const Value: string);
begin
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropAuthor)] := Value;
end;

procedure TsgCADDirectExport.SetDrwPropComments(const Value: string);
begin
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropComments)] := Value;
end;

procedure TsgCADDirectExport.SetDrwPropKeywords(const Value: string);
begin
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropKeywords)] := Value;
end;

procedure TsgCADDirectExport.SetDrwPropSavedBy(const Value: string);
begin
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropLastSavedBy)] := Value;
end;

procedure TsgCADDirectExport.SetDrwPropSubject(const Value: string);
begin
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropSubject)] := Value;
end;

procedure TsgCADDirectExport.SetDrwPropTitle(const Value: string);
begin
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropTitle)] := Value;
end;

procedure TsgCADDirectExport.SetDrwPropRevisionNumber(const Value: string);
begin
  FDrwPropInfo.Values[IntToStr(cntXCodeDrwPropRevisionNumber)] := Value;
end;

procedure TsgCADDirectExport.SetOleFrameVariable(const AValue: Integer);
begin

end;

procedure TsgCADDirectExport.SetVersion(const Value: TsgDWGVersion);
begin
  FVersion := Value;
end;

procedure TsgCADDirectExport.UpdateHeadvarExtentsAndLimits;
var
  vModel, vPaper: TsgDXFLayout;
begin
  if ExportCADImage.LayoutsCount > 0 then
  begin
    vModel := TsgDXFConverterAccess(Converter).GetModelLayout;
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.ExtMin := vModel.ExtMin;
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.ExtMax := vModel.ExtMax;
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.LimMin := vModel.LimMin;
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.LimMax := vModel.LimMax;
  end;
  vPaper := GetPaperLayout;
  if Assigned(vPaper) then
  begin
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.PExtMin := vPaper.ExtMin;
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.PExtMax := vPaper.ExtMax;
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.PLimMin := vPaper.LimMin;
    TsgDXFConverterAccess(Converter).PHeadVarStruct^.PLimMax := vPaper.LimMax;
  end;
end;

procedure TsgCADDirectExport.UpdateTimeStamps;
var
  vConv: TsgDXFConverterAccess;
  vTotalEditTime, vNow, vNewTotalEditTime: Double;
  vTimeStamp: TTimeStamp;
begin
  vConv := TsgDXFConverterAccess(Converter);
  vTotalEditTime := vConv.DrwPropTotalEditingTime.Date +
    vConv.DrwPropTotalEditingTime.Time / MSecsPerDay;
  vNow := Now;
  if not IsZero(vConv.LastSave) then
  begin
    vNewTotalEditTime := vTotalEditTime + (vNow - vConv.LastSave);
    vTimeStamp.Date := Trunc(vNewTotalEditTime);
    vTimeStamp.Time := Round(Abs(vNewTotalEditTime - vTimeStamp.Date) * MSecsPerDay);
    vConv.DrwPropTotalEditingTime := vTimeStamp;
  end;
  vConv.DrwPropModifiedDateTime := TDateTime(vNow);
  vConv.LastSave := vNow;
end;

{ TsgDWGExportDataClass }

constructor TsgDWGExportDataClass.Create;
begin
  inherited Create;
  FBits := nil;
  FOwnerObject := nil;
  FGroup := nil;
  FExportData := CreateExportData;
  FGroupIndex := 0;
end;

constructor TsgDWGExportDataClass.CreateEx(const ABits, AOwnerObject, AGroup: TObject;
  AExportData: TsgDirectExportData);
begin
  Create;
  FBits := ABits;
  FOwnerObject := AOwnerObject;
  FGroup := AGroup;
  FExportData := AExportData;
end;

function TsgDWGExportDataClass.GetPExportData: PsgDirectExportData;
begin
  Result := @FExportData;
end;

end.
