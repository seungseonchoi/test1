{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   Export CAD to SVG                        }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools?                       }
{************************************************************}
unit CADtoSVG;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
  Math, CADImage, sgConsts, sgLines, CADExport,    // 최승선 수정  CADExport 삭제
  DXFConv, sgBitmap, ExtData, sgXMLParser
//{$IFDEF SG_SVG}
//  , SVG
//{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF}

{$IFDEF SG_USE_PNG}
  , pngimage
{$ELSE}
 {$IFNDEF SG_FIREMONKEY}
  {$IFDEF SGDEL_XE2}
  , Vcl.Imaging.jpeg
  {$ELSE}
    {$IFNDEF SGFPC}, jpeg{$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF};

//{$DEFINE SG_DEBUG}

const
  cnstStaticPenWidth: Boolean = {$IFDEF SG_CLIENTSVG}True{$ELSE}False{$ENDIF};
  cnstStaticPenWidthValue = {$IFDEF SG_CLIENTSVG}45{$ELSE}50{$ENDIF};

{
type
  TsgSimpleExport = class;  // 최승선 수정    TsgSimpleExport 추가
}

type
  TsgCADtoSVG = class(TsgSimpleExport)
  private
    FStream: TStream;
    FVersion: string;
    FHasLine: Boolean;
    FHasFill: Boolean;
    FLineColor: TColor;
    FFillColor: TColor;
    FPrecision: Double;
    FWeight: Integer;
    FClipPathName: string;
    FInsertViewBoxPosition: Integer;
    FFontName: string;
    FFontHeight: Single;
    FLineTypeMode: Integer;
    FNullWeightMode: Integer;
    FTransparency: Boolean;
    function ExportValue(const AName: string; const AValue: Integer): string;
    function ExportValues(const ANames: array of string;
      const AValues: array of Integer): string;
    procedure ExportLine(const Points);
    procedure ExportPath(Points: PPoint; Counts: PInteger; Count: Integer;
      const AClosed: Boolean; const ADefineStyle: Boolean = True);
    procedure ExportPoints(Points: PPoint; Count: Integer; const AClosed: Boolean);
    procedure ExportPoly(Points: PPoint; Count: Integer; const AClosed: Boolean);
    procedure ExportRect(const R: PRect);
    procedure ExpData(const AValue: string);
    procedure ExpTextA(const AValue: AnsiString);
    procedure ExpTextW(const AValue: WideString);
    procedure ExpInsData(const APos: Integer; const AValue: string);
    procedure F2DRectToStrings(const ARect: TF2DRect;
      var AX, AY, AWith, AHeight: string);
    function LineWeigthToStr(const AValue: Double): string;
    procedure DefineStyle;
    procedure DefineExtendedData;
    function GetStackPenEntity: TsgDXFPenEntity;
    function GetEntLineWeight(const AEntity: TsgDXFEntity): Double;
    function GetStrokeWidth: Double;
    function GetMaxSize: Double;
    procedure SetPrecision(const Value: Double);
    procedure SetNullWeightMode(const Value: Integer);
  protected
    function GetCompressed: Boolean; virtual;
    function GetLineTypeMode: Integer; override;
    function GetTransparency: Boolean; override;
    function GetPolylineTextInt(const AEntity: TsgDXFEntity; var AValue: string): Boolean;
    procedure ExportHeader;
    procedure ExportBodyStart;
    procedure AddViewBoxToXmlBody(const ARect: TF2DRect);
    procedure AddFillBackgound(const R: PF2DRect);
    procedure ExportBodyEnd;
    procedure ExpFillRgn(P: PRect; Count: Integer); override;
    procedure ExpPolyline(Points: PPoint; Count: Integer); override;
    procedure ExpPolygon(Points: PPoint; Count: Integer); override;
    procedure ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer); override;
    procedure ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer); override;
    procedure ExpImage(const R: TRect; AImage: TPersistent); override;
    procedure ExpImageUV(const APoint1, APoint2, APoint3,  APoint4: TPoint;
      AImage: TPersistent); override;
    procedure ExpSaveDC; override;
    procedure ExpRestoreDC; override;
    procedure ExpTextOut(X, Y: Integer; const ATextA: AnsiString;
      const ATextW: WideString; const ATextParams: PsgExpTextParam); override;
    procedure ExpExportBlock(const blockString: string); override; // 최승선 수정
    function GetTransformMatrix(const AMatrix: TFMatrix): string;
    procedure ExpSetFont(AFont: TFont); override;
    function IsFormatMultiPages: Boolean; override;
    procedure PageEnd(N: Integer); override;
    procedure PageStart(N: Integer); override;
    procedure SetLineTypeMode(const AValue: Integer); override;
    procedure SetTransparency(const AValue: Boolean); override;
    procedure SaveToStreamCustom(S: TStream); override;
    procedure SaveToStreamInternal(S: TStream);
    function IntToStrInternal(const AValue: Integer): string;
    function DoubleToStrInternal(const AValue: Double): string;
    function ValueToStrInternal(const AValue: Double): string;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    procedure ApplyParams(const AExportParams: TsgExportParams); override;
    property Version: string read FVersion write FVersion;
    property Precision: Double read FPrecision write SetPrecision;
    property NullWeightMode: Integer read FNullWeightMode write SetNullWeightMode;
    property Compressed: Boolean read GetCompressed;
//    procedure ExportBlock(const blockString: string); override;  // 최승선 수정
  end;

  TsgCADtoSVGZ = class(TsgCADtoSVG)
  protected
    function GetCompressed: Boolean; override;
  end;

implementation

{$IFNDEF SG_WIN_FPC}
   {$DEFINE SG_USEZlib}
{$ENDIF}

uses
  sgZip,
  {$IFDEF SG_USEZlib}
     {$IFDEF FPC}
     {$ELSE}
       {$IFDEF SGDEL_XE2}System.ZLib{$ELSE}zlibpas_sg{$ENDIF},
     {$ENDIF}
  {$ENDIF}
  sgFunction;//, CADExport; //최승선 수정    CADExport 추가

const
  {$IFDEF SG_USEZlib}
     {$IFDEF FPC}
     Z_DEFLATED   = 8;
     {$ELSE}
     {$ENDIF}
  {$ENDIF}

  cnstCR = #13#10;
  cnstPrecision = 1 / 100;
  cnstPageSize = 800;
  cnstDocType = '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">' + cnstCR;
  cnstXmlBodyBegin = '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">';
  cnstXmlBodyEnd = '</svg>' + cnstCR;
  cnstHeadXML = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>' + cnstCR;
  cnstTagEnd = '/>' + cnstCR;
  cnstTitle = '  <title>Export to SVG</title>' + cnstCR;
  cnstDescBegin = '  <desc>Generated by CADtoSVG Version 1.0: ';
  cnstDescEnd = '</desc>' + cnstCR;
  cnstNone = 'none';
  cnstFontNameDefault = 'arial';
  cnstDefLineWeight = '0.01';

type
  TsgInteger = {$IFDEF SGDEL_4}Int64{$ELSE}Integer{$ENDIF};
  TsgBMAdapterAccess = class(TsgBMAdapter);
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgCADImageAccess = class(TsgCADImage);
  TsgDXFConvAccess = class(TsgDXFConverter);

function GetColor(AColor: TColor): string;
var
  vColors: array [0..3] of Byte absolute AColor;
  vBuf: Byte;
begin
  vBuf := vColors[0];
  vColors[0] := vColors[2];
  vColors[2] := vBuf;
  Result := '#' + IntToHex(AColor, 6);
end;

function GetSvgText(const AStr: string): string;
begin
  Result := AStr;
  if Length(Result) > 0 then
  begin
    Result := ReplaceASCICode(AStr, True);
    ReplaceWebOrAnsiCode(False, Result, 4);
  end;
end;

function GetsgDataTime: string;
begin
  Result :=  FormatDateTime('dd/mm/yyyy ', Now) + FormatDateTime('hh:nn:ss', Time);
end;


function StreamCRC32(AStream: TStream; ASeed: Cardinal): Cardinal;
var
  vPosition: Int64;
  vMemoryStream: TMemoryStream;
begin
  if AStream is TCustomMemoryStream then
  begin
    Result := sgFunction.CRC32(TCustomMemoryStream(AStream).Memory, ASeed, AStream.Size)
  end
  else
  begin
    vPosition := AStream.Position;
    try
      AStream.Position := 0;
      vMemoryStream := TMemoryStream.Create;
      try
        vMemoryStream.CopyFrom(AStream, AStream.Size);
        Result := sgFunction.CRC32(vMemoryStream.Memory, ASeed, AStream.Size);
      finally
        vMemoryStream.Free;
      end;
    finally
      AStream.Position := vPosition;
    end;
  end;
end;

function PackGZStream(AStreamIn, AStreamOut: TStream): Boolean;
{$IFDEF SG_USEZlib}
var
{$IFDEF SGDEL_XE2}
  vZipper: System.ZLib.TCompressionStream;
{$ELSE}
  vStreamOut: TMemoryStream;
{$ENDIF}
  vHeader: TsgGzHeader;
  vFooter: TsgGzTail;
  vStreamOutPos: Int64;
begin
  Result := True;
  try
    vStreamOutPos := AStreamOut.Position;
    FillChar(vHeader, SizeOf(vHeader), 0);
    vHeader.ID1 := (cnstGZPacked and $FF);
    vHeader.ID2 := ((cnstGZPacked shr 8) and $FF);
    vHeader.CompMethod := Z_DEFLATED;
    AStreamOut.Write(vHeader, SizeOf(vHeader) - 2);
{$IFDEF SGDEL_XE2}
    vZipper := System.ZLib.TCompressionStream.Create(AStreamOut);
    try
      vZipper.CopyFrom(AStreamIn, AStreamIn.Size);
    finally
      vZipper.Free;
    end;
{$ELSE}
    vStreamOut := TMemoryStream.Create;
    try
      vStreamOut.CopyFrom(AStreamIn, AStreamIn.Size);
      vStreamOut.Position := 0;
      sgZip.PackStream(vStreamOut);
      vStreamOut.Position := 0;
      AStreamOut.CopyFrom(vStreamOut, vStreamOut.Size);
    finally
      FreeAndNil(vStreamOut);
    end;
{$ENDIF}
    FillChar(vFooter, SizeOf(vFooter), 0);
    vFooter.CRC32 := StreamCRC32(AStreamIn, 0);
    vFooter.Size := AStreamIn.Size;

    AStreamOut.Position := AStreamOut.Position - SizeOf(vFooter) + 4;
//    AStreamOut.Seek(-SizeOf(vFooter) + 4, 1);

    AStreamOut.Write(vFooter, SizeOf(vFooter));
    AStreamOut.Position := vStreamOutPos;
    AStreamOut.Write(vHeader, SizeOf(vHeader));
    AStreamOut.Position := AStreamOut.Size;
  except
    Result := False;
  end;
{$ELSE}
begin
  Result := False;
{$ENDIF}
end;

{ TsgCADtoSVG }


procedure TsgCADtoSVG.AddFillBackgound(const R: PF2DRect);
var
  vRect, vFillStyle, vSize: string;
  vX, vY, vWidth, vHeight: string;
begin
  if Transparency then Exit;
  vFillStyle := 'style="fill:' + GetColor(BackgroundColor) + '"';
  vX := '0%';
  vY := '0%';
  vWidth := '100%';
  vHeight := '100%';
  if R <> nil then
    F2DRectToStrings(R^, vX, vY, vWidth, vHeight);
  vSize := 'x="' + vX + '" y="' + vY + '" width="' + vWidth + '" height="' + vHeight + '"';
  vRect := '<rect ' + vSize + ' ' + vFillStyle + cnstTagEnd;
  ExpData(vRect);
end;

procedure TsgCADtoSVG.AddViewBoxToXmlBody(const ARect: TF2DRect);
var
  vViewBox: string;
  vMinX, vMinY, vWidth, vHeight: string;
begin
  if FInsertViewBoxPosition > 0 then
  begin
    F2DRectToStrings(ARect, vMinX, vMinY, vWidth, vHeight);

    vViewBox := ' viewBox="' + vMinX + ' ' + vMinY + ' ' + vWidth + ' ' + vHeight + '"';
    ExpInsData(FInsertViewBoxPosition, vViewBox);
    FInsertViewBoxPosition := -1;
    AddFillBackgound(@ARect);
  end;
end;

procedure TsgCADtoSVG.ApplyParams(const AExportParams: TsgExportParams);
begin
  inherited ApplyParams(AExportParams);
  XScale := 1;
  NullWeightMode := AExportParams.NullWeightMode;
//  LineTypeMode := 1;
end;

constructor TsgCADtoSVG.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  FVersion := '1.0';
  FPrecision := cnstPrecision;
  FTransparency := True;
  Self.CADImage.ExpProcs.ExpTextOut := ExpTextOut;
  Self.CADImage.ExpProcs.ExpExportBlock := ExpExportBlock; // 최승선 수정
  Self.CADImage.ExpProcs.ExpFillRgn := nil;//used ExpPolyPolygonInteger
  TTFMode := ttfAuto;
end;

procedure TsgCADtoSVG.DefineExtendedData;
{$IFDEF SG_CLIENTSVG}
var
  vStrExtData {$IFDEF SG_CLIENTSVG}, vValue {$ENDIF}: string;
  vEntity: TsgDXFEntity;
begin
  vStrExtData := '';
  if EntitiesStack.Count > 0 then
  begin
    vEntity := TsgDXFEntity(EntitiesStack.Last);
    if GetPolylineTextInt(vEntity, vValue) then
    begin
      SetPolylineText(vEntity, vValue);
      vStrExtData := ' id="' +  vValue +  '"';
      vStrExtData := vStrExtData + ' tag="KPIrag"';
      if vEntity.Handle <> cnstBadHandle then
        vStrExtData := vStrExtData + ' handle="' + IntToHex(vEntity.Handle, 0) + '"';
    end;
  end;
  if Length(vStrExtData) > 0 then
    ExpData(vStrExtData);
{$ELSE}
begin
{$ENDIF}
end;

procedure TsgCADtoSVG.DefineStyle;
const
  cnstDashArraySep = ',';
  cnstDashMinLen = 1;
var
  I: Integer;
  vStyle, vDashStr, vDashMinStr: string;
  vStrokeWidth, vScale, vDash: Double;
  vLTEntity: TsgDXFPenEntity;
  vEL: TsgLTypeElement;
  vDashArray, vStrokeWidthStr: string;
{$IFDEF SG_CLIENTSVG}
 vValue: string;
{$ENDIF}
begin
  vStyle := ' style="';
  {$IFDEF SG_CLIENTSVG}
  if (EntitiesStack.Count > 0) and GetPolylineTextInt(TsgDXFEntity(EntitiesStack.Last), vValue) then
  begin
    vStyle := vStyle + 'stroke:none;' + 'fill:' + GetColor($000000) + ';' + 'opacity:0.15;';
  end
  else
  {$ENDIF}
  begin
    if FHasLine then
    begin
      vStyle := vStyle + 'stroke:' + GetColor(FLineColor) + '; ';
      if (LineTypeMode = 1) then
      begin
        vLTEntity := GetStackPenEntity;
        if Assigned(vLTEntity) then
        begin
          vDashMinStr := ValueToStrInternal(cnstDashMinLen);
          if Assigned(vLTEntity.Lines) and (not (vLTEntity.Lines.IsSolid and (vLTEntity.LineTypeScale > 0))) then
          begin
            vDashArray := '';
            vScale := vLTEntity.LineTypeScale * Self.CADImage.Converter.GetLTScale * XScale;
            for I := 0 to vLTEntity.Lines.ElementsCount - 1 do
            begin
              vEL := vLTEntity.Lines.Elements[I];
              vDash := Abs(vEL.Dash) * vEL.Scale * vScale;
              if vDash * FPrecision > cnstDashMinLen then
                vDashStr := DoubleToStrInternal(vDash)
              else
                vDashStr := vDashMinStr;
              if I > 0 then
                vDashStr := cnstDashArraySep + vDashStr;
              vDashArray := vDashArray + vDashStr;
            end;
            if vLTEntity.Lines.ElementsCount mod 2 <> 0 then
              vDashArray := vDashArray + DoubleToStrInternal(0);
            if Length(vDashArray) > 0 then
              vStyle := vStyle + 'stroke-dasharray:' + vDashArray + '; ';
          end;
        end;
      end;
    end
    else
      vStyle := vStyle + 'stroke:' + cnstNone + '; ';
    if FHasFill then
      vStyle := vStyle + 'fill:' + GetColor(FFillColor) + ';'
    else
      vStyle := vStyle + 'fill:' + cnstNone + ';';
  end;
  if FNullWeightMode <> 1 then
  begin
    vStrokeWidthStr := '';
    case FNullWeightMode of
      2:
      begin
        vStrokeWidthStr := LineWeigthToStr(FPrecision * 10);
      end;
      3:
      begin
        vStrokeWidthStr := ValueToStrInternal(NullWidth);
      end;
      4:
      begin
        vLTEntity := GetStackPenEntity;
        if Assigned(vLTEntity) then
          vStrokeWidthStr := LineWeigthToStr(GetEntLineWeight(vLTEntity))
        else
          vStrokeWidthStr := LineWeigthToStr(NullWidth);
      end;
    else
      vStrokeWidth := GetStrokeWidth;
      if vStrokeWidth * FPrecision > 1 then
        vStrokeWidthStr := DoubleToStrInternal(vStrokeWidth);
    end;
    if Length(vStrokeWidthStr) > 0 then
      vStyle := vStyle + ' stroke-width:'  + vStrokeWidthStr + ';';
  end;
  vStyle := vStyle + '"';
  ExpData(vStyle);
end;

function TsgCADtoSVG.GetStackPenEntity: TsgDXFPenEntity;
var
  vEnt: TObject;
begin
  Result := nil;
  if EntitiesStack.Count > 0 then
  begin
    vEnt := TObject(EntitiesStack.Last);
    if vEnt is TsgDXFPenEntity then
      Result := TsgDXFPenEntity(vEnt);
  end;
end;

function TsgCADtoSVG.GetStrokeWidth: Double;
begin
  if cnstStaticPenWidth then
    Result := cnstStaticPenWidthValue
  else
    Result := PenWidth * XScale / 100;
end;

function TsgCADtoSVG.GetTransformMatrix(const AMatrix: TFMatrix): string;
begin
  Result := ' transform="matrix(' +
    DoubleToStr(AMatrix.M[0, 0], cnstPoint) + ' ' +
    DoubleToStr(AMatrix.M[0, 1], cnstPoint) + ' ' +
    DoubleToStr(AMatrix.M[1, 0], cnstPoint) + ' ' +
    DoubleToStr(AMatrix.M[1, 1], cnstPoint) + ' ' +
    DoubleToStr(AMatrix.M[3, 0], cnstPoint) + ' ' +
    DoubleToStr(AMatrix.M[3, 1], cnstPoint) + ')" '
end;

function TsgCADtoSVG.GetTransparency: Boolean;
begin
  Result := FTransparency;
end;

function TsgCADtoSVG.GetCompressed: Boolean;
begin
  Result := False;
end;

function TsgCADtoSVG.GetEntLineWeight(const AEntity: TsgDXFEntity): Double;
begin
  Result := TsgCADImageAccess(CADImage).GetEntLineWeight(AEntity,
    TsgCADImageAccess(CADImage).FDraw.Insert);
end;

function TsgCADtoSVG.GetLineTypeMode: Integer;
begin
  Result := FLineTypeMode;
end;

function TsgCADtoSVG.GetMaxSize: Double;
begin
  Result := cnstPageSize / cnstPrecision;
end;

function TsgCADtoSVG.GetPolylineTextInt(const AEntity: TsgDXFEntity;
  var AValue: string): Boolean;
begin
  Result := False;
  AValue := '';
{$IFDEF SG_CLIENTSVG}
  if Assigned(AEntity) and (AEntity is TsgDXFPolyline) then
  begin
    if DXFConv.GetPolylineText(AEntity, AValue) then
      Result := True;
  end;
{$ENDIF}
end;

procedure TsgCADtoSVG.ExpData(const AValue: string);
var
  vStr: sgRawByteString;
begin
{$IFDEF SGDEL_2009}
  vStr := ConvertToAnsiString(AValue, CP_ACP);
{$ELSE}
  vStr := AValue;
{$ENDIF}
  FStream.Write(vStr[1], Length(vStr) * SizeOf(vStr[1]));
end;

procedure TsgCADtoSVG.ExpTextA(const AValue: AnsiString);
var
  vStr: WideString;
begin
  vStr := ConvertToWideString(AValue, CP_ACP);
  ExpTextW(vStr);
end;

procedure TsgCADtoSVG.ExpTextW(const AValue: WideString);
var
  vStr: sgRawByteString;
begin
  vStr := ConvertToUtf8(GetSvgText(AValue));
  FStream.Write(vStr[1], Length(vStr) * SizeOf(vStr[1]));
end;

procedure TsgCADtoSVG.ExpFillRgn(P: PRect; Count: Integer);
var
  vPts: array[0..3] of Integer;
begin
  FHasLine := True;
  FHasFill := True;
  FFillColor := GetFillColor;
  FLineColor := FFillColor;
  while Count > 0 do
  begin
    if (P^.Bottom - P^.Top > 1) and (P^.Right - P^.Left > 1)  then
      ExportRect(P)
    else
    begin
      vPts[0] := P^.Left;
      vPts[1] := P^.Top;
      vPts[2] := P^.Right;
      vPts[3] := P^.Bottom;
      ExportLine(vPts);
    end;
    Inc(P);
    Dec(Count);
  end;
end;

procedure TsgCADtoSVG.ExpImage(const R: TRect; AImage: TPersistent);
begin
  ExpImageUV(Point(R.Left,R.Bottom),R.BottomRight,
    Point(R.Right,R.Top),R.TopLeft, AImage);
end;

procedure TsgCADtoSVG.ExpImageUV(const APoint1, APoint2, APoint3,
  APoint4: TPoint; AImage: TPersistent);
var
  vImage, vEnCode, vMatrixStr, vTypeImage: string;
  vMemStream, vMemStreamEnCode: TMemoryStream;
  vExportGraphic: TPersistent;
  vBM: TsgBMAdapter;
  vChar: AnsiChar;
  vUvector, vVvector, vPoint, vSize: TFPoint;
  vMatrix: TFMatrix;
begin
  if (GetSizeGraphic(AImage).cx < 1) or (GetSizeGraphic(AImage).cy < 1) then
    Exit;
  if IsGreatImage(AImage) then
    Exit;
{$IFNDEF SG_FIREMONKEY}
  {$IFDEF SG_USE_PNG}
  vExportGraphic :={$IFDEF SGDEL_2009}TPngImage{$ELSE}TPngObject{$ENDIF}.Create;
  vTypeImage := 'png';
  {$ELSE}
  vExportGraphic := TJPEGImage.Create;
  vTypeImage := 'jpeg';
  {$ENDIF}
{$ELSE}
  vExportGraphic := TBitmap.Create;
  vTypeImage := 'png';
{$ENDIF}
  vBM := TsgBMAdapter.Create;
  try
    vBM.Assign(AImage);
    vExportGraphic.Assign(vBM.Bitmap);
    vMemStream := TMemoryStream.Create;
    try
    {$IFNDEF SG_FIREMONKEY}
      TGraphic(vExportGraphic).SaveToStream(vMemStream);
    {$ELSE}
      TBitmap(vExportGraphic).SaveToStream(vMemStream);
    {$ENDIF}
      vMemStreamEnCode := TMemoryStream.Create;
      try
        vMemStream.Position := 0;
        EncodeBase64(TStream(vMemStream),TStream(vMemStreamEnCode));

        vPoint := MakeFPoint(APoint4.X, APoint4.Y);
        vUvector := SubFPoint2D(MakeFPoint(APoint1.X, APoint1.Y), vPoint);
        vVvector := SubFPoint2D(MakeFPoint(APoint3.X, APoint3.Y), vPoint);
        vSize := MakeFPoint(GetSizeGraphic(AImage).cx, GetSizeGraphic(AImage).cy);

        vMatrix := sgFunction.FMat2DByImage(vPoint, vUvector, vVvector, vSize.X, vSize.Y);

        vMatrixStr := 'transform=" matrix(' +
        DoubleToStrInternal(vMatrix.V1[0])+ ', ' +
        DoubleToStrInternal(vMatrix.V1[1])+ ', ' +
        DoubleToStrInternal(vMatrix.V2[0])+ ', ' +
        DoubleToStrInternal(vMatrix.V2[1])+ ', ' +
        DoubleToStrInternal(vMatrix.V4[0]) + ', ' +
        DoubleToStrInternal(vMatrix.V4[1]) + ')" ';

        vImage := '<image ';
        vImage := vImage + 'x="' + ValueToStrInternal(0) + '" ';
        vImage := vImage + 'y="' + ValueToStrInternal(0) + '" ';
        vImage := vImage + 'width="' + ValueToStrInternal(vSize.X) + '" ';
        vImage := vImage + 'height="' + ValueToStrInternal(vSize.Y) + '" ';
        vImage := vImage + vMatrixStr;
        vMemStreamEnCode.Position := vMemStreamEnCode.Size;
        vChar := #0;
        vMemStreamEnCode.Write(vChar, 1);
        vEnCode := string(AnsiString(PAnsiChar(vMemStreamEnCode.Memory)));
        vImage := vImage + 'xlink:href="data:image/' + vTypeImage+';base64,' +
          vEnCode + '"/> '+ cnstCR;
        ExpData(vImage);
      finally
        vMemStreamEnCode.Free;
      end;
    finally
      vMemStream.Free;
    end;
  finally
    vExportGraphic.Free;
    vBM.Free;
  end;
end;

procedure TsgCADtoSVG.ExpInsData(const APos: Integer; const AValue: string);
var
  vBufferSize: Integer;
  vStreamBuffer: TMemoryStream;
begin
  vBufferSize := FStream.Position - APos;
  if vBufferSize > 0 then
  begin
    FStream.Position := APos;
    vStreamBuffer := TMemoryStream.Create;
    try
      vStreamBuffer.CopyFrom(FStream, vBufferSize);
      FStream.Position := APos;
      ExpData(AValue);
      vStreamBuffer.Position := 0;
      FStream.CopyFrom(vStreamBuffer, vStreamBuffer.Size);
    finally
      vStreamBuffer.Free;
    end;
  end
  else
    ExpData(AValue);
end;

procedure TsgCADtoSVG.ExportBodyEnd;
begin
  ExpData(cnstXmlBodyEnd);
end;

procedure TsgCADtoSVG.ExportBodyStart;
var
  vHeader, vBackgoundFill: string;
begin
  vHeader := cnstXmlBodyBegin;
  if not Transparency then
  begin
    vBackgoundFill := ' style="background:' + GetColor(BackgroundColor) + '"';
    Insert(vBackgoundFill, vHeader, Length(vHeader));
  end;
  ExpData(vHeader + cnstCR);
  FInsertViewBoxPosition := FStream.Position - Length(cnstCR) - Length('>');
  ExpData(cnstTitle);
  ExpData(cnstDescBegin + GetsgDataTime + cnstDescEnd);
end;

procedure TsgCADtoSVG.ExportHeader;
begin
  ExpData(cnstHeadXML);
  ExpData(cnstDocType);
end;

//최승선 수정
procedure TsgCADtoSVG.ExpExportBlock(const blockString: string);
begin
  ExpData(blockString);
end;

procedure TsgCADtoSVG.ExportLine(const Points);
var
  vPoly: string;
  P: PInteger;
begin
  P := PInteger(@Points);
  vPoly := '<line x1="' + IntToStrInternal(P^);
  Inc(P);
  vPoly := vPoly + '" y1="' + IntToStrInternal(P^);
  Inc(P);
  vPoly := vPoly + '" x2="' + IntToStrInternal(P^);
  Inc(P);
  vPoly := vPoly + '" y2="' + IntToStrInternal(P^) + '"';
  ExpData(vPoly);
  DefineStyle;
  DefineExtendedData;
  ExpData(cnstTagEnd);
end;

procedure TsgCADtoSVG.ExpPolyline(Points: PPoint; Count: Integer);
begin
  FHasLine := True;
  FHasFill := False;
  FLineColor := GetStrokeColor;
  FWeight := 0;
  ExportPoints(Points, Count, False);
end;

procedure TsgCADtoSVG.ExpPolyPolygon(const Points; Counts: PInteger; Count: Integer);
begin
  FHasLine := False;
  FHasFill := True;
  FFillColor := GetFillColor;
  FWeight := 0;
  ExportPath(PPoint(@Points), Counts, Count, True);
end;

procedure TsgCADtoSVG.ExpPolyPolyline(const Points; Counts: PInteger; Count: Integer);
begin
  FHasLine := True;
  FHasFill := False;
  FLineColor := GetStrokeColor;
  FWeight := 0;
  ExportPath(PPoint(@Points), Counts, Count, False);
end;

procedure TsgCADtoSVG.ExpPolygon(Points: PPoint; Count: Integer);
begin
  FHasLine := False;
  FHasFill := True;
  FFillColor := GetFillColor;
  FWeight := 0;
  ExportPoints(Points, Count, True);
end;

procedure TsgCADtoSVG.ExpSaveDC;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCADtoSVG.ExpSetFont(AFont: TFont);
var
  I: Integer;
begin
  FFontName := AFont.Name;
  if Length(FFontName) > 0 then
  begin
    for I := 1 to Length(FFontName) do
    begin
      if Integer(FFontName[I]) < 9 then
      begin
        SetLength(FFontName, I - 1);
        Break;
      end;
    end;
  end;
  if Length(FFontName) = 0 then
    FFontName := cnstFontNameDefault;
  FFontHeight :=AFont.Height;
//  FFontColor := AFont.Color;
end;
{
function FMat2DNormalize(const AMatrix: TFMatrix;
  const AScale: PDouble = nil):  TFMatrix;
var
  vMax: Double;
begin
  vMax := Max(Max(AMatrix.M[0, 0], AMatrix.M[0, 1]),
              Max(AMatrix.M[1, 0], AMatrix.M[1, 1]));
  if AScale <> nil then
    AScale^ :=  vMax;
  if vMax  <> 0 then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.M[0, 0] := AMatrix.M[0, 0] / vMax;
    Result.M[0, 1] := AMatrix.M[0, 1] / vMax;
    Result.M[1, 0] := AMatrix.M[1, 0] / vMax;
    Result.M[1, 1] := AMatrix.M[1, 1] / vMax;
    Result.M[3, 0] := AMatrix.M[3, 0];
    Result.M[3, 1] := AMatrix.M[3, 1];
  end
  else
    Result := AMatrix;
end;
}
procedure TsgCADtoSVG.ExpTextOut(X, Y: Integer; const ATextA: AnsiString;
  const ATextW: WideString; const ATextParams: PsgExpTextParam);
var
  vText, vStyle, vPositionStr: string;
  vFontColor: TColor;
  vMatrix: TFMatrix;
  vTextSize: Double;
begin
  vFontColor := GetFontColor;
  vTextSize := Abs(FFontHeight);
  vText := '<text  ';
  vPositionStr := ExportValues(['x', 'y'], [X, Y]);
  if ATextParams <> nil then
  begin
    if ATextParams.HKoef <> 0 then
      vTextSize :=  Abs(FFontHeight) * 1.5 / ATextParams.HKoef;
    if CADImage.TTFMode = ttfGDI then
    begin
      vMatrix := StdMat(MakeFPoint(1, -1, 1), cnstFPointZero);
      vMatrix := FMatXMat(vMatrix, ATextParams^.Draw);
      vMatrix := FMat2DNormalize(vMatrix);
      vMatrix.M[3, 0] := X * FPrecision;
      vMatrix.M[3, 1] := Y * FPrecision;
      vPositionStr := GetTransformMatrix(vMatrix);
    end;
  end;
  vText := vText + vPositionStr;
  ExpData(vText);
  if Length(FFontName) = 0 then
    FFontName := cnstFontNameDefault;
  vStyle := ' style="fill:' + GetColor(vFontColor) +
    '; font-family:' + FFontName +
    '; font-size:' + IntToStrInternal(Ceil(vTextSize)) + '"';
  ExpData(vStyle + '>');
  if Length(ATextW) > 0 then
    ExpTextW(ATextW)
  else
    ExpTextA(ATextA);
  ExpData('</text>' + cnstCR);
end;


function TsgCADtoSVG.IsFormatMultiPages: Boolean;
begin
  Result := False;
end;

function TsgCADtoSVG.LineWeigthToStr(const AValue: Double): string;
begin
  if AValue > 0 then
    Result := ValueToStrInternal(AValue)
  else
    Result := cnstDefLineWeight;
end;

procedure TsgCADtoSVG.ExpRestoreDC;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgCADtoSVG.PageEnd(N: Integer);
begin
  if N > 0 then
    ExpData('</g>' + cnstCR)
  else
  begin
    if Length(FClipPathName) > 0 then
      ExpData('</g>' + cnstCR);
  end;
end;

procedure TsgCADtoSVG.PageStart(N: Integer);
var
  vMax, I: Integer;
  vBoundsRect: TRect;
  vPoints: array [0..3] of TPoint;
  vCounts: array[0..0] of Integer;
  vSize: Double;
  vExportRect: TF2DRect;
begin
  if N > 0 then
    ExpData('<g  id="Page' + IntToStr(N) +'" display="none">' + cnstCR)
  else
  begin
    vSize := GetMaxSize;
    vBoundsRect := GetExportRect;
    vMax := MaxI(vBoundsRect.Right - vBoundsRect.Left, vBoundsRect.Bottom - vBoundsRect.Top);
    if {(vMax < vSize) and} (vMax > 0) then
      XScale := vSize/vMax;
    FPrecision := 1/XScale;
    vExportRect.Left := vBoundsRect.Left * XScale;
    vExportRect.Top := vBoundsRect.Top * XScale;  
    vExportRect.Right := vBoundsRect.Right * XScale;  
    vExportRect.Bottom := vBoundsRect.Bottom * XScale;
    AddViewBoxToXmlBody(vExportRect);
    FClipPathName := '';
    if HasClippingRect then
    begin
      FClipPathName := 'ClipPath' + IntToStr(N);
      ExpData('<g  id="Page' + IntToStr(N) + '" clip-rule="nonzero" clip-path="url(#' +
        FClipPathName + ')" >' + cnstCR);
      ExpData('<clipPath id="' + FClipPathName + '">' + cnstCR);
      vPoints[0] := vBoundsRect.TopLeft;
      vPoints[1] := vBoundsRect.TopLeft;
      vPoints[1].X := vBoundsRect.Right;
      vPoints[2] := vBoundsRect.BottomRight;
      vPoints[3] := vBoundsRect.BottomRight;
      vPoints[3].X := vBoundsRect.Left;
      vCounts[0] := 4;
      for I := Low(vPoints) to High(vPoints) do
      begin
        vPoints[I].X := Round(vPoints[I].X * XScale);
        vPoints[I].Y := Round(vPoints[I].Y * XScale);
      end;
      ExportPath(@vPoints[0], @vCounts[0], 1, True, False);
      ExpData('</clipPath>' + cnstCR);
    end;
  end;
end;

procedure TsgCADtoSVG.F2DRectToStrings(const ARect: TF2DRect;
  var AX, AY, AWith, AHeight: string);
begin
  AX := DoubleToStrInternal(ARect.Left);
  AY := DoubleToStrInternal(Min(ARect.Top, ARect.Bottom));
  AWith := DoubleToStrInternal(ARect.Right - ARect.Left);
  AHeight := DoubleToStrInternal(Abs(ARect.Top - ARect.Bottom));
end;

procedure TsgCADtoSVG.ExportPath(Points: PPoint; Counts: PInteger; Count: Integer;
  const AClosed: Boolean; const ADefineStyle: Boolean = True);
var
  PP: PPoint;
  PC, P: PInteger;
  C, vCnt, X, Y: Integer;
  vPath, vPts: string;
begin
  vPath := '<path d="';
  PP := Points;
  PC := Counts;
  while Count > 0 do
  begin
    C := PC^;
    Inc(PC);
    P := PInteger(PP);
    vCnt := C;
    if vCnt > 0 then
    begin
      X := P^; Inc(P);
      Y := P^; Inc(P);
      vPts := 'M' + IntToStrInternal(X) + ',' + IntToStrInternal(Y) + ' ';
      Dec(vCnt);
    end;
    while vCnt > 0 do
    begin
      X := P^; Inc(P);
      Y := P^; Inc(P);
      vPts := vPts + 'L' + IntToStrInternal(X) + ',' + IntToStrInternal(Y) + ' ';
      Dec(vCnt);
    end;
    if AClosed then
    begin
      P := PInteger(PP);
      X := P^; Inc(P);
      Y := P^;
      vPts := vPts + 'L' + IntToStrInternal(X) + ',' + IntToStrInternal(Y) + ' ';
    end;
    vPath := vPath + vPts;
    Inc(PP, C);
    Dec(Count);
  end;
  ExpData(vPath + '"');
  if ADefineStyle then
    DefineStyle;
  DefineExtendedData;
  ExpData(cnstTagEnd);
end;

procedure TsgCADtoSVG.ExportPoints(Points: PPoint; Count: Integer; const AClosed: Boolean);
var
  vPixel: array[0 .. 1] of TPoint;
begin
  case Count of
    0:;
    1:
      begin
        vPixel[0] := Points^;
        vPixel[1] := Points^;
        ExportLine(vPixel);
      end;
    2: ExportLine(Points^);
  else
    ExportPoly(Points, Count, AClosed);
  end;
end;

procedure TsgCADtoSVG.ExportPoly(Points: PPoint; Count: Integer; const AClosed: Boolean);
var
  vPoly: string;
  X, Y: Integer;
  P: PInteger;
begin
  vPoly := '<polyline points="';
  P := PInteger(Points);
  while Count > 0 do
  begin
    X := P^; Inc(P);
    Y := P^; Inc(P);
    vPoly := vPoly + IntToStrInternal(X) + ',' + IntToStrInternal(Y) + ' ';
    Dec(Count);
  end;
  if AClosed then
  begin
    P := PInteger(Points);
    X := P^; Inc(P);
    Y := P^;
    vPoly := vPoly + IntToStrInternal(X) + ',' + IntToStrInternal(Y) + ' ';
  end;
  ExpData(vPoly + '"');
  DefineStyle;
  DefineExtendedData;
  ExpData(cnstTagEnd);
end;

procedure TsgCADtoSVG.ExportRect(const R: PRect);
var
  vRect: string;
begin
  vRect := '<rect ' + ExportValues(['x', 'y', 'width', 'height'],
    [R^.Left, R^.Top, R^.Right - R^.Left, R^.Bottom - R^.Top]);
  ExpData(vRect);
  DefineStyle;
  DefineExtendedData;
  ExpData(cnstTagEnd);
end;

function TsgCADtoSVG.ExportValue(const AName: string; const AValue: Integer): string;
begin
  Result := AName + '="' + IntToStrInternal(AValue) + '"';
end;

function TsgCADtoSVG.ExportValues(const ANames: array of string;
  const AValues: array of Integer): string;
var
  I: Integer;
  vNameValue: string;
begin
  Result := '';
  for I := Low(ANames) to High(ANames) do
  begin
    vNameValue := ExportValue(ANames[I], AValues[I]) + ' ';
    Result := Result + vNameValue;
  end;
  I := Length(Result);
  if I > 0 then
   SetLength(Result, I - 1);
end;

procedure TsgCADtoSVG.SaveToStreamCustom(S: TStream);
var
  vStream: TMemoryStream;
begin
  if Compressed then
  begin
    vStream := TMemoryStream.Create;
    try
      SaveToStreamInternal(vStream);
      vStream.Position := 0;
      if not PackGZStream(vStream, S) then
      begin
        vStream.Position := 0;
        S.CopyFrom(vStream, vStream.Size);
      end;
    finally
      vStream.Free;
    end;
  end
  else
    SaveToStreamInternal(S);
end;

procedure TsgCADtoSVG.SaveToStreamInternal(S: TStream);
var
  vSaveDC: Char;
  vIsCrossoverMatrix: Boolean;
  vXScale: Double;
{$IFDEF SG_DEBUG}
  vTickCount: Cardinal;
{$ENDIF}
begin
{$IFDEF SG_DEBUG}
  vTickCount := GetTickCount;
{$ENDIF}
  if {SaveAsSvgM and} (CADImage is TsgVectorImageWithSave) and TsgVectorImageWithSave(CADImage).CanSaveToStream then
  begin
    TsgVectorImageWithSave(CADImage).SaveToStream(S);
  end
  else
  begin
    vIsCrossoverMatrix := Converter.IsCrossoverMatrix;
    vXScale := XScale;
    FStream := S;
    vSaveDC := SetDecimalSeparator('.');
    try
      Converter.IsCrossoverMatrix := False;
      ExportHeader;
      ExportBodyStart;
      try
        inherited SaveToStreamCustom(FStream);
      finally
        ExportBodyEnd;
      end;
    finally
      XScale := vXScale;
      SetDecimalSeparator(vSaveDC);
      FStream := nil;
      Converter.IsCrossoverMatrix := vIsCrossoverMatrix;
    end;
  end;
{$IFDEF SG_DEBUG}
  MessageBox(0, PChar('TickCount= '+IntToStr(GetTickCount - vTickCount)), 'SVG EXPORT DEBUG MODE', MB_OK or MB_ICONINFORMATION);
{$ENDIF}
end;

procedure TsgCADtoSVG.SetLineTypeMode(const AValue: Integer);
begin
  FLineTypeMode := AValue;
end;

procedure TsgCADtoSVG.SetNullWeightMode(const Value: Integer);
begin
  if (Value >= 0) and (Value <= 4) then
    FNullWeightMode := Value;
end;

procedure TsgCADtoSVG.SetPrecision(const Value: Double);
begin
  FPrecision := Value;
  if FPrecision <= 0 then
    FPrecision := cnstPrecision;
end;

procedure TsgCADtoSVG.SetTransparency(const AValue: Boolean);
begin
  FTransparency := AValue;
end;

function TsgCADtoSVG.IntToStrInternal(const AValue: Integer): string;
begin
  Result := DoubleToStrInternal(AValue);
end;

function TsgCADtoSVG.DoubleToStrInternal(const AValue: Double): string;
begin
  Result := ValueToStrInternal(AValue * FPrecision);
end;

function TsgCADtoSVG.ValueToStrInternal(const AValue: Double): string;
begin
  Result := DoubleToStr(AValue, cnstPoint);
end;

{ TsgCADtoSVGZ }

function TsgCADtoSVGZ.GetCompressed: Boolean;
begin
  Result := True;
end;

end.
