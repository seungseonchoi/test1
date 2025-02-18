unit DXFExportDirect;
{$INCLUDE SGDXF.inc}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows, {$IFNDEF SGFPC}sgMetafiles,{$ENDIF}
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils, Classes, Math, Graphics, sgConsts, sgFunction, sgLists,
  CADImage, CADtoDXF, CADtoGCode, CADDirectExport, CADExport, DXFConv
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
  {$IFDEF SGDEL_6}, Types{$ENDIF}
{$ENDIF};

type
  TsgDXFExportDirect = class(TsgCADExport)
  private
    FCADImage: TsgCADImage;
    FExportFormat: TsgExportFormat;
    FExtData: Pointer;
    FLineWeightScale: Double;
    FOffset: TF2DPoint;
    FOwnerBox: TFRect;
    FAlternateBlack: Boolean;
    FVersion: TsgDWGVersion;
    FUnitSize: Double;
    FScale: Double;
    FUse01MM: Boolean;
    FIsTrial: Boolean;
    FMessage: string;
    FAppId: string;
    FCanvas: TCanvas;
{$IFDEF SG_DELPHI_VCL}
    FMetafile: TMetafile;
{$ENDIF}
    function GetClassExport: TsgCADExportClass;
    function GetCanvas: TCanvas;
    function GetExportFormat: TsgExportFormat;
    procedure SetAlternateBlack(AValue: Boolean);
    procedure SetOffset(AValue: TF2DPoint);
    procedure SetOwnerBox(const AValue: TFRect);
    procedure SetScale(AValue: Double);
    procedure SetUse01MM(AValue: Boolean);
    procedure SetUnitSize(AValue: Double);
    procedure SetVersion(AVersion: TsgDWGVersion);
    procedure SetExportFormat(const Value: TsgExportFormat);
  protected
    function GetConverter: TsgDXFConverter; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStreamCustom(S: TStream); override;
    procedure InitializeVersionData(const AIsTrial: Boolean;
      const AMessage: string; AppId: string = ''); override;
    procedure SetOffsetInternal(const AType: Byte; const AValue: Double);
    procedure SetCADImage(const ACADImage: TsgCADImage);
    property Image: TsgCADImage read FCADImage;
  public
    constructor Create(ACADImage: TsgCADImage); override;
    destructor Destroy; override;
    procedure ApplyParams(const AExportParams: TsgExportParams); override;
{$IFDEF SG_DELPHI_VCL}
    procedure LoadFromMetafile(AMetafile: TMetafile);
    procedure LoadFromMetafileStream(Stream: TStream);
{$ENDIF}
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToStream(S: TStream); override;
    procedure SaveToFile(const AFileName: String); override;

    procedure BeginDraw;
    procedure EndDraw;//apply canvas
    property Canvas: TCanvas read GetCanvas;

    property ExportFormat: TsgExportFormat read GetExportFormat write SetExportFormat;
    property AlternateBlack: Boolean read FAlternateBlack write SetAlternateBlack;
    property Offset: TF2DPoint read FOffset write SetOffset;
    property OwnerBox: TFRect read FOwnerBox write SetOwnerBox;
    property Scale: Double read FScale write SetScale;
    property UnitSize: Double read FUnitSize write SetUnitSize;
    property Use01MM: Boolean read FUse01MM write SetUse01MM;
    property Version: TsgDWGVersion read FVersion write SetVersion;
  end;

  TsgDXFExport = class(TsgDXFExportDirect)
  private
    function GetDefaultLTypeScale: Double;
    function GetMillimetres: Boolean;
    function GetOffsetX: Double;
    function GetOffsetY: Double;
    procedure SetDefaultLTypeScale(const AValue: Double);
    procedure SetMillimetres(const AValue: Boolean);
    procedure SetOffsetX(const AValue: Double);
    procedure SetOffsetY(const AValue: Double);
  public
    constructor Create; reintroduce; virtual;
    property DefaultLTypeScale: Double read GetDefaultLTypeScale write SetDefaultLTypeScale;
    property Millimetres: Boolean read GetMillimetres write SetMillimetres;
    property OffsetX: Double read GetOffsetX write SetOffsetX;
    property OffsetY: Double read GetOffsetY write SetOffsetY;
  end;

implementation

uses
  CADtoDWG;

type
  TsgCADDirectExportAccess = class(TsgCADDirectExport);

procedure TsgDXFExportDirect.BeginDraw;
begin
  FreeAndNil(FCanvas);
{$IFDEF SG_DELPHI_VCL}
  FreeAndNil(FMetafile);
{$ENDIF}
end;

constructor TsgDXFExportDirect.Create(ACADImage: TsgCADImage);
begin
  inherited Create(ACADImage);
  FOwnerBox := cnstBadRect;
  FExportFormat := efDxf;
  FExtData := nil;
  FVersion := acR2004;
  FAlternateBlack := False;
  FUnitSize := 1;
  FUse01MM := False;
  FScale := 1;
  FLineWeightScale := 1;
  SetCADImage(ACADImage);
end;

destructor TsgDXFExportDirect.Destroy;
begin
  try
    FreeAndNil(FCADImage);
  except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
  end;
  FreeAndNil(FCanvas);
{$IFDEF SG_DELPHI_VCL}
  FreeAndNil(FMetafile);
{$ENDIF}
  inherited Destroy;
end;

procedure TsgDXFExportDirect.EndDraw;
begin
  try
    FreeAndNil(FCanvas);
{$IFDEF SG_DELPHI_VCL}
    LoadFromMetafile(FMetafile);
{$ENDIF}
  finally
{$IFDEF SG_DELPHI_VCL}
    FreeAndNil(FMetafile);
{$ENDIF}
  end;
end;

procedure TsgDXFExportDirect.InitializeVersionData(const AIsTrial: Boolean;
  const AMessage: string; AppId: string);
begin
  FIsTrial := AIsTrial;
  FMessage := AMessage;
  FAppId := AppId;
end;

procedure TsgDXFExportDirect.SetOffsetInternal(const AType: Byte; const AValue: Double);
begin
  case AType of
    0:  FOffset.V[0] := AValue;
    1:  FOffset.V[1] := AValue;
  end;
end;

procedure TsgDXFExportDirect.ApplyParams(const AExportParams: TsgExportParams);
begin
  inherited ApplyParams(AExportParams);
  FExtData := AExportParams.ExtData;
  FVersion := AExportParams.Version;
  FAlternateBlack := AExportParams.AlternateBlack;
  FUnitSize := AExportParams.UnitSize;
  FUse01MM := AExportParams.Use01MM;
  FScale := AExportParams.Scale;
  FOffset := AExportParams.OffsetPoint;
  FLineWeightScale := AExportParams.LineWeightScale;
end;

procedure TsgDXFExportDirect.SetAlternateBlack(AValue: Boolean);
begin
  FAlternateBlack := AValue;
end;

procedure TsgDXFExportDirect.SetCADImage(const ACADImage: TsgCADImage);
begin
  FCADImage := ACADImage;
end;

procedure TsgDXFExportDirect.SetOffset(AValue: TF2DPoint);
begin
  FOffset := AValue;
end;

procedure TsgDXFExportDirect.SetOwnerBox(const AValue: TFRect);
begin
  FOwnerBox := AValue;
end;

procedure TsgDXFExportDirect.SetScale(AValue: Double);
begin
  FScale := AValue;
end;

procedure TsgDXFExportDirect.SetUse01MM(AValue: Boolean);
begin
  FUse01MM := AValue;
end;

procedure TsgDXFExportDirect.SetUnitSize(AValue: Double);
begin
  FUnitSize := AValue;
end;

procedure TsgDXFExportDirect.SetVersion(AVersion: TsgDWGVersion);
begin
  FVersion := AVersion;
end;

procedure TsgDXFExportDirect.SetExportFormat(const Value: TsgExportFormat);
begin
  if Value in [efDwg, efDxf, efDxt, efGCode] then
    FExportFormat := Value;
end;

procedure TsgDXFExportDirect.SaveToStreamCustom(S: TStream);
begin
  SaveToStream(S);
end;

procedure TsgDXFExportDirect.LoadFromFile(const AFileName: string);
{$IFDEF SG_DELPHI_VCL}
var
  vStream: TFileStream;
{$ENDIF}
begin
{$IFDEF SG_DELPHI_VCL}
   vStream := TFileStream.Create(AFileName, fmOpenRead);
   try
     LoadFromMetafileStream(vStream);
   finally
     vStream.Free;
   end;
{$ENDIF}
end;

{$IFDEF SG_DELPHI_VCL}
procedure TsgDXFExportDirect.LoadFromMetafile(AMetafile: TMetafile);
var
  vMetafileLoader: TsgConvertMetafileToCad;
  vBox: TFRect;
begin
  try
    FreeAndNil(FCADImage);
  except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
  end;
  FCADImage := {TsgCADImage}TsgVectorImage.Create;
  try
    FCADImage.Converter.InitializeSectionsBegin;
    FCADImage.Converter.InitializeSectionsEnd;
    FCADImage.CurrentLayout := FCADImage.Layouts[0];
    if IsBadRect(FOwnerBox) then
      vBox := MakeFRectByPoints(cnstFPointZero, MakeFPointFrom2D(GetMetafileSize(AMetafile)))
    else
      vBox := FOwnerBox;
    vMetafileLoader := TsgConvertMetafileToCad.Create(FCADImage, nil, vBox, '', FExtData);
    try
      vMetafileLoader.AlternateBlack := AlternateBlack;
      vMetafileLoader.Use01MM := Use01MM;
      vMetafileLoader.Scale := Scale;
      vMetafileLoader.UnitSize := UnitSize;
      vMetafileLoader.OffsetX := Offset.X;
      vMetafileLoader.OffsetY := Offset.Y;
      vMetafileLoader.LineWeightScale := FLineWeightScale;
      vMetafileLoader.Convert(AMetafile, nil);
    finally
      vMetafileLoader.Free;
    end;
  except
    FreeAndNil(FCADImage);
  end;
end;
{$ENDIF}

function TsgDXFExportDirect.GetCanvas: TCanvas;
begin
  if not Assigned(FCanvas) then
  begin
{$IFDEF SG_DELPHI_VCL}
    FMetafile := TMetafile.Create;
    FCanvas := TMetafileCanvas.Create(FMetafile, 0);
{$ELSE}
    FCanvas := TCanvas.Create;
{$ENDIF}
  end;
  Result := FCanvas;
end;

function TsgDXFExportDirect.GetClassExport: TsgCADExportClass;
begin
  case FExportFormat of
    efDwg: Result := TsgCADtoDWG;
    efGCode: Result := TsgCADtoGCode;
  else
    Result := TsgCADtoDXF;
  end;
end;

function TsgDXFExportDirect.GetConverter: TsgDXFConverter;
begin
  if Assigned(FCADImage) then
    Result := FCADImage.Converter
  else
    Result := inherited GetConverter;
end;

function TsgDXFExportDirect.GetExportFormat: TsgExportFormat;
begin
  Result := FExportFormat;
end;

procedure TsgDXFExportDirect.LoadFromStream(Stream: TStream);
begin
{$IFDEF SG_DELPHI_VCL}
  LoadFromMetafileStream(Stream);
{$ENDIF}
end;

{$IFDEF SG_DELPHI_VCL}
procedure TsgDXFExportDirect.LoadFromMetafileStream(Stream: TStream);
var
  vMetafile: TMetafile;
begin
  vMetafile := TMetafile.Create;
  try
    vMetafile.LoadFromStream(Stream);
    LoadFromMetafile(vMetafile);
  finally
    vMetafile.Free;
  end;
end;
{$ENDIF}

procedure TsgDXFExportDirect.SaveToFile(const AFileName: String);
begin
  ExportFormat := sgConsts.GetExportFormat(ExtractFileExt(AFileName));
  inherited SaveToFile(AFileName);
end;

procedure TsgDXFExportDirect.SaveToStream(S: TStream);
var
  vCADtoFormat: TsgCADExport;
  vClassExport: TsgCADExportClass;
begin
  if Assigned(FCADImage) then
  begin
    FCADImage.GetExtents;
    vClassExport := GetClassExport;
    if Assigned(vClassExport) then
    begin
      vCADtoFormat := vClassExport.Create(FCADImage);
      try
        if vCADtoFormat is TsgCADDirectExport then
          TsgCADDirectExport(vCADtoFormat).Version := FVersion;
        TsgCADDirectExportAccess(vCADtoFormat).InitializeVersionData(FIsTrial, FMessage, FAppId);
        vCADtoFormat.SaveToStream(S);
      finally
        vCADtoFormat.Free;
      end;
    end;
  end;
end;

{ TsgDXFExport }

constructor TsgDXFExport.Create;
begin
  inherited Create(nil);
end;

function TsgDXFExport.GetDefaultLTypeScale: Double;
begin
  Result := 1;
end;

function TsgDXFExport.GetMillimetres: Boolean;
begin
  Result := False;
end;

function TsgDXFExport.GetOffsetX: Double;
begin
  Result := Offset.X;
end;

function TsgDXFExport.GetOffsetY: Double;
begin
  Result := Offset.Y;
end;

procedure TsgDXFExport.SetDefaultLTypeScale(const AValue: Double);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgDXFExport.SetMillimetres(const AValue: Boolean);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure TsgDXFExport.SetOffsetX(const AValue: Double);
begin
  SetOffsetInternal(0, AValue);
end;

procedure TsgDXFExport.SetOffsetY(const AValue: Double);
begin
  SetOffsetInternal(1, AValue);
end;

end.
