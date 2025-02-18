unit sgCadProxyEditor;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  {$IFDEF SG_LINUX_FPC}
  LCLType, LCLIntf, LMessages,
  {$ENDIF}
{$ELSE}
  Types,
{$ENDIF}
  Messages, Forms, XMLInterface, sgConsts, sgFunction,
  sgDrawingNavigator, SysUtils, Classes, Graphics, Controls, CADImage,
  DXFConv;

type
  TsgMainFormFrame = class
  public
    constructor Create(const AForm: TObject); virtual; abstract;
    function Open(const AFileName: string): Boolean; virtual; abstract;
    function Save(const ASource: TGraphic; const AFileName: string;
      const AFormat: TsgExportFormat;
      const AVersion: TsgDWGVersion): Boolean; virtual; abstract;
    function GetImage: TsgCADImage; virtual; abstract;
    function GetDrawingNavigator: TsgDrawingNavigator; virtual; abstract;
  end;

  { TsgViewerEditor }

  TsgViewerEditor = class(TsgProxyEditor)
  private
    FLockDeleteEntities: Boolean;
    FSelectedEntities: TList;
    FMainApp: TObject;
    procedure UpdateExtents;
    procedure ViewerEditorPaint(Sender: TObject);
  protected
    class procedure sgInvalidateRect(hWnd: HWND; const ARect: TRect;
      const AErase: Boolean = True);
    function GetImage: TsgCADImage;
    function GetPaintBox: TsgDrawingNavigator;
    procedure DoPaint(const ACanvas: TCanvas); virtual;
    function GetImageCoords(const APoint: TPoint): TFPoint; override;
    function GetScreenCoords(const APoint: TFPoint): TPoint; override;
    function GetScreenBox(const ABox: TFRect): TRect;
    function GetSelectEntitiesBox: TFRect;
//----
    function GetCoordsMDown: TFPoint; override;
    function GetCoordsMMove: TFPoint; override;
    function GetCoordsMUp: TFPoint; override;
    function GetCoordMObject: TObject; override;
    function GetViewRect: TRect; override;
    function GetViewFRect: TFRect; override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(const AValue: Boolean); override;
//----
  public
    Log: TStringList;
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure Clear; virtual;
    procedure CalcExtents; override;
    procedure Changed; override;
    procedure DeleteEntities; override;
    procedure GetSelectedEntities(const AList: TList); override;
    procedure FitToSize; override;
    function AfterChange: Integer; override;
    function BeforeChange(const AEntities: TList): Integer; override;
    function ResultList: TStringList; override;
    function SaveImage(const AName: string; const AIsDWG: Boolean;
      AVer: TsgDWGVersion = acR2004;
      IsConvertImageToOLE: Boolean = True): Boolean; override;
    function SetSelectedEntities(const AList: TList): Integer; override;
    procedure Invalidate; override;
    procedure InvalidateFRect(const ARect: TFRect); override;
    procedure ShowSelectEntities; override;
//----
    procedure BeginEnt(const AEnt: TsgDXFEntity; const AInternal: Boolean); override;
    procedure BeginList(const AList: TList; const ACount: Integer; const AInternal: Boolean); override;
    procedure EndEnt(const AEnt: TsgDXFEntity); override;
    procedure EndList(const AList: TList; const ACount: Integer); override;
    function CreatingHatch(const APatternName: string;
      const AScale, AAngle: Double; const AColor1: PsgColorCAD;
      const AColor2: PsgColorCAD = nil): TsgDXFEntity; override;
    procedure ShowCADRect(ARect: TFRect); override;
    procedure ShowWindowRect(ARect: TRect); override;
    function IsChanged: Boolean; override;
    function LispRun(const AText: string; AResults: TStrings = nil): Integer; override;
    procedure OpenVPort; override;
    procedure RecreateBlockAsTemplate; override;
    procedure GetShowEntitySettings(var AMethod: TsgShowEntityMethod;
      var APrecision: Double); override;
    procedure SetShowEntitySettings(const AMethod: TsgShowEntityMethod;
      const APrecision: Double = 10); override;
    function GetCustomDrawParams(const AHandle: UInt64): TsgCustomSelectMode; override;
    procedure SetCustomDrawParams(const AHandle: UInt64;
      const ACustomDraw: TsgCustomSelectMode;
      const AUnregister: Boolean); override;
//---
  end;

  TsgXmlMainApplication = class(TInterfacedObject, IsgMainApplication)
  private
    FEditor: TsgViewerEditor;
    FMainForm: TsgMainFormFrame;
    procedure SetMainForm(const AValue: TsgMainFormFrame);
  protected
    function CreateEditor: TsgViewerEditor; virtual;
    function sgPaintBox: TsgDrawingNavigator;
    function SetCadProps: Boolean;
  public
    constructor Create(const AMainForm: TsgMainFormFrame);
    destructor Destroy; override;
    procedure Clear;
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
    function ApplyXML(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    procedure SetCurrDrawingSizes(const AWidth, AHeight: Integer);
    procedure SetDrawingChange(const AGuid: TGUID; const AMode: Integer; const AChange: Boolean);
    function CloseDrawingGuid(const AGuid: TGUID): Boolean;
    function GetActualGraphic(const AExportParams: TObject): {$IFNDEF SG_FIREMONKEY}TGraphic{$ELSE}TObject{$ENDIF};
    function GetDrawingsCount: Integer;
    function GetDrawingGuid(const AIndex: Integer): TsgDrawingXMLData;
    function GetCurrentDrawingGuid: TGUID;
    function GetCADEditorCurrent: TObject;
    function GetViewRect: TRect;
    function GetViewFRect: TFRect;
    procedure SetViewFRect(const ARect: TFRect);
    procedure WriteError(const AMessage: string);
    function RotateCurrentDrawing(const AX, AY, AZ: Double): Integer;
    procedure SetCurrentDrawingGuid(const AValue: TGUID);
    procedure MainUpdate(const AMode: TsgMainUpdateMode = mumSilent);
    function CommandReport(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandSpecification(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandGetData(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandSetData(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandCreateBtiEntity(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function CommandXml(const AXMLParams: string; AOut: IsgXMLOut): Integer;
    function SaveCompleate: Boolean;
    procedure UpdateLayouts(AIsSetDefault: Boolean);
    procedure PaintExternal(Sender: TObject);
    property MainForm: TsgMainFormFrame read FMainForm;
    property ViewerEditor: TsgViewerEditor read FEditor;
  end;

implementation

type
  TsgDrawingNavigatorAccess = class(TsgDrawingNavigator);

{ TsgXmlMainApplication }

function TsgXmlMainApplication.ApplyXML(const AXMLParams: string;
  AOut: IsgXMLOut): Integer;
begin
  Result := 0;
end;

procedure TsgXmlMainApplication.Clear;
begin
  if Assigned(FEditor) then
    FEditor.Clear;
end;

function TsgXmlMainApplication.CloseDrawingGuid(const AGuid: TGUID): Boolean;
begin
  Result := False;
end;

function TsgXmlMainApplication.CommandCreateBtiEntity(const AXMLParams: string;
  AOut: IsgXMLOut): Integer;
begin
  Result := 0;
end;

function TsgXmlMainApplication.CommandGetData(const AXMLParams: string;
  AOut: IsgXMLOut): Integer;
begin
  Result := -1;
end;

function TsgXmlMainApplication.CommandReport(const AXMLParams: string;
  AOut: IsgXMLOut): Integer;
begin
  Result := -1;
end;

function TsgXmlMainApplication.CommandSetData(const AXMLParams: string;
  AOut: IsgXMLOut): Integer;
begin
  Result := -1;
end;

function TsgXmlMainApplication.CommandSpecification(const AXMLParams: string;
  AOut: IsgXMLOut): Integer;
begin
  Result := 0;
end;

function TsgXmlMainApplication.CommandXml(const AXMLParams: string;
  AOut: IsgXMLOut): Integer;
begin
  Result := 0;
end;

constructor TsgXmlMainApplication.Create(const AMainForm: TsgMainFormFrame);
begin
  inherited Create;
  SetMainForm(AMainForm);
  FEditor := CreateEditor;
end;

function TsgXmlMainApplication.CreateDrawing: TGUID;
begin
  Result := cnstGUID_NULL;
end;

function TsgXmlMainApplication.CreateEditor: TsgViewerEditor;
begin
  Result := TsgViewerEditor.Create(Self);
end;

procedure TsgXmlMainApplication.CreateUserMenu(const ANames: TStrings;
  const ASenders, AClicks, AOnOPopups, AImageIndexes: TList;
  const AMode: Integer; const AIconsBitmap: TBitmap);
begin
end;

destructor TsgXmlMainApplication.Destroy;
begin
  FreeAndNil(FMainForm);
  FreeAndNil(FEditor);
  inherited Destroy;
end;

function TsgXmlMainApplication.GetActualGraphic(
  const AExportParams: TObject): TGraphic;
begin
  Result := nil;
end;

function TsgXmlMainApplication.GetCADEditorCurrent: TObject;
begin
  Result := nil;
end;

function TsgXmlMainApplication.GetCurrentDrawingGuid: TGUID;
begin
  Result := cnstFailNewDrawing;
  if Assigned(sgPaintBox.Picture.Graphic) then
    Result := TsgCADImage(sgPaintBox.Picture.Graphic).Guid;
end;

function TsgXmlMainApplication.GetDrawingGuid(
  const AIndex: Integer): TsgDrawingXMLData;
var
  vCADImage: TsgCADImage;
begin
  Result.Name := '';
  FillChar(Result, SizeOf(Result), #0);
  vCADImage := sgPaintBox.Picture.Graphic as TsgCADImage;
  if Assigned(vCADImage) then
  begin
    Result.Guid := vCADImage.Guid;
    Result.Name := vCADImage.FileName;
    Result.Current := True;
  end;
end;

function TsgXmlMainApplication.GetDrawingsCount: Integer;
begin
  Result := 0;
  if Assigned(sgPaintBox.Picture.Graphic) then
    Result := 1;
end;

function TsgXmlMainApplication.GetViewFRect: TFRect;
begin
  Result := FEditor.GetViewFRect;
end;

function TsgXmlMainApplication.GetViewRect: TRect;
begin
  Result := FEditor.GetViewRect;
end;

function TsgXmlMainApplication.LoadFromStream(AStream: TStream; AExt: string;
  const AMode: string; const AGuid: PGUID): Boolean;
var
  vDirTemp, vFileTemp: string;
begin
  Result := False;
  if Assigned(AStream) and (Length(AExt) > 0) then
  begin
    vDirTemp := sgFunction.GetTempDir;
    if Length(vDirTemp) > 0 then
    begin
      vFileTemp := AddLastSlash(vDirTemp) + IntToHex(Uint64(AStream), 0) + AExt;
      try
        if OpenFile(vFileTemp, False, AMode, AGuid) then
          Result := True;
      finally
        try
          DeleteFile(vFileTemp);
        except
        end;
      end;
    end;
  end;
end;

procedure TsgXmlMainApplication.MainUpdate(const AMode: TsgMainUpdateMode);
begin
end;

function TsgXmlMainApplication.OpenFile(AFileName: string; AddResent: Boolean;
  const AMode: string; const AGuid: PGUID; AASync: Boolean): Boolean;
begin
  Result := False;
  try
    MainForm.Open(AFileName);
    if SetCadProps then
      Result := True;
  except
    Result := False;
  end;
end;

procedure TsgXmlMainApplication.PaintExternal(Sender: TObject);
begin
  if Assigned(FEditor) then
    FEditor.ViewerEditorPaint(Sender);
end;

function TsgXmlMainApplication.RotateCurrentDrawing(const AX, AY,
  AZ: Double): Integer;
begin
  Result := 0;
end;

function TsgXmlMainApplication.SaveBitmapToStream(const ASource: TBitmap;
  const AExportParams: TObject; const AName: string;
  const AStream: TStream): Boolean;
begin
  Result := Assigned(ASource) and Assigned(AStream);
  if Result then
    ASource.SaveToStream(AStream);
end;

function TsgXmlMainApplication.SaveCompleate: Boolean;
begin
  Result := True;
end;

function TsgXmlMainApplication.SaveFileWithXmlParams(const AXMLParams: string;
  AStorage: TsgStorage; var AFormat: Integer): Integer;
var
  vFileName, vExt: string;
  vFormat: TsgExportFormat;
  vImage: TsgCADImage;
  vVersion: TsgDWGVersion;
begin
  Result := cnstSaveResultError;
  if not Assigned(FMainForm) then Exit;
  begin
    vImage := FMainForm.GetImage;
    if Assigned(vImage) then
    begin
      try
        vFileName := AStorage.FileName;
        vExt := ExtractFileExt(vFileName);
        vFormat := GetExportFormat(vExt);
        if vFormat = efAuto then
          Result := cnstSaveResultUndefinedFormat
        else
        begin
          if vImage is TsgImageWithHandles then
            vVersion := TsgDWGVersion(vImage.Converter.HeadVarStruct.Version)
          else
            vVersion := acR2004;
          FMainForm.Save(vImage, vExt, vFormat, vVersion);
          Result := cnstSaveResultOk;
        end;
      except
        Result := cnstSaveResultError;
      end;
    end;
  end;
end;

function TsgXmlMainApplication.SetCadProps: Boolean;
var
  vCADImage: TsgCADImage;
begin
  Result := False;
  if sgPaintBox.Picture.Graphic is TsgCADImage then
  begin
    Result := True;
    vCADImage := TsgCADImage(sgPaintBox.Picture.Graphic);
    if not Assigned(vCADImage.CurrentLayout) then
      vCADImage.CurrentLayout := vCADImage.Layouts[0];
  end;
end;

procedure TsgXmlMainApplication.SetCurrDrawingSizes(const AWidth,
  AHeight: Integer);
begin
end;

procedure TsgXmlMainApplication.SetCurrentDrawingGuid(const AValue: TGUID);
begin

end;

procedure TsgXmlMainApplication.SetDrawingChange(const AGuid: TGUID;
  const AMode: Integer; const AChange: Boolean);
begin
end;

procedure TsgXmlMainApplication.SetMainForm(const AValue: TsgMainFormFrame);
begin
  FreeAndNil(FMainForm);
  FMainForm := AValue;
end;

procedure TsgXmlMainApplication.SetRibbonVisible(const Value: Boolean);
begin
end;

procedure TsgXmlMainApplication.SetViewFRect(const ARect: TFRect);
var
  vPaintBox: TsgDrawingNavigator;
begin
  vPaintBox := MainForm.GetDrawingNavigator;
  if Assigned(vPaintBox) then
    vPaintBox.ShowRect(ARect);
end;

function TsgXmlMainApplication.sgPaintBox: TsgDrawingNavigator;
begin
  Result := FMainForm.GetDrawingNavigator;
end;

procedure TsgXmlMainApplication.UpdateLayouts(AIsSetDefault: Boolean);
begin
end;

procedure TsgXmlMainApplication.WriteError(const AMessage: string);
begin
end;

{ TsgViewerEditor }


procedure TsgViewerEditor.UpdateExtents;
var
  vImage: TsgCADImage;
begin
  vImage := GetImage;
  if Assigned(vImage) then
  begin
    if Assigned(vImage.CurrentLayout) then
    begin
      vImage.Converter.Loads(vImage.CurrentLayout);
//      if Assigned(FDrawRect) then
//        FDrawRect.Box := FCADImage.CurrentLayout.Box;
      vImage.RefreshCurrentLayout;
    end;
  end;
end;

procedure TsgViewerEditor.ViewerEditorPaint(Sender: TObject);
var
  vPaintBox: TsgDrawingNavigator;
begin
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
    DoPaint(vPaintBox.Canvas);
end;

class procedure TsgViewerEditor.sgInvalidateRect(hWnd: HWND;
  const ARect: TRect; const AErase: Boolean = True);
begin
  InvalidateRect(hWnd, @ARect, AErase);
end;

function TsgViewerEditor.AfterChange: Integer;
begin
  Result := 0;
  EndUpdate;
end;

function TsgViewerEditor.BeforeChange(const AEntities: TList): Integer;
begin
  Result := 0;
  BeginUpdate;
  if FSelectedEntities.Count > 0 then
    CopyLists(AEntities, FSelectedEntities, cmAppend);
end;

procedure TsgViewerEditor.BeginEnt(const AEnt: TsgDXFEntity;
  const AInternal: Boolean);
begin
end;

procedure TsgViewerEditor.BeginList(const AList: TList; const ACount: Integer;
  const AInternal: Boolean);
begin
end;

procedure TsgViewerEditor.BeginUpdate;
begin
end;

procedure TsgViewerEditor.CalcExtents;
begin
  UpdateExtents;
end;

procedure TsgViewerEditor.Changed;
begin
end;

procedure TsgViewerEditor.Clear;
begin
  FSelectedEntities.Count := 0;
end;

constructor TsgViewerEditor.Create(const AOwner: TObject);
begin
  inherited Create(nil);
  FSelectedEntities := TList.Create;
  Log := TStringList.Create;
  if AOwner is TsgXmlMainApplication then
    FMainApp := TsgXmlMainApplication(AOwner);
end;


function TsgViewerEditor.CreatingHatch(const APatternName: string;
  const AScale, AAngle: Double; const AColor1: PsgColorCAD;
  const AColor2: PsgColorCAD): TsgDXFEntity;
begin
  Result := nil;
end;

procedure TsgViewerEditor.DeleteEntities;
var
  I: Integer;
  vImage: TsgCADImage;
  vEntity: TsgDXFEntity;
begin
  if FLockDeleteEntities then Exit;
  FLockDeleteEntities := True;
  BeginUpdate;
  try
    vImage := GetImage;
    for I := FSelectedEntities.Count - 1 downto 0 do
    begin
      vEntity := FSelectedEntities[I];
      if vImage.CurrentLayout.RemoveEntity(vEntity) then
      begin
        FSelectedEntities.Delete(I);
        vEntity.Free;
      end;
    end;
  finally
    EndUpdate;
    FLockDeleteEntities := False;
  end;
end;

destructor TsgViewerEditor.Destroy;
begin
  FMainApp := nil;
  FreeAndNil(FSelectedEntities);
  FreeAndNil(Log);
  inherited Destroy;
end;

procedure TsgViewerEditor.DoPaint(const ACanvas: TCanvas);
var
  vImage: TsgCADImage;
  I: Integer;
  vEnt: TsgDXFEntity;
  vBox: TFRect;
  vDrawRect: TRect;
begin
  vImage := GetImage;
  if not Assigned(vImage) then Exit;
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsClear;
  for I := 0 to FSelectedEntities.Count - 1 do
  begin
    vEnt := TsgDXFEntity(FSelectedEntities[I]);
    vBox := vEnt.Box;
    if not IsBadRect(vBox) then
    begin
      vDrawRect := GetScreenBox(vBox);
      if (vDrawRect.Right <> vDrawRect.Left) or  (vDrawRect.Top <> vDrawRect.Left) then
        ACanvas.Rectangle(vDrawRect);
    end;
  end;
end;

procedure TsgViewerEditor.EndEnt(const AEnt: TsgDXFEntity);
begin
end;

procedure TsgViewerEditor.EndList(const AList: TList; const ACount: Integer);
begin
end;

procedure TsgViewerEditor.EndUpdate;
var
  vPaintBox: TsgDrawingNavigator;
begin
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
    vPaintBox.Invalidate;
end;

procedure TsgViewerEditor.FitToSize;
var
  vNavigator: TsgDrawingNavigator;
begin
  vNavigator := GetPaintBox;
  if Assigned(vNavigator) then
    vNavigator.FitToSize;
end;

function TsgViewerEditor.GetCoordMObject: TObject;
begin
  Result := nil;
end;

function TsgViewerEditor.GetCoordsMDown: TFPoint;
begin
  Result := cnstFPointZero;
end;

function TsgViewerEditor.GetCoordsMMove: TFPoint;
begin
  Result := cnstFPointZero;
end;

function TsgViewerEditor.GetCoordsMUp: TFPoint;
begin
  Result := cnstFPointZero;
end;

function TsgViewerEditor.GetCustomDrawParams(
  const AHandle: UInt64): TsgCustomSelectMode;
begin
  FillChar(Result, SizeOf(Result), #0);
end;

function TsgViewerEditor.GetImage: TsgCADImage;
begin
  Result := TsgXmlMainApplication(FMainApp).MainForm.GetImage;
end;

function TsgViewerEditor.GetImageCoords(const APoint: TPoint): TFPoint;
var
  vPaintBox: TsgDrawingNavigator;
  vUnits: string;
begin
  Result := cnstFPointZero;
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
    Result := vPaintBox.GetDrawingCoords(APoint.X, APoint.Y, vUnits);
end;

function TsgViewerEditor.GetScreenBox(const ABox: TFRect): TRect;
var
  vImage: TsgCADImage;
begin
  vImage := GetImage;
  if Assigned(vImage) then
  begin
    Result.TopLeft := vImage.GetPoint(ABox.TopLeft);
    Result.BottomRight := Result.TopLeft;
    ExpandRect(Result, vImage.GetPoint(ABox.BottomRight));
    ExpandRect(Result, vImage.GetPoint(MakeFPoint(ABox.Right, ABox.Top, ABox.Z1)));
    ExpandRect(Result, vImage.GetPoint(MakeFPoint(ABox.Right, ABox.Top, ABox.Z2)));
    ExpandRect(Result, vImage.GetPoint(MakeFPoint(ABox.Left, ABox.Bottom, ABox.Z1)));
    ExpandRect(Result, vImage.GetPoint(MakeFPoint(ABox.Left, ABox.Bottom, ABox.Z2)));
    ExpandRect(Result, vImage.GetPoint(MakeFPoint(ABox.Left, ABox.Bottom, ABox.Z2)));
    ExpandRect(Result, vImage.GetPoint(MakeFPoint(ABox.Right, ABox.Bottom, ABox.Z1)));
  end
  else
  begin
    Result.TopLeft := cnstPointZero;
    Result.BottomRight := cnstPointZero;
  end;
end;

function TsgViewerEditor.GetScreenCoords(const APoint: TFPoint): TPoint;
var
  vImage: TsgCADImage;
begin
  Result := cnstPointZero;
  vImage := GetImage;
  if Assigned(vImage) then
    Result := vImage.GetPoint(APoint);
end;

procedure TsgViewerEditor.GetSelectedEntities(const AList: TList);
begin
  if Assigned(FSelectedEntities) and (FSelectedEntities.Count > 0) then
    CopyLists(AList, FSelectedEntities);
end;

function TsgViewerEditor.GetSelectEntitiesBox: TFRect;
var
  I: Integer;
begin
  Result := cnstBadRect;
  for I := 0 to FSelectedEntities.Count - 1 do
    UnionFRect(Result, TsgDXFEntity(FSelectedEntities[I]).Box);
end;

procedure TsgViewerEditor.GetShowEntitySettings(
  var AMethod: TsgShowEntityMethod; var APrecision: Double);
begin
end;

function TsgViewerEditor.GetViewRect: TRect;
var
  vPaintBox: TsgDrawingNavigator;
begin
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
    Result := vPaintBox.ClientRect
  else
    Result := cnstBad2DRect;
end;

function TsgViewerEditor.GetViewFRect: TFRect;
var
  vViewRect: TRect;
begin
  vViewRect := GetViewRect;
  if not IsBadRectI(vViewRect) then
  begin
    ExpandFRect(Result, GetImageCoords(vViewRect.TopLeft));
    ExpandFRect(Result, GetImageCoords(vViewRect.BottomRight));
    ExpandFRect(Result, GetImageCoords(Point(vViewRect.Left, vViewRect.Bottom)));
    ExpandFRect(Result, GetImageCoords(Point(vViewRect.Right, vViewRect.Top)));
  end
  else
    Result := cnstBadRect;
end;

procedure TsgViewerEditor.Invalidate;
var
  vPaintBox: TsgDrawingNavigator;
begin
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
    vPaintBox.Invalidate;
end;

procedure TsgViewerEditor.InvalidateFRect(const ARect: TFRect);
var
  vPaintBox: TsgDrawingNavigator;
begin
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
  begin
    if not IsBadRect(ARect) then
      sgInvalidateRect(vPaintBox.Handle, GetScreenBox(ARect), True);
  end;
end;

function TsgViewerEditor.IsChanged: Boolean;
begin
  Result := False;
end;

function TsgViewerEditor.LispRun(const AText: string; AResults: TStrings): Integer;
begin
  Result := 0;
end;

procedure TsgViewerEditor.OpenVPort;
var
  vPaintBox: TsgDrawingNavigator;
begin
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
    TsgDrawingNavigatorAccess(vPaintBox).OpenVPort;
end;

procedure TsgViewerEditor.RecreateBlockAsTemplate;
begin
end;

function TsgViewerEditor.ResultList: TStringList;
begin
  Result := Log;
end;

function TsgViewerEditor.SaveImage(const AName: string; const AIsDWG: Boolean;
  AVer: TsgDWGVersion; IsConvertImageToOLE: Boolean): Boolean;
begin
  Result := False;
end;

procedure TsgViewerEditor.SetCustomDrawParams(const AHandle: UInt64;
  const ACustomDraw: TsgCustomSelectMode; const AUnregister: Boolean);
begin
end;

procedure TsgViewerEditor.SetReadOnly(const AValue: Boolean);
begin
end;

function TsgViewerEditor.SetSelectedEntities(const AList: TList): Integer;
begin
  BeginUpdate;
  try
    FSelectedEntities.Clear;
    if Assigned(AList) and (AList.Count > 0) then
      CopyLists(FSelectedEntities, AList);
    Result := FSelectedEntities.Count;
  finally
    EndUpdate;
  end;
end;

procedure TsgViewerEditor.SetShowEntitySettings(
  const AMethod: TsgShowEntityMethod; const APrecision: Double);
begin
end;

procedure TsgViewerEditor.ShowCADRect(ARect: TFRect);
begin
end;

procedure TsgViewerEditor.ShowSelectEntities;
var
  vPaintBox: TsgDrawingNavigator;
  vShowRect: TFRect;
begin
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
  begin
    vShowRect := GetSelectEntitiesBox;
    if not IsBadRect(vShowRect) then
      vPaintBox.ShowRect(vShowRect);
  end;
end;

procedure TsgViewerEditor.ShowWindowRect(ARect: TRect);
var
  vPaintBox: TsgDrawingNavigator;
begin
  vPaintBox := GetPaintBox;
  if Assigned(vPaintBox) then
    TsgDrawingNavigatorAccess(vPaintBox).ZoomRect(ARect);
end;

function TsgViewerEditor.GetPaintBox: TsgDrawingNavigator;
begin
  Result := TsgXmlMainApplication(FMainApp).MainForm.GetDrawingNavigator;
end;

function TsgViewerEditor.GetReadOnly: Boolean;
begin
  Result := False;
end;

end.
