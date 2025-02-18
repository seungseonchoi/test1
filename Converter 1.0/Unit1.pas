unit Unit1;
{$INCLUDE CADSoftTools.inc}
//If you see error "File not found: 'CADSoftTools.INC'",
//then please contact info@cadsofttools.com to get a compilable version.
//If you have a compilable version, you need to copy the contents of
// Lib7/LibXE5win32/LibXE5win64... to Lib directory according to your
// Delphi version and target CPU platform.

{$INCLUDE SGDXF.inc}

{.$DEFINE SG_XMLAPI}

interface

uses
{$IFDEF MSWINDOWS}
   Windows,ShellAPI, ShlObj, ActiveX,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF SGFPC}
  FPImage, PrintersDlgs, LCLType, LCLIntf, sgShellAPI,
  SynEdit, SynHighlighterXML,
  {$ELSE}
  JPEG,
  {$ENDIF}
  ExtCtrls, StdCtrls, CADImage, DXFConv, DXF, ComCtrls, Menus, Printers, MVFont,
  ExtDlgs, Buttons, sgConsts, sgFunction, fBuy,
  DWG, HPGL2, CGM, SVG, DWF, sgSTLImage,
  {$IFDEF SG_XMLAPI}
  XMLInterface, sgCadProxyEditor,
  {$ENDIF}
  {$IFDEF CADExportVCL}
  CADtoDXF, CADtoSVG, CADtoCGM, CADtoHPGL, CADtoPDF, CADtoSWF, CADtoDWG,
  {$IFDEF SGDEL_2009}
    fPDFExportDlg,
  {$ENDIF}

  {$ENDIF}
  CADExport,
  fLayers,
  sgLines, sgSelection, IniFiles, SHX, Math, sgDrawingNavigator, sgTools
  {$IFDEF SGDEL_4}
  ,ActnList
  {$ENDIF}
{$IFDEF SG_USEGDIPLUS}
{$IFDEF SG_WINAPI_GDIPLUS}
  ,Winapi.GDIPAPI, Winapi.GDIPOBJ, Winapi.GDIPUTIL
{$ELSE}
  ,GDIPlus
{$ENDIF}
{$ENDIF}
{$IFDEF SGDEL_2007}
  ,GIFImg
{$ENDIF}
  , System.RegularExpressions, Vcl.OleAuto, ExcelXP
  ;

const
  sNotVectorial: string = 'Raster drawings can not be saved to CAD format';
  fAccuracy = 0.003;

type
  TSelectionMode = (smNone, smRectangle, smEllipse);

  TEditMemo = {$IFDEF FPC}SynEdit.TSynEdit{$ELSE}TMemo{$ENDIF};

  TRectangleAround = record
    Box: TFRect;
    PaperSpace: Integer;
  end;

  TsgXmlIdeForm = class(TCustomForm)
    InputXml: TEditMemo;
    OutputXml: TEditMemo;
  end;

  TInsertBounds = record
    Left: TsgFloat;
    Top: TsgFloat;
    Right: TsgFloat;
    Bottom: TsgFloat;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    Panel2: TPanel;
    prbProgress: TProgressBar;
    pgcDrawing: TPageControl;
    tbsImage: TTabSheet;
    tbsStructure: TTabSheet;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MIFile: TMenuItem;
    MIOpen: TMenuItem;
    MIClose: TMenuItem;
    N4: TMenuItem;
    MIExit: TMenuItem;
    pnlFileName: TPanel;
    pnlTime: TPanel;
    MIView: TMenuItem;
    PrintDialog1: TPrintDialog;
    MIPrint: TMenuItem;
    MIScale: TMenuItem;
    MIAuto: TMenuItem;
    MIx2: TMenuItem;
    MIx4: TMenuItem;
    MIx8: TMenuItem;
    MIx16: TMenuItem;
    MIPrev: TMenuItem;
    N1: TMenuItem;
    N251: TMenuItem;
    N501: TMenuItem;
    MIx1: TMenuItem;
    OpenDialog1: TOpenPictureDialog;
    Panel4: TPanel;
    BtnOpen: TSpeedButton;
    BtnPlus: TSpeedButton;
    BtnMinus: TSpeedButton;
    BtnReal: TSpeedButton;
    BtnPrev: TSpeedButton;
    BtnPrint: TSpeedButton;
    N2: TMenuItem;
    MIAbout: TMenuItem;
    BtnSave: TSpeedButton;
    mmiSaveAs: TMenuItem;
    SavePictureDialog: TSavePictureDialog;
    ImageColors1: TMenuItem;
    mmiBlack: TMenuItem;
    mmiNormal: TMenuItem;
    btnLayers: TSpeedButton;
    BtnPrint1: TSpeedButton;
    pnlCoords: TPanel;
    btnFitToScreen: TSpeedButton;
    btnIsWithoutBorder: TSpeedButton;
    btnDrawAllColor: TSpeedButton;
    btnBlackWhite: TSpeedButton;
    btnWhiteBackground: TSpeedButton;
    btnBlackBackground: TSpeedButton;
    btnShowLineWeight: TSpeedButton;
    btnSplittedArc: TSpeedButton;
    sb3DOrbit: TSpeedButton;
    BtnPrint2: TSpeedButton;
    PopupMenu1: TPopupMenu;
    Printer1: TMenuItem;
    Metafile1: TMenuItem;
    mmiSaveAsBigEMF: TMenuItem;
    SavePictureDialogEMF: TSavePictureDialog;
    pnlLayouts: TPanel;
    lblLayouts: TLabel;
    cbLayouts: TComboBox;
    sb3DAxes: TSpeedButton;
    mmiShowpoint: TMenuItem;
    mmiFindtext: TMenuItem;
    mmiReopenSameView: TMenuItem;
    miFonts: TMenuItem;
    N3: TMenuItem;
    sbFonts: TSpeedButton;
    sbDrid: TSpeedButton;
    sbDrawingBox: TSpeedButton;
    mmiReopenSWIsometric: TMenuItem;
    sbShowBorder: TSpeedButton;
    OpenBorder: TOpenPictureDialog;
    Snap1: TMenuItem;
    mmiObjectSnapAll: TMenuItem;
    imgFrame: TImage;
    edtNumPartsCircle: TEdit;
    udNumPartsCircle: TUpDown;
    edtNumPartsSpline: TEdit;
    udNumPartsSpline: TUpDown;
    edtGridCount: TEdit;
    udGridCount: TUpDown;
    lblNumPartsCircle: TLabel;
    lblNumPartsSpline: TLabel;
    lblDridCount: TLabel;
    N5: TMenuItem;
    pmiDoublebuffered: TMenuItem;
    pnlSize: TPanel;
    GotoHomepage1: TMenuItem;
    XMLAPI1: TMenuItem;
    ProccessXML1: TMenuItem;
    procedure MIOpenClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure MIPrintClick(Sender: TObject);
    procedure MIAutoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MIPrevClick(Sender: TObject);
    procedure BtnPlusClick(Sender: TObject);
    procedure BtnMinusClick(Sender: TObject);
    procedure pgcDrawingChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure MIHelpClick(Sender: TObject);
    procedure OpenDialog1Show(Sender: TObject);
    procedure OpenDialog1Close(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure mmiNormalClick(Sender: TObject);
    procedure SoftGoldHomePage1Click(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure btnLayersClick(Sender: TObject);
    procedure BtnPrint1Click(Sender: TObject);
    procedure btnFitToScreenClick(Sender: TObject);
    procedure btnIsWithoutBorderClick(Sender: TObject);
    procedure btnShowLineWeightClick(Sender: TObject);
    procedure btnSplittedArcClick(Sender: TObject);
    procedure btnDrawAllColorClick(Sender: TObject);
    procedure btnWhiteBackgroundClick(Sender: TObject);
    procedure sb3DOrbitClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Printer1Click(Sender: TObject);
    procedure Metafile1Click(Sender: TObject);
    procedure BtnPrint2Click(Sender: TObject);
    procedure mmiSaveAsBigEMFClick(Sender: TObject);
    procedure cbLayoutsChange(Sender: TObject);
    procedure pgcDrawingChange(Sender: TObject);
    procedure speNumPartsCircleChange(Sender: TObject);
    procedure speNumPartsSplineChange(Sender: TObject);
    procedure sb3DAxesClick(Sender: TObject);
    procedure mmiShowpointClick(Sender: TObject);
    procedure mmiFindtextClick(Sender: TObject);
    procedure mmiReopenSameViewClick(Sender: TObject);
    procedure miFontsClick(Sender: TObject);
    procedure speGridCountChange(Sender: TObject);
    procedure sbDrawingBoxClick(Sender: TObject);
    procedure mmiReopenSWIsometricClick(Sender: TObject);
    procedure sbShowBorderClick(Sender: TObject);
    procedure mmiObjectSnapAllClick(Sender: TObject);
    procedure actFormUpdate(Sender: TObject);
    procedure sbDridClick(Sender: TObject);
    procedure BtnRealClick(Sender: TObject);
    procedure mmiScaleClick(Sender: TObject);
    procedure pmiDoublebufferedClick(Sender: TObject);
    procedure GotoHomepage1Click(Sender: TObject);
    procedure ProccessXML1Click(Sender: TObject);
    procedure SaveToSVG(ACADImage: TsgCADImage; const AFileName: string);
    procedure SvgConvert();
    procedure SaveTextToCSV(ACADImage: TsgCADImage; const ACSVFileName, regExFile: string);
  private
{$IFDEF SG_XMLAPI}
    FXMLIDE: TsgXMLIDE;
    FXmlMainApp: TsgXmlMainApplication;
{$ENDIF}
    FSearchingText: string;
    FTextPoint: TFPoint;
    FTextBox: TFRect;
    FCADParams: TsgCADIterate;
    FSearchResult: Boolean;
    FFindFlag: Boolean;
    FSelectionMode: TSelectionMode;
    FSearchModeIsAll: Boolean;
    FSearchEntities: TList;
    {$IFDEF DEMO}
    FFrameInfoHandle: THandle;
    {$ENDIF}
    FFileName: string;
    FOptions: TIniFile;
    FStoredNullWidth: Integer;
    FsgPaintBox: TsgDrawingNavigator;
    FStart: Int64;
    FIntPoints: TList;
    {$IFDEF SGDEL_4}
    FactForm: TAction;
    {$ENDIF}
    FSaveDialogFilter: TStrings;
    FGraphic: TGraphic;
    FImg: TsgCADImage;
    FOnPaintExternal: TNotifyEvent;
    FXMLForm: TsgXmlIdeForm;
    procedure Enable3DOrbit(AIsEnabled: Boolean);
    procedure Enable3DAxes(AIsEnabled: Boolean);
    procedure LoadStructure;
    procedure Time;
    procedure EnableControls;
    procedure RestoreNullWidth(AGraphic: TGraphic);
    procedure SetImgOpt;
    procedure SetNullWidth(AGraphic: TGraphic; AWidth: Integer);
    procedure ViewLayouts;
    {$IFNDEF SGDEL_4}
    procedure UpdateActions;
    procedure DoActionIdle;
    {$ENDIF}
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure sgPaintBoxScaling(Sender: TObject);
    procedure sgPaintBoxProgress
    {$IFDEF SGFPC}
    (Sender: TObject; Stage: TFPImgProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: AnsiString; var Continue : Boolean);
    {$ELSE}
    (Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: String);
    {$ENDIF}
    procedure sgPaintBoxPaint(Sender: TObject);
    procedure sgPaintBoxPictureChange(Sender: TObject);
    procedure sgPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{$IFDEF DEMO}
    procedure DestroyFrameInfo;
{$ENDIF}
    function GetGraphic: TGraphic;
    function ExportCad(const ASource: TsgCADImage; const AFormat: TsgExportFormat;
      const AFileName: string; const AVersion: TsgDWGVersion): Boolean;
    function ExportRastr(const ASource: TsgCADImage;
      const AFormat: TsgExportFormat; const AFileName: string): Boolean;
    procedure UpdateXMLIDE;
    function DoProcessXML(const AXml: string): string;
  protected
    procedure ActualGraphic(var AResult: TGraphic);
    function AddRectToConv(ABox: TFRect): TsgDXFEntity;
    function CreateViewer(const AName: string): TsgXmlIdeForm;
    procedure DoXMLMessage(const S: WideString);
    procedure LoadXML(Sendr: TObject);
    procedure ExecuteProcessXML(Sender: TObject);
    procedure SelectSearchEntities;
    procedure LoadOptions;
    procedure BreakLoading(Sender: TObject);
    procedure DrawTo(const ADest: TObject; const ADstRect: TRect;
      const ASource: TGraphic); overload;
    function GetCADImage: TsgCADImage;    
    function GetGridCount: Integer;
    function GetNumPartsCircle: Integer;
    function GetNumPartsSpline: Integer;
    procedure SetGridCount(AValue: Integer);
    procedure SetNumPartsCircle(AValue: Integer);
    procedure SetNumPartsSpline(AValue: Integer);
    procedure SetCADInfoVisible(const AVal: Boolean);
    function StartPrint(const ATitle: string): Boolean;
    procedure EndPrint;
    procedure CreateXMLIDE;
    property GridCount: Integer read GetGridCount write SetGridCount;
    property NumPartsCircle: Integer read GetNumPartsCircle write SetNumPartsCircle;
    property NumPartsSpline: Integer read GetNumPartsSpline write SetNumPartsSpline;
  public
    destructor Destroy; override;
    procedure NewFile();
    procedure Open(const AFileName: String);
    function  Save(const ASource: TGraphic; const AFileName: string;
      const AFormat: TsgExportFormat; const AVersion: TsgDWGVersion): Boolean;
    function FindText(const S: String; const ASelection: TSelectionMode; const ASearch: Boolean): Boolean;
    function ReadCADEntities(Entity: TsgDXFEntity): Integer;
    procedure WndProc(var Message: TMessage); override;
    {$IFDEF SGDEL_4}
    property actForm: TAction read FactForm;
    {$ENDIF}
    property sgPaintBox: TsgDrawingNavigator read FsgPaintBox;
    property Img: TsgCADImage read GetCADImage;
    property Graphic: TGraphic read GetGraphic;
    property SearchEntities: TList read FSearchEntities;
  end;

  TCustomMemoAccess = class(TCustomMemo);

const
  cnstInputXMLMemo = 'InputXMLMemo';

var
  XMLViewerName: string = 'Viewer';

var
  Form1: TForm1;

implementation

uses
  Unit2, Unit3,
  fFonts, fPaths
  {$IFDEF DEMO}
  ,fInfo
  {$ENDIF}
{$IFDEF SGDEL_2009}
  ,PNGImage
{$ENDIF}
;

{$R *.dfm}

const
  cnstExportMsg = ' software which is not part of CAD Import VCL package.';
  cnstExportMsgDXF = 'DXF/SVG/CGM/PDF/SWF Export is made via CAD Export VCL' + cnstExportMsg;
  cnstNotCAD = 'It is not CAD file. This operation does not work.';

  cnstDefGridCount = 20;
  cnstEMFExt = '.emf';
  cnstPrnOrient: array[Boolean] of TPrinterOrientation = (poPortrait, poLandscape);

  cnstAutoCAD = 'AutoCAD';

type
  TsgDrawingNavigatorAccess = class(TsgDrawingNavigator);

{$IFDEF SG_XMLAPI}
  TsgMainForm1 = class(TsgMainFormFrame)
  private
    FForm: TForm1;
  public
    constructor Create(const AForm: TObject); override;
    function Open(const AFileName: string): Boolean; override;
    function Save(const ASource: TGraphic; const AFileName: string;
      const AFormat: TsgExportFormat; const AVersion: TsgDWGVersion): Boolean; override;
    function GetImage: TsgCADImage; override;
    function GetDrawingNavigator: TsgDrawingNavigator; override;
  end;
{$ENDIF}

function CreateLines(var AList: TList; D: Integer; AWidth, AHeight: Integer): boolean;
var
  I: Integer;
  vdx, vdy: Integer;
begin
  if AList = nil then
    AList := TList.Create
  else
    AList.Clear;
  I := 0;
  if D <= 0 then
    D := 1;
  vdx := D;
  vdy := D;
  while (I * vdx <= AWidth) do
  begin
    AList.Add(Pointer(I * vdx));
    AList.Add(Pointer(0));
    AList.Add(Pointer(I * vdx));
    AList.Add(Pointer(AHeight));
    inc(I);
  end;
  I := 0;
  while (I * vdy <= AHeight) do
  begin
    AList.Add(Pointer(0));
    AList.Add(Pointer(I * vdy));
    AList.Add(Pointer(AWidth));
    AList.Add(Pointer(I * vdy));
    inc(I);
  end;
  Result := True;
  if AList.Count < 2 then
    Result := False;
end;

procedure DrawPolyPolyLine(DC: HDC; IntPoints: TList);
var
  I, N, C: Integer;
  P: Pointer;
begin
  N := IntPoints.Count shr 2;
  if N = 0 then
    Exit;
  C := IntPoints.Count;
  for I := 0 to N - 1 do
    IntPoints.Add(Pointer(2));
  P := IntPoints.List;
  Inc(PInteger(P), C);
  PolyPolyline(DC, IntPoints.List[0], P^, N);
end;

{ TForm1 }

procedure TForm1.LoadOptions;
var
  vSet: string;
begin
  if not Assigned(FOptions) then
  begin
    vSet := ExtractFilePath(Application.ExeName) + sSettings;
    FOptions := TIniFile.Create(vSet);
  end;
  bUseSHXFonts := FOptions.ReadBool(sGeneral, sUseSHX, bUseSHXFonts);
  sSHXSearchPaths := FOptions.ReadString(sGeneral, sSHXPaths, sSHXSearchPaths);
  sDefaultSHXPath := FOptions.ReadString(sGeneral, sSHXPath, sDefaultSHXPath);
  sDefaultSHXFont := FOptions.ReadString(sGeneral, sSHXFont, sDefaultSHXFont);
end;

function TForm1.AddRectToConv(ABox: TFRect): TsgDXFEntity;

  function AddVertex(const AOwner: TsgDXFEntity;
    const APoint: TFPoint): TsgDXFVertex;
  begin
    Result := TsgDXFVertex.Create;
    Result.Point := APoint;
    AOwner.AddEntity(Result);
  end;

begin
  Result := TsgDXFPolyline.Create;
  AddVertex(Result, ABox.TopLeft);
  AddVertex(Result, MakeFPoint(ABox.BottomRight.X, ABox.TopLeft.Y, ABox.TopLeft.Z));
  AddVertex(Result, ABox.BottomRight);
  AddVertex(Result, MakeFPoint(ABox.TopLeft.X, ABox.BottomRight.Y, ABox.BottomRight.Z));
  Result.ColorCAD := MakeColorCAD(acRGBColor, clRed);
  Result.SetLWeight(1);
  TsgDXFPolyline(Result).Closed := True;
end;

function TForm1.ReadCADEntities(Entity: TsgDXFEntity): Integer;
var
  S1, S2: String;
  vRectAround: ^TRectangleAround;
  P: TFPoint;
  CurAttrib: TsgDXFAttrib;
  vBox: TFRect;
  vAttribFlag: Boolean;
begin
  Result := 0;
  if FFindFlag then
    Exit;
  if Entity is TsgDXFMText then
  begin
    FTextPoint := TsgDXFMText(Entity).Point;
  end;
  if Entity is TsgDXFText then
  begin
    S1 := AnsiUpperCase(FSearchingText);
    S2 := AnsiUpperCase(TsgDXFText(Entity).Text);
    vAttribFlag := False;
    if FCADParams.Insert <> nil then
    begin
      P := FPointXMat(TsgDXFText(Entity).StartPoint, FCADParams.Matrix);
      CurAttrib := FCADParams.Insert.Attrib(S2, P);
      if CurAttrib <> nil then
      begin
        S2 := CurAttrib.Text;
        vBox := CurAttrib.Box;
        vAttribFlag := True;
      end;
    end;
    if AnsiPos(S1, S2) > 0 then
    begin
      FSearchResult := True;
      FTextPoint := FPointXMat(TsgDXFText(Entity).StartPoint, FCADParams.Matrix);
      FTextBox.TopLeft := FPointXMat(TsgDXFText(Entity).Box.TopLeft, FCADParams.Matrix);
      FTextBox.BottomRight := FPointXMat(TsgDXFText(Entity).Box.BottomRight, FCADParams.Matrix);
      if not(FSearchModeIsAll) then
        FFindFlag := True;
      if FSelectionMode = smRectangle then
      begin
        New(vRectAround);
        if not vAttribFlag then
        begin
          vRectAround^.Box.TopLeft := FPointXMat(Entity.Box.TopLeft, FCADParams.Matrix);
          vRectAround^.Box.BottomRight := FPointXMat(Entity.Box.BottomRight, FCADParams.Matrix);
        end
        else
          vRectAround^.Box := vBox;
        vRectAround.PaperSpace := Ord(Img.Layouts[0] <> Img.CurrentLayout);
        FSearchEntities.Add(vRectAround);
      end;
    end;
  end;
end;

procedure TForm1.SelectSearchEntities;
var
  I: Integer;
  Ent: TsgDXFEntity;
begin
  for I := 0 to FSearchEntities.Count - 1 do
  begin
    Ent := AddRectToConv(TRectangleAround(FSearchEntities[I]^).Box);
    if Ent <> nil then
    begin
      Img.Converter.Sections[csEntities].AddEntity(Ent);
      Img.Converter.DoCreate(Ent);
      Img.Converter.Loads(Ent);
      Ent.PaperSpace := TRectangleAround(FSearchEntities[I]^).PaperSpace;
    end;
  end;
  sgPaintBox.Invalidate;
end;

function TForm1.FindText(const S: String; const ASelection: TSelectionMode; const ASearch: Boolean): Boolean;
var
  I: Integer;
begin
  FSearchingText := S;
  FSelectionMode := ASelection;
  FSearchModeIsAll := ASearch;
  FSearchResult := False;
  FCADParams.Matrix := cnstIdentityMat;
  Img.Converter.AutoInsert := True; // to get all the elements inside of inserts
  FFindFlag := False;
  FSearchEntities := TList.Create;
  try
    Img.Converter.Iterate(ReadCADEntities, nil, FCADParams);
    SelectSearchEntities;
  finally
    for I := 0 to FSearchEntities.Count - 1 do
      Dispose(FSearchEntities[I]);
    FSearchEntities.Free;
  end;
  Result := FSearchResult;
end;

procedure TForm1.Time;
var
  T: Int64;
begin
  T := ({$IFDEF SGFPC}GetTickCount64{$ELSE}GetCurrentTime{$ENDIF} - FStart) div 1000;
  pnlTime.Caption := Format('%.2d:%.2d', [T div 60, T mod 60]);
  pnlTime.Update;
end;

procedure TForm1.LoadStructure;

  procedure Add(Parent: TTreeNode; E: TsgDXFEntity);
  var
    I: Integer;
  begin
    Parent := TreeView1.Items.AddChildObject(Parent, E.EntName, E);
    Application.ProcessMessages;
    prbProgress.Position := prbProgress.Position + 1;
    for I := 0 to E.Count - 1 do Add(Parent, E[I]);
  end;

var
  I: Integer;
begin
  if Img = nil then Exit;
  prbProgress.Position := 0;
  prbProgress.Max := Img.Converter.Count;
  pnlFileName.Caption := 'Structure';
  pnlFileName.Update;
  TreeView1.Items.BeginUpdate;
  try
    for I := 0 to Img.Converter.Main.Count - 1 do
      Add(nil, Img.Converter.Main[I]);
  finally
    prbProgress.Position := 0;
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TForm1.EnableControls;
var
  I: Integer;
  vNotEmpty: Boolean;
  vCAD: Boolean;
begin
  vNotEmpty := not sgPaintBox.Empty;
  vCAD := Assigned(Img);
  for I := 0 to MIScale.Count-1 do
    MIScale.Items[I].Enabled := vNotEmpty;
  MIPrint.Enabled := vNotEmpty;
  BtnSave.Enabled := vNotEmpty;
  BtnPrint.Enabled := vNotEmpty;
  BtnPrint1.Enabled := vNotEmpty;
  BtnPrint2.Enabled := vNotEmpty;
  BtnPlus.Enabled := vNotEmpty;
  BtnMinus.Enabled := vNotEmpty;
  BtnReal.Enabled := vNotEmpty and (Abs(1 - sgPaintBox.Scale) > fAccuracy);
  BtnPrev.Enabled := vNotEmpty and sgPaintBox.HasPreviousView;
  btnLayers.Enabled := vCAD;
  btnFitToScreen.Enabled := vNotEmpty;
  mmiSaveAsBigEMF.Enabled := vNotEmpty;
  mmiSaveAs.Enabled := vNotEmpty;
  MIPrint.Enabled := vNotEmpty;
  MIClose.Enabled := vNotEmpty;
  btnIsWithoutBorder.Enabled := vCAD;
  btnDrawAllColor.Enabled := vCAD;
  btnBlackWhite.Enabled := vCAD;
  btnShowLineWeight.Enabled := vCAD;
  btnSplittedArc.Enabled := vCAD;
  sbDrawingBox.Enabled := vCAD;
  sb3DOrbit.Enabled := vCAD and Img.CurrentLayout.IsModel;
  sb3DAxes.Enabled := vCAD;
  sbDrid.Enabled := vCAD;
  sbShowBorder.Enabled := vCAD;
  mmiShowpoint.Enabled := vCAD;
  mmiFindtext.Enabled := vCAD;
  mmiReopenSameView.Enabled := vCAD;
  mmiReopenSWIsometric.Enabled := vCAD;
  mmiBlack.Enabled := vCAD;
  mmiNormal.Enabled := vCAD;
  mmiObjectSnapAll.Enabled := vCAD;
{$IFDEF DEMO}
  if vCAD then
    DestroyFrameInfo;
{$ENDIF}
end;

procedure TForm1.EndPrint;
begin
  RestoreNullWidth(Graphic);
  sgPaintBox.Refresh;
end;

procedure TForm1.Open(const AFileName: String);
var
  vBreakForm: TForm;

  function MakeBreakForm: TForm;
  const
    sBreakFormName = 'fmBreakForm';
  var
    vB: TButton;
  begin
    Result := TForm(Application.FindComponent(sBreakFormName));
    if not Assigned(Result) then
    begin
      Result := TForm.Create(Application);
      Result.Name := sBreakFormName;
      Result.BorderStyle := bsDialog;
      Result.FormStyle := fsStayOnTop;
      Result.Caption := 'Loading...';
      vB := TButton.Create(Result);
      vB.Parent := Result;
      vB.SetBounds(8, 8, 125, 23);
      vB.Caption := 'Break loading';
      vB.OnClick := BreakLoading;
    end;
    Result.Show;
    Result.SetBounds(Left + Width div 2 - 151 div 2,
        Top + Height div 2 - 76 div 2, 151, 76);
  end;

  procedure NewCadImage;
  var
    vCAD: TsgCADImage;
  begin
    FFileName := 'New';
    vCAD := TsgCADImage.Create;
    try
      vCAD.Converter.InitializeSectionsBegin;
      vCAD.Converter.InitializeSectionsEnd(True);
      if vCAD.CurrentLayout = nil then
        vCAD.CurrentLayout := vCAD.Layouts[0];
      sgPaintBox.Picture.Graphic := vCAD;
    finally
      vCAD.Free;
    end;
  end;
begin
  sgPaintBox.BeginUpdate;
  try
    SetCADInfoVisible(False);
    {$IFDEF SG_XMLAPI}
    if Assigned(FXmlMainApp) then
      FXmlMainApp.Clear;
    {$ENDIF}
    TreeView1.Items.Clear;
    Screen.Cursor := crAppStart;
    vBreakForm := MakeBreakForm;
    try
      sb3DAxes.Down := False;
      MICloseClick(nil);
      FStart := {$IFDEF SGFPC}GetTickCount64{$ELSE}GetCurrentTime{$ENDIF};
      if Length(AFileName) > 0 then
      begin
        sgPaintBox.LoadFromFile(AFileName);
        FFileName := AFileName;
      end
      else
         NewCadImage;
      pnlFileName.Caption := UpperCase(ExtractFileName(FFileName));
      pnlLayouts.Visible := False;
      if Assigned(Img) then
      begin
        SetImgOpt;
        tbsStructure.TabVisible := True;
        ViewLayouts;
        NumPartsCircle := sgConsts.GetNumberOfCircleParts;
        NumPartsSpline := sgConsts.GetNumberOfSplineParts;
      end;
      if not Assigned(FImg) or not FImg.CurrentLayout.IsModel or
         not TsgDrawingNavigatorAccess(sgPaintBox).OpenVPort then
        sgPaintBox.FitToSize;
    finally
      SetCADInfoVisible(Assigned(Img));
      if Assigned(vBreakForm) then
        vBreakForm.Close;
      EnableControls;
      prbProgress.Position := 0;
      Screen.Cursor := crDefault;
    end;
  finally
    UpdateXMLIDE;
    sgPaintBox.EndUpdate;
  end;
end;

procedure TForm1.MIOpenClick(Sender: TObject);
begin
  OpenDialog1.FileName := '';
  CADPreview := True;
  try
    if OpenDialog1.Execute then
    begin
      CADPreview := False;
      MICloseClick(nil);
      Open(OpenDialog1.FileName);
    end;
  finally
    CADPreview := False;
  end;
end;

procedure TForm1.MICloseClick(Sender: TObject);
begin
  ListBox1.Clear;
  TreeView1.Items.Clear;
  sgPaintBox.Picture.Graphic := nil;
  UpdateXMLIDE;
  FFileName := '';
  pnlFileName.Caption := '';
  pnlCoords.Caption := '';
  pnlTime.Caption := '';
  MIClose.Enabled := False;
  MIPrint.Enabled := False;
  BtnSave.Enabled := False;
  pgcDrawing.ActivePage := tbsImage;
  tbsStructure.TabVisible := False;
  prbProgress.Max := 100;
  EnableControls;
  cbLayouts.Items.Clear;
  pnlLayouts.Visible := False;
end;

procedure TForm1.MIExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  E: TsgDXFEntity;
  S: String;
begin
  if Node.Selected then
  begin
    E := TsgDXFEntity(Node.Data);
    if (E = nil) or (E.SrcStart = nil) or (E.SrcEnd = nil) then
      ListBox1.Clear
    else
    begin
      SetString(S, E.SrcStart, E.SrcEnd - E.SrcStart);
      ListBox1.Items.Text := S;
    end;
  end;
end;

procedure TForm1.SetNullWidth(AGraphic: TGraphic; AWidth: Integer);
begin
  if AGraphic is TsgCADImage then
  begin
    FStoredNullWidth := TsgCADImage(AGraphic).NullWidth;
    TsgCADImage(AGraphic).NullWidth := AWidth;
  end;
end;

procedure TForm1.SetNumPartsCircle(AValue: Integer);
begin
  udNumPartsCircle.Position := AValue;
  edtNumPartsCircle.Text := IntToStr(AValue);
end;

procedure TForm1.SetNumPartsSpline(AValue: Integer);
begin
  udNumPartsSpline.Position := AValue;
  edtNumPartsSpline.Text := IntToStr(AValue);
end;

procedure TForm1.RestoreNullWidth(AGraphic: TGraphic);
begin
  if AGraphic is TsgCADImage then
    TsgCADImage(AGraphic).NullWidth := FStoredNullWidth;
end;

function TForm1.ExportCad(const ASource: TsgCADImage; const AFormat: TsgExportFormat;
  const AFileName: string; const AVersion: TsgDWGVersion): Boolean;
var
  vExport: TsgCADExport;
  vExportParams: TsgExportParams;
begin
  {$IFDEF CADExportVCL}
  Result := False;
  vExport := nil;
  if Assigned(ASource) then
  begin
    case AFormat of
      efDxf:
        begin
          vExport := TsgCADtoDXF.Create(ASource); //direct export
          TsgCADtoDXF(vExport).Version := AVersion;
        end;
      efSvg:  vExport := TsgCADtoSVG.Create(ASource);
      efCgm:  vExport := TsgCADtoCGM.Create(ASource);
      efPdf:
        begin
          vExportParams := DefExportParams;
          {$IFDEF SGDEL_2009}
          if PDFExportDlgExecute(Self, nil, @vExportParams) then
          {$ENDIF}
          begin
            vExport := TsgPDFExport.Create(ASource);
            TsgSimpleExport(vExport).ApplyParams(vExportParams);
          end;
        end;
      efSwf:  vExport := TsgCADtoSWF.Create(ASource);
      efPlt:
        begin
          vExport := TsgCADtoHPGL.Create(ASource);
          TsgCADtoHPGL(vExport).XScale := 40;
        end;
      efDwg:
        begin
          vExport := TsgCADtoDWG.Create(ASource);
          TsgCADtoDWG(vExport).Version := AVersion;
        end;
    end;
    if Assigned(vExport) then
    begin
      try
        vExport.SaveToFile(AFileName);
        Result := True;
        //MessageBox(Application.Handle, PChar(cnstExportMsgDXF), 'WARNING', MB_ICONWARNING);
      finally
        vExport.Free;
      end;
    end;
  end
  else
    ShowMessage(sNotVectorial)
     {$ENDIF}
end;

function TForm1.ExportRastr(const ASource: TsgCADImage; const AFormat: TsgExportFormat;
  const AFileName: string): Boolean;
var
  vExport: TObject;
  vWidth, vHeight: Integer;
  vRatio: Double;
  vBmp: TBitmap;
  vStream: TStream;
  vStreamPersist: IStreamPersist;
begin
  Result := False;
  vExport := nil;
  if Assigned(ASource) then
    case AFormat of
      efBitmap:       vExport := TBitmap.Create;
      efJpg..efJpeg:  vExport := TJPEGImage.Create;
  {$IFDEF SGDEL_2007}
      efGif:          vExport := TGIFImage.Create;
  {$ENDIF}
  {$IFDEF SGDEL_2009}
      efPng:          vExport := TPngImage.Create;
  {$ENDIF}
  {$IFNDEF SGFPC}
      efWmf..efEmf:   vExport := TMetafile.Create;
  {$ENDIF}
    end;
  if Assigned(vExport) then
  begin
    try
    // Converting CAD-drawing width/height to Screen size for export
      vWidth := ASource.Width;
      vHeight := ASource.Height;
      if vHeight <> 0 then
        vRatio := vWidth / vHeight
      else
        vRatio := 1;
      if vRatio <= 1 then
      begin
        vHeight := Screen.Height;
        vWidth := Round(vHeight * vRatio);
      end
      else
      begin
        vWidth := Screen.Width;
        vHeight := Round(vWidth / vRatio);
      end;
      if AFormat in [efBitmap, efJpg .. efJpeg, efPng, efGif] then
      begin
        if AFormat = efBitmap then
        begin
          SetSizeGraphic(TGraphic(vExport), vWidth, vHeight);
          DrawTo(vExport, Rect(0, 0, vWidth, vHeight), ASource);
        end
        else
        begin
          vBmp := TBitmap.Create;
          try
            SetSizeGraphic(vBmp, vWidth, vHeight);
            DrawTo(vBmp, Rect(0, 0, vWidth, vHeight), ASource);
            TGraphic(vExport).Assign(vBmp);
          finally
            vBmp.Free;
          end;
        end;
      end
      else
      begin
        {$IFNDEF SGFPC}
        if AFormat in [efWmf, efEmf] then
        begin
          SetSizeGraphic(TGraphic(vExport), vWidth, vHeight);
          DrawTo(vExport, Rect(0, 0, vWidth, vHeight), ASource);
          TMetafile(vExport).Enhanced := AFormat = efEmf;
        end;
        {$ENDIF}
      end;
      if vExport.GetInterface(IStreamPersist, vStreamPersist) then
      begin
        vStream := TFileStream.Create(AFileName, fmCreate);
        try
          vStreamPersist.SaveToStream(vStream);
          Result := True;
        finally
          vStream.Free;
        end;
      end;
    finally
      vExport.Free;
    end;
  end
  else
    ShowMessage('Unknown format');
end;

function TForm1.Save(const ASource: TGraphic; const AFileName: string;
  const AFormat: TsgExportFormat; const AVersion: TsgDWGVersion): Boolean;
var
  vFormatByFileName: TsgExportFormat;
  vFileName, vFileExt: string;
  vImg: TsgCADImage;
  vCur: TCursor;
begin
  Result := False;
  vFileName := AFileName;
  vFileExt := AnsiLowerCase(ExtractFileExt(vFileName));
  vFormatByFileName := GetExportFormat(vFileExt);
  if AFormat <> vFormatByFileName then
    vFileName := vFileName + GetExportExt(AFormat);
  vImg := nil;
  if ASource is TsgCADImage then
    vImg := TsgCADImage(ASource);
  vCur := Cursor;
  Cursor := crHourGlass;
  try
    case AFormat of
      efDxf, efSvg, efCgm, efPdf, efSwf, efPlt, efDwg:
        //ExportCad(vImg, AFormat, vFileName, AVersion);
        SaveToSVG(vImg, vFileName);    // 2024.11.28 최승선 수정
      efBitmap, efJpg..efJpeg, efPng, efWmf..efEmf, efGif:
        ExportRastr(vImg, AFormat, vFileName);
      else
        ShowMessage('Unknown format');
    end;
  finally
    Cursor := vCur;
  end;
end;

// Printing funtcions

procedure TForm1.MIPrintClick(Sender: TObject);
var
  W, H: Double;
  PW, PH: Integer;
  R: TRect;
begin
  if StartPrint('Direct draw CAD to printer') then
  try
    W := Printer.PageWidth / Graphic.Width;
    H := Printer.PageHeight / Graphic.Height;
    if W > H then W := H;
    PW := Round(W * Graphic.Width);
    PH := Round(W * Graphic.Height);
    R := Bounds((Printer.PageWidth - PW) div 2, (Printer.PageHeight - PH) div 2, PW, PH);
    DrawTo(Printer, R, Graphic);
  finally
    EndPrint;
  end;
end;

procedure TForm1.BtnPrint1Click(Sender: TObject);
const
  cnstScaleFactor = 0.5;
var
  vPxlsX, vPxlsY: Double;
  R: TRect;
begin
  if StartPrint('CAD Import VCL printing 1:1') then
  try
    vPxlsX := 1;
    vPxlsY := 1;
    if Assigned(Img) then
    begin
      vPxlsX := GetDeviceCaps(Printer{$IFDEF SGFPC}.Canvas{$ENDIF}.Handle, LOGPIXELSX);
      vPxlsY := GetDeviceCaps(Printer{$IFDEF SGFPC}.Canvas{$ENDIF}.Handle, LOGPIXELSY);
      if Img.Millimetres then
      begin
        vPxlsX := vPxlsX / cnstMMPerInch;
        vPxlsY := vPxlsY / cnstMMPerInch;
      end;
    end;
    R := Rect(0, 0, Round(vPxlsX * Graphic.Width * cnstScaleFactor),
      Round(vPxlsY * Graphic.Height * cnstScaleFactor));
    DrawTo(Printer, R, Graphic);
  finally
    EndPrint;
  end;
end;

procedure TForm1.Printer1Click(Sender: TObject);
var
  W, H: Double;
  PW, PH: Integer;
  vGr: TGraphic;
  R: TRect;
begin
  if StartPrint('Draw actual CAD to printer throw metafile') then
  try
    {$IFNDEF SGFPC}
    vGr := TMetafile.Create;
    try
      ActualGraphic(vGr);
      W := Printer.PageWidth / vGr.Width;
      H := Printer.PageHeight / vGr.Height;
      if W > H then W := H;
      PW := Round(W * vGr.Width);
      PH := Round(W * vGr.Height);
      R := Bounds((Printer.PageWidth - PW) div 2, (Printer.PageHeight - PH) div 2, PW, PH);
      DrawTo(Printer, R, vGr);
    finally
      vGr.Free;
    end;
    {$ENDIF}
  finally
    EndPrint;
  end;
end;

function CreatePanel(const AOwner: TComponent; const AParent: TWinControl;
  const ACaption: string = ''): TPanel;
begin
  Result := TPanel.Create(AOwner);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.BevelOuter := bvNone;
  Result.BorderStyle := bsNone;
  Result.BorderWidth := 8;
end;

function CreateGroupBox(const AOwner: TComponent; const AParent: TWinControl;
  const ACaption: string): TGroupBox;
begin
  Result := TGroupBox.Create(AOwner);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  {$IFNDEF FPC}
  {$IFDEF SG_CONTROL_HASPROPERTY_MARGINS}
  Result.Margins.SetBounds(4, 4, 4, 4);
  Result.AlignWithMargins := True;
  {$ENDIF}
  {$ENDIF}
  Result.Width := Result.Width div 2;
end;

function TForm1.CreateViewer(const AName: string): TsgXmlIdeForm;
var
  R: TRect;
  vView: TWinControl;
  vOutputPanel: TGroupBox;
  vViewPanel: TPanel;
begin
  Result := TsgXmlIdeForm(TForm.CreateNew(Self));
//  TForm(Result).FormStyle := fsStayOnTop;
  TForm(Result).Position := poOwnerFormCenter;
  Result.Caption := AName;
  R := BoundsRect;
  InflateRect(R, -((R.Right - R.Left) div 6), -((R.Bottom - R.Top) div 6));
  Result.BoundsRect := R;

  vOutputPanel := CreateGroupBox(Result, Result, 'Output XML');
  vOutputPanel.Align := alClient;

  vViewPanel := CreatePanel(Result, vOutputPanel);
  vViewPanel.Align := alClient;

  Result.OutputXml := TEditMemo.Create(FXMLForm);

{$IFDEF FPC}
{$ELSE}
  TMemo(Result.OutputXml).ReadOnly := True;
{$ENDIF}
  vView := Result.OutputXml;
  vView.Name := XMLViewerName;
  vView.Parent := vViewPanel;
  vView.Align := alClient;
  vView.SetTextBuf(nil);
{$IFDEF FPC}
  if vView is TSynEdit then
    TSynEdit(vView).Highlighter := TSynXMLSyn.Create(Result);
{$ELSE}
  if vView is TCustomMemo then
    TCustomMemoAccess(vView).ScrollBars := ssBoth;
{$ENDIF}
end;

procedure TForm1.CreateXMLIDE;
begin
{$IFDEF SG_XMLAPI}
  FXmlMainApp := TsgXmlMainApplication.Create(TsgMainForm1.Create(Self));
  FXMLIDE := TsgXMLIDE.Create;
  FXMLIDE.MainApplication := Self.FXmlMainApp;
  FXMLIDE.CADEditor := FXmlMainApp.ViewerEditor;
  FXMLIDE.OnXMLMessage := DoXMLMessage;
  FXMLIDE.HWNDMainForm := Self.Handle;
  FOnPaintExternal := FXmlMainApp.PaintExternal;
{$ELSE}
  XMLAPI1.Visible := False;
{$ENDIF}
end;

procedure TForm1.UpdateXMLIDE;
begin
{$IFDEF SG_XMLAPI}
  if not (Assigned(FXMLIDE) and Assigned(sgPaintBox)) then Exit;
  FXmlMainApp.Clear;
  FXMLIDE.CADImage := TsgCADImage(sgPaintBox.Picture.Graphic);
{$ENDIF}
end;

const
  fmtCmdXML = '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak;
  fmtCst = '<cadsofttools version="2">'+sLineBreak+'  <%s/>'+sLineBreak+'</cadsofttools>';

procedure TForm1.DoXMLMessage(const S: WideString);
begin
  if Assigned(FXMLForm) then
    FXMLForm.OutputXml.Text := S;
end;

procedure TForm1.ExecuteProcessXML(Sender: TObject);
var
  vStr: string;
begin
  if Assigned(FXMLForm) then
  begin
    vStr := DoProcessXML(FXMLForm.InputXml.Text);
    FXMLForm.OutputXml.Text := vStr;
  end;
end;

function TForm1.DoProcessXML(const AXml: string): string;
begin
  Result := '';
{$IFDEF SG_XMLAPI}
  if Assigned(FXMLIDE) then
  begin
    if not Assigned(FXMLIDE.CADImage)  then
      NewFile;
    Result := FXMLIDE.ProcessXML(AXml);
  end;
{$ENDIF}
end;

procedure TForm1.ProccessXML1Click(Sender: TObject);
{$IFDEF SG_XMLAPI}
var
  vSplitter: TSplitter;
  vInputPanel: TGroupBox;
  vInputButtonsPanel: TPanel;
  vInputXMLMemo: TWinControl;
  vExecuteButton: TButton;
  vLoadButton: TButton;
begin
  if Assigned(FXMLForm) then
  begin
    if FXMLForm.Showing then
      FXMLForm.SetFocus
    else
      FXMLForm.Show;
    Exit;
  end;
  FXMLForm := CreateViewer('Process XML');

  vInputPanel := CreateGroupBox(FXMLForm, FXMLForm, 'Input XML');
  vInputPanel.Left := 1;
  vInputPanel.Align := alLeft;
  vInputPanel.Width := FXMLForm.Width div 2;

  vSplitter := TSplitter.Create(FXMLForm);
  vSplitter.Parent := FXMLForm;
  vSplitter.Width := 3;
  vSplitter.Left := 2;
  vSplitter.Align := alLeft;

  vInputButtonsPanel := CreatePanel(FXMLForm, vInputPanel);
  vInputButtonsPanel.Height := 24 + 8 * 2;
  vInputButtonsPanel.Align := alBottom;

  vExecuteButton := TButton.Create(FXMLForm);
  vExecuteButton.Parent := vInputButtonsPanel;
  vExecuteButton.Height := 24;
  vExecuteButton.Width := 80;
  vExecuteButton.Align := alRight;
  vExecuteButton.Caption := 'Run';
  vExecuteButton.Name := 'btnExecute';
  vExecuteButton.OnClick := ExecuteProcessXML;

  vLoadButton := TButton.Create(FXMLForm);
  vLoadButton.Parent := vExecuteButton.Parent;
  vLoadButton.Height := vExecuteButton.Height;
  vLoadButton.Width := vExecuteButton.Width;
  vLoadButton.Align := alLeft;
  vLoadButton.Caption := 'Load XML...';
  vLoadButton.Name := 'btnLoad';
  vLoadButton.OnClick := LoadXML;
  TsgXmlIdeForm(FXMLForm).InputXml := TEditMemo.Create(FXMLForm);
  vInputXMLMemo := TsgXmlIdeForm(FXMLForm).InputXml;
  vInputXMLMemo.Parent := vInputPanel;
  {$IFNDEF FPC}
  {$IFDEF SG_CONTROL_HASPROPERTY_MARGINS}
  vInputXMLMemo.AlignWithMargins := True;
  vInputXMLMemo.Margins.SetBounds(cnstBD, cnstBD, cnstBD, cnstBD);
  {$ENDIF}
  {$ENDIF}
  vInputXMLMemo.Align := alClient;
  vInputXMLMemo.Name := cnstInputXMLMemo;
  vInputXMLMemo.SetTextBuf(PChar(fmtCmdXML + Format(fmtCst, ['get'])));
{$IFDEF FPC}
  if vInputXMLMemo is TSynEdit then
    TSynEdit(vInputXMLMemo).Highlighter := TSynXMLSyn.Create(FXMLForm);
{$ELSE}
  TMemo(vInputXMLMemo).ScrollBars := ssBoth;
{$ENDIF}
//  TForm(FXMLForm).OnClose := CloseProcessXML;

  FXMLForm.Show;
{$ELSE}
begin
{$ENDIF}
end;

procedure TForm1.LoadXML(Sendr: TObject);
{$IFDEF SG_XMLAPI}
var
  vOD: TOpenDialog;
  vFileName: string;
begin
  vOD := TOpenDialog.Create(nil);
  try
    vOD.Filter := 'XML|*.xml';
    vOD.DefaultExt := '.xml';
    if vOD.Execute then
    begin
      vFileName := vOD.FileName;
      if Length(ExtractFileExt(vFileName)) < 0 then
        vFileName := vFileName + vOD.DefaultExt;
      if FileExists(vFileName) then
        FXMLForm.InputXml.Lines.LoadFromFile(vFileName)
      else
        ShowMessage('File not found!');
    end;
  finally
    vOD.Free;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TForm1.Metafile1Click(Sender: TObject);
var
  vGr: TGraphic;
begin
  SavePictureDialog.FilterIndex := GetFilterIndexFromList(FSaveDialogFilter, cnstEMFExt);
  if sgPaintBox.Empty or not SavePictureDialog.Execute then Exit;
  {$IFNDEF SGFPC}
  vGr := TMetafile.Create;
  try
    ActualGraphic(vGr);
    Save(vGr, SavePictureDialog.FileName, efEmf, acR2000);
  finally
    vGr.Free;
  end;
  {$ENDIF}
end;

procedure TForm1.BreakLoading(Sender: TObject);
begin
  if CADImage.Loading <> nil then CADImage.Loading.StopLoading;
end;

procedure TForm1.ActualGraphic(var AResult: TGraphic);
{$IFNDEF SGFPC}
const
  cnstMMCf: array[Boolean] of Integer = (500, 1000);
  cnstExpandCf: array[Boolean] of Integer = (5, 10);
var
  vMC: TMetafileCanvas;
  vRect: TRect;
  vRGN: HRGN;
  DC: HDC;
  mmToPixelX, mmToPixelY: Double;
  vClientR: TRect;
  vW, vH: Integer;
  vIsNT: Boolean;
begin
  vClientR := sgPaintBox.ClientRect;
  vW := vClientR.Right - vClientR.Left;
  vH := vClientR.Bottom - vClientR.Top;
  DC := GetDC(0);
  try
    mmToPixelX := GetDeviceCaps(DC,HORZSIZE) /
      GetDeviceCaps(DC,HORZRES);
    mmToPixelY := GetDeviceCaps(DC,VERTSIZE) /
      GetDeviceCaps(DC,VERTRES);
  finally
    ReleaseDC(0, DC);
  end;
  vRect := sgPaintBox.PictureRect;
  vRect.Left := vRect.Left - vClientR.Left;
  vRect.Top := vRect.Top - vClientR.Top;
  vRect.Right := vRect.Right - vClientR.Left;
  vRect.Bottom := vRect.Bottom - vClientR.Top;
  vIsNT := Win32Platform = VER_PLATFORM_WIN32_NT;
  TMetafile(AResult).MMWidth := Round(vW * cnstMMCf[vIsNT] * mmToPixelX);
  TMetafile(AResult).MMHeight := Round(vH * cnstMMCf[vIsNT] * mmToPixelY);
  vRect.Left := vRect.Left * cnstExpandCf[vIsNT];
  vRect.Top := vRect.Top * cnstExpandCf[vIsNT];
  vRect.Right := vRect.Right * cnstExpandCf[vIsNT];
  vRect.Bottom := vRect.Bottom * cnstExpandCf[vIsNT];
  vMC := TMetafileCanvas.Create(TMetafile(AResult), 0);
  try
    vRGN := CreateRectRgn(0, 0, vW * cnstExpandCf[vIsNT], vH * cnstExpandCf[vIsNT]);
    SelectClipRgn(vMC.Handle, vRGN);
    vMC.StretchDraw(vRect, Graphic);
    DeleteObject(vRGN);
  finally
    vMC.Free;
    if Img <> nil then
      Img.IsShowBackground := True;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TForm1.MIAutoClick(Sender: TObject);
begin
  sgPaintBoxScaling(nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  sSHXSearchPaths := ExpandFileName('../../../SHX');
  FSaveDialogFilter := TsgStringList.Create;
{$IFDEF SGDEL_2007}
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + GraphicFilter(TGIFImage);
{$ENDIF}
{$IFDEF SGDEL_2009}
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + GraphicFilter(TPngImage);
{$ENDIF}
{$IFDEF CADExportVCL}
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|SVG (*.svg)|*.svg';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|CGM (*.cgm)|*.cgm';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|PDF (*.pdf)|*.pdf';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|HPGL (*.plt)|*.plt';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|SWF (*.swf)|*.swf';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DXF 2000 (*.dxf)|*.dxf';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DXF 2004 (*.dxf)|*.dxf';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DXF 2007 (*.dxf)|*.dxf';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DXF 2010 (*.dxf)|*.dxf';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DXF 2013 (*.dxf)|*.dxf';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DXF 2018 (*.dxf)|*.dxf';

  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DWG 2000 (*.dwg)|*.dwg';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DWG 2004 (*.dwg)|*.dwg';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DWG 2010 (*.dwg)|*.dwg';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DWG 2013 (*.dwg)|*.dwg';
  SavePictureDialog.Filter := SavePictureDialog.Filter + '|' + cnstAutoCAD + ' DWG 2018 (*.dwg)|*.dwg';
{$ENDIF}
  SetCADInfoVisible(False);
  FsgPaintBox := TsgDrawingNavigator.Create(Self);
  FsgPaintBox.SysMenuIconsVisible := False;
  FsgPaintBox.Parent := tbsImage;
  FsgPaintBox.AutoFocus := True;
  FsgPaintBox.RectZooming := True;
  FsgPaintBox.OnChangeScale := sgPaintBoxScaling;
  FsgPaintBox.OnPaint := sgPaintBoxPaint;
  FsgPaintBox.OnMouseMove := sgPaintBoxMouseMove;
  FsgPaintBox.OnProgress := sgPaintBoxProgress;
  FsgPaintBox.Align := alClient;
  FsgPaintBox.DoubleBuffered := True;
  FsgPaintBox.Color := clWhite;
  FsgPaintBox.OnPictureChange := sgPaintBoxPictureChange;
  {$IFDEF SGFPC}
  pmiDoublebuffered.AutoCheck := True;
  {$ENDIF}
  pmiDoublebuffered.Enabled := FsgPaintBox.DoubleBuffered;
  {$IFDEF DEMO}
  FFrameInfoHandle := fInfo.CreateInfoFrame(FsgPaintBox.Handle, Point(1, 1));
  {$ENDIF}
  {$IFDEF SGDEL_4}
  FactForm := TAction.Create(Self);
  FactForm.OnUpdate := actFormUpdate;
  FactForm.Caption := 'CAD Import VCL and CAD Export VCL demo viewer';
  Self.Action := actForm;
  {$ENDIF}
  FactForm.DisableIfNoHandler := False;
  Application.OnIdle := AppIdle;
  LoadOptions;
  OpenDialog1.Filter := GraphicFilter(TGraphic);
  pnlLayouts.Visible := False;
  if ParamCount > 0 then
  begin
    Open(ParamStr(1));
    OpenDialog1.FileName := ParamStr(1);
  end;
  sbDrawingBox.Enabled := False;
  NumPartsCircle := sgConsts.GetNumberOfCircleParts;
  NumPartsSpline := sgConsts.GetNumberOfSplineParts;
  GridCount := cnstDefGridCount;
  ParseDialogFilter(SavePictureDialog.Filter, FSaveDialogFilter);
  CreateXMLIDE;
  SvgConvert();
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
  if (pgcDrawing.ActivePage <> tbsImage) or (Shift-[ssCtrl] <> []) then Exit;
  case Key of
    VK_ADD: BtnPlusClick(nil);
    VK_SUBTRACT: BtnMinusClick(nil);
    VK_MULTIPLY:
      if ssCtrl in Shift then
        BtnReal.Click
      else
        btnFitToScreen.Click;
  end;
end;

procedure TForm1.MIPrevClick(Sender: TObject);
begin
  sgPaintBox.CallPreviousView;
end;

procedure TForm1.sgPaintBoxScaling(Sender: TObject);
begin
  MIPrev.Enabled := sgPaintBox.HasPreviousView;
  BtnPrev.Enabled := MIPrev.Enabled;
end;

procedure TForm1.sgPaintBoxProgress
    {$IFDEF SGFPC}
    (Sender: TObject; Stage: TFPImgProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: AnsiString; var Continue : Boolean);
    {$ELSE}
    (Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: String);
    {$ENDIF}
begin
  if Stage = psStarting then
  begin
    pnlFileName.Caption := Msg;
    pnlFileName.Update;
  end;
  prbProgress.Position := PercentDone;
  Time;
  if Stage = psEnding then
  begin
    prbProgress.Position := 0;
    pnlFileName.Caption := UpperCase(ExtractFileName(OpenDialog1.FileName))
  end;
end;

procedure TForm1.BtnPlusClick(Sender: TObject);
begin
  sgPaintBox.AlterScale(2, False, sgPaintBox.Center);
end;

procedure TForm1.BtnMinusClick(Sender: TObject);
begin
  sgPaintBox.AlterScale(0.5, False, sgPaintBox.Center);
end;

procedure TForm1.pgcDrawingChanging(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange := Assigned(Img);
  FocusControl(nil);
end;

procedure TForm1.pmiDoublebufferedClick(Sender: TObject);
begin
  sgPaintBox.DoubleBuffered := pmiDoublebuffered.Checked;
end;

procedure TForm1.MIHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTENTS,0);
end;

procedure TForm1.OpenDialog1Show(Sender: TObject);
begin
  MinDXFSize := 140;
end;

procedure TForm1.OpenDialog1Close(Sender: TObject);
begin
  MinDXFSize := 0;
end;

procedure TForm1.DrawTo(const ADest: TObject; const ADstRect: TRect;
  const ASource: TGraphic);
var
  vCanvas: TCanvas;
begin
  vCanvas := nil;
  if ADest is TCanvas then
    vCanvas := TCanvas(ADest)
  else
    if ADest is TBitmap then
      vCanvas := TBitmap(ADest).Canvas
    else
      {$IFNDEF SGFPC}
      if ADest is TMetafile then
        vCanvas := TMetafileCanvas.Create(TMetafile(ADest), 0)
      else
      {$ENDIF}
        if ADest is TPrinter then
        begin
          vCanvas := TPrinter(ADest).Canvas;
          TPrinter(ADest).BeginDoc;
        end;
  try
    if vCanvas <> nil then
      vCanvas.StretchDraw(ADstRect, ASource);
  finally
    {$IFNDEF SGFPC}
    if ADest is TMetafile then
      vCanvas.Free
    else
    {$ENDIF}
      if ADest is TPrinter then
        TPrinter(ADest).EndDoc;
  end;
end;

procedure TForm1.BtnSaveClick(Sender: TObject);
var
  I, Err, vVer: Integer;
  S, vExt: string;
  vFormat: TsgExportFormat;
  vVersion: TsgDWGVersion;
begin
  if not sgPaintBox.Empty then
  begin
    SavePictureDialog.FileName := ExtractFileName(OpenDialog1.FileName);
    if SavePictureDialog.Execute then
    begin
      I := SavePictureDialog.FilterIndex - 1;
      // first item is default extension
      vExt := AnsiLowerCase(TStrings(FSaveDialogFilter.Objects[I])[0]);
      vFormat := GetExportFormat(vExt);
      vVersion := acR2000;
      if vFormat in [efDxf, efDwg] then
      begin
        S := Copy(FSaveDialogFilter[I], 5, 4);
        Val(S, vVer, Err);
        if Err = 0 then
          case vVer of
            2000: vVersion := acR2000;
            2004: vVersion := acR2004;
            2007: vVersion := acR2007;
            2010: vVersion := acR2010;
          end;
      end;
      Save(Graphic, SavePictureDialog.FileName, vFormat, vVersion);
    end;
  end;
end;

procedure TForm1.mmiNormalClick(Sender: TObject);
begin
  if mmiNormal.Checked then
  begin
    Img.DrawMode := dmNormal;
    btnDrawAllColor.Down := True;
  end
  else
  begin
    Img.DrawMode := dmBlack;
    btnBlackWhite.Down := True;
  end;
end;

procedure TForm1.SoftGoldHomePage1Click(Sender: TObject);
const
  cnstUrl = 'www.cadsofttools.com';
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(Handle, 'open', cnstUrl, '', '', SW_SHOW);
  {$ELSE}
  sgOpenURL(cnstUrl);
  {$ENDIF}
end;

procedure TForm1.MIAboutClick(Sender: TObject);
begin
  fBuy.ShowAboutExecute;
end;

procedure TForm1.btnLayersClick(Sender: TObject);
begin
  if Assigned(Img) then
    LayersDialogExecute(Img, sgPaintBox);
end;

procedure TForm1.sgPaintBoxPaint(Sender: TObject);
var
  vLeftTop, vRightBottom: TPoint;
  vExt: TFRect;
  vBMPFrame: TBitmap;
  vImgRect, vPicRect: TRect;
  vPt: TPoint;
begin
  if Assigned(Img) and not Img.Empty then
  begin
    vExt := Img.Extents;
    vPicRect := sgPaintBox.PictureRect;
    if FSelectionMode = smEllipse then
    begin
      vLeftTop.X := Round((vPicRect.Right - vPicRect.Left) * (FTextBox.Left - vExt.Left) / (vExt.Right - vExt.Left)) + vPicRect.Left;
      vLeftTop.Y := Round((vPicRect.Bottom - vPicRect.Top) * (1 - (FTextBox.Top - vExt.Bottom) / (vExt.Top - vExt.Bottom))) + vPicRect.Top;
      vRightBottom.X := Round((vPicRect.Right - vPicRect.Left) *  (FTextBox.Right - vExt.Left) / (vExt.Right - vExt.Left)) + vPicRect.Left;
      vRightBottom.Y := Round((vPicRect.Bottom - vPicRect.Top) * (1 - (FTextBox.Bottom - vExt.Bottom) / (vExt.Top - vExt.Bottom))) + vPicRect.Top;
      sgPaintBox.Canvas.Pen.Color := clRed;
      sgPaintBox.Canvas.Brush.Style := bsClear;
      sgPaintBox.Canvas.Ellipse(vLeftTop.X - 10, vLeftTop.Y - 10, vRightBottom.X + 10, vRightBottom.Y + 10);
    end;
    if sbShowBorder.Enabled and sbShowBorder.Down then
    begin
      vBMPFrame := TBitmap.Create;
      try
        Graphic.Transparent := True;
        vBMPFrame.PixelFormat := pf8bit;
        vBMPFrame.Width := sgPaintBox.Width;
        vBMPFrame.Height := sgPaintBox.Height;
        vBMPFrame.Assign(imgFrame.Picture.Bitmap);

        // draw stretched border bitmap
        sgPaintBox.Canvas.StretchDraw(Rect(0, 0, sgPaintBox.Width, sgPaintBox.Height),
          vBMPFrame);

        FillChar(vImgRect, SizeOf(TRect), #0);
        vImgRect.Left := sgPaintBox.Width div 4;
        vImgRect.Top := sgPaintBox.Height div 4;
        vImgRect.Right := vImgRect.Left + sgPaintBox.Width div 2;
        vImgRect.Bottom := vImgRect.Top + sgPaintBox.Height div 2;
        // draw stretched CAD graphic
        sgPaintBox.Canvas.StretchDraw(sgPaintBox.PictureRect, Img);
      finally
        vBMPFrame.Free;
      end;
    end;
    if sbDrid.Down and (GridCount > 0) then
    begin
      if CreateLines(FIntPoints, Round(Img.AbsWidth*Img.Scale.X/GridCount),
        Round(Img.AbsWidth*Img.Scale.X),
        Round(Img.AbsHeight*Img.Scale.Y)) then
      begin
        sgPaintBox.Canvas.Pen.Color := clGray;
        {$IFDEF MSWINDOWS}
        OffsetViewportOrgEx(sgPaintBox.Canvas.Handle, vPicRect.Left, vPicRect.Top, vPt);
        {$ENDIF}
        DrawPolyPolyLine(sgPaintBox.Canvas.Handle, FIntPoints);
        SetViewportOrgEx(sgPaintBox.Canvas.Handle, vPt.X, vPt.Y, nil);
      end;
    end;
    if Assigned(FOnPaintExternal)  then
      FOnPaintExternal(Sender);
  end;
end;

procedure TForm1.sgPaintBoxPictureChange(Sender: TObject);
var
  S: string;
  R: TFRect;
begin
  if FGraphic <> sgPaintBox.Picture.Graphic then
  begin
    FGraphic := sgPaintBox.Picture.Graphic;
    if FGraphic is TsgCADImage then
      FImg := TsgCADImage(FGraphic)
    else
      FImg := nil;
  end;
  S := '';
  if Assigned(FImg) then
  begin
    R := FImg.Extents;
    S := Format('%.3f x %.3f x %.3f', [R.Right - R.Left, R.Top - R.Bottom, R.Z2 - R.Z1]);
  end
  else
    if Assigned(Graphic) then
      S := Format('%d x %d', [Graphic.Width, Graphic.Height]);
  pnlSize.Width := Canvas.TextWidth(S) + 4;
  pnlSize.Caption := S;
end;

procedure TForm1.sgPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  vPt: TFPoint;
  vUnits: string;
begin
  vPt := sgPaintBox.GetDrawingCoords(X, Y, vUnits);
  pnlCoords.Caption := Format('%.3f; %.3f; %.3f', [vPt.X, vPt.Y, vPt.Z]);
  if vUnits <> '' then
    pnlCoords.Caption := pnlCoords.Caption + ' (' + vUnits + ')';
end;

procedure TForm1.btnFitToScreenClick(Sender: TObject);
begin
  sgPaintBox.FitToSize;
  sgPaintBoxScaling(Self);
end;

procedure TForm1.btnIsWithoutBorderClick(Sender: TObject);
begin
  if Assigned(Img) then
    Img.IsWithoutBorder := not Img.IsWithoutBorder;
end;

procedure TForm1.btnShowLineWeightClick(Sender: TObject);
begin
  Img.IsShowLineWeight := btnShowLineWeight.Down;
end;

procedure TForm1.btnSplittedArcClick(Sender: TObject);
begin
  Img.UseWinEllipse := not btnSplittedArc.Down;
end;

procedure TForm1.btnDrawAllColorClick(Sender: TObject);
begin
  if btnDrawAllColor.Down then
  begin
    Img.DrawMode := dmNormal;
    mmiNormal.Checked := True;
  end
  else
  begin
    Img.DrawMode := dmBlack;
    mmiBlack.Checked := True;
  end;
end;

procedure TForm1.SetCADInfoVisible(const AVal: Boolean);
begin
  if csDestroying in pnlLayouts.ComponentState then Exit;
  cbLayouts.Visible := AVal;
  lblDridCount.Visible := AVal;
  lblLayouts.Visible := AVal;
  lblNumPartsCircle.Visible := AVal;
  lblNumPartsSpline.Visible := AVal;
  edtNumPartsCircle.Visible := AVal;
  edtNumPartsSpline.Visible := AVal;
  edtGridCount.Visible := AVal;
  udNumPartsCircle.Visible := AVal;
  udNumPartsSpline.Visible := AVal;
  udGridCount.Visible := AVal;
end;

procedure TForm1.SetGridCount(AValue: Integer);
begin
  udGridCount.Position := AValue;
  edtGridCount.Text := IntToStr(AValue);
end;

procedure TForm1.SetImgOpt;
begin
  btnShowLineWeightClick(nil);
  btnSplittedArcClick(nil);
  btnDrawAllColorClick(nil);
  btnWhiteBackgroundClick(nil);
end;

procedure TForm1.btnWhiteBackgroundClick(Sender: TObject);
begin
  sgPaintBox.BeginUpdate;
  try
    if btnWhiteBackground.Down then
    begin
      if Assigned(Img) then
      begin
        Img.BackgroundColor := clWhite;
        Img.DefaultColor := clBlack;
      end;
      sgPaintBox.Color := clWhite;
    end
    else
    begin
      if Assigned(Img) then
      begin
        Img.BackgroundColor := clBlack;
        Img.DefaultColor := clWhite;
      end;
      sgPaintBox.Color := clBlack;
    end;
  finally
    sgPaintBox.EndUpdate;
  end;
end;

procedure TForm1.sb3DAxesClick(Sender: TObject);
begin
  sb3DAxes.Down := sb3DAxes.Down or False;
  if sb3DAxes.Down then
    Img.IsDraw3DAxes := True
  else
    Img.IsDraw3DAxes := False;
  sgPaintBox.Invalidate;
end;


procedure TForm1.sb3DOrbitClick(Sender: TObject);
begin
  sb3DOrbit.Down := sb3DOrbit.Down or False;
  sgPaintBox.Orbit3D.Visible := sb3DOrbit.Down;
  sgPaintBox.Invalidate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FOptions.Free;
  FIntPoints.Free;
  ClearFilterList(FSaveDialogFilter);
  FSaveDialogFilter.Free;
end;

procedure TForm1.BtnPrint2Click(Sender: TObject);
var
  P: TPoint;
begin
//  SysUtils.Win32Check(Windows.GetCursorPos(P));
  GetCursorPos(P);
  PopupMenu1.Popup(P.X, P.Y);
end;

procedure TForm1.mmiSaveAsBigEMFClick(Sender: TObject);
{$IFNDEF SGFPC}
const
  cnstMaxEMFDim = 3276700;
{$IFDEF SGDEL_4}
var
  vM: TMetafile;
{$ENDIF}
begin
  if sgPaintBox.Empty or not Assigned(Img) then Exit;
  SavePictureDialogEMF.FileName := '*' + cnstEMFExt;
  if not SavePictureDialogEMF.Execute then Exit;
  if LowerCase(ExtractFileExt(SavePictureDialogEMF.FileName)) <> cnstEMFExt then
    SavePictureDialogEMF.FileName := ChangeFileExt(SavePictureDialogEMF.FileName, cnstEMFExt);
  {$IFNDEF SGDEL_4}
  Img.ExportToMetafile(SavePictureDialogEMF.FileName,
    cnstMaxEMFDim, Round(cnstMaxEMFDim * Img.AbsHeight / Img.AbsWidth));
  {$ELSE}
  vM := Img.ExportToMetafile(cnstMaxEMFDim);
  try
    vM.SaveToFile(SavePictureDialogEMF.FileName);   // this metafile can be opened in this viewer
  finally
    vM.Free;
  end;
  {$ENDIF}
{$ELSE}
begin
{$ENDIF}
end;

procedure TForm1.ViewLayouts;
var
  I: Integer;
begin
  if Assigned(Img) then
  begin
    sgPaintBox.BeginUpdate;
    try
      cbLayouts.Items.BeginUpdate;
      try
        cbLayouts.Items.Clear;
        for I := 0 to Img.LayoutsCount - 1 do
          cbLayouts.Items.AddObject(Img.Layouts[I].Name, Img.Layouts[I]);
        I := Img.Converter.DefaultLayoutIndex;
        if I >= Img.LayoutsCount then
          I := 0;
        cbLayouts.ItemIndex := I;
        Img.CurrentLayout := Img.Layouts[I];
      finally
        cbLayouts.Items.EndUpdate;
      end;
      pnlLayouts.Visible := cbLayouts.Items.Count > 0;
      Enable3DAxes(Img.CurrentLayout.CADSpace <> csUndefined);
      Enable3DOrbit(Img.CurrentLayout.CADSpace <> csUndefined);
      SetCADInfoVisible(True);
    finally
      sgPaintBox.EndUpdate;
    end;
  end;
end;

procedure TForm1.Enable3DAxes(AIsEnabled: Boolean);
begin
  sb3DAxes.Down := False;
  sb3DAxes.Enabled := AIsEnabled;
  if AIsEnabled = False then
    Img.IsDraw3DAxes := AIsEnabled;
end;

procedure TForm1.Enable3DOrbit(AIsEnabled: Boolean);
begin
  sb3DOrbit.Down := False;
  sb3DOrbit.Enabled := AIsEnabled;
end;

procedure TForm1.cbLayoutsChange(Sender: TObject);
begin
  if cbLayouts.Items.Objects[cbLayouts.ItemIndex] <> nil then
  begin
    sgPaintBox.BeginUpdate;
    try
      Img.CurrentLayout := Img.Layouts[cbLayouts.ItemIndex];
      sbDrawingBoxClick(nil);
      Enable3DAxes(Img.CurrentLayout.CADSpace <> csUndefined);
      Enable3DOrbit(Img.CurrentLayout.CADSpace <> csUndefined);
    finally
      sgPaintBox.EndUpdate;
    end;
  end;
end;

procedure TForm1.pgcDrawingChange(Sender: TObject);
begin
  if TreeView1.Items.Count = 0 then
    LoadStructure;
  if Assigned(pgcDrawing.ActivePage) and (pgcDrawing.ActivePage.PageIndex = 0) then
    pnlFileName.Caption := ExtractFileName(FFileName);
end;

procedure TForm1.speNumPartsCircleChange(Sender: TObject);
var
  vVal: Integer;
begin
  vVal := NumPartsCircle;
  if vVal >= iMinNumberOfPart then
  begin
    sgConsts.SetNumberOfCircleParts(vVal);
    if Assigned(Img) then
      Img.Converter.NumberOfPartsInCircle := sgConsts.GetNumberOfCircleParts;
    sgPaintBox.Refresh;
  end
  else
    NumPartsCircle := iMinNumberOfPart;
end;

procedure TForm1.speNumPartsSplineChange(Sender: TObject);
var
  vVal: Integer;
begin
  vVal := NumPartsSpline;
  if vVal >= iMinNumberOfPart then
  begin
    sgConsts.SetNumberOfSplineParts(vVal);
    if Assigned(Img) then
      Img.Converter.NumberOfPartsInSpline := sgConsts.GetNumberOfSplineParts;
    sgPaintBox.Refresh;
  end
  else
    NumPartsSpline := iMinNumberOfPart;
end;

function TForm1.StartPrint(const ATitle: string): Boolean;
begin
  Result := False;
  if Assigned(Graphic) then
  begin
    Printer.Orientation := cnstPrnOrient[Graphic.Width > Graphic.Height];
    Result := PrintDialog1.Execute;
    if Result then
    begin
      SetNullWidth(Graphic, 3);
      Printer.Title := ATitle;
    end;
  end;
end;

procedure TForm1.mmiShowpointClick(Sender: TObject);
var
  Form2: TForm2;
begin
  Form2 := TForm2.Create(Self);
  try
    Form2.ShowModal;
  finally
    Form2.Free;
  end;
end;

procedure TForm1.NewFile;
begin
  Open('');
end;

procedure TForm1.mmiFindtextClick(Sender: TObject);
var
  Form3: TForm3;
begin
  Form3 := TForm3.Create(Self);
  try
    Form3.ShowModal;
  finally
    Form3.Free;
  end;
end;

procedure TForm1.mmiReopenSameViewClick(Sender: TObject);
var
  vFPoint: TFPoint;
  vRect: TRect;
  vScale: Extended;
  vLayoutName, vUnits, vFileName: String;
begin
  if Assigned(Img) then
  begin
    sgPaintBox.BeginUpdate;
    try
      vLayoutName := Img.CurrentLayout.Name;
      vRect := sgPaintBox.ClientRect;
      vFPoint := sgPaintBox.GetDrawingCoords((vRect.Left + vRect.Right) div 2,
        (vRect.Top + vRect.Bottom) div 2, vUnits);
      vScale := sgPaintBox.Scale;
      vFileName := FFileName;
      MICloseClick(Self);
      Open(vFileName);
      Img.CurrentLayout := TsgDXFLayout(Img.Converter.Sections[csLayouts].FindEntByName(vLayoutName));
      sgPaintBox.ShowPoint(vFPoint, vScale);
    finally
      sgPaintBox.EndUpdate;
    end;
  end
  else
    ShowMessage(cnstNotCAD);
end;

procedure TForm1.miFontsClick(Sender: TObject);
begin
  if FontsOptionsExecute(FOptions, Form1) and (FFileName <> '') then
    Open(FFileName)
  else
    sgPaintBox.Invalidate;
end;

procedure TForm1.speGridCountChange(Sender: TObject);
begin
//  sgPaintBox.Picture.Graphic.Transparent := not sbDrid.Down;
  sgPaintBox.Invalidate;
end;

procedure TForm1.sbDridClick(Sender: TObject);
begin
  speGridCountChange(nil);
end;

procedure TForm1.sbDrawingBoxClick(Sender: TObject);
var
  vBox: TFRect;
begin
  if Img = nil then Exit;
  sgPaintBox.BeginUpdate;
  try
    sbDrawingBox.Down := sbDrawingBox.Down or False;
    Enable3DOrbit(not sbDrawingBox.Down);
    if sbDrawingBox.Down then
    begin
      vBox := Img.CurrentLayout.Box;
      Img.DrawingBox := MakeFRect((vBox.Left + vBox.Right) / 2,
        vBox.Top, vBox.Z1, vBox.Right, vBox.Bottom, vBox.Z2)
    end
    else
      Img.ResetDrawingBox;
  finally
    sgPaintBox.EndUpdate;
  end;
end;

procedure TForm1.mmiReopenSWIsometricClick(Sender: TObject);
var
  vLayoutName, vFileName: String;
begin
  if Assigned(Img) then
  begin
    sgPaintBox.BeginUpdate;
    try
      vLayoutName := Img.CurrentLayout.Name;
      if FFileName = '' then Exit;
      vFileName := FFileName;
      MICloseClick(Self);
      Open(vFileName);
      Img.CurrentLayout := TsgDXFLayout(Img.Converter.Sections[csLayouts].FindEntByName(vLayoutName));
      if Img.CurrentLayout.CADSpace = cs3D then
      begin
        Img.RotToView(vdTop);
        Img.Rotate(AxisZ, 45);
        Img.Rotate(AxisX, -54.735610317245345684622999669981);
      end
      else ShowMessage('The current layout of this file is not model. This operation does not work.');
    finally
      sgPaintBox.EndUpdate;
    end;
  end
  else ShowMessage(cnstNotCAD);
end;

procedure TForm1.sbShowBorderClick(Sender: TObject);
begin
  if sbShowBorder.Down then
  begin
    if OpenBorder.Execute then
    begin
      imgFrame.Picture.LoadFromFile(OpenBorder.FileName);
      sb3DOrbit.Enabled := False;
      sb3DAxes.Enabled := False;
    end
    else
    begin
      sbShowBorder.Down := False;
      sb3DOrbit.Enabled := True;
      sb3DAxes.Enabled := True;
      Graphic.Transparent := False;
    end;
  end
  else
  begin
    if not imgFrame.Picture.Bitmap.Empty then
      imgFrame.Picture.Graphic := nil;
    Graphic.Transparent := False;
    sb3DOrbit.Enabled := True;
    sb3DAxes.Enabled := True;
  end;
  sgPaintBox.Invalidate;
end;


procedure TForm1.mmiObjectSnapAllClick(Sender: TObject);
{$IFDEF SGDEL_4}
var
  vSnapMask: TObjectSnapState;
  vMatrixMode: TsgSelectionMode;
{$ENDIF}
begin
{$IFDEF SGDEL_4}
  mmiObjectSnapAll.Checked := not mmiObjectSnapAll.Checked;
  //Object Snap. Snap mode setup
  sgPaintBox.BeginUpdate;
  try
    vMatrixMode := smDisabled;
    if Assigned(Img) then
    begin
      vMatrixMode := Img.GetMatrixMode();
      Img.SetMatrixMode(smMatrixOnly);
    end;
    try
      Exclude(vSnapMask, osDisabled);
      if mmiObjectSnapAll.Checked then
        vSnapMask := cnstSnapMaskAll
      else
        vSnapMask := cnstSnapMaskNone;
      sgPaintBox.SnapControl.SnapMask := vSnapMask;
      TsgDrawingNavigatorAccess(sgPaintBox).PictureChanged := True;
    finally
      if Assigned(Img) then
        Img.SetMatrixMode(vMatrixMode);
    end;
  finally
    sgPaintBox.EndUpdate;
  end;
{$ENDIF}
end;

function TForm1.GetCADImage: TsgCADImage;
begin
  Result := FImg;
end;

function TForm1.GetGraphic: TGraphic;
begin
  Result := FGraphic;
end;

function TForm1.GetGridCount: Integer;
begin
  Result := StrToIntDef(edtGridCount.Text, cnstDefGridCount);
end;

function TForm1.GetNumPartsCircle: Integer;
begin
  Result := StrToIntDef(edtNumPartsCircle.Text, sgConsts.GetNumberOfCircleParts);
end;

function TForm1.GetNumPartsSpline: Integer;
begin
  Result := StrToIntDef(edtNumPartsSpline.Text, sgConsts.GetNumberOfSplineParts);
end;

procedure TForm1.GotoHomepage1Click(Sender: TObject);
const
  cnstUrl = 'https://cadsofttools.com/products/cad-vcl-enterprise/';
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(Application.Handle, 'open', cnstUrl, nil, nil, SW_SHOW);
  {$ELSE}
  sgOpenURL(cnstUrl);
  {$ENDIF}
end;

procedure TForm1.actFormUpdate(Sender: TObject);
begin
  EnableControls;
end;

destructor TForm1.Destroy;
begin
{$IFDEF SG_XMLAPI}
  FreeAndNil(FXMLIDE);
{$ENDIF}
  inherited;
end;

{$IFDEF DEMO}
procedure TForm1.DestroyFrameInfo;
var
  vFrameInfo: TComponent;
begin
  if FFrameInfoHandle <> 0 then
  begin
    vFrameInfo := FindControl(FFrameInfoHandle);
    if Assigned(vFrameInfo) then
    begin
      vFrameInfo.Free;
      FFrameInfoHandle := 0;
    end;
  end;
end;
{$ENDIF}


{$IFNDEF SGDEL_4}
procedure TForm1.UpdateActions;
var
  I: Integer;

  procedure TraverseClients(Container: TWinControl);
  var
    I: Integer;
    Control: TControl;
  begin
    if Container.Showing then
      for I := 0 to Container.ControlCount - 1 do
      begin
        Control := Container.Controls[I];
        if Control.Visible then
            Control.Update;
        if Control is TWinControl then
          TraverseClients(TWinControl(Control));
      end;
  end;

begin
  if (csDesigning in ComponentState) or not Showing then Exit;
  { Update form }
  Update;
  { Update main menu's top-most items }
  if Menu <> nil then
    for I := 0 to Menu.Items.Count - 1 do
        if Menu.Items[I].Visible then Update;
  { Update any controls }
  TraverseClients(Self);
  actFormUpdate(nil);
end;

procedure TForm1.DoActionIdle;
var
  I: Integer;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
    if Screen.CustomForms[I].HandleAllocated and
       IsWindowVisible(Screen.CustomForms[I].Handle) and
       IsWindowEnabled(Screen.CustomForms[I].Handle) then
      UpdateActions;
end;

{$ENDIF}

procedure TForm1.WndProc(var Message: TMessage);
begin
  inherited;
  {$IFNDEF SGDEL_4}
  DoActionIdle;
  {$ENDIF}
end;

procedure TForm1.AppIdle(Sender: TObject; var Done: Boolean);
begin
  {$IFNDEF SGDEL_4}
  DoActionIdle;
  {$ENDIF}
end;

procedure TForm1.BtnRealClick(Sender: TObject);
begin
  sgPaintBox.Scale := 1.0;
  BtnReal.Enabled := False;
end;

procedure TForm1.mmiScaleClick(Sender: TObject);
begin
  sgPaintBox.Scale := TComponent(Sender).Tag / 100.0;
end;

{$IFDEF SG_XMLAPI}
{ TsgMainForm1 }

constructor TsgMainForm1.Create(const AForm: TObject);
begin
  FForm := TForm1(AForm);
end;

function TsgMainForm1.GetDrawingNavigator: TsgDrawingNavigator;
begin
  Result := FForm.sgPaintBox;
end;

function TsgMainForm1.GetImage: TsgCADImage;
begin
  Result := FForm.Img;
end;

function TsgMainForm1.Open(const AFileName: string): Boolean;
begin
  Result := True;
  try
    FForm.Open(AFileName);
  except
    Result := False;
  end;
end;

function TsgMainForm1.Save(const ASource: TGraphic; const AFileName: string;
  const AFormat: TsgExportFormat; const AVersion: TsgDWGVersion): Boolean;
begin
  Result := False;
  try
    if FForm.Save(ASource, AFileName, AFormat, AVersion) then
      Result := True;
  except
  end;
end;
{$ENDIF}

// 2025.01.25 최승선 수정
//------------------------------------------------------------------------------
// 로그 내용을 파일로 저장
//------------------------------------------------------------------------------
procedure SaveLogToFile(const LogDirectory, LogFileName, LogContent: string);
var
  FileStream: TFileStream;
  LogData: TBytes;
  Encoding: TEncoding;
begin
  ForceDirectories(LogDirectory); // 디렉토리가 없으면 생성
  if FileExists(IncludeTrailingPathDelimiter(LogDirectory) + LogFileName) then
    FileStream := TFileStream.Create(IncludeTrailingPathDelimiter(LogDirectory) + LogFileName, fmOpenReadWrite or fmShareDenyWrite)
  else
    FileStream := TFileStream.Create(IncludeTrailingPathDelimiter(LogDirectory) + LogFileName, fmCreate);
  try
    FileStream.Seek(0, soEnd); // 파일 끝으로 이동
    Encoding := TEncoding.UTF8; // BOM 포함 UTF-8 사용
    LogData := Encoding.GetPreamble + Encoding.GetBytes(LogContent + sLineBreak);
    FileStream.Write(LogData[0], Length(LogData));
  finally
    FileStream.Free;
  end;
end;

//------------------------------------------------------------------------------
// 에러 내용을 파일로 저장
//------------------------------------------------------------------------------
procedure SaveErrorToFile(const ErrorDirectory, ErrorFileName, ErrorContent: string);
var
  FileStream: TFileStream;
  ErrorData: TBytes;
  Encoding: TEncoding;
begin
  ForceDirectories(ErrorDirectory); // 디렉토리가 없으면 생성
  if FileExists(IncludeTrailingPathDelimiter(ErrorDirectory) + ErrorFileName) then
    FileStream := TFileStream.Create(IncludeTrailingPathDelimiter(ErrorDirectory) + ErrorFileName, fmOpenReadWrite or fmShareDenyWrite)
  else
    FileStream := TFileStream.Create(IncludeTrailingPathDelimiter(ErrorDirectory) + ErrorFileName, fmCreate);
  try
    FileStream.Seek(0, soEnd); // 파일 끝으로 이동
    Encoding := TEncoding.UTF8; // BOM 포함 UTF-8 사용
    ErrorData := Encoding.GetPreamble + Encoding.GetBytes(ErrorContent + sLineBreak);
    FileStream.Write(ErrorData[0], Length(ErrorData));
  finally
    FileStream.Free;
  end;
end;

// 2024.11.28 최승선 수정
//------------------------------------------------------------------------------
// CAD 데이터를 CSV로 저장
//------------------------------------------------------------------------------
procedure TForm1.SaveTextToCSV(ACADImage: TsgCADImage; const ACSVFileName, regExFile: string);
var
  CSVData: TStringList;
  I, J: Integer;
  Entity: TsgDXFEntity;
  Bounds: TInsertBounds; // 블록 또는 텍스트의 좌표 영역 정보를 저장
  BlockBounds: array of TInsertBounds; // 블록 좌표 배열
  BlockBounds120: array of TInsertBounds; // 블록의 120% 확장된 좌표 배열
  SingleTextBounds: array of TInsertBounds; // Single Line Text 좌표 배열
  SingleTexts: array of string; // Single Line Text 내용을 저장하는 배열
  BlockToTextMap: array of array of Integer; // 블록별 포함된 텍스트 인덱스를 매핑
  BlockTextBundles: array of string; // 블록별 묶인 텍스트
  IsTextAssigned: array of Boolean; // Single Line Text가 블록에 포함되었는지 여부
  MatchedClass, TempText: string;
  RegexList: TStringList; // 정규식을 저장하는 리스트

  // 엑셀에서 정규식을 로드
  procedure LoadRegexFromExcel(const FileName: string);
  var
    ExcelApp, Workbook, Sheet: OleVariant;
    Row, LastRow: Integer;
  begin
    RegexList := TStringList.Create;
    try
      ExcelApp := CreateOleObject('Excel.Application');
      try
        Workbook := ExcelApp.Workbooks.Open(FileName);
        Sheet := Workbook.Worksheets[1];

        // 마지막 행 확인
        LastRow := Sheet.UsedRange.Rows.Count;

        // Excel 데이터 읽기
        for Row := 2 to LastRow do
        begin
          RegexList.Add(Sheet.Cells[Row, 1].Value + '=' + Sheet.Cells[Row, 2].Value);
        end;

        Workbook.Close(False); // Workbook 닫기
      finally
        ExcelApp.Quit;         // Excel 종료
        VarClear(ExcelApp);    // Variant 해제
        VarClear(Workbook);    // Variant 해제
        VarClear(Sheet);       // Variant 해제
      end;
    except
      on E: Exception do
        ShowMessage('Excel 파일을 읽는 중 오류 발생: ' + E.Message);
    end;
  end;


  // 정규식을 순차적으로 적용하여 클래스 매칭
  function MatchClassForText(const Text: string): string;
  var
    RegEx: TRegEx;
    Pair, ClassName, Pattern: string;
    Matched: Boolean;
  begin
    Result := 'UnKnown'; // 기본값 설정
    for Pair in RegexList do
    begin
      // "클래스명=정규식" 형식 분리
      ClassName := Copy(Pair, 1, Pos('=', Pair) - 1);
      Pattern := Copy(Pair, Pos('=', Pair) + 1, MaxInt);
      // 정규식 적용
      RegEx := TRegEx.Create(Pattern);
      Matched := RegEx.IsMatch(Text);
      if Matched then
      begin
        Result := ClassName;
        Break; // 일치하면 더 이상 처리하지 않음
      end;
    end;
  end;

  // 특수 문자를 XML 엔터티로 변환
  function EscapeXMLSpecialChars(const Input: string): string;
  begin
    Result := Input;
    Result := sysUtils.StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
    Result := sysUtils.StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := sysUtils.StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result := sysUtils.StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
    Result := sysUtils.StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
    Result := sysUtils.StringReplace(Result, '°', '&#176;', [rfReplaceAll]);
  end;

  // 주어진 영역(Bounds)을 중심을 기준으로 120%로 확장
  procedure AdjustBounds(var Bounds: TInsertBounds);
  var
    CenterX, CenterY: Double;
  begin
    CenterX := (Bounds.Left + Bounds.Right) / 2; // 중심 X 좌표 계산
    CenterY := (Bounds.Top + Bounds.Bottom) / 2; // 중심 Y 좌표 계산

    // 좌, 우, 상, 하 거리 계산 및 120%로 확장
    Bounds.Left := CenterX - (CenterX - Bounds.Left) * 1.2;
    Bounds.Right := CenterX + (Bounds.Right - CenterX) * 1.2;
    Bounds.Top := CenterY + (Bounds.Top - CenterY) * 1.2;
    Bounds.Bottom := CenterY - (CenterY - Bounds.Bottom) * 1.2;
  end;

  // `BlockBounds120`와 `SingleTextBounds`를 비교하여 텍스트가 블록에 포함되는지 확인
  function IsTextInBlock(Block: TInsertBounds; TextBounds: TInsertBounds): Boolean;
  begin
    Result := (TextBounds.Left >= Block.Left) and (TextBounds.Right <= Block.Right) and
              (TextBounds.Top <= Block.Top) and (TextBounds.Bottom >= Block.Bottom);
  end;

  // 블록(120%) 영역에 포함된 텍스트를 매핑하여 `BlockToTextMap`에 저장
  procedure MapTextsToBlocks();
  var
    BlockIndex, TextIndex: Integer;
  begin
    SetLength(BlockToTextMap, Length(BlockBounds120)); // 블록 개수만큼 배열 초기화
    SetLength(IsTextAssigned, Length(SingleTextBounds)); // SingleTextBounds와 동일한 크기 초기화

    for BlockIndex := 0 to High(BlockBounds120) do
    begin
      for TextIndex := 0 to High(SingleTextBounds) do
      begin
        if IsTextInBlock(BlockBounds120[BlockIndex], SingleTextBounds[TextIndex]) then
        begin
          // 블록에 포함된 텍스트 인덱스를 배열에 추가
          SetLength(BlockToTextMap[BlockIndex], Length(BlockToTextMap[BlockIndex]) + 1);
          BlockToTextMap[BlockIndex][High(BlockToTextMap[BlockIndex])] := TextIndex;
          IsTextAssigned[TextIndex] := True; // 해당 텍스트가 블록에 포함되었음을 표시
        end;
      end;
    end;
  end;

  // 블록별 포함된 텍스트를 묶어서 `BlockTextBundles`에 저장
  procedure CreateBlockTextBundles();
  var
    BlockIndex, TextIndex: Integer;
    TextIndices: array of Integer; // 동적 배열
    SortedTexts: TStringList;      // 텍스트를 정렬하기 위한 리스트
    IndexCount: Integer;
    TempText: string;              // 임시 문자열 변수
    i: Integer;                    // 반복문용 변수
  begin
    SetLength(BlockTextBundles, Length(BlockBounds120)); // 블록 개수만큼 배열 초기화
    SortedTexts := TStringList.Create;
    try
      for BlockIndex := 0 to High(BlockBounds120) do
      begin
        IndexCount := Length(BlockToTextMap[BlockIndex]);
        SetLength(TextIndices, IndexCount); // TextIndices 배열 크기 설정
        for TextIndex := 0 to IndexCount - 1 do
        begin
          TextIndices[TextIndex] := BlockToTextMap[BlockIndex][TextIndex];
        end;

        SortedTexts.Clear;

        // 텍스트 인덱스와 Y 좌표를 정렬 리스트에 추가
        for TextIndex in TextIndices do
        begin
          SortedTexts.Add(SingleTexts[TextIndex]); // 텍스트만 추가
        end;

        // 직접 치환: 줄바꿈 대신 "-"로 묶음
        TempText := '';
        for i := 0 to SortedTexts.Count - 1 do
        begin
          if i > 0 then
            TempText := TempText + '-'; // 구분자로 "-" 추가
          TempText := TempText + SortedTexts[i]; // 텍스트 추가
        end;

        BlockTextBundles[BlockIndex] := TempText; // 묶인 텍스트 저장
      end;
    finally
      SortedTexts.Free; // 메모리 해제
    end;
  end;

  // 블록을 검색하고 BlockBounds 및 BlockBounds120 배열에 저장
  procedure FindBlocks();
  var
    J: Integer;
    BlockEntity: TsgDXFEntity;
    Box: TFRect;
  begin
    if ACADImage.Converter.Counts[csEntities] > 0 then
    begin
      for J := 0 to ACADImage.Converter.Counts[csEntities] - 1 do
      begin
        BlockEntity := ACADImage.Converter.Entities[J];
        if BlockEntity is TsgDXFInsert then
        begin
          Box := BlockEntity.Box;
          Bounds.Left := Box.Left;
          Bounds.Top := Box.Top;
          Bounds.Right := Box.Right;
          Bounds.Bottom := Box.Bottom;

          SetLength(BlockBounds, Length(BlockBounds) + 1);
          BlockBounds[High(BlockBounds)] := Bounds;

          AdjustBounds(Bounds);
          SetLength(BlockBounds120, Length(BlockBounds120) + 1);
          BlockBounds120[High(BlockBounds120)] := Bounds;
        end;
      end;
    end
    else
      ShowMessage('No entities in csEntities section.');
  end;


begin
  CSVData := TStringList.Create; // CSV 데이터 저장 리스트 생성
  try
    // 정규식을 엑셀에서 로드
    LoadRegexFromExcel(regExFile);
    FindBlocks(); // 블록을 검색하고 BlockBounds 및 BlockBounds120 배열에 저장

    // 텍스트 엔티티를 순회하여 Single Line Text를 저장
    for I := 0 to ACADImage.Converter.Counts[csEntities] - 1 do
    begin
      Entity := ACADImage.Converter.Entities[I];
      if (Entity is TsgDXFText) then
      begin
        if Entity.Box.Left <> 0 then
        begin
          // 싱글 텍스트 영역 저장
          Bounds.Left := Entity.Box.Left;
          Bounds.Top := Entity.Box.Top;
          Bounds.Right := Entity.Box.Right;
          Bounds.Bottom := Entity.Box.Bottom;

          // 싱글 텍스트 영역 배열에 저장
          SetLength(SingleTextBounds, Length(SingleTextBounds) + 1);
          SingleTextBounds[High(SingleTextBounds)] := Bounds;

          // 싱글 텍스트 내용 배열에  저장
          SetLength(SingleTexts, Length(SingleTexts) + 1);
          SingleTexts[High(SingleTexts)] := TsgDXFText(Entity).Text;
        end;
      end
      else if (Entity is TsgDXFMText) then
      begin
        // Multi Line Text를 CSV에 바로 저장 (줄바꿈 처리 추가)
        TempText := TsgDXFMText(Entity).Text; // Multi Line Text 가져오기
        //TempText := System.SysUtils.StringReplace(TempText, sLineBreak, '-', [rfReplaceAll]); // 줄바꿈을 '-'로 치환
        TempText := System.SysUtils.StringReplace(TempText, '\', '-', [rfReplaceAll]); // '\'를 '-'로 치환
        MatchedClass := MatchClassForText(TempText);
        CSVData.Add(Format('%s,%s', [MatchedClass, TempText]));
        //CSVData.Add(Format('Multi Line,"%s"', [TempText])); // 수정된 텍스트 추가
        //CSVData.Add(TempText); // 수정된 텍스트 추가
      end;
    end;

    // 텍스트를 블록과 2차원배열의 텍스트에 매핑(블록영역의 120프로에 속하는 텍스트)
    MapTextsToBlocks();

    // 블록별 텍스트 묶음 생성(y 좌표로 앞뒤 구분하여 -로 묶어준다)
    CreateBlockTextBundles();

    // 블록별 묶인 텍스트를 CSV에 추가
    for I := 0 to High(BlockTextBundles) do
    begin
      // 공백 문자열 제외 조건 추가
      if Trim(BlockTextBundles[I]) <> '' then
      begin
        //CSVData.Add(Format('Block Single Line,"%s"', [BlockTextBundles[I]]));
        TempText := BlockTextBundles[I];
        MatchedClass := MatchClassForText(TempText);
        CSVData.Add(Format('%s,%s', [MatchedClass, TempText]));
      end;
    end;

    // 블록에 포함되지 않은 Single Line Text를 추가
    for I := 0 to High(SingleTexts) do
    begin
      if not IsTextAssigned[I] then // 블록에 포함되지 않은 경우
      begin
        //CSVData.Add(Format('Single Line,"%s"', [SingleTexts[I]]));
        TempText := SingleTexts[I];
        MatchedClass := MatchClassForText(TempText);
        CSVData.Add(Format('%s,%s', [MatchedClass, TempText]));
      end;
    end;

    // CSV 파일로 저장
    CSVData.SaveToFile(ACSVFileName); // CSV 파일 저장
  finally
    CSVData.Free; // 메모리 해제
  end;
end;



procedure TForm1.SaveToSVG(ACADImage: TsgCADImage; const AFileName: string);
var
  vCADtoSVG: TsgCADtoSVG;
begin
  vCADtoSVG := TsgCADtoSVG.Create(ACADImage);
  try
    //1. 텍스트 모드
    vCADtoSVG.TTFMode := ttfGDI;
    // 2. 라인 두께 관련 설정
    vCADtoSVG.IsShowLineWeight := True;    // 라인 두께 표시
    vCADtoSVG.NullWeightMode := 4;       // layer 두께 적용
    // 3. 배경색 및 기본 색상 설정
    vCADtoSVG.DrawMode := TsgDXFDrawMode.dmNormal; // 기본 드로우 모드 설정
    vCADtoSVG.Transparency := False;
    if Img.BackgroundColor = clBlack then
    begin
      vCADtoSVG.BackgroundColor := clBlack; // 배경색을 검은색으로 설정
      vCADtoSVG.DefaultColor := clWhite;    // 배경색에 맞게 기본 색상을 흰색으로 설정
    end
    else
    begin
      vCADtoSVG.BackgroundColor := clWhite; // 배경색을 흰색으로 설정
      vCADtoSVG.DefaultColor := clBlack;    // 배경색에 맞게 기본 색상을 검정으로 설정
    end;
    // 4. svg파일 저장
    vCADtoSVG.SaveToFile(AFileName);
  finally
    vCADtoSVG.Free;
  end;
end;

procedure TForm1.SvgConvert;
  procedure openFile(openFilePath: string);
  begin
    CADPreview := True;
    try
      CADPreview := False;
      MICloseClick(nil);
      Open(openFilePath);
    finally
      CADPreview := False;
    end;
  end;

  procedure saveFile(const ASource: TGraphic; const AFileName: string);
  var
    vFileName: string;
    vImg: TsgCADImage;
    vCur: TCursor;
  begin
    CADPreview := True;
    vFileName := AFileName;
    vImg := nil;
    if ASource is TsgCADImage then
      vImg := TsgCADImage(ASource);
    vCur := Cursor;
    Cursor := crHourGlass;
    try
      SaveToSVG(vImg, vFileName);
    finally
      Cursor := vCur;
    end;
  end;

var
  iniFile: TIniFile;
  srcDir, tgtDir, srcFilePath, tgtFilePath, csvFilePath, fileNameWithoutExt, groupID, logDir, errDir, logFile, errFile, regExFile: string;
  sr: TSearchRec;
  totalFiles, successCount, failCount: Integer;
  startTime, endTime: TDateTime;
  LogContent, ErrorContent: TStringList;
begin
  // 로그 초기화
  startTime := Now;
  groupID := FormatDateTime('yyyymmdd_hhnnss', startTime);

  LogContent := TStringList.Create;
  ErrorContent := TStringList.Create;
  try
    // 설정파일 경로
    iniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
    try
      // 설정파일에서 디렉터리 경로 읽기
      srcDir := iniFile.ReadString('Paths', 'SourceDirectory', '');
      tgtDir := iniFile.ReadString('Paths', 'TargetDirectory', '');
      logDir := iniFile.ReadString('Paths', 'LogDirectory', '');
      errDir := iniFile.ReadString('Paths', 'ErrorDirectory', '');
      regExFile := iniFile.ReadString('Paths', 'regExFile', '');

      // 디렉터리 값이 비어 있는 경우 예외 처리
      if (srcDir = '') or (tgtDir = '') or (logDir = '') or (errDir = '') then
        raise Exception.Create('One or more directories not specified in config.ini.');

      // 로그 디렉터리 생성
      ForceDirectories(IncludeTrailingPathDelimiter(logDir) + FormatDateTime('yyyy', startTime));
      ForceDirectories(IncludeTrailingPathDelimiter(errDir) + FormatDateTime('yyyy', startTime));

      // 로그 파일 경로 설정
      logFile := IncludeTrailingPathDelimiter(logDir) + FormatDateTime('yyyy', startTime) + '\' + FormatDateTime('yyyymmdd', startTime) + '.log';
      errFile := IncludeTrailingPathDelimiter(errDir) + FormatDateTime('yyyy', startTime) + '\' + FormatDateTime('yyyymmdd', startTime) + '_error.log';

      // 초기 로그 작성
      LogContent.Add(Format('--- 작업 그룹 시작: %s ---', [DateTimeToStr(startTime)]));
      LogContent.Add(Format('작업 그룹 ID: %s', [groupID]));

      totalFiles := 0;
      successCount := 0;
      failCount := 0;

      // 디렉터리 내의 DWG 파일 처리
      if FindFirst(srcDir + '*.dwg', faAnyFile, sr) = 0 then
      begin
        repeat
          Inc(totalFiles);
          srcFilePath := srcDir + sr.Name;
          fileNameWithoutExt := ChangeFileExt(sr.Name, ''); // 파일명만 추출(확장자 제외)
          tgtFilePath := tgtDir + fileNameWithoutExt + '.svg';
          csvFilePath := tgtDir + fileNameWithoutExt + '.csv';

          try
            openFile(srcFilePath);
            saveFile(Graphic, tgtFilePath);

            // 텍스트 추출 및 CSV 저장
            if Graphic is TsgCADImage then
              SaveTextToCSV(TsgCADImage(Graphic), csvFilePath, regExFile);

            Inc(successCount);
          except
            on E: Exception do
            begin
              Inc(failCount);
              ErrorContent.Add(Format('작업 그룹 ID: %s', [groupID]));
              ErrorContent.Add('--- 에러 로그 시작 ---');
              ErrorContent.Add(Format('파일 이름: %s', [sr.Name]));
              ErrorContent.Add(Format('에러 발생 시각: %s', [DateTimeToStr(Now)]));
              ErrorContent.Add(Format('에러 메시지: %s', [E.Message]));
              ErrorContent.Add('--- 에러 로그 종료 ---');
            end;
          end;
        until FindNext(sr) <> 0;
        FindClose(sr);
      end
      else
      begin
        raise Exception.Create('No DWG files found in the source directory.');
      end;

      // 최종 로그 작성
      endTime := Now;
      LogContent.Add(Format('총 파일 수: %d', [totalFiles]));
      LogContent.Add(Format('성공 파일 수: %d', [successCount]));
      LogContent.Add(Format('실패 파일 수: %d', [failCount]));
      LogContent.Add(Format('소요 시간: %.2f 초', [(endTime - startTime) * 24 * 60 * 60]));
      LogContent.Add(Format('--- 작업 그룹 종료: %s ---', [DateTimeToStr(endTime)]));

      // 로그 파일 저장
      SaveLogToFile(IncludeTrailingPathDelimiter(logDir) + FormatDateTime('yyyy', startTime),
                    FormatDateTime('yyyymmdd', startTime) + '.log', LogContent.Text);
      if ErrorContent.Count > 0 then
        SaveErrorToFile(IncludeTrailingPathDelimiter(errDir) + FormatDateTime('yyyy', startTime),
                        FormatDateTime('yyyymmdd', startTime) + '_error.log', ErrorContent.Text);
    finally
      iniFile.Free;
    end;
  finally
    LogContent.Free;
    ErrorContent.Free;
  end;

  // 프로그램 종료
  Application.Terminate;
end;

end.
