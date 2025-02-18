unit fCustomExport;

{$INCLUDE SGDXF.INC}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CADExport,
{$IFDEF SGMULLANG}
  sgMultilang,
{$ENDIF}
  IniFiles, Grids, ValEdit;

type
  TfrmCustomExport = class(TFrame)
    tvTabs: TTreeView;
    pnlSizes: TPanel;
    gbSizes: TGroupBox;
    lblWidth: TLabel;
    lblHeight: TLabel;
    lblUnits: TLabel;
    lblMargin: TLabel;
    lblMarginUnits: TLabel;
    rbCustom: TRadioButton;
    edtWidth: TEdit;
    edtHeight: TEdit;
    cbUnits: TComboBox;
    rbStandard: TRadioButton;
    cbSizes: TComboBox;
    edtMargin: TEdit;
    gbColors: TGroupBox;
    lblBackgroundColor: TLabel;
    lblDefaultColor: TLabel;
    ccbBackgroundColor: TColorBox;
    ccbDefaultColor: TColorBox;
    rgPalette: TRadioGroup;
    gbQuality: TGroupBox;
    pnlLayouts: TPanel;
    gbLayouts: TGroupBox;
    rbModel: TRadioButton;
    rbAllLayouts: TRadioButton;
    rbLayoutByName: TRadioButton;
    rbAllPaperSpaces: TRadioButton;
    edtLayoutName: TEdit;
    rbCurrentLayout: TRadioButton;
    pnlLineWidth: TPanel;
    gbLinewidth: TGroupBox;
    lbWidthNull: TLabel;
    lbWidthNullMM: TLabel;
    lblLineWeightScale: TLabel;
    cbSaveLinewight: TCheckBox;
    edtWidthNull: TEdit;
    udWidthNull: TUpDown;
    edtLineWeightScale: TEdit;
    pnlProps: TPanel;
    gbProps: TGroupBox;
    lblTitle: TLabel;
    lblAuthor: TLabel;
    lblSubject: TLabel;
    lblKeywords: TLabel;
    edtTitle: TEdit;
    edtAuthor: TEdit;
    edtSubject: TEdit;
    edtKeywords: TEdit;
    gbWeightByColor: TGroupBox;
    vleColorToLineWeight: TValueListEditor;
    btColorLoad: TButton;
    btColorSave: TButton;
    btColorClear: TButton;
    rbPlotSetting: TRadioButton;
    cbHighQuality: TCheckBox;
    procedure ccbColorGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure rbCustomClick(Sender: TObject);
    procedure edtExit(Sender: TObject);
    procedure edtKeyPress(Sender: TObject; var Key: Char);
    procedure udWidthNullChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure rbLayoutClick(Sender: TObject);
    procedure tvTabsClick(Sender: TObject);
    procedure tvTabsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vleColorToLineWeightDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    FTranslated: Boolean;
    FPageWidth: Double;
    FPageHeight: Double;
    FOnChanged: TNotifyEvent;
    FInvalidFloating: string;
{$IFDEF SGMULLANG}
    FMLText: TsgMultiLang;
{$ENDIF}
    FExportParams: TsgExportParams;
    FStdWeights: TStringList;
{$IFDEF SGMULLANG}
    procedure MLTextAfterLoad(Sender: TObject);
{$ENDIF}
    function GetSize(var W, H: Double): Boolean;
    procedure SetPageHeight(const Value: Double);
    procedure SetPageWidth(const Value: Double);
    procedure UpDownEditChange(AEdit: TEdit; AUpDown: TUpDown;
      Direction: TUpDownDirection; ADelta: Double = 0.3);
    procedure Translate;
    procedure SetExportParams(const Value: TsgExportParams);
    procedure ClearColorToLineWeight;
  protected
    procedure CMChanged(var Message: TMessage); message CM_CHANGED;
    function GetLayoutExportMode: Integer;
    procedure SetLayoutExportMode(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadOptions(const AOptions: TCustomIniFile);
    procedure SaveOptions(const AOptions: TCustomIniFile);
    function DoMsgDlg(const AText: string; const ACaption: string = '';
      AType: Cardinal = MB_OK): Integer;
    property Caption;
{$IFDEF SGMULLANG}
    property MLText: TsgMultiLang read FMLText;
{$ENDIF}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property PageWidth: Double read FPageWidth write SetPageWidth;
    property PageHeight: Double read FPageHeight write SetPageHeight;
    property ExportParams: TsgExportParams read FExportParams write SetExportParams;
  end;

  procedure GetExportParam(AFrame: TfrmCustomExport; AExportParams: PsgExportParams);
  procedure LoadExportParams(AFrame: TfrmCustomExport; AExportParams: PsgExportParams);

implementation

uses
  sgConsts, SysConst;

{$R *.dfm}

const
  MMPerInch = 25.4;
  SNonZero: string = 'The value must be greater than zero';
  cMaxDoubleValue = 1.7e+10;
  cMinDoubleValue = - cMaxDoubleValue;
  cnstDefWidth = 210;
  cnstDefHeight = 297;

type
  TCustomColorBoxAccess = class(TCustomColorBox);
  TWinControlAccess = class(TWinControl);

function DtoS(D: Double; Digits: Integer = 4): string;
var
  DS: Char;
begin
  DS := SetDecimalSeparator('.');
  try
    Result := FloatToStrF(D, ffFixed, 15, Digits);
  finally
    SetDecimalSeparator(DS);
  end;
end;

function StrToD(var S: string; var D: Double): LongBool;
var
  DS: Char;
begin
  DS := SetDecimalSeparator('.');
  try
    Integer(Result) := Pos(DS, S);
    if Integer(Result) > 0 then
      S[Integer(Result)] := '.';
    Val(S, D, Integer(Result));
    Result := not Result;
  finally
    SetDecimalSeparator(DS);
  end;
end;

function StrToI(var S: string; var I: Integer): LongBool;
begin
  Val(S, I, Integer(Result));
  Result := not Result;
end;

function IsSizeUnitsMM(const S: string; var W, H: Double): Boolean;
var
  vPos, I: Integer;
  vS: string;
begin
  Result := False;
  vPos := Pos(' x ', S);
  if vPos > 0 then
  begin
    I := vPos + 3;
    repeat
      vS := S[vPos - 1] + vS;
      Dec(vPos);
    until S[vPos - 1] = ' ';
    StrToD(vS, W);
    vS := '';
    repeat
      vS := vS + S[I];
      Inc(I);
    until S[I] = ' ';
    StrToD(vS, H);
    Inc(I);
    if S[I] <> '"' then
      Result := True;
  end;
end;

procedure GetExportParam(AFrame: TfrmCustomExport; AExportParams: PsgExportParams);
var
  S: string;
begin
  if Assigned(AFrame) and Assigned(AExportParams) then
  begin
    AExportParams^.PageWidth := AFrame.PageWidth;
    AExportParams^.PageHeight := AFrame.PageHeight;
    S := AFrame.edtMargin.Text;
    StrToD(S, AExportParams^.Margin);
    AExportParams^.DrawMode := AFrame.rgPalette.ItemIndex;
    AExportParams^.BackgroundColor := AFrame.ccbBackgroundColor.Selected;
    AExportParams^.DefaultColor := AFrame.ccbDefaultColor.Selected;
    AExportParams^.SaveLineWeight := AFrame.cbSaveLinewight.Checked;
    S := AFrame.edtWidthNull.Text;
    StrToD(S, AExportParams^.NullWidth);
    S := AFrame.edtLineWeightScale.Text;
    StrToD(S, AExportParams^.LineWeightScale);
    AExportParams^.UseHighQuality := AFrame.cbHighQuality.Checked;
//    AExportParams^.Quality := 1 / AFrame.trbarQuality.Position;
    AExportParams^.LayoutExportMode := AFrame.GetLayoutExportMode;
    AExportParams^.LayoutNameExportMode := AFrame.edtLayoutName.Text;
    AExportParams^.Title := AFrame.edtTitle.Text;
    AExportParams^.Author := AFrame.edtAuthor.Text;
    AExportParams^.Subjct := AFrame.edtSubject.Text;
    AExportParams^.Keywords := AFrame.edtKeywords.Text;
    AExportParams^.UseExtentsFromPlotSettings := AFrame.rbPlotSetting.Checked;
  end;
end;

procedure LoadExportParams(AFrame: TfrmCustomExport; AExportParams: PsgExportParams);
begin
  if Assigned(AFrame) and Assigned(AExportParams) then
  begin
    AFrame.PageWidth := AExportParams^.PageWidth;
    AFrame.PageHeight := AExportParams^.PageHeight;
    AFrame.edtMargin.Text := DtoS(AExportParams^.Margin);
    AFrame.rgPalette.ItemIndex := AExportParams^.DrawMode;
    AFrame.ccbBackgroundColor.Selected := AExportParams^.BackgroundColor;
    AFrame.ccbDefaultColor.Selected := AExportParams^.DefaultColor;
    AFrame.cbSaveLinewight.Checked := AExportParams^.SaveLineWeight;
    AFrame.edtWidthNull.Text := DtoS(AExportParams^.NullWidth);
    AFrame.edtLineWeightScale.Text := DtoS(AExportParams^.LineWeightScale);
    AFrame.cbHighQuality.Checked := AExportParams^.UseHighQuality;
//    AFrame.trbarQuality.Position := Round(1 / AExportParams^.Quality);
    AFrame.SetLayoutExportMode(AExportParams^.LayoutExportMode);
    AFrame.edtLayoutName.Text := AExportParams^.LayoutNameExportMode;
    AFrame.edtTitle.Text := AExportParams^.Title;
    AFrame.edtAuthor.Text := AExportParams^.Author;
    AFrame.edtSubject.Text := AExportParams^.Subjct;
    AFrame.edtKeywords.Text := AExportParams^.Keywords;
  end;
end;

{ TfrmCustomExport }

procedure SetControlsColors(AParent: TWinControl);
const
  cnstColors: array[Boolean] of TColor = (clBtnFace, clWindow);
var
  I: Integer;
begin
  for I := 0 to AParent.ControlCount - 1 do
    if AParent.Controls[I] is TWinControl then
      TWinControlAccess(AParent.Controls[I]).Color := cnstColors[AParent.Controls[I].Enabled];
end;

procedure TfrmCustomExport.ccbColorGetColors(Sender: TCustomColorBox;
  Items: TStrings);
{$IFDEF SGMULLANG}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF SGMULLANG}
  if Assigned(FMLText) then
    for I := 0 to Items.Count - 1 do
      Items[I] := FMLText.GetFromIni('', Items[I]);
{$ENDIF}
end;

procedure TfrmCustomExport.CMChanged(var Message: TMessage);
begin
  inherited;
  if ComponentState * [csLoading..csReading, csDesigning] = [] then
  begin
    GetSize(FPageWidth, FPageHeight);
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

constructor TfrmCustomExport.Create(AOwner: TComponent);
var
  I: Integer;
  R: TRect;
begin
  inherited Create(AOwner);
  FStdWeights := TStringList.Create;
  for I := Low(sgDXFLineWeights) to High(sgDXFLineWeights) do
    FStdWeights.Add(IntToStr(I) + FStdWeights.NameValueSeparator +
      DtoS(sgDXFLineWeights[I] / 100, 2));
  cbSizes.ItemIndex := 9;
  ClearColorToLineWeight;
  FExportParams := DefExportParams;
  LoadExportParams(Self, @FExportParams);
  FInvalidFloating := LoadResString(@SInvalidFloat);
  R := pnlSizes.BoundsRect;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TPanel then Controls[I].BoundsRect := R;
  tvTabs.Items.Item[0].Data := Pointer(pnlSizes);
  tvTabs.Items.Item[1].Data := Pointer(pnlLayouts);
  tvTabs.Items.Item[2].Data := Pointer(pnlLineWidth);
  tvTabs.Items.Item[3].Data := Pointer(pnlProps);
  tvTabs.Selected := tvTabs.Items.Item[0];
  tvTabsClick(nil);
{$IFDEF SGMULLANG}
  CreateMultilangParams(FMLText, Self, MLTextAfterLoad);
{$ENDIF}
  if not FTranslated then
    Translate;
  AutoSize := True;
  AutoSize := False;
  Changed;
  cbSizes.ItemIndex := 9;
end;

destructor TfrmCustomExport.Destroy;
begin
  FreeAndNil(FStdWeights);
  inherited Destroy;
end;

function TfrmCustomExport.DoMsgDlg(const AText, ACaption: string;
  AType: Cardinal): Integer;
begin
  if ACaption = '' then
    Result := MessageBox(Handle, PChar(AText), nil, AType)
  else Result := MessageBox(Handle, PChar(AText), PChar(ACaption), AType);
end;

procedure TfrmCustomExport.edtExit(Sender: TObject);
var
  S: string;
  D: Double;
begin
  S := TWinControlAccess(Sender).Text;
  if not StrToD(S, D) then
  begin
    DoMsgDlg(Format(FInvalidFloating, [S]), '', MB_ICONERROR);
    TWinControl(Sender).SetFocus;
  end
  else
    if D < 0 then
    begin
      DoMsgDlg(SNonZero, '', MB_ICONWARNING);
      TWinControl(Sender).SetFocus;
    end
    else
      TWinControlAccess(Sender).Text := S;
end;

procedure TfrmCustomExport.edtKeyPress(Sender: TObject; var Key: Char);
begin
  {$IFDEF SGDEL_2009}
  if CharInSet(Key, [',', GetDecimalSeparator]) then
  {$ELSE}
  if Key in [',', DecimalSeparator] then
  {$ENDIF}
    Key := '.';
end;

function TfrmCustomExport.GetSize(var W, H: Double): Boolean;
var
  S: string;
begin
  Result := False;
  if rbStandard.Checked then
    Result := IsSizeUnitsMM(cbSizes.Text, W, H)
  else
  if rbCustom.Checked then
  begin
    Result := cbUnits.ItemIndex = 0;
    S := edtWidth.Text;
    StrToD(S, W);
    S := edtHeight.Text;
    StrToD(S, H);
  end
  else
  if rbPlotSetting.Checked then
  begin
    Result := True;
    W := cnstDefWidth;
    H := cnstDefHeight;
  end;
  if not Result then
  begin
    W := W * MMPerInch;
    H := H * MMPerInch;
  end;
end;

procedure TfrmCustomExport.LoadOptions(const AOptions: TCustomIniFile);
begin
  if Assigned(AOptions) then
  try
    edtWidth.Text := AOptions.ReadString(ClassName, edtWidth.Name, edtWidth.Text);
    edtHeight.Text := AOptions.ReadString(ClassName, edtHeight.Name, edtHeight.Text);
    cbUnits.ItemIndex := AOptions.ReadInteger(ClassName, cbUnits.Name, 0);
    rbStandard.Checked := AOptions.ReadBool(ClassName, rbStandard.Name, True);
    rbPlotSetting.Checked := AOptions.ReadBool(ClassName, rbPlotSetting.Name, False);
    rbCustom.Checked := (not rbStandard.Checked) and (not rbPlotSetting.Checked);
    cbSizes.ItemIndex := AOptions.ReadInteger(ClassName, cbSizes.Name, 9);
    edtMargin.Text := AOptions.ReadString(ClassName, edtMargin.Name, '5');
    rgPalette.ItemIndex := AOptions.ReadInteger(ClassName, rgPalette.Name, 0);
    ccbBackgroundColor.HandleNeeded;
    ccbBackgroundColor.Selected := AOptions.ReadInteger(ClassName, ccbBackgroundColor.Name, clWhite);
    ccbDefaultColor.HandleNeeded;
    ccbDefaultColor.Selected := AOptions.ReadInteger(ClassName, ccbDefaultColor.Name, clBlack);
    cbSaveLinewight.Checked := AOptions.ReadBool(ClassName, cbSaveLinewight.Name, True);
    edtLineWeightScale.Text := AOptions.ReadString(ClassName, edtLineWeightScale.Name, '1.0');
    edtWidthNull.Text := AOptions.ReadString(ClassName, edtWidthNull.Name, '0.3');
    cbHighQuality.Checked := AOptions.ReadBool(ClassName, cbHighQuality.Name, False);
    SetLayoutExportMode(AOptions.ReadInteger(ClassName, gbLayouts.Name, 3));
    edtLayoutName.Text := AOptions.ReadString(ClassName, edtLayoutName.Name, '');
    edtTitle.Text := AOptions.ReadString(ClassName, edtTitle.Name, edtTitle.Text);
    edtAuthor.Text := AOptions.ReadString(ClassName, edtAuthor.Name, edtAuthor.Text);
    edtSubject.Text := AOptions.ReadString(ClassName, edtSubject.Name, edtSubject.Text);
    edtKeywords.Text := AOptions.ReadString(ClassName, edtKeywords.Name, edtKeywords.Text);
    rbCustomClick(rbCustom);
  except
  end;
end;

{$IFDEF SGMULLANG}
procedure TfrmCustomExport.MLTextAfterLoad(Sender: TObject);
begin
  Translate;
end;
{$ENDIF}

procedure TfrmCustomExport.Translate;
var
  I: Integer;
begin
  TCustomColorBoxAccess(ccbBackgroundColor).PopulateList;
  TCustomColorBoxAccess(ccbDefaultColor).PopulateList;
  cbSizes.Items.BeginUpdate;
  try
    for I := 0 to cbSizes.Items.Count - 1 do
      cbSizes.Items[I] := Format(cbSizes.Items[I], [lblMarginUnits.Caption]);
  finally
    cbSizes.Items.EndUpdate;
  end;
{$IFDEF SGMULLANG}
  sColor := MLText.GetFromIni('', 'Color');
  sLineweight := MLText.GetFromIni('', 'Lineweight');
  vleColorToLineWeight.TitleCaptions[0] := sColor;
  vleColorToLineWeight.TitleCaptions[1] := sLineweight;
  SNonZero := MLText.GetFromIni('', SNonZero);
  FInvalidFloating := MLText.GetFromIni('', FInvalidFloating);
  tvTabs.Items.BeginUpdate;
  try
    for I := 0 to tvTabs.Items.Count - 1 do
      tvTabs.Items.Item[I].Text := MLText.GetFromIni('', tvTabs.Items.Item[I].Text);
  finally
    tvTabs.Items.EndUpdate;
  end;
{$ENDIF}
  FTranslated := True;
end;

procedure TfrmCustomExport.tvTabsClick(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(tvTabs.Selected) then
  begin
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TPanel then
        if tvTabs.Selected.Data = Controls[I] then
          Controls[I].Show
        else Controls[I].Hide;
  end
  else gbSizes.Show;
end;

procedure TfrmCustomExport.tvTabsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  tvTabsClick(nil);
end;

procedure TfrmCustomExport.rbCustomClick(Sender: TObject);
begin
  edtWidth.Enabled := rbCustom.Checked;
  edtHeight.Enabled := rbCustom.Checked;
  cbUnits.Enabled := rbCustom.Checked;
  cbSizes.Enabled := rbStandard.Checked;
  SetControlsColors(TWinControl(Sender).Parent);
end;

procedure TfrmCustomExport.rbLayoutClick(Sender: TObject);
begin
  edtLayoutName.Enabled := Sender = rbLayoutByName;
  if Assigned(Sender) then
    SetControlsColors(TWinControl(Sender).Parent);
end;

procedure TfrmCustomExport.SaveOptions(const AOptions: TCustomIniFile);
begin
  try
    if Assigned(AOptions) then
    try
      AOptions.WriteString(ClassName, edtWidth.Name, edtWidth.Text);
      AOptions.WriteString(ClassName, edtHeight.Name, edtHeight.Text);
      AOptions.WriteInteger(ClassName, cbUnits.Name, cbUnits.ItemIndex);
      AOptions.WriteBool(ClassName, rbStandard.Name, rbStandard.Checked);
      AOptions.WriteBool(ClassName, rbPlotSetting.Name, rbPlotSetting.Checked);
      AOptions.WriteInteger(ClassName, cbSizes.Name, cbSizes.ItemIndex);
      AOptions.WriteString(ClassName, edtMargin.Name, edtMargin.Text);
      AOptions.WriteInteger(ClassName, rgPalette.Name, rgPalette.ItemIndex);
      AOptions.WriteInteger(ClassName, ccbBackgroundColor.Name, ccbBackgroundColor.Selected);
      AOptions.WriteInteger(ClassName, ccbDefaultColor.Name, ccbDefaultColor.Selected);
      AOptions.WriteBool(ClassName, cbSaveLinewight.Name, cbSaveLinewight.Checked);
      if edtLineWeightScale.Enabled then
        AOptions.WriteString(ClassName, edtLineWeightScale.Name, edtLineWeightScale.Text);
      AOptions.WriteString(ClassName, edtWidthNull.Name, edtWidthNull.Text);
      AOptions.WriteBool(ClassName, cbHighQuality.Name, cbHighQuality.Checked);
      AOptions.WriteInteger(ClassName, gbLayouts.Name, GetLayoutExportMode);
      AOptions.WriteString(ClassName, edtLayoutName.Name, edtLayoutName.Text);
      AOptions.WriteString(ClassName, edtTitle.Name, edtTitle.Text);
      AOptions.WriteString(ClassName, edtAuthor.Name, edtAuthor.Text);
      AOptions.WriteString(ClassName, edtSubject.Name, edtSubject.Text);
      AOptions.WriteString(ClassName, edtKeywords.Name, edtKeywords.Text);
    finally
      AOptions.UpdateFile;
    end;
  except
  end;
end;

function TfrmCustomExport.GetLayoutExportMode: Integer;
var
  J: Integer;
begin
  Result := 0;
  J := 0;
  while J < gbLayouts.ControlCount do
  begin
    if gbLayouts.Controls[J] is TRadioButton then
      if TRadioButton(gbLayouts.Controls[J]).Checked then
        J := gbLayouts.ControlCount
      else Inc(Result);
    Inc(J);
  end;
end;

procedure TfrmCustomExport.SetExportParams(const Value: TsgExportParams);
begin
  FExportParams := Value;
  LoadExportParams(Self, @FExportParams);
end;

procedure TfrmCustomExport.SetLayoutExportMode(const Value: Integer);
var
  vExpLayoutButton: TRadioButton;
begin
  vExpLayoutButton := nil;
  case Value of
    0: vExpLayoutButton := rbModel;
    1: vExpLayoutButton := rbAllLayouts;
    2: vExpLayoutButton := rbLayoutByName;
    3: vExpLayoutButton := rbAllPaperSpaces;
    4: vExpLayoutButton := rbCurrentLayout;
  end;
  if Assigned(vExpLayoutButton) then
  begin
    vExpLayoutButton.Checked := True;
    rbLayoutClick(vExpLayoutButton);
  end;
end;

procedure TfrmCustomExport.SetPageHeight(const Value: Double);
begin
  FPageHeight := Value;
  edtHeight.Text := DtoS(FPageHeight);
end;

procedure TfrmCustomExport.SetPageWidth(const Value: Double);
begin
  FPageWidth := Value;
  edtWidth.Text := DtoS(FPageWidth);
end;

procedure TfrmCustomExport.UpDownEditChange(AEdit: TEdit; AUpDown: TUpDown;
  Direction: TUpDownDirection; ADelta: Double = 0.3);
var
  S: string;
  Value: Double;
  vDelta: Double;
begin
  S := AEdit.Text;
  StrToD(S, Value);
  vDelta := ADelta;
  case Direction of
    updUp: Value := Value + vDelta;
    updDown: Value := Value - vDelta;
  end;
  if AUpDown.Min > Value then
    Value := AUpDown.Min;
  if AUpDown.Max < Value then
    Value := AUpDown.Max;
  AEdit.Text := DtoS(Value);
  if Assigned(AEdit.OnExit) then
    AEdit.OnExit(AEdit);
end;

procedure TfrmCustomExport.vleColorToLineWeightDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
//var
  //Brush: TBrush;
  //Pen: TPen;
begin
  if (ACol = 0) and (ARow >= 1) then
  begin
    //Brush := TBrush.Create;
    //Pen := TPen.Create;
    //SaveCanvasInstruments(vleColorToLineWeight.Canvas, Brush, Pen);
    try
      vleColorToLineWeight.Canvas.Pen.Mode := pmCopy;
      vleColorToLineWeight.Canvas.Pen.Style := psSolid;
      vleColorToLineWeight.Canvas.Pen.Color := clBlack;
      vleColorToLineWeight.Canvas.Brush.Bitmap := nil;
      vleColorToLineWeight.Canvas.Brush.Style := bsSolid;
      vleColorToLineWeight.Canvas.Brush.Color := arrDXFtoRGBColors[ARow];
      vleColorToLineWeight.Canvas.Rectangle(Rect.Left +
        vleColorToLineWeight.Canvas.TextWidth('999') + 8, Rect.Top + 1,
        Rect.Right - 4, Rect.Bottom);
    finally
      //RestoreCanvasInstruments(vleColorToLineWeight.Canvas, Brush, Pen);
      //FreeAndNil(Brush);
      //FreeAndNil(Pen);
    end;
  end;
end;

procedure TfrmCustomExport.udWidthNullChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  if Direction = updNone then
    if NewValue < udWidthNull.Min then
      Direction := updDown
    else
      if NewValue > udWidthNull.Max then
        Direction := updUp;
  UpDownEditChange(edtWidthNull, udWidthNull, Direction);
end;

procedure TfrmCustomExport.ClearColorToLineWeight;
var
  I, J: Integer;
begin
  vleColorToLineWeight.Strings.BeginUpdate;
  try
    vleColorToLineWeight.Strings.Clear;
    for I := 1 to 255 do
    begin
      J := vleColorToLineWeight.Strings.Add(IntToStr(I) +
        vleColorToLineWeight.Strings.NameValueSeparator + DtoS(0, 2));
      vleColorToLineWeight.ItemProps[J].KeyDesc := IntToStr(I);
      vleColorToLineWeight.ItemProps[J].EditStyle := esPickList;
      vleColorToLineWeight.ItemProps[J].PickList := FStdWeights;
    end;
  finally
    vleColorToLineWeight.Strings.EndUpdate;
  end;
end;

end.
