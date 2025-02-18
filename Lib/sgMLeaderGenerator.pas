unit sgMLeaderGenerator;

interface

uses
  SysUtils, sgConsts, sgFunction, DXFConv;

type
  TsgMLeaderGenerator = class
  private
    FMLeader: TsgCADMultiLeader;
    FConverter: TsgDXFConverter;
    FConsistentStyle: TsgCADMLeaderStyle;
    FActiveContextData: TsgMLeaderContextData;
    FContent: TsgDXFEntity;
    FLabels: TsgMLeaderLabels;
    FMText: TsgDXFMText;
    procedure DoCreateLeaders(AMLeader: TsgMLeader);
    function CreateLeader(AMLLine: TsgMLeaderLine; AMLeader: TsgMLeader): TsgDXFLeader;
    function CreateMText: TsgDXFMText;
    function CreateTextBorder(ATextObject: TsgDXFEntity): TsgDXFEntity;
    function CreateContent: TsgDXFInsert;
    function DoAdd(AEntity: TsgDXFEntity; ALoad: Boolean = True): TsgDXFEntity;
    procedure DoConnect;
  protected
    function GetArrowScale: Extended; virtual;
    function BeginGenerate: TsgDXFInsert;
    procedure EndGenerate(AInsert: TsgDXFInsert);
  public
    constructor Create(AMLeader: TsgCADMultiLeader; AConverter: TsgDXFConverter); virtual;
    destructor Destroy; override;
    procedure Generate; virtual;
  end;

implementation

type
  TsgCADMultiLeaderAccess = class(TsgCADMultiLeader);
  TsgCADMLeaderStyleAccess = class(TsgCADMLeaderStyle);
  TsgCADMLeaderAnnotContextAccess = class(TsgCADMLeaderAnnotContext);
  TsgMLeaderContextDataAccess = class(TsgMLeaderContextData);
  TsgMLeaderAccess = class(TsgMLeader);
  TsgMLeaderLineAccess = class(TsgMLeaderLine);
  TsgDXFBlockRecordAccess = class(TsgDXFBlockRecord);
  TsgDXFLeaderAccess = class(TsgDXFLeader);
  TsgDXFInsertAccess = class(TsgDXFInsert);
  TsgDXFTextAccess = class(TsgDXFText);

{ TsgMLeaderGenerator }

function TsgMLeaderGenerator.BeginGenerate: TsgDXFInsert;
begin
  FContent := TsgCADMultiLeaderAccess(FMLeader).RenderObject;
  Result := nil;
  if FContent.EntType = ceInsert then
  begin
    Result := TsgDXFInsert(FContent);
    FContent := Result.Block;
  end;
  FContent.Clear;
end;

constructor TsgMLeaderGenerator.Create(AMLeader: TsgCADMultiLeader;
  AConverter: TsgDXFConverter);
begin
  FMLeader := AMLeader;
  FConverter := AConverter;
  FConsistentStyle := TsgCADMultiLeaderAccess(FMLeader).GetConsistentStyle;
  FActiveContextData := TsgCADMultiLeaderAccess(FMLeader).Context.Item;
  FLabels := TsgCADMultiLeaderAccess(FMLeader).Labels;
end;

function TsgMLeaderGenerator.CreateContent: TsgDXFInsert;
var
  I: Integer;
  vAttrib: TsgDXFAttrib;
begin
  Result := TsgDXFInsert.Create;
  Result.BlockRecord := FActiveContextData.BlockRecord;
  Result.Point := FActiveContextData.BlkLocation;
  Result.Extrusion := FActiveContextData.BlkNormal;
  Result.Angle := FActiveContextData.BlkRotation;
  Result.Scale := PtXScalar(FActiveContextData.BlkScale, 1);
  TsgDXFInsertAccess(Result).CalculateMatrix;
  TsgDXFInsertAccess(Result).DelayedLoadAttribs := True;
  for I := Low(FLabels) to High(FLabels) do
  begin
    vAttrib := TsgDXFAttrib.Create;
    vAttrib.AssignEntity(FLabels[I].Attdef);
    vAttrib.Value := FLabels[I].Text;
    vAttrib.Point := FPointXMat(FLabels[I].Attdef.Point, Result.GetMatrix);
    if TsgDXFTextAccess(FLabels[I].Attdef).HasSecond then
      vAttrib.Point1 := FPointXMat(FLabels[I].Attdef.Point1, Result.GetMatrix);
    Result.AddEntity(vAttrib);
  end;
  Result.ColorCAD := FActiveContextData.BlockColor;
  Result.Layer := FMLeader.Layer;
end;

function TsgMLeaderGenerator.CreateLeader(AMLLine: TsgMLeaderLine;
  AMLeader: TsgMLeader): TsgDXFLeader;
begin
  Result := TsgDXFLeaderAccess(TsgDXFLeader.Create);
  Result.DimStyle := FConverter.DimensionStyleByName(sStandardName);
  Result.ArrowSize := FConsistentStyle.ArrowHeadSize;
  Result.ArrowScale := GetArrowScale;
  Result.IsSpline := FConsistentStyle.LeaderLineType = 2;
  TsgDXFLeaderAccess(Result).FlagsExtented :=
    TsgDXFLeaderAccess(Result).FlagsExtented or Ord(FConsistentStyle.LeaderLineType = 1);
  Result.Visibility := FConsistentStyle.LeaderLineType <> 0;
  Result.Controls.AppendDynArray(AMLLine.Points);
  Result.Controls.Add(AMLeader.ConnectionPoint);
  if FConsistentStyle.LandingEnabled then
    Result.Controls.Add(AddFPoint(AMLeader.ConnectionPoint,
      PtXScalar(AMLeader.Direction, AMLeader.LandingDistance)));
  Result.Fit.Assign(Result.Controls);

  if Assigned(FConsistentStyle.Arrow) then
    Result.DIMLDRBLK := TsgDXFBlockRecordAccess(FConsistentStyle.Arrow).Block
  else
    Result.ArrowType := Ord(datClosedfilled);

  Result.ColorCAD := FConsistentStyle.ColorCAD;
  Result.LineType := FConsistentStyle.LineType;
  Result.Layer := FMLeader.Layer;
end;

function TsgMLeaderGenerator.CreateMText: TsgDXFMText;
begin
  Result := TsgDXFMText.Create;
  Result.Style := FActiveContextData.TextStyle;
  Result.Height := FActiveContextData.TextHeight;
  Result.Text := FActiveContextData.TextLabel;
  case FActiveContextData.TextAlignType of
    0: Result.Align := 1;
    1: Result.Align := 2;
    2: Result.Align := 3;
  end;
  Result.RectHeight := FActiveContextData.BoundaryHeight;
  Result.RectWidth := FActiveContextData.BoundaryWidth;
  Result.LineSpacingFactor := FActiveContextData.LineSpacingFactor;
  Result.LineSpacingStyle := FActiveContextData.LineSpacingStyle;
  Result.BackgroundFlags := (Ord(FActiveContextData.IsBackgroundEnabled) shl 1) or
    Ord(FActiveContextData.IsBackgroundMaskFillOn);
  Result.BackgroundTransparency := FActiveContextData.BackgroundTransparency;
  Result.BackgroundColor := FActiveContextData.BackgroundColor;
  Result.BackgroundScaleFactor := FActiveContextData.BackgroundScaleFactor;
  Result.Point := FActiveContextData.TextLocation;
  Result.Point1 := FActiveContextData.TextDirection;
  Result.Angle := FActiveContextData.TextRotation;
  Result.Extrusion := FActiveContextData.TextNormal;
  Result.ColorCAD := FActiveContextData.TextColor;
  Result.Layer := FMLeader.Layer;
end;

function TsgMLeaderGenerator.CreateTextBorder(
  ATextObject: TsgDXFEntity): TsgDXFEntity;
var
  vPoly: TsgDXFLWPolyline;

  function CreateVertex(const APoint: TFPoint): TsgDXFVertex; overload;
  begin
    Result := TsgDXFVertex.Create;
    Result.Point := APoint;
  end;

  function CreateVertex(const X, Y, Z: Double): TsgDXFVertex; overload;
  begin
    Result := CreateVertex(MakeFPoint(X, Y, Z));
  end;

begin
  vPoly := TsgDXFLWPolyline.Create;
  vPoly.AddEntity(CreateVertex(ATextObject.Box.Left, ATextObject.Box.Top, ATextObject.Box.Z1));
  vPoly.AddEntity(CreateVertex(ATextObject.Box.Right, ATextObject.Box.Top, ATextObject.Box.Z1));
  vPoly.AddEntity(CreateVertex(ATextObject.Box.Right, ATextObject.Box.Bottom, ATextObject.Box.Z1));
  vPoly.AddEntity(CreateVertex(ATextObject.Box.Left, ATextObject.Box.Bottom, ATextObject.Box.Z1));
  vPoly.Closed := True;
  vPoly.ColorCAD := FConsistentStyle.ColorCAD;
  vPoly.Layer := FMLeader.Layer;
  Result := vPoly;
end;

destructor TsgMLeaderGenerator.Destroy;
begin
  FConsistentStyle.Free;
  inherited Destroy;
end;

function TsgMLeaderGenerator.DoAdd(AEntity: TsgDXFEntity; ALoad: Boolean): TsgDXFEntity;
begin
  Result := AEntity;
  FContent.AddEntity(AEntity);
  if ALoad then
    FConverter.Loads(AEntity);
end;

procedure TsgMLeaderGenerator.DoConnect;
var
  vMLeader: TsgMLeaderAccess;
  vLine: TsgDXFLine;
  W: Double;

  procedure DoConnectLine;
  begin
    if IsBadRect(FMText.Block.Box) then
      W := 0
    else
      W := FMText.Block.Box.Right - FMText.Block.Box.Left;
    vLine := TsgDXFLine.Create;
    vLine.Point := AddFPoint(vMLeader.ConnectionPoint, PtXScalar(vMLeader.Direction, vMLeader.LandingDistance));
    vLine.Point1 := AddFPoint(vLine.Point, PtXScalar(vMLeader.Direction, FConsistentStyle.LandingGap * GetArrowScale + W));
    vLine.ColorCAD := FConsistentStyle.ColorCAD;
    vLine.LineType := FConsistentStyle.LineType;
    vLine.Layer := FMLeader.Layer;
    DoAdd(vLine);
  end;

begin
  if FActiveContextData.MLeadersCount > 0 then
  begin
    vMLeader := TsgMLeaderAccess(FActiveContextData.MLeaders[0]);
    if vMLeader.Direction.X < 0 then
    begin
      case FConsistentStyle.RightAttachment of
        0:;
        1:;
        2:;
        3:;
        4:;
        5:;
        6, 7, 8:
          begin
            DoConnectLine;
          end;
        9:;
        10:;
      end;
    end
    else
    begin
      case FActiveContextData.LeftAttachment of
        0:;
        1:;
        2:;
        3:;
        4:;
        5:;
        6:
          begin
            DoConnectLine;
          end;
        7:;
        8:;
        9:;
        10:;
      end;
    end;
  end;
end;

procedure TsgMLeaderGenerator.DoCreateLeaders(AMLeader: TsgMLeader);
var
  K: Integer;
begin
  for K := 0 to AMLeader.LinesCount - 1 do
    DoAdd(CreateLeader(AMLeader.Lines[K], AMLeader));
end;

procedure TsgMLeaderGenerator.EndGenerate(AInsert: TsgDXFInsert);
begin
  FConverter.Loads(FContent);
  if Assigned(AInsert) then
    FConverter.Loads(AInsert);
end;

procedure TsgMLeaderGenerator.Generate;
var
  J: Integer;
  vInsert: TsgDXFInsert;
begin
  vInsert := BeginGenerate;
  try
    for J := 0 to FActiveContextData.MLeadersCount - 1 do
      DoCreateLeaders(FActiveContextData.MLeaders[J]);
    if FActiveContextData.HasTextContents then
    begin
      FMText := CreateMText;
      DoAdd(FMText);
      if FConsistentStyle.TextFrameEnabled then
        DoAdd(CreateTextBorder(FMText))
      else
        DoConnect;
    end;
    if FActiveContextData.HasContentsBlock and Assigned(FActiveContextData.BlockRecord) then
      DoAdd(CreateContent);
  finally
    EndGenerate(vInsert);
  end;
end;

function TsgMLeaderGenerator.GetArrowScale: Extended;
begin
  if mlpScale in TsgCADMultiLeaderAccess(FMLeader).OverrideFlags then
    Result := FConsistentStyle.ScaleFactor
  else
    Result := FActiveContextData.OverallScale;
end;

end.
