unit MeasureParams;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFDEF MSWINDOWS}
Windows,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  sgProxyGraphics, sgFMXTypes, System.UITypes,
{$ELSE}
  Graphics,
{$ENDIF}
  SysUtils, Classes, Registry, IniFiles;

const
  sMeasure3DSection = '3DSection';

  sSelectNodeColor = 'SelectNodeColor';
  sMarkerHoverColor = 'MarkerHoverColor';
  sSelectMarkerColor = 'SelectMarkerColor';

  sEdgesColor = 'EdgesColor';
  sEdgesThickness = 'EdgesThickness';
  sEdgesHoverColor = 'EdgesHoverColor';

  sEdgesHoverThickness = 'EdgesHoverThickness';
  sSelectEdgesColor = 'SelectEdgesColor';
  sSelectEdgesThickness = 'SelectEdgesThickness';

  sSelectSurfaceColor = 'SelectSurfaceColor';
  sEdgesAreaHoverThickness = 'EdgesAreaHoverThickness';
  sSelectEdgesAreaColor = 'SelectEdgesAreaColor';

  sSelectEdgesAreaThickness = 'SelectEdgesAreaThickness';
  sDimensionColor = 'DimensionColor';
  sSelectDimensionColor = 'SelectDimensionColor';

  sDimensionThickness = 'DimensionThickness';
  sSelectDimensionThickness = 'SelectDimensionThickness';
  sTextColor = 'TextColor';

  sSelectTextColor = 'SelectTextColor';
  sTextIdent = 'TextIdent';
  sFrameColor = 'FrameColor';

  sSelectFrameColor = 'SelectFrameColor';
  sFrameThickness = 'FrameThickness';
  sSelectFrameThickness = 'SelectFrameThickness';

  sFrameBackgroundColor = 'FrameBackgroundColor';
  sSelectFrameBackgroundColor = 'SelectFrameBackgroundColor';
  sArrowSize = 'ArrowSize';
  sDimensionIdent = 'DimensionIdent';
  sTextheight = 'TextHeight';

  sMarkerSize = 'MarkerSize';
  sPointSize = 'PointSize';

  sDimensionLineColor = 'DimensionLineColor';
  sBoxDimensionLineColor = 'BoxDimensionLineColor';
  sBoxFrameColor = 'BoxFrameColor';

  sDimLine = 'DimLine';
  sDimEXE = 'DimExe';
  sDimGap = 'DimGap';
  sDimLwd = 'DimLwd';
  sDimLwt = 'DimLwt';
  sDimTofl = 'DimTofl';


type
  TsgMeasureParams = record
    SelectNodeColor: TColor;
    MarkerHoverColor: TColor;
    SelectMarkerColor: TColor;

    EdgesColor: TColor;
    EdgesThickness: Double;
    EdgesHoverColor: TColor;

    EdgesHoverThickness: Double;
    SelectEdgesColor: TColor;
    SelectEdgesThickness: Double;

    SelectSurfaceColor: TColor;
    EdgesAreaHoverThickness: Double;
    SelectEdgesAreaColor: TColor;

    SelectEdgesAreaThickness: Double;
    DimensionColor: TColor;
    SelectDimensionColor: TColor;

    DimensionThickness: Double;
    SelectDimensionThickness: Double;

    TextColor: TColor;
    SelectTextColor: TColor;

    TextIdent: Double;

    FrameColor: TColor;
    SelectFrameColor: TColor;

    FrameThickness: Double;
    SelectFrameThickness: Double;

    FrameBackgroundColor: TColor;
    SelectFrameBackgroundColor: TColor;

    ArrowSize: Double;
    DimensionIdent: Double;
    TextHeight: Double;//29

    MarkerSize: Double;
    PointSize:  Double;

    DimensionLineColor: TColor;

    BoxDimensionLineColor: TColor;
    BoxFrameColor: TColor;

    DimLine: Double;
    DimExe: Double;
    DimGap: Double;

    DimLwd: Double;
    DimLwt: Double;

    DimTofl: Boolean;

    DimensionScaleFactor: Double;
  end;

const
  cnstDef3DMeasureParam: TsgMeasureParams = (
    SelectNodeColor: $B48246;
    MarkerHoverColor: $62AEDF;
    SelectMarkerColor: clYellow;

    EdgesColor: $181818;
    EdgesThickness: 1;
    EdgesHoverColor: $62AEDF;

    EdgesHoverThickness: 1;
    SelectEdgesColor: $62AEDF;
    SelectEdgesThickness: 1;

    SelectSurfaceColor: $62AEDF;
    EdgesAreaHoverThickness: 1;
    SelectEdgesAreaColor: $db914f;

    SelectEdgesAreaThickness: 1;
    DimensionColor: $FFA454;
    SelectDimensionColor: cllime;

    DimensionThickness: 1;
    SelectDimensionThickness: 1;
    TextColor: $633c3c;

    SelectTextColor: clYellow;
    TextIdent: 1;
    FrameColor: $FFA454;

    SelectFrameColor: clLime;
    FrameThickness: 1;
    SelectFrameThickness: 1;

    FrameBackgroundColor: $d4d4d4;
    SelectFrameBackgroundColor: clLime;
    ArrowSize: 25;

    DimensionIdent: 1;

    TextHeight: 13;

    MarkerSize: 6;
    PointSize: 2;

    DimensionLineColor: $FFA454;
    BoxDimensionLineColor: $00B900;
    BoxFrameColor: $00B900;

    DimLine: 30;
    DimExe: 15;//DimLine / 2
    DimGap: 5;

    DimLwd: 2;
    DimLwt: 1;

    DimTofl: False;

    DimensionScaleFactor: 1;
    );


function LoadParamsFromSection(AIniFile: TCustomIniFile; ASectionName: string): TsgMeasureParams;

var
  Measure3DParams: TsgMeasureParams = ();

implementation

function LoadParamsFromSection(AIniFile: TCustomIniFile; ASectionName: string): TsgMeasureParams;
begin
  Result := cnstDef3DMeasureParam;

  Result.SelectNodeColor := AIniFile.ReadInteger(ASectionName, sSelectNodeColor,
    cnstDef3DMeasureParam.SelectNodeColor);

  Result.MarkerHoverColor := AIniFile.ReadInteger(ASectionName, sMarkerHoverColor,
    cnstDef3DMeasureParam.MarkerHoverColor);

  Result.SelectMarkerColor := AIniFile.ReadInteger(ASectionName, sSelectMarkerColor,
    cnstDef3DMeasureParam.SelectMarkerColor);

  Result.EdgesColor := AIniFile.ReadInteger(ASectionName, sEdgesColor,
    cnstDef3DMeasureParam.EdgesColor);

  Result.EdgesThickness := AIniFile.ReadFloat(ASectionName, sEdgesThickness,
    cnstDef3DMeasureParam.EdgesThickness);

  Result.EdgesHoverColor := AIniFile.ReadInteger(ASectionName, sEdgesHoverColor,
    cnstDef3DMeasureParam.EdgesHoverColor);

  Result.EdgesHoverThickness := AIniFile.ReadFloat(ASectionName, sEdgesHoverThickness,
    cnstDef3DMeasureParam.EdgesHoverThickness);

  Result.SelectEdgesColor := AIniFile.ReadInteger(ASectionName, sSelectEdgesColor,
    cnstDef3DMeasureParam.SelectEdgesColor);

  Result.SelectEdgesThickness := AIniFile.ReadFloat(ASectionName, sSelectEdgesThickness,
    cnstDef3DMeasureParam.SelectEdgesThickness);

  Result.SelectSurfaceColor := AIniFile.ReadInteger(ASectionName, sSelectSurfaceColor,
    cnstDef3DMeasureParam.SelectSurfaceColor);

  Result.EdgesAreaHoverThickness := AIniFile.ReadFloat(ASectionName, sEdgesAreaHoverThickness,
    cnstDef3DMeasureParam.EdgesAreaHoverThickness);
  Result.SelectEdgesAreaColor := AIniFile.ReadInteger(ASectionName, sSelectEdgesAreaColor,
    cnstDef3DMeasureParam.SelectEdgesAreaColor);

  Result.SelectEdgesAreaThickness := AIniFile.ReadFloat(ASectionName, sSelectEdgesAreaThickness,
    cnstDef3DMeasureParam.SelectEdgesAreaThickness);

  Result.DimensionColor := AIniFile.ReadInteger(ASectionName, sDimensionColor,
    cnstDef3DMeasureParam.DimensionColor);

  Result.SelectDimensionColor := AIniFile.ReadInteger(ASectionName, sSelectDimensionColor,
    cnstDef3DMeasureParam.SelectDimensionColor);

  Result.DimensionThickness := AIniFile.ReadFloat(ASectionName, sDimensionThickness,
    cnstDef3DMeasureParam.DimensionThickness);
  Result.SelectDimensionThickness := AIniFile.ReadFloat(ASectionName, sSelectDimensionThickness,
    cnstDef3DMeasureParam.SelectDimensionThickness);

  Result.TextColor := AIniFile.ReadInteger(ASectionName, sTextColor,
    cnstDef3DMeasureParam.TextColor);

  Result.SelectTextColor := AIniFile.ReadInteger(ASectionName, sSelectTextColor,
    cnstDef3DMeasureParam.SelectTextColor);

  Result.TextIdent := AIniFile.ReadFloat(ASectionName, sTextIdent,
    cnstDef3DMeasureParam.TextIdent);

  Result.FrameColor := AIniFile.ReadInteger(ASectionName, sFrameColor,
    cnstDef3DMeasureParam.FrameColor);
  Result.SelectFrameColor := AIniFile.ReadInteger(ASectionName, sSelectFrameColor,
    cnstDef3DMeasureParam.SelectFrameColor);

  Result.FrameThickness := AIniFile.ReadFloat(ASectionName, sFrameThickness,
    cnstDef3DMeasureParam.FrameThickness);
  Result.SelectFrameThickness := AIniFile.ReadFloat(ASectionName, sSelectFrameThickness,
    cnstDef3DMeasureParam.SelectFrameThickness);

  Result.FrameBackgroundColor := AIniFile.ReadInteger(ASectionName, sFrameBackgroundColor,
    cnstDef3DMeasureParam.FrameBackgroundColor);
  Result.SelectFrameBackgroundColor := AIniFile.ReadInteger(ASectionName, sSelectFrameBackgroundColor,
    cnstDef3DMeasureParam.SelectFrameBackgroundColor);

  Result.ArrowSize := AIniFile.ReadFloat(ASectionName, sArrowSize,
    cnstDef3DMeasureParam.ArrowSize);
  Result.DimensionIdent := AIniFile.ReadFloat(ASectionName, sDimensionIdent,
    cnstDef3DMeasureParam.DimensionIdent);

  Result.TextHeight := AIniFile.ReadFloat(ASectionName, sTextheight,
    cnstDef3DMeasureParam.TextHeight);

  Result.MarkerSize := AIniFile.ReadFloat(ASectionName, sMarkerSize,
    cnstDef3DMeasureParam.MarkerSize);
  Result.PointSize := AIniFile.ReadFloat(ASectionName, sPointSize,
    cnstDef3DMeasureParam.PointSize);

  Result.DimensionLineColor := AIniFile.ReadInteger(ASectionName, sDimensionLineColor,
    cnstDef3DMeasureParam.DimensionLineColor);

  Result.BoxDimensionLineColor := AIniFile.ReadInteger(ASectionName, sBoxDimensionLineColor,
    cnstDef3DMeasureParam.BoxDimensionLineColor);
  Result.BoxFrameColor := AIniFile.ReadInteger(ASectionName, sBoxFrameColor,
    cnstDef3DMeasureParam.BoxFrameColor);


  Result.DimLine := AIniFile.ReadFloat(ASectionName, sDimLine,
    cnstDef3DMeasureParam.DimLine);
  Result.DimExe := AIniFile.ReadFloat(ASectionName, sDimEXE,
    cnstDef3DMeasureParam.DimExe);

  Result.DimGap := AIniFile.ReadFloat(ASectionName, sDimGap,
    cnstDef3DMeasureParam.DimGap);
  Result.DimLwd := AIniFile.ReadFloat(ASectionName, sDimLwd,
    cnstDef3DMeasureParam.DimLwd);
  Result.DimLwt := AIniFile.ReadFloat(ASectionName, sDimLwt,
    cnstDef3DMeasureParam.DimLwt);

  Result.DimTofl := AIniFile.ReadInteger(ASectionName, sDimTofl,
    Integer(cnstDef3DMeasureParam.DimTofl)) <> 0;
end;

end.
