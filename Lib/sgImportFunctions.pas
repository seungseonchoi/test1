{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{                TsgIteratorBase class                       }
{                                                            }
{      Copyright (c) 2002-2014 SoftGold software company     }
{                                                            }
{************************************************************}

unit sgImportFunctions;
{$INCLUDE SGDXF.inc}
interface

uses
  DXFConv, sgConsts, sgFunction, sgLists,
  {$IFDEF SG_FIREMONKEY}
  System.Generics.Collections
  {$ELSE}
  Classes
  {$ENDIF};

type
  TsgPDFVectorizationMode = (pdfvmNone, pdfvmAll, pdfvmAuto);

const
  cnstDefaultVectorizationMode = pdfvmAuto;

procedure AddVertexIfNotEqual(const APolyline: TsgDXFLWPolyline;
  const APoint: TFPoint);

procedure TransformToCorrectBoundary(const ABoundary: TsgObjectList;
  const AConverter: TsgDXFConverter);

function GetCorrectContour(APath: TFPointList): TFPointList;

implementation

procedure AddVertexIfNotEqual(const APolyline: TsgDXFLWPolyline; const APoint: TFPoint);
var
  vIsEqual: Boolean;
  vLastPt: TFPoint;
begin
  vIsEqual := False;
  if APolyline.Count > 0 then
  begin
    vLastPt := TsgDXFVertex(APolyline.Entities[APolyline.Count - 1]).Point;
    vIsEqual := IsEqualFPoints(vLastPt, APoint);
  end;
  if not vIsEqual then
    AddVertexInPolyline(APolyline, APoint);
end;

procedure TransformToCorrectBoundary(const ABoundary: TsgObjectList;
const AConverter: TsgDXFConverter);
var
  I,J,K, vIndex: Integer;
  vEntity: TObject;
  vLineEnt: TsgDXFLine absolute vEntity;
  vPolyEnt: TsgCADBasePolyline absolute vEntity;
  vLine: TsgDXFLine;
  vIsLines,vIsOther,vIsPolies: Boolean;
  vNewBoundary: TsgObjectList;
  vPoint1, vPoint2: TFPoint;
  vEntDel: TsgDXFEntity;
begin
  J := ABoundary.Count;
  vIsLines := False;
  vIsOther := False;
  vIsPolies := False;
  for I := 0 to ABoundary.Count - 1 do
  begin
    vEntity := ABoundary[I];
    if vEntity is TsgDXFLine then
    begin
      J := I;
      vIsLines := True;
    end
    else
    begin
      if vEntity is TsgDXFLWPolyline then
      begin
        vIsPolies := True;
        vPoint1 := TsgDXFVertex(TsgDXFLWPolyline(vEntity).Entities[0]).Point;
        vPoint2 := TsgDXFVertex(TsgDXFLWPolyline(vEntity).Entities[TsgDXFLWPolyline(vEntity).Count - 1]).Point;
        if IsEqualFPoints(vPoint1, vPoint2) then
        begin
          TsgDXFLWPolyline(vEntity).Closed := True;
          vIndex := TsgDXFLWPolyline(vEntity).Count - 1;
          vEntDel := TsgDXFLWPolyline(vEntity).Entities[vIndex];
          TsgDXFLWPolyline(vEntity).DeleteEntity(vIndex);
          vEntDel.Free;
          AConverter.Loads(TsgDXFLWPolyline(vEntity));
        end;
      end
      else
        vIsOther := True;
    end;
  end;
  if vIsPolies and (vIsLines or vIsOther) then
  begin
    vNewBoundary := TsgObjectList.Create;
    try
      for I := J to ABoundary.Count - 1 do
      begin
        vEntity := ABoundary[I];
        if vEntity is TsgCADBasePolyline then
        begin
          for K := 0 to vPolyEnt.PointCount - 2 do
          begin
            vLine := TsgDXFLine.Create;
            vNewBoundary.Add(vLine);
            vLine.AssignEntity(vPolyEnt);
            vLine.Point := vPolyEnt.PolyPoints[K];
            vLine.Point1 := vPolyEnt.PolyPoints[K + 1];
            AConverter.Loads(vLine);
          end;
          AConverter.RemoveEntity(TsgDXFEntity(vEntity), True);
          ABoundary[I] := nil;
        end
        else
        begin
          vNewBoundary.Add(vEntity);
          ABoundary[I] := nil;
        end;
      end;
      ABoundary.Clear;
      ABoundary.Assign(vNewBoundary);
    finally
      vNewBoundary.Free;
    end;
  end;
end;
////
procedure DeleteBadPointsFormPath(APath,AResult: TFPointList; AIsClearMergeSegs: Boolean);
var
  I,J: Integer;
  vPt1,vPt2,vPt3,vPt4: TFPoint;
  vTmpList: TFPointList;
  vIsRemoves,vIsNoAdd: Boolean;
begin
  vTmpList := TFPointList.Create;
  try
    vTmpList.Assign(APath);
    AResult.Clear;
    repeat
      vIsRemoves := False;
      I := 1;
      vPt1 := vTmpList[0];
      AResult.Add(vPt1);
      while I < vTmpList.Count do
      begin
        vPt2 := vTmpList[I];
        while IsEqualFPoints(vPt1, vPt2) and (I < vTmpList.Count) and (vTmpList.Count > 2) do
        begin
          Inc(I);
          vPt2 := vTmpList[I];
          vIsRemoves := True;
        end;
        vIsNoAdd := False;
        if AIsClearMergeSegs then
          for J := AResult.Count - 1 downto 1 do
          begin
            vPt3 := AResult[J - 1];
            vPt4 := AResult[J];
            if IsPointOnSegmentPts(vPt3, vPt4, vPt2) and not(IsEqualFPoints(vPt3, vPt2) and (J = 1)) then
            begin
              vIsNoAdd := True;
              Break;
            end;
          end;
        if vIsNoAdd then
        begin
          Inc(I);
          vIsRemoves := True;
        end
        else
        begin
          J := I + 1;
          while J < vTmpList.Count do
          begin
            vPt3 := vTmpList[J];
            if IsEqualFPoints(vPt2, vPt3) then
            begin
              vIsRemoves := True;
              Inc(J);
            end
            else
              if IsPointOnLinePts(vPt1, vPt2, vPt3) then
              begin
                vIsRemoves := True;
                vPt2 := vPt3;
                Inc(J);
              end
              else
                Break;
          end;
          AResult.Add(vPt2);
          vPt1 := vPt2;
          I := J;
        end;
      end;
      if vIsRemoves then
      begin
        vTmpList.Assign(AResult);
        AResult.Clear;
      end;
    until not vIsRemoves;
  finally
    vTmpList.Free;
  end;
end;

function SplitPathToNotSelfIntersect(ASegments: TList; APath: TFPointList): Boolean;
var
  I,J,vCnt: Integer;
  vPt1,vPt2,vPt3,vPt4,vPtCross: TFPoint;
  vTmpList: TFPointList;

  procedure SplitInternal(AList: TFPointList; AStart,AEnd: Integer;
    ACross: TFPoint; AIsAddFirst,AIsAddLast: Boolean);
  var
    K: Integer;
  begin
    if AStart = AEnd then
      Exit;
    if AStart < AEnd then
    begin
      for K := AStart to AEnd do
        AList.Add(APath[K]);
    end
    else
    begin
      for K := AStart to APath.Count - 1 do
        AList.Add(APath[K]);
      for K := 0 to AEnd do
        AList.Add(APath[K]);
    end;
    if AIsAddFirst and not IsEqualFPoints(ACross, AList.First) then
      AList.Insert(0, ACross);
    if AIsAddLast and not IsEqualFPoints(ACross, AList.Last) then
      AList.Add(ACross);
    SplitPathToNotSelfIntersect(ASegments, AList);
    AList.Clear;
  end;

begin
  Result := False;
  vCnt := APath.Count - 1;
  if APath.Count > 3 then
    for I := 0 to vCnt do
    begin
      vPt1 := APath[I];
      vPt2 := APath[(I + 1) mod APath.Count];
      for J := I + 2 to vCnt do
      begin
        vPt3 := APath[J];
        vPt4 := APath[(J + 1) mod APath.Count];
        if IsCrossSegmentsPts(vPt1, vPt2, vPt3, vPt4, @vPtCross) then
        begin
          if IsEqualFPoints(vPt2, vPtCross) and IsEqualFPoints(vPt3, vPtCross) then
            Continue;
          if IsEqualFPoints(vPt1, vPtCross) and IsEqualFPoints(vPt4, vPtCross) then
            Continue;
          Result := True;
          vTmpList := TFPointList.Create;
          try
            if I > 0 then
              SplitInternal(vTmpList, 0, I, vPtCross, False, True)
            else
              SplitInternal(vTmpList, vCnt, 0, vPtCross, False, True);
            SplitInternal(vTmpList, I + 1, J, vPtCross, True, True);
            if J < vCnt then
              SplitInternal(vTmpList, J + 1, vCnt, vPtCross, True, False)
            else
              SplitInternal(vTmpList, vCnt, J mod APath.Count, vPtCross, True, False);
          finally
            vTmpList.Free;
          end;
          Exit;
        end;
      end;
    end;
  vTmpList := TFPointList.Create;
  vTmpList.Assign(APath);
  ASegments.Add(vTmpList);
end;

procedure MakeNotSelfIntersectingPath(ASegments: TList; ANewPath: TFPointList;
  AExcludeList: TsgIntegerList = nil; AStartIndex: Integer = 0;
  AIsLast: Boolean = True; AConnectPt: PFPoint = nil);
var
  I: Integer;
  vConnectPt,vNewConnectPt: TFPoint;
  vTmpPath: TFPointList;
  vIsExListFree: Boolean;

  function GetConnectPt(AList: TFPointList; AIsLastPt: Boolean): TFPoint;
  begin
    if AIsLastPt then
      Result := AList.Last
    else
      Result := AList.First;
  end;

begin
  if not Assigned(AExcludeList) then
  begin
    AExcludeList := TsgIntegerList.Create;
    AExcludeList.Add(AStartIndex);
    vIsExListFree := True;
  end
  else
    vIsExListFree := False;
  if Assigned(AConnectPt) then
    vConnectPt := AConnectPt^
  else
  begin
    vConnectPt := GetConnectPt(ASegments[AStartIndex], AIsLast);
    vTmpPath := ASegments[AStartIndex];
    ANewPath.AppendArray(vTmpPath);
  end;
  vTmpPath := TFPointList.Create;
  try
    for I := AStartIndex + 1 to ASegments.Count - 1 do
    begin
      if (AExcludeList.IndexOf(I) < 0) and IsEqualFPoints(vConnectPt, GetConnectPt(ASegments[I], AIsLast)) then
      begin
        vTmpPath.Clear;
        vTmpPath.Assign(ASegments[I]);
        if AIsLast then
          vTmpPath.Flip;
        vNewConnectPt := vTmpPath.Last;
        vTmpPath.First := MiddleFPoint(vTmpPath.First, vTmpPath[1]);
        vTmpPath.Last := MiddleFPoint(vTmpPath.Last, vTmpPath[vTmpPath.Count - 2]);
        ANewPath.AppendArray(vTmpPath);
        AExcludeList.Add(I);
        MakeNotSelfIntersectingPath(ASegments, ANewPath, AExcludeList,
          AStartIndex + 1, not AIsLast, @vNewConnectPt);
      end;
    end;
  finally
    if vIsExListFree then
      AExcludeList.Free;
    vTmpPath.Free;
  end;
end;

procedure TransformPathToNotSelfIntersect(APath: TFPointList; AIsClosed: Boolean);
var
  vSegments: TList;
begin
  vSegments := TList.Create;
  try
    if not IsEqualFPoints(APath.First, APath.Last) and AIsClosed then
      APath.Add(APath.First);
    while SplitPathToNotSelfIntersect(vSegments, APath) do
    begin
      APath.Clear;
      MakeNotSelfIntersectingPath(vSegments, APath);
      sgConsts.ClearList(vSegments);
    end;
  finally
    FreeList(vSegments);
  end;
end;

function GetCorrectContour(APath: TFPointList): TFPointList;
const
  cnstMaxCorrections = 20;
var
  vCorrections: Integer;
begin
  Result := TFPointList.Create;
  try
    DeleteBadPointsFormPath(APath, Result, False);
    vCorrections := 0;
    repeat
      if vCorrections >= cnstMaxCorrections then
      begin
        Result.Clear;
        Break;
      end
      else
      begin
        if not IsEqualFPoints(Result.First, Result.Last) then
          Result.Add(Result.First);
        TransformPathToNotSelfIntersect(Result, True);
        if Result.Count > 1 then
          DeleteBadPointsFormPath(Result, Result, True);
        Inc(vCorrections);
      end;
    until (Result.Count = 0) or IsEqualFPoints(Result.First, Result.Last);
  except
    Result.Free;
    raise;
  end;
end;

end.
