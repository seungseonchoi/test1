{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                      XML properties                        }
{                                                            }
{     Copyright (c) 2002 - 2019 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}
unit Properties;
{$INCLUDE SGDXF.inc}

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

interface

uses
  {$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
  {$ENDIF}
  SysUtils, Classes, sgConsts, sgLists, sgFunction, sgXMLParser
{$IFDEF SG_PLUGINSHOST}
  , PluginsConstants
{$ENDIF}
{$IFDEF SGDEL_XE2}
  ,System.Types
{$ENDIF}
  ;

type
  TsgOperationMode = (opAdd, opAddIfNotExist);

  TsgObject = class;
  TsgObjectClass = class of TsgObject;

  TsgObjectPropertyEnum = (opObject, opSizeMode, opWidth, opHeight, opSaveRatio,
    opPixelFormat, opHorDPU, opVertDPU, opDPURatio, opSpecUnitInMM,
    opSpecUnitInInch, opImageIndex, opUsable, opTif1BitCompress,
    opTifColorCompress, opJpegQuality, opGifTransparent);

  TsgObject = class(TsgInterfacedObject{$IFNDEF HAS_OBJECT_CAST_FROM_INTERFACE}, IInstancePersist{$ENDIF})
  private
  protected
{$IFNDEF HAS_OBJECT_CAST_FROM_INTERFACE}
    { IInstancePersist }
    function GetInstance: TObject;
{$ENDIF}
    class function GetClassByXMLName(const AName: string): TsgObjectClass;
    function GetEnumType: TsgObjectPropertyEnum;// virtual;
  public
    constructor Create; virtual;
    procedure Assign(const AObj: TsgObject);
    class function GetPropertyName: string; virtual;
    function GetStrValue: string; virtual;
    class function GetXMLName: string;
    procedure RemoveProperty(AList: TsgObjectList; AProperty: TsgObjectClass);
    function SetStrValue(const S: string): Boolean; virtual;
    function IsLikeGroup: Boolean; virtual;
    function IsSameValue(AObject: TsgObject): Boolean; virtual; abstract;
    property EnumType: TsgObjectPropertyEnum read GetEnumType;
//    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;

  TsgListObjectClass = class of TsgListObject;

  IsgXMLObject = interface
    [cnstGuid_XMLObject]
    function GetNodeName: string;
    function ToNode(const AParentNode: TsgNode;
      const AParams: TsgXMLParams): TsgNode;
    procedure FromNode(const ANode: TsgNodeSample);

    procedure GetXMLIdsChangingHiding(const AChangeList, AHideList: TsgIntegerList;
      AProgID: TsgProgID);
  end;

  IsgXMLSubObjects = interface
    [cnstGuid_XMLSubObjects]
    procedure ToSubEntities(const AParentNode: TsgNode;
      const AParams: TsgXMLParams);
  end;

  IsgXMLSupportModeller = interface
    [cnstGuid_XMLSupportModeller]
    function GetIsNotFree: Boolean;
  end;

  IsgXMLSupportBuildKey = interface
    [cnstGuid_XMLSupportBuildKey]
    function IsUsedInKeyConstruction: Boolean;
  end;

  IsgVisibleObject = interface
    [cnstGuid_VisibleObject]
    function GetVisibleElement: Boolean;
    procedure SetVisibleElement(const AValue: Boolean);
    procedure GetMatrix(out AMatrix: TFMatrix);
    function GetMeasureKoef: Double;
    function GetArea: Double;
    function GetBox(const IsCache: Boolean = False): TFRect;
    function GetAlphaBlend: Integer;
    procedure SetAlphaBlend(const AValue: Integer);
    function GetCustomColor: Cardinal;
    procedure SetCustomColor(const AValue: Cardinal);
    //Update from the root and rebuild the entire tree.
    procedure NormalizeVisibilityTree;
  end;

  IsgVisibleObjectByPath = interface
    [cnstGuid_VisibleObjectByPath]
    procedure GetSetVisibleRepresentation(const AKey: UInt64; var AValue: Boolean;
      const AIsGet: Boolean = True);
    function GetArea(const AKey: UInt64): Double;
    function GetBox3D(const AKey: UInt64): TFRect;
    function GetAlphaBlend(const AKey: UInt64): Integer;
    procedure SetAlphaBlend(const AKey: UInt64; const AValue: Integer);
    function GetCustomColor(const AKey: UInt64): Cardinal;
    procedure SetCustomColor(const AKey: UInt64; const AValue: Cardinal);
    //Update from the root and rebuild the entire tree.
    procedure NormalizeVisibilityTree;
  end;

  TsgListObject = class(TsgObject, IsgXMLObject{$IFDEF SG_PLUGINSHOST}, IsgXMLPluginItem{$ENDIF})
  protected
    function AddToXMLParam(const AParams: TsgXMLParams; const ANode: TsgNodeSample): Boolean;
    function CanAddedToXMLParams: Boolean; virtual;
    function GetNodeName: string; virtual;
    function GetObjectType: Integer; virtual;
    procedure EntityLoaded; virtual;
    procedure DoErrorXMLNode(const ANodeErrors: TsgNode;
      const AChild: Boolean; const ANode: TsgNodeSample;
      const AId: TsgXMLId);
    function ToXMLNode(const ANode: TsgNode;
      const AParams: TsgXMLParams): Integer; virtual;
    function FromXMLNode(const AType: TsgXMLType; const ANode: TsgNodeSample;
      const AIsChild: Boolean; const AResult: IsgResultNode = nil): Integer; virtual;
    function FromXMLSubEntities(const ANode: TsgNodeSample;
      const AResult: IsgResultNode = nil): Integer; virtual;
{$IFDEF SG_PLUGINSHOST}
    function ExportToPluginsInternal(ANode: IsgNodeFrame): Integer; virtual;
    function SetPropertyFromPluginsInternal(ANode: IsgNodeFrame;
      const AType: TsgPlcType; const AValue: Variant): Integer;  virtual;
{$ENDIF}
    //Forced measure to store visibility in 3d.
    function GetFlags: Integer; virtual;
    function GetVisibility: Boolean; virtual;
    function CanShow: Boolean; virtual;
    function GetVisibleRepresentation(const AKey: UInt64): Boolean; virtual;
    procedure SetVisibleRepresentation(const AKey: UInt64; const AValue: Boolean); virtual;
    function GetAlphaBlendRepresentation(const AKey: UInt64): Integer; virtual;
    procedure SetAlphaBlendRepresentation(const AKey: UInt64; const AValue: Integer); virtual;
    function GetCustomColorRepresentation(const AKey: UInt64): Cardinal; virtual;
    procedure SetCustomColorRepresentation(const AKey: UInt64; const AValue: Cardinal); virtual;
  public
    procedure AddProperty(const AList: TsgObjectList;  const AProperty: TsgObject;
      const AMode: TsgOperationMode = opAddIfNotExist);
    procedure AddPropertyCopy(const AList: TsgObjectList; const AProperty: TsgObject);
    class function XMLDescription: string; virtual;
    function IsSameValue(AObject: TsgObject): Boolean; override;
    function ToXMLWithParams(const AParentNode: TsgNode;
      const AParams: TsgXMLParams): TsgNode; virtual;
    function ToXML(const AParentNode: TsgNode;
      const AMode: TsgXMLModes = cnstDefaultXMLMode): TsgNode;
//  IsgXMLObject
    function FromXML(const ANode: TsgNodeSample;
      const AResult: IsgResultNode = nil): Integer; virtual;
    function ToNode(const AParentNode: TsgNode;
      const AParams: TsgXMLParams): TsgNode;
    procedure FromNode(const AParentNode: TsgNodeSample);
    procedure GetXMLIdsChangingHiding(const AChangeList, AHideList: TsgIntegerList;
      AProgID: TsgProgID); virtual;
{$IFDEF SG_PLUGINSHOST}
    function ExportToPlugins(ANode: IsgNodeFrame): Integer;
    function ChangeFromPlugins(ANode: IsgNodeFrame; const APropName: string;
      const AValue: Variant): Integer;
{$ENDIF}
  end;


  TsgGlobalsgObjects = class(TsgObjectList)
  public
    procedure ClearFull;
  end;

  TsgGlobalsgTypes = class(TStringList)
  private
    function GetMetaClass(Index: Integer): TClass;
  public
    procedure AddClass(AClass: TsgObjectClass; const AInspIndexName: Integer = -1);
    function GetClass(const S: string): TsgObjectClass;
    function GetClassByName(const AName: string): TClass;
    property MetaClass[Index: Integer]: TClass read GetMetaClass;
  end;

  TsgObjectInfo = class
  public
    ObjClass: TClass;
    ObjType:  TsgObjectPropertyEnum;
  end;

  TsgGlobalsgObjectsInfo = class
  private
    FInfoByQuickFind: TsgObjectInfo;
    FListInfo: TsgObjectList;
  protected
    function CompareInfo(const A, B: Pointer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddInfo(const AClass: TClass; const AType: TsgObjectPropertyEnum);
    function GetInfo(const AClass: TClass): TsgObjectInfo;
  end;

var
  GlobalsgTypes: TsgGlobalsgTypes;

function GetXMLType(const AName: string): TsgXMLType;
function GetObjectXMLName(const AClassName: string): string;{$IFNDEF SG_USE_NOSOURCE_3DMODELER}{$IFDEF USE_INLINE} inline;{$ENDIF}{$ENDIF}
function GetObjectPropertyName(const AClassName: string): string;{$IFNDEF SG_USE_NOSOURCE_3DMODELER}{$IFDEF USE_INLINE} inline;{$ENDIF}{$ENDIF}
function GetObjectName(const AXMLName: string): string;
function GetObjectNameInsp(const AClassType: TClass): string;

procedure GetInterfaceForData(const AKey: UInt64; const AData: TObject;
  const AGuid: TGUID; out IElement);

procedure TranslateEntNamesIns(const AProc: TsgObjProcTranslate);

implementation

type
  TsgMetaObject = class
    ClassTyp: TClass;
    NameInsp: string;
    NameIndex: Integer;
  public
    constructor Create;
  end;

  TsgNodeSampleAccess = class(TsgNodeSample);

var
  GlobalsgObjects: TsgGlobalsgObjects;
  GlobalsgObjectsInfo: TsgGlobalsgObjectsInfo;
  GlobalListNamesOfClasses: TsgStringList = nil;
  GlobalListTypeOfClasses: TsgObjectCollection = nil;

procedure GetInterfaceForData(const AKey: UInt64; const AData: TObject;
  const AGuid: TGUID; out IElement);

var
  I: Integer;
  vHashItem: TsgHashItem;
  vColl: TsgCollection;
begin
 Pointer(IElement) := nil;
 if not Assigned(AData) then
    Exit;
  vColl := TsgCollection(AData);
  I := vColl.IndexOf(AKey);
  if I >= 0 then
  begin
    vHashItem := vColl[I];
    if Assigned(vHashItem.Data) then
      Supports(TObject(vHashItem.Data), AGuid , IElement)
  end;
end;

function IndexOfProperty(const AProperties: TsgObjectList; const AProperty: TClass): Integer;
begin
  Result := 0;
  while (Result < AProperties.Count) and (AProperties[Result].ClassType <> AProperty) do
    Inc(Result);
  if Result >= AProperties.Count then
    Result := -1;
end;

function HasPropeprty(const AProperties: TsgObjectList; const AProperty: TClass): Boolean;
begin
  Result := IndexOfProperty(AProperties, AProperty) > -1;
end;

function GetPropertyByClass(const AProperties: TsgObjectList; const AProperty: TClass): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOfProperty(AProperties, AProperty);
  if I > -1 then
    Result := AProperties[I];
end;

function GetXMLType(const AName: string): TsgXMLType;
begin
  Result.Id := GetXMLId(AName);
  if Result.Id = xmlUndefined then
  begin
    Result.ClassId := GlobalsgTypes.GetClassByName(AName);
    if Assigned(Result.ClassId) then
      Result.Id := xmlClassId;
  end
  else
    Result.ClassId := nil;
end;

function GetObjectPropertyName(const AClassName: string): string;{$IFNDEF SG_USE_NOSOURCE_3DMODELER}{$IFDEF USE_INLINE} inline;{$ENDIF}{$ENDIF}
var
  vLen: Integer;
  vPrefix: string;
begin
  Result := AClassName;
  vLen := Length(AClassName);
  if vLen > 0 then
  begin
    if Copy(Result, 1, 3) = 'Tsg' then
    begin
      vPrefix := Copy(Result, 4, 3);
      if (vPrefix = 'DXF') or (vPrefix = 'CAD') then
        Delete(Result, 1, 6)
      else
      begin
        if Copy(Result, 4, 2) = '2D' then
        begin
          Delete(Result, 1, 5);
          Result := 'h' + Result;
        end
        else
          Delete(Result, 1, 3);
      end;
    end;
  end
  else
    Result := cnstXMLEntity;
end;

function GetObjectName(const AXMLName: string): string;
const
  cnstXMLPrefixLen = Length(cnstXMLPrefix);
begin
  Result := AXMLName;
  if Length(Result) > 0 then
  begin
    if (Copy(Result, 1, cnstXMLPrefixLen) = cnstXMLPrefix) then
      Delete(Result, 1, cnstXMLPrefixLen);
  end;
end;

function GetObjectXMLName(const AClassName: string): string;{$IFNDEF SG_USE_NOSOURCE_3DMODELER}{$IFDEF USE_INLINE} inline;{$ENDIF}{$ENDIF}
begin
  Result := cnstXMLPrefix + GetObjectPropertyName(AClassName);
end;

function GetObjectNameInsp(const AClassType: TClass): string;
var
  I: Integer;
  vHash: UInt64;
  vMetaData: TsgMetaObject;
begin
  Result := '';
  if Assigned(GlobalListTypeOfClasses) and (GlobalListTypeOfClasses.Count > 0) then
  begin
    vHash := GetHashCodeByClass(AClassType);
    I := GlobalListTypeOfClasses.IndexOf(vHash);
    if I > -1 then
    begin
      vMetaData := TsgMetaObject(GlobalListTypeOfClasses.Items[I].Data);
      Result := vMetaData.NameInsp;
      if Length(Result) < 1 then
      begin
        I := GlobalListNamesOfClasses.IndexOfObject(vMetaData);
        if I > -1 then
          Result := GlobalListNamesOfClasses[I];
      end;
    end;
  end
  else
  begin
    for I := 0 to GlobalListNamesOfClasses.Count - 1 do
    begin
      vMetaData := TsgMetaObject(GlobalListNamesOfClasses.Objects[I]);
      if vMetaData.ClassTyp = AClassType then
      begin
        Result := vMetaData.NameInsp;
        if Length(Result) < 1 then
          Result := GlobalListNamesOfClasses[I];
        Break;
      end;
    end;
  end;
end;

procedure TranslateEntNamesIns(const AProc: TsgObjProcTranslate);
var
  I: Integer;
  vMetaData: TsgMetaObject;
begin
  if not Assigned(AProc) then
    Exit;
  for I := 0 to GlobalListNamesOfClasses.Count - 1 do
  begin
    vMetaData := TsgMetaObject(GlobalListNamesOfClasses.Objects[I]);
    if vMetaData.NameIndex > -1 then
      vMetaData.NameInsp := AProc(cnstInspEntNames[vMetaData.NameIndex])
    else
      vMetaData.NameInsp := GlobalListNamesOfClasses[I];
  end;
end;

{ TsgObject }

procedure TsgObject.Assign(const AObj: TsgObject);
begin
  SetStrValue(AObj.GetStrValue);
end;

constructor TsgObject.Create;
begin
  inherited Create;
end;

class function TsgObject.GetClassByXMLName(const AName: string): TsgObjectClass;
begin
  Result := TsgObjectClass(GlobalsgTypes.GetClassByName(AName));
end;

function TsgObject.GetEnumType: TsgObjectPropertyEnum;
var
  vInfo: TsgObjectInfo;
begin
  Result := opObject;
  vInfo := GlobalsgObjectsInfo.GetInfo(ClassType);
  if Assigned(vInfo) then
    Result := vInfo.ObjType;
end;

{$IFNDEF HAS_OBJECT_CAST_FROM_INTERFACE}
function TsgObject.GetInstance: TObject;
begin
  Result := Self;
end;
{$ENDIF}

class function TsgObject.GetPropertyName: string;
begin
  Result := GetObjectPropertyName(ClassName);
end;

function TsgObject.GetStrValue: string;
begin
  Result := GetPropertyName; // normal for entities and should be overriden for atoms
end;

class function TsgObject.GetXMLName: string;
begin
  Result := GetObjectXMLName(ClassName);
end;

function TsgObject.IsLikeGroup: Boolean;
begin
  Result := False;
end;

procedure TsgObject.RemoveProperty(AList: TsgObjectList; AProperty: TsgObjectClass);
var
  I: Integer;
begin
  for I := AList.Count - 1 downto 0 do
    if TsgObject(AList[I]).ClassType = AProperty then
      AList.Delete(I);
end;

function TsgObject.SetStrValue(const S: string): Boolean;
begin
  Result := False;
end;

{ TsgListObject }

procedure TsgListObject.AddProperty(const AList: TsgObjectList;
  const AProperty: TsgObject; const AMode: TsgOperationMode);
begin
  case AMode of
    opAddIfNotExist:
      begin
        if not HasPropeprty(AList, AProperty.ClassType) then
          AList.Add(AProperty);
      end;
  else//opAdd
    AList.Add(AProperty);
  end;
end;

procedure TsgListObject.AddPropertyCopy(const AList: TsgObjectList;
  const AProperty: TsgObject);
var
  vObjNew: TsgObject;
begin
  vObjNew := TsgObjectClass(AProperty.ClassType).Create;
  vObjNew.Assign(AProperty);
  AList.Add(vObjNew);
end;

function TsgListObject.AddToXMLParam(const AParams: TsgXMLParams; const ANode: TsgNodeSample): Boolean;
begin
  Result := CanAddedToXMlParams;
  if Result then
    TsgNodeSampleAccess(ANode).Entity := Self;
end;

function TsgListObject.CanAddedToXMLParams: Boolean;
begin
  Result := False;
end;

function TsgListObject.CanShow: Boolean;
begin
  Result := False;
end;

procedure TsgListObject.DoErrorXMLNode(const ANodeErrors: TsgNode;
  const AChild: Boolean; const ANode: TsgNodeSample;
  const AId: TsgXMLId);
var
  vMessage: string;
  vError: TsgNode;
begin
  if Assigned(ANodeErrors) and (ANode.NodeType in [ntAttribute, ntElement]) then
  begin
    case AId of
      xmlLineWeight:  vMessage := cnstXMLLineWeigthValueError;
      xmlName:        vMessage := cnstXMLNotCorrectName;
      xmlGlobalWidth: vMessage := cnstXMLGlobalWidthValueError;
      xmlLayerName:   vMessage := cnstXMLValueError;
      xmlUndefined:   vMessage := cnstXMLUnsupported + ' ' +
        cnstXMLItemType[AChild] + ' ' +ANode.FullName;
    else
      vMessage := cnstXMLNames[AId].Name;
    end;
    vError := ANodeErrors.AddChildNV(cnstXMLError);
    vError.AddAttribNV(cnstXMLMessage, vMessage);
    if ANode.HasPosition then
      vError.AddAttribNV(cnstXMLPosition).ValueData.ValueAsPoint :=
        Point(ANode.Position.X, ANode.Position.Y);
  end;
end;

class function TsgListObject.XMLDescription: string;
var
  S: string;
begin
  S := GetPropertyName;
  {$IFDEF SGDEL_2009}
  if CharInSet(S[1], ['a', 'A', 'o', 'O', 'i', 'I', 'e', 'E', 'u', 'U']) then
  {$ELSE}
  if S[1] in ['a', 'A', 'o', 'O', 'i', 'I', 'e', 'E', 'u', 'U'] then
  {$ENDIF}
    Result := 'Represents an ' + GetPropertyName + ' in a Drawing Database.'
  else
    Result := 'Represents a ' + GetPropertyName + ' in a Drawing Database.';
end;

procedure TsgListObject.FromNode(const AParentNode: TsgNodeSample);
begin
  FromXML(AParentNode, nil);
end;

function TsgListObject.FromXML(const ANode: TsgNodeSample;
  const AResult: IsgResultNode = nil): Integer;

  function FromXMLNodeInternal(const ANodeItem: TsgNodeSample;
    const AChild: Boolean; const AResult: IsgResultNode = nil): Integer;
  var
    vXMLType: TsgXMLType;
  begin
    Result := cnstXML_UNSUPPORTED;
    vXMLType := GetXMLType(ANodeItem.Name);
    case vXMLType.Id of
      xmlUndefined:
        begin
        end;
      xmlSubEntities:
        begin
          if AChild then
            Result := FromXMLSubEntities(TsgNode(ANodeItem), AResult);
        end
    else
      Result := FromXMLNode(vXMLType, ANodeItem, AChild, AResult);
    end;
    if (Result = cnstXML_UNSUPPORTED) and Assigned(AResult) then
      DoErrorXMLNode(AResult.Errors, AChild, ANodeItem, xmlUndefined);
  end;

var
  I: Integer;
  vRez: Integer;
  vPathKey: UInt64;
  vItem: TsgNodeSample;
begin
  Result := cnstXML_OK;
  vRez := Result;
  I:= 0;
  while (vRez <> cnstXML_ERROR) and (I < ANode.AttributeNodesCount) do
  begin
    vRez := FromXMLNodeInternal(ANode.AttributeNodes[I], False, AResult);
    Inc(I);
  end;
  I:= 0;
  while (vRez <> cnstXML_ERROR) and (I < ANode.ChildNodesCount) do
  begin
    vRez := FromXMLNodeInternal(ANode.ChildNodes[I], True, AResult);
    Inc(I);
  end;

  if CanShow then
  begin
    vPathKey := GetAttributeHandle(ANode, sgConsts.cnstXMLNames[xmlPathKey].Name);
    vItem := ANode.GetAttributeByName(sgConsts.cnstXMLNames[xmlVisible3D].Name);
    if vItem <> nil then
      SetVisibleRepresentation(vPathKey,
        vItem.ValueAsBool);
    vItem := ANode.GetAttributeByName(sgConsts.cnstXMLNames[xmlAlphaBlend].Name);
    if vItem <> nil then
      SetAlphaBlendRepresentation(vPathKey,
        vItem.ValueAsInt);
    vItem := ANode.GetAttributeByName('CustomColor');
    if vItem <> nil then
      SetCustomColorRepresentation(vPathKey,
        vItem.ValueData.ValueAsColor);
    // Old version
    //SetVisibleRepresentation(vPathKey, GetVisibility);
  end;

  if vRez <> cnstXML_ERROR then
    EntityLoaded
  else
    Result := vRez;
end;

{$IFDEF SG_PLUGINSHOST}
function TsgListObject.ExportToPlugins(ANode: IsgNodeFrame): Integer;
begin
  Result := ExportToPluginsInternal(ANode);
end;

function TsgListObject.ExportToPluginsInternal(ANode: IsgNodeFrame): Integer;
begin
  Result := cnstExportToXML_OK;
end;

function TsgListObject.ChangeFromPlugins(ANode: IsgNodeFrame; const APropName: string;
  const AValue: Variant): Integer;
var
  vType: TsgPlcType;
begin
  vType := GetPclType(APropName);
  Result := SetPropertyFromPluginsInternal(ANode, vType, AValue);
end;

function TsgListObject.SetPropertyFromPluginsInternal(ANode: IsgNodeFrame;
  const AType: TsgPlcType; const AValue: Variant): Integer;
begin
  Result := cnstExportToXML_NO;
end;
{$ENDIF}

function TsgListObject.FromXMLNode(const AType: TsgXMLType;
  const ANode: TsgNodeSample; const AIsChild: Boolean;
  const AResult: IsgResultNode = nil): Integer;
begin
  Result := cnstXML_UNSUPPORTED;
end;

function TsgListObject.FromXMLSubEntities(const ANode: TsgNodeSample;
  const AResult: IsgResultNode = nil): Integer;
begin
  Result := cnstXML_OK;
end;

function TsgListObject.GetAlphaBlendRepresentation(const AKey: UInt64): Integer;
begin
  Result := -1;
end;

function TsgListObject.GetCustomColorRepresentation(const AKey: UInt64): Cardinal;
begin
  Result := 0;
end;

function TsgListObject.GetFlags: Integer;
begin
  Result := 0;
end;

function TsgListObject.GetNodeName: string;
begin
  Result := GetXMLName;
end;

function TsgListObject.GetObjectType: Integer;
begin
  Result := -1;
end;

function TsgListObject.GetVisibility: Boolean;
begin
  Result := True;
end;

function TsgListObject.GetVisibleRepresentation(const AKey: UInt64): Boolean;
begin
  Result := False;
end;

procedure TsgListObject.GetXMLIdsChangingHiding(const AChangeList,
  AHideList: TsgIntegerList; AProgID: TsgProgID);
begin
//  if AProgID = piCADNavigator then
//  GetXMLIdsChangingHiding(AChangeList, AHideList);
end;

function TsgListObject.IsSameValue(AObject: TsgObject): Boolean;
begin
  Result := AObject = Self;
end;

procedure TsgListObject.SetAlphaBlendRepresentation(const AKey: UInt64;
  const AValue: Integer);
begin
end;

procedure TsgListObject.SetCustomColorRepresentation(const AKey: UInt64;
  const AValue: Cardinal);
begin

end;

procedure TsgListObject.SetVisibleRepresentation(const AKey: UInt64;
  const AValue: Boolean);
begin

end;

procedure TsgListObject.EntityLoaded;
begin
end;

function TsgListObject.ToNode(const AParentNode: TsgNode;
  const AParams: TsgXMLParams): TsgNode;
begin
  Result := ToXMLWithParams(AParentNode, AParams);
end;

function TsgListObject.ToXML(const AParentNode: TsgNode;
  const AMode: TsgXMLModes = cnstDefaultXMLMode): TsgNode;
var
  vParams: TsgXMLParams;
begin
  vParams := CreateXMLParams(AMode);
  Result := ToXMLWithParams(AParentNode, vParams);
end;

function TsgListObject.ToXMLWithParams(const AParentNode: TsgNode;
  const AParams: TsgXMLParams): TsgNode;
begin
  Result := AParentNode.AddChildNV(GetNodeName);
  ToXMLNode(Result, AParams);
end;

function TsgListObject.ToXMLNode(const ANode: TsgNode;
  const AParams: TsgXMLParams): Integer;
var
  vPathKey: Uint64;
//  vFlagAttrib: TsgNodeSample;
//  vFlags: Integer;
begin
  if AddToXMLParam(AParams, ANode) then
  begin
    if xmlForViewing in AParams.Mode then
      ANode.AddAttribNV(sgConsts.cnstXMLNames[xmlCanShow].Name).ValueAsBool := CanShow;
    if CanShow then
    begin
      if AParams.NodeData is TsgNode then
      begin
        vPathKey := GetAttributeHandle(TsgNode(AParams.NodeData),
          sgConsts.cnstXMLNames[xmlPathKey].Name);
        if vPathKey <> 0 then
        begin
          ANode.AddAttribNV(sgConsts.cnstXMLNames[xmlPathKey].Name).ValueAsHandle :=
            vPathKey;
          ANode.AddAttribNV(sgConsts.cnstXMLNames[xmlVisible3D].Name).ValueAsBool :=
            GetVisibleRepresentation(vPathKey);
          ANode.AddAttribNV(sgConsts.cnstXMLNames[xmlAlphaBlend].Name).ValueAsInt :=
            GetAlphaBlendRepresentation(vPathKey);
          ANode.AddAttribNV('CustomColor').ValueData.ValueAsColor :=
            GetCustomColorRepresentation(vPathKey);

          //The variant of using visibility through the flag does not work at all!!!
//          vFlags := GetFlags;
//
//          vFlags := (vFlags and 127) or
//            (Byte(not GetVisibleRepresentation(vPathKey)) shl 7);
//
//          vFlagAttrib := ANode.GetAttributeByName(
//            sgConsts.cnstXMLNames[xmlFlags].Name);
//          if Assigned(vFlagAttrib) then
//            vFlagAttrib.ValueAsHandle := vFlags
//          else
//            ANode.AddAttribNV(cnstXMLNames[xmlFlags].Name).ValueAsInt := vFlags;
        end;
      end;
    end;
  end;
  Result := cnstXML_OK;
end;

{ TsgGlobalsgObjects }

procedure TsgGlobalsgObjects.ClearFull;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
  Clear;
end;

{ TsgGlobalsgTypes }

procedure TsgGlobalsgTypes.AddClass(AClass: TsgObjectClass;
  const AInspIndexName: Integer = -1);
var
  vMetaObject: TsgMetaObject;
  vHash: Uint64;
  vXMLName: string;
begin
  vXMLName := AClass.GetXMLName;

  vMetaObject := TsgMetaObject.Create;
  vMetaObject.ClassTyp := AClass;
  vMetaObject.NameIndex := AInspIndexName;
  if vMetaObject.NameIndex >= 0 then
    vMetaObject.NameInsp := cnstInspEntNames[vMetaObject.NameIndex]
  else
    vMetaObject.NameInsp := vXMLName;

  GlobalListNamesOfClasses.AddObject(vXMLName, vMetaObject);
  AddObject(AnsiLowerCase(AClass.GetPropertyName), vMetaObject);

  if Assigned(GlobalListTypeOfClasses) then
  begin
    vHash := GetHashCodeByClass(AClass);
    GlobalListTypeOfClasses.Add(vHash, vMetaObject);
  end;
end;

function TsgGlobalsgTypes.GetClass(const S: string): TsgObjectClass;
var
  vIndOf: Integer;
begin
  vIndOf := IndexOf(AnsiLowerCase(S));
  if vIndOf = -1 then
    Result := nil
  else
    Result := TsgObjectClass(TsgMetaObject(Objects[vIndOf]).ClassTyp);
end;

function TsgGlobalsgTypes.GetClassByName(const AName: string): TClass;
var
  vIndex: Integer;
begin
  Result := nil;
  vIndex := GlobalListNamesOfClasses.IndexOf(AName);
  if vIndex > -1 then
    Result := TsgMetaObject(GlobalListNamesOfClasses.Objects[vIndex]).ClassTyp;
end;


function TsgGlobalsgTypes.GetMetaClass(Index: Integer): TClass;
begin
  Result := TsgMetaObject(Objects[Index]).ClassTyp;
end;

{ TsgGlobalsgObjectsInfo }

procedure TsgGlobalsgObjectsInfo.AddInfo(const AClass: TClass;
  const AType: TsgObjectPropertyEnum);
var
  vInfo: TsgObjectInfo;
begin
  vInfo := TsgObjectInfo.Create;
  vInfo.ObjClass := AClass;
  vInfo.ObjType := AType;
  FListInfo.Add(vInfo);
end;

function TsgGlobalsgObjectsInfo.CompareInfo(const A, B: Pointer): Integer;
var
  vInfoA: TsgObjectInfo absolute A;
  vInfoB: TsgObjectInfo absolute B;
begin
  Result := sgCompareStr(vInfoA.ObjClass.ClassName, vInfoB.ObjClass.ClassName);
end;

constructor TsgGlobalsgObjectsInfo.Create;
begin
  inherited Create;
  FListInfo := TsgObjectList.Create;
  FListInfo.Sorted := True;
  FListInfo.Duplicates := dupAccept;
  FListInfo.ProcCompare := CompareInfo;
end;

destructor TsgGlobalsgObjectsInfo.Destroy;
begin
  TsgObjectList.ClearList(FListInfo);
  FListInfo.Free;
  inherited Destroy;
end;

function TsgGlobalsgObjectsInfo.GetInfo(const AClass: TClass): TsgObjectInfo;
var
  I: Integer;
begin
  Result := nil;
  FInfoByQuickFind.ObjClass := AClass;
  I := FListInfo.IndexOf(FInfoByQuickFind);
  if I > -1 then
    Result := TsgObjectInfo(FListInfo[I]);
end;


{ TsgMetaObject }

constructor TsgMetaObject.Create;
begin
  inherited Create;
  NameIndex := -1;
end;

initialization
  GlobalListNamesOfClasses := TsgStringList.Create;
  GlobalListNamesOfClasses.Sorted := True;
  GlobalListNamesOfClasses.Duplicates := dupIgnore;
{$IFDEF SG_CAD_NAVIGATOR}//need for inspector
  GlobalListTypeOfClasses := TsgObjectCollection.Create;
{$ENDIF}
  GlobalsgObjectsInfo := TsgGlobalsgObjectsInfo.Create;
  GlobalsgObjects := TsgGlobalsgObjects.Create;

  GlobalsgTypes := TsgGlobalsgTypes.Create;
  GlobalsgTypes.Sort;

finalization
   FreeAndNil(GlobalListTypeOfClasses);
   GlobalsgObjects.ClearFull;
   GlobalsgObjects.Free;
   ClearObjects(GlobalsgTypes);
   GlobalsgTypes.Free;
   GlobalsgObjectsInfo.Free;
   FreeAndNil(GlobalListNamesOfClasses);
end.
