{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                     XML Interface                          }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit XMLInterface;
{$INCLUDE SGDXF.inc}

interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPImage,
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Types,
{$ELSE}
  Graphics, {$IFDEF CS_USEFORM}Menus,{$ENDIF}
{$ENDIF}
  SysUtils, Classes, Properties, DXFConv,
{$IFDEF SG_USE_EXPORT}
   CADExport,
{$ENDIF}
  CADImage, sgXMLParser, sgConsts, sgFunction, sgLists
{$IFDEF CS_USEFORM}
  , Controls, Forms, sgDrawingNavigator
{$ENDIF}
{$IFDEF SG_USE_CADXML}
  , sgCADXML
{$ENDIF}
{$IFDEF SG_USE_PNG}
  , pngimage
{$ELSE}
  {$IFDEF SGDEL_XE2}
    {$IFNDEF SG_FIREMONKEY}
  , Vcl.Imaging.jpeg
    {$ENDIF}
  {$ELSE}
{$IFNDEF SGFPC}
  , jpeg
{$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
  ;

type
  TsgXMLKey = (xkUndefined, xkGet, xkAdd, xkSelect, xkApply, xkDelete, xkLoad,
    xkUnload, xkSwitchDrawing, xkGetImage, xkContextMenu, xkRibbon, xkCommand,
    xkGetSelected, xkFitToSize, xkSignToEvent, xkUnSelect, xkGetViewRect,
    xkSetViewRect, xkGetBox, xkShowSelectedEntities, xkSave, xkCreateHatch,
    xkGetDrawingChanged, xkGetView, xkSetView, xkCustomSelectMode, xkEditor,
    xkRegistration, xkSetSpecialCustomInsert, xkSupportedCommandsList,
    xkSupportedClassesList, xkHelp, (*xkItem,*) xkMenuItemClick, (*xkChanged*,)
    (*xkExportParams,*) (*xkSelected,*) xkGetExternalFiles, xkInvalidate,
    xkGetDrawingsList, xkCaptureScreen (*, xkSuccess*),
    xkSetDrawingChanged, xkMenuButton, xkGetCADPoint, xkReport,
    xkGetData, xkSetData, xkFindTextEntities, xkIterator, xkCreateBtiEntity,
    xkWaterMark, xkSpecialLayers, xkGlobalVariable, xkGetSaveHandles,
    {xkGetSpecification,} xkRemove,
    xkAppCommand, xkAppMainCommand);

  TsgXMLKeys = set of TsgXMLKey;

  TsgXMLEvent = (xeUndefined, xeOnMouseMove, xeOnMouseDown, xeOnMouseUp,
    xeOnMouseWheel, xeOnSelectEntities, xeOnConfirmEntitiesDeletion,
    xeOnChangeEntities, xeOnCopyEntities, xeOnCreateEntities, xeOnDeleteEntities,
    xeOnCancelEntities, xeOnLayoutBeforeChange, xeOnLayoutAfterChange,
    xeOnPasteEntities, xeOnRedoEntities, xeOnUndoEntities, xeOnKeyDown,
    xeOnKeyUp, xeOnKeyPress, xeOnPaint, xeOnContextClick, xeOnContextPopUp,
    xeOnButtonClick, xeApplicationExeption, xeOnMeasure, xeOnGetInfo,
    xeOnSelectBTIEntities, xeOnImageNew, xeOnImageLoad, xeOnImageSave,
    xeOnImageClose, xeOnAcisNumber, xeOnDragAndDrop, xeOnDrawingSwitch);

  TsgXMLEvents = set of TsgXMLEvent;

  TsgNearestFlag = (nfVisibleEnts);
  TsgNearestFlags = set of TsgNearestFlag;

  TsgParentGetter = function (const AParent: TsgDXFEntity;
    const AName: string): TsgDXFEntity of object;

const
  cnstEvents: array[TsgXMLEvent] of record
    Name: string;
    ValueType: TsgDataType;
    Description: string;
  end = (
    (Name: 'OnUndefined';                ValueType: dtUndefined;   Description: 'Abstract event'),
    (Name: 'OnMouseMove';                ValueType: dtParameters;  Description: 'Return values: X="Screen X" Y="Screen Y" point="x,y,z"'),
    (Name: 'OnMouseDown';                ValueType: dtParameters;  Description: 'Return values: X="Screen X" Y="Screen Y" point="x,y,z"'),
    (Name: 'OnMouseUp';                  ValueType: dtParameters;  Description: 'Return values: X="Screen X" Y="Screen Y" point="x,y,z"'),
    (Name: 'OnMouseWheel';               ValueType: dtParameters;  Description: 'Return values: X="Screen X" Y="Screen Y" WheelDelta="DELTA" point="x,y,z"'),
    (Name: 'OnSelectEntity';             ValueType: dtString;      Description: 'Return values: Result handle="handle of selected entity"'),
    (Name: 'OnConfirmEntitiesDeletion';  ValueType: dtString;  Description: 'Event returns handles of the entities to be deleted. Parent application can call a confirmation dialog and delete the entities by <delete/> command.'), // command line
    (Name: 'OnChangeEntities';           ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnCopyEntities';             ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnCreateEntities';           ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnDeleteEntities';           ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnCancelEntities';           ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnLayoutBeforeChange';       ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnLayoutAfterChange';        ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnPasteEntities';            ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnRedoEntities';             ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnUndoEntities';             ValueType: dtString;  Description: 'Return values: Result handle="handle of selected entity'),
    (Name: 'OnKeyDown';                  ValueType: dtParameters;  Description: 'Return values: Key="KEYCHAR"'),
    (Name: 'OnKeyUp';                    ValueType: dtParameters;  Description: 'Return values: Key="KEYCHAR"'),
    (Name: 'OnKeyPress';                 ValueType: dtParameters;  Description: 'Return values: Key="KEY"'),
    (Name: 'OnPaint';                    ValueType: dtUndefined;   Description: ''),
    (Name: 'OnMenuContextClick';         ValueType: dtParameters;   Description: 'Return values: Caption and Index of menu'),
    (Name: 'OnMenuContextPopup';         ValueType: dtParameters;   Description: 'Context menu event OnPopup'),
    (Name: 'OnMenuButtonClick';          ValueType: dtParameters;   Description: 'Return values: Caption and Handle of button'),
    (Name: 'OnApplicationException';     ValueType: dtParameters;  Description: 'Return values: info of an exception'),
    (Name: 'OnMeasure';                  ValueType: dtParameters;  Description: 'Return values: Measure'),
    (Name: 'OnGetInfo';                  ValueType: dtString;  Description: 'Return info'),
    (Name: 'OnSelectBTIEntity';          ValueType: dtString;      Description: 'Return values of bti entities'),
    (Name: 'OnImageNew';                 ValueType: dtString;     Description: 'Image new'),
    (Name: 'OnImageLoad';                ValueType: dtString;      Description: 'Image load'),
    (Name: 'OnImageSave';                ValueType: dtString;      Description: 'Image save'),
    (Name: 'OnImageClose';               ValueType: dtString;     Description: 'Image close'),
    (Name: 'OnAcisNumber';               ValueType: dtParameters; Description: 'Return 3d entity number in file'),
    (Name: 'OnDragAndDrop';              ValueType: dtParameters;  Description: 'Return values: DragAndDrop'),
    (Name: 'OnDrawingSwitch';            ValueType: dtParameters;  Description: 'Return values: GUID info')
  );

type
  TsgProxyEditor = class({$IFDEF SG_FIREMONKEY}TsgInterfacedObject{$ELSE}TInterfacedObject{$ENDIF})
  private
    FOnPasteEntities: TCADNotifyEvent;
    FOnChangeEntities: TCADNotifyEvent;
    FOnConfirmEntitiesDeletion: TCADConfirmEvent;
    FOnUndoEntities: TCADNotifyEvent;
    FOnRedoEntities: TCADNotifyEvent;
    FOnDeleteEntities: TCADNotifyEvent;
    FOnLayoutChange: TCADNotifyEvent;
    FOnGetInfo: TCADGetInfoEvent;
    FOnLayoutBeforeChange: TCADNotifyEvent;
    FOnSelectEntities: TCADNotifyEvent;
    FOnCreateEntities: TCADNotifyEvent;
    FOnCopyEntities: TCADNotifyEvent;
    FOnCancelEntities: TCADNotifyEvent;
    FOnSelectBTIEntities: TCADNotifyEvent;

{$IFDEF CS_USEFORM}
    FOnMouseDownEvent: TMouseEvent;
    FOnMouseMoveEvent: TMouseMoveEvent;
    FOnMouseUpEvent: TMouseEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
{$ENDIF}
    FOnPaint: TNotifyEvent;
    FOnImageNew: TCADImageClose;
    FOnImageLoad: TCADImageLoad;
    FOnImageSave: TCADImageSave;
    FOnImageClose: TCADImageClose;
    FOnAcisNumber: TsgCADNotifyMessage;
    FSkipChanges: Boolean;
    procedure SetSkipChanges(const Value: Boolean);
  protected
    function GetCoordsMDown: TFPoint; virtual; abstract;
    function GetCoordsMMove: TFPoint; virtual; abstract;
    function GetCoordsMUp: TFPoint; virtual; abstract;
    function GetCoordMObject: TObject; virtual; abstract;
    function GetImageCoords(const APoint: TPoint): TFPoint; virtual; abstract;
    function GetOwner: TObject; virtual;
    function GetScreenCoords(const APoint: TFPoint): TPoint; virtual; abstract;
    function GetViewRect: TRect;  virtual; abstract;
    function GetViewFRect: TFRect;  virtual; abstract;
    function GetReadOnly: Boolean; virtual; abstract;
    function GetSilentMode: Boolean; virtual;
    procedure SetReadOnly(const AValue: Boolean); virtual; abstract;
    procedure SetSilentMode(const AValue: Boolean); virtual;

    procedure DoChangeEntities(const Image, AEntities: TObject);
    function DoCanDeleteEntities(const Image, AEntities: TObject): Integer;
    procedure DoCopyEntities(const Image, AEntities: TObject);
    procedure DoCreateEntities(const Image, AEntities: TObject);
    procedure DoDeleteEntities(const Image, AEntities: TObject);
    procedure DoGetInfo(const Image: TObject; const AName, AXML: string;
      const AAttribData: TStrings);
    procedure DoLayoutChange(const Image, AEntities: TObject);
    procedure DoLayoutBeforeChange(const Image, AEntities: TObject);
    procedure DoPasteEntities(const Image, AEntities: TObject);
    procedure DoRedoEntities(const Image, AEntities: TObject);
    procedure DoSelectEntities(const Image, AEntities: TObject);
    procedure DoSelectBTIEntities(const Image, AEntities: TObject);
    procedure DoUndoEntities(const Image, AEntities: TObject);
    procedure DoCancelEntities(const Image, AEntities: TObject);
{$IFDEF CS_USEFORM}
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; X, Y: Integer; var AHadnled: Boolean);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
{$ENDIF}
    procedure DoPaint(Sender: TObject);
  public
    constructor Create(const AOwner: TObject); virtual;
    procedure BeginEnt(const AEnt: TsgDXFEntity; const AInternal: Boolean);virtual; abstract;
    procedure BeginList(const AList: TList; const ACount: Integer; const AInternal: Boolean);virtual; abstract;

{use this function to change selected entitites}
    function AfterChange: Integer;  virtual; abstract;
    function BeforeChange(const AEntities: TList): Integer;  virtual; abstract;

    procedure CalcExtents; virtual; abstract;
    procedure Changed;virtual; abstract;
    function CreatingHatch(const APatternName: string;
      const AScale, AAngle: Double; const AColor1: PsgColorCAD;
      const AColor2: PsgColorCAD = nil): TsgDXFEntity; virtual; abstract;
    procedure DeleteEntities; virtual; abstract;
    procedure DoImageClose(Sender: TObject;
      const AAttribData: TStrings);
    procedure DoImageNew(Sender: TObject;
      const AAttribData: TStrings);
    procedure DoImageLoad(Sender: TObject; const AState: Integer;
      const AAttribData: TStrings);
    procedure DoImageSave(Sender: TObject; const AFileName: string;
      const AFormat: TsgExportFormat; const AStream: TStream;
      const AAttribData: TStrings);
    procedure ShowCADRect(ARect: TFRect);  virtual; abstract;
    procedure ShowWindowRect(ARect: TRect);  virtual; abstract;
    procedure EndEnt(const AEnt: TsgDXFEntity);virtual; abstract;
    procedure EndList(const AList: TList; const ACount: Integer);virtual; abstract;
    procedure FitToSize; virtual; abstract;
    procedure GetNearestEntity(const R: TRect; const AFlags: TsgNearestFlags;
      var NearestEntityName: string; var APoint2D: TPoint;
      var APoint3D: TFPoint); virtual;
    procedure GetSelectedEntities(const AList: TList); virtual; abstract;
    function GetCurrentDrawingGuid: TGUID; virtual;
    procedure Invalidate; virtual; abstract;
    procedure InvalidateFRect(const ARect: TFRect); virtual; abstract;
    function IsChanged: Boolean; virtual; abstract;
    function IsOnlyCommand: Boolean; virtual;
    function IsEntUsed(const AEnt: TsgDXFEntity): Boolean; virtual;
    function LispRun(const AText: string; AResults: TStrings = nil): Integer; virtual; abstract;
    procedure OpenVPort; virtual; abstract;
    function SaveImage(const AName: string; const AIsDWG: Boolean;
      AVer: TsgDWGVersion = acR2004;
      IsConvertImageToOLE: Boolean = True): Boolean; virtual; abstract;
    function GetCustomDrawParams(const AHandle: UInt64): TsgCustomSelectMode; virtual; abstract;
    procedure GetShowEntitySettings(var AMethod: TsgShowEntityMethod;
      var APrecision: Double); virtual; abstract;
    procedure SetCustomDrawParams(const AHandle: UInt64;
      const ACustomDraw: TsgCustomSelectMode;
      const AUnregister: Boolean); virtual; abstract;
    function SetSelectedEntities(const AList: TList): Integer; virtual; abstract;
    procedure SetShowEntitySettings(const AMethod: TsgShowEntityMethod;
      const APrecision: Double = 10); virtual; abstract;
    procedure ShowSelectEntities; virtual; abstract;
    procedure RecreateBlockAsTemplate; virtual; abstract;
    function ResultList: TStringList; virtual; abstract;

    function SetSpecialLayers(const ABlocked, ANoSelected: string): Integer; virtual;

    property CoordsMDown: TFPoint read GetCoordsMDown;
    property CoordsMMove: TFPoint read GetCoordsMMove;
    property CoordsMUp: TFPoint read GetCoordsMUp;
    property CoordMObject: TObject read GetCoordMObject;
    property Owner: TObject read GetOwner;
    property OnGetInfo: TCADGetInfoEvent read FOnGetInfo write FOnGetInfo;
    property OnChangeEntities: TCADNotifyEvent read FOnChangeEntities write
      FOnChangeEntities;
    property OnConfirmEntitiesDeletion: TCADConfirmEvent read
      FOnConfirmEntitiesDeletion write FOnConfirmEntitiesDeletion;
    property OnCopyEntities: TCADNotifyEvent read FOnCopyEntities write
      FOnCopyEntities;
    property OnCreateEntities: TCADNotifyEvent read FOnCreateEntities write
      FOnCreateEntities;
    property OnDeleteEntities: TCADNotifyEvent read FOnDeleteEntities write
      FOnDeleteEntities;
    property OnLayoutAfterChange: TCADNotifyEvent read FOnLayoutChange write
      FOnLayoutChange;
    property OnLayoutBeforeChange: TCADNotifyEvent read FOnLayoutBeforeChange write
      FOnLayoutBeforeChange;
    property OnPasteEntities: TCADNotifyEvent read FOnPasteEntities write
      FOnPasteEntities;
    property OnRedoEntities: TCADNotifyEvent read FOnRedoEntities write
      FOnRedoEntities;
    property OnSelectEntities: TCADNotifyEvent read FOnSelectEntities write
      FOnSelectEntities;
    property OnSelectBTIEntities: TCADNotifyEvent read FOnSelectBTIEntities write
      FOnSelectBTIEntities;
    property OnUndoEntities: TCADNotifyEvent read FOnUndoEntities write
      FOnUndoEntities;
    property OnCancelEntities: TCADNotifyEvent read FOnCancelEntities write
      FOnCancelEntities;
{$IFDEF CS_USEFORM}
    property OnMouseDown: TMouseEvent read FOnMouseDownEvent write
      FOnMouseDownEvent;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMoveEvent write
      FOnMouseMoveEvent;
    property OnMouseUp: TMouseEvent read FOnMouseUpEvent write
      FOnMouseUpEvent;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write
      FOnMouseWheel;

    property OnKeyDown: TKeyEvent read FOnKeyDown write
      FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write
      FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write
      FOnKeyUp;
{$ENDIF}
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnImageNew: TCADImageClose read FOnImageNew write FOnImageNew;
    property OnImageLoad: TCADImageLoad read FOnImageLoad write FOnImageLoad;
    property OnImageSave: TCADImageSave read FOnImageSave write FOnImageSave;
    property OnImageClose: TCADImageClose read FOnImageClose write FOnImageClose;
    property OnAcisNumber: TsgCADNotifyMessage read FOnAcisNumber write FOnAcisNumber;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property SilentMode: Boolean read GetSilentMode write SetSilentMode;
    property SkipChanges: Boolean read FSkipChanges write SetSkipChanges;
  end;

  TsgXMLResult = class(TsgInterfacedObject, IsgResultNode)
  private
    FResultNode: TsgNode;
    FRootOut: TsgNode;
    FXMLOut: TsgParser;
  protected
    procedure CreateResultNode(const AName: string);
    function GetResult: TsgNode;
    function GetOutput: TsgNode;
    function GetErrors: TsgNode;
    function InitResultNodeOnCreating: Boolean; virtual;
    procedure InitRoot(var AParser: TsgParser; var ARoot: TsgNode);
  public
    constructor Create; virtual;
    constructor CreateByInstruction(AName: string);
    destructor Destroy; override;
    property RootOut: TsgNode read FRootOut;
    property XMLOut: TsgParser read FXMLOut;
  end;

  TsgXMLOut = class(TsgXMLResult, IsgXMLOut)
  protected
    function AddAttribDataToOut(const AOut: TsgNode; const AItems: TStrings): Boolean;
    function AddXmlDataToOut(const AOut: TsgNode; const AXML: string): Boolean;
  public
    procedure AddToOutput(const Attribs: TStrings);
    procedure AddToOutputNV(const AName, AValue: string; const AMessage: string = '');
    procedure AddToOutputFromNode(const ANode: TObject);
    procedure AddItemsToOutput(const AItems: TStrings);
    procedure AddXmlToOutput(const AXML: string);
    procedure AddToError(const Attribs: TStrings);
    procedure AddToErrorNV(const AName, AValue: string; const AMessage: string = '');
  end;

  TsgXMLIDE = class(TsgXMLOut, IsgXMLIde)
  private
    FResult: string;
    FXMLInp: TsgParser;
    FSelectedHandles: TsgObjectHandleList;
    FHWNDMainForm: HWND;
    FCADImage: TsgCADImage;
    FInstructions: TStringList;
    FCADEditor: TsgProxyEditor;
    FOnXMLMessage: TsgXMLMessage;
    FPathToXMLExamples: string;
    FXMLEvents: TsgXMLEvents;
{$IFDEF SGAPPPROTECT}
    FNotAppCmd: Boolean;
{$ENDIF}
    //Used to describe the parameters for the search in the current command
    FXMLCommParameterList: TStringList;
{$IFDEF CS_USEFORM}
    FOnApplicationExeptionPrev: TExceptionEvent;
{$ENDIF}
    FCallWaitFormEvent: TNotifyEvent;
    function CreateResultEvent(const AName: string): TsgNode;
    procedure ClearParser;
    procedure DoXMLParserError(const AMessage: string;
      const APosition: PPoint);
    procedure SendEvents(const AEventResult: TsgNode;
      const AImage: TObject; AImgGuid: PGUID = nil);
    procedure CheckResults;
    function CheckForApp(ANode: TsgNodeSample; AResult: IsgResultNode): Boolean;
    function GetEntityByHandle(const AParent: TsgDXFEntity;
      const AHandle: string): TsgDXFEntity;
    function GetEntityOnName(const AParent: TsgDXFEntity;
      const AName: string): TsgDXFEntity;
    function GetEntityByPathKey(const AEntity: TsgDXFEntity): TsgDXFEntity;
    function GetIResultNode: IsgResultNode;
    function GetImageGuid: TGUID;
    function GetKey(const AStr: string): TsgXMLKey;
    function GetEntityByNode(const ANode: TsgNodeSample;
      const AErrorsNode: TsgNode;
      AHandles: TsgInt64List = nil; AEntityList: TList = nil): TsgDXFEntity;
    procedure AddEntityToSelectHandles(const AEntity: TsgDXFEntity);
    function GetEntityFromSelectHandles(const AIndex: Integer): TsgDXFEntity;
    function IndexOfSelectedHandles(const AEntity: TsgDXFEntity): Integer;
    function GetXMLOutString: string;
    function GetOnXMLMessage: TsgXMLMessage;
    procedure GetParentEntity(const APath,ADelimeter: string;
      const AProc: TsgParentGetter; var AParent: TsgDXFEntity);
    procedure GetXrefLayout(const APath,ADelimeter: string;
      var AParent: TsgDXFEntity);
    procedure ExpEntitiesToXml(const AEntities: TList; AOut: TsgNode;
      const AMode: TsgXMLModes = cnstDefaultXMLMode);
    procedure SetCADImage(const Value: TsgCADImage);
    function SaveConverterToXML: string;
    function SaveEntityToXML(AEntity: TsgListObject): string;
    function AddSelectEntity(AEntity: TsgDXFEntity;
      const ANode: TsgNodeSample): Boolean;
    function SetSelectEntities(AEntities: TList;
      const ANode: TsgNodeSample; const AMode: Integer = 0): Boolean;
    procedure AddEntitiesToNode(const ANode: TsgNode;
      const Image, AEntities: TObject;const AType: TsgXMLEvent);
    procedure AddExceptionToNode(const ANode: TsgNode;
      const AException: Exception);
    procedure SaveToFileWithExportParams(ANode: TsgNodeSample;
      AResult: IsgResultNode);
    procedure ConvertStreamToOutputBase64(AStream: TStream;
      AOutput: TsgNode; AFormat: TsgExportFormat);

    procedure AddAttribsNoUseInFilter(const ANode: TsgNode; const Attribs: TList);
    procedure ApplyFilter(const AFilter: TsgNode; const AEntities: TList);
    function ExtractAttribsNoUseInFilter(const ANode: TsgNode; const Attribs: TList): Boolean;
    function GetEntitiesByNodeWithFilter(const ANode: TsgNodeSample;
      const AErrorsNode: TsgNode; AHandles: TsgInt64List; AEntityList: TList): Boolean;


    function FilterElement(const AEnt: TsgDXFEntity;
      const AFindEnt: TsgDXFEntity): Boolean;
    function FilterCurElement(const AEnt: TsgDXFEntity;
      const AFindEnt: TsgDXFEntity): Boolean;
    function FilterBlockEnt(const ABlock: TsgDXFBlock;
      const AFindEnt: TsgDXFEntity): Boolean;
    function ScanBlocks(const AGroup: TsgDXFGroup;
      const AFindEnt: TsgDXFEntity): Boolean;
    function ScanElements(const AGroup: TsgDXFGroup;
      const AFindEnt: TsgDXFEntity): Boolean;
    function IsCanDelete(AObject: TsgDXFEntity;
      var AParent: TsgDXFGroup): Boolean;

    procedure SetCADEditor(const Value: TsgProxyEditor);
    procedure SetOnXMLMessage(const Value: TsgXMLMessage);
    function CommandToXMLNode(const ANode: TsgNode;
      const AResult: TsgNode; ACommand: TsgXMLKey): Integer;
    procedure CommandInernalIterate(ANode: TsgNodeSample;
      AResult: IsgResultNode; const ACommand: Integer);
    procedure CommandSupportedCommandsList(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSupportedClassesList(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGet(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetBox(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetCADPoint(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetExternalFiles(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandAdd(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandFindTextEntities(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandFitToSize(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandIterator(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSelect(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandUnSelect(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandApply(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandApplyXML(ANode: TsgNodeSample; AResult: IsgResultNode;
      const AUseChilds: Boolean);
    procedure CommandDelete(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandRemove(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandLoad(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandUnLoad(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSwitchDrawing(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetDrawingsList(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetImage(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandContextMenu(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandMenuButton(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandRibbon(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandCommand(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandMainAppFunction(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetSelected(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandReport(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSignToEvent(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSaveToFile(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetDrawingChanged(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSetDrawingChanged(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetView(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSetView(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandCustomSelectMode(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandEditorAttribs(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSetShowEntitySettings(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandShowSelectEntities(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandCreateHatch(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandRegistration(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSetSpecialCustomInsert(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandInvalidate(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGetViewRect(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSetViewRect(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandCaptureScreen(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandWaterMark(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSpecialLayers(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandGlobalVariable(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSaveHandles(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandAppCommand(ANode: TsgNodeSample; AResult: IsgResultNode);
{$IFDEF SGABVIEWER}
    procedure CommandGetData(ANode: TsgNodeSample; AResult: IsgResultNode);
    procedure CommandSetData(ANode: TsgNodeSample; AResult: IsgResultNode);
{$ENDIF}
{$IFDEF SG_BTI_INTERFACE}
    procedure CommandCreateBtiEntity(ANode: TsgNodeSample; AResult: IsgResultNode);
{$ENDIF}
    function CommandHelp(ANode: TsgNodeSample): string;
    procedure ProcessXMLNode(ANode: TsgNode; var ANodesCount: Integer);
    procedure SaveToHTMLHelp(const AFileName: string);

    function IsInvalidRefs: Boolean;
{$IFDEF CS_USEFORM}
    procedure OnMouseEvent(const AType: TsgXMLEvent; Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y, WheelDelta: Integer);
{$ENDIF}
    procedure OnEventCustom(const AType: TsgXMLEvent;
      const Image, AEntities: TObject;
      const AttribNames, AttribValues: array of string;
      const ANeedGuid: Boolean;
      const ANode: TsgNode = nil);
    function CreateImageEvent(const AEvent: TsgXMLEvent; Sender: TObject;
      const AXML: string; const AAttribData: TStrings;
      var AOut: TsgNode): TsgNode;
    function IsXmlActive(const ANode: TsgNode;
      const AKey: TsgXMLKey; var ANodesCount: Integer): Boolean;
  protected
    procedure DoTrialMessage;
    function InitResultNodeOnCreating: Boolean; override;
    procedure SetCADEditorOnly(const AValue: TsgProxyEditor);
  public
    MainApplication: IsgMainApplication;
    constructor Create; override;
    destructor Destroy; override;
    function CheckXMLInterfaceVersion(const AMainNode: TsgNodeSample): Boolean;
    procedure GetHints(const ANodeName: string; var Hints: TStringList);
    // main function of XML IDE:
    function ProcessXML(const AInput: string): string;
    property CADImage: TsgCADImage read FCADImage write SetCADImage;
    property CADEditor: TsgProxyEditor read FCADEditor write SetCADEditor;
    property HWNDMainForm: HWND read FHWNDMainForm write FHWNDMainForm;
    property OnCallWaitFormEvent: TNotifyEvent read FCallWaitFormEvent
      write FCallWaitFormEvent;
    //mouse events
{$IFDEF CS_USEFORM}
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
{$ENDIF}
    //MainApplication event
    procedure OnApplicationExeption(Sender: TObject; E: Exception);
{$IFNDEF SG_FIREMONKEY}
    procedure OnContextMenuClick(Sender: TObject);
    procedure OnContextMenuPopUp(Sender: TObject);
{$ENDIF}
    procedure OnInterfaceButtonClick(const AHandle: THandle; const ACaption: string);
    procedure OnButtonMenuClick(const AHandle: THandle;
      const ACaption: string);
    //editor events
    procedure OnGetInfo(const Image: TObject; const AName, AXML: string;
      const AttribsData: TStrings);
    procedure OnChangeEntities(const Image, AEntities: TObject);
    procedure OnCopyEntities(const Image, AEntities: TObject);
    procedure OnCreateEntities(const Image, AEntities: TObject);
    procedure OnDeleteEntities(const Image, AEntities: TObject);
    procedure OnCancelEntities(const Image, AEntities: TObject);
    procedure OnLayoutAfterChange(const Image, AEntities: TObject);
    procedure OnLayoutBeforeChange(const Image, AEntities: TObject);
    procedure OnPasteEntities(const Image, AEntities: TObject);
    procedure OnRedoEntities(const Image, AEntities: TObject);
    procedure OnUndoEntities(const Image, AEntities: TObject);
    procedure OnConfirmEntitiesDeletion(const Image, AEntities: TObject;
      var AValue: Integer);
    procedure OnSelectEntities(const Image, AEntities: TObject);
    procedure OnSelectBTIEntities(const Image, AEntities: TObject);
    //key events
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    //main events
    procedure OnPaint(Sender: TObject);
    procedure OnMeasure(Sender: TObject);
    procedure OnDragAndDrop(Sender: TObject);
    procedure OnImageNew(Sender: TObject;
      const AAttribData: TStrings);
    procedure OnImageLoad(Sender: TObject; const AState: Integer;
     const AAttribData: TStrings);
    procedure OnImageClose(Sender: TObject;
     const AAttribData: TStrings);
    procedure OnImageSave(Sender: TObject; const AFileName: string;
      const AFormat: TsgExportFormat; const AStream: TStream;
      const AAttribData: TStrings);
    procedure OnAcisNumber(const Sender: TObject;
      const AParams: TsgMessageParams);
    procedure OnDrawingSwitch(const Sender: TObject;
      const ADrawing: TsgDrawingXMLData);
    property OnXMLMessage: TsgXMLMessage read GetOnXMLMessage write SetOnXMLMessage;
    property XMLEvents: TsgXMLEvents read FXMLEvents;
  end;

const
  cnstXMLCmdComment = 'Comment';
  cnstXMLCmdBox = 'Box';
  cnstXMLMode = 'Mode';
  cnstXMLCmdRect = 'Rect';
  cnstXMLCmdAdd = 'add';
  cnstXMLCmdApply = 'apply';
  cnstXMLCmdBase64 = 'base64';
  cnstXMLCmdCaption = 'caption';
  cnstXMLCmdColor = 'color';
  cnstXMLCmdCommand = 'command';
  cnstXMLCmdContextMenu = 'ContextMenu';
  cnstXMLCmdCreateHatch = 'CreateHatch';
  cnstXMLCmdCurrent = 'current';
  cnstXMLCmdDelete = 'delete';
  cnstXMLCmdRemove = 'remove';
  cnstXMLCmdEvent = 'Event';
  cnstXMLCmdFile = 'file';
  cnstXMLCmdFittoSize = 'FitToSize';
  cnstXMLCmdGet = 'get';
  cnstXMLCmdGetBox = 'getbox';
  cnstXMLCmdGetImage = 'getimage';
  cnstXMLCmdGetSelected = 'getselected';
  cnstXMLCmdGlobalX = 'globalx';
  cnstXMLCmdGlobalY = 'globaly';
  cnstXMLCmdHandle = 'handle';
  cnstXMLCmdGuid = 'guid';
  cnstXMLCmdPathName = 'pathname';
  cnstXMLCmdPathKey = 'pathkey';
  cnstXMLCmdXREFPath = 'xrefpath';
  cnstXMLCmdHatchName = 'hatchname';
  cnstXMLCmdIndex = 'index';
  cnstXMLCmdItem = 'item';
  cnstXMLCmdItems = 'items';
  cnstXMLCmdLine = 'line';
  cnstXMLCmdLoad = 'load';
  cnstXMLCmdMarker = 'marker';
  cnstXMLCmdBoxCad = 'BoxCad';
  cnstXMLCmdMenuItemClick = 'menuItemClick';
  cnstXMLCmdMessage = 'message';
  cnstXMLCmdMode = 'mode';
  cnstXMLContextMenuIcons = 'Icons';
  cnstXMLContextMenuIconsCode = 'Image';
  cnstXMLCmdImageIndex = 'ImageIndex';
  cnstXMLCmdName = 'name';
  cnstXMLCmdOnClick = 'onClick';
  cnstXMLCmdOnPopup = 'onPopup';
  cnstXMLCmdOut = 'out';
  cnstXMLCmdPoint = 'point';
  cnstXMLCmdPoint1 = 'point1';
  cnstXMLCmdPolyline = 'polyline';
  cnstXMLCmdResult = 'Result';
  cnstXMLCmdRibbon = 'ribbon';
  cnstXMLCmdSave = 'save';
  cnstXMLCmdSaveERROR = 'file is not saved';
  cnstXMLCmdSaveHandle = 'savehandle';
  cnstXMLCmdSaveOK = 'file is saved';
  cnstXMLCmdSelect = 'select';
  cnstXMLCmdSelected = 'selected';
  cnstXMLCmdSelectedCount = 'SelectedCount';
  cnstXMLCmdSelectEntities = 'selectEntities';
  cnstXMLCmdConfirmEntitiesDeletion = 'ConfirmEntitiesDeletion';
  cnstXMLCmdSender = 'sender';
  cnstXMLCmdGetViewRect = 'getviewrect';
  cnstXMLCmdSetViewRect = 'setviewrect';
  cnstXMLCmdShowSelectedEntities = 'showselectedentities';
  cnstXMLCmdSignToEvent = 'signtoevent';
  cnstXMLCmdSwitchDrawing = 'switchdrawing';
  cnstXMLGetDrawingsList = 'getdrawingslist';
  cnstXMLCaptureScreen = 'capturescreen';
  cnstXMLCmdText = 'text';
  cnstXMLCmdType = 'type';
  cnstXMLCmdUnload = 'unload';
  cnstXMLCmdUnselect = 'unselect';
  cnstXMLCmdVersion = 'version';
  cnstXMLCmdVertex = 'vertex';
  cnstXMLCmdVisible = 'visible';
  cnstXMLCmdAngle = 'angle';
  cnstXMLCmdPatternScale = 'patternscale';
  cnstXMLCmdGetDrawingChanged = 'getdrawingchanged';
  cnstXMLCmdSetDrawingChanged = 'setdrawingchanged';
  cnstXMLCmdButtonMenu = 'buttonmenu';
  cnstXMLCMDDrawingNavigator = 'DrawingNavigator';
  cnstXMLCmdGetCADCoords = 'getcadcoords';
  cnstXMLCmdChanged = 'changed';
  cnstXMLCmdIsConvertImageToOle = 'isconvertimagetoole';
  cnstXMLCmdGetView = 'GetView';
  cnstXMLCmdSetView = 'SetView';
  cnstXMLCmdCustomSelectMode = 'CustomSelectMode';
  cnstXMLCmdReadOnly = 'ReadOnly';
  cnstXMLCmdShowEntityMethod = 'ShowEntityMethod';
  cnstXMLCmdShowEntityPrecision = 'ShowEntityPrecision';
  cnstXMLCmdEditor = 'Editor';
  cnstXMLCmdDrawMatrix = 'DrawMatrix';
  cnstXMLCmdDrawing = 'Drawing';
  cnstXMLCmdEx = 'Ex';
  cnstXMLCmdEy = 'Ey';
  cnstXMLCmdEz = 'Ez';
  cnstXMLCmdE0 = 'E0';
  cnstXMLCmdShowSelectEntities = 'ShowSelectEntities';
//  cnstXMLNotSupported = 'NotSupported';//use cnstXMLUnsupported
  cnstXMLCmdUser = 'User';
  cnstXMLCmdEMail = 'EMail';
  cnstXMLCmdKey = 'Key';
  cnstXMLCmdRegistration = 'Registration';
  cnstXMLCmdSetSpecialCustomInsert = 'SetCustomInsert';
  cnstXMLCmdValue = 'value';
  cnstXmlCmdCustomDraw = 'CustomDraw';
  cnstXmlCmdAskOnDelete = 'AskOnDelete';
  cnstXmlCmdHeight = 'Height';
  cnstXmlCmdWeight = 'Weight';
  cnstXmlCmdWidth = 'Width';
  cnstXmlCmdDepth = 'Depth';
  cnstXmlCmdTop = 'Top';
  cnstXmlCmdLeft = 'Left';
  cnstXmlCmdRight = 'Right';
  cnstXmlCmdBottom = 'Bottom';
  cnstXmlCmdSupportedInstructionsList = 'SupportedInstructionsList';
  cnstXMLDiscriptionSupportedInstructionsList = 'Returns all command supported by XML interface';
  cnstXmlCmdSupportedCommandsList = 'SupportedCommandsList';
  cnstXmlSupportedClassesList = 'SupportedClassesList';
  cnstXMLCMdParam = 'Param';
  cnstXMLCMdDecription = 'Description';
  cnstXMLCMdInputParam = 'Input params';
  cnstXMLCMdReturningValues = 'Returning values';
  cnstXMLCmdButton = 'Button';
  cnstXMLCmdLButton = 'LBUTTON';
  cnstXMLCmdRButton = 'RBUTTON';
  cnstXMLCmdMButton = 'MBUTTON';
  cnstXMLCmdX = 'X';
  cnstXMLCmdY = 'Y';
  cnstXMLCmdShift = 'Shift';
  cnstXMLCmdKeyChar = 'Key';
  cnstXMLCmdWheelDelta = 'WheelDelta';
  cnstXmlCmdClipMode = 'ClipMode';
  cnstXmlCmdClipRect = 'ClipRect';
  cnstXMLCmdReport = 'Report';
  cnstXMLCmdGetData = 'GetData';
  cnstXMLCmdSetData = 'SetData';
  cnstXMLCmdFindTextEntities = 'FindText';
  cnstXMLCmdIterator = 'Iterator';
  cnstXMLCmdState = 'State';
  cnstXmlCmdEnable = 'Enable';
  cnstXmlExternalForm = 'ExternalForm';
  cnstXMLCmdCreateBtiEntity = 'CreateBTIEntity';
  cnstXMLCmdWaterMark = 'WaterMark';
  cnstXMLCmdSpecialLayers = 'SpecialLayers';
  cnstXMLCmdGlobalVariable = 'GlobalVariable';
  cnstXMLCmdAppMainFunction = 'AppCommand';
  cnstXMLCmdSaveHandles = 'GetSaveHandles';
  cnstXMLCmdAppCommand = 'AppFunction';
  cnstXMLCmdStack = 'Stack';

  cnstXmlCmdModeInterfacce = 'INTERFACE';

  cnstXmlCmdModeScreenToCad = 'SCREENTOCAD';
  cnstXmlCmdModeCadToScreen = 'CADTOSCREEN';
  cnstXmlCmdModeNearestPoint = 'NEARESTPOINT';

  cnstXMLCmdCommandDeleteError = 'Object with handle %s can not be removed';
  cnstXMLCmdCommandDeleteResult = 'Object with handle %s removed';

  cnstXMLCmdCommandRemoveErrorEntities = 'Has not attribute "handle"';
  cnstXMLCmdCommandRemoveErrorSource = 'Has not SOURCE';
  cnstXMLCmdCommandRemoveErrorDest = 'Has not DESTINATION';
  cnstXMLCmdCommandRemoveErrorSourceDestEqual = 'SOURCE and DESTINATION are equal';
  cnstXMLCmdEntityAddError = 'Object with handle %s can not be added';
  cnstXMLCmdEntityDelError = 'Object with handle %s can not be removed';
  cnstXMLCmdEntityFindError = 'Objects with handle %s can not be finded';

  //Errors message
  cnstXMLMsgUnsupportedCommand = 'Unsupported command';
  cnstXMLMsgUnsupportedFunction = 'Unsupported function';
  cnstXMLMessageUnsupportedCommand = cnstXMLMsgUnsupportedCommand + ' <%s>';
  cnstXMLMessageNotHasNode = 'Does not have node';
  cnstXMLMessageNotFound =  'The object is not found';
  cnstXMLMessageFilePathNotFound = 'The file path is not found';

  cnstXMLInputParamParamsGet = '[' + cnstXMLCmdHandle + ']';
  cnstXMLInputParamSelect = cnstXMLCmdHandle;

  cnstXMLInputParamApply = 'Any object''s attribute, that has no read-only modifier, can be used as a parameter';
  cnstXMLInputParamLoad = 'File name';
  cnstXMLInputParamUnload = cnstXMLCmdGuid+'|'+cnstXMLCmdCurrent;
  cnstXMLInputParamSwitch = cnstXMLCmdGuid;
  cnstXMLInputParamGetImage = cnstXMLCmdTop + ', ' + cnstXMLCmdLeft+
    ', ' + cnstXMLCmdRight + ', ' + cnstXMLCmdBottom +
    '[' + cnstXMLCmdWeight + ', ' + cnstXMLCmdHeight + ']';
  cnstXMLInputParamContextMenu = cnstXMLCmdMode;
  cnstXMLInputParamRibbon = cnstXMLCmdVisible;
  cnstXMLInputParamCommand = 'All supported command line';
  cnstXMLInputParamSelected = cnstXMLCmdHandle;
  cnstXMLInputParamSignToEvent = cnstXMLCmdEvent;
  cnstXMLInputParamUnselect = cnstXMLCmdHandle+'[' + cnstXMLCmdMarker + ']';
  cnstXMLInputParamSave = cnstXMLCmdFile +
    ', [ ' + cnstXMLCmdVersion + ', ' +
    cnstXMLCmdType + ', ' + cnstXMLCmdIsConvertImageToOle+']';
  cnstXMLInputParamCreateHatch = cnstXMLCmdHatchName +
    ', [ ' + cnstXMLCmdAngle + ', ' + cnstXMLCmdPatternScale +']';
  cnstXMLInputParamGetDrawingChanged = '';
  cnstXMLInputParamGetView = '';
  cnstXMLInputParamSetView = 'CircleZoomPercent, UCSOrigin, UCSXDir, UCSYDir, '+
    'UCSVP, ViewAspectRatio, ViewCenterPoint, ViewDirection, ViewTarget, ' +
    'ViewHeight, ViewTwistAngle';
  cnstXMLInputParamCustomSelectMode = cnstXMLCmdHandle+ ', ' +
    cnstXMLCmdCustomDraw+ ', ' +  cnstXMLCmdReadOnly+ ', ' + cnstXMLCmdAskOnDelete;
  cnstXMLInputParamEditor = cnstXMLCmdReadOnly+ ', ' +
    cnstXMLCmdShowEntityMethod + ', ' + cnstXMLCmdShowEntityPrecision;
  cnstXMLInputParamRegistration = cnstXMLCmdUser + ', ' +
    cnstXMLCmdEMail + ', ' + cnstXMLCmdKey;

  cnstXMLDiscriptionSupportedCommandsList = 'Returns all command supported by XML interface';
  cnstXMLDiscriptionSupportedClassesList = 'Returns classes of objects (lines, sections, layers, etc.) supported by the XML interface. All objects are returned with default values.';
  cnstXMLEmptyParams = 'The command has no params';
  cnstXMLHelp = 'Help';
  csntNestedStructure = 'NestedStructure';
  cnstExportParams = 'ExportParams';
  cntSelected = 'Selected';
  cnstGetExternalFiles = 'GetExternalFiles';
  cnstXMLInvalidate = 'Invalidate';

  cnstXMLErrorFormatType = 'Unsupported format.';

//create buttons event message
  cnstXMLNotEnteredPageName = 'Page name not entered';
  cnstXMLNotEnteredToolbarName = 'Toolbar name not entered';
  cnstXMLNotEnteredButtonCaption = 'Button caption not entered';
  cnstXMLNotEnteredMode = 'Mode(ADD/DEL) not entered';
  cnstXMLPageNotFound = 'Page not found';
  cnstXMLToolbarNotFound = 'Toolbar not found';
  cnstXMLButtonNotFound = 'Button not found';
  cnstXMLButtonAlreadyExist = 'Button already exists';
  cnstXMLButtonDelete = 'Button deleted';
  cnstXMLButtonAdded = 'Button added';
  cnstXMLButtonEnabled = 'Button is enable';
  cnstXMLButtonDisabled = 'Button is disable';
  cnstXMLAttributeNameNotFound = 'The Name attribute not found';
  cnstXMLBlockPatternInvalide = 'BlockPattern invalide';
  cnstXMLBlockPatternNotFound = 'BlockPattern not found';

  // Command line options
  cnstXMLOptionsName = 'Options';
  cnstXMLOptionCommandMode = 'CommandMode';
  cnstXMLOptionHideMainWindow = 'HideMainWindow';
  cnstXMLOptionNoCloseProgram = 'NoCloseProgram';

  cnstXMLOptions = 'Options';
  cnstXMLInterface = 'Interface';

type
  TXMLCommParameter = (cpDrawingData, cpHandle, cpFileName, cpCaption, cpText,
    cpParameters, cpShowEntityPrecision, cpShowEntityMethod, cpTop, cpLeft,
    cpRight, cpBottom, cpWidth, cpHeight, cpBase64, cpCalcs, cpItems, cpMode,
    cpReadOnly, cpCustomDraw, cpAskOnDelete, cpEnabled,
    {save params:} cpProportional, cpBitPerPixel, cpQuality, cpMeasureInPixels,
    cpTransparent, cpDPUX, cpDPUY, cpCompression, cpPageWidth, cpPageHeight,
    cpAuthor, cpLayoutExportMode, cpLayoutNameExportMode, cpVersion,
    cpIsConvertImageToOLE,{/save} cpMarker, cpPathToXMLExamples, cpPathname,
    {add <- in the end event and result} cpEvent, cpResult,
    cpSelectedCount, cpGuid, cpIndex, cpHatchName, cpAngle, cpPatternScale,
    cpColor, cpLineWeight, cpOutput, cpErrors, cpInstruction, cpItem,
    cpExportParams, cpBox, cpCreated, cpUpdated, cpChanged, cpSelected,
    cpFormat, cpDrawing, cpExternalFile, cpVisibleArea, cpCADArea, cpRect,
    cpUser, cpEMail, cpKey, cpRegistration, cpVisible, cpSetDrawingMode,
    cpDrawingMode, cpButtonCaption, cpPoint, cpSelectMode);

  TXMLCommParameters = set of TXMLCommParameter;

const

 cntKeyWords: array [TsgXMLKey] of record
    Name: string;
    Description: string;
  end = (
    (Name: 'Undefined'; Description:'';),//xkUndefined
    (Name: cnstXMLCmdGet; Description: 'The GET instruction returns properties of an entity specified by handle or the structure of the drawing if no parameters are given.'),//xkGet
    (Name: cnstXMLCmdAdd; Description: 'The ADD instruction adds entities and Drawing Structure Data to the current drawing.'),//xkAdd
    (Name: cnstXMLCmdSelect; Description: 'The SELECT Instruction selects entity(ies) by its(their) handle(s)'),//xkSelect
    (Name: cnstXMLCmdApply; Description: 'The APPLY instruction assigns new values to the entity attributes. The entity must be selected beforhand by using the Select Instruction, by mouse, etc.'),//xkApply
    (Name: cnstXMLCmdDelete; Description: 'The DELETE instruction deletes the selected entities.'),//xkDelete
    (Name: cnstXMLCmdLoad; Description: 'The LOAD instruction loads a file from the HDD or from a URL.'),//xkLoad
    (Name: cnstXMLCmdUnload; Description: 'Closes the specified drawing. The drawing can be specified by guid, index or current parameter.'),//xkUnload
    (Name: cnstXMLCmdSwitchDrawing; Description: 'Switches the active drawing. The drawing can be specified by guid or index.'),//xkSwitch
    (Name: cnstXMLCmdGetImage; Description: 'Returns am image of the current view in Base64 encoding.'),//xkGetImage
    (Name: cnstXMLCmdContextMenu; Description: 'Changes the context menu. ' + cnstXMLInputParamContextMenu),//xkContextMenu
    (Name: cnstXMLCmdRibbon; Description: 'Defines the ribbon visibility. Use the instruction with the on/off switch.'),//xkRibbon
    (Name: cnstXMLCmdCommand; Description: 'The Command instruction executes Command line with parameters described in the Text attribute.'),//xkInstruction
    (Name: cnstXMLCmdGetSelected; Description: 'The GETSELECTED Instruction returns the handles of the selected entities.'),//xkGetSelected
    (Name: cnstXMLCmdFittoSize; Description: 'The FITTOSIZE instruction fits the drawing to the display area proportionally.'),//xkFitToSize
    (Name: cnstXMLCmdSignToEvent; Description: 'The SignToEvent Instruction signs for particular events to accept callbacks with XML data.'),//xkSignToEvent
    (Name: cnstXMLCmdUnselect; Description: 'The UNSELECT instruction unselects entities which are specified by Handles.'),//xkUnSelect
    (Name: cnstXMLCmdGetViewRect; Description: 'Unsupported'),//xkGetViewRect,
    (Name: cnstXMLCmdSetViewRect; Description: 'Unsupported'),//xkSetViewRect,
    (Name: cnstXMLCmdGetBox; Description: 'Returns the 3D box that encloses the selected entities.'),//xkGetBox
    (Name: cnstXMLCmdShowSelectedEntities; Description: 'The SHOWSELECTEDENTITIES instruction zooms view to show the selected entities. Parameters are set in the <a href="#Editor">Editor</a> Instruction.'),//xkShowSelectedEntities
    (Name: cnstXMLCmdSave; Description: 'Saves the current drawing. The ExportParams attribute must be defined.'),//xkSave
    (Name: cnstXMLCmdCreateHatch; Description: 'The CREATEHATCH instruction creates Hatch using selected entities as contours. Please select an entity by mouse or by Select xml Instruction before calling this Instruction.'),//xkCreateHatch
    (Name: cnstXMLCmdGetDrawingChanged; Description: 'Returns a true or false value depending on the changes applied to the current drawing.'),//xkGetDrawingChanged,
    (Name: cnstXMLCmdGetView; Description: 'The GETVIEW instruction asks current view area as <a href="#cstVport">cstVport</a> data. Use with SETVIEW instruction.'),//xkGetView
    (Name: cnstXMLCmdSetView; Description: 'The SETVIEW instruction zooms the display area to given <a href="#cstVport">cstVport</a> data. Use with GETVIEW instruction.'),//xkSetView
    (Name: cnstXMLCmdCustomSelectMode; Description: 'The CUSTOMSELECTMODE instruction allows defining custom selections and turning on the read-only mode with a possibility to select certain entities.'),//xkCustomSelectMode
    (Name: cnstXMLCmdEditor; Description: 'The EDITOR instruction specifies parameters for the CAD editor.'),//xkEditor
    (Name: cnstXMLCmdRegistration; Description: 'The REGISTRATION instruction can be used to input a license key for CADSoftTools software.'),//xkRegistration
    (Name: cnstXMLCmdSetSpecialCustomInsert; Description: 'For special use.'),//xkSetSpecialCustomInsert
    (Name: cnstXmlCmdSupportedInstructionsList; Description: cnstXmlDiscriptionSupportedInstructionsList),//xkSupportedInstructionsList
    (Name: cnstXmlSupportedClassesList; Description: cnstXmlDiscriptionSupportedClassesList),//xkSupportedClassesList
    (Name: cnstXMLHelp; Description: 'Creates CAD XML API help in html format'),//xkHelp
    //structures
   // (Name: 'Item'; Description: 'Creates a context menu item with a caption from the parameter "caption". This caption is returned as result data when the item is clicked.'), //
    (Name: cnstXMLCmdMenuItemClick; Description: 'MenuItemClick is a result instruction only. it returns the caption of a clicked context menu item and its index.'), //
   // (Name: 'Changed'; Description: 'CHANGED is a result of the GETDRAWINGCHANGED instruction. CHANGED returns True if any changes were implemented in a Drawing.'),
  //  (Name: cntExportParams; Description: 'ExportParams is a child attribute of the SAVE instruction.'),
    //(Name: cntSelected; Description: 'SELECTED is a result node of the <a href=#GETSELECTED> GETSELECTED </a> instruction, which stores handles of the selected entities.'),
    (Name: cnstGetExternalFiles; Description: 'Returns a list of referenced files with paths.'),
    (Name: cnstXMLInvalidate; Description: 'Invalidates the current layout or area specified by the Box attribute.'),
    (Name: cnstXMLGetDrawingsList; Description: 'Returns guids for all opened drawings.'),
    (Name: cnstXMLCaptureScreen; Description: 'Unsupported.'),
    //(Name: 'Success'; Description: 'SUCCESS is a result node when ProcessXML is called successfully.')
    (Name: cnstXMLCmdSetDrawingChanged; Description: 'Set true or false value depending on changes were applied to the drawing by guid or Mode. If do not find GUID then Mode = 1(current drawing)'),
    (Name: cnstXMLCmdButtonMenu; Description: 'Add or delete button by tools palette'),
    (name: cnstXMLCmdGetCADCoords; Description: 'Convert point to cad coordinat system'),
    (name: cnstXMLCmdReport; Description: 'Generate data by report'),
    (name: cnstXMLCmdGetData; Description: 'Gets the data located by the definition of "Name"'),
    (name: cnstXMLCmdSetData; Description: 'Sets the data located by the definition of "Name"'),
    (name: cnstXMLCmdFindTextEntities; Description: 'Find text entities'),
    (name: cnstXMLCmdIterator; Description: 'Iterate drawing entities'),
    (name: cnstXMLCmdCreateBtiEntity; Description: 'Create BTI entities'),
    (name: cnstXMLCmdWaterMark; Description: 'Set or get watermark. Mode = 0 is "get". Mode = 1 is "set"'),
    (name: cnstXMLCmdSpecialLayers; Description: 'Set special layers'),
    (name: cnstXMLCmdGlobalVariable; Description: 'Get global variable'),
    (name: cnstXMLCmdSaveHandles; Description: 'Get Saved handles'),
    (name: cnstXMLCmdRemove; Description: 'The REMOVE statement removes objects.'),//xkRemove
    (name: cnstXMLCmdAppCommand; Description: 'The Function instruction executes application'),//for dll
    (name: cnstXMLCmdAppMainFunction; Description: 'The Command instruction executes application')//only in Enterprise
    );

const

 cnsTXMLCommParameters: array[TXMLCommParameter] of record
    Name: string;
    ValueType: TsgDataType;
    Description: string;
  end = (
    (Name: 'DrawingData';          ValueType: dtList;        Description: 'Drawing data structure'), //add, get
    (Name: 'Handle';               ValueType: dtEntHandle;   Description: 'Handle can be given directly via $ or indirectly via @ attributes, it also accepts a list of handles divided by the ";" sign. For example: Handle="$24", Handle="@1", Handle="@;$27;$28"'), //load, save
    (Name: 'File';                 ValueType: dtString;      Description: 'Path to the file'), //load, save
    (Name: cnstXMLCmdCaption;      ValueType: dtString;      Description: 'Caption of a menu item'),
    (Name: 'Text';                 ValueType: dtString;      Description: 'Text for the console command line, help, etc.'), // command line
    (Name: 'Parameters';           ValueType: dtString;      Description: 'Any object''s attribute that has no read-only modifiers can be used as a parameter'), //apply
    (Name: 'ShowEntityPrecision';  ValueType: dtDouble;      Description: 'Property of the ShowSelectEntities instruction, a value of the precision calculation.'), //
    (Name: 'ShowEntityMethod';     ValueType: dtByte;        Description: 'Property of the ShowSelectEntities instruction, the type of the precision calculation. Possible parameters are (0: Proportional, 1: Absolute, 2: FitToSize)'), //
    (Name: cnstXmlCmdTop;          ValueType: dtInteger;     Description: 'Top'), //
    (Name: cnstXmlCmdLeft;         ValueType: dtInteger;     Description: 'Left'), //
    (Name: cnstXmlCmdRight;        ValueType: dtInteger;     Description: 'Right'), //
    (Name: cnstXmlCmdBottom;       ValueType: dtInteger;     Description: 'Bottom'), //
    (Name: cnstXmlCmdWidth;        ValueType: dtInteger;     Description: 'Width'), //
    (Name: cnstXmlCmdHeight;       ValueType: dtInteger;     Description: 'Height'), //
    (Name: cnstXMLCmdBase64;       ValueType: dtBase64;      Description: 'Picture in Base64 encoding.'), //
    (Name: 'Calcs';                ValueType: dtList;        Description: 'Accepts <a href="#Calc">Calc</a> child nodes.'), //
    (Name: 'Items';                ValueType: dtList;        Description: 'Accepts <a href="#cmdItem">Item</a> child nodes.'), //
    (Name: 'Mode';                 ValueType: dtInteger;     Description: 'Context menu mode: 0: replace, 1: add to bottom, 2: add to top'), //
    (Name: 'ReadOnly';             ValueType: dtBool;        Description: 'Read-only status'), //
    (Name: 'CustomDraw';           ValueType: dtBool;        Description: 'The selected entities are drawn with custom color and lineweight'), //
    (Name: 'AskOnDelete';          ValueType: dtBool;        Description: 'If true, the library asks before deleting entities, whose handles are specified in the CUSTOMSELECTMODE parameters '), //
    (Name: 'Enabled';              ValueType: dtBool;        Description: 'Enabled (True) or Disabled (False)'), //
    {save}
    (Name: 'Proportional';         ValueType: dtBool;        Description: ''),
    (Name: 'BitPerPixel';          ValueType: dtByte;        Description: ''),
    (Name: 'Quality';              ValueType: dtDouble;      Description: ''),
    (Name: 'MeasureInPixels';      ValueType: dtBool;        Description: ''),
    (Name: 'Transparent';          ValueType: dtBool;        Description: 'Sets transparency for GIF'),
    (Name: 'DPUX';                 ValueType: dtByte;        Description: 'DPI X'),
    (Name: 'DPUY';                 ValueType: dtByte;        Description: 'DPI Y'),
    (Name: 'Compression';          ValueType: dtString;      Description: 'Compression for raster: LZW, Deflate, JPEG, CCITT3, CCITT4, CCITT6, Rle, Auto, None'),
    (Name: 'PageWidth';            ValueType: dtInteger;     Description: 'Page Width'),
    (Name: 'PageHeight';           ValueType: dtInteger;     Description: 'Page Height'),
    (Name: 'Author';               ValueType: dtString;      Description: 'Author'),
    (Name: 'LayoutExportMode';     ValueType: dtByte;        Description: 'Selects layouts to be exported: 0: Model; 1: AllLayouts; 2:LayoutByName; 3:AllPaperSpaces; 4:CurrentLayout.'),
    (Name: 'LayoutNameExportMode'; ValueType: dtString;      Description: 'Layout name, used if layouts export mode is "LayoutByName."'),
    (Name: 'Version';              ValueType: dtString;      Description: 'DWG or DXF version. For instance: acR2004, acR2000.'),
    (Name: 'IsConvertImageToOLE';  ValueType: dtBool;        Description: 'Converts external images to OLE in DWG/DXF export.'),
    {/save}
    (Name: 'Marker';               ValueType: dtBool;        Description: 'Shows (True) or does not show (False) editor markers for the selected entity.'), //
    (Name: 'PathToXMLExamples';    ValueType: dtString;      Description: 'The path to the parent XML examples directory for using them in the HTML Help.'), //
    (Name: 'PathName';             ValueType: dtString;      Description: 'A set of symbols showing the location of the named sections or objects in the Converter name space (which is similar to the DXF structure). As a separator the sign ";" is used.'), //
    (Name: 'Event';                ValueType: dtString;      Description: 'The accepted event names are: <a href="#OnSelectEntity"> "OnSelectEntity" </a>, <a href="#OnMouseDown">"OnMouseDown"</a> , <a href="#OnConfirmEntitiesDeletion">OnConfirmEntitiesDeletion</a>.'), //
    (Name: 'Result';               ValueType: dtList;        Description: 'Result data.'), //
    (Name: 'SelectedCount';        ValueType: dtInteger;     Description: 'The number of the selected entities'),
    (Name: 'Guid';                 ValueType: dtString;      Description: 'Drawing Guid'),
    (Name: 'Index';                ValueType: dtInteger;     Description: 'Drawing Index'),
    (Name: 'HatchName';            ValueType: dtString;      Description: 'Hatch name'), //
    (Name: 'Angle';                ValueType: dtDouble;      Description: 'Angle'),
    (Name: 'PatternScale';         ValueType: dtDouble;      Description: 'Pattern scale'),
    (Name: 'Color';                ValueType: dtColorCAD;    Description: 'Color CAD'),
    (Name: 'LineWeight';           ValueType: dtDouble;      Description: 'Line weight'),
    (Name: 'Output';               ValueType: dtList;        Description: 'Contains the result of the command'),
    (Name: 'Errors';               ValueType: dtList;        Description: 'Contains the errors of the command'),
    (Name: 'Instruction';          ValueType: dtString;      Description: 'The name of the command which the result is for'),
    (Name: 'Item';                 ValueType: dtListItem;    Description: 'Creates a context menu item with the caption from parameter "caption". This caption is returned as result data when the item is clicked.'),
    (Name: cnstExportParams;       ValueType: dtList;        Description: 'ExportParams is a child attribute of the SAVE instruction.'),
    (Name: 'Box';                  ValueType: dtTFRect;      Description: 'Box '),
    (Name: 'Created';              ValueType: dtListItem;    Description: 'Created '),
    (Name: 'Updated';              ValueType: dtListItem;    Description: 'Updated '),
    (Name: 'Changed';              ValueType: dtBool;        Description: 'CHANGED returns True if any changes were implemented in a Drawing.'),
    (Name: 'Selected';             ValueType: dtListItem;    Description: 'Stores handles of the selected entities'),
    (Name: 'Format';               ValueType: dtString;      Description: 'Output file format'),
    (Name: 'Drawing';              ValueType: dtListItem;    Description: 'Drawing item'),
    (Name: 'ExternalFile';         ValueType: dtListItem;    Description: 'External file item'),
    (Name: 'VisibleArea';          ValueType: dtListItem;    Description: 'VisibleArea item'),
    (Name: 'CADArea';              ValueType: dtListItem;    Description: 'CADArea item'),
    (Name: 'Rect';                 ValueType: dtTFRect;      Description: 'Rect'),
    (Name: 'User';                 ValueType: dtString;      Description: 'User'),
    (Name: 'EMail';                ValueType: dtString;      Description: 'EMail'),
    (Name: 'Key';                  ValueType: dtString;      Description: 'Key'),
    (Name: 'Registration';         ValueType: dtInteger;     Description: 'Registration'),
    (Name: 'Visible';              ValueType: dtBool;        Description: 'Visible'),
    (Name: 'Mode';                 ValueType: dtInteger;     Description: '0 - by guid, 1 - current drawing, 2 - all drawings'),
    (Name: 'Mode';                 ValueType: dtInteger;     Description: 'Full drawing mode'),
    (Name: 'Caption';              ValueType: dtString;      Description: 'Caption of button'),
    (name: 'Point';                ValueType: dtFPoints2DOr3D; Description: 'points for conversion to cad coordinate system'),
    (name: 'Mode';                 ValueType: dtInteger;     Description: '0 - set, 1 - remove, 2 - add. default value = 0')
  );

  cnstTypeSaveFormat: array[0..1]
    of record
    TypeFormat: string;
    IsDWG: Boolean;
  end = (
    (TypeFormat:'dwg'; IsDWG: True),
    (TypeFormat:'dxf'; IsDWG: False)
    );

  cnstVersionFile: array[TsgDWGVersion]
    of string =('acR09', 'acR10', 'acR11', 'acR12', 'acR13', 'acR14', 'acR2000',
    'acR2004', 'acR2007', 'acR2010', 'acR2013', 'acR2018');

  cnstDecimalSeparatorNames = cnstCommaPoint;

type
  TsgSupportDWGVersion = set of TsgDWGVersion;

const
  cnstSupportDWGVersion: TsgSupportDWGVersion = [acR2000, acR2004];

function CheckChildsEED(const AChild, AFilterChild: TsgNode; const ACheckEED: Boolean = True): Boolean;

implementation

{$IFDEF SG_DELPHI_VCL}
uses
{$IFDEF SGAPPPROTECT}
  AppProtect,
{$ENDIF}
{$IFDEF SG_ABVIEWER}
  Constants, sgTranslateModule,
{$ENDIF}
{$IFNDEF SG_NON_WIN_PLATFORM}
  ActiveX
{$ENDIF}
  ;
{$ELSE}
  {$IFDEF SGAPPPROTECT}uses AppProtectMultiplatform;{$ENDIF}
{$ENDIF}

type
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgDXFInsertAccess = class(TsgDXFInsert);
  TsgDXFConverterAccess = class(TsgDXFConverter);
{$IFDEF CS_USEFORM}
  TsgDrawingNavigatorAccess = class(TsgDrawingNavigator);
{$ENDIF}
  TsgDXFVportAccess = class(TsgDXFVport);
  TsgCADImageAccess = class(TsgCADImage);

  TsgXMLIDEIteateEntitiesMethod = procedure(const AEntities: TList) of object;

  TsgXMLIDEIteateEntities = class
  private
    FCADParams: TsgCADIterate;
    FEntities: TList;
    FInserts: TList;
    FIsUseFilter: Boolean;
    FFindValue: string;
    FFindHandle: UInt64;
    FImage: TsgCADImage;
    FMode: Integer;
    FNode: TsgNodeSample;
    FOutput: TsgNode;
    FParentNode: TsgNode;
    FXMLIDE: TsgXMLIDE;
    function ApplyWithFilter(AEntity: TsgDXFEntity): Integer;
    function ApplyXMLForEntity(AEntity: TsgDXFEntity): Integer;
    function FindEntityByHandle(AEntity: TsgDXFEntity): Integer;
    function FindEntityEnd(AEntity: TsgDXFEntity): Integer;
    procedure FindTextMethod(const AEntities: TList);
    procedure IteratorEntitiesMethod(const AEntities: TList);
    procedure CalcEntititesBox(const AOnlyBox: Boolean);
  protected
    function IterateInternal(ANode: TsgNodeSample; AResult: IsgResultNode;
      const AFlags: Integer; AMethod: TsgXMLIDEIteateEntitiesMethod): TsgNode;
  public
    constructor Create(AXMLIDE: TsgXMLIDE);
    destructor Destroy; override;
    function FindText(ANode: TsgNodeSample; AResult: IsgResultNode): TsgNode;
    function FindEntity(ANode: TsgNodeSample; AResult: IsgResultNode): TsgNode;
    function IteratorEntities(ANode: TsgNodeSample; AResult: IsgResultNode): TsgNode;
  end;

const
  cnstKeyNotUsedCADEditor: TsgXMLKeys = [xkUndefined, xkLoad,
    xkContextMenu, xkRibbon, xkCommand, xkRegistration, xkSupportedCommandsList,
    xkSupportedClassesList, xkHelp, (*xkItem,*) xkMenuItemClick, xkGetDrawingsList,
    xkGetViewRect, xkSetViewRect, xkMenuButton, xkUnload, xkGetData, xkSetData, xkWatermark,
    xkSwitchDrawing];

var
  KeyWords: TsgStringList = nil;
  AppKeyWords: TsgXMLKeys = [];

function _EOFTrial: Boolean;
begin
  Result :=
{$IFDEF SGAPPPROTECT}
   {$IFNDEF SG_DELPHI_VCL}AppProtectMultiplatform.{$ELSE}AppProtect.{$ENDIF}EOFTrial;
{$ELSE}
    False;
{$ENDIF}
end;

{ TsgXMLIDE }

procedure DoMessageError(const ANodeResult: TsgNode; const AMessage: string;
  const ANode: TsgNodeSample);
var
  vError: TsgNode;
begin
  vError := ANodeResult.AddChildNV(cnstXMLError);
  vError.AddAttribNV(cnstXMLCmdMessage, AMessage);
  if Assigned(ANode) and ANode.HasPosition then
    vError.AddAttribNV(cnstXMLPosition).ValueData.ValueAsPoint :=
      Point(ANode.Position.X, ANode.Position.Y);
end;

procedure MakeMessageText(const AResultList: TStringList;
  const AResCode: Integer; AResult: IsgResultNode);
var
  vResStr,vTmpStr: string;
  I: Integer;
begin
  vResStr := '';
  for I := 0 to AResultList.Count - 1 do
  begin
    vTmpStr := AResultList[I];
    if (AResultList.Count > 1) and (Pos(cnstSpace, vTmpStr) > 0) then
      vTmpStr := cnstQuotationMark + vTmpStr + cnstQuotationMark;
    vResStr := vResStr + vTmpStr;
  end;
  if AResCode > 0 then
    AResult.Errors.AddChildNV(cnstXMLError).AddAttribNV(cnstXMLCmdMessage,
      vResStr)
  else
    if AResultList.Text <> '' then
      AResult.Output.AddAttribNV(cnstXMLCmdResult).ValueAsStr := vResStr;
end;

function ShiftToStr(const AShift: TShiftState): string;
var
  vShift: Integer absolute AShift;
begin
  if AShift = [] then
    Result := ''
  else
    Result := IntToStr(vShift);
end;

procedure AddAttribShift(const ANode: TsgNode; const AShift: TShiftState);
var
  vValue: string;
begin
  vValue := ShiftToStr(AShift);
  if Length(vValue) > 0 then
    ANode.AddAttribNV(cnstXMLCmdShift).ValueAsStr := vValue;
end;

function GetMarkerSelectMode(const ANode: TsgNodeSample): Boolean;
var
  vAttrib: TsgNodeSample;
begin
  Result := {$IFDEF SG_CADIMAGE_DLL.DLl}False{$ELSE}True{$ENDIF};
  vAttrib := ANode.GetAttributeByName(cnstXMLCmdMarker);
  if Assigned(vAttrib) then
    Result := vAttrib.ValueAsBool;
end;

procedure DrawingXMLDataToNode(const AData: TsgDrawingXMLData;
  const ANode: TsgNode);
begin
  ANode.AddAttribNV(cnstXMLCmdGuid).ValueAsStr := GuidToStr(AData.Guid);
  ANode.AddAttribNV(cnstXMLCmdFile).ValueAsStr := AData.Name;
  ANode.AddAttribNV(cnstXMLCmdIndex).ValueAsInt := AData.Index;
  ANode.AddAttribNV(cnstXMLCmdMode).ValueAsInt := AData.Mode;
  ANode.AddAttribNV(cnstXMLCmdCurrent).ValueAsBool := AData.Current;
  if AData.State <> 0 then
    ANode.AddAttribNV(cnstXMLCmdState).ValueAsInt := AData.State;
end;

procedure DrawingXMLDataFromNode(const ANode: TsgNode; var AData: TsgDrawingXMLData);
begin
  FillChar(AData, SizeOf(AData), 0);
  AData.Guid := StrToGUID(ANode.GetAttributeByName(cnstXMLCmdGuid).ValueAsStr);
  AData.Name := GetAttributeStr(ANode, cnstXMLCmdFile);
  AData.Index := GetAttributeInt(ANode, cnstXMLCmdIndex, -1);
  AData.Mode := GetAttributeInt(ANode, cnstXMLCmdMode, 0);
end;

function TsgXMLIDE.AddSelectEntity(AEntity: TsgDXFEntity;
  const ANode: TsgNodeSample): Boolean;
var
  vAddEntity: TList;
begin
  vAddEntity := TList.Create;
  try
    vAddEntity.Add(AEntity);
    Result := SetSelectEntities(vAddEntity, ANode, 2);
  finally
    vAddEntity.Free;
  end;
end;

function CheckAttribs(const AChild, AFilterChild: TsgNode;
  const ACheckEED: Boolean = True): Boolean;
var
  J, K: Integer;
  vAttrib, vFilterAttrib: TsgNodeSample;
begin
  Result := False;
  if Assigned(AChild) then
  begin
    K := 0;
    for J := 0 to AFilterChild.AttributeNodesCount - 1 do
    begin
      vFilterAttrib := AFilterChild.AttributeNodes[J];
      vAttrib := AChild.GetAttributeByName(vFilterAttrib.Name);
      if not Assigned(vAttrib) then
        Break;
      if not vFilterAttrib.ValueData.IsEqual(vAttrib.ValueData) then
        Break;
      Inc(K);
    end;
    Result := K = AFilterChild.AttributeNodesCount;
    if Result and ACheckEED and (AFilterChild.ChildNodesCount > 0) then
      Result := CheckChildsEED(AChild, AFilterChild);
  end;
end;

function CheckChildsEED(const AChild, AFilterChild: TsgNode; const ACheckEED: Boolean = True): Boolean;
var
  I, J, M, K: Integer;
  vEed, vFilterEed, vEedAppName, vFilterEedAppName, vEedItem, vFilterEedItem: TsgNodeSample;
  vAppName: string;
begin
  Result := True;
  vFilterEed := AFilterChild.GetChildByName(cnstXMLNames[xmlEedList].Name);
  if Assigned(vFilterEed) then
  begin
    vEed := AChild.GetChildByName(cnstXMLNames[xmlEedList].Name);
    if Assigned(vEed) then
    begin
      for I := 0 to vFilterEed.ChildNodesCount - 1 do
      begin
        vFilterEedAppName := vFilterEed.ChildNodes[I];
        vAppName := GetAttributeStr(vFilterEedAppName, cnstXMLNames[xmlName].Name, '');
        if Length(vAppName) > 0 then
        begin
          vEedAppName := TsgNode(vEed).GetNodeByAttribValue(cnstXMLNames[xmlApplication].Name,
            cnstXMLNames[xmlName].Name, vAppName);
          if Assigned(vEedAppName) then
          begin
            K := 0;
            for J := 0 to vFilterEedAppName.ChildNodesCount - 1 do
            begin
              vFilterEedItem := vFilterEedAppName.ChildNodes[J];
              for M := 0 to vEedAppName.ChildNodesCount - 1 do
              begin
                vEedItem := vEedAppName.ChildNodes[M];
                if CheckAttribs(TsgNode(vFilterEedItem), TsgNode(vEedItem), False) then
                begin
                  Inc(K);
                  Break;
                end;
              end;
            end;
            if K <> vFilterEedAppName.ChildNodesCount then
              Result := False;
          end
          else
            Result := False;
        end;
        if not Result then
          Break;
      end;
    end
    else
      Result := False;
  end;
end;

procedure SelectWithFilter(const AFilter: TsgNode; const AEntity: TsgDXFEntity;
  const ASelectEntities: TList; AParentNode: TsgNode);
const
  cnstXMLMode: TsgXMLModes = [xmAddSubEntities, xmlGetDefaultValue];
var
  vEntityNode, vFilterChild: TsgNode;
  I: Integer;
  vEntityXMLName: string;
begin
  vEntityNode := nil;
  AParentNode.Clear;
  if AFilter.ChildNodesCount > 0 then
  begin
    vEntityXMLName := TsgDXFEntityAccess(AEntity).GetXMLName;
    for I := 0 to AFilter.ChildNodesCount - 1 do
    begin
      vFilterChild := TsgNode(AFilter.ChildNodes[I]);
      if SameText(vFilterChild.Name, vEntityXMLName) then
      begin
          AEntity.ToXML(AParentNode, cnstXMLMode);
          vEntityNode := TsgNode(AParentNode.ChildNodes[0]);
          if CheckAttribs(vEntityNode, vFilterChild) then
            Break
          else
            vEntityNode := nil;
      end;
    end;
  end
  else
  begin
    AEntity.ToXML(AParentNode, cnstXMLMode);
    vEntityNode := TsgNode(AParentNode.ChildNodes[0]);
  end;
  if Assigned(vEntityNode) then
  begin
    if CheckAttribs(vEntityNode, AFilter) then
      ASelectEntities.Add(AEntity);
  end;
end;

procedure TsgXMLIDE.ApplyFilter(const AFilter: TsgNode; const AEntities: TList);
var
  I, J: Integer;
  vSelect: TList;
  vParentNode: TsgNode;
  vEntity: TsgDXFEntity;
begin
  if AEntities.Count < 1 then
    Exit;
  vSelect := TList.Create;
  vParentNode := TsgNode.Create;
  try
    vSelect.Capacity := AEntities.Count;
    for I := 0 to AEntities.Count - 1 do
    begin
      vEntity := AEntities.List[I];
      for J := 0 to vEntity.Count - 1 do
        SelectWithFilter(AFilter, vEntity.Entities[J], vSelect, vParentNode);
    end;
    AEntities.Count := 0;
    CopyLists(AEntities, vSelect);
  finally
    vParentNode.Free;
    vSelect.Free;
  end;
end;

function TsgXMLIDE.CreateResultEvent(const AName: string): TsgNode;
begin
  Result := TsgNode.Create;
  Result.Name := cnstXMLResult;
  Result.AddAttribNV(cnstXMLInstruction).ValueAsStr := AName;
  Result.AddAttribNV(cnstXMLType).ValueAsStr := cnstXMLTypeEvent;
end;

procedure TsgXMLIDE.ClearParser;
begin
  if RootOut.HasChildNodes then
    RootOut.Childs.ClearNodes;
end;

procedure TsgXMLIDE.AddExceptionToNode(const ANode: TsgNode;
  const AException: Exception);
var
  vErrors: TsgNode;
begin
  vErrors := TsgNode(ANode.GetChildByName(cnstXMLErrors));
  if not Assigned(vErrors) then
    vErrors := ANode.AddChildNV(cnstXMLErrors);
  DoMessageError(vErrors, AException.{$IFDEF SGDEL_2009}ToString{$ELSE}ClassName{$ENDIF}, ANode);
end;

procedure TsgXMLIDE.AddAttribsNoUseInFilter(const ANode: TsgNode;
  const Attribs: TList);
var
  I: Integer;
begin
  for I := 0 to Attribs.Count -  1 do
    ANode.AddAttrib(Attribs[I]);
  Attribs.Count := 0;
end;

procedure TsgXMLIDE.ExpEntitiesToXml(const AEntities: TList; AOut: TsgNode;
  const AMode: TsgXMLModes = cnstDefaultXMLMode);
var
  I: Integer;
  vBox: TFRect;
  vEntity: TsgDXFEntity;
begin
  vBox := cnstBadRect;
  for I := AEntities.Count - 1 downto 0 do
  begin
    vEntity := TsgDXFEntity(AEntities[I]);
    vEntity.ToXML(AOut, AMode);
    UnionFRect(vBox, vEntity.Box);
  end;
  if not IsBadRect(vBox) then
    AOut.AddAttribNV(cnstXMLCmdBox).ValueAsFRect := vBox;
end;

function TsgXMLIDE.ExtractAttribsNoUseInFilter(const ANode: TsgNode;
  const Attribs: TList): Boolean;
const
  cnstExtractAttribs: array [0..6] of string = (cnstXMLCmdPathName,
    cnstXMLCmdHandle, cnstXMLCmdMarker, cnstXMLCmdMode, cnstXMLCmdValue,
    cnstXMLCmdXREFPath, cnstXMLCmdPathKey);

  procedure Extract(const AName: string);
  var
    vAttrib: TsgNodeSample;
  begin
    vAttrib := ANode.GetAttributeByName(AName);
    if Assigned(vAttrib) then
    begin
      ANode.RemoveAttrib(vAttrib);
      Attribs.Add(vAttrib);
    end;
  end;

var
  I: Integer;
begin
  for I := Low(cnstExtractAttribs) to High(cnstExtractAttribs) do
    Extract(cnstExtractAttribs[I]);
  Result := ANode.AttributeNodesCount + ANode.ChildNodesCount > 0;
end;

procedure TsgXMLIDE.AddEntitiesToNode(const ANode: TsgNode;
  const Image, AEntities: TObject; const AType: TsgXMLEvent);

  procedure AddEntity(const AEntity: TsgDXFEntity);
  var
    vNode: TsgNode;
    vEntity: TObject;
  begin
    vEntity := GetEntityByPathKey(AEntity);
    if Assigned(vEntity) then
    begin
      vNode := ANode.AddChildNV(cnstXMLCmdSelected, '');
      vNode.AddAttribNV(cnstXMLCmdHandle).ValueAsHandle := TsgDXFEntity(vEntity).Handle;
      if AEntity is TsgObjEntity3D then
      begin
        // Since the result of the event is transmitted through copying rows,
        // we cannot transfer large volumes.
        // To get the full information, you need to use Get
        TsgObjEntity3D(AEntity).ToXML(vNode,
          [xmAddSubEntities, xmNoSubEntitiesNode, xmOnlyChildNodes, xmlForViewing,
           xmlModMeshes, xmlModGeometry]);
      end;
      case AType of
        xeOnSelectBTIEntities:
          begin
            TsgDXFInsertAccess(vEntity).ToXMLNode(vNode, CreateXMLParams);
          end;
      end;
    end;
  end;

var
  I, vCount: Integer;
  vList: TList absolute AEntities;
begin
  vCount := 0;
  if AEntities is TList then
  begin
    vCount := vList.Count;
    for I := 0 to vCount - 1 do
      AddEntity(TsgDXFEntity(vList[I]));
  end
  else
  begin
    if AEntities is TsgDXFEntity then
    begin
      vCount := 1;
      AddEntity(TsgDXFEntity(AEntities));
    end;
  end;
  ANode.AddAttribNV(cnstXMLCmdSelectedCount).ValueAsInt := vCount;
end;

procedure TsgXMLIDE.AddEntityToSelectHandles(const AEntity: TsgDXFEntity);
begin
  if AEntity.Handle = cnstBadHandle then
    FSelectedHandles.Add(cnstBadHandle, AEntity)
  else
    FSelectedHandles.Add(AEntity.Handle, nil);
end;

function TsgXMLIDE.CheckXMLInterfaceVersion(const AMainNode: TsgNodeSample): Boolean;
var
  vNodeVersion: TsgNodeSample;
  vVersion: TsgVersion;
begin
  Result := False;
  if Assigned(AMainNode) then
  begin
    vNodeVersion := AMainNode.GetAttributeByName(cnstXMLCmdVersion);
    if Assigned(vNodeVersion) then
    begin
      vVersion := vNodeVersion.ValueData.ValueAsVersion;
      Result := vVersion.Major = cnstXMLIntfaceVersion.Major;
    end;
  end;
end;

constructor TsgXMLIDE.Create;
begin
  inherited Create;
  FInstructions := nil;
  FXMLInp := TsgParser.Create;
  FCADEditor := nil;
  FXMLCommParameterList := TStringList.Create;
{$IFDEF CS_USEFORM}
  FOnApplicationExeptionPrev := Application.OnException;
  Application.OnException := OnApplicationExeption;
{$ENDIF}
end;

procedure TsgXMLIDE.CommandCustomSelectMode(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  I: Integer;
  vEntity: TsgDXFEntity;
  vList: TsgInt64List;
  vEntities: TList;

  procedure SetMode(const ANode: TsgNodeSample; const AMode: TsgEntityMode;
    var AModes: TsgEntityModes; var AUnregister: Boolean);
  begin
    if Assigned(ANode) then
    begin
      AUnregister := False;
      if ANode.ValueAsBool then
        Include(AModes, AMode)
      else
        Exclude(AModes, AMode);
    end;
  end;

  procedure SetCustomSelectModeForHandle(ANodeSample: TsgNodeSample; AHandle: Int64);
  var
    vNodeAttrib, vAttribCustomDraw, vAttribReadOnly, vAttribAskOnDelete: TsgNodeSample;
    vCustomDraw: TsgCustomSelectMode;
    vUnregister: Boolean;
  begin
    vCustomDraw := CADEditor.GetCustomDrawParams(AHandle);
    if (vCustomDraw.Mode = []) and (AHandle <> cnstBadHandle) then
    begin
      vCustomDraw := CADEditor.GetCustomDrawParams(cnstBadHandle);//set default params
      vCustomDraw.Mode := [];
    end;
    vNodeAttrib := ANode.GetAttributeByName(cnstXMLNames[xmlColor].Name);
    if Assigned(vNodeAttrib) then
      vCustomDraw.Color := ConvertColorCADToRGB(StrToColorCAD(vNodeAttrib.ValueAsStr));
    vNodeAttrib := ANode.GetAttributeByName(cnstXMLNames[xmlLineWeight].Name);
    if Assigned(vNodeAttrib) then
      vCustomDraw.LineWidth := Round(vNodeAttrib.ValueAsDouble);
    vAttribCustomDraw := ANode.GetAttributeByName(cnstXmlCmdCustomDraw);
    vAttribReadOnly := ANode.GetAttributeByName(cnstXmlCmdReadOnly);
    vAttribAskOnDelete := ANode.GetAttributeByName(cnstXmlCmdAskOnDelete);
    vUnregister := True;
    SetMode(vAttribCustomDraw, emCustomDraw, vCustomDraw.Mode, vUnregister);
    SetMode(vAttribReadOnly, emReadOnly, vCustomDraw.Mode, vUnregister);
    SetMode(vAttribAskOnDelete, emAskOnDelete, vCustomDraw.Mode, vUnregister);
    FCADEditor.SetCustomDrawParams(AHandle, vCustomDraw, vUnregister);
  end;

begin
  if not Assigned(CADEditor) then
    Exit;

  vList := TsgInt64List.Create;
  vEntities := TList.Create;
  try
    vEntity := GetEntityByNode(ANode, AResult.Errors, vList, vEntities);
    if Assigned(vEntity) then
    begin
      if vEntities.Count > 1 then
      begin
        for I := vEntities.Count - 1 downto 0 do
        begin
          SetCustomSelectModeForHandle(ANode, TsgDXFEntity(vEntities[I]).Handle);
        end;
      end
      else
      begin
        SetCustomSelectModeForHandle(ANode, vEntity.Handle);
      end;
    end
    else
      SetCustomSelectModeForHandle(ANode, cnstBadHandle);
  finally
    vList.Free;
    vEntities.Free;
  end;
end;

procedure TsgXMLIDE.CommandCreateHatch(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vHatch: TsgCADHatch;
  vNode: TsgNode;
  vNodeAttr: TsgNodeAttrib;
  vColor, vColor2: TsgColorCAD;
  vPColor1, vPColor2: PsgColorCAD;
  vScale, vAngle: Double;
  vHatchName: string;
begin
  if IsInvalidRefs then
    Exit;
  vHatchName := sSOLID;
  vNodeAttr := TsgNodeAttrib(ANode.GetAttributeByName(cnstXMLCmdHatchName));
  if Assigned(vNodeAttr) and (Length(vNodeAttr.ValueAsStr) > 0) then
    vHatchName := vNodeAttr.ValueAsStr;
  vAngle := 0;
  vScale := 1;
  if Assigned(ANode.GetAttributeByName(cnstXMLCmdAngle)) then
    vAngle := ANode.GetAttributeByName(cnstXMLCmdAngle).ValueAsDouble;
  if Assigned(ANode.GetAttributeByName(cnstXMLCmdPatternScale)) then
    vScale := ANode.GetAttributeByName(cnstXMLCmdPatternScale).ValueAsDouble;
  vPColor1 := nil;
  vPColor2 := nil;
  vNodeAttr := TsgNodeAttrib(ANode.GetAttributeByName(cnstXMLNames[xmlColor].Name));
  if Assigned(vNodeAttr) then
  begin
    vColor := StrToColorCAD(vNodeAttr.ValueAsStr);
    vPColor1 := @vColor;
  end;

  vNodeAttr := TsgNodeAttrib(ANode.GetAttributeByName(cnstXMLNames[xmlGradientOneColor].Name));
  if Assigned(vNodeAttr) then
  begin
    vColor := StrToColorCAD(vNodeAttr.ValueAsStr);
    vPColor1 := @vColor;
  end;

  vNodeAttr := TsgNodeAttrib(ANode.GetAttributeByName(cnstXMLNames[xmlGradientTwoColor].Name));
  if Assigned(vNodeAttr) then
  begin
    vColor2 := StrToColorCAD(vNodeAttr.ValueAsStr);
    vPColor2 := @vColor2;
  end;

  vHatch := TsgCADHatch(CADEditor.CreatingHatch(vHatchName, vScale, vAngle,
    vPColor1, vPColor2));
  if Assigned(vHatch) then
  begin
    CADImage.Converter.Loads(vHatch);
    vNode := AResult.Output.AddChildNV(cnstXMLCmdSelected);
    vNodeAttr := vNode.AddAttribNV(cnstXMLCmdHandle, '');
    vNodeAttr.ValueAsHandle := vHatch.Handle;
  end
  else
  begin
    vNode := AResult.Errors.AddChildNV(cnstXMLError);
    vNode.AddAttribNV(cnstXMLMessage, sXMLInterfaceHatchCreatingError);
  end;
end;

{$IFDEF SGABVIEWER}
procedure TsgXMLIDE.CommandGetData(ANode: TsgNodeSample;
  AResult: IsgResultNode);
begin
  if MainApplication <> nil then
    MainApplication.CommandGetData(SaveNodeToXMLString(ANode), Self);
end;

procedure TsgXMLIDE.CommandSetData(ANode: TsgNodeSample;
  AResult: IsgResultNode);
begin
  if MainApplication <> nil then
    MainApplication.CommandSetData(SaveNodeToXMLString(ANode), Self);
end;
{$ENDIF}
{$IFDEF SG_BTI_INTERFACE}
procedure TsgXMLIDE.CommandCreateBtiEntity(ANode: TsgNodeSample;
  AResult: IsgResultNode);
begin
  if MainApplication <> nil then
    MainApplication.CommandCreateBtiEntity(SaveNodeToXMLString(ANode), Self);
end;

{$ENDIF}

procedure TsgXMLIDE.CommandGetDrawingChanged(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNode;
  I: Integer;
  vXmlGuid: TsgDrawingXMLData;
begin
  if not Assigned(CADEditor) then
    Exit;
  vNode := AResult.Output;
  vNode.AddAttribNV(cnstXMLCmdChanged).ValueAsBool := CADEditor.IsChanged;
  if CheckForApp(ANode, AResult) and (MainApplication.DrawingsCount > 0) then
  begin
    for I := 0 to MainApplication.DrawingsCount - 1 do
    begin
      vXmlGuid := MainApplication.DrawingGuid[I];
      if vXmlGuid.Current then
      begin
        vNode.AddAttribNV(cnstXMLCmdMode).ValueAsInt := vXmlGuid.Mode;
        vNode.AddAttribNV(cnstXMLCmdState).ValueAsInt := vXmlGuid.State;
        Break;
      end;
    end;
  end;
end;

procedure TsgXMLIDE.CommandSetDrawingChanged(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNodeChange, vNodeMode, vNodeGuid: TsgNodeSample;
  vMode: Integer;
  vGuid: TGUID;
begin
  if not Assigned(CADEditor) then
    Exit;
  vNodeChange := ANode.GetAttributeByName(cnstXMLCmdChanged);
  if Assigned(vNodeChange) then
  begin
    vMode := 0;
    vGuid := cnstFailNewDrawing;
    vNodeMode := ANode.GetAttributeByName(cnstXMLCmdMode);
    if Assigned(vNodeMode) then
      vMode := vNodeMode.ValueAsInt;
    vNodeGuid := ANode.GetAttributeByName(cnstXMLCmdGuid);
    if Assigned(vNodeGuid) then
      vGuid := StrToGUID(vNodeGuid.ValueAsStr)
    else
    begin
      if vMode = 0 then
        vMode := 1;
    end;
    MainApplication.SetDrawingChange(vGuid, vMode, vNodeChange.ValueAsBool);
  end
  else
    DoMessageError(AResult.Errors, cnstXMLInterfaceSetValueNotFoundError, ANode);
end;

procedure TsgXMLIDE.CommandGetExternalFiles(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  I: Integer;
  vList: TStringList;
  vNode: TsgNode;
begin
  if IsInvalidRefs then
    Exit;
  vList := TStringList.Create;
  try
    FCADImage.GetExternalFiles(vList);
  finally
    if vList.Count = 0 then
    begin
      vNode  :=  AResult.Errors.AddChildNV(cnstXMLError);
      vNode.AddAttribNV(cnstXMLMessage).Value := sXMLInterfaceFileNotFoundError;
    end
    else
      vNode := AResult.Output;
      for I := 0 to vList.Count - 1 do
        vNode.AddChildNV(cnstXMLExternalFile).
          AddAttribNV(cnstXMLFileName).ValueAsStr := vList[I];
    vList.Free;
  end;
end;

procedure TsgXMLIDE.CommandGetView(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNode;
  vVPort: TsgDXFVport;
begin
  if IsInvalidRefs then
    Exit;
  vVPort :=  TsgCADImageAccess(FCADImage).CreateDefaultVPort(0);
  try
  vNode := vVPort.ToXML(AResult.Output);
  vNode.RemoveAttributeByName(cnstXMLNames[xmlHandle].Name);
  vNode.RemoveAttributeByName(cnstXMLNames[xmlName].Name);
  finally
    vVPort.Free;
  end;
end;

procedure TsgXMLIDE.CommandGetViewRect(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNode;
begin
  if MainApplication <> nil then
  begin
    vNode := AResult.Output.AddChildNV('VisibleArea');
    vNode.AddAttribNV(cnstXMLHeight).ValueAsInt := GetRectHeight(MainApplication.GetViewRect);
    vNode.AddAttribNV(cnstXMLWidth).ValueAsInt := GetRectWidth(MainApplication.GetViewRect);
    vNode := AResult.Output.AddChildNV('CADArea');
    vNode.AddAttribNV(cnstXMLCmdRect).ValueAsFRect := MainApplication.GetViewFRect;
  end;
end;

procedure TsgXMLIDE.CommandSetShowEntitySettings(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNodeSample;
  vMethod: TsgShowEntityMethod;
  vPrecision: Double;
begin
  if not Assigned(CADEditor) then
    Exit;
  CADEditor.GetShowEntitySettings(vMethod, vPrecision);
  vNode := ANode.GetAttributeByName(cnstXMLCmdShowEntityMethod);
  if Assigned(vNode) then
    vMethod := TsgShowEntityMethod(vNode.ValueAsInt);
  vNode := ANode.GetAttributeByName(cnstXMLCmdShowEntityPrecision);
  if Assigned(vNode) then
    vPrecision := vNode.ValueAsDouble;
  CADEditor.SetShowEntitySettings(vMethod, vPrecision);
end;

procedure TsgXMLIDE.CommandSetSpecialCustomInsert(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNodeColor, vNodeWeight: TsgNodeSample;
  vEntity: TsgDXFEntity;
begin
  if not Assigned(CADEditor) then
    Exit;
  vEntity := GetEntityByNode(ANode, AResult.Errors);
  if Assigned(vEntity) and (vEntity.EntType = ceInsert) then
  begin
    vNodeColor := ANode.GetAttributeByName(cnstXMLNames[xmlColor].Name);
    vNodeWeight := ANode.GetAttributeByName(cnstXMLNames[xmlLineWeight].Name);
    if Assigned(vNodeColor) or Assigned(vNodeWeight) then
    begin
      CADEditor.BeginEnt(vEntity, True);
      if Assigned(vNodeColor) then
        vEntity.ColorCAD := vNodeColor.ValueData.ValueAsColorCAD;
      if Assigned(vNodeWeight) then
        vEntity.LineWeight := vNodeWeight.ValueData.ValueAsDouble;
      CADEditor.EndEnt(vEntity);
      CADEditor.Changed;
    end;
    AddSelectEntity(vEntity, ANode);
    CADEditor.RecreateBlockAsTemplate;
  end;
end;

procedure TsgXMLIDE.CommandInvalidate(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNodeSample;
  vBox: TFRect;
begin
  if not Assigned(CADEditor) then
    Exit;
  vNode := ANode.GetAttributeByName(cnstXMLCmdBox);
  vBox := cnstBadRect;
  if Assigned(vNode) then
    vBox := vNode.ValueAsFRect;
  if Assigned(CADEditor) then
  begin
    if IsBadRect(vBox) then
      CADEditor.Invalidate
    else
      CADEditor.InvalidateFRect(vBox)
  end;
end;

procedure TsgXMLIDE.CommandSetView(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNodeVPort: TsgNodeSample;
  vVPort, vActiveVPort: TsgDXFVport;
begin
  if IsInvalidRefs then
    Exit;
  vVPort := TsgDXFVport.Create;
  try
    vNodeVPort := ANode.GetChildByName(TsgDXFVportAccess(vVPort).GetNodeName);
    if Assigned(vNodeVPort) then
    begin
      vVPort.FromXML(vNodeVPort, AResult);
      vActiveVPort := FCADImage.Converter.ActiveVPort;
      if vVPort.ViewHeight <= 0 then
        vVPort.ViewHeight := vActiveVPort.ViewHeight;
      vActiveVPort.AssignEntity(vVPort);
      TsgDXFConverterAccess(FCADImage.Converter).UpdateViewTwistMatrix;
      CADEditor.OpenVPort;
    end;
  finally
    vVPort.Free;
  end;
end;

procedure TsgXMLIDE.CommandSetViewRect(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNodeRect: TsgNodeSample;
  vRect: TFRect;
begin
  if MainApplication <> nil then
  begin
    vNodeRect := ANode.GetAttributeByName(cnstXMLCmdRect);
    if Assigned(vNodeRect) then
    begin
      vRect := vNodeRect.ValueAsFRect;
      MainApplication.SetViewFRect(vRect);
    end;
  end;
end;

procedure TsgXMLIDE.CommandShowSelectEntities(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vMethod: TsgShowEntityMethod;
  vPrecision: Double;
begin
  if not Assigned(CADEditor) then
    Exit;
  CADEditor.GetShowEntitySettings(vMethod, vPrecision);
  try
    CommandSetShowEntitySettings(ANode, AResult);
    FCADEditor.ShowSelectEntities;
  finally
    CADEditor.SetShowEntitySettings(vMethod, vPrecision);
  end;
end;

destructor TsgXMLIDE.Destroy;
begin
{$IFDEF CS_USEFORM}
  Application.OnException := FOnApplicationExeptionPrev;
{$ENDIF}
  FSelectedHandles := nil;
  if FInstructions <> nil then
    FreeAndNil(FInstructions);
  FXMLCommParameterList.Free;
  FreeAndNil(FXMLInp);
  inherited Destroy;
end;

procedure TsgXMLIDE.OnInterfaceButtonClick(const AHandle: THandle;
  const ACaption: string);
begin
  if xeOnButtonClick in FXMLEvents then
    OnButtonMenuClick(AHandle, ACaption);
end;

procedure TsgXMLIDE.OnButtonMenuClick(const AHandle: THandle;
  const ACaption: string);
var
  vHandleS: string;
begin
  if AHandle = 0 then
    vHandleS := ''
  else
    vHandleS := HandleToStr(AHandle);
  OnEventCustom(xeOnButtonClick, nil, nil,
    [cnstXMLCmdCaption, cnstXMLCmdHandle], [ACaption, vHandleS], False);
end;


procedure TsgXMLIDE.DoTrialMessage;
{$IFDEF SG_DELPHI_VCL}
{$IFDEF SGAPPPROTECT}
const
  cnstTrialMessage1: string = 'The trial period is over.';
  cnstTrialMessage2: string = 'Please contact us at info@cadsofttools.com';
{$IFDEF SG_CADEDITORX}
  function AddText(const AText: string; const H, X, Y, Z: Double): TFRect;
  var
    vText: TsgDXFText;
  begin
    vText := TsgDXFText.Create;
    FCADImage.CurrentLayout.AddEntity(vText);
    vText.Layer := FCADImage.Converter.LayerByName(cnstTrialMessage1);
    vText.Layer.ColorCAD := MakeColorCAD(acRGBColor, clRed);
    vText.Layer.Locked := True;
    vText.Layer.Visible := True;
    vText.Layer.Visibility := True;
    vText.Text := AText;
    vText.ColorCAD := vText.Layer.ColorCAD;
    vText.Height := H;
    vText.Point := MakeFPoint(X, Y, Z);
    FCADImage.Converter.Loads(vText);
    Result := vText.Box;
  end;

  function AddPoly(const APoints: array of TFPoint; const AClosed: Boolean): TFRect;
  var
    I: Integer;
    vPoly: TsgDXFPolyline;
  begin
    vPoly := TsgDXFPolyline.Create;
    FCADImage.CurrentLayout.AddEntity(vPoly);
    vPoly.ColorCAD := MakeColorCAD(acRGBColor, clRed);
    vPoly.Layer := FCADImage.Converter.LayerByName(cnstTrialMessage1);
    for I := Low(APoints) to High(APoints) do
      AddVertexInPolyline(vPoly, APoints[I]);
    vPoly.Closed := AClosed;
    FCADImage.Converter.Loads(vPoly);
    Result := vPoly.Box;
  end;

var
  vExtents: TFRect;
  vHeight: Double;
  vBox: TFRect;
{$ENDIF}
begin
{$IFDEF SG_CADEDITORX}
  if not IsInvalidRefs then
  begin
    vExtents := FCADImage.CurrentLayout.Box;
    if IsBadRect(vExtents) then
      vExtents := MakeFRect(0, 210, 0, 297, 0, 0);
    vHeight := Abs(vExtents.Top - vExtents.Bottom);
    if vHeight <= 0 then
      vHeight := 2.5;
    vBox := cnstBadRect;
    UnionFRect(vBox, AddText(cnstTrialMessage1, vHeight * 0.35, vExtents.Left, vExtents.Bottom + vHeight * 0.1, vExtents.Z1));
    UnionFRect(vBox, AddText(cnstTrialMessage2, vHeight * 0.35, vExtents.Left, vExtents.Bottom + vHeight * 0.5, vExtents.Z1));
    vBox.Top := vExtents.Top;
    vBox.Bottom := vExtents.Bottom;
    AddPoly([vBox.TopLeft, MakeFPoint(vBox.Right, vBox.Top, vBox.Z1),
      vBox.BottomRight, MakeFPoint(vBox.Left, vBox.Bottom, vBox.Z2),
      vBox.TopLeft], True);
    if Assigned(FCADImage) then
    begin
      CADEditor.CalcExtents;
      CADEditor.FitToSize;
    end;
  end;
{$ENDIF}
  if _EOFTrial then
    DoMessageError(RootOut, cnstTrialMessage1 + ' ' + cnstTrialMessage2, nil)
{$IFDEF SG_ABVIEWER}
  else
    if SendAppMessage(RM_GetRegLevel, 0, 0) in [2,3] then
      DoMessageError(RootOut, sgTranslateModule.sEnterpriseAndTrialFeature, nil)
{$ENDIF}
{$ELSE}
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
{$ENDIF}
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TsgXMLIDE.DoXMLParserError(const AMessage: string;
  const APosition: PPoint);
var
  vError: TsgNode;
begin
  CreateResultNode(cnstXMLParserError);
  vError := GetIResultNode.Errors.AddChildNV(cnstXMLError);
  vError.AddAttribNV(cnstXMLCmdMessage, AMessage);
  if APosition <> nil then
    vError.AddAttribNV(cnstXMLPosition).ValueData.ValueAsPoint := APosition^;
end;

procedure TsgXMLIDE.CommandEditorAttribs(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNodeSample;
begin
  if not Assigned(CADEditor) then
    Exit;
  vNode := ANode.GetAttributeByName(cnstXMLCmdReadOnly);
  if Assigned(vNode) then
    CADEditor.ReadOnly := vNode.ValueAsBool;
  CommandSetShowEntitySettings(ANode, AResult);
end;

procedure TsgXMLIDE.CommandGetBox(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vEntity: TsgDXFEntity;
  I: Integer;
  vBox: TFRect;
  vList: TsgInt64List;
  vEntities: TList;
  vSelectedEntities: TList;
  vBoxCAD: Boolean;
  vBoxCADAttrib: TsgNodeSample;
  vResult: TsgNode;
  vSize: TFPoint;

  function GetBoxByEntityList(const AEntities: TList): TFRect;
  var
    I: Integer;
  begin
    Result := cnstBadRect;
    if AEntities.Count > 0 then
      for I := AEntities.Count - 1 downto 0 do
        UnionFRect(Result,TsgDXFEntity(AEntities[I]).Box);
  end;

  procedure SetBox(var ABox: TFRect);
  begin
    if vEntities.Count > 0 then
    begin
      if vEntities.Count > 1 then
        ABox := GetBoxByEntityList(vEntities)
      else
        ABox := vEntity.Box;
    end
    else
      ABox := FCADImage.CurrentLayout.Box;
  end;

begin
  if IsInvalidRefs then
    Exit;
  vResult := AResult.Output;
  vBoxCADAttrib := ANode.GetAttributeByName(cnstXMLCmdBoxCad);
  if Assigned(vBoxCADAttrib) then
  begin
    vBoxCAD := vBoxCADAttrib.ValueAsBool;
    if vBoxCAD then
    begin
      vResult.AddAttribNV(cnstXMLHeight).ValueAsDouble := FCADImage.AbsHeight;
      vResult.AddAttribNV(cnstXMLWidth).ValueAsDouble := FCADImage.AbsWidth;
      vBox := FCADImage.Extents;
    end
    else
      vBox := FCADImage.CurrentLayout.Box;
    vResult.AddAttribNV(cnstXMLCmdBox).ValueAsFRect := vBox;
    Exit;
  end;
  vBox := cnstBadRect;
  vList := TsgInt64List.Create;
  vEntities := TList.Create;
  try
    vEntity := GetEntityByNode(ANode, AResult.Errors, vList, vEntities);
    if (not Assigned(vEntity)) then
    begin // Process selecter entity
      if ANode.AttributeNodesCount > 0 then
        Exit;
      if FSelectedHandles.Count > 0 then
      begin
        for I := 0 to FSelectedHandles.Count - 1 do
        begin
          vEntity := GetEntityFromSelectHandles(I);
          if vEntity = nil then
            Continue;
          vEntities.Add(vEntity);
        end;
      end;

      if Assigned(CADEditor) then
      begin
        vSelectedEntities := TList.Create;
        try
          CADEditor.BeforeChange(vSelectedEntities);
          try
            if vSelectedEntities.Count > 0 then
            begin
              for I := 0 to vSelectedEntities.Count - 1 do
                vEntities.Add(vSelectedEntities[I]);
            end;
          finally
            CADEditor.AfterChange;
          end;
        finally
          vSelectedEntities.Free;
        end;
      end;
      if vEntities.Count > 0 then
        vEntity := vEntities[0];
      SetBox(vBox);
    end
    else
      SetBox(vBox);
  finally
    vList.Free;
    vEntities.Free;
  end;

  vResult.AddAttribNV(cnstXMLCmdBox).ValueAsFRect := vBox;
  if not IsBadRect(vBox) then
  begin
    vSize := AbsFPoint(SubFPoint(vBox.BottomRight, vBox.TopLeft));
    vResult.AddAttribNV(cnstXmlCmdWidth).ValueAsDouble := vSize.X;
    vResult.AddAttribNV(cnstXmlCmdHeight).ValueAsDouble := vSize.Y;
    if vSize.Z <> 0 then
      vResult.AddAttribNV(cnstXmlCmdDepth).ValueAsDouble := vSize.Z;
  end;
end;

procedure TsgXMLIDE.CommandGetCADPoint(ANode: TsgNodeSample; AResult: IsgResultNode);
var
  vAtributePoint: TsgNodeSample;
  vMode: TsgNodeSample;
  vPoint: TFPoint;
  vPointScreen: TPoint;
  vModeString: string;
  vBox: TRect;
  vFlags: TsgNearestFlags;
  vNearestEntName: string;
  vPoint2D: TPoint;

  procedure GetCadToScreen;
  begin
    vPoint := CADEditor.GetImageCoords(vAtributePoint.ValueData.ValueAsPoint);
    AResult.Output.AddAttribNV(cnstXMLCmdPoint).ValueAsFPoint := vPoint;
  end;

begin
  if not IsInvalidRefs then
  begin
    vAtributePoint := ANode.GetAttributeByName(cnstXMLCmdPoint);
    if Assigned(vAtributePoint) then
    begin
      vMode := ANode.GetAttributeByName(cnstXMLMode);
      if Assigned(vMode) then
      begin
        vModeString := AnsiUpperCase(vMode.ValueAsStr);
        if vModeString = cnstXmlCmdModeCadToScreen then
        begin
          vPointScreen := CADEditor.GetScreenCoords(vAtributePoint.ValueData.ValueAsFPoint);
          vPoint := MakeFPoint(vPointScreen.X, vPointScreen.Y);
          AResult.Output.AddAttribNV(cnstXMLCmdPoint).ValueAsFPoint := vPoint;
        end
        else
          if vModeString = cnstXmlCmdModeScreenToCad then
            GetCadToScreen
          else
            if vModeString = cnstXmlCmdModeNearestPoint then
            begin
              vBox := MakeRectFromF2DRect(ANode.GetAttributeByName(cnstXMLCmdBox).ValueAsF2DRect);
              vFlags := [];
              vMode := ANode.GetAttributeByName(cnstXMLCmdVisible);
              if Assigned(vMode) and vMode.ValueAsBool then
                Include(vFlags, nfVisibleEnts);
              CADEditor.GetNearestEntity(vBox, vFlags, vNearestEntName, vPoint2D, vPoint);
              AResult.Output.AddAttribNV(cnstXMLCmdPoint).ValueAsFPoint := vPoint;
              AResult.Output.AddAttribNV(cnstXMLCmdX).ValueAsInt := vPoint2D.X;
              AResult.Output.AddAttribNV(cnstXMLCmdY).ValueAsInt := vPoint2D.Y;
              AResult.Output.AddAttribNV(cnstXMLCmdName).ValueAsStr := vNearestEntName;
            end
            else
              DoMessageError(AResult.Errors, sXMLInterfaceInvalidMode, ANode);
      end
      else
        GetCadToScreen;
    end
    else
      DoMessageError(AResult.Errors, sXMLInterfaceEntityNotSelectError, ANode);
  end;
end;

procedure TsgXMLIDE.CommandGet(ANode: TsgNodeSample;
   AResult: IsgResultNode);
var
  vOut: TsgNode;
  vEntity: TsgDXFEntity;
  vBox: TFRect;
  vList: TsgInt64List;
  vEntities: TList;
  vMode: TsgXMLModes;
  vModeAttrib: TsgNodeSample;
  vApplyFilter: Boolean;
  vModeInt: Integer;
begin
  if IsInvalidRefs then
    Exit;
  vOut := AResult.Output;
  vList := TsgInt64List.Create;
  vEntities := TList.Create;
  try
    vMode := cnstDefaultXMLMode + [xmAddSectionEntities];
    vModeAttrib := ANode.GetAttributeByName(cnstXMLMode);
    if Assigned(vModeAttrib) then
    begin
      // ModeAttrib.ValueAsInt is bit coded
      // 0 bit xmAddSubEntities
      // 1 bit xmAddSectionEntities
      // 2 bit xmNoSubEntitiesNode
      // 3 bit xmCallHelp
      // 4 bit xmOnlyChildNodes
      // 5 bit xmlGetDefaultValue
      // 6 bit xmlForViewing
      // 7 bit xmlModMeshes
      // 8 bit xmlModPhysical
      // 9 bit xmlModAliases
      // 10 bit xmFullXRefNames
      // 11 bit xmlModGeometry
      // 12 bit xmlModReduced
      // 13 bit xmPathGenSup

      vModeInt := vModeAttrib.ValueAsInt;
      vMode := sgIntToXMLModes(vModeInt);
    end;
    vApplyFilter := GetEntitiesByNodeWithFilter(ANode, AResult.Errors, vList, vEntities);
    vEntity := nil;
    vBox := cnstBadRect;
    if vEntities.Count > 0 then
      vEntity := vEntities.First;
    if not Assigned(vEntity) then
    begin
      if not vApplyFilter then
      begin
        FCADImage.Converter.ToXML(vOut, vMode);
        vBox := FCADImage.CurrentLayout.Box;
      end;
    end
    else
    begin
      if vEntities.Count > 1 then
        ExpEntitiesToXML(vEntities, vOut, vMode)
      else
      begin
        vBox := vEntity.Box;
        vEntity.ToXML(vOut, vMode);
      end;
    end;
  finally
    vList.Free;
    vEntities.Free;
  end;
  if not IsBadRect(vBox) then
    vOut.AddAttribNV(cnstXMLCmdBox).ValueAsFRect := vBox;
end;

procedure TsgXMLIDE.CommandAdd(ANode: TsgNodeSample;
  AResult: IsgResultNode);
begin
  if IsInvalidRefs then
    Exit;
  FCADImage.Converter.FromXML(ANode, AResult);
  if Assigned(CADEditor) then
    CADEditor.Changed;
end;

procedure TsgXMLIDE.CommandFindTextEntities(ANode: TsgNodeSample;
  AResult: IsgResultNode);
begin
  CommandInernalIterate(ANode, AResult, 0);
end;

procedure TsgXMLIDE.CommandFitToSize(ANode: TsgNodeSample;
  AResult: IsgResultNode);
begin
  if not Assigned(CADEditor) then
    Exit;
  CADEditor.CalcExtents;
  CADEditor.FitToSize;
end;

procedure TsgXMLIDE.CommandIterator(ANode: TsgNodeSample;
  AResult: IsgResultNode);
begin
  CommandInernalIterate(ANode, AResult, 1);
end;

procedure TsgXMLIDE.CommandSelect(ANode: TsgNodeSample;
   AResult: IsgResultNode);
var
  I: Integer;
  vEntity: TsgDXFEntity;
  vIndex: Integer;
  AList: TsgInt64List;
  AEntities: TList;
  vSelect, vGroup, vMarker: Boolean;
  vMode: Integer;

  procedure DoMsgError(const AEntity: TsgDXFEntity; const AGroup: Boolean);
  var
    I: Integer;
    vNode: TsgNode;
  begin
    if AGroup then
      DoMessageError(AResult.Errors, sXMLInterfaceEntityNotSelectError, ANode)
    else
      if Assigned(AEntity) then
      begin
        vNode := AEntity.ToXML(AResult.Result, [xmNoSubEntitiesNode]);
        if Assigned(vNode.Childs) then
          vNode.Childs.ClearNodes;
        I := 0;
        while I < vNode.AttributeNodesCount do
        begin
          case GetXMLId(vNode.AttributeNodes[I].Name) of
            xmlName, xmlHandle:
              Inc(I);
          else
            vNode.Attributes.Delete(I);
          end;
        end;
      end
      else
        DoMessageError(AResult.Errors, sXMLInterfaceEntityNotFoundError, ANode);
  end;

begin
  vSelect := False;
  vGroup := False;
  vEntity := nil;
  AList := TsgInt64List.Create;
  AEntities := TList.Create;
  try
    GetEntitiesByNodeWithFilter(ANode, AResult.Errors, AList, AEntities);
    if AEntities.Count > 0 then
      vEntity := AEntities.First;
    if AEntities.Count > 0 then
    begin
      if AEntities.Count > 1 then
      begin
        vGroup := True;
        vEntity := nil;
      end;
      vMarker := GetMarkerSelectMode(ANode);
      if not Assigned(CADEditor) then
        vMarker := False;
      vSelect := True;
      if vMarker then
      begin
        vSelect := False;
        if Assigned(CADEditor)then
        begin
          vMode := GetAttributeInt(ANode, cnsTXMLCommParameters[cpSelectMode].Name, 0);
          vSelect := SetSelectEntities(AEntities, ANode, vMode);
        end;
      end
      else
      begin
        for I := 0 to AEntities.Count - 1 do
        begin
          vEntity := TsgDXFEntity(AEntities[I]);
          vIndex := IndexOfSelectedHandles(vEntity);
          if vIndex < 0 then
            AddEntityToSelectHandles(vEntity);
        end;
      end;
    end;
  finally
    AList.Free;
    AEntities.Free;
    if not vSelect then
      DoMsgError(vEntity, vGroup);
  end;
end;

procedure TsgXMLIDE.CommandUnSelect(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  I, vIndex: Integer;
  AList: TsgInt64List;
  AEntities: TList;
  vMarker: Boolean;
  vEntity: TsgDXFEntity;
begin
  AList := TsgInt64List.Create;
  AEntities := TList.Create;
  try
    GetEntitiesByNodeWithFilter(ANode, AResult.Errors, AList, AEntities);
    vMarker := GetMarkerSelectMode(ANode);
    if not Assigned(CADEditor) then
      vMarker := False;
    if AEntities.Count > 0 then
    begin
      if vMarker then
      begin
        if Assigned(CADEditor) then
          SetSelectEntities(AEntities, ANode, 1);
      end
      else
      begin
        for I := 0 to AEntities.Count - 1 do
        begin
          vEntity := TsgDXFEntity(AEntities[I]);
          vIndex := IndexOfSelectedHandles(vEntity);
          if vIndex >= 0 then
            FSelectedHandles.Delete(vIndex);
        end;
      end;
    end
    else
    begin
      if vMarker then
        CADEditor.SetSelectedEntities(nil)
      else
        FSelectedHandles.Clear;
    end;
  finally
    AList.Free;
    AEntities.Free;
  end;
end;

procedure TsgXMLIDE.CommandWaterMark(ANode: TsgNodeSample;
  AResult: IsgResultNode);
const
  cnstWatermark = 'Watermark';
  cnstXMLText = 'Text';
  cnstXMLParameters = 'Parameters';

var
  vOut: TsgNode;
  vLayer: TsgDXFLayer;
  vStyle: TsgDXFStyle;
  vWatermark: TsgDXFText;
  vConverter: TsgDXFConverter;

  function ReadParamsFromNode: TsgWatermarkParams;
  var
    vChild: TsgNodeSample;
    vPosition: Integer;
  begin
    if Assigned(ANode.GetChildByName(cnstXMLText)) then
    begin
      vChild := ANode.GetChildByName(cnstXMLText);
      Result.Text := GetAttributeStr(vChild, 'Value');
    end;
    if Assigned(ANode.GetChildByName(cnstXMLParameters)) then
    begin
      vChild := ANode.GetChildByName(cnstXMLParameters);
      Result.Angle := GetAttributeFloat(vChild, 'Angle', -1);
      Result.FontName := GetAttributeStr(vChild, 'FontName');
      Result.Color := GetAttributeInt(vChild, 'Color', -1);
      Result.Scale := GetAttributeFloat(vChild, 'Scale', -1);
      vPosition := GetAttributeInt(vChild, 'Align', 0);
      if (vPosition > -1) and (vPosition < 5) then
        Result.Position := TsgWatermarkPosition(vPosition)
      else
        Result.Position := DefWatermarkPapams.Position;
    end;
  end;

  function ReadParamsFromEntity: TsgWatermarkParams;
  begin
    Result.Text := vWatermark.Text;
    Result.Angle := vWatermark.Rotation;
    Result.FontName := vWatermark.Style.FontName;
    Result.Color := vWatermark.Color;
    Result.Scale := 1;
    Result.Position := wpCenter;
  end;


  procedure AlignWatermark(const AParams: TsgWatermarkParams);
  var
    vCADRect, vTextRect: TFRect;
    vPt, vPt1 : TFPoint;
    vTextWidth, vCADWidth, vRatio: Double;
  begin
    vWatermark.Layer.Visible := False;
    vWatermark.Rotation := 0;
    vConverter.Loads(vWatermark);
    FCADImage.GetExtents;
    vCADRect := FCADImage.Extents;
    vTextWidth := vWatermark.Box.Right - vWatermark.Box.Left;
    vCADWidth := vCADRect.Right - vCADRect.Left;
    vRatio := vTextWidth / vCADWidth;
    vWatermark.Height := vWatermark.Height / vRatio * AParams.Scale;
    vWatermark.Rotation := AParams.Angle;
    vConverter.Loads(vWatermark);
    vTextRect := vWatermark.Box;
    case AParams.Position of
      wpCenter:
        begin
          vPt := GetCenterOfRect(vCADRect);
          vPt1 := GetCenterOfRect(vTextRect);
          vPt := AddFPoint(vWatermark.Point, SubFPoint(vPt, vPt1));
          vWatermark.Point := vPt;
        end;
      wpBottomLeft:
        vWatermark.Point := MakeFPoint(vCADRect.Left, vCADRect.Bottom);
      wpBottomRight:
        vWatermark.Point := vCADRect.BottomRight;
      wpTopLeft:
        vWatermark.Point := vCADRect.TopLeft;
      wpTopRight:
        vWatermark.Point := MakeFPoint(vCADRect.Right, vCADRect.Top);
    end;
    vWatermark.Layer.Visible := True;
    vConverter.Loads(vWatermark);
    vOut.AddAttribNV('Point', FPointToStr(vWatermark.Point));
    vOut.AddAttribNV('Box', FRectToStr(vWatermark.Box));
  end;

  function GetWatermark(const ALayerIndex: Integer): TsgDXFText;
  var
    I: Integer;
    vEntity: TsgDXFEntity;
  begin
    Result := nil;
    if ALayerIndex <> -1 then
    begin
      vLayer := vConverter.Layers[ALayerIndex];
      for I := vConverter.Sections[csEntities].Count - 1 downto 0 do
      begin
        vEntity := vConverter.Sections[csEntities].Entities[I];
        if vEntity.Layer = vLayer then
        begin
          Result := TsgDXFText(vEntity);
          vStyle := Result.Style;
          Break;
        end;
      end;
    end;
  end;

  procedure ApplyParams(const AParams: PsgWatermarkParams; const ADefParams: PsgWatermarkParams = nil);
  var
    vCurParams: TsgWatermarkParams;
  begin
    if Assigned(ADefParams) then
      vCurParams := ADefParams^
    else
      vCurParams := ReadParamsFromEntity;
    vWatermark.Text := AParams^.Text;
    if AParams^.Angle = -1 then
      AParams^.Angle := vCurParams.Angle;
    if AParams^.FontName <> '' then
      vWatermark.Style.FontName := AParams^.FontName
    else
      vWatermark.Style.FontName := vCurParams.FontName;
    if AParams^.Color <> -1 then
      vWatermark.Color := AParams^.Color
    else
      vWatermark.Color := vCurParams.Color;
    if AParams^.Scale = -1 then
      AParams^.Scale := DefWatermarkPapams.Scale;
    AParams^.Position := AParams^.Position;
  end;

var
  vMode, vLayerIndex: Integer;
  vParams: TsgWatermarkParams;
begin
  if not Assigned(FCADImage) then
    Exit;
  vOut := AResult.Output;
  vMode := GetAttributeInt(ANode, cnstXMLMode, 0);
  vConverter := FCADImage.Converter;
  vLayerIndex := vConverter.Sections[csLayers].IndexOfName(cnstWatermark);
  vWatermark := GetWatermark(vLayerIndex);
  case vMode of
    1:                                   //set parametrs
      begin
        vParams := ReadParamsFromNode;
        if vParams.Text = '' then
        begin
          if Assigned(vWatermark) then
          begin                          //remove watermark
            vConverter.Sections[csEntities].RemoveEntity(vWatermark);
            vConverter.Sections[csLayers].RemoveEntity(vLayer);
            vLayer.Free;
            vStyle.Free;
            vWatermark.Free;
          end;
        end
        else
        begin
          if Assigned(vWatermark) then   //edit watermark
            ApplyParams(@vParams)
          else                           //create watermark
          begin
            vStyle := TsgDXFStyle.Create;
            vStyle.Name := cnstWatermark;
            vLayer := TsgDXFLayer.Create;
            vLayer.Name := cnstWatermark;
            vConverter.Sections[csLayers].AddEntity(vLayer);
            vWaterMark := TsgDXFText.Create;
            vWatermark.Layer := vLayer;
            vWatermark.Style := vStyle;
            vConverter.Loads(vWaterMark);
            vConverter.Sections[csEntities].AddEntity(vWaterMark);
            ApplyParams(@vParams, @DefWatermarkPapams);
          end;
          AlignWatermark(vParams);
        end;
        FCADImage.GetExtents;
      end;
  else                                   //read paramers
    begin
      if Assigned(vWatermark) then
      begin
        vParams := ReadParamsFromEntity;
        vOut.AddAttribNV('Text', vParams.Text);
        vOut.AddAttribNV('Angle', FloatToStr(vParams.Angle));
        vOut.AddAttribNV('FontName', vParams.FontName);
        vOut.AddAttribNV('Color', HexDisplayPrefix + IntToHex(vParams.Color, 6));
      end;
    end;
  end;
end;

procedure TsgXMLIDE.CommandApply(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vOldBox: TFRect;
  I, vMarkerMode: Integer;
  vEntity: TsgDXFEntity;
  vSelectedEntities: TList;
  vChanged: Boolean;
  vMode, vMarker: TsgNodeSample;
  vSkipChanges, vSkipChangesPrev: Boolean;
begin
  if not Assigned(FCADEditor) then
    Exit;
  vMode := ANode.GetAttributeByName(cnstXMLMode);
  if Assigned(vMode) and (AnsiUpperCase(vMode.ValueAsStr) = cnstXmlCmdModeInterfacce) then
    CommandApplyXML(ANode, AResult, True)
  else
  begin
    vSkipChanges := GetAttributeBool(ANode, 'skipchanges', false);
    vMarkerMode := -1;
    vMarker := ANode.GetAttributeByName(cnstXMLCmdMarker);
    try
      if Assigned(vMarker) then
      begin
        TsgNode(ANode).RemoveAttrib(vMarker);
        vMarkerMode := Integer(vMarker.ValueAsBool);
      end;
      if (vMarkerMode <> 1) and Assigned(FSelectedHandles) and (FSelectedHandles.Count > 0) then
      begin
        vOldBox := cnstBadRect;
        vChanged := False;
        for I := 0 to FSelectedHandles.Count - 1 do
        begin
          vEntity := GetEntityFromSelectHandles(I);
          if vEntity = nil then
            Continue;
          UnionFRect(vOldBox, vEntity.Box);
          if TsgDXFEntity(vEntity) is TsgDXFBlock then
            TsgDXFBlock(vEntity).IsLoaded := False;
          vEntity.FromXML(ANode, AResult);
          FCADImage.Converter.Loads(vEntity);
          if not vChanged then
            vChanged := True;
          UnionFRect(vOldBox, vEntity.Box);
        end;
        if Assigned(CADEditor) then
        begin
          if not IsBadRect(vOldBox) then
            CADEditor.InvalidateFRect(vOldBox)
          else
            if vChanged then
              CADEditor.Invalidate;
        end;
      end;
      if (vMarkerMode <> 0) and Assigned(CADEditor) then
      begin
        vSkipChangesPrev := CADEditor.SkipChanges;
        vSelectedEntities := TList.Create;
        try
          CADEditor.SkipChanges := vSkipChanges;
          CADEditor.BeforeChange(vSelectedEntities);
          try
            if vSelectedEntities.Count > 0 then
            begin
              for I := 0 to vSelectedEntities.Count - 1 do
              begin
                if TsgDXFEntity(vSelectedEntities[I]) is TsgDXFBlock then
                  TsgDXFBlock(vSelectedEntities[I]).IsLoaded := False;
                TsgListObject(vSelectedEntities[I]).FromXML(ANode, AResult);
                FCADImage.Converter.Loads(vSelectedEntities[I]);
              end;
            end;
          finally
            CADEditor.AfterChange;
          end;
        finally
          CADEditor.SkipChanges := vSkipChangesPrev;
          vSelectedEntities.Free;
        end;
      end;
    finally
      if Assigned(vMarker) then
        TsgNode(ANode).AddAttrib(vMarker);
    end;
  end;
end;

procedure TsgXMLIDE.CommandApplyXML(ANode: TsgNodeSample; AResult: IsgResultNode; const AUseChilds: Boolean);
var
  I: Integer;
  vInXML: string;
begin
  if CheckForApp(ANode, AResult) then
    if AUseChilds then
    begin
      for I := 0 to ANode.ChildNodesCount - 1 do
        CommandApplyXML(ANode.ChildNodes[I], AResult, False);
    end
    else
    begin
      vInXML := SaveNodeToXMLString(ANode);
      MainApplication.ApplyXML(vInXML, Self);
    end;
end;

function TsgXMLIDE.FilterElement(const AEnt: TsgDXFEntity;
  const AFindEnt: TsgDXFEntity): Boolean;
begin
  Result := AEnt = AFindEnt;
end;

function TsgXMLIDE.FilterCurElement(const AEnt: TsgDXFEntity;
  const AFindEnt: TsgDXFEntity): Boolean;
var
  vMLineStyle: TsgMLineStyle;
  vMLineEnt: TsgMLineEntry;
  I: Integer;
begin
  Result := False;
  if AEnt = nil then Exit;
  if AEnt is TsgMLineStyle then
  begin
    vMLineStyle := TsgMLineStyle(AEnt);
    for I := 0 to vMLineStyle.Count - 1 do
    begin
      vMLineEnt := vMLineStyle.Entries[I];
      if Assigned(vMLineEnt.LineType) then
        Result := Result or FilterElement(vMLineEnt.LineType, AFindEnt);
    end;
  end;
  if AEnt is TsgDXFLayer then
  begin
    if TsgDXFLayer(AEnt).XrefLink then
      Result := Result or FilterElement(AEnt, AFindEnt);
  end;
  if Assigned(AEnt.Layer) then
    Result := Result or FilterElement(AEnt.Layer, AFindEnt);
  if Assigned(AEnt.LineType) then
    Result := Result or FilterElement(AEnt.LineType, AFindEnt);
  case AEnt.EntType of
    ceDimension:
      begin
        if Assigned(TsgDXFDimension(AEnt).Style) then
          Result := Result or FilterElement(TsgDXFDimension(AEnt).Style, AFindEnt);
        if Assigned(TsgDXFDimension(AEnt).TextStyle) then
          Result := Result or FilterElement(TsgDXFDimension(AEnt).TextStyle, AFindEnt);
        if Assigned(TsgDXFInsert(AEnt).Block) then
          Result := Result or FilterElement(TsgDXFInsert(AEnt).Block, AFindEnt);
        if FilterBlockEnt(TsgDXFInsert(AEnt).Block, AFindEnt) then
          Result := True;
      end;
    ceText:
      if Assigned(TsgDXFText(AEnt).Style) then
        Result := Result or FilterElement(TsgDXFText(AEnt).Style, AFindEnt);
    ceMText:
      if Assigned(TsgDXFMText(AEnt).Style) then
        Result := Result or FilterElement(TsgDXFMText(AEnt).Style, AFindEnt);
    ceInsert:
      begin
        if Assigned(TsgDXFInsert(AEnt).Block) then
          Result := Result or FilterElement(TsgDXFInsert(AEnt).Block, AFindEnt);
        if FilterBlockEnt(TsgDXFInsert(AEnt).Block, AFindEnt) then
          Result := True;
      end;
    ceLeader:
      if Assigned(TsgDXFLeader(AEnt).DimStyle) then
        Result := Result or FilterElement(TsgDXFLeader(AEnt).DimStyle, AFindEnt);
    ceTolerance:
      if Assigned(TsgDXFTolerance(AEnt).DimStyle) then
        Result := Result or FilterElement(TsgDXFTolerance(AEnt).DimStyle, AFindEnt);
    ceMLine:
      if Assigned(TsgCADMLine(AEnt).Style) then
        Result := Result or FilterElement(TsgCADMLine(AEnt).Style, AFindEnt);
  end;
end;

function TsgXMLIDE.FilterBlockEnt(const ABlock: TsgDXFBlock;
  const AFindEnt: TsgDXFEntity): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ABlock = nil then Exit;
  for I := 0 to ABlock.Count - 1 do
  begin
    Result := FilterCurElement(ABlock.Entities[I], AFindEnt);
    if Result then
      Break;
  end;
end;

function TsgXMLIDE.ScanBlocks(const AGroup: TsgDXFGroup;
  const AFindEnt: TsgDXFEntity): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AGroup = nil then Exit;
  for I := 0 to AGroup.Count - 1 do
    if AGroup.Entities[I] is TsgDXFBlock then
    begin
      Result := FilterBlockEnt(TsgDXFBlock(AGroup.Entities[I]), AFindEnt);
      if Result then
        Break;
    end;
end;

function TsgXMLIDE.ScanElements(const AGroup: TsgDXFGroup;
  const AFindEnt: TsgDXFEntity): Boolean;
var
  I: Integer;
  vEnt: TsgDXFEntity;
begin
  Result := False;
  if AGroup = nil then Exit;
  for I := 0 to AGroup.Count - 1 do
  begin
    vEnt := AGroup.Entities[I];
    Result := FilterCurElement(vEnt, AFindEnt);
    if Result then
      Break;
  end;
end;

function TsgXMLIDE.IndexOfSelectedHandles(const AEntity: TsgDXFEntity): Integer;
begin
  Result := FSelectedHandles.IndexOf(AEntity.Handle, AEntity);
end;

function TsgXMLIDE.InitResultNodeOnCreating: Boolean;
begin
  Result := False;
end;

function TsgXMLIDE.IsCanDelete(AObject: TsgDXFEntity; var AParent: TsgDXFGroup): Boolean;
const
  cnstGroupFind: array[0..3] of TConvSection = (csEntities, csBlocks, csLayers,
    csMLineStyles);
var
  vGroup: TsgDXFGroup;
  vBlock: TsgDXFBlock;
  vStyle: TsgDXFStyle;
  vLayer: TsgDXFLayer;
  vDimStyle: TsgDXFDimensionStyle;
  vLineType: TsgDXFLineType;
  vMLineStyle: TsgMLineStyle;
  vBlockIsWork, vResult: Boolean;
  I: Integer;
begin
  if IsInvalidRefs then
  begin
    Result := False;
    Exit;
  end;
  AParent := nil;
  Result := False;
  if AObject is TsgDXFBlock then
  begin
    vGroup := FCADImage.Converter.Sections[csBlocks];
    for I := 0 to vGroup.Count - 1 do
    if vGroup.Entities[I] is TsgDXFBlock then
    begin
      vBlock := TsgDXFBlock(vGroup.Entities[I]);
      if vBlock = AObject then
      begin
        vBlockIsWork := (vBlock.Layout <> nil) or (vBlock.IsBlockIntetnal) or (Copy(vBlock.Name, 1, 1) = '*')
          or (Copy(vBlock.Name, 1, 1) = '_');
        if not vBlockIsWork then
          Result := True;
        AParent := vGroup;
      end;
    end;
  end;

  if AObject is TsgDXFStyle then
  begin
    vGroup := FCADImage.Converter.Sections[csStyles];
    for I := 0 to vGroup.Count - 1 do
    if vGroup.Entities[I] is TsgDXFStyle then
    begin
      vStyle := TsgDXFStyle(vGroup.Entities[I]);
      if vStyle = AObject then
      begin
        Result := True;
        AParent := vGroup;
      end;
    end;
  end;

  if AObject is TsgDXFLayer then
  begin
    vGroup := FCADImage.Converter.Sections[csLayers];
    for I := 0 to vGroup.Count - 1 do
    if vGroup.Entities[I] is TsgDXFLayer then
    begin
      vLayer := TsgDXFLayer(vGroup.Entities[I]);
      if vLayer = AObject then
      begin
        if (vLayer.Name = sLayerDefPoints) then
          Result := False
        else
          Result := True;
        AParent := vGroup;
      end;
    end;
  end;

  if AObject is TsgDXFDimensionStyle then
  begin
    vGroup := FCADImage.Converter.Sections[csDimStyles];
    for I := 0 to vGroup.Count - 1 do
    if vGroup.Entities[I] is TsgDXFDimensionStyle then
    begin
      vDimStyle := TsgDXFDimensionStyle(vGroup.Entities[I]);
      if vDimStyle = AObject then
      begin
        Result := True;
        AParent := vGroup;
      end;
    end;
  end;

  if AObject is TsgDXFLineType then
  begin
    vGroup := FCADImage.Converter.Sections[csLTypes];
    for I := 0 to vGroup.Count - 1 do
    if vGroup.Entities[I] is TsgDXFLineType then
    begin
      vLineType := TsgDXFLineType(vGroup.Entities[I]);
      if vLineType = AObject then
      begin
        if (vLineType.Name = sByBlock) or (vLineType.Name = sByLayer) or (vLineType.Name = sContinuous) then
          REsult := False
        else
          Result := True;
        AParent := vGroup;
      end;
    end;
  end;

  if AObject is TsgMLineStyle then
  begin
    vGroup := FCADImage.Converter.Sections[csMLineStyles];
    for I := 0 to vGroup.Count - 1 do
    if vGroup.Entities[I] is TsgMLineStyle then
    begin
      vMLineStyle := TsgMLineStyle(vGroup.Entities[I]);
      if vMLineStyle = AObject then
      begin
        Result := True;
        AParent := vGroup;
      end;
    end;
  end;

  if AObject is TsgDXFLayout then
  begin
    vGroup := FCADImage.Converter.Sections[csLayouts];
    if Assigned(vGroup) and (vGroup.Count > 1) and (FCADImage.CurrentLayout <> AObject) and  (not TsgDXFLayout(AObject).IsModel)  then
    begin
      if vGroup.IndexOfEntity(AObject) > -1 then
      begin
        Result := True;
        AParent := vGroup;
      end;
    end;
  end;

  if not Result then
  begin
    vGroup := TsgDXFGroup(FCADImage.CurrentLayout);
    if Assigned(vGroup) then
    begin
      if vGroup.IndexOfEntity(AObject) > -1 then
      begin
        Result := True;
        AParent := vGroup;
      end;
    end;
    Exit;
  end;

  for I := Low(cnstGroupFind) to High(cnstGroupFind) do
  begin
    if cnstGroupFind[I] = csBlocks then
      vResult :=ScanBlocks(CADImage.Converter.Sections[csBlocks], AObject)
    else
      vResult := ScanElements(CADImage.Converter.Sections[cnstGroupFind[I]], AObject);
    if vResult then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

procedure TsgXMLIDE.CommandDelete(ANode: TsgNodeSample; AResult: IsgResultNode);
var
  vParent: TsgDXFGroup;
  I, vMode: Integer;
  vEntity: TsgDXFEntity;
  vRemoved: Boolean;
  vDeleteNode: TsgNode;
begin
  if IsInvalidRefs then
    Exit;
  if FSelectedHandles.Count > 0 then
  begin
    vMode := GetAttributeInt(ANode, cnstXMLMode, -1);
    I := 0;
    while I < FSelectedHandles.Count do
    begin
      vEntity := GetEntityFromSelectHandles(I);
      if vEntity = nil then
      begin
        Inc(I);
        Continue;
      end;

      if IsCanDelete(vEntity, vParent) then
      begin
        FSelectedHandles.Delete(I);
        if Assigned(vParent) then
        begin
          if vMode <= 0 then
          begin
            if Assigned(CADEditor) then
              CADEditor.BeginEnt(vEntity, False);
            try
              vParent.RemoveEntity(vEntity);
              AResult.Output.AddChildNV(cnstXMLDeleted).
                AddAttribNV(cnstXMLNames[xmlHandle].Name).ValueAsHandle := vEntity.Handle;
            finally
              if Assigned(CADEditor) then
                CADEditor.EndEnt(nil);
            end;
          end
          else
          begin
            vRemoved := False;
            try
              if vParent.RemoveEntity(vEntity) then
              begin
                vRemoved := True;
                vDeleteNode := AResult.Output.AddChildNV(cnstXMLDeleted);
                vDeleteNode.AddAttribNV(cnstXMLNames[xmlHandle].Name).ValueAsHandle := vEntity.Handle;
                if (vMode = 2) and Assigned(CADEditor) and CADEditor.IsEntUsed(vEntity) then
                  vMode := 1;
                vDeleteNode.AddAttribNV(cnstXMLNames[xmlMode].Name).ValueAsInt := vMode;
              end;
            finally
              if vRemoved and (vMode = 2) then
              begin
                try
                  FreeAndNil(vEntity);
                except
                end;
              end;
            end;
          end;
        end;
      end
      else
      begin
      begin
        DoMessageError(AResult.Errors,Format(cnstXMLCmdCommandDeleteError,
          [HandleToStr(vEntity.Handle)]), ANode);
        Inc(I);
      end;
      end;
    end;
    if Assigned(CADEditor) then
      CADEditor.Invalidate;
  end;
  if Assigned(FCADEditor) then
    CADEditor.DeleteEntities;
end;

//<remove handle=""  source="" destination=""/>
procedure TsgXMLIDE.CommandRemove(ANode: TsgNodeSample; AResult: IsgResultNode);

//  function GetParentEntity(const ANode: TsgNodeSample): TsgDXFEntity;
//  var
//    vHandles: TsgInt64List;
//    vSrcEnts: TList;
//    vNodeSrc: TsgNode;
//  begin
//    Result := nil;
//    if Assigned(ANode) then
//    begin
//      vHandles := TsgInt64List.Create;
//      try
//        vSrcEnts := TList.Create;
//        try
//          vNodeSrc := TsgNode.Create;
//          try
//            vNodeSrc.AddAttribNV(cnstXMLNames[xmlHandle].Name).Value := ANode.Value;
//            GetEntityByNode(vNodeSrc, AResult.Errors, vHandles, vSrcEnts);
//            if vSrcEnts.Count = 1 then
//              Result := vSrcEnts[0];
//          finally
//            vNodeSrc.Free;
//          end;
//        finally
//          vSrcEnts.Free;
//        end;
//      finally
//        vHandles.Free;
//      end;
//    end;
//  end;

  function GetParentEntity(const ANode: TsgNodeSample): TsgDXFEntity;
  const
    cnstParentSections: array [0..1] of TConvSection = (csLayouts, csBlocks);
  var
    I: Integer;
    vEntities: TsgDXFEntity;
    vHandle: UInt64;
    vIndex: Integer;
  begin
    Result := nil;
    if Assigned(ANode) then
    begin
      vHandle := ANode.ValueAsInt64;
      if vHandle <> cnstBadHandle then
      begin
        for I := Low(cnstParentSections) to High(cnstParentSections) do
        begin
          vEntities := FCADImage.Converter.Sections[cnstParentSections[I]];
          if Assigned(vEntities) then
          begin
            vIndex := vEntities.IndexOfHandle(vHandle);
            if vIndex > -1 then
            begin
              Result := vEntities[vIndex];
              Break;
            end;
          end;
        end;
      end;
    end;
  end;

var
  I, vIndex: Integer;
  vDest: TsgNodeSample;
  vSourceEnt, vDestEnt: TsgDXFEntity;
  vHandles: TsgInt64List;
  vEnt: TsgDXFEntity;
  vChanged: Boolean;
  vObjEntities: TsgObjectList;
begin
  if IsInvalidRefs then
    Exit;
  vChanged := False;
  try
  vSourceEnt := GetParentEntity(ANode.GetAttributeByName(cnstXMLSource));
  if Assigned(vSourceEnt) then
  begin
    vHandles := TsgInt64List.Create;
    try
      if GetAttributeInt64List(ANode, cnstXMLHandle, vHandles, cnstDecimalSeparatorNames) > 0 then
      begin
        vHandles.Sorted := True;
        while (vHandles.Count > 0) and (vHandles.Last = cnstBadHandle) do
          vHandles.Count := vHandles.Count - 1;
        while (vHandles.Count > 0) and (vHandles.First = cnstBadHandle) do
          vHandles.Delete(0);
        if vHandles.Count > 0 then
        begin
          vDest := ANode.GetAttributeByName(cnstXMLDest);
          if Assigned(vDest) then
          begin
            vDestEnt := GetParentEntity(vDest);
            if Assigned(vDestEnt) then
            begin
              if vDestEnt <> vSourceEnt then
              begin
                if vDestEnt is TsgDXFBlock then
                  TsgDXFBlock(vDestEnt).IsLoaded := False;
                vObjEntities := TsgObjectList.Create;
                try
                  vObjEntities.Capacity := vHandles.Count;
                  try
                    for I := vSourceEnt.Count - 1 downto 0 do
                    begin
                      vEnt := vSourceEnt[I];
                      if vEnt.Handle <> cnstBadHandle then
                        vIndex := vHandles.IndexOf(vEnt.Handle)
                      else
                        vIndex := -1;
                      if vIndex > -1 then
                      begin
                        if vSourceEnt.DeleteEntity(I) <> nil then
                        begin
                          vChanged := True;
                          vObjEntities.Add(vEnt);
                          vHandles.Delete(vIndex);
                        end
                        else
                          DoMessageError(AResult.Errors, Format(cnstXMLCmdEntityDelError, [HandleToStr(vEnt.Handle)]), ANode);
                      end;
                    end;
                  finally
                    if vObjEntities.Count > 0 then
                    begin
                      for I := vObjEntities.Count - 1 downto 0 do
                      begin
                        vEnt := TsgDXFEntity(vObjEntities[I]);
                        if vDestEnt.AddEntity(vEnt) > -1 then
                        begin
                          vChanged := True;
                          AResult.Output.AddChildNV(cnstXMLDeleted).AddAttribNV(cnstXMLNames[xmlHandle].Name).ValueAsHandle := vEnt.Handle;
                        end
                        else
                        begin
                          vSourceEnt.AddEntity(vEnt);
                          DoMessageError(AResult.Errors, Format(cnstXMLCmdEntityAddError, [HandleToStr(vEnt.Handle)]), ANode);
                        end;
                      end;
                    end;
                  end;
                finally
                  vObjEntities.Free;
                end;
                if vHandles.Count > 0 then
                  DoMessageError(AResult.Errors, Format(cnstXMLCmdEntityFindError, [vHandles.ToStr(cnstDecimalSeparatorNames)]), ANode);
              end
              else
                DoMessageError(AResult.Errors, cnstXMLCmdCommandRemoveErrorSourceDestEqual, ANode);
            end
            else
              DoMessageError(AResult.Errors, cnstXMLCmdCommandRemoveErrorDest, ANode);
          end
          else
            DoMessageError(AResult.Errors, cnstXMLCmdCommandRemoveErrorDest, ANode);
        end;
      end
      else
        DoMessageError(AResult.Errors, cnstXMLCmdCommandRemoveErrorEntities, ANode);
    finally
      vHandles.Free;
    end;
  end
  else
    DoMessageError(AResult.Errors, cnstXMLCmdCommandRemoveErrorSource, ANode);
  finally
    if vChanged and Assigned(CADEditor) then
      CADEditor.Invalidate;
  end;
end;

procedure TsgXMLIDE.CommandLoad(ANode: TsgNodeSample; AResult: IsgResultNode);
var
  vFileName, vExt, vMode: string;
  vStream: TStream;
  vPGuid: PGUID;
  vGuid: TGUID;
  vFileNameAttrib, vBase64Attrib, vFormatAttrib: TsgNodeSample;
  vNodeGuid: TsgNodeSample;
  vDrawingsCount: Integer;

  procedure DoMsgError(const AMessage: string);
  begin
    CADImage := nil;
    DoMessageError(AResult.Errors, AMessage, ANode);
  end;

  function DoAfterOpen: Boolean;
  var
    I: Integer;
 begin
   Result := False;
   if vDrawingsCount < MainApplication.DrawingsCount then
   begin
     vGuid := MainApplication.CurrentDrawingGuid;
     I := 0;
     while (I < MainApplication.DrawingsCount) and not Result do
       if not IsEqualGUID(MainApplication.DrawingGuid[I].Guid, vGuid) then
         Inc(I)
       else
         Inc(Result);
      if Result then
        DrawingXMLDataToNode(MainApplication.DrawingGuid[I], AResult.Output);
    end;
    MakeMessageText(FCADEditor.ResultList, 0, AResult);
    FCADEditor.ResultList.Clear;
  end;

begin
  if not CheckForApp(ANode, AResult) then Exit;
  vDrawingsCount := MainApplication.DrawingsCount;
  vPGuid := nil;
  vFileNameAttrib := ANode.GetAttributeByName(cnstXMLCmdFile);
  if Assigned(vFileNameAttrib) then
    vFileName := vFileNameAttrib.ValueAsStr
  else
    vFileName := '';
  if Length(vFileName) > 0 then
    vFileName := Trim(vFileName);
  vMode := GetAttributeStr(ANode, cnstXMLMode, '');
  vNodeGuid := ANode.GetAttributeByName(cnstXMLCmdGuid);
  if Assigned(vNodeGuid) then
  begin
    vGuid := vNodeGuid.ValueData.ValueAsGuid;
    if not IsGuidNull(vGuid) then
      vPGuid := @vGuid;
  end;
  vBase64Attrib := ANode.GetAttributeByName(cnstXMLCmdBase64);
  if Assigned(vBase64Attrib) then
  begin
    vFileName := GetAttributeStr(ANode, cnstXMLCmdName, '');
    vStream := TMemoryStream.Create;
    try
      DecodeBase64(vBase64Attrib.ValueData.ValueAsText, nil, vStream);
      vFormatAttrib := ANode.GetAttributeByName(cnstXMLFormat);
      if Assigned(vFormatAttrib) then
        vExt := vFormatAttrib.ValueAsStr
      else
        vExt := '';
      if (vExt <> '') and (vExt[1] <> '.') then
        vExt := '.' + vExt;
      vStream.Position := 0;
      vExt := vFileName + vExt;
      if MainApplication.LoadFromStream(vStream, vExt, vMode, vPGuid) then
        DoAfterOpen
      else
        DoMsgError(sXMLInterfaceFileLoadError);
    finally
      vStream.Free;
    end;
  end
  else
    if FileExists(vFileName) or (Copy(vFileName, 1, 4) = 'http') then
    begin
{$IFDEF SG_USE_CADXML}
      if AnsiLowerCase(ExtractFileExt(vFileName)) = '.xml' then
      begin
        FCADImage.Converter.Clear;
        LoadFromXML(FCADImage, vFileName);
        DoAfterOpen;
      end
      else
{$ENDIF}
      if MainApplication.OpenFile(vFileName, True, vMode, vPGuid) then
        DoAfterOpen
      else
        DoMsgError(sXMLInterfaceFileLoadError);
    end
    else
    begin
      if (Length(vFileName) = 0) and (vPGuid <> nil) then
      begin
        if MainApplication.OpenFile(vFileName, False, cnstModeNew, vPGuid) then
          DoAfterOpen
        else
          DoMsgError(sXMLInterfaceFileNewError);
      end
      else
        DoMsgError(sXMLInterfaceFileNotFoundError);
    end;
end;

procedure TsgXMLIDE.CommandUnLoad(ANode: TsgNodeSample; AResult: IsgResultNode);
var
  vCurrentDrawingGuid, vGuid: TGUID;
  vGuidNode, vCurrNode, vIndexNode: TsgNodeSample;
  vIndex: Integer;

  function HasAttrib(const ANode: TsgNodeSample; const AName: string;
    var AAttrib: TsgNodeSample): Boolean;
  begin
    AAttrib := ANode.GetAttributeByName(AName);
    Result := Assigned(AAttrib)
  end;

begin
  if not CheckForApp(ANode, AResult) then Exit;
  vGuid := GUID_NULL;
  // Search for drawing identifier by attribute:
  // - "guid";
  if HasAttrib(ANode, cnstXMLCmdGuid, vGuidNode) then
  begin
    try
      vGuid := StrToGUID(vGuidNode.ValueAsStr);
    except
      on EConvertError do
      begin
        vGuid := GUID_NULL;
        DoMessageError(AResult.Errors, sXMLInterfaceInvalidGuid, vGuidNode);
      end;
    end;
  end;
  // - "current";
  if IsGuidNull(vGuid) and HasAttrib(ANode, cnstXMLCmdCurrent, vCurrNode) then
    vGuid := MainApplication.CurrentDrawingGuid;
  // - "index";
  if IsGuidNull(vGuid) and HasAttrib(ANode, cnstXMLCmdIndex, vIndexNode) then
  begin
    vIndex := vIndexNode.ValueAsInt;
    if (vIndex >= 0) and (vIndex < MainApplication.DrawingsCount) then
      vGuid := MainApplication.DrawingGuid[vIndex].Guid
    else
      DoMessageError(AResult.Errors, sXMLInterfaceInvalidIndex, vIndexNode);
  end;
  // if drawing identifier find
  if not IsGuidNull(vGuid) then
  begin
    try
      // get cerrent active drawing identifier
      vCurrentDrawingGuid := MainApplication.CurrentDrawingGuid;
      // set cerrent active drawing if it is not
      if not sgIsEqualIID(vCurrentDrawingGuid, vGuid) then
        MainApplication.CurrentDrawingGuid := vGuid;
      // do close drawing
      MainApplication.CloseDrawingGuid(vGuid);
    finally
      // restore previous active drawing
      if not sgIsEqualIID(vGuid, vCurrentDrawingGuid) then
        MainApplication.CurrentDrawingGuid := vCurrentDrawingGuid;
    end;
  end;
end;

procedure TsgXMLIDE.CommandSwitchDrawing(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNodeSample;
  vIndex: Integer;
  vGuid: TGUID;
  vOk: Boolean;
begin
  if not CheckForApp(ANode, AResult) then Exit;
  vOk := False;
  vNode := ANode.GetAttributeByName(cnstXMLCmdGuid);
  if Assigned(vNode) then
  begin
    try
      vGuid := StrToGUID(vNode.ValueAsStr);
      MainApplication.CurrentDrawingGuid := vGuid;
      if IsEqualGUID(MainApplication.CurrentDrawingGuid, vGuid) then
        vOk := True;
    except on EConvertError do
      DoMessageError(AResult.Errors, sXMLInterfaceInvalidGuid, vNode);
    end;
  end;

  if not vOk then
  begin
    vNode := ANode.GetAttributeByName(cnstXMLCmdIndex);
    if Assigned(vNode) then
    begin
      vIndex := vNode.ValueAsInt;
      if (vIndex >= 0) and (vIndex < MainApplication.DrawingsCount) then
      begin
        vGuid := MainApplication.DrawingGuid[vIndex].Guid;
        MainApplication.CurrentDrawingGuid := vGuid;
        if IsEqualGUID(MainApplication.CurrentDrawingGuid, vGuid) then
          vOk := True;
      end
      else
        DoMessageError(AResult.Errors, sXMLInterfaceInvalidIndex, vNode);
    end;
  end;

  if not vOk then
    DoMessageError(AResult.Errors, cnstXMLValueError, ANode);
end;

procedure TsgXMLIDE.CommandGetDrawingsList(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  I: Integer;
  vXmlGuid: TsgDrawingXMLData;
begin
  if CheckForApp(ANode, AResult) then
  begin
    for I := 0 to MainApplication.DrawingsCount - 1 do
    begin
      vXmlGuid := MainApplication.DrawingGuid[I];
      DrawingXMLDataToNode(vXmlGuid, AResult.Output.AddChildNV(cnstXMLCmdDrawing));
    end;
  end;
end;

function TsgXMLIDE.CommandToXMLNode(const ANode: TsgNode;
  const AResult: TsgNode; ACommand: TsgXMLKey): Integer;
var
  vResult, vTempResult: TsgNode;
  vPseudoResult: TsgNode;
  vTempNode: TsgNode;

  procedure AddParam(AInternalNode: TsgNode; AParam: TXMLCommParameter);
  begin
    FXMLCommParameterList.AddObject(cnsTXMLCommParameters[AParam].Name, Pointer(AParam));
    AInternalNode.AddAttribNV(cnsTXMLCommParameters[AParam].Name).ValueAsStr := cnstDataTypes[cnsTXMLCommParameters[AParam].ValueType].Name;
  end;

  procedure AddParams(AInternalNode: TsgNode; AParams: array of TXMLCommParameter);
  var
    I: Integer;
  begin
    for I := Low(AParams) to High(AParams) do
      AddParam(AInternalNode, AParams[I]);
  end;

  function AddChildParam(AInternalNode: TsgNode; AParam: TXMLCommParameter): TsgNode;
  begin
    FXMLCommParameterList.AddObject(cnsTXMLCommParameters[AParam].Name, Pointer(AParam));
    Result := AInternalNode.AddChildNV(cnsTXMLCommParameters[AParam].Name);
    Result.ValueAsStr := cnstDataTypes[cnsTXMLCommParameters[AParam].ValueType].Name;
  end;

  procedure AddChildParams(AInternalNode: TsgNode; AParams: array of TXMLCommParameter);
  var
    I: Integer;
  begin
    for I := Low(AParams) to High(AParams) do
      AddChildParam(AInternalNode, AParams[I]);
  end;


  function CreateOutput(AInternalResult: TsgNode): TsgNode;
  begin
    AInternalResult.Name := cnstXMLResult;
    AddParam(AInternalResult, cpInstruction);
    Result := AddChildParam(AInternalResult, cpOutput);
    AddChildParam(AInternalResult, cpErrors);
  end;

begin
  Result := 0;
  vPseudoResult := nil;
  if Assigned(AResult) then
    vResult := CreateOutput(AResult)
  else
  begin
    vPseudoResult := TsgNode.Create;
    vResult := CreateOutput(vPseudoResult);
  end;
  try
    case ACommand of
      xkUndefined: ;
      xkGet:
        begin
          AddParams(ANode, [cpHandle, cpPathname]);
          //result:
          AddChildParam(vResult, cpDrawingData);
        end;
      xkGetCADPoint:
        begin
          AddParams(ANode, [cpPoint]);
        end;
      xkAdd:
        begin
          AddParams(ANode, [cpDrawingData]);
          //result
          vTempResult := AddChildParam(vResult, cpCreated);
          AddParams(vTempResult, [cpHandle]);
          vTempResult := AddChildParam(vResult, cpUpdated);
          AddParams(vTempResult, [cpHandle]);
        end;
      xkSelect:
        begin
          AddParams(ANode, [cpHandle, cpMarker, cpPathname, cpSelectMode]);
        end;
      xkApply:
        begin
          AddParams(ANode, [cpParameters]);
        end;
      xkDelete: ;
      xkLoad:
        begin
          AddParams(ANode, [cpFileName, cpBase64]);
          //result
          AddParams(vResult, [cpGuid, cpFileName, cpIndex]);
        end;
      xkUnload:
        begin
          AddParams(ANode, [cpIndex, cpGuid]);
        end;
      xkSwitchDrawing:
        begin
          AddParams(ANode, [cpIndex, cpGuid]);
        end;
      xkGetDrawingsList:
        begin
          //result
          vTempResult := AddChildParam(vResult, cpDrawing);
          AddParams(vTempResult, [cpGuid, cpFileName, cpIndex, cpDrawingMode]);
        end;
      xkGetImage:
        begin
          AddParams(ANode, [cpTop, cpLeft, cpRight, cpBottom, cpWidth, cpHeight]);
          //result:
          AddChildParam(vResult,cpBase64);
        end;
      xkContextMenu:
      begin
        AddParams(ANode, [cpMode]);
        AddParams(AddChildParam(AddChildParam(ANode, cpItems), cpItem),[cpCaption]);
        //result:
        AddChildParam(vResult, cpParameters);
      end;
     /// xkItem: AddParams(ANode, [cpCaption]);
      xkRibbon:
        begin
          AddParams(ANode, [cpVisible]);
        end;
      xkCommand:
        begin
          AddParams(ANode, [cpText]);
          //result
          AddParams(vResult, [cpResult]);
        end;
      xkGetSelected:
        begin
          //result:
          AddParams(vResult, [cpSelectedCount]);
          vTempResult := AddChildParam(vResult, cpSelected);
          AddParams(vTempResult, [cpHandle]);
        end;
      xkFitToSize: ;
      xkSignToEvent:
      begin
        AddParams(ANode, [cpEvent, cpEnabled]);
      end;
      xkUnSelect:
        begin
          AddParams(ANode, [cpMarker, cpHandle, cpPathname]);
        end;
      xkGetViewRect:
        begin
          // result
          vTempResult := AddChildParam(vResult, cpVisibleArea);
          AddParams(vTempResult, [cpWidth, cpHeight]);
          vTempResult := AddChildParam(vResult, cpCADArea);
          AddParams(vTempResult, [cpRect]);
        end;
      xkSetViewRect:
        begin
          AddParams(ANode, [cpRect]);
        end;
      xkGetBox:
        begin
          AddParams(ANode, [cpHandle, cpPathname]);
          //result
          AddParams(vResult, [cpBox]);
        end;
      xkShowSelectedEntities:
        begin
          AddParams(ANode, [cpShowEntityMethod]);
        end;
      xkSave:
        begin
          vTempNode := AddChildParam(ANode, cpExportParams);
          AddChildParams(vTempNode, [cpWidth, cpHeight, cpProportional, cpBitPerPixel,
              cpMeasureInPixels, cpTransparent, cpDPUX, cpDPUY, cpCompression,
              cpPageHeight, cpAuthor, cpLayoutExportMode, cpLayoutNameExportMode,
              cpVersion, cpIsConvertImageToOLE]);
        end;
      xkCreateHatch:
        begin
          AddParams(ANode, [cpHatchName, cpAngle, cpPatternScale, cpColor]);
          //result
          vTempNode := AddChildParam(vResult, cpSelected);
          AddParams(vTempNode, [cpHandle]);
        end;
      xkGetDrawingChanged:
        begin
          //result
          AddParams(vResult, [cpChanged]);
        end;
      xkSetDrawingChanged:
        begin
          AddParams(ANode, [cpGuid, cpSetDrawingMode, cpChanged]);
        end;
      xkGetView:  ;
      xkSetView: {ANode.AddChildNV(TsgDXFVport.GetPropertyName).ValueAsStr := TsgDXFVport.GetPropertyName};
      xkCustomSelectMode:
      begin
        AddParams(ANode, [cpCustomDraw, cpReadOnly, cpAskOnDelete, cpColor,
          cpLineWeight, cpHandle]);
      end;
      xkEditor:
      begin
        AddParams(ANode, [cpReadOnly, cpShowEntityPrecision, cpShowEntityMethod]);
      end;
      xkRegistration:
        begin
          AddParams(ANode, [cpUser, cpEMail, cpKey]);
          // result
          AddParams(vResult, [cpRegistration]);
        end;
      xkSetSpecialCustomInsert:
        begin
          AddParams(ANode, [cpHandle, cpColor, cpLineWeight]);
        end;
      xkSupportedCommandsList: ;
      xkSupportedClassesList: ;
      xkInvalidate:
        begin
          AddParams(ANode, [cpBox]);
        end;
      xkHelp:
        begin
          AddParams(ANode, [cpText, cpFileName, cpPathToXMLExamples]);
          // result
        end;
      xkCaptureScreen:
        begin
          //result
          AddParams(vResult, [cpFormat, cpBase64]);
        end;
      xkGetExternalFiles:
        begin
          //result
          vTempNode := AddChildParam(vResult, cpExternalFile);
          AddParams(vTempNode, [cpFileName]);
        end;
      xkMenuButton:
        begin
           AddParams(ANode, [cpButtonCaption])
        end;
    end;
  finally
    if Assigned(vPseudoResult) then
      vPseudoResult.Free;
  end;
end;

procedure TsgXMLIDE.CommandGetImage(ANode: TsgNodeSample;
  AResult: IsgResultNode);
{$IFDEF SG_USE_EXPORT}
var
  vGraphic: TGraphic;
  vBitmap: TBitmap;
  vStream: TMemoryStream;
  vStreamBase64:TStringStream;
  vNodeExportParams: TsgNode;
  vDrawRect: TRect;
  vExportParams: TsgCommonExportParams;
  vFileName: string;
  vBackgroundColor: Integer;
begin
  vFileName := '';
  vStream := nil;
  try
    vNodeExportParams := TsgNode(ANode.GetChildByName(cnstExportParams));
    if Assigned(vNodeExportParams) then
    begin
      vExportParams := TsgCommonExportParams.Create;
      try
        vExportParams.FromNode(vNodeExportParams, nil);
        if vExportParams.IsDestinationFile then
          vFileName := vExportParams.SaveFileName;
        case vExportParams.Cliping.ClipMode of
          6:
            begin
              vBitmap := TBitmap(MainApplication.GetActualGraphic(vExportParams));
              vStream := TMemoryStream.Create;
              if vExportParams.Format = efAuto then
                vExportParams.Format := efBitmap;
               MainApplication.SaveBitmapToStream(vBitmap, vExportParams, '', vStream);
            end;
        else
          vGraphic := TGraphic(MainApplication.GetActualGraphic(vExportParams));

          if Assigned(vGraphic) then
          begin
            vBackgroundColor := clEmpty;
            vDrawRect := Rect(0, 0, vExportParams.RasterExportParams.Width,
              vExportParams.RasterExportParams.Height);
            if vGraphic is TsgCADImage then
            begin
              vBackgroundColor := TsgCADImage(vGraphic).BackgroundColor;
              case vExportParams.Cliping.ClipMode of
                4:  vDrawRect := MakeRectFromFRect(vExportParams.Cliping.ClipRect);
              end;
            end;
            vBitmap := TBitmap.Create;
            try
              try
                vBitmap.PixelFormat := vExportParams.RasterExportParams.Depth;
                vBitmap.Width := vExportParams.RasterExportParams.Width;
                vBitmap.Height := vExportParams.RasterExportParams.Height;
                SetBitmapTransparent(vBitmap,
                  vExportParams.RasterExportParams.Transparent, vBackgroundColor);
                vBitmap.Canvas.StretchDraw(vDrawRect, vGraphic);
                vStream := TMemoryStream.Create;
                if vExportParams.Format = efAuto then
                  vExportParams.Format := efBitmap;
                MainApplication.SaveBitmapToStream(vBitmap, vExportParams, '', vStream);
              except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
              end;
            finally
              vBitmap.Free;
            end;
          end;
        end;
      finally
        vExportParams.Free;
      end;
    end;
    if Assigned(vStream) and (vStream.Size > 0) then
    begin
      vStream.Position := 0;
      vStreamBase64 := TStringStream.Create('');
      try
        EncodeBase64(TStream(vStream), TStream(vStreamBase64));
        AResult.Output.AddAttribNV(cnstXMLCmdBase64).Value := vStreamBase64.DataString;
      finally
        FreeAndNil(vStreamBase64);
      end;
      if Length(vFileName) > 0 then
      begin
        vStream.Position := 0;
        try
          vStream.SaveToFile(vFileName);
        except
{$IFDEF _FIXINSIGHT_} on E: Exception do EmptyExceptionHandler(E);{$ENDIF}
        end;
      end;
    end
    else
      DoMessageError(AResult.Errors, sXMLInterfaceComFrmError, ANode);
  finally
    FreeAndNil(vStream);
  end;
{$ELSE}
begin
  DoMessageError(AResult.Errors, sXMLInterfaceComFrmError, ANode);
{$ENDIF}
end;

procedure TsgXMLIDE.CommandContextMenu(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vList: TStringList;
  vClicks, vPopups, vSenders, vImageIndexes: TList;
  vAttrib, vChildNode: TsgNodeSample;
  vMode, vImageIndex: Integer;
  vFlags, I: Integer;
  vClick, vPopup, vSender: Pointer;
  vCaption, vBase64Icons: string;
  vItems, vIcons: TsgNodeSample;
  vStream: TStream;
  vBitmap: TBitmap;
begin
  vBitmap := nil;
  try
    if not CheckForApp(ANode, AResult) then Exit;
    if ANode.HasAttribute(cnstXMLCmdMode) then
    begin
      vAttrib := ANode.GetAttributeByName(cnstXMLCmdMode);
      vMode := vAttrib.ValueAsInt
    end
    else
      vMode := 1;
    vItems := nil;
    if ANode.ChildNodesCount > 0 then
    begin
      vIcons := ANode.GetChildByName(cnstXMLContextMenuIcons);
      if Assigned(vIcons) then
      begin
        vBase64Icons := GetAttributeStr(vIcons, cnstXMLContextMenuIconsCode, '');
        if Length(vBase64Icons) > 0 then
        begin
          vStream := TMemoryStream.Create;
          try
            DecodeBase64(vBase64Icons, nil, vStream);
            vStream.Position := 0;
            if vStream.Size > 0 then
            begin
              vBitmap := TBitmap.Create;
              vBitmap.LoadFromStream(vStream);
            end;
          finally
            vStream.Free;
          end;
        end;
      end;
      vItems := ANode.GetChildByName(cnstXMLCmdItems);
      if not Assigned(vItems) then
        vItems := ANode;
    end;
    if Assigned(vItems) then
    begin
      vList := TStringList.Create;
      vClicks := TList.Create;
      vPopups := TList.Create;
      vSenders := TList.Create;
      vImageIndexes := TList.Create;
      try
        for I := 0 to vItems.ChildNodesCount - 1 do
        begin
          vChildNode := vItems.ChildNodes[I];
          if CompareText(vChildNode.Name, cnstXMLCmdItem) = 0 then
          begin
            vAttrib := vChildNode.GetAttributeByName(cnstXMLCmdCaption);
            if Assigned(vAttrib) then
            begin
              vCaption := vAttrib.ValueData.ValueAsStr;
              vClick := nil;
              vPopup := nil;
              vSender := nil;
              vFlags := 0;

              vAttrib := vChildNode.GetAttributeByName(cnstXMLCmdSender);
              if Assigned(vAttrib) then
                vSender := vAttrib.ValueAsPointer;

              vAttrib := vChildNode.GetAttributeByName(cnstXMLCmdOnClick);
              if Assigned(vAttrib) then
                vClick := vAttrib.ValueAsPointer;

              vAttrib := vChildNode.GetAttributeByName(cnstXMLCmdOnPopup);
              if Assigned(vAttrib) then
                vPopup := vAttrib.ValueAsPointer;

              vAttrib := vChildNode.GetAttributeByName(cnstXMLCmdVisible);
              if Assigned(vAttrib) then
                vFlags := Integer(not vAttrib.ValueAsBool);

              vAttrib := vChildNode.GetAttributeByName(cnstXMLCmdEnable);
              if Assigned(vAttrib) then
                vFlags := vFlags or (Integer(not vAttrib.ValueAsBool) shl 1);

              vImageIndex := GetAttributeInt(vChildNode, cnstXMLCmdImageIndex, -1);

              vList.AddObject(vCaption, Pointer(vFlags));
              vClicks.Add(vClick);
              vPopups.Add(vPopup);
              vSenders.Add(vSender);
              vImageIndexes.Add(TObject(vImageIndex));
            end;
          end;
        end;
        MainApplication.CreateUserMenu(vList, vSenders,
          vClicks, vPopups, vImageIndexes, vMode, vBitmap);
      finally
        vPopups.Free;
        vClicks.Free;
        vList.Free;
        vSenders.Free;
        vImageIndexes.Free;
      end;
    end;
  finally
    vBitmap.Free;
  end;
end;

procedure TsgXMLIDE.CommandMenuButton(ANode: TsgNodeSample; AResult: IsgResultNode);
begin
  CommandApplyXML(ANode, AResult, False);
end;

procedure TsgXMLIDE.CommandRegistration(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vUser, vEMail, vKey: string;
  vNode: TsgNode;
  vResult: Integer;
begin
  if not CheckForApp(ANode, AResult) then Exit;
  vUser := '';
  vEMail:= '';
  vKey:= '';
  if Assigned(ANode.GetAttributeByName(cnstXMLCmdUser)) then
    vUser := ANode.GetAttributeByName(cnstXMLCmdUser).ValueAsStr;
  if Assigned(ANode.GetAttributeByName(cnstXMLCmdEMail)) then
    vEMail := ANode.GetAttributeByName(cnstXMLCmdEMail).ValueAsStr;
  if Assigned(ANode.GetAttributeByName(cnstXMLCmdKey)) then
    vKey := ANode.GetAttributeByName(cnstXMLCmdKey).ValueAsStr;
{$IFDEF SG_DELPHI_VCL}
{$IFDEF SGAPPPROTECT}
  vResult := AppProtect.StRg(PChar(vUser), PChar(vEMail), PChar(vKey));
{$ELSE}
  vResult := 0;
{$ENDIF}
{$ELSE}
  vResult := 0;
{$ENDIF}
  vNode := AResult.Output;
  vNode.AddAttribNV(cnstXMLCmdRegistration).ValueAsInt := vResult;
end;

procedure TsgXMLIDE.CommandRibbon(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNodeSample;
begin
  if CheckForApp(ANode, AResult) then
  begin
    vNode := ANode.GetAttributeByName(cnstXMLCmdVisible);
    if Assigned(vNode) then
      MainApplication.SetRibbonVisible(vNode.ValueAsBool);
  end;
end;

procedure TsgXMLIDE.CommandCaptureScreen(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vBmp: TBitmap;
{$IFNDEF SG_FIREMONKEY}
  vMemoryStream: TMemoryStream;
  vFormat: TsgExportFormat;
  vExportGraphic: TGraphic;
{$ENDIF}

  function WindowSnap(AWindowHandle: HWND; ABmp: TBitmap): Boolean;
{$IFNDEF SG_NON_WIN_PLATFORM}
  var
    vRect: TRect;
    vUser32DLLHandle: THandle;
    vPrintWindowAPI: function(sourceHandle: HWND; destinationHandle: HDC;
      nFlags: UINT): BOOL; stdcall;
{$ENDIF}
  begin
     Result := False;
{$IFNDEF SG_NON_WIN_PLATFORM}
     vUser32DLLHandle := GetModuleHandle(user32);
     if vUser32DLLHandle <> 0 then
     begin
       @vPrintWindowAPI := GetProcAddress(vUser32DLLHandle, 'PrintWindow');
       if @vPrintWindowAPI <> nil then
       begin
         GetWindowRect(AWindowHandle, vRect) ;
         ABmp.Width := vRect.Right - vRect.Left;
         ABmp.Height := vRect.Bottom - vRect.Top;
         ABmp.Canvas.Lock;
         try
           Result := vPrintWindowAPI(AWindowHandle, ABmp.Canvas.Handle, 0) ;
         finally
           ABmp.Canvas.Unlock;
         end;
       end;
     end;
{$ENDIF}
  end;
begin
  vBmp := TBitmap.Create;
  try
    if WindowSnap(HWNDMainForm, vBmp) then
    begin
{$IFNDEF SG_FIREMONKEY}
      vMemoryStream := TMemoryStream.Create;
{$IFDEF SG_USE_PNG}
      vExportGraphic :={$IFDEF SGDEL_2009}TPngImage{$ELSE}TPngObject{$ENDIF}.Create;
      vFormat := efPng;
{$ELSE}
      vExportGraphic := TJpegImage.Create;
      vFormat := efJpeg;
{$ENDIF}
      try
        vExportGraphic.Assign(vBmp);
        vExportGraphic.SaveToStream(vMemoryStream);
        vMemoryStream.Position := 0;
        ConvertStreamToOutputBase64(vMemoryStream,
          AResult.Output, vFormat);
      finally
        vMemoryStream.Free;
        vExportGraphic.Free;
      end;
{$ELSE}
  // TODO: for Firemonkey
{$ENDIF}
    end;
  finally
    vBmp.Free;
  end;
end;

procedure TsgXMLIDE.CommandCommand(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vResults: TsgStringList;
  vResCode: Integer;
  vCommand: string;
begin
  if not Assigned(CADEditor) then
    Exit;
  vResults := TsgStringList.Create;
  try
    vResults.Delimiter := cnstCommaPoint;
    vCommand := ANode.GetAttributeByName(cnstXMLCmdText).Value;
    vResCode := CADEditor.LispRun(vCommand, vResults);
    MakeMessageText(vResults, vResCode, AResult);
    if vResCode = cnstLispCommandSendError then
      AResult.Errors.AddAttribNV(cnstXMLCommand, vCommand);
  finally
    vResults.Free;
  end;
end;

procedure TsgXMLIDE.CommandAppCommand(ANode: TsgNodeSample; AResult: IsgResultNode);
const
  cnstNames: array[0..1] of string = ('ApplyViewMode', 'GetEntityBox');
var
  vResults: TsgStringList;
  vIndexFunction,  vResCode, vViewModeIndex, vHandle: Integer;
  vViewModeName, vCommand: string;
  vMatrix: TFMatrix;
  vIterate: TsgXMLIDEIteateEntities;
begin
  if not Assigned(CADEditor) then
    Exit;
  vIndexFunction := -1;
  vCommand := GetAttributeStr(ANode, cnstXMLName, '');
  if Length(vCommand) > 0 then
    vIndexFunction := IndexOfStrings(vCommand, cnstNames, @sgCompareTxt);
  vResults := TsgStringList.Create;
  try
    vResults.Delimiter := cnstCommaPoint;
    case vIndexFunction of
      0://ApplyViewMode
      begin
        vResCode := cnstLispCommandSendError;
        vViewModeIndex := GetAttributeInt(ANode, cnstXMLValue, Ord(Low(TsgDXFViewDirection)) - 1);
        if (vViewModeIndex >= Ord(Low(TsgDXFViewDirection))) and (vViewModeIndex <= Ord(High(TsgDXFViewDirection))) then
        begin
          vResCode := cnstLispCommandSendOk;
          FCADImage.RotToView(TsgDXFViewDirection(vViewModeIndex))
        end
        else
        begin
          vViewModeName := GetAttributeStr(ANode, cnstXMLMode, '');
          if Length(vViewModeName) > 0 then
            vResCode := CADEditor.LispRun(vViewModeName, vResults);
        end;
        if vResCode <> cnstLispCommandSendError then
        begin
          vMatrix := cnstIdentityMat;
          vMatrix.EX := FcadImage.GetRealImagePoint(cnstXOrtAxis);
          vMatrix.EY := FcadImage.GetRealImagePoint(cnstYOrtAxis);
          vMatrix.EZ := FcadImage.GetRealImagePoint(cnstZOrtAxis);
          //SetDrawingMatrixEx direction Y negative
          vMatrix.EX.Y := -vMatrix.EX.Y;
          vMatrix.EY.Y := -vMatrix.EY.Y;
          vMatrix.EZ.Y := -vMatrix.EZ.Y;
          //
          FcadImage.CustomDraw := False;
          TsgCADImageAccess(FcadImage).SetDrawingMatrixEx(vMatrix);
          CADEditor.LispRun('FittoWindow', vResults);
        end
        else
          MakeMessageText(vResults, cnstLispCommandSendError, AResult)
      end;
      1://GetEntityBox
      begin
        //vResCode := cnstLispCommandSendError;
        vHandle := GetAttributeInt64(ANode, cnstXMLValue, cnstBadHandle);
        if vHandle <> cnstBadHandle then
        begin
          vIterate := TsgXMLIDEIteateEntities.Create(Self);
          try
            vIterate.FindEntity(ANode, AResult);
          finally
            vIterate.Free;
          end;
        end
        else
        begin
          AResult.Errors.AddAttribNV(cnstXMLName, vCommand);
          DoMessageError(AResult.Errors, sXMLInterfaceInvalidHandle, ANode);
        end;
      end;
    else
      MakeMessageText(vResults, cnstLispCommandSendError, AResult);
      AResult.Errors.AddAttribNV(cnstXMLName, vCommand);
    end;
  finally
    vResults.Free;
  end;
end;

procedure TsgXMLIDE.CommandMainAppFunction(ANode: TsgNodeSample; AResult: IsgResultNode);
begin
  if MainApplication <> nil then
    MainApplication.CommandXml(SaveNodeToXMLString(ANode), Self);
end;

procedure TsgXMLIDE.CommandGetSelected(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vSelectedEntities: TList;
  vNode: TsgNode;
  vAttrib: TsgNodeSample;
  I: Integer;
  vMode: TsgXMLModes;
begin
  if not Assigned(CADEditor) then
   Exit;
  vSelectedEntities := TList.Create;
  try
    CADEditor.GetSelectedEntities(vSelectedEntities);
    AResult.Output.AddAttribNV(cnstXMLCmdSelectedCount).ValueAsInt :=
      vSelectedEntities.Count;
    if vSelectedEntities.Count > 0 then
    begin
      vAttrib := ANode.GetAttributeByName(cnstXMLMode);
      if Assigned(vAttrib) then
      begin
        vMode := sgIntToXMLModes(vAttrib.ValueAsInt);
        ExpEntitiesToXML(vSelectedEntities, AResult.Output, vMode);
      end
      else
      begin
        for I := 0 to vSelectedEntities.Count - 1 do
        begin
           vNode := AResult.Output.AddChildNV(cnstXMLCmdSelected);
           vAttrib := vNode.AddAttribNV(cnstXMLCmdHandle, '');
           vAttrib.ValueAsHandle := TsgDXFEntity(vSelectedEntities[I]).Handle;
        end;
      end;
    end;
  finally
    vSelectedEntities.Free;
  end;
end;

procedure TsgXMLIDE.CommandReport(ANode: TsgNodeSample; AResult: IsgResultNode);
var
//  I: Integer
  vRez: Integer;
  vInXML: string;
begin
  if CheckForApp(ANode, AResult) then
  begin
//    if AUseChilds then
//    begin
//      for I := 0 to ANode.ChildNodesCount - 1 do
//        CommandApplyXML(ANode.ChildNodes[I], AResult, False);
//    end
//    else
    begin
      vInXML := SaveNodeToXMLString(ANode);
      vRez := MainApplication.CommandReport(vInXML, Self);
      case vRez of
        -1:  DoMessageError(GetIResultNode.Errors,
               Format(cnstXMLMessageUnsupportedCommand,[ANode.Name]), ANode);
      end;
    end;
  end;
end;

procedure TsgXMLIDE.CommandSignToEvent(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNodeEvent, vNodeEnabled: TsgNodeSample;
  vOutput: TsgNode;
  vSet: Boolean;
  vCmdStr: string;
  J, vEventType: TsgXMLEvent;
begin
  vNodeEvent := ANode.GetAttributeByName(cnstXMLCmdEvent);
  if Assigned(vNodeEvent) then
  begin
    vEventType := xeUndefined;
    vCmdStr := LowerCase(vNodeEvent.ValueAsStr);
    for J := Low(TsgXMLEvent) to High(TsgXMLEvent) do
    begin
      if CompareText(vCmdStr, cnstEvents[J].Name) = 0 then
      begin
        vEventType := J;
        Break;
      end;
    end;
    if vEventType <> xeUndefined then
    begin
      vSet := True;
      vNodeEnabled := ANode.GetAttributeByName(cnstXMLEnabled);
      if Assigned(vNodeEnabled) then
        vSet := vNodeEnabled.ValueAsBool;
      if vSet then
        Include(FXMLEvents, vEventType)
      else
        Exclude(FXMLEvents, vEventType);
    end;
  end
  else
  begin
    for J := Low(TsgXMLEvent) to High(TsgXMLEvent) do
    begin
      if J = xeUndefined then
        Continue;
      vOutput := AResult.Output.AddChildNV(cnstXMLCmdSignToEvent);
      vOutput.AddAttribNV(cnstXMLCmdEvent).ValueAsStr := cnstEvents[J].Name;
      vOutput.AddAttribNV(cnstXMLEnabled).ValueAsBool := J in FXMLEvents;
    end;
  end;
end;

procedure TsgXMLIDE.CommandSpecialLayers(ANode: TsgNodeSample; AResult: IsgResultNode);
var
  I, vRez: Integer;
  vNode: TsgNodeSample;
  vName, vBlocked, vNoSelected: string;
begin
  if Assigned(FCADEditor) then
  begin
    vBlocked := '';
    vNoSelected := '';
    for I := 0 to ANode.ChildNodesCount - 1 do
    begin
      vNode := ANode.ChildNodes[I];
      vName := '';
      if SameText(vNode.Name, cnstXmlItem) then
        vName := GetAttributeStr(vNode, cnstXMLName, '');
      if Length(vName) > 0 then
      begin
        if SameText(vName, cnstXmlLayersBlocked) then
          vBlocked := GetAttributeStr(vNode, cnstXMLValue);
        if SameText(vName, cnstXmlLayersNoSelected) then
          vNoSelected := GetAttributeStr(vNode, cnstXMLValue);
      end;
    end;
    vRez := FCADEditor.SetSpecialLayers(vBlocked, vNoSelected);
    case vRez of
      -1: DoMessageError(GetIResultNode.Errors, Format(cnstXMLMessageUnsupportedCommand,[ANode.Name]), ANode);
    else
       AResult.Output.AddAttribNV(cnstXMLCmdResult).ValueAsInt := vRez;
    end;
  end;
end;

procedure TsgXMLIDE.CommandGlobalVariable(ANode: TsgNodeSample; AResult: IsgResultNode);
var
  vHeader: TsgDXFSection;
  vName: string;
  vNode: TsgNode;
  vVariables, vVariable: TsgNodeSample;
  vVariablesValue: TsgStringList;
  vNames: TsgStringList;
  I: Integer;
begin
  if Assigned(FCADImage) then
  begin
    vHeader := TsgDXFConverterAccess(FCADImage.Converter).GetHeaderSection;
    if Assigned(vHeader) then
    begin
      vName := GetAttributeStr(ANode, cnstXMLName, '');
      if Length(vName) > 0 then
      begin
        vNode := TsgNode.Create;
        try
          vHeader.ToXML(vNode, [xmlGetDefaultValue]);
          vVariables := vNode.GetChildByName(TsgDXFSectionHeader.GetXMLName);
          if Assigned(vVariables) and (vVariables.AttributeNodesCount > 1)  then
          begin
            vVariablesValue := nil;
            vNames := TsgStringList.Create;
            try
              vNames.LineBreak := cnstDecimalSeparatorNames;
              vNames.Text := vName;
              DeleteEmptyStrings(vNames);
              if vNames.Count > 1 then
                vVariablesValue := TsgStringList.Create;
              for I := 0 to vNames.Count - 1 do
              begin
                vName := vNames[I];
                vVariable := vVariables.GetAttributeByName(vName);
                if Assigned(vVariable) then
                begin
                  if Assigned(vVariablesValue) then
                    vVariablesValue.Add(vName + vVariablesValue.NameValueSeparator + vVariable.ValueAsStr)
                  else
                  begin
                    AResult.Output.AddAttribNV(cnstXMLCmdResult).ValueAsStr := vVariable.ValueAsStr;
                    AResult.Output.AddAttribNV(cnstXMLName).ValueAsStr := vName;
                  end;
                end
                else
                  DoMessageError(GetIResultNode.Errors, Format(cnstXMLMessageUnsupportedCommand,[ANode.Name, vName]), ANode);
              end;
              if Assigned(vVariablesValue) then
                AddItemsToOutput(vVariablesValue);
            finally
              FreeAndNil(vVariablesValue);
              FreeAndNil(vNames);
            end;
          end;
        finally
          FreeAndNil(vNode);
        end;
      end;
    end
    else
      DoMessageError(GetIResultNode.Errors, Format(cnstXMLMessageUnsupportedCommand,[ANode.Name]), ANode);
  end;
end;

procedure TsgXMLIDE.CommandSaveHandles(ANode: TsgNodeSample; AResult: IsgResultNode);
begin
  if Assigned(FCADImage) then
    TsgDXFConverterAccess(FCADImage.Converter).ConvExtended.GetSaveHandlesToNode(AResult.Output);
end;

procedure TsgXMLIDE.CommandSupportedClassesList(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode: TsgNode;
  I: Integer;
  vEntity: TsgListObject;
  vListObjectClass: TsgListObjectClass;
begin
  vNode := AResult.Output;
  vEntity := nil;
  for I := 0 to GlobalsgTypes.Count - 1 do
  begin
    vListObjectClass := TsgListObjectClass(GlobalsgTypes.MetaClass[I]);
    if (vListObjectClass = TsgDXFConverter) or (vListObjectClass = TsgDXFSectionEntities) then
      Continue
    else
      vEntity := TsgListObjectClass(GlobalsgTypes.MetaClass[I]).Create;
    if vEntity = nil then
      Continue;
    try
      vEntity.ToXML(vNode, [xmNoSubEntitiesNode]);
    finally
      FreeAndNil(vEntity);
    end;
  end;
  CommandHelp(RootOut);
end;

function TsgXMLIDE.CommandHelp(ANode: TsgNodeSample): string;

  procedure ProcessAttribs(ANodeHelp: TsgNodeSample);
  var
    J: Integer;
    S: string;
  begin
    for J := 0 to ANodeHelp.AttributeNodesCount - 1 do
    begin
      S := ANodeHelp.AttributeNodes[J].Name;
      if S <> csntNestedStructure then
        ANodeHelp.AttributeNodes[J].Value := GetXMLHelp(GetXMLType(ANodeHelp.AttributeNodes[J].Name));
    end;

  end;

var
  I: Integer;
  S: string;
  vStrList: TsgStringList;
  vNode: TsgNodeSample;
  vNodeAttr: TsgNode;
begin
  Result := '';
  if ANode.ChildNodesCount = 0 then
  begin
    vNode := ANode.GetAttributeByName(cnsTXMLCommParameters[cpPathToXMLExamples].Name);
    if vNode <> nil then
      FPathToXMLExamples := vNode.Value;
    vNode := ANode.GetAttributeByName(cnsTXMLCommParameters[cpFileName].Name);
    if (vNode <> nil) and (vNode.Value <> '') then
      SaveToHTMLHelp(vNode.Value);
    vNode := ANode.GetAttributeByName(cnsTXMLCommParameters[cpText].Name);
    if vNode <> nil then
      S := vNode.Value;
    if S <> '' then
    begin
      vStrList := TsgStringList.Create;
      try
        GetHints(S, TStringList(vStrList));
        vNodeAttr := RootOut.AddChildNV('Attributes');
        for I := 0 to vStrList.Count - 1 do
          vNodeAttr.AddAttribNV(vStrList.Names[I]).Value := vStrList.ValueFromIndex[I];
      finally
        vStrList.Free;
      end;
    end;
    Exit;
  end;
  if RootOut <> ANode then
  begin
    ProcessAttribs(ANode);
    S := ANode.Name;
    if (Copy(S, 1, 3) = cnstXMLPrefix) then
      Delete(S, 1, 3);
    S := InsertSpace(S);
    if (S <> 'Comment') then
      TsgNode(ANode).AddAttribNV('Description', S);
  end;
  for I := 0 to ANode.ChildNodesCount - 1 do
    CommandHelp(ANode.ChildNodes[I]);
end;

procedure TsgXMLIDE.CommandInernalIterate(ANode: TsgNodeSample;
  AResult: IsgResultNode; const ACommand: Integer);
var
  vIterate: TsgXMLIDEIteateEntities;
begin
  if IsInvalidRefs then
    Exit;
  vIterate := TsgXMLIDEIteateEntities.Create(Self);
  try
    case ACommand of
      0: vIterate.FindText(ANode, AResult);
      1: vIterate.IteratorEntities(ANode, AResult);
    end;
  finally
    vIterate.Free;
  end;
end;

procedure TsgXMLIDE.CommandSupportedCommandsList(ANode: TsgNodeSample;
  AResult: IsgResultNode);
var
  vNode, vChild: TsgNode;
  //vParam: string;
  I: TsgXMLKey;
begin
  vNode := AResult.Output;
  for I := Low(TsgXMLKey) to High(TsgXMLKey) do
  begin
    if I in [xkGetBox] then
      Continue;
    vChild := vNode.AddChildNV(cntKeyWords[I].Name);
//    vParam := cntKeyWords[I].InputParams;
//    if vParam = '' then
//      vParam := cnstXmlEmptyParams;
    CommandToXMLNode(vChild, nil, TsgXMLKey(I));
   // vChild.AddAttribNV(cnstXMLCMdDecription).ValueAsStr := cntKeyWords[I].Description;
  //  vChild.AddAttribNV(cnstXMLCMdInputParam).ValueAsStr := vParam;
  //  vChild.AddAttribNV(cnstXMLCMdReturningValues).ValueAsStr := cntKeyWords[I].ReturningValues;
  end;
  CommandHelp(RootOut);
end;

function TsgXMLIDE.CheckForApp(ANode: TsgNodeSample; AResult: IsgResultNode): Boolean;
begin
  Result := Assigned(MainApplication);
  if not Result then
    DoMessageError(AResult.Errors, sXMLInterfaceMainApplicationError, ANode);
end;

procedure TsgXMLIDE.CheckResults;
var
  vResultsNode: TsgNode;
  I, vErrorsSum: Integer;

  procedure CheckNodeName(const ANode: TsgNode; const AName: string;
    const ASum: PInteger);
  var
    vNode: TsgNode;
  begin
    vNode := TsgNode(ANode.GetChildByName(AName));
    if Assigned(vNode) then
    begin
      if ASum <> nil then
        ASum^ := ASum^ + vNode.ChildNodesCount;
      if (vNode.ChildNodesCount = 0) and (vNode.AttributeNodesCount = 0) then
      begin
        if ANode.RemoveChild(vNode) then
          vNode.Free;
      end;
    end;
  end;

begin
  vResultsNode := TsgNode(RootOut.GetChildByName(cnstXMLResults));
  if Assigned(vResultsNode) then
  begin
    vErrorsSum := 0;
    for I := 0 to vResultsNode.ChildNodesCount - 1 do
    begin
      CheckNodeName(TsgNode(vResultsNode.ChildNodes[I]), cnstXMLOutput, nil);
      CheckNodeName(TsgNode(vResultsNode.ChildNodes[I]), cnstXMLErrors, @vErrorsSum);
    end;
    if vErrorsSum > 0 then
      vResultsNode.AddAttribNV(cnstXMLErrors).ValueAsInt := vErrorsSum;
  end
  else
    DoXMLParserError(cnstXMLMessageNotHasNode + ': ' + cnstXMLResults, nil);
end;

function TsgXMLIDE.GetOnXMLMessage: TsgXMLMessage;
begin
  Result := FOnXMLMessage;
end;

function TsgXMLIDE.GetKey(const AStr: string):  TsgXMLKey;
var
  I: TsgXMLKey;
  vIndex: Integer;
begin
  Result := xkUndefined;
  if not Assigned(KeyWords) then
  begin
    KeyWords := TsgStringList.Create;
    KeyWords.Sorted := True;
    KeyWords.CaseSensitive := False;
    KeyWords.Duplicates := dupIgnore;
    for I := Low(TsgXMLKey) to High(TsgXMLKey) do
      KeyWords.AddObject(cntKeyWords[I].Name, TObject(I));
  end;
  vIndex := KeyWords.IndexOf(AStr);
  if vIndex > -1 then
    Result := TsgXMLKey(KeyWords.Objects[vIndex]);
end;

function TsgXMLIDE.GetEntitiesByNodeWithFilter(const ANode: TsgNodeSample;
  const AErrorsNode: TsgNode; AHandles: TsgInt64List; AEntityList: TList): Boolean;
var
  vHasFilter: Boolean;
  vAttribsNoFilter: TList;
  vEntity: TsgDXFEntity;
  vLayout: TsgDXFLayout;
begin
  if IsInvalidRefs then
  begin
    Result := False;
    Exit;
  end;
  Result := False;
  vAttribsNoFilter := TList.Create;
  try
    vEntity := GetEntityByNode(ANode, AErrorsNode, AHandles, AEntityList);
    if (AEntityList.Count = 0) and Assigned(vEntity) then
      AEntityList.Add(vEntity);
    vHasFilter := ExtractAttribsNoUseInFilter(TsgNode(ANode), vAttribsNoFilter);
    try
      if vHasFilter then
      begin
        if AEntityList.Count = 0 then
        begin
          vLayout := CADImage.CurrentLayout;
          AEntityList.Add(vLayout);
        end;
        ApplyFilter(TsgNode(ANode), AEntityList);
        Result := True;
      end;
    finally
      AddAttribsNoUseInFilter(TsgNode(ANode), vAttribsNoFilter);
    end;
  finally
    vAttribsNoFilter.Free;
  end;
end;

procedure TsgXMLIDE.GetParentEntity(const APath,ADelimeter: string;
  const AProc: TsgParentGetter; var AParent: TsgDXFEntity);
var
  vStrValues: TsgStringList;
  I: Integer;
begin
  vStrValues := TsgStringList.Create;
  try
    vStrValues.LineBreak := ADelimeter;
    vStrValues.Text := APath;
    DeleteEmptyStrings(vStrValues);
    for I := 0 to vStrValues.Count - 1 do
    begin
      AParent := AProc(AParent, vStrValues[I]);
      if not Assigned(AParent) then
        Break;
    end;
  finally
    vStrValues.Free;
  end;
end;

procedure TsgXMLIDE.GetXrefLayout(const APath,ADelimeter: string; var AParent: TsgDXFEntity);
var
  vXref: TsgDXFXref;
  vImage: TGraphic;
begin
  GetParentEntity(APath, ADelimeter, GetEntityByHandle, AParent);
  if AParent is TsgDXFBlock then
  begin
    vXref := TsgDXFBlock(AParent).Xref;
    if Assigned(vXref) then
    begin
      vImage := vXref.CADImage;
      if Assigned(vImage) and (vImage is TsgCADImage) then
        AParent := TsgCADImage(vImage).CurrentLayout;
    end;
  end;
end;

function TsgXMLIDE.GetEntityOnName(const AParent: TsgDXFEntity;
  const AName: string): TsgDXFEntity;
var
  vConvSec: TConvSection;
  vSection: TsgDXFEntity;
begin
  Result := nil;
  if Assigned(AParent) then
    Result := AParent.FindEntByName(AName)
  else
  begin
    vSection := TsgDXFConverterAccess(CADImage.Converter).GetHeaderSection;
    if Assigned(vSection) and SameText(AName, vSection.Name) then
      Result := vSection;
    if not Assigned(Result) then
    begin
      for vConvSec := Low(TConvSection) to High(TConvSection) do
      begin
        vSection := FCADImage.Converter.Sections[vConvSec];
        if Assigned(vSection) and SameText(AName, vSection.Name) then
        begin
          Result := vSection;
          Break;
        end;
      end;
    end;
  end;
end;

function TsgXMLIDE.GetEntityByHandle(const AParent: TsgDXFEntity;
  const AHandle: string): TsgDXFEntity;
var
  vXref: TsgDXFXref;
  vImage: TGraphic;
  vHandle: UInt64;
begin
  Result := nil;
  vHandle := StrToInt64(AHandle);
  if Assigned(AParent) then
  begin
    if AParent is TsgDXFBlock then
    begin
      vXref := TsgDXFBlock(AParent).Xref;
      if Assigned(vXref) then
      begin
        vImage := vXref.CADImage;
        if Assigned(vImage) and (vImage is TsgCADImage) then
          Result := TsgCADImage(vImage).Converter.Sections[csBlocks].FindEntByHandle(vHandle);
      end;
    end;
  end
  else
    Result := FCADImage.Converter.Sections[csBlocks].FindEntByHandle(vHandle);
end;

function TsgXMLIDE.GetEntityByNode(const ANode: TsgNodeSample;
  const AErrorsNode: TsgNode; AHandles: TsgInt64List = nil;
  AEntityList: TList = nil): TsgDXFEntity;
var
  I: Integer;
  vParent: TsgDXFEntity;
  vStrValues: TsgStringList;
  vNodeHandle, vNodePathName, vNodeXrefPath, vNodePathKey: TsgNodeSample;
  vHandle: Int64;
  vConv: TsgDXFConverter;

  vEntitiesIternal: TList;

  function IsInternalObject(const AEntityAddress: string;
    const AEntities: TList): TObject;
  var
    vEntityAddress: String;
    vPathKey: string;
  begin
    Result := nil;
    vEntityAddress := AEntityAddress;
    if Length(vEntityAddress) > 0 then
    begin
      if vEntityAddress[1] = '#' then
      begin
        vEntityAddress[1] := '$';
        Result := TObject(StrToInt64(vEntityAddress));
        if not Assigned(Result) then
          Exit;
        AEntities.Add(Result);
      end
      else if vEntityAddress[1] = cnst3DHadnle then
      begin
        vEntityAddress[1] := '$';
        vPathKey := '';
        if Assigned(vNodePathKey) then
        begin
          vPathKey := vNodePathKey.ValueAsStr;
        end;
        Result := TsgDXFConverterAccess(FCADImage.Converter).ForwardToModeler(vEntityAddress, vPathKey);
        if not Assigned(Result) then
          Exit;
        AEntities.Add(Result);
      end;
    end;
  end;

begin
  Result := nil;
  vParent := nil;
  if IsInvalidRefs then
    Exit;
  if Assigned(AHandles) then
    AHandles.Count := 0;
  if Assigned(AEntityList) then
    AEntityList.Count := 0;
  vNodePathKey := ANode.GetAttributeByName(cnstXMLCmdPathKey);
  vNodePathName := ANode.GetAttributeByName(cnstXMLCmdPathName);
  // Priority PathName Handle
  if Assigned(vNodePathName) then
  begin
    vNodeXrefPath := ANode.GetAttributeByName(cnstXMLCmdXREFPath);
    if Assigned(vNodeXrefPath) then
    begin
      GetXrefLayout(vNodeXrefPath.ValueAsStr, cnstDecimalSeparatorNames, vParent);
      GetParentEntity(vNodePathName.ValueAsStr, cnstDecimalSeparatorNames, GetEntityOnName, vParent);
    end
    else
      GetParentEntity(vNodePathName.ValueAsStr, cnstDecimalSeparatorNames, GetEntityOnName, vParent);
    if Assigned(vParent) then
    begin
      Result := vParent;
      if Assigned(AHandles) then
        AHandles.Add(Result.Handle);
      if Assigned(AEntityList) then
        AEntityList.Add(Result);
    end
    else
     DoMessageError(AErrorsNode,
       Format(sXMLInterfaceObjectNotFindHandle, [vNodePathName.ValueAsStr]),
       vNodePathName);
  end
  else
  begin
    vNodeHandle := ANode.GetAttributeByName(cnstXMLCmdHandle);
    vNodeXrefPath := ANode.GetAttributeByName(cnstXMLCmdXREFPath);
    vConv := FCADImage.Converter;
    if Assigned(vNodeXrefPath) then
    begin
      GetXrefLayout(vNodeXrefPath.ValueAsStr, cnstDecimalSeparatorNames, vParent);
      if Assigned(vParent) then
        vConv := vParent.Converter;
    end;
    if Assigned(vNodeHandle) then
    begin
      vEntitiesIternal := TList.Create;
      try
        if Assigned(AHandles) then
        begin
          vStrValues := TsgStringList.Create;
          try
            vStrValues.LineBreak := cnstDecimalSeparatorNames;
            vStrValues.Text := vNodeHandle.ValueAsStr;
            DeleteEmptyStrings(vStrValues);
            for I := 0 to vStrValues.Count - 1 do
            begin
              if Assigned(IsInternalObject(vStrValues[I], vEntitiesIternal)) then
                Continue;
              vHandle := TsgDXFConverterAccess(vConv).RestoreHandle(vStrValues[I]);
              if vHandle <> 0 then
                AHandles.Add(vHandle)
              else
                DoMessageError(AErrorsNode,
                  Format(sXMLInterfaceObjectNotFindHandle,[vStrValues[I]]),
                  vNodeHandle);
            end;
          finally
            vStrValues.Free;
          end;
        end;
        if Assigned(AHandles) then
        begin
          if AHandles.Count > 0 then
          begin
            Result := vConv.FindObjByHandle(AHandles[0]);
            if Assigned(AEntityList) then
              vConv.FindObjByHandles(AHandles, AEntityList);
            if AEntityList.Count <> AHandles.Count then
              DoMessageError(AErrorsNode, sXMLInterfaceObjectsNotFind, vNodeHandle);
          end;

          if vEntitiesIternal.Count > 0 then
            for I := 0 to vEntitiesIternal.Count - 1 do
              AEntityList.Add(vEntitiesIternal[I]);
        end
        else
        begin
          Result := vConv.FindObjByHandle(
            TsgDXFConverterAccess(vConv).
            RestoreHandle(vNodeHandle.Value));
          if not Assigned(Result) then
           DoMessageError(AErrorsNode,
             Format(sXMLInterfaceObjectNotFindHandle, [vNodeHandle.Value]),
             vNodeHandle);
        end;
      finally
        vEntitiesIternal.Free;
      end;
    end;
  end;
end;

function TsgXMLIDE.GetEntityByPathKey(const AEntity: TsgDXFEntity): TsgDXFEntity;
var
  PathKey: string;
begin
  try
    Result := AEntity;
    if AEntity is TsgObjEntity3D then
    begin
      Result := TsgDXFEntity(TsgObjEntity3D(AEntity).OriginalEntity);
      if not (TObject(Result) is TsgDXFEntity) then
      begin
        PathKey := TsgObjEntity3D(AEntity).PathKey;
        if (Length(PathKey) > 0) and (PathKey[1]= '-') then
          Result := TsgSupportPathKey.GetDXFEntityByPathStrKey(PathKey);
      end;
    end;
  except
    Result := nil;
  end;
end;

function TsgXMLIDE.GetEntityFromSelectHandles(const AIndex: Integer): TsgDXFEntity;
begin
  Result := TsgDXFEntity(FSelectedHandles.GetObject(AIndex));
  if not Assigned(Result) then
    Result := FCADImage.Converter.FindObjByHandle(FSelectedHandles.GetHandle(AIndex));
end;

function TsgXMLIDE.GetXMLOutString: string;
begin
  CheckResults;
  Result := XMLOut.XMLString;
  ClearParser;
end;

procedure TsgXMLIDE.GetHints(const ANodeName: string; var Hints: TStringList);
var
  vNode: TsgNode;
  vXMLKey: TsgXMLKey;
  vInd: TsgXMLKey;
  I: Integer;
  J: TsgXMLId;
  vListObjectClass: TsgListObjectClass;
  vEntity: TsgListObject;
  S: string;
begin
  if FInstructions = nil then
  begin
    FInstructions := TStringList.Create;
    for vXMLKey := Low(cntKeyWords) to High(cntKeyWords) do
      FInstructions.AddObject(cntKeyWords[vXMLKey].Name, TObject(vXMLKey));
    FInstructions.Sort;
  end;
  if FInstructions.IndexOf(ANodeName) <> -1 then
  begin
    vInd := TsgXMLKey(FInstructions.Objects[FInstructions.IndexOf(ANodeName)]);
    vNode := TsgNode.Create;
    try
      CommandToXMLNode(vNode, nil, vInd);
      if vNode.Attributes <> nil then
        for I := 0 to vNode.Attributes.Count - 1 do
        begin
          if LowerCase(vNode.AttributeNodes[I].Name) = 'parameters' then
          begin
            for J := Low(cnstXMLNames) to High(cnstXMLNames) do
            begin
              S := cnstDataTypes[cnstXMLNames[J].ValueType].Name;
              if cnstXMLNames[J].Name <> '' then
                Hints.Add(cnstXMLNames[J].Name + '=' + S);
            end;
            Continue;
          end;
          Hints.Add(vNode.AttributeNodes[I].Name + '=' + vNode.AttributeNodes
            [I].Value);
        end;
    finally
      vNode.Free;
    end;
  end
  else
  begin
    S := ANodeName;
    if (Copy(S, 1, 3) = cnstXMLPrefix) then
      Delete(S, 1, 3);
    I := GlobalsgTypes.IndexOf(S);
    if I <> -1 then
    begin
      vListObjectClass := TsgListObjectClass(GlobalsgTypes.MetaClass[I]);
      if (vListObjectClass = TsgDXFConverter) or
        (vListObjectClass = TsgDXFSectionEntities) then
        Exit;
      vEntity := vListObjectClass.Create;
      vNode := TsgNode.Create;
      try
        vEntity.ToXML(vNode, [xmCallHelp]);
        if vNode.ChildNodesCount > 0 then
        begin
          vNode := TsgNode(vNode.ChildNodes[0]);
          if vNode.Attributes <> nil then
          begin
            for I := 0 to vNode.Attributes.Count - 1 do
            begin
              S := cnstDataTypes[cnstXMLNames[GetXMLType(vNode.AttributeNodes[I].Name).Id].ValueType].Name;
              // if S = '' then
              // S := InsertSpace(S);
              Hints.Add(vNode.AttributeNodes[I].Name + '=' + S);
            end;
            Hints.Add(cnstXMLNames[xmlHandleSave].Name + '=' + cnstDataTypes[cnstXMLNames[xmlHandleSave].ValueType].Name);
          end;
        end;
      finally
        vEntity.Free;
        vNode.Free;
      end;
    end;
  end;
end;

function TsgXMLIDE.GetImageGuid: TGUID;
begin
  Result := cntsErrorGuid;
  if IsInvalidRefs then
    Exit;
  if CADEDitor <> nil then
    Result := CADEDitor.GetCurrentDrawingGuid
  else
    Result := CADImage.Guid;
end;

function TsgXMLIDE.GetIResultNode: IsgResultNode;
begin
  Result := Self;
end;

function TsgXMLIDE.ProcessXML(const AInput: string): string;
var
  vMainNode: TsgNodeSample;
  J: Integer;
  vHandle: THandle;
  vImg: TsgCADImage absolute vHandle;
  vNode: TsgNode;
  vCmdsCount: Integer;
  vMessageXMLError: string;
  vPosition: TPoint;
begin
  FResult := '';
  if AInput = '' then
  begin
    Result := SaveConverterToXML;
    Exit;
  end;
  ClearParser;
  //
  if not IsValidXML(AInput, vMessageXMLError, @vPosition) then
  begin
    DoXMLParserError(vMessageXMLError, @vPosition);
    Result := GetXMLOutString;
    Exit;
  end;
  //vOut := RootOut.AddChildNV(cnstXMLOut);
  try
  FXMLInp.DebugInfo := True;
  FXMLInp.LoadFromString(AInput);
{$IFDEF SGDEL_2009}
  GetEncodingCodePageName(FXMLInp.SaveEncoding);
{$ENDIF}
  vMainNode := FXMLInp.ROOT.NodeByName[cnstXMLRootNode];
  if not CheckXMLInterfaceVersion(vMainNode) then
  begin
    if Assigned(vMainNode) then
      CreateResultNode(vMainNode.Name)
    else
      CreateResultNode(cnstUnknown);
    DoMessageError(GetIResultNode.Errors, sXMLInterfaceVersionError, vMainNode);
    Result := GetXMLOutString;
    Exit;
  end;
{$IFDEF SGAPPPROTECT}
  vCmdsCount := 0;
  {$IFDEF CS_USEFORM}
  FNotAppCmd := GetAttributeInt64(vMainNode, cnstXmlAppId64, 0) <> Int64(Application);
  {$ELSE}
  FNotAppCmd := False;
  {$ENDIF}
{$ENDIF}
  for J := 0 to vMainNode.ChildNodesCount - 1 do
  begin
    vNode := TsgNode(vMainNode.ChildNodes[J]);
    ProcessXMLNode(vNode, vCmdsCount);
  end;
{$IFDEF SGAPPPROTECT}
  if vCmdsCount > 0 then
    DoTrialMessage;
{$ENDIF};
  finally
    Result := GetXMLOutString;
  end;
end;

procedure TsgXMLIDE.SetCADEditorOnly(const AValue: TsgProxyEditor);
begin
  FCADEditor := AValue;
end;

function TsgXMLIDE.SaveConverterToXML: string;
begin
  if IsInvalidRefs then
    Result := ''
  else
    Result := SaveEntityToXML(FCADImage.Converter);
end;

function TsgXMLIDE.SaveEntityToXML(AEntity: TsgListObject): string;
var
  vXMLParser: TsgParser;
  vRoot: TsgNode;
  vDocument: TsgNodeDocument;
{$IFDEF SGDEL_2009}
  vCodePageName: string;
{$ENDIF}
begin
  Result := '';
  if not IsInvalidRefs then
  begin
    vXMLParser := TsgParser.Create;
    try
      vDocument := TsgNodeDocument.Create;
      vDocument.Name := cnstXML;
      vXMLParser.ROOT.Add(vDocument);
      vDocument.AddAttribNV(cnstVersion).ValueAsStr := '1.0';
{$IFDEF SGDEL_2009}
      vCodePageName := GetEncodingCodePageName(vXMLParser.SaveEncoding);
      if Length(vCodePageName) > 0 then
        vDocument.AddAttribNV(cnstEncoding).ValueAsStr := vCodePageName;
{$ENDIF}
      vDocument.AddAttribNV(cnstStandalone).ValueAsStr := 'no';
      vRoot := TsgNode.Create;
      vRoot.Name := cnstXMLRootNode;
      vXMLParser.ROOT.Add(vRoot);
      vRoot.AddAttribNV(cnstVersion).ValueData.ValueAsVersion := cnstXMLIntfaceVersion;
      AEntity.ToXML(vRoot);
      Result := vXMLParser.XMLString;
    finally
      vXMLParser.Free;
    end;
  end;
end;

procedure TsgXMLIDE.ProcessXMLNode(ANode: TsgNode; var ANodesCount: Integer);
var
  vKey: TsgXMLKey;
  vError: TsgNode;
begin
  if ANode.NodeType <> ntElement then
    Exit;
  vKey := GetKey(ANode.Name);
  CreateResultNode(ANode.Name);
  if Assigned(MainApplication) then
  begin
    if vKey = xkCommand then
      MainApplication.MainUpdate(mumFullUpdate)
    else
      MainApplication.MainUpdate(mumXmlInterfaceUpdate);
  end;

  if (not (vKey in cnstKeyNotUsedCADEditor)) and (not Assigned(CADEditor) or
    CADEditor.IsOnlyCommand) then
  begin
    if Assigned(MainApplication) and IsEqualGUID(MainApplication.CreateDrawing, cnstFailNewDrawing) then
    begin
      vError :=  GetIResultNode.Errors.AddChildNV(cnstXMLError);
      vError.AddAttribNV(cnstXMLMessage,
        Format(cnstXMLMessageUnsupportedCommand,[ANode.Name]));
      Exit;
    end;
  end;
  if vKey = xkRegistration then
    CommandRegistration(ANode, GetIResultNode)
  else
    if IsXmlActive(ANode, vKey, ANodesCount) then
    begin
      case vKey of
        xkGet: CommandGet(ANode, GetIResultNode);
        xkGetCADPoint: CommandGetCADPoint(ANode, GetIResultNode);
        xkAdd: CommandAdd(ANode, GetIResultNode);
        xkGetExternalFiles: CommandGetExternalFiles(ANode, GetIResultNode);
        xkGetBox: CommandGetBox(ANode, GetIResultNode);
        xkFindTextEntities: CommandFindTextEntities(ANode, GetIResultNode);
        xkFitToSize: CommandFitToSize(ANode, GetIResultNode);
        xkIterator: CommandIterator(ANode, GetIResultNode);
        xkSelect: CommandSelect(ANode, GetIResultNode);
        xkUnSelect: CommandUnSelect(ANode, GetIResultNode);
        xkApply: CommandApply(ANode, GetIResultNode);
        xkDelete: CommandDelete(ANode, GetIResultNode);
        xkRemove: CommandRemove(ANode, GetIResultNode);
        xkLoad: CommandLoad(ANode, GetIResultNode);
        xkUnload: CommandUnLoad(ANode, GetIResultNode);
        xkSwitchDrawing: CommandSwitchDrawing(ANode, GetIResultNode);
        xkGetDrawingsList: CommandGetDrawingsList(ANode, GetIResultNode);
        xkGetImage: CommandGetImage(ANode, GetIResultNode);
        xkContextMenu: CommandContextMenu(ANode, GetIResultNode);
        xkRibbon: CommandRibbon(ANode, GetIResultNode);
        xkCommand: CommandCommand(ANode, GetIResultNode);
        xkGetSelected: CommandGetSelected(ANode, GetIResultNode);
        xkSignToEvent: CommandSignToEvent(ANode, GetIResultNode);
        xkShowSelectedEntities:  CommandShowSelectEntities(ANode, GetIResultNode);
        xkSave: CommandSaveToFile(ANode, GetIResultNode);
        xkCreateHatch: CommandCreateHatch(ANode, GetIResultNode);
        xkGetDrawingChanged: CommandGetDrawingChanged(ANode, GetIResultNode);
        xkSetDrawingChanged: CommandSetDrawingChanged(ANode, GetIResultNode);
        xkGetView: CommandGetView(ANode, GetIResultNode);
        xkSetView: CommandSetView(ANode, GetIResultNode);
        xkCustomSelectMode: CommandCustomSelectMode(ANode, GetIResultNode);
        xkEditor: CommandEditorAttribs(ANode, GetIResultNode);
        xkSetSpecialCustomInsert: CommandSetSpecialCustomInsert(ANode, GetIResultNode);
        xkSupportedCommandsList: CommandSupportedCommandsList(ANode, GetIResultNode);
        xkSupportedClassesList: CommandSupportedClassesList(ANode, GetIResultNode);
        xkInvalidate: CommandInvalidate(ANode, GetIResultNode);
        xkHelp: CommandHelp(ANode);
        xkGetViewRect: CommandGetViewRect(ANode, GetIResultNode);
        xkSetViewRect: CommandSetViewRect(ANode, GetIResultNode);
        xkMenuButton: CommandMenuButton(ANode, GetIResultNode);
        xkCaptureScreen: CommandCaptureScreen(ANode, GetIResultNode);
        xkReport: CommandReport(ANode, GetIResultNode);
        xkWaterMark:  CommandWaterMark(ANode, GetIResultNode);
        xkSpecialLayers:  CommandSpecialLayers(ANode, GetIResultNode);
        xkGlobalVariable:  CommandGlobalVariable(ANode, GetIResultNode);
        xkGetSaveHandles: CommandSaveHandles(ANode, GetIResultNode);
        xkAppCommand:  CommandAppCommand(ANode, GetIResultNode);
        xkAppMainCommand: CommandMainAppFunction(ANode, GetIResultNode);
{$IFDEF SGABVIEWER}
        xkGetData:  CommandGetData(ANode, GetIResultNode);
        xkSetData:  CommandSetData(ANode, GetIResultNode);
{$ENDIF}
{$IFDEF SG_BTI_INTERFACE}
        xkCreateBtiEntity: CommandCreateBtiEntity(ANode, GetIResultNode);
{$ENDIF}
        else
        begin
          if not SameText(ANode.Name, cnstXMLCmdComment) then
            DoMessageError(GetIResultNode.Errors,
             Format(cnstXMLMessageUnsupportedCommand,[ANode.Name]), ANode);
        end;
      end;
  end;
end;

procedure TsgXMLIDE.SaveToHTMLHelp(const AFileName: string);
const
  cnstXMLHelpAddAttribString = ' parameters are: <br/>';
  cnstXMLHelpAddChildString = ' field accepts the following child: <br/>';
var
  vStrList, vExample, vSortedInstructions, vSortedInstructionParameters: TStringList;
  I: TsgXMLKey;
  vInd: TsgDataType;
  vComParamType: TXMLCommParameter;
  vEvent: TsgXMLEvent;
  J, K, vIndSyntax, ComParamInd, vDescriptionInd, vExampleNumber: Integer;
  vNode, vChild, vResult : TsgNode;
  vEntity: TsgListObject;
  vListObjectClass: TsgListObjectClass;
  vXMLInp: TsgParser;
  S, vSyntax, vCmdPref, vPathToExamples, vClassName: string;
  vHasEventsAndCommands: Boolean;
  vCommands, vEvents: TStringList;
  //vHasResult: Boolean;

  function GetCommParameterToStr(AXMLID: TXMLCommParameter): string;
  begin
    Result := IntToStr(Integer(AXMLID));
  end;

  procedure AddHeader;
  begin
    vStrList.Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">');
    vStrList.Add('<html> <head> <title> CADSoftTools XML Reference </title>');
    vStrList.Add(' <style type="text/css"> TABLE { width: 700px; border-collapse: collapse; } ');
    vStrList.Add(' TD, TH {   padding: 3px;  border: 1px solid black; /* Ïàðàìåòðû ðàìêè */   }   TH {    background: #b0e0e6; /* Öâåò ôîíà */   }  </style>');
    vStrList.Add('<script type="text/javascript" src="shFilter.js"></script>');
    vStrList.Add('<script type="text/javascript" src="shCore.js"></script>');
    vStrList.Add('<script type="text/javascript" src="shBrushXml.js"></script>');
    vStrList.Add('<link type="text/css" rel="stylesheet" href="shCoreDefault.css"/>');
    vStrList.Add('<link type="text/css" rel="stylesheet" href="shTable.css">');
    vStrList.Add('<script type="text/javascript">SyntaxHighlighter.all();</script>');
    vStrList.Add('</head> <body>');
    vStrList.Add('<H1>CADSoftTools XML Reference  </H1> <br/>');
  end;

  procedure AddContents;
  var
    vIndx: Integer;
    S: string;
  begin
    vStrList.Add('Filter: ');
    vStrList.Add('<input name="filt" onkeyup="filter(this, ''tab1'')" style="width:150px;" type="text" placeholder="Type to search"  />');
    vStrList.Add('    <p/>');


    vStrList.Add('<table  id="tab1">');
      vStrList.Add('<tr>');

        vStrList.Add('<th>');
        vStrList.Add('<b><a href="#Instructions"> Instructions </a></b>');
      vStrList.Add('<input name="filtins" onkeyup="FilterColumns(this, ''tab1'', 0)" style="width:150px;" type="text" placeholder="Type to search"  />');
        vStrList.Add('</th>');

        vStrList.Add('<th>');
        vStrList.Add('<b><a href="#InstructionParameters"><NOBR> Instruction Parameters </NOBR> </a> </b>');
      vStrList.Add('<input name="filtinsparam" onkeyup="FilterColumns(this, ''tab1'', 1)" style="width:150px;" type="text" placeholder="Type to search"  />');
        vStrList.Add('</th>');


        vStrList.Add('<th>');
        vStrList.Add('<b><a href="#Classes">Classes</a></b>');
      vStrList.Add('<input name="filtclasses" onkeyup="FilterColumns(this, ''tab1'', 2)" style="width:150px;" type="text" placeholder="Type to search"  />');
        vStrList.Add('</th>');

        vStrList.Add('<th>');
        vStrList.Add('<b><a href="#ClassesParameters"> <NOBR> Classes Parameters</NOBR></a></b>');
      vStrList.Add('<input name="filtclassesparam" onkeyup="FilterColumns(this, ''tab1'', 3)" style="width:150px;" type="text" placeholder="Type to search"  />');
        vStrList.Add('</th>');

        vStrList.Add('<th>');
        vStrList.Add('<b><a href="#Types">Types </a> </b>');
      vStrList.Add('<input name="filtinsparam" onkeyup="Filtertypes(this, ''tab1'', 4)" style="width:150px;" type="text" placeholder="Type to search"  />');
        vStrList.Add('</th>');

      if vHasEventsAndCommands then
      begin
          vStrList.Add('<th>');
          vStrList.Add('<b><a href="#Events">Events</a></b>');
        vStrList.Add('<input name="filtevents" onkeyup="FilterColumns(this, ''tab1'', 5)" style="width:150px;" type="text" placeholder="Type to search"  />');
          vStrList.Add('</th>');

          vStrList.Add('<th>');
          vStrList.Add('<b><a href="#Commands"><NOBR> Command line examples</NOBR> </a></b>');
        vStrList.Add('<input name="filtcommands" onkeyup="FilterColumns(this, ''tab1'', 6)" style="width:150px;" type="text" placeholder="Type to search"  />');
          vStrList.Add('</th>');
      end;

      vStrList.Add('</tr>');


    for vIndx := 0 to XMLIds.Count - 1 do
    begin
      vStrList.Add('<tr>');

      vStrList.Add('<td>');
      if vIndx < vSortedInstructions.Count then
      begin
        S := cntKeyWords[TsgXMLKey(vSortedInstructions.Objects[vIndx])].Name;
        vStrList.Add('<a href="#' + S + '">' + S + '</a>');
      end;
      vStrList.Add('</td>');

      vStrList.Add('<td>');
      if vIndx  < vSortedInstructionParameters.Count then
      begin
        S := cnsTXMLCommParameters[TXMLCommParameter(vSortedInstructionParameters.Objects[vIndx])].Name;
        vStrList.Add('<a href="#' + vCmdPref + S +
          GetCommParameterToStr(TXMLCommParameter(vSortedInstructionParameters.Objects[vIndx])) + '">' + S + '</a>');
      end;
      vStrList.Add('</td>');

      vStrList.Add('<td>');
      if vIndx < GlobalsgTypes.Count then
      begin
        S := cnstXMLPrefix + TsgListObjectClass(GlobalsgTypes.MetaClass[vIndx]).GetPropertyName;
        vStrList.Add('<a href="#' + S + '">' + S + '</a>');
      end;
      vStrList.Add('</td>');

      vStrList.Add('<td>');
      if vIndx <= XMLIds.Count then
      begin
        S := cnstXMLNames[TsgXMLID(TsgObjectInt64(XMLIds.Objects[vIndx]).FieldInt)].Name;
        vStrList.Add('<a href="#' + S + '">' + S + '</a>');
      end;
      vStrList.Add('</td>');

      vStrList.Add('<td>');
      if vIndx <= Ord(High(cnstDataTypes)) then
      begin
        S := cnstDataTypes[TsgDataType(vIndx)].Name;
        vStrList.Add('<a href="#' + S + '">' + S + '</a>');
      end;
      vStrList.Add('</td>');

      if vHasEventsAndCommands then
      begin
        vStrList.Add('<td>');
        if vIndx <= Ord(High(TsgXMLEvent)) then
        begin
          S := cnstEvents[TsgXMLEvent(vIndx)].Name;
          vStrList.Add('<a href="#' + S + '">' + S + '</a>');
        end;
        vStrList.Add('</td>');

        vStrList.Add('<td>');
        if vIndx < vCommands.Count then
        begin
          vExample.LoadFromFile(FPathToXMLExamples + 'Commands\' + vCommands[vIndx]);
          S := vCommands[vIndx];
          StringReplace(S,ExtractFileExt(vCommands[vIndx]),'');
          vStrList.Add('<a href="#' + S + '">' + S + '</a>');
        end;
        vStrList.Add('</td>');
      end;
      vStrList.Add('</tr>');
    end;
    vStrList.Add('</table >');

  end;

  procedure AddParameter(AXMLID: TsgXMLId; AddAncor: Boolean = False; AIsSyntaxOnly: Boolean = False);
  var
    S: string;
  begin
    if AXMLID = xmlUndefined then
      Exit;
    if not AIsSyntaxOnly then
    begin
      vStrList.Add('<tr>');
        vStrList.Add('<td>');
        if AddAncor then
        begin
          vStrList.Add('<a name="' + cnstXMLNames[AXMLID].Name + '"></a>');
          vStrList.Add(cnstXMLNames[AXMLID].Name);
        end
        else
        begin
          S := cnstXMLNames[AXMLID].Name;
          vStrList.Add('<a href="#' + S + '">' + S + '</a>');
        end;
        vStrList.Add('</td>');
        vStrList.Add('<td>');
        S := cnstDataTypes[cnstXMLNames[AXMLID].ValueType].Name;
        vStrList.Add('<a href="#' + S + '">' + S + '</a>');
        vStrList.Add('</td>');
        vStrList.Add('<td>');
        if cnstXMLNames[AXMLID].Description <> '' then
          vStrList.Add(cnstXMLNames[AXMLID].Description)
        else
          vStrList.Add(InsertSpace(cnstXMLNames[AXMLID].Name));
        vStrList.Add('</td>');
      vStrList.Add('</tr>');
      S := cnstDataTypes[cnstXMLNames[AXMLID].ValueType].Name;
    end;
    vSyntax := vSyntax  +  '<a href="#' + cnstXMLNames[AXMLID].Name + '">' + cnstXMLNames[AXMLID].Name + '</a> ' +
      '=' + '<a href="#' + S + '">' + '"' + S + '"' + '</a> ';
  end;

  procedure AddParameterInstruction(AXMLID: TXMLCommParameter; AddAncor: Boolean = False; AIsSyntaxOnly: Boolean = False);
  var
    S: string;
  begin
  if not AIsSyntaxOnly then
  begin
    vStrList.Add('<tr>');
      vStrList.Add('<td>');
      S := cnsTXMLCommParameters[AXMLID].Name;
      if AddAncor then
        vStrList.Add('<a name="' + vCmdPref + cnsTXMLCommParameters[AXMLID].Name
        + GetCommParameterToStr(AXMLID) + '"></a>' + S)
      else
        vStrList.Add('<a href="#' + vCmdPref + S + GetCommParameterToStr(AXMLID) + '">' + S + '</a>');
      vStrList.Add('</td>');
      vStrList.Add('<td>');
      S := cnstDataTypes[cnsTXMLCommParameters[AXMLID].ValueType].Name;
      vStrList.Add('<a href="#' + S + '">' + S + '</a>');
      vStrList.Add('</td>');
      vStrList.Add('<td>');
      vStrList.Add(cnsTXMLCommParameters[AXMLID].Description);
      vStrList.Add('</td>');
    vStrList.Add('</tr>');
  end;
    vSyntax := vSyntax  + ' <a href="#' + vCmdPref + cnsTXMLCommParameters[AXMLID].Name + '">' + cnsTXMLCommParameters[AXMLID].Name + '</a> ' +
      '="' + '<a href="#' + S + '">' + S + '</a>" ';
  end;

  procedure AddParameterByText(AName, ADescr: string);
  begin
    vStrList.Add('<tr>');
      vStrList.Add('<td>');
      vStrList.Add('<a href="#' + AName + '">' + AName + '</a>');
      vStrList.Add('</td>');
      vStrList.Add('<td>');
      vStrList.Add('');
      vStrList.Add('</td>');
      vStrList.Add('<td>');
      vStrList.Add(ADescr);
      vStrList.Add('</td>');
    vStrList.Add('</tr>');
  end;


  procedure AddEvent(AXMLID: TsgXMLEvent; AddAncor: Boolean = False);
  var
    S: string;
  begin
    vStrList.Add('<tr>');
      vStrList.Add('<td>');
      S := cnstEvents[AXMLID].Name;
      if AddAncor then
        vStrList.Add('<a name="' + cnstEvents[AXMLID].Name + '"></a>' + S)
      else
        vStrList.Add('<a href="#' + S + '">' + S + '</a>');
      vStrList.Add('</td>');
      vStrList.Add('<td>');
      S := cnstDataTypes[cnstEvents[AXMLID].ValueType].Name;
      vStrList.Add('<a href="#' + S + '">' + S + '</a>');
      vStrList.Add('</td>');
      vStrList.Add('<td>');
      vStrList.Add(cnstEvents[AXMLID].Description);
      vStrList.Add('</td>');
    vStrList.Add('</tr>');
    vSyntax := vSyntax  + '<a href="#' + cnstEvents[AXMLID].Name + '">' + cnstEvents[AXMLID].Name + '</a> ' +
      '="' + '<a href="#' + S + '">' + S + '</a>" ';
  end;


  procedure AddParametersTableBegin(AStrList: TStringList);
  begin
    AStrList.Add('<table >');
      AStrList.Add('<tr>');
        AStrList.Add('<th>');
        AStrList.Add('<b>Parameter</b>');
        AStrList.Add('</th>');
        AStrList.Add('<th>');
        AStrList.Add('<b>Type</b>');
        AStrList.Add('</th>');
        AStrList.Add('<th>');
        AStrList.Add('<b>Description</b>');
        AStrList.Add('</th>');
      AStrList.Add('</tr>');
  end;

  procedure AddParametersTableEnd(AStrList: TStringList);
  begin
    AStrList.Add('</table>');
  end;

  procedure AddType(AType: TsgDataType);
  begin
      vStrList.Add('<tr>');
        vStrList.Add('<td>');
       vStrList.Add('<a name="' + cnstDataTypes[AType].Name + '"></a>');
        vStrList.Add(cnstDataTypes[AType].Name);
        vStrList.Add('</td>');
        vStrList.Add('<td>');
        vStrList.Add(cnstDataTypes[AType].Description);
        vStrList.Add('</td>');
      vStrList.Add('</tr>');
  end;

  procedure AddTypesTable;
  begin
    vStrList.Add('<table >');
      vStrList.Add('<tr>');
        vStrList.Add('<th>');
        vStrList.Add('<b>Type</b>');
        vStrList.Add('</th>');
        vStrList.Add('<th>');
        vStrList.Add('<b>Description</b>');
        vStrList.Add('</th>');
      vStrList.Add('</tr>');
  end;

  procedure AddParameterInternal(const ANodeInt: TsgNodeSample; AIsSyntaxOnly: Boolean);
  begin
    ComParamInd := FXMLCommParameterList.IndexOf(ANodeInt.Name);
    if ComParamInd <> -1 then
      AddParameterInstruction(TXMLCommParameter(FXMLCommParameterList.Objects[ComParamInd]), False, AIsSyntaxOnly)
    else
      AddParameter(GetXMLId(ANodeInt.Name), False, AIsSyntaxOnly);
  end;

  procedure GenerateInstructionParameters(ANode: TsgNode; AIsSyntaxOnly: Boolean = False);
  var
    M: Integer;
  begin
    for M := 0 to ANode.AttributeNodesCount - 1 do
      AddParameterInternal(ANode.AttributeNodes[M], AIsSyntaxOnly);
    vSyntax := vSyntax + '/&gt; <br/>';
  end;

  procedure GenerateInstructionParametersByChild(ANode: TsgNode);
  var
    M: Integer;
  begin
    for M := 0 to ANode.ChildNodesCount - 1 do
      AddParameterInternal(ANode.ChildNodes[M], False);
  end;

  function FindDescription(AXML: string): string;
  var
    vFrom, vTo: PChar;
  begin
    Result := '';
    vFrom := AnsiStrPos(PChar(AXML), PChar('<!-- Description:'));
    vTo := AnsiStrPos(PChar(AXML), PChar('-->'));
    if (vFrom = nil) or (vTo = nil) then
      Exit;
    SetLength(Result,vTo - vFrom - 5);
    StrLCopy(@Result[1], vFrom + 5, Length(Result));
    ReplaceAnsi(Result, '<', '&lt;');
    ReplaceAnsi(Result, '>', '&gt;');
  end;

  function  GetFileList(const Path: string; List: TStrings): string;
  var
    Rec: TSearchRec;
  begin
    if FindFirst(Path + '*.xml', faAnyFile, Rec) = 0 then
    repeat
      if (Rec.Name = '.') or (Rec.Name = '..') then Continue;
      if (Rec.Attr and faDirectory) <> 0 then
      begin
        List.Add(Rec.Name);
        GetFileList(Path + Rec.Name + '\', List);
      end
      else
        List.Add(Rec.Name);
    until FindNext(Rec) <> 0;
    FindClose(Rec);
    if List.Count > 0 then
      Result := Path
    else
      Result := ''
  end;

  function GetBoltTextForHtml(AText: string): string;
  begin
    Result := '<b>' + AText + '</b>';
  end;

  procedure AddNodeToHtml(ANode: TsgNode; AStrList: TStringList;
    AXMLKey: TsgXMLKey; ASyntax: Boolean = False);
  var
    vNode: TsgNode;
    J: Integer;
  begin
    if ANode.AttributeNodesCount > 0 then
    begin
      if ASyntax then
      begin
        AStrList.Add(Format('<H4> %s </H4>', ['Syntax:']));
        vIndSyntax := AStrList.Count;
        vSyntax := '&lt;' + GetBoltTextForHtml(cntKeyWords[AXMLKey].Name) + ' ';
      end;
      AStrList.Add(GetBoltTextForHtml(ANode.Name) + cnstXMLHelpAddAttribString);
      AddParametersTableBegin(AStrList);
      GenerateInstructionParameters(ANode);
      AddParametersTableEnd(AStrList);
      AStrList.Add('<br/>');
      if ASyntax then
        AStrList.Insert(vIndSyntax, vSyntax);
    end;
    if ANode.ChildNodesCount > 0 then
    begin
      AStrList.Add(GetBoltTextForHtml(ANode.Name) + cnstXMLHelpAddChildString);
      AddParametersTableBegin(AStrList);
      GenerateInstructionParametersByChild(ANode);
      AddParametersTableEnd(AStrList);
      AStrList.Add('<br/>');
    end;
    for J := 0 to ANode.ChildNodesCount - 1 do
    begin
      vNode := TsgNode(ANode.ChildNodes[J]);
      AddNodeToHtml(vNode, AStrList, AXMLKey, False);
    end;
  end;

  procedure AddInputParameters(ANode: TsgNode; AXMLKey: TsgXMLKey;
    AStrList: TStringList);
  begin
    AStrList.Add('<br/>');
    AddNodeToHtml(ANode, AStrList, AXMLKey, True);
  end;

  procedure AddOutputParameters(ANode: TsgNode; AStrList: TStringList);
  begin
    //AStrList.Add(Format('<H4> %s </H4>', ['The returned value has the following structure:']));
    AStrList.Add('<br/>');
    AStrList.Add('The returned value has the following structure:');
    AStrList.Add('<br/>');
    AddNodeToHtml(ANode, AStrList, xkUndefined, False);
  end;

begin
  vCmdPref := 'cmd';
  vDescriptionInd := -1;
  vIndSyntax := -1;
  vChild := nil;
  vHasEventsAndCommands := {$IFDEF SG_CADIMAGE_DLL.DLl}False{$ELSE}True{$ENDIF};
  vPathToExamples  := FPathToXMLExamples;
  vExample := TStringList.Create;
  if vHasEventsAndCommands then
  begin
    vCommands := TStringList.Create;
    GetFileList(FPathToXMLExamples + 'Commands\', vCommands);
    vEvents := TStringList.Create;
    GetFileList(FPathToXMLExamples + 'Events\', vEvents);
  end
  else
  begin
    vCommands := nil;
    vEvents := nil;
  end;
  if XMLIds = nil then
    CreateXMLIdTable;
  vStrList := TStringList.Create;
  vSortedInstructions := TStringList.Create;
  for I := Low(cntKeyWords) to High(cntKeyWords) do
  begin
    if (AppKeyWords = []) or (I in AppKeyWords) then
      vSortedInstructions.AddObject(cntKeyWords[I].Name, Pointer(I));
  end;
  vSortedInstructions.Sort;


  vSortedInstructionParameters := TStringList.Create;
  for vComParamType := Low(TXMLCommParameter) to high(TXMLCommParameter)  do
    vSortedInstructionParameters.AddObject(cnsTXMLCommParameters[vComParamType].Name, Pointer(vComParamType));
  vSortedInstructionParameters.Sort;

  try
    AddHeader;
    AddContents;

    vStrList.Add('<a name="Instructions"></a>');
    vStrList.Add('<H2> Instructions </H2>');
//    for I := Low(cntKeyWords) to High(cntKeyWords) do
    for K := 0 to vSortedInstructions.Count - 1 do
    begin
      I := TsgXMLKey(vSortedInstructions.Objects[K]);
      vStrList.Add('<a name="' + cntKeyWords[I].Name + '"></a>');
      vStrList.Add(Format('<H3> %s </H3>', [cntKeyWords[I].Name]));
      if cntKeyWords[I].Description <> '' then
      begin
        vStrList.Add(cntKeyWords[I].Description);
        vDescriptionInd := vStrList.Count - 1;
      end;
      vNode := TsgNode.Create;
      vNode.Name := cntKeyWords[I].Name;
      vResult := TsgNode.Create;

      FXMLCommParameterList.Sorted := False;
      FXMLCommParameterList.Clear;
      CommandToXMLNode(vNode, vResult, I);
      FXMLCommParameterList.Sorted := True;

      AddInputParameters(vNode, I, vStrList);
      AddOutputParameters(vResult, vStrList);
      vStrList.Add('<br/>');
      S := vPathToExamples + 'Instructions\' + cntKeyWords[I].Name +'.xml';
      vExampleNumber := 1;
      while FileExists(S) do
      begin
        vExample.LoadFromFile(S);
        S := vExample.Text;
        ReplaceAnsi(S, '<', '&lt;');
        //StringReplace(S, '<', '&lt;'):
        if vExampleNumber = 1 then
          vStrList.Add('Example: <br/>')
        else
        begin
          vStrList.Add('Example ' + IntToStr(vExampleNumber) + ': <br/>');
          vStrList.Add(FindDescription(vExample.Text))
        end;
        vStrList.Add('<pre class="brush: xml;"> ');
        vStrList.Append(S);
        vStrList.Add('</pre>');
        S := FindDescription(vExample.Text);
        if (S <> '') and (vExampleNumber = 1) and (vDescriptionInd <> -1) then
          vStrList[vDescriptionInd] := S;
        Inc(vExampleNumber);
        S := vPathToExamples + 'Instructions\' + cntKeyWords[I].Name + IntToStr(vExampleNumber) + '.xml';
      end;
      vStrList.Add('<hr/>');
      FreeAndNil(vNode);
      FreeAndNil(vResult);
      vDescriptionInd := -1;
    end;

    vStrList.Add('<a name="InstructionParameters"></a>');
    vStrList.Add('<H2> Instruction Parameters </H2>');
    AddParametersTableBegin(vStrList);
    for K := 0 to vSortedInstructionParameters.Count - 1 do
      AddParameterInstruction(TXMLCommParameter(vSortedInstructionParameters.Objects[K]), True);
    AddParametersTableEnd(vStrList);

    vStrList.Add('<a name="Classes"></a>');
    vStrList.Add('<H2> Classes </H2>');
    for K := 0 to GlobalsgTypes.Count - 1 do
    begin
      vListObjectClass := TsgListObjectClass(GlobalsgTypes.MetaClass[K]);
      if (vListObjectClass = TsgDXFConverter) or (vListObjectClass = TsgDXFSectionEntities) then
        Continue
      else
        vEntity := TsgListObjectClass(GlobalsgTypes.MetaClass[K]).Create;
      if vEntity = nil then
        Continue;
      vNode := TsgNode.Create;
      vXMLInp := TsgParser.Create;
      try
        vEntity.ToXML(vNode,[xmAddSubEntities, xmCallHelp]);
        if vNode.ChildNodesCount > 0 then
        begin
          vChild := TsgNode(vNode.ChildNodes[0]);
          vStrList.Add('<a name="' + vChild.Name + '"></a>');
          vStrList.Add(Format('<H3> %s </H3>', [vChild.Name]));
          vStrList.Add(vEntity.XMLDescription);
          vStrList.Add(Format('<H4> %s </H4>', ['Syntax:']));
          vIndSyntax := vStrList.Count;
          vSyntax := '&lt;<b>' + vChild.Name + '</b> ';
        end;
        AddParametersTableBegin(vStrList);
        if vNode.ChildNodesCount > 0 then
        begin
          vChild := TsgNode(vNode.ChildNodes[0]);
          for J := 0 to vChild.AttributeNodesCount - 1 do
            AddParameter(GetXMLId(vChild.AttributeNodes[J].Name));
        end;
        vSyntax := vSyntax + '/&gt;';
        vStrList.Insert(vIndSyntax, vSyntax + ' <br/>');
        AddParametersTableEnd(vStrList);
        if Assigned(vChild) and (vChild.ChildNodesCount > 0) then
        begin
          vStrList.Add('<br/>');
          vStrList.Add(vChild.Name + ' accepts the following child atributes: <br/>');
          AddParametersTableBegin(vStrList);
          for J := 0 to vChild.ChildNodesCount - 1 do
            AddParameter(GetXMLId(vChild.ChildNodes[J].Name));
          AddParametersTableEnd(vStrList);
        end;
      vStrList.Add('<br/>');
      if Assigned(vChild) then
        vClassName := vChild.Name
      else
        vClassName := cnstXMLError;
      if (Copy(vClassName, 1, 3) = cnstXMLPrefix)  then
        Delete(vClassName, 1, 3);
      S := vPathToExamples + 'Classes\' + vClassName +'.xml';
      vExampleNumber := 1;
      while FileExists(S) do
      begin
        vExample.LoadFromFile(S);
        S := vExample.Text;
        ReplaceAnsi(S, '<', '&lt;');
        if vExampleNumber = 1 then
          vStrList.Add('Example: <br/>')
        else
        begin
          vStrList.Add('Example ' + IntToStr(vExampleNumber) + ': <br/>');
          vStrList.Add(FindDescription(vExample.Text));
        end;
        vStrList.Add('<pre class="brush: xml;"> ');
        vStrList.Append(S);
        vStrList.Add('</pre>');
        S := FindDescription(vExample.Text);
        if (S <> '') and (vExampleNumber = 1) and (vDescriptionInd <> -1) then
          vStrList[vDescriptionInd] := S;
        Inc(vExampleNumber);
        S := vPathToExamples + 'Classes\' + vClassName + IntToStr(vExampleNumber) + '.xml';
      end;
      vStrList.Add('<hr/>');
      finally
        FreeAndNil(vEntity);
        FreeAndNil(vXMLInp);
        vDescriptionInd := -1;
      end;
    end;

    vStrList.Add('<a name="ClassesParameters"></a>');
    vStrList.Add('<H2> Parameters </H2>');
    AddParametersTableBegin(vStrList);
    vSyntax := '';
    for J := 0 to XMLIds.Count - 1 do
      AddParameter(TsgXMLId(TsgObjectInt64(XMLIds.Objects[J]).FieldInt), True);
    AddParametersTableEnd(vStrList);

    vStrList.Add('<a name="Types"></a>');
    vStrList.Add('<H2> Types </H2>');
    AddTypesTable;
    for vInd := Low(cnstDataTypes) to High(cnstDataTypes) do
      AddType(vInd);
    vStrList.Add('</table>');
    if vHasEventsAndCommands then
    begin
      vStrList.Add('<a name="Events"></a>');
      vStrList.Add('<H2> Events </H2>');
      AddParametersTableBegin(vStrList);
      for vEvent := Low(TsgXMLEvent) to high(TsgXMLEvent)  do
        AddEvent(vEvent, True);
      AddParametersTableEnd(vStrList);
      vStrList.Add('<h3>SignToEvent examples</h3>');
      for J := 0 to vEvents.Count - 1 do
      begin
        vExample.LoadFromFile(FPathToXMLExamples + 'Events\' + vEvents[J]);
        S := vEvents[J];
        StringReplace(S,ExtractFileExt(vEvents[J]),'');
        vStrList.Add('<a name="' + S + '"></a>');
        vStrList.Add('<H3>' + S + '</H3>');
        S := FindDescription(vExample.Text);
        vStrList.Add(S);
        S := vExample.Text;
        ReplaceAnsi(S, '<', '&lt;');
        vStrList.Add('Example: <br/>');
        vStrList.Add('<pre class="brush: xml;"> ');
        vStrList.Append(S);
        vStrList.Add('</pre>');
      end;
      vStrList.Add('<a name="Commands"></a>');
      vStrList.Add('<H2> Command line examples </H2>');
      vStrList.Add('Command line supports hundreds of commands which are described in User Help. Here are some examples which shows the most important command line actions which enhance XML interface.');
      for J := 0 to vCommands.Count - 1 do
      begin
        vExample.LoadFromFile(FPathToXMLExamples + 'Commands\' + vCommands[J]);
        S := vCommands[J];
        StringReplace(S,ExtractFileExt(vCommands[J]),'');
        vStrList.Add('<a name="' + S + '"></a>');
        vStrList.Add('<H3>' + S + '</H3>');
        S := FindDescription(vExample.Text);
        vStrList.Add(S);
        S := vExample.Text;
        ReplaceAnsi(S, '<', '&lt;');
        vStrList.Add('Example: <br/>');
        vStrList.Add('<pre class="brush: xml;"> ');
        vStrList.Append(S);
        vStrList.Add('</pre>');
      end;
    end;
    vStrList.Add('</body> </html>');
  finally
    vStrList.SaveToFile(AFileName);
    vStrList.Free;
    vExample.Free;
    vCommands.Free;
    vEvents.Free;
    vSortedInstructions.Free;
  end;
end;

procedure TsgXMLIDE.CommandSaveToFile(ANode: TsgNodeSample;
  AResult: IsgResultNode);
const
  cnstXMLErrorFileName = 'The name of the output file is not specified.';
  cnstXMLErrorFileNameAndType = 'The file extension does not match the format.';
var
  I, vIndexFormat: Integer;
  vSaveTypeFormat, vFullPathFile, vExt, vFileName: string;
  vIsConvertImageToOle: Boolean;
  vSaveVersionFormat: TsgDWGVersion;
  vNode: TsgNode;
  vNodeSample: TsgNodeSample;
  vNodeAttr: TsgNodeAttrib;
begin
  vNodeSample := ANode.GetChildByName(cnstXMLExportParams);
  if Assigned(vNodeSample) then
  begin
    SaveToFileWithExportParams(vNodeSample, AResult);
    Exit;
  end;
  vNodeSample := ANode.GetAttributeByName(cnstXMLCmdFile);
  if not Assigned(vNodeSample) then
  begin
    DoMessageError(AResult.Errors, cnstXMLErrorFileName, ANode);
    Exit;
  end;
  vFullPathFile := vNodeSample.ValueAsStr;
  vExt := ExtractFileExt(vFullPathFile);
  vFileName := ExtractFileName(vFullPathFile);
  if Length(vFileName) = 0 then
  begin
    DoMessageError(AResult.Errors, cnstXMLErrorFileName, ANode);
    Exit;
  end;

{$IFDEF SG_USE_CADXML}
  if AnsiLowerCase(vExt) = '.xml' then
  begin
    SaveToXML(FCADImage, vFullPathFile);
    vNode := AResult.Output;
    vNodeAttr := vNode.AddAttribNV(cnstXMLCmdOut, '');
    vNodeAttr.Value := cnstXMLCmdSaveOK;
    Exit;
  end;
{$ENDIF}

  vNodeSample := ANode.GetAttributeByName(cnstXMLCmdType);
  if Assigned(vNodeSample) then
  begin
    vSaveTypeFormat := vNodeSample.ValueAsStr;
    if Length(vExt) = 0 then
      vFullPathFile := vFullPathFile + '.' + vSaveTypeFormat
    else
      if not SameText(Copy(vExt, 2, Length(vExt)), vSaveTypeFormat) then
      begin
        DoMessageError(AResult.Errors, cnstXMLErrorFileNameAndType, ANode);
        Exit;
      end;
  end
  else
  begin
    vSaveTypeFormat := cnstTypeSaveFormat[1].TypeFormat;
    if  Length(vExt) = 0 then
      vFullPathFile := vFullPathFile + '.' + vSaveTypeFormat
    else
    begin
      vSaveTypeFormat := Copy(vExt, 2, Length(vExt));
    end;
  end;
  vIndexFormat := -1;
  for I := Low(cnstTypeSaveFormat) to High(cnstTypeSaveFormat) do
  begin
    if SameText(vSaveTypeFormat, cnstTypeSaveFormat[I].TypeFormat) then
    begin
      vIndexFormat := I;
      Break;
    end;
  end;
  if vIndexFormat < 0 then
  begin
    DoMessageError(AResult.Errors, cnstXMLErrorFormatType, ANode);
    Exit;
  end;

  vNodeSample := ANode.GetAttributeByName(cnstXMLCmdVersion);
  if Assigned(vNodeSample) then
  begin
    for vSaveVersionFormat := Low(cnstVersionFile) to High(cnstVersionFile) do
      if SameText(vNodeSample.ValueAsStr, cnstVersionFile[vSaveVersionFormat]) then
        Break
  end
  else
  begin
    if cnstTypeSaveFormat[vIndexFormat].IsDWG then
      vSaveVersionFormat := acR2000
    else
      vSaveVersionFormat := acR2004;
  end;

  if not (vSaveVersionFormat in cnstSupportDWGVersion) then
  begin
    DoMessageError(AResult.Errors, cnstXMLErrorFormatType, ANode);
    Exit;
  end;

  if cnstTypeSaveFormat[vIndexFormat].IsDWG then
    vSaveVersionFormat := acR2000;

  vNodeSample := ANode.GetAttributeByName(cnstXMLCmdIsConvertImageToOle);
  if Assigned(vNodeSample) then
    vIsConvertImageToOle := vNodeSample.ValueAsBool
  else
    vIsConvertImageToOle := True;

  vNode := AResult.Output;
  vNodeAttr := vNode.AddAttribNV(cnstXMLCmdOut, '');

  if CADEditor.SaveImage(vFullPathFile, cnstTypeSaveFormat[vIndexFormat].IsDWG,
    vSaveVersionFormat, vIsConvertImageToOle) then
       vNodeAttr.Value := cnstXMLCmdSaveOK
     else
       vNodeAttr.Value := cnstXMLCmdSaveERROR;
end;

function TsgXMLIDE.IsInvalidRefs: Boolean;
begin
  Result := not Assigned(FCADImage) or not Assigned(FCADEditor);
end;

function TsgXMLIDE.IsXmlActive(const ANode: TsgNode;
  const AKey: TsgXMLKey; var ANodesCount: Integer): Boolean;
begin
  Result := True;
{$IFDEF SG_DELPHI_VCL}
{$IFDEF SGAPPPROTECT}
  if not (not _EOFTrial {$IFDEF SG_ABVIEWER}and not (SendAppMessage(RM_GetRegLevel, 0, 0) in [2,3]){$ENDIF}) then
  begin
    if FNotAppCmd then
    begin
       Result := False;
       Inc(ANodesCount, Ord(AKey <> xkUndefined))
    end;
  end;
{$ENDIF}
{$ENDIF}
end;

procedure TsgXMLIDE.ConvertStreamToOutputBase64(AStream: TStream;
  AOutput: TsgNode; AFormat: TsgExportFormat);
var
  vStreamBase64: TStringStream;
begin
  AOutput.AddAttribNV(cnstXMLFormat).Value := GetExportExt(AFormat);
  if Assigned(AStream) then
  begin
    vStreamBase64 := TStringStream.Create('');
    try
      AStream.Position := 0;
      EncodeBase64(AStream, TStream(vStreamBase64));
      AOutput.AddAttribNV(cnstXMLCmdBase64).Value := vStreamBase64.DataString;
    finally
      FreeAndNil(vStreamBase64);
    end;
  end;
end;

procedure TsgXMLIDE.SaveToFileWithExportParams(ANode: TsgNodeSample;
  AResult: IsgResultNode);
{$IFDEF SG_USE_EXPORT}
var
  vSaveResult: Integer;
  vNode: TsgNode;
  vFormat: Integer;
  vCommonExportParams: TsgCommonExportParams;
  vXmlString: string;
//  vExpVersion: string;
//  vOwner: TsgNodeSample;
begin
  if CheckForApp(ANode, AResult) then
  begin
    vCommonExportParams := TsgCommonExportParams.Create;
    try
      vCommonExportParams.FromNode(TsgNode(ANode));
      if not vCommonExportParams.IsSupportVersionOnFormat then
      begin
        vNode := TsgNode(ANode.GetChildByName(cnstXMLVersion));
        if not Assigned(vNode) then
          vNode := TsgNode(ANode);
        DoMessageError(AResult.Errors, cnstXMLErrorFormatType, vNode);
        Exit;
      end
      else
      begin
//        vExpVersion := GetAttributeStr(ANode, cnstXMLVersion, '');
//        if Length(vExpVersion) = 0 then
//        begin
//          vOwner := ANode;
//          repeat
//            vOwner := vOwner.Owner;
//          until Assigned(vOwner) and SameText(vOwner.Name, cnstXmlRootNode);
//          if Assigned(vOwner) then
//          begin
//            vExpVersion := GetAttributeStr(vOwner, cnstXMLVersion, '');
//            if Length(vExpVersion) > 0 then
//              TsgNode(ANode).AddAttribNV(cnstXMLCmdVersion).ValueAsStr := vExpVersion;//cnstXMLIntfaceVersion;
//          end;
//        end;
      end;
      vXmlString := SaveNodeToXMLString(ANode);
      vSaveResult := MainApplication.SaveFileWithXmlParams(vXmlString, vCommonExportParams.Destination, vFormat);
      try
        if vSaveResult <> cnstSaveResultOk then
        begin
          vNode := AResult.Result;
          if Assigned(vNode) then
            vNode.AddChildNV(cnstXMLResult).TextAsStr :=
               GetSaveResultStr(vSaveResult);
        end
        else
          if Assigned(RootOut) and (not vCommonExportParams.Destination.IsFile) and vCommonExportParams.Destination.IsStream then
            ConvertStreamToOutputBase64(vCommonExportParams.Destination.Stream, AResult.Output,
              TsgExportFormat(vFormat))
      finally
        MainApplication.SaveCompleate;
      end;
    finally
      vCommonExportParams.Free;
    end;
  end;
{$ELSE}
begin
  DoMessageError(AResult.Errors, sXMLInterfaceComFrmError, ANode);
{$ENDIF}
end;

procedure TsgXMLIDE.SendEvents(const AEventResult: TsgNode;
  const AImage: TObject; AImgGuid: PGUID = nil);
var
  vGuid: TGUID;
  vEvents: TsgParser;
  vRoot, vResults, vErrors: TsgNode;
begin
  if not Assigned(AEventResult) then
    Exit;
  if AImage is TsgCADImage then
  begin
    vGuid := TsgCADImage(AImage).Guid;
    AImgGuid := @vGuid;
  end;

  vEvents := TsgParser.Create;
  try
    InitRoot(vEvents, vRoot);
    vResults := vRoot.AddChildNV(cnstXMLResults);
    vResults.AddChild(AEventResult);
    if AImgGuid <> nil then
      AEventResult.AddAttribNV(cnstXMLCmdGuid).ValueAsStr := GuidToStr(AImgGuid^);
    vErrors := TsgNode(AEventResult.GetChildByName(cnstXMLErrors));
    if Assigned(vErrors) and (vErrors.ChildNodesCount > 0) then
      vResults.AddAttribNV(cnstXMLErrors).ValueAsInt := vErrors.ChildNodesCount;
    FOnXMLMessage(vEvents.SaveToString);
  finally
    vEvents.Free;
  end;
end;

procedure TsgXMLIDE.SetCADEditor(const Value: TsgProxyEditor);
begin
  FCADEditor := Value;
  if Assigned(FCADEditor) then
  begin
    FCADEditor.OnSelectEntities := OnSelectEntities;
    FCADEditor.OnSelectBTIEntities := OnSelectBTIEntities;
{$IFDEF CS_USEFORM}
    FCADEditor.OnMouseDown := OnMouseDown;
    FCADEditor.OnMouseMove := OnMouseMove;
    FCADEditor.OnMouseUp := OnMouseUp;
    FCADEditor.OnMouseWheel := OnMouseWheel;
{$ENDIF}
    FCADEditor.OnConfirmEntitiesDeletion := OnConfirmEntitiesDeletion;

    FCADEditor.OnGetInfo := OnGetInfo;
    FCADEditor.OnChangeEntities := OnChangeEntities;
    FCADEditor.OnCopyEntities := OnCopyEntities;
    FCADEditor.OnCreateEntities := OnCreateEntities;
    FCADEditor.OnDeleteEntities := OnDeleteEntities;
    FCADEditor.OnCancelEntities := OnCancelEntities;
    FCADEditor.OnLayoutAfterChange := OnLayoutAfterChange;
    FCADEditor.OnLayoutBeforeChange := OnLayoutBeforeChange;
    FCADEditor.OnPasteEntities := OnPasteEntities;
    FCADEditor.OnRedoEntities := OnRedoEntities;
    FCADEditor.OnUndoEntities := OnUndoEntities;
{$IFDEF CS_USEFORM}
    FCADEditor.OnKeyDown := OnKeyDown;
    FCADEditor.OnKeyUp := OnKeyUp;
    FCADEditor.OnKeyPress := OnKeyPress;
{$ENDIF}
    FCADEditor.OnPaint := OnPaint;
    FCADEditor.OnImageClose := OnImageClose;
    FCADEditor.OnImageNew := OnImageNew;
    FCADEditor.OnImageLoad := OnImageLoad;
    FCADEditor.OnImageSave := OnImageSave;
    DXFConv.SetDefaultAcisNumberProc(OnAcisNumber);
  end
  else
    DXFConv.SetDefaultAcisNumberProc(nil);
end;

procedure TsgXMLIDE.SetCADImage(const Value: TsgCADImage);
begin
  FSelectedHandles := nil;
  FCADImage := Value;
  if Assigned(FCADImage) then
    FSelectedHandles := TsgDXFConverterAccess(FCADImage.Converter).ConvExtended.SelectedHandles;
end;

procedure TsgXMLIDE.OnEventCustom(const AType: TsgXMLEvent;
  const Image, AEntities: TObject;
  const AttribNames, AttribValues: array of string;
  const ANeedGuid: Boolean;
  const ANode: TsgNode = nil);
var
  I: Integer;
  vNode, vOut: TsgNode;
  vImgGuid: TGUID;
  vGuid: PGUID;
begin
  vGuid := nil;
  if ANeedGuid then
  begin
    vGuid := @vImgGuid;
    vImgGuid := GetImageGuid;
  end;
  vNode := CreateResultEvent(cnstEvents[AType].Name);
  try
    try
      vOut := vNode.AddChildNV(cnstXMLOutput);
      if Assigned(ANode) then
      begin
        vOut.Assign(ANode);
        vOut.Name := cnstXMLOutput
      end;
      for I := Low(AttribNames) to High(AttribNames) do
        if Length(AttribValues[I]) > 0 then
          vOut.AddAttribNV(AttribNames[I], AttribValues[I]);
      if Image is TsgCADImage then
        vImgGuid := TsgCADImage(Image).Guid;
      if Assigned(AEntities) then
        AddEntitiesToNode(vOut, Image, AEntities, AType);
    except
      on E: Exception do
        AddExceptionToNode(vNode, E);
    end;
  finally
    SendEvents(vNode, nil, vGuid);
  end;
end;

procedure TsgXMLIDE.OnChangeEntities(const Image, AEntities: TObject);
begin
  if xeOnChangeEntities in FXMLEvents then
    OnEventCustom(xeOnChangeEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnConfirmEntitiesDeletion(const Image, AEntities: TObject;
  var AValue: Integer);
begin
  AValue := cnstConfirmUndefined;
  if not (xeOnConfirmEntitiesDeletion in FXMLEvents) then
    Exit;
  AValue := cnstConfirmNo;
  if xeOnConfirmEntitiesDeletion in FXMLEvents then
    OnEventCustom(xeOnConfirmEntitiesDeletion, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnApplicationExeption(Sender: TObject; E: Exception);
var
  vSender, ExClass, ExInfo: string;
begin
  try
{$IFDEF CS_USEFORM}
    if Assigned(FOnApplicationExeptionPrev) then
      FOnApplicationExeptionPrev(Sender, E);
{$ENDIF}
  finally
    if (xeApplicationExeption in FXMLEvents) then
    begin
      vSender := HandleToStr(UInt64(Sender));
      ExClass := E.ClassName;
      ExInfo :=  ExceptionToString(E);
      OnEventCustom(xeApplicationExeption, nil, nil, ['sender', 'class', 'message'],
        [vSender, ExClass, ExInfo], True);
    end;
  end;
end;

{$IFNDEF SG_FIREMONKEY}
procedure TsgXMLIDE.OnContextMenuClick(Sender: TObject);
{$IFDEF CS_USEFORM}
var
  vMenuItem: TMenuItem;
begin
  vMenuItem := TMenuItem(Sender);
  OnEventCustom(xeOnContextClick, nil, nil,
    [cnstXMLCmdCaption, cnstXMLCmdIndex],
    [vMenuItem.Caption, IntToStr(vMenuItem.Parent.IndexOf(vMenuItem))], True);
{$ELSE}
begin
{$ENDIF}
end;

{$IFDEF CS_USEFORM}
procedure AddContextMenuItem(const AMenuItem: TMenuItem; const ANode: TsgNode);
var
  I: Integer;
  vItem: TsgNode;
begin
  vItem := ANode.AddChildNV(cnstXmlItem);
  vItem.AddAttribNV(cnstXMLCmdCaption).ValueAsStr := AMenuItem.Caption;
  vItem.AddAttribNV(cnstXMLCmdVisible).ValueAsBool := AMenuItem.Visible;
  vItem.AddAttribNV(cnstXmlCmdEnable).ValueAsBool := AMenuItem.Enabled;
 // vItem.AddAttribNV(cnstXMLCmdIndex).ValueAsInt := AMenuItem.Parent.IndexOf(AMenuItem);
  for I := 0 to AMenuItem.Count - 1 do
    AddContextMenuItem(AMenuItem.Items[I], vItem);
end;
{$ENDIF}

procedure TsgXMLIDE.OnContextMenuPopUp(Sender: TObject);
{$IFDEF CS_USEFORM}
var
  I: Integer;
  vMenu: TPopupMenu;
  vNode: TsgNode;
begin
  vMenu := TPopupMenu(Sender);
  vNode := nil;
  try
    if Assigned(vMenu) and Assigned(vMenu.Items) and (vMenu.Items.Count > 0) then
    begin
      vNode := TsgNode.Create;
      for I := 0 to vMenu.Items.Count - 1 do
        AddContextMenuItem(vMenu.Items[I], vNode);
    end;
    OnEventCustom(xeOnContextPopUp, nil, nil,  [], [], True, vNode);
  finally
    vNode.Free;
  end;
{$ELSE}
begin
{$ENDIF}
end;
{$ENDIF}

procedure TsgXMLIDE.OnCopyEntities(const Image, AEntities: TObject);
begin
  if xeOnCopyEntities in FXMLEvents then
    OnEventCustom(xeOnCopyEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnCreateEntities(const Image, AEntities: TObject);
begin
  if xeOnCreateEntities in FXMLEvents then
    OnEventCustom(xeOnCreateEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnDeleteEntities(const Image, AEntities: TObject);
begin
  if xeOnDeleteEntities in FXMLEvents then
    OnEventCustom(xeOnDeleteEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if xeOnKeyDown in FXMLEvents then
    OnEventCustom(xeOnKeyDown, nil, nil, [cnstXMLCmdKeyChar, cnstXMLCmdShift],
      [IntToStr(Integer(Key)), ShiftToStr(Shift)], True);
end;

procedure TsgXMLIDE.OnKeyPress(Sender: TObject; var Key: Char);
begin
  if xeOnKeyPress in FXMLEvents then
    OnEventCustom(xeOnKeyPress, nil, nil,
     [cnstXMLCmdKeyChar], [IntToStr(Integer(Key))], True);
end;

procedure TsgXMLIDE.OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if xeOnKeyUp in FXMLEvents then
    OnEventCustom(xeOnKeyUp, nil, nil, [cnstXMLCmdKeyChar, cnstXMLCmdShift],
      [IntToStr(Integer(Key)), ShiftToStr(Shift)], True);
end;

procedure TsgXMLIDE.OnLayoutBeforeChange(const Image, AEntities: TObject);
begin
  if xeOnLayoutBeforeChange in FXMLEvents then
    OnEventCustom(xeOnLayoutBeforeChange, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnLayoutAfterChange(const Image, AEntities: TObject);
begin
  if xeOnLayoutBeforeChange in FXMLEvents then
    OnEventCustom(xeOnLayoutAfterChange, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnDragAndDrop(Sender: TObject);
var
  vDragAndDropNode: TsgNode;
begin
  if xeOnDragAndDrop in FXMLEvents then
  begin
    vDragAndDropNode := TsgNode(Sender);
    OnEventCustom(xeOnDragAndDrop, nil, nil, [],[], True, vDragAndDropNode);
  end;
end;

procedure TsgXMLIDE.OnMeasure(Sender: TObject);
var
  vMeasureNode: TsgNode;
begin
  if xeOnMeasure in FXMLEvents then
  begin
    vMeasureNode := TsgNode(Sender);
    OnEventCustom(xeOnMeasure, nil, nil, [],[], True, vMeasureNode);
  end;
end;

function TsgXMLIDE.CreateImageEvent(const AEvent: TsgXMLEvent;
  Sender: TObject; const AXML: string;
  const AAttribData: TStrings; var AOut: TsgNode): TsgNode;
begin
  Result := nil;
  AOut := nil;
  if AEvent in FXMLEvents then
  begin
    Result := CreateResultEvent(cnstEvents[AEvent].Name);
    AOut := Result.AddChildNV(cnstXMLOutput);
    AddXmlDataToOut(AOut, AXML);
    AddAttribDataToOut(AOut, AAttribData);
  end;
end;

procedure TsgXMLIDE.OnGetInfo(const Image: TObject; const AName, AXML: string;
  const AttribsData: TStrings);
var
  vNode, vOut: TsgNode;
begin
  vNode := CreateImageEvent(xeOnGetInfo, Image, AXML, AttribsData, vOut);
  try
    if Assigned(vOut) then
      vOut.AddAttribNV(cnstXMLName).ValueAsStr := AName;
  finally
    SendEvents(vNode, nil, nil);
  end;
end;

procedure TsgXMLIDE.OnImageNew(Sender: TObject; const AAttribData: TStrings);
var
  vNode, vOut: TsgNode;
begin
  vNode := CreateImageEvent(xeOnImageNew, Sender, '', AAttribData, vOut);
  try
  finally
    SendEvents(vNode, nil, nil);
  end;
end;

procedure TsgXMLIDE.OnImageLoad(Sender: TObject; const AState: Integer;
  const AAttribData: TStrings);
var
  vNode, vOut: TsgNode;
begin
  vNode := CreateImageEvent(xeOnImageLoad, Sender, '', AAttribData, vOut);
  try
    if Assigned(vOut) then
      vOut.AddAttribNV(cnstXMLCmdState).ValueAsInt := AState;
  finally
    SendEvents(vNode, nil, nil);
  end;
end;

procedure TsgXMLIDE.OnImageClose(Sender: TObject; const AAttribData: TStrings);
var
  vNode, vOut: TsgNode;
begin
  vNode := CreateImageEvent(xeOnImageClose, Sender, '', AAttribData, vOut);
  try
  finally
    SendEvents(vNode, nil, nil);
  end;
end;

procedure TsgXMLIDE.OnImageSave(Sender: TObject;
  const AFileName: string; const AFormat: TsgExportFormat;
  const AStream: TStream; const AAttribData: TStrings);
var
  vOut: TsgNode;
  vNode: TsgNode;
begin
  vNode := CreateImageEvent(xeOnImageSave, Sender, '', AAttribData, vOut);
  try
    if Assigned(vOut) then
    begin
      if Length(AFileName) > 0 then
      begin
        vOut.AddAttribNV(cnstXMLCmdFile).ValueAsStr := AFileName;
        vOut.AddAttribNV(cnstXMLFormat).ValueAsStr := GetExportExt(AFormat);
      end
      else
        ConvertStreamToOutputBase64(AStream, vOut, AFormat);
    end;
  finally
    SendEvents(vNode, nil, nil);
  end;
end;

procedure TsgXMLIDE.OnAcisNumber(const Sender: TObject;
  const AParams: TsgMessageParams);
var
  vOut: TsgNode;
  vNode: TsgNode;
begin
  vNode := CreateImageEvent(xeOnAcisNumber, Sender, '', nil, vOut);
  try
    if Assigned(vOut) then
    begin
      vOut.AddAttribNV(cnstXMLMessage).ValueAsStr := AParams.Msg;
      vOut.AddAttribNV(cnstXMLCount).ValueAsInt := AParams.Code;
    end;
  finally
    SendEvents(vNode, nil, nil);
  end;
end;

procedure TsgXMLIDE.OnDrawingSwitch(const Sender: TObject;
  const ADrawing: TsgDrawingXMLData);
var
  vOut: TsgNode;
  vNode: TsgNode;
begin
  vNode := CreateImageEvent(xeOnDrawingSwitch, Sender, '', nil, vOut);
  try
    if Assigned(vOut) then
      DrawingXMLDataToNode(ADrawing, vOut);
  finally
    SendEvents(vNode, nil, nil);
  end;
end;

{$IFDEF CS_USEFORM}
procedure TsgXMLIDE.OnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  OnMouseEvent(xeOnMouseDown, Sender, Button, Shift, X, Y, 0);
end;

procedure TsgXMLIDE.OnMouseEvent(const AType: TsgXMLEvent; Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, WheelDelta: Integer);
var
  vNode, vOut: TsgNode;
  vImgGuid: TGUID;
  vCADCoords: TFPoint;
  I: Integer;
  vHandles: string;
  vEntities: TsgObjectList;
begin
  if not (AType in FXMLEvents) then
    Exit;
  vImgGuid := GetImageGuid;
  vNode := CreateResultEvent(cnstEvents[AType].Name);
  try
    try
      vOut := vNode.AddChildNV(cnstXMLOutput);
      if not (AType in [xeOnMouseMove, xeOnMouseWheel]) then
      begin
        case Button of
          mbLeft   : vOut.AddAttribNV(cnstXMLCmdButton, cnstXMLCmdLButton);
          mbRight  : vOut.AddAttribNV(cnstXMLCmdButton, cnstXMLCmdRButton);
          mbMiddle : vOut.AddAttribNV(cnstXMLCmdButton, cnstXMLCmdMButton);
        end;
      end;
      vOut.AddAttribNV(cnstXMLCmdX, IntToStr(X));
      vOut.AddAttribNV(cnstXMlCmdY, IntToStr(Y));
      if CADEDitor <> nil then
      begin
        case AType of
          xeOnMouseDown:  vCADCoords := CADEditor.CoordsMDown;
          xeOnMouseUp:    vCADCoords := CADEditor.CoordsMUp;
        else
          vCADCoords := CADEditor.CoordsMMove;
        end;
        vOut.AddAttribNV(cnstXMLCmdPoint).ValueAsFPoint := vCADCoords;

        if Assigned(CADEditor.CoordMObject) then
        begin
          if CADEditor.CoordMObject is TsgDXFEntity then
            vHandles := HandleToStr(TsgDXFEntity(CADEditor.CoordMObject).Handle)
          else
          begin
            vHandles := '';
            if CADEditor.CoordMObject is TsgObjectList then
            begin
              vEntities := TsgObjectList(CADEditor.CoordMObject);
              for I := 0 to vEntities.Count - 1 do
              begin
                if I > 0 then
                  vHandles := vHandles + cnstPointSeparator;
                vHandles := vHandles + HandleToStr(TsgDXFEntity(vEntities[I]).Handle);
              end;
            end;
          end;
          vOut.AddAttribNV(cnstXMLNames[xmlHandle].Name).ValueAsStr := vHandles;
        end;
      end;
      AddAttribShift(vOut, Shift);
      if AType = xeOnMouseWheel then
        vOut.AddAttribNV(cnstXMLCmdWheelDelta).ValueAsInt := WheelDelta;
    except
      on E: Exception do
        AddExceptionToNode(vNode, E);
    end;
  finally
    SendEvents(vNode, nil, @vImgGuid);
  end;
end;

procedure TsgXMLIDE.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  OnMouseEvent(xeOnMouseMove, Sender, mbLeft, Shift, X, Y, 0);
end;

procedure TsgXMLIDE.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  OnMouseEvent(xeOnMouseUp, Sender, Button, Shift, X, Y, 0);
end;

procedure TsgXMLIDE.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := False;
  OnMouseEvent(xeOnMouseWheel, Sender, mbLeft, Shift,
    MousePos.X, MousePos.Y, WheelDelta);
end;
{$ENDIF}
procedure TsgXMLIDE.OnPaint(Sender: TObject);
begin
  if xeOnPaint in FXMLEvents then
    OnEventCustom(xeOnPaint, nil, nil, [], [], True);
end;

procedure TsgXMLIDE.OnPasteEntities(const Image, AEntities: TObject);
begin
  if xeOnPasteEntities in FXMLEvents then
    OnEventCustom(xeOnPasteEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnRedoEntities(const Image, AEntities: TObject);
begin
  if xeOnRedoEntities in FXMLEvents then
    OnEventCustom(xeOnRedoEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnSelectEntities(const Image, AEntities: TObject);
begin
  if xeOnSelectEntities in FXMLEvents then
    OnEventCustom(xeOnSelectEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnSelectBTIEntities(const Image, AEntities: TObject);
begin
  if xeOnSelectBTIEntities in FXMLEvents then
    OnEventCustom(xeOnSelectBTIEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnUndoEntities(const Image, AEntities: TObject);
begin
  if xeOnUndoEntities in FXMLEvents then
    OnEventCustom(xeOnUndoEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.OnCancelEntities(const Image, AEntities: TObject);
begin
  if xeOnCancelEntities in FXMLEvents then
    OnEventCustom(xeOnCancelEntities, Image, AEntities, [], [], True);
end;

procedure TsgXMLIDE.SetOnXMLMessage(const Value: TsgXMLMessage);
begin
  FOnXMLMessage := Value;
end;

// Mode
// 0 - set select
// 1 - remove from select
// 2 - add to select
function TsgXMLIDE.SetSelectEntities(AEntities: TList;
  const ANode: TsgNodeSample; const AMode: Integer = 0): Boolean;
var
  vSelectedEntities: TList;
  I, vCountBefore, vCountAfter: Integer;
begin
  Result := False;
  if Assigned(CADEditor) then
  begin
    case AMode of
     1, 2:
      begin
        vSelectedEntities := TList.Create;
        try
          CADEditor.GetSelectedEntities(vSelectedEntities);
          vCountBefore := vSelectedEntities.Count;
          if AMode > 1 then
          begin
            for I := 0 to AEntities.Count - 1 do
              vSelectedEntities.Add(AEntities[I]);
          end
          else
          begin
            for I := 0 to AEntities.Count - 1 do
              vSelectedEntities.Remove(AEntities[I]);
          end;
          vCountAfter := CADEditor.SetSelectedEntities(vSelectedEntities);
          if vCountAfter > vCountBefore then
            Result := True;
        finally
          vSelectedEntities.Free;
        end;
      end;
    else
      Result := True;
      CADEditor.SetSelectedEntities(AEntities);
    end
  end;
end;

{ TsgProxyEditor }

constructor TsgProxyEditor.Create(const AOwner: TObject);
begin
  inherited Create;
end;

procedure TsgProxyEditor.DoChangeEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnChangeEntities) then
    FOnChangeEntities(Image, AEntities);
end;

procedure TsgProxyEditor.DoCancelEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnCancelEntities) then
    FOnCancelEntities(Image, AEntities);
end;

function TsgProxyEditor.DoCanDeleteEntities(const Image, AEntities: TObject): Integer;
begin
  Result := cnstConfirmUndefined;
  if Assigned(FOnConfirmEntitiesDeletion) then
  begin
    Result := cnstConfirmNo;
    FOnConfirmEntitiesDeletion(Image, AEntities, Result);
  end;
end;

procedure TsgProxyEditor.DoCreateEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnCreateEntities) then
    FOnCreateEntities(Image, AEntities);
end;

procedure TsgProxyEditor.DoCopyEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnCopyEntities) then
    FOnCopyEntities(Image, AEntities);
end;

procedure TsgProxyEditor.DoDeleteEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnDeleteEntities) then
    FOnDeleteEntities(Image, AEntities);
end;

procedure TsgProxyEditor.DoGetInfo(const Image: TObject;
  const AName, AXML: string; const AAttribData: TStrings);
begin
  if Assigned(FOnGetInfo) then
    FOnGetInfo(Image, AName, AXML, AAttribData);
end;

procedure TsgProxyEditor.DoLayoutChange(const Image, AEntities: TObject);
begin
  if Assigned(FOnLayoutChange) then
    FOnLayoutChange(Image, AEntities);
end;

procedure TsgProxyEditor.DoLayoutBeforeChange(const Image, AEntities: TObject);
begin
  if Assigned(FOnLayoutBeforeChange) then
    FOnLayoutBeforeChange(Image, AEntities);
end;
{$IFDEF CS_USEFORM}
procedure TsgProxyEditor.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDownEvent) then
    FOnMouseDownEvent(Sender, Button, Shift, X, Y);
end;

procedure TsgProxyEditor.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMoveEvent) then
    FOnMouseMoveEvent(Sender, Shift, X, Y);
end;

procedure TsgProxyEditor.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUpEvent) then
    FOnMouseUpEvent(Sender, Button, Shift, X, Y);
end;

procedure TsgProxyEditor.DoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; X, Y: Integer; var AHadnled: Boolean);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Sender, Shift, WheelDelta, Point(X, Y), AHadnled);
end;

procedure TsgProxyEditor.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Sender, Key, Shift);
end;

procedure TsgProxyEditor.DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Sender, Key, Shift);
end;

procedure TsgProxyEditor.DoKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Sender, Key);
end;
{$ENDIF}
procedure TsgProxyEditor.DoPaint(Sender: TObject);
begin
  if Assigned(FOnPaint) then
    FOnPaint(Sender);
end;

procedure TsgProxyEditor.DoImageNew(Sender: TObject;
  const AAttribData: TStrings);
begin
  if Assigned(FOnImageNew) then
    FOnImageNew(Sender, AAttribData);
end;

procedure TsgProxyEditor.DoImageLoad(Sender: TObject; const AState: Integer;
  const AAttribData: TStrings);
begin
  if Assigned(FOnImageLoad) then
    FOnImageLoad(Sender, AState, AAttribData);
end;

procedure TsgProxyEditor.DoImageClose(Sender: TObject;
  const AAttribData: TStrings);
begin
  if Assigned(FOnImageClose) then
    FOnImageClose(Sender, AAttribData);
end;

procedure TsgProxyEditor.DoImageSave(Sender: TObject;
  const AFileName: string; const AFormat: TsgExportFormat;
  const AStream: TStream; const AAttribData: TStrings);
begin
  if Assigned(FOnImageSave) then
    FOnImageSave(Sender, AFileName, AFormat, AStream, AAttribData);
end;

procedure TsgProxyEditor.DoPasteEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnPasteEntities) then
    FOnPasteEntities(Image, AEntities);
end;

procedure TsgProxyEditor.DoRedoEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnRedoEntities) then
    FOnRedoEntities(Image, AEntities);
end;

procedure TsgProxyEditor.DoSelectEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnSelectEntities) then
    FOnSelectEntities(Image, AEntities);
end;

procedure TsgProxyEditor.DoSelectBTIEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnSelectBTIEntities) then
    FOnSelectBTIEntities(Image, AEntities);
end;

procedure TsgProxyEditor.DoUndoEntities(const Image, AEntities: TObject);
begin
  if Assigned(FOnUndoEntities) then
    FOnUndoEntities(Image, AEntities);
end;

function TsgProxyEditor.GetCurrentDrawingGuid: TGUID;
begin
  Result := cntsErrorGuid;
end;

procedure TsgProxyEditor.GetNearestEntity(const R: TRect; const AFlags: TsgNearestFlags;
  var NearestEntityName: string; var APoint2D: TPoint;
  var APoint3D: TFPoint);
begin

end;

function TsgProxyEditor.GetOwner: TObject;
begin
  Result := nil;
end;

function TsgProxyEditor.GetSilentMode: Boolean;
begin
  Result := False;
end;

function TsgProxyEditor.IsEntUsed(const AEnt: TsgDXFEntity): Boolean;
begin
  Result := False;
end;

function TsgProxyEditor.IsOnlyCommand: Boolean;
begin
  Result := False;
end;

procedure TsgProxyEditor.SetSilentMode(const AValue: Boolean);
begin
end;

procedure TsgProxyEditor.SetSkipChanges(const Value: Boolean);
begin
  FSkipChanges := Value;
end;

function TsgProxyEditor.SetSpecialLayers(const ABlocked, ANoSelected: string): Integer;
begin
  Result := 0;
end;

{ TsgXMLResult }

constructor TsgXMLResult.Create;
begin
  CreateByInstruction('');
end;

constructor TsgXMLResult.CreateByInstruction(AName: string);
begin
  inherited Create;
  if Length(AName) = 0 then
    AName := cnstXMLGlobalFunction;
  InitRoot(FXMLOut, FRootOut);
  if InitResultNodeOnCreating then
    CreateResultNode(AName);
end;

procedure TsgXMLResult.CreateResultNode(const AName: string);
var
  vResultsNode: TsgNode;
begin
  vResultsNode := TsgNode(RootOut.GetChildByName(cnstXMLResults));
  if not Assigned(vResultsNode) then
  begin
    vResultsNode := RootOut.AddChildNV(cnstXMLResults);
  end;
  FResultNode := vResultsNode.AddChildNV(cnstXMLResult);
  FResultNode.AddAttribNV(cnstXMLInstruction, AName);
end;

destructor TsgXMLResult.Destroy;
begin
  FreeAndNil(FXMLOut);
  inherited Destroy;
end;

function TsgXMLResult.GetErrors: TsgNode;
begin
  Result := nil;
  if Assigned(FResultNode) then
  begin
    Result := TsgNode(FResultNode.GetChildByName(cnstXMLErrors));
    if not Assigned(Result) then
    begin
      Result := TsgNode(FResultNode.AddChildNV(cnstXMLErrors));
    end;
  end;
end;

function TsgXMLResult.GetOutput: TsgNode;
begin
  Result := nil;
  if Assigned(FResultNode) then
  begin
    Result := TsgNode(FResultNode.GetChildByName(cnstXMLOutput));
    if not Assigned(Result) then
    begin
      Result := TsgNode(FResultNode.AddChildNV(cnstXMLOutput));
    end;
  end;
end;

function TsgXMLResult.GetResult: TsgNode;
begin
  Result := FResultNode;
end;

function TsgXMLResult.InitResultNodeOnCreating: Boolean;
begin
  Result := True;
end;

procedure TsgXMLResult.InitRoot(var AParser: TsgParser; var ARoot: TsgNode);
begin
  AParser := TsgParser.Create;
  ARoot := AParser.CreateRootNode(cnstXMLRootNode, True);
  ARoot.AddAttribNV(cnstXMLCmdVersion).ValueData.ValueAsVersion :=
    cnstXMLIntfaceVersion;
end;

{ TsgXMLOut }

function TsgXMLOut.AddAttribDataToOut(const AOut: TsgNode; const AItems: TStrings): Boolean;
var
  I: Integer;
  vItems, vItem: TsgNode;
  vIsNameValue: Boolean;
  S: string;
begin
  Result := False;
  if Assigned(AItems) then
  begin
    Result := True;
    vItems := AOut.AddChildNV(cnstXmlItems);
    if AItems.Count > 0 then
    begin
      vIsNameValue := Length(AItems.Names[0]) > 0;
      for I := 0 to AItems.Count - 1 do
      begin
        vItem := vItems.AddChildNV(cnstXmlItem);
        if vIsNameValue then
        begin
          S := AItems.Names[I];
          vItem.AddAttribNV(cnstXMLName).ValueAsStr := S;
          vItem.AddAttribNV(cnstXMLValue).ValueAsStr := {$IFDEF SGDEL_7}AItems.ValueFromIndex[I]{$ELSE}Copy(AItems[I], Length(S) + 2, MaxInt){$ENDIF};
        end
        else
          vItem.AddAttribNV(cnstXMLValue).ValueAsStr := AItems[I];
      end;
    end;
  end;
end;

function TsgXMLOut.AddXmlDataToOut(const AOut: TsgNode; const AXML: string): Boolean;
var
  vOutInfo, vInfo: TsgNode;
  vParser: TsgParser;
  vXmlName: string;
begin
  Result := False;
  if Length(AXML) > 0 then
  begin
    vXmlName := cnstXMLInfo;
    vOutInfo := AOut.AddChildNV(vXmlName);
    vParser := TsgParser.Create;
    try
      vParser.LoadFromString('<' + vXmlName + '>' + AXML + '</' + vXmlName + '>');
      vInfo := TsgNode(vParser.ROOT.NodeByName[vXmlName]);
      if Assigned(vInfo) then
        vOutInfo.Assign(vInfo);
    finally
      vParser.Free;
    end;
  end;
end;

procedure TsgXMLOut.AddXmlToOutput(const AXML: string);
begin
  AddXmlDataToOut(GetOutput, AXML);
end;

procedure TsgXMLOut.AddToError(const Attribs: TStrings);
begin
  GetErrors.AddChildNV(cnstXMLError).AddAttribs(Attribs);
end;

procedure TsgXMLOut.AddToErrorNV(const AName, AValue: string;
  const AMessage: string = '');
var
  vError: TsgNode;
begin
  vError := GetErrors.AddChildNV(cnstXMLError);
  vError.AddAttribNV(AName, AValue);
  if Length(AMessage) > 0 then
    vError.AddAttribNV(cnstXMLMessage).ValueAsStr := AMessage;
end;

procedure TsgXMLOut.AddToOutput(const Attribs: TStrings);
begin
  GetOutput.AddAttribs(Attribs);
end;

procedure TsgXMLOut.AddItemsToOutput(const AItems: TStrings);
begin
  AddAttribDataToOut(GetOutput, AItems);
end;

procedure TsgXMLOut.AddToOutputFromNode(const ANode: TObject);
var
  I: Integer;
  vNode: TsgNodeSample absolute ANode;
  vOut, vChild: TsgNode;
begin
  if not (ANode is TsgNode) then
    Exit;
  vOut := GetOutput;
  for I := 0 to vNode.ChildNodesCount - 1 do
  begin
    vChild := TsgNode.Create;
    vChild.Assign(vNode.ChildNodes[I]);
    vOut.AddChild(vChild);
  end;
end;

procedure TsgXMLOut.AddToOutputNV(const AName, AValue: string;
  const AMessage: string = '');
var
  vOut: TsgNode;
begin
  vOut := GetOutput;
  vOut.AddAttribNV(AName, AValue);
  if Length(AMessage) > 0 then
    vOut.AddAttribNV(cnstXMLMessage).ValueAsStr := AMessage;
end;

{ TsgXMLIDEIteateEntities }

procedure TsgXMLIDEIteateEntities.CalcEntititesBox(const AOnlyBox: Boolean);
var
  I, J: Integer;
  vBox: TFRect;
  vNode, vOutput: TsgNode;
  vSelectEnt: TsgDXFEntity;
  vHandles: string;
  vOwnerHandle: UInt64;
begin
  vOutput := FOutput;
  vOwnerHandle := cnstBadHandle;
  if Assigned(FCADParams.Insert) and (FCADParams.Insert.EntType <> ceInsert) then
    vOwnerHandle := FCADParams.Insert.Handle;
  for I := 0 to FEntities.Count - 1 do
  begin
    vSelectEnt := FEntities[I];
    vBox := GetRealBox(vSelectEnt.Box, FCADParams.Matrix);
    if AOnlyBox then
    begin
      vNode := vOutput.AddChildNV(vSelectEnt.GetXMLName);
      if Assigned(FInserts) and (FInserts.Count > 0) then
      begin
        vHandles := HandleToStr(TsgDXFEntity(FInserts[0]).Handle);
        for J := 1 to FInserts.Count - 1 do
          vHandles := vHandles + cnstPointSeparator + HandleToStr(TsgDXFEntity(FInserts[J]).Handle);
        vNode.AddAttribNV(cnstXMLCmdStack).ValueAsStr := vHandles;
      end;
    end
    else
    begin
      vNode := vSelectEnt.ToXML(vOutput);
      if vOwnerHandle <> cnstBadHandle then
        vNode.AddAttribNV(cnstXMLNames[xmlOwnerHandle].Name).ValueAsHandle := vOwnerHandle;
    end;
    vNode.AddAttribNV(cnstXMLCmdBoxCad).ValueAsFRect := vBox;
  end;
end;

constructor TsgXMLIDEIteateEntities.Create(AXMLIDE: TsgXMLIDE);
begin
  inherited Create;
  FXMLIDE := AXMLIDE;
  FEntities := TList.Create;
  FParentNode := TsgNode.Create;
end;

destructor TsgXMLIDEIteateEntities.Destroy;
begin
  FEntities.Free;
  FParentNode.Free;
  inherited Destroy;
end;

function TsgXMLIDEIteateEntities.ApplyWithFilter(AEntity: TsgDXFEntity): Integer;
var
  vTextEnt: TsgDXFText absolute AEntity;
  vFind, vText: string;
  vIsAccept: Boolean;
begin
  Result := 0;
  FEntities.Count := 0;
  if AEntity is TsgDXFText then
  begin
    vFind := FFindValue;
    vText := vTextEnt.Text;
    if (FMode and 1) <> 0 then
    begin
      vFind := AnsiUpperCase(vFind);
      vText := AnsiUpperCase(vText);
    end;
    if (FMode and 2) <> 0 then
      vIsAccept := Pos(vFind, vText) > 0
    else
      vIsAccept := vFind = vText;
    if vIsAccept then
      if FIsUseFilter then
        SelectWithFilter(TsgNode(FNode), AEntity, FEntities, FParentNode)
      else
        FEntities.Add(AEntity);
    CalcEntititesBox(False);
  end;
end;

function TsgXMLIDEIteateEntities.FindEntityByHandle(AEntity: TsgDXFEntity): Integer;
begin
  Result := 1;
  if Assigned(AEntity) then
  begin
    FEntities.Count := 0;
    if (AEntity.Handle = FFindHandle) then
    begin
      FEntities.Add(AEntity);
      CalcEntititesBox(True);
    end;
    if AEntity.IsInsert and Assigned(FInserts) then
      FInserts.Add(AEntity);
  end;
end;

function TsgXMLIDEIteateEntities.FindEntityEnd(AEntity: TsgDXFEntity): Integer;
begin
  Result := 1;
  if AEntity.IsInsert and Assigned(FInserts) then
  begin
    if (FInserts.Count > 0) and (FInserts.Last = AEntity) then
      FInserts.Count := FInserts.Count - 1;
  end;
end;

function TsgXMLIDEIteateEntities.ApplyXMLForEntity(AEntity: TsgDXFEntity): Integer;
var
  I: Integer;

  procedure ApplyXML(AEnt: TsgDXFEntity);
  var
    J,vCmd: Integer;
  begin
    if FEntities.IndexOf(AEnt) >= 0 then
      Exit;
    FEntities.Add(AEnt);
    FXMLIDE.FSelectedHandles.Clear;
    FXMLIDE.FSelectedHandles.Add(AEnt.Handle, nil);
    for J := 0 to FNode.ChildNodesCount - 1 do
      FXMLIDE.ProcessXMLNode(TsgNode(FNode.ChildNodes[J]), vCmd);
  end;

begin
  Result := 0;
  if FMode and 2 <> 0 then
    ApplyXML(AEntity);
  for I := 0 to AEntity.Count - 1 do
    ApplyXML(AEntity[I]);
end;

procedure TsgXMLIDEIteateEntities.FindTextMethod(const AEntities: TList);
var
  vEntity: TsgDXFEntity;
  I: Integer;
begin
  if AEntities.Count = 0 then
    FImage.CurrentLayout.Iterate(FImage.Converter, ApplyWithFilter, nil)
  else
    for I := 0 to AEntities.Count - 1 do
    begin
      vEntity := AEntities[I];
      if vEntity.IsInsert then
        TsgDXFInsert(vEntity).Block.Iterate(FImage.Converter, ApplyWithFilter, nil);
    end;
end;

procedure TsgXMLIDEIteateEntities.IteratorEntitiesMethod(const AEntities: TList);
var
  vEntity: TsgDXFEntity;
  I: Integer;
begin
  if AEntities.Count = 0 then
    FImage.CurrentLayout.Iterate(FImage.Converter, ApplyXMLForEntity, nil)
  else
    for I := 0 to AEntities.Count - 1 do
    begin
      vEntity := AEntities[I];
      ApplyXMLForEntity(vEntity);
      if (FMode and 1 <> 0) and vEntity.IsInsert then
        TsgDXFInsert(vEntity).Block.Iterate(FImage.Converter, ApplyXMLForEntity, nil);
    end;
end;

function TsgXMLIDEIteateEntities.IterateInternal(ANode: TsgNodeSample;
  AResult: IsgResultNode; const AFlags: Integer;
  AMethod: TsgXMLIDEIteateEntitiesMethod): TsgNode;
var
  vAutoInsert: Boolean;
  vNoUseAttrs,vSrcEnts: TList;
  vHandles: TsgInt64List;
  vAttrib: TsgNodeSample;
begin
  FNode := ANode;
  FOutput := AResult.Output;
  Result := FOutput;
  vAttrib := ANode.GetAttributeByName(cnstXMLMode);
  if Assigned(vAttrib) then
    FMode := vAttrib.ValueAsInt
  else
    FMode := 0;
  case AFlags of
    1, 2:
    begin
     vAttrib := ANode.GetAttributeByName(cnstXMLCmdValue);
      if Assigned(vAttrib) then
      begin
        if AFlags = 1 then
          FFindValue := vAttrib.ValueAsStr
        else
          FFindHandle := vAttrib.ValueAsInt64
      end
      else
      begin
        DoMessageError(AResult.Errors, sXMLInterfaceComFrmError, ANode);
        Exit;
      end;
    end;
  end;
  FillChar(FCADParams, SizeOf(FCADParams), 0);
  FCADParams.Matrix := cnstIdentityMat;
  FImage := FXMLIDE.CADImage;
  vAutoInsert := FImage.Converter.AutoInsert;
  try
    FImage.Converter.Params := @FCADParams;
    FImage.Converter.AutoInsert := True;
    vNoUseAttrs := TList.Create;
    try
      if AFlags <> 2 then
        FIsUseFilter := FXMLIDE.ExtractAttribsNoUseInFilter(TsgNode(ANode), vNoUseAttrs)
      else
        FIsUseFilter := False;
      vHandles := TsgInt64List.Create;
      try
        vSrcEnts := TList.Create;
        try
          FXMLIDE.GetEntityByNode(ANode, AResult.Errors, vHandles, vSrcEnts);
          AMethod(vSrcEnts);
        finally
          vSrcEnts.Free;
        end;
      finally
        vHandles.Free;
      end;
    finally
      FXMLIDE.AddAttribsNoUseInFilter(TsgNode(ANode), vNoUseAttrs);
      vNoUseAttrs.Free;
    end;
  finally
    FImage.Converter.AutoInsert := vAutoInsert;
  end;
end;

function TsgXMLIDEIteateEntities.FindText(ANode: TsgNodeSample;
  AResult: IsgResultNode): TsgNode;
begin
  Result := IterateInternal(ANode, AResult, 1, FindTextMethod);
end;

function TsgXMLIDEIteateEntities.FindEntity(ANode: TsgNodeSample; AResult: IsgResultNode): TsgNode;
var
  vAutoInsertPrev: Boolean;
  vParamsPrev: PsgCADIterate;
  vAttrib: TsgNodeSample;
begin
  FNode := ANode;
  FOutput := AResult.Output;
  Result := FOutput;
  vAttrib := ANode.GetAttributeByName(cnstXMLCmdValue);
  if not Assigned(vAttrib) then
  begin
    DoMessageError(AResult.Errors, sXMLInterfaceComFrmError, ANode);
    Exit;
  end;
  FFindHandle := vAttrib.ValueAsInt64;
  FMode := GetAttributeInt(ANode, cnstXMLMode, 0);
  FillChar(FCADParams, SizeOf(FCADParams), 0);
  FCADParams.Matrix := cnstIdentityMat;
  FImage := FXMLIDE.CADImage;
  FInserts := TList.Create;
  try
    vParamsPrev := FImage.Converter.Params;
    vAutoInsertPrev := FImage.Converter.AutoInsert;
    try
      FImage.Converter.Params := @FCADParams;
      FImage.Converter.AutoInsert := False;
      FImage.CurrentLayout.Iterate(FImage.Converter, FindEntityByHandle, FindEntityEnd);
    finally
      FImage.Converter.Params := vParamsPrev;
      FImage.Converter.AutoInsert := vAutoInsertPrev;
    end;
  finally
    FreeAndNil(FInserts);
  end;
end;

function TsgXMLIDEIteateEntities.IteratorEntities(ANode: TsgNodeSample;
  AResult: IsgResultNode): TsgNode;
begin
  Result := IterateInternal(ANode, AResult, 0, IteratorEntitiesMethod);
end;

initialization
 {$IFDEF SG_CADIMAGE_DLL.DLl}
 AppKeyWords := [xkGet, xkAdd, xkSelect, xkUnSelect,
    xkApply, xkDelete, xkGetSelected, xkGetBox, xkLoad, xkSave, xkGetImage,
    xkGetExternalFiles, xkGlobalVariable, xkGetSaveHandles,
    xkRegistration, xkHelp
   ];
 {$ENDIF}

finalization
  FreeAndNil(KeyWords)

end.
