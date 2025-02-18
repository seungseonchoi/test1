{************************************************************}
{                   Delphi VCL Extensions                    }
{                                                            }
{             3D Rendering with GLScene www.glscene.org      }
{                                                            }
{     Copyright (c) 2002 - 2019 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit sg3DNavigator;

{$I SGDXF.inc}
{$I CADSoftTools.inc}

{$IFDEF SG_ABVIEWER}
  {$DEFINE SG_ALL_3D_FILES}
{$ELSE}
  {$IFDEF SG_CADEDITORX}
    {$DEFINE SG_ALL_3D_FILES}
  {$ENDIF}
{$ENDIF}

{$UNDEF SG_USE_CONSTANT}
{$IFDEF SG_ABVIEWER}
  {$DEFINE SG_USE_CONSTANT}
  {$DEFINE USE_CLIP}
{$ENDIF}
{$IFDEF SG_CADEDITORX}
  {$DEFINE SG_USE_CONSTANT}
  {$DEFINE USE_CLIP}
{$ENDIF}
{$IFDEF SG_KERNEL_TEST}
  {$DEFINE SG_USE_CONSTANT}
{$ENDIF}
{$IFDEF SG_CADVIEWX}
  {$DEFINE SG_USE_CONSTANT}
  {$DEFINE USE_CLIP}
{$ENDIF}
{$IFDEF SG_INVENTORY}
  {$DEFINE SG_USE_CONSTANT}
{$ENDIF}
{$IFDEF SG_BIG_CLIENT}
  {$DEFINE SG_USE_CONSTANT}
  {$DEFINE USE_CLIP}
{$ENDIF}

{$IFDEF SG_INLINE}
  {$DEFINE USE_INLINE}
{$ENDIF}

//{$DEFINE DEBUG_STRUCTURE}
//{$DEFINE DEBUG_EXPLODED}

//{$DEFINE CHECK_TIME}
//{$DEFINE SG_CALC_NORMALS}
//{$DEFINE LOOP_STL}
//{$DEFINE LOOP_FACE_FILE_BUFFERED}

{$DEFINE ADD_POINT_CIRCLE}

{$IFDEF USE_CLIP}
// Clipping using the module GLMeshCSG
//{$DEFINE CLIP_GLMESH}
// Clipping with OpenGL using ClipPlane
//{$DEFINE CLIP_GLCLIP}
//{$DEFINE CLIP_CSG}
{$DEFINE CLIP_SGMESH}


{$IFNDEF CLIP_GLMESH}
  {$IFNDEF CLIP_SGMESH}
    {$IFNDEF CLIP_GLCLIP}
      {$IFNDEF CLIP_CSG}
         {$DEFINE CLIP_GLMESH}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$ENDIF}

{$DEFINE LOOP_GL_SCENE}

{$IFDEF CLIP_CSG}
  {$DEFINE CLIP_CSG_GLMESH}
  {$DEFINE CLIP_CSG_GLMESH_SGMESH}
{$ENDIF}

{$IFDEF CLIP_GLCSG}
  {$DEFINE CLIP_GLCLIP_GLCSG}
  {$DEFINE CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}
  {$DEFINE CLIP_GLCSG_GLMESH_SGMESH}
  {$DEFINE CLIP_GLCSG_GLMESH}
{$ENDIF}

{$IFDEF CLIP_GLCLIP}
  {$DEFINE CLIP_GLCLIP_GLCSG}
  {$DEFINE CLIP_GLCLIP_GLMESH}
  {$DEFINE CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}
{$ENDIF}

{$IFDEF CLIP_GLMESH}
  {$DEFINE CLIP_GLCLIP_GLMESH}
  {$DEFINE CLIP_GLCSG_GLMESH}
  {$DEFINE CLIP_CSG_GLMESH}
  {$DEFINE CLIP_GLMESH_SGMESH}
  {$DEFINE CLIP_CSG_GLMESH_SGMESH}
  {$DEFINE CLIP_GLCSG_GLMESH_SGMESH}
  {$DEFINE CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}
{$ENDIF}

{$IFDEF CLIP_SGMESH}
  {$DEFINE CLIP_GLMESH_SGMESH}
  {$DEFINE CLIP_CSG_GLMESH_SGMESH}
  {$DEFINE CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}
  {$DEFINE CLIP_GLCSG_GLMESH_SGMESH}
{$ENDIF}


{$IFNDEF CLIP_GLCLIP}
  {$IFNDEF CLIP_GLCSG}
    {$DEFINE CLIP_N_GLCLIP_N_GLCSG}
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF SG_FIREMONKEY}
  sgProxyGraphics, sgFMXTypes, System.UITypes, MeasureParams,
{$ELSE}
  sgOrbit3D, SGDrawingNavigator, MeasureParams,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls,
{$IFDEF SGFPC}
  FPImage, GraphUtil, GraphType,
  {$IFDEF SG_LINUX_FPC}
  LCLType, LCLIntf, LMessages,
  {$ENDIF}
{$ENDIF}
{$IFDEF GLSCENE13}
  GLVectorGeometry, GLBaseClasses, GLVectorTypes, GLVectorLists, GLGeometryBB, GLPersistentClasses,
{$ELSE}
  VectorGeometry, BaseClasses, VectorTypes, VectorLists, GeometryBB, PersistentClasses,
{$ENDIF}
  GLScene, GLVectorFileObjects, GLObjects, GLTexture, OpenGL1x, GLContext,
  GLMaterial, GLCoordinates, GLCrossPlatform, GLColor, GLRenderContextInfo,
  sgConsts, CADImage, GLNavigator, GLGeomObjects, GLCanvas,
  GLHUDObjects, GLDynamicTexture, Math, DXFConv, GLSelection,
  SyncObjs, GLGraphics, DXF, sgComparer, sgXMLParser, XMLInterface,
  sgFunction
{$IFDEF USE_CLIP}
  , CADExport, CADto3D
{$IFNDEF SG_NON_WIN_PLATFORM}
  ,ActiveX
{$ENDIF}
{$ENDIF}
{$IFDEF CLIP_GLMESH}
  , GLMeshCSG
{$ENDIF}
{$IFDEF CLIP_SGMESH}
  , sgMeshOperations, sg3DControls
{$ENDIF}
{$IFDEF SG_CALC_NORMALS}
  , MeshUtils
{$ENDIF}
  {$IFDEF SG_ALL_3D_FILES}
  ,GLFileOBJ, GLFileASE, GLFileB3D, GLFileGL2, GLFileGTS,
  GLFileLMTS, GLFileLWO, GLFileMD2, GLFileMD3, GLFileMD5, GLFileMDC, GLFileNMF,
  GLFileNurbs, GLFileOCT, GLFilePLY, GLFileQ3BSP, GLFileSMD, GLFileTIN,
  sgGLFileVRML, GLFile3DS,
  GLFileSTL, //Must be BEFORE GlFileX
  GLFileX
  {$ENDIF}
{$IFDEF SG_USE_CONSTANT}
  ,Constants, sgGraphics
{$ENDIF}
  ,GLState, OpenGLTokens,  GLTextureFormat, sgLists, sgSTLImage,
  GLHiddenLineShader, sgBitmap
{$IFDEF SGABVIEWER}
  ,sgApplicationVerInfo
{$ENDIF}
{$IFDEF SGDEL_XE2}
  ,System.Types, Vcl.Imaging.PNGImage
{$ENDIF}
  , sgTools ,sgModeller,sgMDefinitions, sgMFunctions
{$IF DEFINED(SG_OBB) OR DEFINED(SG_INTERNAL_TEST_AB3DKERNEL)}
  , sgMLists, System.Generics.Collections, System.Generics.Defaults
{$IFEND}
{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
  ,fmPolygons, fmStructure, frmFileStructure, sgMLists
{$ENDIF}
//{$IFDEF DEBUG_STRUCTURE AND DEBUG_EXPLODED}
{$IF DEFINED(DEBUG_STRUCTURE) OR DEFINED(DEBUG_EXPLODED)}
  ,Vcl.ComCtrls, Vcl.StdCtrls
//{$ENDIF}
{$IFEND}
  , sgDrawRect, sgHelperMeshes, Properties
{$IFDEF SG_XKT}
  , sgXktXeokit, sgXktModel, sgXktUtils
{$ENDIF};


type
  Esg3DNavError = class(Exception);
  Esg3DNavDLLError = class(Esg3DNavError);

  TsgTypeIntersect = (tiMesh, tiSnap, tiSnapEdge, tiDimension, tiSurfSurf);
  TsgExplodeProp = (epRadial, epX, epY, epZ);
  TsgTypeLine = (tlMass, tlOuterWire);

{$IFDEF GLSCENE13}
  TRenderContextInfo = TGLRenderContextInfo;
{$ENDIF}

{$IFDEF SG_OBB}
  TOBB = array[0..7] of TsgVector3d;
  POBB = ^TOBB;
{$ENDIF}

  Tsg3DDrawingNavigator = class;

  TsgExplodeProps = record
    ExplodeType: TsgExplodeProp;
    Value: Integer;
  end;

  TsgMousePoint = record
    Pos: TPoint;
    Shift: TShiftState;
  end;

  TProgress = class(TPersistent)
  private
    FPercentDone: Extended;
    FStep: Extended;
    FCaption: TCaption;
    FProgressEvent: TProgressEvent;
    FProgressSender: TObject;
    FSendPercentDone: ShortInt;
    FUpdateDelay: Cardinal;
    FUpdateThread: TThread;
    procedure SetCaption(const AValue: TCaption);
    procedure SetUpdateDelay(const Value: Cardinal);
    procedure DoProgressInternal(Stage: TProgressStage);
    procedure ThreadTerminate(Sender: TObject);
  protected
    procedure DoProgress(Stage: TProgressStage); virtual;
    procedure UpdateParams(const APercentDone, AStep: Extended); virtual;
    property ProgressSender: TObject read FProgressSender write FProgressSender;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Starting; virtual;
    procedure Progress; virtual;
    procedure Ending; virtual;
    property ProgressEvent: TProgressEvent read FProgressEvent write FProgressEvent;
    property PercentDone: Extended read FPercentDone write FPercentDone;
    property Step: Extended read FStep write FStep;
    property Caption: TCaption read FCaption write SetCaption;
    property UpdateDelay: Cardinal read FUpdateDelay write SetUpdateDelay;
  end;

  TCustomNode = class;

  TsgProcCustomNode = function(const ANode: TCustomNode): Integer of object;

  TCustomNode = class(TPersistentObjectList, IsgVisibleObject)
  private
    FOwner: TCustomNode;
    FObj: TObject;// "INSERT"
    FMesh: TObject;
    FBox: TFRect;
    FBoxByVisible: TFRect;
    FVisible: Boolean;
    FDelta: TAffineVector;
    procedure SetDelta(const Value: TAffineVector);
  protected
    //procedure SetBox(const ABox: TFRect);
    function GetOwner: TPersistent; override;
    procedure GetMeshList(const AMeshes: TsgObjectList);
    procedure NormalizeVisibilityTree;
    procedure Iterate(const AProc: TsgProcCustomNode);
    function IsAtomElementForExploder: Boolean;
  public
    constructor Create(AOwner: TCustomNode); reintroduce; virtual;
    function GetRoot: TCustomNode;
    class function GetNodeUnboxingElenent(ARoot, ANode: TCustomNode;
      const AClasses: array of TClass): TCustomNode;
    class function GetNodeOwnerByTopoClass(ARoot, ANode: TCustomNode;
      const AClasses: array of TClass): TCustomNode;
    class function GetPathKeyByNode(const ANode: TCustomNode): String;
    class procedure ClearVisualization(const ANode: TCustomNode);
    function ConvertBoxIfExploded(ARoot: TCustomNode;
      var ABox: TFRect): Boolean;
    function IsExploded: Boolean;
    function Find(AObj: TObject; var I: Integer): Boolean;
    procedure GetMatrix(out AMatrix: TFMatrix); virtual;
    function GetArea: Double;
    function GetMeasureKoef: Double;
    function GetBox(const IsCache: Boolean = False): TFRect; virtual;
{$IFDEF SG_ROTATE_CENTER_TEST}
    function GetCurrentBox(const IsCache: Boolean = False): TFRect;
{$ENDIF}
    function GetAlphaBlend: Integer;
    procedure SetAlphaBlend(const AValue: Integer);
    function GetVisibleElement: Boolean;
    procedure SetVisibleElement(const AValue: Boolean);
    function GetCustomColor: Cardinal;
    procedure SetCustomColor(const AValue: Cardinal);
    property Obj: TObject read FObj write FObj;
    property Owner: TCustomNode read FOwner;
    property Mesh: TObject read FMesh write FMesh;
    property Visible: Boolean read FVisible;
    property Delta: TAffineVector read FDelta write SetDelta;
  end;

  TsgGLLines = class;
  TsgGLPolyline = class;

  TsgMeshObjectAddDeleteProc = procedure(const APoly: TsgGLPolyline) of object;

  TsgMeshObject = class(TMeshObject)
  private
    FOriginalData: TMemoryStream;
    FVisibleData: TMemoryStream;
    FEntity: TObject;
    FNode: TCustomNode;
    FEdgeIndexList: TIntegerList;
    FArea: Double;
    FBox: TFRect;
    function GetRootInsert: TObject;
    function GetInsert: TObject;
    procedure AddMeshToGLPolyline(const APoly: TsgGLPolyline);
    procedure RemoveMeshToGLPolyline(const APoly: TsgGLPolyline);
    procedure AddRemoveConnectionWithEdges(AList: TsgGLLines;
      AProc: TsgMeshObjectAddDeleteProc);
  protected
    procedure AddConnectionWithEdges(AList: TsgGLLines);
    procedure BreakConnectionWithEdges(AList: TsgGLLines);
    procedure SetEntity(AEnt: TObject);
    procedure SetBox(const ABox: TFRect);
    function GetBox: TFRect;
    function GetArea: Double;
    procedure SetArea(const AArea: Double);
    procedure GetEdgeIndexListByData(var AEdgeIndexList: TIntegerList);
  public
    constructor CreateWithEnt(AOwner: TMeshObjectList; AEnt: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    function  IsUseVisibleData: Boolean;
    procedure SaveVisibleData;
    procedure RestoreVisibleData;
    procedure SaveData;
    procedure RestoreData;
    property Entity: TObject read FEntity;
    property Insert: TObject read GetInsert;
    property Node: TCustomNode read FNode write FNode;
    property RootInsert: TObject read GetRootInsert;
    property Area: Double read GetArea;
  end;

  TsgSnapPoint = class
  private
    FPoint: TFPoint;
  public
    property Point: TFPoint read FPoint write FPoint;
  end;

  Tsg3DNavigator = class;

{$IFDEF CLIP_GLCLIP}
  TsgGLHUDSprite = class(TGLHUDSprite)
  public
    procedure DoRender(var rci: TRenderContextInfo;
      renderSelf, renderChildren: Boolean); override;
  end;

  TsgProcRenderObject = procedure(const A3DNavigator: Tsg3DNavigator;
    var rci: TRenderContextInfo) of object;

{$ENDIF}
  //Current mini state for drawing TsgGLLines
  PsgGLLinesStates = ^TsgGLLinesStates;
  TsgGLLinesStates = record
    Color: TColorVector;
    PointSize: TGLfloat;
    LineWidth: TGLfloat;
  end;

  // Lite polyline by GLScene
  TsgGLPolyline = class(TFPointList(* TAffineVectorList*))
  private
    F3DNav: Tsg3DNavigator;
    FExData: TFPointList;
    FDimList: TFPointList;
    FTextList: TFPointList;
    FBegin, FEnd: TFPoint;
    FCenter: TFPoint;
    FRadius: Double;
    FIndexToCircle: Integer;
    FMaterialName: string;
    FMaterialNamePoint: string;
    FMaterialNameText: string;
    FMaterialNameLine: string;
    FLineWidth: Single;
    FHeight: Single;
    FArrowSize: Single;
    FDimTextPos: TFPoint;
    FPointSize: TGLfloat;
    FMode: Integer;
    FVisiblePoints: Boolean;
    FNormal: TFPoint;
    FNormals: TFPointList;
    FRect: TFRect;
    FType: Integer;
    FScale: Double;

    FFrameColor: TColor;

    FMeshList: TsgObjectList;
    FCountUses: Integer;
    FCurrentGLLinesStates: PsgGLLinesStates;
    procedure GenerateArrowScreen(APointScreen: TFPoint;
      AAngle: Double; const APoints: TFPointList);
    function IsContainsPoint(APoint: TFPoint): Boolean;
    function GetDistance: Double;
    function GetPointSize: TGLfloat;
    function GetRadius: Double;
    function GetByScale(AValue: Double): Double;
    function GetSizePixel(var rci: TRenderContextInfo): Double;
    function ClientToWorld(var rci: TRenderContextInfo;
      X, Y: Double; Z: Double = 0): TVector3f;
    function GetColorByMaterialName(var rci: TRenderContextInfo;
      AName: string; const ADefaultColor: TColor): TColor;
    procedure SetupColorByMaterialName(var rci: TRenderContextInfo;
      AName: string);
    procedure SetupColor(var rci: TRenderContextInfo;
      AName: string);
    procedure SetupLineStyle(var rci: TRenderContextInfo);
    procedure SetupPointStyle(var rci: TRenderContextInfo);
    procedure GLPush(var rci: TRenderContextInfo);
    procedure GLPop(var rci: TRenderContextInfo);
    function GetMaterailName: string;{$IFDEF SG_INLINE}inline;{$ENDIF}
    function GetMaterailNamePoint: string;{$IFDEF SG_INLINE}inline;{$ENDIF}
    function GetMaterialNameLine: string;{$IFDEF SG_INLINE}inline;{$ENDIF}
    function GetMaterialNameText: string;{$IFDEF SG_INLINE}inline;{$ENDIF}
    function GetWorldToScreenAffine(var rci: TRenderContextInfo;
      APoint: TFPoint; AMatrix: TMatrix4f): TAffineVector;
    function GetWorldToScreenFPoint(var rci: TRenderContextInfo;
      APoint: TFPoint; AMatrix: TMatrix4f): TFPoint;
    procedure SetMaterailName(const Value: string);{$IFDEF SG_INLINE}inline;{$ENDIF}
    procedure SetMaterailNamePoint(const Value: string);{$IFDEF SG_INLINE}inline;{$ENDIF}
    procedure DrawText(var rci: TRenderContextInfo;
      APoint1, APoint2: TFPoint);
    procedure SetMaterialNameLine(const Value: string);{$IFDEF SG_INLINE}inline;{$ENDIF}
    procedure SetMaterialNameText(const Value: string);{$IFDEF SG_INLINE}inline;{$ENDIF}
    procedure SetPointSize(const Value: TGLfloat);

    property FrameColor: TColor read FFrameColor
      write FFrameColor;
  public
    constructor Create; override;
    constructor Create(A3DNav: Tsg3DNavigator = nil); overload;//override;
    destructor Destroy; override;
    procedure Assign(Source: TsgBaseList); override;
    procedure BuildListPoints(var rci: TRenderContextInfo);
    procedure BuildList(var rci: TRenderContextInfo);
    property Center: TFPoint read FCenter write FCenter;
    property Distance: Double read GetDistance;
    property Radius: Double read GetRadius;
    property LineWidth: Single read FLineWidth write FLineWidth;
    property ArrowSize: Single read FArrowSize write FArrowSize;
    property Mode: Integer read FMode write FMode;
    property MaterialName: string read GetMaterailName write SetMaterailName;
    property MaterialNamePoint: string read GetMaterailNamePoint
      write SetMaterailNamePoint;
    property MaterialNameText: string read GetMaterialNameText
      write SetMaterialNameText;
    property MaterialNameLine: string read GetMaterialNameLine
      write SetMaterialNameLine;
    property PointSize: TGLfloat read GetPointSize write SetPointSize;
    property VisiblePoints: Boolean read FVisiblePoints write FVisiblePoints;
  end;

  TsgGLLines = class(TGLImmaterialSceneObject)
  private
    FPolylines: TsgObjectList;
    FDepthTest: Boolean;
    FVisiblePoints: Boolean;
    FCurrentGLLinesStates: TsgGLLinesStates;
    procedure PreProcessing(const AProgressObject: TProgress;
      const AAccuracy: Double; const ARoot: TPersistentObjectList);
  protected
    function ReleaseLastNode: TsgGLPolyline;{$IFDEF SG_INLINE}inline;{$ENDIF}
    procedure ApplyPolylinesScale(const AScale: Double; ADeep: Integer = MaxInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure ClearGLHandles;
    procedure DeleteLastNode;
    function AddNode: TsgGLPolyline;
    procedure BuildList(var rci: TRenderContextInfo); override;
    procedure DoRender(var rci: TRenderContextInfo; ARenderSelf,
      ARenderChildren: Boolean); override;
    property DepthTest: Boolean read FDepthTest write FDepthTest;
    property VisiblePoints: Boolean read FVisiblePoints write FVisiblePoints;
  end;


  TsgMeshObjectList = class(TMeshObjectList)
  public
      procedure BuildList(var mrci: TRenderContextInfo); override;
  end;

  TsgGLPoints = class(TGLPoints)
  public
  end;

  Tsg3DAxis = class(TGLMemoryViewer)
  private
    FDefCameraScale: Single;
    FGLScene: TGLScene;
    FGLDCAxes: TGLDummyCube;
    FGLArrowX: TGLArrowLine;
    FGLArrowY: TGLArrowLine;
    FGLArrowZ: TGLArrowLine;
    FGLCamera: TGLCamera;
    function GetBoundingSphereRadius: Single;
  protected
    procedure SetObjectsVisible(AVisible: Boolean);
    property GLDCAxes: TGLDummyCube read FGLDCAxes;
  public
    constructor CreateEx(AGLScene: TGLScene; AOwner: TComponent);
    procedure HideAxis;
    procedure LoadIdentity;
    procedure ShowAxis;
    procedure SetCameraPosition(X, Y, Z: Single);
    procedure SetDefPositions;
    procedure ResetRotations;
    property BoundingSphereRadius: Single read GetBoundingSphereRadius;
    property DefCameraScale: Single read FDefCameraScale;
  end;

{$IFDEF CLIP_SGMESH}
  Tsg3DNavigator = class(TGLNonVisualViewer, IsgSceneViewerComponent)
{$ELSE}
  Tsg3DNavigator = class(TGLNonVisualViewer)
{$ENDIF}
  private
    FAxes: Tsg3DAxis;
    FBoxImage: TFRect;
{$IFDEF USE_CLIP}
    FCrossSectionOn: Boolean;
    FLockClip: Boolean;
{$IFDEF CLIP_GLCLIP_GLMESH}
    FClip : Array [GL_CLIP_PLANE0..GL_CLIP_PLANE5,0..1] of TFPoint;
    FClipGL : Array [GL_CLIP_PLANE0..GL_CLIP_PLANE5, 0..3] Of Double;
    FClipUse: Array [GL_CLIP_PLANE0..GL_CLIP_PLANE5] Of Boolean;
{$ENDIF}
    //FMeshContext: PsgContext;
    FMeshContextList: TsgPointerList;// PsgContext;
    //FOnProgress: TProgressEvent;
    FSceneCenter: TAffineVector;
    FSceneRadius: Single;
{$ENDIF}
{$IFDEF CLIP_CSG_GLMESH_SGMESH }
    FLastOriginalMesh: Integer;
    FLineOuterWireOld, FLineMassOld: TsgGLLines;
{$IFDEF CLIP_SGMESH}
    FOriginalMeshData: TsgObjectList;
    FCuttingPlanes: TGLCuttingPlanes;
    FGizmoCrossSection: TsgPlaneGizmo;
{$ENDIF}
{$ENDIF}

    FCameraStyle: TGLCameraStyle;
    FDefCameraScale: Single;
{$IFDEF CLIP_GLCLIP}
    FGLAxisImage: TsgGLHUDSprite;
{$ELSE}
    FGLAxisImage: TGLHUDSprite;
{$ENDIF}
    FGLScene: TGLScene;
    FGLMaterialLibrary: TGLMaterialLibrary;
    FLights: array[0..7] of TGLLightSource;
    FGLCamera: TGLCamera;
    FGLCubeExtents: TGLCube;
    FGLDCScene: TGLDummyCube;
    FGLFreeForm: TGLFreeForm;
    FGLDefSceneMat: {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.TMatrix;
    FGLDCTarget: TGLDummyCube;
    FGLDirectRender: TGLDirectOpenGL;
    FGLSphere: TGLSphere;
    FShowCubeExtents: Boolean;
    FShowAxis: Boolean;
    FAABBBox: TAABB;
    FCenter: TVector3f;
    FCurrentLine: TsgGLPolyline;
    FOnScenePositionChange: TNotifyEvent;
    FOnAfterRender: TNotifyEvent;
    FOnBeforeRender: TNotifyEvent;
    FOnMeasureDeleteEvent: TNotifyEvent;
    FOnWrapUpRender: TDirectRenderEvent;
    FLineMass: TsgGLLines;
    FLineOuterWire: TsgGLLines;
    FTextMass: TsgGLLines;
    FBoxLine: TsgGLLines;
    FDimensionList: TsgObjectList;
    FDimensionListClear: Boolean;
    FIsVisibleDimensionList: Boolean;
    FScaleByPrint: Double;
    FIsDirty: Integer;
    FTempListObjects: TsgObjectList;
    FVSync: TVSyncMode;
    F3DDrawingNavigator: Tsg3DDrawingNavigator;
    FPainting: Boolean;
    procedure DoOnAfter3DAxisRender(Sender: TObject);
    procedure DoOnAfterRender(Sender: TObject);
    procedure DoOnBeforeRender(Sender: TObject);
    procedure DoOnBefore3DAxisRender(Sender: TObject);
    procedure DoOnDirectOGLRender(Sender: TObject; var rci: TRenderContextInfo);
    procedure DoOnWrapUpRender(Sender: TObject; var rci: TRenderContextInfo);
    procedure DoResetCamera;
    procedure DoOnInitiateRendering(Sender: TObject; var rci: TRenderContextInfo);
{$IFDEF CLIP_GLCLIP_GLCSG}
    procedure DoInitiateRenderingBuffer(Sender: TObject; var rci: TRenderContextInfo);
    procedure DoWrapUpRenderingBuffer(Sender: TObject; var rci: TRenderContextInfo);
{$ENDIF}
    function GetBackgroundColor: TColor;
    function GetBoundingSphereRadius: Single;
    function GetDefSceneMatrix: {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.TMatrix;
    function GetFilterProp: string;
    function GetGLFreeForm: TGLFreeForm;
    function GetShowAxis: Boolean;
    function GetShowCubeExtents: Boolean;
    function GetStyleCamera: TGLCameraStyle;
    function GetTrianglesCount: Integer;
    procedure HideLights;
    procedure HideScene;
    procedure SetAxisPos;
    procedure SetShowAxis(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetShowCubeExtents(const Value: Boolean);
    procedure SetStyleCamera(const Value: TGLCameraStyle);
    procedure SetSceneScale;
    procedure ShowLights;
    procedure ShowScene;
    procedure RenderAxis;
    procedure ScenePositionChange(Sender: TObject);
    procedure UpdateArrows;
    // lights
    function GeLights(Index: Integer): TGLLightSource;
    function GeLightsCount: Integer;

    function CreateGLPolyline: TsgGLPolyline;
    procedure DeleteGLPolyline(AObject: TObject);
    function GetIsVisibleDimensionList: Boolean;
    procedure SetIsVisibleDimensionList(const Value: Boolean);
    function GetScaleByPrint: Double;
    procedure SetScaleByPrint(const Value: Double);
    function GetFieldOfView: single;
    procedure SetFieldOfView(const Value: single);
    function GetIsRenderingContextAvailable: Boolean;
    procedure DoBeforeRender(Sender: TObject);
    procedure ForceRenderingContext;
  protected
{$IFDEF CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}
    procedure AddClip(AClipPlan: Integer; APointPlan, ANormal: TFPoint);
    procedure RemoveClip(AClipPlan: Integer);
{$IFDEF CLIP_SGMESH}
    procedure SaveOriginalMeshData;
    procedure RestoreOriginalMeshData;
{$ENDIF}
{$ENDIF}
    function ClientToWorld(X, Y: Double; Z: Double = 0): TVector3f;
    procedure InitSceneObject(AObject: TGLBaseSceneObject;
      const APosition, ADirection: TVector3f; const AOffs: Single);
    procedure PositionLight(const ACenter: TVector3f;
      const ADistance: Single);
    function GetMaterialByName(AMaterialName: string): TGLMaterial;
    procedure UpdateMaterialLibrary(const ADrawMode: TsgDXFDrawMode);
    function GetTypeIntersects(X, Y: Integer;
      const AMeshObjects: TsgObjectList; var ACancel: Boolean;
      const APoints, ANormals: TFPointList;
      const AType: TsgTypeIntersect): Boolean;
    function GetTypeIntersect(X, Y: Integer;
      var AObject: TObject; AIntersect, ANormal: PFPoint;
      var ACancel: Boolean; const AType: TsgTypeIntersect): Boolean;
    function GetDimension(X, Y: Integer): TObject;
    procedure ClearDimension;
    function IsClipPlaneUses: Boolean;
    procedure DoBufferChange(Sender: TObject); override;
    procedure DoBufferStructuralChange(Sender: TObject); override;
    procedure DoCreateRC;
    procedure Painting;
    procedure Painted;
    { IsgSceneViewerComponent }
    function GetBuffer: TGLSceneBuffer;
    function GetBaseRenderObject: TGLBaseSceneObject;

    property OnWrapUpRender: TDirectRenderEvent read FOnWrapUpRender write FOnWrapUpRender;
    property IsVisibleDimensionList: Boolean read GetIsVisibleDimensionList
      write SetIsVisibleDimensionList;
    property ScaleByPrint: Double read GetScaleByPrint write SetScaleByPrint;
    property VSync: TVSyncMode read FVSync write FVSync default vsmNoSync;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure DoAfterLoadScene;
    class function GetFilter: string;
    function IsEmpty: Boolean;
    procedure LoadFromFile(const AFileName: string; ADoClear: Boolean = True);
    procedure LoadFromStream(const AFileName: string; Stream: TStream;
      ADoClear: Boolean = True);
    procedure LoadSceneIdentity;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Render(baseObject: TGLBaseSceneObject = nil); override;
    procedure SetDefSceneMatrix;
    procedure Rotate(const APitch, ATurn, ARoll: Extended); overload;
    procedure Rotate(Axis: TsgAxes; Angle: Extended); overload;
    procedure PerformAutoCentering;
    procedure Translate(const AVector: TAffineVector);
    procedure Invalidate;
    function Perform(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;

    property Axes: Tsg3DAxis read FAxes;
    property BoundingSphereRadius: Single read GetBoundingSphereRadius;
    property CameraStyle: TGLCameraStyle read GetStyleCamera write SetStyleCamera;
    property DefSceneMatrix: {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.TMatrix read GetDefSceneMatrix;
    property FieldOfView: single read GetFieldOfView write SetFieldOfView;
    {$IFDEF SGDEL_2006}
    property Filter: string read GetFilterProp;
    {$ENDIF}
    property GLDCScene:TGLDummyCube read FGLDCScene;
    property GLDCTarget:TGLDummyCube read FGLDCTarget;
    property GLFreeForm: TGLFreeForm read GetGLFreeForm;
    property Lights[Index: Integer]: TGLLightSource read GeLights;
    property LightsCount: Integer read GeLightsCount;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property ShowAxis: Boolean read GetShowAxis write SetShowAxis;
    property ShowCubeExtents: Boolean read GetShowCubeExtents write SetShowCubeExtents;
    property TrianglesCount: Integer read GetTrianglesCount;
    property IsRenderingContextAvailable: Boolean read GetIsRenderingContextAvailable;
    property OnScenePositionChange: TNotifyEvent read FOnScenePositionChange write FOnScenePositionChange;
    property OnAfterRender: TNotifyEvent read FOnAfterRender write FOnAfterRender;
    property OnBeforeRender: TNotifyEvent read FOnBeforeRender write FOnBeforeRender;
{$IFDEF CLIP_SGMESH}
    property CuttingPlanes: TGLCuttingPlanes read FCuttingPlanes;
    property GizmoCrossSection: TsgPlaneGizmo read FGizmoCrossSection;
{$ENDIF}
  end;

  TsgGLGraphic = class(TsgGLGraphicCustom)
  private
    FNavigator: Tsg3DDrawingNavigator;
    FViewport: TRectangle;
    FBuffer: TGLSceneBuffer;
    FChangedCount: Integer;
    FPrinting: Boolean;
  protected
    procedure Changed(Sender: TObject); override;
    procedure DoRender;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure DrawTransparent(ACanvas: TCanvas; const Rect: TRect; Opacity: Byte); {$IFDEF SGDEL_2009}override{$ELSE}virtual{$ENDIF};
    property Viewport: TRectangle read FViewport write FViewport;
  public
    procedure AssignGLProperties(const AGraphic: TsgGLGraphic);
    procedure UpdateViewport; override;
    procedure SetNavigator(const ANavigator: Tsg3DDrawingNavigator);
    property Printing: Boolean read FPrinting write FPrinting;
  end;

  Tsg3DShowingStyle = (ssSmoothShading, ssFlatShading, ssHiddenLines,
    ssWireframe, ssLighting, ssOutlineShader);
  Tsg3DShowingStyles = set of Tsg3DShowingStyle;

  TsgSupportAlphaBlend = class
  public
    class function GetAlpha(const AMeshes: TsgObjectList): Double;
    class procedure SetAlpha(const AMeshLib: TGLMaterialLibrary;
      const AMeshes: TsgObjectList;
      const AValue: Double);
  end;

  TsgSupportCustomColor = class
  public
    class function GetCustomColor(const AMeshes: TsgObjectList): string;
    class procedure SetCustomColor(const AMeshLib: TGLMaterialLibrary;
      const AMeshes: TsgObjectList;
      const AValue: string);
  end;

  TsgPickObject = class(TPersistent)
  private
    FEnabled: Boolean;
    FOwner: TPersistent;
    FObjects: TsgObjectList;
    FOnPick: TNotifyEvent;
    FPick: TObject;
    FPickCache: TObject;
    FPickCached: Boolean;
    FLines: TsgGLLines;
    FPoly: TsgGLPolyline;
    FColor: TColor;
    FLineWidth: Single;
    FTag: TObject;
    FPos: TPoint;
    FTopNode: TCustomNode;
    FUserString: string;
    procedure SetObject(AObject: TObject);
    function GetObject: TObject;
    function GetGLVisible: Boolean;
    procedure SetGLVisible(const Value: Boolean);
    procedure SetUserString(const Value: string);
  protected
    procedure DoPick;
    procedure CreateLines;
    function GetOwner: TPersistent; override;
    property OnPick: TNotifyEvent read FOnPick write FOnPick;
    property Lines: TsgGLLines read FLines;
    property Pos: TPoint read FPos write FPos;
    property TopNode: TCustomNode read FTopNode write FTopNode;
    property GLVisible: Boolean read GetGLVisible write SetGLVisible;
    property UserString: string read FUserString write SetUserString;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure ClearLines;
    procedure BuildLines;
    procedure SetAlpha(const Alpha: Double);
    function GetAlpha: Double;
    property Obj: TObject read GetObject write SetObject;
    property Objects: TsgObjectList read FObjects;
    property Color: TColor read FColor write FColor;
    property LineWidth: Single read FLineWidth write FLineWidth;
    property PickCached: Boolean read FPickCached write FPickCached;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Tag: TObject read FTag write FTag;
  end;

  TTransformation = class;

  TsgOnDim3dCreateProc = procedure(const APickObj: TsgPickObject;
    const AType: Integer; const ABoxOffset: TFPoint) of object;

{$IFDEF SG_FIREMONKEY}
  TCreateParams = record
    Caption: PChar;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClass;
    WinClassName: array[0..255] of Char;
  end;

  TsgOrbit3D = class(TControl)
    procedure PaintWindow(AContextInfo: THandle); virtual; abstract;
  end;

  TsgDrawingNavigator = class(TObject)
    procedure CreateWnd; virtual; abstract;
    procedure DestroyWnd; virtual; abstract;
    procedure Loaded; virtual; abstract;
    function CoordinateConvertion(ACoordX, ACoordY: Integer;
      var APointInUCS: TFPoint): TFPoint;  virtual; abstract;
    function CreateSnapShotBitmap: Graphics.TBitmap; virtual; abstract;
    procedure DoOnColorChanged(const AColor: TColor); virtual; abstract;
    procedure DoOnCursorChanged; virtual; abstract;
    procedure DoPictureChange(Sender: TObject); virtual; abstract;
    function FinishEntity(Entity: TsgDXFEntity): Integer; virtual; abstract;
    function GetActualGraphic: TGraphic; virtual; abstract;
    function GetBox: TFRect; virtual; abstract;
    function GetBoxOffset: TFPoint; virtual; abstract;
    function GetEntity(Entity: TsgDXFEntity): Integer; virtual; abstract;
    function GetOrbit3DClass: TClass; virtual; abstract;
    function GetSizesAsString: string; virtual; abstract;
    procedure GetView(var AMatrix: TFMatrix); virtual; abstract;
    function GetHasTriangledMesh: Boolean; virtual; abstract;
    function GetZoomInEnable: Boolean; virtual; abstract;
    function GetZoomOutEnable: Boolean; virtual; abstract;
    function InitializeIterateParams: PsgCADIterate; virtual; abstract;
    procedure LoadView(const AMatrix: TFMatrix;
      const APostMessage: Boolean = True); virtual; abstract;
    procedure VisibleChanging; virtual; abstract;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; abstract;
    procedure OrbitVisibleChanging(Sender: TObject); virtual; abstract;
    procedure StopMouseMoveOperationsHandler(var Message: TMessage); virtual; abstract;
    function AlterScale(Factor: Double; IsAbsolute: Boolean;
      Position: TPoint): Boolean; virtual; abstract;
    function BeginUpdate: Integer; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure CreateParams(var Params: TCreateParams); virtual; abstract;
    function Empty: Boolean; virtual; abstract;
    function EndUpdate: Integer; virtual; abstract;
    procedure FitToSize; virtual; abstract;
    function GetDrawingCoords(ACoordX, ACoordY: Integer;
      var AUnits: string): TFPoint; virtual; abstract;
    function GetDrawingInternalCoords(ACoordX, ACoordY: Integer): TFPoint; virtual; abstract;
    function GetDrawingUCSCoords(ACoordX, ACoordY: Integer;
      var AUnits: string): TFPoint; virtual; abstract;
    function GetRealPoint(ACoordX, ACoordY: Double;
      var APointInUCS: TFPoint): TFPoint; virtual; abstract;
    function IsCanBeEdited: Boolean; virtual; abstract;
    function IsCanShowCoords: Boolean; virtual; abstract;
    function IsCanShowSizes: Boolean; virtual; abstract;
    function IsUseGraphic: Boolean; virtual; abstract;
    procedure LoadFromConverter(const AConverter: TsgDXFConverter;
      const ACADImage: TsgCADImage); virtual; abstract;
    procedure LoadFromFile(const FileName: string); virtual; abstract;
    function MovePictureRect2D(const ADeltaX, ADeltaY: Double): Boolean; virtual; abstract;
    function OpenVPort: Boolean; virtual; abstract;
    procedure RotToView(const A3DView: TsgDXFViewDirection); virtual; abstract;
    procedure WndProc(var Message: TMessage); virtual; abstract;
  end;
{$ENDIF}

  Tsg3DDrawingNavigator = class(TsgDrawingNavigator)
  private
{$IFDEF DEBUG_STRUCTURE}
    FTreeView: TTreeView;
{$ENDIF}
    FInsertStack: TsgList;

    FCurrMeshObject: TMeshObject;
    FCurrVertexes: TFGVertexIndexList;
    FCurrColor: TColor;
    FNeedNormals: Boolean;
    FCurrMeshObject3dFace: TMeshObject;
    FCurrVertexes3dFace: TFGVertexIndexList;
    FCurrColor3dFace: TColor;
    FNeedNormals3dFace: Boolean;

    FDeferredObjectToDelete: TsgObjectList;

    FCurrLineWieght: Single;
    FConverter: TsgDXFConverter;
    FDefaultColor: TColor;
    FIsLoadedFromConv: Boolean;
    FIsBrepEntityIterate: Boolean;
    FNav3D: Tsg3DNavigator;

    FHiddenLinesShader: TGLShader;
    FOutlineShader: TGLShader;

    FLoadFromImage: TsgCADImage;
    FShowingStyle: Tsg3DShowingStyles;
    FVectorFile: {$IFDEF GLSCENE13}TGLVectorFile{$ELSE}TVectorFile{$ENDIF};
    FCoord: TVector3f;
    FCoordInUCS: TVector3f;
    FDeviationCoefficient: Double;
    FIsHasTriangledMesh: Boolean;
    FOuterWireColor: TColor;
    FCrossSectionEdgesColor: TColor;
    FOuterWireLineWieght: Single;
    FBoxOffs: TFPoint;
    FShowEdges: Boolean;
    FShowBoxDimensions: Boolean;
    FVisibleScene: Boolean;

    FEdgeIntersect: Boolean;
    FSnapObj: array[0..1] of TsgPickObject;
    FIntersectPointSufr: array[0..1] of TFPoint;
    FIntersectNormalSurf: array[0..1] of TFPoint;

{$IFDEF USE_CLIP}
    FSnapPlan: array[0..2] of TsgPickObject;
    FIntersectPointPlanSufr: array[0..2] of TFPoint;
    FIntersectNormalPlanSurf: array[0..2] of TFPoint;
    FIndexSnapPlan, FSnapPlanCount: Integer;
    FSnapPlanEntity: Boolean;

    FOnPickPointClipEvent: TNotifyEvent;
    FOnCuttingPlanesChangeEvent: TNotifyEvent;
{$ENDIF}
    FIndexSnap: Integer;
    FSnapEdge: TsgPickObject;

    FSnapDim: TsgPickObject;

    FOriginalObject: TsgGLPolyline;
    FMoveDim: TsgPickObject;
    FDimSize: Double;

    FDimImageByDrawing: TsgCADImage;
    FTextByDrawing: TsgDXFText;
    FDimByDrawing: TsgDXFDimension;
    FAccuracyDim: Double;

    FHighLightObj: TsgPickObject;
    FOnHighLight: TNotifyEvent;
    FMove: TsgMousePoint;
    FDown: TsgMousePoint;
    FUp: TsgMousePoint;
    FHighlightSaveMatName: string;
    FHighlightSaveMatNames: TStringList;

//    //Extrema
//    FSurfIndex: TsgIntegerList;
//    FIntersectPointSufr: TFPointList;
//    FIntersectNormalSurf: TFPointList;

    FSearchEntMesh: TsgMeshObject;
    FPickObjects: TsgPickObject;
    FOnPickObject: TNotifyEvent;
    FOnMeasureEvent: TNotifyEvent;
    FIntersectCache: PFPoint;
    FLockPaintSysMenuIcons: Boolean;
    FRoot: TPersistentObjectList;
    FInsert: TsgDXFInsert;
    FMouseRotate: Boolean;

    FDrawMode: TsgDXFDrawMode;
    FLoadFromImageChange: TNotifyEvent;
    FTransformation: TTransformation;
    FUserHighLight: Boolean;
    FMeasureMode3D: TsgMeasureMode3d;

    FProgressObject: TProgress;
    FBusy: Boolean;
    FOwnDC: HDC;
    FLastScreenPos: TPoint;
    FMaterialEntity: TsgDXFEntity;
    FMeshEntity: TsgDXFEntity;
    FSelectByFace: Boolean;
    FExplodeProps: TsgExplodeProps;

{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
    FPolygons: TfrmPolygons;
    FStructure: TfrmStructure;
    FSelectionForPolygons: TsgModObjList;
    FFileList: TStringList;
    FStructureFile: TfmFileStructure;
{$ENDIF}

{$IFDEF SG_ROTATE_CENTER_TEST}
    FIsRequestedDrawMatrix: Boolean;
    FDrawMatrixReload: TFMatrix;
{$ENDIF}

    FGreyShader: TGLShader;
    function ExtractArrayFromList(AAnyList: TObject; var A: Pointer): Integer;
    function AddVertices(const APoints: TFPointList): Integer;
    function AddVertex(const Point: TFPoint; PNormal: PFPoint = nil): Integer;
//    procedure AddVertexes(const P1, P2, P3: TFPoint);
    procedure UpdateVertexIndexList(const AVertexIndexList: TsgIntegerList);
//    procedure AddPoint(Pt: TFPoint; PointsList: TList);
    procedure AssignMaterialForMesh(const Mesh: TMeshObject;
      const AMaterial: TColor);
    procedure AppShowingStyle;
    procedure ApplyShowingStyleToMaterial(AMaterial: TGLMaterial);
    procedure AddMaterialsDimension;
    procedure AddBoxDimensions(var ABoxLine: TsgGLLines;
  const ABox: PFRect; const AVisible: Boolean;
  const IsGetPoint: Boolean = True
  {$IFDEF SG_OBB}; const AOBB: POBB = nil{$ENDIF});
    procedure UpdateBoxDimensions(const ANode: TCustomNode);
    function CheckCurVertexes: Boolean;
    function CreateMeshObject(AOwner: TMeshObjectList; AEntity: TObject;
      AMode: {$IFDEF GLSCENE13}TGLMeshObjectMode{$ELSE}TMeshObjectMode{$ENDIF} = momFaceGroups): TMeshObject;
    procedure CreateGLSceneLine(var ALine: TsgGLPolyline; AColor: TColor;
      ALineWidth: Single; const ATypeLine: TsgTypeLine = tlMass);
    procedure CreateGLScenePlane(var APlane: TGLPlane);
    function CheckCount(ALine: TsgGLPolyline; Count: Integer): Boolean;
    procedure DoOnWrapUpRender(Sender: TObject; var rci: TRenderContextInfo);
    function IsSelectByBody(const AShift: TShiftState): Boolean;

    procedure DrawModEdge(const AEdge: TsgModTopoEdge; var AParams: TsgBrepModIterateParams);
    procedure DrawModMeshVertexForward(const AVertex: TsgModMeshSurfaceVertex;
      const AParams: TsgBrepModIterateParams; var ASumNormals: TFPoint);
    procedure DrawModMeshVertexReversed(const AVertex: TsgModMeshSurfaceVertex;
      const AParams: TsgBrepModIterateParams; var ASumNormals: TFPoint);
    procedure DrawModFace(const AFace: TsgModTopoFace; var AParams: TsgBrepModIterateParams);
    procedure DrawModEntity(const AEnt: TsgModEntity;  var AParams: TsgBrepModIterateParams);
{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
    procedure InitPolygons;
    procedure InitStructure;
    procedure InitFileStructure;
{$ENDIF}
    procedure DrawACIS(Entity: TsgDXFEntity);
    procedure DrawLine(Entity: TsgDXFEntity);
    procedure DrawLineAndZTick(Line: TsgDXFLine; P1, P2: TFPoint);
    procedure DrawPolyLineExt(Points: TFPointList; Solid, Closed: Boolean);
    procedure DrawPolyLineExtByTextArray(const Points: array of TFPoint; Solid, Closed: Boolean);
    procedure DrawPolyLine(Entity: TsgDXFEntity);
    procedure DrawBaseSolid(Entity: TsgDXFEntity);
    procedure DrawSolid(Entity: TsgDXFEntity);
    procedure DrawSolidAsPolygon(Entity: TsgDXFEntity);
    procedure Draw3dFace(Entity: TsgDXFEntity);
    procedure DrawFlatPoly3D(Entity: TsgDXFEntity);
    procedure DrawText(Entity: TsgDXFEntity);
    procedure DrawHatch(Entity: TsgDXFEntity);
    procedure DrawImageEnt(Entity: TsgDXFEntity);
    procedure DrawImage(Entity: TsgDXFEntity);
    procedure DrawMesh(Entity: TsgDXFEntity; AGLMaterial: TGLLibMaterial);
    procedure GetScreenAABB(var AAABB: TAABB);
    procedure GetBoundingBox(var ABoundingBox: THmgBoundingBox);
    function GetFaceGroup(AMeshObject: TMeshObject; AMode: {$IFDEF GLSCENE13}TGLFaceGroupMeshMode{$ELSE}TFaceGroupMeshMode{$ENDIF};
      const AMaterial: TColor; AFaceGroupClass: TClass): TFGVertexIndexList; overload;
    function GetFaceGroup(AMeshObject: TMeshObject; AMode: {$IFDEF GLSCENE13}TGLFaceGroupMeshMode{$ELSE}TFaceGroupMeshMode{$ENDIF};
      AMaterial: TGLLibMaterial; AFaceGroupClass: TClass): TFGVertexIndexList; overload;
{$IFDEF SG_CALC_NORMALS}
    procedure FacesSmooth(aMeshObj: TMeshObject;
      aWeldDistance: Single=0.0000001; aThreshold: Single=35.0);
{$ENDIF}
    procedure GenNormals(AMesh: TMeshObject);
    function GetNavigator: Tsg3DNavigator;
    function GetPoint(const Point: TFPoint): TFPoint;
    function GetPointf(const APoint: TFPoint): TAffineVector;
    function GetShowingStyle: Tsg3DShowingStyles;
    function GetMeshQuality: TsgMeshQuality;
    procedure Navigator3DAfterRender(Sender: TObject);
    procedure Navigator3DBeforeRender(Sender: TObject);
    procedure PrepareData(ALine: TsgGLPolyline; Points: TFPointList;
      SolidPolyline: Boolean; ClosedPolyline: Boolean);
    procedure SetShowingStyle(const Value: Tsg3DShowingStyles);
    procedure SetupLight(const ABox: TFRect);
    procedure ScenePositionChange(Sender: TObject);
    procedure SetMeshQuality(const Value: TsgMeshQuality);
    procedure SetDefault3DSceneOptions;
    procedure SetHighLightObject(AHighLightObject: TObject);
    procedure SetSnapObject(ASnapObject: TObject);
    procedure SetPickObject(APickObject: TObject);
    procedure SetShowHighlight(const Value: Boolean);
    procedure SetSnap(const Value: Boolean);
    procedure SetMouseRotate(const Value: Boolean);
    //
    procedure FinalMeshObject(var AMesh: TMeshObject; ANeedNormals: Boolean;
      const IsDeferredDeletion: Boolean = False);
    procedure Load3dFaceMeshObject;
    procedure Create3DFaceMeshObject(AEntity: TsgDXFEntity);
    //
    function MeasureDimensionEsc: Boolean;

    function CreateDimension: TsgPickObject;
    function GetDimension(const AIndex: Integer = -1): TsgPickObject;
    function GetDimensionIndex(const AObject: TsgPickObject): Integer;

    procedure ResetSelectObjects;

    function GetPickObject: TObject;
    procedure PickObjectEvent(Sender: TObject);
    procedure HighLightObjectEvent(Sender: TObject);
    procedure SnapObjEvent(Sender: TObject);
    procedure SnapEdgeObjEvent(Sender: TObject);
    procedure SnapDimObjEvent(Sender: TObject);
    procedure DimensionObjEvent(Sender: TObject);
    procedure ResetDefaults;
    function GetHighLightObject: TObject;
    function GetSnapObject: TObject;
    procedure DoNodes;
    function FindMesh(X, Y: Integer): TMeshObject;
    function FindDim(X, Y: Integer): TObject;
    function FindSnap(X, Y: Integer; var APoint, ANormal: TFPoint): TObject;
    function FindSnapEdge(X, Y: Integer): TObject;
    function GetShowHighlight: Boolean;
    function GetSnap: Boolean;
    procedure SetLoadFromImage(const Value: TsgCADImage);
    procedure UpdateMaterialLibrary;
    function GetSnapEdge: Boolean;
    procedure SetSnapEdge(const Value: Boolean);
    procedure SetOnPickObject(const Value: TNotifyEvent);
    procedure SetMeasureEvent(const Value: TNotifyEvent);
    procedure SetMeasureDeleteEvent(const Value: TNotifyEvent);
    procedure SetMoveDim(const Value: TsgPickObject);
    function CreateGLPolyline: TsgGLPolyline;
    procedure DeleteGLPolyline(AObject: TObject);
    function GetCADCoords(const APoint: TPoint): TFPoint; overload;
    function GetCADCoords(const APoint: TFPoint): TFPoint; overload;
    function GetScreenCoords(const APoint: TFPoint): TFPoint; overload;
    function GetScreenCoords(const APoint: TAffineVector): TFPoint; overload;
    function GetEnablePickObjects: Boolean;
    procedure SetEnablePickObjects(const Value: Boolean);
    procedure SetUserHighLight(const Value: Boolean);
    procedure SetMeasureMode3D(const Value: TsgMeasureMode3d);
    procedure SetProgressObject(const AValue: TProgress);
    function GetShowEdges: Boolean;
    procedure SetShowEdges(const Value: Boolean);
    function GetShowBoxDimensions: Boolean;
    procedure SetShowBoxDimensions(const Value: Boolean);
    function  VisibleScene: Boolean;
{$IFDEF USE_CLIP}
    function GetShowSnapPlan: Boolean;
    procedure SetShowSnapPlan(const Value: Boolean);
    procedure SnapPlanEvent(Sender: TObject);
    function GetSnapPlanNormal: TFPoint;
    function GetSnapPlanPoint: TFPoint;
    procedure SetSnapPlanPointsCount(const Value: Integer);
    function GetCrossSectionOn: Boolean;
    procedure SetCrossSectionOn(const Value: Boolean);
    function GetSceneCenter: TAffineVector;
    function GetSceneRadius: Single;
    procedure SetCrossSectChangeEvent(const Value: TNotifyEvent);
    function GetCuttingMode: TCuttingMode;
    procedure SetCuttingMode(const Value: TCuttingMode);
{$ENDIF}
    function GetAntiAliasing: Integer;
    procedure SetAntiAliasing(const Value: Integer);
    procedure DoMeasureEvent(AMeasureObject: TObject);
    function RayCastPlaneIntersect(const rayStart, rayVector : TFPoint;
      const planePoint, planeNormal : TFPoint;
      intersectPoint : PFPoint = nil) : Boolean;
    procedure SnapObjMouseUp;
    procedure SnapEdgeMouseUp;
{$IFDEF USE_CLIP}
    procedure SetCrossSectionEdgesColor(const Value: TColor);
{$ENDIF}
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    {$IFDEF MSWINDOWS}
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    {$ENDIF}
    procedure DestroyRC;
    function GetModelLayoutBox: TFRect;
    function ColorToSceneColor(AColor: TColor): TColor;
    // Structure
    procedure SetVisualization(const AEntity: TObject;
      const AData: Pointer; const APath: String);
    procedure DropVisualization(const AData: TObject);
    procedure ClearVisualization;
    procedure SetSelectByFace(const Value: Boolean);
{$IFDEF DEBUG_STRUCTURE}
    procedure UpdateRootToTreeView(AParent: TTreeNode; ANodeLine: TCustomNode);
    procedure RootToTreeView(AParent: TTreeNode; ANodeLine: TCustomNode);
{$ENDIF}
    procedure SetExplodeProps(const Value: TsgExplodeProps);
  protected
    procedure DestroyWnd; override;
    procedure Loaded; override;

    function CoordinateConvertion(ACoordX, ACoordY: Integer;
      var APointInUCS: TFPoint): TFPoint; override;
    function CreateSnapShotBitmap: Graphics.TBitmap; override;
    procedure DoOnColorChanged(const AColor: TColor); override;
    procedure DoOnCursorChanged; override;
    procedure DoPictureChange(Sender: TObject); override;
    function FinishEntity(Entity: TsgDXFEntity): Integer; override;
    function GetActualGraphic: TGraphic; override;
    function ClientToWorld(const X, Y, Z: Double): TVector3f; overload;
    function ClientToWorld(X, Y: Double; AUCSPoint: PFPoint = nil): TFPoint; overload; override;
    function ClientToWorld(const APosition: TF2DPoint): TVector3f; overload;
    function ClientToWorld(const APosition: TFPoint): TVector3f; overload;
    function ShowRectConvertUnits(ARect: TFRect;
      const AConvertUnits: Boolean): Boolean; override;
    function CreateProgressObject: TProgress; virtual;
    function GetBox: TFRect; override;
    function GetBoxOffset: TFPoint; override;
    function GetEntity(Entity: TsgDXFEntity): Integer; override;
    function GetFilteredEntity(Entity: TsgDXFEntity): Integer;
    function GetMaterialByName(AMaterialName: string): TGLMaterial;
    function GetOrbit3DClass: TClass; override;
    function GetSizesAsString: string; override;
    procedure GetPolylineText(const AText: string; const APolyline: TFPointList);
    function GetDimensionTextPolyline(const APolyline: TsgGLPolyline): Boolean;
    procedure GetView(var AMatrix: TFMatrix); override;
    function GetHasTriangledMesh: Boolean; override;
    function GetZoomInEnable: Boolean; override;
    function GetZoomOutEnable: Boolean; override;
    function InitializeIterateParams: PsgCADIterate; override;
    procedure LoadView(const AMatrix: TFMatrix;
      const APostMessage: Boolean = True); override;
{$IFDEF CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}
    procedure SaveOutline;
    procedure RestoreOutline;
    procedure ApplyClip;
    procedure AddClip(AClipPlan: Integer; APointPlan, ANormal: TFPoint);
    procedure RemoveClip(AClipPlan: Integer);
{$ENDIF}
{$IFDEF USE_CLIP}
    procedure SetPickPointClipEvent(const Value: TNotifyEvent);
{$ENDIF}

    procedure VisibleChanging; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure OrbitVisibleChanging(Sender: TObject); override;
    function GetMeshLines(AMesh: TMeshObject; AList: TFPointList;
      AMatrix: PFMatrix): Boolean;
    function EntToGLObj(AEnt: TObject; APIndex: PInteger = nil): TObject;
    procedure DeleteEnts(AEntities: TObject);
    function GetNodeMeshes(ANode: TCustomNode;
      const AClasses: array of TClass): TsgObjectList;
    function GetInsertMeshes(AInsert: TObject): TsgObjectList;
    procedure RepaintScene;
    procedure Reload(AEntities: TObject);
    procedure SetSelected2(AEntities: TObject; APickObj: TsgPickObject;
       const ASelectModTopoClass: array of TClass);
    procedure SetSelected(AEntities: TObject);
    procedure StopMouseMoveOperationsHandler(var Message: TMessage); override;
    property IntersectCache: PFPoint read FIntersectCache;
    property PickObjects: TsgPickObject read FPickObjects;
    property MoveDim: TsgPickObject read FMoveDim write SetMoveDim;
    property ProgressObject: TProgress read FProgressObject write SetProgressObject;
    property SelectByFace: Boolean read FSelectByFace write SetSelectByFace;
    property ShowHighlight: Boolean read GetShowHighlight write SetShowHighlight;
    property ShowSnap: Boolean read GetSnap write SetSnap;
    property ShowSnapEdge: Boolean read GetSnapEdge write SetSnapEdge;
{$IFDEF USE_CLIP}
    property ShowSnapPlan: Boolean read GetShowSnapPlan write SetShowSnapPlan;
    property BoxOffset: TFPoint read FBoxOffs;
    property SnapPlanPoint: TFPoint read GetSnapPlanPoint;
    property SnapPlanNormal: TFPoint read GetSnapPlanNormal;
    property SceneCenter: TAffineVector read GetSceneCenter;
    property SceneRadius: Single read GetSceneRadius;
    property SnapPlanPointsCount: Integer read FSnapPlanCount write SetSnapPlanPointsCount;
{$ENDIF}
    function ZoomRectEx(ARect: TFRect; const IsRealBox: Boolean): Boolean;
    function ZoomRectEx2D(const ARect: TF2DRect): Boolean; override;
    procedure ProcModEntity(const AEnt: TsgModEntity; const AData: Pointer);
    procedure OrbitChange(Sender: TObject); override;
{$IFDEF CLIP_CSG_GLMESH_SGMESH}
    procedure UnBoxing(const AType: Integer; const ADelt: Double);
{$ENDIF}
    property Root: TPersistentObjectList read FRoot;
  public
    constructor Create(AOwner: TComponent);{$IFNDEF SG_FIREMONKEY}override;{$ENDIF}
    destructor Destroy; override;
    function AlterScale(Factor: Double; IsAbsolute: Boolean;
      Position: TPoint): Boolean; override;
    function BeginUpdate: Integer; override;
    procedure Clear; override;
{$IFNDEF SGFPC}
    procedure CreateParams(var Params: TCreateParams); override;
{$ENDIF}
    function Empty: Boolean; override;
    function EndUpdate: Integer; override;
    procedure FitToSize; override;
    function GetDrawingCoords(ACoordX, ACoordY: Integer;
      var AUnits: string): TFPoint; override;
    function GetDrawingInternalCoords(ACoordX, ACoordY: Integer): TFPoint; override;
    function GetDrawingUCSCoords(ACoordX, ACoordY: Integer;
      var AUnits: string): TFPoint; override;
    function GetRealPoint(ACoordX, ACoordY: Double;
      var APointInUCS: TFPoint): TFPoint; override;
    function IsCanBeEdited: Boolean; override;
    function IsCanShowCoords: Boolean; override;
    function IsCanShowSizes: Boolean; override;
    function IsUseGraphic: Boolean; override;
    function IsSimple3DBody: Boolean;
    procedure LoadFromConverter(const AConverter: TsgDXFConverter;
      const ACADImage: TsgCADImage); override;
    procedure LoadFromFile(const FileName: string); override;
    function MeasureDimensionDelete: Boolean;
    procedure MeasureDimensionDeleteAll;
    function MeasureDimensionDeleteByObject(const AObject: TsgPickObject): Boolean;
    function MovePictureRect2D(const ADeltaX, ADeltaY: Double): Boolean; override;
    function OpenVPort: Boolean; override;
    function Rotate(const APitch, ATurn, ARoll: Extended): Boolean; overload;{$IFNDEF SG_FIREMONKEY}override;{$ENDIF}
    function Rotate(Axis: TsgAxes; Angle: Extended): Boolean; overload;{$IFNDEF SG_FIREMONKEY}override;{$ENDIF}
    procedure RotToView(const A3DView: TsgDXFViewDirection); override;
    procedure SaveCrossSectionOCT(const AName: String; const AOutputType: Integer);
    procedure Translate(const AVector: TAffineVector);
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateMeasure;
    procedure SaveDimensions(const AFileName: string = '');
    procedure LoadDimensions(const AFileName: string; const AOnCreate: TsgOnDim3dCreateProc = nil;
      ACustom: Boolean = False);
    function DimensionsToXMLNode(const ANode: TsgNode): Integer;
    function DimensionsFromXMLNode(const ANode: TsgNodeSample; const AOnCreate: TsgOnDim3dCreateProc = nil): Integer;
    procedure SaveToStreamGLScene(const AFileName: string; const AStream: TStream; AUseClip: Boolean = False);
{$IFDEF SG_OBJ_TO_DWG}
    procedure SaveToCadImage(const ACADImage: TsgCADImage);
{$ENDIF}
{$IFDEF SG_XKT}
    //XKT
    procedure SaveToXKT(const AFileName: string;
      const AStream: TStream);
{$ENDIF}

    property AccuracyDim: Double read FAccuracyDim write FAccuracyDim;
    property EnablePickObjects: Boolean read GetEnablePickObjects write SetEnablePickObjects;
    property MeshQuality: TsgMeshQuality read GetMeshQuality write
      SetMeshQuality;
    property LoadFromImage: TsgCADImage read FLoadFromImage write SetLoadFromImage;
    property Navigator3D: Tsg3DNavigator read GetNavigator;
    property OuterWireColor: TColor read FOuterWireColor write FOuterWireColor;
{$IFDEF USE_CLIP}
    property CrossSectionEdgesColor: TColor read FCrossSectionEdgesColor write SetCrossSectionEdgesColor;
{$ENDIF}
    property ShowingStyle: Tsg3DShowingStyles read GetShowingStyle write SetShowingStyle;
    property HighLightObject: TObject read GetHighLightObject write SetHighLightObject;
    property SnapObject: TObject read GetSnapObject write SetSnapObject;
    property PickObject: TObject read GetPickObject write SetPickObject;
    property OnMeasureEvent: TNotifyEvent read FOnMeasureEvent write SetMeasureEvent;
    property OnMeasureDeleteEvent: TNotifyEvent write SetMeasureDeleteEvent;
    property OnHighLight: TNotifyEvent read FOnHighLight write FOnHighLight;
    property OnPickObject: TNotifyEvent read FOnPickObject write SetOnPickObject;
    property MeasureMode3D: TsgMeasureMode3d read FMeasureMode3D write SetMeasureMode3D;
    property MouseRotate: Boolean read FMouseRotate write SetMouseRotate;
    property UserHighLight: Boolean read FUserHighLight write SetUserHighLight;
    property IsShowEdges: Boolean read GetShowEdges write SetShowEdges;
    property IsShowBoxDimensions: Boolean read GetShowBoxDimensions
      write SetShowBoxDimensions;
    property ExplodeProps: TsgExplodeProps read FExplodeProps write SetExplodeProps;
{$IFDEF USE_CLIP}
    property CrossSectionOn: Boolean read GetCrossSectionOn write SetCrossSectionOn;
    property OnPickPointClipEvent: TNotifyEvent read FOnPickPointClipEvent write SetPickPointClipEvent;
    property OnCuttingPlanesChangeEvent: TNotifyEvent read FOnCuttingPlanesChangeEvent write SetCrossSectChangeEvent;
    property CuttingMode: TCuttingMode read GetCuttingMode write SetCuttingMode;
{$ENDIF}
    property AntiAliasingMode: Integer read GetAntiAliasing write SetAntiAliasing;
  end;

  TTransformation = class
  private
    FMatrix: PFMatrix;
  public
    constructor Create(AMatrix: PFMatrix);
    function Transformf(const APoint: TFPoint): TAffineVector; {$IFDEF SG_INLINE}inline;{$ENDIF}
    function Transform(const APoint: TFPoint): TFPoint; {$IFDEF SG_INLINE}inline;{$ENDIF}
    property MatrixAsAddress: PFMatrix read FMatrix;
  end;

  TsgVectorGLScene = class(TsgVectorImage3D)
  public
    Navigator: TsgDrawingNavigator;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const FileName: string); override;
  end;

  TsgConverterNav3d = class(TsgConverterObj)
  protected
    function Convert: Integer; override;
  end;

var
  Solid3DFileExts: TStringList = nil;
  CF_IGES, CF_STEP: Word;
  DefBlendingParams: TGLBlendingParameters = nil;

const
  S3dfImportLibraryName = 'converter3df';
  S3dfImportLibraryExt = 'dll';
  S3dfImportLibrary = S3dfImportLibraryName + '.' + S3dfImportLibraryExt;
  cntInstImprtLibPath = 'http://www.cadsofttools.com/download/' +
    S3dfImportLibraryName + {$IFDEF SG_CPUX64}'x64' + {$ENDIF}  '.zip'; //needs to be right!
  cnstSTLFileExt = 'stl';
  cnstSTLFileExtWithDot = '*.stl';
  cnstSTLFileDesc = 'Stereolithography files';
  cnstSTLFileDescWithExt = cnstSTLFileDesc + ' (' + cnstSTLFileExtWithDot + ')';
  cnstSTLFilterDesc = cnstSTLFileDescWithExt + '|' + cnstSTLFileExtWithDot + '|';
  cnstSTLGLVectorFileClassName = 'TGLSTLVectorFile';

  cnstEdgeWeight3DDef = 1;
  bShowEdgesAfterLoad: Boolean = True;
  bShowBoxDimensions: Boolean = False;
  cnstEdgeWeight3D: Double = cnstEdgeWeight3DDef;

  cnstScale = 1;
  cnstAccuracyDim = 1E-03;//1E-05;
  cnstAccuracyDimSTL = 1;

type
  TsgOpenGLVersionsInfo = record
    Ver_1_0: Boolean; // always true if initialize
    Ver_1_1: Boolean;
    Ver_2_0: Boolean;
    Ver_2_1: Boolean;
    Ver_3_0: Boolean;
    Ver_4_0: Boolean;
  end;

const

  cnstSgGlVersionInfo: TsgOpenGLVersionsInfo = (
    ver_1_0: False;
    Ver_1_1: False;
    Ver_2_0: False;
    Ver_2_1: False;
    Ver_3_0: False;
    Ver_4_0: False;
  );


{$IFDEF SG_SHARED_VCL}
  {$IFDEF SGFPC}
    cnstDim3DVCL: TsgDimStyle = ({%H-});
  {$ELSE}
    cnstDim3DVCL: TsgDimStyle = ();
  {$ENDIF}
  cnstDim3DVCLName = 'DimStyleByDrawing3DVCL';
{$ENDIF}


procedure CreateCamera(AOwner, ATargetObject: TGLBaseSceneObject;
  var ACamera: TGLCamera; AScale: Single);
procedure CreateLight(AOwner: TGLBaseSceneObject; var ALight: TGLLightSource);
function GetACISIndexOf(const VectorList: TAffineVectorList;
  const V: TAffineVector): Integer;
function Is3DVectorFile(const AFileName: string): Boolean;
function IsACISEqual(const A, B: Double): Boolean;
function IsACISVectorEquals(const V1, V2: TAffineVector): Boolean;
function IsPolyClockWise(const PolyVerts: TAffineVectorList;
  const PolyIndices: TIntegerList = nil): Boolean;
function IsTriClockWise(const V0, V1, V2: TAffineVector): Boolean;
procedure LoadViewFrom3DNavigator(const ACADImage: TsgCADImage;
  const ANavigator: Tsg3DDrawingNavigator);
procedure LoadViewFromCADImage(const ACADImage: TsgCADImage;
  const ANavigator: Tsg3DDrawingNavigator);
procedure SetDefDirForCamera(ACamera: TGLBaseSceneObject);
procedure SetDefDirForObject(AObject: TGLBaseSceneObject);
procedure SetDefDistanceForCamera(const GLCamera: TGLCamera; BoundingSphereRadius,
  SceneScale: Single; TargetEmpty: Boolean = False);
procedure SetColorForMaterial(Material: TGLMaterial; const Color: TColorVector);
procedure GLMatrixToMatrix(const AGLMat: TMatrix4f; var AMatrix: TFMatrix);
procedure MatrixToGLMatrix(const AMatrix: TFMatrix; var AGLMat: TMatrix4f);
function InitializeSgGLVersion(AGLContext: TGLContext): Boolean;

procedure BlendingParametersToMaterial(ASrc: TGLBlendingParameters; ADstMaterial: TGLMaterial);
function GetGLVersionInfo: TsgOpenGLVersionsInfo;

implementation

uses
{$IFDEF CS_USEFORM}
  Forms,
{$ENDIF}
{$IFDEF SG_ALL_3D_FILES}
  GLFileVRML,
{$ENDIF}
{$IFDEF GLSCENE13}GLApplicationFileIO{$ELSE}ApplicationFileIO{$ENDIF}, GLViewer;

const
//  cnstHighlightColor = $00000000 or clYellow;//$1A000000 or clYellow;
//  cnstPickColor = clAqua;
  Lock: TCriticalSection = nil;
  cnstColorAspect = 1.0 / 255.0;
  cnstLoadDataBaseProgressPart = 0.4;

  cnstAlphaPref: string = 'sgAlpha';
  cnstXMLUserStringData = 'UserString';


function GetGLVersionInfo: TsgOpenGLVersionsInfo;
var
  vContext: TGLContext;
begin
  if not cnstSgGlVersionInfo.Ver_1_0 then
  begin
    try
      vContext := GLContextManager.CreateContext(nil);
      try
        if Assigned(vContext) then
        begin
          vContext.CreateMemoryContext(0, 16, 16);
          vContext.Activate;
          try
            InitializeSgGLVersion(vContext);
          finally
            vContext.Deactivate;
          end;
        end;
      finally
        vContext.Free;
      end;
    except
    end;
  end;
  Result := cnstSgGlVersionInfo;
end;

type
  TsgDrawRectAccess = class(TsgDrawRect);
  TsgBaseListAccess = class(TsgBaseList);

  PRGBA = ^TRGBA;
  TRGBA = record
    Red, Green, Blue, Alpha: Byte;
  end;

  TMouseButton = {$IFDEF SG_FIREMONKEY}System.UITypes{$ELSE}Controls{$ENDIF}.TMouseButton;

  TsgVertexList = class(TFGVertexIndexList)
  private
    FMode1: Cardinal;
    FMode2: Cardinal;
  public
    constructor Create; override;
    procedure BuildList(var mrci: TRenderContextInfo); override;
  end;

  TsgTeselledVertexList = class(TFGVertexIndexList)
  public
    procedure BuildList(var mrci: TRenderContextInfo); override;
  end;

  TsgHiddenLineShader = class(TGLShader)
  private
    FBackgroundColor: TColorVector;
    FPassCount: Integer;
  public
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
  end;

  TsgOutlineStencilShader = class(TGLShader)
  private
    FPassCount: integer;
    FLineColor: TGLColor;
    FBackGroundColor: TGLColor;
    FOutlineWidth: Single;
    procedure SetOutlineWidth(AWidth: single);
  protected
    procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LineColor: TGLColor read FLineColor write FLineColor;
    property BackGroundColor: TGLColor read FBackGroundColor write FBackGroundColor;
    property LineWidth: Single read FOutlineWidth write SetOutlineWidth;
  end;

  TsgGreyShader = class(TGLShader)
  private
    FGLLibMaterial: TGLLibMaterial;
    FColor: TColorVector;
    FPassCount: Integer;
    procedure ApplyColors(const AColor: TColorVector);
    procedure QSetColor(AGLColor: TGLColor; const AColor: TColorVector);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
  end;

  TCustomIterator = class
  private
    FMatrix: TFMatrix;
    FParams: TsgCADIterate;
  protected
    function GetParams: PsgCADIterate; virtual;
    function GetEnt(Entity: TsgDXFEntity): Integer; virtual;
    property Matrix: TFMatrix read FMatrix write FMatrix;
    property Params: PsgCADIterate read GetParams;
  public
    constructor Create; virtual;
    procedure Iterate(AConverter: TsgDXFConverter); virtual;
  end;

  TCountIterator = class(TCustomIterator)
  private
    FCount: Integer;
  protected
    function GetEnt(Entity: TsgDXFEntity): Integer; override;
    property Count: Integer read FCount;
  end;

  TCustomFindEnt = class(TCustomIterator)
  private
    FObj: TObject;
    function GetObject: TObject;
    procedure SetObject(const Value: TObject);
  protected
    function GetEnt(Entity: TsgDXFEntity): Integer; override;
    property Obj: TObject read GetObject write SetObject;
  end;

  TsgGLPickLines = class(TsgGLLines)
  private
    FPickObjects: TsgPickObject;
  protected
    property PickObjects: TsgPickObject read FPickObjects write FPickObjects;
  public
    destructor Destroy; override;
    procedure BuildList(var rci: TRenderContextInfo); override;
  end;

  TNodeLite = class(TCustomNode)
  protected
    function AddChild(AObj: TObject): TNodeLite;
  public
    destructor Destroy; override;
//    function GetBox: TFRect; override;
    function GetNodeByStrKey(AKey: String): TNodeLite;
    function GetNodeByKey(AKey: TsgObjectList;  AStart: Integer): TNodeLite;
    function Find(AObj: TObject; var I: Integer): Boolean;
    function FindDeep(AObj: TObject; var AOwner: TNodeLite; var I: Integer;
      const ALevel: Integer = MaxInt): Boolean;
    procedure Clean; override;
    procedure CleanEx(const AFreeList: TList; const ARecurse: TList;
      var ADepth: Integer);
  end;

  TNodeGroup = class(TNodeLite)
  private
    FGroup: TObject;
  public
    property Group: TObject read FGroup write FGroup;
  end;

  TNodeMatrix = class(TNodeLite)
  private
    FMatrix: TFMatrix;
  protected
    procedure GetMatrix(out AMatrix: TFMatrix); override;
  end;

  TRoot = class(TNodeMatrix)
  private
    FNodeList: TsgPointerList;
    FTypeExploded: Integer;
    F3DDrawingNavigator: Tsg3DDrawingNavigator;
    function SortByExploded(const A, B: Pointer): Integer;
    function AddNodeToList(const ANode: TCustomNode): Integer;
  protected
    function CreateListByExploded(const AType: Integer;
      const ANewList: Boolean = False): TsgPointerList;
    procedure FreeListByExploded;
    function GetBoxByExploded: TFRect;
  public
    class procedure SortNodes(const ANode: TPersistentObjectList);
    procedure Clean; override;
  end;

  TEntList = class(TList)
  private
    function GetEntities(Index: Integer): TsgDXFEntity;{$IFDEF SG_INLINE} inline;{$ENDIF}
    procedure SetEntities(Index: Integer; const Value: TsgDXFEntity);
    function GetBox(Index: Integer): TAABB;{$IFDEF SG_INLINE} inline;{$ENDIF}
  public
    property Entities[Index: Integer]: TsgDXFEntity read GetEntities write SetEntities; default;
    property Box[Index: Integer]: TAABB read GetBox;
  end;

  TTesIndexList = class(TFGVertexIndexList)
  public
    procedure BuildList(var mrci: TRenderContextInfo); override;
  end;

  TsgInsertStack = class(TsgList)
  public
    function GetPath: String;
    function Pop: TObject;
    procedure Push(const AObject: TObject);
  end;

  TsgMinBox = class
  public
    class function BoxPointSqrDistanceTo(const ANode: TCustomNode;
      const APt: TFPoint): Single; overload;
    class function BoxPointSqrDistanceTo(const AMesh: TMeshObject;
      const APt: TVector3f): Single; overload;
    class function BoxPointSqrDistanceTo(AMin, AMax: TFPoint;
      const APt: TFPoint): Single; overload;
    class function BoxPointSqrDistanceTo(AMin, AMax: TVector3f;
      const APt: TVector3f): Single; overload;
  end;

  //TsgDXFVertexList = class (TFGVertexNormalTexIndexList)
  TGLBaseMeshAccess = class(TGLBaseMesh);
  TGLBaseSceneObjectAccess = class(TGLBaseSceneObject);
  TsgCADBasePolyLineAccess = class(TsgCADBasePolyline);
  TsgDXFPolylineAccess = class(TsgDXFPolyline);
  TGLSceneBufferAccess = class(TGLSceneBuffer);
  TsgDXFConverterAccess = class(TsgDXFConverter);
  TsgCADImageAccess = class(TsgCADImage);
  TsgDXFEntityAccess = class(TsgDXFEntity);
  TsgCADCurvePolygonAccess = class(TsgCADCurvePolygon);
  TsgDXFInsertAccess = class(TsgDXFInsert);
  TGLFreeFormAccess = class(TGLFreeForm);
  TsgDXFSolidAccess = class(TsgDXFSolid);
  TsgDXFDimensionAccess = class(TsgDXFDimension);
  TsgDXFDimensionStyleAccess = class(TsgDXFDimensionStyle);
  TsgDXFTextAccess = class(TsgDXFText);
  TsgDXFBlockAccess = class(TsgDXFBlock);
  TsgDXFLayoutAccess = class(TsgDXFLayout);
  Tsg3DNavigatorAccess = class(Tsg3DNavigator);
  TsgModEntityAccess = class(TsgModEntity);
  TsgDXF3DFaceAccess = class(TsgDXF3DFace);

type
  TFaceGroupClass = class of {$IFDEF GLSCENE13}TGLFaceGroup{$ELSE}TFaceGroup{$ENDIF};
  Tsg3DCube = array[0..7] of TFPoint;

const
  cntZeroZ = 0.0001;
  cntDefCameraPos: TVector3f = (X:7; Y:5; Z:-4);
  //cntDefCameraPos: TVector3f = (-100, 0, cntZeroZ);
  cntDefAxisHeight = 0.5;
  cntDefAxisPos: TVector3f = (X:0; Y:0; Z:0){(1.5, 0, -1.5)};
  cntAxisImageWidth = 128;
  cntAxisImageHeight = 128;
  cntAxisPosMinY = cntAxisImageHeight + 16;
  cntDefPerspCameraScale = 12;
  cntDefOrthCameraScale = 1;
  cntDefPerspAxisCameraScale = 1;
  cnstLineWidthShader = 1;

  cntMMPerInch = 25.4;
  cntGLGraphicResolution = 150;
  cntGLGraphicMaxWidth = 600 / cntMMPerInch * cntGLGraphicResolution;
  cntGLGraphicMaxHeigth = 420 / cntMMPerInch * cntGLGraphicResolution;

  LtAmbient: TColorVector = (X:0.01; Y:0.01; Z:0.01; W:1);
  LtDiffuse: TColorVector = (X:0.26; Y:0.26; Z:0.26; W:1);
  LtSpecular: TColorVector = (X:0.4; Y:0.4; Z:0.4; W:1);


function InitializeSgGLVersion(AGLContext: TGLContext): Boolean;
begin
  Result := cnstSgGlVersionInfo.Ver_1_0;
  if Result then
    Exit;
  if not Assigned(AGLContext) then
    Exit;
  if AGLContext.GL <> nil then
  begin
    cnstSgGlVersionInfo.Ver_1_0 := AGLContext.GL.VERSION_1_0;
    Result := cnstSgGlVersionInfo.Ver_1_0;
    if Result then
    begin
      cnstSgGlVersionInfo.Ver_1_1 := AGLContext.GL.VERSION_1_1;
      cnstSgGlVersionInfo.Ver_2_0 := AGLContext.GL.VERSION_2_0;
      cnstSgGlVersionInfo.Ver_2_1 := AGLContext.GL.VERSION_2_1;
      cnstSgGlVersionInfo.Ver_3_0 := AGLContext.GL.VERSION_3_0;
      cnstSgGlVersionInfo.Ver_4_0 := AGLContext.GL.VERSION_4_0;
    end;
  end;
end;

function GetIllumination(const cl: TColorVector): Single; overload;
begin
  Result := (cl.X * 0.2126) + (cl.Y * 0.7152) + (cl.Z * 0.0722);
end;

function GetIllumination(const cl: TGLColor): Single; overload;
begin
  Result := GetIllumination(cl.Color);
end;

function ColorVectorToIllumination(const cl: TColorVector): TColorVector;
var
  vIllum: Single;
begin
  vIllum := GetIllumination(cl);
  Result := VectorMake(vIllum, vIllum, vIllum, cl.W);
end;

procedure DisableClipPlane(A3DNav: Tsg3DNavigator);
{$IFDEF CLIP_GLCLIP_GLMESH}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF CLIP_GLCLIP_GLMESH}
   for I := GL_CLIP_PLANE0 to GL_CLIP_PLANE5 do
   begin
     if A3DNav.FClipUse[I] = True then
       GL.Disable(I);
   end;
{$ENDIF}
end;

procedure EnableClipPlane(A3DNav: Tsg3DNavigator);
{$IFDEF CLIP_GLCLIP_GLMESH}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF CLIP_GLCLIP_GLMESH}
   for I := GL_CLIP_PLANE0 to GL_CLIP_PLANE5 do
   begin
     if A3DNav.FClipUse[I] = True then
       GL.Enable(I);
   end;
{$ENDIF}
end;


// cube
//    v6----- v5
//   /|      /|
//  v1------v0|
//  | |     | |
//  | |v7---|-|v4
//  |/      |/
//  v2------v3
//  v7 = min  v0 = max
procedure AddCubeByAABB(AABB: TAABB; vTriangles: TAffineVectorList);
const
  cnstCubeIndices : array[0..35] of Integer =
    (0,1,2, 2,3,0,
     0,3,4, 4,5,0,
     0,5,6, 6,1,0,
     1,6,7, 7,2,1,
     7,4,3, 3,2,7,
     4,7,6, 6,5,4);
var
  vVertices : TAffineVectorList;
  I: Integer;
begin
  vVertices := TAffineVectorList.Create;
  try
    vVertices.Add(AABB.Max); //V0
    vVertices.Add(AABB.Min.X,AABB.Max.Y,AABB.Max.Z); //V1
    vVertices.Add(AABB.Min.X,AABB.Min.Y,AABB.Max.Z); //V2
    vVertices.Add(AABB.Max.X,AABB.Min.Y,AABB.Max.Z); //V3
    vVertices.Add(AABB.Max.X,AABB.Min.Y,AABB.Min.Z); //V4
    vVertices.Add(AABB.Max.X,AABB.Max.Y,AABB.Min.Z); //V5
    vVertices.Add(AABB.Min.X,AABB.Max.Y,AABB.Min.Z); //V6
    vVertices.Add(AABB.Min); //V7
    vTriangles.Clear;
    for I := Low(cnstCubeIndices) to High(cnstCubeIndices) do
    begin
      vTriangles.Add(vVertices.List[cnstCubeIndices[I]]);
    end;
  finally
    vVertices.Free;
  end;
end;

// cube
//    v6----- v5
//   /|      /|
//  v1------v0|
//  | |     | |
//  | |v7---|-|v4
//  |/      |/
//  v2------v3
//  v7 = min  v0 = max
procedure AddCubeByAABB2(AABB: TAABB; vTriangles: TAffineVectorList);
begin
  vTriangles.Clear;
  vTriangles.Add(AABB.Max);
  vTriangles.Add(FPoint2Vect(MakeFPoint(0,0,1)));
  vTriangles.Add(AABB.Max);
  vTriangles.Add(FPoint2Vect(MakeFPoint(1,0,0)));

  vTriangles.Add(AABB.Min);
  vTriangles.Add(FPoint2Vect(MakeFPoint(0,0,-1)));
  vTriangles.Add(AABB.Min);
  vTriangles.Add(FPoint2Vect(MakeFPoint(-1,0,0)));

  vTriangles.Add(AABB.Max);
  vTriangles.Add(FPoint2Vect(MakeFPoint(0,1,0)));
  vTriangles.Add(AABB.Min);
  vTriangles.Add(FPoint2Vect(MakeFPoint(0,-1,0)));
end;

procedure MakeCube(const ARect: TFRect; var V: Tsg3DCube);
begin
  V[0] := MakeFPoint(ARect.Right, ARect.Top, ARect.Z1);
  V[1] := MakeFPoint(ARect.Left, ARect.Top, ARect.Z1);
  V[2] := MakeFPoint(ARect.Left, ARect.Bottom, ARect.Z1);
  V[3] := MakeFPoint(ARect.Right, ARect.Bottom, ARect.Z1);
  V[4] := MakeFPoint(ARect.Right, ARect.Bottom, ARect.Z2);
  V[5] := MakeFPoint(ARect.Right, ARect.Top, ARect.Z2);
  V[6] := MakeFPoint(ARect.Left, ARect.Top, ARect.Z2);
  V[7] := MakeFPoint(ARect.Left, ARect.Bottom, ARect.Z2);
end;

function sgCalcPlaneNormal(const AP1, AP2, AP3: TFPoint): TFPoint;
begin
  Result := sgFunction.sgCalcPlaneNormal(AP1, AP2, AP3);
end;


procedure CreateCamera(AOwner, ATargetObject: TGLBaseSceneObject; var ACamera: TGLCamera;
  AScale: Single);
begin
  ACamera := TGLCamera.CreateAsChild(AOwner);
  ACamera.CameraStyle := csPerspective;
//  ACamera.DepthOfView := 700.0;
//  ACamera.FocalLength := 70.0;
  //ACamera.NearPlaneBias := 0.1;
  SetDefDirForCamera(ACamera);
  ACamera.TargetObject := ATargetObject;
  ACamera.SceneScale := AScale;
//  ACamera.Position.AsAffineVector := cntDefCameraPos;
end;

procedure CreateLight(AOwner: TGLBaseSceneObject; var ALight: TGLLightSource);
begin
  ALight := TGLLightSource.CreateAsChild(AOwner);
  ALight.ConstAttenuation := 1.0;
  ALight.Ambient.Color := LtAmbient;
  ALight.Diffuse.Color := LtDiffuse;
  ALight.Specular.Color := LtSpecular;
  ALight.LightStyle := {lsSpot}lsParallel;
  ALight.SpotCutOff := 180;
  ALight.SpotDirection.SetVector(0, 0, -1);

  //ALight.SpotDirection.SetVector(0.5773503, 0.5773503, 0.5773503);
  //ALight.Position.AsAffineVector :=  cntDefAxisPos;
  ALight.Position.AsAffineVector := VectorScale(cntDefCameraPos, 10);
  //ALight.Position.Assign(AOwner.Position);
end;

procedure SetDefDirForCamera(ACamera: TGLBaseSceneObject);
begin
  ACamera.Direction.SetVector(0, 0, -1);
  ACamera.Up.SetVector(0, 1, 0);
end;

procedure SetDefDirForObject(AObject: TGLBaseSceneObject);
begin
  AObject.Direction.SetVector(0, 0, 1);
  AObject.Up.SetVector(0, 1, 0);
end;

function GetMaterialName(AMaterial: TColor): string;
begin
  Result := IntToHex(AMaterial, 8);
end;

procedure SetMeshMaterial(AMesh: TMeshObject; const AMaterialName: string;
  AMaterialLibrary: TGLAbstractMaterialLibrary);
var
  I: Integer;
begin
  AMesh.DropMaterialLibraryCache;
  for I := 0 to AMesh.FaceGroups.Count - 1 do
    AMesh.FaceGroups[I].MaterialName := AMaterialName;
  if AMaterialLibrary is TGLMaterialLibrary then
    AMesh.PrepareMaterialLibraryCache(TGLMaterialLibrary(AMaterialLibrary));
end;

procedure BlendingParametersToMaterial(ASrc: TGLBlendingParameters; ADstMaterial: TGLMaterial);
var
  Alpha: Single;
begin
  if Assigned(ASrc) and Assigned(ADstMaterial) then
  begin
    ADstMaterial.BeginUpdate;
    try
      ADstMaterial.BlendingParams.UseAlphaFunc := ASrc.UseAlphaFunc;
      ADstMaterial.BlendingParams.UseBlendFunc := ASrc.UseBlendFunc;
      ADstMaterial.BlendingParams.AlphaFunctType := ASrc.AlphaFunctType;
      ADstMaterial.BlendingParams.AlphaFuncRef := ASrc.AlphaFuncRef;
      ADstMaterial.BlendingParams.SeparateBlendFunc := ASrc.SeparateBlendFunc;
      ADstMaterial.BlendingParams.BlendFuncSFactor := ASrc.BlendFuncSFactor;
      ADstMaterial.BlendingParams.BlendFuncDFactor := ASrc.BlendFuncDFactor;
      ADstMaterial.BlendingParams.AlphaBlendFuncSFactor := ASrc.AlphaBlendFuncSFactor;
      ADstMaterial.BlendingParams.AlphaBlendFuncDFactor := ASrc.AlphaBlendFuncDFactor;
      if not ADstMaterial.BlendingParams.UseAlphaFunc then
        Alpha := 1.0 - ADstMaterial.BlendingParams.AlphaFuncRef
      else
        Alpha := 1.0;
      ADstMaterial.FrontProperties.Diffuse.Alpha := Alpha;
      ADstMaterial.FrontProperties.Specular.Alpha := Alpha;
      ADstMaterial.BackProperties.Diffuse.Alpha := Alpha;
      ADstMaterial.BackProperties.Specular.Alpha := Alpha;
    finally
      ADstMaterial.EndUpdate;
    end;
  end;
end;

procedure SetColorForMaterialNB(Material: TGLMaterial; const Color: TColorVector);
const
  cnstShininess: TShininess = 128;
begin
  Material.BlendingMode := bmCustom;
  Material.Texture.MappingSCoordinates.AsVector := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.XHmgVector;
  Material.Texture.MappingTCoordinates.AsVector := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.XHmgVector;

  //Material.FrontProperties.Ambient.Color := Color;
  Material.FrontProperties.Diffuse.Color := Color;
  Material.FrontProperties.Specular.Color := Color;
  //Material.FrontProperties.Emission.Color := Color;
  Material.FrontProperties.Shininess := cnstShininess;
  Material.BackProperties.Assign(Material.FrontProperties);
end;

procedure SetColorForMaterial(Material: TGLMaterial; const Color: TColorVector);
begin
  SetColorForMaterialNB(Material, Color);
  if Assigned(DefBlendingParams) then
    BlendingParametersToMaterial(DefBlendingParams, Material);
end;

function _ColorToRGB(Color: TColor): Longint;
const
  SystemColor = $FF000000;
begin
  if (Color and SystemColor = SystemColor) and (Color and $FFFFFF <= 30) then
    Result := GetSysColor(Color and $000000FF)
  else
    Result := Color;
end;

function ColorToVector(AColor: TColor): TColorVector;
var
  vRGBA: TRGBA;
  vWinColor: Cardinal absolute vRGBA;
begin
  vWinColor := _ColorToRGB(AColor);
  Result.V[0] := vRGBA.Red * cnstColorAspect;
  Result.V[1] := vRGBA.Green * cnstColorAspect;
  Result.V[2] := vRGBA.Blue * cnstColorAspect;
  Result.V[3] := 1.0 - vRGBA.Alpha * cnstColorAspect;
end;

procedure DoGLMaterial(GLMaterial: TGLMaterial; Material: TsgDXFMaterial);
const
  cnstShininess: TShininess = 128;
//var
//  Picture: TPicture;
begin
//  Picture := TPicture.Create;
//  try
//    GLMaterial.BlendingMode := bmCustom;
//    GLMaterial.Texture.MappingSCoordinates.AsVector := XHmgVector;
//    GLMaterial.Texture.MappingTCoordinates.AsVector := XHmgVector;

    //GLMaterial.GetLibMaterial.Name := Material.Name;

    //Picture.LoadFromFile(Material.Diffuse.FileName);
    //GLMaterial.GetActualPrimaryTexture.Image.Assign(Picture.Graphic);

    GLMaterial.FrontProperties.Ambient.Color := ColorToVector(ConvertColorCADToRGB(Material.Ambient.MaterialColor.Color));
    GLMaterial.FrontProperties.Diffuse.Color := ColorToVector(ConvertColorCADToRGB(Material.Diffuse.MaterialColor.Color));
    GLMaterial.FrontProperties.Specular.Color := ColorToVector(ConvertColorCADToRGB(Material.Specular.MaterialColor.Color));


    //GLMaterial.FrontProperties.Emission.Color := Color;
    GLMaterial.FrontProperties.Shininess := cnstShininess;
    GLMaterial.BackProperties.Assign(GLMaterial.FrontProperties);
//  finally
//    Picture.Free;
//  end;
end;


function AssignMaterial(AMaterial: TColor;
  AMaterials: TGLLibMaterials): TGLLibMaterial; overload;
var
  vMatName: string;
begin
  if AMaterial = clBlack then
    AMaterial := clGray;
  Result := nil;
  if Assigned(AMaterials) then
  begin
    vMatName := GetMaterialName(AMaterial);
    Result := AMaterials.GetLibMaterialByName(vMatName);
    if not Assigned(Result) then
    begin
      Result := AMaterials.Add;
      Result.Name := vMatName;
      SetColorForMaterial(Result.Material, ColorToVector(AMaterial));
    end;
  end;
end;

function AssignMaterial(const AMaterial: TColor;
  AGLMaterialLibrary: TGLMaterialLibrary): string; overload;
var
  vMatLibItem: TGLLibMaterial;
begin
  Result := '';
  if Assigned(AGLMaterialLibrary) then
  begin
    vMatLibItem := AssignMaterial(AMaterial, AGLMaterialLibrary.Materials);
    if Assigned(vMatLibItem) then
      Result := vMatLibItem.Name;
  end;
end;

procedure SetDefDistanceForCamera(const GLCamera: TGLCamera; BoundingSphereRadius,
  SceneScale: Single; TargetEmpty: Boolean = False);
begin
  if (GLCamera.TargetObject <> nil) and (not TargetEmpty) then
  begin
    GLCamera.SceneScale := SceneScale;
    if BoundingSphereRadius > 0 then
    begin
      case GLCamera.CameraStyle of
        csPerspective:
          begin
            GLCamera.AdjustDistanceToTarget(BoundingSphereRadius * 0.25);
            GLCamera.DepthOfView := 2 * GLCamera.DistanceToTarget + 2 * BoundingSphereRadius;
          end;
        csOrthogonal:
          begin

          end;
      end;
    end;
  end;
end;

function SceneBufferOrthoScreenToWorld(ABuffer: TGLSceneBuffer;
  AScreenPos: TVector3f; ASceneScale: Single): TAffineVector;
var
  vFocalKoef: Single;
begin
  vFocalKoef := 100 * ABuffer.Camera.NearPlaneBias / (ABuffer.Camera.FocalLength *
    ASceneScale);
  if ABuffer.ViewPort.Width > ABuffer.ViewPort.Height then
    vFocalKoef := vFocalKoef / ABuffer.ViewPort.Width
  else
    vFocalKoef := vFocalKoef / ABuffer.ViewPort.Height;
  SetVector(Result,
    VectorCombine3(
      ABuffer.Camera.AbsolutePosition,
      ABuffer.Camera.AbsoluteUpVectorToTarget,
      ABuffer.Camera.AbsoluteRightVectorToTarget, 1,
    (AScreenPos.V[1] - (ABuffer.ViewPort.Height / 2)) * vFocalKoef,
    (AScreenPos.V[0] - (ABuffer.ViewPort.Width / 2)) * vFocalKoef));
  Result := VectorSubtract(Result, ABuffer.Camera.Position.AsAffineVector);
end;

function MeshEntCompare(Item1, Item2: TObject): Integer;
begin
  Result := Integer(TsgNativeUInt(TsgMeshObject(Item1).Entity) -
    TsgNativeUInt(TsgMeshObject(Item2).Entity));
end;

function MeshInsertCompare(Item1, Item2: TObject): Integer;
begin
  Result := Integer(TsgNativeUInt(TsgMeshObject(Item1).Insert) -
    TsgNativeUInt(TsgMeshObject(Item2).Insert));
end;

function NodesCompare(Item1, Item2: TObject): Integer;
begin
  Result := Integer(TsgNativeUInt(TNodeLite(Item1).Obj) -
    TsgNativeUInt(TNodeLite(Item2).Obj));
end;

function QFind(AList: TPersistentObjectList; ASorted: Boolean;
  ADuplicates: TDuplicates; const ACompare: TObjectListSortCompare;
  const AItem: Pointer; var AIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  with AList do
  begin
    H := Count - 1;
    if ASorted then
    begin
      L := 0;
      while L <= H do
      begin
        I := (L + H) shr 1;
        C := ACompare(List^[I], AItem);
        if C < 0 then
          L := I + 1
        else
        begin
          H := I - 1;
          if C = 0 then
          begin
            Result := True;
            if ADuplicates <> dupAccept then
              L := I;
          end;
        end;
      end;
      AIndex := L;
    end
    else
    begin
      AIndex := 0;
      while (AIndex <= H) and (ACompare(List^[AIndex], AItem) <> 0) do
        Inc(AIndex);
      if AIndex <= H then
        Result := True;
    end;
  end;
end;

function BoxToAABB(const ABox: TFRect): TAABB;
begin
  Result.Min.X := ABox.Left;
  Result.Min.Y := ABox.Bottom;
  Result.Min.Z := ABox.Z1;
  Result.Max.X := ABox.Right;
  Result.Max.Y := ABox.Top;
  Result.Max.Z := ABox.Z2;
end;

function CalcSceneScale(const AGLSize: TAffineVector; ASize: TFPoint): Double; overload;
var
  vLen: Extended;

  function DoCalcSceneScale(const ASceneX, ASceneY, AWidth, AHeight: Double): Double;
  var
    vScaleX, vScaleY, vNewSizeX, vNewSizeY: Double;
  begin
    vScaleX := AWidth / ASceneX;
    vScaleY := AHeight / ASceneY;
    Result := Max(vScaleX, vScaleY);
    vNewSizeX := ASceneX * Result;
    if vNewSizeX > AWidth then
      Result := Result * AWidth / vNewSizeX;
    vNewSizeY := ASceneY * Result;
    if vNewSizeY > AHeight then
      Result := Result * AHeight / vNewSizeY;
  end;

begin
  vLen := NormalizeVector(ASize.V);
  case GetBoxType(ASize, fAccuracy) of
    bxXY, bxXYZ: Result := DoCalcSceneScale(AGLSize.V[0], AGLSize.V[1], vLen * ASize.V[0], vLen * ASize.V[1]);
    bxXZ: Result := DoCalcSceneScale(AGLSize.V[0], AGLSize.V[2], vLen * ASize.V[0], vLen * ASize.V[2]);
    bxYZ: Result := DoCalcSceneScale(AGLSize.V[1], AGLSize.V[2], vLen * ASize.V[1], vLen * ASize.V[2]);
    bxX: Result := vLen * ASize.V[0] / AGLSize.V[0];
    bxY: Result := vLen * ASize.V[1] / AGLSize.V[1];
    bxZ: Result := vLen * ASize.V[2] / AGLSize.V[2];
  else
    Result := 1;
  end;
end;

function CalcSceneScale(const AABB: TAABB; const AExtents: TFRect): Double; overload;
var
  vGLSize: TAffineVector;
  vSize: TFPoint;
begin
  vGLSize.V[0] := AABB.max.V[0] - AABB.min.V[0];
  vGLSize.V[1] := AABB.max.V[1] - AABB.min.V[1];
  vGLSize.V[2] := AABB.max.V[2] - AABB.min.V[2];
  vSize.V[0] := AExtents.V[1].V[0] - AExtents.V[0].V[0];
  vSize.V[1] := AExtents.V[0].V[1] - AExtents.V[1].V[1];
  vSize.V[2] := AExtents.V[1].V[2] - AExtents.V[0].V[2];
  Result:= CalcSceneScale(vGLSize, vSize);
end;

type
  Tsg3DDrwNavOrbit3D = class(TsgOrbit3D)
  protected
    procedure PaintWindow(AContextInfo: sgConsts.THandle); override;
  end;

function GetACISIndexOf(const VectorList: TAffineVectorList;
  const V: TAffineVector): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to VectorList.Count - 1 do
    if IsACISVectorEquals(VectorList[I], V) then
    begin
      Result := I;
      Break;
    end;
end;

function Is3DVectorFile(const AFileName: string): Boolean;
{$IFNDEF SG_NO3DMODELS}
var
  vExt: string;
begin
  vExt := Copy(ExtractFileExt(AFileName), 2, MaxInt);
  if Solid3DFileExts =  nil then
    Result := GetVectorFileFormats.FindExt(vExt) <> nil
  else
    Result := Solid3DFileExts.IndexOf(ExtractFileExt(AFileName)) >= 0;
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

function IsACISEqual(const A, B: Double): Boolean;
begin
  Result := Round(A) = Round(B);
end;

function IsACISVectorEquals(const V1, V2: TAffineVector): Boolean;
begin
  Result := IsACISEqual(V1.V[0], V2.V[0]) and IsACISEqual(V1.V[1], V2.V[1]) and IsACISEqual(V1.V[2], V2.V[2]);
end;

function IsPolyClockWise(const PolyVerts: TAffineVectorList;
  const PolyIndices: TIntegerList = nil): Boolean;
var
  I, J: Integer;
  vDeterminant: Single;
  vMatrix: TAffineMatrix;
  vPolyIndices: TIntegerList;
begin
  vDeterminant := 0;
  vPolyIndices := TIntegerList.Create;
  try
    if PolyIndices =  nil then
      for I := 0 to PolyVerts.Count - 1 do
        vPolyIndices.Add(I)
    else
      vPolyIndices.Assign(PolyIndices);

    for I := 0 to vPolyIndices.Count-1 do
    begin
      for J := 0 to 2 do
        if (I + J) >= vPolyIndices.Count then
          vMatrix.V[J] := PolyVerts[vPolyIndices[I + J - vPolyIndices.Count]]
        else
          vMatrix.V[J] := PolyVerts[vPolyIndices[I + J]];
      vDeterminant := vDeterminant + MatrixDeterminant(vMatrix);
    end;
  finally
    vPolyIndices.Free;
  end;
  Result := vDeterminant < 0;
end;

function IsTriClockWise(const V0, V1, V2: TAffineVector): Boolean;
var
  vMatrix: TAffineMatrix;
begin
  vMatrix.V[0] := V0;
  vMatrix.V[1] := V1;
  vMatrix.V[2] := V2;
  Result := MatrixDeterminant(vMatrix) < 0;
end;

procedure LoadViewFrom3DNavigator(const ACADImage: TsgCADImage;
  const ANavigator: Tsg3DDrawingNavigator);
var
  vMatrix: TFMatrix;
begin
  if (ANavigator <> nil) and (ACADImage <> nil) then
  begin
    ANavigator.GetView(vMatrix);
    ACADImage.SetDrawMatrix(vMatrix);
  end;
end;

procedure LoadViewFromCADImage(const ACADImage: TsgCADImage;
  const ANavigator: Tsg3DDrawingNavigator);
begin
  if (ANavigator <> nil) and (not ANavigator.Empty) and (ACADImage <> nil) then
    ANavigator.LoadView(ACADImage.DrawMatrix);
end;

procedure GLMatrixToMatrix(const AGLMat: TMatrix4f; var AMatrix: TFMatrix);
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 2 do
      AMatrix.M[I, J] := AGLMat.V[I].V[J];
end;

procedure MatrixToGLMatrixFull4f(const AMatrix: TFMatrix; var AGLMat: TMatrix4f);
var
  I, J: Integer;
begin
  FillChar(AGLMat, SizeOf(AGLMat), 0);
  for I := 0 to 3 do
    for J := 0 to 2 do
      AGLMat.V[I].V[J] := AMatrix.M[I, J];
  AGLMat.V[3].V[3] := 1;
end;

procedure MatrixToGLMatrix(const AMatrix: TFMatrix; var AGLMat: TMatrix4f);
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      AGLMat.V[I].V[J] := AMatrix.M[I, J];
  for I := 0 to 2 do
  begin
    AGLMat.V[I].V[3] := 0;
    AGLMat.V[3].V[I] := 0;
  end;
  AGLMat.V[3].V[3] := 1;
end;

{ Tsg3DNavigator }

procedure Tsg3DNavigator.Clear;
var
  I: Integer;
begin
{$IFDEF CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}
{$IFDEF CLIP_SGMESH}
  RestoreOriginalMeshData;
{$ENDIF}
{$ENDIF}
  for I := 0 to FGLFreeForm.MeshObjects.Count - 1 do
    FGLFreeForm.MeshObjects[I].FaceGroups.Clear;
  FGLFreeForm.MeshObjects.Clear;
  for I := FGLFreeForm.Count - 1 downto 1 do
    FGLFreeForm.Children[I].Free;
  FGLFreeForm.Skeleton.Clear;
  FGLMaterialLibrary.Materials.Clear;
  FCurrentLine := nil;
  FLineMass := nil;
  FLineOuterWire := nil;
  FTextMass := nil;
  //Was added to GLDCScene as it should be drawn last
  FreeAndNil(FBoxLine);
  if FDimensionListClear then
    ClearDimension;
end;

procedure Tsg3DNavigator.ClearDimension;
var
  I: Integer;
begin
  if Assigned(FDimensionList) then
  begin
    for I := 0 to FDimensionList.Count - 1 do
    begin
      if Assigned(FOnMeasureDeleteEvent) then
        FOnMeasureDeleteEvent(TsgPickObject(FDimensionList[I]));
      TsgPickObject(FDimensionList[I]).Free;
    end;
    FreeAndNil(FDimensionList);
  end;
end;

function Tsg3DNavigator.ClientToWorld(X, Y: Double; Z: Double = 0): TVector3f;
begin
  if FCameraStyle = csOrthogonal then
    Result := Buffer.OrthoScreenToWorld(Round(X), Round(Buffer.Height - Y))
  else
    Result := Buffer.ScreenToWorld(AffineVectorMake(X, Buffer.Height - Y, Z));
end;

constructor Tsg3DNavigator.Create(AOwner: TComponent);

var
  I: Integer;
  vParent: TGLBaseSceneObject;

  procedure SetDefValues(AValue: Tsg3DNavigator);
  begin
    AValue.Buffer.FaceCulling := False;
    AValue.Buffer.AmbientColor.Color := clrGray20;
    AValue.FieldOfView := 154.3915;
  end;
  procedure SetAllTransparent(AFace: TGLFaceProperties);
  begin
    AFace.Ambient.Color := clrWhite;
    AFace.Diffuse.Color := clrWhite;
    AFace.Emission.Color := clrBlack;
    AFace.Specular.Color := clrWhite;
    AFace.Shininess := 128;
  end;

begin
  F3DDrawingNavigator := Tsg3DDrawingNavigator(AOwner);

  FTempListObjects := TsgObjectList.Create;
  FCurrentLine := nil;
  FLineMass := nil;
  FLineOuterWire := nil;
{$IFDEF USE_CLIP}
  FLockClip := False;
  FMeshContextList := TsgPointerList.Create;
{$ENDIF}
{$IFDEF CLIP_CSG_GLMESH}
  FLineOuterWireOld := nil;
  FLineMassOld := nil;
{$ENDIF}
  FTextMass := nil;
  FBoxLine := nil;
  FIsVisibleDimensionList := True;
  FDimensionListClear := True;
  FScaleByPrint := 1;
  FShowCubeExtents := True;
  FShowAxis := True;
  FDefCameraScale := cntDefPerspCameraScale;
  FCameraStyle := csCustom;


  inherited Create(AOwner);
  FVSync := vsmNoSync;
  Buffer.ViewerBeforeRender := DoBeforeRender;
  FGLScene := TGLScene.Create(Self);
  FGLMaterialLibrary := TGLMaterialLibrary.Create(Self);

  FGLDCScene := TGLDummyCube.CreateAsChild(FGLScene.Objects);
  FGLDCScene.CubeSize := 1;
  FGLDCScene.Position.SetPoint(0, 0, 0);
  SetDefDirForObject(FGLDCScene);
  FGLDCScene.EdgeColor.SetColor(1, 1, 1, 1);
  FGLDCScene.Scale.SetVector(1, 1, 1);
  FGLDefSceneMat := FGLDCScene.Matrix;
  FGLDCScene.Position.OnNotifyChange := ScenePositionChange;

  //FGLDCScene.ShowAxes := True;

  FGLFreeForm := TGLFreeForm(TGLFreeForm.NewInstance);
  TGLFreeFormAccess(FGLFreeForm).FMeshObjects  := TsgMeshObjectList.CreateOwned(FGLFreeForm);
  FGLFreeForm.CreateAsChild(FGLDCScene);
  FGLFreeForm.ObjectsSorting := osRenderBlendedLast;

  FGLFreeForm.AutoCentering := [macCenterX, macCenterY, macCenterZ,
    macUseBarycenter];
  FGLFreeForm.AutoScaling.SetPoint(1, 1, 1);
  SetDefDirForObject(FGLFreeForm);
  FGLFreeForm.MaterialLibrary := FGLMaterialLibrary;
//  FGLFreeForm.Material.BackProperties.Ambient.Color := clrGray80{clrGray20};
//  FGLFreeForm.Material.BackProperties.Diffuse.Color := clrGray80;
//  FGLFreeForm.Material.BackProperties.Emission.Color := clrGray80{clrBlack};
//  FGLFreeForm.Material.BackProperties.Specular.Color := clrBlack;
//  FGLFreeForm.Material.FrontProperties.Assign(FGLFreeForm.Material.BackProperties);
  FGLFreeForm.Scale.SetVector(1, 1, 1);
  FGLFreeForm.IgnoreMissingTextures := True;

  FGLCubeExtents := TGLCube.CreateAsChild(FGLFreeForm);
  SetDefDirForObject(FGLCubeExtents);
  FGLCubeExtents.Scale.SetVector(1, 1, 1);
  FGLCubeExtents.Material.BackProperties.Ambient.Color := clrBlack;
  FGLCubeExtents.Material.BackProperties.Diffuse.SetColor(0.3, 0.3, 0.3, 0);
  FGLCubeExtents.Material.BackProperties.Emission.Color := FGLCubeExtents.Material.BackProperties.Diffuse.Color;
  FGLCubeExtents.Material.BackProperties.Specular.Color := FGLCubeExtents.Material.BackProperties.Diffuse.Color;
  FGLCubeExtents.Material.PolygonMode := pmLines;
  FGLCubeExtents.Material.FaceCulling := fcNoCull;
  FGLCubeExtents.Material.FrontProperties.Assign(FGLCubeExtents.Material.BackProperties);
  FGLCubeExtents.Material.MaterialOptions := [moNoLighting];
  FGLCubeExtents.CubeWidth := 2;
  FGLCubeExtents.CubeHeight := 2;
  FGLCubeExtents.CubeDepth := 2;
  FGLCubeExtents.Visible := FShowCubeExtents;

  FGLDCTarget := TGLDummyCube.CreateAsChild(FGLScene.Objects);
  SetDefDirForObject(FGLDCTarget);

  //camera
  CreateCamera(FGLDCTarget, FGLDCTarget, FGLCamera, FDefCameraScale);
  Buffer.Camera := FGLCamera;
//  FGLCamera.NearPlaneBias := 0.01;

  //axes arrows
  FAxes := Tsg3DAxis.CreateEx(FGLScene, Self);
  FAxes.Width := cntAxisImageWidth;
  FAxes.Height := cntAxisImageHeight;
  FAxes.AfterRender := DoOnAfter3DAxisRender;
  FAxes.BeforeRender := DoOnBefore3DAxisRender;

  vParent := FGLScene.Objects;
  //light
  for I := 0 to LightsCount - 1 do
    CreateLight(vParent, FLights[I]);

{$IFDEF CLIP_GLCLIP}
  FGLAxisImage := TsgGLHUDSprite.CreateAsChild(FGLScene.Objects);
{$ELSE}
  FGLAxisImage := TGLHUDSprite.CreateAsChild(FGLScene.Objects);
{$ENDIF}
  FGLAxisImage.Width := cntAxisImageWidth;
  FGLAxisImage.Height := cntAxisImageHeight;

  SetDefDirForObject(FGLAxisImage);

  FGLAxisImage.Material.Texture.ImageClassName := TGLBlankImage.ClassName;
  TGLBlankImage(FGLAxisImage.Material.Texture.Image).Width := cntAxisImageWidth;
  TGLBlankImage(FGLAxisImage.Material.Texture.Image).Height := cntAxisImageHeight;

//  TGLBlankImage(FGLAxisImage.Material.Texture.Image).ColorFormat := 6408;
  FGLAxisImage.Material.Texture.MagFilter := maNearest;
  FGLAxisImage.Material.Texture.MinFilter := miNearest;
  FGLAxisImage.Material.Texture.TextureMode := tmModulate;
  FGLAxisImage.Material.Texture.Compression := tcNone;
  FGLAxisImage.Material.Texture.Disabled := False;

  SetAllTransparent(FGLAxisImage.Material.BackProperties);
  SetAllTransparent(FGLAxisImage.Material.FrontProperties);

  //FGLAxisImage.Material.Texture.Border := 0;
  FGLAxisImage.Material.Texture.Disabled := False;
  FGLAxisImage.Material.BlendingMode := bmTransparency;
//  FGLAxisImage.Material.Texture.ImageAlpha := tiaTopLeftPointColorTransparent;
  FGLAxisImage.Material.Texture.TextureMode := tmReplace;
  FGLAxisImage.Material.Texture.MappingSCoordinates.AsVector := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.XHmgVector;
  FGLAxisImage.Material.Texture.MappingTCoordinates.AsVector := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.XHmgVector;
  FGLAxisImage.Scale.SetVector(1, 1, 1);
  FGLAxisImage.AlphaChannel := 0;
  FGLAxisImage.Visible := FShowAxis;

  FGLSphere := TGLSphere.CreateAsChild(vParent);
  FGLSphere.Position.Assign(FGLDCScene.Position);
  FGLSphere.Visible := False;

  FGLDirectRender := TGLDirectOpenGL.CreateAsChild(FGLScene.Objects);
  FGLDirectRender.OnRender := DoOnDirectOGLRender;

  BackgroundColor := $00DDDDDD;
  SetDefValues(Self);
  Buffer.WrapUpRendering := DoOnWrapUpRender;
  Buffer.AfterRender := DoOnAfterRender;
  Buffer.BeforeRender := DoOnBeforeRender;
  Buffer.InitiateRendering := DoOnInitiateRendering;

{$IFDEF CLIP_GLCLIP_GLCSG}
  Buffer.InitiateRendering := DoInitiateRenderingBuffer;
  Buffer.WrapUpRendering := DoWrapUpRenderingBuffer;
  Buffer.ContextOptions := Buffer.ContextOptions + [roStencilBuffer, roTwoSideLighting];
{$ENDIF}
{$IFDEF CLIP_SGMESH}
    FCuttingPlanes := TGLCuttingPlanes.Create;
    FGizmoCrossSection := TsgPlaneGizmo.Create(Self);
{$ENDIF}

  ShowAxis := False;
  CameraStyle := csOrthogonal;{FGLCamera.CameraStyle;}
  DoResetCamera;
end;

function Tsg3DNavigator.CreateGLPolyline: TsgGLPolyline;
begin
  Result := TsgGLPolyline.Create(Self);
  if Assigned(Result) then
    FTempListObjects.Add(Result);
end;

procedure Tsg3DNavigator.DoCreateRC;
begin
  Buffer.Resize(0, 0, Width, Height);
  Buffer.CreateRC(Tsg3DDrawingNavigator(Owner).FOwnDC, False);
  Buffer.RenderingContext.GL.DebugMode := False;
  Buffer.RenderingContext.GL.ARB_debug_output := False;
  InitializeSgGLVersion(Buffer.RenderingContext);
end;

procedure Tsg3DNavigator.DeleteGLPolyline(AObject: TObject);
var
  I: Integer;
begin
  I := FTempListObjects.IndexOf(AObject);
  if I <> -1 then
  begin
    TObject(FTempListObjects[I]).Free;
    FTempListObjects.Delete(I);
  end;
end;

destructor Tsg3DNavigator.Destroy;
var
  I: Integer;
begin
//  FGLLightSource.Free;
//  FGLCamera.Free;
//  FGLDCScene.Free;
{$IFDEF CLIP_SGMESH}
  if Assigned(FCuttingPlanes) then
    FCuttingPlanes.Free;
  if Assigned(FGizmoCrossSection) then
    FGizmoCrossSection.Free;
{$ENDIF}
{$IFDEF USE_CLIP}
  FMeshContextList.Free;
{$ENDIF}
  for I := 0 to FTempListObjects.Count - 1 do
      TObject(FTempListObjects[I]).Free;
  FTempListObjects.Free;
  inherited Destroy;
end;

procedure Tsg3DNavigator.DoAfterLoadScene;
var
  I: Integer;
  vMats: TGLLibMaterial;
//  vAABB: TAABB;
//  vMaxx: TGLFloat;
  vMaterial: TColor;
  vNavigator: Tsg3DDrawingNavigator;
begin
  if FGLMaterialLibrary.Materials.Count = 0 then
  begin
    FGLFreeForm.Material.MaterialLibrary := FGLMaterialLibrary;
    vMats := FGLMaterialLibrary.Materials.Add;
    vMats.Material.FrontProperties.Diffuse.Red := 0;
    FGLFreeForm.Material.LibMaterialName := vMats.Name;
    vMaterial := clBlue;
    for I := 0 to FGLMaterialLibrary.Materials.Count - 1 do
      SetColorForMaterial(FGLMaterialLibrary.Materials[I].Material, ColorToVector(vMaterial));
    if Owner is Tsg3DDrawingNavigator then
    begin
      vNavigator := Tsg3DDrawingNavigator(Owner);
      for I := 0 to FGLFreeForm.MeshObjects.Count - 1 do
      begin
        vNavigator.AssignMaterialForMesh(FGLFreeForm.MeshObjects[I], vMaterial);
        if FGLFreeForm.MeshObjects[I].Normals.Count = 0 then
          vNavigator.GenNormals(FGLFreeForm.MeshObjects[I]);
      end;
    end;
  end;
//  vAABB := BBToAABB(FGLFreeForm.BoundingBox);
//  vMaxx := MaxFloat(MaxFloat(vAABB.max.V[0] - vAABB.min.V[0],
//    vAABB.max.V[1] - vAABB.min.V[1]), vAABB.max.V[2] - vAABB.min.V[2]);
//  FGLCubeExtents.CubeWidth := vMaxx;
//  FGLCubeExtents.CubeHeight := vMaxx;
//  FGLCubeExtents.CubeDepth := vMaxx;
////  FGLCubeExtents.CubeWidth := vAABB.max[0] - vAABB.min[0];
////  FGLCubeExtents.CubeHeight := vAABB.max[1] - vAABB.min[1];
////  FGLCubeExtents.CubeDepth := vAABB.max[2] - vAABB.min[2];
//  FGLCubeExtents.Position.AsAffineVector := VectorLerp(vAABB.min, vAABB.max, 0.5);
  FGLSphere.Radius := FGLDCScene.BoundingSphereRadius / 15;
end;

procedure Tsg3DNavigator.DoBufferChange(Sender: TObject);
begin
  if not FPainting then
  begin
    if (not Buffer.Rendering) and (not Buffer.Freezed) then
      Invalidate;
    Inc(FIsDirty);
  end;
end;

procedure Tsg3DNavigator.DoOnAfter3DAxisRender(Sender: TObject);
begin
  ShowScene;
  FAxes.HideAxis;
end;

procedure Tsg3DNavigator.DoOnAfterRender(Sender: TObject);
begin
  if Assigned(FOnAfterRender) then
    FOnAfterRender(Self);
end;

procedure Tsg3DNavigator.DoOnBefore3DAxisRender(Sender: TObject);
begin
  HideScene;
  if FShowAxis then
    FAxes.ShowAxis;
end;

procedure Tsg3DNavigator.DoOnBeforeRender(Sender: TObject);
begin
{$IFDEF CLIP_SGMESH}
  if Assigned(FGizmoCrossSection) then
    FGizmoCrossSection.GizmoRepaint;
{$ENDIF}
  if Assigned(FOnBeforeRender) then
    FOnBeforeRender(Self);
end;

procedure Tsg3DNavigator.DoOnDirectOGLRender(Sender: TObject;
  var rci: TRenderContextInfo);
begin
end;

procedure Tsg3DNavigator.DoOnInitiateRendering(Sender: TObject;
  var rci: TRenderContextInfo);
begin
  if Assigned(Sender) then
  begin
    rci.GLStates.Enable(stPolygonOffsetFill);
    rci.GLStates.Enable(stPolygonOffsetLine);
    rci.GLStates.Enable(stPolygonOffsetPoint);
    rci.GLStates.SetPolygonOffset(1, Measure3DParams.DimLwd);
  end;
end;

Var
  clip0 : Array [0..3] Of Double = (-1.0, 0.0, 0.0, 1 / 2.0);
  clip5 : Array [0..3] Of Double = (-0.0, -0.0, -1.0,-( 1 / 2.0) );
{$IFDEF CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}

procedure Tsg3DNavigator.AddClip(AClipPlan: Integer; APointPlan, ANormal: TFPoint);
{$IFDEF CLIP_GLMESH}
var
  I: Integer;
  vData: TsgClipPLaneData;
{$ENDIF}
{$IFDEF CLIP_SGMESH}
var
  vPlane: TCuttingPlane;
{$ENDIF}
begin
{$IFDEF CLIP_GLCLIP_GLMESH}
  // Clips the object visually via OpenGL.
  FClipUse[AClipPlan + GL_CLIP_PLANE0] := True;
  FClip[AClipPlan + GL_CLIP_PLANE0, 0] := APointPlan;
  FClip[AClipPlan + GL_CLIP_PLANE0, 1] := ANormal;
{$ENDIF}

{$IFDEF CLIP_GLMESH}
  // Slices the model by means of OpenCascade.
  ClearClipPlaneMeshBuilder;
  for I := GL_CLIP_PLANE0 to GL_CLIP_PLANE5 do
  begin
    if FClipUse[I] = True then
    begin
      vData.ClipIndex := 1;
      vData.Positon.Center := FClip[I, 0];
      vData.Positon.Normal := FClip[I, 1];
      vData.MeshData := nil;
      AddClipPlaneMeshBuilder(vData);
    end;
  end;
{$ENDIF}
{$IFDEF CLIP_SGMESH}
  if not Assigned(FCuttingPlanes) then
    FCuttingPlanes := TGLCuttingPlanes.Create;
  vPlane.Point := AddFPoint(APointPlan, Tsg3DDrawingNavigator(Owner).FBoxOffs);
  vPlane.Normal := ANormal;
  FCuttingPlanes.PlaneByIndex[AClipPlan] := vPlane;
{$ENDIF}
end;

procedure Tsg3DNavigator.RemoveClip(AClipPlan: Integer);
begin
{$IFNDEF CLIP_SGMESH}
  FClipUse[AClipPlan + GL_CLIP_PLANE0] := False;
  FClip[AClipPlan + GL_CLIP_PLANE0, 0] := cnstFPointZero;
  FClip[AClipPlan + GL_CLIP_PLANE0, 1] := cnstFPointZero;
  FClipGL[AClipPlan + GL_CLIP_PLANE0, 0] := 0;
  FClipGL[AClipPlan + GL_CLIP_PLANE0, 1] := 0;
  FClipGL[AClipPlan + GL_CLIP_PLANE0, 2] := 0;
  FClipGL[AClipPlan + GL_CLIP_PLANE0, 3] := 0;
{$ENDIF}
{$IFDEF CLIP_SGMESH}
  if Assigned(FCuttingPlanes) then
  begin
    if AClipPlan >= 0 then
      FCuttingPlanes.PlaneDelByIndex(AClipPlan)
    else
      FCuttingPlanes.Clear;
  end;
{$ENDIF}
end;

{$IFDEF CLIP_SGMESH}
procedure Tsg3DNavigator.SaveOriginalMeshData;
var
  I: Integer;
  vWriter: TBinaryWriter;
  vMeshDataStream: TMemoryStream;
begin
  if not Assigned(FOriginalMeshData) then
  begin
    FOriginalMeshData := TsgObjectList.Create;
    FOriginalMeshData.Count := FGLFreeForm.MeshObjects.Count;
    for I := 0 to FGLFreeForm.MeshObjects.Count - 1 do
    begin
      if TsgMeshObject(FGLFreeForm.MeshObjects.Items[I]).InheritsFrom(TsgMeshObject) then
      begin
        TsgMeshObject(FGLFreeForm.MeshObjects.Items[I]).SaveData;
        FOriginalMeshData[I] := nil;
      end
      else
      begin
        vMeshDataStream := TMemoryStream.Create;
        vWriter := TBinaryWriter.Create(vMeshDataStream);
        try
          FGLFreeForm.MeshObjects[I].WriteToFiler(vWriter);
        finally
          vWriter.Free;
        end;
        FOriginalMeshData[I] := vMeshDataStream;
        //FGLFreeForm.MeshObjects[I].Clear;
      end;
    end;
  end;
end;

procedure Tsg3DNavigator.RestoreOriginalMeshData;
var
  I, vCnt: Integer;
  vReader: TBinaryReader;
  vMeshDataStream: TMemoryStream;
begin
  if Assigned(FOriginalMeshData) then
  begin
    vCnt := FOriginalMeshData.Count;
    if vCnt <> FGLFreeForm.MeshObjects.Count then
      Assert(False, 'Mesh counts not equal');
    for I := 0 to FGLFreeForm.MeshObjects.Count - 1 do
    begin
      if Assigned(FOriginalMeshData[I]) then
      begin
          FGLFreeForm.MeshObjects[I].Clear;
          vMeshDataStream := TMemoryStream(FOriginalMeshData[I]);
          vMeshDataStream.Position := 0;
          vReader := TBinaryReader.Create(vMeshDataStream);
          try
            FGLFreeForm.MeshObjects[I].ReadFromFiler(vReader);
          finally
            vReader.Free;
          end;
          vMeshDataStream.Free;
      end
      else
        TsgMeshObject(FGLFreeForm.MeshObjects.Items[I]).RestoreData;
    end;
    FOriginalMeshData.Free;
    FOriginalMeshData := nil;
  end;
end;
{$ENDIF}

{$IFDEF CLIP_GLCLIP_GLCSG}

procedure Tsg3DNavigator.DoInitiateRenderingBuffer(Sender: TObject;
  var rci: TRenderContextInfo);
{$IFDEF CLIP_GLCLIP}
var
  vPlane: THmgPlane;
  M, vMIT: TMatrix4f;
  I: Integer;
  vPointPlan, vNormal: TVector3f;
{$ENDIF}
begin
  //
  //Clip
  //
{$IFDEF CLIP_GLCLIP}
  for I := GL_CLIP_PLANE0 to GL_CLIP_PLANE5 do
  begin
    if FClipUse[I] = True then
    begin
      vPointPlan := FPoint2Vect(AddFPoint(FClip[I, 0], Tsg3DDrawingNavigator(Parent).FBoxOffs));

      vNormal := FPoint2Vect(FClip[I, 1]);
      M := GLDCScene.Matrix;
      vPointPlan := VectorTransform(vPointPlan, M);
      vMIT :=M;
      InvertMatrix(vMIT);
      TransposeMatrix(vMIT);
      vNormal:=VectorTransform(vNormal, vMIT);
      vPlane := PlaneMake( vPointPlan, vNormal);
      FClipGL[I, 0] := vPlane.X;
      FClipGL[I, 1] := vPlane.Y;
      FClipGL[I, 2] := vPlane.Z;
      FClipGL[I, 3] := vPlane.W;
      GL.Enable(I);
      GL.ClipPlane(I, @FClipGL[I, 0]);
    end
    else
      GL.Disable(I);
  end;
{$ENDIF}

end;

procedure Tsg3DNavigator.DoWrapUpRenderingBuffer(Sender: TObject;
  var rci: TRenderContextInfo);
{$IFDEF CLIP_CSG}
var
  I: Integer;
{$ENDIF}
begin
  // Clip
{$IFDEF CLIP_CSG}
  for I := GL_CLIP_PLANE0 to GL_CLIP_PLANE5 do
  begin
    if FClipUse[I] = True then
      GL.Disable(I);
  end;
{$ENDIF}
end;
{$ENDIF}

{$ENDIF}


procedure Tsg3DNavigator.DoOnWrapUpRender(Sender: TObject;
  var rci: TRenderContextInfo);

{$IFDEF DEMO}
var
  I: Integer;
  vGLCanvas: TGLCanvas;
  vClientWidth, vClientHeight, vTextWidth, vTextHeight: Integer;
  vTextPosX, vTextPosY, vInitTextPosX, vInitTextPosY: Integer;
  vPenWidthOld: Integer;

  procedure MoveToTheNextChar(const CharIndex: Integer);
  begin
    vTextPosX := vInitTextPosX;
    Inc(vTextPosX, 2 * CharIndex * vTextWidth);
    vTextPosY := vInitTextPosY;
    vGLCanvas.MoveTo(vTextPosX, vTextPosY);
  end;

  procedure DrawVerticalLine; overload;
  begin
    vGLCanvas.MoveTo(vTextPosX, vTextPosY);
    vGLCanvas.LineTo(vTextPosX, vTextPosY + vTextHeight);
  end;

  procedure DrawHorizontalLine;  overload;
  begin
    vGLCanvas.MoveTo(vTextPosX, vTextPosY);
    vGLCanvas.LineTo(vTextPosX + vTextWidth, vTextPosY);
  end;

  procedure DrawLineTo(const X, Y: Integer);
  begin
    vGLCanvas.LineTo(X, Y);
    vGLCanvas.MoveTo(X, Y);
  end;

  procedure DrawHorizontalLine(const Offset: Integer); overload;
  begin
    MoveToTheNextChar(3);
    vGLCanvas.MoveTo(Integer(vTextPosX + vTextWidth div 4), Integer(vTextPosY + Offset));
    Inc(vTextPosY, Offset);
    DrawLineTo(vTextPosX + vTextWidth - vTextWidth div 4, vTextPosY);
  end;

  procedure DrawVertLine(const Offset: Integer); overload;
  begin
    MoveToTheNextChar(3);
    vGLCanvas.MoveTo(Integer(vTextPosX + Offset), Integer(vTextPosY + vTextHeight div 4));
    DrawLineTo(vTextPosX + Offset, vTextPosY + vTextHeight - vTextHeight div 4);
  end;

  procedure DrawSlesh(const OffsetX, OffsetY, SignDivX1,
    SignDivX2, SignY: Integer);
  begin
    MoveToTheNextChar(3);
    Inc(vTextPosX, OffsetX + SignDivX1 * vTextWidth div 4);
    Inc(vTextPosY, OffsetY);
    vGLCanvas.MoveTo(vTextPosX, vTextPosY);
    DrawLineTo(vTextPosX + SignDivX2 * vTextWidth div 4, vTextPosY +
      SignY * vTextHeight div 4);
  end;
{$ENDIF}

begin
  if Assigned(FOnWrapUpRender) then
    FOnWrapUpRender(Self, rci);
{$IFDEF DEMO}
  vClientWidth := Buffer.Width;
  vClientHeight := Buffer.Height;
  vGLCanvas := TGLCanvas.Create(vClientWidth, vClientHeight);
  try
    vTextWidth := 50;
    vTextHeight := 100;
    vInitTextPosX := vClientWidth div 2 - vTextWidth * 4;
    vInitTextPosY := vClientHeight div 2 - vTextHeight div 2;
    vGLCanvas.PenColor := GetColorByBackgroud(BackgroundColor);
    vPenWidthOld :=  vGLCanvas.PenWidth;
    vGLCanvas.PenWidth := 5;

    MoveToTheNextChar(0); //D
    DrawVerticalLine;
    MoveToTheNextChar(0);
    DrawLineTo(vTextPosX + vTextWidth div 2, vTextPosY);
    DrawLineTo(vTextPosX + vTextWidth, vTextPosY + vTextHeight div 4);
    DrawLineTo(vTextPosX + vTextWidth, vTextPosY + vTextHeight - vTextHeight div 4);
    MoveToTheNextChar(0);
    vGLCanvas.MoveTo(vTextPosX, vTextPosY + vTextHeight);
    Inc(vTextPosY, vTextHeight);
    DrawLineTo(vTextPosX + vTextWidth div 2, vTextPosY);
    DrawLineTo(vTextPosX + vTextWidth, vTextPosY - vTextHeight div 4);
//    vGLCanvas.ArcDirection := adClockWise;
//    vGLCanvas.Arc(vTextPosX, vTextPosY, vTextPosX + vTextWidth,
//      vTextPosY + vTextHeight, vTextPosX, vTextPosY, vTextPosX,
//      vTextPosY + vTextHeight);

    MoveToTheNextChar(1); //E
    DrawVerticalLine;
    for I := 0 to 2 do
    begin
      MoveToTheNextChar(1);
      Inc(vTextPosY, I * (vTextHeight div 2));
      DrawHorizontalLine;
    end;

    for I := 0 to 1 do //M
    begin
      MoveToTheNextChar(2);
      Inc(vTextPosX, I * vTextWidth);
      DrawVerticalLine;
    end;
    MoveToTheNextChar(2);
    vGLCanvas.LineTo(Integer(vTextPosX + vTextWidth div 2), Integer(vTextPosY + vTextHeight div 2));
    MoveToTheNextChar(2);
    Inc(vTextPosX, vTextWidth);
    vGLCanvas.MoveTo(vTextPosX, vTextPosY);
    vGLCanvas.LineTo(Integer(vTextPosX - vTextWidth div 2), Integer(vTextPosY + vTextHeight div 2));

    DrawHorizontalLine(0); //O
    DrawHorizontalLine(vTextHeight);
    DrawVertLine(0);
    DrawVertLine(vTextWidth);
    DrawSlesh(vTextWidth, 0, -1, 1, 1);
    DrawSlesh(vTextWidth, vTextHeight, -1, 1, -1);
    DrawSlesh(0, 0, 1, -1, 1);
    DrawSlesh(0, vTextHeight, 1, -1, -1);

//    MoveToTheNextChar(3); //O
//    vGLCanvas.Ellipse(vTextPosX + vTextWidth div 2, vTextPosY + vTextHeight div 2,
//      vTextWidth div 2, vTextHeight div 2);
    vGLCanvas.PenWidth := vPenWidthOld;
  finally
    vGLCanvas.Free;
  end;
{$ENDIF}
  FIsDirty := 0;
end;

procedure Tsg3DNavigator.DoResetCamera;
begin
  if (FCameraStyle = csOrthogonal) and (not IsEmpty) then
  begin
    FDefCameraScale := 0;
  end;

  FGLDCScene.Position.AsVector := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NullHmgVector;
  FGLDCTarget.Position.AsVector := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NullHmgVector;
  FGLCamera.Position.AsAffineVector := cntDefCameraPos;

  SetDefDirForObject(FGLDCTarget);
  SetDefDirForCamera(FGLCamera);
  SetDefDirForObject(FGLDCScene);

  SetDefDistanceForCamera(FGLCamera, BoundingSphereRadius, FDefCameraScale,
    IsEmpty);

  FGLDefSceneMat := FGLDCScene.Matrix;

  FAxes.SetDefPositions;
  SetDefDistanceForCamera(FAxes.Camera, FAxes.BoundingSphereRadius,
    FAxes.DefCameraScale);
  //SetAxisPos;
end;

function Tsg3DNavigator.GeLightsCount: Integer;
begin
  Result := High(FLights) - Low(FLights) + 1;
end;

function Tsg3DNavigator.GeLights(Index: Integer): TGLLightSource;
begin
  Result := FLights[Index];
end;

function Tsg3DNavigator.GetBackgroundColor: TColor;
begin
  Result := Buffer.BackgroundColor;
end;

function Tsg3DNavigator.GetBaseRenderObject: TGLBaseSceneObject;
begin
  Result := FGLDCScene;
end;

function Tsg3DNavigator.GetBoundingSphereRadius: Single;
begin
  Result := 0;
  if not IsEmpty then
    Result := FGLFreeForm.BoundingSphereRadius;
end;

function Tsg3DNavigator.GetBuffer: TGLSceneBuffer;
begin
  Result := Buffer;
end;

function Tsg3DNavigator.GetDefSceneMatrix: {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.TMatrix;
begin
  Result := FGLDefSceneMat;
end;

function Tsg3DNavigator.GetDimension(X, Y: Integer): TObject;
var
  I: Integer;
  vDim: TsgPickObject;
  vPolylineObject: TsgGLPolyline;
  vPoint: TFPoint;

begin
  Result := nil;
  if not Assigned(FDimensionList) then
    Exit;
  vPoint := MakeFPoint(X, Buffer.Height - Y);
  for I := 0 to FDimensionList.Count - 1 do
  begin
    vDim := TsgPickObject(FDimensionList.List[I]);
    vPolylineObject := TsgPickObject(FDimensionList.List[I]).FPoly;
    if vPolylineObject.IsContainsPoint(vPoint) then
    begin
      Result := TObject(vDim);
      Break;
    end;
  end;
end;

class function Tsg3DNavigator.GetFilter: string;
var
  I, vPos: Integer;
  aSTLFilters: array[0..2] of string;
begin
  aSTLFilters[0] := cnstSTLFileExtWithDot + ';';
  aSTLFilters[1] := aSTLFilters[0];
  aSTLFilters[2] := cnstSTLFilterDesc;
  Result := VectorFileFormatsFilter;
  //STL extensions must be deleted from the filter, STL File format supports CADImportVCL kernel
  for I := Low(aSTLFilters) to High(aSTLFilters) do
  begin
    vPos := Pos(aSTLFilters[I], Result);
    if vPos > 0 then
      Delete(Result, vPos, Length(aSTLFilters[I]));
  end;
end;

function Tsg3DNavigator.GetFilterProp: string;
begin
  Result := GetFilter;
end;

function Tsg3DNavigator.GetGLFreeForm: TGLFreeForm;
begin
  Result := FGLFreeForm;
end;

function Tsg3DNavigator.GetIsVisibleDimensionList: Boolean;
begin
  Result := FIsVisibleDimensionList;
end;

function Tsg3DNavigator.GetMaterialByName(AMaterialName: string): TGLMaterial;
var
  vMat: TGLLibMaterial;
begin
  Result := nil;
  if Assigned(FGLMaterialLibrary) then
  begin
    vMat := FGLMaterialLibrary.Materials.GetLibMaterialByName(AMaterialName);
    if Assigned(vMat) then
      Result := vMat.Material;
  end;
end;

function Tsg3DNavigator.GetTypeIntersect(X, Y: Integer; var AObject: TObject;
  AIntersect, ANormal: PFPoint; var ACancel: Boolean;
  const AType: TsgTypeIntersect): Boolean;
var
  I, vMeshIndex: Integer;
  vDistance, vMin, vMax: Extended;
  vMeshs: TsgObjectList;
  vIntersects, vNormals: TFPointList;
  vIntersectsTemp, vNormalTemp: TFPointList;
  vMeshsTemp: TsgObjectList;

  vIsOnlyAlpha: Boolean;

  vRayStart, vIntersect, vRayStartWorld: TFPoint;
  vNode: TCustomNode;
  vUniqueMeshList: TsgObjectList;
  vNavigator: Tsg3DDrawingNavigator;

  function IsAlphaMesh(const AMesh: TObject): Boolean;
  var
    vMesh: TsgMeshObject;
  begin
    Result := False;
    if AMesh is TsgMeshObject then
    begin
      vMesh := TsgMeshObject(AMesh);
      if (vMesh.FaceGroups.Count <> 0) and
         (vMesh.FaceGroups[0].MaterialName <> '') and
         (SysUtils.StrPos(PChar(@vMesh.FaceGroups[0].MaterialName[1]),
           PChar(@cnstAlphaPref[1])) <> nil) then
        Result := True;
    end;
  end;
begin
  Result := False;
  AObject := nil;
  vMeshs := TsgObjectList.Create;
  try
    vIntersects := TFPointList.Create;
    vNormals := nil;
    if ANormal <> nil then
      vNormals := TFPointList.Create;
    GetTypeIntersects(X, Y, vMeshs, ACancel, vIntersects, vNormals, AType);

    case AType of
      tiSnap, tiSnapEdge:
        begin
          vNormalTemp := nil;
          vIntersectsTemp := TFPointList.Create;
          if vNormals <> nil then
            vNormalTemp := TFPointList.Create;
          vMeshsTemp := TsgObjectList.Create;
          try
            vMeshsTemp.Assign(vMeshs);
            vIntersectsTemp.Assign(vIntersects);
            if vNormalTemp <> nil then
              vNormalTemp.Assign(vNormals);
            GetTypeIntersects(X, Y, vMeshs, ACancel, vIntersects, vNormals, tiMesh);
            vMeshs.AppendDynArray(vMeshsTemp);// CopyLists(vMeshs, vMeshsTemp, cmAppend);
            vIntersects.AppendDynArray(vIntersectsTemp);
            if vNormalTemp <> nil then
              vNormals.AppendDynArray(vNormalTemp);
          finally
            vIntersectsTemp.Free;
            if vNormalTemp <> nil then
              vNormalTemp.Free;
            vMeshsTemp.Free;
          end;
        end;
    end;

    case AType of
      tiSnapEdge:
        begin
          vNormalTemp := nil;
          vIntersectsTemp := TFPointList.Create;
          if vNormals <> nil then
            vNormalTemp := TFPointList.Create;
          vMeshsTemp := TsgObjectList.Create;
          try
            vMeshsTemp.Assign(vMeshs);
            vIntersectsTemp.Assign(vIntersects);
            if vNormalTemp <> nil then
              vNormalTemp.Assign(vNormals);
            GetTypeIntersects(X, Y, vMeshs, ACancel, vIntersects, vNormals, tiDimension);

            vMeshs.AppendDynArray(vMeshsTemp); //CopyLists(vMeshs, vMeshsTemp, cmAppend);
            vIntersects.AppendDynArray(vIntersectsTemp);
            if vNormalTemp <> nil then
              vNormals.AppendDynArray(vNormalTemp);
          finally
            vIntersectsTemp.Free;
            if vNormalTemp <> nil then
              vNormalTemp.Free;
            vMeshsTemp.Free;
          end;
        end;
    end;

    try
      vIsOnlyAlpha := False;
      for I := 0 to vMeshs.Count - 1 do
      begin
        if IsAlphaMesh(vMeshs[I]) then
          vIsOnlyAlpha := True
        else
        begin
          vIsOnlyAlpha := False;
          Break;
        end;
      end;

      if vIsOnlyAlpha then
      begin
        vMeshIndex := -1;
        vNode := nil;
        vNavigator := nil;
        vRayStart := Vect2FPoint(Camera.AbsolutePosition);
        if Owner is Tsg3DDrawingNavigator then
        begin
          vNavigator := Tsg3DDrawingNavigator(Owner);
          vUniqueMeshList := TsgObjectList.Create;
          try
            vUniqueMeshList.Duplicates := dupIgnore;
            for I := 0 to vMeshs.Count - 1 do
            begin
              vUniqueMeshList.Add(
                TCustomNode.GetNodeOwnerByTopoClass(TCustomNode(vNavigator.FRoot),
                TsgMeshObject(vMeshs[I]).Node,
                [TsgModTopoSolid, TsgModPartCompound]));
            end;

            vMax := 0;
            vRayStartWorld := Vect2FPoint(VectorTransform(Camera.AbsolutePosition,
              GLFreeForm.InvAbsoluteMatrix));
            for I := 0 to vUniqueMeshList.Count - 1 do
            begin
              if Assigned(vUniqueMeshList[I]) then
              begin
                vDistance := 1 + TsgMinBox.BoxPointSqrDistanceTo(
                  TCustomNode(vUniqueMeshList[I]), vRayStartWorld);
                if vDistance > vMax then
                begin
                  vMax := vDistance;
                  vNode := TCustomNode(vUniqueMeshList[I]);
                end;
              end;
            end;
          finally
            vUniqueMeshList.Free;
          end;

        end;

        vMin := MaxExtended;
        I := 0;
        while (I < vMeshs.Count) and not ACancel do
        begin

          if Assigned(vNode) and Assigned(vNavigator) then
          begin
            if TCustomNode.GetNodeOwnerByTopoClass(TCustomNode(vNavigator.FRoot),
                TsgMeshObject(vMeshs[I]).Node,
                [TsgModTopoSolid, TsgModPartCompound]) <> vNode then
            begin
              Inc(I);
              Continue;
            end;
          end;

          vIntersect := Vect2FPoint(GLFreeForm.LocalToAbsolute(FPoint2Vect(vIntersects[I])));
          vDistance := DistanceFPointSqr(vRayStart, vIntersect);
          if vDistance < vMin then
          begin
            vMin := vDistance;
            vMeshIndex := I;
          end;
          Inc(I);
        end;
      end
      else
      begin
        vMin := MaxExtended;
        vMeshIndex := -1;
        I := 0;
        vRayStart := Vect2FPoint(Camera.AbsolutePosition);
        while (I < vMeshs.Count) and not ACancel do
        begin
          if not IsAlphaMesh(vMeshs[I]) then
          begin
            vIntersect := Vect2FPoint(GLFreeForm.LocalToAbsolute(FPoint2Vect(vIntersects[I])));
            vDistance := DistanceFPointSqr(vRayStart, vIntersect);
            if vDistance < vMin then
            begin
              vMin := vDistance;
              vMeshIndex := I;
            end;
          end;
          Inc(I);
        end;
      end;
      if (vMeshIndex >= 0) and not ACancel then
      begin
        AObject := vMeshs[vMeshIndex];
        if AIntersect <> nil then
          AIntersect^ := vIntersects[vMeshIndex];
        if ANormal <> nil then
          ANormal^ := vNormals[vMeshIndex];
        Result := True;
      end;
    finally
      vIntersects.Free;
      vNormals.Free;
    end;
  finally
    for I := 0 to vMeshs.Count - 1 do
      if TObject(vMeshs[I]) <> AObject then
        DeleteGLPolyline(vMeshs[I]);
    vMeshs.Free;
  end;
end;

function Tsg3DNavigator.GetTypeIntersects(X, Y: Integer;
  const AMeshObjects: TsgObjectList; var ACancel: Boolean;
  const APoints, ANormals: TFPointList;
  const AType: TsgTypeIntersect): Boolean;
var
  vMeshObject: TMeshObject;
  vPolylineObject: TsgGLPolyline;
  vPolylineBox:  TsgGLPolyline;
  vP: TsgNativeUInt;
  vEnt: TsgDXFEntity absolute vP;
  I, J, K, vIterateCount, vCountObject: Integer;
  vCountObjectLineMass, vCountObjectLineOuterWire,
    vCountObjectDimension: Integer;
  vStartPos3f, vRayVect3f: TVector3f;
  vRayPoint, vAnalyzedPoint, vAnalyzedPoint2: TFPoint;
  vStartPos4, vRayVect, vIntersect, vIntersectNormal: TVector4f;
  vTriangles: TAffineVectorList;
  vInTriangle, vInTriangleLocal, vResult: Boolean;
  vTr: PAffineVectorArray;
  vMin, vMax: TFPoint;
  vAABB: TAABB;
  vSize: TsgFloat;
  vFlagAnalize: Boolean;

  procedure AddCube(APoint, APoint2: TFPoint; ATriangles: TAffineVectorList;
    const ASize: TsgFloat = 0.2);
  var
    vMatrix: TFMatrix;
    hw, hh, hd, nd: TsgFloat;
    vDis: TsgFloat;
    vTransform: TTransformation;
    vPoint: TFPoint;
  begin
    if not IsEqualFPoints(APoint, APoint2) then
    begin
      vMatrix := MatFromExtr(Ort(SubFPoint(APoint2, APoint)), 0);
      vDis := DistanceFPoint(APoint, APoint2);
      vPoint := MakeFPoint(0, 0, vDis/2);
    end
    else
    begin
      vMatrix := cnstIdentityMat;
      vDis := ASize;
      vPoint := cnstFPointZero;
    end;
    vMatrix := FMatXMat(vMatrix, FMatByTranslate(APoint));
    vMatrix.E0 := AddFPoint(vMatrix.E0, AffineTransformPoint(vPoint, vMatrix));
    nd := 1;
    hw := ASize * 0.5;
    hh := ASize * 0.5;
    hd := vDis * 0.5;
    vTransform := TTransformation.Create(@vMatrix);
    try
      // cpFront
      // Normal3f(0, 0, nd);
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh, hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw * nd, hh * nd, hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh, hd)));

      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh, hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw * nd, -hh * nd, hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh, hd)));

      // cpBack
      // Normal3f(0, 0, -nd), APoint));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh, -hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw * nd, -hh * nd, -hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh, -hd)));

      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh, -hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw * nd, hh * nd, -hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh, -hd)));

      // cpLeft
      //  Normal3f(-nd, 0, 0), APoint));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, hh, hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, hh * nd, -hd * nd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh, -hd)));

      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh, -hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh * nd, hd * nd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, hh, hd)));

      // cpRight
      //  Normal3f(nd, 0, 0), APoint));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh, hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, -hh * nd, hd * nd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, -hh, -hd)));

      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, -hh, -hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh * nd, -hd * nd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh, hd)));

      // cpTop
      //  Normal3f(0, nd, 0), APoint));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, hh, -hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw * nd, hh, hd * nd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh, hd)));

      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, hh, hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw * nd, hh, -hd * nd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, hh, -hd)));

      // cpBottom
      //  Normal3f(0, -nd, 0), APoint));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh, -hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw * nd, -hh, -hd * nd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, -hh, hd)));

      ATriangles.Add(vTransform.Transformf(MakeFPoint(hw, -hh, hd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw * nd, -hh, hd * nd)));
      ATriangles.Add(vTransform.Transformf(MakeFPoint(-hw, -hh, -hd)));
    finally
      vTransform.Free;
    end;
  end;

  procedure TrianglesCast;
  var
    L: Integer;
  begin
    if Assigned(vTriangles) then
    begin
      vInTriangle := False;
      J := 0;
      vTr := vTriangles.List;
      while (J < vTriangles.Count) and not ACancel do
      begin
        vInTriangleLocal := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.RayCastTriangleIntersect(vStartPos4, vRayVect,
          vTr^[J], vTr^[J + 1], vTr^[J + 2], @vIntersect, @vIntersectNormal);
        vInTriangle := vInTriangle or vInTriangleLocal;
        Inc(J, 3);
        if vInTriangleLocal then
        begin
          case AType of
            tiMesh:
              begin
                if Assigned(AMeshObjects) then
                  AMeshObjects.Add(vMeshObject);
                if Assigned(APoints) then
                  APoints.Add(Vect2FPoint(vIntersect));
              end;
            tiSnap:
              begin
                if Assigned(AMeshObjects) then
                begin
                  vPolylineBox := CreateGLPolyline;
                  vPolylineBox.Add(vAnalyzedPoint);
                  vPolylineBox.Add(vAnalyzedPoint);
                  vPolylineBox.PointSize := Measure3DParams.MarkerSize;
                  vPolylineBox.MaterialNamePoint :=
                    AssignMaterial(Measure3DParams.MarkerHoverColor, FGLMaterialLibrary);
                  vPolylineBox.VisiblePoints := True;
                  //vPolylineBox.Assign(vTriangles);
                  AMeshObjects.Add(vPolylineBox);
                end;
                if Assigned(APoints) then
                  APoints.Add(Vect2FPoint(vIntersect));//APoints.Add(vAnalyzedPoint); // APoints.Add(Vect2FPoint(vIntersect));
              end;
            tiSnapEdge:
              begin
                if Assigned(AMeshObjects) then
                begin
                  vPolylineBox := CreateGLPolyline;
                  for L := 0 to vPolylineObject.Count - 1 do
                  begin
                    vPolylineBox.Add(vPolylineObject.List[L]);
                  end;

                  vPolylineBox.FNormal := vPolylineObject.FNormal;
                  vPolylineBox.FNormals.Assign(vPolylineObject.FNormals);

                  vPolylineBox.PointSize := Measure3DParams.MarkerSize;
                  vPolylineBox.MaterialNamePoint :=
                    AssignMaterial(Measure3DParams.SelectNodeColor, FGLMaterialLibrary);
                  vPolylineBox.MaterialName :=
                    AssignMaterial(Measure3DParams.EdgesColor, FGLMaterialLibrary);

                  //vPolylineBox.Assign(vTriangles);
                  AMeshObjects.Add(vPolylineBox);
                end;
                if Assigned(APoints) then
                  APoints.Add(Vect2FPoint(vIntersect));//APoints.Add(vAnalyzedPoint); // APoints.Add(Vect2FPoint(vIntersect));
              end;
            tiDimension:
              begin
                if Assigned(AMeshObjects) then
                begin
                  //vPolylineBox := vPolylineObject;
                  AMeshObjects.Add(TsgPickObject(FDimensionList.List[I]));
                end;
                if Assigned(APoints) then
                  APoints.Add(Vect2FPoint(vIntersect));//APoints.Add(vAnalyzedPoint); // APoints.Add(Vect2FPoint(vIntersect));

              end;
          end;
          if Assigned(ANormals) then
            ANormals.Add(Vect2FPoint(vIntersectNormal));
        end;
      end;
      vResult := vResult or vInTriangle;
    end;
  end;

  procedure ExtendMinMax(const ASize: Double; var AMin: Double; var AMax: Double);
  begin
    if IsEqual(AMin, AMax) then
    begin
      AMin := AMin - ASize;
      AMax := AMax + ASize;
    end;
  end;

begin
  if Assigned(AMeshObjects) then
    AMeshObjects.Clear;
  if Assigned(APoints) then
    APoints.Clear;
  if Assigned(ANormals) then
    ANormals.Clear;
  vSize := 0;
  case AType of
    tiSnap, tiSnapEdge, tiDimension:
      begin
        vSize := DistanceFPoint(Vect2FPoint(ClientToWorld(0, 0)),
          Vect2FPoint(ClientToWorld(1, 0)),fDoubleResolution*fDoubleResolution);
        vSize := vSize * 20;
      end;
  end;
  vStartPos3f := GLFreeForm.AbsoluteToLocal(ClientToWorld(X, Y));
  MakeVector(vStartPos4, vStartPos3f);
  vRayVect := Camera.AbsoluteVectorToTarget;
  NormalizeVector(vRayVect);
  vRayVect := GLFreeForm.AbsoluteToLocal(vRayVect);
  vRayVect3f := AffineVectorMake(vRayVect);
  vRayPoint := Vect2FPoint(vRayVect3f);
  vCountObject := 0;
  vCountObjectLineMass := 0;//hint!
  vCountObjectLineOuterWire := 0;
  case AType of
    tiMesh: vCountObject := GLFreeForm.MeshObjects.Count;
    tiSnap, tiSnapEdge:
    begin
      if Assigned(FLineMass) then
        vCountObjectLineMass := FLineMass.FPolylines.Count
      else
        vCountObjectLineMass := 0;

      if Assigned(FLineOuterWire) then
        vCountObjectLineOuterWire := FLineOuterWire.FPolylines.Count
      else
        vCountObjectLineOuterWire := 0;

      if Assigned(FDimensionList) then
        vCountObjectDimension := FDimensionList.Count
      else
        vCountObjectDimension := 0;



      vCountObject := vCountObjectLineMass + vCountObjectLineOuterWire + vCountObjectDimension;
    end;
    tiDimension:
    begin
      if Assigned(FDimensionList) then
        vCountObject := FDimensionList.Count;
    end;
  end;
  I := 0;
  vP := 0;
  vResult := False;
  while (I < vCountObject) and not ACancel do
  begin
    vFlagAnalize := True;
    vPolylineObject := nil;
    case AType of
      tiSnap, tiSnapEdge:
      begin
        if I >= vCountObjectLineMass then
        begin
          if I >= (vCountObjectLineMass + vCountObjectLineOuterWire) then
            vPolylineObject := TsgPickObject(FDimensionList.List[I-vCountObjectLineMass-vCountObjectLineOuterWire]).FPoly
          else
            vPolylineObject := TsgGLPolyline(FLineOuterWire.FPolylines.List[I-vCountObjectLineMass])
        end
        else
          vPolylineObject := TsgGLPolyline(FLineMass.FPolylines.List[I]);
      end;
      tiDimension:
        begin
          if FDimensionList.List[I] <> nil then
          begin
            vPolylineObject := TsgPickObject(FDimensionList.List[I]).FPoly;
          end;
        end;
      tiMesh:
        begin
          vMeshObject := GLFreeForm.MeshObjects[I];
          vFlagAnalize := vMeshObject.Visible;
        end;
    end;
    case AType of
      tiSnap, tiSnapEdge, tiDimension:
      begin
        if (not Assigned(vPolylineObject)) or (vPolylineObject.FCountUses <= 0)
          or (vPolylineObject.Count = 0) then
        begin
          Inc(I);
          Continue;
        end;
      end;
      tiMesh:
      begin
        if vMeshObject.Vertices.Count = 0 then
        begin
          Inc(I);
          Continue;
        end;
      end;
    end;
    if vFlagAnalize then
    begin
      case AType of
        tiSnap, tiSnapEdge, tiDimension:
        begin
          vPolylineObject.GetExtents(vMin, vMax);
          if IsEqual(vMin.X,vMax.X) or IsEqual(vMin.Y,vMax.Y)
             or IsEqual(vMin.Z,vMax.Z) then
          begin
            ExtendMinMax(vSize, vMin.X, vMax.X);
            ExtendMinMax(vSize, vMin.Y, vMax.Y);
            ExtendMinMax(vSize, vMin.Z, vMax.Z);
          end;
          vAABB.Min := FPoint2Vect(vMin);
          vAABB.Max := FPoint2Vect(vMax);
        end;
        tiMesh: vMeshObject.GetExtents(vAABB);
      end;
      if RayCastBoxIntersect(vStartPos3f, vRayVect3f, vAABB.min, vAABB.max) then
      begin
        vIterateCount := 0;
        case AType of
          tiSnap: vIterateCount := vPolylineObject.Count - 1;
          tiSnapEdge, tiDimension:
            begin
              vIterateCount := vPolylineObject.Count - 2;
            end;
        end;

{$IFDEF ADD_POINT_CIRCLE}
        case AType of
          tiSnap:
            begin
              if vPolylineObject.Radius > 0 then
              begin
                vAnalyzedPoint := vPolylineObject.Center;
                vTriangles := TAffineVectorList.Create;
                try
                  AddCube(vAnalyzedPoint, vAnalyzedPoint, vTriangles, vSize);
                  TrianglesCast;
                finally
                  vTriangles.Free;
                end;
              end;
            end;
        end;
{$ENDIF}

        for K := 0 to vIterateCount do
        begin
          case AType of
            tiSnap:
              begin
                vAnalyzedPoint := vPolylineObject.List[K];
                vTriangles := TAffineVectorList.Create;
                AddCube(vAnalyzedPoint, vAnalyzedPoint, vTriangles, vSize);
              end;
            tiSnapEdge, tiDimension:
              begin
                vAnalyzedPoint := vPolylineObject.List[K];
                vAnalyzedPoint2 := vPolylineObject.List[K+1];
                vTriangles := TAffineVectorList.Create;
                AddCube(vAnalyzedPoint, vAnalyzedPoint2, vTriangles, vSize);
              end;
            tiMesh: vTriangles := vMeshObject.ExtractTriangles;
          end;
          try
            TrianglesCast;
          finally
            vTriangles.Free;
          end;
        end;
      end;
    end;
    Inc(I);
  end;
  if Assigned(AMeshObjects) then
    Result := AMeshObjects.Count > 0
  else
    Result := vResult;
  Result := Result and not ACancel;
end;

function Tsg3DNavigator.GetScaleByPrint: Double;
begin
  Result := FScaleByPrint;
end;

function Tsg3DNavigator.GetShowAxis: Boolean;
begin
  Result := FShowAxis;
end;

function Tsg3DNavigator.GetShowCubeExtents: Boolean;
begin
  Result := FShowCubeExtents;
end;

function Tsg3DNavigator.GetStyleCamera: TGLCameraStyle;
begin
  Result := FCameraStyle;
end;

function Tsg3DNavigator.GetTrianglesCount: Integer;
begin
  Result := FGLFreeForm.MeshObjects.TriangleCount;
end;

procedure Tsg3DNavigator.HideLights;
var
  I: Integer;
begin
  for I := 0 to LightsCount - 1 do
    Lights[I].Visible := False;
end;

procedure Tsg3DNavigator.HideScene;
begin
  HideLights;
  FGLDCScene.Visible := False;
  FGLDCTarget.Visible := False;
  Camera.Visible := False;
end;

procedure Tsg3DNavigator.InitSceneObject(AObject: TGLBaseSceneObject;
  const APosition, ADirection: TVector3f; const AOffs: Single);
begin
  AObject.BeginUpdate;
  try
    // Set (0,0,0) as scene center
    AObject.Position.AsAffineVector := APosition;
    // reset scene scale
    AObject.Scale.SetVector(1, 1, 1);
    // reset scene rotations
    AObject.ResetRotations;
    AObject.Direction.AsAffineVector := VectorNormalize(ADirection);
    if AOffs <> 0 then
      AObject.Move(AOffs);
    if AObject is TGLLightSource then
      if TGLLightSource(AObject).LightStyle = lsParallel then
        TGLLightSource(AObject).SpotDirection := AObject.Direction;
  finally
    AObject.EndUpdate;
  end;
end;

procedure Tsg3DNavigator.Invalidate;
begin
{$IFDEF SG_FIREMONKEY}
{$ELSE}
  if Owner is TControl then
    TControl(Owner).Invalidate;
{$ENDIF}
end;

function Tsg3DNavigator.IsClipPlaneUses: Boolean;
{$IFDEF CLIP_GLCLIP_GLMESH}
var
  I: Integer;
{$ENDIF}
begin
  Result := False;
{$IFDEF USE_CLIP}
  if not FCrossSectionOn then
    Exit;
  if FLockClip then
    Exit;
{$ENDIF}

{$IFDEF CLIP_GLCLIP_GLMESH}
  for I := GL_CLIP_PLANE0 to GL_CLIP_PLANE5 do
  begin
    if FClipUse[I] = True then
    begin
      Result := True;
      Break;
    end;
  end;
{$ENDIF}
{$IFDEF CLIP_SGMESH}
  if Assigned(FCuttingPlanes) and (FCuttingPlanes.PlaneCount > 0) then
    Result := True;
{$ENDIF}
end;

function Tsg3DNavigator.IsEmpty: Boolean;
begin
  // GLFreeForm.Count = 1
  //   - GLCubeExtents
  Result := (GLFreeForm.MeshObjects.Count = 0) and
    (GLFreeForm.Count <= 1);
end;

procedure Tsg3DNavigator.LoadFromFile(const AFileName: string;
  ADoClear: Boolean = True);
var
  vStream: TMemoryStream;
  vDir: string;
begin
  vDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(AFileName));
    vStream := TMemoryStream.Create;
    try
      vStream.LoadFromFile(AFileName);
      LoadFromStream(AFileName, vStream, ADoClear);
    finally
      vStream.Free;
    end;
  finally
    SetCurrentDir(vDir);
  end;
end;

procedure Tsg3DNavigator.LoadFromStream(const AFileName: string; Stream: TStream;
  ADoClear: Boolean = True);
begin
  if ADoClear then
    Clear;
  try
    FGLFreeForm.LoadFromStream(AFileName, Stream);
    DoAfterLoadScene;
  finally
    DoResetCamera;
  end;
end;

procedure Tsg3DNavigator.LoadSceneIdentity;
begin
  FGLDCScene.Matrix := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.IdentityHmgMatrix;
  FGLFreeForm.Matrix := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.IdentityHmgMatrix;
  SetSceneScale;
end;

procedure Tsg3DNavigator.Painted;
begin
  FPainting := False;
end;

procedure Tsg3DNavigator.Painting;
begin
  FPainting := True;
end;

function Tsg3DNavigator.Perform(Msg: Cardinal; WParam: WPARAM;
  LParam: LPARAM): LRESULT;
begin
{$IFDEF SG_FIREMONKEY}
{$ELSE}
  Result := TControl(Owner).Perform(Msg, WParam, LParam);
{$ENDIF}
end;

procedure Tsg3DNavigator.PerformAutoCentering;
begin

end;

procedure Tsg3DNavigator.PositionLight(const ACenter: TVector3f;
  const ADistance: Single);
const
  cnstDistScale = 1;
begin
  // front, back
  InitSceneObject(Lights[0], ACenter, AffineVectorMake(0, 0, 1), ADistance * cnstDistScale);
  InitSceneObject(Lights[1], ACenter, AffineVectorMake(0, 0, -1), ADistance * cnstDistScale);
  // top, bottom
  InitSceneObject(Lights[2], ACenter, AffineVectorMake(0, 1, 0), ADistance * cnstDistScale);
  InitSceneObject(Lights[3], ACenter, AffineVectorMake(0, -1, 0), ADistance * cnstDistScale);
  // right, left
  InitSceneObject(Lights[4], ACenter, AffineVectorMake(1, 0.2, 0.1), ADistance * cnstDistScale);
  InitSceneObject(Lights[5], ACenter, AffineVectorMake(-1, 0.2, 0.1), ADistance * cnstDistScale);
  // near eyes
  InitSceneObject(Lights[6], ACenter, AffineVectorMake(-0.4, 0.3, 1), ADistance * cnstDistScale);
  InitSceneObject(Lights[7], ACenter, AffineVectorMake(0.4, 0.3, 1), ADistance * cnstDistScale);
end;

procedure Tsg3DNavigator.Render(baseObject: TGLBaseSceneObject);
begin
  ForceRenderingContext;
  Buffer.Render(baseObject);
end;

procedure Tsg3DNavigator.ForceRenderingContext;
begin
  if Buffer.RenderingContext = nil then
  begin
    Buffer.SetViewPort(0, 0, Width, Height);
    Buffer.CreateRC(Tsg3DDrawingNavigator(Owner).FOwnDC, False);
    Buffer.RenderingContext.GL.DebugMode := False;
    Buffer.RenderingContext.GL.ARB_debug_output := False;
  end;
end;

procedure Tsg3DNavigator.RenderAxis;
begin
  if FShowAxis then
  try
    Buffer.BeginUpdate;
    try
      FAxes.Render(FAxes.FGLDCAxes);
      FAxes.CopyToTexture(FGLAxisImage.Material.Texture);
    finally
      Buffer.EndUpdate;
    end;
  except
    FAxes.SetObjectsVisible(False);
    FGLAxisImage.Visible := False;
    FShowAxis := False;
    ShowScene;
  end;
end;

procedure Tsg3DNavigator.Rotate(const APitch, ATurn, ARoll: Extended);
var
  vCenter, vDeltaCenter: TVector3f;
begin
  FGLDCScene.BeginUpdate;
  try
    vCenter := VectorTransform(FCenter, FGLDCScene.Matrix);
    Translate(VectorNegate(FGLDCScene.Position.AsAffineVector));
    FGLCamera.RotateObject(FGLDCScene, -APitch, -ATurn, ARoll);
    vDeltaCenter := VectorSubtract(vCenter,
      VectorTransform(FCenter, FGLDCScene.Matrix));
    Translate(vDeltaCenter);
  finally
    FGLDCScene.EndUpdate;
  end;
end;

procedure Tsg3DNavigator.Rotate(Axis: TsgAxes; Angle: Extended);
var
  vPitch, vTurn, vRoll: Single;
begin
  vPitch := 0;
  vTurn := 0;
  vRoll := 0;
  case Axis of
    axisX: vPitch := Angle;
    axisY: vTurn := Angle;
    axisZ: vRoll := Angle;
  end;
  Rotate(vPitch, vTurn, vRoll);
end;

procedure Tsg3DNavigator.ScenePositionChange(Sender: TObject);
begin
  if Assigned(FOnScenePositionChange) then
    FOnScenePositionChange(Self);
  UpdateArrows;
end;

procedure Tsg3DNavigator.SetAxisPos;
begin
  if FGLAxisImage = nil then Exit;
  if FShowAxis then
  begin
    FGLAxisImage.Position.SetPoint(FGLAxisImage.Width / 2,
      Max(Buffer.Height - FGLAxisImage.Height / 2, cntAxisPosMinY), 0);
  end;
end;

procedure Tsg3DNavigator.SetBackgroundColor(const Value: TColor);
begin
  Buffer.BackgroundColor := Value;
end;

procedure Tsg3DNavigator.SetDefSceneMatrix;
begin
  FGLDCScene.Matrix := FGLDefSceneMat;
  SetSceneScale;
end;

procedure Tsg3DNavigator.SetIsVisibleDimensionList(const Value: Boolean);
var
  I: Integer;
begin
  if Value <> FIsVisibleDimensionList then
  begin
    if Assigned(FDimensionList) then
    begin
      FIsVisibleDimensionList := Value;
      for I := 0 to FDimensionList.Count - 1 do
      begin
        TsgPickObject(FDimensionList[I]).GLVisible := FIsVisibleDimensionList;
      end;
    end;
  end;
end;

procedure Tsg3DNavigator.SetScaleByPrint(const Value: Double);
var
  I: Integer;
begin
  if Value <> FScaleByPrint then
  begin
    FScaleByPrint := Value;
    if Assigned(FBoxLine) then
      FBoxLine.ApplyPolylinesScale(FScaleByPrint, 1);
    if Assigned(FDimensionList) then
    begin
      for I := 0 to FDimensionList.Count - 1 do
      begin
        TsgPickObject(FDimensionList[I]).FPoly.FScale := FScaleByPrint;
      end;
    end;
  end;
end;

procedure Tsg3DNavigator.SetSceneScale;
begin
//  if FGLFreeForm  <> nil then
//    FGLFreeForm.Scale.AsAffineVector := FSceneScale;
  if FGLCubeExtents <> nil then
    FGLCubeExtents.Scale.Assign(FGLFreeForm.Scale);
end;

procedure Tsg3DNavigator.SetShowAxis(const Value: Boolean);
begin
  if FAxes = nil then Exit;
  if Value <> FShowAxis then
  begin
    FShowAxis := Value;
    FAxes.SetObjectsVisible(Value);
    FGLAxisImage.Visible := Value;
    if Owner is TControl then
      TControl(Owner).Repaint;
  end;
end;

procedure Tsg3DNavigator.SetShowCubeExtents(const Value: Boolean);
begin
  if Value <> FShowCubeExtents then
  begin
    FShowCubeExtents := Value;
    FGLCubeExtents.Visible := Value;
    if Owner is TControl then
      TControl(Owner).Repaint;
  end;
end;

procedure Tsg3DNavigator.SetStyleCamera(const Value: TGLCameraStyle);
begin
  if Value <> FCameraStyle then
  begin
    case Value of
      csPerspective, csOrthogonal:
        Camera.CameraStyle := Value;
    else
      raise Esg3DNavError.Create(sCameraStyleIsNoSupported);
    end;
    FCameraStyle := Value;
    FAxes.FGLCamera.CameraStyle := Value;
    if FCameraStyle = csPerspective then
    begin
      FDefCameraScale := cntDefPerspCameraScale;
      FAxes.FDefCameraScale := cntDefPerspAxisCameraScale;
    end
    else
    begin
      FDefCameraScale := cntDefOrthCameraScale;
      FAxes.FDefCameraScale := cntDefPerspAxisCameraScale / 2;
    end;
    DoResetCamera;
  end;
end;

procedure Tsg3DNavigator.ShowLights;
var
  I: Integer;
begin
  for I := 0 to LightsCount - 1 do
    Lights[I].Visible := True;
end;

procedure Tsg3DNavigator.ShowScene;
begin
  FGLDCScene.Visible := True;
  ShowLights;
  FGLDCTarget.Visible := True;
  Camera.Visible := True;
end;

procedure Tsg3DNavigator.Translate(const AVector: TAffineVector);
begin
  {FGLDCTarget}FGLDCScene.Position.Translate(AVector);
end;

procedure Tsg3DNavigator.UpdateArrows;
var
  vM: TMatrix4f;
begin
  FAxes.GLDCAxes.BeginUpdate;
  try
    FAxes.GLDCAxes.ResetRotations;
    vM := FGLDCScene.Matrix;
    {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NormalizeMatrix(vM);
    FAxes.GLDCAxes.Matrix := vM;
    FAxes.Camera.RotateObject(FAxes.GLDCAxes, 0, -180, 0);
  finally
    FAxes.GLDCAxes.EndUpdate;
  end;
end;

procedure Tsg3DNavigator.UpdateMaterialLibrary(const ADrawMode: TsgDXFDrawMode);
var
  I, J: Integer;
  vfg: TFGVertexIndexList;
  vMatLibItem: TGLLibMaterial;
  vGreyShader: TGLShader;
  S: string;

  procedure ApplyMaterialLibrary(AMaterialLib: TGLAbstractMaterialLibrary);
  var
    I: Integer;
    vSceneObj: TGLBaseSceneObject;
    vSgSceneObj: TsgGLLines;
  begin
    for I := FGLFreeForm.Count - 1 downto 1 do
    begin
      vSceneObj := FGLFreeForm.Children[I];
      if vSceneObj is TsgGLLines then
      begin
        vSgSceneObj := TsgGLLines(vSceneObj);
        vSgSceneObj.DestroyHandle;
        vSgSceneObj.Material.MaterialLibrary := AMaterialLib;
      end;
    end;
  end;

  procedure DoPrepareMaterialLibraryCache(AMaterialLib: TGLMaterialLibrary);
  var
    I, J: Integer;
    vfg: TFGVertexIndexList;
  begin
    for I := 0 to GLFreeForm.MeshObjects.Count - 1 do
      for J := 0 to GLFreeForm.MeshObjects[I].FaceGroups.Count - 1 do
      begin
        vfg := TFGVertexIndexList(GLFreeForm.MeshObjects[I].FaceGroups[J]);
        vfg.PrepareMaterialLibraryCache(AMaterialLib);
      end;
  end;

begin
  case ADrawMode of
    dmNormal:
      begin
        vGreyShader := nil;
        if Owner is Tsg3DDrawingNavigator then
          vGreyShader := Tsg3DDrawingNavigator(Owner).FGreyShader;
        for I := 0 to FGLMaterialLibrary.Materials.Count - 1 do
        begin
          vMatLibItem := TGLLibMaterial(FGLMaterialLibrary.Materials[I]);
          if vMatLibItem.Material.Texture.Enabled then
            vMatLibItem.Material.Texture.TextureFormat := tfDefault;
          if vMatLibItem.Shader = vGreyShader then
            vMatLibItem.Shader := nil;
        end;
        DoPrepareMaterialLibraryCache(FGLMaterialLibrary);
        ApplyMaterialLibrary(FGLMaterialLibrary);
      end;
    dmGray:
      begin
        vGreyShader := nil;
        if Owner is Tsg3DDrawingNavigator then
          vGreyShader := Tsg3DDrawingNavigator(Owner).FGreyShader;
        for I := 0 to FGLMaterialLibrary.Materials.Count - 1 do
        begin
          vMatLibItem := TGLLibMaterial(FGLMaterialLibrary.Materials[I]);
          if vMatLibItem.Material.Texture.Enabled then
            vMatLibItem.Material.Texture.TextureFormat := tfLuminance
          else
            vMatLibItem.Shader := vGreyShader;
        end;
        DoPrepareMaterialLibraryCache(FGLMaterialLibrary);
        { TODO: apply greyscale for lines }
        ApplyMaterialLibrary(nil);
      end;
  else
    vMatLibItem := AssignMaterial(clGray, FGLMaterialLibrary.Materials);
    for I := 0 to GLFreeForm.MeshObjects.Count - 1 do
      for J := 0 to GLFreeForm.MeshObjects[I].FaceGroups.Count - 1 do
      begin
        vfg := TFGVertexIndexList(GLFreeForm.MeshObjects[I].FaceGroups[J]);
        S := vfg.MaterialName;
        try
          vfg.MaterialName := vMatLibItem.Name;
          vfg.PrepareMaterialLibraryCache(FGLMaterialLibrary);
        finally
          vfg.MaterialName := S;
        end;
      end;
    ApplyMaterialLibrary(nil);
  end;
end;

function Tsg3DNavigator.GetFieldOfView: single;
begin
  if not Assigned(Buffer.Camera) then
    Result := 0
  else
    if Buffer.Width < Buffer.Height then
      Result := Buffer.Camera.GetFieldOfView(Buffer.Width)
    else
      Result := Buffer.Camera.GetFieldOfView(Buffer.Height);
end;

function Tsg3DNavigator.GetIsRenderingContextAvailable: Boolean;
begin
  Result := Buffer.RCInstantiated and Buffer.RenderingContext.IsValid;
end;

procedure Tsg3DNavigator.SetFieldOfView(const Value: single);
begin
  if Assigned(Buffer.Camera) then
  begin
    if Buffer.Width < Buffer.Height then
      Buffer.Camera.SetFieldOfView(Value, Buffer.Width)
    else
      Buffer.Camera.SetFieldOfView(Value, Buffer.Height);
  end;
end;

procedure Tsg3DNavigator.DoBufferStructuralChange(Sender: TObject);
begin
  inherited DoBufferStructuralChange(Sender);
end;

procedure Tsg3DNavigator.DoBeforeRender(Sender: TObject);
begin
  SetupVSync(VSync);
end;

procedure Tsg3DNavigator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (Buffer <> nil) then
  begin
    if (AComponent = Buffer.Camera) then
      Buffer.Camera := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

{ Tsg3DDrawingNavigator }

//procedure Tsg3DDrawingNavigator.AddVertexes(const P1, P2, P3: TFPoint);
//begin
//  AddVertex(P1);
//  AddVertex(P2);
//  AddVertex(P3);
//end;

procedure Tsg3DDrawingNavigator.UpdateMeasure;
var
  I: Integer;
  vEdgesBox: TsgGLLines;
  vDim: TsgGLPolyline;
  vDis, vRadius: Double;
  vPoint: TFPoint;
begin
  if Assigned(FNav3D.FDimensionList) then
  begin
    for I := 0 to FNav3D.FDimensionList.Count - 1 do
    begin
      vDim := TsgGLPolyline(TsgPickObject(FNav3D.FDimensionList[I]).Obj);
      vDim.FTextList.Count := 0;
      if vDim.FType = 7 then
      begin
        vPoint := SubFPoint(vDim.List[0], FBoxOffs);
        GetPolylineText(
          'X=' + DoubleToStringWithPrecision(vPoint.X * MeasureScaleFactor, MeasurePrecisionFactor) + ' ' +
          ' Y=' + DoubleToStringWithPrecision(vPoint.Y * MeasureScaleFactor, MeasurePrecisionFactor) + ' ' +
          ' Z=' + DoubleToStringWithPrecision(vPoint.Z * MeasureScaleFactor, MeasurePrecisionFactor) + ' ',
          vDim.FTextList);
      end
      else if vDim.Count <= 2 then
      begin
        vDis := vDim.GetDistance;
        GetPolylineText(DoubleToStringWithPrecision(vDis, MeasurePrecisionFactor),
           vDim.FTextList);
       end
       else
       begin
         vRadius := vDim.Radius;
         GetPolylineText('R ' + DoubleToStringWithPrecision(vRadius, MeasurePrecisionFactor),
           vDim.FTextList);
       end;
       TsgPickObject(FNav3D.FDimensionList[I]).FPoly.FTextList.Assign(vDim.FTextList);
    end;
  end;
  if Assigned(FNav3D.FBoxLine) then
  begin
    if FNav3D.FBoxLine.Count >= 2 then
    begin
      vEdgesBox := TsgGLLines(FNav3D.FBoxLine.Children[1]);
      for I := 0 to vEdgesBox.FPolylines.Count - 1 do
      begin
        vDim := TsgGLPolyline(vEdgesBox.FPolylines[I]);
        if vDim.FTextList.Count > 0 then
        begin
          vDim.FTextList.Count := 0;
          vDis := vDim.GetDistance;
          GetPolylineText(DoubleToStringWithPrecision(vDis, MeasurePrecisionFactor),
             vDim.FTextList);
        end;
      end;
    end;
  end;
end;
{$IFDEF CLIP_CSG_GLMESH_SGMESH}
procedure Tsg3DDrawingNavigator.UnBoxing(const AType: Integer; const ADelt: Double);
type
  TsgRecurceData = record
    List: TList;
    Center: TFPoint;
    Normal: TFPoint;
    _Type: Integer;
    R: Double;
    H: Double
  end;
  PsgRecurceData = ^TsgRecurceData;

const
  cnstMaxDepthRecurce: Integer = 20;
var
  I: Integer;
  vBox: TFRect;
  //vNode: TCustomNode;
  vCenterBox: TFPoint;
  vMax, vDelt, vR: Double;
  vPoly: TsgGLPolyline;
  vRecurceData, vRecurceData2: TsgRecurceData;
  vDepth: Integer;

  procedure TranslateNode(const AMeshList: TsgObjectList;
    const ARecureceData: PsgRecurceData; const ANewPoint: TFPoint);
  var
    I, J: Integer;
    vDelta: TAffineVector;
    vMesh, vMeshTemp: TsgMeshObject;
    vVisiblePack: Boolean;
    vEdgeIndexListData: TIntegerList;
    vReader: TBinaryReader;
    vLineOuterCurrent : TsgGLLines;
  begin
    vDelta := FPoint2Vect(ANewPoint);

    for I := 0 to AMeshList.Count - 1 do
    begin
      vMesh:= TsgMeshObject(AMeshList[I]);
      if Assigned(vMesh) then
      begin
        vVisiblePack := False;
        vLineOuterCurrent := FNav3D.FLineOuterWire;
        if vMesh.IsUseVisibleData then
        begin
          vMesh.RestoreVisibleData;
          if Assigned(FNav3D.FLineOuterWireOld) then
            vLineOuterCurrent := FNav3D.FLineOuterWireOld;
          vMesh.AddConnectionWithEdges(vLineOuterCurrent);
          vVisiblePack := True;
        end;
        vMesh.Translate(vDelta);
        //Translate edges
        begin
          for J := 0 to vMesh.FEdgeIndexList.Count - 1 do
          begin
            vPoly := TsgGLPolyline(vLineOuterCurrent.FPolylines[vMesh.FEdgeIndexList[J]]);
            if vPoly <> nil then
            begin
              if (vPoly.FCountUses >= 0) and (vPoly.FCountUses < 10000) then
              begin
                vPoly.Transform(FMatByTranslate(ANewPoint));
                vPoly.FCountUses := vPoly.FCountUses + 10000;
              end;
            end;
          end;
        end;
        if vVisiblePack then
        begin
          vMesh.SaveVisibleData;
          vMesh.Clear;
          vMesh.Visible := False;
          vMesh.BreakConnectionWithEdges(vLineOuterCurrent);
        end;

{$IFDEF CLIP_CSG_GLMESH_SGMESH}
        // Process the original meshes if there is a cross-section
        if Assigned(FNav3D.FLineOuterWireOld) then
        begin
          vMeshTemp := TsgMeshObject.CreateWithEnt(nil, nil);
          try
            vMeshTemp.Assign(vMesh);

            vMeshTemp.Clear;
            vMesh.FOriginalData.Position := 0;
            vReader := TBinaryReader.Create(vMesh.FOriginalData);
            try
              vMeshTemp.ReadFromFiler(vReader);
            finally
              vReader.Free;
            end;
            vMeshTemp.Translate(vDelta);
            vMeshTemp.SaveData;
            vMeshTemp.FOriginalData.Position := 0;
            vMesh.FOriginalData.Clear;
            vMesh.FOriginalData.CopyFrom(vMeshTemp.FOriginalData, 0);

            vEdgeIndexListData := TIntegerList.Create;
            try
              vMeshTemp.GetEdgeIndexListByData(vEdgeIndexListData);
              for J := 0 to vEdgeIndexListData.Count - 1 do
              begin
                vPoly := TsgGLPolyline(FNav3D.FLineOuterWireOld.FPolylines[vEdgeIndexListData[J]]);
                if vPoly <> nil then
                begin
                  if (vPoly.FCountUses >= 0) and (vPoly.FCountUses < 10000) then
                  begin
                    vPoly.Transform(FMatByTranslate(ANewPoint));
                    vPoly.FCountUses := vPoly.FCountUses + 10000;
                  end;
                end;
              end;
            finally
              vEdgeIndexListData.Free;
            end;
          finally
            vMeshTemp.Free;
          end;
        end;
{$ENDIF}

      end;
    end;
  end;

  procedure ApplyNode(const ANode: TCustomNode;
    const ARecureceData: PsgRecurceData);
  var
    vMeshList: TsgObjectList;
    vCenter, vNewPoint, vB: TFPoint;
    vBox: TFRect;
    vDist: Double;
    delta: TAffineVector;
    vProjectionPoint: TFPoint;
  begin
    vMeshList := TsgObjectList.Create;
    try
      ANode.GetMeshList(vMeshList);
      vBox := ANode.GetBox;

      case AType of
        1: begin
          vCenter := SubFPoint(GetCenterOfRect(vBox), FBoxOffs);
          vDist := DistanceFPoint(ARecureceData^.Center, vCenter);
          vNewPoint := GetPointOnLine(ARecureceData^.Center, vCenter,
            vDist + ARecureceData^.H * (vDist / ARecureceData^.R));
          vNewPoint := SubFPoint(vNewPoint, vCenter);
          delta := FPoint2Vect(vNewPoint);
          vNewPoint := SubFPoint(vNewPoint, Vect2FPoint(ANode.Delta));
          ANode.Delta := delta;
        end;
        2,3,4: begin
          vCenter :=  SubFPoint(GetCenterOfRect(vBox), FBoxOffs);
          RayCastPlaneIntersect(vCenter, cnstXOrtAxis, ARecureceData^.Center, cnstXOrtAxis,@vProjectionPoint);
          case AType of
            2: RayCastPlaneIntersect(vCenter, cnstXOrtAxis,ARecureceData^.Center, cnstXOrtAxis,@vProjectionPoint);
            3: RayCastPlaneIntersect(vCenter, cnstYOrtAxis,ARecureceData^.Center, cnstYOrtAxis,@vProjectionPoint);
            4: RayCastPlaneIntersect(vCenter, cnstZOrtAxis,ARecureceData^.Center, cnstZOrtAxis,@vProjectionPoint);
          end;
          vB := vProjectionPoint;
          vDist := DistanceFPoint(vB, vCenter);
          vNewPoint := GetPointOnLine(vB, vCenter, vDist + ARecureceData^.H * (vDist / ARecureceData^.R));
          vNewPoint := SubFPoint(vNewPoint, vCenter);
          delta := FPoint2Vect(vNewPoint);
          vNewPoint := SubFPoint(vNewPoint, Vect2FPoint(ANode.Delta));
          ANode.Delta := delta;
        end
      end;

      TranslateNode(vMeshList, ARecureceData, vNewPoint);

    finally
      vMeshList.Free;
    end;
  end;

  procedure FlatListProcess(const ARecureceData: PsgRecurceData);
  var
    I: Integer;
    FList: TsgPointerList;
    vD: Double;
    vNode: TCustomNode;

    vMeshList: TsgObjectList;
    vCenter, vNewPoint, vB: TFPoint;
    vBox: TFRect;
    vDist: Double;
    delta: TAffineVector;
    vProjectionPoint: TFPoint;

    vPrevBox: TFRect;

  begin
    FList := TRoot(FRoot).CreateListByExploded(ARecureceData^._Type, True);
    vD := 0;
    vPrevBox := cnstFRectZero;

    for I := 0 to FList.Count - 1  do
    begin
      vNode := FList[I];
      vBox := vNode.GetBox;

      if TsgTypeComparer.CmpFPoint(GetSizeFRect(vPrevBox), GetSizeFRect(vBox)) <> 0 then
      begin
        vPrevBox := vBox;
        case AType of
          5: vD := vD + GetSizeFRect(vBox).X;
          6: vD := vD + GetSizeFRect(vBox).Y;
          7: vD := vD + GetSizeFRect(vBox).Z;
        end;
      end;

      vMeshList := TsgObjectList.Create;
      try
        vNode.GetMeshList(vMeshList);


        vCenter :=  SubFPoint(GetCenterOfRect(vBox), FBoxOffs);
        RayCastPlaneIntersect(vCenter, cnstXOrtAxis, ARecureceData^.Center, cnstXOrtAxis,@vProjectionPoint);
        case AType of
          5: RayCastPlaneIntersect(vCenter, cnstXOrtAxis,ARecureceData^.Center, cnstXOrtAxis,@vProjectionPoint);
          6: RayCastPlaneIntersect(vCenter, cnstYOrtAxis,ARecureceData^.Center, cnstYOrtAxis,@vProjectionPoint);
          7: RayCastPlaneIntersect(vCenter, cnstZOrtAxis,ARecureceData^.Center, cnstZOrtAxis,@vProjectionPoint);
        end;
        vB := vProjectionPoint;
        vDist := DistanceFPoint(vB, vCenter);
        vNewPoint := GetPointOnLine(vB, vCenter, vD+3);
        vNewPoint := SubFPoint(vNewPoint, vCenter);
        delta := FPoint2Vect(vNewPoint);
        vNewPoint := SubFPoint(vNewPoint, Vect2FPoint(vNode.Delta));
        vNode.Delta := delta;

        TranslateNode(vMeshList, ARecureceData, vNewPoint);

      finally
        vMeshList.Free;
      end;

//      if IsBadRect(vPrevBox)  then
//      begin
//        vPrevBox := vBox;
//        case AType of
//          2: vD := vD + GetSizeFRect(vBox).X;
//          3: vD := vD + GetSizeFRect(vBox).Y;
//          4: vD := vD + GetSizeFRect(vBox).Z;
//        end;
//      end
//      else
//      begin
//        if TsgTypeComparer.CmpFPoint(GetSizeFRect(vPrevBox), GetSizeFRect(vBox)) <> 0 then
//        begin
//          vPrevBox := vBox;
//          case AType of
//            2: vD := vD + GetSizeFRect(vBox).X;
//            3: vD := vD + GetSizeFRect(vBox).Y;
//            4: vD := vD + GetSizeFRect(vBox).Z;
//          end;
//        end;
//      end;

    end;

  end;

  procedure LoopNode(const ANode: TCustomNode;
    const ARecureceData: PsgRecurceData; var ADepth: Integer);
  var
    I: Integer;
    vDepth: Integer;
  begin
    vDepth := ADepth + 1;
    if vDepth > cnstMaxDepthRecurce then
    begin
      ARecureceData^.List.Add(ANode);
      Exit;
    end;

    if not Assigned(ANode) then
      Exit;
    if ANode.FObj is TsgModPartCompound then
    begin
      if TsgModPartCompound(ANode.FObj).PartType = ptSheet then
      begin
        ApplyNode(ANode, ARecureceData);
        Exit;
      end;
    end;
    if (ANode.FObj is TsgModTopoSolid) or (ANode.FObj is TsgModTopoShell) then
    begin
      ApplyNode(ANode, ARecureceData);
    end
    else
    begin
      for I := 0 to ANode.Count - 1 do
      begin
        LoopNode(TCustomNode(ANode.Items[I]), ARecureceData, vDepth);
      end;
    end;
  end;

begin
  vBox := TCustomNode(FRoot).GetBox;
  vR := 1;
  vDelt := 0;

  case AType of
    1: begin
      vMax := DistanceFPoint(vBox.TopLeft, vBox.BottomRight)*3;
      vDelt := vMax * (ADelt / 100);
      vR := DistanceFPoint(vBox.TopLeft, vBox.BottomRight)/2;
      vCenterBox := SubFPoint(GetCenterOfRect(vBox), FBoxOffs);
    end;
    2,3,4: begin
      vMax := DistanceFPoint(vBox.TopLeft, vBox.BottomRight)*5;
      vDelt := vMax * (ADelt / 100);
//      case AType of
//        2: vCenterBox := SubFPoint(vBox.TopLeft , FBoxOffs);
//        3: vCenterBox := SubFPoint(vBox.TopLeft , FBoxOffs);
//        4: vCenterBox := SubFPoint(MakeFPoint(vBox.Left, vBox.Bottom, vBox.Z1) , FBoxOffs);
//      end;

      vCenterBox := SubFPoint(GetCenterOfRect(vBox), FBoxOffs);

      case AType of
        2: vR := DistanceFPoint(vBox.TopLeft, MakeFPoint(vBox.Right, vBox.Top, vBox.Z1));
        3: vR := DistanceFPoint(vBox.TopLeft, MakeFPoint(vBox.Left, vBox.Top, vBox.Z2));
        4: vR := DistanceFPoint(vBox.TopLeft, MakeFPoint(vBox.Left, vBox.Bottom, vBox.Z1));
      end;
    end
  end;

  if vR = 0 then
    vR := 1;

  vRecurceData.Center := vCenterBox;
  vRecurceData.Normal := cnstFPointZero;
  vRecurceData._Type := AType;
  vRecurceData.R  := vR;
  vRecurceData.H := vDelt;
  vRecurceData.List := TList.Create;
  vRecurceData2 := vRecurceData;
  vRecurceData2.List := TList.Create;
  try
    case AType of
      1..4:
      begin

        vDepth := 0;
        LoopNode(TCustomNode(FRoot), @vRecurceData, vDepth);

        I := 0;
        while vRecurceData.List.Count > 0 do
        begin
          vDepth := 0;
          LoopNode(TCustomNode(vRecurceData.List[I]), @vRecurceData2, vDepth);
          Inc(I);
          if I = vRecurceData.List.Count then
          begin
            if vRecurceData2.List.Count > 0 then
            begin
              vRecurceData.List.Assign(vRecurceData2.List);
              vRecurceData2.List.Clear;
              I := 0;
            end
            else
              vRecurceData.List.Clear;
          end;
        end;
      end;
      5..7: FlatListProcess(@vRecurceData);
    end;

  finally
    vRecurceData.List.Free;
    vRecurceData2.List.Free;
  end;

  if Assigned(Navigator3D.FLineOuterWire) then
  begin
    for I := 0 to Navigator3D.FLineOuterWire.FPolylines.Count - 1 do
    begin
      vPoly := TsgGLPolyline(FNav3D.FLineOuterWire.FPolylines[I]);
      if vPoly <> nil then
      begin
        if vPoly.FCountUses >= 10000 then
          vPoly.FCountUses := vPoly.FCountUses - 10000;
      end;
    end;
  end;

  if Assigned(Navigator3D.FLineOuterWireOld) then
  begin
    for I := 0 to Navigator3D.FLineOuterWireOld.FPolylines.Count - 1 do
    begin
      vPoly := TsgGLPolyline(FNav3D.FLineOuterWireOld.FPolylines[I]);
      if vPoly <> nil then
      begin
        if vPoly.FCountUses >= 10000 then
          vPoly.FCountUses := vPoly.FCountUses - 10000;
      end;
    end;
  end;

end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.UpdateMaterialLibrary;
begin
  FNav3D.UpdateMaterialLibrary(FDrawMode);
end;

procedure Tsg3DDrawingNavigator.UpdateVertexIndexList(
  const AVertexIndexList: TsgIntegerList);
var
  I: Integer;
begin
  if (FCurrMeshObject = nil) or (FCurrVertexes = nil) then Exit;
  if AVertexIndexList.Count > 0 then
  begin
    FCurrVertexes.VertexIndices.Clear;
    for I := 0 to AVertexIndexList.Count - 1 do
      FCurrVertexes.Add(AVertexIndexList[I]);
  end;
end;

function Tsg3DDrawingNavigator.AlterScale(Factor: Double; IsAbsolute: Boolean;
  Position: TPoint): Boolean;
var
  vRayStart, vRayVector, vRayStartNew: {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.TVector;
begin
  if IsAbsolute then Factor := Factor / Scale;
  if FNav3D.CameraStyle = csOrthogonal then
  begin
    SetVector(vRayStart, ClientToWorld(Position.X, Position.Y, 0));
    SetVector(vRayVector, FNav3D.Camera.AbsoluteVectorToTarget);
    NormalizeVector(vRayVector);
    FNav3D.Camera.SceneScale := FNav3D.Camera.SceneScale * Factor;
    SetVector(vRayStartNew, ClientToWorld(Position.X, Position.Y, 0));
    VectorSubtract(vRayStartNew, vRayStart, vRayStartNew);
    Translate(AffineVectorMake(vRayStartNew));
  end
  else
  begin
    { TODO: AlterScale for CameraStyle = csPerspective }
    FNav3D.Camera.SceneScale := FNav3D.Camera.SceneScale * Factor;
    ScenePositionChange(Self);
  end;
  Result := True;
end;

procedure Tsg3DDrawingNavigator.ApplyShowingStyleToMaterial(AMaterial: TGLMaterial);
begin
  if FNav3D = nil then Exit;
  if ssLighting in FShowingStyle then
    FNav3D.Buffer.Lighting := True
  else
    FNav3D.Buffer.Lighting := False;
  if ssFlatShading in FShowingStyle then
  begin
    FNav3D.Buffer.ShadeModel := smFlat;
  end;

  if ssSmoothShading in FShowingStyle then
  begin
    FNav3D.Buffer.ShadeModel := smSmooth;
  end;

  if (ssHiddenLines in FShowingStyle) or (ssWireframe in FShowingStyle) then
    AMaterial.PolygonMode := pmLines
  else
    AMaterial.PolygonMode := pmFill;
end;

procedure Tsg3DDrawingNavigator.AppShowingStyle;
begin
  ShowingStyle := FShowingStyle;
end;

procedure Tsg3DDrawingNavigator.AssignMaterialForMesh(const Mesh: TMeshObject;
  const AMaterial: TColor);
var
  I: Integer;
begin
  if Mesh = nil then Exit;
  for I := 0 to Mesh.FaceGroups.Count - 1 do
    (Mesh.FaceGroups[I] as TFGVertexIndexList).MaterialName :=
      AssignMaterial(AMaterial, FNav3D.FGLMaterialLibrary);
  //Mesh.Cl
end;

function Tsg3DDrawingNavigator.AddVertices(const APoints: TFPointList): Integer;
begin
  if (FCurrMeshObject <> nil) and (FCurrVertexes <> nil) then
  begin
    Result := 0;
    while Result < APoints.Count do
    begin
      FCurrVertexes.Add(FCurrMeshObject.Vertices.Add(GetPointf(APoints[Result])));
      Inc(Result);
    end;
  end
  else
    Result := 0;
end;

{$IFDEF SG_OBB}
function SV_Decomp(A : TsgMatrix3d; var S: TsgVector3d; var V: TsgMatrix3d): Boolean;
var
  I, J, K, L, I1, K1, L1, Mn, Its           : Integer;
  C, F, G, H, T, X, Y, Z, Tst1, Tst2, Scale : Double;
  R                                         : TsgVector3d;
  FlagTest: Boolean;

  function DSgn(A, B : Double) : Double;
  begin
    if B < 0.0 then
      Result := - Abs(A)
    else
      Result := Abs(A);
  end;

  function Pythag(X, Y : Double) : Double;
  { Computes Sqrt(X^2 + Y^2) without destructive underflow or overflow }
  var
    AbsX, AbsY : Double;
  begin
    AbsX := Abs(X);
    AbsY := Abs(Y);
    if AbsX > AbsY then
      Pythag := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
    else if AbsY = 0.0 then
      Pythag := 0.0
    else
      Pythag := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
  end;
begin
  Result := True;
  Scale := 0.0;
  G := 0.0;
  X := 0.0;

  { Householder reduction to bidiagonal form }
  for I := 0 to 2 do
  begin
    L := I + 1;
    R.V[I] := Scale * G;
    G := 0.0;
    T := 0.0;
    Scale := 0.0;
    for K := I to 2 do
      Scale := Scale + Abs(A.V[K].V[I]);

    if Scale <> 0.0 then
    begin
      for K := I to 2 do
      begin
        A.V[K].V[I] := A.V[K].V[I] / Scale;
        T := T + Sqr(A.V[K].V[I]);
      end;

      F := A.V[I].V[I];
      G := - DSgn(Sqrt(T), F);
      H := F * G - T;
      A.V[I].V[I] := F - G;
      if I < 2 then
      begin
        for J := L to 2 do
        begin
          T := 0.0;
          for K := I to 2 do
            T := T + A.V[K].V[I] * A.V[K].V[J];
          F := T / H;
          for K := I to 2 do
            A.V[K].V[J] := A.V[K].V[J] + F * A.V[K].V[I];
        end;
      end;
      for K := I to 2 do
        A.V[K].V[I] := Scale * A.V[K].V[I];
    end;
    S.V[I] := Scale * G;
    G := 0.0;
    T := 0.0;
    Scale := 0.0;
    if not ((I > 2) or (I = 2)) then
    begin
      for K := L to 2 do
        Scale := Scale + Abs(A.V[I].V[K]);

      if Scale <> 0.0 then
      begin
        for K := L to 2 do
        begin
          A.V[I].V[K] := A.V[I].V[K] / Scale;
          T := T + Sqr(A.V[I].V[K]);
        end;

        F := A.V[I].V[L];
        G := - DSgn(Sqrt(T), F);
        H := F * G - T;
        A.V[I].V[L] := F - G;

        for K := L to 2 do
          R.V[K] := A.V[I].V[K] / H;

        if I < 2 then
        begin
          for J := L to 2 do
          begin
            T := 0.0;
            for K := L to 2 do
              T := T + A.V[J].V[K] * A.V[I].V[K];
            for K := L to 2 do
              A.V[J].V[K] := A.V[J].V[K] + T * R.V[K];
          end;
        end;
        for K := L to 2 do
          A.V[I].V[K] := Scale * A.V[I].V[K];
      end;
    end;
    X := Max(X, Abs(S.V[I]) + Abs(R.V[I]));
  end;

  { Accumulation of right-hand transformations }
  for I := 2 downto 0 do
  begin
    if not (I = 2) then
    begin
      if G <> 0.0 then
      begin
        for J := L to 2 do
          { Double division avoids possible underflow }
          V.V[J].V[I] := (A.V[I].V[J] / A.V[I].V[L]) / G;

        for J := L to 2 do
        begin
          T := 0.0;
          for K := L to 2 do
            T := T + A.V[I].V[K] * V.V[K].V[J];
          for K := L to 2 do
            V.V[K].V[J] := V.V[K].V[J] + T * V.V[K].V[I];
        end;
      end;
      for J := L to 2 do
      begin
        V.V[I].V[J] := 0.0;
        V.V[J].V[I] := 0.0;
      end;
    end;
    V.V[I].V[I] := 1.0;
    G := R.V[I];
    L := I;
  end;


  { Accumulation of left-hand transformations }
  Mn := 2;

  for I := Mn downto 0 do
  begin
    L := I + 1;
    G := S.V[I];
    if I <> 2 then
    begin
      for J := L to 2 do
        A.V[I].V[J] := 0.0;
    end;
    if G <> 0.0 then
    begin
      if I <> Mn then
      begin
        for J := L to 2 do
        begin
          T := 0.0;

          for K := L to 2 do
            T := T + A.V[K].V[I] * A.V[K].V[J];

          { Double division avoids possible underflow }
          F := (T / A.V[I].V[I]) / G;

          for K := I to 2 do
            A.V[K].V[J] := A.V[K].V[J] + F * A.V[K].V[I];
        end;
      end;
      for J := I to 2 do
        A.V[J].V[I] := A.V[J].V[I] / G;
      A.V[I].V[I] := A.V[I].V[I] + 1.0;
      Continue;
    end;
    for J := I to 2 do
      A.V[J].V[I] := 0.0;

    A.V[I].V[I] := A.V[I].V[I] + 1.0;
  end;

  { Diagonalization of the bidiagonal form }
  Tst1 := X;
  for K := 2 downto 0 do
  begin
    K1 := K - 1;
    Its := 0;
    while True do
    begin
      { Test for splitting }
      FlagTest := True;
      for L := K downto 0 do
      begin
        L1 := L - 1;
        Tst2 := Tst1 + Abs(R.V[L]);
        if Tst2 = Tst1 then
        begin
         FlagTest := False;
         Break;
        end;
        { R[Lb] is always zero, so there is no exit
            through the bottom of the loop  }
        Tst2 := Tst1 + Abs(S.V[L1]);
        if Tst2 = Tst1 then
          Break;
      end;
      if FlagTest then
      begin
        { Cancellation of R[L] if L greater than 1 }
        C := 0.0;
        T := 1.0;

        for I := L to K do
        begin
          F := T * R.V[I];
          R.V[I] := C * R.V[I];
          Tst2 := Tst1 + Abs(F);
          if Tst2 = Tst1 then
            Break;
          G := S.V[I];
          H := Pythag(F, G);
          S.V[I] := H;
          C := G / H;
          T := - F / H;

          for J := 0 to 2 do
          begin
            Y := A.V[J].V[L1];
            Z := A.V[J].V[I];
            A.V[J].V[L1] := Y * C + Z * T;
            A.V[J].V[I] := - Y * T + Z * C;
          end;
        end;
      end;

      { Test for convergence }
      Z := S.V[K];
      if L = K then
        Break;

      if Its = 30 then
      begin
        Result := False;
        Exit;
      end;

      { Shift from bottom 2 by 2 minor }
      Its := Its + 1;
      X := S.V[L];
      Y := S.V[K1];
      G := R.V[K1];
      H := R.V[K];
      F := 0.5 * (((G + Z) / H) * ((G - Z) / Y) + Y / H - H / Y);
      G := Pythag(F, 1.0);
      F := X - (Z / X) * Z + (H / X) * (Y / (F + DSgn(G, F)) - H);

      { Next QR transformation }
      C := 1.0;
      T := 1.0;

      for I1 := L to K1 do
      begin
        I := I1 + 1;
        G := R.V[I];
        Y := S.V[I];
        H := T * G;
        G := C * G;
        Z := Pythag(F, H);
        R.V[I1] := Z;
        C := F / Z;
        T := H / Z;
        F := X * C + G * T;
        G := - X * T + G * C;
        H := Y * T;
        Y := Y * C;

        for J := 0 to 2 do
        begin
          X := V.V[J].V[I1];
          Z := V.V[J].V[I];
          V.V[J].V[I1] := X * C + Z * T;
          V.V[J].V[I] := - X * T + Z * C;
        end;

        Z := Pythag(F, H);
        S.V[I1] := Z;
        { Rotation can be arbitrary if Z is zero }
        if Z <> 0.0 then
        begin
          C := F / Z;
          T := H / Z;
        end;
        F := C * G + T * Y;
        X := - T * G + C * Y;

        for J := 0 to 2 do
        begin
          Y := A.V[J].V[I1];
          Z := A.V[J].V[I];
          A.V[J].V[I1] := Y * C + Z * T;
          A.V[J].V[I] := - Y * T + Z * C;
        end;
      end;

      R.V[L] := 0.0;
      R.V[K] := F;
      S.V[K] := X;
    end;

    { Convergence }
    if Z < 0.0 then
    begin

      { S[K] is made non-negative }
      S.V[K] := - Z;

      for J := 0 to 2 do
        V.V[J].V[K] := - V.V[J].V[K];
    end;
  end;
end;

function EigenSym(A      : TsgMatrix3d;
                  var Lambda : TsgVector3d;
                  var V      : TsgMatrix3d): Boolean;
var
  I, J, K : Integer;
  R       : Double;
begin
  Result := SV_Decomp(A, Lambda, V);

  if Result = False then Exit;

  { Sort eigenvalues and eigenvectors }
  for I := 0 to 1 do
    begin
      K := I;
      R := Lambda.V[I];
      for J := I+1 to 2 do
        if Lambda.V[J] > R then
          begin
            K := J;
            R := Lambda.V[J];
          end;

      SwapDoubles(Lambda.V[I], Lambda.V[K]);
      for J := 0 to 2 do
        SwapDoubles(V.V[J].V[I], V.V[J].V[K]);
    end;

  { Make sure that the first component of each eigenvector is > 0 }
  for J := 0 to 2 do
    if V.V[0].V[J] < 0.0 then
      for I := 0 to 2 do
        V.V[I].V[J] := - V.V[I].V[J];
end;

function PCA(R      : TsgMatrix3d;
              var Lambda : TsgVector3d;
              var C, Rc  : TsgMatrix3d): Boolean;
var
  I, J : Integer;
  Rac  : Double;
begin
  { Compute eigenvalues and eigenvectors of correlation matrix }
  C := NullMatrix3d;
  Rc:= NullMatrix3d;
  Lambda := NullVector3d;
  Result := EigenSym(R, Lambda, C);

  if Result = False then
    Exit;

  { Compute correlations between principal factors and reduced variables }
  for J := 0 to 2 do
  begin
    Rac := Sqrt(Lambda.V[J]);
    for I := 0 to 2 do
      Rc.V[I].V[J] := C.V[I].V[J] * Rac;
  end;
end;

function ComputeMean(PointsList: TsgVector3dList): TsgVector3d;
var
  i: Integer;
begin
  Result := Vector3dMake(0.0, 0.0, 0.0);
  for i := 0 to PointsList.Count - 1 do
    Result := VectorAdd(Result, PointsList[i]);
  Result := VectorScale(Result, 1/ PointsList.Count);
end;

function OuterProduct(const c: TsgVector3d; const r: TsgVector3d): TsgMatrix3d;
begin
  Result.V[0].X := c.X * r.X;
  Result.V[1].X := c.Y * r.X;
  Result.V[2].X := c.Z * r.X;
  Result.V[0].Y := c.X * r.Y;
  Result.V[1].Y := c.Y * r.Y;
  Result.V[2].Y := c.Z * r.Y;
  Result.V[0].Z := c.X * r.Z;
  Result.V[1].Z := c.Y * r.Z;
  Result.V[2].Z := c.Z * r.Z;
end;

function MatrixAdd(const a, b: TsgMatrix3d): TsgMatrix3d;
begin
  Result.V[0] := VectorAdd(a.V[0], b.V[0]);
  Result.V[1] := VectorAdd(a.V[1], b.V[1]);
  Result.V[2] := VectorAdd(a.V[2], b.V[2]);
end;

function MatrixDiv(const a: TsgMatrix3d; const d: Double): TsgMatrix3d;
begin
  Result.V[0] := VectorScale(a.V[0], 1/d);
  Result.V[1] := VectorScale(a.V[1], 1/d);
  Result.V[2] := VectorScale(a.V[2], 1/d);
end;

procedure Covariance(PointsList: TsgVector3dList; out mean: TsgVector3d; out cov: TsgMatrix3d);
var
  i: Integer;
  v: TsgVector3d;
begin
  mean := ComputeMean(PointsList);
  cov := NullMatrix3d;
  for i := 0 to PointsList.Count - 1 do
  begin
    v := VectorSubtract(PointsList[i], mean);
    cov := MatrixAdd(cov, OuterProduct(v, v));
  end;
  cov := MatrixDiv(cov,(PointsList.Count));
end;

function ComputePrincipalAxes(const AMesh: TsgObjectList; var vOBB: TOBB): Boolean;
var
  PointsList: TsgVector3dList;

  I, J, K: Integer;
  vMesh: TsgMeshObject;
  vList: TFGVertexIndexList;
  vVertxs: PAffineVectorArray;
  vIndxs: PIntegerArray;
  v1,v2,v3: TFPoint;


  mean: TsgVector3d;
  cov, r, m: TsgMatrix3d;
  Lam: TsgVector3d;
  orientedPoints: array[0..5] of TsgVector3d;
  d, min1, max1, min2, max2, min3, max3: Double;
  colM: TsgVector3d;
begin
  Result := False;
  PointsList := TsgVector3dList.Create;
  try
    for I := 0 to AMesh.Count - 1 do
    begin
      vMesh := TsgMeshObject(AMesh[I]);
      vVertxs := PAffineVectorArray(vMesh.Vertices.List);
      for J := 0 to vMesh.FaceGroups.Count - 1 do
      begin
        if vMesh.FaceGroups[J] is TFGVertexIndexList then
        begin
          vList := TFGVertexIndexList(vMesh.FaceGroups[J]);
          vIndxs := PIntegerArray(vList.VertexIndices.List);
          K := 0;
          while K < vList.VertexIndices.Count - 1 do
          begin
            v1 := Vect2FPoint(vVertxs^[vIndxs^[K]]);
            v2 := Vect2FPoint(vVertxs^[vIndxs^[K+1]]);
            v3 := Vect2FPoint(vVertxs^[vIndxs^[K+2]]);
            PointsList.Add(v1);
            PointsList.Add(v2);
            PointsList.Add(v3);
            Inc(K,3);
          end;
        end;
      end;
    end;

    if PointsList.Count < 2 then
    begin
      Exit;
    end;

    Covariance(PointsList, mean, cov);
    PCA(cov, Lam, m, r);
    TransposeMatrix(m);
    min1 := INFINITY; max1 := -INFINITY;
    min2 := INFINITY; max2 := -INFINITY;
    min3 := INFINITY; max3 := -INFINITY;
    d := 0.0;
    for I := 0 to PointsList.Count - 1 do
    begin
      colM := Vector3dMake(m.V[0].X, m.V[0].Y, m.V[0].Z);
      d := VectorDotProduct(colM, PointsList[I]);
      if min1 > d then min1 := d;
      if max1 < d then max1 := d;
      colM := Vector3dMake(m.V[1].X, m.V[1].Y, m.V[1].Z);
      d := VectorDotProduct(colM, PointsList[I]);
      if min2 > d then min2 := d;
      if max2 < d then max2 := d;
      colM := Vector3dMake(m.V[2].X, m.V[2].Y, m.V[2].Z);
      d := VectorDotProduct(colM, PointsList[I]);
      if min3 > d then min3 := d;
      if max3 < d then max3 := d;
    end;

    orientedPoints[0] := VectorScale(Vector3dMake(m.V[0].X, m.V[0].Y, m.V[0].Z), min1);
    orientedPoints[1] := VectorScale(Vector3dMake(m.V[0].X, m.V[0].Y, m.V[0].Z), max1);
    orientedPoints[2] := VectorScale(Vector3dMake(m.V[1].X, m.V[1].Y, m.V[1].Z), min2);
    orientedPoints[3] := VectorScale(Vector3dMake(m.V[1].X, m.V[1].Y, m.V[1].Z), max2);
    orientedPoints[4] := VectorScale(Vector3dMake(m.V[2].X, m.V[2].Y, m.V[2].Z), min3);
    orientedPoints[5] := VectorScale(Vector3dMake(m.V[2].X, m.V[2].Y, m.V[2].Z), max3);
  finally
    PointsList.Free;
  end;

  vOBB[0] := VectorAdd(VectorAdd(orientedPoints[0], orientedPoints[2]), orientedPoints[4]);
  vOBB[1] := VectorAdd(VectorAdd(orientedPoints[1], orientedPoints[2]), orientedPoints[4]);
  vOBB[2] := VectorAdd(VectorAdd(orientedPoints[1], orientedPoints[2]), orientedPoints[5]);
  vOBB[3] := VectorAdd(VectorAdd(orientedPoints[0], orientedPoints[2]), orientedPoints[5]);
  vOBB[4] := VectorAdd(VectorAdd(orientedPoints[0], orientedPoints[3]), orientedPoints[4]);
  vOBB[5] := VectorAdd(VectorAdd(orientedPoints[1], orientedPoints[3]), orientedPoints[4]);
  vOBB[6] := VectorAdd(VectorAdd(orientedPoints[1], orientedPoints[3]), orientedPoints[5]);
  vOBB[7] := VectorAdd(VectorAdd(orientedPoints[0], orientedPoints[3]), orientedPoints[5]);
  Result := True;
end;

//Version convexHul and FindMinimumBox
const
  EPS = 1e-9;

type
  PsgFace = ^TsgFace;
  PsgEdge = ^TsgEdge;

  TsgEdge = record
    rev: PsgEdge;
    f: PsgFace;
  end;

  TsgFace = record
    a, b, c: Integer;
    q: TsgVector3d;
    e1, e2, e3: PsgEdge;
    points: TsgMIntegerList;
    dead: Integer;
    public
      procedure Init(a_, b_, c_: Integer; q_: TsgVector3d);
      procedure Clear;
  end;

procedure TsgFace.Init(a_, b_, c_: Integer; q_: TsgVector3d);
begin
  a := a_;
  b := b_;
  c := c_;
  q := q_;
  e1 := nil;
  e2 := nil;
  e3 := nil;
  points := TsgMIntegerList.Create;
  dead := MaxInt;;
end;

procedure TsgFace.Clear;
begin
  points.Free;
end;

type
  TsgMersenne32 = class
  private type
    QWORD = UInt64;
  private const
     // Define MT19937 constants (32-bit RNG)
     N = 624;M = 397;R = 31;A = $9908B0DF;F = 1812433253;
     U = 11;S = 7;B = $9D2C5680;T = 15;C = $EFC60000;L = 18;
     MASK_LOWER = (QWORD(1) shl R) - 1;
     MASK_UPPER = QWORD(1) shl R;
     class var mt:array[0..N-1] of dword;
     class var index:word;
     class procedure twist;inline;static;
   public
     class constructor create;
     class procedure initialize(const seed:dword);inline;static;
     { 32 bit unsigned integer }
     class function URand:dword;inline;static;
     { 32 bit float in range 0..1 }
     class function URandf:single;inline;static;
     { 32 bit signed integer }
     class function SRand:integer;inline;static;
     {32 bit float in the range -1..1 }
     class function SRandf:single;inline;static;
   end;
  Rnd32 = TsgMersenne32;

class constructor TsgMersenne32.Create;
begin
  initialize(5489); // default seed for mt199937
end;

class procedure TsgMersenne32.Initialize(const seed:dword);
var
  i:dword;
begin
  mt[0] := seed;
 for  i := 1 to pred(N) do
   mt[i] := F * (mt[i - 1] xor (mt[i - 1] shr 30)) + i;
 index := N;
end;

class procedure TsgMersenne32.Twist;
var
  i:integer;
begin
  for i:=0 to N-M-1 do
    mt[i]:=mt[i+M] xor {twist} (((mt[i] and MASK_UPPER) or
    (mt[i+1] and MASK_LOWER)) shr 1)xor(dword(-(mt[i+1] and 1)) and A);
  for i:=N-M to N-2 do
    mt[i]:=mt[i+(M-N)]xor{twist}(((mt[i] and MASK_UPPER) or
    (mt[i+1] and MASK_LOWER)) shr 1)xor(dword(-(mt[i+1] and 1)) and A);
    mt[N-1]:=mt[M-1] xor {twist} (((mt[n-1] and MASK_UPPER) or (mt[0] and
    MASK_LOWER)) shr 1)xor(dword(-(mt[0] and 1)) and A);
  index:=0;
end;


class function TsgMersenne32.URand:dword;
var
  i:integer;
begin
  i := index;
  if  index >= N then
  begin
    Twist;
    i := index;
  end;
  Result := mt[i];index := i + 1;
  Result := Result xor (mt[i] shr U);
  Result := Result xor (Result shl S) and B;
  Result := Result xor (Result shl T) and C;
  Result := Result xor(Result shr L);
end;

class function TsgMersenne32.URandf:single;
begin
  Result := URand * 2.32830643653869628906e-10;
end;

class function TsgMersenne32.SRandf:single;
begin
  Result :=URand * 4.6566128730773926e-010;
end;

class function TsgMersenne32.SRand:integer;
begin
  Result := URand;
end;

procedure Shuffle(l: TsgVector3dList);
var
  i, ind: Integer;
  v: TsgVector3d;
begin
  for i := l.Count - 1 downto 0 do
  begin
    ind := Rnd32.URand mod (i + 1);
    v := l[i];
    l[i] := l[ind];
    l[ind] := v;
  end;
end;

procedure Prepare(p: TsgVector3dList);
var
  I, N, M: Integer;
  ve: TsgMIntegerList;
  ve2: TsgVector3dList;
  temp, temp1, temp2: TsgVector3d;
begin
  N := p.Count;
  Shuffle(p);
  ve := TsgMIntegerList.Create;
  try
    ve.Add(0);
    for I := 1 to N - 1 do
    begin
      M := ve.Count;
      case M of
        1: begin
             temp := VectorSubtract(p[ve[0]], p[i]);
             if sqrt(VectorDotProduct(temp, temp)) > EPS then
               ve.Add(I);
            end;
        2: begin
             temp1 := VectorSubtract(p[ve[1]], p[ve[0]]);
             temp2 := VectorSubtract(p[i], p[ve[0]]);
             temp := VectorCrossProduct(temp1, temp2);
             if sqrt(VectorDotProduct(temp, temp)) > EPS then
               ve.Add(I);
           end;
        else
             temp1 := VectorSubtract(p[ve[1]], p[ve[0]]);
             temp2 := VectorSubtract(p[ve[2]], p[ve[0]]);
             temp := VectorCrossProduct(temp1, temp2);
             temp1 := VectorSubtract(p[i], p[ve[0]]);
             if Abs(VectorDotProduct(temp1, temp)) > EPS then
             begin
               ve.Add(I);
               Break;
             end;
      end;
    end;
    if ve.Count = 4 then
    begin
      ve2 := TsgVector3dList.Create;
      try
        for I := 0 to ve.Count do
          ve2.Add(p[ve[i]]);
        ve.Flip;
        for I := 0 to ve.Count do
          p.Delete(ve[i]);
        for I := 0 to ve2.Count do
          p.Add(ve2[I]);
      finally
        ve2.Free;
      end;
    end;
  finally
    ve.Free;
  end;
end;

procedure Set_Union(Input1, Input2: TsgMIntegerList; Dest: TsgMIntegerList);
var
  I,J, K, Index2: Integer;
begin
  J := 0;
  I := 0;
  while I < Input1.Count do
  begin
    if J = Input2.Count then
    begin
      for K := I to Input1.Count - 1 do
        Dest.Add(Input1[K]);
      Exit;
    end;
    if Input2[J] < Input1[I] then
    begin
      Dest.Add(Input2[J]);
      Inc(J);
    end
    else
    begin
      Dest.Add(Input1[I]);
      if not (Input1[I] < Input2[J]) then
        Inc(J);
      Inc(I);
    end;
  end;
  for I := J to Input2.Count - 1 do
    Dest.Add(Input2[I]);
end;

type
   TLP = class(TList<PsgFace>)
   end;
   TLLP = class(TList<TList<PsgFace>>)
   end;

procedure hull3d(p: TsgVector3dList; faceIndices: TsgMIntegerList);
var
  I, J, j2, K, K_, M, N, Iter: Integer;
  face, new_face: TLP;
  conflict: TLLP;
  F1, F2: PsgFace;
  F, Fn: PsgFace;
  Q: Double;
  v, u: Integer;
  parr: array[0..2] of Integer;
  earr: array[0..3] of PsgEdge;

  GFaceList: TList<PsgFace>;
  GEdgeList: TList<PsgEdge>;

  tempPoints: TsgMIntegerList;

  function add_face(A, B, C: Integer): PsgFace;
  var
    temp1, temp2: TsgVector3d;
    Res: PsgFace;
  begin
    New(Res);
    GFaceList.Add(Res);
    temp1 := VectorSubtract(p[B], p[A]);
    temp2 := VectorSubtract(p[C], p[A]);
    Res.Init(A, B, C, VectorCrossProduct(temp1, temp2));
    face.Add(Res);
    Result := Res;
  end;

  procedure glue(const F1, F2: PsgFace; var E1, E2: PsgEdge);
  begin
    New(e1);
    New(e2);
    GEdgeList.Add(e1);
    GEdgeList.Add(e2);
    e1^.rev := e2;
    e2^.rev := e1;
    e1^.f := f2;
    e2^.f := f1;
  end;

begin
  N := p.Count;
  Prepare(p);

  tempPoints := TsgMIntegerList.Create;
  face := TLP.Create;
  new_face := TLP.Create;
  conflict := TLLP.Create;

  GFaceList := TList<PsgFace>.Create;
  GEdgeList := TList<PsgEdge>.Create;

  try

    for I := 0 to N - 1 do
      new_face.Add(nil);

    for I := 0 to N - 1 do
      conflict.Add(TLP.Create);


    F1 := add_face(0, 1, 2);
    F2 := add_face(0, 2, 1);
    glue(F1, F2, F1^.e1, F2^.e3);
    glue(F1, F2, F1^.e2, F2^.e2);
    glue(F1, F2, F1^.e3, F2^.e1);
    for I := 3 to N - 1 do
    begin
      for J := 0 to 1 do
      begin
        case J of
          0: F := F1;
          1: F := F2;
        end;

        Q := VectorDotProduct(VectorSubtract(p[i], p[F^.a]), F^.q);
        if Q > EPS then
          conflict[I].Add(F);
        if Q >= -EPS then
          F^.points.Add(i);
      end;
    end;

    for I := 3 to N - 1 do
    begin
        // mark all visible faces as dead
        for J := 0 to conflict[i].Count - 1 do
        begin
          F := conflict[i][j];
          F^.dead := minI(F^.dead, I);
        end;

        // If a dead face and alive face are adjacent, we have an exposed edge
        // Vertex v will be a vertex on some exposed edge
        v := -1;
        for M := 0 to conflict[i].Count - 1 do
        begin
          F := conflict[i][M];
          if F^.dead <> i then
            Continue;

          parr[0] := F^.a;
          parr[1] := F^.b;
          parr[2] := F^.c;

          earr[0] := F^.e1;
          earr[1] := F^.e2;
          earr[2] := F^.e3;

          if ((F^.e2 = nil) or (F^.e3 = nil)) then
          begin
            // Remove dead faces
            new_face.Count := 0;
            for K := 0 to face.Count - 1 do
            begin
              f := face[K];
              if f^.dead > N then
              begin
                new_face.Add(f);
              end;
            end;
            face.Count := new_face.Count;
            for K := 0 to new_face.Count - 1 do
              face[K] := new_face[K];
            faceIndices.Count := 0;
            for K := 0 to face.Count - 1 do
            begin
              f := face[K];
              faceIndices.Add(f^.a);
              faceIndices.Add(f^.b);
              faceIndices.Add(f^.c);
            end;
            Exit;

          end;

          for J := 0 to 2 do
          begin
            j2 := (j + 1);
            if (j2 >= 3) then
              j2 := j2 - 3;

            if(earr[j]^.f^.dead > i) then
            begin
              // F is dead and earr[j]->f is alive.
              // We should add a new face Fn, attach it to earr[j]->f,
              // combine the point lists of the two faces into Fn,
              // and store Fn in new_face[parr[j]] so we can glue all the new faces together in a cone.
              Fn := add_face(parr[j], parr[j2], i);
              new_face[parr[j]] := Fn;

              Set_Union(F^.points, earr[j]^.f^.points, Fn^.points);

              tempPoints.Count := 0;

              for K := 0 to Fn^.points.Count - 1 do
              begin
                K_ := Fn^.points[K];
                if (K_ > I) and
                  (VectorDotProduct(
                    VectorSubtract(p[K_], p[Fn^.a]), Fn^.q) > EPS
                  ) then
                begin
                  tempPoints.Add(K_);
                end;
              end;
              Fn^.points.Count := tempPoints.Count;
              for K := 0 to tempPoints.Count - 1 do
                Fn^.points[K] := tempPoints[K];

              for K := 0 to Fn^.points.Count - 1 do
              begin
                conflict[Fn^.points[K]].Add(Fn);
              end;

              earr[j]^.rev^.f := Fn;
              Fn^.e1 := earr[j];
              v := parr[j];
            end;
          end;
        end;
        // There are no exposed edges
        if(v = -1)  then
          Continue;

        // Glue all the new cone faces together
        while new_face[v]^.e2 = nil do
        begin
          u := new_face[v]^.b;
          glue(new_face[v], new_face[u], new_face[v]^.e2, new_face[u]^.e3);
          v := u;
        end;

        for K := 0 to new_face.Count - 1 do
        begin
          if new_face[K] = nil then
            Continue;
          if (new_face[K]^.e1 = nil) or (new_face[K]^.e2 = nil) or (new_face[K]^.e3 = nil) then
            new_face[K]^.dead := 0;
        end;

    end;
    // Remove dead faces
    new_face.Count := 0;
    for K := 0 to face.Count - 1 do
    begin
      f := face[K];
      if (f^.dead > N) then
      begin
        new_face.Add(f);
      end;
    end;
    face.Count := new_face.Count;
    for K := 0 to new_face.Count - 1 do
      face[K] := new_face[K];
    faceIndices.Count := 0;
    for K := 0 to face.Count - 1 do
    begin
      f := face[K];
      faceIndices.Add(f^.a);
      faceIndices.Add(f^.b);
      faceIndices.Add(f^.c);
    end;
  finally
    for I := 0 to GEdgeList.Count - 1 do
      Dispose(GEdgeList[I]);
    for I := 0 to GFaceList.Count - 1 do
    begin
      f := GFaceList[I];
      f.Clear;
      Dispose(f);
    end;
    GFaceList.Free;
    GEdgeList.Free;
    tempPoints.Free;
    face.Free;
    new_face.Free;
    for I := 0 to conflict.Count - 1 do
    begin
      conflict[I].Free;
    end;
    conflict.Free;
  end;
end;

function ComputeMBB(const AMesh: TsgObjectList; var vOBB: TOBB): Boolean;
var
  PointsList: TsgVector3dList;

  I, J, K: Integer;
  vMesh: TsgMeshObject;
  vList: TFGVertexIndexList;
  vVertxs: PAffineVectorArray;
  vIndxs: PIntegerArray;
  v1,v2,v3: TFPoint;

  m1: TsgMatrix4d;
  vBox: TF3DInterval;

  faceIndices: TsgMIntegerList;
  chIndexes: TList<Integer>;
  convexHull: TsgVector3dList;
  PrevValue: Integer;

begin
  Result := False;
  PointsList := TsgVector3dList.Create;
  try
    for I := 0 to AMesh.Count - 1 do
    begin
      vMesh := TsgMeshObject(AMesh[I]);
      vVertxs := PAffineVectorArray(vMesh.Vertices.List);
      for J := 0 to vMesh.FaceGroups.Count - 1 do
      begin
        if vMesh.FaceGroups[J] is TFGVertexIndexList then
        begin
          vList := TFGVertexIndexList(vMesh.FaceGroups[J]);
          vIndxs := PIntegerArray(vList.VertexIndices.List);
          K := 0;
          while K < vList.VertexIndices.Count - 1 do
          begin
            v1 := Vect2FPoint(vVertxs^[vIndxs^[K]]);
            v2 := Vect2FPoint(vVertxs^[vIndxs^[K+1]]);
            v3 := Vect2FPoint(vVertxs^[vIndxs^[K+2]]);
            PointsList.Add(v1);
            PointsList.Add(v2);
            PointsList.Add(v3);
            Inc(K,3);
          end;
        end;
      end;
    end;

    if PointsList.Count < 2 then
    begin
      Exit;
    end;


    faceIndices := TsgMIntegerList.Create;
    chIndexes := TList<Integer>.Create;
    convexHull := TsgVector3dList.Create;
    try
      hull3d(PointsList, faceIndices);

      chIndexes := TList<Integer>.Create;
      for i := 0 to faceIndices.Count - 1 do
        chIndexes.Add(faceIndices[i]);
      chIndexes.Sort;
      convexHull := TsgVector3dList.Create;
      convexHull.Add(PointsList.List^[chIndexes[0]]);
      PrevValue := chIndexes[0];
      for I := 0 to chIndexes.Count - 1 do
      begin
        if chIndexes[I] = PrevValue then
          Continue;
        PrevValue := chIndexes[I];
        convexHull.Add(PointsList.List^[PrevValue]);
      end;

      FindMinimumBox(convexHull.List, convexHull.Count, m1, vBox);
    finally
      convexHull.Free;
      chIndexes.Free;
      faceIndices.Free;
    end;

    for I := 0 to 7 do
    begin
      vOBB[I] := PointTransform(Corner(vBox, I), m1);
    end;
    Result := True;
  finally
    PointsList.Free;
  end;
end;



{$ENDIF}


procedure Tsg3DDrawingNavigator.UpdateBoxDimensions(const ANode: TCustomNode);
var
  vBox: TFRect;
{$IFDEF SG_OBB}
  vMeshList: TsgObjectList;
  vOBB: TOBB;
  vP_OBB: POBB;
{$ENDIF}
begin
{$IFDEF SG_ADD_BOX}
  if Assigned(ANode) then
  begin
{$IFDEF SG_ROTATE_CENTER_TEST}
    vBox := ANode.GetCurrentBox;
{$ELSE}
    vBox := ANode.GetBox;
{$ENDIF}

{$IFDEF SG_OBB}
    vMeshList := TsgObjectList.Create;
    try
      ANode.GetMeshList(vMeshList);
      vP_OBB := @vOBB;
{$IFDEF SG_OBB_MIN}
      if not ComputeMBB(vMeshList, vOBB) then
{$ELSE}
      if not ComputePrincipalAxes(vMeshList, vOBB) then
{$ENDIF}
        vP_OBB := nil;
    finally
      vMeshList.Free;
    end;
{$ENDIF}

    if TCustomNode(FRoot).IsExploded then
    begin
      if ANode.ConvertBoxIfExploded(TCustomNode(FRoot), vBox) then
        AddBoxDimensions(FNav3D.FBoxLine, @vBox, FShowBoxDimensions, FIsLoadedFromConv)
      else
        AddBoxDimensions(FNav3D.FBoxLine, @vBox, False, FIsLoadedFromConv);
    end
    else
      AddBoxDimensions(FNav3D.FBoxLine, @vBox, FShowBoxDimensions, FIsLoadedFromConv{$IFDEF SG_OBB}, vP_OBB{$ENDIF});
  end
  else
  begin
    if TCustomNode(FRoot).IsExploded then
      AddBoxDimensions(FNav3D.FBoxLine, nil, False, FIsLoadedFromConv)
    else
    begin
{$IFDEF SG_ROTATE_CENTER_TEST}
      vBox := TCustomNode(FRoot).GetCurrentBox;
      AddBoxDimensions(FNav3D.FBoxLine, @vBox, FShowBoxDimensions, FIsLoadedFromConv);
{$ELSE}
      AddBoxDimensions(FNav3D.FBoxLine, nil, FShowBoxDimensions, FIsLoadedFromConv);
{$ENDIF}
    end;
  end;
{$ENDIF}
end;


procedure Tsg3DDrawingNavigator.AddBoxDimensions(var ABoxLine: TsgGLLines;
  const ABox: PFRect; const AVisible: Boolean;
  const IsGetPoint: Boolean = True
  {$IFDEF SG_OBB}; const AOBB: POBB = nil{$ENDIF});

const
  cnstBoxEdge : array[0..11, 0..1] of Integer =
    ( (0,1) , (1, 2), (2, 3), (3, 0),
      (0,5) , (3, 4), (2, 7), (1, 6),
      (5,4) , (4, 7), (7, 6), (6, 5));
var
  I, IB, ID: Integer;
  V: Tsg3DCube;
  vRect: TFRect;
  vIsUpdate: Boolean;

  vEdgeBox, vEdgeDim:  TsgGLLines;

  procedure UpdateEdgeBox(const ALine: TsgGLPolyLine;
    APoint1, APoint2: TFPoint);
  var
    vDis: TsgFloat;
  begin
    ALine.List[0] := APoint1;
    ALine.List[1] := APoint2;
    if ALine.FTextList.Count > 0 then
    begin
      ALine.FTextList.Count := 0;
      vDis := DistanceFPoint(ALine.List[0], ALine.List[1]);
      ALine.FBegin := ALine.List[0];
      ALine.FEnd := ALine.List[1];
      GetPolylineText(DoubleToStringWithPrecision(vDis * MeasureScaleFactor, MeasurePrecisionFactor),
        ALine.FTextList);
    end;
  end;

  procedure AddEdgeBox(const AGLLines: TsgGLLines;
    APoint1, APoint2, ANormal: TFPoint; AIsAddDim: Boolean = False);
  var
    vLine: TsgGLPolyLine;
    vDis: TsgFloat;
  begin
     vLine := AGLLines.AddNode;
     vLine.LineWidth := FOuterWireLineWieght;
     vLine.MaterialName := AssignMaterial(Measure3DParams.BoxDimensionLineColor,
       FNav3D.FGLMaterialLibrary);
     vLine.MaterialNamePoint := AssignMaterial(Measure3DParams.SelectNodeColor,
       FNav3D.FGLMaterialLibrary);
    vLine.Add(APoint1);
    vLine.Add(APoint2);
    vLine.FNormal := ANormal;
    if AIsAddDim then
    begin
      vLine.FType := 0;
      vLine.LineWidth := Measure3DParams.DimLwd;
      vLine.FBegin := vLine.List[0];
      vLine.FEnd := vLine.List[1];
      vDis := DistanceFPoint(vLine.List[0], vLine.List[1]);
      GetPolylineText(DoubleToStringWithPrecision(vDis * MeasureScaleFactor, MeasurePrecisionFactor),
        vLine.FTextList);
      vLine.PointSize := Measure3DParams.MarkerSize;
      vLine.MaterialName :=
         AssignMaterial(Measure3DParams.BoxDimensionLineColor, FNav3D.FGLMaterialLibrary);
      vLine.MaterialNamePoint :=
         AssignMaterial(Measure3DParams.BoxDimensionLineColor, FNav3D.FGLMaterialLibrary);
      vLine.MaterialNameText :=
         AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRT), FNav3D.FGLMaterialLibrary);
      vLine.MaterialNameLine :=
         AssignMaterial(Measure3DParams.BoxDimensionLineColor, FNav3D.FGLMaterialLibrary);
      vLine.FrameColor := Measure3DParams.BoxFrameColor;
      vLine.ArrowSize := FDimByDrawing.ArrowSize;
    end;
  end;

begin
  if Assigned(FNav3D) then
  begin
    vIsUpdate := True;
    IB := 0;
    ID := 0;
    if not Assigned(ABoxLine) then
    begin
      //Root Object
      ABoxLine := TsgGLLines.CreateAsChild(FNav3D.GLDCScene);
      ABoxLine.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
      ABoxLine.ObjectStyle := ABoxLine.ObjectStyle +
       [osDirectDraw];
      //Contains cube edges without dimensions - drawn with depth
      vEdgeBox := TsgGLLines.CreateAsChild(ABoxLine);
      vEdgeBox.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
      vEdgeBox.ObjectStyle := vEdgeBox.ObjectStyle +
       [osDirectDraw];
      //Contains edge measurements - drawn without depth.
      vEdgeDim := TsgGLLines.CreateAsChild(ABoxLine);
      vEdgeDim.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
      vEdgeDim.ObjectStyle := vEdgeDim.ObjectStyle +
       [osIgnoreDepthBuffer,osDirectDraw];
      vIsUpdate := False;
      ABoxLine.MoveLast;
    end
    else
    begin
      vEdgeBox := TsgGLLines(ABoxLine.Children[0]);
      vEdgeDim := TsgGLLines(ABoxLine.Children[1]);
      ABoxLine.MoveLast;
    end;
    if Assigned(ABox) then
    begin
      vRect := ABox^;
    end
    else
    begin
      vRect := FNav3D.FBoxImage;
      vRect.TopLeft := SubFPoint(vRect.TopLeft, FBoxOffs);
      vRect.BottomRight := SubFPoint(vRect.BottomRight, FBoxOffs);
      if IsGetPoint then
      begin
        vRect.TopLeft := GetPoint(vRect.TopLeft);
        vRect.BottomRight := GetPoint(vRect.BottomRight);
      end;
    end;
{$IFDEF SG_OBB}
    if Assigned(AOBB) then
    begin

    V[0] := AOBB^[0];
    V[1] := AOBB^[3];
    V[2] := AOBB^[7];
    V[3] := AOBB^[4];

    V[4] := AOBB^[5];
    V[5] := AOBB^[1];
    V[6] := AOBB^[2];
    V[7] := AOBB^[6];

    end
    else
{$ENDIF}
      MakeCube(vRect, V);
    for I := Low(cnstBoxEdge) to High(cnstBoxEdge) do
    begin
      if vIsUpdate then
      begin
        case I of
         1,2,6:
         begin
           UpdateEdgeBox(TsgGLPolyLine(vEdgeDim.FPolylines[ID]),V[cnstBoxEdge[I, 0]],
                  V[cnstBoxEdge[I, 1]]);
           Inc(ID);
         end;
         else
         begin
           UpdateEdgeBox(TsgGLPolyLine(vEdgeBox.FPolylines[IB]),V[cnstBoxEdge[I, 0]],
             V[cnstBoxEdge[I, 1]]);
           Inc(IB);
         end;
        end;
      end
      else
      begin
        case I of
          1,2,6: AddEdgeBox(vEdgeDim,V[cnstBoxEdge[I, 0]],
                   V[cnstBoxEdge[I, 1]], cnstFPointZero, True);
          else
            AddEdgeBox(vEdgeBox, V[cnstBoxEdge[I, 0]],
              V[cnstBoxEdge[I, 1]], cnstFPointZero);
        end;
      end;
    end;
    ABoxLine.Visible := AVisible;
  end;
end;

procedure Tsg3DDrawingNavigator.AddMaterialsDimension;
begin
  AssignMaterial(Measure3DParams.MarkerHoverColor, FNav3D.FGLMaterialLibrary);
  AssignMaterial(Measure3DParams.SelectNodeColor, FNav3D.FGLMaterialLibrary);
  AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRT), FNav3D.FGLMaterialLibrary);
  AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRD), FNav3D.FGLMaterialLibrary);
  AssignMaterial(Measure3DParams.SelectNodeColor, FNav3D.FGLMaterialLibrary);
  AssignMaterial(Measure3DParams.EdgesColor, FNav3D.FGLMaterialLibrary);
end;

function Tsg3DDrawingNavigator.AddVertex(const Point: TFPoint;
  PNormal: PFPoint = nil): Integer;
begin
  if (FCurrMeshObject <> nil) and (FCurrVertexes <> nil) then
  begin
    Result := FCurrMeshObject.Vertices.Add(GetPointf(Point));
    FCurrVertexes.Add(Result);
    if PNormal <> nil then
      FCurrMeshObject.Normals.Add(FPoint2Vect(PNormal^))
  end
  else
    Result := -1;
end;

function Tsg3DDrawingNavigator.BeginUpdate: Integer;
begin
  FNav3D.Buffer.BeginUpdate;
  Result := FNav3D.Buffer.Updating;
  ImgLock;
end;

function Tsg3DDrawingNavigator.CheckCount(ALine: TsgGLPolyline;
  Count: Integer): Boolean;
begin
  if Count = 0 then
  begin
    Navigator3D.FLineMass.DeleteLastNode;
    Result := False;
  end
  else
    Result := True;
end;

function Tsg3DDrawingNavigator.CheckCurVertexes: Boolean;
begin
  Result := FCurrVertexes is TsgVertexList;
end;

procedure Tsg3DDrawingNavigator.Clear;
var
  vModeller: TsgModeller;
begin
  inherited Clear;
  //Clear select
  PickObject := nil;
  PickObjects.Objects.Clear;
  PickObjects.ClearLines;

  if not Navigator3D.IsEmpty then
    Navigator3D.Clear;
  ClearVisualization;
  FMoveDim := nil;
  LoadFromImage := nil;
  FIsLoadedFromConv := False;
  FIsHasTriangledMesh := False;
  FRoot.Clean;
end;

procedure Tsg3DDrawingNavigator.ClearVisualization;
var  vModeller: TsgModeller;
begin
  if Assigned(FConverter) then
  begin
    vModeller := TsgDXFConverterAccess(FConverter).GetModeller;
    if  Assigned(vModeller) then
    begin
      vModeller.OnClearVisualization := DropVisualization;
      vModeller.ClearVisualization;
    end;
    TCustomNode.ClearVisualization(TCustomNode(FRoot));
  end;
end;

function Tsg3DDrawingNavigator.ClientToWorld(const X, Y, Z: Double): TVector3f;
begin
  Result := FNav3D.ClientToWorld(X, Y, Z);
end;

function Tsg3DDrawingNavigator.ClientToWorld(const APosition: TF2DPoint): TVector3f;
begin
  Result := ClientToWorld(APosition.X, APosition.Y, 0);
end;

function Tsg3DDrawingNavigator.ClientToWorld(const APosition: TFPoint): TVector3f;
begin
  Result := ClientToWorld(APosition.X, APosition.Y, APosition.Z);
end;

function Tsg3DDrawingNavigator.ClientToWorld(X, Y: Double;
  AUCSPoint: PFPoint): TFPoint;
var
  vMatrix: TFMatrix;
  UCS: TFMatrix;
begin
  GetView(vMatrix);
  Result := CADUnProject(MakeFPoint(X, Y, 0), vMatrix);
  if Assigned(AUCSPoint) and Assigned(LoadFromImage) then
  begin
    AUCSPoint^ := Result;
    TsgCADImageAccess(LoadFromImage).TransformToUCS(AUCSPoint^);
  end;
end;

function Tsg3DDrawingNavigator.ColorToSceneColor(AColor: TColor): TColor;
begin
  Result := AColor;
  case Result of
    clNone: Result := FDefaultColor;
  end;
end;

function Tsg3DDrawingNavigator.CoordinateConvertion(ACoordX, ACoordY: Integer;
  var APointInUCS: TFPoint): TFPoint;
begin
  if (ACoordX = FMove.Pos.X) and (ACoordY = FMove.Pos.Y) and Assigned(FIntersectCache) then
  begin
    Result := SubFPoint(FIntersectCache^, FBoxOffs);
    APointInUCS := Result;
  end
  else
    Result := GetRealPoint(ACoordX, ACoordY, APointInUCS);
end;

constructor Tsg3DDrawingNavigator.Create(AOwner: TComponent);
var
  I: Integer;
  vPHVS: PsgHeadVarStruct;
  vDimStyle: TsgDXFDimensionStyle;

{$IFDEF SG_SHARED_VCL}
  vIndex: Integer;
  vTextStyle: TsgDXFStyle;
  //vMeasureStyle: TsgDXFDimensionStyle;
{$ENDIF}
begin
  inherited Create(AOwner);
{$IFDEF DEBUG_STRUCTURE}
    FTreeView := TTreeView.Create(Self);
    FTreeView.Parent := Self;
    FTreeView.ReadOnly := True;
    FTreeView.Left := 10;
    FTreeView.Top := 10;
    FTreeView.Color := clSilver;
    FTreeView.Align := alNone;
    FTreeView.Width := 600;
    FTreeView.Height := 500;
    FTreeView.BringToFront;
{$ENDIF}

  FHighlightSaveMatNames := TStringList.Create;
  FInsertStack := TsgInsertStack.Create;
  FTransformation := TTransformation.Create(@IterateParam.Matrix);
  FDrawMode := dmNormal;
  FMouseRotate := True;
  FNeedNormals := False;
  FIsHasTriangledMesh := False;
  FIsLoadedFromConv := False;
  FCurrMeshObject := nil;
  FCurrVertexes := nil;
  FVectorFile := nil;
  FIsBrepEntityIterate := False;
  FBoxOffs := cnstFPointZero;
  FHiddenLinesShader := TGLHiddenLineShader.Create(Self); //TsgHiddenLineShader.Create(Self);
  TGLHiddenLineShader(FHiddenLinesShader).Solid := True;
  // Line
  TGLHiddenLineShader(FHiddenLinesShader).FrontLine.Width := cnstLineWidthShader;
  // Line solid
  TGLHiddenLineShader(FHiddenLinesShader).BackLine.Width := cnstLineWidthShader;

  //Outline Stencil Shader
  FOutlineShader := TsgOutlineStencilShader.Create(Self);
  TsgOutlineStencilShader(FOutlineShader).LineWidth := FOuterWireLineWieght * 2;
  TsgOutlineStencilShader(FOutlineShader).LineColor.AsWinColor := FOuterWireColor;

  FGreyShader := TsgGreyShader.Create(Self);

  FNav3D := Tsg3DNavigator.Create(Self);
  FNav3D.BackgroundColor := Color;
  FNav3D.ShowAxis := True;
  FVisibleScene := True;
{$IFDEF USE_CLIP}
  FNav3D.ShowCubeExtents := False;
{$ELSE}
  FNav3D.ShowCubeExtents := False;
{$ENDIF}

{$IFDEF SG_ROTATE_CENTER_TEST}
  FIsRequestedDrawMatrix := False;
{$ENDIF}

  FNav3D.OnScenePositionChange := ScenePositionChange;
  FNav3D.OnAfterRender := Navigator3DAfterRender;
  FNav3D.OnBeforeRender := Navigator3DBeforeRender;
  FNav3D.OnWrapUpRender := DoOnWrapUpRender;
  DoOnCursorChanged;
//  ZoomInChange := 15.75 / 15;
//  ZoomOutChange := 15 / 15.75;
  FDefaultColor := clBlack;
  FShowEdges := bShowEdgesAfterLoad;
  FShowBoxDimensions := bShowBoxDimensions;
  if Orbit3D is Tsg3DDrwNavOrbit3D then
    Tsg3DDrwNavOrbit3D(Orbit3D).DegreesPerPixel := 1 / 3;
  ShowingStyle := [ssSmoothShading, ssLighting];
  FDeviationCoefficient := cnstModLinDeflection;
  ViewRectMode := True;

  FAccuracyDim := cnstAccuracyDim;

  FPickObjects := TsgPickObject.Create(Self);
  FPickObjects.Color := Measure3DParams.SelectEdgesAreaColor;
  FPickObjects.OnPick := PickObjectEvent;
  FPickObjects.Lines.ObjectStyle := FPickObjects.Lines.ObjectStyle +
    [osIgnoreDepthBuffer, osDirectDraw];
  FPickObjects.PickCached := True;
  FPickObjects.Enabled := True;
  FPickObjects.Lines.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;

  FHighLightObj := TsgPickObject.Create(Self);
  FHighLightObj.Color := Measure3DParams.SelectSurfaceColor;
  FHighLightObj.OnPick := HighLightObjectEvent;
  FHighLightObj.Lines.ObjectStyle := FHighLightObj.Lines.ObjectStyle +
    [osIgnoreDepthBuffer, osDirectDraw];
  FHighLightObj.Lines.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;

  for I := Low(FSnapObj) to High(FSnapObj) do
  begin
    FSnapObj[I] := TsgPickObject.Create(Self);
    FSnapObj[I].Color := Measure3DParams.EdgesHoverColor; //Measure3DParams.EdgesColor;
    FSnapObj[I].OnPick := SnapObjEvent;
    FSnapObj[I].Lines.ObjectStyle := FSnapObj[I].Lines.ObjectStyle +
      [osIgnoreDepthBuffer, osDirectDraw];
    FSnapObj[I].Lines.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
    FSnapObj[I].Enabled := True;
  end;
  FIndexSnap := 0;
  FEdgeIntersect := False;

{$IFDEF USE_CLIP}
  for I := Low(FSnapPlan) to High(FSnapPlan) do
  begin
    FSnapPlan[I] := TsgPickObject.Create(Self);
    FSnapPlan[I].Color := Measure3DParams.EdgesColor;
    FSnapPlan[I].OnPick := SnapPlanEvent;
    FSnapPlan[I].Lines.ObjectStyle := FSnapPlan[I].Lines.ObjectStyle +
      [osIgnoreDepthBuffer, osDirectDraw];
    FSnapPlan[I].Lines.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
    FSnapPlan[I].Enabled := True;
  end;
  FIndexSnapPlan := 0;
{$ENDIF}

  FSnapEdge := TsgPickObject.Create(Self);
  FSnapEdge.Color := Measure3DParams.EdgesHoverColor;
  FSnapEdge.OnPick := SnapEdgeObjEvent;
  FSnapEdge.Lines.ObjectStyle := FSnapEdge.Lines.ObjectStyle +
    [osIgnoreDepthBuffer, osDirectDraw];
  FSnapEdge.Lines.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
  FSnapEdge.Enabled := True;

  FSnapDim := TsgPickObject.Create(Self);
  FSnapDim.Color := Measure3DParams.EdgesHoverColor;
  FSnapDim.OnPick := SnapDimObjEvent;
  FSnapDim.Lines.ObjectStyle := FSnapEdge.Lines.ObjectStyle +
     [osIgnoreDepthBuffer, osDirectDraw];
  FSnapDim.Lines.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
  FSnapDim.Enabled := True;

  FDimImageByDrawing := TsgCADImage.Create;
  FDimImageByDrawing.Converter.InitializeSectionsBegin;
  FDimImageByDrawing.Converter.InitializeSectionsEnd;
  FDimImageByDrawing.CurrentLayout := FDimImageByDrawing.Layouts[0];
  FDimImageByDrawing.GetExtents;

  FDimByDrawing := TsgDXFDimension.Create;
  FDimImageByDrawing.CurrentLayout.AddEntity(FDimByDrawing);
  FDimByDrawing.Flags := 1;

  vPHVS := TsgDXFConverterAccess(FDimImageByDrawing.Converter).PHeadVarStruct;

{$IFDEF SG_SHARED_VCL}
  vPHVS^.DimProps := cnstDim3DVCL;

  vDimStyle := TsgDXFDimensionStyle.Create;
  vDimStyle.Name := cnstDim3DVCLName;
  TsgDXFDimensionStyleAccess(vDimStyle).FDimProps := vPHVS^.DimProps;
  vDimStyle.TextHeight := Measure3DParams.TextHeight;
  vDimStyle.ArrowSize := Measure3DParams.ArrowSize;
  FDimImageByDrawing.Converter.Sections[csDimStyles].AddEntity(vDimStyle);
  FDimImageByDrawing.Converter.Loads(vDimStyle);
  vPHVS^.DimStyle := vDimStyle.Name;
  FDimByDrawing.Style := vDimStyle;

  vIndex := 0;
  for I := Low(cnstCadStyles) to High(cnstCadStyles) do
  begin
    if lgAny in cnstCadStyles[I].LangIds then
    begin
      vIndex := I;
      Break;
    end;
  end;

  FDimByDrawing.Style.TextStyle := TsgDXFStyle.Create;
  FDimByDrawing.Style.TextStyle.Name := cnstDim3DVCLName;
  FDimByDrawing.Style.TextStyle.FontName :=  cnstCadStyles[vIndex].Font;
  FDimByDrawing.Style.TextStyle.PrimaryFont :=  cnstCadStyles[vIndex].FFile;
  FDimImageByDrawing.Converter.Sections[csStyles].AddEntity(FDimByDrawing.Style.TextStyle);
  FDimImageByDrawing.Converter.Loads(FDimByDrawing.Style.TextStyle);
  vPHVS^.DimTextStyle := FDimByDrawing.Style.TextStyle.Name;

//  vMeasureStyle := GetMeasureStyle;
//  if Assigned(vMeasureStyle) then
//  begin
//    vTextStyle := FDimByDrawing.Style.TextStyle;
//    try
//      FDimByDrawing.Style.AssignEntity(vMeasureStyle);
//      vTextStyle.AssignEntity(vMeasureStyle.TextStyle);
//    finally
//      FDimByDrawing.Style.TextStyle := vTextStyle;
//    end;
//  end;
  FDimImageByDrawing.Converter.Loads(FDimByDrawing.Style.TextStyle);
  FDimImageByDrawing.Converter.Loads(FDimByDrawing.Style);
{$ELSE}
  vDimStyle :=  TsgDXFDimensionStyle.Create;
  vDimStyle.AssignEntity(GetMeasureStyle3D);
  vDimStyle.TextStyle := TsgDXFStyle.Create;
  vDimStyle.TextStyle.AssignEntity(GetMeasureStyle3D.TextStyle);
  FDimImageByDrawing.Converter.Sections[csStyles].AddEntity(vDimStyle.TextStyle);
  FDimImageByDrawing.Converter.Loads(vDimStyle.TextStyle);
  FDimImageByDrawing.Converter.Sections[csDimStyles].AddEntity(vDimStyle);
  FDimImageByDrawing.Converter.Loads(vDimStyle);

  vPHVS^.DimProps := TsgDXFDimensionStyleAccess(vDimStyle).FDimProps;
  vPHVS^.DimStyle := vDimStyle.Name;
  vPHVS^.DimTextStyle := vDimStyle.TextStyle.Name;

  FDimByDrawing.Style := vDimStyle;
  TsgDXFDimensionAccess(FDimByDrawing).ActualDimStyle.AssignEntity(FDimByDrawing.Style);
{$ENDIF}

  FDimByDrawing.Layer := FDimImageByDrawing.Converter.LayerByName(sContinuous);
  TsgDXFDimensionAccess(FDimByDrawing).SetLineType(FDimImageByDrawing.Converter.LTypeByName(sContinuous));
  FDimImageByDrawing.Converter.Loads(FDimByDrawing);

  FTextByDrawing := TsgDXFText.Create;

  FTextByDrawing.Point := cnstFPointZero;
  FTextByDrawing.Style := FDimByDrawing.Style.TextStyle;
  FTextByDrawing.Height := FDimByDrawing.TextHeight;
  FTextByDrawing.Text := '';
  FDimImageByDrawing.CurrentLayout.AddEntity(FTextByDrawing);
  FDimImageByDrawing.Converter.Loads(FTextByDrawing);
  SetDefault3DSceneOptions;
  FSearchEntMesh := TsgMeshObject(TsgMeshObject.NewInstance);
  FRoot := TRoot.Create(nil);
  TRoot(FRoot).F3DDrawingNavigator := Self;
  FProgressObject := CreateProgressObject;
{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
  FSelectionForPolygons := TsgModObjList.Create(False);
  FFileList := TStringList.Create;
{$ENDIF}
  FDeferredObjectToDelete := TsgObjectList.Create;
  FBusy := False;
end;

function Tsg3DDrawingNavigator.CreateGLPolyline: TsgGLPolyline;
begin
  Result := FNav3D.CreateGLPolyline;
end;

procedure Tsg3DDrawingNavigator.CreateGLSceneLine(var ALine: TsgGLPolyline;
  AColor: TColor; ALineWidth: Single; const ATypeLine: TsgTypeLine = tlMass);
begin
  if ATypeLine = tlOuterWire then
    ALine := Navigator3D.FLineOuterWire.AddNode
  else
    ALine := Navigator3D.FLineMass.AddNode;
  ALine.LineWidth := ALineWidth;
  ALine.MaterialName := AssignMaterial(AColor, FNav3D.FGLMaterialLibrary);
  ALine.MaterialNamePoint := AssignMaterial(Measure3DParams.SelectNodeColor, FNav3D.FGLMaterialLibrary);
end;

procedure Tsg3DDrawingNavigator.CreateGLScenePlane(var APlane: TGLPlane);
begin
  APlane := TGLPlane.CreateAsChild(FNav3D.FGLFreeForm);
  APlane.ShowAxes := False;
end;

function Tsg3DDrawingNavigator.CreateMeshObject(AOwner: TMeshObjectList;
  AEntity: TObject; AMode: {$IFDEF GLSCENE13}TGLMeshObjectMode{$ELSE}TMeshObjectMode{$ENDIF} = momFaceGroups): TMeshObject;
var
  I, J: Integer;
  N, Root: TNodeLite;
  Insert: TsgDXFInsert;
  vInsertsTree: TsgObjectList;
begin
  Result := TsgMeshObject.CreateWithEnt(AOwner, AEntity);
  //Result.Name := HexDisplayPrefix + IntToHex(TsgNativeUInt(AEntity), 0); //IntToHex(Integer(Result), 4);
  Result.Mode := AMode;
  Result.TexCoords.Clear;

  Insert := IterateParam.Insert;
  if Insert <> nil then
  begin
    vInsertsTree := TsgObjectList.Create;
    try
      repeat
        vInsertsTree.Add(Insert);
        Insert := Insert.OwnerInsert;
      until Insert = nil;

      Insert := IterateParam.Insert;

      Root := TNodeLite(FRoot);
      N := nil;
      if vInsertsTree.Count > 1 then
      begin
        I := vInsertsTree.Count - 1;
        if Root.Find(vInsertsTree[I], J) then
        begin
          N := TNodeLite(Root[J]);
          Dec(I);
          while I >= 0 do
          begin
            if N.Find(vInsertsTree[I], J) then
              N := TNodeLite(N[J]);
            Dec(I);
          end;
        end;
        if N = Root then
          N := nil;
      end
      else
        if Root.Find(Insert, I) then
          N := TNodeLite(Root[I]);
      if N = nil then
        if TRoot(FRoot).FindDeep(Insert, N, J) then
          N := TNodeLite(N[J]);
      TsgMeshObject(Result).Node := N;
    finally
      vInsertsTree.Free;
    end;
  end;
end;

function Tsg3DDrawingNavigator.CreateProgressObject: TProgress;
begin
  Result := TProgress.Create;
  Result.ProgressEvent := Progress;
  Result.ProgressSender := Self;
  Result.UpdateDelay := 5000;
end;


function Tsg3DDrawingNavigator.CreateSnapShotBitmap: Graphics.TBitmap;
var
  vGLImage: TGLImage;

  function GLImageCreate32BitsBitmap(GLImage: TGLImage): TGLBitmap;
  var
    w, h: Integer;
  {$IFDEF FPC}
    RIMG: TRawImage;
  {$ELSE}
    y, x, x4: Integer;
    vSrc, vDest: PAnsiChar;
  {$ENDIF}
  begin
    if GLImage.Blank then
    begin
      Result := nil;
      exit;
    end;
    GLImage.Narrow;
    Result := TGLBitmap.Create;
    Result.PixelFormat := glpf32bit;
    w := GLImage.Width;
    h := GLImage.Height;
    SetSizeGraphic(Result, w, h);
    if h > 0 then
    begin
  {$IFDEF FPC}
      RIMG.Init;
      rimg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width, Height);
      rimg.Description.RedShift := 0;
      rimg.Description.BlueShift := 16;
      rimg.Description.LineOrder := riloBottomToTop;
      RIMG.DataSize := GLImage.DataSize;
      rimg.Data := PByte(GLImage.Data);
      { READ THIS PLEASE: !!!!!!!!!!!!  }
      { If you get a compile time error on the following line, you could either
        A) update your lazarus to >= 0.9.27-r18329, or
        B) comment the next line and un-comment the "Workaround for older Lazarus "
           part after the next line }

      result.LoadFromRawImage(rimg, false);

      { -- "Workaround for older Lazarus "
          LIntfImg:=TLazIntfImage.Create(rimg,False);
          try
            result.LoadFromIntfImage(LIntfImg);
          finally
            FreeAndNil(LIntfImg);
          end;
        -- End of "Workaround for older Lazarus " }
  {$ELSE}
      vSrc := @PAnsiChar(GLImage.Data)[w * 4 * (h - 1)];
      for y := 0 to h - 1 do
      begin
        vDest := Result.ScanLine[y];
        for x := 0 to w - 1 do
        begin
          x4 := x * 4;
          vDest[x4 + 0] := vSrc[x4 + 2];
          vDest[x4 + 1] := vSrc[x4 + 1];
          vDest[x4 + 2] := vSrc[x4 + 0];
          vDest[x4 + 3] := vSrc[x4 + 3];
        end;
        Dec(vSrc, w * 4);
      end;
  {$ENDIF}
    end;
  end;

  procedure DoSwapBuffer;
  begin
    {$IFDEF MSWINDOWS}
    SwapBuffers(FOwnDC);
    {$ENDIF}
  end;

begin
  if Assigned(FNav3D) then
  begin
    if FNav3D.FIsDirty > 0 then
      RepaintScene;
    DoSwapBuffer;
    try
      vGLImage := FNav3D.Buffer.CreateSnapShot;
      try
        Result := GLImageCreate32BitsBitmap(vGLImage);
      finally
        vGLImage.Free;
      end;
    finally
      DoSwapBuffer;
    end;
  end
  else
    Result := nil;
end;

destructor Tsg3DDrawingNavigator.Destroy;
var
  I: Integer;
begin
  FHighlightSaveMatNames.Free;
  FInsertStack.Free;
  FPickObjects.OnPick := nil;
  Clear;
  FVectorFile.Free;
  FSearchEntMesh.FreeInstance;
  DisposeAndNil(FIntersectCache);
  FPickObjects.Free;
  FHighLightObj.Free;
  for I := Low(FSnapObj) to High(FSnapObj) do
    FSnapObj[I].Free;
{$IFDEF USE_CLIP}
  for I := Low(FSnapPlan) to High(FSnapPlan) do
    FSnapPlan[I].Free;
{$ENDIF}
  FSnapEdge.Free;
  FSnapDim.Free;
  FreeAndNil(FDimImageByDrawing);
  FDimByDrawing := nil;
  FRoot.Free;
  FTransformation.Free;
  FProgressObject.Free;
{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
  FSelectionForPolygons.Free;
  FFileList.Free;
{$ENDIF}
  FDeferredObjectToDelete.Free; // If the list is not empty it's a bug
  inherited Destroy;
end;

procedure Tsg3DDrawingNavigator.DoNodes;

var
  P: PsgCADIterate;
  Root: TRoot;

  procedure AddEntNodes(AEnts: TsgDXFEntity; ANodes: TNodeLite; AIsBrep: Boolean);
  var
    I: Integer;
    N: TNodeLite;
    Ent: TsgDXFEntity;
    vPathKey: string;
  begin
    for I := 0 to AEnts.Count - 1 do
    begin
      Ent := AEnts[I];
      case Ent.EntType of
        ceInsert:
          begin
            N := ANodes.AddChild(Ent);
            vPathKey := TCustomNode.GetPathKeyByNode(N);
            SetVisualization(Ent, TObject(N), vPathKey);
            //N.SetBox(TsgDXFInsert(Ent).Box);
            AddEntNodes(TsgDXFInsert(Ent).Block, N, False);
          end;
        ceIges, ceStep, ceBrep, ceParasolid, ceInventor,
        ceRegion, ceBody, ceSurface, ce3DSolid:
        begin
          N := TNodeGroup.Create(ANodes);
          TNodeGroup(N).Group := TsgBrepModEntity(Ent).Compound;
          N.Obj := TsgBrepModEntity(Ent).Compound;
          //N.SetBox(TsgBrepModEntity(Ent).Box);
          ANodes.Add(N);
          //Only after the formation of the tree!!!
          vPathKey := TCustomNode.GetPathKeyByNode(N);
          SetVisualization(N.Obj, TObject(N), vPathKey);
        end;
      end;
    end;
  end;

begin
  FRoot.Clean;
  Root := TRoot(FRoot);
  Root.F3DDrawingNavigator := Self;
  P := InitializeIterateParams;
  Root.FMatrix := P^.Matrix;
  Root.Obj := FConverter.Sections[csEntities];
  AddEntNodes(FConverter.Sections[csEntities], Root, False);
  Root.SortNodes(Root);
end;

procedure Tsg3DDrawingNavigator.DoOnColorChanged(const AColor: TColor);
begin
  if FNav3D.BackgroundColor <> AColor then
  begin
    FNav3D.BackgroundColor := Color;
    if Picture.Graphic is TsgVectorGLScene then
      TsgVectorGLScene(Picture.Graphic).BackgroundColor := Color;
    SetShowingStyle(FShowingStyle);
  end;
end;

procedure Tsg3DDrawingNavigator.DoOnCursorChanged;
begin
  inherited DoOnCursorChanged;
end;

procedure NewFaceGroup(Param: Pointer); cdecl;
//var
//  vParams: PsgParamTriangle;
begin
//  vParams := PsgParamTriangle(Param);
//  Tsg3DDrawingNavigator(vParams^.ASelf).FCurrVertexes :=
//    Tsg3DDrawingNavigator(vParams^.ASelf).GetFaceGroup(TMeshObject(vParams^.MeshPointer),
//      fgmmTriangles, Tsg3DDrawingNavigator(vParams^.ASelf).FCurrColor, TFGVertexIndexList);
//  vParams^.VertexPointer := Tsg3DDrawingNavigator(vParams^.ASelf).FCurrVertexes;
end;

{$IFNDEF SG_CALC_NORMALS}
function FindVector(AList: TAffineVectorList; const AVector: TAffineVector;
  var I: Integer): Boolean;
var
  vList: PAffineVectorArray;

  function _VectorEquals(const V1, V2: TAffineVector): Boolean;
  begin
    Result := IsZero(V1.X - V2.X) and IsZero(V1.Y - V2.Y) and IsZero(V1.Z - V2.Z);
  end;

begin
  Result := False;
  I := AList.Count - 1;
  vList := AList.List;
  while (I >= 0) and not Result do
    if _VectorEquals(vList^[I], AVector) then
      Inc(Result)
    else
      Dec(I);
end;
{$ENDIF}

//function AddTrialgleVertex(AParam: PsgParamTriangle; const AVertex: TsgVertex): Integer;
//var
//  vVect: TAffineVector;
//begin
//  vVect := Tsg3DDrawingNavigator(AParam^.ASelf).GetPointf(AVertex);
//{$IFNDEF SG_CALC_NORMALS}
//  if not FindVector(TMeshObject(AParam^.MeshPointer).Vertices, vVect, Result) then
//{$ENDIF}
//    Result := TMeshObject(AParam^.MeshPointer).Vertices.Add(vVect);
//  TFGVertexIndexList(AParam^.VertexPointer).Add(Result);
//end;

//procedure GetTriangle(DimTriangle: PsgDimTriangle; Param: Pointer); cdecl;
//var
//  vMesh: TMeshObject;
//  I, vIndex: Integer;
//  vTriangle: PsgTriangle;
//  vParams: PsgParamTriangle;
//  vVertexes: TFGVertexIndexList;
//  v3dNav: Tsg3DDrawingNavigator;
//begin
//  vParams := PsgParamTriangle(Param);
//  v3dNav :=  Tsg3DDrawingNavigator(vParams^.ASelf);
//  if not v3dNav.FIsHasTriangledMesh then
//    v3dNav.FIsHasTriangledMesh := True;
//  vVertexes := vParams^.VertexPointer;
//  if vVertexes <> nil then
//  begin
//    vMesh := vParams^.MeshPointer;
//    if vMesh <> nil then
//    begin
//      vTriangle := PsgTriangle(DimTriangle);
//      Inc(PByte(vTriangle), SizeOf(DimTriangle^.CountTriangle));
//      for I := 0 to DimTriangle^.CountTriangle - 1 do
//      begin
//        vIndex := AddTrialgleVertex(vParams, vTriangle^.Corner1);
//        vMesh.Normals.Count := vMesh.Vertices.Count;
//        vMesh.Normals.TranslateItem(vIndex, FPoint2Vect(vTriangle^.Normal));
//        vIndex := AddTrialgleVertex(vParams, vTriangle^.Corner2);
//        vMesh.Normals.Count := vMesh.Vertices.Count;
//        vMesh.Normals.TranslateItem(vIndex, FPoint2Vect(vTriangle^.Normal));
//        vIndex := AddTrialgleVertex(vParams, vTriangle^.Corner3);
//        vMesh.Normals.Count := vMesh.Vertices.Count;
//        vMesh.Normals.TranslateItem(vIndex, FPoint2Vect(vTriangle^.Normal));
//        Inc(vTriangle);
//      end;
//    end;
//  end;
//end;

type
  PsgTestLine = ^TsgTestLine;
  TsgTestLine = packed record
    Color: Integer;
    Point1: TFPoint;
    Point2: TFPoint;
    SegmentNumber: Integer;
    Normal: TFPoint;
    Creator: Pointer;
  end;

procedure CallBackOuterLineDraw_(AData: PsgTestLine;
  const AIsRealPoint: Boolean = False); cdecl;
const
  cnstBadCan = 9000000000;
var
  v3dNav: Tsg3DDrawingNavigator;
  vPoint1, vPoint2: TFPoint;
  vPoint, vLastPoint: TFPoint;

  function IsBadPoint(APoint: TFPoint): Boolean;
  begin
    Result := {$IFNDEF SGDEL_7}sgFunction.{$ENDIF}IsNan(APoint.X) or IsInfinite(APoint.X);
    Result := Result or {$IFNDEF SGDEL_7}sgFunction.{$ENDIF}IsNan(APoint.Y) or IsInfinite(APoint.Y);
    Result := Result or {$IFNDEF SGDEL_7}sgFunction.{$ENDIF}IsNan(APoint.Z) or IsInfinite(APoint.Z);
  end;

  function AddTrialgleVertex(A3DNav: Tsg3DDrawingNavigator; const AVertex: TFPoint): Integer;
  var
    vVect: TAffineVector;
  begin
    vVect := FPoint2Vect(AVertex);
  {$IFNDEF SG_CALC_NORMALS}
    if not FindVector(A3DNav.FCurrMeshObject.Vertices, vVect, Result) then
  {$ENDIF}
      Result := A3DNav.FCurrMeshObject.Vertices.Add(vVect);
    A3DNav.FCurrVertexes.Add(Result);
  end;

  procedure NewLine(A3DNav: Tsg3DDrawingNavigator; APoint1, APoint2, ANormal: TFPoint);
  begin
    A3DNav.CreateGLSceneLine(A3DNav.FNav3D.FCurrentLine,
      A3DNav.FOuterWireColor, A3DNav.FOuterWireLineWieght, tlOuterWire);
    A3DNav.FNav3D.FCurrentLine.Add(vPoint1);
    A3DNav.FNav3D.FCurrentLine.Add(vPoint2);
    A3DNav.FNav3D.FCurrentLine.FNormal := ANormal;

    A3DNav.FNav3D.FCurrentLine.FMeshList.Add(A3DNav.FCurrMeshObject);
    A3DNav.FNav3D.FCurrentLine.FCountUses := 1;
  end;

begin
  vPoint1 := MakeFPoint(AData.Point1.X, AData.Point1.Y, AData.Point1.Z);
  vPoint2 := MakeFPoint(AData.Point2.X, AData.Point2.Y, AData.Point2.Z);
  if IsBadPoint(vPoint1) or IsBadPoint(vPoint2) then
    Exit;
  if IsRangeFPoints(vPoint1, cnstFPointZero, MakeFPoint(cnstBadCan, cnstBadCan, cnstBadCan)) or
    IsRangeFPoints(vPoint2, cnstFPointZero, MakeFPoint(cnstBadCan, cnstBadCan, cnstBadCan)) then
  begin
    v3dNav := Tsg3DDrawingNavigator(AData.Creator);
    if AIsRealPoint then
    begin
      vPoint1 := vPoint1;
      vPoint2 := vPoint2;
    end
    else
    begin
      vPoint1 := v3dNav.GetPoint(vPoint1);
      vPoint2 := v3dNav.GetPoint(vPoint2);
    end;
    if (not Assigned(v3dNav.FNav3D.FCurrentLine)) or (AData.SegmentNumber = 0) then
    begin
      NewLine(v3dNav, vPoint1, vPoint2, MakeFPoint(AData.Normal.X, AData.Normal.Y, AData.Normal.Z));
    end
    else
    begin
      vLastPoint := v3dNav.FNav3D.FCurrentLine[v3dNav.FNav3D.FCurrentLine.Count-1];
      vPoint := vPoint1;
      if IsEqualFPoints(vLastPoint, vPoint) then
      begin
        v3dNav.FNav3D.FCurrentLine.Add(vPoint2);
      end
      else
        NewLine(v3dNav, vPoint1, vPoint2, cnstFPointZero);
    end;
  end;
end;

procedure CallBackOuterLineDrawReal(AData: PsgTestLine); cdecl;
begin
  CallBackOuterLineDraw_(AData, True);
end;

procedure CallBackOuterLineDraw(AData: PsgTestLine); cdecl;
begin
  CallBackOuterLineDraw_(AData);
end;

procedure Tsg3DDrawingNavigator.Draw3dFace(Entity: TsgDXFEntity);
var
  OldCurrMeshObject: TMeshObject;
  OldCurrVertexes: TFGVertexIndexList;
begin
  OldCurrMeshObject := FCurrMeshObject;
  OldCurrVertexes := FCurrVertexes;
  try
    FCurrMeshObject := FCurrMeshObject3dFace;
    FCurrVertexes := FCurrVertexes3dFace;
    DrawBaseSolid(Entity);
  finally
    FCurrMeshObject := OldCurrMeshObject;
    FCurrVertexes := OldCurrVertexes;
  end;
end;

var
  NbExtraVertices: Integer;
  NewVertices: PFPointArray;
  NewPointList: TFPointList;

function AllocNewVertex: PAffineVector;
begin
  Inc(NbExtraVertices);
  Result := @NewVertices[NbExtraVertices - 1];
end;

procedure TessError(Errno: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Assert(False, IntToStr(Errno) + ': ' + string(GluErrorString(Errno)));
end;

procedure TessIssueVertex(VertexData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  NewPointList.Add(PFPoint(VertexData)^);
end;

procedure TessCombine(Coords: PDoubleVector; Vertex_data: Pointer;
  Weight: PGLFloat; var OutData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  OutData := AllocNewVertex;
  PFPoint(OutData)^ := MakeFPoint(Coords^[0], Coords^[1], Coords^[2]);
end;

(*procedure TessBegin(Mode: TGLenum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  //
end;

procedure TessEnd;
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  //
end;*)

procedure GLNodesRenderTesselatedPolygon(APolygon, APolygonOut: TFPointList;
  ANormal: PAffineVector = nil; AInvertNormals: Boolean = False);
var
  I: Integer;
  Tess: PGLUTesselator;
  DblVector: TAffineDblVector;
begin
  if APolygon.Count > 2 then
  begin
    // Create and initialize the GLU tesselator
    Tess := GluNewTess;
    //GluTessCallback(Tess, GLU_TESS_BEGIN, @TessBegin);
    GluTessCallback(Tess, GLU_TESS_VERTEX, @TessIssueVertex);
    //GluTessCallback(Tess, GLU_TESS_END, @TessEnd);
    GluTessCallback(Tess, GLU_TESS_ERROR, @TessError);
    GluTessCallback(Tess, GLU_TESS_COMBINE, @TessCombine);
    NbExtraVertices := 0;
    // Issue normal
    if Assigned(ANormal) then
    begin
      GL.Normal3fv(PGLFloat(ANormal));
      GluTessNormal(Tess, ANormal^.V[0], ANormal^.V[1], ANormal^.V[2]);
    end;
    // Issue polygon
    GluTessBeginPolygon(Tess, nil);
    GluTessBeginContour(Tess);

    // no spline, use direct coordinates
    GetMem(NewVertices, APolygon.Count * SizeOf(TFPoint));
    FillChar(NewVertices^, APolygon.Count * SizeOf(TFPoint), 0);
    NewPointList := APolygonOut;
    if AInvertNormals then
    begin
      for I := APolygon.Count - 1 downto 0 do
      begin
        DblVector := TAffineDblVector(APolygon.List^[I]);
        GluTessVertex(Tess, DblVector, @APolygon.List^[I]);
      end;
    end
    else
    begin
      for I := 0 to APolygon.Count - 1 do
      begin
        DblVector := TAffineDblVector(APolygon.List^[I]);
        GluTessVertex(Tess, DblVector, @APolygon.List^[I]);
      end;
    end;

    GluTessEndContour(Tess);
    GluTessEndPolygon(Tess);
    // release stuff
    if Assigned(NewVertices) then
      FreeMem(NewVertices);
    GluDeleteTess(Tess);
  end;
end;

procedure Tsg3DDrawingNavigator.DrawSolidAsPolygon(Entity: TsgDXFEntity);
var
  vPts: TsgPoints4;
  vSolid: TsgDXFSolidAccess absolute Entity;
  vPolygon, vPolygonOut: TFPointList;
begin
  vPts[0] := vSolid.Point;
  vPts[1] := vSolid.Point1;
  vPts[3] := vSolid.Point2;
  vPts[2] := vSolid.Point3;
  vPolygon := TFPointList.Create;
  try
    vPolygon.AppendArray(vPts);
    vPolygonOut := TFPointList.Create;
    try
      GLNodesRenderTesselatedPolygon(vPolygon, vPolygonOut);
      case vPolygonOut.Count of
        6: AddVertices(vPolygonOut);
      else
        if vPolygonOut.Count = 0 then
          vPolygonOut.Assign(vPolygon);
        if DistanceFPoint(vPolygonOut[2], vPolygonOut[3]) < fAccuracy then
          vPolygonOut.Delete(2)
        else
        if DistanceFPoint(vPolygonOut[0], vPolygonOut[1]) < fAccuracy then
          vPolygonOut.Delete(0)
        else
        if DistanceFPoint(vPolygonOut[1], vPolygonOut[2]) < fAccuracy then
          vPolygonOut.Delete(1)
        else
        if DistanceFPoint(vPolygonOut[0], vPolygonOut[3]) < fAccuracy then
          vPolygonOut.Delete(3)
        else
        if DistanceFPoint(vPolygonOut[0], vPolygonOut[2]) < fAccuracy then
          vPolygonOut.Delete(0)
        else
        if DistanceFPoint(vPolygonOut[1], vPolygonOut[3]) < fAccuracy then
          vPolygonOut.Delete(3);
        AddVertices(vPolygonOut); // 0,1,2; 3...
        if vPolygonOut.Count > 3 then
        begin
          // ...0, 2
          FCurrVertexes.Add(FCurrVertexes.VertexIndices[FCurrVertexes.VertexIndices.Count - 4]);
          FCurrVertexes.Add(FCurrVertexes.VertexIndices[FCurrVertexes.VertexIndices.Count - 3]);
        end;
      end;
    finally
      vPolygonOut.Free;
    end;
  finally
    vPolygon.Free;
  end;
end;

procedure Tsg3DDrawingNavigator.DrawBaseSolid(Entity: TsgDXFEntity);
var
  vPts: TFPointList;
  vSolid: TsgDXFSolidAccess;
  I: Integer;
begin
  vSolid := TsgDXFSolidAccess(Entity);
  vPts := TFPointList.Create;
  try
    if vSolid.GetDrawPoints(vPts) then
    begin
      for I := 0 to vPts.Count - 1 do
        AddVertex(vPts[I]);
    end;
  finally
    FreeAndNil(vPts);
  end;
end;

procedure Tsg3DDrawingNavigator.ProcModEntity(const AEnt: TsgModEntity; const AData: Pointer);

 procedure sgDrawMesh(AFace: TsgModTopoFace; const AStartMatrix: TsgMatrix4d);
  var
    vMesh: TsgModMeshTriangles;
    vTriangle: TsgModMeshTriangleNode;
    vVertex: TsgModMeshSurfaceVertex;
    vPntCnt: Integer;

  procedure LoadMeshVertex(const AMesh: TsgModMeshTriangles;const AVertex: TsgModMeshSurfaceVertex; const AStartMatrix: TsgMatrix4d);
  var
   vInd: Integer;
   V: TsgVector3d;
    vMesh: TMeshObject;
  begin
    vMesh := FCurrMeshObject;
    vInd := AVertex.VertexIndex;

     V := sgMFunctions.PointTransform(AVertex.Point, AStartMatrix);
     {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.SetVector(vMesh.Vertices.List^[vInd], V.X, V.Y, V.Z);

     V := DirectionTransform(AVertex.Normal, AStartMatrix);
     if AMesh.MaterialOrientation = bsoReversed then
       NegateVector(V);
     {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.SetVector(vMesh.Normals.List^[vInd], V.X, V.Y, V.Z);
  end;

  begin
    begin
      FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor, TFGVertexIndexList);

      vMesh := TsgModMeshTriangles(AFace.Mesh);

      vPntCnt := FCurrMeshObject.Vertices.Count;
      vMesh.EnumeratePoints(vPntCnt);
      if vPntCnt = FCurrMeshObject.Vertices.Count then
        Exit;

      FCurrMeshObject.Vertices.Count := vPntCnt;
      FCurrMeshObject.Normals.Count := vPntCnt;

      vVertex := vMesh.FirstVertex;
      while Assigned(vVertex) do
      begin
        LoadMeshVertex(vMesh, vVertex, AStartMatrix);
        vVertex := vVertex.Next;
      end;


      vTriangle := vMesh.FirstTriangle;
      while Assigned(vTriangle) do
      begin
        FCurrVertexes.VertexIndices.Add(vTriangle.Vertex[1].VertexIndex);
        FCurrVertexes.VertexIndices.Add(vTriangle.Vertex[2].VertexIndex);
        FCurrVertexes.VertexIndices.Add(vTriangle.Vertex[3].VertexIndex);
        vTriangle := vTriangle.Next;
      end;

      if vMesh.MaterialOrientation = bsoReversed then
        FCurrVertexes.VertexIndices.Reverse;

    end;
  end;

begin

  case AEnt.GetEntType of
    mtTopology:
    begin
      case TsgModTopoEdge(AEnt).GetShapeType of
        btEdge:
        begin
          //sgDrawEdge(TsgModTopoEdge(AEnt), IdentityMatrix4d);
        end;
        btFace:
        begin
          sgDrawMesh(TsgModTopoFace(AEnt), IdentityMatrix4d);
        end;
      end;
    end;
  end;
end;





procedure Tsg3DDrawingNavigator.DrawModEdge(const AEdge: TsgModTopoEdge; var AParams: TsgBrepModIterateParams);
var
  vPoint: TFPoint;
  vPoly: TsgModMeshPolyline;
  vNode: TsgModMeshPolylineNode;
  vColorAttrib: TsgModAttribColorBase;
  vBox: TFRect;
begin
  vPoly := AEdge.ExtractFirstPolyline;
  if Assigned(vPoly) and Assigned(vPoly.FirstNode) then
  begin
    vColorAttrib := AEdge.QueryColorAttrib;
    FCurrColor := ColorToSceneColor(TsgBrepModEntity.FindColor(AParams, vColorAttrib));
    CreateGLSceneLine(FNav3D.FCurrentLine,
      FOuterWireColor, FOuterWireLineWieght, tlOuterWire);
    FNav3D.FCurrentLine.FNormal := AParams.CurrentFaceSunNormal;
    if FCurrMeshObject.Vertices.Count > 0  then
    begin
      FNav3D.FCurrentLine.FMeshList.Add(FCurrMeshObject);
      FNav3D.FCurrentLine.FCountUses := 1;
    end;
    vBox := TsgMeshObject(FCurrMeshObject).GetBox;
    vNode := vPoly.FirstNode;
    while Assigned(vNode) do
    begin
      vPoint := PointTransform(vNode.Point, AParams.Matrix);
      ExpandFRect(vBox, vPoint);
      FNav3D.FCurrentLine.Add(vPoint);
      vNode := vNode.Next;
    end;
    TsgMeshObject(FCurrMeshObject).SetBox(vBox);
  end;
end;

procedure Tsg3DDrawingNavigator.DrawModMeshVertexForward(const AVertex: TsgModMeshSurfaceVertex;
  const AParams: TsgBrepModIterateParams; var ASumNormals: TFPoint);
var
  vInd: Integer;
  V: TsgVector3d;
begin
  vInd := AVertex.VertexIndex;
  V := PointTransform(AVertex.Point, AParams.Matrix);
  {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.SetVector(FCurrMeshObject.Vertices.List^[vInd], V.X, V.Y, V.Z);
  V := DirectionTransform(AVertex.Normal, AParams.Matrix);
  ASumNormals := AddFPoint(ASumNormals, V);
  {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.SetVector(FCurrMeshObject.Normals.List^[vInd], V.X, V.Y, V.Z);
end;

procedure Tsg3DDrawingNavigator.DrawModMeshVertexReversed(const AVertex: TsgModMeshSurfaceVertex;
  const AParams: TsgBrepModIterateParams; var ASumNormals: TFPoint);
var
  vInd: Integer;
  V: TsgVector3d;
begin
  vInd := AVertex.VertexIndex;
  V := PointTransform(AVertex.Point, AParams.Matrix);
  {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.SetVector(FCurrMeshObject.Vertices.List^[vInd], V.X, V.Y, V.Z);
  V := DirectionTransform(AVertex.Normal, AParams.Matrix);
  NegateVector(V);
  ASumNormals := AddFPoint(ASumNormals, V);
  {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.SetVector(FCurrMeshObject.Normals.List^[vInd], V.X, V.Y, V.Z);
end;

procedure Tsg3DDrawingNavigator.DrawModFace(const AFace: TsgModTopoFace; var AParams: TsgBrepModIterateParams);
var
  I: Integer;
  vMesh: TsgModMeshTriangles;
  vTriangle: TsgModMeshTriangleNode;
  vVertex: TsgModMeshSurfaceVertex;
  vPntCnt: Integer;
  vArea: Double;
  vBox: TFRect;
//  vSumNorm: TFPoint;
  V: TsgVector3d;
{$IFDEF DEBUG_RANDOM_COLOR_FACE}
  vRandom: TRGBColor;
{$ELSE}
  vColorAttrib: TsgModAttribColorBase;
{$ENDIF}
begin

{$IFDEF DEBUG_RANDOM_COLOR_FACE}
//Random color for Face
  vRandom.X := Random(255);
  vRandom.Y := Random(255);
  vRandom.Z := Random(255);
  FCurrColor := ConvertRGBtoColor(vRandom.X, vRandom.Y, vRandom.Z);
  FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor, TFGVertexIndexList);
{$ELSE}
  vColorAttrib := AFace.QueryColorAttrib;
  FCurrColor := ColorToSceneColor(TsgBrepModEntity.FindColor(AParams, vColorAttrib));
  FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor, TFGVertexIndexList);
{$ENDIF}

  vMesh := TsgModMeshTriangles(AFace.Mesh);

  vPntCnt := FCurrMeshObject.Vertices.Count;
  vMesh.EnumeratePoints(vPntCnt);
  if vPntCnt = FCurrMeshObject.Vertices.Count then
    Exit;

  FCurrMeshObject.Vertices.Count := vPntCnt;
  FCurrMeshObject.Normals.Count := vPntCnt;

  vBox := cnstBadRect;
  AParams.CurrentFaceSunNormal := cnstFPointZero;
  vVertex := vMesh.FirstVertex;
  if vMesh.MaterialOrientation = bsoReversed then
    while Assigned(vVertex) do
    begin
      DrawModMeshVertexReversed(vVertex, AParams, AParams.CurrentFaceSunNormal);
      vVertex := vVertex.Next;
    end
  else
    while Assigned(vVertex) do
    begin
      DrawModMeshVertexForward(vVertex, AParams, AParams.CurrentFaceSunNormal);
      vVertex := vVertex.Next;
    end;

  NormalizeVector(AParams.CurrentFaceSunNormal);

  vTriangle := vMesh.FirstTriangle;
  vArea := 0;
  while Assigned(vTriangle) do
  begin
    vArea := vArea + TriangleArea(vTriangle.Vertex[1].Point,
      vTriangle.Vertex[2].Point, vTriangle.Vertex[3].Point);

    for I := 1 to 3 do
    begin
      V := PointTransform(vTriangle.Vertex[I].Point, AParams.Matrix);
      ExpandFRect(vBox, V);
    end;

    FCurrVertexes.VertexIndices.Add(vTriangle.Vertex[1].VertexIndex);
    FCurrVertexes.VertexIndices.Add(vTriangle.Vertex[2].VertexIndex);
    FCurrVertexes.VertexIndices.Add(vTriangle.Vertex[3].VertexIndex);
    vTriangle := vTriangle.Next;
  end;
  TsgMeshObject(FCurrMeshObject).SetArea(vArea);
  TsgMeshObject(FCurrMeshObject).SetBox(vBox);

  if vMesh.MaterialOrientation = bsoReversed then
    FCurrVertexes.VertexIndices.Reverse;
end;

procedure Tsg3DDrawingNavigator.DrawModEntity(const AEnt: TsgModEntity;
  var AParams: TsgBrepModIterateParams);
var
  vNodeLite: TNodeLite;
  vPathKey: String;

  procedure CreateNode(const AEnt: TsgModEntity;
    var AParams: TsgBrepModIterateParams);
  var
    vNode: TNodeMatrix;
    vPathKey: String;
  begin
    if Assigned(AParams.Data) then
    begin
      vNode := TNodeMatrix.Create(TNodeLite(AParams.Data));
      vNode.Obj := AEnt;
      vNode.FMatrix := Matrix434To34(AParams.Matrix);
      TNodeLite(AParams.Data).Add(vNode);
      vPathKey := TCustomNode.GetPathKeyByNode(vNode);
      SetVisualization(TsgModVisibleEntity(AEnt), TObject(vNode), vPathKey);
      //vNode.SetBox(TsgBrepModSupport.GetBox(AEnt));
      AParams.Data := vNode;
    end;
  end;
begin
  case AEnt.GetEntType of
    mtStructure:
    begin
      case TsgModPartEntity(AEnt).GetPartType of
        mptCompound, mptInstance:
        begin
          if TsgModEntityAccess(AEnt).CanShow then
            CreateNode(AEnt, AParams);
        end;
      end;
    end;
    mtTopology:
    begin
      case TsgModTopoShape(AEnt).GetShapeType of
        btSolid, btShell, btShape:
        begin
          CreateNode(AEnt, AParams);
        end;
        btEdge:
        begin
          DrawModEdge(TsgModTopoEdge(AEnt), AParams);
          TsgModTopoEdge(AEnt).ClearIsProcessed(True);
        end;
        btFace:
        begin
          vNodeLite := nil;
          if Assigned(AParams.Data) then
          begin
            vNodeLite := TNodeLite.Create(TNodeLite(AParams.Data));
            vNodeLite.Obj := AEnt;
            //vNodeLite.SetBox(TsgBrepModSupport.GetBox(AEnt));
            TNodeLite(AParams.Data).Add(vNodeLite);
          end;
          FCurrMeshObject := CreateMeshObject(FNav3D.GLFreeForm.MeshObjects, TsgModTopoFace(AEnt));
          TsgMeshObject(FCurrMeshObject).Node := vNodeLite;
          vNodeLite.Mesh := FCurrMeshObject;
          vPathKey := TCustomNode.GetPathKeyByNode(vNodeLite);
          SetVisualization(TsgModVisibleEntity(AEnt), TObject(vNodeLite), vPathKey);
          DrawModFace(TsgModTopoFace(AEnt), AParams);
        end;
      end;
    end;
  end;
end;

{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
procedure Tsg3DDrawingNavigator.InitPolygons;
begin
  if not Assigned(FPolygons) then
  begin
    FPolygons := TfrmPolygons.Create(Self);
    FPolygons.Modeller := TsgDXFConverterAccess(FConverter).Modeller;
    FPolygons.SelectionList := FSelectionForPolygons;//FMainProcessor.SelectedEntities;
    FPolygons.OnFormHide := nil;
    FPolygons.OnSelectionChange := nil;//SelectionChangeExternal;
    FPolygons.OnPickPoint := nil;//PointChangeExternal;
    FPolygons.KeyPreview := True;
    FPolygons.OnKeyUp := nil;//FormKeyUp;
  end;
end;

procedure Tsg3DDrawingNavigator.InitStructure;
begin
  if not Assigned(FStructure) then
  begin
    FStructure := TfrmStructure.Create(self);
    FStructure.SelectionList := FSelectionForPolygons;
    FStructure.Modeller := TsgDXFConverterAccess(FConverter).Modeller;
    FStructure.OnFormHide := nil;
    //FStructure.OnVisibleSelectionOnOff := nil;
    //FStructure.OnVisibleAll := nil;
    //FStructure.OnVisibleSelectedOnly := nil;
    FStructure.OnSelectionChange := nil;
    FStructure.KeyPreview := True;
    FStructure.OnKeyUp := nil;
  end;
end;

procedure Tsg3DDrawingNavigator.InitFileStructure;
begin
  if not Assigned(FStructureFile) then
  begin
    FStructureFile := TfmFileStructure.Create(self);
    FStructureFile.FileList := FFileList;
    FStructureFile.FormStyle := fsStayOnTop;
    FStructureFile.OnFormHide := nil;
    FStructureFile.OnSelectionChange := nil;
    FStructureFile.KeyPreview := True;
    FStructureFile.OnKeyUp := nil;
  end;
end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.DrawACIS(Entity: TsgDXFEntity);
var
  vBrepModParams: TsgBrepModIterateParams;
//  vIndex: Integer;
  (*vOwner,*) vInsert, vNodes: TNodeLite;
  vPathKey: String;
begin
  case TsgDXFEntity(Entity).EntType of
    ceIges, ceStep, ceBrep, ceParasolid, ceInventor,
    ceRegion, ceBody, ceSurface, ce3DSolid:
    begin
      vBrepModParams.CADColor := EntColor(Entity, nil);
      vBrepModParams.Matrix := Matrix34To44(FTransformation.MatrixAsAddress^);
      vBrepModParams.PartColor := nil;
      vBrepModParams.ShapeColor := nil;
      //????
//      vIndex := -1;
      vInsert := TNodeLite(FRoot).GetNodeByStrKey(
        TsgInsertStack(FInsertStack).GetPath + '-$' + IntToHex(UInt64(TsgBrepModEntity(Entity).Compound), 0));
      if not Assigned(vInsert) then
      begin
        vNodes := TNodeLite(FRoot).GetNodeByStrKey(
        TsgInsertStack(FInsertStack).GetPath);
        if not Assigned(vNodes) then
          vNodes := TNodeLite(FRoot);
        vInsert := TNodeGroup.Create(vNodes);
        TNodeGroup(vInsert).Group := TsgBrepModEntity(Entity).Compound;
        vInsert.Obj := TsgBrepModEntity(Entity).Compound;
        vNodes.Add(vInsert);
        //Only after the formation of the tree!!!
        vPathKey := TCustomNode.GetPathKeyByNode(vInsert);
        SetVisualization(vInsert.Obj, TObject(vInsert), vPathKey);
        vInsert := vInsert;
      end;

      vBrepModParams.Data := vInsert;

//      vInsert := TNodeLite(FRoot);
//      if Assigned(IterateParam.Insert) then
//      begin
//        vInsert.FindDeep(IterateParam.Insert, vOwner, vIndex);
//        if (vIndex >= 0) and (vIndex < TRoot(vOwner).Count) then
//          vInsert := Pointer(TRoot(vOwner)[vIndex]);
//      end;
//      vInsert.FindDeep(Entity, vOwner, vIndex);
//      if (vIndex >= 0) and (vIndex < TRoot(vOwner).Count) then
//        vBrepModParams.Data := Pointer(TRoot(vOwner)[vIndex])
//      else
//        vBrepModParams.Data := nil;

      TsgBrepModEntity(Entity).MeshLinPrecision := FDeviationCoefficient;
      TsgBrepModEntity(Entity).DrawIterateCompound(DrawModEntity, vBrepModParams);

      if Navigator3D.GetTrianglesCount > 0 then
        FIsHasTriangledMesh := True;
      TRoot(FRoot).SortNodes(TRoot(FRoot));
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.DrawFlatPoly3D(Entity: TsgDXFEntity);
var
  I, J, vCount, vIndex: Integer;
  vFlatPoly3D: TsgFlatPoly3D;
  vList: TFPointList;
begin
  vList := TFPointList.Create;
  try
    vFlatPoly3D := TsgFlatPoly3D(Entity);
    vIndex := 0;
    for I := 0 to vFlatPoly3D.Counts.Count - 1 do
    begin
      vCount := vFlatPoly3D.Counts[I];
      if vCount > 0 then
      begin
        J := vIndex;
        vList.Count := 0;
        while J <= (vIndex + vCount-1) do
        begin
          vList.Add(vFlatPoly3D.XY[J]);
          Inc(J);
        end;
        DrawPolyLineExt(vList, True, False);
      end;
      Inc(vIndex, vCount);
    end;
  finally
    vList.Free;
  end;
end;

procedure Tsg3DDrawingNavigator.DrawImage(Entity: TsgDXFEntity);
var
  AImageEnt: TsgDXFImageEnt absolute Entity;
  vPlane: TGLPlane;
  vAngle1, vAngle2: Double;
  vBitmap: TsgBMAdapter;
  vP, vP1, vP2, vPC: TFPoint;
begin
  if (AImageEnt.Picture = nil) or (AImageEnt.Picture.Graphic = nil) or IsBadRect(AImageEnt.Box) then
    Exit;

  CreateGLScenePlane(vPlane);

  vP := GetPoint(AImageEnt.Point);
  vP1 := GetPoint(AImageEnt.Point1);
  vP2 := GetPoint(AImageEnt.Point2);
//  vPC := GetCenterOfRect(AImageEnt.Box);
  vPC := MiddleFPoint(vP1, vP2);
  vAngle1 := GetAngleByPoints(vP, vP1, False, fDoubleResolution);
  vAngle2 := GetAngleByPoints(vP, vP2, False, fDoubleResolution);

//  vPlane.Position.AsAffineVector := GetPointf(vPC);
  vPlane.Position.AsAffineVector := FPoint2Vect(vPC);
  vPlane.Width := AImageEnt.Width;
  vPlane.Height := AImageEnt.Height;
  if IsZero(vAngle1) then
  begin
    vPlane.RollAngle := 0;
    if vAngle2 > 180 then
      vPlane.Scale.Y := -1;
  end
  else
    vPlane.RollAngle := vAngle1;


  vPlane.Material.Texture.Disabled := False;

  vBitmap := TsgBMAdapter.Create;
  try
    vBitmap.Assign(AImageEnt.Picture.Graphic);
    vPlane.Material.Texture.Image.Assign(vBitmap.Bitmap);
    {$IFDEF SGDEL_XE2}
    if AImageEnt.Picture.Graphic is TPngImage then
    begin
      vPlane.Material.BlendingMode := bmTransparency;
      vPlane.Material.Texture.TextureMode := tmReplace;
    end;
    {$ENDIF}
  finally
    vBitmap.Free;
  end;
end;

procedure Tsg3DDrawingNavigator.DrawImageEnt(Entity: TsgDXFEntity);
var
  vImg: TsgDXFImageEnt absolute Entity;
begin
  if vImg.ClipPointsCount <= 2 then
    DrawImage(Entity);
end;

procedure Tsg3DDrawingNavigator.DrawLine(Entity: TsgDXFEntity);
var
  vLine: TsgDXFLine;
  vList: TFPointList;
begin
  vLine := TsgDXFLine(Entity);
  vList := TFPointList.Create;
  try
    vList.Add(vLine.Point);
    vList.Add(vLine.Point1);
    if vLine.ZThick = 0 then
      DrawPolyLineExt(vList, True, False)
    else
      DrawLineAndZTick(vLine, vLine.Point, vLine.Point1);
  finally
    vList.Free;
  end;
end;

procedure Tsg3DDrawingNavigator.DrawLineAndZTick(Line: TsgDXFLine; P1,
  P2: TFPoint);
var
  vList: TFPointList;
  vPts: TsgPoints4;
  vNormal: TFPoint;
  vZThick: TFPoint;

  procedure AddEdgePlane( APoint1, APoint2, ANormal: TFPoint);
  var
    vLine: TsgGLPolyline;
  begin
    CreateGLSceneLine(vLine, FCurrColor, FCurrLineWieght);
    vLine.Add(APoint1);
    vLine.Add(APoint2);
    vLine.FNormal := ANormal;
  end;

begin
  { TODO: DrawLineAndZTick as mesh }
  vList := TFPointList.Create;
  try
    vList.Add(P1);
    vList.Add(P2);
    vZThick := FPointXMat(MakeFPoint(0, 0, Line.ZThick),
      ExtrusionToMatrix(Line.Extrusion));
    vList.Add(AddFPoint(P2, vZThick));
    vList.Add(AddFPoint(P1, vZThick));

    vPts[0] := GetPoint(vList[0]);
    vPts[1] := GetPoint(vList[1]);
    vPts[2] := GetPoint(vList[2]);
    vPts[3] := GetPoint(vList[3]);
    vNormal := sgCalcPlaneNormal(vPts[0], vPts[1], vPts[2]);
    AddEdgePlane(vPts[0], vPts[1], vNormal);
    AddEdgePlane(vPts[1], vPts[2], vNormal);
    AddEdgePlane(vPts[2], vPts[3], vNormal);
    AddEdgePlane(vPts[3], vPts[0], vNormal);

    FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor, TFGVertexIndexList);
    AddVertex(vList[0]);
    AddVertex(vList[1]);
    AddVertex(vList[2]);
    AddVertex(vList[0]);
    AddVertex(vList[2]);
    AddVertex(vList[3]);
    FinalMeshObject(FCurrMeshObject, FNeedNormals);
  finally
    vList.Free;
  end;
end;

procedure Tsg3DDrawingNavigator.DrawMesh(Entity: TsgDXFEntity; AGLMaterial: TGLLibMaterial);
var
  I, C: Integer;
begin
  FMeshEntity := Entity;
  for I := 0 to TsgDXFMesh(FMeshEntity).Vertices.Count - 1 do
    FCurrMeshObject.Vertices.Add(GetPointf(TsgDXFMesh(FMeshEntity).Vertices[I]));
  if TsgDXFMesh(FMeshEntity).TexCoords.Count > 0 then
  begin
    FCurrMeshObject.TexCoords.Count := TsgDXFMesh(FMeshEntity).TexCoords.Count;
    for I := 0 to TsgDXFMesh(FMeshEntity).TexCoords.Count do
      FCurrMeshObject.TexCoords[I] := FPoint2Vect(TsgDXFMesh(FMeshEntity).TexCoords[I]);
  end
  else
    FCurrMeshObject.TexCoords.Assign(FCurrMeshObject.Vertices);
  C := TsgDXFMesh(FMeshEntity).ExtractFaceIndices(nil);
  if C > 0 then
  begin
    FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, AGLMaterial, TFGVertexIndexList);
    FCurrVertexes.VertexIndices.Count := C * 3;
    TsgDXFMesh(FMeshEntity).ExtractFaceIndices(@FCurrVertexes.VertexIndices.List^[0]);
  end;
  //FNeedNormals := False;
end;

procedure Tsg3DDrawingNavigator.DrawPolyLine(Entity: TsgDXFEntity);
var
  I: Integer;
  vPolyLine: TsgCADBasePolyLineAccess;
  vDXFPolyLine: TsgDXFPolyLine;
  vPolyLineAccess: TsgDXFPolylineAccess;
  vVertexList: TFPointList;
  vVertexIndexList: TsgIntegerList;
begin
  vPolyLine := TsgCADBasePolyLineAccess(Entity);
  vPolyLineAccess := TsgDXFPolylineAccess(Entity);

  if Entity is TsgDXFPolyLine then
    vDXFPolyLine := TsgDXFPolyLine(Entity)
  else
    vDXFPolyLine := nil;

  vVertexList := TFPointList.Create;
  vVertexIndexList := TsgIntegerList.Create;
  try
    vVertexList.Count := 0;
    vVertexIndexList.Count := 0;

    if (vDXFPolyLine <> nil) and vPolyLine.IsPolygonMesh then
      vPolyLineAccess.GetVertexListForPolygonMesh(vDXFPolyLine.MeshM,
        vDXFPolyLine.MeshN, vVertexList, vVertexIndexList)
    else
    if vPolyLine.IsPolyZThickness then
      vPolyLineAccess.GetVertexListForPolygonMesh(2,
        vPolyLine.PolyPoints.Count shr 1, vVertexList, vVertexIndexList)
    else
      if (vDXFPolyLine <> nil) and vPolyLineAccess.IsPolyFaceParamsValid and (vPolyLineAccess.Flags in [$50, $40]) then
        vPolyLineAccess.GetVertexListForPolyFaceMesh(vVertexList)
      else
        DrawPolyLineExt(vPolyLine.PolyPoints, vPolyLine.Lines.IsSolid, vPolyLine.Closed);

     if vVertexList.Count > 0 then
     begin
       for I := 0 to vVertexList.Count - 1 do
         AddVertex(vVertexList[I]);
       if vVertexIndexList.Count > 0 then
         UpdateVertexIndexList(vVertexIndexList)
     end;
  finally
    vVertexList.Free;
    vVertexIndexList.Free;
  end;
end;

procedure Tsg3DDrawingNavigator.DrawPolyLineExt(Points: TFPointList; Solid,
  Closed: Boolean);
var
  vLine: TsgGLPolyline;
begin
  CreateGLSceneLine(vLine, FCurrColor, FCurrLineWieght);
  PrepareData(vLine, Points, Solid, Closed);
end;

procedure Tsg3DDrawingNavigator.DrawPolyLineExtByTextArray(
  const Points: array of TFPoint; Solid, Closed: Boolean);
var
  vLine: TsgGLPolyline;
  I: Integer;
begin
  vLine := Navigator3D.FTextMass.AddNode;
  vLine.LineWidth := FCurrLineWieght;
  vLine.MaterialName := AssignMaterial(FCurrColor, FNav3D.FGLMaterialLibrary);
  vLine.MaterialNamePoint := AssignMaterial(clYellow, FNav3D.FGLMaterialLibrary);
  if Length(Points) = 0 then
  begin
    Navigator3D.FTextMass.DeleteLastNode;
    Exit;
  end;
  for I := Low(Points) to High(Points) do
    vLine.Add(GetPoint(Points[I]));
  if Solid then
  begin
    if Closed then
      vLine.Mode := GL_LINE_LOOP;
  end
  else
    vLine.Mode := GL_LINES;
end;

procedure Tsg3DDrawingNavigator.DrawSolid(Entity: TsgDXFEntity);
begin
  DrawSolidAsPolygon(Entity);
  //DrawBaseSolid(Entity);
end;

procedure Tsg3DDrawingNavigator.DrawHatch(Entity: TsgDXFEntity);
var
  vPolygon: TsgCADPolyPolygon;
  I, J: Integer;
  vPolylineList: TFPointList;
  vList2D: TF2DPointList;
  vPoint: TFPoint;
begin
  vPolylineList := TFPointList.Create;
  vPolygon := TsgCADPolyPolygon(Entity);
  try
    for I := 0 to vPolygon.Boundaries.Count - 1 do
    begin
      vPolylineList.Count := 0;
      vList2D := TF2DPointList(vPolygon.Boundaries[I]);
      for J := 0 to vList2D.Count - 1 do
      begin
        vPoint := MakeFPointFrom2D(vList2D.List[J]);
        vPolylineList.Add(vPoint);
      end;
      DrawPolyLineExt(vPolylineList, True, False);
    end;
  finally
    vPolylineList.Free;
  end;
end;

procedure Tsg3DDrawingNavigator.DrawText(Entity: TsgDXFEntity);
var
  I: Integer;
  vCollection: TsgTextLinesCollection;
  vText: TsgDXFText;
  P: PFPointArray;
begin
  vText := TsgDXFText(Entity);
  vCollection := TsgTextLinesCollection.Create(TFPointList.Create, True);
  try
    if FLoadFromImage.Converter.GetTextPolylines(vText, vCollection) > 0 then
    begin
      P := TFPointList(vCollection.Poly).List;
      for I := 0 to vCollection.Counts.Count - 1 do
      begin
        DrawPolyLineExtByTextArray(Slice(P^, vCollection.Counts[I]), True, False);
        Inc(PFPoint(P), vCollection.Counts[I]);
      end;
    end;
  finally
    vCollection.Free;
  end;
end;

function Tsg3DDrawingNavigator.Empty: Boolean;
begin
  if FNav3D = nil then
    Result := True
  else
    Result := FNav3D.IsEmpty;
end;

function Tsg3DDrawingNavigator.EndUpdate: Integer;
begin
  if FNav3D.Buffer.Updating > 0 then
    FNav3D.Buffer.EndUpdate;
  Result := FNav3D.Buffer.Updating;
  ImgUnLock;
  Invalidate;
end;

function Tsg3DDrawingNavigator.EntToGLObj(AEnt: TObject;
  APIndex: PInteger = nil): TObject;
var
  I: Integer;
  vList: PPointerObjectList;
begin
  Result := nil;
  if APIndex = nil then APIndex := @I;
  FSearchEntMesh.SetEntity(AEnt);
  vList := FNav3D.GLFreeForm.MeshObjects.List;
  QFind(FNav3D.GLFreeForm.MeshObjects, True, dupIgnore, MeshEntCompare,
    FSearchEntMesh, APIndex^);
  if (APIndex^ >= 0) and (APIndex^ < FNav3D.GLFreeForm.MeshObjects.Count) then
    Result := vList^[APIndex^];
end;

function Tsg3DDrawingNavigator.ExtractArrayFromList(AAnyList: TObject;
  var A: Pointer): Integer;
begin
  Pointer(A) := nil;
  Result := 0;
  if AAnyList is TsgObjectList then
  begin
    Result := TsgObjectList(AAnyList).Count;
    if Result > 0 then
      A := TsgObjectList(AAnyList).List;
  end
  else
    if AAnyList is TList then
    begin
      Result := TList(AAnyList).Count;
      if Result > 0 then
        A := @TList(AAnyList).List{$IFDEF LIST_PTR}^{$ENDIF}[0];
    end
end;

procedure Tsg3DDrawingNavigator.FitToSize;
var
  vBox: TFRect;
  vDrawMatrix: TFMatrix;
  vView: TsgDrawRect;
begin
  GetView(vDrawMatrix);
  vBox := GetBox;
  if TCustomNode(FRoot).IsExploded then
    vBox := TRoot(FRoot).GetBoxByExploded;
  vView := TsgDrawRect.Create;
  try
    vView.AttachMatrix(@vDrawMatrix);
    OffsetFRect(vBox, -FBoxOffs.X, -FBoxOffs.Y, -FBoxOffs.Z);
    vView.Box := vBox;
    vView.FitTo(0, 0, FNav3D.Buffer.ViewPort.Width, FNav3D.Buffer.ViewPort.Height);
  finally
    vView.Free;
  end;
  LoadView(vDrawMatrix);
end;

procedure Tsg3DDrawingNavigator.FinalMeshObject(var AMesh: TMeshObject; ANeedNormals: Boolean;
  const IsDeferredDeletion: Boolean = False);
begin
  if AMesh <> nil then
  begin
    if AMesh.Vertices.Count = 0 then
    begin
      if IsDeferredDeletion then
        FDeferredObjectToDelete.Add(AMesh)
      else
        FreeAndNil(AMesh);
    end
    else
      if ANeedNormals or (not CheckCurVertexes) then
        GenNormals(AMesh);
  end;
end;

function Tsg3DDrawingNavigator.FinishEntity(Entity: TsgDXFEntity): Integer;
begin
//  if Entity is TsgDXFACISIndexFace then
//    FIsBrepEntityIterate := False;
//  if Entity is TsgBrepIndexFace then
//    FIsBrepEntityIterate := False;
  case Entity.EntType of
    ceInsert:
      begin
        TsgInsertStack(FInsertStack).Pop;
        Load3dFaceMeshObject;
        FCurrMeshObject3dFace := nil;
      end;
    ceMesh:
      if FMeshEntity = Entity then
        FMeshEntity := nil;
  end;
  Result := 0;
end;

{$IFDEF SG_CALC_NORMALS}
procedure Tsg3DDrawingNavigator.FacesSmooth(aMeshObj: TMeshObject; aWeldDistance: Single=0.0000001; aThreshold: Single=35.0);
Var
  I, J, K, L: integer;
  WeldedVertex: TAffineVectorList;
  TmpIntegerList: TIntegerList;
  IndexMap: TStringList;
  v, n: TAffineVector;
  indicesMap : TIntegerList;
  Index: Integer;
  FaceList: TIntegerList;
  NormalList: TAffineVectorList;
  FaceNormalList: TAffineVectorList;
  FaceGroup: TFaceGroup;
  FG, FG1: TFGVertexIndexList;
  Threshold: Single;
  Angle: Single;
  ReferenceMap: TIntegerList;
  ID1, ID2: Integer;
  Index1, Index2, Index3: Integer;

  Tex1, Tex2, Tex3, Tex4: TTexPoint;
  List: TIntegerList;
  DupList: TStringList;

  vFaceCountDivTwoSubOne, vRefMapIndex1, vRefMapIndex2: Integer;

  function FindReferenceIndex(aID: Integer): Integer;
  begin
    Result := ReferenceMap.List^[aID];
  end;
  function iMin(a, b: Integer): Integer;
  begin
    if a<b then
      Result := a
    else
      Result := b;
  end;
  function iMax(a, b: Integer): Integer;
  begin
    if a>b then
      Result := a
    else
      Result := b;
  end;
begin
  aMeshObj.Normals.Count := aMeshObj.Vertices.Count;
  Threshold := aThreshold * Pi/180.0;
  //build the vectices reference map
  ReferenceMap := TIntegerList.Create;
  WeldedVertex := TAffineVectorList.Create;
  WeldedVertex.Assign(aMeshObj.Vertices);
  indicesMap := TIntegerList.Create;
  //first of all, weld the very closed vertices
  WeldVertices(WeldedVertex, indicesMap, aWeldDistance);
  //then, rebuild the map list
  IndexMap := TStringList.Create;
  for I:=0 to WeldedVertex.Count-1 do
  begin
    ReferenceMap.Assign(indicesMap);
    TmpIntegerList := TIntegerList.Create;
    Index := ReferenceMap.IndexOf(I);
    while Index>=0 do
    begin
      TmpIntegerList.Add(Index);
      ReferenceMap[Index] := -99999;
      Index := ReferenceMap.IndexOf(I);
    end;
    IndexMap.AddObject(IntToStr(I), TmpIntegerList);
  end;
  ReferenceMap.Assign(indicesMap);

  //never used these, free them all
  WeldedVertex.free;
  indicesMap.free;
  //create a TexPoint list for save face infomation, where s=facegroup index, t=face index
  FaceList := TIntegerList.Create;
  NormalList := TAffineVectorList.Create;
  FaceNormalList := TAffineVectorList.Create;
  //NormalIndex := TIntegerList.Create;

  for I:=0 to aMeshObj.FaceGroups.Count-1 do
  begin
    FaceGroup := aMeshObj.FaceGroups[I];
    TmpIntegerList := TFGVertexIndexList(FaceGroup).VertexIndices;
    for J:=0 to (TmpIntegerList.Count div 3)-1 do
    begin
      FaceList.Add(I);
      FaceList.Add(J);
      CalcPlaneNormal(aMeshObj.Vertices[TmpIntegerList[J * 3 + 0]],
        aMeshObj.Vertices[TmpIntegerList[J * 3 + 1]],
        aMeshObj.Vertices[TmpIntegerList[J * 3 + 2]],
        n);
      //add three normals for one trangle
      FaceNormalList.Add(n);
      NormalList.Add(n);
      NormalList.Add(n);
      NormalList.Add(n);
    end;
  end;

  //do smooth
  vFaceCountDivTwoSubOne := (FaceList.Count div 2)-1;
  for I:=0 to vFaceCountDivTwoSubOne do
  begin
    Index := FaceList.List^[I shl 1];
    Index1 := FaceList.List^[I shl 1 +1];
    FG := TFGVertexIndexList(aMeshObj.FaceGroups.List^[Index]);
    for J:=0 to 2 do
    begin
      vRefMapIndex1 := FG.VertexIndices.List^[Index1*3+J];
      ID1 := ReferenceMap.List^[vRefMapIndex1];
      for K:=I+1 to vFaceCountDivTwoSubOne do
      begin

        Index2 := FaceList.List^[K shl 1];
        Index3 := FaceList.List^[K shl 1+1];
        FG1 := TFGVertexIndexList(aMeshObj.FaceGroups.List^[Index2]);
        //if I<>K then
        begin
          for L:=0 to 2 do
          begin
            //two face contain the same vertex
            //vRefMapIndex1 := FG.VertexIndices.List^[Index1*3+J];
            vRefMapIndex2 := FG1.VertexIndices.List^[Index3*3+L];
            ID2 := ReferenceMap.List^[vRefMapIndex2];
            if ID1=ID2 then
            begin
              Angle := VectorDotProduct(FaceNormalList.List^[I],FaceNormalList.List^[K]);
              if angle>Threshold then
              begin
                NormalList.List^[I*3+J] := VectorAdd(NormalList.List^[I*3+J],FaceNormalList.List^[K]);
                NormalList.List^[K*3+L] := VectorAdd(NormalList.List^[K*3+L],FaceNormalList.List^[I]);
              end;
            end;
          end;
        end;
      end;
      NormalizeVector(NormalList.List^[I*3+J]);
    end;
  end;

  for I:=0 to (FaceList.Count div 2)-1 do
  begin
    Index := FaceList[I*2+0];
    FG := TFGVertexIndexList(aMeshObj.FaceGroups[Index]);
    Index := FaceList[I*2+1];
    aMeshObj.Normals[FG.VertexIndices[(Index*3+0)]] := VectorNegate(NormalList[(I*3+0)]);
    aMeshObj.Normals[FG.VertexIndices[(Index*3+1)]] := VectorNegate(NormalList[(I*3+1)]);
    aMeshObj.Normals[FG.VertexIndices[(Index*3+2)]] := VectorNegate(NormalList[(I*3+2)]);
  end;

  FaceList.free;
  NormalList.free;
  FaceNormalList.free;
  ReferenceMap.free;
  for I:=0 to IndexMap.Count-1 do
    IndexMap.Objects[I].free;
  IndexMap.free;
end;
{$ENDIF}

// only for trialngles, no fgmmQuads
procedure Tsg3DDrawingNavigator.GenNormals(AMesh: TMeshObject);
{$IFNDEF SG_CALC_NORMALS}
var
  I, J, C, K, L, N: Integer;
  vNorm: TAffineVector;
  vVerts: array[0 .. 3] of TAffineVector;
  vList: TFGVertexIndexList;
  vIndxs: PIntegerArray;
  vVertxs: PAffineVectorArray;
{$ENDIF}
begin
  if AMesh.Normals.Count > 0 then Exit;
{$IFDEF SG_CALC_NORMALS}
  FacesSmooth(AMesh, 0.0000001, 65.0 );
{$ELSE}
  AMesh.Normals.Count := AMesh.Vertices.Count;
  vVertxs := PAffineVectorArray(AMesh.Vertices.List);
  for I := 0 to AMesh.FaceGroups.Count - 1 do
    if AMesh.FaceGroups[I] is TFGVertexIndexList then
    begin
      vList := TFGVertexIndexList(AMesh.FaceGroups[I]);
      N := vList.VertexIndices.Count;
      vIndxs := PIntegerArray(vList.VertexIndices.List);
      if vList.Mode <> fgmmQuads then
      begin
        K := 3;
        for J := 0 to (N div K) - 1 do
        begin
          L := J * K;
          vVerts[0] := vVertxs^[vIndxs^[L]];
          vVerts[1] := vVertxs^[vIndxs^[L + 1]];
          vVerts[2] := vVertxs^[vIndxs^[L + 2]];
          CalcPlaneNormal(vVerts[0], vVerts[1], vVerts[2], vNorm);
          for C := 0 to K - 1 do
            AMesh.Normals.TranslateItem(vIndxs^[J * K + C], vNorm);
        end;
      end
      else
      begin
        K := 4;
        for J := 0 to (N div K) - 1 do
        begin
          L := J * K;
          vVerts[0] := vVertxs^[vIndxs^[L]];
          vVerts[1] := vVertxs^[vIndxs^[L + 1]];
          vVerts[2] := vVertxs^[vIndxs^[L + 2]];
          vVerts[3] := vVertxs^[vIndxs^[L + 3]];

          CalcPlaneNormal(vVerts[0], vVerts[1], vVerts[2], vNorm);
          AMesh.Normals.TranslateItem(vIndxs^[L], vNorm);
          AMesh.Normals.TranslateItem(vIndxs^[L + 1], vNorm);
          AMesh.Normals.TranslateItem(vIndxs^[L + 2], vNorm);

          CalcPlaneNormal(vVerts[0], vVerts[2], vVerts[3], vNorm);
          AMesh.Normals.TranslateItem(vIndxs^[L + 0], vNorm);
          AMesh.Normals.TranslateItem(vIndxs^[L + 2], vNorm);
          AMesh.Normals.TranslateItem(vIndxs^[L + 3], vNorm);
        end;
      end;
    end;
  AMesh.Normals.Normalize;
{$ENDIF}
end;

procedure Tsg3DDrawingNavigator.GetBoundingBox(var ABoundingBox: THmgBoundingBox);
var
  I: Integer;
  vBox: TFRect;
begin
  HandleNeeded;
  vBox := GetBox;
//    ABoundingBox := FNav3D.GLDCScene.BoundingBox...;
  for I := 0 to 7 do
    ABoundingBox.BBox[I] := VectorMake(vBox.V[I div 4].V[0], vBox.V[(I div 2) mod 2].V[1], vBox.V[I and 1].V[2]);
  for I := 0 to 7 do
    ABoundingBox.BBox[I] := VectorTransform(ABoundingBox.BBox[I], FNav3D.GLDCScene.Matrix);
end;

{$IFDEF USE_CLIP}
function Tsg3DDrawingNavigator.GetSceneCenter: TAffineVector;
begin
  Result := NullVector;
  if Assigned(FNav3D) then
    Result := FNav3D.FSceneCenter;
end;

function Tsg3DDrawingNavigator.GetSceneRadius: Single;
begin
  Result := 1;
  if Assigned(FNav3D) then
    Result := FNav3D.FSceneRadius;
end;

procedure Tsg3DDrawingNavigator.SetSnapPlanPointsCount(const Value: Integer);
begin
  FIndexSnapPlan := 0;
  FSnapPlanCount := Value;
  if FSnapPlanCount < Low(FSnapPlan) + 1 then
    FSnapPlanCount := Low(FSnapPlan) + 1;
  if FSnapPlanCount > High(FSnapPlan) + 1 then
    FSnapPlanCount := High(FSnapPlan) + 1;
  if FSnapPlanCount = 2 then
  begin
    FSnapPlanEntity := True;
    FIndexSnapPlan := FSnapPlanCount - 1;
  end
  else
    FSnapPlanEntity := False;
end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.GetScreenAABB(var AAABB: TAABB);
var
  I: Integer;
  vglbox: THmgBoundingBox;
begin
  GetBoundingBox(vglbox);
  FNav3D.ForceRenderingContext;
  FNav3D.Buffer.WorldToScreen(@vglbox.BBox[0], 8);
  AAABB := BBToAABB(vglbox);
  if (AAABB.Max.X = AAABB.Min.X) and (AAABB.Max.Y = AAABB.Min.Y) then
  begin
    for I := Low(AAABB.Max.V) to High(AAABB.Max.V) do
      AAABB.Max.V[I] := AAABB.Max.V[I] + 0.5;
    for I := Low(AAABB.Min.V) to High(AAABB.Min.V) do
      AAABB.Min.V[I] := AAABB.Min.V[I] - 0.5;
  end;
end;

function Tsg3DDrawingNavigator.GetActualGraphic: TGraphic;
{$IFDEF SG_LINUX_FPC}
const MB_APPLMODAL = MB_OK;
{$ENDIF}
var
  vGLGraphic: TsgGLGraphic;
begin
  SetActualGraphic(nil);
  vGLGraphic := TsgGLGraphic.Create;
  try
    vGLGraphic.HandleType := bmDIB;
    vGLGraphic.SetNavigator(Self);
    SetActualGraphic(vGLGraphic);
  except
    on E: Exception do
    begin
      MessageBox({$IFDEF CS_USEFORM}Application.Handle{$ELSE}0{$ENDIF},
        PChar(E.Message),
{$IFDEF SGABVIEWER}
        PChar(GetProgNameAndVer(True))
{$ELSE}
        nil
{$ENDIF},
        MB_APPLMODAL);
      vGLGraphic.Free;
      vGLGraphic := nil;
    end;
  end;
  Result := vGLGraphic;
end;

function Tsg3DDrawingNavigator.GetAntiAliasing: Integer;
begin
  Result := -1;
  if Assigned(FNav3D) then
    Result := Integer(FNav3D.Buffer.AntiAliasing);
end;

function Tsg3DDrawingNavigator.GetBox: TFRect;
var
  vAABB: TAABB;
{$IFDEF SG_ROTATE_CENTER_TEST}
  vBox: TFRect;
{$ENDIF}
begin
  if Assigned(FConverter) then
  begin
{$IFDEF SG_ROTATE_CENTER_TEST}
    vBox := TCustomNode(FRoot).GetCurrentBox(True);
    if IsBadRect(vBox) then
    begin
      Result := GetModelLayoutBox;
      OffsetFRect(Result, FBoxOffs.X, FBoxOffs.Y, FBoxOffs.Z);
    end
    else
      Result := vBox;
{$ELSE}
    Result := GetModelLayoutBox;
    OffsetFRect(Result, FBoxOffs.X, FBoxOffs.Y, FBoxOffs.Z);
{$ENDIF}

  end
  else
  begin
    //FNav3D.GLFreeForm.GetExtents(vAABB.min, vAABB.max);
    vAABB := BBToAABB(FNav3D.GLFreeForm.BoundingBox);
    Result.TopLeft := Vect2FPoint(vAABB.min);
    Result.BottomRight := Vect2FPoint(vAABB.max);
    SwapDoubles(Result.Top, Result.Bottom);
  end;
  PerformBroadcastEvents(WM_GETIMAGEBOX, WPARAM(@Result), LPARAM(FLoadFromImage));
end;

function Tsg3DDrawingNavigator.GetBoxOffset: TFPoint;
begin
  Result := FBoxOffs;
end;

function Tsg3DDrawingNavigator.GetDimension(
  const AIndex: Integer = -1): TsgPickObject;
begin
  Result := nil;
  if Assigned(FNav3D.FDimensionList) then
    if FNav3D.FDimensionList.Count > 0 then
    begin
      if AIndex <= -1 then
        Result := TsgPickObject(FNav3D.FDimensionList.Last)
      else
      begin
        if AIndex <= FNav3D.FDimensionList.Count then
          Result := TsgPickObject(FNav3D.FDimensionList.List[AIndex]);
      end;
    end;
end;

function Tsg3DDrawingNavigator.GetDimensionIndex(
  const AObject: TsgPickObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(AObject) then
  begin
    if Assigned(FNav3D.FDimensionList) then
    begin
      for I := 0 to FNav3D.FDimensionList.Count - 1 do
      begin
        if FNav3D.FDimensionList.List[I] = AObject then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
end;

function Tsg3DDrawingNavigator.GetDrawingCoords(ACoordX, ACoordY: Integer;
  var AUnits: string): TFPoint;
begin
  Result := GetDrawingInternalCoords(ACoordX, ACoordY);
end;

function Tsg3DDrawingNavigator.GetDrawingInternalCoords(ACoordX,
  ACoordY: Integer): TFPoint;
var
  vTmp: TFPoint;
begin
  Result := CoordinateConvertion(ACoordX, ACoordY, vTmp);
end;

function Tsg3DDrawingNavigator.GetDrawingUCSCoords(ACoordX, ACoordY: Integer;
  var AUnits: string): TFPoint;
begin
  CoordinateConvertion(ACoordX, ACoordY, Result);
end;

function Tsg3DDrawingNavigator.GetEnablePickObjects: Boolean;
begin
  Result := False;
  if Assigned(FPickObjects) then
    Result := FPickObjects.Enabled;
end;

function Tsg3DDrawingNavigator.GetEntity(Entity: TsgDXFEntity): Integer;
var
//  I, J, K: Integer;
//  vEntityChildren: TsgDXFEntity;
//  vLine: TGLLines;
//  vCount, vCountSum: Integer;
  S, vPath: string;
  vGLLibMaterial: TGLLibMaterial;
  vMaterial: TsgDXFMaterial;

 function GetVisibleEntity(AEntity: TsgDXFEntity): Boolean;
  var
    vInsert: TsgDXFInsert;
  begin
    Result := Entity.Visible and Entity.Visibility;
    vInsert := IterateParam.Insert;
    while Assigned(vInsert) and Result do
    begin
      Result := Result and vInsert.Visibility;
      vInsert := vInsert.OwnerInsert;
    end;
  end;

  function IsMaterialDefault(const AMaterial: TsgDXFMaterial): Boolean;
  begin
    Result := (AMaterial.Diffuse.MapSource = 0) and
      IsEqualColorCAD(AMaterial.Ambient.MaterialColor.Color, cnstColorCADByLayer) and
      IsEqualColorCAD(AMaterial.Diffuse.MaterialColor.Color, cnstColorCADByLayer) and
      IsEqualColorCAD(AMaterial.Specular.MaterialColor.Color, cnstColorCADByLayer);
  end;

begin
  Result := 0;
  FProgressObject.Progress;
  if FIsBrepEntityIterate then
    Exit;
  FNeedNormals := False;
  FNeedNormals3dFace := False;
  FCurrMeshObject := nil;
  FCurrVertexes := nil;
  FCurrColor := FDefaultColor;

  if Entity.IsInsert then
  begin
    Result := 1;
    case Entity.EntType of
      ceInsert:
      begin
        TsgInsertStack(FInsertStack).Push(Entity);
      end;
    end;
    Exit;
  end;

  if not GetVisibleEntity(Entity) then  Exit;
  if (FNav3D = nil) or (FNav3D.GLFreeForm = nil) or (FVectorFile = nil) then Exit;
  FMaterialEntity := MaterialEnt(Entity, IterateParam.Insert, FMeshEntity, nil);
  FCurrColor := ColorToSceneColor(EntColor(Entity, IterateParam.Insert));
  FCurrLineWieght := EntLineWeight(Entity, IterateParam.Insert);
  if FCurrLineWieght = 0 then
    FCurrLineWieght := 1;

  if Entity.EntType <> ce3dFace then
    FCurrMeshObject := CreateMeshObject(FNav3D.GLFreeForm.MeshObjects, Entity)
  else
    if (FCurrMeshObject3dFace = nil) or (FCurrColor3dFace <> FCurrColor) then
    begin
      Load3dFaceMeshObject;
      Create3DFaceMeshObject(Entity);
    end;

  case Entity.EntType of
    ceMesh:
      begin
        vMaterial := nil;
        if Assigned(FMaterialEntity) then
          vMaterial := FMaterialEntity.Material;
        if Assigned(vMaterial) and
           not ((vMaterial.Name = 'Global') and IsMaterialDefault(vMaterial)) then
        begin
          S := '';
          if vMaterial.Diffuse.MapSource = 1 then
          begin
            S := vMaterial.Diffuse.FileName;
            if not FileExists(S) then
            begin
              S := ExtractFileName(S);
              if Assigned(FLoadFromImage) then
              begin
                vPath := ExtractFilePath(FLoadFromImage.FileName);
                if vPath <> '' then
                  S := IncludeTrailingPathDelimiter(vPath) + S;
              end;
            end;
          end;
          if (S <> '') and FileExists(S) then
            vGLLibMaterial := FNav3D.FGLMaterialLibrary.AddTextureMaterial(vMaterial.Name, S)
          else
            vGLLibMaterial := FNav3D.FGLMaterialLibrary.Materials.Add;
          vGLLibMaterial.TextureMatrix := PMatrix(vMaterial.Diffuse.Mapper.AsAddress)^;
          DoGLMaterial(vGLLibMaterial.Material, vMaterial);
          vGLLibMaterial.Material.Texture.MinFilter := miLinear;
          vGLLibMaterial.Material.Texture.TextureMode := tmReplace;
        end
        else
          vGLLibMaterial := AssignMaterial(FCurrColor, FNav3D.FGLMaterialLibrary.Materials);
        DrawMesh(Entity, vGLLibMaterial);
        Result := 1;
      end;
    ceText:
      DrawText(Entity);
    ceLine:
      DrawLine(Entity);
    ce3dFace:
      Draw3dFace(Entity);
    ceSolid, ceTrace:
      begin
        FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor, TFGVertexIndexList);
        DrawSolid(Entity);
      end;
    cePolyPolygon, ceGradient, ceGradientPolygon, ceCurvePolygon, ceHatch:
      begin
        DrawHatch(Entity);
      end;
    ceCircle, ceArc, ceEllipse:
      begin
        if TsgDXFCircle(Entity).Lines.IsSolid and TsgDXFCircle(Entity).IsPolyZThickness then
          FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor,
            TFGVertexIndexList)
        else
          FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor,
            TsgVertexList);
        DrawPolyLine(Entity);
      end;
    ceLWPolyline, cePolyline, cePath:
      begin
        FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor,
          TFGVertexIndexList);
        DrawPolyLine(Entity);
      end;
    ceRegion, ceBody, ceSurface, ce3DSolid,
    ceIges, ceStep, ceBrep, ceParasolid, ceInventor:
      DrawACIS(Entity);
    ceSpline, ceLeader:
      begin
        FCurrVertexes := GetFaceGroup(FCurrMeshObject, fgmmTriangles, FCurrColor,
          TFGVertexIndexList);
        DrawPolyLineExt(TsgCADBasePolyline(Entity).PolyPoints,
          Entity.Lines.IsSolid, TsgCADBasePolyline(Entity).Closed);//DrawBasePolyLine??
      end;
    ceImageEnt:
      DrawImageEnt(Entity);
    ceFlatPoly3D:
      DrawFlatPoly3D(Entity);
  end;
  FinalMeshObject(FCurrMeshObject, FNeedNormals, True);
end;

function Tsg3DDrawingNavigator.GetFaceGroup(AMeshObject: TMeshObject; AMode: {$IFDEF GLSCENE13}TGLFaceGroupMeshMode{$ELSE}TFaceGroupMeshMode{$ENDIF};
  const AMaterial: TColor; AFaceGroupClass: TClass): TFGVertexIndexList;
begin
  Result := nil;
  if AMeshObject = nil then Exit;
  Result := TFaceGroupClass(AFaceGroupClass).CreateOwned(AMeshObject.FaceGroups) as TFGVertexIndexList;
  Result.Mode := AMode;
  Result.MaterialName := AssignMaterial(AMaterial, FNav3D.FGLMaterialLibrary);
end;

function Tsg3DDrawingNavigator.GetHasTriangledMesh: Boolean;
begin
  Result := FIsHasTriangledMesh;
end;

function Tsg3DDrawingNavigator.GetHighLightObject: TObject;
begin
  Result := FHighLightObj.Obj;
end;

function Tsg3DDrawingNavigator.GetSnapObject: TObject;
begin
  Result := FSnapObj[FIndexSnap].Obj;
end;

function Tsg3DDrawingNavigator.GetNodeMeshes(ANode: TCustomNode;
  const AClasses: array of TClass): TsgObjectList;
var
  vNode: TCustomNode;

  procedure NodesToList(ANodeLine: TCustomNode; AList: TsgObjectList);
  var
    I: Integer;
    vMesh: TsgMeshObject;
  begin
    if Assigned(ANodeLine) then
    begin
      vMesh := TsgMeshObject(ANodeLine.Mesh);
      if Assigned(vMesh) and (AList.IndexOf(vMesh) = -1) then
        AList.Add(vMesh);
      for I := 0 to ANodeLine.Count - 1 do
      begin
        NodesToList(TCustomNode(ANodeLine.Items[I]), AList);
      end;
    end;
  end;


begin
  Result := TsgObjectList.Create;
  vNode := TCustomNode.GetNodeOwnerByTopoClass(TCustomNode(FRoot), ANode, AClasses);
  NodesToList(vNode, Result);
end;


function Tsg3DDrawingNavigator.GetInsertMeshes(AInsert: TObject): TsgObjectList;
var
  I: Integer;
  vMesh: TsgMeshObject;
  vNode: TCustomNode;
  vIsEqual: Boolean;
begin
  Result := TsgObjectList.Create;
  for I := 0 to FNav3D.GLFreeForm.MeshObjects.Count - 1 do
  begin
    vIsEqual := False;
    vMesh := TsgMeshObject(FNav3D.GLFreeForm.MeshObjects[I]);
    vNode := vMesh.Node;
    while Assigned(vNode) and not vIsEqual do
    begin
      if vNode.Obj = AInsert then
        vIsEqual := True;
      vNode := vNode.Owner;
    end;
    if vIsEqual then
      if Result.IndexOf(vMesh) = -1 then
        Result.Add(vMesh);
  end;
end;

{$IFDEF USE_CLIP}
function Tsg3DDrawingNavigator.GetSnapPlanNormal: TFPoint;
var
  vPoly: TsgGLPolyline;
  vNormal: TVector4f;

begin
  Result := cnstXOrtAxis;
  case FSnapPlanCount of
  1: //Change base point
    Result := FIntersectNormalPlanSurf[FIndexSnapPlan];
  2: //Set plane by select entity
    begin//Surface, Line, Circle
      if Assigned(FSnapPlan[0].Obj) and (FSnapPlan[0].Obj.ClassType = TsgGLPolyline) then //Snap to edge
      begin
        vPoly := TsgGLPolyline(FSnapPlan[0].Obj);
        if vPoly.Radius > 0 then
        begin//Circle
          Result := vPoly.FNormal;
          //Result := sgCalcPlaneNormal(vPoly.List[0], vPoly.List[1], vPoly.List[2]);
        end
        else
        begin//Straight line
          Result := SubFPoint(vPoly.Last, vPoly.First);
        end;
      end
      else//Snap to surface
        Result := FIntersectNormalPlanSurf[FIndexSnapPlan];
    end;
  3: //Set plane by 3 points
    begin
      Result := sgCalcPlaneNormal(FIntersectPointPlanSufr[0], FIntersectPointPlanSufr[1], FIntersectPointPlanSufr[2]);
    end;
  end;
  vNormal := VectorMake(FPoint2Vect(Result));
  vNormal := FNav3D.GLFreeForm.LocalToAbsolute(vNormal);
  if VectorDotProduct(FNav3D.Camera.AbsoluteDirection, vNormal) < 0 then
  begin
    Result.X := - Result.X;
    Result.Y := - Result.Y;
    Result.Z := - Result.Z;
  end;
end;

function Tsg3DDrawingNavigator.GetSnapPlanPoint: TFPoint;
var
  vPoly: TsgGLPolyline;
begin
  Result := cnstFPointZero;
  case FSnapPlanCount of
  1:
    Result := FIntersectPointPlanSufr[FIndexSnapPlan];
  2:
    begin//Surface, Line, Circle
      if Assigned(FSnapPlan[0].Obj) and (FSnapPlan[0].Obj.ClassType = TsgGLPolyline) then //Snap to edge
      begin
        vPoly := TsgGLPolyline(FSnapPlan[0].Obj);
        if vPoly.Radius > 0 then
        begin//Circle
          Result := vPoly.FCenter;
        end
        else
        begin//Straight line
          Result := FIntersectPointPlanSufr[FIndexSnapPlan];
        end;
      end
      else//Snap to surface
        Result := FIntersectPointPlanSufr[FIndexSnapPlan];
    end;
  3:
    begin
      Result := AddFPoint(FIntersectPointPlanSufr[0], FIntersectPointPlanSufr[1]);
      Result := AddFPoint(Result, FIntersectPointPlanSufr[2]);
      Result.X := Result.X / 3;
      Result.Y := Result.Y / 3;
      Result.Z := Result.Z / 3;
    end;
  end;
end;
{$ENDIF}

function Tsg3DDrawingNavigator.GetMaterialByName(AMaterialName: string): TGLMaterial;
begin
  Result := FNav3D.GetMaterialByName(AMaterialName);
end;

//procedure GetOuterLines(AData: PsgTestLine); cdecl;
//var
//  vList: TAffineVectorList;
//
//  function VertexToAffineVector(const V: TsgVertex): TAffineFltVector;
//  begin
//    Result[0] := V.X;
//    Result[1] := V.Y;
//    Result[2] := V.Z;
//  end;
//
//begin
//  vList := TAffineVectorList(AData.Creator);
//  vList.Add(VertexToAffineVector(AData^.Point1));
//  vList.Add(VertexToAffineVector(AData^.Point2));
//end;

function Tsg3DDrawingNavigator.GetMeshLines(AMesh: TMeshObject;
  AList: TFPointList; AMatrix: PFMatrix): Boolean;
var
  vMesh: TsgMeshObject;
  vNode: TCustomNode;

  procedure DrawEdge(const AEdge: TsgModTopoEdge;  const AList: TFPointList);
  var
    I: Integer;
    vPoly: TsgModMeshPolyline;
    vNode: TsgModMeshPolylineNode;
    vList: TFPointList;
  begin
    vPoly := AEdge.ExtractFirstPolyline;
    if Assigned(vPoly) and Assigned(vPoly.FirstNode) then
    begin
      vList := TFPointList.Create;
      try
        vNode := vPoly.FirstNode;
        while Assigned(vNode) do
        begin
          vList.Add(vNode.Point);
          vNode := vNode.Next;
        end;
        if vList.Count >=2 then
        begin
          AList.Add(vList[0]);
          for I := 1 to vList.Count - 2 do
          begin
            AList.Add(vList[I]);
            AList.Add(vList[I]);
          end;
          AList.Add(vList[vList.Count - 1]);
        end;
      finally
        vList.Free;
      end;
    end;
  end;

  procedure ExtractShape(AShape: TsgModTopoShape; const AList: TFPointList);
  var
    I: Integer;
    vShape: TsgModTopoShape;

  begin
    case AShape.GetShapeType of
      btEdge:
      begin
        TsgModTopoEdge(AShape).ClearIsProcessed(True);
        DrawEdge(TsgModTopoEdge(AShape), AList);
        Exit;
      end;
      btWire: ;
      btFace: ;
    end;

    //Shape
    for I := 0 to AShape.SubShapesCount - 1 do
    begin
      vShape := AShape.Subshape[I];
      ExtractShape(vShape, AList);
    end;
  end;

begin
  if AMesh is TsgMeshObject then
  begin
    vMesh := TsgMeshObject(AMesh);
    if Assigned(vMesh.Entity) then
    begin
      if AMatrix <> nil then
      begin
        if vMesh.Node <> nil then
        begin
          TNodeLite(vMesh.Node).GetMatrix(AMatrix^);
          //
          vNode := TCustomNode.GetNodeOwnerByTopoClass(TCustomNode(FRoot),
            vMesh.Node, [TsgModTopoSolid, TsgModTopoShell]);
          AMatrix^ := FMatXMat(AMatrix^, FMatByTranslate(Vect2FPoint(vNode.Delta)));
        end
        else
          AMatrix^ := StdMat(cnstFPointSingle, FBoxOffs);
      end;
      if Assigned(AList) then
      begin
        if vMesh.Entity is TsgModEntity then
        begin
          TsgModTopoFace(vMesh.Entity).ClearIsProcessed(True);
          ExtractShape(TsgModTopoFace(vMesh.Entity), AList);
        end;
      end;
    end;
  end;
  Result := AList.Count > 0;
end;

function Tsg3DDrawingNavigator.GetMeshQuality: TsgMeshQuality;
begin
  if IsEqual(FDeviationCoefficient, cnstDeviationCoefficientDefault) then
    Result := mqLow
  else
    if IsEqual(FDeviationCoefficient, cnstDeviationCoefficientRealistic) then
      Result := mqHigh
    else
     Result := mqNormal;
end;

function Tsg3DDrawingNavigator.GetModelLayoutBox: TFRect;
begin
  if Assigned(FConverter) then
  begin
    Result := TsgDXFConverterAccess(FConverter).GetModelLayout.Box;
    case GetBoxType(Result) of
      bxX:
        begin
          Result.Top := Result.Top + (Result.Right - Result.Left) * 0.5;
          Result.Bottom := Result.Bottom - (Result.Right - Result.Left) * 0.5;
        end;
      bxY:
        begin
          Result.Left := Result.Left - (Result.Top - Result.Bottom) * 0.5;
          Result.Right := Result.Right + (Result.Top - Result.Bottom) * 0.5;
        end;
    end;
  end
  else
    Result := cnstBadRect;
end;

function Tsg3DDrawingNavigator.GetNavigator: Tsg3DNavigator;
begin
  Result := FNav3D;
end;

function Tsg3DDrawingNavigator.GetOrbit3DClass: TClass;
begin
  Result := Tsg3DDrwNavOrbit3D;
end;

function Tsg3DDrawingNavigator.GetPickObject: TObject;
begin
  Result := FPickObjects.Obj
end;

function Tsg3DDrawingNavigator.GetPoint(const Point: TFPoint): TFPoint;
begin
  Result := FTransformation.Transform(Point);
end;

function Tsg3DDrawingNavigator.GetPointf(const APoint: TFPoint): TAffineVector;
begin
  Result := FTransformation.Transformf(APoint);
end;

function Tsg3DDrawingNavigator.GetRealPoint(ACoordX, ACoordY: Double;
  var APointInUCS: TFPoint): TFPoint;
begin
  if not (dnsMoving in State) and not MouseCapture then
  begin
    FCoord := ClientToWorld(ACoordX, ACoordY, 0);
    FCoordInUCS := VectorTransform(FCoord, FNav3D.GLDCScene.InvAbsoluteMatrix);

    FCoord.V[0] := FCoord.V[0] - FBoxOffs.X;
    FCoord.V[1] := FCoord.V[1] - FBoxOffs.Y;
    FCoord.V[2] := FCoord.V[2] - FBoxOffs.Z;

    FCoordInUCS.V[0] := FCoordInUCS.V[0] - FBoxOffs.X;
    FCoordInUCS.V[1] := FCoordInUCS.V[1] - FBoxOffs.Y;
    FCoordInUCS.V[2] := FCoordInUCS.V[2] - FBoxOffs.Z;
  end;
  Result := Vect2FPoint(FCoord);
  APointInUCS := Vect2FPoint(FCoordInUCS);
end;

function Tsg3DDrawingNavigator.GetShowBoxDimensions: Boolean;
begin
  Result := FShowBoxDimensions;
end;

function Tsg3DDrawingNavigator.GetShowEdges: Boolean;
begin
  Result := FShowEdges;
end;

function Tsg3DDrawingNavigator.GetShowHighlight: Boolean;
begin
  Result := FHighLightObj.Enabled
end;

function Tsg3DDrawingNavigator.GetSnap: Boolean;
begin
  Result := FSnapObj[FIndexSnap].Enabled
end;

{$IFDEF USE_CLIP}
function Tsg3DDrawingNavigator.GetShowSnapPlan: Boolean;
begin
  Result := FSnapPlan[FIndexSnapPlan].Enabled
end;
{$ENDIF}

function Tsg3DDrawingNavigator.GetSnapEdge: Boolean;
begin
  Result := FSnapEdge.Enabled;
end;

function Tsg3DDrawingNavigator.GetShowingStyle: Tsg3DShowingStyles;
begin
  Result := FShowingStyle;
end;

function Tsg3DDrawingNavigator.GetSizesAsString: string;
var
  vTriangCount: Integer;
begin
  vTriangCount := FNav3D.TrianglesCount;
  Result := Format(STriangleCount, [vTriangCount]);
end;

procedure Tsg3DDrawingNavigator.GetView(var AMatrix: TFMatrix);
var
  vPos2D, vTranslate: TAffineFltVector;
  vAABB: TAABB;
  P: TFPoint;
  vScale: TFPoint;
  vMf4: TMatrix4f;
  vRect: TFRect;
begin
  vMf4 := FNav3D.GLDCScene.Matrix;
  {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NormalizeMatrix(vMf4);
  GLMatrixToMatrix(vMf4, AMatrix);
  GetScreenAABB(vAABB);
  if Assigned(FConverter) then
    TsgDXFConverterAccess(FConverter).GetModelLayout.SetRotMatrix(AMatrix);
  vRect := GetBox;
  TransRectCorners(vRect, AMatrix);
  vScale.X := 1.0/CalcSceneScale(vAABB, vRect);
  vScale.Y := -vScale.X;
  vScale.Z := vScale.X;
  AMatrix := FMatScale(AMatrix, vScale);
  vTranslate := VectorTransform(FNav3D.FCenter, FNav3D.GLDCScene.Matrix);
  if TGLSceneBufferAccess(FNav3D.Buffer).RenderDPI = 0 then
    FNav3D.Buffer.Render(nil);
  vPos2D := FNav3D.Buffer.WorldToScreen(vTranslate);
  vPos2D.V[1] := FNav3D.Buffer.Height - vPos2D.V[1];
  // additional scale and translate
  P := SubFPoint(Vect2FPoint(FNav3D.FCenter), FBoxOffs);
  P := FPointXMat(P, AMatrix);
  AMatrix.E0.X := vPos2D.V[0] - P.X;
  AMatrix.E0.Y := vPos2D.V[1] - P.Y;
  //  AMatrix.E0.Z := vPos2D[2] - P.Z;
end;

function Tsg3DDrawingNavigator.GetZoomInEnable: Boolean;
const
  cnstBigValue = 9000000;
var
  vAABB: TAABB;
  vWidth, vHeight: Double;
begin
  Result := False;
  if FNav3D.Buffer.RenderingContext = nil then Exit;
  GetScreenAABB(vAABB);
  vWidth:= vAABB.max.V[0] - vAABB.min.V[0];
  vHeight := vAABB.max.V[1] - vAABB.min.V[1];
  Result := (vWidth < cnstBigValue) and (vHeight < cnstBigValue);
end;

function Tsg3DDrawingNavigator.GetZoomOutEnable: Boolean;
var
  vAABB: TAABB;
  vWidth, vHeight: Double;
begin
  Result := False;
  if FNav3D.Buffer.RenderingContext = nil then Exit;
  GetScreenAABB(vAABB);
  vWidth:= vAABB.max.V[0] - vAABB.min.V[0];
  vHeight := vAABB.max.V[1] - vAABB.min.V[1];
  Result := (vWidth > iMinPicSize) and (vHeight > iMinPicSize);
end;

procedure Tsg3DDrawingNavigator.HighLightObjectEvent(Sender: TObject);
begin
  FHighLightObj.BuildLines;
  Navigator3DAfterRender(nil);
  RepaintScene;
  if Assigned(FOnHighLight) then
    FOnHighLight(Self);
end;

procedure Tsg3DDrawingNavigator.SnapObjEvent(Sender: TObject);
begin
  TsgPickObject(Sender).BuildLines;
  Navigator3DAfterRender(nil);
  RepaintScene;
  if Assigned(FOnHighLight) then
    FOnHighLight(Self);
end;

{$IFDEF USE_CLIP}
procedure Tsg3DDrawingNavigator.SnapPlanEvent(Sender: TObject);
begin
  TsgPickObject(Sender).BuildLines;
  Navigator3DAfterRender(nil);
  RepaintScene;
  if Assigned(FOnHighLight) then
    FOnHighLight(Self);
end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.StopMouseMoveOperationsHandler(
  var Message: TMessage);
begin
  if Message.Msg <> CM_MOUSELEAVE then
    inherited StopMouseMoveOperationsHandler(Message);
end;

procedure Tsg3DDrawingNavigator.SnapDimObjEvent(Sender: TObject);
begin
  FSnapDim.BuildLines;
  Navigator3DAfterRender(nil);
  RepaintScene;
end;

procedure Tsg3DDrawingNavigator.SnapEdgeObjEvent(Sender: TObject);
begin
  FSnapEdge.BuildLines;
  Navigator3DAfterRender(nil);
  RepaintScene;
//  if Assigned(FOnHighLight) then
//    FOnHighLight(Self);
end;

procedure Tsg3DDrawingNavigator.DimensionObjEvent(Sender: TObject);
var
  vDimObj: TsgPickObject;
begin
  if Assigned(FMoveDim) then
  begin
    TsgPickObject(FMoveDim.Obj).BuildLines;
    Navigator3DAfterRender(nil);
    RepaintScene;
  end
  else
  begin
    vDimObj := GetDimension;
    if Assigned(vDimObj) then
    begin
      vDimObj.BuildLines;
      Navigator3DAfterRender(nil);
      RepaintScene;
      if Assigned(FOnHighLight) then
         FOnHighLight(Self);
    end;
  end;
end;

function Tsg3DDrawingNavigator.InitializeIterateParams: PsgCADIterate;
begin
  Result := inherited InitializeIterateParams;
  Result^.Matrix.E0 := FBoxOffs;
end;

function Tsg3DDrawingNavigator.IsCanBeEdited: Boolean;
begin
  Result := False;
end;

function Tsg3DDrawingNavigator.IsCanShowCoords: Boolean;
begin
  Result := True;
end;

function Tsg3DDrawingNavigator.IsCanShowSizes: Boolean;
begin
  Result := True;
end;

function Tsg3DDrawingNavigator.IsSelectByBody(const AShift: TShiftState): Boolean;
begin
  if FMeasureMode3D = mmHighLighting then
    Result := False
  else
  begin
    Result := (not FSelectByFace);
    if (AShift * [ssShift] <> []) then
      Result := not Result;
  end;
end;

function Tsg3DDrawingNavigator.IsSimple3DBody: Boolean;
begin
  Result := False;
end;

function Tsg3DDrawingNavigator.IsUseGraphic: Boolean;
begin
  Result := False;
end;

procedure Tsg3DDrawingNavigator.VisibleChanging;
begin
  inherited VisibleChanging;
  if Assigned(FLoadFromImage) and Assigned(FConverter) and FIsLoadedFromConv then
    if not Visible then
    begin
      LoadView(FLoadFromImage.DrawMatrix);
    end;
end;

procedure Tsg3DDrawingNavigator.WndProc(var Message: TMessage);
var
  R: TRect;
  vIterate: Boolean;
begin
  vIterate := False;
  if (Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST) then
  begin
    case TWMKey(Message).CharCode of
      VK_ESCAPE: vIterate := MeasureDimensionEsc;
    end;
  end;
  if not vIterate then
  begin
    case Message.Msg of
      WM_CREATE:
        begin
          {$IFDEF MSWINDOWS}
          if WindowHandle <> 0 then
            Windows.GetClientRect(WindowHandle, R)
          else
          {$ENDIF}
            R := Rect(0, 0, Width, Height);
          FOwnDC := GetDC(WindowHandle);
          FNav3D.Width := R.Right - R.Left;
          FNav3D.Height := R.Bottom - R.Top;
          inherited WndProc(Message);
          FNav3D.DoCreateRC;
        end;
    else
      inherited WndProc(Message);
    end;
  end;
  if Message.Msg = WM_USER + 1 then
  begin
    if Assigned(LoadFromImage) then
      FDrawMode := LoadFromImage.DrawMode;
    UpdateMaterialLibrary;
    RepaintScene;
  end;
end;

function Tsg3DDrawingNavigator.ZoomRectEx2D(const ARect: TF2DRect): Boolean;
begin
  ZoomRectEx(MakeFRectFromF2dRect(ARect), False);
end;

function Tsg3DDrawingNavigator.ZoomRectEx(ARect: TFRect; const IsRealBox: Boolean): Boolean;
var
  vDrawRect: TsgDrawRect;
  vDrawMatrix: TFMatrix;
  vSize, vCenter: TF2DPoint;

  function Split2DRect(const R: TF2DRect; var ASize: TF2DPoint): TF2DPoint;
  begin
    ASize.X := Abs(R.Right - R.Left);
    ASize.Y := Abs(R.Bottom - R.Top);
    Result.X := 0.5 * (R.Left + R.Right);
    Result.Y := 0.5 * (R.Top + R.Bottom);
  end;

begin
  vDrawRect := TsgDrawRect.Create;
  try
    vDrawRect.AttachMatrix(@vDrawMatrix);
    vDrawRect.Box := GetBox;
    GetView(vDrawMatrix);
    if IsRealBox then
      ARect := GetRealBox(ARect, vDrawRect.Matrix^);
    vCenter := Split2DRect(MakeF2dRectFromFRect(ARect), vSize);
    Result := vDrawRect.Zoom(Rect(0, 0, ClientWidth, ClientHeight), vCenter, vSize);
    vDrawRect.DettachMatrix;
    LoadView(vDrawMatrix);
  finally
    vDrawRect.Free;
  end;
end;

procedure Tsg3DDrawingNavigator.DoPictureChange(Sender: TObject);
var
  vImage: TsgCADImage;
begin
  if Assigned(FLoadFromImageChange) then
    FLoadFromImageChange(Sender);
  vImage := nil;
  if Sender is TsgCADImage then
    vImage := TsgCADImage(Sender)
  else
    if (Sender is TPicture) and (TPicture(Sender).Graphic is TsgCADImage) then
      vImage := TsgCADImage(TPicture(Sender).Graphic);
  if ControlTool.Img <> vImage then
    ControlTool.UpdateImg;
  if Assigned(vImage) then
  begin
    if FDrawMode <> vImage.DrawMode then
    begin
      FDrawMode := vImage.DrawMode;
      UpdateMaterialLibrary;
      Invalidate;
    end;
  end
  else
  begin
    Clear;
    LoadFromImage := nil;
    FConverter := nil;
  end;
end;

procedure Tsg3DDrawingNavigator.LoadView(const AMatrix: TFMatrix;
  const APostMessage: Boolean = True);
var
  I: Integer;
  vSceneScale: Double;
  vPos2D, vTranslate: TAffineFltVector;
  vfM: TMatrix4f;
  vBox, vRect: TFRect;
  vBoxCenter, vRectCenter, vSize: TFPoint;
  vRC: TGLContext;
  vAABB: TAABB;
  vBounds: THmgBoundingBox;
  vDPI: Integer;
  vPixelsPerInch: TPoint;
  vMatrix: TFMatrix;
begin
  HandleNeeded;
  vBox := GetBox;
  vRect := vBox;
  vMatrix := FMatScale(AMatrix, MakeFPoint(1, -1, 1));

  OffsetFRect(vRect, -FBoxOffs.X, -FBoxOffs.Y, -FBoxOffs.Z);
  TransRectCorners(vRect, vMatrix);
  vRectCenter := MiddleFPoint(vRect.TopLeft, vRect.BottomRight);

  vBoxCenter := MiddleFPoint(vBox.TopLeft, vBox.BottomRight);


  MakeVector(FNav3D.FAABBBox.min, vBox.Left, vBox.Bottom, vBox.Z1);
  MakeVector(FNav3D.FAABBBox.max, vBox.Right, vBox.Top, vBox.Z2);
  FNav3D.FCenter := FPoint2Vect(vBoxCenter);
  // get rotmatrix
  MatrixToGLMatrix(vMatrix, vfM);
  {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NormalizeMatrix(vfM);
  FNav3D.GLDCScene.BeginUpdate;
  Lock.Enter;
  try
    FNav3D.Camera.BeginUpdate;
    try
      FNav3D.FAxes.ResetRotations;
      vDPI := TGLSceneBufferAccess(FNav3D.Buffer).RenderDPI;
      if vDPI = 0 then
      begin
        vPixelsPerInch := sgFunction.GetLogPixelsPerInch;
        vDPI := MaxI(vPixelsPerInch.X, vPixelsPerInch.Y);
        if vDPI = 0 then
          vDPI := 96;
      end;

      FNav3D.InitSceneObject(FNav3D.GLDCScene,
        VectorTransform(VectorNegate(FNav3D.FCenter), vfM), {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.ZVector, 0);
      vSize := AbsFPoint(SubFPoint(vBox.BottomRight, vBox.TopLeft));
      FNav3D.GLDCScene.CubeSize := Max(vSize.Z, Max(vSize.X, vSize.Y));

      //
      FNav3D.FGLCubeExtents.CubeWidth := vSize.X;
      FNav3D.FGLCubeExtents.CubeHeight := vSize.Y;
      FNav3D.FGLCubeExtents.CubeDepth := vSize.Z;
      FNav3D.FGLCubeExtents.Position.AsAffineVector := FPoint2Vect(vBoxCenter);
      FNav3D.FBoxImage := vBox;
      //

      FNav3D.Camera.SceneScale := 1;
      FNav3D.Camera.DepthOfView := 10 * FNav3D.GLDCScene.BoundingSphereRadius;
      FNav3D.InitSceneObject(FNav3D.Camera, {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NullVector, {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.ZVector, 3 * FNav3D.GLDCScene.CubeSize);
      FNav3D.Camera.NearPlaneBias := 0.5;

      vRC := CurrentGLContext;

      if not Assigned(vRC) then
      begin
        FNav3D.ForceRenderingContext;
        FNav3D.Buffer.RenderingContext.Activate;
      end;

      try
        FNav3D.Buffer.RenderingContext.PipelineTransformation.IdentityAll;
        FNav3D.Camera.ApplyPerspective(FNav3D.Buffer.ViewPort, FNav3D.Buffer.ViewPort.Width,
          FNav3D.Buffer.ViewPort.Height, vDPI);
      finally
        if not Assigned(vRC) then
          FNav3D.Buffer.RenderingContext.Deactivate;
      end;

      // rotate scene
      FNav3D.GLDCScene.Matrix := MatrixMultiply(vfM, FNav3D.GLDCScene.Matrix);
      FNav3D.FAxes.FGLDCAxes.Matrix := MatrixMultiply(vfM, FNav3D.FAxes.FGLDCAxes.Matrix);
      // calculate SceneScale
      GetBoundingBox(vBounds);
      if FNav3D.CameraStyle = csOrthogonal then
        GetScreenAABB(vAABB)
      else
      begin
        FNav3D.Buffer.WorldToScreen(@vBounds.BBox[0], 8);
        for I := 0 to 7 do
          vBounds.BBox[I] := FNav3D.Buffer.ScreenToVector(vBounds.BBox[I]);
        vAABB := BBToAABB(vBounds);
      end;
      vSceneScale := CalcSceneScale(vAABB, vRect);
      FDimSize := (vAABB.Max.Y - vAABB.Min.Y) / 250;
      if IsZero(FDimSize) then
        FDimSize := (1.0/vSceneScale)/ 250;
      { TODO: calc vSceneScale for CameraStyle = csPerspective in LoadView method }
      if FNav3D.CameraStyle <> csOrthogonal then
        vSceneScale := vSceneScale * FNav3D.GLDCScene.BoundingSphereRadius / 16;
      FNav3D.Camera.SceneScale := vSceneScale;
      // translate scene to glscene to viewrect center
      vPos2D := FPoint2Vect(vRectCenter);
      vPos2D.V[1] := FNav3D.Buffer.ViewPort.Height + vPos2D.V[1];
      vPos2D.V[2] := 0;
      vTranslate := FNav3D.Buffer.ScreenToWorld(vPos2D);
      vTranslate.V[2] := 0;
      FNav3D.Translate(VectorScale(vTranslate, 1 / vSceneScale));
      if APostMessage then
        PostMessage(Handle, WM_USER + 1, 0, 0);
    finally
      FNav3D.Camera.EndUpdate;
    end;
  finally
    FNav3D.GLDCScene.EndUpdate;
    Lock.Leave;
  end;
end;

{$IFDEF CLIP_CGLCLIP_GLCSG_GLMESH_SGMESH}
procedure Tsg3DDrawingNavigator.SaveOutline;
begin
{$IFDEF CLIP_GLCSG_GLMESH_SGMESH}
  if not Assigned(FNav3D.FLineOuterWireOld) then
  begin
    FNav3D.FLineOuterWireOld := FNav3D.FLineOuterWire;
    if Assigned(FNav3D.FLineOuterWire) then
      FNav3D.FLineOuterWire.Visible := False;
    FNav3D.FLineOuterWire := TsgGLLines.CreateAsChild(FNav3D.GLFreeForm);
    FNav3D.FLineOuterWire.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
  end;

  if not Assigned(FNav3D.FLineMassOld) then
  begin
    FNav3D.FLineMassOld := FNav3D.FLineMass;
    if Assigned(FNav3D.FLineMass) then
      FNav3D.FLineMass.Visible := False;
    FNav3D.FLineMass := TsgGLLines.CreateAsChild(FNav3D.GLFreeForm);
    FNav3D.FLineMass.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
  end;
{$ENDIF}
end;

procedure Tsg3DDrawingNavigator.RestoreOutline;
{$IFDEF CLIP_GLCSG_GLMESH_SGMESH}
var
  vTemp: TsgGLLines;
{$ENDIF}
begin
{$IFDEF CLIP_GLCSG_GLMESH_SGMESH}
  if Assigned(FNav3D.FLineOuterWireOld) then
  begin
    vTemp := FNav3D.FLineOuterWire;
    FNav3D.FLineOuterWire := FNav3D.FLineOuterWireOld;
    vTemp.Free;
    FNav3D.FLineOuterWireOld := nil;
    if Assigned(FNav3D.FLineOuterWire) then
      FNav3D.FLineOuterWire.Visible := IsShowEdges;
  end;

  if Assigned(FNav3D.FLineMassOld) then
  begin
    vTemp := FNav3D.FLineMass;
    FNav3D.FLineMass := FNav3D.FLineMassOld;
    vTemp.Free;
    FNav3D.FLineMassOld := nil;
    if Assigned(FNav3D.FLineMass) then
      FNav3D.FLineMass.Visible := True;
  end;
{$ENDIF}
end;



procedure Tsg3DDrawingNavigator.ApplyClip;
{$IFDEF CLIP_GLMESH}
var
  I: Integer;
  vMesh: TMeshObject;
  vLastOriginalMesh, vIndexAddMesh(*, vIndexResulMesh*): Integer;
  vData: TsgClipPLaneData;
  vDataMesh: TsgEndMakeShapeData;
  vParams: TsgParamTriangle;
  vParamsDLL: TsgDLLParam;
  vList3DFace: TList;
  vPoints: TFPointList;
  J, M: Integer;
  AData: TsgTestLine;

{$ENDIF}

  procedure AfterProcessing(var ALine: TsgGLLines; AIsPreProcessing: Boolean);
  begin
    if ALine.FPolylines.Count = 0 then
    begin
      FNav3D.GLFreeForm.Remove(ALine, False);
      ALine.Free;
      ALine := nil;
    end
    else
      if AIsPreProcessing then
        ALine.PreProcessing(FProgressObject, FAccuracyDim, FRoot);
  end;

{$IFDEF CLIP_SGMESH}
var
  I, J, M, N: Integer;
  vMat: TMatrix4f;
  vMesh: TsgMeshObject;
  vLineList: TList;
  vVertexList: TAffineVectorList;
  vEdgePointList: TFPointList;
  vEdgePolyline: TsgGLPolyline;
  vIsFindIndex: Boolean;
{$ENDIF}

begin
{$IFDEF CLIP_GLMESH_SGMESH}
  if FNav3D.FGLFreeForm.MeshObjects.Count <> 0 then
  begin
    //vLastOriginalMesh := FNav3D.FLastOriginalMesh;
    //Edge
    FNav3D.FLineOuterWire.Free;
    FNav3D.FLineOuterWire := TsgGLLines.CreateAsChild(FNav3D.GLFreeForm);
    FNav3D.FLineOuterWire.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;

    FNav3D.FLineMass.Free;
    FNav3D.FLineMass := TsgGLLines.CreateAsChild(FNav3D.GLFreeForm);
    FNav3D.FLineMass.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
{$IFDEF CLIP_SGMESH}
    FNav3D.RestoreOriginalMeshData;
    {for I := 0 to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
    begin
      TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).RestoreData;
      if FNav3D.FGLFreeForm.MeshObjects.Items[I].Normals.Count = 0 then
        GenNormals(FNav3D.FGLFreeForm.MeshObjects[I]);
    end;}

    if Assigned(FNAV3D.FCuttingPlanes) and (FNAV3D.FCuttingPlanes.PlaneCount > 0)  then
    begin
      vMat := FNav3D.FGLFreeForm.Matrix;
      //MatrixInvert(vMat);
      FNAV3D.FCuttingPlanes.Prepare(vMat);
      //Cutting Lines
      if Assigned(FNav3D.FLineMassOld) then
        for I := 0 to FNav3D.FLineMassOld.FPolylines.Count - 1 do
        begin
          vVertexList := TAffineVectorList.Create;
          vEdgePolyLine := TsgGLPolyline(FNav3D.FLineMassOld.FPolylines[I]);
          for J := 0 to vEdgePolyLine.Count - 1 do
            vVertexList.Add(FPoint2Vect(vEdgePolyLine.Items[J]));
          FNAV3D.FCuttingPlanes.Apply(vVertexList, vLineList);
          vVertexList.Free;
          for J := 0 to vLineList.Count - 1 do
          begin
            vVertexList := TAffineVectorList(vLineList[J]);
            if vVertexList.Count > 1 then
            begin
              CreateGLSceneLine(vEdgePolyLine, FOuterWireColor, TsgGLPolyline(FNav3D.FLineMassOld.FPolylines[I]).LineWidth, tlMass);
              vEdgePolyLine.MaterialName := TsgGLPolyline(FNav3D.FLineMassOld.FPolylines[I]).MaterialName;
              vEdgePolyLine.MaterialNamePoint := TsgGLPolyline(FNav3D.FLineMassOld.FPolylines[I]).MaterialNamePoint;
              vEdgePolyLine.MaterialNameText := TsgGLPolyline(FNav3D.FLineMassOld.FPolylines[I]).MaterialNameText;
              vEdgePolyLine.MaterialNameLine := TsgGLPolyline(FNav3D.FLineMassOld.FPolylines[I]).MaterialNameLine;
              for M := 0 to vVertexList.Count - 1 do
                vEdgePolyLine.Add(Vect2FPoint(vVertexList[M]));
            end;
            vVertexList.Free;
          end;
          vLineList.Free;
        end;
      //Cutting Edges
      FNav3D.SaveOriginalMeshData;

      if Assigned(FNav3D.FLineOuterWireOld) then
        for I := 0 to FNav3D.FLineOuterWireOld.FPolylines.Count - 1 do
        begin
          vEdgePolyLine := TsgGLPolyline(FNav3D.FLineOuterWireOld.FPolylines[I]);
          FNAV3D.FCuttingPlanes.Apply(vEdgePolyline, vLineList);
          for J := 0 to vLineList.Count - 1 do
          begin
            if Assigned(vLineList[J]) and (TFPointList(vLineList[J]).Count > 1) then
            begin
              vEdgePointList := TFPointList(vLineList[J]);
              CreateGLSceneLine(vEdgePolyLine, FOuterWireColor, FOuterWireLineWieght, tlOuterWire);
              vEdgePolyline.AppendDynArray(vEdgePointList);
              for M := 0 to TsgGLPolyline(FNav3D.FLineOuterWireOld.FPolylines[I]).FMeshList.Count - 1 do
              begin
                vMesh := TsgMeshObject(TsgGLPolyline(FNav3D.FLineOuterWireOld.FPolylines[I]).FMeshList[M]);
                if Assigned(vMesh) then
                begin
                  // Replace old indexes with new ones in the mesh.
                  // If the edge has multiplied, add the index to the mesh.
                  vIsFindIndex := False;
                  for N := 0 to vMesh.FEdgeIndexList.Count - 1 do
                  begin
                    if vMesh.FEdgeIndexList[N] = I then
                    begin
                      vMesh.FEdgeIndexList[N] := FNav3D.FLineOuterWire.FPolylines.Count - 1 + 10000;
                      vIsFindIndex := True;
                    end;
                  end;
                  if not vIsFindIndex then
                  begin
                    vMesh.FEdgeIndexList.Add(FNav3D.FLineOuterWire.FPolylines.Count - 1 + 10000)
                  end;
                end;
                vEdgePolyLine.FMeshList.Add(vMesh);
              end;
              vEdgePolyLine.FCountUses := TsgGLPolyline(FNav3D.FLineOuterWireOld.FPolylines[I]).FMeshList.Count;
              vEdgePointList.Free;
            end;
          end;
          vLineList.Free;
        end;
      //Cutting Meshes

      for I := 0 to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
      begin
        vMesh :=  TsgMeshObject.CreateWithEnt(nil, nil);
        try
          vMesh.Assign(FNav3D.FGLFreeForm.MeshObjects.Items[I]);
          {TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).SaveData;}
          FNav3D.FGLFreeForm.MeshObjects.Items[I].Clear;
          if FNav3D.FGLFreeForm.MeshObjects.Items[I] is TsgMeshObject then
            TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).FEdgeIndexList.Clear;
          FNAV3D.FCuttingPlanes.Apply(vMesh, FNav3D.FGLFreeForm.MeshObjects.Items[I], vLineList);
          if FNav3D.FGLFreeForm.MeshObjects.Items[I] is TsgMeshObject then
          begin
            for N := vMesh.FEdgeIndexList.Count - 1 downto 0 do
            begin
              if vMesh.FEdgeIndexList[N] >= 10000 then
              begin
                TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).
                  FEdgeIndexList.Add(vMesh.FEdgeIndexList[N] - 10000);
              end
              else
                vMesh.FEdgeIndexList.Delete(N);
            end;
          end;

          for J := 0 to vLineList.Count - 1 do
          begin
            vVertexList := TAffineVectorList(vLineList[J]);
            if vVertexList.Count > 1 then
            begin
              CreateGLSceneLine(vEdgePolyLine, FCrossSectionEdgesColor, FOuterWireLineWieght, tlOuterWire);
              for M := 0 to vVertexList.Count - 1 do
                vEdgePolyLine.Add(Vect2FPoint(vVertexList[M]));
              vEdgePolyLine.FMeshList.Add(FNav3D.FGLFreeForm.MeshObjects.Items[I]);
              if FNav3D.FGLFreeForm.MeshObjects.Items[I] is TsgMeshObject then
                TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).
                  FEdgeIndexList.Add(FNav3D.FLineOuterWire.FPolylines.Count - 1);
              vEdgePolyLine.FCountUses := 1;
            end;
            vVertexList.Free;
          end;
          vLineList.Free;
        finally
          vMesh.Free;
        end;
      end;
    end;
    AfterProcessing(Navigator3D.FLineOuterWire, False);
    AfterProcessing(Navigator3D.FLineMass, False);
    if Assigned(Navigator3D.FLineOuterWire) then
      Navigator3D.FLineOuterWire.Visible := IsShowEdges;
    if Assigned(Navigator3D.FLineMass) then
      Navigator3D.FLineMass.Visible := True;
    AppShowingStyle;
    FNav3D.FGLFreeForm.StructureChanged;
  end;
{$ENDIF}

{$IFDEF CLIP_GLMESH}
    vLastOriginalMesh := FNav3D.FLastOriginalMesh;
    if vLastOriginalMesh < FNav3D.FGLFreeForm.MeshObjects.Count-1 then
    begin
      while vLastOriginalMesh < FNav3D.FGLFreeForm.MeshObjects.Count-1 do
        FNav3D.FGLFreeForm.MeshObjects.DeleteAndFree(FNav3D.FGLFreeForm.MeshObjects.Count-1);
    end;
    //
    //
    //
    vData.ClipIndex := -1;
    vData.Positon.Center := cnstBadPoint;
    vData.Positon.Normal := cnstBadPoint;
    vData.MeshData := @vDataMesh;
    //
    vParamsDLL.GetTriangleFuncParam := @vParams;
    //
    vDataMesh.Params := @vParamsDLL;
    vDataMesh.GetTriangleFunc := @GetTriangle;
    vDataMesh.NewFaceGroupFunc := @NewFaceGroup;
    vDataMesh.BeginSegment := nil;
    vDataMesh.EndSegment := nil;
    //
    FCurrColor := clRed;
    FCurrMeshObject := CreateMeshObject(FNav3D.GLFreeForm.MeshObjects, nil);
    vParams.ASelf := Self;
    vParams.MeshPointer := FCurrMeshObject;
    vParams.VertexPointer := nil;

    sgAddClipPlane(FNav3D.FMeshContext [0], vData);

    FinalMeshObject(FCurrMeshObject, False);

    vIndexAddMesh := FNav3D.FGLFreeForm.MeshObjects.Count-1;
//    vMesh := FNav3D.FGLFreeForm.MeshObjects[vIndexAddMesh];

    for I := 0 to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
    begin
        if FNav3D.FGLFreeForm.MeshObjects[I].Normals.Count = 0 then
          GenNormals(FNav3D.FGLFreeForm.MeshObjects[I]);
    end;

    for I := 0 to vLastOriginalMesh do
    begin
      vMesh := CreateMeshObject(FNav3D.GLFreeForm.MeshObjects, nil);
      // CSG_Subtraction   CSG_Intersection  CSG_Union

      CSG_Operation(FNav3D.FGLFreeForm.MeshObjects.Items[vIndexAddMesh], FNav3D.FGLFreeForm.MeshObjects.Items[I],
        CSG_Subtraction,vMesh,
        FNav3D.FGLFreeForm.MeshObjects.Items[I].FaceGroups[0].MaterialName,
        FNav3D.FGLFreeForm.MeshObjects.Items[I].FaceGroups[0].MaterialName
        );
    end;

    for I := 0 to vIndexAddMesh do
    begin
      FNav3D.FGLFreeForm.MeshObjects.Items[I].Visible := False;
    end;



    try
      for M := vIndexAddMesh+1 to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
      begin
        vList3DFace := BuildLoopsByMesh(FNav3D.FGLFreeForm.MeshObjects.Items[M], cnstIdentityMat, cnstAccuracyParam);
        try
          AData.Creator := Self;
          for I := 0 to vList3DFace.Count - 1 do
          begin
            vPoints := TFPointList(vList3DFace[I]);
            if vPoints.Count < 2 then
              Continue;
            for J := 0 to vPoints.Count - 2 do
            begin
              AData.Point1 := TFPoint(vPoints[J]);
              AData.Point2 := TFPoint(vPoints[J+1]);
              AData.SegmentNumber := J;
              AData.Normal := cnstXOrtAxis;
              CallBackOuterLineDrawReal(@AData);
            end;
          end;
        finally
          FreeList(vList3DFace);
        end;
      end;
      FAccuracyDim := cnstAccuracyDimSTL;
    finally
      FProgressObject.Starting;
      I := Navigator3D.FLineOuterWire.FPolylines.Count + 1;
      FProgressObject.Step := 100 * (1 - cnstLoadDataBaseProgressPart) / I;
      AfterProcessing(Navigator3D.FLineOuterWire, True);
      if Assigned(Navigator3D.FLineOuterWire) then
        Navigator3D.FLineOuterWire.Visible := IsShowEdges;
      FProgressObject.Ending;
    end;

    FNav3D.FGLFreeForm.StructureChanged;




  end;
{$ENDIF}
{$ENDIF}
end;

procedure Tsg3DDrawingNavigator.AddClip(AClipPlan: Integer; APointPlan, ANormal: TFPoint);
var
  vFirst: Boolean;
begin
  if FNav3D.FCuttingPlanes.PlaneCount = 0 then
    vFirst := True
  else
    vFirst := False;
  if not FNav3D.IsClipPlaneUses then
    SaveOutline;
  FNav3D.AddClip(AClipPlan, APointPlan, ANormal);
  if vFirst then
    FNav3D.FCrossSectionOn := True;
  ApplyClip;
end;

procedure Tsg3DDrawingNavigator.RemoveClip(AClipPlan: Integer);
{$IFDEF CLIP_GLCSG_GLMESH}
var
  I: Integer;
{$ENDIF}
begin
  FNav3D.RemoveClip(AClipPlan);
  if not FNav3D.IsClipPlaneUses then
  begin
    FNav3D.FCrossSectionOn := False;
{$IFDEF CLIP_GLCSG_GLMESH_SGMESH}
{$IFDEF CLIP_SGMESH}
    FNav3D.RestoreOriginalMeshData;
    {for I := 0 to FNav3D.FLastOriginalMesh do
    begin
      TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).RestoreData;
    end;}
    FNav3D.GLFreeForm.StructureChanged;
{$ELSE}
    if FNav3D.FLastOriginalMesh < FNav3D.FGLFreeForm.MeshObjects.Count-1 then
    begin
      while FNav3D.FLastOriginalMesh < FNav3D.FGLFreeForm.MeshObjects.Count-1 do
        FNav3D.FGLFreeForm.MeshObjects.DeleteAndFree(FNav3D.FGLFreeForm.MeshObjects.Count-1);
    end;
    for I := 0 to FNav3D.FLastOriginalMesh do
    begin
      FNav3D.FGLFreeForm.MeshObjects.Items[I].Visible := True;
    end;
{$ENDIF}
    RestoreOutline;
{$ENDIF}
    AppShowingStyle;
  end
  else
    ApplyClip;
end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.Create3DFaceMeshObject(AEntity: TsgDXFEntity);
begin
  FCurrColor3dFace := FCurrColor;
  FCurrMeshObject3dFace := CreateMeshObject(FNav3D.GLFreeForm.MeshObjects, AEntity);
  FCurrVertexes3dFace := GetFaceGroup(FCurrMeshObject3dFace, fgmmTriangles, FCurrColor,
      TFGVertexIndexList);
end;

function Tsg3DDrawingNavigator.CreateDimension;
var
  vDimensionObj: TsgPickObject;
begin
  vDimensionObj := TsgPickObject.Create(Self);
  vDimensionObj.Color := Measure3DParams.DimensionLineColor;
  vDimensionObj.OnPick := DimensionObjEvent;
  vDimensionObj.Lines.ObjectStyle := vDimensionObj.Lines.ObjectStyle +
    [osIgnoreDepthBuffer, osDirectDraw];
  vDimensionObj.Lines.Material.MaterialLibrary := FNav3D.FGLMaterialLibrary;
  vDimensionObj.Enabled := True;
  Result := vDimensionObj;
  if not Assigned(FNav3D.FDimensionList) then
    FNav3D.FDimensionList := TsgObjectList.Create;
  FNav3D.FDimensionList.Add(vDimensionObj);
end;

procedure Tsg3DDrawingNavigator.Load3dFaceMeshObject;
var
  OldCurrMeshObject: TMeshObject;
  OldCurrVertexes: TFGVertexIndexList;
begin
  if (FCurrMeshObject3dFace <> nil) and (FCurrVertexes3dFace <> nil) then
  begin
    OldCurrMeshObject := FCurrMeshObject;
    OldCurrVertexes := FCurrVertexes;
    try
      FCurrVertexes := FCurrVertexes3dFace;
      FCurrMeshObject := FCurrMeshObject3dFace;
      FinalMeshObject(FCurrMeshObject, FNeedNormals3dFace);
    finally
      FCurrVertexes := OldCurrVertexes;
      FCurrMeshObject := OldCurrMeshObject;
    end;
  end;
end;

{$IFDEF DEBUG_STRUCTURE}
procedure Tsg3DDrawingNavigator.UpdateRootToTreeView(AParent: TTreeNode; ANodeLine: TCustomNode);
begin
  FTreeView.Items.Clear;
  ANodeLine.NormalizeVisibilityTree;
  RootToTreeView(AParent, ANodeLine);
  FTreeView.FullExpand;
end;

procedure Tsg3DDrawingNavigator.RootToTreeView(AParent: TTreeNode; ANodeLine: TCustomNode);
var
  I: Integer;
  vNode: TTreeNode;
  S: String;
begin
  if Assigned(ANodeLine) then
  begin
    S := Format('(%s) - ', [BoolToStr(TCustomNode(ANodeLine).FVisible)]);
    S := S + Format('%s - %s', [ANodeLine.ClassName, ANodeLine.Obj.ClassName]);
    vNode := FTreeView.Items.AddChild(AParent,S);
    for I := 0 to ANodeLine.Count - 1 do
    begin
      RootToTreeView(vNode, TCustomNode(ANodeLine.Items[I]));
    end;
  end;

end;

{$ENDIF}

procedure Tsg3DDrawingNavigator.LoadFromConverter(const AConverter: TsgDXFConverter;
  const ACADImage: TsgCADImage);
var
  vGLForm: TGLBaseMeshAccess;//TGLFreeForm;
  vBox: TFRect;
  I: Integer;
{$IFDEF LOOP_STL}
  vList3DFace: TList;
  vPoints: TFPointList;
  J, K: Integer;
  AData: TsgTestLine;
  vInsertStl: TsgDXFInsert;
{$ENDIF}
{$IFDEF CHECK_TIME}
  TicksLoadGLScene: Cardinal;
{$ENDIF}
  CountIterator: TCountIterator;



  procedure AfterProcessing(var ALine: TsgGLLines; AIsPreProcessing: Boolean);
  begin
    if ALine.FPolylines.Count = 0 then
    begin
      FNav3D.GLFreeForm.Remove(ALine, False);
      ALine.Free;
      ALine := nil;
    end
    else
      if AIsPreProcessing then
        ALine.PreProcessing(FProgressObject, FAccuracyDim, FRoot);
  end;
begin
  if FNav3D = nil then Exit;
{$IFDEF CLIP_SGMESH}
  if FNav3D.IsClipPlaneUses then
  begin
    FNav3D.Buffer.Freeze;
//    FNav3D.FLockClip := True;
//    for I := 0 to FNav3D.FLastOriginalMesh do
//      TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).RestoreData;
    RestoreOutline;
//    FNav3D.GLFreeForm.StructureChanged;
  end;
{$ENDIF}
  LoadFromImage := ACADImage;
  FNav3D.Clear;
  ClearVisualization;
  FRoot.Clean;
  vGLForm := TGLBaseMeshAccess(FNav3D.GLFreeForm);
  if vGLForm = nil then Exit;
  FConverter := AConverter;
  FreeAndNil(FVectorFile);
  FVectorFile := {$IFDEF GLSCENE13}TGLVectorFile{$ELSE}TVectorFile{$ENDIF}.Create(vGLForm);
  try
    vGLForm.PrepareVectorFile(FVectorFile);
    if Assigned(vGLForm.Scene) then vGLForm.Scene.BeginUpdate;
    try
      FProgressObject.PercentDone := 0;
      FProgressObject.Step := 1/100;
      FProgressObject.Starting;
      try
      Navigator3D.FLineMass := TsgGLLines.CreateAsChild(FNav3D.GLFreeForm);
      Navigator3D.FLineOuterWire := TsgGLLines.CreateAsChild(FNav3D.GLFreeForm);
      Navigator3D.FTextMass := TsgGLLines.CreateAsChild(FNav3D.GLFreeForm);
      try
        Navigator3D.FLineMass.Material.MaterialLibrary := Navigator3D.FGLMaterialLibrary;
        Navigator3D.FLineOuterWire.Material.MaterialLibrary := Navigator3D.FGLMaterialLibrary;
        Navigator3D.FTextMass.Material.MaterialLibrary := Navigator3D.FGLMaterialLibrary;
        vBox := GetModelLayoutBox;
        PerformBroadcastEvents(WM_GETIMAGEBOX, WPARAM(@vBox), LPARAM(FLoadFromImage));
        FBoxOffs := MakeFPoint(-vBox.Left, -vBox.Bottom, -vBox.Z1);
        DoNodes;
{$IFDEF CHECK_TIME}
    TicksLoadGLScene := GetTickCount;
{$ENDIF}

{$IFDEF LOOP_STL}
        for K := 0 to ACADImage.CurrentLayout.PaperSpaceBlock.Count - 1 do
        begin
          if ACADImage.CurrentLayout.PaperSpaceBlock.Entities[K].EntType = ceInsert then
          begin
            vInsertStl := TsgDXFInsert(ACADImage.CurrentLayout.PaperSpaceBlock.Entities[K]);
            if Pos(cnstSgStlNamePref, vInsertStl.Block.Name) <> 0then
            begin
              vList3DFace := BuildLoops(vInsertStl.Block, vInsertStl.GetMatrix, cnstAccuracyParam);
              try
                AData.Creator := Self;
                for I := 0 to vList3DFace.Count - 1 do
                begin
                  vPoints := TFPointList(vList3DFace[I]);
                  if vPoints.Count < 2 then
                    Continue;
                  for J := 0 to vPoints.Count - 2 do
                  begin
                    AData.Point1 := TFPoint(vPoints[J]);
                    AData.Point2 := TFPoint(vPoints[J+1]);
                    AData.SegmentNumber := J;
                    AData.Normal := cnstXOrtAxis;
                    CallBackOuterLineDraw(@AData);
                  end;
                end;
              finally
                FreeList(vList3DFace);
              end;
              FAccuracyDim := cnstAccuracyDimSTL;
            end;
          end;
        end;
{$ENDIF}
        CountIterator := TCountIterator.Create;
        try
          CountIterator.Iterate(AConverter);
          FProgressObject.Step := 100 * cnstLoadDataBaseProgressPart/(CountIterator.Count + 1);
        finally
          CountIterator.Free;
        end;
        AutoInsert := False;
        inherited LoadFromConverter(AConverter, ACADImage);
        AutoInsert := True;
{$IFDEF CHECK_TIME}
    OutputDebugString(PChar(Format('Loading GLScene time is %s mlsek',
      [FloatToStr(GetTickCount - TicksLoadGLScene)])));
{$ENDIF}
      finally
        I := Navigator3D.FLineMass.FPolylines.Count + 1;
        Inc(I, Navigator3D.FLineOuterWire.FPolylines.Count);
        Inc(I, Navigator3D.FTextMass.FPolylines.Count);
        FProgressObject.Step := 100 * (1 - cnstLoadDataBaseProgressPart) / I;
        AfterProcessing(Navigator3D.FLineMass, True);
        AfterProcessing(Navigator3D.FLineOuterWire, True);
        if Assigned(Navigator3D.FLineOuterWire) then
          Navigator3D.FLineOuterWire.Visible := IsShowEdges;
        AfterProcessing(Navigator3D.FTextMass, False);
      end;
      finally
        FProgressObject.Ending;
      end;
    finally
      if Assigned(vGLForm.Scene) then vGLForm.Scene.EndUpdate;
    end;
    Load3dFaceMeshObject;

{$IFDEF DEBUG_STRUCTURE}
    UpdateRootToTreeView(nil, TCustomNode(FRoot));
{$ENDIF}



{$IFDEF CLIP_CSG_GLMESH_SGMESH}
    FNav3D.FLastOriginalMesh :=FNav3D.FGLFreeForm.MeshObjects.Count-1;
{$ENDIF}
    ResetDefaults;
    //vGLForm.PerformAutoScaling;
    //vGLForm.PerformAutoCentering;
    vGLForm.PrepareMesh;
    FNav3D.GLFreeForm.MeshObjects.Sort(MeshEntCompare);
    FNav3D.DoAfterLoadScene;
    FIsLoadedFromConv := True;
    AddMaterialsDimension;
    AppShowingStyle;
    LoadView(ACADImage.DrawMatrix);
    AddBoxDimensions(FNav3D.FBoxLine, nil, FShowBoxDimensions);
{$IFDEF CLIP_SGMESH}
    FNav3D.FLockClip := False;
    GetBoundingSphere(FNav3D.FGLFreeForm.MeshObjects, FNav3D.FSceneRadius, FNav3D.FSceneCenter);
    FNav3D.FSceneCenter := VectorTransform(FNav3D.FSceneCenter, FNav3D.FGLFreeForm.Matrix);
    if FNav3D.IsClipPlaneUses then
    begin
      SaveOutline;
      ApplyClip;
    end;
    FNav3D.Buffer.Melt;
{$ENDIF}
  finally
    FreeAndNil(FVectorFile);
  end;
end;

procedure Tsg3DDrawingNavigator.LoadFromFile(const FileName: string);
var
  I: Integer;
{$IFDEF LOOP_GL_SCENE}
  vList3DFace: TList;
  vPoints: TFPointList;
  J: Integer;
  AData: TsgTestLine;
  vMin, vMax: TAffineVector;


  procedure AfterProcessing(var ALine: TsgGLLines; AIsPreProcessing: Boolean);
  begin
    if ALine.FPolylines.Count = 0 then
    begin
      FNav3D.GLFreeForm.Remove(ALine, False);
      ALine.Free;
      ALine := nil;
    end
    else
      if AIsPreProcessing then
        ALine.PreProcessing(FProgressObject, FAccuracyDim, FRoot);
  end;
{$ENDIF}

begin
  if Assigned(FNav3D) then
  begin
    FNav3D.LoadFromFile(FileName);
{$IFDEF LOOP_GL_SCENE}
    Navigator3D.FLineOuterWire := TsgGLLines.CreateAsChild(FNav3D.GLFreeForm);
    try
      Navigator3D.FLineOuterWire.Material.MaterialLibrary := Navigator3D.FGLMaterialLibrary;
      vList3DFace := BuildLoopsByMeshes(FNav3D.FGLFreeForm.MeshObjects, cnstIdentityMat, cnstAccuracyParam);
      try
        AData.Creator := Self;
        for I := 0 to vList3DFace.Count - 1 do
        begin
          vPoints := TFPointList(vList3DFace[I]);
          if vPoints.Count < 2 then
            Continue;
          for J := 0 to vPoints.Count - 2 do
          begin
            AData.Point1 := TFPoint(vPoints[J]);
            AData.Point2 := TFPoint(vPoints[J+1]);
            AData.SegmentNumber := J;
            AData.Normal := cnstXOrtAxis;
            CallBackOuterLineDrawReal(@AData);
          end;
        end;
      finally
        FreeList(vList3DFace);
      end;
      FAccuracyDim := cnstAccuracyDimSTL;

    finally
      I := Navigator3D.FLineOuterWire.FPolylines.Count + 1;
      FProgressObject.Step := 100 * (1 - cnstLoadDataBaseProgressPart) / I;
      AfterProcessing(Navigator3D.FLineOuterWire, True);
      if Assigned(Navigator3D.FLineOuterWire) then
        Navigator3D.FLineOuterWire.Visible := IsShowEdges;

      FNav3D.FGLFreeForm.MeshObjects.GetExtents(vMin, vMax);
      FNav3D.FBoxImage.TopLeft := Vect2FPoint(vMin);
      FNav3D.FBoxImage.BottomRight := Vect2FPoint(vMax);
      SwapDoubles(FNav3D.FBoxImage.Top, FNav3D.FBoxImage.Bottom);
      AddBoxDimensions(FNav3D.FBoxLine, nil, FShowBoxDimensions, False);
    end;
{$ENDIF}
  end;
  FIsLoadedFromConv := False;
  AppShowingStyle;
end;

procedure Tsg3DDrawingNavigator.DoOnWrapUpRender(Sender: TObject; var rci: TRenderContextInfo);
var
  vGLCanvas: TGLCanvas;
  vSprite: TGLHUDSprite;

  procedure ApplyMat(AMaterial: TGLMaterial);
  begin
    AMaterial.Texture.ImageClassName := TGLBlankImage.ClassName;
    AMaterial.Texture.MagFilter := maNearest;
    AMaterial.Texture.MinFilter := miNearest;
    AMaterial.Texture.Compression := tcNone;
    AMaterial.BlendingMode := bmTransparency;
    AMaterial.Texture.ImageAlpha := tiaTopLeftPointColorTransparent;
    AMaterial.Texture.TextureMode := tmReplace;
    AMaterial.Texture.Disabled := False;
  end;

  procedure RenderSysMenuIcon(ASprite: TGLHUDSprite; ASysMenuIcon: TsgSysMenuIcon);
  var
    R: TRect;
    Icon: TBitmap;
  begin
    if SysMenuIcons[ASysMenuIcon] <> nil then
    begin
      Icon := TBitmap.Create;
      try
        Icon.PixelFormat := pf24bit;
        SetSizeGraphic(Icon, SysMenuIcons[ASysMenuIcon]);
        R := Rect(0, 0, Icon.Width, Icon.Height);
        Icon.Canvas.StretchDraw(R, SysMenuIcons[ASysMenuIcon]);
        ASprite.Material.Texture.Image.Assign(Icon);
      finally
        Icon.Free;
      end;
      ASprite.Width := ASprite.Material.Texture.Image.Width;
      ASprite.Height := ASprite.Material.Texture.Image.Height;
      R := SysMenuIcons.GetSysMenuIconRect(ASysMenuIcon);
      OffsetRect(R, (R.Right - R.Left) div 2, (R.Bottom - R.Top) div 2);
      ASprite.Position.AsAffineVector := AffineVectorMake(R.Left, R.Top, 0);
      ASprite.Render(rci);
    end;
  end;

var
  vNeedRenderSysMenuIcons: Boolean;
{$IFDEF CS_USEFORM}
  Form: TCustomForm;
{$ENDIF}
begin
  if Orbit3D.Visible and Navigator3D.IsRenderingContextAvailable then
    Tsg3DDrwNavOrbit3D(Orbit3D).PaintWindow(THandle(@rci));
  vNeedRenderSysMenuIcons := SysMenuIconsVisible and not FLockPaintSysMenuIcons;
{$IFDEF CS_USEFORM}
  if vNeedRenderSysMenuIcons then
  begin
    Form := GetParentForm(Self);
    if Assigned(Form) {$IFNDEF SG_MDI}and (TForm(Form).FormStyle = fsMDIChild) {$ENDIF}then
      if Form.WindowState <> wsMaximized then
        vNeedRenderSysMenuIcons := False;
  end;
{$ENDIF}
  if vNeedRenderSysMenuIcons then
  begin
    vGLCanvas := TGLCanvas.Create(Navigator3D.Buffer.Width, Navigator3D.Buffer.Height);
    try
      vSprite := TGLHUDSprite.Create(nil);
      try
        vGLCanvas.InvertYAxis;
        vSprite.AlphaChannel := 0;
        vSprite.Visible := True;
        ApplyMat(vSprite.Material);

        RenderSysMenuIcon(vSprite, sgTools.smiMinimize);
        RenderSysMenuIcon(vSprite, sgTools.smiRestore);
        RenderSysMenuIcon(vSprite, sgTools.smiClose);
      finally
        vSprite.Free;
      end;
    finally
      vGLCanvas.Free;
    end;
  end;
end;

function Tsg3DDrawingNavigator.FindDim(X, Y: Integer): TObject;
begin
  Result := FNav3D.GetDimension(X, Y);
end;

function Tsg3DDrawingNavigator.FindMesh(X, Y: Integer): TMeshObject;
var
  vCancel: Boolean;
begin
  vCancel := False;
  if FIntersectCache = nil then
    New(FIntersectCache);

  if not FNav3D.GetTypeIntersect(X, Y, TObject(Result), FIntersectCache, nil, vCancel, tiMesh) then
    DisposeAndNil(FIntersectCache);

//  if not FNav3D.GetMeshIntersect(X, Y, Result, FIntersectCache, nil, vCancel) then
//    DisposeAndNil(FIntersectCache);
end;

function Tsg3DDrawingNavigator.FindSnap(X, Y: Integer;
  var APoint, ANormal: TFPoint): TObject;
var
  vCancel: Boolean;
begin
  vCancel := False;

  Result := FNav3D.GetDimension(X, Y);
  if Assigned(Result) then
  begin
    Exit;
  end;

  FNav3D.GetTypeIntersect(X, Y, TObject(Result), @APoint, @ANormal, vCancel, tiSnap);
end;

function Tsg3DDrawingNavigator.FindSnapEdge(X, Y: Integer): TObject;
var
  vCancel: Boolean;
  vNormal: PFPoint;
begin
  vCancel := False;

  Result := FNav3D.GetDimension(X, Y);
  if Assigned(Result) then
  begin
    Exit;
  end;

  if FIntersectCache = nil then
    New(FIntersectCache);

  New(vNormal);

  Result := nil;
  if not FNav3D.GetTypeIntersect(X, Y, TObject(Result), FIntersectCache, vNormal, vCancel, tiSnapEdge) then
    DisposeAndNil(FIntersectCache);

  if Result is TsgGLPolyline then
  begin
    //TsgGLPolyline(Result).FNormal := vNormal^
  end
  else
    if not (Result is TsgPickObject) then
      Result := nil;

//  if Assigned(Result) then
//  begin
//    if not ((TObject(Result).ClassType = TsgGLPolyline) or (TObject(Result).ClassType = TsgPickObject)) then
//    begin
//      Result := nil;
//    end
//    else
//    begin
//      TsgGLPolyline(Result).FNormal := vNormal^;
//    end;
//  end;

  DisposeAndNil(vNormal);
end;

function Tsg3DDrawingNavigator.MeasureDimensionEsc: Boolean;
var
  I: Integer;
  vDimObj: TsgPickObject;
  vGLPoly: TsgGLPolyline;
begin
  Result := False; // Iterate
{$IFDEF USE_CLIP}
  if ShowSnapPlan then
  begin
    if FIndexSnapPlan = 2 then
    begin
      FIndexSnapPlan := 0;
      FSnapPlan[0].Obj := nil;
      FSnapPlan[1].Obj := nil;
      FSnapPlan[2].Obj := nil;
      Result := True;
    end;
  end;
{$ENDIF}
  if ShowSnap then
  begin
    if FIndexSnap = 1 then
    begin
      FIndexSnap := 0;
      FSnapObj[0].Obj := nil;
      FSnapObj[1].Obj := nil;
      Result := True;
    end;
  end;
  if Assigned(FMoveDim) then
  begin
    if Assigned(FOriginalObject) then
    begin
      vDimObj := TsgPickObject(FMoveDim.Obj);
      if Assigned(vDimObj) then
      begin
        vGLPoly := TsgGLPolyline(vDimObj.Obj);
        if Assigned(vGLPoly) then
          vGLPoly.Assign(FOriginalObject);
      end;
      vDimObj.DoPick;
    end;
    MoveDim := nil;
    FSnapDim.Obj := nil;
    FSnapEdge.Obj := nil;
    for I := Low(FSnapObj) to High(FSnapObj) do
      FSnapObj[I].Obj := nil;
{$IFDEF USE_CLIP}
    for I := Low(FSnapPlan) to High(FSnapPlan) do
      FSnapPlan[I].Obj := nil;
{$ENDIF}
    Result := True;
  end;
end;

function Tsg3DDrawingNavigator.MeasureDimensionDelete: Boolean;
var
  I, vIndex: Integer;
  vDeleteObj: TsgPickObject;
begin
  Result := False; // Iterate
  begin
    if Assigned(FMoveDim) then
    begin
      vDeleteObj := TsgPickObject(FMoveDim.Obj);
      vIndex := GetDimensionIndex(vDeleteObj);
      if vIndex >=0 then
      begin
        FNav3D.FDimensionList.Delete(vIndex);
        if Assigned(FNav3D.FOnMeasureDeleteEvent) then
          FNav3D.FOnMeasureDeleteEvent(vDeleteObj);
        vDeleteObj.Free;
        MoveDim := nil;
        FSnapDim.Obj := nil;
        FSnapEdge.Obj := nil;
        for I := Low(FSnapObj) to High(FSnapObj) do
          FSnapObj[I].Obj := nil;
{$IFDEF USE_CLIP}
        for I := Low(FSnapPlan) to High(FSnapPlan) do
          FSnapPlan[I].Obj := nil;
{$ENDIF}
        Result := True;
      end;
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.MeasureDimensionDeleteAll;
begin
  MeasureDimensionDelete;
  FNav3D.ClearDimension;
end;

function Tsg3DDrawingNavigator.MeasureDimensionDeleteByObject(
  const AObject: TsgPickObject): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  if Assigned(FMoveDim) then
  begin
    if FMoveDim.Obj = AObject then
    begin
      MeasureDimensionDelete;
      Exit;
    end;
  end;
  if Assigned(AObject) then
  begin
    vIndex := GetDimensionIndex(AObject);
    if vIndex >=0 then
    begin
      FNav3D.FDimensionList.Delete(vIndex);
      if Assigned(FNav3D.FOnMeasureDeleteEvent) then
        FNav3D.FOnMeasureDeleteEvent(AObject);
      AObject.Free;
      Result := True;
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.MouseDown(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{$IFDEF CLIP_SGMESH}
var
  vPos, vRayVect: TVector4f;
{$ENDIF}
begin
  FDown.Pos := Point(X, Y);
  FDown.Shift := Shift;
{$IFDEF CLIP_SGMESH}
  if Assigned(FNav3D.FGizmoCrossSection) then
  begin
    vPos := VectorMake(ClientToWorld(X, Y, 0), 1);
    vRayVect := FNav3D.Camera.AbsoluteVectorToTarget;
    NormalizeVector(vRayVect);
    if FNav3D.FGizmoCrossSection.MouseDown(Button, Shift, vPos, vRayVect) then
      Exit;
  end;
{$ENDIF}
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure Tsg3DDrawingNavigator.SetPickObject(APickObject: TObject);
begin
  FPickObjects.Obj := APickObject;
end;

{$IFDEF USE_CLIP}
procedure Tsg3DDrawingNavigator.SetPickPointClipEvent(
  const Value: TNotifyEvent);
begin
  FOnPickPointClipEvent := Value;
end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.SetProgressObject(const AValue: TProgress);
begin
  FProgressObject.Assign(AValue);
end;

procedure Tsg3DDrawingNavigator.SetHighLightObject(AHighLightObject: TObject);
var
  I: Integer;
begin
  I := -1;
  if Assigned(AHighLightObject) and (AHighLightObject.ClassType = TsgPickObject) then
    I := GetDimensionIndex(TsgPickObject(AHighLightObject));
  if I <> -1 then
  begin
    FSnapDim.Obj := GetDimension(I);
  end
  else
  begin
    FSnapDim.Obj := nil;
    FHighLightObj.Obj := AHighLightObject;
  end;
end;

procedure Tsg3DDrawingNavigator.SetSnapObject(ASnapObject: TObject);
begin
  FSnapObj[FIndexSnap].Obj := ASnapObject;
end;


procedure Tsg3DDrawingNavigator.SetLoadFromImage(const Value: TsgCADImage);
begin
  if FLoadFromImage <> Value then
  begin
    if Assigned(FLoadFromImage) then
    begin
      FLoadFromImage.OnChange := FLoadFromImageChange;
      FLoadFromImageChange := nil;
          end;
    FLoadFromImage := Value;
    if Assigned(FLoadFromImage) then
    begin
      FLoadFromImageChange := FLoadFromImage.OnChange;
      FLoadFromImage.OnChange := DoPictureChange;
    end;
    FDrawMode := dmNormal;
  end;
end;

procedure Tsg3DDrawingNavigator.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vObject: TObject;
  vSnapPolyline: TsgGLPolyline;
  vGLPolyline: TObject;
  vDimObj: TsgPickObject;
  vIntersectPoint, vIntersectNormal: TFPoint;
  vDeltaHeight: Double;
  vPoint1, vPoint2: TFPoint;
  vGLPoly: TsgGLPolyline;
  vIndexToCircle: Integer;
{$IFDEF USE_CLIP}
  I: Integer;
  vEdgePolyline: TsgGLPolyline;
{$ENDIF}
{$IFDEF CLIP_SGMESH}
  vPos, vRayVect: TVector4f;
{$ENDIF}
  vFindMesh: TMeshObject;
  vList: TsgObjectList;
  vSelectModTopoClass: TClassArray;

  function GetIndex(A3DList: TFPointList; A2DPoint: TFPoint): Integer;
  var
    I: Integer;
    vDist, vDistMin: Double;
    vPoint: TFPoint;
  begin
    Result := -1;
    vDistMin := MaxDouble;
    for I := 0 to A3DList.Count - 1 do
    begin
      vPoint := GetScreenCoords(A3DList[I]);
      vPoint.Y := FNav3D.Buffer.Height - vPoint.Y;
      vPoint.Z := 0;
      vDist := DistanceFPoint2D(vPoint, A2DPoint);
      if vDist < vDistMin then
      begin
        vDistMin := vDist;
        Result := I;
      end;
    end;
  end;

  function GetHeight(APoint1, APoint2: TFPoint; AMousePoint: TFPoint;
    IsDirection: Boolean = False): Double;
  var
    vPointCrossed: TFPoint;
    vAngle: Double;
  begin
    APoint1.Y := FNav3D.Buffer.Height - APoint1.Y;
    APoint2.Y := FNav3D.Buffer.Height - APoint2.Y;
    APoint1.Z := 0;
    APoint2.Z := 0;
    vPointCrossed := PointCrossedPerpendicularLines(APoint1, APoint2, AMousePoint);
    Result := DistanceFPoint(APoint1, vPointCrossed);
    if IsDirection then
    begin
      vAngle := GetAngleOfVectors(SubFPoint(APoint1, APoint2), SubFPoint(APoint1, vPointCrossed), False);
      if vAngle > 90 then
        Result := -Result;
    end;
  end;

begin
  if FBusy then
    Exit;
  if RectZoomingProcess then
  begin
    inherited;
    Exit;
  end;
  FBusy := True;
  try
    FMove.Shift := Shift;
    if AppActive and (FMouseRotate and (Shift = [ssLeft]) and not Orbit3D.Visible and Orbit3D.MouseCapture) then
    begin
      Tsg3DDrwNavOrbit3D(Orbit3D).MouseMove([ssLeft], X, Y);
      Cursor := TCursor(Ord(stateXY) + 1);
      Exit;
    end;
    if AppActive and (MoveByMouse and (FMouseRotate or Orbit3D.MouseCapture)) then
    begin
      inherited MouseMove(Shift, X, Y);
      Exit;
    end;
  {$IFDEF CLIP_SGMESH}
    if Assigned(FNav3D.FGizmoCrossSection)  and ((FMove.Pos.X <> X) or (FMove.Pos.Y <> Y)) then
    begin
      vPos := VectorMake(ClientToWorld(X, Y, 0), 1);
      vRayVect := FNav3D.Camera.AbsoluteVectorToTarget;
      NormalizeVector(vRayVect);
      if FNav3D.FGizmoCrossSection.MouseMove(Shift, vPos, vRayVect) then
      begin
        FMove.Pos := Point(X, Y);
        Exit;
      end;
    end;
  {$ENDIF}
    if ImageDragMode then
    begin
      inherited;
      Exit;
    end;
    if AppActive and not Orbit3D.Visible and not Orbit3D.MouseCapture then
    begin
{$IFDEF CLIP_SGMESH}
    if not Assigned(FNav3D.FGizmoCrossSection) or (FNav3D.FGizmoCrossSection.Operation = goNone) then
{$ENDIF}
      if FMouseRotate and (Shift = [ssLeft]) and not Orbit3D.MouseCapture then
      begin
        FMove.Pos := Point(X, Y);
        Tsg3DDrwNavOrbit3D(Orbit3D).MouseDown(TMouseButton(mbLeft), Shift, X, Y);
        Orbit3D.CurrentState := stateXY;
        Cursor := TCursor(Ord(Orbit3D.CurrentState) + 1);
        Exit;
      end;

      //Move Dim
      if Assigned(FMoveDim) and ((FMove.Pos.X <> X) or (FMove.Pos.Y <> Y)) then
      begin
        vDimObj := TsgPickObject(FMoveDim.Obj);
        if Assigned(vDimObj) then
        begin
          vGLPoly := TsgGLPolyline(vDimObj.Obj);
          if Assigned(vGLPoly) then
          begin
            if vGLPoly.Count = 2 then
            begin
              case vGLPoly.FType of
                0:
                  begin
                    vDeltaHeight := DistanceFPoint(vGLPoly.FBegin, vGLPoly.FEnd);
                    vPoint1 := GetScreenCoords(vGLPoly.FBegin);
                    vPoint2 := GetScreenCoords(AddFPoint(vGLPoly.FBegin, PtXScalar(vGLPoly.FNormal, vDeltaHeight)));
                    vGLPoly.FHeight := GetHeight(vPoint1, vPoint2, MakeFPoint(FMove.Pos.X, FMove.Pos.Y), True);
                    FMove.Pos := Point(X, Y);
                  end;
                1, 2, 3, 7:
                  begin
                    vGLPoly.FDimTextPos :=  AddFpoint(vGLPoly.FDimTextPos, MakeFPoint(X - FMove.Pos.X, FMove.Pos.Y - Y));
                    FMove.Pos := Point(X, Y);
                  end;
              else
                FMove.Pos := Point(X, Y);//no after changed
              end;
            end
            else
            begin
              vIndexToCircle := GetIndex(vGLPoly, MakeFPoint(FMove.Pos.X, FMove.Pos.Y));
              if vIndexToCircle >= 0 then
              begin
                vPoint1 := GetScreenCoords(vGLPoly.FCenter);
                vPoint2 := GetScreenCoords(vGLPoly[vIndexToCircle]);
                vGLPoly.FHeight := GetHeight(vPoint2, vPoint1, MakeFPoint(FMove.Pos.X, FMove.Pos.Y));
                vGLPoly.FIndexToCircle := vIndexToCircle;
                FMove.Pos := Point(X, Y);
              end;
              FMove.Pos := Point(X, Y);
            end;
          end;
          vDimObj.DoPick;
        end;
        inherited MouseMove(Shift, X, Y);
      end;

      if not MouseCapture then
      begin
        if (not ShowSnap) and (not ShowSnapEdge) and ((FMove.Pos.X <> X) or (FMove.Pos.Y <> Y)) then
        begin
          vGLPolyline := FindDim(X, Y);
          if Assigned(vGLPolyline) then
          begin
            FMove.Pos := Point(X, Y);
            FSnapDim.Obj := vGLPolyline;
            FSnapDim.Pos := FMove.Pos;
          end
          else
            FSnapDim.Obj := nil;
        end;


        if ShowHighlight and ([ssRight, ssMiddle] * Shift = []) and ((FMove.Pos.X <> X) or (FMove.Pos.Y <> Y)) then
        begin
          FMove.Pos := Point(X, Y);
          vFindMesh := FindMesh(X, Y);
          if Assigned(vFindMesh) and (vFindMesh is TsgMeshObject) then
          begin
            FHighLightObj.TopNode := TsgMeshObject(vFindMesh).Node;
            vList := TsgObjectList.Create;
            try
              if Assigned(TsgMeshObject(vFindMesh).Node) then
                vList.Add(TsgMeshObject(vFindMesh).Node);
              if IsSelectByBody(Shift) then
                vSelectModTopoClass := SetDynamicArray([TsgModTopoSolid,
                  (*TsgModTopoShell,*) TsgModPartCompound])//bodies
              else
                vSelectModTopoClass := SetDynamicArray([TsgModTopoFace]);//faces
              SetSelected2(vList,FHighLightObj, vSelectModTopoClass);
              if vList.Count > 0 then
                FHighLightObj.Tag := TObject(vList[0])
              else
                if vList.Count = 0 then
                  FHighLightObj.Tag := nil;

              if FHighLightObj.Objects.Count = 0 then
              begin
                HighLightObject := vFindMesh
              end
              else
              begin
                FHighLightObj.DoPick;
              end;

            finally
              vList.Free;
            end;
          end
          else
            HighLightObject := nil;
          FHighLightObj.Pos := FMove.Pos;
        end;
  {$IFDEF USE_CLIP}
        if ShowSnapPlan and ((FMove.Pos.X <> X) or (FMove.Pos.Y <> Y)) then
        begin
          FMove.Pos := Point(X, Y);
          vObject := FindSnap(X, Y, vIntersectPoint, vIntersectNormal);
          if Assigned(vObject) then
          begin
            if vObject.ClassType = TsgPickObject then
            begin
              FSnapPlan[FIndexSnapPlan].Obj := vObject;
            end
            else
            begin
              if  vObject.ClassType = TsgGLPolyline then
              begin // point on the curve
                vSnapPolyline := TsgGLPolyline(vObject);
                FSnapPlan[FIndexSnapPlan].Obj := vSnapPolyline;

                FIntersectPointPlanSufr[FIndexSnapPlan] := FSnapPlan[FIndexSnapPlan].FPoly.List[0];
                FIntersectNormalPlanSurf[FIndexSnapPlan] := cnstFPointZero;
              end
              else
              begin // point on the surface
                vSnapPolyline := CreateGLPolyline;
                vSnapPolyline.Add(vIntersectPoint);
                vSnapPolyline.Add(vIntersectPoint);
                vSnapPolyline.PointSize := Measure3DParams.MarkerSize;
                vSnapPolyline.MaterialNamePoint :=
                  AssignMaterial(Measure3DParams.MarkerHoverColor, FNav3D.FGLMaterialLibrary);
                vSnapPolyline.VisiblePoints := True;

                FSnapPlan[FIndexSnapPlan].Obj := vSnapPolyline;

                FIntersectPointPlanSufr[FIndexSnapPlan] := vIntersectPoint;
                FIntersectNormalPlanSurf[FIndexSnapPlan] := vIntersectNormal;
              end;
              if FSnapPlanEntity then
              begin
                vObject := FindSnapEdge(X, Y);
                if Assigned(vObject) and (vObject.ClassType = TsgGLPolyline) then
                begin//snap to edge
                  vEdgePolyline := TsgGLPolyline(vObject);
                  vEdgePolyline.PointSize := Measure3DParams.MarkerSize;
                  vEdgePolyline.MaterialNamePoint := AssignMaterial(Measure3DParams.MarkerHoverColor, FNav3D.FGLMaterialLibrary);
                  vEdgePolyline.VisiblePoints := True;
                  FSnapPlan[0].Obj := vEdgePolyline;
                  FSnapPlan[0].Pos := FMove.Pos;
                  CalcPlaneParams(vEdgePolyline, vEdgePolyline.FCenter, vEdgePolyline.FNormal);
                  if CalcCircleParams(vEdgePolyline, vEdgePolyline.FCenter, vEdgePolyline.FRadius, FAccuracyDim) then
                  begin
                    vSnapPolyline.List[0] := vEdgePolyline.FCenter;
                    vSnapPolyline.List[1] := vEdgePolyline.FCenter;
                  end;
                  FSnapPlan[FIndexSnapPlan].Obj := vSnapPolyline;
                end
                else
                begin//snap at surface
                 FSnapPlan[0].Obj := nil;
                end;
              end;
            end;
            FSnapPlan[FIndexSnapPlan].Pos := FMove.Pos;
          end
          else
          begin
            FSnapPlan[FIndexSnapPlan].Obj := nil;
            if FSnapPlanEntity then
              for I := Low(FSnapPlan) to High(FSnapPlan) do
                  FSnapPlan[I].Obj := nil;
          end;
        end;
  {$ENDIF}

        if ShowSnap and ((FMove.Pos.X <> X) or (FMove.Pos.Y <> Y)) then
        begin
          FMove.Pos := Point(X, Y);
          if FEdgeIntersect then
          begin
            vObject := FindSnapEdge(X, Y);
            if Assigned(vObject) then
            begin
              if vObject.ClassType = TsgPickObject then
              begin
                SnapObject := vObject;
              end
              else
              begin
                if  vObject.ClassType = TsgGLPolyline then
                begin
                  vSnapPolyline := TsgGLPolyline(vObject);
                  SnapObject := vSnapPolyline;
                  FIntersectPointSufr[FIndexSnap] := FSnapObj[FIndexSnap].FPoly.List[0];
                  FIntersectNormalSurf[FIndexSnap] := cnstFPointZero;
                end;
                FSnapObj[FIndexSnap].Pos := FMove.Pos;
              end;
            end
            else
            begin
              SnapObject := nil;
            end;
          end
          else
          begin
            vObject := FindSnap(X, Y, vIntersectPoint, vIntersectNormal);
            if Assigned(vObject) then
            begin
              if vObject.ClassType = TsgPickObject then
              begin
                SnapObject := vObject;
              end
              else
              begin
                if  vObject.ClassType = TsgGLPolyline then
                begin // point on the curve
                  vSnapPolyline := TsgGLPolyline(vObject);
                  SnapObject := vSnapPolyline;

                  FIntersectPointSufr[FIndexSnap] := FSnapObj[FIndexSnap].FPoly.List[0];
                  FIntersectNormalSurf[FIndexSnap] := cnstFPointZero;
                end
                else
                begin // point on the surface
                  vSnapPolyline := CreateGLPolyline;
                  vSnapPolyline.Add(vIntersectPoint);
                  vSnapPolyline.Add(vIntersectPoint);
                  vSnapPolyline.PointSize := Measure3DParams.MarkerSize;
                  vSnapPolyline.MaterialNamePoint :=
                    AssignMaterial(Measure3DParams.MarkerHoverColor, FNav3D.FGLMaterialLibrary);
                  vSnapPolyline.VisiblePoints := True;

                  SnapObject := vSnapPolyline;

                  FIntersectPointSufr[FIndexSnap] := vIntersectPoint;
                  FIntersectNormalSurf[FIndexSnap] := vIntersectNormal;
                end;
              end;
              FSnapObj[FIndexSnap].Pos := FMove.Pos;
            end
            else
            begin
              SnapObject := nil;
            end;
          end;
        end;
        if ShowSnapEdge and ((FMove.Pos.X <> X) or (FMove.Pos.Y <> Y)) then
        begin
            FMove.Pos := Point(X, Y);
            vGLPolyline := FindSnapEdge(X, Y);
            if Assigned(vGLPolyline) then
            begin
              FSnapEdge.Obj := vGLPolyline;
              FSnapEdge.Pos := FMove.Pos;
            end
            else
            begin
              FSnapEdge.Obj := nil;
            end;
        end;
        inherited MouseMove(Shift, X, Y);
      end;

    end
    else
      inherited MouseMove(Shift, X, Y);
  finally
    FBusy := False;
    if csDesignInteractive in ControlStyle then
      FNav3D.Buffer.NotifyMouseMove(Shift, X, Y);
  end;
end;

procedure Tsg3DDrawingNavigator.GetPolylineText(const AText: string;
  const APolyline: TFPointList);
var
  I, J, K: Integer;
  vCollection: TsgTextLinesCollection;
begin
  FTextByDrawing.Text := AText;
  FTextByDrawing.Scale := cnstScale;
  FDimImageByDrawing.Converter.Loads(FTextByDrawing.Style);
  TsgDXFTextAccess(FTextByDrawing).Loaded(FDimImageByDrawing.Converter);

  vCollection := TsgTextLinesCollection.Create(TFPointList.Create, True);
  try
    if FDimImageByDrawing.Converter.GetTextPolylines(FTextByDrawing, vCollection) > 0 then
    begin
      K := 0;
      for I := 0 to vCollection.Counts.Count - 1 do
      begin
        if vCollection.Counts[I] > 1 then
          for J := 1 to vCollection.Counts[I] - 1 do
          begin
            APolyline.Add(TFPointList(vCollection.Poly).List[K + J - 1]);
            APolyline.Add(TFPointList(vCollection.Poly).List[K + J]);
          end;
        Inc(K, vCollection.Counts[I]);
      end;
    end;
  finally
    vCollection.Free;
  end;
end;

function Tsg3DDrawingNavigator.GetDimensionTextPolyline(const APolyline: TsgGLPolyline): Boolean;
var
  vDis: TsgFloat;
  vCenter, vPoint: TFPoint;
  vRadius: Double;
begin
  Result := True;
  if APolyline.FType = 7 then
  begin
    vPoint := SubFPoint(APolyline.List[0], FBoxOffs);
      GetPolylineText(
        'X=' + DoubleToStringWithPrecision(vPoint.X * MeasureScaleFactor, MeasurePrecisionFactor) + ' ' +
        ' Y=' + DoubleToStringWithPrecision(vPoint.Y * MeasureScaleFactor, MeasurePrecisionFactor) + ' ' +
        ' Z=' + DoubleToStringWithPrecision(vPoint.Z * MeasureScaleFactor, MeasurePrecisionFactor) + ' ',
        APolyline.FTextList);
  end
  else if APolyline.Count <= 2 then
  begin
    vDis := DistanceFPoint(APolyline.List[0], APolyline.List[1]);
    GetPolylineText(DoubleToStringWithPrecision(vDis * MeasureScaleFactor, MeasurePrecisionFactor),
      APolyline.FTextList);
  end
  else
  begin
    if CalcCircleParams(APolyline, vCenter, vRadius, FAccuracyDim) then
    begin
      GetPolylineText('R ' + DoubleToStringWithPrecision(vRadius * MeasureScaleFactor, MeasurePrecisionFactor),
        APolyline.FTextList);
      APolyline.FCenter := vCenter;
      APolyline.FRadius := vRadius;
    end
    else
      Result := False;
  end;
end;

procedure Tsg3DDrawingNavigator.DoMeasureEvent(AMeasureObject: TObject);
begin
  if Assigned(FOnMeasureEvent) then
    FOnMeasureEvent(AMeasureObject);
end;

function Tsg3DDrawingNavigator.RayCastPlaneIntersect(const rayStart, rayVector : TFPoint;
                             const planePoint, planeNormal : TFPoint;
                             intersectPoint : PFPoint = nil) : Boolean;
var
   sp : TFPoint;
   t, d : TsgFloat;
begin
   d:=PointDotProduct(rayVector, planeNormal);
   Result:=((d>fDoubleResolution) or (d<-fDoubleResolution));
   if Result and Assigned(intersectPoint) then
   begin
      sp := SubFPoint(planePoint, rayStart);
      d:=1/d;
      t:=PointDotProduct(sp, planeNormal)*d;
      intersectPoint^ := AddFPoint(PtXScalar(rayStart, 1.0), PtXScalar(rayVector, t ));
   end;
end;

procedure Tsg3DDrawingNavigator.SnapObjMouseUp;
var
  vPolylineDim: TsgGLPolyline;
  vDimObj: TsgPickObject;
  vCenter: TFPoint;
  vRadius: Double;
  vCircle: array[0..1] of TFPoint;
  vLine: array[0..1] of TsgLine;
  vCountCircle, vCountLine: Integer;
  vPlane:array[0..1] of TFPoint;
  vPlaneNormal:array[0..1] of TFPoint;
  vPoint: array[0..1] of TFPoint;
  vMidPoints: array[0..1]  of TFPoint;
  vAngle: Double;
  vIsExData: Boolean;
  vCountPlane, vCountPoint: Integer;
  I, vType: Integer;



  function CalcOfDistanceTwoPlanes(const ABeginPoint, AEndPoint: TFPoint;
    const APlaneNormal: TFPoint; var AMidPoints: array of TFPoint;
    var APoint: array of TFPoint): Boolean;
  var
    vMidPoint: TFPoint;
    vMidPoints: array[0..1] of TFPoint;
    vDist: Double;
  begin
    Result := False;
    vMidPoint := MiddleFPoint(ABeginPoint, AEndPoint);
    if RayCastPlaneIntersect(vMidPoint, APlaneNormal,
          ABeginPoint, APlaneNormal, @vMidPoints[0]) then
    begin
      if RayCastPlaneIntersect(vMidPoints[0], APlaneNormal,
          AEndPoint, APlaneNormal, @vMidPoints[1]) then
      begin
        vDist := DistanceFPoint(vMidPoints[0], vMidPoints[1]);
        if not IsEqual(vDist, 0, cnstAccuracyDim) then
        begin
          AMidPoints[0] := vMidPoints[0];
          AMidPoints[1] := vMidPoints[1];
          APoint[0] := ABeginPoint;
          APoint[1] := AEndPoint;
          Result := True;
        end;
      end;
    end;
  end;

begin
  if FEdgeIntersect and Assigned(FSnapObj[FIndexSnap].Obj) and (FSnapObj[FIndexSnap].Obj.ClassType <> TsgPickObject)  then
  begin
    case FIndexSnap of
      0: // Add One Edge
      begin
        Inc(FIndexSnap);
      end;
      1: // Add Two Edge
      begin
        vCountCircle := 0;
        vCountLine := 0;
        vIsExData := False;
        vType := 0;

        for I := 0 to 1 do
        begin
          if CalcCircleParams(FSnapObj[I].FPoly, vCenter, vRadius, FAccuracyDim) then
          begin
            FSnapObj[I].FPoly.FCenter := vCenter;
            FSnapObj[I].FPoly.FRadius := vRadius;
            vCircle[vCountCircle] := FSnapObj[I].FPoly.FCenter;
            vPoint[vCountCircle] := FSnapObj[I].FPoly.List[0];
            Inc(vCountCircle);
          end
          else if FSnapObj[I].FPoly.Count = 2 then
          begin
            vLine[vCountLine].Point1 := FSnapObj[I].FPoly.List[0];
            vLine[vCountLine].Point2 := FSnapObj[I].FPoly.List[1];
            Inc(vCountLine);
          end;
        end;

        vMidPoints[0] := cnstFPointZero;
        vMidPoints[1] := cnstFPointZero;

        if vCountCircle = 2 then // Two Circle
        begin
          vMidPoints[0] := vCircle[0];
          vMidPoints[1] := vCircle[1];
          // vIsExData := True;
          vType := 1;
        end
        else if (vCountCircle = 1) and (vCountLine = 1) then
        begin
          vMidPoints[0] := vCircle[0];
          vMidPoints[1] := sgPointLineClosestPoint(vCircle[0], vLine[0].Point1, vLine[0].Point2);
          vType := 2;
          vIsExData := True;

          vPoint[0] := vMidPoints[0];

          if DistanceFPoint(vLine[0].Point1, vMidPoints[1]) > DistanceFPoint(vLine[0].Point2, vMidPoints[1]) then
            vPoint[1] := vLine[0].Point2
          else
            vPoint[1] := vLine[0].Point1;
        end else if (vCountLine = 2) then
        begin

          vAngle := DistanceFVector(sgFunction.Vector(SubFPoint(vLine[0].Point2, vLine[0].Point1),
            SubFPoint(vLine[1].Point2, vLine[1].Point1)));
          if IsZero(vAngle) then
          begin
            vMidPoints[0] := MiddleFPoint(vLine[0].Point1, vLine[0].Point2);
            vMidPoints[1] := sgPointLineClosestPoint(vMidPoints[0], vLine[1].Point1, vLine[1].Point2);
            vType := 1;
          end;
        end;

        FIndexSnap := 0;
        for I := Low(FSnapObj) to High(FSnapObj) do
          FSnapObj[I].Obj := nil;

        vPolylineDim := CreateGLPolyline;
        vPolylineDim.FType := vType;
        vPolylineDim.LineWidth := Measure3DParams.DimLwd;

        // Add Edge
        vPolylineDim.Add(vMidPoints[0]);
        vPolylineDim.Add(vMidPoints[1]);

        // Add Ext data
        if vIsExData then
        begin
          vPolylineDim.FExData.Add(vPoint[0]);
          vPolylineDim.FExData.Add(vMidPoints[0]);
          vPolylineDim.FExData.Add(vMidPoints[1]);
          vPolylineDim.FExData.Add(vPoint[1]);
        end;

        vPolylineDim.FBegin := vPolylineDim.List[0];
        vPolylineDim.FEnd := vPolylineDim.List[1];

        if GetDimensionTextPolyline(vPolylineDim) then
        begin
           vDimObj := CreateDimension;
           vDimObj.Pos := FSnapObj[FIndexSnap].Pos;
           vDimObj.LineWidth := Measure3DParams.DimLwd;

          vPolylineDim.Mode := GL_LINES;

          vPolylineDim.PointSize := Measure3DParams.PointSize;
          vPolylineDim.MaterialName :=
             AssignMaterial(clRed, FNav3D.FGLMaterialLibrary);
//              vPolylineDim.MaterialNamePoint :=
//                 AssignMaterial(clRed, FNav3D.FGLMaterialLibrary);
          vPolylineDim.MaterialNamePoint :=
            AssignMaterial(Measure3DParams.SelectNodeColor, FNav3D.FGLMaterialLibrary);

          vPolylineDim.MaterialNameText :=
             AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRT), FNav3D.FGLMaterialLibrary);
          vPolylineDim.MaterialNameLine :=
             AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRD), FNav3D.FGLMaterialLibrary);

          vPolylineDim.ArrowSize := FDimByDrawing.ArrowSize;

          vDimObj.Obj := vPolylineDim;

          vDimObj.Pos := FMove.Pos;


          DoMeasureEvent(vDimObj);
        end
        else
          DeleteGLPolyline(vPolylineDim);
        //
      end;
    end;
  end;
  if (not FEdgeIntersect) and Assigned(FSnapObj[FIndexSnap].Obj) and (FSnapObj[FIndexSnap].Obj.ClassType <> TsgPickObject)  then
  begin
    vType := 0;
    if FMeasureMode3D = mmSnapPoint then
    begin
      FIndexSnap := 1;
      vType := 7;
    end;
    case FIndexSnap of
      0: // Add One Point
      begin
        Inc(FIndexSnap);
      end;
      1: // Add Two Point
      begin
        FIndexSnap := 0;
        for I := Low(FSnapObj) to High(FSnapObj) do
          FSnapObj[I].Obj := nil;

        if vType = 7 then
        begin
          vMidPoints[0] := FIntersectPointSufr[0];
          vMidPoints[1] := FIntersectPointSufr[0];
          vIsExData := False;
        end
        else
        begin
          vCountPoint := 0;
          vCountPlane := 0;
          vIsExData := False;
          //Calculation dimension
          for I := Low(FIntersectNormalSurf) to High(FIntersectNormalSurf) do
          begin
            if not IsEqualFPoints(FIntersectNormalSurf[I],cnstFPointZero) then
            begin
              vPlane[vCountPlane] := FIntersectPointSufr[I];
              vPlaneNormal[vCountPlane] := FIntersectNormalSurf[I];
              Inc(vCountPlane);
            end
            else
            begin
              vPoint[vCountPoint] := FIntersectPointSufr[I];
              Inc(vCountPoint);
            end;
          end;

          if vCountPlane = 2 then
          begin // Two surface
            vType := 2;
            vMidPoints[0] := vPlane[0];
            vMidPoints[1] := vPlane[1];

            vAngle := GetAngleOfVectors(vPlaneNormal[0], vPlaneNormal[1], False, True);
            vAngle := Abs(Cos(vAngle * fPiDividedBy180));

            if IsEqual(vAngle, 1) then
            begin
              if CalcOfDistanceTwoPlanes(vPlane[0], vPlane[1], vPlaneNormal[0],
                   vMidPoints, vPoint) then
                vIsExData := True;
            end;
          end
          else if vCountPlane = 1 then
          begin // One surface and Point
            vType := 3;
            vMidPoints[0] := vPoint[0];
            vMidPoints[1] := vPlane[0];

            if CalcOfDistanceTwoPlanes( vPoint[0], vPlane[0], vPlaneNormal[0],
                 vMidPoints, vPoint) then
              vIsExData := True;
          end
          else
          begin
            vMidPoints[0] := vPoint[0];
            vMidPoints[1] :=  vPoint[1];
            vType := 1;
          end;
        end;

        vPolylineDim := CreateGLPolyline;
        vPolylineDim.FType := vType;
        vPolylineDim.LineWidth := Measure3DParams.DimLwd;

        // Add Edge
        vPolylineDim.Add(vMidPoints[0]);
        vPolylineDim.Add(vMidPoints[1]);

        // Add Ext data
        if vIsExData then
        begin
          vPolylineDim.FExData.Add(vPoint[0]);
          vPolylineDim.FExData.Add(vMidPoints[0]);
          vPolylineDim.FExData.Add(vMidPoints[1]);
          vPolylineDim.FExData.Add(vPoint[1]);
        end;

        vPolylineDim.FBegin := vPolylineDim.List[0];
        vPolylineDim.FEnd := vPolylineDim.List[1];

        if GetDimensionTextPolyline(vPolylineDim) then
        begin
           vDimObj := CreateDimension;
           vDimObj.Pos := FSnapObj[FIndexSnap].Pos;
           vDimObj.LineWidth := Measure3DParams.DimLwd;

          vPolylineDim.Mode := GL_LINES;

          vPolylineDim.PointSize := Measure3DParams.PointSize;
          vPolylineDim.MaterialName :=
             AssignMaterial(clRed, FNav3D.FGLMaterialLibrary);
//              vPolylineDim.MaterialNamePoint :=
//                 AssignMaterial(clRed, FNav3D.FGLMaterialLibrary);
          vPolylineDim.MaterialNamePoint :=
            AssignMaterial(Measure3DParams.SelectNodeColor, FNav3D.FGLMaterialLibrary);

          vPolylineDim.MaterialNameText :=
             AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRT), FNav3D.FGLMaterialLibrary);
          vPolylineDim.MaterialNameLine :=
             AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRD), FNav3D.FGLMaterialLibrary);

          vPolylineDim.ArrowSize := FDimByDrawing.ArrowSize;

          vDimObj.Obj := vPolylineDim;

          vDimObj.Pos := FMove.Pos;


          DoMeasureEvent(vDimObj);
        end
        else
          DeleteGLPolyline(vPolylineDim);
        //
      end;
    end;
  end;
  if Assigned(FSnapObj[FIndexSnap].Obj) and (FSnapObj[FIndexSnap].Obj.ClassType = TsgPickObject) then
  begin
    if Assigned(FMoveDim) then
    begin
      MoveDim := nil;
      FIndexSnap := 0;
      FSnapObj[0].Obj := nil;
      FSnapObj[1].Obj := nil;
    end
    else
    begin
      if FIndexSnap <> 0 then
      begin
        FSnapObj[0].Obj := FSnapObj[1].Obj;
        FSnapObj[0].Pos := FSnapObj[1].Pos;
      end;
      FIndexSnap := 0;
      FSnapObj[1].Obj := nil;
      MoveDim := FSnapObj[FIndexSnap];
    end;
  end;

end;

procedure Tsg3DDrawingNavigator.SnapEdgeMouseUp;
var
  vPolylineDim: TsgGLPolyline;
  vDimObj: TsgPickObject;
  vType: Integer;
begin
  if Assigned(FSnapEdge.Obj) and (FSnapEdge.Obj.ClassType = TsgGLPolyline)  then
  begin
    vType := 0;
    vPolylineDim := CreateGLPolyline;
    vPolylineDim.FType := vType;//default
    vPolylineDim.LineWidth := Measure3DParams.DimLwd;

    // Add Edge
    vPolylineDim.Assign(FSnapEdge.FPoly);
    vPolylineDim.FHeight := Measure3DParams.DimLine;

    vPolylineDim.FBegin := vPolylineDim.List[0];
    vPolylineDim.FEnd := vPolylineDim.List[1];

    if GetDimensionTextPolyline(vPolylineDim) then
    begin
       vDimObj := CreateDimension;
       vDimObj.Pos := FSnapEdge.Pos;
       vDimObj.LineWidth := Measure3DParams.DimLwd;

      vPolylineDim.Mode := GL_LINES;

      vPolylineDim.PointSize := Measure3DParams.PointSize;
      vPolylineDim.MaterialName :=
         AssignMaterial(clRed, FNav3D.FGLMaterialLibrary);
//          vPolylineDim.MaterialNamePoint :=
//             AssignMaterial(clRed, FNav3D.FGLMaterialLibrary);
          vPolylineDim.MaterialNamePoint :=
            AssignMaterial(Measure3DParams.SelectNodeColor, FNav3D.FGLMaterialLibrary);

      vPolylineDim.MaterialNameText :=
         AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRT), FNav3D.FGLMaterialLibrary);
      vPolylineDim.MaterialNameLine :=
         AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRD), FNav3D.FGLMaterialLibrary);

      vPolylineDim.ArrowSize := FDimByDrawing.ArrowSize;

      vDimObj.Obj := vPolylineDim;

      vDimObj.Pos := FMove.Pos;

      DoMeasureEvent(vDimObj);
    end
    else
      DeleteGLPolyline(vPolylineDim);
    //
    FSnapEdge.Obj := nil;
  end;
  if Assigned(FSnapEdge.Obj) and (FSnapEdge.Obj.ClassType = TsgPickObject) then
  begin
    SetMoveDim(FSnapEdge);
  end;
end;

procedure Tsg3DDrawingNavigator.MouseUp(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vSamePos: Boolean;
  vList: TsgObjectList;
  I: Integer;
  vNotSkipProcessing: Boolean;
{$IFDEF CLIP_SGMESH}
  vPos, vRayVect: TVector4f;
{$ENDIF}
  vFindMesh: TMeshObject;
  vEntityProxy: TsgObjEntity3D;
  vEnabled: Boolean;
  vNode: TCustomNode;
  vSelectModTopoClass: TClassArray;

  procedure DoSelect(AList: TsgObjectList);
  begin
    SetSelected(AList);
    if AList.Count > 0 then
      FPickObjects.Tag := TObject(AList[0])
    else
      if AList.Count = 0 then
        FPickObjects.Tag := nil;
    FPickObjects.DoPick;
  end;

  procedure DoSetPick(APick: TsgMeshObject);
  var
    vEnabled: Boolean;
  begin
    vEnabled := FPickObjects.Enabled;
    try
      FPickObjects.Enabled := False;
      PickObject := APick;
      FPickObjects.Pos := Point(X, Y);
      if (HighLightObject <> nil) and (APick <> nil) then
        FPickObjects.TopNode := TsgMeshObject(APick).Node
      else
        FPickObjects.TopNode := nil;
    finally
      FPickObjects.Enabled := vEnabled;
    end;
    FPickObjects.DoPick;
  end;

  procedure SetMoveDim(ABaseObject: TsgPickObject);
  begin
    if Assigned(FMoveDim) then
    begin
      MoveDim := nil;
      ABaseObject.Obj := nil;
    end
    else
    begin
      MoveDim := ABaseObject;
    end;
  end;

begin
  if RectZoomingProcess then
  begin
    inherited;
    Exit;
  end;
  if AppActive and (FMouseRotate and not Orbit3D.Visible and Orbit3D.MouseCapture) then
  begin
    Tsg3DDrwNavOrbit3D(Orbit3D).MouseUp(TMouseButton(mbLeft), [ssLeft], X, Y);
    Cursor := TCursor(cnstCursorCross);
    if (FDown.Pos.X <> X) or (FDown.Pos.Y <> Y) then
      Exit;
  end;
  FUp.Pos := Point(X, Y);
  FUp.Shift := Shift;
{$IFDEF CLIP_SGMESH}
  if Assigned(FNav3D.FGizmoCrossSection) then
  begin
    vPos := VectorMake(ClientToWorld(X, Y, 0), 1);
    vRayVect := FNav3D.Camera.AbsoluteVectorToTarget;
    NormalizeVector(vRayVect);
    if FNav3D.FGizmoCrossSection.MouseUp(Button, Shift, vPos, vRayVect) then
      Exit;
  end;
{$ENDIF}
  if not Orbit3D.Visible and not Orbit3D.MouseCapture and (Button = TMouseButton(mbLeft)) then
  begin
    if FMouseRotate then
      Cursor := TCursor(cnstCursorCross);
    vNotSkipProcessing := True;
    if Assigned(FSnapDim.Obj) and (FSnapDim.Obj.ClassType = TsgPickObject) then
    begin
      vNotSkipProcessing := False;
      SetMoveDim(FSnapDim);
    end;

    if (FPickObjects.Enabled) and (not ImageDragMode) and vNotSkipProcessing then
    begin
      vSamePos := (FHighLightObj.Pos.X = X) and (FHighLightObj.Pos.Y = Y);
      if (PickObject <> HighLightObject) and (HighLightObject <> nil) and vSamePos then
      begin
        DoSetPick(TsgMeshObject(HighLightObject));
      end
      else
        if (FPickObjects.Pos.X <> X) or (FPickObjects.Pos.Y <> Y) then
        begin
          vFindMesh := FindMesh(X, Y);
          if Assigned(vFindMesh) and (vFindMesh is TsgMeshObject) then
          begin
            FPickObjects.TopNode := TsgMeshObject(vFindMesh).Node;
            vList := TsgObjectList.Create;
            try
              vEnabled := FPickObjects.Enabled;
              try
                FPickObjects.Enabled := False;
                if Assigned(TsgMeshObject(vFindMesh).Node) then
                  vList.Add(TsgMeshObject(vFindMesh).Node);
                if IsSelectByBody(Shift) then
                  vSelectModTopoClass := SetDynamicArray([TsgModTopoSolid,
                    (*TsgModTopoShell,*) TsgModPartCompound])
                else
                  vSelectModTopoClass := SetDynamicArray([TsgModTopoFace]);
                SetSelected2(vList,FPickObjects, vSelectModTopoClass);

                if (vList.Count > 0) and (FPickObjects.Objects.Count > 0) then
                begin
                  if Assigned(FConverter) then
                  begin
                    PickObject := TsgMeshObject(FPickObjects.Objects[0]);
                    FPickObjects.Pos := Point(X, Y);
                    vNode := TCustomNode.GetNodeOwnerByTopoClass(
                      TCustomNode(FRoot),
                      TsgMeshObject(vFindMesh).Node, vSelectModTopoClass);
                    FPickObjects.TopNode := vNode;
                    vEntityProxy := FConverter.GetEntityProxy(vNode.Obj,
                       TCustomNode.GetPathKeyByNode(vNode));
                    FPickObjects.Tag := TObject(vEntityProxy);

                    UpdateBoxDimensions(vNode);

                  end
                  else
                    FPickObjects.Tag := TObject(vList[0])
                end
                else
                  if vList.Count = 0 then
                    FPickObjects.Tag := nil;
              finally
                FPickObjects.Enabled := vEnabled;
              end;

              if FPickObjects.Objects.Count = 0 then
              begin
                DoSetPick(TsgMeshObject(vFindMesh));
              end
              else
              begin
                FPickObjects.DoPick;
              end;
            finally
              vList.Free;
            end
          end
          else
          begin
            UpdateBoxDimensions(nil);
            DoSetPick(nil);
          end;



          //DoSetPick(TsgMeshObject(FindMesh(X, Y)));
        end;
//        else
//          if Assigned(FPickObjects.TopNode) then
//          begin
//            vList := TsgObjectList.Create;
//            try
//              if FPickObjects.TopNode.Owner <> FRoot then
//              begin
//                FPickObjects.TopNode := FPickObjects.TopNode.Owner;
//                vList.Add(FPickObjects.TopNode.Obj);
//                DoSelect(vList);
//              end
//              else
//              begin
//                if FPickObjects.TopNode.ClassType = TNodeGroup then
//                begin
//                  vList.Add(TNodeGroup(FPickObjects.TopNode).Group);
//                  DoSelect(vList);
//                end;
//              end;
//            finally
//              vList.Free;
//            end;
//          end;
    end;

{$IFDEF USE_CLIP}
    if FSnapPlan[FIndexSnapPlan].Enabled and vNotSkipProcessing then
    begin
      if Assigned(FSnapPlan[FIndexSnapPlan].Obj) and (FSnapPlan[FIndexSnapPlan].Obj.ClassType <> TsgPickObject)  then
      begin
        if FIndexSnapPlan + 1 < FSnapPlanCount then
            Inc(FIndexSnapPlan) // Add One and Two Point
          else
          begin
            {vCutPlane :=  TGLPlane.CreateAsChild(FNav3d.GLDCScene);
            vCutPlane.Position.AsAffineVector := FPoint2Vect(FIntersectPointPlanSufr[0]);
            vCutPlane.Direction.AsAffineVector := FPoint2Vect(sgCalcPlaneNormal(FIntersectPointPlanSufr[0],
              FIntersectPointPlanSufr[1],
              FIntersectPointPlanSufr[2]));
            vCutPlane.Height := Max((FNav3D.FBoxImage.Top - FNav3D.FBoxImage.Bottom),
              Max(FNav3D.FBoxImage.Z2 - FNav3D.FBoxImage.Z1, FNav3D.FBoxImage.Right - FNav3D.FBoxImage.Left));
            vCutPlane.Width := vCutPlane.Height;}
            if Assigned(FOnPickPointClipEvent) then
              FOnPickPointClipEvent(Self);

            FIndexSnapPlan := 0;
            if FSnapPlanEntity then
              FIndexSnapPlan := FSnapPlanCount - 1;
            for I := Low(FSnapPlan) to High(FSnapPlan) do
              FSnapPlan[I].Obj := nil;
            //

          end;
      end;
      if Assigned(FSnapPlan[FIndexSnapPlan].Obj) and (FSnapPlan[FIndexSnapPlan].Obj.ClassType = TsgPickObject) then
      begin
        //Move
      end;
    end;
{$ENDIF}

    if FSnapObj[FIndexSnap].Enabled and vNotSkipProcessing then
    begin
      SnapObjMouseUp;
    end;
    if FSnapEdge.Enabled and vNotSkipProcessing then
    begin
      SnapEdgeMouseUp;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure Tsg3DDrawingNavigator.OrbitChange(Sender: TObject);
begin
  if not IsImgLocked then
    inherited OrbitChange(Sender);
end;

procedure Tsg3DDrawingNavigator.OrbitVisibleChanging(Sender: TObject);
begin
  inherited OrbitVisibleChanging(Sender);
  HighLightObject := nil;
  Invalidate;
end;

function Tsg3DDrawingNavigator.MovePictureRect2D(const ADeltaX,
  ADeltaY: Double): Boolean;
var
  vMousePointX, vMousePointY: Extended;
  vOldPos, vNewPos, vTrans: TAffineVector;
  vKoef: Single;
begin
  Result := False;
  vMousePointX := ADeltaX + MouseMovePrevPoint.X;
  vMousePointY := ADeltaY + MouseMovePrevPoint.Y;
  vOldPos := ClientToWorld(MouseMovePrevPoint.X, MouseMovePrevPoint.Y, 0);
  vNewPos := ClientToWorld(vMousePointX, vMousePointY, 0);
  if VectorNorm(vOldPos) <> 0 then
  begin
    VectorSubtract(vNewPos, vOldPos, vTrans);
    if FNav3D.CameraStyle <> csOrthogonal then
    begin
      { TODO: calc vKoef for csPerspective translate }
      //vKoef := FNav3D.BoundingSphereRadius * FNav3D.Camera.SceneScale;
      vKoef := 3 * FNav3D.Camera.DepthOfView;
      VectorScale(vTrans, vKoef, vTrans);
    end;
    Translate(vTrans);
    Result := True;
  end;
end;

procedure Tsg3DDrawingNavigator.Navigator3DAfterRender(Sender: TObject);
var
  I: Integer;

  procedure RestoreHighlightMatName(AMesh: TMeshObject;
    const AHighlightSaveMatName: string);
  var
    I: Integer;
    vMatLibItem: TGLLibMaterial;
    vfgps: {$IFDEF GLSCENE13}TGLFaceGroups{$ELSE}TFaceGroups{$ENDIF};
//    vMesh: TMeshObject;
  begin
    case FDrawMode of
      dmNormal, dmGray:
        begin
          SetMeshMaterial(AMesh, AHighlightSaveMatName, FNav3D.FGLMaterialLibrary);
        end;
    else
      vMatLibItem := AssignMaterial(clGray, FNav3D.FGLMaterialLibrary.Materials);
      vfgps := AMesh.FaceGroups;
      for I := 0 to vfgps.Count - 1 do
      begin
        vfgps[I].MaterialName := vMatLibItem.Name;
        vfgps[I].PrepareMaterialLibraryCache(FNav3D.FGLMaterialLibrary);
        vfgps[I].MaterialName := AHighlightSaveMatName;
      end;
    end;
  end;

begin
  if (HighLightObject is TMeshObject) and (FHighlightSaveMatName <> '') then
  begin
    RestoreHighlightMatName(TMeshObject(HighLightObject), FHighlightSaveMatName);
    FHighlightSaveMatName := '';
  end
  else if (FHighLightObj.Objects.Count > 0) and (FHighlightSaveMatNames.Count > 0) then
  begin
    for I := 0 to FHighLightObj.Objects.Count - 1 do
    begin
      if FHighlightSaveMatNames[I] <> '' then
        RestoreHighlightMatName(TMeshObject(FHighLightObj.Objects[I]),
         FHighlightSaveMatNames[I]);
    end;
    FHighlightSaveMatNames.Clear;
  end;
end;

procedure Tsg3DDrawingNavigator.Navigator3DBeforeRender(Sender: TObject);
var
  I: Integer;
  procedure SaveHighlightMat(AMesh: TMeshObject;
    const AIndex: Integer = -1);
  var
//    vMesh: TMeshObject;
    vHighlightItem: TGLLibMaterial;
    vMatLib: TGLMaterialLibrary;
  begin
    if not  Assigned(AMesh) then
      Exit;
    if AMesh.FaceGroups.Count > 0 then
    begin
      vMatLib := TGLMaterialLibrary(FHighLightObj.Lines.Material.MaterialLibrary);
      vHighlightItem := AssignMaterial(FHighLightObj.Color, vMatLib.Materials);
      if AIndex = -1 then
        FHighlightSaveMatName := AMesh.FaceGroups[0].MaterialName
      else
        FHighlightSaveMatNames.Add(AMesh.FaceGroups[0].MaterialName);
      SetMeshMaterial(AMesh, vHighlightItem.Name, vMatLib);
    end
    else
    begin
      if AIndex = -1 then
        FHighlightSaveMatName := ''
      else
        FHighlightSaveMatNames.Add('');
    end;
  end;

begin
  if HighLightObject is TMeshObject then
  begin
    SaveHighlightMat(TMeshObject(HighLightObject));
  end
  else if FHighLightObj.Objects.Count > 0 then
  begin
    FHighlightSaveMatNames.Clear;
    for I := 0 to FHighLightObj.Objects.Count - 1 do
    begin
      SaveHighlightMat(TMeshObject(FHighLightObj.Objects[I]), I);
    end;
  end;
end;

function Tsg3DDrawingNavigator.OpenVPort: Boolean;
begin
  Result := False;
end;

procedure Tsg3DDrawingNavigator.PickObjectEvent(Sender: TObject);
var
{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
  I: Integer;
{$ENDIF}
  vAABB: TAABB;
{$IFDEF SG_ROTATE_CENTER_TEST}
  vBox: TFRect;
{$ENDIF}
begin
  FPickObjects.BuildLines;
  Navigator3DAfterRender(nil);
  RepaintScene;
  if Assigned(FOnPickObject) then
    FOnPickObject(Self);
{$IFDEF SG_INTERNAL_TEST_AB3DKERNEL}
  InitPolygons;
  InitStructure;
  InitFileStructure;
  if PickObjects.Obj is TsgMeshObject then
  begin
    FPolygons.SelectionList.Clear;
    FStructureFile.FileList.Clear;
    for I := 0 to GlobalBrepParser.Count - 1 do
    begin
      FStructureFile.FileList.AddObject(LoadFromImage.FileName+IntToStr(I),
      GlobalBrepParser[I]);
    end;
    FStructureFile.SourceChangeExternal(Sender);

    for I := 0 to PickObjects.Objects.Count - 1 do
    begin
      if TObject(PickObjects.Objects[I]).ClassType = TsgMeshObject then
      begin
        FPolygons.SelectionList.Add(TsgMeshObject(PickObjects.Objects[I]).Entity);
        FStructureFile.SelectionList.Add(TsgMeshObject(PickObjects.Objects[I]).Entity);
      end;
    end;
    FPolygons.Show;
    FPolygons.SelectionChangeExternal(Sender);
    FStructure.Show;
    FStructure.SelectionChangeExternal(Sender);
    FStructureFile.Show;
    FStructureFile.SelectionChangeExternal(Sender);
  end;
{$ENDIF}
  if PickObjects.Obj is TsgMeshObject then
  begin
{$IFDEF SG_ROTATE_CENTER_TEST}
    TsgMeshObject(PickObjects.Obj).GetExtents(vAABB);
    if Assigned(PickObjects.TopNode) then
    begin
      vBox := TCustomNode(PickObjects.TopNode).GetCurrentBox;
      if not IsBadRect(vBox) then
        vAABB := BoxToAABB(vBox);
    end;
{$ELSE}
   TsgMeshObject(PickObjects.Obj).GetExtents(vAABB)
{$ENDIF}
  end
  else
  begin
{$IFDEF SG_ROTATE_CENTER_TEST}
    vBox := TCustomNode(FRoot).GetCurrentBox;
    if IsBadRect(vBox) then
      vAABB := FNav3D.FAABBBox
    else
    begin
      vAABB := BoxToAABB(vBox);
    end;
{$ELSE}
    vAABB := FNav3D.FAABBBox;
{$ENDIF}
  end;
  FNav3D.FCenter := VectorScale(VectorAdd(vAABB.Min, vAABB.Max), 0.5);
end;

procedure Tsg3DDrawingNavigator.PrepareData(ALine: TsgGLPolyline;
  Points: TFPointList; SolidPolyline: Boolean; ClosedPolyline: Boolean);
var
  I: Integer;
begin
  if not CheckCount(ALine, Points.Count) then
    Exit;
  for I := 0 to Points.Count - 1 do
    ALine.Add(GetPoint(Points[I]));
  if SolidPolyline then
  begin
    if ClosedPolyline then
      ALine.Mode := GL_LINE_LOOP;
  end
  else
    ALine.Mode := GL_LINES;
end;

function Tsg3DDrawingNavigator.GetFaceGroup(AMeshObject: TMeshObject;
  AMode: {$IFDEF GLSCENE13}TGLFaceGroupMeshMode{$ELSE}TFaceGroupMeshMode{$ENDIF}; AMaterial: TGLLibMaterial;
  AFaceGroupClass: TClass): TFGVertexIndexList;
begin
  Result := TFaceGroupClass(AFaceGroupClass).CreateOwned(AMeshObject.FaceGroups) as TFGVertexIndexList;
  Result.Mode := AMode;
  Result.MaterialName := AMaterial.Name;
end;

function Tsg3DDrawingNavigator.GetFilteredEntity(Entity: TsgDXFEntity): Integer;
var
  vInsert: TsgDXFInsert;
begin
  Result := 0;
  if Entity.IsInsert then
  begin
    Result := GetEntity(Entity)
  end
  else
  begin
    vInsert := IterateParam.Insert;
    if vInsert = FInsert then
      Result := GetEntity(Entity)
  end;

//  while Assigned(vInsert) and (vInsert <> FInsert) do
//    vInsert := vInsert.OwnerInsert;
//  if vInsert = FInsert then
//    Result := GetEntity(Entity)
end;

procedure Tsg3DDrawingNavigator.Reload(AEntities: TObject);
var
  I, J, C, vCountLineOuterWire: Integer;
  vMeshes: TsgObjectList;
  vMesh: TsgMeshObject;
  vParams, vTmpParams: PsgCADIterate;
//  vCustomFindEnt: TCustomFindEnt;
  vInsert: TsgDXFInsertAccess;
//  vAutoInsert: Boolean;
  vEntitiesCount: Integer;
  vEntities: PsgObjectArray;

  vEntityProxy: TsgObjEntity3D;
  vNode: TNodeLite;
{$IFDEF SG_ROTATE_CENTER_TEST}
  vDrawMatrix: TFMatrix;
{$ENDIF}

  vIndex: Integer;
  vOwner, vInsertNode: TNodeLite;

begin

{$IFDEF SG_ROTATE_CENTER_TEST}
  if not FIsRequestedDrawMatrix then
  begin
    GetView(FDrawMatrixReload);
    FIsRequestedDrawMatrix := True;
  end;
{$ENDIF}

{$IFDEF DEBUG_STRUCTURE}
  UpdateRootToTreeView(nil, TCustomNode(FRoot));
{$ENDIF}


  vEntitiesCount := ExtractArrayFromList(AEntities, Pointer(vEntities));
  if vEntitiesCount > 0 then
  begin
    C := -1;
    vCountLineOuterWire := -1;
    FNav3D.Buffer.Freeze;
    try
      BeginUpdate;
      try
{$IFDEF CLIP_SGMESH}
      if FNav3D.IsClipPlaneUses then
      begin
        FNav3D.FLockClip := True;
        FNav3D.RestoreOriginalMeshData;
        {for I := 0 to FNav3D.FLastOriginalMesh do
          TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).RestoreData;}


        RestoreOutline;
        FNav3D.GLFreeForm.StructureChanged;
      end;
{$ENDIF}
      TGLFreeFormAccess(FNav3D.GLFreeForm).DropMaterialLibraryCache;
      if Assigned(FNav3D.FLineOuterWire) then
          vCountLineOuterWire := FNav3D.FLineOuterWire.FPolylines.Count;
      for I := 0 to vEntitiesCount - 1 do
      begin
        if vEntities^[I] is TsgObjEntity3D then
        begin
          vEntityProxy := TsgObjEntity3D(vEntities^[I]);
          vNode := TNodeLite(FRoot).GetNodeByStrKey(
            vEntityProxy.PathKey);
          if not Assigned(vNode) then
            Continue;
          vMeshes := TsgObjectList.Create;
          try
            vNode.GetMeshList(vMeshes);
            for J := 0 to vMeshes.Count - 1 do
            begin
              vMesh := TsgMeshObject(vMeshes[J]);
              if vNode.Visible then
              begin
                if vMesh.IsUseVisibleData then
                begin
                  vMesh.RestoreVisibleData;
                  vMesh.AddConnectionWithEdges(FNav3D.FLineOuterWire);
                  vMesh.Visible := True;
                end;
              end
              else
              begin
                if not vMesh.IsUseVisibleData then
                begin
                  vMesh.SaveVisibleData;
                  vMesh.Clear;
                  vMesh.Visible := False;
                  vMesh.BreakConnectionWithEdges(FNav3D.FLineOuterWire);
                end;
              end;
            end;
          finally
            vMeshes.Free;
          end;
          Continue;
        end;

        vInsert := nil;
        if vEntities^[I] is TsgDXFInsert then
          vInsert := TsgDXFInsertAccess(vEntities^[I]);

        if Assigned(vInsert) then
        begin
          vMeshes := GetInsertMeshes(vEntities^[I]);
          try
            for J := vMeshes.Count - 1 downto 0 do
            begin
              vMesh := TsgMeshObject(vMeshes[J]);
              vMesh.BreakConnectionWithEdges(FNav3D.FLineOuterWire);
              TObject(vMeshes[J]).Free;
            end;
          finally
            vMeshes.Free;
          end;

          // For complex drawings, a different search algorithm is needed!!!
          // There will be an Insert list (the object can have multiple occurrences!!!
          TNodeLite(FRoot).FindDeep(vInsert, vOwner, vIndex);
          if (vIndex >= 0) and (vIndex < TRoot(vOwner).Count) then
          begin
            vInsertNode := Pointer(TRoot(vOwner)[vIndex]);
//            TCustomNode.ClearVisualization(TCustomNode(vInsertNode));
            vInsertNode.Clear;
          end;

        end;
        if Assigned(FNav3D.FLineOuterWire) then
          vCountLineOuterWire := FNav3D.FLineOuterWire.FPolylines.Count
        else
          vCountLineOuterWire := 0;
        C := FNav3D.GLFreeForm.MeshObjects.Count;
        vParams := InitializeIterateParams;
        {vCustomFindEnt := TCustomFindEnt.Create;
        try
          vCustomFindEnt.Obj := TObject(AEntities[I]);
          vCustomFindEnt.Matrix := vParams^.Matrix;
          vCustomFindEnt.Iterate(FConverter);
          vParams^.Matrix := vCustomFindEnt.Matrix;
        finally
          vCustomFindEnt.Free;
        end;}
        if Assigned(vInsert) then
        begin
          vTmpParams := FConverter.Params;
          try
            FConverter.Params := vParams;
            //vAutoInsert := FConverter.AutoInsert;
            FVectorFile := Pointer(1);
            try
              //FConverter.AutoInsert := True;
              FInsert := vInsert;
              //vInsert.Invoke(FConverter, GetEntity, FinishEntity);
              TsgDXFConverterAccess(FConverter).GetModelLayout.Iterate(FConverter, GetFilteredEntity, FinishEntity);
              FInsert := nil;
            finally
              //FConverter.AutoInsert := vAutoInsert;
              FVectorFile := nil;
            end;
          finally
            FConverter.Params := vTmpParams;
          end;
          Load3dFaceMeshObject;
          ResetDefaults;
        end;
      end;

    {$IFDEF CLIP_CSG_GLMESH_SGMESH }
      FNav3D.FLastOriginalMesh := FNav3D.GLFreeForm.MeshObjects.Count - 1;
    {$ENDIF}

      PickObjects.Objects.Clear;
      if (C <> -1) and (C < FNav3D.GLFreeForm.MeshObjects.Count) then
      begin
        for I := C to FNav3D.GLFreeForm.MeshObjects.Count - 1 do
          PickObjects.Objects.Add(FNav3D.GLFreeForm.MeshObjects[I]);
        PickObjects.BuildLines;
        Navigator3D.GLFreeForm.MeshObjects.Sort(MeshEntCompare);
      end;
      if Assigned(FNav3D.FLineOuterWire) then
      begin
        FNav3D.FLineOuterWire.ClearGLHandles;
        if vCountLineOuterWire < FNav3D.FLineOuterWire.FPolylines.Count then
        begin
          for I := 0 to FNav3D.GLFreeForm.MeshObjects.Count - 1 do
            TsgMeshObject(FNav3D.GLFreeForm.MeshObjects[I]).FEdgeIndexList.Clear;
          FProgressObject.PercentDone := 0;
          FProgressObject.Starting;
          FProgressObject.Step :=
            100 / (FNav3D.FLineOuterWire.FPolylines.Count + vEntitiesCount);
          FNav3D.FLineOuterWire.PreProcessing(FProgressObject, FAccuracyDim,FRoot);
          FProgressObject.Ending;
        end;
      end;
      AppShowingStyle;
      FNav3D.GLFreeForm.StructureChanged;
      finally
{$IFDEF CLIP_SGMESH}
        FNav3D.FLockClip := False;
        GetBoundingSphere(FNav3D.FGLFreeForm.MeshObjects, FNav3D.FSceneRadius, FNav3D.FSceneCenter);
        FNav3D.FSceneCenter := VectorTransform(FNav3D.FSceneCenter, FNav3D.FGLFreeForm.Matrix);
        if FNav3D.IsClipPlaneUses then
        begin
          SaveOutline;
          ApplyClip;
        end;
{$ENDIF}
        EndUpdate;
      end;
    finally
      FNav3D.Buffer.Melt;
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.RepaintScene;
begin
  if (UpdateCount = 0) and not (csDestroying in ComponentState) then
    Perform(WM_PAINT, FOwnDC, 0);
end;

procedure Tsg3DDrawingNavigator.ResetDefaults;
var
  I: Integer;
  vMesh: TMeshObject;
begin
  // We process the list of meshes selected for removal
  for I := 0 to FDeferredObjectToDelete.Count - 1 do
  begin
    vMesh := TMeshObject(FDeferredObjectToDelete[I]);
    if TsgMeshObject(vMesh).InheritsFrom(TsgMeshObject) then
      TsgMeshObject(vMesh).BreakConnectionWithEdges(FNav3D.FLineOuterWire);
    FreeAndNil(vMesh);
  end;
  FDeferredObjectToDelete.Clear;

  FCurrMeshObject3dFace := nil;
  FCurrVertexes3dFace := nil;
  FCurrMeshObject := nil;
  FCurrVertexes := nil;
  FIsBrepEntityIterate := False;
  FCurrColor := clNone;
  FCurrColor3dFace := clNone;
  FCurrLineWieght := 1;
end;

procedure Tsg3DDrawingNavigator.ResetSelectObjects;
var
  I: Integer;
begin
  PickObject := nil;
  HighLightObject := nil;
  for I := Low(FSnapObj) to High(FSnapObj) do
  begin
    FSnapObj[I].Obj := nil;
  end;
{$IFDEF USE_CLIP}
  for I := Low(FSnapPlan) to High(FSnapPlan) do
  begin
    FSnapPlan[I].Obj := nil;
  end;
{$ENDIF}
  FIndexSnap := 0;
  FSnapDim.Obj := nil;
  FSnapEdge.Obj := nil;
  MoveDim := nil;
end;

function Tsg3DDrawingNavigator.Rotate(Axis: TsgAxes; Angle: Extended): Boolean;
begin
  Result := False;
  if FNav3D = nil then Exit;
  FNav3D.Rotate(Axis, Angle);
  LastViewName := sUserView;
  Result := True;
end;

function Tsg3DDrawingNavigator.Rotate(const APitch, ATurn, ARoll: Extended): Boolean;
begin
  Result := False;
  if FNav3D = nil then Exit;
  FNav3D.Rotate(APitch, ATurn, ARoll);
  LastViewName := sUserView;
  Result := True;
end;

procedure Tsg3DDrawingNavigator.RotToView(const A3DView: TsgDXFViewDirection);
var
  vM4f, vSceneM4f: TMatrix4f;
begin
  if not Assigned(FNav3D) then Exit;
  FNav3D.Buffer.BeginUpdate;
  try
  if (A3DView = vdDefault) and Assigned(FConverter) then
    MatrixToGLMatrix(FConverter.ViewTwistMatrix, vM4f)
  else
    vM4f := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.IdentityHmgMatrix;
  FNav3D.GLDCScene.ResetRotations;
  FNav3D.FAxes.ResetRotations;
  case A3DView of
    vdDefault:
      begin
        vSceneM4f := FNav3D.FAxes.FGLDCAxes.Matrix;
        {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NormalizeMatrix(vSceneM4f);
        FNav3D.FAxes.FGLDCAxes.Matrix := MatrixMultiply(vM4f, vSceneM4f);

        vSceneM4f := FNav3D.GLDCScene.Matrix;
        {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NormalizeMatrix(vSceneM4f);
        FNav3D.GLDCScene.Matrix := MatrixMultiply(vM4f, vSceneM4f);
      end;
    vdFront:
      begin
        Rotate(AxisX, -90);
      end;
    vdBottom:
      begin
        Rotate(AxisY, 180);
      end;
    vdLeft:
      begin
        Rotate(AxisY, 90);
        Rotate(AxisZ, 90);
      end;
    vdRight:
      begin
        Rotate(AxisY, -90);
        Rotate(AxisZ, -90);
      end;
    vdBack:
      begin
        Rotate(AxisX, 90);
        Rotate(AxisZ, 180);
      end;
    vdSWIsometric:
      begin
        Rotate(AxisZ, 45);
        Rotate(AxisX, -fIsometricRotation);
      end;
    vdSEIsometric:
      begin
        Rotate(AxisZ, -45);
        Rotate(AxisX, -fIsometricRotation);
      end;
    vdNWIsometric:
      begin
        Rotate(AxisZ, 135);
        Rotate(AxisX, -fIsometricRotation);
      end;
    vdNEIsometric:
      begin
        Rotate(AxisZ, -135);
        Rotate(AxisX, -fIsometricRotation);
      end;
  end;
  LastViewName := GetViewDirectionName(A3DView);
  FitToSize;
  finally
    FNav3D.Buffer.EndUpdate;
  end;
end;

procedure Tsg3DDrawingNavigator.SaveCrossSectionOCT(const AName: String; const AOutputType: Integer);
begin
  raise EExternalException.Create('change in furure version');
end;

procedure Tsg3DDrawingNavigator.SaveDimensions(const AFileName: string = '');
var
  vFileName: string;
  vXML: TsgParser;
  vNode: TsgNode;
begin
  vFileName := AFileName;
  if (vFileName = '') then
    if Assigned(LoadFromImage) then
      vFileName := LoadFromImage.FileName + cnstXMLDim3dFileExt + '.xml'
    else
      Exit;
  if UpperCase(ExtractFileExt(vFileName)) <> '.XML' then
    vFileName := vFileName + '.xml';
  vXML := TsgParser.Create;
  try
    vNode := vXML.CreateRootNode(cnstXMLRootNode, True);
    vNode.AddAttribNV(cnstXMLCmdVersion).ValueData.ValueAsVersion := cnstXMLIntfaceVersion;
    vNode := vNode.AddChildNV(cnstXMLDim3dNode);
    if DimensionsToXMLNode(vNode) > 0 then
      vXML.SaveToFile(vFileName);
  finally
    vXML.Free;
  end;
end;

{$IFDEF SG_XKT}
procedure Tsg3DDrawingNavigator.SaveToXKT(const AFileName: string;
  const AStream: TStream);
var
  I: Integer;
  vModel: TsgXKTModel;
  vCfg: TsgXKTModelCfg;
  rootMetaObjectId: string;
  rootMetaObject, currentMetaObject: TsgXKTMetaObject;
  nextGeometryId: UInt64;
  geometryId: string;
  meshId: string;
  entityId: string;
  vMesh: TsgMeshObject;
  params: TsgGeometryParams;

  vLog: TStringList;

  procedure ConvertMeshToXKT(const AMesh: TsgMeshObject;
    const positions: TsgFloat64Array; const indices: TsgUInt32Array);
  var
    I,J, Index, MarkIndex: Integer;
    vVertxs: PAffineVectorArray;
    vIndxs: PIntegerArray;
    vList: TFGVertexIndexList;
    vCountIndices: Integer;
    vIndices, vIndicesCopy, vIndicesTemp: TsgIntegerList;
  begin
    vVertxs := PAffineVectorArray(vMesh.Vertices.List);
    positions.Count := vMesh.Vertices.Count * 3;

    vIndices := TsgIntegerList.Create;
    vIndicesCopy := TsgIntegerList.Create;
    vIndicesTemp:= TsgIntegerList.Create;
    try
      vCountIndices := 0;
      for I := 0 to AMesh.FaceGroups.Count - 1 do
        if AMesh.FaceGroups[I] is TFGVertexIndexList then
          Inc(vCountIndices,
            TFGVertexIndexList(AMesh.FaceGroups[I]).VertexIndices.Count);
      vIndices.Count := vCountIndices;
      vCountIndices := 0;
      for I := 0 to AMesh.FaceGroups.Count - 1 do
      if AMesh.FaceGroups[I] is TFGVertexIndexList then
      begin
        vList := TFGVertexIndexList(AMesh.FaceGroups[I]);
        vIndxs := PIntegerArray(vList.VertexIndices.List);
        for J := 0 to vList.VertexIndices.Count - 1 do
        begin
          vIndices[TsgMath.IncPostfix(vCountIndices)] := vIndxs^[J];
        end;
      end;

      if vIndices.Count = 0 then
        Exit;

      vIndicesCopy.AppendArray(vIndices.List^, vIndices.Count);
      vIndicesTemp.Count := vIndicesCopy.Count;
      vIndicesCopy.Sort;
      vIndicesCopy.Sorted := True;
      Index := 0;
      MarkIndex := vIndicesCopy[0];
      vIndicesTemp[Index] := 0;

      positions[0] := vVertxs^[MarkIndex].X;
      positions[1] := vVertxs^[MarkIndex].Y;
      positions[2] := vVertxs^[MarkIndex].Z;

      for I := 1 to vIndicesCopy.Count - 1 do
      begin
        if MarkIndex <> vIndicesCopy[I] then
        begin
          Inc(Index);
          MarkIndex := vIndicesCopy[I];
          positions[Index*3] := vVertxs^[MarkIndex].X;
          positions[Index*3+1] := vVertxs^[MarkIndex].Y;
          positions[Index*3+2] := vVertxs^[MarkIndex].Z;

        end;
        vIndicesTemp[I] := Index;
      end;
      positions.Count := (Index+1)*3;
      indices.Count := vIndices.Count;
      for I := 0 to vIndices.Count - 1 do
      begin
        Index := vIndicesCopy.IndexOf(vIndices[I]);
        indices[I] := vIndicesTemp[Index];
      end;

    finally
      vIndices.Free;
      vIndicesCopy.Free;
      vIndicesTemp.Free;
    end;
  end;

begin


  vCfg := cnstXKTModelCfgDefault;


  vModel := TsgXKTModel.Create(cnstXKTModelCfgDefault);
  try
     rootMetaObjectId := TsgMath.createUUID;

     nextGeometryId := 0;

     rootMetaObject := vModel.createMetaObject(
       TsgMetaObjectParams.Create(rootMetaObjectId, 'Model', 'Model')
       );

     currentMetaObject := rootMetaObject;

  var paramsG: TsgGeometryParams;
  var paramsM: TsgMeshParams;
  var paramsE: TsgEntityParams;
  var Index: Integer := 0;
  var CurrentKey: string;

 var Position : TFPoint;
  var Scale: TFPoint;
  var  rotation: TFPoint;
  var color : TsgFloat32Array;
  color := TsgFloat32Array.Create(3);

  SetLength(paramsE.meshIds, 1);

  var vMatLibItem: TGLLibMaterial;
  var vColor : TColorVector;


    for I := 0 to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
    begin
      if TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).InheritsFrom(TsgMeshObject) then
      begin
        vMesh := TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]);

        if vMesh.FaceGroups.Count > 0 then
        begin
          vMatLibItem := Navigator3D.FGLMaterialLibrary.Materials.
            GetLibMaterialByName(vMesh.FaceGroups[0].MaterialName);
          vColor := vMatLibItem.Material.FrontProperties.Diffuse.Color;

        end;


        CurrentKey := TsgMath.createUUID();

        geometryId := 'Geometry' + IntToStr(TsgMath.IncPostfix(nextGeometryId))+CurrentKey;
        meshId := 'Mesh' + IntToStr(TsgMath.IncPostfix(nextGeometryId))+CurrentKey;
        entityId := 'Entity' + IntToStr(TsgMath.IncPostfix(nextGeometryId))+CurrentKey;

        //Geometry
        paramsG.geometryId := geometryId;
        paramsG.primitiveType := 'triangles';
        paramsG.positions := TsgFloat64Array.Create(0);
        paramsG.indices := TsgUInt32Array.Create(0);

        ConvertMeshToXKT(vMesh, paramsG.positions, paramsG.indices);

        if paramsG.indices.Count = 0 then
          Continue;

        paramsG.edgeThreshold := 10;

        vModel.createGeometry(paramsG);

        Position := MakeFPoint(0,0,0);
        Scale := MakeFPoint(1,1,1);
        rotation := MakeFPoint(0,0,0);

       color[0] := vColor.X;//0;//Random;//1;
       color[1] := vColor.Y;//1;//Random;//0;
       color[2] := vColor.Z;//0;//Random;//0;


    paramsM.meshId := meshId;
    paramsM.geometryId := geometryId;
    paramsM.Position := @Position;
    paramsM.Scale := @Scale;
    paramsM.rotation := @rotation;
    paramsM.color := color;
    paramsM.opacity := 1;
    paramsM.metallic := 1;

    vModel.createMesh(paramsM);

    if I mod 20 = 0 then
    begin

     currentMetaObject := vModel.createMetaObject(
       TsgMetaObjectParams.Create(TsgMath.createUUID, 'Block', 'Block', rootMetaObject.metaObjectId)
       );
    end;


    paramsE.entityId := entityId;
    paramsE.meshIds[0] := meshId;

    vModel.createEntity(paramsE);

    vModel.createMetaObject(
   TsgMetaObjectParams.Create(paramsE.entityId,
     'Default',
     'Abviewer Mesh',
      currentMetaObject.metaObjectId)
  );


      end;
    end;

  //
 vModel.createDefaultMetaObjects();

  vModel.finalize();

  var vStream: TMemoryStream;
  var vXktData: TsgXktData;

   vStream := TMemoryStream.Create;
   try
     vStream.Position := 0;
     vXktData := TsgXktData.Create;
     try
       vXktData.Save(vStream, vModel);
       vStream.Position := 0;
       vStream.SaveToFile('D:\1test_3.xkt');
     finally
       vXktData.Free;
     end;
   finally
     vStream.Free;
   end;

  finally
  end;
end;
{$ENDIF}

{$IFDEF SG_OBJ_TO_DWG}
procedure Tsg3DDrawingNavigator.SaveToCadImage(const ACADImage: TsgCADImage);
var
  I, J: Integer;
  vTrianglesList: TAffineVectorList;
  vBlock: TsgDXFBlock;
  vSTLInsert: TsgDXFInsert;
  vLayer: TsgDXFLayer;
  vMesh: TMeshObject;
  vFace: TsgDXF3dFace;
begin
  if not Assigned(ACADImage) then
    Exit;

  vLayer := ACADImage.Converter.LayerByName('0');
  vLayer.ColorCAD := ConvertARGBToColorCAD(clGray);

  vBlock := TsgDXFBlock.Create;
  vBlock.Name := GetNameCorrect(cnstSgStlNamePref + IntToStr(TsgNativeUINT(vBlock)));
  ACADImage.Converter.Sections[csBlocks].AddEntity(vBlock);
  vSTLInsert := TsgDXFInsert.Create;
  vSTLInsert.ColorCAD := vLayer.ColorCAD;
  vSTLInsert.LineWeight := fLineWeightByLayer;
  vSTLInsert.Block := vBlock;
  ACADImage.CurrentLayout.PaperSpaceBlock.AddEntity(vSTLInsert);

  for I := 0 to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
  begin
    vMesh := FNav3D.FGLFreeForm.MeshObjects.Items[I];
    vTrianglesList := vMesh.ExtractTriangles;
    try
      J := 0;
      while J < vTrianglesList.Count do
      begin
        vFace := TsgDXF3dFace.Create;
        vFace.Point := Vect2FPoint(vTrianglesList[J]);
        vFace.Point1 := Vect2FPoint(vTrianglesList[J+1]);
        vFace.Point2 := Vect2FPoint(vTrianglesList[J+2]);
        vFace.Point3 := vFace.Point2;
        vBlock.AddEntity(vFace);
        Inc(J, 3);
      end;
    finally
      vTrianglesList.Free;
    end;
  end;

  J := 0;
  while J < vBlock.Count do
  begin
    vFace := TsgDXF3dFace(vBlock[J]);
    ACADImage.Converter.Loads(vFace);
    Inc(J);
  end;
  ACADImage.Converter.Loads(vBlock);
  ACADImage.Converter.Loads(vSTLInsert);

  ACADImage.GetExtents;
end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.SaveToStreamGLScene(const AFileName: string;
  const AStream: TStream; AUseClip: Boolean);
{$IFDEF SG_XKT}
var
  ext: string;
{$ENDIF}
begin
  if not Assigned(FNav3D) then
    Exit;
  if not Assigned(FNav3D.GLFreeForm) then
    Exit;

{$IFDEF SG_XKT}
  ext := ExtractFileExt(AFileName);
  System.Delete(ext, 1, 1);
  SaveToXKT(AFileName, AStream);
  if True then //XKT
  begin

  end;
  Exit;
{$ENDIF}

  if AUseClip  then
    FNav3D.GLFreeForm.SaveToStream(AFileName, AStream)
  else
  begin
{$IFDEF CLIP_SGMESH}
    if FNav3D.IsClipPlaneUses then
    begin
      FNav3D.FLockClip := True;
      FNav3D.RestoreOriginalMeshData;
      {for I := 0 to FNav3D.FLastOriginalMesh do
        TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).RestoreData;}
      RestoreOutline;
      FNav3D.GLFreeForm.StructureChanged;
    end;
{$ENDIF}

    FNav3D.GLFreeForm.SaveToStream(AFileName, AStream);

{$IFDEF CLIP_SGMESH}
    FNav3D.FLockClip := False;
    if FNav3D.IsClipPlaneUses then
    begin
      SaveOutline;
      ApplyClip;
    end;
{$ENDIF}
  end;

end;

procedure Tsg3DDrawingNavigator.LoadDimensions(const AFileName: string;
  const AOnCreate: TsgOnDim3dCreateProc = nil; ACustom: Boolean = False);
var
  vFileName, vExt: string;
  vXML: TsgParser;
  vNode: TsgNodeSample;
begin
  if Length(AFileName) = 0 then
    ACustom := False;
  vExt := '';
  if not ACustom then
    vExt := cnstXMLDim3dFileExt;
  vExt := vExt + cnstXMLExt;
  vFileName := AFileName;
  if (vFileName = '') then
  begin
    if Assigned(LoadFromImage) then
      vFileName := LoadFromImage.FileName + vExt
    else
      Exit;
  end
  else
  begin
    if not SameText(ExtractFileExt(vFileName), vExt) then
      vFileName := vFileName + vExt;
  end;
  if not FileExists(vFileName) then
    Exit;
  vXML := TsgParser.Create;
  try
    if vXML.LoadFromFile(vFileName) then
    begin
      vNode := vXML.ROOT.NodeByName[cnstXMLRootNode];
      if Assigned(vNode) and (vNode.ClassType = TsgNode) then
      begin
        vNode := vNode.GetChildByName(cnstXMLDim3dNode);
        if Assigned(vNode) and (vNode.ClassType = TsgNode) then
          DimensionsFromXMLNode(vNode, AOnCreate);
      end;
    end;
  finally
    vXML.Free;
  end;
end;


function Tsg3DDrawingNavigator.DimensionsToXMLNode(const ANode: TsgNode): Integer;
var
  I, vCntI: Integer;
  vDimNode, vPolyNode: TsgNode;
  vPolylineDim: TsgGLPolyline;
  vDimObj: TsgPickObject;

  function CreateTopDimNode(AName: string; AObj: TsgPickObject): TsgNode;
  begin
    Result := TsgNode.Create;
    Result.Name := AName;
    if Assigned(AObj) then
      Result.AddAttribNV(cnstXMLUserStringData).ValueAsStr := AObj.UserString;
  end;

begin
  Result := 0;
  if not Assigned(FNav3D) then
    Exit;
  if not Assigned(FNav3D.FDimensionList) then
    Exit;
  vCntI := FNav3D.FDimensionList.Count - 1;
  if vCntI < 0  then
    Exit;
  ANode.AddAttribNV(cnstXMLDim3dScaleFactor).ValueAsDouble := MeasureScaleFactor;
  ANode.AddAttribNV(cnstXMLDim3dPrecisionFactor).ValueAsInt := MeasurePrecisionFactor;
  ANode.AddAttribNV(cnstXMLDim3dDisplayedUnits).ValueAsInt :=
    Integer(MeasureDisplayedUnits);
  ANode.AddAttribNV(cnstXMLDim3dInitialUnits).ValueAsInt :=
    Integer(MeasureInitialUnits);

  ANode.AddAttribNV(cnstXMLDim3dBoxOffset).ValueAsFPoint := FBoxOffs;
  for I := 0 to vCntI do
  begin
    vDimObj := GetDimension(I);
    if Assigned(vDimObj) and Assigned(vDimObj.Obj) and vDimObj.Obj.InheritsFrom(TsgGLPolyline) then
    begin
      vPolylineDim := TsgGLPolyline(vDimObj.Obj);


      if vPolylineDim.FType = 7 then
      begin
        vDimNode := CreateTopDimNode(cnstXMLDim3dDimPoint, vDimObj);
        vDimNode.Name := cnstXMLDim3dDimPoint;
      end
      else
      if vPolylineDim.Count = 2 then
      begin
        vDimNode := CreateTopDimNode(cnstXMLDim3dDimDistance, vDimObj);
        vDimNode.Name := cnstXMLDim3dDimDistance;
        vDimNode.AddAttribNV(cnstXMLDim3dDistance).ValueAsDouble := vPolylineDim.Distance;
      end
      else
      begin
        vDimNode := CreateTopDimNode(cnstXMLDim3dDimRadius, vDimObj);
        vDimNode.AddAttribNV(cnstXMLDim3dRadius).ValueAsDouble := vPolylineDim.Radius;
        vDimNode.AddAttribNV(cnstXMLDim3dCenter).ValueAsFPoint := vPolylineDim.Center;
      end;
      if Assigned(vDimNode) then
      begin
        vDimNode.AddAttribNV(cnstXMLDim3dBegin).ValueAsFPoint := vPolylineDim.FBegin;
        vDimNode.AddAttribNV(cnstXMLDim3dEnd).ValueAsFPoint := vPolylineDim.FEnd;
        vDimNode.AddAttribNV(cnstXMLDim3dNormal).ValueAsFPoint := vPolylineDim.FNormal;
        vDimNode.AddAttribNV(cnstXMLDim3dPolylineType).ValueAsInt := vPolylineDim.FType;
        vDimNode.AddAttribNV(cnstXMLDim3dPosition).ValueData.ValueAsPoint := vDimObj.Pos;

        vPolyNode := TsgNode.Create;
        vPolyNode.Name := cnstXMLDim3dPolyline;
        vPolylineDim.ToXML(vPolyNode, cnstXMLDim3dPolyline);
        vDimNode.AddChild(vPolyNode);
        if Assigned(vPolylineDim.FExData) then
        begin
          vPolyNode := TsgNode.Create;
          vPolyNode.Name := cnstXMLDim3dPolylineExt;
          vPolylineDim.FExData.ToXML(vPolyNode, cnstXMLDim3dPolylineExt);
          vDimNode.AddChild(vPolyNode);
        end;
        ANode.AddChild(vDimNode);
        Inc(Result);
      end;
    end;
  end;
end;

function Tsg3DDrawingNavigator.DimensionsFromXMLNode(const ANode: TsgNodeSample; const AOnCreate: TsgOnDim3dCreateProc): Integer;
var
  I, vCntI, vDimType: Integer;
  vDimNode, vPolyNode, vAttr: TsgNodeSample;
  vPolylineDim: TsgGLPolyline;
  vDimObj: TsgPickObject;
  vPoint: TFPoint;
  vBoxOffset: TFPoint;
  vDimensionScaleFactor: Double;
begin
  Result := 0;
  vAttr := ANode.GetAttributeByName(cnstXMLDim3dScaleFactor);
  if Assigned(vAttr) then
    MeasureProps.ScaleFactor := vAttr.ValueAsDouble;
  vDimensionScaleFactor := IfThen(
    isZero(MeasureProps.ScaleFactor), 1.0,
    MeasureProps.ScaleFactor);
  vAttr := ANode.GetAttributeByName(cnstXMLDim3dPrecisionFactor);
  if Assigned(vAttr) then
     MeasureProps.PrecisionFactor := vAttr.ValueAsInt;
  vAttr := ANode.GetAttributeByName(cnstXMLDim3dDisplayedUnits);
  if Assigned(vAttr) then
     MeasureProps.DisplayedUnits := TsgInsUnits(vAttr.ValueAsInt);
  vAttr := ANode.GetAttributeByName(cnstXMLDim3dInitialUnits);
  if Assigned(vAttr) then
     MeasureProps.InitialUnits := TsgInsUnits(vAttr.ValueAsInt);
  vAttr := ANode.GetAttributeByName(cnstXMLDim3dBoxOffset);
  vBoxOffset := cnstFPointZero;
  if Assigned(vAttr) then
    vBoxOffset := vAttr.ValueAsFPoint;

  vCntI := ANode.ChildNodesCount - 1;
  for I := 0 to vCntI do
  begin
    vDimNode := ANode.ChildNodes[I];
    if (vDimNode.ClassType = TsgNode) then
    begin
      vDimType := -1;
      if vDimNode.Name = cnstXMLDim3dDimDistance then
        vDimType := 0
      else
      if vDimNode.Name = cnstXMLDim3dDimRadius then
        vDimType := 1
      else
      if vDimNode.Name = cnstXMLDim3dDimPoint then
        vDimType := 7;
      if vDimType >= 0 then
      begin

        vPolylineDim := CreateGLPolyline;

        vPolyNode := vDimNode.GetChildByName(cnstXMLDim3dPolyline);
        if Assigned(vPolyNode) and (vPolyNode.ClassType = TsgNode) then
          vPolylineDim.FromXML(vPolyNode);

        vPolyNode := vDimNode.GetChildByName(cnstXMLDim3dPolylineExt);
        if Assigned(vPolyNode) and (vPolyNode.ClassType = TsgNode) then
          vPolylineDim.FExData.FromXML(vPolyNode);

        vAttr := vDimNode.GetAttributeByName(cnstXMLDim3dBegin);
        if Assigned(vAttr) then
          vPolylineDim.FBegin := vAttr.ValueAsFPoint;

        vAttr := vDimNode.GetAttributeByName(cnstXMLDim3dEnd);
        if Assigned(vAttr) then
          vPolylineDim.FEnd := vAttr.ValueAsFPoint;

        vAttr := vDimNode.GetAttributeByName(cnstXMLDim3dNormal);
        if Assigned(vAttr) then
          vPolylineDim.FNormal := vAttr.ValueAsFPoint;

        vAttr := vDimNode.GetAttributeByName(cnstXMLDim3dPolylineType);
        if Assigned(vAttr) then
          vPolylineDim.FType := vAttr.ValueAsInt;

        case vDimType  of
        0:
          begin
            GetPolylineText(DoubleToStringWithPrecision(vPolylineDim.Distance, MeasurePrecisionFactor), vPolylineDim.FTextList);
          end;
        1:
          begin
            vAttr := vDimNode.GetAttributeByName(cnstXMLDim3dRadius);
            if Assigned(vAttr) then
              vPolylineDim.FRadius := vAttr.ValueAsDouble/vDimensionScaleFactor;

            vAttr := vDimNode.GetAttributeByName(cnstXMLDim3dCenter);
            if Assigned(vAttr) then
              vPolylineDim.FCenter := vAttr.ValueAsFPoint;

            GetPolylineText('R ' +  DoubleToStringWithPrecision(vPolylineDim.Radius, MeasurePrecisionFactor), vPolylineDim.FTextList);
          end;
        7:
          begin
            vPoint := SubFPoint(vPolylineDim.List[0], FBoxOffs);
            GetPolylineText(
              'X=' + DoubleToStringWithPrecision(vPoint.X * MeasureScaleFactor, MeasurePrecisionFactor) + ' ' +
              ' Y=' + DoubleToStringWithPrecision(vPoint.Y * MeasureScaleFactor, MeasurePrecisionFactor) + ' ' +
              ' Z=' + DoubleToStringWithPrecision(vPoint.Z * MeasureScaleFactor, MeasurePrecisionFactor) + ' ',
              vPolylineDim.FTextList);
          end;
        end;

        vPolylineDim.LineWidth := Measure3DParams.DimLwd;
        vPolylineDim.FHeight := Measure3DParams.DimLine;
        vPolylineDim.Mode := GL_LINES;
        vPolylineDim.PointSize := Measure3DParams.PointSize;
        vPolylineDim.MaterialName := AssignMaterial(clRed, FNav3D.FGLMaterialLibrary);
        vPolylineDim.MaterialNamePoint := AssignMaterial(Measure3DParams.SelectNodeColor, FNav3D.FGLMaterialLibrary);
        vPolylineDim.MaterialNameText := AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRT), FNav3D.FGLMaterialLibrary);
        vPolylineDim.MaterialNameLine := AssignMaterial(ConvertColorCADToRGB(FDimByDrawing.DIMCLRD), FNav3D.FGLMaterialLibrary);
        vPolylineDim.ArrowSize := FDimByDrawing.ArrowSize;

        vDimObj := CreateDimension;

        vAttr := vDimNode.GetAttributeByName(cnstXMLUserStringData);
        if Assigned(vAttr) then
          vDimObj.UserString := vAttr.ValueAsStr;
        vDimObj.Obj := vPolylineDim;
        vAttr := vDimNode.GetAttributeByName(cnstXMLDim3dPosition);
        if Assigned(vAttr) then
          vDimObj.Pos := vAttr.ValueData.ValueAsPoint;
         if Assigned(AOnCreate) then
          AOnCreate(vDimObj, vDimType, vBoxOffset);
      end;
      Inc(Result);
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.ScenePositionChange(Sender: TObject);
begin
  SetupLight(GetBox);
end;


procedure Tsg3DDrawingNavigator.SetAntiAliasing(const Value: Integer);
begin
  if (Value < Integer(Low(TGLAntiAliasing))) or (Value > Integer(High(TGLAntiAliasing))) then
    Exit;
  if Assigned(FNav3D) then
  begin
    if FNav3D.Buffer.AntiAliasing <> TGLAntiAliasing(Value) then
    begin
      FNav3D.Buffer.AntiAliasing := TGLAntiAliasing(Value);
      RecreateWnd{$IFDEF SGFPC}(Self){$ENDIF};
    end;
  end;
end;
{$IFDEF USE_CLIP}
function Tsg3DDrawingNavigator.GetCrossSectionOn: Boolean;
begin
  Result := False;
  if Assigned(FNav3D) then
    Result := FNav3D.FCrossSectionOn;
end;

function Tsg3DDrawingNavigator.GetCuttingMode: TCuttingMode;
begin
  Result := FNav3D.FCuttingPlanes.CuttingMode;
end;

procedure Tsg3DDrawingNavigator.SetCrossSectChangeEvent(
  const Value: TNotifyEvent);
begin
  FOnCuttingPlanesChangeEvent := Value;
end;

procedure Tsg3DDrawingNavigator.SetCrossSectionEdgesColor(const Value: TColor);
begin
  FCrossSectionEdgesColor := Value;
  CrossSectionOn := CrossSectionOn;
end;

procedure Tsg3DDrawingNavigator.SetCrossSectionOn(const Value: Boolean);
{$IFDEF CLIP_GLCSG_GLMESH}
var
  I: Integer;
{$ENDIF}
begin
  if not Assigned(FNav3D) then
    Exit;
  if Value then
  begin
    if not FNav3D.IsClipPlaneUses then
      SaveOutline;
    FNav3D.FCrossSectionOn := Value;
    ApplyClip;
  end
  else
  if FNav3D.FCrossSectionOn then
  begin
    FNav3D.FCrossSectionOn := Value;
{$IFDEF CLIP_GLCSG_GLMESH_SGMESH}
{$IFDEF CLIP_SGMESH}
    FNav3D.RestoreOriginalMeshData;
    {for I := 0 to FNav3D.FLastOriginalMesh do
    begin
      TsgMeshObject(FNav3D.FGLFreeForm.MeshObjects.Items[I]).RestoreData;
    end;}
    FNav3D.GLFreeForm.StructureChanged;
{$ELSE}
    if FNav3D.FLastOriginalMesh < FNav3D.FGLFreeForm.MeshObjects.Count-1 then
    begin
      while FNav3D.FLastOriginalMesh < FNav3D.FGLFreeForm.MeshObjects.Count-1 do
        FNav3D.FGLFreeForm.MeshObjects.DeleteAndFree(FNav3D.FGLFreeForm.MeshObjects.Count-1);
    end;
    for I := 0 to FNav3D.FLastOriginalMesh do
    begin
      FNav3D.FGLFreeForm.MeshObjects.Items[I].Visible := True;
    end;
{$ENDIF}
    RestoreOutline;
{$ENDIF}
    AppShowingStyle;
  end;
end;
procedure Tsg3DDrawingNavigator.SetCuttingMode(const Value: TCuttingMode);
begin
  FNav3D.CuttingPlanes.CuttingMode := Value;
  CrossSectionOn := CrossSectionOn;
end;

{$ENDIF}

procedure Tsg3DDrawingNavigator.SetDefault3DSceneOptions;
begin
  FOuterWireColor := Measure3DParams.EdgesColor;//clGreen;
  if cnstUseCustomEdgeColor then
    FCrossSectionEdgesColor := cnstCustomEdgeColor
  else
    FCrossSectionEdgesColor := FOuterWireColor;
  FOuterWireLineWieght := cnstEdgeWeight3D;
  if Assigned(FOutlineShader) then
  begin
    TsgOutlineStencilShader(FOutlineShader).LineWidth := FOuterWireLineWieght * 2;
    TsgOutlineStencilShader(FOutlineShader).LineColor.AsWinColor := FOuterWireColor;
  end;
end;

procedure Tsg3DDrawingNavigator.SetEnablePickObjects(const Value: Boolean);
begin
  if Assigned(FPickObjects) then
    FPickObjects.Enabled := Value;
end;

procedure Tsg3DDrawingNavigator.SetExplodeProps(const Value: TsgExplodeProps);
var
  vDrawMatrix: TFMatrix;
  vType: Integer;
begin
  FExplodeProps := Value;
  vType := Integer(Value.ExplodeType)+1;
  ResetSelectObjects;
  if Assigned(Navigator3D.FBoxLine) then
    Navigator3D.FBoxLine.Visible := False;
  FNav3D.Buffer.Freeze;
  try
    BeginUpdate;
    try
      if FExplodeProps.Value = 0 then
        if Assigned(Navigator3D.FBoxLine) then
          Navigator3D.FBoxLine.Visible := FShowBoxDimensions;
{$IFDEF CLIP_CSG_GLMESH_SGMESH}
      UnBoxing(vType, FExplodeProps.Value);
{$ENDIF}
      FNav3D.GLFreeForm.StructureChanged;
    finally
      EndUpdate;
    end;
  finally
    GetView(vDrawMatrix);
    LoadView(vDrawMatrix);
    FNav3D.Buffer.Melt;
  end;
end;

procedure Tsg3DDrawingNavigator.SetMeasureEvent(const Value: TNotifyEvent);
begin
  FOnMeasureEvent := Value;
end;

procedure Tsg3DDrawingNavigator.SetMeasureMode3D(const Value: TsgMeasureMode3d);
begin
  FMeasureMode3D := Value;
  ShowHighlight := False;
  ShowSnap := False;
  ShowSnapEdge := False;
{$IFDEF USE_CLIP}
  ShowSnapPlan := False;
{$ENDIF}
  EnablePickObjects := True;
  FEdgeIntersect := False;

  case FMeasureMode3D of
    mmNone:
        if UserHighLight then
          ShowHighlight := True;
    mmHighLighting:
        ShowHighlight := True;
    mmSnap, mmSnapPoint:
        ShowSnap := True;
    mmSnapEdge:
        ShowSnapEdge := True;
    mmOffAll:
      begin
        EnablePickObjects := False;
        UserHighLight := False;
      end;
    mmSnapPlan:
      begin
{$IFDEF USE_CLIP}
        ShowSnapPlan := True;
{$ENDIF}
      end;
    mmSnapBetweenEdge:
      begin
        FEdgeIntersect := True;
        ShowSnap := True;
      end;
  end;
end;

procedure Tsg3DDrawingNavigator.SetMeasureDeleteEvent(const Value: TNotifyEvent);
begin
  if Assigned(FNav3D) then
    FNav3D.FOnMeasureDeleteEvent := Value;
end;

procedure Tsg3DDrawingNavigator.SetMeshQuality(const Value: TsgMeshQuality);
var
  vMatrix: TFMatrix;
  vExplodePropsSave: TsgExplodeProps;
begin
  if FIsLoadedFromConv and Assigned(FLoadFromImage) and
    Assigned(FConverter) and HasTriangledMesh and (Value <> MeshQuality) then
  begin
    FNav3D.Buffer.Freeze;
    try
      BeginUpdate;
      try
        vExplodePropsSave := FExplodeProps;
        case Value of
          mqLow: FDeviationCoefficient := cnstDeviationCoefficientDefault;
          mqNormal: FDeviationCoefficient := cnstModLinDeflection;
          mqHigh: FDeviationCoefficient := cnstDeviationCoefficientRealistic;
        end;
        ResetSelectObjects;
        FConverter.MeshQuality := FDeviationCoefficient;
        GetView(vMatrix);
        if Assigned(FNav3D) then
          FNav3D.FDimensionListClear := False;
        try
          LoadFromConverter(FConverter, FLoadFromImage);
        finally
          if Assigned(FNav3D) then
            FNav3D.FDimensionListClear := True;
        end;
        if vExplodePropsSave.Value > 0 then
        begin
          FExplodeProps := vExplodePropsSave;
{$IFDEF CLIP_CSG_GLMESH_SGMESH}
          UnBoxing(Integer(vExplodePropsSave.ExplodeType)+1, vExplodePropsSave.Value);
          AddBoxDimensions(FNav3D.FBoxLine, nil, False, FIsLoadedFromConv)
{$ENDIF}
        end;
      finally
        EndUpdate;
      end;
    finally
      LoadView(vMatrix);
      FNav3D.Buffer.Melt;
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.SetMouseRotate(const Value: Boolean);
begin
  FMouseRotate := Value;
end;

procedure Tsg3DDrawingNavigator.SetMoveDim(const Value: TsgPickObject);
var
  vDimObj: TsgPickObject;
  vGLPoly: TsgGLPolyline;
begin
  if Assigned(FMoveDim) or (Value = nil) then
  begin
    FMoveDim := nil;
    if Assigned(FOriginalObject) then
    begin
      FOriginalObject.Free;
      FOriginalObject := nil;
    end;
  end
  else
  begin
    vDimObj := TsgPickObject(Value.Obj);
    if Assigned(vDimObj) then
    begin
      vGLPoly := TsgGLPolyline(vDimObj.Obj);
      if Assigned(vGLPoly) then
      begin
        FOriginalObject := TsgGLPolyline.Create(FNav3D);
        FOriginalObject.Assign(vGLPoly);
      end;
    end;
    FMoveDim := Value;
  end;
end;

procedure Tsg3DDrawingNavigator.SetOnPickObject(const Value: TNotifyEvent);
begin
  FOnPickObject := Value;
end;

//procedure Tsg3DDrawingNavigator.SetParamsByMeshBuilder(
//  AParams: PsgParamTriangle; AInitData: PsgInitMeshBuilderData);
//begin
//  AParams^.ASelf := Self;
//  AParams^.MeshPointer := FCurrMeshObject;
//  AParams^.VertexPointer := nil;
//  ZeroMemory(AInitData, Sizeof(TsgInitMeshBuilderData));
//  AInitData^.TriangleFunc := @GetTriangle;
//  AInitData^.MeshParam := AParams;
//  AInitData^.NewFaceGroupFunc := @NewFaceGroup;
//  // Add Outer Wire
//{$IFDEF SG_SNAP3D}
//  AInitData^.Creator := Self;
//  AInitData^.DrawWireFunc := @CallBackOuterLineDraw;
//{$ENDIF}
//end;

procedure Tsg3DDrawingNavigator.SetSelected2(AEntities: TObject;
  APickObj: TsgPickObject; const ASelectModTopoClass: array of TClass);
var
  I: Integer;
  vEntitiesCount: Integer;
  vEntities: PsgObjectArray;

  procedure AddMeshObjects(AInsert: TObject; const ASelectModTopoClass: array of TClass);
  var
    I: Integer;
    vMeshes: TsgObjectList;
  begin
    vMeshes := GetNodeMeshes(TCustomNode(AInsert), ASelectModTopoClass);
    try
      for I := 0 to vMeshes.Count - 1 do
      begin
        //if APickObj.Objects.IndexOf(vMeshes[I]) = -1 then
          APickObj.Objects.Add(vMeshes[I]);
      end;
    finally
      vMeshes.Free;
    end;
  end;
begin
  vEntitiesCount := ExtractArrayFromList(AEntities, Pointer(vEntities));
  begin
    BeginUpdate;
    try
      if vEntitiesCount = 0 then
      begin
        APickObj.Obj := nil;
        APickObj.Objects.Clear;
        APickObj.ClearLines;
      end
      else
      begin
        //if EnablePickObjects then
        begin
          APickObj.Objects.Clear;
          for I := 0 to vEntitiesCount - 1 do
          begin
            AddMeshObjects(vEntities^[I], ASelectModTopoClass);
          end;
          APickObj.BuildLines;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.SetSelectByFace(const Value: Boolean);
begin
  FSelectByFace := Value;
end;

procedure Tsg3DDrawingNavigator.SetSelected(AEntities: TObject);
var
  I, J: Integer;
  vMesh: TObject;
  vEnt: TsgDXFEntity;
  vEntitiesCount: Integer;
  vEntities: PsgObjectArray;
{$IFDEF SG_ADD_BOX}
    vBox: TFRect;
{$IFDEF SG_ROTATE_CENTER_TEST}
  vAABB: TAABB;
{$ENDIF}

{$ENDIF}

  procedure AddMeshObjects(AInsert: TObject);
  var
    I: Integer;
    vMeshes: TsgObjectList;
  begin
    vMeshes := GetInsertMeshes(AInsert);
    try
      for I := 0 to vMeshes.Count - 1 do
      begin
        if PickObjects.Objects.IndexOf(vMeshes[I]) = -1 then
          PickObjects.Objects.Add(vMeshes[I]);
      end;
    finally
      vMeshes.Free;
    end;
  end;

  procedure AddMeshObjectsByEntityPath(AEntityPath: TsgObjEntity3D);
  var
    I: Integer;
    vMeshList: TsgObjectList;
    vNode: TNodeLite;
  begin
    vNode := TNodeLite(FRoot).GetNodeByStrKey(AEntityPath.PathKey);
    if Assigned(vNode) then
    begin
{$IFDEF SG_ADD_BOX}
{$IFDEF SG_ROTATE_CENTER_TEST}
      PickObjects.TopNode := vNode;
{$ENDIF}
      UpdateBoxDimensions(vNode);
{$ENDIF}
      vMeshList := TsgObjectList.Create;
      try
        vNode.GetMeshList(vMeshList);
        for I := 0 to vMeshList.Count - 1 do
        begin
          //if PickObjects.Objects.IndexOf(vMeshList[I]) = -1 then
            PickObjects.Objects.Add(vMeshList[I]);
        end;
      finally
        vMeshList.Free;
      end;
    end;
  end;

begin
  vEntitiesCount := ExtractArrayFromList(AEntities, Pointer(vEntities));
  if Assigned(PickObject) and (vEntitiesCount = 1) and
     (TsgMeshObject(PickObject).Insert = vEntities^[0]) then
     PickObjects.BuildLines
  else
  begin
    BeginUpdate;
    try
      if vEntitiesCount = 0 then
      begin
        PickObject := nil;
        PickObjects.Objects.Clear;
        PickObjects.ClearLines;
{$IFDEF SG_ADD_BOX}
{$IFDEF SG_ROTATE_CENTER_TEST}
        PickObjects.TopNode := nil;
{$ENDIF}
        UpdateBoxDimensions(nil);
{$ENDIF}
      end
      else
      begin
        if EnablePickObjects then
        begin
          //PickObjects.Objects.Clear;
          for I := 0 to vEntitiesCount - 1 do
          begin
            if TObject(vEntities^[I]) is TsgModEntity then
            begin
              AddMeshObjects(vEntities^[I]);;
            end
            else
            begin
              vEnt := TsgDXFEntity(vEntities^[I]);
              case vEnt.EntType of
                ceInsert: AddMeshObjects(vEnt);
                ceRegion, ceBody, ceSurface,  ce3DSolid,
                ceIges, ceStep, ceBrep, ceParasolid, ceInventor:
                  begin
                    for J := 0 to vEnt.Count - 1 do
                      if vEnt[J].EntType = ceInsert then
                        AddMeshObjects(vEnt[J]);
                  end;
              else
                begin
                    if vEnt is TsgObjEntity3D then
                    begin
                      AddMeshObjectsByEntityPath(TsgObjEntity3D(vEnt));
                    end
                    else
                    begin
                      vMesh := EntToGLObj(vEnt);
                      if vMesh <> nil then
                        PickObjects.Objects.Add(vMesh);
                    end;
                end;
              end;
            end;
          end;
          PickObjects.BuildLines;
        end;
      end;
{$IFDEF SG_ROTATE_CENTER_TEST}
      vAABB := FNav3D.FAABBBox;
      if Assigned(PickObjects.TopNode) then
      begin
        vBox := TCustomNode(PickObjects.TopNode).GetCurrentBox;
        if not IsBadRect(vBox) then
          vAABB := BoxToAABB(vBox);
      end
      else
      begin
        vBox := TCustomNode(FRoot).GetCurrentBox;
        if not IsBadRect(vBox) then
          vAABB := BoxToAABB(vBox);
      end;
      FNav3D.FCenter := VectorScale(VectorAdd(vAABB.Min, vAABB.Max), 0.5);
{$ENDIF}
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.SetShowBoxDimensions(const Value: Boolean);
begin
  if FShowBoxDimensions <> Value then
  begin
    FShowBoxDimensions := Value;
    if TCustomNode(FRoot).IsExploded then
      Navigator3D.FBoxLine.Visible := False
    else
      Navigator3D.FBoxLine.Visible := FShowBoxDimensions;
    Navigator3DAfterRender(nil);
    RepaintScene;
  end;
end;

procedure Tsg3DDrawingNavigator.SetShowEdges(const Value: Boolean);
begin
  if FShowEdges <> Value then
  begin
    FShowEdges := Value;
    VisibleScene;
    case FMeasureMode3D of
      mmSnap, mmSnapEdge, mmSnapBetweenEdge:;
      mmNone, mmHighLighting, mmOffAll:
        begin
          if Assigned(FNav3D) and Assigned(Navigator3D.FLineOuterWire) then
          begin
            Navigator3D.FLineOuterWire.Visible := FShowEdges;
            Navigator3DAfterRender(nil);
            RepaintScene;
          end;
        end;
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.SetShowHighlight(const Value: Boolean);
begin
  if FHighLightObj.Enabled <> Value then
  begin
    if FHighLightObj.Enabled then
      HighLightObject := nil;
    FHighLightObj.Enabled := Value;
    FPickObjects.Enabled := True;
  end;
end;

procedure Tsg3DDrawingNavigator.SetSnap(const Value: Boolean);
{$IFDEF ADD_POINT_CIRCLE}
  var
    I: Integer;
{$ENDIF}
  procedure SetVisiblePointToLine(ALine: TsgGLLines; AVisible: Boolean);
  begin
    ALine.ClearGLHandles;
    ALine.VisiblePoints := AVisible;
  end;
begin
  if (FSnapObj[0].Enabled <> Value) or FEdgeIntersect then
  begin
    MoveDim := nil;

    if FEdgeIntersect then
    begin
       FSnapObj[0].Color := Measure3DParams.EdgesHoverColor;
       FSnapObj[1].Color := Measure3DParams.EdgesHoverColor;
    end
    else
    begin
       FSnapObj[0].Color := Measure3DParams.EdgesColor;
       FSnapObj[1].Color := Measure3DParams.EdgesColor;
    end;

    if FSnapObj[0].Enabled then
    begin
      FSnapObj[0].Obj := nil;
      FSnapObj[1].Obj := nil;
    end;
    FSnapObj[0].Enabled := Value;
    FSnapObj[1].Enabled := Value;
    if FSnapObj[0].Enabled then
    begin
      FPickObjects.Obj := nil;
      FPickObjects.Enabled := False;
    end;
    if Assigned(FNav3D) then
    begin
      if Assigned(FNav3D.FLineMass) then
        SetVisiblePointToLine(FNav3D.FLineMass, FSnapObj[0].Enabled and (not FEdgeIntersect));
      if Assigned(FNav3D.FLineOuterWire) then
      begin
        SetVisiblePointToLine(FNav3D.FLineOuterWire, FSnapObj[0].Enabled and (not FEdgeIntersect));
        if (FSnapEdge.Enabled = True) or (IsShowEdges = False) then
          FNav3D.FLineOuterWire.Visible := FSnapObj[0].Enabled;
      end;
{$IFDEF ADD_POINT_CIRCLE}
      if Assigned(FNav3D.FDimensionList) then
        for I := 0 to FNav3D.FDimensionList.Count - 1 do
          SetVisiblePointToLine(TsgPickObject(FNav3D.FDimensionList.List[I]).FLines,
            FSnapObj[0].Enabled and (not FEdgeIntersect));
{$ENDIF}
      Navigator3DAfterRender(nil);
      RepaintScene;
    end;
  end;
end;

{$IFDEF USE_CLIP}
procedure Tsg3DDrawingNavigator.SetShowSnapPlan(const Value: Boolean);
  procedure SetVisiblePointToLine(ALine: TsgGLLines; AVisible: Boolean);
  begin
    ALine.ClearGLHandles;
    ALine.VisiblePoints := AVisible;
  end;
begin
  if FSnapPlan[0].Enabled <> Value then
  begin
    if FSnapPlan[0].Enabled then
    begin
      FSnapPlan[0].Obj := nil;
      FSnapPlan[1].Obj := nil;
      FSnapPlan[2].Obj := nil;
    end;
    FSnapPlan[0].Enabled := Value;
    FSnapPlan[1].Enabled := Value;
    FSnapPlan[2].Enabled := Value;
    if FSnapPlan[0].Enabled then
    begin
      FPickObjects.Obj := nil;
      FPickObjects.Enabled := False;
    end;
    if Assigned(FNav3D) then
    begin
      if Assigned(FNav3D.FLineMass) then
        SetVisiblePointToLine(FNav3D.FLineMass, FSnapPlan[0].Enabled);
      if Assigned(FNav3D.FLineOuterWire) then
      begin
        SetVisiblePointToLine(FNav3D.FLineOuterWire, FSnapPlan[0].Enabled);
        if (FSnapEdge.Enabled = True) or (IsShowEdges = False) then
          FNav3D.FLineOuterWire.Visible := FSnapPlan[0].Enabled;
      end;
      Navigator3DAfterRender(nil);
      RepaintScene;
    end;
  end;
end;
{$ENDIF}

{$IFDEF CLIP_CSG}

const
  USE_EPSILON_TEST = TRUE;
  EPSILON = 0.000001;

  // coplanar_tri_tri
  //
function Coplanar_tri_tri(const N, V0, V1, V2, U0, U1,
  U2: TAffineFLTVEctor): Integer;
var
  A: TAffineFLTVector;
  I0, I1: Shortint;

  function EDGE_AGAINST_TRI_EDGES(const V0, V1, U0, U1,
    U2: TAffineFLTVector): Integer;
  var
    Ax, Ay, Bx, By, Cx, Cy, E, D, F: Single;

    // * this edge to edge test is based on Franlin Antonio's gem:
    // "Faster Line Segment Intersection", in Graphics Gems III,
    // pp. 199-202 */
    function EDGE_EDGE_TEST(const V0, U0, U1: TAffineFLTVector): Integer;
    begin
      Result := 0;
      Bx := U0.V[I0] - U1.V[I0];
      By := U0.V[I1] - U1.V[I1];
      Cx := V0.V[I0] - U0.V[I0];
      Cy := V0.V[I1] - U0.V[I1];
      F := Ay * Bx - Ax * By;
      D := By * Cx - Bx * Cy;
      if ((F > 0) and (D >= 0) and (D <= F)) or
        ((F < 0) and (D <= 0) and (D >= F)) then
      begin
        E := Ax * Cy - Ay * Cx;
        if (F > 0) then
        begin
          if (E >= 0) and (E <= F) then
            Result := 1
        end
        else if (E <= 0) and (E >= F) then
          Result := 1;
      end;
    end;

  begin
    Ax := V1.V[I0] - V0.V[I0];
    Ay := V1.V[I1] - V0.V[I1];
    // * test edge U0,U1 against V0,V1 */
    Result := EDGE_EDGE_TEST(V0, U0, U1);
    if Result = 1 then
      Exit;
    // * test edge U1,U2 against V0,V1 */
    Result := EDGE_EDGE_TEST(V0, U1, U2);
    if Result = 1 then
      Exit;
    // * test edge U2,U1 against V0,V1 */
    Result := EDGE_EDGE_TEST(V0, U2, U0);
  end;

  function POINT_IN_TRI(const V0, U0, U1, U2: TAffineFLTVector): Integer;
  var
    A, B, C, D0, D1, D2: Single;
  begin
    Result := 0;
    // * is T1 completly inside T2? */
    // * check if V0 is inside tri(U0,U1,U2) */
    A := U1.V[I1] - U0.V[I1];
    B := -(U1.V[I0] - U0.V[I0]);
    C := -A * U0.V[I0] - B * U0.V[I1];
    D0 := A * V0.V[I0] + B * V0.V[I1] + C;

    A := U2.V[I1] - U1.V[I1];
    B := -(U2.V[I0] - U1.V[I0]);
    C := -A * U1.V[I0] - B * U1.V[I1];
    D1 := A * V0.V[I0] + B * V0.V[I1] + C;

    A := U0.V[I1] - U2.V[I1];
    B := -(U0.V[I0] - U2.V[I0]);
    C := -A * U2.V[I0] - B * U2.V[I1];
    D2 := A * V0.V[I0] + B * V0.V[I1] + C;
    if (D0 * D1 > 0.0) then
      if (D0 * D2 > 0.0) then
        Result := 1;
  end;

/// Begin Main logic ///////////////////////////////
begin
  // * first project onto an axis-aligned plane, that maximizes the area */
  // * of the triangles, compute indices: i0,i1. */
  A.V[0] := Abs(N.V[0]);
  A.V[1] := Abs(N.V[1]);
  A.V[2] := Abs(N.V[2]);
  if (A.V[0] > A.V[1]) then
  begin
    if (A.V[0] > A.V[2]) then
    begin
      I0 := 1; // * A[0] is greatest */
      I1 := 2;
    end
    else
    begin
      I0 := 0; // * A[2] is greatest */
      I1 := 1;
    end
  end
  else
  begin // * A[0]<=A[1] */
    if (A.V[2] > A.V[1]) then
    begin
      I0 := 0; // * A[2] is greatest */
      I1 := 1;
    end
    else
    begin
      I0 := 0; // * A[1] is greatest */
      I1 := 2;
    end
  end;

  // * test all edges of triangle 1 against the edges of triangle 2 */
  Result := EDGE_AGAINST_TRI_EDGES(V0, V1, U0, U1, U2);
  if Result = 1 then
    Exit;
  Result := EDGE_AGAINST_TRI_EDGES(V1, V2, U0, U1, U2);
  if Result = 1 then
    Exit;
  Result := EDGE_AGAINST_TRI_EDGES(V2, V0, U0, U1, U2);
  if Result = 1 then
    Exit;

  // * finally, test if tri1 is totally contained in tri2 or vice versa */
  Result := POINT_IN_TRI(V0, U0, U1, U2);
  if Result = 1 then
    Exit;
  Result := POINT_IN_TRI(U0, V0, V1, V2);
end;


function Tri_tri_intersect(const V0, V1, V2, U0, U1,
  U2: TAFFineFLTVector): Integer;
var
  E1, E2: TAffineFLTVector;
  N1, N2: TAffineFLTVector;
  D1, D2: Single;
  Du0, Du1, Du2, Dv0, Dv1, Dv2: Single;
  D: TAffineFLTVector;
  Isect1: array [0 .. 1] of Single;
  Isect2: array [0 .. 1] of Single;
  Du0du1, Du0du2, Dv0dv1, Dv0dv2: Single;
  Index: Shortint;
  Vp0, Vp1, Vp2: Single;
  Up0, Up1, Up2: Single;
  B, C, Max: Single;

  procedure ISECT(VV0, VV1, VV2, D0, D1, D2: Single;
    var Isect0, Isect1: Single);
  begin
    Isect0 := VV0 + (VV1 - VV0) * D0 / (D0 - D1);
    Isect1 := VV0 + (VV2 - VV0) * D0 / (D0 - D2);
  end;

  function COMPUTE_INTERVALS(VV0, VV1, VV2, D0, D1, D2, D0D1, D0D2: Single;
    var Isect0, Isect1: Single): Integer;
  begin
    Result := 0;
    if (D0D1 > 0.0) then
      // * here we know that D0D2<=0.0 */
      // * that is D0, D1 are on the same side, D2 on the other or on the plane */ \
      ISECT(VV2, VV0, VV1, D2, D0, D1, Isect0, Isect1)
    else if (D0D2 > 0.0) then
      // * here we know that d0d1<=0.0 */
      ISECT(VV1, VV0, VV2, D1, D0, D2, Isect0, Isect1)
    else if (D1 * D2 > 0.0) or (D0 <> 0.0) then
      // * here we know that d0d1<=0.0 or that D0!=0.0 */
      ISECT(VV0, VV1, VV2, D0, D1, D2, Isect0, Isect1)
    else if (D1 <> 0.0) then
      ISECT(VV1, VV0, VV2, D1, D0, D2, Isect0, Isect1)
    else if (D2 <> 0.0) then
      ISECT(VV2, VV0, VV1, D2, D0, D1, Isect0, Isect1)
    else
      // * triangles are coplanar */
      Result := Coplanar_tri_tri(N1, V0, V1, V2, U0, U1, U2);
  end;

// * sort so that a<=b */
  procedure SORT(var A: Single; var B: Single);
  var
    C: Single;
  begin
    if (A > B) then
    begin
      C := A;
      A := B;
      B := C;
    end;
  end;

begin
  // * compute plane equation of triangle(V0,V1,V2) */
  E1 := VectorSubtract(V1, V0);
  E2 := VectorSubtract(V2, V0);
  N1 := VectorCrossProduct(E1, E2);
  D1 := -VectorDotProduct(N1, V0);
  // * plane equation 1: N1.X+d1=0 */

  // * put U0,U1,U2 into plane equation 1 to compute signed distances to the plane*/
  Du0 := VectorDotProduct(N1, U0) + D1;
  Du1 := VectorDotProduct(N1, U1) + D1;
  Du2 := VectorDotProduct(N1, U2) + D1;

  // * coplanarity robustness check */
  if USE_EPSILON_TEST = TRUE then
  begin
    if (Abs(Du0) < EPSILON) then
      Du0 := 0.0;
    if (Abs(Du1) < EPSILON) then
      Du1 := 0.0;
    if (Abs(Du2) < EPSILON) then
      Du2 := 0.0;
  end;
  Du0du1 := Du0 * Du1;
  Du0du2 := Du0 * Du2;

  if (Du0du1 > 0.0) and (Du0du2 > 0.0) then
  begin // * same sign on all of them + not equal 0 ? */
    Result := 0; // * no intersection occurs */
    Exit;
  end;

  // * compute plane of triangle (U0,U1,U2) */
  E1 := VectorSubtract(U1, U0);
  E2 := VectorSubtract(U2, U0);
  N2 := VectorCrossProduct(E1, E2);
  D2 := -VectorDotProduct(N2, U0);
  // * plane equation 2: N2.X+d2=0 */

  // * put V0,V1,V2 into plane equation 2 */
  Dv0 := VectorDotProduct(N2, V0) + D2;
  Dv1 := VectorDotProduct(N2, V1) + D2;
  Dv2 := VectorDotProduct(N2, V2) + D2;

  if USE_EPSILON_TEST = TRUE then
  begin
    if (Abs(Dv0) < EPSILON) then
      Dv0 := 0.0;
    if (Abs(Dv1) < EPSILON) then
      Dv1 := 0.0;
    if (Abs(Dv2) < EPSILON) then
      Dv2 := 0.0;
  end;

  Dv0dv1 := Dv0 * Dv1;
  Dv0dv2 := Dv0 * Dv2;

  if (Dv0dv1 > 0.0) and (Dv0dv2 > 0.0) then
  begin // * same sign on all of them + not equal 0 ? */
    Result := 0; // * no intersection occurs */
    Exit;
  end;

  // * compute direction of intersection line */
  D := VectorCrossProduct(N1, N2);

  // * compute and index to the largest component of D */
  Max := Abs(D.V[0]);
  index := 0;
  B := Abs(D.V[1]);
  C := Abs(D.V[2]);
  if (B > Max) then
  begin
    Max := B;
    index := 1;
  end;
  if (C > Max) then
  begin
    // max:=c;   why?
    index := 2;
  end;
  // * this is the simplified projection onto L*/
  Vp0 := V0.V[index];
  Vp1 := V1.V[index];
  Vp2 := V2.V[index];

  Up0 := U0.V[index];
  Up1 := U1.V[index];
  Up2 := U2.V[index];

  // * compute interval for triangle 1 */
  COMPUTE_INTERVALS(Vp0, Vp1, Vp2, Dv0, Dv1, Dv2, Dv0dv1, Dv0dv2, Isect1[0],
    Isect1[1]);

  // * compute interval for triangle 2 */
  COMPUTE_INTERVALS(Up0, Up1, Up2, Du0, Du1, Du2, Du0du1, Du0du2, Isect2[0],
    Isect2[1]);

  SORT(Isect1[0], Isect1[1]);
  SORT(Isect2[0], Isect2[1]);

  if (Isect1[1] < Isect2[0]) or (Isect2[1] < Isect1[0]) then
    Result := 0
  else
    Result := 1;
end;
{$ENDIF}


procedure Tsg3DDrawingNavigator.SetSnapEdge(const Value: Boolean);
{$IFDEF CLIP_CSG}
//var
//  vPoint1, vPoint2, vPoint3, vPoint4: TVector3f;
//  vVector1, vVector2 : TVector3f;
//  vPlane: THmgPlane;
//  M: TMatrix4f;
//  vBoundingBox: THmgBoundingBox;

//  I: Integer;
//  vAABBBig, vAABBSmall: TAABB;
//  vTrianglesBig, vTrianglesSmall: TAffineVectorList;
//  vGLSphere: TGLSphere;
//  vColor: TGLColor;

//  vMesh: TMeshObject;
//  vVertexes3dFace: TFGVertexIndexList;
// vVect: TAffineVector;
//  vIndex: Integer;
//  vFreeForm,vFreeForm2, vResultFreeForm: TGLFreeForm;
//  Mesh : TMeshObject;

//  vLastOriginalMesh, vIndexAddMesh, vIndexResulMesh: Integer;

//  vList1, vList2: TAffineVectorList;
{$ENDIF}

{$IFDEF CLIP_CSG}
  function IsIntersectVL_VL(AList1, AList2: TAffineVectorList): Boolean;
  var
    I, J: Integer;
    vBreak: Boolean;
  begin
     Result := True;
     I := 0;
     J := 0;
     vBreak := True;
     while (I < AList1.Count) and vBreak do
     begin
       J := 0;
       while (J < AList2.Count) and vBreak do
       begin
         if Tri_tri_intersect(AList1.List^[I], AList1.List^[I+1], AList1.List^[I+2],
            AList2.List^[J], AList2.List^[J+1], AList2.List^[J+2]) = 1 then
           vBreak := False;
         Inc(J, 3);
       end;
       Inc(I, 3);
     end;
     Result := not vBreak;
  end;
{$ENDIF}

begin

{$IFDEF CLIP_CSG}
//  if FNav3D.FGLFreeForm.MeshObjects.Count <> 0 then
//  begin
//
//    vLastOriginalMesh := FNav3D.FGLFreeForm.MeshObjects.Count-1;
//
//    MakeVector(vAABBBig.Min, FNav3D.FBoxImage.Left, FNav3D.FBoxImage.Bottom, (FNav3D.FBoxImage.Z2 - FNav3D.FBoxImage.Z1)*0.5);
//    MakeVector(vAABBBig.Max, (FNav3D.FBoxImage.Right - FNav3D.FBoxImage.Left)*0.5, FNav3D.FBoxImage.Top, FNav3D.FBoxImage.Z1);
//
//    vAABBBig.Min.X := vAABBBig.Min.X - 10;
//    vAABBBig.Min.Y := vAABBBig.Min.Y - 1;
//
//    vAABBBig.Max.X := vAABBBig.Max.X + 10;
//    vAABBBig.Max.Z := vAABBBig.Max.Z + 1;
//
////    MakeVector(vAABBBig.Min, FNav3D.FBoxImage.Left, FNav3D.FBoxImage.Bottom, FNav3D.FBoxImage.Z2);
////    MakeVector(vAABBBig.Max, FNav3D.FBoxImage.Right, FNav3D.FBoxImage.Top, FNav3D.FBoxImage.Z1);
//
////    MakeVector(vAABBSmall.Min, FNav3D.FBoxImage.Left, FNav3D.FBoxImage.Bottom, (FNav3D.FBoxImage.Z2 - FNav3D.FBoxImage.Z1)*0.5);
////    MakeVector(vAABBSmall.Max, (FNav3D.FBoxImage.Right - FNav3D.FBoxImage.Left)*0.5, FNav3D.FBoxImage.Top, FNav3D.FBoxImage.Z1);
//
//    M := FNav3D.GLDCScene.Matrix;
//
//
//    vMesh := CreateMeshObject(FNav3D.GLFreeForm.MeshObjects, nil);
//
//    vIndexAddMesh := FNav3D.FGLFreeForm.MeshObjects.Count-1;
//
//    vVertexes3dFace := GetFaceGroup(vMesh, fgmmTriangles,
//      clRed, TFGVertexIndexList);
//
//    vTrianglesSmall := TAffineVectorList.Create;
//    try
//      AddCubeByAABB(vAABBBig,  vTrianglesSmall);
//
////      for I := 0 to vTrianglesSmall.Count - 1 do
////        vTrianglesSmall.List^[I] := VectorTransform(vTrianglesSmall.List^[I], M);
//
//      for I := 0 to vTrianglesSmall.Count - 1 do
//      begin
//        //vVect := GetPointf(Vect2FPoint(vTrianglesSmall.List^[I]));
//        vVect := vTrianglesSmall.List^[I];
//        vIndex := TMeshObject(vMesh).Vertices.Add(vVect);
//        TFGVertexIndexList(vVertexes3dFace).Add(vIndex);
//
//      end;
//    finally
//      vTrianglesSmall.Free;
//    end;
//
//    for I := 0 to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
//    begin
//        if FNav3D.FGLFreeForm.MeshObjects[I].Normals.Count = 0 then
//          GenNormals(FNav3D.FGLFreeForm.MeshObjects[I]);
//    end;
//
//
//    for I := 0 to vLastOriginalMesh do
//    begin
//
////      vList1 := FNav3D.FGLFreeForm.MeshObjects.Items[I].ExtractTriangles;
////      vList2 := FNav3D.FGLFreeForm.MeshObjects.Items[vIndexAddMesh].ExtractTriangles;
////
////      if not  IsIntersectVL_VL(vList1, vList2) then
////      begin
////        sgNop;
////        Continue;
////      end;
//
//      vMesh := CreateMeshObject(FNav3D.GLFreeForm.MeshObjects, nil);
////      vVertexes3dFace := GetFaceGroup(vMesh, fgmmTriangles,
////        clRed, TFGVertexIndexList);
//    // CSG_Subtraction   CSG_Intersection  CSG_Union
//      CSG_Operation(FNav3D.FGLFreeForm.MeshObjects.Items[vIndexAddMesh], FNav3D.FGLFreeForm.MeshObjects.Items[I],
//        CSG_Intersection,vMesh,vVertexes3dFace.MaterialName,vVertexes3dFace.MaterialName);
//    end;
//
//    FNav3D.FGLFreeForm.StructureChanged;
//
//    for I := 0 to vIndexAddMesh do
//    begin
//      FNav3D.FGLFreeForm.MeshObjects.Items[I].Visible := False;
//    end;
//
////
////    for I := vIndexAddMesh+1 to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
////    begin
////      if I <> vIndexAddMesh+49 then
////        FNav3D.FGLFreeForm.MeshObjects.Items[I].Visible := False;
////    end;
//
//    Exit;
//  end;
{$ENDIF}

  if FSnapEdge.Enabled <> Value then
  begin
    MoveDim := nil;
    if FSnapEdge.Enabled then
      FSnapEdge.Obj := nil;
    FSnapEdge.Enabled := Value;
    if FSnapEdge.Enabled then
    begin
      FPickObjects.Obj := nil;
      FPickObjects.Enabled := False;
    end;
    if Assigned(FNav3D) and Assigned(FNav3D.FLineOuterWire) then
    begin
      if (FSnapEdge.Enabled = True) or (IsShowEdges = False) then
      begin
        FNav3D.FLineOuterWire.ClearGLHandles;
        FNav3D.FLineOuterWire.Visible := FSnapEdge.Enabled;
        Navigator3DAfterRender(nil);
        RepaintScene;
      end;
    end;
  end;
end;

function Tsg3DDrawingNavigator.VisibleScene: Boolean;
var
  I, vStartIterate: Integer;
begin
  if (FShowingStyle = []) or  ((FShowingStyle - [ssLighting]) = []) then
    Result := False
  else
    Result := True;
  //if (FVisibleScene <> Result) then
  //begin
    FVisibleScene := Result;
    if Assigned(FNav3D) and Assigned(Navigator3D.FLineOuterWire) then
    begin
      vStartIterate := 0;
{$IFDEF CLIP_CSG_CLIP_GLMESH}
      if FNav3D.IsClipPlaneUses then
        vStartIterate := FNav3D.FLastOriginalMesh + 2;
{$ENDIF}
      for I := vStartIterate to FNav3D.FGLFreeForm.MeshObjects.Count - 1 do
        FNav3D.FGLFreeForm.MeshObjects[I].Visible := FVisibleScene;
      for I := FNav3D.FGLFreeForm.Count - 1 downto 1 do
        FNav3D.FGLFreeForm.Children[I].Visible := FVisibleScene;
{$IFDEF CLIP_CSG_GLMESH_SGMESH }
      if Assigned(FNav3D.FLineOuterWireOld) then
        FNav3D.FLineOuterWireOld.Visible := False;
      if Assigned(FNav3D.FLineMassOld) then
        FNav3D.FLineMassOld.Visible := False;
{$ENDIF}
      if Assigned(FNav3D.FLineOuterWire) then
        FNav3D.FLineOuterWire.Visible := FShowEdges;
      if Assigned(FNav3D.FGLCubeExtents) then
        FNav3D.FGLCubeExtents.Visible := FNav3D.ShowCubeExtents;
      if Assigned(FNav3D.FBoxLine) then
      begin
        if Assigned(FRoot) and TCustomNode(FRoot).IsExploded then
          FNav3D.FBoxLine.Visible := False
        else
          FNav3D.FBoxLine.Visible := FShowBoxDimensions;
      end;
      Navigator3DAfterRender(nil);
      RepaintScene;
    //end;
  end;
end;

procedure Tsg3DDrawingNavigator.SetShowingStyle(const Value: Tsg3DShowingStyles);
var
  I: Integer;
  vContextOptions: TContextOptions;
begin
  if FNav3D = nil then Exit;
  FShowingStyle := Value;
  vContextOptions := FNav3D.Buffer.ContextOptions;
{$IFDEF CLIP_N_GLCLIP_N_GLCSG}
  if ssOutlineShader in FShowingStyle then
    FNav3D.Buffer.ContextOptions := FNav3D.Buffer.ContextOptions + [roStencilBuffer]
  else
    FNav3D.Buffer.ContextOptions := FNav3D.Buffer.ContextOptions - [roStencilBuffer];
{$ENDIF}

  begin
    for I := 0 to FNav3D.FGLMaterialLibrary.Materials.Count - 1 do
    begin
      ApplyShowingStyleToMaterial(FNav3D.FGLMaterialLibrary.Materials[I].Material);
      if ssOutlineShader in FShowingStyle then
      begin
        TsgOutlineStencilShader(FOutlineShader).BackGroundColor.AsWinColor := FNav3D.BackgroundColor;
        FNav3D.FGLMaterialLibrary.Materials[I].Shader :=  FOutlineShader;
      end
      else
      if ssHiddenLines in FShowingStyle then
      begin
        if ssFlatShading in FShowingStyle then
          TGLHiddenLineShader(FHiddenLinesShader).ShadeModel := smFlat;
        if ssSmoothShading in FShowingStyle then
          TGLHiddenLineShader(FHiddenLinesShader).ShadeModel := smSmooth;
        if (ssSmoothShading in FShowingStyle) or (ssFlatShading in FShowingStyle) then
          TGLHiddenLineShader(FHiddenLinesShader).Solid := True
        else
          TGLHiddenLineShader(FHiddenLinesShader).Solid := False;
        FNav3D.FGLMaterialLibrary.Materials[I].Shader := FHiddenLinesShader;
      end
      else
        FNav3D.FGLMaterialLibrary.Materials[I].Shader := nil;
    end;
  end;
  UpdateMaterialLibrary;
  if FOwnDC <> 0 then
    if vContextOptions <> FNav3D.Buffer.ContextOptions then
      RecreateWnd{$IFDEF SGFPC}(Self){$ENDIF}
    else
    begin
      FNav3D.Buffer.DestroyRC;
      FNav3D.ForceRenderingContext;
    end;

  VisibleScene;
  FNav3D.GLFreeForm.StructureChanged;
  Repaint;
end;

procedure Tsg3DDrawingNavigator.SetupLight(const ABox: TFRect);
var
  vCenter: TVector3f;
  vOffs: Single;
begin
  vCenter := VectorScale(VectorAdd(FNav3D.FAABBBox.Min, FNav3D.FAABBBox.Max), 0.5);
  vCenter := VectorTransform(vCenter, FNav3D.GLDCScene.Matrix);
  vOffs := 3 * FNav3D.GLDCScene.BoundingSphereRadius;
  FNav3D.PositionLight(vCenter, vOffs);
end;

procedure Tsg3DDrawingNavigator.SetUserHighLight(const Value: Boolean);
begin
  FUserHighLight := Value;
  if MeasureMode3D = mmNone then
    ShowHighlight := Value;
end;

procedure Tsg3DDrawingNavigator.DropVisualization(const AData: TObject);
begin
  if Assigned(AData) and (AData is TsgCollection) then
    TsgCollection(AData).Free;
end;

procedure Tsg3DDrawingNavigator.SetVisualization(
  const AEntity: TObject; const AData: Pointer; const APath: String);
var
  vCollection: TsgCollection;
  vKey: UInt64;
  vType: Byte;
begin
  vType := 0;
  vCollection := nil;
  if AEntity is TsgModVisibleEntity then
  begin
    vType := 1;
    vCollection := TsgCollection(TsgModVisibleEntity(AEntity).Visualization)
  end
  else
  begin
    if AEntity is TsgDXFInsert then
    begin
      vType := 2;
      vCollection := TsgCollection(TsgDXFInsertAccess(AEntity).Visualization);
    end;
  end;
  if vType > 0 then
  begin
    if not Assigned(vCollection) then
    begin
      vCollection := sgLists.TsgCollection.Create;
      case vType of
        2: TsgDXFInsertAccess(AEntity).Visualization := vCollection;
      else
        TsgModVisibleEntity(AEntity).Visualization := vCollection;
      end;
    end;

//    OutputDebugString(PChar(Format('PathKey %s', [APath])));

    vKey := sgComparer.GetHashCodeStr(APath);
    vCollection.Add(vKey, AData);
  end;
end;

function Tsg3DDrawingNavigator.ShowRectConvertUnits(ARect: TFRect;
  const AConvertUnits: Boolean): Boolean;
var
  v2DRect: TF2DRect;
begin
  Result := False;
  if Assigned(LoadFromImage) then
  begin
    if AConvertUnits then
    begin
      ARect.TopLeft := ControlTool.ConvertUnits(ARect.TopLeft);
      ARect.BottomRight := ControlTool.ConvertUnits(ARect.BottomRight);
    end;
  end;
  Result := ZoomRectEx(ARect, True);
end;

procedure Tsg3DDrawingNavigator.Translate(const AVector: TAffineVector);
begin
  FNav3D.Translate(AVector);
end;

procedure Tsg3DDrawingNavigator.DeleteEnts(AEntities: TObject);
var
  I, J, C: Integer;
  vObj: TObject;
  P: PsgObjectArray;
begin
  BeginUpdate;
  try
    C := ExtractArrayFromList(AEntities, Pointer(P));
    for I := 0 to C - 1 do
    begin
      vObj := EntToGLObj(P^[I], @J);
      if Assigned(vObj) then
      begin
        Navigator3D.GLFreeForm.MeshObjects.Delete(J);
        vObj.Free;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure Tsg3DDrawingNavigator.DeleteGLPolyline(AObject: TObject);
begin
  FNav3D.DeleteGLPolyline(AObject);
end;

function Tsg3DDrawingNavigator.GetCADCoords(const APoint: TPoint): TFPoint;
begin
  Result := GetCADCoords(MakeFPointFromPoint(APoint));
end;

function Tsg3DDrawingNavigator.GetCADCoords(const APoint: TFPoint): TFPoint;
var
  vRez: TAffineVector;
begin
  vRez := ClientToWorld(APoint);
  Result := Vect2FPoint(vRez);
end;

function Tsg3DDrawingNavigator.GetScreenCoords(const APoint: TFPoint): TFPoint;
begin
  Result := GetScreenCoords(FPoint2Vect(APoint));
end;

function Tsg3DDrawingNavigator.GetScreenCoords(const APoint: TAffineVector): TFPoint;
var
  vRez: TAffineVector;
  vTranslate: TAffineVector;
begin
  vTranslate := VectorTransform(APoint, FNav3D.GLDCScene.Matrix);
  vRez := FNav3D.Buffer.WorldToScreen(vTranslate);
  Result := Vect2FPoint(vRez);
end;

{$IFNDEF SGFPC}
procedure Tsg3DDrawingNavigator.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPSIBLINGS;
    WindowClass.Style := WindowClass.Style or CS_OWNDC;
  end;
end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.DestroyWnd;
begin
  DestroyRC;
  inherited DestroyWnd;
end;

procedure Tsg3DDrawingNavigator.Loaded;
begin
  inherited Loaded;
  HandleNeeded;
end;

procedure Tsg3DDrawingNavigator.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  p: TPoint;
begin
  if Assigned(FNav3D) then
  begin

{$IFDEF SG_ROTATE_CENTER_TEST}
    if FIsRequestedDrawMatrix then
    begin
      LoadView(FDrawMatrixReload, False);
    end;
    FIsRequestedDrawMatrix := False;
{$ENDIF}

    FNav3D.Painting;
    try
      FNav3D.RenderAxis;
      p := ClientToScreen(Point(0, 0));
      if (FLastScreenPos.X <> p.X) or (FLastScreenPos.Y <> p.Y) then
      begin
        // Workaround for MS OpenGL "black borders" bug
        if FNav3D.Buffer.RCInstantiated then
          PostMessage(Handle, WM_SIZE, SIZE_RESTORED, Width + (Height shl 16));
        FLastScreenPos := p;
      end;
      if Message.DC <> FOwnDC then
        BeginPaint(Handle, PS);
      try
        if FNav3D.IsRenderingContextAvailable and (Width > 0) and (Height > 0) then
          FNav3D.Render(nil);
      finally
        if Message.DC <> FOwnDC then
          EndPaint(Handle, PS);
        Message.Result := 0;
      end;
    finally
      FNav3D.Painted;
    end;
  end;
end;

procedure Tsg3DDrawingNavigator.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if FNav3D.IsRenderingContextAvailable then
    Message.Result := 1
  else
    inherited;
end;

procedure Tsg3DDrawingNavigator.WMSize(var Message: TMessage);
var
  vMsg: TMsg;
begin
  inherited;
  if TWMSize(Message).Width * TWMSize(Message).Height <> 0 then
  begin
    if (FNav3D.Width <> TWMSize(Message).Width) or (FNav3D.Height <> TWMSize(Message).Height) then
    begin
      FNav3D.Buffer.BeginUpdate;
      try
        FNav3D.Width := TWMSize(Message).Width;
        FNav3D.Height := TWMSize(Message).Height;
        FNav3D.Buffer.Resize(0, 0, FNav3D.Width, FNav3D.Height);
      finally
        FNav3D.Buffer.EndUpdate;
      end;
    end;
    if FNav3D.Buffer.RenderingContext = nil then
      FNav3D.DoCreateRC;
    FNav3D.SetAxisPos;
    if {$IFDEF SGDEL_XE2}(csAligning in ControlState) and{$ENDIF} HandleAllocated then
      PeekMessage(vMsg, WindowHandle, WM_SIZE, WM_SIZE, PM_REMOVE); //??
  end;
end;

{$IFDEF MSWINDOWS}
procedure Tsg3DDrawingNavigator.WMDestroy(var Message: TWMDestroy);
begin
  DestroyRC;
  inherited;
end;
{$ENDIF}

procedure Tsg3DDrawingNavigator.DestroyRC;
begin
  if Assigned(FNav3D.Buffer) then
    FNav3D.Buffer.DestroyRC;
  if FOwnDC <> 0 then
  begin
    if WindowHandle <> 0 then
      ReleaseDC(WindowHandle, FOwnDC);
    FOwnDC := 0;
  end;
end;

{ Tsg3DDrwNavOrbit3D }

procedure Tsg3DDrwNavOrbit3D.PaintWindow(AContextInfo: sgConsts.THandle);

  {$IFNDEF MSWINDOWS}
  function GetObjectType(const AContext: sgConsts.THandle): Integer;
  begin
    Result := OBJ_BITMAP;
  end;
  {$ENDIF}

var
  I: Integer;
  vGLCanvas: TGLCanvas;
begin
  if GetObjectType(AContextInfo) in [OBJ_DC, OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC] then Exit;
  vGLCanvas := TGLCanvas.Create(PRenderContextInfo(AContextInfo)^.viewPortSize.cx,
    PRenderContextInfo(AContextInfo)^.viewPortSize.cy);
  try
    vGLCanvas.PenColor := Color;
    vGLCanvas.Ellipse(Center.X, Center.Y, BigRadius, BigRadius);
    for I := 0 to 3 do
      vGLCanvas.Ellipse(SmallCenters[I].X, SmallCenters[I].Y, SmallRadius);
  finally
    vGLCanvas.Free;
  end;
end;

{ Tsg3DAxis }

constructor Tsg3DAxis.CreateEx(AGLScene: TGLScene; AOwner: TComponent);

  procedure SetArray(const A1: array of Single; A2: TGLCoordinates;
    ADoAsPoint: Boolean = False; Arrow: TGLArrowLine = nil);
  begin
    if ADoAsPoint then
      A2.SetPoint(A1[0], A1[1], A1[2])
    else
      A2.SetVector(A1[0], A1[1], A1[2]);
  end;

  procedure CreareArrow(var Arrow: TGLArrowLine; const ADirection,
    APosition, AUp, AColor: array of Single);
  begin
    Arrow := TGLArrowLine.CreateAsChild(FGLDCAxes);
    Arrow.BottomArrowHeadHeight := 0.5;
    Arrow.BottomArrowHeadRadius := 0.2;
    Arrow.BottomRadius := 0.1;
    SetDefDirForObject(Arrow);
    SetArray(ADirection, Arrow.Direction);
    SetArray(APosition, Arrow.Position, True);
    SetArray(AUp, Arrow.Up);
    Arrow.Height := cntDefAxisHeight;
    Arrow.Scale.SetVector(1, 1, 1);
    Arrow.TopArrowHeadHeight := 0.5;
    Arrow.TopArrowHeadRadius := 0.2;
    Arrow.TopRadius := 0.1;
    SetColorForMaterialNB(Arrow.Material, VectorMake(AColor[0], AColor[1], AColor[2], 1));
    //Arrow.Material.Texture.Border := 0;
  end;

begin
  FGLScene := AGLScene;
  FDefCameraScale := cntDefPerspAxisCameraScale;

  inherited Create(AOwner);
  FGLDCAxes := TGLDummyCube.CreateAsChild(FGLScene.Objects);

  FGLDCAxes.Position.AsAffineVector := cntDefAxisPos;
  SetDefDirForObject(FGLDCAxes);

  CreareArrow(FGLArrowX, [1, 0, 0], [0.3, 0, 0], [0, 0, -1], [1, 0.133, 0.153, 1]);//$2722FF
  CreareArrow(FGLArrowY, [0, 1, 0], [0, 0.3, 0], [0, 0, -1], [0.133, 1, 0.133, 1]);//$22FF22
  CreareArrow(FGLArrowZ, [0, 0, 1], [0, 0, 0.3], [0, 1, 0], [0.133, 0.282, 1, 1]);//$FF4822

  CreateCamera(FGLScene.Objects, FGLDCAxes, FGLCamera, 1);
  Camera := FGLCamera;
  SetDefPositions;
  SetDefDistanceForCamera(FGLCamera, BoundingSphereRadius, FDefCameraScale);
  Buffer.ContextOptions := [roDoubleBuffer, roDestinationAlpha];
  Buffer.AmbientColor.Color := clrTransparent;
  Buffer.BackgroundColor := clWhite;
  Buffer.BackgroundAlpha := 0;
//  Buffer.Resize(10, 10,
//    cntAxisImageWidth, cntAxisImageHeight);
end;

function Tsg3DAxis.GetBoundingSphereRadius: Single;
begin
  Result := FGLArrowX.BoundingSphereRadius * 5;
end;

procedure Tsg3DAxis.HideAxis;
begin
  FGLDCAxes.Visible := False;
end;

procedure Tsg3DAxis.LoadIdentity;
begin
  FGLDCAxes.Matrix := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.IdentityHmgMatrix;
  SetDefDistanceForCamera(Camera, BoundingSphereRadius, FDefCameraScale);
end;

procedure Tsg3DAxis.ResetRotations;
begin
  FGLDCAxes.ResetRotations;
  Camera.Position := FGLDCAxes.Position;
  Camera.Move(2 * BoundingSphereRadius);
  Camera.RotateObject(FGLDCAxes, 0, 180, 0);
end;

procedure Tsg3DAxis.SetCameraPosition(X, Y, Z: Single);
const
  cntKoef = 0.5;
begin
  Camera.Position.SetPoint(X, Y, Z);
  //FGLLightSource.Position.SetPoint(X * cntKoef , Y * cntKoef, Z * cntKoef);
  //FGLLightSource.Position.AsAffineVector := NullVector;
end;

procedure Tsg3DAxis.SetDefPositions;
begin
  FGLDCAxes.Position.AsAffineVector := cntDefAxisPos;
  SetCameraPosition(cntDefCameraPos.V[0], cntDefCameraPos.V[1], cntDefCameraPos.V[2]);
end;

procedure Tsg3DAxis.ShowAxis;
begin
  FGLDCAxes.Visible := True;
  //FGLLightSource.Visible := True;
end;

procedure Tsg3DAxis.SetObjectsVisible(AVisible: Boolean);
var
  I: Integer;
begin
  FGLDCAxes.Visible := AVisible;
  for I := 0 to FGLDCAxes.Count - 1 do
    FGLDCAxes.Children[I].Visible := AVisible;
end;

{ TsgGLGraphic }

procedure TsgGLGraphic.AssignGLProperties(const AGraphic: TsgGLGraphic);
begin
  FNavigator := AGraphic.FNavigator;
  FViewport := AGraphic.FViewport;
  FBuffer := AGraphic.FBuffer;
  FChangedCount:= AGraphic.FChangedCount;
  FPrinting := AGraphic.FPrinting;
end;

procedure TsgGLGraphic.Changed(Sender: TObject);
begin
  inherited Changed(Sender);
  Inc(FChangedCount);
end;

procedure TsgGLGraphic.DoRender;
var
  vShowAsix, vOrbit3DVisible: Boolean;
  vBackGround: TColor;
  vAlign: TAlign;
  vBounds: TRect;
  vBitmap: TBitmap;
  vDrawMatrix: TFMatrix;
//  vIsVisibleDimensionList: Boolean;
  vTmpViewport: TRectangle;

  procedure GLSceneBufferRenderToBitmap;
  var
    vNewView: TFMatrix;
    vBoundingBox, vBox, vRect: TFRect;
    vOldScale: Double;
    vDrawRect, vTmpDrawRect: TsgDrawRectAccess;
  begin
    FBuffer.BeginUpdate;
    FNavigator.Navigator3D.GLDCScene.BeginUpdate;
    FBuffer.Camera.BeginUpdate;
    vTmpDrawRect := TsgDrawRectAccess(TsgDrawRect.Create);
    vOldScale := FNavigator.FNav3D.ScaleByPrint;
    try
      vNewView := vDrawMatrix;
      vBoundingBox := FNavigator.GetBox;
      if TCustomNode(FNavigator.FRoot).IsExploded then
      begin
        vBoundingBox := TRoot(FNavigator.FRoot).GetBoxByExploded;
      end;
      vBox := vBoundingBox;
      OffsetFRect(vBox, -FNavigator.FBoxOffs.X, -FNavigator.FBoxOffs.Y, -FNavigator.FBoxOffs.Z);
      vTmpDrawRect.Matrix^ := vDrawMatrix;
      vTmpDrawRect.Box := vBox;

      if (FViewport.Width = 0) and (FViewport.Height = 0) then
      begin
        if FNavigator.ViewRectMode then
        begin
          vRect := vTmpDrawRect.FRect;
          FViewport.Left := Round(Int(vRect.Left));
          FViewport.Top := Round(Int(vRect.Bottom));
          FViewport.Width := Round(vRect.Right - vRect.Left);
          FViewport.Height := Round(vRect.Top - vRect.Bottom);
        end
        else
        begin
          FViewport.Left := 0;
          FViewport.Top := 0;
          FViewport.Width := Width;
          FViewport.Height := Height;
          vTmpDrawRect.FitTo(vTmpViewport.Left, vTmpViewport.Top, vTmpViewport.Width, vTmpViewport.Height);
        end;
      end
      else
        vTmpDrawRect.FitTo(vTmpViewport.Left, vTmpViewport.Top, vTmpViewport.Width, vTmpViewport.Height);

      vDrawRect := TsgDrawRectAccess(TsgDrawRect.Create);
      try
        vDrawRect.Box := vBox;
        vDrawRect.AttachMatrix(@vNewView);
        vDrawRect.FitTo(FViewport.Left, FViewport.Top, FViewport.Width, FViewport.Height);
        FNavigator.FNav3D.ScaleByPrint := Power(Abs(vDrawRect.Determinant3D/vTmpDrawRect.Determinant3D), 1/3);
        try
          FBuffer.Resize(0, 0, Width, Height);
          FNavigator.LoadView(vNewView);
          vBitmap := FNavigator.CreateSnapShotBitmap;
          try
            Canvas.Draw(0, 0, vBitmap);
          finally
            vBitmap.Free;
          end;
        finally
          FBuffer.Resize(vTmpViewport.Left, vTmpViewport.Top,
            vTmpViewport.Width, vTmpViewport.Height);
        end;
      finally
        vDrawRect.Free;
      end;
    finally
      FNavigator.FNav3D.ScaleByPrint := vOldScale;
      FBuffer.Camera.EndUpdate;
      FNavigator.Navigator3D.GLDCScene.EndUpdate;
      FBuffer.EndUpdate;
      vTmpDrawRect.Free;
    end;
  end;

begin
  if Assigned(FNavigator) and Assigned(FBuffer) then
  begin
    vAlign := FNavigator.Align;
    vBounds := FNavigator.BoundsRect;
    //FNavigator.BeginUpdate;
    vTmpViewport := FBuffer.ViewPort;
    FNavigator.GetView(vDrawMatrix);
    vBackGround := FNavigator.Navigator3D.BackgroundColor;
    try
      FNavigator.Align := alNone;
      FNavigator.SetBounds(0, 0, Width, Height);
      if FPrinting then
      begin
        FNavigator.DoOnColorChanged(clWhite);
        FNavigator.UpdateBoxDimensions(nil);
      end;
      Transparent := not FPrinting;
      TransparentColor := FNavigator.Navigator3D.BackgroundColor;
      vShowAsix := FNavigator.Navigator3D.ShowAxis;
      vOrbit3DVisible := FNavigator.Orbit3D.Visible;
      //vIsVisibleDimensionList := FNavigator.Navigator3D.IsVisibleDimensionList;
      //FNavigator.Navigator3D.IsVisibleDimensionList := False;
      FNavigator.FLockPaintSysMenuIcons := True;
      try
        FNavigator.Navigator3D.ShowAxis := False;
        FNavigator.Orbit3D.Visible := False;
        GLSceneBufferRenderToBitmap;
        FChangedCount := 0;
      finally
        FNavigator.Navigator3D.ShowAxis := vShowAsix;
        FNavigator.Orbit3D.Visible := vOrbit3DVisible;
        FNavigator.FLockPaintSysMenuIcons := False;
        //FNavigator.Navigator3D.IsVisibleDimensionList := vIsVisibleDimensionList;
      end;
    finally
      FNavigator.Navigator3D.BackgroundColor := vBackGround;
      FNavigator.BoundsRect := vBounds;
      FNavigator.Align := vAlign;
      FNavigator.LoadView(vDrawMatrix);
      //FNavigator.EndUpdate;
    end;
  end;
end;

procedure TsgGLGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if (FChangedCount > 0) or (ACanvas = Canvas) then
  begin
    DoRender;
  end;
  if ACanvas <> Canvas then
    inherited Draw(ACanvas, Rect);
end;

procedure TsgGLGraphic.DrawTransparent(ACanvas: TCanvas; const Rect: TRect;
  Opacity: Byte);
begin
{$IFDEF SGDEL_2009}
  if (FChangedCount > 0) or (ACanvas = Canvas) then
  begin
    DoRender;
  end;
  if ACanvas <> Canvas then
    inherited DrawTransparent(ACanvas, Rect, Opacity);
{$ELSE}
  Draw(ACanvas, Rect);
{$ENDIF};
end;

procedure TsgGLGraphic.SetNavigator(const ANavigator: Tsg3DDrawingNavigator);
begin
  if FNavigator <> ANavigator then
  begin
    FNavigator := ANavigator;
    if Assigned(FNavigator) and Assigned(FNavigator.Navigator3D) then
    begin
      FBuffer := FNavigator.Navigator3D.Buffer;
      SetSizeGraphic(Self, FBuffer.Width, FBuffer.Height);
    end
    else
      FBuffer := nil;
  end;
end;

procedure TsgGLGraphic.UpdateViewport;
begin
  FViewport := TRectangle(Rect(0, 0, Self.Width, Self.Height));
end;

{ TsgVertexList }

procedure TsgVertexList.BuildList(var mrci: TRenderContextInfo);
begin
  TsgMeshObject(Owner.Owner).DeclareArraysToOpenGL(mrci, False);
  glDrawElements(FMode1, VertexIndices.Count, GL_UNSIGNED_INT, VertexIndices.List);
end;

constructor TsgVertexList.Create;
begin
  inherited Create;
  FMode1 := GL_LINES;
  FMode2 := MAXDWORD;
end;

{ TsgHiddenLineShader }

procedure TsgHiddenLineShader.DoApply(var rci: TRenderContextInfo;
  Sender: TObject);
begin
  FPassCount := 1;
  rci.GLStates.PolygonMode := pmFill;
  //rci.GLStates.SetGLPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  //rci.GLStates.ResetGLPolygonMode;
  glPushAttrib(GL_CURRENT_BIT or GL_ENABLE_BIT);
  glColor3fv(@FBackgroundColor);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(1, 2);
end;

function TsgHiddenLineShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
   case FPassCount of
     1:
       begin
         FPassCount := 2;
         //rci.GLStates.SetGLPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
         rci.GLStates.PolygonMode := pmLines;
         glPopAttrib;
         Result := True;
       end;
    else
      Result := False;
   end;
end;

{TsgOutlineStencilShader}

constructor TsgOutlineStencilShader.Create(AOwner: TComponent);
begin
  inherited;
  FOutLineWidth := 2;
  FLineColor := TGLColor.CreateInitialized(Self, clrBlack);
  FBackgroundColor := TGLColor.CreateInitialized(Self, clrBtnFace);
  // no material
  ShaderStyle := ssReplace;
end;

destructor TsgOutlineStencilShader.Destroy;
begin
  FBackgroundColor.Free;
  FLineColor.Free;
  inherited;
end;

procedure TsgOutlineStencilShader.DoApply(var rci: TRenderContextInfo; Sender:
  TObject);
begin
  GL.PushAttrib(GL_ALL_ATTRIB_BITS);
  GL.Disable(GL_LIGHTING);
  GL.ClearStencil(0);
  GL.Clear(GL_STENCIL_BUFFER_BIT);
  GL.Enable(GL_STENCIL_TEST);
  GL.StencilFunc(GL_ALWAYS, 1, $FFFF);
  GL.StencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
  GL.PolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  GL.Color4fv(FBackgroundColor.AsAddress);
  FPassCount := 1;
end;

function TsgOutlineStencilShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  case FPassCount of
    1:
    begin
      GL.Disable(GL_LIGHTING);
      GL.StencilFunc(GL_NOTEQUAL, 1, $FFFF);
      GL.StencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
      GL.LineWidth(FOutlineWidth);
      GL.PolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      GL.Color3fv(FLineColor.AsAddress);
      FPassCount := 2;
      Result := True;
    end;
    2:
    begin
      GL.PopAttrib();
      Result := False;
    end;
  else
    Assert(False);
    Result := False;
  end;
end;

procedure TsgOutlineStencilShader.SetOutlineWidth(AWidth: single);
begin
  if FOutlineWidth <> AWidth then
  begin
    FOutlineWidth := AWidth;
    NotifyChange(Self);
  end;
end;

{ TsgGreyShader }

procedure TsgGreyShader.ApplyColors(const AColor: TColorVector);
begin
  QSetColor(FGLLibMaterial.Material.FrontProperties.Diffuse, AColor);
  QSetColor(FGLLibMaterial.Material.FrontProperties.Specular, AColor);
  QSetColor(FGLLibMaterial.Material.BackProperties.Diffuse, AColor);
  QSetColor(FGLLibMaterial.Material.BackProperties.Specular, AColor);
end;

constructor TsgGreyShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShaderStyle := ssHighLevel;
end;

procedure TsgGreyShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin
  FGLLibMaterial := TGLLibMaterial(Sender);
  FColor := FGLLibMaterial.Material.FrontProperties.Diffuse.Color;
  ApplyColors(ColorVectorToIllumination(FColor));
  FPassCount := 1;
end;

function TsgGreyShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  if FPassCount = 1 then
  begin
    FPassCount := 2;
    ApplyColors(FColor);
    Result := True;
  end
  else
    Result := False;
end;

procedure TsgGreyShader.QSetColor(AGLColor: TGLColor;
  const AColor: TColorVector);
begin
  PColorVector(AGLColor.AsAddress)^ := AColor;
end;

{ TsgTeselledVertexList }

procedure TsgTeselledVertexList.BuildList(var mrci: TRenderContextInfo);
begin
  inherited BuildList(mrci);
end;


{ TCustomIterator }

constructor TCustomIterator.Create;
begin
  FMatrix := cnstIdentityMat;
end;

function TCustomIterator.GetEnt(Entity: TsgDXFEntity): Integer;
begin
  Result := 0;
end;

function TCustomIterator.GetParams: PsgCADIterate;
begin
  Result := @FParams;
end;

procedure TCustomIterator.Iterate(AConverter: TsgDXFConverter);
var
  vParams: PsgCADIterate;
  vAutoInsert: Boolean;
begin
  if Assigned(AConverter) then
  begin
    vParams := AConverter.Params;
    try
      vAutoInsert := AConverter.AutoInsert;
      try
        AConverter.AutoInsert := True;
        FParams.Matrix := Matrix;
        AConverter.Iterate(GetEnt, nil, FParams);
      finally
        AConverter.AutoInsert := vAutoInsert;
      end;
    finally
      AConverter.Params := vParams;
    end;
  end;
end;

{ TCountIterator }

function TCountIterator.GetEnt(Entity: TsgDXFEntity): Integer;
begin
  Result := 0;
  Inc(FCount);
end;

{ TCustomFindEnt }

function TCustomFindEnt.GetEnt(Entity: TsgDXFEntity): Integer;
begin
  Result := 0;
  if Entity = Obj then
    FMatrix := FParams.Matrix;
end;

function TCustomFindEnt.GetObject: TObject;
begin
  Result := FObj;
end;

procedure TCustomFindEnt.SetObject(const Value: TObject);
begin
  FObj := Value;
end;

{ TsgGLPolyline }

procedure TsgGLPolyline.Assign(Source: TsgBaseList);
begin
  inherited Assign(Source);
  if Source is TsgGLPolyline then
  begin
    F3DNav := TsgGLPolyline(Source).F3DNav;
    FMaterialName := TsgGLPolyline(Source).MaterialName;
    FMaterialNamePoint := TsgGLPolyline(Source).MaterialNamePoint;
    FMaterialNameText := TsgGLPolyline(Source).MaterialNameText;
    FMaterialNameLine := TsgGLPolyline(Source).MaterialNameLine;
    FLineWidth := TsgGLPolyline(Source).LineWidth;
    FArrowSize := TsgGLPolyline(Source).ArrowSize;
    FPointSize := TsgGLPolyline(Source).PointSize;
    FMode := TsgGLPolyline(Source).Mode;
    FVisiblePoints := TsgGLPolyline(Source).VisiblePoints;
    FNormal := TsgGLPolyline(Source).FNormal;
    FTextList.Assign(TsgGLPolyline(Source).FTextList);
    FDimList.Assign(TsgGLPolyline(Source).FDimList);
    FBegin := TsgGLPolyline(Source).FBegin;
    FEnd := TsgGLPolyline(Source).FEnd;
    FCenter := TsgGLPolyline(Source).FCenter;
    FRadius := TsgGLPolyline(Source).FRadius;
    FHeight := TsgGLPolyline(Source).FHeight;
    FDimTextPos := TsgGLPolyline(Source).FDimTextPos;
    FNormals.Assign(TsgGLPolyline(Source).FNormals);
    FExData.Assign(TsgGLPolyline(Source).FExData);
    FRect := cnstBadRect;
    FType := TsgGLPolyline(Source).FType;
    FIndexToCircle := TsgGLPolyline(Source).FIndexToCircle;

    FMeshList.Assign(TsgGLPolyline(Source).FMeshList);
    FCountUses := TsgGLPolyline(Source).FCountUses;


  end;
end;

function TsgGLPolyline.GetWorldToScreenAffine(var rci: TRenderContextInfo;
  APoint: TFPoint; AMatrix: TMatrix4f): TAffineVector;
begin
  Result := VectorTransform(FPoint2Vect(APoint), AMatrix);
  Result:= TGLSceneBuffer(rci.buffer).WorldToScreen(Result);
  Result.V[2] := 0;
end;

function TsgGLPolyline.GetWorldToScreenFPoint(var rci: TRenderContextInfo;
  APoint: TFPoint; AMatrix: TMatrix4f): TFPoint;
begin
  Result := Vect2FPoint(GetWorldToScreenAffine(rci, APoint, AMatrix));
end;

//!!!! Attention
//The points are drawn by calling the BuildList procedure again,
//since they always have a color different from the color of
//the line and this will lead to a drop in drawing speed
procedure TsgGLPolyline.BuildListPoints(var rci: TRenderContextInfo);
var
  I: Integer;
begin
  if (Count > 1) and FVisiblePoints then
  begin
    SetupPointStyle(rci);
    GL.Begin_(GL_POINTS);
    //GL.Color4f(1, 0, 0, 1);
    // single color
    for I := 0 to Count - 1 do
      GL.Vertex3dv(@List^[I]);
{$IFDEF ADD_POINT_CIRCLE}
    if FRadius > 0 then
      GL.Vertex3d(FCenter.X, FCenter.Y, FCenter.Z);
{$ENDIF}
    GL.End_;
  end;
end;

procedure TsgGLPolyline.BuildList(var rci: TRenderContextInfo);
var
  I: Integer;
  vPoint1, vPoint2: TFPoint;

  procedure DoDimensionPolyline(const APoint, APoint2, ANormal: TFPoint;
      const APolyline: TFPointList);
  var
    vSize, vExe, vSizePixel: Double;
    vIndexBegin, vIndexEnd: Integer;
    function AddAndCombineItem(const APointAdd, ANormalBase: TFPoint;
      vLen: Double; const APolylineBase: TFPointList): Integer;
    begin
      Result := APolylineBase.Add(APointAdd);
      APolylineBase.CombineItem(Result, ANormalBase, vLen);
    end;
  begin
    vSizePixel := GetSizePixel(rci);
    vSize := GetByScale(FHeight);
    if Abs(vSize) < Abs(GetByScale(Measure3DParams.DimExe) * 2) then
      vSize :=  Abs(GetByScale(Measure3DParams.DimExe) * 2) * sgSign(FHeight);
    vSize := vSize * vSizePixel;
    vExe := GetByScale(Measure3DParams.DimExe) * vSizePixel;

    APolyline.Add(APoint);
    vIndexBegin := AddAndCombineItem(APoint, ANormal,
      IfThen(FHeight < 0, vSize, vSize), APolyline);
    APolyline.Add(APoint2);
    vIndexEnd := AddAndCombineItem(APoint2, ANormal,
      IfThen(FHeight < 0, vSize, vSize), APolyline);

    AddAndCombineItem( APolyline[vIndexBegin-1], ANormal,
      IfThen(FHeight < 0, vSize + vExe, vSize - vExe), APolyline);
    AddAndCombineItem( APolyline[vIndexEnd-1], ANormal,
      IfThen(FHeight < 0, vSize + vExe, vSize - vExe), APolyline);
  end;

  procedure GetDimensionPolyline2(const APoint, APoint2, ANormal: TFPoint;
      const APolyline: TFPointList);
  var
    I: Integer;
    vMatrixContex: TMatrix4f;
    vPoint, vPoint2: TAffineVector;
    vDis: TsgFloat;
  begin

    GLPush(rci);

    vMatrixContex := rci.PipelineTransformation.ModelMatrix;

    DoDimensionPolyline(APoint, APoint2,
     FNormal, APolyline);

    vPoint := GetWorldToScreenAffine(rci, APolyline.List[0], vMatrixContex);
    vPoint2 := GetWorldToScreenAffine(rci, APolyline.List[1], vMatrixContex);
    vDis := DistanceFPoint(Vect2FPoint(vPoint), Vect2FPoint(vPoint2));
    if vDis < GetByScale(Measure3DParams.DimExe) then
    begin
      if FNormals.Count > 0 then
      begin
        for I := 0 to FNormals.Count - 1 do
        begin
          if IsEqualFPoints(FNormal, FNormals.List[I]) then
            Continue;
          APolyline.Clear;
          DoDimensionPolyline(APoint, APoint2, FNormals.List[I], APolyline);
        end;
      end;

    end;

    GLPop(rci);
  end;

begin
  if Count > 1 then
  begin
    // first, we setup the line color & stippling styles
    SetupLineStyle(rci);
//    if rci.bufferDepthTest then
//      rci.GLStates.Enable(stDepthTest)
//    else
//      rci.GLStates.Disable(stDepthTest);
    // lines, cubic splines or bezier
    GL.Begin_(FMode);
    // single color
    for I := 0 to Count - 1 do
      GL.Vertex3dv(@List^[I]);
    // Ext data point
    for I := 0 to FExData.Count - 1 do
      GL.Vertex3dv(@FExData.List^[I]);
    GL.End_;

    vPoint1 := cnstFPointZero;
    vPoint2 := cnstFPointZero;
    if (not IsEqualFPoints(FBegin, FEnd)) or (FType = 7) then
    begin
      if Count <=2 then
      begin
        FDimList.Clear;
        GetDimensionPolyline2(FBegin,
              FEnd, FNormal,
              FDimList);
        vPoint1 := FDimList.List[4];
        vPoint2 := FDimList.List[5];
{$IFDEF CLIP_GLCLIP}
        DisableClipPlane(F3DNav); //GL.Disable(GL_CLIP_PLANE0);
{$ENDIF}
        GL.Begin_(FMode);
        SetupColorByMaterialName(rci, FMaterialNameLine);
        // single color
        //FDimList.TransformAsVectors();
        for I := 0 to FDimList.Count - 1 do
          GL.Vertex3dv(@FDimList.List^[I]);
        GL.End_;
{$IFDEF CLIP_GLCLIP}
        EnableClipPlane(F3DNav);//GL.Enable(GL_CLIP_PLANE0);
{$ENDIF}
      end
      else
        FDimList.Clear;
    end;

{$IFDEF CLIP_GLCLIP}
    DisableClipPlane(F3DNav); //GL.Disable(GL_CLIP_PLANE0);
{$ENDIF}
    DrawText(rci, vPoint1, vPoint2);
{$IFDEF CLIP_GLCLIP}
    EnableClipPlane(F3DNav);//GL.Enable(GL_CLIP_PLANE0);
{$ENDIF}
  end;
end;

function TsgGLPolyline.ClientToWorld(var rci: TRenderContextInfo; X, Y,
  Z: Double): TVector3f;
begin
  //if FCameraStyle = csOrthogonal then
    Result := TGLSceneBufferAccess(rci.buffer).OrthoScreenToWorld(Round(X),
       Round(TGLSceneBufferAccess(rci.buffer).Height - Y));
//    else
//      Result := Buffer.ScreenToWorld(AffineVectorMake(X, Buffer.Height - Y, Z));
end;

constructor TsgGLPolyline.Create;
begin
  Create(nil);
end;

constructor TsgGLPolyline.Create(A3DNav: Tsg3DNavigator);
begin
  inherited Create;
  F3DNav := A3DNav;
  FTextList := TFPointList.Create;
  FDimList := TFPointList.Create;
  FNormals := TFPointList.Create;
  FExData := TFPointList.Create;
  FLineWidth := Measure3DParams.DimLwd;
  FScale := 1;
  FHeight := 0;
  FIndexToCircle := 0;
  FPointSize := Measure3DParams.PointSize;
  FVisiblePoints := False;
  FMode := GL_LINE_STRIP;
  FRect := cnstBadRect;

  FFrameColor := Measure3DParams.FrameColor;

  FMeshList := TsgObjectList.Create;
  FCountUses := 1;

end;

destructor TsgGLPolyline.Destroy;
begin
  FMeshList.Free;
  FTextList.Free;
  FDimList.Free;
  FNormals.Free;
  FExData.Free;
  inherited Destroy;
end;

procedure TsgGLPolyline.DrawText(var rci: TRenderContextInfo;
      APoint1, APoint2: TFPoint);
var
  vCanvas : TGLCanvas;
  vTextPos: TFPoint;
  I: Integer;
  vDim: TFPointList;
  vPoint, vPoint1, vPoint2, vFrameOffset: TFPoint;
  vMatrixContex: TMatrix4f;

  vDimArrow, vDimArrow1: TFPointList;

  vMatrix: TFMatrix;
  vAngle: Double;

  vAabb1, vAabb2: TAABB;

  vMin1, vMin2, vMax1, vMax2, vStartDim: TFPoint;

  vSize: TsgFloat;
  vOldColor: TColor;


  function CreateBufferGLCanvas(var rci : TRenderContextInfo) : TGLCanvas;
  var
    buffer : TGLSceneBuffer;
  begin
     buffer:=TGLSceneBuffer(rci.buffer);
     Result:=TGLCanvas.Create(buffer.Width, buffer.Height);
  end;

  procedure DrawArrow(const APoints: TFPointList);
  var
    I, n: Integer;
  begin
    n := APoints.Count;
    if n > 1 then
    begin
      GL.Begin_(GL_TRIANGLES);
      for i := 0 to APoints.Count - 1 do
        GL.Vertex2f(APoints.List[I].X, APoints.List[I].Y);
      GL.End_;
    end;
  end;

begin
  if FTextList.Count > 0 then
  begin
    GLPush(rci);
    try
      vDim := TFPointList.Create;
      vDimArrow := TFPointList.Create;
      vDimArrow1 := TFPointList.Create;
      try
        vMatrixContex := rci.PipelineTransformation.ModelMatrix;
        if Count <=2 then
        begin
          vPoint := GetWorldToScreenFPoint(rci, APoint1, vMatrixContex);
          vPoint2:= GetWorldToScreenFPoint(rci, APoint2, vMatrixContex);

          vAngle := GetAngleByPoints(vPoint, vPoint2, False, fDoubleResolution ,1, 1);
          vTextPos := MiddleFPoint(vPoint2, vPoint);

          GenerateArrowScreen(vPoint2, vAngle, vDimArrow);
          GenerateArrowScreen(vPoint, vAngle-180, vDimArrow1);
          vDimArrow.GetExtents(vMin1, vMax1);
          vDimArrow1.GetExtents(vMin2, vMax2);
          vAabb1.Min := FPoint2Vect(vMin1);
          vAabb1.Max := FPoint2Vect(vMax1);
          vAabb2.Min := FPoint2Vect(vMin2);
          vAabb2.Max := FPoint2Vect(vMax2);
          if IntersectAABBsAbsolute(vAabb1, vAabb2) then
          begin
            vDimArrow.Clear;
            vDimArrow1.Clear;
            GenerateArrowScreen(vPoint2, vAngle-180, vDimArrow);
            GenerateArrowScreen(vPoint, vAngle, vDimArrow1);
          end;
        end
        else
        begin
          vPoint := GetWorldToScreenFPoint(rci, List[FIndexToCircle], vMatrixContex);
          vPoint2:= GetWorldToScreenFPoint(rci, FCenter, vMatrixContex);
          vAngle := GetAngleByPoints(vPoint, vPoint2, False, fDoubleResolution ,1, 1);
          vMatrix := FMatByAngle(vAngle-180);
          vMatrix := FMatXMat(vMatrix, FMatByTranslate(vPoint2));
          if Measure3DParams.DimTofl then
            vStartDim := cnstFPointZero
          else
            vStartDim := MakeFPoint(DistanceFPoint(vPoint, vPoint2), 0, 0);
          vDim.Add(FPointXMat(vStartDim, vMatrix));
          vSize := GetByScale(FHeight);
          if vSize < GetByScale(Measure3DParams.DimExe) then
            vSize := GetByScale(Measure3DParams.DimExe);
          vPoint1 := FPointXMat(MakeFPoint(DistanceFPoint(vPoint, vPoint2)+vSize, 0, 0) , vMatrix);
          vDim.Add(vPoint1);
          GenerateArrowScreen(vPoint, vAngle, vDimArrow);
          vTextPos := vDim.List[vDim.Count-1];
        end;

        vCanvas:=CreateBufferGLCanvas(rci);
        try
          vCanvas.PenColor := GetColorByMaterialName(rci, FMaterialNameLine, clMaroon);
          vCanvas.PenWidth := Round(GetByScale(FLineWidth));
          vCanvas.InvertYAxis;

          // Draw Arrow
          vCanvas.StopPrimitive;
          vCanvas.PenColor := GetColorByMaterialName(rci, FMaterialNameLine, clMaroon);//clBlue;
          DrawArrow(vDimArrow);
          DrawArrow(vDimArrow1);

          // Draw Fill Box Text
          vCanvas.StopPrimitive;
          FTextList.GetExtents(vMin1, vMax1);
          vMin1 := MakeFPoint(GetByScale(vMin1.X), GetByScale(vMin1.Y));
          vMax1 := MakeFPoint(GetByScale(vMax1.X), GetByScale(vMax1.Y));
          vMin1 := AddFPoint(vMin1, MakeFPoint(-GetByScale(Measure3DParams.DimGap),
            -GetByScale(Measure3DParams.DimGap)));
          vMax1 := AddFPoint(vMax1, MakeFPoint(GetByScale(Measure3DParams.DimGap),
            GetByScale(Measure3DParams.DimGap)));
          vMin1 := AddFPoint(AddFPoint(vMin1, vTextPos), FDimTextPos);
          vMax1 := AddFPoint(AddFPoint(vMax1, vTextPos), FDimTextPos);
          if Count > 2 then
          begin
            vAngle := GetAngleByPoints(vPoint2, vTextPos, False, fDoubleResolution, 1, 1);
            if (vAngle <= 90) or (vAngle >= 270) then
            begin
              vFrameOffset := SubFPoint(vTextPos, vMin1);
              vMax1 := AddFPoint(vMax1, vFrameOffset);
              vMin1 := vTextPos;
            end
            else
            begin
              vFrameOffset := SubFPoint(vTextPos, MakeFpoint(vMax1.X, vMin1.Y));
              vMax1 := AddFPoint(vMax1, vFrameOffset);
              vMin1 := AddFPoint(vMin1, vFrameOffset);
            end;
          end
          else
            vFrameOffset := cnstFPointZero;


          FRect.Left := vMin1.X;
          FRect.Top := vMax1.Y;
          FRect.Right := vMax1.X;
          FRect.Bottom := vMin1.Y;

          vOldColor := vCanvas.PenColor;
          try
            //line to text
            vCanvas.PenColor := GetColorByMaterialName(rci, FMaterialNameLine, clMaroon);
            vCanvas.Line(vMin1.X, vMin1.Y, vTextPos.X, vTextPos.Y);
            //fill box
            vCanvas.PenColor := Measure3DParams.FrameBackgroundColor;
            vCanvas.FillRect(vMin1.X, vMin1.Y,
              vMax1.X, vMax1.Y);
            vCanvas.PenColor := FrameColor;
            // Box
            vCanvas.Line(vMin1.X, vMin1.Y, vMax1.X, vMin1.Y);
            vCanvas.Line(vMax1.X, vMin1.Y, vMax1.X, vMax1.Y);
            vCanvas.Line(vMax1.X, vMax1.Y, vMin1.X, vMax1.Y);
            vCanvas.Line(vMin1.X, vMax1.Y, vMin1.X, vMin1.Y);
          finally
            vCanvas.PenColor := vOldColor;
          end;

          //Draw Ext Line Text
          vCanvas.StopPrimitive;

          //Draw Radius Line
          vCanvas.PenWidth := Round(GetByScale(FLineWidth));
          if vDim.Count > 1 then
          begin
            for I := 0 to (vDim.Count div 2) - 1 do
            begin
              vCanvas.Line(vDim.List[I*2].X,vDim.List[I*2].Y,
                vDim.List[I*2+1].X, vDim.List[I*2+1].Y);
            end;
          end;

          //Draw Text
          vTextPos := sgFunction.AddFPoint(vTextPos, FDimTextPos);
          vTextPos := sgFunction.AddFPoint(vTextPos, vFrameOffset);
          vCanvas.PenWidth := Round(GetByScale(Measure3DParams.DimLwt));
          vCanvas.PenColor := GetColorByMaterialName(rci, FMaterialNameText, clMaroon);//clBlue;
          for I := 0 to (FTextList.Count div 2) -1  do
          begin
            vCanvas.Line(vTextPos.X + GetByScale(FTextList.List[I*2].X),
             vTextPos.Y +GetByScale(FTextList.List[I*2].Y),
             vTextPos.X +GetByScale(FTextList.List[I*2+1].X),
             vTextPos.Y +GetByScale(FTextList.List[I*2+1].Y));
          end;
        finally
          vCanvas.Free;
        end;
      finally
        vDimArrow.Free;
        vDimArrow1.Free;
        vDim.Free;
      end;
    finally
      GLPop(rci);
    end;
  end;

end;

procedure TsgGLPolyline.GenerateArrowScreen(APointScreen: TFPoint;
  AAngle: Double; const APoints: TFPointList);
var
  vMatrix: TFMatrix;

  procedure AddSolid(const X1,Y1, X2,Y2, X3,Y3: Double; const APoints: TFPointList;
    const AMatrix: TFMatrix);
  begin
    APoints.Add(FPointXMat(MakeFPoint(X1, Y1, 0), AMatrix));
    APoints.Add(FPointXMat(MakeFPoint(X2, Y2, 0), AMatrix));
    APoints.Add(FPointXMat(MakeFPoint(X3, Y3, 0), AMatrix));
  end;
begin
  vMatrix := FMatByAngle(AAngle);
  vMatrix := FMatXMat(vMatrix, FMatByTranslate(APointScreen));
  AddSolid(-GetByScale(ArrowSize),
            GetByScale(ArrowSize/6), 0,0,
           -GetByScale(ArrowSize),
           -GetByScale(ArrowSize/6), APoints, vMatrix);
end;

function TsgGLPolyline.GetByScale(AValue: Double): Double;
begin
  Result := AValue * FScale;
end;

function TsgGLPolyline.GetColorByMaterialName(var rci: TRenderContextInfo;
  AName: string; const ADefaultColor:TColor): TColor;
var
  vMatLibItem: TGLLibMaterial;
  vMaterialCache: TGLMaterial;

  function ConvertColorVector(const ARed, AGreen, ABlue: Single): TColor;
  begin
    Result := RGB(
      Round(255 * ARed),
      Round(255 * AGreen),
      Round(255 * ABlue));
  end;
begin
  vMaterialCache := nil;
  if Assigned(rci.materialLibrary) then
  begin
    vMatLibItem := TGLMaterialLibrary(rci.materialLibrary).LibMaterialByName(AName);
    if Assigned(vMatLibItem) then
      vMaterialCache := vMatLibItem.Material;
  end;
  if Assigned(vMaterialCache) then
    Result := ConvertColorVector(vMaterialCache.FrontProperties.Diffuse.Red,
    vMaterialCache.FrontProperties.Diffuse.Green, vMaterialCache.FrontProperties.Diffuse.Blue)
  else
    Result := ADefaultColor;
end;

function TsgGLPolyline.GetDistance: Double;
begin
  Result := 0;
  if Count <=2 then
    Result := DistanceFPoint(List[0], List[1]) * F3DNav.F3DDrawingNavigator.MeasureScaleFactor;
end;

function TsgGLPolyline.GetMaterailName: string;
begin
  Result := FMaterialName;
end;

function TsgGLPolyline.GetMaterailNamePoint: string;
begin
  Result := FMaterialNamePoint;
end;

function TsgGLPolyline.GetMaterialNameLine: string;
begin
  Result := FMaterialNameLine;
end;

function TsgGLPolyline.GetMaterialNameText: string;
begin
  Result := FMaterialNameText;
end;

function TsgGLPolyline.GetRadius: Double;
begin
  Result := FRadius * F3DNav.F3DDrawingNavigator.MeasureScaleFactor;
end;

function TsgGLPolyline.GetPointSize: TGLfloat;
begin
  Result := FPointSize;
end;

function TsgGLPolyline.GetSizePixel(var rci: TRenderContextInfo): Double;
begin
  Result := DistanceFPoint(Vect2FPoint(ClientToWorld(rci, 0, 0)),
        Vect2FPoint(ClientToWorld(rci, 1, 0)));
end;

procedure TsgGLPolyline.GLPop(var rci: TRenderContextInfo);
var
  vCurrentMode: GLint;
begin
  GL.GetIntegerv(GL_MATRIX_MODE, @vCurrentMode);
  try
    rci.PipelineTransformation.Pop;
    GL.MatrixMode(GL_MODELVIEW);
    GL.PopMatrix;
    GL.MatrixMode(GL_PROJECTION);
    GL.PopMatrix;
    GL.MatrixMode(GL_TEXTURE);
    GL.PopMatrix;
    GL.MatrixMode(GL_COLOR);
    GL.PopMatrix;
  finally
    GL.MatrixMode(vCurrentMode);
  end;
  CurrentGLContext.PipelineTransformation.Pop;
end;

procedure TsgGLPolyline.GLPush(var rci: TRenderContextInfo);
var
  vCurrentMode: GLint;
begin
  CurrentGLContext.PipelineTransformation.Push;
  GL.GetIntegerv(GL_MATRIX_MODE, @vCurrentMode);
  try
    GL.MatrixMode(GL_COLOR);
    GL.PushMatrix;
    GL.MatrixMode(GL_TEXTURE);
    GL.PushMatrix;
    GL.MatrixMode(GL_PROJECTION);
    GL.PushMatrix;
    GL.MatrixMode(GL_MODELVIEW);
    GL.PushMatrix;
    rci.PipelineTransformation.Push;
  finally
    GL.MatrixMode(vCurrentMode);
  end;
end;

function TsgGLPolyline.IsContainsPoint(APoint: TFPoint): Boolean;
begin
  Result := False;
  if not IsEqualFRects(FRect, cnstBadRect) then
  begin
    Result := IsPointInFRect2D(FRect, APoint);
  end;
end;

procedure TsgGLPolyline.SetMaterailName(const Value: string);
begin
  FMaterialName := Value;
end;

procedure TsgGLPolyline.SetMaterailNamePoint(const Value: string);
begin
  FMaterialNamePoint := Value;
end;

procedure TsgGLPolyline.SetMaterialNameLine(const Value: string);
begin
  FMaterialNameLine := Value;
end;

procedure TsgGLPolyline.SetMaterialNameText(const Value: string);
begin
  FMaterialNameText := Value;
end;

procedure TsgGLPolyline.SetupColorByMaterialName(var rci: TRenderContextInfo;
  AName: string);
var
  vCurrentPenColorVector: TColorVector;
begin
  vCurrentPenColorVector.V[3] := 1;
  SetVector(vCurrentPenColorVector,
    ConvertWinColor(GetColorByMaterialName(rci, AName, clBlack),
    vCurrentPenColorVector.V[3]));
  GL.Color4fv(@vCurrentPenColorVector);
end;

procedure TsgGLPolyline.SetupColor(var rci: TRenderContextInfo;
  AName: string);
var
  vMatLibItem: TGLLibMaterial;
  vMaterialCache: TGLMaterial;
  vColor: TColorVector;
begin
  vMaterialCache := nil;
  if Assigned(rci.materialLibrary) then
  begin
    vMatLibItem := TGLMaterialLibrary(rci.materialLibrary).
      LibMaterialByName(AName);
    if Assigned(vMatLibItem) then
      vMaterialCache := vMatLibItem.Material;
  end;
  if Assigned(vMaterialCache) then
    vColor := vMaterialCache.FrontProperties.Diffuse.Color
  else
    vColor := clrGray50;
  if Assigned(FCurrentGLLinesStates) and
    (not CompareMem(@(FCurrentGLLinesStates^.Color), @vColor, sizeof(TColorVector))) then
  begin
    FCurrentGLLinesStates^.Color := vColor;
    GL.Color4fv(PGLfloat(@(FCurrentGLLinesStates^.Color)));
  end;
end;

procedure TsgGLPolyline.SetupLineStyle(var rci: TRenderContextInfo);
begin
  if Assigned(FCurrentGLLinesStates) and
    (FCurrentGLLinesStates^.LineWidth <> FLineWidth * FScale) then
  begin
    FCurrentGLLinesStates^.LineWidth := FLineWidth * FScale;
    rci.GLStates.LineWidth := FLineWidth * FScale;
  end;
  SetupColor(rci, MaterialName);
end;

procedure TsgGLPolyline.SetupPointStyle(var rci: TRenderContextInfo);
begin
  {$IFDEF SG_CPUX64}if not IsWine then{$ENDIF}
  begin
    if Assigned(FCurrentGLLinesStates) and
      (FCurrentGLLinesStates^.PointSize <> FPointSize) then
    begin
      FCurrentGLLinesStates^.PointSize := FPointSize;
      rci.GLStates.PointSize := FCurrentGLLinesStates^.PointSize;
    end;
  end;
  if Length(MaterialNamePoint) > 0 then
    SetupColor(rci, MaterialNamePoint);
end;

procedure TsgGLPolyline.SetPointSize(const Value: TGLfloat);
begin
  FPointSize := Value;
end;

{ TsgGLLines }

constructor TsgGLLines.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPolylines := TsgObjectList.Create;
  FDepthTest := True;
  FVisiblePoints := False;
end;

procedure TsgGLLines.ApplyPolylinesScale(const AScale: Double; ADeep: Integer);
var
  I: Integer;
begin
  if ADeep >= 0 then
  begin
    for I := 0 to FPolylines.Count - 1 do
      TsgGLPolyline(FPolylines[I]).FScale := AScale;
    if ADeep > 0 then
      for I := 0 to Count - 1 do
        if Children[I] is TsgGLLines then
          TsgGLLines(Children[I]).ApplyPolylinesScale(AScale, ADeep - 1);
  end;
end;

procedure TsgGLLines.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TsgGLLines then
  begin
    Clear;
    for I := 0 to TsgGLLines(Source).FPolylines.Count - 1 do
      AddNode.Assign(TsgGLPolyline(TsgGLLines(Source).FPolylines[I]));
  end;
  inherited Assign(Source);
end;

procedure TsgGLLines.BuildList(var rci: TRenderContextInfo);
var
  I: Integer;
  vDepthTest: Boolean;
  vMaterialLibrary: TObject;
  vOldVisiblePoints: Boolean;

  function IsDraw(const APolylines: TsgGLPolyline): Boolean;
  const
    cnstFourEdge : array[0..3, 0..1] of Integer =
      ( (0,7) , (3, 6), (1, 4), (2, 5));
  var
    I: Integer;
    vMatrixContex: TMatrix4f;
    vPointMax, vPointMin: TFPoint;
    vDisModel, vDisModelMax: Double;
    vMin, vMax: TFPoint;
    V: Tsg3DCube;
  begin
    Result := False;
    if APolylines.FTextList.Count > 0 then
    begin
      APolylines.GLPush(rci);
      try
        vMatrixContex := rci.PipelineTransformation.ModelMatrix;
        MakeCube(APolylines.F3DNav.FBoxImage, V);
        vDisModelMax := 0;
        for I := Low(cnstFourEdge) to High(cnstFourEdge) do
        begin
          vPointMin := APolylines.GetWorldToScreenFPoint(rci,
            V[cnstFourEdge[I, 0]], vMatrixContex);
          vPointMax := APolylines.GetWorldToScreenFPoint(rci,
            V[cnstFourEdge[I, 1]], vMatrixContex);
          vDisModel := DistanceFPoint(vPointMin, vPointMax);
          if vDisModel > vDisModelMax then
            vDisModelMax := vDisModel;
        end;
        APolylines.FTextList.GetExtents(vMin, vMax);
        vMin := MakeFPoint(APolylines.GetByScale(vMin.X), APolylines.GetByScale(vMin.Y));
        vMax := MakeFPoint(APolylines.GetByScale(vMax.X), APolylines.GetByScale(vMax.Y));
        case APolylines.FType of
          7: begin
            if vDisModelMax * 4.5 > DistanceFPoint(vMin, vMax) * 2 then
              Result := True;
          end;
          else
            if vDisModelMax > DistanceFPoint(vMin, vMax) * 2 then
              Result := True;
        end;

      finally
        APolylines.GLPop(rci);
      end;
    end
    else
      Result := True;
  end;


begin
  vMaterialLibrary := rci.materialLibrary;
  vDepthTest := rci.bufferDepthTest;
  try
    rci.materialLibrary := Material.materialLibrary;
    rci.bufferDepthTest := FDepthTest;
    if (osDirectDraw in ObjectStyle) then
    begin
      for I := 0 to FPolylines.Count - 1 do
      begin
        if not IsDraw(TsgGLPolyline(FPolylines[I])) then
          Exit;
      end;
    end;

    //
    //The setting for each Polylines causes a large delay in the rendering.
    //
    //
    rci.GLStates.Disable(stLighting);
    rci.GLStates.Disable(stLineStipple);
    rci.GLStates.Disable(stLineSmooth);
    if FVisiblePoints then
      rci.GLStates.Disable(stBlend);

    FCurrentGLLinesStates.Color := clrWhite;
    FCurrentGLLinesStates.PointSize := -1;
    FCurrentGLLinesStates.LineWidth := 1;
    GL.Color4fv(PGLfloat(@clrWhite));
    //GL.PointSize(1);
    GL.LineWidth(1);

    for I := 0 to FPolylines.Count - 1 do
    begin
      if TsgGLPolyline(FPolylines[I]).FCountUses <= 0 then
        Continue;
      TsgGLPolyline(FPolylines[I]).FCurrentGLLinesStates :=
         @FCurrentGLLinesStates;
      TsgGLPolyline(FPolylines[I]).BuildList(rci);
    end;

    FCurrentGLLinesStates.Color := clrWhite;
    FCurrentGLLinesStates.PointSize := -1;
    FCurrentGLLinesStates.LineWidth := 1;
    GL.Color4fv(PGLfloat(@clrWhite));
    //GL.PointSize(1);
    GL.LineWidth(1);

    for I := 0 to FPolylines.Count - 1 do
    begin
      if TsgGLPolyline(FPolylines[I]).FCountUses <= 0 then
        Continue;

      if FVisiblePoints then
      begin
        vOldVisiblePoints := TsgGLPolyline(FPolylines[I]).VisiblePoints;
        TsgGLPolyline(FPolylines[I]).VisiblePoints := True;
      end
      else
        vOldVisiblePoints := True;

      TsgGLPolyline(FPolylines[I]).FCurrentGLLinesStates := @FCurrentGLLinesStates;
      TsgGLPolyline(FPolylines[I]).BuildListPoints(rci);
      if FVisiblePoints then
        TsgGLPolyline(FPolylines[I]).VisiblePoints := vOldVisiblePoints;
    end;

  finally
    rci.bufferDepthTest := vDepthTest;
    rci.materialLibrary := vMaterialLibrary;
  end;
end;

procedure TsgGLLines.Clear;
begin
  while FPolylines.Count > 0 do
    DeleteLastNode;
  DeleteChildCameras;
  DestroyHandles;
end;

procedure TsgGLLines.ClearGLHandles;
begin
  DeleteChildCameras;
  DestroyHandles;
end;

destructor TsgGLLines.Destroy;
begin
  Clear;
  FPolylines.Free;
  inherited Destroy;
end;

procedure TsgGLLines.DoRender(var rci: TRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
begin
  if ARenderSelf then
  begin
    if (osDirectDraw in ObjectStyle) or rci.amalgamating then
      BuildList(rci)
    else
      rci.GLStates.CallList(GetHandle(rci));
  end;
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TsgGLLines.PreProcessing(const AProgressObject: TProgress;
  const AAccuracy: Double; const ARoot: TPersistentObjectList);
type
  PHashItem = ^THashItem;
  THashItem = record
    Hash:  UInt64;
    Index: TsgNativeInt;
  end;
  THashItemsArray =  array[0..MaxInt div SizeOf(THashItem) - 1] of THashItem;
  PHashItemsArray = ^THashItemsArray;
const
  cnstSign = TsgNativeInt({$IFDEF SG_CPUX64}$8000000000000000{$ELSE}$80000000{$ENDIF});
  costMagicalIndex = $BEDA;
var
  I, J, K, vIndexI, vIndexJ: Integer;
  P: PHashItemsArray;
  L: TsgCollection;
  vPoly: TsgGLPolyline;
  vMesh: TsgMeshObject;
  vPolylines: TsgObjectList;

//{$IFDEF ADD_POINT_CIRCLE}
//  vCenter: TFPoint;
//  vRadiaus: Double;
//{$ENDIF}

  function IsEqualGLPolylines(const APolylines: TsgObjectList; I1, I2: TsgNativeInt): Boolean;
  var
    I, J, M: Integer;
    vPoly1, vPoly2: TsgGLPolyline;
    vP1, vP2: TFPointList;
    vInverse: Boolean;
    vNodeUnbox1,vNodeUnbox2: TCustomNode;
    vMesh1, vMesh2: TsgMeshObject;
  begin
    Result := False;
    vP1 := TFPointList(APolylines[Abs(I1) - costMagicalIndex]);
    vP2 := TFPointList(APolylines[Abs(I2) - costMagicalIndex]);
    if (vP1 = nil) or (vP2 = nil) then
      Exit;
    vInverse := (I1 xor I2) and cnstSign <> 0;
    if vP1.Count <> vP2.Count then
      Exit;
    Result := True;
    if vInverse then
      for M := vP1.Count - 1 downto 0 do
      begin
        if not IsEqualFPoints(vP1.List[M], vP2.List[vP2.Count -1 - M]) then
        begin
          Result := False;
          Break;
        end;
      end
    else
      for M := 0 to vP1.Count - 1 do
      begin
        if not IsEqualFPoints(vP1.List[M], vP2.List[M]) then
        begin
          Result := False;
          Break;
        end;
      end;
    if Result and Assigned(ARoot) then
    begin
      //Edges belonging to different topological units that are carried
      // by explosion cannot be combined.
      vPoly1 := TsgGLPolyline(vPolylines[Abs(I1) - costMagicalIndex]);
      vPoly2 := TsgGLPolyline(vPolylines[Abs(I2) - costMagicalIndex]);

      if Assigned(vPoly1.FMeshList) or Assigned(vPoly2.FMeshList) then
      begin
        for I := 0 to vPoly1.FMeshList.Count - 1 do
        begin
          vMesh1 := TsgMeshObject(vPoly1.FMeshList[I]);
          if not Assigned(vMesh1) then
            Continue;
          vNodeUnbox1 := TCustomNode.GetNodeUnboxingElenent(TCustomNode(ARoot),
            vMesh1.Node, [TsgModTopoSolid, TsgModTopoShell]);
          for J := 0 to vPoly2.FMeshList.Count - 1 do
          begin
            vMesh2 := TsgMeshObject(vPoly2.FMeshList[J]);
            if not Assigned(vMesh2) then
              Continue;
            vNodeUnbox2 := TCustomNode.GetNodeUnboxingElenent(TCustomNode(ARoot),
              vMesh2.Node, [TsgModTopoSolid, TsgModTopoShell]);
            if vNodeUnbox1 <> vNodeUnbox2 then
            begin
              Result := False;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
begin
  vPolylines := TsgObjectList.Create;
  try
    vPolylines.Assign(FPolylines);
    FPolylines.Count := 0;
    if vPolylines.Count > 1 then
    begin
      L := TsgCollection.Create;
      try
        L.Duplicates := dupAccept;
        GetMem(P, SizeOf(THashItem) * vPolylines.Count * 2);
        TsgBaseListAccess(L).Attach(P, vPolylines.Count * 2);
        for I := 0 to vPolylines.Count - 1 do
        begin
          P^[2*I].Hash := HashPoint(@TFPointList(vPolylines[I]).List^[0]);
          P^[2*I].Index := costMagicalIndex + I;
          P^[2*I+1].Hash := HashPoint(
            @TFPointList(vPolylines[I]).List^[TFPointList(vPolylines[I]).Count - 1]);
          P^[2*I+1].Index := -(costMagicalIndex + I);
        end;
        L.Sort;
        J := 0;
        while J < L.Count do
        begin
          I := J;
          Inc(I);
          while (I < L.Count) and (P^[J].Hash = P^[I].Hash) and (Abs(P^[J].Index) <> Abs(P^[I].Index)) do
          begin
            if IsEqualGLPolylines(vPolylines, P^[J].Index, P^[I].Index) then
            begin
              vIndexI := Abs(P^[I].Index) - costMagicalIndex;
              vIndexJ := Abs(P^[J].Index) - costMagicalIndex;
              vPoly := TsgGLPolyline(vPolylines[vIndexI]);
              TsgGLPolyline(vPolylines[vIndexJ]).FNormals.Add(vPoly.FNormal);
              for K := 0 to vPoly.FMeshList.Count - 1 do
                TsgGLPolyline(vPolylines[vIndexJ]).FMeshList.Add(vPoly.FMeshList[K]);
              TsgGLPolyline(vPolylines[vIndexJ]).FCountUses :=
                TsgGLPolyline(vPolylines[vIndexJ]).FCountUses + vPoly.FCountUses;
              vPolylines[vIndexI] := nil;
              vPoly.Free;
            end;
            Inc(I);

            if (I mod 2) = 0 then
            begin
              if Assigned(AProgressObject) then
                AProgressObject.Progress;
            end;

          end;
          J := I
        end;
      finally
        L.Free;
      end;
    end;

    //vPolylines.Pack;
    for I := vPolylines.Count - 1 downto 0 do
    begin
      if vPolylines[I] = nil then
        vPolylines.Delete(I);
    end;
    FPolylines.Assign(vPolylines);
  finally
    vPolylines.Free;
  end;

  for I := 0 to FPolylines.Count - 1 do
  begin
    vPoly := TsgGLPolyline(FPolylines[I]);
    for J := 0 to vPoly.FMeshList.Count - 1 do
    begin
      if Assigned(vPoly.FMeshList[J]) and (TObject(vPoly.FMeshList[J]).InheritsFrom(TsgMeshObject)) then
      begin
        vMesh := TsgMeshObject(vPoly.FMeshList[J]);
        vMesh.FEdgeIndexList.Add(I);
      end;
    end;
  end;

//{$IFDEF ADD_POINT_CIRCLE}
//  for I := 0 to FPolylines.Count - 1 do
//  begin
//    vPoly := TsgGLPolyline(FPolylines[I]);
//    if CalcCircleParams( vPoly, vCenter, vRadiaus, AAccuracy) then
//    begin
//      vPoly.Center := vCenter;
//      vPoly.FRadius := vRadiaus;
//    end;
//  end;
//{$ENDIF}

end;

function TsgGLLines.AddNode: TsgGLPolyline;//TGLLines;
var
  vDepthImmersion: Byte;

  function Find3dNavigator(const AOwner: TComponent; var ADepthImmersion: Byte): Tsg3DNavigator;
  var
    vOwner: TComponent;
  begin
    Result := nil;
    if ADepthImmersion <> 0 then
    begin
      Dec(ADepthImmersion);
      vOwner := AOwner.Owner;
      if Assigned(vOwner) then
        if vOwner is Tsg3DNavigator then
          Result := Tsg3DNavigator(vOwner)
        else
          Result := Find3dNavigator(vOwner, ADepthImmersion);
    end;
  end;
begin
  vDepthImmersion := 10;
  Result := TsgGLPolyline.Create(Find3dNavigator(Owner, vDepthImmersion));
  FPolylines.Add(Result);
end;

function TsgGLLines.ReleaseLastNode: TsgGLPolyline;
var
  I: Integer;
begin
  I := FPolylines.Count - 1;
  if I >= 0 then
  begin
    Result := TsgGLPolyline(FPolylines.List[I]);
    FPolylines.Delete(I);
  end
  else
    Result := nil;
end;

procedure TsgGLLines.DeleteLastNode;
begin
  ReleaseLastNode.Free;
end;

{ TsgMeshObject }

constructor TsgMeshObject.CreateWithEnt(AOwner: TMeshObjectList; AEnt: TObject);
begin
  CreateOwned(AOwner);
  SetEntity(AEnt);
  FEdgeIndexList := TIntegerList.Create;
  FArea := -1;
  FBox := cnstBadRect;
end;

destructor TsgMeshObject.Destroy;
begin
  if Assigned(FVisibleData) then
    RestoreVisibleData;
  if Assigned(FOriginalData) then
    RestoreData;
  FEdgeIndexList.Free;
  inherited Destroy;
end;

procedure TsgMeshObject.WriteToFiler(writer: TVirtualWriter);
begin
  inherited WriteToFiler(writer);
  FEdgeIndexList.WriteToFiler(writer);
end;

procedure TsgMeshObject.ReadFromFiler(reader: TVirtualReader);
begin
  inherited ReadFromFiler(reader);
  FEdgeIndexList.ReadFromFiler(reader);
end;

procedure TsgMeshObject.SaveVisibleData;
var
  vWriter: TBinaryWriter;
begin
  if not Assigned(FVisibleData) then
  begin
    FVisibleData := TMemoryStream.Create;
    vWriter := TBinaryWriter.Create(FVisibleData);
    try
      WriteToFiler(vWriter);
    finally
      vWriter.Free;
    end;
  end;
end;

procedure TsgMeshObject.RestoreVisibleData;
var
  vReader: TBinaryReader;
begin
  if Assigned(FVisibleData) then
  begin
    Clear;
    FVisibleData.Position := 0;
    vReader := TBinaryReader.Create(FVisibleData);
    try
      ReadFromFiler(vReader);
    finally
      vReader.Free;
    end;
    FVisibleData.Free;
    FVisibleData := nil;
  end;
end;

procedure TsgMeshObject.SaveData;
var
  vWriter: TBinaryWriter;
begin
  if not Assigned(FOriginalData) then
  begin
    FOriginalData := TMemoryStream.Create;
    vWriter := TBinaryWriter.Create(FOriginalData);
    try
      WriteToFiler(vWriter);
    finally
      vWriter.Free;
    end;
    //Clear;
  end;
end;

procedure TsgMeshObject.RestoreData;
var
  vReader: TBinaryReader;
begin
  if Assigned(FOriginalData) then
  begin
    Clear;
    FOriginalData.Position := 0;
    vReader := TBinaryReader.Create(FOriginalData);
    try
      ReadFromFiler(vReader);
    finally
      vReader.Free;
    end;
    FOriginalData.Free;
    FOriginalData := nil;
  end;
end;

procedure TsgMeshObject.SetBox(const ABox: TFRect);
begin
  FBox := ABox;
end;

function TsgMeshObject.GetBox: TFRect;
begin
  Result := FBox;
end;

procedure TsgMeshObject.GetEdgeIndexListByData(
  var AEdgeIndexList: TIntegerList);
var
  vReader: TBinaryReader;
  vTempMesh: TsgMeshObject;
begin
  if Assigned(FOriginalData) then
  begin
    vTempMesh := TsgMeshObject.CreateWithEnt(nil, nil);
    try
      vTempMesh.Clear;
      FOriginalData.Position := 0;
      vReader := TBinaryReader.Create(FOriginalData);
      try
        vTempMesh.ReadFromFiler(vReader);
      finally
        vReader.Free;
      end;
      AEdgeIndexList.Clear;
      AEdgeIndexList.Assign(vTempMesh.FEdgeIndexList);
    finally
      vTempMesh.Free;
    end;
  end;
end;

function TsgMeshObject.GetArea: Double;
begin
  Result := FArea;
end;

function TsgMeshObject.GetInsert: TObject;
begin
  if Assigned(FNode) then
    Result := FNode.Obj
  else
    Result := nil;
end;

function TsgMeshObject.GetRootInsert: TObject;
var
  N: TCustomNode;
begin
  Result := nil;
  if Assigned(FNode) then
  begin
    N := TNodeLite(FNode);
    while Assigned(N) and Assigned(N.Owner) and (N.Owner.ClassType <> TRoot) do
      N := N.Owner;
    if Assigned(N) then
      Result := N.Obj;
  end;
end;

function TsgMeshObject.IsUseVisibleData: Boolean;
begin
  Result := Assigned(FVisibleData);
end;

procedure TsgMeshObject.AddMeshToGLPolyline(const APoly: TsgGLPolyline);
begin
  APoly.FMeshList.Add(Self);
  Inc(APoly.FCountUses);
end;

procedure TsgMeshObject.RemoveMeshToGLPolyline(const APoly: TsgGLPolyline);
var
  I: Integer;
begin
  I := APoly.FMeshList.IndexOf(Self);
  if I <> - 1 then
  begin
    APoly.FMeshList.Delete(I);
    Dec(APoly.FCountUses);
  end
end;

procedure TsgMeshObject.AddRemoveConnectionWithEdges(AList: TsgGLLines;
  AProc: TsgMeshObjectAddDeleteProc);
var
  I: Integer;
  vPoly: TsgGLPolyline;
begin
  for I := 0 to FEdgeIndexList.Count - 1 do
  begin
    vPoly := TsgGLPolyline(AList.FPolylines[FEdgeIndexList[I]]);
    if vPoly <> nil then
    begin
      AProc(vPoly);
    end;
  end;
end;

procedure TsgMeshObject.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TsgMeshObject then
  begin
    FEdgeIndexList.Assign(TsgMeshObject(Source).FEdgeIndexList);
    FEntity := TsgMeshObject(Source).FEntity;
    FNode := TsgMeshObject(Source).FNode;
  end;
end;

procedure TsgMeshObject.AddConnectionWithEdges(AList: TsgGLLines);
begin
  AddRemoveConnectionWithEdges(AList, AddMeshToGLPolyline);
end;

procedure TsgMeshObject.BreakConnectionWithEdges(AList: TsgGLLines);
begin
  AddRemoveConnectionWithEdges(AList, RemoveMeshToGLPolyline);
end;

procedure TsgMeshObject.SetArea(const AArea: Double);
begin
  FArea := AArea;
end;

procedure TsgMeshObject.SetEntity(AEnt: TObject);
begin
  FEntity := AEnt;
end;

{ TsgPickObject }

procedure TsgPickObject.BuildLines;
var
  I, J, K: Integer;
  vM: TFMatrix;
  vLines: TFPointList;
  P: PFPointArray;
  vPolyArray: PFPointArray;
begin
  ClearLines;
  CreateLines;
  vLines := TFPointList.Create;
  try
    for I := 0 to FObjects.Count - 1 do
    begin
      vLines.Clear;
      if not Assigned(FObjects[I]) then
        Continue;
      if TObject(FObjects[I]).ClassType = TsgGLPolyline then
      begin
        FPoly.Assign(TsgGLPolyline(FObjects[I]));
        FPoly.MaterialName := AssignMaterial(FColor, Tsg3DDrawingNavigator(FOwner).Navigator3D.FGLMaterialLibrary);
        FPoly.LineWidth := LineWidth;
      end
      else
      if TObject(FObjects[I]).ClassType = TsgPickObject then
      begin
        FPoly.Assign(TsgPickObject(FObjects[I]).FPoly);
        FPoly.MaterialName := AssignMaterial(FColor, Tsg3DDrawingNavigator(FOwner).Navigator3D.FGLMaterialLibrary);
        FPoly.LineWidth := FPoly.LineWidth + Measure3DParams.DimensionThickness;
      end
      else
      if Tsg3DDrawingNavigator(FOwner).GetMeshLines(TsgMeshObject(FObjects[I]), vLines, @vM) then
      begin
        P := vLines.List;
        K := FPoly.Count;
        FPoly.Count := K + vLines.Count;
        vPolyArray := @FPoly.List^[K];
        for J := 0 to vLines.Count - 1 do
          vPolyArray^[J] := FPointXMat(P^[J], vM);
      end;
    end;
  finally
    vLines.Free;
  end;
  FLines.Visible := FPoly.Count > 0;
end;

procedure TsgPickObject.ClearLines;
begin
  if FLines <> nil then
  begin
    FLines.FPolylines.Remove(FPoly);
    FLines.Clear;
    FPoly.Clear;
    FLines.FPolylines.Add(FPoly);
  end;
end;

constructor TsgPickObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FObjects := TsgObjectList.Create;
  FOwner := AOwner;
  FPick := nil;
  LineWidth := Measure3DParams.DimensionThickness;
  CreateLines;
end;

procedure TsgPickObject.CreateLines;
var
  vNav3d: Tsg3DNavigator;
begin
  vNav3d := Tsg3DDrawingNavigator(FOwner).Navigator3D;
  if not Assigned(FLines) then
  begin
    FLines := TsgGLPickLines.CreateAsChild(vNav3d.GLDCScene);
    FLines.DepthTest := False;
    TsgGLPickLines(FLines).PickObjects := Self;
    FPoly := FLines.AddNode;
  end;
  FPoly.Mode := GL_LINES;
  FPoly.MaterialName := AssignMaterial(Color, vNav3d.FGLMaterialLibrary);
  FPoly.LineWidth := LineWidth;
end;

destructor TsgPickObject.Destroy;
begin
  FObjects.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TsgPickObject.DoPick;
begin
  if FEnabled then
    if Assigned(FOnPick) then
      FOnPick(Self);
end;

function TsgPickObject.GetAlpha: Double;
begin
  Result := TsgSupportAlphaBlend.GetAlpha(FObjects);
end;

function TsgPickObject.GetGLVisible: Boolean;
begin
  Result := FLines.Visible;
end;

function TsgPickObject.GetObject: TObject;
begin
  Result := FPick;
end;

function TsgPickObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TsgPickObject.SetAlpha(const Alpha: Double);
var
  vMeshLib: TGLMaterialLibrary;
begin
  vMeshLib := Tsg3DDrawingNavigator(FOwner).Navigator3D.FGLMaterialLibrary;
  TsgSupportAlphaBlend.SetAlpha(vMeshLib, FObjects, Alpha);
end;

procedure TsgPickObject.SetGLVisible(const Value: Boolean);
begin
  FLines.Visible := Value;
end;

procedure TsgPickObject.SetObject(AObject: TObject);
var
  I: Integer;
begin
  if AObject <> FPick then
  begin

    if FPickCached and not Assigned(FPickCache) then
    begin
      FPickCache := AObject;
      if Assigned(FPick) then
      begin
        Tsg3DDrawingNavigator(FOwner).DeleteGLPolyline(FPick);
        FPick := nil;
        DoPick;
      end;
    end;

    I := FObjects.IndexOf(AObject);
    if I = -1 then
    begin
      FObjects.Clear;
      FObjects.Add(AObject);
    end;

    if FPickCached and Assigned(FPickCache) then
    begin
      Tsg3DDrawingNavigator(FOwner).DeleteGLPolyline(FPick);
      FPick := FPickCache;
      FPickCache := nil;
    end
    else
    begin
      Tsg3DDrawingNavigator(FOwner).DeleteGLPolyline(FPick);
      FPick := AObject;
    end;
    if FPick = nil then
    begin
      Tag := nil;
      TopNode := nil;
    end;
    DoPick;
  end
  else
    if (FPick = nil) and (FObjects.Count > 0) then
    begin
      FObjects.Clear;
      Tag := nil;
      TopNode := nil;
      DoPick;
    end;
end;

procedure TsgPickObject.SetUserString(const Value: string);
begin
  FUserString := Value;
end;

{ TsgGLPickLines }

procedure TsgGLPickLines.BuildList(var rci: TRenderContextInfo);
var
  w: Single;
begin
  w := rci.GLStates.LineWidth;
  inherited BuildList(rci);
  rci.GLStates.LineWidth := w;
end;

destructor TsgGLPickLines.Destroy;
begin
  if Assigned(FPickObjects) then
    FPickObjects.FLines := nil;
  inherited Destroy;
end;

{ TTesIndexList }

procedure TTesIndexList.BuildList(var mrci: TRenderContextInfo);
begin
  inherited
end;

{ TCustomNode }

class procedure TCustomNode.ClearVisualization(const ANode: TCustomNode);
var
  I: Integer;
  vEntity: TObject;
  vCollection: TObject;
begin
  vEntity := ANode.FObj;
  if Assigned(vEntity) and (vEntity is TsgDXFInsert) then
  begin
    vCollection := TsgDXFInsertAccess(vEntity).Visualization;
    if Assigned(vCollection) and (vCollection is TsgCollection) then
    begin
      TsgCollection(vCollection).Free;
      TsgDXFInsertAccess(vEntity).Visualization := nil;
    end;
  end;
  for I := 0 to ANode.Count - 1 do
    TCustomNode.ClearVisualization(TCustomNode(ANode.Items[I]));
end;

function TCustomNode.ConvertBoxIfExploded(ARoot: TCustomNode;
  var ABox: TFRect): Boolean;
var
  vNode: TCustomNode;
begin
  Result := True;
  vNode := GetNodeOwnerByTopoClass(ARoot, Self,
    [TsgModTopoSolid, TsgModTopoShell]);
  if vNode = ARoot then
    Result := False
  else
    ABox := FRectXMat(ABox, FMatByTranslate(Vect2FPoint(vNode.Delta)));
end;

constructor TCustomNode.Create(AOwner: TCustomNode);
begin
  inherited Create;
  FOwner := AOwner;
  FVisible := True;
  FDelta := {$IFDEF GLSCENE13}GLVectorGeometry{$ELSE}VectorGeometry{$ENDIF}.NullVector;
  FBoxByVisible := cnstBadRect;
end;

function TCustomNode.Find(AObj: TObject; var I: Integer): Boolean;
var
  N: TNodeLite;
begin
  N := TNodeLite.Create(nil);
  N.Obj := AObj;
  Result := QFind(Self, True, dupIgnore, NodesCompare, N, I);
  N.Free;
end;

function TCustomNode.GetArea: Double;
var
  I: Integer;
  vArea, vAreaSun: Double;
begin
  Result := -1;
  vAreaSun := 0;
  if Assigned(FMesh) then
  begin
     vArea := TsgMeshObject(FMesh).Area;
     if vArea >= 0 then
       vAreaSun := vArea;
  end;
  for I := 0 to Count - 1 do
  begin
    vArea := TCustomNode(Items[I]).GetArea;
    if vArea >= 0 then
      vAreaSun := vAreaSun + vArea;
  end;
  if vAreaSun > 0 then
    Result := vAreaSun;
end;

//procedure TCustomNode.SetBox(const ABox: TFRect);
//begin
//  FBox := ABox;
//end;

function TCustomNode.GetBox(const IsCache: Boolean = False): TFRect;
var
  I: Integer;
  vBox: TFRect;
begin
  if IsCache then
  begin
    Result := FBox;
    Exit;
  end;
  Result := cnstBadRect;
  if Assigned(FMesh) then
  begin
    vBox := TsgMeshObject(FMesh).GetBox;
    if not IsBadRect(vBox) then
      Result := vBox;
  end;
  for I := 0 to Count - 1 do
  begin
    vBox := TCustomNode(Items[I]).GetBox;
    if not IsBadRect(vBox) then
    begin
      ExpandFRect(Result, vBox.BottomRight);
      ExpandFRect(Result, vBox.TopLeft);
    end;
  end;
  FBox := Result;
end;

function TCustomNode.GetCustomColor: Cardinal;
var
  vMeshes: TsgObjectList;
  vCustomColor: string;
  vCode: Integer;
begin
  vMeshes := TsgObjectList.Create;
  try
    GetMeshList(vMeshes);
    vCustomColor := '$' + TsgSupportCustomColor.GetCustomColor(vMeshes);
    Val(vCustomColor, Result, vCode);
    if vCode <> 0 then
      Result := clByBlock;
  finally
    vMeshes.Free;
  end;
end;

{$IFDEF SG_ROTATE_CENTER_TEST}
function TCustomNode.GetCurrentBox(const IsCache: Boolean = False): TFRect;
var
  I: Integer;
  vBox: TFRect;
begin
  if (not IsBadRect(FBoxByVisible)) and IsCache then
  begin
    Result := FBoxByVisible;
    Exit;
  end;
  Result := cnstBadRect;
  if Assigned(FMesh) and FVisible then
  begin
    vBox := TsgMeshObject(FMesh).GetBox;
    if not IsBadRect(vBox) then
      Result := vBox;
  end;
  for I := 0 to Count - 1 do
  begin
    vBox := TCustomNode(Items[I]).GetCurrentBox;
    if not IsBadRect(vBox) then
    begin
      ExpandFRect(Result, vBox.BottomRight);
      ExpandFRect(Result, vBox.TopLeft);
    end;
  end;
  FBoxByVisible := Result;
end;
{$ENDIF}

function TCustomNode.GetAlphaBlend: Integer;
var
  vMeshes: TsgObjectList;
  vAlpha: Double;
  vAlphaDec: Currency;
begin
  vMeshes := TsgObjectList.Create;
  try
    GetMeshList(vMeshes);
    vAlpha := TsgSupportAlphaBlend.GetAlpha(vMeshes);
    vAlphaDec := vAlpha * 100;
    Result := Trunc(vAlphaDec);
  finally
    vMeshes.Free;
  end;
end;

procedure TCustomNode.SetAlphaBlend(const AValue: Integer);
var
  vRoot: TRoot;
  vMeshLib: TGLMaterialLibrary;
  vMeshes: TsgObjectList;
begin
  vRoot := TRoot(GetRoot);
  if Assigned(vRoot) then
  begin
    vMeshLib := vRoot.F3DDrawingNavigator.Navigator3D.FGLMaterialLibrary;
    vMeshes := TsgObjectList.Create;
    try
      GetMeshList(vMeshes);
      TsgSupportAlphaBlend.SetAlpha(vMeshLib, vMeshes, AValue / 100);
    finally
      vMeshes.Free;
    end;
  end;
end;

procedure TCustomNode.SetCustomColor(const AValue: Cardinal);
var
  vRoot: TRoot;
  vMeshLib: TGLMaterialLibrary;
  vMeshes: TsgObjectList;
  vValue: string;
begin
  vRoot := TRoot(GetRoot);
  if Assigned(vRoot) then
  begin
    vMeshLib := vRoot.F3DDrawingNavigator.Navigator3D.FGLMaterialLibrary;
    vMeshes := TsgObjectList.Create;
    try
      GetMeshList(vMeshes);
      vValue := AssignMaterial(AValue, vMeshLib);
      TsgSupportCustomColor.SetCustomColor(vMeshLib, vMeshes, vValue);
    finally
      vMeshes.Free;
    end;
  end;
end;

procedure TCustomNode.SetDelta(const Value: TAffineVector);
var
  I: Integer;
begin
  FDelta := Value;
  for I := 0 to Count - 1 do
  begin
    TCustomNode(Items[I]).Delta := Value;
  end;
end;

procedure TCustomNode.GetMatrix(out AMatrix: TFMatrix);
begin
  if Owner <> nil then
    Owner.GetMatrix(AMatrix);
  if (Obj <> nil) and (Obj is TsgDXFInsert) then
    AMatrix := FMatXMat(TsgDXFInsertAccess(Obj).FMatrix, AMatrix)
end;

function TCustomNode.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TCustomNode.GetMeasureKoef: Double;
var
  vRoot: TRoot;
begin
  Result := 1;
  vRoot := TRoot(GetRoot);
  if Assigned(vRoot) then
  begin
    Result := vRoot.F3DDrawingNavigator.MeasureScaleFactor;
  end;
end;

procedure TCustomNode.GetMeshList(const AMeshes: TsgObjectList);
var
  I: Integer;
begin
  if not Assigned(AMeshes) then
    Exit;
  if Assigned(FMesh) then
    AMeshes.Add(FMesh);
  for I := 0 to Count - 1 do
    TCustomNode(Items[I]).GetMeshList(AMeshes);
end;

class function TCustomNode.GetNodeUnboxingElenent(ARoot, ANode: TCustomNode;
  const AClasses: array of TClass): TCustomNode;
var
  I: Integer;
  vList: TList;
  vOwner: TCustomNode;
begin
  Result := nil;
  vList := TList.Create;
  try
    vOwner := ANode;
    while Assigned(vOwner) do
    begin
      vList.Add(vOwner);
      vOwner := vOwner.Owner;
    end;
  finally
    for I := vList.Count - 1 downto 0 do
    begin
      if TCustomNode(vList[I]).Obj is TsgModPartCompound then
      begin
        if TsgModPartCompound(TCustomNode(vList[I]).Obj).PartType = ptSheet then
        begin
          Result := TCustomNode(vList[I]);
          Break;
        end;
      end;
      if IsEntity(TCustomNode(vList[I]).Obj, AClasses) then
      begin
        Result := TCustomNode(vList[I]);
        Break;
      end;
    end;
    vList.Free;
  end;
end;

class function TCustomNode.GetNodeOwnerByTopoClass(ARoot, ANode: TCustomNode;
  const AClasses: array of TClass): TCustomNode;
begin
  Result := ANode;
  while Assigned(Result) and (Result <> ARoot) do
  begin
    if IsEntity(Result.Obj, AClasses) then
      Break;
    Result := Result.Owner;
  end;
end;

procedure TCustomNode.NormalizeVisibilityTree;
var
  vRoot: TCustomNode;
  vVisCount, vInVisCount: Integer;

  procedure CheckNodes(const ANode: TCustomNode;
    var AVisCount, AInVisCount: Integer);
  var
    I, vVisCount, vInVisCount: Integer;
  begin
    if ANode.FVisible then
      Inc(AVisCount)
    else
      Inc(AInVisCount);
    vVisCount := 0;
    vInVisCount:= 0;

    for I := 0 to ANode.Count - 1 do
    begin
      CheckNodes(TCustomNode(ANode.Items[I]), vVisCount, vInVisCount);
    end;
    Inc(AVisCount, vVisCount);
    Inc(AInVisCount, vInVisCount);

    //Update visibility if all children are shown or hidden and
    //it doesn't match the current flag
    if ANode.Count > 0 then
    begin
      if ANode.FVisible then
      begin
        if vVisCount = 0 then
        begin
          ANode.FVisible := False;
          Dec(AVisCount);
          Inc(AInVisCount);
        end;
      end
      else
      begin
        if vInVisCount = 0 then
        begin
          ANode.FVisible := True;
          Dec(AInVisCount);
          Inc(AVisCount);
        end;
      end;
    end;
  end;

begin
  vRoot := Self;
  while vRoot.Owner <> nil do
    vRoot := vRoot.Owner;

  vVisCount := 0;
  vInVisCount := 0;
  CheckNodes(vRoot, vVisCount, vInVisCount);
end;

class function TCustomNode.GetPathKeyByNode(const ANode: TCustomNode): String;
var
  I: Integer;
  vOwner: TCustomNode;
  vEntity: TObject;
  vList: TList;
  vIEntity: IsgXMLSupportBuildKey;
  vXMLObject: IsgXMLObject;
begin
  Result := '';
  vList := TList.Create;
  try
    vOwner := ANode;
    while Assigned(vOwner) do
    begin
      vEntity := TObject(TCustomNode(vOwner).FObj);
      vOwner := vOwner.Owner;

      if not Assigned(vEntity) then
      begin
        Continue;
      end;

      if not Supports(vEntity, IsgXMLSupportBuildKey, vIEntity) then
        Continue;

      if vIEntity.IsUsedInKeyConstruction = False then
        Continue;
      if not Supports(vEntity, IsgXMLObject, vXMLObject) then
        Break;

      if Assigned(vEntity) then
        vList.Add(vEntity);
    end;
  finally
    for I := vList.Count - 1 downto 0 do
    begin
      Result := Result + '-'+ IntToStr(UInt64(vList[I]));
    end;
    vList.Free;
  end;
end;

function TCustomNode.GetRoot: TCustomNode;
var
  vOwner, vFind: TCustomNode;
begin
  Result := nil;
  vFind := nil;
  vOwner := Self;
  while Assigned(vOwner) do
  begin
    vFind := vOwner;
    vOwner := vOwner.Owner;
  end;
  if Assigned(vFind) and (vFind is TRoot) then
    Result := vFind;
end;

function TCustomNode.GetVisibleElement: Boolean;
begin
  Result := FVisible;
  if Assigned(FMesh) then
  begin
    //Result := TsgMeshObject(FMesh).Visible;
  end;
end;

function TCustomNode.IsAtomElementForExploder: Boolean;
begin
  Result := False;
  if not Assigned(FObj)  then
    Exit;
  if not (FObj is TsgModObject) then
    Exit;
  if (FObj is TsgModPartCompound) and
     (TsgModPartCompound(FObj).PartType = ptSheet) then
    Result := True
  else if (FObj is TsgModTopoSolid) or (FObj is TsgModTopoShell) then
    Result := True;
end;

function TCustomNode.IsExploded: Boolean;
var
  I: Integer;
begin
  Result := False;
  if not IsEquals(Vect2FPoint(FDelta), NullVector3d, 1e-12) then
    Result := True
  else
    for I := 0 to Count - 1 do
    begin
      if TCustomNode(Items[I]).IsExploded then
      begin
        Result := True;
        Break;
      end;
    end;
end;

procedure TCustomNode.Iterate(const AProc: TsgProcCustomNode);
var
  I: Integer;
begin
  if AProc(Self) = 1 then
  begin
    for I := 0 to Count - 1 do
      TCustomNode(Items[I]).Iterate(AProc);
  end;
end;

procedure TCustomNode.SetVisibleElement(const AValue: Boolean);
var
  I: Integer;
begin
  FVisible := AValue;
//  if Assigned(FMesh) then
//  begin
//    if TsgMeshObject(FMesh).Visible <> FVisible then
//      TsgMeshObject(FMesh).Visible := FVisible;
//  end;
  // Change visible all childs
  for I := 0 to Count - 1 do
    TCustomNode(Items[I]).SetVisibleElement(FVisible);
end;

{ TNode }

function TNodeLite.AddChild(AObj: TObject): TNodeLite;
begin
  Result := TNodeLite.Create(Self);
  Result.Obj := AObj;
  Add(Result);
end;

procedure TNodeLite.CleanEx(const AFreeList: TList;
  const ARecurse: TList; var ADepth: Integer);
var
  I: Integer;
  vDepth: Integer;
begin
  vDepth := ADepth + 1;
  if vDepth > 20 then
  begin
    ARecurse.Add(Self);
    Exit;
  end;
  I := Count - 1;
  while I >= 0 do
  begin
    TNodeLite(List^[I]).CleanEx(AFreeList, ARecurse, vDepth);
    Delete(I);
    I := Count - 1;
  end;
  AFreeList.Add(Self);
end;

procedure TNodeLite.Clean;
var
  I: Integer;
  N: TNodeLite;
begin
  I := Count - 1;
  while I >= 0 do
  begin
    N := TNodeLite(List^[I]);
    Delete(I);
    N.Free;
    I := Count - 1;
  end;
end;

destructor TNodeLite.Destroy;
begin
  Clean;
  inherited Destroy;
end;

function TNodeLite.Find(AObj: TObject; var I: Integer): Boolean;
begin
  Result := inherited Find(AObj, I);
end;

function TNodeLite.FindDeep(AObj: TObject; var AOwner: TNodeLite; var I: Integer;
  const ALevel: Integer): Boolean;
var
  J: Integer;
begin
  Result := False;
  if Find(AObj, I) then
  begin
    Result := True;
    AOwner := Self;
  end;
  if not Result and (ALevel > 0) then
  begin
    J := 0;
    while (J < Count) and not Result do
    begin
      Result := TNodeLite(Items[J]).FindDeep(AObj, AOwner, I, ALevel - 1);
      Inc(J);
    end;
  end;
end;

//function TNodeLite.GetBox: TFRect;
//var
//  vMatrix: TFMatrix;
//begin
//  Result := Inherited GetBox;
//  vMatrix := cnstIdentityMat;
//  GetMatrix(vMatrix);
//  Result := FRectXMat(Result, vMatrix);
//end;

function TNodeLite.GetNodeByKey(AKey: TsgObjectList; AStart: Integer): TNodeLite;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if TCustomNode(Items[I]).Obj = AKey[AStart] then
    begin
      if AStart < AKey.Count - 1 then
        Result := TNodeLite(Items[I]).GetNodeByKey(AKey, AStart + 1)
      else begin
        Result := TNodeLite(Items[I]);
      end;
      Break;
    end;
  end;
end;

function TNodeLite.GetNodeByStrKey(AKey: String): TNodeLite;
var
  vKey: TsgObjectList;
begin
  Result := nil;
  vKey := TsgObjectList.Create;
  try
    TsgSupportPathKey.GetObjectListByPathStrKey(AKey, vKey);
    if vKey.Count > 0 then
    begin
      Result := GetNodeByKey(vKey, 0);
    end;
  finally
    vKey.Free;
  end;
end;

{ TNodeMatrix }

procedure TNodeMatrix.GetMatrix(out AMatrix: TFMatrix);
begin
  AMatrix := FMatrix;
end;

{ TRoot }

function TRoot.SortByExploded(const A, B: Pointer): Integer;
var
  Left, Right: TNodeLite;
  vBoxLeft, vBoxRight: TFRect;
  vCompPointLeft, vCompPointRight: TFPoint;
begin
  Left := TNodeLite(PPointer(A)^);
  Right := TNodeLite(PPointer(B)^);

  vBoxLeft := Left.GetBox(True);
  vBoxRight := Right.GetBox(True);

  vCompPointLeft := GetCenterOfRect(vBoxLeft);
  vCompPointRight := GetCenterOfRect(vBoxRight);

  Result := 0;
  case FTypeExploded of
    1: Result := TsgTypeComparer.CmpFPointByX(vCompPointLeft, vCompPointRight);
    2: Result := TsgTypeComparer.CmpFPointByY(vCompPointLeft, vCompPointRight);
    3: Result := TsgTypeComparer.CmpFPointByZ(vCompPointLeft, vCompPointRight);
  end;

  if Result = 0 then
  begin
    Result := TsgTypeComparer.CmpFPoint(GetSizeFRect(vBoxLeft), GetSizeFRect(vBoxRight));
  end;

end;

class procedure TRoot.SortNodes(const ANode: TPersistentObjectList);
var
  I: Integer;
begin
  ANode.Sort(NodesCompare);
  for I := 0 to ANode.Count - 1 do
    SortNodes(TPersistentObjectList(ANode[I]));
end;

function TRoot.AddNodeToList(const ANode: TCustomNode): Integer;
begin
  Result := 1;
  if Assigned(FNodeList) and ANode.IsAtomElementForExploder then
  begin
    Result := 0;
    FNodeList.Add(ANode);
  end;
end;

procedure TRoot.Clean;
var
  I: Integer;
  vFreeList, vRecurseList, vRecurseList2: TList;
  vDepth: Integer;
begin
  vFreeList := TList.Create;
  vRecurseList := TList.Create;
  vRecurseList2:= TList.Create;
  try
    vDepth := 0;
    CleanEx(vFreeList, vRecurseList, vDepth);

    I := 0;
    while vRecurseList.Count > 0 do
    begin
      vDepth := 0;
      TNodeLite(vRecurseList[I]).CleanEx(vFreeList, vRecurseList2, vDepth);
      Inc(I);
      if I = vRecurseList.Count then
      begin
        if vRecurseList.Count > 0 then
        begin
          vRecurseList.Assign(vRecurseList2);
          vRecurseList2.Clear;
          I := 0;
        end
        else
          vRecurseList.Clear;
      end;
    end;
    for I := 0 to vFreeList.Count - 1 do
    begin
      if vFreeList[I] = Self then
        Continue;
      TNodeLite(vFreeList[I]).Free;
    end;
    if Assigned(FNodeList) then
      FreeAndNil(FNodeList);
  finally
    vFreeList.Free;
    vRecurseList.Free;
    vRecurseList2.Free;
  end;
end;

function TRoot.CreateListByExploded(const AType: Integer;
  const ANewList: Boolean = False): TsgPointerList;
begin
  Result := nil;

  FTypeExploded := AType;

  if ANewList and Assigned(FNodeList) then
  begin
    FNodeList.Free;
    FNodeList := nil;
  end;

  if not Assigned(FNodeList) then
  begin
    FNodeList := TsgPointerList.Create;
    FNodeList.ProcCompare := SortByExploded;
    //Let's expand the tree into a flat list
    Iterate(AddNodeToList);
    FNodeList.Sorted := True;
    Result := FNodeList;
  end
  else
    Result := FNodeList;
end;

procedure TRoot.FreeListByExploded;
begin
end;

function TRoot.GetBoxByExploded: TFRect;
var
  vList: TsgPointerList;
  I: Integer;
  vNode: TCustomNode;
  vBox: TFRect;
begin
  Result := cnstBadRect;
  vList := CreateListByExploded(1, True);
  for I := 0 to vList.Count - 1 do
  begin
    vNode := vList[I];
    if vNode.Visible = False then
      Continue;
    vBox := vNode.GetBox;
    vBox := FRectXMat(vBox, FMatByTranslate(Vect2FPoint(vNode.Delta)));
    ExpandFRect(Result, vBox.BottomRight);
    ExpandFRect(Result, vBox.TopLeft);
  end;
end;

{ TTransformation }

constructor TTransformation.Create(AMatrix: PFMatrix);
begin
  FMatrix := AMatrix;
end;

function TTransformation.Transformf(const APoint: TFPoint): TAffineVector;
begin
  TransformPointTo3f(APoint, FMatrix^, @Result);
end;

function TTransformation.Transform(const APoint: TFPoint): TFPoint;
begin
  Result := FPointXMat(APoint, FMatrix^);
end;

{$IFDEF CLIP_GLCLIP}
{ TsgGLHUDSprite }

procedure TsgGLHUDSprite.DoRender(var rci: TRenderContextInfo; renderSelf,
  renderChildren: Boolean);
begin
  DisableClipPlane(Tsg3DNavigator(Owner.Owner.Owner))  ; //GL.Disable(GL_CLIP_PLANE0);
  inherited DoRender(rci, renderSelf, renderChildren);
  EnableClipPlane(Tsg3DNavigator(Owner.Owner.Owner)) ;//GL.Enable(GL_CLIP_PLANE0);
end;
{$ENDIF}

{ TEntList }

function TEntList.GetBox(Index: Integer): TAABB;
begin
  Result := BoxToAABB(Entities[Index].Box);
end;

function TEntList.GetEntities(Index: Integer): TsgDXFEntity;
begin
  Result := List[Index];
end;

procedure TEntList.SetEntities(Index: Integer; const Value: TsgDXFEntity);
begin
  List[Index] := Value;
end;

{ TProgress }

type
  TProgressUpdateThread = class(TThread)
  protected
    FPogress: TProgress;
    FSync: Boolean;
    procedure SyncProc;
  public
    procedure Execute; override;
  end;

procedure TProgressUpdateThread.SyncProc;
begin
  if Assigned(FPogress) then
    FPogress.DoProgressInternal(psRunning);
end;

procedure TProgressUpdateThread.Execute;
begin
  while Assigned(FPogress) and (FPogress.FUpdateDelay > 0) do
  begin
    Sleep(FPogress.FUpdateDelay);
    FSync := True;
    Synchronize(SyncProc);
    FSync := False;
  end;
end;

procedure TProgress.Progress;
begin
  DoProgress(psRunning);
end;

procedure TProgress.SetCaption(const AValue: TCaption);
begin
  FCaption := AValue;
end;

procedure TProgress.SetUpdateDelay(const Value: Cardinal);
begin
  FUpdateDelay := Value;
end;

procedure TProgress.Starting;
begin
  if FUpdateThread = nil then
  begin
    FUpdateThread := TProgressUpdateThread.Create(False);
    FUpdateThread.FreeOnTerminate := True;
    FUpdateThread.OnTerminate := ThreadTerminate;
  end;
  TProgressUpdateThread(FUpdateThread).FPogress := Self;
  DoProgress(psStarting);
end;

procedure TProgress.ThreadTerminate(Sender: TObject);
begin
  if Sender = FUpdateThread then
    FUpdateThread := nil;
end;

procedure TProgress.UpdateParams(const APercentDone, AStep: Extended);
begin
  if Assigned(FUpdateThread) then
    TProgressUpdateThread(FUpdateThread).FPogress := nil;
  FPercentDone := APercentDone;
  FSendPercentDone := Round(FPercentDone) - 1;
  FStep := AStep;
end;

procedure TProgress.Assign(Source: TPersistent);
begin
  if Source is TProgress then
  begin
    if Assigned(FUpdateThread) then
      TProgressUpdateThread(FUpdateThread).FPogress := nil;
    FCaption := TProgress(Source).FCaption;
    FPercentDone := TProgress(Source).FPercentDone;
    FStep := TProgress(Source).FStep;
    FProgressSender := TProgress(Source).FProgressSender;
    FUpdateDelay := TProgress(Source).FUpdateDelay;
    FSendPercentDone := TProgress(Source).FSendPercentDone;
  end
  else
    inherited Assign(Source);
end;

destructor TProgress.Destroy;
begin
  if Assigned(FUpdateThread) then
  begin
    TProgressUpdateThread(FUpdateThread).FPogress := nil;
    TProgressUpdateThread(FUpdateThread).OnTerminate := nil;
  end;
  FUpdateThread := nil;
  inherited Destroy;
end;

procedure TProgress.DoProgress(Stage: TProgressStage);
begin
  FPercentDone := FPercentDone + FStep;
  if FPercentDone > 100 then FPercentDone := 100;
  if (FPercentDone - FSendPercentDone >= 1.0) or (Stage in [psStarting, psEnding]) then
  begin
    FSendPercentDone := Round(FPercentDone);
    DoProgressInternal(Stage);
  end
  else
    if Assigned(FUpdateThread) and (TProgressUpdateThread(FUpdateThread).FSync) then
    begin
{$IFDEF SGDEL_XE}
      TProgressUpdateThread(FUpdateThread).Yield;
{$ELSE}
{$IFDEF POSIX}
      sched_yield;
{$ENDIF}
{$IFDEF MSWINDOWS}
    {$IFNDEF SGFPC}SwitchToThread;{$ENDIF}
{$ENDIF}
{$ENDIF}
      DoProgressInternal(Stage);
{$IFDEF CS_USEFORM}
      Application.ProcessMessages;
{$ENDIF}
    end;
end;

procedure TProgress.Ending;
begin
  if Assigned(FUpdateThread) then
    TProgressUpdateThread(FUpdateThread).FPogress := nil;
  DoProgress(psEnding);
end;

procedure TProgress.DoProgressInternal(Stage: TProgressStage);
{$IFDEF SGFPC}
var Continue : Boolean;
{$ENDIF}
begin
  if Assigned(FProgressEvent) then
  begin
    {$IFDEF SGFPC}
    Continue := True;
    {$ENDIF}
    FProgressEvent(ProgressSender, Stage, FSendPercentDone, False,
        cnstRectZero, Caption{$IFDEF SGFPC}, Continue{$ENDIF});
  end;
end;

{ TsgMeshObjectList }

procedure TsgMeshObjectList.BuildList(var mrci: TRenderContextInfo);
var
  i: Integer;

  objList: TPersistentObjectList;
  distList: TSingleList;
  plist: PPointerObjectList;
  obj: TMeshObject;
  vTmp: TVector4f;
  vCameraPosition: TVector3f;

begin
  if mrci.objectsSorting = osRenderBlendedLast then
  begin
    distList := TSingleList.Create;
    objList := TPersistentObjectList.Create;
    distList.GrowthDelta := Count ; // no reallocations
    objList.GrowthDelta := distList.GrowthDelta;
    try
      vTmp := VectorTransform(mrci.cameraPosition, mrci.PipelineTransformation.InvModelMatrix);
      MakeVector(vCameraPosition, vTmp.X, vTmp.Y, vTmp.Z);
      // render opaque stuff
      for i := 0 to Count - 1 do
      begin
        obj := TMeshObject(Items[i]);
        if Assigned(Obj) and obj.Visible then
        begin
          if obj.Mode <> momFaceGroups then
            obj.BuildList(mrci)
          else
          begin
            if obj.FaceGroups.Count = 0 then
              obj.BuildList(mrci)
            else
            if obj.FaceGroups[0].MaterialName = '' then
              obj.BuildList(mrci)
            else
            begin
              if SysUtils.StrPos(PChar(@obj.FaceGroups[0].MaterialName[1]), PChar(@cnstAlphaPref[1])) = nil then
                obj.BuildList(mrci)
              else
              begin
                objList.Add(obj);
                distList.Add(1 +
                  TsgMinBox.BoxPointSqrDistanceTo(obj, vCameraPosition));
              end;
            end;
          end;
        end;
      end;
      if distList.Count > 0 then
      begin
        if distList.Count > 1 then
          FastQuickSortLists(0, distList.Count - 1, distList, objList);
        plist := objList.List;
        for i := objList.Count - 1 downto 0 do
          TMeshObject(plist^[i]).BuildList(mrci);
      end;
    finally
      objList.Free;
      distList.Free;
    end;
  end
  else
  begin
    for i := 0 to Count - 1 do
      with Items[i] do
        if Visible then
          BuildList(mrci);
  end;


end;


{ TsgVectorGLScene }

procedure TsgVectorGLScene.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is  TsgVectorGLScene then
    Navigator := TsgVectorGLScene(Source).Navigator;
end;

procedure TsgVectorGLScene.LoadFromFile(const FileName: string);
begin
  inherited LoadFromFile(FileName);
  if Assigned(Navigator) then
    Navigator.LoadFromFile(FileName);
end;

{ TsgConverterNav3d }

function TsgConverterNav3d.Convert: Integer;
var
  vImage: TsgCADImageAccess;
begin
  Result := 0;
  if (Destination is TsgDrawingNavigator) and (Source is TsgCADImage) then
  begin
    vImage := TsgCADImageAccess(Source);
    try
      TsgDrawingNavigator(Destination).LoadFromConverter(vImage.Converter, vImage);
      Result := Ord(not TsgDrawingNavigator(Destination).Empty);
    except
      Result := -1;
    end;
  end;
end;

{ TsgInsertStack }

function TsgInsertStack.GetPath: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + '-' + '$' + IntToHex(UInt64(Items[I]), 0);
end;

function TsgInsertStack.Pop: TObject;
begin
  Result := nil;
  if Count > 0 then
  begin
    Result := TObject(Items[Count - 1]);
    Count := Count - 1;
  end;
end;

procedure TsgInsertStack.Push(const AObject: TObject);
begin
  Add(Pointer(AObject));
end;

{ TsgSupportAlphaBlend }

class function TsgSupportAlphaBlend.GetAlpha(
  const AMeshes: TsgObjectList): Double;
var
  vAlpha: Double;
  I: Integer;
  vPolyline: TsgGLPolyline;
  vMesh: TsgMeshObject;

  function GetAlpha(const AMatName: string): Double;
  var
    vPosAlpha: Integer;
    vAlpha: string;
  begin
    vPosAlpha := StringPos(cnstAlphaPref, AMatName, 1);
    Result := 1;
    if vPosAlpha > 0 then // Alpha already set
    begin
      vAlpha := Copy(AMatName, vPosAlpha + Length(cnstAlphaPref), Length(AMatName));
      Result := StrToInt(vAlpha)/100;
    end;
  end;

begin
  Result := -1;
  vAlpha := 0;
  for I := 0 to AMeshes.Count - 1 do
  begin
    if not Assigned(AMeshes[I]) then
      Continue;
    if TObject(AMeshes[I]).ClassType = TsgGLPolyline then
    begin
      vPolyline := TsgGLPolyline(AMeshes[I]);
      vAlpha := GetAlpha(vPolyline.MaterialName);
    end
    else
    if TObject(AMeshes[I]).ClassType = TsgPickObject then
    begin
      vAlpha := TsgPickObject(AMeshes[I]).GetAlpha;
    end
    else
    if TObject(AMeshes[I]).ClassType = TsgMeshObject then
    begin
      vMesh := TsgMeshObject(AMeshes[I]);
      if vMesh.IsUseVisibleData then
        Continue;
      vAlpha := GetAlpha(vMesh.FaceGroups[0].MaterialName)
    end;

    if (Result = -1) and (Result <> vAlpha) then
    begin
      Result := vAlpha;
    end
    else
      if (Result <> vAlpha) then
      begin
        Result := -1;
        Break;
      end;
  end;
end;

class procedure TsgSupportAlphaBlend.SetAlpha(
  const AMeshLib: TGLMaterialLibrary; const AMeshes: TsgObjectList;
  const AValue: Double);
var
  I: Integer;
  vPolyline: TsgGLPolyline;
  vMesh: TsgMeshObject;
  vAlphaName: string;

  function GetAlphaMaterial(const AMatName: string; const Alpha: Double;
    const AGLMaterialLibrary: TGLMaterialLibrary): string;
  var
    vMatLibItem, vMatAlphaLibItem: TGLLibMaterial;
    vPosAlpha: Integer;
    vWithoutAlpha, vAlphaMatName: string;
    vAlphaPack: Byte;
    vAlphaDec: Currency;
  begin
    Result := '';
    if Assigned(AGLMaterialLibrary) then
    begin
      vPosAlpha := StringPos(cnstAlphaPref, AMatName, 1);
      vWithoutAlpha := AMatName;
      if vPosAlpha > 0 then // Alpha already set
        vWithoutAlpha := Copy(AMatName, 1, vPosAlpha - 1);
      vMatLibItem := AGLMaterialLibrary.Materials.GetLibMaterialByName(vWithoutAlpha);
      if Assigned(vMatLibItem) then
      begin
        if (Alpha >= 0.0) and (Alpha < 1.0) then
        begin
          vAlphaDec := Alpha * 100;
          vAlphaPack := Trunc(vAlphaDec);
          vAlphaMatName := vWithoutAlpha + cnstAlphaPref +IntToStr(vAlphaPack);
          vMatAlphaLibItem := AGLMaterialLibrary.Materials.GetLibMaterialByName(vAlphaMatName);
          if not Assigned(vMatAlphaLibItem) then
          begin
            vMatAlphaLibItem := AGLMaterialLibrary.Materials.Add;
            vMatAlphaLibItem.Material.Assign(vMatLibItem.Material);
            vMatAlphaLibItem.Name := vAlphaMatName;
          end;
          Result := vMatAlphaLibItem.Name;
          vMatAlphaLibItem.Material.BlendingMode := bmTransparency;
          vMatAlphaLibItem.Material.DepthProperties.DepthTest := True;
          vMatAlphaLibItem.Material.MaterialOptions := [moIgnoreFog, moNoLighting];
          //vMatAlphaLibItem.Material.FaceCulling := fcCull;
          vMatAlphaLibItem.Material.FrontProperties.Diffuse.Alpha := Alpha;
          vMatAlphaLibItem.Material.FrontProperties.Specular.Alpha := Alpha;
          vMatAlphaLibItem.Material.BackProperties.Diffuse.Alpha := Alpha;
          vMatAlphaLibItem.Material.BackProperties.Specular.Alpha := Alpha;
        end
        else
          Result := vWithoutAlpha;
      end;
    end;
  end;

begin
  for I := 0 to AMeshes.Count - 1 do
  begin
    if not Assigned(AMeshes[I]) then
      Continue;
    if TObject(AMeshes[I]).ClassType = TsgGLPolyline then
    begin
      vPolyline := TsgGLPolyline(AMeshes[I]);
      vPolyline.MaterialName := GetAlphaMaterial(vPolyline.MaterialName,
        AValue, AMeshLib);
    end
    else
    if TObject(AMeshes[I]).ClassType = TsgPickObject then
    begin
      TsgPickObject(AMeshes[I]).SetAlpha(AValue)
    end
    else
    if TObject(AMeshes[I]).ClassType = TsgMeshObject then
    begin
      vMesh := TsgMeshObject(AMeshes[I]);
      if vMesh.IsUseVisibleData then
        Continue;
      vAlphaName := GetAlphaMaterial(vMesh.FaceGroups[0].MaterialName,
        AValue, AMeshLib);
      SetMeshMaterial(vMesh, vAlphaName, AMeshLib);
    end;
  end;
end;

{ TsgSupportCustomColor }

class function TsgSupportCustomColor.GetCustomColor(const AMeshes: TsgObjectList): string;
var
  vCustomColor: string;
  I: Integer;
  vPolyline: TsgGLPolyline;
  vMesh: TsgMeshObject;
  vPosAlpha: Integer;
  vWithoutAlpha, vAlphaMatName: string;

begin
  Result := '';
  vCustomColor := '';
  for I := 0 to AMeshes.Count - 1 do
  begin
    if not Assigned(AMeshes[I]) then
      Continue;
    if TObject(AMeshes[I]).ClassType = TsgGLPolyline then
    begin
      vPolyline := TsgGLPolyline(AMeshes[I]);
      vCustomColor := vPolyline.MaterialName;
    end
    else
    if TObject(AMeshes[I]).ClassType = TsgPickObject then
    begin
      //vAlpha := TsgPickObject(AMeshes[I]).GetAlpha;
    end
    else
    if TObject(AMeshes[I]).ClassType = TsgMeshObject then
    begin
      vMesh := TsgMeshObject(AMeshes[I]);
      if vMesh.IsUseVisibleData then
        Continue;

      vAlphaMatName := vMesh.FaceGroups[0].MaterialName;
      vPosAlpha := StringPos(cnstAlphaPref, vAlphaMatName, 1);
      vWithoutAlpha := vAlphaMatName;
      if vPosAlpha > 0 then // Alpha already set
        vWithoutAlpha := Copy(vAlphaMatName, 1, vPosAlpha - 1);
      vCustomColor := vWithoutAlpha;
    end;

    if (Result = '') and (Result <> vCustomColor) then
    begin
      Result := vCustomColor;
    end
    else
      if (Result <> vCustomColor) then
      begin
        Result := '';
        Break;
      end;
  end;

end;

class procedure TsgSupportCustomColor.SetCustomColor(const AMeshLib: TGLMaterialLibrary;
  const AMeshes: TsgObjectList;
  const AValue: string);
var
  I: Integer;
  vPolyline: TsgGLPolyline;
  vMesh: TsgMeshObject;
  vAlphaName: string;

begin
  for I := 0 to AMeshes.Count - 1 do
  begin
    if not Assigned(AMeshes[I]) then
      Continue;
    if TObject(AMeshes[I]).ClassType = TsgGLPolyline then
    begin
      vPolyline := TsgGLPolyline(AMeshes[I]);
      vPolyline.MaterialName := AValue;
    end
    else
    if TObject(AMeshes[I]).ClassType = TsgPickObject then
    begin
      //TsgPickObject(AMeshes[I]).SetAlpha(AValue)
    end
    else
    if TObject(AMeshes[I]).ClassType = TsgMeshObject then
    begin
      vMesh := TsgMeshObject(AMeshes[I]);
      if vMesh.IsUseVisibleData then
        Continue;
      SetMeshMaterial(vMesh, AValue, AMeshLib);
    end;
  end;
end;

{ TsgMinBox }

class function TsgMinBox.BoxPointSqrDistanceTo(const AMesh: TMeshObject;
  const APt: TVector3f): Single;
var
  vMin, vMax: TVector3f;
begin
  AMesh.GetExtents(vMin, vMax);
  Result := BoxPointSqrDistanceTo(vMin, vMax, APt);
end;

class function TsgMinBox.BoxPointSqrDistanceTo(AMin, AMax: TFPoint;
  const APt: TFPoint): Single;
var
  vMin, vMax, vPt: TVector3f;
begin
  vMin := FPoint2Vect(AMin);
  vMax := FPoint2Vect(AMax);
  vPt := FPoint2Vect(APt);
  Result := BoxPointSqrDistanceTo(vMin, vMax, vPt);
end;

class function TsgMinBox.BoxPointSqrDistanceTo(AMin, AMax: TVector3f;
  const APt: TVector3f): Single;
var
  I: Integer;
  vTemp: Single;
  v: array[0..1] of TVector3f;
begin
  v[0] := AMin;
  v[1] := AMax;
  SubtractVector(v[0], APt);
  SubtractVector(v[1], APt);
  Result := VectorNorm(VectorMake(v[0].V[0], v[0].V[1], v[0].V[2]));
  for I := 1 to 7 do
  begin
    vTemp := VectorNorm(VectorMake(v[(I shr 2) mod 2].V[0], v[(I shr 1) mod 2].V[1], v[I mod 2].V[2]));
    if vTemp < Result then
      Result := vTemp;
  end;
end;

class function TsgMinBox.BoxPointSqrDistanceTo(const ANode: TCustomNode;
  const APt: TFPoint): Single;
var
  vBox: TFRect;
  vMin, vMax, vPt: TVector3f;
begin
  vBox := ANode.GetBox;
  SwapDoubles(vBox.Top, vBox.Bottom);
  vMin := FPoint2Vect(vBox.TopLeft);
  vMax := FPoint2Vect(vBox.BottomRight);
  vPt := FPoint2Vect(APt);
  Result := BoxPointSqrDistanceTo(vMin, vMax, vPt);
end;

procedure InitConstants;
begin
{$IFDEF SG_SHARED_VCL}
  cnstDim3DVCL := cnstDefHeadVarStruct.DimProps;
  with cnstDim3DVCL do
  begin
    Alt := False;
    AltF := 1.0;
    APost := '';
    Asz := 14;
    Sah := False;
    Arrows.Blk := datClosedblank;
    Arrows.Blk1 := datClosedblank;
    Arrows.Blk2 := datClosedblank;
    Arrows.LrBlk := datClosedblank;
    Cen := 0.09;
    ClrD := cnstColorCADByBlock;
    ClrE := cnstColorCADByBlock;
    ClrT := cnstColorCADByBlock;
    SD1 := False;
    SD2 := False;
    SE1 := False;
    SE2 := False;
    Dec := 2;//change
    Exe := 4;
    Exo := 0;
    Gap := 4;
    LFac := 1;//change
    LwD := -1.0;
    LwE := -1.0;
    Post := '';
    Scale := 1.0;//change
    Tad := 0;
    Tih := False;
    Tix := 0;
    Toh := False;
    Txt := 14;
  end;
{$ENDIF}
end;

{$IFNDEF SGFPC}
{$IFDEF SG_CPUX64}
var
  ArithmeticExceptionMask: TArithmeticExceptionMask;
{$ENDIF}
{$ENDIF}
initialization
  InitConstants();
{$IFNDEF SGFPC}
{$IFDEF SG_CPUX64}
  ArithmeticExceptionMask := GetExceptionMask;
  if not (exInvalidOp in ArithmeticExceptionMask) then
  begin
    Include(ArithmeticExceptionMask, exInvalidOp);
    SetExceptionMask(ArithmeticExceptionMask);
  end;
{$ENDIF}
{$ENDIF}
  Lock := TCriticalSection.Create;
{$IFDEF USE_CLIP}
  RegisterClasses([TsgMeshObject]);
{$ENDIF}
{$IFDEF SG_ALL_3D_FILES}
  GetVectorFileFormats.Remove(GLFileVRML.TGLVRMLVectorFile);
{$ENDIF}

finalization
  DefBlendingParams.Free;
  Solid3DFileExts.Free;
  Lock.Free;
end.

