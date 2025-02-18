{************************************************************}
{   CAD.VCL Cross Platform Library  for Delphi / Lazarus     }
{                                                            }
{                   W2D files TGrapic class                  }
{                                                            }
{     Copyright (c) 2002 - 2020 SoftGold software company    }
{                       CADSoftTools™                        }
{************************************************************}

unit W2D;
{$INCLUDE SGDXF.inc}
{$IFNDEF SG_SHARED_VCL}
  {$DEFINE SG_USE_PNG}
  {$DEFINE SG_USE_TIFF}
{$ENDIF}

//{$DEFINE SG_W2D_DEBUG}
{$IFDEF SG_W2D_DEBUG}
  {$DEFINE SG_W2D_DEBUG_FILE}
  {.$DEFINE SG_W2D_DEBUG_OUTPUTDEBUGSTRING}
{$ENDIF}
{
  This source code was written according WHIP! Data Specification
  by Autodesk, Inc. (Copyright © 2000-2003).
  All related documentation are available at URL <www.autodesk.com>
}
interface

uses
{$IFNDEF SG_NON_WIN_PLATFORM}
  Windows,
{$ENDIF}
{$IFDEF SGFPC}
  LCLIntf, LCLType, FPimage,
{$ENDIF}
  SysUtils, Classes,
{$IFDEF SG_FIREMONKEY}
  {$IFDEF SG_FM_MOBILE}
    {$DEFINE SG_CPUX64}
  {$ENDIF}
  FMX.Graphics, sgFMXTypes, sgProxyGraphics, FMX.Surfaces,
{$ELSE}
  Graphics,
{$ENDIF}
  Math, sgConsts, DXFConv, CADImage,
  sgFunction, sgLists
{$IFDEF SGDEL_XE2}
  , System.Types, System.UITypes
{$ELSE}
{$IFDEF SGDEL_6}
  , Types
{$ENDIF}
{$ENDIF}
{$IFDEF SG_OPENING_IN_THEADS}
  ,SyncObjs
{$ENDIF}
;

type
{ dwf format exceptions}

  TFileFormatError = (ffeBadExtension, ffePackageFormatErr, ffeW2DFormatErr,
    ffeXMLFormatErr);

  TCmndFormatError = (cfeUnknownSingleByteCmndCode, cfeInternalError,
    cfeUnknownExtByteCmndCode, cfeExtByteCmndCodeFormatErr,
    cfeUnknownExtASCIICmndCode, cfeExtASCIICmndCodeFormatErr);

  EFormatError = class(Exception)
    ErrorCode: TFileFormatError;
  end;

  ECmndError = class(Exception)
    ErrorCode: TCmndFormatError;
    Addr: Integer;
  end;

  PsgEtBounding = ^TsgEtBounding;
  TsgEtBounding = record
    Point1: TFPoint;
    Point2: TFPoint;
  end;

  TsgEtMallocAction = procedure(ASize: Integer; AUserData: Pointer);
  TsgEtFreeAction = procedure(APtr: Pointer; AUserData: Pointer);
  TsgEtNewVertexAction = procedure(A, B, C: Integer; AUserData: Pointer);

  PsgEbDecompressConfigs = ^TsgEbDecompressConfigs;
  TsgEbDecompressConfigs = record
    bounding: PsgEtBounding;
    malloc_action: TsgEtMallocAction;
    free_action: TsgEtFreeAction;
    new_vertex_action: TsgEtNewVertexAction;
    user_data: Pointer;
  end;

  PsgMTableInfo = ^TsgMTableInfo;
  TsgMTableInfo = record
    flags: Integer;

    mlengths: PInteger;
    mlengths_used: Integer;
    mlengths_allocated: Integer;

    m2stackoffsets: PInteger;
    m2stackoffsets_used: Integer;
    m2stackoffsets_allocated: Integer;

    m2gateoffsets: PInteger;
    m2gateoffsets_used: Integer;
    m2gateoffsets_allocated: Integer;

    dummies: PInteger;
    dummies_used: Integer;
    dummies_allocated: Integer;

    patches: PInteger;
    patches_used: Integer;
    patches_allocated: Integer;

    bounding: PsgEtBounding;

    x_quantization: Integer;
    y_quantization: Integer;
    z_quantization: Integer;

    x_quantization_normals: Integer;
    y_quantization_normals: Integer;
    z_quantization_normals: Integer;
  end;

  PsgHalfEdge = ^TsgHalfEdge;
  TsgHalfEdge = record
    start: Integer;
    twin: Integer;
  end;

  PsgHalfEdgeArray = ^TsgHalfEdgeArray;
  TsgHalfEdgeArray = record
    edges: PsgHalfEdge;
    allocated: Integer;
    used: Integer;
    visitations: PInteger;

    visitations_used: Integer;
  end;

  TsgByteArray = array[0..MaxInt div SizeOf(Byte) - 1] of Byte;
  PsgByteArray = ^TsgByteArray;
  TsgHEArray = array[0..MaxInt div SizeOf(TsgHalfEdge) - 1] of TsgHalfEdge;
  PsgHEArray = ^TsgHEArray;

  PsgEdgebreakerHeader0 = ^TsgEdgebreakerHeader0;
  TsgEdgebreakerHeader0 = record
    scheme: Byte;
    mtable_scheme: Byte;
    points_scheme: Byte;
    normals_scheme: Byte;
    opslen: Integer;
    mtablelen: Integer;
    pointslen: Integer;
    pcount: Integer;
  end;

  PsgEdgebreakerHeader1 = ^TsgEdgebreakerHeader1;
  TsgEdgebreakerHeader1 = record
    v0: TsgEdgebreakerHeader0;
    normalslen: Integer;
  end;

  PsgEdgebreakerHeader = ^TsgEdgebreakerHeader;
  TsgEdgebreakerHeader = record
    scheme: Byte;
    mtable_scheme: Byte;
    points_scheme: Byte;
    normals_scheme: Byte;
    opslen: Integer;
    mtablelen: Integer;
    pointslen: Integer;
    pcount: Integer;
    normalslen: Integer;
  end;

  TsgProcImageLoad = procedure(const AMem: TMemoryStream; const ABmp: TBitmap);
  TsgGraphicFormat = TsgExportFormat;


{ w2d graphics }

const
  SW2DImage = 'W2D image';

  sgEmptyProgressRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

  DefaultColorMap : array [0 .. 256 * 4 - 1] of Byte = (0, 0, 0, 255, 128, 0, 0,
    255, 0, 128, 0, 255, 128, 128, 0, 255, 0, 0, 128, 255, 128, 0, 128, 255, 0,
    128, 128, 255, 192, 192, 192, 255, 192, 220, 192, 255, 166, 202, 240, 255,
    0, 0, 0, 255, 0, 0, 51, 255, 0, 0, 102, 255, 0, 0, 153, 255, 0, 0, 204, 255,
    0, 0, 255, 255, 0, 51, 0, 255, 0, 51, 51, 255, 0, 51, 102, 255, 0, 51, 153,
    255, 0, 51, 204, 255, 0, 51, 255, 255, 0, 102, 0, 255, 0, 102, 51, 255, 0,
    102, 102, 255, 0, 102, 153, 255, 0, 102, 204, 255, 0, 102, 255, 255, 0, 153,
    0, 255, 0, 153, 51, 255, 0, 153, 102, 255, 0, 153, 153, 255, 0, 153, 204,
    255, 0, 153, 255, 255, 0, 204, 0, 255, 0, 204, 51, 255, 0, 204, 102, 255, 0,
    204, 153, 255, 0, 204, 204, 255, 0, 204, 255, 255, 0, 255, 0, 255, 0, 255,
    51, 255, 0, 255, 102, 255, 0, 255, 153, 255, 0, 255, 204, 255, 0, 255, 255,
    255, 51, 0, 0, 255, 51, 0, 51, 255, 51, 0, 102, 255, 51, 0, 153, 255, 51, 0,
    204, 255, 51, 0, 255, 255, 51, 51, 0, 255, 51, 51, 51, 255, 51, 51, 102,
    255, 51, 51, 153, 255, 51, 51, 204, 255, 51, 51, 255, 255, 51, 102, 0, 255,
    51,102,51, 255, 51, 102, 102, 255, 51, 102, 153, 255, 51, 102, 204, 255, 51,
    102, 255, 255, 51, 153, 0, 255, 51, 153, 51, 255, 51, 153, 102, 255, 51,
    153, 153, 255, 51, 153, 204, 255, 51,153, 255, 255, 51, 204, 0, 255, 51,
    204, 51, 255, 51, 204, 102, 255, 51, 204, 153, 255, 51, 204, 204, 255, 51,
    204, 255, 255, 51, 255, 0, 255, 51, 255, 51, 255, 51, 255, 102, 255, 51,
    255, 153, 255, 51, 255, 204, 255, 51, 255, 255, 255, 102, 0, 0, 255, 102, 0,
    51, 255, 102, 0, 102, 255, 102, 0, 153, 255, 102, 0, 204, 255, 102, 0, 255,
    255, 102, 51, 0, 255, 102, 51, 51, 255, 102, 51, 102, 255, 102, 51, 153,
    255, 102, 51, 204, 255, 102, 51, 255, 255, 102, 102, 0, 255, 102, 102,51,
    255, 102, 102, 102, 255, 102, 102, 153, 255, 102, 102, 204, 255, 102, 102,
    255, 255, 102, 153, 0, 255, 102, 153, 51, 255, 102, 153, 102, 255, 102, 153,
    153, 255, 102, 153, 204, 255, 102, 153, 255, 255, 102, 204, 0, 255, 102,
    204, 51, 255, 102, 204, 102, 255, 102, 204, 153, 255, 102, 204, 204, 255,
    102, 204, 255, 255, 102, 255, 0, 255, 102, 255,51, 255, 102, 255, 102, 255,
    102, 255, 153, 255, 102, 255, 204, 255, 102, 255, 255, 255, 153, 0, 0, 255,
    153, 0, 51, 255, 153, 0, 102, 255, 153, 0, 153, 255, 153, 0, 204, 255, 153,
    0, 255, 255, 153, 51, 0, 255, 153, 51,51, 255, 153, 51, 102, 255, 153, 51,
    153, 255, 153, 51, 204, 255, 153, 51, 255, 255, 153, 102, 0, 255, 153, 102,
    51, 255, 153, 102, 102, 255, 153, 102, 153, 255, 153, 102, 204, 255, 153,
    102, 255, 255, 153, 153, 0, 255, 153, 153, 51, 255, 153, 153, 102, 255, 153,
    153, 153, 255, 153, 153, 204, 255, 153, 153, 255, 255, 153, 204, 0, 255,
    153, 204, 51, 255, 153, 204, 102, 255, 153, 204, 153, 255, 153, 204, 204,
    255, 153, 204, 255, 255, 153, 255, 0, 255, 153, 255, 51, 255, 153, 255, 102,
    255, 153, 255, 153, 255, 153, 255, 204, 255, 153, 255, 255, 255, 204, 0, 0,
    255, 204, 0, 51, 255, 204, 0, 102, 255, 204, 0, 153, 255, 204, 0, 204, 255,
    204, 0, 255, 255, 204, 51, 0, 255, 204, 51, 51, 255, 204, 51, 102, 255, 204,
    51, 153, 255, 204, 51, 204, 255, 204, 51, 255, 255, 204, 102, 0, 255, 204,
    102, 51, 255, 204, 102, 102, 255, 204, 102, 153, 255, 204, 102, 204, 255,
    204, 102, 255, 255, 204, 153, 0, 255, 204, 153, 51, 255, 204, 153, 102, 255,
    204, 153, 153, 255, 204, 153, 204, 255, 204, 153, 255, 255, 204, 204, 0,
    255, 204, 204, 51, 255, 204, 204, 102, 255, 204, 204, 153, 255, 204, 204,
    204, 255, 204, 204, 255, 255, 204, 255, 0, 255, 204, 255, 51, 255, 204, 255,
    102, 255, 204, 255, 153, 255, 204, 255, 204, 255, 204, 255, 255, 255, 255,
    0, 0, 255, 255, 0, 51, 255, 255, 0, 102, 255, 255, 0, 153, 255, 255, 0, 204,
    255, 255, 0, 255, 255, 255, 51, 0, 255, 255, 51, 51, 255, 255, 51, 102, 255,
    255, 51, 153, 255, 255, 51, 204, 255, 255, 51, 255, 255, 255, 102, 0, 255,
    255, 102, 51, 255, 255, 102, 102, 255, 255, 102, 153, 255, 255, 102, 204,
    255, 255, 102, 255, 255, 255, 153, 0, 255, 255, 153, 51, 255, 255, 153, 102,
    255, 255, 153, 153, 255, 255, 153, 204, 255, 255, 153, 255, 255, 255, 204,
    0, 255, 255, 204, 51, 255, 255, 204, 102, 255, 255, 204, 153, 255, 255, 204,
    204, 255, 255, 204, 255, 255, 255, 255, 0, 255, 255, 255, 51, 255, 255, 255,
    102, 255, 255, 255, 153, 255, 255, 255, 204, 255, 255, 255, 255, 255, 0, 0,
    0, 255, 13, 13, 13, 255, 26, 26, 26, 255, 40, 40, 40, 255, 53, 53, 53, 255,
    67, 67, 67, 255, 80, 80, 80, 255, 93, 93, 93, 255, 107, 107, 107, 255, 120,
    120, 120, 255, 134, 134, 134, 255, 147, 147, 147, 255, 161, 161, 161, 255,
    174, 174, 174, 255, 187, 187, 187, 255, 201, 201, 201, 255, 214, 214, 214,
    255, 228, 228, 228, 255, 241, 241, 241, 255, 255, 255, 255, 255, 255, 251,
    240, 255, 160, 160, 164, 255, 128, 128,128, 255, 255, 0, 0, 255, 0, 255, 0,
    255, 255, 255, 0, 255, 0, 0, 255, 255, 255, 0, 255, 255, 0, 255, 255, 255,
    255, 255, 255, 255);

  NewDefaultColorMap : array [0 .. 256 * 4 - 1] of Byte = (
   0, 0, 0, 255, 128, 0, 0, 255, 0, 128, 0, 255, 128, 128, 0, 255, 0, 0, 128,
   255, 128, 0, 128, 255, 0, 128, 128, 255, 192, 192, 192, 255, 192, 220, 192,
   255, 166, 202, 240, 255, 0, 0, 0, 255, 0, 0, 51, 255, 0, 0, 102, 255, 0, 0,
   153, 255, 0, 0, 204, 255, 0, 0, 255, 255, 0, 51, 0, 255, 0, 51, 51, 255, 0,
   51, 102, 255, 0, 51, 153, 255, 0, 51, 204, 255, 0, 51, 255, 255, 0, 102, 0,
   255, 0, 102, 51, 255, 0, 102, 102, 255, 0, 102, 153, 255, 0, 102, 204, 255,
   0, 102, 255, 255, 0, 153, 0, 255, 0, 153, 51, 255, 0, 153, 102, 255, 0, 153,
   153, 255, 0, 153, 204, 255, 0, 153, 255, 255, 0, 204, 0, 255, 0, 204, 51, 255,
   0, 204, 102, 255, 0, 204, 153, 255, 0, 204, 204, 255, 0, 204, 255, 255, 0,
   255, 0, 255, 0, 255, 51, 255, 0, 255, 102, 255, 0, 255, 153, 255, 0, 255,
   204, 255, 0, 255, 255, 255, 51, 0, 0, 255, 51, 0, 51, 255, 51, 0, 102, 255,
   51, 0, 153, 255, 51, 0, 204, 255, 51, 0, 255, 255, 51, 51, 0, 255, 51, 51,
   51, 255, 51, 51, 102, 255, 51, 51, 153, 255, 51, 51, 204, 255, 51, 51,
   255, 255, 51, 102, 0, 255, 51, 102, 51, 255, 51, 102, 102, 255, 51, 102, 153,
   255, 51, 102, 204, 255, 51, 102, 255, 255, 51, 153, 0, 255, 51, 153, 51, 255,
   51, 153, 102, 255, 51, 153, 153, 255, 51, 153, 204, 255, 51, 153, 255, 255,
   51, 204, 0, 255, 51, 204, 51, 255, 51, 204, 102, 255, 51, 204, 153, 255,
   51, 204, 204, 255, 51, 204, 255, 255, 51, 255, 0, 255, 51, 255, 51, 255,
   51, 255, 102, 255, 51, 255, 153, 255, 51, 255, 204, 255, 51, 255, 255, 255,
   102, 0, 0, 255, 102, 0, 51, 255, 102, 0, 102, 255, 102, 0, 153, 255, 102,
   0, 204, 255, 102, 0, 255, 255, 102, 51, 0, 255, 102, 51, 51, 255, 102, 51,
   102, 255, 102, 51, 153, 255, 102, 51, 204, 255, 102, 51, 255, 255, 102, 102,
   0, 255, 102, 102, 51, 255, 102, 102, 102, 255, 102, 102, 153, 255, 102, 102,
   204, 255, 102, 102, 255, 255, 102, 153, 0, 255, 102, 153, 51, 255, 102, 153,
   102, 255, 102, 153, 153, 255, 102, 153, 204, 255, 102, 153, 255, 255, 102,
   204, 0, 255, 102, 204, 51, 255, 102, 204, 102, 255, 102, 204, 153, 255,
   102, 204, 204, 255, 102, 204, 255, 255, 102, 255, 0, 255, 102, 255, 51, 255,
   102, 255, 102, 255, 102, 255, 153, 255, 102, 255, 204, 255, 102, 255, 255,
   255, 153, 0, 0, 255, 153, 0, 51, 255, 153, 0, 102, 255, 153, 0, 153, 255,
   153, 0, 204, 255, 153, 0, 255, 255, 153, 51, 0, 255, 153, 51, 51, 255,
   153, 51, 102, 255, 153, 51, 153, 255, 153, 51, 204, 255, 153, 51, 255, 255,
   153, 102, 0, 255, 153, 102, 51, 255, 153, 102, 102, 255, 153, 102, 153, 255,
   153, 102, 204, 255, 153, 102, 255, 255, 153, 153, 0, 255, 153, 153, 51, 255,
   153, 153, 102, 255, 153, 153, 153, 255, 153, 153, 204, 255, 153, 153, 255,
   255, 153, 204, 0, 255, 153, 204, 51, 255, 153, 204, 102, 255, 153, 204, 153,
   255, 153, 204, 204, 255, 153, 204, 255, 255, 153, 255, 0, 255,
   153, 255, 51, 255, 153, 255, 102, 255, 153, 255, 153, 255, 153, 255, 204,
   255, 153, 255, 255, 255, 204, 0, 0, 255, 204, 0, 51, 255, 204, 0, 102, 255,
   204, 0, 153, 255, 204, 0, 204, 255, 204, 0, 255, 255, 204, 51, 0, 255,
   204, 51, 51, 255, 204, 51, 102, 255, 204, 51, 153, 255, 204, 51, 204, 255,
   204, 51, 255, 255, 204, 102, 0, 255, 204, 102, 51, 255, 204, 102, 102, 255,
   204, 102, 153, 255, 204, 102, 204, 255, 204, 102, 255, 255, 204, 153,0, 255,
   204, 153, 51, 255, 204, 153, 102, 255, 204, 153, 153, 255, 204, 153, 204,
   255, 204, 153, 255, 255, 204, 204, 0, 255, 204, 204, 51, 255, 204, 204, 102,
   255, 204, 204, 153, 255, 204, 204, 204, 255, 204, 204, 255, 255, 204, 255, 0,
   255, 204, 255, 51, 255, 204, 255, 102, 255, 204, 255, 153, 255, 204, 255,
   204, 255, 204, 255, 255, 255, 255, 0, 0, 255, 255, 0, 51, 255, 255, 0, 102,
   255, 255,0,153, 255, 255, 0, 204, 255, 255, 0, 255, 255, 255, 51, 0, 255,
   255, 51, 51, 255, 255, 51, 102, 255, 255, 51, 153, 255, 255, 51, 204, 255,
   255, 51, 255, 255, 255, 102, 0, 255, 255, 102, 51, 255, 255, 102, 102, 255,
   255, 102, 153, 255, 255, 102, 204, 255, 255, 102, 255, 255, 255, 153, 0, 255,
   255, 153, 51, 255, 255, 153, 102, 255, 255, 153, 153, 255, 255, 153, 204, 255,
   255, 153, 255, 255, 255, 204, 0, 255, 255, 204, 51, 255, 255, 204, 102, 255,
   255, 204,153, 255, 255, 204, 204, 255, 255, 204, 255, 255, 255, 255, 0, 255,
   255, 255, 51, 255, 255, 255, 102, 255, 255, 255, 153, 255, 255, 255, 204, 255,
   255, 255, 255, 255, 0, 0, 0, 255, 13, 13, 13, 255, 26, 26, 26, 255, 40, 40,
   40, 255, 53, 53, 53, 255, 67, 67, 67, 255, 80, 80, 80, 255, 93, 93, 93, 255,
   107, 107, 107, 255, 120, 120, 120, 255, 134, 134, 134, 255, 147, 147, 147,
   255, 161, 161, 161, 255, 174, 174, 174, 255, 187, 187, 187, 255, 201, 201,
   201, 255, 214, 214, 214, 255, 228, 228, 228, 255, 241, 241, 241, 255, 255,
   255, 255, 255, 255, 251, 240, 255, 160, 160, 164, 255, 128, 128, 128, 255,
   255, 0, 0, 255, 0, 255, 0, 255, 255, 255, 0, 255, 0, 0, 255, 255, 255, 0,
   255, 255, 0, 255, 255, 255, 255, 255, 255, 255);

  WeightScale = 0.0004;

resourcestring
  sExtASCIIErrorCode = 'ExtASCII error code';
  sCorruptFileError = 'Corrupt File Error';
  sW2DBinaryComandError = 'ExtBinary comand format error';
  sIllegalOpcodeFmt = 'Illegal opcode %.2x at %.8x';
  sIllegalShellFmt = 'Illegal "Shell" at %.8x';

type

  PByteArray = ^TByteArray;
  TByteArray = array [Word] of Byte;

  PData = PByte;

  TPolyTriangle = record
    Point1, Point2, Point3: TPoint;
  end;

{ dwf Parser}
const

// wd2 functions opcode map
// one Byte opcodes
  cnstSetColor03h                   = $03;
  cnstSetColor43h                   = $43;
  cnstSetColor63h                   = $63;
  cnstSetFont06h                    = $06;
  cnstSetObjectNode0                = $0E;
  cnstSetObjectNode1                = $4E;
  cnstSetObjectNode2                = $6E;
  cnstSetLineWeight                 = $17;
  cnstSetFillMode0                  = $46;
  cnstSetFillMode1                  = $66;
  cnstSetMarkerGlyph0               = $47;
  cnstSetMarkerGlyph1               = $87;
  cnstSetCurrentPoint               = $4F;
  cnstSetMarkerSize0                = $53;
  cnstSetMarkerSize1                = $73;
  cnstSetVisibility0                = $56;
  cnstSetVisibility1                = $76;
  cnstSetLayer                      = $AC;
//
  cnstDrawGouraudPolytriangle07h    = $07;
  cnstDrawGouraudPolytriangle67h    = $67;
  cnstDrawPolytriangle14h           = $14;
  cnstDrawPolytriangle54h           = $54;
  cnstDrawPolytriangle74h           = $74;
  cnstDrawContourSet0               = $0B;
  cnstDrawContourSet1               = $6B;
  cnstDrawLine0                     = $0C;
  cnstDrawLine1                     = $4C;
  cnstDrawLine2                     = $6C;
  cnstDrawLine3                     = $8C;
  cnstDrawPolyline0                 = $10;
  cnstDrawPolyline1                 = $70;
  cnstDrawGouraudPolyline0          = $11;
  cnstDrawGouraudPolyline1          = $71;
  cnstDrawCircle0                   = $12;
  cnstDrawCircle1                   = $52;
  cnstDrawCircle2                   = $72;
  cnstDrawCircle3                   = $92;
  cnstDrawText0                     = $18;
  cnstDrawText78h                   = $78;
  cnstDrawEllipse0                  = $45;
  cnstDrawEllipse1                  = $65;
  cnstDrawPolymarker0               = $4D;
  cnstDrawPolymarker1               = $6D;
  cnstDrawPolymarker2               = $8D;
  cnstDrawPolylinePolygon50h        = $50;
  cnstDrawPolyBeziercurve           = $62;
  cnstDrawTexturedPolytriangle      = $77;

// extended ASCII opcodes
// index
  cnstExtASCIIAlignment         = $00;
  cnstExtASCIIAuthor            = $01;
  cnstExtASCIIBlockRef          = $02;
  cnstExtASCIIBackGround        = $03;
  cnstExtASCIIBlockMeaning      = $04;
  cnstExtASCIIBezier            = $05;
  cnstExtASCIICircle            = $06;
  cnstExtASCIICodePage          = $07;
  cnstExtASCIIColor             = $08;
  cnstExtASCIIColorMap          = $09;
  cnstExtASCIIComment           = $0A;
  cnstExtASCIIComments          = $0B;
  cnstExtASCIIContour           = $0C;
  cnstExtASCIICopyright         = $0D;
  cnstExtASCIICreated           = $0E;
  cnstExtASCIICreator           = $0F;
  cnstExtASCIIDashPattern       = $10;
  cnstExtASCIIDescription       = $11;
  cnstExtASCIIDirectory         = $12;
  cnstExtASCIIDrawingInfo       = $13;
  cnstExtASCIIEllipse           = $14;
  cnstExtASCIIEmbeddedFont      = $15;
  cnstExtASCIIEmbedFile         = $16;
  cnstExtASCIIEmbed             = $17;
  cnstExtASCIIEncryption        = $18;
  cnstExtASCIIEndOfDwf          = $19;
  cnstExtASCIIFillPattern       = $1A;
  cnstExtASCIIFillPatternScale  = $1B;
  cnstExtASCIIFont              = $1C;
  cnstExtASCIIGlyph             = $1D;
  cnstExtASCIIGouraud           = $1E;
  cnstExtASCIIGroup4PNGImage    = $1F;
  cnstExtASCIIGroupBegin        = $20;
  cnstExtASCIIGroupEnd          = $21;
  cnstExtASCIIGuid              = $22;
  cnstExtASCIIGuidList          = $23;
  cnstExtASCIIImage             = $24;
  cnstExtASCIIInkedArea         = $25;
  cnstExtASCIIKeywords          = $26;
  cnstExtASCIILayer             = $27;
  cnstExtASCIILinePattern       = $28;
  cnstExtASCIILinesOverwrite    = $29;
  cnstExtASCIILineStyle         = $2A;
  cnstExtASCIILineWeight        = $2B;
  cnstExtASCIIModified          = $2C;
  cnstExtASCIINamedView         = $2D;
  cnstExtASCIINode              = $2E;
  cnstExtASCIINonStdFontList    = $2F;
  cnstExtASCIIOrientation       = $30;
  cnstExtASCIIPenPattern        = $31;
  cnstExtASCIIPenPatternOptions = $32;
  cnstExtASCIIPenPat_Options    = $33;
  cnstExtASCIIPlotInfo          = $34;
  cnstExtASCIIPlotOptimized     = $35;
  cnstExtASCIIProjection        = $36;
  cnstExtASCIIPsswd             = $37;
  cnstExtASCIISignData          = $38;
  cnstExtASCIISourceCreated     = $39;
  cnstExtASCIISourceFilename    = $3A;
  cnstExtASCIISourceModified    = $3B;
  cnstExtASCIIText              = $3C;
  cnstExtASCIITime              = $3D;
  cnstExtASCIITitle             = $3E;
  cnstExtASCIIUnits             = $3F;
  cnstExtASCIIURL               = $40;
  cnstExtASCIIUserData          = $41;
  cnstExtASCIIView              = $42;
  cnstExtASCIIViewport          = $43;
  cnstExtASCIIFontExtension     = $44;
  cnstExtASCIITextHAlign        = $45;
  cnstExtASCIITextVAlign        = $46;

  cnstExtASCIIUnknown           = $47;

  cnstTableExtASCIICount          = cnstExtASCIIUnknown;  // 71
  cnstTableExtASCIIByteCount      = cnstTableExtASCIICount * 32;
  arTableExtASCII: array [0 .. 70] of string = ('Alignment', 'Author',
    'BlockRef', 'Background', 'BlockMeaning', 'Bezier', 'Circle', 'CodePage',
    'Color', 'ColorMap', 'Comment', 'Comments', 'Contour', 'Copyright',
    'Created', 'Creator', 'DashPattern', 'Description', 'Directory',
    'DrawingInfo', 'Ellipse', 'Embedded_Font', 'EmbedFile', 'Embed',
    'Encryption', 'EndOfDwf', 'FillPattern', 'FillPatternScale', 'Font',
    'Glyph', 'Gouraud', 'Group4PNGImage', 'GroupBegin', 'GroupEnd', 'Guid',
    'GuidList', 'Image', 'InkedArea', 'Keywords', 'Layer', 'LinePattern',
    'LinesOverwrite', 'LineStyle', 'LineWeight', 'Modified', 'NamedView',
    'Node', 'NonStdFontList', 'Orientation', 'PenPattern', 'PenPatternOptions',
    'PenPat_Options', 'PlotInfo', 'PlotOptimized', 'Projection', 'Psswd',
    'SignData', 'SourceCreated', 'SourceFilename', 'SourceModified', 'Text',
    'Time', 'Title', 'Units', 'URL', 'UserData', 'View', 'Viewport', 'FontExtension',
    'TextHAlign', 'TextVAlign');


{$IFDEF SG_W2D_DEBUG}
  cnstOpcodeString: array[0..255] of string = (
    'Termination         ', 'Pause               ', '-- 0x02 --          ', '-- 0x03 --          ',
    '-- 0x04 --          ', '-- 0x05 --          ', '-- 0x06 --          ', 'Callback            ',
    'Color_By_Index      ', 'Color_By_Index_16   ', 'Color_By_FIndex     ', 'Color_By_Value      ',
    'Color_Map           ', 'Edge_Pattern        ', 'Edge_Weight         ', '[Rsrv:Complex_Clip] ',
    'PolyPolyline        ', '-- 0x011 --         ', 'External_Reference  ', '-- 0x13 --          ',
    'Thumbnail           ', 'URL                 ', 'Unicode_Options     ', '-- 0x17 --          ',
    'XML                 ', 'LOD                 ', 'Sphere              ', '-- 0x1B --          ',
    '-- 0x1C --          ', '-- 0x1D --          ', '-- 0x1E --          ', '-- 0x1F --          ',
    'Text_Spacing        ', 'Selectability       ', 'Color               ', 'Window_Frame        ',
    'Texture_Matrix      ', 'Modelling_Matrix    ', 'Repeat_Object       ', '-- " --             ',
    'Open_Segment        ', 'Close_Segment       ', 'Text_Alignment      ', 'Marker_Size         ',
    '<Streaming_Mode>    ', 'Line_Pattern        ', 'Local_Light         ', 'Cutting_Plane       ',
    'Priority            ', '-- 1 --             ', '-- 2 --             ', '-- 3 --             ',
    '-- 4 --             ', '-- 5 --             ', '-- 6 --             ', '-- 7 --             ',
    '-- 8 --             ', 'Close_GeoAttributes ', 'Geometry_Attibutes  ', 'Comment             ',
    'Include_Segment     ', 'Line_Weight         ', 'Camera              ', 'Conditional         ',
    'Marker_Symbol       ', 'NURBS_Surface       ', 'Bounding_Info       ', 'Circle              ',
    'Dictionary          ', 'Ellipse             ', 'Text_Font           ', 'Polygon             ',
    'Heuristics          ', 'File_Info           ', 'Line_Style          ', 'Renumber_Key_Global ',
    'Polyline            ', 'Mesh                ', 'NURBS_Curve         ', 'Clip_Region         ',
    'Face_Pattern        ', 'PolyCylinder        ', 'Rendering_Options   ', 'Shell               ',
    'Text                ', 'User_Options        ', 'Visibility          ', 'Window              ',
    'Marker              ', 'Cylinder            ', 'Start_Compression   ', 'Start_User_Data     ',
    'Circular_Chord      ', 'Stop_User_Data      ', 'Spot_Light          ', 'Dictionary_Locater  ',
    'Infinite_Line       ', 'Area_Light          ', 'Bounding            ', 'Circular_Arc        ', // `abc
    'Distant_Light       ', 'Elliptical_Arc      ', 'Font                ', 'Grid                ',
    'Handedness          ', 'Image               ', 'Glyph Definition    ', 'Renumber_Key_Local  ',
    'Line                ', '-- m --             ', 'User_Index          ', 'Clip_Rectangle      ',
    'Window_Pattern      ', 'Tag                 ', 'Reference           ', 'Reopen_Segment      ', // pqrs
    'Texture             ', '-- u --             ', 'User_Value          ', 'Circular_Wedge      ',
    'Text_With_Encoding  ', '-- y --             ', 'Stop_Compression    ', 'Style_Segment       ',
    'Text_Path           ', 'View                ', 'Color_RGB           ', 'Delete_Object       ',
    '-- 0x80 --          ', '-- 0x81 --          ', '-- 0x82 --          ', '-- 0x83 --          ',
    '-- 0x84 --          ', '-- 0x85 --          ', '-- 0x86 --          ', '-- 0x87 --          ',
    '-- 0x88 --          ', '-- 0x88 --          ', '-- 0x8A --          ', '-- 0x8B --          ',
    '-- 0x8C --          ', '-- 0x8D --          ', '-- 0x8E --          ', '-- 0x8F --          ',
    '-- 0x90 --          ', '-- 0x91 --          ', '-- 0x92 --          ', '-- 0x93 --          ',
    '-- 0x94 --          ', '-- 0x95 --          ', '-- 0x96 --          ', '-- 0x97 --          ',
    '-- 0x98 --          ', '-- 0x98 --          ', '-- 0x9A --          ', '-- 0x9B --          ',
    '-- 0x9C --          ', '-- 0x9D --          ', '-- 0x9E --          ', '-- 0x9F --          ',
    '-< 0xA0 >-          ', '-< 0xA1 >-          ', '-< 0xA2 >-          ', '-< 0xA3 >-          ',
    '-< 0xA4 >-          ', '-< 0xA5 >-          ', '-< 0xA6 >-          ', '-< 0xA7 >-          ',
    '-< 0xA8 >-          ', '-< 0xA8 >-          ', '-< 0xAA >-          ', '-< 0xAB >-          ',
    '-< 0xAC >-          ', '-< 0xAD >-          ', '-< 0xAE >-          ', '-< 0xAF >-          ',
    '-< 0xB0 >-          ', '-< 0xB1 >-          ', '-< 0xB2 >-          ', '-< 0xB3 >-          ',
    '-< 0xB4 >-          ', '-< 0xB5 >-          ', '-< 0xB6 >-          ', '-< 0xB7 >-          ',
    '-< 0xB8 >-          ', '-< 0xB8 >-          ', '-< 0xBA >-          ', '-< 0xBB >-          ',
    '-< 0xBC >-          ', '-< 0xBD >-          ', '-< 0xBE >-          ', '-< 0xBF >-          ',
    '-< 0xC0 >-          ', '-< 0xC1 >-          ', '-< 0xC2 >-          ', '-< 0xC3 >-          ',
    '-< 0xC4 >-          ', '-< 0xC5 >-          ', '-< 0xC6 >-          ', '-< 0xC7 >-          ',
    '-< 0xC8 >-          ', '-< 0xC8 >-          ', '-< 0xCA >-          ', '-< 0xCB >-          ',
    '-< 0xCC >-          ', '-< 0xCD >-          ', '-< 0xCE >-          ', '-< 0xCF >-          ',
    '-< 0xD0 >-          ', '-< 0xD1 >-          ', '-< 0xD2 >-          ', '-< 0xD3 >-          ',
    '-< 0xD4 >-          ', '-< 0xD5 >-          ', '-< 0xD6 >-          ', '-< 0xD7 >-          ',
    '-< 0xD8 >-          ', '-< 0xD8 >-          ', '-< 0xDA >-          ', '-< 0xDB >-          ',
    '-< 0xDC >-          ', '-< 0xDD >-          ', '-< 0xDE >-          ', '-< 0xDF >-          ',
    'W3D_Image           ', '-< 0xE1 >-          ', '-< 0xE2 >-          ', '-< 0xE3 >-          ',
    '-< 0xE4 >-          ', '-< 0xE5 >-          ', '-< 0xE6 >-          ', '-< 0xE7 >-          ',
    '-< 0xE8 >-          ', '-< 0xE8 >-          ', '-< 0xEA >-          ', '-< 0xEB >-          ',
    '-< 0xEC >-          ', '-< 0xED >-          ', '-< 0xEE >-          ', '-< 0xEF >-          ',
    '-- 0xF0 --          ', '-- 0xF1 --          ', '-- 0xF2 --          ', '-- 0xF3 --          ',
    '-- 0xF4 --          ', '-- 0xF5 --          ', '-- 0xF6 --          ', '-- 0xF7 --          ',
    '-- 0xF8 --          ', '-- 0xF8 --          ', '-- 0xFA --          ', '-- 0xFB --          ',
    '-- 0xFC --          ', '-- 0xFD --          ', '[Pseudo-Handler]    ', '<Extended>          ');
{$ENDIF}

    ZlibDict: Ansistring =
      '(DashPattern (LineStyle (Copyright (Keywords (Viewport (CodePage {'+
      #$04#$04#$00#$00#$01#$00#$00''+
      #$00#$00#$00#$FF#$00#$00#$80#$FF#$00#$80#$00#$FF#$00#$80#$80#$FF''+
      #$80#$00#$00#$FF#$80#$00#$80#$FF#$80#$80#$00#$FF#$C0#$C0#$C0#$FF''+
      #$C0#$DC#$C0#$FF#$F0#$CA#$A6#$FF#$00#$00#$00#$FF#$33#$00#$00#$FF''+
      #$66#$00#$00#$FF#$99#$00#$00#$FF#$CC#$00#$00#$FF#$FF#$00#$00#$FF''+
      #$00#$33#$00#$FF#$33#$33#$00#$FF#$66#$33#$00#$FF#$99#$33#$00#$FF''+
      #$CC#$33#$00#$FF#$FF#$33#$00#$FF#$00#$66#$00#$FF#$33#$66#$00#$FF''+
      #$66#$66#$00#$FF#$99#$66#$00#$FF#$CC#$66#$00#$FF#$FF#$66#$00#$FF''+
      #$00#$99#$00#$FF#$33#$99#$00#$FF#$66#$99#$00#$FF#$99#$99#$00#$FF''+
      #$CC#$99#$00#$FF#$FF#$99#$00#$FF#$00#$CC#$00#$FF#$33#$CC#$00#$FF''+
      #$66#$CC#$00#$FF#$99#$CC#$00#$FF#$CC#$CC#$00#$FF#$FF#$CC#$00#$FF''+
      #$00#$FF#$00#$FF#$33#$FF#$00#$FF#$66#$FF#$00#$FF#$99#$FF#$00#$FF''+
      #$CC#$FF#$00#$FF#$FF#$FF#$00#$FF#$00#$00#$33#$FF#$33#$00#$33#$FF''+
      #$66#$00#$33#$FF#$99#$00#$33#$FF#$CC#$00#$33#$FF#$FF#$00#$33#$FF''+
      #$00#$33#$33#$FF#$33#$33#$33#$FF#$66#$33#$33#$FF#$99#$33#$33#$FF''+
      #$CC#$33#$33#$FF#$FF#$33#$33#$FF#$00#$66#$33#$FF#$33#$66#$33#$FF''+
      #$66#$66#$33#$FF#$99#$66#$33#$FF#$CC#$66#$33#$FF#$FF#$66#$33#$FF''+
      #$00#$99#$33#$FF#$33#$99#$33#$FF#$66#$99#$33#$FF#$99#$99#$33#$FF''+
      #$CC#$99#$33#$FF#$FF#$99#$33#$FF#$00#$CC#$33#$FF#$33#$CC#$33#$FF''+
      #$66#$CC#$33#$FF#$99#$CC#$33#$FF#$CC#$CC#$33#$FF#$FF#$CC#$33#$FF''+
      #$00#$FF#$33#$FF#$33#$FF#$33#$FF#$66#$FF#$33#$FF#$99#$FF#$33#$FF''+
      #$CC#$FF#$33#$FF#$FF#$FF#$33#$FF#$00#$00#$66#$FF#$33#$00#$66#$FF''+
      #$66#$00#$66#$FF#$99#$00#$66#$FF#$CC#$00#$66#$FF#$FF#$00#$66#$FF''+
      #$00#$33#$66#$FF#$33#$33#$66#$FF#$66#$33#$66#$FF#$99#$33#$66#$FF''+
      #$CC#$33#$66#$FF#$FF#$33#$66#$FF#$00#$66#$66#$FF#$33#$66#$66#$FF''+
      #$66#$66#$66#$FF#$99#$66#$66#$FF#$CC#$66#$66#$FF#$FF#$66#$66#$FF''+
      #$00#$99#$66#$FF#$33#$99#$66#$FF#$66#$99#$66#$FF#$99#$99#$66#$FF''+
      #$CC#$99#$66#$FF#$FF#$99#$66#$FF#$00#$CC#$66#$FF#$33#$CC#$66#$FF''+
      #$66#$CC#$66#$FF#$99#$CC#$66#$FF#$CC#$CC#$66#$FF#$FF#$CC#$66#$FF''+
      #$00#$FF#$66#$FF#$33#$FF#$66#$FF#$66#$FF#$66#$FF#$99#$FF#$66#$FF''+
      #$CC#$FF#$66#$FF#$FF#$FF#$66#$FF#$00#$00#$99#$FF#$33#$00#$99#$FF''+
      #$66#$00#$99#$FF#$99#$00#$99#$FF#$CC#$00#$99#$FF#$FF#$00#$99#$FF''+
      #$00#$33#$99#$FF#$33#$33#$99#$FF#$66#$33#$99#$FF#$99#$33#$99#$FF''+
      #$CC#$33#$99#$FF#$FF#$33#$99#$FF#$00#$66#$99#$FF#$33#$66#$99#$FF''+
      #$66#$66#$99#$FF#$99#$66#$99#$FF#$CC#$66#$99#$FF#$FF#$66#$99#$FF''+
      #$00#$99#$99#$FF#$33#$99#$99#$FF#$66#$99#$99#$FF#$99#$99#$99#$FF''+
      #$CC#$99#$99#$FF#$FF#$99#$99#$FF#$00#$CC#$99#$FF#$33#$CC#$99#$FF''+
      #$66#$CC#$99#$FF#$99#$CC#$99#$FF#$CC#$CC#$99#$FF#$FF#$CC#$99#$FF''+
      #$00#$FF#$99#$FF#$33#$FF#$99#$FF#$66#$FF#$99#$FF#$99#$FF#$99#$FF''+
      #$CC#$FF#$99#$FF#$FF#$FF#$99#$FF#$00#$00#$CC#$FF#$33#$00#$CC#$FF''+
      #$66#$00#$CC#$FF#$99#$00#$CC#$FF#$CC#$00#$CC#$FF#$FF#$00#$CC#$FF''+
      #$00#$33#$CC#$FF#$33#$33#$CC#$FF#$66#$33#$CC#$FF#$99#$33#$CC#$FF''+
      #$CC#$33#$CC#$FF#$FF#$33#$CC#$FF#$00#$66#$CC#$FF#$33#$66#$CC#$FF''+
      #$66#$66#$CC#$FF#$99#$66#$CC#$FF#$CC#$66#$CC#$FF#$FF#$66#$CC#$FF''+
      #$00#$99#$CC#$FF#$33#$99#$CC#$FF#$66#$99#$CC#$FF#$99#$99#$CC#$FF''+
      #$CC#$99#$CC#$FF#$FF#$99#$CC#$FF#$00#$CC#$CC#$FF#$33#$CC#$CC#$FF''+
      #$66#$CC#$CC#$FF#$99#$CC#$CC#$FF#$CC#$CC#$CC#$FF#$FF#$CC#$CC#$FF''+
      #$00#$FF#$CC#$FF#$33#$FF#$CC#$FF#$66#$FF#$CC#$FF#$99#$FF#$CC#$FF''+
      #$CC#$FF#$CC#$FF#$FF#$FF#$CC#$FF#$00#$00#$FF#$FF#$33#$00#$FF#$FF''+
      #$66#$00#$FF#$FF#$99#$00#$FF#$FF#$CC#$00#$FF#$FF#$FF#$00#$FF#$FF''+
      #$00#$33#$FF#$FF#$33#$33#$FF#$FF#$66#$33#$FF#$FF#$99#$33#$FF#$FF''+
      #$CC#$33#$FF#$FF#$FF#$33#$FF#$FF#$00#$66#$FF#$FF#$33#$66#$FF#$FF''+
      #$66#$66#$FF#$FF#$99#$66#$FF#$FF#$CC#$66#$FF#$FF#$FF#$66#$FF#$FF''+
      #$00#$99#$FF#$FF#$33#$99#$FF#$FF#$66#$99#$FF#$FF#$99#$99#$FF#$FF''+
      #$CC#$99#$FF#$FF#$FF#$99#$FF#$FF#$00#$CC#$FF#$FF#$33#$CC#$FF#$FF''+
      #$66#$CC#$FF#$FF#$99#$CC#$FF#$FF#$CC#$CC#$FF#$FF#$FF#$CC#$FF#$FF''+
      #$00#$FF#$FF#$FF#$33#$FF#$FF#$FF#$66#$FF#$FF#$FF#$99#$FF#$FF#$FF''+
      #$CC#$FF#$FF#$FF#$FF#$FF#$FF#$FF#$00#$00#$00#$FF#$0D#$0D#$0D#$FF''+
      #$1A#$1A#$1A#$FF#$28#$28#$28#$FF#$35#$35#$35#$FF#$43#$43#$43#$FF''+
      #$50#$50#$50#$FF#$5D#$5D#$5D#$FF#$6B#$6B#$6B#$FF#$78#$78#$78#$FF''+
      #$86#$86#$86#$FF#$93#$93#$93#$FF#$A1#$A1#$A1#$FF#$AE#$AE#$AE#$FF''+
      #$BB#$BB#$BB#$FF#$C9#$C9#$C9#$FF#$D6#$D6#$D6#$FF#$E4#$E4#$E4#$FF''+
      #$F1#$F1#$F1#$FF#$FF#$FF#$FF#$FF#$F0#$FB#$FF#$FF#$A4#$A0#$A0#$FF''+
      #$80#$80#$80#$FF#$00#$00#$FF#$FF#$00#$FF#$00#$FF#$00#$FF#$FF#$FF''+
      #$FF#$00#$00#$FF#$FF#$00#$FF#$FF#$FF#$FF#$00#$FF#$FF#$FF#$FF#$FF''+
      '(NamedView (Author (Background (Bezier (Bounds (Clip (Color '+
      '(ColorMap (Comment (Created (Creator (Description (DrawingInfo '+
      ')(Embed ''image/vnd.dwg;'' ''AutoCAD-r13'' ''unknown.dwg'' '''')(View (Gouraud '+
      '(Image (Layer (LineCap (LineJoin (LinePattern (LineWeight (Modified '+
      '(Projection (Scale (SourceCreated (SourceFilename (SourceModified (Text '+
      '(URL ''http://www.com'')''ftp://ftp.'+ //367
      '00001000200030004000500060007000800090011001200130'+
      '01400150016001700180019002100220023002400250026002'+
      '70028002900310032003300340035003600370038003900410'+
      '04200430044004500460047004800490051005200530054005'+
      '50056005700580059006100620063006400650066006700680'+
      '06900710072007300740075007600770078007900810082008'+
      '30084008500860087008800890091009200930094009500960'+
      '09700980099010102010301040105010601070108010901110'+
      '11201130114011501160117011801190121012201230124012'+
      '50126012701280129013101320133013401350136013701380'+
      '13901410142014301440145014601470148014901510152015'+
      '30154015501560157015801590161016201630164016501660'+
      '16701680169017101720173017401750176017701780179018'+
      '10182018301840185018601870188018901910192019301940'+
      '19501960197019801990202030204020502060207020802090'+
      '21102120213021402150216021702180219022102220223022'+
      '40225022602270228022902310232023302340235023602370'+
      '23802390241024202430244024502460247024802490251025'+
      '20253025402550256025702580259026102620263026402650'+
      '26602670268026902710272027302740275027602770278027'+
      '90281028202830284028502860287028802890291029202930'+
      '29402950296029702980299030304030503060307030803090'+
      '31103120313031403150316031703180319032103220323032'+
      '40325032603270328032903310332033303340335033603370'+
      '33803390341034203430344034503460347034803490351035'+
      '20353035403550356035703580359036103620363036403650'+
      '36603670368036903710372037303740375037603770378037'+
      '90381038203830384038503860387038803890391039203930'+
      '39403950396039703980399040405040604070408040904110'+
      '41204130414041504160417041804190421042204230424042'+
      '50426042704280429043104320433043404350436043704380'+
      '43904410442044304440445044604470448044904510452045'+
      '30454045504560457045804590461046204630464046504660'+
      '46704680469047104720473047404750476047704780479048'+
      '10482048304840485048604870488048904910492049304940'+
      '49504960497049804990505060507050805090511051205130'+
      '51405150516051705180519052105220523052405250526052'+
      '70528052905310532053305340535053605370538053905410'+
      '54205430544054505460547054805490551055205530554055'+
      '50556055705580559056105620563056405650566056705680'+
      '56905710572057305740575057605770578057905810582058'+
      '30584058505860587058805890591059205930594059505960'+
      '59705980599060607060806090611061206130614061506160'+
      '61706180619062106220623062406250626062706280629063'+
      '10632063306340635063606370638063906410642064306440'+
      '64506460647064806490651065206530654065506560657065'+
      '80659066106620663066406650666066706680669067106720'+
      '67306740675067606770678067906810682068306840685068'+
      '60687068806890691069206930694069506960697069806990'+
      '70708070907110712071307140715071607170718071907210'+
      '72207230724072507260727072807290731073207330734073'+
      '50736073707380739074107420743074407450746074707480'+
      '74907510752075307540755075607570758075907610762076'+
      '30764076507660767076807690771077207730774077507760'+
      '77707780779078107820783078407850786078707880789079'+
      '10792079307940795079607970798079908080908110812081'+
      '30814081508160817081808190821082208230824082508260'+
      '82708280829083108320833083408350836083708380839084'+
      '10842084308440845084608470848084908510852085308540'+
      '85508560857085808590861086208630864086508660867086'+
      '80869087108720873087408750876087708780879088108820'+
      '88308840885088608870888088908910892089308940895089'+
      '60897089808990909110912091309140915091609170918091'+
      '90921092209230924092509260927092809290931093209330'+
      '93409350936093709380939094109420943094409450946094'+
      '70948094909510952095309540955095609570958095909610'+
      '96209630964096509660967096809690971097209730974097'+
      '50976097709780979098109820983098409850986098709880'+
      '98909910992099309940995099609970998099911112111311'+
      '14111511161117111811191122112311241125112611271128'+
      '11291132113311341135113611371138113911421143114411'+
      '45114611471148114911521153115411551156115711581159'+
      '11621163116411651166116711681169117211731174117511'+
      '76117711781179118211831184118511861187118811891192'+
      '11931194119511961197119811991212131214121512161217'+
      '12181219122212231224122512261227122812291232123312'+
      '34123512361237123812391242124312441245124612471248'+
      '12491252125312541255125612571258125912621263126412'+
      '65126612671268126912721273127412751276127712781279'+
      '12821283128412851286128712881289129212931294129512'+
      '96129712981299131314131513161317131813191322132313'+
      '24132513261327132813291332133313341335133613371338'+
      '13391342134313441345134613471348134913521353135413'+
      '55135613571358135913621363136413651366136713681369'+
      '13721373137413751376137713781379138213831384138513'+
      '86138713881389139213931394139513961397139813991414'+
      '15141614171418141914221423142414251426142714281429'+
      '14321433143414351436143714381439144214431444144514'+
      '46144714481449145214531454145514561457145814591462'+
      '14631464146514661467146814691472147314741475147614'+
      '77147814791482148314841485148614871488148914921493'+
      '14941495149614971498149915151615171518151915221523'+
      '15241525152615271528152915321533153415351536153715'+
      '38153915421543154415451546154715481549155215531554'+
      '15551556155715581559156215631564156515661567156815'+
      '69157215731574157515761577157815791582158315841585'+
      '15861587158815891592159315941595159615971598159916'+
      '16171618161916221623162416251626162716281629163216'+
      '33163416351636163716381639164216431644164516461647'+
      '16481649165216531654165516561657165816591662166316'+
      '64166516661667166816691672167316741675167616771678'+
      '16791682168316841685168616871688168916921693169416'+
      '95169616971698169917171817191722172317241725172617'+
      '27172817291732173317341735173617371738173917421743'+
      '17441745174617471748174917521753175417551756175717'+
      '58175917621763176417651766176717681769177217731774'+
      '17751776177717781779178217831784178517861787178817'+
      '89179217931794179517961797179817991818191822182318'+
      '24182518261827182818291832183318341835183618371838'+
      '18391842184318441845184618471848184918521853185418'+
      '55185618571858185918621863186418651866186718681869'+
      '18721873187418751876187718781879188218831884188518'+
      '86188718881889189218931894189518961897189818991919'+
      '22192319241925192619271928192919321933193419351936'+
      '19371938193919421943194419451946194719481949195219'+
      '53195419551956195719581959196219631964196519661967'+
      '19681969197219731974197519761977197819791982198319'+
      '84198519861987198819891992199319941995199619971998'+
      '19992222322242225222622272228222922332234223522362'+
      '23722382239224322442245224622472248224922532254225'+
      '52256225722582259226322642265226622672268226922732'+
      '27422752276227722782279228322842285228622872288228'+
      '92293229422952296229722982299232324232523262327232'+
      '82329233323342335233623372338233923432344234523462'+
      '34723482349235323542355235623572358235923632364236'+
      '52366236723682369237323742375237623772378237923832'+
      '38423852386238723882389239323942395239623972398239'+
      '92424252426242724282429243324342435243624372438243'+
      '92443244424452446244724482449245324542455245624572'+
      '45824592463246424652466246724682469247324742475247'+
      '62477247824792483248424852486248724882489249324942'+
      '49524962497249824992525262527252825292533253425352'+
      '53625372538253925432544254525462547254825492553255'+
      '42555255625572558255925632564256525662567256825692'+
      '57325742575257625772578257925832584258525862587258'+
      '82589259325942595259625972598259926262726282629263'+
      '32634263526362637263826392643264426452646264726482'+
      '64926532654265526562657265826592663266426652666266'+
      '72668266926732674267526762677267826792683268426852'+
      '68626872688268926932694269526962697269826992727282'+
      '72927332734273527362737273827392743274427452746274'+
      '72748274927532754275527562757275827592763276427652'+
      '76627672768276927732774277527762777277827792783278'+
      '42785278627872788278927932794279527962797279827992'+
      '82829283328342835283628372838283928432844284528462'+
      '84728482849285328542855285628572858285928632864286'+
      '52866286728682869287328742875287628772878287928832'+
      '88428852886288728882889289328942895289628972898289'+
      '92929332934293529362937293829392943294429452946294'+
      '72948294929532954295529562957295829592963296429652'+
      '96629672968296929732974297529762977297829792983298'+
      '42985298629872988298929932994299529962997299829993'+
      '33343335333633373338333933443345334633473348334933'+
      '54335533563357335833593364336533663367336833693374'+
      '33753376337733783379338433853386338733883389339433'+
      '95339633973398339934343534363437343834393444344534'+
      '46344734483449345434553456345734583459346434653466'+
      '34673468346934743475347634773478347934843485348634'+
      '87348834893494349534963497349834993535363537353835'+
      '39354435453546354735483549355435553556355735583559'+
      '35643565356635673568356935743575357635773578357935'+
      '84358535863587358835893594359535963597359835993636'+
      '37363836393644364536463647364836493654365536563657'+
      '36583659366436653666366736683669367436753676367736'+
      '78367936843685368636873688368936943695369636973698'+
      '36993737383739374437453746374737483749375437553756'+
      '37573758375937643765376637673768376937743775377637'+
      '77377837793784378537863787378837893794379537963797'+
      '37983799383839384438453846384738483849385438553856'+
      '38573858385938643865386638673868386938743875387638'+
      '77387838793884388538863887388838893894389538963897'+
      '38983899393944394539463947394839493954395539563957'+
      '39583959396439653966396739683969397439753976397739'+
      '78397939843985398639873988398939943995399639973998'+
      '39994444544464447444844494455445644574458445944654'+
      '46644674468446944754476447744784479448544864487448'+
      '84489449544964497449844994545464547454845494555455'+
      '64557455845594565456645674568456945754576457745784'+
      '57945854586458745884589459545964597459845994646474'+
      '64846494655465646574658465946654666466746684669467'+
      '54676467746784679468546864687468846894695469646974'+
      '69846994747484749475547564757475847594765476647674'+
      '76847694775477647774778477947854786478747884789479'+
      '54796479747984799484849485548564857485848594865486'+
      '64867486848694875487648774878487948854886488748884'+
      '88948954896489748984899494955495649574958495949654'+
      '96649674968496949754976497749784979498549864987498'+
      '84989499549964997499849995555655575558555955665567'+
      '55685569557655775578557955865587558855895596559755'+
      '98559956565756585659566656675668566956765677567856'+
      '79568656875688568956965697569856995757585759576657'+
      '67576857695776577757785779578657875788578957965797'+
      '57985799585859586658675868586958765877587858795886'+
      '58875888588958965897589858995959665967596859695976'+
      '59775978597959865987598859895996599759985999666676'+
      '66866696677667866796687668866896697669866996767686'+
      '76967776778677967876788678967976798679968686968776'+
      '87868796887688868896897689868996969776978697969876'+
      '98869896997699869997777877797788778977987799787879'+
      '78887889789878997979887989799879998888988998989999'+
      '000{'#$04#$04#$00#$00#$01#$00#$00''+
      #$FF#$FF#$FF#$FF#$00#$00#$FF#$FF#$00#$FF#$FF#$FF#$00#$FF#$00#$FF''+
      #$FF#$FF#$00#$FF#$FF#$00#$00#$FF#$FF#$00#$FF#$FF#$00#$00#$00#$FF''+
      #$80#$80#$80#$FF#$C0#$C0#$C0#$FF#$00#$00#$FF#$FF#$7F#$7F#$FF#$FF''+
      #$00#$00#$A5#$FF#$52#$52#$A5#$FF#$00#$00#$7F#$FF#$3F#$3F#$7F#$FF''+
      #$00#$00#$4C#$FF#$26#$26#$4C#$FF#$00#$00#$26#$FF#$13#$13#$26#$FF''+
      #$00#$3F#$FF#$FF#$7F#$9F#$FF#$FF#$00#$29#$A5#$FF#$52#$67#$A5#$FF''+
      #$00#$1F#$7F#$FF#$3F#$4F#$7F#$FF#$00#$13#$4C#$FF#$26#$2F#$4C#$FF''+
      #$00#$09#$26#$FF#$13#$17#$26#$FF#$00#$7F#$FF#$FF#$7F#$BF#$FF#$FF''+
      #$00#$52#$A5#$FF#$52#$7C#$A5#$FF#$00#$3F#$7F#$FF#$3F#$5F#$7F#$FF''+
      #$00#$26#$4C#$FF#$26#$39#$4C#$FF#$00#$13#$26#$FF#$13#$1C#$26#$FF''+
      #$00#$BF#$FF#$FF#$7F#$DF#$FF#$FF#$00#$7C#$A5#$FF#$52#$91#$A5#$FF''+
      #$00#$5F#$7F#$FF#$3F#$6F#$7F#$FF#$00#$39#$4C#$FF#$26#$42#$4C#$FF''+
      #$00#$1C#$26#$FF#$13#$21#$26#$FF#$00#$FF#$FF#$FF#$7F#$FF#$FF#$FF''+
      #$00#$A5#$A5#$FF#$52#$A5#$A5#$FF#$00#$7F#$7F#$FF#$3F#$7F#$7F#$FF''+
      #$00#$4C#$4C#$FF#$26#$4C#$4C#$FF#$00#$26#$26#$FF#$13#$26#$26#$FF''+
      #$00#$FF#$BF#$FF#$7F#$FF#$DF#$FF#$00#$A5#$7C#$FF#$52#$A5#$91#$FF''+
      #$00#$7F#$5F#$FF#$3F#$7F#$6F#$FF#$00#$4C#$39#$FF#$26#$4C#$42#$FF''+
      #$00#$26#$1C#$FF#$13#$26#$21#$FF#$00#$FF#$7F#$FF#$7F#$FF#$BF#$FF''+
      #$00#$A5#$52#$FF#$52#$A5#$7C#$FF#$00#$7F#$3F#$FF#$3F#$7F#$5F#$FF''+
      #$00#$4C#$26#$FF#$26#$4C#$39#$FF#$00#$26#$13#$FF#$13#$26#$1C#$FF''+
      #$00#$FF#$3F#$FF#$7F#$FF#$9F#$FF#$00#$A5#$29#$FF#$52#$A5#$67#$FF''+
      #$00#$7F#$1F#$FF#$3F#$7F#$4F#$FF#$00#$4C#$13#$FF#$26#$4C#$2F#$FF''+
      #$00#$26#$09#$FF#$13#$26#$17#$FF#$00#$FF#$00#$FF#$7F#$FF#$7F#$FF''+
      #$00#$A5#$00#$FF#$52#$A5#$52#$FF#$00#$7F#$00#$FF#$3F#$7F#$3F#$FF''+
      #$00#$4C#$00#$FF#$26#$4C#$26#$FF#$00#$26#$00#$FF#$13#$26#$13#$FF''+
      #$3F#$FF#$00#$FF#$9F#$FF#$7F#$FF#$29#$A5#$00#$FF#$67#$A5#$52#$FF''+
      #$1F#$7F#$00#$FF#$4F#$7F#$3F#$FF#$13#$4C#$00#$FF#$2F#$4C#$26#$FF''+
      #$09#$26#$00#$FF#$17#$26#$13#$FF#$7F#$FF#$00#$FF#$BF#$FF#$7F#$FF''+
      #$52#$A5#$00#$FF#$7C#$A5#$52#$FF#$3F#$7F#$00#$FF#$5F#$7F#$3F#$FF''+
      #$26#$4C#$00#$FF#$39#$4C#$26#$FF#$13#$26#$00#$FF#$1C#$26#$13#$FF''+
      #$BF#$FF#$00#$FF#$DF#$FF#$7F#$FF#$7C#$A5#$00#$FF#$91#$A5#$52#$FF''+
      #$5F#$7F#$00#$FF#$6F#$7F#$3F#$FF#$39#$4C#$00#$FF#$42#$4C#$26#$FF''+
      #$1C#$26#$00#$FF#$21#$26#$13#$FF#$FF#$FF#$00#$FF#$FF#$FF#$7F#$FF''+
      #$A5#$A5#$00#$FF#$A5#$A5#$52#$FF#$7F#$7F#$00#$FF#$7F#$7F#$3F#$FF''+
      #$4C#$4C#$00#$FF#$4C#$4C#$26#$FF#$26#$26#$00#$FF#$26#$26#$13#$FF''+
      #$FF#$BF#$00#$FF#$FF#$DF#$7F#$FF#$A5#$7C#$00#$FF#$A5#$91#$52#$FF''+
      #$7F#$5F#$00#$FF#$7F#$6F#$3F#$FF#$4C#$39#$00#$FF#$4C#$42#$26#$FF''+
      #$26#$1C#$00#$FF#$26#$21#$13#$FF#$FF#$7F#$00#$FF#$FF#$BF#$7F#$FF''+
      #$A5#$52#$00#$FF#$A5#$7C#$52#$FF#$7F#$3F#$00#$FF#$7F#$5F#$3F#$FF''+
      #$4C#$26#$00#$FF#$4C#$39#$26#$FF#$26#$13#$00#$FF#$26#$1C#$13#$FF''+
      #$FF#$3F#$00#$FF#$FF#$9F#$7F#$FF#$A5#$29#$00#$FF#$A5#$67#$52#$FF''+
      #$7F#$1F#$00#$FF#$7F#$4F#$3F#$FF#$4C#$13#$00#$FF#$4C#$2F#$26#$FF''+
      #$26#$09#$00#$FF#$26#$17#$13#$FF#$FF#$00#$00#$FF#$FF#$7F#$7F#$FF''+
      #$A5#$00#$00#$FF#$A5#$52#$52#$FF#$7F#$00#$00#$FF#$7F#$3F#$3F#$FF''+
      #$4C#$00#$00#$FF#$4C#$26#$26#$FF#$26#$00#$00#$FF#$26#$13#$13#$FF''+
      #$FF#$00#$3F#$FF#$FF#$7F#$9F#$FF#$A5#$00#$29#$FF#$A5#$52#$67#$FF''+
      #$7F#$00#$1F#$FF#$7F#$3F#$4F#$FF#$4C#$00#$13#$FF#$4C#$26#$2F#$FF''+
      #$26#$00#$09#$FF#$26#$13#$17#$FF#$FF#$00#$7F#$FF#$FF#$7F#$BF#$FF''+
      #$A5#$00#$52#$FF#$A5#$52#$7C#$FF#$7F#$00#$3F#$FF#$7F#$3F#$5F#$FF''+
      #$4C#$00#$26#$FF#$4C#$26#$39#$FF#$26#$00#$13#$FF#$26#$13#$1C#$FF''+
      #$FF#$00#$BF#$FF#$FF#$7F#$DF#$FF#$A5#$00#$7C#$FF#$A5#$52#$91#$FF''+
      #$7F#$00#$5F#$FF#$7F#$3F#$6F#$FF#$4C#$00#$39#$FF#$4C#$26#$42#$FF''+
      #$26#$00#$1C#$FF#$26#$13#$21#$FF#$FF#$00#$FF#$FF#$FF#$7F#$FF#$FF''+
      #$A5#$00#$A5#$FF#$A5#$52#$A5#$FF#$7F#$00#$7F#$FF#$7F#$3F#$7F#$FF''+
      #$4C#$00#$4C#$FF#$4C#$26#$4C#$FF#$26#$00#$26#$FF#$26#$13#$26#$FF''+
      #$BF#$00#$FF#$FF#$DF#$7F#$FF#$FF#$7C#$00#$A5#$FF#$91#$52#$A5#$FF''+
      #$5F#$00#$7F#$FF#$6F#$3F#$7F#$FF#$39#$00#$4C#$FF#$42#$26#$4C#$FF''+
      #$1C#$00#$26#$FF#$21#$13#$26#$FF#$7F#$00#$FF#$FF#$BF#$7F#$FF#$FF''+
      #$52#$00#$A5#$FF#$7C#$52#$A5#$FF#$3F#$00#$7F#$FF#$5F#$3F#$7F#$FF''+
      #$26#$00#$4C#$FF#$39#$26#$4C#$FF#$13#$00#$26#$FF#$1C#$13#$26#$FF''+
      #$3F#$00#$FF#$FF#$9F#$7F#$FF#$FF#$29#$00#$A5#$FF#$67#$52#$A5#$FF''+
      #$1F#$00#$7F#$FF#$4F#$3F#$7F#$FF#$13#$00#$4C#$FF#$2F#$26#$4C#$FF''+
      #$09#$00#$26#$FF#$17#$13#$26#$FF#$00#$00#$00#$FF#$2D#$2D#$2D#$FF''+
      #$5B#$5B#$5B#$FF#$89#$89#$89#$FF#$B7#$B7#$B7#$FF#$B3#$B3#$B3#$FF''+
      #$00#$00#$00#$FF#$00#$00#$FF#$FF#$00#$FF#$FF#$FF#$00#$FF#$00#$FF''+
      #$FF#$FF#$00#$FF#$FF#$00#$00#$FF#$FF#$00#$FF#$FF#$FF#$FF#$FF#$FF''+
      #$80#$80#$80#$FF#$C0#$C0#$C0#$FF#$00#$00#$FF#$FF#$7F#$7F#$FF#$FF''+
      #$00#$00#$CC#$FF#$66#$66#$CC#$FF#$00#$00#$99#$FF#$4C#$4C#$99#$FF''+
      #$00#$00#$7F#$FF#$3F#$3F#$7F#$FF#$00#$00#$4C#$FF#$26#$26#$4C#$FF''+
      #$00#$3F#$FF#$FF#$7F#$9F#$FF#$FF#$00#$33#$CC#$FF#$66#$7F#$CC#$FF''+
      #$00#$26#$99#$FF#$4C#$5F#$99#$FF#$00#$1F#$7F#$FF#$3F#$4F#$7F#$FF''+
      #$00#$13#$4C#$FF#$26#$2F#$4C#$FF#$00#$7F#$FF#$FF#$7F#$BF#$FF#$FF''+
      #$00#$66#$CC#$FF#$66#$99#$CC#$FF#$00#$4C#$99#$FF#$4C#$72#$99#$FF''+
      #$00#$3F#$7F#$FF#$3F#$5F#$7F#$FF#$00#$26#$4C#$FF#$26#$39#$4C#$FF''+
      #$00#$BF#$FF#$FF#$7F#$DF#$FF#$FF#$00#$99#$CC#$FF#$66#$B2#$CC#$FF''+
      #$00#$72#$99#$FF#$4C#$85#$99#$FF#$00#$5F#$7F#$FF#$3F#$6F#$7F#$FF''+
      #$00#$39#$4C#$FF#$26#$42#$4C#$FF#$00#$FF#$FF#$FF#$7F#$FF#$FF#$FF''+
      #$00#$CC#$CC#$FF#$66#$CC#$CC#$FF#$00#$99#$99#$FF#$4C#$99#$99#$FF''+
      #$00#$7F#$7F#$FF#$3F#$7F#$7F#$FF#$00#$4C#$4C#$FF#$26#$4C#$4C#$FF''+
      #$00#$FF#$BF#$FF#$7F#$FF#$DF#$FF#$00#$CC#$99#$FF#$66#$CC#$B2#$FF''+
      #$00#$99#$72#$FF#$4C#$99#$85#$FF#$00#$7F#$5F#$FF#$3F#$7F#$6F#$FF''+
      #$00#$4C#$39#$FF#$26#$4C#$42#$FF#$00#$FF#$7F#$FF#$7F#$FF#$BF#$FF''+
      #$00#$CC#$66#$FF#$66#$CC#$99#$FF#$00#$99#$4C#$FF#$4C#$99#$72#$FF''+
      #$00#$7F#$3F#$FF#$3F#$7F#$5F#$FF#$00#$4C#$26#$FF#$26#$4C#$39#$FF''+
      #$00#$FF#$3F#$FF#$7F#$FF#$9F#$FF#$00#$CC#$33#$FF#$66#$CC#$7F#$FF''+
      #$00#$99#$26#$FF#$4C#$99#$5F#$FF#$00#$7F#$1F#$FF#$3F#$7F#$4F#$FF''+
      #$00#$4C#$13#$FF#$26#$4C#$2F#$FF#$00#$FF#$00#$FF#$7F#$FF#$7F#$FF''+
      #$00#$CC#$00#$FF#$66#$CC#$66#$FF#$00#$99#$00#$FF#$4C#$99#$4C#$FF''+
      #$00#$7F#$00#$FF#$3F#$7F#$3F#$FF#$00#$4C#$00#$FF#$26#$4C#$26#$FF''+
      #$3F#$FF#$00#$FF#$9F#$FF#$7F#$FF#$33#$CC#$00#$FF#$7F#$CC#$66#$FF''+
      #$26#$99#$00#$FF#$5F#$99#$4C#$FF#$1F#$7F#$00#$FF#$4F#$7F#$3F#$FF''+
      #$13#$4C#$00#$FF#$2F#$4C#$26#$FF#$7F#$FF#$00#$FF#$BF#$FF#$7F#$FF''+
      #$66#$CC#$00#$FF#$99#$CC#$66#$FF#$4C#$99#$00#$FF#$72#$99#$4C#$FF''+
      #$3F#$7F#$00#$FF#$5F#$7F#$3F#$FF#$26#$4C#$00#$FF#$39#$4C#$26#$FF''+
      #$BF#$FF#$00#$FF#$DF#$FF#$7F#$FF#$99#$CC#$00#$FF#$B2#$CC#$66#$FF''+
      #$72#$99#$00#$FF#$85#$99#$4C#$FF#$5F#$7F#$00#$FF#$6F#$7F#$3F#$FF''+
      #$39#$4C#$00#$FF#$42#$4C#$26#$FF#$FF#$FF#$00#$FF#$FF#$FF#$7F#$FF''+
      #$CC#$CC#$00#$FF#$CC#$CC#$66#$FF#$99#$99#$00#$FF#$99#$99#$4C#$FF''+
      #$7F#$7F#$00#$FF#$7F#$7F#$3F#$FF#$4C#$4C#$00#$FF#$4C#$4C#$26#$FF''+
      #$FF#$BF#$00#$FF#$FF#$DF#$7F#$FF#$CC#$99#$00#$FF#$CC#$B2#$66#$FF''+
      #$99#$72#$00#$FF#$99#$85#$4C#$FF#$7F#$5F#$00#$FF#$7F#$6F#$3F#$FF''+
      #$4C#$39#$00#$FF#$4C#$42#$26#$FF#$FF#$7F#$00#$FF#$FF#$BF#$7F#$FF''+
      #$CC#$66#$00#$FF#$CC#$99#$66#$FF#$99#$4C#$00#$FF#$99#$72#$4C#$FF''+
      #$7F#$3F#$00#$FF#$7F#$5F#$3F#$FF#$4C#$26#$00#$FF#$4C#$39#$26#$FF''+
      #$FF#$3F#$00#$FF#$FF#$9F#$7F#$FF#$CC#$33#$00#$FF#$CC#$7F#$66#$FF''+
      #$99#$26#$00#$FF#$99#$5F#$4C#$FF#$7F#$1F#$00#$FF#$7F#$4F#$3F#$FF''+
      #$4C#$13#$00#$FF#$4C#$2F#$26#$FF#$FF#$00#$00#$FF#$FF#$7F#$7F#$FF''+
      #$CC#$00#$00#$FF#$CC#$66#$66#$FF#$99#$00#$00#$FF#$99#$4C#$4C#$FF''+
      #$7F#$00#$00#$FF#$7F#$3F#$3F#$FF#$4C#$00#$00#$FF#$4C#$26#$26#$FF''+
      #$FF#$00#$3F#$FF#$FF#$7F#$9F#$FF#$CC#$00#$33#$FF#$CC#$66#$7F#$FF''+
      #$99#$00#$26#$FF#$99#$4C#$5F#$FF#$7F#$00#$1F#$FF#$7F#$3F#$4F#$FF''+
      #$4C#$00#$13#$FF#$4C#$26#$2F#$FF#$FF#$00#$7F#$FF#$FF#$7F#$BF#$FF''+
      #$CC#$00#$66#$FF#$CC#$66#$99#$FF#$99#$00#$4C#$FF#$99#$4C#$72#$FF''+
      #$7F#$00#$3F#$FF#$7F#$3F#$5F#$FF#$4C#$00#$26#$FF#$4C#$26#$39#$FF''+
      #$FF#$00#$BF#$FF#$FF#$7F#$DF#$FF#$CC#$00#$99#$FF#$CC#$66#$B2#$FF''+
      #$99#$00#$72#$FF#$99#$4C#$85#$FF#$7F#$00#$5F#$FF#$7F#$3F#$6F#$FF''+
      #$4C#$00#$39#$FF#$4C#$26#$42#$FF#$FF#$00#$FF#$FF#$FF#$7F#$FF#$FF''+
      #$CC#$00#$CC#$FF#$CC#$66#$CC#$FF#$99#$00#$99#$FF#$99#$4C#$99#$FF''+
      #$7F#$00#$7F#$FF#$7F#$3F#$7F#$FF#$4C#$00#$4C#$FF#$4C#$26#$4C#$FF''+
      #$BF#$00#$FF#$FF#$DF#$7F#$FF#$FF#$99#$00#$CC#$FF#$B2#$66#$CC#$FF''+
      #$72#$00#$99#$FF#$85#$4C#$99#$FF#$5F#$00#$7F#$FF#$6F#$3F#$7F#$FF''+
      #$39#$00#$4C#$FF#$42#$26#$4C#$FF#$7F#$00#$FF#$FF#$BF#$7F#$FF#$FF''+
      #$66#$00#$CC#$FF#$99#$66#$CC#$FF#$4C#$00#$99#$FF#$72#$4C#$99#$FF''+
      #$3F#$00#$7F#$FF#$5F#$3F#$7F#$FF#$26#$00#$4C#$FF#$39#$26#$4C#$FF''+
      #$3F#$00#$FF#$FF#$9F#$7F#$FF#$FF#$33#$00#$CC#$FF#$7F#$66#$CC#$FF''+
      #$26#$00#$99#$FF#$5F#$4C#$99#$FF#$1F#$00#$7F#$FF#$4F#$3F#$7F#$FF''+
      #$13#$00#$4C#$FF#$2F#$26#$4C#$FF#$33#$33#$33#$FF#$5B#$5B#$5B#$FF''+
      #$84#$84#$84#$FF#$AD#$AD#$AD#$FF#$D6#$D6#$D6#$FF#$FF#$FF#$FF#$FF'';

// Extended Binary Opcodes
// $00
  cnstExtBinarySetColorMap                    = $0001;
  cnstExtBinaryDrawMappedBitonalImage         = $0002; //WD_IMAGE_BITONAL_MAPPED_EXT_OPCODE
  cnstExtBinaryDrawMappedGroup3XImage         = $0003; //WD_IMAGE_GROUP3X_MAPPED_EXT_OPCODE
  cnstExtBinaryDrawIndexedColorImage          = $0004; //WD_IMAGE_INDEXED_EXT_OPCODE
  cnstExtBinaryDrawMappedColorImage           = $0005; //WD_IMAGE_MAPPED_EXT_OPCODE
  cnstExtBinaryDrawRGBImage                   = $0006; //WD_IMAGE_RGB_EXT_OPCODE
  cnstExtBinaryDrawRGBAImage                  = $0007; //WD_IMAGE_RGBA_EXT_OPCODE
  cnstExtBinaryDrawJPEGImage                  = $0008; //WD_IMAGE_JPEG_EXT_OPCODE
  cnstExtBinaryDrawGroup4PNGImage0            = $0009; //WD_IMAGE_GROUP4_BITONAL_EXT_OPCODE
  cnstExtBinaryDrawGroup4PNGImage1            = $000C; //WD_IMAGE_PNG_EXT_OPCODE
  cnstExtBinaryDrawGroup4XMappedExt           = $000D; //WD_IMAGE_GROUP4X_MAPPED_EXT_OPCODE


//  DefineCompressedData0          = $0010;
//  DefineCompressedData1          = $0011;
// $01
  cnstExtBinaryDefineGuid                     = $014A;
  cnstExtBinaryDefinePassword                 = $014B;
  cnstExtBinaryDefineFileTime                 = $014D;
  cnstExtBinaryDefineBlockMeaning             = $0142;
  cnstExtBinaryDefineEncryption               = $0143;
  cnstExtBinaryDefineOrientation              = $0145;
  cnstExtBinaryDefineAlignment                = $0147;
  cnstExtBinaryDirectory                      = $0160;
  cnstExtBinaryDefineUserData                 = $0162;
  cnstExtBinarySetPenPattern                  = $0164;
  cnstExtBinaryDefineSignData                 = $0166;
  cnstExtBinaryDefineGuidList                 = $0168;
  cnstExtBinaryTextHAlign                     = $0173; //#define WD_EXBO_TEXT_HALIGN
  cnstExtBinaryTextVAlign                     = $0175; //#define WD_EXBO_TEXT_VALIGN

  // Block ref
  cnstExtBinaryGraphicsHdr                    = 335; // $014F //WD_EXBO_
  cnstExtBinaryOverlayHdr                     = 336;
  cnstExtBinaryRedlineHdr                     = 337;
  cnstExtBinaryThumbnail                      = 338;
  cnstExtBinaryPreview                        = 339;
  cnstExtBinaryPverlayPreview                 = 340;
  cnstExtBinaryFont                           = 341;
  cnstExtBinaryGraphics                       = 342;
  cnstExtBinaryOverlay                        = 343;
  cnstExtBinaryRedline                        = 344;
  cnstExtBinaryUser                           = 345;
  cnstExtBinaryNull                           = 346;
  cnstExtBinaryGlobalSheet                    = 347;
  cnstExtBinaryGlobal                         = 348;
  cnstExtBinarySignature                      = 349; //$015D
  cnstExtBinaryBlockRef0                      = 350; //$015E
  cnstExtBinaryBlockRef1                      = 351; //$015F

type
  TsgVAlign = (vaDescentline, vaBaseline, vaHalfline, vaCapline, Ascentline);
  TsgHAlign = (haLeft, haCenter, haRight);


const
  cnstVAlignNames: array[TsgVAlign] of string = (
    'Descentline', 'Baseline', 'Halfline', 'Capline', 'Ascentline');
  cnstHAlignNames: array[TsgHAlign] of string = (
    'Left', 'Center', 'Right');


type
  TwxdParser = class;
  Tw2dParser = class;
  Tw3dParser = class;

  TdwfIdHeader= packed record
    FormatTag: array [0 .. 5] of Byte;
//  VersionTag: Byte;
    MajorRevision: array [0 .. 1] of Byte;
    Period: Byte;
    MinorRevision: array [0 .. 1] of Byte;
  end;
  Tw2dParserProc = procedure of object;
  Tw2dParserProgressEvent = procedure(ASender: TObject; APercent: Double) of object;

{ TsgW2DImage
  Top-level class. }
  TsgW2DImage = class(TsgVectorImage)
  private
    FLayoutName: string;
    Fw2dParser: Tw2dParser;
    procedure CreateParser;
    procedure DoProgress(ASender: TObject; APercent: Double);
    procedure FreeParser;
  protected
    procedure ApplyLineWeightFactor(var ALineWeight: Double); override;
  public
    procedure LoadFromFile(const AFileName: string); override;
    procedure LoadFromStream(AStream: TStream); override;
    property LayoutName: string read FLayoutName write FLayoutName;
  end;

  Tw2dConverter = class(TsgDXFConverter)
  end;

  TwxdParser = class
  private
    //Former global variables
    //-->
    RedColor, GreenColor, BlueColor, AlphaColor: Integer;
    IsColorInd: Boolean;
    CurrColor_: TColor;
    LineWeight: Integer;
    CustomColorsCnt: Integer;
    CurrColorMap: PByteArray;
    VisibleMode, FillMode: Boolean;
    FontHeight: Integer;
    FontRotation: Double;

    gTextHAlign: TsgHAlign;
    gTextVAlign: TsgVAlign;
    //<--
    FBackgroundColor: Integer;
    FIsSetBackgroundColor: Boolean;
    FCnt: Integer;
    FConv: TsgDXFConverter;
    FDataLength: Integer;
    FEntities: TsgObjectList;
    FOnProgress: Tw2dParserProgressEvent;
    FPData: PData;
    FPercent: Double;
    FPoint: TFPoint;
    FStyle: TsgDXFStyle;
    FTagCnt: Integer;
    FXScale: Double;
    FLayers: TsgCollection;
    FHyperLinkList: TsgObjectCollection;
    FCurrentURLIndex: Integer;
    FCurrentLayerName: string;
    FCurrentLayer : TsgDXFLayer;
    FUnitMatrix: TFMatrix;
    procedure DoProgress;
    function GetBackgroundColor: Integer;
    function GetByte: Byte;
    function GetEntitiesList: TsgObjectList;
    function GetLong: Integer;
    function GetSmall: Integer;
    function GetSmallint: Smallint;
    function GetUnsignedLong: Integer;
    function GetCount: Word;
    function GetWord: Integer;
    procedure RestoreData(const AData: Pointer; const ACount: Integer;
      const ADataLen: Integer);
    procedure SetData(const AData: Pointer; const ACount: Integer;
      const ADataLen: Integer);
    procedure StoreData(var AData: Pointer; var ACount: Integer;
      var ADataLen: Integer; const AOffs: Cardinal = Cardinal(-1)); virtual;
    procedure TextStyle(const AName: string);
    function GetPDataArray(const AIndex: Integer): Byte;
    function GetPDataValue: Byte;
  protected
    function ReadCurrColor(const AByIndex: Boolean): TColor;
    function GetCurrentColor: TColor;
    procedure SetCurrentColor(const AColor: TColor);
    procedure Error(const AErrorString: string); virtual;
    procedure ErrorFmt(const AErrorString: string;
      AArgs: array of const); virtual;
    procedure GetEntities; virtual; abstract;
    function IsDWF: Boolean; virtual; abstract;
    property PDataArray[const AIndex: Integer]: Byte read GetPDataArray;
    property PDataValue: Byte read GetPDataValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(AConv: TsgDXFConverter; AStream: TStream);
    property BackgroundColor: Integer read GetBackgroundColor;
    property IsSetBackgroundColor: Boolean read FIsSetBackgroundColor;
    property UnitMatrix: TFMatrix read FUnitMatrix;
    property EntitiesList: TsgObjectList read GetEntitiesList;
    property OnProgress: Tw2dParserProgressEvent read FOnProgress write
      FOnProgress;
  end;

  Tw2dParser = class(TwxdParser)
  private
    FAbsX: Double;
    FAbsY: Double;
    FCustomColorMap: PByteArray;
    FVersion: Integer;
    FCurrentBlockRef: TsgDXFBlock;
    FShadowBlockRef: TsgDXFBlock;
    FLogBuff: PChar;
    FRelX: Double;
    FRelY: Double;
    FMarkerGlyph: Integer;
    procedure AddImage(const AFuncImageLoad: TsgProcImageLoad;
      const ACommand: Word; const ABmpType: TsgGraphicFormat);
    procedure AddImageUnSafe(const AFuncImageLoad: TsgProcImageLoad;
      const ACommand: Word; const ABmpType: TsgGraphicFormat);
    procedure AddEntity(const AEntity: TsgDXFEntity);
    procedure ContourSet(const AProc: Tw2dParserProc);
    procedure DecompressLZ;
    procedure DecompressZlib;
    procedure DoColorMap;
    procedure ExecuteExtASCII;
    procedure ExecuteExtBinary;
    procedure ExecuteOneByte;
    procedure GetAbsPointInt;
    function GetAngle: Double;
    function GetExtASCIIndex: Integer;
    function GetInt: Integer;
    function GetIntSkipWhiteSpace: Integer;
    function GetIntByName(const AName: string): Integer;
    procedure GetRelPointLong;
    procedure GetRelPointSmall;
    procedure GetUnits;
    function Get_FS: Double;
    procedure GetPNG;
    procedure GetRGBAInt;
    function GetText: string;
    function GetTextValue: string;
    procedure GetURL;
//    procedure Init(S : Tstream);
    procedure Line(ACount: Integer; AProc: Tw2dParserProc);
    procedure CreateAndAddImage(const ALowerLeftPointer,
      AUpperRightPointer: TFPoint; const ABmp: TBitmap;
      const ABmpType: Integer = Integer(efAuto));
    function NewArc: TsgDXFArc;
    function NewCircle: TsgDXFCircle;
    function NewEllipse(ACenter: TFPoint; AR1, AR2: Double): TsgDXFEllipse;
    function NewLine: TsgDXFLine;
    function NewPoint: TsgDXFPoint;
    function NewPolyline: TsgDXFPolyline;
    function NewText: TsgDXFText;
    procedure Parse;
    procedure PolyLine(ACount: Integer; AProc: Tw2dParserProc;
      AGouraud: Boolean);
    procedure PolyMarker(const AIsBinary: Boolean; AProc: Tw2dParserProc);
    procedure PolyTriangle(ACount: Integer; AProc: Tw2dParserProc;
      AGouraud: Boolean);
    procedure SetColorMapFromBinary;
    procedure SetLayer(const AIsBinary: Boolean);
    procedure SkipExtASCII;
//    procedure SkipExtBinary;
    procedure SkipWhiteSpace;
    procedure SkipCharSet(const ACharSet: TSysCharSet);
  protected
    procedure GetEntities; override;
    function IsDWF: Boolean; override;
    procedure SkipTo(const AChar: Char);
  public
    destructor Destroy; override;
  // Tmp
    //property LogBuff: PChar read FLogBuff write FLogBuff;
  end;

  Tw2dParserOverlay = class(Tw2dParser)
  protected
    function IsDWF: Boolean; override;
    procedure GetEntities; override;
  public

  end;

  Tw3dParser = class(TwxdParser)
  private
    FFlagSeenHeader: Boolean;
    FVersionW3D: Integer;
    FColor: TColor;
    FLWeight: Single;
    FTextRotation: Single;
    FTextSize: Single;
    FLastMask: Integer;
    FInsertPrev: TsgDXFInsert;
    FCurrentBlock: TsgDXFBlock;
    FCountPrevEntityBlock: Integer;
    FListShadowBlock: TList;
    FStackBlock: TsgStackObject;
    procedure BoundingInfo;
    procedure Circle(AIsCircle: Boolean);
    procedure Color;
    procedure ColorRGB;
    function GetPoint: TFPoint;
    function GetSingle: Single;
    function GetStr(ALen: Integer): string;
    function GetStringBT: string;
    function GetStringST: string;
    function GetStringVT: string;
    function GetStringVariableLength: string;
    function GetMatrix: TFMatrix;
    procedure Heuristics;
    procedure Line;
    procedure LinePattern;
    procedure LineType;
    procedure LineWeight;
    procedure Matrix;
    procedure TextureMatrix;
    function NewEntity(AClass: TsgDXFEntityClass): TsgDXFEntity;
    procedure Shell;
    function EnumerateEdges(AListFaces: TsgIntegerList; ASuboptions: Byte): Integer;
    procedure TrivialDecompressFaces(AListFaces: TsgIntegerList;
       AHasNegativeFaces: Integer);
    procedure UnpackMTable(AMData: PByte;
      MTable: PsgMTableInfo; var AProxyHash: TsgCollection);
    function DecompressPreprocess(AOpslen: Integer; AOps: PByte;
      MTable: PsgMTableInfo; offsets: TsgIntegerList; var ecount_out: Integer): Integer;
    function ProcessOpcodes(AOpslen: Integer; AOps: PByte;
      MTable: PsgMTableInfo; AEa: PsgHalfEdgeArray; vBufferFase: TsgIntegerList;
      var AOpcodePointcount: Integer): Integer;
    function PatchFaces( AOpcodePointCount: Integer; MTable: PsgMTableInfo;
      AProxyHash: TsgCollection; vBufferFase: TsgIntegerList;
      AEa: PsgHalfEdgeArray; associations: TsgIntegerList): Integer;
    function UnpackPointsAndNormals(AEdgebreakerHeader: PsgEdgebreakerHeader;
      AEa: PsgHalfEdgeArray; pcount: Integer; AProxyHash: TsgCollection; len: Integer;
      pdata: PByte; quantized_points, quantized_normals, tristrips: TsgIntegerList): Integer;
    procedure EdgebreakerDecompress(AMemory: TMemoryStream;
      APointsList: TsgSingleList; ANormalsList: TsgSingleList;
      vBufferFase: TsgIntegerList; AConfig: PsgEbDecompressConfigs;
      var by_tristrips_out: Boolean);
    procedure ResdOptionals(ASuboptions: Byte; ASuboptions2: Integer; ACountPoint: Integer;
      vBufferFase: TsgIntegerList; vPoly: TsgDXFPolyline);
    procedure ShellFormat1_Standard(ASuboptions: Byte; ASuboptions2: Integer);
    procedure ShellFormat2_Edgebreaker(ASuboptions: Byte; ASuboptions2: Integer);
    procedure ShellFormat3_BoundingOnly(ASuboptions: Byte; ASuboptions2: Integer);
    procedure ShellFormat4_Collection(ASuboptions: Byte; ASuboptions2: Integer);
    procedure ShellFormat5_NullShell(ASuboptions: Byte; ASuboptions2: Integer);
    procedure StyleSegment;
    procedure Polygon;
    procedure Comment;
    procedure Window_Frame;
    procedure File_Info;
    procedure OpenSegment;
    procedure CloseSegment;
    procedure IncludeSegment;
    procedure W3DImage;
    procedure User_Options;
    procedure TextAlignment;
    procedure Polyline;
    procedure TextFont;
    procedure Texture;
    procedure TextWithEncoding;
    procedure View;
    procedure Visibility;
    procedure Window;
    procedure NurbsCurve;
    procedure RenderingOptions;
    procedure DistantLight;
    procedure Terminator;
    procedure GeometryAttributes;
    procedure Reference;
    procedure Grid;
    procedure Renumber;
    procedure AreaLight;
    function ExtractName(const AName: string): string;
    procedure SetInitMatrix(const AInsert: TsgDXFInsert; const AMatrix: TFMatrix);
  protected
    procedure Decompress;
    procedure GetEntities; override;
    function IsDWF: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

//procedure InitW2D;

var
  CF_W2D: Word;

implementation

uses zlibpas_sg, DWG

{$IFNDEF SGFPC}
 {$IFNDEF SG_FIREMONKEY}
  {$IFDEF SGDEL_XE2}
    ,Vcl.Imaging.jpeg
  {$ELSE}
  ,jpeg
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFNDEF SGFPC}
  {$IFNDEF SG_FIREMONKEY}
    {$IFDEF SG_USE_PNG}
    ,pngimage
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFNDEF SG_FIREMONKEY}
  {$IFDEF SG_USE_TIFF}
    ,GraphicEx
  {$ENDIF}
{$ENDIF}
  , sgBitmap;

const

  cnstWHIP05_DWF_FILE_VERSION                                 = 17;
  cnstREVISION_WHEN_HISTORY_BUFFER_PRELOADED                  = 23;
  cnstWHIP10_DWF_FILE_VERSION                                 = 25;
  cnstWHIP20_DWF_FILE_VERSION                                 = 30;
  cnstREVISION_WHEN_SET_FONT_STRING_FIXED                     = 31;
  cnstREVISION_WHEN_DRAW_TEXT_ATTRIBUTES_REMOVED              = 32;
  cnstREVISION_WHEN_BINARY_COLOR_CHANGED_TO_RGBA_FROM_BGRA    = 33;
  cnstWHIP30_DWF_FILE_VERSION                                 = 34;
  cnstREVISION_WHEN_COMMA_FOR_DECIMAL_CORRECTED               = 35;
  cnstREVISION_WHEN_UNDERLINING_WAS_ADDED                     = 35;
  cnstHOMER_DWF_FILE_VERSION                                  = 35;
  cnstREVISION_WHEN_STRINGS_CHANGED_TO_UNICODE_FROM_ASCII     = 36;
  cnstWHIP31_DWF_FILE_VERSION                                 = 36;
  cnstREVISION_WHEN_HEIDI_DRAWING_CHANGED                     = 37;
  cnstREVISION_WHEN_WIDE_LINES_ADDED                          = 37;
  cnstREVISION_WHEN_MULTI_URLS_WERE_SUPPORTED                 = 37;
  cnstREVISION_WHEN_DEFAULT_COLORMAP_WAS_CHANGED              = 38;
  cnstREVISION_WHEN_ZLIB_COMPRESSION_IS_SUPPORTED             = 39;
  cnstREVISION_WHEN_LANDSCAPE_FLIP_ADDED_TO_PLOTINFO          = 40;
  cnstREVISION_WHEN_ROTATION_ADDED_TO_PLOTINFO                = 41;
  cnstREVISION_WHEN_CONTOUR_SET_USED_FOR_VIEWPORTS            = 42;
  cnstWHIP40_DWF_FILE_VERSION                                 = 42;
  cnstWHIP50_DWF_FILE_VERSION                                 = 50;
  cnstREVISION_WHEN_READ_WRITE_MODE_WAS_SUPPORTED             = 55;
  cnstWHIP52_DWF_FILE_VERSION                                 = 55;
  cnstREVISION_WHEN_PACKAGE_FORMAT_BEGINS                     = 600;
  cnstREVISION_WHEN_MACRO_IS_SUPPORTED                        = 601;
  cnstREVISION_WHEN_TEXT_ALIGNMENT_IS_SUPPORTED               = 601;





   //suboptions
   cnstTKSH_STANDARD                 = $00; //no special suboptions
   cnstTKSH_COMPRESSED_POINTS        = $01; //points are compressed
   cnstTKSH_COMPRESSED_FACES         = $02; //faces are compressed
   cnstTKSH_TRISTRIPS                = $04; //face list represents tristrips
   cnstTKSH_HAS_OPTIONALS            = $08; //optionals exist; refer to #Opt_Opcode
   cnstTKSH_FIRSTPASS                = $10; //this is not a refinement of a previously-seen entity
   cnstTKSH_BOUNDING_ONLY            = $20; //a bounding box representation only.
   cnstTKSH_CONNECTIVITY_COMPRESSION = $40; //the polyedra is compressed using the 'connectivity compression' algorithm
   cnstTKSH_EXPANDED                 = $80; //expanded suboptions exist; refer to #Suboptions2
   //suboptions2
   cnstTKSH2_COLLECTION              = $0001; //instead of a shell, this opcode contains a TKE_Terminate-terminated list of opcodes to be inserted into a shell's LOD.  Incompatible with TKSH_FIRSTPASS.
   cnstTKSH2_NULL                    = $0002; //an empty shell used only as a placeholder for later revisiting
   cnstTKSH2_HAS_NEGATIVE_FACES      = $0004; //negative faces exist
   cnstTKSH2_GLOBAL_QUANTIZATION     = $0008; //polyhedra points are globally quantized

  cnstOPT_TERMINATE                   = 0;  //terminate the list of optional attributes
  cnstOPT_ALL_NORMALS_COMPRESSED      = 1;  //set compressed normals on all vertices
  cnstOPT_NORMALS_COMPRESSED          = 2;  //set compressed normals on some vertices
  cnstOPT_ALL_PARAMETERS_COMPRESSED   = 3;  //set compressed parameters (texture coordinates) on all vertices
  cnstOPT_PARAMETERS_COMPRESSED       = 4;  //set compressed parameters on some vertices
  cnstOPT_ALL_VFCOLORS                = 5;  //set colors on all vertices, to apply to drawing of interpolated faces
  cnstOPT_VERTEX_FCOLORS              = 6;  //set colors on some vertices, to apply to drawing of interpolated faces
  cnstOPT_ALL_VECOLORS                = 7;  //set colors on all vertices, to apply to drawing of interpolated edges
  cnstOPT_VERTEX_ECOLORS              = 8;  //set colors on some vertices, to apply to drawing of interpolated edges
  cnstOPT_ALL_VMCOLORS                = 9;  //set colors on all vertices, to apply to drawing of vertex_markers
  cnstOPT_VERTEX_MCOLORS              = 10; //set colors on some vertices, to apply to drawing of vertex_markers
  cnstOPT_ALL_VFINDICES               = 11; //set colors-by-index on all vertices, to apply to drawing of interpolated faces
  cnstOPT_VERTEX_FINDICES             = 12; //set colors-by-index on some vertices, to apply to drawing of interpolated faces
  cnstOPT_ALL_VEINDICES               = 13; //set colors-by-index on all vertices, to apply to drawing of interpolated edges
  cnstOPT_VERTEX_EINDICES             = 14; //set colors-by-index on some vertices, to apply to drawing of interpolated edges
  cnstOPT_ALL_VMINDICES               = 15; //set colors-by-index on all vertices, to apply to drawing of vertex_markers
  cnstOPT_VERTEX_MINDICES             = 16; //set colors-by-index on some vertices, to apply to drawing of vertex_markers
  cnstOPT_ALL_NORMALS                 = 17; //set normals on all vertices
  cnstOPT_NORMALS                     = 18; //set normals on some vertices
  cnstOPT_ALL_NORMALS_POLAR           = 19; //set normals (specified as polar coordinates) on all vertices
  cnstOPT_NORMALS_POLAR               = 20; //set normals (specified as polar coordinates) on some vertices
  cnstOPT_ALL_VMVISIBILITIES          = 21; //put explicit local visibility setting on all vertex markers
  cnstOPT_VERTEX_MARKER_VISIBILITIES  = 22; //put explicit local visibility setting on some vertex markers
  cnstOPT_ALL_VMSYMBOLS               = 23; //put explicit local marker symbol setting on all vertices
  cnstOPT_VERTEX_MARKER_SYMBOLS       = 24; //put explicit local marker symbol setting on some vertices
  cnstOPT_ALL_VMSIZES                 = 25; //put explicit local size setting on all vertex markers
  cnstOPT_VERTEX_MARKER_SIZES         = 26; //put explicit local size setting on some vertex markers
  cnstOPT_PARAMETERS                  = 27; //set parameters (texture coordinates) on some vertices
  cnstOPT_ALL_PARAMETERS              = 28; //set parameters (texture coordinates) on all vertices
  //**
  cnstOPT_ALL_FACE_COLORS             = 33; //set colors on all faces
  cnstOPT_FACE_COLORS                 = 34; //set colors on some faces
  cnstOPT_ALL_FACE_INDICES            = 35; //set colors-by-index on all faces
  cnstOPT_FACE_INDICES                = 36; //set colors-by-index on some faces
  cnstOPT_ALL_FACE_NORMALS_POLAR      = 37; //set normals (specified as polar coordinates) on all faces
  cnstOPT_FACE_NORMALS_POLAR          = 38; //set normals (specified as polar coordinates) on some faces
  cnstOPT_ALL_FACE_VISIBILITIES       = 39; //put explicit local visibility setting on all faces
  cnstOPT_FACE_VISIBILITIES           = 40; //put explicit local visibility setting on some faces
  cnstOPT_ALL_FACE_PATTERNS           = 41; //put explicit local pattern setting on all faces
  cnstOPT_FACE_PATTERNS               = 42; //put explicit local pattern setting on some faces
  //**
  cnstOPT_FACE_REGIONS                = 51; // apply region identifiers to some faces (for the purposes of grouping faces so that their attributes can be set at once)
  //**
  cnstOPT_ALL_EDGE_COLORS             = 71; //set colors on all edges
  cnstOPT_EDGE_COLORS                 = 72; //set colors on some edges
  cnstOPT_ALL_EDGE_INDICES            = 73; //set colors-by-index on all edges
  cnstOPT_EDGE_INDICES                = 74; //set colors-by-index on some edges
  cnstOPT_ALL_EDGE_NORMALS_POLAR      = 75; //set normals (specified as polar coordinates) on all edges
  cnstOPT_EDGE_NORMALS_POLAR          = 76; //set normals (specified as polar coordinates) on some edges
  cnstOPT_ALL_EDGE_VISIBILITIES       = 77; //put explicit local visibility setting on all edges
  cnstOPT_EDGE_VISIBILITIES           = 78; //put explicit local visibility setting on some edges
  cnstOPT_ALL_EDGE_PATTERNS           = 79; //put explicit local pattern setting on all edges
  cnstOPT_EDGE_PATTERNS               = 80; //put explicit local pattern setting on some edges
  cnstOPT_ALL_EDGE_WEIGHTS            = 81; //put explicit local thickness setting on all edges
  cnstOPT_EDGE_WEIGHTS                = 82; //put explicit local thickness setting on some edges
  //**
  cnstOPT_RESERVED_SPATIAL            = 90;
  cnstOPT_ENTITY_ATTRIBUTES           = 99;

  //TKO_Font_Options {
  cnstTKO_Font_Names                    = $00000001;   //the name of the font (i.e. which font to use)
  cnstTKO_Font_Size                     = $00000002;   //the size at which to draw characters
  cnstTKO_Font_Size_Tolerance           = $00000004;   //the size tolerance outside of which fonts must be regenerated
  cnstTKO_Font_Transforms               = $00000008;   //transformation mode (set with a bit from TKO_Font_Transforms)
  cnstTKO_Font_Rotation                 = $00000010;   //character rotation, specified in degrees
  cnstTKO_Font_Slant                    = $00000020;   //character slant
  cnstTKO_Font_Width_Scale              = $00000040;   //scale factor for width
  cnstTKO_Font_Extended                 = $00000080;   //extends font options to a second byte
  cnstTKO_Font_Extended_Mask            = $FFFFFF00;   //internal use, indicates bits which require TKO_Font_Extended
  cnstTKO_Font_Extra_Space              = $00000100;   //in addition to the spacing specified within the font itself, the extra space to add between characters
  cnstTKO_Font_Line_Spacing             = $00000200;   //extra spacing between lines
  cnstTKO_Font_Outline                  = $00000400;   //draw only the outline (i.e. don't fill)
  cnstTKO_Font_Underline                = $00000800;   //add an underline to the font
  cnstTKO_Font_Strikethrough            = $00001000;   //add a strikethrough to the font
  cnstTKO_Font_Overline                 = $00002000;   //add an overline to the font
  cnstTKO_Font_Uniform_Spacing          = $00004000;   //force non-proportional spacing
  cnstTKO_Font_Extended2                = $00008000;   //for further expansion
  cnstTKO_Font_Extended2_Mask           = $FFFF0000;   //for further expansion
  cnstTKO_Font_Greeking_Limit           = $00010000;   //limit at which text may be replaced with a crude representation
  cnstTKO_Font_Fill_Edges               = $00020000;   //fill edges of characters to improve appearance ar small sizes
  cnstTKO_Font_Bold                     = $00040000;   //choose or simulate a bold variation
  cnstTKO_Font_Italic                   = $00080000;   //choose or simulate an italic variation
  cnstTKO_Font_Renderer                 = $00100000;   //limit font source
  cnstTKO_Font_Greeking_Mode            = $00200000;   //select how to draw (or not) greeked text
  cnstTKO_Font_Preference               = $00400000;   //select preferred drawing modes

  cnstTKO_Camera_Perspective_Bit  = $01;  //perspective bit setting
  cnstTKO_Camera_Stretched_Bit    = $02;  //stretched bit setting
  cnstTKO_Camera_Projection_Mask  = $03;  //self-explanatory
  cnstTKO_Camera_Orthographic     = $00;  //orthographic projection
  cnstTKO_Camera_Perspective      = $01;  //perspective projection
  cnstTKO_Camera_Stretched        = $02;  //stretched projection
  cnstTKO_Camera_Oblique_Y        = $04;  //oblique y setting
  cnstTKO_Camera_Oblique_X        = $08;  //oblique x setting
  cnstTKO_Camera_Oblique_Mask     = $0C;  //self-explanatory
  cnstTKO_Camera_Near_Limit       = $10;  //near limit setting

  cnstTKO_Heuristic_Hidden_Surfaces         = $00000001;
  cnstTKO_Heuristic_Backplane_Cull          = $00000002;
  cnstTKO_Heuristic_Polygon_Handedness      = $00000004;
  cnstTKO_Heuristic_Quick_Moves             = $00000008;
  cnstTKO_Heuristic_Partial_Erase           = $00000010;
  cnstTKO_Heuristic_Memory_Purge            = $00000020;
  cnstTKO_Heuristic_Related_Select_Limit    = $00000040;
  cnstTKO_Heuristic_Internal_Shell_Limit    = $00000080;
  cnstTKO_Heuristic_Clipping                = $00000100;
  cnstTKO_Heuristic_Transformations         = $00000200;
  cnstTKO_Heuristic_Intersecting_Polygons   = $00000400;
  cnstTKO_Heuristic_Polygon_Crossings       = $00000800;
  cnstTKO_Heuristic_Concave_Polygons        = $00001000;
  cnstTKO_Heuristic_Incremental_Updates     = $00002000;
  cnstTKO_Heuristic_Selection_Sorting       = $00004000;

  cnstTKO_Heuristic_Extended                = $00008000;  //extended bit

  cnstTKO_Heuristic_Culling                 = $00010000;
  cnstTKO_Heuristic_Exclude_Bounding        = $00020000;
  cnstTKO_Heuristic_Detail_Selection        = $00040000;
  cnstTKO_Heuristic_Ordered_Drawing         = $00080000;
  cnstTKO_Heuristic_Ordered_Unit            = $00100000;
  cnstTKO_Heuristic_Ordered_Weights         = $00200000;
  cnstTKO_Heuristic_Internal_Polyline_Limit = $00400000;
  cnstTKO_Heuristic_Ordered_Grid            = $00800000;

  cnstTKO_Heuristic_Selection_Level         = $01000000;
  cnstTKO_Heuristic_Static                  = $02000000;

  cnstTKO_Heuristic_Extras                = cnstTKO_Heuristic_Polygon_Handedness or
    cnstTKO_Heuristic_Quick_Moves;

  cnstTKO_Heur_Extra_Left_Handed_Polys    = $01;
  cnstTKO_Heur_Extra_Quick_Move_Spriting  = $02;

  cnstTKO_Heur_View_Frustum_Culling       = $0001;
  cnstTKO_Heur_Obscuration_Culling        = $0002;
  cnstTKO_Heur_Extent_Culling             = $0004;
  cnstTKO_Heur_View_Frustum_Culling_Off   = $0010;
  cnstTKO_Heur_Obscuration_Culling_Off    = $0020;
  cnstTKO_Heur_Extent_Culling_Off         = $0040;
  cnstTKO_Heur_Culling_Extended           = $0080;
  cnstTKO_Heur_Obscuration_Use_Octree     = $0100;
  cnstTKO_Heur_Maximum_Extent_Mode        = $0200;
  cnstTKO_Heur_Vector_Culling             = $0400;
  cnstTKO_Heur_Vector_Tolerance           = $0800;
  cnstTKO_Heur_Vector_Culling_Off         = $1000;
  cnstTKO_Heur_Vector_Tolerance_Off       = $2000;
  cnstTKO_Heur_Culling_Extended_Mask      = $FF00;

  cnstTKO_Heur_Max_Extent_Mode_None       = 0;
  cnstTKO_Heur_Max_Extent_Mode_Dot        = 1;
  cnstTKO_Heur_Max_Extent_Mode_Bounding   = 2;
  cnstTKO_Heur_Max_Extent_Mode_Defer      = 3;

  cnstTKO_Heur_Order_World_Volume         = 0;
  cnstTKO_Heur_Order_Screen_Extent        = 1;
  cnstTKO_Heur_Order_Distance             = 2;
  cnstTKO_Heur_Order_Divergence           = 3;
  cnstTKO_Heur_Order_Density              = 4;
  cnstTKO_Heur_Order_Priority             = 5;
  cnstTKO_Heur_Order_Count                = 6;

  cnstTKO_Heur_Selection_Level_Entity       = 0;
  cnstTKO_Heur_Selection_Level_Segment      = 1;
  cnstTKO_Heur_Selection_Level_Segment_Tree = 2;

  cnstTKO_Heur_Force_Enum_Signed_Dummy_Value = -1; //hack: prevent some compilers from assuming enum is unsigned

  cnstCS_INVALID                     = 0;
  cnstCS_TRIVIAL                     = 1;
  cnstCS_TRIVIAL_IMPLICIT            = 2;
  cnstCS_REPULSE                     = 3;
  cnstCS_NONE                        = 4;
  cnstCS_EDGEBREAKER                 = 5;
  cnstCS_LINE_EXTEND                 = 6;
  cnstCS_TRIVIAL_POLAR               = 7;

  // first byte is common/shared items, plus flag for extended bits
  cnstTKO_Geo_Face            = $00000001;
  cnstTKO_Geo_Edge            = $00000002;
  cnstTKO_Geo_Line            = $00000004;
  cnstTKO_Geo_Marker          = $00000008;
  cnstTKO_Geo_Text            = $00000010;
  cnstTKO_Geo_Window          = $00000020;
  cnstTKO_Geo_Image           = $00000040;
  cnstTKO_Geo_Extended        = $00000080;
  cnstTKO_Geo_Extended_Mask   = $FFFFFF00;

  // extras for color
  cnstTKO_Geo_Ambient         = $00000100;
  cnstTKO_Geo_Light           = $00000200;
  cnstTKO_Geo_Face_Contrast   = $00000400;
  cnstTKO_Geo_Window_Contrast = $00000800;
  cnstTKO_Geo_Front           = $00001000;
  cnstTKO_Geo_Back            = $00002000;
  cnstTKO_Geo_Vertex          = $00004000;

  cnstTKO_Geo_Geom_Colors     = $0000701F;
  cnstTKO_Geo_Every_Colors    = $000073BF;
  cnstTKO_Geo_Extended_Colors = $00008000;
  cnstTKO_Geo_Extended_Colors_Mask = $FFFF0000;

  cnstTKO_Geo_Edge_Contrast   = $00010000;
  cnstTKO_Geo_Line_Contrast   = $00020000;
  cnstTKO_Geo_Marker_Contrast = $00040000;
  cnstTKO_Geo_Vertex_Contrast = $00080000;
  cnstTKO_Geo_Cut_Edge        = $00100000;
  cnstTKO_Geo_Simple_Reflection=$00200000;
  cnstTKO_Geo_Cut_Face        = $00400000;
  cnstTKO_Geo_Extended2       = $00800000;
  cnstTKO_Geo_Extended2_Mask  = $FF000000;
  cnstTKO_Geo_Text_Contrast   = $01000000;
  cnstTKO_Geo_All_Colors      = $017F7F7F;

  //extras for selectability (light is same as for color)
  cnstTKO_Geo_String_Cursor   = $00000100;

  cnstTKO_Geo_Geom_Selects    = $0000435F;
  cnstTKO_Geo_All_Selects     = $0000437F;

  // extras for visibility (string cursor same as selectability)
  cnstTKO_Geo_Face_Lighting   = $00000200;
  cnstTKO_Geo_Edge_Lighting   = $00000400;
  cnstTKO_Geo_Marker_Lighting = $00000800;
  cnstTKO_Geo_Light_Visibles  = $00000E00;

  cnstTKO_Geo_Silhouette_Edge = $00001000;
  cnstTKO_Geo_Perimeter_Edge  = $00002000;
  cnstTKO_Geo_Mesh_Quad       = $00004000;
  cnstTKO_Geo_Hard_Edge       = $00008000;

  cnstTKO_Geo_Cutting_Plane   = $00010000;
  cnstTKO_Geo_Shadow_Emit     = $00020000;
  cnstTKO_Geo_Shadow_Cast     = $00040000;
  cnstTKO_Geo_Shadow_Receive  = $00080000;
  cnstTKO_Geo_Shadow_Visibles = $000E0000;
  cnstTKO_Geo_Vertex_Vis      = $00200000;
  cnstTKO_Geo_Cut_Geometry    = $00500000;
  cnstTKO_Geo_Adjacent_Edge   = $01000000;
  cnstTKO_Geo_NonCulled_Edge  = $02000000;
  cnstTKO_Geo_Edge_Visibles   = $0300F002;

  cnstTKO_Geo_Geom_Visibles   = $0301FFFF;

  cnstTKO_Geo_All_Visibles    = $037FFF7F;


  cnstTKO_Channel_Diffuse         = 0; //self-explanatory
  cnstTKO_Channel_Specular        = 1; //self-explanatory
  cnstTKO_Channel_Mirror          = 2; //self-explanatory
  cnstTKO_Channel_Transmission    = 3; //self-explanatory
  cnstTKO_Channel_Emission        = 4; //self-explanatory
  cnstTKO_Channel_Gloss           = 5; //self-explanatory
  cnstTKO_Channel_Index           = 6; //color by index
  cnstTKO_Channel_Extended        = 7; //extended bit
  cnstTKO_Channel_Environment     = 8; //env map
  cnstTKO_Channel_Bump            = 9; //bump map

  cnstTKO_Channel_Count           = 10;


  cnstTKO_Texture_Param_Source    = $00000001;   //refer to ::HC_Define_Texture
  cnstTKO_Texture_Tiling          = $00000002;
  cnstTKO_Texture_Interpolation   = $00000004;
  cnstTKO_Texture_Decimation      = $00000008;
  cnstTKO_Texture_Red_Mapping     = $00000010;
  cnstTKO_Texture_Green_Mapping   = $00000020;
  cnstTKO_Texture_Blue_Mapping    = $00000040;
  cnstTKO_Texture_Alpha_Mapping   = $00000080;
  cnstTKO_Texture_Param_Function  = $00000100;
  cnstTKO_Texture_Layout          = $00000200;
  cnstTKO_Texture_Transform       = $00000400;
  cnstTKO_Texture_Value_Scale     = $00000800;
  cnstTKO_Texture_Caching         = $00001000;
  cnstTKO_Texture_DownSample      = $00002000;
  cnstTKO_Texture_No_DownSample   = $00004000;
  cnstTKO_Texture_Extended        = $00008000;   // extended bit
  cnstTKO_Texture_Extended_Mask   = $FFFF0000;   // internal use, indicates bit which require TKO_Texture_Extended
  cnstTKO_Texture_Decal           = $00010000;
  cnstTKO_Texture_Modulate        = $00020000;
  cnstTKO_Texture_Param_Offset    = $00040000;

  // Represents various rendering properties
  cnstTKO_Interp_Texture_Faces            = $00000001;
  cnstTKO_Interp_Texture_Edges            = $00000002;
  cnstTKO_Interp_Texture_Markers          = $00000004;
  cnstTKO_Interp_Texture                  = $00000007;

  cnstTKO_Interp_Color_Faces              = $00000008;
  cnstTKO_Interp_Color_Edges              = $00000010;
  cnstTKO_Interp_Color_Markers            = $00000020;
  cnstTKO_Interp_Color                    = $00000038;

  cnstTKO_Interp_Index_Faces              = $00000040;
  cnstTKO_Interp_Index_Edges              = $00000080;
  cnstTKO_Interp_Index                    = $000000C0;

  cnstTKO_Interp_Lighting_Faces_Gouraud   = $00000100;
  cnstTKO_Interp_Lighting_Faces_Phong     = $00000200;
  cnstTKO_Interp_Lighting_Edges_Gouraud   = $00000400;
  cnstTKO_Interp_Lighting_Edges_Phong     = $00000800;
  cnstTKO_Interp_Lighting_Faces           = $00000300;
  cnstTKO_Interp_Lighting_Edges           = $00000C00;
  cnstTKO_Interp_Lighting                 = $00000F00;

  cnstTKO_Rendo_HSR_Algorithm             = $00001000;
  cnstTKO_Rendo_THSR_Algorithm            = $00002000;
  cnstTKO_Rendo_Any_HSR                   = $00003000;

  cnstTKO_Rendo_Local_Viewer              = $00004000;
  cnstTKO_Rendo_Perspective_Correction    = $00008000;
  cnstTKO_Rendo_Display_Lists             = $00010000;

  cnstTKO_Rendo_Debug                     = $00020000;

  cnstTKO_Rendo_Technology                = $00040000;
  cnstTKO_Rendo_Quantization              = $00080000;
  cnstTKO_Rendo_TQ                        = $000C0000;

  cnstTKO_Rendo_Attribute_Lock            = $00100000;

  cnstTKO_Rendo_Face_Displacement         = $00200000;
  cnstTKO_Rendo_Fog                       = $00400000;

  cnstTKO_Rendo_Buffer_Options            = $00800000;
  cnstTKO_Rendo_Hidden_Line_Options       = $01000000;

  cnstTKO_Rendo_LOD                       = $02000000;
  cnstTKO_Rendo_LOD_Options               = $04000000;

  cnstTKO_Rendo_NURBS_Curve_Options       = $08000000;
  cnstTKO_Rendo_NURBS_Surface_Options     = $10000000;
  cnstTKO_Rendo_NURBS_Options             = $18000000;

  cnstTKO_Rendo_Stereo                    = $20000000;
  cnstTKO_Rendo_Stereo_Separation         = $40000000;

  cnstTKO_Rendo_Extended              = $80000000;


  // Represents attribute lock types for various attributes;

  cnstTKO_Lock_Callback                   = $00000001;
  cnstTKO_Lock_Camera                     = $00000002;
  cnstTKO_Lock_Color                      = $00000004;
  cnstTKO_Lock_Color_Map                  = $00000008;
  cnstTKO_Lock_Driver                     = $00000010;
  cnstTKO_Lock_Driver_Options             = $00000020;
  cnstTKO_Lock_Edge_Pattern               = $00000040;
  cnstTKO_Lock_Edge_Weight                = $00000080;
  cnstTKO_Lock_Face_Pattern               = $00000100;
  cnstTKO_Lock_Handedness                 = $00000200;
  cnstTKO_Lock_Heuristics                 = $00000400;
  cnstTKO_Lock_Line_Pattern               = $00000800;
  cnstTKO_Lock_Line_Weight                = $00001000;
  cnstTKO_Lock_Marker_Size                = $00002000;
  cnstTKO_Lock_Marker_Symbol              = $00004000;
  cnstTKO_Lock_Metafile                   = $00008000;
  cnstTKO_Lock_Modelling_Matrix           = $00010000;
  cnstTKO_Lock_Rendering_Options          = $00020000;
  cnstTKO_Lock_Selectability              = $00040000;
  cnstTKO_Lock_Styles                     = $00080000;
  cnstTKO_Lock_Text_Alignment             = $00100000;
  cnstTKO_Lock_Text_Font                  = $00200000;
  cnstTKO_Lock_Text_Path                  = $00400000;
  cnstTKO_Lock_Text_Spacing               = $00800000;
  cnstTKO_Lock_User_Options               = $01000000;
  cnstTKO_Lock_User_Value                 = $02000000;
  cnstTKO_Lock_Texture_Matrix             = $04000000;
  cnstTKO_Lock_Visibility                 = $08000000;
  cnstTKO_Lock_Window                     = $10000000;
  cnstTKO_Lock_Window_Frame               = $20000000;
  cnstTKO_Lock_Window_Pattern             = $40000000;
  cnstTKO_Lock_All                        = $7FFFFFFF;

  cnstTKO_Rendo_Tessellation              = $00000001;
  cnstTKO_Rendo_Transparency_Style        = $00000002;
  cnstTKO_Rendo_Transparency_Hardware     = $00000004;
  cnstTKO_Rendo_Cut_Geometry              = $00000008;
  cnstTKO_Rendo_Depth_Range               = $00000010;
  cnstTKO_Rendo_Mask_Transform            = $00000020;
  cnstTKO_Rendo_Image_Scale               = $00000040;
  cnstTKO_Rendo_Local_Cutting_Planes      = $00000080;
  cnstTKO_Rendo_Simple_Shadow             = $00000100;
  cnstTKO_Rendo_Geometry_Options          = $00000200;
  cnstTKO_Rendo_Image_Tint                = $00000400;
  cnstTKO_Interp_Index_Face_Isolines      = $00000800;
  cnstTKO_Rendo_Force_Grayscale           = $00001000;
  cnstTKO_Rendo_Transparency_Options      = $00002000;
  cnstTKO_Rendo_General_Displacement      = $00004000;
  cnstTKO_Rendo_Join_Cutoff_Angle         = $00008000;
  cnstTKO_Rendo_Screen_Range              = $00010000;
  cnstTKO_Rendo_Stereo_Distance           = $00020000;
  cnstTKO_Rendo_Shadow_Map                = $00040000;
  cnstTKO_Rendo_Simple_Reflection         = $00080000;

  cnstTKO_Buffer_Size_Limit               = $01;
  cnstTKO_Buffer_Retention                = $02;
  cnstTKO_Buffer_Color_Depth_Match        = $04;
  cnstTKO_Buffer_Color_Depth_Full         = $08;

  cnstTKO_Hidden_Line_Visibility_On           = $00000001;
  cnstTKO_Hidden_Line_Visibility_Off          = $00000002;
  cnstTKO_Hidden_Line_Pattern                 = $00000004;
  cnstTKO_Hidden_Line_Face_Displacement       = $00000008;
  cnstTKO_Hidden_Line_Dim_Factor              = $00000010;
  cnstTKO_Hidden_Line_Render_Faces_On         = $00000020;
  cnstTKO_Hidden_Line_Render_Faces_Off        = $00000040;
  cnstTKO_Hidden_Line_Extended                = $00000080;
  cnstTKO_Hidden_Line_Extended_Mask           = $FFFFFF00;
  cnstTKO_Hidden_Line_Silhouette_Cleanup_On   = $00000100;
  cnstTKO_Hidden_Line_Silhouette_Cleanup_Off  = $00000200;
  cnstTKO_Hidden_Line_Extended2               = $00008000;
  cnstTKO_Hidden_Line_Extended2_Mask          = $FFFF0000;
  cnstTKO_Hidden_Line_Color                   = $00010000;
  cnstTKO_Hidden_Line_Weight                  = $00020000;
  cnstTKO_Hidden_Line_Image_Outline_On        = $00040000;
  cnstTKO_Hidden_Line_Image_Outline_Off       = $00080000;
  cnstTKO_Hidden_Line_HSR_Algorithm           = $00100000;

  cnstTKO_NURBS_Curve_Budget              = $0001;
  cnstTKO_NURBS_Curve_Continued_Budget    = $0002;
  cnstTKO_NURBS_Curve_View_Dependent      = $0004;
  cnstTKO_NURBS_Curve_Max_Deviation       = $0008;
  cnstTKO_NURBS_Surface_Budget            = $0010;
  cnstTKO_NURBS_Surface_Trim_Budget       = $0020;
  cnstTKO_NURBS_Surface_Max_Facet_Width   = $0040;
  cnstTKO_NURBS_Curve_Max_Angle           = $1000;
  cnstTKO_NURBS_Curve_Max_Length          = $2000;

  cnstTKO_NURBS_Extended                  = $0080;
  cnstTKO_NURBS_Extended_Mask             = $FF00;

  cnstTKO_NURBS_Surface_Max_Facet_Angle   = $0100;
  cnstTKO_NURBS_Surface_Max_Facet_Deviation = $0200;
  cnstTKO_NURBS_Surface_Max_Trim_Curve_Deviation = $0400;

  cnstTKO_NURBS_Curve_Mask                = $F00F;
  cnstTKO_NURBS_Surface_Mask              = $0FF0;

  cnstTKO_LOD_Conserve_Memory             = $00000001;
  cnstTKO_LOD_Screen_Space                = $00000002;
  cnstTKO_LOD_Physical                    = $00000004;
  cnstTKO_LOD_Tolerance_FRU               = $00000008;
  cnstTKO_LOD_Tolerance_ORU               = $00000010;
  cnstTKO_LOD_Preprocess                  = $00000020;
  cnstTKO_LOD_Bounding_Current            = $00000040;
  cnstTKO_LOD_Bounding_Explicit           = $00000080;
  cnstTKO_LOD_Ratio                       = $00000100;
  cnstTKO_LOD_Threshold                   = $00000200;
  cnstTKO_LOD_Min_Triangle_Count          = $00000400;
  cnstTKO_LOD_Clamp                       = $00000800;
  cnstTKO_LOD_Num_Levels                  = $00001000;
  cnstTKO_LOD_Max_Degree                  = $00002000;
  cnstTKO_LOD_Tolerance                   = $00004000;
  cnstTKO_LOD_Usefulness_Heuristic        = $00008000;
  cnstTKO_LOD_Calculation_Cutoff          = $00010000;
  cnstTKO_LOD_Fallback                    = $00020000;
  cnstTKO_LOD_Collapse_Vertices           = $00040000;
  cnstTKO_LOD_Algorithm                   = $00080000;
  cnstTKO_LOD_Mode_Segment                = $00100000;

  cnstTKO_Tessellation_Cylinder           = $01;
  cnstTKO_Tessellation_Sphere             = $02;

  cnstTKO_Transparency_Peeling_Layers     = $10;
  cnstTKO_Transparency_Peeling_Min_Area   = $20;

  cnstTKO_Cut_Geometry_Level              = $01;
  cnstTKO_Cut_Geometry_Tolerance          = $02;
  cnstTKO_Cut_Geometry_Match_Color        = $04;

  cnstTKO_Simple_Shadow_On               = $0001;
  cnstTKO_Simple_Shadow_Off              = $0002;
  cnstTKO_Simple_Shadow_Plane            = $0004;
  cnstTKO_Simple_Shadow_Light_Direction  = $0008;
  cnstTKO_Simple_Shadow_Color            = $0010;
  cnstTKO_Simple_Shadow_Resolution       = $0020;
  cnstTKO_Simple_Shadow_Blur             = $0040;
  cnstTKO_Simple_Shadow_Extended         = $0080;
  cnstTKO_Simple_Shadow_Extended_Mask    = $FF00;
  cnstTKO_Simple_Shadow_Auto             = $0100;
  cnstTKO_Simple_Shadow_Opacity          = $0200;
  cnstTKO_Simple_Shadow_Extended2        = $8000;

  cnstTKO_Shadow_Map_On                  = $0001;
  cnstTKO_Shadow_Map_Off                 = $0002;
  cnstTKO_Shadow_Map_Resolution          = $0004;
  cnstTKO_Shadow_Map_Samples             = $0008;
  cnstTKO_Shadow_Map_Jitter_On           = $0010;
  cnstTKO_Shadow_Map_Jitter_Off          = $0020;
  cnstTKO_Shadow_Map_Extended            = $0080;

  cnstTKO_Simple_Reflection_On           = $0001;
  cnstTKO_Simple_Reflection_Off          = $0002;
  cnstTKO_Simple_Reflection_Plane        = $0004;
  cnstTKO_Simple_Reflection_Opacity      = $0008;
  cnstTKO_Simple_Reflection_Fading_On    = $0010;
  cnstTKO_Simple_Reflection_Fading_Off   = $0020;
  cnstTKO_Simple_Reflection_Blur         = $0040;
  cnstTKO_Simple_Reflection_Extended     = $0080;
  cnstTKO_Simple_Reflection_Attenuation  = $0100;
  cnstTKO_Simple_Reflection_Extended2    = $8000;

  cnstTKO_Geometry_Options_Dihedral                       = $01;
  cnstTKO_Geometry_Options_Reverse_PolyCylinder_Radii     = $02;
  cnstTKO_Geometry_Options_No_Reverse_PolyCylinder_Radii  = $04;
  cnstTKO_Geometry_Options_Reverse_PolyCylinder_Colors    = $08;
  cnstTKO_Geometry_Options_No_Reverse_PolyCylinder_Colors = $10;




  cnstMTABLE_HAS_LENGTHS = $1;
  cnstMTABLE_HAS_M2STACKOFFSETS = $2;
  cnstMTABLE_HAS_M2GATEOFFSETS = $4;
  cnstMTABLE_HAS_DUMMIES = $8;
  cnstMTABLE_HAS_PATCHES = $10;
  cnstMTABLE_HAS_BOUNDING = $20;
  cnstMTABLE_HAS_QUANTIZATION = $40;
  cnstMTABLE_HAS_QUANTIZATION_NORMALS = $80;

  cnstDEFAULT_QUANTIZATION = 11;

  GARBAGE_EDGE = Integer($80808080);
  GARBAGE_VERTEX = Integer($80808080);
  DUMMY_VERTEX =  Integer($80000003);


  cnstVertex_None         = $0000; //no local vertex attributes
  cnstVertex_Normal       = $0001; //explicit normals
  cnstVertex_Parameter    = $0002; //parameters (for texturing, etc...)
  cnstVertex_Face_Color   = $0004; //vertex color, to be applied to interpolated faces
  cnstVertex_Edge_Color   = $0008; //vertex color, to be applied to interpolated edges
  cnstVertex_Marker_Color = $0010; //vertex color, to be applied to marker
  cnstVertex_Face_Index   = $0020; //vertex color-by-index, to be applied to interpolated faces
  cnstVertex_Edge_Index   = $0040; //vertex color-by-index, to be applied to interpolated edges
  cnstVertex_Marker_Index = $0080; //vertex color-by-index, to be applied to marker
  cnstVertex_Any_Color    = $00FC; //multi-bit alias to quickly test for any color or color-by-index
  cnstVertex_Marker_Visibility = $0100;
  cnstVertex_Marker_Size  = $0200;
  cnstVertex_Marker_Symbol= $0400;

  cnstNC_HAS_WEIGHTS = $01; // an array of floats for the weights is specified with the TK_NURBS_Curve
  cnstNC_HAS_KNOTS   = $02; // an array of floats for the knots is specified with the TK_NURBS_Curve
  cnstNC_HAS_START   = $04; // a float is specified for where the TK_NURBS_Curve starts in parametric [0,1] space
  cnstNC_HAS_END     = $08; // a float is specified for where the TK_NURBS_Curve ends in parametric [0,1]

  CaseC = 0;
  CaseL = 1;
  CaseE = 2;
  CaseR = 3;
  CaseS = 4;
  CaseM = 5;
  CaseM2 =6;

  cnstTKO_Circular_Center = $01;

  DUMMY = GARBAGE_VERTEX-1;
  ALIAS = GARBAGE_VERTEX-2;

  bits: array[0..8] of Integer = ( 2, 6, 10, 14, 18, 22, 26, 31, 32 );
  bits2: array[0..2] of Integer = ( 16, 31, 32 );

type

  TsgDWFInsert = class(TsgSVGInsert)
  protected
    procedure LoadedInternal(AConverter: TsgDXFConverter); override;
  end;

  PsgChannel = ^TsgChannel;
  TsgChannel = record
    RGB: array[0..2] of double;
    Name: string;
  end;

  TsgVarStream = class
  private
    FData: PInteger;
    FRData: PInteger;
    FAllocated: Integer;
    FUsed: Integer;
    FBit: Integer;
    FRUsed: Integer;
    FRBit: Integer;
    FCanReallocate: Integer;
    FStatus: Integer;
    FMask: array[0..32] of DWORD;
    FRange: array[0..32] of DWORD;
  public
    constructor Create(const ASize: Integer; APointer: PByte);
    destructor Destroy;  override;
    procedure Get2(const ANumbits: Integer; var AVal: Integer);
    function Get(const ANumbitsArray: array of Integer): Integer;
  end;

  TsgEasyStack = class
  private
    FData: PInteger;
    FAllocated: Integer;
    FUsed: Integer;
    procedure StackExpand;
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Clear;
    function Pop: Integer;
    function PopInternal(AOffset: Integer; var AOut: Integer): Integer;
    procedure Push(const AValue: Integer);
    property Used: Integer read FUsed write FUsed;
  end;

  TsgUrlValue = class(TObject)
  private
    FURL: string;
    FName: string;
  public
    property URL: string read FURL write FURL;
    property Name: string read FName write FName;
  end;



{$IFDEF SG_W2D_DEBUG_FILE}
var
  vStrings: TStringList;
  vFileStream: TFileStream;
{$ENDIF}
{$IFDEF SG_W2D_DEBUG}
var
  gIndex: Integer;
  gLevelTab: Integer;
{$ENDIF}


{$IFDEF SG_FIREMONKEY}
  procedure LoadBitmap(const AMem: TMemoryStream; const ABmp: TBitmap);
  var
    vBitmap: TBitmap;
  begin
    vBitmap := TBitmap.Create;
    try
      vBitmap.LoadFromStream(AMem);
      ABmp.Assign(vBitmap);
    finally
      vBitmap.Free;
    end;
  end;

{$IFDEF SG_USE_PNG}
  procedure LoadPNG(const AMem: TMemoryStream; const ABmp: TBitmap);
  var
    vPng: TBitmapSurface;
  begin
    vPng := nil;
    try
      if TBitmapCodecManager.LoadFromStream(AMem, vPng) then
      begin
        ABmp.Assign(vPng);
      end;
    finally
      vPng.Free;
    end;
  end;
{$ENDIF}

{$ELSE}

{$IFDEF SG_USE_PNG}
  procedure LoadPNG(const AMem: TMemoryStream; const ABmp: TBitmap);
  var
    vPng: TPngImage;
  begin
    vPng := TPngImage.Create;
    try
      vPng.LoadFromStream(AMem);
      ABmp.Assign(vPng);
    finally
      vPng.Free;
    end;
  end;
{$ENDIF}

{$IFDEF SG_USE_TIFF}
  procedure LoadTiff(const AMem: TMemoryStream; const ABmp: TBitmap);
  var
    vTiff: TTIFFGraphic;
  begin
    vTiff := TTIFFGraphic.Create;
    try
      vTiff.LoadFromStream(AMem);
      ABmp.Assign(vTiff);
    finally
      vTiff.Free;
    end;
  end;
{$ENDIF}


  procedure LoadJPEG(const AMem: TMemoryStream; const ABmp: TBitmap);
  var
    vJpg: TJPEGImage;
  begin
    vJpg := TJPEGImage.Create;
    try
      vJpg.LoadFromStream(AMem);
      ABmp.Assign(vJpg);
    finally
      vJpg.Free;
    end;
  end;
{$ENDIF}

procedure MoveImageData(const AMem: TMemoryStream; const ABmp: TBitmap; ABytePerPixel: Byte);
var
  I: Integer;
{$IFDEF SG_FIREMONKEY}
  vBitData: TBitmapData;
{$ENDIF}
begin
{$IFDEF SG_FIREMONKEY}
  ABmp.Map(TMapAccess.ReadWrite, vBitData);
{$ENDIF}
  for I := 0 to ABmp.Height - 1 do
{$IFNDEF SG_FIREMONKEY}
    System.Move(Pointer(IntPtr(AMem.Memory) + I * ABmp.Width * ABytePerPixel)^,
      ABmp.ScanLine[I]^, ABmp.Width * ABytePerPixel);
{$ELSE}
    System.Move(Pointer(IntPtr(AMem.Memory) + I * ABmp.Width * ABytePerPixel)^,
      vBitData.GetScanline(I)^, ABmp.Width * ABytePerPixel);
{$ENDIF}
{$IFDEF SG_FIREMONKEY}
  ABmp.Unmap(vBitData);
{$ENDIF}
end;

procedure LoadRGBAImage(const AMem: TMemoryStream; const ABmp: TBitmap);
begin
  MoveImageData(AMem, ABmp, 4);
end;

procedure LoadRGBImage(const AMem: TMemoryStream; const ABmp: TBitmap);
begin
  MoveImageData(AMem, ABmp, 3);
end;

//procedure InitW2D;
//begin
//  // Added for
//  // http://192.168.0.102/bugzilla/show_bug.cgi?id=6755
//  CurrColor_ := 0;
//end;

function GetVAlignType(const AName: string): TsgVAlign;
var
  I: TsgVAlign;
begin
  Result := vaBaseline;
  for I := Low(cnstVAlignNames) to High(cnstVAlignNames) do
    if AName = cnstVAlignNames[I] then
    begin
      Result := I;
      Break;
    end;
end;

function GetHAlignType(const AName: string): TsgHAlign;
var
  I: TsgHAlign;
begin
  Result := haLeft;
  for I := Low(cnstHAlignNames) to High(cnstHAlignNames) do
    if AName = cnstHAlignNames[I] then
    begin
      Result := I;
      Break;
    end;
end;

{ TsgDWFInsert }

procedure TsgDWFInsert.LoadedInternal(AConverter: TsgDXFConverter);
var
  I: Integer;
begin
  if Block <> nil then
    for I := 0 to Block.Count - 1 do
      AConverter.Loads(Block.Entities[I]);
  inherited LoadedInternal(AConverter);
end;

{ TsgW2DImage }

procedure TsgW2DImage.ApplyLineWeightFactor(var ALineWeight: Double);
begin
  ALineWeight := ALineWeight * 0.25;
end;

procedure TsgW2DImage.CreateParser;
begin
  Fw2dParser := Tw2dParser.Create;
  Fw2dParser.OnProgress := DoProgress;
  Progress(Self, psStarting, 0, False, sgEmptyProgressRect, '');
end;

procedure TsgW2DImage.DoProgress(ASender: TObject; APercent: Double);
begin
  Progress(Self, psRunning, Round(APercent), False, sgEmptyProgressRect, '');
end;

procedure TsgW2DImage.FreeParser;
begin
  Fw2dParser.Free;
  Progress(Self, psEnding, 100, False, sgEmptyProgressRect, '');
end;

procedure TsgW2DImage.LoadFromFile(const AFileName: string);
var
  vFileStream: TFileStream;
begin
  LayoutName := AFileName;
  vFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(vFileStream);
    Self.FileName := AFileName;
  finally
    vFileStream.Free
  end;
end;

procedure TsgW2DImage.LoadFromStream(AStream: TStream);
var
  I: Integer;
  vEntity: TsgDXFEntity;
begin
  CreateParser;
  try
    Fw2dParser.LoadFromStream(Converter, AStream);
    Converter.InitializeSectionsBegin;
    Converter.InitializeSectionsEnd;
    for I := 0 to Fw2dParser.EntitiesList.Count - 1 do
    begin
      vEntity := TsgDXFEntity(Fw2dParser.EntitiesList.Items[I]);
      if Converter.Counts[csEntities] >= 500000 then
        vEntity.Free
      else
      begin
        Converter.Sections[csEntities].AddEntity(vEntity);
        if Assigned(Converter.OnCreate) then
          Converter.OnCreate(vEntity);
        Converter.Loads(vEntity);
      end;
    end;
    GetExtents;
  finally
    FreeParser;
  end;
end;

{ Tw2dParser }

procedure Tw2dParser.AddEntity(const AEntity: TsgDXFEntity);

  procedure AddHyperLink(const AEntity: TsgDXFEntity);
  var
    vLinkIndex: Integer;
    vValue: TsgUrlValue;
  begin
     vLinkIndex := FHyperLinkList.IndexOf(FCurrentURLIndex);
     if vLinkIndex <> -1 then
     begin
       vValue := TsgUrlValue(FHyperLinkList.Items[vLinkIndex].Data);
       AEntity.HyperLink := vValue.Name + cnstHyperLinkDelimiter + vValue.URL;
     end
  end;

begin
  AEntity.Layer := FCurrentLayer;
  if FCurrentURLIndex >= 0 then
    AddHyperLink(AEntity);
  if FCurrentBlockRef = nil then
    EntitiesList.Add(AEntity)
  else
    FCurrentBlockRef.AddEntity(AEntity);
end;

procedure Tw2dParser.ContourSet(const AProc: Tw2dParserProc);
var
  I, J: Integer;
  vPolyline: TsgDXFPolyline;
  vVertex: TsgDXFVertex;
  vContoursList: TsgList;
  vCurrentPointsCounter: TFPointList;
  vNumContours: Integer;
  vCADCurvePolygon: TsgCADCurvePolygon;

  procedure AddHatchOfFPointList(const ABoundary: Tsg2DBoundaryList;
    const APointList: TFPointList);
  var
    I: Integer;
    vPoly: Tsg2DPolyline;
    vPoint: TF2DPoint;
  begin
    vPoly := Tsg2DPolyline.Create;
    ABoundary.Add(vPoly);
    for I := 0 to APointList.Count - 1 do
    begin
      vPoint.X := APointList[I].X;
      vPoint.Y := APointList[I].Y;
      if (I > 0) and IsEqualF2DPoints(vPoint, vPoly.EndPoint) then
        Continue;
      vPoly.AddVertex(vPoint);
    end;
    vPoly.Closed := not IsEqualF2DPoints(vPoly.StartPoint, vPoly.EndPoint);
  end;

begin
  vContoursList := TsgList.Create;
  try
    vNumContours := GetSmallint;
    while vNumContours > 0 do
    begin
      vCurrentPointsCounter := TFPointList.Create;
      vContoursList.Add(vCurrentPointsCounter);
      vCurrentPointsCounter.Count := GetSmallint;
      Dec(vNumContours);
    end;

    for I := 0 to vContoursList.Count - 1 do
    begin
      vCurrentPointsCounter := vContoursList[I];
      for J := 0 to vCurrentPointsCounter.Count - 1 do
      begin
        AProc; // to read in FPoint
        vCurrentPointsCounter.Items[J] := FPoint;
      end;
    end;

    vCADCurvePolygon := nil;
    if FillMode then
    begin
       vCADCurvePolygon := TsgCADCurvePolygon.Create;
       vCADCurvePolygon.Color := GetCurrentColor;
       vCADCurvePolygon.Visibility := VisibleMode;
       AddEntity(vCADCurvePolygon);
    end;

    for I := 0 to vContoursList.Count - 1 do
    begin
      vCurrentPointsCounter := vContoursList[I];
      if Assigned(vCADCurvePolygon) then
      begin
        vCADCurvePolygon.AddBoundaryList(7);
        AddHatchOfFPointList(vCADCurvePolygon.BoundaryDataLast, vCurrentPointsCounter);
      end
      else
      begin
        vPolyline := NewPolyline;
        for J := 0 to vCurrentPointsCounter.Count - 1 do
        begin
          vVertex := TsgDXFVertex.Create;
          vVertex.Point := vCurrentPointsCounter.Items[J];
          vPolyline.AddEntity(vVertex);
        end;
      end;
    end;
  finally
    vContoursList.ClearTypeList(ptvObject);
    vContoursList.Free;
  end;
end;

procedure Tw2dParser.CreateAndAddImage(const ALowerLeftPointer,
  AUpperRightPointer: TFPoint; const ABmp: TBitmap;
  const ABmpType: Integer = Integer(efAuto));
var
  vImageEnt: TsgDXFImageEnt;
{$IFNDEF SG_FIREMONKEY}
  vBitmap: TsgBitmap;
{$ENDIF}
begin
  vImageEnt := TsgDXFImageEnt.Create;
{$IFNDEF SG_FIREMONKEY}
  if ABmp.PixelFormat in [pf24bit, pf32bit] then
  begin
    vBitmap := TsgBitmap.Create;
    try
      vBitmap.AssignFromBMP(ABmp);
      vImageEnt.SetImage(vBitmap);
    finally
      vBitmap.Free;
    end;
  end
  else
    vImageEnt.SetImage(TGraphic(ABmp));
  case TsgGraphicFormat(ABmpType) of
    efPng: vImageEnt.Transparency := ABmp.Transparent and (ABmp.PixelFormat = pf24bit);
  else
    vImageEnt.Transparency := ABmp.Transparent;
  end;
  vImageEnt.TransparentColor := ABmp.TransparentColor;
{$ELSE}
  vImageEnt.SetImage(TGraphic(ABmp));
{$ENDIF}
  vImageEnt.Point := ALowerLeftPointer;
  vImageEnt.UVector := MakeFPoint(AUpperRightPointer.X - ALowerLeftPointer.x, 0, 0);
  vImageEnt.VVector := MakeFPoint(0, AUpperRightPointer.Y - ALowerLeftPointer.y, 0);
  vImageEnt.Size := MakeFPoint(ABmp.Width, ABmp.Height);
  vImageEnt.UVector := PtXScalar(vImageEnt.UVector,1/vImageEnt.Size.X);
  vImageEnt.VVector := PtXScalar(vImageEnt.VVector,1/vImageEnt.Size.Y);
  AddEntity(vImageEnt);
end;

procedure Tw2dParser.DecompressLZ;
var
  vLiteralDataRun, vCompressionRun: Integer;
  vTmpCnt, vTmpLen: Integer;
  vTmpData: PData;
  vPByte: PByte;
  vMS: TMemoryStream;

  procedure DoWriteBuffer1;
  begin
    if vLiteralDataRun > 0 then
    begin
      vPByte := PByte(FPData);
      Inc(vPByte, FCnt);
      vMS.WriteBuffer(vPByte^, vLiteralDataRun);
      Inc(FCnt, vLiteralDataRun);
    end;
  end;

  procedure DoWriteBuffer2;
  begin
    if vCompressionRun <> 0 then
    begin
      if vCompressionRun = 15 then
        vCompressionRun := GetByte + 15;
      Inc(vCompressionRun, 3);
      vLiteralDataRun := GetWord + 1;
      vMS.Size := vMS.Size + vCompressionRun;
      vPByte := PByte(vMS.Memory);
      Inc(vPByte, vMS.Position - vLiteralDataRun);
      vMS.WriteBuffer(vPByte^, vCompressionRun);
    end;
  end;

begin

  Inc(FCnt, 7);
  vMS := TMemoryStream.Create;
  try
    vMS.WriteBuffer(PByte(ZlibDict)^, Length(ZlibDict));
    repeat
      vLiteralDataRun := GetByte;
      if vLiteralDataRun = 0 then
        Break;
      vCompressionRun := vLiteralDataRun shr 4;
      vLiteralDataRun := vLiteralDataRun and $0F;
      if vLiteralDataRun = 15 then
        vLiteralDataRun := GetByte + 15;
      DoWriteBuffer1;
      DoWriteBuffer2;
    until False;
    //vMS.SaveToFile('D:\out.w2d');
    StoreData(Pointer(vTmpData), vTmpCnt, vTmpLen);
    try
      SetData(vMS.Memory, Length(ZlibDict), vMS.Size{ - Length(ZlibDict) - 1});
    finally
      RestoreData(vTmpData, vTmpCnt, vTmpLen);
    end;
  finally
    vMS.Free;
  end;
end;

procedure Tw2dParser.DecompressZlib;
var
  vBuf, vTmpData: PData;
  vTmpCnt, vTmpLen, vCnt: Integer;
  vZS: zlibpas_sg.z_stream;

  procedure InitZStream(var AZS: zlibpas_sg.z_stream);
  begin
    FillChar(AZS, SizeOf(AZS), 0);
    AZS.avail_in := FDataLength - FCnt;
    AZS.next_in := pBytef(FPData);
    Inc(AZS.next_in, FCnt);
    inflateInit(AZS);
  end;

begin
  Inc(FCnt, 7);
  InitZStream(vZS);
  vBuf := nil;
  try
    vCnt := 0;
    repeat
      ReallocMem(vBuf, vCnt + 65536);
      vZS.avail_out := 65536;
      vZS.next_out := pBytef(TsgNativeUInt(vBuf) + Cardinal(vCnt));
      if inflate(vZS, Z_SYNC_FLUSH) = Z_NEED_DICT then
        inflateSetDictionary(vZS, pBytef(ZlibDict), Length(ZlibDict) - 2047)
      else
      begin
        Inc(vCnt, 65536 - vZS.avail_out);
        if vZS.avail_out > 0 then
          Break;
      end;
    until False;
    StoreData(Pointer(vTmpData), vTmpCnt, vTmpLen, vZS.avail_in);
    try
      SetData(vBuf, 0, vCnt);
    finally
      RestoreData(vTmpData, vTmpCnt, vTmpLen);
    end;
  finally
    FreeMem(vBuf);
    inflateEnd(vZS);
  end;
end;

procedure Tw2dParser.DoColorMap;
var
  I: Integer;
begin
  SkipWhiteSpace;
  CustomColorsCnt := GetInt;
  Getmem(FCustomColorMap, CustomColorsCnt * 4);
  for I := 0 to CustomColorsCnt - 1 do
  begin
    GetRGBAInt;
    if IsColorInd then
      Error(sExtASCIIErrorCode);
    FCustomColorMap^[I * 4]     := RedColor;
    FCustomColorMap^[I * 4 + 1] := GreenColor;
    FCustomColorMap^[I * 4 + 2] := BlueColor;
    FCustomColorMap^[I * 4 + 3] := AlphaColor;
  end;
  CurrColorMap := @FCustomColorMap^[0];
end;

procedure Tw2dParser.ExecuteExtASCII;
var
  vCircle: TsgDXFCircle;
  vText: TsgDXFText;
  I,J: Integer;
  vString: string;
  vNumCount, vCountPoint, vSumPoints: Integer;
  vPoint: TFPoint;
begin
  DoProgress;
  I := GetExtASCIIndex;
  case I of

    cnstExtASCIIBackGround:
      begin
        FIsSetBackgroundColor := True;
        GetRGBAInt;
        FBackgroundColor := ReadCurrColor(IsColorInd);
        if BackgroundColor = clByLayer then
          FBackgroundColor := clBlack;
      end;

    cnstExtASCIIAuthor,
    cnstExtASCIIComments,
    cnstExtASCIICopyright,
    cnstExtASCIICreator,
    cnstExtASCIIDescription,
    cnstExtASCIIKeywords,
    cnstExtASCIITitle,
    cnstExtASCIISourceFilename:  //WT_Informational
      begin
        vString := GetTextValue;
      end;


    cnstExtASCIICircle:
      begin
        vCircle := NewCircle;
        GetAbsPointInt;
        vCircle.Point := FPoint;
        SkipWhiteSpace;
        vCircle.Radius := GetInt;
      end;

    cnstExtASCIIColor:
      begin
        GetRGBAInt;
        SetCurrentColor(ReadCurrColor(False));
      end;

    cnstExtASCIIColorMap: DoColorMap;

    cnstExtASCIIFillPattern: GetTextValue;

    cnstExtASCIIGroup4PNGImage: GetPNG;

    // ???: change global variable LayoutName
    //cnstExtASCIITitle: LayoutName := GetTextValue;

    // ???: debug info
    //cnstExtASCIIGlyph: MessageBox(0, 'MarkerGlyph', 'Info', MB_OK);

//    cnstExtASCIIViewport: ;

    cnstExtASCIIURL:
      GetURL;

    cnstExtASCIIUnits: GetUnits;
//    cnstExtASCIIBezier:
//      sgNop;

    cnstExtASCIIContour:
      begin
        vNumCount := GetIntSkipWhiteSpace;
        vSumPoints := 0;
        for I := 0 to vNumCount - 1 do
        begin
          vCountPoint := GetIntSkipWhiteSpace;
          vSumPoints := vSumPoints + vCountPoint;
        end;
        for J := 0 to vSumPoints - 1 do
        begin
          vPoint.X := GetIntSkipWhiteSpace;
          Inc(FCnt);
          vPoint.Y  := GetIntSkipWhiteSpace;
        end;


      end;

    cnstExtASCIILayer: SetLayer(False);

    cnstExtASCIILineWeight:
      begin
        LineWeight := Round(GetIntSkipWhiteSpace * WeightScale);
      end;

    cnstExtASCIIFont:
      begin
          FontHeight := GetIntByName('Height');
      end;

    cnstExtASCIIFontExtension: TextStyle(GetText);

    cnstExtASCIIText:
      begin
         GetAbsPointInt;
         vText := NewText;
        vText.Point := FPoint;
        vText.Text := GetText;
      end;

    cnstExtASCIITextHAlign:
      begin
        SkipCharSet([#0, #9, #10, #13, #32]);
        gTextHAlign := GetHAlignType(GetTextValue);
      end;
    cnstExtASCIITextVAlign:
      begin
        SkipCharSet([#0, #9, #10, #13, #32]);
        gTextVAlign := GetVAlignType(GetTextValue);
      end;


  end;
  FTagCnt := 0;
  SkipExtASCII;
end;

procedure Tw2dParser.AddImage(const AFuncImageLoad: TsgProcImageLoad;
  const ACommand: Word; const ABmpType: TsgGraphicFormat);
begin
{$IFNDEF SGFPC}
{$IFDEF SG_OPENING_IN_THEADS}
  TThread.CurrentThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      AddImageUnsafe(AFuncImageLoad, ACommand, ABmpType);
    end);
{$ELSE}
   AddImageUnsafe(AFuncImageLoad, ACommand, ABmpType);
{$ENDIF}
{$ELSE}
   AddImageUnsafe(AFuncImageLoad, ACommand, ABmpType);
{$ENDIF}
end;

procedure Tw2dParser.AddImageUnsafe(const AFuncImageLoad: TsgProcImageLoad;
  const ACommand: Word; const ABmpType: TsgGraphicFormat);
var
  vColPixelArray, vRowPixelArray: Integer;
  {vIdentifierImage, }vLengthData: Integer;
  I: integer;
  vLowerLeftPointer, vUpperRightPointer: TFPoint;
  vBmp: TBitmap;
  vMemStream: TMemoryStream;
  vCurrentByte: Byte;
  vCountColors: Byte;
  vR, vG, vB, vA: Byte;

begin
  vColPixelArray := GetWord;
  vRowPixelArray := GetWord;
  GetRelPointLong;
  vLowerLeftPointer := FPoint;
  GetRelPointLong;
  vUpperRightPointer := FPoint;
  {vIdentifierImage := }GetLong;
  case ACommand of
    cnstExtBinaryDrawGroup4XMappedExt:
      begin
        vCountColors := GetByte;
        vCountColors := IfThen(vCountColors = 0, 256, vCountColors);
        for I := 1 to vCountColors do
        begin
          vR := GetByte;
          vG := GetByte;
          vB := GetByte;
          vA := GetByte;
          ConvertRGBtoColor(vR, vG, vB, vA);
        end;
        vLengthData := GetLong;
      end;
    else
      vLengthData := GetLong;
  end;

  vMemStream :=  TMemoryStream.Create;
  try
    for I := 1 to vLengthData do
    begin
      vCurrentByte := GetByte;
      vMemStream.Write(vCurrentByte, 1);
    end;
    vMemStream.Position:=0;
    vBmp := TBitmap.Create;

    case ACommand of
      cnstExtBinaryDrawRGBAImage:
        begin
          vBmp.PixelFormat := pf32bit;
          vBmp.Width := vColPixelArray;
          vBmp.Height := vRowPixelArray;
        end;
      cnstExtBinaryDrawRGBImage:
        begin
          vBmp.PixelFormat := pf24bit;
          vBmp.Width := vColPixelArray;
          vBmp.Height := vRowPixelArray;
        end;
    end;

    try
      AFuncImageLoad(vMemStream, vBmp);
      CreateAndAddImage(vLowerLeftPointer, vUpperRightPointer, vBmp, Ord(ABmpType));
    finally
      vBmp.Free;
    end;
  finally
    vMemStream.Free;
  end;
end;

procedure Tw2dParser.ExecuteExtBinary;

var
  vDataCnt: Integer;
  vCommand: Word;
  vSavePosition: Integer;

  procedure ExecCmd(var ADataCounter: Integer);
  begin
    while ADataCounter > 3 do // To exclude vCommand and CmndTag
    begin
      Inc(FCnt);
      Dec(ADataCounter);
    end;
    FLogBuff := PChar(TsgNativeUInt(FPData) + Cardinal(FCnt));
    if (FCnt >= FDataLength) or (PDataValue <> Byte('}'))
       {or (FTagCnt <> 0)} then
      Error(sW2DBinaryComandError);
  end;




//  cnstExtBinarySetColorMap                    = $0001;
//  cnstExtBinaryDrawMappedBitonalImage         = $0002; //WD_IMAGE_BITONAL_MAPPED_EXT_OPCODE
//  cnstExtBinaryDrawMappedGroup3XImage         = $0003; //WD_IMAGE_GROUP3X_MAPPED_EXT_OPCODE
//  cnstExtBinaryDrawIndexedColorImage          = $0004; //WD_IMAGE_INDEXED_EXT_OPCODE
//  cnstExtBinaryDrawMappedColorImage           = $0005; //WD_IMAGE_MAPPED_EXT_OPCODE
//  cnstExtBinaryDrawRGBImage                   = $0006; //WD_IMAGE_RGB_EXT_OPCODE
//  cnstExtBinaryDrawRGBAImage                  = $0007; //WD_IMAGE_RGBA_EXT_OPCODE
//  cnstExtBinaryDrawJPEGImage                  = $0008; //WD_IMAGE_JPEG_EXT_OPCODE
//  cnstExtBinaryDrawGroup4PNGImage0            = $0009; //WD_IMAGE_GROUP4_BITONAL_EXT_OPCODE
//  cnstExtBinaryDrawGroup4PNGImage1            = $000C; //WD_IMAGE_PNG_EXT_OPCODE
//  cnstExtBinaryDrawGroup4XMappedExt           = $000D; //WD_IMAGE_GROUP4X_MAPPED_EXT_OPCODE

begin
  DoProgress;
  FTagCnt := 1;
  Inc(FCnt);
  vDataCnt := GetUnsignedLong;
  vCommand := GetWord;
  case vCommand of
    cnstExtBinarySetColorMap: SetColorMapFromBinary;
    cnstExtBinaryDrawMappedBitonalImage .. cnstExtBinaryDrawMappedColorImage,
    {$IFNDEF SG_USE_PNG} {$IFNDEF SG_FIREMONKEY} cnstExtBinaryDrawGroup4PNGImage1, {$ENDIF} {$ENDIF}
    cnstExtBinaryDrawGroup4PNGImage0
    {$IFNDEF SG_USE_TIFF} {$IFNDEF SG_FIREMONKEY}, cnstExtBinaryDrawGroup4XMappedExt{$ENDIF} {$ENDIF}:
      begin
        Inc(FCnt, 4);
        GetRelPointLong;
        GetRelPointLong;
        Dec(vDataCnt, 20);
        ExecCmd(vDataCnt);
      end;
    cnstExtBinaryDrawRGBImage:
    begin
      AddImage(@LoadRGBImage, vCommand, efBitmap);
    end;
    cnstExtBinaryDrawRGBAImage:
    begin
      AddImage(@LoadRGBAImage, vCommand, efBitmap);
    end;
{$IFNDEF SG_FIREMONKEY}
  {$IFDEF SG_USE_TIFF}
    cnstExtBinaryDrawGroup4XMappedExt:
    begin
      AddImage(@LoadTiff, vCommand, efTiff);
    end;
  {$ENDIF}
    cnstExtBinaryDrawJPEGImage:
      begin
        AddImage(@LoadJPEG, vCommand, efJpeg);
      end;
  {$IFDEF SG_USE_PNG}
    cnstExtBinaryDrawGroup4PNGImage1:
      begin
        AddImage(@LoadPNG, vCommand, efPng);
      end;
  {$ENDIF}
{$ELSE}
    cnstExtBinaryDrawJPEGImage, cnstExtBinaryDrawGroup4PNGImage1
      {$IFDEF SG_FM_WINDOWS}, cnstExtBinaryDrawGroup4XMappedExt{$ENDIF}:
      begin
        AddImage(@LoadBitmap, vCommand, efBitmap);
      end;
{$ENDIF}
    cnstExtBinaryTextHAlign:
      begin
        gTextHAlign :=  TsgHAlign(GetByte);
      end;
    cnstExtBinaryTextVAlign:
      begin
        gTextVAlign :=  TsgVAlign(GetByte);
      end;
    cnstExtBinaryGraphicsHdr,
    cnstExtBinaryOverlayHdr,
    cnstExtBinaryRedlineHdr,
    cnstExtBinaryThumbnail,
    cnstExtBinaryPreview,
    cnstExtBinaryPverlayPreview,
    cnstExtBinaryFont,
    cnstExtBinaryGraphics,
    cnstExtBinaryOverlay,
    cnstExtBinaryRedline,
    cnstExtBinaryUser,
    cnstExtBinaryNull,
    cnstExtBinaryGlobalSheet,
    cnstExtBinaryGlobal,
    cnstExtBinarySignature,
    cnstExtBinaryBlockRef0,
    cnstExtBinaryBlockRef1:
      begin
        vSavePosition := FCnt;

        if vCommand = cnstExtBinaryThumbnail then
        begin
          if not Assigned(FShadowBlockRef) then
          begin
            FShadowBlockRef := TsgDXFBlock.Create;
            FShadowBlockRef.Name := 'W2DSHADOW_' +IntToHex(TsgNativeUInt(FShadowBlockRef), 8);
          end;
          FCurrentBlockRef := FShadowBlockRef;
        end
        else
          FCurrentBlockRef := nil;

        FCnt := vSavePosition;
        ExecCmd(vDataCnt);
      end

  else
    ExecCmd(vDataCnt);
  end;
end;

{ Tw2dParser.ExecuteOneByte

  Main parser method. }

procedure Tw2dParser.ExecuteOneByte;
var
  vArc: TsgDXFArc;
  vCircle: TsgDXFCircle;
  vEllipse: TsgDXFEllipse;
  vText: TsgDXFText;
  vByte0: Byte;
  vWord0: Word;
  vPoint1, vPoint2: TFPoint;
  I: Integer;
  vR1, vR2: Double;
  vBox : TFRect;
  vTilt: Double;

  vNameLength: Integer;
  vNameFont: string;

  procedure Skip;
  begin
    vByte0 := GetByte;
    vWord0 := 0 or vByte0;
    if vByte0 = 0 then
      vWord0 := GetWord
    else
      Dec(vWord0);
    while vWord0 > 1 do
    begin
      GetSmallint;
      Dec(vWord0);
    end;
  end;

begin
  DoProgress;
  vByte0 := GetByte;
  case vByte0 of
    cnstSetColor03h:
      begin
        RedColor := GetByte;
        GreenColor := GetByte;
        BlueColor := GetByte;
        AlphaColor := GetByte;
        if FVersion < cnstREVISION_WHEN_BINARY_COLOR_CHANGED_TO_RGBA_FROM_BGRA then
        begin
          SwapInts(RedColor, BlueColor);
        end;
        SetCurrentColor(ReadCurrColor(False));
      end;
    cnstSetColor43h:
      begin
        RedColor := GetIntSkipWhiteSpace;
        SetCurrentColor(ReadCurrColor(True));
      end;
    cnstSetColor63h:
      begin
        RedColor := GetByte;
        SetCurrentColor(ReadCurrColor(True));
      end;
    cnstSetFont06h:
      begin
if FVersion < cnstREVISION_WHEN_SET_FONT_STRING_FIXED then
        begin
          vNameLength := GetSmallint;
          SetLength(vNameFont, vNameLength);
          for I := 1 to vNameLength do
            vNameFont[I] := Char(GetWord);
          TextStyle(vNameFont);
          GetByte;// bold_italic
          //          m_option_style.set_bold (bold_italic & WD_FONT_STYLE_BOLD);
          //          m_option_style.set_italic (bold_italic & WD_FONT_STYLE_ITALIC);
          GetSmallint;
          //          if ((cs & 0xFF) != cs)
          //              return WT_Result::Corrupt_File_Error;
          //          m_option_charset.set((WT_Byte)cs);
          GetSmallint; //pitch_and_family
          //          m_option_pitch.set ((WT_Byte)(pitch_and_family & WT_Font_Option_Pitch::PITCH_BITS));
          //          m_option_family.set((WT_Byte)(pitch_and_family & WT_Font_Option_Family::FAMILY_BITS));
        end
        else
        begin
          vWord0 := GetWord;
          if (vWord0 and $0001) <> 0 then
            TextStyle(GetText);        // $0001  $0400
          if (vWord0 and $0002) <> 0 then
            GetByte;        // $0002  $0200
          if (vWord0 and $0004) <> 0 then
            GetByte;        // $0004  $0100
          if (vWord0 and $0008) <> 0 then
            GetByte;        // $0008  $0080
          if (vWord0 and $0010) <> 0 then
            GetByte;        // $0010  $0040
          if (vWord0 and $0020) <> 0 then
            FontHeight := GetUnsignedLong;      // $0020  $0020
          if (vWord0 and $0040) <> 0 then
            FontRotation := GetAngle;  // $0040  $0010
          if (vWord0 and $0080) <> 0 then
            FXScale := GetWord / 1024;  // $0080  $0008
          if (vWord0 and $0100) <> 0 then
            GetWord;        // $0100  $0004
          if (vWord0 and $0200) <> 0 then
            GetWord;        // $0200  $0002
          if (vWord0 and $0400) <> 0 then
            GetUnsignedLong;        // $0400  $0001
        end;
      end;
    cnstSetObjectNode1, cnstSetMarkerSize1:
      GetUnsignedLong;
    cnstSetObjectNode2:
      GetSmall;
    cnstSetLineWeight:
      LineWeight := Round(GetLong * WeightScale);
    cnstSetFillMode0:
      FillMode := True;
    cnstSetFillMode1:
      FillMode := False;
    cnstSetMarkerGlyph0, cnstSetMarkerSize0:
      GetIntSkipWhiteSpace;
    cnstSetMarkerGlyph1:
      GetWord;
    cnstSetCurrentPoint:
      begin
        FAbsX := GetUnsignedLong;
        FAbsY := GetUnsignedLong;
        FRelX := FAbsX;
        FRelY := FAbsY;
      end;
    cnstSetVisibility0:
      VisibleMode := True;
    cnstSetVisibility1:
      VisibleMode := False;
    cnstSetLayer: SetLayer(True);
    cnstDrawGouraudPolytriangle07h:
      PolyTriangle(GetSmallint, GetRelPointSmall, True);
    cnstDrawGouraudPolytriangle67h:
      PolyTriangle(GetSmallint, GetRelPointLong, True);
    cnstDrawPolytriangle14h:
      PolyTriangle(GetSmallint, GetRelPointSmall, False);
    cnstDrawPolytriangle54h:
      PolyTriangle(GetIntSkipWhiteSpace, GetAbsPointInt, False);
    cnstDrawPolytriangle74h:
      PolyTriangle(GetSmallint, GetRelPointLong, False);
    cnstDrawContourSet0:
      ContourSet(GetRelPointSmall);
    cnstDrawContourSet1:
      ContourSet(GetRelPointLong);
    cnstDrawLine0:
      Line(1, GetRelPointSmall);
    cnstDrawLine1:
      Line(1, GetAbsPointInt);
    cnstDrawLine2:
      Line(1, GetRelPointLong);
    cnstDrawLine3:
      Line(GetByte, GetRelPointSmall);
    cnstDrawPolyline0:
      PolyLine(GetSmallint, GetRelPointSmall, False);
    cnstDrawPolyline1:
      PolyLine(GetSmallint, GetRelPointLong, False);
    cnstDrawGouraudPolyline0:
      PolyLine(GetSmallint, GetRelPointSmall, True);
    cnstDrawGouraudPolyline1:
      PolyLine(GetSmallint, GetRelPointLong, True);
    cnstDrawPolylinePolygon50h:
      PolyLine(GetIntSkipWhiteSpace, GetAbsPointInt, False);
    cnstDrawCircle0:
      begin
        vCircle := NewCircle;
        GetRelPointSmall;
        vCircle.Point := FPoint;
        vCircle.Radius := GetWord;
      end;
    cnstDrawCircle1:
      begin
        vCircle := NewCircle;
        GetAbsPointInt;
        vCircle.Point := FPoint;
        vCircle.Radius := GetIntSkipWhiteSpace;
      end;
    cnstDrawCircle2:
      begin
        vCircle := NewCircle;
        GetRelPointLong;
        vCircle.Point := FPoint;
        vCircle.Radius := GetUnsignedLong;
      end;
    cnstDrawCircle3:
      begin
        vArc := NewArc;
        GetRelPointLong;
        vArc.Point := FPoint;
        vArc.Radius := GetUnsignedLong;
        vArc.StartAngle := GetAngle;
        vArc.EndAngle := GetAngle;
        if vArc.StartAngle = vArc.EndAngle then
          vArc.EndAngle := vArc.EndAngle + 360;
      end;
    cnstDrawText0:
      begin
        vText := NewText;
        GetRelPointLong;
        vText.Point := FPoint;
        vText.Text := GetText;
        Skip;
        Skip;
        GetRelPointLong;
        vPoint1 := FPoint;
        GetRelPointLong;
        vPoint2 := FPoint;
        vText.Rotation := GetAngleByPoints(vPoint1, vPoint2, False);
        GetRelPointLong;
        GetRelPointLong;
        if PDataValue = 1 then
          Skip;
      end;
    cnstDrawText78h:
      begin
        vText := NewText;
        GetRelPointLong;
        vText.Point := FPoint;
        vText.Text := GetText;

        FConv.Loads(vText);
        vBox := vText.Box;
        case gTextHAlign of
          haCenter:
          begin
            vText.Point := MakeFPoint(vText.Point.X -
              (vBox.Right - vBox.Left)/2,vText.Point.Y) ;
          end;
          haRight:
          begin
            vText.Point := MakeFPoint(vText.Point.X -
              (vBox.Right - vBox.Left),vText.Point.Y) ;
          end;
        end;


  {      if (FPData^[FCnt] = $22) or (FPData^[FCnt] = $27) then
          vText.Text := GetText
        else if FPData^[FCnt] = $7B then
        begin
          Inc(FCnt);
          I := GetUnsignedLong * 2;
          Inc(FCnt, I+1);
        end;}
      end;
    cnstDrawEllipse0:
      begin
        GetAbsPointInt;
        vR1 := GetIntSkipWhiteSpace;
        Inc(FCnt);
        vR2 := GetInt;
        NewEllipse(FPoint, vR1, vR2);
      end;
    cnstDrawEllipse1:
      begin
        GetRelPointLong;
        vR1 := GetUnsignedLong;
        vR2 := GetUnsignedLong;
        vEllipse := NewEllipse(FPoint, vR1, vR2);
        vEllipse.StartAngle := GetAngle;
        vEllipse.EndAngle := GetAngle;
        if vEllipse.StartAngle = vEllipse.EndAngle then
        begin
          vEllipse.StartAngle := 0;
          vEllipse.EndAngle := 360;
        end;

        vTilt :=  GetAngle;
        vEllipse.RadPt := RotateFPoint(vEllipse.RadPt, vTilt);
      end;
    cnstDrawPolymarker0:
      begin
        if FVersion < cnstREVISION_WHEN_MACRO_IS_SUPPORTED then
          PolyMarker(False, GetAbsPointInt)
        else
          PolyMarker(False, GetAbsPointInt);
      end;
    cnstDrawPolymarker1: //$6D
      if FVersion < cnstREVISION_WHEN_MACRO_IS_SUPPORTED then
        PolyMarker(True, GetRelPointLong) // Polymarker
      else
        PolyMarker(True, GetRelPointLong); //Macro
    cnstDrawPolymarker2: //$8D
      if FVersion < cnstREVISION_WHEN_MACRO_IS_SUPPORTED then
        PolyMarker(True, GetRelPointSmall) // Polymarker
      else
        PolyMarker(True, GetRelPointSmall); //Macro
    cnstDrawPolyBeziercurve:
      begin
        GetSmallint;
        GetRelPointLong;
        while vWord0 > 0 do
        begin
          GetRelPointLong;
          GetRelPointLong;
          GetRelPointLong;
          Dec(vWord0);
        end;
      end;
    cnstDrawTexturedPolytriangle:
      begin
        GetSmallint;
        while vWord0 > 0 do
        begin
          GetRelPointLong;
          Dec(vWord0);
        end;
      end;
  end;
end;

procedure Tw2dParser.GetAbsPointInt;
begin
  FPoint.X := GetIntSkipWhiteSpace;
  Inc(FCnt);
  FPoint.Y  := GetInt;
end;

function Tw2dParser.GetAngle: Double;
begin
  Result := GetWord * 360 / 65536;
end;

function Tw2dParser.GetExtASCIIndex: Integer;
var
  S: string;
begin
  Inc(FCnt);
  S := GetTextValue;
  Result := 0;
  while (Result < cnstTableExtASCIICount) and (arTableExtASCII[Result] <> S) do
    Inc(Result);
end;

function Tw2dParser.GetInt: Integer;
{$IFNDEF SG_ASSEMBLER32}
var
  C: PByte;
  vNeg: Boolean;
begin
  Result := 0;
  C := PByte(TsgNativeUInt(FPData) + TsgNativeUInt(FCnt));
  vNeg := C^ = Byte('-');
  if not vNeg then
    Result := C^ - $30;
  Inc(C);
  Inc(FCnt);
  while (C^ >= $30) and (C^ <= $39) do
  begin
    Result := 10 * Result;
    Result := Result + C^ - $30;
    Inc(FCnt);
    Inc(C);
  end;
  if vNeg then
    Result := -Result;
//  PByte(FLogBuff) := FPData;
//  Inc(PByte(FLogBuff), FCnt);
end;
{$ELSE}
asm
  pushad
  mov    edx,                Self
  push   edx
  mov    edi,                [edx+ FPData]
  mov    ecx,                [edx+ FCnt]
  xor    eax,                eax
  mov    ebx,                1
  cmp    byte ptr [edi+ ecx], '-'
  jne    @FIRST_DIGIT
  mov    ebx,                $FFFFFFFF
  Inc    ecx
@FIRST_DIGIT :
  mov    esi,                $0A
  cmp    byte ptr [edi+ ecx], $30
  jb     @EXIT
  cmp    byte ptr [edi+ ecx], $39
  ja     @EXIT
  mov    al,                 byte ptr [edi+ ecx]
  sub    al,                 $30
@NEXT_DIGIT :
  Inc    ecx
  cmp    byte ptr [edi+ ecx], $30
  jb     @EXIT
  cmp    byte ptr [edi+ ecx], $39
  ja     @EXIT
  imul   esi
  xor    edx,                edx
  mov    dl,                 byte ptr [edi+ ecx]
  sub    dl,                 $30
  add    eax,                edx
  jmp    @NEXT_DIGIT
@EXIT :
  pop    edx
  mov    [edx+ FCnt],        ecx
  mov    [edx+ FLogBuff],    edi
  add    [edx+ FLogBuff],    ecx
  imul   ebx
  mov    @Result,            eax
  popad
end;
{$ENDIF}


function Tw2dParser.GetIntSkipWhiteSpace: Integer;
begin
  SkipWhiteSpace;
  Result := GetInt;
end;

procedure Tw2dParser.GetPNG;
{$IFDEF SG_USE_PNG}
const
  cnstByteString = '$xx';

var
  vFormat : string;
  vByteMask: string;
  vCurrentByte, vWriteByte: Byte;
  //vColPixelArray, vRowPixelArray: Integer;
  //vIdentifierImage, vLengthData: Integer;
  vLowerLeftPointer, vUpperRightPointer: TFPoint;
  vBmp: TBitmap;
  vMemStream: TMemoryStream;

begin
   vFormat := GetTextValue ;
   Inc(FCnt);
   if vFormat = 'PNG' then
   begin
      {vIdentifierImage := }GetIntSkipWhiteSpace;
      {vColPixelArray := }GetIntSkipWhiteSpace;
      Inc(FCnt);// ","
      {vRowPixelArray := }GetIntSkipWhiteSpace;
      GetAbsPointInt; // Lower-Left
      vLowerLeftPointer := MakeFPoint(FPoint.X, FPoint.Y);
      GetAbsPointInt; // Upper-Right
      vUpperRightPointer := MakeFPoint(FPoint.X, FPoint.Y);
      SkipTo('(');
      {vLengthData := }GetIntSkipWhiteSpace;
      vMemStream :=  TMemoryStream.Create;
      try
        vByteMask := cnstByteString;
        vCurrentByte := 0;
        repeat
          try
            SkipWhiteSpace;
            vCurrentByte := PDataValue;
            if not (vCurrentByte in [48..57, 65..70, 97..102])  then
            begin
              Inc(FCnt);
              continue;
            end;
            vByteMask[2] := Char(vCurrentByte);
            Inc(FCnt);
            vByteMask[3] := Char(PDataValue);
            Inc(FCnt);
            vWriteByte := StrToInt(vByteMask);
            vMemStream.Write(vWriteByte,1) ;
          except
            Break;
          end;
        until vCurrentByte = 41 ;
        SkipExtASCII;
        SkipExtASCII;
        vMemStream.Position:=0;
        vBmp := TBitmap.Create;
        try
          LoadPNG(vMemStream, vBmp);
          CreateAndAddImage(vLowerLeftPointer, vUpperRightPointer, vBmp);
        finally
          vBmp.Free;
        end;
      finally
        vMemStream.Free;
      end;
   end;
{$ELSE}
begin
{$ENDIF}

end;

function Tw2dParser.GetIntByName(const AName: string): Integer;
var
  vLeadByte: Byte;
  vText: string;

  function GetByteBeforeInc: Byte;
  begin
    Inc(FCnt);
    Result := PDataValue;
  end;

begin
  Result := 0;
  vLeadByte := PDataValue;
  vText := '';
  while vLeadByte <> Byte(')') do
  begin
   if vLeadByte = Byte('(') then
   begin
     Inc(FCnt);
     vText := GetTextValue;
     if vText = AName then
     begin
       Result := GetIntSkipWhiteSpace;
       SkipExtASCII;
       Exit;
     end
     else
     begin
        SkipExtASCII;
        vLeadByte :=PDataValue;
     end;
   end
   else
     vLeadByte :=GetByteBeforeInc;
  end;
end;

procedure Tw2dParser.GetRelPointLong;
begin
  FRelX := GetLong + FRelX;// - FAbsX;
  FRelY := GetLong + FRelY;// - FAbsY;
  FPoint.X := FRelX;
  FPoint.Y := FRelY;
end;

procedure Tw2dParser.GetRelPointSmall;
begin
  FRelX := GetSmall + FRelX;// - FAbsX;
  FRelY := GetSmall + FRelY;// - FAbsY;
  FPoint.X := FRelX;
  FPoint.Y := FRelY;
end;

procedure Tw2dParser.GetRGBAInt;
begin
  IsColorInd := False;
  RedColor := GetIntSkipWhiteSpace;
  if PDataValue = Byte(',') then
  begin
    Inc(FCnt);
    GreenColor := GetInt;
    if PDataValue <> Byte(',') then
      Error(sExtASCIIErrorCode);
    Inc(FCnt);
    BlueColor := GetInt;
    if PDataValue <> Byte(',') then
      Error(sExtASCIIErrorCode);
    Inc(FCnt);
    AlphaColor := GetInt;
    if FVersion < cnstREVISION_WHEN_BINARY_COLOR_CHANGED_TO_RGBA_FROM_BGRA then
    begin
      SwapInts(RedColor, BlueColor);
    end;
  end
  else
  begin
    Dec(FCnt);
    IsColorInd := True;
  end;
end;

function Tw2dParser.GetText: string;
var

  I, vLength: Integer;
  vLeadByte, vByte, vQuote: Byte;
  vProcessingEscapedLiteral: Boolean;
  vProcessingQuotedString: Boolean;
  vStringAccumulator: TsgEasyStack;

  function ReadByte: Byte;
  begin
    Result := PDataValue;
    Inc(FCnt);
  end;

  function RawByteHexStringToWideString(const AHexBin: string): WideString;
  var
    vLen: Integer;
    vPointer: PWord;
  {$IFDEF SG_FM_MOBILE}
    vBuffer: TBytes;
  {$ENDIF}
  begin
    vLen := Length(AHexBin) div 4;
    SetLength(Result, vLen);
  {$IFNDEF SG_FM_MOBILE}
    HexToBin(PChar(AHexBin), Pointer(Result), vLen shl 1);
    vPointer := PWord(Result);
    while vLen > 0 do
    begin
      vPointer^ := Swap(vPointer^);
      Inc(vPointer);
      Dec(vLen);
    end;
  {$ELSE}
    SetLength(vBuffer, vLen shl 1);
    HexToBin(PChar(AHexBin), 0, vBuffer, 0, vLen shl 1);
    Result := TEncoding.BigEndianUnicode.GetString(vBuffer);
  {$ENDIF}
  end;

begin
  Result := '';
  SkipCharSet([#0, #9, #10, #13, #32]);//   SkipWhiteSpace;
  vLeadByte := PDataValue;
  if Chr(vLeadByte) = '{' then  (*binary*)
  begin
        Inc(FCnt);// get '{'
        vLength := GetLong;
        SetLength(Result, vLength);
        for I := 1 to vLength do
          Result[I] := Char(GetWord);
        vLeadByte := PDataValue;
        if Chr(vLeadByte) <> '}' then
          Error(sCorruptFileError + ': no final } for text.');
        Inc(FCnt);// get '}'
  end
  else
  begin
    vProcessingEscapedLiteral := False;
    vProcessingQuotedString := False;
    vQuote := 0;
    vStringAccumulator := TsgEasyStack.Create;
    try
      while True do
      begin
        vByte := ReadByte;
        vStringAccumulator.Push(vByte);
        if vStringAccumulator.Used > 65536 then
          Error(sCorruptFileError + ': exceeded the limit of the text');
        if vProcessingEscapedLiteral then
          vProcessingEscapedLiteral := False
        else
        case Chr(vByte) of
          cnstQuotationMark, (*unicode*)
          cnstApos:
            begin
              if (vByte = vQuote) and vProcessingQuotedString then
              begin
                vLength := vStringAccumulator.Used;
                SetLength(Result, vLength);
                for I := vLength downto 1 do
                  Result[I] := Char(vStringAccumulator.Pop);
                SetLength(Result, vLength-1);
                Break;
              end;
              if vStringAccumulator.Used = 1 then
              begin
                if vQuote = 0 then
                  vQuote := vByte;
                if vQuote = vByte then
                begin
                  vStringAccumulator.Clear;
                  vProcessingQuotedString := True;
                end;
              end;
              if (vQuote = 0) and (Chr(vByte) = cnstApos) and (vStringAccumulator.Used > 1) then
                Error(sCorruptFileError);
            end;
          cnstSpace,
          ')',
          #9,  // white space (Tab)
          #10, // white space (Line Feed)
          #13: // white space (Carriage Return)
            begin
              if not vProcessingQuotedString then
              begin
                vLength := vStringAccumulator.Used;
                SetLength(Result, vLength);
                for I := vLength downto 1 do
                  Result[I] := Char(vStringAccumulator.Pop);
                SetLength(Result, vLength-1);
                Dec(FCnt); //????
                Break;
              end;
            end;
          '(':
            begin
              if (not vProcessingQuotedString) and
                 ((*!allow_initial_open_paren) or *) vStringAccumulator.Used <> 1) then
                Error(sCorruptFileError);
            end;
          '{':
            begin
              if not vProcessingQuotedString then
                Error(sCorruptFileError);
            end;
          cnstBackSlash:
            begin
              vStringAccumulator.Pop;
              vProcessingEscapedLiteral := True;
            end;
        end;
      end;

      vLength := Length(Result);
      if (vLength <> 0) and (Chr(vLeadByte) = '"') then
      begin
        if (vLength mod 4 <> 0) then
          Error(sCorruptFileError);
        Result := string(RawByteHexStringToWideString(Result));
      end;
    finally
      vStringAccumulator.Free;
    end;
  end;
end;

function Tw2dParser.GetTextValue : string;
var
  vLim: set of AnsiChar;
  I: Integer;
  vC: Char;
begin
  SkipWhiteSpace;
  Result := StringOfChar(#0, 4096);
  I := 1;
  vLim := [#9, #10, #13, #32, ')'];
  vC := Chr(PDataValue);
  if CharInSet(vC, [#$22, #$27]) then
  begin
    vLim := [vC];
    Inc(FCnt);
  end;
  while not(CharInSet(Chr(PDataValue), vLim)) do
  begin
    Result[I] := Chr(PDataValue);
    Inc(I);
    Inc(FCnt);
  end;
  SetLength(Result, I - 1);
end;

procedure  Tw2dParser.SkipCharSet(const ACharSet: TSysCharSet);
begin
  while CharInSet(Chr(PDataValue), ACharSet) do
    Inc(FCnt);
end;

procedure Tw2dParser.Line(ACount: Integer; AProc: Tw2dParserProc);
var
  vLine: TsgDXFLine;
begin
  while ACount > 0 do
  begin
    vLine := NewLine;
    AProc;
    vLine.Point := FPoint;
    AProc;
    vLine.Point1 := FPoint;
    Dec(ACount);
  end;
end;


function Tw2dParser.Get_FS: Double;
var
  S: String;
  C: Char;
begin
  SkipWhiteSpace;
  while true do
  begin
    C := Chr(PDataValue);
    if not CharInSet(C, ['0'..'9','.','E','e','+','-']) then
      Break;
    S := S + C;
    Inc(FCnt);
  end;
  Result := StrToDouble(S, '.');;
end;


function Tw2dParser.NewArc: TsgDXFArc;
begin
  Result := TsgDXFArc.Create;
  Result.Color := GetCurrentColor;
  Result.SetLWeight(LineWeight);
  Result.Visibility := VisibleMode;
  AddEntity(Result);
end;

function Tw2dParser.NewCircle: TsgDXFCircle;
begin
  Result := TsgDXFCircle.Create;
  Result.Color := GetCurrentColor;
  Result.SetLWeight(LineWeight);
  Result.Visibility := VisibleMode;
  AddEntity(Result);
end;

function Tw2dParser.NewEllipse(ACenter: TFPoint; AR1, AR2: Double): TsgDXFEllipse;
begin
  Result := TsgDXFEllipse.Create;
  Result.Color := GetCurrentColor;
  Result.SetLWeight(LineWeight);
  Result.Visibility := VisibleMode;
  Result.Point := ACenter;

  // Cheak incorrect format
  if AR1 = 0 then
  begin
    if AR2 = 0 then
      AR2 := 1;
    AR1 := AR2;
  end
  else
    if AR2 = 0 then
      AR2 := AR1;

  if AR1 > AR2 then
  begin
    Result.Ratio := AR2 / AR1;
    Result.RadPt := MakeFPoint(AR1, 0);
  end
  else
  begin
    Result.Ratio := AR1 / AR2;
    Result.RadPt := MakeFPoint(AR2, 0);
  end;
  AddEntity(Result);
end;

function Tw2dParser.NewLine: TsgDXFLine;
begin
  Result := TsgDXFLine.Create;
  Result.Color := GetCurrentColor;
  Result.SetLWeight(Lineweight);
  Result.Visibility := VisibleMode;
  AddEntity(Result);
end;

function Tw2dParser.NewPoint: TsgDXFPoint;
begin
  Result := TsgDXFPoint.Create;
  Result.Color := GetCurrentColor;
  Result.SetLWeight(LineWeight);
  Result.Visibility := VisibleMode;
  AddEntity(Result);
end;

function Tw2dParser.NewPolyline: TsgDXFPolyline;
begin
  Result := TsgDXFPolyline.Create;
  Result.Polyline3D := True;
  Result.Color := GetCurrentColor;
  Result.SetLWeight(LineWeight);
  Result.Visibility := VisibleMode;
  AddEntity(Result);
end;

function Tw2dParser.NewText: TsgDXFText;
begin
  Result := TsgDXFText.Create;
  if FStyle <> nil then
    Result.Style := FStyle
  else
    Result.Style := FConv.StyleByName(cnstStandard);
  Result.Color := GetCurrentColor;
  Result.Height := FontHeight * 0.64;
  Result.Scale := FXScale;
  Result.Rotation := FontRotation;
  Result.Visibility := VisibleMode;
  AddEntity(Result);
end;

//function Tw2dParser.NewText: TsgDXFText;
//begin
//  Result := TsgDXFText.Create;
//  if FStyle <> nil then
//    Result.Style := FStyle
//  else  begin
//    FStyle := FConv.StyleByName('Arial');
//    FStyle.FontName := 'Arial';
//    Result.Style := FStyle;
//  end;
//  //  Result.Style := FConv.StyleByName(cnstStandard);
//  Result.Color := CurrColor;
//  Result.Height := FontHeight * 0.64;
//  //Result.HAlign :=   ;
//  Result.Scale := FXScale;
//  Result.Rotation := FontRotation;
//  Result.Visibility := VisibleMode;
//  AddEntity(Result);
//end;





procedure Tw2dParser.Parse;
begin
  case PDataValue of
    $7B:
      ExecuteExtBinary;
    $28:
      ExecuteExtASCII;
  else
    ExecuteOneByte;
  end;
end;

procedure Tw2dParser.PolyLine(ACount: Integer; AProc: Tw2dParserProc;
  AGouraud: Boolean);
var
  vPolyline: TsgDXFPolyline;
  vVertex: TsgDXFVertex;
begin
  vPolyline := NewPolyLine;
  if AGouraud then
  begin
    AProc;
    (*CurrColor := *)GetUnsignedLong;
    vVertex := TsgDXFVertex.Create;
    vVertex.Point := FPoint;
    vPolyline.AddEntity(vVertex);
    AProc;
    (*CurrColor := *)GetUnsignedLong;
    vVertex := TsgDXFVertex.Create;
    vVertex.Point := FPoint;
    vPolyline.AddEntity(vVertex);
    Dec(ACount, 2);
  end;
  while ACount > 0 do
  begin
    AProc;
    if AGouraud then
      (*CurrColor := *)GetUnsignedLong;
    vVertex := TsgDXFVertex.Create;
    vVertex.Point := FPoint;
    vPolyline.AddEntity(vVertex);
    Dec(ACount);
  end;
end;

procedure Tw2dParser.PolyMarker(const AIsBinary: Boolean; AProc: Tw2dParserProc);
var
  vCount: Integer;
  vPoint: TsgDXFPoint;
begin
  if AIsBinary then
    vCount := GetCount
  else
    vCount := GetIntSkipWhiteSpace;

  while vCount > 0 do
  begin
    AProc;
    // Default marker glyph
    case FMarkerGlyph of
      0: begin end;
    end;
    vPoint := NewPoint;
    vPoint.Point := FPoint;
    Dec(vCount);
  end;
end;

procedure Tw2dParser.PolyTriangle(ACount: Integer; AProc: Tw2dParserProc;
  AGouraud: Boolean);
var
  I: Integer;
  vPoint: array[0..2] of TFPoint;
  vColorVertex: array[0..2] of TColor;
  vOldColor: TColor;

  function GetColor: TColor;
  var
    vRGB: Integer;
  begin
    if AGouraud then
    begin
      vRGB := GetUnsignedLong and $00FFFFFF;
      SetCurrentColor(vRGB);
    end;
    Result := GetCurrentColor;
  end;

  procedure AddSolid(const APoint, APoint1, APoint2: TFPoint;
    const AColor: TColor);
  var
    vSolid: TsgDXFSolid;
  begin
    vSolid :=   TsgDXFSolid.Create;
    vSolid.Color := AColor;
    vSolid.Point := APoint;
    vSolid.Point1 := APoint1;
    vSolid.Point2 := APoint2;
    vSolid.Point3 := APoint2;
    AddEntity(vSolid);
  end;

  function GetPoint(const AProc: Tw2dParserProc; var ACount: Integer): TFPoint;
  begin
    AProc;
    Dec(ACount);
    Result := FPoint;
  end;

  procedure AddTriangle(APoint: array of TFPoint;
    AColor: array of TColor);
  var
    I: Integer;
    vColor: array[0..1] of TColor;
    vGradient: TsgCADCurvePolygon;
    vBoundary: Tsg2DBoundaryList;
    vPoly: Tsg2DPolyline;
  begin
    vColor[0] := AColor[0];
    vColor[1] := vColor[0];
    for I := 1 to 2 do
      if vColor[0] <> AColor[I] then
      begin
        vColor[1] := AColor[I];
        Break;
      end;

    if vColor[0] = vColor[1] then  //Solid
    begin
      AddSolid(APoint[0], APoint[1], APoint[2], vColor[0]);
    end
    else
    begin
      vGradient := TsgCADCurvePolygon.Create;
      vBoundary := vGradient.AddBoundaryList(7);
      vPoly := Tsg2DPolyline.Create;
      vBoundary.Add(vPoly);
      for I := 0 to 2 do
        vPoly.AddVertex(MakeF2DPointFrom3D(APoint[I]));
      vPoly.Closed := True;
      vGradient.GradientAngle := 25;
      vGradient.GradientName := cnstGradientName[3];
      for I := 0 to 1 do
        vGradient.GradientColorCAD[I] := MakeColorCAD(acRGBColor, vColor[I]);
      AddEntity(vGradient);
    end;
  end;

begin
  vOldColor := GetCurrentColor;
  try
    for I := 0 to 2 do
    begin
      vPoint[I] := GetPoint(AProc, ACount);
      vColorVertex[I] := GetColor;
    end;

    AddTriangle(vPoint, vColorVertex);

    while ACount > 0 do
    begin
      for I := 0 to 1 do
        vPoint[I] := vPoint[I+1];
      vPoint[2] := GetPoint(AProc, ACount);
      for I := 0 to 1 do
        vColorVertex[I] := vColorVertex[I+1];
      vColorVertex[2] := GetColor;

      AddTriangle(vPoint, vColorVertex);
    end;
  finally
    SetCurrentColor(vOldColor);
  end;
end;

procedure Tw2dParser.SetColorMapFromBinary;
var
  J: Integer;
begin
  CustomColorsCnt := GetByte;
  if CustomColorsCnt = 0 then
    CustomColorsCnt := 256;
  Getmem(FCustomColorMap, CustomColorsCnt * 4);
  for J := 0 to CustomColorsCnt {div 4} - 1 do
  begin
    if FVersion < cnstREVISION_WHEN_BINARY_COLOR_CHANGED_TO_RGBA_FROM_BGRA then
    begin
      FCustomColorMap^[J * 4 + 2] := GetByte; // blue
      FCustomColorMap^[J * 4 + 1] := GetByte; // green
      FCustomColorMap^[J * 4] := GetByte;     // red
      FCustomColorMap^[J * 4 + 3] := GetByte; // alpha
    end
    else
    begin
      FCustomColorMap^[J * 4] := GetByte;     // red
      FCustomColorMap^[J * 4 + 1] := GetByte; // green
      FCustomColorMap^[J * 4 + 2] := GetByte; // blue
      FCustomColorMap^[J * 4 + 3] := GetByte; // alpha
    end;
  end;
  if (PDataValue <> Byte('}')) then
    Error(sW2DBinaryComandError);
  CurrColorMap := @FCustomColorMap^[0];
  Inc(FCnt);
end;

procedure Tw2dParser.SetLayer(const AIsBinary: Boolean);
var
  vNumLayer, vIndexLayer: Integer;
begin
  if AIsBinary then
    vNumLayer := GetSmallint
  else
    vNumLayer := StrToInt(GetTextValue);

  vIndexLayer := FLayers.IndexOf(UInt64(vNumLayer));
  if vIndexLayer >= 0 then
  begin
    FCurrentLayer := TsgDXFLayer(FLayers.Items[vIndexLayer].Data);
    FCurrentLayerName := FCurrentLayer.Name;
  end
  else
  begin
    if AIsBinary then
      FCurrentLayerName := IntToStr(vNumLayer)
    else
      FCurrentLayerName := GetText;
    FCurrentLayerName := GetNameCorrect(FCurrentLayerName, [cnstVertSlash, cnstStar]);
    FCurrentLayer := FConv.LayerByName(FCurrentLayerName);
    FLayers.Add(UInt64(vNumLayer), Pointer(FCurrentLayer));
  end;
end;

procedure Tw2dParser.SkipExtASCII;
begin
  while FCnt < FDataLength do
    case Chr(GetByte) of
      ')':
        Exit;
      '(':
        begin
          Dec(FCnt);
          ExecuteExtASCII;
        end;
    end;
end;

procedure  Tw2dParser.SkipWhiteSpace;
begin
  while CharInSet(Chr(PDataValue), [#9, #10, #13, #32]) do
    Inc(FCnt);
end;

procedure Tw2dParser.SkipTo(const AChar: Char);
begin
  while  Chr(PDataValue)<> AChar do
    Inc(FCnt);
  Inc(FCnt);
end;

procedure Tw2dParser.GetUnits;
var
  I, J: Integer;
//  vBlock: TsgDXFBlock;
//  vMatrix: TFMatrix;

  procedure Next;
  begin
    SkipWhiteSpace;
    if PDataValue <> Byte('(') then
      Error(sExtASCIIErrorCode);
    Inc(FCnt);
  end;

begin
  SkipWhiteSpace;
  GetTextValue;
  Inc(FCnt);
  Next;
  for I := 0 to 3 do
  begin
    Next;
    for J := 0 to 2 do
      FUnitMatrix.M[I,J] := Get_FS;
    Get_FS;
    SkipExtASCII;
  end;

//  vBlock := TsgDXFBlock.Create;
//  vBlock.Name := 'Block_' + IntToHex(Integer(vBlock), 8);
//  FConv.Sections[csBlocks].AddEntity(vBlock);
//  FInsert := TsgDWFInsert.Create;
//  FInsert.Block := vBlock;
//  vMatrix.M[0, 0] := 1;
//  vMatrix.M[1, 1] := 1;
//  TsgDWFInsert(FInsert).SetInitMatrix(vMatrix);
//  EntitiesList.Add(FInsert);
end;


procedure Tw2dParser.GetURL;
var
  I: Integer;
  vIsDigit: Boolean;
  vText: string;
  vList: TStringList;

  function CutContent(const AList: TStringList): string;
  var
    vBreak: Integer;
    vOpenDoubleQuotes: Boolean;
    vByte: Byte;
    vPosBreak: Integer;
    vSubStr: string;
  begin
    Result := '';
    vSubStr := '';
    vBreak := 1;
    vOpenDoubleQuotes := False;
    while FCnt < FDataLength do
    begin
      vByte := GetByte;
      case Chr(vByte) of
        '''':
        begin
          vSubStr :=  vSubStr + Chr(vByte);
          vOpenDoubleQuotes := not vOpenDoubleQuotes;
         end;
        ')':
        begin
          if not vOpenDoubleQuotes then
          begin
            Dec(vBreak);
            if vBreak = 0 then
            begin
              Dec(FCnt);
              Break;
            end;
            AList.Add(vSubStr);
            vSubStr := '';
          end;
        end;
        '(':
          begin
            if not vOpenDoubleQuotes then
            begin
              vSubStr := '';
              Inc(vBreak);
            end;
          end;
        else
          if vBreak > 1 then
            vSubStr :=  vSubStr + Chr(vByte);
      end;
      Result := Result + Chr(vByte);
    end;
    vPosBreak := StringPos('(', Result, 1);
    if vPosBreak >= 1 then
      Delete(Result, vPosBreak, Length(Result)- vPosBreak + 1);
  end;

  procedure ParseStr(const AStr: string;
    var vIndex, vAddress, vName: string);
  var
    I: Integer;
    vLen: Integer;
    vMark: Integer;
    vFlag: Boolean;
    vCopyStr: string;
  begin
    vIndex := '';
    vAddress := '';
    vName := '';
    vMark := 0;
    vCopyStr := AStr;
    vLen := Length(vCopyStr);
    vFlag := True;
    ReplaceToPos(vCopyStr, '''', #13, Length(AStr));
    for I := 1 to vLen do
    begin
      if vCopyStr[I] = #13 then
      begin
        vFlag := not vFlag;
        Continue;
      end;
      if  (vCopyStr[I] = ' ') and vFlag then
      begin
        Inc(vMark);
        Continue;
      end;
      case vMark of
        0: vIndex := vIndex + vCopyStr[I];
        1: vAddress := vAddress + vCopyStr[I];
        2: vName := vName + vCopyStr[I];
      end;
    end;
    vIndex := Trim(vIndex);
  end;

  procedure AddULRListOne(const AStr: string);
  var
    vValue: TsgUrlValue;
  begin
    FHyperLinkList.Clear;
    vValue := TsgUrlValue.Create;
    vValue.URL := AStr;
    vValue.Name := AStr;
    FHyperLinkList.Add(0, vValue);
    FCurrentURLIndex := 0;
  end;

  procedure AddULRList(const AStr: string);
  var
    vIndex, vLinkIndex: Integer;
    vIndexStr, vAddress, vName: string;
    vValue: TsgUrlValue;
  begin
    ParseStr(AStr, vIndexStr, vAddress, vName);
    if Length(vIndexStr) > 0 then
    begin
      vIndex := StrToInt(vIndexStr);
      vLinkIndex := FHyperLinkList.IndexOf(vIndex);
      if vLinkIndex <> -1 then
      begin
        TsgUrlValue(FHyperLinkList.Items[vLinkIndex].Data).URL := vAddress;
        TsgUrlValue(FHyperLinkList.Items[vLinkIndex].Data).Name := vName;
      end
      else
      begin
        vValue := TsgUrlValue.Create;
        vValue.URL := vAddress;
        vValue.Name := vName;
        FHyperLinkList.Add(vIndex, vValue);
      end;
      FCurrentURLIndex := vIndex;
    end;
  end;

begin
  vList := TStringList.Create;
  try
    vText := CutContent(vList);
    vText := Trim(vText);
    vIsDigit := True;
    for I := 1 to Length(vText) do
      if not IsDigit(vText[I]) then
      begin
        vIsDigit := False;
        Break;
      end;
    if (Length(vText) > 0) and vIsDigit then
    begin
      FCurrentURLIndex := StrToInt(vText)
    end
    else
    begin
      if vList.Count = 0 then
      begin
        ReplaceToPos(vText, '''', #13, Length(vText));
        vText := Trim(vText);
        if Length(vText) > 0 then
          AddULRListOne(vText)
        else
          FCurrentURLIndex := -1
      end
      else
      for I := 0 to vList.Count - 1 do
        AddULRList(Trim(vList[I]));
    end;
  finally
    vList.Free;
  end;

//  Next;
//  vKey := GetIntSkipWhiteSpace;
//  vText := GetTextValue;
end;

procedure Tw2dParser.GetEntities;
var
  vCmnd: Word;
  vByte: Byte;
  I: Integer;
begin
  if FCnt < 0 then
  begin
    FCnt := 6;
    FVersion := 0;
    for I := 0 to 5 do
    begin
      vByte := GetByte;
      case Char(vByte) of
        '0'..'9':
          begin
            FVersion := 10 * FVersion  +  StrToInt(Char(vByte));
          end;
      end;
    end;
  end;
  FAbsX := 0;
  FAbsY := 0;
  FMarkerGlyph := 0;
  VisibleMode:= True;
  CurrColorMap := @DefaultColorMap[0];
  if FVersion < cnstREVISION_WHEN_DEFAULT_COLORMAP_WAS_CHANGED then
    CurrColorMap := @DefaultColorMap[0]
  else
    CurrColorMap := @NewDefaultColorMap[0];
  repeat
    SkipWhiteSpace;
    if PDataValue = $7B then
    begin
      vCmnd:= (PDataArray[FCnt + 6] shl 8) or PDataArray[FCnt + 5];
      case vCmnd of
        $0010, $0123:
          DecompressLZ;
        $0011:
          DecompressZlib;
      else
        Parse;  // ExtBinary
      end;
    end
    else
      Parse;
  until FCnt >= FDataLength - 2;
  // Delete Shadow Block
  if Assigned(FShadowBlockRef) then
  begin
    FShadowBlockRef.Free;
    FShadowBlockRef := nil;
    FCurrentBlockRef := nil;
  end;
end;

function Tw2dParser.IsDWF : Boolean;
const
  cnstDWF = $46574428; //'(DWF'
  cnstW2D = $44325728; //'(W2D'
  cnst_V = $5620; //' V';
begin
  Result := ((PCardinal(FPData)^ = cnstDWF) or (PCardinal(FPData)^ = cnstW2D)) and
    (PWord(TsgNativeUInt(FPData) + 4)^ = cnst_V);
end;

destructor Tw2dParser.Destroy;
begin
  FreeMem(FCustomColorMap);
  inherited Destroy;
end;

{ Tw3dParser }

procedure Tw3dParser.BoundingInfo;
var
  vByte: Byte;
  vPoint1, vPoint2: TFPoint;
  vHalfSize: Single;
  vDouble: Double;
  vLine: TsgDXFLine;

  function GetPointsByCenter(ACenter: TFPoint;
    var APoint1, APoint2: TFPoint): Single;
  begin
    Result := GetSingle;
    APoint1 := MakeFPoint(ACenter.X - Result, ACenter.Y - Result,
      ACenter.Z - Result);
    APoint2 := MakeFPoint(ACenter.X + Result, ACenter.Y + Result,
      ACenter.Z + Result);
  end;

begin
  vByte := GetByte;
  vPoint1 := GetPoint;
  if vByte = 0 then
    vPoint2 := GetPoint
  else
    GetPointsByCenter(vPoint1, vPoint1, vPoint2);
  vHalfSize := vPoint2.X - vPoint1.X;
  if vHalfSize < vPoint2.Y - vPoint1.Y then
    vHalfSize := vPoint2.Y - vPoint1.Y;
  if vHalfSize < vPoint2.Z - vPoint1.Z then
    vHalfSize := vPoint2.Z - vPoint1.Z;
  vDouble := (vHalfSize - vPoint2.X + vPoint1.X) / 2;
  vPoint1.X := vPoint1.X - vDouble;
  vPoint2.X := vPoint2.X + vDouble;
  vDouble := (vHalfSize - vPoint2.Y + vPoint1.Y) / 2;
  vPoint1.Y := vPoint1.Y - vDouble;
  vPoint2.Y := vPoint2.Y + vDouble;
  vDouble := (vHalfSize - vPoint2.Z + vPoint1.Z) / 2;
  vPoint1.Z := vPoint1.Z - vDouble;
  vPoint2.Z := vPoint2.Z + vDouble;
  vLine := TsgDXFLine(NewEntity(TsgDXFLine));
  vLine.Point := vPoint1;
  vLine.Point1 := vPoint1;
  vLine := TsgDXFLine(NewEntity(TsgDXFLine));
  vLine.Point := vPoint2;
  vLine.Point1 := vPoint2;
end;



procedure Tw3dParser.Circle(AIsCircle: Boolean);
var
  vPolyline: TsgDXFPolyline;
  vStartPoint, vMiddlePoint, vEndPoint, vCenterPoint: TFPoint;
  m_flags: Byte;
begin
  vStartPoint := GetPoint;
  vMiddlePoint := GetPoint;
  vEndPoint := GetPoint;
  if FVersionW3D >= 1215 then
    m_flags := GetByte
  else
    m_flags := 0;
  if (m_flags and cnstTKO_Circular_Center) <> 0 then
    vCenterPoint := GetPoint;
  vPolyline := TsgDXFPolyline(NewEntity(TsgDXFPolyline));
  CircleFromThreePoints(vPolyline, vStartPoint, vMiddlePoint, vEndPoint, 100, AIsCircle);
end;

procedure Tw3dParser.CloseSegment;
var
  I: Integer;
  vEntity: TsgDXFEntity;
  vTempBlock, vShadowBlock, vPrevCurrentBlock: TsgDXFBlock;
  vLoad: Boolean;
begin
  vLoad := True;
  vTempBlock := FCurrentBlock;

  if FStackBlock.Count <> 0 then
  begin
    FCurrentBlock := TsgDXFBlock(FStackBlock.Pop);
    if FCurrentBlock = vTempBlock then
    begin
      vLoad := False
    end
  end
  else
    FCurrentBlock := nil;

  I := 0;
  vShadowBlock := nil;
  while I < FListShadowBlock.Count do
  begin
    if Pointer(vTempBlock) = Pointer(FListShadowBlock[I]) then
    begin
      vShadowBlock := vTempBlock;
      FListShadowBlock.Delete(I);
      vPrevCurrentBlock := FCurrentBlock;
      FCurrentBlock := TsgDXFBlock(FStackBlock.Pop);
      if FCurrentBlock = vPrevCurrentBlock then
      begin
        vLoad := False
      end;
      vTempBlock := FCurrentBlock;
      FInsertPrev := nil;
      Break;
    end;
    Inc(I);
  end;
  if vShadowBlock <> nil then
  begin
    I := 0;
    while I < vShadowBlock.Count do
    begin
      vEntity := vShadowBlock[I];
      FConv.Loads(vEntity);
      Inc(I);
    end;
     FConv.Loads(vShadowBlock);
  end;

  if vLoad and (vTempBlock <> nil) then
  begin
    I := 0;
    while I < vTempBlock.Count do
    begin
      vEntity := vTempBlock[I];
      FConv.Loads(vEntity);
      Inc(I);
    end;
     FConv.Loads(vTempBlock);
  end;
end;

procedure Tw3dParser.Color;
var
  vChannels: Word;
  vProgress: Integer;

//  vGloss, vIndex: Double;

  vDiffuse, vSpecular, vMirror, vTransmission: TsgChannel;
  vEmission, vEnvironment, vBump: TsgChannel;

  vDiffuseRGB, vSpecularRGB, vMirrorRGB, vTransmissionRGB: TColor;
  vEmissionRGB{, vEnvironmentRGB, vBumpRGB}: TColor;
  procedure SetChannelName( AChannel: TsgChannel; ALength: Integer;
    WhichChannel: Integer = -1 );
  begin
    SetLength(AChannel.Name,ALength);
    if WhichChannel <> -1 then
      vChannels := vChannels or vChannels shl WhichChannel;
  end;

  procedure ReadChannel(AChannels: Word; AChannel: TsgChannel;
    AColor: PColor; vFlag: Integer);
  begin
    if AChannels and (1 shl vFlag) <> 0 then
    begin
      vProgress := GetByte;
      if vProgress <> 0 then
      begin
        SetChannelName (AChannel, vProgress);
        AChannel.Name := GetStr(vProgress);
      end
      else
      begin
        AColor^ := GetByte;
        AColor^ := AColor^ or GetByte shl 8;
        AColor^ := AColor^ or GetByte shl 16;
      end;
    end;
  end;

begin
  FLastMask := GetByte;
  if FLastMask and cnstTKO_Geo_Extended <> 0  then
    FLastMask := FLastMask or GetByte shl 8;
  if FLastMask and cnstTKO_Geo_Extended_Colors <> 0 then
    FLastMask := FLastMask or GetByte shl 16;
  if FLastMask and cnstTKO_Geo_Extended2 <> 0 then
    FLastMask := FLastMask or GetByte shl 24;
  vChannels := GetByte;
  if vChannels and (1 shl cnstTKO_Channel_Extended) <> 0 then
    vChannels := vChannels or GetByte shl 8;
  vProgress := -1;
  // diffuse - basic color of the objects
  if vChannels and (1 shl cnstTKO_Channel_Diffuse) <> 0 then
  begin
    if vProgress = -1 then
      vProgress := GetByte;
    if vProgress = 255 then
      vProgress := GetByte;
    if vProgress <> 0 then
    begin
      SetChannelName (vDiffuse, vProgress);
      vDiffuse.Name := GetStr(vProgress)
    end
    else
    begin
      vDiffuseRGB := GetByte;
      vDiffuseRGB := vDiffuseRGB or GetByte shl 8;
      vDiffuseRGB := vDiffuseRGB or GetByte shl 16;

      FColor := vDiffuseRGB;
    end;
  end;
  ReadChannel(vChannels, vSpecular, @vSpecularRGB, cnstTKO_Channel_Specular);
  ReadChannel(vChannels, vMirror, @vMirrorRGB, cnstTKO_Channel_Mirror);
  ReadChannel(vChannels, vTransmission, @vTransmissionRGB, cnstTKO_Channel_Transmission);
  ReadChannel(vChannels, vEmission, @vEmissionRGB, cnstTKO_Channel_Emission);
  if vChannels and (1 shl cnstTKO_Channel_Gloss) <> 0 then
    GetSingle;//vGloss := GetSingle;
  if vChannels and (1 shl cnstTKO_Channel_Index) <> 0 then
    GetSingle;//vIndex := GetSingle;
  if vChannels and (1 shl cnstTKO_Channel_Environment) <> 0 then
  begin
    vProgress := GetByte;
    //if vProgress <> 0 then
      SetChannelName (vEnvironment, vProgress);
    vEnvironment.Name := GetStr(vProgress)
  end;
  if vChannels and (1 shl cnstTKO_Channel_Bump) <> 0 then
  begin
    vProgress := GetByte;
    //if vProgress <> 0 then
      SetChannelName (vBump, vProgress);
    vBump.Name := GetStr(vProgress)
  end;
end;

procedure Tw3dParser.ColorRGB;
begin
  FLastMask := GetByte;
  if FLastMask and cnstTKO_Geo_Extended <> 0 then
    FLastMask := FLastMask or GetByte shl 8;
  if FLastMask and cnstTKO_Geo_Extended_Colors <> 0 then
    FLastMask := FLastMask or GetByte shl 16;
  if FLastMask and cnstTKO_Geo_Extended2 <> 0 then
    FLastMask := FLastMask or GetByte shl 24;
  FColor := GetByte;
  FColor := FColor or GetByte shl 8;
  FColor := FColor or GetByte shl 16;
end;

function Tw3dParser.GetPoint: TFPoint;
begin
  Result.X := GetSingle;
  Result.Y := GetSingle;
  Result.Z := GetSingle;
end;

function Tw3dParser.GetSingle: Single;
begin
  PInteger(@Result)^ := GetLong;
  if PCardinal(@Result)^ = $FFC00000 then
    PCardinal(@Result)^ := 0;
end;

function Tw3dParser.GetStr(ALen: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, ALen);
  for I := 1 to ALen do
    Result[I] := Chr(GetByte);
end;

function Tw3dParser.GetStringBT: string;
begin
  Result := GetStr(GetByte);
end;

function Tw3dParser.GetStringST: string;
begin
  Result := GetStr(GetWord);
end;

function Tw3dParser.GetStringVT: string;
var
  vL: Integer;
begin
  vL := GetByte;
  case vL of
    254: vL := GetWord;
    255: vL := GetUnsignedLong;
  end;
  Result := GetStr(vL);
end;

function Tw3dParser.GetStringVariableLength: string;
var
  vLength: Integer;
begin
  vLength := GetWord;
  if vLength = 65535 then
    vLength := GetLong;
  Result := GetStr(vLength);
end;

procedure Tw3dParser.Heuristics;
var
  vMask, vValue: Integer;
  vCulling: Integer;
  vOrderedWeightsMask: Byte;
  vProgress: Byte;
begin
  vMask := GetWord;
  if vMask and cnstTKO_Heuristic_Extended <> 0 then
    vMask := vMask or GetWord shl 16;
  vValue := GetWord;
  if vMask and cnstTKO_Heuristic_Extended <> 0 then
    vValue := vValue or GetWord shl 16;
  if (vMask and vValue and cnstTKO_Heuristic_Related_Select_Limit) <> 0 then
  begin
    GetLong;
  end;
  if (vMask and vValue and cnstTKO_Heuristic_Internal_Shell_Limit) <> 0 then
  begin
    GetLong;
  end;
  if (vMask and vValue and cnstTKO_Heuristic_Extras) <> 0 then
  begin
    GetByte;
  end;
  if (FVersionW3D > 705) and
     (vMask and cnstTKO_Heuristic_Culling <> 0) and
     (vMask and vValue and cnstTKO_Heuristic_Culling <> 0) then
  begin
    vCulling := GetByte;
  end
  else
    vCulling := 0;
  if (vCulling and cnstTKO_Heur_Culling_Extended) <> 0 then
  begin
    vCulling := vCulling or GetByte shl 8;
  end;
  if (FVersionW3D > 1200) and
     (vMask and cnstTKO_Heuristic_Culling <> 0) and
     (vCulling and cnstTKO_Heur_Obscuration_Culling <> 0) then
  begin
    GetLong;
  end;
  if (FVersionW3D > 1200) and
     (vMask and cnstTKO_Heuristic_Culling <> 0) and
     (vCulling and cnstTKO_Heur_Extent_Culling <> 0) then
  begin
    GetLong;
  end;
  if (FVersionW3D > 1200) and
     (vMask and cnstTKO_Heuristic_Culling <> 0) and
     (vCulling and cnstTKO_Heur_Maximum_Extent_Mode <> 0) then
  begin
    GetLong;
  end;
  if (vMask and cnstTKO_Heuristic_Ordered_Weights)  <> 0 then
  begin
    vOrderedWeightsMask := GetByte;
    vProgress := 0;
    while vProgress < cnstTKO_Heur_Order_Count do
    begin
      if (vOrderedWeightsMask and (1 shl vProgress)) <> 0 then
      begin
        GetSingle;
      end;
      Inc(vProgress);
    end;
  end;
  if (vMask and vValue and cnstTKO_Heuristic_Internal_Polyline_Limit) <> 0 then
  begin
    GetLong;
  end;
  if (vMask and vValue and cnstTKO_Heuristic_Selection_Level) <> 0 then
  begin
    GetByte;
  end;
  if (vMask and cnstTKO_Heuristic_Culling <> 0) and
     (vCulling and cnstTKO_Heur_Vector_Culling <> 0) then
  begin
    GetPoint;
    GetPoint;
    GetPoint;
  end;
  if (vMask and cnstTKO_Heuristic_Culling <> 0) and
     (vCulling and cnstTKO_Heur_Vector_Tolerance <> 0) then
  begin
    GetSingle;
  end;
end;

procedure Tw3dParser.Line;
var
  vLine: TsgDXFLine;
begin
  vLine := TsgDXFLine(NewEntity(TsgDXFLine));
  vLine.Point := GetPoint;
  vLine.Point1 := GetPoint;
end;

procedure Tw3dParser.LinePattern;
begin
  GetByte;
  GetStringBT;
end;

procedure Tw3dParser.LineType;
begin
  GetStringBT;
  GetStringST;
end;

procedure Tw3dParser.LineWeight;
//var
//  vMUnits: Byte;
begin
  FLWeight := GetSingle;
  if FLWeight < 0 then
  begin
    GetByte;//vMUnits := GetByte;
    FLWeight := -FLWeight;
  end
  else
  begin
//    vMUnits := 8;
  end;
end;

procedure Tw3dParser.Matrix;
var
  vBlock: TsgDXFBlock;
  vInsert: TsgDXFInsert;
  vMatrix: TFMatrix;
  vTempString: string;
  I: Integer;
begin
  vMatrix := GetMatrix;

  if (FCurrentBlock <> nil) and (FInsertPrev <> nil) then
  begin
    SetInitMatrix(FInsertPrev, vMatrix);
  end
  else
  begin
    vBlock := TsgDXFBlock.Create;
    FStackBlock.Push(FCurrentBlock);

    if FCurrentBlock = nil then
      vBlock.Name := 'DWFText_' + IntToHex(TsgNativeUInt(vBlock), 8)
    else
    begin
      FListShadowBlock.Add(vBlock);
      vTempString := FCurrentBlock.Name;
      vBlock.Name := 'DWFSHADOW_' +vTempString+ IntToHex(TsgNativeUInt(vBlock), 8);
      // Copy Entity courent block to shadow block
      I := FCountPrevEntityBlock;
      while I < FCurrentBlock.Count  do
      begin
        vBlock.AddEntity(FCurrentBlock.DeleteEntity(FCurrentBlock.Count-1));
      end;
      FCountPrevEntityBlock := 0;
    end;

    FConv.Sections[csBlocks].AddEntity(vBlock);
    vInsert := TsgDXFInsert(NewEntity(TsgDXFInsert));
    vInsert.Block := vBlock;
    SetInitMatrix(vInsert, vMatrix);
    FInsertPrev := vInsert;

    //
    // Important! - Appoint FCurrentBlock only after create vInsert
    //
    FCurrentBlock := vBlock;
  end;
end;

function Tw3dParser.NewEntity(AClass: TsgDXFEntityClass): TsgDXFEntity;
begin
  Result := AClass.Create;
  Result.Color := FColor;
  Result.LineWeight := FLWeight;
  if FCurrentBlock = nil then
    EntitiesList.Add(Result)
  else
    FCurrentBlock.AddEntity(Result);
end;

procedure Tw3dParser.NurbsCurve;
var
  vOptionals, vDegree: Byte;
  I: Integer;
  vControlPointCount, vKnotCountImplicit: Integer;
  vControlPoints: TFPointList;
  vWeights, vKnots: TsgSingleList;
//  vStart, vEnd: TsgFloat;
begin
  vOptionals := GetByte;
  vDegree := GetByte;
  vControlPointCount := GetLong;
  vControlPoints := TFPointList.Create;
  vWeights := TsgSingleList.Create;
  vKnots := TsgSingleList.Create;
  try
    vControlPoints.Count := vControlPointCount;
    for I := 0 to vControlPoints.Count - 1 do
      vControlPoints[I] := GetPoint;
    if vOptionals and cnstNC_HAS_WEIGHTS <> 0 then
    begin
      vWeights.Count := vControlPointCount;
      for I := 0 to vWeights.Count - 1 do
        vWeights[I] := GetSingle;
    end;
    vKnotCountImplicit := vDegree + vControlPointCount + 1;
    if vOptionals and cnstNC_HAS_KNOTS <> 0 then
    begin
      vKnots.Count := vKnotCountImplicit;
      for I := 0 to vKnots.Count - 1 do
        vKnots[I] := GetSingle;
    end;
    if vOptionals and cnstNC_HAS_START <> 0 then
    begin
      GetSingle;{vStart := GetSingle;}
    end
    else
      begin {vStart := 0.0;} end;
    if vOptionals and cnstNC_HAS_END <> 0 then
    begin
      GetSingle;//vEnd := GetSingle;
    end
    else
      begin {vEnd := 1.0;} end;
  finally
    vControlPoints.Free;
    vWeights.Free;
    vKnots.Free;
  end;
end;

function Tw3dParser.EnumerateEdges(AListFaces: TsgIntegerList;
  ASuboptions: Byte): Integer;
var
  vMpEdgeEnumeration: TsgIntegerList;
  vSortList: TF2DPointList;
  vTempPoint: TF2DPoint;
  vLen, vIndex, I, vEnumcount, vTemp: Integer;
begin
  vMpEdgeEnumeration := TsgIntegerList.Create;
  vSortList := TF2DPointList.Create;
  try
    vMpEdgeEnumeration.Count := AListFaces.Count * 4;
    vEnumcount := 0;
    if ASuboptions and cnstTKSH_TRISTRIPS <> 0 then
    begin
    end
    else
    begin
      vIndex := 0;
      while vIndex < AListFaces.Count do
      begin
        vLen := Abs(AListFaces[vIndex]);
        for I := 1 to vLen do
        begin
          if I = 1 then
            vMpEdgeEnumeration[vEnumcount] := AListFaces[vIndex + vLen]
          else
            vMpEdgeEnumeration[vEnumcount] := AListFaces[vIndex + I - 1];
          Inc(vEnumcount);
          vMpEdgeEnumeration[vEnumcount] := AListFaces[vIndex + I];
          Inc(vEnumcount);
        end;
        vIndex := vIndex + vLen + 1;
      end;
    end;

    for I := 0 to (vEnumcount div 2) - 1 do
    begin
      if vMpEdgeEnumeration[I*2] > vMpEdgeEnumeration[I*2+1] then
      begin
        vTemp := vMpEdgeEnumeration[I*2];
        vMpEdgeEnumeration[I*2] := vMpEdgeEnumeration[I*2+1];
        vMpEdgeEnumeration[I*2+1] := vTemp;
      end;
    end;

    vSortList.Capacity := vMpEdgeEnumeration.Count div 2;
    I := 0;
    while I < vEnumcount do
    begin
      vSortList.Add(MakeF2DPoint(vMpEdgeEnumeration[I],vMpEdgeEnumeration[I+1]));
      Inc(I, 2);
    end;
    vSortList.Sort;

    vTempPoint := vSortList.First;
    vIndex := 1;
    for I := 1 to vSortList.Count - 1 do
    begin
      if not IsEqualF2DPoints(vTempPoint, vSortList[I]) then
      begin
        vTempPoint := vSortList[I];
        Inc(vIndex);
      end;
    end;
  finally
    vSortList.Free;
    vMpEdgeEnumeration.Free;
  end;
  Result := vIndex;
end;

procedure Tw3dParser.TrivialDecompressFaces(AListFaces: TsgIntegerList;
  AHasNegativeFaces: Integer);
var
  vBytesPerSample, vLenFaces, I, J: Integer;
begin
  if AListFaces.List[0] < 8 then
    Exit;
  vBytesPerSample := AListFaces.List[0] div 8;
  vLenFaces := (AListFaces.Count - 1) div vBytesPerSample;
  J := 0;
  I := 1;
  if (FVersionW3D >= 650) and (AHasNegativeFaces <> 0) then
  begin
    case vBytesPerSample of
      1: AListFaces.Delete(0);
      2:
      begin
        while I < AListFaces.Count do
        begin
          AListFaces.List[J] := AListFaces.List[I] or (AListFaces.List[I+1] shl 8);
          Inc(I,2);
          Inc(J);
        end;
      end;
      4:
      begin
        while I < AListFaces.Count do
        begin
          AListFaces.List[J] := AListFaces.List[I] or
            (AListFaces.List[I+1] shl 8) or
            (AListFaces.List[I+2] shl 16) or
            (AListFaces.List[I+3] shl 24);
          Inc(I,4);
          Inc(J);
        end;
      end;
    end;
  end
  else
  begin
    case vBytesPerSample of
      1: AListFaces.Delete(0);
      2:
      begin
        while I < AListFaces.Count do
        begin
          AListFaces.List[J] := AListFaces.List[I];
          AListFaces.List[J] := AListFaces.List[J] +
            (AListFaces.List[I+1] shl 8);
          Inc(I,2);
          Inc(J);
        end;
      end;
      4:
      begin
        while I < AListFaces.Count do
        begin
          AListFaces.List[J] := AListFaces.List[I];
          AListFaces.List[J] := AListFaces.List[J] + (AListFaces.List[I+1] shl 8);
          AListFaces.List[J] := AListFaces.List[J] + (AListFaces.List[I+2] shl 16);
          AListFaces.List[J] := AListFaces.List[J] + (AListFaces.List[I+3] shl 24);
          Inc(I,4);
          Inc(J);
        end;
      end;
    end;
  end;
  AListFaces.Count := vLenFaces;
end;

procedure Tw3dParser.RenderingOptions;
var
  vMask, vValue: array[0..1] of Integer;
  vLockMask, vLockValue: Integer;
  {vLockColorMask,} vLockColorValue: Integer;
  vBufferOptionsMask, vBufferOptionsValue: Byte;
  vHlrOptions: Integer;
  vHlrWeight: TsgFloat;
  vNurbsOptionsMask, vNurbsOptionsValue: Integer;
  vLodOptionsMask, vLodOptionsValue: Integer;
  vNumCutoffs, vNumRatios, vNumThresholds, vNumCylinder, vNumSphere: Byte;
  vTessellations, vTransparencyOptions, vShadowMap, vGeometryOptions, vCutGeometry: Byte;
  I: Integer;
  vSimpleShadow, vSimpleReflection: Word;
begin
  vMask[0] := GetLong;
  if vMask[0] and cnstTKO_Rendo_Extended <> 0 then
    vMask[1] := GetLong
  else
    vMask[1] := 0;
  vValue[0] := GetLong;
  if vMask[0] and cnstTKO_Rendo_Extended <> 0 then
    vValue[1] := GetLong
  else
    vValue[1] := 0;

  if vMask[0] and cnstTKO_Rendo_Any_HSR <> 0 then
    GetByte; // m_hsr

  if vMask[0] and cnstTKO_Rendo_TQ <> 0 then
    GetByte; // m_tq

  if  (vMask[0] and vValue[0] and cnstTKO_Rendo_Face_Displacement) <> 0 then
    GetLong; // m_face_displacement

  if  (vMask[0] and vValue[0] and cnstTKO_Rendo_Attribute_Lock) <> 0 then
  begin
    vLockMask := GetLong; // m_lock_mask
    vLockValue := GetLong; // m_lock_value
    if ((vLockMask and vLockValue and cnstTKO_Lock_Color) <> 0) then
      GetLong;//vLockColorMask := GetLong; // m_lock_color_mask
    if ((vLockMask and vLockValue and cnstTKO_Lock_Color) <> 0) then
    begin
      if FVersionW3D <= 1105 then
      begin
        GetLong; //vLockColorValue := GetLong; // m_lock_color_value
        // ...
        // ...
        // ...
      end
      else
      begin
        vLockColorValue := GetLong; // m_lock_color_value
        if vLockColorValue and (cnstTKO_Geo_Face or cnstTKO_Geo_Front) <> 0 then
        begin
          GetWord; // m_lock_color_face_mask
          GetWord; // m_lock_color_face_value
        end;
        if vLockColorValue and cnstTKO_Geo_Back <> 0 then
        begin
          GetWord; // m_lock_color_back_mask
          GetWord; // m_lock_color_back_value
        end;
        if vLockColorValue and cnstTKO_Geo_Edge <> 0 then
        begin
          GetWord; // m_lock_color_edge_mask
          GetWord; // m_lock_color_edge_value
        end;
        if vLockColorValue and cnstTKO_Geo_Line <> 0 then
        begin
          GetWord; // m_lock_color_line_mask
          GetWord; // m_lock_color_line_value
        end;
        if vLockColorValue and cnstTKO_Geo_Text <> 0 then
        begin
          GetWord; // m_lock_color_text_mask
          GetWord; // m_lock_color_text_value
        end;
        if vLockColorValue and cnstTKO_Geo_Marker <> 0 then
        begin
          GetWord; // m_lock_color_marker_mask
          GetWord; // m_lock_color_marker_value
        end;
        if vLockColorValue and cnstTKO_Geo_Vertex <> 0 then
        begin
          GetWord; // m_lock_color_vertex_mask
          GetWord; // m_lock_color_vertex_value
        end;
        if vLockColorValue and cnstTKO_Geo_Window <> 0 then
        begin
          GetWord; // m_lock_color_window_mask
          GetWord; // m_lock_color_window_value
        end;
        if vLockColorValue and cnstTKO_Geo_Face_Contrast <> 0 then
        begin
          GetWord; // m_lock_color_face_contrast_mask
          GetWord; // m_lock_color_face_contrast_value
        end;
        if vLockColorValue and cnstTKO_Geo_Edge_Contrast <> 0 then
        begin
          GetWord; // m_lock_color_edge_contrast_mask
          GetWord; // m_lock_color_edge_contrast_value
        end;
        if vLockColorValue and cnstTKO_Geo_Line_Contrast <> 0 then
        begin
          GetWord; // m_lock_color_line_contrast_mask
          GetWord; // m_lock_color_line_contrast_value
        end;
        if (vLockColorValue and cnstTKO_Geo_Text_Contrast <> 0) or
          (FVersionW3D < 1555) and (vLockColorValue and cnstTKO_Geo_Cut_Edge <> 0) then
        begin
          GetWord; // m_lock_color_text_contrast_mask
          GetWord; // m_lock_color_text_contrast_value
        end;
        if vLockColorValue and cnstTKO_Geo_Marker_Contrast <> 0 then
        begin
          GetWord; // m_lock_color_marker_contrast_mask
          GetWord; // m_lock_color_marker_contrast_value
        end;
        if vLockColorValue and cnstTKO_Geo_Vertex_Contrast <> 0 then
        begin
          GetWord; // m_lock_color_vertex_contrast_mask
          GetWord; // m_lock_color_vertex_contrast_value
        end;
        if vLockColorValue and cnstTKO_Geo_Window_Contrast <> 0 then
        begin
          GetWord; // m_lock_color_window_contrast_mask
          GetWord; // m_lock_color_window_contrast_value
        end;
        if vLockColorValue and cnstTKO_Geo_Simple_Reflection <> 0 then
        begin
          if FVersionW3D >= 1550 then
          begin
            GetWord; // m_lock_color_simple_reflection_mask
            GetWord; // m_lock_color_simple_reflection_value
          end;
        end;
        if vLockColorValue and cnstTKO_Geo_Cut_Face <> 0 then
        begin
          if FVersionW3D >= 1220 then
          begin
            GetWord; // m_lock_color_cut_face_mask
            GetWord; // m_lock_color_cut_face_value
          end;
        end;
        if vLockColorValue and cnstTKO_Geo_Cut_Edge <> 0 then
        begin
          if FVersionW3D >= 1220 then
          begin
            GetWord; // m_lock_color_cut_edge_mask
            GetWord; // m_lock_color_cut_edge_value
          end;
        end;
      end;
    end;
    if (vLockMask and vLockValue and cnstTKO_Lock_Visibility <> 0) then
    begin
      GetLong; // m_lock_visibility_mask
      GetLong; // m_lock_visibility_value
    end;
  end;

  if vMask[0] and vValue[0] and cnstTKO_Rendo_Fog <> 0 then
  begin
    GetSingle; // m_fog_limits[2]
    GetSingle; //
  end;
  if vMask[0] and vValue[0] and cnstTKO_Rendo_Debug <> 0 then
  begin
    GetLong; // m_debug
  end;
  if vMask[0] and vValue[0] and cnstTKO_Rendo_Stereo_Separation <> 0 then
  begin
    GetSingle; // m_stereo_separation
  end;
  if vMask[1] and vValue[1] and cnstTKO_Rendo_Stereo_Distance <> 0 then
  begin
    GetSingle; // m_stereo_distance
  end;
  if vMask[0] and vValue[0] and cnstTKO_Rendo_Buffer_Options <> 0 then
  begin
    vBufferOptionsMask := GetByte; // m_buffer_options_mask
    vBufferOptionsValue := GetByte; // m_buffer_options_value
    if vBufferOptionsMask and vBufferOptionsValue and cnstTKO_Buffer_Size_Limit <> 0 then
      GetLong; // m_buffer_size_limit
  end;

  if vMask[0] and vValue[0] and cnstTKO_Rendo_Hidden_Line_Options <> 0 then
  begin
    vHlrOptions := GetByte;
    if vHlrOptions and cnstTKO_Hidden_Line_Extended <> 0 then
      vHlrOptions := vHlrOptions or GetByte shl 8;
    if vHlrOptions and cnstTKO_Hidden_Line_Extended2 <> 0 then
      vHlrOptions := vHlrOptions or GetByte shl 16;
    if vHlrOptions and cnstTKO_Hidden_Line_Pattern <> 0 then
      GetLong; // m_hlr_line_pattern
    if vHlrOptions and cnstTKO_Hidden_Line_Dim_Factor <> 0 then
      GetSingle; // m_hlr_dim_factor
    if vHlrOptions and cnstTKO_Hidden_Line_Face_Displacement <> 0 then
      GetSingle; // m_hlr_face_displacement
    if vHlrOptions and cnstTKO_Hidden_Line_Color <> 0 then
    begin
      GetSingle; // m_hlr_color[3]
      GetSingle;
      GetSingle;
    end;
    if vHlrOptions and cnstTKO_Hidden_Line_Weight <> 0 then
    begin
      vHlrWeight := GetSingle; // m_hlr_weight
      if vHlrWeight <> -1.0 then
        GetByte; // m_hlr_weight_units
    end;
    if vHlrOptions and cnstTKO_Hidden_Line_HSR_Algorithm <> 0 then
      GetByte; // m_hlr_hsr_algorithm
  end;

  if vMask[0] and cnstTKO_Rendo_NURBS_Options <> 0 then
  begin
    vNurbsOptionsMask := GetByte;
    if vNurbsOptionsMask and cnstTKO_NURBS_Extended <> 0 then
      vNurbsOptionsMask := vNurbsOptionsMask or GetByte shl 8;
    vNurbsOptionsValue := GetByte;
    if vNurbsOptionsMask and cnstTKO_NURBS_Extended <> 0 then
      vNurbsOptionsValue := vNurbsOptionsValue or GetByte shl 8;
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Curve_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Curve_Budget <> 0)then
      GetLong; // m_curve_budget
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Curve_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Curve_Continued_Budget <> 0)then
      GetLong; // m_curve_continued_budget
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Surface_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Surface_Trim_Budget <> 0)then
      GetLong; // m_surface_trim_budget
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Surface_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Surface_Max_Trim_Curve_Deviation <> 0)then
      GetSingle; // m_surface_max_trim_curve_deviation
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Surface_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Surface_Max_Facet_Angle <> 0)then
      GetSingle; // m_surface_max_facet_angle
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Surface_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Surface_Max_Facet_Deviation <> 0)then
      GetSingle; // m_surface_max_facet_deviation
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Surface_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Surface_Max_Facet_Width <> 0)then
      GetSingle; // m_surface_max_facet_width
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Surface_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Curve_Max_Angle <> 0)then
      GetSingle; // m_curve_max_angle
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Surface_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Curve_Max_Deviation <> 0)then
      GetSingle; // m_curve_max_deviation
    if (vMask[0] and vValue[0] and cnstTKO_Rendo_NURBS_Surface_Options <> 0) and
      (vNurbsOptionsMask and vNurbsOptionsValue and cnstTKO_NURBS_Curve_Max_Length <> 0)then
      GetSingle; // m_curve_max_length
  end;

  if vMask[0] and vValue[0] and cnstTKO_Rendo_LOD_Options <> 0 then
  begin
    vLodOptionsMask := GetLong;
    vLodOptionsValue := GetLong;
    if vLodOptionsMask and vLodOptionsValue and cnstTKO_LOD_Algorithm <> 0 then
      GetByte; // m_lod_algorithm
    if vLodOptionsMask and vLodOptionsValue and cnstTKO_LOD_Bounding_Explicit <> 0 then
    begin
      GetPoint; // m_bounding[6]
      GetPoint;
    end;
    if vLodOptionsMask and vLodOptionsValue and cnstTKO_LOD_Calculation_Cutoff <> 0 then
    begin
      vNumCutoffs := GetByte; // m_num_cutoffs
      I := 0;
      while I < vNumCutoffs do
      begin
        GetSingle; // m_cutoff[8]
      end;
    end;
    if vLodOptionsMask and vLodOptionsValue and cnstTKO_LOD_Clamp <> 0 then
      GetByte; // m_clamp
    if vLodOptionsMask and vLodOptionsValue and cnstTKO_LOD_Fallback <> 0 then
      GetByte; // m_fallback
    if vLodOptionsMask and vLodOptionsValue and cnstTKO_LOD_Max_Degree <> 0 then
      GetLong; // m_max_degree
    if vLodOptionsMask and cnstTKO_LOD_Min_Triangle_Count <> 0 then
      GetLong; // m_min_triangle_count
    if vLodOptionsMask and cnstTKO_LOD_Num_Levels <> 0 then
      GetByte; // m_num_levels
    if vLodOptionsMask and cnstTKO_LOD_Ratio <> 0 then
    begin
      vNumRatios := GetByte; // m_num_ratios
      I := 0;
      while I < vNumRatios do
      begin
        GetSingle; // m_ratio[8]
      end;
    end;
    if vLodOptionsMask and cnstTKO_LOD_Threshold <> 0 then
    begin
      vNumThresholds := GetByte; // m_num_thresholds
      I := 0;
      while I < vNumThresholds do
      begin
        GetSingle; // m_threshold[8]
      end;
    end;
    if (vLodOptionsMask and cnstTKO_LOD_Threshold <> 0) and (FVersionW3D >= 908) then
      GetByte; // m_threshold_type
    if vLodOptionsMask and vLodOptionsValue and cnstTKO_LOD_Usefulness_Heuristic <> 0 then
      GetByte; // m_heuristic
  end;

  if vMask[1] and vValue[1] and cnstTKO_Rendo_Tessellation <> 0 then
  begin
    vTessellations := GetByte;
    if vTessellations and cnstTKO_Tessellation_Cylinder <> 0 then
    begin
      vNumCylinder := GetByte; // m_num_cylinder
      I := 0;
      while I < vNumCylinder do
      begin
        GetByte; // m_cylinder[8]
      end;
    end;
    if vTessellations and cnstTKO_Tessellation_Sphere <> 0 then
    begin
      vNumSphere := GetByte; // m_num_sphere
      I := 0;
      while I < vNumSphere do
      begin
        GetByte; // m_sphere[8]
      end;
    end;
  end;

  if vMask[1] and vValue[1] and
    (cnstTKO_Rendo_Transparency_Style or cnstTKO_Rendo_Transparency_Options) <> 0 then
  begin
    vTransparencyOptions := GetByte;
    if vTransparencyOptions and cnstTKO_Transparency_Peeling_Layers <> 0 then
      GetByte; // m_depth_peeling_layers
    if vTransparencyOptions and cnstTKO_Transparency_Peeling_Layers <> 0 then
      GetSingle; // m_depth_peeling_min_area
  end;

  if vMask[1] and vValue[1] and cnstTKO_Rendo_Cut_Geometry <> 0 then
  begin
    vCutGeometry := GetByte; // m_cut_geometry
    if vCutGeometry and cnstTKO_Cut_Geometry_Level <> 0 then
      GetByte; // m_cut_geometry_level
    if vCutGeometry and cnstTKO_Cut_Geometry_Tolerance <> 0 then
      GetSingle; // m_cut_geometry_tolerance
    if vCutGeometry and cnstTKO_Cut_Geometry_Match_Color <> 0 then
      GetByte; // m_cut_geometry_match
  end;

  if vMask[1] and vValue[1] and cnstTKO_Rendo_Depth_Range <> 0 then
  begin
    GetSingle; // m_depth_range[2]
    GetSingle;
  end;
  if vMask[1] and vValue[1] and cnstTKO_Rendo_Mask_Transform <> 0 then
  begin
    GetByte; // m_mask_transform
  end;
  if vMask[1] and vValue[1] and cnstTKO_Rendo_Image_Scale <> 0 then
  begin
    GetSingle; // m_image_scale[2]
    GetSingle;
  end;

  if vMask[1] and vValue[1] and cnstTKO_Rendo_Simple_Shadow <> 0 then
  begin
    vSimpleShadow := GetByte;
    if vSimpleShadow and cnstTKO_Simple_Shadow_Extended <> 0 then
      vSimpleShadow := vSimpleShadow or GetByte shl 8;
    if vSimpleShadow and cnstTKO_Simple_Shadow_Plane <> 0 then
    begin
      GetSingle; // m_simple_shadow_plane[4]
      GetSingle;
      GetSingle;
      GetSingle;
    end;
    if vSimpleShadow and cnstTKO_Simple_Shadow_Light_Direction <> 0 then
    begin
      GetSingle; // m_simple_shadow_light[3]
      GetSingle;
      GetSingle;
    end;
    if vSimpleShadow and cnstTKO_Simple_Shadow_Color <> 0 then
    begin
      GetByte; // m_simple_shadow_color[3]
      GetByte;
      GetByte;
    end;
    if vSimpleShadow and cnstTKO_Simple_Shadow_Blur <> 0 then
    begin
      GetByte; // m_simple_shadow_blur
    end;
    if vSimpleShadow and cnstTKO_Simple_Shadow_Resolution <> 0 then
    begin
      GetWord; // m_simple_shadow_resolution
    end;
    if vSimpleShadow and cnstTKO_Simple_Shadow_Opacity <> 0 then
    begin
      GetSingle; // m_simple_shadow_opacity
    end;
  end;

  if vMask[1] and vValue[1] and cnstTKO_Rendo_Shadow_Map <> 0 then
  begin
    vShadowMap := GetByte;
    if vShadowMap and cnstTKO_Shadow_Map_Resolution <> 0 then
      GetWord; // m_shadow_map_resolution
    if vShadowMap and cnstTKO_Shadow_Map_Samples <> 0 then
      GetByte; // m_shadow_map_samples
  end;
  if vMask[1] and vValue[1] and cnstTKO_Rendo_Simple_Reflection <> 0 then
  begin
    vSimpleReflection := GetByte; //m_simple_reflection
    if vSimpleReflection and cnstTKO_Simple_Reflection_Extended <> 0 then
      vSimpleReflection := vSimpleReflection or GetByte shl 8;
    if vSimpleReflection and cnstTKO_Simple_Reflection_Plane <> 0 then
    begin
      GetSingle; // m_simple_reflection_plane[4]
      GetSingle;
      GetSingle;
      GetSingle;
    end;
    if vSimpleReflection and cnstTKO_Simple_Reflection_Opacity <> 0 then
      GetSingle; // m_simple_reflection_opacity
    if vSimpleReflection and cnstTKO_Simple_Reflection_Blur <> 0 then
      GetLong; // m_simple_reflection_blur
    if vSimpleReflection and cnstTKO_Simple_Reflection_Attenuation <> 0 then
      GetSingle; // m_simple_reflection_hither
    if vSimpleReflection and cnstTKO_Simple_Reflection_Attenuation <> 0 then
      GetSingle; // m_simple_reflection_yon
  end;
  // Geometry options
  if vMask[1] and vValue[1] and cnstTKO_Rendo_Geometry_Options <> 0 then
  begin
    vGeometryOptions := GetByte; // m_geometry_options
    if vGeometryOptions and cnstTKO_Geometry_Options_Dihedral <> 0 then
      GetSingle; // m_dihedral
  end;
  // image tint
  if vMask[1] and vValue[1] and cnstTKO_Rendo_Image_Tint <> 0 then
  begin
    GetByte;       // m_image_tint_color
    GetByte;
    GetByte;
  end;
  if (vMask[1] and vValue[1] and cnstTKO_Rendo_General_Displacement <> 0) and
    (FVersionW3D >= 1335) then
  begin
    GetLong;       // m_general_displacement
  end
  else
    vMask[1] := vMask[1] and (not cnstTKO_Rendo_General_Displacement);

  if (vMask[1] and vValue[1] and cnstTKO_Rendo_Join_Cutoff_Angle <> 0) and
    (FVersionW3D >= 1405) then
  begin
    GetLong;       // m_join_cutoff_angle
  end
  else
    vMask[1] := vMask[1] and (not cnstTKO_Rendo_Join_Cutoff_Angle);

  if vMask[1] and vValue[1] and cnstTKO_Rendo_Screen_Range <> 0 then
  begin
    GetSingle;       // m_screen_range[4]
    GetSingle;
    GetSingle;
    GetSingle;
  end;
  // m_display_list_level = TKO_Display_List_Level_Entity;

  if vMask[0] and vValue[0] and cnstTKO_Rendo_Display_Lists <> 0 then
  begin
    if FVersionW3D >= 1515 then
      GetByte;       // m_display_list_level
  end;
end;

procedure Tw3dParser.ResdOptionals(ASuboptions: Byte; ASuboptions2: Integer; ACountPoint: Integer;
  vBufferFase: TsgIntegerList; vPoly: TsgDXFPolyline);
var
  I: Integer;
  vCount: Integer;
  vCompressionScheme: Byte;
  vBitsPerValue: Byte;
  vCode: Byte;
  vParamwidth: Byte;
  vNormalCount: Integer;
  vWorkspaceUsed: Integer;
  vExists: TsgIntegerList;
  function GetByNormalsCompressed(AConditionBalue: Integer): Integer;
  begin
    if AConditionBalue < 256 then
      Result := GetByte
    else
      if AConditionBalue < 65536 then
        Result := GetWord
      else
        Result := GetLong;
  end;

begin
  if ASuboptions and cnstTKSH_HAS_OPTIONALS <> 0 then
  begin
    while True do
    begin
      vCode := GetByte;
      case vCode of
        cnstOPT_ALL_NORMALS_COMPRESSED:
          begin
            //byte operation, byte compression_scheme, byte bits_per_sample,
            //array of compressed floats data (see below)
            vCompressionScheme := GetByte;
            vBitsPerValue := GetByte;
            if  FVersionW3D >= 650 then
              vCount := GetLong
            else
              if vCompressionScheme = cnstCS_TRIVIAL then
                  vCount := 3 * ACountPoint
              else
                  vCount := (ACountPoint * vBitsPerValue + 7) div 8;
            for I := 1 to vCount do
            begin
              GetByte;
            end;
          end;
        cnstOPT_NORMALS_COMPRESSED:
          begin
            GetByte;//vCompressionScheme := GetByte;
            GetByte;//vBitsPerValue := GetByte;
            vNormalCount := GetByNormalsCompressed(ACountPoint);
            vExists := TsgIntegerList.Create;
            vExists.Count := ACountPoint;
            vExists.FillChar(0);
            try
              I := 0;
              while I < vNormalCount do
              begin
                GetByNormalsCompressed(ACountPoint);//vIndex := GetByNormalsCompressed(ACountPoint);
                vExists[I] := vExists[I] or cnstVertex_Normal;
                Inc(I);
              end;
              if FVersionW3D >= 650 then
              begin
                //the data length
                vWorkspaceUsed := GetLong;
                //the raw normals data
                for I := 1 to vWorkspaceUsed do
                begin
                  GetByte;
                end;
              end
              else
              begin
                //vWorkspaceUsed := ACountPoint * 3;
                I := 0;
                while I < ACountPoint do
                begin
                  if vExists[I] and cnstVertex_Normal <> 0 then
                  begin
                    GetByte;
                    GetByte;
                    GetByte;
                  end;
                  Inc(I);
                end;
              end;
            finally
              vExists.Free;
            end;
          end;
        cnstOPT_ALL_FACE_COLORS:
          begin
            if FVersionW3D > 650 then
            begin
              GetByte;  //CompressionScheme
              GetByte;//vBitsPerValue := GetByte;
              vCount := GetLong;
              for I := 1 to vCount do
              begin
                GetByte;
              end;
            end
            else
            begin
            end;
          end;
        cnstOPT_ALL_EDGE_VISIBILITIES:
          begin
            GetByte;  //CompressionScheme
            vCount := EnumerateEdges(vBufferFase,ASuboptions);
            for I := 1 to vCount do
            begin
              GetByte;
            end;
          end;
        cnstOPT_ALL_PARAMETERS,
        cnstOPT_ALL_PARAMETERS_COMPRESSED:
          begin
            if vCode = cnstOPT_ALL_PARAMETERS then
            begin
              vParamwidth := GetByte;
              for I := 1 to ACountPoint*vParamwidth do
              begin
                GetSingle;
              end;
            end
            else
              if FVersionW3D >= 650 then
              begin
                GetByte;//vCompressionScheme := GetByte;
                if FVersionW3D > 1175 then
                  vParamwidth := GetByte
                else
                  vParamwidth := 3;
                for I := 1 to vParamwidth*2 do
                begin
                  GetSingle;
                end;
                GetByte;//vBitsPerValue := GetByte;
                vWorkspaceUsed := GetLong;
                for I := 1 to vWorkspaceUsed do
                begin
                  GetByte;
                end;
              end
              else
              begin
                vParamwidth := 3;
                GetByte;//vCompressionScheme := GetByte;
                GetByte;//vBitsPerValue := GetByte;
                for I := 1 to vParamwidth*2 do
                begin
                  GetSingle;
                end;
                vWorkspaceUsed := vParamwidth*ACountPoint;
                for I := 1 to vWorkspaceUsed do
                begin
                  GetByte;
                end;
              end;
          end;
        cnstOPT_TERMINATE:
          begin
            //OPT_TERMINATE
            Exit;
          end;
      //else
         // raw data - Error
      end;
    end;
  end;
end;

procedure Tw3dParser.ShellFormat1_Standard(ASuboptions: Byte; ASuboptions2: Integer);

//byte suboptions, [short suboptions2], [int index], byte level_of_detail,
// variable points, variable faces, [variable attributes]
var
  vPoly: TsgDXFPolyline;
  vVertex: TsgDXFVertex;
  I, J, vCount: Integer;
  vCountPoint: Integer;

  {vCompressionScheme: Byte;}
  vFaceListLength: Integer;
  vBitsPerValue: Byte;
  vBufferFase: TsgIntegerList;

  function GetIndex: Integer;
  begin
    Result := 0;
    case vBitsPerValue of
      8: Result := GetByte;
      16: Result := GetWord;
    end;
    Inc(vCount, 16 div vBitsPerValue);
  end;
begin
  vPoly := nil;
  vCountPoint := 0;
  if ASuboptions and cnstTKSH_FIRSTPASS = 0 then
  begin
    GetLong; // Index
  end;
  GetByte;  // LevelOfDetail
  if ASuboptions and cnstTKSH_COMPRESSED_POINTS = 0 then
  begin
    vCount := GetLong;
    vCountPoint := vCount;
    vPoly := TsgDXFPolyline(NewEntity(TsgDXFPolyline));
    vPoly.Flags := 64;
    while vCount > 0 do
    begin
      vVertex := TsgDXFVertex.Create;
      vVertex.Point := GetPoint;
      vPoly.AddEntity(vVertex);
      Dec(vCount);
    end;
  end
  else
  begin
  // Compressed point
  end;

  vBufferFase := TsgIntegerList.Create;
  try
    if ASuboptions and cnstTKSH_COMPRESSED_FACES <> 0 then
    begin
      // Compressed Faces
      GetByte; // CompressionScheme
      vFaceListLength := GetLong;

      vCount := 0;
      while vCount < vFaceListLength do
      begin
        vBufferFase.Add(GetByte);
        Inc(vCount);
      end;
      TrivialDecompressFaces(vBufferFase,ASuboptions2 and cnstTKSH2_HAS_NEGATIVE_FACES);
      vCount := 0;
      while vCount < vBufferFase.Count do
      begin
        I := vBufferFase[vCount];
        Inc(vCount);
        vVertex := TsgDXFVertex.Create;
        vVertex.Flags := 128;
        for J := 1 to I do
        begin
          if J <= 4 then
            vVertex.PolyFaceVertexIndex[J] := vBufferFase[vCount]+1;
          Inc(vCount);
        end;
        if I = 3 then
          vVertex.PolyFaceVertexIndex[4] := vVertex.PolyFaceVertexIndex[3];
        if vPoly <> nil then
          vPoly.AddEntity(vVertex);
      end;
    end;

    ResdOptionals(ASuboptions, ASuboptions2, vCountPoint, vBufferFase, vPoly);
  finally
    vBufferFase.Free;
  end;
end;

function Swap32(AIndex: Integer): Integer;
begin
  Result := (AIndex shr 24 and $000000ff) or
   ( (AIndex and $00ff0000) shr 8) or
   ( (AIndex and $0000ff00) shl 8) or
   ( AIndex shl 24);
end;

function ToNext4(AIndex: Integer): Integer;
begin
  Result := (3 - (AIndex - 1) mod 4)
end;

procedure MTableInfoInit(MTable: PsgMTableInfo);
begin
    MTable.flags := 0;
    MTable.mlengths_used := 0;
    MTable.mlengths_allocated := 0;
    MTable.mlengths :=  nil;
    MTable.m2stackoffsets_used := 0;
    MTable.m2stackoffsets_allocated := 0;
    MTable.m2stackoffsets :=  nil;
    MTable.m2gateoffsets_used := 0;
    MTable.m2gateoffsets_allocated := 0;
    MTable.m2gateoffsets := nil;
    MTable.dummies_used := 0;
    MTable.dummies_allocated := 0;
    MTable.dummies := nil;
    MTable.patches_used := 0;
    MTable.patches_allocated := 0;
    MTable.patches := nil;
    MTable.bounding := nil;
    MTable.x_quantization := cnstDEFAULT_QUANTIZATION;
    MTable.y_quantization := cnstDEFAULT_QUANTIZATION;
    MTable.z_quantization := cnstDEFAULT_QUANTIZATION;
    MTable.x_quantization_normals := cnstDEFAULT_QUANTIZATION;
    MTable.y_quantization_normals := cnstDEFAULT_QUANTIZATION;
    MTable.z_quantization_normals := cnstDEFAULT_QUANTIZATION;
end;


procedure MTableInfoFree(MTable: PsgMTableInfo);
begin
    if MTable.mlengths <> nil then
    begin
        FreeMem(MTable.mlengths);
        MTable.mlengths := nil;
    end;

    if MTable.m2stackoffsets <> nil then
    begin
        FreeMem(MTable.m2stackoffsets);
        MTable.mlengths := nil;
    end;

    if MTable.m2gateoffsets <> nil then
    begin
        FreeMem(MTable.m2gateoffsets);
        MTable.mlengths := nil;
    end;


    if MTable.dummies <> nil then
    begin
        FreeMem(MTable.dummies);
        MTable.mlengths := nil;
    end;

    if MTable.patches <> nil then
    begin
        FreeMem(MTable.patches);
        MTable.mlengths := nil;
    end;

    if MTable.bounding <> nil then
    begin
        FreeMem(MTable.bounding);
        MTable.mlengths := nil;
    end;
end;



procedure Tw3dParser.UnpackMTable(AMData: PByte;
  MTable: PsgMTableInfo; var AProxyHash: TsgCollection);
var
  vIndex, vTemp: PInteger;
  I, vPrevDummy, vPrevPatch, vIntegerToSingle: Integer;
  function GetInteger(var AIndex: PInteger): Integer;
  begin
    Result := AIndex^;
    Inc(AIndex);
  end;

begin
  vIndex := PInteger(AMData);
  MTable.flags := GetInteger(vIndex);
  if MTable.flags and cnstMTABLE_HAS_LENGTHS <> 0 then
  begin
//        m->mlengths_used = swap32(*in++);
//        m->mlengths_allocated = m->mlengths_used;
//        m->mlengths = (int *) EA_MALLOC (m->mlengths_allocated * sizeof( int ) );
  end
  else
  begin
    MTable.mlengths_used := 0;
    MTable.mlengths_allocated := 0;
    MTable.mlengths := nil;
  end;

  if MTable.flags and cnstMTABLE_HAS_M2STACKOFFSETS <> 0 then
  begin
//        ASSERT( m->flags & MTABLE_HAS_M2GATEOFFSETS );
//        m->m2stackoffsets_used = swap32(*in++);
//        m->m2stackoffsets_allocated = m->m2stackoffsets_used;
//        m->m2stackoffsets = (int *) EA_MALLOC (m->m2stackoffsets_allocated * sizeof( int ) );
//        m->m2gateoffsets_used = m->m2stackoffsets_used;
//        m->m2gateoffsets_allocated = m->m2gateoffsets_used;
//        m->m2gateoffsets = (int *) EA_MALLOC (m->m2gateoffsets_allocated * sizeof( int ) );
  end
  else
  begin
    MTable.m2stackoffsets_used := 0;
    MTable.m2stackoffsets_allocated := 0;
    MTable.m2gateoffsets_used := 0;
    MTable.m2gateoffsets_allocated := 0;
    MTable.m2stackoffsets := nil;
    MTable.m2gateoffsets := nil;
  end;

  if MTable.flags and cnstMTABLE_HAS_DUMMIES <> 0  then
  begin
    MTable.dummies_used := GetInteger(vIndex);
    MTable.dummies_allocated := MTable.dummies_used;
    MTable.dummies := AllocMem(MTable.dummies_allocated*SizeOf(Integer));
  end
  else
  begin
    MTable.dummies_used := 0;
    MTable.dummies_allocated := 0;
    MTable.dummies := nil;
  end;

  if MTable.flags and cnstMTABLE_HAS_PATCHES <> 0  then
  begin
        MTable.patches_used := GetInteger(vIndex);
        MTable.patches_allocated := MTable.patches_used;
        MTable.patches := AllocMem(MTable.patches_allocated*SizeOf(Integer));
        AProxyHash := TsgCollection.Create;//        proxy_hash = new_vhash( m->patches_used + 2, malloc, free );
  end
  else
  begin
    MTable.patches_used := 0;
    MTable.patches_allocated := 0;
    MTable.patches := nil;
  end;
  vTemp := MTable.mlengths;
  for I :=0 to MTable.mlengths_used - 1 do
  begin
    vTemp^ := GetInteger(vIndex);
    Inc(vTemp);
  end;
  vTemp := MTable.m2stackoffsets;
  for I :=0 to MTable.m2stackoffsets_used - 1 do
  begin
    vTemp^ := GetInteger(vIndex);
    Inc(vTemp);
  end;
  vTemp := MTable.m2gateoffsets;
  for I :=0 to MTable.m2gateoffsets_used - 1 do
  begin
    vTemp^ := GetInteger(vIndex);
    Inc(vTemp);
  end;

  vPrevDummy := 0;
  vTemp := MTable.dummies;
  for I :=0 to MTable.dummies_used - 1 do
  begin
    vTemp^ := GetInteger(vIndex) + vPrevDummy;
    vPrevDummy := vTemp^;
    Inc(vTemp);
  end;

//    ASSERT( (m->patches_used % 2) == 0 );
  vPrevPatch := 0;
  vTemp := MTable.patches;
  I := 0;
  while I < MTable.patches_used do
  begin
    PsgIntegerArray(vTemp)[I] := GetInteger(vIndex) + vPrevPatch;
    vPrevPatch := PsgIntegerArray(vTemp)[I];
    PsgIntegerArray(vTemp)[I+1] := GetInteger(vIndex);
    AProxyHash.Add(UInt64(PsgIntegerArray(vTemp)[I]),Pointer(PsgIntegerArray(vTemp)[I+1]));
    Inc(I,2);
  end;

  if MTable.flags and cnstMTABLE_HAS_BOUNDING <> 0 then
  begin
    MTable.bounding := PsgEtBounding(AllocMem(SizeOf(TsgEtBounding)));
    vIntegerToSingle := GetInteger(vIndex);
    MTable.bounding^.Point1.X := PSingle(@vIntegerToSingle)^;
    vIntegerToSingle := GetInteger(vIndex);
    MTable.bounding^.Point1.Y := PSingle(@vIntegerToSingle)^;
    vIntegerToSingle := GetInteger(vIndex);
    MTable.bounding^.Point1.Z := PSingle(@vIntegerToSingle)^;
    vIntegerToSingle := GetInteger(vIndex);
    MTable.bounding^.Point2.X := PSingle(@vIntegerToSingle)^;
    vIntegerToSingle := GetInteger(vIndex);
    MTable.bounding^.Point2.Y := PSingle(@vIntegerToSingle)^;
    vIntegerToSingle := GetInteger(vIndex);
    MTable.bounding^.Point2.Z := PSingle(@vIntegerToSingle)^;
  end
  else
    MTable.bounding := nil;

  if MTable.flags and cnstMTABLE_HAS_QUANTIZATION <> 0 then
  begin
    MTable.x_quantization := GetInteger(vIndex);
    MTable.y_quantization := GetInteger(vIndex);
    MTable.z_quantization := GetInteger(vIndex);
  end
  else
  begin
    MTable.x_quantization := cnstDEFAULT_QUANTIZATION;
    MTable.y_quantization := cnstDEFAULT_QUANTIZATION;
    MTable.z_quantization := cnstDEFAULT_QUANTIZATION;
  end;

  if MTable.flags and cnstMTABLE_HAS_QUANTIZATION_NORMALS <> 0 then
  begin
    MTable.x_quantization_normals := GetInteger(vIndex);
    MTable.y_quantization_normals := GetInteger(vIndex);
    MTable.z_quantization_normals := GetInteger(vIndex);
  end
  else
  begin
    MTable.x_quantization_normals := cnstDEFAULT_QUANTIZATION;
    MTable.y_quantization_normals := cnstDEFAULT_QUANTIZATION;
    MTable.z_quantization_normals := cnstDEFAULT_QUANTIZATION;
  end;

//    *proxy_hash_out = proxy_hash;
end;



function sgHalfEdgeArrayInit( AEa: PsgHalfEdgeArray; AInitialSize: Integer): Integer;
var
  I: Integer;
begin
  AEa.allocated := AInitialSize;
  AEa.edges := AllocMem(AEa.allocated * SizeOf(TsgHalfEdge));
  AEa.used := 0;
  for I := 0 to AEa.allocated - 1 do
  begin
    PsgHEArray(AEa.edges)[I].start := GARBAGE_EDGE;
    PsgHEArray(AEa.edges)[I].twin := GARBAGE_EDGE;
  end;
  AEa.visitations_used := 0;
  AEa.visitations := nil;
  Result := 1;
end;

procedure sgHalfEdgeArrayFree(AEa: PsgHalfEdgeArray);
begin
  if AEa.allocated <> 0 then
  begin
    FreeMem(AEa.edges);
    AEa.edges := nil;
    AEa.allocated := 0;
  end;
  if AEa.visitations <> nil then
  begin
    FreeMem(AEa.visitations);
    AEa.visitations := nil;
    AEa.visitations_used := 0;
  end;
end;

function Tw3dParser.DecompressPreprocess(AOpslen: Integer; AOps: PByte;
  MTable: PsgMTableInfo; offsets: TsgIntegerList; var ecount_out: Integer): Integer;
var
  i, j, s, length, stackoffset: Integer;
  ecount, scount, mcount, m2count: Integer;
  estack, sstack: TsgEasyStack;
  vTempByteArray: PsgByteArray;
  procedure SetByIndex(AOffsets: TsgIntegerList; AIndex, AValue: Integer);
  begin
    if AIndex >= AOffsets.Count then
      AOffsets.Count := AIndex+1;
    AOffsets[AIndex] := AValue;
  end;
begin
  Result := 0;
  ecount := 0;
  scount := 0;
  mcount := 0;
  m2count := 0;
  estack := TsgEasyStack.Create;
  sstack := TsgEasyStack.Create;
  try
    i := 0;
    vTempByteArray := PsgByteArray(AOps);
    while (i < AOpslen) and (sstack.used >= 0) do
    begin
      case vTempByteArray[i] of
        CaseC: Dec(ecount);
        CaseL, CaseR: Inc(ecount);
        CaseE:
        begin
         Inc(ecount, 3);
         if estack.used > 0 then
         begin
           s := sstack.Pop;
           SetByIndex(offsets, s, ecount - 2 - estack.Pop);
         end
         else
           Exit;
        end;
        CaseS:
        begin
         Dec(ecount);
         sstack.Push(scount);
         estack.Push(ecount);
         Inc(scount);
        end;
        CaseM:
        begin
         length := PsgIntegerArray(MTable.mlengths)[mcount];
         Inc(mcount);
         Dec(ecount, length + 1);
        end;
        CaseM2:
        begin
        length := PsgIntegerArray(MTable.mlengths)[m2count];
        stackoffset := PsgIntegerArray(MTable.m2stackoffsets)[m2count];
        sstack.PopInternal(stackoffset, s );
        estack.PopInternal(stackoffset, j );
        SetByIndex(offsets, s, length + ecount - 2 - j);
        Inc(m2count);
        Dec(ecount);
        end;
      end;
      Inc(I);
    end;
  finally
    estack.Free;
    sstack.Free;
    ecount_out := ecount;
  end;
  Result := 1;
end;

destructor Tw3dParser.Destroy;
begin
  FStackBlock.Clear;
  FStackBlock.Free;
  FListShadowBlock.Free;
  inherited Destroy;
end;

procedure Tw3dParser.DistantLight;
begin
  GetPoint;
  if FVersionW3D >= 1170 then
    GetByte;
end;

procedure sgExpandHash(AOldSize, ANewSzie: Integer;
  N, P, start, twin: TsgIntegerList);
begin
  N.Count := ANewSzie;
  P.Count := ANewSzie;
  start.Count := ANewSzie;
  twin.Count := ANewSzie;
end;

function Tw3dParser.ProcessOpcodes(AOpslen: Integer; AOps: PByte;
  MTable: PsgMTableInfo; AEa: PsgHalfEdgeArray; vBufferFase: TsgIntegerList;
  var AOpcodePointcount: Integer): Integer;
var
  N, P, start, twin: TsgIntegerList;
  hash_allocated, hash_used: Integer;
  I, J, k: Integer;
  vPointcount: Integer;
  gate_stack, available_edges: TsgEasyStack;
  ecount, scount: Integer;
  offsets: TsgIntegerList;
  vTemp: PByte;
  vCaseTemp: Byte;
  ei, gi, bi, hi: Integer;
  vFacesBlock: Integer;
begin
  Result := 0;
  hash_allocated := 100;
  ecount := 0;
  scount := 0;
  sgHalfEdgeArrayInit( AEa, 3 * AOpslen );
  N := TsgIntegerList.Create(hash_allocated);
  P := TsgIntegerList.Create(hash_allocated);
  start := TsgIntegerList.Create(hash_allocated);
  twin := TsgIntegerList.Create(hash_allocated);
  offsets := TsgIntegerList.Create;
  i := 0;
  vPointcount := 0;
  ei := 0;
  vFacesBlock := 0;
  gate_stack := TsgEasyStack.Create;
  available_edges := TsgEasyStack.Create;

  try

  while i < AOpslen do
  begin
    gate_stack.used := 0;
    available_edges.used := 0;
    vTemp := AOps;
    Inc(vTemp, i);
    DecompressPreprocess( AOpslen - i, vTemp, mtable, offsets, ecount);
    if ecount <= 0 then
      Exit;
    if ecount >= hash_allocated then
    begin
      sgExpandHash( hash_allocated, ecount*2, P, N, start, twin );
      hash_allocated := ecount*2;
    end;
    hash_used := ecount;

    for J := 0 to ecount-1 do
    begin
      P[j] := j - 1;
      N[j] := j + 1;
      start[j] := vPointcount;
      Inc(vPointcount);
      twin[j] := GARBAGE_EDGE;
    end;

    P[0] := ecount - 1;
    N[ecount-1] := 0;
    gi := 0;

    while gate_stack.Used >= 0 do
    begin
      vBufferFase[vFacesBlock] := 3;
      vBufferFase[vFacesBlock+1] := start[gi];
      vBufferFase[vFacesBlock+2] := start[N[gi]];
      if available_edges.used = 0 then
      begin
        if hash_used >= hash_allocated then
        begin
          sgExpandHash(hash_allocated, hash_allocated*2, P, N, start, twin);
          hash_allocated := hash_allocated*2;
        end;
        available_edges.Push(hash_used);
        Inc(hash_used);
      end;
      if twin[gi] <> GARBAGE_EDGE then
        PsgHEArray(AEa.edges)[twin[gi]].twin := ei;
      PsgHEArray(AEa.edges)[ ei ].twin := twin[gi];
      PsgHEArray(AEa.edges)[ ei ].start := start[gi];
      PsgHEArray(AEa.edges)[ ei+1 ].start := start[ N[gi] ];

      vCaseTemp := PsgByteArray(AOps)[i];
      Inc(i);
      case vCaseTemp of
        CaseC:
        begin
          vBufferFase[vFacesBlock+3] := vPointcount;
          PsgHEArray(AEa.edges)[ ei+1 ].twin := GARBAGE_EDGE;
          PsgHEArray(AEa.edges)[ ei+2 ].twin := GARBAGE_EDGE;
          PsgHEArray(AEa.edges)[ ei+2 ].start := vPointcount;
          hi := available_edges.Pop;
          N[ P[gi] ] := hi;
          P[hi] := P[gi];
          N[hi] := gi;
          P[gi] := hi;
          start[hi] := start[gi];
          start[gi] := vPointcount;
          twin[gi] := ei+1;
          twin[hi] := ei+2;
          Inc(vPointcount);
        end;
        CaseL:
        begin
          vBufferFase[vFacesBlock+3] := start[ P[gi] ];
          PsgHEArray(AEa.edges)[ ei+1 ].twin := GARBAGE_EDGE;
          PsgHEArray(AEa.edges)[ ei+2 ].twin := twin[ P[gi] ];
          if twin[ P[gi] ] <> GARBAGE_EDGE then
              PsgHEArray(AEa.edges)[ twin[P[gi]] ].twin := ei+2;
          PsgHEArray(AEa.edges)[ ei+2 ].start := start[ P[gi]];
          start[ gi ] := start[ P[gi] ];
          available_edges.Push(P[gi]);
          P[gi] := P[ P[gi] ];
          N[ P[gi] ] := gi;
          twin[gi] := ei+1;
        end;
        CaseE:
        begin
          vBufferFase[vFacesBlock+3] := start[ P[gi] ];
          PsgHEArray(AEa.edges)[ ei+1 ].twin := twin[ N[gi] ];
          if twin[ N[gi] ] <> GARBAGE_EDGE then
              PsgHEArray(AEa.edges)[ twin[N[gi]] ].twin := ei+1;
          PsgHEArray(AEa.edges)[ ei+2 ].twin := twin[ P[gi] ];
          if twin[ P[gi] ] <> GARBAGE_EDGE then
              PsgHEArray(AEa.edges)[ twin[P[gi]] ].twin := ei+2;
          PsgHEArray(AEa.edges)[ ei+2 ].start := start[ P[gi]];
          available_edges.Push(gi);
          available_edges.Push(P[gi]);
          available_edges.Push(N[gi]);
          if gate_stack.Used > 0 then
              gi := gate_stack.Pop
          else
              gate_stack.used := -1;
        end;
        CaseR:
        begin
          vBufferFase[vFacesBlock+3] := start[ N[N[gi]] ];
          PsgHEArray(AEa.edges)[ ei+1 ].twin := twin[ N[gi] ];
          if twin[ N[gi] ] <> GARBAGE_EDGE then
              PsgHEArray(AEa.edges)[ twin[N[gi]] ].twin := ei+1;
          PsgHEArray(AEa.edges)[ ei+2 ].twin := GARBAGE_EDGE;
          PsgHEArray(AEa.edges)[ ei+2 ].start := start[ N[N[gi]] ];
          available_edges.Push(N[gi]);
          N[gi] := N[N[gi]];
          P[N[gi]] := gi;
          twin[gi] := ei+2;
        end;
        CaseS:
        begin
          bi := gi;
          for K := 0 to offsets[ scount ] do
            bi := N[bi];
          vBufferFase[vFacesBlock+3] := start[N[bi]];
          PsgHEArray(AEa.edges)[ ei+1 ].twin := GARBAGE_EDGE;
          PsgHEArray(AEa.edges)[ ei+2 ].twin := GARBAGE_EDGE;
          PsgHEArray(AEa.edges)[ ei+2 ].start := start[N[bi]];
          Inc(scount);

          hi := available_edges.Pop;
          gate_stack.Push(hi);
          N[P[gi]] := hi;
          P[hi] := P[gi];
          N[hi] := N[bi];
          P[N[bi]] := hi;
          twin[hi] := ei+2;
          start[hi] := start[gi];
          start[gi] := start[N[bi]];
          twin[gi] := ei+1;
          P[gi] := bi;
          N[bi] := gi;
        end;
        CaseM: begin end;
        CaseM2: begin end;
      end;
      if (vBufferFase[vFacesBlock+1] = vBufferFase[vFacesBlock+2]) or
         (vBufferFase[vFacesBlock+3] = vBufferFase[vFacesBlock+1]) or
         (vBufferFase[vFacesBlock+3] = vBufferFase[vFacesBlock+1]) then
        Exit;
      Inc(vFacesBlock,4);
//            ASSERT( validate_edge( ea, ea->edges + ei, NULL ) );
//            ASSERT( validate_edge( ea, ea->edges + ei + 1, NULL ) );
//            ASSERT( validate_edge( ea, ea->edges + ei + 2, NULL ) );
      if gate_stack.used >= 0 then
      begin
//                ASSERT( validate_loop( gi, N, P ) );
//
//                ASSERT( i % 20 ||
//                        loop_contains_no_freed_edges( gi, N, &available_edges ) );
      end;
      Inc(AEa.used, 3);
      Inc(ei, 3);
    end;
    scount := 0;
  end;
  finally
    gate_stack.Free;
    available_edges.Free;
    N.Free;
    P.Free;
    start.Free;
    twin.Free;
    offsets.Free;
    AOpcodePointcount := vPointcount;
  end;
end;



function HNEXT(I: Integer): Integer;
begin
  Result := 3*(I div 3) + ((I+1) mod 3);
end;

function HPREV(I: Integer): Integer;
begin
  Result := 3*(I div 3) + ((I+2) mod 3);
end;

function Tw3dParser.PatchFaces( AOpcodePointCount: Integer; MTable: PsgMTableInfo;
  AProxyHash: TsgCollection; vBufferFase: TsgIntegerList;
  AEa: PsgHalfEdgeArray; associations: TsgIntegerList): Integer;
var
  vPointMap: TsgIntegerList;
  vIndexProxy: Integer;
  I: Integer;
  shift, flen, flen_temp: Integer;
  next, prev: Integer;
  //pcount: Integer; //For future versions
begin
  vPointMap := TsgIntegerList.Create;
  try
    vPointMap.Count := AOpcodePointCount;
    vPointMap.FillChar(0);
    for I := 0 to MTable.dummies_used - 1 do
      vPointMap[ PsgIntegerArray(MTable.dummies)[I]] := DUMMY;
    I := 0;
    while I < MTable.patches_used do
    begin
      vPointMap[ PsgIntegerArray(MTable.patches)[I]] := ALIAS;
      Inc(I,2);
    end;
    shift := 0;
    for I := 0 to AOpcodePointCount - 1 do
    begin
      if vPointMap[I] < 0 then
        Inc(shift)
      else
        vPointMap[I] := shift;
    end;
    //pcount := AOpcodePointCount - shift; //For future versions
    flen_temp := vBufferFase.Count;
    flen := vBufferFase.Count;
    I := 0;
    shift := 0;
    while I < flen_temp do
    begin
      if (vPointMap[vBufferFase[I+1]] = DUMMY) or
         (vPointMap[vBufferFase[I+2]] = DUMMY) or
         (vPointMap[vBufferFase[I+3]] = DUMMY) then
      begin
        Inc(shift, 4);
        Dec(flen, 4);
      end
      else
      begin
        vBufferFase[I+1 - shift] := vBufferFase[I+1];
        vBufferFase[I+2 - shift] := vBufferFase[I+2];
        vBufferFase[I+3 - shift] := vBufferFase[I+3];
      end;
      Inc(I,4);
    end;

    for I := 0 to flen - 1 do
    begin
      if (I and $3) = 0 then
        Continue;
      if vPointMap[vBufferFase[I]] = ALIAS then
      begin
        vIndexProxy := AProxyHash.IndexOf(vBufferFase[I]);
        if vIndexProxy >= 0 then
        begin
          vBufferFase[I] := Integer(AProxyHash.Items[vIndexProxy].Data);
        end;
      end
      else
        vBufferFase[I] := vBufferFase[I] - vPointMap[vBufferFase[I]];
    end;

    if associations <> nil then
    begin
    //        shift = 0;
    //        for( i = 0 ; i < opcode_pointcount ; i++ ) {
    //            if( pointmap[ i ] == DUMMY ) {
    //                shift += 3;
    //            }
    //            else {
    //                associations[ i*3 + 0 - shift ] = associations[ i*3 + 0 ];
    //                associations[ i*3 + 1 - shift ] = associations[ i*3 + 1 ];
    //                associations[ i*3 + 2 - shift ] = associations[ i*3 + 2 ];
    //            }
    //        }
    //
    //        ptr = associations;
    //        for( i = 0 ; i < opcode_pointcount * 3 ; i++ ) {
    //
    //            ASSERT( i%3 || validate_associations( ptr, opcode_pointcount ) );
    //            ASSERT( ptr[0] < opcode_pointcount );
    //            if( ptr[0] != GARBAGE_VERTEX ) {
    //                if( pointmap[ ptr[0] ] == DUMMY )  {
    //                    ptr[0] = GARBAGE_VERTEX;
    //                }
    //                else if( pointmap[ ptr[0] ] == ALIAS ) {
    //                    vhash_result = vhash_lookup_item( proxy_hash, I2V(ptr[0]), &item );
    //                    ASSERT( vhash_result == VHASH_STATUS_SUCCESS );
    //                    ptr[0] = V2I(item);
    //                    ASSERT( ptr[0] >= 0 );
    //                }
    //                else {
    //                    ptr[0] -= pointmap[ ptr[0] ];
    //                }
    //                ASSERT( ptr[0] < i );
    //            }
    //            ptr++;
    //        }
    end;

    if AEa <> nil then
    begin
      I := 0;
      while i < AEa.used do
      begin
        if vPointMap[PsgHEArray(AEa.edges)[I].start] = ALIAS then
        begin
          vIndexProxy := AProxyHash.IndexOf(PsgHEArray(AEa.edges)[I].start);
          if vIndexProxy >= 0 then
            PsgHEArray(AEa.edges)[I].start := Integer(AProxyHash.Items[vIndexProxy].Data);
        end
        else
        begin
          if vPointMap[PsgHEArray(AEa.edges)[I].start] = DUMMY then
          begin
            PsgHEArray(AEa.edges)[I].start := DUMMY_VERTEX;
            next := HNEXT(i);
            prev := HPREV(i);
            if PsgHEArray(AEa.edges)[next].twin <> GARBAGE_EDGE then
              PsgHEArray(AEa.edges)[PsgHEArray(AEa.edges)[next].twin].twin := GARBAGE_EDGE;
            PsgHEArray(AEa.edges)[next].start := DUMMY_VERTEX;
            PsgHEArray(AEa.edges)[prev].start := DUMMY_VERTEX;
            PsgHEArray(AEa.edges)[I].twin := GARBAGE_EDGE;
            PsgHEArray(AEa.edges)[next].twin := GARBAGE_EDGE;
            PsgHEArray(AEa.edges)[prev].twin := GARBAGE_EDGE;
            while next = (I+1) do
            begin
              Inc(I);
              next := HNEXT(i);
            end;
          end
          else
          begin
            PsgHEArray(AEa.edges)[I].start := PsgHEArray(AEa.edges)[I].start -
              vPointMap[PsgHEArray(AEa.edges)[I].start];
          end;
        end;
        Inc(I);
      end;
    end;

  finally
    vPointMap.Free;
  end;
  vBufferFase.Count := flen;
  Result := 1;
end;

function lookup_vertex(proxy_hash: TsgCollection; v: Integer): Integer;
var
  vIndexProxy: Integer;
begin
  if v = DUMMY_VERTEX then
  begin
    Result := DUMMY_VERTEX;
    Exit;
  end;
  vIndexProxy := proxy_hash.IndexOf(v);
  Result := Integer(proxy_hash.Items[vIndexProxy].Data);
end;

function ProxyVertexIndex(proxy_hash: TsgCollection; v: Integer): Integer;
begin
  if (DWORD(v) and $80000000) <> 0then
    Result := lookup_vertex((proxy_hash),(v))
  else
    Result := v;
end;

procedure predict(AEa: PsgHalfEdgeArray; ei: Integer; AProxyHash: TsgCollection;
  quantized_points: TsgIntegerList; var prediction_out: array of Integer);
var
  a, b, c: Integer;
  ai, bi, ci, twin: Integer;
begin
  ai := PsgHEArray(AEa.edges)[ei].start;
  a := 3 * ProxyVertexIndex( AProxyHash, ai );
  bi := PsgHEArray(AEa.edges)[HNEXT(ei)].start;
  b := 3 * ProxyVertexIndex( AProxyHash, bi );
  twin := PsgHEArray(AEa.edges)[ei].twin and ( not $80000000);
  ci := PsgHEArray(AEa.edges)[HPREV(twin)].start;
  c := 3 * ProxyVertexIndex( AProxyHash, ci );
  prediction_out[0] := quantized_points[a] + quantized_points[b] - quantized_points[c];
  prediction_out[1] := quantized_points[a+1] + quantized_points[b+1] - quantized_points[c+1];
  prediction_out[2] := quantized_points[a+2] + quantized_points[b+2] - quantized_points[c+2];
end;

function Tw3dParser.UnpackPointsAndNormals(AEdgebreakerHeader: PsgEdgebreakerHeader;
  AEa: PsgHalfEdgeArray; pcount: Integer; AProxyHash: TsgCollection; len: Integer;
  pdata: PByte; quantized_points, quantized_normals, tristrips: TsgIntegerList): Integer;
var
  next_tristrip_ei: Integer;
  //num_tristrips,next_tristrip_id, tristrip_total_length: Integer; //For future versions
  touched: PsgByteArray;
  regular_edges, discontinuous_edges: TsgEasyStack;
  vs: TsgVarStream;
  I, code, ei, k, v, v2, v3, c: Integer;
  by_tristrips : Boolean;
  diffs_count, coords_count, next_diff: Integer;
  coords, diffs, normals, ndiffs: TsgIntegerList;
  //eav: Integer;  //For future versions
  proxyv: Integer;
  p: array[0..2] of Integer;
  procedure InitialTwoList(vs: TsgVarStream;
    AList1, Alist2: TsgIntegerList; ACount1, ACount2: Integer);
  var
    I: Integer;
  begin
    AList1.Count := 3 * ACount1;
    for I := 0 to ACount1 - 1 do
    begin
      AList1[I*3+0] := vs.Get(bits2);
      AList1[I*3+1] := vs.Get(bits2);
      AList1[I*3+2] := vs.Get(bits2);
    end;
    Alist2.Count := 3 * ACount2;
    for I := 0 to ACount2 - 1 do
    begin
      Alist2[I*3+0] := vs.Get(bits);
      Alist2[I*3+1] := vs.Get(bits);
      Alist2[I*3+2] := vs.Get(bits);
    end;
  end;

begin
  next_tristrip_ei := -1;
  //next_tristrip_id := -1; //For future versions
  //num_tristrips := -1; //For future versions
  //tristrip_total_length := 0; //For future versions

  next_diff := 0;
  coords := nil;
  diffs := nil;
  normals := nil;
  ndiffs := nil;

  touched := PsgByteArray(AllocMem(pcount*SizeOf(Byte)));
  FillChar(touched^, pcount*SizeOf(Byte), 0);
  regular_edges := TsgEasyStack.Create;
  discontinuous_edges := TsgEasyStack.Create;
  vs := TsgVarStream.Create(len, pdata);
  try
    code := vs.Get(bits);
    by_tristrips := code <> 0;
    coords_count := vs.Get(bits2);
    diffs_count := vs.Get(bits2);
    if AEdgebreakerHeader.points_scheme > 0 then
    begin
      coords := TsgIntegerList.Create;
      diffs := TsgIntegerList.Create;
      InitialTwoList(vs, coords, diffs, coords_count, diffs_count)
    end;
    if AEdgebreakerHeader.normals_scheme > 0 then
    begin
      normals := TsgIntegerList.Create;
      ndiffs := TsgIntegerList.Create;
      InitialTwoList(vs, normals, ndiffs, coords_count, diffs_count)
    end;
    if by_tristrips then
    begin
  //    {
  //        if( tristrips_out != NULL ) {
  //            num_tristrips = vsget( &vs, bits2 );
  //            ASSERT( num_tristrips > 0 );
  //            tristrip_starts = (int *) EA_MALLOC( num_tristrips * sizeof( int ) );
  //            tristrip_lengths = (int *) EA_MALLOC( num_tristrips * sizeof( int ) );
  //            for( i = 0 ; i < num_tristrips ; i++ ) {
  //                tristrip_starts[i] = vsget( &vs, bits2 );
  //                if( i > 0 )
  //                    tristrip_starts[i] += tristrip_starts[i-1];
  //            }
  //            tristrip_total_length = 0;
  //            for( i = 0 ; i < num_tristrips ; i++ ) {
  //                code = vsget( &vs, bits );
  //                ASSERT( code >= 0 );
  //                tristrip_lengths[i] = code + 3;
  //                tristrip_total_length += tristrip_lengths[i] + 1;
  //            }
  //            tristrips = (int *) EA_MALLOC( tristrip_total_length * sizeof( int ) );
  //            tsptr = tristrips;
  //            next_tristrip_ei = tristrip_starts[0];
  //            next_tristrip_id = 0;
  //        }
  //        else
  //            by_tristrips = false;
  //    }
    end;
    if (quantized_points <> nil) or (quantized_normals <> nil) then
    begin
      c := 0;
      for I := 0 to AEa.used -1 do
      begin
        ei := I;
        if ei = next_tristrip_ei then
        begin
  //                ASSERT( by_tristrips );
  //                if( decode_tristrip( ei, tristrip_lengths[next_tristrip_id], ea, proxy_hash, tsptr ) <= 0 ){
  //                    by_tristrips = false;
  //                    next_tristrip_ei = -1;
  //
  //                    COMPLAIN( "tristrip decoding failed.  Falling back to triangles\n" );
  //                }
  //                else {
  //                    ASSERT( tristrip_lengths[next_tristrip_id] > 0 );
  //                    tsptr += tristrip_lengths[next_tristrip_id] + 1;
  //                    next_tristrip_id++;
  //                    if( next_tristrip_id == num_tristrips )
  //                        next_tristrip_ei = -1;
  //                    else
  //                        next_tristrip_ei = tristrip_starts[next_tristrip_id];
  //                }
        end;

        //#define PROXY_VERTEX_INDEX(proxy_hash,v) (VERTEX_SPECIAL(v)?lookup_vertex((proxy_hash),(v)):v)


        v := ProxyVertexIndex(AProxyHash,PsgHEArray(AEa.edges)[ei].start);
        if v = DUMMY_VERTEX then
          Continue;
        if touched[v] <> 0 then
          Continue;

        v2 := ProxyVertexIndex(AProxyHash,PsgHEArray(AEa.edges)[HNEXT(ei)].start);
        v3 := ProxyVertexIndex(AProxyHash,PsgHEArray(AEa.edges)[HPREV(ei)].start);
        if (v2 = DUMMY_VERTEX) or (v3 = DUMMY_VERTEX) then
          Continue;

        for K := 0 to 2 do
        begin
          v := ProxyVertexIndex(AProxyHash,PsgHEArray(AEa.edges)[ei].start);
          if not (touched[v] <> 0) then
          begin
            touched[v] := 1;
            if quantized_points <> nil then
            begin
              quantized_points[v*3] := coords[c*3];
              quantized_points[v*3+1] := coords[c*3+1];
              quantized_points[v*3+2] := coords[c*3+2];
            end;
            if quantized_normals <> nil then
            begin
              quantized_normals[v*3] := normals[c*3];
              quantized_normals[v*3+1] := normals[c*3+1];
              quantized_normals[v*3+2] := normals[c*3+2];
            end;
            Inc(c);
          end;
          if (DWORD(PsgHEArray(AEa.edges)[ei].twin) and $80000000) <> 0 then
          begin

          end
          else
            regular_edges.Push(ei);
          ei := HNEXT(ei);
        end;


        while ((regular_edges.used) or (discontinuous_edges.used)) <> 0 do
        begin
          if regular_edges.used <> 0 then
            ei := regular_edges.Pop
          else
            ei := discontinuous_edges.Pop;
          ei := PsgHEArray(AEa.edges)[ei].twin;
          v := PsgHEArray(AEa.edges)[HPREV(ei)].start;
          if v = DUMMY_VERTEX then
            Continue;
          if (DWORD(v) and $80000000) <> 0 then
          begin
            //eav := v and (not $80000000); //For future versions
            proxyv := ProxyVertexIndex(AProxyHash,v);
          end
          else
          begin
            //eav := v; //For future versions
            proxyv := v;
          end;
          if proxyv = DUMMY_VERTEX then
            Continue;
          if not (touched[proxyv] <> 0) then
          begin
            if quantized_points <> nil then
            begin
              predict(AEa,ei, AProxyHash, quantized_points, p);
              quantized_points[proxyv*3+0] := p[0] - diffs[next_diff*3 + 0];
              quantized_points[proxyv*3+1] := p[1] - diffs[next_diff*3 + 1];
              quantized_points[proxyv*3+2] := p[2] - diffs[next_diff*3 + 2];
            end;
            if quantized_normals <> nil then
            begin
              predict(AEa,ei, AProxyHash, quantized_normals, p);
              quantized_normals[proxyv*3+0] := p[0] - ndiffs[next_diff*3 + 0];
              quantized_normals[proxyv*3+1] := p[1] - ndiffs[next_diff*3 + 1];
              quantized_normals[proxyv*3+2] := p[2] - ndiffs[next_diff*3 + 2];
            end;
            Inc(next_diff);
            ei := HNEXT(ei);
            if (DWORD(PsgHEArray(AEa.edges)[ei].twin) and $80000000) <> 0 then
            begin

            end
            else
              regular_edges.Push(ei);
            ei := HNEXT(ei);
            if (DWORD(PsgHEArray(AEa.edges)[ei].twin) and $80000000) <> 0 then
            begin

            end
            else
              regular_edges.Push(ei);
            touched[proxyv] := 1;
          end;
        end;

      end;
    end
//    else
//    begin
//      if by_tristrips then
  //        if( decode_all_tristrips(
  //                    ea, 0, tristrips,
  //                    num_tristrips, tristrip_starts, tristrip_lengths,
  //                    proxy_hash ) <= 0)
  //            by_tristrips = false;
//    end
    ;
  finally
    regular_edges.Free;
    discontinuous_edges.Free;
    FreeAndNil(coords);
    FreeAndNil(normals);
    FreeAndNil(diffs);
    FreeAndNil(ndiffs);
    vs.Free;
    FreeMem(touched);
  end;
  Result := 1;
end;

function MAXVAL(bits: Integer): Integer;
begin
  Result := ((1 shl(bits))-1);
end;

function UnquantizePoints(pcount: Integer; quantized_points: TsgIntegerList;
  bounding: PsgEtBounding; x_quantization, y_quantization, z_quantization: Integer;
  points_out: TsgSingleList): Integer;
var
  xmaxval, ymaxval, zmaxval: Integer;
  fx_range_inverse, fy_range_inverse, fz_range_inverse: Single;
  I: Integer;
begin
  xmaxval := MAXVAL(x_quantization);
  ymaxval := MAXVAL(y_quantization);
  zmaxval := MAXVAL(z_quantization);

  if bounding.Point2.X = bounding.Point1.X then
    fx_range_inverse := 0
  else
    fx_range_inverse := (bounding.Point2.X - bounding.Point1.X) / xmaxval;
 if bounding.Point2.Y = bounding.Point1.Y then
    fy_range_inverse := 0
  else
    fy_range_inverse := (bounding.Point2.Y - bounding.Point1.Y) / ymaxval;
 if bounding.Point2.Z = bounding.Point1.Z then
    fz_range_inverse := 0
  else
    fz_range_inverse := (bounding.Point2.Z - bounding.Point1.Z) / zmaxval;

  I := 0;
  while i < pcount*3 do
  begin
    if quantized_points[i+0] = xmaxval then
      points_out[i+0] := bounding.Point2.X
    else
      points_out[i+0] := bounding.Point1.X + (fx_range_inverse * quantized_points[I+0]);
    if quantized_points[i+1] = ymaxval then
      points_out[i+1] := bounding.Point2.Y
    else
      points_out[i+1] := bounding.Point1.Y + (fy_range_inverse * quantized_points[I+1]);
    if quantized_points[i+2] = zmaxval then
      points_out[i+2] := bounding.Point2.Z
    else
      points_out[i+2] := bounding.Point1.Z + (fz_range_inverse * quantized_points[I+2]);
    Inc(I,3);
  end;
  Result := 1;
end;

procedure Tw3dParser.EdgebreakerDecompress(AMemory: TMemoryStream;
  APointsList: TsgSingleList; ANormalsList: TsgSingleList;
  vBufferFase: TsgIntegerList; AConfig: PsgEbDecompressConfigs;
  var by_tristrips_out: Boolean);
var
  vEdgebreakerHeader: TsgEdgebreakerHeader;
  vEdgebreakerHeader_In: PsgEdgebreakerHeader;
  vOffset, vOpcodePointcount, pcount: Integer;
  vOps, pdata: PByte;
  vMTable: TsgMTableInfo;
  vMData: PByte;
  {vBounding: PsgEtBounding;}
  vEa: PsgHalfEdgeArray;
  associations, quantized_points, quantized_normals, tristrips: TsgIntegerList;
  by_tristrips: Boolean;
  normal_bbox: TsgEtBounding;
  vProxyHash: TsgCollection;
begin
  quantized_points := nil;
  quantized_normals := nil;
  tristrips := nil;
  associations := nil;
  vEa := nil;
  vProxyHash := nil;
  by_tristrips := False;
  MTableInfoInit(@vMTable);
  try
    vEdgebreakerHeader_In := PsgEdgebreakerHeader(AMemory.Memory);
    vEdgebreakerHeader := vEdgebreakerHeader_In^;
    if vEdgebreakerHeader.scheme < 1 then
      vEdgebreakerHeader.normalslen := 0;
    if vEdgebreakerHeader.opslen = 0 then
      Exit;

    pcount := vEdgebreakerHeader.pcount;
    case vEdgebreakerHeader.scheme of
      0: vOffset := SizeOf(TsgEdgebreakerHeader0);
      1,2: vOffset := SizeOf(TsgEdgebreakerHeader1);
      else
        Exit;
    end;
    vOps := PByte(AMemory.Memory);
    Inc(vOps, vOffset);

    Inc(vOffset,vEdgebreakerHeader.opslen);
    Inc(vOffset,ToNext4(vOffset));
    vMData := PByte(AMemory.Memory);
    Inc(vMData, vOffset);
    UnpackMTable( vMData, @vMTable, vProxyHash);

    if vMTable.bounding <> nil then
    begin {vBounding := vMTable.bounding;} end
    else
    begin
      if (AConfig = nil) or (AConfig.bounding = nil) then
      begin
        if ((vEdgebreakerHeader.scheme >= 2) and (vEdgebreakerHeader.points_scheme > 0)) or
          ((vEdgebreakerHeader.scheme < 2) and (vEdgebreakerHeader.pointslen > 0)) then
        begin
          //if a bounding was passed to compress, it has to be passed to decompress, too!
          Exit;
        end;
        begin {vBounding := nil;} end;
      end
      else
      begin {vBounding := AConfig.bounding;} end;
    end;

    Inc(vOffset, vEdgebreakerHeader.mtablelen);
    Inc(vOffset, ToNext4(vOffset));

    if vEdgebreakerHeader.scheme >= 2 then
    begin
      vEa := AllocMem(SizeOf(TsgHalfEdgeArray));
      ProcessOpcodes(vEdgebreakerHeader.opslen, vOps, @vMTable, vEa,
        vBufferFase, vOpcodePointcount);
    end
    else
    begin
  //        associations = (int *) EA_MALLOC ( (hptr.opslen*3) * 3 * sizeof( int ) );
  //        if( associations == NULL ) {
  //            COMPLAIN( "associations array allocation failed during decompress" );
  //        }
  //        if((status = old_process_opcodes(
  //                hptr.opslen,
  //                ops,
  //                &mtable,
  //                associations,
  //                faces_out,
  //                &opcode_pointcount )) <= 0 )
  //            goto done;
    end;

    if PatchFaces(vOpcodePointcount, @vMTable, vProxyHash, vBufferFase, vEa, associations) <= 0 then
      Exit;

    if (vEdgebreakerHeader.scheme >= 2) and (vEdgebreakerHeader.pointslen > 0) then
    begin
      pdata := PByte(AMemory.Memory);
      Inc(pdata, vOffset);
      if vEdgebreakerHeader.points_scheme > 0 then
        quantized_points := TsgIntegerList.Create(3 * pcount);
      if vEdgebreakerHeader.normals_scheme > 0 then
        quantized_normals := TsgIntegerList.Create(3 * pcount);
      if UnpackPointsAndNormals(@vEdgebreakerHeader, vEa, pcount, vProxyHash, vEdgebreakerHeader.pointslen,
        pdata, quantized_points, quantized_normals, tristrips) <= 0 then
        Exit;
      if tristrips <> nil then
        by_tristrips := True;

      if quantized_points <> nil then
      begin
  //            status = unquantize_points(
  //                pcount, quantized_points, bounding,
  //                mtable.x_quantization, mtable.y_quantization, mtable.z_quantization,
  //                points_out );
  //            if( status <= 0 )
  //                goto done;
      end;

      if quantized_normals <> nil then
      begin
        normal_bbox.Point1 := MakeFPoint(-1, -1, -1);
        normal_bbox.Point2 := MakeFPoint(1, 1, 1);

        if UnquantizePoints( pcount, quantized_normals, @normal_bbox,
                  vMTable.x_quantization_normals, vMTable.y_quantization_normals,
                  vMTable.z_quantization_normals, ANormalsList ) <= 0 then
          Exit;
      end;
      // fix hint
      //Inc(vOffset, vEdgebreakerHeader.pointslen + ToNext4(vEdgebreakerHeader.pointslen));
      //Inc(vOffset, vEdgebreakerHeader.normalslen+ ToNext4(vEdgebreakerHeader.normalslen));
    end
    else
    begin
  //        if( hptr.pointslen > 0 ) {
  //            pdata = ((char *)stream + offset);
  //            ASSERT( hptr.points_scheme <= 1 );
  //            status = old_unpack_points(
  //                    associations, &hptr, pdata, points_out, bounding,
  //                    mtable.x_quantization, mtable.y_quantization, mtable.z_quantization );
  //            if( status <= 0 )
  //                goto done;
  //            offset += hptr.pointslen + TONEXT4(hptr.pointslen);
  //        }
  //        if( hptr.scheme >= 1 &&
  //            hptr.normalslen > 0 ) {
  //            pdata = ((char *)stream + offset);
  //            ASSERT( hptr.normals_scheme == 0 );
  //            status = old_unpack_normals(
  //                    associations, &hptr, pdata, normals_out,
  //                    mtable.x_quantization_normals, mtable.y_quantization_normals, mtable.z_quantization_normals );
  //            if( status <= 0 )
  //                goto done;
  //            offset += hptr.normalslen + TONEXT4(hptr.normalslen);
  //        }
    end;
  finally
    FreeAndNil(associations);
    FreeAndNil(quantized_points);
    FreeAndNil(quantized_normals);
    FreeAndNil(tristrips);
    FreeAndNil(vProxyHash);
    if vEa <> nil then
    begin
      sgHalfEdgeArrayFree(vEa);
      FreeMem(vEa);
    end;
    by_tristrips_out := by_tristrips;
    MTableInfoFree(@vMTable);
  end;

end;

const
  g: Integer = 0;

procedure Tw3dParser.ShellFormat2_Edgebreaker(ASuboptions: Byte; ASuboptions2: Integer);
var
  I, J: Integer;
  {vCompressionScheme, }vByte: Byte;
  vCount, vCountPoint: Integer;
  vMemory: TMemoryStream;
  vEdgebreakerHeader: PsgEdgebreakerHeader;
  vPointsList: TsgSingleList;
  vNormalsList: TsgSingleList;
  vBufferFase: TsgIntegerList;
  by_tristrips: Boolean;
  vPoly: TsgDXFPolyline;
  vVertex: TsgDXFVertex;
begin
  if ASuboptions and cnstTKSH_FIRSTPASS = 0 then
  begin
    GetLong; // Index
  end;
  GetByte;  // LevelOfDetail
  if ASuboptions and (cnstTKSH_COMPRESSED_POINTS or cnstTKSH_CONNECTIVITY_COMPRESSION) <> 0 then
    GetByte;//vCompressionScheme := GetByte;
  vCount := GetLong;

  vPointsList := TsgSingleList.Create;
  vNormalsList := TsgSingleList.Create;
  vBufferFase := TsgIntegerList.Create;
  try
    vMemory := TMemoryStream.Create;
    try
      vMemory.Size := vCount;
      vMemory.Position := 0;
      for I := 1 to vCount do
      begin
        vByte := GetByte;
        vMemory.Write(vByte,1);
      end;
      vEdgebreakerHeader := PsgEdgebreakerHeader(vMemory.Memory);
      vPointsList.Count := vEdgebreakerHeader.pcount*3;
      vBufferFase.Count := vEdgebreakerHeader.opslen*4;
      if (vEdgebreakerHeader.scheme >= 1) and (vEdgebreakerHeader.normalslen <> 0) then
        vNormalsList.Count := vEdgebreakerHeader.pcount*3
      else
        vNormalsList.Count := 0;

      EdgebreakerDecompress(vMemory, vPointsList, vNormalsList, vBufferFase, nil, by_tristrips);

      Inc(g);

      if by_tristrips then
        ASuboptions := ASuboptions or cnstTKSH_TRISTRIPS;
      if ((ASuboptions and cnstTKSH_COMPRESSED_POINTS) <> 0) or (FVersionW3D < 651) then
        Exit;

      vCountPoint := vPointsList.Count div 3;
      vPoly := TsgDXFPolyline(NewEntity(TsgDXFPolyline));
      vPoly.Flags := 64;
      while vCountPoint > 0 do
      begin
        vVertex := TsgDXFVertex.Create;
        vVertex.Point := GetPoint;
        vPoly.AddEntity(vVertex);
        Dec(vCountPoint);
      end;

      vCount := 0;
      while vCount < vBufferFase.Count do
      begin
        I := vBufferFase[vCount];
        Inc(vCount);
        vVertex := TsgDXFVertex.Create;
        vVertex.Flags := 128;
        for J := 1 to I do
        begin
          if J < 4 then
            vVertex.PolyFaceVertexIndex[J] := vBufferFase[vCount]+1;
          Inc(vCount);
        end;
        if I = 3 then
          vVertex.PolyFaceVertexIndex[4] := vVertex.PolyFaceVertexIndex[3];
        if vPoly <> nil then
          vPoly.AddEntity(vVertex);
      end;
    finally
      vMemory.Free;
    end;

    vCountPoint :=  vPointsList.Count div 3;
    ResdOptionals(ASuboptions, ASuboptions2, vCountPoint, vBufferFase, vPoly);

  finally
    vPointsList.Free;
    vNormalsList.Free;
    vBufferFase.Free;
  end;

end;

procedure Tw3dParser.ShellFormat3_BoundingOnly(ASuboptions: Byte; ASuboptions2: Integer);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure Tw3dParser.ShellFormat4_Collection(ASuboptions: Byte; ASuboptions2: Integer);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure Tw3dParser.ShellFormat5_NullShell(ASuboptions: Byte; ASuboptions2: Integer);
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure Tw3dParser.StyleSegment;
var
  vProgress: Integer;
  vByte: Byte;
  vLength: Integer;
  vStr1, vCondition: string;
begin
  vByte := GetByte;
  vProgress := vByte;
  if vProgress <> 0 then
    vLength := vProgress
  else
  begin
    vByte := GetByte;
    vLength := vByte;
  end;
  vStr1 := GetStr(vLength);
  if vProgress = 0 then
  begin
    vByte := GetByte;
    vCondition := GetStr(vByte);
  end;
end;

procedure Tw3dParser.Polygon;
var
  I, vCount: Integer;
begin
  vCount := GetLong;
  for I := 0 to vCount do
    GetPoint;
end;

procedure Tw3dParser.Polyline;
var
  vCount: Integer;
  vPolyline: TsgDXFPolyline;
  vVertex : TsgDXFVertex;
begin
  vCount := GetLong;
  if vCount < 0 then
    vCount := -vCount;
  vPolyline := TsgDXFPolyline(NewEntity(TsgDXFPolyline));

  while vCount > 0 do
  begin
    vVertex := TsgDXFVertex.Create;
    vVertex.Point := GetPoint;
    vPolyline.AddEntity(vVertex);
    Dec(vCount);
  end;
end;

procedure Tw3dParser.Comment;
var
  vStrings: TStringList;
  vString: string;
  vByte: Byte;
  I: Integer;
begin
  vStrings := TStringList.Create;
  vString := '';
  try
    while True do
    begin
      vByte := GetByte;
      if vByte = 10 then
        Break;
      vString := vString + Chr(vByte);
    end;
  finally
    if not FFlagSeenHeader then
    begin
      if StringPos('; HSF V',vString, 1) = 1 then
      begin
        FFlagSeenHeader := True;
        FVersionW3D := 0;
        I := 8;
        while True do
        begin
          case vString[I] of
            '0'..'9':
            begin
               FVersionW3D := 10 * FVersionW3D  +  StrToInt(vString[I]);
            end;
            ' ': Break;
          end;
          Inc(I);
        end;
      end;
    end;
    vStrings.Free;
  end;
end;

constructor Tw3dParser.Create;
begin
  inherited Create;
  FInsertPrev := nil;
  FCurrentBlock := nil;
  FStackBlock := TsgStackObject.Create;
  FStackBlock.Clear;
  FListShadowBlock := TList.Create;
end;

procedure Tw3dParser.Window_Frame;
begin
  GetByte;
end;

procedure Tw3dParser.File_Info;
begin
  GetLong;
end;

procedure Tw3dParser.SetInitMatrix(const AInsert: TsgDXFInsert; const AMatrix: TFMatrix);
var
  vTempAngle: Double;
  vPoint, vScale, vExtrusion: TFPoint;
begin
  ExtractMatrixParams(AMatrix, vPoint, vScale,
    vExtrusion, vTempAngle);
  AInsert.Angle := vTempAngle;
  AInsert.Point := vPoint;
  AInsert.Scale := vScale;
  AInsert.Extrusion := vExtrusion;
end;

function Tw3dParser.ExtractName(const AName: string): string;
const
  cnstSharp = '#';
  cnstSpace = ' ';
  cnstAsk = '?';
  cntsSl = '/';
begin
  Result := AName;
  if AnsiPos(cnstSharp, Result) > 0 then
    ReplaceAnsi(Result, cnstSharp, '$23');
  if AnsiPos(cnstSpace, Result) > 0 then
    ReplaceAnsi(Result, cnstSpace, '$20');
  if AnsiPos(cntsSl, Result) > 0 then
    ReplaceAnsi(Result, cntsSl, '$2F');
  if AnsiPos(cnstAsk, Result) > 0 then
    ReplaceAnsi(Result, cnstAsk, '$3F');
end;

procedure Tw3dParser.OpenSegment;
var
  vName: string;
  vBlock: TsgDXFBlock;
  vInsert: TsgDXFInsert;
begin
  vName := GetStringBT;
  vName := ExtractName(vName);
  FCountPrevEntityBlock := 0;
  if vName = '' then
  begin
    FStackBlock.Push(FCurrentBlock); // No name segment - by increment Close.
    if Assigned(FCurrentBlock) then
      FCountPrevEntityBlock := FCurrentBlock.Count;
  end
  else
  begin
    vBlock := TsgDXFBlock.Create;
    FStackBlock.Push(FCurrentBlock);
    vBlock.Name := vName;
    FConv.Sections[csBlocks].AddEntity(vBlock);
    if StringPos(ExtractName('?Include Library'), vName,1) = 0 then
    begin
      vInsert := TsgDXFInsert(NewEntity(TsgDXFInsert));
      vInsert.Block := vBlock;
      FInsertPrev := vInsert;
    end;
    //
    // Important! - Appoint FCurrentBlock only after create vInsert
    //
    FCurrentBlock := vBlock;
  end;
end;

procedure Tw3dParser.User_Options;
var
  vOptions: string;
begin
  vOptions := GetStringVariableLength;
end;


procedure Tw3dParser.Shell;
var
  vSuboptions: Byte;
  vSuboptions2: Integer;
begin
  vSuboptions := GetByte;
  if vSuboptions and cnstTKSH_EXPANDED <> 0 then
  begin
    vSuboptions2 := GetWord;
    // TKSH2_NULL -- Shell is format 5. This bit takes precedence over
    // TKSH2_COLLECTION, TKSH_CONNECTIVITY_COMPRESSION and/or TKSH_BOUNDING_ONLY
    if vSuboptions2 and cnstTKSH2_NULL <> 0 then
    begin
      ShellFormat5_NullShell(vSuboptions, vSuboptions2);
      Exit;
    end;
    //TKSH_BOUNDING_ONLY -- Shell is format 3. This bit takes precedence over
    //TKSH_CONNECTIVITY_COMPRESSION and TKSH2_COLLECTION
    if vSuboptions and cnstTKSH_BOUNDING_ONLY <> 0 then
    begin
      ShellFormat3_BoundingOnly(vSuboptions, vSuboptions2);
      Exit;
    end;
    if vSuboptions2 and cnstTKSH2_COLLECTION <> 0 then
    Begin
      ShellFormat4_Collection(vSuboptions, vSuboptions2);
      Exit;
    End;
  end
  else
    vSuboptions2 := 0;
  if vSuboptions and cnstTKSH_BOUNDING_ONLY <> 0 then
  begin
      ShellFormat3_BoundingOnly(vSuboptions, vSuboptions2);
      Exit;
  end;
  if vSuboptions and cnstTKSH_CONNECTIVITY_COMPRESSION <> 0 then
  begin
      ShellFormat2_Edgebreaker(vSuboptions, vSuboptions2);
      Exit;
  end;
  ShellFormat1_Standard(vSuboptions, vSuboptions2);
end;

procedure Tw3dParser.Terminator;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure Tw3dParser.GeometryAttributes;
begin
{$IFDEF _FIXINSIGHT_}sgNOP;{$ENDIF}
end;

procedure Tw3dParser.Reference;
var
  {vIndex: Integer;}
  vByte: Byte;
  vTempStr: string;
begin
  GetLong;//vIndex := GetLong;
  vByte := GetByte;
  if vByte <> 0 then
    vTempStr := GetStr(vByte);
end;

procedure Tw3dParser.Grid;
var
  {vType: Byte;}
  vOrigin, Ref1, Ref2: TFPoint;
  vCounts: array[0..1] of Integer;
begin
  GetByte;//vType := GetByte;
  vOrigin := GetPoint;
  Ref1 := GetPoint;
  Ref2 := GetPoint;
  vCounts[0] := GetLong;
  vCounts[1] := GetLong;
end;

procedure Tw3dParser.Renumber;
//var
//  vValue: Integer;
begin
  GetLong;//vValue := GetLong;
end;

procedure Tw3dParser.AreaLight;
var
  I, vCount: Integer;
//  vOptions: Byte;
begin
  vCount := GetLong;
  for I := 0 to vCount-1 do
    GetPoint;
  GetByte;//vOptions := GetByte;
end;

procedure Tw3dParser.TextAlignment;
begin
  GetByte;
end;

procedure Tw3dParser.TextFont;
var
  vMask, vValue: Integer;
begin
  // Read  obligatory components
  //mask (Byte)  bitmask of options.
  //value (Byte) bitmask indicating which options are set on.
  vMask := GetByte;
  vValue := GetByte;
  if vMask and cnstTKO_Font_Extended  <> 0  then
  begin
    vMask := vMask or GetByte shl 8;
  end;
  if vMask and cnstTKO_Font_Extended  <> 0  then
  begin
    vValue := vValue or GetByte shl 8;
  end;
  if vMask and cnstTKO_Font_Extended2 <> 0 then
  begin
    vMask := vMask or GetWord shl 16;
  end;
  if vMask and cnstTKO_Font_Extended2 <> 0 then
  begin
    vValue := vValue or GetWord shl 16;
  end;

  if (vMask and vValue and cnstTKO_Font_Names) <> 0 then
  begin
    TextStyle(GetStringBT);
  end;
  if (vMask and vValue and cnstTKO_Font_Size) <> 0 then
  begin
    FTextSize := GetSingle;
    //Size_Units
    GetByte;
  end;
  if (vMask and vValue and cnstTKO_Font_Size_Tolerance) <> 0 then
  begin
    //Font_Size_Tolerence
    GetSingle;
    GetByte;
  end;
  if (vMask and vValue and cnstTKO_Font_Transforms) <> 0 then
  begin
    //Font_Transforms
    GetByte;
  end;
  if (vMask and vValue and cnstTKO_Font_Rotation) <> 0 then
  begin
    FTextRotation := GetSingle;
  end;
  if (vMask and vValue and cnstTKO_Font_Slant) <> 0 then
  begin
    //Font_Slant
    GetSingle;
  end;
  if (vMask and vValue and cnstTKO_Font_Width_Scale) <> 0 then
  begin
    //Font_Width_Scale
    GetSingle;
  end;
  if (vMask and vValue and cnstTKO_Font_Extra_Space) <> 0 then
  begin
    //Font_Extra_Space
    GetSingle;
    //Font_Extra_Space_Units
    GetByte;
  end;
  if (vMask and vValue and cnstTKO_Font_Line_Spacing) <> 0 then
  begin
    //Font_Line_Spacing
    GetSingle;
  end;
  if (vMask and vValue and cnstTKO_Font_Greeking_Limit) <> 0 then
  begin
    //Greeking_Limit
    GetSingle;
    //Greeking_Units
    GetByte;
  end;
  if (vMask and vValue and cnstTKO_Font_Greeking_Mode) <> 0 then
  begin
    //Greeking_Mode
    GetByte;
  end;
  if (vMask and vValue and cnstTKO_Font_Renderer) <> 0 then
  begin
    //Font_Renderer
    GetByte;
  end;
end;

procedure Tw3dParser.Texture;
var
  vByte{, vParamSource, vTiling, vInterpolation}: Byte;
{  vDecimation, vRedMapping, vGreenMapping, vBlueMapping: Byte;}
{  vAlphaMapping, vParamFunction, vLayout, vApplyMode, vParamOffset: Byte;}
  vMNameLength{, vMImageLength}: Integer;
  vName, vImage: string;
  vFlags: Integer;
  vMValueScale: array[0..1] of Single;
begin
  vByte := GetByte;
  if vByte = 255 then
    vMNameLength := GetLong
  else
    vMNameLength := vByte;
  vName := GetStr(vMNameLength);

  vByte := GetByte;
  if vByte = 255 then
    vMNameLength := GetLong
  else
    vMNameLength := vByte;
  vImage := GetStr(vMNameLength);

  vFlags := GetWord;

  if (vFlags and cnstTKO_Texture_Extended) <> 0 then
  begin
    vFlags := vFlags or (GetWord shl 16)
  end;

  if (vFlags and cnstTKO_Texture_Param_Source) <> 0 then
  begin
    GetByte;//vParamSource := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Tiling) <> 0 then
  begin
    GetByte;//vTiling := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Interpolation) <> 0 then
  begin
    GetByte;//vInterpolation := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Decimation) <> 0 then
  begin
    GetByte;//vDecimation := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Red_Mapping) <> 0 then
  begin
    GetByte;//vRedMapping := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Green_Mapping) <> 0 then
  begin
    GetByte;//vGreenMapping := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Blue_Mapping) <> 0 then
  begin
    GetByte;//vBlueMapping := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Alpha_Mapping) <> 0 then
  begin
    GetByte;//vAlphaMapping := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Param_Function) <> 0 then
  begin
     GetByte;//vParamFunction := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Layout) <> 0 then
  begin
    GetByte;//vLayout := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Value_Scale) <> 0 then
  begin
    vMValueScale[0] := GetSingle;
    vMValueScale[1] := GetSingle;
  end;

  if (vFlags and cnstTKO_Texture_Transform) <> 0  then
  begin
    vByte := GetByte; //GetData (tk, byte) -> m_progress = (int)byte
    GetStr(vByte); // GetData (tk, m_transform, m_progress)
  end;

  if (vFlags and (cnstTKO_Texture_Modulate or cnstTKO_Texture_Decal)) <> 0 then
  begin
    GetByte;//vApplyMode := GetByte;
  end;

  if (vFlags and cnstTKO_Texture_Param_Offset) <> 0 then
  begin
    GetByte;//vParamOffset := GetByte;
  end;
end;

procedure Tw3dParser.TextureMatrix;
var
  vMatrix: TFMatrix;
begin
  vMatrix := GetMatrix;
end;

procedure Tw3dParser.TextWithEncoding;
var
  vText: TsgDXFText;
  vWideChar: PWideChar;
  S, vS2: string;
  vEnc: Byte;
begin
  vText := TsgDXFText(NewEntity(TsgDXFText));
  if FStyle <> nil then
    vText.Style := FStyle
  else
    vText.Style := FConv.StyleByName(cnstStandard);
  vText.Point := GetPoint;
  vText.Height := FTextSize;
  vText.Rotation := FTextRotation;

  vEnc := GetByte;
  S := GetStringVT;
  if vEnc >= 4 then
  begin
    vWideChar := PWideChar(WideString(S));
    GetByte;
    while vWideChar^ <> #0 do
    begin
      vS2 := vS2 + '\U+' + IntToHex(TsgNativeUInt(vWideChar^), 4);
      Inc(vWideChar, SizeOf(Char));
    end;
    S := vS2;
  end;
  vText.Text := S;
end;

procedure Tw3dParser.View;
var
  vFieldWidth: Single;
  vPosition, vUpVector: TFPoint;
  vVPort: TsgDXFVPort;
  vProjection: Byte;
  vName: string;

  function GetAngleOfVectors(const AV1, AV2: TFPoint): Double;
  begin
    Result := (AV1.X * AV2.X + AV1.Y * AV2.Y + AV1.Z * AV2.Z) /
      Sqrt((AV1.X * AV1.X  + AV1.Y * AV1.Y + AV1.Z * AV1.Z) *
      (AV2.X * AV2.X  + AV2.Y * AV2.Y + AV2.Z * AV2.Z));
    if Abs(Result) > 1 then
    begin
      if Result > 0 then
        Result := 0
      else
        Result := 180;
    end
    else
      Result := ArcCos(Result) * f180DividedByPi;
  end;

begin
  vProjection := GetByte;
  vPosition := GetPoint;
  vVPort := TsgDXFVPort.Create;
  try
  vVPort.ViewTarget := GetPoint;
  vPosition.X := vPosition.X - vVPort.ViewTarget.X;
  vPosition.Y := vPosition.Y - vVPort.ViewTarget.Y;
  vPosition.Z := vPosition.Z - vVPort.ViewTarget.Z;
  vUpVector := Ort(GetPoint);
  vVPort.ViewDirection := Ort(vPosition);
  vVPort.ViewTwistAngle := GetAngleOfVectors(MakeFPoint(0, 0, 1), vUpVector);
  vFieldWidth := GetSingle;
  vVPort.ViewHeight := GetSingle;
  if vFieldWidth <> 0 then
    vVPort.ViewAspectRatio := 2 * vVPort.ViewHeight / vFieldWidth;

  //Camera_Oblique_Y
  if vProjection and cnstTKO_Camera_Oblique_Y <> 0 then
    GetSingle;
  //Camera_Oblique_X
  if vProjection and cnstTKO_Camera_Oblique_X <> 0 then
    GetSingle;
  //Camera_Near_Limit
  if vProjection and cnstTKO_Camera_Near_Limit <> 0 then
    GetSingle;
  vName := GetStringBT;
  if FConv.LayoutsCount = 1 then
  begin
    Tw2dConverter(FConv).ActiveVPort := vVPort;
    if vName = 'default' then
    begin
      vVPort.Name := sActiveVPort;
      Tw2dConverter(FConv).Sections[csVPorts].AddEntity(vVPort);
      vVPort := nil;
    end;
  end
  else
     FConv.Layouts[FConv.LayoutsCount - 1].SetRotMatrix(
       GetViewTwistMatrix(vVPort.ViewDirection,vVPort.ViewTwistAngle));
  finally
    vVPort.Free;
  end;
end;

procedure Tw3dParser.Visibility;
var
  vMask: Integer;
//  vValue: Integer;
begin
  vMask := GetByte;
  GetByte;//vValue := GetByte;
  if vMask and cnstTKO_Geo_Extended <> 0  then
  begin
    vMask := vMask or GetWord shl 8;
    GetWord;//vValue := vValue or GetWord shl 8;
  end;
  if vMask and cnstTKO_Geo_Extended2 <> 0  then
  begin
    GetByte;//vMask := vMask or GetByte shl 24;
    GetByte;//vValue := vValue or GetByte shl 24;
  end;
end;

procedure Tw3dParser.W3DImage;
var
  vName: string;
  vMSize: array[0..1] of Integer;
//  vMBPP: Byte;
begin
  vName := GetStringBT;
  vMSize[0] := GetLong;
  vMSize[1] := GetLong;
  GetByte;//vMBPP := GetByte;
end;

procedure Tw3dParser.Window;
begin
  GetSingle;
  GetSingle;
  GetSingle;
  GetSingle;
end;

procedure Tw3dParser.Decompress;
var
  vBuf, vTmpData: PData;
  vTmpCnt, vTmpLen, vCnt: Integer;
  vZS: z_stream;

  procedure InitZStream(var AZS: z_stream);
  begin
    FillChar(AZS, SizeOf(AZS), 0);
    AZS.avail_in := FDataLength - FCnt;
    AZS.next_in := pBytef(FPData);
    Inc(AZS.next_in, FCnt);
    inflateInit(AZS);
  end;

begin
  InitZStream(vZS);
  vBuf := nil;
  try
    vCnt := 0;
    repeat
      ReallocMem(vBuf, vCnt + 65536);
      vZS.avail_out := 65536;
      vZS.next_out := pBytef(TsgNativeUInt(vBuf) + Cardinal(vCnt));
      inflate(vZS, Z_SYNC_FLUSH);
      Inc(vCnt, 65536 - vZS.avail_out);
    until vZS.avail_out > 0;
    StoreData(Pointer(vTmpData), vTmpCnt, vTmpLen, vZS.avail_in);
    try
      Inc(PByte(FPData), FCnt); // ???: Not needed because "SetData" changing it
      SetData(vBuf, 0, vCnt);
    finally
      RestoreData(vTmpData, vTmpCnt, vTmpLen);
    end;
  finally
    FreeMem(vBuf);
    inflateEnd(vZS);
  end;
end;

{ Tw3dParser.GetEntities
  Main parser method. }
procedure Tw3dParser.GetEntities;
var
  vByte: Byte;
{$IFDEF SG_W2D_DEBUG}
  vStr, vStrEnd, vStrTab: String;
{$ENDIF}
begin
  if FCnt < 0 then
    FCnt := 0;
  while FCnt < FDataLength do
  begin
    DoProgress;
    vByte := GetByte;

{$IFDEF SG_W2D_DEBUG_FILE}
    if vStrings.Count > 1000{1} then
    begin
      vStrings.SaveToStream(vFileStream);
      FlushFileBuffers(vFileStream.Handle);
      vStrings.Clear;
    end;
{$ENDIF}
{$IFDEF SG_W2D_DEBUG}
    Inc(gIndex);

    case AnsiChar(vByte) of
      '(': begin
        vStrTab := StringOfChar('.',gLevelTab*2);
        Inc(gLevelTab);
      end;
      ')': begin
        Dec(gLevelTab);
        vStrTab := StringOfChar('.',gLevelTab*2);
      end
    else
      if gLevelTab > 0 then
        vStrTab := StringOfChar('.',gLevelTab*2+2)
      else
        vStrTab := '';
    end;

    vStr :=  Format('%10d',[gIndex])+ ': '+vStrTab +' '+ cnstOpcodeString[vByte];
    vStrEnd := '-ok';
{$ENDIF}

    case AnsiChar(vByte) of
      #00: Terminator;
      '"': Color;
      '%': Matrix;
      '$': TextureMatrix;
      '(': OpenSegment;
      ')': CloseSegment;
      '<': IncludeSegment; //$3C
      AnsiChar($E0): W3DImage;
      '*': TextAlignment;
      '-': LinePattern;
      ';': Comment;// while GetByte <> 10 do;
      '=': LineWeight;
      'B': BoundingInfo;
      'C': Circle(True);
      'E': Inc(FCnt, 36);
      'F': TextFont;
      'H': Heuristics;
      'I': File_Info;
      'J': LineType;
      'S': Shell;
      'U': User_Options;
      'Z': Decompress;
      'c': Circle(False);
      'e': Inc(FCnt, 44);
      'l': Line;
      't': Texture;
      'x': TextWithEncoding;
      '}': View;
      '~': ColorRGB;
      #$47: Self.Polygon;
      #$23: Window_Frame;
      'V': Visibility; //$56
      #$57: Window;
      '{': StyleSegment;// $7B
      'L': Polyline;
      'N': NurbsCurve; // $4E
      'R': RenderingOptions; // $52
      'd': DistantLight; // $64
      ':': GeometryAttributes;
      'r': Reference;
      'g': Grid; // $67
      'K','k','0': Renumber; //TKE_Renumber_Key_Global,TKE_Renumber_Key_Local,TKE_Priority
      'a': AreaLight; // $61
      'q', 'z':  ;
{$IFDEF SG_W2D_DEBUG}
    else
      vStrEnd := '-no read';
{$ENDIF}
      //ErrorFmt(sIllegalOpcodeFmt, [Pointer(TsgNativeUInt(FPData) + Cardinal(FCnt) - 1), FCnt - 1]);
    end;

{$IFDEF SG_W2D_DEBUG}
    vStr := vStr + vStrEnd;
{$ENDIF}
{$IFDEF SG_W2D_DEBUG_FILE}
    vStrings.Add(vStr);
{$ELSE}
{$IFDEF SG_W2D_DEBUG_OUTPUTDEBUGSTRING}
  OutputDebugString(PChar(vStr));
{$ENDIF}
{$ENDIF}

  end;
end;

function Tw3dParser.GetMatrix: TFMatrix;
var
  I, J: Integer;
begin
{$IFDEF _FIXINSIGHT_}FillChar(Result, SizeOf(Result), 0);{$ENDIF}
  for I := 0 to 3 do
    for J := 0 to 2 do
      Result.M[I, J] := GetSingle;
end;

procedure Tw3dParser.IncludeSegment;
var
  vName: string;
  vBlock: TsgDXFBlock;
  vInsert: TsgDXFInsert;
begin
  vName := GetStringBT;
  vName := ExtractName(vName);
  vBlock := TsgDXFBlock(TsgDXFTable(FConv.Sections[csBlocks]).FindEntByName(vName));
  if vBlock <> nil then
  begin
    vInsert := TsgDXFInsert(NewEntity(TsgDXFInsert));
    vInsert.Block := vBlock;
  end;
end;

function Tw3dParser.IsDWF : Boolean;
var
  vPInteger: PInteger;
begin
  vPInteger := PInteger(FPData);
  Result := vPInteger^ = $48203B3B;
  if Result then
  begin
    Inc(vPInteger);
    Result := vPInteger^ = $56204653;
  end;
  FFlagSeenHeader := False;
end;

{ TwxdParser }

procedure TwxdParser.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, 100 * (FPercent + FCnt / FDataLength));
end;

procedure TwxdParser.Error(const AErrorString: string);
begin
  raise ECmndError.Create(AErrorString);
end;

procedure TwxdParser.ErrorFmt(const AErrorString: string;
  AArgs: array of const);
begin
  raise ECmndError.CreateFmt(AErrorString, AArgs);
end;

function TwxdParser.GetBackgroundColor: Integer;
begin
  Result := FBackgroundColor;
end;

function TwxdParser.GetByte: Byte;
begin
  Result := PDataValue;
  Inc(FCnt);
end;

function TwxdParser.GetEntitiesList: TsgObjectList;
begin
  Result := FEntities;
end;

function TwxdParser.GetLong: Integer;
begin
  Result := GetWord + GetWord shl 16;
end;

function TwxdParser.GetPDataArray(const AIndex: Integer): Byte;
begin
  if AIndex < FDataLength then
    Result := PByte(TsgNativeUInt(FPData) + Cardinal(AIndex))^
  else
    Result := 0;
end;

function TwxdParser.GetPDataValue: Byte;
begin
  Result := PDataArray[FCnt];
end;

function TwxdParser.GetSmall: Integer;
begin
  Result := Smallint(GetWord);
end;

function TwxdParser.GetSmallint: Smallint;
begin
  Result := GetByte;
  if Result = 0 then
    Result := GetWord + 256;
end;

function TwxdParser.GetUnsignedLong: Integer;
begin
  Result := Integer(GetLong);
end;

function TwxdParser.GetCount: Word;
begin
  Result := GetByte;
  if Result = 0 then
    Result := GetWord + 256;
end;

function TwxdParser.GetCurrentColor: TColor;
begin
  Result := CurrColor_;
end;

function TwxdParser.GetWord: Integer;
begin
  Result := GetByte + GetByte shl 8;
end;

function TwxdParser.ReadCurrColor(const AByIndex: Boolean): TColor;
var
  vCnt: Integer;
begin
  if AByIndex then
  begin
    vCnt := Integer(RedColor) shl 2;
    RedColor := CurrColorMap^[vCnt];
    GreenColor := CurrColorMap^[vCnt + 1];
    BlueColor := CurrColorMap^[vCnt + 2];
  end;
  Result := Byte(RedColor) or (Byte(GreenColor) shl 8) or (Byte(BlueColor) shl 16);
  if Result = clBlack then
   Result := clByLayer;
end;

procedure TwxdParser.RestoreData(const AData: Pointer; const ACount: Integer;
  const ADataLen: Integer);
begin
  FPData := AData;
  FCnt := ACount;
  FDataLength := ADataLen;
  FPercent := FCnt / FDataLength;
end;

procedure TwxdParser.SetCurrentColor(const AColor: TColor);
begin
  CurrColor_ := AColor;
end;

procedure TwxdParser.SetData(const AData: Pointer; const ACount: Integer;
  const ADataLen: Integer);
begin
  FPData := AData;
  FCnt := ACount;
  FDataLength := ADataLen;
  GetEntities;
end;

procedure TwxdParser.StoreData(var AData: Pointer; var ACount,
  ADataLen: Integer; const AOffs: Cardinal);
begin
  FPercent := FCnt / FDataLength;
  if Integer(AOffs) <> -1 then
    FCnt := Cardinal(FDataLength) - AOffs;
  AData := FPData;
  ACount := FCnt;
  ADataLen := FDataLength;
end;

procedure TwxdParser.TextStyle(const AName: string);
begin
  FStyle := FConv.StyleByName(AName);
  FStyle.FontName := AName;
end;

constructor  TwxdParser.Create;
begin
  FEntities := TsgObjectList.Create;
  FLayers := TsgCollection.Create;
  FHyperLinkList := TsgObjectCollection.Create;
  FCurrentURLIndex := -1;
  FUnitMatrix := cnstIdentityMat;
  FXScale := 1.0;
end;

destructor TwxdParser.Destroy;
var
  I: Integer;
begin
  FEntities.Free;
  FLayers.Free;
  for I := 0 to FHyperLinkList.Count - 1 do
    TsgUrlValue(FHyperLinkList[I].Data).Free;
  FHyperLinkList.Free;
  inherited Destroy;
end;

procedure TwxdParser.LoadFromStream(AConv: TsgDXFConverter; AStream: TStream);
var
  vMS: TMemoryStream;
begin
  FConv := AConv;
  FConv.UseSHXFonts := False;
  vMS := nil;
  try

{$IFDEF SG_W2D_DEBUG_FILE}
    vStrings := TStringList.Create;
    vFileStream := TFileStream.Create('d:\t22.txt',fmCreate);
    vFileStream.Position := 0;
{$ENDIF}
{$IFDEF SG_W2D_DEBUG}
    gIndex := 0;
    gLevelTab := 0;
{$ENDIF}

    if AStream is TCustomMemoryStream then
      vMS := TMemoryStream(AStream)
    else
    begin
      vMS := TMemoryStream.Create;
      vMS.LoadFromStream(AStream);
      vMS.Position := 0;
    end;
    FDataLength := vMS.Size - vMS.Position;
    FPData := vMS.Memory;
    Inc(PChar(FPData), vMS.Position);
    if not IsDWF then
      raise EFormatError.Create('File Format Error');
    FCnt := -1;
    FBackgroundColor := clWhite;
    FIsSetBackgroundColor := False;
    FontRotation := 0;
    // Init Layer
    FCurrentLayerName := '0';
    FCurrentLayer := FConv.LayerByName(FCurrentLayerName);
    FLayers.Add(UInt64(0), Pointer(FCurrentLayer));
    //
    gTextHAlign := haLeft;
    gTextVAlign := vaBaseline;
    GetEntities;
  finally
    if vMS <> AStream then
      vMS.Free;

{$IFDEF SG_W2D_DEBUG_FILE}
  vStrings.SaveToStream(vFileStream);
  vStrings.Free;
  vFileStream.Free;
{$ENDIF}
  end;
end;

{ TsgEasyStack }

constructor TsgEasyStack.Create;
begin
  FAllocated := 6;
  FData := AllocMem(FAllocated * SizeOf(Integer));
  FUsed := 0;
end;

destructor TsgEasyStack.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  FData := nil;
  FAllocated := 0;
  inherited Destroy;
end;

procedure TsgEasyStack.Clear;
var
  I, vUsed: Integer;
begin
  vUsed := FUsed;
  for I := 0 to vUsed-1 do
    Pop;
end;

function TsgEasyStack.Pop: Integer;
begin
  Dec(FUsed);
  Result := PsgIntegerArray(FData)[FUsed];
end;

function TsgEasyStack.PopInternal(AOffset: Integer; var AOut: Integer): Integer;
var
  I, vPlace, vReturnVal: Integer;
begin
  vPlace := FUsed - AOffset - 1;
  vReturnVal := PsgIntegerArray(FData)[vPlace];
  for I := vPlace to (FUsed - 1) - 1 do
    PsgIntegerArray(FData)[I] := PsgIntegerArray(FData)[I+1];
  Dec(FUsed);
  AOut := vReturnVal;
  Result := 1;
end;

procedure TsgEasyStack.Push(const AValue: Integer);
begin
  if FUsed = FAllocated then
    StackExpand;
  PsgIntegerArray(FData)[FUsed] := AValue;
  Inc(FUsed);
end;

procedure TsgEasyStack.StackExpand;
var
  vTemp: Pinteger;
  I: Integer;
begin
  if FAllocated < 2 then
    FAllocated := 2
  else
    FAllocated := FAllocated * 2;
  vTemp := AllocMem(FAllocated * SizeOf(Integer));
  if FData <> nil then
  begin
    for I := 0 to FUsed - 1 do
      PsgIntegerArray(vTemp)[i] := PsgIntegerArray(FData)[i];
    FreeMem(FData);
  end;
  FData := vTemp;
end;

{ TsgVarStream }

constructor TsgVarStream.Create(const ASize: Integer; APointer: PByte);
var
  I: Integer;
begin
  FAllocated := ASize div 4;
  FData := nil;
  FRData := PInteger(APointer);
  FUsed := 0;
  FBit := 0;
  FRUsed := 0;
  FRBit := 0;
  FCanReallocate := 0;
  FStatus := 1;
  FMask[0] := 0;
  FRange[0] := 0;
  for I := 1 to 32 do
  begin
    FMask[I] := DWORD(-1) shr (32-I);
    FRange[I] := ($1 shl (I-1)) - 1;
  end;
end;

destructor TsgVarStream.Destroy;
begin
  inherited Destroy;
end;

function TsgVarStream.Get(const ANumbitsArray: array of Integer): Integer;
var
  I, vVal: Integer;
begin
  I := 0;
  while True do
  begin
    Get2(ANumbitsArray[I], vVal);
    if vVal = Integer(FMask[ANumbitsArray[I]]) then
      Inc(I)
    else
      Break;
  end;

  Dec(vVal,FRange[ANumbitsArray[I]]);
  Result := vVal;
end;

procedure TsgVarStream.Get2(const ANumbits: Integer; var AVal: Integer);
var
  vShift: Integer;
begin
  if FRBit + ANumbits <= 32 then
  begin
    AVal := (PsgIntegerArray(FRData)[FRUsed] shr (32 - FRBit - ANumbits)) and
      FMask[ANumbits];
    Inc(FRBit, ANumbits);
  end
  else
  begin
    vShift := (ANumbits + FRBit - 32);
    AVal := (PsgIntegerArray(FRData)[FRUsed] shl vShift) and FMask[ANumbits];
    Inc(FRUsed);
    AVal := AVal or (PsgIntegerArray(FRData)[FRUsed] shr (32 - vShift));
    Inc(FRBit, ANumbits - 32);
  end;
end;

{ Tw2dParserOverlay }

procedure Tw2dParserOverlay.GetEntities;
{$IFDEF SG_USE_TIFF}
var
  vMemStream: TMemoryStream;
  vBmp: TBitmap;
{$ENDIF}
begin
{$IFDEF SG_USE_TIFF}
  vMemStream := TMemoryStream.Create;
  try
    vMemStream.Write(FPData^, FDataLength);
    vBmp := TBitmap.Create;
    try
      LoadTiff(vMemStream, vBmp);
      CreateAndAddImage(MakeFPoint(0,0), MakeFPoint(vBmp.Width, vBmp.Height), vBmp);
    finally
      vBmp.Free;
    end;
  finally
    vMemStream.Free;
  end;
{$ENDIF}
end;

function Tw2dParserOverlay.IsDWF: Boolean;
begin
  Result := True;
end;

initialization
{$IFNDEF SG_NON_WIN_PLATFORM}
  CF_W2D := RegisterClipboardFormat('SoftGold W2D Image');
{$ENDIF}
  TPicture.RegisterClipboardFormat(CF_W2D, TsgW2DImage);
  TPicture.RegisterFileFormat('W2D', SW2DImage, TsgW2DImage);

finalization
  TPicture.UnRegisterGraphicClass(TsgW2DImage);

end.
