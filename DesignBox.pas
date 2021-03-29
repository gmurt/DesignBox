unit DesignBox;

interface

uses
  Windows, Messages, System.SysUtils, System.Classes, Vcl.Controls, System.Types, System.Generics.Collections,
  vcl.Graphics, System.Generics.Defaults, System.Json, vcl.Imaging.Jpeg, Vcl.Forms;

type
  TDesignBoxMode = (dbmSelect, dbmDraw);

  TItemOption = (canSize, canMove, canDelete, canSelect, canEdit);
  TItemOptions = set of TItemOption;

  TDesignBox = class;
  TDesignBoxBaseItem = class;

  TDesignBoxAfterDrawEvent = reference to procedure(Sender: TObject; aNewItem : TDesignBoxBaseItem; aIndex: integer);  // triggered after MANUALLY drawing an item

  IBrushObject = interface
    ['{66B92A59-B773-4DE2-97DF-34CA29245A0E}']
    function GetBrush: TBrush;
    procedure SetBrush(const Value: TBrush);
    property Brush: TBrush read GetBrush write SetBrush;
  end;

  IFontObject = interface
    ['{29E2758B-E873-4E77-971D-930588E43164}']
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    property Font: TFont read GetFont write SetFont;
  end;

  IPenObject = interface
    ['{09E70A1C-6C2D-4BDE-8D1C-3311B186F411}']
    function GetPen: TPen;
    procedure SetPen(const Value: TPen);
    property Pen: TPen read GetPen write SetPen;
  end;

  TDesignBoxSelectItemEvent = procedure(Sender: TObject; AItem: TDesignBoxBaseItem) of object;

  TDesignUndoList = class
  private
    FDesignBox: TDesignBox;
    FChanges: TStrings;
    FChangesBookmark: integer;
    function GetCount: integer;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCurrentSnapshot: string;
    procedure SaveSnapshot(AForce: Boolean);
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    destructor Destroy; override;
    procedure Undo;
    procedure Redo;
    property Count: integer read GetCount;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  end;

  TDesignFont = class(TFont)
  public
    procedure SaveToJson(AJson: TJsonObject);
    procedure LoadFromJson(AJson: TJsonObject);
  end;

  TDesignBrush = class(TBrush)
  public
    procedure SaveToJson(AJson: TJsonObject);
    procedure LoadFromJson(AJson: TJsonObject);
  end;

  TDesignPen = class(TPen)
  public
    procedure SaveToJson(AJson: TJsonObject);
    procedure LoadFromJson(AJson: TJsonObject);
  end;

  TDesignBoxItemInterface = class(TPersistent, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TDesignBoxBaseItem = class(TDesignBoxItemInterface)
  private
    FDesignBox: TDesignBox;
    FPosition: TPoint;
    FWidth: integer;
    FHeight: integer;
    FSelected: Boolean;
    FDrawOffset: TPoint;
    fOptions : TItemOptions;
    fVisible: boolean;
    fTag: NativeInt;
    fData: TObject;
    function GetLeftMM: Extended;
    function GetTopMM: Extended;
    procedure Changed;
    procedure SetLeftMM(const Value: Extended);
    procedure SetTopMM(const Value: Extended);
    procedure SetSelectedItem(const Value: Boolean);
    procedure SetOptions(const Value: TItemOptions);
    function GetWidthMM: Extended;
    procedure SetWidthMM(const Value: Extended);
    function GetHeightMm: Extended;
    procedure SetHeightMm(const Value: Extended);
    function GetCenterPtMm: TPointF;
    procedure SetCenterPtMm(const Value: TPointF);
    procedure SetVisible(const Value: boolean);
  protected
    function RectPixels: TRect; virtual;
    function RectMM: TRectF;
    function BoundsRect: TRect;
    procedure PaintToCanvas(ACanvas: TCanvas); virtual; abstract;
    procedure DrawSelectedRect(ACanvas: TCanvas); virtual;
    property Data: TObject read fData write fData; // for linking external object instances (or even other design box items)
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    destructor Destroy; override;
    function PointInRgn(x, y: integer): Boolean;
    function RectsIntersect(ARect: TRect): Boolean;
    procedure UpdateToDragPosition;
    procedure SnapToGrid;
    procedure SaveToJson(AJson: TJsonObject); virtual;
    function asJsonObject: TJsonObject;
    procedure LoadFromJson(AJson: TJsonObject); virtual;
    property Selected: Boolean read FSelected write SetSelectedItem;
    property LeftMM: Extended read GetLeftMM write SetLeftMM;
    property TopMM: Extended read GetTopMM write SetTopMM;
    property WidthMM: Extended read GetWidthMM write SetWidthMM;
    property HeightMM: Extended read GetHeightMm write SetHeightMm;
    property CenterPtMm: TPointF read GetCenterPtMm write SetCenterPtMm;
    property Options: TItemOptions read fOptions write SetOptions;
    property Visible: boolean read fVisible write SetVisible;
    property Tag: NativeInt read fTag write fTag;
  end;

  TDesignBoxBaseItemClass = class of TDesignBoxBaseItem;

  TDragHandlePos = (dhTopLeft,dhTopRight,dhBottomRight,dhBottomLeft);  // clockwise from (0,0)
  TDragHandleRectArray = array[TDragHandlePos] of TRect;

  /// base class for items with a set of grab handles - note Text items will not have this as Font size & text property will determine bounds
  TDesignBoxItemSizable = class(TDesignBoxBaseItem)
  private
    fSelectHandles: TDragHandleRectArray;
    //fIsDraggingHandle: boolean;      // is user dragging via a select handle?
    //fSelectedHandle: TDragHandlePos; // if so which one ?
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    procedure DrawSelectedRect(ACanvas: TCanvas); override;

  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SetCoords(ALeftMM, ATopMM, ARightMM, ABottomMM: single);
  end;

  TDesignBoxItemOutlineShape = class(TDesignBoxItemSizable, IPenObject)
  private
    FPen: TDesignPen;
    function GetPen: TPen;
    procedure DoPenChange(Sender: TObject);
    procedure SetPen(const Value: TPen);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Pen: TPen read GetPen write SetPen;

  end;

  TDesignBoxItemFilledShape = class(TDesignBoxItemOutlineShape, IBrushObject)
  private
    FBrush: TDesignBrush;
    procedure DoBrushChange(Sender: TObject);
    function GetBrush: TBrush;
    procedure SetBrush(const Value: TBrush);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Brush: TBrush read GetBrush write SetBrush;
  end;

  TDesignBoxItemText = class(TDesignBoxItemFilledShape, IFontObject)
  private
    FText: string;
    FFont: TDesignFont;
    function GetFont: TFont;
    procedure DoFontChange(Sender: TObject);
    procedure SetText(const AText: string);
    procedure SetFont(const Value: TFont);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Text: string read FText write SetText;
    property Font: TFont read GetFont write SetFont;
  end;



  TDesignBoxItemEllipse = class(TDesignBoxItemFilledShape)
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  end;

  TDesignBoxItemRectangle = class(TDesignBoxItemFilledShape)
  private
    FRoundnessMm: single;
    procedure SetRoundnessMm(const Value: single);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    property RoundnessMm: single read FRoundnessMm write SetRoundnessMm;
  end;

  TDesignBoxItemGraphic = class(TDesignBoxItemSizable)
  private
    FGraphic: TPicture;
    procedure SetGraphic(const Value: TPicture);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Graphic: TPicture read FGraphic write SetGraphic;
  end;

  TItemAlignment = (ialLeftSides, ialTopSides, ialRightSides, ialBottomSides, ialToGrid,
    ialVertCenters, ialHorzCenters);

  TDesignBoxItemList = class(TObjectList<TDesignBoxBaseItem>)
  private
    FDesignBox: TDesignBox;
    function GetSelectedCount: integer;
    procedure PaintToCanvas(ACanvas: TCanvas);
    function BoundsRectMM: TRectF;
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    function BoundsRect: TRect;
    procedure AlignItems(const aAlignment : TItemAlignment);
    function Add(AItem: TDesignBoxBaseItem; ABoundsMm: TRectF): TDesignBoxBaseItem; overload;
    function Add(AClass: TDesignBoxBaseItemClass; ABoundsMm: TRectF): TDesignBoxBaseItem; overload;
    function ItemAtPos(x, y: integer): TDesignBoxBaseItem;
    procedure LoadFromJson(AJson: TJsonObject);
    procedure SaveToJson(AJson: TJsonObject);
    procedure DeleteSelected;
    property SelectedCount: integer read GetSelectedCount;
    procedure DeselectAll;
    procedure SelectAll;
    procedure SelectItems(AItems: array of TDesignBoxBaseItem; const ADeselectOthers: Boolean = False);
    property DesignBox: TDesignBox read fDesignBox;


  end;

  TDesignMeasurementSystem = (dbMetric, dbMetricCent, dbImperial);

  TDesignBoxRulerOptions = class(TPersistent)
  private
    fDesignBox : TDesignBox;
    fForegroundColor: TColor;
    fBackgroundColor: TColor;
    fVisible: Boolean;
    fRulerHeightPx : integer;
    fRulerWidthPx : integer;
    fShowUnits: boolean;
    fUnits: string;
    fMeasurementSystem : TDesignMeasurementSystem;
    fFont : TDesignFont;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetVisible(const Value: boolean);
    function  GetHeightPx: integer;
    function  GetWidthPx: integer;
    procedure SetShowUnits(const Value: boolean);
    procedure SetUnits(const Value: string);
    procedure SetFont(const Value: TDesignFont);
    procedure internalHandleRulerFontChanged(Sender: TObject);
    procedure SetMeasurementSystem(const Value: TDesignMeasurementSystem);
  protected
    const
      UNIT_NAMES: array[TDesignMeasurementSystem] of string = ('mm', 'cm', 'in');

  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromJsonObject(AJson: TJsonObject);
    procedure SaveToJsonObject(AJson: TJsonObject);
  published
    property ForegroundColor: TColor read fForegroundColor write SetForegroundColor default clBlack;
    property BackgroundColor: TColor read fBackgroundColor write SetBackgroundColor default clWhite;
    property Visible: boolean read fVisible write SetVisible default true;
    property WidthPx : integer read GetWidthPx;
    property HeightPx : integer read GetHeightPx;
    property ShowUnits: boolean read fShowUnits write SetShowUnits default false;
    property Units: string read fUnits write SetUnits;
    property MeasurementSystem: TDesignMeasurementSystem read fMeasurementSystem write SetMeasurementSystem;
    property Font: TDesignFont read fFont write SetFont;
  end;

  TDesignBoxGridStyle = (dbgsHorz, dbgsVert, dbgsBoth);

  TDesignBoxGridOptions = class(TPersistent)
  private
    FDesignBox: TDesignBox;
    FVisible: Boolean;
    FSizeMm: integer;
    FColor: TColor;
    FSnapToGrid: boolean;
    fStyle: TDesignBoxGridStyle;
    procedure SetSizeMm(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetSnapToGrid(const Value: Boolean);
    procedure SetStyle(const Value: TDesignBoxGridStyle);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromJsonObject(AJson: TJsonObject);
    procedure SaveToJsonObject(AJson: TJsonObject);
    function PixelSize: integer;
  published
    property Color: TColor read FColor write SetColor default $00F7E1CE;
    property SizeMm: integer read FSizeMm write SetSizeMm default 5;
    property Visible: Boolean read FVisible write SetVisible default False;
    property SnapToGrid: Boolean read FSnapToGrid write SetSnapToGrid default False;
    property Style: TDesignBoxGridStyle read fStyle write SetStyle default dbgsBoth;
  end;

  TDesignBoxBackgroundPosition = (bgStretch, bgStretchProp, bgCentre);

  TDesignBoxCanvas = class(TGraphicControl)
  private
    //FTempCanvas: TBitmap;
    FBuffer: TBitmap;
    FDesignBox: TDesignBox;
    FBrush: TBrush;
    FFont: TFont;
    FPen: TPen;
    FDragging: Boolean;
    FMouseDownPos: TPoint;
    FMouseXY: TPoint;
    fBackground: TPicture;
    fBackgroundPos: TDesignBoxBackgroundPosition;
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);

    procedure OnBrushChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure OnPenChanged(Sender: TObject);
    procedure SetBackground(const Value: TPicture);
    procedure SetBackgroundPos(const Value: TDesignBoxBackgroundPosition);
    function GetDestRect(aOriginalSize, aClientSize: TSize; Stretch, Proportional, Center: boolean): TRect;
    procedure SaveBackgroundToJson(const aJson: TJsonObject);
    procedure LoadBackgroundFromJson(const aJson: TJsonObject);

  protected
    procedure WMEraseBackground(var message: TMessage); message WM_ERASEBKGND;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DrawBackground(const aRect: TRect);
    procedure SaveToJson(const aJson: TJsonObject);
    procedure LoadFromJson(const aJson: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetSize(w, h: integer);
    function TextExtent(const AText: string): TSizeF;
    function TextWidth(const AText: string): single;
    function TextHeight(const AText: string): single;
    function MeasureText(const AText: string): TSizeF;

    function TextOut(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
    function Draw(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
    function StretchDraw(ARect: TRectF; AGraphic: TGraphic): TDesignBoxItemGraphic;

    function Rectangle(ALeftMM, ATopMM, ARightMM, ABottomMM: single; const ARoundnessMm: single = 0): TDesignBoxItemRectangle; overload;
    function Rectangle(ABoundsMm: TRectF; const ARoundnessMm: single = 0): TDesignBoxItemRectangle; overload;
    function Ellipse(ABoundsMm: TRectF): TDesignBoxItemEllipse; overload;
    function Ellipse(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemEllipse; overload;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Background: TPicture read fBackground write SetBackground;
    property BackgroundPos: TDesignBoxBackgroundPosition read fBackgroundPos write SetBackgroundPos;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
  end;

  TDesignBoxMouseInteractionOptions = class
  private
    fDragSelect : boolean;
    procedure SetDragSelect(const Value: boolean);
  published
    constructor Create;
    property DragSelect: boolean read fDragSelect write SetDragSelect default True;
  end;

  TDesignBox = class(TScrollingWinControl)
  private
    FMode: TDesignBoxMode;
    fDrawClass : TDesignBoxBaseItemClass;
    FCanvas: TDesignBoxCanvas;
    FItems: TDesignBoxItemList;
    FSelectedItems: TDesignBoxItemList;
    FOnSelectItem: TDesignBoxSelectItemEvent;
    FBackgroundColor: TColor;
    FPageColor: TColor;
    FPageOffset: TPoint;
    FPageSizeMM: TSize;
    FUndoList: TDesignUndoList;
    FOnChange: TNotifyEvent;
    FGridOptions: TDesignBoxGridOptions;
    FRulerOptions: TDesignBoxRulerOptions;
    FUpdateCount: integer;
    FAfterDrawItem: TDesignBoxAfterDrawEvent;
    FBorderStyle: TBorderStyle;
    fMouseInteraction: TDesignBoxMouseInteractionOptions;
    function GetSelectedItems: TDesignBoxItemList;
    procedure SetBackgroundColor(const Value: TColor);
    procedure RecordSnapshot;
    procedure DrawRulers(ACanvas: TCanvas);
    procedure ResizeCanvas;
    function GetPageHeightMM: integer;
    function GetPageWidthMM: integer;
    procedure SetPageHeightMM(const Value: integer);
    procedure SetPageWidthMM(const Value: integer);
    procedure SetGridOptions(const Value: TDesignBoxGridOptions);
    procedure DrawGrid(ACanvas: TCanvas);
    procedure SetRulerOptions(const Value: TDesignBoxRulerOptions);
    procedure SetPageColor(const Value: TColor);

    procedure SetBorderStyle(Value: TBorderStyle);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetMouseInteraction(const Value: TDesignBoxMouseInteractionOptions);

  protected

    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;

    procedure Loaded; override;
    procedure Resize; override;

    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;

    procedure WMEraseBackground(var message: TMessage); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure Clear;
    procedure Endupdate;
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromJson(AJsonData: string);
    procedure SaveToFile(AFilename: string);
    procedure LoadFromFile(AFilename: string);
    procedure BringToFront;
    procedure BringForwards;
    procedure SendToBack;
    procedure SendBackwards;
    procedure SetPageSize(AWidthMM, AHeightMM: integer); overload;
    procedure SetPageSize(APageSize: TSize); overload;
    procedure Redraw;
    procedure Undo;
    procedure Redo;
    procedure SaveSnapShot(aForce: boolean);
    function  CanUndo : boolean;
    function  CanRedo : boolean;
    procedure AlignItems(const aAlignment: TItemAlignment);

    property Canvas: TDesignBoxCanvas read FCanvas;
    property Items: TDesignBoxItemList read FItems;
    property Mode: TDesignBoxMode read FMode write FMode default dbmSelect;
    property DrawClass : TDesignBoxBaseItemClass read fDrawClass write fDrawClass;
    property SelectedItems: TDesignBoxItemList read GetSelectedItems;// write SetSelectedItem;

    procedure SaveAsBitmap(const AFileName: string); overload;
    procedure SaveAsBitmap(const ABitmap: TBitmap); overload;
    procedure SaveAsBitmap(const AStream: TStream); overload;

    procedure SaveAsJpeg(const aFilename: string; const aCompressionQuality: TJPEGQualityRange = 100; const aGrayscale: boolean = false); overload;
    procedure SaveAsJpeg(const aStream: TStream; const aCompressionQuality: TJPEGQUalityRange = 100; const aGrayscale: boolean = false); overload;
    procedure SaveAsPNG(const aFileName: string; const aTransparent: boolean); overload;
    procedure SaveAsPNG(const aStream: TStream; const aTransparent: boolean); overload;

  published
    property Align;
    property BackgroundColor: TColor read fBackgroundColor write SetBackgroundColor default clSilver;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Ctl3D;
    property PageColor : TColor read fPageColor write SetPageColor default clWhite;
    property GridOptions: TDesignBoxGridOptions read FGridOptions write SetGridOptions;
    property PopupMenu;
    property RulerOptions: TDesignBoxRulerOptions read fRulerOptions write SetRulerOptions;
    property MouseInteraction: TDesignBoxMouseInteractionOptions read fMouseInteraction write SetMouseInteraction;
    property PageWidthMM: integer read GetPageWidthMM write SetPageWidthMM;
    property PageHeightMM: integer read GetPageHeightMM write SetPageHeightMM;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem: TDesignBoxSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property AfterDrawItem: TDesignBoxAfterDrawEvent read fAfterDrawItem write fAfterDrawItem;
    property OnResize;
  end;

procedure Register;

function MmToPixels(AValue: Extended): integer;
function PixelsToMM(AValue: Extended) : single;
function MMtoMu(AValue: Extended) : integer;
function MuToMM(AValue: integer) : Extended;

implementation

uses System.NetEncoding, PngImage, Math, System.UITypes, System.RTTI;

const
  C_HIGHLIGHT_COLOR = clHotlight;
  C_SELECTBOX_INFLATE = 0;
  C_DPI = 300;
  C_MM_PER_INCH = 25.4;
  C_DRAG_THRESHOLD = 4; //px

procedure Register;
begin
  RegisterComponents('Samples', [TDesignBox]);
end;

function ScreenDpi: Integer;
begin
  Result := Screen.PixelsPerInch;
end;

function MmToPixels(AValue: Extended): integer;
var
  AScale: single;
begin
  AScale := ScreenDpi / C_DPI;
  Result := Round((C_DPI / C_MM_PER_INCH) * AValue * AScale);
end;

function MmToPixelsF(AValue: Extended): Extended;
var
  AScale: single;
begin
  AScale := ScreenDpi / C_DPI;
  Result := ((C_DPI / C_MM_PER_INCH) * AValue) * AScale;
end;

function PixelsToMM(AValue: Extended) : single;
begin
  Result := AValue / (ScreenDpi / C_MM_PER_INCH);
end;

function MMtoMu(AValue: Extended) : integer;
begin
  result := Trunc(AValue * 1000);
end;

function MuToMM(AValue: integer) : Extended;
begin
  result := AValue / 1000;
end;

function MuToPixels(AValue: integer) : integer;
begin
  result := MMtoPixels(MuToMM(AValue));
end;

function PixelsToMu(AValue: integer): integer;
begin
  result := Trunc(PixelsToMM(AValue) * 1000);
end;

{ TDesignBoxItemInterface }

function TDesignBoxItemInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TDesignBoxItemInterface._AddRef: Integer;
begin
  Result := -1;
end;

function TDesignBoxItemInterface._Release: Integer;
begin
  Result := -1;
end;

{ TDesignBox }

procedure TDesignBox.AlignItems(const aAlignment: TItemAlignment);
begin
  SelectedItems.AlignItems(aAlignment);
  Redraw;
end;

procedure TDesignBox.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDesignBox.BringForwards;
var
  AItem: TDesignBoxBaseItem;
  AIndex: integer;
begin
  for AItem in SelectedItems do
  begin
    AIndex := FItems.IndexOf(AItem);
    if AIndex < FItems.Count-1 then
      FItems.Move(AIndex, AIndex+1);
  end;
  Redraw;
end;

procedure TDesignBox.BringToFront;
var
  AItem: TDesignBoxBaseItem;
begin
  for AItem in SelectedItems do
  begin
    FItems.Extract(AItem);
    FItems.Add(AItem);
  end;
  Redraw;
end;

function TDesignBox.CanRedo: boolean;
begin
  result := FUndoList.CanRedo;
end;

function TDesignBox.CanUndo: boolean;
begin
  result := fUndoList.CanUndo;
end;

procedure TDesignBox.Clear;
begin
  FItems.Clear;
  FCanvas.Background := nil;
  Redraw;
end;

procedure TDesignBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TDesignBox.CMVisibleChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  if not (csDestroying in ComponentState) then
    for I := 0 to DockClientCount - 1 do
      DockClients[I].Visible := Visible;
end;

constructor TDesignBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csPannable, csGestures];
  AutoScroll := True;

  HorzScrollBar.Tracking := True;
  VertScrollBar.Tracking := True;
  fDrawClass := TDesignBoxItemRectangle; // default
  FCanvas := TDesignBoxCanvas.Create(Self);
  //FBuffer := TBitmap.Create;
  FItems := TDesignBoxItemList.Create(Self);
  FSelectedItems := TDesignBoxItemList.Create(Self);
  FUndoList := TDesignUndoList.Create(Self);
  FGridOptions := TDesignBoxGridOptions.Create(Self);
  FRulerOptions := TDesignBoxRulerOptions.Create(Self);
  fMouseInteraction := TDesignBoxMouseInteractionOptions.Create;

  FSelectedItems.OwnsObjects := False;
  FPageSizeMM.Width := 100;
  FPageSizeMM.Height := 100;
  FBackgroundColor := clSilver;
  FPageColor := clWhite;
  FBorderStyle := bsSingle;

  DoubleBuffered := True;
  FMode := dbmSelect;

  FUpdateCount := 0;

  FCanvas.Left := 0;
  FCanvas.Top := 0;

  FCanvas.Parent := Self;
end;


procedure TDesignBox.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

destructor TDesignBox.Destroy;
begin
  FCanvas.Free;
  FItems.Free;
  //FBuffer.Free;
  FSelectedItems.Free;
  FUndoList.Free;
  FGridOptions.Free;
  FRulerOptions.Free;
  fMouseInteraction.Free;
  inherited;
end;
                    {
function TDesignBox.GetFont: TFont;
begin
  result := FCanvas.Font;
end;                 }

function TDesignBox.GetPageHeightMM: integer;
begin
  Result := FPageSizeMM.Height;
end;

function TDesignBox.GetPageWidthMM: integer;
begin
  Result := FPageSizeMM.Width;
end;

function TDesignBox.GetSelectedItems: TDesignBoxItemList;
var
  AItem: TDesignBoxBaseItem;
begin
  Result := FSelectedItems;
  FSelectedItems.Clear;
  for AItem in FItems do
  begin
    if AItem.Selected then
      Result.Add(AItem);
  end;
end;

procedure TDesignBox.Loaded;
begin
  inherited;
  ResizeCanvas;
  SaveSnapShot(True);
  Redraw;
end;

procedure TDesignBox.LoadFromFile(AFilename: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFilename);
    AStream.Position := 0;
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TDesignBox.LoadFromStream(AStream: TStream);
var
  AStringStream: TStringStream;
begin
  AStringStream := TStringStream.Create;
  try
    AStringStream.CopyFrom(AStream, AStream.Size);
    LoadFromJson(AStringStream.DataString);
  finally
    AStringStream.Free;
  end;
end;

procedure TDesignBox.PaintWindow(DC: HDC);
begin
  //  Do nothing
end;

procedure TDesignBox.LoadFromJson(AJsonData: string);
var
  AJson: TJSONObject;
begin
  AJson := nil;
  BeginUpdate;
  try
    AJson := TJsonObject.ParseJSONValue(AJsonData) as TJSONObject;
    fRulerOptions.LoadFromJsonObject(AJson);
    if assigned(AJson.Values['pageWidthMM']) then
      PageWidthMM := TJsonNumber(AJson.values['pageWidthMM']).asInt;
    if assigned(AJson.Values['pageHeightMM']) then
      PageHeightMM := TJsonNumber(AJson.values['pageHeightMM']).asInt;
    if assigned(AJson.Values['backgroundColor']) then
      fBackgroundColor := StringToColor(AJson.values['backgroundColor'].value);
    if assigned(AJson.Values['pageColor']) then
      fPageColor := StringToColor(AJson.values['pageColor'].value);
    if assigned(AJson.Values['mode']) then
      fMode := TRttiEnumerationType.GetValue<TDesignBoxMode>(AJson.Values['mode'].value);
    if assigned(AJson.Values['drawClass']) then
      fDrawClass := TDesignBoxBaseItemClass(GetClass(AJson.Values['drawClass'].Value));
    FCanvas.LoadFromJson(AJson);
    FItems.LoadFromJson(AJson);
  finally
    EndUpdate;
    AJson.Free;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TDesignBoxCanvas.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AItem: TDesignBoxBaseItem;
  ADeselectOthers: Boolean;
begin
  FDesignBox.FRulerOptions.fRulerWidthPx := FDesignBox.fPageOffset.X;
  FDesignBox.FRulerOptions.fRulerHeightPx := FDesignBox.fPageOffset.Y;

  X := X - FDesignBox.FPageOffset.X;
  Y := Y - FDesignBox.FPageOffset.Y;

  ADeselectOthers := True;

  FMouseDownPos := Point(X, Y);

  AItem := FDesignBox.FItems.ItemAtPos(x, y);

  if (ssShift in Shift) then ADeselectOthers := False;
  if (AItem <> nil) and (AItem.Selected) then ADeselectOthers := False;

  if ADeselectOthers then FDesignBox.FItems.DeselectAll;

  if (AItem <> nil) and (canSelect in AItem.Options) and (AItem.Visible) then
  begin
    case (ssShift in Shift) of
      True: AItem.Selected := not AItem.Selected;
      False: AItem.Selected := True and (canSelect in AItem.Options);
    end;
  end;

  FDragging := (FDesignBox.MouseInteraction.dragSelect);
  FMouseXY := Point(X, Y);

  if (AItem <> nil) and (AItem.Selected) and (Assigned(FDesignBox.FOnSelectItem)) then
    FDesignBox.FOnSelectItem(Self, AItem);

  if FDesignBox.SelectedItems.Count > 0 then
    FDesignBox.FMode := dbmSelect;
  inherited;
end;

procedure TDesignBoxCanvas.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AItem: TDesignBoxBaseItem;
  ASelectRect: TRect;
  ADragOffset: TPoint;
  Xsnap: integer;
  YSnap: integer;
  AGridPixels: Extended;
begin
  inherited;
  X := X - FDesignBox.FPageOffset.X;
  Y := Y - FDesignBox.FPageOffset.Y;
  FMouseXY := Point(X, Y);

  ADragOffset := Point(0, 0);
  if FDragging then
    ADragOffset := Point((X - FMouseDownPos.X),
                         (Y - FMouseDownPos.Y));


  if (FDragging) and (FDesignBox.FItems.SelectedCount > 0) then
  begin
    for AItem in FDesignBox.FItems do
    begin
      if AItem.Selected and (canMove in AItem.Options) then
        AItem.FDrawOffset := ADragOffset;
    end;

    if FDesignBox.FGridOptions.SnapToGrid then
    begin
      ASelectRect := FDesignBox.SelectedItems.BoundsRect;
      AGridPixels :=  MmToPixelsF(FDesignBox.FGridOptions.SizeMm);
      XSnap := Round((Round(ASelectRect.Left / AGridPixels) * AGridPixels) - ASelectRect.Left);
      YSnap := Round((Round(ASelectRect.Top / AGridPixels) * AGridPixels) - ASelectRect.Top);
      for AItem in FDesignBox.FItems do
      begin
        if AItem.Selected and (canMove in AItem.Options) then
          AItem.FDrawOffset.Offset(xsnap, ySnap);
      end;
    end;
    FDesignBox.Redraw;
  end
  else
    Invalidate;
end;

procedure TDesignBoxCanvas.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ADragArea: TRect;
  AItem: TDesignBoxBaseItem;
  ARect: TRectF;
  ANewItem : TDesignBoxBaseItem;
begin
  inherited;
  X := X - FDesignBox.FPageOffset.X;
  Y := Y - FDesignBox.FPageOffset.Y;

  if (FDragging) and (FDesignBox.FItems.SelectedCount = 0) and (FDesignBox.Mode = dbmSelect) then
  begin
    ADragArea := Rect(Min(FMouseDownPos.X, X), Min(FMouseDownPos.Y, Y), Max(FMouseDownPos.X, X), Max(FMouseDownPos.Y, Y));
    if (ADragArea.Width > C_DRAG_THRESHOLD) and (ADragArea.Height > C_DRAG_THRESHOLD) then
      for AItem in FDesignBox.FItems do
      begin
        AItem.Selected := AItem.RectsIntersect(ADragArea) and (canSelect in AItem.Options) and (Aitem.Visible);
      end;
  end
  else
  begin
    for AItem in FDesignBox.SelectedItems do
    begin
      AItem.UpdateToDragPosition;
    end;
  end;
  FDragging := False;

  ARect := RectF(PixelsToMM(FMouseDownPos.X),
                PixelsToMM(FMouseDownPos.Y),
                PixelsToMM(FMouseXY.X),
                PixelsToMM(FMouseXY.Y));
  ARect.NormalizeRect;
  if (FDesignBox.FMode = dbmDraw) then
  begin
    // FCanvas.Rectangle(ARect, 0);
    aNewItem := FDesignBox.fDrawClass.Create(FDesignBox);
    aNewItem.LeftMM := aRect.Left;
    aNewItem.TopMM := aRect.Top;
    aNewItem.WidthMM := aRect.Width;
    aNewItem.HeightMM := aRect.Height;
    if aNewItem is TDesignBoxItemText then
      TDesignBoxItemText(aNewItem).Text := format('Item %d', [Succ(FDesignBox.fItems.Count)]);
    var idx := FDesignBox.fItems.add(aNewItem);
    if assigned(FDesignBox.AfterDrawItem) then
      FDesignBox.AfterDrawItem(self, aNewItem, idx);
  end;

  FDesignBox.Redraw;
  FDesignBox.RecordSnapshot;
end;

procedure TDesignBox.DrawRulers(ACanvas: TCanvas);
var
  aUnit: integer; // if imperial, these are 1/16's of an inch
  aDisplayUnit : integer;
  aPixels: integer;
  AMarkSize: integer;
  tz : TSize;
  RectOrigin: TRect;
  ATopRuler: TRect;
  ALeftRuler: TRect;
  AUnitStr: string;
  LittleMarkFreq : integer;
  BigMarkFreq : integer;
const
  C_BIG_MARK = 8;
  C_LITTLE_MARK = 4;
begin

  case FRulerOptions.MeasurementSystem of
    dbImperial: begin      // 1/16's of an inch
      LittleMarkFreq := 2;
      BigMarkFreq := 16;
    end;
    else begin        // 1mm
      LittleMarkFreq := 2; //mm
      BigMarkFreq := 10;   // mm
    end;
  end;

  ACanvas.Brush.Color := fRulerOptions.BackgroundColor;
  ACanvas.Pen.Style := psClear;

  ATopRuler := Rect(FPageOffset.X, 0, FCanvas.Width, FPageOffset.Y);
  ATopRuler.Offset(0, VertScrollBar.Position);

  ALeftRuler := Rect(0, FPageOffset.Y, FPageOffset.X, FCanvas.Height);
  ALeftRuler.Offset(HorzScrollBar.Position, 0);

  ACanvas.FillRect(ATopRuler);
  ACanvas.FillRect(ALeftRuler);
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := fRulerOptions.ForegroundColor;
  ACanvas.Pen.Mode := pmCopy;

  ACanvas.Polyline([Point(0, ATopRuler.Bottom), Point(FCanvas.Width, ATopRuler.Bottom)]);
  ACanvas.Polyline([Point(ALeftRuler.Right, 0), Point(ALeftRuler.Right, FCanvas.Height)]);

  ACanvas.Font.Assign(FRulerOptions.Font); // likely be the same as the DesignBox, but *could* be different if we allow it
  try
    ACanvas.Font.Color := fRulerOptions.ForegroundColor;
    aUnit := 1;  // either 1mm or 1/16th inch
    aPixels := 0;
    ACanvas.Brush.Style := bsClear;
    while aPixels < ATopRuler.Width do
    begin
      // convert inches units to MM so we can still draw the lines in the right place
      if FRulerOptions.fMeasurementSystem = dbImperial then
      begin
        aPixels := MMtoPixels((aUnit / 16) * 25.4); // convert a 1/16in unit to mm & then to pixels
        aDisplayUnit := aUnit div 16; // display units are inches
      end
      else begin
        aPixels := MMtoPixels(aUnit); // usual mm 2 px conversion
        if fRulerOptions.MeasurementSystem = dbMetricCent then
          aDisplayUnit := aUnit div 10 // centimetres display units
        else
          aDisplayUnit := aUnit;
      end;
      AMarkSize := 0;
      if (aUnit) mod LittleMarkFreq = 0 then AMarkSize := C_LITTLE_MARK;
      if (aUnit mod BigMarkFreq = 0) and (FRulerOptions.fMeasurementSystem = dbImperial) then AMarkSize := C_BIG_MARK;
      if (aUnit) mod BigMarkFreq = 0 then AMarkSize := C_BIG_MARK;
      if AMarkSize > 0 then
      begin
        ACanvas.Polyline([Point(aPixels + ATopRuler.Left, ATopRuler.Bottom-AMarkSize),
                          Point(aPixels + ATopRuler.Left, ATopRuler.Bottom)]);
        if AMarkSize = C_BIG_MARK then
        begin
          tz := ACanvas.TextExtent(aDisplayUnit.ToString);
          ACanvas.TextOut((aPixels + ATopRuler.Left) - (tz.Width div 2), ((ATopRuler.Bottom - tz.Height) - (AMarkSize + 1)), aDisplayUnit.ToString);
        end;
      end;
      Inc(aUnit);
    end;

    aUnit := 1;
    while MmToPixels(aUnit) < ALeftRuler.Height do
    begin
      // convert inches units to MM so we can still draw the lines in the right place
      if FRulerOptions.fMeasurementSystem = dbImperial then
      begin
        aPixels := MMtoPixels((aUnit / 16) * 25.4); // convert a single "unit" to mm
        aDisplayUnit := aUnit div 16;
      end
      else begin
        aPixels := MMtoPixels(aUnit);
        if fRulerOptions.MeasurementSystem = dbMetricCent then
          aDisplayUnit := aUnit div 10
        else
          aDisplayUnit := aUnit;
      end;
      AMarkSize := 0;
      if aUnit mod LittleMarkFreq = 0 then AMarkSize := C_LITTLE_MARK;
      if (aUnit mod BigMarkFreq = 0) and (FRulerOptions.fMeasurementSystem = dbImperial) then AMarkSize := C_BIG_MARK;
      if aUnit mod BigMarkFreq = 0 then AMarkSize := C_BIG_MARK;
      if AMarkSize > 0 then
      begin
        ACanvas.Polyline([Point(ALeftRuler.Right-AMarkSize, aPixels+ALeftRuler.Top),
                          Point(ALeftRuler.Right, aPixels+ALeftRuler.Top)]);
        if AMarkSize = C_BIG_MARK then
        begin
          tz := ACanvas.TextExtent(aDisplayUnit.ToString);
          ACanvas.TextOut((ALeftRuler.Right-tz.width) - (AMarkSize+1), (aPixels+ALeftRuler.Top) - (tz.Height div 2), aDisplayUnit.ToString);
        end;
      end;
      Inc(aUnit);
    end;

    // Draw the Units (or whatever ?)
    ACanvas.Brush.Color := clWhite;
    ACanvas.Pen.Color := clWhite;
    rectOrigin := Rect(ALeftRuler.Left, ATopRuler.Top, ALeftRuler.Right, ATopRuler.Bottom);
    ACanvas.Rectangle(rectOrigin);
    if fRulerOptions.ShowUnits then
    begin
      rectOrigin := Rect(ALeftRuler.Left, ATopRuler.Top, ALeftRuler.Right, ATopRuler.Bottom);
      AUnitStr := fRulerOptions.Units;
      tz := ACanvas.TextExtent(AUnitStr);
      ACanvas.Brush.Style := bsClear;
      ACanvas.TextRect(RectOrigin, AUnitStr, [tfCenter, tfVerticalCenter, tfSingleLine]);
    end;

  finally
    ACanvas.Font.Assign(self.Font);
  end;
end;

procedure TDesignBox.Endupdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      Redraw;
      RecordSnapshot;
    end;
  end;
end;

procedure TDesignBox.DrawGrid(ACanvas: TCanvas);
var
  x, y: single;
  APageWidth, APageHeight: integer;
begin
  APageWidth := MmToPixels(FPageSizeMm.Width);
  APageHeight := MmToPixels(FPageSizeMm.Height);

  ACanvas.Pen.Color := FGridOptions.Color;
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;

  if FGridOptions.Style in [dbgsVert, dbgsBoth] then
  begin
    X := 0;
    while X < FPageSizeMM.Width do
    begin
      ACanvas.Pen.Color := FGridOptions.Color;
      ACanvas.Pen.Mode := pmCopy;
      ACanvas.MoveTo(MmToPixels(x), 0);
      ACanvas.LineTo(MmToPixels(x), APageHeight);
      X := X + FGridOptions.SizeMm;
    end;
  end;

  if FGridOptions.Style in [dbgsHorz, dbgsBoth] then
  begin
    Y := 0;
    while Y < FPageSizeMM.Height do
    begin
      ACanvas.MoveTo(0, MmToPixels(y));
      ACanvas.LineTo(APageWidth, MmToPixels(y));
      Y := Y + FGridOptions.SizeMm;
    end;
  end;

end;

procedure TDesignBoxCanvas.Paint;
var
  ARect: TRect;
begin
  Canvas.Brush.Color := FDesignBox.FBackgroundColor;

  Canvas.FillRect(ClientRect);

  if (FBuffer.Width = 0) then
   FDesignBox.Redraw;

  Canvas.Draw(FDesignBox.FPageOffset.X, FDesignBox.FPageOffset.Y, FBuffer);

  if FDesignBox.FRulerOptions.Visible then
    FDesignBox.DrawRulers(Canvas);

  if (FDragging) and (FDesignBox.FItems.SelectedCount = 0) then
  begin
    ARect := Rect(FMouseDownPos.X+FDesignBox.FPageOffset.X,
                  FMouseDownPos.Y+FDesignBox.FPageOffset.Y,
                  FMouseXY.X+FDesignBox.FPageOffset.X,
                  FMouseXY.Y+FDesignBox.FPageOffset.Y);
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Mode := pmNot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ARect);
  end;
  //Canvas.Brush.Style := bsClear;
  //Canvas.Brush.Color := clDkGray;
  //Canvas.FrameRect(ClientRect);

end;



procedure TDesignBox.RecordSnapshot;
begin
  if FUpdateCount > 0 then
    Exit;
  fUndoList.SaveSnapshot(False);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDesignBox.Redo;
begin
  fUndoList.Redo;
end;

procedure TDesignBox.Redraw;
var
  AItem: TDesignBoxBaseItem;
begin
  if FUpdateCount > 0 then
    Exit;

  ResizeCanvas;
  FCanvas.FBuffer.Canvas.Brush.Color := PageColor;
  FCanvas.FBuffer.Canvas.Brush.Style := bsClear;
  FCanvas.FBuffer.Canvas.Pen.Color := clBlack;
  FCanvas.FBuffer.Canvas.Pen.Mode := pmCopy;
  FCanvas.FBuffer.Canvas.Rectangle(0, 0, FCanvas.FBuffer.Width, FCanvas.FBuffer.Height);

  FCanvas.DrawBackground(Rect(0, 0, FCanvas.FBuffer.Width, FCanvas.FBuffer.Height));

  if FGridOptions.Visible then
    DrawGrid(FCanvas.FBuffer.Canvas);

  if FItems.Count > 0 then
  begin
    for AItem in FItems do
    begin
      if AItem.Visible then
        AItem.PaintToCanvas(FCanvas.FBuffer.Canvas);
      if AItem.Selected then
        AItem.DrawSelectedRect(FCanvas.FBuffer.Canvas);
    end;
  end;
  FCanvas.FBuffer.Canvas.Pen.Style := psSolid;
  FCanvas.FBuffer.Canvas.Brush.Style := bsClear;
  FCanvas.FBuffer.Canvas.Pen.Color := clBlack;
  FCanvas.FBuffer.Canvas.Pen.Mode := pmCopy;
  FCanvas.FBuffer.Canvas.Pen.Width := 1;
  FCanvas.FBuffer.Canvas.Rectangle(0,0, FCanvas.FBuffer.Width, FCanvas.FBuffer.Height);

  Invalidate;
end;

procedure TDesignBox.Resize;
begin
  inherited;
  FCanvas.FBuffer.Width := Width;
  FCanvas.FBuffer.Height := Height;
  Redraw;
  Invalidate;
end;

procedure TDesignBox.ResizeCanvas;
begin
  FCanvas.SetSize(MmToPixels(FPageSizeMM.Width)+1,
                  MmToPixels(FPageSizeMM.Height)+1);
end;

procedure TDesignBox.SaveSnapShot(aForce: boolean);
begin

  if fUpdateCount > 0 then EXIT;

  fUndoList.SaveSnapshot(aForce);
end;

procedure TDesignBox.SaveToFile(AFilename: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveToStream(AStream);
    AStream.SaveToFile(AFilename);
  finally
    AStream.Free;
  end;
end;

procedure TDesignBox.SaveToStream(AStream: TStream);
var
  AJson: TJsonObject;
  AStringStream: TStringStream;

begin
  AJson := TJsonObject.Create;
  try
    FRulerOptions.SaveToJsonObject(AJSon);
    FGridOptions.SaveToJsonObject(AJson);
    AJson.AddPair('pageWidthMM', TJsonNumber.Create(PageWidthMM));
    AJson.AddPair('pageHeightMM', TJsonNumber.Create(PageHeightMM));
    AJson.AddPair('backgroundColor', ColorToString(FBackgroundColor));
    AJson.AddPair('pageColor', ColorToString(fPageColor));
    AJson.AddPair('mode', TRttiEnumerationType.GetName(fMode));
    AJson.AddPair('drawClass', fDrawClass.ClassName);
    FCanvas.SaveToJson(AJson);
    FItems.SaveToJson(AJson);
    AStringStream := TStringStream.Create(AJson.ToJSON);
    try
      AStringStream.Position := 0;
      AStream.CopyFrom(AStringStream, AStringStream.Size);
    finally
      AStringStream.Free;
    end;
  finally
    AJson.Free;
  end;
end;

procedure TDesignBox.SendBackwards;
var
  AItem: TDesignBoxBaseItem;
  AIndex: integer;
  ICount: integer;
  ASelected: TDesignBoxItemList;
begin
  ASelected := SelectedItems;
  for ICount := FItems.Count-1 downto 0 do
  begin
    AItem := ASelected[ICount];
    AIndex := FItems.IndexOf(AItem);
    if AIndex > 0 then
      FItems.Move(AIndex, AIndex-1);
  end;
  Redraw;
end;

procedure TDesignBox.SendToBack;
var
  AItem: TDesignBoxBaseItem;
  ICount: integer;
  ASelected: TDesignBoxItemList;
begin
  ASelected := SelectedItems;
  for ICount := ASelected.Count-1 downto 0 do
  begin
    AItem := ASelected[ICount];
    FItems.Extract(AItem);
    FItems.Insert(0, AItem);
  end;
  Redraw;
end;


procedure TDesignBox.SetBackgroundColor(const Value: TColor);
begin
  fBackgroundColor := Value;
  Redraw;
end;

procedure TDesignBox.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TDesignBox.SetGridOptions(const Value: TDesignBoxGridOptions);
begin
  FGridOptions.Assign(Value);
end;

procedure TDesignBox.SetMouseInteraction(const Value: TDesignBoxMouseInteractionOptions);
begin
  fMouseInteraction := Value;
end;

procedure TDesignBox.SetPageColor(const Value: TColor);
begin
  fPageColor := Value;
  Redraw;
end;

procedure TDesignBox.SetPageHeightMM(const Value: integer);
begin
  FPageSizeMM.Height := Value;
  if assigned(OnResize) then
    OnResize(self);
  Redraw;
end;

procedure TDesignBox.SetPageSize(APageSize: TSize);
begin
  FPageSizeMM.Width := APageSize.Width;
  FPageSizeMM.Height := APageSize.Height;
  if assigned(onResize) then
    OnResize(Self);
  Redraw;
end;

procedure TDesignBox.SetPageWidthMM(const Value: integer);
begin
  FPageSizeMM.Width := Value;
  if assigned(OnResize) then
    OnResize(self);
  Redraw;
end;

procedure TDesignBox.SetPageSize(AWidthMM, AHeightMM: integer);
var
  ASize: TSize;
begin
  ASize.Width := AwidthMM;
  ASize.Height := AHeightMM;
  SetPageSize(ASize);
end;



procedure TDesignBox.SetRulerOptions(const Value: TDesignBoxRulerOptions);
begin
  fRulerOptions.assign(Value);
end;


procedure TDesignBox.Undo;
begin
  fUndoList.Undo;
end;


procedure TDesignBox.WMEraseBackground(var message: TMessage);
begin
  message.Result := 1;
end;

procedure TDesignBox.WMHScroll(var Message: TWMHScroll);
begin
  FCanvas.Invalidate;
  inherited;
end;

procedure TDesignBox.WMNCHitTest(var Message: TWMNCHitTest);
begin
  DefaultHandler(Message);
end;

procedure TDesignBox.WMVScroll(var Message: TWMVScroll);
begin
  FCanvas.Invalidate;
  inherited;
end;

{ TDesignBoxItemText }

constructor TDesignBoxItemText.Create(ADesignBox: TDesignBox);
begin
  inherited Create(ADesignBox);
  FFont := TDesignFont.Create;
  FFont.Assign(ADesignBox.Canvas.Font); //get the current font from DesignBox
  FFont.OnChange := DoFontChange;
end;

destructor TDesignBoxItemText.Destroy;
begin
  inherited;
  FFont.Free;
end;

procedure TDesignBoxItemText.DoFontChange(Sender: TObject);
begin
  Changed;
end;

function TDesignBoxItemText.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TDesignBoxItemText.PaintToCanvas(ACanvas: TCanvas);
var
  r: TRect;
begin
  inherited;
  ACanvas.Font.Assign(FFont);
  // calculate width/height...
  FWidth := PixelsToMu(ACanvas.TextWidth(FText));
  FHeight := PixelsToMu(ACanvas.TextHeight(FText));

  r := RectPixels;
  ACanvas.Rectangle(r.Left, r.Top, r.Right+1, r.Bottom+1);
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(r.Left, r.Top, FText);
end;

procedure TDesignBoxItemText.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  Text := AJson.Values['text'].Value;
  FFont.LoadFromJson(AJson);
end;



procedure TDesignBoxItemText.SaveToJson(AJson: TJsonObject);
begin
  inherited;
  AJson.AddPair('text', Text);
  FFont.SaveToJson(AJson);
end;

procedure TDesignBoxItemText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TDesignBoxItemText.SetText(const AText: string);
begin
  if FText <> AText then
  begin
    FText := AText;
    Changed;
  end;
end;

{ TDesignBoxBaseItem }

procedure TDesignBoxBaseItem.Changed;
begin
  FDesignBox.Redraw;
  if not FDesignBox.Canvas.FDragging then
    FDesignBox.SaveSnapshot(False);
end;

constructor TDesignBoxBaseItem.Create(ADesignBox: TDesignBox);
begin
  inherited Create;
  FDesignBox := ADesignBox;
  FSelected := False;
  FVisible := TRUE; // if false cannot be selected either
  FOptions := [low(TITemOption)..High(TItemOption)];
end;

destructor TDesignBoxBaseItem.Destroy;
begin
  inherited;
end;

procedure TDesignBoxBaseItem.DrawSelectedRect(ACanvas: TCanvas);
var
  ARect: TRect;
begin
  if FSelected and FVisible then
  begin
    ARect := RectPixels;
    InflateRect(ARect, C_SELECTBOX_INFLATE, C_SELECTBOX_INFLATE);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Color := C_HIGHLIGHT_COLOR;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Width := 1;
    ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right + 1, ARect.Bottom + 1);
  end;
end;

procedure TDesignBoxBaseItem.UpdateToDragPosition;
begin
  FPosition.X := FPosition.X + PixelsToMu(FDrawOffset.X);
  FPosition.Y := FPosition.Y + PixelsToMu(FDrawOffset.Y);
  FDrawOffset := Point(0,0);
end;

function TDesignBoxBaseItem.GetCenterPtMm: TPointF;
begin
  Result := RectMM.CenterPoint;
end;

function TDesignBoxBaseItem.GetHeightMm: Extended;
begin
  Result := MuToMM(FHeight);
end;

function TDesignBoxBaseItem.GetLeftMM: Extended;
begin
  Result := MuToMM(FPosition.X);
end;

function TDesignBoxBaseItem.GetTopMM: Extended;
begin
  Result := MuToMM(FPosition.Y);
end;

function TDesignBoxBaseItem.GetWidthMM: Extended;
begin
  Result := MuToMM(FWidth);
end;

function TDesignBoxBaseItem.PointInRgn(x, y: integer): Boolean;
begin
  Result := PtInRect(RectPixels, Point(x,y));
end;

function TDesignBoxBaseItem.asJsonObject: TJsonObject;
begin
  result := TJsonObject.Create;
  SaveToJson(result);
end;

function TDesignBoxBaseItem.BoundsRect: TRect;
begin
  // Stored measurements are Mu (not Pixels) so conversion needed here as BoundsRect is pixels
  Result := Rect(MuToPixels(FPosition.X),
                 MuToPixels(FPosition.Y),
                 MuToPixels(FPosition.X + FWidth),
                 MuToPixels(FPosition.Y + FHeight));
end;

function TDesignBoxBaseItem.RectMM: TRectF;
begin
//  Result := RectPixels; <-- removed this as converting from MM to Px to MM is just asking for rounding trouble
  Result.Left := LeftMM;
  Result.Top := TopMM;
  Result.Right := WidthMM + LeftMM;
  Result.Bottom := HeightMM + TopMM;
  OffsetRect(Result, PixelsToMM(FDrawOffset.X), PixelsToMM(fDrawOffset.Y)); // need to convert FDrawOffset (pixels) to standard measurement unit (MM)
end;

function TDesignBoxBaseItem.RectPixels: TRect;
begin
  Result.Left := MuToPixels(FPosition.X);
  Result.Top := MuToPixels(FPosition.Y);
  Result.Right := MuToPixels(FPosition.X + FWidth);
  Result.Bottom := MuToPixels(FPosition.Y + FHeight);
  OffsetRect(Result, FDrawOffset.X, FDrawOffset.Y);
end;

function TDesignBoxBaseItem.RectsIntersect(ARect: TRect): Boolean;
begin
  ARect.NormalizeRect;
  Result := IntersectRect(ARect, RectPixels);
end;

procedure TDesignBoxBaseItem.LoadFromJson(AJson: TJsonObject);
var
  jOptions: TJsonArray;
  aOption : TItemOption;
  jv: TJsonValue;
begin
  // Stored measurements are Mu, so storing as int not float now
  // -- note property names have changed (add "Mu" suffix)
  { TODO 1 -oSC -Json : to add support for reading x, y, width, height back as pixels for backward compatibility if it's an issue for Graham (its not for me)}
  FPosition.X := TJsonNumber(AJson.Values['xMu']).asInt;
  FPosition.Y := TJsonNumber(AJson.Values['yMu']).asInt;
  FWidth := TJsonNumber(AJson.Values['widthMu']).asInt;
  FHeight := TJsonNumber(AJson.Values['heightMu']).asInt;
  if assigned(aJson.Values['options']) then
  begin
    fOptions := [];
    jOptions := aJson.Values['options'] as TJsonArray;
    for jv in jOptions do
    begin
      if jv is TJSONString then
      begin
        aOption := TRttiEnumerationType.GetValue<TItemOption>(TJsonString(jv).Value);
        include(fOptions, aOption);
      end;
    end;
  end;
  if assigned(aJson.Values['visible']) then
    Visible := TJsonBool(aJson.Values['visible']).AsBoolean;
end;

procedure TDesignBoxBaseItem.SaveToJson(AJson: TJsonObject);
var
  aOption : TItemOption;
  jOptions : TJsonArray;
begin
  // stored measurements are Mu (micrometres), so reading as Int
  AJson.AddPair('obj', ClassName);
  AJson.AddPair('xMu',  TJsonNumber.Create(FPosition.X));
  AJson.AddPair('yMu', TJsonNumber.Create(FPosition.Y));
  AJson.AddPair('widthMu', TJsonNumber.Create(FWidth));
  AJson.AddPair('heightMu',TJsonNumber.Create(FHeight));
  jOptions := TJsonArray.Create;
  for aOption in fOptions do
    jOptions.Add(TRttiEnumerationType.GetName(aOption));
  AJson.AddPair('options', jOptions);
  AJson.AddPair('visible', TJsonBool.Create(Visible));
end;

procedure TDesignBoxBaseItem.SetCenterPtMm(const Value: TPointF);
begin
  LeftMM := Value.X - (WidthMM / 2);
  TopMM := Value.Y - (HeightMM / 2);
  Changed;
end;

procedure TDesignBoxBaseItem.SetHeightMm(const Value: Extended);
begin
  FHeight := MmToMu(Value);
end;

procedure TDesignBoxBaseItem.SetLeftMM(const Value: Extended);
begin
  FPosition.X := MmToMu(Value)
end;

procedure TDesignBoxBaseItem.SetOptions(const Value: TItemOptions);
begin
  fOptions := Value;
end;

procedure TDesignBoxBaseItem.SetSelectedItem(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Changed;
  end;
end;

procedure TDesignBoxBaseItem.SetTopMM(const Value: Extended);
begin
  FPosition.Y := MmToMu(Value)
end;

procedure TDesignBoxBaseItem.SetVisible(const Value: boolean);
begin
  if fVisible <> Value then
  begin
    fVisible := Value;
    if not fVisible then fSelected := false;
    Changed;
  end;
end;

procedure TDesignBoxBaseItem.SetWidthMM(const Value: Extended);
begin
  FWidth := MmToMu(Value)
end;

procedure TDesignBoxBaseItem.SnapToGrid;
var
  AGridMm: integer;
begin
  AGridMm := FDesignBox.GridOptions.SizeMm;
  LeftMM := (Round(LeftMM / AGridMm) * AGridMm);
  TopMM := (Round(TopMM / AGridMm) * AGridMm);
  Changed;
end;

{ TDesignBoxItemList }

function TDesignBoxItemList.BoundsRect: TRect;
var
  AItem: TDesignBoxBaseItem;
  ICount: integer;
begin
  for ICount := 0 to Count-1 do
  begin
    AItem := Items[ICount];
    if ICount = 0 then
      Result := AItem.RectPixels
    else
      Result := Result.Union(Result, AItem.RectPixels);
  end;
end;

function TDesignBoxItemList.BoundsRectMM: TRectF;
var
  AItem: TDesignBoxBaseItem;
  ICount: integer;
begin
  for ICount := 0 to Count-1 do
  begin
    AItem := Items[ICount];
    if ICount = 0 then
      Result := AItem.RectMM
    else
      Result := Result.Union(Result, AItem.RectMM);
  end;
end;


function TDesignBoxItemList.Add(AClass: TDesignBoxBaseItemClass; ABoundsMm: TRectF): TDesignBoxBaseItem;
var
  ABrushIntf: IBrushObject;
  AFontIntf: IFontObject;
  APenIntf: IPenObject;
begin
  Result := AClass.Create(FDesignBox);
  if Supports(Result, IBrushObject, ABrushIntf) then ABrushIntf.Brush.Assign(FDesignBox.Canvas.Brush);
  if Supports(Result, IFontObject, ABrushIntf) then AFontIntf.Font.Assign(FDesignBox.Canvas.Font);
  if Supports(Result, IPenObject, APenIntf) then APenIntf.Pen.Assign(FDesignBox.Canvas.Pen);
  Result := Add(Result, ABoundsMm);
end;

procedure TDesignBoxItemList.AlignItems(const aAlignment: TItemAlignment);
var
  aRect : TRectF;
  AItem: TDesignBoxBaseItem;
begin
  aRect := BoundsRectMM;  //
  for AItem in self do
  begin
    case aAlignment of
      ialLeftSides    : AItem.LeftMM := aRect.Left;
      ialTopSides     : AItem.TopMM  := aRect.Top;
      ialRightSides   : AItem.LeftMM := aRect.Right - aItem.WidthMM;
      ialBottomSides  : Aitem.TopMM  := aRect.Bottom - AItem.HeightMM;
      ialToGrid       : AItem.SnapToGrid;
      ialVertCenters  : AItem.CenterPtMm  := PointF(AItem.CenterPtMm.X, aRect.CenterPoint.Y);
      ialHorzCenters  : AItem.CenterPtMm  := PointF(aRect.CenterPoint.X, AItem.CenterPtMm.Y);
    end;
  end;
end;

function TDesignBoxItemList.Add(AItem: TDesignBoxBaseItem; ABoundsMm: TRectF): TDesignBoxBaseItem;
begin
  Result := AItem;
  Result.LeftMM := ABoundsMm.Left;
  Result.TopMM := ABoundsMm.Top;
  if ABoundsMm.Width > 0 then Result.WidthMM := ABoundsMm.Width;
  if ABoundsMm.Height > 0 then Result.HeightMM := ABoundsMm.Height;
  Add(result);
  FDesignBox.ReDraw;
  FDesignBox.RecordSnapshot;
end;

constructor TDesignBoxItemList.Create(ADesignBox: TDesignBox);
begin
  inherited Create(True);
  FDesignBox := ADesignBox;
end;

procedure TDesignBoxItemList.DeleteSelected;
var
  ICount: integer;
begin
  for ICount := Count-1 downto 0 do
    if Items[ICount].Selected and (canDelete in Items[ICount].Options) then
      Delete(ICount);
  FDesignBox.Redraw;
end;

procedure TDesignBoxItemList.DeselectAll;
var
  AItem: TDesignBoxBaseItem;
begin
  FDesignBox.BeginUpdate;
  try
    for AItem in Self do
      AItem.Selected := False;
  finally
    FDesignBox.Endupdate;
  end;
end;

function TDesignBoxItemList.GetSelectedCount: integer;
var
  AItem: TDesignBoxBaseItem;
begin
  Result := 0;
  for AItem in Self do
    if AItem.Selected then
      Result := Result + 1;
end;

function TDesignBoxItemList.ItemAtPos(x, y: integer): TDesignBoxBaseItem;
var
  AItem: TDesignBoxBaseItem;
begin
  Result := nil;
  for AItem in Self do
  begin
    if AItem.PointInRgn(x, y) then
    begin
      Result := AItem;
    end;
  end;
end;

procedure TDesignBoxItemList.LoadFromJson(AJson: TJsonObject);
var
  AItems: TJsonArray;
  AObj: TJsonObject;
  AObjName: string;
  ICount: integer;
  AItem: TDesignBoxBaseItem;
  AItemClass : TDesignBoxBaseItemClass;
begin
  FDesignBox.BeginUpdate;
  try
    Clear;
    AItems := AJson.Values['items'] as TJSONArray;
    if AItems = nil then
      Exit;
    for ICount := 0 to AItems.Count-1 do
    begin
      AObj := AItems.Items[ICount] as TJSONObject;
      AObjName := AObj.Values['obj'].Value.toLower;
      AItemClass := TDesignBoxBaseItemClass(GetClass(AObjName));
      if AItemClass <> nil then
      begin
        AItem := AItemClass.Create(FDesignBox);
        AItem.LoadFromJson(AObj);
        Add(AItem);
      end;
    end;
  finally
    FDesignBox.Endupdate;
  end;
end;

procedure TDesignBoxItemList.PaintToCanvas(ACanvas: TCanvas);
var
  AItem: TDesignBoxBaseItem;
begin
  for AItem in Self do
  begin
    if AItem.Visible then
      AItem.PaintToCanvas(ACanvas);
  end;
end;

procedure TDesignBoxItemList.SaveToJson(AJson: TJsonObject);
var
  AItems: TJsonArray;
  AObj: TJsonObject;
  AItem: TDesignBoxBaseItem;
begin
  AItems := TJSONArray.Create;// AJson.A['items'];
  AJson.AddPair('items', AItems);
  for AItem in Self do
  begin
    AObj := TJsonObject.Create;
    AItem.SaveToJson(AObj);
    AItems.Add(AObj);
  end;
end;


procedure TDesignBoxItemList.SelectAll;
var
  AItem: TDesignBoxBaseItem;
begin
  FDesignBox.BeginUpdate;
  try
    for AItem in Self do
      if (canSelect in AItem.Options) and (AItem.Visible) then
        AItem.Selected := TRUE;
  finally
    FDesignBox.Endupdate;
  end;
end;

procedure TDesignBoxItemList.SelectItems(AItems: array of TDesignBoxBaseItem; const ADeselectOthers: Boolean = False);
var
  AItem: TDesignBoxBaseItem;
begin
  FDesignBox.BeginUpdate;
  try
    if ADeselectOthers then
      DesignBox.Items.DeselectAll;
    for AItem in AItems do
      if (canSelect in AItem.Options) and (Aitem.Visible) then
        AItem.Selected := True;
  finally
    FDesignBox.Endupdate;
  end;
end;

{ TDesignBoxItemGraphic }

constructor TDesignBoxItemGraphic.Create(ADesignBox: TDesignBox);
begin
  inherited;
  FGraphic := TPicture.Create;
end;

destructor TDesignBoxItemGraphic.Destroy;
begin
  FGraphic.Free;
  inherited;
end;

procedure TDesignBoxItemGraphic.LoadFromJson(AJson: TJsonObject);
var
  AStream: TMemoryStream;
  AEncoded: TStringStream;
  AType: string;
  AGraphic: TGraphic;
  AImgObj: TJSONObject;
begin
  inherited;
  AImgObj := AJson.Values['img'] as TJSONObject;

  AStream := TMemoryStream.Create;
  AEncoded := TStringStream.Create((AImgObj.Values['data'].Value));
  try
    AEncoded.Position := 0;
    TNetEncoding.Base64.Decode(AEncoded, AStream);
    AStream.Position := 0;
    AType := AImgObj.Values['type'].Value.ToLower;
    AGraphic := nil;
    if AType = TPngImage.ClassName.ToLower then AGraphic := TPngImage.Create;
    if AType = TBitmap.ClassName.ToLower then AGraphic := TBitmap.Create;
    if AType = TJPEGImage.ClassName.ToLower then AGraphic := TJPEGImage.Create;
    if AGraphic <> nil then
    begin
      AGraphic.LoadFromStream(AStream);
      FGraphic.Assign(AGraphic);
    end;
  finally
    AStream.Free;
    AEncoded.Free;
  end;
end;

procedure TDesignBoxItemGraphic.PaintToCanvas(ACanvas: TCanvas);
var
  ARect: TRect;
begin
  inherited;
  ARect := RectPixels;
  ACanvas.StretchDraw(ARect, FGraphic.Graphic);
end;

procedure TDesignBoxItemGraphic.SaveToJson(AJson: TJsonObject);
var
  AStream: TMemoryStream;
  AEncoded: TStringStream;
  AImg: TJSONObject;
begin
  inherited;
  AStream := TMemoryStream.Create;
  AEncoded := TStringStream.Create;
  try
    FGraphic.SaveToStream(AStream);
    AStream.Position := 0;
    TNetEncoding.Base64.Encode(AStream, AEncoded);
    AImg := TJsonObject.Create;
    AJson.AddPair('img', AImg);
    AImg.AddPair('type', FGraphic.Graphic.ClassName);
    AImg.AddPair('data', AEncoded.DataString);
  finally
    AStream.Free;
    AEncoded.Free;
  end;
end;

procedure TDesignBoxItemGraphic.SetGraphic(const Value: TPicture);
begin
  FGraphic.Assign(Value);
end;

{ TDesignFont }

procedure TDesignFont.LoadFromJson(AJson: TJsonObject);
var
  AObj: TJSONObject;
begin
  AObj := AJson.Values['font'] as TJSONObject;
  if AObj <> nil then
  begin
    Name := AObj.Values['name'].Value;
    Size := StrToIntDef(AObj.Values['size'].Value, 10);
    Color := StrToIntDef(AObj.Values['color'].Value, 0);
  end;
end;

procedure TDesignFont.SaveToJson(AJson: TJsonObject);
var
  AObj: TJsonObject;
begin
  AObj := TJSONObject.Create;
  AObj.AddPair('name', Name);
  AObj.AddPair('size', Size.ToString);
  AObj.AddPair('color', IntToStr(Color));
  AJson.AddPair('font', AObj);
end;

{ TDesignBoxItemFilledShape }

constructor TDesignBoxItemFilledShape.Create(ADesignBox: TDesignBox);
begin
  inherited;
  FBrush := TDesignBrush.Create;
  FBrush.OnChange := DoBrushChange;
end;

destructor TDesignBoxItemFilledShape.Destroy;
begin
  FBrush.Free;
  inherited;
end;

procedure TDesignBoxItemFilledShape.DoBrushChange(Sender: TObject);
begin
  Changed;
end;

function TDesignBoxItemFilledShape.GetBrush: TBrush;
begin
  Result := FBrush;
end;


procedure TDesignBoxItemFilledShape.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FBrush.LoadFromJson(AJson);
end;

procedure TDesignBoxItemFilledShape.PaintToCanvas(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Brush.Assign(FBrush);
end;

procedure TDesignBoxItemFilledShape.SaveToJson(AJson: TJsonObject);
begin
  inherited;
  FBrush.SaveToJson(AJson);
end;

procedure TDesignBoxItemFilledShape.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

{ TDesignBoxItemSelectable }


constructor TDesignBoxItemSizable.Create(ADesignBox: TDesignBox);
begin
  inherited;
  //
end;

destructor TDesignBoxItemSizable.Destroy;
begin
  //
  inherited;
end;

procedure TDesignBoxItemSizable.DrawSelectedRect(ACanvas: TCanvas);
var
  ii: TDragHandlePos;
  aRect, handleRect : TRect;
  handlePoints : array[TDragHandlePos] of TPoint; // array of the centrepoints of each selectHandle
const
  CH_RADIUS = 4; // 16 pixel square grab handlePoints
begin

  inherited DrawSelectedRect(ACanvas);

  if FSelected and (canSize in fOptions) then
  begin
    aRect := RectPixels;
    InflateRect(aRect, C_SELECTBOX_INFLATE, C_SELECTBOX_INFLATE); // duplicate the inherited version
    ACanvas.Pen.Color := C_HIGHLIGHT_COLOR;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := ACanvas.Pen.Color;
    ACanvas.Pen.Width := 1;
    handlePoints[dhTopLeft]     := aRect.TopLeft;
    handlePoints[dhTopRight]    := Point(aRect.Right, aRect.Top);
    handlePoints[dhBottomRight] := aRect.BottomRight;
    handlePoints[dhBottomLeft]  := Point(aRect.Left, aRect.Bottom);

    for ii := Low(TDragHandlePos) to High(TDragHandlePos) do
    begin
      handleRect := Rect(handlePoints[ii].X - CH_RADIUS, handlePoints[ii].Y - CH_RADIUS, handlePoints[ii].X + CH_RADIUS, handlePoints[ii].Y + CH_RADIUS);
      fSelectHandles[ii] := handleRect; // save for comparison in PointInRgn later
      ACanvas.Rectangle(handleRect);
    end;
  end;
end;

procedure TDesignBoxItemSizable.PaintToCanvas(ACanvas: TCanvas);
begin
  // does nothing. Just prevents errors if inherited called on descendants

end;

procedure TDesignBoxItemSizable.SetCoords(ALeftMM, ATopMM, ARightMM, ABottomMM: single);
begin
  FPosition.X := MMtoMu(ALeftMM);
  FPosition.Y := MMtoMu(ATopMM);
  FWidth := MMtoMu(ARightMM-ALeftMM);
  FHeight := MMtoMu(ABottomMM-ATopMM);
  Changed;
end;

{ TDesignBrush }

procedure TDesignBrush.LoadFromJson(AJson: TJsonObject);
var
  AObj: TJSONObject;
begin
  AObj := AJson.Values['brush'] as TJSONObject;
  if AObj <> nil then
  begin
    Color := StrToIntDef(AObj.Values['color'].Value, 0);
    Style := TBrushStyle(StrToIntDef(AObj.Values['style'].Value, 0));
  end;
end;

procedure TDesignBrush.SaveToJson(AJson: TJsonObject);
var
  AObj: TJSONObject;
begin
  AObj := TJSONObject.Create;
  AObj.AddPair('color', IntToStr(Color));
  AObj.AddPair('style', IntToStr(Ord(Style)));
  AJson.AddPair('brush', AObj);
end;

{ TDesignPen }

procedure TDesignPen.LoadFromJson(AJson: TJsonObject);
var
  AObj: TJSONObject;
begin
  AObj := AJson.Values['pen'] as TJSONObject;
  if AObj <> nil then
  begin
    Color := StrToIntDef(AObj.Values['color'].Value, 0);
    Style := TPenStyle(StrToIntDef(AObj.Values['style'].Value, 0));
    Width := StrToIntDef(AObj.Values['width'].Value, 1);
  end;
end;

procedure TDesignPen.SaveToJson(AJson: TJsonObject);
var
  AObj: TJSONObject;
begin
  AObj := TJSONObject.Create;
  AObj.AddPair('color', IntToStr(Color));
  AObj.AddPair('style', IntToStr(Ord(Style)));
  AObj.AddPair('width', IntToStr(Width));
  AJson.AddPair('pen', AObj);
end;

{ TDesignUndoList }

function TDesignUndoList.GetCanRedo: Boolean;
begin
  Result := FChangesBookmark < FChanges.Count;
end;

function TDesignUndoList.GetCanUndo: Boolean;
begin
  Result := FChangesBookmark > 1;
end;

function TDesignUndoList.GetCount: integer;
begin
  Result := FChanges.Count;
end;

function TDesignUndoList.GetCurrentSnapshot: string;
begin
  Result := '';
  if FChanges.Count > 0 then
    Result := FChanges[FChangesBookmark-1];
end;

procedure TDesignUndoList.Redo;
begin
  if FChangesBookmark < FChanges.Count then
  begin
    Inc(FChangesBookmark);
    FDesignBox.LoadFromJson(GetCurrentSnapshot);
  end;
end;

procedure TDesignUndoList.Undo;
begin
  if FChangesBookmark > 0 then
    Dec(FChangesBookmark);
  FDesignBox.LoadFromJson(GetCurrentSnapshot);
end;

constructor TDesignUndoList.Create(ADesignBox: TDesignBox);
begin
  FChanges := TStringList.Create;
  FChangesBookmark := 0;
  FDesignBox := ADesignBox;
end;

destructor TDesignUndoList.Destroy;
begin
  FChanges.Free;
  inherited;
end;

procedure TDesignUndoList.SaveSnapshot(AForce: Boolean);
var
  AStream: TStringStream;
begin
  if (FDesignBox.FUpdateCount > 0) and (not AForce) then
    Exit;

  while FChanges.Count > FChangesBookmark do
    FChanges.Delete(FChanges.Count-1);

  AStream := TStringStream.Create;
  try
    FDesignBox.SaveToStream(AStream);
    if (FChanges.Count = 0) or (FChanges[FChangesBookmark-1] <> AStream.DataString) then
    begin
      FChanges.Add(AStream.DataString);
      FChangesBookmark := FChanges.Count;
    end;
  finally
    AStream.Free;
  end;
end;

{ TDesignBoxItemEllipse }

procedure TDesignBoxItemEllipse.PaintToCanvas(ACanvas: TCanvas);
var
  ARect: TRect;
begin
  inherited;
  ARect := RectPixels;
  ACanvas.Ellipse(ARect);
end;

{ TDesignBoxItemRectangle }

constructor TDesignBoxItemRectangle.Create(ADesignBox: TDesignBox);
begin
  inherited;
  FRoundnessMm := 0;
end;

procedure TDesignBoxItemRectangle.PaintToCanvas(ACanvas: TCanvas);
var
  r: TRect;
begin
  inherited;
  r := RectPixels;
  ACanvas.RoundRect(Rect(r.Left,
                         r.Top,
                         r.Right+1,
                         r.Bottom+1),
                    MmToPixels(FRoundnessMm),
                    MmToPixels(FRoundnessMm));
end;

procedure TDesignBoxItemRectangle.SetRoundnessMm(const Value: single);
begin
  if FRoundnessMm <> Value then
  begin
    FRoundnessMm := Value;
    Changed;
  end;
end;

{ TDesignBoxGridOptions }

procedure TDesignBoxGridOptions.Assign(Source: TPersistent);
begin
  inherited;
  FVisible := (Source as TDesignBoxGridOptions).FVisible;
  FColor := (Source as TDesignBoxGridOptions).FColor;
  FSizeMm := (Source as TDesignBoxGridOptions).FSizeMm;
  FDesignBox.Invalidate;
end;

constructor TDesignBoxGridOptions.Create(ADesignBox: TDesignBox);
begin
  inherited Create;
  FDesignBox := ADesignBox;
  FVisible := False;
  FColor := $00F7E1CE;
  FSizeMm := 5;
  FSnapToGrid := false;
  FStyle := dbgsBoth;
end;

procedure TDesignBoxGridOptions.LoadFromJsonObject(AJson: TJsonObject);
begin
  if assigned(AJson.Values['gridVisible']) then
    fVisible := TJsonBool(AJson.values['gridVisible']).asBoolean;
  if assigned(AJson.Values['gridSizeMM']) then
    fSizeMM := TJsonNumber(AJson.Values['gridSizeMM']).AsInt;
  if assigned(AJson.Values['gridColor']) then
    fColor:= StringToColor(AJson.Values['gridColor'].value);
  if assigned(AJson.Values['gridSnapTo']) then
    fSnapToGrid := TJsonBool(AJson.values['gridSnapTo']).asBoolean;
end;

function TDesignBoxGridOptions.PixelSize: integer;
begin
  result := MmToPixels(FSizeMm);
end;

procedure TDesignBoxGridOptions.SaveToJsonObject(AJson: TJsonObject);
begin
  AJson.AddPair('gridVisible', TJsonBool.Create(fVisible));
  AJson.AddPair('gridSizeMM', TJsonNumber.Create(fSizeMM));
  AJson.AddPair('gridSnapTo', TJsonBool.Create(fSnapToGrid));
end;

procedure TDesignBoxGridOptions.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FDesignBox.Redraw;
  end;
end;

procedure TDesignBoxGridOptions.SetSizeMm(const Value: integer);
begin
  if FSizeMm <> Value then
  begin
    FSizeMm := Value;
    FDesignBox.Redraw;
  end;
end;

procedure TDesignBoxGridOptions.SetSnapToGrid(const Value: Boolean);
begin
  FSnapToGrid := Value;
end;

procedure TDesignBoxGridOptions.SetStyle(const Value: TDesignBoxGridStyle);
begin
  if fStyle <> Value then
  begin
    fStyle := Value;
    FDesignBox.Redraw;
  end;
end;

procedure TDesignBoxGridOptions.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    FDesignBox.Redraw;
  end;
end;

{ TDesignBoxItemOutlineShape }

constructor TDesignBoxItemOutlineShape.Create(ADesignBox: TDesignBox);
begin
  inherited;
  FPen := TDesignPen.Create;
  FPen.Color := clBlack;
  FPen.Style := psSolid;
  FPen.OnChange := DoPenChange;
end;

destructor TDesignBoxItemOutlineShape.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TDesignBoxItemOutlineShape.DoPenChange(Sender: TObject);
begin
  Changed;
end;

function TDesignBoxItemOutlineShape.GetPen: TPen;
begin
  Result := FPen;
end;

procedure TDesignBoxItemOutlineShape.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  FPen.LoadFromJson(AJson);
end;

procedure TDesignBoxItemOutlineShape.PaintToCanvas(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Pen.Assign(FPen);
end;

procedure TDesignBoxItemOutlineShape.SaveToJson(AJson: TJsonObject);
begin
  inherited;
  FPen.SaveToJson(AJson);
end;

procedure TDesignBoxItemOutlineShape.SetPen(const Value: TPen);
begin
  fPen.Assign(Value);
end;

{ TDesignBoxRulerOptions }

procedure TDesignBoxRulerOptions.Assign(Source: TPersistent);
begin
  inherited;
  FVisible := (Source as TDesignBoxRulerOptions).Visible;
  FForegroundColor := (Source as TDesignBoxRulerOptions).ForegroundColor;
  FBackgroundColor := (Source as TDesignBoxRulerOptions).BackgroundColor;
  fRulerWidthPx := (Source as TDesignBoxRulerOptions).WidthPx;
  fRulerHeightPx := (Source as TDesignBoxRulerOptions).HeightPx;
  FDesignBox.Invalidate;
end;

constructor TDesignBoxRulerOptions.Create(ADesignBox: TDesignBox);
begin
  inherited Create;
  fDesignBox := aDesignBox;
  fBackgroundColor := clWhite;
  fForegroundColor := clBlack;
  fVisible := true;
  fShowUnits := false; // backward compatibility
  fFont := TDesignFont.Create;
  fFont.assign(ADesignBox.Font);
  fFont.OnChange := internalHandleRulerFontChanged;
  fMeasurementSystem := dbMetric;
  fUnits := UNIT_NAMES[FMeasurementSystem];
end;

destructor TDesignBoxRulerOptions.Destroy;
begin
  fFont.free;
  inherited;
end;

function TDesignBoxRulerOptions.GetHeightPx: integer;
begin
  result := fRulerHeightPx;
end;

function TDesignBoxRulerOptions.GetWidthPx: integer;
begin
  result := fRulerWidthPx;
end;

procedure TDesignBoxRulerOptions.LoadFromJsonObject(AJson: TJsonObject);
begin
  if assigned(AJson.Values['rulersVisible']) then
    fVisible := TJsonBool(AJson.values['rulersVisible']).asBoolean;
  if assigned(AJson.Values['rulerForegroundColor']) then
    fForegroundColor := StringToColor(AJson.Values['rulerForegroundColor'].value);
  if assigned(AJson.Values['rulerBackgroundColor']) then
    fBackgroundColor := StringToColor(AJson.Values['rulerBackgroundColor'].value);
  if assigned(AJson.Values['rulerShowUnits']) then
    fShowUnits := TJsonBool(aJson.Values['rulerShowUnits']).AsBoolean;
  if assigned(AJson.Values['rulerUnits']) then
    fUnits := AJson.Values['rulerUnits'].value;
  if assigned(AJson.Values['rulerFont']) then
    FFont.LoadFromJson(TJsonObject(AJson.Values['rulerFont']));
  if assigned(AJson.Values['measurementSystem']) then
    fMeasurementSystem := TRttiEnumerationType.GetValue<TDesignMeasurementSystem>(AJson.Values['measurementSystem'].value);
end;

procedure TDesignBoxRulerOptions.internalHandleRulerFontChanged(Sender: TObject);
begin
  fDesignBox.Invalidate;
end;

procedure TDesignBoxRulerOptions.SaveToJsonObject(AJson: TJsonObject);
begin
  AJson.AddPair('rulersVisible', TJsonBool.Create(fVisible));
  AJson.AddPair('rulerForegroundColor', ColorToString(fForegroundColor));
  AJson.AddPair('rulerBackgroundColor', ColorToString(fBackgroundColor));
  AJson.AddPair('rulerShowUnits', TJsonBool.Create(fShowUnits));
  AJson.AddPair('rulerUnits', fUnits);
  AJson.AddPair('measurementSystem', TRttiEnumerationType.GetName(fMeasurementSystem));
  var JFont := TJsonObject.Create;
  fFont.SaveToJson(JFont);
  AJson.AddPair('rulerFont', JFont);
end;

procedure TDesignBoxRulerOptions.SetBackgroundColor(const Value: TColor);
begin
  if fBackgroundColor <> Value then
  begin
    fBackgroundColor := Value;
    fDesignBox.Invalidate;
  end;
end;

procedure TDesignBoxRulerOptions.SetFont(const Value: TDesignFont);
begin
  fFont.Assign(Value);
end;

procedure TDesignBoxRulerOptions.SetForegroundColor(const Value: TColor);
begin
  if fForegroundColor <> Value then
  begin
    fForegroundColor := Value;
    fDesignBox.Invalidate;
  end;
end;

procedure TDesignBoxRulerOptions.SetMeasurementSystem(const Value: TDesignMeasurementSystem);
begin
  if (fMeasurementSystem <> Value) then
  begin
    fMeasurementSystem := Value;
    fUnits := UNIT_NAMES[fMeasurementSystem];
    fDesignBox.Invalidate;
  end;
end;

procedure TDesignBoxRulerOptions.SetShowUnits(const Value: boolean);
begin
  if fShowUnits <> Value then
  begin
    fShowUnits := Value;
    fDesignBox.Invalidate;
  end;
end;

procedure TDesignBoxRulerOptions.SetUnits(const Value: string);
begin
  if fUnits <> Value then
  begin
    fUnits := Value;
    fDesignBox.Invalidate;
  end;
end;

procedure TDesignBoxRulerOptions.SetVisible(const Value: boolean);
begin
  if fVisible <> Value then
  begin
    fVisible := Value;
    fDesignBox.Redraw; // needed for SetSize to remove fPageOffsets if no rulers
  end;
end;

{ TDesignBoxCanvas }

function TDesignBoxCanvas.TextOut(ALeftMM, ATopMM: single;
  AText: string): TDesignBoxItemText;
begin
  BeginUpdate;
  try
    if Trim(AText) = ''  then
      AText := '<empty>';
    Result := TDesignBoxItemText.Create(FDesignBox);
    Result.LeftMM := ALeftMM;
    Result.TopMM := ATopMM;
    Result.Text := AText;
    Result.Brush.Assign(FBrush);
    Result.Font.Assign(FFont);
    FDesignBox.Items.Add(Result);
    FDesignBox.Redraw;
  finally
    EndUpdate;
  end;
end;

function TDesignBoxCanvas.Draw(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
begin
  BeginUpdate;
  try
    Result := StretchDraw(RectF(ALeftMM,
                          ATopMM,
                          ALeftMM + PixelsToMM(AGraphic.Width),
                          ATopMM + PixelsToMM(AGraphic.Height)),
                          AGraphic);
  finally
    EndUpdate;
  end;
end;

procedure TDesignBoxCanvas.DrawBackground(const aRect: TRect);
var
  imageRect: TRect;
  aBgSize: TSize;
  aBufferSize: TSize;
begin
  if assigned(fBackground.Graphic) and not fBackground.Graphic.Empty then
  begin
    aBgSize := TSize.Create(fBackground.Graphic.Width, fBackground.Graphic.Height);
    aBufferSize := TSize.Create(fBuffer.Width, fBuffer.Height);
    case fBackgroundPos of
      bgStretch :     imageRect := GetDestRect(aBgSize, aBufferSize, True, false, false);
      bgStretchProp:  imageRect := GetDestRect(aBgSize, aBufferSize, True, True, True);
      bgCentre:       imageRect := GetDestRect(TSize.Create(fBackground.Graphic.Width, fBackground.Graphic.Height), TSize.Create(fBuffer.Width, fBuffer.Height), False, False, True);
    end;
    FBuffer.Canvas.StretchDraw(imageRect, fBackground.Graphic);
  end;

end;

function TDesignBoxCanvas.GetDestRect(aOriginalSize: TSize; aClientSize: TSize; Stretch, Proportional, Center: boolean): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := aOriginalSize.Width;
  h := aOriginalSize.Height;
  cw := aClientSize.Width;
  ch := aClientSize.Height;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  Result.Left := 0;
  Result.Top := 0;
  result.Right := w;
  Result.Bottom := h;

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;

procedure TDesignBoxCanvas.LoadFromJson(const aJson: TJsonObject);
begin
  LoadBackgroundFromJson(aJson);
end;

procedure TDesignBoxCanvas.LoadBackgroundFromJson(const aJson: TJsonObject);
var
  AStream: TMemoryStream;
  AEncoded: TStringStream;
  AType: string;
  AGraphic: TGraphic;
  jBackgroundImage: TJSONObject;
begin
  if assigned(AJson.Values['background']) then
  begin
    jBackgroundImage := AJson.Values['background'] as TJSONObject;

    AStream := TMemoryStream.Create;
    AEncoded := TStringStream.Create((jBackgroundImage.Values['data'].Value));
    try
      AEncoded.Position := 0;
      TNetEncoding.Base64.Decode(AEncoded, AStream);
      AStream.Position := 0;
      AType := jBackgroundImage.Values['type'].Value.ToLower;
      AGraphic := nil;
      if AType = TPngImage.ClassName.ToLower then AGraphic := TPngImage.Create;
      if AType = TBitmap.ClassName.ToLower then AGraphic := TBitmap.Create;
      if AType = TJPEGImage.ClassName.ToLower then AGraphic := TJPEGImage.Create;
      if AGraphic <> nil then
      begin
        AGraphic.LoadFromStream(AStream);
        FBackground.Assign(AGraphic);
      end;
    finally
      AStream.Free;
      AEncoded.Free;
    end;
  end;
end;

function TDesignBoxCanvas.StretchDraw(ARect: TRectF; AGraphic: TGraphic): TDesignBoxItemGraphic;
begin
  BeginUpdate;
  try
    Result := TDesignBoxItemGraphic.Create(FDesignBox);
    Result.Graphic.Assign(AGraphic);
    FDesignBox.Items.Add(Result, ARect);
  finally
    EndUpdate;
  end;
end;

function TDesignBoxCanvas.Ellipse(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemEllipse;
begin
  Result := Ellipse(ALeftMM, ATopMM, ARightMM, ABottomMM);
end;

procedure TDesignBoxCanvas.EndUpdate;
begin
  FDesignBox.Endupdate;
end;

function TDesignBoxCanvas.Ellipse(ABoundsMm: TRectF): TDesignBoxItemEllipse;
begin
  Result := FDesignBox.Items.Add(TDesignBoxItemEllipse, ABoundsMm) as TDesignBoxItemEllipse;
end;

function TDesignBoxCanvas.MeasureText(const AText: string): TSizeF;
begin
  Result := FBuffer.Canvas.TextExtent(aText);
  Result.cx := PixelsToMM(Result.cx);
  Result.cy := PixelsToMM(Result.cy);
end;

function TDesignBoxCanvas.Rectangle(ALeftMM, ATopMM, ARightMM,
  ABottomMM: single; const ARoundnessMm: single): TDesignBoxItemRectangle;
begin
  Result := Rectangle(RectF(ALeftMM, ATopMM, ARightMM, ABottomMM), ARoundnessMm);
end;

function TDesignBoxCanvas.Rectangle(ABoundsMm: TRectF; const ARoundnessMm: single = 0): TDesignBoxItemRectangle;
begin
  BeginUpdate;
  try
    Result := FDesignBox.Items.Add(TDesignBoxItemRectangle, ABoundsMm) as TDesignBoxItemRectangle;
    Result.RoundnessMm := ARoundnessMm;
  finally
    EndUpdate;
  end;
end;

procedure TDesignBoxCanvas.BeginUpdate;
begin
  FDesignBox.BeginUpdate;
end;

constructor TDesignBoxCanvas.Create(AOwner: TComponent);
begin
  inherited;
  Width := 50;
  Height := 50;
  FBuffer := TBitmap.Create;
  FDesignBox := (AOwner as TDesignBox);
  FBackground := TPicture.Create;
  FBackgroundPos := bgCentre;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FPen := TPen.Create;
  FBrush.OnChange := OnBrushChanged;
  FFont.OnChange := OnFontChanged;;
  FPen.OnChange := OnPenChanged;
end;

function TDesignBoxCanvas.TextExtent(const AText: string): TSizeF;
begin
  Result := FBuffer.Canvas.TextExtent(AText);
  Result.cx := PixelsToMM(Result.cx);
  Result.cy := PixelsToMM(Result.cy);
end;

function TDesignBoxCanvas.TextWidth(const AText: string): single;
begin
  Result := PixelsToMM(FBuffer.Canvas.TextWidth(AText));
end;

procedure TDesignBoxCanvas.WMEraseBackground(var message: TMessage);
begin
  message.Result := 1;
end;

function TDesignBoxCanvas.TextHeight(const AText: string): single;
begin
  Result := PixelsToMM(FBuffer.Canvas.TextHeight(AText));
end;

destructor TDesignBoxCanvas.Destroy;
begin
  FBuffer.Free;
  FBackground.Free;
  FBrush.Free;
  FFont.Free;
  FPen.Free;
  inherited;
end;

procedure TDesignBoxCanvas.OnBrushChanged(Sender: TObject);
var
  AItem: TDesignBoxBaseItem;
  AIntf: IBrushObject;
begin
  for AItem in FDesignBox.SelectedItems do
  begin
    if Supports(AItem, IBrushObject, AIntf) then
      AIntf.Brush.Assign(FBrush);
  end;
end;

procedure TDesignBoxCanvas.OnFontChanged(Sender: TObject);
var
  AItem: TDesignBoxBaseItem;
  AIntf: IFontObject;
begin
  for AItem in FDesignBox.SelectedItems do
  begin
    if Supports(AItem, IFontObject, AIntf) then
      AIntf.Font.Assign(FFont);
  end;
end;

procedure TDesignBoxCanvas.OnPenChanged(Sender: TObject);
var
  AItem: TDesignBoxBaseItem;
  AIntf: IPenObject;
begin
  for AItem in FDesignBox.SelectedItems do
  begin
    if Supports(AItem, IPenObject, AIntf) then
      AIntf.Pen.Assign(FPen);
  end;
end;

procedure TDesignBoxCanvas.SaveToJson(const aJson: TJsonObject);
begin
  SaveBackgroundToJson(aJson);
end;

procedure TDesignBoxCanvas.SaveBackgroundToJson(const aJson: TJsonObject);
var
  aStream, aEncoded: TStream;
  jBackgroundImage: TJsonObject;
begin
  if assigned(fBackground.Graphic) and (not fBackground.Graphic.Empty) then
  begin
    AStream := TMemoryStream.Create;
    AEncoded := TStringStream.Create;
    try
      FBackground.SaveToStream(AStream);
      AStream.Position := 0;
      TNetEncoding.Base64.Encode(AStream, AEncoded);
      jBackgroundImage := TJsonObject.Create;
      AJson.AddPair('background', jBackgroundImage);
      // match the storage format of the TDesignBoxGraphicItem
      jBackgroundImage.AddPair('type', FBackground.Graphic.ClassName);
      jBackgroundImage.AddPair('data', TStringStream(AEncoded).DataString);
    finally
      AStream.Free;
      AEncoded.Free;
    end;
  end;
end;

procedure TDesignBoxCanvas.SetBackground(const Value: TPicture);
begin
  fBackground.Assign(Value);
  FDesignBox.Redraw;
end;

procedure TDesignBoxCanvas.SetBackgroundPos(const Value: TDesignBoxBackgroundPosition);
begin
  if fBackgroundPos <> Value then
  begin
    fBackgroundPos := Value;
    FDesignBox.Redraw;
  end;
end;

procedure TDesignBoxCanvas.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TDesignBoxCanvas.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TDesignBoxCanvas.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
end;


procedure TDesignBoxCanvas.SetSize(w, h: integer);
var
  ARulerWidth: integer;
begin
  fBuffer.Canvas.Font := fDesignBox.RulerOptions.Font;
  try
    aRulerWidth := FBuffer.Canvas.TextWidth('9999') + 6;
  finally
    fBuffer.Canvas.Font := fDesignBox.Font;
  end;

  case FDesignBox.RulerOptions.Visible of
    True: FDesignBox.FPageOffset := Point(aRulerWidth, aRulerWidth); // slightly narrower
    False: FDesignBox.FPageOffset := Point(0, 0);
  end;

  Width := Max(w + FDesignBox.FPageOffset.x, FDesignBox.ClientWidth);
  Height := Max(h + FDesignBox.FPageOffset.y, FDesignBox.ClientHeight);


  FBuffer.SetSize(w,h);
  FBuffer.Canvas.Brush.Color := FDesignBox.FPageColor;
  FBuffer.Canvas.FillRect(Rect(0,0,w, h));

end;

{ Export }

procedure TDesignBox.SaveAsJpeg(const aFilename: string; const aCompressionQuality: TJPEGQUalityRange = 100; const aGrayscale: boolean = false);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFilename, fmCreate);
  try
    SaveAsJpeg(aFileStream, aCompressionQuality, aGrayscale);
  finally
    aFileStream.Free;
  end;
end;

procedure TDesignBox.SaveAsBitmap(const AFilename: string);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveAsBitmap(AStream);
    AStream.Position := 0;
    AStream.SaveToFile(AFilename);
  finally
    AStream.Free;
  end;
end;

procedure TDesignBox.SaveAsBitmap(const ABitmap: TBitmap);
begin
  ABitmap.SetSize(MmToPixels(FPageSizeMM.cx), MmToPixels(FPageSizeMM.cy));
  FItems.PaintToCanvas(ABitmap.Canvas);
end;

procedure TDesignBox.SaveAsBitmap(const AStream: TStream);
var
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    SaveAsBitmap(ABmp);
    ABmp.SaveToStream(AStream);
  finally
    ABmp.Free;
  end;
end;

procedure TDesignBox.SaveAsJpeg(const aStream: TStream; const aCompressionQuality: TJPEGQUalityRange = 100; const aGrayscale: boolean = false);
var
  ABmp: TBitmap;
  AJpeg: TJPEGImage;
begin
  ABmp := TBitmap.Create;
  AJpeg := TJPEGImage.Create;
  try
    SaveAsBitmap(ABmp);
    aJpeg.assign(ABmp);
    aJpeg.CompressionQuality := aCompressionQuality;
    case aGrayScale of
      True: aJpeg.PixelFormat := jf8Bit; // grayscale JPEGs only need 8-bit
      False: aJpeg.PixelFormat := jf24bit;
    end;
    aJpeg.Grayscale := aGrayscale;
    aJpeg.Compress;
    aStream.Seek(0, soFromBeginning);
    aJpeg.SaveToStream(aStream);
  finally
    ABmp.Free;
    AJpeg.Free;
  end;

end;

procedure TDesignBox.SaveAsPNG(const aStream: TStream; const aTransparent: boolean);
var
  ABmp: TBitmap;
  APng: TPngImage;
begin
  ABmp := TBitmap.Create;
  APng := TPngImage.Create;
  try
    ABmp.Canvas.Brush.Color := clWhite;
    ABmp.SetSize(MmToPixels(FPageSizeMM.cx), MmToPixels(FPageSizeMM.cy));
    SaveAsBitmap(ABmp);
    ABmp.Transparent := True;
    APng.Assign(ABmp);
    APng.SaveToStream(AStream);
  finally
    ABmp.Free;
    APng.Free;
  end;
end;

procedure TDesignBox.SaveAsPNG(const aFilename: string; const aTransparent: boolean);
var
  aFileStream: TFileStream;
begin
  aFileStream := TFileStream.Create(aFilename, fmCreate);
  try
    SaveAsPNG(aFileStream, aTransparent);
  finally
    aFileStream.free;
  end;
end;


{ TDesignBoxMouseInteractionOptions }

constructor TDesignBoxMouseInteractionOptions.Create;
begin
  inherited Create;
  fDragSelect := TRUE;
end;

procedure TDesignBoxMouseInteractionOptions.SetDragSelect(const Value: boolean);
begin
  fDragSelect := Value;
end;

initialization
  RegisterClass(TDesignBoxItemText);
  RegisterClass(TDesignBoxItemRectangle);
  RegisterClass(TDesignBoxItemEllipse);
  RegisterClass(TDesignBoxItemGraphic);

finalization
  UnRegisterClass(TDesignBoxItemText);
  UnRegisterClass(TDesignBoxItemRectangle);
  UnRegisterClass(TDesignBoxItemEllipse);
  UnRegisterClass(TDesignBoxItemGraphic);

end.
