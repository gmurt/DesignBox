unit DesignBox;

{ DONE : Grid not being drawn correctly - set to 20mm and it's drawing @ 25mm - https://i.imgur.com/bcM8KiS.png - appears to be because MMtoPixels using Screen DPI whereas others are using fixed 96 dpi }
{ DONE : Snap to Grid not working as expected }
{ DONE : High DPI support -- hard coded "96" needs to be Monitor.PixelsPerInch
{ DONE : Undo goes back "too far" (UndoList[-1] ?) and then won't redo }
{ DONE : Drag to select, followed by a single click off-object (to de-select) erases the grid }

interface

uses
  Windows, Messages, System.SysUtils, System.Classes, Vcl.Controls, System.Types, System.Generics.Collections,
  vcl.Graphics, System.Generics.Defaults, System.Json;

type
  TDesignBoxMode = (dbmSelect, dbmDrawRect, dbmDrawEllipse);

  TItemOption = (canSize, canMove, canDelete, canSelect);
  TItemOptions = set of TItemOption;

  TDesignBox = class;
  TDesignBoxBaseItem = class;

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
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCurrentSnapshot: string;
    procedure SaveSnapshot(AForce: Boolean);
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    destructor Destroy; override;
    procedure Undo;
    procedure Redo;
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
  protected
    function RectPixels: TRect; virtual;
    function RectMM: TRectF;
    function BoundsRect: TRect;
    procedure PaintToCanvas(ACanvas: TCanvas); virtual; abstract;
    procedure DrawSelectedRect(ACanvas: TCanvas); virtual;
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    destructor Destroy; override;
    function PointInRgn(x, y: integer): Boolean;
    function RectsIntersect(ARect: TRect): Boolean;
    procedure UpdateToDragPosition;
    procedure SnapToGrid;
    procedure SaveToJson(AJson: TJsonObject); virtual;
    procedure LoadFromJson(AJson: TJsonObject); virtual;
    property Selected: Boolean read FSelected write SetSelectedItem;
    property LeftMM: Extended read GetLeftMM write SetLeftMM;
    property TopMM: Extended read GetTopMM write SetTopMM;
    property WidthMM: Extended read GetWidthMM write SetWidthMM;
    property HeightMM: Extended read GetHeightMm write SetHeightMm;
    property CenterPtMm: TPointF read GetCenterPtMm write SetCenterPtMm;
    property Options: TItemOptions read fOptions write SetOptions;
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

  TDesignBoxItemList = class(TObjectList<TDesignBoxBaseItem>)
  private
    FDesignBox: TDesignBox;
    function GetSelectedCount: integer;
  protected
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    function BoundsRect: TRect;
    function Add(AItem: TDesignBoxBaseItem; ABoundsMm: TRectF): TDesignBoxBaseItem; overload;
    function Add(AClass: TDesignBoxBaseItemClass; ABoundsMm: TRectF): TDesignBoxBaseItem; overload;
    function ItemAtPos(x, y: integer): TDesignBoxBaseItem;
    procedure LoadFromJson(AJson: TJsonObject);
    procedure SaveToJson(AJson: TJsonObject);
    procedure DeleteSelected;
    property SelectedCount: integer read GetSelectedCount;
    procedure DeselectAll;
    procedure SelectAll;
    procedure SelectItems(AItems: array of TDesignBoxBaseItem);
    property DesignBox: TDesignBox read fDesignBox;


  end;

  TDesignBoxRulerOptions = class(TPersistent)
  private
    fDesignBox : TDesignBox;
    fForegroundColor: TColor;
    fBackgroundColor: TColor;
    fVisible: Boolean;
    fRulerHeightPx : integer;
    fRulerWidthPx : integer;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetVisible(const Value: boolean);
    function GetHeightPx: integer;
    function GetWidthPx: integer;
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromJsonObject(AJson: TJsonObject);
    procedure SaveToJsonObject(AJson: TJsonObject);
  published
    property ForegroundColor: TColor read fForegroundColor write SetForegroundColor default clBlack;
    property BackgroundColor: TColor read fBackgroundColor write SetBackgroundColor default clWhite;
    property Visible: boolean read fVisible write SetVisible default true;
    property WidthPx : integer read GetWidthPx;
    property HeightPx : integer read GetHeightPx;
  end;

  TDesignBoxGridOptions = class(TPersistent)
  private
    FDesignBox: TDesignBox;
    FVisible: Boolean;
    FSizeMm: integer;
    FColor: TColor;
    FSnapToGrid: boolean;
    procedure SetVisible(const Value: Boolean);
    procedure SetSizeMm(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetSnapToGrid(const Value: Boolean);
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
  end;

  TItemAlignment = (ialLeftSides, ialTopSides, ialRightSides, ialBottomSides, ialToGrid,
    ialVertCenters, ialHorzCenters);

  TDesignBoxCanvas = class
  private
    FTempCanvas: TBitmap;
    FDesignBox: TDesignBox;
    FBrush: TBrush;
    FFont: TFont;
    FPen: TPen;
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);

    procedure OnBrushChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure OnPenChanged(Sender: TObject);

  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    destructor Destroy; override;
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

    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;

  end;


  TDesignBox = class(TGraphicControl)
  private
    FMode: TDesignBoxMode;
    FCanvas: TDesignBoxCanvas;
    FItems: TDesignBoxItemList;
    FSelectedItems: TDesignBoxItemList;
    FDragging: Boolean;
    FMouseDownPos: TPoint;
    FMouseXY: TPoint;
    FBuffer: TBitmap;
    FOnSelectItem: TDesignBoxSelectItemEvent;
    FBackgroundColor: TColor;
    FPageColor: TColor;
    FUpdating: Boolean;
    FPageOffset: TPoint;
    FPageSizeMM: TSize;
    FUndoList: TDesignUndoList;
    FOnChange: TNotifyEvent;
    FGridOptions: TDesignBoxGridOptions;
    FRulerOptions: TDesignBoxRulerOptions;
    FUpdateCount: integer;
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
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure WMEraseBackground(var message: TMessage); message WM_ERASEBKGND;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
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
    //property Brush: TBrush read FBrush write SetBrush;
    //property Font: TFont read FFont write SetFont;
    //property Pen: TPen read FPen write SetPen;
    procedure Redraw;
    procedure Undo;
    procedure Redo;
    procedure SaveSnapShot(aForce: boolean);
    function CanUndo : boolean;
    function CanRedo : boolean;
    procedure AlignItems(const aAlignment: TItemAlignment);

    property Canvas: TDesignBoxCanvas read FCanvas;
    property Items: TDesignBoxItemList read FItems;
    property Mode: TDesignBoxMode read FMode write FMode default dbmSelect;
    property SelectedItems: TDesignBoxItemList read GetSelectedItems;// write SetSelectedItem;

  published
    property Align;
    property BackgroundColor: TColor read fBackgroundColor write SetBackgroundColor default clSilver;
    property PageColor : TColor read fPageColor write SetPageColor default clWhite;
    property GridOptions: TDesignBoxGridOptions read FGridOptions write SetGridOptions;
    property PopupMenu;
    property RulerOptions: TDesignBoxRulerOptions read fRulerOptions write SetRulerOptions;
    property PageWidthMM: integer read GetPageWidthMM write SetPageWidthMM;
    property PageHeightMM: integer read GetPageHeightMM write SetPageHeightMM;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem: TDesignBoxSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnResize;
  end;

procedure Register;

function MmToPixels(AValue: Extended): integer;
function PixelsToMM(AValue: Extended) : single;

implementation

uses System.NetEncoding, PngImage, Jpeg, Math, System.UITypes, Forms, System.RTTI;

const
  C_HIGHLIGHT_COLOR = clHotlight;
  C_SELECTBOX_INFLATE = 0;
  C_DPI = 300;
  C_MM_PER_INCH = 25.4;


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
  Result := Round(((C_DPI / C_MM_PER_INCH) * AValue) * AScale);
end;

function PixelsToMM(AValue: Extended) : single;
begin
  Result := AValue / (ScreenDpi/ C_MM_PER_INCH);
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
var
  AItem: TDesignBoxBaseItem;
  ASelectedRect: TRect; // what pixel (horz or vert) are we aligning on?
begin
  ASelectedRect := SelectedItems.BoundsRect;
  for AItem in FSelectedItems do
  begin
    case aAlignment of
      ialLeftSides    : AItem.LeftMM := PixelsToMM(ASelectedRect.Left);
      ialTopSides     : AItem.TopMM  := PixelsToMM(ASelectedRect.Top);
      ialRightSides   : AItem.LeftMM := PixelsToMM(ASelectedRect.Right) - aItem.WidthMM;
      ialBottomSides  : Aitem.TopMM  := PixelsToMM(ASelectedRect.Bottom) - AItem.HeightMM;
      ialToGrid       : AItem.SnapToGrid;
      ialVertCenters  : AItem.CenterPtMm  := PointF(AItem.CenterPtMm.X, PixelsToMM(ASelectedRect.CenterPoint.Y));
      ialHorzCenters  : AItem.CenterPtMm  := PointF(PixelsToMM(ASelectedRect.CenterPoint.X), AItem.CenterPtMm.Y);
    end;
  end;

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
  Redraw;
end;

constructor TDesignBox.Create(AOwner: TComponent);
begin
  inherited;
  FUpdating := True;
  FCanvas := TDesignBoxCanvas.Create(Self);
  FBuffer := TBitmap.Create;
  FItems := TDesignBoxItemList.Create(Self);
  FSelectedItems := TDesignBoxItemList.Create(Self);
  FUndoList := TDesignUndoList.Create(Self);
  FGridOptions := TDesignBoxGridOptions.Create(Self);
  FRulerOptions := TDesignBoxRulerOptions.Create(Self);

  FSelectedItems.OwnsObjects := False;
  FPageSizeMM.Width := 100;
  FPageSizeMM.Height := 100;
  FBackgroundColor := clSilver;
  FPageColor := clWhite;

  ResizeCanvas;

  FMode := dbmSelect;
  FUpdating := False;
  Redraw;
  FUpdateCount := 0;
end;

destructor TDesignBox.Destroy;
begin
  FCanvas.Free;
  FItems.Free;
  FBuffer.Free;
  FSelectedItems.Free;
  FUndoList.Free;
  FGridOptions.Free;
  FRulerOptions.Free;
  inherited;
end;

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
  SaveSnapShot(True);
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

procedure TDesignBox.LoadFromJson(AJsonData: string);
var
  AJson: TJSONObject;
begin
  AJson := TJsonObject.ParseJSONValue(AJsonData) as TJSONObject;
  try
    fRulerOptions.LoadFromJsonObject(AJson);
    if assigned(AJson.Values['pageWidthMM']) then
      PageWidthMM := TJsonNumber(AJson.values['pageWidthMM']).asInt;
    if assigned(AJson.Values['pageHeightMM']) then
      PageHeightMM := TJsonNumber(AJson.values['pageHeightMM']).asInt;
    if assigned(AJson.Values['backgroundColor']) then
      fBackgroundColor := StringToColor(AJson.values['backgroundColor'].value);
    FItems.LoadFromJson(AJson);
    Redraw;
    if Assigned(FOnChange) then
      FOnChange(Self);
  finally
    AJson.Free;
  end;
end;

procedure TDesignBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AItem: TDesignBoxBaseItem;
  ADeselectOthers: Boolean;
begin
  FRulerOptions.fRulerWidthPx := fPageOffset.X;
  FRulerOptions.fRulerHeightPx := fPageOffset.Y;

  X := X - FPageOffset.X;
  Y := Y - FPageOffset.Y;

  ADeselectOthers := True;

  FMouseDownPos := Point(X, Y);

  AItem := FItems.ItemAtPos(x, y);

  if (ssShift in Shift) then ADeselectOthers := False;
  if (AItem <> nil) and (AItem.Selected) then ADeselectOthers := False;

  if ADeselectOthers then FItems.DeselectAll;

  if (AItem <> nil) and (canSelect in AItem.Options) then
  begin
    case (ssShift in Shift) of
      True: AItem.Selected := not AItem.Selected;
      False: AItem.Selected := True and (canSelect in AItem.Options);
    end;
  end;

  FDragging := True;
  FMouseXY := Point(X, Y);

  if (AItem <> nil) and (AItem.Selected) and (Assigned(FOnSelectItem)) then
    FOnSelectItem(Self, AItem);

  if SelectedItems.Count > 0 then
    FMode := dbmSelect;
  inherited;
end;

procedure TDesignBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AItem: TDesignBoxBaseItem;
  ASelectRect: TRect;
  ADragOffset: TPoint;
  Xsnap: integer;
  YSnap: integer;
  AGridPixels: integer;
begin
  inherited;
  X := X - FPageOffset.X;
  Y := Y - FPageOffset.Y;
  FMouseXY := Point(X, Y);

  ADragOffset := Point(0, 0);
  if FDragging then
    ADragOffset := Point((X - FMouseDownPos.X),
                         (Y - FMouseDownPos.Y));


  if (FDragging) and (FItems.SelectedCount > 0) then
  begin
    for AItem in FItems do
    begin
      if AItem.Selected and (canMove in AItem.Options) then
        AItem.FDrawOffset := ADragOffset;
    end;

    if FGridOptions.SnapToGrid then
    begin
      ASelectRect := SelectedItems.BoundsRect;
      AGridPixels :=  MmToPixels(FGridOptions.SizeMm);
      XSnap := (Round(ASelectRect.Left / AGridPixels) * AGridPixels) - ASelectRect.Left;
      YSnap := (Round(ASelectRect.Top / AGridPixels) * AGridPixels) - ASelectRect.Top;
      for AItem in FItems do
      begin
        if AItem.Selected and (canMove in AItem.Options) then
          AItem.FDrawOffset.Offset(xsnap, ySnap);
      end;
    end;
    Redraw;
  end
  else
    Invalidate;
end;

procedure TDesignBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ADragArea: TRect;
  AItem: TDesignBoxBaseItem;
  ARect: TRectF;
begin
  inherited;
  X := X - FPageOffset.X;
  Y := Y - FPageOffset.Y;

  if (FDragging) and (FItems.SelectedCount = 0) and (Mode = dbmSelect) then
  begin
    ADragArea := Rect(Min(FMouseDownPos.X, X), Min(FMouseDownPos.Y, Y), Max(FMouseDownPos.X, X), Max(FMouseDownPos.Y, Y));
    if (ADragArea.Width > 4) and (ADragArea.Height > 4) then
    for AItem in FItems do
    begin
      AItem.Selected := AItem.RectsIntersect(ADragArea) and (canSelect in AItem.Options);
    end;
  end
  else
  begin
    for AItem in SelectedItems do
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
  case FMode of
     dbmDrawRect    : FCanvas.Rectangle(ARect, 0);
     dbmDrawEllipse : FCanvas.Ellipse(ARect);
  end;

  Redraw;
  RecordSnapshot;
end;

procedure TDesignBox.DrawRulers(ACanvas: TCanvas);
var
  AMm: integer;
  AMarkSize: integer;
  tz : TSize;
const
  C_BIG_MARK = 10;
  C_LITTLE_MARK = 5;
begin

  ACanvas.Brush.Color := fRulerOptions.BackgroundColor;
  ACanvas.Pen.Style := psClear;
  ACanvas.FillRect(Rect(0, 0, Width, FPageOffset.Y));
  ACanvas.FillRect(Rect(0, 0, FPageOffset.X, Height));
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := fRulerOptions.ForegroundColor;
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Polyline([Point(0, FPageOffset.Y), Point(Width, FPageOffset.Y)]);
  ACanvas.Polyline([Point(FPageOffset.X, 0), Point(FPageOffset.X, Height)]);

  ACanvas.Font.Color := fRulerOptions.ForegroundColor;
  AMm := 1;
  while MmToPixels(AMm) < Width do
  begin
    AMarkSize := 0;
    if (AMm) mod 2 = 0 then AMarkSize := C_LITTLE_MARK;
    if (AMm) mod 10 = 0 then AMarkSize := C_BIG_MARK;
    if AMarkSize > 0 then
    begin
      ACanvas.Polyline([Point(MmToPixels(AMm)+FPageOffset.X, FPageOffset.Y-AMarkSize),
                        Point(MmToPixels(AMm)+FPageOffset.X, FPageOffset.Y)]);
      if AMarkSize = 10 then
      begin
        tz := ACanvas.TextExtent(AMm.ToString);
        ACanvas.TextOut((MmToPixels(AMm)+FPageOffset.X) - (tz.Width div 2), FPageOffset.Y-20, AMm.ToString);
      end;
    end;
    Inc(AMm);
  end;

  AMm := 1;
  while MmToPixels(AMm) < Height do
  begin
    AMarkSize := 0;
    if AMm mod 2 = 0 then AMarkSize := C_LITTLE_MARK;
    if AMm mod 10 = 0 then AMarkSize := C_BIG_MARK;
    if AMarkSize > 0 then
    begin
      ACanvas.Polyline([Point(FPageOffset.X-AMarkSize, MmToPixels(AMm)+FPageOffset.Y),
                        Point(FPageOffset.X, MmToPixels(AMm)+FPageOffset.Y)]);
      if AMarkSize = 10 then
      begin
        tz := ACanvas.TextExtent(AMm.ToString);
        ACanvas.TextOut(FPageOffset.X-tz.width - C_LITTLE_MARK, (MmToPixels(AMm)+FPageOffset.Y) - (tz.Height div 2), AMm.ToString);
      end;
    end;
    Inc(AMm);
  end;
end;

procedure TDesignBox.Endupdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      Redraw;
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

  X := 0;
  while X < FPageSizeMM.Width do
  begin
    ACanvas.Pen.Color := FGridOptions.Color;
    ACanvas.Pen.Mode := pmCopy;
    ACanvas.MoveTo(MmToPixels(x), 0);
    ACanvas.LineTo(MmToPixels(x), APageHeight);
    X := X + FGridOptions.SizeMm;
  end;

  Y := 0;
  while Y < FPageSizeMM.Height do
  begin
    ACanvas.MoveTo(0, MmToPixels(y));
    ACanvas.LineTo(APageWidth, MmToPixels(y));
    Y := Y + FGridOptions.SizeMm;
  end;

end;

procedure TDesignBox.Paint;
var
  ARect: TRect;
  aRulerWidth: integer;
  ACanvas: TCanvas;
begin
  ACanvas := inherited Canvas;

  aRulerWidth := ACanvas.TextWidth('9999') + 6;
  case FRulerOptions.Visible of
    True: FPageOffset := Point(aRulerWidth, aRulerWidth); // slightly narrower
    False: FPageOffset := Point(0, 0);
  end;

  ACanvas.Brush.Color := FBackgroundColor;

  ACanvas.FillRect(ClientRect);
  ACanvas.FrameRect(ClientRect);

  if (FBuffer.Width = 0) then
    Redraw;

  if FRulerOptions.Visible then
    DrawRulers(ACanvas);

  ACanvas.Draw(FPageOffset.X, FPageOffset.Y, FBuffer);

  if (FDragging) and (FItems.SelectedCount = 0) then
  begin
    ARect := Rect(FMouseDownPos.X+FPageOffset.X,
                  FMouseDownPos.Y+FPageOffset.Y,
                  FMouseXY.X+FPageOffset.X,
                  FMouseXY.Y+FPageOffset.Y);
    ACanvas.Pen.Style := psDot;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Mode := pmNot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(ARect);
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Brush.Color := clDkGray;
  ACanvas.FrameRect(ClientRect);

end;


procedure TDesignBox.RecordSnapshot;
begin
  if FUpdating then
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
  FBuffer.Canvas.Brush.Color := PageColor;
  FBuffer.Canvas.Brush.Style := bsClear;
  FBuffer.Canvas.Pen.Color := clBlack;
  FBuffer.Canvas.Pen.Mode := pmCopy;
  FBuffer.Canvas.Rectangle(0, 0, FBuffer.Width, FBuffer.Height);

  if FGridOptions.Visible then
    DrawGrid(FBuffer.Canvas);

  if FItems.Count > 0 then
  begin
    for AItem in FItems do
    begin
      AItem.PaintToCanvas(FBuffer.Canvas);
      if AItem.Selected then
        AItem.DrawSelectedRect(FBuffer.Canvas);
    end;
  end;
  FBuffer.Canvas.Pen.Style := psSolid;
  FBuffer.Canvas.Brush.Style := bsClear;
  FBuffer.Canvas.Pen.Color := clBlack;
  FBuffer.Canvas.Pen.Mode := pmCopy;
  FBuffer.Canvas.Pen.Width := 1;
  FBuffer.Canvas.Rectangle(0,0, FBuffer.Width, FBuffer.Height);

  Invalidate;
end;

procedure TDesignBox.Resize;
begin
  inherited;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  Redraw;
  Invalidate;
end;

procedure TDesignBox.ResizeCanvas;
begin
  FBuffer.SetSize(MmToPixels(FPageSizeMM.Width)+1,
                  MmToPixels(FPageSizeMM.Height)+1);
  FBuffer.Canvas.Brush.Color := FPageColor;
  FBuffer.Canvas.FillRect(Rect(0,0,FBuffer.Width, FBuffer.Height));
end;

procedure TDesignBox.SaveSnapShot(aForce: boolean);
begin
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



procedure TDesignBox.SetGridOptions(const Value: TDesignBoxGridOptions);
begin
  FGridOptions.Assign(Value);
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

{ TDesignBoxItemText }

constructor TDesignBoxItemText.Create(ADesignBox: TDesignBox);
begin
  inherited Create(ADesignBox);
  FFont := TDesignFont.Create;

  FFont.Size := 16;
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
  FWidth := (ACanvas.TextWidth(FText));
  FHeight := (ACanvas.TextHeight(FText));

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
  if not FDesignBox.FDragging then
    FDesignBox.SaveSnapshot(False);
end;

constructor TDesignBoxBaseItem.Create(ADesignBox: TDesignBox);
begin
  inherited Create;
  FDesignBox := ADesignBox;
  FSelected := False;
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
  if FSelected then
  begin
    ARect := RectPixels;
    InflateRect(ARect, C_SELECTBOX_INFLATE, C_SELECTBOX_INFLATE);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Color := C_HIGHLIGHT_COLOR;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Width := 1;
    ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right+1, ARect.Bottom+1);
  end;
end;

procedure TDesignBoxBaseItem.UpdateToDragPosition;
begin
  FPosition.X := FPosition.X + FDrawOffset.X;
  FPosition.Y := FPosition.Y + FDrawOffset.Y;
  FDrawOffset := Point(0,0);
end;

function TDesignBoxBaseItem.GetCenterPtMm: TPointF;
begin
  Result := RectMM.CenterPoint;
end;

function TDesignBoxBaseItem.GetHeightMm: Extended;
begin
  Result := PixelsToMM(FHeight);
end;

function TDesignBoxBaseItem.GetLeftMM: Extended;
begin
  Result := PixelsToMM(FPosition.X);
end;

function TDesignBoxBaseItem.GetTopMM: Extended;
begin
  Result := PixelsToMM(FPosition.Y);
end;


function TDesignBoxBaseItem.GetWidthMM: Extended;
begin
  Result := PixelsToMM(FWidth);
end;

function TDesignBoxBaseItem.PointInRgn(x, y: integer): Boolean;
begin
  Result := PtInRect(RectPixels, Point(x,y));
end;

function TDesignBoxBaseItem.BoundsRect: TRect;
begin
  Result := Rect(FPosition.X,
                 FPosition.Y,
                 FPosition.X + FWidth,
                 FPosition.Y + FHeight);
end;

function TDesignBoxBaseItem.RectMM: TRectF;
begin
  Result := RectPixels;
  Result.Left := PixelsToMM(Result.Left);
  Result.Top := PixelsToMM(Result.Top);
  Result.Right := PixelsToMM(Result.Right);
  Result.Bottom := PixelsToMM(Result.Bottom);
end;

function TDesignBoxBaseItem.RectPixels: TRect;
begin
  Result.Left := FPosition.X;
  Result.Top := FPosition.Y;
  Result.Right := FPosition.X + FWidth;
  Result.Bottom := FPosition.Y + FHeight;
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
  FPosition.X := MmToPixels(StrToFloatDef(AJson.Values['x'].Value, 0));
  FPosition.Y := MmToPixels(StrToFloatDef(AJson.Values['y'].Value, 0));
  FWidth := MmToPixels(StrToFloatDef(AJson.Values['width'].Value, 0));
  FHeight := MmToPixels(StrToFloatDef(AJson.Values['height'].Value, 0));
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
end;

procedure TDesignBoxBaseItem.SaveToJson(AJson: TJsonObject);
var
  aOption : TItemOption;
  jOptions : TJsonArray;
begin
  AJson.AddPair('obj', ClassName);
  AJson.AddPair('x',  PixelsToMm(FPosition.X).ToString);
  AJson.AddPair('y', PixelsToMm(FPosition.Y).ToString);
  AJson.AddPair('width', PixelsToMm(FWidth).ToString);
  AJson.AddPair('height',PixelsToMm(FHeight).ToString);
  jOptions := TJsonArray.Create;
  for aOption in fOptions do
    jOptions.Add(TRttiEnumerationType.GetName(aOption));
  AJson.AddPair('options', jOptions);
end;

procedure TDesignBoxBaseItem.SetCenterPtMm(const Value: TPointF);
begin
  LeftMM := Value.X - (WidthMM / 2);
  TopMM := Value.Y - (HeightMM / 2);
  Changed;
end;

procedure TDesignBoxBaseItem.SetHeightMm(const Value: Extended);
begin
  FHeight := MmToPixels(Value);
end;

procedure TDesignBoxBaseItem.SetLeftMM(const Value: Extended);
begin
  FPosition.X := MmToPixels(Value)
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
  FPosition.Y := MmToPixels(Value)
end;

procedure TDesignBoxBaseItem.SetWidthMM(const Value: Extended);
begin
  FWidth := MmToPixels(Value)
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

function TDesignBoxItemList.Add(AItem: TDesignBoxBaseItem; ABoundsMm: TRectF): TDesignBoxBaseItem;
begin
  Result := AItem;
  Result.LeftMM := ABoundsMm.Left;
  Result.TopMM := ABoundsMm.Top;
  if ABoundsMm.Width > 0 then Result.FWidth := MmToPixels(ABoundsMm.Width);
  if ABoundsMm.Height > 0 then Result.FHeight := MmToPixels(ABoundsMm.Height);
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
  for AItem in Self do
    AItem.Selected := false;
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
  FDesignBox.FUpdating := True;
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
    FDesignBox.FUpdating := False;
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
  for AItem in Self do
    if canSelect in AItem.Options then
      AItem.Selected := TRUE;
end;

procedure TDesignBoxItemList.SelectItems(AItems: array of TDesignBoxBaseItem);
var
  AItem: TDesignBoxBaseItem;
begin
  for AItem in AItems do
    if canSelect in AItem.Options then
      AItem.Selected := True;
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
  // calculate width/height...
  //FWidth := FGraphic.Width;
  //FHeight := FGraphic.Height;

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

  if FSelected then
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
  FPosition.X := MmToPixels(ALeftMM);
  FPosition.Y := MmToPixels(ATopMM);
  FWidth := MmToPixels(ARightMM-ALeftMM);
  FHeight := MmToPixels(ABottomMM-ATopMM);
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
  if (FDesignBox.FUpdating) and (not AForce) then
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
  FDesignBox.Invalidate;
end;

constructor TDesignBoxRulerOptions.Create(ADesignBox: TDesignBox);
begin
  inherited Create;
  fDesignBox := aDesignBox;
  fBackgroundColor := clWhite;
  fForegroundColor := clBlack;
  fVisible := true;
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
end;

procedure TDesignBoxRulerOptions.SaveToJsonObject(AJson: TJsonObject);
begin
  AJson.AddPair('rulersVisible', TJsonBool.Create(fVisible));
  AJson.AddPair('rulerForegroundColor', ColorToString(fForegroundColor));
  AJson.AddPair('rulerBackgroundColor', ColorToString(fBackgroundColor));
end;

procedure TDesignBoxRulerOptions.SetBackgroundColor(const Value: TColor);
begin
  if fBackgroundColor <> Value then
  begin
    fBackgroundColor := Value;
    fDesignBox.Invalidate;
  end;
end;

procedure TDesignBoxRulerOptions.SetForegroundColor(const Value: TColor);
begin
  if fForegroundColor <> Value then
  begin
    fForegroundColor := Value;
    fDesignBox.Invalidate;
  end;
end;

procedure TDesignBoxRulerOptions.SetVisible(const Value: boolean);
begin
  if fVisible <> Value then
  begin
    fVisible := Value;
    fDesignBox.Invalidate;
  end;
end;


{ TDesignBoxCanvas }

function TDesignBoxCanvas.TextOut(ALeftMM, ATopMM: single;
  AText: string): TDesignBoxItemText;
begin
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
  FDesignBox.RecordSnapshot;
end;

function TDesignBoxCanvas.Draw(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
begin
  Result := StretchDraw(RectF(ALeftMM,
                        ATopMM,
                        ALeftMM + PixelsToMM(AGraphic.Width),
                        ATopMM + PixelsToMM(AGraphic.Height)),
                        AGraphic);
end;

function TDesignBoxCanvas.StretchDraw(ARect: TRectF; AGraphic: TGraphic): TDesignBoxItemGraphic;
begin
  Result := TDesignBoxItemGraphic.Create(FDesignBox);
  Result.Graphic.Assign(AGraphic);
  FDesignBox.Items.Add(Result, ARect);
end;

function TDesignBoxCanvas.Ellipse(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemEllipse;
begin
  Result := Ellipse(ALeftMM, ATopMM, ARightMM, ABottomMM);
end;

function TDesignBoxCanvas.Ellipse(ABoundsMm: TRectF): TDesignBoxItemEllipse;
begin
  Result := FDesignBox.Items.Add(TDesignBoxItemEllipse, ABoundsMm) as TDesignBoxItemEllipse;
end;

function TDesignBoxCanvas.MeasureText(const AText: string): TSizeF;
begin
  Result := FTempCanvas.Canvas.TextExtent(aText);
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
  Result := FDesignBox.Items.Add(TDesignBoxItemRectangle, ABoundsMm) as TDesignBoxItemRectangle;
  Result.RoundnessMm := ARoundnessMm;
end;

constructor TDesignBoxCanvas.Create(ADesignBox: TDesignBox);
begin
  inherited Create;
  FTempCanvas := TBitmap.Create;
  FDesignBox := ADesignBox;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FPen := TPen.Create;
  FBrush.OnChange := OnBrushChanged;
  FFont.OnChange := OnFontChanged;;
  FPen.OnChange := OnPenChanged;
end;

function TDesignBoxCanvas.TextExtent(const AText: string): TSizeF;
begin
  Result := FTempCanvas.Canvas.TextExtent(AText);
  Result.cx := PixelsToMM(Result.cx);
  Result.cy := PixelsToMM(Result.cy);
end;

function TDesignBoxCanvas.TextWidth(const AText: string): single;
begin
  Result := PixelsToMM(FTempCanvas.Canvas.TextWidth(AText));
end;

function TDesignBoxCanvas.TextHeight(const AText: string): single;
begin
  Result := PixelsToMM(FTempCanvas.Canvas.TextHeight(AText));
end;

destructor TDesignBoxCanvas.Destroy;
begin
  FTempCanvas.Free;
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
  FDesignBox.Redraw;
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
  FDesignBox.Redraw;
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
  FDesignBox.Redraw;
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
