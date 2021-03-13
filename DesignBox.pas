unit DesignBox;

{ DONE : Grid not being drawn correctly - set to 20mm and it's drawing @ 25mm - https://i.imgur.com/bcM8KiS.png - appears to be because MMtoPixels using Screen DPI whereas others are using fixed 96 dpi }
{ DONE : Snap to Grid not working as expected }
{ TODO : High DPI support -- hard coded "96" needs to be Monitor.PixelsPerInch
{ TODO : Undo goes back "too far" (UndoList[-1] ?) and then won't redo }
{ TODO : Drag to select, followed by a single click off-object (to de-select) erases the grid }

interface

uses
  Windows, Messages, System.SysUtils, System.Classes, Vcl.Controls, System.Types, System.Generics.Collections,
  vcl.Graphics, System.Generics.Defaults, System.Json;

type

  TItemOption = (canSize, canMove, canDelete);
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
    FPositionMM: TPointF;
    FWidthMM: single;
    FHeightMM: single;
    FSelected: Boolean;
    FSnapRect: TRect;
    fOptions : TItemOptions;
    function GetLeftMM: single;
    function GetTopMM: single;
    procedure Changed;
    procedure SetLeftMM(const Value: single);
    procedure SetTopMM(const Value: single);
    procedure SetSelectedItem(const Value: Boolean);
    procedure SetOptions(const Value: TItemOptions);
  protected
    function RectPixels: TRect; virtual;
    procedure OffsetByPixels(X, Y: integer; const ARedraw: Boolean = True);
    procedure PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint); virtual;
    procedure DrawSelectedRect(ACanvas: TCanvas; ASnapOffset: TPoint); virtual;
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    destructor Destroy; override;
    function PointInRgn(x, y: integer): Boolean;
    function RectsIntersect(ARect: TRect): Boolean;
    procedure FixToSnapRect;
    procedure SaveToJson(AJson: TJsonObject); virtual;
    procedure LoadFromJson(AJson: TJsonObject); virtual;
    property Selected: Boolean read FSelected write SetSelectedItem;
    property LeftMM: single read GetLeftMM write SetLeftMM;
    property TopMM: single read GetTopMM write SetTopMM;
    property WidthMM: single read fWidthMM write fWidthMM;
    property HeightMM: single read fHeightMM write fHeightMM;
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
    procedure DrawSelectedRect(ACanvas: TCanvas; ASnapOffset: TPoint); override;

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
    procedure PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint); override;
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
    procedure PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint); override;
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
    procedure PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint); override;
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
    procedure PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint); override;
  end;

  TDesignBoxItemRectangle = class(TDesignBoxItemFilledShape)
  private
    FRoundnessMm: single;
    procedure SetRoundnessMm(const Value: single);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    property RoundnessMm: single read FRoundnessMm write SetRoundnessMm;
  end;

  TDesignBoxItemGraphic = class(TDesignBoxItemSizable)
  private
    FGraphic: TPicture;
    procedure SetGraphic(const Value: TPicture);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint); override;
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
    function AddText(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
    function AddGraphic(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
    function AddRectangle(ABoundsMm: TRectF; const ARoundnessMm: single = 0): TDesignBoxItemRectangle;
    function AddEllipse(ABoundsMm: TRectF): TDesignBoxItemEllipse;
    function Add(AItem: TDesignBoxBaseItem; ABoundsMm: TRectF): TDesignBoxBaseItem; overload;
    function Add(AClass: TDesignBoxBaseItemClass; ABoundsMm: TRectF): TDesignBoxBaseItem; overload;
    function ItemAtPos(x, y: integer): TDesignBoxBaseItem;
    property DesignBox: TDesignBox read fDesignBox;

    procedure LoadFromJson(AJson: TJsonObject);
    procedure SaveToJson(AJson: TJsonObject);
    procedure DeleteSelected;
    property SelectedCount: integer read GetSelectedCount;
    procedure DeselectAll;

  end;

  TDesignBoxRulerOptions = class(TPersistent)
  private
    fDesignBox : TDesignBox;
    fForegroundColor: TColor;
    fBackgroundColor: TColor;
    fVisible: Boolean;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetVisible(const Value: boolean);
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromJsonObject(AJson: TJsonObject);
    procedure SaveToJsonObject(AJson: TJsonObject);
  published
    property ForegroundColor: TColor read fForegroundColor write SetForegroundColor default clBlack;
    property BackgroundColor: TColor read fBackgroundColor write SetBackgroundColor default clWhite;
    property Visible: boolean read fVisible write SetVisible default true;
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
    property Color: TColor read FColor write SetColor default $00EEEEEE;
    property SizeMm: integer read FSizeMm write SetSizeMm default 5;
    property Visible: Boolean read FVisible write SetVisible default False;
    property SnapToGrid: Boolean read FSnapToGrid write SetSnapToGrid default False;
  end;

  TItemAlignment = (ialLeftSides, ialTopSides, ialRightSides, ialBottomSides, ialToGrid);

  TDesignBox = class(TGraphicControl)
  private
    FItems: TDesignBoxItemList;
    FSelectedItems: TDesignBoxItemList;
    FDragging: Boolean;
    FMouseDownPos: TPoint;
    FMouseXY: TPoint;
    FBuffer: TBitmap;
    FOnSelectItem: TDesignBoxSelectItemEvent;
    FBrush: TBrush;
    FFont: TFont;
    FPen: TPen;
    FBackgroundColor: TColor;
    FUpdating: Boolean;
    FPageOffset: TPoint;
    FPageSizeMM: TSize;
    FUndoList: TDesignUndoList;
    FOnChange: TNotifyEvent;
    FGridOptions: TDesignBoxGridOptions;
    FRulerOptions: TDesignBoxRulerOptions;
    FMouseObjectOffset: TPoint;
    procedure OnBrushChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure OnPenChanged(Sender: TObject);

    function GetSelectedItems: TDesignBoxItemList;
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);
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
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure WMEraseBackground(var message: TMessage); message WM_ERASEBKGND;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function  SelectedItemsPixelRect: TRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
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
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
    property BackgroundColor: TColor read fBackgroundColor write SetBackgroundColor;
    property Items: TDesignBoxItemList read FItems;
    property SelectedItems: TDesignBoxItemList read GetSelectedItems;// write SetSelectedItem;
    procedure Redraw;
    procedure Undo;
    procedure Redo;
    procedure SaveSnapShot(aForce: boolean);
    function CanUndo : boolean;
    function CanRedo : boolean;
    procedure AlignItems(const aAlignment: TItemAlignment);
  published
    property Align;
    property GridOptions: TDesignBoxGridOptions read FGridOptions write SetGridOptions;
    property PopupMenu;
    property RulerOptions: TDesignBoxRulerOptions read fRulerOptions write SetRulerOptions;
//    property RulerBackground: TColor read GetRulerBackground write SetRulerBackground default clWhite;
//    property RulerForeground: TColor read GetRulerForeground write SetRulerForeground default clBlack;
//    property ShowRulers: Boolean read GetShowRulers write SetShowRulers default True;
    property PageWidthMM: integer read GetPageWidthMM write SetPageWidthMM;
    property PageHeightMM: integer read GetPageHeightMM write SetPageHeightMM;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem: TDesignBoxSelectItemEvent read FOnSelectItem write FOnSelectItem;

  end;

procedure Register;

implementation

uses System.NetEncoding, PngImage, Jpeg, Math, System.UITypes, Forms;

const
  C_HIGHLIGHT_COLOR = clHotlight;
  C_SELECTBOX_INFLATE = 2;
  C_DPI = 96;

procedure Register;
begin
  RegisterComponents('Samples', [TDesignBox]);
end;

function MmToPixels(AValue: single): single;
begin
  Result := ({Screen.PixelsPerInch} C_DPI / 25.4) * AValue;
end;

function PixelsToMM(AValue: single) : single;
begin
  result := (AValue / ({Screen.PixelsPerInch}C_DPI/ 25.4));
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
  aSelectedItemsPixelRect: TRect; // what pixel (horz or vert) are we aligning on?
begin
  if FSelectedItems.Count < 2 then EXIT;

  aSelectedItemsPixelRect := SelectedItemsPixelRect;
  for AItem in FSelectedItems do
  begin
    case aAlignment of
      ialLeftSides:   AItem.LeftMM := PixelsToMM(aSelectedItemsPixelRect.Left);
      ialTopSides:    AItem.TopMM  := PixelsToMM(aSelectedItemsPixelRect.Top);
      ialRightSides:  AItem.LeftMM := PixelsToMM(aSelectedItemsPixelRect.Right) - aItem.WidthMM;
      ialBottomSides: Aitem.TopMM := PixelsToMM(aSelectedItemsPixelRect.Bottom) - AItem.HeightMM;
      ialToGrid: ;
    end;
  end;

  Redraw;

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
  FBuffer := TBitmap.Create;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FPen := TPen.Create;
  FItems := TDesignBoxItemList.Create(Self);
  FSelectedItems := TDesignBoxItemList.Create(Self);
  FSelectedItems.OwnsObjects := False;
  FGridOptions := TDesignBoxGridOptions.Create(Self);
  FRulerOptions := TDesignBoxRulerOptions.Create(Self);
  FPageSizeMM.Width := 100;
  FPageSizeMM.Height := 100;

  // This must be after creating any sub-objects as it calls CreateSnapshot and we need all objects instanciated
  FUndoList := TDesignUndoList.Create(Self);

  ResizeCanvas;
  FBrush.OnChange := OnBrushChanged;
  FFont.OnChange := OnFontChanged;;
  FPen.OnChange := OnPenChanged;
  FBackgroundColor := clWhite;
  FUpdating := False;
  Redraw;
end;

destructor TDesignBox.Destroy;
begin
  FItems.Free;
  FBuffer.Free;
  FSelectedItems.Free;
  FFont.Free;
  FBrush.Free;
  FPen.Free;
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
  aSelectedItemsPixelRect: TRect;
begin
  inherited;
  X := X - FPageOffset.X;
  Y := Y - FPageOffset.Y;

  ADeselectOthers := True;

  FMouseDownPos := Point(X, Y);

  AItem := FItems.ItemAtPos(x, y);
  if (ssShift in Shift) then ADeselectOthers := False;
  if (AItem <> nil) and (AItem.Selected) then ADeselectOthers := False;

  if ADeselectOthers then FItems.DeselectAll;

  if (AItem <> nil) then
  begin
    case (ssShift in Shift) of
      True: AItem.Selected := not AItem.Selected;
      False: AItem.Selected := True;
    end;
  end;

  FDragging := True;
  FMouseXY := Point(X, Y);
  // virtual rectangle containing all selected objects (or just the one)
  aSelectedItemsPixelRect := SelectedItemsPixelRect;
  // delta to the top left of the selection(s) (negative values)
  FMouseObjectOffset.X := FMouseXY.X - aSelectedItemsPixelRect.TopLeft.X;
  FMouseObjectOffset.Y := FMouseXY.Y - aSelectedItemsPixelRect.TopLeft.Y;

  if (AItem <> nil) and (AItem.Selected) and (Assigned(FOnSelectItem)) then
    FOnSelectItem(Self, AItem);
end;

procedure TDesignBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AItem: TDesignBoxBaseItem;
begin
  inherited;
  X := X - FPageOffset.X;
  Y := Y - FPageOffset.Y;
  if (FDragging) and (FItems.SelectedCount > 0) then
  begin
    // this moves the items
    for AItem in FItems do
    begin
      if AItem.Selected and (canMove in AItem.Options) then
        AItem.OffsetByPixels(X - FMouseXY.X, Y - FMouseXY.Y, False);
    end;
    Redraw;
  end;
  FMouseXY := Point(X, Y);
  if FDragging then
    Invalidate;
end;

procedure TDesignBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ADragArea: TRect;
  AItem: TDesignBoxBaseItem;
begin
  inherited;
  X := X - FPageOffset.X;
  Y := Y - FPageOffset.Y;

  if (FDragging) and (FItems.SelectedCount = 0) then
  begin
    ADragArea := Rect(Min(FMouseDownPos.X, X), Min(FMouseDownPos.Y, Y), Max(FMouseDownPos.X, X), Max(FMouseDownPos.Y, Y));
    if (ADragArea.Width > 4) and (ADragArea.Height > 4) then
    for AItem in FItems do
    begin
      AItem.Selected := AItem.RectsIntersect(ADragArea);
    end;
  end
  else
  begin
    // snap objects if required...
    if (FGridOptions.SnapToGrid) then
    begin
      for AItem in SelectedItems do
        AItem.FixToSnapRect;
    end;
  end;




  FDragging := False;
  Redraw; // invalidate
  RecordSnapshot;

end;

procedure TDesignBox.OnBrushChanged(Sender: TObject);
var
  AItem: TDesignBoxBaseItem;
  AIntf: IBrushObject;
begin
  for AItem in SelectedItems do
  begin
    if Supports(AItem, IBrushObject, AIntf) then
      AIntf.Brush.Assign(FBrush);
  end;
  Redraw;
end;

procedure TDesignBox.OnFontChanged(Sender: TObject);
var
  AItem: TDesignBoxBaseItem;
  AIntf: IFontObject;
begin
  for AItem in SelectedItems do
  begin
    if Supports(AItem, IFontObject, AIntf) then
      AIntf.Font.Assign(FFont);
  end;
  Redraw;
end;

procedure TDesignBox.OnPenChanged(Sender: TObject);
var
  AItem: TDesignBoxBaseItem;
  AIntf: IPenObject;
begin
  for AItem in SelectedItems do
  begin
    if Supports(AItem, IPenObject, AIntf) then
      AIntf.Pen.Assign(FPen);
  end;
  Redraw;
end;

procedure TDesignBox.DrawRulers(ACanvas: TCanvas);
var
  APxPerMM: Extended;
  AMm: integer;
  AMarkSize: integer;
  tz : TSize;
const
  C_BIG_MARK = 10;
  C_LITTLE_MARK = 5;
begin
  APxPerMm := C_DPI/ 25.4;

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
  while (AMm*APxPerMM) < Width do
  begin
    AMarkSize := 0;
    if (AMm) mod 2 = 0 then AMarkSize := C_LITTLE_MARK;
    if (AMm) mod 10 = 0 then AMarkSize := C_BIG_MARK;
    if AMarkSize > 0 then
    begin
      ACanvas.Polyline([Point(Round(AMm*APxPerMm)+FPageOffset.X, FPageOffset.Y-AMarkSize),
                        Point(Round(AMm*APxPerMm)+FPageOffset.X, FPageOffset.Y)]);
      if AMarkSize = 10 then
      begin
        tz := ACanvas.TextExtent(AMm.ToString);
        ACanvas.TextOut((Round(AMm*APxPerMm)+FPageOffset.X) - (tz.Width div 2), FPageOffset.Y-20, AMm.ToString);
      end;
    end;
    Inc(AMm);
  end;

  AMm := 1;
  while (AMm*APxPerMM) < Height do
  begin
    AMarkSize := 0;
    if AMm mod 2 = 0 then AMarkSize := C_LITTLE_MARK;
    if AMm mod 10 = 0 then AMarkSize := C_BIG_MARK;
    if AMarkSize > 0 then
    begin
      ACanvas.Polyline([Point(FPageOffset.X-AMarkSize, Round(AMm*APxPerMm)+FPageOffset.Y),
                        Point(FPageOffset.X, Round(AMm*APxPerMm)+FPageOffset.Y)]);
      if AMarkSize = 10 then
      begin
        tz := ACanvas.TextExtent(AMm.ToString);
        ACanvas.TextOut(FPageOffset.X-tz.width - C_LITTLE_MARK, (Round(AMm*APxPerMm)+FPageOffset.Y) - (tz.Height div 2), AMm.ToString);
      end;
    end;
    Inc(AMm);
  end;
end;

procedure TDesignBox.DrawGrid(ACanvas: TCanvas);
var
  AGridSize: single;
  x, y: single;
  APageWidth, APageHeight: integer;
begin
  AGridSize := MmToPixels(FGridOptions.SizeMm);
  APageWidth := Round(MmToPixels(FPageSizeMm.Width));
  APageHeight := Round(MmToPixels(FPageSizeMm.Height));

  ACanvas.Pen.Color := FGridOptions.Color;
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Width := 1;

  X := 0;
  while X < APageWidth do
  begin
    ACanvas.Pen.Color := FGridOptions.Color;
    ACanvas.Pen.Mode := pmCopy;
    ACanvas.MoveTo(Round(x), 0);
    ACanvas.LineTo(Round(x), APageHeight);
    X := X + AGridSize;
  end;

  Y := 0;
  while Y < APageHeight do
  begin
    ACanvas.MoveTo(0, Round(y));
    ACanvas.LineTo(APageWidth, Round(y));
    Y := Y + AGridSize;
  end;

end;

procedure TDesignBox.Paint;
var
  ARect: TRect;
  aRulerWidth: integer;
begin
  aRulerWidth := Canvas.TextWidth('9999');
  case FRulerOptions.Visible of
    True: FPageOffset := Point(aRulerWidth, aRulerWidth); // slightly narrower
    False: FPageOffset := Point(0, 0);
  end;
  Canvas.Brush.Color := clLtGray;

  Canvas.FillRect(ClientRect);
  Canvas.FrameRect(ClientRect);

  if (FBuffer.Width = 0) then
    Redraw;

  if FRulerOptions.Visible then
    DrawRulers(Canvas);

  Canvas.Draw(FPageOffset.X, FPageOffset.Y, FBuffer);

  if (FDragging) and (FItems.SelectedCount = 0) then
  begin
    ARect := Rect(FMouseDownPos.X+FPageOffset.X,
                  FMouseDownPos.Y+FPageOffset.Y,
                  FMouseXY.X+FPageOffset.X,
                  FMouseXY.Y+FPageOffset.Y);
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Mode := pmNot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ARect);
  end;
  Canvas.Brush.Style := bsClear;
  Canvas.Brush.Color := clDkGray;
  Canvas.FrameRect(ClientRect);

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
  ASnapRect: TRect;
  AGridPixelSize: single;
  ASnapOffset: TPoint;
begin
  //FBuffer.Free;
  //FBuffer := TBitmap.Create;
  ResizeCanvas;
  FBuffer.Canvas.Brush.Color := BackGroundColor;
  FBuffer.Canvas.Brush.Style := bsSolid;
  FBuffer.Canvas.Pen.Color := clBlack;
  FBuffer.Canvas.Rectangle(0, 0, FBuffer.Width, FBuffer.Height);

  if FGridOptions.Visible then
    DrawGrid(FBuffer.Canvas);

  if FGridOptions.SnapToGrid then
  begin
    GetSelectedItems;
    AGridPixelSize := MmToPixels(FGridOptions.FSizeMm);

    ASnapRect := SelectedItemsPixelRect;
    //FBuffer.Canvas.Rectangle(ASnapRect);

    ASnapOffset.x := Round(Round(ASnapRect.Left / AGridPixelSize) * AGridPixelSize) - ASnapRect.Left;
    ASnapOffset.Y := Round(Round(ASnapRect.Top / AGridPixelSize) * AGridPixelSize) - ASnapRect.Top;
    ASnapRect.Offset(ASnapOffset);

    ASnapOffset.X := ASnapRect.Left - SelectedItemsPixelRect.Left;
    ASnapOffset.Y := ASnapRect.Top - SelectedItemsPixelRect.Top;

    //FBuffer.Canvas.Pen.Color := clRed;
    //FBuffer.Canvas.Rectangle(ASnapRect);
  end
  else
    ASnapOffset := Point(0,0);


  if FItems.Count > 0 then
  begin
    for AItem in FItems do
    begin
      case AItem.Selected of
        True: AItem.PaintToCanvas(FBuffer.Canvas, ASnapOffset);
        False: AItem.PaintToCanvas(FBuffer.Canvas, Point(0, 0));
      end;
      if AItem.Selected then
        AItem.DrawSelectedRect(FBuffer.Canvas, ASnapOffset);
    end;
  end;


  FBuffer.Canvas.Brush.Style := bsClear;
  FBuffer.Canvas.Pen.Color := clBlack;
  FBuffer.Canvas.Rectangle(0,0, FBuffer.Width, FBuffer.Height);

  Invalidate;
end;

procedure TDesignBox.Resize;
begin
  inherited;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  Redraw;
end;

procedure TDesignBox.ResizeCanvas;
begin
  FBuffer.SetSize(Round((C_DPI / 25.4)*FPageSizeMM.Width)+1, Round((C_DPI / 25.4)*FPageSizeMM.Height)+1);
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

function TDesignBox.SelectedItemsPixelRect: TRect;
var
  II: Integer;
begin
  if FSelectedItems.Count = 0 then
  begin
    result.left := 0; result.Top := 0; result.Width := 0; result.Height := 0;
    exit;
  end;

  // find the maximum extent of all selected objects as a Rect in MM.
  result.left := fSelectedItems[0].RectPixels.Left;
  result.Top := fSelectedItems[0].RectPixels.Top;
  result.Right:= FSelectedItems[0].RectPixels.Left + FSelectedItems[0].RectPixels.Width;
  result.Bottom := FSelectedItems[0].RectPixels.Top + FSelectedItems[0].RectPixels.Height;

  if fSelectedItems.Count < 2 then exit;

  for II := 1 to fSelectedItems.Count-1 do
  begin
    if fSelectedItems[II].RectPixels.Left < result.Left then
      result.Left := fSelectedItems[II].RectPixels.Left;
    if fSelectedItems[II].RectPixels.Top < result.Top then
      result.Top := fSelectedItems[II].RectPixels.Top;
    if fSelectedItems[II].RectPixels.Width + fSelectedItems[II].RectPixels.Left > result.Right then
      result.Right := fSelectedItems[II].RectPixels.Width + fSelectedItems[II].RectPixels.Left;
    if fSelectedItems[II].RectPixels.Height + fSelectedItems[II].RectPixels.Top > result.Bottom then
      result.Bottom := fSelectedItems[II].RectPixels.Height + fSelectedItems[II].RectPixels.Top;
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

procedure TDesignBox.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TDesignBox.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TDesignBox.SetGridOptions(const Value: TDesignBoxGridOptions);
begin
  FGridOptions.Assign(Value);
end;

procedure TDesignBox.SetPageHeightMM(const Value: integer);
begin
  FPageSizeMM.Height := Value;
  Redraw;
end;

procedure TDesignBox.SetPageSize(APageSize: TSize);
begin
  FPageSizeMM.Width := APageSize.Width;
  FPageSizeMM.Height := APageSize.Height;
  Redraw;
end;

procedure TDesignBox.SetPageWidthMM(const Value: integer);
begin
  FPageSizeMM.Width := Value;
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

procedure TDesignBox.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
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

procedure TDesignBoxItemText.PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint);
begin
  inherited;
  ACanvas.Rectangle(RectPixels);
  ACanvas.Font.Assign(FFont);
  // calculate width/height...
  FWidthMM := (ACanvas.TextWidth(FText) / C_DPI) * 25.4;
  FHeightMM := (ACanvas.TextHeight(FText) / C_DPI) * 25.4;
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(RectPixels.Left, RectPixels.Top, FText);
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
  FOptions := [canMove, canSize, canDelete];
end;

destructor TDesignBoxBaseItem.Destroy;
begin
  inherited;
end;

procedure TDesignBoxBaseItem.DrawSelectedRect(ACanvas: TCanvas; ASnapOffset: TPoint);
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
    OffsetRect(ARect, ASnapOffset.X, ASnapOffset.Y);
    ACanvas.Rectangle(ARect);
  end;
end;

procedure TDesignBoxBaseItem.FixToSnapRect;
begin

  FPositionMM.X := PixelsToMM(FSnapRect.Left);
  FPositionMM.Y := PixelsToMM(FSnapRect.Top);
end;

function TDesignBoxBaseItem.GetLeftMM: single;
begin
  Result := FPositionMM.X;
end;

function TDesignBoxBaseItem.GetTopMM: single;
begin
  Result := FPositionMM.Y;
end;

procedure TDesignBoxBaseItem.OffsetByPixels(X, Y: integer; const ARedraw: Boolean = True);
begin
  FPositionMM.X := FPositionMM.X + ((x / C_DPI) * 25.4);
  FPositionMM.Y := FPositionMM.Y + ((y / C_DPI) * 25.4);
  if ARedraw then
    Changed;
end;

procedure TDesignBoxBaseItem.PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint);
begin
  FSnapRect := RectPixels;
  OffsetRect(FSnapRect, ADrawOffset.X, ADrawOffset.Y);
end;

function TDesignBoxBaseItem.PointInRgn(x, y: integer): Boolean;
begin
  Result := PtInRect(RectPixels, Point(x,y));
end;

function TDesignBoxBaseItem.RectPixels: TRect;
begin
  Result.Left := Round((C_DPI / 25.4) *FPositionMM.X);
  Result.Top := Round((C_DPI / 25.4) *FPositionMM.Y);
  Result.Right := Round(((FPositionMM.X+FWidthMM) / 25.4) * C_DPI);
  Result.Bottom := Round(((FPositionMM.Y + FHeightMM) / 25.4) * C_DPI);
end;

function TDesignBoxBaseItem.RectsIntersect(ARect: TRect): Boolean;
var
  x1, y1, x2, y2: integer;
begin
  x1 := Min(ARect.Left, ARect.Right);
  x2 := Max(ARect.Left, ARect.Right);
  y1 := Min(ARect.Top, ARect.Bottom);
  y2 := Max(ARect.Top, ARect.Bottom);
  Result := IntersectRect(Rect(x1, y1, x2, y2), RectPixels);
end;

procedure TDesignBoxBaseItem.LoadFromJson(AJson: TJsonObject);
begin
  FPositionMM.X := StrToFloatDef(AJson.Values['x'].Value, 0); // AJson.F['x'];
  FPositionMM.Y := StrToFloatDef(AJson.Values['y'].Value, 0);
  FWidthMM := StrToFloatDef(AJson.Values['width'].Value, 0);
  FHeightMM := StrToFloatDef(AJson.Values['height'].Value, 0);
end;

procedure TDesignBoxBaseItem.SaveToJson(AJson: TJsonObject);
begin
  AJson.AddPair('obj', ClassName);
  AJson.AddPair('x', FPositionMM.X.ToString);
  AJson.AddPair('y', FPositionMM.Y.ToString);
  AJson.AddPair('width', FWidthMM.ToString);
  AJson.AddPair('height',FHeightMM.ToString);
end;

procedure TDesignBoxBaseItem.SetLeftMM(const Value: single);
begin
  FPositionMM.X := Value;
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

procedure TDesignBoxBaseItem.SetTopMM(const Value: single);
begin
  FPositionMM.Y := Value;
end;

{ TDesignBoxItemList }

function TDesignBoxItemList.AddRectangle(ABoundsMm: TRectF; const ARoundnessMm: single = 0): TDesignBoxItemRectangle;
begin
  Result := Add(TDesignBoxItemRectangle, ABoundsMm) as TDesignBoxItemRectangle;
  Result.RoundnessMm := ARoundnessMm;
end;

function TDesignBoxItemList.AddEllipse(ABoundsMm: TRectF): TDesignBoxItemEllipse;
begin
  Result := Add(TDesignBoxItemEllipse, ABoundsMm) as TDesignBoxItemEllipse;
end;

function TDesignBoxItemList.AddText(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
begin
  if Trim(AText) = ''  then
    AText := '<empty>';
  Result := TDesignBoxItemText.Create(FDesignBox);
  Result.LeftMM := ALeftMM;
  Result.TopMM := ATopMM;
  Result.Text := AText;
  Result.Brush.Assign(FDesignBox.Brush);
  Add(Result);
  FDesignBox.Redraw;
  FDesignBox.RecordSnapshot;
end;

function TDesignBoxItemList.Add(AClass: TDesignBoxBaseItemClass; ABoundsMm: TRectF): TDesignBoxBaseItem;
var
  ABrushIntf: IBrushObject;
  AFontIntf: IFontObject;
  APenIntf: IPenObject;
begin
  Result := AClass.Create(FDesignBox);
  if Supports(Result, IBrushObject, ABrushIntf) then ABrushIntf.Brush.Assign(FDesignBox.Brush);
  if Supports(Result, IFontObject, ABrushIntf) then AFontIntf.Font.Assign(FDesignBox.Font);
  if Supports(Result, IPenObject, APenIntf) then APenIntf.Pen.Assign(FDesignBox.Pen);
  Result := Add(Result, ABoundsMm);
end;

function TDesignBoxItemList.Add(AItem: TDesignBoxBaseItem; ABoundsMm: TRectF): TDesignBoxBaseItem;
begin
  Result := AItem;
  Result.LeftMM := ABoundsMm.Left;
  Result.TopMM := ABoundsMm.Top;
  if ABoundsMm.Width > 0 then Result.FWidthMM := ABoundsMm.Height;
  if ABoundsMm.Height > 0 then Result.FHeightMM := ABoundsMm.Width;
  Add(result);
  FDesignBox.ReDraw;
  FDesignBox.RecordSnapshot;

end;

function TDesignBoxItemList.AddGraphic(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
begin
  Result := TDesignBoxItemGraphic.Create(FDesignBox);
  Result.Graphic.Assign(AGraphic);
  Add(Result, RectF(ALeftMM, ATopMM, 0, 0));
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

procedure TDesignBoxItemGraphic.PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint);
var
  ARect: TRect;
begin
  inherited;
  ARect := RectPixels;
  // calculate width/height...
  FWidthMM := (FGraphic.Width / C_DPI) * 25.4;
  FHeightMM := (FGraphic.Height / C_DPI) * 25.4;
  OffsetRect(ARect, ADrawOffset.X, ADrawOffset.Y);
  ACanvas.Draw(ARect.Left, ARect.Top, FGraphic.Graphic);
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
    AImg := TJsonObject.Create; //AJson.Values['img'] as TJSONObject;
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

procedure TDesignBoxItemFilledShape.PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint);
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

procedure TDesignBoxItemSizable.DrawSelectedRect(ACanvas: TCanvas; ASnapOffset: TPoint);
var
  ii: TDragHandlePos;
  aRect, handleRect : TRect;
  handlePoints : array[TDragHandlePos] of TPoint; // array of the centrepoints of each selectHandle
const
  CH_RADIUS = 4; // 16 pixel square grab handlePoints
begin
  inherited DrawSelectedRect(ACanvas, ASnapOffset);

  if FSelected then
  begin
    aRect := RectPixels;
    InflateRect(aRect, C_SELECTBOX_INFLATE, C_SELECTBOX_INFLATE); // duplicate the inherited version
    ACanvas.Pen.Color := C_HIGHLIGHT_COLOR;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := ACanvas.Pen.Color;

    handlePoints[dhTopLeft]     := aRect.TopLeft;
    handlePoints[dhTopRight]    := Point(aRect.Right, aRect.Top);
    handlePoints[dhBottomRight] := aRect.BottomRight;
    handlePoints[dhBottomLeft]  := Point(aRect.Left, aRect.Bottom);

    for ii := Low(TDragHandlePos) to High(TDragHandlePos) do
    begin
      handleRect := Rect(handlePoints[ii].X - CH_RADIUS, handlePoints[ii].Y - CH_RADIUS, handlePoints[ii].X + CH_RADIUS, handlePoints[ii].Y + CH_RADIUS);
      OffsetRect(handleRect, ASnapOffset.X, ASnapOffset.Y);
      fSelectHandles[ii] := handleRect; // save for comparison in PointInRgn later
      ACanvas.Rectangle(handleRect);
    end;
  end;
end;

procedure TDesignBoxItemSizable.SetCoords(ALeftMM, ATopMM, ARightMM, ABottomMM: single);
begin
  FPositionMM.X := ALeftMM;
  FPositionMM.Y := ATopMM;
  FWidthMM := ARightMM-ALeftMM;
  FHeightMM := ABottomMM-ATopMM;
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
  SaveSnapshot(True);
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

procedure TDesignBoxItemEllipse.PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint);
var
  ARect: TRect;
begin
  inherited;
  ARect := RectPixels;
  OffsetRect(ARect, ADrawOffset.X, ADrawOffset.Y);
  ACanvas.Ellipse(ARect);
end;

{ TDesignBoxItemRectangle }

constructor TDesignBoxItemRectangle.Create(ADesignBox: TDesignBox);
begin
  inherited;
  FRoundnessMm := 0;
end;

procedure TDesignBoxItemRectangle.PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint);
var
  r: TRect;
begin
  inherited;
  r := RectPixels;
  //ACanvas.RoundRect(r, Round(MmToPixels(FRoundnessMm)), Round(MmToPixels(FRoundnessMm)));
  //if ADrawOffset <> Point(0,0) then
  begin
    OffsetRect(r, ADrawOffset.X, ADrawOffset.Y);
    ACanvas.RoundRect(r, Round(MmToPixels(FRoundnessMm)), Round(MmToPixels(FRoundnessMm)));
  end;
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
  FColor := $00EEEEEE;
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
  result := Round((C_DPI / 25.4) * FSizeMm);
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

procedure TDesignBoxItemOutlineShape.PaintToCanvas(ACanvas: TCanvas; ADrawOffset: TPoint);
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
