unit DesignBox;

{ TODO 2 : Zoom / Scale facility }
{ TODO 3 : Editable text items - no keyboard events on TGraphicControl - maybe need to inherit from TWinControl and add our own Canvas - or just ignore and go with a "property sheet" UI paradigm ? }
{ TODO 1 : Lines - aka two point selection) }
{ TODO 1 : Resize graphics (4 point selection box) }
{ TODO 1 : Shapes - fill styles FDiagonal etc }
{ TODO 2 : Actual bar code items - i.e. dynamically draw the QR/DataMatrix/EAN barcode not just an image }
{ TODO 2 : background image }
{ TODO 2 : rulers ?  }
{ TODO 3 : Shift/Ctrl Key item selection (TGraphicControl lack of keyboard may be an issue here too) }

interface

uses
  Windows, Messages, System.SysUtils, System.Classes, Vcl.Controls, System.Types, System.Generics.Collections,
  Graphics, System.Generics.Defaults, Json;

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
    procedure OffsetByPixels(X, Y: integer);
    procedure PaintToCanvas(ACanvas: TCanvas); virtual; abstract;
    procedure DrawSelectedRect(ACanvas: TCanvas); virtual;
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    destructor Destroy; override;
    function PointInRgn(x, y: integer): Boolean;
    function RectsIntersect(ARect: TRect): Boolean;
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

  TDesignBoxItemText = class(TDesignBoxBaseItem, IBrushObject, IFontObject, IPenObject)
  private
    FText: string;
    FFont: TDesignFont;
    FBrush: TDesignBrush;
    FPen: TDesignPen;
    function GetFont: TFont;
    function GetBrush: TBrush;
    function GetPen: TPen;
    procedure DoFontChange(Sender: TObject);
    procedure DoBrushChange(Sender: TObject);
    procedure DoPenChange(Sender: TObject);
    procedure SetText(const AText: string);
    procedure SetFont(const Value: TFont);
    procedure SetBrush(const Value: TBrush);
    procedure SetPen(const Value: TPen);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Text: string read FText write SetText;
    property Font: TFont read GetFont write SetFont;
    property Brush: TBrush read GetBrush write SetBrush;
    property Pen: TPen read GetPen write SetPen;
  end;

  TDragHandlePos = (dhTopLeft,dhTopRight,dhBottomRight,dhBottomLeft);  // clockwise from (0,0)
  TDragHandleRectArray = array[TDragHandlePos] of TRect;

  /// base class for items with a set of grab handles - note Text items will not have this as Font size & text property will determine bounds
  TDesignBoxItemSizable = class(TDesignBoxBaseItem)
  private
    fSelectHandles: TDragHandleRectArray;
    //fIsDraggingHandle: boolean;      // is user dragging via a select handle?
    //fSelectedHandle: TDragHandlePos; // if so which one ?
  protected
    procedure DrawSelectedRect(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
  end;

  TDbShape = (dbEllipse, dbRectangle, dbLine);

  TDesignBoxItemShape = class(TDesignBoxItemSizable, IBrushObject, IPenObject)
  private
    FShape: TdbShape;
    FBrush: TDesignBrush;
    FPen: TDesignPen;

    procedure DoBrushChange(Sender: TObject);
    procedure DoPenChange(Sender: TObject);
    function GetBrush: TBrush;
    function GetPen: TPen;
    procedure SetBrush(const Value: TBrush);
    procedure SetPen(const Value: TPen);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Brush: TBrush read GetBrush write SetBrush;
    property Pen: TPen read GetPen write SetPen;
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
    function AddShape(AShape: TDbShape; ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemShape;
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    function AddText(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
    function AddGraphic(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
    function AddRectangle(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemShape;
    function AddEllipse(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemShape;
    function AddLine(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemShape;
    function Add(AClass: TDesignBoxBaseItemClass; ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxBaseItem; overload;
    function ItemAtPos(x, y: integer): TDesignBoxBaseItem;
    property DesignBox: TDesignBox read fDesignBox;

    procedure LoadFromJson(AJson: TJsonObject);
    procedure SaveToJson(AJson: TJsonObject);
    procedure DeleteSelected;
    property SelectedCount: integer read GetSelectedCount;
    procedure DeselectAll;

  end;

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
    FShowRulers: Boolean;
    fDrawCount : integer;
    procedure OnBrushChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure OnPenChanged(Sender: TObject);

    function GetSelectedItems: TDesignBoxItemList;
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);
    procedure SetBackgroundColor(const Value: TColor);
    procedure RecordSnapshot;
    procedure SetShowRulers(const Value: Boolean);
    procedure DrawRulers(ACanvas: TCanvas);
    procedure ResizeCanvas;
    function GetPageHeightMM: integer;
    function GetPageWidthMM: integer;
    procedure SetPageHeightMM(const Value: integer);
    procedure SetPageWidthMM(const Value: integer);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure WMEraseBackground(var message: TMessage); message WM_ERASEBKGND;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
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
    procedure SendToBack;
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
    property DrawCount: integer read fDrawCount write fDrawCount;
  published
    property Align;
    property OnSelectItem: TDesignBoxSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property PopupMenu;
    property ShowRulers: Boolean read FShowRulers write SetShowRulers default True;
    property PageWidthMM: integer read GetPageWidthMM write SetPageWidthMM;
    property PageHeightMM: integer read GetPageHeightMM write SetPageHeightMM;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

uses System.NetEncoding, PngImage, Jpeg, Math, System.UITypes;

const
  C_HIGHLIGHT_COLOR = clHotlight;
  C_SELECTBOX_INFLATE = 2;

procedure Register;
begin
  RegisterComponents('Samples', [TDesignBox]);
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
  FUndoList := TDesignUndoList.Create(Self);

  FPageSizeMM.Width := 100;
  FPageSizeMM.Height := 100;

  ResizeCanvas;
  FBrush.OnChange := OnBrushChanged;
  FFont.OnChange := OnFontChanged;;
  FPen.OnChange := OnPenChanged;
  FBackgroundColor := clWhite;
  FShowRulers := True;
  FUpdating := False;
  fDrawCount := 0;
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
    if assigned(AJson.Values['rulers']) then
      ShowRulers := TJsonBool(AJson.values['rulers']).asBoolean;
    if assigned(AJson.Values['pageWidthMM']) then
      PageWidthMM := TJsonNumber(AJson.values['pageWidthMM']).asInt;
    if assigned(AJson.Values['pageHeightMM']) then
      PageHeightMM := TJsonNumber(AJson.values['pageHeightMM']).asInt;
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
    for AItem in FItems do
    begin
      if AItem.Selected and (canMove in AItem.Options) then
        AItem.OffsetByPixels(X- FMouseXY.X, Y - FMouseXY.Y);
    end;
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
  end;
  FDragging := False;
  Invalidate;
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
  tw, th: integer;
begin
  APxPerMm := 96 / 25.4;


  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Style := psClear;
  ACanvas.FillRect(Rect(0, 0, Width, FPageOffset.Y));
  ACanvas.FillRect(Rect(0, 0, FPageOffset.X, Height));
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Polyline([Point(0, FPageOffset.Y), Point(Width, FPageOffset.Y)]);
  ACanvas.Polyline([Point(FPageOffset.X, 0), Point(FPageOffset.X, Height)]);

  AMm := 1;
  while (AMm*APxPerMM) < Width do
  begin
    AMarkSize := 0;
    if (AMm) mod 2 = 0 then AMarkSize := 5;
    if (AMm) mod 10 = 0 then AMarkSize := 10;
    if AMarkSize > 0 then
    begin
      ACanvas.Polyline([Point(Round(AMm*APxPerMm)+FPageOffset.X, FPageOffset.Y-AMarkSize),
                        Point(Round(AMm*APxPerMm)+FPageOffset.X, FPageOffset.Y)]);
      if AMarkSize = 10 then
      begin
        tw := ACanvas.TextWidth(AMm.ToString);
        ACanvas.TextOut((Round(AMm*APxPerMm)+FPageOffset.X) - (tw div 2), FPageOffset.Y-20, AMm.ToString);
      end;
    end;
    Inc(AMm);
  end;

  AMm := 1;
  while (AMm*APxPerMM) < Height do
  begin
    AMarkSize := 0;
    if AMm mod 2 = 0 then AMarkSize := 5;
    if AMm mod 10 = 0 then AMarkSize := 10;
    if AMarkSize > 0 then
    begin
      ACanvas.Polyline([Point(FPageOffset.X-AMarkSize, Round(AMm*APxPerMm)+FPageOffset.Y),
                        Point(FPageOffset.X, Round(AMm*APxPerMm)+FPageOffset.Y)]);
      if AMarkSize = 10 then
      begin
        th := ACanvas.TextHeight(AMm.ToString);
        ACanvas.TextOut(FPageOffset.X-20, (Round(AMm*APxPerMm)+FPageOffset.Y) - (th div 2), AMm.ToString);
      end;
    end;
    Inc(AMm);
  end;


end;

procedure TDesignBox.Paint;
var
  ARect: TRect;
begin
  case FShowRulers of
    True: FPageOffset := Point(40, 40);
    False: FPageOffset := Point(0, 0);
  end;
  Canvas.Brush.Color := clLtGray;
  Canvas.FillRect(ClientRect);
  Canvas.FrameRect(ClientRect);

  if (FBuffer.Width = 0) then
    Redraw;

  if FShowRulers then
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
begin
  //FBuffer.Free;
  //FBuffer := TBitmap.Create;
  ResizeCanvas;

  inc(fDrawCount);


  FBuffer.Canvas.Brush.Color := BackGroundColor;
  FBuffer.Canvas.Brush.Style := bsSolid;
  FBuffer.Canvas.Pen.Color := clBlack;
  FBuffer.Canvas.Rectangle(0,0, FBuffer.Width, FBuffer.Height);
  //FBuffer.Canvas.Brush.Style := bsClear;
  if FItems.Count > 0 then
  begin
    for AItem in FItems do
    begin
      AItem.PaintToCanvas(FBuffer.Canvas);
      if AItem.Selected then
        AItem.DrawSelectedRect(FBuffer.Canvas);
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
  FBuffer.SetSize(Round((96/25.4)*FPageSizeMM.Width), Round((96/25.4)*FPageSizeMM.Height));
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
    AJson.AddPair('rulers', TJsonBool.Create(ShowRulers));
    AJson.AddPair('pageWidthMM', TJsonNumber.Create(PageWidthMM));
    AJson.AddPair('pageHeightMM', TJsonNumber.Create(PageHeightMM));
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

procedure TDesignBox.SendToBack;
var
  AItem: TDesignBoxBaseItem;
begin
  for AItem in SelectedItems do
  begin
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

procedure TDesignBox.SetShowRulers(const Value: Boolean);
begin
  if FShowRulers <> Value then
  begin
    FShowRulers := Value;
    Invalidate;
  end;
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
  FBrush := TDesignBrush.Create;
  FPen := TDesignPen.Create;

  FFont.Size := 16;
  FFont.OnChange := DoFontChange;

  FBrush.Color := clWhite;
  FBrush.Style := bsSolid;
  FBrush.OnChange := DoBrushChange;

  FPen.Color := clBlack;
  FPen.Style := psSolid;
  FPen.OnChange := DoPenChange;
end;

destructor TDesignBoxItemText.Destroy;
begin
  inherited;
  FFont.Free;
  FBrush.Free;
  FPen.Free;
end;

procedure TDesignBoxItemText.DoBrushChange(Sender: TObject);
begin
  Changed;
end;

procedure TDesignBoxItemText.DoFontChange(Sender: TObject);
begin
  Changed;
end;

procedure TDesignBoxItemText.DoPenChange(Sender: TObject);
begin
  Changed;
end;

function TDesignBoxItemText.GetBrush: TBrush;
begin
  Result := FBrush;
end;

function TDesignBoxItemText.GetFont: TFont;
begin
  Result := FFont;
end;

function TDesignBoxItemText.GetPen: TPen;
begin
  Result := FPen;
end;

procedure TDesignBoxItemText.PaintToCanvas(ACanvas: TCanvas);
begin
  ACanvas.Font.Assign(FFont);
  ACanvas.Brush.Assign(FBrush);
  ACanvas.Pen.Assign(FPen);
  // calculate width/height...

  FWidthMM := (ACanvas.TextWidth(FText) / 96) * 25.4;
  FHeightMM := (ACanvas.TextHeight(FText) / 96) * 25.4;
  ACanvas.Rectangle(RectPixels);
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(RectPixels.Left, RectPixels.Top, FText);
end;

procedure TDesignBoxItemText.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  Text := AJson.Values['text'].Value;
  FBrush.LoadFromJson(AJson);
  FFont.LoadFromJson(AJson);
  FPen.LoadFromJson(AJson);
end;



procedure TDesignBoxItemText.SaveToJson(AJson: TJsonObject);
begin
  inherited;
  AJson.AddPair('text', Text);
  FBrush.SaveToJson(AJson);
  FFont.SaveToJson(AJson);
  FPen.SaveToJson(AJson);
end;

procedure TDesignBoxItemText.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TDesignBoxItemText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TDesignBoxItemText.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
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
    ACanvas.Rectangle(ARect);
  end;
end;

function TDesignBoxBaseItem.GetLeftMM: single;
begin
  Result := FPositionMM.X;
end;

function TDesignBoxBaseItem.GetTopMM: single;
begin
  Result := FPositionMM.Y;
end;

procedure TDesignBoxBaseItem.OffsetByPixels(X, Y: integer);
begin
  FPositionMM.X := FPositionMM.X + ((x / 96) * 25.4);
  FPositionMM.Y := FPositionMM.Y + ((y / 96) * 25.4);
  Changed;
end;

function TDesignBoxBaseItem.PointInRgn(x, y: integer): Boolean;
begin
  Result := PtInRect(RectPixels, Point(x,y));
end;

function TDesignBoxBaseItem.RectPixels: TRect;
begin
  Result.Left := Round((96 / 25.4) *FPositionMM.X);
  Result.Top := Round((96 / 25.4) *FPositionMM.Y);
  Result.Right := Round(((FPositionMM.X+FWidthMM) / 25.4) * 96);
  Result.Bottom := Round(((FPositionMM.Y + FHeightMM) / 25.4) * 96);
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
        {
procedure TDesignBoxBaseItem.SendToBack;
begin
  FDesignBox.Items.SendToBack(Self);
end;
        }
procedure TDesignBoxBaseItem.SetLeftMM(const Value: single);
begin
  FPositionMM.X := Value;
end;

procedure TDesignBoxBaseItem.SetOptions(const Value: TItemOptions);
begin
  fOptions := Value;
//  Changed;
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

function TDesignBoxItemList.AddLine(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemShape;
begin
  Result := AddShape(dbLine, ALeftMM, ATopMM, ARightMM, ABottomMM);
end;

function TDesignBoxItemList.AddRectangle(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemShape;
begin
  Result := AddShape(dbRectangle, ALeftMM, ATopMM, ARightMM, ABottomMM);
end;

function TDesignBoxItemList.AddShape(AShape: TDbShape; ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemShape;
begin
  Result := TDesignBoxItemShape.Create(FDesignBox);
  Result.FShape := AShape;
  Result.LeftMM := ALeftMM;
  Result.TopMM := ATopMM;
  Result.FWidthMM := ARightMM - ALeftMM;
  Result.FHeightMM := ABottomMM - ATopMM;
  Result.Brush.Assign(FDesignBox.Brush);
  Result.Pen.Assign(FDesignBox.Pen);
  Add(Result);
  FDesignBox.Redraw;
  FDesignBox.RecordSnapshot;
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

function TDesignBoxItemList.Add(AClass: TDesignBoxBaseItemClass; ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxBaseItem;
begin
  result := AClass.Create(FDesignBox);
  Result.LeftMM := ALeftMM;
  Result.TopMM := ATopMM;
  Add(result);
  FDesignBox.ReDraw;
  FDesignBox.RecordSnapshot;
end;

function TDesignBoxItemList.AddEllipse(ALeftMM, ATopMM, ARightMM, ABottomMM: single): TDesignBoxItemShape;
begin
  Result := AddShape(dbEllipse, ALeftMM, ATopMM, ARightMM, ABottomMM);
end;

function TDesignBoxItemList.AddGraphic(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
begin
  Result := TDesignBoxItemGraphic.Create(FDesignBox);
  Result.LeftMM := ALeftMM;
  Result.TopMM := ATopMM;
  Result.Graphic.Assign(AGraphic);
  Add(Result);
  FDesignBox.Redraw;
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
begin
  // calculate width/height...
  FWidthMM := (FGraphic.Width / 96) * 25.4;
  FHeightMM := (FGraphic.Height / 96) * 25.4;
  ACanvas.Draw(RectPixels.Left, RectPixels.Top, FGraphic.Graphic);
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

{ TDesignBoxItemShape }

constructor TDesignBoxItemShape.Create(ADesignBox: TDesignBox);
begin
  inherited;
  FBrush := TDesignBrush.Create;
  FPen := TDesignPen.Create;
  FBrush.OnChange := DoBrushChange;
  FPen.OnChange := DoPenChange;
end;

destructor TDesignBoxItemShape.Destroy;
begin
  FBrush.Free;
  FPen.Free;
  inherited;
end;

procedure TDesignBoxItemShape.DoBrushChange(Sender: TObject);
begin
  Changed;
end;

procedure TDesignBoxItemShape.DoPenChange(Sender: TObject);
begin
  Changed;
end;

function TDesignBoxItemShape.GetBrush: TBrush;
begin
  Result := FBrush;
end;

function TDesignBoxItemShape.GetPen: TPen;
begin
  Result := FPen;
end;

procedure TDesignBoxItemShape.LoadFromJson(AJson: TJsonObject);
var
  AShape: string;
begin
  inherited;
  AShape := AJson.Values['shape'].Value;
  if AShape = 'ellipse' then FShape := dbEllipse;
  if AShape = 'rectangle' then FShape := dbRectangle;
  if AShape = 'line' then FShape := dbLine;

  FBrush.LoadFromJson(AJson);
  FPen.LoadFromJson(AJson);
end;

procedure TDesignBoxItemShape.PaintToCanvas(ACanvas: TCanvas);
begin
  ACanvas.Pen.Assign(FPen);
  ACanvas.Brush.Assign(FBrush);

  case FShape of
    dbEllipse: ACanvas.Ellipse(RectPixels);
    dbRectangle: ACanvas.Rectangle(RectPixels);
    dbLine:
      begin
        ACanvas.MoveTo(RectPixels.TopLeft.X, RectPixels.TopLeft.Y);
        ACanvas.LineTo(RectPixels.BottomRight.X, RectPixels.BottomRight.Y);
      end;
  end;
end;

procedure TDesignBoxItemShape.SaveToJson(AJson: TJsonObject);
begin
  inherited;
  case FShape of
    dbEllipse   : AJson.AddPair('shape', 'ellipse');
    dbRectangle : AJson.AddPair('shape', 'rectangle');
    dbLine      : AJson.AddPair('shape', 'line');
  end;
  FBrush.SaveToJson(AJson);
  FPen.SaveToJson(AJson);
end;

procedure TDesignBoxItemShape.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

{ TDesignBoxItemSelectable }

procedure TDesignBoxItemShape.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
end;

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

initialization

  RegisterClass(TDesignBoxItemText);
  RegisterClass(TDesignBoxItemShape);
  RegisterClass(TDesignBoxItemGraphic);

end.
