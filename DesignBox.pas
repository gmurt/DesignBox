unit DesignBox;



{ TODO 1 : Bug - text background colors not drawing reliably - seem to depend on the color of adjacent items in the list }
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
  Graphics, JsonDataObjects, System.Generics.Defaults;

type
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

  TDesignBoxBaseItem = class(TSingletonImplementation)
  private
    FDesignBox: TDesignBox;
    FPositionMM: TPointF;
    FWidthMM: single;
    FHeightMM: single;
    FSelected: Boolean;
    function RectPixels: TRect; virtual;
    function GetLeftMM: single;
    function GetTopMM: single;
    procedure Changed;
    procedure SetLeftMM(const Value: single);
    procedure SetTopMM(const Value: single);
    procedure SetSelectedItem(const Value: Boolean);
  protected
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
  end;

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
    function ItemAtPos(x, y: integer): TDesignBoxBaseItem;

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
    procedure OnBrushChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure OnPenChanged(Sender: TObject);

    function GetSelectedItems: TDesignBoxItemList;
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure WMEraseBackground(var message: TMessage); message WM_ERASEBKGND;
    procedure Redraw;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(AFilename: string);
    procedure LoadFromFile(AFilename: string);
    procedure BringToFront;
    procedure SendToBack;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
    property Items: TDesignBoxItemList read FItems;
    property SelectedItems: TDesignBoxItemList read GetSelectedItems;// write SetSelectedItem;
  published
    property Align;
    property OnSelectItem: TDesignBoxSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property PopupMenu;
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

procedure TDesignBox.Clear;
begin
  FItems.Clear;
  Redraw;
end;

constructor TDesignBox.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer := TBitmap.Create;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FPen := TPen.Create;
  FItems := TDesignBoxItemList.Create(Self);
  FSelectedItems := TDesignBoxItemList.Create(Self);
  FSelectedItems.OwnsObjects := False;
  FBuffer.SetSize(Width, Height);
  FBrush.OnChange := OnBrushChanged;
  FFont.OnChange := OnFontChanged;;
  FPen.OnChange := OnPenChanged;
end;

destructor TDesignBox.Destroy;
begin
  FItems.Free;
  FBuffer.Free;
  FSelectedItems.Free;
  FFont.Free;
  FBrush.Free;
  FPen.Free;
  inherited;
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
  AJson: TJsonObject;
begin
  AJson := TJsonObject.Create;
  try
    AJson.LoadFromStream(AStream);
    FItems.LoadFromJson(AJson);
    Redraw;
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
  if (FDragging) and (FItems.SelectedCount > 0) then
  begin
    for AItem in FItems do
    begin
      if AItem.Selected then
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

procedure TDesignBox.Paint;
var
  ARect: TRect;
begin

  if (FBuffer.Width = 0) then
    Redraw;
  Canvas.Draw(0, 0, FBuffer);

  if (FDragging) and (FItems.SelectedCount = 0) then
  begin
    ARect := Rect(FMouseDownPos.X, FMouseDownPos.Y, FMouseXY.X, FMouseXY.Y);
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Mode := pmNot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ARect);
  end;
end;


procedure TDesignBox.Redraw;
var
  AItem: TDesignBoxBaseItem;
begin
  //FBuffer.Free;
  //FBuffer := TBitmap.Create;
  FBuffer.SetSize(Width, Height);
  FBuffer.Canvas.Brush.Color := clWhite;
  FBuffer.Canvas.Brush.Style := bsSolid;
  FBuffer.Canvas.Pen.Color := clBlack;
  FBuffer.Canvas.Rectangle(ClientRect);
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
  Invalidate;
end;

procedure TDesignBox.Resize;
begin
  inherited;
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  Redraw;
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
begin
  AJson := TJsonObject.Create;
  try
    FItems.SaveToJson(AJson);
    AJson.SaveToStream(AStream);
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


procedure TDesignBox.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TDesignBox.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TDesignBox.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
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
  Text := AJson.S['text'];
  FBrush.LoadFromJson(AJson.O['brush']);
  FFont.LoadFromJson(AJson.O['font']);
  FPen.LoadFromJson(AJson.O['pen']);
end;



procedure TDesignBoxItemText.SaveToJson(AJson: TJsonObject);
begin
  inherited;
  AJson.S['text'] := Text;
  FBrush.SaveToJson(AJson.O['brush']);
  FFont.SaveToJson(AJson.O['font']);
  FPen.SaveToJson(AJson.O['pen']);
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
end;

constructor TDesignBoxBaseItem.Create(ADesignBox: TDesignBox);
begin
  inherited Create;
  FDesignBox := ADesignBox;
  FSelected := False;
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
  FPositionMM.X := AJson.F['x'];
  FPositionMM.Y := AJson.F['y'];
  FWidthMM := AJson.F['width'];
  FHeightMM := AJson.F['height'];
end;

procedure TDesignBoxBaseItem.SaveToJson(AJson: TJsonObject);
begin
  AJson.S['obj'] := ClassName;
  AJson.F['x'] := FPositionMM.X;
  AJson.F['y'] := FPositionMM.Y;
  AJson.F['width'] := FWidthMM;
  AJson.F['height'] := FHeightMM;
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
end;

function TDesignBoxItemList.AddText(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
begin
  Result := TDesignBoxItemText.Create(FDesignBox);
  Result.LeftMM := ALeftMM;
  Result.TopMM := ATopMM;
  Result.Text := AText;
  Result.Brush.Assign(FDesignBox.Brush);
  Add(Result);
  FDesignBox.Redraw;
end;
    {
procedure TDesignBoxItemList.BringToFront(AObj: TDesignBoxBaseItem);
begin
  Extract(AObj);
  Add(AObj);
  FDesignBox.Redraw;
end;  }

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
    if Items[ICount].Selected then
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
  AItem: TDesignBoxBaseItem;
begin
  Clear;
  AItems := AJson.A['items'];
  for AObj in AItems do
  begin
    AItem := nil;
    AObjName := AObj.S['obj'].ToLower;
    if AObjName = TDesignBoxItemText.ClassName.ToLower then AItem := TDesignBoxItemText.Create(FDesignBox);
    if AObjName = TDesignBoxItemGraphic.ClassName.ToLower then AItem := TDesignBoxItemGraphic.Create(FDesignBox);
    if AObjName = TDesignBoxItemShape.ClassName.ToLower then AItem := TDesignBoxItemShape.Create(FDesignBox);

    if AItem <> nil then
    begin
      AItem.LoadFromJson(AObj);
      Add(AItem);
    end;
  end;
end;

procedure TDesignBoxItemList.SaveToJson(AJson: TJsonObject);
var
  AItems: TJsonArray;
  AObj: TJsonObject;
  AItem: TDesignBoxBaseItem;
begin
  AItems := AJson.A['items'];
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
  AImg: TGraphic;
begin
  inherited;
  AStream := TMemoryStream.Create;
  AEncoded := TStringStream.Create(AJson.O['img'].S['data']);
  try
    AEncoded.Position := 0;
    TNetEncoding.Base64.Decode(AEncoded, AStream);
    AStream.Position := 0;
    AType := AJson.O['img'].S['type'].ToLower;
    AImg := nil;
    if AType = TPngImage.ClassName.ToLower then AImg := TPngImage.Create;
    if AType = TBitmap.ClassName.ToLower then AImg := TBitmap.Create;
    if AType = TJPEGImage.ClassName.ToLower then AImg := TJPEGImage.Create;
    if AImg <> nil then
    begin
      AImg.LoadFromStream(AStream);
      FGraphic.Assign(AImg);
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
begin
  inherited;
  AStream := TMemoryStream.Create;
  AEncoded := TStringStream.Create;
  try
    FGraphic.SaveToStream(AStream);
    AStream.Position := 0;
    TNetEncoding.Base64.Encode(AStream, AEncoded);
    AJson.O['img'].S['type'] := FGraphic.Graphic.ClassName;
    AJson.O['img'].S['data'] := AEncoded.DataString;
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
begin
  Name := AJson.S['name'];
  Size := AJson.I['size'];
  Color := AJson.I['color'];
end;

procedure TDesignFont.SaveToJson(AJson: TJsonObject);
begin
  AJson.S['name'] := Name;
  AJson.I['size'] := Size;
  AJson.I['color'] := Color;
end;

{ TDesignBoxItemShape }

constructor TDesignBoxItemShape.Create(ADesignBox: TDesignBox);
begin
  inherited;
  FBrush := TDesignBrush.Create;
  FPen := TDesignPen.Create;

end;

destructor TDesignBoxItemShape.Destroy;
begin
  FBrush.Free;
  FPen.Free;
  inherited;
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
  AShape := AJson.S['shape'];
  if AShape = 'ellipse' then FShape := dbEllipse;
  if AShape = 'rectangle' then FShape := dbRectangle;
  if AShape = 'line' then FShape := dbLine;

  FBrush.LoadFromJson(AJson.O['brush']);
  FPen.LoadFromJson(AJson.O['pen']);
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
    dbEllipse   : AJson.S['shape'] := 'ellipse';
    dbRectangle : AJson.S['shape'] := 'rectangle';
    dbLine      : AJson.S['shape'] := 'line';
  end;
  FBrush.SaveToJson(AJson.O['brush']);
  FPen.SaveToJson(AJson.O['pen']);
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
begin
  Color := AJson.I['color'];
  Style := TBrushStyle(AJson.I['style']);
end;

procedure TDesignBrush.SaveToJson(AJson: TJsonObject);
begin
  AJson.I['color'] := Color;
  AJson.I['style'] := Ord(Style);
end;

{ TDesignPen }

procedure TDesignPen.LoadFromJson(AJson: TJsonObject);
begin
  Color := AJson.I['color'];
  Style := TPenStyle(AJson.I['style']);
  Width := AJson.I['width'];
end;

procedure TDesignPen.SaveToJson(AJson: TJsonObject);
begin
  AJson.I['color'] := Color;
  AJson.I['style'] := Ord(Style);
  AJson.I['width'] := Width;
end;

end.
