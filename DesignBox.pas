unit DesignBox;

interface

uses
  Windows, Messages, System.SysUtils, System.Classes, Vcl.Controls, System.Types, System.Generics.Collections,
  Graphics, JsonDataObjects;

type
  TDesignBox = class;
  TDesignBoxBaseItem = class;

  TDesignBoxSelectItemEvent = procedure(Sender: TObject; AItem: TDesignBoxBaseItem) of object;

  TDesignFont = class(TFont)
  public
    procedure SaveToJson(AJson: TJsonObject);
    procedure LoadFromJson(AJson: TJsonObject);
  end;

  TDesignBoxBaseItem = class
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
    procedure DrawSelectedRect(ACanvas: TCanvas);
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    function PointInRgn(x, y: integer): Boolean;
    function RectsIntersect(ARect: TRect): Boolean;
    procedure SaveToJson(AJson: TJsonObject); virtual;
    procedure LoadFromJson(AJson: TJsonObject); virtual;
    procedure SendToBack;
    procedure BringToFront;
    property Selected: Boolean read FSelected write SetSelectedItem;
    property LeftMM: single read GetLeftMM write SetLeftMM;
    property TopMM: single read GetTopMM write SetTopMM;
  end;

  TDesignBoxItemText = class(TDesignBoxBaseItem)
  private
    FText: string;
    FFont: TDesignFont;
    FBackground: TColor;
    procedure DoFontChange(Sender: TObject);
    procedure SetText(const AText: string);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetBackground(const Value: TColor);
  protected
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Text: string read FText write SetText;
    property Font: TFont read GetFont write SetFont;
    property BackgroundColor: TColor read FBackground write SetBackground default clWhite;
  end;

  TDbShape = (dbEllipse, dbRectangle, dbLine);

  TDesignBoxItemShape = class(TDesignBoxBaseItem)
  private
    FShape: TdbShape;
    FBackground: TColor;
    FLineColor: TColor;
    procedure SetLineColor(const Value: TColor);
  protected
    procedure SetBackground(const Value: TColor);
    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    procedure SaveToJson(AJson: TJsonObject); override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property LineColor: TColor read FLineColor write SetLineColor default clBlack;
    property BackgroundColor: TColor read FBackground write SetBackground default clWhite;
  end;

  TDesignBoxItemGraphic = class(TDesignBoxBaseItem)
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
    function GetSelectedItem: TDesignBoxBaseItem;
    procedure SetSelectedItem(const Value: TDesignBoxBaseItem);
    function GetSelectedCount: integer;
    function AddShape(AShape: TDbShape; ALeftMM, ATopMM, ARightMM, ABottomMM: single; ABorderColor, AFillColor: TColor): TDesignBoxItemShape;
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    function AddText(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
    function AddGraphic(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
    function AddRectangle(ALeftMM, ATopMM, ARightMM, ABottomMM: single; ABorderColor, AFillColor: TColor): TDesignBoxItemShape;
    function AddEllipse(ALeftMM, ATopMM, ARightMM, ABottomMM: single; ABorderColor, AFillColor: TColor): TDesignBoxItemShape;
    function AddLine(ALeftMM, ATopMM, ARightMM, ABottomMM: single; ABorderColor: TColor): TDesignBoxItemShape;
    function ItemAtPos(x, y: integer): TDesignBoxBaseItem;
    procedure BringToFront(AObj: TDesignBoxBaseItem);
    procedure SendToBack(AObj: TDesignBoxBaseItem);
    procedure LoadFromJson(AJson: TJsonObject);
    procedure SaveToJson(AJson: TJsonObject);
    procedure DeleteSelected;
    property SelectedItem: TDesignBoxBaseItem read GetSelectedItem write SetSelectedItem;
    property SelectedCount: integer read GetSelectedCount;
  end;

  TDesignBox = class(TGraphicControl)
  private
    FItems: TDesignBoxItemList;
    FSelectedItem: TDesignBoxBaseItem;
    FDragging: Boolean;
    FMouseDownPos: TPoint;
    FMouseXY: TPoint;
    FBuffer: TBitmap;
    FDragRect: TRect;
    FOnSelectItem: TDesignBoxSelectItemEvent;
    procedure SetSelectedItem(const Value: TDesignBoxBaseItem);
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
    property Items: TDesignBoxItemList read FItems;
    property SelectedItem: TDesignBoxBaseItem read FSelectedItem write SetSelectedItem;
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

uses System.NetEncoding, PngImage, Jpeg, Math;

procedure Register;
begin
  RegisterComponents('Samples', [TDesignBox]);
end;

{ TDesignBox }

procedure TDesignBox.BringToFront;
var
  AItem: TDesignBoxBaseItem;
begin
  for AItem in FItems do
  begin
    if AItem.Selected then
      AItem.BringToFront;
  end;
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
  FItems := TDesignBoxItemList.Create(Self);
  FBuffer.SetSize(Width, Height);
end;

destructor TDesignBox.Destroy;
begin
  FItems.Free;
  FBuffer.Free;
  inherited;
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
begin
  inherited;
  FMouseDownPos := Point(X, Y);
  AItem := FItems.ItemAtPos(x, y);
  if AItem = nil then
    SelectedItem := nil
  else
  begin
    if (AItem.Selected = False) then
      SelectedItem := AItem;
  end;

  FDragging := True;
  FMouseXY := Point(X, Y);
  if (SelectedItem <> nil) and (Assigned(FOnSelectItem)) then
    FOnSelectItem(Self, SelectedItem);
end;

procedure TDesignBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AItem: TDesignBoxBaseItem;
begin
  inherited;
  if (FDragging) and (FItems.SelectedItem <> nil) then
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
  if (FDragging) and (FItems.SelectedItem = nil) then
  begin
    ADragArea := Rect(FMouseDownPos.X, FMouseDownPos.Y, X, Y);
    for AItem in FItems do
    begin
      AItem.Selected := AItem.RectsIntersect(ADragArea);
    end;
  end;

  FDragging := False;
  Invalidate;
end;

procedure TDesignBox.Paint;
var
  ARect: TRect;
begin

  if (FBuffer.Width = 0) then
    Redraw;
  Canvas.Draw(0, 0, FBuffer);
  if (FDragging) and (FItems.SelectedItem = nil) then
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
  FBuffer.SetSize(Width, Height);
  FBuffer.Canvas.Brush.Color := clWhite;
  FBuffer.Canvas.Pen.Color := clBlack;
  FBuffer.Canvas.Rectangle(ClientRect);
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
  for AItem in FItems do
  begin
    if AItem.Selected then
      AItem.SendToBack;
  end;
end;

procedure TDesignBox.SetSelectedItem(const Value: TDesignBoxBaseItem);
var
  AItem: TDesignBoxBaseItem;
begin
  FSelectedItem := Value;
  FItems.SelectedItem := FSelectedItem;
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
  FBackground := clWhite;
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
  ARect: TRect;
begin
  ACanvas.Font.Assign(FFont);
  // calculate width/height...
  FWidthMM := (ACanvas.TextWidth(FText) / 96) * 25.4;
  FHeightMM := (ACanvas.TextHeight(FText) / 96) * 25.4;
  ACanvas.TextOut(RectPixels.Left, RectPixels.Top, FText);
  ACanvas.Pen.Color := clBlack;
  if FBackground = clNone then
    ACanvas.Brush.Style := bsClear
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := FBackground;
  end;
  ACanvas.TextOut(RectPixels.Left, RectPixels.Top, FText);
end;

procedure TDesignBoxItemText.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  Text := AJson.S['text'];
  FBackground := AJson.I['bg_color'];
  FFont.LoadFromJson(AJson.O['font']);
end;



procedure TDesignBoxItemText.SaveToJson(AJson: TJsonObject);
begin
  inherited;
  AJson.S['text'] := Text;
  AJson.I['bg_color'] := FBackground;
  FFont.SaveToJson(AJson.O['font']);
end;

procedure TDesignBoxItemText.SetBackground(const Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
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

procedure TDesignBoxBaseItem.BringToFront;
begin
  FDesignBox.Items.BringToFront(Self);
end;

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

procedure TDesignBoxBaseItem.DrawSelectedRect(ACanvas: TCanvas);
var
  ARect: TRect;
begin
  if FSelected then
  begin
    ARect := RectPixels;
    InflateRect(ARect, 2, 2);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Color := clBlue;
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

procedure TDesignBoxBaseItem.SendToBack;
begin
  FDesignBox.Items.SendToBack(Self);
end;

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

function TDesignBoxItemList.AddLine(ALeftMM, ATopMM, ARightMM, ABottomMM: single; ABorderColor: TColor): TDesignBoxItemShape;
begin
  Result := AddShape(dbLine, ALeftMM, ATopMM, ARightMM, ABottomMM, ABorderColor, clNone);
end;

function TDesignBoxItemList.AddRectangle(ALeftMM, ATopMM, ARightMM, ABottomMM: single; ABorderColor, AFillColor: TColor): TDesignBoxItemShape;
begin
  Result := AddShape(dbRectangle, ALeftMM, ATopMM, ARightMM, ABottomMM, ABorderColor, AFillColor);
end;

function TDesignBoxItemList.AddShape(AShape: TDbShape; ALeftMM, ATopMM, ARightMM, ABottomMM: single; ABorderColor, AFillColor: TColor): TDesignBoxItemShape;
begin
  Result := TDesignBoxItemShape.Create(FDesignBox);
  Result.FShape := AShape;
  Result.LeftMM := ALeftMM;
  Result.TopMM := ATopMM;
  Result.FWidthMM := ARightMM - ALeftMM;
  Result.FHeightMM := ABottomMM - ATopMM;
  Result.FBackground := AFillColor;
  Result.LineColor := ABorderColor;
  Add(Result);
  FDesignBox.Redraw;
end;

function TDesignBoxItemList.AddText(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
begin
  Result := TDesignBoxItemText.Create(FDesignBox);
  Result.LeftMM := ALeftMM;
  Result.TopMM := ATopMM;
  Result.Text := AText;
  Add(Result);
  FDesignBox.Redraw;
end;

procedure TDesignBoxItemList.BringToFront(AObj: TDesignBoxBaseItem);
begin
  Extract(AObj);
  Add(AObj);
  FDesignBox.Redraw;
end;

function TDesignBoxItemList.AddEllipse(ALeftMM, ATopMM, ARightMM, ABottomMM: single; ABorderColor, AFillColor: TColor): TDesignBoxItemShape;
begin
  Result := AddShape(dbEllipse, ALeftMM, ATopMM, ARightMM, ABottomMM, ABorderColor, AFillColor);
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

function TDesignBoxItemList.GetSelectedCount: integer;
var
  AItem: TDesignBoxBaseItem;
begin
  Result := 0;
  for AItem in Self do
    if AItem.Selected then
      Result := Result + 1;
end;

function TDesignBoxItemList.GetSelectedItem: TDesignBoxBaseItem;
var
  AItem: TDesignBoxBaseItem;
begin
  Result := nil;
  for AItem in Self do
  begin
    if AItem.Selected then
    begin
      Result := AItem;
      Exit;
    end;
  end;
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

procedure TDesignBoxItemList.SendToBack(AObj: TDesignBoxBaseItem);
begin
  Extract(AObj);
  Insert(0, AObj);
  FDesignBox.Redraw;
end;

procedure TDesignBoxItemList.SetSelectedItem(const Value: TDesignBoxBaseItem);
var
  AItem: TDesignBoxBaseItem;
begin
  for AItem in Self do
  begin
    AItem.Selected := AItem = Value;
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
  FBackground := clWhite;
  FLineColor := clBlack;
end;

destructor TDesignBoxItemShape.Destroy;
begin
  inherited;
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
  FBackground := AJson.I['bg_color'];
  FLineColor := AJson.I['line_color'];
end;

procedure TDesignBoxItemShape.PaintToCanvas(ACanvas: TCanvas);
var
  ARect: TRect;
begin
  ACanvas.Pen.Color := LineColor;
  ACanvas.Pen.Style := psSolid;
  if FBackground = clNone then
    ACanvas.Brush.Style := bsClear
  else
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := FBackground;
  end;
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
  AJson.I['bg_color'] := FBackground;
  AJson.I['line_color'] := FLineColor;
end;

procedure TDesignBoxItemShape.SetBackground(const Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    FDesignBox.Redraw;
  end;
end;

procedure TDesignBoxItemShape.SetLineColor(const Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    FDesignBox.Redraw;
  end;
end;

end.
