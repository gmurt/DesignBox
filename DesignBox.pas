unit DesignBox;

interface

uses
  Windows, Messages, System.SysUtils, System.Classes, Vcl.Controls, System.Types, System.Generics.Collections,
  Graphics;

type
  TDesignBox = class;
  TDesignBoxBaseItem = class;

  TDesignBoxSelectItemEvent = procedure(Sender: TObject; AItem: TDesignBoxBaseItem) of object;

  TDesignBoxBaseItem = class
  private
    FDesignBox: TDesignBox;
    FPositionMM: TRectF;
    FWidthMM: single;
    FHeightMM: single;
    FSelected: Boolean;
    FCanvasRgn: TRect;
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
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    function PointInRgn(x, y: integer): Boolean;
    property Selected: Boolean read FSelected write SetSelectedItem;
    property LeftMM: single read GetLeftMM write SetLeftMM;
    property TopMM: single read GetTopMM write SetTopMM;
  end;

  TDesignBoxItemText = class(TDesignBoxBaseItem)
  private
    FText: string;
    FFont: TFont;
    procedure DoFontChange(Sender: TObject);
    procedure SetText(const AText: string);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
  protected

    procedure PaintToCanvas(ACanvas: TCanvas); override;
  public
    constructor Create(ADesignBox: TDesignBox); override;
    destructor Destroy; override;
    property Text: string read FText write SetText;
    property Font: TFont read GetFont write SetFont;
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
    property Graphic: TPicture read FGraphic write SetGraphic;
  end;

  TDesignBoxItemList = class(TObjectList<TDesignBoxBaseItem>)
  private
    FDesignBox: TDesignBox;
    function GetSelectedItem: TDesignBoxBaseItem;
    procedure SetSelectedItem(const Value: TDesignBoxBaseItem);
  public
    constructor Create(ADesignBox: TDesignBox); virtual;
    function AddText(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
    function AddGraphic(ALeftMM, ATopMM: single; AGraphic: TGraphic): TDesignBoxItemGraphic;
    function ItemAtPos(x, y: integer): TDesignBoxBaseItem;
    property SelectedItem: TDesignBoxBaseItem read GetSelectedItem write SetSelectedItem;
  end;

  TDesignBox = class(TGraphicControl)
  private
    FItems: TDesignBoxItemList;
    FSelectedItem: TDesignBoxBaseItem;
    FDragging: Boolean;
    FMouseDownPos: TPoint;
    FMouseXY: TPoint;
    FBuffer: TBitmap;
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
    property Items: TDesignBoxItemList read FItems;
    property SelectedItem: TDesignBoxBaseItem read FSelectedItem write SetSelectedItem;
  published
    property Align;
    property OnSelectItem: TDesignBoxSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDesignBox]);
end;

{ TDesignBox }

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

procedure TDesignBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AItem: TDesignBoxBaseItem;
begin
  inherited;
  FMouseDownPos := Point(X, Y);
  SelectedItem := FItems.ItemAtPos(x, y);
  FDragging := True;
  FMouseXY := Point(X, Y);
  if (SelectedItem <> nil) and (Assigned(FOnSelectItem)) then
    FOnSelectItem(Self, SelectedItem);
end;

procedure TDesignBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FDragging) and (FItems.SelectedItem <> nil) then
  begin
    FItems.SelectedItem.OffsetByPixels(X- FMouseXY.X, Y - FMouseXY.Y);
  end;
  FMouseXY := Point(X, Y);
end;

procedure TDesignBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FDragging := False;
end;

procedure TDesignBox.Paint;
{ }
begin
  if (FBuffer.Width = 0) then
    Redraw;
  Canvas.Draw(0, 0, FBuffer);
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
      AItem.PaintToCanvas(FBuffer.Canvas);
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
  FFont := TFont.Create;
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
begin
  ACanvas.Font.Assign(FFont);
  // calculate width/height...
  FWidthMM := (ACanvas.TextWidth(FText) / 96) * 25.4;
  FHeightMM := (ACanvas.TextHeight(FText) / 96) * 25.4;
  ACanvas.TextOut(RectPixels.Left, RectPixels.Top, FText);
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(RectPixels.Left, RectPixels.Top, FText);
  if FSelected then
  begin
    ACanvas.Pen.Color := clBlue;
    ACanvas.Rectangle(RectPixels);
  end;
end;

procedure TDesignBoxItemText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TDesignBoxItemText.SetText(const AText: string);
begin
  FText := AText;
  Changed;
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

function TDesignBoxBaseItem.GetLeftMM: single;
begin
  Result := FPositionMM.Left;
end;

function TDesignBoxBaseItem.GetTopMM: single;
begin
  Result := FPositionMM.Top;
end;

procedure TDesignBoxBaseItem.OffsetByPixels(X, Y: integer);
begin
  FPositionMM.Left := FPositionMM.Left + ((x / 96) * 25.4);
  FPositionMM.Top := FPositionMM.Top + ((y / 96) * 25.4);
  Changed;
end;

function TDesignBoxBaseItem.PointInRgn(x, y: integer): Boolean;
begin
  Result := PtInRect(RectPixels, Point(x,y));
end;

function TDesignBoxBaseItem.RectPixels: TRect;
begin
  Result.Left := Round((96 / 25.4) *FPositionMM.Left);
  Result.Top := Round((96 / 25.4) *FPositionMM.Top);
  Result.Right := Round(((FPositionMM.Left+FWidthMM) / 25.4) * 96);
  Result.Bottom := Round(((FPositionMM.Top + FHeightMM) / 25.4) * 96);
end;

procedure TDesignBoxBaseItem.SetLeftMM(const Value: single);
begin
  FPositionMM.Left := Value;
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
  FPositionMM.Top := Value;
end;

{ TDesignBoxItemList }

function TDesignBoxItemList.AddText(ALeftMM, ATopMM: single; AText: string): TDesignBoxItemText;
begin
  Result := TDesignBoxItemText.Create(FDesignBox);
  Result.LeftMM := ALeftMM;
  Result.TopMM := ATopMM;
  Result.Text := AText;
  Add(Result);
  FDesignBox.Redraw;
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

procedure TDesignBoxItemGraphic.PaintToCanvas(ACanvas: TCanvas);
begin
  // calculate width/height...
  FWidthMM := (FGraphic.Width / 96) * 25.4;
  FHeightMM := (FGraphic.Height / 96) * 25.4;
  ACanvas.Draw(RectPixels.Left, RectPixels.Top, FGraphic.Graphic);
  if FSelected then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Color := clBlue;
    ACanvas.Rectangle(RectPixels);
  end;
end;

procedure TDesignBoxItemGraphic.SetGraphic(const Value: TPicture);
begin
  FGraphic.Assign(Value);
end;

end.
