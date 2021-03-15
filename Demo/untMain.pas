unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Generics.Collections,
  System.Types, DesignBox, Vcl.Imaging.pngimage, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ComCtrls, Vcl.Samples.Spin;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    btnAddText: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Image1: TImage;
    btnAddGraphic: TButton;
    btnSave: TButton;
    btnLoad: TButton;
    btnClear: TButton;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    actBringToFront: TAction;
    actSendToBack: TAction;
    BringToFront1: TMenuItem;
    SendToBack1: TMenuItem;
    N1: TMenuItem;
    actDelete: TAction;
    Delete1: TMenuItem;
    btnAddRectangle: TButton;
    btnAddEllipse: TButton;
    StatusBar1: TStatusBar;
    btnFont: TButton;
    btnBorderColor: TButton;
    btnFillColor: TButton;
    FontDialog1: TFontDialog;
    dlgBorderColor: TColorDialog;
    dlgFillColor: TColorDialog;
    chkNoFill: TCheckBox;
    txtTextItem: TEdit;
    chkNoBorder: TCheckBox;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    actUndo: TAction;
    actRedo: TAction;
    CheckBox3: TCheckBox;
    spinWidth: TSpinEdit;
    spinHeight: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    actBringForwards: TAction;
    actSendBackwards: TAction;
    SendBackwards1: TMenuItem;
    BringForwards1: TMenuItem;
    chkGridVisible: TCheckBox;
    SpinEdit1: TSpinEdit;
    DesignBox1: TDesignBox;
    chkSnapTogrid: TCheckBox;
    Label3: TLabel;
    Button4: TButton;
    alignPopup: TPopupMenu;
    actAlignleft: TAction;
    actAlignRight: TAction;
    actAlignTop: TAction;
    actAlignBottom: TAction;
    AlignLeft1: TMenuItem;
    AlignRight1: TMenuItem;
    AlignTop1: TMenuItem;
    AlignBottom1: TMenuItem;
    actAlignToGrid: TAction;
    Aligntogrid1: TMenuItem;
    N2: TMenuItem;
    Button5: TButton;
    procedure btnAddTextClick(Sender: TObject);
    procedure btnAddGraphicClick(Sender: TObject);
    procedure DesignBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DesignBox1SelectItem(Sender: TObject; AItem: TDesignBoxBaseItem);
    procedure DesignBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure actBringToFrontExecute(Sender: TObject);
    procedure actSendToBackExecute(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure btnAddRectangleClick(Sender: TObject);
    procedure btnAddEllipseClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnBorderColorClick(Sender: TObject);
    procedure btnFillColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkNoFillClick(Sender: TObject);
    procedure chkNoBorderClick(Sender: TObject);
    procedure txtTextItemChange(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure DesignBox1Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure spinWidthChange(Sender: TObject);
    procedure spinHeightChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure actBringForwardsExecute(Sender: TObject);
    procedure actSendBackwardsExecute(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure chkGridVisibleClick(Sender: TObject);
    procedure chkSnapTogridClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure actAlignleftExecute(Sender: TObject);
    procedure Button4DropDownClick(Sender: TObject);
    procedure actAlignToGridExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
  private
    function AppDir: string;
    procedure UpdateItemCoords;
    procedure UpdateActionStates;
    { Private declarations }
  protected
    procedure DoShow; override;
  public

    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.actAlignleftExecute(Sender: TObject);
begin
  DesignBox1.AlignItems(TItemAlignment(TAction(Sender).tag));
end;

procedure TfrmMain.actAlignToGridExecute(Sender: TObject);
begin
  DesignBox1.AlignItems(TItemAlignment(TAction(Sender).tag));
end;

procedure TfrmMain.actBringForwardsExecute(Sender: TObject);
begin
  DesignBox1.BringForwards;
end;

procedure TfrmMain.actBringToFrontExecute(Sender: TObject);
begin
  DesignBox1.BringToFront;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
begin
  DesignBox1.Items.DeleteSelected;
end;

procedure TfrmMain.actRedoExecute(Sender: TObject);
begin
  DesignBox1.Redo;
end;

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
begin
  DesignBox1.items.SelectAll
end;

procedure TfrmMain.actSendBackwardsExecute(Sender: TObject);
begin
  DesignBox1.SendBackwards;
end;

procedure TfrmMain.actSendToBackExecute(Sender: TObject);
begin
  DesignBox1.SendToBack;
end;

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  DesignBox1.Undo;
end;

function TfrmMain.AppDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

procedure TfrmMain.btnBorderColorClick(Sender: TObject);
begin
  if dlgBorderColor.Execute then DesignBox1.Canvas.Pen.Color := dlgBorderColor.Color;
end;

procedure TfrmMain.btnFillColorClick(Sender: TObject);
begin
  if (dlgFillColor.Execute) then DesignBox1.Canvas.Brush.Color := dlgFillColor.Color;
end;

procedure TfrmMain.btnFontClick(Sender: TObject);
begin
 if FontDialog1.execute then DesignBox1.Canvas.Font.Assign(FontDialog1.Font);
end;

procedure TfrmMain.btnAddTextClick(Sender: TObject);
var
  item: TDesignBoxItemText;
begin
  item := DesignBox1.Canvas.TextOut(20, 20, txtTextItem.text);
  DesignBox1.items.DeselectAll;
  item.selected := True;
end;

procedure TfrmMain.btnAddGraphicClick(Sender: TObject);
var
  item: TDesignBoxItemGraphic;
begin
  item := DesignBox1.Canvas.Draw(20, 20, Image1.Picture.Graphic);
  DesignBox1.items.DeselectAll;
  item.selected := True;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  DesignBox1.SaveToFile(AppDir+'data.json');
end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  item: TDesignBoxItemRectangle;
begin
  item := DesignBox1.Canvas.Rectangle(RectF(20, 20, 40, 40), 5);
  DesignBox1.items.DeselectAll;
  item.selected := True;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  DesignBox1.AlignItems(TItemAlignment.ialLeftSides);
end;

procedure TfrmMain.Button4DropDownClick(Sender: TObject);
begin
  Button4.DropDownMenu := alignPopup;
end;

procedure TfrmMain.chkNoFillClick(Sender: TObject);
begin
  case TCheckBox(Sender).Checked of
    True: DesignBox1.Canvas.Brush.Style := bsClear;
    False: DesignBox1.Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TfrmMain.chkNoBorderClick(Sender: TObject);
const
  C_STYLES: array[boolean] of TPenStyle = (psSolid, psClear);
begin
  DesignBox1.Canvas.Pen.Style := C_STYLES[TCheckBox(Sender).checked];
end;

procedure TfrmMain.CheckBox3Click(Sender: TObject);
begin
  DesignBox1.RulerOptions.Visible := CheckBox3.Checked;
end;

procedure TfrmMain.chkGridVisibleClick(Sender: TObject);
begin
  DesignBox1.GridOptions.Visible := TCheckBox(Sender).Checked;
end;

procedure TfrmMain.chkSnapTogridClick(Sender: TObject);
begin
  DesignBox1.GridOptions.SnapToGrid := TCheckBox(Sender).Checked;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  DesignBox1.LoadFromFile(AppDir+'data.json');
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  DesignBox1.Clear;
end;

procedure TfrmMain.btnAddRectangleClick(Sender: TObject);
var
  item: TDesignBoxItemRectangle;
begin
  item := DesignBox1.Canvas.Rectangle(RectF(20, 20, 40, 40));
  DesignBox1.items.DeselectAll;
  item.selected := True;
end;

procedure TfrmMain.btnAddEllipseClick(Sender: TObject);
var
  item: TDesignBoxItemEllipse;
begin
  item := DesignBox1.Canvas.Ellipse(RectF(20, 20, 40, 40));
  DesignBox1.items.DeselectAll;
  item.selected := True;
end;

procedure TfrmMain.DesignBox1Change(Sender: TObject);
begin
  UpdateActionStates;
end;

procedure TfrmMain.DesignBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateItemCoords;
end;

procedure TfrmMain.DesignBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  UpdateItemCoords;
end;

procedure TfrmMain.DesignBox1SelectItem(Sender: TObject; AItem: TDesignBoxBaseItem);
begin
  if not assigned(AItem) then EXIT;
  if AItem is TDesignBoxItemText then
    txtTextItem.Text := TDesignBoxItemText(AItem).Text
  else
    txtTextItem.text := '';
  if Aitem is TDesignBoxItemOutlineShape then
    chkNoBorder.checked := TDesignBoxItemOutlineShape(AItem).Pen.style = psClear;
  if AItem is TDesignBoxItemFilledShape then
    chkNoFill.checked := TDesignBoxItemFilledShape(aItem).Brush.Style = bsClear;
  if AItem is TDesignBoxItemText then
  begin
    chkNoBorder.checked := TDesignBoxItemText(AItem).Pen.style = psClear;
    chkNoFill.checked := TDesignBoxItemText(AItem).Brush.Style = bsClear;
  end;

  {UpdateItemCoords(DesignBox1.SelectedItem);
  // no selected item = set default fonts/color for next item
  btnFont.Enabled := (not assigned(DesignBox1.SelectedItem)) or (DesignBox1.SelectedItem is TDesignBoxItemText);
  btnBorderColor.Enabled := (not assigned(DesignBox1.SelectedItem)) or (DesignBox1.SelectedItem is TDesignBoxItemShape) or (DesignBox1.SelectedItem is TDesignBoxItemText); // text can have font color
  btnFillColor.Enabled := (not assigned(DesignBox1.SelectedItem)) or (DesignBox1.SelectedItem is TDesignBoxItemShape) or (DesignBox1.SelectedItem is TDesignBoxItemText); // text can have background color
  if DesignBox1.SelectedItem is TDesignBoxItemText then
  begin
    FontDialog1.Font.Assign(TDesignBoxItemText(DesignBox1.SelectedItem).Font);
    btnBorderColor.Caption := 'Text Colour';
  end;
  if DesignBox1.SelectedItem is TDesignBoxItemShape then
  begin
    dlgBorderColor.Color := TDesignBoxItemShape(DesignBox1.SelectedItem).LineColor;
    btnBorderColor.Caption := 'Border';
    dlgFillColor.Color := TDesignBoxItemShape(DesignBox1.SelectedItem).BackgroundColor;
  end;  }
end;

procedure TfrmMain.DoShow;
var
  textItem: TDesignBoxItemText;
  textSize : TSizeF;
const
  C_DESIGNBOX = 'DesignBox';
begin
  inherited;
  UpdateActionStates;
  //
  textSize := DesignBox1.Canvas.MeasureText(C_DESIGNBOX);
  textItem := DesignBox1.Canvas.TextOut(DesignBox1.PageWidthMM - textSize.Width - 1, DesignBox1.PageHeightMM - textSize.Height - 1, C_DESIGNBOX);
  textItem.Font.Color := clGrayText;
  textItem.Pen.Style := psClear;
  textItem.Brush.Style := bsClear;
  textItem.options := []; // can't select, move, size or delete
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  self.Caption := application.title;
  dlgBorderColor.Color := clWebNavy;
  dlgFillColor.Color := clWebLightSkyBlue;
  txtTextItem.text := 'Some Text';
  chkGridVisible.Checked := DesignBox1.GridOptions.Visible;
  chkSnapTogrid.Checked := DesignBox1.GridOptions.SnapToGrid;
  spinWidth.value := Designbox1.PageWidthMM;
  spinHeight.Value := DesignBox1.PageHeightMM;
  spinHeight.MinValue := 10;
  spinHeight.MaxValue := 1000;
  spinWidth.MinValue := 10;
  spinWidth.MaxValue := 1000;
end;

procedure TfrmMain.PopupMenu1Popup(Sender: TObject);
begin
  UpdateActionStates;
end;

procedure TfrmMain.SpinEdit1Change(Sender: TObject);
begin
  DesignBox1.GridOptions.SizeMm := SpinEdit1.Value;
end;

procedure TfrmMain.spinHeightChange(Sender: TObject);
begin
  DesignBox1.PageHeightMM := spinHeight.Value;
end;

procedure TfrmMain.spinWidthChange(Sender: TObject);
begin
  DesignBox1.PageWidthMM := spinWidth.Value;
end;

procedure TfrmMain.txtTextItemChange(Sender: TObject);
begin
  if (DesignBox1.SelectedItems.Count = 1) then
    if DesignBox1.SelectedItems[0] is TDesignBoxItemText then
      TDesignBoxItemText(DesignBox1.SelectedItems[0]).Text := TEdit(Sender).Text;
end;

procedure TfrmMain.UpdateActionStates;
begin
  actBringToFront.Enabled := DesignBox1.Items.SelectedCount > 0;
  actSendToBack.Enabled := DesignBox1.Items.SelectedCount > 0;
  actDelete.Enabled := DesignBox1.Items.SelectedCount > 0;
  actUndo.Enabled := DesignBox1.CanUndo;
  actRedo.Enabled := DesignBox1.CanRedo;
end;

procedure TfrmMain.UpdateItemCoords;
begin
  if DesignBox1.SelectedItems.Count > 1 then
  begin
    StatusBar1.SimpleText := 'Multiple items selected';
    Exit;
  end;

  if DesignBox1.SelectedItems.Count = 1 then
      StatusBar1.SimpleText := Format('X = %fmm | Y = %fmm' ,[DesignBox1.SelectedItems[0].LeftMM, DesignBox1.SelectedItems[0].TopMM])
  else
    StatusBar1.SimpleText := 'No items selected';
end;

end.
