unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Generics.Collections,
  System.Types, DesignBox, Vcl.Imaging.pngimage, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.ComCtrls;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    btnAddText: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    DesignBox1: TDesignBox;
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
    CheckBox1: TCheckBox;
    Edit1: TEdit;
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
    procedure CheckBox1Click(Sender: TObject);
  private
    function AppDir: string;
    procedure UpdateItemCoords;
    procedure UpdateActionStates;
    { Private declarations }
  public

    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.actBringToFrontExecute(Sender: TObject);
begin
  DesignBox1.BringToFront;
end;

procedure TfrmMain.actDeleteExecute(Sender: TObject);
begin
  DesignBox1.Items.DeleteSelected;
end;

procedure TfrmMain.actSendToBackExecute(Sender: TObject);
begin
  DesignBox1.SendToBack;
end;

function TfrmMain.AppDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

procedure TfrmMain.btnBorderColorClick(Sender: TObject);
begin
  if dlgBorderColor.Execute then DesignBox1.Pen.Color := dlgBorderColor.Color;
end;

procedure TfrmMain.btnFillColorClick(Sender: TObject);
begin
  if (dlgFillColor.Execute) then DesignBox1.Brush.Color := dlgFillColor.Color;
end;

procedure TfrmMain.btnFontClick(Sender: TObject);
begin
 if FontDialog1.execute then DesignBox1.Font.Assign(FontDialog1.Font);
end;

procedure TfrmMain.btnAddTextClick(Sender: TObject);
var
  item: TDesignBoxItemText;
begin
  item := DesignBox1.Items.AddText(20, 20, 'Some Text');
  DesignBox1.items.DeselectAll;
  item.selected := True;
end;

procedure TfrmMain.btnAddGraphicClick(Sender: TObject);
var
  item: TDesignBoxItemGraphic;
begin
  item := DesignBox1.Items.AddGraphic(20, 20, Image1.Picture.Graphic);
  DesignBox1.items.DeselectAll;
  item.selected := True;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  DesignBox1.SaveToFile(AppDir+'data.json');
end;

procedure TfrmMain.CheckBox1Click(Sender: TObject);
begin
  case CheckBox1.Checked of
    True: DesignBox1.Brush.Style := bsClear;
    False: DesignBox1.Brush.Style := bsSolid;

  end;
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
  item: TDesignBoxItemShape;
begin
  item := DesignBox1.Items.AddRectangle(20, 20, 40, 40);
  DesignBox1.items.DeselectAll;
  item.selected := True;
end;

procedure TfrmMain.btnAddEllipseClick(Sender: TObject);
var
  item: TDesignBoxItemShape;
begin
  item := DesignBox1.Items.AddEllipse(20, 20, 40, 40);
  DesignBox1.items.DeselectAll;
  item.selected := True;
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

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  self.Caption := application.title;
  dlgBorderColor.Color := clWebNavy;
  dlgFillColor.Color := clWebLightSkyBlue;
end;

procedure TfrmMain.PopupMenu1Popup(Sender: TObject);
begin
  UpdateActionStates;
end;

procedure TfrmMain.UpdateActionStates;
begin
  actBringToFront.Enabled := DesignBox1.Items.SelectedCount > 0;
  actSendToBack.Enabled := DesignBox1.Items.SelectedCount > 0;
  actDelete.Enabled := DesignBox1.Items.SelectedCount > 0;
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