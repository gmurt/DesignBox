unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Generics.Collections,
  System.Types, DesignBox, Vcl.Imaging.pngimage, System.Actions, Vcl.ActnList, Vcl.Menus;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    DesignBox1: TDesignBox;
    Image1: TImage;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    actBringToFront: TAction;
    actSendToBack: TAction;
    BringToFront1: TMenuItem;
    SendToBack1: TMenuItem;
    N1: TMenuItem;
    actDelete: TAction;
    Delete1: TMenuItem;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DesignBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DesignBox1SelectItem(Sender: TObject; AItem: TDesignBoxBaseItem);
    procedure DesignBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure actBringToFrontExecute(Sender: TObject);
    procedure actSendToBackExecute(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    function AppDir: string;
    procedure UpdateItemCoords(AItem: TDesignBoxBaseItem);
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

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  DesignBox1.Items.AddText(20, 20, 'Some Text');
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  DesignBox1.Items.AddGraphic(20, 20, Image1.Picture.Graphic);
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  DesignBox1.SaveToFile(AppDir+'data.json');
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  DesignBox1.LoadFromFile(AppDir+'data.json');
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  DesignBox1.Clear;
end;

procedure TfrmMain.Button6Click(Sender: TObject);
begin
  DesignBox1.Items.AddRectangle(20, 20, 40, 40, clBlack, clYellow);
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
  DesignBox1.Items.AddEllipse(20, 20, 40, 40, clBlack, clSkyBlue);
end;

procedure TfrmMain.DesignBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateItemCoords(DesignBox1.SelectedItem);
end;

procedure TfrmMain.DesignBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  UpdateItemCoords(DesignBox1.SelectedItem);
end;

procedure TfrmMain.DesignBox1SelectItem(Sender: TObject; AItem: TDesignBoxBaseItem);
begin
  UpdateItemCoords(DesignBox1.SelectedItem);
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

procedure TfrmMain.UpdateItemCoords(AItem: TDesignBoxBaseItem);
begin
  Label1.Caption := '-';
  Label2.Caption := '-';
  if AItem <> nil then
  begin
    Label1.Caption := 'X: '+FormatFloat('0.00', AItem.LeftMM)+' mm';
    Label2.Caption := 'Y: '+FormatFloat('0.00', AItem.TopMM)+' mm';
  end;
end;

end.
