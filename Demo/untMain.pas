unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Generics.Collections,
  System.Types, DesignBox, Vcl.Imaging.pngimage;

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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DesignBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DesignBox1SelectItem(Sender: TObject; AItem: TDesignBoxBaseItem);
    procedure DesignBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure UpdateItemCoords(AItem: TDesignBoxBaseItem);
    { Private declarations }
  public

    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  DesignBox1.Items.AddText(20, 20, 'Some Text');
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  DesignBox1.Items.AddGraphic(20, 20, Image1.Picture.Graphic);
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
