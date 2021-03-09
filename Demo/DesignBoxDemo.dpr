program DesignBoxDemo;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  DesignBox in 'DesignBox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
