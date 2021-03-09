program DesignBoxDemo;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Design Box Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
