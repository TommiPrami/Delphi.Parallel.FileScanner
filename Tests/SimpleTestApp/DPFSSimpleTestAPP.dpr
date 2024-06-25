program DPFSSimpleTestAPP;

uses
  Vcl.Forms,
  DPFSForm.Main in 'DPFSForm.Main.pas' {DPFSMainForm},
  DPFSUnit.Parallel.FileScanner in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.pas',
  DPFSUnit.Parallel.FileScanner.Spring in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.Spring.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDPFSMainForm, DPFSMainForm);
  Application.Run;
end.
