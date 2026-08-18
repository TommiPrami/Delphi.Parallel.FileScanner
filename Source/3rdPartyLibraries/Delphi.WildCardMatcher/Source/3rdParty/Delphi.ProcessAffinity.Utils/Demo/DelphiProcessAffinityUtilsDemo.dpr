program DelphiProcessAffinityUtilsDemo;

uses
  Vcl.Forms,
  DPAUForm.Main in 'DPAUForm.Main.pas' {DPAUMainForm},
  Delphi.ProcessAffinity.Utils in '..\Source\Delphi.ProcessAffinity.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDPAUMainForm, DPAUMainForm);
  Application.Run;
end.
