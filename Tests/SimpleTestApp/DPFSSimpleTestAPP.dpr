program DPFSSimpleTestAPP;

uses
  FastMM5,
  Vcl.Forms,
  DPFSForm.Main in 'DPFSForm.Main.pas' {DPFSMainForm},
  DPFSUnit.Parallel.FileScanner in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.pas',
  DPFSUnit.Parallel.FileScanner.Spring in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.Spring.pas';

{$R *.res}

begin
  FastMM_DeleteEventLogFile;
{$IFDEF DEBUG}
  FastMM_EnterDebugMode;
{$ELSE}
  FastMM_EnterMinimumAddressAlignment(maa16Bytes);
  FastMM_SetOptimizationStrategy(mmosOptimizeForSpeed);
{$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDPFSMainForm, DPFSMainForm);
  Application.Run;
end.
