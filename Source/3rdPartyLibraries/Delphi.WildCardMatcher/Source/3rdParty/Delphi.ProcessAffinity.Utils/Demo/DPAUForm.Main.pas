unit DPAUForm.Main;

interface

uses
  Winapi.Messages, Winapi.Windows, System.Classes, System.SysUtils, System.Variants, Vcl.Controls, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.Graphics, Vcl.StdCtrls;

type
  TDPAUMainForm = class(TForm)
    ButtonRefresh: TButton;
    ButtonRestore: TButton;
    ButtonUseEfficiencyCores: TButton;
    ButtonUsePerformanceCores: TButton;
    CheckBoxForce: TCheckBox;
    MemoLog: TMemo;
    PanelButtons: TPanel;
    ButtonClearLog: TButton;
    procedure ButtonClearLogClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonRestoreClick(Sender: TObject);
    procedure ButtonUseEfficiencyCoresClick(Sender: TObject);
    procedure ButtonUsePerformanceCoresClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function MaskToBits(const AMask: NativeUInt): string;
    procedure ApplyMask(const ADescription: string; const AMask: NativeUInt);
    procedure LogCurrentMasks;
    procedure LogMask(const AName: string; const AMask: NativeUInt);
  end;

var
  DPAUMainForm: TDPAUMainForm;

implementation

uses
  Delphi.ProcessAffinity.Utils;

{$R *.dfm}

procedure TDPAUMainForm.FormCreate(Sender: TObject);
begin
  MemoLog.Lines.Add(Format('Logical processors: %d', [TThread.ProcessorCount]));
  MemoLog.Lines.Add('Bit map below is one character per logical processor, CPU 0 first ("1" = in mask)');
  MemoLog.Lines.Add('');

  LogCurrentMasks;
end;

function TDPAUMainForm.MaskToBits(const AMask: NativeUInt): string;
var
  LCpu: Integer;
begin
  Result := '';

  for LCpu := 0 to TThread.ProcessorCount - 1 do
    if (AMask and (NativeUInt(1) shl LCpu)) <> 0 then
      Result := Result + '1'
    else
      Result := Result + '.';
end;

procedure TDPAUMainForm.LogMask(const AName: string; const AMask: NativeUInt);
begin
  MemoLog.Lines.Add(Format('%-18s 0x%s  %s', [AName, IntToHex(UInt64(AMask), 16), MaskToBits(AMask)]));
end;

procedure TDPAUMainForm.LogCurrentMasks;
begin
  // Handle-less overloads operate on the current process
  LogMask('System mask', GetSystemAffinityMask);
  LogMask('Process mask', GetAffinityMask);
  LogMask('Performance cores', GetPerformanceAffinityMask(CheckBoxForce.Checked));
  LogMask('Efficiency cores', GetEfficiencyAffinityMask(CheckBoxForce.Checked));
  MemoLog.Lines.Add('');
end;

procedure TDPAUMainForm.ApplyMask(const ADescription: string; const AMask: NativeUInt);
begin
  if AMask = 0 then
  begin
    MemoLog.Lines.Add(ADescription + ': no mask available');
    MemoLog.Lines.Add('  (homogeneous CPU, already modified mask without "Force query", or an API failure)');
    MemoLog.Lines.Add('');

    Exit;
  end;

  if SetAffinityMask(AMask) then
    MemoLog.Lines.Add(ADescription + ': mask applied')
  else
    MemoLog.Lines.Add(ADescription + ': SetAffinityMask failed');

  LogMask('Process mask now', GetAffinityMask);
  MemoLog.Lines.Add('');
end;

procedure TDPAUMainForm.ButtonClearLogClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

procedure TDPAUMainForm.ButtonRefreshClick(Sender: TObject);
begin
  LogCurrentMasks;
end;

procedure TDPAUMainForm.ButtonUsePerformanceCoresClick(Sender: TObject);
begin
  ApplyMask('Use performance cores', GetPerformanceAffinityMask(CheckBoxForce.Checked));
end;

procedure TDPAUMainForm.ButtonUseEfficiencyCoresClick(Sender: TObject);
begin
  ApplyMask('Use efficiency cores', GetEfficiencyAffinityMask(CheckBoxForce.Checked));
end;

procedure TDPAUMainForm.ButtonRestoreClick(Sender: TObject);
begin
  RestoreAffinityMask;

  MemoLog.Lines.Add('Affinity mask restored to the system mask');
  LogMask('Process mask now', GetAffinityMask);
  MemoLog.Lines.Add('');
end;

end.
