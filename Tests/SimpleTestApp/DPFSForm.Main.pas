unit DPFSForm.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TDPFSMainForm = class(TForm)
    ButtonParallelScan: TButton;
    ButtonParallelScanSpring: TButton;
    MemoLog: TMemo;
    procedure ButtonParallelScanClick(Sender: TObject);
    procedure ButtonParallelScanSpringClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DPFSMainForm: TDPFSMainForm;

implementation

uses
  DPFSUnit.Parallel.FileScanner, DPFSUnit.Parallel.FileScanner.Spring, Spring.Collections;

{$R *.dfm}

procedure TDPFSMainForm.ButtonParallelScanClick(Sender: TObject);
var
  LParallelScanner: TParallelFileScanner;
  LFilesList: TStringList;
  LExludes: TFileScanExcludes;
begin
  MemoLog.Clear;

  LFilesList := TStringList.Create;
  LParallelScanner := TParallelFileScanner.Create(['*.pas', '*.inc', '*.dfm']);
  try
    // LExludes.
    if LParallelScanner.GetFileList(['..\..\..\..\Source\', '..\..\..\..\Tests\'], LExludes, LFilesList) then
    begin
      MemoLog.Lines.AddStrings(LFilesList);
      MemoLog.Lines.Add('');
      MemoLog.Lines.Add('OK ' + LFilesList.Count.ToString + ' files.');
    end
    else
      MemoLog.Lines.Add('No files found.');

    MemoLog.Lines.Add('');
    MemoLog.Lines.Add('  Elapsed time: ' + LParallelScanner.DiskScanTimeForFiles.ToString + ' ms.');
  finally
    LParallelScanner.Free;
    LFilesList.Free;
  end;
end;

procedure TDPFSMainForm.ButtonParallelScanSpringClick(Sender: TObject);
var
  LParallelScanner: TParallelFileScannerSpring;
  LFilesList: IList<string>;
  LExludes: TFileScanExcludes;
begin
  MemoLog.Clear;

  LFilesList := TCollections.CreateList<string>;
  LParallelScanner := TParallelFileScannerSpring.Create(['*.pas', '*.inc', '*.dfm']);
  try
    // LExludes.
    if LParallelScanner.GetFileList(['..\..\..\..\Source\', '..\..\..\..\Tests\'], LExludes, LFilesList) then
    begin
      MemoLog.Lines.AddStrings(LFilesList.ToArray);
      MemoLog.Lines.Add('');
      MemoLog.Lines.Add('OK ' + LFilesList.Count.ToString + ' files.');
    end
    else
      MemoLog.Lines.Add('No files found.');

    MemoLog.Lines.Add('');
    MemoLog.Lines.Add('  Elapsed time: ' + LParallelScanner.DiskScanTimeForFiles.ToString + ' ms.');
  finally
    LParallelScanner.Free;
  end;
end;

end.
