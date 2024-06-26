﻿unit DPFSForm.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DPFSUnit.Parallel.FileScanner;

type
  TDPFSMainForm = class(TForm)
    ButtonParallelScan: TButton;
    ButtonParallelScanSpring: TButton;
    MemoLog: TMemo;
    procedure ButtonParallelScanClick(Sender: TObject);
    procedure ButtonParallelScanSpringClick(Sender: TObject);
  private
    function GetExcludes: TFileScanExcludes;
    function GetExtensions: TArray<string>;
    function GetSearchDirectories: TArray<string>;
  public
  end;

var
  DPFSMainForm: TDPFSMainForm;

implementation

uses
  DPFSUnit.Parallel.FileScanner.Spring, Spring.Collections;

{$R *.dfm}

procedure TDPFSMainForm.ButtonParallelScanClick(Sender: TObject);
var
  LParallelScanner: TParallelFileScanner;
  LFilesList: TStringList;
  LExludes: TFileScanExcludes;
begin
  MemoLog.Clear;

  LFilesList := TStringList.Create;
  LParallelScanner := TParallelFileScanner.Create(GetExtensions);
  try
    LExludes := GetExcludes;

    if LParallelScanner.GetFileList(GetSearchDirectories, LExludes, LFilesList) then
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
  LParallelScanner := TParallelFileScannerSpring.Create(GetExtensions);
  try
    LExludes := GetExcludes;

    if LParallelScanner.GetFileList(GetSearchDirectories, LExludes, LFilesList) then
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

function TDPFSMainForm.GetExcludes: TFileScanExcludes;
begin
  Result.BeginUpdate;
  try
    Result.PathPrefixes := ['..\..\..\..\Source\3rdPartyLibraries\OmniThreadLibrary\'];
  finally
    Result.EndUpdate;
  end;
end;

function TDPFSMainForm.GetExtensions: TArray<string>;
begin
  Result := ['*.pas', '*.inc', '*.dfm', '*.dpr', '*.dproj'];
end;

function TDPFSMainForm.GetSearchDirectories: TArray<string>;
begin
  Result := ['..\..\..\..\Source\', '..\..\..\..\Tests\'];
end;

end.
