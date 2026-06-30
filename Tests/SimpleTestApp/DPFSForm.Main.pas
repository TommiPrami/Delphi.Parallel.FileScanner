unit DPFSForm.Main;

interface

{$INCLUDE ..\..\Source\Units\DPFSUnit.Parallel.FileScanner.inc}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DPFSUnit.Parallel.FileScanner;

type
  TDPFSMainForm = class(TForm)
    ButtonParallelScan: TButton;
    ButtonParallelScanSpring: TButton;
    MemoLog: TMemo;
    ButtonOtlQueue: TButton;
    CheckBoxConvertRelativePathsToAbsolute: TCheckBox;
    procedure ButtonOtlQueueClick(Sender: TObject);
    procedure ButtonParallelScanClick(Sender: TObject);
    procedure ButtonParallelScanSpringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    procedure UpdateGUIState(const ASender: TControl; const AEnabled: Boolean);
    procedure LogCommon(const AParallelScanner: TParallelFileScannerCustom);
    function GetExcludes: TFileScanExclusions;
    function GetExtensions: TArray<string>;
    function GetSearchDirectories: TArray<string>;
    // procedure GetSearchDirectoriesStrings(const ADirectories: TStringList);
  end;

var
  DPFSMainForm: TDPFSMainForm;

implementation

{$IF DEFINED(USE_SPRING4D) or DEFINED(USE_OMNI_THREAD_LIBRARY)}
uses
{$IF DEFINED(USE_SPRING4D) and DEFINED(USE_OMNI_THREAD_LIBRARY)}
  DPFSUnit.Parallel.FileScanner.Spring, Spring.Collections,
  OtlTaskControl, OtlContainers, OtlCommon;
{$ELSEIF DEFINED(USE_SPRING4D)}
  DPFSUnit.Parallel.FileScanner.Spring, Spring.Collections;
{$ELSE}
  OtlTaskControl, OtlContainers, OtlCommon;
{$IFEND}
{$IFEND}

{$R *.dfm}

procedure TDPFSMainForm.ButtonOtlQueueClick(Sender: TObject);
{$IFDEF USE_OMNI_THREAD_LIBRARY}
var
  LParallelScanner: TParallelFileScanner;
  LOTLValueQueue: IOmniValueQueue;
  LExcludes: TFileScanExclusions;
  LResultFileName: string;
  LFileCount: Integer;
{$ENDIF}
begin
{$IFDEF USE_OMNI_THREAD_LIBRARY}
  LFileCount := 0;

  UpdateGUIState(Sender as TControl, False);
  try
    LOTLValueQueue := CreateOmniValueQueue(False);
    LParallelScanner := TParallelFileScanner.Create(GetExtensions);
    try
      LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
      LExcludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LOTLValueQueue, LFileCount, tpIdle) then
      begin
        var LValue: TOmniValue;

        while LOTLValueQueue.TryDequeue(LValue) do
        begin
          LResultFileName := LValue;

          MemoLog.Lines.Add(LResultFileName);
        end;

        MemoLog.Lines.Add('');
        MemoLog.Lines.Add('OK ' + LFileCount.ToString + ' files.');
      end
      else
        MemoLog.Lines.Add('No files found.');

      LogCommon(LParallelScanner);
    finally
      LParallelScanner.Free;
    end;
  finally
    UpdateGUIState(Sender as TControl, True);
  end;
{$ENDIF}
end;

procedure TDPFSMainForm.ButtonParallelScanClick(Sender: TObject);
var
  LParallelScanner: TParallelFileScanner;
  LFilesList: TStringList;
  LExcludes: TFileScanExclusions;
begin
  UpdateGUIState(Sender as TControl, False);
  try
    LFilesList := TStringList.Create;
    LParallelScanner := TParallelFileScanner.Create(GetExtensions);
    try
      LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
      LExcludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LFilesList
      {$IFDEF USE_OMNI_THREAD_LIBRARY}, tpIdle {$ENDIF}) then
      begin
        MemoLog.Lines.AddStrings(LFilesList);
        MemoLog.Lines.Add('');
        MemoLog.Lines.Add('OK ' + LFilesList.Count.ToString + ' files.');
      end
      else
        MemoLog.Lines.Add('No files found.');

      LogCommon(LParallelScanner);
    finally
      LParallelScanner.Free;
      LFilesList.Free;
    end;
  finally
    UpdateGUIState(Sender as TControl, True);
  end;
end;

procedure TDPFSMainForm.ButtonParallelScanSpringClick(Sender: TObject);
{$IFDEF USE_SPRING4D}
var
  LParallelScanner: TParallelFileScannerSpring;
  LFilesList: IList<string>;
  LExcludes: TFileScanExclusions;
{$ENDIF}
begin
{$IFDEF USE_SPRING4D}
  UpdateGUIState(Sender as TControl, False);
  try
    LFilesList := TCollections.CreateList<string>;
    LParallelScanner := TParallelFileScannerSpring.Create(GetExtensions);
    try
      LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
      LExcludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LFilesList
        {$IFDEF USE_OMNI_THREAD_LIBRARY}, tpIdle {$ENDIF}) then
      begin
        MemoLog.Lines.AddStrings(LFilesList.ToArray);
        MemoLog.Lines.Add('');
        MemoLog.Lines.Add('OK ' + LFilesList.Count.ToString + ' files.');
      end
      else
        MemoLog.Lines.Add('No files found.');

      LogCommon(LParallelScanner);
    finally
      LParallelScanner.Free;
    end;
  finally
    UpdateGUIState(Sender as TControl, True);
  end;
{$ENDIF}
end;

procedure TDPFSMainForm.FormCreate(Sender: TObject);
begin
  // Keep every scan variant visible so they stay discoverable; just disable the ones whose
  // backend was not compiled into this build. The missing define is shown in the caption
  // because VCL does not display hints for disabled windowed controls.
  {$IFNDEF USE_OMNI_THREAD_LIBRARY}
  ButtonOtlQueue.Enabled := False;
  ButtonOtlQueue.Caption := ButtonOtlQueue.Caption + ' (needs USE_OMNI_THREAD_LIBRARY)';
  {$ENDIF}

  {$IFNDEF USE_SPRING4D}
  ButtonParallelScanSpring.Enabled := False;
  ButtonParallelScanSpring.Caption := ButtonParallelScanSpring.Caption + ' (needs USE_SPRING4D)';
  {$ENDIF}
end;

function TDPFSMainForm.GetExcludes: TFileScanExclusions;
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

(*
procedure TDPFSMainForm.GetSearchDirectoriesStrings(const ADirectories: TStringList);
begin
  ADirectories.AddStrings(GetSearchDirectories);
end; *)

procedure TDPFSMainForm.LogCommon(const AParallelScanner: TParallelFileScannerCustom);
begin
  MemoLog.Lines.Add('');
  MemoLog.Lines.Add('  Elapsed time: ' + AParallelScanner.DiskScanTimeForFiles.ToString
    + ' ms. (excluding fetching skipped files count)');
  MemoLog.Lines.Add('  ' + AParallelScanner.SkippedFilesCount.ToString + ' files skipped');
end;

procedure TDPFSMainForm.UpdateGUIState(const ASender: TControl; const AEnabled: Boolean);
begin
  Enabled := AEnabled;
  ASender.Enabled := AEnabled;

  if not AEnabled then
  begin
    Screen.Cursor := crHourGlass;
    MemoLog.Clear;
    MemoLog.Lines.BeginUpdate;
  end
  else
  begin
    MemoLog.Lines.EndUpdate;
    MemoLog.Perform(WM_VSCROLL, SB_BOTTOM, 0);
    Screen.Cursor := crDefault;
  end;
end;

end.
