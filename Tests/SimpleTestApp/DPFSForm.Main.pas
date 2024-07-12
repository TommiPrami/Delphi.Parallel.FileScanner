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
    procedure ButtonParallelScanClick(Sender: TObject);
    procedure ButtonParallelScanSpringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonOtlQueueClick(Sender: TObject);
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

uses
  DPFSUnit.Parallel.FileScanner.Spring, Spring.Collections
  {$IFDEF USE_OMNI_THREAD_LIBRARY}, OtlTaskControl, OtlContainers, OtlCommon {$ENDIF};

{$R *.dfm}

procedure TDPFSMainForm.ButtonOtlQueueClick(Sender: TObject);
{$IFDEF USE_OMNI_THREAD_LIBRARY}
var
  LParallelScanner: TParallelFileScanner;
  LOTLValueQueue: IOmniValueQueue;
  LExludes: TFileScanExclusions;
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
      LExludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExludes, LOTLValueQueue, LFileCount, tpIdle) then
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
  LExludes: TFileScanExclusions;
begin
  UpdateGUIState(Sender as TControl, False);
  try
    LFilesList := TStringList.Create;
    LParallelScanner := TParallelFileScanner.Create(GetExtensions);
    try
      LExludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExludes, LFilesList
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
var
  LParallelScanner: TParallelFileScannerSpring;
  LFilesList: IList<string>;
  LExludes: TFileScanExclusions;
begin
  UpdateGUIState(Sender as TControl, False);
  try

    LFilesList := TCollections.CreateList<string>;
    LParallelScanner := TParallelFileScannerSpring.Create(GetExtensions);
    try
      LExludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExludes, LFilesList
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
end;

procedure TDPFSMainForm.FormCreate(Sender: TObject);
begin
  ButtonOtlQueue.Enabled := {$IFDEF USE_OMNI_THREAD_LIBRARY}True; {$ELSE}False;{$ENDIF};
  ButtonOtlQueue.Visible := ButtonOtlQueue.Enabled;
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
