unit DPFSForm.Main;

interface

{$INCLUDE ..\..\Source\Units\DPFSUnit.Parallel.FileScanner.inc}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Diagnostics,
  System.Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  DPFSUnit.Parallel.FileScanner, Vcl.ExtCtrls;

type
  // Runs one scan and returns its elapsed time in milliseconds (and the file count via the out param).
  TTimedScan = function(var AFileCount: Integer): Double of object;

  TDPFSMainForm = class(TForm)
    ButtonOtlQueue: TButton;
    ButtonParallelScan: TButton;
    ButtonParallelScanSpring: TButton;
    CheckBoxConvertRelativePathsToAbsolute: TCheckBox;
    ComboBoxDirectories: TComboBox;
    MemoLog: TMemo;
    PanelSpeedTest: TPanel;
    ButtonSpeedTest: TButton;
    EditLoopCount: TEdit;
    procedure ButtonOtlQueueClick(ASender: TObject);
    procedure ButtonParallelScanClick(ASender: TObject);
    procedure ButtonParallelScanSpringClick(ASender: TObject);
    procedure ButtonSpeedTestClick(ASender: TObject);
    procedure FormCreate(ASender: TObject);
  strict private
    function GetExcludes: TFileScanExclusions;
    function GetExtensions: TArray<string>;
    function GetSearchDirectories: TArray<string>;
    function TimedRtlScan(var AFileCount: Integer): Double;
    procedure BenchmarkScan(const ACaption: string; const ATimedScan: TTimedScan; const ALoopCount: Integer);
    procedure LogCommon(const AParallelScanner: TParallelFileScannerCustom);
    procedure UpdateGUIState(const ASender: TControl; const AEnabled: Boolean);
    {$IFDEF USE_OMNI_THREAD_LIBRARY}
    function TimedOtlQueueScan(var AFileCount: Integer): Double;
    {$ENDIF}
    {$IFDEF USE_SPRING4D}
    function TimedSpringScan(var AFileCount: Integer): Double;
    {$ENDIF}
  end;

var
  DPFSMainForm: TDPFSMainForm;

implementation

uses
  FastMM5
{$IF DEFINED(USE_SPRING4D) or DEFINED(USE_OMNI_THREAD_LIBRARY)}
  {$IF DEFINED(USE_SPRING4D) and DEFINED(USE_OMNI_THREAD_LIBRARY)}
  , DPFSUnit.Parallel.FileScanner.Spring, Spring.Collections, OtlTaskControl, OtlContainers, OtlCommon;
  {$ELSEIF DEFINED(USE_SPRING4D)}
  , DPFSUnit.Parallel.FileScanner.Spring, Spring.Collections;
{$ELSE}
  , OtlTaskControl, OtlContainers, OtlCommon;
  {$IFEND}
{$IFEND}

{$R *.dfm}

procedure TDPFSMainForm.ButtonOtlQueueClick(ASender: TObject);
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

  UpdateGUIState(ASender as TControl, False);
  try
    LOTLValueQueue := CreateOmniValueQueue(False);
    LParallelScanner := TParallelFileScanner.Create(GetExtensions);
    try
      LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
      LExcludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LOTLValueQueue, LFileCount, tpNormal) then
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
    UpdateGUIState(ASender as TControl, True);
  end;
{$ENDIF}
end;

procedure TDPFSMainForm.ButtonParallelScanClick(ASender: TObject);
var
  LParallelScanner: TParallelFileScanner;
  LFilesList: TStringList;
  LExcludes: TFileScanExclusions;
begin
  UpdateGUIState(ASender as TControl, False);
  try
    LFilesList := TStringList.Create;
    LParallelScanner := TParallelFileScanner.Create(GetExtensions);
    try
      LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
      LExcludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LFilesList
      {$IFDEF USE_OMNI_THREAD_LIBRARY}, tpNormal {$ENDIF}) then
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
    UpdateGUIState(ASender as TControl, True);
  end;
end;

procedure TDPFSMainForm.ButtonParallelScanSpringClick(ASender: TObject);
{$IFDEF USE_SPRING4D}
var
  LParallelScanner: TParallelFileScannerSpring;
  LFilesList: IList<string>;
  LExcludes: TFileScanExclusions;
{$ENDIF}
begin
{$IFDEF USE_SPRING4D}
  UpdateGUIState(ASender as TControl, False);
  try
    LFilesList := TCollections.CreateList<string>;
    LParallelScanner := TParallelFileScannerSpring.Create(GetExtensions);
    try
      LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
      LExcludes := GetExcludes;

      if LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LFilesList
        {$IFDEF USE_OMNI_THREAD_LIBRARY}, tpNormal {$ENDIF}) then
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
    UpdateGUIState(ASender as TControl, True);
  end;
{$ENDIF}
end;

procedure TDPFSMainForm.ButtonSpeedTestClick(ASender: TObject);
var
  LLoopCount: Integer;
begin
  UpdateGUIState(ASender as TControl, False);
  try
    LLoopCount := StrToIntDef(EditLoopCount.Text, -1);
    if LLoopCount <= 0 then
    begin
      LLoopCount := 100;
      EditLoopCount.Text := LLoopCount.ToString;
    end;

    MemoLog.Lines.Clear;
    MemoLog.Lines.Add('Speed test - 5 warm-up runs (discarded) then ' + LLoopCount.ToString + ' timed runs per scan.');
    MemoLog.Lines.Add('Directories: ' + string.Join('; ', GetSearchDirectories));
    MemoLog.Lines.Add('Extensions : ' + string.Join(' ', GetExtensions));
    MemoLog.Lines.Add('');

    BenchmarkScan('Default (RTL TStringList)', TimedRtlScan, LLoopCount);
    Application.ProcessMessages; // keep the app responsive between the (frozen) timed loops
    {$IFDEF USE_OMNI_THREAD_LIBRARY}
    BenchmarkScan('OTL value queue', TimedOtlQueueScan, LLoopCount);
    Application.ProcessMessages;
    {$ENDIF}
    {$IFDEF USE_SPRING4D}
    BenchmarkScan('Spring4D IList', TimedSpringScan, LLoopCount);
    {$ENDIF}
  finally
    UpdateGUIState(ASender as TControl, True);
  end;
end;

procedure TDPFSMainForm.BenchmarkScan(const ACaption: string; const ATimedScan: TTimedScan; const ALoopCount: Integer);
const
  WARMUP_RUNS = 5;
var
  LFileCount: Integer;
  LTimes: TList<Double>;
  LBest, LSum, LElapsed: Double;
  LIndex: Integer;
begin
  // Warm-up runs prime the filesystem cache and code paths; their times are discarded.
  for LIndex := 1 to WARMUP_RUNS do
    ATimedScan(LFileCount);

  LTimes := TList<Double>.Create;
  try
    LBest := 0;
    LSum := 0;

    for LIndex := 1 to ALoopCount do
    begin
      LElapsed := ATimedScan(LFileCount);
      LTimes.Add(LElapsed);
      LSum := LSum + LElapsed;

      if (LIndex = 1) or (LElapsed < LBest) then
        LBest := LElapsed;
    end;

    LTimes.Sort; // for the median

    MemoLog.Lines.Add(Format('%-26s files=%-5d best=%7.2f ms   avg=%7.2f ms   median=%7.2f ms',
      [ACaption, LFileCount, LBest, LSum / ALoopCount, LTimes[LTimes.Count div 2]]));
  finally
    LTimes.Free;
  end;
end;

function TDPFSMainForm.TimedRtlScan(var AFileCount: Integer): Double;
var
  LParallelScanner: TParallelFileScanner;
  LFilesList: TStringList;
  LExcludes: TFileScanExclusions;
  LStopwatch: TStopwatch;
begin
  LParallelScanner := TParallelFileScanner.Create(GetExtensions);
  LFilesList := TStringList.Create;
  try
    LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
    LExcludes := GetExcludes;

    LStopwatch := TStopwatch.StartNew;
    LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LFilesList
      {$IFDEF USE_OMNI_THREAD_LIBRARY}, tpNormal {$ENDIF});
    LStopwatch.Stop;

    Result := LStopwatch.Elapsed.TotalMilliseconds;
    AFileCount := LFilesList.Count;
  finally
    LFilesList.Free;
    LParallelScanner.Free;
  end;
end;

{$IFDEF USE_OMNI_THREAD_LIBRARY}
function TDPFSMainForm.TimedOtlQueueScan(var AFileCount: Integer): Double;
var
  LParallelScanner: TParallelFileScanner;
  LOTLValueQueue: IOmniValueQueue;
  LExcludes: TFileScanExclusions;
  LStopwatch: TStopwatch;
  LFileCount: Integer;
begin
  LParallelScanner := TParallelFileScanner.Create(GetExtensions);
  try
    LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
    LExcludes := GetExcludes;
    LOTLValueQueue := CreateOmniValueQueue(False);
    LFileCount := 0;

    LStopwatch := TStopwatch.StartNew;
    LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LOTLValueQueue, LFileCount, tpNormal);
    LStopwatch.Stop;

    AFileCount := LFileCount;
    Result := LStopwatch.Elapsed.TotalMilliseconds;

    // Drain (outside the timed section) before the queue is released: releasing a non-empty
    // OmniThreadLibrary queue repeatedly can fault, so empty it between runs.
    var LValue: TOmniValue;
    while LOTLValueQueue.TryDequeue(LValue) do
      ;
  finally
    LParallelScanner.Free;
  end;
end;
{$ENDIF}

{$IFDEF USE_SPRING4D}
function TDPFSMainForm.TimedSpringScan(var AFileCount: Integer): Double;
var
  LParallelScanner: TParallelFileScannerSpring;
  LFilesList: IList<string>;
  LExcludes: TFileScanExclusions;
  LStopwatch: TStopwatch;
begin
  LParallelScanner := TParallelFileScannerSpring.Create(GetExtensions);
  try
    LParallelScanner.ConvertRelativePathsToAbsolute := CheckBoxConvertRelativePathsToAbsolute.Checked;
    LExcludes := GetExcludes;
    LFilesList := TCollections.CreateList<string>;

    LStopwatch := TStopwatch.StartNew;
    LParallelScanner.GetFileList(GetSearchDirectories, LExcludes, LFilesList
      {$IFDEF USE_OMNI_THREAD_LIBRARY}, tpNormal {$ENDIF});
    LStopwatch.Stop;

    AFileCount := LFilesList.Count;
    Result := LStopwatch.Elapsed.TotalMilliseconds;
  finally
    LParallelScanner.Free;
  end;
end;
{$ENDIF}

procedure TDPFSMainForm.FormCreate(ASender: TObject);
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
  Result.PathPrefixes := ['..\..\..\..\Source\3rdPartyLibraries\OmniThreadLibrary\'];
end;

function TDPFSMainForm.GetExtensions: TArray<string>;
begin
  Result := ['*.pas', '*.inc', '*.dfm', '*.dpr', '*.dproj'];
end;

function TDPFSMainForm.GetSearchDirectories: TArray<string>;
var
  LDirectories: TStringList;
begin
  // The combo box holds a semicolon-separated list of root directories (editable).
  LDirectories := TStringList.Create;
  try
    LDirectories.StrictDelimiter := True;
    LDirectories.Delimiter := ';';
    LDirectories.DelimitedText := ComboBoxDirectories.Text;

    for var LIndex := 0 to LDirectories.Count - 1 do
      LDirectories[LIndex] := Trim(LDirectories[LIndex]);

    Result := LDirectories.ToStringArray;
  finally
    LDirectories.Free;
  end;
end;

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
