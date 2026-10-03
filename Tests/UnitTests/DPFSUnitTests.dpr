program DPFSUnitTests;

{$IFNDEF TESTINSIGHT}
  {$APPTYPE CONSOLE}
{$ENDIF}
{.$DEFINE CHECK_MEMORY_LEAKS}

{$STRONGLINKTYPES ON}

uses
  FastMM5,
  {$IFDEF CHECK_MEMORY_LEAKS}
  DUnitX.MemoryLeakMonitor.FastMM5,
  {$ENDIF }
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.XML.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  DPFSUnit.Parallel.FileScanner in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.pas',
  DPFSUnit.Parallel.FileScanner.OTL in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.OTL.pas',
  DPFSUnit.Parallel.FileScanner.Spring in '..\..\Source\Units\DPFSUnit.Parallel.FileScanner.Spring.pas',
  DPFSUnit.Parallel.FileScanner.Tests.Common in 'Tests\DPFSUnit.Parallel.FileScanner.Tests.Common.pas',
  DPFSUnit.Parallel.FileScanner.Tests in 'Tests\DPFSUnit.Parallel.FileScanner.Tests.pas',
  DPFSUnit.Parallel.FileScanner.OTL.Tests in 'Tests\DPFSUnit.Parallel.FileScanner.OTL.Tests.pas',
  DPFSUnit.Parallel.FileScanner.Spring.Tests in 'Tests\DPFSUnit.Parallel.FileScanner.Spring.Tests.pas',
  DPFSUnit.Parallel.FileScanner.Workers.Tests in 'Tests\DPFSUnit.Parallel.FileScanner.Workers.Tests.pas';

{ keep comment here to protect the following conditional from being removed by the IDE when adding a unit }
{$IFNDEF TESTINSIGHT}
var
  LRunner: ITestRunner;
  LResults: IRunResults;
  LLogger: ITestLogger;
  LNunitLogger: ITestLogger;
{$ENDIF}
begin
{$IFDEF DEBUG}
  // FastMM5 debug mode scrubs freed memory and checks blocks on free and reallocation, so a use-after-free or an
  // overrun in the multithreaded scanning code fails loudly in the tests instead of silently "working".
  FastMM_EnterDebugMode;
{$ENDIF}

{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    LRunner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    LRunner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    LRunner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      LLogger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      LRunner.AddLogger(LLogger);
    end;
    //Generate an NUnit compatible XML File
    LNunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    LRunner.AddLogger(LNunitLogger);

    //Run tests
    LResults := LRunner.Execute;
    if not LResults.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
