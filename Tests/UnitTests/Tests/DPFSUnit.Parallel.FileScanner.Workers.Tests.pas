unit DPFSUnit.Parallel.FileScanner.Workers.Tests;

// Windows only, like the scanners, so TThreadPriority being Windows-specific is fine.
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  DUnitX.TestFramework;

type
  // TFileScanWorkers on its own - limits, clamping, the Initialize helpers - and the scanner owning it. How a scan
  // uses its worker count is tested with every scanner class in TParallelFileScannerTestsCustom.
  [TestFixture]
  TFileScanWorkersTests = class(TObject)
  public
    [Test] procedure DefaultsAreOneWorkerPerCoreAtNormalPriority;
    [Test] procedure CountIsKeptWithinLimits;
    [Test] procedure InitializeSetsLimitsAndCount;
    [Test] procedure InitializeDefaultsToCoreCountAndOne;
    [Test] procedure ExplicitMinimumBeatsDefaultMaximum;
    [Test] procedure InitializePercentageTakesShareOfCores;
    [Test] procedure PercentageOverHundredAllowsMoreWorkersThanCores;
    [Test] procedure InvalidArgumentsRaise;
    [Test] procedure SettingOneLimitPushesTheOther;
    [Test] procedure ScannerOwnsAndFreesWorkers;
    [Test] procedure ScannerFreesWorkersWhenCreateFails;
  end;

implementation

uses
  System.Classes, System.Math, System.SysUtils, DPFSUnit.Parallel.FileScanner;

type
  // Reports its own destruction, so the tests can see the scanner freeing it.
  TTrackedWorkers = class(TFileScanWorkers)
  strict private
    FDestroyed: PBoolean;
  public
    constructor Create(const ADestroyed: PBoolean);
    destructor Destroy; override;
  end;

  // A scanner whose constructor fails after the inherited one has run.
  TFailingScanner = class(TParallelFileScanner)
  public
    constructor Create(const AExtensions: TArray<string>; const AWorkers: TFileScanWorkers;
      const ASortResultList: Boolean = True); override;
  end;

{ TTrackedWorkers }

constructor TTrackedWorkers.Create(const ADestroyed: PBoolean);
begin
  inherited Create;

  FDestroyed := ADestroyed;
  FDestroyed^ := False;
end;

destructor TTrackedWorkers.Destroy;
begin
  FDestroyed^ := True;

  inherited Destroy;
end;

{ TFailingScanner }

constructor TFailingScanner.Create(const AExtensions: TArray<string>; const AWorkers: TFileScanWorkers;
  const ASortResultList: Boolean = True);
begin
  inherited Create(AExtensions, AWorkers, ASortResultList);

  raise EAbort.Create('Create failed on purpose');
end;

{ TFileScanWorkersTests }

procedure TFileScanWorkersTests.DefaultsAreOneWorkerPerCoreAtNormalPriority;
begin
  var LWorkers := TFileScanWorkers.Create;
  try
    Assert.AreEqual(Max(1, TThread.ProcessorCount), TFileScanWorkers.CoreCount, 'CoreCount');
    Assert.AreEqual(1, LWorkers.MinCount, 'MinCount');
    Assert.AreEqual(TFileScanWorkers.CoreCount, LWorkers.MaxCount, 'MaxCount');
    Assert.AreEqual(TFileScanWorkers.CoreCount, LWorkers.Count, 'Count');
    Assert.IsTrue(LWorkers.Priority = TThreadPriority.tpNormal, 'Priority');
  finally
    LWorkers.Free;
  end;
end;

// Minimum 2 and maximum 6: asking for 1 or 42 gives 2 or 6.
procedure TFileScanWorkersTests.CountIsKeptWithinLimits;
begin
  var LWorkers := TFileScanWorkers.Create;
  try
    LWorkers.MinCount := 2;
    LWorkers.MaxCount := 6;

    LWorkers.Count := 1;
    Assert.AreEqual(2, LWorkers.Count, 'Count below MinCount');

    LWorkers.Count := 42;
    Assert.AreEqual(6, LWorkers.Count, 'Count above MaxCount');

    LWorkers.Count := 4;
    Assert.AreEqual(4, LWorkers.Count, 'Count within the limits');
  finally
    LWorkers.Free;
  end;
end;

procedure TFileScanWorkersTests.InitializeSetsLimitsAndCount;
begin
  var LWorkers := TFileScanWorkers.Create;
  try
    LWorkers.Initialize(4, 6, 2);
    Assert.AreEqual(4, LWorkers.Count, 'Initialize(4, 6, 2): Count');
    Assert.AreEqual(6, LWorkers.MaxCount, 'Initialize(4, 6, 2): MaxCount');
    Assert.AreEqual(2, LWorkers.MinCount, 'Initialize(4, 6, 2): MinCount');

    LWorkers.Initialize(42, 6, 2);
    Assert.AreEqual(6, LWorkers.Count, 'Initialize(42, 6, 2)');

    LWorkers.Initialize(1, 6, 2);
    Assert.AreEqual(2, LWorkers.Count, 'Initialize(1, 6, 2)');
  finally
    LWorkers.Free;
  end;
end;

procedure TFileScanWorkersTests.InitializeDefaultsToCoreCountAndOne;
begin
  var LWorkers := TFileScanWorkers.Create;
  try
    LWorkers.Initialize(2, 3, 2);
    LWorkers.Initialize(1000);
    Assert.AreEqual(TFileScanWorkers.CoreCount, LWorkers.MaxCount, 'Initialize(1000): MaxCount back to the default');
    Assert.AreEqual(1, LWorkers.MinCount, 'Initialize(1000): MinCount back to the default');
    Assert.AreEqual(TFileScanWorkers.CoreCount, LWorkers.Count, 'Initialize(1000): Count');

    LWorkers.Initialize(0);
    Assert.AreEqual(1, LWorkers.Count, 'Initialize(0)');
  finally
    LWorkers.Free;
  end;
end;

// An explicit minimum above the default maximum wins - say 4 workers asked for on a 2-core computer.
procedure TFileScanWorkersTests.ExplicitMinimumBeatsDefaultMaximum;
begin
  var LMinCount := TFileScanWorkers.CoreCount + 3;
  var LWorkers := TFileScanWorkers.Create;
  try
    LWorkers.Initialize(1, -1, LMinCount);
    Assert.AreEqual(LMinCount, LWorkers.MinCount, 'MinCount');
    Assert.AreEqual(LMinCount, LWorkers.MaxCount, 'MaxCount raised to the explicit MinCount');
    Assert.AreEqual(LMinCount, LWorkers.Count, 'Count');
  finally
    LWorkers.Free;
  end;
end;

procedure TFileScanWorkersTests.InitializePercentageTakesShareOfCores;
begin
  var LCores := TFileScanWorkers.CoreCount;
  var LWorkers := TFileScanWorkers.Create;
  try
    LWorkers.InitializePercentage(50);
    Assert.AreEqual(EnsureRange(Integer(Round(LCores * 50 / 100)), 1, LCores), LWorkers.Count, 'InitializePercentage(50)');

    // Two workers for every three cores, but at least 4 - even with fewer than 4 cores.
    LWorkers.InitializePercentage(66.666, -1, 4);
    Assert.AreEqual(Max(4, Integer(Round(LCores * 66.666 / 100))), LWorkers.Count, 'InitializePercentage(66.666, -1, 4)');
    Assert.AreEqual(4, LWorkers.MinCount, 'InitializePercentage(66.666, -1, 4): MinCount');
  finally
    LWorkers.Free;
  end;
end;

procedure TFileScanWorkersTests.PercentageOverHundredAllowsMoreWorkersThanCores;
begin
  var LCores := TFileScanWorkers.CoreCount;
  var LWorkers := TFileScanWorkers.Create;
  try
    LWorkers.InitializePercentage(200, 4 * LCores);
    Assert.AreEqual(2 * LCores, LWorkers.Count, 'InitializePercentage(200, 4 * CoreCount)');

    // Without a higher maximum, the default one (CoreCount) still holds.
    LWorkers.InitializePercentage(200);
    Assert.AreEqual(LCores, LWorkers.Count, 'InitializePercentage(200)');
  finally
    LWorkers.Free;
  end;
end;

procedure TFileScanWorkersTests.InvalidArgumentsRaise;
begin
  var LWorkers := TFileScanWorkers.Create;
  try
    Assert.WillRaise(procedure begin LWorkers.Initialize(4, 0) end, EArgumentOutOfRangeException, 'AMaxCount 0');
    Assert.WillRaise(procedure begin LWorkers.Initialize(4, -1, -5) end, EArgumentOutOfRangeException, 'AMinCount -5');
    Assert.WillRaise(procedure begin LWorkers.Initialize(4, 2, 6) end, EArgumentException,
      'explicit AMinCount above explicit AMaxCount');
    Assert.WillRaise(procedure begin LWorkers.InitializePercentage(0) end, EArgumentOutOfRangeException, '0 %');
    Assert.WillRaise(procedure begin LWorkers.InitializePercentage(NaN) end, EArgumentOutOfRangeException, 'NaN %');
    Assert.WillRaise(procedure begin LWorkers.MaxCount := 0 end, EArgumentOutOfRangeException, 'MaxCount 0');
    Assert.WillRaise(procedure begin LWorkers.MinCount := 0 end, EArgumentOutOfRangeException, 'MinCount 0');
  finally
    LWorkers.Free;
  end;
end;

procedure TFileScanWorkersTests.SettingOneLimitPushesTheOther;
begin
  var LWorkers := TFileScanWorkers.Create;
  try
    LWorkers.Initialize(6, 6, 1);

    LWorkers.MinCount := 8;
    Assert.AreEqual(8, LWorkers.MaxCount, 'MinCount 8 raises MaxCount');
    Assert.AreEqual(8, LWorkers.Count, 'and Count follows');

    LWorkers.MaxCount := 3;
    Assert.AreEqual(3, LWorkers.MinCount, 'MaxCount 3 lowers MinCount');
    Assert.AreEqual(3, LWorkers.Count, 'and Count follows');
  finally
    LWorkers.Free;
  end;
end;

procedure TFileScanWorkersTests.ScannerOwnsAndFreesWorkers;
var
  LDestroyed: Boolean;
begin
  var LWorkers := TTrackedWorkers.Create(@LDestroyed);
  var LScanner := TParallelFileScanner.Create(['*.pas'], LWorkers);
  try
    Assert.AreSame(LWorkers, LScanner.Workers, 'Workers is the instance given to Create');
    Assert.IsFalse(LDestroyed, 'freed too early');
  finally
    LScanner.Free;
  end;

  Assert.IsTrue(LDestroyed, 'The scanner must free its Workers');

  LScanner := TParallelFileScanner.Create(['*.pas'], nil);
  try
    Assert.IsNull(LScanner.Workers, 'Workers without one given to Create');
  finally
    LScanner.Free;
  end;
end;

procedure TFileScanWorkersTests.ScannerFreesWorkersWhenCreateFails;
var
  LDestroyed: Boolean;
begin
  var LWorkers := TTrackedWorkers.Create(@LDestroyed);

  Assert.WillRaise(
    procedure
    begin
      TFailingScanner.Create(['*.pas'], LWorkers);
    end,
    EAbort, 'TFailingScanner.Create');

  Assert.IsTrue(LDestroyed, 'The scanner must free its Workers when its constructor fails');
end;

end.
