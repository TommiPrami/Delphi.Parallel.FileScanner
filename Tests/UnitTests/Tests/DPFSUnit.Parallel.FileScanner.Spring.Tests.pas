unit DPFSUnit.Parallel.FileScanner.Spring.Tests;

interface

{$INCLUDE ..\..\..\Source\Units\DPFSUnit.Parallel.FileScanner.inc}

{$IF DEFINED(USE_SPRING4D)}
uses
  DUnitX.TestFramework, DPFSUnit.Parallel.FileScanner.Tests.Common;

type
  // TParallelFileScannerSpring is TParallelFileScanner with IList<string> results, so only those are tested here;
  // the walk itself is covered by TParallelFileScannerTests.
  [TestFixture]
  TParallelFileScannerSpringTests = class(TObject)
  strict private
    FBaseline: TFileSet;
    FScanRoots: TArray<string>;
    function ScanToList: TArray<string>;
  public
    [SetupFixture] procedure SetupFixture;
    [TearDownFixture] procedure TearDownFixture;

    [Test] procedure ListMatchesBaseline;
    [Test] procedure SortedListIsInCompareTextOrder;
  end;
{$IFEND}

implementation

{$IF DEFINED(USE_SPRING4D)}
uses
  System.SysUtils, Spring.Collections, DPFSUnit.Parallel.FileScanner, DPFSUnit.Parallel.FileScanner.Spring;

{ TParallelFileScannerSpringTests }

function TParallelFileScannerSpringTests.ScanToList: TArray<string>;
var
  LExclusions: TFileScanExclusions;
  LList: IList<string>;
  LScanner: TParallelFileScannerSpring;
begin
  LScanner := TParallelFileScannerSpring.Create(TScanTree.Extensions);
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LList := TCollections.CreateList<string>;

    Assert.IsTrue(LScanner.GetFileList(FScanRoots, LExclusions, LList), 'GetFileList found nothing');

    Result := LList.ToArray;
  finally
    LScanner.Free;
  end;
end;

procedure TParallelFileScannerSpringTests.SetupFixture;
begin
  FScanRoots := [TScanTree.SourceRoot];
  FBaseline := TScanTree.BaselineFiles(FScanRoots);
end;

procedure TParallelFileScannerSpringTests.TearDownFixture;
begin
  FreeAndNil(FBaseline);
end;

procedure TParallelFileScannerSpringTests.ListMatchesBaseline;
begin
  AssertSameFiles(FBaseline, ScanToList, 'GetFileList (IList<string>)');
end;

procedure TParallelFileScannerSpringTests.SortedListIsInCompareTextOrder;
var
  LDisorders: Integer;
  LFiles: TArray<string>;
begin
  LFiles := ScanToList;
  LDisorders := 0;

  for var LIndex := 1 to High(LFiles) do
    if CompareText(LFiles[LIndex - 1], LFiles[LIndex]) > 0 then
      Inc(LDisorders);

  Assert.IsTrue(Length(LFiles) > 1, 'The scan must find files to sort');
  Assert.AreEqual(0, LDisorders, 'Neighbours out of CompareText order');
end;
{$IFEND}

end.
