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
    [Test] procedure AppendingKeepsEarlierItemsAndSortsAll;
    [Test] procedure UnsortedListHoldsEveryFile;
    [Test] procedure StringListRootsOverloadMatchesBaseline;
    [Test] procedure NoRootsFindNothing;
  end;
{$IFEND}

implementation

{$IF DEFINED(USE_SPRING4D)}
uses
  System.Classes, System.SysUtils, Spring.Collections, DPFSUnit.Parallel.FileScanner,
  DPFSUnit.Parallel.FileScanner.Spring;

{ TParallelFileScannerSpringTests }

function TParallelFileScannerSpringTests.ScanToList: TArray<string>;
var
  LExclusions: TFileScanExclusions;
  LList: IList<string>;
  LScanner: TParallelFileScannerSpring;
begin
  LScanner := TParallelFileScannerSpring.Create(TScanTree.Extensions, nil);
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

// Items already in the list stay, and the whole list ends up in CompareText order (the full-sort path).
procedure TParallelFileScannerSpringTests.AppendingKeepsEarlierItemsAndSortsAll;
var
  LExclusions: TFileScanExclusions;
  LList: IList<string>;
  LScanner: TParallelFileScannerSpring;
begin
  LScanner := TParallelFileScannerSpring.Create(TScanTree.Extensions, nil);
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LList := TCollections.CreateList<string>;
    LList.Add('ZZZ earlier item');
    LList.Add('AAA earlier item');

    Assert.IsTrue(LScanner.GetFileList(FScanRoots, LExclusions, LList), 'GetFileList found nothing');

    Assert.AreEqual(FBaseline.Count + 2, LList.Count, 'Earlier items plus the files');
    Assert.AreEqual('AAA earlier item', LList.First, 'First item');
    Assert.AreEqual('ZZZ earlier item', LList.Last, 'Last item');

    for var LIndex := 1 to LList.Count - 1 do
      Assert.IsTrue(CompareText(LList[LIndex - 1], LList[LIndex]) <= 0,
        Format('Items %d and %d out of CompareText order', [LIndex - 1, LIndex]));

    LList.Delete(LList.Count - 1);
    LList.Delete(0);
    AssertSameFiles(FBaseline, LList.ToArray, 'GetFileList into a non-empty IList<string>');
  finally
    LScanner.Free;
  end;
end;

procedure TParallelFileScannerSpringTests.UnsortedListHoldsEveryFile;
var
  LExclusions: TFileScanExclusions;
  LList: IList<string>;
  LScanner: TParallelFileScannerSpring;
begin
  LScanner := TParallelFileScannerSpring.Create(TScanTree.Extensions, nil, False);
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LList := TCollections.CreateList<string>;

    Assert.IsTrue(LScanner.GetFileList(FScanRoots, LExclusions, LList), 'GetFileList found nothing');
    AssertSameFiles(FBaseline, LList.ToArray, 'GetFileList (IList<string>), SortResultList False');
  finally
    LScanner.Free;
  end;
end;

procedure TParallelFileScannerSpringTests.StringListRootsOverloadMatchesBaseline;
var
  LExclusions: TFileScanExclusions;
  LList: IList<string>;
  LRoots: TStringList;
  LScanner: TParallelFileScannerSpring;
begin
  LScanner := TParallelFileScannerSpring.Create(TScanTree.Extensions, nil);
  LRoots := TStringList.Create;
  try
    LScanner.ConvertRelativePathsToAbsolute := True;
    LRoots.AddStrings(FScanRoots);
    LList := TCollections.CreateList<string>;

    Assert.IsTrue(LScanner.GetFileList(LRoots, LExclusions, LList), 'GetFileList found nothing');
    AssertSameFiles(FBaseline, LList.ToArray, 'GetFileList (IList<string>) with TStringList roots');
  finally
    LRoots.Free;
    LScanner.Free;
  end;
end;

procedure TParallelFileScannerSpringTests.NoRootsFindNothing;
var
  LExclusions: TFileScanExclusions;
  LList: IList<string>;
  LScanner: TParallelFileScannerSpring;
begin
  LScanner := TParallelFileScannerSpring.Create(TScanTree.Extensions, nil);
  try
    LList := TCollections.CreateList<string>;

    Assert.IsFalse(LScanner.GetFileList([FScanRoots[0] + '\NoSuchFolder'], LExclusions, LList), 'GetFileList result');
    Assert.AreEqual(0, LList.Count, 'Files');
  finally
    LScanner.Free;
  end;
end;
{$IFEND}

end.
