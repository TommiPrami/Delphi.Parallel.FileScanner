unit Delphi.WildCardMatcher.DUnitX;

interface

uses
  DUnitX.TestFramework,
  Delphi.WildCardMatcher;

type
  [TestFixture]
  TWildCardMatcherDUnitX = class
  public
    { Spec example 1: '*' }
    [Test]
    procedure AsteriskMatchesAnyNumberOfCharsTest;
    [Test]
    procedure AsteriskMatchesEmptyTest;
    [Test]
    procedure AsteriskMustBeAtStartToMatchPrefix_SpecExampleTest;
    [Test]
    procedure LeadingAsteriskMatchesAnywhereTest;
    [Test]
    procedure SurroundingAsterisksMatchSubstringTest;
    [Test]
    procedure ConsecutiveAsterisksCollapseTest;

    { Spec example 2: '?' }
    [Test]
    procedure QuestionMarkMatchesSingleCharTest;
    [Test]
    procedure QuestionMarkRequiresACharacterTest;

    { Spec example 3: '<abc>' }
    [Test]
    procedure CharClassPositiveSetTest;

    { Spec example 4: '<!abc>' }
    [Test]
    procedure CharClassNegatedSetTest;
    [Test]
    procedure NegatedSetAtStartSpecExampleTest;

    { Spec example 5: '<a-c>' }
    [Test]
    procedure CharClassRangeTest;
    [Test]
    procedure CombinedRangesTest;
    [Test]
    procedure NegatedRangeTest;

    { Spec example 6: digit runs '<#>', '<###>' }
    [Test]
    procedure HashMatchesSingleDigitTest;
    [Test]
    procedure HashRejectsNonDigitTest;
    [Test]
    procedure DigitRunMatchesSeveralDigitsTest;
    [Test]
    procedure DigitRunEdgeCasesTest;
    [Test]
    procedure SequenceMixesDigitsAndAnyCharTest;
    [Test]
    procedure SequenceEquivalentFormsTest;

    { Edge cases }
    [Test]
    procedure EmptyPatternMatchesOnlyEmptyInputTest;
    [Test]
    procedure EmptyInputMatchesOnlyEmptyOrAsteriskTest;
    [Test]
    procedure ExactLiteralMatchTest;
    [Test]
    procedure LiteralCaseInsensitiveByDefaultTest;
    [Test]
    procedure CaseSensitiveFlagTest;
    [Test]
    procedure UnterminatedClassReturnsFalseTest;
    [Test]
    procedure EmptyClassReturnsFalseTest;
    [Test]
    procedure LiteralCloseBracketAsFirstClassCharTest;
    [Test]
    procedure DashAsLiteralAtStartOrEndOfClassTest;

    { Quoted-string alternation: '<"foo"|"bar">' }
    [Test]
    procedure QuotedAltSingleAlternativeMatchesLiteralTest;
    [Test]
    procedure QuotedAltMultipleAlternativesAnyMatchesTest;
    [Test]
    procedure QuotedAltNoneOfTheAlternativesMatchesFailsTest;
    [Test]
    procedure QuotedAltCaseInsensitiveByDefaultTest;
    [Test]
    procedure QuotedAltCaseSensitiveFlagTest;
    [Test]
    procedure QuotedAltCombinedWithAsteriskTest;
    [Test]
    procedure QuotedAltReadmeExamplePatternTest;
    [Test]
    procedure QuotedAltAlternativesWithDifferentLengthsBacktrackTest;
    [Test]
    procedure QuotedAltEmptyStringAlternativeMatchesZeroCharsTest;
    [Test]
    procedure QuotedAltNegatedSameLengthAlternativesTest;
    [Test]
    procedure QuotedAltNegatedConsumesLongestAlternativeLengthTest;
    [Test]
    procedure QuotedAltNegatedRequiresEnoughInputTest;
    [Test]
    procedure QuotedAltMalformedReturnsFalseTest;
    [Test]
    procedure QuotedAltCloseBracketInsideQuotedAlternativeIsLiteralTest;
    [Test]
    procedure PipeOutsideClassIsLiteralTest;

    { Combinations }
    [Test]
    procedure ComplexFileNameLikePatternsTest;
    [Test]
    procedure MixedSpecialCharsInClassTest;
    [Test]
    procedure ManyAsterisksDoNotExplodeTest;

    { Multi-pattern overloads }
    [Test]
    procedure MatchTArrayReturnsTrueWhenAnyMatchesTest;
    [Test]
    procedure MatchTArrayReturnsFalseWhenNoneMatchTest;
    [Test]
    procedure MatchTArrayEmptyReturnsFalseTest;
    [Test]
    procedure MatchTArrayShortCircuitsOnFirstMatchTest;
    [Test]
    procedure MatchTArrayHonoursCaseSensitiveFlagTest;
    [Test]
    procedure MatchTStringsReturnsTrueWhenAnyMatchesTest;
    [Test]
    procedure MatchTStringsReturnsFalseWhenNoneMatchTest;
    [Test]
    procedure MatchTStringsEmptyReturnsFalseTest;
    [Test]
    procedure MatchTStringsHonoursCaseSensitiveFlagTest;

    { Non-ASCII character handling }
    [Test]
    procedure NonAsciiFinnishCaseInsensitiveTest;
    [Test]
    procedure NonAsciiFinnishCaseSensitiveTest;
    [Test]
    procedure NonAsciiFinnishInsideCharClassTest;
    [Test]
    procedure NonAsciiFinnishInsidePatternBacktrackingTest;
    [Test]
    procedure NonAsciiGermanSpanishFrenchTest;
    [Test]
    procedure NonAsciiCyrillicAndGreekTest;    [Test]
    procedure NonAsciiMixedAsciiAndUnicodeTest;

    { Instance API: pre-registered patterns }
    [Test]
    procedure InstanceDefaultConstructorHasNoRegisteredPatternsTest;
    [Test]
    procedure InstanceCreateWithSinglePatternRegistersItTest;
    [Test]
    procedure InstanceCreateWithTArrayRegistersAllTest;
    [Test]
    procedure InstanceCreateWithTStringsRegistersAllTest;
    [Test]
    procedure InstanceMatchNoArgsUsesRegisteredPatternsTest;
    [Test]
    procedure InstanceMatchNoArgsEmptySetReturnsFalseTest;
    [Test]
    procedure InstanceMatchWithPatternIgnoresRegisteredByDefaultTest;
    [Test]
    procedure InstanceMatchWithPatternAndAlsoMatchRegisteredTest;
    [Test]
    procedure InstanceMatchWithTArrayPatternsTest;
    [Test]
    procedure InstanceMatchWithTStringsPatternsAndAlsoMatchRegisteredTest;
    [Test]
    procedure InstanceMatchCaseSensitiveWithAlsoMatchRegisteredTest;
    [Test]
    procedure InstanceMatchEmptyRegisteredSetWithAlsoMatchRegisteredTrueTest;
    [Test]
    procedure InstanceMatchRegisteredHitButAdHocMissWithAlsoMatchRegisteredTest;
    [Test]
    procedure InstanceCaseSensitivityIsLockedAtCreateTest;
    [Test]
    procedure InstanceMatchWorksOnDefaultInitialisedRecordTest;

    { Registered patterns run the COMPILED engine - edge-case coverage.
      Ad-hoc Match(input, pattern) runs the interpreting engine; these
      tests pin the compiled path to the same semantics. }
    [Test]
    procedure CompiledCharClassEdgeCasesTest;
    [Test]
    procedure CompiledQuotedAltTest;
    [Test]
    procedure CompiledStarFastPathsTest;
    [Test]
    procedure CompiledDigitAndQuestionTest;
    [Test]
    procedure CompiledCaseSensitiveTest;
    [Test]
    procedure CompiledEmptyAndMalformedPatternTest;

    { TWildCardFilter: include / exclude wrapper }
    [Test]
    procedure FilterEmptyAcceptsEverythingTest;
    [Test]
    procedure FilterIncludeOnlyTest;
    [Test]
    procedure FilterExcludeOnlyTest;
    [Test]
    procedure FilterExcludeWinsOverIncludeTest;
    [Test]
    procedure FilterSinglePatternOverloadTest;
    [Test]
    procedure FilterTStringsOverloadWithNilTest;
    [Test]
    procedure FilterCaseSensitivityTest;
    [Test]
    procedure FilterQuotedAltPatternsTest;
    [Test]
    procedure FilterDefaultInitialisedRecordAcceptsEverythingTest;

    { ValidatePattern }
    [Test]
    procedure ValidatePatternAcceptsGoodPatternsTest;
    [Test]
    procedure ValidatePatternRejectsMalformedPatternsTest;

    { Match attribution: MatchIndex / AcceptsEx }
    [Test]
    procedure MatchIndexTest;
    [Test]
    procedure FilterAcceptsExTest;

    { Path mode (wcoPathMode) }
    [Test]
    procedure PathModeSegmentStarTest;
    [Test]
    procedure PathModeGlobstarTest;
    [Test]
    procedure PathModeQuestionMarkTest;
    [Test]
    procedure PathModeTrailingSegmentStarTest;
    [Test]
    procedure PathModeCaseSensitiveComboTest;
    [Test]
    procedure PathModeAdhocMatchesRegisteredTest;
    [Test]
    procedure PathModeOffIsDefaultBehaviourTest;

    { Differential fuzzing: registered vs ad-hoc engine, all modes }
    [Test]
    procedure DifferentialFuzzTest;

    { Lint: compiler-style warnings and hints }
    [Test]
    procedure LintPatternMalformedTest;
    [Test]
    procedure LintPatternStarRunTest;
    [Test]
    procedure LintPatternClassAndAlternationTest;
    [Test]
    procedure LintPatternEmptyTest;
    [Test]
    procedure LintSetDuplicatesAndShadowingTest;
    [Test]
    procedure FilterLintIncludeAlsoExcludedTest;
    [Test]
    procedure FilterLintMatchAllTest;
    [Test]
    procedure FilterLintCleanFilterTest;
    [Test]
    procedure LintPreAngleBracketSyntaxTest;

    { IsWildCard and plain paths }
    [Test]
    procedure IsWildCardTest;
    [Test]
    procedure IsWildCardFalseMeansPatternMatchesItselfTest;

    { API additions }
    [Test]
    procedure PatternMatchesEverythingTest;
    [Test]
    procedure PatternCountAndFilterCountsTest;
    [Test]
    procedure FilterBulkFilterTest;

    { Additional matcher coverage }
    [Test]
    procedure FileNameCharactersAreLiteralTest;
    [Test]
    procedure UnicodeNonAsciiSimdCandidateTest;
    [Test]
    procedure LongLiteralPatternsTest;
    [Test]
    procedure QuotedAltUnicodeCaseInsensitiveTest;
    [Test]
    procedure PathModeClassAndQuotedAltSeparatorsTest;
    [Test]
    procedure PathModeMixedSeparatorsTest;
    [Test]
    procedure FilterPathModeOptionsTest;

    { Wildcard combinations - every case checked in both engines }
    [Test]
    procedure StarAndQuestionCombinationsTest;
    [Test]
    procedure StarAndSequenceCombinationsTest;
    [Test]
    procedure StarAndClassCombinationsTest;
    [Test]
    procedure StarAndAlternationCombinationsTest;
    [Test]
    procedure FixedWidthTokenCombinationsTest;
    [Test]
    procedure NegatedFormCombinationsTest;
    [Test]
    procedure CaseSensitivityCombinationsTest;
    [Test]
    procedure PathModeCombinationsTest;
    [Test]
    procedure RealWorldMaskCombinationsTest;
    [Test]
    procedure WildcardsInsideQuotesAreLiteralTest;
    [Test]
    procedure AnchoringCombinationsTest;
    [Test]
    procedure BacktrackingCombinationsTest;
    [Test]
    procedure EmptyInputCombinationsTest;
    [Test]
    procedure ReferenceMatcherFuzzTest;
  end;

implementation

uses
  System.Classes, System.SysUtils;

{ Fuzzing random source - shared by the fuzz and equivalence tests }

function FuzzNextRandom(var ASeed: Cardinal; const ARange: Cardinal): Cardinal;
begin
  // Deterministic LCG (Numerical Recipes constants) - keeps the fuzz run
  // reproducible regardless of the RTL's global Random state.
  ASeed := (ASeed * 1664525) + 1013904223;
  Result := (ASeed shr 16) mod ARange;
end;

{ Spec example 1: '*' }

procedure TWildCardMatcherDUnitX.AsteriskMatchesAnyNumberOfCharsTest;
begin
  // wh* finds what, white, why (from the spec)
  Assert.IsTrue(TWildCard.Create.Match('what',  'wh*'));
  Assert.IsTrue(TWildCard.Create.Match('white', 'wh*'));
  Assert.IsTrue(TWildCard.Create.Match('why',   'wh*'));
end;

procedure TWildCardMatcherDUnitX.AsteriskMatchesEmptyTest;
begin
  // '*' must match zero characters as well
  Assert.IsTrue(TWildCard.Create.Match('',    '*'));
  Assert.IsTrue(TWildCard.Create.Match('wh',  'wh*'), '"wh" should match "wh*" (trailing * = zero chars)');
end;

procedure TWildCardMatcherDUnitX.AsteriskMustBeAtStartToMatchPrefix_SpecExampleTest;
begin
  // From the spec: "wh* finds what, white, and why, but not awhile or watch"
  Assert.IsFalse(TWildCard.Create.Match('awhile', 'wh*'), '"awhile" must NOT match "wh*"');
  Assert.IsFalse(TWildCard.Create.Match('watch',  'wh*'), '"watch" must NOT match "wh*"');
end;

procedure TWildCardMatcherDUnitX.LeadingAsteriskMatchesAnywhereTest;
begin
  Assert.IsTrue(TWildCard.Create.Match('readme.txt', '*.txt'));
  Assert.IsTrue(TWildCard.Create.Match('a.txt',      '*.txt'));
  Assert.IsTrue(TWildCard.Create.Match('.txt',       '*.txt'));
  Assert.IsFalse(TWildCard.Create.Match('readme.doc', '*.txt'));
end;

procedure TWildCardMatcherDUnitX.SurroundingAsterisksMatchSubstringTest;
begin
  Assert.IsTrue(TWildCard.Create.Match('hello world',   '*world*'));
  Assert.IsTrue(TWildCard.Create.Match('worldview',     '*world*'));
  Assert.IsTrue(TWildCard.Create.Match('world',         '*world*'));
  Assert.IsFalse(TWildCard.Create.Match('hello',        '*world*'));
end;

procedure TWildCardMatcherDUnitX.ConsecutiveAsterisksCollapseTest;
begin
  // '**' must behave the same as '*'
  Assert.IsTrue(TWildCard.Create.Match('anything',  '**'));
  Assert.IsTrue(TWildCard.Create.Match('a.b.c.txt', '**.txt'));
  Assert.IsTrue(TWildCard.Create.Match('a.b.c.txt', '***.txt'));
end;

{ Spec example 2: '?' }

procedure TWildCardMatcherDUnitX.QuestionMarkMatchesSingleCharTest;
begin
  // b?ll finds ball, bell, bill (from the spec)
  Assert.IsTrue(TWildCard.Create.Match('ball', 'b?ll'));
  Assert.IsTrue(TWildCard.Create.Match('bell', 'b?ll'));
  Assert.IsTrue(TWildCard.Create.Match('bill', 'b?ll'));
end;

procedure TWildCardMatcherDUnitX.QuestionMarkRequiresACharacterTest;
begin
  // '?' must consume exactly one char - 'bll' has none to consume
  Assert.IsFalse(TWildCard.Create.Match('bll',  'b?ll'), '"bll" has no middle char');
  Assert.IsFalse(TWildCard.Create.Match('baal', 'b?ll'), '"baal" has two middle chars');
end;

{ Spec example 3: '<abc>' }

procedure TWildCardMatcherDUnitX.CharClassPositiveSetTest;
begin
  // b<ae>ll finds ball and bell, but not bill (from the spec)
  Assert.IsTrue (TWildCard.Create.Match('ball', 'b<ae>ll'));
  Assert.IsTrue (TWildCard.Create.Match('bell', 'b<ae>ll'));
  Assert.IsFalse(TWildCard.Create.Match('bill', 'b<ae>ll'));
end;

{ Spec example 4: '<!abc>' }

procedure TWildCardMatcherDUnitX.CharClassNegatedSetTest;
begin
  // b<!ae>ll finds bill and bull, but not ball or bell (from the spec)
  Assert.IsTrue (TWildCard.Create.Match('bill', 'b<!ae>ll'));
  Assert.IsTrue (TWildCard.Create.Match('bull', 'b<!ae>ll'));
  Assert.IsFalse(TWildCard.Create.Match('ball', 'b<!ae>ll'));
  Assert.IsFalse(TWildCard.Create.Match('bell', 'b<!ae>ll'));
end;

procedure TWildCardMatcherDUnitX.NegatedSetAtStartSpecExampleTest;
begin
  // From the spec: "<!a>*" finds all items that do not begin with 'a'.
  Assert.IsTrue (TWildCard.Create.Match('banana', '<!a>*'));
  Assert.IsTrue (TWildCard.Create.Match('zebra',  '<!a>*'));
  Assert.IsTrue (TWildCard.Create.Match('x',      '<!a>*'));
  Assert.IsFalse(TWildCard.Create.Match('apple',  '<!a>*'));
  Assert.IsFalse(TWildCard.Create.Match('a',      '<!a>*'));
end;

{ Spec example 5: '<a-c>' }

procedure TWildCardMatcherDUnitX.CharClassRangeTest;
begin
  // b<a-c>d finds bad, bbd, bcd (from the spec)
  Assert.IsTrue (TWildCard.Create.Match('bad', 'b<a-c>d'));
  Assert.IsTrue (TWildCard.Create.Match('bbd', 'b<a-c>d'));
  Assert.IsTrue (TWildCard.Create.Match('bcd', 'b<a-c>d'));
  Assert.IsFalse(TWildCard.Create.Match('bdd', 'b<a-c>d'));
  Assert.IsFalse(TWildCard.Create.Match('b0d', 'b<a-c>d'));
end;

procedure TWildCardMatcherDUnitX.CombinedRangesTest;
begin
  // Multiple ranges in one class
  Assert.IsTrue (TWildCard.Create.Match('X', '<a-zA-Z>'));
  Assert.IsTrue (TWildCard.Create.Match('x', '<a-zA-Z>'));
  Assert.IsFalse(TWildCard.Create.Match('5', '<a-zA-Z>'));

  // Range + literals + digits
  Assert.IsTrue (TWildCard.Create.Match('q',  '<a-cq0-9>'));
  Assert.IsTrue (TWildCard.Create.Match('b',  '<a-cq0-9>'));
  Assert.IsTrue (TWildCard.Create.Match('7',  '<a-cq0-9>'));
  Assert.IsFalse(TWildCard.Create.Match('z',  '<a-cq0-9>'));
end;

procedure TWildCardMatcherDUnitX.NegatedRangeTest;
begin
  Assert.IsTrue (TWildCard.Create.Match('1', '<!a-z>'));
  Assert.IsTrue (TWildCard.Create.Match('!', '<!a-z>'));
  Assert.IsFalse(TWildCard.Create.Match('m', '<!a-z>'));
end;

{ Spec example 6: digit runs '<#>', '<###>' }

procedure TWildCardMatcherDUnitX.HashMatchesSingleDigitTest;
begin
  // 1<#>3 finds 103, 113, 123 (from the spec)
  Assert.IsTrue(TWildCard.Create.Match('103', '1<#>3'));
  Assert.IsTrue(TWildCard.Create.Match('113', '1<#>3'));
  Assert.IsTrue(TWildCard.Create.Match('123', '1<#>3'));
  Assert.IsTrue(TWildCard.Create.Match('193', '1<#>3'));
end;

procedure TWildCardMatcherDUnitX.HashRejectsNonDigitTest;
begin
  Assert.IsFalse(TWildCard.Create.Match('1a3', '1<#>3'), '"a" is not a digit');
  Assert.IsFalse(TWildCard.Create.Match('1 3', '1<#>3'), 'space is not a digit');
  Assert.IsFalse(TWildCard.Create.Match('13',  '1<#>3'), '"<#>" must consume exactly one digit');
end;

procedure TWildCardMatcherDUnitX.DigitRunMatchesSeveralDigitsTest;
begin
  // '<###>' is shorthand for three digits.
  Assert.IsTrue (TWildCard.Create.Match('Test_001.log', 'Test_<###>.log'));
  Assert.IsTrue (TWildCard.Create('Test_<###>.log').Match('Test_999.log'), 'registered engine');
  Assert.IsFalse(TWildCard.Create.Match('Test_01.log',   'Test_<###>.log'), 'too few digits');
  Assert.IsFalse(TWildCard.Create.Match('Test_0001.log', 'Test_<###>.log'), 'too many digits');
  Assert.IsFalse(TWildCard.Create.Match('Test_0A1.log',  'Test_<###>.log'), 'non-digit in the middle');

  // Runs compose with each other and with '*'.
  Assert.IsTrue (TWildCard.Create.Match('2026-10-13.log', '<####>-<##>-<##>.log'));
  Assert.IsTrue (TWildCard.Create('*_<##>.txt').Match('report_v2_07.txt'));
  Assert.IsFalse(TWildCard.Create('*_<##>.txt').Match('report_v2_7.txt'));
end;

procedure TWildCardMatcherDUnitX.DigitRunEdgeCasesTest;
begin
  // A bare '#' is an ordinary literal character now.
  Assert.IsTrue (TWildCard.Create.Match('#', '#'));
  Assert.IsFalse(TWildCard.Create.Match('5', '#'), 'bare # is a literal, not a digit');

  // '#' mixed with other members is a plain class member.
  Assert.IsTrue (TWildCard.Create.Match('#', '<#a>'));
  Assert.IsTrue (TWildCard.Create.Match('a', '<#a>'));
  Assert.IsFalse(TWildCard.Create.Match('5', '<#a>'), '<#a> is the set {#, a}, not a digit run');

  // Negation is a class, not a negated digit run: '<!#>' = anything but '#'.
  Assert.IsTrue (TWildCard.Create.Match('5', '<!#>'));
  Assert.IsFalse(TWildCard.Create.Match('#', '<!#>'));

  // Leading '>' is a literal member, so '<>#>' is the set {>, #}.
  Assert.IsTrue (TWildCard.Create.Match('>', '<>#>'));
  Assert.IsTrue (TWildCard.Create.Match('#', '<>#>'));
  Assert.IsFalse(TWildCard.Create.Match('5', '<>#>'));

  // Unterminated digit run is a malformed class - never matches.
  Assert.IsFalse(TWildCard.Create.Match('12', '<##'));
  Assert.IsFalse(TWildCard.Create('<##').Match('12'));
end;

procedure TWildCardMatcherDUnitX.SequenceMixesDigitsAndAnyCharTest;
var
  LError: string;
begin
  // '<?##>': any one character, then two digits.
  Assert.IsTrue (TWildCard.Create.Match('x12.txt', '<?##>.txt'));
  Assert.IsTrue (TWildCard.Create('<?##>.txt').Match('912.txt'), 'registered engine');
  Assert.IsFalse(TWildCard.Create.Match('12.txt',  '<?##>.txt'), '? needs a character of its own');
  Assert.IsFalse(TWildCard.Create.Match('x1a.txt', '<?##>.txt'), 'a is not a digit');
  Assert.IsFalse(TWildCard.Create('<?##>.txt').Match('xy12.txt'));

  // Order matters: '<#?#>' is digit, any, digit.
  Assert.IsTrue (TWildCard.Create.Match('1x2', '<#?#>'));
  Assert.IsFalse(TWildCard.Create.Match('x12', '<#?#>'));
  Assert.IsTrue (TWildCard.Create('<#?#>').Match('1-2'));

  // '<?>' alone is the same as '?'.
  Assert.IsTrue (TWildCard.Create.Match('a', '<?>'));
  Assert.IsFalse(TWildCard.Create.Match('',  '<?>'));
  Assert.IsFalse(TWildCard.Create('<?>').Match('ab'));

  // Path mode: a '?' in a sequence does not match a separator, like a bare '?'.
  Assert.IsFalse(TWildCard.Create([wcoPathMode]).Match('a\12', 'a<?##>'));
  Assert.IsFalse(TWildCard.Create('a<?##>', [wcoPathMode]).Match('a\12'));
  Assert.IsTrue (TWildCard.Create.Match('a\12', 'a<?##>'), 'without path mode ? matches the separator');

  // '?' mixed with other members, or after '!', is a plain class.
  Assert.IsTrue (TWildCard.Create.Match('a', '<?a>'));
  Assert.IsFalse(TWildCard.Create.Match('b', '<?a>'), '<?a> is the set {?, a}');
  Assert.IsTrue (TWildCard.Create.Match('b', '<!?>'));
  Assert.IsFalse(TWildCard.Create.Match('?', '<!?>'));

  Assert.IsTrue(TWildCard.ValidatePattern('<?##>.txt', LError), LError);
end;

procedure TWildCardMatcherDUnitX.SequenceEquivalentFormsTest;
const
  SEED = 20261014;
  ITERATIONS = 2000;
  // Digit-heavy fragments so the masks get real hits, not just agreement
  // on "no match".
  INPUT_FRAGMENTS: array[0..9] of string = ('A', 'x', '1', '2', '7', '\', '.t', '#', '?', 'A1');
  MODE_OPTIONS: array[0..3] of TWildCardOptions = ([], [wcoCaseSensitive], [wcoPathMode], [wcoCaseSensitive, wcoPathMode]);
  // Each row lists spellings of the same pattern; all must agree on
  // every input, in both engines and every mode.
  EQUIVALENT_FORMS: array[0..5] of array[0..3] of string = (
    ('A*<##>.txt',    'A*<#><#>.txt',   'A*<#><#>.txt',  'A*<#><#>.txt'),
    ('<?##>.txt',     '<?><##>.txt',    '?<##>.txt',     '?<#><#>.txt'),
    ('*<#?#>*',       '*<#>?<#>*',      '*<#><?><#>*',   '*<#>?<#>*'),
    ('*<??>*',        '*??*',           '*<?><?>*',      '*<?>?*'),
    ('*A<###>*',      '*A<#><#><#>*',   '*A<##><#>*',    '*A<#><##>*'),
    ('**<?#>\*',      '**?<#>\*',       '**<?><#>\*',    '**?<#>\*'));
var
  LSeed: Cardinal;
  LIteration, LIndex, LLen, LMode, LRow, LForm: Integer;
  LInput: string;
  LExpected: Boolean;
  LHits: array[0..High(EQUIVALENT_FORMS)] of Integer;
  LMatchers: array[0..High(EQUIVALENT_FORMS), 0..3, 0..High(MODE_OPTIONS)] of TWildCard;
begin
  FillChar(LHits, SizeOf(LHits), 0);

  // The user-facing promise: '<##>' and '<#><#>' (and '<?##>' and
  // '<?><##>') are the same pattern - '<...>' only marks the region.
  Assert.AreEqual(TWildCard.Create.Match('A12.txt', 'A*<##>.txt'), TWildCard.Create.Match('A12.txt', 'A*<#><#>.txt'));
  Assert.IsTrue (TWildCard.Create.Match('Abc12.txt', 'A*<##>.txt'));
  Assert.IsTrue (TWildCard.Create.Match('Abc12.txt', 'A*<#><#>.txt'));
  Assert.IsTrue (TWildCard.Create.Match('x12.txt', '<?##>.txt'));
  Assert.IsTrue (TWildCard.Create.Match('x12.txt', '<?><##>.txt'));

  for LRow := 0 to High(EQUIVALENT_FORMS) do
    for LForm := 0 to 3 do
      for LMode := 0 to High(MODE_OPTIONS) do
        LMatchers[LRow, LForm, LMode] := TWildCard.Create(EQUIVALENT_FORMS[LRow, LForm], MODE_OPTIONS[LMode]);

  LSeed := SEED;

  for LIteration := 1 to ITERATIONS do
  begin
    LLen := FuzzNextRandom(LSeed, 6);
    LInput := '';

    for LIndex := 1 to LLen do
      LInput := LInput + INPUT_FRAGMENTS[FuzzNextRandom(LSeed, Length(INPUT_FRAGMENTS))];

    if FuzzNextRandom(LSeed, 2) = 0 then
      LInput := LInput + '.txt';

    for LRow := 0 to High(EQUIVALENT_FORMS) do
      for LMode := 0 to High(MODE_OPTIONS) do
      begin
        LExpected := TWildCard.Create(MODE_OPTIONS[LMode]).Match(LInput, EQUIVALENT_FORMS[LRow, 0]);

        if LExpected then
          Inc(LHits[LRow]);

        for LForm := 0 to 3 do
        begin
          Assert.AreEqual(LExpected, TWildCard.Create(MODE_OPTIONS[LMode]).Match(LInput, EQUIVALENT_FORMS[LRow, LForm]),
            Format('ad-hoc: <%s> vs <%s> disagree on input <%s> (mode %d)',
            [EQUIVALENT_FORMS[LRow, 0], EQUIVALENT_FORMS[LRow, LForm], LInput, LMode]));
          Assert.AreEqual(LExpected, LMatchers[LRow, LForm, LMode].Match(LInput),
            Format('registered: <%s> vs <%s> disagree on input <%s> (mode %d)',
            [EQUIVALENT_FORMS[LRow, 0], EQUIVALENT_FORMS[LRow, LForm], LInput, LMode]));
        end;
      end;
  end;

  // Agreeing on "no match" everywhere would prove nothing.
  for LRow := 0 to High(EQUIVALENT_FORMS) do
    Assert.IsTrue(LHits[LRow] >= 20, Format('<%s> matched only %d random inputs', [EQUIVALENT_FORMS[LRow, 0], LHits[LRow]]));
end;

{ Edge cases }

procedure TWildCardMatcherDUnitX.EmptyPatternMatchesOnlyEmptyInputTest;
begin
  Assert.IsTrue (TWildCard.Create.Match('', ''));
  Assert.IsFalse(TWildCard.Create.Match('x', ''));
end;

procedure TWildCardMatcherDUnitX.EmptyInputMatchesOnlyEmptyOrAsteriskTest;
begin
  Assert.IsTrue (TWildCard.Create.Match('', ''));
  Assert.IsTrue (TWildCard.Create.Match('', '*'));
  Assert.IsTrue (TWildCard.Create.Match('', '***'));
  Assert.IsFalse(TWildCard.Create.Match('', '?'));
  Assert.IsFalse(TWildCard.Create.Match('', '<#>'));
  Assert.IsFalse(TWildCard.Create.Match('', 'a'));
  Assert.IsFalse(TWildCard.Create.Match('', '<a-z>'));
end;

procedure TWildCardMatcherDUnitX.ExactLiteralMatchTest;
begin
  Assert.IsTrue (TWildCard.Create.Match('readme.txt', 'readme.txt'));
  Assert.IsFalse(TWildCard.Create.Match('readme.txt', 'readme.doc'));
  Assert.IsFalse(TWildCard.Create.Match('readme.txt', 'readme.tx'),  'shorter pattern must not match');
  Assert.IsFalse(TWildCard.Create.Match('readme.tx',  'readme.txt'), 'shorter input must not match');
end;

procedure TWildCardMatcherDUnitX.LiteralCaseInsensitiveByDefaultTest;
begin
  // Default is case-insensitive (Windows convention)
  Assert.IsTrue(TWildCard.Create.Match('README.TXT', 'readme.txt'));
  Assert.IsTrue(TWildCard.Create.Match('ReadMe.Txt', '*.TXT'));
  Assert.IsTrue(TWildCard.Create.Match('FOO', '<a-z>??'));
end;

procedure TWildCardMatcherDUnitX.CaseSensitiveFlagTest;
begin
  Assert.IsFalse(TWildCard.Create(True).Match('README.TXT', 'readme.txt'));
  Assert.IsTrue (TWildCard.Create(True).Match('readme.txt', 'readme.txt'));
  Assert.IsFalse(TWildCard.Create(True).Match('FOO',        '<a-z>??'));
  Assert.IsTrue (TWildCard.Create(True).Match('foo',        '<a-z>??'));
end;

procedure TWildCardMatcherDUnitX.UnterminatedClassReturnsFalseTest;
begin
  // Patterns with an unterminated '<' are malformed - never match
  Assert.IsFalse(TWildCard.Create.Match('a',  '<abc'));
  Assert.IsFalse(TWildCard.Create.Match('ab', 'a<bc'));
end;

procedure TWildCardMatcherDUnitX.EmptyClassReturnsFalseTest;
begin
  // '<>' is treated as an unterminated class with '>' as literal content;
  // the next '>' would close it. Bare '<>' on its own is malformed.
  Assert.IsFalse(TWildCard.Create.Match('a', '<>'));
end;

procedure TWildCardMatcherDUnitX.LiteralCloseBracketAsFirstClassCharTest;
begin
  // First content char '>' is a literal - '<>abc>' matches '>', 'a', 'b' or 'c'
  Assert.IsTrue (TWildCard.Create.Match('>', '<>abc>'));
  Assert.IsTrue (TWildCard.Create.Match('a', '<>abc>'));
  Assert.IsTrue (TWildCard.Create.Match('c', '<>abc>'));
  Assert.IsFalse(TWildCard.Create.Match('x', '<>abc>'));
end;

procedure TWildCardMatcherDUnitX.DashAsLiteralAtStartOrEndOfClassTest;
begin
  // '-' at the start/end of a class is a literal, not part of a range
  Assert.IsTrue (TWildCard.Create.Match('-', '<-x>'));
  Assert.IsTrue (TWildCard.Create.Match('x', '<-x>'));
  Assert.IsTrue (TWildCard.Create.Match('-', '<a->'));
  Assert.IsTrue (TWildCard.Create.Match('a', '<a->'));
  Assert.IsFalse(TWildCard.Create.Match('b', '<a->'));
end;

{ Combinations }

procedure TWildCardMatcherDUnitX.ComplexFileNameLikePatternsTest;
begin
  // Realistic file-mask style patterns
  Assert.IsTrue (TWildCard.Create.Match('Foo1.pas',  '*.pas'));
  Assert.IsTrue (TWildCard.Create.Match('Foo1.pas',  'Foo?.pas'));
  Assert.IsTrue (TWildCard.Create.Match('Foo7.pas',  'Foo<#>.pas'));
  Assert.IsFalse(TWildCard.Create.Match('FooA.pas',  'Foo<#>.pas'));
  Assert.IsTrue (TWildCard.Create.Match('Test_001.log', 'Test_<###>.log'));
  Assert.IsFalse(TWildCard.Create.Match('Test_00A.log', 'Test_<###>.log'));
end;

procedure TWildCardMatcherDUnitX.MixedSpecialCharsInClassTest;
begin
  // Class combining a literal '_' and '-', followed by a digit run
  Assert.IsTrue (TWildCard.Create.Match('Foo_5.pas', 'Foo<_-><#>.pas'));
  Assert.IsTrue (TWildCard.Create.Match('Foo-5.pas', 'Foo<_-><#>.pas'));
  Assert.IsFalse(TWildCard.Create.Match('Foo.5.pas', 'Foo<_-><#>.pas'));
end;

procedure TWildCardMatcherDUnitX.ManyAsterisksDoNotExplodeTest;
begin
  // Defensive: pathological pattern with many '*' should still terminate
  // quickly thanks to the consecutive-asterisk collapse.
  Assert.IsTrue (TWildCard.Create.Match('abcdefghijklmnop',
    '*a*b*c*d*e*f*g*h*i*j*k*l*m*n*o*p*'));
  Assert.IsFalse(TWildCard.Create.Match('abcdefghijklmnop',
    '*a*b*c*d*e*f*g*h*i*j*k*l*m*n*o*z*'));
end;

{ Quoted-string alternation: '<"foo"|"bar">' }

procedure TWildCardMatcherDUnitX.QuotedAltSingleAlternativeMatchesLiteralTest;
begin
  // A class with a single quoted alternative matches just that literal at
  // the current position, consuming its length.
  Assert.IsTrue (TWildCard.Create.Match('foo',    '<"foo">'));
  Assert.IsTrue (TWildCard.Create.Match('foobar', '<"foo">*'));
  Assert.IsFalse(TWildCard.Create.Match('fo',     '<"foo">'), 'not enough input to span the alt');
  Assert.IsFalse(TWildCard.Create.Match('bar',    '<"foo">'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltMultipleAlternativesAnyMatchesTest;
begin
  // The class succeeds when ANY of the listed alternatives matches at the
  // current position.
  Assert.IsTrue (TWildCard.Create.Match('foo', '<"foo"|"bar"|"baz">'));
  Assert.IsTrue (TWildCard.Create.Match('bar', '<"foo"|"bar"|"baz">'));
  Assert.IsTrue (TWildCard.Create.Match('baz', '<"foo"|"bar"|"baz">'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltNoneOfTheAlternativesMatchesFailsTest;
begin
  Assert.IsFalse(TWildCard.Create.Match('qux', '<"foo"|"bar"|"baz">'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltCaseInsensitiveByDefaultTest;
begin
  Assert.IsTrue(TWildCard.Create.Match('FOO', '<"foo"|"bar">'));
  Assert.IsTrue(TWildCard.Create.Match('Bar', '<"foo"|"bar">'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltCaseSensitiveFlagTest;
begin
  Assert.IsTrue (TWildCard.Create(True).Match('foo', '<"foo"|"bar">'));
  Assert.IsFalse(TWildCard.Create(True).Match('FOO', '<"foo"|"bar">'));
  Assert.IsFalse(TWildCard.Create(True).Match('Bar', '<"foo"|"bar">'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltCombinedWithAsteriskTest;
begin
  // Embedded in a larger pattern: '*<"foo"|"bar">*' = the input must
  // contain either 'foo' or 'bar' somewhere.
  Assert.IsTrue (TWildCard.Create.Match('xxfooyy', '*<"foo"|"bar">*'));
  Assert.IsTrue (TWildCard.Create.Match('xxbaryy', '*<"foo"|"bar">*'));
  Assert.IsFalse(TWildCard.Create.Match('xxxxxxx',  '*<"foo"|"bar">*'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltReadmeExamplePatternTest;
begin
  // README example: '*<"3rdparty"|"ThirdParty">*.md' matches Markdown files
  // anywhere on the path that mention either name.
  Assert.IsTrue (TWildCard.Create.Match('docs/3rdparty/notes.md',    '*<"3rdparty"|"ThirdParty">*.md'));
  Assert.IsTrue (TWildCard.Create.Match('src/ThirdPartyReadme.md',   '*<"3rdparty"|"ThirdParty">*.md'));
  Assert.IsTrue (TWildCard.Create.Match('3RDPARTY-overview.md',      '*<"3rdparty"|"ThirdParty">*.md'),
    'CI default: case difference in the alt content still matches');
  Assert.IsFalse(TWildCard.Create.Match('docs/internal/notes.md',    '*<"3rdparty"|"ThirdParty">*.md'));
  Assert.IsFalse(TWildCard.Create.Match('docs/ThirdParty/notes.txt', '*<"3rdparty"|"ThirdParty">*.md'),
    'extension mismatch');
end;

procedure TWildCardMatcherDUnitX.QuotedAltAlternativesWithDifferentLengthsBacktrackTest;
begin
  // 'a' and 'ab' are both candidates at position 1.  With trailing 'b' in
  // the pattern, the engine must try the longer alt first or backtrack so
  // that input 'ab' followed by 'b' = 'abb' succeeds.
  Assert.IsTrue(TWildCard.Create.Match('abb', '<"a"|"ab">b'),
    'engine must explore both alternatives to find the one that lets the tail match');
  // Input 'ab' - first alt 'a' matches and leaves 'b' for the trailing 'b'.
  Assert.IsTrue(TWildCard.Create.Match('ab',  '<"a"|"ab">b'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltEmptyStringAlternativeMatchesZeroCharsTest;
begin
  // An empty alternative '""' matches zero characters - always succeeds at
  // the current position, consumes nothing.
  Assert.IsTrue (TWildCard.Create.Match('foo',    '<"">foo'));
  Assert.IsTrue (TWildCard.Create.Match('foobar', '<""|"foo">bar'),
    'empty alt matches first, leaves the whole input for the tail');
  // Single empty alt at end - matches when nothing else is left.
  Assert.IsTrue (TWildCard.Create.Match('foo', 'foo<"">'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltNegatedSameLengthAlternativesTest;
begin
  // Negation: NONE of the alts may be a prefix at the current position;
  // consume max(altLen) chars on success.  All alts here are length 3.
  Assert.IsTrue (TWildCard.Create.Match('quxsuffix', '<!"foo"|"bar">*'));
  Assert.IsFalse(TWildCard.Create.Match('foosuffix', '<!"foo"|"bar">*'));
  Assert.IsFalse(TWildCard.Create.Match('barsuffix', '<!"foo"|"bar">*'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltNegatedConsumesLongestAlternativeLengthTest;
begin
  // Alternatives have different lengths (3 and 6).  Longest = 6 so the
  // class consumes 6 chars on success.
  Assert.IsTrue (TWildCard.Create.Match('abcdef-tail', '<!"foo"|"barbaz">-tail'),
    'six chars consumed, then "-tail" matches the rest');
  Assert.IsFalse(TWildCard.Create.Match('foo-something', '<!"foo"|"barbaz">-tail'),
    'first three chars match the short alt as a prefix -> negation fails');
  Assert.IsFalse(TWildCard.Create.Match('barbaz-tail', '<!"foo"|"barbaz">-tail'),
    'first six chars match the long alt as a prefix -> negation fails');
end;

procedure TWildCardMatcherDUnitX.QuotedAltNegatedRequiresEnoughInputTest;
begin
  // Input must have at least max(altLen) chars remaining; otherwise the
  // class cannot consume its slice and the match fails.
  Assert.IsFalse(TWildCard.Create.Match('ab', '<!"foo"|"bar">'),
    'only 2 chars left but the class needs to consume 3');
  Assert.IsFalse(TWildCard.Create.Match('',   '<!"foo">'),
    'no input left, cannot consume the slice');
end;

procedure TWildCardMatcherDUnitX.QuotedAltMalformedReturnsFalseTest;
begin
  // Unterminated quote - the class never closes, FindClassEnd returns 0,
  // engine returns False.
  Assert.IsFalse(TWildCard.Create.Match('foo', '<"foo'));
  Assert.IsFalse(TWildCard.Create.Match('foo', '<"foo"|"bar'));
  // Missing '|' between alternatives.
  Assert.IsFalse(TWildCard.Create.Match('foo', '<"foo""bar">'));
  // Garbage between alternatives.
  Assert.IsFalse(TWildCard.Create.Match('foo', '<"foo"x"bar">'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltCloseBracketInsideQuotedAlternativeIsLiteralTest;
begin
  // A '>' inside '"..."' is part of the alternative literal, not the
  // class terminator.  This is the whole point of being quote-aware.
  Assert.IsTrue (TWildCard.Create.Match('a>b', '<"a>b">'));
  Assert.IsTrue (TWildCard.Create.Match('<x>', '<"<x>">'));
  // '[' and ']' are ordinary characters everywhere, quoted or not.
  Assert.IsTrue (TWildCard.Create.Match('[x]', '<"[x]">'));
end;

procedure TWildCardMatcherDUnitX.PipeOutsideClassIsLiteralTest;
begin
  // '|' has no special meaning outside '<...>' - it is just a literal
  // character.
  Assert.IsTrue (TWildCard.Create.Match('a|b',  'a|b'));
  Assert.IsTrue (TWildCard.Create.Match('a|b',  'a?b'),  '? matches the literal |');
  Assert.IsFalse(TWildCard.Create.Match('ab',   'a|b'),  'no | in input -> no match');
end;

{ Multi-pattern overloads }

procedure TWildCardMatcherDUnitX.MatchTArrayReturnsTrueWhenAnyMatchesTest;
begin
  // Should match because the second pattern matches '*.pas'
  Assert.IsTrue(TWildCard.Create.Match('Unit1.pas', TArray<string>.Create('*.txt', '*.pas', '*.dpr')));
  // Single-element array still works
  Assert.IsTrue(TWildCard.Create.Match('Unit1.pas', TArray<string>.Create('*.pas')));
end;

procedure TWildCardMatcherDUnitX.MatchTArrayReturnsFalseWhenNoneMatchTest;
begin
  Assert.IsFalse(TWildCard.Create.Match('Unit1.pas', TArray<string>.Create('*.txt', '*.doc', '*.csv')));
end;

procedure TWildCardMatcherDUnitX.MatchTArrayEmptyReturnsFalseTest;
begin
  // Empty pattern list - nothing to match against, must return False
  Assert.IsFalse(TWildCard.Create.Match('anything', TArray<string>.Create()));
end;

procedure TWildCardMatcherDUnitX.MatchTArrayShortCircuitsOnFirstMatchTest;
var
  LPatterns: TArray<string>;
begin
  // First pattern matches; later malformed pattern must not be evaluated.
  // If short-circuiting works, the unterminated '<' never gets visited.
  LPatterns := TArray<string>.Create('*.pas', '<unterminated');
  Assert.IsTrue(TWildCard.Create.Match('Unit1.pas', LPatterns));
end;

procedure TWildCardMatcherDUnitX.MatchTArrayHonoursCaseSensitiveFlagTest;
var
  LPatterns: TArray<string>;
begin
  LPatterns := TArray<string>.Create('*.PAS', '*.DPR');

  Assert.IsTrue (TWildCard.Create.Match('unit1.pas', LPatterns), 'case-insensitive default');
  Assert.IsFalse(TWildCard.Create(True).Match('unit1.pas', LPatterns),  'case-sensitive should not match');
end;

procedure TWildCardMatcherDUnitX.MatchTStringsReturnsTrueWhenAnyMatchesTest;
var
  LList: TStringList;
begin
  LList := TStringList.Create;
  try
    LList.Add('*.txt');
    LList.Add('*.pas');
    LList.Add('*.dpr');

    Assert.IsTrue(TWildCard.Create.Match('Unit1.pas', LList));
  finally
    LList.Free;
  end;
end;

procedure TWildCardMatcherDUnitX.MatchTStringsReturnsFalseWhenNoneMatchTest;
var
  LList: TStringList;
begin
  LList := TStringList.Create;
  try
    LList.Add('*.txt');
    LList.Add('*.doc');

    Assert.IsFalse(TWildCard.Create.Match('Unit1.pas', LList));
  finally
    LList.Free;
  end;
end;

procedure TWildCardMatcherDUnitX.MatchTStringsEmptyReturnsFalseTest;
var
  LList: TStringList;
begin
  LList := TStringList.Create;
  try
    Assert.IsFalse(TWildCard.Create.Match('anything', LList));
  finally
    LList.Free;
  end;
end;

procedure TWildCardMatcherDUnitX.MatchTStringsHonoursCaseSensitiveFlagTest;
var
  LList: TStringList;
begin
  LList := TStringList.Create;
  try
    LList.Add('*.PAS');
    LList.Add('*.DPR');

    Assert.IsTrue (TWildCard.Create.Match('unit1.pas', LList));
    Assert.IsFalse(TWildCard.Create(True).Match('unit1.pas', LList));
  finally
    LList.Free;
  end;
end;

{ Non-ASCII character handling }

procedure TWildCardMatcherDUnitX.NonAsciiFinnishCaseInsensitiveTest;
begin
  // Finnish 'ä', 'ö', 'å' (and capitals 'Ä', 'Ö', 'Å') round-trip through
  // the Unicode-aware ToUpper fallback, so case-insensitive matching must
  // treat them as the same character.
  Assert.IsTrue(TWildCard.Create.Match('p' + #$00E4 + 'iv' + #$00E4, 'p' + #$00C4 + 'IV' + #$00C4),
    '"pAEivAE" lowercase should match "PAEIVAE" pattern case-insensitively');
  Assert.IsTrue(TWildCard.Create.Match('M' + #$00C4 + 'KEL' + #$00C4, 'm' + #$00E4 + 'kel' + #$00E4),
    'mixed-case Finnish should match');
  Assert.IsTrue(TWildCard.Create.Match('h' + #$00F6 + 'yl' + #$00E4 + 't' + #$00E4,
                                'H' + #$00D6 + 'YL' + #$00C4 + 'T' + #$00C4));
  Assert.IsTrue(TWildCard.Create.Match('p' + #$00E5 + 'iv' + #$00E5, 'P' + #$00C5 + 'IV' + #$00C5),
    'Finnish/Swedish a-ring');
end;

procedure TWildCardMatcherDUnitX.NonAsciiFinnishCaseSensitiveTest;
begin
  // Case-sensitive path must distinguish 'ä' from 'Ä'.
  Assert.IsTrue (TWildCard.Create(True).Match('p' + #$00E4 + 'iv' + #$00E4, 'p' + #$00E4 + 'iv' + #$00E4));
  Assert.IsFalse(TWildCard.Create(True).Match('p' + #$00E4 + 'iv' + #$00E4, 'p' + #$00C4 + 'IV' + #$00C4),
    'case-sensitive should reject case mismatch on Finnish letters');
end;

procedure TWildCardMatcherDUnitX.NonAsciiFinnishInsideCharClassTest;
begin
  // Character class with non-ASCII members.
  Assert.IsTrue (TWildCard.Create.Match(#$00E4, '<' + #$00E4 + #$00F6 + #$00E5 + '>'));
  Assert.IsTrue (TWildCard.Create.Match(#$00C4, '<' + #$00E4 + #$00F6 + #$00E5 + '>'),
    'uppercase input should match lowercase class members CI');
  Assert.IsFalse(TWildCard.Create.Match(#$00FC, '<' + #$00E4 + #$00F6 + #$00E5 + '>'),
    'German u-umlaut is NOT in the Finnish set');

  // Negation with non-ASCII
  Assert.IsFalse(TWildCard.Create.Match(#$00E4, '<!' + #$00E4 + #$00F6 + #$00E5 + '>'));
  Assert.IsTrue (TWildCard.Create.Match('a',    '<!' + #$00E4 + #$00F6 + #$00E5 + '>'));
end;

procedure TWildCardMatcherDUnitX.NonAsciiFinnishInsidePatternBacktrackingTest;
begin
  // '*' must traverse non-ASCII characters correctly.
  Assert.IsTrue (TWildCard.Create.Match('P' + #$00C4 + 'IV' + #$00C4 + 'KIRJA.txt', '*' + #$00E4 + '*.txt'),
    '"*<lower-a-umlaut>*" must find the upper-a-umlaut chars CI');
  Assert.IsTrue (TWildCard.Create.Match('Yhteenveto_p' + #$00E4 + 'iv' + #$00E4 + 'lt' + #$00E4 + '.log',
                                 '*p' + #$00C4 + 'IV' + #$00C4 + 'LT' + #$00C4 + '*'));
  Assert.IsFalse(TWildCard.Create.Match('something_else.txt', '*' + #$00E4 + '*.txt'),
    'no umlaut anywhere - must not match');
end;

procedure TWildCardMatcherDUnitX.NonAsciiGermanSpanishFrenchTest;
begin
  // German u-umlaut
  Assert.IsTrue(TWildCard.Create.Match('M' + #$00FC + 'nchen', 'm' + #$00DC + 'nchen'),
    'German u-umlaut CI');
  // German sharp s - Unicode ToUpper may keep it as 'ß' (Char-by-Char), so
  // case-insensitive lowercase-vs-lowercase must work; we don't promise
  // 'ß' == 'SS' (that's a multi-char expansion).
  Assert.IsTrue(TWildCard.Create.Match('stra' + #$00DF + 'e', 'stra' + #$00DF + 'e'));

  // Spanish n-tilde
  Assert.IsTrue(TWildCard.Create.Match('a' + #$00F1 + 'o', 'A' + #$00D1 + 'O'),
    'Spanish n-tilde CI');
  Assert.IsTrue(TWildCard.Create.Match('JALAPE' + #$00D1 + 'O.txt', '*' + #$00F1 + 'O.txt'));

  // French e-acute, c-cedilla
  Assert.IsTrue(TWildCard.Create.Match('Caf' + #$00E9, 'CAF' + #$00C9), 'French e-acute CI');
  Assert.IsTrue(TWildCard.Create.Match('fran' + #$00E7 + 'ais', 'FRAN' + #$00C7 + 'AIS'),
    'French c-cedilla CI');
end;

procedure TWildCardMatcherDUnitX.NonAsciiCyrillicAndGreekTest;
begin
  // Cyrillic small ya -> capital Ya (U+044F -> U+042F)
  Assert.IsTrue(TWildCard.Create.Match(#$044F + 'blok', #$042F + 'BLOK'),
    'Cyrillic CI roundtrip');
  // Greek small alpha -> capital alpha (U+03B1 -> U+0391)
  Assert.IsTrue(TWildCard.Create.Match(#$03B1 + 'rch', #$0391 + 'RCH'),
    'Greek CI roundtrip');
end;

procedure TWildCardMatcherDUnitX.NonAsciiMixedAsciiAndUnicodeTest;
begin
  // ASCII and non-ASCII intermixed; '?', digit runs, class all combine fine.
  Assert.IsTrue (TWildCard.Create.Match('Raportti_2024_' + #$00E4 + '.log', 'raportti_<####>_?.log'));
  Assert.IsTrue (TWildCard.Create.Match('P' + #$00E4 + 'iv' + #$00E4 + 'kirja_03.pas',
                                 'P<' + #$00E4 + #$00F6 + '>iv<' + #$00E4 + #$00F6 + '>kirja_<##>.pas'));
  Assert.IsFalse(TWildCard.Create.Match('P' + #$00FC + 'iv' + #$00FC + 'kirja_03.pas',
                                 'P<' + #$00E4 + #$00F6 + '>iv<' + #$00E4 + #$00F6 + '>kirja_<##>.pas'),
    'u-umlaut input must not satisfy a-umlaut/o-umlaut class');
end;

{ Instance API: pre-registered patterns }

procedure TWildCardMatcherDUnitX.InstanceDefaultConstructorHasNoRegisteredPatternsTest;
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create;
  Assert.AreEqual(0, Length(LMask.RegisteredPatterns), 'no patterns registered');
  Assert.IsFalse(LMask.CaseSensitive, 'default case-insensitive');
  Assert.IsFalse(LMask.Match('anything'), 'no registered patterns -> never matches');
end;

procedure TWildCardMatcherDUnitX.InstanceCreateWithSinglePatternRegistersItTest;
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create('*.pas');
  Assert.AreEqual(1, Length(LMask.RegisteredPatterns));

  Assert.IsTrue (LMask.Match('Unit1.pas'));
  Assert.IsTrue (LMask.Match('UNIT1.PAS'),  'CI default');
  Assert.IsFalse(LMask.Match('Unit1.dpr'));
end;

procedure TWildCardMatcherDUnitX.InstanceCreateWithTArrayRegistersAllTest;
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create(TArray<string>.Create('*.pas', '*.dpr', '*.inc'));
  Assert.AreEqual(3, Length(LMask.RegisteredPatterns));

  Assert.IsTrue (LMask.Match('Unit1.pas'));
  Assert.IsTrue (LMask.Match('Program.dpr'));
  Assert.IsTrue (LMask.Match('Common.inc'));
  Assert.IsFalse(LMask.Match('Readme.txt'));
end;

procedure TWildCardMatcherDUnitX.InstanceCreateWithTStringsRegistersAllTest;
var
  LList: TStringList;
  LMask: TWildCard;
begin
  LList := TStringList.Create;
  try
    LList.Add('*.pas');
    LList.Add('*.dpr');

    LMask := TWildCard.Create(LList);
    Assert.AreEqual(2, Length(LMask.RegisteredPatterns));
    Assert.IsTrue (LMask.Match('Unit1.pas'));
    Assert.IsTrue (LMask.Match('Project.dpr'));
    Assert.IsFalse(LMask.Match('Notes.txt'));
  finally
    LList.Free;
  end;
end;

procedure TWildCardMatcherDUnitX.InstanceMatchNoArgsUsesRegisteredPatternsTest;
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create(TArray<string>.Create('Foo*', 'Bar*'));
  Assert.IsTrue (LMask.Match('Foo123'));
  Assert.IsTrue (LMask.Match('BarBaz'));
  Assert.IsFalse(LMask.Match('Other'));
end;

procedure TWildCardMatcherDUnitX.InstanceMatchNoArgsEmptySetReturnsFalseTest;
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create;
  // No registered patterns - Match(input) must always return False.
  Assert.IsFalse(LMask.Match('anything'));
  Assert.IsFalse(LMask.Match(''));
end;

procedure TWildCardMatcherDUnitX.InstanceMatchWithPatternIgnoresRegisteredByDefaultTest;
var
  LMask: TWildCard;
begin
  // Register one pattern, then call Match(input, otherPattern) - by default
  // only the ad-hoc pattern is tried, the registered one is ignored.
  LMask := TWildCard.Create('*.pas');

  // Input matches the registered pattern but NOT the ad-hoc one -> False
  Assert.IsFalse(LMask.Match('Unit1.pas', '*.dpr'),
    'registered must NOT contribute when AAlsoMatchRegistered is False (default)');

  // Input matches the ad-hoc pattern -> True
  Assert.IsTrue(LMask.Match('Program.dpr', '*.dpr'));
end;

procedure TWildCardMatcherDUnitX.InstanceMatchWithPatternAndAlsoMatchRegisteredTest;
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create('*.pas');

  // With AAlsoMatchRegistered=True, registered patterns ARE also tried
  Assert.IsTrue(LMask.Match('Unit1.pas', '*.dpr', True),
    'registered ".pas" should match when AAlsoMatchRegistered is True');
  Assert.IsTrue(LMask.Match('Program.dpr', '*.dpr', True),
    'ad-hoc pattern still matches when AAlsoMatchRegistered is True');
  Assert.IsFalse(LMask.Match('Readme.txt', '*.dpr', True),
    'neither matches -> False');
end;

procedure TWildCardMatcherDUnitX.InstanceMatchWithTArrayPatternsTest;
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create('*.pas');

  // Ad-hoc multi-pattern, registered ignored
  Assert.IsTrue (LMask.Match('Project.dpr', TArray<string>.Create('*.dpr', '*.inc')));
  Assert.IsFalse(LMask.Match('Unit1.pas',   TArray<string>.Create('*.dpr', '*.inc')),
    'registered .pas must not contribute by default');

  // Same call with AAlsoMatchRegistered=True - now the registered .pas matches
  Assert.IsTrue(LMask.Match('Unit1.pas', TArray<string>.Create('*.dpr', '*.inc'), True));
end;

procedure TWildCardMatcherDUnitX.InstanceMatchWithTStringsPatternsAndAlsoMatchRegisteredTest;
var
  LMask: TWildCard;
  LExtras: TStringList;
begin
  LMask := TWildCard.Create('*.pas');
  LExtras := TStringList.Create;
  try
    LExtras.Add('*.dpr');
    LExtras.Add('*.inc');

    // Default (AAlsoMatchRegistered=False): only the extras are tried
    Assert.IsTrue (LMask.Match('Program.dpr', LExtras),
      'TStrings extras: .dpr matches');
    Assert.IsFalse(LMask.Match('Unit1.pas',   LExtras),
      'TStrings extras: registered .pas must NOT contribute by default');
    Assert.IsFalse(LMask.Match('Readme.txt',  LExtras),
      'TStrings extras: nothing matches');

    // With AAlsoMatchRegistered=True the registered .pas matches too
    Assert.IsTrue(LMask.Match('Unit1.pas', LExtras, True),
      'TStrings extras + AAlsoMatchRegistered=True: registered .pas now matches');
    Assert.IsFalse(LMask.Match('Readme.txt', LExtras, True),
      'TStrings extras + AAlsoMatchRegistered=True: still no match');
  finally
    LExtras.Free;
  end;
end;

procedure TWildCardMatcherDUnitX.InstanceMatchCaseSensitiveWithAlsoMatchRegisteredTest;
var
  LMask: TWildCard;
begin
  // Case-sensitive instance with registered patterns. AAlsoMatchRegistered
  // must route through the CS engine for BOTH the ad-hoc pattern and the
  // registered set.
  LMask := TWildCard.Create('*.pas', True);

  // Registered '.pas' is lowercase, so 'UNIT.PAS' does NOT match it in CS
  // mode.  Ad-hoc 'UNIT.PAS' literal also won't match input 'UNIT1.PAS'.
  Assert.IsFalse(LMask.Match('UNIT1.PAS', 'OTHER*', True),
    'CS + AAlsoMatchRegistered: neither extra nor registered (case mismatch) matches');

  // Lowercase input matches the registered '.pas' under CS only when also
  // requested
  Assert.IsFalse(LMask.Match('unit1.pas', '*.xyz'),
    'CS: extra alone does not match');
  Assert.IsTrue(LMask.Match('unit1.pas', '*.xyz', True),
    'CS + AAlsoMatchRegistered: registered ".pas" matches lowercase input');

  // Ad-hoc match still wins fast (must respect CS exact case)
  Assert.IsTrue (LMask.Match('Program.DPR', '*.DPR', True),
    'CS: ad-hoc with exact-case extension matches');
  Assert.IsFalse(LMask.Match('Program.DPR', '*.dpr', True),
    'CS: lowercase ad-hoc must NOT match uppercase input, and registered .pas does not match .DPR either');
end;

procedure TWildCardMatcherDUnitX.InstanceMatchEmptyRegisteredSetWithAlsoMatchRegisteredTrueTest;
var
  LMask: TWildCard;
begin
  // No registered patterns + AAlsoMatchRegistered=True: only the ad-hoc
  // pattern decides; the "also try registered" branch is a no-op because
  // there are no registered patterns to try.
  LMask := TWildCard.Create;

  Assert.IsTrue (LMask.Match('Unit1.pas', '*.pas', True),
    'extra matches, empty registered set is harmless');
  Assert.IsFalse(LMask.Match('Unit1.pas', '*.dpr', True),
    'extra does not match and registered is empty -> False');
  Assert.IsFalse(LMask.Match('Unit1.pas', TArray<string>.Create('*.dpr', '*.inc'), True),
    'TArray variant: same behaviour with empty registered set');
end;

procedure TWildCardMatcherDUnitX.InstanceMatchRegisteredHitButAdHocMissWithAlsoMatchRegisteredTest;
var
  LMask: TWildCard;
begin
  // Specifically exercises the "ad-hoc fails, then fall back to registered"
  // ordering inside the engine - covers all three ad-hoc overloads
  // (single, TArray, TStrings).
  LMask := TWildCard.Create(TArray<string>.Create('*.pas', '*.dpr'));

  // Single ad-hoc pattern misses, registered '.pas' catches it
  Assert.IsTrue(LMask.Match('Unit1.pas', '*.xyz', True),
    'single ad-hoc miss -> registered .pas catches');

  // TArray ad-hoc all miss, registered .dpr catches
  Assert.IsTrue(LMask.Match('Project.dpr',
    TArray<string>.Create('*.xyz', '*.zzz'), True),
    'TArray ad-hoc all miss -> registered .dpr catches');

  // None of the ad-hoc nor registered match
  Assert.IsFalse(LMask.Match('Readme.txt',
    TArray<string>.Create('*.xyz', '*.zzz'), True),
    'nothing matches anywhere');
end;

procedure TWildCardMatcherDUnitX.InstanceCaseSensitivityIsLockedAtCreateTest;
var
  LCI, LCS: TWildCard;
begin
  LCI := TWildCard.Create('foo*');
  LCS := TWildCard.Create('foo*', True);

  Assert.IsFalse(LCI.CaseSensitive);
  Assert.IsTrue (LCS.CaseSensitive);

  // CI instance matches regardless of case
  Assert.IsTrue (LCI.Match('FooBar'));
  Assert.IsTrue (LCI.Match('foobar'));
  Assert.IsTrue (LCI.Match('FOOBAR'));

  // CS instance matches only exact lowercase prefix
  Assert.IsTrue (LCS.Match('foobar'));
  Assert.IsFalse(LCS.Match('FooBar'));
  Assert.IsFalse(LCS.Match('FOOBAR'));

  // Ad-hoc Match uses the instance's locked case mode too
  Assert.IsTrue (LCS.Match('UNIT.PAS', '*.PAS'),  'CS instance: exact case match');
  Assert.IsFalse(LCS.Match('Unit.PAS', '*.pas'), 'CS instance: must not lower-case match');
end;

procedure TWildCardMatcherDUnitX.InstanceMatchWorksOnDefaultInitialisedRecordTest;
var
  LMask: TWildCard;
begin
  // FCaseSensitive is a non-managed Boolean - a bare 'var LMask: TWildCard'
  // does NOT guarantee it starts False (stack garbage), so we use Default()
  // to force a real zero-initialisation.  This documents the "skip Create"
  // shortcut for callers who want a one-off CI matcher with no registered
  // patterns: Default(TWildCard) is equivalent to TWildCard.Create(False).
  LMask := Default(TWildCard);

  Assert.IsFalse(LMask.CaseSensitive);
  Assert.AreEqual(0, Length(LMask.RegisteredPatterns));

  Assert.IsFalse(LMask.Match('anything'),           'empty set');
  Assert.IsTrue (LMask.Match('Unit1.pas', '*.pas'), 'ad-hoc still works');
end;

{ Registered patterns run the COMPILED engine - edge-case coverage }

procedure TWildCardMatcherDUnitX.CompiledCharClassEdgeCasesTest;
begin
  // Literal '>' as first content char
  Assert.IsTrue (TWildCard.Create('<>abc>').Match('>'));
  Assert.IsTrue (TWildCard.Create('<>abc>').Match('a'));
  Assert.IsFalse(TWildCard.Create('<>abc>').Match('x'));

  // Literal '-' at start / end of class
  Assert.IsTrue (TWildCard.Create('<-x>').Match('-'));
  Assert.IsTrue (TWildCard.Create('<a->').Match('-'));
  Assert.IsFalse(TWildCard.Create('<a->').Match('b'));

  // Ranges + negation
  Assert.IsTrue (TWildCard.Create('b<a-c>d').Match('bbd'));
  Assert.IsFalse(TWildCard.Create('b<a-c>d').Match('bdd'));
  Assert.IsTrue (TWildCard.Create('b<!ae>ll').Match('bull'));
  Assert.IsFalse(TWildCard.Create('b<!ae>ll').Match('ball'));
  Assert.IsTrue (TWildCard.Create('<a-cq0-9>').Match('7'));
  Assert.IsFalse(TWildCard.Create('<a-cq0-9>').Match('z'));
end;

procedure TWildCardMatcherDUnitX.CompiledQuotedAltTest;
begin
  Assert.IsTrue (TWildCard.Create('<"foo"|"bar">').Match('bar'));
  Assert.IsFalse(TWildCard.Create('<"foo"|"bar">').Match('qux'));
  Assert.IsTrue (TWildCard.Create('*<"3rdparty"|"ThirdParty">*.md').Match('docs\3rdparty\notes.md'));
  Assert.IsFalse(TWildCard.Create('*<"3rdparty"|"ThirdParty">*.md').Match('docs\internal\notes.md'));

  // Different-length alternatives require backtracking across alts
  Assert.IsTrue (TWildCard.Create('<"a"|"ab">b').Match('abb'));
  Assert.IsTrue (TWildCard.Create('<"a"|"ab">b').Match('ab'));

  // Empty alternative matches zero chars
  Assert.IsTrue (TWildCard.Create('<""|"foo">bar').Match('foobar'));
  Assert.IsTrue (TWildCard.Create('foo<"">').Match('foo'));

  // Negation consumes the longest alternative's length
  Assert.IsTrue (TWildCard.Create('<!"foo"|"bar">*').Match('quxsuffix'));
  Assert.IsFalse(TWildCard.Create('<!"foo"|"bar">*').Match('foosuffix'));
  Assert.IsTrue (TWildCard.Create('<!"foo"|"barbaz">-tail').Match('abcdef-tail'));
  Assert.IsFalse(TWildCard.Create('<!"foo"|"barbaz">-tail').Match('barbaz-tail'));

  // '>' inside a quoted alternative is a literal
  Assert.IsTrue (TWildCard.Create('<"a>b">').Match('a>b'));
  Assert.IsTrue (TWildCard.Create('<"<x>">').Match('<x>'));
end;

procedure TWildCardMatcherDUnitX.CompiledStarFastPathsTest;
begin
  // Tail anchor ('*literal' at pattern end)
  Assert.IsTrue (TWildCard.Create('*.pas').Match('Unit1.pas'));
  Assert.IsFalse(TWildCard.Create('*.pas').Match('Unit1.pa'));
  Assert.IsTrue (TWildCard.Create('*.pas').Match('.pas'));
  Assert.IsFalse(TWildCard.Create('*.pas').Match('pas'), 'input shorter than the literal tail');

  // Multiple stars with first-char skips
  Assert.IsTrue (TWildCard.Create('*a*b*c*').Match('xaxbxcx'));
  Assert.IsFalse(TWildCard.Create('*a*b*c*').Match('xcxbxa'), 'letters out of order');

  // Prefix literal then trailing star
  Assert.IsTrue (TWildCard.Create('wh*').Match('what'));
  Assert.IsFalse(TWildCard.Create('wh*').Match('awhile'));

  // Star followed by a digit run (digit skip) and by a class
  Assert.IsTrue (TWildCard.Create('*Repository*_v<#>_*.xyz').Match('OrderRepository_v3_final.xyz'));
  Assert.IsFalse(TWildCard.Create('*Repository*_v<#>_*.xyz').Match('OrderRepository_vX_final.xyz'));
  Assert.IsTrue (TWildCard.Create('*<##>.log').Match('trace_42.log'));
  Assert.IsFalse(TWildCard.Create('*<##>.log').Match('trace_4.log'));
  Assert.IsTrue (TWildCard.Create('*<abc>end').Match('xxxaend'));
  Assert.IsFalse(TWildCard.Create('*<abc>end').Match('xxxdend'));
end;

procedure TWildCardMatcherDUnitX.CompiledDigitAndQuestionTest;
begin
  Assert.IsTrue (TWildCard.Create('Test_<###>.log').Match('Test_001.log'));
  Assert.IsFalse(TWildCard.Create('Test_<###>.log').Match('Test_00A.log'));
  Assert.IsTrue (TWildCard.Create('b?ll').Match('ball'));
  Assert.IsFalse(TWildCard.Create('b?ll').Match('bll'));
  Assert.IsTrue (TWildCard.Create('*_v<#>_*').Match('x_v3_y'));
  Assert.IsFalse(TWildCard.Create('*_v<#>_*').Match('x_vA_y'));
end;

procedure TWildCardMatcherDUnitX.CompiledCaseSensitiveTest;
begin
  // CS instance: ordinal compiled engine
  Assert.IsTrue (TWildCard.Create('*.pas', True).Match('unit1.pas'));
  Assert.IsFalse(TWildCard.Create('*.pas', True).Match('UNIT1.PAS'));
  Assert.IsTrue (TWildCard.Create('<"foo"|"bar">', True).Match('bar'));
  Assert.IsFalse(TWildCard.Create('<"foo"|"bar">', True).Match('BAR'));

  // CI instance: per-char engine, pattern case irrelevant
  Assert.IsTrue (TWildCard.Create('*.PAS').Match('unit1.pas'));
  Assert.IsTrue (TWildCard.Create('<"FOO"|"BAR">').Match('bar'));
end;

procedure TWildCardMatcherDUnitX.CompiledEmptyAndMalformedPatternTest;
begin
  // Empty registered pattern matches only the empty input
  Assert.IsTrue (TWildCard.Create('').Match(''));
  Assert.IsFalse(TWildCard.Create('').Match('x'));

  // Malformed patterns never match (compiled as invalid)
  Assert.IsFalse(TWildCard.Create('<abc').Match('a'));
  Assert.IsFalse(TWildCard.Create('<"foo').Match('foo'));
  Assert.IsFalse(TWildCard.Create('<"foo"|"bar').Match('foo'));
  Assert.IsFalse(TWildCard.Create('<"foo""bar">').Match('foo'));
  Assert.IsFalse(TWildCard.Create('<"foo"x"bar">').Match('foo'));

  // A malformed pattern in a set must not break the others
  Assert.IsTrue(TWildCard.Create(TArray<string>.Create('<bad', '*.pas')).Match('Unit1.pas'));
end;

{ TWildCardFilter: include / exclude wrapper }

procedure TWildCardMatcherDUnitX.FilterEmptyAcceptsEverythingTest;
var
  LFilter: TWildCardFilter;
begin
  // Both lists empty: everything passes.
  LFilter := TWildCardFilter.Create;

  Assert.IsTrue(LFilter.Accepts('anything.txt'));
  Assert.IsTrue(LFilter.Accepts(''));
  Assert.IsTrue(LFilter.Accepts('C:\some\path\file.pas'));
end;

procedure TWildCardMatcherDUnitX.FilterIncludeOnlyTest;
var
  LFilter: TWildCardFilter;
begin
  // Include list restricts; empty exclude list excludes nothing.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas', '*.dpr'),
    TArray<string>.Create());

  Assert.IsTrue (LFilter.Accepts('Unit1.pas'));
  Assert.IsTrue (LFilter.Accepts('Project.dpr'));
  Assert.IsFalse(LFilter.Accepts('Notes.txt'));
  Assert.IsFalse(LFilter.Accepts('Readme.md'));
end;

procedure TWildCardMatcherDUnitX.FilterExcludeOnlyTest;
var
  LFilter: TWildCardFilter;
begin
  // Empty include list = everything included; excludes carve out.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create(),
    TArray<string>.Create('*backup*', '*.tmp'));

  Assert.IsTrue (LFilter.Accepts('Unit1.pas'));
  Assert.IsTrue (LFilter.Accepts('Notes.txt'));
  Assert.IsFalse(LFilter.Accepts('Unit1_backup.pas'));
  Assert.IsFalse(LFilter.Accepts('scratch.tmp'));
end;

procedure TWildCardMatcherDUnitX.FilterExcludeWinsOverIncludeTest;
var
  LFilter: TWildCardFilter;
begin
  // A file matching BOTH lists is rejected - exclude always wins.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas'),
    TArray<string>.Create('*_test*'));

  Assert.IsTrue (LFilter.Accepts('Unit1.pas'));
  Assert.IsFalse(LFilter.Accepts('Unit1_test.pas'), 'in include AND exclude -> excluded');
  Assert.IsFalse(LFilter.Accepts('Notes.txt'),      'not included at all');
end;

procedure TWildCardMatcherDUnitX.FilterSinglePatternOverloadTest;
var
  LFilter: TWildCardFilter;
begin
  LFilter := TWildCardFilter.Create('*.pas', '*_backup*');

  Assert.IsTrue (LFilter.Accepts('Unit1.pas'));
  Assert.IsFalse(LFilter.Accepts('Unit1_backup.pas'));
  Assert.IsFalse(LFilter.Accepts('Notes.txt'));

  // Empty string means "no pattern for that side", not "match empty input".
  LFilter := TWildCardFilter.Create('', '*.tmp');
  Assert.IsTrue (LFilter.Accepts('anything.pas'), 'empty include = include everything');
  Assert.IsFalse(LFilter.Accepts('scratch.tmp'));
end;

procedure TWildCardMatcherDUnitX.FilterTStringsOverloadWithNilTest;
var
  LIncludes: TStringList;
  LFilter: TWildCardFilter;
begin
  LIncludes := TStringList.Create;
  try
    LIncludes.Add('*.pas');
    LIncludes.Add('*.dpr');

    // nil exclude list is tolerated - nothing excluded.
    LFilter := TWildCardFilter.Create(LIncludes, nil);

    Assert.IsTrue (LFilter.Accepts('Unit1.pas'));
    Assert.IsFalse(LFilter.Accepts('Notes.txt'));

    // nil include list = everything included.
    LFilter := TWildCardFilter.Create(nil, LIncludes);
    Assert.IsFalse(LFilter.Accepts('Unit1.pas'), 'now the same list excludes');
    Assert.IsTrue (LFilter.Accepts('Notes.txt'));
  finally
    LIncludes.Free;
  end;
end;

procedure TWildCardMatcherDUnitX.FilterCaseSensitivityTest;
var
  LFilter: TWildCardFilter;
begin
  // Case-insensitive by default - applies to both lists.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas'),
    TArray<string>.Create('*backup*'));
  Assert.IsTrue (LFilter.Accepts('UNIT1.PAS'));
  Assert.IsFalse(LFilter.Accepts('UNIT1_BACKUP.PAS'));

  // Case-sensitive: neither list matches on case mismatch.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas'),
    TArray<string>.Create('*backup*'), True);
  Assert.IsFalse(LFilter.Accepts('UNIT1.PAS'),       'CS include must not match');
  Assert.IsTrue (LFilter.Accepts('unit1_BACKUP.pas'), 'CS exclude must not match uppercase BACKUP');
  Assert.IsFalse(LFilter.Accepts('unit1_backup.pas'), 'CS exclude matches exact case');
end;

procedure TWildCardMatcherDUnitX.FilterQuotedAltPatternsTest;
var
  LFilter: TWildCardFilter;
begin
  // Full pattern syntax works in both lists.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.md'),
    TArray<string>.Create('*<"draft"|"backup">*'));

  Assert.IsTrue (LFilter.Accepts('docs\notes.md'));
  Assert.IsFalse(LFilter.Accepts('docs\notes_draft.md'));
  Assert.IsFalse(LFilter.Accepts('docs\backup_notes.md'));
  Assert.IsFalse(LFilter.Accepts('docs\notes.txt'));
end;

procedure TWildCardMatcherDUnitX.FilterDefaultInitialisedRecordAcceptsEverythingTest;
var
  LFilter: TWildCardFilter;
begin
  // Default(TWildCardFilter) = no patterns anywhere - accepts everything,
  // case-insensitive.  Mirrors the Default(TWildCard) guarantee.
  LFilter := Default(TWildCardFilter);

  Assert.IsFalse(LFilter.CaseSensitive);
  Assert.AreEqual(0, Length(LFilter.IncludePatterns));
  Assert.AreEqual(0, Length(LFilter.ExcludePatterns));
  Assert.IsTrue(LFilter.Accepts('anything'));
  Assert.IsTrue(LFilter.Accepts(''));
end;

{ ValidatePattern }

procedure TWildCardMatcherDUnitX.ValidatePatternAcceptsGoodPatternsTest;
var
  LError: string;
begin
  Assert.IsTrue(TWildCard.ValidatePattern('', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('*', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('*.pas', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('Test_<###>.log', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('b<ae>ll', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('<>abc>', LError), 'leading > is a literal member');
  Assert.IsTrue(TWildCard.ValidatePattern('<-x>', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('<!a-z>', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('<"a"|"b">', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('<!"foo"|"bar">', LError));
  Assert.IsTrue(TWildCard.ValidatePattern('foo<"">', LError), 'empty alternative is legal');
  Assert.IsTrue(TWildCard.ValidatePattern('a|b', LError), '| outside a class is a literal');
  Assert.IsTrue(TWildCard.ValidatePattern('C:\Temp\Foo[ab].txt', LError), '[ and ] are plain literals');
  Assert.IsTrue(TWildCard.ValidatePattern('Track #01.mp3', LError), '# is a plain literal');
  Assert.IsTrue(TWildCard.ValidatePattern('a[bc', LError), 'unbalanced [ is just text now');

  Assert.AreEqual('', LError, 'no error message for valid patterns');
end;

procedure TWildCardMatcherDUnitX.ValidatePatternRejectsMalformedPatternsTest;
var
  LError: string;
begin
  Assert.IsFalse(TWildCard.ValidatePattern('<abc', LError));
  Assert.IsTrue(LError.Contains('position 1'), LError);

  Assert.IsFalse(TWildCard.ValidatePattern('a<bc', LError));
  Assert.IsTrue(LError.Contains('position 2'), LError);

  Assert.IsFalse(TWildCard.ValidatePattern('<>', LError), 'bare <> is an unterminated class');
  Assert.IsTrue(LError.Contains('position 1'), LError);

  Assert.IsFalse(TWildCard.ValidatePattern('Test_<###.log', LError), 'unterminated digit run');
  Assert.IsTrue(LError.Contains('position 6'), LError);

  Assert.IsFalse(TWildCard.ValidatePattern('<"foo', LError), 'unterminated quoted alternative');
  Assert.IsTrue(LError.Contains('position 2'), LError);

  Assert.IsFalse(TWildCard.ValidatePattern('<"a"x"b">', LError), 'garbage between alternatives');
  Assert.IsTrue(LError.Contains('position 5'), LError);

  Assert.IsFalse(TWildCard.ValidatePattern('<"a"|"b', LError), 'unterminated second alternative');
  Assert.IsTrue(LError.Contains('position 6'), LError);

  Assert.IsFalse(TWildCard.ValidatePattern('<"a"', LError), 'class never closed after alternative');
  Assert.IsTrue(LError.Contains('position 1'), LError);
end;

{ Match attribution: MatchIndex / AcceptsEx }

procedure TWildCardMatcherDUnitX.MatchIndexTest;
var
  LMask: TWildCard;
begin
  LMask := TWildCard.Create(TArray<string>.Create('*.txt', '*.pas', '*.dpr'));

  Assert.AreEqual(0, LMask.MatchIndex('readme.txt'));
  Assert.AreEqual(1, LMask.MatchIndex('Unit1.pas'));
  Assert.AreEqual(2, LMask.MatchIndex('Project.dpr'));
  Assert.AreEqual(-1, LMask.MatchIndex('image.png'));

  // First hit wins - the catch-all registered first shadows the rest.
  LMask := TWildCard.Create(TArray<string>.Create('*', '*.pas'));
  Assert.AreEqual(0, LMask.MatchIndex('Unit1.pas'));

  // Empty registered set never matches.
  LMask := Default(TWildCard);
  Assert.AreEqual(-1, LMask.MatchIndex('anything'));
end;

procedure TWildCardMatcherDUnitX.FilterAcceptsExTest;
var
  LFilter: TWildCardFilter;
  LIncludeIndex, LExcludeIndex: Integer;
begin
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas', '*.dpr'),
    TArray<string>.Create('*_test*', '*backup*'));

  Assert.IsTrue(LFilter.AcceptsEx('Project.dpr', LIncludeIndex, LExcludeIndex));
  Assert.AreEqual(1, LIncludeIndex, 'matched by the .dpr include');
  Assert.AreEqual(-1, LExcludeIndex);

  Assert.IsFalse(LFilter.AcceptsEx('Unit_test.pas', LIncludeIndex, LExcludeIndex));
  Assert.AreEqual(0, LIncludeIndex, 'was included by .pas ...');
  Assert.AreEqual(0, LExcludeIndex, '... but rejected by *_test*');

  Assert.IsFalse(LFilter.AcceptsEx('Notes.txt', LIncludeIndex, LExcludeIndex));
  Assert.AreEqual(-1, LIncludeIndex, 'not included at all');
  Assert.AreEqual(-1, LExcludeIndex, 'exclude never consulted a match');

  // Exclude-only filter: include index stays -1 for accepted inputs.
  LFilter := TWildCardFilter.Create(TArray<string>.Create(), TArray<string>.Create('*backup*'));

  Assert.IsTrue(LFilter.AcceptsEx('Unit1.pas', LIncludeIndex, LExcludeIndex));
  Assert.AreEqual(-1, LIncludeIndex);
  Assert.AreEqual(-1, LExcludeIndex);

  Assert.IsFalse(LFilter.AcceptsEx('Unit1_backup.pas', LIncludeIndex, LExcludeIndex));
  Assert.AreEqual(0, LExcludeIndex);
end;

{ Path mode (wcoPathMode) }

procedure TWildCardMatcherDUnitX.PathModeSegmentStarTest;
begin
  // '*' stays inside one path segment ('\' and '/' both count).
  Assert.IsTrue (TWildCard.Create('*.pas', [wcoPathMode]).Match('Unit1.pas'));
  Assert.IsFalse(TWildCard.Create('*.pas', [wcoPathMode]).Match('src\Unit1.pas'), '* must not cross a backslash');
  Assert.IsFalse(TWildCard.Create('*.pas', [wcoPathMode]).Match('src/Unit1.pas'), '* must not cross a forward slash');

  Assert.IsTrue (TWildCard.Create('src\*.pas', [wcoPathMode]).Match('src\Unit1.pas'));
  Assert.IsFalse(TWildCard.Create('src\*.pas', [wcoPathMode]).Match('src\sub\Unit1.pas'));

  Assert.IsTrue (TWildCard.Create('*\*.pas', [wcoPathMode]).Match('src\Unit1.pas'));
  Assert.IsFalse(TWildCard.Create('*\*.pas', [wcoPathMode]).Match('a\b\Unit1.pas'), 'exactly one directory level');
end;

procedure TWildCardMatcherDUnitX.PathModeGlobstarTest;
begin
  // A run of two or more stars crosses separators.
  Assert.IsTrue(TWildCard.Create('**.pas', [wcoPathMode]).Match('a\b\c\Unit1.pas'));
  Assert.IsTrue(TWildCard.Create('**.pas', [wcoPathMode]).Match('Unit1.pas'));

  Assert.IsTrue (TWildCard.Create('src\**\final.txt', [wcoPathMode]).Match('src\a\b\final.txt'));
  Assert.IsTrue (TWildCard.Create('src\**\final.txt', [wcoPathMode]).Match('src\a\final.txt'));
  Assert.IsFalse(TWildCard.Create('src\**\final.txt', [wcoPathMode]).Match('src\final.txt'),
    'the separators around ** are literal - at least one directory level required');
  Assert.IsFalse(TWildCard.Create('src\**\final.txt', [wcoPathMode]).Match('other\a\final.txt'));
end;

procedure TWildCardMatcherDUnitX.PathModeQuestionMarkTest;
begin
  Assert.IsTrue (TWildCard.Create('a?b', [wcoPathMode]).Match('axb'));
  Assert.IsFalse(TWildCard.Create('a?b', [wcoPathMode]).Match('a\b'), '? must not match a separator');
  Assert.IsFalse(TWildCard.Create('a?b', [wcoPathMode]).Match('a/b'));

  // Without path mode '?' matches separators like any other char.
  Assert.IsTrue(TWildCard.Create('a?b').Match('a\b'));
end;

procedure TWildCardMatcherDUnitX.PathModeTrailingSegmentStarTest;
begin
  Assert.IsTrue (TWildCard.Create('src\*', [wcoPathMode]).Match('src\file.txt'));
  Assert.IsTrue (TWildCard.Create('src\*', [wcoPathMode]).Match('src\'), 'trailing * matches the empty segment');
  Assert.IsFalse(TWildCard.Create('src\*', [wcoPathMode]).Match('src\a\b'), 'trailing * must not cross into deeper segments');
  Assert.IsTrue (TWildCard.Create('src\**', [wcoPathMode]).Match('src\a\b'), 'trailing ** crosses');
end;

procedure TWildCardMatcherDUnitX.PathModeCaseSensitiveComboTest;
begin
  Assert.IsTrue (TWildCard.Create('src\*.pas', [wcoCaseSensitive, wcoPathMode]).Match('src\unit.pas'));
  Assert.IsFalse(TWildCard.Create('src\*.pas', [wcoCaseSensitive, wcoPathMode]).Match('SRC\unit.pas'));
  Assert.IsFalse(TWildCard.Create('src\*.pas', [wcoCaseSensitive, wcoPathMode]).Match('src\sub\unit.pas'));

  Assert.IsTrue(TWildCard.Create('src\*.pas', [wcoPathMode]).Match('SRC\UNIT.PAS'), 'CI + path mode');
end;

procedure TWildCardMatcherDUnitX.PathModeAdhocMatchesRegisteredTest;
const
  PATTERNS: array[0..5] of string = ('*.pas', 'src\*.pas', '**.pas', 'a?b', 'src\**', '*\*.pas');
  INPUTS: array[0..5] of string = ('src\unit.pas', 'src\unit.pas', 'a\b\c.pas', 'a\b', 'src\a\b', 'src\unit.pas');
var
  LIndex: Integer;
  LAdhoc: TWildCard;
  LAdhocResult, LRegisteredResult: Boolean;
begin
  // The interpreting (ad-hoc) and compiled (registered) engines must
  // agree on path-mode semantics.
  LAdhoc := TWildCard.Create([wcoPathMode]);

  for LIndex := 0 to High(PATTERNS) do
  begin
    LAdhocResult := LAdhoc.Match(INPUTS[LIndex], PATTERNS[LIndex]);
    LRegisteredResult := TWildCard.Create(PATTERNS[LIndex], [wcoPathMode]).Match(INPUTS[LIndex]);

    Assert.IsTrue(LAdhocResult = LRegisteredResult,
      Format('engines disagree in path mode: pattern=<%s> input=<%s> ad-hoc=%s registered=%s',
      [PATTERNS[LIndex], INPUTS[LIndex], BoolToStr(LAdhocResult, True), BoolToStr(LRegisteredResult, True)]));
  end;
end;

procedure TWildCardMatcherDUnitX.PathModeOffIsDefaultBehaviourTest;
begin
  // Options [] behaves exactly like the classic Boolean constructors.
  Assert.IsTrue(TWildCard.Create('*.pas', []).Match('src\Unit1.pas'), 'without wcoPathMode, * crosses separators');
  Assert.IsTrue(TWildCard.Create('*.pas', []).Match('SRC\UNIT1.PAS'), 'case-insensitive by default');

  Assert.IsFalse(TWildCard.Create('*.pas').PathMode);
  Assert.IsFalse(TWildCard.Create('*.pas', []).PathMode);
  Assert.IsTrue (TWildCard.Create('*.pas', [wcoPathMode]).PathMode);
  Assert.IsTrue (TWildCard.Create('*.pas', [wcoCaseSensitive]).CaseSensitive);
end;

{ Differential fuzzing helpers }

function FuzzRandomPattern(var ASeed: Cardinal): string;
const
  // '#', '[' and ']' are plain literals - kept in the alphabet so both
  // engines get exercised on them next to the real class syntax '<...>'.
  PATTERN_CHARS: array[0..21] of Char = (
    'a', 'b', 'c', 'A', 'Z', '0', '7', '*', '?', '#', '<', '>', '[', ']', '!', '"', '|', '-', '\', '/', '.', 'x');
var
  LLen, LIndex: Integer;
begin
  LLen := FuzzNextRandom(ASeed, 11); // 0..10 chars
  SetLength(Result, LLen);

  for LIndex := 1 to LLen do
    Result[LIndex] := PATTERN_CHARS[FuzzNextRandom(ASeed, Length(PATTERN_CHARS))];
end;

function FuzzRandomInput(var ASeed: Cardinal): string;
const
  INPUT_CHARS: array[0..15] of Char = (
    'a', 'b', 'c', 'A', 'B', 'Z', '0', '7', '\', '/', '.', 'x', 'y', '#', '[', ']');
var
  LLen, LIndex: Integer;
begin
  LLen := FuzzNextRandom(ASeed, 13); // 0..12 chars
  SetLength(Result, LLen);

  for LIndex := 1 to LLen do
    Result[LIndex] := INPUT_CHARS[FuzzNextRandom(ASeed, Length(INPUT_CHARS))];
end;

function FuzzDerivedInput(const APattern: string; var ASeed: Cardinal): string;
const
  FILL_CHARS: array[0..5] of Char = ('a', 'b', 'Z', '0', '\', '.');
var
  LIndex, LRun, LRunIndex: Integer;
begin
  // Substitutes metacharacters with plausible content so a useful share
  // of derived inputs actually MATCH their pattern - purely random
  // inputs almost never do.
  Result := '';

  for LIndex := 1 to Length(APattern) do
    case APattern[LIndex] of
      '*':
        begin
          LRun := FuzzNextRandom(ASeed, 4); // 0..3 chars
          for LRunIndex := 1 to LRun do
            Result := Result + FILL_CHARS[FuzzNextRandom(ASeed, Length(FILL_CHARS))];
        end;
      '?':
        Result := Result + FILL_CHARS[FuzzNextRandom(ASeed, Length(FILL_CHARS))];
      '#':
        // A literal '#' outside a class, a digit inside a '<##>' run -
        // either guess yields useful inputs.
        if FuzzNextRandom(ASeed, 2) = 0 then
          Result := Result + '#'
        else
          Result := Result + Chr(Ord('0') + Integer(FuzzNextRandom(ASeed, 10)));
      '<', '>', '!', '"', '|':
        ; // drop class/alternation syntax - close enough for fuzzing
    else
      Result := Result + APattern[LIndex];
    end;
end;

procedure TWildCardMatcherDUnitX.DifferentialFuzzTest;
const
  FUZZ_SEED = 20260715;
  FUZZ_ITERATIONS = 2000;
  MODE_COUNT = 4;
  MODE_OPTIONS: array[0..MODE_COUNT - 1] of TWildCardOptions = (
    [], [wcoCaseSensitive], [wcoPathMode], [wcoCaseSensitive, wcoPathMode]);
  MODE_NAMES: array[0..MODE_COUNT - 1] of string = ('CI', 'CS', 'CI+Path', 'CS+Path');
var
  LSeed: Cardinal;
  LIteration, LInputIndex, LMode: Integer;
  LPattern, LInput, LError: string;
  LAdhoc: array[0..MODE_COUNT - 1] of TWildCard;
  LRegistered: TWildCard;
  LInputs: array[0..4] of string;
  LPatternIsValid: Boolean;
  LAdhocResult, LRegisteredResult: Boolean;
begin
  // Differential fuzzing: the registered (compiled) and ad-hoc
  // (interpreting) engines are independent implementations of the same
  // semantics - random patterns and inputs must never make them disagree,
  // in any mode.  Deterministic seed, so a failure is reproducible.
  LSeed := FUZZ_SEED;

  for LMode := 0 to MODE_COUNT - 1 do
    LAdhoc[LMode] := TWildCard.Create(MODE_OPTIONS[LMode]);

  for LIteration := 1 to FUZZ_ITERATIONS do
  begin
    LPattern := FuzzRandomPattern(LSeed);
    LPatternIsValid := TWildCard.ValidatePattern(LPattern, LError);

    LInputs[0] := FuzzRandomInput(LSeed);
    LInputs[1] := FuzzRandomInput(LSeed);
    LInputs[2] := FuzzRandomInput(LSeed);
    LInputs[3] := FuzzDerivedInput(LPattern, LSeed);
    LInputs[4] := '';

    for LMode := 0 to MODE_COUNT - 1 do
    begin
      LRegistered := TWildCard.Create(LPattern, MODE_OPTIONS[LMode]);

      for LInputIndex := 0 to High(LInputs) do
      begin
        LInput := LInputs[LInputIndex];

        LAdhocResult := LAdhoc[LMode].Match(LInput, LPattern);
        LRegisteredResult := LRegistered.Match(LInput);

        Assert.IsTrue(LAdhocResult = LRegisteredResult,
          Format('Engines disagree: mode=%s pattern=<%s> input=<%s> iteration=%d seed=%d ad-hoc=%s registered=%s',
          [MODE_NAMES[LMode], LPattern, LInput, LIteration, FUZZ_SEED,
           BoolToStr(LAdhocResult, True), BoolToStr(LRegisteredResult, True)]));

        // A pattern rejected by ValidatePattern must never match anything.
        if not LPatternIsValid then
          Assert.IsFalse(LRegisteredResult,
            Format('Malformed pattern matched: pattern=<%s> input=<%s> (%s)', [LPattern, LInput, LError]));
      end;
    end;
  end;
end;

{ Lint helpers }

function DiagnosticsContain(const ADiagnostics: TArray<TWildCardDiagnostic>; const AKind: TWildCardDiagnosticKind;
  const AMessagePart: string): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(ADiagnostics) do
    if (ADiagnostics[LIndex].Kind = AKind) and ADiagnostics[LIndex].Message.Contains(AMessagePart) then
      Exit(True);

  Result := False;
end;

{ Lint: compiler-style warnings and hints }

procedure TWildCardMatcherDUnitX.LintPatternMalformedTest;
var
  LDiagnostics: TArray<TWildCardDiagnostic>;
begin
  LDiagnostics := TWildCard.LintPattern('<abc');

  Assert.AreEqual(1, Length(LDiagnostics));
  Assert.IsTrue(LDiagnostics[0].Kind = wdkWarning);
  Assert.IsTrue(LDiagnostics[0].Message.Contains('never matches'), LDiagnostics[0].Message);
  Assert.AreEqual('<abc', LDiagnostics[0].Pattern);
  Assert.AreEqual(-1, LDiagnostics[0].PatternIndex, 'no list context for LintPattern');
end;

procedure TWildCardMatcherDUnitX.LintPatternStarRunTest;
begin
  // '**' without path mode collapses to '*' - worth a hint.
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('**.pas'), wdkHint, 'collapse'));

  // With path mode '**' is meaningful - no diagnostics at all.
  Assert.AreEqual(0, Length(TWildCard.LintPattern('**.pas', [wcoPathMode])));

  // But three or more stars behave like two.
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('***.pas', [wcoPathMode]), wdkHint, 'behaves the same'));

  // A clean everyday mask has nothing to report.
  Assert.AreEqual(0, Length(TWildCard.LintPattern('*.pas')));
end;

procedure TWildCardMatcherDUnitX.LintPatternClassAndAlternationTest;
begin
  // Empty range (low > high).
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('<z-a>'), wdkHint, 'empty (low > high)'));

  // Duplicate alternatives; case-awareness follows the options.
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('<"a"|"a">'), wdkHint, 'duplicate alternative'));
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('<"a"|"A">'), wdkHint, 'duplicate alternative'),
    'CI: a and A are the same alternative');
  Assert.AreEqual(0, Length(TWildCard.LintPattern('<"a"|"A">', [wcoCaseSensitive])),
    'CS: a and A are distinct alternatives');

  // Degenerate alternations.
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('foo<"">'), wdkHint, 'no effect'));
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('<!"">'), wdkWarning, 'never matches'));
end;

procedure TWildCardMatcherDUnitX.LintPatternEmptyTest;
begin
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern(''), wdkHint, 'empty'));
end;

procedure TWildCardMatcherDUnitX.LintSetDuplicatesAndShadowingTest;
var
  LMask: TWildCard;
begin
  // Case-insensitive: '*.pas' and '*.PAS' are the same pattern.
  LMask := TWildCard.Create(TArray<string>.Create('*.pas', '*.PAS'));
  Assert.IsTrue(DiagnosticsContain(LMask.Lint, wdkWarning, 'duplicates pattern #0'));

  // Case-sensitive: they are distinct - nothing to report.
  LMask := TWildCard.Create(TArray<string>.Create('*.pas', '*.PAS'), True);
  Assert.AreEqual(0, Length(LMask.Lint));

  // A match-everything pattern makes later ones unreachable.
  LMask := TWildCard.Create(TArray<string>.Create('*', '*.pas'));
  Assert.IsTrue(DiagnosticsContain(LMask.Lint, wdkWarning, 'unreachable'));

  // Clean set.
  LMask := TWildCard.Create(TArray<string>.Create('*.pas', '*.dpr', '*.inc'));
  Assert.AreEqual(0, Length(LMask.Lint));
end;

procedure TWildCardMatcherDUnitX.FilterLintIncludeAlsoExcludedTest;
var
  LFilter: TWildCardFilter;
  LDiagnostics: TArray<TWildCardDiagnostic>;
  LIndex: Integer;
  LFound: Boolean;
begin
  // The classic mistake: filter *.bat in AND out - the include is dead.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas', '*.bat'),
    TArray<string>.Create('*.bat'));

  LDiagnostics := LFilter.Lint;
  Assert.IsTrue(DiagnosticsContain(LDiagnostics, wdkWarning, 'can never accept anything'));

  LFound := False;

  for LIndex := 0 to High(LDiagnostics) do
    if LDiagnostics[LIndex].Message.Contains('can never accept anything') then
    begin
      Assert.IsTrue(LDiagnostics[LIndex].List = wdlIncludes, 'reported against the include list');
      Assert.AreEqual(1, LDiagnostics[LIndex].PatternIndex, 'the *.bat include is pattern #1');
      LFound := True;
    end;

  Assert.IsTrue(LFound);

  // Case-insensitive filters catch the mismatched-case variant too.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.BAT'),
    TArray<string>.Create('*.bat'));
  Assert.IsTrue(DiagnosticsContain(LFilter.Lint, wdkWarning, 'can never accept anything'));

  // Case-sensitive: '*.BAT' vs '*.bat' are genuinely different patterns.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.BAT'),
    TArray<string>.Create('*.bat'), [wcoCaseSensitive]);
  Assert.AreEqual(0, Length(LFilter.Lint));
end;

procedure TWildCardMatcherDUnitX.FilterLintMatchAllTest;
var
  LFilter: TWildCardFilter;
begin
  // A match-everything exclude rejects every input.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas'),
    TArray<string>.Create('*'));
  Assert.IsTrue(DiagnosticsContain(LFilter.Lint, wdkWarning, 'the filter accepts nothing'));

  // A match-everything include makes the other includes redundant.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*', '*.pas'),
    TArray<string>.Create('*.tmp'));
  Assert.IsTrue(DiagnosticsContain(LFilter.Lint, wdkHint, 'redundant'));

  // In path mode a single '*' does NOT match everything - no warnings.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('src\*.pas'),
    TArray<string>.Create('*'), [wcoPathMode]);
  Assert.IsFalse(DiagnosticsContain(LFilter.Lint, wdkWarning, 'accepts nothing'));
end;

procedure TWildCardMatcherDUnitX.FilterLintCleanFilterTest;
var
  LFilter: TWildCardFilter;
begin
  // A sensible everyday filter produces no diagnostics at all.
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas', '*.dpr', '*.inc'),
    TArray<string>.Create('*\__history\*', '*backup*', '*.tmp'));

  Assert.AreEqual(0, Length(LFilter.Lint));
end;

procedure TWildCardMatcherDUnitX.LintPreAngleBracketSyntaxTest;
begin
  // The pre-'<...>' alternation syntax is now plain text containing '"' and
  // '|', which no Windows path can contain - it would silently never match.
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('*["3rdParty"|"ThirdParty"]*'), wdkWarning, 'outside a'));
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('a>b'), wdkWarning, 'outside a'));
  Assert.IsTrue(DiagnosticsContain(TWildCard.LintPattern('a|b'), wdkWarning, 'outside a'));

  // Inside a class those characters are syntax, not literals.
  Assert.AreEqual(0, Length(TWildCard.LintPattern('*<"3rdParty"|"ThirdParty">*')));
  Assert.AreEqual(0, Length(TWildCard.LintPattern('<"a|b">')), '| inside a quoted alternative');

  // No false positives on valid Windows paths containing '[', ']' or '#'.
  Assert.AreEqual(0, Length(TWildCard.LintPattern('C:\Temp\Foo[ab].txt')));
  Assert.AreEqual(0, Length(TWildCard.LintPattern('Track #01.mp3')));
end;

{ IsWildCard and plain paths }

procedure TWildCardMatcherDUnitX.IsWildCardTest;
begin
  // Plain text - any valid Windows path is not a wildcard pattern.
  Assert.IsFalse(TWildCard.IsWildCard(''));
  Assert.IsFalse(TWildCard.IsWildCard('C:\Temp\Foo.txt'));
  Assert.IsFalse(TWildCard.IsWildCard('C:\Temp\Foo[ab].txt'), '[ and ] are legal file name characters');
  Assert.IsFalse(TWildCard.IsWildCard('Track #01.mp3'), '# is a legal file name character');
  Assert.IsFalse(TWildCard.IsWildCard('\\server\share\dir\file.txt'));
  Assert.IsFalse(TWildCard.IsWildCard('C:/forward/slashes.txt'));
  Assert.IsFalse(TWildCard.IsWildCard('Files (x86)\{GUID}\a,b;c=d+e@f~g''h.txt'));

  // Literal outside a class, so not wildcard syntax either.
  Assert.IsFalse(TWildCard.IsWildCard('a|b'));
  Assert.IsFalse(TWildCard.IsWildCard('a>b'));
  Assert.IsFalse(TWildCard.IsWildCard('say "hi"'));

  // Wildcards.
  Assert.IsTrue(TWildCard.IsWildCard('C:\Temp\*oo.txt'));
  Assert.IsTrue(TWildCard.IsWildCard('Foo?.txt'));
  Assert.IsTrue(TWildCard.IsWildCard('*'));
  Assert.IsTrue(TWildCard.IsWildCard('Test_<###>.log'));
  Assert.IsTrue(TWildCard.IsWildCard('b<ae>ll'));
  Assert.IsTrue(TWildCard.IsWildCard('*<"3rdParty"|"ThirdParty">\*'));

  // Malformed is still "wildcard" - ValidatePattern is the syntax check.
  Assert.IsTrue(TWildCard.IsWildCard('Foo<abc'));
end;

procedure TWildCardMatcherDUnitX.IsWildCardFalseMeansPatternMatchesItselfTest;
const
  SEED = 20261013;
  ITERATIONS = 3000;
  MODE_OPTIONS: array[0..3] of TWildCardOptions = ([], [wcoCaseSensitive], [wcoPathMode], [wcoCaseSensitive, wcoPathMode]);
var
  LSeed: Cardinal;
  LIteration, LIndex, LLen, LMode, LChecked: Integer;
  LText: string;
begin
  // The contract behind IsWildCard: text it calls plain is matched
  // literally by both engines in every mode - so it always matches itself.
  // Random strings over all printable ASCII (metacharacters included) plus
  // a few non-ASCII letters; the property is checked whenever IsWildCard
  // says False.
  LSeed := SEED;
  LChecked := 0;

  for LIteration := 1 to ITERATIONS do
  begin
    LLen := FuzzNextRandom(LSeed, 16);
    SetLength(LText, LLen);

    for LIndex := 1 to LLen do
      if FuzzNextRandom(LSeed, 10) = 0 then
        LText[LIndex] := Char($00E4 + FuzzNextRandom(LSeed, 3)) // ä, å, æ
      else
        LText[LIndex] := Char(32 + FuzzNextRandom(LSeed, 95));

    if TWildCard.IsWildCard(LText) then
      Continue;

    Inc(LChecked);

    for LMode := 0 to High(MODE_OPTIONS) do
    begin
      Assert.IsTrue(TWildCard.Create(MODE_OPTIONS[LMode]).Match(LText, LText),
        Format('ad-hoc engine: plain text <%s> does not match itself (mode %d)', [LText, LMode]));
      Assert.IsTrue(TWildCard.Create(LText, MODE_OPTIONS[LMode]).Match(LText),
        Format('registered engine: plain text <%s> does not match itself (mode %d)', [LText, LMode]));
    end;
  end;

  Assert.IsTrue(LChecked > ITERATIONS div 4, Format('only %d plain strings generated - fuzz alphabet too wildcard-heavy', [LChecked]));
end;

{ API additions }

procedure TWildCardMatcherDUnitX.PatternMatchesEverythingTest;
begin
  Assert.IsTrue (TWildCard.PatternMatchesEverything('*'));
  Assert.IsTrue (TWildCard.PatternMatchesEverything('**'));
  Assert.IsFalse(TWildCard.PatternMatchesEverything(''));
  Assert.IsFalse(TWildCard.PatternMatchesEverything('*.pas'));

  // Path mode: a single '*' cannot cross separators, '**' can.
  Assert.IsFalse(TWildCard.PatternMatchesEverything('*', True));
  Assert.IsTrue (TWildCard.PatternMatchesEverything('**', True));
end;

procedure TWildCardMatcherDUnitX.PatternCountAndFilterCountsTest;
var
  LMask: TWildCard;
  LFilter: TWildCardFilter;
begin
  LMask := TWildCard.Create(TArray<string>.Create('*.pas', '*.dpr'));
  Assert.AreEqual(2, LMask.PatternCount);

  LMask := Default(TWildCard);
  Assert.AreEqual(0, LMask.PatternCount);

  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas', '*.dpr', '*.inc'),
    TArray<string>.Create('*.tmp'));
  Assert.AreEqual(3, LFilter.IncludeCount);
  Assert.AreEqual(1, LFilter.ExcludeCount);
end;

procedure TWildCardMatcherDUnitX.FilterBulkFilterTest;
var
  LFilter: TWildCardFilter;
  LAccepted: TArray<string>;
begin
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('*.pas'),
    TArray<string>.Create('*_test*'));

  LAccepted := LFilter.Filter(TArray<string>.Create(
    'Unit1.pas', 'Notes.txt', 'Unit2.pas', 'Unit_test.pas', 'Unit3.pas'));

  Assert.AreEqual(3, Length(LAccepted));
  Assert.AreEqual('Unit1.pas', LAccepted[0], 'input order preserved');
  Assert.AreEqual('Unit2.pas', LAccepted[1]);
  Assert.AreEqual('Unit3.pas', LAccepted[2]);

  // Empty input list.
  LAccepted := LFilter.Filter(nil);
  Assert.AreEqual(0, Length(LAccepted));
end;

{ Additional matcher coverage }

procedure TWildCardMatcherDUnitX.FileNameCharactersAreLiteralTest;
begin
  // '[', ']' and '#' are legal in Windows file names, so the syntax never
  // gives them a meaning - a path containing them matches itself.
  Assert.IsTrue (TWildCard.Create.Match('C:\Temp\Foo[ab].txt', 'C:\Temp\Foo[ab].txt'));
  Assert.IsTrue (TWildCard.Create('C:\Temp\Foo[ab].txt').Match('C:\Temp\Foo[ab].txt'));
  Assert.IsFalse(TWildCard.Create.Match('C:\Temp\Fooa.txt', 'C:\Temp\Foo[ab].txt'), '[ab] is not a class');

  Assert.IsTrue (TWildCard.Create.Match('Track #01.mp3', 'Track #01.mp3'));
  Assert.IsTrue (TWildCard.Create('Track #01.mp3').Match('Track #01.mp3'));

  // Literal '#' next to a digit run - the README example.
  Assert.IsTrue (TWildCard.Create.Match('Track #01.mp3', 'Track #<##>.mp3'));
  Assert.IsFalse(TWildCard.Create.Match('Track 001.mp3', 'Track #<##>.mp3'));

  // Brackets combine with wildcards as plain text.
  Assert.IsTrue (TWildCard.Create('*[old]*').Match('notes[old].txt'));
  Assert.IsFalse(TWildCard.Create('*[old]*').Match('notes_old.txt'));
end;

procedure TWildCardMatcherDUnitX.UnicodeNonAsciiSimdCandidateTest;
const
  A_UML_LOWER = #$00E4; // ä
  A_UML_UPPER = #$00C4; // Ä
begin
  // Pins the SIMD candidate rule: a non-ASCII input char must reach the
  // scalar Unicode ToUpper check instead of being skipped by the
  // two-value (ASCII upper/lower) scan - 'ä' equals NEITHER scan value
  // of the pattern char 'Ä', only FastToUpper makes them meet.
  // Tail-anchor path:
  Assert.IsTrue(TWildCard.Create.Match('x' + A_UML_LOWER, '*' + A_UML_UPPER), 'ad-hoc engine, tail anchor');
  Assert.IsTrue(TWildCard.Create('*' + A_UML_UPPER).Match('x' + A_UML_LOWER), 'registered engine, tail anchor');

  // First-char-skip path ('?' after the literal defeats the tail anchor):
  Assert.IsTrue(TWildCard.Create.Match('x' + A_UML_LOWER + 'b', '*' + A_UML_UPPER + '?'), 'ad-hoc skip loop');
  Assert.IsTrue(TWildCard.Create('*' + A_UML_UPPER + '?').Match('x' + A_UML_LOWER + 'b'), 'registered skip loop');

  // Case-sensitive: ordinal compare only.
  Assert.IsFalse(TWildCard.Create(True).Match('x' + A_UML_LOWER, '*' + A_UML_UPPER));
  Assert.IsFalse(TWildCard.Create('*' + A_UML_UPPER, True).Match('x' + A_UML_LOWER));
end;

procedure TWildCardMatcherDUnitX.LongLiteralPatternsTest;
const
  LONG_TAIL = 'abcdefghijklmnopqrst'; // 20 chars - exercises SIMD-width compares
begin
  // Tail anchor with a literal longer than one vector block.
  Assert.IsTrue (TWildCard.Create.Match('XX' + LONG_TAIL, '*' + LONG_TAIL));
  Assert.IsTrue (TWildCard.Create.Match('XX' + UpperCase(LONG_TAIL), '*' + LONG_TAIL), 'CI');
  Assert.IsFalse(TWildCard.Create.Match('XX' + Copy(LONG_TAIL, 1, 19) + 'X', '*' + LONG_TAIL),
    'mismatch on the last char of a long tail');

  Assert.IsTrue (TWildCard.Create('*' + LONG_TAIL, True).Match('XX' + LONG_TAIL));
  Assert.IsFalse(TWildCard.Create('*' + LONG_TAIL, True).Match('XX' + UpperCase(LONG_TAIL)), 'CS');

  // Long literal in the middle (skip loop + literal run compare).
  Assert.IsTrue (TWildCard.Create.Match('junk' + LONG_TAIL + 'junk.pas', '*' + LONG_TAIL + '*.pas'));
  Assert.IsFalse(TWildCard.Create.Match('junk' + Copy(LONG_TAIL, 1, 10) + 'junk.pas', '*' + LONG_TAIL + '*.pas'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltUnicodeCaseInsensitiveTest;
const
  A_UMLAUT_LOWER = #$00E4; // ä
  A_UMLAUT_UPPER = #$00C4; // Ä
begin
  Assert.IsTrue (TWildCard.Create.Match(A_UMLAUT_UPPER, '<"' + A_UMLAUT_LOWER + '">'),
    'CI: quoted alternative must match across Unicode case');
  Assert.IsTrue (TWildCard.Create('<"' + A_UMLAUT_LOWER + '">').Match(A_UMLAUT_UPPER));

  Assert.IsFalse(TWildCard.Create(True).Match(A_UMLAUT_UPPER, '<"' + A_UMLAUT_LOWER + '">'), 'CS');
  Assert.IsFalse(TWildCard.Create('<"' + A_UMLAUT_LOWER + '">', True).Match(A_UMLAUT_UPPER));
end;

procedure TWildCardMatcherDUnitX.PathModeClassAndQuotedAltSeparatorsTest;
begin
  // Quoted alternatives are literal - a separator inside one matches
  // normally even in path mode.
  Assert.IsTrue(TWildCard.Create('<"a\b">', [wcoPathMode]).Match('a\b'));

  // Character classes are evaluated normally in path mode too - an
  // explicit separator member may match one.
  Assert.IsTrue(TWildCard.Create('a<\a>b', [wcoPathMode]).Match('a\b'),
    'class with an explicit backslash member matches a separator');

  // ...but '?' and '*' never do.
  Assert.IsFalse(TWildCard.Create('a?b', [wcoPathMode]).Match('a\b'));
end;

procedure TWildCardMatcherDUnitX.PathModeMixedSeparatorsTest;
begin
  // Globstar crosses BOTH separator styles.
  Assert.IsTrue(TWildCard.Create('**.pas', [wcoPathMode]).Match('a/b\c.pas'));

  // A single segment star crosses neither.
  Assert.IsFalse(TWildCard.Create('*', [wcoPathMode]).Match('a/b'));
  Assert.IsFalse(TWildCard.Create('*', [wcoPathMode]).Match('a\b'));
  Assert.IsTrue (TWildCard.Create('*', [wcoPathMode]).Match('ab'));
end;

procedure TWildCardMatcherDUnitX.FilterPathModeOptionsTest;
var
  LFilter: TWildCardFilter;
begin
  LFilter := TWildCardFilter.Create(
    TArray<string>.Create('src\*.pas'),
    TArray<string>.Create('src\*_test.pas'), [wcoPathMode]);

  Assert.IsTrue(LFilter.PathMode);
  Assert.IsFalse(LFilter.CaseSensitive);

  Assert.IsTrue (LFilter.Accepts('src\Unit1.pas'));
  Assert.IsFalse(LFilter.Accepts('src\deep\Unit1.pas'), 'include * must not cross the separator');
  Assert.IsFalse(LFilter.Accepts('src\Unit1_test.pas'), 'excluded');
end;

{ Wildcard combinations - helpers }

function OptionsText(const AOptions: TWildCardOptions): string;
begin
  if wcoCaseSensitive in AOptions then
    Result := 'CS'
  else
    Result := 'CI';

  if wcoPathMode in AOptions then
    Result := Result + '+Path';
end;

// Checks one case in both engines: ad-hoc (interpreting) and registered (compiled).
procedure CheckMatch(const APattern, AInput: string; const AExpected: Boolean; const AOptions: TWildCardOptions = []);
begin
  Assert.AreEqual(AExpected, TWildCard.Create(AOptions).Match(AInput, APattern),
    Format('ad-hoc %s: pattern <%s>, input <%s>', [OptionsText(AOptions), APattern, AInput]));
  Assert.AreEqual(AExpected, TWildCard.Create(APattern, AOptions).Match(AInput),
    Format('registered %s: pattern <%s>, input <%s>', [OptionsText(AOptions), APattern, AInput]));
end;

// Same, in all four case / path mode combinations - for cases whose result depends on neither.
procedure CheckMatchAllModes(const APattern, AInput: string; const AExpected: Boolean);
begin
  CheckMatch(APattern, AInput, AExpected, []);
  CheckMatch(APattern, AInput, AExpected, [wcoCaseSensitive]);
  CheckMatch(APattern, AInput, AExpected, [wcoPathMode]);
  CheckMatch(APattern, AInput, AExpected, [wcoCaseSensitive, wcoPathMode]);
end;

{ Wildcard combinations }

procedure TWildCardMatcherDUnitX.StarAndQuestionCombinationsTest;
begin
  CheckMatchAllModes('*?', '', False);
  CheckMatchAllModes('*?', 'a', True);
  CheckMatchAllModes('*?', 'ab', True);

  CheckMatchAllModes('?*', '', False);
  CheckMatchAllModes('?*', 'a', True);

  CheckMatchAllModes('?*?', 'a', False);
  CheckMatchAllModes('?*?', 'ab', True);
  CheckMatchAllModes('?*?', 'abc', True);

  CheckMatchAllModes('*?*', '', False);
  CheckMatchAllModes('*?*', 'x', True);

  CheckMatchAllModes('??*??', 'abc', False);
  CheckMatchAllModes('??*??', 'abcd', True);
  CheckMatchAllModes('??*??', 'abcdefg', True);

  CheckMatchAllModes('a*?b', 'ab', False);
  CheckMatchAllModes('a*?b', 'axb', True);
  CheckMatchAllModes('a*?b', 'axxb', True);
  CheckMatchAllModes('a*?b', 'abb', True);

  CheckMatchAllModes('a?*b', 'ab', False);
  CheckMatchAllModes('a?*b', 'acb', True);

  CheckMatchAllModes('*a?', 'ab', True);
  CheckMatchAllModes('*a?', 'a', False);
  CheckMatchAllModes('*a?', 'xxaz', True);
  CheckMatchAllModes('*a?', 'xxa', False);
  CheckMatchAllModes('*a?', 'aa', True);

  CheckMatchAllModes('?a*', 'ba', True);
  CheckMatchAllModes('?a*', 'a', False);
  CheckMatchAllModes('?a*', 'aa', True);

  CheckMatchAllModes('***?***', '', False);
  CheckMatchAllModes('***?***', 'q', True);
end;

procedure TWildCardMatcherDUnitX.StarAndSequenceCombinationsTest;
begin
  CheckMatchAllModes('*<#>', '', False);
  CheckMatchAllModes('*<#>', 'a1', True);
  CheckMatchAllModes('*<#>', '1a', False);
  CheckMatchAllModes('*<#>', '1', True);

  CheckMatchAllModes('<#>*', '1', True);
  CheckMatchAllModes('<#>*', '1abc', True);
  CheckMatchAllModes('<#>*', 'a1', False);

  CheckMatchAllModes('*<#>*', 'abc', False);
  CheckMatchAllModes('*<#>*', 'ab3c', True);

  CheckMatchAllModes('*<##>*', 'a1b2', False);
  CheckMatchAllModes('*<##>*', 'a12', True);

  CheckMatchAllModes('*<#>*<#>*', 'a1b2', True);
  CheckMatchAllModes('*<#>*<#>*', 'a12', True);
  CheckMatchAllModes('*<#>*<#>*', 'a1', False);

  CheckMatchAllModes('*<#?#>*', 'x1-2y', True);
  CheckMatchAllModes('*<#?#>*', 'x12y', False);

  CheckMatchAllModes('<?>*<?>', 'a', False);
  CheckMatchAllModes('<?>*<?>', 'ab', True);

  CheckMatchAllModes('*<???>', 'ab', False);
  CheckMatchAllModes('*<???>', 'abc', True);
  CheckMatchAllModes('*<???>', 'abcd', True);

  CheckMatchAllModes('<###>*.log', '123.log', True);
  CheckMatchAllModes('<###>*.log', '12.log', False);
  CheckMatchAllModes('<###>*.log', '1234.log', True);
  CheckMatchAllModes('<###>*.log', '123x.log', True);

  CheckMatchAllModes('*_<####>-<##>-<##>.txt', 'backup_2026-10-14.txt', True);
  CheckMatchAllModes('*_<####>-<##>-<##>.txt', 'backup_2026-1-14.txt', False);
  CheckMatchAllModes('*_<####>-<##>-<##>.txt', 'a_b_2026-10-14.txt', True);

  CheckMatchAllModes('<?##>*<?##>', 'x12y34', True);
  CheckMatchAllModes('<?##>*<?##>', 'x12x12', True);
  CheckMatchAllModes('<?##>*<?##>', 'x12', False);
end;

procedure TWildCardMatcherDUnitX.StarAndClassCombinationsTest;
begin
  CheckMatchAllModes('*<ab>', 'xa', True);
  CheckMatchAllModes('*<ab>', 'xc', False);
  CheckMatchAllModes('*<ab>', '', False);

  CheckMatchAllModes('<ab>*', 'a', True);
  CheckMatchAllModes('<ab>*', 'bxyz', True);
  CheckMatchAllModes('<ab>*', 'c', False);

  CheckMatchAllModes('*<!ab>', 'aac', True);
  CheckMatchAllModes('*<!ab>', 'aab', False);
  CheckMatchAllModes('*<!ab>', '', False);

  // Order matters: the a-c character must come before the x-z one.
  CheckMatchAllModes('*<a-c>*<x-z>*', 'qqbqqyqq', True);
  CheckMatchAllModes('*<a-c>*<x-z>*', 'qqyqqbqq', False);

  CheckMatchAllModes('<a-c>?<a-c>', 'aXc', True);
  CheckMatchAllModes('<a-c>?<a-c>', 'aXd', False);

  // '<0-9>' is a range class, not a sequence - same result as '<#>' though.
  CheckMatchAllModes('*<0-9><0-9>', 'v12', True);
  CheckMatchAllModes('*<0-9><0-9>', 'v1', False);

  CheckMatchAllModes('<!0-9>*', 'a1', True);
  CheckMatchAllModes('<!0-9>*', '1a', False);
  CheckMatchAllModes('<!0-9>*', '', False);

  CheckMatchAllModes('*<>>*', 'a>b', True);
  CheckMatchAllModes('*<>>*', 'ab', False);

  CheckMatchAllModes('*<-x>*', 'a-b', True);
  CheckMatchAllModes('*<-x>*', 'axb', True);
  CheckMatchAllModes('*<-x>*', 'ab', False);

  CheckMatchAllModes('<a->*', '-z', True);
  CheckMatchAllModes('<a->*', 'a', True);
  CheckMatchAllModes('<a->*', 'b', False);
end;

procedure TWildCardMatcherDUnitX.StarAndAlternationCombinationsTest;
begin
  CheckMatchAllModes('*<"ab"|"cd">', 'xxab', True);
  CheckMatchAllModes('*<"ab"|"cd">', 'xxcd', True);
  CheckMatchAllModes('*<"ab"|"cd">', 'xxad', False);
  CheckMatchAllModes('*<"ab"|"cd">', 'ab', True);

  CheckMatchAllModes('<"ab"|"cd">*', 'abzzz', True);
  CheckMatchAllModes('<"ab"|"cd">*', 'zab', False);

  CheckMatchAllModes('*<"ab"|"cd">*', 'zzcdzz', True);
  CheckMatchAllModes('*<"ab"|"cd">*', 'zacbdz', False);

  // Overlapping alternatives: both lengths must be tried.
  CheckMatchAllModes('*<"a"|"ab">c', 'ac', True);
  CheckMatchAllModes('*<"a"|"ab">c', 'abc', True);
  CheckMatchAllModes('*<"a"|"ab">c', 'xabc', True);
  CheckMatchAllModes('*<"a"|"ab">c', 'abd', False);

  CheckMatchAllModes('<"a"|"ab">b', 'ab', True);
  CheckMatchAllModes('<"a"|"ab">b', 'abb', True);
  CheckMatchAllModes('<"a"|"ab">b', 'ac', False);

  // Empty alternative: optional text.
  CheckMatchAllModes('<""|"x">y', 'y', True);
  CheckMatchAllModes('<""|"x">y', 'xy', True);
  CheckMatchAllModes('<""|"x">y', 'xxy', False);

  CheckMatchAllModes('*<""|"x">', '', True);
  CheckMatchAllModes('*<""|"x">', 'anything', True);
  CheckMatchAllModes('<"x"|"">*', '', True);

  // Negated alternation after a star: the LAST two characters must not be "ab".
  CheckMatchAllModes('*<!"ab">', 'cd', True);
  CheckMatchAllModes('*<!"ab">', 'xab', False);
  CheckMatchAllModes('*<!"ab">', 'a', False);

  CheckMatchAllModes('<!"ab">*', 'ac', True);
  CheckMatchAllModes('<!"ab">*', 'abc', False);
  CheckMatchAllModes('<!"ab">*', 'a', False);

  CheckMatchAllModes('*.<"pas"|"dpr"|"dpk">', 'Unit1.pas', True);
  CheckMatchAllModes('*.<"pas"|"dpr"|"dpk">', 'P.dpk', True);
  CheckMatchAllModes('*.<"pas"|"dpr"|"dpk">', 'x.dcu', False);
  CheckMatchAllModes('*.<"pas"|"dpr"|"dpk">', 'x.pas.bak', False);

  CheckMatchAllModes('*<"\"|"/">*', 'a/b', True);
  CheckMatchAllModes('*<"\"|"/">*', 'a\b', True);
  CheckMatchAllModes('*<"\"|"/">*', 'ab', False);
end;

procedure TWildCardMatcherDUnitX.FixedWidthTokenCombinationsTest;
begin
  // No star: every token consumes a fixed amount, side by side.
  CheckMatchAllModes('?<a>?', 'xay', True);
  CheckMatchAllModes('?<a>?', 'xby', False);

  CheckMatchAllModes('<#><a-z>?', '1bZ', True);
  CheckMatchAllModes('<#><a-z>?', 'a1Z', False);

  CheckMatchAllModes('<?#><!#>', 'x1y', True);
  CheckMatchAllModes('<?#><!#>', 'x12', True);
  CheckMatchAllModes('<?#><!#>', 'x1#', False);

  CheckMatchAllModes('<"ab"|"c"><#>', 'ab1', True);
  CheckMatchAllModes('<"ab"|"c"><#>', 'c1', True);
  CheckMatchAllModes('<"ab"|"c"><#>', 'abc1', False);

  CheckMatchAllModes('<#><"ab"|"c">', '1ab', True);
  CheckMatchAllModes('<#><"ab"|"c">', '1c', True);
  CheckMatchAllModes('<#><"ab"|"c">', '1', False);

  CheckMatchAllModes('<!"x"><?>', 'ab', True);
  CheckMatchAllModes('<!"x"><?>', 'xb', False);

  CheckMatchAllModes('<##><!"00">', '1201', True);
  CheckMatchAllModes('<##><!"00">', '1200', False);

  CheckMatchAllModes('a<#>b<?>c', 'a1bZc', True);
  CheckMatchAllModes('a<#>b<?>c', 'a1bc', False);

  CheckMatchAllModes('<a-c><#><!a-c>', 'b5x', True);
  CheckMatchAllModes('<a-c><#><!a-c>', 'b5c', False);
end;

procedure TWildCardMatcherDUnitX.NegatedFormCombinationsTest;
begin
  CheckMatchAllModes('<!a><!b>', 'ba', True);
  CheckMatchAllModes('<!a><!b>', 'ab', False);
  CheckMatchAllModes('<!a><!b>', 'cc', True);

  CheckMatchAllModes('<!a-z>*<!a-z>', '1x2', True);
  CheckMatchAllModes('<!a-z>*<!a-z>', '1', False);
  CheckMatchAllModes('<!a-z>*<!a-z>', 'ax1', False);

  // Negated alternation consumes the length of the longest alternative (3).
  CheckMatchAllModes('<!"ab"|"cde">*', 'xyz', True);
  CheckMatchAllModes('<!"ab"|"cde">*', 'abz', False);
  CheckMatchAllModes('<!"ab"|"cde">*', 'cdx', True);
  CheckMatchAllModes('<!"ab"|"cde">*', 'cde', False);
  CheckMatchAllModes('<!"ab"|"cde">*', 'xy', False);

  // '<!#>' is a class (anything but '#'), not a negated digit.
  CheckMatchAllModes('*<!#>', '12a', True);
  CheckMatchAllModes('*<!#>', '123', True);
  CheckMatchAllModes('*<!#>', '12#', False);

  CheckMatchAllModes('<!?>', 'a', True);
  CheckMatchAllModes('<!?>', '?', False);

  // Double negation by two tokens.
  CheckMatchAllModes('<!"a"><!b>', 'xy', True);
  CheckMatchAllModes('<!"a"><!b>', 'ay', False);
  CheckMatchAllModes('<!"a"><!b>', 'xb', False);
end;

procedure TWildCardMatcherDUnitX.CaseSensitivityCombinationsTest;
begin
  CheckMatch('*.<"PAS"|"dpr">', 'a.pas', True);
  CheckMatch('*.<"PAS"|"dpr">', 'a.DPR', True);
  CheckMatch('*.<"PAS"|"dpr">', 'a.pas', False, [wcoCaseSensitive]);
  CheckMatch('*.<"PAS"|"dpr">', 'a.PAS', True, [wcoCaseSensitive]);
  CheckMatch('*.<"PAS"|"dpr">', 'a.dpr', True, [wcoCaseSensitive]);
  CheckMatch('*.<"PAS"|"dpr">', 'a.DPR', False, [wcoCaseSensitive]);

  CheckMatch('<a-c>x', 'BX', True);
  CheckMatch('<a-c>x', 'BX', False, [wcoCaseSensitive]);
  CheckMatch('<a-c>x', 'bx', True, [wcoCaseSensitive]);
  CheckMatch('<a-c>x', 'bX', False, [wcoCaseSensitive]);

  CheckMatch('<!a>*', 'A', False);
  CheckMatch('<!a>*', 'A', True, [wcoCaseSensitive]);

  CheckMatch('<!"ab">?', 'ABc', False);
  CheckMatch('<!"ab">?', 'ABc', True, [wcoCaseSensitive]);

  CheckMatch('<"Ab">', 'aB', True);
  CheckMatch('<"Ab">', 'aB', False, [wcoCaseSensitive]);

  CheckMatch('*<?#>X', 'q1x', True);
  CheckMatch('*<?#>X', 'q1x', False, [wcoCaseSensitive]);

  // Case never matters for digits, '?' and separators.
  CheckMatchAllModes('<?##>', 'Z12', True);
  CheckMatchAllModes('<?##>', 'z12', True);
end;

procedure TWildCardMatcherDUnitX.PathModeCombinationsTest;
begin
  CheckMatch('*<#>', 'a\1', True);
  CheckMatch('*<#>', 'a\1', False, [wcoPathMode]);
  CheckMatch('**<#>', 'a\1', True, [wcoPathMode]);

  CheckMatch('<?#>', '\1', True);
  CheckMatch('<?#>', '\1', False, [wcoPathMode]);

  CheckMatch('*\<"Win32"|"Win64">\*', 'Out\Win32\x.exe', True, [wcoPathMode]);
  CheckMatch('*\<"Win32"|"Win64">\*', 'Out\Win32\sub\x.exe', False, [wcoPathMode]);
  CheckMatch('*\<"Win32"|"Win64">\*', 'Out\Win32\sub\x.exe', True);
  CheckMatch('*\<"Win32"|"Win64">\*', 'a\b\Win32\x', False, [wcoPathMode]);
  CheckMatch('**\<"Win32"|"Win64">\*', 'a\b\Win32\x', True, [wcoPathMode]);

  CheckMatch('*.<"pas"|"inc">', 'src\a.pas', True);
  CheckMatch('*.<"pas"|"inc">', 'src\a.pas', False, [wcoPathMode]);
  CheckMatch('**.<"pas"|"inc">', 'src\a.pas', True, [wcoPathMode]);

  CheckMatch('<?>*', '\a', True);
  CheckMatch('<?>*', '\a', False, [wcoPathMode]);

  CheckMatch('?', '/', False, [wcoPathMode]);
  CheckMatch('*', '', True, [wcoPathMode]);
  CheckMatch('**', 'a/b\c', True, [wcoPathMode]);

  CheckMatch('*/*', 'a/b', True, [wcoPathMode]);
  CheckMatch('*/*', 'a/b/c', False, [wcoPathMode]);
  CheckMatch('*/*', 'a/b/c', True);

  CheckMatch('*<?##>', 'dir\x12', False, [wcoPathMode]);
  CheckMatch('*<?##>', 'dir\x12', True);
  CheckMatch('**\<?##>', 'dir\ab\x12', True, [wcoPathMode]);

  // Classes and quoted alternatives are evaluated normally in path mode -
  // only '*' and '?' (also inside a sequence) refuse separators.
  CheckMatch('<!a>', '\', True, [wcoPathMode]);
  CheckMatch('<!"ab">', 'x\', True, [wcoPathMode]);
  CheckMatch('<"a\">*', 'a\b', True, [wcoPathMode]);
end;

procedure TWildCardMatcherDUnitX.RealWorldMaskCombinationsTest;
const
  PHOTO = 'IMG_<####><##><##>_<######>.<"jpg"|"jpeg"|"heic">';
  BUILD_OUTPUT = '*\<"Win32"|"Win64">\<"Debug"|"Release">\*.<"dcu"|"exe">';
begin
  CheckMatch(PHOTO, 'IMG_20261014_235959.jpg', True);
  CheckMatch(PHOTO, 'IMG_20261014_235959.jpeg', True);
  CheckMatch(PHOTO, 'IMG_2026101_235959.jpg', False);
  CheckMatch(PHOTO, 'img_20261014_235959.HEIC', True);
  CheckMatch(PHOTO, 'img_20261014_235959.HEIC', False, [wcoCaseSensitive]);
  CheckMatch(PHOTO, 'IMG_20261014_235959.png', False);

  CheckMatch(BUILD_OUTPUT, 'C:\p\Win32\Debug\a.dcu', True);
  CheckMatch(BUILD_OUTPUT, 'C:\p\Win64\Release\b.exe', True);
  CheckMatch(BUILD_OUTPUT, 'C:\p\Win32\Debug\a.pas', False);
  CheckMatch(BUILD_OUTPUT, 'C:\p\Linux64\Debug\a.dcu', False);
  CheckMatch(BUILD_OUTPUT, 'C:\p\Win32\Debug\a.dcu', False, [wcoPathMode]);

  CheckMatch('~$*', '~$Report.docx', True);
  CheckMatch('~$*', 'Report.docx', False);

  CheckMatch('Track #<##> - *.mp3', 'Track #01 - Intro.mp3', True);
  CheckMatch('Track #<##> - *.mp3', 'Track #1 - Intro.mp3', False);

  CheckMatch('Foo[<ab>].txt', 'Foo[a].txt', True);
  CheckMatch('Foo[<ab>].txt', 'Foo[ab].txt', False);
  CheckMatch('Foo[<ab>].txt', 'Foo[c].txt', False);

  CheckMatch('*.~*', 'Unit1.~pas', True);
  CheckMatch('*.~*', 'Unit1.pas', False);

  // Names that do not start with '_' or '.'.
  CheckMatch('<!"_"|".">*', '_hidden', False);
  CheckMatch('<!"_"|".">*', '.git', False);
  CheckMatch('<!"_"|".">*', 'src', True);
  CheckMatch('<!"_"|".">*', '', False);

  CheckMatch('*<"3rdParty"|"ThirdParty">*', 'src\3rdParty\x.pas', True);
  CheckMatch('*<"3rdParty"|"ThirdParty">*', 'src\thirdparty\x.pas', True);
  CheckMatch('*<"3rdParty"|"ThirdParty">*', 'src\Vendor\x.pas', False);
end;

procedure TWildCardMatcherDUnitX.WildcardsInsideQuotesAreLiteralTest;
begin
  CheckMatchAllModes('<"*">', '*', True);
  CheckMatchAllModes('<"*">', 'a', False);

  CheckMatchAllModes('<"?"|"#">', '?', True);
  CheckMatchAllModes('<"?"|"#">', '#', True);
  CheckMatchAllModes('<"?"|"#">', 'a', False);
  CheckMatchAllModes('<"?"|"#">', '1', False);

  CheckMatchAllModes('<"<#>">', '<#>', True);
  CheckMatchAllModes('<"<#>">', '1', False);

  CheckMatchAllModes('<"a|b">', 'a|b', True);
  CheckMatchAllModes('<"a|b">', 'a', False);

  CheckMatchAllModes('*.<"bak"|"tmp"|"~*">', 'a.~*', True);
  CheckMatchAllModes('*.<"bak"|"tmp"|"~*">', 'a.~x', False);
  CheckMatchAllModes('*.<"bak"|"tmp"|"~*">', 'a.tmp', True);
end;

procedure TWildCardMatcherDUnitX.AnchoringCombinationsTest;
begin
  // Without a star at an end, the pattern must cover the whole input.
  CheckMatchAllModes('abc', 'abcd', False);
  CheckMatchAllModes('abc', 'xabc', False);
  CheckMatchAllModes('a*', 'ba', False);
  CheckMatchAllModes('*a', 'ab', False);
  CheckMatchAllModes('<#>', '12', False);
  CheckMatchAllModes('<"ab">', 'abc', False);
  CheckMatchAllModes('<"ab">', 'xab', False);
  CheckMatchAllModes('<?##>', 'x123', False);
  CheckMatchAllModes('<ab>', 'ab', False);
  CheckMatchAllModes('<!"a">', 'bb', False);
  CheckMatchAllModes('?', 'ab', False);
end;

procedure TWildCardMatcherDUnitX.BacktrackingCombinationsTest;
var
  LIndex: Integer;
  LManyA, LManyDigits, LManyPairs: string;
begin
  // Patterns that make a naive matcher backtrack a lot - results must be
  // right, and the run must stay quick.
  LManyA := StringOfChar('a', 30);
  LManyDigits := StringOfChar('1', 25);
  LManyPairs := '';

  for LIndex := 1 to 10 do
    LManyPairs := LManyPairs + 'a1';

  CheckMatchAllModes('*a*a*a*a*b', LManyA, False);
  CheckMatchAllModes('*a*a*a*a*b', LManyA + 'b', True);

  CheckMatchAllModes('*<#>*<#>*<#>*<#>x', LManyDigits, False);
  CheckMatchAllModes('*<#>*<#>*<#>*<#>x', LManyDigits + 'x', True);

  CheckMatchAllModes('*<"a"|"aa">*<"a"|"aa">*b', LManyA, False);
  CheckMatchAllModes('*<"a"|"aa">*<"a"|"aa">*b', LManyA + 'b', True);

  CheckMatchAllModes('*<?#>*<?#>*!', LManyPairs, False);
  CheckMatchAllModes('*<?#>*<?#>*!', LManyPairs + '!', True);

  CheckMatchAllModes('*<a-z>*<!a-z>*<a-z>*<!a-z>*z', LManyPairs, False);
  CheckMatchAllModes('*<a-z>*<!a-z>*<a-z>*<!a-z>*z', LManyPairs + 'z', True);
end;

procedure TWildCardMatcherDUnitX.EmptyInputCombinationsTest;
begin
  CheckMatchAllModes('', '', True);
  CheckMatchAllModes('*', '', True);
  CheckMatchAllModes('**', '', True);
  CheckMatchAllModes('?', '', False);
  CheckMatchAllModes('<?>', '', False);
  CheckMatchAllModes('<#>', '', False);
  CheckMatchAllModes('<a>', '', False);
  CheckMatchAllModes('<!a>', '', False);
  CheckMatchAllModes('<"">', '', True);
  CheckMatchAllModes('<""|"a">', '', True);
  CheckMatchAllModes('*<"">*', '', True);
  CheckMatchAllModes('<!"">', '', False);
  CheckMatchAllModes('<!"a">', '', False);

  CheckMatchAllModes('<"">', 'a', False);
  CheckMatchAllModes('<"">a<"">', 'a', True);
end;

{ Reference matcher - a deliberately naive third implementation of the README
  rules, written without looking at either engine's fast paths: tokens matched
  by plain memoized recursion, no pruning, no SIMD.  ASCII input only, so case
  folding is UpperCase. }

type
  TReferenceTokenKind = (rtkLiteral, rtkStar, rtkAnyChar, rtkDigit, rtkClass, rtkAlternation);

  TReferenceToken = record
    Alternatives: TArray<string>;   // rtkAlternation
    Kind: TReferenceTokenKind;
    Literal: Char;                  // rtkLiteral
    Members: string;                // rtkClass: single characters
    Negate: Boolean;                // rtkClass, rtkAlternation
    Ranges: string;                 // rtkClass: low, high pairs
    SegmentOnly: Boolean;           // rtkStar: a single '*' in path mode
  end;

  TReferenceMatcher = class
  strict private
    FCaseSensitive: Boolean;
    FInput: string;
    FMemo: TArray<ShortInt>;        // per (token, input position): -1 unknown, 0 no, 1 yes
    FPathMode: Boolean;
    FTokens: TArray<TReferenceToken>;
    FValid: Boolean;
    procedure AddToken(const AToken: TReferenceToken);
    function CharInClass(const AToken: TReferenceToken; const AChar: Char): Boolean;
    function IsSeparator(const AChar: Char): Boolean;
    function MatchFrom(const ATokenIndex, AInputIndex: Integer): Boolean;
    function MatchTokenAt(const ATokenIndex, AInputIndex: Integer): Boolean;
    function ParseAngleBrackets(const APattern: string; var AIndex: Integer): Boolean;
    function StartsWithAt(const AText: string; const AInputIndex: Integer): Boolean;
  public
    // False when the pattern is malformed - it then never matches anything
    function Parse(const APattern: string; const AOptions: TWildCardOptions): Boolean;
    function Match(const AInput: string): Boolean;
  end;

procedure TReferenceMatcher.AddToken(const AToken: TReferenceToken);
begin
  FTokens := FTokens + [AToken];
end;

function TReferenceMatcher.CharInClass(const AToken: TReferenceToken; const AChar: Char): Boolean;
var
  LIndex: Integer;
begin
  if Pos(AChar, AToken.Members) > 0 then
    Exit(True);

  LIndex := 1;

  while LIndex < Length(AToken.Ranges) do
  begin
    if (AChar >= AToken.Ranges[LIndex]) and (AChar <= AToken.Ranges[LIndex + 1]) then
      Exit(True);

    Inc(LIndex, 2);
  end;

  Result := False;
end;

function TReferenceMatcher.IsSeparator(const AChar: Char): Boolean;
begin
  Result := (AChar = '\') or (AChar = '/');
end;

function TReferenceMatcher.Match(const AInput: string): Boolean;
begin
  if not FValid then
    Exit(False);

  if FCaseSensitive then
    FInput := AInput
  else
    FInput := UpperCase(AInput);

  SetLength(FMemo, (Length(FTokens) + 1) * (Length(FInput) + 2));
  FillChar(FMemo[0], Length(FMemo), $FF);

  Result := MatchFrom(0, 1);
end;

function TReferenceMatcher.MatchFrom(const ATokenIndex, AInputIndex: Integer): Boolean;
var
  LSlot: Integer;
begin
  LSlot := (ATokenIndex * (Length(FInput) + 2)) + AInputIndex;

  if FMemo[LSlot] < 0 then
    FMemo[LSlot] := Ord(MatchTokenAt(ATokenIndex, AInputIndex));

  Result := FMemo[LSlot] = 1;
end;

// Does the pattern from token ATokenIndex on match the input from AInputIndex on?
function TReferenceMatcher.MatchTokenAt(const ATokenIndex, AInputIndex: Integer): Boolean;
var
  LAlternative: string;
  LEnd, LLongest: Integer;
  LHasChar: Boolean;
  LToken: TReferenceToken;
begin
  if ATokenIndex > High(FTokens) then
    Exit(AInputIndex = Length(FInput) + 1);

  LToken := FTokens[ATokenIndex];
  LHasChar := AInputIndex <= Length(FInput);

  case LToken.Kind of
    rtkStar:
      begin
        // The star takes FInput[AInputIndex .. LEnd - 1], shortest first.
        for LEnd := AInputIndex to Length(FInput) + 1 do
        begin
          if MatchFrom(ATokenIndex + 1, LEnd) then
            Exit(True);

          if LToken.SegmentOnly and (LEnd <= Length(FInput)) and IsSeparator(FInput[LEnd]) then
            Exit(False);
        end;

        Result := False;
      end;
    rtkAnyChar:
      Result := LHasChar and not (FPathMode and IsSeparator(FInput[AInputIndex])) and MatchFrom(ATokenIndex + 1, AInputIndex + 1);
    rtkDigit:
      Result := LHasChar and CharInSet(FInput[AInputIndex], ['0'..'9']) and MatchFrom(ATokenIndex + 1, AInputIndex + 1);
    rtkLiteral:
      Result := LHasChar and (FInput[AInputIndex] = LToken.Literal) and MatchFrom(ATokenIndex + 1, AInputIndex + 1);
    rtkClass:
      Result := LHasChar and (CharInClass(LToken, FInput[AInputIndex]) <> LToken.Negate)
        and MatchFrom(ATokenIndex + 1, AInputIndex + 1);
    rtkAlternation:
      if LToken.Negate then
      begin
        // No alternative may start here; consumes the length of the longest one.
        LLongest := 0;

        for LAlternative in LToken.Alternatives do
        begin
          if StartsWithAt(LAlternative, AInputIndex) then
            Exit(False);

          if Length(LAlternative) > LLongest then
            LLongest := Length(LAlternative);
        end;

        Result := (AInputIndex + LLongest <= Length(FInput) + 1) and MatchFrom(ATokenIndex + 1, AInputIndex + LLongest);
      end
      else
      begin
        for LAlternative in LToken.Alternatives do
          if StartsWithAt(LAlternative, AInputIndex) and MatchFrom(ATokenIndex + 1, AInputIndex + Length(LAlternative)) then
            Exit(True);

        Result := False;
      end;
  else
    Result := False;
  end;
end;

// AIndex is at '<'; on success it is moved past the closing '>'.
function TReferenceMatcher.ParseAngleBrackets(const APattern: string; var AIndex: Integer): Boolean;
var
  LEnd, LIndex, LQuoteEnd: Integer;
  LToken: TReferenceToken;
begin
  // Sequence: nothing but '#' and '?' up to the closing '>'.
  LEnd := AIndex + 1;

  while (LEnd <= Length(APattern)) and CharInSet(APattern[LEnd], ['#', '?']) do
    Inc(LEnd);

  if (LEnd > AIndex + 1) and (LEnd <= Length(APattern)) and (APattern[LEnd] = '>') then
  begin
    for LIndex := AIndex + 1 to LEnd - 1 do
    begin
      LToken := Default(TReferenceToken);

      if APattern[LIndex] = '#' then
        LToken.Kind := rtkDigit
      else
        LToken.Kind := rtkAnyChar;

      AddToken(LToken);
    end;

    AIndex := LEnd + 1;
    Exit(True);
  end;

  LToken := Default(TReferenceToken);
  LIndex := AIndex + 1;
  LToken.Negate := (LIndex <= Length(APattern)) and (APattern[LIndex] = '!');

  if LToken.Negate then
    Inc(LIndex);

  if (LIndex <= Length(APattern)) and (APattern[LIndex] = '"') then
  begin
    // Alternation: "alt" ('|' "alt")* '>'
    LToken.Kind := rtkAlternation;

    while True do
    begin
      if (LIndex > Length(APattern)) or (APattern[LIndex] <> '"') then
        Exit(False);

      LQuoteEnd := LIndex + 1;

      while (LQuoteEnd <= Length(APattern)) and (APattern[LQuoteEnd] <> '"') do
        Inc(LQuoteEnd);

      if LQuoteEnd > Length(APattern) then
        Exit(False);

      LToken.Alternatives := LToken.Alternatives + [Copy(APattern, LIndex + 1, LQuoteEnd - LIndex - 1)];
      LIndex := LQuoteEnd + 1;

      if LIndex > Length(APattern) then
        Exit(False);

      if APattern[LIndex] = '>' then
        Break;

      if APattern[LIndex] <> '|' then
        Exit(False);

      Inc(LIndex);
    end;

    AddToken(LToken);
    AIndex := LIndex + 1;
    Exit(True);
  end;

  // Single-character class; a '>' right after '<' or '<!' is a member.
  LToken.Kind := rtkClass;
  LEnd := LIndex;

  if (LEnd <= Length(APattern)) and (APattern[LEnd] = '>') then
    Inc(LEnd);

  while (LEnd <= Length(APattern)) and (APattern[LEnd] <> '>') do
    Inc(LEnd);

  if LEnd > Length(APattern) then
    Exit(False);

  // Members are LIndex .. LEnd - 1; 'x-y' is a range unless the '-' is the last member.
  while LIndex < LEnd do
    if (LIndex + 2 < LEnd) and (APattern[LIndex + 1] = '-') then
    begin
      LToken.Ranges := LToken.Ranges + APattern[LIndex] + APattern[LIndex + 2];
      Inc(LIndex, 3);
    end
    else
    begin
      LToken.Members := LToken.Members + APattern[LIndex];
      Inc(LIndex);
    end;

  AddToken(LToken);
  AIndex := LEnd + 1;
  Result := True;
end;

function TReferenceMatcher.Parse(const APattern: string; const AOptions: TWildCardOptions): Boolean;
var
  LIndex, LStarCount: Integer;
  LPattern: string;
  LToken: TReferenceToken;
begin
  FCaseSensitive := wcoCaseSensitive in AOptions;
  FPathMode := wcoPathMode in AOptions;
  FTokens := nil;
  FValid := False;

  // Case-insensitive: both sides are folded to upper case.
  if FCaseSensitive then
    LPattern := APattern
  else
    LPattern := UpperCase(APattern);

  LIndex := 1;

  while LIndex <= Length(LPattern) do
  begin
    LToken := Default(TReferenceToken);

    case LPattern[LIndex] of
      '*':
        begin
          LStarCount := 0;

          while (LIndex <= Length(LPattern)) and (LPattern[LIndex] = '*') do
          begin
            Inc(LStarCount);
            Inc(LIndex);
          end;

          LToken.Kind := rtkStar;
          LToken.SegmentOnly := FPathMode and (LStarCount = 1);
          AddToken(LToken);
        end;
      '?':
        begin
          LToken.Kind := rtkAnyChar;
          AddToken(LToken);
          Inc(LIndex);
        end;
      '<':
        if not ParseAngleBrackets(LPattern, LIndex) then
          Exit(False);
    else
      begin
        LToken.Kind := rtkLiteral;
        LToken.Literal := LPattern[LIndex];
        AddToken(LToken);
        Inc(LIndex);
      end;
    end;
  end;

  FValid := True;
  Result := True;
end;

function TReferenceMatcher.StartsWithAt(const AText: string; const AInputIndex: Integer): Boolean;
begin
  Result := Copy(FInput, AInputIndex, Length(AText)) = AText;
end;

type
  TReferencePiece = record
    Pattern: string;
    Example: string;   // an input fragment the piece matches (case-insensitively)
  end;

const
  REFERENCE_PIECES: array[0..37] of TReferencePiece = (
    (Pattern: 'a';             Example: 'a'),
    (Pattern: 'B';             Example: 'b'),
    (Pattern: '1';             Example: '1'),
    (Pattern: '\';             Example: '\'),
    (Pattern: '/';             Example: '/'),
    (Pattern: '.';             Example: '.'),
    (Pattern: '#';             Example: '#'),
    (Pattern: '[';             Example: '['),
    (Pattern: '*';             Example: ''),
    (Pattern: '**';            Example: ''),
    (Pattern: '?';             Example: 'x'),
    (Pattern: '<#>';           Example: '5'),
    (Pattern: '<##>';          Example: '42'),
    (Pattern: '<?#>';          Example: 'a7'),
    (Pattern: '<#?>';          Example: '7\'),
    (Pattern: '<??>';          Example: 'ab'),
    (Pattern: '<?>';           Example: '\'),
    (Pattern: '<ab>';          Example: 'b'),
    (Pattern: '<!ab>';         Example: 'c'),
    (Pattern: '<a-c>';         Example: 'B'),
    (Pattern: '<!0-9>';        Example: 'x'),
    (Pattern: '<>a>';          Example: '>'),
    (Pattern: '<-a>';          Example: '-'),
    (Pattern: '<a->';          Example: 'a'),
    (Pattern: '<#a>';          Example: '#'),
    (Pattern: '<!#>';          Example: '1'),
    (Pattern: '<\/>';          Example: '/'),
    (Pattern: '<"ab"|"b">';    Example: 'ab'),
    (Pattern: '<"a"|"ab">';    Example: 'ab'),
    (Pattern: '<""|"x">';      Example: ''),
    (Pattern: '<"">';          Example: ''),
    (Pattern: '<!"a"|"bc">';   Example: 'xy'),
    (Pattern: '<!"\">';        Example: 'a'),
    (Pattern: '<"A"|"1\">';    Example: '1\'),
    (Pattern: '<!"">';         Example: ''),
    (Pattern: '<"*"|"?">';     Example: '*'),
    (Pattern: '*';             Example: ''),
    (Pattern: '?';             Example: 'b'));

  REFERENCE_MALFORMED_PIECES: array[0..4] of string = ('<ab', '<"a', '<"a""b">', '<!"a"x>', '<');

  REFERENCE_FILL_CHARS = 'aB1\/.#x>';

procedure TWildCardMatcherDUnitX.ReferenceMatcherFuzzTest;
const
  SEED = 20261015;
  ITERATIONS = 3000;
  MODE_OPTIONS: array[0..3] of TWildCardOptions = ([], [wcoCaseSensitive], [wcoPathMode], [wcoCaseSensitive, wcoPathMode]);
var
  LAdhoc: TWildCard;
  LChecks, LMatches: Integer;
  LError, LExample, LPattern: string;
  LExpected: Boolean;
  LInputs: array[0..2] of string;
  LIteration, LPieceCount, LPieceIndex, LFillIndex, LInputIndex, LMode, LPosition: Integer;
  LPiece: TReferencePiece;
  LReference: TReferenceMatcher;
  LRegistered: TWildCard;
  LSeed: Cardinal;
begin
  // Both engines against an independent reference implementation (the
  // differential fuzz test only compares the engines with each other, so it
  // cannot catch a mistake they share).  Patterns are built from pieces
  // with known example inputs, so a good share of the inputs match.
  LSeed := SEED;
  LChecks := 0;
  LMatches := 0;

  LReference := TReferenceMatcher.Create;
  try
    for LIteration := 1 to ITERATIONS do
    begin
      LPattern := '';
      LExample := '';
      LPieceCount := 1 + Integer(FuzzNextRandom(LSeed, 5));

      for LPieceIndex := 1 to LPieceCount do
      begin
        LPiece := REFERENCE_PIECES[FuzzNextRandom(LSeed, Length(REFERENCE_PIECES))];
        LPattern := LPattern + LPiece.Pattern;

        if LPiece.Pattern[1] = '*' then
        begin
          for LFillIndex := 1 to Integer(FuzzNextRandom(LSeed, 4)) do
            LExample := LExample + REFERENCE_FILL_CHARS[1 + FuzzNextRandom(LSeed, Length(REFERENCE_FILL_CHARS))];
        end
        else
          LExample := LExample + LPiece.Example;
      end;

      if FuzzNextRandom(LSeed, 20) = 0 then
        LPattern := LPattern + REFERENCE_MALFORMED_PIECES[FuzzNextRandom(LSeed, Length(REFERENCE_MALFORMED_PIECES))];

      // The example as is, a mutated copy (one character replaced, removed or added) and the empty input.
      LInputs[0] := LExample;
      LInputs[1] := LExample;
      LPosition := 1 + Integer(FuzzNextRandom(LSeed, Length(LExample) + 1));

      case FuzzNextRandom(LSeed, 3) of
        0:
          if LPosition <= Length(LExample) then
            LInputs[1][LPosition] := REFERENCE_FILL_CHARS[1 + FuzzNextRandom(LSeed, Length(REFERENCE_FILL_CHARS))];
        1:
          Delete(LInputs[1], LPosition, 1);
      else
        Insert(REFERENCE_FILL_CHARS[1 + FuzzNextRandom(LSeed, Length(REFERENCE_FILL_CHARS))], LInputs[1], LPosition);
      end;

      LInputs[2] := '';

      for LMode := 0 to High(MODE_OPTIONS) do
      begin
        // The reference parser and ValidatePattern must agree on what is malformed.
        Assert.AreEqual(LReference.Parse(LPattern, MODE_OPTIONS[LMode]), TWildCard.ValidatePattern(LPattern, LError),
          Format('reference parser and ValidatePattern disagree on <%s> (%s)', [LPattern, LError]));

        LAdhoc := TWildCard.Create(MODE_OPTIONS[LMode]);
        LRegistered := TWildCard.Create(LPattern, MODE_OPTIONS[LMode]);

        for LInputIndex := 0 to High(LInputs) do
        begin
          LExpected := LReference.Match(LInputs[LInputIndex]);

          Assert.AreEqual(LExpected, LAdhoc.Match(LInputs[LInputIndex], LPattern),
            Format('ad-hoc %s differs from the reference: pattern <%s>, input <%s>, iteration %d',
            [OptionsText(MODE_OPTIONS[LMode]), LPattern, LInputs[LInputIndex], LIteration]));
          Assert.AreEqual(LExpected, LRegistered.Match(LInputs[LInputIndex]),
            Format('registered %s differs from the reference: pattern <%s>, input <%s>, iteration %d',
            [OptionsText(MODE_OPTIONS[LMode]), LPattern, LInputs[LInputIndex], LIteration]));

          Inc(LChecks);

          if LExpected then
            Inc(LMatches);
        end;
      end;
    end;
  finally
    LReference.Free;
  end;

  // Agreeing only on "no match" would prove little.
  Assert.IsTrue(LMatches * 5 > LChecks, Format('only %d of %d checks matched', [LMatches, LChecks]));
  Assert.IsTrue(LMatches * 5 < LChecks * 4, Format('%d of %d checks matched - too few mismatches', [LMatches, LChecks]));
end;

initialization
  TDUnitX.RegisterTestFixture(TWildCardMatcherDUnitX);

end.
