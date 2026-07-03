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

    { Spec example 3: '[abc]' }
    [Test]
    procedure CharClassPositiveSetTest;

    { Spec example 4: '[!abc]' }
    [Test]
    procedure CharClassNegatedSetTest;
    [Test]
    procedure NegatedSetAtStartSpecExampleTest;

    { Spec example 5: '[a-c]' }
    [Test]
    procedure CharClassRangeTest;
    [Test]
    procedure CombinedRangesTest;
    [Test]
    procedure NegatedRangeTest;

    { Spec example 6: '#' }
    [Test]
    procedure HashMatchesSingleDigitTest;
    [Test]
    procedure HashRejectsNonDigitTest;

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

    { Quoted-string alternation: '["foo"|"bar"]' }
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
  end;

implementation

uses
  System.Classes, System.SysUtils;

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

{ Spec example 3: '[abc]' }

procedure TWildCardMatcherDUnitX.CharClassPositiveSetTest;
begin
  // b[ae]ll finds ball and bell, but not bill (from the spec)
  Assert.IsTrue (TWildCard.Create.Match('ball', 'b[ae]ll'));
  Assert.IsTrue (TWildCard.Create.Match('bell', 'b[ae]ll'));
  Assert.IsFalse(TWildCard.Create.Match('bill', 'b[ae]ll'));
end;

{ Spec example 4: '[!abc]' }

procedure TWildCardMatcherDUnitX.CharClassNegatedSetTest;
begin
  // b[!ae]ll finds bill and bull, but not ball or bell (from the spec)
  Assert.IsTrue (TWildCard.Create.Match('bill', 'b[!ae]ll'));
  Assert.IsTrue (TWildCard.Create.Match('bull', 'b[!ae]ll'));
  Assert.IsFalse(TWildCard.Create.Match('ball', 'b[!ae]ll'));
  Assert.IsFalse(TWildCard.Create.Match('bell', 'b[!ae]ll'));
end;

procedure TWildCardMatcherDUnitX.NegatedSetAtStartSpecExampleTest;
begin
  // From the spec: "[!a]*" finds all items that do not begin with 'a'.
  Assert.IsTrue (TWildCard.Create.Match('banana', '[!a]*'));
  Assert.IsTrue (TWildCard.Create.Match('zebra',  '[!a]*'));
  Assert.IsTrue (TWildCard.Create.Match('x',      '[!a]*'));
  Assert.IsFalse(TWildCard.Create.Match('apple',  '[!a]*'));
  Assert.IsFalse(TWildCard.Create.Match('a',      '[!a]*'));
end;

{ Spec example 5: '[a-c]' }

procedure TWildCardMatcherDUnitX.CharClassRangeTest;
begin
  // b[a-c]d finds bad, bbd, bcd (from the spec)
  Assert.IsTrue (TWildCard.Create.Match('bad', 'b[a-c]d'));
  Assert.IsTrue (TWildCard.Create.Match('bbd', 'b[a-c]d'));
  Assert.IsTrue (TWildCard.Create.Match('bcd', 'b[a-c]d'));
  Assert.IsFalse(TWildCard.Create.Match('bdd', 'b[a-c]d'));
  Assert.IsFalse(TWildCard.Create.Match('b0d', 'b[a-c]d'));
end;

procedure TWildCardMatcherDUnitX.CombinedRangesTest;
begin
  // Multiple ranges in one class
  Assert.IsTrue (TWildCard.Create.Match('X', '[a-zA-Z]'));
  Assert.IsTrue (TWildCard.Create.Match('x', '[a-zA-Z]'));
  Assert.IsFalse(TWildCard.Create.Match('5', '[a-zA-Z]'));

  // Range + literals + digits
  Assert.IsTrue (TWildCard.Create.Match('q',  '[a-cq0-9]'));
  Assert.IsTrue (TWildCard.Create.Match('b',  '[a-cq0-9]'));
  Assert.IsTrue (TWildCard.Create.Match('7',  '[a-cq0-9]'));
  Assert.IsFalse(TWildCard.Create.Match('z',  '[a-cq0-9]'));
end;

procedure TWildCardMatcherDUnitX.NegatedRangeTest;
begin
  Assert.IsTrue (TWildCard.Create.Match('1', '[!a-z]'));
  Assert.IsTrue (TWildCard.Create.Match('!', '[!a-z]'));
  Assert.IsFalse(TWildCard.Create.Match('m', '[!a-z]'));
end;

{ Spec example 6: '#' }

procedure TWildCardMatcherDUnitX.HashMatchesSingleDigitTest;
begin
  // 1#3 finds 103, 113, 123 (from the spec)
  Assert.IsTrue(TWildCard.Create.Match('103', '1#3'));
  Assert.IsTrue(TWildCard.Create.Match('113', '1#3'));
  Assert.IsTrue(TWildCard.Create.Match('123', '1#3'));
  Assert.IsTrue(TWildCard.Create.Match('193', '1#3'));
end;

procedure TWildCardMatcherDUnitX.HashRejectsNonDigitTest;
begin
  Assert.IsFalse(TWildCard.Create.Match('1a3', '1#3'), '"a" is not a digit');
  Assert.IsFalse(TWildCard.Create.Match('1 3', '1#3'), 'space is not a digit');
  Assert.IsFalse(TWildCard.Create.Match('13',  '1#3'), '"#" must consume exactly one digit');
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
  Assert.IsFalse(TWildCard.Create.Match('', '#'));
  Assert.IsFalse(TWildCard.Create.Match('', 'a'));
  Assert.IsFalse(TWildCard.Create.Match('', '[a-z]'));
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
  Assert.IsTrue(TWildCard.Create.Match('FOO', '[a-z]??'));
end;

procedure TWildCardMatcherDUnitX.CaseSensitiveFlagTest;
begin
  Assert.IsFalse(TWildCard.Create(True).Match('README.TXT', 'readme.txt'));
  Assert.IsTrue (TWildCard.Create(True).Match('readme.txt', 'readme.txt'));
  Assert.IsFalse(TWildCard.Create(True).Match('FOO',        '[a-z]??'));
  Assert.IsTrue (TWildCard.Create(True).Match('foo',        '[a-z]??'));
end;

procedure TWildCardMatcherDUnitX.UnterminatedClassReturnsFalseTest;
begin
  // Patterns with an unterminated '[' are malformed - never match
  Assert.IsFalse(TWildCard.Create.Match('a',  '[abc'));
  Assert.IsFalse(TWildCard.Create.Match('ab', 'a[bc'));
end;

procedure TWildCardMatcherDUnitX.EmptyClassReturnsFalseTest;
begin
  // '[]' is treated as an unterminated class with ']' as literal content;
  // the next ']' would close it. Bare '[]' on its own is malformed.
  Assert.IsFalse(TWildCard.Create.Match('a', '[]'));
end;

procedure TWildCardMatcherDUnitX.LiteralCloseBracketAsFirstClassCharTest;
begin
  // First content char ']' is a literal - '[]abc]' matches ']', 'a', 'b' or 'c'
  Assert.IsTrue (TWildCard.Create.Match(']', '[]abc]'));
  Assert.IsTrue (TWildCard.Create.Match('a', '[]abc]'));
  Assert.IsTrue (TWildCard.Create.Match('c', '[]abc]'));
  Assert.IsFalse(TWildCard.Create.Match('x', '[]abc]'));
end;

procedure TWildCardMatcherDUnitX.DashAsLiteralAtStartOrEndOfClassTest;
begin
  // '-' at the start/end of a class is a literal, not part of a range
  Assert.IsTrue (TWildCard.Create.Match('-', '[-x]'));
  Assert.IsTrue (TWildCard.Create.Match('x', '[-x]'));
  Assert.IsTrue (TWildCard.Create.Match('-', '[a-]'));
  Assert.IsTrue (TWildCard.Create.Match('a', '[a-]'));
  Assert.IsFalse(TWildCard.Create.Match('b', '[a-]'));
end;

{ Combinations }

procedure TWildCardMatcherDUnitX.ComplexFileNameLikePatternsTest;
begin
  // Realistic file-mask style patterns
  Assert.IsTrue (TWildCard.Create.Match('Foo1.pas',  '*.pas'));
  Assert.IsTrue (TWildCard.Create.Match('Foo1.pas',  'Foo?.pas'));
  Assert.IsTrue (TWildCard.Create.Match('Foo7.pas',  'Foo#.pas'));
  Assert.IsFalse(TWildCard.Create.Match('FooA.pas',  'Foo#.pas'));
  Assert.IsTrue (TWildCard.Create.Match('Test_001.log', 'Test_###.log'));
  Assert.IsFalse(TWildCard.Create.Match('Test_00A.log', 'Test_###.log'));
end;

procedure TWildCardMatcherDUnitX.MixedSpecialCharsInClassTest;
begin
  // Class combining range + literal digit + literal letter
  Assert.IsTrue (TWildCard.Create.Match('Foo_5.pas', 'Foo[_-]#.pas'));
  Assert.IsTrue (TWildCard.Create.Match('Foo-5.pas', 'Foo[_-]#.pas'));
  Assert.IsFalse(TWildCard.Create.Match('Foo.5.pas', 'Foo[_-]#.pas'));
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

{ Quoted-string alternation: '["foo"|"bar"]' }

procedure TWildCardMatcherDUnitX.QuotedAltSingleAlternativeMatchesLiteralTest;
begin
  // A class with a single quoted alternative matches just that literal at
  // the current position, consuming its length.
  Assert.IsTrue (TWildCard.Create.Match('foo',    '["foo"]'));
  Assert.IsTrue (TWildCard.Create.Match('foobar', '["foo"]*'));
  Assert.IsFalse(TWildCard.Create.Match('fo',     '["foo"]'), 'not enough input to span the alt');
  Assert.IsFalse(TWildCard.Create.Match('bar',    '["foo"]'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltMultipleAlternativesAnyMatchesTest;
begin
  // The class succeeds when ANY of the listed alternatives matches at the
  // current position.
  Assert.IsTrue (TWildCard.Create.Match('foo', '["foo"|"bar"|"baz"]'));
  Assert.IsTrue (TWildCard.Create.Match('bar', '["foo"|"bar"|"baz"]'));
  Assert.IsTrue (TWildCard.Create.Match('baz', '["foo"|"bar"|"baz"]'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltNoneOfTheAlternativesMatchesFailsTest;
begin
  Assert.IsFalse(TWildCard.Create.Match('qux', '["foo"|"bar"|"baz"]'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltCaseInsensitiveByDefaultTest;
begin
  Assert.IsTrue(TWildCard.Create.Match('FOO', '["foo"|"bar"]'));
  Assert.IsTrue(TWildCard.Create.Match('Bar', '["foo"|"bar"]'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltCaseSensitiveFlagTest;
begin
  Assert.IsTrue (TWildCard.Create(True).Match('foo', '["foo"|"bar"]'));
  Assert.IsFalse(TWildCard.Create(True).Match('FOO', '["foo"|"bar"]'));
  Assert.IsFalse(TWildCard.Create(True).Match('Bar', '["foo"|"bar"]'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltCombinedWithAsteriskTest;
begin
  // Embedded in a larger pattern: '*["foo"|"bar"]*' = the input must
  // contain either 'foo' or 'bar' somewhere.
  Assert.IsTrue (TWildCard.Create.Match('xxfooyy', '*["foo"|"bar"]*'));
  Assert.IsTrue (TWildCard.Create.Match('xxbaryy', '*["foo"|"bar"]*'));
  Assert.IsFalse(TWildCard.Create.Match('xxxxxxx',  '*["foo"|"bar"]*'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltReadmeExamplePatternTest;
begin
  // README example: '*["3rdparty"|"ThirdParty"]*.md' matches Markdown files
  // anywhere on the path that mention either name.
  Assert.IsTrue (TWildCard.Create.Match('docs/3rdparty/notes.md',    '*["3rdparty"|"ThirdParty"]*.md'));
  Assert.IsTrue (TWildCard.Create.Match('src/ThirdPartyReadme.md',   '*["3rdparty"|"ThirdParty"]*.md'));
  Assert.IsTrue (TWildCard.Create.Match('3RDPARTY-overview.md',      '*["3rdparty"|"ThirdParty"]*.md'),
    'CI default: case difference in the alt content still matches');
  Assert.IsFalse(TWildCard.Create.Match('docs/internal/notes.md',    '*["3rdparty"|"ThirdParty"]*.md'));
  Assert.IsFalse(TWildCard.Create.Match('docs/ThirdParty/notes.txt', '*["3rdparty"|"ThirdParty"]*.md'),
    'extension mismatch');
end;

procedure TWildCardMatcherDUnitX.QuotedAltAlternativesWithDifferentLengthsBacktrackTest;
begin
  // 'a' and 'ab' are both candidates at position 1.  With trailing 'b' in
  // the pattern, the engine must try the longer alt first or backtrack so
  // that input 'ab' followed by 'b' = 'abb' succeeds.
  Assert.IsTrue(TWildCard.Create.Match('abb', '["a"|"ab"]b'),
    'engine must explore both alternatives to find the one that lets the tail match');
  // Input 'ab' - first alt 'a' matches and leaves 'b' for the trailing 'b'.
  Assert.IsTrue(TWildCard.Create.Match('ab',  '["a"|"ab"]b'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltEmptyStringAlternativeMatchesZeroCharsTest;
begin
  // An empty alternative '""' matches zero characters - always succeeds at
  // the current position, consumes nothing.
  Assert.IsTrue (TWildCard.Create.Match('foo',    '[""]foo'));
  Assert.IsTrue (TWildCard.Create.Match('foobar', '[""|"foo"]bar'),
    'empty alt matches first, leaves the whole input for the tail');
  // Single empty alt at end - matches when nothing else is left.
  Assert.IsTrue (TWildCard.Create.Match('foo', 'foo[""]'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltNegatedSameLengthAlternativesTest;
begin
  // Negation: NONE of the alts may be a prefix at the current position;
  // consume max(altLen) chars on success.  All alts here are length 3.
  Assert.IsTrue (TWildCard.Create.Match('quxsuffix', '[!"foo"|"bar"]*'));
  Assert.IsFalse(TWildCard.Create.Match('foosuffix', '[!"foo"|"bar"]*'));
  Assert.IsFalse(TWildCard.Create.Match('barsuffix', '[!"foo"|"bar"]*'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltNegatedConsumesLongestAlternativeLengthTest;
begin
  // Alternatives have different lengths (3 and 6).  Longest = 6 so the
  // class consumes 6 chars on success.
  Assert.IsTrue (TWildCard.Create.Match('abcdef-tail', '[!"foo"|"barbaz"]-tail'),
    'six chars consumed, then "-tail" matches the rest');
  Assert.IsFalse(TWildCard.Create.Match('foo-something', '[!"foo"|"barbaz"]-tail'),
    'first three chars match the short alt as a prefix -> negation fails');
  Assert.IsFalse(TWildCard.Create.Match('barbaz-tail', '[!"foo"|"barbaz"]-tail'),
    'first six chars match the long alt as a prefix -> negation fails');
end;

procedure TWildCardMatcherDUnitX.QuotedAltNegatedRequiresEnoughInputTest;
begin
  // Input must have at least max(altLen) chars remaining; otherwise the
  // class cannot consume its slice and the match fails.
  Assert.IsFalse(TWildCard.Create.Match('ab', '[!"foo"|"bar"]'),
    'only 2 chars left but the class needs to consume 3');
  Assert.IsFalse(TWildCard.Create.Match('',   '[!"foo"]'),
    'no input left, cannot consume the slice');
end;

procedure TWildCardMatcherDUnitX.QuotedAltMalformedReturnsFalseTest;
begin
  // Unterminated quote - the class never closes, FindClassEnd returns 0,
  // engine returns False.
  Assert.IsFalse(TWildCard.Create.Match('foo', '["foo'));
  Assert.IsFalse(TWildCard.Create.Match('foo', '["foo"|"bar'));
  // Missing '|' between alternatives.
  Assert.IsFalse(TWildCard.Create.Match('foo', '["foo""bar"]'));
  // Garbage between alternatives.
  Assert.IsFalse(TWildCard.Create.Match('foo', '["foo"x"bar"]'));
end;

procedure TWildCardMatcherDUnitX.QuotedAltCloseBracketInsideQuotedAlternativeIsLiteralTest;
begin
  // A ']' inside '"..."' is part of the alternative literal, not the
  // class terminator.  This is the whole point of being quote-aware.
  Assert.IsTrue (TWildCard.Create.Match('a]b', '["a]b"]'));
  Assert.IsTrue (TWildCard.Create.Match('[x]', '["[x]"]'));
end;

procedure TWildCardMatcherDUnitX.PipeOutsideClassIsLiteralTest;
begin
  // '|' has no special meaning outside '[...]' anymore - it is just a
  // literal character.
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
  // If short-circuiting works, the unterminated '[' never gets visited.
  LPatterns := TArray<string>.Create('*.pas', '[unterminated');
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
  Assert.IsTrue (TWildCard.Create.Match(#$00E4, '[' + #$00E4 + #$00F6 + #$00E5 + ']'));
  Assert.IsTrue (TWildCard.Create.Match(#$00C4, '[' + #$00E4 + #$00F6 + #$00E5 + ']'),
    'uppercase input should match lowercase class members CI');
  Assert.IsFalse(TWildCard.Create.Match(#$00FC, '[' + #$00E4 + #$00F6 + #$00E5 + ']'),
    'German u-umlaut is NOT in the Finnish set');

  // Negation with non-ASCII
  Assert.IsFalse(TWildCard.Create.Match(#$00E4, '[!' + #$00E4 + #$00F6 + #$00E5 + ']'));
  Assert.IsTrue (TWildCard.Create.Match('a',    '[!' + #$00E4 + #$00F6 + #$00E5 + ']'));
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
  // ASCII and non-ASCII intermixed; '?', '#', class all combine fine.
  Assert.IsTrue (TWildCard.Create.Match('Raportti_2024_' + #$00E4 + '.log', 'raportti_####_?.log'));
  Assert.IsTrue (TWildCard.Create.Match('P' + #$00E4 + 'iv' + #$00E4 + 'kirja_03.pas',
                                 'P[' + #$00E4 + #$00F6 + ']iv[' + #$00E4 + #$00F6 + ']kirja_##.pas'));
  Assert.IsFalse(TWildCard.Create.Match('P' + #$00FC + 'iv' + #$00FC + 'kirja_03.pas',
                                 'P[' + #$00E4 + #$00F6 + ']iv[' + #$00E4 + #$00F6 + ']kirja_##.pas'),
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
  // Literal ']' as first content char
  Assert.IsTrue (TWildCard.Create('[]abc]').Match(']'));
  Assert.IsTrue (TWildCard.Create('[]abc]').Match('a'));
  Assert.IsFalse(TWildCard.Create('[]abc]').Match('x'));

  // Literal '-' at start / end of class
  Assert.IsTrue (TWildCard.Create('[-x]').Match('-'));
  Assert.IsTrue (TWildCard.Create('[a-]').Match('-'));
  Assert.IsFalse(TWildCard.Create('[a-]').Match('b'));

  // Ranges + negation
  Assert.IsTrue (TWildCard.Create('b[a-c]d').Match('bbd'));
  Assert.IsFalse(TWildCard.Create('b[a-c]d').Match('bdd'));
  Assert.IsTrue (TWildCard.Create('b[!ae]ll').Match('bull'));
  Assert.IsFalse(TWildCard.Create('b[!ae]ll').Match('ball'));
  Assert.IsTrue (TWildCard.Create('[a-cq0-9]').Match('7'));
  Assert.IsFalse(TWildCard.Create('[a-cq0-9]').Match('z'));
end;

procedure TWildCardMatcherDUnitX.CompiledQuotedAltTest;
begin
  Assert.IsTrue (TWildCard.Create('["foo"|"bar"]').Match('bar'));
  Assert.IsFalse(TWildCard.Create('["foo"|"bar"]').Match('qux'));
  Assert.IsTrue (TWildCard.Create('*["3rdparty"|"ThirdParty"]*.md').Match('docs\3rdparty\notes.md'));
  Assert.IsFalse(TWildCard.Create('*["3rdparty"|"ThirdParty"]*.md').Match('docs\internal\notes.md'));

  // Different-length alternatives require backtracking across alts
  Assert.IsTrue (TWildCard.Create('["a"|"ab"]b').Match('abb'));
  Assert.IsTrue (TWildCard.Create('["a"|"ab"]b').Match('ab'));

  // Empty alternative matches zero chars
  Assert.IsTrue (TWildCard.Create('[""|"foo"]bar').Match('foobar'));
  Assert.IsTrue (TWildCard.Create('foo[""]').Match('foo'));

  // Negation consumes the longest alternative's length
  Assert.IsTrue (TWildCard.Create('[!"foo"|"bar"]*').Match('quxsuffix'));
  Assert.IsFalse(TWildCard.Create('[!"foo"|"bar"]*').Match('foosuffix'));
  Assert.IsTrue (TWildCard.Create('[!"foo"|"barbaz"]-tail').Match('abcdef-tail'));
  Assert.IsFalse(TWildCard.Create('[!"foo"|"barbaz"]-tail').Match('barbaz-tail'));

  // ']' inside a quoted alternative is a literal
  Assert.IsTrue (TWildCard.Create('["a]b"]').Match('a]b'));
  Assert.IsTrue (TWildCard.Create('["[x]"]').Match('[x]'));
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

  // Star followed by '#' (digit skip) and by a class
  Assert.IsTrue (TWildCard.Create('*Repository*_v#_*.xyz').Match('OrderRepository_v3_final.xyz'));
  Assert.IsFalse(TWildCard.Create('*Repository*_v#_*.xyz').Match('OrderRepository_vX_final.xyz'));
  Assert.IsTrue (TWildCard.Create('*[abc]end').Match('xxxaend'));
  Assert.IsFalse(TWildCard.Create('*[abc]end').Match('xxxdend'));
end;

procedure TWildCardMatcherDUnitX.CompiledDigitAndQuestionTest;
begin
  Assert.IsTrue (TWildCard.Create('Test_###.log').Match('Test_001.log'));
  Assert.IsFalse(TWildCard.Create('Test_###.log').Match('Test_00A.log'));
  Assert.IsTrue (TWildCard.Create('b?ll').Match('ball'));
  Assert.IsFalse(TWildCard.Create('b?ll').Match('bll'));
  Assert.IsTrue (TWildCard.Create('*_v#_*').Match('x_v3_y'));
  Assert.IsFalse(TWildCard.Create('*_v#_*').Match('x_vA_y'));
end;

procedure TWildCardMatcherDUnitX.CompiledCaseSensitiveTest;
begin
  // CS instance: ordinal compiled engine
  Assert.IsTrue (TWildCard.Create('*.pas', True).Match('unit1.pas'));
  Assert.IsFalse(TWildCard.Create('*.pas', True).Match('UNIT1.PAS'));
  Assert.IsTrue (TWildCard.Create('["foo"|"bar"]', True).Match('bar'));
  Assert.IsFalse(TWildCard.Create('["foo"|"bar"]', True).Match('BAR'));

  // CI instance: per-char engine, pattern case irrelevant
  Assert.IsTrue (TWildCard.Create('*.PAS').Match('unit1.pas'));
  Assert.IsTrue (TWildCard.Create('["FOO"|"BAR"]').Match('bar'));
end;

procedure TWildCardMatcherDUnitX.CompiledEmptyAndMalformedPatternTest;
begin
  // Empty registered pattern matches only the empty input
  Assert.IsTrue (TWildCard.Create('').Match(''));
  Assert.IsFalse(TWildCard.Create('').Match('x'));

  // Malformed patterns never match (compiled as invalid)
  Assert.IsFalse(TWildCard.Create('[abc').Match('a'));
  Assert.IsFalse(TWildCard.Create('["foo').Match('foo'));
  Assert.IsFalse(TWildCard.Create('["foo"|"bar').Match('foo'));
  Assert.IsFalse(TWildCard.Create('["foo""bar"]').Match('foo'));
  Assert.IsFalse(TWildCard.Create('["foo"x"bar"]').Match('foo'));

  // A malformed pattern in a set must not break the others
  Assert.IsTrue(TWildCard.Create(TArray<string>.Create('[bad', '*.pas')).Match('Unit1.pas'));
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
    TArray<string>.Create('*["draft"|"backup"]*'));

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

initialization
  TDUnitX.RegisterTestFixture(TWildCardMatcherDUnitX);

end.
