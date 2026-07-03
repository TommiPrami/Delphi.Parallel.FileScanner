unit Delphi.WildCardMatcher;

// Windows / DOS style wildcard matcher with a few common extensions.
//
//   *                  matches zero or more characters
//   ?                  matches exactly one character
//   #                  matches exactly one decimal digit (0-9)
//   [abc]              matches any one of the characters in the set
//   [!abc]             matches any one character NOT in the set
//   [a-z]              matches any one character in the range (low..high, ascending)
//   ["foo"|"bar"]      matches the literal "foo" OR the literal "bar" at this position
//   [!"foo"|"bar"]     matches a slice of length max(altLen) that is NEITHER prefix
//
// Sets may combine literals and ranges, e.g. '[a-zA-Z0-9_]'.
// A literal ']' inside a single-char class must be the first content character:
// '[]abc]' matches ']', 'a', 'b' or 'c'.  '[!]abc]' negates the same set.
// A '-' that is the first or last content character is treated as a literal,
// e.g. '[-x]' matches '-' or 'x'; '[a-]' matches 'a' or '-'.
//
// Two flavours of '[...]':
//
//   Single-char class: '[abc]', '[a-z]', '[!xyz]' - matches exactly one input
//   character.  This is the legacy DOS form.
//
//   Quoted-string alternation: '["foo"|"bar"|"baz"]' - matches one of the
//   listed literal strings at the current position and consumes its length.
//   '|' inside the brackets is the alternative separator.  Auto-detected:
//   if the first content character (after an optional '!') is '"', the
//   class is parsed in alternation form; otherwise the legacy single-char
//   rules apply.
//
//   Negated alternation '[!"foo"|"bar"]' succeeds when NONE of the listed
//   alternatives is a prefix at the current position; it then consumes the
//   length of the LONGEST alternative.  When the input has fewer characters
//   left than the longest alternative, the match fails (just like the
//   positive form would when there is not enough input to span the alt).
//
//   Empty quoted alternative ('""') is allowed and matches zero characters
//   (always succeeds, consumes nothing).  Backslashes / quote escapes are
//   NOT supported - quoted alternation is intended for file-mask use and
//   Windows file names cannot contain the characters '[', ']', '|' or '"'
//   to begin with, so the syntax stays safe to embed in literal masks.
//
//   '"' OUTSIDE a '[...]' class is a plain literal character.
//
// Matching is case-insensitive by default (Windows convention); pass
// ACaseSensitive = True for ordinal comparison.  The case-sensitive and
// case-insensitive engines are separate code paths - no per-character
// branch on the flag - so case-sensitive matching is noticeably faster.
//
// The CI engine pre-upper-cases the pattern once (cheap, patterns are
// small and scanned many times during backtracking) so the inner loop
// only has to upper-case the input side.  Upper-casing uses an inline
// ASCII fast path with a Unicode fallback for chars >= U+0080, so
// non-ASCII letters (e.g. Finnish 'å'/'Å') still compare correctly.
//
// The multi-pattern overloads return True on the first pattern that matches
// (short-circuit) and do not report which one - keep the call site simple.
//
// TWildCard can also be used as an instance value (record) that pre-compiles
// a set of patterns at construction time.  This skips the per-call
// FastUpperString step for callers who match many inputs against the same
// fixed pattern set.  Case-sensitivity is locked in at Create and used for
// every Match call on that instance.  Patterns come first, ACaseSensitive
// last with default False, e.g.
//   var LMask := TWildCard.Create(['*.pas', '*.dpr']);            // CI
//   var LSrc  := TWildCard.Create(['*.pas', '*.dpr'], True);      // CS
//   for var LFile in TDirectory.GetFiles(...) do
//     if LMask.Match(LFile) then ...
// Instance Match(input, pattern) calls use the ad-hoc pattern only by
// default; pass AAlsoMatchRegistered = True to also try the registered set.

interface

uses
  System.Classes;

type
  TWildCard = record
  strict private
    // Instance state - set by Create overloads, used by instance Match.
    // Always go through one of the Create overloads (or Default(TWildCard))
    // to initialise - a bare 'var LMask: TWildCard' leaves FCaseSensitive
    // with stack garbage because Boolean is an unmanaged type.
    FPatterns: TArray<string>;   // pre-upper-cased if CI, originals if CS
    FCaseSensitive: Boolean;
    // Case-independent helpers
    class function IsAsciiDigit(const AChar: Char): Boolean; static; inline;
    class function FindClassEnd(const APattern: string; const AStart: Integer): Integer; static;
    class function IsQuotedAltClass(const APattern: string; const AClassStart: Integer): Boolean; static;
    // Case-sensitive path (direct ordinal compare, no ToUpper calls)
    class function CharInClassCS(const APattern: string; const AStart, AEnd: Integer; const AChar: Char): Boolean; static;
    class function AltMatchesAtCS(const AInput: string; const AInputIdx: Integer;
      const APattern: string; const AAltStart, AAltLen: Integer): Boolean; static;
    class function MatchQuotedAltClassCS(const AInput, APattern: string;
      const AInputIdx, AClassStart, AClassEnd: Integer): Boolean; static;
    class function MatchRecursiveCS(const AInput, APattern: string; AInputIdx, APatternIdx: Integer): Boolean; static;
    // Case-insensitive path (ToUpper compare)
    class function CharInClassCI(const APattern: string; const AStart, AEnd: Integer; const AChar: Char): Boolean; static;
    class function AltMatchesAtCI(const AInput: string; const AInputIdx: Integer;
      const APattern: string; const AAltStart, AAltLen: Integer): Boolean; static;
    class function MatchQuotedAltClassCI(const AInput, APattern: string;
      const AInputIdx, AClassStart, AClassEnd: Integer): Boolean; static;
    class function MatchRecursiveCI(const AInput, APattern: string; AInputIdx, APatternIdx: Integer): Boolean; static;
    // Walks FPatterns and returns True on the first match.  FPatterns are
    // already in the correct form (pre-upper-cased for CI), so no per-call
    // FastUpperString work is needed here.
    function MatchRegistered(const AInput: string): Boolean;
  public
    // === Instance API: pre-register patterns + match many inputs ===
    // ACaseSensitive is locked in for the lifetime of the instance and
    // defaults to case-insensitive matching (Windows convention).
    // For one-off matching (no pre-registered patterns) use the no-arg
    // Create overload directly, e.g.
    //   if TWildCard.Create.Match(LFile, '*.pas') then ...
    //   if TWildCard.Create(True).Match('CASE.txt', '*.txt') then ...
    //   if TWildCard.Create.Match(LFile, ['*.pas', '*.dpr']) then ...
    class function Create(const ACaseSensitive: Boolean = False): TWildCard; overload; static;
    class function Create(const APattern: string; const ACaseSensitive: Boolean = False): TWildCard; overload; static;
    class function Create(const APatterns: TArray<string>; const ACaseSensitive: Boolean = False): TWildCard; overload; static;
    class function Create(const APatterns: TStrings; const ACaseSensitive: Boolean = False): TWildCard; overload; static;

    // Match against the registered set only.
    function Match(const AInput: string): Boolean; overload;
    // Match against an ad-hoc pattern (uses the instance's case mode). AAlsoMatchRegistered = True also tries the registered set.
    function Match(const AInput, APattern: string; const AAlsoMatchRegistered: Boolean = False): Boolean; overload;
    function Match(const AInput: string; const APatterns: TArray<string>; const AAlsoMatchRegistered: Boolean = False): Boolean; overload;
    function Match(const AInput: string; const APatterns: TStrings; const AAlsoMatchRegistered: Boolean = False): Boolean; overload;
    property CaseSensitive: Boolean read FCaseSensitive;
    property RegisteredPatterns: TArray<string> read FPatterns;
  end;

implementation

uses
  System.Character, System.SysUtils;

{ TWildCard - shared helpers }

// Inline ASCII fast path for ToUpper.  ASCII a..z is a single subtract;
// everything else < U+0080 returns unchanged; only chars >= U+0080 fall
// back to AnsiUpperCase (locale-aware via LCMapStringW on Windows).
// System.SysUtils.UpperCase is ASCII-only - it does NOT upper-case
// 'ä'/'å'/'ü' etc. - so AnsiUpperCase is required for Unicode correctness.
// Saves a per-char OS call on the 99.x% ASCII filename case.
//
// NOTE: we compare via Ord() instead of 'AChar < #128'.  Char-literal
// #128 is parsed as AnsiChar; comparing WideChar to AnsiChar goes through
// a signed-byte path where e.g. WideChar(228) < AnsiChar(128) evaluates
// to True (228 as signed byte = -28).  Ord() forces unsigned Integer
// comparison which is correct.
function FastToUpper(const AChar: Char): Char; inline;
begin
  if (AChar >= 'a') and (AChar <= 'z') then
    Result := Chr(Ord(AChar) - 32)
  else if Ord(AChar) < 128 then
    Result := AChar
  else
    Result := AnsiUpperCase(AChar)[1];
end;

// Returns True iff AStr contains at least one char that FastToUpper would
// change (ASCII lowercase or any non-ASCII).  Lets the caller skip the
// allocation when upper-casing would be a no-op (e.g. pattern '*').
function NeedsUpperCase(const AStr: string): Boolean;
var
  LIdx: Integer;
  LChar: Char;
begin
  for LIdx := 1 to Length(AStr) do
  begin
    LChar := AStr[LIdx];

    if ((LChar >= 'a') and (LChar <= 'z')) or (Ord(LChar) >= 128) then
      Exit(True);
  end;

  Result := False;
end;

function FastUpperString(const AStr: string): string;
var
  LIdx: Integer;
  LChar: Char;
begin
  // Skip the allocation entirely when there is nothing to change - common
  // for patterns like '*' or 'FOO*.PAS'.
  if not NeedsUpperCase(AStr) then
    Exit(AStr);

  // Hot path: walk every char.  Non-ASCII falls back to OS UpperCase which
  // handles Latin-1, German umlauts, Cyrillic, Greek etc. correctly.
  SetLength(Result, Length(AStr));

  for LIdx := 1 to Length(AStr) do
  begin
    LChar := AStr[LIdx];
    if (LChar >= 'a') and (LChar <= 'z') then
      Result[LIdx] := Chr(Ord(LChar) - 32)
    else if Ord(LChar) < 128 then
      Result[LIdx] := LChar
    else
      Result[LIdx] := AnsiUpperCase(LChar)[1];
  end;
end;

class function TWildCard.IsAsciiDigit(const AChar: Char): Boolean;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

class function TWildCard.IsQuotedAltClass(const APattern: string; const AClassStart: Integer): Boolean;
var
  LIndex: Integer;
begin
  // AClassStart points at '['.  Returns True if the class is in quoted
  // alternation form, i.e. the first content character (after an optional
  // negation '!') is a '"'.  Caller has already validated AClassStart.
  LIndex := AClassStart + 1;

  if (LIndex <= Length(APattern)) and (APattern[LIndex] = '!') then
    Inc(LIndex);

  Result := (LIndex <= Length(APattern)) and (APattern[LIndex] = '"');
end;

class function TWildCard.FindClassEnd(const APattern: string; const AStart: Integer): Integer;
var
  LIndex: Integer;
  LIsQuotedAlt: Boolean;
begin
  // AStart points at '['. Returns index of ']' that closes the class, or 0
  // if the class is unterminated.
  //
  // Two scanning modes:
  //   Legacy single-char class: leading ']' is treated as a literal so
  //     '[]abc]' is a valid class containing ']abc'.
  //   Quoted-alternation class: ']' inside a '"..."' span is part of the
  //     alternative literal and does NOT close the class.  An unterminated
  //     '"' (no closing quote before end of pattern) makes the class
  //     malformed (returns 0).
  LIndex := AStart + 1;

  if (LIndex <= Length(APattern)) and (APattern[LIndex] = '!') then
    Inc(LIndex);

  LIsQuotedAlt := (LIndex <= Length(APattern)) and (APattern[LIndex] = '"');

  if (not LIsQuotedAlt) and (LIndex <= Length(APattern)) and (APattern[LIndex] = ']') then
    Inc(LIndex);

  while LIndex <= Length(APattern) do
  begin
    if LIsQuotedAlt and (APattern[LIndex] = '"') then
    begin
      // Skip past the alternative's content to the closing '"'.
      Inc(LIndex);

      while (LIndex <= Length(APattern)) and (APattern[LIndex] <> '"') do
        Inc(LIndex);

      if LIndex > Length(APattern) then
        Exit(0);

      Inc(LIndex);
      Continue;
    end;

    if APattern[LIndex] = ']' then
      Exit(LIndex);

    Inc(LIndex);
  end;

  Result := 0;
end;

{ TWildCard - case-sensitive path }

class function TWildCard.CharInClassCS(const APattern: string; const AStart, AEnd: Integer; const AChar: Char): Boolean;
var
  LIndex: Integer;
  LNegate: Boolean;
begin
  LIndex := AStart + 1;
  LNegate := False;

  if APattern[LIndex] = '!' then
  begin
    LNegate := True;
    Inc(LIndex);
  end;

  Result := False;

  while LIndex < AEnd do
  begin
    // 'X-Y' is a range when '-' is followed by a real content character.
    // When '-' is the last content character (next position is the closing
    // ']'), it is treated as a literal.
    if (LIndex + 2 < AEnd) and (APattern[LIndex + 1] = '-') then
    begin
      if (AChar >= APattern[LIndex]) and (AChar <= APattern[LIndex + 2]) then
        Result := True;

      Inc(LIndex, 3);
    end
    else
    begin
      if AChar = APattern[LIndex] then
        Result := True;

      Inc(LIndex);
    end;
  end;

  if LNegate then
    Result := not Result;
end;

class function TWildCard.AltMatchesAtCS(const AInput: string; const AInputIdx: Integer;
  const APattern: string; const AAltStart, AAltLen: Integer): Boolean;
var
  LIdx: Integer;
begin
  // Empty alternative ('""') matches zero characters - always True.
  if AAltLen = 0 then
    Exit(True);

  if AInputIdx + AAltLen - 1 > Length(AInput) then
    Exit(False);

  for LIdx := 0 to AAltLen - 1 do
    if AInput[AInputIdx + LIdx] <> APattern[AAltStart + LIdx] then
      Exit(False);

  Result := True;
end;

class function TWildCard.MatchQuotedAltClassCS(const AInput, APattern: string;
  const AInputIdx, AClassStart, AClassEnd: Integer): Boolean;
var
  LIdx: Integer;
  LNegated: Boolean;
  LAfterClass: Integer;
  LAltStart, LAltLen: Integer;
  LAltContentEnd: Integer;
  LAfterAlt: Integer;
  LMaxLen: Integer;
  LAnyMatched: Boolean;
begin
  // AClassStart points at '[', AClassEnd at the closing ']'.  Iterates the
  // alternatives, recursing into MatchRecursiveCS on the remainder of the
  // pattern for each.  For the negated form, the longest alternative
  // dictates how much input the class consumes when it succeeds.
  //
  // Note: the separator check is done BEFORE attempting to match an alt,
  // so a malformed class (missing '|' between alts, stray garbage) fails
  // unconditionally regardless of what the first alt would have matched.
  LIdx := AClassStart + 1;
  LNegated := APattern[LIdx] = '!';

  if LNegated then
    Inc(LIdx);

  LAfterClass := AClassEnd + 1;

  if not LNegated then
  begin
    while LIdx < AClassEnd do
    begin
      if APattern[LIdx] <> '"' then
        Exit(False);

      LAltStart := LIdx + 1;
      LAltContentEnd := LAltStart;

      while (LAltContentEnd < AClassEnd) and (APattern[LAltContentEnd] <> '"') do
        Inc(LAltContentEnd);

      if LAltContentEnd >= AClassEnd then
        Exit(False);

      LAltLen := LAltContentEnd - LAltStart;
      LAfterAlt := LAltContentEnd + 1;

      if (LAfterAlt < AClassEnd) and (APattern[LAfterAlt] <> '|') then
        Exit(False);

      if AltMatchesAtCS(AInput, AInputIdx, APattern, LAltStart, LAltLen) then
        if MatchRecursiveCS(AInput, APattern, AInputIdx + LAltLen, LAfterClass) then
          Exit(True);

      if LAfterAlt < AClassEnd then
        LIdx := LAfterAlt + 1
      else
        LIdx := LAfterAlt;
    end;

    Result := False;
  end
  else
  begin
    LMaxLen := 0;
    LAnyMatched := False;

    while LIdx < AClassEnd do
    begin
      if APattern[LIdx] <> '"' then
        Exit(False);

      LAltStart := LIdx + 1;
      LAltContentEnd := LAltStart;

      while (LAltContentEnd < AClassEnd) and (APattern[LAltContentEnd] <> '"') do
        Inc(LAltContentEnd);

      if LAltContentEnd >= AClassEnd then
        Exit(False);

      LAltLen := LAltContentEnd - LAltStart;
      LAfterAlt := LAltContentEnd + 1;

      if (LAfterAlt < AClassEnd) and (APattern[LAfterAlt] <> '|') then
        Exit(False);

      if LAltLen > LMaxLen then
        LMaxLen := LAltLen;

      if (not LAnyMatched) and AltMatchesAtCS(AInput, AInputIdx, APattern, LAltStart, LAltLen) then
        LAnyMatched := True;

      if LAfterAlt < AClassEnd then
        LIdx := LAfterAlt + 1
      else
        LIdx := LAfterAlt;
    end;

    if LAnyMatched then
      Exit(False);

    if (LMaxLen > 0) and (AInputIdx + LMaxLen - 1 > Length(AInput)) then
      Exit(False);

    Result := MatchRecursiveCS(AInput, APattern, AInputIdx + LMaxLen, LAfterClass);
  end;
end;

class function TWildCard.MatchRecursiveCS(const AInput, APattern: string; AInputIdx, APatternIdx: Integer): Boolean;
var
  LClassEnd: Integer;
begin
  while APatternIdx <= Length(APattern) do
  begin
    case APattern[APatternIdx] of
      '*':
        begin
          while (APatternIdx <= Length(APattern)) and (APattern[APatternIdx] = '*') do
            Inc(APatternIdx);

          if APatternIdx > Length(APattern) then
            Exit(True);

          while AInputIdx <= Length(AInput) + 1 do
          begin
            if MatchRecursiveCS(AInput, APattern, AInputIdx, APatternIdx) then
              Exit(True);

            Inc(AInputIdx);
          end;

          Exit(False);
        end;
      '?':
        begin
          if AInputIdx > Length(AInput) then
            Exit(False);

          Inc(AInputIdx);
          Inc(APatternIdx);
        end;
      '#':
        begin
          if AInputIdx > Length(AInput) then
            Exit(False);

          if not IsAsciiDigit(AInput[AInputIdx]) then
            Exit(False);

          Inc(AInputIdx);
          Inc(APatternIdx);
        end;
      '[':
        begin
          LClassEnd := FindClassEnd(APattern, APatternIdx);
          if LClassEnd = 0 then
            Exit(False);

          if IsQuotedAltClass(APattern, APatternIdx) then
            Exit(MatchQuotedAltClassCS(AInput, APattern, AInputIdx, APatternIdx, LClassEnd));

          if AInputIdx > Length(AInput) then
            Exit(False);

          if not CharInClassCS(APattern, APatternIdx, LClassEnd, AInput[AInputIdx]) then
            Exit(False);

          Inc(AInputIdx);
          APatternIdx := LClassEnd + 1;
        end;
    else
      begin
        if AInputIdx > Length(AInput) then
          Exit(False);

        if AInput[AInputIdx] <> APattern[APatternIdx] then
          Exit(False);

        Inc(AInputIdx);
        Inc(APatternIdx);
      end;
    end;
  end;

  Result := AInputIdx > Length(AInput);
end;

{ TWildCard - case-insensitive path }

class function TWildCard.CharInClassCI(const APattern: string; const AStart, AEnd: Integer; const AChar: Char): Boolean;
var
  LIndex: Integer;
  LNegate: Boolean;
  LCharUpper: Char;
begin
  // Pattern is pre-upper-cased by the public Match dispatcher, so the
  // pattern-side chars need no conversion here.  Only the input char
  // is upper-cased - once, outside the class-walk loop.
  LCharUpper := FastToUpper(AChar);

  LIndex := AStart + 1;
  LNegate := False;

  if APattern[LIndex] = '!' then
  begin
    LNegate := True;
    Inc(LIndex);
  end;

  Result := False;

  while LIndex < AEnd do
  begin
    if (LIndex + 2 < AEnd) and (APattern[LIndex + 1] = '-') then
    begin
      if (LCharUpper >= APattern[LIndex]) and (LCharUpper <= APattern[LIndex + 2]) then
        Result := True;

      Inc(LIndex, 3);
    end
    else
    begin
      if LCharUpper = APattern[LIndex] then
        Result := True;

      Inc(LIndex);
    end;
  end;

  if LNegate then
    Result := not Result;
end;

class function TWildCard.AltMatchesAtCI(const AInput: string; const AInputIdx: Integer;
  const APattern: string; const AAltStart, AAltLen: Integer): Boolean;
var
  LIdx: Integer;
begin
  // Pattern alt content is pre-upper-cased by the public Match dispatcher,
  // so only the input side needs FastToUpper here.
  if AAltLen = 0 then
    Exit(True);

  if AInputIdx + AAltLen - 1 > Length(AInput) then
    Exit(False);

  for LIdx := 0 to AAltLen - 1 do
    if FastToUpper(AInput[AInputIdx + LIdx]) <> APattern[AAltStart + LIdx] then
      Exit(False);

  Result := True;
end;

class function TWildCard.MatchQuotedAltClassCI(const AInput, APattern: string;
  const AInputIdx, AClassStart, AClassEnd: Integer): Boolean;
var
  LIdx: Integer;
  LNegated: Boolean;
  LAfterClass: Integer;
  LAltStart, LAltLen: Integer;
  LAltContentEnd: Integer;
  LAfterAlt: Integer;
  LMaxLen: Integer;
  LAnyMatched: Boolean;
begin
  // See the CS variant for the matching/validation rules - the only
  // difference is the per-character upper-casing on the input side.
  LIdx := AClassStart + 1;
  LNegated := APattern[LIdx] = '!';

  if LNegated then
    Inc(LIdx);

  LAfterClass := AClassEnd + 1;

  if not LNegated then
  begin
    while LIdx < AClassEnd do
    begin
      if APattern[LIdx] <> '"' then
        Exit(False);

      LAltStart := LIdx + 1;
      LAltContentEnd := LAltStart;

      while (LAltContentEnd < AClassEnd) and (APattern[LAltContentEnd] <> '"') do
        Inc(LAltContentEnd);

      if LAltContentEnd >= AClassEnd then
        Exit(False);

      LAltLen := LAltContentEnd - LAltStart;
      LAfterAlt := LAltContentEnd + 1;

      if (LAfterAlt < AClassEnd) and (APattern[LAfterAlt] <> '|') then
        Exit(False);

      if AltMatchesAtCI(AInput, AInputIdx, APattern, LAltStart, LAltLen) then
        if MatchRecursiveCI(AInput, APattern, AInputIdx + LAltLen, LAfterClass) then
          Exit(True);

      if LAfterAlt < AClassEnd then
        LIdx := LAfterAlt + 1
      else
        LIdx := LAfterAlt;
    end;

    Result := False;
  end
  else
  begin
    LMaxLen := 0;
    LAnyMatched := False;

    while LIdx < AClassEnd do
    begin
      if APattern[LIdx] <> '"' then
        Exit(False);

      LAltStart := LIdx + 1;
      LAltContentEnd := LAltStart;

      while (LAltContentEnd < AClassEnd) and (APattern[LAltContentEnd] <> '"') do
        Inc(LAltContentEnd);

      if LAltContentEnd >= AClassEnd then
        Exit(False);

      LAltLen := LAltContentEnd - LAltStart;
      LAfterAlt := LAltContentEnd + 1;

      if (LAfterAlt < AClassEnd) and (APattern[LAfterAlt] <> '|') then
        Exit(False);

      if LAltLen > LMaxLen then
        LMaxLen := LAltLen;

      if (not LAnyMatched) and AltMatchesAtCI(AInput, AInputIdx, APattern, LAltStart, LAltLen) then
        LAnyMatched := True;

      if LAfterAlt < AClassEnd then
        LIdx := LAfterAlt + 1
      else
        LIdx := LAfterAlt;
    end;

    if LAnyMatched then
      Exit(False);

    if (LMaxLen > 0) and (AInputIdx + LMaxLen - 1 > Length(AInput)) then
      Exit(False);

    Result := MatchRecursiveCI(AInput, APattern, AInputIdx + LMaxLen, LAfterClass);
  end;
end;

class function TWildCard.MatchRecursiveCI(const AInput, APattern: string; AInputIdx, APatternIdx: Integer): Boolean;
var
  LClassEnd: Integer;
begin
  while APatternIdx <= Length(APattern) do
  begin
    case APattern[APatternIdx] of
      '*':
        begin
          while (APatternIdx <= Length(APattern)) and (APattern[APatternIdx] = '*') do
            Inc(APatternIdx);

          if APatternIdx > Length(APattern) then
            Exit(True);

          while AInputIdx <= Length(AInput) + 1 do
          begin
            if MatchRecursiveCI(AInput, APattern, AInputIdx, APatternIdx) then
              Exit(True);

            Inc(AInputIdx);
          end;

          Exit(False);
        end;
      '?':
        begin
          if AInputIdx > Length(AInput) then
            Exit(False);

          Inc(AInputIdx);
          Inc(APatternIdx);
        end;
      '#':
        begin
          if AInputIdx > Length(AInput) then
            Exit(False);

          if not IsAsciiDigit(AInput[AInputIdx]) then
            Exit(False);

          Inc(AInputIdx);
          Inc(APatternIdx);
        end;
      '[':
        begin
          LClassEnd := FindClassEnd(APattern, APatternIdx);
          if LClassEnd = 0 then
            Exit(False);

          if IsQuotedAltClass(APattern, APatternIdx) then
            Exit(MatchQuotedAltClassCI(AInput, APattern, AInputIdx, APatternIdx, LClassEnd));

          if AInputIdx > Length(AInput) then
            Exit(False);

          if not CharInClassCI(APattern, APatternIdx, LClassEnd, AInput[AInputIdx]) then
            Exit(False);

          Inc(AInputIdx);
          APatternIdx := LClassEnd + 1;
        end;
    else
      begin
        if AInputIdx > Length(AInput) then
          Exit(False);

        // Pattern is pre-upper-cased; only upper-case the input side.
        if FastToUpper(AInput[AInputIdx]) <> APattern[APatternIdx] then
          Exit(False);

        Inc(AInputIdx);
        Inc(APatternIdx);
      end;
    end;
  end;

  Result := AInputIdx > Length(AInput);
end;

{ TWildCard - instance API: Create overloads }

class function TWildCard.Create(const ACaseSensitive: Boolean): TWildCard;
begin
  Result.FCaseSensitive := ACaseSensitive;
  Result.FPatterns := nil;
end;

class function TWildCard.Create(const APattern: string;
  const ACaseSensitive: Boolean): TWildCard;
begin
  Result.FCaseSensitive := ACaseSensitive;
  SetLength(Result.FPatterns, 1);
  if ACaseSensitive then
    Result.FPatterns[0] := APattern
  else
    Result.FPatterns[0] := FastUpperString(APattern);
end;

class function TWildCard.Create(const APatterns: TArray<string>; const ACaseSensitive: Boolean): TWildCard;
var
  LIdx: Integer;
begin
  Result.FCaseSensitive := ACaseSensitive;
  SetLength(Result.FPatterns, Length(APatterns));

  if ACaseSensitive then
  begin
    for LIdx := 0 to High(APatterns) do
      Result.FPatterns[LIdx] := APatterns[LIdx];
  end
  else
  begin
    for LIdx := 0 to High(APatterns) do
      Result.FPatterns[LIdx] := FastUpperString(APatterns[LIdx]);
  end;
end;

class function TWildCard.Create(const APatterns: TStrings; const ACaseSensitive: Boolean): TWildCard;
var
  LIdx: Integer;
begin
  Result.FCaseSensitive := ACaseSensitive;
  SetLength(Result.FPatterns, APatterns.Count);

  if ACaseSensitive then
  begin
    for LIdx := 0 to APatterns.Count - 1 do
      Result.FPatterns[LIdx] := APatterns[LIdx];
  end
  else
  begin
    for LIdx := 0 to APatterns.Count - 1 do
      Result.FPatterns[LIdx] := FastUpperString(APatterns[LIdx]);
  end;
end;

{ TWildCard - instance API: Match overloads }

function TWildCard.MatchRegistered(const AInput: string): Boolean;
var
  LPattern: string;
begin
  // FPatterns are stored pre-prepared (upper-cased for CI), so we go
  // straight into the recursive engine with no per-call conversion cost.
  if FCaseSensitive then
  begin
    for LPattern in FPatterns do
      if MatchRecursiveCS(AInput, LPattern, 1, 1) then
        Exit(True);
  end
  else
  begin
    for LPattern in FPatterns do
      if MatchRecursiveCI(AInput, LPattern, 1, 1) then
        Exit(True);
  end;

  Result := False;
end;

function TWildCard.Match(const AInput: string): Boolean;
begin
  Result := MatchRegistered(AInput);
end;

function TWildCard.Match(const AInput, APattern: string; const AAlsoMatchRegistered: Boolean): Boolean;
begin
  // Try the ad-hoc pattern first (must FastUpperString it because it was
  // not pre-prepared at Create time).
  if FCaseSensitive then
    Result := MatchRecursiveCS(AInput, APattern, 1, 1)
  else
    Result := MatchRecursiveCI(AInput, FastUpperString(APattern), 1, 1);

  if Result then
    Exit;

  if AAlsoMatchRegistered then
    Result := MatchRegistered(AInput);
end;

function TWildCard.Match(const AInput: string; const APatterns: TArray<string>; const AAlsoMatchRegistered: Boolean): Boolean;
var
  LPattern: string;
begin
  if FCaseSensitive then
  begin
    for LPattern in APatterns do
      if MatchRecursiveCS(AInput, LPattern, 1, 1) then
        Exit(True);
  end
  else
  begin
    for LPattern in APatterns do
      if MatchRecursiveCI(AInput, FastUpperString(LPattern), 1, 1) then
        Exit(True);
  end;

  if AAlsoMatchRegistered then
    Result := MatchRegistered(AInput)
  else
    Result := False;
end;

function TWildCard.Match(const AInput: string; const APatterns: TStrings; const AAlsoMatchRegistered: Boolean): Boolean;
var
  LIdx: Integer;
begin
  if FCaseSensitive then
  begin
    for LIdx := 0 to APatterns.Count - 1 do
      if MatchRecursiveCS(AInput, APatterns[LIdx], 1, 1) then
        Exit(True);
  end
  else
  begin
    for LIdx := 0 to APatterns.Count - 1 do
      if MatchRecursiveCI(AInput, FastUpperString(APatterns[LIdx]), 1, 1) then
        Exit(True);
  end;

  if AAlsoMatchRegistered then
    Result := MatchRegistered(AInput)
  else
    Result := False;
end;

end.
