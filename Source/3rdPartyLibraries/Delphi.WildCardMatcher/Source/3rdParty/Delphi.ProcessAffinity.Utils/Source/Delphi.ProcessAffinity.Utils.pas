unit Delphi.ProcessAffinity.Utils;

{
  Code is highly influenced by code from the Graphics32 project:
    - https://github.com/graphics32/graphics32

  Notes:
    - Windows only.
    - Affinity masks cover a single processor group (max 64 logical processors).
      On systems with more than one processor group the returned masks are not meaningful.
    - Functions do not raise; they return 0 (or False) on failure. When the failure
      comes from a WinAPI call, GetLastError holds the error code.
}

interface

  {$IFNDEF MSWINDOWS}
    {$MESSAGE Error 'Delphi.ProcessAffinity.Utils supports Windows only'}
  {$ENDIF}

  // Mask of the performance cores ("P-cores") of a hybrid CPU, masked with the system affinity mask.
  // Returns 0 when:
  //   - Windows is older than 10 (EfficiencyClass requires Windows 10)
  //   - the CPU is homogeneous, so there is no efficiency/performance split
  //   - the process affinity mask has already been modified (pass AForce = True to override)
  //   - a WinAPI call fails
  function GetPerformanceAffinityMask(const AProcessHandle: THandle; const AForce: Boolean = False): NativeUInt; overload;
  function GetPerformanceAffinityMask(const AForce: Boolean = False): NativeUInt; overload;

  // Mask of the efficiency cores ("E-cores") of a hybrid CPU, masked with the system affinity mask.
  // Returns 0 in the same situations as GetPerformanceAffinityMask.
  function GetEfficiencyAffinityMask(const AProcessHandle: THandle; const AForce: Boolean = False): NativeUInt; overload;
  function GetEfficiencyAffinityMask(const AForce: Boolean = False): NativeUInt; overload;

  // Process affinity mask, masked with the system affinity mask. Returns 0 on failure.
  function GetAffinityMask(const AProcessHandle: THandle): NativeUInt; overload;
  function GetAffinityMask: NativeUInt; overload;

  // System affinity mask. The WinAPI queries it through a process handle. Returns 0 on failure.
  function GetSystemAffinityMask(const AProcessHandle: THandle): NativeUInt; overload;
  function GetSystemAffinityMask: NativeUInt; overload;

  // Sets the process affinity mask. ANewMask is masked with the system affinity mask first.
  // Returns True when the mask was applied or was already in effect,
  // False when the resulting mask is empty or a WinAPI call fails.
  function SetAffinityMask(const AProcessHandle: THandle; const ANewMask: NativeUInt): Boolean; overload;
  function SetAffinityMask(const ANewMask: NativeUInt): Boolean; overload;

  // Restores the process affinity mask back to the system affinity mask.
  procedure RestoreAffinityMask(const AProcessHandle: THandle); overload;
  procedure RestoreAffinityMask; overload;

implementation

uses
  Winapi.Windows, System.SysUtils;

type
  TEfficiencyArray = array[Byte] of KAFFINITY;

  PKAffinity = ^KAFFINITY;

  // Declaration in Delphi 11 & 12.2 lacks EfficiencyClass.
  // GroupMask is declared with a generous upper bound so indexing it passes range
  // checking; only the first GroupCount elements exist in memory, and the record
  // is only ever accessed through a pointer, never instantiated.
  TProcessorRelationship = record
    Flags: Byte;
    EfficiencyClass: Byte;
    Reserved: array[0..19] of Byte;
    GroupCount: Word;
    GroupMask: array[0..255] of GROUP_AFFINITY;
  end;
  PProcessorRelationship = ^TProcessorRelationship;

procedure BuildEfficiencyMap(var AEfficiencyMap: TEfficiencyArray; const ASize: Cardinal;
  const APProcessorInfo: PSystemLogicalProcessorInformationEx);
var
  LSize: Cardinal;
  LPProcessorInfo: PSystemLogicalProcessorInformationEx;
  LProcessor: PProcessorRelationship;
  LCoreMask: PKAffinity;
  LIndex: Integer;
begin
  LSize := ASize;
  LPProcessorInfo := APProcessorInfo;

  ZeroMemory(@AEfficiencyMap, SizeOf(AEfficiencyMap));

  // For each efficiency class create a core mask
  while LSize > 0 do
  begin
    if LPProcessorInfo.Relationship = RelationProcessorCore then
    begin
      LProcessor := @LPProcessorInfo.Processor;
      LCoreMask := @AEfficiencyMap[LProcessor.EfficiencyClass];

      for LIndex := 0 to LProcessor.GroupCount - 1 do
        LCoreMask^ := LCoreMask^ or LProcessor.GroupMask[LIndex].Mask;
    end;

    Dec(LSize, LPProcessorInfo.Size);
    Inc(PByte(LPProcessorInfo), LPProcessorInfo.Size);
  end;
end;

procedure CreateMasksFromEfficiencyMap(const AEfficiencyMap: TEfficiencyArray; var AEfficiencyMask, APerformanceMask: NativeUInt);
var
  LIndex: Integer;
  LRest: Integer;
begin
  AEfficiencyMask := 0;
  APerformanceMask := 0;

  // Assume the lowest populated efficiency class holds the efficiency cores. Find it.
  LIndex := Low(AEfficiencyMap);
  while (LIndex <= High(AEfficiencyMap)) and (AEfficiencyMap[LIndex] = 0) do
    Inc(LIndex);

  if LIndex > High(AEfficiencyMap) then
    Exit;

  AEfficiencyMask := AEfficiencyMap[LIndex];

  // All higher classes are considered performance cores
  for LRest := LIndex + 1 to High(AEfficiencyMap) do
    APerformanceMask := APerformanceMask or AEfficiencyMap[LRest];

  // Homogeneous CPU: only one efficiency class, so there is no efficiency/performance split
  if APerformanceMask = 0 then
    AEfficiencyMask := 0;
end;

function GetHybridAffinityMask(const AProcessHandle: THandle; const APerformanceCores: Boolean; const AForce: Boolean): NativeUInt;
var
  LProcessMask: NativeUInt;
  LSystemMask: NativeUInt;
  LSize: Cardinal;
  LProcessorInfoBuffer: TBytes;
  LPProcessorInfo: PSystemLogicalProcessorInformationEx;
  LEfficiencyMap: TEfficiencyArray;
  LEfficiencyMask: NativeUInt;
  LPerformanceMask: NativeUInt;
begin
  Result := 0;

  // TProcessorRelationship.EfficiencyClass requires Windows 10
  if not CheckWin32Version(10, 0) then
    Exit;

  if not GetProcessAffinityMask(AProcessHandle, LProcessMask, LSystemMask) then
    Exit;

  // Skip if the mask has already been modified
  if (not AForce) and (LProcessMask <> LSystemMask) then
    Exit;

  LSize := 0;
  if not GetLogicalProcessorInformationEx(RelationProcessorCore, nil, LSize) then
    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      Exit;

  if LSize = 0 then
    Exit;

  SetLength(LProcessorInfoBuffer, LSize);
  LPProcessorInfo := @LProcessorInfoBuffer[0];

  // The cast works around the RTL declaring the buffer parameter with the non-Ex pointer type
  if not GetLogicalProcessorInformationEx(RelationProcessorCore, PSystemLogicalProcessorInformation(LPProcessorInfo), LSize) then
    Exit;

  // For each efficiency class create a core mask
  BuildEfficiencyMap(LEfficiencyMap, LSize, LPProcessorInfo);

  // Split the map into efficiency and performance core masks
  CreateMasksFromEfficiencyMap(LEfficiencyMap, LEfficiencyMask, LPerformanceMask);

  if APerformanceCores then
    Result := LPerformanceMask
  else
    Result := LEfficiencyMask;

  // Obey the system affinity mask
  Result := LSystemMask and Result;
end;

function GetPerformanceAffinityMask(const AProcessHandle: THandle; const AForce: Boolean = False): NativeUInt;
begin
  Result := GetHybridAffinityMask(AProcessHandle, True, AForce);
end;

function GetPerformanceAffinityMask(const AForce: Boolean = False): NativeUInt;
begin
  Result := GetPerformanceAffinityMask(GetCurrentProcess, AForce);
end;

function GetEfficiencyAffinityMask(const AProcessHandle: THandle; const AForce: Boolean = False): NativeUInt;
begin
  Result := GetHybridAffinityMask(AProcessHandle, False, AForce);
end;

function GetEfficiencyAffinityMask(const AForce: Boolean = False): NativeUInt;
begin
  Result := GetEfficiencyAffinityMask(GetCurrentProcess, AForce);
end;

function GetAffinityMask(const AProcessHandle: THandle): NativeUInt;
var
  LProcessMask: NativeUInt;
  LSystemMask: NativeUInt;
begin
  Result := 0;

  if not GetProcessAffinityMask(AProcessHandle, LProcessMask, LSystemMask) then
    Exit;

  // Mask the process mask with the system mask
  Result := LProcessMask and LSystemMask;
end;

function GetAffinityMask: NativeUInt;
begin
  Result := GetAffinityMask(GetCurrentProcess);
end;

function GetSystemAffinityMask(const AProcessHandle: THandle): NativeUInt;
var
  LProcessMask: NativeUInt;
  LSystemMask: NativeUInt;
begin
  Result := 0;

  if not GetProcessAffinityMask(AProcessHandle, LProcessMask, LSystemMask) then
    Exit;

  Result := LSystemMask;
end;

function GetSystemAffinityMask: NativeUInt;
begin
  Result := GetSystemAffinityMask(GetCurrentProcess);
end;

function SetAffinityMask(const AProcessHandle: THandle; const ANewMask: NativeUInt): Boolean;
var
  LProcessMask: NativeUInt;
  LSystemMask: NativeUInt;
  LNewMask: NativeUInt;
begin
  Result := False;

  if not GetProcessAffinityMask(AProcessHandle, LProcessMask, LSystemMask) then
    Exit;

  // Mask the new mask with the system mask
  LNewMask := ANewMask and LSystemMask;

  if LNewMask = 0 then
    Exit;

  // Already in effect, nothing to do
  if LNewMask = LProcessMask then
    Exit(True);

  Result := SetProcessAffinityMask(AProcessHandle, LNewMask);
end;

function SetAffinityMask(const ANewMask: NativeUInt): Boolean;
begin
  Result := SetAffinityMask(GetCurrentProcess, ANewMask);
end;

procedure RestoreAffinityMask(const AProcessHandle: THandle);
var
  LProcessMask: NativeUInt;
  LSystemMask: NativeUInt;
begin
  if not GetProcessAffinityMask(AProcessHandle, LProcessMask, LSystemMask) then
    Exit;

  if LProcessMask <> LSystemMask then
    SetProcessAffinityMask(AProcessHandle, LSystemMask);
end;

procedure RestoreAffinityMask;
begin
  RestoreAffinityMask(GetCurrentProcess);
end;

end.
