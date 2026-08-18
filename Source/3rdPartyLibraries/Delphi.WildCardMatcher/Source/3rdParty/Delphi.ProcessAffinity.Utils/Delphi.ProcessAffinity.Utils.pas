unit Delphi.ProcessAffinity.Utils;

 {
   Code is highly influenced by the code on Graphics32 project: 
     - https://github.com/graphics32/graphics32 
 }

interface

  function GetPerformanceAffinityMask(const AProcessHandle: THandle; const AForce: Boolean = False): NativeUInt;
  function GetAffinityMask(const AProcessHandle: THandle): NativeUInt;
  function GetSystemAffinityMask(const AProcessHandle: THandle): NativeUInt;
  function SetAffinityMask(const AProcessHandle: THandle; const ANewMask: NativeUInt): Boolean;
  procedure RestoreAffinityMask(const AProcessHandle: THandle);

implementation

uses
  Winapi.Windows, System.SysUtils;

type
  TEfficiencyArray = array[Byte] of KAFFINITY;

  // Declaration in Delphi 11 & 12.2 lacks EfficiencyClass
  TProcessorRelationship = record
    Flags: Byte;
    EfficiencyClass: Byte;
    Reserved: array[0..19] of Byte;
    GroupCount: Word;
    GroupMask: array[0..0] of GROUP_AFFINITY;
  end;

procedure BuildEfficiencyMap(var AEfficiencyMap: TEfficiencyArray; var ASize: Cardinal;
  var APProcessorInfo: PSystemLogicalProcessorInformationEx);
begin
  // For each efficiency class create a core mask

  while ASize > 0 do
  begin
    var LCoreMask: ^KAFFINITY;

    if APProcessorInfo.Relationship = RelationProcessorCore then
    begin
      LCoreMask := @AEfficiencyMap[TProcessorRelationship(APProcessorInfo.Processor).EfficiencyClass];

      for var LIndex := 0 to APProcessorInfo.Processor.GroupCount - 1 do
        LCoreMask^ := LCoreMask^ or APProcessorInfo.Processor.GroupMask[LIndex].Mask;
    end;

    Dec(ASize, APProcessorInfo.Size);
    Inc(PByte(APProcessorInfo), APProcessorInfo.Size);
  end;
end;

function CreateMaskFromEfficiencyMap(const AEfficiencyMap: TEfficiencyArray): NativeUInt;
begin
  Result := 0;
  var LIndex := 0;

  while LIndex < High(AEfficiencyMap) do
  begin
    if AEfficiencyMap[LIndex] <> 0 then
    begin
      // Assume the first performance class is "efficiency". Skip it.
      Inc(LIndex);

      while LIndex <= High(AEfficiencyMap) do
      begin
        Result := Result or AEfficiencyMap[LIndex];

        Inc(LIndex);
      end;

      Break;
    end;

    Inc(LIndex);
  end;
end;

function GetPerformanceAffinityMask(const AProcessHandle: THandle; const AForce: Boolean = False): NativeUInt;
var
  LProcessMask: NativeUInt;
  LSystemMask: NativeUInt;
  LSize: Cardinal;
  LProcessorInfoBuffer: TBytes;
  LPProcessorInfo: PSystemLogicalProcessorInformationEx;
  LEfficiencyMap: TEfficiencyArray;
begin
  Result := 0;

  // TProcessorRelationship.EfficiencyClass requires Windows 10
  if (not CheckWin32Version(10, 0)) then
    Exit;

  if not GetProcessAffinityMask(AProcessHandle, LProcessMask, LSystemMask) then
    Exit;

  // Skip if mask has already been modified
  if (not AForce) and (LProcessMask <> LSystemMask) then
    Exit;

  LSize := 0;
  if not GetLogicalProcessorInformationEx(RelationProcessorCore, nil, LSize) then
    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      Exit;

  SetLength(LProcessorInfoBuffer, LSize);
  LPProcessorInfo := @LProcessorInfoBuffer[0];

  if not GetLogicalProcessorInformationEx(RelationProcessorCore, PSystemLogicalProcessorInformation(LPProcessorInfo), LSize) then
    Exit;

  ZeroMemory(@LEfficiencyMap, SizeOf(LEfficiencyMap));

  // For each efficiency class create a core mask
  BuildEfficiencyMap(LEfficiencyMap, LSize, LPProcessorInfo);

  // Create a mask for performance cores
  Result := CreateMaskFromEfficiencyMap(LEfficiencyMap);

  // Obey SystemMask
  Result := LSystemMask and Result;
end;

function GetAffinityMask(const AProcessHandle: THandle): NativeUInt;
var
  LProcessMask: NativeUInt;
  LSystemMask: NativeUInt;
begin
  Result := 0;

  if not GetProcessAffinityMask(AProcessHandle, LProcessMask, LSystemMask) then
    Exit;

  // Mask the Process mask with System mask
  Result := LProcessMask and LSystemMask;
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


function SetAffinityMask(const AProcessHandle: THandle; const ANewMask: NativeUInt): Boolean;
var
  LProcessMask: NativeUInt;
  LSystemMask: NativeUInt;
begin
  Result := False;

  if not GetProcessAffinityMask(AProcessHandle, LProcessMask, LSystemMask) then
    Exit;

  // Mask the new mask with System mask
  var LNewMask: NativeUInt := ANewMask and LSystemMask;

  if (LNewMask <> 0) and (LNewMask <> LProcessMask) then
    if SetProcessAffinityMask(AProcessHandle, LNewMask) then
      Result := True;
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

end.
