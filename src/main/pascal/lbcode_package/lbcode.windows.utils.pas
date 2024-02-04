unit LBCode.Windows.Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Windows, Generics.Collections;

procedure WaitForKeyPressOrTimeout(const Millis: integer);

procedure WaitForKeyPressOrTimeoutMsg(const Millis: integer);


procedure AddProgramFilesDirectories(constref StrList: TStringList);


function IsExe32onWindows32: boolean;

function IsExe32onWindows64: boolean;

function IsExe64onWindows64: boolean;

type
  TIntList = specialize TList<integer>;


function ListResourceTypes(const Module: HMODULE): TIntList;

function ListRCStringIDs(const Module: HMODULE): TIntList;

function GetRCString(const ResourceID: integer): string;


implementation


var
  Exe32onWin32, Exe32onWin64, Exe64onWin64: boolean;
  EnvProcArch, EnvProcArch6432: string;


procedure WaitForKeyPressOrTimeout(const Millis: integer);
var
  StartTime: DWORD;
  ElapsedTime: integer;
  InputRecords: TInputRecord;
  BytesRead: DWORD;
begin
  StartTime := GetTickCount; // Get current tick count in milliseconds

  repeat
    // Check if a key event is available in the console input buffer
    if PeekConsoleInput(GetStdHandle(STD_INPUT_HANDLE), InputRecords, 1, BytesRead) then
    begin
      if BytesRead > 0 then
      begin
        ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), InputRecords, 1, BytesRead);
        if (InputRecords.EventType = KEY_EVENT) and
          InputRecords.Event.KeyEvent.bKeyDown then
          Exit;
      end;
    end;

    // Calculate elapsed time in milliseconds
    ElapsedTime := GetTickCount - StartTime;

    Sleep(30);
  until (ElapsedTime >= Millis);
end;


procedure WaitForKeyPressOrTimeoutMsg(const Millis: integer);
begin
  WriteLn('Press any key or wait ', (Millis div 1000), ' seconds to continue . . .');
  WaitForKeyPressOrTimeout(Millis);
end;


procedure AddProgramFilesDirectories(constref StrList: TStringList);
var
  s:string;
begin
  if IsExe32onWindows64 or IsExe64onWindows64 then
  begin
    StrList.Add(SysUtils.GetEnvironmentVariable('ProgramW6432'));
    StrList.Add(SysUtils.GetEnvironmentVariable('ProgramFiles(x86)'));
  end
  else
  begin
    StrList.Add(SysUtils.GetEnvironmentVariable('ProgramFiles'));
  end;
end;


function IsExe32onWindows32: boolean;
begin
  Result := Exe32onWin32;
end;

function IsExe32onWindows64: boolean;
begin
  Result := Exe32onWin64;
end;

function IsExe64onWindows64: boolean;
begin
  Result := Exe64onWin64;
end;


function EnumResTypeProc(hModule: HMODULE; lpszType: PChar; lParam: longint): longbool;
  stdcall;
var
  List: TIntList;
begin
  list := TIntList(lParam);
  list.Add(integer(lpszType));
  Result := True;
end;


function ListResourceTypes(const Module: HMODULE): TIntList;
var
  ResList: TIntList;
begin
  ResList := TIntList.Create;
  EnumResourceTypes(Module, @EnumResTypeProc, longint(ResList));
  Result := ResList;
end;

function EnumResNameProc(hModule: HMODULE; lpszType: PChar; lpszName: PChar;
  lParam: longint): longbool; stdcall;
var
  List: TIntList;
begin
  list := TIntList(lParam);
  list.Add(integer(lpszName));
  Result := True;
end;

function ListRCStringIDs(const Module: HMODULE): TIntList;
var
  ResList: TIntList;
begin
  ResList := TIntList.Create;
  EnumResourceNames(Module, RT_RCDATA, @EnumResNameProc, longint(ResList));
  Result := ResList;
end;

function GetRCString(const ResourceID: integer): string;
var
  hResInfo, hResData: HRSRC;
  ResSize: DWORD;
  ResHandle: HGLOBAL;
  ResData: PChar;
begin
  Result := '';

  // Find the resource by its ID
  hResInfo := FindResource(HInstance, PChar(ResourceID), RT_RCDATA);
  if hResInfo = 0 then
    Exit;

  // Get the size of the resource
  ResSize := SizeofResource(HInstance, hResInfo);

  // Load the resource data
  ResHandle := LoadResource(HInstance, hResInfo);
  if ResHandle = 0 then
    Exit;

  // Lock the resource data
  ResData := LockResource(ResHandle);
  if ResData = nil then
    Exit;

  // Convert the resource data to a string
  SetString(Result, ResData, ResSize);

  // Unlock the resource (optional but included for completeness)
  UnlockResource(ResHandle);

  // Free the resource (optional but included for completeness)
  FreeResource(ResHandle);
end;



initialization
  begin
    EnvProcArch := SysUtils.GetEnvironmentVariable('PROCESSOR_ARCHITECTURE');
    EnvProcArch6432 := SysUtils.GetEnvironmentVariable('PROCESSOR_ARCHITEW6432');

    Exe32onWin64 := EnvProcArch.Equals('x86') and EnvProcArch6432.equals('AMD64');
    Exe64onWin64 := EnvProcArch.Equals('AMD64');
    Exe32onWin32 := not IsExe32onWindows64 and not IsExe64onWindows64;
  end;


end.
