unit LBCode.SystemUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure AddSubdirectoriesMaxDepth(constref DirList: TStringList;
  const BaseDir: string; const MaxDepth: integer);


function GetMemBlockIdSize(const Obj: TObject): string;


function GetExpandedEnvironmentStrings(const OrgStr: string;
  const MaxLength: integer = 4096): string;


implementation

uses
  JwaWinBase;

procedure AddSubdirectoriesMaxDepth(constref DirList: TStringList;
  const BaseDir: string; const MaxDepth: integer);
var
  SearchRec: TSearchRec;
  FindResult: integer;
  SubdirPath: string;
begin
  if MaxDepth = 0 then
    exit;

  if DirList = nil then
    raise EArgumentException.Create('DirList can''t be nil.');

  FindResult := FindFirst(IncludeTrailingPathDelimiter(BaseDir) +
    '*.*', faDirectory, SearchRec);
  try
    while FindResult = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
        (SearchRec.Attr and faDirectory <> 0) then
      begin
        SubdirPath := IncludeTrailingPathDelimiter(BaseDir) + SearchRec.Name;
        if DirList.IndexOf(SubdirPath) = -1 then
          DirList.Add(SubdirPath);
        AddSubdirectoriesMaxDepth(DirList, SubdirPath, MaxDepth - 1);
      end;
      FindResult := FindNext(SearchRec);
    end;
  finally
    SubdirPath := '';
    SysUtils.FindClose(SearchRec);
  end;
end;


// returns block id/size in "block $015ED068 size 16" format,
// ie same as used by HeapTrc
function GetMemBlockIdSize(const Obj: TObject): string;
var
  BlockId: string;
  BlockSize: string;
begin
  BlockId := PtrUInt(Obj).ToHexString();
  BlockSize := MemSize(Obj).ToString;
  Result := 'block $' + BlockId + ' size ' + BlockSize;
end;


// unknown env vars are left as is, ie "%envvar%", _not_ empty string
function GetExpandedEnvironmentStrings(const OrgStr: string;
  const MaxLength: integer = 4096): string;
var
  s: string;
  i: integer;
begin
  SetLength(s, MaxLength);
  i := ExpandEnvironmentStrings(PChar(OrgStr), PChar(s), MaxLength);
  SetLength(s, i - 1);
  Result := s;
end;


end.
