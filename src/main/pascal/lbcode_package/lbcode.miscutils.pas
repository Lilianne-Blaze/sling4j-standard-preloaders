unit LBCode.MiscUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function CompareBool(const Left, Right: boolean): integer;


implementation


function CompareBool(const Left, Right: boolean): integer;
begin
  if left = right then
    Exit(0);

  Result := Ord(Left) - Ord(Right);
end;


end.
