unit s_utils;

interface

uses
  SysUtils, s_types;

function stringRepeat(s: string; n: Integer): string;
function getEvenIdx(a: TArrayInt): TArrayInt;

implementation

function stringRepeat(s: string; n: Integer): string;
var
    i: Integer;
begin
    result := '';
    for i := 1 to n do
    begin
        result := result + s;
    end;
end;

function getEvenIdx(a: TArrayInt): TArrayInt;
var
    i: integer;
begin
    setlength(result, 0);
    for i := low(a) to high(a) do
    begin
        if (i mod 2 = 0) then
        begin
            setlength(result, length(result) + 1);
            result[length(result) - 1] := a[i];
        end;
    end;
end;

function getOddIdx(a: TArrayInt): TArrayInt;
var
    i: integer;
begin
    setlength(result, 0);
    for i := low(a) to high(a) do
    begin
        if (i mod 2 = 1) then
        begin
            setlength(result, length(result) + 1);
            result[length(result) - 1] := a[i];
        end;
    end;
end;

end.
