unit e_componenttype_t;

interface

uses
  SysUtils, s_types;

type

  PComponentType = ^TComponentType;
  TComponentType = record
    id: Integer;
    name: TFixedString;
  end;

  PComponentTypeNode = ^TComponentTypeNode;
  TComponentTypeNode = record
    data: TComponentType;
    next: PComponentTypeNode;
  end;

implementation

end.
