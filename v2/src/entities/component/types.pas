unit e_component_t;

interface

uses
  SysUtils, s_types;

type
  PComponent = ^TComponent;
  TComponent = record
    id: Integer;
    cTypeId: Integer;
    manufacturer_name: TFixedString;
    model_name: TFixedString;
    description: TFixedString;
    cost: Real;
    availability: Integer;
  end;
  
  PComponentNode = ^TComponentNode;
  TComponentNode = record
    data: TComponent;
    next: PComponentNode;
  end;

implementation

end.
