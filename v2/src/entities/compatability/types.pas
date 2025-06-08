unit e_compatability_t;

interface

uses
  SysUtils, s_types;

type
  PCompatability = ^TCompatability;
  TCompatability = record
    left_id: Integer;
    right_id: Integer;
  end;

  PCompatabilityNode = ^TCompatabilityNode;
  TCompatabilityNode = record
    data: TCompatability;
    next: PCompatabilityNode;
  end;

  TAPCompatabilityNode = array of PCompatabilityNode;

implementation

end.
