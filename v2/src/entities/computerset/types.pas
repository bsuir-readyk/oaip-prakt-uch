unit e_computerset_t;

interface

uses
  SysUtils, s_types;

type
  PComputerSet = ^TComputerSet;
  TComputerSet = record
    id: Integer;
    cpu_id: Integer;
    motherboard_id: Integer;
    ram_id: Integer;
    psu_id: Integer;
    storage_id: Integer;
    totalCost: Real;
  end;
  
  
  PComputerSetNode = ^TComputerSetNode;
  TComputerSetNode = record
    data: TComputerSet;
    next: PComputerSetNode;
  end;

implementation

end.
