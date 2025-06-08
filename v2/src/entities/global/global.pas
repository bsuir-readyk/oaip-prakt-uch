unit e_global;

interface

uses
    s_logging,
    e_component_t,
    e_componenttype_t,
    e_compatability_t,
    e_computerset_t,
    e_order_t;
    

type TGData = record
    LogLevel: TLOG_LEVEL;
    Components: PComponentNode;
    NewComponentId: Integer;
    ComponentTypes: PComponentTypeNode;
    NewComponentTypeId: Integer;
    Compatabilities: PCompatabilityNode;
    Computersets: PComputerSetNode;
    NewcomputersetId: Integer;
    Orders: POrderNode;
    NewOrderId: Integer;
  end;

var GData: TGData;

procedure initGData();

implementation

procedure initGData();
var
    _PComponent: PComponentNode;
    _TComponent: TComponent;

    _PComponentTypeNode: PComponentTypeNode;
    _TComponentType: TComponentType;

    _PCompatabilityNode: PCompatabilityNode;
    _TCompatability: TCompatability;

    _PComputersetNode: PComputersetNode;
    _TComputerset: TComputerset;
    
    _POrderNode: POrderNode;
    _TOrder: TOrder;
begin
    _PComponent := new(PComponentNode);
    _PComponent^.data := _TComponent;
    _PComponent^.next := nil;

    _PComponentTypeNode := new(PComponentTypeNode);
    _PComponentTypeNode^.data := _TComponentType;
    _PComponentTypeNode^.next := nil;
    
    _PCompatabilityNode := new(PCompatabilityNode);
    _PCompatabilityNode^.data := _TCompatability;
    _PCompatabilityNode^.next := nil;

    _PComputersetNode := new(PComputersetNode);
    _PComputersetNode^.data := _TComputerset;
    _PComputersetNode^.next := nil;
    
    _POrderNode := new(POrderNode);
    _POrderNode^.data := _TOrder;
    _POrderNode^.next := nil;

    GData.Components := _PComponent;
    GData.NewComponentId := 0;

    GData.ComponentTypes := _PComponentTypeNode;
    GData.NewComponentId := 0;

    GData.Compatabilities := _PCompatabilityNode;

    GData.computersets := _PComputersetNode;
    GData.NewcomputersetId := 0;

    GData.Orders := _POrderNode;
    GData.NewOrderId := 0;

    GData.LogLevel := LL_INFO;
end;

end.
