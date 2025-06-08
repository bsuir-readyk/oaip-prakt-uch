unit e_component_io;

interface

uses
  SysUtils, Classes, Crt, e_global, e_component_t, s_logging;

procedure DisposeComponentNodes(AListHead: PComponentNode);
procedure LoadComponentsFromDatFile(const AFileName: string; var AComponentsList: PComponentNode; var ANextId: Integer);
procedure SaveComponentsToDatFile(const AFileName: string; const AComponentsList: PComponentNode);

implementation

procedure DisposeComponentNodes(AListHead: PComponentNode);
var
  CurrentNode, NextNode: PComponentNode;
begin
  if AListHead = nil then Exit;

  CurrentNode := AListHead^.next; // Start with the first actual data node
  while CurrentNode <> nil do
  begin
    NextNode := CurrentNode^.next;
    dispose(CurrentNode);
    CurrentNode := NextNode;
  end;
  AListHead^.next := nil; // Mark the list as empty after the dummy head
end;

procedure SaveComponentsToDatFile(const AFileName: string; const AComponentsList: PComponentNode);
var
  F: file of TComponent;
  CurrentNode: PComponentNode;
  log: TLogF;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Saving Components to ' + AFileName);
  AssignFile(F, AFileName);
  try
    Rewrite(F);
    CurrentNode := AComponentsList;
    if CurrentNode <> nil then // Skip potential dummy head if data starts at .next
    begin
        CurrentNode := CurrentNode^.next;
    end;

    while CurrentNode <> nil do
    begin
      Write(F, CurrentNode^.data);
      CurrentNode := CurrentNode^.next;
    end;
    log(LL_INFO, 'Components saved successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error saving Components: ' + E.Message);
  end;
  CloseFile(F);
end;

procedure LoadComponentsFromDatFile(const AFileName: string; var AComponentsList: PComponentNode; var ANextId: Integer);
var
  F: file of TComponent;
  TempComponent: TComponent;
  NewNode, LastNode: PComponentNode;
  log: TLogF;
  MaxId: Integer;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Loading Components from ' + AFileName);

  // 1. Clear existing list properly
  if AComponentsList = nil then
  begin
    AComponentsList := new(PComponentNode); // Create dummy head
    AComponentsList^.next := nil;
  end
  else
  begin
    DisposeComponentNodes(AComponentsList); // Dispose existing nodes after dummy head
  end;

  LastNode := AComponentsList; // Start with dummy head to append after it
  MaxId := -1;
  ANextId := 0; // Default next ID if file not found or empty

  if not FileExists(AFileName) then
  begin
    log(LL_WARN, 'File ' + AFileName + ' not found. No Components loaded.');
    Exit;
  end;

  AssignFile(F, AFileName);
  try
    Reset(F);
    if Eof(F) then // Handle empty file case
    begin
      log(LL_INFO, 'File ' + AFileName + ' is empty. No Components loaded.');
      CloseFile(F);
      Exit;
    end;

    while not Eof(F) do
    begin
      Read(F, TempComponent);
      NewNode := new(PComponentNode);
      NewNode^.data := TempComponent;
      NewNode^.next := nil;
      
      LastNode^.next := NewNode;
      LastNode := NewNode;

      if TempComponent.id > MaxId then
        MaxId := TempComponent.id;
    end;
    log(LL_INFO, 'Components loaded successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error loading Components: ' + E.Message);
  end;
  CloseFile(F);
  ANextId := MaxId + 1;
end;

end. 
