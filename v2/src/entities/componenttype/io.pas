unit e_componenttype_io;

interface

uses
  SysUtils, Classes, Crt, e_global, e_componenttype_t, s_logging;

procedure DisposeComponentTypeNodes(AListHead: PComponentTypeNode);
procedure LoadComponentTypesFromDatFile(const AFileName: string; var AComponentTypesList: PComponentTypeNode; var ANextId: Integer);
procedure SaveComponentTypesToDatFile(const AFileName: string; const AComponentTypesList: PComponentTypeNode);

implementation

procedure DisposeComponentTypeNodes(AListHead: PComponentTypeNode);
var
  CurrentNode, NextNode: PComponentTypeNode;
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

procedure SaveComponentTypesToDatFile(const AFileName: string; const AComponentTypesList: PComponentTypeNode);
var
  F: file of TComponentType;
  CurrentNode: PComponentTypeNode;
  log: TLogF;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Saving ComponentTypes to ' + AFileName);
  AssignFile(F, AFileName);
  try
    Rewrite(F);
    CurrentNode := AComponentTypesList;
    if CurrentNode <> nil then // Skip potential dummy head if data starts at .next
    begin
        CurrentNode := CurrentNode^.next;
    end;

    while CurrentNode <> nil do
    begin
      Write(F, CurrentNode^.data);
      CurrentNode := CurrentNode^.next;
    end;
    log(LL_INFO, 'ComponentTypes saved successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error saving ComponentTypes: ' + E.Message);
  end;
  CloseFile(F);
end;

procedure LoadComponentTypesFromDatFile(const AFileName: string; var AComponentTypesList: PComponentTypeNode; var ANextId: Integer);
var
  F: file of TComponentType;
  TempComponentType: TComponentType;
  NewNode, LastNode: PComponentTypeNode;
  log: TLogF;
  MaxId: Integer;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Loading ComponentTypes from ' + AFileName);

  // 1. Clear existing list properly
  if AComponentTypesList = nil then // Should not happen if GData is initialized
  begin
    AComponentTypesList := new(PComponentTypeNode); // Create dummy head
    AComponentTypesList^.next := nil;
  end
  else
  begin
    DisposeComponentTypeNodes(AComponentTypesList); // Dispose existing nodes after dummy head
  end;
  
  LastNode := AComponentTypesList; // Start with dummy head to append after it
  MaxId := -1;
  ANextId := 0; // Default next ID if file not found or empty

  if not FileExists(AFileName) then
  begin
    log(LL_WARN, 'File ' + AFileName + ' not found. No ComponentTypes loaded.');
    Exit;
  end;

  AssignFile(F, AFileName);
  try
    Reset(F);
    if Eof(F) then // Handle empty file case
    begin
      log(LL_INFO, 'File ' + AFileName + ' is empty. No ComponentTypes loaded.');
      CloseFile(F);
      Exit;
    end;

    while not Eof(F) do
    begin
      Read(F, TempComponentType);
      NewNode := new(PComponentTypeNode);
      NewNode^.data := TempComponentType;
      NewNode^.next := nil;
      
      LastNode^.next := NewNode; // Append new node
      LastNode := NewNode;       // Move LastNode to the new node

      if TempComponentType.id > MaxId then
        MaxId := TempComponentType.id;
    end;
    log(LL_INFO, 'ComponentTypes loaded successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error loading ComponentTypes: ' + E.Message);
  end;
  CloseFile(F);
  ANextId := MaxId + 1;
end;

end. 
