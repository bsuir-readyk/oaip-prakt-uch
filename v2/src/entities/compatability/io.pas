unit e_compatability_io;

interface

uses
  SysUtils, Classes, Crt, e_global, e_compatability_t, s_logging;

procedure DisposeCompatabilityNodes(AListHead: PCompatabilityNode);
procedure LoadCompatabilitiesFromDatFile(const AFileName: string; var ACompatabilitiesList: PCompatabilityNode);
procedure SaveCompatabilitiesToDatFile(const AFileName: string; const ACompatabilitiesList: PCompatabilityNode);

implementation

procedure DisposeCompatabilityNodes(AListHead: PCompatabilityNode);
var
  CurrentNode, NextNode: PCompatabilityNode;
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

procedure SaveCompatabilitiesToDatFile(const AFileName: string; const ACompatabilitiesList: PCompatabilityNode);
var
  F: file of TCompatability;
  CurrentNode: PCompatabilityNode;
  log: TLogF;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Saving Compatabilities to ' + AFileName);
  AssignFile(F, AFileName);
  try
    Rewrite(F);
    CurrentNode := ACompatabilitiesList;
    if CurrentNode <> nil then // Skip potential dummy head
    begin
        CurrentNode := CurrentNode^.next;
    end;

    while CurrentNode <> nil do
    begin
      Write(F, CurrentNode^.data);
      CurrentNode := CurrentNode^.next;
    end;
    log(LL_INFO, 'Compatabilities saved successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error saving Compatabilities: ' + E.Message);
  end;
  CloseFile(F);
end;

procedure LoadCompatabilitiesFromDatFile(const AFileName: string; var ACompatabilitiesList: PCompatabilityNode);
var
  F: file of TCompatability;
  TempCompatability: TCompatability;
  NewNode, LastNode: PCompatabilityNode;
  log: TLogF;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Loading Compatabilities from ' + AFileName);

  // 1. Clear existing list properly
  if ACompatabilitiesList = nil then
  begin
    ACompatabilitiesList := new(PCompatabilityNode); // Create dummy head
    ACompatabilitiesList^.next := nil;
  end
  else
  begin
    DisposeCompatabilityNodes(ACompatabilitiesList);
  end;

  LastNode := ACompatabilitiesList;

  if not FileExists(AFileName) then
  begin
    log(LL_WARN, 'File ' + AFileName + ' not found. No Compatabilities loaded.');
    Exit;
  end;

  AssignFile(F, AFileName);
  try
    Reset(F);
    if Eof(F) then // Handle empty file case
    begin
      log(LL_INFO, 'File ' + AFileName + ' is empty. No Compatabilities loaded.');
      CloseFile(F);
      Exit;
    end;

    while not Eof(F) do
    begin
      Read(F, TempCompatability);
      NewNode := new(PCompatabilityNode);
      NewNode^.data := TempCompatability;
      NewNode^.next := nil;
      
      LastNode^.next := NewNode;
      LastNode := NewNode;
    end;
    log(LL_INFO, 'Compatabilities loaded successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error loading Compatabilities: ' + E.Message);
  end;
  CloseFile(F);
end;

end. 
