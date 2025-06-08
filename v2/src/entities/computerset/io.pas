unit e_computerset_io;

interface

uses
  SysUtils, Classes, Crt, e_global, e_computerset_t, s_logging;

procedure DisposeComputerSetNodes(AListHead: PComputerSetNode);
procedure LoadComputerSetsFromDatFile(const AFileName: string; var AComputerSetsList: PComputerSetNode; var ANextId: Integer);
procedure SaveComputerSetsToDatFile(const AFileName: string; const AComputerSetsList: PComputerSetNode);

implementation

procedure DisposeComputerSetNodes(AListHead: PComputerSetNode);
var
  CurrentNode, NextNode: PComputerSetNode;
begin
  if AListHead = nil then Exit;
  CurrentNode := AListHead^.next;
  while CurrentNode <> nil do
  begin
    NextNode := CurrentNode^.next;
    dispose(CurrentNode);
    CurrentNode := NextNode;
  end;
  AListHead^.next := nil;
end;

procedure SaveComputerSetsToDatFile(const AFileName: string; const AComputerSetsList: PComputerSetNode);
var
  F: file of TComputerSet;
  CurrentNode: PComputerSetNode;
  log: TLogF;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Saving ComputerSets to ' + AFileName);
  AssignFile(F, AFileName);
  try
    Rewrite(F);
    CurrentNode := AComputerSetsList;
    if CurrentNode <> nil then // Skip dummy head
    begin
        CurrentNode := CurrentNode^.next;
    end;

    while CurrentNode <> nil do
    begin
      Write(F, CurrentNode^.data);
      CurrentNode := CurrentNode^.next;
    end;
    log(LL_INFO, 'ComputerSets saved successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error saving ComputerSets: ' + E.Message);
  end;
  CloseFile(F);
end;

procedure LoadComputerSetsFromDatFile(const AFileName: string; var AComputerSetsList: PComputerSetNode; var ANextId: Integer);
var
  F: file of TComputerSet;
  TempComputerSet: TComputerSet;
  NewNode, LastNode: PComputerSetNode;
  log: TLogF;
  MaxId: Integer;
begin
  log := GetLogger(GData.LogLevel);
  log(LL_INFO, 'Loading ComputerSets from ' + AFileName);

  if AComputerSetsList = nil then
  begin
    AComputerSetsList := new(PComputerSetNode);
    AComputerSetsList^.next := nil;
  end
  else
  begin
    DisposeComputerSetNodes(AComputerSetsList);
  end;

  LastNode := AComputerSetsList;
  MaxId := -1;
  ANextId := 0;

  if not FileExists(AFileName) then
  begin
    log(LL_WARN, 'File ' + AFileName + ' not found. No ComputerSets loaded.');
    Exit;
  end;

  AssignFile(F, AFileName);
  try
    Reset(F);
    if Eof(F) then
    begin
      log(LL_INFO, 'File ' + AFileName + ' is empty. No ComputerSets loaded.');
      CloseFile(F);
      Exit;
    end;

    while not Eof(F) do
    begin
      Read(F, TempComputerSet);
      NewNode := new(PComputerSetNode);
      NewNode^.data := TempComputerSet;
      NewNode^.next := nil;
      
      LastNode^.next := NewNode;
      LastNode := NewNode;

      if TempComputerSet.id > MaxId then
        MaxId := TempComputerSet.id;
    end;
    log(LL_INFO, 'ComputerSets loaded successfully.');
  except
    on E: Exception do
      log(LL_ERR, 'Error loading ComputerSets: ' + E.Message);
  end;
  CloseFile(F);
  ANextId := MaxId + 1;
end;

end. 
