unit uclass;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
//  Variants,
  Dialogs,
  SvcMgr,
//  Forms,
  Graphics;

type
  TOnStatus = procedure(Sender: TObject; Status: string) of object;

//type
//  TCProcedure = procedure of object;

type
  TMyThread = class(TThread)
  public
    constructor Create(ThreadSuspended: Boolean = False); reintroduce; virtual;
  end;

type
  TCThreadProc = class;

  { estrutura para o funcionamenta da thread }
  PCThreadProcRec = ^TCThreadProcRec;
  TCThreadProcRec = record
  private
    procedure doInternalOnEnd;   // event informa o fim
  public
    OnIni: TNotifyEvent;   // event informa o inicio
    OnEnd: TNotifyEvent;   // event informa o fim
    CProc: TThreadMethod; //TCProcedure;
    CExec: TCThreadProc;
    CErro: Boolean;
    procedure doNotifyEnd;
  public
    function IsRun: Boolean;
    procedure Clear;
    procedure doRun;
    procedure doRunSync;
  end;

  TCThreadProc = class(TMyThread)
  private
    fSync: Boolean;
    fStop: Boolean; // variavel que informa que o processo foi parado
    Rec: PCThreadProcRec;
    procedure IniProcess;
    procedure EndProcess;
  protected
    procedure Execute; override; // rotina sobre carregada

  public
    property Terminated;
    constructor Create(var Rec: TCThreadProcRec; CSync: Boolean = False); reintroduce; // na criação da thread
  end;


  { TCThreadProc melhorada }
  //PThreadMethod =^TThreadMethod;
  TCThreadProcess = class(TThread)
  private
    m_SecBetweenRuns: Int32;
    m_OnIniProc: TNotifyEvent;
    m_OnEndProc: TNotifyEvent;
    //m_RunProc: TThreadProcedure ;
    procedure IniProc;
    procedure EndProc;
  protected
    procedure Execute; override;
    procedure RunProc; virtual ;
  public
    property OnIniProc: TNotifyEvent read m_OnIniProc write m_OnIniProc;
    property OnEndProc: TNotifyEvent read m_OnEndProc write m_OnEndProc;
    property SecBetweenRuns: Int32 read m_SecBetweenRuns write m_SecBetweenRuns;
    constructor Create(const aCreateSuspended: Boolean;
      const aFreeOnTerminate: Boolean = False); reintroduce;
  end;



implementation

uses Math, StrUtils, SyncObjs;


procedure ClearCThreadProcRec(var CRec: TCThreadProcRec);
begin
  CRec.OnIni := nil;
  CRec.OnEnd := nil;
  CRec.CProc := nil;
  CRec.CExec := nil;
  CRec.CErro := False;
end;

{ TMyThread }

constructor TMyThread.Create(ThreadSuspended: Boolean);
begin
  inherited Create(ThreadSuspended);
  FreeOnTerminate := True;
end;

{ TCThreadProcRec }

procedure TCThreadProcRec.Clear;
begin
  ClearCThreadProcRec(Self);

end;

procedure TCThreadProcRec.doInternalOnEnd;
begin
  Self.CExec :=nil;
end;

procedure TCThreadProcRec.doNotifyEnd;
begin
  Self.CExec :=nil;
end;

procedure TCThreadProcRec.doRun;
begin
  Self.CExec :=TCThreadProc.Create(Self);
end;

procedure TCThreadProcRec.doRunSync;
begin
  Self.CExec:=TCThreadProc.Create(Self);
end;

function TCThreadProcRec.IsRun: Boolean;
begin
  Result :=Assigned(Self.CExec);
end;

{ TCThreadProc }

constructor TCThreadProc.Create(var Rec: TCThreadProcRec; CSync: Boolean);
begin
  inherited Create();
  Self.fSync := CSync;
  Self.fStop := False;
  Self.Rec   := @Rec;
end;

procedure TCThreadProc.EndProcess;
begin
  if Assigned(Rec.OnEnd) then
  begin
    Rec.OnEnd(Self);
  end;
end;

procedure TCThreadProc.Execute;
  procedure do_error(message: string);
  begin
    MessageDlg(message, mtError, [], 0);
  end;

  procedure DoProcedure;
  begin
    try
      if Assigned(Rec.CProc) then
      begin
        if fSync then
        begin
          Self.Synchronize(Rec.CProc)
        end
        else
        begin
          Rec.CProc;
        end;
      end;
    except
      on E: Exception do
        if Rec.CErro then
        begin
          do_error(E.message);
        end;
    end;
    Rec.doInternalOnEnd;
  end;

begin
  if Assigned(Rec.OnIni) then
  try
    Synchronize(Self, IniProcess);
  except
    on E: Exception do
      do_error(E.message);
  end;

  DoProcedure;

  try
    Rec.doNotifyEnd;
  except
    on E: Exception do
      do_error(E.message);
  end;

  if Assigned(Rec.OnEnd) then
  try
    Synchronize(Self, EndProcess);
  except
    on E: Exception do
      do_error(E.message);
  end;

  DoTerminate;
end;

procedure TCThreadProc.IniProcess;
begin
  if Assigned(Rec.OnIni) then
  begin
    Rec.OnIni(Self);
  end;
end;


{ TCThreadProcess }

constructor TCThreadProcess.Create(const aCreateSuspended,
  aFreeOnTerminate: Boolean);
begin
    FreeOnTerminate :=aFreeOnTerminate;
    inherited Create(aCreateSuspended);
    m_SecBetweenRuns :=10;
end;

procedure TCThreadProcess.EndProc;
begin
    if Assigned(m_OnEndProc) then
    begin
        Self.m_OnEndProc(Self) ;
    end;
end;

procedure TCThreadProcess.Execute;
var
  Count: Integer;
begin
    //
    // inicio
    // sincroniza o método Form.OnINI
    if Assigned(Self.m_OnIniProc) then
    begin
        Synchronize(Self, Self.IniProc);
    end;

    Count :=0;

    //exec
    while not Terminated do  // loop around until we should stop
    begin
        Inc(Count);
//        if Count >= SecBetweenRuns then
//        begin
//            Count :=0;

            { place your service code here }
            { this is where the action happens }
            {if Assigned(Self.m_RunProc) then
            begin
                Self.m_RunProc() ;
            end;}
            Self.RunProc ;

//        end;
        Sleep(1000);
    end;

    //
    // fim
    // sincroniza o método Form.OnEND
    if Assigned(Self.m_OnEndProc) then
    begin
        Synchronize(Self, Self.EndProc);
    end;
end;


procedure TCThreadProcess.IniProc;
begin
    if Assigned(m_OnIniProc) then
    begin
        Self.m_OnIniProc(Self) ;
    end;
end;

procedure TCThreadProcess.RunProc;
begin

end;



end.
