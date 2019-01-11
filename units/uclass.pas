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
    m_OnBeforeExecute: TNotifyEvent;
    m_OnStrProc: TGetStrProc;
  protected
    procedure Execute; override;
    procedure RunProc; virtual ;
    procedure CallOnBeforeExecute;
    procedure CallOnStrProc(const aStr: string); overload ;
    procedure CallOnStrProc(const aStr: string; const args: array of const); overload ;
  public
    property Terminated;
    property SecBetweenRuns: Int32 read m_SecBetweenRuns write m_SecBetweenRuns;
    property OnBeforeExecute: TNotifyEvent
        read m_OnBeforeExecute
        write m_OnBeforeExecute;
    property OnStrProc: TGetStrProc
        read m_OnStrProc
        write m_OnStrProc;
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

procedure TCThreadProcess.CallOnBeforeExecute;
begin
    if GetCurrentThreadId = MainThreadID then
    begin
        if Assigned(m_OnBeforeExecute) then m_OnBeforeExecute(Self);
    end
    else begin
        Synchronize(CallOnBeforeExecute);
    end;
end;

procedure TCThreadProcess.CallOnStrProc(const aStr: string);
//
// A mágica é que o método anônimo captura a variável aStr.
// Assim, será totalmente seguro usar o Queue em vez de Synchronize aqui.
begin
    //
    // Estou usando um pequeno truque aqui para garantir que o código crítico
    // seja chamado dentro do thread da GUI sem ter um método separado. O ID do
    // thread da GUI é armazenado em MainThreadID, portanto, apenas verifique
    // se o resultado da função GetCurrentThreadID corresponde a esse valor e
    // chame Synchronize caso contrário.
    if GetCurrentThreadId = MainThreadID then
    begin
        if Assigned(m_OnStrProc) then
            m_OnStrProc(aStr);
    end
    else begin
        Synchronize(
            procedure
            begin
                CallOnStrProc(aStr);
            end);
    end;
    //
    // Uma nota usando o Queue: quando o thread é liberado, todas as chamadas de
    // Queue pendentes são removidas. Portanto, se o seu código depender de todas
    // essas chamadas sendo tratadas no thread da GUI, você deve certificar-se
    // de que a instância do thread tenha pelo menos esse tempo.
end;

procedure TCThreadProcess.CallOnStrProc(const aStr: string;
  const args: array of const);
begin
    CallOnStrProc(Format(aStr, args));
end;

constructor TCThreadProcess.Create(const aCreateSuspended,
  aFreeOnTerminate: Boolean);
begin
    FreeOnTerminate :=aFreeOnTerminate;
    inherited Create(aCreateSuspended);
    m_SecBetweenRuns :=10;
end;

procedure TCThreadProcess.Execute;
var
  Count: Integer;
begin
    //
    // inicio
    // sincroniza o method do Form, como incicio de tarefa
    CallOnBeforeExecute;

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
end;

procedure TCThreadProcess.RunProc;
begin

end;

end.
