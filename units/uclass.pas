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
  TGetIntProc = procedure(const I: Int64) of object;
  TGetBooProc = procedure(const B: Boolean) of object;

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
    constructor Create(var Rec: TCThreadProcRec; CSync: Boolean = False); reintroduce; // na cria��o da thread
  end;


  { TThread melhorada }
  TCThreadProcess = class(TThread)
  private
    m_SecBetweenRuns: Int32;
    //
    // pointer da view (form) que indica o inicio da Thread
    // utilizado para trava de alguns controles
    // nota: todos os controles travados aqui, deve ser liberado em OnTerminate
    m_OnBeforeExecute: TNotifyEvent;
    //
    // pointer da view (form) que substitui o method Execute
    // pode ser apontado de varios locais
    m_OnExecute: TNotifyEvent;
    //
    // pointer da view (form) que recebe uma string
    // pode ser usado para amostra de status
    m_OnStrProc: TGetStrProc;
    //
    // pointer da view (form) que recebe um Int64
    // pode ser usado para amostra em ProgressBar
    m_OnIntProc: TGetIntProc;
  protected
    m_Interval: Cardinal ;
    procedure Execute; override;
    //
    // procedure ligado ao method Execute
    procedure RunProc; virtual ;
    //
    // procedure que indica o inicio da Thread
    procedure CallOnBeforeExecute;
    procedure CallOnExecute;
    procedure CallOnStrProc(const aStr: string); overload ;
    procedure CallOnStrProc(const aStr: string; const args: array of const); overload ;
    procedure CallOnIntProc(const aInt: Int64);
  public
    property Terminated;
    property SecBetweenRuns: Int32 read m_SecBetweenRuns write m_SecBetweenRuns;
    property OnBeforeExecute: TNotifyEvent
        read m_OnBeforeExecute
        write m_OnBeforeExecute;
    property OnExecute: TNotifyEvent
      read m_OnExecute
      write m_OnExecute;
    property OnStrProc: TGetStrProc
        read m_OnStrProc
        write m_OnStrProc;
    property OnIntProc: TGetIntProc
        read m_OnIntProc
        write m_OnIntProc;
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
// A m�gica � que o m�todo an�nimo captura a vari�vel aStr.
// Assim, ser� totalmente seguro usar o Queue em vez de Synchronize aqui.
begin
    //
    // Estou usando um pequeno truque aqui para garantir que o c�digo cr�tico
    // seja chamado dentro do thread da GUI sem ter um m�todo separado. O ID do
    // thread da GUI � armazenado em MainThreadID, portanto, apenas verifique
    // se o resultado da fun��o GetCurrentThreadID corresponde a esse valor e
    // chame Synchronize caso contr�rio.
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
    // Uma nota usando o Queue: quando o thread � liberado, todas as chamadas de
    // Queue pendentes s�o removidas. Portanto, se o seu c�digo depender de todas
    // essas chamadas sendo tratadas no thread da GUI, voc� deve certificar-se
    // de que a inst�ncia do thread tenha pelo menos esse tempo.
end;

procedure TCThreadProcess.CallOnExecute;
begin
    if GetCurrentThreadId = MainThreadID then
    begin
        if Assigned(m_OnExecute) then m_OnExecute(Self);
    end
    else begin
        Synchronize(CallOnExecute);
    end;
end;

procedure TCThreadProcess.CallOnIntProc(const aInt: Int64);
begin
    if GetCurrentThreadId = MainThreadID then
    begin
        if Assigned(m_OnIntProc) then
            m_OnIntProc(aInt);
    end
    else begin
        Synchronize(
            procedure
            begin
                CallOnIntProc(aInt);
            end);
    end;
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
    // sincroniza o method da view (form), como inicio de tarefa
    CallOnBeforeExecute;

    Count :=0;
    m_Interval :=0 ;

    //
    // desvio
    if Assigned(m_OnExecute) then
    begin
        CallOnExecute ;
    end
    else begin
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
            Inc(m_Interval) ;
        end;
    end;
end;

procedure TCThreadProcess.RunProc;
begin

end;

end.
