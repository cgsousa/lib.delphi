{***
* Synchronize and Queue with Parameters
* Todos os direitos reservados
* Autor: Carlos Gonzaga
* Data: 11.03.2017
********************************************************************************
* Na classe TThread do Delphi, há um método Synchronize para chamar no contexto
* do thread da GUI. Isso é necessário no caso, por exemplo, de você querer
* atualizar uma barra de progresso ou um rótulo de status em um formulário,
* porque a VCL não é thread-safe. Enquanto Synchronize é um método de bloqueio
* (ou seja, o código de thread continua quando o thread da GUI terminou a chamada
* do método), as versões recentes do Delphi introduziram um método de Queue sem
* bloqueio.
*
* A desvantagem de usar Synchronize é que é muito difícil lidar com parâmetros
* durante uma chamada desse tipo. A solução padrão é usar campos no descendente
* TThread que mantêm os parâmetros durante a Sincronização, mas isso não funcionará
* corretamente com o Queue. Como a Queue não está bloqueando, pode haver várias
* chamadas à Queue esperando para serem processadas, o que geralmente não pode
* compartilhar o mesmo campo. Pior, usar o acesso à Queue para esses campos também
* precisa ser thread-safe. O encadeamento de Queues pode apenas definir esse campo
* para a próxima chamada para Queue enquanto uma chamada de Queue anterior é
* executada no encadeamento principal usando apenas esse campo. Como esse problema
* não existe com o Synchronize, a mudança de Synchronize para Queue só precisa ser
* feita com mais cuidado.
*
* Uma maneira de lidar com parâmetros para Synchronize é o uso de Métodos Anônimos.
* Há uma versão sobrecarregada de Synchronize e Queue, tendo um TThreadProcedure
* como parâmetro:
*
*   TThreadMethod = procedure of object;
*   TThreadProcedure = reference to procedure;
*   ...
*   procedure Queue(AMethod: TThreadMethod); overload;
*   procedure Synchronize(AMethod: TThreadMethod); overload;
*   procedure Queue(AThreadProc: TThreadProcedure); overload;
*   procedure Synchronize(AThreadProc: TThreadProcedure); overload;
*
* Um exemplo simples para uma chamada parametrizada de Synchronize esta nas
* implementações do métodos:
*
*   procedure CallOnStrProc(const aStr: string); overload ;
*   procedure CallOnIntProc(const aInt: Int64);
*
*}
unit uclass;

{*
******************************************************************************
|* PROPÓSITO: Registro de Alterações
******************************************************************************

Símbolo : Significado

[+]     : Novo recurso
[*]     : Recurso modificado/melhorado
[-]     : Correção de Bug (assim esperamos)

*}

interface

uses
  SysUtils,
  Classes;



type
  SvcUtil = Object
    function IsServiceInstalled(const srv_name: string): Boolean;
  end;





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
    constructor Create(var Rec: TCThreadProcRec; CSync: Boolean = False); reintroduce; // na criação da thread
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


function CalcPerc(const aDividend, aDivisor: Extended): Word ;



implementation

uses Windows, Math, StrUtils, SyncObjs,
  //
  // services
  SvcMgr,
  WinSvc;


function CalcPerc(const aDividend, aDivisor: Extended): Word ;
var
  frc: Single;
begin
    frc :=aDividend /aDivisor ;
    frc :=frc *100 ;
    Result :=Trunc(frc) ;
    if Result = 0 then
        Result :=1;
end;




{ SvcUtil }

function SvcUtil.IsServiceInstalled(const srv_name: string): Boolean;
const
  //
  // assume that the total number of
  // services is less than 4096.
  // increase if necessary
  cnMaxServices = 4096;

type
  TSvcA = array[0..cnMaxServices]
          of TEnumServiceStatus;
  PSvcA = ^TSvcA;

var
  //
  // temp. use
  j: Integer;

  //
  // service control
  // manager handle
  schm: SC_Handle;

  //
  // bytes needed for the
  // next buffer, if any
  nBytesNeeded,

  //
  // number of services
  nServices,

  //
  // pointer to the
  // next unread service entry
  nResumeHandle: DWord;

  //
  // service status array
  ssa : PSvcA;

begin
    Result :=False ;

    // connect to the service
    // control manager
    schm :=OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
    if schm > 0 then
    begin
        nResumeHandle :=0;

        New(ssa);

        EnumServicesStatus(
          schm,
          SERVICE_WIN32,
          SERVICE_STATE_ALL,
          ssa^[0],
          SizeOf(ssa^),
          nBytesNeeded,
          nServices,
          nResumeHandle );


        //
        // assume that our initial array
        // was large enough to hold all
        // entries. add code to enumerate
        // if necessary.
        //

        for j := 0 to nServices-1 do
        begin
            if StrPas(
              ssa^[j].lpServiceName ) =srv_name then
            begin
                Result :=True;
                Break ;
            end;
        end;

        Dispose(ssa);

        // close service control
        // manager handle
        CloseServiceHandle(schm);

    end;
end;

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
//    MessageDlg(message, mtError, [], 0);
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
