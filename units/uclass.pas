{***
* Synchronize and Queue with Parameters
* Todos os direitos reservados
* Autor: Carlos Gonzaga
* Data: 11.03.2017
********************************************************************************
* Na classe TThread do Delphi, h� um m�todo Synchronize para chamar no contexto
* do thread da GUI. Isso � necess�rio no caso, por exemplo, de voc� querer
* atualizar uma barra de progresso ou um r�tulo de status em um formul�rio,
* porque a VCL n�o � thread-safe. Enquanto Synchronize � um m�todo de bloqueio
* (ou seja, o c�digo de thread continua quando o thread da GUI terminou a chamada
* do m�todo), as vers�es recentes do Delphi introduziram um m�todo de Queue sem
* bloqueio.
*
* A desvantagem de usar Synchronize � que � muito dif�cil lidar com par�metros
* durante uma chamada desse tipo. A solu��o padr�o � usar campos no descendente
* TThread que mant�m os par�metros durante a Sincroniza��o, mas isso n�o funcionar�
* corretamente com o Queue. Como a Queue n�o est� bloqueando, pode haver v�rias
* chamadas � Queue esperando para serem processadas, o que geralmente n�o pode
* compartilhar o mesmo campo. Pior, usar o acesso � Queue para esses campos tamb�m
* precisa ser thread-safe. O encadeamento de Queues pode apenas definir esse campo
* para a pr�xima chamada para Queue enquanto uma chamada de Queue anterior �
* executada no encadeamento principal usando apenas esse campo. Como esse problema
* n�o existe com o Synchronize, a mudan�a de Synchronize para Queue s� precisa ser
* feita com mais cuidado.
*
* Uma maneira de lidar com par�metros para Synchronize � o uso de M�todos An�nimos.
* H� uma vers�o sobrecarregada de Synchronize e Queue, tendo um TThreadProcedure
* como par�metro:
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
* implementa��es do m�todos:
*
*   procedure CallOnStrProc(const aStr: string);
*   procedure CallOnIntProc(const aInt: Int64);
*
*}
unit uclass;

{*
******************************************************************************
|* PROP�SITO: Registro de Altera��es
******************************************************************************

S�mbolo : Significado

[+]     : Novo recurso
[*]     : Recurso modificado/melhorado
[-]     : Corre��o de Bug (assim esperamos)

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
  TGetExceptProc = procedure(Sender: TObject; const E: Exception) of object;

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
  strict private
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
    // pointer da view (form) que indica o inicio da Thread
    // utilizado para trava de alguns controles
    // nota: todos os controles travados aqui, deve ser liberado em OnTerminate
    m_OnINI: TNotifyEvent;

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
    // indicador do termino
    procedure DoTerminate; override;

    //
    // procedure que indica o inicio da Thread
    procedure CallOnBeforeExecute;
    procedure CallOnExecute;
    procedure CallOnINI;
    procedure CallOnIntProc(const aInt: Int64);
    procedure CallOnStrProc(const aStr: string); overload ;
    procedure CallOnStrProc(const aStr: string; const args: array of const); overload ;
  public
    property Terminated;
    property SecBetweenRuns: Int32 read m_SecBetweenRuns write m_SecBetweenRuns;

    property OnBeforeExecute: TNotifyEvent
        read m_OnBeforeExecute
        write m_OnBeforeExecute;

    property OnExecute: TNotifyEvent
        read m_OnExecute
        write m_OnExecute;

    property OnINI: TNotifyEvent read m_OnINI write m_OnINI;

    property OnIntProc: TGetIntProc
        read m_OnIntProc
        write m_OnIntProc;

    property OnStrProc: TGetStrProc
        read m_OnStrProc
        write m_OnStrProc;

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
        //Queue();
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

procedure TCThreadProcess.CallOnINI;
begin
    if GetCurrentThreadId = MainThreadID then
    begin
        if Assigned(m_OnINI) then m_OnINI(Self);
    end
    else begin
        Synchronize(CallOnINI);
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
    m_SecBetweenRuns :=1;
end;

procedure TCThreadProcess.DoTerminate;
begin
    inherited DoTerminate ;
    //
    // finaliza recursos
end;

procedure TCThreadProcess.Execute;
var
  Count: Integer;
begin
    //
    // inicio
    // sincroniza o method da view (form), como inicio de tarefa
    if Assigned(m_OnINI) then
        CallOnINI
    else
        CallOnBeforeExecute;

    //
    // desvio execute
    if Assigned(m_OnExecute) then
        m_OnExecute(Self)
    //
    // execute normal
    else begin
        //
        // inicia 1a chamada
        if SecBetweenRuns > 1 then
        begin
            Self.RunProc ;
        end;

        //
        // inicializa contadores
        Count :=0;
        m_Interval :=0 ;

        //
        // loop around until we should stop
        while not Terminated do
        begin
            //
            // caso aconte�a algum error/except
            // garante a proxima exec.
            try
              if SecBetweenRuns > 1 then
              begin
                  Inc(Count);
                  if Count >= SecBetweenRuns then
                  begin
                      Count :=0;
                      Self.RunProc ;
                  end;
              end
              //
              // run em 1s
              else begin
                  Self.RunProc ;
              end;
              //Self.RunProc ;
              //
              // wait um sec
              //Sleep(1000 *SecBetweenRuns);
              Sleep(1000);
              Inc(m_Interval) ;
            except
              on E:Exception do
              begin
                  CallOnStrProc(E.Message);
              end;
            end;
        end;
    end;
end;

procedure TCThreadProcess.RunProc;
begin

end;


end.
