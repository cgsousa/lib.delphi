{***
* Extensão de classes ADO para um melhor desenpenho de recursos
* Autor: Carlos GONZAGA
*}
unit uadodb;


{.$DEFINE ADOCONN_EXTEND}

interface

uses
  Classes,
  DB,
  ADODB,
  SysUtils,
  IniFiles,
  Generics.Collections
  ;


type
  TFrmtStrDateTime = (sdDMY_HNS, sdDMY_HN, sdDMY_H, sdDMY, sdHNS, sdHN);
  TFrmtStr = record
  public
    class function Cur(const AValue: Currency): string; static;
    class function Dat(const AValue: TDateTime;
                       const AFormat: TFrmtStrDateTime = sdDMY_HN): string; static;
    class function Int(const AValue: Int32): string ; static;
    class function Flt(const AValue: Extended; const ADec: Word): string; static;
  public
    class function SQLStr(const AStr: string;
                          const ATrunc: Integer = 0): string ; static;
  end;


  CFrmtStr = class
  public
    class function Flt(const AValue: Extended; const ADec: Word =2): string;
    class function Cur(const AValue: Currency): string ;
    class function Int(const AValue: Int64): string ;
    class function Dat(const AValue: TDateTime; const AIncTime: Boolean =False): string;
//    class function Zero(const AValue: Cardinal; const A): string ;
  end;


{$REGION 'TMemIniFile'}
type
  TMemIniFile = class( IniFiles.TMemIniFile)
  private
    _section: string ;
  public
    function ReadStr(const AIdent: string; const ASection: string = ''): string ;
    function ReadInt(const AIdent: string; const ASection: string = ''): Integer ;
    function ReadBoo(const AIdent: string; const ASection: string = ''): Boolean ;
    function ReadTime(const AIdent: string; const ASection: string = ''): TDateTime ; overload ;
    //
    procedure WriteValue(const AIdent: string ; const AValue: Variant;
      const ASection: string = '');
  end;

{$ENDREGION}


type
  TCEmpresa = class;
  TCEmissor = class;
  TCWS = class
  private
    _owner: TCEmissor;
  private
    _view: Boolean;
    _pass,_user: string;
    _saveSOAP: Boolean;
    _trans: Boolean;
    _ambpro: Boolean ;
    _envsinc: Boolean;
    _timeout: Integer;
  public
    property view: Boolean read _view;
    property passWord: string read _pass;
    property userName: string read _user;
    property saveSOAP: Boolean read _saveSOAP;
    property trans: Boolean read _trans;
    property ambPro: Boolean read _ambpro;
    property envSinc: Boolean read _envsinc;
    property timeOut: Integer read _timeout;
  public
    constructor Create(AOwner: TCEmissor);
  end;

  TCEmissor = class
  private
    _owner: TCEmpresa;
  private
    _prefeitura: string ;
    _save: Boolean ;
    _localSave: string ;
    _localIniProvedor: string ;
    _localSchemas: string ;
  protected
    _cnpj: string;
    _rzsocial: string;
    _ie,_im: string;
    _cnae,_codsvc: string;
    _munici,_siafi: string;
    _token: string;
    _toprps: Integer ;
    //cert
    _localPFX: string ;
    _senha: string ;
    _numeroSerie: string ;
  public
    property CNPJ: string read _cnpj;
    property rzSocial: string read _rzsocial;
    property InscE: string read _ie;
    property InscM: string read _im;
    property codigoAtividade: string read _cnae;
    property codigoServico: string read _codsvc write _codsvc;
    property municipio: string read _munici ;
    property SIAFI: string read _siafi;
    property Token: string read _token write _token;
    property TopRPS: Integer read _toprps;
    property LocalPFX: string read _localPFX;
    property Senha: string read _senha;
    property numeroSerie: string read _numeroserie write _numeroserie;
  public
    property prefeitura: string read _prefeitura;
    property salvar: Boolean read _save;
    property localSalva: string read _localSave;
    property localIniProvedor: string read _localIniProvedor;
    property localSchemas: string read _localSchemas;
  public
    WS: TCWS ;
    constructor Create(AOwner: TCEmpresa);
  end;

  TCEmpresa = class
  private
    class var FInstance: TCEmpresa;
    class function GetInstance: TCEmpresa; static;
  private
    m_DateServ: TDateTime ;
    IniFileName: string ;
    procedure DoIniLoad();
  public
    class property Instance : TCEmpresa read GetInstance;
    class function UnFormatCNPJ(const Doc: string): string;
    class function UnFormatIE(const Doc: string): string;
  public
    codfil: SmallInt;
    CNPJ: string;
    xFant: string;
    RzSoci: string;
    IE: string;
    xEnder: string;
    Numero: string;
    Comple: string;
    Bairro: string;
    CodMun: Integer;
    Munici: string;
    UF: string;
    CEP: string;
    Fone: string;
    Email: string;
    CodCta: SmallInt ;
    Logo: TStream;
  public
    property DateServ: TDateTime read m_DateServ;
    procedure DoLoad(const codfil: SmallInt);
    procedure DoIniSave() ;
  end;


type
  TFieldCassHlp = class helper for DB.TField
    function ToString: string ;
  end;

type

  EConnectionADO = class(EDatabaseError)
    constructor Create();
  end;

  TParameters = class helper for ADODB.TParameters
    function AddParamStr(const AName: string; const AValue: string;
      const ASize: Integer): TParameter;
    function AddParamInt(const AName: string; const AValue: Integer): TParameter;
    function AddParamFlt(const AName: string; const AValue: Double;
      const ASize, APresicion: Word): TParameter;
    function AddParamDatetime(const AName: string; const AValue: TDateTime): TParameter;
  end;

  TCustomADODataSet = class helper for ADODB.TCustomADODataSet
  public
    function FStr(const Value: string): string;
  end;

  TCColumnDef = class
  public
    Index: Integer;
    Name: string ;
    cTyp: TFieldType;
    cVal: Variant;
    cValBlob: TStream;
    cKey: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

type
//  IADOQuery = Interface(IInterface)
//  end;

  TADOQuery = class(ADODB.TADOQuery)
  private
    m_FileName: string ;
    m_TimeBeforeOpen: TDateTime;
    m_TimeAfterOpen: TDateTime;
    procedure OnBeforeOpen(DataSet: TDataSet);
    procedure OnAfterOpen(DataSet: TDataSet);
  public
    constructor NewADOQuery(const aParamCheck: Boolean
      {;const AUniDirectional: Boolean = False} ); overload ;
    constructor NewADOQuery(aConnStr: WideString); overload ;
    function AddCmd(ACmd: string): Integer ; overload ;
    function AddCmd(ACmd: string; Args: array of const): Integer ; overload ;
    function AddParamWithValue(const AName: string; const AType: TFieldType;
      const AValue: Variant;
      const ASize: Integer = 0;
      const APrecision: Byte = 0): TParameter;
    function AddParamDatetime(const AName: string; const AValue: TDateTime;
      const AIncTime: Boolean = False): TParameter;
    function ConnectAndOpen: Boolean ;
  public
    function Field(const AFieldName: string): TField; overload;
    function Field(const AFieldIndex: Integer): TField; overload;
    function Param(const AParamName: string): TParameter; overload;
    function Param(const AParamIndex: Integer): TParameter; overload;
  public
    procedure SaveToFile(const aFileName: string = '0.SQL');
    procedure DoClear ;
  public
    class function getDateTime: TDateTime ;
    class function getHost(out host_id, host_nm: string): Boolean ;
    class function getNewID(): string ;
    class function getVersion(): string ;
    class function getCompLevel(): SmallInt ;
    class function ident_current(const table_name: string): Integer ;
    class function Exists(const sql_dml: string): Boolean ;
  end;

  TADOStoredProc = class(ADODB.TADOStoredProc)
  public
    constructor NewADOStoredProc(const AStoredProcName: string;
      const AParamCheck: Boolean =False); overload ;
    constructor NewADOStoredProc(const aConnStr, aStoredProcName: string); overload ;
    function AddParamWithValue(const AName: string;
      const AType: TFieldType ;
      const AValue: Variant): TParameter;
    function AddParamDatetime(const AName: string; const aValue: TDateTime;
      const aIncTime: Boolean = False): TParameter;
    function AddParamOut(const AName: string;
      const AType: TFieldType): TParameter;
    function AddParamRet(const AName: string): TParameter;
    function Param(const AParamName: string): TParameter; overload;
    function Param(const AParamIndex: Integer): TParameter; overload;
  end;

//  TCommandDML = (cmdInsert, cmdUpdade) ;
  TADOCommand = class(ADODB.TADOCommand)
  private
    _columns: TList<TCColumnDef> ;
  public
    constructor NewADOCommand(const AParamCheck: Boolean); overload;
    constructor NewADOCommand(const aConnStr: WideString); overload;
    procedure AddCmd(ACmd: string); overload ;
    procedure AddCmd(ACmd: string; Args: array of const); overload ;
    function FStr(const Value: string): string;
  public
    function AddParamWithValue(const AName: string;
      const AType: TFieldType ;
      const AValue: Variant;
      const ADirection: TParameterDirection = pdInput): TParameter;
    function AddColumn(const AName: string;
      const AType: TFieldType ;
      const AValue: Variant;
      const AKey: Boolean = False): TCColumnDef;
    function AddColumnBlob(const AName: string;
      const AValue: TStream): TCColumnDef;
  public
    procedure SaveToFile(const AFileName: string = '0.SQL');
    procedure ExecInsert(const ATableName: string;
      const ABeginTran: Boolean = True);
    procedure ExecUpdate(const ATableName: string;
      const ABeginTran: Boolean = True);
    function Param(const AParamName: string): TParameter; overload;
    function Param(const AParamIndex: Integer): TParameter; overload;
  end;

type
  IGenSerial = interface
    ['{168E21AE-F871-4F6E-B20A-EFDA38CDA9EE}']
    function getIdent: string ;
    property ident: string read getIdent ;
    function getValue: Cardinal;
    property Value: Cardinal read getValue;
    function getIniVal: Cardinal;
    property iniVal: Cardinal read getIniVal;
    function getIncVal: Int32 ;
    property incVal: Int32 read getIncVal;
    function getMinVal: Int32 ;
    property minVal: Int32 read getMinVal;
    function getMaxVal: Cardinal;
    property maxVal: Cardinal read getMaxVal;
    function getDescri: string ;
    property descri: string read getDescri ;
    function getCatego: string ;
    property catego: string read getCatego;
    procedure setValue ;
    function nextValue: Cardinal ;
    function readValue: Boolean ;
  end;

  TCGenSerial = class(TInterfacedObject, IGenSerial)
  private
    m_ident: string ;
    m_value: Cardinal;
    m_inival: Cardinal;
    m_incval: Int32 ;
    m_minval: Int32 ;
    m_maxval: Cardinal;
    m_descri: string ;
    m_catego: string ;
    function getIdent: string ;
    function getValue: Cardinal;
    function getIniVal: Cardinal;
    function getIncVal: Int32 ;
    function getMinVal: Int32 ;
    function getMaxVal: Cardinal;
    function getDescri: string ;
    function getCatego: string ;
  public
    property ident: string read getIdent ;
    property Value: Cardinal read getValue;
    property iniVal: Cardinal read getIniVal;
    property incVal: Int32 read getIncVal;
    property minVal: Int32 read getMinVal;
    property maxVal: Cardinal read getMaxVal;
    property descri: string read getDescri ;
    property catego: string read getCatego;
    procedure setValue ;
    function nextValue: Cardinal ;
    function readValue: Boolean ;
    constructor Create(const aIdent, aDescri, aCatego: string ;
      const aIniVal: Cardinal;
      const aIncVal, aMinVal: Int32 ;
      const aMaxVal: Cardinal); reintroduce ;
    class function New(const aIdent, aDescri, aCatego: string ;
      const aIniVal: Cardinal;
      const aIncVal, aMinVal: Int32 ;
      const aMaxVal: Cardinal): IGenSerial ;
  end;


type // thread-safe
  ObjConnectionString =object
  strict private
    m_provider: string ;
    m_server: string;
    m_uid: string ;
    m_pwd: string ;
    m_database: string;
    m_trustedConnection: Boolean;
    m_MARS: Boolean;
    m_dtCompatibility: Word;
  private
    m_Date: TDateTime ;
    m_LevelComp: Word ;
    procedure doInitVars ;
  public
    property Server: string read m_server;
    property UserID: string read m_uid;
    property Passwd: string read m_pwd;
    property Database: string read m_database;
    constructor Create(const aFileName: string);
    function Build: string ;
  End;



var
  ConnectionADO: ADODB.TADOConnection;
  LocalFormatSettings: TFormatSettings;
  Empresa: TCEmpresa ;
  ConnectionString: String = '';
  dbConnectionString: ObjConnectionString;


function DesencriptarVar(Texto: String):String;

function buildConnectionStringFromIniFile(const aFileName: string): String;

function NewADOConnFromIniFile(const AFileName: string): ADODB.TADOConnection ;


//
// funcs Conn
function getConnDatetime(): TDateTime ;
function getConnCompLevel(): Word;


implementation

uses ActiveX, Math, StrUtils, DateUtils, IOUtils, Variants,
  ustr, uini;

var
  dbConnCompLevel: Word;

function DesencriptarVar(Texto: String):String;
var w : string;
i : integer;
begin
FOR i := 1 TO Length(texto) do
w := w + chr( Ord(texto[i]) - i - 19 );
result:= w;
end;

function buildConnectionStringFromIniFile(const aFileName: string): String;
//  const aTrustedConnection: Boolean;
//  const aDataTypeCompatibility:Word);
var
  I: IMemIniFile ;
  provider,server,user,pass,database: string ;
  trustedConnection,mars: Boolean;
  dtCompatibility: Word;
begin
  //
    if TFile.Exists(aFileName) then
    begin
        Result :='Persist Security Info=false;Packet Size=4096';
        I :=TCMemIniFile.New(aFileName);
        I.Section :='Banco de Dados Local';

        provider :=UpperCase(I.ValStr('Provider'));
        server  :=I.ValStr('Servidor');
        user :=I.ValStr('Usuario');
        pass:=DesencriptarVar(I.ValStr('Senha'));
        database :=I.ValStr('Banco');
        trustedConnection:=I.ValBoo('TrustedConnection');
        mars :=I.ValBoo('MARS');

        if provider = '' then
        begin
            provider :='sqloledb.1';
        end;

        //
        // ms sql > 2000
        if Pos('SQLNCLI',provider) > 0 then
        begin

            dtCompatibility :=I.ValInt('DataTypeCompatibility',80);

            Result :=Result +Format(';Provider=%s',[provider]);
            Result :=Result +Format(';Server=%s',[server]);
            Result :=Result +Format(';Database=%s',[database]);
            //
            // Trusted connection
            if trustedConnection then
                Result :=Result +';Trusted_Connection=yes'
            //
            // Standard security
            else begin
                Result :=Result +Format(';Uid=%s',[user]);
                Result :=Result +Format(';Pwd=%s',[pass]);
            end;
            //
            // Enable MARS (Multiple Active Result Sets)
            if mars then
                Result :=Result +';MARS Connection=True';
            //
            // DataTypeCompatibility
            if dtCompatibility > 0 then
                Result :=Result +Format(';DataTypeCompatibility=%d',[dtCompatibility]);
        end
        //
        // ms sql <= 2000
        else begin
            Result :=Result +Format(';Provider=%s',[provider]);
            Result :=Result +Format(';Workstation ID=%s',[server]);
            Result :=Result +Format(';Data Source=%s',[server]);
            Result :=Result +Format(';Initial Catalog=%s',[database]);
            //
            // Trusted connection
            if trustedConnection then
                Result :=Result +';Integrated Security=SSPI'
            //
            // Standard security
            else begin
                Result :=Result +Format(';User ID=%s',[user]);
                Result :=Result +Format(';Password=%s',[pass]);
            end;
            //
            // Enable MARS (Multiple Active Result Sets)
            if mars then
                Result :=Result +';MultipleActiveResultSets=True';
        end;
        //
        // finaliza string
        Result :=Result +';';
    end;
end;

function getConnDatetime(): TDateTime ;
var
  Q: TADOQuery ;
begin
    Result :=0;
    if uadodb.ConnectionString <> '' then
        Q :=TADOQuery.NewADOQuery(uadodb.ConnectionString)
    else
        Q :=TADOQuery.NewADOQuery(False) ;
    try
        Q.AddCmd('select getdate() as dh_sistem');
        Q.Open;
        if not Q.IsEmpty then
            Result :=Q.Field('dh_sistem').AsDateTime ;
    finally
        Q.Free ;
    end;
end;

function getConnCompLevel(): Word;
var
  Q: TADOQuery;
  S: string;
  E: Integer;
begin
    if dbConnCompLevel > 0 then
        Result :=dbConnCompLevel
    else begin
        Result :=0;
        if uadodb.ConnectionString <> '' then
            Q :=TADOQuery.NewADOQuery(uadodb.ConnectionString)
        else
            Q :=TADOQuery.NewADOQuery(False) ;

        try
            Q.AddCmd('declare @pro_ver sysname; set @pro_ver =%s',[Q.FStr('ProductVersion')]);
            Q.AddCmd('select serverproperty(@pro_ver) as pro_ver');
            try
                Q.Open ;
                S :=Q.Field('pro_ver').AsString;
            except Result :=0 end;
        finally
            Q.Free ;
        end;
        //
        // trata retorno
        Result :=Pos('.', S);
        if Result > 0 then
        begin
            S :=Copy(S, 1, Result-1);
            Val(S, Result, E);
        end
        else
            Result :=0 ;
        dbConnCompLevel :=Result;
    end;
end;

function NewADOConnFromIniFile(const AFileName: string): ADODB.TADOConnection ;
const
  SECTION = 'Banco de Dados Local';
var
  M: TMemIniFile ;
begin
    Result :=nil;
    if TFile.Exists(AFileName) then
    begin
//        CoInitialize(0) ;
        Result :=TADOConnection.Create(nil);
        M :=TMemIniFile.Create(AFileName);
        try
            Result.ConnectionString :='Provider=SQLOLEDB.1;Persist Security Info=True;Packet Size=4096';
            Result.ConnectionString :=Result.ConnectionString +';Workstation ID=' +M.ReadString(SECTION,'Servidor','');
            Result.ConnectionString :=Result.ConnectionString +';Data Source='    +M.ReadString(SECTION,'Servidor','');
            Result.ConnectionString :=Result.ConnectionString +';User ID='        +M.ReadString(SECTION,'Usuario','');
            Result.ConnectionString :=Result.ConnectionString +';Password='       +DesencriptarVar(M.ReadString(SECTION,'Senha',''));
            Result.ConnectionString :=Result.ConnectionString +';Initial Catalog='+M.ReadString(SECTION,'Banco','');
            Result.LoginPrompt:=False;
            Result.KeepConnection :=True;
        finally
            M.Free ;
        end;
    end;
end;


{ ObjConnectionString }

function ObjConnectionString.Build: string;
begin
    Result :='Persist Security Info=True;Packet Size=4096';
    //
    // ms sql > 2000
    if Pos('SQLNCLI',m_provider) > 0 then
    begin
        Result :=Result +Format(';Provider=%s',[m_provider]);
        Result :=Result +Format(';Server=%s',[m_server]);
        Result :=Result +Format(';Database=%s',[m_database]);
        //
        // Trusted connection
        if m_trustedConnection then
            Result :=Result +';Trusted_Connection=yes'
        //
        // Standard security
        else begin
            Result :=Result +Format(';Uid=%s',[m_uid]);
            Result :=Result +Format(';Pwd=%s',[m_pwd]);
        end;
        //
        // Enable MARS (Multiple Active Result Sets)
        if m_MARS then
            Result :=Result +';MARS Connection=True';
        //
        // DataTypeCompatibility
        if m_dtCompatibility > 0 then
            Result :=Result +Format(';DataTypeCompatibility=%d',[m_dtCompatibility]);
    end
    //
    // ms sql <= 2000
    else begin
        Result :=Result +Format(';Provider=%s',[m_provider]);
        Result :=Result +Format(';Workstation ID=%s',[m_server]);
        Result :=Result +Format(';Data Source=%s',[m_server]);
        Result :=Result +Format(';Initial Catalog=%s',[m_database]);
        //
        // Trusted connection
        if m_trustedConnection then
            Result :=Result +';Integrated Security=SSPI'
        //
        // Standard security
        else begin
            Result :=Result +Format(';User ID=%s',[m_uid]);
            Result :=Result +Format(';Password=%s',[m_pwd]);
        end;
        //
        // Enable MARS (Multiple Active Result Sets)
        if m_MARS then
            Result :=Result +';MultipleActiveResultSets=True';
    end;
    //
    // finaliza string
    Result :=Result +';';
end;

constructor ObjConnectionString.Create(const aFileName: string);
var
  I: IMemIniFile ;
begin
    if TFile.Exists(aFileName) then
    begin
        I :=TCMemIniFile.New(aFileName);
        I.Section :='Banco de Dados Local';

        m_provider :=UpperCase(I.ValStr('Provider'));
        m_server  :=I.ValStr('Servidor');
        m_uid :=I.ValStr('Usuario');
        m_pwd:=DesencriptarVar(I.ValStr('Senha'));
        m_database :=I.ValStr('Banco');
        m_trustedConnection:=I.ValBoo('TrustedConnection');
        m_MARS :=I.ValBoo('MARS');
        m_dtCompatibility :=I.ValInt('DataTypeCompatibility',80);
        if m_provider = '' then
        begin
            m_provider :='sqloledb.1';
        end;
    end;
end;

procedure ObjConnectionString.doInitVars;
var
  Q: TADOQuery ;
  S: string;
  E,P: Integer;
begin
    m_Date :=0;
    m_LevelComp :=80;

    //
    //
    Q :=TADOQuery.NewADOQuery(Self.Build) ;
    try
        Q.AddCmd('declare @dt_sys datetime; set @dt_sys =getdate();');
        Q.AddCmd('declare @pro_ver sysname; set @pro_ver=%s;',[Q.FStr('ProductVersion')]);
        Q.AddCmd('select @dt_sys as sys_date                ');
        Q.AddCmd('  ,serverproperty(@pro_ver) as sys_prover ');
        Q.Open;
        m_Date :=Q.Field('sys_date').AsDateTime ;
        S :=Q.Field('sys_prover').AsString;
    finally
        Q.Free ;
    end;

    //
    // ler product (ms sql) version
    P :=Pos('.', S);
    if P > 0 then
    begin
        S :=Copy(S, 1, P-1);
        Val(S, m_LevelComp, E);
    end;
end;


{$REGION 'TMemIniFile'}

function TMemIniFile.ReadBoo(const AIdent, ASection: string): Boolean;
begin
    if ASection <> '' then
    begin
        _section :=ASection ;
    end;
    Result :=Self.ReadBool(_section, AIdent, False);
end;

function TMemIniFile.ReadInt(const AIdent, ASection: string): Integer;
begin
    if ASection <> '' then
    begin
        _section :=ASection ;
    end;
    Result :=Self.ReadInteger(_section, AIdent, 0);
end;

function TMemIniFile.ReadStr(const AIdent, ASection: string): string;
begin
    if ASection <> '' then
    begin
        _section :=ASection ;
    end;
    Result :=Self.ReadString(_section, AIdent, '');
end;

function TMemIniFile.ReadTime(const AIdent, ASection: string): TDateTime;
begin
    if ASection <> '' then
    begin
        _section :=ASection ;
    end;
    Result :=Self.ReadTime(_section, AIdent, 0);
end;

procedure TMemIniFile.WriteValue(const AIdent: string ; const AValue: Variant;
    const ASection: string);
begin
    if ASection <> '' then
    begin
        _section :=ASection ;
    end;
    case VarType(AValue) of
      varSmallint, varWord, varInteger, varInt64: Self.WriteInteger(_section, AIdent, AValue);
      varBoolean: Self.WriteBool(_section, AIdent, AValue);
    else
      Self.WriteString(_section, AIdent, AValue);
    end;
end;

{$ENDREGION}


{ TCEmpresa }

procedure TCEmpresa.DoIniLoad;
var
  C: TMemIniFile ;
begin
    C :=TMemIniFile.Create(IniFileName);
    try
        //geral
        {Emissor._prefeitura :=C.ReadStr('prefeitura', 'Geral');
        Emissor._save       :=C.ReadBoo('salva');
        Emissor._localSave  :=C.ReadStr('localSalva');
        Emissor._localIniProvedor :=C.ReadStr('localIniProvedor');
        Emissor._localSchemas :=C.ReadStr('localSchemas');

        //emitente
        Emissor._token  :=C.ReadStr('Token', 'Emit');
        Emissor._toprps :=C.ReadInt('TopRPS');
        if(Emissor._toprps <= 0)or(Emissor._toprps > 50)then
        begin
            Emissor._toprps :=50;
        end;
        Emissor._codsvc :=C.ReadStr('codSvc');

        //Certificado
        Emissor._localPFX    :=C.ReadStr('local', 'Cert');
        Emissor._senha       :=C.ReadStr('senha');
        Emissor._numeroSerie :=C.ReadStr('numeroSerie');

        //[WebService]
        Emissor.WS._view :=C.ReadBoo('view', 'WS');
        Emissor.WS._pass :=C.ReadStr('pass');
        Emissor.WS._user :=C.ReadStr('user');
        Emissor.WS._saveSOAP :=C.ReadBoo('saveSOAP');
        Emissor.WS._trans :=C.ReadBoo('trans');
        Emissor.WS._ambpro :=C.ReadBoo('tpAmb');
        Emissor.WS._envsinc :=C.ReadBoo('envSinc');
        if MilliSecondOf(C.ReadTime('timeOut')) < Emissor.WS.timeOut then
        begin
            Emissor.WS._timeout :=MilliSecondOf(C.ReadTime('timeOut'));
        end;
        }
    finally
        C.Free ;
    end;
end;

procedure TCEmpresa.DoIniSave;
var
  C: TMemIniFile ;
begin
    C :=TMemIniFile.Create(IniFileName);
    try
        {
        //geral
        C.WriteValue('prefeitura', Emissor.Prefeitura, 'Geral');
        C.WriteValue('salva', Emissor.Salvar);
        C.WriteValue('localSalva', Emissor.LocalSalva);
        C.WriteValue('localIniProvedor', Emissor.LocalIniProvedor);
        C.WriteValue('localSchemas', Emissor.LocalSchemas);

        //emitente
        C.WriteValue('CNPJ', Emissor.CNPJ, 'Emit');
        C.WriteValue('rzSocial', Emissor.rzSocial);
        C.WriteValue('IM', Emissor.InscM);
        C.WriteValue('codMun', Self.CodMun);
        C.WriteValue('Token', Emissor.token);
        C.WriteValue('topRPS', Emissor.TopRPS);
        C.WriteValue('codSvc', Emissor.codigoServico);

        //Certificado
        C.WriteValue('local', Emissor.LocalPFX, 'Cert');
        C.WriteValue('senha', Emissor.Senha);
        C.WriteValue('numeroSerie', Emissor.numeroSerie);

        //[WebService]
        C.WriteValue('view', False, 'WS');
        C.WriteValue('pass', '');
        C.WriteValue('user', '');
        C.WriteValue('saveSOAP', False);
        C.WriteValue('trans', True);
        C.WriteValue('tpAmb', Ord(Emissor.WS.ambPro));
        C.WriteValue('envSinc', Ord(Emissor.WS.envSinc));

        C.UpdateFile ;
        }

{
        //cria locais
        IsRelativePath :=TPath.IsRelativePath(_localCfg.localSalva) ;
        if IsRelativePath and (not TDirectory.Exists(_localCfg.localSalva)) then
        begin
            try TDirectory.CreateDirectory(_localCfg.localSalva) except end;
        end;

        IsRelativePath :=TPath.IsRelativePath(_localCfg.localIniProvedor) ;
        if IsRelativePath and (not TDirectory.Exists(_localCfg.localIniProvedor)) then
        begin
            try TDirectory.CreateDirectory(_localCfg.localIniProvedor) except end;
        end;

        IsRelativePath :=TPath.IsRelativePath(_localCfg.localSchemas) ;
        if IsRelativePath and (not TDirectory.Exists(_localCfg.localSchemas)) then
        begin
            try TDirectory.CreateDirectory(_localCfg.localSchemas) except end;
        end;
}
    finally
        C.Free ;
    end;
end;

procedure TCEmpresa.DoLoad(const codfil: SmallInt);
var
  Q: TADOQuery ;
var
  arq_nome: string ;
  arq_str: TFileStream ;
begin

    Q :=TADOQuery.NewADOQuery(False);
    try
      Q.AddCmd('declare @codfil smallint; set @codfil=%d;',[codfil]);

      Q.AddCmd('select getdate() as dt_syst ,            ');
      Q.AddCmd('  f.codloja as fil_codigo,               ');
      Q.AddCmd('  f.cnpj as fil_cnpj ,                   ');
      Q.AddCmd('  f.nomefantasia as fil_xxfant,          ');
      Q.AddCmd('  f.nomerazao as fil_rzsoci,             ');
      Q.AddCmd('  f.ie as fil_ie ,                       ');
      Q.AddCmd('  f.endereco as fil_lograd,              ');
      Q.AddCmd('  f.numero as fil_numero,                ');
      Q.AddCmd('  f.bairro as fil_bairro,                ');
      Q.AddCmd('  f.codigocidade as fil_codmun,          ');
      Q.AddCmd('  f.cidade as fil_munici,                ');
      Q.AddCmd('  f.estado as fil_uf,                    ');
      Q.AddCmd('  f.cep as fil_cep,                      ');
      Q.AddCmd('  f.tel as fil_fone,f.email as fil_email,');
      Q.AddCmd('  f.logomarca as fil_logo,               ');
      Q.AddCmd('  f.codconta as fil_codcta               ');
      Q.AddCmd('from loja f                              ');
      Q.AddCmd('where f.codloja =@codfil                 ');

      Q.Open;
      Self.m_DateServ :=Q.Field('dt_syst').AsDateTime;
      Self.codfil:=Q.Field('fil_codigo').AsInteger;
      Self.CNPJ  :=TCEmpresa.UnFormatCNPJ(Q.Field('fil_cnpj').AsString) ;
      Self.xFant :=Q.Field('fil_xxfant').AsString;
      Self.RzSoci :=Q.Field('fil_rzsoci').AsString;
      Self.IE  :=TCEmpresa.UnFormatIE(Q.Field('fil_ie').AsString) ;
      Self.xEnder:=Q.Field('fil_lograd').AsString;
      Self.Numero:=Q.Field('fil_numero').AsString;
      Self.Bairro:=Q.Field('fil_bairro').AsString;

      if Q.Field('fil_codmun').IsNull or (Q.Field('fil_codmun').Value='') then
        Self.CodMun:=0
      else
        Self.CodMun:=Q.Field('fil_codmun').AsInteger;

      Self.Munici:=Q.Field('fil_munici').AsString;
      Self.UF    :=Q.Field('fil_uf').AsString;
      Self.CEP   :=Q.Field('fil_cep').AsString;
      Self.Fone   :=Q.Field('fil_fone').AsString;

      Self.CodCta:=Q.Field('fil_codcta').AsInteger;

      arq_nome :=Trim(Q.Field('fil_logo').AsString);

    finally
      Q.Destroy;
    end;

    //cria stream logo marca
    if FileExists(arq_nome) then
    begin
        //arq_str :=TFileStream.Create(arq_nome, fmOpenRead);
        arq_str :=TFileStream.Create(arq_nome, fmShareDenyWrite);
        try
            arq_str.Position :=0;
            Self.Logo :=TMemoryStream.Create ;
            Self.Logo.CopyFrom(arq_str, arq_str.Size) ;
        finally
            arq_str.Free ;
        end;
    end
    else
        Self.Logo :=nil;

end;

class function TCEmpresa.GetInstance: TCEmpresa;
begin
    if FInstance = nil then
    begin
        FInstance :=TCEmpresa.Create ;
//        FInstance.DoIniLoad ;
    end;
    Result :=FInstance;
end;

class function TCEmpresa.UnFormatCNPJ(const Doc: string): string;
begin
    Result :=Trim(Doc);
    Result :=StringReplace(Result, '.', '', [rfReplaceAll]) ;
    Result :=StringReplace(Result, '-', '', [rfReplaceAll]) ;
    Result :=StringReplace(Result, '/', '', [rfReplaceAll]) ;
end;

class function TCEmpresa.UnFormatIE(const Doc: string): string;
begin
    Result :=Trim(Doc) ;
    Result :=StringReplace(Result, '.', '', [rfReplaceAll]) ;
    Result :=StringReplace(Result, '-', '', [rfReplaceAll]) ;
end;

{ TFieldCassHlp }

function TFieldCassHlp.ToString: string;
begin
    Result :=Self.AsString ;
    Result :=Trim(Result);
end;

{ TParameters }

function TParameters.AddParamDatetime(const AName: string;
  const AValue: TDateTime): TParameter;
begin
    Result :=CreateParameter(AName, ftDateTime, pdInput, 0, AValue)
    ;
end;

function TParameters.AddParamFlt(const AName: string; const AValue: Double;
  const ASize, APresicion: Word): TParameter;
begin
    Result :=CreateParameter(AName, ftFloat, pdInput, ASize, Null);
    Result.Precision :=APresicion ;
    Result.Value :=AValue ;
end;

function TParameters.AddParamInt(const AName: string;
  const AValue: Integer): TParameter;
begin
    Result :=CreateParameter(AName, ftInteger, pdInput, 0, AValue)
    ;
end;

function TParameters.AddParamStr(const AName, AValue: string;
  const ASize: Integer): TParameter;
begin
    Result :=CreateParameter(AName, ftString, pdInput, ASize, AValue)
    ;
end;

{ EConnectionADO }

constructor EConnectionADO.Create;
begin
  inherited Create(
    'Objeto "uADODB.ConnectionADO=nil" global de conexão não foi inicializado corretamente!'
    );
end;

{ TADOConnection

class function TADOConnection.GetInstance: TADOConnection;
begin
    Result :=FInstance ;

end;

class procedure TADOConnection.SetInstance(AConnection: TADOConnection);
begin
    FInstance :=AConnection ;

end; }

{ TCustomADODataSet }

function TCustomADODataSet.FStr(const Value: string): string;
begin
    Result :=QuotedStr(Value)
    ;
end;


{ TADOQuery }

function TADOQuery.AddCmd(ACmd: string): Integer;
var
  P: TParameter;
begin
    if Self.Active then
    begin
      Self.Close ;
      Self.SQL.Clear;
    end;
    Result :=Self.SQL.Add(ACmd)
    ;
end;

function TADOQuery.AddCmd(ACmd: string; Args: array of const): Integer;
begin
    Result :=Self.AddCmd(Format(ACmd,Args))
    ;
end;

function TADOQuery.AddParamDatetime(const AName: string;
  const AValue: TDateTime; const AIncTime: Boolean): TParameter;
var
  Y,M,D: Word ;
begin
    Result :=Self.Parameters.CreateParameter(AName, ftDateTime, pdInput, 0, Null) ;
    Result.Value :=Trunc(AValue);
    if AIncTime then
    begin
        DecodeDate(AValue, Y, M, D);
        Result.Value :=EncodeDateTime(Y, M, D, 23, 59, 00, 000) ;
    end;
end;

function TADOQuery.AddParamWithValue(const AName: string;
  const AType: TFieldType;
  const AValue: Variant;
  const ASize: Integer;
  const APrecision: Byte): TParameter;
begin
    Result :=Self.Parameters.CreateParameter(AName, AType, pdInput, 0, Null) ;
    case AType of
      ftString: Result.Size :=ASize;
      ftFloat:
      begin
        Result.Size :=ASize;
        Result.Precision :=APrecision;
      end;
      ftCurrency:
      begin
        Result.Size :=12;
        Result.Precision :=4;
      end;
    end;
    Result.Value :=AValue ;
end;

function TADOQuery.ConnectAndOpen: Boolean;
begin
    Result :=True ;
    if Self.Connection <> nil then
    begin
        try
            Self.Connection.Connected :=True ;
            Self.Open ;
        except
            Result :=False ;
            Self.Connection.Close ;
            raise;
        end;
    end;
end;

procedure TADOQuery.DoClear;
begin
    if Self.Active then
      Self.Close ;
    Self.SQL.Clear ;
end;

class function TADOQuery.Exists(const sql_dml: string): Boolean;
var
  Q: TADOQuery;
begin
    Q :=TADOQuery.NewADOQuery(False);
    try
        Q.AddCmd(sql_dml);
        Q.Open;
        Result :=not Q.IsEmpty ;
    finally
        Q.Free ;
    end;
end;

function TADOQuery.Field(const AFieldName: string): TField;
begin
    Result :=Self.FieldByName(Trim(AFieldName));
end;

function TADOQuery.Field(const AFieldIndex: Integer): TField;
begin
    Result :=Self.Fields[AFieldIndex];
end;

class function TADOQuery.getCompLevel: SmallInt;
var
  Q: TADOQuery;
  S: string;
  E: Integer;
begin
    Result :=0;
    Q :=TADOQuery.NewADOQuery(False);
    try
        Q.AddCmd('declare @pro_ver sysname; set @pro_ver =%s',[Q.FStr('ProductVersion')]);
        Q.AddCmd('select serverproperty(@pro_ver) as pro_ver');
        try
            Q.Open ;
            S :=Q.Field('pro_ver').AsString;
        except Result :=0 end;
    finally
        Q.Free ;
    end;
    //
    // trata retorno
    Result :=Pos('.', S);
    if Result > 0 then
    begin
        S :=Copy(S, 1, Result-1);
        Val(S, Result, E);
    end;
end;

class function TADOQuery.getDateTime: TDateTime;
var
  Q: TADOQuery;
begin

    Q :=TADOQuery.NewADOQuery(False);
    try
        Q.AddCmd('select getdate() as dh_sistem');
        Q.Open;
        if Q.IsEmpty then
            Result :=0
        else
            Result :=Q.Field('dh_sistem').AsDateTime ;
    finally
        Q.Free ;
    end;

end;

class function TADOQuery.getHost(out host_id, host_nm: string): Boolean;
var
  Q: TADOQuery;
begin
    Q :=TADOQuery.NewADOQuery(False);
    try
        Q.AddCmd('select host_id() as host_id, host_name() as host_nm');
        try
            Q.Open ;
            host_id :=Trim(Q.Field('host_id').AsString);
            host_nm :=Trim(Q.Field('host_nm').AsString);
        except Result :=False end;
    finally
        Q.Free ;
    end;
end;

class function TADOQuery.getNewID: string;
var
  Q: TADOQuery;
begin
    Q :=TADOQuery.NewADOQuery(False);
    try
        Q.AddCmd('select newid() as terminal_id');
        try
            Q.Open ;
            Result :=Q.Field('terminal_id').AsString;
        except Result :='' end;
    finally
        Q.Free ;
    end;
end;

class function TADOQuery.getVersion: string;
var
  Q: TADOQuery;
begin
    Q :=TADOQuery.NewADOQuery(False);
    try
        Q.AddCmd('select @@version as sql_ver');
        try
            Q.Open ;
            Result :=Q.Field('sql_ver').AsString;
        except Result :='' end;
    finally
        Q.Free ;
    end;
end;

class function TADOQuery.ident_current(const table_name: string): Integer;
var
  Q: TADOQuery;
begin
    Q :=TADOQuery.NewADOQuery(False);
    try
        Q.AddCmd('select ident_current(%s) as ident',[Q.FStr(table_name)]);
        try
            Q.Open ;
            Result :=Q.Field('ident').AsInteger;
        except Result :=0 end;
    finally
        Q.Free ;
    end;
end;

constructor TADOQuery.NewADOQuery(aConnStr: WideString);
begin
    inherited Create(nil);
    Self.ConnectionString :=aConnStr;
    Self.CursorLocation :=clUseServer;
    Self.LockType :=ltReadOnly ;
    Self.CursorType :=ctOpenForwardOnly;
    Self.ParamCheck :=False;
    Self.BeforeOpen :=OnBeforeOpen;
    Self.AfterOpen :=OnAfterOpen;
//    Self.CommandTimeout :=30;
    Self.m_TimeBeforeOpen :=0;
    Self.m_TimeAfterOpen :=0;
end;

constructor TADOQuery.NewADOQuery(const aParamCheck: Boolean
  {;const AUniDirectional: Boolean});
begin
    if not Assigned(ConnectionADO) then
    begin
        raise EConnectionADO.Create ;
    end;
    inherited Create(nil);
    Self.Connection :=ConnectionADO;
    Self.ParamCheck :=aParamCheck;
    Self.SetUniDirectional(False); //AUniDirectional;
    Self.BeforeOpen :=OnBeforeOpen;
    Self.AfterOpen :=OnAfterOpen;
    Self.m_TimeBeforeOpen :=0;
    Self.m_TimeAfterOpen :=0;
end;

procedure TADOQuery.OnAfterOpen(DataSet: TDataSet);
var
  S: TFileStream ;
  E: Boolean ;
  Buf: AnsiString ;
begin
    m_TimeAfterOpen :=Now ;
    if m_FileName <> '' then
    begin
        E :=FileExists(m_FileName) ;
        S :=TFileStream.Create(m_FileName,
                              IfThen( E,
                                      Integer(fmOpenReadWrite),
                                      Integer(fmCreate)) or fmShareDenyWrite );

        if E then
        begin
            S.Seek(0, soFromEnd);  // vai para EOF
        end
        else begin
            Buf :=SQL.CommaText ;
            S.Write(Pointer(Buf)^,Length(Buf));
            Buf :=sLineBreak;
            S.Write(Pointer(Buf)^,Length(Buf));
        end;
        Buf :=Format('--// %d segundos'#13#10,[SecondsBetween(m_TimeAfterOpen,m_TimeBeforeOpen)]) ;
        S.Write(Pointer(Buf)^,Length(Buf));
        S.Free ;    //AddLog(sLineBreak);
    end;
end;

procedure TADOQuery.OnBeforeOpen(DataSet: TDataSet);
begin
    m_TimeBeforeOpen :=Now ;

end;

function TADOQuery.Param(const AParamName: string): TParameter;
begin
  Result :=Self.Parameters.ParamByName(AParamName)  ;

end;

function TADOQuery.Param(const AParamIndex: Integer): TParameter;
begin
  Result :=Self.Parameters[AParamIndex]  ;

end;

procedure TADOQuery.SaveToFile(const aFileName: string);
begin
    m_FileName :=aFileName  ;
    SQL.SaveToFile(AFileName);
end;

{ TADOStoredProc }

function TADOStoredProc.AddParamDatetime(const AName: string;
  const aValue: TDateTime; const aIncTime: Boolean): TParameter;
var
  Y,M,D: Word ;
begin
    Result :=Self.Parameters.CreateParameter(AName, ftDateTime, pdInput, 0, Null) ;
    Result.Value :=Trunc(AValue);
    if AIncTime then
    begin
        DecodeDate(AValue, Y, M, D);
        Result.Value :=EncodeDateTime(Y, M, D, 23, 59, 00, 000) ;
    end;
end;

function TADOStoredProc.AddParamOut(const AName: string;
  const AType: TFieldType): TParameter;
begin
    Result :=Self.Parameters.CreateParameter(AName, AType, pdOutput, 0, Null) ;
    case AType of
      ftString: Result.Size :=1024;
    end;
    Result.Value :=Null ;
end;

function TADOStoredProc.AddParamRet(const AName: string): TParameter;
begin
    Result :=Self.Parameters.CreateParameter( AName,
                                              ftInteger,
                                              pdReturnValue, 0, 0) ;
end;

function TADOStoredProc.AddParamWithValue(const AName: string;
  const AType: TFieldType; const AValue: Variant): TParameter;
begin
    Result :=Self.Parameters.CreateParameter(AName, AType, pdInput, 0, Null) ;
    case AType of
      ftString: Result.Size :=Length(VarToStr(AValue)) ;
    end;
    Result.Value :=AValue ;
end;

constructor TADOStoredProc.NewADOStoredProc(const aConnStr,
  aStoredProcName: string);
begin
    inherited Create(nil);
    Self.ConnectionString :=WideString(aConnStr) ;
    Self.ParamCheck :=False;
    Self.SetUniDirectional(True);
    Self.ProcedureName :=WideString(aStoredProcName);
    Self.Parameters.CreateParameter('@ret_cod',
                                    ftInteger,
                                    pdReturnValue,
                                    0,
                                    Null) ;
end;

constructor TADOStoredProc.NewADOStoredProc(const AStoredProcName: string;
  const AParamCheck: Boolean);
begin
    if not Assigned(ConnectionADO) then
    begin
        raise EConnectionADO.Create ;
    end;
    inherited Create(nil);
    Self.Connection :=ConnectionADO;
    Self.ParamCheck :=AParamCheck;
    Self.SetUniDirectional(True);
    Self.ProcedureName :=AStoredProcName;
    Self.Parameters.CreateParameter('@ret_cod',
                                    ftInteger,
                                    pdReturnValue,
                                    0,
                                    Null) ;
end;


function TADOStoredProc.Param(const AParamName: string): TParameter;
begin
    Result :=Self.Parameters.ParamByName(AParamName)  ;

end;

function TADOStoredProc.Param(const AParamIndex: Integer): TParameter;
begin
    Result :=Self.Parameters[AParamIndex]  ;
end;

{ TADOCommand }


procedure TADOCommand.AddCmd(ACmd: string);
begin
    Self.CommandText :=Self.CommandText +ACmd +#13#10;
end;

procedure TADOCommand.AddCmd(ACmd: string; Args: array of const);
begin
    AddCmd(Format(ACmd, Args))
    ;
end;

function TADOCommand.AddColumn(const AName: string; const AType: TFieldType;
  const AValue: Variant; const AKey: Boolean): TCColumnDef;
begin
    Result :=TCColumnDef.Create ;
    Result.Name :=Trim(AName) ;
    Result.cTyp :=AType ;
    Result.cVal :=AValue;
    Result.cKey :=AKey;
    Result.Index :=_columns.Count;
    _columns.Add(Result) ;
end;

function TADOCommand.AddColumnBlob(const AName: string;
  const AValue: TStream): TCColumnDef;
begin
    Result :=TCColumnDef.Create ;
    Result.Name :=Trim(AName) ;
    Result.cTyp :=ftBlob ;
    Result.cValBlob := AValue;
    Result.Index :=_columns.Count;
    _columns.Add(Result) ;
end;

function TADOCommand.AddParamWithValue(const AName: string;
  const AType: TFieldType ;
  const AValue: Variant;
  const ADirection: TParameterDirection): TParameter;
begin
    Result :=Self.Parameters.CreateParameter(AName, AType, ADirection, 0, Null) ;
//    case VarType(AValue) of
//      varShortInt, varWord, varSmallint, varInteger, varInt64: Result.DataType :=ftInteger;
//      varString:
//      begin
//        Result.DataType :=ftString ;
//        Result.Size :=Length(VarToStr(AValue)) ;
//      end;
//      varSingle, varDouble, varCurrency:
//      begin
//        Result.DataType :=ftFloat;
//        Result.Size :=12;
//        Result.Precision :=4;
//      end;
//      varDate:
//      begin
//        Result.DataType :=ftDateTime;
//      end;
//    end;
    case AType of
      ftString: Result.Size :=Length(VarToStr(AValue)) ;
    end;
    Result.Value :=AValue ;
end;

procedure TADOCommand.ExecInsert(const ATableName: string;
  const ABeginTran: Boolean);
var
  C: TCColumnDef ;
  P: TParameter ;
  T: Integer ;
begin
    Self.AddCmd('insert into %s (', [ATableName]);

    //list-fields
    for C in _columns do
    begin
        if C.Index < _columns.Count -1 then
          Self.AddCmd('   %s,', [C.Name])
        else
          Self.AddCmd('   %s)', [C.Name]);
    end;
    Self.AddCmd('values (', [ATableName]);

    //list-params-manual
    for C in _columns do
    begin
        if C.Index < _columns.Count -1 then
            Self.AddCmd('   ?,')
        else
            Self.AddCmd('   ?)');
    end;

    //list-values
    for C in _columns do
    begin
        P :=Self.AddParamWithValue('@'+C.Name, C.cTyp, C.cVal) ;
        if C.cTyp = ftBlob then
        begin
            P.LoadFromStream(C.cValBlob, C.cTyp);
        end;
    end;

    T :=0;
    if ABeginTran then
    begin
        if not Self.Connection.InTransaction then
          T :=Self.Connection.BeginTrans
        else
          T :=1;
    end;

    try
        Self.SaveToFile();
        Self.Execute ;
        if T > 1 then Self.Connection.CommitTrans;
//        Result :=True ;
    except
        if T > 1 then Self.Connection.RollbackTrans;
        raise;
    end;
end;

procedure TADOCommand.ExecUpdate(const ATableName: string;
  const ABeginTran: Boolean);
var
  C: TCColumnDef ;
  P: TParameter ;
  T: Integer ;
begin
    Self.CommandText :='';
    Self.AddCmd('update %s  ', [ATableName]);
    Self.AddCmd('set        ');

    //columns-set, no key
    for C in _columns do
    begin
        if not C.cKey then
        begin
            if C.Index < _columns.Count -1 then
                Self.AddCmd('   %s  =?,', [C.Name])
            else
                Self.AddCmd('   %s  =?', [C.Name]);
        end;
    end;

    //columns-where, key
    Self.AddCmd('where      ');
    for C in _columns do
    begin
        if C.cKey then
        begin
            if C.Index  = 0 then
                Self.AddCmd('%s =?', [C.Name])
            else
                Self.AddCmd('and %s =?', [C.Name]);
        end;
    end;

    //params-values,no key
    for C in _columns do
    begin
        if not C.cKey then
        begin
            P :=Self.AddParamWithValue(C.Name, C.cTyp, C.cVal) ;
            if C.cTyp = ftBlob then
            begin
                P.LoadFromStream(C.cValBlob, C.cTyp);
            end;
        end;
    end;

    //params-values, key
    for C in _columns do
    begin
        if C.cKey then
        begin
            P :=Self.AddParamWithValue(C.Name, C.cTyp, C.cVal) ;
        end;
    end;

    T :=0;
    if ABeginTran then
    begin
        if not Self.Connection.InTransaction then
          T :=Self.Connection.BeginTrans
        else
          T :=1;
    end;

    try
//        Self.SaveToFile();
        Self.Execute ;
        if T > 1 then Self.Connection.CommitTrans;
//        Result :=True ;
    except
        if T > 1 then Self.Connection.RollbackTrans;
        raise;
    end;
end;

function TADOCommand.FStr(const Value: string): string;
begin
    Result :=QuotedStr(Value)
    ;
end;

constructor TADOCommand.NewADOCommand(const aConnStr: WideString);
begin
    inherited Create(nil);
    Self.ConnectionString :=aConnStr;
    Self.ParamCheck :=False;
    _columns:=TList<TCColumnDef>.Create;
end;

constructor TADOCommand.NewADOCommand(const AParamCheck: Boolean);
begin
    if not Assigned(ConnectionADO) then
    begin
        raise EConnectionADO.Create ;
    end;
    inherited Create(nil);
    Self.Connection :=ConnectionADO;
    Self.ParamCheck :=AParamCheck;
    _columns:=TList<TCColumnDef>.Create;
end;

function TADOCommand.Param(const AParamName: string): TParameter;
begin
    Result :=Self.Parameters.ParamByName(AParamName)  ;

end;

function TADOCommand.Param(const AParamIndex: Integer): TParameter;
begin
    Result :=Self.Parameters[AParamIndex]  ;

end;

procedure TADOCommand.SaveToFile(const AFileName: string);
var
  S: TStreamWriter;
begin
    S :=TStreamWriter.Create(AFileName);
    try
        S.Write(Self.CommandText);
    finally
        S.Free;
    end;
end;


{$IFDEF ADOCONN_EXTEND}
{ TADOConnection }

class function TADOConnection.NewADOConnection(
  const AProviderTyp: TADOConnProviderTyp;
  const AServer, AUsername, APassword, ADataSource: string): TADOConnection;
var
  conStr: string ;
begin

    case AProviderTyp of
        proMSSQL: conStr :='';
        proOracle: conStr :='';
        proPgSQL:
        begin
          conStr :='Provider=MSDASQL.1;Persist Security Info=True;';
          conStr :=conStr +Format('User ID=%s;',[AUsername] );
          conStr :=conStr +Format('Password=%s;',[APassword] );
          conStr :=conStr +Format('Data Source=Devart ODBC PgSQL;
          conStr :=conStr +Format('Initial Catalog=nw10neway';
        end;
    end;
end;
{$ENDIF}


{ TFrmtStr }

class function TFrmtStr.Cur(const AValue: Currency): string;
begin
    Result :=FormatFloat('#,##0.00', AValue, LocalFormatSettings) ;

end;

class function TFrmtStr.Dat(const AValue: TDateTime;
  const AFormat: TFrmtStrDateTime): string;
var
  Y,M,D,H,N,S,MS: Word ;
begin
    Result :='';
    DecodeDateTime(AValue,Y,M,D,H,N,S,MS);
    if Y > 1900 then
    begin
        case AFormat of
          sdDMY_HNS: Result :='dd/mm/yyyy hh:nn:ss';
          sdDMY_HN: Result :='dd/mm/yyyy hh:nn';
          sdDMY_H: Result :='dd/mm/yyyy hh';
          sdDMY: Result :='dd/mm/yyyy';
          sdHNS: Result :='hh:nn:ss';
          sdHN: Result :='hh:nn';
        end;
        Result :=FormatDateTime(Result,
                                AValue,
                                LocalFormatSettings) ;
    end;
end;


class function TFrmtStr.Flt(const AValue: Extended; const ADec: Word): string;
begin
    Result :='';
end;

class function TFrmtStr.Int(const AValue: Int32): string;
begin
    Result :=FormatFloat('#,##0', AValue) ;

end;

class function TFrmtStr.SQLStr(const AStr: string;
  const ATrunc: Integer): string;
begin
    if ATrunc > 0 then
        Result :=QuotedStr(Copy(AStr,1,ATrunc))
    else
        Result :=QuotedStr(AStr);
end;

{ TCEmissor }

constructor TCEmissor.Create(AOwner: TCEmpresa);
begin
    _owner :=AOwner ;
    _save :=True ;
    _localSave        :=ExtractFilePath(_owner.IniFileName) + 'NFSe' ;
    _localIniProvedor :=_localSave ;
    _localSchemas     :=_localSave  + 'NFSe\schemas' ;
    _toprps  :=50 ;
    WS :=TCWS.Create(Self) ;
end;

{ TCWS }

constructor TCWS.Create(AOwner: TCEmissor);
begin
    _owner :=AOwner ;
    _timeout :=180*1000; //3min

end;

{ TCColumnDef }

constructor TCColumnDef.Create;
begin
    Self.Index :=0;
    Self.cTyp :=ftUnknown;
    Self.cVal :=null;
    Self.cValBlob :=TMemoryStream.Create;
    Self.cKey :=False;
end;

destructor TCColumnDef.Destroy;
begin
    cValBlob.Free ;
    inherited;
end;


{ CFrmtStr }

class function CFrmtStr.Cur(const AValue: Currency): string;
begin
    Result :=CFrmtStr.Flt(AValue) ;

end;

class function CFrmtStr.Dat(const AValue: TDateTime; const AIncTime: Boolean): string;
begin
    if AIncTime then
        DateTimeToString(Result, '', AValue, LocalFormatSettings)
    else
        DateTimeToString(Result,
                        LocalFormatSettings.ShortDateFormat,
                        AValue, LocalFormatSettings);
end;

class function CFrmtStr.Flt(const AValue: Extended; const ADec: Word): string;
begin
    if ADec = 0 then
        Result :='#,##0.00'
    else
        Result :='#,##0.'+ DupeString('0', ADec);
    Result  :=FormatFloat(Result, AValue) ;
end;

class function CFrmtStr.Int(const AValue: Int64): string;
begin
    Result :=FormatFloat('#,##0', AValue) ;

end;


{ TCGenSerial }

constructor TCGenSerial.Create(const aIdent, aDescri, aCatego: string ;
  const aIniVal: Cardinal; const aIncVal, aMinVal: Int32;
  const aMaxVal: Cardinal);
begin
    inherited Create;
    Self.m_ident :=aIdent ;
    Self.m_inival:=aIniVal;
    Self.m_incval:=aIncVal;
    Self.m_minval:=aMinVal;
    Self.m_maxval:=aMaxVal;
    Self.m_descri:=aDescri;
    Self.m_catego:=aCatego;
end;

function TCGenSerial.getCatego: string;
begin
    Result :=m_catego ;

end;

function TCGenSerial.getDescri: string;
begin
    Result :=m_descri ;

end;

function TCGenSerial.getIdent: string;
begin
    Result :=m_ident ;

end;

function TCGenSerial.getIncVal: Int32;
begin
    Result :=m_incval ;

end;

function TCGenSerial.getIniVal: Cardinal;
begin
    Result :=m_inival ;

end;

function TCGenSerial.getMaxVal: Cardinal;
begin
    Result :=m_maxval ;

end;

function TCGenSerial.getMinVal: Int32;
begin
    Result :=m_minval ;

end;

function TCGenSerial.getValue: Cardinal;
begin
    Result :=m_value ;

end;

class function TCGenSerial.New(const aIdent, aDescri, aCatego: string ;
  const aIniVal: Cardinal; const aIncVal, aMinVal: Int32;
  const aMaxVal: Cardinal): IGenSerial;
begin
    Result :=TCGenSerial.Create(aIdent, aDescri, aCatego,
                                aIniVal, aIncVal,
                                aMinVal, aMaxVal);
end;

function TCGenSerial.nextValue: Cardinal;
var
  sp: TADOStoredProc ;
begin
    sp :=TADOStoredProc.NewADOStoredProc('dbo.sp_nextval');
    try
        //
        sp.AddParamWithValue('@ser_ident', ftString, Self.m_ident);
        sp.AddParamOut('@ser_outval', ftInteger);
        sp.AddParamWithValue('@read_only', ftSmallint, 0);

        try
            sp.ExecProc ;
            Result :=sp.Param('@ser_outval').Value ;
        except
            Result :=0;
        end;

    finally
        sp.Free ;
    end;
end;

function TCGenSerial.readValue: Boolean;
var
  Q: TADOQuery ;
begin
    //
    Q :=TADOQuery.NewADOQuery(False);
    try
      Q.AddCmd('declare @ser_id varchar(50); set @ser_id =%s;   ',[Q.FStr(Self.m_ident)]) ;
      Q.AddCmd('select *from genserial where ser_ident =@ser_id ');
      Q.Open ;
      Result :=not Q.IsEmpty ;
      if Result then
      begin
          m_ident :=Q.Field('ser_ident').AsString ;
          m_value :=Q.Field('ser_valor').AsLargeInt;
          m_inival :=Q.Field('ser_inival').AsInteger;
          m_incval :=Q.Field('ser_incval').AsInteger;
          m_minval :=Q.Field('ser_minval').AsInteger;
          m_maxval :=Q.Field('ser_maxval').AsLargeInt;
          m_descri :=Q.Field('ser_descri').AsString ;
          m_catego :=Q.Field('ser_catego').AsString ;
      end;
    finally
      Q.Free ;
    end;
end;

procedure TCGenSerial.setValue;
var
  sp: TADOStoredProc ;
begin
    sp :=TADOStoredProc.NewADOStoredProc('dbo.sp_setval');
    try
        //
        // chk se existe categoria
        // coloca como prefixo no ID para compatibilizar com a sp
        if Self.m_catego <> '' then
            sp.AddParamWithValue('@ser_ident', ftString, Format('[%s]%s',[Self.m_catego,Self.m_ident]))
        else
            sp.AddParamWithValue('@ser_ident', ftString, Self.m_ident);
        sp.AddParamWithValue('@ser_inival', ftInteger, Self.m_inival);
        sp.AddParamWithValue('@ser_incval', ftInteger, Self.m_incval);
        sp.AddParamWithValue('@ser_minval', ftInteger, Self.m_minval);
        sp.AddParamWithValue('@ser_maxval', ftInteger, Self.m_maxval);
        if Length(Self.m_descri) > 0 then
            sp.AddParamWithValue('@ser_descri', ftString, Self.m_descri)
        else
            sp.AddParamWithValue('@ser_descri', ftString, ' ');
        sp.ExecProc ;
    finally
        sp.Free ;
    end;
end;


initialization
    ConnectionADO :=nil  ;
    GetLocaleFormatSettings(0, LocalFormatSettings);
    StrU.Create(LocalFormatSettings);

finalization
    ConnectionADO :=nil  ;


end.
