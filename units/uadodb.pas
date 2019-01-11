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
  {TADOConnProviderTyp = (proMSSQL, proOracle, proPgSQL) ;
  TADOConnection = class(ADODB.TADOConnection)
  private
    class var _instance: TADOConnection;
  protected
    procedure DoConnect; override;
  public
    class function getInstance(): TADOConnection; static ;
    class function NewADOConnFromIniFile(const AFileName: string): TADOConnection ;
    //    class function NewADOConnection(const AProviderTyp: TADOConnProviderTyp;
//      const AServer, AUsername, APassword: string;
//      const ADefaultDB: string = '';
//      const ADataSource: string): TADOConnection ;
  end;}


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

  TADOQuery = class(ADODB.TADOQuery)
  public
    constructor NewADOQuery(const AParamCheck: Boolean = False
      {;const AUniDirectional: Boolean = False} );
    function AddCmd(ACmd: string): Integer ; overload ;
    function AddCmd(ACmd: string; Args: array of const): Integer ; overload ;
    function AddParamWithValue(const AName: string; const AType: TFieldType;
      const AValue: Variant;
      const ASize: Integer = 0;
      const APrecision: Byte = 0): TParameter;
    function AddParamDatetime(const AName: string; const AValue: TDateTime;
      const AIncTime: Boolean = False): TParameter;
  public
    function Field(const AFieldName: string): TField; overload;
    function Field(const AFieldIndex: Integer): TField; overload;
    function Param(const AParamName: string): TParameter; overload;
    function Param(const AParamIndex: Integer): TParameter; overload;
  public
    procedure SaveToFile(const AFileName: string = '0.SQL');
    procedure DoClear ;
  public
    class function getDateTime: TDateTime ;
    class function getHost(out host_id, host_nm: string): Boolean ;
    class function getNewID(): string ;
    class function getVersion(): string ;
    class function ident_current(const table_name: string): Integer ;
    class function Exists(const sql_dml: string): Boolean ;
  end;

  TADOStoredProc = class(ADODB.TADOStoredProc)
  public
    constructor NewADOStoredProc(const AStoredProcName: string;
      const AParamCheck: Boolean = False);
    function AddParamWithValue(const AName: string;
      const AType: TFieldType ;
      const AValue: Variant): TParameter;
    function AddParamOut(const AName: string;
      const AType: TFieldType): TParameter;
    function Param(const AParamName: string): TParameter; overload;
    function Param(const AParamIndex: Integer): TParameter; overload;
  end;

//  TCommandDML = (cmdInsert, cmdUpdade) ;
  TADOCommand = class(ADODB.TADOCommand)
  private
    _columns: TList<TCColumnDef> ;
  public
    constructor NewADOCommand(const AParamCheck: Boolean = False);
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


//type
//  TCCustomADODataSet = class(ADODB.TCustomADODataSet)
//  public
//  end;
//
//  TCQuery = class(TCCustomADODataSet)
//  private
//    FSQL: TStrings;
//    FRowsAffected: Integer;
//    function GetSQL: TStrings;
//    procedure SetSQL(const Value: TStrings);
//  protected
//    procedure QueryChanged(Sender: TObject);
//  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
//    function ExecSQL: Integer; {for TQuery compatibility}
//    property RowsAffected: Integer read FRowsAffected;
//  published
//    property CommandTimeout;
//    property DataSource;
//    property EnableBCD;
//    property ParamCheck;
//    property Parameters;
//    property Prepared;
//    property SQL: TStrings read GetSQL write SetSQL;
//  end;



var
  ConnectionADO: ADODB.TADOConnection;
  LocalFormatSettings: TFormatSettings;
  Empresa: TCEmpresa ;

function NewADOConnFromIniFile(const AFileName: string): ADODB.TADOConnection ;


function DesencriptarVar(Texto: String):String;


implementation

uses ActiveX, Math, StrUtils, DateUtils, IOUtils, Variants;

function DesencriptarVar(Texto: String):String;
var w : string;
i : integer;
begin
FOR i := 1 TO Length(texto) do
w := w + chr( Ord(texto[i]) - i - 19 );
result:= w;
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


{ TADOConnection

procedure TADOConnection.DoConnect;
begin
    inherited;
    //ConnectionADO :=Self;

end;

class function TADOConnection.getInstance: TADOConnection;
begin
    if _instance = nil then
    begin
        _instance :=NewADOConnFromIniFile('Configuracoes.ini') ;
    end;
    Result :=_instance ;
end;

class function TADOConnection.NewADOConnFromIniFile(
  const AFileName: string): TADOConnection;
const
  SECTION = 'Banco de Dados Local';
var
  M: TMemIniFile ;
begin
    Result :=nil;
    if TFile.Exists(AFileName) then
    begin
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
end;}


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

    Q :=TADOQuery.NewADOQuery();
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
      Self.SQL.Clear ;
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
    Q :=TADOQuery.NewADOQuery();
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

class function TADOQuery.getDateTime: TDateTime;
var
  Q: TADOQuery;
begin

    Q :=TADOQuery.NewADOQuery();
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
    Q :=TADOQuery.NewADOQuery();
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
    Q :=TADOQuery.NewADOQuery();
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
    Q :=TADOQuery.NewADOQuery();
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
    Q :=TADOQuery.NewADOQuery();
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

constructor TADOQuery.NewADOQuery(const AParamCheck: Boolean
  {;const AUniDirectional: Boolean});
begin
    if not Assigned(ConnectionADO) then
    begin
        raise EConnectionADO.Create ;
    end;
    inherited Create(nil);
    Self.Connection :=ConnectionADO;
    Self.ParamCheck :=AParamCheck;
    Self.SetUniDirectional(False); //AUniDirectional;
end;

function TADOQuery.Param(const AParamName: string): TParameter;
begin
  Result :=Self.Parameters.ParamByName(AParamName)  ;

end;

function TADOQuery.Param(const AParamIndex: Integer): TParameter;
begin
  Result :=Self.Parameters[AParamIndex]  ;

end;

procedure TADOQuery.SaveToFile(const AFileName: string);
begin
  Self.SQL.SaveToFile(AFileName)
  ;
end;

{ TADOStoredProc }

function TADOStoredProc.AddParamOut(const AName: string;
  const AType: TFieldType): TParameter;
begin
    Result :=Self.Parameters.CreateParameter(AName, AType, pdOutput, 0, Null) ;
    case AType of
      ftString: Result.Size :=1024;
    end;
    Result.Value :=Null ;
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


initialization
    ConnectionADO :=nil  ;
    GetLocaleFormatSettings(0, LocalFormatSettings);

finalization
    ConnectionADO :=nil  ;


end.
