unit ucademp;

interface

uses SysUtils, Classes,
  Generics.Collections ;

type
  TCCadEmp = class;
  TCEndereco = class;

  IEndereco =Interface(IInterface)
    function getxLogr: String;
    procedure setxLogr(const aValue: String);
    property xLogr: String read getxLogr write setxLogr;

    function getNumero: String;
    procedure setNumero(const aValue: String);
    property numero: String read getNumero write setNumero;

    function getxCompl: String;
    procedure setxCompl(const aValue: String);
    property xCompl: String read getxCompl write setxCompl;

    function getxBairro: String;
    procedure setxBairro(const aValue: String);
    property xBairro: String read getxBairro write setxBairro;

    function getcMun: Integer;
    procedure setcMun(const aValue: Integer);
    property cMun: Integer read getcMun write setcMun;

    function getxMun: String;
    procedure setxMun(const aValue: String);
    property xMun: String read getxMun write setxMun;

    function getUF: String;
    procedure setUF(const aValue: String);
    property UF: String read getUF write setUF;

    function getCEP: Integer;
    procedure setCEP(const aValue: Integer);
    property CEP: Integer read getCEP write setCEP;

    function getcPais: Integer;
    property cPais: Integer read getcPais;

    function getxPais: String;
    property xPais: String read getxPais;
  end;

  TCodigoRegimeTrib = (crtSimplesNacional, crtSimplesExReceita, crtRegimeNormal);

  ICadEmp = Interface(IInterface)
    function getCNPJ: String;
    procedure setCNPJ(const aValue: String);
    property CNPJ: String read getCNPJ write setCNPJ;

    function getxNome: String;
    procedure setxNome(const aValue: String);
    property xNome: String read getxNome write setxNome;

    function getxFant: String;
    procedure setxFant(const aValue: String);
    property xFant: String read getxFant write setxFant;

    function getEnder: TCEndereco ;
    property ender: TCEndereco read getEnder ;

    function getIE: String;
    procedure setIE(const aValue: String);
    property IE: String read getIE write setIE;

    function getIEST: String;
    procedure setIEST(const aValue: String);
    property IEST: String read getIEST write setIEST;

    function getIM: String;
    procedure setIM(const aValue: String);
    property IM: String read getIM write setIM;

    function getCNAE: String;
    procedure setCNAE(const aValue: String);
    property CNAE: String read getCNAE write setCNAE;

    function getCRT: TCodigoRegimeTrib;
    procedure setCRT(const aValue: TCodigoRegimeTrib);
    property CRT: TCodigoRegimeTrib read getCRT write setCRT;

    function getFone: String;
    procedure setFone(const aValue: String);
    property fone: String read getFone write setFone;

    function getEmail: String;
    procedure setEmail(const aValue: String);
    property email: String read getEmail write setEmail;

    procedure Load(const codfil: SmallInt);
  end;

  TCEndereco = class(TAggregatedObject, IEndereco)
  private
    m_xLogr: String;
    m_Numero: String;
    m_xCompl: String;
    m_xBairro: String;
    m_cMun: Integer;
    m_xMun: String;
    m_UF: String;
    m_CEP: Integer;
    m_cPais: Integer;
    m_xPais: String;

    function getxLogr: String;
    procedure setxLogr(const aValue: String);

    function getNumero: String;
    procedure setNumero(const aValue: String);

    function getxCompl: String;
    procedure setxCompl(const aValue: String);

    function getxBairro: String;
    procedure setxBairro(const aValue: String);

    function getcMun: Integer;
    procedure setcMun(const aValue: Integer);

    function getxMun: String;
    procedure setxMun(const aValue: String);

    function getUF: String;
    procedure setUF(const aValue: String);

    function getCEP: Integer;
    procedure setCEP(const aValue: Integer);

    function getcPais: Integer;
    function getxPais: String;
  public
    constructor Create(aController: IInterface);
    property xLogr: String read getxLogr write setxLogr;
    property numero: String read getNumero write setNumero;
    property xCompl: String read getxCompl write setxCompl;
    property xBairro: String read getxBairro write setxBairro;
    property cMun: Integer read getcMun write setcMun;
    property xMun: String read getxMun write setxMun;
    property UF: String read getUF write setUF;
    property CEP: Integer read getCEP write setCEP;
    property cPais: Integer read getcPais;
    property xPais: String read getxPais;
  end;

  TCCadEmp = class(TInterfacedObject, ICadEmp, IEndereco)
  private
    m_CodFil: SmallInt ;
    m_CNPJ: String;
    m_xNome: String;
    m_xFant: String;
    m_ender: TCEndereco;
    m_IE: String;
    m_IEST: String;
    m_IM: String;
    m_CNAE: String;
    m_CRT: TCodigoRegimeTrib;
    m_fone: String;
    m_email: String;

    function getCNPJ: String;
    procedure setCNPJ(const aValue: String);

    function getxNome: String;
    procedure setxNome(const aValue: String);

    function getxFant: String;
    procedure setxFant(const aValue: String);

    function getEnder: TCEndereco ;

    function getIE: String;
    procedure setIE(const aValue: String);

    function getIEST: String;
    procedure setIEST(const aValue: String);

    function getIM: String;
    procedure setIM(const aValue: String);

    function getCNAE: String;
    procedure setCNAE(const aValue: String);

    function getCRT: TCodigoRegimeTrib;
    procedure setCRT(const aValue: TCodigoRegimeTrib);

    function getFone: String;
    procedure setFone(const aValue: String);

    function getEmail: String;
    procedure setEmail(const aValue: String);

  public
    constructor Create();
    destructor Destroy; override;
    procedure Load(const codfil: SmallInt);
    property CNPJ: String read getCNPJ write setCNPJ;
    property xNome: String read getxNome write setxNome;
    property xFant: String read getxFant write setxFant;
    property ender: TCEndereco read getEnder implements IEndereco;
    property IE: String read getIE write setIE;
    property IEST: String read getIEST write setIEST;
    property IM: String read getIM write setIM;
    property CNAE: String read getCNAE write setCNAE;
    property CRT: TCodigoRegimeTrib read getCRT write setCRT;
    property fone: String read getFone write setFone;
    property email: String read getEmail write setEmail;
  public
    class function New(const aCodFil: Smallint): ICadEmp ;
  end;

var
  CadEmp: ICadEmp ;

implementation

uses uadodb, ustr ;

{ TCEndereco }

constructor TCEndereco.Create(aController: IInterface);
begin
    inherited Create(aController);
    m_cPais :=1058 ;
    m_xPais :='BRASIL';
end;

function TCEndereco.getCEP: Integer;
begin
    Result :=m_CEP ;
end;

function TCEndereco.getcMun: Integer;
begin
    Result :=m_cMun ;
end;

function TCEndereco.getcPais: Integer;
begin
    Result :=m_cPais ;
end;

function TCEndereco.getNumero: String;
begin
    Result :=m_Numero ;
end;

function TCEndereco.getUF: String;
begin
    Result :=m_UF ;
end;

function TCEndereco.getxBairro: String;
begin
    Result :=m_xBairro ;
end;

function TCEndereco.getxCompl: String;
begin
    Result :=m_xCompl ;
end;

function TCEndereco.getxLogr: String;
begin
    Result :=m_xLogr ;
end;

function TCEndereco.getxMun: String;
begin
    Result :=m_xMun ;
end;

function TCEndereco.getxPais: String;
begin
    Result :=m_xPais ;
end;

procedure TCEndereco.setCEP(const aValue: Integer);
begin
    m_CEP :=aValue ;
end;

procedure TCEndereco.setcMun(const aValue: Integer);
begin
    m_cMun :=aValue ;
end;

procedure TCEndereco.setNumero(const aValue: String);
begin
    m_Numero :=aValue ;
end;

procedure TCEndereco.setUF(const aValue: String);
begin
    m_UF :=aValue ;
end;

procedure TCEndereco.setxBairro(const aValue: String);
begin
    m_xBairro :=aValue ;
end;

procedure TCEndereco.setxCompl(const aValue: String);
begin
    m_xCompl :=aValue ;
end;

procedure TCEndereco.setxLogr(const aValue: String);
begin
    m_xLogr :=aValue ;
end;

procedure TCEndereco.setxMun(const aValue: String);
begin
    m_xMun :=aValue ;
end;


{ TCCadEmp }

constructor TCCadEmp.Create;
begin
    inherited Create;
    m_ender :=TCEndereco.Create(Self);
end;

destructor TCCadEmp.Destroy;
begin
    m_ender.Free ;
    inherited Destroy;
end;

function TCCadEmp.getCNAE: String;
begin
    Result :=m_CNAE ;
end;

function TCCadEmp.getCNPJ: String;
begin
    Result :=m_CNPJ ;
end;

function TCCadEmp.getCRT: TCodigoRegimeTrib;
begin
    Result :=m_CRT ;
end;

function TCCadEmp.getEmail: String;
begin
    Result :=m_email ;
end;

function TCCadEmp.getEnder: TCEndereco;
begin
    Result :=m_ender ;
end;

function TCCadEmp.getFone: String;
begin
    Result :=m_fone ;
end;

function TCCadEmp.getIE: String;
begin
    Result :=m_IE ;
end;

function TCCadEmp.getIEST: String;
begin
    Result :=m_IEST ;
end;

function TCCadEmp.getIM: String;
begin
    Result :=m_IM ;
end;

function TCCadEmp.getxFant: String;
begin
    Result :=m_xFant ;
end;

function TCCadEmp.getxNome: String;
begin
    Result :=m_xNome ;
end;

procedure TCCadEmp.Load(const codfil: SmallInt);
var
  Q: TADOQuery;
  U: UtilStr ;
var
  crt: string;
begin
    //
    m_CodFil :=codfil ;
    //
    Q :=TADOQuery.NewADOQuery();
    try
      Q.AddCmd('declare @codfil smallint; set @codfil=%d;',[codfil]);
      Q.AddCmd('select                                   ');
      Q.AddCmd('  f.codloja as fil_codigo,               ');
      Q.AddCmd('  f.cnpj as fil_cnpj ,                   ');
      Q.AddCmd('  f.nomefantasia as fil_xfant,           ');
      Q.AddCmd('  f.nomerazao as fil_xnome,              ');
      Q.AddCmd('  f.ie as fil_ie ,                       ');
      Q.AddCmd('  f.endereco as fil_xlogr,               ');
      Q.AddCmd('  f.numero as fil_numero,                ');
      Q.AddCmd('  f.bairro as fil_bairro,                ');
      Q.AddCmd('  f.codigocidade as fil_codmun,          ');
      Q.AddCmd('  f.cidade as fil_munici,                ');
      Q.AddCmd('  f.estado as fil_uf,                    ');
      Q.AddCmd('  f.cep as fil_cep,                      ');
      Q.AddCmd('  f.tel as fil_fone,f.email as fil_email,');
      Q.AddCmd('  f.tipoempresa as fil_crt               ');
      Q.AddCmd('from loja f                              ');
      Q.AddCmd('where f.codloja =@codfil                 ');

      Q.Open;

      m_CNPJ  :=U.getNumber(Q.Field('fil_cnpj').AsString) ;
      m_xNome :=Q.Field('fil_xnome').AsString;
      m_xFant :=Q.Field('fil_xfant').AsString;

      m_ender.xLogr   :=Q.Field('fil_xlogr').AsString;
      m_ender.numero  :=Q.Field('fil_numero').AsString;
      m_ender.xBairro :=Q.Field('fil_bairro').AsString;
      if not Q.Field('fil_codmun').IsNull then
          m_ender.cMun:=Q.Field('fil_codmun').AsInteger;
      m_ender.xMun    :=Q.Field('fil_munici').AsString;
      m_ender.UF      :=Q.Field('fil_uf').AsString;
      m_ender.CEP :=StrToIntDef(U.getNumber(Q.Field('fil_cep').AsString), 0);

      m_IE  :=U.getNumber(Q.Field('fil_ie').AsString) ;

      m_fone  :=U.getNumber(Q.Field('fil_fone').AsString);
      m_email :=Q.Field('fil_email').AsString;
      //
      // CRT
      m_CRT :=crtRegimeNormal ;
      if not Q.Field('fil_crt').IsNull then
      begin
          crt :=LowerCase(Q.Field('fil_crt').AsString) ;
          if crt ='simples' then m_CRT :=crtSimplesNacional
          else if crt <>'normal' then m_CRT :=crtSimplesExReceita ;
      end;

    finally
      Q.Destroy;
    end;
end;

class function TCCadEmp.New(const aCodFil: Smallint): ICadEmp;
begin
    Result :=TCCadEmp.Create ;
    Result.Load(aCodFil);

end;

procedure TCCadEmp.setCNAE(const aValue: String);
begin
    m_CNAE :=aValue ;
end;

procedure TCCadEmp.setCNPJ(const aValue: String);
begin
    m_CNPJ :=aValue ;
end;

procedure TCCadEmp.setCRT(const aValue: TCodigoRegimeTrib);
begin
    m_CRT :=aValue ;
end;

procedure TCCadEmp.setEmail(const aValue: String);
begin
    m_email :=aValue ;
end;

procedure TCCadEmp.setFone(const aValue: String);
begin
    m_fone :=aValue ;
end;

procedure TCCadEmp.setIE(const aValue: String);
begin
    m_IE :=aValue ;
end;

procedure TCCadEmp.setIEST(const aValue: String);
begin
    m_IEST :=aValue ;
end;

procedure TCCadEmp.setIM(const aValue: String);
begin
    m_IM :=aValue ;
end;

procedure TCCadEmp.setxFant(const aValue: String);
begin
    m_xFant :=aValue ;
end;

procedure TCCadEmp.setxNome(const aValue: String);
begin
    m_xNome :=aValue ;
end;


end.
