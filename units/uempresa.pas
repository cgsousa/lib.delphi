unit uempresa;

interface

uses
  Classes,
  Generics.Collections
  ;

type
  TCEmissor = class ;
  TCEmpresa = class
  private
    class var Instance: TCEmpresa;
  private
    m_Emissor: TCEmissor ;
  public
    class function getInstance: TCEmpresa; static;
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
    Logo: TStream;
  public
    constructor Create ;
    procedure DoLoad(const codfil: SmallInt);
  end;


implementation

uses uadodb ;

{ TCEmpresa }

constructor TCEmpresa.Create;
begin

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
      Q.AddCmd('declare @codfil smallint; set @codfil = %d;',[codfil]);

      Q.AddCmd('select                                   ');
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

      Q.Open;

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

    //inicializa serie /cnpj
    {if Self.AtivaNumeracao and (Self.CNPJ <> '') then
    begin
        //serie unica
        TCGenSerial.getInstance.SetVal(Format('nfe.%s.nserie.000',[Self.CNPJ]),
                                    1, 1, 1, 999999999,
                                    'Contador de doc. fiscal(NFE) por serie unica no cnpj');
        //serie normal
//        TCGenSerial.getInstance.SetVal(Format('nfe.%s.nserie',[Self.CNPJ]),
//                                    1, 1, 1, 799,
//                                    'Contador de nserie por cnpj');

        //serial para cod.ntf
//        TCGenSerial.getInstance.SetVal(Format('nfe.codntf'),
//                                    1, 1, 1, 0,
//                                    'Identificador unico para cod. sequencial da nota fiscal');
    end;}
end;

class function TCEmpresa.getInstance: TCEmpresa;
begin
    if Instance = nil then
    begin
        Instance :=TCEmpresa.Create ;
        Instance.DoLoad(1);
    end;
    Result :=Instance;
end;


end.
