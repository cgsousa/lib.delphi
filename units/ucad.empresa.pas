unit ucad.empresa;

interface

uses
  Classes,
  Generics.Collections ,
  pcnConversao
  ;

type
  TCEmisNFE = class ;
  TCEmpresa = class (TPersistent)
  private
    class var Instance: TCEmpresa;
  private
    m_EmisNFE: TCEmisNFE ;
  public
    class function getInstance: TCEmpresa; static;
  public
    codfil: SmallInt;
    CNPJ: string;
    nmFant: string;
    rzSoci: string;
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
    property EmisNFE: TCEmisNFE read m_EmisNFE;
    constructor Create ;
    procedure DoLoad(const codfil: SmallInt);
  end;

  TCEmisNFE = class (TPersistent)
  private
    m_Owner: TCEmpresa ;
    procedure Load() ;
  public
    m_codseq: Int16;
    m_codemp: Int16;
    m_codufe: Int16;
    m_codmun: Int32;

    m_tipamb: TpcnTipoAmbiente;
    m_tipemi: TpcnTipoEmissao;
    m_tipimp: TpcnTipoImpressao;

    m_procemi: TpcnProcessoEmissao;
    m_verproc: string;

    m_certif: TMemoryStream;
    m_xsenha: string;
    m_serial: string;
    constructor Create (aOwner: TCEmpresa);
    procedure Merge ;
  end;



implementation

uses uadodb ;

{ TCEmpresa }

constructor TCEmpresa.Create;
begin
    m_EmisNFE :=TCEmisNFE.Create(Self) ;

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
      Self.CNPJ :=Q.Field('fil_cnpj').AsString ;
      Self.nmFant :=Q.Field('fil_xxfant').AsString;
      Self.RzSoci :=Q.Field('fil_rzsoci').AsString;
      Self.IE :=Q.Field('fil_ie').AsString ;
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

      //Self.CodCta:=Q.Field('fil_codcta').AsInteger;

      //arq_nome :=Trim(Q.Field('fil_logo').AsString);

    finally
      Q.Destroy;
    end;

    //cria stream logo marca
    {if FileExists(arq_nome) then
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
    }

    Self.m_EmisNFE.Load ;
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


{ TCEmisNFE }

constructor TCEmisNFE.Create(aOwner: TCEmpresa);
begin
    m_Owner :=aOwner ;

end;

procedure TCEmisNFE.Load;
var
  Q: TADOQuery ;
begin
    Q :=TADOQuery.NewADOQuery() ;
    try
        Q.AddCmd('select *from emisnfe where emi_codemp =%d',[m_Owner.codfil]);
        Q.Open ;
        if Q.IsEmpty then
        begin
            Self.m_codemp :=m_Owner.codfil ;
            {Self.m_tipamb :=Tdm_nfe.getInstance.m_NFE.Configuracoes.WebServices.Ambiente ;
            Self.m_tipemi :=Tdm_nfe.getInstance.m_NFE.Configuracoes.Geral.FormaEmissao ;
            Self.m_tipimp :=Tdm_nfe.getInstance.m_NFE.DANFE.TipoDANFE ;}
            Self.m_procemi :=peAplicativoContribuinte ;
            Self.m_verproc :='ATAC SVC 2.5 (2018)' ;
            //Self.Insert ;
        end
        else begin
            Self.m_tipamb :=TpcnTipoAmbiente(Q.Field('emi_tipamb').AsInteger) ;
            Self.m_tipemi :=TpcnTipoEmissao(Q.Field('emi_tipemi').AsInteger)  ;
            Self.m_tipimp :=TpcnTipoImpressao(Q.Field('emi_tipimp').AsInteger);
        end;
    finally
        Q.Free ;
    end;
end;

procedure TCEmisNFE.Merge;
var
  C: TADOCommand ;

begin
    C :=TADOCommand.NewADOCommand() ;
    try
      //
      // atualiza
      if Self.m_codseq > 0 then
      begin
          C.AddCmd('update emisnfe set  ') ;
          C.AddCmd('  emi_tipemi =%d    ',[Ord(Self.m_tipemi)]) ;
          C.AddCmd('where emi_codseq =%d',[Self.m_codseq]);
      end
      //
      // inclui novo emitente
      else begin
        C.AddCmd('insert into emisnfe ( emi_codemp, ');
        C.AddCmd('                      emi_tipamb, ');
        C.AddCmd('                      emi_tipemi, ');
        C.AddCmd('                      emi_tipimp) ');
        C.AddCmd('values              ( %d,--emi_codemp',[Self.m_codemp]);
        C.AddCmd('                      %d,--emi_tipamb',[Ord(Self.m_tipamb)]);
        C.AddCmd('                      %d,--emi_tipemi',[Ord(Self.m_tipemi)]);
        C.AddCmd('                      %d)--emi_tipimp',[Ord(Self.m_tipimp)]);
      end;
      //
      C.Execute ;
      //
    finally
      C.Free ;
    end;
end;

end.
