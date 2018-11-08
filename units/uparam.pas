unit uparam;

interface

uses
  Classes,
  DB,
  SysUtils,
  Generics.Collections
  ;


type
  TCParametro = class
  private
    _id: string ;
    _xtype: TFieldType;
    _xvalue: string;
    _comple: string;
    _catego: string;
    _descri: string;
    _codfil: smallint;
    _exists: Boolean;
    procedure Merge;
  public
    constructor NewParametro(const AParamName: string;
      const AParamType: TFieldType) ;
    function Load(const AParamName: string =''): Boolean ;
    function ReadStr(const ADefault: string = ''): string ;
    function ReadInt(const ADefault: Int64 = 0): Int64 ;
    function ReadBoo(const ADefault: Boolean = false): Boolean ;
    function ReadFlt(const ADefault: Double = 0): Double ;
    function ReadDat(const ADefault: TDateTime = 0): TDateTime;
    procedure Save ;
  public
    property Ident: string read _id;
    property ValTyp: TFieldType read _xtype write _xtype;
    property xValor: string read _xvalue write _xvalue;
    property Comple: string read _comple write _comple;
    property Catego: string read _catego write _catego;
    property Descricao: string read _descri write _descri;
  end;

  TCParametroList = class (TList<TCParametro>)
  private
  public
    function AddNew(const AParamName: string): TCParametro ;
    function IndexOf(const AParamName: string): TCParametro; overload ;
    function Load(const aIdent, aCatego: string): Boolean;
    class function getCatList: TCParametroList ;
  end;

  //
  // params system
  TRegSistem = record
  public
    //
    // servidor smtp
    smtp_serv: TPair<string, string>;
    smtp_port: TPair<string, Word>;
    smtp_usr: TPair<string, string>;
    smtp_pwd: TPair<string, string>;
    smtp_ssl: TPair<string, Boolean>;
    smtp_tls: TPair<string, Boolean>;
    procedure Load() ;
  end;


  //var
//  PARAM_BOO_SN: array[Boolean] of string = ('NÃO','SIM');

var
  RegSistem: TRegSistem ;


implementation

uses uadodb ;

{ TCParametro }

function TCParametro.Load(const AParamName: string): Boolean;
var
  Q: TADOQuery ;
begin
    //
    if AParamName <> '' then
        Self._id :=AParamName ;
    //
    Q :=TADOQuery.NewADOQuery();
    try
        Q.AddCmd('declare @prm_ident varchar(50); set @prm_ident = %s ',[Q.FStr(Self._id)]) ;
        Q.AddCmd('select *from parametro                              ');
        Q.AddCmd('where prm_ident = @prm_ident                        ');
        Q.Open ;
        Result :=not Q.IsEmpty;
        if Result then
        begin
            _exists :=True ;
            Self._xtype :=TFieldType(Q.Field('prm_valtyp').AsInteger) ;
            Self._xvalue :=Q.Field('prm_xvalue').AsString ;
            Self._catego :=Q.Field('prm_catego').AsString ;
            Self._descri :=Q.Field('prm_descri').AsString ;
        end
        else begin
            _exists :=False ;
            Self._xvalue :='';
            Self._descri :='';
        end;
    finally
        Q.Free ;
    end;
end;

procedure TCParametro.Merge;
var
  C: TADOCommand ;
  P: Integer;
  cat: string;
begin
    C :=TADOCommand.NewADOCommand ;
    try
        C.AddColumn('prm_valtyp', ftSmallint, Ord(Self._xtype)) ;
        case Self._xtype of
            ftSmallint,ftInteger,ftWord: C.AddColumn('prm_xvalue', ftInteger, Self.ReadInt()) ;
            ftBoolean: C.AddColumn('prm_xvalue', ftBoolean, Self.ReadBoo()) ;
            ftDateTime: C.AddColumn('prm_xvalue', ftDateTime, Self.ReadDat());
            ftFloat,ftCurrency: C.AddColumn('prm_xvalue', ftFloat, Self.ReadFlt());
        else
            C.AddColumn('prm_xvalue', ftString, Self.ReadStr()) ;
        end;

        if Self._catego = '' then
        begin
            Self._catego :='GERAL' ;
        end;
        C.AddColumn('prm_catego', ftString, Self._catego) ;

        if Self._descri <> '' then
        begin
            C.AddColumn('prm_descri', ftString, Self._descri) ;
        end;

        C.AddColumn('prm_ident', ftString, Self._id, True) ;
        if Self._exists then
            C.ExecUpdate('parametro')
        else
            C.ExecInsert('parametro');
    finally
        C.Free ;
    end;
end;

constructor TCParametro.NewParametro(const AParamName: string;
  const AParamType: TFieldType);
begin
    Create ;
    Self._id :=AParamName ;
    Self._xtype:=AParamType;
    case Self._xtype of
        ftString: Self._xvalue:='';
        ftSmallint,ftInteger,ftWord: Self._xvalue:='0';
        ftBoolean: Self._xvalue:='0';
        ftFloat,ftCurrency: Self._xvalue:='0,00';
    end;
    Self._catego:='';
    Self._descri:='';
    Self._codfil:=99;
end;

function TCParametro.ReadBoo(const ADefault: Boolean): Boolean;
begin
    if not TryStrToBool(Self._xvalue, Result) then
    begin
        Result :=ADefault;
    end;
end;

function TCParametro.ReadDat(const ADefault: TDateTime): TDateTime;
begin
    if not TryStrToDate(Self._xvalue, Result) then
    begin
        Result :=ADefault;
    end;
end;

function TCParametro.ReadFlt(const ADefault: Double = 0): Double ;
begin
    if not TryStrToFloat(Self._xvalue, Result) then
    begin
        Result :=ADefault;
    end;
end;

function TCParametro.ReadInt(const ADefault: Int64): Int64;
begin
    if not TryStrToInt64(Self._xvalue, Result) then
    begin
        Result :=ADefault;
    end;
end;

function TCParametro.ReadStr(const ADefault: string): string;
begin
    if Self._xvalue <> '' then
        Result :=Self._xvalue
    else
        Result :=ADefault
    ;
end;

procedure TCParametro.Save;
var
  C: TADOCommand ;
  P: Integer;
  cat: string;
begin
    C :=TADOCommand.NewADOCommand ;
    try
        C.AddCmd('declare @prm_ident varchar(50); set @prm_ident =%s',[C.FStr(Self._id)]);
        C.AddCmd('declare @prm_valtyp smallint; set @prm_valtyp =%d',[Ord(Self._xtype)]);
        if Self._xtype = ftAutoInc then
        begin
            C.AddCmd('declare @prm_xvalue bigint; set @prm_xvalue =%d',[Self.ReadInt()]) ;
            C.AddCmd('declare @prm_maxval bigint  set @prm_maxval =0 ');
            if Self._catego = 'NFE' then
            begin
                C.AddCmd('set @prm_maxval =999999999                 ');
            end;
        end
        else
            C.AddCmd('declare @prm_xvalue varchar(125); set @prm_xvalue =%s',[C.FStr(Self._xvalue)]);

        C.AddCmd('declare @prm_catego varchar(30); set @prm_catego =%s',[C.FStr(Self._catego)]);

        if Self._descri <> '' then
            C.AddCmd('declare @prm_descri varchar(50); set @prm_descri =%s',[C.FStr(Self._descri)])
        else
            C.AddCmd('declare @prm_descri varchar(50); set @prm_descri =null');

        if Self._comple <> '' then
            C.AddCmd('declare @prm_comple varchar(125); set @prm_comple =%s',[C.FStr(Self._comple)])
        else
            C.AddCmd('declare @prm_comple varchar(125); set @prm_comple =null');

        if Self._exists then
        begin
            //
            // param auto-inc
            if Self._xtype = ftAutoInc then
            begin
                C.AddCmd('update genserial set          ') ;
                C.AddCmd('  ser_valor   =@prm_xvalue ,  ') ;
                C.AddCmd('  ser_descri  =@prm_descri    ') ;
                C.AddCmd('where ser_ident =@prm_ident   ') ;
            end
            //
            // param normal
            else begin
                C.AddCmd('update parametro set          ') ;
                C.AddCmd('  prm_valtyp  =@prm_valtyp ,  ') ;
                C.AddCmd('  prm_xvalue  =@prm_xvalue ,  ') ;
                C.AddCmd('  prm_catego  =@prm_catego ,  ') ;
                C.AddCmd('  prm_descri  =@prm_descri ,  ') ;
                C.AddCmd('  prm_update  =getdate()      ') ;
                C.AddCmd('where prm_ident =@prm_ident   ') ;
            end;
        end
        else begin
            //
            // param auto-inc
            if Self._xtype = ftAutoInc then
            begin
                C.AddCmd('insert into genserial(ser_ident  , ') ;
                C.AddCmd('                      ser_valor  , ') ;
                C.AddCmd('                      ser_inival , ') ;
                C.AddCmd('                      ser_maxval , ') ;
                C.AddCmd('                      ser_catego , ') ;
                C.AddCmd('                      ser_descri ) ') ;
                C.AddCmd('values (@prm_ident  , ') ;
                C.AddCmd('        @prm_xvalue , ') ;
                C.AddCmd('        @prm_xvalue , ') ;
                C.AddCmd('        @prm_maxval , ') ;
                C.AddCmd('        @prm_catego , ') ;
                C.AddCmd('        @prm_descri)  ') ;
            end
            //
            // param normal
            else begin
                C.AddCmd('insert into parametro(prm_ident  , ') ;
                C.AddCmd('                      prm_valtyp , ') ;
                C.AddCmd('                      prm_xvalue , ') ;
                C.AddCmd('                      prm_catego , ') ;
                C.AddCmd('                      prm_descri , ') ;
                C.AddCmd('                      prm_comple ) ') ;
                C.AddCmd('values (@prm_ident  , ') ;
                C.AddCmd('        @prm_valtyp , ') ;
                C.AddCmd('        @prm_xvalue , ') ;
                C.AddCmd('        @prm_catego , ') ;
                C.AddCmd('        @prm_descri , ') ;
                C.AddCmd('        @prm_comple ) ') ;
            end;
        end;
        //C.SaveToFile();
        C.Execute ;
    finally
        C.Free ;
    end;
end;

{ TCParametroList }

function TCParametroList.AddNew(const AParamName: string): TCParametro;
var
  p: Integer ;
begin
    Result :=TCParametro.Create ;
    p :=Pos(';', AParamName) ;
    if p > 0 then
    begin
        Result._id :=Copy(AParamName, p+1, Length(AParamName)) ;
        Result._catego :=Copy(AParamName, 1, p-1) ;
    end
    else
        Result._id :=AParamName ;
    Add(Result) ;
end;

class function TCParametroList.getCatList: TCParametroList;
var
  Q: TADOQuery ;
  P: TCParametro;
begin
    //
    Result :=TCParametroList.Create ;

    //
    Q :=TADOQuery.NewADOQuery();
    try
        Q.AddCmd('select               ');
        Q.AddCmd('  prm_catego         ');
        Q.AddCmd('from parametro       ');
        Q.AddCmd('group by prm_catego  ');
        //
        Q.Open ;
        //
        while not Q.Eof do
        begin
            P :=Result.AddNew('') ;
            P._catego:=Q.Field('prm_catego').AsString;
            Q.Next ;
        end;
    finally
        Q.Free ;
    end;
end;

function TCParametroList.IndexOf(const AParamName: string): TCParametro;
var
  found: Boolean ;
begin
    found :=False;
    for Result in Self do
    begin
        if Result._id =AParamName then
        begin
            found :=True ;
            Break ;
        end;
    end;
    if not found then
    begin
        Result :=nil ;
    end;
end;

function TCParametroList.Load(const aIdent, aCatego: string): Boolean;
var
  Q: TADOQuery ;
  P: TCParametro;
begin
    //
    Self.Clear ;
    //
    // parametros
    Q :=TADOQuery.NewADOQuery();
    try
      Q.AddCmd('declare @identi varchar(50); set @identi = %s ',[Q.FStr(aIdent)]) ;
      Q.AddCmd('declare @catego varchar(30); set @catego = %s ',[Q.FStr(aCatego)]) ;
      Q.AddCmd('select                                        ');
      Q.AddCmd('  prm_ident   ,                               ');
      Q.AddCmd('  prm_valtyp  ,                               ');
      Q.AddCmd('  prm_xvalue  ,                               ');
      Q.AddCmd('  prm_catego  ,                               ');
      Q.AddCmd('  prm_descri  ,                               ');
      Q.AddCmd('  prm_comple                                  ');
      Q.AddCmd('from parametro                                ');

      if aIdent <> '' then
      begin
          Q.AddCmd('where prm_ident = @identi                ');
      end
      else if aCatego <> '' then
      begin
          Q.AddCmd('where prm_catego = @catego               ');
      end;

      //
      // link com genserial
      // e define o tipo do param como autoinc(14)
      Q.AddCmd('--//                                       ');
      Q.AddCmd('union all                                  ');
      Q.AddCmd('--//                                       ');
      Q.AddCmd('select                                     ');
      Q.AddCmd('  ser_ident  ,                             ');
      Q.AddCmd('  14 as ser_valtype,                       ');
      Q.AddCmd('  convert(varchar,ser_valor) as ser_xvalue,');
      Q.AddCmd('  ser_catego ,                             ');
      Q.AddCmd('  ser_descri ,                             ');
      Q.AddCmd('  null as ser_comple                       ');
      Q.AddCmd('from genserial                             ');

      if aIdent <> '' then
      begin
          Q.AddCmd('where ser_ident = @identi              ');
      end
      else if aCatego <> '' then
      begin
          Q.AddCmd('where ser_catego = @catego             ');
      end;

      Q.AddCmd('order by prm_catego, prm_ident             ');

      //
      Q.Open ;
      //
      Result :=not Q.IsEmpty;
      while not Q.Eof do
      begin
          P :=Self.IndexOf(Q.Field('prm_ident').AsString) ;
          if P = nil then
          begin
              P :=Self.AddNew(Q.Field('prm_ident').AsString) ;
              P._xtype :=TFieldType(Q.Field('prm_valtyp').AsInteger) ;
              P._xvalue:=Q.Field('prm_xvalue').AsString;
              P._catego:=Q.Field('prm_catego').AsString;
              P._descri:=Q.Field('prm_descri').AsString;
              P._comple:=Q.Field('prm_comple').AsString;
              P._exists:=True ;
          end;
          Q.Next ;
      end;

    finally
        Q.Free ;
    end;
end;



{ TRegSistem }

procedure TRegSistem.Load;
const
  SYS_CATEGO = 'SISTEMA' ;
var
  params: TCParametroList ;
  p: TCParametro ;
begin
    //
    //
    params :=TCParametroList.Create ;
    try
        //
        // carrega parametros do sistema
        params.Load('', SYS_CATEGO) ;

        //
        // servidor smtp
        smtp_serv.Key :='smtp_serv';
        p :=params.IndexOf(smtp_serv.Key) ;
        if p = nil then
        begin
            p :=params.AddNew(smtp_serv.Key) ;
            p.ValTyp :=ftString ;
            P.xValor :='smtp.gmail.com';
            P.Catego :='SISTEMA';
            p.Descricao :='Servidor SMTP';
            P.Save ;
        end;
        smtp_serv.Value :=p.ReadStr() ;

        //
        // porta smtp
        smtp_port.Key :='smtp_port';
        p :=params.IndexOf(smtp_port.Key) ;
        if p = nil then
        begin
            p :=params.AddNew(smtp_port.Key) ;
            p.ValTyp :=ftInteger ;
            P.xValor :='587'; //'465';
            P.Catego :='SISTEMA';
            p.Descricao :='Porta do servidor SMTP';
            P.Save ;
        end;
        smtp_port.Value :=p.ReadInt() ;

        //
        // user smtp
        smtp_usr.Key :='smtp_usr';
        p :=params.IndexOf(smtp_usr.Key) ;
        if p = nil then
        begin
            p :=params.AddNew(smtp_usr.Key) ;
            p.ValTyp :=ftString ;
            P.xValor :='rodrigo@atacsistemas.com';
            P.Catego :='SISTEMA';
            p.Descricao :='Usuario de login do servidor SMTP';
            P.Save ;
        end;
        smtp_usr.Value :=p.ReadStr() ;

        //
        // pwd smtp
        smtp_pwd.Key :='smtp_pwd';
        p :=params.IndexOf(smtp_pwd.Key) ;
        if p = nil then
        begin
            p :=params.AddNew(smtp_pwd.Key) ;
            p.ValTyp :=ftString ;
            P.xValor :='atac0412';
            P.Catego :='SISTEMA';
            p.Descricao :='Senha de login do servidor SMTP';
            P.Save ;
        end;
        smtp_pwd.Value :=p.ReadStr() ;

        //
        // SSL
        smtp_ssl.Key :='smtp_ssl';
        p :=params.IndexOf(smtp_ssl.Key) ;
        if p = nil then
        begin
            p :=params.AddNew(smtp_ssl.Key) ;
            p.ValTyp :=ftBoolean ;
            P.xValor :='0';
            P.Catego :='SISTEMA';
            p.Descricao :='Usa SSL';
            P.Save ;
        end;
        smtp_ssl.Value :=p.ReadBoo() ;

        //
        // TLS
        smtp_tls.Key :='smtp_tls';
        p :=params.IndexOf(smtp_tls.Key) ;
        if p = nil then
        begin
            p :=params.AddNew(smtp_tls.Key) ;
            p.ValTyp :=ftBoolean ;
            P.xValor :='1';
            P.Catego :='SISTEMA';
            p.Descricao :='Usa TLS';
            P.Save ;
        end;
        smtp_tls.Value :=p.ReadBoo() ;

    finally
        params.Free ;
    end;
end;

end.
