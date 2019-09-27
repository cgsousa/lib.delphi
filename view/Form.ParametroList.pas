{***
* View/Form para lista de parametros do sistema.
* ATAC Sistemas
* Todos os Direitos Reservados
* Autor: Carlos Gonzaga
* Data: 17.08.2018
*}
unit Form.ParametroList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls,
  //JEDI
  JvExStdCtrls, JvExExtCtrls, JvExtComponent, JvCtrls,
  JvFooter, JvToolEdit, JvButton,
  //TMS
  AdvCombo, AdvGroupBox, HTMLabel ,
  //
  VirtualTrees, uVSTree,
  //
  FormBase,
  uparam ;


type
  Tfrm_ParametroList = class(TBaseForm)
    pnl_Footer: TJvFooter;
    btn_Close: TJvFooterBtn;
    vst_Grid1: TVirtualStringTree;
    cbx_Catego: TAdvComboBox;
    btn_Altera: TJvFooterBtn;
    htm_Status: THTMLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbx_CategoSelect(Sender: TObject);
    procedure vst_Grid1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure btn_CloseClick(Sender: TObject);
    procedure btn_AlteraClick(Sender: TObject);
    procedure vst_Grid1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    { Private declarations }
    m_params: TCParametroList ;
    m_catego: string ;
    procedure doResetForm ;
    procedure doLoadGrid ;
    procedure doParamNFE ;
  public
    { Public declarations }
    class procedure lp_Show(const aCatego: string);
  end;


const
  FMT_NFE_NUM_FISCAL_ID ='nfe.%s.nserie.%.3d';
  FMT_NFCE_NUM_FISCAL_ID ='nfce.%s.nserie.%.3d';

implementation

{$R *.dfm}

uses TypInfo, DB,
  pcnConversao ,
  uTaskDlg, uadodb,
  Form.Parametro;


{ Tfrm_Parametros }

procedure Tfrm_ParametroList.btn_AlteraClick(Sender: TObject);
var
  P: TCParametro ;
begin
    if CMsgDlg.Confirm('Deseja alterar o valor do parametro?')then
    begin
        P :=m_params.Items[vst_Grid1.IndexItem];

//        if(P.Catego ='NFE')and(Pos('forma_emissao', P.Ident)>0) then
//        begin
//            P.ComboBox.Clear ;
//            P.ComboBox.AddStrings(m_tipemi);
//        end;
        if Tfrm_Parametro.fn_Show(P) then
        begin
            doLoadGrid ;
        end;
        ActiveControl :=vst_Grid1 ;
    end;
end;

procedure Tfrm_ParametroList.btn_CloseClick(Sender: TObject);
begin
    Self.Close ;

end;

procedure Tfrm_ParametroList.cbx_CategoSelect(Sender: TObject);
begin
    doLoadGrid ;

end;

procedure Tfrm_ParametroList.doLoadGrid;
begin
    vst_Grid1.Clear ;
    if cbx_Catego.ItemIndex > 0 then
        m_params.Load('', cbx_Catego.Text)
    else
        m_params.Load('', '');
    vst_Grid1.RootNodeCount :=m_params.Count ;
    if vst_Grid1.RootNodeCount > 0 then
    begin
        vst_Grid1.IndexItem :=0 ;
        btn_Altera.Enabled :=True;
    end
    else
        btn_Altera.Enabled :=False;
end;

procedure Tfrm_ParametroList.doParamNFE;
var
  Q: TADOQuery ;
  P: TCParametro ;
var
  fstr_mod, fnum_ser, fult_num: TField ;
  id_param, descr_param: string ;
var
  I: TpcnTipoEmissao;
  tip_emi: TStrings ;
begin
    Q :=TADOQuery.NewADOQuery(False) ;
    try
      //
      // ler old numeros
      Q.AddCmd('select  modelonf as str_mod,          ');
      Q.AddCmd('        numerocaixa as num_ser,       ');
      Q.AddCmd('        max(codcontadornfe) as ult_num');
      Q.AddCmd('from contadornfe                      ');
      Q.AddCmd('group by modelonf, numerocaixa        ');
      Q.Open ;
      fstr_mod :=Q.Field('str_mod');
      fnum_ser :=Q.Field('num_ser');
      fult_num :=Q.Field('ult_num');
      while not Q.Eof do
      begin
          if fstr_mod.AsString ='NFE' then
          begin
              id_param :=Format(FMT_NFE_NUM_FISCAL_ID,
                                [Empresa.CNPJ,fnum_ser.AsInteger]);
              //
              // procura param na lista
              P :=m_params.IndexOf(id_param);
              descr_param :='UUID PARA NFE POR CNPJ/MOD/SERIE';
          end
          else begin
              id_param :=Format(FMT_NFCE_NUM_FISCAL_ID,
                                [Empresa.CNPJ,fnum_ser.AsInteger]);
              //
              // procura param na lista
              P :=m_params.IndexOf(id_param);
              descr_param :='UUID PARA NFCE POR CNPJ/MOD/SERIE';
          end;

          //
          // se não existe!
          // cria um novo parametro (genserial)
          if P = nil then
          begin
              P :=m_params.AddNew(id_param) ;
              P.xValor :=fult_num.AsString ;
              P.ValTyp:=ftAutoInc ;
              P.Catego :='NFE';
              P.Descricao:=descr_param;
              P.Save ;
          end;
          Q.Next ;
      end;
    finally
      Q.Free ;
    end;

    //
    // Forma Emissao por CNPJ
    //
    // forma emissão
    P :=TCParametro.NewParametro(Format('forma_emissao.%s',[Empresa.CNPJ]), ftArray);
    if not P.Load() then
    begin
        //
        // obtem os tipos enumerados
        // tipo emissão do ACBr
        tip_emi :=TStringList.Create ;
        for I := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
            tip_emi.Add(
                          GetEnumName(TypeInfo(TpcnTipoEmissao), Integer(I) )
                          ) ;
        //
        // inicializa param
        P.xValor :='0';
        P.Comple :=tip_emi.CommaText ;
        P.Catego :='NFE';
        P.Descricao :='Forma de emissão da NFE/NFCe por CNPJ' ;
        P.Save ;
    end;

end;

procedure Tfrm_ParametroList.doResetForm;
var
  L: TCParametroList ;
  P: TCParametro ;
begin

    cbx_Catego.Clear ;
    cbx_Catego.AddItem('TODOS', nil);
    if m_catego <> '' then
    begin
        cbx_Catego.AddItem(m_catego, nil);
        cbx_Catego.ItemIndex :=1;
        cbx_Catego.Enabled :=False ;
        if m_catego = 'NFE' then
        begin
            m_params.Load('', m_catego) ;
        end;
    end
    else begin
        cbx_Catego.ItemIndex :=0;
        m_params.Load('', '') ;
    end;

//    doParamNFE ;
    RegSistem.Load ;

    L :=TCParametroList.getCatList ;
    for P in L do
    begin
        cbx_Catego.AddItem(P.Catego, nil);
    end;
    doLoadGrid ;
    ActiveControl :=vst_Grid1 ;
end;

procedure Tfrm_ParametroList.FormCreate(Sender: TObject);
begin
    m_params :=TCParametroList.Create
    ;
end;

procedure Tfrm_ParametroList.FormShow(Sender: TObject);
begin
    doResetForm ;

end;

class procedure Tfrm_ParametroList.lp_Show(const aCatego: string);
var
  F: Tfrm_ParametroList ;
begin
    F :=Tfrm_ParametroList.Create(Application) ;
    try
        F.vst_Grid1.Clear ;
        F.m_catego :=aCatego ;
        F.ShowModal ;
    finally
        FreeAndNil(F) ;
    end;
end;

procedure Tfrm_ParametroList.vst_Grid1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  P: TCParametro ;
begin
    if Assigned(Node) then
    begin
        P :=m_params.Items[Node.Index] ;
        htm_Status.HTMLText.Clear;
        htm_Status.HTMLText.Add(P.Descricao) ;
    end;
end;

procedure Tfrm_ParametroList.vst_Grid1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
const
  FLAG_SN: array[Boolean] of string = ('NÃO', 'SIM');
var
  P: TCParametro;
  V, C: Integer ;
  tip_emi: TStrings ;
begin
    if Assigned(Node) then
    begin
        P :=m_params.Items[Node.Index] ;
        case Column of
            0: CellText :=P.Ident ;
            //
            // format valor
            1:
            case p.ValTyp of
                ftSmallint, ftInteger, ftWord: CellText :=P.xValor;
                ftBoolean: CellText := FLAG_SN[P.ReadBoo];
                ftAutoInc:if P.xValor <> '' then
                          begin
                              CellText :=P.xValor;
                          end
                          else begin
                              CellText :='Ainda não utilizado';
                          end;
                //ftFloat,ftCurrency:
                ftArray:
                begin
                    Val(P.xValor, V, C);
                    if C<>0 then
                        CellText :=Format('Erro convert [pos:%d]',[C])
                    else begin
                        tip_emi :=TStringList.Create;
                        try
                            tip_emi.CommaText :=P.Comple ;
                            CellText :=tip_emi.Strings[V];
                        finally
                            tip_emi.Free ;
                        end;
                    end;
                end;
            else
                CellText :=P.xValor;
            end;

            {if(P.Catego ='NFE')and(Pos('forma_emissao', P.Ident) > 0)then
            begin
              Val(P.xValor, V, C);
              if C<>0 then
                  CellText :=Format('Erro convert [pos:%d]',[C])
              else
                  CellText :=m_tipemi.Strings[V];
            end
            else begin
                CellText :=P.xValor;
            end;}
            2: CellText :=P.Catego;
        end;
    end;
end;

end.
