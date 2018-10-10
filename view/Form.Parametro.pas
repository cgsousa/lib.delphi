{***
* View/Form para edição de parametro do sistema.
* ATAC Sistemas
* Todos os Direitos Reservados
* Autor: Carlos Gonzaga
* Data: 17.08.2018
*}
unit Form.Parametro;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls,
  //JEDI
  JvExStdCtrls, JvExExtCtrls, JvExtComponent, JvExControls, JvCtrls,
  JvGradient, JvFooter, JvToolEdit, JvButton,
  //TMS
  AdvCombo, AdvGroupBox, AdvMemo, AdvEdit,
  //
  FormBase,
  uparam, JvMemo;

type
  Tfrm_Parametro = class(TBaseForm)
    pnl_Footer: TJvFooter;
    btn_OK: TJvFooterBtn;
    btn_Close: TJvFooterBtn;
    AdvGroupBox1: TAdvGroupBox;
    edt_ID: TAdvEdit;
    AdvGroupBox2: TAdvGroupBox;
    edt_ValXXX: TAdvEdit;
    txt_ValStr: TJvMemo;
    AdvGroupBox3: TAdvGroupBox;
    txt_Descricao: TJvMemo;
    JvGradient1: TJvGradient;
    cbx_ValXXX: TAdvComboBox;
    procedure btn_CloseClick(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    m_param: TCParametro ;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    { Public declarations }
    class function fn_Show(aParam: TCParametro): Boolean ;
  end;


implementation

{$R *.dfm}

uses DB ;


{ Tfrm_Parametro }

procedure Tfrm_Parametro.btn_CloseClick(Sender: TObject);
begin
    Self.Close
    ;
end;

procedure Tfrm_Parametro.btn_OKClick(Sender: TObject);
begin
    case m_param.ValTyp of
        ftSmallint,ftInteger,ftWord,ftAutoInc,
        ftFloat,ftCurrency: m_param.xValor :=edt_ValXXX.Text ;
        ftArray: m_param.xValor :=IntToStr(cbx_ValXXX.ItemIndex);
    else
        m_param.xValor :=txt_ValStr.Lines.Text ;
    end;
    m_param.Descricao :=txt_Descricao.Lines.Text ;
    m_param.Save ;
    ModalResult :=mrOk ;
end;

class function Tfrm_Parametro.fn_Show(aParam: TCParametro): Boolean;
var
  F: Tfrm_Parametro ;
begin
    F :=Tfrm_Parametro.Create(Application) ;
    try
        F.m_param :=aParam ;
        Result :=F.ShowModal =mrOk ;
    finally
        FreeAndNil(F);
    end;
end;

procedure Tfrm_Parametro.FormShow(Sender: TObject);
begin
    Self.DoClear(Self);
    Self.Caption :=Format('Modifica valor do Parametro: %s/%s',[m_param.Catego,
                                                                m_param.Ident]);
    txt_ValStr.Clear ;
    txt_Descricao.Clear;

    edt_ID.Text :=m_param.Ident ;
    txt_Descricao.Lines.Add(m_param.Descricao) ;
    //txt_Descricao.RefreshMemo ;

    //
    // config. entrada de dados
    case m_param.ValTyp of
        ftSmallint,ftInteger,ftWord,ftAutoInc:
        begin
            txt_ValStr.Visible :=False ;
            cbx_ValXXX.Visible :=False ;
            edt_ValXXX.EditType :=etNumeric ;
            edt_ValXXX.Precision:=0;
            edt_ValXXX.IntValue :=m_param.ReadInt() ;
            edt_ValXXX.SelStart :=Length(edt_ValXXX.Text) ;
            ActiveControl :=edt_ValXXX ;
        end;
        ftBoolean:
        begin
            txt_ValStr.Visible :=False ;
            edt_ValXXX.Visible :=False ;
            cbx_ValXXX.Top :=edt_ValXXX.Top ;
            cbx_ValXXX.Clear ;
            cbx_ValXXX.AddItem('NÃO', nil);
            cbx_ValXXX.AddItem('SIM', nil);
            cbx_ValXXX.ItemIndex :=m_param.ReadInt() ;
        end;
        ftFloat,ftCurrency:
        begin
            txt_ValStr.Visible :=False ;
            cbx_ValXXX.Visible :=False ;
            edt_ValXXX.EditType :=etFloat ;
            edt_ValXXX.Precision:=2;
            edt_ValXXX.FloatValue :=m_param.ReadFlt() ;
            edt_ValXXX.SelStart :=Length(edt_ValXXX.Text);
            ActiveControl :=edt_ValXXX ;
        end;
        ftArray:
        begin
            edt_ValXXX.Visible :=False ;
            txt_ValStr.Visible :=False ;
            cbx_ValXXX.Top :=edt_ValXXX.Top ;
            cbx_ValXXX.Items.CommaText :=m_param.Comple;
            cbx_ValXXX.ItemIndex :=m_param.ReadInt() ;
        end
    else
        edt_ValXXX.Visible :=False;
        txt_ValStr.Visible :=True ;
        txt_ValStr.Lines.Add(m_param.ReadStr() );
        //txt_ValStr.RefreshMemo ;
        txt_ValStr.SelStart :=Length(m_param.ReadStr()) ;
        ActiveControl :=txt_ValStr ;
    end;
end;

procedure Tfrm_Parametro.KeyDown(var Key: Word; Shift: TShiftState);
begin
    if not (ActiveControl is TAdvMemo) then
    begin
        inherited;
    end;
end;

end.
