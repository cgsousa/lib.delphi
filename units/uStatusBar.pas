unit uStatusBar;

interface

{$I TMSDEFS.INC}

uses ComCtrls
  {$IFDEF TMSPACK}
  ,AdvOfficeStatusBar, AdvOfficeStatusBarStylers
  {$ENDIF}
  ;

type
  {.$IFDEF TMSPACK}
  {.$ESLE}
  {.$ENDIF}

  TCStatusBarWidget = class
  private
    const ID_CONFIG =0;
    const ID_FILTER =1;
    const ID_NEW =2;
    const ID_EDIT =3;
    const ID_COMMIT =4;
    const ID_DELETE =5;
    const ID_STATUS =6;
  private
  {$IFDEF TMSPACK}
    m_Origin: TAdvOfficeStatusBar;
  {$ELSE}
    m_Origin: TStatusBar;
  {$ENDIF}
    m_Config: string;
    m_Filter: string;
    m_New: string;
    m_Edit: string;
    m_Commit: string;
    m_Delete: string;
    m_Status: string;
    procedure setStatus(Value: string) ;
    procedure AddPanels() ;
  public
    constructor Create(aOrigin: {$IFDEF TMSPACK}TAdvOfficeStatusBar{$ELSE}TStatusBar{$ENDIF};
      const aSimple: Boolean); reintroduce;
    property Config: string read m_Config write m_Config;
    property Filter: string read m_Filter write m_Filter;
    property NewItem: string read m_New write m_New;
    property EditItem: string read m_Edit write m_Edit;
    property Commit: string read m_Commit write m_Commit;
    property DeleteItem: string read m_Delete write m_Delete;
    property Status: string read m_Status write setStatus;
    function AddPanel(
      const aStyle: {$IFDEF TMSPACK}TAdvOfficeStatusPanelStyle{$ELSE}TStatusPanelStyle{$ENDIF};
      const aWidth: Word =0
      ): {$IFDEF TMSPACK}TAdvOfficeStatusPanel{$ELSE}TStatusPanel{$ENDIF};
  end;

implementation


{ TCStatusBarWidget }

function TCStatusBarWidget.AddPanel(
  const aStyle: {$IFDEF TMSPACK}TAdvOfficeStatusPanelStyle{$ELSE}TStatusPanelStyle{$ENDIF};
  const aWidth: Word
  ): {$IFDEF TMSPACK}TAdvOfficeStatusPanel{$ELSE}TStatusPanel{$ENDIF};
begin
    Result :=m_Origin.Panels.Add ;
    Result.Style:=aStyle;
    if aWidth > 0 then
        Result.Width:=aWidth;
end;

procedure TCStatusBarWidget.AddPanels;
begin
    with m_Origin.Panels.Add do
    begin
        Text :='Ctrl+F1=Configurações';
        Width:=135 ;
    end;
    with m_Origin.Panels.Add do
    begin
        Text :='F2=Filtro/Busca' ;
        Width:=115 ;
    end;
    with m_Origin.Panels.Add do
    begin
        Text :='F3=Novo' ;
        Width:=75 ;
    end;
    with m_Origin.Panels.Add do
    begin
        Text :='F4=Alterar' ;
        Width:=75 ;
    end;
    with m_Origin.Panels.Add do
    begin
        Text :='F6=Gravar' ;
        Width:=75 ;
    end;
    with m_Origin.Panels.Add do
    begin
        Text :='F7=Deletar' ;
        Width:=75 ;
    end;
    with m_Origin.Panels.Add do
    begin
        Text :='Status' ;
    end;
end;

constructor TCStatusBarWidget.Create(
  aOrigin: {$IFDEF TMSPACK}TAdvOfficeStatusBar{$ELSE}TStatusBar{$ENDIF};
  const aSimple: Boolean);
begin
    m_Origin :=aOrigin ;
    m_Origin.SimplePanel :=aSimple ;
    if not m_Origin.SimplePanel then
    begin
        m_Origin.Panels.Clear ;
        //AddPanels ;
    end;
end;

procedure TCStatusBarWidget.setStatus(Value: string);
begin
    if m_Origin.SimplePanel then
    begin
        m_Origin.SimpleText :=Value ;
    end
    else begin
        m_Origin.Panels[ID_STATUS].Text :=Value ;
    end;
end;

end.
