unit uStatusBar;

interface

{$I TMSDEFS.INC}

uses ComCtrls
  {$IFDEF TMSPACK}
  ,AdvOfficeStatusBar, AdvOfficeStatusBarStylers
  {$ENDIF}
  ;

type
  TCStatusBarWidget = class
  private
  {$IFDEF TMSPACK}
    m_Origin: TAdvOfficeStatusBar;
  {$ELSE}
    m_Origin: TStatusBar;
  {$ENDIF}
    m_Config: string;
    m_Filter: string;
    m_Version: string;
    m_Status: string;
    procedure Panels() ;
  public
    constructor Create(aOrigin: {$IFDEF TMSPACK}TAdvOfficeStatusBar{$ELSE}TStatusBar{$ENDIF}); reintroduce;

  end;

implementation

{ TCStatusBarWidget }

constructor TCStatusBarWidget.Create(aOrigin: {$IFDEF TMSPACK}TAdvOfficeStatusBar{$ELSE}TStatusBar{$ENDIF});
begin
    m_Origin :=aOrigin ;

end;

procedure TCStatusBarWidget.Panels;
//var
//  P: {$IFDEF TMSPACK} TAdvOfficeStatusPanel {$ELSE} TStatusPanel {$ENDIF} ;
begin
    m_Origin.Panels.Clear ;
    with m_Origin.Panels.Add do
    begin
        Text :='F2-Consulta cadastro' ;
        Width:=121 ;
    end;
end;

end.
