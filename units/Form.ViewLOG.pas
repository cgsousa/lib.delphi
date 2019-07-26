unit Form.ViewLOG;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  FormBase,
  JvExStdCtrls, JvRichEdit ;

type
  IViewLOG = Interface(IInterface)
    procedure setTitle(const aValue: string);
    procedure setStr(const aStr: string) ;
    procedure setVisible(const aValue: Boolean) ;
  end;

  Tfrm_ViewLOG = class(TBaseForm, IViewLOG)
    txt_Log: TJvRichEdit;
  private
    { Private declarations }
    m_JvRich: TJvRichEdit;
    procedure doCreateCtls ;
  private
    { IViewLOG }
    procedure setTitle(const aValue: string);
    procedure AddInfo(const aStr: string);
  public
    { Public declarations }
    constructor CreateNew(aOwner: TComponent; Dummy: Integer); override;
    procedure OnStr(const aStr: string) ;
    procedure setVisible(const aValue: Boolean) ;
    procedure setStr(const aStr: string) ;
    class function New(const aTitle: string): IViewLOG;
  end;



implementation

{.$R *.dfm}

{ Tfrm_ViewLOG }

procedure Tfrm_ViewLOG.AddInfo(const aStr: string);
begin
    //Self.Hide
end;

constructor Tfrm_ViewLOG.CreateNew(aOwner: TComponent; Dummy: Integer);
begin
    inherited CreateNew(aOwner, Dummy);
    BorderIcons :=[biSystemMenu];
    BorderStyle :=bsSingle ;
    BorderWidth :=3 ;
    Ctl3D :=False;
    Font.Name :='Trebuchet MS' ;
    Font.Size :=10 ;
    Height :=480;
    Width :=640 ;
    DefaultMonitor :=dmDesktop;
	  Position  :=poScreenCenter;
    //
    //
    doCreateCtls ;
end;

procedure Tfrm_ViewLOG.doCreateCtls;
begin
    m_JvRich :=TJvRichEdit.Create(Self);
    m_JvRich.Parent :=Self ;
    m_JvRich.Align :=alClient ;
    m_JvRich.Color :=clInfoBk ;
    m_JvRich.ReadOnly :=True ;
    m_JvRich.ScrollBars :=ssVertical ;
    m_JvRich.Clear ;
end;

class function Tfrm_ViewLOG.New(const aTitle: string): IViewLOG;
begin
    Result :=Tfrm_ViewLOG.CreateNew(Application, 0);
    Result.setTitle(aTitle);
end;

procedure Tfrm_ViewLOG.OnStr(const aStr: string);
var
  P: Integer ;
  S: string ;
begin
    //
    // busca info
    P :=Pos('ERR', aStr) ;
    if P > 0 then
    begin
        S :=Copy(aStr,4,Length(aStr));
        m_JvRich.AddFormatText(S, [], 'Trebuchet MS', clRed) ;
        m_JvRich.AddFormatText(sLineBreak,[]) ;
    end
    else
        m_JvRich.Lines.Add(aStr);
    ActiveControl :=m_JvRich ;
    {txt_Log.SelLength := 0;
    txt_Log.SelStart:=txt_Log.GetTextLen;
    txt_Log.Perform( EM_SCROLLCARET, 0, 0 );}
end;

procedure Tfrm_ViewLOG.setStr(const aStr: string);
begin
    Self.OnStr(aStr);
    Self.Show ;
end;

procedure Tfrm_ViewLOG.setTitle(const aValue: string);
begin
    Self.Caption :=aValue ;

end;

procedure Tfrm_ViewLOG.setVisible(const aValue: Boolean);
begin
    Self.Visible :=aValue ;

end;

end.
