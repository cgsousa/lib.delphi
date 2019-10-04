unit Form.ViewLOG;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  uclass, FormBase,
  JvExStdCtrls, JvRichEdit ;

type
  //TModelChangedEvent = procedure of object;
  IViewLOG = Interface(IInterface)
    procedure addStr(const aStr: string; const aColor: TColor =clDefault);
    procedure clear ;
    procedure showLOG(const aModal: Boolean);
    procedure hideLOG ;
  end;

  Tfrm_ViewLOG = class(TBaseForm, IViewLOG)
    txt_Log: TJvRichEdit;
  private
    { Private declarations }
    m_JvRich: TJvRichEdit;
    procedure doCreateCtls ;
  private
    { IViewLOG }
  public
    { Public declarations }
    constructor CreateNew(const aCaption: string;
      const aHeight, aWidth: Integer); reintroduce;
    destructor Destroy; override ;
    procedure addStr(const aStr: string; const aColor: TColor =clDefault);
    procedure clear ;
    procedure showLOG(const aModal: Boolean) ;
    procedure hideLOG ;
    class function New(const aTitle: string;
      const aHeight: Integer =240;
      const aWidth: Integer =320): IViewLOG;
  end;



implementation

{.$R *.dfm}

{ Tfrm_ViewLOG }

procedure Tfrm_ViewLOG.addStr(const aStr: string; const aColor: TColor);
begin
    if aColor <> clDefault then
    begin
        m_JvRich.AddFormatText(aStr, [], 'Trebuchet MS', aColor) ;
        m_JvRich.AddFormatText(sLineBreak,[]) ;
    end
    else
    m_JvRich.Lines.Add(aStr);
    ActiveControl :=m_JvRich ;
    {txt_Log.SelLength := 0;
    txt_Log.SelStart:=txt_Log.GetTextLen;
    txt_Log.Perform( EM_SCROLLCARET, 0, 0 );}
end;

procedure Tfrm_ViewLOG.clear;
begin
    m_JvRich.Clear ;

end;

constructor Tfrm_ViewLOG.CreateNew(const aCaption: string;
  const aHeight, aWidth: Integer);
begin
    inherited CreateNew(Application, 0);
    BorderIcons :=[biSystemMenu];
    BorderStyle :=bsSingle ;
    BorderWidth :=3 ;
    Ctl3D :=False;
    Font.Name :='Trebuchet MS' ;
    Font.Size :=10 ;
    Height :=aHeight;
    Width :=aWidth ;
    DefaultMonitor :=dmDesktop;
	  Position  :=poScreenCenter;
    Caption :=aCaption ;
    //
    //
    doCreateCtls ;

end;

destructor Tfrm_ViewLOG.Destroy;
begin
    m_JvRich.Destroy ;
    inherited;
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

procedure Tfrm_ViewLOG.hideLOG;
begin
    Self.Close ;
end;

class function Tfrm_ViewLOG.New(const aTitle: string;
  const aHeight, aWidth: Integer): IViewLOG;
begin
    Result :=Tfrm_ViewLOG.CreateNew(aTitle, aHeight, aWidth);

end;

procedure Tfrm_ViewLOG.showLOG(const aModal: Boolean);
begin
    if aModal then
        Self.ShowModal
    else
        Self.show;
end;

end.
