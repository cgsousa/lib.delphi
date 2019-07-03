{***
* Extensão para dos componentes JVCL
* Todos os direitos reservados
* Autor: Carlos Gonzaga
* Data: 27.10.2017
*}
unit uJVCL;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  //
  JvExStdCtrls, JvExExtCtrls, JvExtComponent, JvCtrls,
  JvFooter, JvToolEdit, JvGroupBox, JvButton, JvAppInst;


type
  TJvImgBtn =class(JvCtrls.TJvImgBtn)
  private
    m_ShortCut: String ;
  protected
  public
    property ShortCut: String read m_ShortCut write m_ShortCut;
  end;

  TJvFooterBtn =class(JvFooter.TJvFooterBtn)
  private
    m_ShortCut: String ;
    m_ShortCutFont: TFont;
    m_Rect: TRect;
  protected
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawButtonText(TextBounds: TRect; TextEnabled: Boolean); override;
  end;

const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

implementation

uses JvJCLUtils ;


{ TJvFooterBtn }

constructor TJvFooterBtn.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    m_ShortCutFont :=TFont.Create;
    m_ShortCutFont.Name :='Times New Roman';
    m_ShortCutFont.Size :=6;
    m_ShortCutFont.Style:=[fsBold];
end;

destructor TJvFooterBtn.Destroy;
begin
    m_ShortCutFont.Destroy ;
    inherited;
end;

procedure TJvFooterBtn.DrawButtonText(TextBounds: TRect; TextEnabled: Boolean);
var
  Flags: DWORD;
  RealCaption: string;
  fontName: string;
  fontSize: Word;
  fontStyl: TFontStyles;
begin
    Flags := DrawTextBiDiModeFlags(DT_VCENTER or Alignments[Alignment]);
    if WordWrap then
        Flags := Flags or DT_WORDBREAK;

    RealCaption := GetRealCaption;

    fontName :=Canvas.Font.Name;
    fontSize :=Canvas.Font.Size;
    fontStyl :=Canvas.Font.Style;

    Canvas.Brush.Style := bsClear;
    if TextEnabled then
    begin
        JvJCLUtils.DrawText(Canvas, RealCaption, Length(RealCaption), TextBounds, Flags);
        //atalho
        if m_ShortCut <> '' then
        begin
            Canvas.Font.Name :=m_ShortCutFont.Name;
            Canvas.Font.Size :=m_ShortCutFont.Size;
            Canvas.Font.Style :=m_ShortCutFont.Style;
            Canvas.TextOut(m_Rect.Left, m_Rect.Bottom -Canvas.TextHeight(m_ShortCut), m_ShortCut);
        end;
    end
    else begin
        OffsetRect(TextBounds, 1, 1);
        Canvas.Font.Color := clBtnHighlight;
        JvJCLUtils.DrawText(Canvas, RealCaption, Length(RealCaption), TextBounds, Flags);
        OffsetRect(TextBounds, -1, -1);
        Canvas.Font.Color := clBtnShadow;
        JvJCLUtils.DrawText(Canvas, RealCaption, Length(RealCaption), TextBounds, Flags);
        //atalho
        Canvas.Font.Name :=m_ShortCutFont.Name;
        Canvas.Font.Size :=m_ShortCutFont.Size;
        Canvas.Font.Style :=m_ShortCutFont.Style;
        Canvas.TextOut(m_Rect.Left, m_Rect.Bottom -Canvas.TextHeight(m_ShortCut), m_ShortCut);
    end;
    Canvas.Font.Name :=fontName;
    Canvas.Font.Size :=fontSize;
    Canvas.Font.Style :=fontStyl;
end;

procedure TJvFooterBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  R, RectContent, RectText, RectImage, RectArrow: TRect;
begin
  DrawButtonFrame(DrawItemStruct, RectContent);

  //R := ClientRect;
  //InflateRect(R, -4, -4);
  R :=RectContent;
  if not DisableDrawDown and (DrawItemStruct.itemState and ODS_SELECTED <> 0) and Enabled then
  begin
    {$IFDEF JVCLThemesEnabled}
    if StyleServices.Enabled then
      OffsetRect(R, 1, 0)
    else
    {$ENDIF JVCLThemesEnabled}
      OffsetRect(R, 1, 1);
  end;

  CalcButtonParts(R, RectText, RectImage);
  if DropArrow and Assigned(DropDownMenu) then
  begin
    RectArrow := Rect(Width - 16, Height div 2, Width - 9, Height div 2 + 7);
    if (DrawItemStruct.itemState and ODS_SELECTED <> 0) then
      OffsetRect(RectArrow, 1, 1);
    DrawDropArrow(Canvas, RectArrow);
    if (DrawItemStruct.itemState and ODS_SELECTED <> 0) then
      OffsetRect(RectContent, 1, -1)
  end;
  m_Rect :=R;
  DrawButtonText(RectText, Enabled);
  DrawButtonImage(RectImage);
  DrawButtonFocusRect(RectContent);
end;



end.
