unit uravecodbase;

interface

uses Windows    ,
    Messages    ,
    SysUtils    ,
    Classes     ,
    Graphics		,
    Printers		,
    Controls    ,
    Generics.Collections

    ,RpBase
    ,RpDefine
    ,RpDevice
    ,RpFiler
    ,RpSystem
    ,RpRender
    ,RpRenderText
    ,RpRenderPDF
    ,RpRenderRTF
    ,rpBars

    ;


type
  ERptError = class(Exception);

  TRptOrientation = type TOrientation;
  TRptDestination = type TReportDest;
  TRptRenderOutput = (roPreview, roPrinter, roPDF, roRTF, roText, roNone) ;

  TRptPrintBand = (pbPageH, pbPageF, pbColH, pbGroupH, pbGroupF);

  TCReportCab = class
  private
  public
  end;

  TCReportBase = class (TCustomControl)
  private
    _ptr: TRvSystem ;
  protected
    _rpt: TBaseReport;
    _printerName,_paperName: string;
    _paperSize: Byte;
    _paperW,_paperH: Double;
    _marginLeft,_marginTop: Double;
    _marginRight,_marginBottom: Double;

    _dataHora: TDateTime;
    _pageNum: Integer;

    property Motor: TRvSystem read _ptr;
    property Report: TBaseReport read _rpt;

    procedure DoInit(Sender: TObject); virtual;
    procedure DoNewPage(Sender: TObject); virtual; abstract ;
    procedure DoPrintHeader(Sender: TObject); virtual;
    procedure DoPrintFooter(Sender: TObject); virtual;
    procedure DoPrint(Sender: TObject); virtual; abstract ;
  protected
    const FONT_ARIAL = 'Arial';
    const FONT_TAHOMA = 'Tahoma';
    const FONT_COURIER_NEW = 'Courier New';
    const FONT_SIZE_6 = 6;
    const FONT_SIZE_8 = 8;
    const FONT_SIZE_10 = 10;
    const FONT_SIZE_12 = 12;
    const DMPAPER_A4   = 9;      { A4 210 x 297 mm                     }
    const TAB_COLS_PAGEH =1 ;
    const TAB_COLS_GROUP =2 ;
    const TAB_COLS_ITEMS =3 ;
    procedure PrintTab(const AIndex: Integer;
                       const AText: string;
                       const AJust: TPrintJustify = pjLeft;
                       const ABoxLine: Byte = 0);

//    procedure PrintTabFlt(const AValue: Extended);
//    procedure PrintTabDat(const AValue: TDateTime; const AIncTime = False);

    procedure PrintText(const AXpos, AYpos: Double;
                        const AText: string;
                        const AJust: TPrintJustify = pjLeft );

    procedure PrintEANBarCode(const AXpos, AYpos: Double;
          const AValue: string) ;

    procedure DrawLine( const X1, Y1, X2, Y2: Double;
                        const Color: TColor ;
                        const Style: TPenStyle = psSolid;
                        const Width: Word = 1;
                        const Mode: TPenMode = pmCopy);
    procedure NewLine(const ALines: Integer; const ABold: Boolean;
      const ARestoreTabIndex: Integer);

  public
    property DataHora: TDateTime read _dataHora write _dataHora;
    property PrinterName: string read _printername write _printername ;
    property PaperName: string read _papername write _papername ;
    property PaperW: Double read _paperW;
    property PaperH: Double read _paperH;
    property MarginLeft: Double read _marginLeft write _marginLeft;
    property MarginTop: Double read _marginTop write _marginTop;
    property MarginRight: Double read _marginRight write _marginRight;
    property MarginBottom: Double read _marginBottom write _marginBottom;
  public
    constructor Create(const AOrientation: TOrientation);
    destructor Destroy; override ;
    procedure SetPaperSize(const ASize: Byte = DMPAPER_A4;
      const AWidth: Double =0; const AHeight: Double =0);
    procedure DoExec(const AReportDest: TReportDest = rdPreview;
      const ADestFileName: string = '') ;
    function getClientWidth: Double ;
  end;

  //base para os rel.
  TCReportTabColumn = class
  private
  public
    xPos: double;
    pJustify: TPrintJustify;
    xWidth: double;
    xMargin: double;
    Lines: byte;
    Shade: byte
  end;

  TCReportTabColumns = class(TList<TCReportTabColumn>)
  private
  public
//    function AddNew(): TCReportTabColumn ;
  end;

  TCReportPage = class (TCReportBase)
  private
  protected
    _tabs: TDictionary<Integer, TCReportTabColumns>;
    _unitName: string ;
    _title,_subtitle: string ;
    procedure DoInit(Sender: TObject); override;
    procedure DoPrintHeader(Sender: TObject); override;
    procedure DoPrintFooter(Sender: TObject); override;
  public
    function ValidPage(const Ypos: Double;
      const LinesLeft: Byte = 0): Boolean;
  end;


implementation

uses Forms, StrUtils , jpeg, Math ,
  uadodb;


{ TCReportBase }

constructor TCReportBase.Create(const AOrientation: TOrientation);
begin
    _ptr :=TRvSystem.Create(Self) ;

    _ptr.SystemOptions:=_ptr.SystemOptions -[soAllowSaveFromPreview];
    _ptr.SystemPrinter.Orientation :=AOrientation;
    _ptr.SystemPrinter.Units :=unCM ;
    _ptr.SystemPrinter.UnitsFactor :=2.54 ;
    _ptr.SystemPrinter.MarginTop :=1;
    _ptr.SystemPrinter.MarginLeft :=1;
    _ptr.SystemPrinter.MarginRight :=1;
    _ptr.SystemPrinter.MarginBottom :=1;
    _ptr.SystemSetups :=_ptr.SystemSetups -[ssAllowSetup,
                                            ssAllowPrinterSetup,
                                            ssAllowPreviewSetup];

    _ptr.OnBeforePrint:=DoInit ;
    _ptr.OnPrintHeader:=DoPrintHeader ;
    _ptr.OnPrintFooter:=DoPrintFooter ;
    _ptr.OnPrint      :=DoPrint;

    _pageNum :=1;
    _dataHora:=Now ;
end;

destructor TCReportBase.Destroy;
begin
    _ptr.Destroy ;
    inherited;
end;

procedure TCReportBase.DoExec(const AReportDest: TReportDest;
  const ADestFileName: string);
var
  pdf: TRvRenderPDF;
begin
    pdf :=nil;

    _ptr.DefaultDest :=AReportDest;
    _ptr.SystemFiler.StatusFormat :='Gerando pagina %d ...';

    case AReportDest of
        rdFile:
        begin
            pdf :=TRvRenderPDF.Create(nil);
            pdf.EmbedFonts  :=True;
//            pdf.ImageQuality:=90;
//            pdf.MetafileDPI:=300;
//            pdf.UseCompression:=False;
//            pdf.DocInfo.Creator :='';
//            pdf.DocInfo.Producer:='';

            _ptr.DoNativeOutput :=False;
            _ptr.RenderObject   :=pdf;
            _ptr.OutputFileName :=ADestFileName;

        end;

        rdPreview:
        begin
            _ptr.SystemPreview.FormState :=wsMaximized ;
            _ptr.SystemPreview.ZoomFactor:=125;
            case _ptr.SystemPrinter.Units of
                unInch: _ptr.SystemPreview.RulerType    :=rtBothIn;
                unMM,unCM: _ptr.SystemPreview.RulerType :=rtBothCm;
            else
                _ptr.SystemPreview.RulerType :=rtNone;
            end;
        end;

        rdPrinter:
        begin
            RPDev.SelectPrinter(PrinterName, False);
            RPDev.SelectPaper(PaperName, False);
            //Papel
            if _paperSize > 0 then
                _rpt.SetPaperSize(_paperSize, 0, 0)
            else
                _rpt.SetPaperSize(0, _paperW, _paperH);
        end;
    end;

    _ptr.Execute ;

    if Assigned(pdf) then
    begin
        pdf.Free ;
    end;
end;

procedure TCReportBase.DoInit(Sender: TObject);
begin
    //
    // base de desenho do rel
    //
    _rpt :=(Sender as TBaseReport);

    //
    // fixa unidade em cm
    //
    _rpt.Units :=unCM;
    _rpt.UnitsFactor :=2.54 ;

    //
    // select printer/papel
    //
    //_rpt.SelectPrinter(_printerName) ;
    //_rpt.SelectPaper(_paperName) ;

    //
    // Config. papel custom
    //
    if(_paperW > 0)and(_paperH > 0)then
    begin
        _rpt.SetPaperSize(0, _paperW, _paperH);
    end;

    //Margens
    _rpt.MarginLeft   :=IfThen(_marginLeft =0, 1, _marginLeft);
    _rpt.MarginTop    :=IfThen(_marginTop =0, 1, _marginTop);
    _rpt.MarginRight  :=IfThen(_marginRight =0, _rpt.MarginLeft, _marginRight);
    _rpt.MarginBottom :=IfThen(_marginBottom =0, _rpt.MarginTop, _marginBottom);
    _rpt.ResetSection ;

    _rpt.LineHeightMethod :=lhmLinesPerInch;

    //start page
    _pageNum :=1;
end;

procedure TCReportBase.DoPrintFooter(Sender: TObject);
begin

end;

procedure TCReportBase.DoPrintHeader(Sender: TObject);
begin

end;

procedure TCReportBase.DrawLine(const X1, Y1, X2, Y2: Double;
  const Color: TColor; const Style: TPenStyle; const Width: Word;
  const Mode: TPenMode);
var
  SavePen: TPen;
  NewPen: TPen;
begin
    SavePen :=TPen.Create;
    NewPen  :=Report.CreatePen(Color, Style, Width, pmCopy);
    try

        SavePen.Assign(Canvas.Pen);
        Canvas.Pen.Assign(NewPen);

        //Draw line
        Report.MoveTo(X1, Y1);
        Report.LineTo(X2, Y2);

    finally
        NewPen.Free;
        Canvas.Pen.Assign(SavePen);
        SavePen.Free;
    end;
end;

function TCReportBase.getClientWidth: Double;
begin
    Result :=0;
    if Assigned(Report) then
    begin
        Result :=Report.SectionRight -Report.SectionLeft ;
    end;
end;

procedure TCReportBase.NewLine(const ALines: Integer; const ABold: Boolean;
  const ARestoreTabIndex: Integer);
var
  I,L: Integer ;
begin
    if ALines < 1 then
        L :=1
    else
        L :=ALines;
    for I :=1 to L do
    begin
        Report.NewLine ;
    end;
    Report.Bold :=ABold ;
    Report.RestoreTabs(ARestoreTabIndex) ;
end;

procedure TCReportBase.PrintEANBarCode(const AXpos, AYpos: Double;
  const AValue: string);
var
  bar: TRPBarsEAN ;
begin
    bar :=TRPBarsEAN.Create(_rpt) ;
    try
        bar.BarHeight :=0.50;
        bar.Text :=AValue ;
        bar.PrintXY(AXpos, AYpos);
    finally
        bar.Free ;
    end;
end;

procedure TCReportBase.PrintTab(const AIndex: Integer; const AText: string;
  const AJust: TPrintJustify; const ABoxLine: Byte);
var tab:TTab;
begin
    if not Assigned(Report) then
    begin
        Exit;
    end;

    tab :=Report.GetTab(AIndex);

    if tab = nil then
    begin
        Exit ;
    end;

    tab.Justify :=AJust ;

    case ABoxLine of
        BOXLINELEFT:
        begin
          tab.Left 		:=True;
          tab.Right 	:=False;
          tab.Top 		:=False;
          tab.Bottom	:=False;
        end;
        BOXLINERIGHT:
        begin
          tab.Left 		:=False;
          tab.Right 	:=True;
          tab.Top 		:=False;
          tab.Bottom	:=False;
        end;
        BOXLINELEFTRIGHT:
        begin
          tab.Left 		:=True;
          tab.Right 	:=True;
          tab.Top 		:=False;
          tab.Bottom	:=False;
        end;
        BOXLINETOP:
        begin
          tab.Left 		:=False;
          tab.Right 	:=False;
          tab.Top 		:=True;
          tab.Bottom	:=False;
        end;
        BOXLINELEFTTOP:
        begin
          tab.Left 		:=True;
          tab.Right 	:=False;
          tab.Top 		:=True;
          tab.Bottom	:=False;
        end;
        BOXLINERIGHTTOP:
        begin
          tab.Left 		:=False;
          tab.Right 	:=True;
          tab.Top 		:=True;
          tab.Bottom	:=False;
        end;
        BOXLINENOBOTTOM:
        begin
          tab.Left 		:=True;
          tab.Right 	:=True;
          tab.Top 		:=True;
          tab.Bottom	:=False;
        end;
        BOXLINEBOTTOM:
        begin
          tab.Left 		:=False;
          tab.Right 	:=False;
          tab.Top 		:=False;
          tab.Bottom	:=True;
        end;
        BOXLINELEFTBOTTOM:
        begin
          tab.Left 		:=True;
          tab.Right 	:=False;
          tab.Top 		:=False;
          tab.Bottom	:=True;
        end;
        BOXLINERIGHTBOTTOM:
        begin
          tab.Left 		:=False;
          tab.Right 	:=True;
          tab.Top 		:=False;
          tab.Bottom	:=True;
        end;
        BOXLINENOTOP:
        begin
          tab.Left 		:=True;
          tab.Right 	:=True;
          tab.Top 		:=False;
          tab.Bottom	:=True;
        end;
        BOXLINETOPBOTTOM:
        begin
          tab.Left 		:=False;
          tab.Right 	:=False;
          tab.Top 		:=True;
          tab.Bottom	:=True;
        end;
        BOXLINENORIGHT:
        begin
          tab.Left 		:=True;
          tab.Right 	:=False;
          tab.Top 		:=True;
          tab.Bottom	:=True;
        end;
        BOXLINENOLEFT:
        begin
          tab.Left 		:=False;
          tab.Right 	:=True;
          tab.Top 		:=True;
          tab.Bottom	:=True;
        end;
        BOXLINEALL:
        begin
          tab.Left 		:=True;
          tab.Right 	:=True;
          tab.Top 		:=True;
          tab.Bottom	:=True;
        end;
    else
        tab.Left 		:=False;
        tab.Right 	:=False;
        tab.Top 		:=False;
        tab.Bottom	:=False;
    end;
    Report.PrintTab(AText);
end;

procedure TCReportBase.PrintText(const AXpos, AYpos: Double;
  const AText: string; const AJust: TPrintJustify);
var
  Text: string ; //TStrings; gerencia depois com mais de uma linha
  Cmd: string ;
  Styles: TFontStyles;
  Color: TColor ;
  P: Integer;

begin

    if not Assigned(Report) then
    begin
        Exit;
    end;

    Text :=AText ; //TStringList.Create ;
    Styles :=[];
    Color :=clNone;

    //comandos
    P :=Pos(#10, AText);
    if P > 0 then
    begin
        Text :=Copy(AText, 1, P-1);
        Cmd :=LowerCase(Copy(AText, P+1, Length(AText)));

        //estilos
        if Pos('bold', Cmd) > 0 then Styles :=[fsBold];
        if Pos('italic', Cmd) > 0 then Styles :=Styles +[fsItalic];
        if Pos('underline', Cmd) > 0 then Styles :=Styles +[fsUnderline];
        if Pos('StrikeOut', Cmd) > 0 then Styles :=Styles +[fsStrikeOut];

        //color
        P :=Pos('color=', Cmd);
        if P > 0 then
        begin
            Cmd :=Copy(Cmd, P+1, Length(Cmd));
            Color :=StringToColor(Cmd) ;
        end;
    end;
//    else
//        Text.Add(AText);

    if fsBold       in Styles then Report.Bold     :=True;
    if fsItalic     in Styles then Report.Italic   :=True;
    if fsUnderline  in Styles then Report.Underline:=True;
    if fsStrikeOut  in Styles then Report.Strikeout:=True;

    //Report.FontColor

    case AJust of
        pjLeft: Report.PrintLeft(Text, AXpos);
        pjRight: Report.PrintRight(Text, AXpos);
        pjCenter: Report.PrintCenter(Text, AXpos);
        //pjCenter: Report.PrintBlock(Text, AXpos, );
    end;

    if fsBold       in Styles then Report.Bold     :=False;
    if fsItalic     in Styles then Report.Italic   :=False;
    if fsUnderline  in Styles then Report.Underline:=False;
    if fsStrikeOut  in Styles then Report.Strikeout:=False;

end;

procedure TCReportBase.SetPaperSize(const ASize: Byte; const AWidth,
  AHeight: Double);
begin
    _paperSize:=ASize ;
    _paperW   :=AWidth;
    _paperH   :=AHeight;
end;

{ TCReportPage<L> }

procedure TCReportPage.DoInit(Sender: TObject);
begin
    inherited;
    _ptr.TitlePreview :=_title ;

end;

procedure TCReportPage.DoPrintFooter(Sender: TObject);
begin
    Report.GotoFooter ;
    Report.MoveTo(Report.MarginLeft, Report.Ypos);
    Report.LineTo(Report.SectionRight, Report.Ypos);
    Report.GotoXY(Report.MarginLeft, Report.Ypos +Report.FontHeight);
    Report.SetFont(FONT_TAHOMA, FONT_SIZE_8); Report.Italic :=True ;
    Report.PrintLeft('Atac Sistemas / atacsistemas.com', Report.MarginLeft);
    Report.PrintRight(_unitName, Report.SectionRight);
    Report.Italic :=False ;
end;

procedure TCReportPage.DoPrintHeader(Sender: TObject);
var
  X: Double ;
  fmtDt, fmtHr, fmtPg: string;
begin
    fmtDt :=FormatDateTime('"Data: "dd/mm/yyy', DataHora, LocalFormatSettings) ;
    fmtHr :=FormatDateTime('"Hora: "hh:nn', DataHora, LocalFormatSettings) ;
    fmtPg :=Format('Página: %s/%s',[Report.Macro(midCurrentPage),
                                    Report.Macro(midTotalPages)]);

    X :=Report.SectionRight -Report.TextWidth(fmtDt) ;

    Report.LinesPerInch :=5;

    Report.SetFont(FONT_TAHOMA, FONT_SIZE_10); Report.AdjustLine ;
    Report.Bold :=True;
    Report.PrintLeft(Empresa.xFant, Report.MarginLeft);
    Report.Bold :=False;

    Report.SetFont(FONT_TAHOMA, FONT_SIZE_8); Report.AdjustLine ;
    Report.NewLine(); Report.PrintLeft(Empresa.xEnder, Report.MarginLeft);
                      Report.PrintLeft(fmtDt, X);
    Report.NewLine(); Report.PrintLeft('SisGerCom- Sistema Automação Comercial', Report.MarginLeft);
                      Report.PrintLeft(fmtHr, X);
    Report.NewLine(); Report.Bold :=True;
                      Report.PrintLeft(_title, Report.MarginLeft);
                      Report.Bold :=False;
                      Report.PrintLeft(fmtPg, X);
    if _subtitle <> '' then
    begin
        Report.NewLine();
        Report.PrintLeft(_subtitle, Report.MarginLeft);
    end;
    Report.LinesPerInch :=6;
    Report.GotoXY(Report.MarginLeft, Report.Ypos +Report.LineHeight /2);
    Report.MoveTo(Report.MarginLeft, Report.Ypos );
    Report.LineTo(Report.SectionRight, Report.Ypos);
end;

function TCReportPage.ValidPage(const Ypos: Double;
  const LinesLeft: Byte): Boolean;
begin
    if LinesLeft > 0 then
        Result :=Report.LinesLeft <= LinesLeft
    else
        Result :=(Report.YPos +Report.LineHeight) >= Ypos;
    if Result then
    begin
        Report.NewPage ;
    end;
end;

end.
