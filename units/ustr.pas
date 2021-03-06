{***
* Utilidade para manipula��o de strings Delphi/FPC
* Todos os direitos reservados
* Autor: Carlos Gonzaga
* Data: 20.05.2007
*}
unit ustr;

{*
******************************************************************************
|* PROP�SITO: Registro de Altera��es
******************************************************************************

S�mbolo : Significado

[+]     : Novo recurso
[*]     : Recurso modificado/melhorado
[-]     : Corre��o de Bug (assim esperamos)

*}

interface

uses SysUtils;


{$REGION 'UtilStr'}
type
  //
  // object strings utils
  //
  UtilStr = object
  strict private
    m_FormatSettings: TFormatSettings ;
    function getFormatSettings: TFormatSettings ;
    //
    // Retorna quantas ocorrencias de <aSubStr> existem em <aStr>
    function Count(const aStr, aSubStr: String): Integer ;
  public
    property FormatSettings: TFormatSettings read getFormatSettings;
    constructor Create(const aFormatSettings: TFormatSettings);

    //
    // Retorna <aCount> copias de <aStr>
    function Dupe(const aCount: Integer; const aStr: string =' '): string;

    //
    // Obtem o tamanho nativo de um string (delphi/fpc)
    function Len(const aStr: String): Integer;
    //
    // chama a PosEx da StsUtils.pas
    function Pos(const aSubStr, aStr: string;
      const aOffset: Integer = 1): Integer;

    //
    // insert, update e delete string
    function Stuff(const aStr, aSubStr: string;
      const aStart, aLen: Cardinal): string;

    //
    // Completa <aStr> com <aChar> a direita, at� o tamanho <aLen>, alinhando
    // <aStr> a esquerda. Se <aStr> for maior que <aLen>, ela ser� truncada
    function padR(const aStr: String; const aLen : Integer;
      const aChar: Char) : String ;

    //
    // Completa <aStr> com <aChar> a esquerda, at� o tamanho <aLen>, alinhando
    // <aStr> a direita. Se <aStr> for maior que <aLen>, ela ser� truncada
    function padL(const aStr: String; const aLen : Integer;
      const aChar: Char) : String ;

    //
    // Centraliza <aStr>, preenchendo com <aChar> a esquerda e direita
    function padC(const aStr: String; const aLen : Integer;
      const aChar: Char) : String ;

    // Ajusta a <aStr> com o tamanho de <aLen> inserindo espa�os no meio,
    // substituindo <aSep> por n X <aChar>  (Justificado)
    function padS(const aStr, aSep: String; const aLen: Integer;
      const aChar: Char =#32; const aRemove: Boolean = True) : String ;

    //
    // Insere ZEROS (0) a esquerda de <aInt/aStr> at� completar <aLen>
    function Zeros(const aInt: Int64 ; const aLen: Integer): string; overload;
    function Zeros(const aStr: String; const aLen: Integer): string; overload;

  public
    //
    // formatings

    function fSQL(const aStr: string): string ;
    function fCNPJ(const aStr: string): string ;
    function fCPF(const aStr: string): string ;
    function fMsk(const aStr, aEditMask: string): string ;

    //
    // Fmt Int
    function FmtInt(const aValue: Extended): string; overload ;
    function FmtInt(const aValue: Extended;
      const aCaption: string; const aComp: Word): string; overload ;

    //
    // Fmt Currency
    function FmtCur(const aValue: Currency): string; overload ;
    function FmtCur(const aValue: Currency;
      const aCaption: string; const aComp: Word): string; overload ;

    //
    // Fmt Float
    function FmtFlt(const aValue: Extended;
      const aPrecision: Word =15; const aDigits: Word=2): string; overload ;
    function FmtFlt(const aValue: Extended;
      const aCaption: string; const aComp: Word): string; overload ;

    //
    // Fmt TDateTime
    function FmtDtTm(const aValue: TDateTime; const aFormat: string =''): string; overload ;
    function FmtDtTm(const aValue: TDateTime;
      const aCaption: string; const aComp: Word): string; overload ;

    //
    // format geral
    function Fmt(const aFormat: string; const Args: array of const): string;

    procedure setThousandSeparator(const aSep: Char);
    procedure setDecimalSeparator(const aSep: Char) ;
  public
    function getNumber(const aStr: string): string;
    function isNumber(const aStr: string): Boolean;
    function Extract(var aStr: string; const aChr: Char =';'): string ;
    //function split(var aStr: string; const aChr: Char =';'): string ;
  end;

{$ENDREGION}


var
  StrU: UtilStr;


implementation

uses MaskUtils, StrUtils;

function CharIsNumber(const C: Char): Boolean;
begin
    Result := CharInSet( C, ['0'..'9'] ) ;
end ;


{ UtilStr }

function UtilStr.Count(const aStr, aSubStr: String): Integer;
var
  ini : Integer ;
begin
    Result := 0 ;
    if aSubStr = '' then Exit ;

    ini := Pos(aSubStr, aStr) ;
    while ini > 0 do
    begin
        Result :=Result + 1 ;
        ini    :=PosEx(aSubStr, aStr, ini + 1 ) ;
    end ;
end;

constructor UtilStr.Create(const aFormatSettings: TFormatSettings);
begin
    m_FormatSettings.CurrencyFormat :=aFormatSettings.CurrencyFormat ;
    m_FormatSettings.NegCurrFormat :=aFormatSettings.NegCurrFormat ;
    m_FormatSettings.ThousandSeparator :=aFormatSettings.ThousandSeparator ;
    m_FormatSettings.DecimalSeparator :=aFormatSettings.DecimalSeparator ;
    m_FormatSettings.CurrencyDecimals :=aFormatSettings.CurrencyDecimals ;
    m_FormatSettings.DateSeparator :=aFormatSettings.DateSeparator ;
    m_FormatSettings.TimeSeparator :=aFormatSettings.TimeSeparator ;
    m_FormatSettings.ListSeparator :=aFormatSettings.ListSeparator ;
    m_FormatSettings.CurrencyString :=aFormatSettings.CurrencyString ;
    m_FormatSettings.ShortDateFormat :=aFormatSettings.ShortDateFormat ;
    m_FormatSettings.LongDateFormat :=aFormatSettings.LongDateFormat ;
    m_FormatSettings.TimeAMString :=aFormatSettings.TimeAMString ;
    m_FormatSettings.TimePMString :=aFormatSettings.TimePMString ;
    m_FormatSettings.ShortTimeFormat :=aFormatSettings.ShortTimeFormat ;
    m_FormatSettings.LongTimeFormat :=aFormatSettings.LongTimeFormat ;
    m_FormatSettings.ShortMonthNames :=aFormatSettings.ShortMonthNames ;
    m_FormatSettings.LongMonthNames :=aFormatSettings.LongMonthNames ;
    m_FormatSettings.ShortDayNames :=aFormatSettings.ShortDayNames ;
    m_FormatSettings.LongDayNames :=aFormatSettings.LongDayNames ;
    m_FormatSettings.TwoDigitYearCenturyWindow :=aFormatSettings.TwoDigitYearCenturyWindow ;
end;

function UtilStr.Dupe(const aCount: Integer; const aStr: string): string;
begin
    Result :=DupeString(aStr, aCount) ;

end;

function UtilStr.Extract(var aStr: string; const aChr: Char): string;
var
  P: Integer;
begin
    P :=Pos(aChr, aStr) ;
    if P > 0 then
    begin
        Result :=Copy(aStr, 1, P-1);
        aStr :=Copy(aStr, P+1, Length(aStr));
    end
    else begin
        Result :=aStr ;
        aStr :='';
    end;
end;

function UtilStr.fCNPJ(const aStr: string): string;
begin
    Result :=FormatMaskText('00\.000\.000\/0000\-00;0; ', aStr);

end;

function UtilStr.fCPF(const aStr: string): string;
begin
    Result :=FormatMaskText('00\.000\.000\/0000\-00;0; ', aStr);

end;

function UtilStr.fMsk(const aStr, aEditMask: string): string;
begin
    Result :=FormatMaskText(aEditMask, aStr);

end;

function UtilStr.fSQL(const aStr: string): string;
begin
    if Trim(aStr) <> '' then
        Result :=QuotedStr(aStr)
    else
        Result :='NULL';
end;

function UtilStr.getFormatSettings: TFormatSettings;
begin
    if m_FormatSettings.CurrencyFormat =0 then
    begin
    {$IFDEF MSWINDOWS}
        GetLocaleFormatSettings(0, m_FormatSettings)
    {$ELSE}
        //
        // inicializa
        m_FormatSettings :=DefaultFormatSettings ;
    {$ENDIF}
        ;
    end
    else begin
        Result :=m_FormatSettings ;
    end;
end;

function UtilStr.getNumber(const aStr: string): string;
Var
  I : Integer ;
  LenValue : Integer;
begin
  Result   := '' ;
  LenValue :=Self.Len(aStr) ;
  for I := 1 to LenValue do
  begin
     if CharIsNumber(aStr[I]) then
        Result := Result + aStr[I];
  end;
end;

function UtilStr.isNumber(const aStr: string): Boolean;
var
  A, LenStr : Integer ;
begin
  LenStr :=Self.Len(aStr) ;
  Result := (LenStr > 0) ;
  A      := 1 ;
  while Result and ( A <= LenStr )  do
  begin
     Result :=CharIsNumber(aStr[A]) ;
     Inc(A) ;
  end;
end;

function UtilStr.Len(const aStr: String): Integer;
begin
     Result :={$IfDef FPC} UTF8Length(aStr)
              {$Else}      Length(aStr)
              {$EndIf}
              ;
end;

function UtilStr.padC(const aStr: String; const aLen: Integer;
  const aChar: Char): String;
var
  nLeft: Integer;
  Tam: integer;
begin
    Tam :=Self.Len(aStr);
    if Tam < aLen then
    begin
        nLeft := Trunc( (aLen -Tam) / 2 ) ;
        Result    :=Self.padR( StringOfChar(aChar, nLeft) + aStr, aLen, aChar) ;
    end
    else
        Result :=LeftStr(aStr, aLen);
end;

function UtilStr.padL(const aStr: String; const aLen: Integer;
  const aChar: Char): String;
var
  Tam: Integer;
begin
    Tam :=Self.Len(aStr);
    if Tam < aLen then
        Result :=StringOfChar(aChar, (aLen - Tam)) +aStr
    else
        Result :=LeftStr(aStr, aLen);
end;

function UtilStr.padR(const aStr: String; const aLen: Integer;
  const aChar: Char): String;
var
  Tam: Integer;
begin
    Tam :=Self.Len(aStr);
    if Tam < aLen then
        Result :=aStr + StringOfChar(aChar, (aLen - Tam))
    else
        Result :=LeftStr(aStr, aLen);
end;

function UtilStr.padS(const aStr, aSep: String; const aLen: Integer;
  const aChar: Char; const aRemove: Boolean): String;
var
  StuffStr : String ;
  nSep, nCharSep, nResto, nFeito, Ini : Integer ;
  D: Double ;
  Sep: String;
begin
    Result :=Copy(aStr, 1, aLen) ;
    Sep :=aSep ;
    if Sep = String(aChar) then  { Troca Separador, senao fica em loop infinito }
    begin
        Result :=StringReplace(Result, Sep, #255,[rfReplaceAll]);
        Sep := #255 ;
    end ;

    nSep :=Self.Count(Result, Sep) ;

    if nSep < 1 then
    begin
        Result :=Self.padR(Result, aLen, aChar) ;
        Exit ;
    end ;

    if aRemove then
        Result := Trim(Result) ;

    D        :=(aLen - (Self.Len(Result)-nSep)) / nSep ;
    nCharSep :=Trunc(D) ;
    nResto   :=aLen - ( (Self.Len(Result) -nSep) + (nCharSep *nSep) ) ;
    nFeito   :=nSep ;
    StuffStr :=String( StringOfChar(aChar, nCharSep ) ) ;

    Ini := Pos(Sep, Result) ;
    while Ini > 0 do
    begin
        Result := StuffString(Result,
            Ini,
            length(Sep),
            StuffStr + ifthen(nFeito <= nResto, String(aChar), '' )
          );

        nFeito := nFeito - 1 ;
        Ini    := Pos(Sep, Result) ;
    end ;
end;

function UtilStr.Pos(const aSubStr, aStr: string;
  const aOffset: Integer): Integer;
begin
    Result :=StrUtils.PosEx(aSubStr, aStr, aOffset) ;

end;

procedure UtilStr.setDecimalSeparator(const aSep: Char);
begin
    m_FormatSettings.DecimalSeparator :=aSep;
end;

procedure UtilStr.setThousandSeparator(const aSep: Char);
begin
    m_FormatSettings.ThousandSeparator :=aSep;
end;

function UtilStr.Stuff(const aStr, aSubStr: string; const aStart,
  aLen: Cardinal): string;
begin
    Result :=Copy(aStr, 1, aStart -1) +
            aSubStr +
            Copy(aStr, aStart + aLen, MaxInt);
end;

function UtilStr.Zeros(const aInt: Int64; const aLen: Integer): string;
begin
    Result :=Self.Zeros( IntToStr( aInt ), aLen) ;

end;

function UtilStr.Zeros(const aStr: String; const aLen: Integer): string;
begin
    Result :=Self.padL(Trim(aStr), aLen, '0') ;

end;

function UtilStr.Fmt(const aFormat: string;
  const Args: array of const): string;
begin
    Result :=Format(aFormat, Args, FormatSettings) ;
    //FmtStr(Result, aFormat, Args, FormatSettings) ;
end;


function UtilStr.FmtCur(const aValue: Currency): string;
begin
    Result :=FloatToStrF(aValue, ffCurrency, 15, 2, FormatSettings);
end;

function UtilStr.FmtCur(const aValue: Currency; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.FmtCur(aValue);
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.FmtDtTm(const aValue: TDateTime; const aFormat: string): string;
begin
    //
    // result datetime format
    // Result :=FormatDateTime(F, aValue, FormatSettings);
    // Result :=DateTimeToStr()
    if Trunc(aValue) > 0 then
        DateTimeToString(Result, aFormat, aValue, FormatSettings)
    else
        DateTimeToString(Result, FormatSettings.LongTimeFormat, aValue, FormatSettings) ;
end;

function UtilStr.FmtDtTm(const aValue: TDateTime; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    //
    // format date/time
    Result :=Self.FmtDtTm(aValue) ;
    //
    // adic. o caption
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.FmtFlt(const aValue: Extended; const aPrecision,
  aDigits: Word): string;
begin
    Result :=FloatToStrF( aValue,
                          ffNumber,
                          aPrecision, aDigits,
                          FormatSettings);
end;

function UtilStr.FmtFlt(const aValue: Extended; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.FmtFlt(aValue) ;
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.FmtInt(const aValue: Extended): string;
begin
    if aValue > 999 then
//        Result :=Format(aValue, ffNumber, 15, 0, FormatSettings)
        Result :=FloatToStrF(aValue, ffNumber, 15, 0, FormatSettings)
    else
        Result :=IntToStr(Trunc(aValue));
end;

function UtilStr.FmtInt(const aValue: Extended; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.FmtInt(aValue) ;
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

end.
