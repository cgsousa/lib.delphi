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

    //function dtoS(const aDate: TDateTime): string ;
    function dhToS(const aDat: TDateTime): string;

  public
    //
    // formatings
    function fInt(const aVal: Extended): string; overload ;
    function fInt(const aVal: Extended;
      const aCaption: string; const aComp: Word): string; overload ;

    function fCur(const aVal: Currency): string; overload ;
    function fCur(const aVal: Currency;
      const aCaption: string; const aComp: Word): string; overload ;

    function fFlt(const aVal: Extended;
      const aPrecision: Word =15; const aDigits: Word=2): string; overload ;
    function fFlt(const aVal: Extended;
      const aCaption: string; const aComp: Word): string; overload ;

    function fDt(const aDt: TDateTime): string;
    function fDtTm(const aDtTm: TDateTime; const aFormat: string =''): string;

    function fTm(const aTm: TDateTime): string; overload ;
    function fTm(const aTm: TDateTime;
      const aCaption: string; const aComp: Word): string; overload ;

    function fSQL(const aStr: string): string ;
    function fCNPJ(const aStr: string): string ;
    function fCPF(const aStr: string): string ;
    function fMsk(const aStr, aEditMask: string): string ;

    //
    // Frmt Int
    function Frmt(const aValue: Int64): string; overload ;
    function Frmt(const aValue: Int64;
      const aCaption: string; const aComp: Word): string; overload ;

    //
    // Frmt Currency
    function Frmt(const aValue: Currency): string; overload ;
    function Frmt(const aValue: Currency;
      const aCaption: string; const aComp: Word): string; overload ;

    //
    // Frmt Float
    function Frmt(const aValue: Extended;
      const aPrecision: Word =15; const aDigits: Word=2): string; overload ;
    function Frmt(const aValue: Extended;
      const aCaption: string; const aComp: Word): string; overload ;

    //
    // Frmt TDateTime
    function Frmt(const aValue: TDateTime): string; overload ;
    function Frmt(const aValue: TDateTime;
      const aCaption: string; const aComp: Word): string; overload ;

    //
    //
    function getNumber(const aStr: string): string;
    function isNumber(const aStr: string): Boolean;
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
    m_FormatSettings :=aFormatSettings ;

end;

function UtilStr.dhToS(const aDat: TDateTime): string;
begin
    Result :=FormatDateTime('dd/mm/yyyy hh:nn', aDat) ;

end;

function UtilStr.Dupe(const aCount: Integer; const aStr: string): string;
begin
    Result :=DupeString(aStr, aCount) ;

end;

function UtilStr.fCNPJ(const aStr: string): string;
begin
    Result :=FormatMaskText('00\.000\.000\/0000\-00;0; ', aStr);

end;

function UtilStr.fCPF(const aStr: string): string;
begin
    Result :=FormatMaskText('00\.000\.000\/0000\-00;0; ', aStr);

end;

function UtilStr.fCur(const aVal: Currency): string ;
begin
    //
    //Result :=Trim(Format('%15.2m',[aVal]))
    Result :=FloatToStrF(aVal, ffCurrency, 15, 2, FormatSettings);
end;

function UtilStr.fCur(const aVal: Currency;
  const aCaption: string; const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.fCur(aVal);
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.fDt(const aDt: TDateTime): string;
begin
    Result :=FormatDateTime('DD/MM/YYYY', aDt, FormatSettings);
end;

function UtilStr.fDtTm(const aDtTm: TDateTime; const aFormat: string): string;
begin
    if aFormat = '' then
        Result :='DD/MM/YYYY hh:nn:ss';
    Result :=FormatDateTime(aFormat, aDtTm, FormatSettings);
end;

function UtilStr.fFlt(const aVal: Extended;
  const aPrecision, aDigits: Word): string;
begin
    Result :=FloatToStrF(aVal, ffNumber, aPrecision, aDigits, FormatSettings);

end;

function UtilStr.fFlt(const aVal: Extended;
  const aCaption: string; const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.fFlt(aVal) ;
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.fInt(const aVal: Extended): string ;
begin
    if aVal > 999 then
        Result :=FormatFloat('#,##0', aVal, FormatSettings)
    else
        Result :=IntToStr(Trunc(aVal));
end;

function UtilStr.fInt(const aVal: Extended; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.fInt(aVal) ;
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.fMsk(const aStr, aEditMask: string): string;
begin
    Result :=FormatMaskText(aEditMask, aStr);

end;

function UtilStr.Frmt(const aValue: Currency): string;
begin
    Result :=FloatToStrF(aValue, ffCurrency, 15, 2, FormatSettings);
end;

function UtilStr.Frmt(const aValue: Currency; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.Frmt(aValue);
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.Frmt(const aValue: Int64): string;
begin
    if aValue > 999 then
        Result :=FloatToStrF(aValue, ffNumber, 15, 0, FormatSettings)
    else
        Result :=IntToStr(aValue);
end;

function UtilStr.Frmt(const aValue: Int64; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.Frmt(aValue) ;
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.fSQL(const aStr: string): string;
begin
    if Trim(aStr) <> '' then
        Result :=QuotedStr(aStr)
    else
        Result :='NULL';
end;

function UtilStr.fTm(const aTm: TDateTime;
  const aCaption: string; const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.fTm(aTm) ;
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.fTm(const aTm: TDateTime): string;
begin
    //
    //
    DateTimeToString(Result, 'hh:nn:ss', aTm, FormatSettings);
end;

function UtilStr.getFormatSettings: TFormatSettings;
begin
    if(m_FormatSettings.ThousandSeparator =#0)or
      (m_FormatSettings.DecimalSeparator =#0)or
      (m_FormatSettings.DateSeparator =#0)then
    begin
        GetLocaleFormatSettings(0, m_FormatSettings);
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

function UtilStr.Frmt(const aValue: Extended; const aPrecision,
  aDigits: Word): string;
begin
    Result :=FloatToStrF(aValue, ffNumber, aPrecision, aDigits, FormatSettings);
end;

function UtilStr.Frmt(const aValue: Extended; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.Frmt(aValue) ;
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

function UtilStr.Frmt(const aValue: TDateTime): string;
var
  F: string;
begin
    //
    // format default
    F :='';
    //
    // Date
    if Trunc(aValue) > 0 then
    begin
        //
        // com time
        if Frac(aValue) > 0 then
            F :='dd/mm/yyyy hh:nn:ss'
        //
        // sem time
        else
            F :='dd/mm/yyyy';
    end
    //
    // time
    else
        F :='dd/mm/yyyy hh:nn:ss';
    //
    // result datetime format
    DateTimeToString(Result, F, aValue, FormatSettings);
end;

function UtilStr.Frmt(const aValue: TDateTime; const aCaption: string;
  const aComp: Word): string;
var
  L: Word;
begin
    Result :=Self.Frmt(aValue) ;
    L :=Self.Len(aCaption) + Self.Len(Result) ;
    if aComp > L then
        Result :=aCaption +Self.Dupe(aComp -L) +Result
    else
        Result :=aCaption +Result;
end;

end.
