{***
* Utilidade para manipulação de strings Delphi/FPC
* Todos os direitos reservados
* Autor: Carlos Gonzaga
* Data: 20.05.2007
*}
unit ustr;

{*
******************************************************************************
|* PROPÓSITO: Registro de Alterações
******************************************************************************

Símbolo : Significado

[+]     : Novo recurso
[*]     : Recurso modificado/melhorado
[-]     : Correção de Bug (assim esperamos)

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
    //m_FormatSettings: TFormatSettings ;
    //
    // Retorna quantas ocorrencias de <aSubStr> existem em <aStr>
    function Count(const aStr, aSubStr: String): Integer ;
  public
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
    // Completa <aStr> com <aChar> a direita, até o tamanho <aLen>, alinhando
    // <aStr> a esquerda. Se <aStr> for maior que <aLen>, ela será truncada
    function padR(const aStr: String; const aLen : Integer;
      const aChar: Char) : String ;
    //
    // Completa <aStr> com <aChar> a esquerda, até o tamanho <aLen>, alinhando
    // <aStr> a direita. Se <aStr> for maior que <aLen>, ela será truncada
    function padL(const aStr: String; const aLen : Integer;
      const aChar: Char) : String ;
    //
    // Centraliza <aStr>, preenchendo com <aChar> a esquerda e direita
    function padC(const aStr: String; const aLen : Integer;
      const aChar: Char) : String ;
    // Ajusta a <aStr> com o tamanho de <aLen> inserindo espaços no meio,
    // substituindo <aSep> por n X <aChar>  (Justificado)
    function padS(const aStr, aSep: String; const aLen: Integer;
      const aChar: Char =#32; const aRemove: Boolean = True) : String ;
    //
    // Insere ZEROS (0) a esquerda de <aInt/aStr> até completar <aLen>
    function Zeros(const aInt: Int64 ; const aLen: Integer): string; overload;
    function Zeros(const aStr: String; const aLen: Integer): string; overload;

    //function dtoS(const aDate: TDateTime): string ;
    function dhToS(const aDat: TDateTime): string;

  public
    //
    // formatings
    //procedure setFormat(const aFormat: TFormatSettings) ;
    function fInt(const aVal: Int64): string ;
    function fCur(const aVal: Currency; const aIncSimbol: Boolean =false): string ;
    function fFlt(const aVal: Extended): string ;
    function fDt(const aDt: TDateTime): string;
    function fDtTm(const aDtTm: TDateTime; const aFormat: string =''): string;
    function fTm(const aTm: TDateTime): string;
    function fSQL(const aStr: string): string ;

    //
    // Retorna <aCount> copias de <aStr>
    function Dupe(const aCount: Integer; const aStr: string =' '): string;

    //
    //
    function getNumber(const aStr: string): string;
    function isNumber(const aStr: string): Boolean;
  end;

{$ENDREGION}


implementation

uses StrUtils;

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

function UtilStr.dhToS(const aDat: TDateTime): string;
begin
    Result :=FormatDateTime('dd/mm/yyyy hh:nn', aDat) ;

end;

function UtilStr.Dupe(const aCount: Integer; const aStr: string): string;
begin
    Result :=DupeString(aStr, aCount) ;

end;

function UtilStr.fCur(const aVal: Currency; const aIncSimbol: Boolean): string ;
begin
    if aIncSimbol then
        Result :=Trim(Format('%15.2m',[aVal]))
    else
        Result :=Self.fFlt(aVal) ;
end;

function UtilStr.fDt(const aDt: TDateTime): string;
begin
    Result :=FormatDateTime('DD/MM/YYYY', aDt);
end;

function UtilStr.fDtTm(const aDtTm: TDateTime; const aFormat: string): string;
begin
    if aFormat = '' then
        Result :='DD/MM/YYYY hh:nn:ss';
    Result :=FormatDateTime(aFormat, aDtTm);
end;

function UtilStr.fFlt(const aVal: Extended): string;
begin
    Result :=Trim(Format('%15.2n',[aVal]));

end;

function UtilStr.fInt(const aVal: Int64): string ;
begin
    if aVal > 999 then
        //Result :=FloatToStrF(
        Result :=FormatFloat('#,##0', aVal)
    else
        Result :=IntToStr(aVal);
end;

function UtilStr.fSQL(const aStr: string): string;
begin
    if Trim(aStr) <> '' then
        Result :=QuotedStr(aStr)
    else
        Result :='NULL';
end;

function UtilStr.fTm(const aTm: TDateTime): string;
begin
    Result :=FormatDateTime('hh:nn:ss', aTm) ;

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


end.
