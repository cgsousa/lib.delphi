{***
* Utilidades para manipulação de strings Delphi/fpc
* Todos os direitos reservados
* Autor: Carlos Gonzaga
* Data: 20.05.2008
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
    //
    // formatings
    //procedure setFormat(const aFormat: TFormatSettings) ;
    function fInt(const aVal: Extended): string ;
    function fCur(const aVal: Currency; const aIncSimbol: Boolean =false): string ;
    function fFlt(const aVal: Extended): string ;
    function fDat(const aDat: TDateTime; const aFormat: string =''): string;
    function fTime(const aTime: TDateTime): string;
    function fSQL(const aStr: string): string ;

    //
    // Retorna <aCount> copias de <aStr>
    function Dupe(const aCount: Integer; const aStr: string =' '): string;
  end;

{$ENDREGION}


implementation

uses SysUtils, StrUtils;


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
        Result :=Format('%12.2m',[aVal])
    else
        Result :=Self.fFlt(aVal) ;
end;

function UtilStr.fDat(const aDat: TDateTime; const aFormat: string): string;
begin
    if aFormat = '' then
        Result :='DD/MM/YYYY hh:nn';
    Result :=FormatDateTime(aFormat, aDat);
end;

function UtilStr.fFlt(const aVal: Extended): string;
begin
    Result :='';
end;

function UtilStr.fInt(const aVal: Extended): string ;
var
  int: Int64 ;
begin
    int :=Trunc(aVal) ;
    if int > 999 then
        Result :=Trim(Format('%9n',[int]))
    else
        Result :=IntToStr(int);
end;

function UtilStr.fSQL(const aStr: string): string;
begin
    if Trim(aStr) <> '' then
        Result :=QuotedStr(aStr)
    else
        Result :='NULL';
end;

function UtilStr.fTime(const aTime: TDateTime): string;
begin
//    if SizeOf(Self.m_FormatSettings) > 0 then
//        Result :=TimeToStr(aTime,Self.m_FormatSettings)
//    else
        Result :=FormatDateTime('hh:nn:ss',aTime) ;
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
