{***
* Classes/Tipos para tratamento de registro de LOG
* Todos os direitos reservados
* Autor: Carlos Gonzaga
* Data: 29.01.2017
*}
unit ulog;

{*
******************************************************************************
|* PROPÓSITO: Registro de Alterações
******************************************************************************

Símbolo : Significado

[+]     : Novo recurso
[*]     : Recurso modificado/melhorado
[-]     : Correção de Bug (assim esperamos)


01.06.2018
[*] Separa o arquivo conforme data (ano\mes)

*}

interface

uses
//  Windows,
  SysUtils,
  uclass,
  Classes;

type
  TOnSWLogChange = procedure(Sender: TObject; const StrLog: string) of object;
  TOnLogChange = procedure(Sender: TObject; const StrLog: string) of object;

{
interface
type
  TSingleton= class
  private
    class var FInstance: TSingleton;
    class function GetInstance: TSingleton; static;
  public
    class property Instance : TSingleton read GetInstance;
  end;
implementation
  class function TSingleton.GetInstance: TSingleton;
  begin
    If FInstance = nil Then
      FInstance := TSingleton.Create();//objeto instanciado através do Finstance
    Result := FInstance;//retorna o objeto
  end;
}

type
  TSWLog = class
  private
    //Singleton
    class var FInstance: TSWLog ;
    class function GetInstance: TSWLog; static ;
  public
    ListLogMessage: TStringList;
    procedure AddLog(message: string); overload;
    procedure AddLog(const cformat: string; const Args: array of const); overload;
    procedure SaveToFile(cfilename: string);
  public
    LogName: string;
    LogPath: string;
    LogFold: string;
    OnSWLogChange: TOnSWLogChange;
  public
    function LogFile: string;
  public
    constructor Create;
    destructor Destroy; override;
    class property Instance : TSWLog read GetInstance;
    class procedure Add(const msg: string); overload;
    class procedure Add(const cformat: string; const Args: array of const); overload;
  end;


//  TCLogFile = class(TStreamWriter)
//  private
//  public
//  end;


  TCLog = class
  private
    m_OnLogChange: TOnLogChange;
    m_Buf: TBytes;
    m_FileDir, m_FileName: string ;
    m_LogFileW: TStreamWriter ;
    m_LogFileS: TFileStream;
    m_AppendIfExists: Boolean;
    m_dhCreate: TDateTime;
    procedure AddLog(const aBinStr: AnsiString);
  public
    property FileName: string read m_FileName write m_FileName;
    property OnLogChange: TOnLogChange read m_OnLogChange write m_OnLogChange ;
    property dhCreate: TDateTime read m_dhCreate;

    constructor Create (const aFileName: string;
      const appendIfExists: Boolean); reintroduce ;
    destructor Destroy; override;

    procedure AddChange(const aMsg: string);
    procedure AddSec(const aBinStr: AnsiString); overload;
    procedure AddSec(const aFormat: string; const args: array of const); overload;
    procedure AddPar(const aBinStr: AnsiString);
  end;

  //
  //
  ILogFile = Interface(IInterface)
    function getFileName: string;
    property FileName: string read getFileName;

    function getLogChange: TOnLogChange ;
    procedure setLogChange(const aValue: TOnLogChange) ;
    property OnLogChange: TOnLogChange read getLogChange write setLogChange;

    procedure AddStr(const aStr: string; const aIncTime: Boolean =False);
  end;

  //
  //
  TCLogFile = class(TInterfacedObject, ILogFile)
  strict private
    m_FileName: string ;
    //m_AppendIfExists: Boolean;
    m_Buf: TStreamWriter ;
    m_OnLogChange: TOnLogChange;
    function getFileName: string;
    function getLogChange: TOnLogChange ;
    procedure setLogChange(const aValue: TOnLogChange) ;

    constructor Create(const aFileName: string;
      const appendIfExists: Boolean); reintroduce;
    destructor Destroy; override ;

  protected
  public
    property FileName: string read getFileName;
    property OnLogChange: TOnLogChange read getLogChange write setLogChange;
    procedure AddStr(const aStr: string; const aIncTime: Boolean =False);
    class function New(const aFileName: string;
      const appendIfExists: Boolean): ILogFile;
  end;



type
  TCExeInfo = class
  private
    class var _instance: TCExeInfo ;
  private
    { Private declarations }
    FCompanyName      : String;
    FFileDescription  : String;
    FFileVersion      : String;
    FInternalName     : String;
    FLegalCopyright   : String;
    FLegalTradeMark   : String;
    FOriginalFileName : String;
    FProductName      : String;
    FProductVersion   : String;
    FComments         : String;
    FComputerName     : String;
    FOsName           : String;
    FWindowsDir       : String;
    FSystemDir        : String;
    FTempDir          : String;
    FFileFlags        : integer;
    FFileOS           : integer;
    FFileType         : integer;
    FFileCreation     : TDateTime;

//    function GetVersion: string;
//    procedure SetVersion(const Value: string);
//    function GetFileVersionInt: integer;
  protected
    function GetVersionPart(const idx: integer): integer;
  public
    class function getInstance: TCExeInfo; static ;
  public
    procedure GetVersionInfoOfApp(const aExeName: String);
    function MajorVersion: integer;
    function MinorVersion: integer;
    function ReleaseNumber: integer;
    function BuildNumber: integer;
  end;


var
  CLog: TSWLog = nil;



implementation

uses Windows, Math, TypInfo ;


{ TCExeInfo }

function TCExeInfo.BuildNumber: integer;
begin
    Result := GetVersionPart(3);
end;

class function TCExeInfo.getInstance: TCExeInfo;
begin
    if _instance = nil then
    begin
        _instance :=TCExeInfo.Create ;
    end;
    Result :=_instance;
end;

procedure TCExeInfo.GetVersionInfoOfApp(const aExeName: String);
type
  PTransBuffer = ^TTransBuffer;
  TTransBuffer = array[1..4] of smallint;
var
  iAppSize, iLenOfValue : DWord;
  pcBuf,pcValue         : PChar;
  VerSize               : DWord;
  pTrans                : PTransBuffer;
  TransStr              : string;
  sAppName              : String;
  fvip                  : pointer;
  ft                    : TFileTime;
  st                    : TSystemTime;
begin
  sAppName := aExeName;
  // get version information values
  iAppSize:= GetFileVersionInfoSize(PChar(sAppName),// pointer to filename string
                                    iAppSize);      // pointer to variable to receive zero
   // if GetFileVersionInfoSize is successful
  if iAppSize > 0 then
    begin
      pcBuf := AllocMem(iAppSize);

      GetFileVersionInfo(PChar(sAppName),              // pointer to filename string
                         0,                            // ignored
                         iAppSize,                     // size of buffer
                         pcBuf);                       // pointer to buffer to receive file-version info.


      VerQueryValue(pcBuf, '\', fvip, iLenOfValue);

      FFileFlags := TVSFixedFileInfo(fvip^).dwFileFlags and TVSFixedFileInfo (fvip^).dwFileFlagsMask;
      FFileOS := TVSFixedFileInfo(fvip^).dwFileOS;
      FFileType := TVSFixedFileInfo(fvip^).dwFileType;

      ft.dwLowDateTime := TVSFixedFileInfo(fvip^).dwFileDateLS;
      ft.dwHighDateTime := TVSFixedFileInfo(fvip^).dwFileDateMS;

      if (ft.dwLowDateTime <> 0) or (ft.dwHighDateTime <> 0) then
      begin
        FileTimeToSystemTime(ft,st);
        FFileCreation := SystemTimeToDateTime(st);
      end
      else
      begin
        {$IFDEF DELPHI_UNICODE}
        FileAge(Application.ExeName, FFileCreation);
        {$ENDIF}
        {$IFNDEF DELPHI_UNICODE}
        FFileCreation := FileDateToDateTime(FileAge(aExeName));
        {$ENDIF}
      end;

      VerQueryValue(pcBuf, PChar('\VarFileInfo\Translation'),
              pointer(ptrans), verSize);
      TransStr:= IntToHex(ptrans^[1], 4) + IntToHex(ptrans^[2], 4);

      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'CompanyName'), Pointer(pcValue),iLenOfValue) then
            FCompanyName := pcValue
      Else  FCompanyName := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'FileDescription'), Pointer(pcValue),iLenOfValue) then
            FFileDescription := pcValue
      Else  FFileDescription := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'FileVersion'), Pointer(pcValue),iLenOfValue) then
            FFileVersion := pcValue
      Else  FFileVersion := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'InternalName'), Pointer(pcValue),iLenOfValue) then
            FInternalName := pcValue
      Else  FInternalName := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'LegalCopyright'), Pointer(pcValue),iLenOfValue) then
            FLegalCopyright := pcValue
      Else  FLegalCopyright := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'LegalTradeMarks'), Pointer(pcValue),iLenOfValue) then
            FLegalTradeMark := pcValue
      Else  FLegalTradeMark := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'OriginalFileName'), Pointer(pcValue),iLenOfValue) then
            FOriginalFileName := pcValue
      Else  FOriginalFileName := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'ProductName'), Pointer(pcValue),iLenOfValue) then
            FProductName := pcValue
      Else  FProductName := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'ProductVersion'), Pointer(pcValue),iLenOfValue) then
            FProductVersion := pcValue
      Else  FProductVersion := '';
      if VerQueryValue(pcBuf,PChar('StringFileInfo\' + TransStr + '\' +
           'Comments'), Pointer(pcValue),iLenOfValue) then
            FComments := pcValue
      Else  FComments := '';
      FreeMem(pcBuf,iAppSize);
    end;
end;

function TCExeInfo.GetVersionPart(const idx: integer): integer;
var
  sl: TStringList;
  verstr: string;
begin
  Result := -1;

  if FFileVersion = '' then
    Exit;

  verstr := StringReplace(FFileVersion,'.',',',[rfReplaceAll]);

  sl := TStringList.Create;
  try
    sl.CommaText := verstr;

    if idx < sl.Count then
      Result := StrToInt(sl.Strings[idx]);
  finally
    sl.Free;
  end;
end;

function TCExeInfo.MajorVersion: integer;
begin
    Result := GetVersionPart(0);
end;

function TCExeInfo.MinorVersion: integer;
begin
    Result := GetVersionPart(1);
end;

function TCExeInfo.ReleaseNumber: integer;
begin
    Result := GetVersionPart(2);
end;

{ TSWLog }

procedure TSWLog.AddLog(message: string);
begin
  try
    if Assigned(OnSWLogChange) then
    begin
      OnSWLogChange(Self, message) ;
    end;
  except
  end;
  if ListLogMessage.Count > 5000 then
  begin
    ListLogMessage.Clear;
  end;
  message := Format(':%s:%s', [DateTimeToStr(Now), message]);
  ListLogMessage.Add(message);
  SaveToFile(LogFile);
end;

class procedure TSWLog.Add(const msg: string);
begin
  if Assigned(FInstance) then
  begin
    FInstance.AddLog(msg);
  end;
end;

class procedure TSWLog.Add(const cformat: string; const Args: array of const);
begin
  TSWLog.Add(Format(cformat, Args));

end;

procedure TSWLog.AddLog(const cformat: string; const Args: array of const);
begin
  AddLog(Format(cformat, Args));

end;

constructor TSWLog.Create;
begin
  inherited Create;
  OnSWLogChange  := nil;
  ListLogMessage := TStringList.Create;
  LogFold        := 'log'
end;

destructor TSWLog.Destroy;
begin
  ListLogMessage.Clear;
  ListLogMessage.Destroy;
  inherited Destroy;
end;

class function TSWLog.GetInstance: TSWLog;
begin
  if FInstance = nil then
  begin
    FInstance :=TSWLog.Create ;
  end;
  Result :=FInstance ;
end;

function TSWLog.LogFile: string;
begin
  Result := Format('%slog\%s.log', [LogPath, LogName]);
end;

procedure TSWLog.SaveToFile(cfilename: string);
begin
  ForceDirectories(ExtractFilePath(cfilename));
  try
    ListLogMessage.SaveToFile(cfilename);
  except
  end;
end;


{ TCLog }


procedure TCLog.AddChange(const aMsg: string);
begin
    if Assigned(OnLogChange) then
    begin
        OnLogChange(Self, aMsg) ;
    end ;
end;

procedure TCLog.AddLog(const aBinStr: AnsiString);
var
  LineBreak: AnsiString ;
  exists: Boolean ;
  Y,M,D: Word ;
begin
    if not Assigned(m_LogFileS) then
    begin
        //
        if m_FileName = '' then
        begin
            DecodeDate(Date, Y, M, D);
            m_FileDir  :=ExtractFilePath(ParamStr(0)) ;
            m_FileDir  :=Format('%s\log\%d\%.2d',[m_FileDir,Y,M]) ;
            if not DirectoryExists(m_FileDir) then
            begin
                ForceDirectories(m_FileDir);
            end;
            m_FileName :=ExtractFileName(ParamStr(0));
            m_FileName :=ChangeFileExt(m_FileName, '');
            m_FileName :=Format('%s\%s_%.2d.log',[m_FileDir,m_FileName,D]) ;
        end;

        exists :=FileExists(m_FileName);

        m_LogFileS :=TFileStream.Create(m_FileName,
                       IfThen( m_AppendIfExists and FileExists(m_FileName),
                               Integer(fmOpenReadWrite),
                       Integer(fmCreate)) or fmShareDenyWrite );

        if exists then
        begin
            m_LogFileS.Seek(0, soFromEnd);  // vai para EOF
            AddLog(sLineBreak);
            AddSec('Arquivo de log aberto');
        end
        else begin
            AddSec('Arquivo de log criado');
        end;
        m_dhCreate :=Now ;
    end;

    LineBreak :=sLineBreak;
    m_LogFileS.Write(Pointer(aBinStr)^,Length(aBinStr));
    m_LogFileS.Write(Pointer(LineBreak)^,Length(LineBreak));
    //FlushFileBuffers(m_LogFileS.Handle);

    if Assigned(m_OnLogChange)then
    begin
        OnLogChange(Self, aBinStr) ;
    end;
end;

procedure TCLog.AddPar(const aBinStr: AnsiString);
begin
    AddLog(#9 +aBinStr +#10);

end;

procedure TCLog.AddSec(const aformat: string; const args: array of const);
begin
    Self.AddSec(
                Format(aformat, args)
                );
end;

procedure TCLog.AddSec(const aBinStr: AnsiString);
var
  hh, nn, ss, ms: Word ;
begin
    DecodeTime(Time, hh, nn, ss, ms);
    AddLog(
            Format('%.2d:%.2d:%.2d|%s',[hh, nn, ss, aBinStr])
          );
end;

constructor TCLog.Create (const aFileName: string;
  const appendIfExists: Boolean);
begin
    inherited Create;
    OnLogChange :=nil;
    m_FileName :=aFileName;
    m_AppendIfExists :=appendIfExists ;
end;

destructor TCLog.Destroy;
begin
    if Assigned(m_LogFileS) then
    begin
        Self.AddSec('Arquivo de log fechado');
        m_LogFileS.Destroy ;
    end;
    inherited;
end;


//begin
//  CLog :=TSWLog.Instance;
//  CLog.LogPath :=ExtractFilePath(ParamStr(0));

{ TCLogFile }

procedure TCLogFile.AddStr(const aStr: string; const aIncTime: Boolean);
var
  H, N, S, MS: Word ;
begin
    if aIncTime then
    begin
        DecodeTime(Time, H, N, S, MS);
        m_Buf.WriteLine('%.2d:%.2d:%.2d|%s',[H, N, S, aStr]) ;
    end
    else begin
        m_Buf.WriteLine(aStr) ;
    end;
end;

constructor TCLogFile.Create(const aFileName: string;
  const appendIfExists: Boolean);
var
  dir: string ;
  exists: Boolean ;
  S: TFileStream;
begin
    inherited Create;
    //
    //
    m_FileName :=aFileName;
    if m_FileName = '' then
    begin
        m_FileName :=ChangeFileExt(ParamStr(0), '.LOG');
    end;

    dir :=ExtractFilePath(m_FileName);
    if not DirectoryExists(dir) then
    begin
        ForceDirectories(dir);
    end;

    exists :=FileExists(m_FileName);
    S :=TFileStream.Create(m_FileName,
                   IfThen( appendIfExists and FileExists(m_FileName),
                           Integer(fmOpenReadWrite),
                   Integer(fmCreate)) or fmShareDenyWrite );

    m_Buf :=TStreamWriter.Create(S) ;

    if exists then
    begin
        S.Seek(0, soFromEnd);  // vai para EOF
        AddStr('Arquivo de log aberto', True);
    end
    else begin
        AddStr('Arquivo de log criado', True);
    end;
    //
    //
    m_OnLogChange :=nil;
end;

destructor TCLogFile.Destroy;
begin
    AddStr('Arquivo de log fechado', True);
    if Assigned(m_Buf.BaseStream) then
        m_Buf.BaseStream.Free ;
    m_Buf.Destroy;
    inherited;
end;

function TCLogFile.getFileName: string;
begin
    Result :=m_FileName ;

end;

function TCLogFile.getLogChange: TOnLogChange;
begin
    Result :=m_OnLogChange ;

end;

class function TCLogFile.New(const aFileName: string;
  const appendIfExists: Boolean): ILogFile;
begin
    Result :=TCLogFile.Create(aFileName, appendIfExists) ;

end;

procedure TCLogFile.setLogChange(const aValue: TOnLogChange);
begin
    m_OnLogChange :=aValue ;

end;

end.