unit uini;

interface

uses SysUtils, IniFiles;

type
  IMemIniFile = Interface(IInterface)
    function getFileName: string ;
    property FileName: string read getFileName;

    function getActiveSection: string ;
    procedure setActiveSection(const aValue: string);
    property Section: String read getActiveSection write setActiveSection;

    procedure WStr(const aKey, aValue: String);
    procedure WInt(const aKey: string; const aValue: Integer);
    procedure WBoo(const aKey: string; const aValue: Boolean);
    procedure WDat(const aKey: string; const aValue: TDateTime);

    function ValStr(const aKey: string; const aDefault: string =''): string;
    function ValInt(const aKey: string; const aDefault: Integer =0): Integer;
    function ValBoo(const aKey: string; const aDefault: Boolean =False): Boolean;
    function ValDat(const aKey: string; const aDefault: TDateTime =0): TDateTime;

    procedure Update;
  end;

  TCMemIniFile =class(TInterfacedObject, IMemIniFile)
  private
    m_MemIni: TMemIniFile;
    m_ActiveSection: String ;
    function getFileName: string ;
    function getActiveSection: string ;
    procedure setActiveSection(const aValue: string);
  public
    property FileName: string read getFileName;
    property Section: String read getActiveSection write setActiveSection;

    procedure WStr(const aKey, aValue: String);
    procedure WInt(const aKey: string; const aValue: Integer);
    procedure WBoo(const aKey: string; const aValue: Boolean);
    procedure WDat(const aKey: string; const aValue: TDateTime);

    function ValStr(const aKey: string; const aDefault: string =''): string;
    function ValInt(const aKey: string; const aDefault: Integer =0): Integer;
    function ValBoo(const aKey: string; const aDefault: Boolean =False): Boolean;
    function ValDat(const aKey: string; const aDefault: TDateTime =0): TDateTime;
  public
    constructor Create(aFileName: string);
    destructor Destroy; override ;
    procedure Update;
    class function New(const aFileName: string): IMemIniFile ;
  end;


implementation


{ TCMemIniFile }


constructor TCMemIniFile.Create(aFileName: string);
begin
    if aFileName <> '' then
        m_MemIni :=TMemIniFile.Create(aFileName)
    else
        m_MemIni :=TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.AppConfig'));
end;

destructor TCMemIniFile.Destroy;
begin
    m_MemIni.Free ;
    inherited;
end;

function TCMemIniFile.getActiveSection: string;
begin
    Result :=m_ActiveSection ;
end;

function TCMemIniFile.getFileName: string;
begin
    Result :=m_MemIni.FileName ;

end;

class function TCMemIniFile.New(const aFileName: string): IMemIniFile;
begin
    Result :=TCMemIniFile.Create(aFileName);

end;

procedure TCMemIniFile.setActiveSection(const aValue: string);
begin
    m_ActiveSection :=aValue ;

end;

procedure TCMemIniFile.Update;
begin
    m_MemIni.UpdateFile ;

end;

function TCMemIniFile.ValBoo(const aKey: string;
  const aDefault: Boolean =False): Boolean;
begin
    Result :=m_MemIni.ReadBool(m_ActiveSection, aKey, aDefault) ;

end;

function TCMemIniFile.ValDat(const aKey: string;
  const aDefault: TDateTime): TDateTime;
begin
    Result :=m_MemIni.ReadDateTime(m_ActiveSection, aKey, aDefault) ;

end;

function TCMemIniFile.ValInt(const aKey: string;
  const aDefault: Integer): Integer;
begin
    Result :=m_MemIni.ReadInteger(m_ActiveSection, aKey, aDefault) ;

end;

function TCMemIniFile.ValStr(const aKey: string;
  const aDefault: string): string;
begin
    Result :=m_MemIni.ReadString(m_ActiveSection, aKey, aDefault) ;

end;

procedure TCMemIniFile.WBoo(const aKey: string; const aValue: Boolean);
begin
    m_MemIni.WriteBool(m_ActiveSection, aKey, aValue);

end;

procedure TCMemIniFile.WDat(const aKey: string; const aValue: TDateTime);
begin
    m_MemIni.WriteDateTime(m_ActiveSection, aKey, aValue);

end;

procedure TCMemIniFile.WInt(const aKey: string; const aValue: Integer);
begin
    m_MemIni.WriteInteger(m_ActiveSection, aKey, aValue);

end;

procedure TCMemIniFile.WStr(const aKey, aValue: String);
begin
    m_MemIni.WriteString(m_ActiveSection, aKey, aValue);

end;

end.
