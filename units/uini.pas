unit uini;

interface

uses SysUtils, IniFiles;

type
  IMemIniFile = Interface(IInterface)

  end;
  TCMemIniFile =class(TMemIniFile)
  private
    m_ActiveSection: String ;
  public
    property Section: String read m_ActiveSection write m_ActiveSection;

    procedure WStr(const aKey, aValue: String);
    procedure WInt(const aKey: string; const aValue: Integer);
    procedure WBoo(const aKey: string; const aValue: Boolean);

    function ValStr(const aKey: string): string;
    function ValInt(const aKey: string): Integer;
    function ValBoo(const aKey: string): Boolean;
  end;


implementation


{ TCMemIniFile }


function TCMemIniFile.ValBoo(const aKey: string): Boolean;
begin
    Result :=Self.ReadBool(m_ActiveSection, aKey, False) ;

end;

function TCMemIniFile.ValInt(const aKey: string): Integer;
begin
    Result :=Self.ReadInteger(m_ActiveSection, aKey, 0) ;

end;

function TCMemIniFile.ValStr(const aKey: string): string;
begin
    Result :=Self.ReadString(m_ActiveSection, aKey, '') ;

end;

procedure TCMemIniFile.WBoo(const aKey: string; const aValue: Boolean);
begin
    Self.WriteBool(m_ActiveSection, aKey, aValue);

end;

procedure TCMemIniFile.WInt(const aKey: string; const aValue: Integer);
begin
    Self.WriteInteger(m_ActiveSection, aKey, aValue);

end;

procedure TCMemIniFile.WStr(const aKey, aValue: String);
begin
    Self.WriteString(m_ActiveSection, aKey, aValue);

end;

end.
