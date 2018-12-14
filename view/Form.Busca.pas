unit Form.Busca;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB,
  FormBase, uclass,
  VirtualTrees, uVSTree,
  JvExStdCtrls, JvButton, JvCtrls, JvFooter, ExtCtrls,
  JvExExtCtrls, JvExtComponent;

{$REGION 'TCBuscaCollumn'}
type
  TCBuscaCollumn = class(TCollectionItem)
  private
    fAlignment: TAlignment;
    fColor: TColor;
    fWidth: Integer;
    fTitle: String;
    fVisible: Boolean;
    fFont: TFont;
    fField: DB.TField;
    fAlignChr: Char;
    fAlignStr: Boolean;
    fAlignSize: Integer;
    fDataType: TFieldType;
    procedure SetField(const Value: DB.TField);
    function getFieldName: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read fAlignment write fAlignment;
    property Color: TColor read fColor write fColor;
    property Width: Integer read fWidth write fWidth;
    property Title: string read fTitle write fTitle;
    property Visible: Boolean read fVisible write fVisible;
    property Font: TFont read fFont write fFont;
    property Field: DB.TField read fField write SetField;
    property AlignStr: Boolean read fAlignStr write fAlignStr default false;
    property AlignChr: Char read fAlignChr write fAlignChr default '0';
    property AlignSize: Integer read fAlignSize write fAlignSize default 0;
    property DataType: TFieldType read fDataType write fDataType default ftUnknown;
    property FieldName: string read getFieldName;
  end;

  TCBuscaCollumnClass = class of TCBuscaCollumn;

  TCBuscaCollumns = class(TCollection)
  private
    function GetColumn(Index: Integer): TCBuscaCollumn;
    procedure SetColumn(Index: Integer; Value: TCBuscaCollumn);
  public
    property Items[Index: Integer]: TCBuscaCollumn read GetColumn write SetColumn; default;
    constructor Create(ItemClass: TCBuscaCollumnClass);
    function IndexOf(FieldName: string): TCBuscaCollumn;
    function Add: TCBuscaCollumn;
  end;

{$ENDREGION}

type
  TGridDataLink = class(TDataLink)
  private
  protected
  public
    function MoveBy(Distance: Integer): Integer; override;
  public
  end;


{$REGION 'TCBusca'}
type
  TAlignBar = (abBottom, abTop);

  TCBuscaBounds = class(TPersistent)
  private
    fWidth: Integer;
    fHeight: Integer;
  published
    property Width: Integer read fWidth write fWidth default 0;
    property Height: Integer read fHeight write fHeight default 0;
  public
    constructor Create;
  end;

  TCBuscaColors = class(TPersistent)
  private
    fColor: TColor;
    FGridColor: TColor;
  public
    constructor Create;
  private
    property Color: TColor read fColor write fColor;
  published
    property GridColor: TColor read FGridColor write FGridColor;
  end;

  TCBusca = class(TComponent)
  private
    FormBusca: TCustomForm;
    FCollumns: TCBuscaCollumns;
    FBuscaBounds: TCBuscaBounds;
    FComboIndex: Integer;
    FCaption: String;
    FResultField: String;
    FAlignBar: TAlignBar;
    FBuscaColors: TCBuscaColors;
    FVTPaintText: TVTPaintText;
    FResultValue: String;
    function GetResultValueInt: Integer;
    function GetResultValueStr: string;

  protected
    procedure CreateForm;
    function GetDataSetFields: TFields;
    function GetDataSet: TDataSet;
    procedure SetDataSet(Value: TDataSet);
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(Value: TStringTreeOptions);
    function GetResultValue: Variant;
    function GetDataSource: TDataSource;

  public
    external_function_get_canvas: function(col: Integer; Value: string; ACanvas: TCanvas): TCanvas;
    function ToStr(const Index: Integer): String;
    function ToInt(const Index: Integer): Integer;
    function ToDate(const Index: Integer): TDate;
    function ToCur(const Index: Integer): Currency;
    function ToFlt(const Index: Integer): Double;

  public
    class function NewBusca(): TCBusca;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  public
    property DataSetFields: TFields read GetDataSetFields;
    property DataSource: TDataSource read GetDataSource;

  published
    property DataSet: TDataSet read GetDataSet write SetDataSet;
    property Options: TStringTreeOptions read GetOptions write SetOptions;
    property Columns: TCBuscaCollumns read FCollumns write FCollumns;
    property Bounds: TCBuscaBounds read FBuscaBounds write FBuscaBounds;
    property ResultField: String read FResultField write FResultField;
    property Caption: String read FCaption write FCaption;
    property ComboIndex: Integer read FComboIndex write FComboIndex;
    property AlignBar: TAlignBar read FAlignBar write FAlignBar;
    property Colors: TCBuscaColors read FBuscaColors write FBuscaColors;
    property ResultValue: Variant read GetResultValue;
    property ResultValueInt: Integer read GetResultValueInt;
    property ResultValueStr: string read GetResultValueStr;
    property OnDrawCell: TVTPaintText read FVTPaintText write FVTPaintText;
  end;

{$ENDREGION}




type
  Tfrm_Busca = class(TBaseForm)
    pnl_Footer: TJvFooter;
    btn_Close: TJvFooterBtn;
    btn_OK: TJvFooterBtn;
    DBTreeGrid: TVirtualStringTree;
    ds_Grid: TDataSource;
    tm_Refresh: TTimer;
    procedure OnPaintGrid(Sender: TBaseVirtualTree; const ACanvas: TCanvas;
      Node: PVirtualNode;Column: TColumnIndex; TextType: TVSTTextType);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure DBTreeGridChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DBTreeGridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure btn_CloseClick(Sender: TObject);
    procedure tm_RefreshTimer(Sender: TObject);
    procedure btn_OKClick(Sender: TObject);

  private
    { Private declarations }
    CRec: TCThreadProcRec;
    CollumnsRead : array of Boolean;
    CollumnsText : array of array of string;
    ActiveNode   : PVirtualNode;
    DataLink     : TGridDataLink;
    function getDataSet: TDataSet;
		procedure LoadDataSet;
		procedure OnIniLoadstrings(Sender: TObject);
		procedure OnEndLoadstrings(Sender: TObject);
		procedure Find;

	protected
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;

  public
    { Public declarations }

		AlignBar     : TAlignBar    ;
		Colors       : TCBuscaColors ;
		Busca        : TCBusca      ;
		IndexNames   : TStringList  ;

		constructor Create(AOwner: TCBusca); reintroduce;
		destructor Destroy; override;
    function ToStr(Index: Integer): string;
    property ResultStr[Column: Integer]: string read ToStr;

    procedure ResetForm;
  end;



implementation

{$R *.dfm}

{$REGION 'TCBuscaCollumn'}

{ TCBuscaCollumn }

constructor TCBuscaCollumn.Create(Collection: TCollection);
begin
    inherited Create(Collection);
    fFont       := TFont.Create;
    fWidth      := 50;
    fColor      := clWindow;
    Visible     := True;
    fAlignment  := taLeftJustify;
    fFont.Color := clBlack;
    fFont.Style := [];
    fFont.Name  := 'Tahoma';
    fAlignChr   := '0';
    fAlignSize  := 10;
    fAlignStr   := false;
end;

destructor TCBuscaCollumn.Destroy;
begin
    fFont.Destroy;
    inherited Destroy;
end;

function TCBuscaCollumn.getFieldName: string;
begin
    if Assigned(Field) then
        Result := Field.FieldName
    else
        Result := '';
end;

procedure TCBuscaCollumn.SetField(const Value: DB.TField);
begin
    fField := Value;
    if Value = nil then
        fDataType := ftUnknown
    else
        fDataType := Value.DataType;
end;


{ TCBuscaCollumns }

function TCBuscaCollumns.Add: TCBuscaCollumn;
begin
    Result :=TCBuscaCollumn(inherited Add)
    ;
end;

constructor TCBuscaCollumns.Create(ItemClass: TCBuscaCollumnClass);
begin
    inherited Create(ItemClass)
    ;
end;

function TCBuscaCollumns.GetColumn(Index: Integer): TCBuscaCollumn;
begin
    Result :=nil;
    if (Index >= 0) and (Index < Count) then
    begin
        Result :=TCBuscaCollumn(inherited Items[Index]) ;
    end;
end;

function TCBuscaCollumns.IndexOf(FieldName: string): TCBuscaCollumn;
var
  I: Integer;
begin
    Result := nil;
    for I  := 0 to Self.Count -1 do
    begin
        if LowerCase(Items[I].Field.FieldName) = LowerCase(FieldName) then
        begin
          Result := Items[I];
        end;
    end;
end;

procedure TCBuscaCollumns.SetColumn(Index: Integer; Value: TCBuscaCollumn);
begin
    if (Index >= 0) and (Index < Count) then
    begin
        Items[Index].Assign(Value);
    end;
end;

{$ENDREGION}

{ TGridDataLink }

function TGridDataLink.MoveBy(Distance: Integer): Integer;
begin
    Result := (inherited MoveBy(Distance))
end;

{$REGION 'TCBusca'}

{ TCBuscaBounds }

constructor TCBuscaBounds.Create;
begin
    inherited Create;
    Width  :=640;
    Height :=480;
end;

{ TCBuscaColors }

constructor TCBuscaColors.Create;
begin
    inherited Create;
    Self.fColor     :=clBtnFace;
    Self.FGridColor :=clWindow;
end;

{ TCBusca }

constructor TCBusca.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    DataSet := nil;

    external_function_get_canvas := nil;

    Caption      :='';// Msg_SelectAnItem;
    FCollumns    := TCBuscaCollumns.Create(TCBuscaCollumn);
    FBuscaColors := TCBuscaColors.Create;

    FBuscaBounds := TCBuscaBounds.Create;

    FormBusca := nil;

end;

procedure TCBusca.CreateForm;
begin
    if not Assigned(FormBusca) then
    begin
        FormBusca :=Tfrm_Busca.Create(nil)
    end;
end;

destructor TCBusca.Destroy;
begin
    FCollumns.Destroy;
    FBuscaColors.Destroy;
    FBuscaBounds.Destroy;
    FreeAndNil(FormBusca);

    inherited Destroy;
end;

function TCBusca.Execute: Boolean;
var
  F: Tfrm_Busca;
var
  B: TCBuscaCollumn;
begin
    //
    // init result
    Result := false;

    //
    // se instance
    if Assigned(FormBusca) then
    begin
        F           :=Tfrm_Busca(FormBusca);
        F.Position  :=poMainFormCenter;
        F.Colors    :=Self.Colors;
        F.Caption   :=FCaption;
        F.Busca     :=Self;
        F.AlignBar  :=FAlignBar;

        if (FBuscaBounds.Width > 0) or (FBuscaBounds.Height > 0) then
        begin
            F.SetBounds(0, 0, FBuscaBounds.Width, FBuscaBounds.Height);
        end
        else
        begin
            F.SetBounds(0, 0, F.Width, F.Height);
        end;

        Result := F.ShowModal =mrOk ;
        if Result then
        begin
            if (Self.Columns.Count > 0) then
            begin
                B := Self.Columns.IndexOf(FResultField);
                if Assigned(B) then
                begin
                    Result            :=True;
                    Self.FResultValue :=F.ToStr(B.Index);
                    Self.FResultValue :=UpperCase(Self.FResultValue);
                end
                else begin
                    Result       :=false;
                    FResultValue :='';
                end;
            end;
        end;

    end;
end;

function TCBusca.GetDataSet: TDataSet;
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        Result :=Tfrm_Busca(FormBusca).ds_Grid.DataSet
    end
    else begin
        Result := nil
    end;
end;

function TCBusca.GetDataSetFields: TFields;
begin
    Result :=DataSet.Fields
    ;
end;

function TCBusca.GetDataSource: TDataSource;
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        Result :=Tfrm_Busca(FormBusca).ds_Grid
    end
    else begin
        Result := nil
    end;
end;

function TCBusca.GetOptions: TStringTreeOptions;
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        Result :=Tfrm_Busca(FormBusca).DBTreeGrid.TreeOptions
    end
    else
    begin
        Result := nil
    end;
end;

function TCBusca.GetResultValue: Variant;
begin
    Result :=FResultValue
    ;
end;

function TCBusca.GetResultValueInt: Integer;
begin
    Result := StrToInt(ResultValue)
    ;
end;

function TCBusca.GetResultValueStr: string;
begin
    Result :=ResultValue
    ;
end;

class function TCBusca.NewBusca: TCBusca;
begin
    Result                  :=TCBusca.Create(nil);
//    Result.ComboIndex       := ComboIndex;
    Result.Colors.Color     :=clBtnFace; // uformat.frmColorEnabled;
    Result.Colors.GridColor :=clBtnFace; // uformat.frmColorEnabled;
    Result.CreateForm;
end;

procedure TCBusca.SetDataSet(Value: TDataSet);
var
  ds: TDataSource;
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        ds :=Tfrm_Busca(FormBusca).ds_Grid
    end
    else begin
        ds :=nil
    end;

    if Assigned(ds) then
    begin
        ds.DataSet :=Value;
        if not ds.DataSet.Active then
        begin
            ds.DataSet.Active :=True;
        end;
    end;
end;

procedure TCBusca.SetOptions(Value: TStringTreeOptions);
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        if Tfrm_Busca(FormBusca).DBTreeGrid.TreeOptions <> Value then
        begin
            Tfrm_Busca(FormBusca).DBTreeGrid.TreeOptions :=Value;
        end;
    end;
end;

function TCBusca.ToCur(const Index: Integer): Currency;
var
  S: string;
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        S :=Tfrm_Busca(FormBusca).ToStr(Index)
    end
    else begin
        S :='';
    end;

    if not TryStrToCurr(S, Result) then
    begin
        Result :=0;
    end;
end;

function TCBusca.ToDate(const Index: Integer): TDate;
var
  S: string;
  D: TDateTime ;
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        S :=Tfrm_Busca(FormBusca).ToStr(Index)
    end
    else begin
        S :='';
    end;

    if not TryStrToDate(S, D) then
    begin
        D :=0
    end;
    Result :=D;
end;

function TCBusca.ToFlt(const Index: Integer): Double;
var
  S: string;
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        S :=Tfrm_Busca(FormBusca).ToStr(Index)
    end
    else begin
        S :='';
    end;

    if not TryStrToFloat(S, Result) then
    begin
        Result :=0;
    end;
end;

function TCBusca.ToInt(const Index: Integer): Integer;
var
  S: string;
begin
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        S :=Tfrm_Busca(FormBusca).ToStr(Index)
    end
    else begin
        S :='';
    end;

    if not TryStrToInt(S, Result) then
    begin
        Result := 0
    end;
end;

function TCBusca.ToStr(const Index: Integer): String;
begin
    Result :='';
    if Assigned(FormBusca) and (FormBusca is Tfrm_Busca) then
    begin
        Result := Tfrm_Busca(FormBusca).ToStr(Index);
    end ;
end;

{$ENDREGION}



{ Tfrm_Busca }

procedure Tfrm_Busca.btn_CloseClick(Sender: TObject);
begin
    ModalResult :=mrCancel
    ;
end;

procedure Tfrm_Busca.btn_OKClick(Sender: TObject);
var
  D: TDataSet;
  P: PVirtualNode;
begin
    D :=DataLink.DataSource.DataSet;
    P :=DBTreeGrid.GetFirstSelected;
    if Assigned(D)and Assigned(P)then
    begin
        if(not getDataSet.IsUniDirectional)then
        begin
            D.First;
            DataLink.MoveBy(P.Index);
        end;
        ModalResult :=mrOk;
    end;
end;

constructor Tfrm_Busca.Create(AOwner: TCBusca);
begin
    inherited Create(AOwner);
    DataLink:=TGridDataLink.Create;
    Busca   :=AOwner;
end;

procedure Tfrm_Busca.DBTreeGridChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
    ActiveNode :=Node
    ;
end;

procedure Tfrm_Busca.DBTreeGridGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  C: TCBuscaCollumn;
  S: string;
begin
    C :=Self.Busca.Columns[Column];
    if not CollumnsRead[Node.Index] then
        CellText :='Carregando..'
    else begin
        if Assigned(C) and C.AlignStr then
        begin
            S :=CollumnsText[Node.Index, Column];
            CellText :=Format('%.'+IntToStr(C.AlignSize)+'d',[StrToIntDef(S,0)]);//SCod(S,C.AlignSize);
            S :=Format('%-'+IntToStr(C.AlignSize)+'s',[S]); //AlignStr(C.AlignSize, S, C.AlignChr, taLeftJustify);
        end
        else begin
            CellText:=CollumnsText[Node.Index, Column];
        end;
    end;
end;

destructor Tfrm_Busca.Destroy;
begin
    Busca :=nil;
    DataLink.Destroy;
    inherited Destroy;
end;

procedure Tfrm_Busca.Find;
begin

end;

procedure Tfrm_Busca.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    tm_Refresh.Enabled :=False;
    while Assigned(CRec.CExec) do Sleep(1000);
    CRec.CExec:=nil;
end;

procedure Tfrm_Busca.FormDestroy(Sender: TObject);
begin
    SetLength(Self.CollumnsRead,0);
    SetLength(Self.CollumnsText,0,0);
end;

procedure Tfrm_Busca.FormShow(Sender: TObject);
var
  ds: TDataSet;
begin
    ds :=getDataSet ;
    if not Assigned(Busca)or not Assigned(ds)then
    begin
        Self.Close;
    end
    else begin
//    uformat.FormatButtonBitmap(btnOK     ,tcbSelect    );
//    uformat.FormatButtonBitmap(btnSearch ,tcbConsulta  );
//    uformat.FormatButtonBitmap(btnFechar ,tcbSair      );
//
//    btnSearch      .ButtonStyle:=uformat.ButtonStyle;
//    btnOK          .ButtonStyle:=uformat.ButtonStyle;
//    btnFechar      .ButtonStyle:=uformat.ButtonStyle;
//    chkSensitive   .ButtonStyle:=uformat.ButtonStyle;

        ResetForm;
    end;
end;

function Tfrm_Busca.getDataSet: TDataSet;
begin
    Result :=Busca.DataSet
    ;
end;

procedure Tfrm_Busca.KeyDown(var Key: Word; Shift: TShiftState);
begin
    if Shift=[] then
    begin
      case Key of
        VK_RETURN : if btn_OK.Enabled      then btn_OK.Click    ;
  //      Vk_F3     : if btnSearch  .Enabled      then btnSearch  .Click    ;
  //      Vk_F4     : if ActiveControl=EditBusca  then DBTreeGrid .SetFocus
  //                  else                             EditBusca  .SetFocus ;
  //      VK_ESCAPE : if btnFechar.Enabled        then btnFechar  .Click    ;
      end;
    end;
    inherited;
end;

procedure Tfrm_Busca.LoadDataSet;
var BC:TCBuscaCollumn;
var D:TDataSet;
var F:Tfrm_Busca;
var C:DB.TField;
var i:Integer;
var x:Integer;
var v:string;
begin
    F :=Self;
    D :=F.Busca.DataSet;
    if not D.IsUniDirectional then D.First;
    x :=0;
    while (not D.Eof) and (D.RecordCount>0) do
    begin
        if Application.Terminated then
        begin
             Break
        end
        else
        begin
             for i:=0 to F.Busca.Columns.Count-1 do
             begin
                  if Application.Terminated then
                  begin
                       Break
                  end
                  else
                  begin
                       BC:=F.Busca.Columns[i];
                       C:=BC.Field;
                       if Assigned(F)and Assigned(C) then
                       begin
                            F.CollumnsRead[x]:=True;
                            case BC.DataType of
                                 ftDate,
                                 ftTimeStamp,
                                 ftDateTime:
                                       v:=DateToStr(C.AsDateTime);
                                 ftTime:
                                       v:=TimeToStr(C.AsDateTime);
                                 ftSmallint,
                                 ftLargeint,
                                 ftWord,
                                 ftInteger:
                                       v:=IntToStr(C.AsInteger);
                                 ftFloat:
                                       v:=FloatToStr(C.AsFloat);
                                 ftCurrency:
                                       v:=CurrToStr(C.AsCurrency);
                                 ftstring,
                                 ftWidestring:
                                       v:=C.Text;
                                 ftBoolean:
                                       if C.AsBoolean then
                                          v:='Sim'
                                       else
                                          v:='Não';
                                 ftGraphic:
                                       v:='[Imagem]';
                                 ftBlob:
                                       v:='[Blob]';
                                 ftMemo:
                                       v:='[Memo]';
                                 ftUnknown:
                                       v:='?????';
                            end;
                            F.CollumnsText[x,i]:=v;
                       end;
                  end;
             end;
        end;
        inc(x);
        D.Next;
    end;
end;

procedure Tfrm_Busca.OnEndLoadstrings(Sender: TObject);
begin
    Self.DBTreeGrid.Refresh;
    CRec.CExec:=nil;
end;

procedure Tfrm_Busca.OnIniLoadstrings(Sender: TObject);
begin
    tm_Refresh.Enabled :=True
    ;
end;

procedure Tfrm_Busca.OnPaintGrid(Sender: TBaseVirtualTree;
  const ACanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  C:TCBuscaCollumn;
begin
    C:=Self.Busca.Columns[Column];
    if Assigned(C)then
    begin
        ACanvas.Font.Assign(C.Font);
    end;

    if Sender.Selected[node]then
    begin
        ACanvas.Font.Color:=clMaroon;
    end
    else begin
        ACanvas.Font.Color:=clBlack;
    end;

    if Assigned(Self.Busca.OnDrawCell) then
    begin
        Self.Busca.OnDrawCell(Sender, ACanvas, Node, Column, TextType);
    end;
end;

procedure Tfrm_Busca.ResetForm;
    //
    //
    procedure StartReadDataSet;
    begin
         CRec.OnIni:=OnIniLoadstrings;
         CRec.OnEnd:=OnEndLoadstrings;
         CRec.CProc:=LoadDataSet;
         CRec.CExec:=TCThreadProc.Create(CRec);
    end;
    //
    //
    procedure creategridcoluns;
        procedure Add(A:TCBuscaCollumn);
        var C: TVirtualTreeColumn;
        begin
            if Assigned(A)then
            begin
                C:=Self.DBTreeGrid.Header.Columns.Add;
                C.Text       := A.Title    ;
                C.Width      := A.Width+10 ;
                C.Alignment  := A.Alignment;
                C.Color      := A.Color    ;
                if A.Visible then
                    C.Options    := C.Options + [coVisible]
                else
                    C.Options    := C.Options - [coVisible];
            end;
        end;
    var i:Integer;
    var L:TCBuscaCollumns;
    var C:TCBuscaCollumn ;
    begin
         Self.DBTreeGrid.Header.Columns.Clear;
         L:=Self.Busca.Columns;
         for i:=0 to L.Count-1 do begin
             C:=L.Items[i];
             Add(C);
         end;
    end;
var
  P:TPoint;
begin
    DBTreeGrid.OnPaintText :=Self.OnPaintGrid;

    getDataSet.DisableControls;

    {Case Self.AlignBar of
        abBottom :Pnl_Busca.Align := alBottom;
    else//abTop
       Pnl_Busca.Align := alTop;
    end;}

    Self.DataLink.DataSource:=Self.ds_Grid;

    DBTreeGrid.RootNodeCount:=0;
    creategridcoluns;

    P.X:=Self.Busca.Columns.Count;
    try
      P.Y:=getDataSet.RecordCount;
    except
      try
         P.Y:=0;
         if not getDataSet.IsUniDirectional then getDataSet.First;
         while not getDataSet.Eof do
         begin
              Inc(P.Y);
              getDataSet.Next;
         end;
         if not getDataSet.IsUniDirectional then getDataSet.First;
      except end;
    end;

    SetLength(Self.CollumnsRead,0);
    SetLength(Self.CollumnsText,0,0);

    SetLength(Self.CollumnsRead,P.Y);
    SetLength(Self.CollumnsText,P.Y,P.X);

    DBTreeGrid.RootNodeCount :=getDataSet.RecordCount;

    StartReadDataSet;

    //edt_Busca.Visible :=False;
end;

procedure Tfrm_Busca.tm_RefreshTimer(Sender: TObject);
begin
    if not Assigned(CRec.CExec)then
    begin
        TTimer(Sender).Enabled :=False;
    end
    else
    begin
        DBTreeGrid.Refresh;
    end;
end;

function Tfrm_Busca.ToStr(Index: Integer): string;
var
  I: Integer;
begin
    Result:='';
    if Assigned(ActiveNode)then
    begin
        I :=ActiveNode.Index;
        Result :=CollumnsText[I, Index];
    end;
end;

end.
