{***
* Clases base para funcionalidades do View/Form
* Todos os Direitos Reservados
* Autor: Carlos Gonzaga
*
*}
unit FormBase;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls, Mask, MaskUtils ;

//	Forms, Controls, StdCtrls, Classes, Windows, Messages ,
//	Generics.Collections	;


{$REGION 'Extenções/class helpers'}
type
  TFormatEditMask = (femCPF, femCNPJ, femCEP, femCustom, femNone) ;
  TMaskEdit = class(Mask.TMaskEdit)
  protected
    FShowMessage: string ;
    function Validate(const Value: string; var Pos: Integer): Boolean; Override;
  public
    function GetMask: String;
    function IsEmpty(): Boolean;
    procedure DoFormatEditMask(const AFormatEditMask: TFormatEditMask;
      const AShowMessage: string = '';
      const AEditMask: string = '') ;
    class function IsEmptyText(const AEditMask: TEditMask; const AText: String): Boolean;
  end;

  TCustomEditHlp = class helper for TCustomEdit
    procedure DoClear;
    procedure DoFormat(const AFormat: string;	const Args: array of const);
    function IsEmpty(const AMsg: string = ''): Boolean ;
  end;

  TCustomComboHlp = class helper for TCustomCombo
  public
    procedure AddText(const AText: string; const AItemIndex: Integer =0);
  end;


  TCPanelStatus = class(TCustomPanel)
  private
    fMultiLine: Boolean;
    fPainting: Integer;
    procedure setMultiLine(const Value: Boolean);

  protected
    procedure drawCaption; dynamic;
    procedure drawCaptionTo(aCanvas: TCanvas ); dynamic;
    procedure drawBorders; dynamic;
    procedure paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MultiLine: Boolean read FMultiLine write setMultiLine default False;
  end;





{$ENDREGION}


{$REGION 'TBaseForm'}
type
  TBaseForm = class (TForm)
  private
    FModified: Boolean ;
    FStatus: TCPanelStatus ;
    procedure DoCreateStatus ;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure DoClear(WCtrl: TWinControl); virtual;
    procedure WMNCHitTest(var M: TWMNCHitTest); message WM_NCHITTEST;
    function ChkPwd(const aInput: string): Boolean; virtual ;
  public
    property Modified: Boolean read FModified write FModified;
    procedure DoControlNext;
    procedure DoControlBack;
    procedure DoResetForm; virtual ;
    procedure DoEnabledControls(aWCtrl: TWinControl;
      const aEnabled: Boolean =True);
    constructor Create(AOwner: TComponent); override;
    procedure setStatus(const aCaption: string;
      const aCursor: TCursor =crDefault) ;

    procedure ResetWContrls(aWCtrl: TWinControl; const aReadOnly: Boolean =false);

  public
    class procedure DoClearItems(WCtrl: TWinControl);
  end;

  TFrame = class(Forms.TFrame)
  public
    procedure DoResetFrame; virtual ;

  end;

{$ENDREGION}


implementation

uses
  Themes, Multimon, HelpIntfs ;

const
  BkModeTransparent = TRANSPARENT;


{ TaskDialog based message dialog; requires Windows Vista or later }
type
  TTaskMessageDialog = class(TCustomTaskDialog)
  private
    FHelpFile: string;
    FParentWnd: HWND;
    FPosition: TPoint;
  strict protected
    procedure DoOnButtonClicked(AModalResult: Integer; var CanClose: Boolean); override;
    procedure DoOnDialogCreated; override;
    procedure DoOnHelp; override;
  public
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Position: TPoint read FPosition write FPosition;
  end;


const
  tdbHelp = -1;
  BASE_FONT =9 ;

var
  Captions: array[TMsgDlgType] of string = ('Advertência', 'Erro',
    'Informação', 'Confirmação', '');
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonCaptions: array[TMsgDlgBtn] of string = (
    '&Sim', '&Não', '&OK', '&Cancelar', '&Abortar',
    '&Repetir', '&Ignorar', '&Todos', 'Não / Todos', 'Sim / Todos',
    'A&juda', '&Fechar');

procedure TTaskMessageDialog.DoOnButtonClicked(AModalResult: Integer;
  var CanClose: Boolean);
begin
  if AModalResult = tdbHelp then
  begin
    CanClose := False;
    DoOnHelp;
  end;
end;

procedure TTaskMessageDialog.DoOnDialogCreated;
var
  Rect: TRect;
  LX, LY: Integer;
  LHandle: HMONITOR;
  LMonitorInfo: TMonitorInfo;
begin
  LX := Position.X;
  LY := Position.Y;
  LHandle := MonitorFromWindow(FParentWnd, MONITOR_DEFAULTTONEAREST);
  LMonitorInfo.cbSize := SizeOf(LMonitorInfo);
  if GetMonitorInfo(LHandle, {$IFNDEF CLR}@{$ENDIF}LMonitorInfo) then
    with LMonitorInfo do
    begin
      GetWindowRect(Handle, Rect);
      if LX < 0 then
        LX := ((rcWork.Right - rcWork.Left) - (Rect.Right - Rect.Left)) div 2;
      if LY < 0 then
        LY := ((rcWork.Bottom - rcWork.Top) - (Rect.Bottom - Rect.Top)) div 2;
      Inc(LX, rcWork.Left);
      Inc(LY, rcWork.Top);
      SetWindowPos(Handle, 0, LX, LY, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
    end;
end;

procedure TTaskMessageDialog.DoOnHelp;
var
  LHelpFile: string;
  LHelpSystem: IHelpSystem;
begin
  if HelpContext <> 0 then
  begin
    if FHelpFile = '' then
      LHelpFile := Application.HelpFile
    else
      LHelpFile := HelpFile;
    if HelpIntfs.GetHelpSystem(LHelpSystem) then
    try
      LHelpSystem.Hook(Application.Handle, LHelpFile, HELP_CONTEXT, HelpContext);
    except
      on E: Exception do
        ShowHelpException(E);
    end;
  end;
end;

function TTaskMessageDialog.Execute(ParentWnd: HWND): Boolean;
begin
  FParentWnd := ParentWnd;
  Result := inherited Execute(ParentWnd);
end;


{ TBaseForm }

function TBaseForm.ChkPwd(const aInput: string): Boolean;
var
  dd,mm,yy, sum: Word ;
begin
    DecodeDate(Date, yy, mm, dd);
    sum :=yy mod 2000;
    sum :=sum +mm +dd;
    Result :=(sum *9) =StrToIntDef(aInput, 0) ;
end;

constructor TBaseForm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    DoCreateStatus ;
end;

procedure TBaseForm.DoClear(WCtrl: TWinControl);
var
	I: Integer ;
	W: TWinControl;
	Base: TComponent ;
	C: TComponent;
begin
	if WCtrl = nil then
		Base :=Self
	else
		Base :=WCtrl;

	for I :=0 to Base.ComponentCount -1 do
	begin
		C :=Base.Components[I] ;
		if C is TCustomEdit then
		begin
			TCustomEdit(C).Clear ;
		end;
//    else if C is TCustomEdit then
//    begin
//      TCustomEdit(C).Clear ;
//    end
	end;
end;

class procedure TBaseForm.DoClearItems(WCtrl: TWinControl);
var
	I: Integer ;
	W: TWinControl;
	Base: TComponent ;
	C: TComponent;
begin

	Base :=WCtrl;
	for I :=0 to Base.ComponentCount -1 do
	begin
		C :=Base.Components[I] ;
		if C is TCustomEdit then
		begin
			TCustomEdit(C).Clear ;
		end
		else if C is TCustomComboBox then
		begin
			TCustomComboBox(C).Clear ;
		end
	end;
end;

procedure TBaseForm.DoControlBack;
begin
  if Assigned(ActiveControl) then
    SelectNext(ActiveControl, False, True);
end;

procedure TBaseForm.DoControlNext;
begin
	if Assigned(ActiveControl) then
		SelectNext(ActiveControl, True, True);
end;

procedure TBaseForm.DoCreateStatus;
begin
    FStatus :=TCPanelStatus.Create(Self);
    FStatus.Parent :=Self;
    FStatus.BevelOuter :=bvNone ;
    FStatus.BorderStyle:=bsSingle;
    FStatus.Caption :='';
    FStatus.Color :=clInfoBk; // GradientActiveCaption ;
    FStatus.Font.Name :='Trebuchet MS';
    FStatus.Height :=50 ;
    FStatus.Padding.Left :=3;
    FStatus.Padding.Top :=3;
    FStatus.Padding.Right :=3;
    FStatus.Padding.Bottom :=3;
    FStatus.TabStop :=False ;
    FStatus.Width :=200;
    FStatus.Visible :=False ;
end;

procedure TBaseForm.DoEnabledControls(aWCtrl: TWinControl;
  const aEnabled: Boolean);
var
	I: Integer ;
	Base, W: TWinControl;
	//Base: TComponent ;
	//C: TComponent;
begin
  if aWCtrl <> nil then
    Base :=aWCtrl
  else
    Base :=Self;
	for I :=0 to Base.ControlCount -1 do
	begin
		W :=TWinControl( Base.Controls[I]) ;
		W.Enabled :=aEnabled;
	end;
end;

procedure TBaseForm.DoResetForm;
begin

end;

procedure TBaseForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
    if not ((ActiveControl is TComboBox) and TComboBox(ActiveControl).DroppedDown) then
    begin
      ModalResult := mrCancel;
    end;
    VK_RETURN:
    if Assigned(ActiveControl) then
    begin
      DoControlNext ;
    end;
  else
    inherited;
  end;
//  if Key = VK_ESCAPE then
//  begin
//    if not ((ActiveControl is TComboBox) and TComboBox(ActiveControl).DroppedDown) then
//    begin
//      ModalResult := mrCancel;
//    end;
//  end;
//  inherited;
end;

procedure TBaseForm.Loaded;
begin
	inherited;
//	BorderStyle :=bsDialog;
	KeyPreview:=True;
	Position  :=poOwnerFormCenter;
//	FActionStatus:=asNone ;
	FModified :=False ;
end;


procedure TBaseForm.ResetWContrls(aWCtrl: TWinControl;
  const aReadOnly: Boolean);
var
	I: Integer ;
	W: TWinControl;
begin
  if aWCtrl <> nil then
  begin
      for I :=0 to aWCtrl.ControlCount -1 do
      begin
          W :=TWinControl( aWCtrl.Controls[I] );
          if W is TCustomEdit then
          begin
              TCustomEdit(W).Clear ;
              TCustomEdit(W).ReadOnly :=aReadOnly ;
          end
          else if W is TCustomComboBox then
          begin
              TCustomComboBox(W).ItemIndex :=-1;
          end
      end;
  end;
end;

procedure TBaseForm.setStatus(const aCaption: string;
  const aCursor: TCursor);
var
  L: TStrings ;
  I: Integer;
  S: string ;
begin
    if aCaption <> '' then
    begin
        if Pos(#13, aCaption) > 0 then
        begin
            L :=TStringList.Create ;
            L.Add(aCaption) ;
            S :='';
            for I :=0 to L.Count -1 do
            begin
                if Length(L.Strings[I]) > Length(S) then
                    S:=L.Strings[I];
            end;
        end
        else
            S :=aCaption ;

        if Canvas.TextWidth(S) > FStatus.Width then
        begin
            FStatus.Width :=Canvas.TextWidth(S) ;
        end;
        FStatus.Caption :=aCaption ;
        FStatus.Left :=(Self.Width -FStatus.Width) div  2 ;
        FStatus.Top :=(Self.Height -FStatus.Height) div  2;
        FStatus.BringToFront ;
        FStatus.Show ;
        FStatus.Repaint;
        if Screen.Cursor <> aCursor then
        begin
            Screen.Cursor :=aCursor ;
        end;
    end
    else begin
        FStatus.Caption :='';
        if Screen.Cursor <> crDefault then
        begin
            Screen.Cursor :=crDefault ;
        end;
        FStatus.SendToBack ;
    end;
end;

procedure TBaseForm.WMNCHitTest(var M: TWMNCHitTest);
begin
	inherited; { ativa a herança da mensagem }
		if M.Result = htClient then { o clique foi na área cliente? }
			M.Result := htCaption; { se sim, faz o Windows pensar que foi no Caption. }
end;

{ TCustomComboHlp }

procedure TCustomComboHlp.AddText(const AText: string;
	const AItemIndex: Integer);
begin
	if Pos('","', AText) = 0 then
		Self.AddItem(AText, nil)
	else begin
		Self.Items.CommaText :=AText;
		Self.ItemIndex :=AItemIndex ;
	end;
end;

{ TMaskEdit }

procedure TMaskEdit.DoFormatEditMask(const AFormatEditMask: TFormatEditMask;
	const AShowMessage, AEditMask: string);
begin
	case AFormatEditMask of
		femCPF:
		begin
			Self.EditMask :='000\.000\.000\.-00;0; ';
			Self.MaxLength:=11 ;
		end;
		femCNPJ:
		begin
			Self.EditMask:='00\.000\.000\/0000\-00;0; ';
			Self.MaxLength:=14 ;
		end;
		femCEP:
		begin
			Self.EditMask:='00000\-999;0; ';
			Self.MaxLength:=8 ;
		end;
		femCustom:
		begin
			Self.EditMask:=AEditMask;
			Self.MaxLength:=Length(GetMask) ;
		end
	else
		Self.EditMask :='';
		Self.MaxLength:=0 ;
	end;
	if AShowMessage <> '' then
		FShowMessage :=AShowMessage ;
end;

function TMaskEdit.GetMask: String;
var
	MaskOffset: Integer;
	CType: TMaskCharType;
	FMaskBlank:Char;
	Mask:String;
begin
	FMaskBlank:= MaskGetMaskBlank(Self.EditMask);
	for MaskOffset := 1 to Length(Editmask) do
	begin
		CType := MaskGetCharType(EditMask, MaskOffset);
		case CType of
			mcLiteral, mcIntlLiteral: Mask:=Mask+EditMask[MaskOffset];
			mcMaskOpt,mcMask:Mask:=Mask+FMaskBlank;
		end;
	end;
	result:= Mask;
end;

function TMaskEdit.IsEmpty: Boolean;
begin
	Result :=TMaskEdit.IsEmptyText(Self.EditMask, Self.Text) ;

end;

class function TMaskEdit.IsEmptyText(const AEditMask: TEditMask;
	const AText: String): Boolean;
var
	MaskOffset: Integer;
	CType: TMaskCharType;
	FMaskBlank:Char;
	Mask:String;
	default:boolean;
begin
	default:=true;
	FMaskBlank:= MaskGetMaskBlank(AEditMask);
	for MaskOffset := 1 to Length(AEditMask) do
	begin
		CType := MaskGetCharType(AEditMask, MaskOffset);
		case CType of
			mcLiteral, mcIntlLiteral: Mask:=Mask+AEditMask[MaskOffset];
			mcMaskOpt,mcMask:Mask:=Mask+FMaskBlank;
			mcFieldSeparator:
			begin
				if AEditMask[MaskOffset+1] = '0' then
				begin
					Mask:='';
					default:=false;
				end;
				Break;
			end;
		end;
	end;
	if default then
		Mask:= FormatMaskText(AEditMask,'');
	Result:=AText = Mask;
end;

function TMaskEdit.Validate(const Value: string; var Pos: Integer): Boolean;
var
	CType: TMaskCharType;
	Offset, MaskOffset: Integer;
	FMaskBlank:Char;
	Mask:String;
begin
	Result :=True;
	Offset := 1;
	Mask:= GetMask;
	if Value = Mask then
		exit;

	Offset := 1;
	for MaskOffset := 1 to Length(EditMask) do
	begin
		FMaskBlank:= MaskGetMaskBlank(Self.EditMask);
		CType := MaskGetCharType(EditMask, MaskOffset);

		if CType in [mcLiteral, mcIntlLiteral, mcMaskOpt] then
			Inc(Offset)
		else
			if (CType = mcMask) and (Value <> '') then
			begin
				if(Value [Offset] = FMaskBlank) or
					((Value [Offset] = ' ') and (EditMask[MaskOffset] <> mMskAscii)) then
				begin
					Result := False;
					Pos := Offset - 1;
					Dialogs.ShowMessage(FShowMessage);
					Self.SetFocus;
					Abort;
				end;
				Inc(Offset);
			end;
	end;
end;

{ TCustomEditHlp }

procedure TCustomEditHlp.DoClear;
begin
	Self.Text :='';

end;

procedure TCustomEditHlp.DoFormat(const AFormat: string;
	const Args: array of const);
begin
	Self.Text :=Format(AFormat, Args) ;

end;

function TCustomEditHlp.IsEmpty(const AMsg: string): Boolean;
begin
	Result :=Self.Text = '';
	if Result and (AMsg <> '') then
	begin
		MessageDlg(AMsg, mtWarning, [mbOk], 0); //  TMensagemDlg.Adverte(AMsg);
		Self.SetFocus ;
	end;
end;


{ TFrame }

procedure TFrame.DoResetFrame;
begin

end;



{ TCPanelStatus }

constructor TCPanelStatus.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    ControlStyle := ControlStyle - [csSetCaption];
    fMultiLine := True;

end;

destructor TCPanelStatus.Destroy;
begin

    inherited Destroy;
end;

procedure TCPanelStatus.drawBorders;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then
      BottomColor := clBtnHighlight;
  end;

begin
  Rect := ClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
end;

procedure TCPanelStatus.drawCaption;
begin
    drawCaptionTo(Self.Canvas)
    ;
end;

procedure TCPanelStatus.drawCaptionTo(aCanvas: TCanvas);
const
  Alignments: array [TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWrap: array [Boolean] of Longint = (DT_SINGLELINE, DT_WORDBREAK);
var
  ATextRect: TRect;
  BevelSize: Integer;
  Flags: Longint;
begin
    if Caption <> '' then
    begin
      aCanvas.Font := Self.Font;

      SetBkMode(Handle, BkModeTransparent);
      aCanvas.Font := Self.Font;
      ATextRect := GetClientRect;
      InflateRect(ATextRect, -BorderWidth, -BorderWidth);
      BevelSize := 0;
      if BevelOuter <> bvNone then
        Inc(BevelSize, BevelWidth);
      if BevelInner <> bvNone then
        Inc(BevelSize, BevelWidth);
      InflateRect(ATextRect, -BevelSize, -BevelSize);
      Flags := DT_EXPANDTABS or WordWrap[MultiLine] or Alignments[Alignment];
      Flags := DrawTextBiDiModeFlags(Flags);
      //
      // calculate required rectangle size
      DrawText(aCanvas.Handle, Caption, -1, ATextRect, Flags or DT_CALCRECT);

      //
      // adjust the rectangle placement
      OffsetRect(ATextRect, 0, -ATextRect.Top + (Height - (ATextRect.Bottom - ATextRect.Top)) div 2);
      case Alignment of
        taRightJustify:
          OffsetRect(ATextRect, -ATextRect.Left + (Width - (ATextRect.Right - ATextRect.Left) - BorderWidth -
            BevelSize), 0);
        taCenter:
          OffsetRect(ATextRect, -ATextRect.Left + (Width - (ATextRect.Right - ATextRect.Left)) div 2, 0);
      end;
      if not Enabled then
        Font.Color := clGrayText;
      //draw text
      DrawText(aCanvas.Handle, Caption, -1, ATextRect, Flags);
    end;
end;

procedure TCPanelStatus.paint;
begin
    Inc(FPainting);
    try
      Canvas.Font := Self.Font;
      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsClear;

      drawBorders;
      drawCaption;

    finally
      Dec(FPainting);
    end;
end;

procedure TCPanelStatus.setMultiLine(const Value: Boolean);
begin
    if fMultiLine <> Value then
    begin
        fMultiLine := Value;
        Invalidate;
    end;
end;

end.
