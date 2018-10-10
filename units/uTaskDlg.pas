unit uTaskDlg;

{$DEFINE TMSPACK}

interface

uses Controls, Forms, Dialogs
  {$IFDEF TMSPACK},TaskDialog{$ENDIF}
  ;

type
  CMsgDlg = class
  private
  public
    class procedure Info(const AMsg: string); overload;
    class procedure Info(const AMsg: string; Args: array of const); overload;
    class function Confirm(const AMsg: string): Boolean ; overload;
//    class function Confirm(const AMsg: string;
//      Args: array of const): Boolean ; overload;
    class function Warning(const AMsg: string;
      const AConfirm: Boolean = False): Boolean ;

    class procedure Error(const AMsg: string); overload ;
    class procedure Error(const AMsg: string; Args: array of const); overload;
  end;


function InputQueryDlg(ACaption, APrompt: string; var Value: string): boolean;


implementation

uses SysUtils;

var
  Captions: array[TMsgDlgType] of string = ('Advertência', 'Erro',
    'Informação', 'Confirmação', '');
//  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
//    IDI_ASTERISK, IDI_QUESTION, nil);
  ButtonCaptions: array[TMsgDlgBtn] of string = (
    '&Sim', '&Não', '&OK', '&Cancelar', '&Abortar',
    '&Repetir', '&Ignorar', '&Todos', 'Não / Todos', 'Sim / Todos',
    'A&juda', '&Fechar');


function AdvTaskMessageDlgPosHelp(const Title, Msg: string;
  const DlgType: TMsgDlgType;
  const Buttons: TMsgDlgButtons = [mbOK];
  const DefaultButton: TMsgDlgBtn = mbOK;
  const HelpCtx: Longint = 0;
  const HelpFileName: string = '';
  const X: Integer = -1;
  const Y: Integer = -1): Integer;
var
  td:  TAdvTaskDialog;
  ray: array[0..3] of integer;
  res: integer;
  p: TCustomForm;
begin

    p := nil;
    if Assigned(screen.ActiveControl) then
    {$IFDEF DELPHI9_LVL}
      p := GetParentForm(screen.ActiveControl, True);
    {$ENDIF}
    {$IFNDEF DELPHI9_LVL}
      p := GetParentForm(screen.ActiveControl);
    {$ENDIF}

    if Assigned(p) then
      td := TAdvTaskDialog.Create(p)
    else
      td := TAdvTaskDialog.Create(Application);

    try
      td.ApplicationIsParent := not Assigned(p);
      td.Instruction := Title;
      td.Content:= msg;
      if DlgType <> mtCustom then
          td.Title  :=Captions[DlgType]
      else
          td.Title  :=Application.Title;

      case DlgType of
        mtWarning:
          begin
            td.Icon := tiWarning;
//            td.Title := SMsgDlgWarning;
          end;
        mtError:
          begin
            td.Icon := tiError;
//            td.Title := SMsgDlgError;
          end;
        mtInformation:
          begin
            td.Icon := tiInformation;
//            td.Title := SMsgDlgInformation;
          end;
        mtConfirmation:
          begin
            td.Icon := tiQuestion;
//            td.Title := SMsgDlgConfirm;
          end;
      end;

      fillchar(ray,sizeof(ray),0);
      td.CommonButtons := [];

    //  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAdvrt, mbRetry, mbIgnore,
    //    mbAll, mbNoToAll, mbYesToAll, mbHelp);

      if (mbYes in Buttons) then
        td.CommonButtons := td.CommonButtons + [cbYes];

      if (mbNo in Buttons) then
        td.CommonButtons := td.CommonButtons + [cbNo];

      if (mbOK in Buttons) then
        td.CommonButtons := td.CommonButtons + [cbOK];

      if (mbCancel in Buttons) then
        td.CommonButtons := td.CommonButtons + [cbCancel];

      if (mbAbort in Buttons) then
        td.CommonButtons := td.CommonButtons + [cbClose];

      if (mbRetry in Buttons) then
        td.CommonButtons := td.CommonButtons + [cbRetry];


      if (mbIgnore in Buttons) then
      begin
//        td.CustomButtons.Add(SMsgDlgIgnore);
        ray[0] := mrIgnore;
      end;

      if (mbAll in Buttons) then
      begin
        ray[td.custombuttons.Count] := mrALL;
//        td.CustomButtons.Add(SMsgDlgAll);
      end;

      if (mbNoToAll in buttons) then
      begin
        ray[td.custombuttons.Count] := mrNoToAll;
//        td.CustomButtons.add(SMsgDlgNoToAll);
      end;

      if (mbYesToAll in buttons) then
      begin
        ray[td.custombuttons.Count] := mrYesToAll;
//        td.Custombuttons.Add(SMsgDlgYesToAll);
      end;

      if (mbHelp in buttons) then
      begin
        ray[td.Custombuttons.Count] := mrNone;
//        td.Custombuttons.Add(SMsgDlgHelp);
      end;

      case DefaultButton of
      mbYes: td.DefaultButton := integer(mrYes);
      mbNo: td.DefaultButton := integer(mrNo);
      mbCancel: td.DefaultButton := integer(mrCancel);
      mbOK: td.DefaultButton := integer(mrOK);
      mbAbort: td.DefaultButton := integer(mrAbort);
      mbRetry: td.DefaultButton := integer(mrRetry);
      mbIgnore: td.DefaultButton := integer(mrIgnore);
      end;

      td.HelpContext := HelpCtx;
      td.Options := td.Options + [doAllowDialogCancel];

      Result := 0;
      res := td.Execute;
      case res of
      1: Result := mrOk;
      2: Result := mrCancel;
      3: Result := mrAbort;
      4: Result := mrRetry;
      6: Result := mrYes;
      7: Result := mrNo;
      else
        if (res > 99) and (res < 100+high(ray)) then
        begin
          result := ray[res-100];

          if (Result = mrNone) and (td.HelpContext > 0) then
          begin
            Application.HelpContext(td.HelpContext);
          end;
        end;
      end;
    finally
      td.Free;
    end;

end;


function InputQueryDlg(ACaption, APrompt: string; var Value: string): boolean;
begin
    Result :=AdvInputQueryDlg(ACaption, APrompt, Value) ;

end;

{ TCMsgDlg }

class function CMsgDlg.Confirm(const AMsg: string): Boolean;
begin
    {$IFDEF TMSPACK}
    Result :=AdvTaskMessageDlgPosHelp(Captions[mtConfirmation], AMsg, mtConfirmation,
    mbYesNo, mbNo) = mrYes;
    {$ENDIF}
end;

class procedure CMsgDlg.Error(const AMsg: string);
begin
    {$IFDEF TMSPACK}
    AdvTaskMessageDlgPosHelp(Captions[mtError], AMsg, mtError);
    {$ENDIF}
end;

class procedure CMsgDlg.Error(const AMsg: string; Args: array of const);
begin
    {$IFDEF TMSPACK}
    CMsgDlg.Error(Format(AMsg, Args));
    {$ENDIF}
end;


class procedure CMsgDlg.Info(const AMsg: string);
begin
    {$IFDEF TMSPACK}
    AdvTaskMessageDlgPosHelp(Captions[mtInformation], AMsg, mtInformation);
    {$ENDIF}
end;

class procedure CMsgDlg.Info(const AMsg: string; Args: array of const);
begin
    {$IFDEF TMSPACK}
    CMsgDlg.Info(Format(AMsg, Args));
    {$ENDIF}
end;

class function CMsgDlg.Warning(const AMsg: string;
  const AConfirm: Boolean): Boolean;
var
  title: string;
begin
    title :=Captions[mtWarning] ;
    {$IFDEF TMSPACK}
    if AConfirm then
        Result :=AdvTaskMessageDlgPosHelp(title, AMsg, mtWarning,
        mbYesNo, mbNo) = mrYes
    else
        Result :=AdvTaskMessageDlgPosHelp(title, AMsg, mtWarning) = mrOk;
    {$ENDIF}
end;

end.
