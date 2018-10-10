object frm_Parametro: Tfrm_Parametro
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Modifica valor do Parametro'
  ClientHeight = 326
  ClientWidth = 634
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object JvGradient1: TJvGradient
    Left = 0
    Top = 0
    Width = 634
    Height = 286
    StartColor = clBtnFace
    EndColor = clSilver
    ExplicitTop = -1
  end
  object pnl_Footer: TJvFooter
    Left = 0
    Top = 286
    Width = 634
    Height = 40
    Align = alBottom
    BevelStyle = bsRaised
    BevelVisible = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    ParentFont = False
    DesignSize = (
      634
      40)
    object btn_OK: TJvFooterBtn
      Left = 422
      Top = 5
      Width = 100
      Height = 30
      Anchors = [akRight, akBottom]
      Caption = 'Gravar'
      TabOrder = 1
      OnClick = btn_OKClick
      ButtonIndex = 0
      SpaceInterval = 6
    end
    object btn_Close: TJvFooterBtn
      Left = 526
      Top = 5
      Width = 100
      Height = 30
      Anchors = [akRight, akBottom]
      Caption = 'Cancelar'
      TabOrder = 0
      OnClick = btn_CloseClick
      ButtonIndex = 1
      SpaceInterval = 6
    end
  end
  object AdvGroupBox1: TAdvGroupBox
    Left = 32
    Top = 8
    Width = 594
    Height = 50
    Caption = ' ID'
    TabOrder = 0
    object edt_ID: TAdvEdit
      Left = 48
      Top = 18
      Width = 529
      Height = 22
      EmptyTextStyle = []
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      Lookup.Font.Charset = DEFAULT_CHARSET
      Lookup.Font.Color = clWindowText
      Lookup.Font.Height = -11
      Lookup.Font.Name = 'Arial'
      Lookup.Font.Style = []
      Lookup.Separator = ';'
      Color = clWindow
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      Text = 'edt_ID'
      Visible = True
      Version = '3.3.2.0'
    end
  end
  object AdvGroupBox2: TAdvGroupBox
    Left = 32
    Top = 64
    Width = 594
    Height = 89
    Caption = ' Valor '
    TabOrder = 1
    object txt_ValStr: TJvMemo
      Left = 48
      Top = 18
      Width = 529
      Height = 55
      Cursor = crIBeam
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'COURIER NEW'
      Font.Style = []
      Lines.Strings = (
        'LINHA 01                                '
        'LINHA 02                                             '
        'LINHA 03                    ')
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object edt_ValXXX: TAdvEdit
      Left = 48
      Top = 18
      Width = 529
      Height = 20
      EmptyTextStyle = []
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      Lookup.Font.Charset = DEFAULT_CHARSET
      Lookup.Font.Color = clWindowText
      Lookup.Font.Height = -11
      Lookup.Font.Name = 'Arial'
      Lookup.Font.Style = []
      Lookup.Separator = ';'
      Color = clWindow
      TabOrder = 0
      Visible = True
      Version = '3.3.2.0'
    end
    object cbx_ValXXX: TAdvComboBox
      Left = 48
      Top = 38
      Width = 529
      Height = 22
      Color = clWindow
      Version = '1.5.1.0'
      Visible = True
      Style = csDropDownList
      EmptyTextStyle = []
      DropWidth = 0
      Enabled = True
      ItemIndex = -1
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      TabOrder = 2
    end
  end
  object AdvGroupBox3: TAdvGroupBox
    Left = 32
    Top = 159
    Width = 594
    Height = 106
    Caption = ' Descri'#231#227'o '
    TabOrder = 2
    object txt_Descricao: TJvMemo
      Left = 48
      Top = 18
      Width = 529
      Height = 71
      Cursor = crIBeam
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'COURIER NEW'
      Font.Style = []
      Lines.Strings = (
        'LINHA 01'
        'LINHA 02'
        'LINHA 03'
        'LINHA 04')
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
