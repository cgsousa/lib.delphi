object frm_ParametroList: Tfrm_ParametroList
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '.:Parametros do Sistema:.'
  ClientHeight = 572
  ClientWidth = 794
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    794
    572)
  PixelsPerInch = 96
  TextHeight = 14
  object htm_Status: THTMLabel
    Left = 5
    Top = 502
    Width = 781
    Height = 25
    ColorTo = 11769496
    BorderWidth = 1
    BorderStyle = bsSingle
    Color = 15524577
    HTMLText.Strings = (
      '<P align="left">TMS <b>STATUS</b> label</P>')
    ParentColor = False
    Transparent = False
    Version = '1.9.0.2'
  end
  object pnl_Footer: TJvFooter
    Left = 0
    Top = 532
    Width = 794
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
      794
      40)
    object btn_Altera: TJvFooterBtn
      Left = 582
      Top = 5
      Width = 100
      Height = 30
      Anchors = [akRight, akBottom]
      Caption = 'Alterar'
      TabOrder = 2
      OnClick = btn_AlteraClick
      ButtonIndex = 1
      SpaceInterval = 6
    end
    object btn_Close: TJvFooterBtn
      Left = 686
      Top = 5
      Width = 100
      Height = 30
      Anchors = [akRight, akBottom]
      Caption = 'Fechar'
      TabOrder = 0
      OnClick = btn_CloseClick
      ButtonIndex = 2
      SpaceInterval = 6
    end
    object cbx_Catego: TAdvComboBox
      Left = 63
      Top = 8
      Width = 145
      Height = 24
      Color = clWindow
      Version = '1.5.1.0'
      Visible = True
      Style = csDropDownList
      EmptyTextStyle = []
      DropWidth = 0
      Enabled = True
      ItemIndex = -1
      LabelCaption = 'Categoria:'
      LabelTransparent = True
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'Tahoma'
      LabelFont.Style = []
      TabOrder = 1
      OnSelect = cbx_CategoSelect
    end
  end
  object vst_Grid1: TVirtualStringTree
    Left = 5
    Top = 5
    Width = 781
    Height = 490
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvNone
    BevelKind = bkTile
    BorderStyle = bsNone
    Colors.DropTargetColor = 7063465
    Colors.DropTargetBorderColor = 4958089
    Colors.FocusedSelectionColor = clActiveCaption
    Colors.FocusedSelectionBorderColor = clActiveCaption
    Colors.GridLineColor = clBtnShadow
    Colors.UnfocusedSelectionBorderColor = clBtnShadow
    DefaultNodeHeight = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    Header.AutoSizeIndex = 0
    Header.Background = clBtnHighlight
    Header.Height = 21
    Header.Options = [hoColumnResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible]
    Header.ParentFont = True
    Header.Style = hsPlates
    ParentFont = False
    RootNodeCount = 30
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toHotTrack, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toRightClickSelect, toCenterScrollIntoView]
    OnChange = vst_Grid1Change
    OnGetText = vst_Grid1GetText
    Columns = <
      item
        Color = 15000804
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus]
        Position = 0
        Width = 250
        WideText = 'Id'
      end
      item
        Position = 1
        Width = 375
        WideText = 'Valor'
      end
      item
        Position = 2
        Width = 121
        WideText = 'Categoria'
      end>
  end
end
