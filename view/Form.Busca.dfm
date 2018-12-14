object frm_Busca: Tfrm_Busca
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'frm_Busca'
  ClientHeight = 456
  ClientWidth = 634
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    634
    456)
  PixelsPerInch = 96
  TextHeight = 14
  object pnl_Footer: TJvFooter
    Left = 0
    Top = 416
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
    ExplicitLeft = 5
    DesignSize = (
      634
      40)
    object btn_OK: TJvFooterBtn
      Left = 422
      Top = 5
      Width = 100
      Height = 30
      Anchors = [akRight, akBottom]
      Caption = 'OK'
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
  object DBTreeGrid: TVirtualStringTree
    Left = 5
    Top = 5
    Width = 621
    Height = 405
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    Header.AutoSizeIndex = -1
    Header.Background = clBtnHighlight
    Header.Height = 21
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible]
    Header.ParentFont = True
    ParentFont = False
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toHotTrack, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toDisableDrawSelection, toExtendedFocus, toMiddleClickSelect, toRightClickSelect, toCenterScrollIntoView]
    OnChange = DBTreeGridChange
    OnGetText = DBTreeGridGetText
    Columns = <>
  end
  object ds_Grid: TDataSource
    Left = 207
    Top = 183
  end
  object tm_Refresh: TTimer
    OnTimer = tm_RefreshTimer
    Left = 24
    Top = 8
  end
end
