object DPAUMainForm: TDPAUMainForm
  Left = 0
  Top = 0
  Caption = 'Delphi Process Affinity Utils Demo'
  ClientHeight = 441
  ClientWidth = 824
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PanelButtons: TPanel
    Left = 0
    Top = 0
    Width = 824
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object ButtonRefresh: TButton
      Left = 8
      Top = 8
      Width = 90
      Height = 25
      Caption = 'Refresh masks'
      TabOrder = 0
      OnClick = ButtonRefreshClick
    end
    object ButtonUsePerformanceCores: TButton
      Left = 104
      Top = 8
      Width = 140
      Height = 25
      Caption = 'Use performance cores'
      TabOrder = 1
      OnClick = ButtonUsePerformanceCoresClick
    end
    object ButtonUseEfficiencyCores: TButton
      Left = 250
      Top = 8
      Width = 140
      Height = 25
      Caption = 'Use efficiency cores'
      TabOrder = 2
      OnClick = ButtonUseEfficiencyCoresClick
    end
    object ButtonRestore: TButton
      Left = 396
      Top = 8
      Width = 110
      Height = 25
      Caption = 'Restore full mask'
      TabOrder = 3
      OnClick = ButtonRestoreClick
    end
    object CheckBoxForce: TCheckBox
      Left = 516
      Top = 12
      Width = 100
      Height = 17
      Hint = 
        'Query the hybrid core masks even if the process affinity mask ha' +
        's already been modified'
      Caption = 'Force query'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 4
    end
    object ButtonClearLog: TButton
      AlignWithMargins = True
      Left = 746
      Top = 8
      Width = 75
      Height = 25
      Margins.Top = 8
      Margins.Bottom = 8
      Align = alRight
      Caption = 'Clear log'
      TabOrder = 5
      OnClick = ButtonClearLogClick
    end
  end
  object MemoLog: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 815
    Height = 394
    Margins.Right = 6
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end
