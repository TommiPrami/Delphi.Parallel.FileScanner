object DPFSMainForm: TDPFSMainForm
  Left = 0
  Top = 0
  Caption = 'Delphi Parallel FileScanner Simple Test App'
  ClientHeight = 472
  ClientWidth = 932
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    932
    472)
  TextHeight = 15
  object ComboBoxDirectories: TComboBox
    AlignWithMargins = True
    Left = 8
    Top = 3
    Width = 916
    Height = 23
    Hint = 'Semicolon-separated list of root directories to scan (editable).'
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    TabOrder = 5
    Text = '..\..\..\..\Source\;..\..\..\..\Tests\'
    Items.Strings = (
      '..\..\..\..\Source\;..\..\..\..\Tests\'
      'C:\Program Files;C:\Program Files (x86)'
      'C:\Windows\System32'
      'C:\Windows'
      'C:\Users')
  end
  object ButtonParallelScan: TButton
    AlignWithMargins = True
    Left = 3
    Top = 32
    Width = 926
    Height = 25
    Hint =
      'Scan directories in parallel on the RTL PPL (TParallelFileScanne' +
      'r) and collect the results in a standard RTL TStringList.'
    Align = alTop
    Caption = 'RTL PPL parallel scan'
    TabOrder = 0
    OnClick = ButtonParallelScanClick
  end
  object ButtonParallelScanOtl: TButton
    AlignWithMargins = True
    Left = 8
    Top = 63
    Width = 916
    Height = 25
    Hint =
      'Scan directories in parallel on OmniThreadLibrary (TParallelFile' +
      'ScannerOTL) and collect the results in a standard RTL TStringLis' +
      't.'
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'OTL parallel scan'
    TabOrder = 7
    OnClick = ButtonParallelScanOtlClick
  end
  object ButtonParallelScanSpring: TButton
    AlignWithMargins = True
    Left = 8
    Top = 94
    Width = 916
    Height = 25
    Hint =
      'Scan directories in parallel on the RTL PPL (TParallelFileScanne' +
      'rSpring) and collect the results in a Spring4D IList<string>.'
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'Spring container parallel scan'
    TabOrder = 1
    OnClick = ButtonParallelScanSpringClick
  end
  object MemoLog: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 220
    Width = 916
    Height = 249
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ButtonOtlQueue: TButton
    AlignWithMargins = True
    Left = 8
    Top = 125
    Width = 916
    Height = 25
    Hint =
      'Scan directories in parallel on OmniThreadLibrary (TParallelFile' +
      'ScannerOTL), streaming the results through an OTL value queue.'
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'OTL Queue parallel scan'
    TabOrder = 3
    OnClick = ButtonOtlQueueClick
  end
  object CheckBoxConvertRelativePathsToAbsolute: TCheckBox
    AlignWithMargins = True
    Left = 8
    Top = 197
    Width = 916
    Height = 17
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'Convert Relative Paths To Absolute'
    TabOrder = 4
  end
  object PanelSpeedTest: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 156
    Width = 926
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 6
    DesignSize = (
      926
      35)
    object ButtonSpeedTest: TButton
      AlignWithMargins = True
      Left = 11
      Top = 4
      Width = 778
      Height = 25
      Hint = 
        'Run each available scan many times and report the fastest, avera' +
        'ge and median time.'
      Margins.Left = 8
      Margins.Right = 8
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Speed test (run all scans, warm-up + n timed runs)'
      TabOrder = 0
      OnClick = ButtonSpeedTestClick
    end
    object EditLoopCount: TEdit
      Left = 800
      Top = 5
      Width = 121
      Height = 23
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = '100'
    end
  end
end
