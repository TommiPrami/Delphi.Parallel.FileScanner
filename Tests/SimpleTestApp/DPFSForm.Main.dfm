object DPFSMainForm: TDPFSMainForm
  Left = 0
  Top = 0
  Caption = 'Delphi Parallel FileScanner Simple Test App'
  ClientHeight = 441
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
    441)
  TextHeight = 15
  object ButtonParallelScan: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 926
    Height = 25
    Hint = 
      'Scan directories in parallel and collect the results in a standa' +
      'rd RTL TStringList.'
    Align = alTop
    Caption = 'Default Parallel Scan'
    TabOrder = 0
    OnClick = ButtonParallelScanClick
  end
  object ButtonParallelScanSpring: TButton
    AlignWithMargins = True
    Left = 8
    Top = 34
    Width = 916
    Height = 25
    Hint = 
      'Scan directories in parallel and collect the results in a Spring' +
      '4D IList<string>.'
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
    Top = 158
    Width = 916
    Height = 280
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
    Top = 65
    Width = 916
    Height = 25
    Hint = 
      'Scan directories in parallel, streaming the results through an O' +
      'mniThreadLibrary value queue.'
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'OTL Queue parallel scan'
    TabOrder = 3
    OnClick = ButtonOtlQueueClick
  end
  object ButtonSpeedTest: TButton
    AlignWithMargins = True
    Left = 8
    Top = 96
    Width = 916
    Height = 25
    Hint = 
      'Run each available scan many times and report the fastest, avera' +
      'ge and median time.'
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'Speed test (run all scans, warm-up + 100 timed runs)'
    TabOrder = 5
    OnClick = ButtonSpeedTestClick
  end
  object CheckBoxConvertRelativePathsToAbsolute: TCheckBox
    AlignWithMargins = True
    Left = 8
    Top = 127
    Width = 916
    Height = 17
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'Convert Relative Paths To Absolute'
    TabOrder = 4
  end
end
