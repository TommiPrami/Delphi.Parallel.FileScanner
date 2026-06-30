object DPFSMainForm: TDPFSMainForm
  Left = 0
  Top = 0
  Caption = 'Delphi Parallel FileScanner Simple Test App'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    624
    441)
  TextHeight = 15
  object ButtonParallelScan: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 618
    Height = 25
    Align = alTop
    Caption = 'Default Parallel Scan'
    Hint = 'Scan directories in parallel and collect the results in a standard RTL TStringList.'
    TabOrder = 0
    OnClick = ButtonParallelScanClick
  end
  object ButtonParallelScanSpring: TButton
    AlignWithMargins = True
    Left = 8
    Top = 34
    Width = 608
    Height = 25
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'Spring container parallel scan'
    Hint = 'Scan directories in parallel and collect the results in a Spring4D IList<string>.'
    TabOrder = 1
    OnClick = ButtonParallelScanSpringClick
  end
  object MemoLog: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 120
    Width = 608
    Height = 321
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'MemoLog')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ButtonOtlQueue: TButton
    AlignWithMargins = True
    Left = 8
    Top = 65
    Width = 608
    Height = 25
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'OTL Queue parallel scan'
    Hint = 'Scan directories in parallel, streaming the results through an OmniThreadLibrary value queue.'
    TabOrder = 3
    OnClick = ButtonOtlQueueClick
  end
  object CheckBoxConvertRelativePathsToAbsolute: TCheckBox
    AlignWithMargins = True
    Left = 8
    Top = 96
    Width = 608
    Height = 17
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'Convert Relative Paths To Absolute'
    TabOrder = 4
  end
end
