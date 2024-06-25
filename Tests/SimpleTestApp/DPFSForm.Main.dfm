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
    Caption = 'ButtonParallelScan'
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
    Caption = 'ButtonParallelScanSpring'
    TabOrder = 1
    OnClick = ButtonParallelScanSpringClick
  end
  object MemoLog: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 80
    Width = 608
    Height = 353
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'MemoLog')
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
