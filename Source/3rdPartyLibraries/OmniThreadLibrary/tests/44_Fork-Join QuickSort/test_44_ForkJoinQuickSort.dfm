object frmQuickSortDemo: TfrmQuickSortDemo
  Left = 0
  Top = 0
  Caption = 'Parallel QuickSort'
  ClientHeight = 286
  ClientWidth = 596
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnSortOnOne: TButton
    Left = 8
    Top = 8
    Width = 106
    Height = 25
    Caption = 'Sequential sort'
    TabOrder = 0
    OnClick = btnSortOnOneClick
  end
  object lbLog: TListBox
    Left = 120
    Top = 0
    Width = 476
    Height = 286
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object btnSortOnAll: TButton
    Left = 8
    Top = 39
    Width = 106
    Height = 25
    Caption = 'Parallel sort'
    TabOrder = 2
    OnClick = btnSortOnAllClick
  end
end
