object Form1: TForm1
  Left = 94
  Top = 153
  Width = 696
  Height = 480
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 8
    Top = 8
    Width = 352
    Height = 288
  end
  object Button1: TButton
    Left = 368
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Connect/Disconnect'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Notices: TMemo
    Left = 8
    Top = 304
    Width = 353
    Height = 89
    TabOrder = 1
  end
end
