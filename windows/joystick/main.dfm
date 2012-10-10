object MainForm: TMainForm
  Left = 302
  Top = 212
  Width = 346
  Height = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 84
    Top = 48
    Width = 69
    Height = 22
  end
  object SpeedButton2: TSpeedButton
    Left = 8
    Top = 48
    Width = 69
    Height = 22
  end
  object SpeedButton3: TSpeedButton
    Left = 160
    Top = 48
    Width = 69
    Height = 22
  end
  object SpeedButton4: TSpeedButton
    Left = 236
    Top = 48
    Width = 69
    Height = 22
  end
  object XLabeledEdit: TLabeledEdit
    Left = 24
    Top = 12
    Width = 65
    Height = 21
    EditLabel.Width = 7
    EditLabel.Height = 13
    EditLabel.Caption = 'X'
    LabelPosition = lpLeft
    LabelSpacing = 10
    TabOrder = 0
  end
  object YLabeledEdit: TLabeledEdit
    Left = 108
    Top = 12
    Width = 65
    Height = 21
    EditLabel.Width = 7
    EditLabel.Height = 13
    EditLabel.Caption = 'Y'
    LabelPosition = lpLeft
    LabelSpacing = 10
    TabOrder = 1
  end
  object ZLabeledEdit: TLabeledEdit
    Left = 200
    Top = 12
    Width = 65
    Height = 21
    EditLabel.Width = 7
    EditLabel.Height = 13
    EditLabel.Caption = 'Z'
    LabelPosition = lpLeft
    LabelSpacing = 10
    TabOrder = 2
  end
  object LogMemo: TMemo
    Left = 8
    Top = 136
    Width = 281
    Height = 101
    TabOrder = 3
  end
  object JoyIndexEdit: TEdit
    Left = 12
    Top = 100
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 224
    Top = 80
  end
end
