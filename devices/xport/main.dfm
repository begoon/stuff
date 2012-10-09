object MainForm: TMainForm
  Left = 192
  Top = 114
  Width = 867
  Height = 799
  Caption = 'XPort LEDs'
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
  object ComLed1: TComLed
    Left = 32
    Top = 40
    Width = 25
    Height = 25
    LedSignal = lsConn
    Kind = lkGreenLight
  end
  object ComLed2: TComLed
    Left = 32
    Top = 72
    Width = 25
    Height = 25
    LedSignal = lsConn
    Kind = lkGreenLight
  end
  object ComLed3: TComLed
    Left = 32
    Top = 104
    Width = 25
    Height = 25
    LedSignal = lsConn
    Kind = lkGreenLight
  end
  object Led1OutSpeedButton: TSpeedButton
    Left = 120
    Top = 40
    Width = 25
    Height = 22
    GroupIndex = 1
    Down = True
    Caption = 'Out'
    OnClick = Led1OutSpeedButtonClick
  end
  object Led1InSpeedButton: TSpeedButton
    Left = 152
    Top = 40
    Width = 25
    Height = 22
    GroupIndex = 1
    Caption = 'In'
    OnClick = Led1InSpeedButtonClick
  end
  object Led2OutSpeedButton: TSpeedButton
    Left = 120
    Top = 72
    Width = 25
    Height = 22
    GroupIndex = 2
    Down = True
    Caption = 'Out'
    OnClick = Led2OutSpeedButtonClick
  end
  object Led2InSpeedButton: TSpeedButton
    Left = 152
    Top = 72
    Width = 23
    Height = 22
    GroupIndex = 2
    Caption = 'In'
    OnClick = Led2InSpeedButtonClick
  end
  object Led3OutSpeedButton: TSpeedButton
    Left = 120
    Top = 104
    Width = 23
    Height = 22
    GroupIndex = 3
    Down = True
    Caption = 'Out'
    OnClick = Led3OutSpeedButtonClick
  end
  object Led3InSpeedButton: TSpeedButton
    Left = 152
    Top = 104
    Width = 23
    Height = 22
    GroupIndex = 3
    Caption = 'In'
    OnClick = Led3InSpeedButtonClick
  end
  object Led1GetButton: TButton
    Left = 64
    Top = 40
    Width = 41
    Height = 23
    Caption = 'Get'
    TabOrder = 0
    OnClick = Led1GetButtonClick
  end
  object Led2GetButton: TButton
    Left = 64
    Top = 72
    Width = 41
    Height = 23
    Caption = 'Get'
    TabOrder = 1
    OnClick = Led2GetButtonClick
  end
  object Led3GetButton: TButton
    Left = 64
    Top = 104
    Width = 41
    Height = 23
    Caption = 'Get'
    TabOrder = 2
    OnClick = Led3GetButtonClick
  end
  object Led11Button: TButton
    Left = 192
    Top = 40
    Width = 17
    Height = 23
    Caption = '1'
    TabOrder = 3
    OnClick = Led11ButtonClick
  end
  object Led10Button: TButton
    Left = 216
    Top = 40
    Width = 17
    Height = 23
    Caption = '0'
    TabOrder = 4
    OnClick = Led10ButtonClick
  end
  object Led21Button: TButton
    Left = 192
    Top = 72
    Width = 17
    Height = 23
    Caption = '1'
    TabOrder = 5
    OnClick = Led21ButtonClick
  end
  object Led20Button: TButton
    Left = 216
    Top = 72
    Width = 17
    Height = 23
    Caption = '0'
    TabOrder = 6
    OnClick = Led20ButtonClick
  end
  object Led31Button: TButton
    Left = 192
    Top = 104
    Width = 17
    Height = 23
    Caption = '1'
    TabOrder = 7
    OnClick = Led31ButtonClick
  end
  object Led30Button: TButton
    Left = 216
    Top = 104
    Width = 17
    Height = 23
    Caption = '0'
    TabOrder = 8
    OnClick = Led30ButtonClick
  end
  object XPortIPLabeledEdit: TLabeledEdit
    Left = 80
    Top = 8
    Width = 153
    Height = 21
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'XPort IP'
    LabelPosition = lpLeft
    LabelSpacing = 5
    TabOrder = 9
    Text = '127.0.0.1'
    OnChange = XPortIPLabeledEditChange
  end
  object CppWebBrowser: TCppWebBrowser
    Left = 32
    Top = 144
    Width = 800
    Height = 600
    TabOrder = 10
    ControlData = {
      4C000000AF520000033E00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object CameraURLLabeledEdit: TLabeledEdit
    Left = 352
    Top = 8
    Width = 473
    Height = 21
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = 'Camera URL'
    LabelPosition = lpLeft
    LabelSpacing = 5
    TabOrder = 11
  end
  object GoButton: TButton
    Left = 352
    Top = 32
    Width = 75
    Height = 25
    Caption = 'View'
    TabOrder = 12
    OnClick = GoButtonClick
  end
end
