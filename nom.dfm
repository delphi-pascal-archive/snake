object Form7: TForm7
  Left = 268
  Top = 120
  Width = 354
  Height = 93
  Caption = 'Felicitation'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 120
  TextHeight = 17
  object Label1: TLabel
    Left = 8
    Top = 5
    Width = 116
    Height = 17
    Caption = 'Entrer votre nom: '
  end
  object Edit1: TEdit
    Left = 8
    Top = 32
    Width = 217
    Height = 25
    TabOrder = 0
  end
  object Button3: TButton
    Left = 232
    Top = 32
    Width = 105
    Height = 25
    Caption = 'Valider'
    TabOrder = 1
    OnClick = Button3Click
  end
end
