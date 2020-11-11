object Form2: TForm2
  Left = 421
  Top = 124
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 242
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 13
    Width = 32
    Height = 13
    Caption = 'Points:'
  end
  object Label2: TLabel
    Left = 151
    Top = 13
    Width = 31
    Height = 13
    Caption = 'Width:'
  end
  object Label3: TLabel
    Left = 148
    Top = 37
    Width = 34
    Height = 13
    Caption = 'Height:'
  end
  object Label4: TLabel
    Left = 313
    Top = 13
    Width = 54
    Height = 13
    Caption = 'Form width:'
  end
  object Label5: TLabel
    Left = 309
    Top = 37
    Width = 58
    Height = 13
    Caption = 'Form height:'
  end
  object Label6: TLabel
    Left = 89
    Top = 80
    Width = 95
    Height = 13
    Caption = 'Max paintBox width:'
  end
  object Label7: TLabel
    Left = 85
    Top = 104
    Width = 99
    Height = 13
    Caption = 'Max paintBox height:'
  end
  object Label8: TLabel
    Left = 74
    Top = 143
    Width = 111
    Height = 13
    Caption = 'Convergency Queue #:'
  end
  object Label9: TLabel
    Left = 70
    Top = 168
    Width = 114
    Height = 13
    Caption = 'Convergency Accuracy:'
  end
  object Edit1: TEdit
    Left = 51
    Top = 10
    Width = 52
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 187
    Top = 10
    Width = 52
    Height = 21
    TabOrder = 1
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 187
    Top = 34
    Width = 52
    Height = 21
    TabOrder = 2
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 371
    Top = 10
    Width = 52
    Height = 21
    TabOrder = 3
  end
  object Edit5: TEdit
    Left = 371
    Top = 34
    Width = 52
    Height = 21
    TabOrder = 4
  end
  object Edit6: TEdit
    Left = 189
    Top = 77
    Width = 52
    Height = 21
    TabOrder = 5
  end
  object Edit7: TEdit
    Left = 189
    Top = 101
    Width = 52
    Height = 21
    TabOrder = 6
  end
  object Button1: TButton
    Left = 40
    Top = 198
    Width = 75
    Height = 25
    Caption = 'Apply && Start'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 315
    Top = 123
    Width = 75
    Height = 25
    Caption = 'Default'
    TabOrder = 8
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 328
    Top = 198
    Width = 75
    Height = 25
    Caption = 'Close this form'
    TabOrder = 9
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 315
    Top = 91
    Width = 75
    Height = 25
    Caption = 'Read from form'
    TabOrder = 10
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 152
    Top = 198
    Width = 145
    Height = 25
    Caption = 'Apply && Start && close this form'
    TabOrder = 11
    OnClick = Button5Click
  end
  object Edit8: TEdit
    Left = 189
    Top = 140
    Width = 52
    Height = 21
    TabOrder = 12
  end
  object Edit9: TEdit
    Left = 189
    Top = 165
    Width = 52
    Height = 21
    TabOrder = 13
  end
end
