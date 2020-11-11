object Form1: TForm1
  Left = 220
  Top = 149
  Width = 659
  Height = 482
  Caption = 
    'Spiral     Enter, 1: Start new       Space, 2: Pause       3: Se' +
    'ttings       Esc, 0: Exit'
  Color = clGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pb: TPaintBox
    Left = 8
    Top = 8
    Width = 635
    Height = 441
    Color = clWhite
    ParentColor = False
    OnPaint = pbPaint
  end
  object Label1: TLabel
    Left = 224
    Top = 88
    Width = 5
    Height = 13
    AutoSize = False
  end
  object Label2: TLabel
    Left = 120
    Top = 130
    Width = 30
    Height = 1
    AutoSize = False
    Color = clGray
    ParentColor = False
  end
  object Timer1: TTimer
    Interval = 40
    OnTimer = Timer1Timer
    Left = 272
    Top = 40
  end
end
