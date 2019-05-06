object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'CueFLAC'
  ClientHeight = 242
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 223
    Width = 472
    Height = 19
    Panels = <>
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 472
    Height = 223
    Align = alClient
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 176
    Top = 56
    object MenuItem1: TMenuItem
      Caption = '&File'
      object MenuItem2: TMenuItem
        Caption = 'E&xit'
        OnClick = MenuItem2Click
      end
    end
  end
end
