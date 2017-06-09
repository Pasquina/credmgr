object fGDNewName: TfGDNewName
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Credential Set New Name'
  ClientHeight = 183
  ClientWidth = 384
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 300
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 300
    Top = 38
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 163
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 40
      Width = 219
      Height = 19
      Margins.Top = 40
      Align = alTop
      Alignment = taCenter
      Caption = 'Enter new credential set name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object ebNewName: TEdit
      AlignWithMargins = True
      Left = 8
      Top = 70
      Width = 261
      Height = 21
      Hint = 
        'Enter the name of new set, or the new name if saving or renaming' +
        ' an existing set'
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Align = alTop
      TabOrder = 0
      OnChange = ebNewNameChange
    end
  end
end
