object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 434
  ClientWidth = 669
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 89
    Width = 8
    Height = 337
    Align = alLeft
    Shape = bsSpacer
    ExplicitTop = 49
    ExplicitHeight = 362
  end
  object Bevel2: TBevel
    Left = 0
    Top = 426
    Width = 669
    Height = 8
    Align = alBottom
    Shape = bsSpacer
    ExplicitLeft = 8
    ExplicitTop = 49
    ExplicitWidth = 362
  end
  object Bevel3: TBevel
    Left = 661
    Top = 89
    Width = 8
    Height = 337
    Align = alRight
    Shape = bsSpacer
    ExplicitLeft = 50
    ExplicitTop = 49
    ExplicitHeight = 362
  end
  object Bevel4: TBevel
    Left = 0
    Top = 81
    Width = 669
    Height = 8
    Align = alTop
    Shape = bsSpacer
    ExplicitLeft = 8
    ExplicitTop = 49
    ExplicitWidth = 354
  end
  object DesignBox1: TDesignBox
    Left = 8
    Top = 89
    Width = 653
    Height = 337
    Align = alClient
    OnSelectItem = DesignBox1SelectItem
    OnMouseDown = DesignBox1MouseDown
    OnMouseMove = DesignBox1MouseMove
    PopupMenu = PopupMenu1
    ExplicitLeft = 88
    ExplicitTop = 144
    ExplicitWidth = 100
    ExplicitHeight = 41
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 669
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Image1: TImage
      Left = 320
      Top = 9
      Width = 41
      Height = 41
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000640000
        0064080600000070E29554000002224944415478DAEDDCCB7203210C44D1F0FF
        1F9D641B511555972468CFDC5E6390388BF103BCBE7FF3456CB200F10A206601
        C42C80980510B300621640CC9282ACB58E1614CBE95E7F7A7E75FD184000D10A
        06240E006474FD1819A4FB3D40367FB661DDE34FF7B78D070410697E408A1376
        170C48F386756F48753DFBFE0031EB0F10B3FE0031EBEFD341A61FEA80000288
        557F8098F5E70E52FDE0569D1F104000B9D9DF562F200F03A9A67BC3BAC1A6FB
        DBC6030288343F20660503924C389D6E90EE8776777F318000A2150C481C00C8
        687F31F687ADAB1B66DEDEDE2F205E01C42C8098A5FDA15E3DA470FBA17BBABF
        EDF580680104106D81E982014916983EA4E096F206DFFEC510905ABF802401C4
        2C76206A81594E1F3270AB1F10B3FA0131AB1F10B3FAC73F184E3F94BB0FAAA9
        F59FEE1F1040C40201B95B90BA7E56CFE341A61B1E7F680EF7A3AEB78D070490
        D2FC800022CDAFAEB78D7FDA21876A3BB7DF040022AE0708205E01A4B821D59C
        FE7230CBE90B4180240104104094F5EC40DC2F7DC69CBE0054DE0F400019DDA0
        1840D40200F93BFFF4978BD50DCAEA51E7EF1EDF1D400001E4DFFE0001449A3F
        6DE0F21F03BCFEA13E5D9F1A4086EB5303C8707D6A5E0772FB0FCABA41AF9F3A
        C90A0224AC0788F6FA6A00791B4835A7BFCC73BF740A0820DAFC801C6E009050
        9FFB2187D31776A62F997EFCA913409209A7030820ADEBC53CEE7EC8DB028859
        00310B206601C42C80980510B30062961F08D2C32F9F9E3B9B0000000049454E
        44AE426082}
      Stretch = True
      Visible = False
    end
    object Label1: TLabel
      Left = 232
      Top = 8
      Width = 60
      Height = 18
      Caption = 'X: 0mm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 232
      Top = 32
      Width = 60
      Height = 18
      Caption = 'Y: 0mm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
    end
    object Button1: TButton
      Left = 16
      Top = 16
      Width = 83
      Height = 25
      Caption = 'Add Text'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 112
      Top = 16
      Width = 83
      Height = 25
      Caption = 'Add QR Code'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 489
      Top = 16
      Width = 83
      Height = 25
      Caption = 'Save'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button5: TButton
      Left = 367
      Top = 16
      Width = 83
      Height = 25
      Caption = 'Clear'
      TabOrder = 3
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 16
      Top = 47
      Width = 83
      Height = 25
      Caption = 'Add Rectangle'
      TabOrder = 4
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 112
      Top = 47
      Width = 83
      Height = 25
      Caption = 'Add Ellipse'
      TabOrder = 5
      OnClick = Button7Click
    end
  end
  object Button4: TButton
    Left = 578
    Top = 16
    Width = 83
    Height = 25
    Caption = 'Load'
    TabOrder = 1
    OnClick = Button4Click
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 328
    Top = 224
    object BringToFront1: TMenuItem
      Action = actBringToFront
    end
    object SendToBack1: TMenuItem
      Action = actSendToBack
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Action = actDelete
    end
  end
  object ActionList1: TActionList
    Left = 224
    Top = 224
    object actBringToFront: TAction
      Caption = 'Bring To Front'
      OnExecute = actBringToFrontExecute
    end
    object actSendToBack: TAction
      Caption = 'Send To Back'
      OnExecute = actSendToBackExecute
    end
    object actDelete: TAction
      Caption = 'Delete'
      OnExecute = actDeleteExecute
    end
  end
end
