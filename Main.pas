unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  Vcl.StdCtrls, Vcl.ComCtrls, Winapi.ShellAPI;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    StatusBar1: TStatusBar;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    procedure MenuItem2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private 宣言 }
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Close();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, true);
end;

procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  FileName: string;
  I, Count: Cardinal;
begin
  try
    Count := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
    for I := 0 to Count - 1 do
    begin
      SetLength(FileName, MAX_PATH + 1);
      DragQueryFile(Msg.Drop, I, PChar(FileName), MAX_PATH);
      SetLength(FileName, StrLen(PChar(FileName)));
      Memo1.Lines.Add(FileName);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

end.
