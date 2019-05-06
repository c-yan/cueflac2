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
    procedure DoCueFLAC(const FileName: string);
    function IsCueFile(const FileName: string): Boolean;
    procedure RestoreCueTimestamp(const FileName: string);
    function LoadCueFile(const FileName: string): TStringList;
    function ExtractFileLine(const Cue: TStringList): string;
    function GetFileLineIndex(const Cue: TStringList): Integer;
    function ExtractWavFileName(const Line: string): string;
    procedure Log(const Fmt: string; const Args: array of const);
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function ReverseString(const S: string): string;
var
  I, L: Integer;
begin
  SetLength(Result, Length(S));
  L := Length(S);

  for I := 0 to L - 1 do
    Result[L - I] := S[I + 1];
end;

function RPos(Substr: string; S: string): Integer;
begin
  Result := Pos(ReverseString(Substr), ReverseString(S));
  if Result <> 0 then
    Result := Length(S) - (Result - 1) - (Length(Substr) - 1);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Close();
end;

procedure TForm1.RestoreCueTimestamp(const FileName: string);
var
  Line: string;
  Cue: TStringList;
  WaveFileName: string;
  WaveFilePath: string;
begin
  Cue := LoadCueFile(FileName);
  try
    Line := ExtractFileLine(Cue);
    if Line = '' then
    begin
      Exit;
    end;
    Log('Line: %s', [Line]);
    WaveFileName := ExtractWavFileName(Line);
    Log('WaveFileName: %s', [WaveFileName]);
    WaveFilePath := ExtractFilePath(FileName) + WaveFileName;
    Log('CueFile: %s, WaveFile: %s', [FileName, WaveFilePath]);
    FileSetDate(FileName, FileAge(WaveFilePath));
  finally
    Cue.Free();
  end;
end;

procedure TForm1.DoCueFLAC(const FileName: string);
begin
  if IsCueFile(FileName) then
  begin
    // ChangeWavToFlac(FileName);
    RestoreCueTimestamp(FileName);
  end;
end;

function TForm1.ExtractFileLine(const Cue: TStringList): string;
var
  LineIndex: Integer;
begin
  LineIndex := GetFileLineIndex(Cue);
  if LineIndex = -1 then
    Result := ''
  else
    Result := Cue.Strings[LineIndex];
end;

function TForm1.ExtractWavFileName(const Line: string): string;
var
  StartIndex, Count: Integer;
begin
  StartIndex := Pos('"', Line) + 1;
  Count := RPos('"', Line) - Pos('"', Line) - 1;
  Result := Copy(Line, StartIndex, Count);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, true);
end;

function TForm1.GetFileLineIndex(const Cue: TStringList): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Cue.Count - 1 do
  begin
    if Pos('FILE', Cue.Strings[I]) = 1 then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TForm1.IsCueFile(const FileName: string): Boolean;
begin
  Result := LowerCase(ExtractFileExt(FileName)) = '.cue';
end;

function TForm1.LoadCueFile(const FileName: string): TStringList;
begin
  Result := TStringList.Create();
  Result.LoadFromFile(FileName);
end;

procedure TForm1.Log(const Fmt: string; const Args: array of const);
begin
  Form1.Memo1.Lines.Add(Format(Fmt, Args));
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
      DoCueFLAC(FileName);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

end.
