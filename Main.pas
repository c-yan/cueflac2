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
    procedure ChangeWavToFlac(const FileName: string);
    function IsWavFile(const FileName: string): Boolean;
    procedure EncodeWavFile(const FileName: string);
    procedure SaveCueFile(const Cue: TStringList; const FilePath: string);
    function ChangeFileLineExtToFlac(const Line: string): string;
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

function GetFileSize(const FileName: string): Int64;
var
  Handle: THandle;
  Data: TWin32FindData;
begin
  Handle := FindFirstFile(PChar(FileName), Data);
  if Handle = INVALID_HANDLE_VALUE then
  begin
    Result := -1;
    Exit;
  end;
  Result := (Int64(Data.nFileSizeHigh) shl 32) + Data.nFileSizeLow;
  Winapi.Windows.FindClose(Handle);
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

procedure TForm1.SaveCueFile(const Cue: TStringList; const FilePath: string);
var
  LineIndex: Integer;
begin
  LineIndex := GetFileLineIndex(Cue);
  Cue.Strings[LineIndex] := ChangeFileLineExtToFlac(Cue.Strings[LineIndex]);
  Cue.SaveToFile(FilePath);
end;

function TForm1.ChangeFileLineExtToFlac(const Line: string): string;
begin
  Result := StringReplace(Line, '.wav', '.flac', []);
end;

procedure TForm1.ChangeWavToFlac(const FileName: string);
var
  Line: string;
  Cue: TStringList;
  WavFileName: string;
  WaveFilePath: string;
  WaveFileSize: Int64;
  FlacFileName: string;
  FlacFilePath: string;
  FlacFileSize: Int64;
begin
  Cue := LoadCueFile(FileName);
  try
    Line := ExtractFileLine(Cue);
    if Line = '' then
    begin
      Exit;
    end;
    WavFileName := ExtractWavFileName(Line);
    if not IsWavFile(WavFileName) then
    begin
      Exit;
    end;
    WaveFilePath := ExtractFilePath(FileName) + WavFileName;
    WaveFileSize := GetFileSize(WaveFilePath);
    if WaveFileSize = -1 then
    begin
      Exit;
    end;
    Log('InFile: %s (%s bytes)', [WavFileName, FormatFloat('#,',
      WaveFileSize)]);
    EncodeWavFile(WaveFilePath);
    FlacFileName := ChangeFileExt(WavFileName, '.flac');
    FlacFilePath := ExtractFilePath(FileName) + FlacFileName;
    FlacFileSize := GetFileSize(FlacFilePath);
    Log('OutFile: %s (%s bytes / %.1f%%)',
      [FlacFileName, FormatFloat('#,', FlacFileSize),
      FlacFileSize / WaveFileSize * 100]);
    if FlacFileSize = -1 then
    begin
      Exit;
    end;
    SaveCueFile(Cue, FileName);
  finally
    Cue.Free();
  end;
end;

procedure TForm1.DoCueFLAC(const FileName: string);
begin
  if IsCueFile(FileName) then
  begin
    ChangeWavToFlac(FileName);
    RestoreCueTimestamp(FileName);
  end;
end;

procedure TForm1.EncodeWavFile(const FileName: string);
var
  FlacExe: string;
  PI: TProcessInformation;
  SI: TStartupInfo;
  CommandLine: string;
begin
  FlacExe := ExtractFilePath(Application.ExeName) + 'flac.exe';
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_MINIMIZE;
  CommandLine := Format('"%s" "%s" "%s"', [FlacExe, '-8', FileName]);
  CreateProcess(nil, PChar(CommandLine), nil, nil, false, 0, nil, nil, SI, PI);
  WaitForSingleObject(PI.hProcess, INFINITE);
  CloseHandle(PI.hThread);
  CloseHandle(PI.hProcess);
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

function TForm1.IsWavFile(const FileName: string): Boolean;
begin
  Result := LowerCase(ExtractFileExt(FileName)) = '.wav';
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
