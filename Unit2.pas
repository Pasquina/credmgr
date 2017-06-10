unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, pGetCredentials, Vcl.StdCtrls,
  Component1, System.Actions, Vcl.ActnList, Vcl.ToolWin, Vcl.ComCtrls, Vcl.Menus;

type
  TForm2 = class(TForm)
    Button1: TButton;
    GetCredentials1: TGetCredentials;
    ListBox1: TListBox;
    StatusBar1: TStatusBar;
    ActionList1: TActionList;
    aGetCredentials: TAction;
    aExit: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    GetCredentials2: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    procedure aGetCredentialsExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.aExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm2.aGetCredentialsExecute(Sender: TObject);
begin
  if (GetCredentials1.GetParameters = mrOK) then
    begin
      StatusBar1.Panels[0].Text := 'Credentials returned.';
      ListBox1.Items.Clear;
      ListBox1.Items.AddStrings(GetCredentials1.Credentials);
    end
  else
    StatusBar1.Panels[0].Text := 'Credential retrieval cancelled.';
end;

end.
