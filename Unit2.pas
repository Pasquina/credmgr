unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, pGetCredentials, Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.ToolWin,
  Vcl.ComCtrls, Vcl.Menus, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client;

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
    FDConnection1: TFDConnection;
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
  if (GetCredentials1.GetParameters = mrOK) then                    // obtain credential set as value list
  begin
    StatusBar1.Panels[0].Text := 'Credentials returned.';           // display success
    GetCredentials1.MergeInto(ListBox1.Items);                      // merge into listbox items
    GetCredentials1.MergeInto(FDConnection1.Params);                // merge into FireDAC connection parameters
  end
  else
    StatusBar1.Panels[0].Text := 'Credential retrieval cancelled.'; // display cancel
end;

end.
