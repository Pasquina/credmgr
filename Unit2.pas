unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, pGetCredentials, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    GetCredentials1: TGetCredentials;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  if (GetCredentials1.GetParameters = mrOK) then
    ShowMessage('Got ''em')
  else
    ShowMessage('Got zip!');
end;

end.
