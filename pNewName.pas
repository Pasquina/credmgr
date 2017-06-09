unit pNewName;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TfGDNewName = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    ebNewName: TEdit;
    procedure FormShow(Sender: TObject);
    procedure ebNewNameChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fGDNewName: TfGDNewName;

implementation

{$R *.dfm}

procedure TfGDNewName.ebNewNameChange(Sender: TObject);
begin
  if ebNewName.Text <> '' then
    OKBtn.Enabled := True
  else
    OKBtn.Enabled := False;
end;

procedure TfGDNewName.FormShow(Sender: TObject);
begin
  ebNewName.SetFocus;
end;

end.
