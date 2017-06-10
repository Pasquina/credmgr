unit Component1;

interface

uses
  System.SysUtils, System.Classes;

type
  TComponent1 = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VyDev Utilityx', [TComponent1]);
end;

end.
