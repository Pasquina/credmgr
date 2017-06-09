unit pCredentialManager;

interface

procedure Register;

implementation

uses
  pGetCredentials, System.Classes;

procedure Register;
begin
  RegisterComponents('VyDev Utilityx', [TGetCredentials]);
end;

end.
