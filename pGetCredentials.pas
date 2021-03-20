unit pGetCredentials;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TGetCredentials = class(TComponent)
  private
    FApplication: String;
    FIniFileName: TFileName;
    FVendor: String;
    FCredentialSet: String;
    FCredentials: TStringList;
    procedure SetApplication(const Value: String);
    procedure SetIniFileName(const Value: TFileName);
    procedure SetVendor(const Value: String);
    procedure SetCredentialSet(const Value: String);
    procedure SetCredentials(const Value: TStringList);
    function GetFullIniFileName(var LIniFile: TFileName): TModalResult;

    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Credentials: TStringList read FCredentials write SetCredentials;
    function GetParameters: TModalResult;
    procedure MergeInto(const AParamList: TStrings);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property Vendor: String read FVendor write SetVendor;
    property Application: String read FApplication write SetApplication;
    property IniFileName: TFileName read FIniFileName write SetIniFileName;
    property CredentialSet: String read FCredentialSet write SetCredentialSet;
  end;

implementation

uses pCredentialHandler, System.IOUtils, VCL.Dialogs;

{ TGetCredentials }

constructor TGetCredentials.Create(AOwner: TComponent);
begin
  inherited;
  Credentials := TStringList.Create;
end;

destructor TGetCredentials.Destroy;
begin
  Credentials.Free;
  inherited;
end;

{ Test the presence of required properties and return the constructed full path to the .ini file if
  all are present. Otherwise, display an error message and return Cancel. }

function TGetCredentials.GetFullIniFileName(var LIniFile: TFileName): TModalResult;
begin
  if (Vendor <> '') and (Application <> '') and (IniFileName <> '') then // test mandatory properties
  begin
    LIniFile := TPath.Combine(TPath.Combine(                             // build the inifile name
      TPath.Combine(System.IOUtils.TPath.GetHomePath, Vendor), Application), IniFileName);
    Result := mrOK;                                                      // return filename build succeeded
  end
  else
  begin
    ShowMessage('All required properties, Vendor, Applilcation and IniFileName must be specified.');
    Result := mrCancel;
  end;
end;

function TGetCredentials.GetParameters: TModalResult;
var
  LIniFile: TFileName;                                 // full drive, path and filename of inifile
  LValueListEditor: TfCredentials;                     // credential manager instance
begin
  Result := mrCancel;                                  // initialize to no credentials returned
  if (GetFullIniFileName(LIniFile)) = mrOK then        // get .inifile full path name
  begin
    LValueListEditor :=                                // create the dialog window
      TfCredentials.CreateLocal(self, LIniFile, Credentials, CredentialSet);
    try
      Result := LValueListEditor.ShowModal;            // get user input
      if Result = mrOK then                            // if the user has selected a set
        CredentialSet := LValueListEditor.TargetCreds; // save the selected set name for next time
    finally
      LValueListEditor.Free;                           // return resources
    end;
  end;
end;

procedure TGetCredentials.MergeInto(const AParamList: TStrings);
var
  LIndex: Integer;
  LFoundIndex: Integer;
begin
  for LIndex := 0 to Pred(Credentials.Count) do
  begin
    LFoundIndex := AParamList.IndexOfName(Credentials.Names[LIndex]);
    if LFoundIndex >= 0 then
    begin
      AParamList.ValueFromIndex[LFoundIndex] := Credentials.ValueFromIndex[LIndex];
    end
    else
    begin
      AParamList.Add(Credentials.Strings[LIndex]);
    end;
  end;
end;

procedure TGetCredentials.SetApplication(const Value: String);
begin
  FApplication := Value;
end;

procedure TGetCredentials.SetCredentials(const Value: TStringList);
begin
  FCredentials := Value;
end;

procedure TGetCredentials.SetCredentialSet(const Value: String);
begin
  FCredentialSet := Value;
end;

procedure TGetCredentials.SetIniFileName(const Value: TFileName);
begin
  FIniFileName := Value;
end;

procedure TGetCredentials.SetVendor(const Value: String);
begin
  FVendor := Value;
end;

end.
