unit pGetCredentials;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls;

type
  TIniStorageScope = (issCurrentUser, issPublic);
  TIniStorageScopes = set of TIniStorageScope;
  TIniStorageScopeName = String;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TGetCredentials = class(TComponent)
  const
    TIniStorageScopeName: array [TIniStorageScope] of TIniStorageScopeName = ('CurrentUser', 'Public');
  private
    FApplication: String;
    FIniFileName: TFileName;
    FVendor: String;
    FCredentialSet: String;
    FCredentials: TStringList;
    FIniFileScope: TIniStorageScope;
    procedure SetApplication(const Value: String);
    procedure SetIniFileName(const Value: TFileName);
    procedure SetVendor(const Value: String);
    procedure SetCredentialSet(const Value: String);
    procedure SetCredentials(const Value: TStringList);
    function GetFullIniFileName(var LIniFile: TFileName): TModalResult;
    procedure SetIniFileScope(const Value: TIniStorageScope);

    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Credentials: TStringList read FCredentials write SetCredentials;
    function GetParameters: TModalResult;
    function SilentUpdate: TModalResult;
    procedure MergeInto(const AParamList: TStrings);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property Vendor: String read FVendor write SetVendor;
    property Application: String read FApplication write SetApplication;
    property IniFileName: TFileName read FIniFileName write SetIniFileName;
    property CredentialSet: String read FCredentialSet write SetCredentialSet;
    property IniFileScope: TIniStorageScope read FIniFileScope write SetIniFileScope stored True default issCurrentUser;
  end;

implementation

uses pCredentialHandler, System.IOUtils, VCL.Dialogs, System.IniFiles;

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
var
  LIniFileFolder: TFilename;                                          // determined by scope property
begin
  if IniFileScope = issCurrentUser then
    LIniFileFolder := System.IOUtils.TPath.GetHomePath                // user's private folder
  else
    LIniFileFolder := System.IOUtils.TPath.GetPublicPath;             // public folder (Program Data)

  if (Vendor <> '') and (Application <> '') and (IniFileName <> '') then // test mandatory properties
  begin
    LIniFile := TPath.Combine(TPath.Combine(                             // build the inifile name
      TPath.Combine(LIniFileFolder, Vendor), Application), IniFileName);
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

procedure TGetCredentials.SetIniFileScope(const Value: TIniStorageScope);
begin
  FIniFileScope := Value;
end;

procedure TGetCredentials.SetVendor(const Value: String);
begin
  FVendor := Value;
end;

{ SilentUpdate uses the credentials obtains earlier and saved in the public property
  Credentials to update the IniFile. No GUI display is made. Entries that are already
  present in the inifile are updated. Entries not present are added. }

function TGetCredentials.SilentUpdate: TModalResult;
var
  LIniFileName: TFileName;                             // fully qualified name of inifile
  LIniFile: TIniFile;                                  // inifile object for update
  LIx: Integer;                                        // iteration index
  LName: String;                                       // work area for entry name
  LValue: String;                                      // work area for entry value
begin
  if GetFullIniFileName(LIniFileName) = mrOK then      // if a valid filename
    begin
      LIniFile := TIniFile.Create(LIniFileName);       // prepare the file
      try
        for LIx := 0 to Pred(Credentials.Count) do
          begin
            LName := Credentials.Names[LIx];           // obtain the name
            LValue := Credentials.ValueFromIndex[LIx]; // obtain the value
            LIniFile.WriteString(CredentialSet, LName, LValue); // update/create the value
          end;
      finally
        LIniFile.Free;                                 // make sure resource is returned
      end;
    end;
end;

end.
