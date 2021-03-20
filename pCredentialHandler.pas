unit pCredentialHandler;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, Vcl.Graphics, Vcl.Controls, Vcl.Forms, IniFiles,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CategoryButtons, Vcl.Grids, Vcl.ValEdit, System.Actions, Vcl.ActnList,
  System.ImageList, Vcl.ImgList, pNewName, System.Classes, Vcl.ComCtrls;

type

  TfCredentials = class(TForm)
    cbCredentialSets: TComboBox;
    CategoryButtons1: TCategoryButtons;
    pnProperties: TPanel;
    Label1: TLabel;
    vleCredentials: TValueListEditor;
    alCredentials: TActionList;
    aSave: TAction;
    aSaveAs: TAction;
    aRename: TAction;
    aDelete: TAction;
    aUse: TAction;
    aCancel: TAction;
    aNew: TAction;
    ilCredentials: TImageList;
    sbHandler: TStatusBar;
    procedure cbCredentialSetsChange(Sender: TObject);
    procedure aCancelExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aUseExecute(Sender: TObject);
    procedure vleCredentialsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aRenameExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
    FCredentialFile: TIniFile;
    FLastCredentialSetName: string;
    FCredentialSetModified: Boolean;
    FParamList: TStringList;
    FTargetCreds: string;
    procedure SetCredentialFile(const Value: TIniFile);
    procedure SetLastCredentialSetName(const Value: string);
    procedure SetCredentialSetModified(const Value: Boolean);
    procedure SetParamList(const Value: TStringList);
    procedure SetTargetCreds(const Value: string);
    property CredentialFile: TIniFile       // inifile that contains credential sets
      read FCredentialFile write SetCredentialFile;
    property ParamList: TStringList            // parameter list returned when credentials selected
      read FParamList write SetParamList;
    property LastCredentialSetName: string  // the name of the most recent credential set retrieved
      read FLastCredentialSetName write SetLastCredentialSetName;
    property CredentialSetModified: Boolean // true if the current credential set has been modified
      read FCredentialSetModified write SetCredentialSetModified;
    procedure LoadValueListEditor(          // load a value list editor with a specific section value set
      const AValueListEditor: TValueListEditor; const AIniFile: TIniFile; const ASectionName: string);
    procedure UpdateIniFile(                // update the ini file with values from the value list editor
      const AValueListEditor: TValueListEditor; const AIniFile: TIniFile; const ASectionName: string);
    procedure AdjustActionSettings;
    function CanChangeCredentials: Boolean; // test to see if credential set needs to be saved

  protected
  public
    { Public declarations }
    property TargetCreds: string            // name of credential set initially requested
      read FTargetCreds write SetTargetCreds;
    constructor CreateLocal(const AOwner: TComponent; const AIniFile: TFileName; const AParamList: TStringList;
      ATargetCreds: string);
    destructor Destroy; override;
  end;

var
  fCredentials: TfCredentials;

implementation

uses
  Vcl.Dialogs;

{$R *.dfm}

{ User has cancelled the retrieve credentials dialog; do nothing }

procedure TfCredentials.aCancelExecute(Sender: TObject);
begin
  Self.Close;                                             // close the form and return mrCancel
end;

{ User wants to create a new credential set from the default model }

procedure TfCredentials.aNewExecute(Sender: TObject);
var
  LGDNewName: TfGDNewName;                                // local variable: newname dialog form object
begin
  if CanChangeCredentials then                            // test for changed credentials

    try
      LGDNewName := TfGDNewName.Create(Self);             // create the newname dialog
      if LGDNewName.ShowModal = mrOK then                 // show and test OK
      begin
        if not CredentialFile.SectionExists(LGDNewName.ebNewName.Text) then // make certain section not already there
        begin
          LoadValueListEditor(                            // load the model values into the editor
            vleCredentials, CredentialFile, '*model');
          cbCredentialSets.ItemIndex :=                   // put the new name in the dropdown
            cbCredentialSets.Items.Add(LGDNewName.ebNewName.Text);
          CredentialSetModified := False;                 // new set not yet modified
          LastCredentialSetName := cbCredentialSets.Text; // new name is name of last set loaded
          AdjustActionSettings;                           // adjust the action list
          UpdateIniFile(                                  // add the values to the inifile
            vleCredentials, CredentialFile, cbCredentialSets.Text);
        end
        else
        begin
          ShowMessage(Format('%s already exists.', [LGDNewName.ebNewName.Text]));
        end;
      end;
    finally
      LGDNewName.Free;                                                    // release the dialog resourcesd
    end;
end;

procedure TfCredentials.aRenameExecute(Sender: TObject);
var
  LGDNewName: TfGDNewName;                                                // local variable: newname dialog form object
  LsOldName: string;                                                      // the old name of the credential set
begin
  try
    LGDNewName := TfGDNewName.Create(Self);                               // create the newname dialog
    if LGDNewName.ShowModal = mrOK then                                   // show and test OK
    begin
      if not CredentialFile.SectionExists(LGDNewName.ebNewName.Text) then // make certain section not already there
      begin
        LsOldName := cbCredentialSets.Text;                               // save the old credential set name
        cbCredentialSets.ItemIndex :=                                     // put the new name in the dropdown
          cbCredentialSets.Items.Add(LGDNewName.ebNewName.Text);
        CredentialSetModified := False;                                   // new set not yet modified
        LastCredentialSetName := cbCredentialSets.Text;                   // new name is name of last set loaded
        AdjustActionSettings;                                             // adjust the action list
        UpdateIniFile(                                                    // add the values to the inifile
          vleCredentials, CredentialFile, cbCredentialSets.Text);
        CredentialFile.EraseSection(LsOldName);                           // erase the old credential set name
      end
      else
      begin
        ShowMessage(Format('%s already exists.', [LGDNewName.ebNewName.Text]));
      end;
    end;
  finally
    LGDNewName.Free;                                                      // release the dialog resourcesd
  end;

end;

{ Save the current credential set }

procedure TfCredentials.aSaveAsExecute(Sender: TObject);
var
  LGDNewName: TfGDNewName;                            // new name dialog
begin
  try
    LGDNewName := TfGDNewName.Create(Self);           // create the newname dialog
    if LGDNewName.ShowModal = mrOK then               // show and test ok
    begin
      cbCredentialSets.ItemIndex :=                   // put the new name in the dropdown
        cbCredentialSets.Items.Add(LGDNewName.ebNewName.Text);
      CredentialSetModified := False;                 // new set not yet modified
      LastCredentialSetName := cbCredentialSets.Text; // new name is name of last set loaded
      AdjustActionSettings;                           // adjust the action list
      UpdateIniFile(                                  // add the values to the inifile
        vleCredentials, CredentialFile, cbCredentialSets.Text);
    end
    else
    begin
      ShowMessage(Format('%s already exists.', [LGDNewName.ebNewName.Text]));
    end;
  finally
    LGDNewName.Free;                                  // release the dialog resourcesd
  end;
end;

procedure TfCredentials.aSaveExecute(Sender: TObject);
begin
  UpdateIniFile(                                      // update the current credential set
    vleCredentials, CredentialFile, cbCredentialSets.Text);
  CredentialSetModified := False;                     // no changes after a save
  AdjustActionSettings;                               // make any needed changes to the action settings
end;

{ When the user has selected a credential set and chosen "Use," the entries are copied to the object passed
  by the caller. Note that this implies a Save. }

procedure TfCredentials.aUseExecute(Sender: TObject);
begin
  if CredentialSetModified then                       // if credentials have been modified
    UpdateIniFile(                                    // update the current credential set
      vleCredentials, CredentialFile, cbCredentialSets.Text);
  CredentialSetModified := False;                     // no changes after a save
  AdjustActionSettings;                               // make any needed changes to the action settings

  ParamList.Assign(vleCredentials.Strings);           // return the edited strings from the editor
  TargetCreds := cbCredentialSets.Text;               // return the current target credential set

  ModalResult := mrOK; // close form and return OK value to caller
end;

{ Tests for a modified credential set and prompts user to save the changes, discard
  the changes or cancel the pending operation }

function TfCredentials.CanChangeCredentials: Boolean;
begin
  Result := True;      // assume credential set doesn't require saving

  if CredentialSetModified then
  begin
    case MessageDlg(Format('%s credential set has been modified.' + sLineBreak + 'Choose Yes to save the new values' +
      sLineBreak + 'Choose No to discard the changes' + sLineBreak + 'Choose Cancel to cancel the pending operation',
      [LastCredentialSetName]), TMsgDlgType.mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbCancel) of
      mrYes:
        begin
          UpdateIniFile(                  // save the values under the current name
            vleCredentials, CredentialFile, LastCredentialSetName);
          CredentialSetModified := False; // unmodified credential set
        end;
      mrNo:
        begin
          // Nothing happens
        end;
      mrCancel:
        begin
          Result := False;                // tell caller to cancel the pending operation
        end;
    end;
  end;
end;

{ Initially, many functions are blocked because there is no selected set in the dropdown.
  When the user chooses a set from the dropdown, these functions are enabled.
  Likewise, if the user deletes all available sets, this routine will disable all
  of the related functions that are no longer meaningful.

  Important note: Cannot use the ActionList OnUpdate, because this form runs as a modal form.
  This means that it is isolated from the main message loop, that is apparently needed for
  OnUpdate to work properly. In other words, OnUpdate doesn't work in a modal form. }

procedure TfCredentials.cbCredentialSetsChange(Sender: TObject);
begin
  if CanChangeCredentials then
  begin
    LoadValueListEditor(                            // now display the value list
      vleCredentials, CredentialFile, cbCredentialSets.Text);
    LastCredentialSetName := cbCredentialSets.Text; // save the new name
    CredentialSetModified := False;                 // credential set not yet modified
    AdjustActionSettings;                           // adjust the action list to correspond with selection
  end
  else
  begin
    cbCredentialSets.ItemIndex := cbCredentialSets.Items.IndexOf(LastCredentialSetName);
  end;
end;

constructor TfCredentials.CreateLocal(const AOwner: TComponent; const AIniFile: TFileName; const AParamList: TStringList;
  ATargetCreds: string);
begin
  inherited Create(AOwner);                            // initial processing
  CredentialFile := TIniFile.Create(AIniFile);         // establish the ini file
  ParamList := AParamList;                             // save the callers parameter list
  TargetCreds := ATargetCreds;                         // caller can request a set (section name)
  cbCredentialSets.Items.BeginUpdate;                  // shield the user from processing
  cbCredentialSets.Clear;                              // make sure it's empty
  CredentialFile.ReadSections(cbCredentialSets.Items); // populate dropdown with section names from inifile
  cbCredentialSets.ItemIndex :=                        // locate model entry not modifiable by the user
    cbCredentialSets.Items.IndexOf('*model');
  if cbCredentialSets.ItemIndex >= 0 then
    cbCredentialSets.DeleteSelected;                   // delete name (not inifile section!] from the dropdown
  cbCredentialSets.Items.EndUpdate;                    // display the dropdown

  { Now load the currently specified credential set using the Target Credentials pointer }

  cbCredentialSets.ItemIndex :=                        // set the credential set name in the dropdown
    cbCredentialSets.Items.IndexOf(TargetCreds);
  LoadValueListEditor(                                 // load the value list editor
    vleCredentials, CredentialFile, cbCredentialSets.Text);
  LastCredentialSetName := cbCredentialSets.Text;      // save the last loaded name
  CredentialSetModified := False;                      // set not yet modified
  AdjustActionSettings;                                // Adjust the buttons to reflect possible activities
end;

destructor TfCredentials.Destroy;
begin
  CredentialFile.Free;                                 // return the inifile
  inherited;
end;

procedure TfCredentials.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CanChangeCredentials;                    // test for and process unsaved credentials
end;

procedure TfCredentials.FormCreate(Sender: TObject);
begin
  sbHandler.Panels[0].Text := CredentialFile.FileName;
end;

{ Delete the currently selected credential set }

procedure TfCredentials.aDeleteExecute(Sender: TObject);
var
  LItemIndex: Integer;                                // hold area for selected index
begin
  LItemIndex := cbCredentialSets.ItemIndex;           // save the current item index
  Dec(LItemIndex);                                    // point to previous item
  CredentialFile.EraseSection(cbCredentialSets.Text); // erase the curent credential set
  cbCredentialSets.DeleteSelected;                    // remove the item from the list
  cbCredentialSets.ItemIndex := LItemIndex;           // point to previous item index
  LoadValueListEditor(                                // load the values from the previous credential set
    vleCredentials, CredentialFile, cbCredentialSets.Text);
  LastCredentialSetName := cbCredentialSets.Text;     // save the last loaded name
  CredentialSetModified := False;                     // set not yet modified
  AdjustActionSettings;                               // Adjust the buttons to reflect possible activities
end;

procedure TfCredentials.AdjustActionSettings;
begin
  if cbCredentialSets.ItemIndex < 0 then              // test list for no valid set chosen

  { No valid set chosen: Disable all functions that need a set }

  begin
    aNew.Enabled := True;
    aNew.ImageIndex := 0;
    aDelete.Enabled := False;
    aDelete.ImageIndex := 7;
    aRename.Enabled := False;
    aRename.ImageIndex := 7;
    aSave.Enabled := False;
    aSave.ImageIndex := 7;
    aSaveAs.Enabled := False;
    aSaveAs.ImageIndex := 7;
    aUse.Enabled := False;
    aUse.ImageIndex := 7;
    aCancel.Enabled := True;
    aCancel.ImageIndex := 6;
  end
  else

  { Valid set chosen: Enable all functions that depend on having a set }

  begin
    aNew.Enabled := True;
    aNew.ImageIndex := 0;
    aDelete.Enabled := True;
    aDelete.ImageIndex := 4;
    aRename.Enabled := True;
    aRename.ImageIndex := 3;
    aSaveAs.Enabled := True;
    aSaveAs.ImageIndex := 2;
    if CredentialSetModified then // save depends on a modified credential set
    begin
      aSave.Enabled := True;
      aSave.ImageIndex := 1;
    end
    else
    begin
      aSave.Enabled := False;
      aSave.ImageIndex := 7;
    end;
    aUse.Enabled := True;
    aUse.ImageIndex := 5;
    aCancel.Enabled := True;
    aCancel.ImageIndex := 6;
  end;
end;

{ Load a value list editor with strings from a specified section in the inifile }

procedure TfCredentials.LoadValueListEditor(const AValueListEditor: TValueListEditor; const AIniFile: TIniFile;
  const ASectionName: string);
begin
  AValueListEditor.Strings.BeginUpdate; // spare the user flicker
  AValueListEditor.Strings.Clear;       // clear out the value list editor
  AIniFile.ReadSectionValues(           // populate  value list editor with selected values
    ASectionName, AValueListEditor.Strings);
  AValueListEditor.Strings.EndUpdate;   // enable responsiveness
  CredentialSetModified := False;       // reset the modified flag
  LastCredentialSetName := ASectionName // name of last set loaded
end;

procedure TfCredentials.SetCredentialFile(const Value: TIniFile);
begin
  FCredentialFile := Value;
end;

procedure TfCredentials.SetCredentialSetModified(const Value: Boolean);
begin
  FCredentialSetModified := Value;
end;

procedure TfCredentials.SetLastCredentialSetName(const Value: string);
begin
  FLastCredentialSetName := Value;
end;

procedure TfCredentials.SetParamList(const Value: TStringList);
begin
  FParamList := Value;
end;

procedure TfCredentials.SetTargetCreds(const Value: string);
begin
  FTargetCreds := Value;
end;

{ Write the current values from the specified value list editor into the specified
  section and inifile }

procedure TfCredentials.UpdateIniFile(const AValueListEditor: TValueListEditor; const AIniFile: TIniFile;
  const ASectionName: string);
var
  LKeyName: String;
  LValue: String;
  LIndex: Integer;
begin
  for LIndex := 0 to Pred(AValueListEditor.Strings.Count) do
    begin
      LKeyName := AValueListEditor.Strings.Names[LIndex];
      LValue := AValueListEditor.Strings.ValueFromIndex[LIndex];
      AIniFile.WriteString(ASectionName, LKeyName, LValue);
    end;
end;

{ Record the fact that the credential set has been modified to be examined before
  other actions are taken }

procedure TfCredentials.vleCredentialsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  CredentialSetModified := True; // mark credential set modified
  AdjustActionSettings;          // adjust the action settings
end;

end.
