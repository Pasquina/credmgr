# credmgr
A Delphi Component Using Value List Editor to Manage Sets of Credentials
## Overview
**credmgr** (Credential Manager) is a Delphi component that simplifies the implementation of the Value List Editor by allowing for a "library" of credentials, stored as sections of a .ini file. After the component is dropped on a form, several properties need to be specified:
- **Vendor** The name of the creator of the entire application (not the creator of this component!) This is used to specify the correct folder name for the .ini file.
- **Application** The name of the application. As with Vendor, this is used to specify the correct folder for the .ini file.
- **IniFileName** The name of the .ini file that will contain the credential library.

The full `drive\path\filename` of the .inifile is derived as `GetHomePath\Vendor\Application\IniFileName.` Delphi documentation has more infomration on `System.IOUtils.TPath.GetHomePath.` On Wihdows Vista or later, this will typically be `C:\Users\<username>\AppData\Roaming\Vendor\Application\IniFileName.`

Utilization of the component is fairly simple.

```delphi
procedure GetCredentialsAndOpen;
if (GetCredentials1.GetParameters = mrOK) then
  begin
    GetCredentials1.MergeInto(FireDACConnector1.Params);
    FireDACConnector1.Connect;
  end;
  ```
  
More complete documentation is contained in the project in the Docs folder. There are several formats for the documentation that was created using Help N Doc. All formats have the same content.
