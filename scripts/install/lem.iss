; Inno Setup script for Lem.
; Compiled by scripts\win-deploy.ps1 (which passes /DAppVersion=<version>);
; expects the deploy-op output in ..\..\bin\.

#ifndef AppVersion
#define AppVersion "0.0.0"
#endif

#define AppName "Lem"
#define AppExe "lem.exe"

[Setup]
AppId={{7E7DA1D6-40F5-4E4D-9E2A-2C7C2C1E6D5B}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher=lem-project
AppPublisherURL=https://lem-project.github.io/
AppSupportURL=https://github.com/lem-project/lem
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
UninstallDisplayIcon={app}\{#AppExe}
SetupIconFile=lem.ico
LicenseFile=..\..\LICENCE
OutputBaseFilename=Lem-{#AppVersion}-setup
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
PrivilegesRequired=admin
PrivilegesRequiredOverridesAllowed=dialog
ChangesEnvironment=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; Flags: unchecked
Name: "addtopath"; Description: "Add Lem to PATH"; Flags: unchecked

[Files]
Source: "..\..\bin\*"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\{#AppExe}"
Name: "{group}\{cm:UninstallProgram,{#AppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\{#AppExe}"; Tasks: desktopicon

[Registry]
; Explorer context menu: "Open with Lem" on any file
Root: HKA; Subkey: "Software\Classes\*\shell\{#AppName}"; ValueType: string; ValueData: "Open with {#AppName}"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\*\shell\{#AppName}"; ValueType: string; ValueName: "Icon"; ValueData: """{app}\{#AppExe}"""
Root: HKA; Subkey: "Software\Classes\*\shell\{#AppName}\command"; ValueType: string; ValueData: """{app}\{#AppExe}"" ""%1"""

[Code]
const
  EnvKeyMachine = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
  EnvKeyUser = 'Environment';

function EnvRootKey(): Integer;
begin
  if IsAdminInstallMode then
    Result := HKEY_LOCAL_MACHINE
  else
    Result := HKEY_CURRENT_USER;
end;

function EnvSubKey(): string;
begin
  if IsAdminInstallMode then
    Result := EnvKeyMachine
  else
    Result := EnvKeyUser;
end;

procedure EnvAddPath(const Dir: string);
var
  Paths: string;
begin
  if not RegQueryStringValue(EnvRootKey, EnvSubKey, 'Path', Paths) then
    Paths := '';
  if Pos(';' + Uppercase(Dir) + ';', ';' + Uppercase(Paths) + ';') > 0 then
    exit;
  if (Paths <> '') and (Paths[Length(Paths)] <> ';') then
    Paths := Paths + ';';
  Paths := Paths + Dir;
  RegWriteExpandStringValue(EnvRootKey, EnvSubKey, 'Path', Paths);
end;

procedure EnvRemovePath(const Dir: string);
var
  Paths: string;
  P: Integer;
begin
  if not RegQueryStringValue(EnvRootKey, EnvSubKey, 'Path', Paths) then
    exit;
  P := Pos(';' + Uppercase(Dir) + ';', ';' + Uppercase(Paths) + ';');
  if P = 0 then
    exit;
  Delete(Paths, P, Length(Dir) + 1);
  RegWriteExpandStringValue(EnvRootKey, EnvSubKey, 'Path', Paths);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if (CurStep = ssPostInstall) and WizardIsTaskSelected('addtopath') then
    EnvAddPath(ExpandConstant('{app}'));
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usPostUninstall then
    EnvRemovePath(ExpandConstant('{app}'));
end;
