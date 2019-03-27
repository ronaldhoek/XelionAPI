unit XPlatform;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants;

type
  TSecretData = record
    Username: string;
    CredentialData: string;
    procedure Clear;
  public
    constructor Create(const aUsername, aCredentialData: string);
  end;

  TXPlatform = class
    class procedure DeleteSecret(const aName: string); static;
    class function GetDeviceName: string; static;
    class function GetPlatformName: string; static;
    class procedure OpenURL(const URL: string); static;
    class function RetrieveSecret(const aName: string): TSecretData; overload;
        static;
    class function RetrieveSecret(const aName: string; out aUsername,
        aCredentialData: string): Boolean; overload; static;
    class function RetrieveSecret<T: class, constructor>(const aName: string): T;
        overload; static;
    class procedure StoreSecret(const aName, aUsername, aCredentialData: string);
        overload; static;
    class procedure StoreSecret(const aName, aInfo: string; const aCredentialData:
        TObject); overload; static;
  end;

implementation

uses
{$IF Defined(IOS)}
  macapi.helpers, iOSapi.Foundation, FMX.helpers.iOS;
{$ELSEIF Defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  Androidapi.helpers,
{$ELSEIF Defined(MACOS)}
Posix.Stdlib,
{$ELSEIF Defined(MSWINDOWS)}
Winapi.ShellAPI, Winapi.Windows,
{$ENDIF}
  REST.Json, System.JSON;

{$IF Defined(MSWINDOWS)}

  ///  <summary>
  ///  https://docs.microsoft.com/en-us/windows/desktop/api/wincred/ns-wincred-_credentiala
  ///  </summary>

type
  CREDENTIAL_ATTRIBUTE = packed record
    Keyword: LPTSTR;
    Flags: DWORD;
    ValueSize: DWORD;
    Value: LPBYTE;
  end;
  PCREDENTIAL_ATTRIBUTE = ^CREDENTIAL_ATTRIBUTE;

  CREDENTIALW = packed record
    Flags: DWORD;
    Type_: DWORD;
    TargetName: LPTSTR;
    Comment: LPTSTR;
    LastWritten: FILETIME;
    CredentialBlobSize: DWORD;
    CredentialBlob: LPBYTE;
    Persist: DWORD;
    AttributeCount: DWORD;
    Attributes: PCREDENTIAL_ATTRIBUTE;
    TargetAlias: LPTSTR;
    UserName: LPTSTR;
  end;
  PCREDENTIALW = ^CREDENTIALW;

  function CredWriteW(Credential: PCREDENTIALW; Flags: DWORD): BOOL; stdcall; external 'Advapi32.dll';
  function CredReadW(TargetName: LPCWSTR; Type_: DWORD; Flags: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall; external 'Advapi32.dll';
  function CredDeleteW(TargetName: LPCWSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall; external 'Advapi32.dll';
  procedure CredFree(Buffer: PVOID); stdcall; external 'Advapi32.dll';

const
  // Credential type
  CRED_TYPE_GENERIC = 1;
  CRED_TYPE_DOMAIN_PASSWORD = 2;
  CRED_TYPE_DOMAIN_CERTIFICATE = 3;


  // Max targetname length (type dependend)
  CRED_MAX_DOMAIN_TARGET_NAME_LENGTH  = 337;
  CRED_MAX_GENERIC_TARGET_NAME_LENGTH = 32767;

  // Max string length (comment and TargetAlias)
  CRED_MAX_STRING_LENGTH = 256;

  // Max username length
  CRED_MAX_USERNAME_LENGTH = 513;

  // Max Credentialdata length
  CRED_MAX_CREDENTIAL_BLOB_SIZE = 512;

  // Max value size of an attribute
  CRED_MAX_VALUE_SIZE = 256;

  // Persistence of credentials (only for loggod on user)
  CRED_PERSIST_SESSION       = 1;  // Current logon only (removed during logoff)
  CRED_PERSIST_LOCAL_MACHINE = 2;  // This machine (persisted after logoff)
  CRED_PERSIST_ENTERPRISE    = 3;  // This and other machines (roaming)

function CredWriteGenericCredentials(const Target, Username, CredentialData: WideString): Boolean;
var
  Credentials: CREDENTIALW;
begin
  if Length(Target) > CRED_MAX_GENERIC_TARGET_NAME_LENGTH then
    raise Exception.Create('Target name max length exeeded');
  if Length(Username) > CRED_MAX_USERNAME_LENGTH then
    raise Exception.Create('Username max length exeeded');
  if Length(CredentialData) > CRED_MAX_CREDENTIAL_BLOB_SIZE then
    raise Exception.Create('Credential data max length exeeded');

  ZeroMemory(@Credentials, SizeOf(Credentials));
  Credentials.TargetName := PWideChar(Target); // cannot be longer than CRED_MAX_GENERIC_TARGET_NAME_LENGTH (32767) characters. Recommended format "Company_Target"
  Credentials.Type_ := CRED_TYPE_GENERIC;
  Credentials.UserName := PWideChar(Username);
  Credentials.Persist := CRED_PERSIST_LOCAL_MACHINE;
  Credentials.CredentialBlob := PByte(CredentialData);
  Credentials.CredentialBlobSize := SizeOf(WideChar)*(Length(CredentialData)); // By convention no trailing null. Cannot be longer than CRED_MAX_CREDENTIAL_BLOB_SIZE (512) bytes
  Result := CredWriteW(@Credentials, 0);
end;

function CredReadGenericCredentials(const Target: WideString; var Username, Password: WideString): Boolean;
var
  credential: PCREDENTIALW;
  le: DWORD;
  s: string;
begin
  Result := False;

  credential := nil;
  if not CredReadW(PWideChar(Target), CRED_TYPE_GENERIC, 0, {var}credential) then
  begin
    le := GetLastError;
    s := 'Could not get "'+Target+'" generic credentials: '+ SysErrorMessage(le)+' '+IntToStr(le);
    OutputDebugString(PChar(s));
    Exit;
  end;

  try
    username := Credential.UserName;
    // By convention blobs that contain strings do not have a trailing NULL.
    // password := WideCharToWideString(PWideChar(Credential.CredentialBlob), Credential.CredentialBlobSize div 2);
    password := Copy(PWideChar(Credential.CredentialBlob), 1, Credential.CredentialBlobSize div SizeOf(WideChar));
  finally
    CredFree(Credential);
  end;

  Result := True;
end;

function CredDeleteGenericCredentials(const Target: WideString): Boolean;
begin
  Result := CredDeleteW(PWideChar(Target), CRED_TYPE_GENERIC, 0);
end;

{$ENDIF}

constructor TSecretData.Create(const aUsername, aCredentialData: string);
begin
  inherited;
  Username := aUsername;
  CredentialData := aCredentialData;
end;

{ TSecretData }

procedure TSecretData.Clear;
begin
  Username := '';
  CredentialData := '';
end;

{ TXPlatform }

class procedure TXPlatform.DeleteSecret(const aName: string);
begin
  CredDeleteGenericCredentials(aName);
end;

class function TXPlatform.GetDeviceName: string;
{$IF Defined(MSWINDOWS)}
var
  iSize: Cardinal;
{$ENDIF}
begin
  {$IF Defined(MSWINDOWS)}
  iSize := 256;
  SetLength(Result, iSize);
  if GetComputerName(PChar(Result), iSize) then
    SetLength(Result, iSize)
  else
    Result := '';
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class function TXPlatform.GetPlatformName: string;
begin
{$IF Defined(ANDROID)}
  Result := 'Android'
{$ELSEIF Defined(MSWINDOWS)}
  Result := 'Windows'
{$ELSEIF Defined(IOS)}
  Result := 'iOS'
{$ELSEIF Defined(MACOS)}
  Result := 'MaxOS'
{$ELSE}
  Result := 'Unknown';
{$ENDIF}
end;

class procedure TXPlatform.OpenURL(const URL: string);
{$IF Defined(ANDROID)}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IF Defined(ANDROID)}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(URL));
  tandroidhelper.Activity.startActivity(Intent);
  // SharedActivity.startActivity(Intent);
{$ELSEIF Defined(MSWINDOWS)}
  ShellExecute(0, 'open', PWideChar(URL), nil, nil, SW_SHOWNORMAL);
{$ELSEIF Defined(IOS)}
  SharedApplication.OpenURL(StrToNSUrl(URL));
{$ELSEIF Defined(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(URL)));
{$ENDIF}
end;

class function TXPlatform.RetrieveSecret(const aName: string): TSecretData;
var
  s1, s2: WideString;
begin
  if CredReadGenericCredentials(aName, s1, s2) then
  begin
    Result.Username := s1;
    Result.CredentialData := s2;
  end else
    Result.Clear;
end;

class function TXPlatform.RetrieveSecret(const aName: string; out aUsername,
    aCredentialData: string): Boolean;
var
  s1, s2: WideString;
begin
  Result := CredReadGenericCredentials(aName, s1, s2);
  if Result then
  begin
    aUsername := s1;
    aCredentialData := s2;
  end;
end;

class function TXPlatform.RetrieveSecret<T>(const aName: string): T;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := TJSONObject.ParseJSONValue(RetrieveSecret(aName).CredentialData);
  try
    Result := TJson.JsonToObject<T>(LJSONValue as TJSONObject)
  finally
    LJSONValue.Free;
  end;
end;

class procedure TXPlatform.StoreSecret(const aName, aUsername, aCredentialData:
    string);
begin
  CredWriteGenericCredentials(aName, aUsername, aCredentialData);
end;

class procedure TXPlatform.StoreSecret(const aName, aInfo: string; const
    aCredentialData: TObject);
begin
  StoreSecret(aName, aInfo, TJson.ObjectToJsonString(aCredentialData));
end;

end.
