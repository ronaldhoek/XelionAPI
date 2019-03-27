unit FormMainU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  REST.Client, REST.Authenticator.Basic, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, Xelion.DataModel, Xelion.Classes.Request,
  Xelion.Classes.Response;

type
  TXelionTokenInfo = class
  private
    FExpires: string;
    FToken: string;
  public
    property Token: string read FToken write FToken;
    property Expires: string read FExpires write FExpires;
  end;

  TForm1 = class(TForm)
    btnLogon: TButton;
    reqLogon: TRESTRequest;
    respLogon: TRESTResponse;
    Memo1: TMemo;
    RESTClient1: TRESTClient;
    btnLogout: TButton;
    btnTestGet: TButton;
    edtResource: TEdit;
    edtHost: TEdit;
    edtTenant: TEdit;
    lblHost: TLabel;
    lblTenant: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    lblUsername: TLabel;
    lblPassword: TLabel;
    edtToken: TEdit;
    lblToken: TLabel;
    Label1: TLabel;
    btnTest: TButton;
    btnRenew: TButton;
    edtTokenExpires: TEdit;
    mmMonitor: TMemo;
    tmrMonitor: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnLogonClick(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
    procedure btnRenewClick(Sender: TObject);
    procedure btnTestGetClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure tmrMonitorTimer(Sender: TObject);
  private
    FSessionAuthToken: TXelionTokenInfo;
    procedure CheckAuthenticated;
    procedure Logon(const aUserName, aPassword: string);
    procedure LogonRenew;
    procedure Logout;
    procedure UpdateSessionAuthToken(const aLoginResponse: TLoginResponse);
    procedure UpdateTokenInfo;
  public
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

uses
  REST.Json, System.JSON,
  Xelion.Connection, Xelion.Requests, System.IniFiles,
  System.IOUtils, XPlatform;

const
  sSecretNameToken = 'ComponentAgro.XelionMonitor.Token';

  sSectionSettings = 'Settings';

  sIdentToken = 'Token';
  sIdentTokenExpires = 'TokenExpires';

function Iif(Condition: Boolean; const ATrue, AFalse: string): string;
begin
  if Condition then
    Result := ATrue
  else
    Result := AFalse;
end;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSessionAuthToken := TXPlatform.RetrieveSecret<TXelionTokenInfo>(sSecretNameToken);
  if (FSessionAuthToken <> nil) and (FSessionAuthToken.Token = '') then
    FreeAndNil(FSessionAuthToken);
  UpdateTokenInfo;
end;

procedure TForm1.btnLogonClick(Sender: TObject);
begin
  Logon(edtUsername.Text, edtPassword.Text);
end;

procedure TForm1.btnLogoutClick(Sender: TObject);
begin
  Logout;
end;

procedure TForm1.btnRenewClick(Sender: TObject);
begin
  LogonRenew;
end;

procedure TForm1.btnTestGetClick(Sender: TObject);
var
  conn: TXelionConnection;
begin
  CheckAuthenticated;
  Memo1.Lines.Clear;

  conn := TXelionConnection.Create(edtHost.Text, edtTenant.Text);
  try
    conn.AuthToken := FSessionAuthToken.Token;
    Memo1.Lines.Add(conn.Execute(edtResource.Text, rtGet));
  finally
    conn.Free;
  end;
end;

procedure TForm1.btnTestClick(Sender: TObject);

  procedure ShowLinks(aRestObj: TRESTObject);
  var
    lnk: TLink;
  begin
    Memo1.Lines.Add('Link count: ' + Length(aRestObj.links).ToString);
    for lnk in aRestObj.links do
      Memo1.Lines.Add('- ' + lnk.rel + '-' + lnk.method + ':' + lnk.href );
  end;

var
  ar: TAddressablesResponse;
  conn: TXelionConnection;
  sr: TStatusResponse;
  cr: TCommunicationsResponse;
  comm: TCommunicationData;
  psr: TPhoneSettingsResponse;
  addr: TAddressableData;
  call: TCallableData;
  clr: TCallablesResponse;
  ir: TInfoResponse;
  pr: TPresenceResponse;
  pres: TPresenceData;
begin
  CheckAuthenticated;
  Memo1.Lines.Clear;

  conn := TXelionConnection.Create(edtHost.Text, edtTenant.Text);
  try
    conn.AuthToken := FSessionAuthToken.Token;

    sr := conn.Execute<TStatusResponse>('me/status', rtGet);
    try
      Memo1.Lines.Add('me/status: ' + sr.Object_);
//      ShowLinks(sr);
      Memo1.Lines.Add('');
    finally
      sr.Free;
    end;

    ir := conn.Execute<TInfoResponse>('me/info', rtGet);
    try
      Memo1.Lines.Add('me/info: ' + ir.object_.userId.ToString + ', ' + ir.object_.person.commonName + ' (' + ir.object_.presenceInfo.status + ')');
//      ShowLinks(ir);
      Memo1.Lines.Add('');
    finally
      ir.Free;
    end;

    psr := conn.Execute<TPhoneSettingsResponse>('me/phone/settings', rtGet);
    try
      Memo1.Lines.Add('me/phone/settings');
      Memo1.Lines.Add('Redirect: ' + psr.object_.redirection + ' (Active: ' + psr.object_.redirectionActive.ToString() + ')');
//      ShowLinks(psr);
      Memo1.Lines.Add('');
    finally
      psr.Free;
    end;

    cr := conn.Execute<TCommunicationsResponse>('communications', rtGet);
    try
      Memo1.Lines.Add('Communications: ' + Length(cr.data).ToString);
      for comm in cr.data do
      begin
        Memo1.Lines.Add('- ' + comm.object_.oid.ToString + ' (' + comm.object_.Date + ') ' +
          comm.object_.commonName + ' (' + comm.object_.status + Iif(comm.object_.displayed, 'T','F') + ')');
        //ShowLinks(comm);
      end;
//      ShowLinks(cr.meta);
      Memo1.Lines.Add('');
    finally
      cr.Free;
    end;

    ar := conn.Execute<TAddressablesResponse>('addressables', rtGet);;
    try
      Memo1.Lines.Add('Addressables: ' + Length(ar.data).ToString);
      for addr in ar.data do
      begin
        Memo1.Lines.Add('- ' + addr.object_.commonName + ' (' + addr.object_.oid.ToString + ') ' + addr.object_.objectType);
        //ShowLinks(addr);
      end;
//      ShowLinks(ar.meta);
      Memo1.Lines.Add('');
    finally
      ar.Free;
    end;

    clr := conn.Execute<TCallablesResponse>('callables', rtGet);;
    try
      Memo1.Lines.Add('Callables: ' + Length(clr.data).ToString);
      for call in clr.data do
      begin
        Memo1.Lines.Add('- ' + call.object_.commonName + ' (' + call.object_.chatSessionId + ') ' + call.object_.phoneNumber);
        if call.object_.isUser and Assigned(call.object_.presenceInfo) then
          Memo1.Lines.Add('  * status: ' + call.object_.presenceInfo.status);
        //ShowLinks(addr);
      end;
//      ShowLinks(ar.meta);
      Memo1.Lines.Add('');
    finally
      clr.Free;
    end;

    pr := conn.Execute<TPresenceResponse>('presence', rtGet);;
    try
      Memo1.Lines.Add('Presence: ' + Length(pr.data).ToString);
      for pres in pr.data do
      begin
        Memo1.Lines.Add('- ' + pres.object_.name + ' (' + pres.object_.id + ') ' + pres.object_.status);
        //ShowLinks(addr);
      end;
//      ShowLinks(ar.meta);
      Memo1.Lines.Add('');
    finally
      pr.Free;
    end;

  finally
    conn.Free;
  end;
end;

procedure TForm1.CheckAuthenticated;
begin
  if (FSessionAuthToken = nil) then
    raise Exception.Create('Login first!');
end;

destructor TForm1.Destroy;
begin
  FreeAndNil(FSessionAuthToken);
  inherited;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  TXPlatform.OpenURL((Sender as TLabel).Text);
end;

procedure TForm1.Logon(const aUserName, aPassword: string);
var
  conn: TXelionConnection;
  lc: TLoginCredentials;
  lr: TLoginResponse;
begin
  if FSessionAuthToken <> nil then
    Logout;

  conn := TXelionConnection.Create(edtHost.Text, edtTenant.Text);
  try
    lc := TLoginCredentials.Create(aUsername, aPassword,
              TXPlatform.GetPlatformName + '-' + TXPlatform.GetDeviceName);
    try
      lr := conn.Execute<TLoginResponse>('me/login', rtPost, lc);
      try
        UpdateSessionAuthToken(lr);
      finally
        lr.Free;
      end;
    finally
      lc.Free;
    end;

  finally
    conn.Free;
  end;
end;

procedure TForm1.LogonRenew;
var
  conn: TXelionConnection;
  lr: TLoginResponse;

  function GetJsonString(const s: string): string;
  begin
    with TJSONString.Create(s) do
    try
      Result := ToString;
    finally
      Free;
    end;
  end;

begin
  CheckAuthenticated;
  if edtPassword.Text = '' then
    raise Exception.Create('Password required');

  conn := TXelionConnection.Create(edtHost.Text, edtTenant.Text);
  try
    conn.AuthToken := FSessionAuthToken.Token;
    lr := conn.Execute<TLoginResponse>('me/renew', rtPost, GetJsonString(edtPassword.Text), TRESTContentType.ctAPPLICATION_JSON);
    try
      UpdateSessionAuthToken(lr);
    finally
      lr.Free;
    end;
  finally
    conn.Free;
  end;
end;

procedure TForm1.Logout;
var
  conn: TXelionConnection;
begin
  CheckAuthenticated;
  conn := TXelionConnection.Create(edtHost.Text, edtTenant.Text);
  try
    conn.AuthToken := FSessionAuthToken.Token;
    try
      conn.Execute('/me/logout', rtPost);
    except
      Application.HandleException(Self);
    end;
    UpdateSessionAuthToken(nil); // Token altijd wissen
  finally
    conn.Free;
  end;
end;

procedure TForm1.tmrMonitorTimer(Sender: TObject);
var
  comm, cr_Ex: TCommunicationData;
  conn: TXelionConnection;
  cr: TCommunicationsResponse;
begin
  if FSessionAuthToken = nil then
    Exit;

  mmMonitor.Lines.Clear;
  conn := TXelionConnection.Create(edtHost.Text, edtTenant.Text);
  try
    conn.AuthToken := FSessionAuthToken.Token;
    cr := conn.Execute<TCommunicationsResponse>('communications', rtGet);
    try
      for comm in cr.data do
        if comm.object_.durationSec = 0 then
      begin
        // Uitgebreide informatie opvragen om te bepalen of het een
        // (nog niet) opgenomen oproep is (en nog niet opgehangen uiteraard).
        cr_ex := conn.Execute<TCommunicationData>('communications/' + comm.object_.oid.ToString, rtGet);
        if cr_ex <> nil then
        try
          if cr_ex.object_.callEndTime = '' then
            mmMonitor.Lines.Add('- ' + cr_ex.object_.oid.ToString + ' (' + cr_ex.object_.Date + ') ' + cr_ex.object_.commonName + ' (' + cr_ex.object_.status + ')');
        finally
          cr_ex.Free;
        end;
      end;
    finally
      cr.Free;
    end;
  finally
    conn.Free;
  end;
end;

procedure TForm1.UpdateSessionAuthToken(const aLoginResponse: TLoginResponse);
begin
  if aLoginResponse = nil then
  begin
    FreeAndNil(FSessionAuthToken);
    TXPlatform.DeleteSecret(sSecretNameToken);
  end else
  begin
    if aLoginResponse.authentication = '' then
      raise Exception.Create('Invalid authentication token');
    if FSessionAuthToken = nil then
      FSessionAuthToken := TXelionTokenInfo.Create;

    // Ovenemen en data opslaan
    FSessionAuthToken.Token := aLoginResponse.authentication;
    FSessionAuthToken.Expires := aLoginResponse.validUntil;
    TXPlatform.StoreSecret(sSecretNameToken, 'Personal Access Token', FSessionAuthToken);
  end;
  UpdateTokenInfo;
end;

procedure TForm1.UpdateTokenInfo;
begin
  // Even weergeven
  if FSessionAuthToken = nil then
  begin
    edtToken.Text := '';
    edtTokenExpires.Text := '';
  end else
  begin
    edtToken.Text := FSessionAuthToken.Token;
    edtTokenExpires.Text := FSessionAuthToken.Expires;
  end;
end;

end.
