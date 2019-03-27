unit Xelion.Connection;

interface

uses
  REST.Client, REST.Types;

type
  TXelionApiVerion = (v1_0);

  TXelionRequestType = (rtGet, rtPost, gtPut, rtUpdate, rtDelete);

  TRESTContentType = REST.Types.TRESTContentType;

  TXelionConnection = class
  private
    FApiVerion: TXelionApiVerion;
    FAuthToken: string;
    FBaseURL: string;
    FRestClient: TRESTClient;
    FTenant: string;
    function GetApiURL: string;
    function GetRestMethod(aType: TXelionRequestType): TRESTRequestMethod;
    function InternalExecute(const aResource, aRequestData: string; aType:
        TXelionRequestType; aContentType: TRESTContentType): TCustomRESTResponse;
        overload;
    function InternalExecute(const aResource: string; aRequestData: TObject; aType:
        TXelionRequestType): TCustomRESTResponse; overload;
  public
    constructor Create(const aBaseURL: string; const aTenant: string = '');
    destructor Destroy; override;
    function Execute(const aResource: string; aType: TXelionRequestType; const
        aRequestData: string; aContentType: TRESTContentType = ctNone): string;
        overload;
    function Execute(const aResource: string; aType: TXelionRequestType;
        aRequestData: TObject = nil): string; overload;
    function Execute<T: class, constructor>(const aResource: string; aType:
        TXelionRequestType; const aRequestData: string; aContentType:
        TRESTContentType = ctNone): T; overload;
    function Execute<T: class, constructor>(const aResource: string; aType:
        TXelionRequestType; aRequestData: TObject = nil): T; overload;
    property ApiVerion: TXelionApiVerion read FApiVerion write FApiVerion;
    property AuthToken: string read FAuthToken write FAuthToken;
  end;

implementation

uses
  System.SysUtils, System.JSON, REST.Json;

const
  sVersionPath: array[TXelionApiVerion] of string = ('v1');

constructor TXelionConnection.Create(const aBaseURL: string; const aTenant:
    string = '');
begin
  inherited Create;
  FRestClient := TRESTClient.Create(nil);
  FBaseURL := aBaseURL;
  if (FBaseURL > '') and (FBaseURL[Length(FBaseURL)] <> '/') then
    FBaseURL := FBaseURL + '/';
  FTenant := aTenant;
end;

destructor TXelionConnection.Destroy;
begin
  inherited;
  FreeAndNil(FRestClient);
end;

function TXelionConnection.Execute(const aResource: string; aType:
    TXelionRequestType; const aRequestData: string; aContentType:
    TRESTContentType = ctNone): string;
var
  resp: TCustomRESTResponse;
begin
  resp := InternalExecute(aResource, aRequestData, aType, aContentType);
  try
    if resp.StatusCode = 200 then
      Result := resp.Content
    else
      raise Exception.CreateFmt('Response error code %d: %s', [resp.StatusCode, resp.Content]);
  finally
    resp.Free;
  end;
end;

function TXelionConnection.Execute(const aResource: string; aType:
    TXelionRequestType; aRequestData: TObject = nil): string;
var
  resp: TCustomRESTResponse;
begin
  resp := InternalExecute(aResource, aRequestData, aType);
  try
    if resp.StatusCode = 200 then
      Result := resp.Content
    else
      raise Exception.CreateFmt('Response error code %d: %s', [resp.StatusCode, resp.Content]);
  finally
    resp.Free;
  end;
end;

function TXelionConnection.Execute<T>(const aResource: string; aType:
    TXelionRequestType; const aRequestData: string; aContentType:
    TRESTContentType = ctNone): T;
var
  resp: TCustomRESTResponse;
begin
  resp := InternalExecute(aResource, aRequestData, aType, aContentType);
  try
    if resp.StatusCode = 200 then
      Result := TJson.JsonToObject<T>(resp.JSONValue as TJSONObject)
    else
      raise Exception.CreateFmt('Response error code %d: %s', [resp.StatusCode, resp.Content]);
  finally
    resp.Free;
  end;
end;

function TXelionConnection.Execute<T>(const aResource: string; aType:
    TXelionRequestType; aRequestData: TObject = nil): T;
var
  resp: TCustomRESTResponse;
begin
  resp := InternalExecute(aResource, aRequestData, aType);
  try
    if resp.StatusCode = 200 then
      Result := TJson.JsonToObject<T>(resp.JSONValue as TJSONObject)
    else
      raise Exception.CreateFmt('Response error code %d: %s', [resp.StatusCode, resp.Content]);
  finally
    resp.Free;
  end;
end;

function TXelionConnection.GetApiURL: string;
begin
  Result := FBaseURL + 'api/' + sVersionPath[FApiVerion];
  if FTenant > '' then Result := Result + '/' + FTenant;
end;

function TXelionConnection.GetRestMethod(aType: TXelionRequestType):
    TRESTRequestMethod;
const
//  TXelionRequestType = (rtGet, rtPost, gtPut, rtUpdate, rtDelete);
  _Method: array[TXelionRequestType] of TRESTRequestMethod =
    (rmGET, rmPOST, rmPUT, rmPATCH, rmDELETE);
begin
  Result := _Method[aType];
end;

function TXelionConnection.InternalExecute(const aResource, aRequestData:
    string; aType: TXelionRequestType; aContentType: TRESTContentType):
    TCustomRESTResponse;
var
  req: TRESTRequest;
begin
  // Get correct URL
  FRestClient.ResetToDefaults;
  FRestClient.BaseURL := GetApiURL;

  // Do we have some authentication set
  if FAuthToken > '' then
    FRestClient.Params.AddItem('Authorization', 'xelion ' + FAuthToken,
      TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]); // <= DO NOT URL ENCODE

  req := TRESTRequest.Create(nil);
  try
    req.Client := FRestClient;
    req.Method := GetRestMethod(aType);
    req.Resource := aResource;
    req.Response := TRESTResponse.Create(nil);
    try
      if aRequestData > '' then
        req.AddBody(aRequestData, AContentType);

      req.Execute;
      // Response object overnemen!
      Result := req.Response;
    except
      req.Response.Free;
      raise;
    end;
  finally
    req.Free;
  end;
end;

function TXelionConnection.InternalExecute(const aResource: string;
    aRequestData: TObject; aType: TXelionRequestType): TCustomRESTResponse;
var
  o: TJSONObject;
  req: TRESTRequest;
begin
  // Get correct URL
  FRestClient.ResetToDefaults;
  FRestClient.BaseURL := GetApiURL;

  // Do we have some authentication set
  if FAuthToken > '' then
    FRestClient.Params.AddItem('Authorization', 'xelion ' + FAuthToken,
      TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]); // <= DO NOT URL ENCODE

  req := TRESTRequest.Create(nil);
  try
    req.Client := FRestClient;
    req.Method := GetRestMethod(aType);
    req.Resource := aResource;
    req.Response := TRESTResponse.Create(nil);
    try
      if aRequestData = nil then
        req.Execute
      else begin
        o := TJson.ObjectToJsonObject(aRequestData);
        try
          req.AddBody(o);
          req.Execute;
        finally
          o.Free;
        end;
      end;
      // Response object overnemen!
      Result := req.Response;
    except
      req.Response.Free;
      raise;
    end;
  finally
    req.Free;
  end;
end;

end.
