unit Xelion.Requests;

interface

uses
  Xelion.Connection;

type
  TXelionRequest = class sealed
  private
    FData: TObject;
    FRequestType: TXelionRequestType;
    FResource: string;
  public
    destructor Destroy; override;
    class function Login(aPostData: TObject): TXelionRequest;
    property Data: TObject read FData;
    property RequestType: TXelionRequestType read FRequestType;
    property Resource: string read FResource;
  end;

implementation

uses
  System.SysUtils;

destructor TXelionRequest.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

class function TXelionRequest.Login(aPostData: TObject): TXelionRequest;
begin
  Result := Self.Create;
  Result.FResource := 'me/login';
  Result.FRequestType := rtPost;
  Result.FData := aPostData;
end;

end.
