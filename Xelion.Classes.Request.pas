unit Xelion.Classes.Request;

interface

  (*
    Bevragings objecten (resources)

    https://partner.xelion.com/files/public/user-guide/resources.html
  *)

type
  (*
    Request data
  *)

  TLoginCredentials = class
  private
    FclientId: string;
    Fpassword: string;
    FuserName: string;
    FuserSpace: string;
  public
    constructor Create(const aUserName, aPassword, aUserSpace: string; const
        aClientID: string = '');
    property clientId: string read FclientId write FclientId;
    property userName: string read FuserName write FuserName;
    property password: string read Fpassword;
    property userSpace: string read FuserSpace write FuserSpace;
  end;

  (*
    Request objects
  *)

implementation

{ TLoginCredentials }

constructor TLoginCredentials.Create(const aUserName, aPassword, aUserSpace:
    string; const aClientID: string = '');
begin
  inherited Create;
  FuserName := aUserName;
  Fpassword := aPassword;
  FuserSpace := aUserSpace;
  FclientId := aClientID;
end;

end.
