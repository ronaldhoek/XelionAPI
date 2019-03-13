unit Xelion.Classes.Response;

interface

uses
  Xelion.Types, Xelion.DataModel;

  (*
    REST response object wrappers
  *)

type
  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/HATEOASLink.html
  ///  </summary>
  THATEOAsLink = class
  strict private
    Fhref: string;
    Fmethod: string;
    Frel: string;
  public
    property href: string read Fhref;
    property method: string read Fmethod;
    property rel: string read Frel;
  end;

  TLink = THATEOAsLink;

  TLinkArray = array of THATEOAsLink;

  (*
    Base response objects

    LET OP!
    Dit werkt niet goed, wanneer de properties op een generics actige wijze
    (bijv. TRESTObject<T>.object_: T) worden gedefinieerd.
  *)

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/RESTObject.html
  ///  </summary>
  TRESTObject = class
  strict private
    Flinks: TLinkArray;
  public
    destructor Destroy; override;
    property links: TLinkArray read Flinks;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/RESTList.Paging.html
  ///  </summary>
  TRESTListPaging = class
  strict private
    FpreviousId: TXelionOID;
    FnextId: TXelionOID;
  public
    property previousId: TXelionOID read FpreviousId;
    property nextId: TXelionOID read FnextId;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/RESTList.MetaData.html
  ///  </summary>
  TRESTListMetaData = class(TRESTObject)
  strict private
    Fpaging: TRESTListPaging;
  public
    destructor Destroy; override;
    property paging: TRESTListPaging read Fpaging;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/RESTList.html
  ///  </summary>
  TRESTList = class
  strict private
    Fmeta: TRESTListMetaData;
  public
    destructor Destroy; override;
    property meta: TRESTListMetaData read Fmeta;
  end;

  (*
    Specific response classes
  *)

  TLoginResponse = class
  strict private
    Fauthentication: string;
    FvalidUntil: string;
  private
    Fbrand: string;
    FbuildNumber: string;
    FserverVersion: string;
    Fversion: string;
  public
    property authentication: string read Fauthentication;
    property brand: string read Fbrand;
    property buildNumber: string read FbuildNumber;
    property serverVersion: string read FserverVersion;
    property validUntil: string read FvalidUntil;
    property version: string read Fversion;
  end;

  ///  <summary>
  ///  me/status
  ///  </summary>
  TStatusResponse = class(TRESTObject)
  strict private
    Fobject: string;
  public
    property object_: string read Fobject;
  end;


  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/AddressablePresenceInfo.html
  ///  </summary>
  TAddressablePresenceInfo = class
  strict private
    Fbusy: Boolean;
    Flocation: string;
    Fmessage: string;
    Fstatus: string;
    FstatusIcon: string;
  public
    property busy: Boolean read Fbusy;
    property location: string read Flocation;
    property message: string read Fmessage;
    property status: string read Fstatus;
    property statusIcon: string read FstatusIcon;
  end;

  TInfoData = class
  strict private
    Fperson: TPerson;
    FpresenceInfo: TAddressablePresenceInfo;
    FuserId: TXelionOID;
  public
    destructor Destroy; override;
    property person: TPerson read Fperson;
    property userId: TXelionOID read FuserId;
    property presenceInfo: TAddressablePresenceInfo read FpresenceInfo;
  end;

  ///  <summary>
  ///  me/info
  ///  </summary>
  TInfoResponse = class(TRESTObject)
  strict private
    Fobject: TInfoData;
  public
    destructor Destroy; override;
    property object_: TInfoData read Fobject;
  end;

  TCommunicationData = class(TRESTObject)
  strict private
    Fobject: TCommunication;
  public
    destructor Destroy; override;
    property object_: TCommunication read Fobject;
  end;

  TCommunications = array of TCommunicationData;

  TCommunicationsResponse = class(TRESTList)
  strict private
    Fdata: TCommunications;
  public
    destructor Destroy; override;
    property data: TCommunications read Fdata;
  end;

  (*
    Phone settings response
  *)

  TPhoneSettings = class
  strict private
    Fanonymous: Boolean;
    FdoNotDisturb: Boolean;
    FfallbackActive: Boolean;
    Fmessage: string;
    Foid: TXelionOID;
    Fredirection: string;
    FredirectionActive: Boolean;
    FredirectionTimeout: Integer;
    FvoicemailActive: Boolean;
  public
    property anonymous: Boolean read Fanonymous;
    property doNotDisturb: Boolean read FdoNotDisturb;
    property fallbackActive: Boolean read FfallbackActive;
    property message: string read Fmessage;
    property oid: TXelionOID read Foid;
    property redirection: string read Fredirection;
    property redirectionActive: Boolean read FredirectionActive;
    property redirectionTimeout: Integer read FredirectionTimeout;
    property voicemailActive: Boolean read FvoicemailActive;
  end;

  TPhoneSettingsResponse = class(TRESTObject)
  strict private
    Fobject: TPhoneSettings;
  public
    destructor Destroy; override;
    property object_: TPhoneSettings read Fobject;
  end;


  TAddressableData = class(TRESTObject)
  strict private
    Fobject: TAddressable;
  public
    destructor Destroy; override;
    property object_: TAddressable read Fobject;
  end;

  TAddressables = array of TAddressableData;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/resources/addressables_list.html
  ///  </summary>
  TAddressablesResponse = class(TRESTList)
  strict private
    Fdata: TAddressables;
  public
    destructor Destroy; override;
    property data: TAddressables read Fdata;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/AddressableInfo.html
  ///  </summary>
  TAddressableInfo = class
  strict private
    Faddressable: TAddressable;
    FchatSessionId: string;
    FcommonName: string;
    Finformation: string;
    FisUser: Boolean;
    Forganisation: TOrganisation;
    FphoneNumber: string;
    FpresenceInfo: TAddressablePresenceInfo;
  public
    destructor Destroy; override;
    property addressable: TAddressable read Faddressable;
    property chatSessionId: string read FchatSessionId;
    property commonName: string read FcommonName;
    property information: string read Finformation;
    property isUser: Boolean read FisUser;
    property organisation: TOrganisation read Forganisation;
    property phoneNumber: string read FphoneNumber;
    property presenceInfo: TAddressablePresenceInfo read FpresenceInfo;
  end;

  TCallableData = class(TRESTObject)
  strict private
    Fobject: TAddressableInfo;
  public
    destructor Destroy; override;
    property object_: TAddressableInfo read Fobject;
  end;

  TCallables = array of TCallableData;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/resources/addressables_callables.html
  ///  </summary>
  TCallablesResponse = class(TRESTList)
  strict private
    Fdata: TCallables;
  public
    destructor Destroy; override;
    property data: TCallables read Fdata;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/Presence.html
  ///  </summary>
  TPresence = class
  strict private
    Faddressable: TAddressable;
    Fbusy: Boolean;
    Fid: string;
    Flocation: string;
    Fmessage: string;
    Fname: string;
    Fstatus: string;
  public
    destructor Destroy; override;
    property addressable: TAddressable read Faddressable;
    property busy: Boolean read Fbusy;
    property id: string read Fid;
    property location: string read Flocation;
    property message: string read Fmessage;
    property name: string read Fname;
    property status: string read Fstatus;
  end;

  TPresenceData = class(TRESTObject)
  strict private
    Fobject: TPresence;
  public
    destructor Destroy; override;
    property object_: TPresence read Fobject;
  end;

  TPresences = array of TPresenceData;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/resources/addressables_presence.html
  ///  </summary>
  TPresenceResponse = class(TRESTList)
  strict private
    Fdata: TPresences;
  public
    destructor Destroy; override;
    property data: TPresences read Fdata;
  end;

implementation

{ TRESTObject }

destructor TRESTObject.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Flinks) - 1  do
    Flinks[i].Free;
  inherited;
end;

{ TRESTListMetaData }

destructor TRESTListMetaData.Destroy;
begin
  Fpaging.Free;
  inherited;
end;

{ TRESTList }

destructor TRESTList.Destroy;
begin
  Fmeta.Free;
  inherited;
end;

{ TInfoData }

destructor TInfoData.Destroy;
begin
  Fperson.Free;
  FpresenceInfo.Free;
  inherited;
end;

{ TInfoResponse }

destructor TInfoResponse.Destroy;
begin
  Fobject.Free;
  inherited;
end;

{ TCommunicationData }

destructor TCommunicationData.Destroy;
begin
  Fobject.Free;
  inherited;
end;

{ TCommunicationsResponse }

destructor TCommunicationsResponse.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fdata) - 1  do
    Fdata[i].Free;
  inherited;
end;

{ TPhoneSettingsResponse }

destructor TPhoneSettingsResponse.Destroy;
begin
  Fobject.Free;
  inherited;
end;

{ TAddressableData }

destructor TAddressableData.Destroy;
begin
  Fobject.Free;
  inherited;
end;

{ TAddressablesResponse }

destructor TAddressablesResponse.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fdata) - 1  do
    Fdata[i].Free;
  inherited;
end;

{ TAddressableInfo }

destructor TAddressableInfo.Destroy;
begin
  Faddressable.Free;
  Forganisation.Free;
  FpresenceInfo.Free;
  inherited;
end;

{ TCallableData }

destructor TCallableData.Destroy;
begin
  Fobject.Free;
  inherited;
end;

{ TCallablesResponse }

destructor TCallablesResponse.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fdata) - 1  do
    Fdata[i].Free;
  inherited;
end;

{ TPresence }

destructor TPresence.Destroy;
begin
  Faddressable.Free;
  inherited;
end;

{ TPresenceData }

destructor TPresenceData.Destroy;
begin
  Fobject.Free;
  inherited;
end;

{ TPresenceResponse }

destructor TPresenceResponse.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fdata) - 1  do
    Fdata[i].Free;
  inherited;
end;

end.
