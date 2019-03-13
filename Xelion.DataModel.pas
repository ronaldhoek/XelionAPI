unit Xelion.DataModel;

interface

uses
  System.Types, Xelion.Types;

  (*
    Xelion Data model

    https://partner.xelion.com/files/public/user-guide/model_reference.html
  *)

type
  (*
    Basic classes
  *)

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/XelionObject.html
  ///  </summary>
  TXelionObject = class
  private
    FchangeType: string;
    Fkeywords: TStringDynArray;
    FobjectType: string;
    Foid: TXelionOID;
  public
    property changeType: string read FchangeType;
    property keywords: TStringDynArray read Fkeywords;
    property objectType: string read FobjectType;
    property oid: TXelionOID read Foid;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/Entity.html
  ///  </summary>
  TEntity = class(TXelionObject)
  private
    Fcomment: string;
    FcommonName: string;
    FiconId: string;
    Fpermissions: string;
  public
    property comment: string read Fcomment;
    property commonName: string read FcommonName;
    property iconId: string read FiconId;
    property permissions: string read Fpermissions;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/TelecomAddress.html
  ///  </summary>
  TTelecomAddress = class(TXelionObject)
  private
    Faddress: string;
    FaddressType: string;
    FcommonName: string;
    Flabel: string;
    ForderNumber: string;
  public
    property address: string read Faddress;
    property addressType: string read FaddressType;
    property commonName: string read FcommonName;
    property label_: string read Flabel;
    property orderNumber: string read ForderNumber;
  end;

  TTelecomAddresses = array of TTelecomAddress;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/DeliveryAddress.html
  ///  </summary>
  TDeliveryAddress = class(TXelionObject)
  private
    FadditionalInformation: string;
    FaddressType: string;
    Fcity: string;
    Fcomments: string;
    FcommonName: string;
    Fcountry: string;
    Fstate: string;
    Fstreet: string;
    FstreetNumber: string;
    FzipCode: string;
  public
    property additionalInformation: string read FadditionalInformation;
    property addressType: string read FaddressType;
    property city: string read Fcity;
    property comments: string read Fcomments;
    property commonName: string read FcommonName;
    property country: string read Fcountry;
    property state: string read Fstate;
    property street: string read Fstreet;
    property streetNumber: string read FstreetNumber;
    property zipCode: string read FzipCode;
  end;

  TDeliveryAddresses = array of TDeliveryAddress;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/Referable.html
  ///  </summary>
  TReferable = class(TEntity)
    // Nothing extra
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/XCCPhoneLine.html
  ///  </summary>
  TPhoneLine = class(TEntity)
  private
    FcallerName: string;
    FexternalNumber: string;
  public
    property callerName: string read FcallerName;
    property externalNumber: string read FexternalNumber;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/Addressable.html
  ///  </summary>
  TAddressable = class(TReferable)
  private
    Fbusy: Boolean;
    FnrCommunications: Integer;
    Fstatus: string;
  public
    property busy: Boolean read Fbusy;
    property nrCommunications: Integer read FnrCommunications;
    property status: string read Fstatus;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/Addressee.html
  ///  </summary>
  TAddressee = class(TXelionObject)
  private
    Faddress: string;
    Faddressable: TAddressable;
    FcommonName: string;
    Frole: string;
  public
    destructor Destroy; override;
    property address: string read Faddress;
    property addressable: TAddressable read Faddressable;
    property commonName: string read FcommonName;
    property role: string read Frole;
  end;

  TAddressees = array of TAddressee;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/Person.html
  ///  </summary>
  TPerson = class(TAddressable)
  private
    FadditionalName: string;
    Faddresses: TDeliveryAddresses;
    FcommunicationLanguage: string;
    FfamilyName: string;
    Fgender: string;
    FgivenName: string;
    Finitials: string;
    FnamePrefix: string;
    FnameSuffix: string;
    FtelecomAddresses: TTelecomAddresses;
    Ftitle: string;
  public
    destructor Destroy; override;
    property additionalName: string read FadditionalName;
    property communicationLanguage: string read FcommunicationLanguage;
    property familyName: string read FfamilyName;
    property gender: string read Fgender;
    property givenName: string read FgivenName;
    property initials: string read Finitials;
    property namePrefix: string read FnamePrefix;
    property nameSuffix: string read FnameSuffix;
    property title: string read Ftitle;
    property telecomAddresses: TTelecomAddresses read FtelecomAddresses;
    property addresses: TDeliveryAddresses read Faddresses;
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/Organisation.html
  ///  </summary>
  TOrganisation = class(TAddressable)
  private
    Faddresses: TDeliveryAddresses;
    FfieldOfBusiness: string;
    Fname: string;
    FtelecomAddresses: TTelecomAddresses;
  public
    destructor Destroy; override;
    property addresses: TDeliveryAddresses read Faddresses;
    property fieldOfBusiness: string read FfieldOfBusiness;
    property name: string read Fname;
    property telecomAddresses: TTelecomAddresses read FtelecomAddresses;
    // btwNumber
    // kvkNumber
  end;

  ///  <summary>
  ///  https://partner.xelion.com/files/public/user-guide/model/Communication.html
  ///  </summary>
  TCommunication = class(TEntity)
  private
    FcallAnswerTime: string;
    FcallAnswerTimeSec: Integer;
    FcallEndTime: string;
    FcallFlowId: string;
    FcallId: string;
    Fcontents: string;
    Fdate: string;
    Fdisplayed: Boolean;
    FdurationSec: Integer;
    Fflagged: Boolean;
    Fincoming: Boolean;
    FisConferenceCall: Boolean;
    FonHoldDuration: Integer;
    Fparticipants: TAddressees;
    Fphone: string;
    FphoneLine: TPhoneLine;
    Fretrieved: Boolean;
    Fstatus: string;
    Fsubject: string;
    Ftrunk: string;
    Fvoicemail: Boolean;
    FvoicemailIsHeard: Boolean;
  public
    destructor Destroy; override;
    property callAnswerTime: string read FcallAnswerTime;
    property callAnswerTimeSec: Integer read FcallAnswerTimeSec;
    property callEndTime: string read FcallEndTime;
    property callFlowId: string read FcallFlowId;
    property callId: string read FcallId;
    property contents: string read Fcontents;
    property date: string read Fdate;
    property displayed: Boolean read Fdisplayed;
    property durationSec: Integer read FdurationSec;
    property flagged: Boolean read Fflagged;
    property incoming: Boolean read Fincoming;
    property isConferenceCall: Boolean read FisConferenceCall;
    property onHoldDuration: Integer read FonHoldDuration;
    property participants: TAddressees read Fparticipants;
    property phone: string read Fphone;
    property phoneLine: TPhoneLine read FphoneLine;
    property retrieved: Boolean read Fretrieved;
    property status: string read Fstatus;
    property subject: string read Fsubject;
    property trunk: string read Ftrunk;
    property voicemail: Boolean read Fvoicemail;
    property voicemailIsHeard: Boolean read FvoicemailIsHeard;
  end;

implementation

{ TAddressee }

destructor TAddressee.Destroy;
begin
  Faddressable.Free;
  inherited;
end;

{ TPerson }

destructor TPerson.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FtelecomAddresses) - 1 do
    FtelecomAddresses[I].Free;
  for I := 0 to Length(Faddresses) - 1 do
    Faddresses[I].Free;
  inherited;
end;

{ TOrganisation }

destructor TOrganisation.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FtelecomAddresses) - 1 do
    FtelecomAddresses[I].Free;
  for I := 0 to Length(Faddresses) - 1 do
    Faddresses[I].Free;
  inherited;
end;

{ TCommunication }

destructor TCommunication.Destroy;
var
  I: Integer;
begin
  FphoneLine.Free;
  for I := 0 to Length(Fparticipants) - 1 do
    Fparticipants[I].Free;
  inherited;
end;

end.
