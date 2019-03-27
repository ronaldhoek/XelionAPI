program TestXelionAPI;

uses
  System.StartUpCopy,
  Xelion.Types in '..\XelionLib\Xelion.Types.pas',
  Xelion.DataModel in '..\XelionLib\Xelion.DataModel.pas',
  Xelion.Classes.Request in '..\XelionLib\Xelion.Classes.Request.pas',
  Xelion.Classes.Response in '..\XelionLib\Xelion.Classes.Response.pas',
  Xelion.Requests in '..\XelionLib\Xelion.Requests.pas',
  Xelion.Connection in '..\XelionLib\Xelion.Connection.pas',
  Xelion.Session in '..\XelionLib\Xelion.Session.pas',
  XPlatform in 'XPlatform.pas',
  FMX.Forms,
  FormMainU in 'FormMainU.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
