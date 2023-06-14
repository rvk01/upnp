unit upnplib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynaUtil, HTTPSend, blcksock;

type
  TPortMapping = record
    RemoteHost: string;
    ExternalPort: integer;
    Protocol: string;
    InternalPort: integer;
    InternalClient: string;
    Enabled: boolean;
    Description: string;
    LeaseDuration: integer;
  end;

  TPortMappings = array of TPortMapping;

type
  TUPnP = class
  private
    FRootXML: string;
    FBaseURL: string;
    FControlURL: string;
    FServiceType: string;
    FResponse: string;
    FResultCode: integer;
    FPortMappings: TPortMappings;
    procedure Discover;
    function GetInternalIP: string;
    function GetExternalIP: string;
    procedure ExecuteSoapAction(const Action, SoapRequest: string);
  public
    constructor Create(const RouterIP: string);
    function IsUPnPAvailable: boolean;
    function GetStatusInfo(out ConnectionStatus: String; out LastError: String; out Uptime: Integer): Boolean;
    function GetLastResultCode: integer;
    function GetLastResponse: string;
    function SetPortMapping(const InternalIP: string; InternalPort, ExternalPort: integer; Protocol: string = 'TCP'; Description: string = ''): boolean;
    function DeletePortMapping(const ExternalPort: integer; Protocol: string = 'TCP'): boolean;
    function GetSpecificPortMapping(const ExternalPort: integer; Protocol: string = 'TCP'): TPortMapping;
    function RefreshPortMapping: boolean;
    property InternalIP: string read GetInternalIP;
    property ExternalIP: string read GetExternalIP;
    property PortMappings: TPortMappings read FPortMappings write FPortMappings;
  end;

const
  crlf = #13#10;

implementation

function GetStringBetweenAndStrip(var str: string; startStr, endStr: string): string;
var
  startPos, endPos: integer;
begin
  startStr := uppercase(startStr);
  endStr := uppercase(endStr);
  startPos := Pos(startStr, uppercase(str));
  if startPos = 0 then exit('');
  startPos := startPos + Length(startStr);
  endPos := Pos(endStr, uppercase(str), startPos);
  if endPos = 0 then exit('');
  Result := Copy(str, startPos, endPos - startPos);
  // strip the string
  startPos := startPos - Length(startStr);
  endPos := endPos + Length(endStr);
  System.Delete(str, startPos, endPos - startPos);
end;

procedure TUPnP.Discover;
begin
  // yet to be implemented of RouterIP = '' or 0.0.0.0
end;

constructor TUPnP.Create(const RouterIP: string);
var
  Socket: TUDPBlockSocket;
  S, Location, Service: string;
  Response: TStringList;
  Cnt: integer;
begin
  FRootXML := '';

  Cnt := 0;

  S := 'M-SEARCH * HTTP/1.1' + crlf +
    'HOST: 239.255.255.250:1900' + crlf +
    'MAN: "ssdp:discover"' + crlf +
    'MX: 3' + crlf +
    'ST: upnp:rootdevice' + crlf + crlf;

  Socket := TUDPBlockSocket.Create;
  Socket.EnableBroadcast(True);
  Socket.Connect(RouterIP, '1900');
  Socket.SendString(S);
  repeat
    Inc(Cnt);
    if Socket.CanRead(3000) then
    begin
      S := Socket.RecvPacket(3000);
      if Pos('LOCATION: ', uppercase(S)) > 0 then
      begin
        Location := Copy(S, Pos('LOCATION:', uppercase(S)) + 9);
        Location := Trim(Copy(Location, 1, Pos(crlf, Location) - 1));
        Response := TStringList.Create;
        try

          HttpGetText(Location, Response);
          FRootXML := Response.Text;

          FBaseURL := Location; // take base of Location for control
          while (FBaseURL <> '') and (Location[Length(FBaseURL)] <> '/') do Delete(FBaseURL, Length(FBaseURL), 1);
          if FBaseURL <> '' then Delete(FBaseURL, Length(FBaseURL), 1);

          // loop all services
          repeat
            Service := GetStringBetweenAndStrip(FRootXML, '<service>', '</service>');
            if Pos(uppercase(':WANIPConnection:'), uppercase(service)) > 0 then
            begin
              S := GetStringBetweenAndStrip(Service, '<SCPDURL>', '</SCPDURL>');
              if S <> '' then
              begin
                Location := FBaseURL + S;
                HttpGetText(Location, Response);
                S := Response.Text;
                if Pos(uppercase('<name>AddPortMapping</name>'), uppercase(S)) > 0 then
                begin
                  FServiceType := GetStringBetweenAndStrip(Service, '<serviceType>', '</serviceType>');
                  S := GetStringBetweenAndStrip(Service, '<controlURL>', '</controlURL>');
                  FControlURL := FBaseURL + S;
                end;
              end;
            end;
          until service = '';

          break;

        finally
          Response.Free;
        end;

      end;

    end;

    sleep(100);

  until (Cnt > 1000);

  Socket.CloseSocket;
  Socket.Free;

end;

function TUPnP.IsUPnPAvailable: boolean;
begin
  Result := FControlURL <> '';
end;

function TUPnP.GetLastResultCode: integer;
begin
  Result := FResultCode;
end;

function TUPnP.GetLastResponse: string;
begin
  Result := FResponse;
end;

function TUPnP.GetInternalIP: string;

  function LocalIPs: string;
  var
    TcpSock: TTCPBlockSocket;
    ipList: TStringList;
  begin
    Result := '';
    ipList := TStringList.Create;
    try
      TcpSock := TTCPBlockSocket.Create;
      try
        TcpSock.Family := SF_IP4;
        TcpSock.ResolveNameToIP(TcpSock.LocalName, ipList);
        Result := ipList.CommaText;
      finally
        TcpSock.Free;
      end;
    finally
      ipList.Free;
    end;
  end;

var
  ipList: TStringList;
begin
  Result := cAnyHost;
  ipList := TStringList.Create;
  try
    ipList.CommaText := LocalIPs;
    if ipList.Count > 0 then
      Result := ipList.Strings[0];
  finally
    ipList.Free;
  end;
end;


function TUPnP.GetExternalIP: string;
var
  SoapRequest: string;
begin
  SoapRequest :=
    '<?xml version="1.0" encoding="utf-8"?>' +
    '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
    '<s:Body>' +
    '<u:GetExternalIPAddress xmlns:u="' + FServiceType + '">' +
    '</u:GetExternalIPAddress>' +
    '</s:Body>' +
    '</s:Envelope>';

  ExecuteSoapAction('GetExternalIPAddress', SoapRequest);

  Result := GetStringBetweenAndStrip(FResponse, '<NewExternalIPAddress>', '</NewExternalIPAddress>');

end;

function TUPnP.GetStatusInfo(out ConnectionStatus: String; out LastError: String; out Uptime: Integer): Boolean;
var
  SoapRequest: string;
begin
  SoapRequest :=
    '<?xml version="1.0" encoding="utf-8"?>' +
    '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
    '<s:Body>' +
    '<u:GetStatusInfo xmlns:u="' + FServiceType + '">' +
    '</u:GetStatusInfo>' +
    '</s:Body>' +
    '</s:Envelope>';

  ExecuteSoapAction('GetStatusInfo', SoapRequest);

  ConnectionStatus :=  GetStringBetweenAndStrip(FResponse, '<NewConnectionStatus>', '</NewConnectionStatus>');
  LastError :=  GetStringBetweenAndStrip(FResponse, '<NewLastConnectionError>', '</NewLastConnectionError>');
  Uptime :=  StrToIntDef(GetStringBetweenAndStrip(FResponse, '<NewUptime>', '</NewUptime>'), 0);

  Result := FResultCode = 200;

end;

function TUPnP.SetPortMapping(const InternalIP: string; InternalPort, ExternalPort: integer; Protocol: string = 'TCP'; Description: string = ''): boolean;
var
  SoapRequest: string;
begin
  SoapRequest :=
    '<?xml version="1.0" encoding="utf-8"?>' +
    '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
    '<s:Body>' +
    '<u:AddPortMapping xmlns:u="' + FServiceType + '">' +
    '<NewRemoteHost></NewRemoteHost>' +
    '<NewExternalPort>' + IntToStr(ExternalPort) + '</NewExternalPort>' +
    '<NewProtocol>' + Protocol + '</NewProtocol>' +
    '<NewInternalPort>' + IntToStr(InternalPort) + '</NewInternalPort>' +
    '<NewInternalClient>' + InternalIP + '</NewInternalClient>' +
    '<NewEnabled>1</NewEnabled>' +
    '<NewPortMappingDescription>' + Description + '</NewPortMappingDescription>' +
    '<NewLeaseDuration>0</NewLeaseDuration>' +
    '</u:AddPortMapping>' +
    '</s:Body>' +
    '</s:Envelope>';

  ExecuteSoapAction('AddPortMapping', SoapRequest);
  Result := FResultCode = 200;

end;

function TUPnP.DeletePortMapping(const ExternalPort: integer; Protocol: string = 'TCP'): boolean;
var
  SoapRequest: string;
begin
  SoapRequest :=
    '<?xml version="1.0" encoding="utf-8"?>' +
    '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
    '<s:Body>' +
    '<u:DeletePortMapping xmlns:u="' + FServiceType + '">' +
    '<NewRemoteHost></NewRemoteHost>' +
    '<NewExternalPort>' + IntToStr(ExternalPort) + '</NewExternalPort>' +
    '<NewProtocol>' + Protocol + '</NewProtocol>' +
    '</u:DeletePortMapping>' +
    '</s:Body>' +
    '</s:Envelope>';

  ExecuteSoapAction('DeletePortMapping', SoapRequest);
  Result := FResultCode = 200;

end;

function FillPortMapping(Found1: String): TPortMapping;
begin
  Result.RemoteHost := GetStringBetweenAndStrip(Found1, '<NewRemoteHost>', '</NewRemoteHost>');
  Result.ExternalPort := StrToIntDef(GetStringBetweenAndStrip(Found1, '<NewExternalPort>', '</NewExternalPort>'), 0);
  Result.Protocol := GetStringBetweenAndStrip(Found1, '<NewProtocol>', '</NewProtocol>');
  Result.InternalPort := StrToIntDef(GetStringBetweenAndStrip(Found1, '<NewInternalPort>', '</NewInternalPort>'), 0);
  Result.InternalClient := GetStringBetweenAndStrip(Found1, '<NewInternalClient>', '</NewInternalClient>');
  Result.Enabled := GetStringBetweenAndStrip(Found1, '<NewEnabled>', '<NewEnabled>') = '1';
  Result.Description := GetStringBetweenAndStrip(Found1, '<NewPortMappingDescription>', '</NewPortMappingDescription>');
  Result.LeaseDuration := StrToIntDef(GetStringBetweenAndStrip(Found1, '<NewLeaseDuration>', '</NewLeaseDuration>'), 0);
end;

function TUPnP.GetSpecificPortMapping(const ExternalPort: integer; Protocol: string = 'TCP'): TPortMapping;
var
  SoapRequest: string;
  Found1: string;
begin
  FillChar(Result, SizeOf(Result), 0);

  SoapRequest :=
    '<?xml version="1.0" encoding="utf-8"?>' +
    '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
    '<s:Body>' +
    '<u:GetSpecificPortMappingEntry xmlns:u="' + FServiceType + '">' +
    '<NewRemoteHost></NewRemoteHost>' +
    '<NewExternalPort>' + IntToStr(ExternalPort) + '</NewExternalPort>' +
    '<NewProtocol>' + Protocol + '</NewProtocol>' +
    '</u:GetSpecificPortMappingEntry>' +
    '</s:Body>' +
    '</s:Envelope>';

  ExecuteSoapAction('GetSpecificPortMappingEntry', SoapRequest);

  if FResultCode = 200 then
  begin
    Found1 := GetStringBetweenAndStrip(FResponse, '<u:GetSpecificPortMappingEntryResponse', '</u:GetSpecificPortMappingEntryResponse>');
    if Found1 <> '' then
    begin
      Result := FillPortMapping(Found1);
    end;
  end;

end;

function TUPnP.RefreshPortMapping: boolean;
var
  SoapRequest: string;
  Found1: string;
  Idx: integer;
  Map: TPortMapping;
begin

  SetLength(FPortMappings, 0);
  Idx := 0;

  repeat

    SoapRequest :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
      '<s:Body>' +
      '<u:GetGenericPortMappingEntry xmlns:u="' + FServiceType + '">' +
      '<NewPortMappingIndex>' + Idx.ToString + '</NewPortMappingIndex>' +
      '</u:GetGenericPortMappingEntry>' +
      '</s:Body>' +
      '</s:Envelope>';

    ExecuteSoapAction('GetGenericPortMappingEntry', SoapRequest);

    if FResultCode = 200 then
    begin
      Found1 := GetStringBetweenAndStrip(FResponse, '<u:GetGenericPortMappingEntryResponse', '</u:GetGenericPortMappingEntryResponse>');
      if Found1 <> '' then
      begin
        Map := FillPortMapping(Found1);
        SetLength(FPortMappings, Length(FPortMappings) + 1);
        FPortMappings[Length(FPortMappings) - 1] := Map;
      end;
    end;

    Inc(Idx);

  until FResultcode <> 200;

  Result := FResultcode = 500; // last one should be 500 (?)

end;

procedure TUPnP.ExecuteSoapAction(const Action, SoapRequest: string);
var
  HTTP: THTTPSend;
  Response: TStringList;
begin
  FResultCode := 0;
  HTTP := THTTPSend.Create;
  Response := TStringList.Create;
  try
    HTTP.Headers.Clear;
    HTTP.Document.Position := 0;
    WriteStrToStream(HTTP.Document, SoapRequest);
    HTTP.MimeType := 'text/xml; charset="utf-8"';
    HTTP.Headers.Add('SOAPAction: "' + FServiceType + '#' + Action + '"');
    if HTTP.HTTPMethod('POST', FControlURL) then
    begin
      Response.LoadFromStream(HTTP.Document);
      FResponse := Response.Text;
      FResultCode := HTTP.ResultCode;
    end;
  finally
    HTTP.Free;
    Response.Free;
  end;
end;

end.
