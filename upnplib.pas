unit upnplib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, sockets, {$IFDEF WINDOWS} Winsock {$ELSE} unixtype, netdb {$ENDIF};

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
  DebugProcType = procedure(Value: String);

type
  TUPnP = class
  private
    FBaseURL: string;
    FControlURL: string;
    FServiceType: string;
    FResponse: string;
    FResultCode: integer;
    FPortMappings: TPortMappings;
    function GetInternalIP: string;
    function GetExternalIP: string;
    procedure ExecuteSoapAction(const Action, SoapRequest: string);
  public
    constructor Create(RouterIP: string = ''; DebugProc: DebugProcType = nil);
    function IsUPnPAvailable: boolean;
    function GetStatusInfo(out ConnectionStatus: string; out LastError: string; out Uptime: integer): boolean;
    function SetPortMapping(const InternalIP: string; InternalPort, ExternalPort: integer; Protocol: string = 'TCP'; Description: string = ''): boolean;
    function DeletePortMapping(const ExternalPort: integer; Protocol: string = 'TCP'): boolean;
    function GetSpecificPortMapping(const ExternalPort: integer; Protocol: string = 'TCP'): TPortMapping;
    function RefreshPortMapping: boolean;

    property ControlURL: string read FControlURL;
    property InternalIP: string read GetInternalIP;
    property ExternalIP: string read GetExternalIP;
    property LastResultCode: integer read FResultCode;
    property LastResponse: string read FResponse;
    property PortMappings: TPortMappings read FPortMappings write FPortMappings;

  end;

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

const
  CRLF = #13#10;

constructor TUPnP.Create(RouterIP: string = ''; DebugProc: DebugProcType = nil); // '' = discover
var
  Socket: TSocket;
  DestAddr: TInetSockAddr;
  // ListenAddr: TInetSockAddr;
  SenderAddrLen: tsocklen;
  SenderAddr: TInetSockAddr;
  MessageLen: SizeInt;
  S, Location, Service: string;
  Response: TStringList;
  Cnt: integer;
  BroadcastEnable: longint;
{$ifdef windows}
  Timeout: DWord;
{$else}
  Timeout: ttimeval;
{$endif}
begin

  Cnt := 0; // timeout counter, max 2 x 3 seconds
  if (RouterIP = '0.0.0.0') or (RouterIP = '') then
    RouterIP := '255.255.255.255'; // discover

  S := 'M-SEARCH * HTTP/1.1' + CRLF +
    'HOST: 239.255.255.250:1900' + CRLF +
    'MAN: "ssdp:discover"' + CRLF +
    'MX: 3' + CRLF +
    'ST: upnp:rootdevice' + CRLF + CRLF;

  Response := TStringList.Create;
  Socket := fpSocket(AF_INET, SOCK_DGRAM, 0);

  try

    BroadcastEnable := 1;
    fpSetSockOpt(Socket, SOL_SOCKET, SO_BROADCAST, @BroadcastEnable, SizeOf(BroadcastEnable));

    {$IFDEF WINDOWS}
    Timeout := 3000;
    fpsetsockopt(Socket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, sizeof(Timeout));
    {$ELSE}
    Timeout.tv_sec:=3;
    Timeout.tv_usec:=0;
    fpsetsockopt(Socket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, sizeof(Timeout));
    {$ENDIF}

    {
    ListenAddr.sin_family := AF_INET;
    ListenAddr.sin_port := htons(1900);
    ListenAddr.sin_addr := StrToNetAddr('0.0.0.0');
    fpbind(Socket, @ListenAddr, SizeOf(ListenAddr));
    }
    DestAddr.sin_family := AF_INET;
    DestAddr.sin_port := htons(1900);
    DestAddr.sin_addr := StrToNetAddr('255.255.255.255');
    fpSendTo(Socket, @S[1], Length(S), 0, @DestAddr, SizeOf(DestAddr));

    repeat
      Inc(Cnt);

      SetLength(S, 2048); // Max UDP Datagram length
      SenderAddrLen := SizeOf(SenderAddr);
      MessageLen := fprecvfrom(Socket, @S[1], Length(S), 0, @SenderAddr, @SenderAddrLen);
      SetLength(S, MessageLen);

      DebugProc('M-SEARCH found the following: ' + #13#10 + S);

      if Pos('LOCATION: ', uppercase(S)) > 0 then
      begin

        // Found one. Reset timeout counter, if this is not the one then wait
        Cnt := 0;

        Location := Copy(S, Pos('LOCATION:', uppercase(S)) + 9);
        Location := Trim(Copy(Location, 1, Pos(CRLF, Location) - 1));

        S := TFPHttpClient.SimpleGet(Location);

        FBaseURL := Location; // take base of Location for control
        while (FBaseURL <> '') and (Location[Length(FBaseURL)] <> '/') do Delete(FBaseURL, Length(FBaseURL), 1);
        if FBaseURL <> '' then Delete(FBaseURL, Length(FBaseURL), 1);

        // loop all services
        repeat
          Service := GetStringBetweenAndStrip(S, '<service>', '</service>');
          if Pos(uppercase(':WANIPConnection:'), uppercase(service)) > 0 then
          begin
            // We found a WAN device
            S := GetStringBetweenAndStrip(Service, '<SCPDURL>', '</SCPDURL>');
            if S <> '' then
            begin
              Location := FBaseURL + S;
              S := TFPHttpClient.SimpleGet(Location);
              if Pos(uppercase('<name>AddPortMapping</name>'), uppercase(S)) > 0 then
              begin
                FServiceType := GetStringBetweenAndStrip(Service, '<serviceType>', '</serviceType>');
                S := GetStringBetweenAndStrip(Service, '<controlURL>', '</controlURL>');
                FControlURL := FBaseURL + S;

                Cnt := 99;
                break; // only break on correct service

              end;
            end;
          end;

        until service = '';

      end;

    until (Cnt > 1);

  finally
    // Socket.CloseSocket;
    // Socket.Free;
    Response.Free;
  end;

end;

function TUPnP.IsUPnPAvailable: boolean;
begin
  Result := FControlURL <> '';
end;


{$IFDEF WINDOWS}

function TUPnP.GetInternalIP: string;
var
  HostName: AnsiString;
  HostEntry: PHostEnt;
begin
  Result := '';
  HostName := '';
  SetLength(HostName, 255);
  if GetHostName(PAnsiChar(HostName), Length(HostName)) = 0 then
  begin
    SetLength(HostName, StrLen(PAnsiChar(HostName)));
    HostEntry := GetHostByName(PAnsiChar(HostName));
    if Assigned(HostEntry) then
      Result := StrPas(inet_ntoa(PInAddr(HostEntry^.h_addr_list^)^));
  end;
end;

{$ELSE}

procedure GetIPAddr(var buf: array of char; const len: longint);
const
  CN_GDNS_ADDR = '127.0.0.1';
  CN_GDNS_PORT = 53;
var
  s: string;
  sock: longint;
  HostAddr: TSockAddr;
  l: integer;
  IPAddr: TInetSockAddr;
begin
  Assert(len >= 16);
  sock := fpsocket(AF_INET, SOCK_DGRAM, 0);
  assert(sock <> -1);
  IPAddr.sin_family := AF_INET;
  IPAddr.sin_port := htons(CN_GDNS_PORT);
  IPAddr.sin_addr.s_addr := StrToHostAddr(CN_GDNS_ADDR).s_addr;
  if (fpConnect(sock, @IPAddr, SizeOf(IPAddr)) <> 0) then exit;
  try
    l := SizeOf(HostAddr);
    if (fpgetsockname(sock, @HostAddr, @l) = 0) then
    begin
      s := NetAddrToStr(HostAddr.sin_addr);
      StrPCopy(PChar(Buf), s);
    end;
  finally
    if (CloseSocket(sock) <> 0) then;
  end;
end;

function TUPnP.GetInternalIP: string;
var
  IPStr: array[0..30] of char;
begin
  GetIPAddr(IPStr, SizeOf(IPStr));
  Result := IPStr;
end;

{$ENDIF}


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

function TUPnP.GetStatusInfo(out ConnectionStatus: string; out LastError: string; out Uptime: integer): boolean;
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

  ConnectionStatus := GetStringBetweenAndStrip(FResponse, '<NewConnectionStatus>', '</NewConnectionStatus>');
  LastError := GetStringBetweenAndStrip(FResponse, '<NewLastConnectionError>', '</NewLastConnectionError>');
  Uptime := StrToIntDef(GetStringBetweenAndStrip(FResponse, '<NewUptime>', '</NewUptime>'), 0);

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

function FillPortMapping(Found1: string): TPortMapping;
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
  // FillChar(Result, SizeOf(Result), 0);

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
  HTTP: TFPHttpClient;
  Data: TRawByteStringStream;
begin
  FResultCode := 0;
  FResponse := '';
  HTTP := TFPHttpClient.Create(nil);
  Data := TRawByteStringStream.Create('');
  try
    HTTP.RequestBody := TRawByteStringStream.Create(SoapRequest);
    HTTP.AddHeader('Content-Type', 'text/xml; charset="utf-8"');
    HTTP.AddHeader('SOAPAction', '"' + FServiceType + '#' + Action + '"');
    HTTP.HTTPMethod('POST', FControlURL, Data, []);
    FResultCode := HTTP.ResponseStatusCode;
    FResponse := Data.Datastring;
  finally
    HTTP.RequestBody.Free;
    HTTP.RequestBody := nil;
    HTTP.Free;
    Data.Free;
  end;
end;

end.
