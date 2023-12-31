program upnptest;

{$mode objfpc}{$H+}

uses
  upnplib,
  SysUtils;

var
  UPnP: TUPnP;

  procedure RefreshAndPrintPortMappings;
  var
    S: String;
    i: integer;
  begin

    // Refresh the portmappings array
    if not UPnP.RefreshPortMapping then
      Writeln('Error RefreshPortMapping' + LineEnding +
        'ResultCode: ' + UPNP.LastResultCode.ToString + LineEnding + UPnP.LastResponse);

    // Print out the Port mappings
    if Length(UPnP.PortMappings) > 0 then
    begin
      Writeln('Portmappings:');
      for i := 0 to Length(UPnP.PortMappings) - 1 do
      begin
        with UPnP.PortMappings[i] do
          S := Format('%d %s %d->%s:%d %s %d s', [i, Protocol, ExternalPort, InternalClient, InternalPort, Description, LeaseDuration]);
        Writeln(S);
      end;
    end
    else
      Writeln('No portmappings');

  end;

procedure DebugProc(Value: String);
begin
  Writeln('  debug --> ' + Value);
end;

var
  Status: string;
  LastError: string;
  Uptime: integer;
begin
  UPnP := TUPnP.Create('0.0.0.0', @DebugProc);
  try

    if UPnP.IsUPnPAvailable then
    begin
      Writeln('UPnP device is available at ' + UPnP.ControlURL);

      UPnP.GetStatusInfo(Status, LastError, Uptime);

      Writeln;
      Writeln('Internal IP: ', UPnP.InternalIP);
      Writeln('External IP: ', UPnP.ExternalIP);
      Writeln('Status: ', Status);
      Writeln('Last error: ', LastError);
      Writeln('Uptime router: ', format('%d days %d hours %d minutes and %d seconds',
        [Uptime div 86400, (Uptime div 3600) mod 24, (Uptime div 60) mod 60, Uptime mod 60]));
      Writeln;

      // Set a port mapping
      if UPnP.SetPortMapping(UPnP.InternalIP, 8081, 8000, 'TCP') then // use own computer IP
      begin
        if UPnP.GetSpecificPortMapping(8000, 'TCP').InternalClient = UPnP.InternalIP then
          Writeln('SetPortMapping OK')
        else
          writeln('SetPortMapping reports OK but Port does not seem to be forwarded');

        // Refresh the portmappings array
        RefreshAndPrintPortMappings;

        Writeln;
        Writeln('Check port. Press Enter to delete and terminate');
        Writeln;
        Readln;
      end
      else
      begin
        Writeln;
        Writeln('Error SetPortMapping' + LineEnding +
          'ResultCode: ' + UPNP.LastResultCode.ToString + LineEnding + UPnP.LastResponse);
        Writeln;
      end;

      // Delete a port mapping
      if UPnP.DeletePortMapping(8000) then
      begin
        Writeln('DeletePortMapping OK');
      end
      else
      begin
        Writeln;
        Writeln('Error DeletePortMapping' + LineEnding +
          'ResultCode: ' + UPNP.LastResultCode.ToString + LineEnding + UPnP.LastResponse);
        Writeln;
      end;

      writeln;
      // Refresh the portmappings array
      RefreshAndPrintPortMappings;

    end
    else
      Writeln('UPnP is not available.');

  finally
    UPnP.Free;
  end;

  Writeln;
  Writeln('We are done, press enter');
  Readln;

end.
