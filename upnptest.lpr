program upnptest;

{$mode objfpc}{$H+}

uses
  SynaUtil,
  upnplib,

  SysUtils;

var
  UPnP: TUPnP;
  S: string;
  i: integer;

procedure RefreshAndPrintPortMappings;
begin

  // Refresh the portmappings array
  if not UPnP.RefreshPortMapping then
    writeln('Error RefreshPortMapping' + LineEnding + UPNP.GetLastResultCode.ToString + LineEnding + UPnP.GetLastResponse);

  // Print out the Port mappings
  if Length(UPnP.PortMappings) > 0 then
  begin
    writeln('Portmappings:');
    for i := 0 to Length(UPnP.PortMappings) - 1 do
    begin
      with UPnP.PortMappings[i] do
        S := Format('%d %s %d->%s:%d %s %d s', [i, Protocol, ExternalPort, InternalClient, InternalPort, Description, LeaseDuration]);
      writeln(S);
    end;
  end
  else
    writeln('No portmappings');

end;

const
  Gateway = '192.168.2.1';

begin
  UPnP := TUPnP.Create(Gateway);

  if UPnP.IsUPnPAvailable then
  begin
    Writeln('UPnP is available.');

    Writeln('External IP: ', UPnP.ExternalIP);
    Writeln('Internal IP: ', UPnP.InternalIP);

    // Set a port mapping
    if UPnP.SetPortMapping(UPnP.InternalIP, 8081, 8000) then // use own computer IP
    begin
      writeln('SetPortMapping OK');

      // Refresh the portmappings array
      RefreshAndPrintPortMappings;

      writeln;
      writeln('Check port. Press Enter to delete and terminate');
      writeln;
      readln;
    end
    else
    begin
      writeln;
      writeln('Error SetPortMapping' + LineEnding + UPNP.GetLastResultCode.ToString + LineEnding + UPnP.GetLastResponse);
      writeln;
    end;

    // Delete a port mapping
    if UPnP.DeletePortMapping(8000) then
    begin
      writeln('DeletePortMapping OK');
    end
    else
    begin
      writeln;
      writeln('Error DeletePortMapping' + LineEnding + UPNP.GetLastResultCode.ToString + LineEnding + UPnP.GetLastResponse);
      writeln;
    end;

    writeln;
    // Refresh the portmappings array
    RefreshAndPrintPortMappings;

  end
  else
    Writeln('UPnP is not available.');

  UPnP.Free;

  writeln;
  writeln('We are done, press enter');
  readln;

end.
