# upnp
UPnP implementation for FPC

Sample

    UPnP := TUPnP.Create('0.0.0.0');

    if UPnP.IsUPnPAvailable then
    begin
      Writeln('UPnP device is available at ' + UPnP.ControlURL);


    // Set a port mapping
    if UPnP.SetPortMapping(UPnP.InternalIP, 8081, 8000, 'TCP') then // use own computer IP

    // Delete a port mapping
    if UPnP.DeletePortMapping(8000) then


Example output

    UPnP is available.

    Internal IP: 192.168.2.11
    External IP: x.x.x.x
    Status: Connected
    Last error: ERROR_NONE
    Uptime router: 23 days, 4 hours 41 minutes and 24 seoconds

    SetPortMapping OK
    Portmappings:
    0 UDP 3332->192.168.2.21:3332 libminiupnpc 0 s
    1 UDP 3344->192.168.2.21:4444 test 0 s
    2 TCP 8000->192.168.2.11:8081  0 s

    Check port. Press Enter to delete and terminate


    DeletePortMapping OK

    Portmappings:
    0 UDP 3332->192.168.2.21:3332 libminiupnpc 0 s
    1 UDP 3344->192.168.2.21:4444 test 0 s

    We are done, press enter
