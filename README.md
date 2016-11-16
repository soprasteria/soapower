# Soapower - Monitoring Webservices

Soapower provides a GUI for

- viewing webservices requests (live and search page),
- download the data from the request and response,
- getting response time,
- viewing 90percentiles response times, etc ...
- Soapower allows monitoring several applications across multiple environments.

It is also possible to set a threshold response time on soapaction, the alerts are rising if the receiver does not send the response back in time.

Administration interface is available for

- Configure environments and webservices (local / remote target / timeout)
- Set the thresholds for response time for each serviceaction
- Import / export service configurations & settings
- Monitoring CPU / Heap Memory / logs file

## Getting started

- [Install activator](http://www.lightbend.com/community/core-tools/activator-and-sbt)
- Run : `activator -jvm-debug 9999 run`

I you have to connect to a proxy, don't forget to add proxy options to activator command : `-Dhttp.proxyHost=XXX -Dhttp.proxyPort=YYYY -Dhttp.nonProxyHosts="localhost|127.0.0.01"` 

# Download & Release Notes

See https://github.com/soapower/soapower/releases

# Licence

This software is licensed under the GNU GENERAL PUBLIC LICENSE V3. See LICENSE file.
