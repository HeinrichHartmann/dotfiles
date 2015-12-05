#!/usr/bin/python

import sys
import BaseHTTPServer
from SimpleHTTPServer import SimpleHTTPRequestHandler

class MyHandler(BaseHTTPServer.BaseHTTPRequestHandler):
     def do_HEAD(s):
         s.send_response(200)
         s.send_header("Content-type", "text/html")
         s.end_headers()
     def do_GET(s):
         """Respond to a GET request."""

         CSS = """
<style>
body {
    background-color: #FFFFFF;
}

h1 {
    color: black;
    text-align: center;
}

h3 {
    text-align: center;
}

.time {
    align:center;
    font-family: "Times New Roman";
    font-size: 20px;
}
</style>
"""


         s.send_response(200)
         s.send_header("Content-type", "text/html")
         s.end_headers()
         s.wfile.write("<html><head><title>World Cup Time:</title>" + CSS + "</head>")
         s.wfile.write("<body><h1>World Cup Time</h1>")
         s.wfile.write("<br> <h3>" + getTime() + " WCT<h3>" )
         s.wfile.write("<br> ... time passed since the world cup final.")
         s.wfile.write("</body></html>")

def getTime():
    from datetime import datetime

    GAME_END = datetime(2014,07,13,00,00,00)

    WORLD_START = datetime(1000,1,1,0,0)

    nnow = WORLD_START + (datetime.now() - GAME_END)

    return "{:04d}-{:02d}-{:02d} {:02d}:{:02d}:{:02d}".format(nnow.year - 1000, nnow.month, nnow.day, nnow.hour, nnow.minute, nnow.second)


def main():
    HandlerClass = SimpleHTTPRequestHandler
    ServerClass  = BaseHTTPServer.HTTPServer
    Protocol     = "HTTP/1.0"

    if sys.argv[1:]:
        port = int(sys.argv[1])
    else:
        port = 8000

    server_address = ('127.0.0.1', port)

    HandlerClass.protocol_version = Protocol
    httpd = ServerClass(server_address, MyHandler)

    sa = httpd.socket.getsockname()
    print "Serving HTTP on", sa[0], "port", sa[1], "..."
    httpd.serve_forever()


if __name__ == "__main__":
    main()
