#!/bin/env python3
import http.server
import socketserver
import sys
import threading
import json

PORT = int(sys.argv[1])

class RequestHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self._handle_request()
        
    def do_POST(self):
        self._handle_request()
        
    def _handle_request(self):
        # open stdout for writing in binary mode
        with open(sys.stdout.fileno(), 'wb', buffering=0) as f:
            json_object = {
                "method": self.command,
                "path": self.path,
                "headers": {header: self.headers[header] for header in sorted(self.headers)},
            }
            # Log body if present
            if 'Content-Length' in self.headers:
                content_length = int(self.headers['Content-Length'])
                body = self.rfile.read(content_length)
                # decode body into dictionary
                try:
                    body = json.loads(body)
                    json_object['body'] = body
                except json.JSONDecodeError:
                    json_object['body'] = body.decode('utf-8')
            json_string = json.dumps(json_object, indent=2)
            _  = f.write(json_string.encode())
        
        # Send response
        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.end_headers()
        
        # Exit after handling one request
        threading.Timer(0.1, self.server.shutdown).start()

with socketserver.TCPServer(("", PORT), RequestHandler) as httpd:
    httpd.serve_forever()
