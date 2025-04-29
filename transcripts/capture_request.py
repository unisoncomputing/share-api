#!/bin/env python3
import http.server
import socketserver
import sys
import threading

PORT = int(sys.argv[1])
log_file = sys.argv[2]

class RequestHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self._handle_request()
        
    def do_POST(self):
        self._handle_request()
        
    def _handle_request(self):
        # Log headers
        with open(log_file, 'wb') as f:
            f.write(f"Request method: {self.command}\n".encode())
            f.write(f"Request path: {self.path}\n".encode())
            f.write(b"Headers:\n")
            for header in self.headers:
                f.write(f"  {header}: {self.headers[header]}\n".encode())
            
            # Log body if present
            if 'Content-Length' in self.headers:
                content_length = int(self.headers['Content-Length'])
                body = self.rfile.read(content_length)
                f.write(b"\nBody:\n")
                f.write(body)
        
        # Send response
        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.end_headers()
        
        # Exit after handling one request
        print(f"Request received and logged to {log_file}")
        threading.Timer(0.1, self.server.shutdown).start()
        # self.server.shutdown()

with socketserver.TCPServer(("", PORT), RequestHandler) as httpd:
    print(f"Webhook listener started on port {PORT}")
    print(f"Waiting for a single request (will be logged to {log_file})...")
    httpd.serve_forever()
