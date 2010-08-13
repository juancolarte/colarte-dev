import cgi
import datetime
import wsgiref.handlers
from lython import gocode

from google.appengine.ext import webapp


class MainPage(webapp.RequestHandler):
  def get(self):
    
    self.response.out.write(gocode('(print "<HTML><BODY>hello</BODY></HTML>")'))

class Guestbook(webapp.RequestHandler):
  def post(self):
    content = self.request.get('content')
    self.response.write(content)


application = webapp.WSGIApplication([
  ('/', MainPage),
  ('/sign', Guestbook)
], debug=True)


def main():
  wsgiref.handlers.CGIHandler().run(application)


if __name__ == '__main__':
  main()

print 'Content-Type: text/plain'
print ''
print 'Hello, world!'
