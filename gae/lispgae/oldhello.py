import cgi
import datetime
import wsgiref.handlers

from google.appengine.ext import webapp


class MainPage(webapp.RequestHandler):
  def get(self):
    self.response.out.write('<html><body>')
    self.response.out.write("""
          <form action="/sign" method="post">
            <div><textarea name="content" rows="3" cols="60"></textarea></div>
            <div><input type="submit" value="Sign Guestbook"></div>
          </form>
        </body>
      </html>""")


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
