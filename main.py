from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app

# Red tendencia
from myapp.controllers.pages             import *

application = webapp.WSGIApplication([("/", Entrance),
				    ("/addcategory", AddCategory),
				    ("/proc_add_category", ProcAddCategory),
				    ("/listcategory", ListCategories)
				    ], debug=False)


def main():
  run_wsgi_app(application)

if __name__ == "__main__":
  main()
