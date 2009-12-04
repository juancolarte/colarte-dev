from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app

# Red tendencia
from myapp.controllers.pages             import *
from myapp.controllers.fileupload 	 import *

application = webapp.WSGIApplication([("/", Entrance),
				    ("/addcategory", AddCategory),
				    ("/proc_add_category", ProcAddCategory),
				    ("/listcategory", ListCategories),
				    ("/upload_file",UploadFileForm),
				    ("/proc_file_upload",ProcUploadFile),
				    ("/listuploadedfiles",ListUploadedFiles)
				    ], debug=True)
def main():
  run_wsgi_app(application)

if __name__ == "__main__":
  main()
