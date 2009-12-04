# coding: utf-8

import os, cgi
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

from google.appengine.ext import db
from myapp.models.blobs import Blobo

class UploadFileForm(webapp.RequestHandler):
    def get(self):
        params = {"page_title" : "Upload File"}        
        path = os.path.join(os.path.dirname(__file__), '../views/form_upload.html')
        self.response.out.write(template.render(path, params))        

class ProcUploadFile(webapp.RequestHandler):
    def post(self):
	blob_file = Blobo()
	blob_file.title = self.request.get('title').encode('utf-8')
	blob = self.request.get('upload_file').encode('utf-8')
	blob_file.blob = db.Blob(blob)
	blob_file.put()
        self.redirect("/listuploadedfiles")

    def get(self):
	blob_file = Blobo()
	blob_file.title = self.request.get('title').encode('utf-8')
	blob = self.request.get('upload_file').encode('utf-8')
	blob_file.blob = db.Blob(blob)
	blob_file.put()
        self.redirect("/listuploadedfiles")
        


class ListUploadedFiles(webapp.RequestHandler):
    def get(self):
	blobs = Blobo.all()
	answer_rows = ""
        for blob in blobs:
            answer_rows = answer_rows +  "<tr><td>%s</td></tr> \n" % cgi.escape(blob.title) 
        params = {"page_title" : "List Files",
    		  "rows_result" : answer_rows }        
        path = os.path.join(os.path.dirname(__file__), '../views/listuploadedfiles.html')
        self.response.out.write(template.render(path, params))        

#http://code.google.com/appengine/docs/python/images/usingimages.html