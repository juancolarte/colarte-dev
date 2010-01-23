# coding: utf-8

import os, cgi
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from django.utils import simplejson as json

from myapp.models.urlstore import Url, Category

class AddUrl(webapp.RequestHandler):
    def get(self):
	categories = Category.all()
	select_options = ""
        for category in categories:
            select_options = select_options +  "<option value='%s'> %s</option>\n" % (cgi.escape(category.category), cgi.escape(category.category)) 
        params = {"page_title" : "New Url",
            	  "select_options" : select_options }
        path = os.path.join(os.path.dirname(__file__), '../views/form_url.html')
        self.response.out.write(template.render(path, params))        

class ProcAddUrl(webapp.RequestHandler):
    def post(self):
	url = Url()
	url.address  = self.request.get('address').encode('utf-8')
	url.title  = self.request.get('title').encode('utf-8')
	url.put()
        self.redirect("/addurl")

class ListUrl(webapp.RequestHandler):
    def get(self):
	urls = Url.all()
	answer_rows = ""
        for url in urls:
            answer_rows = answer_rows +  "<tr><td>%s</td><td>%s</td></tr> \n" % (cgi.escape(url.title), cgi.escape(url.address)) 
        params = {"page_title" : "Urls",
    		  "rows_result" : answer_rows }        
        path = os.path.join(os.path.dirname(__file__), '../views/listurl.html')
        self.response.out.write(template.render(path, params))        
