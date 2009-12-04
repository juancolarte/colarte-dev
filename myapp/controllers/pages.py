# coding: utf-8

import os, cgi
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from django.utils import simplejson as json

from myapp.models.urlstore import Category

class Entrance(webapp.RequestHandler):
    def get(self):
        params = {"page_title" : "Beginning"}        
        path = os.path.join(os.path.dirname(__file__), '../views/ppal.html')
        self.response.out.write(template.render(path, params))        

class AddCategory(webapp.RequestHandler):
    def get(self):
        params = {"page_title" : "New Category"}        
        path = os.path.join(os.path.dirname(__file__), '../views/add_category.html')
        self.response.out.write(template.render(path, params))        

class ProcAddCategory(webapp.RequestHandler):
    def post(self):
	category = Category()
	category.category  = self.request.get('category').encode('utf-8')
	category.description  = self.request.get('description').encode('utf-8')
	category.put()
        self.redirect("/addcategory")
	#valor_procesado = "Agregando : " + category.category + " desc :" + category.description
        #params = {"page_title" : "New Category",
    	#	  "mivalor" : valor_procesado }        
        #path = os.path.join(os.path.dirname(__file__), '../views/procesoform.html')
        #self.response.out.write(template.render(path, params))        

class ListCategories(webapp.RequestHandler):
    def get(self):
	categories = Category.all()
	answer_rows = ""
        for category in categories:
            answer_rows = answer_rows +  "<tr><td>%s</td><td>%s</td></tr> \n" % (cgi.escape(category.category), cgi.escape(category.description)) 
        params = {"page_title" : "New Category",
    		  "rows_result" : answer_rows }        
        path = os.path.join(os.path.dirname(__file__), '../views/listcategories.html')
        self.response.out.write(template.render(path, params))        
