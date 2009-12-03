import cgi

from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext import db

    
class Url(db.Model):
    title   = db.StringProperty()
    address = db.StringProperty()
    category = db.ListProperty(db.Key)

class Category(db.Model):
    category = db.StringProperty()
    description = db.StringProperty()
    @property
    def members(self):
        return Url.gql("WHERE category = :1", self.key())    


def populate():
  Category(category='development', description='Programming Stuff').put()
  Category(category='OS', description='Operating System Stuff').put()
  Category(category='Internet', description='Online Stuff').put()
  Category(category='Movies', description='Movies Stuff').put()
  inter = Category.gql("WHERE category = 'Internet'").get()
  devcat = Category.gql("WHERE category = 'development'").get()
  Url(title='Favorite Searcher', address='http://www.google.com', category=[inter.key()]).put()
  Url(title='Favorite Site', address='http://www.reddit.com', category=[inter.key()]).put()
  Url(title='Lisp Usenet Group', address='http://groups.google.com/comp.lang.lisp', category=[devcat.key()]).put()
  Url(title='Lisp Packages', address='http://www.cliki.net/asdf-install/', category=[devcat.key()]).put()



def doqueries():
  c = Category.gql("WHERE category = 'Internet'").get()
  for i_add in Url.all().filter("category =",c.key()).fetch(1000):
	print i_add.address


def tests():
    	#Query por categorias:
	#Print members of a category:
	development_urls = Url.gql("WHERE category = :categ ", categ=devcat.key())
	for x in development_urls.fetch(10):
	  print x.address

	#Print members of a category:
	internet_urls = Url.gql("WHERE category = :categ ", categ=inter.key())
	for x in internet_urls.fetch(10):
	  print x.address

	#Print members of a category:
	internet_members = inter.members.fetch(10)
	for imember in internet_members:
	  print imember.address

	#Print Members of a category shorter.
	for member in Category.gql("WHERE category = 'development'").get().members.fetch(10):
	  print member.address

	#Print All
	all_categories_qry = Category.all()
	for category in all_categories_qry:
	  urls_in_category = Url.gql("WHERE category = :cat", cat=category.key())
	  for url in urls_in_category.fetch(10):
	    print " %s , %s , %s" %(url.title, url.address, category.category)
    

#Delete 
#q = db.GqlQuery("SELECT * FROM Category WHERE category ='OS' ")
#results = q.fetch(1)
#for result in results:
#    result.delete()
#http://appengine-cookbook.appspot.com/recipe/efficient-paging-for-any-query-and-any-model/?id=ahJhcHBlbmdpbmUtY29va2Jvb2tylwELEgtSZWNpcGVJbmRleCI8YWhKaGNIQmxibWRwYm1VdFkyOXZhMkp2YjJ0eUZ3c1NDRU5oZEdWbmIzSjVJZ2xFWVhSaGMzUnZjbVVNDAsSBlJlY2lwZSI-YWhKaGNIQmxibWRwYm1VdFkyOXZhMkp2YjJ0eUZ3c1NDRU5oZEdWbmIzSjVJZ2xFWVhSaGMzUnZjbVVNNDIM
