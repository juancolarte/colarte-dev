
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

