
from google.appengine.ext import db

class Blobo(db.Model):
    title   = db.StringProperty()
    blob = db.BlobProperty()
