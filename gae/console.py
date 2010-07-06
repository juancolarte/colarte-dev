
#!/usr/bin/python
import code
import getpass
import sys
import datetime

sys.path.append("/home/colarte/dev/google_appengine")
sys.path.append("/home/colarte/dev/google_appengine/lib/yaml/lib")

from google.appengine.ext.remote_api import remote_api_stub
from google.appengine.ext import db

def auth_func():
    return raw_input('Username:'), getpass.getpass('Password:')

app_id = "colarte-code"
host = 'colarte-code.appspot.com'


remote_api_stub.ConfigureRemoteDatastore(app_id, '/remote_api', auth_func, host)
#from transaction import *
#code.interact('App Engine interactive console for %s' % (app_id,), None, locals())

