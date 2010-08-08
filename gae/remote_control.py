#!/usr/bin/env python
#
import os
import sys
from google.appengine.ext import db
from google.appengine.ext.remote_api import remote_api_stub
import getpass

appengine_dirs = ['/home/colarte/dev/google_appengine/']
sys.path.extend(appengine_dirs)
my_root_dir = os.path.abspath(os.path.dirname('/home/colarte/dev/python/GAE/colarte-dev/'))
sys.path.insert(0, my_root_dir)

APP_NAME = 'colarte-dev'
os.environ['AUTH_DOMAIN'] = 'gmail.com'

def auth_func():
    return (raw_input('Username:'), getpass.getpass('Password:'))

# Use local dev server by passing in as parameter:
# servername='localhost:8080'
# Otherwise, remote_api assumes you are targeting APP_NAME.appspot.com
remote_api_stub.ConfigureRemoteDatastore(APP_NAME, '/remote_api', auth_func)
from myapp.models.urlstore import Category
categories = Category.all()
for cat in categories:
    print cat.category
