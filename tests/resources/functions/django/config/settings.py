# A minimal Django project for the shared Serverless test actions: no database,
# no apps, every host allowed (the tests call it under several host names).
import os

SECRET_KEY = os.environ.get("DJANGO_SECRET_KEY", "open-runtimes-test")
DEBUG = False
ALLOWED_HOSTS = ["*"]

ROOT_URLCONF = "config.urls"
WSGI_APPLICATION = "config.wsgi.application"

INSTALLED_APPS = []
MIDDLEWARE = []
DATABASES = {}

# The tests send bodies up to 20 MB
DATA_UPLOAD_MAX_MEMORY_SIZE = 20 * 1024 * 1024

USE_TZ = True
