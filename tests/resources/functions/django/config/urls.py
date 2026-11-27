from django.urls import path, re_path

from . import views

urlpatterns = [
    # Django routing, beyond the shared actions
    path("users/<str:id>", views.user),
    re_path(r"^.*$", views.action),
]

handler500 = "config.views.server_error"
