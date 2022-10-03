#!/usr/bin/perl

use Mojolicious::Lite -signatures;
use Try::Tiny;
use Env;

use RuntimeRequest;
use RuntimeResponse;

post '/' => sub ($c) {
  my $req = new RuntimeRequest($c->req->json, {}, $c->req->headers->to_hash);
  my $res = new RuntimeResponse();

  my $challenge = $c->req->headers->header('HTTP_X_INTERNAL_CHALLENGE') || '';

  if ($challenge eq '' || $challenge ne $ENV{INTERNAL_RUNTIME_KEY}) {
    $c->render(
      json => {
        error => 'Unauthorized',
      }, status => 401
    );
    return;
  }

  try {
    $c->render(json => {
      payload => $req->payload(),
      variables => $req->variables(),
      headers => $req->headers()
    });  
  } catch {
    $c->render(
      json => {
        error => $_
      }, status => 500
    );
  };
};

app->start;
