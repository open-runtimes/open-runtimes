#!/usr/bin/perl

use Mojolicious::Lite -signatures;
use Try::Tiny;
use Env;
use Data::Dump "pp";

use RuntimeRequest;
use RuntimeResponse;

post '/' => sub ($c) {
  my $json = $c->req->json;
  my $req = new RuntimeRequest(
    $json->{payload}, 
    $json->{variables}, 
    $json->{headers}
  );
  my $res = new RuntimeResponse();

  my $challenge = $c->req->headers->header('x-internal-challenge') || '';

  if ($challenge eq '' || $challenge ne $ENV{INTERNAL_RUNTIME_KEY}) {
    $c->render(
      json => {
        error => 'unauthorized',
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
        code => 500,
        message => "Error: $_"
      }, status => 500
    );
  };
};

app->start;
