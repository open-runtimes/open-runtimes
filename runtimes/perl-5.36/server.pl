#!/usr/bin/env perl

use Env;
use Try::Tiny;
use Data::Dump "pp";
use Capture::Tiny "tee";
use Mojolicious::Lite -signatures;
use feature 'unicode_strings';

use RuntimeRequest;
use RuntimeResponse;

use constant USER_CODE_PATH => '/usr/code-start';
use lib USER_CODE_PATH;

app->renderer->default_format('json');
app->exception_format('json');

post '/' => sub ($c) {
  my $challenge = $c->req->headers->header('x-internal-challenge') || '';

  if ($challenge eq '' || $challenge ne $ENV{INTERNAL_RUNTIME_KEY}) {
    $c->render(
      json => {
        code => 401,
        stderr => 'Unauthorized',
      }, 
      status => 401
    );
    return;
  }

  my $req = new RuntimeRequest(
    $c->req->json->{payload}, 
    $c->req->json->{variables}, 
    $c->req->json->{headers}
  );
  my $res = new RuntimeResponse();

  try {
    require "".USER_CODE_PATH."/".$ENV{INTERNAL_RUNTIME_ENTRYPOINT}."";

    if (!defined(&main)) {
      $c->render(
        json => {
          code => 500,
          stderr => 'Error: Module does not specify a main() function.'
        }, 
        status => 500
      );
      return;
    }
  
    my $output = tee {
      main($req, $res);
    };

    $c->render(json => {
      response => $res->getResponse(),
      stdout => $output,
    });  
  } catch {
    $c->render(
      json => {
        code => 500,
        stderr => "Error: $_"
      }, 
      status => 500
    );
  };
};

app->start;

__DATA__

@@ not_found.json.ep
{"code":404,"stderr":"Not found"}

@@ exception.json.ep
{"code":500,"stderr":$exception->message}
