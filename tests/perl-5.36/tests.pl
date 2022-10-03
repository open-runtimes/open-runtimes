#!/usr/bin/env perl

use JSON::XS;
use LWP::UserAgent ();

sub main {
   my ($req, $res) = @_;
   $payload = $req->getPayload();
   $variables = $req->getVariables();
   $headers = $req->getHeaders();

   print "log1\n{hello: world}\n[hello, world]";
   
   $id = decode_json($payload)->{'id'} || 1;

   $client = LWP::UserAgent->new;
   $response = $client->get("https://jsonplaceholder.typicode.com/todos/$id");

   $res->json({
      isTest => \1,
      message => "Hello Open Runtimes \x{1F44B}",
      header => $headers->{'x-test-header'},
      variable => $variables->{'test-variable'},
      todo => decode_json($response->decoded_content)
   });
}
1;
