#!/usr/bin/env perl

use JSON::XS;
use LWP::UserAgent ();
use Acme::RandomEmoji ();

sub main {
   my ($req, $res) = @_;
   $payload = $req->getPayload();
   $variables = $req->getVariables();
   $headers = $req->getHeaders();
   
   $id = $payload->{id} || 1;

   $client = LWP::UserAgent->new;
   $response = $client->get("https://jsonplaceholder.typicode.com/todos/$id");
   
   $emoji = Acme::RandomEmoji::random_emoji();

   $res->json({ 
      message => "Hello Open Runtimes $emoji",
      todo => decode_json($response->decoded_content)
   });
}
1;
