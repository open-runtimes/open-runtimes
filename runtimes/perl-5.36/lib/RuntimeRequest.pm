#!/usr/bin/env perl

package RuntimeRequest;
sub new {
    my ($class, $payload, $variables, $headers) = @_;
    my $self = {
        _payload => $payload || '{}',
        _variables => $variables || {},
        _headers => $headers || {},
    };
    bless $self, $class;
    return $self;
}

sub getPayload {
    my ($self) = @_;
    return $self->{_payload};
}

sub getVariables {
    my ($self) = @_;
    return $self->{_variables};
}

sub getHeaders {
    my ($self) = @_;
    return $self->{_headers};
}
1;
