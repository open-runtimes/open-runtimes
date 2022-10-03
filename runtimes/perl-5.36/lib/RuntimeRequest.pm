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

sub payload {
    my ($self) = @_;
    return $self->{_payload};
}

sub variables {
    my ($self) = @_;
    return $self->{_variables};
}

sub headers {
    my ($self) = @_;
    return $self->{_headers};
}
1;
