#!/usr/bin/env perl

package RuntimeResponse;
sub new {
    my ($class) = @_;
    my $self = {
        _res => {},
    };
    bless $self, $class;
    return $self;
}

sub getResponse {
    my ($self) = @_;
    return $self->{_res};
}

sub send {
    my ($self, $text) = @_;
    $self->{_res} = $text if defined($text);
}

sub json {
    my ($self, $json) = @_;
    $self->{_res} = $json if defined($json);
}
1;
