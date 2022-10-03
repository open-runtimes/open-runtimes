#!/usr/bin/env perl

package RuntimeResponse;
sub new {
    my $class = @_;
    my $self = {
        _res => {},
    };
    bless $self, $class;
    return $self;
}

sub send {
    my ($self, $text) = @_;
    $self->{_res} = $text if defined($text);
    return $self->{_res};
}

sub json {
    my ($self, $json) = @_;
    $self->{_res} = $json if defined($json);
    return $self->{_res};
}
1;
