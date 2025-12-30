package MyApp::Calculator;

use strict;
use warnings;
use v5.16;

use Carp qw(croak);

=head1 NAME

MyApp::Calculator - A simple calculator class

=head1 SYNOPSIS

    use MyApp::Calculator;

    my $calc = MyApp::Calculator->new();
    my $result = $calc->add(2, 3);

=cut

=head2 new(%args)

Creates a new Calculator instance.

=cut

sub new {
    my ($class, %args) = @_;

    my $self = {
        precision => $args{precision} // 2,
        history   => [],
    };

    return bless $self, $class;
}

=head2 add($a, $b)

Adds two numbers and returns the result.

=cut

sub add {
    my ($self, $a, $b) = @_;
    $self->_validate_numbers($a, $b);

    my $result = $a + $b;
    $self->_record_history("$a + $b = $result");

    return $result;
}

=head2 subtract($a, $b)

Subtracts $b from $a and returns the result.

=cut

sub subtract {
    my ($self, $a, $b) = @_;
    $self->_validate_numbers($a, $b);

    my $result = $a - $b;
    $self->_record_history("$a - $b = $result");

    return $result;
}

=head2 multiply($a, $b)

Multiplies two numbers and returns the result.

=cut

sub multiply {
    my ($self, $a, $b) = @_;
    $self->_validate_numbers($a, $b);

    my $result = $a * $b;
    $self->_record_history("$a * $b = $result");

    return $result;
}

=head2 divide($a, $b)

Divides $a by $b and returns the result.
Throws an exception if $b is zero.

=cut

sub divide {
    my ($self, $a, $b) = @_;
    $self->_validate_numbers($a, $b);

    croak "Division by zero" if $b == 0;

    my $result = sprintf("%.*f", $self->{precision}, $a / $b);
    $self->_record_history("$a / $b = $result");

    return $result + 0;  # Convert back to number
}

=head2 power($base, $exponent)

Raises $base to the power of $exponent.

=cut

sub power {
    my ($self, $base, $exponent) = @_;
    $self->_validate_numbers($base, $exponent);

    my $result = $base ** $exponent;
    $self->_record_history("$base ^ $exponent = $result");

    return $result;
}

=head2 get_history()

Returns an array reference of calculation history.

=cut

sub get_history {
    my ($self) = @_;
    return [ @{$self->{history}} ];
}

=head2 clear_history()

Clears the calculation history.

=cut

sub clear_history {
    my ($self) = @_;
    $self->{history} = [];
    return;
}

# Private methods

sub _validate_numbers {
    my ($self, @numbers) = @_;

    for my $num (@numbers) {
        unless (defined $num && $num =~ /^-?\d+(?:\.\d+)?$/) {
            croak "Invalid number: " . (defined $num ? $num : "undef");
        }
    }

    return 1;
}

sub _record_history {
    my ($self, $entry) = @_;
    push @{$self->{history}}, $entry;
    return;
}

1;

__END__

=head1 AUTHOR

Sample Author

=cut
