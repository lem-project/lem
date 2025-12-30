package MyApp::Utils;

use strict;
use warnings;
use v5.16;

use Exporter qw(import);

our @EXPORT_OK = qw(
    greet
    format_list
    trim
    is_empty
    slugify
);

=head1 NAME

MyApp::Utils - Utility functions for the application

=head1 SYNOPSIS

    use MyApp::Utils qw(greet format_list);

    my $greeting = greet("World");
    my $list = format_list(['a', 'b', 'c']);

=cut

=head2 greet($name)

Returns a greeting message for the given name.

=cut

sub greet {
    my ($name) = @_;
    $name //= "Guest";
    return "Hello, $name!";
}

=head2 format_list($arrayref, $separator)

Formats an array reference as a string with the given separator.

=cut

sub format_list {
    my ($items, $separator) = @_;
    $separator //= ", ";

    return "" unless ref $items eq 'ARRAY';
    return join($separator, @$items);
}

=head2 trim($string)

Removes leading and trailing whitespace from a string.

=cut

sub trim {
    my ($str) = @_;
    return "" unless defined $str;

    $str =~ s/^\s+//;
    $str =~ s/\s+$//;

    return $str;
}

=head2 is_empty($value)

Returns true if the value is undefined, empty string, or only whitespace.

=cut

sub is_empty {
    my ($value) = @_;
    return 1 unless defined $value;
    return 1 if $value eq "";
    return 1 if $value =~ /^\s*$/;
    return 0;
}

=head2 slugify($string)

Converts a string to a URL-friendly slug.

=cut

sub slugify {
    my ($str) = @_;
    return "" unless defined $str;

    $str = lc($str);
    $str =~ s/[^a-z0-9\s-]//g;
    $str =~ s/\s+/-/g;
    $str =~ s/-+/-/g;
    $str =~ s/^-|-$//g;

    return $str;
}

1;

__END__

=head1 AUTHOR

Sample Author

=head1 LICENSE

This is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut
