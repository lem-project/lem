#!/usr/bin/env perl
use strict;
use warnings;
use v5.16;

use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use lib "$Bin/../lib";

use MyApp::Calculator;

# Test constructor
subtest 'new' => sub {
    my $calc = MyApp::Calculator->new();
    isa_ok($calc, 'MyApp::Calculator');

    my $calc_precise = MyApp::Calculator->new(precision => 4);
    isa_ok($calc_precise, 'MyApp::Calculator');
};

# Test add()
subtest 'add' => sub {
    my $calc = MyApp::Calculator->new();

    is($calc->add(2, 3), 5, "2 + 3 = 5");
    is($calc->add(-5, 10), 5, "-5 + 10 = 5");
    is($calc->add(0, 0), 0, "0 + 0 = 0");
    is($calc->add(1.5, 2.5), 4, "1.5 + 2.5 = 4");
};

# Test subtract()
subtest 'subtract' => sub {
    my $calc = MyApp::Calculator->new();

    is($calc->subtract(10, 3), 7, "10 - 3 = 7");
    is($calc->subtract(5, 10), -5, "5 - 10 = -5");
    is($calc->subtract(0, 0), 0, "0 - 0 = 0");
};

# Test multiply()
subtest 'multiply' => sub {
    my $calc = MyApp::Calculator->new();

    is($calc->multiply(3, 4), 12, "3 * 4 = 12");
    is($calc->multiply(-3, 4), -12, "-3 * 4 = -12");
    is($calc->multiply(0, 100), 0, "0 * 100 = 0");
};

# Test divide()
subtest 'divide' => sub {
    my $calc = MyApp::Calculator->new();

    is($calc->divide(10, 2), 5, "10 / 2 = 5");
    is($calc->divide(7, 2), 3.5, "7 / 2 = 3.5");

    throws_ok { $calc->divide(10, 0) } qr/Division by zero/, "division by zero throws";
};

# Test power()
subtest 'power' => sub {
    my $calc = MyApp::Calculator->new();

    is($calc->power(2, 3), 8, "2 ^ 3 = 8");
    is($calc->power(10, 0), 1, "10 ^ 0 = 1");
    is($calc->power(5, 1), 5, "5 ^ 1 = 5");
};

# Test history
subtest 'history' => sub {
    my $calc = MyApp::Calculator->new();

    my $history = $calc->get_history();
    is_deeply($history, [], "initial history is empty");

    $calc->add(1, 2);
    $calc->multiply(3, 4);

    $history = $calc->get_history();
    is(scalar @$history, 2, "history has 2 entries");

    $calc->clear_history();
    $history = $calc->get_history();
    is_deeply($history, [], "history cleared");
};

# Test validation
subtest 'validation' => sub {
    my $calc = MyApp::Calculator->new();

    throws_ok { $calc->add("abc", 1) } qr/Invalid number/, "invalid first arg";
    throws_ok { $calc->add(1, undef) } qr/Invalid number/, "undef arg";
};

done_testing();
