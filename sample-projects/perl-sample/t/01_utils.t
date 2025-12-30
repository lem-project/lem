#!/usr/bin/env perl
use strict;
use warnings;
use v5.16;

use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../lib";

use MyApp::Utils qw(greet format_list trim is_empty slugify);

# Test greet()
subtest 'greet' => sub {
    is(greet("World"), "Hello, World!", "greet with name");
    is(greet(), "Hello, Guest!", "greet without name");
    is(greet(undef), "Hello, Guest!", "greet with undef");
};

# Test format_list()
subtest 'format_list' => sub {
    is(format_list(['a', 'b', 'c']), "a, b, c", "default separator");
    is(format_list(['a', 'b', 'c'], " | "), "a | b | c", "custom separator");
    is(format_list([]), "", "empty list");
    is(format_list("not an array"), "", "invalid input");
};

# Test trim()
subtest 'trim' => sub {
    is(trim("  hello  "), "hello", "trim both sides");
    is(trim("hello  "), "hello", "trim right");
    is(trim("  hello"), "hello", "trim left");
    is(trim("hello"), "hello", "no trim needed");
    is(trim(""), "", "empty string");
    is(trim(undef), "", "undef");
};

# Test is_empty()
subtest 'is_empty' => sub {
    ok(is_empty(undef), "undef is empty");
    ok(is_empty(""), "empty string is empty");
    ok(is_empty("   "), "whitespace is empty");
    ok(!is_empty("hello"), "hello is not empty");
    ok(!is_empty("0"), "zero string is not empty");
};

# Test slugify()
subtest 'slugify' => sub {
    is(slugify("Hello World"), "hello-world", "basic slugify");
    is(slugify("  Multiple   Spaces  "), "multiple-spaces", "multiple spaces");
    is(slugify("Special!@#\$%^&*()Chars"), "specialchars", "special chars removed");
    is(slugify("Already-slugified"), "already-slugified", "already slugified");
    is(slugify(""), "", "empty string");
    is(slugify(undef), "", "undef");
};

done_testing();
