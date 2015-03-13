use strict;
use warnings;
use Test::More;

chdir "src/bin/echo";

subtest 'normal' => sub {
    my $got = `emacs --script echo.el hello`;
    is $got, "hello\n";

    $got = `emacs --script echo.el hello world`;
    is $got, "hello world\n";
};

subtest 'n flag' => sub {
    my $got = `emacs --script echo.el -n hello`;
    is $got, "hello";

    $got = `emacs --script echo.el -n hello world`;
    is $got, "hello world";
};

subtest 'escaped character' => sub {
    my $got = `emacs --script echo.el "hello\nworld"`;
    is $got, "hello\nworld\n";

    $got = `emacs --script echo.el '\\0150\\0145\\0154\\0154\\0157'`;
    is $got, "hello\n";
};

done_testing;
