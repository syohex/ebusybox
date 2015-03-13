use strict;
use warnings;
use Test::More;

chdir "src/bin/expr";

subtest 'one argument' => sub {
    my $got = `emacs --script expr.el 1`;
    is $got, "1\n";
};

subtest 'simple calculation' => sub {
    my $got = `emacs --script expr.el 1 + 1`;
    is $got, "2\n";

    $got = `emacs --script expr.el 1 - 1`;
    is $got, "0\n";

    $got = `emacs --script expr.el 7 \\* 8`;
    is $got, "56\n";

    $got = `emacs --script expr.el 4 / 2`;
    is $got, "2\n";

    $got = `emacs --script expr.el 9 ^ 3`;
    is $got, "729\n";
};

subtest 'complicate' => sub {
    my $got = `emacs --script expr.el \\( 1 + 5 \\) \\* \\( 2 + 3 \\)`;
    is $got, "30\n";
};

done_testing;
