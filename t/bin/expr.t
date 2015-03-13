use strict;
use warnings;
use Test::More;

subtest 'one argument' => sub {
    my $got = `ebusybox expr 1`;
    is $got, "1\n";
};

subtest 'simple calculation' => sub {
    my $got = `ebusybox expr 1 + 1`;
    is $got, "2\n";

    $got = `ebusybox expr 1 - 1`;
    is $got, "0\n";

    $got = `ebusybox expr 7 \\* 8`;
    is $got, "56\n";

    $got = `ebusybox expr 4 / 2`;
    is $got, "2\n";

    $got = `ebusybox expr 9 ^ 3`;
    is $got, "729\n";
};

subtest 'complicate' => sub {
    my $got = `ebusybox expr \\( 1 + 5 \\) \\* \\( 2 + 3 \\)`;
    is $got, "30\n";
};

done_testing;
