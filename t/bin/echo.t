use strict;
use warnings;
use Test::More;

subtest 'normal' => sub {
    my $got = `ebusybox echo hello`;
    is $got, "hello\n";

    $got = `ebusybox echo hello world`;
    is $got, "hello world\n";
};

subtest 'n flag' => sub {
    my $got = `ebusybox echo -n hello`;
    is $got, "hello";

    $got = `ebusybox echo -n hello world`;
    is $got, "hello world";
};

subtest 'escaped character' => sub {
    my $got = `ebusybox echo "hello\nworld"`;
    is $got, "hello\nworld\n";

    $got = `ebusybox echo '\\0150\\0145\\0154\\0154\\0157'`;
    is $got, "hello\n";
};

done_testing;
