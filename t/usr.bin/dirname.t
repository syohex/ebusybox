use strict;
use warnings;
use Test::More;

# See also http://pubs.opengroup.org/onlinepubs/009695399/utilities/dirname.html

subtest 'case 1' => sub {
    my $got = `ebusybox dirname //`;
    chomp $got;
    is $got, "/";
};

subtest 'case 2' => sub {
    my $got = `ebusybox dirname ////////////////////////////////////`;
    chomp $got;
    is $got, "/";
};

subtest 'case 4' => sub {
    my $got = `ebusybox dirname foobar//////`;
    chomp $got;
    is $got, ".";
};

subtest 'case 6' => sub {
    my $got = `ebusybox dirname //fooooo//////////`;
    chomp $got;
    is $got, "/";
};

subtest 'case 8' => sub {
    my $got = `ebusybox dirname ''`;
    chomp $got;
    is $got, '.';

    $got = `ebusybox dirname /foo/bar`;
    chomp $got;
    is $got, "/foo";
};

done_testing;
