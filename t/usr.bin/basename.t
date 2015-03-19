use strict;
use warnings;
use Test::More;

# See also http://pubs.opengroup.org/onlinepubs/009695399/utilities/basename.html

subtest 'case 2' => sub {
    my $got = `ebusybox basename //`;
    is $got, "/\n";
};

subtest 'case 3' => sub {
    my $got = `ebusybox basename ////////////////////////////////////`;
    is $got, "/\n";
};

subtest 'case 4' => sub {
    my $got = `ebusybox basename foobar//////`;
    is $got, "foobar\n";
};

subtest 'case 5' => sub {
    my $got = `ebusybox basename /foo/bar/baz`;
    is $got, "baz\n";
};

subtest 'case 6' => sub {
    my $got = `ebusybox basename /foo/bar/baz.c .c`;
    is $got, "baz\n";

    $got = `ebusybox basename /foo/bar/baz.c .java`;
    is $got, "baz.c\n";
};

done_testing;
