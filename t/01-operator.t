#!perl -T
use strict;
use warnings;
use Data::Dumper;
use Test::More 'no_plan';

use Color::Model::RGB qw(:all);

note("--- Operator overload (inherited)\n");
set_format('%02x%02x%02x',1);

my $col0 = -(W);
ok( ($col0->r == -1.0 && $col0->g == -1.0 && $col0->b == -1.0), "negate" );

my $col1 = R + G + B;
ok($col1->hexstr() eq 'ffffff', "addition");

my $col2 = $col1 - rgb(0.5,0.5,0.5);
ok($col2->hexstr() eq '808080', "subtract");

my $col3 = -$col2;
ok($col3->stringify('[%.2f,%.2f,%.2f]',0) eq '[-0.50,-0.50,-0.50]', "negate");

my $col4 = $col2 * 1.5; # 808080 * 1.5
ok($col4->hexstr() eq 'c0c0c0', "multiply");

my $col5 = $col4 / 3;
ok($col5->hexstr() eq '404040', "scalar divide");

ok("$col3,$col4,$col5" eq "000000,c0c0c0,404040", "stringify");


