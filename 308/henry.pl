my $num = 100; #how many primes to find
my $n = 1;
my $d = 1;

while ($pcount < $num) {
	$n++;
	$steps += 3*$n + $d - 2; # 3n + d' - 2
	$d = $n - 1;
	while (1) {
		my $r = $n % $d;
		if ($r) {
			my $q = ($n - $r) / $d;
			#print "$n quot $d = $q\n";
			$steps += 6*$n + 2*$q + 2;
		} else {
			$steps += 4*$n + 2*$n/$d + 1;
			$pcount++ if ($d == 1);
			last;
		}
		$d--;
	}
	$steps += 1;
}

print "$steps\n";
