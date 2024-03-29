BEGIN {
	bright = "\x1B[1m"
	red = "\x1B[31m"
	green = "\x1B[32m"
	cyan = "\x1B[36m"
	reset = "\x1B[0m"

	hit_diff = 0
}
{
	if (hit_diff == 0) {
		# Strip carriage returns from line
		gsub(/\r/, "", $0)

		if ($0 ~ /^diff /) {
			hit_diff = 1;
			print bright $0 reset
		} else if ($0 ~ /^.*\|.*(\+|-)/) {
			left = substr($0, 0, index($0, "|")-1)
			right = substr($0, index($0, "|"))
			gsub(/-+/, red "&" reset, right)
			gsub(/\++/, green "&" reset, right)
			print left right
		} else {
			print $0
		}
	} else {
		# Strip carriage returns from line
		gsub(/\r/, "", $0)

		if ($0 ~ /^-/) {
			print red $0 reset
		} else if ($0 ~ /^\+/) {
			print green $0 reset
		} else if ($0 ~ /^ /) {
			print $0
		} else if ($0 ~ /^@@ (-[0-9]+,[0-9]+ \+[0-9]+,[0-9]+) @@.*/) {
			sub(/^@@ (-[0-9]+,[0-9]+ \+[0-9]+,[0-9]+) @@/, cyan "&" reset)
			print $0
		} else {
			print bright $0 reset
		}
	}
}
