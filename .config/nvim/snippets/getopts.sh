while getopts "h$1" o; do
	case "\$o" in
		h) usage; exit 0 ;;
${1|split_getopts(S.v)}
		*) usage >&2; exit 1 ;;
	esac
done
shift \$((OPTIND-1))
