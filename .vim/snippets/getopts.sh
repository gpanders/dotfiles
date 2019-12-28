while getopts "h" o; do
	case "$o" in
		h) usage; exit 0 ;;
		*) usage >&2; exit 1 ;;
	esac
done
shift $((OPTIND-1))
