#!/bin/sh

# This script is used by Mutt's send_multipart_alternative_filter option to
# convert a plain text email into HTML and bundle it into a
# multipart/alternative format.

printf 'text/html\n\n'

{
	pandoc -s --template email -M document-css=false -f commonmark_x -t html5 || \
	cmark || \
	markdown || \
	exit 1
} 2>/dev/null
