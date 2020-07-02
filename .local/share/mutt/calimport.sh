#!/bin/sh

tmpdir=$(mktemp -d)
cp "$1" "$tmpdir"/orig.ics
sed 's/^METHOD:REQUEST$/METHOD:PUBLISH/' "$tmpdir"/orig.ics > "$1"
rm -rf "$tmpdir"

exec open "$1"
