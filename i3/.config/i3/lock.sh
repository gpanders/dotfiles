#!/usr/bin/env bash

set -e

tmp=$(mktemp --dry-run tmp.XXXXXXXXXX.png)
method="background"
# method="screenshot"

# Use background
if [[ $method == "background" ]]; then
    bg=$(awk '/^feh/ {print $(NF)}' ~/.fehbg | tr -d "'")
    cp "$bg" "$tmp"
elif [[ $method == "screenshot" ]]; then
    # Blur current screen
    scrot --quality 50 "$tmp"
    mogrify -blur 0x8 -filter Gaussian "$tmp"
fi

# Lock screen displaying this image
i3lock --nofork --ignore-empty-password --image "$tmp" --tiling
rm -f "$tmp"
