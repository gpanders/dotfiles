#!/bin/bash

mail="$HOME/.mail"
count=$(find "$mail" -mindepth 4 -maxdepth 4 -path '*/Inbox/new/*' | wc -l)

if [[ -n "$count" && "$count" -gt 0 ]]; then
  echo "#[fg=red]${count// /}#[#{E:status-right-style}] unread - "
fi
