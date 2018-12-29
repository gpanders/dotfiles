#!/bin/sh

if [ "$#" -eq 0 ]; then
  echo "$0: missing required argument"
  return 1
fi

notmuch new
notmuch tag +unread -new -- tag:new
notmuch tag +inbox -- folder:$1/INBOX
notmuch tag +archive -inbox -unread -- folder:$1/Archive
notmuch tag +sent -inbox -unread -- folder:$1/Sent OR folder:$1/Sent\ Mail
notmuch tag +trash -inbox -unread -archive -- folder:$1/Trash
