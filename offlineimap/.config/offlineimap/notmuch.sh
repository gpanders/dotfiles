#!/bin/sh

notmuch new
notmuch tag +inbox -- folder:/INBOX/
notmuch tag +archive -- folder:/Archive/
notmuch tag +sent -- folder:/Sent/
notmuch tag --remove-all +trash -- folder:/Trash/

# Remove new tag from anything not in the inbox
notmuch tag -new -inbox -- NOT tag:inbox
