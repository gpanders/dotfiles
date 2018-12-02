#!/bin/sh
notmuch new
notmuch tag +inbox +unread -new -- tag:new
notmuch tag +archive -inbox -unread -- folder:Personal/Archive
notmuch tag +sent -inbox -unread -- folder:Personal/Sent
notmuch tag +trash -inbox -unread -archive -- folder:Personal/Trash
