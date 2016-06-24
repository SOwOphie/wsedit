#!/bin/bash

if [ "$1" ]; then
	fl="$HOME/.config/wsedit.conf"
	echo >>"$fl"
	echo >>"$fl"
	echo >>"$fl"
	echo "# $1: added by lang/install.sh on `date`:" >>"$fl"
	echo >>"$fl"
	cat "lang/$1.wsconf" >> "$HOME/.config/wsedit.wsconf"
else
	echo "Please specify one of the following available languages:"
	ls lang | grep "wsconf" | sed 's:\(.*\)\.wsconf:\1:'
fi
