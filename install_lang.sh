#!/bin/bash

if [ $(whoami) != "root" ]; then
	echo "Not running as root, will only install definitions for the current user."
	echo -n "Is this okay? [y|N] "
	read -n 1 answer
	echo

	if [ "${answer,,}" == "y" ]; then
		DESTDIR="$DESTDIR$HOME/.config/wsedit/lang/"
	else
		exit 1
	fi

else
	echo "Running as root, will install definitions for all users."
	echo -n "Is this okay? [y|N] "
	read -n 1 answer
	echo

	if [ "${answer,,}" == "y" ]; then
		DESTDIR="$DESTDIR/etc/wsedit/lang/"
	else
		exit 1
	fi
fi

if [ "$DESTDIR" != "" ]; then	# let's be cautious here
	rm -r "${DESTDIR}default/" 2>/dev/null
	mkdir -p "${DESTDIR}"{default,own}/
	cp -v lang/*.wsconf "${DESTDIR}default/"

else
	echo "Script bug detected, root removal prevented."

fi
