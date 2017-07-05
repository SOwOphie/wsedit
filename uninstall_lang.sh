#!/bin/bash

if [ $(whoami) != "root" ]; then
	echo "Not running as root, will only remove user definitions."
	echo -n "Is this okay? [y|N] "
	read -n 1 answer
	echo

	if [ "${answer,,}" == "y" ]; then
		rm -r "${DESTDIR}${HOME}/.config/wsedit/lang/default"
		rmdir "${DESTDIR}${HOME}/.config/wsedit/lang/own"
		rmdir "${DESTDIR}${HOME}/.config/wsedit/lang"
		rmdir "${DESTDIR}${HOME}/.config/wsedit"
	else
		exit 1
	fi

else
	echo "Running as root, will only remove system definitions."
	echo -n "Is this okay? [y|N] "
	read -n 1 answer
	echo

	if [ "${answer,,}" == "y" ]; then
		rm -r "${DESTDIR}/etc/wsedit/lang/default"
		rmdir "${DESTDIR}/etc/wsedit/lang/own"
		rmdir "${DESTDIR}/etc/wsedit/lang"
		rmdir "${DESTDIR}/etc/wsedit"
	else
		exit 1
	fi
fi
