#!/bin/bash

set -e

if [ ! -f $EXECUTABLE ] ; then
    echo "WARNING! $EXECUTABLE does not identify a file; please set to path of Eucalypt command line to test"
else
    echo "Preparing executable $EXECUTABLE"
    mkdir -p local/bin
    cp $EXECUTABLE local/bin/${EXECUTABLE##*/}
    chmod +x $EXECUTABLE
    export PATH=local/bin:$PATH

    echo "PATH is now $PATH, testing `eu` command:"

    eu --help

    if [ $? -ne 0 ] ; then
        echo "WARNING! eu binary is not executable on this architecture"
    fi
fi

exec "$@"
