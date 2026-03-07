#!/bin/bash

cat <<EOF | eu -e 'a'
{ "c": "blah" }
EOF

if [ $? -eq 0 ]
then
    exit 1
else
    exit 0
fi
