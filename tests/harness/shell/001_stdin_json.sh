#!/bin/bash

set -e

cat <<EOF | eu -e 'a'
{ "a": 1234 }
EOF
