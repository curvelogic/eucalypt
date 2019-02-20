#!/bin/bash

set -e

cat <<EOF | eu eu@- | grep Ref
a: 1234 // { tag: "!Ref" }
EOF
