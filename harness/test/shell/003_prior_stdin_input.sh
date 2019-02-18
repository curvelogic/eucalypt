#!/bin/bash

cat <<EOF > tmp.eu
x: y.foo
EOF

cat <<EOF | eu y=json@- tmp.eu
{ "foo": "bar", "fox": "bax" }
EOF
