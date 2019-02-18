cat <<"EOF" > tmp.eu
` { target: :a format: :json doc: "Target a" }
foo: {
  value: :json
}

` { target: :b doc: "Target b" }
baz: {
  value: :yaml
}

` { target: :main format: :text }
x: {
  value: :text
  and: { x: :y }
}
EOF

echo "Targets are:"

eu -l tmp.eu

echo "JSON:"

eu -t a tmp.eu

echo "YAML:"

eu -t b tmp.eu

echo "TEXT:"

eu tmp.eu
