# eucalypt-hs

[![CircleCI](https://circleci.com/gh/curvelogic/eucalypt-hs.svg?style=svg&circle-token=97ae77777028be6a88a53b23b78d5c858a49ef33)](https://circleci.com/gh/curvelogic/eucalypt-hs)

This is a prototype Haskell implementation of the Eucalypt language
for generating, templating, rendering and processing structured data
formats like YAML and JSON.

You need stack (`brew install haskell-stack`...)

For development, run something like:

```
stack install hlint
stack build --test --file-watch --fast --copy-bins --exec "hlint ."
```

To build and install the `eu` binary:

```
stack install
```

**Health warning**

This implementation is currently a very crude substitutional
interpreter for experimenting with the shape of the Eucalypt language.
It has many shortcuts and shortcomings. It's not complete, nor
necessarily even ready for casual use.
