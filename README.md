# Lindy Hop Aachen

[![Build Status](https://travis-ci.org/Y0hy0h/lindyhop-aachen.svg?branch=master)](https://travis-ci.org/Y0hy0h/lindyhop-aachen)

A website about all things Lindy Hop in Aachen.

## Development
To restart the server on changes, use [cargo-watch]:
```bash
cargo watch -x run
```

To also recompile the Elm admin:
```bash
cargo watch -s "cd admin/Elm && yarn build && cd ../.. && cargo run" -i "admin/dist/*" -i "admin/Elm/elm-stuff/**/*" -i "admin/Elm/node_modules/**/*"
```

[cargo-watch]: https://crates.io/crates/cargo-watch
