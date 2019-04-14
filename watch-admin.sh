#!/bin/bash

cargo watch -s "cd admin/Elm && yarn build && cd ../.. && cargo run" -i "admin/dist/*" -i "admin/Elm/elm-stuff/**/*" -i "admin/Elm/node_modules/**/*"