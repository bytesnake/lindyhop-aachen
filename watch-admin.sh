#!/bin/bash

cargo watch -s "yarn build && cargo run" -i "admin/dist/*" -i "admin/elm-stuff/**/*" -i "admin/node_modules/**/*"