
# formatting
elm-format src/ --yes >&2
elm-format tests/ --yes >&2

# tests
elm-verify-examples >&2
# elm-coverage . >&2
elm-test >&2

# docs
# elm make --docs docs.json >&2

# elm-optimize-level-2 examples/Main.elm --output=elm.js >&2

# elm-minify elm.js >&2

# gzip --keep --force elm.min.js >&2

# terser app.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle --output=app.min.js
# uglifyjs elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=elm.min.js
