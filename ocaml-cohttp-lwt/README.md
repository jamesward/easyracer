OCaml + Lwt + Cohttp
--------------------

Enable more open files:
```
ulimit -n 12000
```

Install dependencies:
```
opam install -y --deps-only --with-test .
```

Run:
```
opam exec -- dune exec -- bin/main.exe
```

Test:
```
opam exec -- dune runtest
```
