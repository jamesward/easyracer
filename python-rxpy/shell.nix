{ pkgs ? import <nixpkgs> {} }:
let
  my-python-packages = p: with p; [
    rxpy
    requests
    testcontainers
    pytest
    pytest-asyncio
    deprecation
    docker
    wrapt
  ];
  my-python = pkgs.python3.withPackages my-python-packages;
in my-python.env
