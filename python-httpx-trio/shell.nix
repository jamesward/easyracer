{ pkgs ? import <nixpkgs> {} }:
let
  my-python-packages = p: with p; [
    httpx
    testcontainers
    trio
    pytest
    deprecation
    docker
    wrapt
  ];
  my-python = pkgs.python311.withPackages my-python-packages;
in my-python.env
