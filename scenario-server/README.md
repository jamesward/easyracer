# Easy Racer Scenario Server

```
./sbt ~reStart
```

```
docker build -f Dockerfile-amd64 .
```

```
nix-shell -p graalvm-ce
NO_STATIC=true ./sbt GraalVMNativeImage/packageBin
```
