Java + CompletableFuture
-----------------------

## How to run in local

```bash
./mvnw clean verify
./mvnw dependency:tree

sysctl kern.maxfiles 
sysctl kern.maxfilesperproc
ulimit
lsof -t -i:55023 | xargs kill

./mvnw versions:display-dependency-updates
./mvnw versions:display-plugin-updates
```

## Interesting links

- [Requirements to implement the scenarios](../README.md)
- https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/concurrent/CompletableFuture.html
- https://docs.oracle.com/en/java/javase/21/docs/api/java.net.http/java/net/http/HttpClient.html