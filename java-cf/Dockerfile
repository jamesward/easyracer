# Linux image: full Maven verify (includes Testcontainers against Easy Racer).
# Build:  docker build -t java-cf .
# Run:    docker run --rm -v /var/run/docker.sock:/var/run/docker.sock java-cf
#
# Tests need access to the host Docker daemon (socket mount). Ensure enough RAM
# for Maven (.mvn/jvm.config uses a large heap).

FROM eclipse-temurin:25-jdk-jammy

WORKDIR /app

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        unzip \
    && rm -rf /var/lib/apt/lists/*

COPY . .

RUN chmod +x mvnw \
    && ./mvnw -B -q dependency:go-offline clean package -DskipTests

CMD ["./mvnw", "-B", "clean", "verify"]
