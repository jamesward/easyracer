# Linux image: runs Clojure integration tests with Testcontainers.
# Build:  docker compose build verify
# Run:    docker compose run --rm verify

FROM clojure:temurin-21-tools-deps

WORKDIR /app

# Prime dependency cache first for better layer reuse.
COPY deps.edn ./
RUN clojure -P -M:test

# Copy project sources after dependency resolution.
COPY src ./src
COPY README.md ./

# Run full test suite (all scenarios).
CMD ["clojure", "-M:test"]
