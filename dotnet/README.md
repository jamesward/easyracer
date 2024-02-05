## Setup
To build and launch the server, run the following commands:

```bash
cd ./scenario-server

docker build -t easyracer -f ./Dockerfile-amd64 .

docker run -it -p8080:8080 easyracer --debug
```

Or just run the prebuilt container:
```bash
docker run -it -p8080:8080 ghcr.io/jamesward/easyracer --debug
```

## Run

