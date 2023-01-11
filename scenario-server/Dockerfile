FROM ghcr.io/graalvm/graalvm-ce:22.3.0 as builder
ARG TARGETARCH

WORKDIR /app
COPY . /app

RUN gu install native-image

# BEGIN PRE-REQUISITES FOR STATIC NATIVE IMAGES FOR GRAAL
# SEE: https://github.com/oracle/graal/blob/master/substratevm/StaticImages.md
ARG RESULT_LIB="/staticlibs"

RUN mkdir ${RESULT_LIB}

RUN cd ${RESULT_LIB} && \
    case ${TARGETARCH} in \
      "amd64") PARCH=x86_64 ;; \
      "arm64") PARCH=aarch64 ;; \
    esac && \
    curl -L -s -o musl.tar.gz https://musl.libc.org/releases/musl-1.2.3.tar.gz && \
    mkdir musl && tar -xzf musl.tar.gz -C musl --strip-components 1 && cd musl && \
    ./configure --disable-shared --prefix=${RESULT_LIB} &>/dev/null && \
    make -s && make install -s && \
    ln -s ${RESULT_LIB}/bin/musl-gcc ${RESULT_LIB}/bin/x86_64-linux-musl-gcc && \
    rm -rf ${RESULT_LIB}/muscl && rm -f ${RESULT_LIB}/musl.tar.gz && \
    cp /usr/lib/gcc/$PARCH-redhat-linux/11/libstdc++.a ${RESULT_LIB}/lib/

ENV PATH="$PATH:${RESULT_LIB}/bin"
ENV CC="musl-gcc"

RUN cd ${RESULT_LIB} && \
    curl -L -s -o zlib.tar.gz https://zlib.net/zlib-1.2.13.tar.gz && \
    tar -xzf zlib.tar.gz && \
    cd zlib-1.2.13 && \
    ./configure --static --prefix=${RESULT_LIB} &>/dev/null && \
    make -s && make install -s && \
    rm -rf ${RESULT_LIB}/zlib-1.2.13 && rm -f ${RESULT_LIB}/zlib.tar.gz
#END PRE-REQUISITES FOR STATIC NATIVE IMAGES FOR GRAAL

RUN cd /app && ./sbt clean GraalVMNativeImage/packageBin

# Binary Compression

RUN cd ${RESULT_LIB} && \
    case ${TARGETARCH} in \
      "amd64") PARCH=x86_64 ;; \
      "arm64") PARCH=aarch64 ;; \
    esac && \
    curl -L -s -o xz-libs.rpm https://rpmfind.net/linux/centos-stream/9-stream/BaseOS/$PARCH/os/Packages/xz-libs-5.2.5-8.el9.$PARCH.rpm && \
    rpm -iv --force xz-libs.rpm && \
    rm -f xz-libs.rpm && \
    curl -L -s -o xz.rpm https://rpmfind.net/linux/centos-stream/9-stream/BaseOS/$PARCH/os/Packages/xz-5.2.5-8.el9.$PARCH.rpm && \
    rpm -iv xz.rpm && \
    rm -f xz.rpm && \
    curl -L -s -o upx-${TARGETARCH}_linux.tar.xz https://github.com/upx/upx/releases/download/v4.0.1/upx-4.0.1-${TARGETARCH}_linux.tar.xz && \
    tar -xf upx-${TARGETARCH}_linux.tar.xz && \
    rm -f upx-${TARGETARCH}_linux.tar.xz

RUN ${RESULT_LIB}/upx-4.0.1-${TARGETARCH}_linux/upx -7 /app/target/graalvm-native-image/easyracer-server

FROM scratch

WORKDIR /tmp

COPY --from=builder /app/target/graalvm-native-image/easyracer-server /easyracer-server

ENTRYPOINT ["/easyracer-server"]