Some Additional Info for Contributors
-------------------------------------

## Native Server

1. Install GraalVM
2. Install Native Image
3. Setup muslc
    ```
    mkdir native-toolchain
    cd native-toolchain
    
    wget https://more.musl.cc/10/x86_64-linux-musl/x86_64-linux-musl-native.tgz
    wget https://zlib.net/zlib-1.2.13.tar.gz
    
    tar -xvf x86_64-linux-musl-native.tgz
    tar -xvf zlib-1.2.13.tar.gz
    
    export TOOLCHAIN_DIR=$(realpath x86_64-linux-musl-native)
    export CC=$TOOLCHAIN_DIR/bin/gcc
    
    cd zlib-1.2.13
    ./configure --prefix=$TOOLCHAIN_DIR --static
    make
    make install
    
    export PATH=$TOOLCHAIN_DIR/bin:$PATH
    
    cd ../..
    ```
4. Create Native Image
    ```
    ./sbt GraalVMNativeImage/packageBin
    ```