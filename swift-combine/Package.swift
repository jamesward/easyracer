// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "EasyRacer",
    platforms: [
        .macOS(.v15)
    ],
    dependencies: [
        .package(url: "https://github.com/OpenCombine/OpenCombine.git", from: "0.14.0"),
        .package(url: "https://github.com/alexsteinerde/docker-client-swift.git", from: "0.1.2"),
    ],
    targets: [
        .executableTarget(
            name: "EasyRacer",
            dependencies: [
                "OpenCombine",
                .product(name: "OpenCombineFoundation", package: "OpenCombine"),
                .product(name: "OpenCombineShim", package: "OpenCombine"),
            ]),
        .testTarget(
            name: "EasyRacerTests",
            dependencies: [
                "EasyRacer",
                .product(name: "DockerClientSwift", package: "docker-client-swift"),
            ]),
    ]
)
