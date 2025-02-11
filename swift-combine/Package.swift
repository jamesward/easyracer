// swift-tools-version: 6.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "EasyRacer",
    platforms: [
        .macOS(.v13)
    ],
    dependencies: [
        .package(url: "https://github.com/alexsteinerde/docker-client-swift.git", from: "0.1.2"),
        .package(url: "https://github.com/apple/swift-atomics.git", from: "1.2.0"),
    ],
    targets: [
        .executableTarget(
            name: "EasyRacer",
            dependencies: [
                .product(name: "Atomics", package: "swift-atomics"),
            ]),
        .testTarget(
            name: "EasyRacerTests",
            dependencies: [
                "EasyRacer",
                .product(name: "DockerClientSwift", package: "docker-client-swift"),
            ]),
    ]
)
