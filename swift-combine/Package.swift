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
        .package(url: "https://github.com/jackgene/DockerSwift.git", branch: "main"),
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
            dependencies: ["EasyRacer", "DockerSwift"]),
    ]
)
