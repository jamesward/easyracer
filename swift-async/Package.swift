// swift-tools-version: 6.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "EasyRacer",
    platforms: [
        .macOS(.v13)
    ],
    dependencies: [
        .package(url: "https://github.com/jackgene/DockerSwift.git", branch: "main"),
    ],
    targets: [
        .executableTarget(
            name: "EasyRacer",
            dependencies: []),
        .testTarget(
            name: "EasyRacerTests",
            dependencies: ["EasyRacer", "DockerSwift"]),
    ]
)
