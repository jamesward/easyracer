// swift-tools-version:5.7
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "EasyRacer",
    platforms: [
        .macOS(.v10_14)
    ],
    dependencies: [
        .package(url: "https://github.com/jackgene/docker-client-swift.git", branch: "feature/string-ipaddress"),
    ],
    targets: [
        .executableTarget(
            name: "EasyRacer",
            dependencies: []),
        .testTarget(
            name: "EasyRacerTests",
            dependencies: [
                "EasyRacer",
                .product(name: "DockerClientSwift", package: "docker-client-swift")
            ])
    ]
)
