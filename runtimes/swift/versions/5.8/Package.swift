// swift-tools-version:5.8
import PackageDescription

let package = Package(
    name: "swift-runtime",
    platforms: [
        .macOS(.v10_15),
    ],
    dependencies: [
        // 💧 A server-side Swift web framework.
        .package(url: "https://github.com/vapor/vapor.git", from: "4.122.4"),
    ],
    targets: [
        .executableTarget(
            name: "Runtime",
            dependencies: [
                .product(name: "Vapor", package: "vapor"),
            ]
        ),
    ]
)
