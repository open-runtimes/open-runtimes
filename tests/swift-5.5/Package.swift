// swift-tools-version:5.5
import PackageDescription

let package = Package(
    name: "swift-runtime",
    dependencies: [
        .package(url: "https://github.com/apple/swift-collections.git", from: "1.0.0"),
    ],
    targets: [
        .executableTarget(
            name: "swift-function",
            dependencies: [
                .product(name: "Collections", package: "swift-collections")
            ]
        )
    ]
)
