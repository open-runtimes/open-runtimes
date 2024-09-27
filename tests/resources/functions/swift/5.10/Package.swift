// swift-tools-version:5.10
import PackageDescription

let package = Package(
    name: "swift-runtime",
    dependencies: [
        .package(url: "https://github.com/swift-server/async-http-client.git", from: "1.9.0"),
        .package(url: "https://github.com/apple/swift-crypto.git", "1.0.0" ..< "4.0.0"),
    ],
    targets: [
        .executableTarget(
            name: "swift-function",
            dependencies: [
                .product(name: "AsyncHTTPClient", package: "async-http-client"),
            ]
        ),
    ]
)
