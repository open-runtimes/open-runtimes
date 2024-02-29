// swift-tools-version:5.9
import PackageDescription

let package = Package(
    name: "swift-runtime",
    dependencies: [
        .package(url: "https://github.com/swift-server/async-http-client.git", from: "1.20.1"),
    ],
    targets: [
        .executableTarget(
            name: "swift-function",
            dependencies: [
                .product(name: "AsyncHTTPClient", package: "async-http-client")
            ]
        )
    ]
)
