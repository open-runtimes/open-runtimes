// swift-tools-version:5.8
import PackageDescription

let package = Package(
    name: "swift-runtime",
	platforms: [
	   .macOS(.v13)
	],
    dependencies: [
        .package(url: "https://github.com/swift-server/async-http-client.git", from: "1.0.0"),
    ],
    targets: [
        .executableTarget(
            name: "swift-function",
            dependencies: [
                .product(name: "AsyncHTTPClient", package: "async-http-client")
            ],
			path: "Sources/"
        )
    ]
)
