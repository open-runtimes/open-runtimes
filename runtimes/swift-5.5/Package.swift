// swift-tools-version:5.5
import PackageDescription

let package = Package(
    name: "swift-runtime",
    dependencies: [
        // 💧 A server-side Swift web framework.
        .package(url: "https://github.com/vapor/vapor.git", from: "4.0.0"),
    ],
    targets: [
        .executableTarget(
            name: "Runtime",
            dependencies: [
                .product(name: "Vapor", package: "vapor")
            ],
            swiftSettings: [
               .unsafeFlags(["-cross-module-optimization"], .when(configuration: .release))
           ]
       )
    ]
)
