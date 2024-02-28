import Foundation
import Vapor

extension Request {
    var uri: URI {
        let configuration = application.http.server.configuration
        
        var scheme = "http"
        var host = configuration.hostname
        var port = configuration.port
        
        if headers.contains(name: "x-forwarded-proto") {
            scheme = headers["x-forwarded-proto"].first!
        }
        
        if headers.contains(name: "host") {
            let hostHeader = headers["host"].first!
            
            if hostHeader.contains(":") {
                let parts = hostHeader.split(separator: ":")
                host = String(parts[0])
                port = Int(parts[1])!
            } else {
                host = hostHeader
                port = scheme == "http" ? 80 : 443
            }
        }
        
        return URI(
            scheme: scheme,
            host: host,
            port: port,
            path: url.path,
            query: url.query
        )
    }
}
