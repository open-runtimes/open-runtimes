import Foundation

protocol CollectionType {}
extension Array: CollectionType {}
extension Set: CollectionType {}
extension Dictionary: CollectionType {}
extension NSArray: CollectionType {}
extension NSSet: CollectionType {}
extension NSDictionary: CollectionType {}

class RuntimeContext: @unchecked Sendable {
    let req: RuntimeRequest
    let res: RuntimeResponse
    let logger: RuntimeLogger

    init(request: RuntimeRequest, response: RuntimeResponse, logger: RuntimeLogger) {
        req = request
        res = response
        self.logger = logger
    }

    func log(_ message: Any) {
        logger.write(message: message, type: RuntimeLogger.TYPE_LOG)
    }

    func error(_ message: Any) {
        logger.write(message: message, type: RuntimeLogger.TYPE_ERROR)
    }
}
