import Foundation

protocol CollectionType {}
extension Array : CollectionType {}
extension Set : CollectionType {}
extension Dictionary : CollectionType {}
extension NSArray : CollectionType {}
extension NSSet : CollectionType {}
extension NSDictionary : CollectionType {}

class RuntimeContext {
    let req: RuntimeRequest
    let res: RuntimeResponse
    var logs: [String] = []
    var errors: [String] = []

    init(request: RuntimeRequest, response: RuntimeResponse) {
        self.req = request
        self.res = response
    }

    func log(_ message: Any) {
        if message is CollectionType {
            if let data = try? JSONSerialization.data(withJSONObject: message),
               let string = String(data: data, encoding: .utf8) {
                logs.append(string)
                return
            }
        }
        
        logs.append(String(describing: message))
    }

    func error(_ message: Any) {
        if message is CollectionType {
            if let data = try? JSONSerialization.data(withJSONObject: message),
               let string = String(data: data, encoding: .utf8) {
                errors.append(string)
                return
            }
        }
        
        errors.append(String(describing: message))
    }

}
