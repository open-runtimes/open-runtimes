import Foundation

class RuntimeContext {
    var req: RuntimeRequest
    var res: RuntimeResponse
    var logs: [String] = []
    var errors: [String] = []

    init(request: RuntimeRequest, response: RuntimeResponse) {
        self.req = request
        self.res = response
    }

    func log(message: Any) {
        logs.append(String(describing: message))
    }

    func error(message: Any) {
        errors.append(String(describing: message))
    }

}
