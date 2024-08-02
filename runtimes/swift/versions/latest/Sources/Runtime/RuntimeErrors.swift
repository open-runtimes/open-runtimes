struct FileLocation: CustomStringConvertible {
    let file: String
    let line: Int
    let function: String

    var description: String {
        "\(file):\(line) \(function)"
    }
}

struct SourceLocationError: Error, CustomStringConvertible {
    var fileLocations: [FileLocation]
    let error: Error

    var description: String {
        let locs = fileLocations.dropLast(1).reversed().reduce("") { e, loc in
            e.appending("\(loc)\n")
        }
        return "\(error)\n\(locs)"
    }
}

func annotateError<T>(_ proc: @autoclosure () throws -> T, file: String = #fileID, line: Int = #line, function: String = #function) throws -> T {
    do {
        return try proc()
    } catch {
        if let error = error as? SourceLocationError {
            var error = error
            error.fileLocations.append(FileLocation(file: file, line: line, function: function))
            throw error
        } else {
            throw annotatedError(error, file: file, line: line, function: function)
        }
    }
}

func annotateError<T>(_ proc: @autoclosure () async throws -> T, file: String = #fileID, line: Int = #line, function: String = #function) async throws -> T {
    do {
        return try await proc()
    } catch {
        if let error = error as? SourceLocationError {
            var error = error
            error.fileLocations.append(FileLocation(file: file, line: line, function: function))
            throw error
        } else {
            throw annotatedError(error, file: file, line: line, function: function)
        }
    }
}

func annotatedError(_ error: Error, file: String = #fileID, line: Int = #line, function: String = #function) -> Error {
    SourceLocationError(fileLocations: [FileLocation(file: file, line: line, function: function)], error: error)
}