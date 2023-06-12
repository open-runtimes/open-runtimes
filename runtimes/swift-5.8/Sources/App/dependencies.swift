import Foundation

let outpath = "/usr/local/src"
let outfile = "Package.swift"
let task = Process()
let pipe = Pipe()

task.standardOutput = pipe
task.standardError = pipe
task.arguments = ["-c", "swift package dump-package"]
task.executableURL = URL(fileURLWithPath: "/bin/bash")
try! task.run()

let data = pipe.fileHandleForReading.readDataToEndOfFile()
let json = try! JSONSerialization.jsonObject(with: data, options : .allowFragments) as! [String: Any]

var packages = [String]()
var products = [String]()

var packageBlacklist = ["vapor"]
var productBlacklist = ["Vapor"]

func getPackageString(location: String, requirementDict: [String: Any])->String?{
    if let branch = (requirementDict["branch"] as? [String])?.first{
        return ".package(url: \"\(location)\", branch: \"\(branch)\"),"
    }
    if let range = requirementDict["range"] as? [[String: Any]]{
        if let lowerBound = range.first?["lowerBound"] as? String{
            return ".package(url: \"\(location)\", from: \"\(lowerBound)\"),"
        }
    }
    print("Could not build package dependency string for location \(location)! Unexpected requirements dictionary: \(requirementDict)")
    return nil
}

func buildPackageStrings() {
    if let dependencies = json["dependencies"] as? [[String: Any]] {
        for dependency in dependencies {
            if let scm = dependency["sourceControl"] as? [[String: Any]] {
                for data in scm {
                    guard let identity = data["identity"] as? String,
                    let locations = data["location"] as? [String: Any],
                          let location = (locations["remote"] as? [String])?.first,
                    let requirementsDict = data["requirement"] as? [String: Any],
                    let packageString = getPackageString(location: location, requirementDict: requirementsDict) else{
                        continue
                    }
                    if packageBlacklist.contains(identity) {
                        continue
                    }
                    packages.append(packageString)
                }
            }
        }
    }
}

func buildProductStrings() {
    if let targets = json["targets"] as? [[String: Any]] {
        for target in targets {
            if let dependencies = target["dependencies"] as? [[String: Any]] {
                for dependency in dependencies {
                    let values = dependency["product"]! as! [Any]
                    let identity = values[0] as? String
                    let package = values[1] as? String
                
                    guard let identity = identity, let package = package else {
                        continue
                    }
                    if productBlacklist.contains(identity) {
                        continue
                    }

                    let output = ".product(name: \"\(identity)\", package: \"\(package)\"),"

                    products.append(output)
                }
            }
        }
    }
}

func writePackageStrings() {
    let path = "\(outpath)/\(outfile)"
    var text = try! String(contentsOfFile: path)
    let pattern = #"(.package\(.*)"#
    let regex = try! NSRegularExpression(pattern: pattern, options: [])
    let range = NSRange(text.startIndex..<text.endIndex, in: text)

    regex.enumerateMatches(
        in: text,
        options: [],
        range: range
    ) { (match, _, stop) in
        guard let match = match else { return }

        let lastRange = Range(match.range(at: match.numberOfRanges - 1), in: text)
        let lastMatch = String(text[lastRange!])
        var last = lastMatch
        if last.last != "," {
            last += ","
        }
        let replacement = last + "\n\t\t" + packages.joined(separator: ",\n\t\t")
        
        text = text.replacingOccurrences(of: lastMatch, with: replacement)

        // Set bool pointer to true to stop enumeration
        stop.pointee = true
    }
    try! text.write(
        to: URL(fileURLWithPath: path),
        atomically: true,
        encoding: .utf8
    )
}

func writeProductStrings() {
    let path = "\(outpath)/\(outfile)"
    var text = try! String(contentsOfFile: path)
    let pattern = #"(.product\(.*)"#
    let regex = try! NSRegularExpression(pattern: pattern, options: [])
    let range = NSRange(text.startIndex..<text.endIndex, in: text)

    regex.enumerateMatches(
        in: text,
        options: [],
        range: range
    ) { (match, _, stop) in
        guard let match = match else { return }
        
        let lastRange = Range(match.range(at: match.numberOfRanges - 1), in: text)
        let lastMatch = String(text[lastRange!])
        var last = lastMatch
        if last.last != "," {
            last += ","
        }
        let replacement = last + "\n\t\t\t\t" + products.joined(separator: ",\n\t\t\t\t")
        
        text = text.replacingOccurrences(of: lastMatch, with: replacement)
        
        // Set bool pointer to true to stop enumeration
        stop.pointee = true
    }
    try! text.write(
        to: URL(fileURLWithPath: path),
        atomically: true,
        encoding: .utf8
    )
}

buildPackageStrings()
buildProductStrings()
writePackageStrings()
writeProductStrings()
