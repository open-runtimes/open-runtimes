//
//  File.swift
//  
//
//  Created by Bradley Schofield on 21/12/2021.
//

import Foundation
import Collections

func main(req: RequestValue, res: RequestResponse) -> RequestResponse {
    return res.json(data: ["n": Double.random(in: 0..<1.0)]);
}
