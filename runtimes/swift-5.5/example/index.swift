//
//  File.swift
//  
//
//  Created by Bradley Schofield on 21/12/2021.
//

import Foundation

func main(req: RequestValue, res: RequestResponse) -> RequestResponse {
    return res.json(data: ["hello": "world!"]);
}
