package io.openruntimes.kotlin

import kotlin.Any
import kotlin.collections.ArrayList
import kotlin.collections.arrayListOf

class RuntimeContext(req: RuntimeRequest, res: RuntimeResponse) {
    var req: RuntimeRequest
    var res: RuntimeResponse

    var logs: ArrayList<String>
    var errors: ArrayList<String>

    init {
        this.req = req;
        this.res = res;
        this.logs = arrayListOf<String>()
        this.errors = arrayListOf<String>()
    }

    fun log(message: Any) {
        this.logs.add(message.toString())
    }

    fun error(message: Any) {
        this.errors.add(message.toString())
    }
}

