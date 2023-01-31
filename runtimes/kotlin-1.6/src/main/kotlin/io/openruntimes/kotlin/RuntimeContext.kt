package io.openruntimes.kotlin

import kotlin.Any
import kotlin.collections.ArrayList
import kotlin.collections.arrayListOf

class RuntimeContext(req: RuntimeRequest, res: RuntimeResponse) {
    var req: RuntimeRequest
    var res: RuntimeResponse

    var _logs: ArrayList<String>
    var _errors: ArrayList<String>

    init {
        this.req = req;
        this.res = res;
        this._logs = arrayListOf<String>()
        this._errors = arrayListOf<String>()
    }

    fun log(message: Any) {
        this._logs.add(message.toString())
    }

    fun error(message: Any) {
        this._errors.add(message.toString())
    }
}

