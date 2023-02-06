package io.openruntimes.kotlin

import kotlin.Any
import kotlin.collections.ArrayList
import kotlin.collections.arrayListOf

import com.google.gson.GsonBuilder
import com.google.gson.Gson

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
        if(message is Map<*, *> || message is List<*>) {
            var gson: Gson = GsonBuilder().serializeNulls().create()
            this.logs.add(gson.toJson(message))
        } else {
            this.logs.add(message.toString())
        }
    }

    fun error(message: Any) {
        if(message is Map<*, *> || message is List<*>) {
            var gson: Gson = GsonBuilder().serializeNulls().create()
            this.errors.add(gson.toJson(message))
        } else {
            this.errors.add(message.toString())
        }
    }
}

