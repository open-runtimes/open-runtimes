package io.openruntimes.kotlin

import kotlin.Any
import kotlin.collections.ArrayList
import kotlin.collections.arrayListOf

import com.google.gson.GsonBuilder
import com.google.gson.Gson

class RuntimeContext(val req: RuntimeRequest, val res: RuntimeResponse) {
    companion object {
        private val gson = GsonBuilder().serializeNulls().create()
    }

    val logs = mutableListOf<String>()
    val errors = mutableListOf<String>()

    fun log(message: Any) {
        if(message is Map<*, *> || message is List<*>) {
            this.logs.add(gson.toJson(message))
        } else {
            this.logs.add(message.toString())
        }
    }

    fun error(message: Any) {
        if(message is Map<*, *> || message is List<*>) {
            this.errors.add(gson.toJson(message))
        } else {
            this.errors.add(message.toString())
        }
    }
}

