package io.openruntimes.kotlin

import kotlin.Any
import kotlin.collections.ArrayList
import kotlin.collections.arrayListOf

import com.google.gson.GsonBuilder
import com.google.gson.Gson

class RuntimeContext(val req: RuntimeRequest, val res: RuntimeResponse) {
    val logs = mutableListOf<String>()
    val errors = mutableListOf<String>()

    fun log(message: Any) {
        if(message is Map<*, *> || message is List<*>) {
            val gson = GsonBuilder().serializeNulls().create()
            this.logs.add(gson.toJson(message))
        } else {
            this.logs.add(message.toString())
        }
    }

    fun error(message: Any) {
        if(message is Map<*, *> || message is List<*>) {
            val gson = GsonBuilder().serializeNulls().create()
            this.errors.add(gson.toJson(message))
        } else {
            this.errors.add(message.toString())
        }
    }
}

