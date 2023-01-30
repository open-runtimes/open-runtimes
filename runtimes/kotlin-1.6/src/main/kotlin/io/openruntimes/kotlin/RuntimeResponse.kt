package io.openruntimes.kotlin

import com.google.gson.GsonBuilder;
import com.google.gson.Gson;

public class RuntimeResponse {
    fun send(body: String, statusCode: Int, headers: MutableMap<String, String>): RuntimeOutput {
        return RuntimeOutput(body, statusCode, headers);
    }
    fun send(body: String, statusCode: Int): RuntimeOutput {
        return this.send(body, statusCode, mutableMapOf<String, String>());
    }
    fun send(body: String): RuntimeOutput {
        return this.send(body, 200, mutableMapOf<String, String>());
    }

    fun json(json: MutableMap<String, Any>, statusCode: Int, headers: MutableMap<String, String>): RuntimeOutput {
        var gson = GsonBuilder().serializeNulls().create();

        headers.put("content-type", "application/json");
        return this.send(gson.toJson(json), statusCode, headers);
    }
    fun json(json: MutableMap<String, Any>, statusCode: Int): RuntimeOutput {
        return this.json(json, statusCode, mutableMapOf<String, String>());
    }
    fun json(json: MutableMap<String, Any>): RuntimeOutput {
        return this.json(json, 200, mutableMapOf<String, String>());
    }

    fun empty(): RuntimeOutput {
        return this.send("", 204, mutableMapOf<String, String>());
    }

    fun redirect(url: String, statusCode: Int, headers: MutableMap<String, String>): RuntimeOutput {
        headers.put("location", url);
        return this.send("", statusCode, headers);
    }
    fun redirect(url: String, statusCode: Int): RuntimeOutput {
        return this.redirect(url, statusCode, mutableMapOf<String, String>());
    }
    fun redirect(url: String): RuntimeOutput {
        return this.redirect(url, 301, mutableMapOf<String, String>());
    }
}