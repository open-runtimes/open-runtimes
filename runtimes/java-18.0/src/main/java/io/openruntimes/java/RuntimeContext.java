package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class RuntimeContext {
    private final RuntimeRequest req;
    private final RuntimeResponse res;

    private final List<String> logs = new ArrayList<>();
    private final List<String> errors = new ArrayList<>();

    public RuntimeRequest getReq() {
        return this.req;
    }

    public RuntimeResponse getRes() {
        return this.res;
    }

    public List<String> getLogs() {
        return this.logs;
    }

    public List<String> getErrors() {
        return this.errors;
    }

    private static final Gson gson = new GsonBuilder().serializeNulls().create();

    public RuntimeContext(
            RuntimeRequest req,
            RuntimeResponse res
    ) {
        this.req = req;
        this.res = res;
    }

    public void log(Object message) {
        if (message instanceof Map || message instanceof List || message instanceof Set) {
            this.logs.add(gson.toJson(message));
        } else {
            this.logs.add(message.toString());
        }
    }

    public void error(Object message) {
        if (message instanceof Map || message instanceof List || message instanceof Set) {
            this.errors.add(gson.toJson(message));
        } else {
            this.errors.add(message.toString());
        }
    }
}


