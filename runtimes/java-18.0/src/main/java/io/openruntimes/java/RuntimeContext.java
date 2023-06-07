package io.openruntimes.java;

import org.rapidoid.http.Req;

import com.google.gson.GsonBuilder;
import com.google.gson.Gson;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class RuntimeContext {
    private RuntimeRequest req;
    private RuntimeResponse res;

    private ArrayList<String> logs = new ArrayList<String>();
    private ArrayList<String> errors = new ArrayList<String>();

    public RuntimeRequest getReq() {
        return this.req;
    }

    public RuntimeResponse getRes() {
        return this.res;
    }

    public ArrayList<String> getLogs() {
        return this.logs;
    }

    public ArrayList<String> getErrors() {
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
        if(message instanceof Map || message instanceof List) {
            this.logs.add(gson.toJson(message));
        } else {
            this.logs.add(message.toString());
        }
    }

    public void error(Object message) {
        if(message instanceof Map || message instanceof List) {
            this.errors.add(gson.toJson(message));
        } else {
            this.errors.add(message.toString());
        }
    }
}


