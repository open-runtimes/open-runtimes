package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import java.io.IOException;

public class RuntimeContext {
    private final RuntimeRequest req;
    private final RuntimeResponse res;
    private final RuntimeLogger logger;

    public RuntimeRequest getReq() {
        return this.req;
    }

    public RuntimeResponse getRes() {
        return this.res;
    }

    private static final Gson gson = new GsonBuilder().serializeNulls().create();

    public RuntimeContext(
            RuntimeRequest req,
            RuntimeResponse res,
            RuntimeLogger logger
    ) {
        this.req = req;
        this.res = res;
        this.logger = logger;
    }

    public void log(Object message) {
        try {
            this.logger.write(message, RuntimeLogger.TYPE_LOG, false);
        } catch(IOException e) {
            // Ignore missing logs
        }
    }

    public void error(Object message) {
        try {
            this.logger.write(message, RuntimeLogger.TYPE_ERROR, false);
        } catch(IOException e) {
            // Ignore missing logs
        }
    }
}


