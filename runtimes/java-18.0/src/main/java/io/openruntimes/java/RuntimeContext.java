package io.openruntimes.java;

import org.rapidoid.http.Req;

import java.util.Map;

public class RuntimeContext {
    public RuntimeRequest req;
    public RuntimeResponse res;

    public RuntimeContext(
            RuntimeRequest req,
            RuntimeResponse res
    ) {
        this.req = req;
        this.res = res;
    }
}


