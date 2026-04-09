RUN if [ -f /etc/oracle-release ]; then \
        microdnf install -y bash maven; \
    else \
        apk update && apk add bash maven; \
    fi

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.kt
