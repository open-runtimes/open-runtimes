RUN if [ -f /etc/oracle-release ]; then \
        microdnf install -y zip bash maven; \
    else \
        apt-get update && apt-get install -y zip bash maven; \
    fi

ENV OPEN_RUNTIMES_ENTRYPOINT=Index.java
