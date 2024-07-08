ENV OPEN_RUNTIMES_ENTRYPOINT=main.py
COPY requirements.txt .
ENV FLASK_APP="/usr/local/server/src/server.py"
