ENV OPEN_RUNTIMES_ENTRYPOINT=main.py
COPY requirements.txt .
RUN pip install -r requirements.txt
ENV FLASK_APP="/usr/local/server/src/server.py"
