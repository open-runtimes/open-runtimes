RUN apk update && apk add bash

ENV OPEN_RUNTIMES_ENTRYPOINT=main.py

COPY requirements.txt .

# Create a virtual environment for the server dependencies
RUN python3 -m venv server-env
RUN server-env/bin/pip install --no-cache-dir -r requirements.txt