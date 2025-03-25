RUN <<EOR
    if [ -f /etc/alpine-release ]; then
        apk update && apk add bash
    else
        apt-get update && apt-get install -y bash
    fi
EOR

ENV OPEN_RUNTIMES_ENTRYPOINT=main.py

COPY requirements.txt .

# Create a virtual environment for the server dependencies
RUN python3 -m venv server-env
RUN server-env/bin/pip install --no-cache-dir -r requirements.txt