RUN <<EOR
    if [ -f /etc/alpine-release ]; then
        apk update && apk add bash alpine-sdk libffi-dev
    else
        apt-get update && apt-get install -y bash
    fi
EOR

# Install Poetry package manager
RUN pip3 install --no-cache-dir poetry

ENV OPEN_RUNTIMES_ENTRYPOINT=main.py

COPY requirements.txt .

# Create a virtual environment for the server dependencies
RUN python3 -m venv server-env
RUN server-env/bin/pip install --no-cache-dir -r requirements.txt