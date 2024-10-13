ENV OPEN_RUNTIMES_ENTRYPOINT=main.py

# Create a virtual environment for the server dependencies
WORKDIR /usr/local/build
COPY requirements.txt .
RUN python3 -m venv runtime-env
RUN runtime-env/bin/pip install --no-cache-dir -r requirements.txt

ENV FLASK_APP="/usr/local/server/src/server.py"

WORKDIR /usr/local/server