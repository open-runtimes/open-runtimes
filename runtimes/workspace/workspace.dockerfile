COPY package* /usr/local/server/

RUN apt update && apt install -y \
    build-essential \
    python3 \
    make \
    gcc \
    g++ \
    git \
    bash

RUN ln -sf /bin/bash /bin/sh

RUN npm install pnpm@10 -g

RUN npm ci && npm cache clean --force

# removes hostname from prompt
RUN echo 'PS1="\w \$ "' >> /root/.bashrc

ENV __VITE_ADDITIONAL_SERVER_ALLOWED_HOSTS=preview.torsten.work