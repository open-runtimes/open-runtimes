COPY package* /usr/local/server/

RUN apk update && apk add bash alpine-sdk python3-dev py3-pip

# Fix for Bun
RUN apk --no-cache add ca-certificates wget
RUN wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub
RUN wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.28-r0/glibc-2.28-r0.apk
RUN apk add --no-cache --force-overwrite glibc-2.28-r0.apk

RUN npm install pnpm@9.15.9 bun@1.2.15 -g

RUN npm ci && npm cache clean --force
