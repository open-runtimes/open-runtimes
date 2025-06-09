COPY package* /usr/local/server/

# bash - required in all runtimes
# alpine-sdk - basic tooling like git, gcc, g++, make...
# python3-dev - Install Python (useful to some NPM libs)
# py3-pip - Python package manager
# gcompat - Pre-requirement for Bun
RUN apk update && apk add bash alpine-sdk python3-dev py3-pip gcompat

RUN npm install pnpm@9.15.9 bun@1.2.15 -g

RUN npm ci && npm cache clean --force
