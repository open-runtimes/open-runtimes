ENV OPEN_RUNTIMES_ENTRYPOINT=index.ts
COPY package* /usr/local/server/
RUN apk update && apk add bash npm
RUN npm install pnpm yarn -g
RUN bun install
