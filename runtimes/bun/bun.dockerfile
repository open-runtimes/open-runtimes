ENV OPEN_RUNTIMES_ENTRYPOINT=index.ts
COPY package* /usr/local/server/
RUN apk update && apk add bash npm nss font-noto ca-certificates gcompat python3 make g++ gcc git
RUN npm install pnpm yarn -g
RUN bun install
