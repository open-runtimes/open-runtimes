ENV OPEN_RUNTIMES_ENTRYPOINT=index.ts
COPY package* /usr/local/server/
RUN apk update && apk add bash npm nss font-noto ca-certificates python3 make g++ gcc git
RUN npm install pnpm yarn modclean@2.1.2 -g
RUN bun install && npm install --no-save --prefix /usr/local/server @vercel/nft@^0.29.2
