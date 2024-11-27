COPY package* /usr/local/server/

RUN npm install pnpm@9 -g

RUN npm ci && npm cache clean --force