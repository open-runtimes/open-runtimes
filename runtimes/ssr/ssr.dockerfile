COPY package* /usr/local/server/

RUN npm install pnpm@9 -g
RUN npm install @remix-run/express -g

RUN npm ci && npm cache clean --force

RUN chmod -R +x /usr/local/server/helpers
