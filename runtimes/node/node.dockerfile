COPY package* /usr/local/server/

RUN npm install pnpm@9 -g

RUN npm ci && npm cache clean --force

# Framework helpers

RUN chmod +x /usr/local/server/helpers/astro/bundle.sh
RUN chmod +x /usr/local/server/helpers/astro/server.sh
