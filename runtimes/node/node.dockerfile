COPY package* /usr/local/server/

# Yarn already installed
RUN npm install pm2 pnpm -g

RUN npm ci && npm cache clean --force