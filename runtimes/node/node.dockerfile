COPY package* /usr/local/server/
RUN npm install pm2 -g
RUN npm ci && npm cache clean --force