COPY package* /usr/local/server/
RUN npm ci && npm cache clean --force