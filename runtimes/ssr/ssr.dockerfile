COPY package* /usr/local/server/

RUN npm install pnpm@9 -g
RUN npm install @remix-run/express -g

RUN npm ci && npm cache clean --force

# Framework helpers

RUN chmod +x /usr/local/server/helpers/astro/bundle.sh
RUN chmod +x /usr/local/server/helpers/astro/server.sh

RUN chmod +x /usr/local/server/helpers/sveltekit/bundle.sh
RUN chmod +x /usr/local/server/helpers/sveltekit/server.sh

RUN chmod +x /usr/local/server/helpers/next-js/bundle.sh
RUN chmod +x /usr/local/server/helpers/next-js/server.sh

RUN chmod +x /usr/local/server/helpers/nuxt/bundle.sh
RUN chmod +x /usr/local/server/helpers/nuxt/server.sh

RUN chmod +x /usr/local/server/helpers/remix/bundle.sh
RUN chmod +x /usr/local/server/helpers/remix/server.sh

RUN chmod +x /usr/local/server/helpers/angular/bundle.sh
RUN chmod +x /usr/local/server/helpers/angular/server.sh

# Tell SvelteKit auto adapter to use Node runtime
# Can be any string, as per https://github.com/sveltejs/kit/blob/main/packages/adapter-auto/adapters.js#L38C29-L39
ENV GCP_BUILDPACKS="open-runtimes"