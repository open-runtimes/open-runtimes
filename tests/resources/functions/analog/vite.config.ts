/// <reference types="vitest" />

import { defineConfig } from 'vite';
import analog from '@analogjs/platform';

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => ({
  build: {
    target: ['es2020'],
  },
  resolve: {
    mainFields: ['module'],
  },
  plugins: [
    analog({
      ssr: true,
      nitro: {
        routeRules: {
          '/': {
            prerender: false,
          },
        },
      },
      prerender: {
        routes: [
          '/'
        ],
      },
    }),
  ],
}));
