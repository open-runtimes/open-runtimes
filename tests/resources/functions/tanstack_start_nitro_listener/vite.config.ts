import { defineConfig } from 'vite'
import { tanstackStart } from '@tanstack/react-start/plugin/vite'
import viteReact from '@vitejs/plugin-react'
import viteTsConfigPaths from 'vite-tsconfig-paths'
import { nitroV2Plugin } from '@tanstack/nitro-v2-vite-plugin'

const config = defineConfig({
  plugins: [
    // this is the plugin that enables path aliases
    viteTsConfigPaths({
      projects: ['./tsconfig.json'],
    }),
    tanstackStart(),

    // Force the node-listener preset (exports a `listener`, does not self-listen),
    // overriding the NITRO_PRESET=node_server set by the runtime. Exercises the
    // listener-wrapper path in helpers/tanstack-start/server.sh.
    nitroV2Plugin({ preset: 'node-listener' }),

    viteReact(),
  ],
})

export default config
