import { sveltekit } from "@sveltejs/kit/vite";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [sveltekit()],
  // Loaded from node_modules at runtime instead of being bundled
  ssr: {
    external: ["@repo/ui"],
  },
});
