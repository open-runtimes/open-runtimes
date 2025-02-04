// vite.config.ts
import { defineConfig } from "file:///workspace/open-runtimes/tests/resources/functions/analog/node_modules/vite/dist/node/index.js";
import analog from "file:///workspace/open-runtimes/tests/resources/functions/analog/node_modules/@analogjs/platform/src/index.js";
var vite_config_default = defineConfig(({ mode }) => ({
  build: {
    target: ["es2020"]
  },
  resolve: {
    mainFields: ["module"]
  },
  plugins: [
    analog({
      ssr: true,
      nitro: {
        routeRules: {
          "/": {
            prerender: false
          }
        }
      },
      prerender: {
        routes: [
          "/"
        ]
      }
    })
  ]
}));
export {
  vite_config_default as default
};
//# sourceMappingURL=data:application/json;base64,ewogICJ2ZXJzaW9uIjogMywKICAic291cmNlcyI6IFsidml0ZS5jb25maWcudHMiXSwKICAic291cmNlc0NvbnRlbnQiOiBbImNvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9kaXJuYW1lID0gXCIvd29ya3NwYWNlL29wZW4tcnVudGltZXMvdGVzdHMvcmVzb3VyY2VzL2Z1bmN0aW9ucy9hbmFsb2dcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfZmlsZW5hbWUgPSBcIi93b3Jrc3BhY2Uvb3Blbi1ydW50aW1lcy90ZXN0cy9yZXNvdXJjZXMvZnVuY3Rpb25zL2FuYWxvZy92aXRlLmNvbmZpZy50c1wiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9pbXBvcnRfbWV0YV91cmwgPSBcImZpbGU6Ly8vd29ya3NwYWNlL29wZW4tcnVudGltZXMvdGVzdHMvcmVzb3VyY2VzL2Z1bmN0aW9ucy9hbmFsb2cvdml0ZS5jb25maWcudHNcIjsvLy8gPHJlZmVyZW5jZSB0eXBlcz1cInZpdGVzdFwiIC8+XG5cbmltcG9ydCB7IGRlZmluZUNvbmZpZyB9IGZyb20gJ3ZpdGUnO1xuaW1wb3J0IGFuYWxvZyBmcm9tICdAYW5hbG9nanMvcGxhdGZvcm0nO1xuXG4vLyBodHRwczovL3ZpdGVqcy5kZXYvY29uZmlnL1xuZXhwb3J0IGRlZmF1bHQgZGVmaW5lQ29uZmlnKCh7IG1vZGUgfSkgPT4gKHtcbiAgYnVpbGQ6IHtcbiAgICB0YXJnZXQ6IFsnZXMyMDIwJ10sXG4gIH0sXG4gIHJlc29sdmU6IHtcbiAgICBtYWluRmllbGRzOiBbJ21vZHVsZSddLFxuICB9LFxuICBwbHVnaW5zOiBbXG4gICAgYW5hbG9nKHtcbiAgICAgIHNzcjogdHJ1ZSxcbiAgICAgIG5pdHJvOiB7XG4gICAgICAgIHJvdXRlUnVsZXM6IHtcbiAgICAgICAgICAnLyc6IHtcbiAgICAgICAgICAgIHByZXJlbmRlcjogZmFsc2UsXG4gICAgICAgICAgfSxcbiAgICAgICAgfSxcbiAgICAgIH0sXG4gICAgICBwcmVyZW5kZXI6IHtcbiAgICAgICAgcm91dGVzOiBbXG4gICAgICAgICAgJy8nXG4gICAgICAgIF0sXG4gICAgICB9LFxuICAgIH0pLFxuICBdLFxufSkpO1xuIl0sCiAgIm1hcHBpbmdzIjogIjtBQUVBLFNBQVMsb0JBQW9CO0FBQzdCLE9BQU8sWUFBWTtBQUduQixJQUFPLHNCQUFRLGFBQWEsQ0FBQyxFQUFFLEtBQUssT0FBTztBQUFBLEVBQ3pDLE9BQU87QUFBQSxJQUNMLFFBQVEsQ0FBQyxRQUFRO0FBQUEsRUFDbkI7QUFBQSxFQUNBLFNBQVM7QUFBQSxJQUNQLFlBQVksQ0FBQyxRQUFRO0FBQUEsRUFDdkI7QUFBQSxFQUNBLFNBQVM7QUFBQSxJQUNQLE9BQU87QUFBQSxNQUNMLEtBQUs7QUFBQSxNQUNMLE9BQU87QUFBQSxRQUNMLFlBQVk7QUFBQSxVQUNWLEtBQUs7QUFBQSxZQUNILFdBQVc7QUFBQSxVQUNiO0FBQUEsUUFDRjtBQUFBLE1BQ0Y7QUFBQSxNQUNBLFdBQVc7QUFBQSxRQUNULFFBQVE7QUFBQSxVQUNOO0FBQUEsUUFDRjtBQUFBLE1BQ0Y7QUFBQSxJQUNGLENBQUM7QUFBQSxFQUNIO0FBQ0YsRUFBRTsiLAogICJuYW1lcyI6IFtdCn0K
