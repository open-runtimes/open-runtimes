import 'zone.js/node';
import '@angular/platform-server/init';
import { enableProdMode } from '@angular/core';
import { bootstrapApplication, BootstrapContext } from '@angular/platform-browser';
import { renderApplication } from '@angular/platform-server';
import { provideServerContext } from '@analogjs/router/server';
import { ServerContext } from '@analogjs/router/tokens';

import { AppComponent } from './app/app.component';
import { config } from './app/app.config.server';

if (import.meta.env.PROD) {
  enableProdMode();
}

export function bootstrap(context: BootstrapContext) {
  return bootstrapApplication(AppComponent, config, context);
}

export default async function render(
  url: string,
  document: string,
  serverContext: ServerContext,
  bootstrapContext: BootstrapContext
) {
<<<<<<< Updated upstream
  const html = await renderApplication(() => bootstrap(bootstrapContext), {
=======
  const html = await renderApplication((bootstrapContext: BootstrapContext) => bootstrap(bootstrapContext), {
>>>>>>> Stashed changes
    document,
    url,
    platformProviders: [provideServerContext(serverContext)],
  });

  return html;
}
