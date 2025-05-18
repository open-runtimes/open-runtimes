import { Component } from '@angular/core';
import { toSignal } from '@angular/core/rxjs-interop';
import { injectLoad } from '@analogjs/router';

import { load } from './concurrency.server'; // not included in client build

@Component({
  selector: 'app-concurrency',
  standalone: true,
  template: `
    <p>{{ data().msg }}</p>
  `,
})
export default class HomeComponent {
  data = toSignal(injectLoad<typeof load>(), { requireSync: true });
}
