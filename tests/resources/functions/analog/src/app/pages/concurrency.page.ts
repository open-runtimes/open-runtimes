import { Component, Input } from '@angular/core';
import { LoadResult } from '@analogjs/router';

import { load } from './concurrency.server'; // not included in client build

@Component({
  selector: 'app-concurrency',
  standalone: true,
  template: `
    @if (data.ok) {
      <p>OK Response</p>
    }
    @else {
      <p>FAIL</p>
    }
  `,
})
export default class HomeComponent {
  @Input() load(data: LoadResult<typeof load>) {
    this.data = data;
  }

  data!: LoadResult<typeof load>;
}
