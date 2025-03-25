import { Component } from '@angular/core';

import json from './.config/.file.json';

@Component({
  selector: 'app-hidden',
  standalone: true,
  template: `
    <p>{{ value }}</p>
  `,
})
export default class HomeComponent {
  value = '';

  constructor() {
    this.value = json.value;
  }
}
