import { Component } from '@angular/core';

import json from './.config/.file.json';

@Component({
  selector: 'app-hidden',
  imports: [],
  templateUrl: './hidden.component.html',
})
export class HiddenComponent {
  value = '';

  constructor() {
    this.value = json.value;
  }
}
