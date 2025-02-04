import { Component } from '@angular/core';

@Component({
  selector: 'app-home',
  standalone: true,
  template: `
    <h1>Hello {{name}}</h1>
  `,
})
export default class HomeComponent {
  name = '';

  constructor() {
    this.name = 'Open Runtimes'; // This enforces SSR
  }
}
