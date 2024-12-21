import { Component } from '@angular/core';

@Component({
  selector: 'app-date',
  standalone: true,
  template: `
    <p id="date">{{ date }}</p>
  `,
})
export default class HomeComponent {
  date = '';

  constructor() {
    this.date = new Date().toISOString();
  }
}
