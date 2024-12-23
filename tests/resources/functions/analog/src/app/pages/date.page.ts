import { Component } from '@angular/core';

@Component({
  selector: 'app-date',
  standalone: true,
  template: `
    <p>[DATE_START]{{ date }}[DATE_END]</p>
  `,
})
export default class HomeComponent {
  date = '';

  constructor() {
    this.date = new Date().toISOString();
  }
}
