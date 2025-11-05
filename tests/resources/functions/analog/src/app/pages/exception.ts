import { Component } from '@angular/core';

@Component({
  selector: 'app-exception',
  standalone: true,
  template: `
    <p>{{ msg }}</p>
  `,
})
export default class HomeComponent {
  msg = '';

  constructor() {
    throw new Error('Code exception occurred');
    this.msg = "No exceptions";
  }
}
