import { Component } from '@angular/core';

@Component({
  selector: 'app-logs',
  standalone: true,
  template: `
    <p>{{ msg }}</p>
  `,
})
export default class HomeComponent {
  msg = '';

  constructor() {
    console.log("A log printed");
    console.error("An error printed");

    this.msg = "All logs printed";
  }
}
