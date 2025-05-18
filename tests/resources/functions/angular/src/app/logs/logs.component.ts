import { Component } from '@angular/core';

@Component({
  selector: 'app-logs',
  imports: [],
  templateUrl: './logs.component.html',
})
export class LogsComponent {
  msg = '';

  constructor() {
    console.log("A log printed");
    console.error("An error printed");

    this.msg = "All logs printed";
  }
}
