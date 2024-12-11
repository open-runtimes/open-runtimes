import { Component } from '@angular/core';

@Component({
  selector: 'app-date',
  imports: [],
  templateUrl: './date.component.html',
})
export class DateComponent {
  date = '';

  constructor() {
    this.date = new Date().toISOString();
  }
}
