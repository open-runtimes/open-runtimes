import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-concurrency',
  standalone: true,
  template: `
    <p>OK Response</p>
  `,
})
export default class HomeComponent implements OnInit {
  constructor() {
  }

  async ngOnInit() {
    for (let i = 1; i <= 3; i++) {
      console.log("Concurrent Log " + i);
      await new Promise(resolve => setTimeout(resolve, Math.random() * 1000));
    }
  }
}
