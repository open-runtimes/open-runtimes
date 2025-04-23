import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-concurrency',
  imports: [],
  templateUrl: './concurrency.component.html',
})
export class ConcurrencyComponent implements OnInit {
  msg = '';

  constructor() {
  }

  async ngOnInit() {
    for (let i = 1; i <= 3; i++) {
      console.log("Concurrent Log " + i);
      await new Promise(resolve => setTimeout(resolve, 500 + Math.random() * 500));
    }

    this.msg = "OK Response";
  }
}
