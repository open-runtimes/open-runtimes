import { Component } from '@angular/core';

@Component({
  selector: 'app-exception',
  imports: [],
  templateUrl: './exception.component.html',
})
export class ExceptionComponent {
  msg = '';

  constructor() {
    throw new Error('Code exception occurred');
    this.msg = "No exceptions";
  }
}
