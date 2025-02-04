import { Component } from '@angular/core';
import { faker } from '@faker-js/faker';

@Component({
  selector: 'app-library',
  standalone: true,
  template: `
    <p>[UUID_START]{{ msg }}[UUID_END]</p>
  `,
})
export default class HomeComponent {
  msg = '';

  constructor() {
    const id = faker.string.uuid();

    this.msg = "My UUID is: " + id;
  }
}
