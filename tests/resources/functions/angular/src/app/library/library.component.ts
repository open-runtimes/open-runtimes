import { Component } from '@angular/core';

import { faker } from '@faker-js/faker';

@Component({
  selector: 'app-library',
  imports: [],
  templateUrl: './library.component.html',
})
export class LibraryComponent {
  msg = '';

  constructor() {
    const id = faker.string.uuid();

    this.msg = "My UUID is: " + id;
  }
}
