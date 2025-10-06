import { faker } from '@faker-js/faker';

export const dynamic = 'force-dynamic';

export default function Page() {
  const id = faker.string.uuid();
  
  const msg = "My UUID is: " + id;

  return (
    <p>[UUID_START]{msg}[UUID_END]</p>
  );
}
