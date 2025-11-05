import { define } from "../utils.ts";
import { faker } from "jsr:@jackfiszr/faker@1.1.6";

export default define.page(function Home(ctx) {
  const id = faker.random.uuid();
  
  const msg = "My UUID is: " + id;
  
  return (
   	<p>[UUID_START]{msg}[UUID_END]</p>
  );
});