import { define } from "../utils.ts";

export default define.page(function Home(ctx) {
  const date = new Date().toISOString()
  return (
   	<p>[DATE_START]{date}[DATE_END]</p>
  );
});