import { define } from "../utils.ts";

export default define.page(function Home(ctx) {
  throw new Error('Code exception occurred');
  
  const msg = "No exceptions";
  
  return (
   	<p>{msg}</p>
  );
});