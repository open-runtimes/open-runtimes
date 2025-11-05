import { define } from "../utils.ts";

export default define.page(function Home(ctx) {
  console.log("A log printed");
  console.error("An error printed");
  
  const msg = "All logs printed";
  
  return (
   	<p>{msg}</p>
  );
});