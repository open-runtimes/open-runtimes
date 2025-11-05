import { define } from "../utils.ts";
import json from './.config/.file.json';

export default define.page(function Home(ctx) {
  return (
    <div>
      {json.value}
    </div>
  );
});