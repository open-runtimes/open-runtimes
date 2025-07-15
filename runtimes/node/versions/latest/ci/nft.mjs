// Node file Trace

import { nodeFileTrace } from "@vercel/nft";
import { copyFile, mkdir } from "fs/promises";
import { dirname } from "path";

(async () => {
  const response = await nodeFileTrace(["./server.mjs"]);

  for (const file of response.fileList) {
    if (file.startsWith("node_modules")) {
      const from = "/tmp/nft/" + file;

      const parts = file.split("/");
      parts[0] = "node_modules_min";
      const to = "/tmp/nft/" + parts.join("/");

      await mkdir(dirname(to), { recursive: true });
      await copyFile(from, to);
    }
  }
})();
