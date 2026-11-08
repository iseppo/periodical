// seppo.ai/kihlveod/* and seppo.ai/tuleva/* — reports built daily by this repo's GitHub Action.
// Static assets are served without this script (free, unlimited). It only runs when no asset
// matches: directory URLs (dir/ → dir/index.html; html_handling is "none" so .html URLs
// are never redirected) and video, which lives in R2 (static assets can't serve byte ranges).
import { fromR2 } from "./r2.js";

export default {
  async fetch(request, env) {
    const path = decodeURIComponent(new URL(request.url).pathname);
    if (path.endsWith("/")) {
      const index = await env.ASSETS.fetch(new Request(new URL("index.html", request.url), request));
      if (index.ok) {
        if (!path.startsWith("/tuleva/")) return index;
        const res = new Response(index.body, index);
        res.headers.set("X-Robots-Tag", "noindex, nofollow");
        return res;
      }
    }
    if (request.method === "GET" || request.method === "HEAD") {
      const res = await fromR2(request, env.BIG, path.replace(/^\//, ""));
      if (res) return res;
    }
    return new Response("Not found", { status: 404, headers: { "Content-Type": "text/plain; charset=utf-8" } });
  },
};
