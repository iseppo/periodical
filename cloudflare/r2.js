// Serve an R2 object with Range support (static assets can't do byte ranges; video needs them).
// Copy of seppoai/cloudflare/src/r2.js — keep them in sync.

const TYPES = {
  mp4: "video/mp4", m4v: "video/mp4", mov: "video/quicktime", webm: "video/webm",
  mp3: "audio/mpeg", m4a: "audio/mp4", wav: "audio/wav",
  zip: "application/zip", gz: "application/gzip", pdf: "application/pdf",
  csv: "text/csv", txt: "text/plain", html: "text/html", css: "text/css",
  js: "application/javascript", json: "application/json",
  png: "image/png", jpg: "image/jpeg", jpeg: "image/jpeg", gif: "image/gif",
  svg: "image/svg+xml", webp: "image/webp", woff: "font/woff", woff2: "font/woff2",
};

export function contentTypeFor(key) {
  return TYPES[key.split(".").pop().toLowerCase()] || "application/octet-stream";
}

export async function fromR2(request, bucket, key) {
  if (!key || !bucket) return null;
  const range = parseRange(request.headers.get("Range"));
  const obj = request.method === "HEAD"
    ? await bucket.head(key)
    : await bucket.get(key, range ? { range } : {});
  if (!obj) return null;

  const headers = new Headers();
  obj.writeHttpMetadata(headers);
  headers.set("ETag", obj.httpEtag);
  headers.set("Last-Modified", obj.uploaded.toUTCString());
  headers.set("Accept-Ranges", "bytes");
  if (!headers.has("Content-Type")) headers.set("Content-Type", contentTypeFor(key));

  if (range && obj.range) {
    const start = obj.range.offset ?? 0;
    const length = obj.range.length ?? obj.size - start;
    headers.set("Content-Range", `bytes ${start}-${start + length - 1}/${obj.size}`);
    headers.set("Content-Length", String(length));
    return new Response(obj.body, { status: 206, headers });
  }
  headers.set("Content-Length", String(obj.size));
  return new Response(request.method === "HEAD" ? null : obj.body, { headers });
}

function parseRange(value) {
  const m = value && /^bytes=(\d*)-(\d*)$/.exec(value.trim());
  if (!m) return null;
  if (m[1] === "" && m[2] !== "") return { suffix: Number(m[2]) };
  const offset = Number(m[1]);
  return m[2] === "" ? { offset } : { offset, length: Number(m[2]) - offset + 1 };
}
