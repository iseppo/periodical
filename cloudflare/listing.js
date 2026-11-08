// Directory listing page. Copy of seppoai/cloudflare/src/listing.js — keep them in sync.
// entries: [{ name, dir: bool, size?: number, mtime?: Date }]

const esc = (s) => s.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
const fmtSize = (n) => (n < 1024 ? `${n} B` : n < 1048576 ? `${(n / 1024).toFixed(0)} KB` : `${(n / 1048576).toFixed(1)} MB`);

export function renderListing(dirPath, entries) {
  const sorted = [...entries].sort((a, b) => (a.dir === b.dir ? a.name.localeCompare(b.name, "et") : a.dir ? -1 : 1));
  const rows = sorted.map((e) => {
    const href = encodeURIComponent(e.name) + (e.dir ? "/" : "");
    const date = e.dir || !e.mtime ? "" : e.mtime.toISOString().slice(0, 10);
    const size = e.dir ? "—" : fmtSize(e.size ?? 0);
    return `<tr><td><a href="${href}">${esc(e.name)}${e.dir ? "/" : ""}</a></td><td class="n">${date}</td><td class="n">${size}</td></tr>`;
  }).join("\n");
  return `<!doctype html>
<html lang="et"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>${esc(dirPath)} · seppo.ai</title>
<style>
:root{color-scheme:light dark;--bg:#FBF8F2;--fg:#1A130D;--mute:#6E655B;--line:#E4D8C6;--link:#D8521A}
@media (prefers-color-scheme:dark){:root{--bg:#15110E;--fg:#F4EEE6;--mute:#9A8E82;--line:#34291F;--link:#FF6A2C}}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 "JetBrains Mono",ui-monospace,monospace}
main{max-width:900px;margin:0 auto;padding:32px 16px}
h1{font-size:18px;margin:0 0 20px}h1 a{color:var(--mute);text-decoration:none}
table{width:100%;border-collapse:collapse}td{padding:6px 8px;border-bottom:1px solid var(--line);overflow-wrap:anywhere}
td.n{color:var(--mute);white-space:nowrap;text-align:right;width:1%}a{color:var(--link)}
</style></head><body><main>
<h1><a href="/">&gt; seppo.ai</a> ${esc(dirPath)}</h1>
<table>
<tr><td><a href="../">../</a></td><td class="n"></td><td class="n"></td></tr>
${rows}
</table></main></body></html>
`;
}
