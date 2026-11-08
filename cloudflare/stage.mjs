#!/usr/bin/env node
// Stages the published reports into cloudflare/public/{kihlveod,tuleva}/ for `wrangler deploy`.
// Video goes to cloudflare/media/ instead (uploaded to R2: static assets can't serve byte ranges).
// Run from the repo root after run_turuylevaade.R.
import "./ci-only.mjs"; // ainult GitHub Actions; kohalikud aruanded on vanad
import fs from "node:fs";
import path from "node:path";
import { renderListing } from "./listing.js";

const OUT = "cloudflare/public";
const MEDIA_OUT = "cloudflare/media";
const MEDIA = /\.(mp4|webm|mov)$/i;

const FILES = {
  kihlveod: [
    "tuleva.html", "tuleva-style.css", "aastane_tulu_tuleva_lhv.png", "aastane_tulu_animeeritud.mp4",
    "kihlvedu.png", "ekspress_screenshot.png", "fb_screenshot.png", "tuleva_files/",
  ],
  tuleva: [
    "turuylevaade.html", "III_sammas.html", "koguturg_indeksfondid_osakaal.csv", "koguturg_koguinfo.csv",
    "iii_sammas_koguturg.csv", "iii_sammas_ajalugu.csv", "iii_sammas_investorid_ajalugu.csv",
    "iii_sammas_top_fondid.csv",
  ],
};

fs.rmSync(OUT, { recursive: true, force: true });
fs.rmSync(MEDIA_OUT, { recursive: true, force: true });

for (const [dir, files] of Object.entries(FILES)) {
  for (const f of files) {
    const src = f.replace(/\/$/, "");
    if (!fs.existsSync(src)) {
      console.log(`::warning::${src} puudub — jääb ${dir}/ alt välja`);
      continue;
    }
    const dest = path.join(MEDIA.test(src) ? MEDIA_OUT : OUT, dir, src);
    fs.mkdirSync(path.dirname(dest), { recursive: true });
    fs.cpSync(src, dest, { recursive: true, filter: (p) => !path.basename(p).startsWith(".") });
  }
}

// tuleva/ was a browsable folder on the old server: keep the listing.
const tuleva = path.join(OUT, "tuleva");
const entries = fs.readdirSync(tuleva).map((name) => {
  const s = fs.statSync(path.join(tuleva, name));
  return { name, dir: s.isDirectory(), size: s.size, mtime: s.mtime };
});
fs.writeFileSync(path.join(tuleva, "index.html"), renderListing("/tuleva/", entries));

// tuleva/ is working data, not meant for search results (kihlveod/tuleva.html stays indexable).
fs.writeFileSync(path.join(OUT, "_headers"), "/tuleva/*\n  X-Robots-Tag: noindex, nofollow\n");

for (const d of [OUT, MEDIA_OUT]) {
  if (!fs.existsSync(d)) continue;
  for (const f of fs.readdirSync(d, { recursive: true })) {
    if (fs.statSync(path.join(d, f)).isFile()) console.log(`${d}/${f}`);
  }
}
