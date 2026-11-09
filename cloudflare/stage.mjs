#!/usr/bin/env node
// Stages the published reports into cloudflare/public/{kihlveod,tuleva}/ for `wrangler deploy`.
// Video goes to cloudflare/media/ instead (uploaded to R2: static assets can't serve byte ranges).
// Run from the repo root after run_turuylevaade.R.
//
// Aruanded on rühmad: kui rühmast puudub mõni fail (nt render nurjus), võtame
// KOGU rühma eelmisest edukast avaldusest (cloudflare/last-good/, CI cache),
// et töötav aruanne ei kaoks saidilt ega jääks poolikuks.
import "./ci-only.mjs"; // ainult GitHub Actions; kohalikud aruanded on vanad
import fs from "node:fs";
import path from "node:path";

const OUT = "cloudflare/public";
const MEDIA_OUT = "cloudflare/media";
const LAST_GOOD = "cloudflare/last-good";
const MEDIA = /\.(mp4|webm|mov)$/i;

// dir: avalik kaust; files: rühma failid (kaustad lõpevad "/").
const GROUPS = [
  { name: "kihlvedu", dir: "kihlveod", files: ["tuleva.html", "tuleva_files/", "tuleva-style.css"] },
  { name: "kihlveo pildid", dir: "kihlveod", files: ["aastane_tulu_tuleva_lhv.png", "kihlvedu.png"] },
  { name: "kihlveo video", dir: "kihlveod", files: ["aastane_tulu_animeeritud.mp4"] },
  { name: "kihlveo staatilised", dir: "kihlveod", files: ["ekspress_screenshot.png", "fb_screenshot.png"] },
  {
    name: "turuülevaade", dir: "tuleva",
    files: ["turuylevaade.html", "turuylevaade_files/", "ojs-lib/", "tuleva-style.css", "turuylevaade-style.css",
      "koguturg_indeksfondid_osakaal.csv", "koguturg_koguinfo.csv"],
  },
  {
    name: "III sammas", dir: "tuleva",
    files: ["III_sammas.html", "iii_sammas_koguturg.csv", "iii_sammas_ajalugu.csv",
      "iii_sammas_investorid_ajalugu.csv", "iii_sammas_top_fondid.csv"],
  },
];

const src = (f) => f.replace(/\/$/, "");
const destOf = (dir, f) => path.join(MEDIA.test(f) ? MEDIA_OUT : OUT, dir, src(f));
const copy = (from, to) => {
  fs.mkdirSync(path.dirname(to), { recursive: true });
  fs.cpSync(from, to, { recursive: true, filter: (p) => !path.basename(p).startsWith(".") });
};

fs.rmSync(OUT, { recursive: true, force: true });
fs.rmSync(MEDIA_OUT, { recursive: true, force: true });

let fallbacks = 0;
for (const g of GROUPS) {
  const missing = g.files.filter((f) => !fs.existsSync(src(f)));
  if (missing.length === 0) {
    for (const f of g.files) copy(src(f), destOf(g.dir, f));
    continue;
  }
  // Video on R2-s: kui seda pole, jääb sinna eelmine objekt alles.
  if (g.files.every((f) => MEDIA.test(f))) {
    console.log(`::warning::${g.name}: ${missing.join(", ")} puudub — R2-s jääb eelmine versioon`);
    continue;
  }
  const lg = g.files.map((f) => path.join(LAST_GOOD, g.dir, src(f)));
  if (lg.every((p) => fs.existsSync(p))) {
    console.log(`::warning::${g.name}: puudub ${missing.join(", ")} — avaldan eelmise eduka versiooni`);
    g.files.forEach((f, i) => copy(lg[i], destOf(g.dir, f)));
    fallbacks++;
  } else {
    // Varukoopiat pole (nt esimene jooks): avaldame olemasoleva, nagu varem
    console.log(`::warning::${g.name}: puudub ${missing.join(", ")} ja varukoopiat pole — avaldan olemasoleva`);
    for (const f of g.files) if (fs.existsSync(src(f))) copy(src(f), destOf(g.dir, f));
  }
}

// seppo.ai/tuleva/ avaleht on turuülevaade (kausta sisu enam ei kuvata).
// Ressursid on suhtelised ja samas kaustas, seega koopia töötab otse.
const tuleva = path.join(OUT, "tuleva");
if (fs.existsSync(path.join(tuleva, "turuylevaade.html"))) {
  fs.copyFileSync(path.join(tuleva, "turuylevaade.html"), path.join(tuleva, "index.html"));
} else {
  console.log("::warning::turuylevaade.html puudub — seppo.ai/tuleva/ avaleht puudub");
}

// tuleva/ is working data, not meant for search results (kihlveod/tuleva.html stays indexable).
fs.writeFileSync(path.join(OUT, "_headers"), "/tuleva/*\n  X-Robots-Tag: noindex, nofollow\n");

for (const d of [OUT, MEDIA_OUT]) {
  if (!fs.existsSync(d)) continue;
  for (const f of fs.readdirSync(d, { recursive: true })) {
    if (fs.statSync(path.join(d, f)).isFile()) console.log(`${d}/${f}`);
  }
}
if (fallbacks > 0) console.log(`::warning::${fallbacks} aruanderühma tuli eelmisest avaldusest`);
