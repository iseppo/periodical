#!/usr/bin/env node
// Kaitse: seppo.ai/kihlveod/ ja /tuleva/ avaldab ainult GitHub Actions.
// Kohalikus arvutis on aruanded vanad (renderdatakse CI-s), seega kohalik
// `wrangler deploy` kirjutaks live'i iidsete failidega üle (juhtus 2026-11-08).
// Käivitub wrangler.jsonc build.command'ina ja stage.mjs alguses.
// Uuendamiseks: gh workflow run main.yml
if (process.env.GITHUB_ACTIONS !== "true") {
  console.error(
    "STOPP: seda Workerit tohib avaldada ainult GitHub Actions (kohalikud aruanded on vanad).\n" +
    "Uuenda live'i nii: gh workflow run main.yml\n" +
    "Hädaolukorras tagasi eelmisele versioonile: cd cloudflare && npx wrangler rollback"
  );
  process.exit(1);
}
