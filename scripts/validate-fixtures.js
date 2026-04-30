#!/usr/bin/env node
const fs = require("fs");
const path = require("path");
const { Elm } = require("./validate-fixtures-elm.js");

const dir = process.argv[2] ?? "replay_fixtures";

let files;
try {
  files = fs.readdirSync(dir).filter((f) => f.endsWith(".txt")).sort();
} catch {
  console.error(`Cannot read directory: ${dir}`);
  process.exit(1);
}

if (files.length === 0) {
  console.error(`No .txt files found in ${dir}`);
  process.exit(1);
}

const fileData = files.map((f) => ({
  name: f,
  content: fs.readFileSync(path.join(dir, f), "utf8"),
}));

// --- TCGdex set ID conversion (mirrors replaySetIdToTcgDex in Main.elm) ---

const KNOWN_SET_MAPPINGS = { mebsp: "mep", svbsp: "svp", "zsv10-5": "sv10.5b", "rsv10-5": "sv10.5w" };

function replaySetIdToTcgDex(code) {
  if (KNOWN_SET_MAPPINGS[code]) return KNOWN_SET_MAPPINGS[code];
  // Replace -5 fractional suffix with .5  (sv4-5 → sv4.5, sv10-5w → sv10.5w)
  const dotified = code.replace(/-5(\w*)$/, ".5$1");
  // Zero-pad single-digit sv/me set numbers  (sv4 → sv04, me1 → me01)
  return dotified.replace(/^(sv|me)(\d)(?!\d)/, (_, prefix, digit) => prefix + "0" + digit);
}

function cardApiUrl(id) {
  const parts = id.split("_");
  if (parts.length < 2) return null;
  const [setCode, localId] = parts;
  return `https://api.tcgdex.net/v2/en/sets/${replaySetIdToTcgDex(setCode)}/${localId}`;
}

// --- Card ID check (returns { id, ok, reason }) ---

async function checkCard(id) {
  const url = cardApiUrl(id);
  if (!url) return { id, ok: false, reason: "could not build API URL" };
  try {
    const res = await fetch(url);
    return { id, ok: res.ok, reason: res.ok ? null : `HTTP ${res.status}` };
  } catch {
    return { id, ok: false, reason: "network error" };
  }
}

async function checkAllCards(ids, batchSize = 20) {
  const results = [];
  for (let i = 0; i < ids.length; i += batchSize) {
    const batch = ids.slice(i, i + batchSize);
    results.push(...(await Promise.all(batch.map(checkCard))));
  }
  return results;
}

// --- Main ---

const app = Elm.ValidateFixtures.init({ flags: fileData });

app.ports.done.subscribe(async ({ output, allOk, cardIds }) => {
  process.stdout.write(output);

  if (cardIds.length === 0) {
    process.exit(allOk ? 0 : 1);
    return;
  }

  console.log(`\nChecking ${cardIds.length} unique card ID(s) against TCGdex…`);

  const results = await checkAllCards(cardIds);
  const bad = results.filter((r) => !r.ok);

  if (bad.length === 0) {
    console.log(`All ${cardIds.length} card ID(s) found in TCGdex ✓`);
  } else {
    console.log(`\n${bad.length} card ID(s) not found in TCGdex:`);
    bad.forEach(({ id, reason }) => console.log(`  ${id}  (${reason})`));
  }

  process.exit(allOk && bad.length === 0 ? 0 : 1);
});
