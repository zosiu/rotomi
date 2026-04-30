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

// --- Name comparison ---

// Two known benign differences that should not be flagged as errors:
//
//  1. Energy cards: replay text has "Basic Grass Energy" but the API returns
//     just "Grass Energy" (no "Basic" prefix).
//
//  2. Some cards have flavor text in parentheses in the API name, e.g.
//     "Boss's Orders (Giovanni)", while the parsed name is "Boss's Orders".
//
function namesMatch(apiName, parsedName) {
  const norm = (s) => s.trim().toLowerCase();

  // Direct match
  if (norm(apiName) === norm(parsedName)) return true;

  // Strip "Basic " prefix from parsed name (Energy cards)
  const parsedWithoutBasic = parsedName.replace(/^Basic\s+/i, "");
  if (norm(apiName) === norm(parsedWithoutBasic)) return true;

  // Strip trailing " (…)" flavor text from API name
  const apiWithoutFlavor = apiName.replace(/\s*\([^)]*\)\s*$/, "");
  if (norm(apiWithoutFlavor) === norm(parsedName)) return true;

  // Both adjustments at once
  if (norm(apiWithoutFlavor) === norm(parsedWithoutBasic)) return true;

  return false;
}

// --- Card check: verifies the card exists and the parsed name matches the API name ---
// Returns { id, parsedName, ok, reason }

async function checkCard({ id, name: parsedName }) {
  const url = cardApiUrl(id);
  if (!url) return { id, parsedName, ok: false, reason: "could not build API URL" };
  try {
    const res = await fetch(url);
    if (!res.ok) return { id, parsedName, ok: false, reason: `HTTP ${res.status}` };
    const body = await res.json();
    const apiName = typeof body.name === "string" ? body.name : null;
    if (apiName === null) {
      return { id, parsedName, ok: false, reason: "API response missing name field" };
    }
    if (!namesMatch(apiName, parsedName)) {
      return {
        id,
        parsedName,
        ok: false,
        reason: `name mismatch: parsed "${parsedName}" vs API "${apiName}"`,
      };
    }
    return { id, parsedName, ok: true, reason: null };
  } catch {
    return { id, parsedName, ok: false, reason: "network error" };
  }
}

async function checkAllCards(refs, batchSize = 20) {
  const results = [];
  for (let i = 0; i < refs.length; i += batchSize) {
    const batch = refs.slice(i, i + batchSize);
    results.push(...(await Promise.all(batch.map(checkCard))));
  }
  return results;
}

// --- Main ---

const app = Elm.ValidateFixtures.init({ flags: fileData });

app.ports.done.subscribe(async ({ output, allOk, cardRefs }) => {
  process.stdout.write(output);

  if (cardRefs.length === 0) {
    process.exit(allOk ? 0 : 1);
    return;
  }

  console.log(`\nChecking ${cardRefs.length} unique card ID(s) against TCGdex…`);

  const results = await checkAllCards(cardRefs);
  const bad = results.filter((r) => !r.ok);

  if (bad.length === 0) {
    console.log(`All ${cardRefs.length} card ID(s) verified in TCGdex ✓`);
  } else {
    console.log(`\n${bad.length} card ID(s) had issues:`);
    bad.forEach(({ id, reason }) => console.log(`  ${id}  (${reason})`));
  }

  process.exit(allOk && bad.length === 0 ? 0 : 1);
});
