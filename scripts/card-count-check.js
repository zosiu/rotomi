#!/usr/bin/env node
"use strict";

const fs = require("fs");
const path = require("path");
const { Elm } = require("./card-count-check-elm.js");

// --- CLI parsing ---

const args = process.argv.slice(2);

if (args.length === 0 || args[0] === "--help" || args[0] === "-h") {
  console.log(
    [
      "Usage: node scripts/card-count-check.js <fixture-file> [--url <replay-url>]",
      "",
      "  <fixture-file>   Path to a .txt replay fixture (e.g. replay_fixtures/match_022.txt)",
      "  --url <url>      Replay URL to embed in the visual verification link.",
      "                   Defaults to http://localhost:8000/<relative-path> so it works",
      "                   when elm-live is running in the project root.",
      "",
      "Examples:",
      "  node scripts/card-count-check.js replay_fixtures/match_022.txt",
      "  node scripts/card-count-check.js replay_fixtures/brickfest.txt --url https://gist.../raw/brickfest.txt",
    ].join("\n")
  );
  process.exit(0);
}

const filePath = args[0];

// Determine replay URL: explicit --url flag or auto-generate from file path
let replayUrl = "";
const urlFlagIdx = args.indexOf("--url");
if (urlFlagIdx !== -1 && args[urlFlagIdx + 1]) {
  replayUrl = args[urlFlagIdx + 1];
} else {
  // Relative to project root → served by elm-live at http://localhost:8000/
  const relativePath = path
    .relative(process.cwd(), path.resolve(filePath))
    .replace(/\\/g, "/");
  replayUrl = "http://localhost:8000/" + relativePath;
}

// --- Load fixture ---

let content;
try {
  content = fs.readFileSync(filePath, "utf8");
} catch {
  console.error("Cannot read file: " + filePath);
  process.exit(1);
}

const name = path.basename(filePath);

// --- Run Elm worker ---

const app = Elm.CardCountCheck.init({
  flags: { name, content, replayUrl },
});

app.ports.done.subscribe(({ output, ok }) => {
  process.stdout.write(output);
  process.exit(ok ? 0 : 1);
});
