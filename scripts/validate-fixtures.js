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

const app = Elm.ValidateFixtures.init({ flags: fileData });
app.ports.done.subscribe(({ output, allOk }) => {
  process.stdout.write(output);
  process.exit(allOk ? 0 : 1);
});
