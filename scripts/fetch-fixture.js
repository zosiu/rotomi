#!/usr/bin/env node
// Fetch a TrainingCourt replay log and save it as a fixture.
// Usage: node scripts/fetch-fixture.js <trainingcourt-url> [output-filename]
// Example: node scripts/fetch-fixture.js https://trainingcourt.app/ptcg/logs/ff16c833-... match_ff16c833.txt
//
// Compile the Elm first:
//   elm make src/FetchFixture.elm --output=scripts/fetch-fixture-elm.js

const https = require("https");
const fs = require("fs");
const path = require("path");
const { Elm } = require("./fetch-fixture-elm.js");

const TC_SUPABASE_URL = "https://yuruvpbgsukqiaeduaay.supabase.co";
const TC_ANON_KEY =
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inl1cnV2cGJnc3VrcWlhZWR1YWF5Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjM2NDA2MDcsImV4cCI6MjAzOTIxNjYwN30.GtRRwMpiMMmbcpUci9xXqthWhgL5daKvsUZUaRgFPkI";

const rawUrl = process.argv[2];
if (!rawUrl) {
  console.error("Usage: node scripts/fetch-fixture.js <trainingcourt-url> [output-filename]");
  process.exit(1);
}

const m = rawUrl.match(/trainingcourt\.app\/ptcg\/logs\/([0-9a-f-]{36})/);
if (!m) {
  console.error("Not a trainingcourt log URL:", rawUrl);
  process.exit(1);
}
const uuid = m[1];

const outputName = process.argv[3] || `match_${uuid.slice(0, 8)}.txt`;
const outputPath = path.resolve(__dirname, "../replay_fixtures", outputName);

function fetch(url) {
  return new Promise((resolve, reject) => {
    const urlObj = new URL(url);
    const options = {
      hostname: urlObj.hostname,
      path: urlObj.pathname + urlObj.search,
      headers: {
        apikey: TC_ANON_KEY,
        Authorization: `Bearer ${TC_ANON_KEY}`,
      },
    };
    https.get(options, (res) => {
      let body = "";
      res.on("data", (chunk) => (body += chunk));
      res.on("end", () => resolve(body));
    }).on("error", reject);
  });
}

(async () => {
  const apiUrl = `${TC_SUPABASE_URL}/rest/v1/logs?select=log&id=eq.${uuid}`;
  console.log(`Fetching ${uuid}…`);

  let rows;
  try {
    rows = JSON.parse(await fetch(apiUrl));
  } catch (e) {
    console.error("Request failed:", e.message);
    process.exit(1);
  }

  const log = rows[0]?.log;
  if (!log) {
    console.error("No log found for UUID:", uuid);
    process.exit(1);
  }

  const app = Elm.FetchFixture.init({ flags: { url: rawUrl, log } });

  app.ports.done.subscribe(({ log: logText, ok, summary }) => {
    console.log(summary);

    fs.writeFileSync(outputPath, logText);
    console.log("\nSaved:", outputPath);

    process.exit(ok ? 0 : 1);
  });
})();
