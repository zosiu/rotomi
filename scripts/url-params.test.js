const { test } = require("node:test");
const assert = require("node:assert/strict");
const { readReplayUrl, buildShareUrl } = require("../url-params.js");

test("readReplayUrl: returns URL from replay_url param", () => {
  assert.equal(
    readReplayUrl("?replay_url=https%3A%2F%2Fexample.com%2Freplay.txt"),
    "https://example.com/replay.txt"
  );
});

test("readReplayUrl: returns empty string when param is absent", () => {
  assert.equal(readReplayUrl(""), "");
  assert.equal(readReplayUrl("?other=value"), "");
});

test("buildShareUrl: encodes URL into replay_url param", () => {
  assert.equal(
    buildShareUrl("https://example.com/replay.txt"),
    "?replay_url=https%3A%2F%2Fexample.com%2Freplay.txt"
  );
});

test("buildShareUrl: returns empty string for empty input", () => {
  assert.equal(buildShareUrl(""), "");
});

test("round-trip: read(build(url)) === url", () => {
  const url = "https://example.com/replay.txt";
  assert.equal(readReplayUrl(buildShareUrl(url)), url);
});
