const { test } = require("node:test");
const assert = require("node:assert/strict");
const { readReplayUrl, readSectionIndex, buildShareUrl } = require("../url-params.js");

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

test("readSectionIndex: returns section number from param", () => {
  assert.equal(readSectionIndex("?section=3"), 3);
});

test("readSectionIndex: returns 0 when param is absent", () => {
  assert.equal(readSectionIndex(""), 0);
  assert.equal(readSectionIndex("?replay_url=x"), 0);
});

test("readSectionIndex: returns 0 for negative values", () => {
  assert.equal(readSectionIndex("?section=-1"), 0);
});

test("readSectionIndex: returns 0 for non-numeric values", () => {
  assert.equal(readSectionIndex("?section=abc"), 0);
});

test("buildShareUrl: encodes URL into replay_url param", () => {
  assert.equal(
    buildShareUrl("https://example.com/replay.txt"),
    "?replay_url=https%3A%2F%2Fexample.com%2Freplay.txt"
  );
});

test("buildShareUrl: omits section param when index is 0", () => {
  assert.equal(
    buildShareUrl("https://example.com/replay.txt", 0),
    "?replay_url=https%3A%2F%2Fexample.com%2Freplay.txt"
  );
});

test("buildShareUrl: includes section param when index > 0", () => {
  assert.equal(
    buildShareUrl("https://example.com/replay.txt", 3),
    "?replay_url=https%3A%2F%2Fexample.com%2Freplay.txt&section=3"
  );
});

test("buildShareUrl: returns empty string for empty input", () => {
  assert.equal(buildShareUrl(""), "");
});

test("round-trip: read(build(url, index)) === url and index", () => {
  const url = "https://example.com/replay.txt";
  const index = 5;
  const search = buildShareUrl(url, index);
  assert.equal(readReplayUrl(search), url);
  assert.equal(readSectionIndex(search), index);
});
