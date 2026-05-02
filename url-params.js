const REPLAY_URL_PARAM = "replay_url";
const SECTION_PARAM = "section";
const GROUP_PARAM = "group";
const FLIP_PARAM = "flip";

function readReplayUrl(search) {
  return new URLSearchParams(search).get(REPLAY_URL_PARAM) ?? "";
}

function readSectionIndex(search) {
  const val = new URLSearchParams(search).get(SECTION_PARAM);
  const n = parseInt(val, 10);
  return isNaN(n) || n < 0 ? 0 : n;
}

function readGroupIndex(search) {
  const val = new URLSearchParams(search).get(GROUP_PARAM);
  const n = parseInt(val, 10);
  return isNaN(n) || n < 0 ? 0 : n;
}

// Default is false (don't flip opponent cards). Only stored in URL when explicitly set to true.
function readFlipOpponent(search) {
  const val = new URLSearchParams(search).get(FLIP_PARAM);
  return val === "1";
}

function buildShareUrl(replayUrl, sectionIndex, groupIndex, flipOpponent) {
  if (!replayUrl) return "";
  const params = new URLSearchParams();
  params.set(REPLAY_URL_PARAM, replayUrl);
  if (sectionIndex > 0) params.set(SECTION_PARAM, sectionIndex);
  if (groupIndex > 0) params.set(GROUP_PARAM, groupIndex);
  if (flipOpponent === true) params.set(FLIP_PARAM, "1");
  return "?" + params.toString();
}

if (typeof module !== "undefined")
  module.exports = { readReplayUrl, readSectionIndex, readGroupIndex, readFlipOpponent, buildShareUrl };
