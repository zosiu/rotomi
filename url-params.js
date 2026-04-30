const REPLAY_URL_PARAM = "replay_url";
const SECTION_PARAM = "section";

function readReplayUrl(search) {
  return new URLSearchParams(search).get(REPLAY_URL_PARAM) ?? "";
}

function readSectionIndex(search) {
  const val = new URLSearchParams(search).get(SECTION_PARAM);
  const n = parseInt(val, 10);
  return isNaN(n) || n < 0 ? 0 : n;
}

function buildShareUrl(replayUrl, sectionIndex) {
  if (!replayUrl) return "";
  const params = new URLSearchParams();
  params.set(REPLAY_URL_PARAM, replayUrl);
  if (sectionIndex > 0) params.set(SECTION_PARAM, sectionIndex);
  return "?" + params.toString();
}

if (typeof module !== "undefined")
  module.exports = { readReplayUrl, readSectionIndex, buildShareUrl };
