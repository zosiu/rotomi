const REPLAY_URL_PARAM = "replay_url";
const SECTION_PARAM = "section";
const GROUP_PARAM = "group";

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

function buildShareUrl(replayUrl, sectionIndex, groupIndex) {
  if (!replayUrl) return "";
  const params = new URLSearchParams();
  params.set(REPLAY_URL_PARAM, replayUrl);
  if (sectionIndex > 0) params.set(SECTION_PARAM, sectionIndex);
  if (groupIndex > 0) params.set(GROUP_PARAM, groupIndex);
  return "?" + params.toString();
}

if (typeof module !== "undefined")
  module.exports = { readReplayUrl, readSectionIndex, readGroupIndex, buildShareUrl };
