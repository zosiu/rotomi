const REPLAY_URL_PARAM = "replay_url";

function readReplayUrl(search) {
  return new URLSearchParams(search).get(REPLAY_URL_PARAM) ?? "";
}

function buildShareUrl(replayUrl) {
  return replayUrl
    ? "?" + REPLAY_URL_PARAM + "=" + encodeURIComponent(replayUrl)
    : "";
}

if (typeof module !== "undefined") module.exports = { readReplayUrl, buildShareUrl };
