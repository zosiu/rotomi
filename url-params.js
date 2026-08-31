const REPLAY_URL_PARAM = "replay_url";
const SECTION_PARAM = "section";
const GROUP_PARAM = "group";
const FLIP_PARAM = "flip";
const DEBUG_PARAM = "debug";

// TrainingCourt logs are stored in Supabase. The anon key is public by design
// (exposed in their open-source app and compiled client JS).
const TC_SUPABASE_URL = "https://yuruvpbgsukqiaeduaay.supabase.co";
const TC_ANON_KEY =
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inl1cnV2cGJnc3VrcWlhZWR1YWF5Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjM2NDA2MDcsImV4cCI6MjAzOTIxNjYwN30.GtRRwMpiMMmbcpUci9xXqthWhgL5daKvsUZUaRgFPkI";

function trainingCourtLogId(url) {
  const m = url.match(/trainingcourt\.app\/ptcg\/logs\/([0-9a-f-]{36})/);
  return m ? m[1] : null;
}

async function resolveReplayUrl(rawUrl) {
  const uuid = trainingCourtLogId(rawUrl);
  if (!uuid) return rawUrl;

  const apiUrl = `${TC_SUPABASE_URL}/rest/v1/logs?select=log&id=eq.${uuid}`;
  const resp = await fetch(apiUrl, {
    headers: { apikey: TC_ANON_KEY, Authorization: `Bearer ${TC_ANON_KEY}` },
  });
  const rows = await resp.json();
  const logText = rows[0]?.log;
  if (!logText) return rawUrl;

  const blob = new Blob([logText], { type: "text/plain" });
  return URL.createObjectURL(blob);
}

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

function readDebug(search) {
  const val = new URLSearchParams(search).get(DEBUG_PARAM);
  return val === "1" || val === "true";
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
  module.exports = { readReplayUrl, readSectionIndex, readGroupIndex, readFlipOpponent, readDebug, buildShareUrl, trainingCourtLogId, resolveReplayUrl };
