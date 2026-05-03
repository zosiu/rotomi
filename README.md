# Rotomi

An Elm 0.19.1 web application.

## Prerequisites

- [Elm 0.19.1](https://guide.elm-lang.org/install/elm.html)
- Node.js ≥ 18 (for dev tooling)

## Getting started

```sh
npm install
```

## Development

```sh
npm run dev
```

Starts a live-reloading dev server and opens the app in your browser. The page
reloads automatically whenever you save an Elm file.

## Tests

```sh
npm test           # run once
npm run test:watch # re-run on every file change
```

## Fixture validation

Replay fixture files live in `replay_fixtures/`. To verify they all parse
correctly against the parser logic:

```sh
npm run validate-fixtures
```

Accepts an optional directory argument:

```sh
node scripts/validate-fixtures.js path/to/other/fixtures
```

Exits non-zero on failure, so it can be added to CI.

## Card count check

Verifies that the action-state implementation keeps the card count consistent
throughout a single replay. Both players must always have exactly 60 cards
across deck + prizes + hand + active + bench + attachments + discard + stadium.
The script steps through every action group and stops at the first violation,
printing a per-category breakdown and a link to open that exact moment in the app.

```sh
npm run card-count-check -- replay_fixtures/match_022.txt
```

Build once, then run repeatedly without recompiling:

```sh
npm run card-count-check:build
node scripts/card-count-check.js replay_fixtures/match_004.txt
node scripts/card-count-check.js replay_fixtures/match_009.txt
```

Pass `--url <replay-url>` to embed a specific hosted URL in the visual link
(defaults to `http://localhost:8000/<path>`, which works when `npm run dev` is
running):

```sh
npm run card-count-check -- replay_fixtures/brickfest.txt \
  --url https://gist.githubusercontent.com/.../brickfest.txt
```

Example output on failure:

```
✗  match_004.txt
  Failed at section 4, group 3
  Action: zosiu played Academy at Night.
  red (zosiu):  deck=45  prizes=6  hand=5  active=1  bench=0  attach=0  discard=3  stadium=1  = 61  ← WRONG (off by 1)
  blue (NoxFoxEX):  deck=30  prizes=6  hand=7  active=1  bench=5  attach=3  discard=8  = 60
  Visual: http://localhost:8000/?replay_url=http%3A%2F%2F...&section=4&group=3
```

## Build

```sh
npm run build
```

Produces an optimised `elm.js` alongside `index.html`.

## Deployment

The app is automatically built and deployed to **GitHub Pages** on every push
to `main` via the [deploy workflow](.github/workflows/deploy.yml).

Live URL: `https://zosiu.github.io/rotomi/`
