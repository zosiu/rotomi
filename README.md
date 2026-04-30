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

## Build

```sh
npm run build
```

Produces an optimised `elm.js` alongside `index.html`.

## Deployment

The app is automatically built and deployed to **GitHub Pages** on every push
to `main` via the [deploy workflow](.github/workflows/deploy.yml).

Live URL: `https://zosiu.github.io/rotomi/`
