# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

The THERMOS web application (district heating network planning tool): a Clojure backend + ClojureScript frontend sharing `.cljc` code. Much of the optimisation model lives in sibling repos pulled in as git deps (SSH URLs, so cloning needs GitHub SSH access):

- `thermos/models` (110-thermos-models): core MIP / heuristic network models
- `thermos/importer` (110-thermos-importer): GIS processing utilities
- `cse/clj-lp`: MIP solver driver

The model rules that turn a user's document into the simplified model input (heat losses, pipe diameters, pipe costs) live **here**, mostly in `thermos-backend.solver.interop` and `thermos-util.pipes`/`thermos-util.steam`.

## Build / run

Tools come from the flake devShell: `nix develop` gives clojure on JDK 21, `lessc`, node/npm, git, scip and postgres 17 with postgis. Packaging is tools.build (`build.clj`, the `:build` alias). The dev launcher is `build-src/build/dev.clj` (`-m build.dev`, from the `:dev` alias).

- Dev (starts nREPL, writes `.nrepl-port`, `mount/start`s the backend, watches LESS, compiling `resources/**/*.main.less` into `target/resources/` with `lessc`, and runs shadow-cljs `watch :editor` in the same JVM):
  `clojure -M:dev:server:client` (add `--emacs` for cider middleware). The nREPL includes shadow's middleware, so `(shadow.cljs.devtools.api/repl :editor)` in it gives a cljs REPL.
- Hacking on the sibling libraries checked out in `../`: add the `:local-deps` alias. The build tasks take it as `:aliases '[:local-deps]'`.
- Production jar into `target/thermos.jar`: `clojure -T:build uber`, or `./do-build`, which wraps this in `nix develop`. Other targets: `heat-uber`, `noder-uber`.
- The editor's JS libraries (React, Leaflet, leaflet-draw, jsts, rbush, react-virtualized) are npm packages in `package.json`, built by shadow-cljs (`shadow-cljs.edn`, build `:editor`, entry `thermos-frontend.editor/init`). `build.clj`'s `uber` runs `npm ci` first. The LESS `@import`s leaflet CSS from `node_modules`, and `build.vendor` copies the leaflet JS from there for the server pages. `cljts.core` (from thermos/importer) still requires `cljsjs.jsts`, so `shadow-cljs.edn` resolves that name to npm `jsts`.
- Needs a PostGIS server. In the devShell, `start-postgres` / `stop-postgres` / `reset-postgres` (wrapping `dev/test-env.sh`) run one in `.test-pg/` on 127.0.0.2, and the shell sets `PG_HOST` to match. Config defaults are in `resources/config.edn` (default `pg-host` is `10.233.2.2`). Any key can be overridden by an environment variable or system property, e.g. `PG_HOST` → `:pg-host`. Values from the environment arrive as strings, so code compares against `"true"`.

## Tests

Tests are plain `clojure.test`. Run them all with `clojure -X:server:test` (cognitect test-runner), or one namespace with `clojure -X:server:test :nses '[thermos-backend.solver.interop-test]'`.

The editor's cljs tests (`test/thermos_frontend/*_test.cljs`) run on node with `clojure -M:server:client:test -m shadow.cljs.devtools.cli compile test` (the `:test` build in `shadow-cljs.edn`). That build also runs the inline `{:test ...}` metadata in the shared cljc code, which the clj runner doesn't.

Some test namespaces under `test/thermos_ui/test/...` don't match their file paths, so check the `ns` form before you require one.

## Architecture

**Namespaces under `src/`:**
- `thermos-backend.*`: server. `core` is the entry point, and every long-lived component is a `mount` `defstate` (db `conn`, http-kit `server`, queue consumers, config). Routing uses bidi (`routes/`), server HTML uses hiccup (`pages/`), and DB access uses honeysql + clojure.jdbc (`db/`).
- `thermos-frontend.*`: ClojureScript for the network/supply editor (reagent + re-frame + Leaflet), the only cljs build in `shadow-cljs.edn`. Its state is re-frame's app-db, which is the network document itself (`thermos-frontend.db`); editor-only state lives in it under keywords outside `thermos-specs` (`::db/server` for save and run status, `::popover/popover`, `::toaster/toast`), which `document/keep-interesting` strips, so they're never saved. Only events change app-db:
  - Events are in `thermos-frontend.events` (mostly thin wrappers round `operations`/`document` functions, plus generic `:doc/edit` (apply a function), `:doc/edit-geometry` (the same, then rebuild the spatial index), `:doc/set` and `:doc/update` by path), with a few next to the code that needs them (`map`, `editor`, `editor-keys`). Effects are in `.fx` (server requests, `::fx/call` to call a function from an event), subscriptions in `.subs` (those needing only part of the document derive from `:doc/candidates` or `:view/state`).
  - Components read the document with `doc/value` (the `:doc/get` subscription by path) and change it with `doc/set-value!`, `doc/update!` or a dispatch; form inputs take `:path` instead of `:value-atom`. `doc/current` reads without subscribing, for imperative setup such as Leaflet's. Short-lived UI state (open menus, dialog forms, drawing tools) stays in local reagent atoms.
  - Subscriptions must be deref'd inside a reagent render or track: the map's Leaflet tile layers deref them inside their `reagent/track!`s, and React-Virtualized renderers that subscribe render a reagent component via `as-element`. Leaflet handlers that need the document dispatch events which read `db` (e.g. `::map/with-candidate-at`, synchronous so a click after a move sees it).
  - A global interceptor (`events/mark-unsaved-changes`, using `db/unsaved-change?`) sets needs-save; saving and loading bump `::db/server :revision` so their own changes don't count.
  - `editor-state` just initialises app-db and fetches map tiles. Beware a local called `document`: `js/document.foo` compiles to `document.foo`, which it hides.
- The other pages (projects, new-map wizard, LIDAR, admin) are hiccup rendered in `thermos-backend.pages.*`, with htmx (`resources/public/js/vendor/htmx.min.js`) for interactivity. The shell in `pages.common/page` includes htmx, `resources/public/js/pages.js` (modal dialogs via `hx-target="#modal"`, file drop zones, upload progress), and a `<dialog id="modal">`. Leaflet maps on these pages use `resources/public/js/map-box.js`. The build copies leaflet out of `node_modules` into `target/resources/public/js/vendor/` (`build.vendor`). Handlers return fragments with `routes.responses/fragment`, and in production every response needs `cache-control/no-store`.
- New map wizard: `thermos-backend.map-wizard` holds the pure step and validation logic. Its state lives in the ring session under `::wizards`, keyed by a wizard id in the URL, and is stored as the map's `:parameters` at the end, which `importer/process.clj` reads. Keep geojson in it string-keyed.
- Job notifications: `thermos-backend.notifications` listens for finished `:problems` and `:imports` jobs (`queue/on-finish`). It emails users who ticked `users.job_emails`, and sends Web Push to the browsers in `push_subscriptions`. `thermos-backend.web-push` does the RFC 8291 encryption and VAPID signing with the JDK alone. The VAPID keys live in the `vapid_keys` table and are generated on first start; regenerating them drops all subscriptions. On the browser side, `resources/public/js/push.js` (settings page, and the editor's offer for runs over 20 minutes) works with `resources/public/push-worker.js`. Notification links use `:base-url`.
- `thermos-specs.*` (cljc): clojure.spec definitions for the **network document**, the central data structure (candidates, paths and buildings, tariffs, supply, pipe costs, solution, view state). Keys are namespaced keywords such as `::document/candidates` and `::candidate/inclusion`.
- `thermos-util.*` (cljc): shared calculations (finance, pipes, steam, peak demand, piecewise linearisation).
- `thermos-pages.*` (cljc): small bits shared by the editor and server pages (`style`, `symbols`, `spinner`, restriction info).
- `thermos-cli.*`: command-line tools (heatmap, noder) that reuse the backend importer code without the web app.

**Data flow:** a project contains maps (imported GIS data: `candidates`, `buildings`, and `paths` tables in PostGIS). The editor loads a map's candidates into a document and the user edits it. The saved network is stored as **EDN text** in `networks.content`. A solve is enqueued on the `:problems` queue (`solver.core/queue-problem`). `thermos-backend.queue` is a Postgres-backed job queue (`FOR UPDATE SKIP LOCKED`) with per-queue capacity (`solver-count`, `importer-count`). The solver converts the document through `solver.interop` into model input, runs thermos-models/SCIP, then merges the solution back into the document. Map imports go through the `importer` queue (`importer/process.clj`), which estimates demand using the bundled regression/SVM models in `src/thermos_backend/importer/*.json`.

**Migrations:** `thermos-backend.db.migration` is a homegrown system, not ragtime. It runs automatically when the `conn` state starts:
- Files in `resources/migrations/*.sql` are split on lines starting `--;;`. Each chunk is recorded in the `migrations` table by `(file, row)`. Applied chunks must match their recorded text exactly, so **never edit or reorder existing migration chunks**. Add new chunks at the end of a file, or add a new numbered file.
- A chunk whose text starts with `!` names a Clojure function to call with the connection (e.g. `!thermos-backend.content-migrations.network-metadata/migrate`). These **content migrations** rewrite the stored EDN of every network (`content-migrations.core/modify-networks-with`). Use one when the document format changes in a way old saved networks need patching.

**User restrictions:** user auth levels (`:basic`, `:intermediate`, etc.) cap runtime, jobs per week, and GIS feature counts (see `config.edn`, `thermos-backend.restrictions`, and `restrictions.org`).

## Deployment

Production runs on "buzz" via nixops (`deployment/*.nix`, deployment `thermos-live`). `envrc.buzz` pins `nixos-21.11`, because production uses Postgres 13.7 from that channel. See `README.org` for deploy commands and the SQL recipe for moving a user's projects between databases.
