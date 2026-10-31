#!/usr/bin/env bash
# A throwaway PostGIS server and a dev THERMOS running against it.
#
#   dev/test-env.sh up            start the database, then the app (shadow-cljs etc.)
#                                 in the foreground; stops the database on exit
#   dev/test-env.sh start | stop  just the database
#   dev/test-env.sh app           just the app (database must be running)
#   dev/test-env.sh status | psql | log
#   dev/test-env.sh make-admin EMAIL
#   dev/test-env.sh destroy       stop the database and delete its data
#
# The database lives in .test-pg/ (not target/, which the dev build cleans),
# listening on 127.0.0.2:5432 so as not to clash with any other postgres
# (the app always connects on port 5432).
#
# Once the app has migrated the database, an admin user admin@localhost is
# made; log in as them at http://localhost:8080/token/admin-token
#
# If ../thermos-models (and ../clj-lp) exist they are used instead of the git
# deps; set USE_GIT_DEPS=1 not to.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if [[ -z "${IN_THERMOS_TEST_ENV:-}" ]]; then
    exec nix develop "$ROOT" -c "$0" "$@"
fi

STATE="$ROOT/.test-pg"
DATA="$STATE/data"
SOCK="$STATE/sock"
LOG="$STATE/postgres.log"
HOST=127.0.0.2
DB=thermos
ADMIN=admin@localhost
ADMIN_TOKEN=admin-token

export PGHOST=$HOST PGPORT=5432 PGUSER=postgres

running() { pg_ctl -D "$DATA" status >/dev/null 2>&1; }

start_db() {
    if [[ ! -d "$DATA" ]]; then
        echo "Creating database cluster in $DATA"
        mkdir -p "$STATE" "$SOCK"
        initdb -D "$DATA" -U postgres --auth=trust >/dev/null
    fi
    if running; then
        echo "Database already running"
    else
        pg_ctl -D "$DATA" -l "$LOG" -w \
               -o "-c listen_addresses=$HOST -c port=5432 -c unix_socket_directories=$SOCK" \
               start >/dev/null
        echo "Database running on $HOST:5432 (log in $LOG)"
    fi
    if ! psql -d postgres -tAc "SELECT 1 FROM pg_database WHERE datname = '$DB'" | grep -q 1; then
        createdb "$DB"
    fi
    psql -q -d "$DB" -c "CREATE EXTENSION IF NOT EXISTS postgis" \
         -c "CREATE EXTENSION IF NOT EXISTS postgis_raster"
}

stop_db() {
    if running; then
        pg_ctl -D "$DATA" -m fast -w stop >/dev/null
        echo "Database stopped"
    else
        echo "Database not running"
    fi
}

make_admin() { # EMAIL [LOGIN-TOKEN]
    local token="${2:+'$2'}"
    psql -q -d "$DB" -c "INSERT INTO users (id, name, auth, reset_token)
                         VALUES ('$1', '$1', 'admin', ${token:-NULL})
                         ON CONFLICT (id) DO UPDATE
                         SET auth = 'admin',
                             reset_token = COALESCE(EXCLUDED.reset_token, users.reset_token)"
}

# wait for the app to be up (so it has migrated the database), then add
# the admin user
seed_admin() {
    for _ in $(seq 1 900); do
        if curl -sf -o /dev/null http://localhost:8080/login; then
            make_admin "$ADMIN" "$ADMIN_TOKEN"
            echo
            echo ">>> Admin user ready: http://localhost:8080/token/$ADMIN_TOKEN"
            echo
            return
        fi
        sleep 1
    done
    echo "Gave up waiting for the app to start" >&2
}

run_app() {
    running || { echo "Database is not running; use start or up" >&2; exit 1; }

    local sdeps='{}'
    if [[ -z "${USE_GIT_DEPS:-}" && -d ../thermos-models ]]; then
        sdeps='{:aliases {:test-env {:extra-deps {thermos/models {:local/root "../thermos-models"}'
        [[ -d ../clj-lp ]] && sdeps+=' cse/clj-lp {:local/root "../clj-lp"}'
        sdeps+='}}}}'
        echo "Using local ../thermos-models"
    else
        sdeps='{:aliases {:test-env {}}}'
    fi

    export PG_HOST=$HOST
    seed_admin &
    local seeder=$!
    trap 'kill $seeder 2>/dev/null || true' RETURN
    clojure -Sdeps "$sdeps" -M:dev:server:client:test-env
}

case "${1:-}" in
    start)   start_db ;;
    stop)    stop_db ;;
    app)     run_app ;;
    up)      start_db
             trap stop_db EXIT
             run_app ;;
    status)  if running; then echo "Database running on $HOST:5432"; else echo "Database not running"; fi ;;
    psql)    shift; psql -d "$DB" "$@" ;;
    log)     tail -f "$LOG" ;;
    make-admin)
             [[ -n "${2:-}" ]] || { echo "usage: $0 make-admin EMAIL" >&2; exit 1; }
             make_admin "$2" ;;
    destroy) stop_db; rm -rf "$STATE"; echo "Deleted $STATE" ;;
    *)       sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//'; exit 1 ;;
esac
