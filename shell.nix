{ pkgs ? import <nixpkgs> {}, ... }:
let
  inherit (pkgs) stdenv;
in stdenv.mkDerivation {
  name = "myproject-devenv";
  buildInputs = with pkgs; [
    # postgresql
    postgresql_12
  ];
  shellHook = ''
    export PGDATA=$PWD/postgres_data
    export PGHOST=localhost
    export PGPORT=5432
    export LOG_PATH=$PWD/postgres.log
    export PGDATABASE=postgres

    if [ ! -d $PGDATA ]; then
      echo 'Initializing postgresql database...'
      initdb $PGDATA --auth=trust >/dev/null
    fi
    pg_ctl start -l $LOG_PATH -o "-c listen_addresses='$PGHOST' -c port=$PGPORT"
  '';
}
