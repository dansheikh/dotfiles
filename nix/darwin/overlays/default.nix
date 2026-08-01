{ inputs }:
final: prev: {
  unstable = import inputs.nixpkgs-unstable {
    system = final.system;
    config.allowUnfree = true;
  };

  direnv = prev.unstable.direnv.overrideAttrs (old: {
    doCheck = false;
  });

  sqlit-tui = prev.unstable.sqlit-tui.overrideAttrs (old: {
    dependencies = (old.dependencies or [ ]) ++ [
      final.python3Packages.psycopg2
      final.python3Packages.pymysql
    ];
  });
}

