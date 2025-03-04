{ config, lib, pkgs, pkgs-unstable, ... }:
with pkgs;
let
  inherit (lib) lists mkEnableOption;
  cfg = config.languages;
  cljPkgs = [ pkgs-unstable.clojure pkgs-unstable.clojure-lsp ];
  elixirPkgs = [ pkgs-unstable.elixir pkgs-unstable.elixir-ls ];
  erlangPkgs = [ pkgs-unstable.erlang pkgs-unstable.rebar3 ];
  goPkgs = [ pkgs-unstable.go pkgs-unstable.golangci-lint pkgs-unstable.gopls ];
  haskellPkgs =
    (pkgs-unstable.haskellPackages.ghcWithPackages
      (p: with p; [ cabal-fmt cabal-install haskell-language-server hlint ]));
  javascriptPkgs = [ pkgs-unstable.bun pkgs-unstable.deno pkgs-unstable.nodejs pkgs-unstable.corepack ];
  lispPkgs = [ pkgs-unstable.sbcl pkgs-unstable.abcl ];
  luajit = pkgs-unstable.luajit;
  luajitPkgs =
    [ (luajit.withPackages (p: with p; [ readline ])) pkgs-unstable.lua-language-server ];
  nixPkgs = [ pkgs-unstable.nil pkgs-unstable.nixpkgs-fmt ];
  py = pkgs-unstable.python3;
  pyPkgs = [
    (py.withPackages
      (p:
        with p; [
          hy
          hyrule
          ipython
          jupyterlab
          matplotlib
          numpy
          pandas
          pip
          pyarrow
          pyyaml
          radian
          xlrd
          XlsxWriter
        ]))
    pkgs-unstable.poetry
    pkgs-unstable.ruff
  ];
  rPkgs = pkgs-unstable.rWrapper.override {
    packages = with pkgs.rPackages; [ data_table devtools duckdb duckplyr languageserver noctua paws plumber quarto rmarkdown roxygen2 shiny tidymodels tidyverse usethis vroom ];
  };
  rb = pkgs-unstable.ruby;
  rbPkgs = [ (rb.withPackages (p: with p; [ ruby-lsp ])) ];
  sqlPkgs = [ pkgs-unstable.sqlfluff ];
  treeSitterPkgs = [ pkgs-unstable.tree-sitter ];
  zigPkgs = [ zig zls ];
in
{
  options.languages = {
    clojure.enable = mkEnableOption "clojure";
    efmLangserver.enable = mkEnableOption "efm-langserver";
    elixir.enable = mkEnableOption "elixir";
    erlang.enable = mkEnableOption "erlang";
    gleam.enable = mkEnableOption "gleam";
    go.enable = mkEnableOption "go";
    haskell.enable = mkEnableOption "haskell";
    javascript.enable = mkEnableOption "javascript";
    lisp.enable = mkEnableOption "lisp";
    luajit.enable = mkEnableOption "luajit";
    nix.enable = mkEnableOption "nix";
    python.enable = mkEnableOption "python";
    r.enable = mkEnableOption "r";
    ruby.enable = mkEnableOption "ruby";
    rust.enable = mkEnableOption "rust";
    sql.enable = mkEnableOption "sql";
    treeSitter.enable = mkEnableOption "tree-sitter";
    zig.enable = mkEnableOption "zig";
  };
  config = {
    home.packages = with pkgs;
      (lists.optionals (cfg.clojure.enable) cljPkgs) ++
      (lists.optional (cfg.efmLangserver.enable) pkgs-unstable.efm-langserver) ++
      (lists.optionals (cfg.elixir.enable) elixirPkgs) ++
      (lists.optionals (cfg.erlang.enable) erlangPkgs) ++
      (lists.optional (cfg.gleam.enable) pkgs-unstable.gleam) ++
      (lists.optionals (cfg.go.enable) goPkgs) ++
      (lists.optional (cfg.haskell.enable) haskellPkgs) ++
      (lists.optionals (cfg.javascript.enable) javascriptPkgs) ++
      (lists.optionals (cfg.lisp.enable) lispPkgs) ++
      (lists.optionals (cfg.luajit.enable) luajitPkgs) ++
      (lists.optionals (cfg.nix.enable) nixPkgs) ++
      (lists.optionals (cfg.python.enable) pyPkgs) ++
      (lists.optional (cfg.r.enable) rPkgs) ++
      (lists.optionals (cfg.ruby.enable) rbPkgs) ++
      (lists.optional (cfg.rust.enable) rustup) ++
      (lists.optionals (cfg.sql.enable) sqlPkgs) ++
      (lists.optionals (cfg.treeSitter.enable) treeSitterPkgs) ++
      (lists.optionals (cfg.zig.enable) zigPkgs);
  };
}

