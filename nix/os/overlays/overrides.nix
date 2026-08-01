{ inputs }: final: prev: {
  # Per-package overrides go here.
  # Example:
  #
  #   direnv = prev.unstable.direnv.overrideAttrs (old: {
  #     doCheck = false;
  #   });

  # ── Desktop shell ───────────────────────────────────────────────────────
  noctalia = inputs.noctalia.packages.${prev.stdenv.hostPlatform.system}.default;
  # noctalia = (inputs.noctalia.packages.${prev.stdenv.hostPlatform.system}.default.overrideAttrs (finalAttrs: previousAttrs: {
  #   mesonFlags = (previousAttrs.mesonFlags or [ ]) ++ [ "-Dtests=disabled" ];
  # }));
}
