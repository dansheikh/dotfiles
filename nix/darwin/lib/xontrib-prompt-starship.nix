# lib/xontrib-prompt-starship.nix
# Shared xontrib-prompt-starship derivation — imported by both NixOS and darwin.
# Avoids duplicating the derivation and ensures version/hash drift is impossible.
#
# Usage (NixOS — programs.xonsh.extraPackages):
#   extraPackages = ps: [ ((import ../../../lib/xontrib-prompt-starship.nix) pkgs ps) ];
#
# Usage (darwin — manual pythonEnv.pkgs):
#   xontrib = (import ../../../lib/xontrib-prompt-starship.nix) pkgs pkgs.unstable.xonsh.pythonEnv.pkgs;

pkgs: ps:
  ps.buildPythonPackage rec {
    pname   = "xontrib-prompt-starship";
    version = "0.3.8";
    format  = "setuptools";

    src = pkgs.fetchurl {
      url  = "https://files.pythonhosted.org/packages/ff/36/4bbbf8590dae84616c8081a54fbbc935c58a4c3e160c62980bed820e2144/xontrib_prompt_starship-0.3.8.tar.gz";
      hash = "sha256-KZxYooPFWwpbjGdOzwlu4QD+Brcu8D4RsShfEg0klMc=";
    };

    nativeBuildInputs = [ ps.setuptools ];

    # Use Python to strip the xonsh self-dependency — avoids shell quoting
    # issues with sed and bracket characters inside Nix string literals.
    postPatch = ''
      python3 -c "import re, pathlib; p = pathlib.Path('setup.py'); p.write_text(re.sub(r'install_requires=\[.*?\]', 'install_requires=[]', p.read_text()))"
    '';
    doCheck = false;

    meta = {
      description = "Starship cross-shell prompt for xonsh";
      homepage    = "https://github.com/anki-code/xontrib-prompt-starship";
      license     = pkgs.lib.licenses.mit;
    };
  }
