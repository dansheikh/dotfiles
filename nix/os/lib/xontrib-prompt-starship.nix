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
  pname = "xontrib-prompt-starship";
  version = "0.3.8";
  format = "setuptools";

  src = pkgs.fetchPypi {
    pname = "xontrib_prompt_starship";
    inherit version;
    hash = "sha256-11fC+u5N9r7869v7A3xX4569i1x7a3xx4569iXONSH0=";
  };

  nativeBuildInputs = [ ps.setuptools ];

  # Use Python to strip the xonsh self-dependency — avoids shell quoting
  # issues with sed and bracket characters inside Nix string literals.
  postPatch = ''
    python3 -c "import re, pathlib; p = pathlib.Path('setup.py'); p.write_text(re.sub(r'install_requires=\[.*?\]', 'install_requires=[]', p.read_text()))"
  '';

  propagatedBuildInputs = [
    ps.xonsh
  ];

  doCheck = false;

  meta = {
    description = "Starship prompt helper extension for xonsh";
    homepage = "https://github.com/ahelal/xontrib-prompt-starship";
    license = pkgs.lib.licenses.mit;
  };
}

