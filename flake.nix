{
  description = "monologiq's neovim flake";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0";

  outputs =
    { self, ... }@inputs:

    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      forEachSupportedSystem =
        f:
        inputs.nixpkgs.lib.genAttrs supportedSystems (
          system:
          f {
            inherit system;
            pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
          }
        );
    in
    {
      packages = forEachSupportedSystem (
        { pkgs, ... }:
        {
          default =
            let
              emacs = pkgs.emacs-pgtk;

              emacsWithPkgs = (pkgs.emacsPackagesFor emacs).emacsWithPackages (
                epkgs: with epkgs; [
                  ellama
                  markdown-mode
                  meow-tree-sitter
                  svg-lib
                  auto-dark
                  corfu
                  magit
                  meow
                  nix-ts-mode
                  treesit-grammars.with-all-grammars
                  vterm
                ]
              );

              extraDeps = with pkgs; [
                ripgrep
                git
                nixd
              ];

            in
            pkgs.stdenv.mkDerivation {
              name = "emacs-wrapped";

              buildInputs = [ pkgs.makeWrapper ];

              unpackPhase = "true"; # nothing to unpack
              installPhase = ''
                mkdir -p $out/bin

                # symlink the real Emacs binary
                ln -s ${emacsWithPkgs}/bin/emacs $out/bin/emacs-original

                # wrap it with extra PATH
                makeWrapper $out/bin/emacs-original $out/bin/emacs \
                  --prefix PATH : ${pkgs.lib.makeBinPath extraDeps}
              '';
            };
        }
      );
      devShells = forEachSupportedSystem (
        { pkgs, system }:
        {
          default = pkgs.mkShellNoCC {
            packages = with pkgs; [
              self.formatter.${system}
              lua-language-server
              nixd
            ];

            env = { };

            shellHook = "";
          };
        }
      );

      # Nix formatter

      # This applies the formatter that follows RFC 166, which defines a standard format:
      # https://github.com/NixOS/rfcs/pull/166

      # To format all Nix files:
      # git ls-files -z '*.nix' | xargs -0 -r nix fmt
      # To check formatting:
      # git ls-files -z '*.nix' | xargs -0 -r nix develop --command nixfmt --check
      formatter = forEachSupportedSystem ({ pkgs, ... }: pkgs.nixfmt-rfc-style);
    };
}
