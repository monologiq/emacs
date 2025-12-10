{
  description = "monologiq's neovim flake";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0"; # Stable Nixpkgs (use 0.1 for unstable)

  outputs =
    { self, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux" # 64-bit Intel/AMD Linux
        "aarch64-linux" # 64-bit ARM Linux
        "aarch64-darwin" # 64-bit ARM macOS
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
                  vterm
                  treesit-grammars.with-all-grammars
                  nix-ts-mode
                  meow
		  meow-tree-sitter
                  magit
                  corfu
                  auto-dark
                ]
              );

              extraDeps = with pkgs; [
                nodejs_22
                ripgrep
                fd
                git
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

          # default =
          #   let
          #     neovimConfig = pkgs.neovimUtils.makeNeovimConfig {
          #       viAlias = true;
          #       vimAlias = true;
          #       withNodeJS = true;
          #       withRuby = false;

          #       plugins = with pkgs.vimPlugins; [
          #         lz-n
          #         nvim-treesitter.withAllGrammars
          #         nvim-cmp
          #       ];

          #     };

          #     languageServers = with pkgs; [
          #       lua-language-server
          #       nixd
          #     ];

          #     additionalTools = with pkgs; [ nodejs ];

          #     initLua = ''
          #       local user_config = vim.fn.expand('$HOME/.config/nvim/init.lua')
          #       if vim.fn.filereadable(user_config) == 1 then
          #         dofile(user_config)
          #       else
          #         vim.opt.number = true
          #         vim.opt.mouse = 'a'
          #         vim.opt.termguicolors = true
          #         print("No user config found at ~/.config/nvim/init.lua")
          #         print("Plugins are available but not configured")
          #       end
          #     '';

          #     customNeovim = pkgs.symlinkJoin {
          #       name = "neovim-with-lsps";
          #       paths = [
          #         (pkgs.wrapNeovimUnstable pkgs.neovim-unwrapped (
          #           neovimConfig
          #           // {
          #             luaRcContent = initLua;
          #             #wrapperArgs =
          #             #  pkgs.lib.escapeShellArgs neovimConfig.wrapperArgs
          #             #  + " "
          #             #  + ''--add-flags -u --add-flags ${pkgs.writeText "init.lua" initLua}'';
          #             wrapRc = false;
          #           }
          #         ))
          #       ];
          #       buildInputs = [ pkgs.makeWrapper ];
          #       postBuild = ''
          #         wrapProgram $out/bin/nvim \
          #           --prefix PATH : ${pkgs.lib.makeBinPath (languageServers ++ additionalTools)}
          #       '';
          #     };
          #   in
          #   customNeovim;
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
