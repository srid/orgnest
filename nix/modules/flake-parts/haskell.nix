{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }: {
    # Our only Haskell project. You can have multiple projects, but this template
    # has only one.
    # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
    haskellProjects.default = {
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /src)
          (root + /orgnest.cabal)
          (root + /LICENSE)
          (root + /README.md)
        ];
      });

      # The base package set (this value is the default)
      # basePackages = pkgs.haskellPackages;

      # Packages to add on top of `basePackages`
      packages = {
        org-parser.source = inputs.org-mode-hs + /org-parser;
        slugify.source = "0.1.0.2";
      };

      # Add your package overrides here
      settings = {
        org-parser.jailbreak = true;
      };

      # Development shell configuration
      devShell = {
        hlsCheck.enable = false;
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    # Default package & app.
    packages.default = self'.packages.orgnest;
    apps.default = self'.apps.orgnest;
  };
}
