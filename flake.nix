{
  inputs = {
    common.url = "github:nammayatri/common";

    shared-kernel.url = "github:nammayatri/shared-kernel";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      debug = true;
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.shared-kernel.haskellFlakeProjectModules.output
          ];
          packages = { };
          settings = {
            generic-deriving.check = false;
          };
          autoWire = [ "packages" "checks" ];
        };
        process-compose = { };
        packages.default = self'.packages.namma-dsl;
        devShells.default = pkgs.mkShell {
          # cf. https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.pre-commit.devShell
            config.flake-root.devShell
          ];
        };
      };
    };
}
