{
  inputs = {
    classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    classyplate.flake = false;
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "github:nixos/nixpkgs/43e3b6af08f29c4447a6073e3d5b86a4f45dd420";
    systems.url = "github:nix-systems/default";
    references.url = "github:eswar2001/references/35912f3cc72b67fa63a8d59d634401b79796469e";
    references.flake = true;
    haskell-tools.url = "github:juspay/haskell-tools/577263b05e10abcbc09d0d34dd316fc75e4dc380";
    openapi3-code-generator.url = "github:Chaitanya-nair/Haskell-OpenAPI-Client-Code-Generator/90875689b812eac100dc00e1b6bb617c108f8578";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake { inputs = inputs // { inherit (inputs) nixpkgs; }; } {
      systems = import inputs.systems;
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {
        self',
        pkgs,
        lib,
        config,
        ...
      }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc8107;
          packages = {
            references.source = inputs.references;
            classyplate.source = inputs.classyplate;
            haskell-tools-parser.source = inputs.haskell-tools + /src/parser;
            haskell-tools-backend-ghc.source = inputs.haskell-tools + /src/backend-ghc;
            haskell-tools-ast.source = inputs.haskell-tools + /src/ast;
            haskell-tools-rewrite.source = inputs.haskell-tools + /src/rewrite;
            haskell-tools-prettyprint.source = inputs.haskell-tools + /src/prettyprint;
            haskell-tools-refactor.source = inputs.haskell-tools + /src/refactor;
            haskell-tools-builtin-refactorings.source = inputs.haskell-tools + /src/builtin-refactorings;
            haskell-tools-demo.source = inputs.haskell-tools + /demo;
            openapi3-code-generator.source = inputs.openapi3-code-generator + /openapi3-code-generator ;
          };
          settings = {
            haskell-tools-builtin-refactorings = {
              check = false;
            };
            haskell-tools-daemon = {
              check = false;
            };
            openapi3-code-generator = {
              check = false;
            };
          };
        };
        packages.default =  self'.packages.codegen;
      };
    };
}