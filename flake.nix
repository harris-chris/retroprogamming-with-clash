{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    clash-compiler.url = "github:clash-lang/clash-compiler";
};
  outputs = {
      self
      , nixpkgs
      , clash-compiler
  }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {
    devShell.${system} = clash-compiler.devShells.${system}.ghc962;
  };
}
