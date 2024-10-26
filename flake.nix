{
  description = "org-file-fetcher.el";

  inputs = {
    nixpkgs.url = "github:nixOS/nixpkgs/release-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
  	flake-utils.lib.eachDefaultSystem (system:
			let
				name = "org-file-fetcher";
				src = ./.;
				pkgs = import nixpkgs {
					inherit system;
				};
			in with pkgs; { 
				devShells.default = mkShell {
					inherit name;
					buildInputs = with pkgs; [
						eask
					];
					shellHook = ''
						unset XDG_CONFIG_HOME
						eask install-deps --dev
						eask install-deps
						echo "Entered development environment"
					'';
				};
			}
		);

  
}
