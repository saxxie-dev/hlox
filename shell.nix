with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    nodejs
    spago
    purescript
    nodePackages.purescript-language-server
    nodePackages.pscid

    (vscode-with-extensions.override {
      # When the extension is already available in the default extensions set.
      vscodeExtensions = with vscode-extensions; [
        bbenoist.nix
      ]
      # Concise version from the vscode market place when not available in the default set.
      ++ vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "ide-purescript";
          publisher = "nwolverson";
          version = "0.23.2";
          sha256 = "56f62302f50bbb83c7c3ec258d445f7f9f74f7961446fd6f8e5d6c85d48d16a3";
        }
        {
          name = "language-purescript";
          publisher = "nwolverson";
          version = "0.2.4";
          sha256 = "fc2cc16515dd9840418ae5deada644c4638a05e51a50f21ac807499ec08c8699";
        }
        {
          name = "dhall-lang";
          publisher = "dhall";
          version = "0.0.4";
          sha256 = "eef610dd3a368488ac9a8f48416456c0ab2ee255d9521d0eafcf560cb3264069";
        }
      ];
    })
  ];
}