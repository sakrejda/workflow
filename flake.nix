{
  description = "Personal R package: workflow";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.rPackages.buildRPackage {
      name = "workflow";
      src = fetchGit {
        name = "workflow-source";
        url = "git+ssh://git@fawkes.io:2222/data/git/git/workflow";
        ref = "main";
        rev = "a789d8b818f8e8e22369b74fd77cbb0905b9ad02";
#        sha256 = "sha256-ExPwNbzrL4EcyzUiFbvmDZuzK+gTd4DnmaG3A3mxYKA=";
      };
      propagatedBuildInputs = with nixpkgs.legacyPackages.x86_64-linux.rPackages; [
        R6
        tibble
        rlang
        purrr
        remotes
        fs
        stringr
        curl
        rmarkdown
        rappdirs
        qs
        uuid
      ];
    };
  };
}

