let

sources = import ./nix/sources.nix;
nixos-22-05 = import sources."nixos-22.05" {};
nixos-22-11 = import sources."nixos-22.11" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    sockets-and-pipes = ./sockets-and-pipes;
};

depOverrides = new: old: {
    ascii = new.callPackage ./nix/ascii.nix {};
    ascii-case = new.callPackage ./nix/ascii-case.nix {};
    ascii-caseless = new.callPackage ./nix/ascii-caseless.nix {};
    ascii-char = new.callPackage ./nix/ascii-char.nix {};
    ascii-numbers = new.callPackage ./nix/ascii-numbers.nix {};
    ascii-predicates = new.callPackage ./nix/ascii-predicates.nix {};
    ascii-superset = new.callPackage ./nix/ascii-superset.nix {};
    ascii-th = new.callPackage ./nix/ascii-th.nix {};
    attoparsec-run = new.callPackage ./nix/attoparsec-run.nix {};
    # doctest = new.callPackage ./nix/doctest.nix {};
    # relude = new.callPackage ./nix/relude.nix {};
    unfork = new.callPackage ./nix/unfork.nix {};
};

ghc."8.10" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

ghc."9.0" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

in

symlinkJoin {
    name = "sockets-and-pipes";
    paths = concatMap (x: [x.sockets-and-pipes]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
