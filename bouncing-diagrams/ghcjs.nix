{ reflex-platform,  ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    diagrams-ghcjs = self.callPackage (reflex-platform.nixpkgs.fetchgit {
      url = git://github.com/ghcjs/diagrams-ghcjs;
      rev = "411acffe0b3cb61b7d28b9b9ef404809ea5ab64d";
      sha256 = "88312d5cfb020fe94814edb8050bb243df3190c7b8819ba8c7949678b6352570";
    }) {};
};
}
