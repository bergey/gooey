{ reflex-platform,  ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    diagrams-reflex = self.callPackage (reflex-platform.nixpkgs.fetchgit {
      url = git://github.com/diagrams/diagrams-reflex;
      rev = "f1ba4f2f8767ffa239376daf9a5936420a932410";
      sha256 = "d988d9c4ad39f63b5edc73b854b994b8a54ecd4191c81e3a1b0466e789905adf";
    }) {};
};
}
