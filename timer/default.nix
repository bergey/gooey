{ mkDerivation, base, bytestring, containers, file-embed
, ghcjs-base, ghcjs-dom, stdenv, text, transformers
}:
mkDerivation {
  pname = "timer";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers file-embed ghcjs-base ghcjs-dom text
    transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
