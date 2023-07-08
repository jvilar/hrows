{ mkDerivation, base, base-orphans, bytestring, cereal, containers
, deepseq, fetchgit, hpack, lib, MonadRandom, profunctors, random
, semigroups, transformers
}:
mkDerivation {
  pname = "auto";
  version = "0.4.3.1";
  src = fetchgit {
    url = "https://github.com/jvilar/auto.git";
    sha256 = "1n1vx7sdnajhb2f5z0jdvbwijsplyb977va6dkrcvwbmizkqhdih";
    rev = "c44b4a5fe12c7818a98eb1fe6f65de202050a0d0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base-orphans bytestring cereal containers deepseq MonadRandom
    profunctors random semigroups transformers
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/mstksg/auto";
  description = "Denotative, locally stateful programming DSL & platform";
  license = lib.licenses.mit;
}
