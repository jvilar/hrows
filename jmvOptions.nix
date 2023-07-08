{ mkDerivation, base, containers, fetchgit, lib, mtl, text }:
mkDerivation {
  pname = "jmvOptions";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/jvilar/jmvOptions.git";
    sha256 = "0gwjlah00mp24pg53r2vflffgz12kv4841k3yk7jjr8ka8b68x74";
    rev = "5ddff6621f5f76304340c8431a01b0535c42e313";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base containers mtl text ];
  description = "A helper for writing the list of options descriptions for System.Console.GetOpt";
  license = "unknown";
}
