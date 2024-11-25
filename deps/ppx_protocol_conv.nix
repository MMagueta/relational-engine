{ lib, ocamlPackages, fetchFromGitHub, defaultVersion ? "5.2.1" }:
ocamlPackages.buildDunePackage {
  duneVersion = "3";
  
  pname = "ppx_protocol_conv";
  version = defaultVersion;

  minimalOCamlVersion = "4.07";

  src = fetchFromGitHub {
    owner = "andersfugmann";
    repo = "ppx_protocol_conv";
    rev = "master";
    sha256 = "jJJ6FEn+AkMknqun0GkXh5DSXsJnFbdE7U8BQ7Dd/Fw=";
  };

  buildInputs = [];
  
  propagatedBuildInputs = [];

  doCheck = true;

  meta = {
    license = lib.licenses.bsd3;
    homepage = "https://github.com/andersfugmann/ppx_protocol_conv";
  };
}
