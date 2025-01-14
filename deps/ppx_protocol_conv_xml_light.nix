{ lib, ppx_protocol_conv, ocamlPackages, fetchFromGitHub, defaultVersion ? "5.2.1" }:
ocamlPackages.buildDunePackage {
  duneVersion = "3";
  
  pname = "ppx_protocol_conv_xml_light";
  version = defaultVersion;

  minimalOCamlVersion = "4.08";

  src = fetchFromGitHub {
    owner = "andersfugmann";
    repo = "ppx_protocol_conv";
    rev = "master";
    sha256 = "gFYg0251NOPPkc01BiSU3NEj3JWGKg1fu6UgQ67BBZM=";
  };

  buildInputs = [ppx_protocol_conv ocamlPackages.xml-light ocamlPackages.ppx_sexp_conv ocamlPackages.sexplib ocamlPackages.alcotest ];
  
  propagatedBuildInputs = [ ocamlPackages.xml-light ocamlPackages.ppxlib ocamlPackages.ppx_sexp_conv ocamlPackages.sexplib ocamlPackages.alcotest ];

  doCheck = true;

  meta = {
    license = lib.licenses.bsd3;
    homepage = "https://github.com/andersfugmann/ppx_protocol_conv";
  };
}
