{
  buildGoModule,
  fetchFromGitHub,
  lib,
  ...
}:

buildGoModule {
  pname = "generic-device-plugin";
  version = "20241207-unstable";

  src = fetchFromGitHub {
    owner = "squat";
    repo = "generic-device-plugin";
    rev = "36bfc606bba2064de6ede0ff2764cbb52edff70d";
    hash = "sha256-xztISJxFWKFWvanWY6WBx7KBIbJdUUkVX+YM0xLgBzk=";
  };

  vendorHash = "sha256-L0OYB6iI4z1o4FEmzpL0Qbc9uamyJZ89HWV77D10p3M=";

  # requires a docker instance running
  doCheck = false;

  meta = with lib; {
    description = "The generic-device-plugin enables allocating generic Linux devices, such as serial devices, the FUSE device, or video cameras, to Kubernetes Pods.";
    homepage = "https://github.com/squat/generic-device-plugin";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
    mainProgram = "generic-device-plugin";
  };
}
