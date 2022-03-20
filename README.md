<h1 align="center">NixNG</h1>

<p align="center">
  <img src="./nix-ng.png" width="384">
</p>

NixNG is a, as of now, GNU/Linux distribution, which may be a considered a late
sibling to NixOS. It shares many of its designs and utilizes the Nix package
manager at its core. The defining features of NixNG are:
- A lighter and simpler design, due to the omission of systemd
- The possibility of multiple init system choices, like runit, OpenRC, or even
  systemd in the future
- A "minimal by default" package set. While `nixpkgs`, and by extension NixOS,
  takes the "full featured by default" approach
- Suitable for building containers, due to being lightweight
- Fully structured configuration for modules, that means no string `extraConfig`

## Current state
NixNG cannot boot on real hardware due to lacking a kernel and initramfs, but it
can "boot" as a container, be it LXC or OCI. Some modules are implemented but
there is still a lot to be done. If you want to get a feel for it, you can look
into `./examples` where you will find fully functional systems, mostly catered
to be containers.

### Testing out
We have built up several functional containers, which showcase the syntax and
functionality of NixNG. The sources are located in `/examples/`. Currently you
can build the top-level derivation which you theoretically could activate
(don't, your system will break) with:

`nix build .#examples.<systemName>.config.system.build.top-level`

Or also an OCI image which you can load into Docker, Podman, etc, with:

`nix build .#examples.<systemName>.config.system.build.ociImage.build`

Alternatively, you can stream the image without saving it into the Nix store
with:

`nix build .#examples.<systemName>.config.system.build.ociImage.stream && ./result | docker load`.

## Contributing
I've provided some basic docs in `./doc`, start with
[README.org](/doc/README.org). There are some open issue which need work,
you can also implement new services, those are always welcome. If you have any
gripes with how NixOS functions and would them be fixed in NixNG, please also
open an issue and we'll see what we can do.

## Requesting New Services

If you have a particular service or feature, which you would like to see in
NixNG, please open an issue and someone will implement your request.

## License
This project is mostly licensed under the MPL, v2, each file has a license
header or associated `.license` file clearly stating its license. I also give
explicit permission to the NixOS project to re-license any part of this
repository under the copyright of `Richard Brežák and NixNG contributors` under
the MIT license and to include it in [nixpkgs](https://github.com/NixOS/nixpkgs)
or other NixOS organization projects.

### Contributing
By contributing, you agree to the terms of the MPL, v2, license plus the
exceptions laid out in the section [License](#license). You can find some
documentation in [README.org](/doc/README.org).

## Relation to nixpkgs/NixOS

As already stated in the [License](#license) section, the NixOS organization can
freely relicense all code copyrighted under me or NixNG contributors in this
repository under MIT and include it in
[nixpkgs](https://github.com/NixOS/nixpkgs) and other NixOS organization
projects. Therefore I encourage upstreaming as much as possible from NixNG to
NixOS so that as many people as possible can benefit from my work. I currently
see NixNG as a container specific distribution and a playground for new ways of
structuring modules. NixNG is to NixOS as Alpine is to Debian, Alpine is seldom
used on bare metal and I doubt NixNG will catch on as a bare metal distro, but
it's perfect for containers.
