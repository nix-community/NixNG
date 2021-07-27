<h1 align="center">NixNG</h1>

<p align="center">
  <img src="./nix-ng.png" width="384">
</p>

NixNG is a, as of now, GNU/Linux distribution, which may be a considered a late sibling to NixOS. It shares many of its designs and utilizes the Nix package manager at its core. The defining features of NixNG are:
- A lighter and simpler design, due to the omission of systemd
- The possibility of multiple init system choices, like runit, OpenRC, or even systemd in the future
- A "minimal by default" package set. While `nixpkgs`, and by extension NixOS, takes the "full featured by default" approach
- Suitable for building containers, due to being lightweight
- Fully structured configuration for modules, that means no string `extraConfig`

## Current state
NixNG cannot boot on real hardware due to lacking a kernel and initramfs, but it can "boot" as a container, be it LXC or OCI. Some modules are implemented but there is still a lot to be done. If you want to get a feel for it, you can look into `./examples` where you will find fully functional systems, mostly catered to be containers.

### Testing out
We have built up several functional containers, which showcase the syntax and functionality of NixNG. The sources arelocacted in `/examples/`. Currently you can build the top-level derivation which you theoretically could activate (don't, your system will break) with:

`nix build .#<systemName>.config.system.build.top-level`

Or also an OCI image which you can load into Docker, Podman, etc, with:

`nix build .#<systemName>.config.system.build.ociImage.build`

Alternatively, you can stream the image without saving it into the Nix store with:

`nix build .#<systemName>.config.system.build.ociImage.stream && ./result | docker load`.

## Contributing
Look at the code, make changes, open a PR. There are no style guidelines as of now, but try to write readable and well formatted Nix code, without tabs or trailing whitespace. If you need help with something please feel free to open an issue or contact me on Discord `Magic_RB#6785`.

## License
This project is licensed under the GNU GPLv3, as per [COPYING](COPYING). I also give explicit permission to the NixOS project to re-license any part of this repository (that is covered by [COPYING](COPYING) and doesn't have it's own licensing) under the MIT license and to include it in [nixpkgs](https://github.com/NixOS/nixpkgs).

I also state that using this project as a flake or `import`ing it after using any version of `fetchurl` doesn't constitute as linking. Therefore, you are not required to license your project under the GPLv3, though your project must be licensed under a GPL-compatible license as per the [GNU GPL compatible license list](https://www.gnu.org/licenses/license-list.html).

### Contributing
By contributing, you agree to the terms of the GNU GPLv3 license plus the exceptions laid out in the section [License](#license). You also give up all rights to the code contributed, in exchange for a legally binding promise that this repository (excluding any separately licensed parts) will always be licensed under a GPL-compatible license as per the [GNU GPL compatible license list](https://www.gnu.org/licenses/license-list.html).

## Relation to nixpkgs/NixOS

As already stated in the [License](#license) section, the NixOS organization can freely relicense all code (owned by me) in this repository under MIT and include it in [nixpkgs](https://github.com/NixOS/nixpkgs). Therefore I encourage upstreaming as much as possible from NixNG to NixOS so that as many people as possible can benefit from my work. I currently see NixNG as a container specific distribution and a playground for new ways of structuring modules. NixNG is to NixOS as Alpine is to Debian, Alpine is seldom used on bare metal and I doubt NixNG will catch on as a bare metal distro, but it's perfect for containers.
