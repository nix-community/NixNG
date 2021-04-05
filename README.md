# NixNG 

![NixNG logo](./nix-ng.svg)

NixNG is a, as of now, GNU/Linux distribution, which may be a considered a late sibling to NixOS. It shares many of its
designs and utilizes the Nix package manager at its core. The defining features of NixNG are:
- a lighter and simpler design, due to the omission of SystemD
- the possibility of multiple init system choices, like runit, OpenRC and even SystemD in the future
- a "minimal by default" package set, while `nixpkgs` and by extension NixOS takes the "full featured by default" approach
- suitable for building containers, due to being lightweight
- fully structured configuration for modules, no string `extraConfig`

## Current state
NixNG cannot boot on real hardware due to missing it's own kernel and initramfs, but it can "boot" as a container, be it
lxc, or docker. Some modules are implemented but there a lot to do. If you want to get a feel for it, you can look into
`./examples` where you'll find fully working systems, mostly catered to be containers.

## Contributing
Look at the code, make changes, open a PR. There are no style guidelines as of now, but try to write readable and well
formatted Nix code, without tabs or trailing whitespace. If you need help with something please feel free to open an
issue or contact me on Discord `Magic_RB#6785`.

### I would, but I don't want to create an account on your Gitea
Don't worry, you can login using GitHub or GitLab and your password never touches my server, login is handled by GH or
GL, my server just gets told that it's you.

## License
This project is licensed under the GNU GPLv3, as per [COPYING](COPYING). I also give explicit permission to the NixOS
project to re-license any part of this repository (that is covered by [COPYING](COPYING) and doesn't have it's own
licensing) under the MIT license and to include it in [nixpkgs](https://github.com/NixOS/nixpkgs).

I also state that using this project as a flake or `import`ing after using some version `fetchurl` to download it,
doesn't constitute as linking and therefore, you are not required to license your project under the GPLv3, though your
project must be licensed under a GPL-compatible license as per the [GNU GPL compatible license
list](https://www.gnu.org/licenses/license-list.html).

### Contributing
By contributing, you agree to the terms of the GNU GPLv3 license + the exceptions laid out in the section
[License](#license). You also give up all rights to the code contributed, in exchange for a legally bounding promise
that this repository (excluding any separately licensed parts) will always be licensed under a GPL-compatible license as
per the [GNU GPL compatible license list](https://www.gnu.org/licenses/license-list.html).


