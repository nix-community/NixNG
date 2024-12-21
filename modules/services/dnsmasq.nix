{
  pkgs,
  lib,
  nglib,
  config,
  ...
}:
let
  cfg = config.services.dnsmasq;
  cfgInit = config.init.services.dnsmasq;

  # BEGIN Copyright (c) 2003-2024 Eelco Dolstra and the Nixpkgs/NixOS contributors
  stateDir = "/var/lib/dnsmasq";

  # True values are just put as `name` instead of `name=true`, and false values
  # are turned to comments (false values are expected to be overrides e.g.
  # mkForce)
  formatKeyValue =
    name: value:
    if value == true then
      name
    else if value == false then
      "# setting `${name}` explicitly set to false"
    else
      lib.generators.mkKeyValueDefault { } "=" name value;

  settingsFormat = pkgs.formats.keyValue {
    mkKeyValue = formatKeyValue;
    listsAsDuplicateKeys = true;
  };
in
# END Copyright (c) 2003-2024 Eelco Dolstra and the Nixpkgs/NixOS contributors
{
  options.services.dnsmasq = {
    enable = lib.mkEnableOption "dnsmasq";

    package = lib.mkOption {
      description = "dnsmasq package to use";
      type = lib.types.package;
      default = pkgs.dnsmasq.override { dbusSupport = false; };
    };

    # BEGIN Copyright (c) 2003-2024 Eelco Dolstra and the Nixpkgs/NixOS contributors
    settings = lib.mkOption {
      type = lib.types.submodule { freeformType = settingsFormat.type; };

      default = { };
      description = ''
        Configuration of dnsmasq. Lists get added one value per line (empty
        lists and false values don't get added, though false values get
        turned to comments). Gets merged with

            {
              dhcp-leasefile = "${stateDir}/dnsmasq.leases";
              log-facility = "-";
            }
      '';
      example = lib.literalExpression ''
        {
          domain-needed = true;
          dhcp-range = [ "192.168.0.2,192.168.0.254" ];
        }
      '';
    };
    # END Copyright (c) 2003-2024 Eelco Dolstra and the Nixpkgs/NixOS contributors
  };

  config = lib.mkIf cfg.enable (
    let
      configFile = settingsFormat.generate "dnsmasq.conf" cfg.settings;
    in
    {
      # BEGIN Copyright (c) 2003-2024 Eelco Dolstra and the Nixpkgs/NixOS contributors
      services.dnsmasq.settings = {
        dhcp-leasefile = lib.mkDefault "${stateDir}/dnsmasq.leases";
        log-facility = "-";
      };
      # END Copyright (c) 2003-2024 Eelco Dolstra and the Nixpkgs/NixOS contributors

      init.services.dnsmasq = {
        enabled = true;
        user = lib.mkDefault "dnsmasq";
        group = lib.mkDefault "dnsmasq";

        # This is the default directory for dnsmasq's leasefile.
        ensureSomething.create."stateDir" = {
          type = "directory";
          mode = "755";
          owner = "${cfgInit.user}:${cfgInit.group}";
          persistent = true;
          dst = stateDir;
        };

        script = pkgs.writeShellScript "dnsmasq-run" ''
          ${lib.getExe cfg.package} --test
          ${lib.getExe cfg.package} \
            --keep-in-foreground \
            --pid-file=/run/dnsmasq.pid \
            --conf-file=${configFile}
        '';
      };

      environment.systemPackages = [ cfg.package ];

      users.users.${cfgInit.user} = nglib.mkDefaultRec {
        description = "dnsmasq";
        group = cfgInit.group;
        createHome = false;
        home = "/var/empty";
        useDefaultShell = true;
        uid = config.ids.uids.dnsmasq;
      };

      users.groups.${cfgInit.group} = nglib.mkDefaultRec { gid = config.ids.gids.dnsmasq; };
    }
  );
}
