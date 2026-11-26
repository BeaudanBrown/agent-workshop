# The abrec service: admin site on localhost behind nginx, and the public
# site served by nginx from the rendered release. TLS, DNS and restricting
# the admin site to a private network are the consumer's choice: merge them
# into services.nginx.virtualHosts.<adminDomain> and .<publicDomain>.
{ self }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.abrec;
  inherit (lib) mkOption types;
  libraryDir = "/var/lib/abrec";
  configFile = pkgs.writeText "abrec.json" (
    builtins.toJSON {
      inherit libraryDir;
      inherit (cfg) adminDomain port;
      publicUrl = "https://${cfg.publicDomain}";
    }
  );
  commonHeaders = ''
    add_header X-Content-Type-Options nosniff always;
    add_header Referrer-Policy no-referrer always;
    add_header X-Robots-Tag "noindex, nofollow" always;
  '';
in
{
  options.services.abrec = {
    enable = lib.mkEnableOption "abrec, a read-aloud recorder and private podcast publisher";
    package = mkOption {
      type = types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.abrec;
      defaultText = lib.literalExpression "agent-workshop.packages.\${system}.abrec";
      description = "The abrec package.";
    };
    adminDomain = mkOption {
      type = types.str;
      example = "abrec.example.com";
      description = ''
        Host name of the admin site. It has no login: whoever can reach it can
        change and delete the library, so restrict it to a private network.
      '';
    };
    publicDomain = mkOption {
      type = types.str;
      example = "audiobooks.example.com";
      description = "Host name of the public listener site and feeds, served over HTTPS.";
    };
    port = mkOption {
      type = types.port;
      default = 8790;
      description = "Port of the admin site on 127.0.0.1.";
    };
    maxUploadSize = mkOption {
      type = types.str;
      default = "4g";
      description = "Largest admin request nginx accepts, in nginx size syntax.";
    };
    libraryDir = mkOption {
      type = types.str;
      default = libraryDir;
      readOnly = true;
      description = "The library: masters, metadata and rendered releases. abrec makes no backups of it.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.abrec = {
      isSystemUser = true;
      group = "abrec";
      home = libraryDir;
    };
    users.groups.abrec = { };

    systemd.services.abrec = {
      description = "abrec admin site and publisher";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = "${lib.getExe cfg.package} serve ${configFile}";
        User = "abrec";
        Group = "abrec";
        StateDirectory = "abrec";
        # Others may traverse the library to reach releases/, but not list it.
        StateDirectoryMode = "0711";
        UMask = "0022";
        Restart = "on-failure";
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHostname = true;
        RestrictSUIDSGID = true;
        RestrictNamespaces = true;
        RestrictRealtime = true;
        LockPersonality = true;
        SystemCallArchitectures = "native";
        # curl fetches cover images by URL.
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        CapabilityBoundingSet = "";
      };
    };

    services.nginx = {
      enable = true;
      virtualHosts.${cfg.adminDomain} = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.port}";
          recommendedProxySettings = true;
          extraConfig = ''
            client_max_body_size ${cfg.maxUploadSize};
            proxy_request_buffering off;
            proxy_read_timeout 600s;
            proxy_send_timeout 600s;
          '';
        };
        extraConfig = commonHeaders + ''
          add_header Content-Security-Policy "default-src 'self'; img-src 'self'; media-src 'self'; style-src 'self'; script-src 'self'; object-src 'none'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'" always;
        '';
      };
      virtualHosts.${cfg.publicDomain} = {
        root = "${libraryDir}/site";
        locations."/".tryFiles = "$uri $uri/ =404";
        locations."~ /\\.".extraConfig = "deny all;";
        locations."~ \\.xml$".extraConfig = ''
          types { }
          default_type "application/rss+xml; charset=utf-8";
        '';
        extraConfig = commonHeaders + ''
          add_header Content-Security-Policy "default-src 'none'; img-src 'self'; media-src 'self'; style-src 'self'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'" always;
        '';
      };
    };
  };
}
