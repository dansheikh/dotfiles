{ inputs, ... }:

# Per-host entrypoint for danish-dell-precision7550 (Dell Precision 7550).
# Shared concerns live in modules/ — only machine-specific config lives here.
#
# GPU topology: Intel UHD Graphics (CometLake-H GT2, i915) + Nvidia Quadro T1000 Mobile (TU117, Turing) dGPU
# PRIME offload mode: Intel iGPU drives the display; Nvidia wakes on demand.
# PRIME offload prevents driver conflicts; boot.extraModprobeConfig below
# suppresses the residual "failed to probe lspcon" i915 error on port D.
#
# Bus IDs below are standard for the Precision 7550 Coffee Lake topology.
# Verify with `lspci | grep -E "VGA|3D|Display"` and adjust if needed:
#   Intel:  "xx:02.0" → PCI:xx:2:0
#   Nvidia: "xx:00.0" → PCI:xx:0:0

{
  imports = [
    ./hardware-configuration.nix
    # No nixos-hardware module exists for the 7550 — configured manually below.
  ];

  # ── Nvidia PRIME — Offload Mode ───────────────────────────────────────────
  hardware.nvidia = {
    # Modesetting required for PRIME and Wayland.
    modesetting.enable = true;

    # Open kernel modules — supported on Turing (Quadro T1000 Mobile / TU117)+.
    # Better Wayland behaviour; explicit sync support in drivers >= 555.
    open = true;

    # nvidiaSettings GUI — useful for verifying PRIME state.
    nvidiaSettings = true;

    # Power management — required for correct suspend/resume on hybrid laptops.
    # Prevents GPU state corruption on wake.
    powerManagement.enable = true;

    # Fine-grained power management — puts dGPU fully to sleep when idle.
    # Requires open = true and driver >= 520. Safe on Turing.
    powerManagement.finegrained = true;

    prime = {
      offload = {
        enable = true;
        # Provides the `nvidia-offload` wrapper script for explicit dGPU launch.
        enableOffloadCmd = true;
      };

      # Intel UHD Graphics (CometLake-H GT2) iGPU — confirmed via lspci.
      # lspci: "00:02.0 VGA compatible controller: Intel ... CometLake-H GT2" → PCI:0:2:0
      intelBusId = "PCI:0:2:0";

      # Nvidia Quadro T1000 Mobile (TU117GLM) dGPU — confirmed via lspci.
      # lspci: "01:00.0 VGA compatible controller: NVIDIA ... TU117GLM" → PCI:1:0:0
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  # ── i915 LSPCON probe suppression ────────────────────────────────────────
  # i915 probes the LSPCON DisplayPort-to-HDMI bridge on port D, which is
  # owned by the Nvidia dGPU. This causes "[drm] *ERROR* Failed to probe lspcon"
  # at every boot. Both modprobe options and kernel parameters are set —
  # modprobe alone is insufficient as i915 probes via an early init path.
  boot.extraModprobeConfig = ''
    options i915 enable_psr=0
    options i915 enable_dp_mst=0
  '';

  boot.kernelParams = [
    "i915.enable_psr=0"
    "i915.enable_dp_mst=0"
    "quiet"
    "loglevel=0"
    "rd.udev.log_level=0" # suppress udev messages in initrd
    "udev.log_priority=0" # suppress udev messages in userspace
  ];

  # Set kernel printk levels: console=0 (silent), default=0, boot=0, syslog=7
  # This suppresses DRM error messages that bypass the loglevel parameter.
  boot.kernel.sysctl."kernel.printk" = "0 0 0 7";

  # ── Host-specific overrides ───────────────────────────────────────────────
  # Add anything else specific to this machine:
  # - additional hardware modules
  # - per-host package additions
  # - host-specific services
}
