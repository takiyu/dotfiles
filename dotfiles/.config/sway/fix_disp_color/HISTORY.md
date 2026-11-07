# History

## Findings

- C uses the monitor DisplayPort input (`DisplayPort-1`).
- A, B, and D use the monitor HDMI input (`HDMI-1`).
- A is connected through a DP-to-HDMI adapter.
- DDC/CI settings are identical on all four monitors: brightness 16,
  contrast 0, color preset 6500K, RGB gains 50/50/50, sharpness 2,
  black level 80.
- The panel color primaries, white point, and gamma 2.2 are identical.
- DP-1 reports EDID 1.4 / 10-bit / DisplayPort.
- The HDMI inputs report EDID 1.3 / 8-bit / HDMI.
- The HDMI inputs interpret the full-range compositor output as limited
  range (16-235). Values below 16 and above 235 are clipped, so shadow
  and highlight detail is lost.
- DP-1 interprets the same output as full range (0-255), so C looked
  correct.
- A stretch LUT (16-235 to 0-255) made the HDMI image worse. A compress
  LUT (0-255 to 16-235) matched C. This confirms the HDMI limited-range
  interpretation.
- Sway 1.9 does not apply ICC profiles. The daemon uses the wlroots
  `zwlr_gamma_control_v1` protocol instead.
- The 1D LUT fixes the range mismatch. It cannot restore chroma
  subsampling or true 10-bit gradation.

## EDID profiles

- FNV-1a 64 hash of the ChangHong MNT HDMI EDID:
  `7699a4c8f5af75af` -> compress 0-255 to 16-235.
- FNV-1a 64 hash of the ChangHong MNT DP EDID:
  `255ce3ed724233c5` -> identity.
- Unknown EDIDs use identity (no correction).
- `profiles.conf` maps hashes to LUT parameters.
- `fix_disp_color --list-edid` prints output names, hashes, and profiles.
- `start.sh` starts the daemon with `--profile ./profiles.conf`.

## Usage

- `--identity` disables the correction.
- `--in-black`, `--in-white`, `--out-black`, `--out-white`, `--gamma`,
  and `--gain-r/g/b` override the profile for testing.
- `check_gamma` shows the DRM `GAMMA_LUT` blob per connector.
- The daemon must stay running; the compositor resets the LUT when the
  client disconnects.
- `start.sh` stops an existing instance before starting. The daemon
  reapplies the LUT after 2 seconds to avoid a disconnect race.
