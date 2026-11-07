# fix_disp_color

Per-output gamma LUT daemon for Sway/wlroots.

## What it does

The daemon reads each output's EDID, computes an FNV-1a 64 hash, and
looks up the matching correction in `profiles.conf`. Unknown EDIDs use
identity (no correction), so a new monitor is never adjusted incorrectly.

The current profiles handle the HDMI-input monitors on this machine.
Their HDMI inputs expect limited-range video (16-235), while the
compositor sends full-range RGB (0-255). The profile pre-compresses
0-255 to 16-235 for those EDIDs.

## Build

```bash
make
```

## Run

```bash
./start.sh
```

`start.sh` stops an existing instance and waits before starting. The
process must stay running. Sway starts it with:

```bash
exec ~/.config/sway/fix_disp_color/start.sh
```

## Add a monitor

```bash
./fix_disp_color --list-edid
```

Add the printed hash to `profiles.conf` with the desired values. Restart
the daemon with `./start.sh`.

## Check

```bash
./fix_disp_color --list
./fix_disp_color --list-edid
./fix_disp_color --print-lut
./check_gamma
```
