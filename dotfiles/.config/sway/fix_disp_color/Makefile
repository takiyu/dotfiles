# ------------------------------------------------------------------------------
# ----------------------- Per-output gamma LUT daemon --------------------------
# ------------------------------------------------------------------------------

.PHONY: all clean check

WAYLAND_SCANNER = wayland-scanner
CC = cc
CFLAGS = -O2 -Wall -Wextra $(shell pkg-config --cflags wayland-client libdrm)
LDLIBS = $(shell pkg-config --libs wayland-client libdrm) -lm

WLR_XML = wlr-gamma-control-unstable-v1.xml
WLR_HDR = wlr-gamma-control-unstable-v1-client-protocol.h
WLR_SRC = wlr-gamma-control-unstable-v1-protocol.c
XDG_XML = xdg-output-unstable-v1.xml
XDG_HDR = xdg-output-unstable-v1-client-protocol.h
XDG_SRC = xdg-output-unstable-v1-protocol.c
PROTO_HDR = $(WLR_HDR) $(XDG_HDR)
PROTO_SRC = $(WLR_SRC) $(XDG_SRC)


# ------------------------------------------------------------------------------
# ----------------------------- Standard Commands ------------------------------
# ------------------------------------------------------------------------------
all: fix_disp_color check_gamma

$(WLR_HDR): $(WLR_XML)
	$(WAYLAND_SCANNER) client-header $< $@

$(WLR_SRC): $(WLR_XML)
	$(WAYLAND_SCANNER) private-code $< $@

$(XDG_HDR): $(XDG_XML)
	$(WAYLAND_SCANNER) client-header $< $@

$(XDG_SRC): $(XDG_XML)
	$(WAYLAND_SCANNER) private-code $< $@

fix_disp_color: fix_disp_color.c $(PROTO_SRC) $(PROTO_HDR)
	$(CC) $(CFLAGS) -o $@ fix_disp_color.c $(PROTO_SRC) $(LDLIBS)

check_gamma: check_gamma.c
	$(CC) $(CFLAGS) -o $@ check_gamma.c $(LDLIBS)


# ------------------------------------------------------------------------------
# --------------------------------- Cleanup ------------------------------------
# ------------------------------------------------------------------------------
clean:
	rm -f fix_disp_color check_gamma $(PROTO_SRC) $(PROTO_HDR)

check: all
	./check_gamma


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
