// -----------------------------------------------------------------------------
// --------------------- Per-output gamma LUT daemon ---------------------------
// -----------------------------------------------------------------------------
#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <glob.h>
#include <math.h>
#include <poll.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>

#include "wlr-gamma-control-unstable-v1-client-protocol.h"
#include "xdg-output-unstable-v1-client-protocol.h"

// -----------------------------------------------------------------------------
// --------------------------------- Constants ---------------------------------
// -----------------------------------------------------------------------------
#define MAX_TARGETS 16
#define MAX_PROFILES 32
#define NAME_LEN 64
#define HASH_LEN 17
#define LABEL_LEN 32

// -----------------------------------------------------------------------------
// ------------------------------ Data Structures ------------------------------
// -----------------------------------------------------------------------------
struct config {
    double in_black;
    double in_white;
    double out_black;
    double out_white;
    double gamma;
    double gain_r;
    double gain_g;
    double gain_b;
};

struct profile {
    char hash[HASH_LEN];
    char label[LABEL_LEN];
    struct config cfg;
};

struct output_state {
    struct wl_list link;
    struct app_state *app;
    struct wl_output *wl_output;
    struct zxdg_output_v1 *xdg_output;
    struct zwlr_gamma_control_v1 *gamma_control;
    uint32_t global_name;
    uint32_t gamma_size;
    char name[NAME_LEN];
    char edid_hash[HASH_LEN];
    struct config cfg;
    bool name_ready;
    bool is_target;
    bool has_profile;
};

struct app_state {
    struct wl_display *display;
    struct wl_registry *registry;
    struct zwlr_gamma_control_manager_v1 *gamma_manager;
    struct zxdg_output_manager_v1 *xdg_output_manager;
    struct wl_list outputs;
    int signal_pipe[2];
    time_t reapply_at;
    struct config cfg;
    struct profile profiles[MAX_PROFILES];
    int profile_count;
    const char *profile_path;
    char targets[MAX_TARGETS][NAME_LEN];
    int target_count;
    int matched_count;
    bool override_config;
    bool list_only;
    bool list_edid;
    bool print_lut;
    bool verbose;
};

// -----------------------------------------------------------------------------
// --------------------------------- Interface ---------------------------------
// -----------------------------------------------------------------------------
static int parse_args(struct app_state *app, int argc, char **argv);
static void print_usage(const char *prog);
static void add_target(struct app_state *app, const char *name);
static bool is_target_name(const struct app_state *app, const char *name);
static int parse_double(const char *text, double *value);
static int validate_config(const struct config *cfg);
static struct config default_config(void);
static struct config identity_config(void);
static bool near(double a, double b);
static bool config_equals(const struct config *a, const struct config *b);
static bool is_identity_config(const struct config *cfg);
static int load_profiles(struct app_state *app);
static const struct profile *find_profile(const struct app_state *app,
                                          const char *hash);
static void fnv1a_hash(const uint8_t *data, size_t len, char *out);
static int read_edid_hash(const char *name, char *hash);
static void print_edid_info(struct output_state *out);
static void refresh_output_profile(struct output_state *out);
static void print_lut(const struct config *cfg);
static int run(struct app_state *app);
static void cleanup(struct app_state *app);
static void destroy_output(struct output_state *out);
static void install_signal_handlers(struct app_state *app);
static void handle_signal(int signum);
static void drain_signal_pipe(int fd);
static void set_output_name(struct output_state *out, const char *name);
static void maybe_create_xdg_output(struct output_state *out);
static void maybe_create_gamma_control(struct output_state *out);
static void apply_gamma(struct output_state *out);
static void fill_lut(uint16_t *lut, uint32_t size, const struct config *cfg);
static uint16_t lut_value(double x, const struct config *cfg, double gain);
static void output_handle_geometry(void *data, struct wl_output *wl_output,
                                   int32_t x, int32_t y, int32_t phys_width,
                                   int32_t phys_height, int32_t subpixel,
                                   const char *make, const char *model,
                                   int32_t transform);
static void output_handle_mode(void *data, struct wl_output *wl_output,
                               uint32_t flags, int32_t width, int32_t height,
                               int32_t refresh);
static void output_handle_done(void *data, struct wl_output *wl_output);
static void output_handle_scale(void *data, struct wl_output *wl_output,
                                int32_t factor);
static void output_handle_name(void *data, struct wl_output *wl_output,
                               const char *name);
static void output_handle_description(void *data, struct wl_output *wl_output,
                                      const char *description);
static void xdg_output_handle_logical_position(
    void *data, struct zxdg_output_v1 *xdg_output, int32_t x, int32_t y);
static void xdg_output_handle_logical_size(void *data,
                                           struct zxdg_output_v1 *xdg_output,
                                           int32_t width, int32_t height);
static void xdg_output_handle_done(void *data,
                                   struct zxdg_output_v1 *xdg_output);
static void xdg_output_handle_name(void *data,
                                   struct zxdg_output_v1 *xdg_output,
                                   const char *name);
static void xdg_output_handle_description(void *data,
                                          struct zxdg_output_v1 *xdg_output,
                                          const char *description);
static void gamma_handle_size(void *data, struct zwlr_gamma_control_v1 *control,
                              uint32_t size);
static void gamma_handle_failed(void *data,
                                struct zwlr_gamma_control_v1 *control);
static void registry_handle_global(void *data, struct wl_registry *registry,
                                   uint32_t name, const char *interface,
                                   uint32_t version);
static void registry_handle_global_remove(void *data,
                                          struct wl_registry *registry,
                                          uint32_t name);

static const struct wl_output_listener output_listener = {
    .geometry = output_handle_geometry,
    .mode = output_handle_mode,
    .done = output_handle_done,
    .scale = output_handle_scale,
    .name = output_handle_name,
    .description = output_handle_description};

static const struct zxdg_output_v1_listener xdg_output_listener = {
    .logical_position = xdg_output_handle_logical_position,
    .logical_size = xdg_output_handle_logical_size,
    .done = xdg_output_handle_done,
    .name = xdg_output_handle_name,
    .description = xdg_output_handle_description};

static const struct zwlr_gamma_control_v1_listener gamma_listener = {
    .gamma_size = gamma_handle_size, .failed = gamma_handle_failed};

static const struct wl_registry_listener registry_listener = {
    .global = registry_handle_global,
    .global_remove = registry_handle_global_remove};

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// ------------------------------ Implementation -------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

// Signal stop flag
static volatile sig_atomic_t g_stop_requested = 0;
static int g_signal_write_fd = -1;

enum {
    OPT_IN_BLACK = 1000,
    OPT_IN_WHITE,
    OPT_OUT_BLACK,
    OPT_OUT_WHITE,
    OPT_GAMMA,
    OPT_GAIN_R,
    OPT_GAIN_G,
    OPT_GAIN_B,
    OPT_IDENTITY,
    OPT_PROFILE,
    OPT_LIST,
    OPT_LIST_EDID,
    OPT_PRINT_LUT
};

int main(int argc, char **argv) {
    struct app_state app;
    memset(&app, 0, sizeof(app));
    wl_list_init(&app.outputs);
    app.signal_pipe[0] = -1;
    app.signal_pipe[1] = -1;
    app.cfg = default_config();

    if (0 != parse_args(&app, argc, argv)) {
        return EXIT_FAILURE;
    }
    if (app.print_lut) {
        print_lut(&app.cfg);
        return EXIT_SUCCESS;
    }
    return run(&app);
}

// -----------------------------------------------------------------------------
// ------------------------------- CLI Parsing ---------------------------------
// -----------------------------------------------------------------------------
static int parse_args(struct app_state *app, int argc, char **argv) {
    static const struct option long_options[] = {
        {"output", required_argument, NULL, 'o'},
        {"in-black", required_argument, NULL, OPT_IN_BLACK},
        {"in-white", required_argument, NULL, OPT_IN_WHITE},
        {"out-black", required_argument, NULL, OPT_OUT_BLACK},
        {"out-white", required_argument, NULL, OPT_OUT_WHITE},
        {"gamma", required_argument, NULL, OPT_GAMMA},
        {"gain-r", required_argument, NULL, OPT_GAIN_R},
        {"gain-g", required_argument, NULL, OPT_GAIN_G},
        {"gain-b", required_argument, NULL, OPT_GAIN_B},
        {"identity", no_argument, NULL, OPT_IDENTITY},
        {"profile", required_argument, NULL, OPT_PROFILE},
        {"list", no_argument, NULL, OPT_LIST},
        {"list-edid", no_argument, NULL, OPT_LIST_EDID},
        {"print-lut", no_argument, NULL, OPT_PRINT_LUT},
        {"verbose", no_argument, NULL, 'v'},
        {"help", no_argument, NULL, 'h'},
        {NULL, 0, NULL, 0}};

    int opt = 0;
    while (-1 != (opt = getopt_long(argc, argv, "o:vh", long_options, NULL))) {
        switch (opt) {
            case 'o':
                add_target(app, optarg);
                break;
            case OPT_IN_BLACK:
                app->override_config = true;
                if (0 != parse_double(optarg, &app->cfg.in_black)) {
                    return -1;
                }
                break;
            case OPT_IN_WHITE:
                app->override_config = true;
                if (0 != parse_double(optarg, &app->cfg.in_white)) {
                    return -1;
                }
                break;
            case OPT_OUT_BLACK:
                app->override_config = true;
                if (0 != parse_double(optarg, &app->cfg.out_black)) {
                    return -1;
                }
                break;
            case OPT_OUT_WHITE:
                app->override_config = true;
                if (0 != parse_double(optarg, &app->cfg.out_white)) {
                    return -1;
                }
                break;
            case OPT_GAMMA:
                app->override_config = true;
                if (0 != parse_double(optarg, &app->cfg.gamma)) {
                    return -1;
                }
                break;
            case OPT_GAIN_R:
                app->override_config = true;
                if (0 != parse_double(optarg, &app->cfg.gain_r)) {
                    return -1;
                }
                break;
            case OPT_GAIN_G:
                app->override_config = true;
                if (0 != parse_double(optarg, &app->cfg.gain_g)) {
                    return -1;
                }
                break;
            case OPT_GAIN_B:
                app->override_config = true;
                if (0 != parse_double(optarg, &app->cfg.gain_b)) {
                    return -1;
                }
                break;
            case OPT_IDENTITY:
                app->override_config = true;
                app->cfg = identity_config();
                break;
            case OPT_PROFILE:
                app->profile_path = optarg;
                break;
            case OPT_LIST:
                app->list_only = true;
                break;
            case OPT_LIST_EDID:
                app->list_edid = true;
                break;
            case OPT_PRINT_LUT:
                app->print_lut = true;
                break;
            case 'v':
                app->verbose = true;
                break;
            case 'h':
                print_usage(argv[0]);
                exit(EXIT_SUCCESS);
            default:
                print_usage(argv[0]);
                return -1;
        }
    }

    return validate_config(&app->cfg);
}

static void print_usage(const char *prog) {
    fprintf(stderr,
            "Usage: %s [options]\n"
            "  -o, --output NAME     target output name (repeatable)\n"
            "      --in-black N      input black level (default 0)\n"
            "      --in-white N      input white level (default 255)\n"
            "      --out-black N     output black level (default 16)\n"
            "      --out-white N     output white level (default 235)\n"
            "      --gamma F         gamma exponent (default 1.0)\n"
            "      --gain-r F        red gain (default 1.0)\n"
            "      --gain-g F        green gain (default 1.0)\n"
            "      --gain-b F        blue gain (default 1.0)\n"
            "      --identity        disable the LUT\n"
            "      --profile FILE    EDID profile file (default ./profiles.conf)\n"
            "      --list            list output names and exit\n"
            "      --list-edid       list output EDID hashes and exit\n"
            "      --print-lut       print sample LUT values and exit\n"
            "  -v, --verbose         verbose logging\n"
            "  -h, --help            show this help\n",
            prog);
}

static void add_target(struct app_state *app, const char *name) {
    if (MAX_TARGETS <= app->target_count) {
        fprintf(stderr, "too many targets\n");
        return;
    }
    for (int i = 0; i < app->target_count; i++) {
        if (0 == strcmp(app->targets[i], name)) {
            return;
        }
    }
    snprintf(app->targets[app->target_count], NAME_LEN, "%s", name);
    app->target_count++;
}

static bool is_target_name(const struct app_state *app, const char *name) {
    for (int i = 0; i < app->target_count; i++) {
        if (0 == strcmp(app->targets[i], name)) {
            return true;
        }
    }
    return false;
}

static int parse_double(const char *text, double *value) {
    char *end = NULL;
    errno = 0;
    double parsed = strtod(text, &end);
    if (0 != errno || end == text || '\0' != *end) {
        fprintf(stderr, "invalid number: %s\n", text);
        return -1;
    }
    *value = parsed;
    return 0;
}

static int validate_config(const struct config *cfg) {
    if (0.0 > cfg->in_black || 255.0 < cfg->in_black) {
        fprintf(stderr, "in-black out of range\n");
        return -1;
    }
    if (0.0 > cfg->in_white || 255.0 < cfg->in_white) {
        fprintf(stderr, "in-white out of range\n");
        return -1;
    }
    if (cfg->in_white <= cfg->in_black) {
        fprintf(stderr, "in-white must be larger than in-black\n");
        return -1;
    }
    if (0.0 > cfg->out_black || 255.0 < cfg->out_black) {
        fprintf(stderr, "out-black out of range\n");
        return -1;
    }
    if (0.0 > cfg->out_white || 255.0 < cfg->out_white) {
        fprintf(stderr, "out-white out of range\n");
        return -1;
    }
    if (cfg->out_white <= cfg->out_black) {
        fprintf(stderr, "out-white must be larger than out-black\n");
        return -1;
    }
    if (0.1 > cfg->gamma || 10.0 < cfg->gamma) {
        fprintf(stderr, "gamma out of range\n");
        return -1;
    }
    if (0.0 > cfg->gain_r || 4.0 < cfg->gain_r || 0.0 > cfg->gain_g ||
        4.0 < cfg->gain_g || 0.0 > cfg->gain_b || 4.0 < cfg->gain_b) {
        fprintf(stderr, "gain out of range\n");
        return -1;
    }
    return 0;
}

static struct config default_config(void) {
    struct config cfg;
    cfg.in_black = 0.0;
    cfg.in_white = 255.0;
    cfg.out_black = 16.0;
    cfg.out_white = 235.0;
    cfg.gamma = 1.0;
    cfg.gain_r = 1.0;
    cfg.gain_g = 1.0;
    cfg.gain_b = 1.0;
    return cfg;
}

static struct config identity_config(void) {
    struct config cfg;
    cfg.in_black = 0.0;
    cfg.in_white = 255.0;
    cfg.out_black = 0.0;
    cfg.out_white = 255.0;
    cfg.gamma = 1.0;
    cfg.gain_r = 1.0;
    cfg.gain_g = 1.0;
    cfg.gain_b = 1.0;
    return cfg;
}


static bool near(double a, double b) {
    return 1e-6 > fabs(a - b);
}


static bool config_equals(const struct config *a, const struct config *b) {
    return near(a->in_black, b->in_black) && near(a->in_white, b->in_white) &&
           near(a->out_black, b->out_black) &&
           near(a->out_white, b->out_white) && near(a->gamma, b->gamma) &&
           near(a->gain_r, b->gain_r) && near(a->gain_g, b->gain_g) &&
           near(a->gain_b, b->gain_b);
}


static bool is_identity_config(const struct config *cfg) {
    struct config identity = identity_config();
    return config_equals(cfg, &identity);
}


// -----------------------------------------------------------------------------
// ------------------------------- EDID Profiles -------------------------------
// -----------------------------------------------------------------------------
static int load_profiles(struct app_state *app) {
    const char *path = app->profile_path;
    if (NULL == path) {
        path = "./profiles.conf";
    }
    FILE *file = fopen(path, "r");
    if (NULL == file) {
        fprintf(stderr, "profile file not found: %s (unknown EDIDs use identity)\n",
                path);
        return 0;
    }

    char line[512];
    while (NULL != fgets(line, sizeof(line), file)) {
        char *start = line;
        while (' ' == *start || '\t' == *start) {
            start++;
        }
        if ('\0' == *start || '\n' == *start || '#' == *start) {
            continue;
        }

        char hash[HASH_LEN];
        char label[LABEL_LEN] = "profile";
        struct config cfg;
        int matched = sscanf(start,
                             "%16s %lf %lf %lf %lf %lf %lf %lf %lf %31s",
                             hash, &cfg.in_black, &cfg.in_white,
                             &cfg.out_black, &cfg.out_white, &cfg.gamma,
                             &cfg.gain_r, &cfg.gain_g, &cfg.gain_b, label);
        if (9 > matched) {
            fprintf(stderr, "invalid profile line: %s", line);
            continue;
        }
        if (0 != validate_config(&cfg)) {
            fprintf(stderr, "invalid profile values for %s\n", hash);
            continue;
        }
        if (MAX_PROFILES <= app->profile_count) {
            fprintf(stderr, "too many profiles\n");
            break;
        }
        struct profile *profile = &app->profiles[app->profile_count];
        snprintf(profile->hash, HASH_LEN, "%s", hash);
        snprintf(profile->label, LABEL_LEN, "%s", label);
        profile->cfg = cfg;
        app->profile_count++;
    }
    fclose(file);
    return 0;
}


static const struct profile *find_profile(const struct app_state *app,
                                          const char *hash) {
    for (int i = 0; i < app->profile_count; i++) {
        if (0 == strcmp(app->profiles[i].hash, hash)) {
            return &app->profiles[i];
        }
    }
    return NULL;
}


static void fnv1a_hash(const uint8_t *data, size_t len, char *out) {
    uint64_t hash = 1469598103934665603ULL;
    for (size_t i = 0; i < len; i++) {
        hash ^= data[i];
        hash *= 1099511628211ULL;
    }
    snprintf(out, HASH_LEN, "%016llx", (unsigned long long)hash);
}


static int read_edid_hash(const char *name, char *hash) {
    char pattern[512];
    snprintf(pattern, sizeof(pattern), "/sys/class/drm/card*-%s/edid", name);
    glob_t matches;
    memset(&matches, 0, sizeof(matches));
    int result = glob(pattern, 0, NULL, &matches);
    if (0 != result || 0 == matches.gl_pathc) {
        globfree(&matches);
        snprintf(hash, HASH_LEN, "0000000000000000");
        return -1;
    }

    int fd = open(matches.gl_pathv[0], O_RDONLY);
    globfree(&matches);
    if (0 > fd) {
        snprintf(hash, HASH_LEN, "0000000000000000");
        return -1;
    }

    uint8_t data[4096];
    size_t total = 0;
    while (total < sizeof(data)) {
        ssize_t count = read(fd, data + total, sizeof(data) - total);
        if (0 > count) {
            break;
        }
        if (0 == count) {
            break;
        }
        total += (size_t)count;
    }
    close(fd);
    if (0 == total) {
        snprintf(hash, HASH_LEN, "0000000000000000");
        return -1;
    }
    fnv1a_hash(data, total, hash);
    return 0;
}


static void print_edid_info(struct output_state *out) {
    char hash[HASH_LEN];
    if (0 != read_edid_hash(out->name, hash)) {
        printf("%-12s %s unknown\n", out->name, hash);
        return;
    }
    const struct profile *profile = find_profile(out->app, hash);
    if (NULL != profile) {
        printf("%-12s %s %s\n", out->name, hash, profile->label);
        return;
    }
    printf("%-12s %s identity\n", out->name, hash);
}


static void refresh_output_profile(struct output_state *out) {
    struct app_state *app = out->app;
    if (!out->name_ready || app->list_only || app->list_edid) {
        return;
    }

    char hash[HASH_LEN] = "0000000000000000";
    const char *label = "identity";
    struct config cfg;
    bool has_profile = false;

    if (app->override_config) {
        cfg = app->cfg;
        label = "override";
        has_profile = out->is_target && !is_identity_config(&cfg);
    } else {
        if (0 != read_edid_hash(out->name, hash)) {
            cfg = identity_config();
            label = "unknown, identity";
        } else {
            const struct profile *profile = find_profile(app, hash);
            if (NULL != profile) {
                cfg = profile->cfg;
                label = profile->label;
                has_profile = out->is_target && !is_identity_config(&cfg);
            } else {
                cfg = identity_config();
                label = "unknown, identity";
            }
        }
    }

    bool changed = (out->has_profile != has_profile) ||
                   !config_equals(&out->cfg, &cfg) ||
                   0 != strcmp(out->edid_hash, hash);
    if (changed && app->verbose) {
        fprintf(stderr, "%s: %s %s\n", out->name, hash, label);
    }
    out->has_profile = has_profile;
    out->cfg = cfg;
    snprintf(out->edid_hash, HASH_LEN, "%s", hash);

    if (!has_profile) {
        if (NULL != out->gamma_control) {
            zwlr_gamma_control_v1_destroy(out->gamma_control);
            out->gamma_control = NULL;
        }
        out->gamma_size = 0;
        return;
    }
    if (NULL == out->gamma_control) {
        maybe_create_gamma_control(out);
        return;
    }
    if (changed) {
        apply_gamma(out);
    }
}


static void print_lut(const struct config *cfg) {
    const uint32_t samples[] = {0, 16, 32, 128, 224, 235, 255};
    const size_t count = sizeof(samples) / sizeof(samples[0]);
    for (size_t i = 0; i < count; i++) {
        uint32_t index = samples[i];
        double x = (double)index / 255.0;
        uint16_t value = lut_value(x, cfg, 1.0);
        printf("%3u -> %5u\n", index, value);
    }
}

// -----------------------------------------------------------------------------
// ------------------------------- Wayland Run ---------------------------------
// -----------------------------------------------------------------------------
static int run(struct app_state *app) {
    if (app->list_edid || !app->override_config) {
        load_profiles(app);
    }
    install_signal_handlers(app);

    app->display = wl_display_connect(NULL);
    if (NULL == app->display) {
        fprintf(stderr, "failed to connect to Wayland\n");
        return EXIT_FAILURE;
    }
    app->registry = wl_display_get_registry(app->display);
    wl_registry_add_listener(app->registry, &registry_listener, app);

    if (0 > wl_display_roundtrip(app->display)) {
        fprintf(stderr, "Wayland roundtrip failed\n");
        cleanup(app);
        return EXIT_FAILURE;
    }
    if (0 > wl_display_roundtrip(app->display)) {
        fprintf(stderr, "Wayland roundtrip failed\n");
        cleanup(app);
        return EXIT_FAILURE;
    }
    if (app->list_only || app->list_edid) {
        cleanup(app);
        return EXIT_SUCCESS;
    }
    if (NULL == app->gamma_manager) {
        fprintf(stderr, "compositor does not support wlr-gamma-control\n");
        cleanup(app);
        return EXIT_FAILURE;
    }

    struct output_state *out;
    wl_list_for_each(out, &app->outputs, link) {
        refresh_output_profile(out);
    }
    if (0 > wl_display_roundtrip(app->display)) {
        fprintf(stderr, "Wayland roundtrip failed\n");
        cleanup(app);
        return EXIT_FAILURE;
    }
    if (app->verbose) {
        const char *path = app->profile_path;
        if (NULL == path) {
            path = "./profiles.conf";
        }
        fprintf(stderr, "profiles: %s (%d entries)\n", path,
                app->profile_count);
        if (0 < app->target_count) {
            fprintf(stderr, "targets:");
            for (int i = 0; i < app->target_count; i++) {
                fprintf(stderr, " %s", app->targets[i]);
            }
            fprintf(stderr, "\n");
        }
    }
    app->reapply_at = time(NULL) + 2;

    int wayland_fd = wl_display_get_fd(app->display);
    while (0 == g_stop_requested) {
        while (0 != wl_display_prepare_read(app->display)) {
            if (0 != wl_display_dispatch_pending(app->display)) {
                fprintf(stderr, "Wayland dispatch failed\n");
                goto done;
            }
        }
        if (0 > wl_display_flush(app->display) && EAGAIN != errno) {
            wl_display_cancel_read(app->display);
            fprintf(stderr, "Wayland flush failed: %s\n", strerror(errno));
            break;
        }

        int timeout = -1;
        if (0 != app->reapply_at) {
            time_t now = time(NULL);
            if (now < app->reapply_at) {
                timeout = (int)(app->reapply_at - now) * 1000;
            } else {
                struct output_state *target;
                wl_list_for_each(target, &app->outputs, link) {
                    apply_gamma(target);
                }
                wl_display_flush(app->display);
                app->reapply_at = 0;
            }
        }

        struct pollfd fds[2];
        fds[0].fd = wayland_fd;
        fds[0].events = POLLIN;
        fds[0].revents = 0;
        fds[1].fd = app->signal_pipe[0];
        fds[1].events = POLLIN;
        fds[1].revents = 0;
        int poll_result = poll(fds, 2, timeout);
        if (0 > poll_result) {
            wl_display_cancel_read(app->display);
            if (EINTR == errno) {
                continue;
            }
            fprintf(stderr, "poll failed: %s\n", strerror(errno));
            break;
        }
        if (0 != (fds[0].revents & (POLLHUP | POLLERR))) {
            wl_display_cancel_read(app->display);
            break;
        }
        if (0 != (fds[0].revents & POLLIN)) {
            if (0 != wl_display_read_events(app->display)) {
                fprintf(stderr, "Wayland read failed\n");
                break;
            }
            if (0 != wl_display_dispatch_pending(app->display)) {
                fprintf(stderr, "Wayland dispatch failed\n");
                break;
            }
        } else {
            wl_display_cancel_read(app->display);
        }
        if (0 != (fds[1].revents & POLLIN)) {
            drain_signal_pipe(fds[1].fd);
            break;
        }
    }

done:
    cleanup(app);
    return EXIT_SUCCESS;
}

static void cleanup(struct app_state *app) {
    struct output_state *out;
    struct output_state *tmp;
    wl_list_for_each_safe(out, tmp, &app->outputs, link) {
        destroy_output(out);
    }
    if (NULL != app->gamma_manager) {
        zwlr_gamma_control_manager_v1_destroy(app->gamma_manager);
        app->gamma_manager = NULL;
    }
    if (NULL != app->xdg_output_manager) {
        zxdg_output_manager_v1_destroy(app->xdg_output_manager);
        app->xdg_output_manager = NULL;
    }
    if (NULL != app->registry) {
        wl_registry_destroy(app->registry);
        app->registry = NULL;
    }
    if (0 <= app->signal_pipe[0]) {
        close(app->signal_pipe[0]);
        app->signal_pipe[0] = -1;
    }
    if (0 <= app->signal_pipe[1]) {
        close(app->signal_pipe[1]);
        app->signal_pipe[1] = -1;
    }
    g_signal_write_fd = -1;
    if (NULL != app->display) {
        wl_display_flush(app->display);
        wl_display_disconnect(app->display);
        app->display = NULL;
    }
}

static void destroy_output(struct output_state *out) {
    if (NULL != out->gamma_control) {
        zwlr_gamma_control_v1_destroy(out->gamma_control);
        out->gamma_control = NULL;
    }
    if (NULL != out->xdg_output) {
        zxdg_output_v1_destroy(out->xdg_output);
        out->xdg_output = NULL;
    }
    if (NULL != out->wl_output) {
        wl_output_destroy(out->wl_output);
        out->wl_output = NULL;
    }
    wl_list_remove(&out->link);
    free(out);
}

// -----------------------------------------------------------------------------
// ------------------------------- Wayland Events ------------------------------
// -----------------------------------------------------------------------------
static void output_handle_geometry(void *data, struct wl_output *wl_output,
                                   int32_t x, int32_t y, int32_t phys_width,
                                   int32_t phys_height, int32_t subpixel,
                                   const char *make, const char *model,
                                   int32_t transform) {
    (void)data;
    (void)wl_output;
    (void)x;
    (void)y;
    (void)phys_width;
    (void)phys_height;
    (void)subpixel;
    (void)make;
    (void)model;
    (void)transform;
}

static void output_handle_mode(void *data, struct wl_output *wl_output,
                               uint32_t flags, int32_t width, int32_t height,
                               int32_t refresh) {
    (void)data;
    (void)wl_output;
    (void)flags;
    (void)width;
    (void)height;
    (void)refresh;
}

static void output_handle_done(void *data, struct wl_output *wl_output) {
    struct output_state *out = data;
    (void)wl_output;
    refresh_output_profile(out);
}

static void output_handle_scale(void *data, struct wl_output *wl_output,
                                int32_t factor) {
    (void)data;
    (void)wl_output;
    (void)factor;
}

static void output_handle_name(void *data, struct wl_output *wl_output,
                               const char *name) {
    struct output_state *out = data;
    (void)wl_output;
    set_output_name(out, name);
}

static void output_handle_description(void *data, struct wl_output *wl_output,
                                      const char *description) {
    (void)data;
    (void)wl_output;
    (void)description;
}

static void xdg_output_handle_logical_position(
    void *data, struct zxdg_output_v1 *xdg_output, int32_t x, int32_t y) {
    (void)data;
    (void)xdg_output;
    (void)x;
    (void)y;
}

static void xdg_output_handle_logical_size(void *data,
                                           struct zxdg_output_v1 *xdg_output,
                                           int32_t width, int32_t height) {
    (void)data;
    (void)xdg_output;
    (void)width;
    (void)height;
}

static void xdg_output_handle_done(void *data,
                                   struct zxdg_output_v1 *xdg_output) {
    struct output_state *out = data;
    (void)xdg_output;
    refresh_output_profile(out);
}

static void xdg_output_handle_name(void *data,
                                   struct zxdg_output_v1 *xdg_output,
                                   const char *name) {
    struct output_state *out = data;
    (void)xdg_output;
    set_output_name(out, name);
}

static void xdg_output_handle_description(void *data,
                                          struct zxdg_output_v1 *xdg_output,
                                          const char *description) {
    (void)data;
    (void)xdg_output;
    (void)description;
}

static void gamma_handle_size(void *data, struct zwlr_gamma_control_v1 *control,
                              uint32_t size) {
    struct output_state *out = data;
    (void)control;

    out->gamma_size = size;
    apply_gamma(out);
}

static void gamma_handle_failed(void *data,
                                struct zwlr_gamma_control_v1 *control) {
    struct output_state *out = data;
    (void)control;

    fprintf(stderr, "gamma control failed for %s\n", out->name);
    if (NULL != out->gamma_control) {
        zwlr_gamma_control_v1_destroy(out->gamma_control);
        out->gamma_control = NULL;
    }
}

static void registry_handle_global(void *data, struct wl_registry *registry,
                                   uint32_t name, const char *interface,
                                   uint32_t version) {
    struct app_state *app = data;

    if (0 == strcmp(interface, wl_output_interface.name)) {
        struct output_state *out = calloc(1, sizeof(*out));
        if (NULL == out) {
            fprintf(stderr, "out of memory\n");
            return;
        }
        out->app = app;
        out->global_name = name;
        uint32_t bind_version = version;
        if (4 < bind_version) {
            bind_version = 4;
        }
        out->wl_output = wl_registry_bind(registry, name, &wl_output_interface,
                                          bind_version);
        wl_output_add_listener(out->wl_output, &output_listener, out);
        wl_list_insert(&app->outputs, &out->link);
        maybe_create_xdg_output(out);
        return;
    }
    if (0 == strcmp(interface, zxdg_output_manager_v1_interface.name)) {
        uint32_t bind_version = version;
        if (3 < bind_version) {
            bind_version = 3;
        }
        app->xdg_output_manager = wl_registry_bind(
            registry, name, &zxdg_output_manager_v1_interface, bind_version);
        struct output_state *out;
        wl_list_for_each(out, &app->outputs, link) {
            maybe_create_xdg_output(out);
        }
        return;
    }
    if (0 == strcmp(interface, zwlr_gamma_control_manager_v1_interface.name)) {
        app->gamma_manager = wl_registry_bind(
            registry, name, &zwlr_gamma_control_manager_v1_interface, 1);
        struct output_state *out;
        wl_list_for_each(out, &app->outputs, link) {
            refresh_output_profile(out);
        }
    }
}

static void registry_handle_global_remove(void *data,
                                          struct wl_registry *registry,
                                          uint32_t name) {
    struct app_state *app = data;
    (void)registry;

    struct output_state *out;
    struct output_state *tmp;
    wl_list_for_each_safe(out, tmp, &app->outputs, link) {
        if (out->global_name == name) {
            destroy_output(out);
            break;
        }
    }
}

// -----------------------------------------------------------------------------
// ------------------------------- Output Setup --------------------------------
// -----------------------------------------------------------------------------
static void set_output_name(struct output_state *out, const char *name) {
    if (out->name_ready) {
        return;
    }
    snprintf(out->name, NAME_LEN, "%s", name);
    out->name_ready = true;
    out->is_target = (0 == out->app->target_count) ||
                     is_target_name(out->app, out->name);
    if (out->is_target) {
        out->app->matched_count++;
    }
    if (out->app->list_only) {
        printf("%s\n", out->name);
        return;
    }
    if (out->app->list_edid) {
        print_edid_info(out);
        return;
    }
    refresh_output_profile(out);
}

static void maybe_create_xdg_output(struct output_state *out) {
    if (NULL == out->app->xdg_output_manager || NULL == out->wl_output ||
        NULL != out->xdg_output) {
        return;
    }
    out->xdg_output = zxdg_output_manager_v1_get_xdg_output(
        out->app->xdg_output_manager, out->wl_output);
    zxdg_output_v1_add_listener(out->xdg_output, &xdg_output_listener, out);
}

// -----------------------------------------------------------------------------
// ----------------------------- Gamma LUT Apply -------------------------------
// -----------------------------------------------------------------------------
static void maybe_create_gamma_control(struct output_state *out) {
    if (NULL == out->app->gamma_manager || !out->name_ready ||
        !out->is_target || !out->has_profile ||
        NULL != out->gamma_control) {
        return;
    }
    out->gamma_control = zwlr_gamma_control_manager_v1_get_gamma_control(
        out->app->gamma_manager, out->wl_output);
    zwlr_gamma_control_v1_add_listener(out->gamma_control, &gamma_listener,
                                       out);
}

static void apply_gamma(struct output_state *out) {
    if (NULL == out->gamma_control || 0 == out->gamma_size ||
        !out->is_target || !out->has_profile) {
        return;
    }

    size_t count = (size_t)out->gamma_size * 3;
    size_t bytes = count * sizeof(uint16_t);
    int fd = memfd_create("fix_disp_color_lut", MFD_CLOEXEC);
    if (0 > fd) {
        perror("memfd_create");
        return;
    }
    if (0 != ftruncate(fd, (off_t)bytes)) {
        perror("ftruncate");
        close(fd);
        return;
    }

    uint16_t *lut =
        mmap(NULL, bytes, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (MAP_FAILED == lut) {
        perror("mmap");
        close(fd);
        return;
    }
    fill_lut(lut, out->gamma_size, &out->cfg);
    munmap(lut, bytes);

    if (0 != lseek(fd, 0, SEEK_SET)) {
        perror("lseek");
        close(fd);
        return;
    }
    zwlr_gamma_control_v1_set_gamma(out->gamma_control, fd);
    close(fd);
    wl_display_flush(out->app->display);

    if (out->app->verbose) {
        fprintf(stderr, "applied LUT to %s (%u entries)\n", out->name,
                out->gamma_size);
    }
}

static void fill_lut(uint16_t *lut, uint32_t size, const struct config *cfg) {
    for (uint32_t i = 0; i < size; i++) {
        double x = (double)i / (double)(size - 1);
        lut[i] = lut_value(x, cfg, cfg->gain_r);
        lut[size + i] = lut_value(x, cfg, cfg->gain_g);
        lut[2 * size + i] = lut_value(x, cfg, cfg->gain_b);
    }
}

static uint16_t lut_value(double x, const struct config *cfg, double gain) {
    double value =
        (x * 255.0 - cfg->in_black) / (cfg->in_white - cfg->in_black);
    if (0.0 > value) {
        value = 0.0;
    }
    if (1.0 < value) {
        value = 1.0;
    }
    value = pow(value, cfg->gamma) * gain;
    if (0.0 > value) {
        value = 0.0;
    }
    if (1.0 < value) {
        value = 1.0;
    }
    double output =
        (cfg->out_black + value * (cfg->out_white - cfg->out_black)) / 255.0;
    if (0.0 > output) {
        output = 0.0;
    }
    if (1.0 < output) {
        output = 1.0;
    }
    return (uint16_t)lround(output * 65535.0);
}

// -----------------------------------------------------------------------------
// ------------------------------- Signal Setup --------------------------------
// -----------------------------------------------------------------------------
static void install_signal_handlers(struct app_state *app) {
    if (0 == pipe2(app->signal_pipe, O_CLOEXEC | O_NONBLOCK)) {
        g_signal_write_fd = app->signal_pipe[1];
    } else {
        perror("pipe2");
    }

    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_handler = handle_signal;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    sigaction(SIGINT, &action, NULL);
    sigaction(SIGTERM, &action, NULL);
}

static void handle_signal(int signum) {
    (void)signum;
    if (0 <= g_signal_write_fd) {
        ssize_t written = write(g_signal_write_fd, "x", 1);
        (void)written;
    }
}

static void drain_signal_pipe(int fd) {
    char buffer[16];
    while (0 < read(fd, buffer, sizeof(buffer))) {
    }
    g_stop_requested = 1;
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
