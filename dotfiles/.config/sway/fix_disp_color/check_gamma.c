// -----------------------------------------------------------------------------
// ----------------------- DRM gamma LUT state checker -------------------------
// -----------------------------------------------------------------------------
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <xf86drm.h>
#include <xf86drmMode.h>

// -----------------------------------------------------------------------------
// ------------------------------ Implementation -------------------------------
// -----------------------------------------------------------------------------
static uint32_t connector_crtc(int fd, drmModeConnectorPtr conn) {
    if (0 == conn->encoder_id) {
        return 0;
    }
    drmModeEncoderPtr encoder = drmModeGetEncoder(fd, conn->encoder_id);
    if (NULL == encoder) {
        return 0;
    }
    uint32_t crtc_id = encoder->crtc_id;
    drmModeFreeEncoder(encoder);
    return crtc_id;
}

static void print_crtc_gamma(int fd, uint32_t crtc_id) {
    drmModeObjectPropertiesPtr props =
        drmModeObjectGetProperties(fd, crtc_id, DRM_MODE_OBJECT_CRTC);
    if (NULL == props) {
        return;
    }

    uint64_t gamma_lut = 0;
    uint64_t gamma_size = 0;
    for (uint32_t i = 0; i < props->count_props; i++) {
        drmModePropertyPtr prop = drmModeGetProperty(fd, props->props[i]);
        if (NULL == prop) {
            continue;
        }
        if (0 == strcmp(prop->name, "GAMMA_LUT")) {
            gamma_lut = props->prop_values[i];
        }
        if (0 == strcmp(prop->name, "GAMMA_LUT_SIZE")) {
            gamma_size = props->prop_values[i];
        }
        drmModeFreeProperty(prop);
    }
    printf(" gamma_lut=%llu gamma_size=%llu", (unsigned long long)gamma_lut,
           (unsigned long long)gamma_size);
    drmModeFreeObjectProperties(props);
}

int main(void) {
    int fd = open("/dev/dri/card1", O_RDWR);
    if (0 > fd) {
        perror("open /dev/dri/card1");
        return 1;
    }
    drmModeResPtr res = drmModeGetResources(fd);
    if (NULL == res) {
        perror("drmModeGetResources");
        close(fd);
        return 1;
    }

    for (int i = 0; i < res->count_connectors; i++) {
        drmModeConnectorPtr conn =
            drmModeGetConnectorCurrent(fd, res->connectors[i]);
        if (NULL == conn) {
            continue;
        }
        const char *type_name =
            drmModeGetConnectorTypeName(conn->connector_type);
        if (NULL == type_name) {
            type_name = "unknown";
        }
        printf("%s-%u status=%s", type_name, conn->connector_type_id,
               DRM_MODE_CONNECTED == conn->connection ? "connected" :
                                                        "disconnected");
        uint32_t crtc_id = connector_crtc(fd, conn);
        printf(" crtc=%u", crtc_id);
        if (0 != crtc_id) {
            print_crtc_gamma(fd, crtc_id);
        }
        printf("\n");
        drmModeFreeConnector(conn);
    }

    drmModeFreeResources(res);
    close(fd);
    return 0;
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
