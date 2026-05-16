// Rich notification plugin with project details
import path from 'path';

const APP_NAME = 'opencode';
const FALLBACK_TITLE = 'OpenCode';
const ICON = 'dialog-information';
const ICON_COMPLETE = 'emblem-default';
const ICON_ERROR = 'dialog-error';
const ICON_PERMISSION = 'dialog-question';
const URGENCY_NORMAL = 'normal';
const URGENCY_CRITICAL = 'critical';

const SOUND_DIR = '/usr/share/sounds/freedesktop/stereo';

// ---------------------------------------------------------------------------
// ------------------------------- Interfaces ----------------------------------
// ---------------------------------------------------------------------------
// Send a desktop notification via `notify-send`.
async function sendNotification($, title, status, urgency, description,
                                iconOverride, soundFile) {
    const icon = iconOverride
        || (urgency === URGENCY_CRITICAL ? ICON_ERROR : ICON);
    const args = [
        '-a', APP_NAME,
        '-u', urgency,
        '-i', icon,
    ];
    const body = description
        ? `${status}\n${description}`
        : status;
    try {
        await $`notify-send ${args} ${title} ${body} 2>/dev/null`;
    } catch (_e) {
        // silent fail — notification daemon may be unavailable
    }

    if (soundFile) {
        try {
            await $`paplay ${soundFile} 2>/dev/null`;
        } catch (_e) {
            // silent fail — audio may be unavailable
        }
    }
}

// ---------------------------------------------------------------------------
// ------------------------------ Implementation -------------------------------
// ---------------------------------------------------------------------------
export const NotificationPlugin = async (
    { project, $, directory }
) => {
    const description = project && project.description
        ? project.description
        : '';
    // Use directory basename as project name when project.name is unavailable.
    const dirName = directory ? path.basename(directory) : null;
    const title = project && project.name
        ? project.name
        : (dirName || FALLBACK_TITLE);

    return {
        event: async ({ event }) => {
            if (event.type === 'session.idle') {
                const status = event.message
                    || 'Session completed';
                await sendNotification(
                    $,
                    title,
                    status,
                    URGENCY_NORMAL,
                    description,
                    ICON_COMPLETE,
                    `${SOUND_DIR}/complete.oga`
                );
            }

            if (event.type === 'session.error') {
                const status = event.message
                    || event.error
                    || 'An error occurred';
                await sendNotification(
                    $,
                    title,
                    status,
                    URGENCY_CRITICAL,
                    description,
                    null,
                    `${SOUND_DIR}/dialog-error.oga`
                );
            }

            if (event.type === 'permission.asked') {
                const status = event.message
                    || 'Task paused — permission requested';
                await sendNotification(
                    $,
                    title,
                    status,
                    URGENCY_NORMAL,
                    description,
                    ICON_PERMISSION
                );
            }
        },
    };
};
