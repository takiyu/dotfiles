// Rich notification plugin with project details
import path from 'path';

const APP_NAME = 'opencode';
const FALLBACK_TITLE = 'OpenCode';
const ICON = 'dialog-information';
const ICON_ERROR = 'dialog-error';
const URGENCY_NORMAL = 'normal';
const URGENCY_CRITICAL = 'critical';

// ---------------------------------------------------------------------------
// ------------------------------- Interfaces ----------------------------------
// ---------------------------------------------------------------------------
// Build a human-readable summary of the current project context.
function buildProjectSubtitle(project, directory, worktree) {
    const parts = [];
    if (project && project.name) {
        parts.push(`Project: ${project.name}`);
    }
    if (worktree) {
        parts.push(`Worktree: ${worktree}`);
    }
    if (directory) {
        parts.push(`Dir: ${directory}`);
    }
    return parts.length > 0 ? parts.join(' | ') : '';
}

// ---------------------------------------------------------------------------
// Send a desktop notification via `notify-send`.
async function sendNotification($, title, status, urgency, subtitle) {
    const args = [
        '-a', APP_NAME,
        '-u', urgency,
        '-i', urgency === URGENCY_CRITICAL ? ICON_ERROR : ICON,
    ];
    const body = subtitle ? `${status}\n${subtitle}` : status;
    try {
        await $`notify-send ${args} ${title} ${body}`;
    } catch (_e) {
        // silent fail — notification daemon may be unavailable
    }
}

// ---------------------------------------------------------------------------
// ------------------------------ Implementation -------------------------------
// ---------------------------------------------------------------------------
export const NotificationPlugin = async (
    { project, $, directory, worktree }
) => {
    const subtitle = buildProjectSubtitle(project, directory, worktree);
    // Use directory basename as project name when project.name is unavailable.
    const dirName = directory ? path.basename(directory) : null;
    const title = project && project.name
        ? project.name
        : (dirName || FALLBACK_TITLE);

    return {
        event: async ({ event }) => {
            if (event.type === 'session.idle') {
                const status = event.message
                    || 'The agent has finished and is now idle.';
                await sendNotification(
                    $,
                    title,
                    status,
                    URGENCY_NORMAL,
                    subtitle
                );
            }

            if (event.type === 'session.error') {
                const status = event.message
                    || event.error
                    || 'An error occurred during the session.';
                await sendNotification(
                    $,
                    title,
                    status,
                    URGENCY_CRITICAL,
                    subtitle
                );
            }
        },
    };
};
