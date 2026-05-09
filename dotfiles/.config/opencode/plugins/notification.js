export const NotificationPlugin = async ({ project, client, $, directory, worktree }) => {
  return {
    event: async ({ event }) => {
      // Send notification on session completion
      if (event.type === 'session.idle') {
        try {
          const title = project ? project.name : 'OpenCode';
          await $`notify-send -a opencode ${title} 'Session completed!'`;
        } catch (e) {
          // silent fail
        }
      }
      // Send notification on session error
      if (event.type === 'session.error') {
        try {
          const title = project ? project.name : 'OpenCode';
          await $`notify-send -a opencode -u critical ${title} 'An error occurred!'`;
        } catch (e) {
          // silent fail
        }
      }
    },
  };
};
