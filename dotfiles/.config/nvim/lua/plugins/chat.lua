return {
  {
    'CopilotC-Nvim/CopilotChat.nvim',
    dependency = { 'copilot.vim', 'plenary.nvim' },
    build = 'make tiktoken', -- Only on MacOS or Linux
    config = function()
      require('CopilotChat').setup({
        show_help = false,
        window = {
          layout = 'vertical',
          width = 0.4,
          height = 0.4,
          border = 'rounded',
        },
        auto_insert_mode = false,
        auto_follow_cursor = true,
        prompts = {
          Explain = {
            prompt = '/COPILOT_EXPLAIN コードを日本語で説明してください',
          },
          Review = {
            prompt = '> /COPILOT_REVIEW\n\nReview the selected code.',
          },
          Fix = {
            prompt =
            '> /COPILOT_GENERATE\n\nThere is a problem in this code. Rewrite the code to show it with the bug fixed.',
          },
          Optimize = {
            prompt = '> /COPILOT_GENERATE\n\nOptimize the selected code to improve performance and readability.',
          },
          Docs = {
            prompt = '> /COPILOT_GENERATE\n\nPlease add documentation comments to the selected code.',
          },
          Tests = {
            prompt = '> /COPILOT_GENERATE\n\nPlease generate tests for my code.',
          },
          Commit = {
            prompt =
            '> #git:staged\n\nWrite one line commit message which starts with a verb (e.g. Fix/Add/Optimize/Arrange)',
          },
        },

        mappings = {
          complete = {
            insert = '<Tab>',
          },
          close = {
            normal = '<C-c>',
            insert = '<C-c>',
          },
          reset = {
            normal = 'R',
          },
          submit_prompt = {
            normal = '<Space>',
            insert = '<C-s>',
          },
          toggle_sticky = {
            detail = 'Makes line under cursor sticky or deletes sticky line.',
            normal = 'gr',
          },
          accept_diff = {
            normal = '<C-y>',
            insert = '<C-y>',
          },
          jump_to_diff = {
            -- normal = 'gj',
            normal = '<leader>j',
          },
          quickfix_diffs = {
            normal = 'gl',
          },
          show_diff = {
            normal = 'gd',
          },
          show_info = {
            normal = 'gi',
          },
          show_help = {
            normal = 'gh',
          },
        }
      })

      -- Key binding
      vim.keymap.set('n', 'C', ':CopilotChatToggle<CR>')
      vim.keymap.set('v', 'C', ':CopilotChat<CR>')

      -- Key binding for actions
      function ShowCopilotChatActionPrompt()
        local actions = require('CopilotChat.actions')
        require('CopilotChat.integrations.telescope').pick(actions.prompt_actions())
      end

      vim.api.nvim_set_keymap('n', 'H', '<cmd>lua ShowCopilotChatActionPrompt()<cr>',
        { noremap = true, silent = true })
    end
  },
}
