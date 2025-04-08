return {
  {
    'CopilotC-Nvim/CopilotChat.nvim',
    dependency = { 'copilot.vim', 'plenary.nvim' },
    build = 'make tiktoken', -- Only on MacOS or Linux
    config = function()
      require('CopilotChat').setup({
        show_help = false,
        model = 'claude-3.5-sonnet',
        window = {
          layout = 'float',
          width = 0.4,
          height = 0.9,
          border = 'rounded',
          row = 1000,  -- bottom
          col = 1000,  -- right
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
            '> #git:staged\n\nWrite one line commit message which starts with a verb (e.g. Fix/Add/Optimize/Arrange). Output only the commit message.',
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
      vim.keymap.set('n', 'H', ':CopilotChatToggle<CR>')
      vim.keymap.set('v', 'H', ':CopilotChat<CR>')

      -- Initial text in chat area
      vim.api.nvim_create_autocmd("BufEnter", {
          pattern = "copilot-*",
          callback = function()
              local row, col = unpack(vim.api.nvim_win_get_cursor(0))
              local line = vim.api.nvim_buf_get_lines(0, row - 1, row, false)[1]
              if col == 0 and line == "" then
                vim.api.nvim_buf_set_text(0, row - 1, col, row - 1, col,
                                          -- {"#files:full: "})
                                          {"#buffers "})
              end
          end,
      })

      -- Key binding for actions
      function ShowCopilotChatActionPrompt()
        local actions = require('CopilotChat.actions')
        require('CopilotChat.integrations.telescope').pick(actions.prompt_actions())
      end

      vim.api.nvim_set_keymap('n', 'C', '<cmd>lua ShowCopilotChatActionPrompt()<cr>',
        { noremap = true, silent = true })
    end
  },
}
