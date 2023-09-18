-- Global configuration
vim.g.lightline = {
  enable = {
    statusline = 1,
    tabline = 1
  },
  colorscheme = 'Tomorrow_Night_Bright',
--   component = {
--     readonly = function()
--       return vim.fn.getbufvar(vim.fn.bufnr('%'), '&readonly') == 1 and 'R' or 'W'
--     end,
--     percentlineinfo = '[%3p%% %3l:%-2v]',
--     fileinfo = function()
--       return vim.fn.expand('&fileencoding') .. '  ' .. vim.fn.expand('&fileformat') .. '  ' .. vim.fn.expand('&filetype')
--     end,
--     asyncrun = '%{g:asyncrun_status}',
--   },
--   component_function = {
--     gitstatus = 'LightlineGitStatus',
--   },
--   separator = { left = '', right = '' },
--   subseparator = { left = '', right = '' },
}

-- -- Status line configuration
-- vim.g.lightline.active = {
--   left = { { 'mode' }, { 'readonly', 'filename', 'modified' } },
--   right = { { 'percentlineinfo' }, { 'fileinfo' }, { 'gitstatus' }, { 'lsp_status' }, { 'lsp_info', 'lsp_hints', 'lsp_errors', 'lsp_warnings', 'lsp_ok' }, { 'asyncrun' } },
-- }
-- 
-- -- Tab line configuration
-- vim.g.lightline.tabline = {
--   left = {{'tabs'}},
--   right = {}
-- }
-- 
-- -- Neovim-LSP components
-- -- require'lightline'.lsp.register()
-- 
-- -- Git状態のステータスライン表示
-- function LightlineGitStatus()
--     if vim.fn.winwidth('.') <= 60 then
--         return ''
--     end
--     local ret = {}
--     local ok, gitgutter_enabled = pcall(vim.fn['GitGutterGetHunkSummary'])
--     if ok and gitgutter_enabled then
--         local symbols = { '++', '-+', '--' }
--         local hunks = vim.fn['GitGutterGetHunkSummary']()
--         for i = 1, 3 do
--             if hunks[i] > 0 then
--                 table.insert(ret, symbols[i] .. hunks[i])
--             end
--         end
--     end
--     local mark = ''  -- edit here for cool mark
--     local branch = vim.fn['fugitive#head']()
--     if branch ~= '' then
--         table.insert(ret, mark .. '[' .. branch .. ']')
--     end
--     return table.concat(ret, ' ')
-- end



return {
  {'itchyny/lightline.vim',
--     config = function()
      -- Update events
--       vim.cmd("autocmd TextChanged * call lightline#update()")
--       vim.cmd("autocmd TextChangedI * call lightline#update()")
--       vim.cmd("autocmd CursorHold * call lightline#update()")
--       vim.cmd("autocmd CursorHoldI * call lightline#update()")
--     end,
  },
--   {'josa42/nvim-lightline-lsp'},        -- Status line for lsp
}
