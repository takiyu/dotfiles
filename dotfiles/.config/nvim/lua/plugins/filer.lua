--------------------------------------------------------------------------------
------------------------------------ Filer -------------------------------------
--------------------------------------------------------------------------------
-- Disable netrw at the very start
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Mapping in Filer
local function OnAttach(bufnr)
  local api = require "nvim-tree.api"
  local function opts(desc)
    return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true,
             silent = true, nowait = true }
  end

  -- Default mappings
  -- api.config.mappings.default_on_attach(bufnr)

  -- Custom mappings
  vim.keymap.set('n', '<C-t>', api.tree.change_root_to_parent,  opts('Up'))
  vim.keymap.set('n', '?',     api.tree.toggle_help,            opts('Help'))
end

return {
  'nvim-tree/nvim-tree.lua',
  config = function()
    -- Setup
    require("nvim-tree").setup{
      on_attach = OnAttach,
    }
    -- Key maps
    vim.api.nvim_set_keymap('', '<C-e>', ':NvimTreeToggle<CR>',  {noremap = true})
    vim.api.nvim_set_keymap('', '<C-f>', ':NvimTreeFindFileToggle<CR>',  {noremap = true})
  end
}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
