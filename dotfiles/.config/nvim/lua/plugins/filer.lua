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

  local function with_close(open_func)
    return function()
      open_func()
      api.tree.close()
    end
  end

  local function tab_open(open_func)
    api.node.open.tab()
    vim.cmd('tabprevious')
    api.tree.close()
    vim.cmd('tabnext')
  end

  -- Default mappings
  -- api.config.mappings.default_on_attach(bufnr)

  -- Custom mappings
  vim.keymap.set('n', '<CR>',  with_close(api.node.open.edit),       opts('Open'))
  vim.keymap.set('n', 'o',     with_close(api.node.open.edit),       opts('Open'))
  vim.keymap.set('n', 't',     tab_open,                             opts('Open: New Tab'))
  vim.keymap.set('n', 'i',     with_close(api.node.open.horizontal), opts('Open: Horizontal Split'))
  vim.keymap.set('n', 's',     with_close(api.node.open.vertical),   opts('Open: Vertical Split'))

  vim.keymap.set('n', 'l',     api.node.open.drop,                   opts('Open: Tree'))
  vim.keymap.set('n', 'h',     api.node.navigate.parent_close,       opts('Close'))
  vim.keymap.set('n', 'u',     api.tree.change_root_to_parent,       opts('Up'))
  vim.keymap.set('n', 'cd',    api.tree.change_root_to_node,         opts('CD'))

  vim.keymap.set('n', 'R',     api.tree.reload,                      opts('Refresh'))
  vim.keymap.set('n', 'f',     api.tree.search_node,                 opts('Search'))
  vim.keymap.set('n', 'I',     api.tree.toggle_hidden_filter,        opts('Toggle Filter: Dotfiles'))
  vim.keymap.set('n', '?',     api.tree.toggle_help,                 opts('Help'))
  vim.keymap.set('n', 'q',     api.tree.close,                       opts('Close'))

  vim.keymap.set('n', '<DEL>', api.fs.remove,                        opts('Delete'))
  vim.keymap.set('n', 'd',     api.fs.remove,                        opts('Delete'))
  vim.keymap.set('n', 'r',     api.fs.rename,                        opts('Rename'))
  vim.keymap.set('n', 'a',     api.fs.create,                        opts('Create'))
  vim.keymap.set('n', 'y',     api.fs.copy.node,                     opts('Copy'))
  vim.keymap.set('n', 'x',     api.fs.cut,                           opts('Cut'))
  vim.keymap.set('n', 'p',     api.fs.paste,                         opts('Paste'))

end

return {
  'nvim-tree/nvim-tree.lua',
  config = function()
    -- Setup
    require("nvim-tree").setup{
      on_attach = OnAttach,
      filters = {
        git_ignored = false,
        dotfiles = true,
      },
    }
    -- Key maps
    vim.api.nvim_set_keymap('', '<C-e>', ':NvimTreeToggle<CR>',  {noremap = true})
    vim.api.nvim_set_keymap('', '<C-f>', ':NvimTreeFindFileToggle<CR>',  {noremap = true})
  end
}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
