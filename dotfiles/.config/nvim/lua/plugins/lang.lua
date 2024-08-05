return {
  ------------------------------------------------------------------------------
  ---------------------------------- Syntaxes ----------------------------------
  ------------------------------------------------------------------------------
  {'vim-jp/cpp-vim',
   ft = {'cpp'}
  },
  {'tikhomirov/vim-glsl',
   ft = {'glsl'}
  },
  {'elzr/vim-json',
   ft = {'json'},
   init = function()
    vim.g.vim_json_syntax_conceal = 0
   end
  },
  {'gutenye/json5.vim',
   ft = {'json5'}
  },
  {'neoclide/jsonc.vim',
   ft = {'jsonc'}
  },
  {'cespare/vim-toml',
   ft = {'toml'}
  },
  {'othree/yajs.vim',
   ft = {'javascript'}
  },
  {'vim-scripts/verilog.vim',
   ft = {'verilog'}
  },
  {'udalov/kotlin-vim',
   ft = {'kotlin'}
  },

  ------------------------------------------------------------------------------
  ---------------------------- Document Generation -----------------------------
  ------------------------------------------------------------------------------
  {'vim-scripts/DoxygenToolkit.vim',
   ft = {'c', 'cpp'}
  },
  {'heavenshell/vim-pydocstring',
   ft = {'python'},
   build = 'make',
   init = function()
    vim.g.pydocstring_enable_mapping=0
   end
  },

  ------------------------------------------------------------------------------
  --------------------------------- Hex Editor ---------------------------------
  ------------------------------------------------------------------------------
  {'fidian/hexmode',
   init = function()
    vim.g.hexmode_patterns = '*.bin,*.exe,*.dat,*.o,*.DAT,*.NAT'
    vim.g.hexmode_xxd_options = '-g 2'  -- xxd option: 2 bytes
   end,
   config = function()
    vim.keymap.set('n', '<F8>', ':lua vim.g.hexmode_xxd_options = "-g 2"<CR>:e<CR>')      -- Mode: Binary
    vim.keymap.set('n', '<F9>', ':lua vim.g.hexmode_xxd_options = "-b -c 16"<CR>:e<CR>')  -- Mode: Hex
   end
  },

  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
}
