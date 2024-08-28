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
    vim.g.hexmode_patterns = '*.bin,*.exe,*.dat,*.o,*.DAT,*.NAT,*.mwf,*.MWF,'
    vim.g.hexmode_xxd_options = '-g 2'  -- xxd option: 2 bytes
   end,
   config = function()
     vim.g.is_hexmode_binary = false
     vim.keymap.set('n', '<F7>', function()
       if vim.g.is_hexmode_binary then
         -- Hex mode
        print("Editer mode: Hex")
         vim.g.hexmode_xxd_options = "-g 2"
         vim.cmd('e')
         vim.g.is_hexmode_binary = false
       else
         -- Binary mode
         print("Editer mode: Binary")
         vim.g.hexmode_xxd_options = "-b -c 16"
         vim.cmd('e')
         vim.g.is_hexmode_binary = true
       end
     end)
   end
  },

  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
}
