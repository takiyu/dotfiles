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
  ------------------------------------------------------------------------------
  ------------------------------------------------------------------------------
}
