local vimscript_code = [[
" ==============================================================================
"                                  nvim-treesitter
" ==============================================================================
lua <<EOF
local status, treesitter = pcall(require, "nvim-treesitter.configs")
if (not status) then
    return
end

require'nvim-treesitter.configs'.setup {
    highlight = {
        enable = true,
        disable = {},
        additional_vim_regex_highlighting = false,
    },
    indent = {
        enable = true,
        disable = {},
    },
    ensure_installed = {
        "bash",
        "bibtex",
        "c",
        "c_sharp",
        "cmake",
        "comment",
        "cpp",
        "css",
        "cuda",
        "dart",
        "dockerfile",
        "glsl",
        "go",
        "haskell",
        "html",
        "http",
        "java",
        "javascript",
        "jsdoc",
        "json",
        "json5",
        "kotlin",
        "latex",
        "llvm",
        "lua",
        "make",
        "markdown",
        "perl",
        "php",
        "python",
        "rust",
        "scss",
        "toml",
        "typescript",
        "verilog",
        "vim",
        "vue",
        "yaml",
    }
}
EOF

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
]]

return {}
