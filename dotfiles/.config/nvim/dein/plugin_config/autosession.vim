" ==============================================================================
"                                    Auto session
" ==============================================================================
lua << EOF
require("autosession").setup({
    msg = nil,
    restore_on_setup = true,
    autosave_on_quit = true,
    force_autosave = true,
    sessionfile_name = ".session.vim",
})
EOF
