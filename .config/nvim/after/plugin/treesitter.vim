if !get(g:, 'loaded_nvim_treesitter')
    finish
endif

lua <<EOF
require('nvim-treesitter.configs').setup {
    highlight = {
        enable = true,
        custom_captures = {
            -- Default treesitter highlighting is way too noisy
            ['parameter'] = 'TSNone',
            ['punctuation.bracket'] = 'TSNone',
            ['punctuation.delimiter'] = 'TSNone',
            ['field'] = 'TSNone',
            ['property'] = 'TSNone',
            ['namespace'] = 'TSNone',
            ['constructor'] = 'TSNone',
            ['function.builtin'] = 'TSFunction',
            ['constant.builtin'] = 'TSConstant',
        },
    },
    refactor = {
        highlight_definitions = { enable = true },
        smart_rename = {
            enable = true,
            keymaps = {
                smart_rename = 'grr',
            },
        },
        navigation = {
            enable = true,
            keymaps = {
                goto_definition = 'gd',
                list_definitions = '<Nop>',
                list_definitions_toc = '<Nop>',
                goto_definition_lsp_fallback = '<C-]>',
                goto_next_usage = '<Nop>',
                goto_previous_usage = '<Nop>',
            },
        },
    },
    playground = {
        enable = true,
    },
    query_linter = {
        enable = true,
        use_virtual_text = true,
        lint_events = {'BufWrite', 'CursorHold'},
    },
}
EOF

hi link TSDefinition CursorLine
hi link TSDefinitionUsage CursorLine
