if not vim.g.loaded_nvim_treesitter then
    return
end

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
            ['function.macro'] = 'TSFunction',
            ['constant.builtin'] = 'TSConstant',
        },
    },
    refactor = {
        highlight_definitions = { enable = true },
        smart_rename = {
            enable = true,
            keymaps = {
                smart_rename = 'g<C-R>',
            },
        },
        navigation = {
            enable = true,
            keymaps = {
                goto_definition = 'gd',
                list_definitions = '<Bslash>d',
                list_definitions_toc = '<Nop>',
                goto_next_usage = ']u',
                goto_previous_usage = '[u',
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
    matchup = {
        enable = true,
    },
}
