if not vim.g.loaded_nvim_treesitter then
    return
end

require('nvim-treesitter.configs').setup {
    highlight = {
        enable = true,
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "gV",
            node_incremental = "<CR>",
            node_decremental = "<BS>",
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
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
            }
        },
        swap = {
            enable = true,
            swap_next = {
                [">a"] = "@parameter.inner",
            },
            swap_previous = {
                ["<a"] = "@parameter.inner",
            }
        }
    }
}
