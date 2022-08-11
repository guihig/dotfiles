local cmp = require 'cmp'
local types = require 'cmp.types'
local lspkind = require('lspkind')
local format = require("cmp_git.format")
local sort = require("cmp_git.sort")

local get_all_buffers = function() return vim.api.nvim_list_bufs() end

-- https://www.youtube.com/watch?v=_DnmphIwnjo&t=1514s
-- https://github.com/tjdevries/config_manager/blob/master/xdg_config/nvim/after/plugin/completion.lua
cmp.setup({
    snippet = {expand = function(args) vim.fn["vsnip#anonymous"](args.body) end},
    -- completion = {completeopt = 'menu,menuone,noinsert'},
    mapping = {
        ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), {'i', 'c'}),
        ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), {'i', 'c'}),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), {'i', 'c'}),
        ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
        ['<C-e>'] = cmp.mapping({
            i = cmp.mapping.abort(),
            c = cmp.mapping.close()
        }),
        ['<C-n>'] = cmp.mapping(cmp.mapping.select_next_item(
                                    {behavior = types.cmp.SelectBehavior.Insert}),
                                {'i', 'c'}),
        ['<C-p>'] = cmp.mapping(cmp.mapping.select_prev_item(
                                    {behavior = types.cmp.SelectBehavior.Insert}),
                                {'i', 'c'}),
        ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = false
        }) -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    },
    sources = cmp.config.sources({
        {name = 'nvim_lsp', keyword_length = 0, priority = 80},
        {name = 'nvim_lua', priority = 70}, {name = 'vsnip', priority = 60},
        {name = "git", priority = 50}, {name = "path", priority = 40},
        {name = 'emoji', priority = 30}, {
            name = "buffer",
            option = {
                get_bufnrs = get_all_buffers,
                keyword_pattern = [[\k\+]] -- Include special characters in word match.
            },
            keyword_length = 5,
            priority = 20
        }
    }),
    formatting = {
        format = lspkind.cmp_format({
            with_text = true,
            menu = ({
                buffer = "[Buffer]",
                nvim_lsp = "[LSP]",
                -- luasnip = "[LuaSnip]",
                vsnip = "[VSnip]",
                nvim_lua = "[Lua]",
                emoji = "[Emoji]",
                path = "[Path]",
                git = "[Git]",
                latex_symbols = "[Latex]"
            })
        })
    },
    completion = {keyword_length = 2},
    experimental = {native_menu = false, ghost_text = true}
})

require("cmp_git").setup({
    -- defaults
    filetypes = {"gitcommit", "octo"},
    remotes = {"upstream", "origin"}, -- in order of most to least prioritized
    enableRemoteUrlRewrites = false, -- enable git url rewrites, see https://git-scm.com/docs/git-config#Documentation/git-config.txt-urlltbasegtinsteadOf
    git = {
        commits = {
            limit = 100,
            sort_by = sort.git.commits,
            format = format.git.commits
        }
    },
    github = {
        issues = {
            fields = {"title", "number", "body", "updatedAt", "state"},
            filter = "all", -- assigned, created, mentioned, subscribed, all, repos
            limit = 100,
            state = "open", -- open, closed, all
            sort_by = sort.github.issues,
            format = format.github.issues
        },
        mentions = {
            limit = 100,
            sort_by = sort.github.mentions,
            format = format.github.mentions
        },
        pull_requests = {
            fields = {"title", "number", "body", "updatedAt", "state"},
            limit = 100,
            state = "open", -- open, closed, merged, all
            sort_by = sort.github.pull_requests,
            format = format.github.pull_requests
        }
    },
    gitlab = {
        issues = {
            limit = 100,
            state = "all", -- opened, closed, all
            sort_by = sort.gitlab.issues,
            format = format.gitlab.issues
        },
        mentions = {
            limit = 100,
            sort_by = sort.gitlab.mentions,
            format = format.gitlab.mentions
        },
        merge_requests = {
            limit = 100,
            state = "all", -- opened, closed, locked, merged
            sort_by = sort.gitlab.merge_requests,
            format = format.gitlab.merge_requests
        }
    },
    trigger_actions = {
        {
            debug_name = "git_commits",
            trigger_character = ":",
            action = function(sources, trigger_char, callback, params, git_info)
                return sources.git:get_commits(callback, params, trigger_char)
            end
        }, {
            debug_name = "gitlab_issues",
            trigger_character = "#",
            action = function(sources, trigger_char, callback, params, git_info)
                return sources.gitlab:get_issues(callback, git_info,
                                                 trigger_char)
            end
        }, {
            debug_name = "gitlab_mentions",
            trigger_character = "@",
            action = function(sources, trigger_char, callback, params, git_info)
                return sources.gitlab:get_mentions(callback, git_info,
                                                   trigger_char)
            end
        }, {
            debug_name = "gitlab_mrs",
            trigger_character = "!",
            action = function(sources, trigger_char, callback, params, git_info)
                return sources.gitlab:get_merge_requests(callback, git_info,
                                                         trigger_char)
            end
        }, {
            debug_name = "github_issues_and_pr",
            trigger_character = "#",
            action = function(sources, trigger_char, callback, params, git_info)
                return sources.github:get_issues_and_prs(callback, git_info,
                                                         trigger_char)
            end
        }, {
            debug_name = "github_mentions",
            trigger_character = "@",
            action = function(sources, trigger_char, callback, params, git_info)
                return sources.github:get_mentions(callback, git_info,
                                                   trigger_char)
            end
        }
    }
})
