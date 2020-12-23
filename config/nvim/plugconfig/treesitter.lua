require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
  },
}

require'nvim-treesitter.configs'.setup {
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
}

require'nvim-treesitter.configs'.setup {
  indent = {
    enable = true
  }
}

-- Custom Parsers

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.vue = {
  install_info = {
    url = "~/dev/treesitter/tree-sitter-vue", -- local path or git repo
    files = {"src/parser.c"}
  },
}
