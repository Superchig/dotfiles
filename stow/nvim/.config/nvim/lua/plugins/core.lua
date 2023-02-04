return {
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "flake8",
        "json-lsp",
        "lua-language-server",
        "prettier",
        "rust-analyzer",
        "shellcheck",
        "shfmt",
        "stylua",
        "svelte-language-server",
        "tailwindcss-language-server",
        "typescript-language-server",
      },
    },
  },

  -- Good old vim-surround
  {
    "tpope/vim-surround",
  },

  {
    "Superchig/vim-markdown",
    config = function()
      vim.g.vim_markdown_new_list_item_indent = 2
      vim.g.vim_markdown_folding_style_pythonic = 1
      vim.g.vim_markdown_folding_level = 6
      -- Disable concealing of markdown syntax
      vim.g.vim_markdown_conceal = 0
      vim.g.vim_markdown_folding_disabled = 1

      vim.cmd([[autocmd Filetype markdown setlocal nospell]])
    end,
  },

  {
    "echasnovski/mini.indentscope",
    -- Modified from https://www.lazyvim.org/plugins/ui#miniindentscope
    opts = {
      draw = {
        delay = 0,
        animation = require("mini.indentscope").gen_animation.none(),
      },
    },
  },
}
