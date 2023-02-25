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

  -- Add prettier to our list of null-ls formatting sources
  -- List of sources at https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
  {
    "jose-elias-alvarez/null-ls.nvim",
    opts = function()
      local null_ls = require("null-ls")
      return {
        sources = {
          null_ls.builtins.formatting.prettier,
        },
      }
    end,
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      -- Automatically format on save or not
      autoformat = false
    }
  },

  -- TODO(Chris): Create an override.lua file for overriding existing plugin configs
  {
    "RRethy/vim-illuminate",
    keys = {
      { "]]", "<Plug>Markdown_MoveToNextHeader", desc = "Next Header", },
      { "[[", "<Plug>Markdown_MoveToPreviousHeader", desc = "Prev Header" },
    }
  },

  {
    "RRethy/vim-illuminate",
    keys = {
      { "]]", "<Plug>Markdown_MoveToNextHeader", desc = "Next Header", },
      { "[[", "<Plug>Markdown_MoveToPreviousHeader", desc = "Prev Header" },
    }
  },

  {
    "hallison/vim-rdoc"
  },

  {
    "earthly/earthly.vim",
    config = function()
      vim.cmd([[autocmd Filetype Earthfile setlocal shiftwidth=4]])
    end,
  }
}
