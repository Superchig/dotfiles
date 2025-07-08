return {
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

  -- TODO(Chris): Create an override.lua file for overriding existing plugin configs
  {
    "RRethy/vim-illuminate",
    keys = {
      { "]]", "<Plug>Markdown_MoveToNextHeader", desc = "Next Header" },
      { "[[", "<Plug>Markdown_MoveToPreviousHeader", desc = "Prev Header" },
    },
  },

  {
    "RRethy/vim-illuminate",
    keys = {
      { "]]", "<Plug>Markdown_MoveToNextHeader", desc = "Next Header" },
      { "[[", "<Plug>Markdown_MoveToPreviousHeader", desc = "Prev Header" },
    },
  },

  {
    "hallison/vim-rdoc",
  },

  {
    "earthly/earthly.vim",
    config = function()
      vim.cmd([[autocmd Filetype Earthfile setlocal shiftwidth=4]])
    end,
  },

  {
    "ChrisWellsWood/roc.vim",
  },

  {
    "Superchig/zig-watch.nvim",
    dir = "~/dotfiles/stow/nvim/.config/nvim/custom-plugins/zig-watch.nvim",
    dev = true,
    config = {},
  },
}
