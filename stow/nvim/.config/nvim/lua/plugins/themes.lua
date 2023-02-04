return {
  -- Add gruvbox
  { "ellisonleao/gruvbox.nvim" },

  -- Eagerly load the tokyonight theme
  {
    "folke/tokyonight.nvim",
    lazy = false,
  },

  -- Configure LazyVim to load gruvbox
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "gruvbox",
    },
  },
}
