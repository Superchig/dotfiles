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
      -- NOTE(Chris): This anonymous function returns the name of the
      -- colorscheme we want to use, and it may set the font or other
      -- UI settings.
      colorscheme = (function()
        if vim.g.neovide then
          -- vim.g.neovide_transparency = 0.8

          if vim.loop.os_uname().sysname == "Linux" then
            vim.o.guifont = "Iosevka Nerd Font:h12,Consolas:h12,Inconsolata:h12"
          else
            vim.o.guifont = "Iosevka Term:h12,Consolas:h12,Inconsolata:h12"
          end

          return "tokyonight"
        end

        if vim.loop.os_uname().sysname == "Darwin" then
          vim.o.background = "light"
          return "one-nvim"
        else
          return "gruvbox"
        end
      end)(),
    },
  },
}
