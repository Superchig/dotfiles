return {
  -- Add gruvbox
  {
    "ellisonleao/gruvbox.nvim",
    opts = {
      overrides = {
        LspReferenceText = { bg = "#57514e" },
        LspReferenceRead = { bg = "#57514e" },
        LspReferenceWrite = { bg = "#57514e" },
      },
    },
  },

  -- Eagerly load the tokyonight theme
  {
    "folke/tokyonight.nvim",
    lazy = false,
  },

  {
    "dracula/vim",
  },

  {
    "projekt0n/github-nvim-theme",
  },

  {
    "Mofiqul/vscode.nvim",
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
            vim.o.guifont = "Iosevka Nerd Font:h12"
          end

          return "tokyonight"
        end

        if vim.loop.os_uname().sysname == "Darwin" then
          if vim.env.TERM == "xterm-ghostty" then
            vim.o.background = "dark"
            return "tokyonight"
          elseif vim.env.TERM == "rio" then
            vim.o.background = "dark"
            return "dracula"
          else
            vim.o.background = "light"
          end
          return "vscode"
        elseif vim.loop.os_uname().sysname == "Linux" then
          if vim.env.TERM == "xterm-kitty" then
            return "gruvbox"
          elseif vim.env.TERM == "xterm-ghostty" then
            return "tokyonight-moon"
          end
        else
          return "gruvbox"
        end
      end)(),
    },
  },
}
