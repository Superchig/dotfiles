return {
  -- Add gruvbox
  {
    "ellisonleao/gruvbox.nvim",
    opts = {
      overrides = {
        LspReferenceText = { bg = "#57514e" },
        LspReferenceRead = { bg = "#57514e" },
        LspReferenceWrite = { bg = "#57514e" },
        ["@comment"] = { link = "GruvboxYellowBold" },
      },
    },
  },

  -- Eagerly load the tokyonight theme
  {
    "folke/tokyonight.nvim",
    lazy = false,
    config = function()
      --- From https://github.com/folke/tokyonight.nvim/discussions/685
      local styles = require("tokyonight.colors").styles
      styles.moon = vim.tbl_extend("force", styles.moon --[[@as Palette]], {
        customized_style = "moon",
      })
      require("tokyonight").setup({
        on_highlights = function(highlights, colors)
          local style_name = colors["customized_style"]
          if style_name == "moon" then
            highlights.Comment = {
              fg = "#ffd8ab",
              bold = true,
            }
          end
        end,
      })
    end,
  },

  {
    "Mofiqul/dracula.nvim",
    opts = {
      overrides = {
        Comment = { fg = "#FFFFA5", bold = true },
      },
    },
  },

  {
    "projekt0n/github-nvim-theme",
  },

  {
    "Mofiqul/vscode.nvim",
  },

  {
    "p00f/alabaster.nvim",
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

          -- To see possible fonts, use `set guifont=*`
          if jit.os == "Linux" then
            vim.o.guifont = "Iosevka Nerd Font,Inconsolata:h12"
          elseif jit.os == "OSX" then
            vim.o.guifont = "PragmataProComfy Mono Liga:h16"
          else
            vim.o.guifont = "Iosevka Nerd Font:h12"
          end

          return "tokyonight"
        end

        if vim.env.TERM == "linux" then
          vim.o.termguicolors = false

          return "vim"
        end

        if jit.os == "OSX" then
          if vim.env.TERM == "xterm-ghostty" then
            vim.o.background = "dark"
            return "tokyonight-moon"
          elseif vim.env.TERM == "rio" then
            vim.o.background = "dark"
            return "dracula"
          else
            vim.o.background = "light"
            return "vscode"
          end
        elseif jit.os == "Linux" then
          if vim.env.TERM == "xterm-kitty" then
            if vim.env.XDG_CURRENT_DESKTOP == "KDE" then
              return "dracula"
            else
              return "gruvbox"
            end
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
