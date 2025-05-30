return {
  -- The next three plugins modify the behavior of f and s
  { "ggandor/leap.nvim", enabled = false },
  { "ggandor/flit.nvim", enabled = false },
  { "folke/flash.nvim", enabled = false },
  -- I haven't taken the time to rebind this plugin to correspond to the
  -- vim-commentary mappings
  -- { "echasnovski/mini.surround", enabled = false },
  -- I've never gotten used to
  -- { "akinsho/bufferline.nvim", enabled = false },
  -- As of 2/4/2023, this doesn't work well with Neovide
  { "folke/noice.nvim", enabled = false },
  -- As of 2/4/2023, this doesn't support highlighting the author in TODO(author):
  -- https://github.com/folke/todo-comments.nvim/issues/10
  { "folke/todo-comments.nvim", enabled = false },
  -- { "RRethy/vim-illuminate", enabled = false },

  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      local keys = require("lazyvim.plugins.lsp.keymaps").get()

      keys[#keys + 1] = { "K", false }
      keys[#keys + 1] = { "J", false }

      opts.setup.tailwindcss = function(_, tailwindcss_opts)
        local default_filetypes = require("lspconfig").tailwindcss.config_def.default_config.filetypes
        tailwindcss_opts.filetypes = { "ruby" }
        vim.list_extend(tailwindcss_opts.filetypes, default_filetypes)

        tailwindcss_opts.settings = {
          tailwindCSS = {
            includeLanguages = {
              ruby = "html",
            },
            experimental = {
              classRegex = {
                [[class= "([^"]*)]],
                [[class: "([^"]*)]],
                [[class= '([^']*)]],
                [[class: '([^']*)]],
              },
            },
          },
        }
      end

      if vim.loop.os_uname().sysname == "Darwin" then
        opts.servers.clangd = {
          cmd = { "/opt/homebrew/opt/llvm@20/bin/clangd", "-header-insertion=never" },
        }
      else
        opts.servers.clangd = {
          cmd = { "clangd", "-header-insertion=never" },
        }
      end

      -- if require("mason-registry").is_installed("slang")
      opts.servers.slangd = {
        settings = {
          slang = {
            inlayHints = {
              deducedTypes = false,
              parameterNames = false,
            },
          },
        },
      }
    end,
  },

  {
    -- Compare to https://www.lazyvim.org/plugins/treesitter#nvim-treesitter
    "nvim-treesitter/nvim-treesitter",
    opts = {
      -- -- Disable highlighting for markdown, since we can just use Superchig/vim-markdown
      -- highlight = { disable = { "markdown" } },
      indent = { disable = { "ruby", "cpp" } },
      -- ensure_installed = { "comment", "lua", "ruby", "python" },
    },
  },

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
        -- C++
        "clangd",
        "codelldb",
        "clang-format",
      },
    },
  },

  {
    "snacks.nvim",
    opts = {
      scroll = { enabled = false },
      indent = {
        enabled = true,
        animate = {
          enabled = false,
        },
      },
    },
    config = function(_, opts)
      require("snacks").setup(opts)

      local orig_notify = vim.notify
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.notify = function(msg, level, notify_opts)
        if msg == "No information available" then
          return
        end

        orig_notify(msg, level, notify_opts)
      end
    end,
  },

  {
    "nvim-neo-tree/neo-tree.nvim",
    opts = {
      window = {
        mappings = {
          ["/"] = "none",
          ["?"] = "none",
          ["<leader>?"] = "show_help",
        },
      },
    },
  },
}
