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
    opts = function()
      local keys = require("lazyvim.plugins.lsp.keymaps").get()

      keys[#keys + 1] = { "K", false }
      keys[#keys + 1] = { "J", false }
    end,
  },

  {
    -- Compare to https://www.lazyvim.org/plugins/treesitter#nvim-treesitter
    "nvim-treesitter/nvim-treesitter",
    opts = {
      -- Disable highlighting for markdown, since we can just use Superchig/vim-markdown
      highlight = { disable = { "markdown" } },
      ensure_installed = { "comment", "css", "lua", "ruby", "python", "typescript", "rust", "vim", "go", "cpp" },
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
        "svelte-language-server",
        "tailwindcss-language-server",
        "typescript-language-server",
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
