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
      keys[#keys + 1] = { "K", "<cmd>bn<cr>", desc = "Next buffer" }
      keys[#keys + 1] = { "J", "<cmd>bp<cr>", desc = "Previous buffer" }
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
}
