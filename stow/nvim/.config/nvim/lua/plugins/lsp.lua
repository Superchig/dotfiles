return {
  {
    "neovim/nvim-lspconfig",
    opts = function()
      local keys = require("lazyvim.plugins.lsp.keymaps").get()
      keys[#keys + 1] = { "K", "<cmd>bn<cr>", desc = "Next buffer" }
      keys[#keys + 1] = { "J", "<cmd>bp<cr>", desc = "Previous buffer" }
    end,
  },
}
