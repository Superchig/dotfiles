return {
  "nvim-neorg/neorg",
  lazy = true,
  cmd = "Neorg",
  ft = "norg",
  build = ":Neorg sync-parsers",
  opts = {
    load = {
      ["core.defaults"] = {}, -- Loads default behaviour
    },
  },
  dependencies = { { "nvim-lua/plenary.nvim" } },
}
