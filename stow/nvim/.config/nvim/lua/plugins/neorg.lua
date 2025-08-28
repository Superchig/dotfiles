return {
  "nvim-neorg/neorg",
  lazy = true,
  cmd = "Neorg",
  ft = "norg",
  build = ":Neorg sync-parsers",
  opts = {
    load = {
      ["core.defaults"] = {}, -- Loads default behaviour
      ["core.dirman"] = {
        config = {
          default_workspace = "tmp",
          workspaces = {
            tmp = "~/tmp",
          },
        },
      },
    },
  },
  dependencies = { { "nvim-lua/plenary.nvim" } },
}
