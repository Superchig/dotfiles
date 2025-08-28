return {
  "nvim-orgmode/orgmode",
  lazy = true,
  cmd = "Org",
  ft = { "org" },
  config = function()
    require("orgmode").setup()
  end,
}
