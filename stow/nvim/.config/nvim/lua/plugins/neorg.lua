---@return string
local function default_workspace_path()
  if jit.os == "OSX" then
    return "~/tmp"
  else
    return "~/Dropbox/norg"
  end
end

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
            tmp = default_workspace_path(),
          },
        },
      },
    },
  },
  dependencies = { { "nvim-lua/plenary.nvim" } },
}
