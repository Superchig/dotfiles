-- A package manager. Not particularly powerful, but it'll get the job done.

local M = {}

local util = require("util")

---@alias bpack.package.Type
---| '"git"'
---| '"vendored"'

---@class bpack.Package
---@field name string
---@field url? string
---@field type? bpack.package.Type

M.bpack_path = util.data .. "/bpack"
M.vendored_path = util.config .. "/bpack-vendored"

---@param bpackages bpack.Package[]
function M.setup(bpackages)
  util.mkdir_p(M.bpack_path)

  for _, bpackage in ipairs(bpackages) do
    local name = bpackage.name
    local url = bpackage.url
    local type = bpackage.type

    if type == nil then
      if string.find(name, "/") ~= nil then
        type = "git"
      end
    end
    if type == nil then
      error("bpack: Error with " .. name .. ": Unable to determine package type")
    end

    local bpackage_path = ""
    if type == "git" then
      if url == nil then
        if string.find(name, "/") ~= nil then
          url = "https://github.com/" .. name .. ".git"
        else
          goto continue
        end
      end
      assert(url ~= nil)

      bpackage_path = M.bpack_path .. "/" .. name
      if not util.file_exists(bpackage_path) then
        vim.notify("Downloading " .. name, vim.log.levels.INFO)
        local completed = vim.system({ "git", "clone", url, bpackage_path }):wait()
        if completed.code ~= 0 then
          error("Failed to git clone " .. name)
        end
      end
    elseif type == "vendored" then
      if url == nil then
        url = M.vendored_path .. "/" .. name
      end
      assert(url ~= nil)

      bpackage_path = url
    end

    assert(bpackage_path ~= "")

    vim.opt.rtp:prepend(bpackage_path)

    ::continue::
  end
end

return M
