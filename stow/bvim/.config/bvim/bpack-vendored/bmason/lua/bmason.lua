--- A portable package manager for installing LSP servers and similar tools.
--- Inspired by mason.nvim, but not nearly as powerful.

local M = {}

local util = require("util")

function M.setup()
  local bin_path = util.data .. "/bmason"
  local download_path = util.cache .. "/bmason"

  local luals_path = bin_path .. "/LuaLS"

  local download_url = ""
  if jit.os == "Linux" then
    download_url =
    "https://github.com/LuaLS/lua-language-server/releases/download/3.15.0/lua-language-server-3.15.0-linux-x64.tar.gz"
  else
    download_url =
    "https://github.com/LuaLS/lua-language-server/releases/download/3.15.0/lua-language-server-3.15.0-darwin-arm64.tar.gz"
  end

  local archive_name = ""
  for part in string.gmatch(download_url, "[^/]+") do
    archive_name = part
  end

  util.mkdir_p(download_path)
  util.mkdir_p(luals_path)

  local archive = download_path .. "/" .. archive_name

  if not util.file_exists(archive) then
    vim.notify("Downloading LuaLS", vim.log.levels.INFO)

    local completed = vim.system({ "curl", "-L", "-o", archive, download_url }, {}):wait()
    if completed.code ~= 0 then
      vim.notify("Failed to download LuaLS, exit code: " .. completed.code, vim.log.levels.ERROR)
    else
      vim.notify("Finished downloading LuaLS", vim.log.levels.INFO)
    end
  end

  if util.dir_empty(luals_path) then
    vim.notify("Extracting LuaLS", vim.log.levels.INFO)

    local completed = vim.system({ "tar", "xvf", archive, "--directory", luals_path }, {}):wait()
    if completed.code ~= 0 then
      vim.notify(
        "Failed to extract LuaLS, exit code: " ..
        completed.code .. "\nStdout: " .. completed.stdout .. "\nStderr: " .. completed.stderr,
        vim.log.levels.ERROR
      )
    else
      vim.notify("Finished extracting LuaLS", vim.log.levels.INFO)
    end
  end

  vim.env.PATH = vim.env.PATH .. ":" .. luals_path .. "/bin"
end

return M
