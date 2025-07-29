-- vim.cmd("colorscheme vim")
-- vim.cmd("colorscheme slate")
-- vim.cmd("colorscheme sorbet")
-- vim.cmd("colorscheme unokai")
-- vim.cmd("colorscheme wildcharm")

vim.o.number = true
vim.o.relativenumber = true
vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 4
vim.o.expandtab = true
vim.o.wrap = false

-- See :help base-directories for more
local config = vim.fn.stdpath("config") -- E.g., ~/.config/bvim/
local data = vim.fn.stdpath("data") -- E.g., ~/.local/share/bvim/
local state = vim.fn.stdpath("state") -- E.g., ~/.local/state/bvim/
local cache = vim.fn.stdpath("cache") -- E.g., ~/.cache/bvim
local init_file = config .. "/init.lua"

vim.cmd([[autocmd FileType lua setlocal expandtab shiftwidth=2 tabstop=2]])
vim.cmd("cabbrev ev e" .. init_file)

vim.cmd([[
	augroup vimrc-incsearch-highlight
	  autocmd!
	  autocmd CmdlineEnter /,\? :set hlsearch
	  autocmd CmdlineLeave /,\? :set nohlsearch
	augroup END
]])

function GetSurroundingChars()
  local pos = vim.api.nvim_win_get_cursor(0)
  local line = pos[1] - 1
  local col = pos[2]

  local l = vim.api.nvim_buf_get_lines(0, line, line + 1, false)[1]

  local left_ch = string.sub(l, col, col)
  local right_ch = string.sub(l, col + 1, col + 1)

  return { left_ch, right_ch }
end

function InsertTwin(ch)
  local pos = vim.api.nvim_win_get_cursor(0)
  local line = pos[1]
  local col = pos[2]

  local surrounding_ch = GetSurroundingChars()
  local left_ch = surrounding_ch[1]
  local right_ch = surrounding_ch[2]

  if right_ch == ch then
    vim.api.nvim_win_set_cursor(0, { line, col + 1 })
  elseif left_ch ~= ch then
    vim.api.nvim_paste(ch .. ch, false, -1)

    vim.api.nvim_win_set_cursor(0, { line, col })
  else
    vim.api.nvim_paste(ch, false, -1)
  end
end

function InsertPair(opener, closer)
  -- print("opener " .. opener .. " .. closer " .. closer)

  if opener == closer then
    InsertTwin(opener)
  end

  vim.api.nvim_paste(opener .. closer, false, -1)

  local pos = vim.api.nvim_win_get_cursor(0)
  local line = pos[1]
  local col = pos[2]

  vim.api.nvim_win_set_cursor(0, { line, col - 1 })
end

function InsertCloser(opener, closer)
  if opener == closer then
    InsertTwin(opener)
  end

  local pos = vim.api.nvim_win_get_cursor(0)
  local line = pos[1] - 1
  local col = pos[2]

  local l = vim.api.nvim_buf_get_lines(0, line, line + 1, false)[1]

  local surrounding_ch = GetSurroundingChars()

  local left_ch = surrounding_ch[1]
  local right_ch = surrounding_ch[2]

  if right_ch == closer then
    vim.api.nvim_win_set_cursor(0, { line + 1, col + 1 })
  else
    vim.api.nvim_paste(closer, false, -1)
  end
end

function DeleteCloser(pairs)
  for _, pair in ipairs(pairs) do
    local opener = pair[1]
    local closer = pair[2]

    local surrounding_ch = GetSurroundingChars()
    local right_ch = surrounding_ch[2]

    -- print("left_ch " .. left_ch .. ", right_ch: " .. right_ch)

    if right_ch == closer then
      -- print("Closer found")
      return "<ESC>xa<BS>"
    end
  end

  -- print("Closer not found")
  return "<BS>"
end

local function auto_pairs()
  -- Map opening brackets to insert closing brackets
  Pairs = {
    { '(', ')' },
    { '{', '}' },
    { '[', ']' },
    { '"', '"' },
    { "'", "'" },
  }

  local pairs = Pairs

  -- Insert closing bracket when typing opening bracket
  for _, pair in ipairs(pairs) do
    vim.keymap.set("i", pair[1], function() InsertPair(pair[1], pair[2]) end)
  end

  -- Skip over closing bracket if typed
  for _, pair in ipairs(pairs) do
    vim.keymap.set("i", pair[2], function() InsertCloser(pair[1], pair[2]) end)
  end

  -- Remove closing bracket when deleting opening bracket
  for _, pair in ipairs(pairs) do
    vim.keymap.set("i", "<BS>", function() return DeleteCloser(pairs) end, { expr = true })
  end
end

-- Call the function to set up auto-pairs
auto_pairs()

local bin_path = data .. "/bmason"
local download_path = cache .. "/bmason"

util = require("util")

local file_exists = util.file_exists

local lsp_bin = bin_path .. "/bmason/LuaLS"
local download_url = "https://github.com/LuaLS/lua-language-server/releases/download/3.15.0/lua-language-server-3.15.0-darwin-arm64.tar.gz"

-- TODO(Chris): Refactor this to be recursive
local function mkdir_p(path)
  local parts = util.split(path, util.path_separator)
  local parts_cumulative = {}
  
  if vim.fn.has("unix") then
    table.insert(parts_cumulative, "/")
  end

  for _, part in ipairs(parts) do
    table.insert(parts_cumulative, part)
    table.insert(parts_cumulative, "/")

    local path_cumulative = util.path_join(unpack(parts_cumulative))

    if not file_exists(path_cumulative) then
      completed = vim.system({"mkdir", path_cumulative}):wait()
      if completed.code ~= 0 then
        error("Failed to create directory: " .. path_cumulative)
      end
    end
  end
end

mkdir_p(download_path)
mkdir_p(lsp_bin)

if not file_exists(lsp_bin) then
  local archive = download_path .. "/lua-language-server-3.15.0-darwin-arm64.tar.gz"

  if not file_exists(archive) then
    local function after_download(completed)
      if completed.code ~= 0 then
        error("Failed to download LuaLS, exit code: " .. completed.code)
      else
        print("Finished downloading LuaLS")
      end
    end

    print("Downloading LuaLS")
    vim.system({"curl", "-L", "-o", archive, download_url}, {}, after_download)
  end

  print("Extracting LuaLS")
  vim.system({"tar", "xvf", archive, "--directory", lsp_bin}, {}, function(completed)
    if completed.code ~= 0 then
      error("Failed to extract LuaLS, exit code: " .. completed.code .. "\nStdout: " .. completed.stdout .. "\nStderr: " .. completed.stderr)
    else
      print("Finished extracting LuaLS")
    end
  end)
end
