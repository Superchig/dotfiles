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
vim.o.cursorline = true

vim.opt.completeopt = { "menuone", "noinsert", "noselect" }

-- See :help base-directories for more
local config = vim.fn.stdpath("config") -- E.g., ~/.config/bvim/
local data = vim.fn.stdpath("data")     -- E.g., ~/.local/share/bvim/
---@diagnostic disable-next-line: unused-local
local state = vim.fn.stdpath("state")   -- E.g., ~/.local/state/bvim/
local cache = vim.fn.stdpath("cache")   -- E.g., ~/.cache/bvim
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
  local right_ch = surrounding_ch[2]

  if right_ch ~= ch then
    vim.api.nvim_paste(ch .. ch, false, -1)
  end

  vim.api.nvim_win_set_cursor(0, { line, col + #ch })
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
    return
  end

  local pos = vim.api.nvim_win_get_cursor(0)
  local line = pos[1] - 1
  local col = pos[2]

  local surrounding_ch = GetSurroundingChars()

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
    local left_ch = surrounding_ch[1]
    local right_ch = surrounding_ch[2]

    -- print("left_ch " .. left_ch .. ", right_ch: " .. right_ch)

    if left_ch == opener and right_ch == closer then
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
  vim.keymap.set("i", "<BS>", function() return DeleteCloser(pairs) end, { expr = true })
end

-- Call the function to set up auto-pairs
auto_pairs()

local bin_path = data .. "/bmason"
local download_path = cache .. "/bmason"

local util = require("util")

local file_exists = util.file_exists

local luals_path = bin_path .. "/LuaLS"
local download_url =
"https://github.com/LuaLS/lua-language-server/releases/download/3.15.0/lua-language-server-3.15.0-darwin-arm64.tar.gz"

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
      local completed = vim.system({ "mkdir", path_cumulative }):wait()
      if completed.code ~= 0 then
        error("Failed to create directory: " .. path_cumulative)
      end
    end
  end
end

mkdir_p(download_path)
mkdir_p(luals_path)

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
  vim.system({ "curl", "-L", "-o", archive, download_url }, {}, after_download)
end

local function dir_empty(path)
  local file_names = vim.system({ "ls", path }):wait()
  return file_names.stdout == ""
end

if dir_empty(luals_path) then
  print("Extracting LuaLS")

  vim.system({ "tar", "xvf", archive, "--directory", luals_path }, {}, function(completed)
    if completed.code ~= 0 then
      error("Failed to extract LuaLS, exit code: " ..
        completed.code .. "\nStdout: " .. completed.stdout .. "\nStderr: " .. completed.stderr)
    else
      print("Finished extracting LuaLS")
    end
  end)
end

vim.env.PATH = vim.env.PATH .. ":" .. luals_path .. "/bin"

---@type integer?
local completion_menu_win = nil
local completion_index = 1
local completions = { "foo", "bar", "baz" }
local completion_ns = vim.api.nvim_create_namespace("bcomplete")
---@type integer?
local completion_extmark_id = nil

local function completion_menu_draw()
  if completion_menu_win == nil then
    print("Can't draw completion menu if it hasn't been created")
    return
  end

  local buf = vim.api.nvim_win_get_buf(completion_menu_win)

  local completion = completions[completion_index]
  local linenum = completion_index - 1

  completion_extmark_id = vim.api.nvim_buf_set_extmark(
    buf,
    completion_ns,
    linenum,
    0,
    {
      end_col = #completion,
      hl_group = "Visual",
      id = completion_extmark_id,
    }
  )
end

local function show_completion_menu()
  local max_completion_len = 0

  for _, completion in ipairs(completions) do
    if #completion > max_completion_len then
      max_completion_len = #completion
    end
  end

  local buf = vim.api.nvim_create_buf(false, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, true, completions)

  completion_menu_win = vim.api.nvim_open_win(buf, false, {
    relative = "cursor",
    width = max_completion_len,
    height = #completions,
    row = 1,
    col = 0,
    style = "minimal",
  })

  -- vim.api.nvim_set_option_value("filetype", "bcompletemenu", { win = completion_menu_win })

  completion_index = 1

  completion_menu_draw()
end

local function completion_menu_down()
  if completion_menu_win == nil then
    print("Can't move down on completion menu when it isn't open")
    return
  end

  completion_index = completion_index + 1
  if completion_index > #completions then
    completion_index = #completions
  end

  completion_menu_draw()
end

local function completion_menu_close()
  if completion_menu_win == nil then
    print("Can't close completion menu if it isn't open")
    return
  end

  vim.api.nvim_win_close(completion_menu_win, true)
  completion_menu_win = nil
end

local function tab_wrapper()
  if completion_menu_win ~= nil then
    completion_menu_down()
  else
    show_completion_menu()
  end
  -- vim.lsp.buf_request_sync(0, "textDocument/cabbrev")
end

local esc = vim.api.nvim_replace_termcodes("<Esc>", true, true, true)
local function esc_wrapper()
  if completion_menu_win ~= nil then
    completion_menu_close()
  else
    vim.api.nvim_feedkeys(esc, "n", false)
  end
end

vim.lsp.config["luals"] = {
  cmd = { "lua-language-server" },
  filetypes = { "lua" },
  root_markers = { { ".luarc.json", ".luarc.json" }, ".git" },
  settings = {
    Lua = {
      runtime = {
        version = "LuaJIT",
      },
      workspace = {
        library = { vim.env.VIMRUNTIME .. "/lua/vim" },
      },
    },
  },
  on_attach = function(client, bufnr)
    vim.lsp.completion.enable(true, client.id, bufnr)

    vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Goto definition (LSP)" })
    vim.keymap.set("n", "gr", vim.lsp.buf.references, { desc = "View references (LSP)" })
    vim.keymap.set("n", "gh", vim.lsp.buf.hover, { desc = "Hover information (LSP)" })

    vim.keymap.set("n", "<Leader>=", vim.lsp.buf.format, { desc = "Format buffer (LSP)" })
    vim.keymap.set("n", "<Leader>cr", vim.lsp.buf.rename, { desc = "Rename symbol (LSP)" })
    vim.keymap.set("n", "<Leader>cd", vim.diagnostic.open_float, { desc = "Show diagnostics in floating window" })
    vim.keymap.set("n", "<Leader>ca", vim.lsp.buf.code_action, { desc = "Handle code actions if available" })

    vim.keymap.set("i", "<Tab>", tab_wrapper, { desc = "Get completion" })
    vim.keymap.set("i", "<Esc>", esc_wrapper, { desc = "Get completion" })
  end,
}

vim.cmd([[autocmd FileType lua lua vim.lsp.enable("luals")]])

vim.g.mapleader = " "

vim.keymap.set("n", "<C-S>", "<cmd>write<CR>", { desc = "Save current buffer" })
vim.keymap.set(
  "n",
  "<Leader>bd",
  function() vim.api.nvim_buf_delete(0, {}) end,
  { desc = "Delete (unload) buffer" }
)
