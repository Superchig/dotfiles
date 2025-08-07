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
vim.o.smartcase = true
vim.o.ignorecase = true

vim.opt.completeopt = { "menuone", "noinsert", "noselect" }

-- Set status line
---@return string
function StatuslineMode()
  local mode_info = vim.api.nvim_get_mode()
  local current_mode = mode_info.mode
 if current_mode == "n" then
    return "Normal"
  elseif current_mode == "i" then
    return "Insert"
  elseif current_mode == "v" then
    return "Visual"
  elseif current_mode == "V" then
    return "Visual Line"
  elseif current_mode == "\22" then -- <C-V>
    return "Visual Block"
  elseif current_mode == "R" then
    return "Replace"
  elseif current_mode == "s" then
    return "Select"
  elseif current_mode == "S" then
    return "Select Line"
  elseif current_mode == "\19" then -- <C-S>
    return "Select Block"
  elseif current_mode == "c" then
    return "Command"
  else
    return "Unknown Mode: " .. current_mode
  end
end

---@return string
function StatuslineFiletype()
  local ft = vim.bo.filetype
  if ft == "" then
    return ""
  end

  return " | " .. "[" .. ft .. "]"
end

---@return string
function StatuslineDiagnostics()
  local diagnostics = vim.diagnostic.get(0, {})
  local severity_counts = {
    [vim.diagnostic.severity.ERROR] = 0,
    [vim.diagnostic.severity.WARN] = 0,
    [vim.diagnostic.severity.INFO] = 0,
    [vim.diagnostic.severity.HINT] = 0,
  }
  for _, diagnostic in ipairs(diagnostics) do
    severity_counts[diagnostic.severity] = severity_counts[diagnostic.severity] + 1
  end

  local parts = {}
  if severity_counts[vim.diagnostic.severity.ERROR] > 0 then
    table.insert(parts, "E " .. severity_counts[vim.diagnostic.severity.ERROR])
  end
  if severity_counts[vim.diagnostic.severity.WARN] > 0 then
    table.insert(parts, "W " .. severity_counts[vim.diagnostic.severity.WARN])
  end
  if severity_counts[vim.diagnostic.severity.INFO] > 0 then
    table.insert(parts, "I " .. severity_counts[vim.diagnostic.severity.INFO])
  end
  if severity_counts[vim.diagnostic.severity.HINT] > 0 then
    table.insert(parts, "H " .. severity_counts[vim.diagnostic.severity.HINT])
  end
  if #parts > 0 then
    table.insert(parts, "| ")
    return table.concat(parts, " ")
  else
    return ""
  end
end

-- The default statusline looks like this: set statusline=%<%f\ %h%w%m%r%=%-14.(%l,%c%V%)\ %P
local statusline_parts = {
  " ",
  "%{v:lua.StatuslineMode()}",
  " | ",

  "%{v:lua.StatuslineDiagnostics()}",

  "%<",  -- Truncate line at start
  "%f ", -- Path to file in buffer, as typed or relative to cwd
  "%h",  -- Help buffer flag, text is "[help]"
  "%w",  -- Preview window flag, text is "[Preview]"
  "%m",  -- Modified flag, text is "[+]"; "[-]" if 'modifiable' is off
  "%r",  -- Readonly flag, text is "[RO]".

  "%=",  -- Separator for a new section

  "%-4.P", -- Percentage through file of displayed window.  This is like the percentage described for 'ruler'
  " ",
  "%(",
  "%l,", -- Line number
  "%c",  -- Column number
  "%V",  -- Virtual column number as -{num}.  Not displayed if equal to 'c'.
  "%)",
  "%{v:lua.StatuslineFiletype()}",
  " ",
}
vim.o.statusline = table.concat(statusline_parts, "")
vim.o.showmode = false
vim.o.laststatus = 3

-- See :help base-directories for more
local config = vim.fn.stdpath("config") -- E.g., ~/.config/bvim/
local data = vim.fn.stdpath("data")     -- E.g., ~/.local/share/bvim/
---@diagnostic disable-next-line: unused-local
local state = vim.fn.stdpath("state")   -- E.g., ~/.local/state/bvim/
local cache = vim.fn.stdpath("cache")   -- E.g., ~/.cache/bvim
local init_file = config .. "/init.lua"

vim.cmd([[autocmd FileType lua setlocal expandtab shiftwidth=2 tabstop=2]])
vim.cmd([[autocmd TextYankPost * silent! lua vim.hl.on_yank {higroup='Visual', timeout=100}]])
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

    if left_ch == opener and right_ch == closer then
      return "<ESC>xa<BS>"
    end
  end

  return "<BS>"
end

local function auto_pairs()
  -- Map opening brackets to insert closing brackets
  Pairs = {
    { '(', ')' },
    { '{', '}' },
    { '[', ']' },
    { '"', '"' },
    -- { "'", "'" },
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

local archive = download_path .. "/" .. archive_name

if not file_exists(archive) then
  local function after_download(completed)
    if completed.code ~= 0 then
      vim.notify("Failed to download LuaLS, exit code: " .. completed.code, vim.log.levels.ERROR)
    else
      vim.notify("Finished downloading LuaLS", vim.log.levels.INFO)
    end
  end

  vim.notify("Downloading LuaLS", vim.log.levels.INFO)
  vim.system({ "curl", "-L", "-o", archive, download_url }, {}, after_download):wait()
end

local function dir_empty(path)
  local file_names = vim.system({ "ls", path }):wait()
  return file_names.stdout == ""
end

if dir_empty(luals_path) then
  vim.notify("Extracting LuaLS", vim.log.levels.ERROR)

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

---@type integer?
local completion_menu_win = nil
local completion_top_index = 1
local completions_height = 8
local completion_index = 0
---@type lsp.CompletionItem[]
local completions = {}
local completion_ns = vim.api.nvim_create_namespace("bcomplete")
---@type integer?
local completion_bg_extmark_id = nil
---@type integer?
local completion_sel_extmark_id = nil
local completion_menu_skip_next = false

local function completion_menu_show()
  local max_completion_label_len = 0

  for _, completion in ipairs(completions) do
    local label = completion.label
    if #label > max_completion_label_len then
      max_completion_label_len = #label
    end
  end

  local buf = vim.api.nvim_create_buf(false, false)

  completion_menu_win = vim.api.nvim_open_win(buf, false, {
    relative = "cursor",
    width = max_completion_label_len,
    height = completions_height,
    row = 1,
    col = 0,
    style = "minimal",
  })

  completion_top_index = 1
  completion_index = 0
end

local function completion_menu_close()
  if completion_menu_win == nil then
    vim.notify("Can't close completion menu if it isn't open", vim.log.levels.ERROR)
    return
  end

  local buf = vim.api.nvim_win_get_buf(completion_menu_win)

  vim.api.nvim_win_close(completion_menu_win, true)
  completion_menu_win = nil

  vim.api.nvim_buf_delete(buf, { force = true })
end

local function completion_menu_draw()
  -- Close if no more valid completions

  if #completions == 0 then
    if completion_menu_win ~= nil then
      completion_menu_close()
    end
    return
  end

  -- Open if valid completions

  if completion_menu_win == nil then
    completion_menu_show()
  end
  assert(completion_menu_win ~= nil)

  -- Ensure that internal state is valid

  if completion_index < 0 then
    completion_index = 0 -- Special null value
  end
  if completion_index > #completions then
    completion_index = #completions
  end

  if completion_index >= completion_top_index + completions_height then
    local bot_index = completion_index
    completion_top_index = bot_index - completions_height + 1
  end

  -- Draw actual floating window

  local buf = vim.api.nvim_win_get_buf(completion_menu_win)

  local win_config = vim.api.nvim_win_get_config(completion_menu_win)

  local completion = completions[completion_index]
  ---@type string[]
  local displayed_labels = {}
  for _, completion_item in ipairs(vim.list_slice(
    completions,
    completion_top_index,
    completion_top_index + completions_height - 1
  )) do
    table.insert(displayed_labels, completion_item.label)
  end
  while #displayed_labels < win_config.height do
    table.insert(displayed_labels, string.rep(" ", win_config.width))
  end

  vim.api.nvim_buf_set_lines(buf, 0, -1, true, displayed_labels)

  completion_bg_extmark_id = vim.api.nvim_buf_set_extmark(
    buf,
    completion_ns,
    0,
    0,
    {
      end_row = win_config.height,
      end_col = win_config.width,
      strict = false,
      hl_group = "Pmenu",
      id = completion_bg_extmark_id,
    }
  )

  if completion_index > 0 then
    -- Draw selection highlight
    local linenum = completion_index - completion_top_index
    local label = completion.label
    completion_sel_extmark_id = vim.api.nvim_buf_set_extmark(
      buf,
      completion_ns,
      linenum,
      0,
      {
        end_col = #label,
        hl_group = "PmenuSel",
        id = completion_sel_extmark_id,
      }
    )

    -- Change current line
  end
end

---@class LspResponse
---@field err lsp.ResponseError
---@field result any
---@field ctx lsp.HandlerContext

---@return lsp.CompletionItem[]
local function get_lsp_completion_items()
  local clients = vim.lsp.get_clients({ bufnr = 0 })
  local position_params = vim.lsp.util.make_position_params(0, "utf-8")

  ---@type table<integer, LspResponse>
  local responses = {}

  for i, client in ipairs(clients) do
    ---@type lsp.CompletionParams
    local params = {
      textDocument = position_params.textDocument,
      position = position_params.position,
    }
    if client.capabilities.textDocument.completion.contextSupport then
      params.context = {
        triggerKind = 1,
      }
    end
    local status_or_request_id = client:request("textDocument/completion", params, function(err, result, ctx)
      responses[i] = {
        err = err,
        result = result,
        ctx = ctx,
      }
    end)
    if not status_or_request_id then
      vim.notify("Failed to make textDocument/completion request to LSP client " .. client.id, vim.log.levels.ERROR)
    end
  end

  for i, client in ipairs(clients) do
    local wait_result, reason = vim.wait(1000, function()
      return responses[i] ~= nil
    end, 10)
    if not wait_result and reason == -1 then
      vim.notify(
        "textDocument/completion request to " .. client.id ..
        " failed to complete because it never returned true",
        vim.log.levels.ERROR
      )
    end
  end

  ---@type lsp.CompletionItem[]
  local completion_items = {}
  for _, response in pairs(responses) do
    local ctx = response.ctx

    if response ~= nil and response.err ~= nil then
      vim.notify(
        "textDocument/completion request to " ..
        ctx.client_id .. " failed with error: " .. vim.inspect(response.err)
      )
      goto continue
    end

    if response.result == nil then
      goto continue
    end

    local result = response.result
    if result.isIncomplete ~= nil then
      ---@type lsp.CompletionList
      local completion_list = result
      -- TODO(Chris): Handle applyKind?, itemDefaults?
      for _, item in ipairs(completion_list.items) do
        table.insert(completion_items, item)
      end
    else
      vim.notify(
        "We haven't implemented handler code for a textDocument/completion response: " .. vim.inspect(result),
        vim.log.levels.ERROR
      )
    end

    ::continue::
  end

  table.sort(completion_items, function(a, b)
    local a_sort_text = a.sortText or a.label
    local b_sort_text = b.sortText or b.label
    return a_sort_text <= b_sort_text
  end)

  return completion_items
end

local function get_completion_word()
  local line = vim.api.nvim_get_current_line()
  local word
  for part in string.gmatch(line, "[%w_]+") do
    word = part
  end
  return word
end

local function display_completions()
  completions = {}

  local completion_items = get_lsp_completion_items()
  local fallback_word = get_completion_word()
  for _, completion in ipairs(completion_items) do
    local word = completion.filterText or fallback_word

    if string.find(completion.label, word) ~= nil then
      table.insert(completions, completion)
    end
  end

  completion_menu_draw()
end

local function completion_menu_down()
  if completion_menu_win == nil then
    vim.notify("Can't move down on completion menu when it isn't open", vim.log.levels.ERROR)
    return
  end

  completion_index = completion_index + 1

  completion_menu_draw()
end

local function get_word_from_completion_item(completion)
  return completion.filterText or get_completion_word() or completion.label
end

local function complete_on_text_change()
  if completion_menu_skip_next then
    completion_menu_skip_next = false
    return
  end

  local word = get_completion_word()
  if word == nil then
    return
  end

  -- local new_completions = {}
  -- for _, completion in ipairs(completions) do
  --   if string.find(completion.label, word) ~= nil then
  --     table.insert(new_completions, completion)
  --   end
  -- end
  -- completions = new_completions

  completion_menu_draw()

  vim.schedule(display_completions)
end

local function tab_wrapper()
  if completion_menu_win ~= nil then
    completion_menu_down()
  else
    display_completions()
  end
end

local esc = vim.api.nvim_replace_termcodes("<Esc>", true, true, true)
local function esc_wrapper()
  if completion_menu_win ~= nil then
    completion_menu_close()
  end
  vim.api.nvim_feedkeys(esc, "n", false)
end

local cr = vim.api.nvim_replace_termcodes("<CR>", true, true, true)
local ctrl_y = vim.api.nvim_replace_termcodes("<C-Y>", true, true, true)
local function cr_wrapper()
  if vim.fn.pumvisible() == 1 then
    vim.api.nvim_feedkeys(ctrl_y, "n", false)
    return
  end

  if completion_menu_win ~= nil then
    local completion = completions[completion_index]

    completion_menu_close()

    local buf = vim.api.nvim_get_current_buf()

    if completion.textEdit ~= nil then
      if completion.textEdit.range ~= nil then
        local text_edit = completion.textEdit
        ---@cast text_edit lsp.TextEdit
        vim.lsp.util.apply_text_edits({ text_edit }, buf, "utf-8")
      else
        vim.notify(
          "We have not yet implemented insertion with lsp.InsertReplaceEdit",
          vim.log.levels.ERROR
        )
      end
    else
      if completion.insertTextFormat == vim.lsp.protocol.InsertTextFormat.PlainText then
        local cursor_pos = vim.api.nvim_win_get_cursor(0)
        local line = cursor_pos[1]
        local col = cursor_pos[2]

        local word = get_word_from_completion_item(completion)
        local start_pos = { line - 1, col - #word } -- For LSP
        local end_pos = { line - 1, col }           -- For LSP

        vim.api.nvim_buf_set_text(
          buf,
          start_pos[1], start_pos[2],
          end_pos[1], end_pos[2],
          {}
        )

        local insert_text = completion.insertText or completion.label
        vim.api.nvim_paste(insert_text, false, -1)

        completion_menu_skip_next = true
      else
        vim.notify(
          "We have not yet implemented insertion of completions with this format: " .. completion.insertTextFormat,
          vim.log.levels.ERROR
        )
      end
    end
  else
    vim.api.nvim_feedkeys(cr, "n", false)
  end
end

local ctrl_n = vim.api.nvim_replace_termcodes("<C-N>", true, true, true)
local function ctrl_n_wrapper()
  if vim.fn.pumvisible() == 1 then
    vim.api.nvim_feedkeys(ctrl_n, "n", false)
    return
  end

  tab_wrapper()
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
  on_attach = function()
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Goto definition (LSP)" })
    vim.keymap.set("n", "gr", vim.lsp.buf.references, { desc = "View references (LSP)" })
    vim.keymap.set("n", "gh", vim.lsp.buf.hover, { desc = "Hover information (LSP)" })

    vim.keymap.set("n", "<Leader>=", vim.lsp.buf.format, { desc = "Format buffer (LSP)" })
    vim.keymap.set("n", "<Leader>cr", vim.lsp.buf.rename, { desc = "Rename symbol (LSP)" })
    vim.keymap.set("n", "<Leader>cd", vim.diagnostic.open_float, { desc = "Show diagnostics in floating window" })
    vim.keymap.set("n", "<Leader>ca", vim.lsp.buf.code_action, { desc = "Handle code actions if available" })

    vim.keymap.set("i", "<Tab>", tab_wrapper, { desc = "Get/next completion" })
    vim.keymap.set("i", "<Esc>", esc_wrapper, { desc = "Exit completion" })
    vim.keymap.set("i", "<CR>", cr_wrapper, { desc = "Accept completion" })
    vim.keymap.set("i", "<C-N>", ctrl_n_wrapper, { desc = "Get/next completion" })

    vim.o.omnifunc = "v:lua.vim.lsp.omnifunc"
    vim.o.foldmethod = "expr"
    vim.o.foldexpr = "v:lua.vim.lsp.foldexpr()"
    vim.o.foldlevel = 1000

    -- print(vim.inspect(client.capabilities.textDocument.completion))

    local augroup_id = vim.api.nvim_create_augroup("bcomplete", {})
    vim.api.nvim_create_autocmd("TextChangedI", {
      pattern = "*", -- Apply to all buffers
      callback = complete_on_text_change,
      group = augroup_id,
    })
  end,
}

vim.cmd([[autocmd FileType lua lua vim.lsp.enable("luals")]])

vim.diagnostic.config({
  virtual_text = true,
  underline = true,
})

vim.g.mapleader = " "

vim.keymap.set("n", "<C-S>", "<cmd>write<CR>", { desc = "Save current buffer" })
vim.keymap.set(
  "n",
  "<Leader>bd",
  function() vim.api.nvim_buf_delete(0, {}) end,
  { desc = "Delete (unload) buffer" }
)
vim.keymap.set("n", "<C-J>", "<C-W>j", { desc = "Switch to window downwards" })
vim.keymap.set("n", "<C-K>", "<C-W>k", { desc = "Switch to window upwards" })
vim.keymap.set("n", "<C-H>", "<C-W>h", { desc = "Switch to window leftwards" })
vim.keymap.set("n", "<C-L>", "<C-W>l", { desc = "Switch to window rightwards" })
vim.keymap.set("n", "<C-S-J>", "<C-W>J", { desc = "Move window downwards" })
vim.keymap.set("n", "<C-S-K>", "<C-W>K", { desc = "Move window upwards" })
vim.keymap.set("n", "<C-S-H>", "<C-W>H", { desc = "Move window leftwards" })
vim.keymap.set("n", "<C-S-L>", "<C-W>L", { desc = "Move window rightwards" })

-- Treesitter
vim.treesitter.language.add("lua")
vim.api.nvim_create_autocmd(
  "FileType",
  {
    desc = "Lua treesitter bvim",
    pattern = "lua",
    callback = function(args)
      vim.treesitter.start(args.buf, "lua")
    end
  }
)
