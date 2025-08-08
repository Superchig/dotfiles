--- An auto-completion plugin.

local M = {}

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

function M.completion_menu_close()
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
      M.completion_menu_close()
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
  for _, completion_item in
  ipairs(vim.list_slice(completions, completion_top_index, completion_top_index + completions_height - 1))
  do
    table.insert(displayed_labels, completion_item.label)
  end
  while #displayed_labels < win_config.height do
    table.insert(displayed_labels, string.rep(" ", win_config.width))
  end

  vim.api.nvim_buf_set_lines(buf, 0, -1, true, displayed_labels)

  completion_bg_extmark_id = vim.api.nvim_buf_set_extmark(buf, completion_ns, 0, 0, {
    end_row = win_config.height,
    end_col = win_config.width,
    strict = false,
    hl_group = "Pmenu",
    id = completion_bg_extmark_id,
  })

  if completion_index > 0 then
    -- Draw selection highlight
    local linenum = completion_index - completion_top_index
    local label = completion.label
    completion_sel_extmark_id = vim.api.nvim_buf_set_extmark(buf, completion_ns, linenum, 0, {
      end_col = #label,
      hl_group = "PmenuSel",
      id = completion_sel_extmark_id,
    })

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
      vim.notify(
        "Failed to make textDocument/completion request to LSP client " .. client.id,
        vim.log.levels.ERROR
      )
    end
  end

  for i, client in ipairs(clients) do
    local wait_result, reason = vim.wait(1000, function()
      return responses[i] ~= nil
    end, 10)
    if not wait_result and reason == -1 then
      vim.notify(
        "textDocument/completion request to "
        .. client.id
        .. " failed to complete because it never returned true",
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
        "textDocument/completion request to "
        .. ctx.client_id
        .. " failed with error: "
        .. vim.inspect(response.err)
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

function M.completion_menu_down()
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

function M.completion_menu_accept()
  local completion = completions[completion_index]

  M.completion_menu_close()

  local buf = vim.api.nvim_get_current_buf()

  if completion.textEdit == nil then
    if completion.insertTextFormat == vim.lsp.protocol.InsertTextFormat.PlainText then
      local cursor_pos = vim.api.nvim_win_get_cursor(0)
      local line = cursor_pos[1]
      local col = cursor_pos[2] + 1 -- Add one because we're going to be in normal mode

      local word = get_word_from_completion_item(completion)
      local start_pos = { line - 1, col - #word } -- For LSP
      local end_pos = { line - 1, col }           -- For LSP

      vim.api.nvim_buf_set_text(buf, start_pos[1], start_pos[2], end_pos[1], end_pos[2], {})

      local insert_text = completion.insertText or completion.label
      vim.api.nvim_paste(insert_text, false, -1)

      completion_menu_skip_next = true
    else
      vim.notify(
        "We have not yet implemented insertion of completions with this format: "
        .. completion.insertTextFormat,
        vim.log.levels.ERROR
      )
    end
  else
    if completion.textEdit.range ~= nil then
      local text_edit = completion.textEdit
      ---@cast text_edit lsp.TextEdit
      vim.lsp.util.apply_text_edits({ text_edit }, buf, "utf-8")
    else
      vim.notify("We have not yet implemented insertion with lsp.InsertReplaceEdit", vim.log.levels.ERROR)
    end
  end
end

function M.complete_on_text_change()
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

---@param expr string
---@return string
local function bcomplete_expr(expr)
  return [[<Esc>:lua require ("bcomplete").]] .. expr .. "<CR>a"
end

---@return string
function M.tab_wrapper()
  if completion_menu_win ~= nil then
    return bcomplete_expr("completion_menu_down()")
  else
    return "<Tab>"
  end
end

---@return string
function M.esc_wrapper()
  if completion_menu_win ~= nil then
    return bcomplete_expr("completion_menu_close()")
  end
  return "<Esc>"
end

---@return string
function M.cr_wrapper()
  if vim.fn.pumvisible() == 1 then
    return "<C-Y>"
  end

  if completion_menu_win == nil then
    return "<CR>"
  end

  return bcomplete_expr("completion_menu_accept()")
end

---@return string
function M.ctrl_n_wrapper()
  if vim.fn.pumvisible() == 1 then
    return "<C-N>"
  end

  return M.tab_wrapper()
end

return M
