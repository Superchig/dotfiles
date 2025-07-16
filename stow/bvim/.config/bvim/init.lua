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

vim.cmd([[autocmd FileType lua setlocal expandtab shiftwidth=2 tabstop=2]])
vim.cmd([[cabbrev ev e ~/.config/bvim/init.lua]])

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
