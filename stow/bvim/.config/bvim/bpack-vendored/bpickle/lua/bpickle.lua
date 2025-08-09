-- Makes it easier to preview and pick a colorscheme.

local M = {}

local util = require("util")

local line = 0
---@type integer
local buf = nil
local accepting = false
local orig_colorscheme = ""
local pickle_colorscheme = ""
local target_background = ""
local exclude = {
  "blue", "darkblue", "default", "delek", "desert", "elflord", "evening", "habamax", "industry", "koehler", "lunaperche",
  "morning", "murphy", "pablo", "peachpuff", "quiet", "retrobox", "ron", "shine", "slate", "sorbet", "torte", "unokai",
  "vim", "wildcharm", "zaibatsu", "zellner",
}

function M.pick()
  local colorschemes = vim.fn.getcompletion("", "color")
  colorschemes = util.subtract_tables(colorschemes, exclude)

  if buf == nil or not vim.api.nvim_buf_is_valid(buf) then
    buf = vim.api.nvim_create_buf(false, false)
    vim.api.nvim_buf_set_lines(buf, 0, -1, true, colorschemes)
    vim.api.nvim_buf_set_name(buf, "bpickle")
    vim.api.nvim_set_option_value("modifiable", false, { buf = buf })
    vim.api.nvim_set_option_value("buftype", "nofile", { buf = buf })
    vim.api.nvim_set_option_value("swapfile", false, { buf = buf })
  end

  vim.api.nvim_create_autocmd({ "BufEnter" }, {
    buffer = buf,
    callback = function()
      orig_colorscheme = vim.g.colors_name
      target_background = vim.o.background

      if pickle_colorscheme ~= "" then
        vim.cmd("colorscheme " .. pickle_colorscheme)
      end
    end,
  })

  vim.api.nvim_create_autocmd({ "CursorMoved" }, {
    buffer = buf,
    callback = function()
      local old_line = line
      local cursor = vim.api.nvim_win_get_cursor(0)
      local col = cursor[2]
      line = cursor[1]

      if line ~= old_line and col == 0 then
        local line_text = vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]

        vim.o.background = target_background
        vim.cmd("colorscheme " .. line_text)
      end
    end,
  })

  vim.api.nvim_create_autocmd({ "BufLeave" }, {
    buffer = buf,
    callback = function()
      if not accepting then
        pickle_colorscheme = vim.g.colors_name

        vim.cmd("colorscheme " .. orig_colorscheme)
      end
      accepting = false
    end,
  })

  vim.keymap.set(
    "n",
    "<CR>",
    ---@return string
    function()
      local cursor = vim.api.nvim_win_get_cursor(0)
      local col = cursor[2]
      local curr_line = cursor[1]

      if col == 0 then
        local line_text = vim.api.nvim_buf_get_lines(0, curr_line - 1, curr_line, true)[1]

        vim.o.background = target_background
        orig_colorscheme = ""
        pickle_colorscheme = ""
        target_background = ""
        vim.cmd("colorscheme " .. line_text)

        accepting = true
        return "<C-^>"
      else
        return ""
      end
    end,
    {
      buffer = buf,
      expr = true,
    }
  )

  vim.api.nvim_set_current_buf(buf)

  local colorschemes_i = 1
  for i, colorscheme in ipairs(colorschemes) do
    if colorscheme == vim.g.colors_name then
      colorschemes_i = i
      break
    end
  end

  vim.api.nvim_win_set_cursor(0, { colorschemes_i, 0 })
end

function M.setup()
  vim.api.nvim_create_user_command("Bpick", M.pick, { desc = "Preview and pick a color interactively." })
end

return M
