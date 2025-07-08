local M = {}
local job_id = nil
local ns = vim.api.nvim_create_namespace("zig-watch")

-- Parse Zig error messages from a line of output
local function parse_error(line)
  -- Match pattern: file:line:col: error: message
  local pattern = "([^:]+):(%d+):(%d+):%s*error:%s*(.+)"
  local file, lnum, col, message = line:match(pattern)
  if file and lnum and col and message then
    return {
      filename = file,
      lnum = tonumber(lnum),
      col = tonumber(col),
      text = message,
      type = "E",
    }
  end
  return nil
end

-- Update quickfix list with parsed errors
local function update_quickfix(errors)
  vim.fn.setqflist({}, "r", { title = "Zig Build Errors", items = errors })

  local win_id = vim.api.nvim_get_current_win()
  local buf_id = vim.api.nvim_win_get_buf(win_id)

  if #errors > 0 then
    vim.cmd("copen")
  else
    vim.cmd("cclose")
  end

  vim.api.nvim_set_current_win(win_id)
  vim.api.nvim_win_set_buf(win_id, buf_id)
end

local function quickfix_to_diagnostics()
  -- Clear any existing diagnostics in this namespace
  vim.diagnostic.reset(ns)

  -- Get the quickfix list
  local qf_list = vim.fn.getqflist()

  -- Convert quickfix items to diagnostics
  local diagnostics = {}
  for _, item in ipairs(qf_list) do
    -- Skip invalid items
    if item.valid == 1 and item.bufnr > 0 then
      local diag = {
        bufnr = item.bufnr,
        lnum = item.lnum - 1, -- Quickfix lines are 1-based, diagnostics are 0-based
        col = item.col - 1, -- Quickfix columns are 1-based, diagnostics are 0-based
        message = item.text,
        severity = vim.diagnostic.severity.ERROR, -- Default to ERROR; adjust as needed
        source = "quickfix",
      }
      local list = diagnostics[item.bufnr]

      if not list then
        list = {}
        diagnostics[item.bufnr] = list
      end

      table.insert(list, diag)
    end
  end

  for bufnr, diag_list in pairs(diagnostics) do
    -- Set diagnostics in the namespace
    vim.diagnostic.set(ns, bufnr, diag_list, {})
  end
end

-- Handle stdout data from the job
---@diagnostic disable-next-line: unused-local
local function on_stderr(chan_id, data, name)
  local errors = {}
  for _, line in ipairs(data) do
    if line and line ~= "" then
      local error = parse_error(line)
      if error then
        table.insert(errors, error)
      end
    end
  end
  vim.schedule(function()
    update_quickfix(errors)
    quickfix_to_diagnostics()
  end)
end

-- Start the Zig build watch command
function M.start_watcher(opts)
  if job_id then
    if not opts["mute_start_messages"] then
      vim.notify("Zig watcher is already running", vim.log.levels.WARN)
    end
    return
  end

  local cmd = {
    "zig",
    "build",
    "-Dno-bin",
    "-fincremental",
    "--watch",
    "--color",
    "off",
    "--prominent-compile-errors",
  }

  job_id = vim.fn.jobstart(cmd, {
    -- on_stdout = on_stderr,
    on_stderr = on_stderr,
    ---@diagnostic disable-next-line: unused-local
    on_exit = function(chan_id, code, event)
      vim.schedule(function()
        vim.notify("Zig watcher stopped with code " .. code, vim.log.levels.INFO)
        job_id = nil
        update_quickfix({})
      end)
    end,
  })

  if job_id <= 0 then
    vim.notify("Failed to start Zig watcher", vim.log.levels.ERROR)
    job_id = nil
  else
    if not opts["mute_start_messages"] then
      vim.notify("Zig watcher started", vim.log.levels.INFO)
    end
  end
end

-- Stop the Zig build watch command
function M.stop_watcher()
  if job_id then
    vim.fn.jobstop(job_id)
    job_id = nil
    vim.notify("Zig watcher stopped", vim.log.levels.INFO)
    update_quickfix({})
  else
    vim.notify("No Zig watcher running", vim.log.levels.WARN)
  end
end

-- Toggle the watcher
function M.toggle_watcher()
  if job_id then
    M.stop_watcher()
  else
    M.start_watcher()
  end
end

-- Setup function to define commands
function M.setup(opts)
  vim.api.nvim_create_user_command("ZigWatchStart", M.start_watcher, {})
  vim.api.nvim_create_user_command("ZigWatchStop", M.stop_watcher, {})
  vim.api.nvim_create_user_command("ZigWatchToggle", M.toggle_watcher, {})

  if opts["autocmd"] then
    vim.api.nvim_create_autocmd({ "FileType" }, {
      pattern = "zig",
      callback = function(ev)
        if ev.file ~= "build.zig" then
          M.start_watcher({ mute_start_messages = true })
        end
      end,
    })
  end
end

return M
