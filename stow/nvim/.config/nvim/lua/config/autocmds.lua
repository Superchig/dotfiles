-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

vim.cmd([[autocmd Filetype markdown setlocal textwidth=80 colorcolumn=+0]])

vim.cmd([[autocmd Filetype c,cpp,objc,objcpp setlocal shiftwidth=4 tabstop=4]])
vim.cmd([[autocmd Filetype c,cpp,objc,objcpp setlocal commentstring=//\ %s]])
vim.cmd([[autocmd Filetype c,cpp,objc,objcpp setlocal makeprg=just\ build]])

vim.cmd([[autocmd Filetype d setlocal shiftwidth=4 tabstop=4]])
vim.cmd([[autocmd Filetype d setlocal commentstring=//\ %s]])
vim.cmd([[autocmd Filetype d set makeprg=dub\ build]])
vim.cmd([[autocmd Filetype d lua SetDErrorFormat()]])
vim.cmd([[autocmd BufWritePost *.d silent !dfmt -i <afile>]])
vim.cmd([[autocmd BufWritePost *.d lua RunMakeAsync()]])
vim.cmd([[autocmd QuickfixCmdPost make cwindow]])

vim.cmd([[autocmd Filetype zig setlocal makeprg=zig\ build\ -Dno-bin\ -fincremental]])

vim.cmd([[autocmd Filetype shaderslang setlocal shiftwidth=4 tabstop=4]])

vim.cmd([[autocmd Filetype glsl setlocal shiftwidth=4 tabstop=4]])

vim.cmd([[autocmd Filetype make setlocal shiftwidth=4 tabstop=4]])

vim.cmd([[autocmd Filetype go setlocal shiftwidth=4 tabstop=4]])

vim.cmd([[autocmd Filetype cs setlocal shiftwidth=4 tabstop=4]])

vim.cmd([[autocmd Filetype js,json setlocal shiftwidth=2 tabstop=2]])

vim.cmd([[autocmd Filetype odin setlocal noexpandtab tabstop=4 shiftwidth=4]])

vim.cmd([[autocmd Filetype just setlocal shiftwidth=4]])

vim.cmd([[autocmd Filetype fish setlocal shiftwidth=4]])

vim.cmd([[autocmd BufNewFile,BufRead *.cls setlocal filetype=apex]])

vim.cmd([[autocmd Filetype norg setlocal textwidth=80 noautoindent nosmartindent shiftwidth=1 tabstop=4]])
vim.cmd([[autocmd Filetype norg lua Snacks.indent.disable()]])

vim.cmd([[autocmd Filetype org setlocal textwidth=80 foldlevel=1000]])

vim.cmd(
  [[autocmd BufNewFile,BufRead *.templ setlocal filetype=templ commentstring=//\ %s autoindent cindent nosmartindent]]
)

-- NOTE(Chris): Not sure where else to put this
if jit.os == "OSX" then
  vim.cmd([[let $CC = "gcc"]])

  -- NOTE(Chris): This will disable transparency in the autocomplete drop-down menu
  -- iTerm2 doesn't handle text transparency very well
  vim.opt.pumblend = 0
end

-- NOTE(Chris): On Windows, you can probably install Zig, if you want to build Treesitter parsers

function Get_bufs_loaded()
  local bufs_loaded = {}

  for i, buf_hndl in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf_hndl) then
      bufs_loaded[i] = buf_hndl
    end
  end

  return bufs_loaded
end

function Close_other_bufs()
  local bufs_loaded = Get_bufs_loaded()
  local curr_buf_hndl = vim.api.nvim_get_current_buf()

  for _, buf_hndl in pairs(bufs_loaded) do
    if buf_hndl ~= curr_buf_hndl then
      vim.api.nvim_buf_delete(buf_hndl, {})
    end
  end
end

vim.api.nvim_create_user_command("Bonly", Close_other_bufs, { desc = "Delete other buffers" })

-- This can broadly replace the functionality of zM for Markdown files
function NotesHeaders(regex)
  regex = regex or "^# .*$"
  vim.cmd([[vimgrep /]] .. regex .. [[/j %]])
  -- cmd([[vimgrep /^# .*$/j %]])
  vim.o.foldlevel = 100
  require("fzf-lua").quickfix()
end

-- This runs the macro @a, waits a second, and then navigates to the next error
function AtAOnError(count)
  if count == nil then
    count = 1
  end

  for i = 1, count do
    print("Handling run ", i)

    vim.cmd("normal @a")
    vim.cmd("sleep 200ms")
    vim.diagnostic.jump({ count = 1, float = true, severity = vim.diagnostic.severity.ERROR })
  end

  print("Done with AtAOnError")
end

vim.cmd([[autocmd BufRead,BufNewFile zoom_items.md nnoremap zM <cmd>lua NotesHeaders()<cr>]])
vim.cmd([[autocmd BufRead,BufNewFile notes.md nnoremap zM <cmd>lua NotesHeaders('^#\\+ .*$')<cr>]])

function SetDErrorFormat()
  vim.o.errorformat = [[%f(%l\,%c): %m]]
end

DJobs = {}

function RunMakeAsync()
  local i = 1
  while #DJobs > 0 do
    local job_id = table.remove(DJobs, i)
    vim.fn.jobstop(job_id)
  end

  -- Modified from https://phelipetls.github.io/posts/async-make-in-nvim-with-lua/
  local lines = {}
  local makeprg = vim.o.makeprg
  if not makeprg then
    return
  end
  local errorformat = vim.o.errorformat
  local cmd = vim.fn.expandcmd(makeprg)

  local function on_event(job_id, data, event)
    if event == "stdout" or event == "stderr" then
      if data then
        vim.list_extend(lines, data)
      end
    end
    if event == "exit" then
      local job_present = false
      for _, j in ipairs(DJobs) do
        if j == job_id then
          job_present = true
          break
        end
      end

      if not job_present then
        return
      end

      vim.fn.setqflist({}, " ", {
        title = cmd,
        lines = lines,
        efm = errorformat,
      })
      vim.api.nvim_command("doautocmd QuickFixCmdPost")

      QuickfixToDiagnostics()

      local win_id = vim.api.nvim_get_current_win()
      local buf_id = vim.api.nvim_win_get_buf(win_id)

      vim.cmd("cwindow")

      vim.api.nvim_set_current_win(win_id)
      vim.api.nvim_win_set_buf(win_id, buf_id)
    end
  end

  local job_id = vim.fn.jobstart(
    cmd,
    { on_stderr = on_event, on_stdout = on_event, on_exit = on_event, stdout_buffered = true, stderr_buffered = true }
  )

  table.insert(DJobs, job_id)
end

function QuickfixToDiagnostics()
  -- Create a new diagnostic namespace
  local ns = vim.api.nvim_create_namespace("quickfix_diagnostics")

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

if vim.env.SUPERCHIG_LAZYVIM_CMD ~= nil then
  vim.schedule(function()
    vim.cmd(vim.env.SUPERCHIG_LAZYVIM_CMD)
  end)
end
