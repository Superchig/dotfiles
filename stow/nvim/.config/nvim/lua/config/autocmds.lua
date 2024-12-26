-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

vim.cmd([[autocmd Filetype markdown set textwidth=78 colorcolumn=+0]])

vim.cmd([[autocmd Filetype c,cpp set shiftwidth=4 tabstop=4]])
vim.cmd([[autocmd Filetype c,cpp set commentstring=//\ %s]])

vim.cmd([[autocmd Filetype make set shiftwidth=4 tabstop=4]])

vim.cmd([[autocmd Filetype go set shiftwidth=4 tabstop=4]])

vim.cmd([[autocmd Filetype js,json set shiftwidth=2 tabstop=2]])

vim.cmd([[autocmd BufNewFile,BufRead *.templ set filetype=templ commentstring=//\ %s autoindent cindent nosmartindent]])

-- NOTE(Chris): Not sure where else to put this
if vim.loop.os_uname().sysname == "Darwin" then
  vim.cmd([[let $CC = "gcc"]])

  -- NOTE(Chris): This will disable transparency in the autocomplete drop-down menu
  -- iTerm2 doesn't handle text transparency very well
  vim.opt.pumblend = 0
elseif vim.loop.os_uname().sysname == "Linux" then
  -- NOTE(Chris): This background color overrides the values in
  -- https://github.com/ellisonleao/gruvbox.nvim
  vim.cmd([[hi LspReferenceText guibg=#57514e]])
  vim.cmd([[hi LspReferenceRead guibg=#57514e]])
  vim.cmd([[hi LspReferenceWrite guibg=#57514e]])
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
