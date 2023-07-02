-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- TODO(Chris): Put this in the config part of some plugin configuration
vim.cmd([[autocmd Filetype markdown lua require('cmp').setup.buffer { enabled = false }]])

vim.cmd([[autocmd Filetype markdown set textwidth=78 colorcolumn=+0]])

vim.cmd([[autocmd Filetype cpp set shiftwidth=4 tabstop=4]])
vim.cmd([[autocmd Filetype cpp set commentstring=//\ %s]])

vim.cmd([[autocmd Filetype make set shiftwidth=4 tabstop=4]])

vim.cmd([[autocmd Filetype go set shiftwidth=4 tabstop=4]])

-- NOTE(Chris): Not sure where else to put this
if vim.loop.os_uname().sysname == 'Darwin'
then
  vim.cmd([[let $CC = "gcc-12"]])
end

-- NOTE(Chris): On Windows, you can probably install Zig, if you want to build Treesitter parsers
