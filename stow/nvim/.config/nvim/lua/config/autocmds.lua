-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- TODO(Chris): Put this in the config part of some plugin configuration
vim.cmd([[autocmd Filetype markdown lua require('cmp').setup.buffer { enabled = false }]])
