-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.o.timeoutlen = 0

vim.opt.completeopt = "menuone,noinsert,noselect"
vim.opt.hlsearch = false
vim.opt.conceallevel = 0

if vim.g.neovide then
  -- vim.g.neovide_transparency = 0.8

  if vim.loop.os_uname().sysname == "Linux" then
    vim.o.guifont = "Iosevka Nerd Font:h12,Consolas:h12,Inconsolata:h12"
  else
    vim.o.guifont = "Iosevka Term:h12,Consolas:h12,Inconsolata:h12"
  end
end
