-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "<leader>fc", "<cmd>e ~/.config/nvim<cr>")

vim.keymap.set("n", "gn", "<cmd>bn<cr>")
vim.keymap.set("n", "gp", "<cmd>bp<cr>")

vim.keymap.set("n", "gh", vim.lsp.buf.hover)

-- Restore the default behavior of `gw`
vim.keymap.del("n", "gw")

-- This can broadly replace the functionality of zM for Markdown files
function NotesHeaders(regex)
  regex = regex or "^# .*$"
  vim.cmd([[vimgrep /]] .. regex .. [[/j %]])
  -- cmd([[vimgrep /^# .*$/j %]])
  -- The ivy theme is used to place the prompt at the top, reversing the
  -- result order
  vim.o.foldlevel = 0
  require("telescope.builtin").quickfix(require("telescope.themes").get_ivy())
end

vim.cmd([[autocmd BufRead,BufNewFile zoom_items.md nnoremap zM :lua NotesHeaders()<cr>]])
vim.cmd([[autocmd BufRead,BufNewFile notes.md nnoremap zM :lua NotesHeaders('^#\\+ .*$')<cr>]])
