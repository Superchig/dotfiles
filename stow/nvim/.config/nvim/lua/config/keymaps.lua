-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "<leader>fc", "<cmd>e ~/.config/nvim<cr>")

vim.keymap.set("n", "gn", "<cmd>bn<cr>", { desc = "Next buffer" })
vim.keymap.set("n", "gp", "<cmd>bp<cr>", { desc = "Previous buffer" })

-- vim.keymap.set("n", "K", "<cmd>BufferLineCycleNext<cr>", { desc = "Next buffer" })
-- vim.keymap.set("n", "J", "<cmd>BufferLineCyclePrev<cr>", { desc = "Prev buffer" })

vim.keymap.set("n", "gh", vim.lsp.buf.hover, { desc = "Show LSP hover info" })
vim.keymap.set("n", "<leader>=", vim.lsp.buf.format, { desc = "Format file with LSP" })

vim.keymap.set("n", "<C-h>", "<C-w>h", { silent = true, desc = "Move to left window" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { silent = true, desc = "Move to right window" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { silent = true, desc = "Move to above window" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { silent = true, desc = "Move to below window" })
