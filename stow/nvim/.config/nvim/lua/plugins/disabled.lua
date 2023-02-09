return {
  -- The next two plugins modify the behavior of f and s
  { "ggandor/leap.nvim", enabled = false },
  { "ggandor/flit.nvim", enabled = false },
  -- I haven't takent the time to rebind this plugin to correspond to the
  -- vim-commentary mappings
  { "echasnovski/mini.surround", enabled = false },
  -- I've never gotten used to
  { "akinsho/bufferline.nvim", enabled = false },
  -- As of 2/4/2023, this doesn't work well with Neovide
  { "folke/noice.nvim", enabled = false },
  -- As of 2/4/2023, this doesn't support highlighting the author in TODO(author):
  -- https://github.com/folke/todo-comments.nvim/issues/10
  { "folke/todo-comments.nvim", enabled = false },
  { "RRethy/vim-illuminate", enabled = false },
}
