return {
  "nvim-treesitter/nvim-treesitter",
  opts = {
    -- Disable highlighting for markdown, since we can just use Superchig/vim-markdown
    highlight = { disable = { "markdown" } },
  },
}
