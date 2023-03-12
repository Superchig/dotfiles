return {
  -- Compare to https://www.lazyvim.org/plugins/treesitter#nvim-treesitter
  "nvim-treesitter/nvim-treesitter",
  opts = {
    -- Disable highlighting for markdown, since we can just use Superchig/vim-markdown
    highlight = { disable = { "markdown" } },
    ensure_installed = { "comment", "css", "lua", "ruby", "python", "typescript", "rust", "vim", "go", "cpp" },
  },
}
