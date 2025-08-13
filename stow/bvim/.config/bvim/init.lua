--- Pluginless configuration

vim.o.number = true
vim.o.relativenumber = true
vim.o.clipboard = "unnamedplus"
vim.o.tabstop = 4
vim.o.expandtab = true
vim.o.wrap = false
vim.o.cursorline = true
vim.o.hlsearch = false
vim.o.smartcase = true
vim.o.ignorecase = true
vim.o.updatetime = 200

if jit.os == "OSX" then
  vim.o.shell = "fish"
end

vim.opt.completeopt = { "menuone", "noinsert", "noselect" }

vim.g.mapleader = " "

local init_file = vim.fn.stdpath("config") .. "/init.lua"

vim.cmd([[
  function! InitLuaFold()
    if getline(v:lnum) =~ '^---\s'
      return '>1'
    endif
    return '='
  endfunction
]])

vim.cmd([[autocmd BufEnter init_lua setlocal foldexpr=InitLuaFold()]])
vim.cmd([[autocmd FileType lua setlocal expandtab shiftwidth=2 tabstop=2]])
vim.cmd([[autocmd TextYankPost * silent! lua vim.hl.on_yank()]])
vim.cmd([[
	augroup vimrc-incsearch-highlight
	  autocmd!
	  autocmd CmdlineEnter /,\? :set hlsearch
	  autocmd CmdlineLeave /,\? :set nohlsearch
	augroup END
]])
vim.cmd("cabbrev ev e" .. init_file)

vim.keymap.set("n", "<C-S>", "<cmd>write<CR>", { desc = "Save current buffer" })
vim.keymap.set("n", "<Leader>bd", function()
  vim.api.nvim_buf_delete(0, {})
end, { desc = "Delete (unload) buffer" })
vim.keymap.set("n", "<C-J>", "<C-W>j", { desc = "Switch to window downwards" })
vim.keymap.set("n", "<C-K>", "<C-W>k", { desc = "Switch to window upwards" })
vim.keymap.set("n", "<C-H>", "<C-W>h", { desc = "Switch to window leftwards" })
vim.keymap.set("n", "<C-L>", "<C-W>l", { desc = "Switch to window rightwards" })
vim.keymap.set("n", "<C-S-J>", "<C-W>J", { desc = "Move window downwards" })
vim.keymap.set("n", "<C-S-K>", "<C-W>K", { desc = "Move window upwards" })
vim.keymap.set("n", "<C-S-H>", "<C-W>H", { desc = "Move window leftwards" })

vim.keymap.set("n", "<Leader><Leader>", ":ls<Cr>:b ", { desc = "Switch buffers" })

--- bpack
local bpack = require("bpack")
bpack.setup({
  --- In-house plugins
  {
    name = "bstatusline",
    type = "vendored",
  },
  {
    name = "bpairs",
    type = "vendored",
  },
  {
    name = "bmason",
    type = "vendored",
  },
  {
    name = "bpickle",
    type = "vendored",
  },
  {
    name = "bcomplete",
    type = "vendored",
  },

  --- Color schemes
  {
    name = "Mofiqul/dracula.nvim",
  },
  {
    name = "catppuccin",
    type = "git",
    url = "https://github.com/catppuccin/nvim.git",
  },
  {
    name = "loctvl842/monokai-pro.nvim",
  },
  {
    name = "folke/tokyonight.nvim",
  },
})

--- Color scheme
local dracula = require("dracula")
dracula.setup({
  overrides = {
    LspReferenceText = { bg = "#4e5567" },
    LspReferenceRead = { link = "LspReferenceText" },
    LspReferenceWrite = { link = "LspReferenceRead" },
  },
})
vim.cmd("colorscheme dracula")
-- local lualine_dracula = require("lualine.themes.dracula-nvim")

--- Statusline
vim.o.statusline = require("bstatusline")
vim.o.showmode = false
vim.o.laststatus = 3

--- Pairs
require("bpairs").setup({
  { "(", ")" },
  { "{", "}" },
  { "[", "]" },
  { '"', '"' },
  -- { "'", "'" },
})

--- bmason
require("bmason").setup()

--- bpickle
require("bpickle").setup()

--- bcomplete
local bcomplete = require("bcomplete")

--- LSP Config

---@type string[]
local lua_annotations = { vim.env.VIMRUNTIME .. "/lua/vim" }
for path in string.gmatch(vim.o.rtp, ",") do
  table.insert(lua_annotations, path .. "/lua")
end
vim.lsp.config["luals"] = {
  cmd = { "lua-language-server" },
  filetypes = { "lua" },
  root_markers = { { ".luarc.json", ".luarc.json" }, ".git" },
  settings = {
    Lua = {
      runtime = {
        version = "LuaJIT",
      },
      workspace = {
        library = lua_annotations,
      },
    },
  },
  on_attach = function()
    vim.keymap.set("n", "gd", vim.lsp.buf.definition, { desc = "Goto definition (LSP)" })
    vim.keymap.set("n", "gr", vim.lsp.buf.references, { desc = "View references (LSP)" })
    vim.keymap.set("n", "gh", vim.lsp.buf.hover, { desc = "Hover information (LSP)" })

    vim.keymap.set("n", "<Leader>=", vim.lsp.buf.format, { desc = "Format buffer (LSP)" })
    vim.keymap.set("n", "<Leader>cr", vim.lsp.buf.rename, { desc = "Rename symbol (LSP)" })
    vim.keymap.set("n", "<Leader>cd", vim.diagnostic.open_float, { desc = "Show diagnostics in floating window" })
    vim.keymap.set("n", "<Leader>ca", vim.lsp.buf.code_action, { desc = "Handle code actions if available" })

    vim.keymap.set("i", "<Tab>", bcomplete.tab_wrapper, { desc = "Get/next completion", expr = true, silent = true })
    vim.keymap.set("i", "<Esc>", bcomplete.esc_wrapper, { desc = "Exit completion", expr = true, silent = true })
    vim.keymap.set("i", "<CR>", bcomplete.cr_wrapper, { desc = "Accept completion", expr = true, silent = true })
    vim.keymap.set("i", "<C-N>", bcomplete.ctrl_n_wrapper, { desc = "Get/next completion", expr = true, silent = true })

    vim.o.omnifunc = "v:lua.vim.lsp.omnifunc"
    vim.o.foldmethod = "expr"
    -- vim.o.foldexpr = "v:lua.vim.lsp.foldexpr()"
    vim.o.foldlevel = 1000

    -- vim.cmd([[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]])
    -- vim.cmd([[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]])
    -- vim.cmd([[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]])

    -- print(vim.inspect(client.capabilities.textDocument.completion))

    if vim.env.BCOMPLETE then
      local augroup_id = vim.api.nvim_create_augroup("bcomplete", {})
      vim.api.nvim_create_autocmd("TextChangedI", {
        pattern = "*", -- Apply to all buffers
        callback = bcomplete.complete_on_text_change,
        group = augroup_id,
      })
    end
  end,
}

vim.cmd([[autocmd FileType lua lua vim.lsp.enable("luals")]])

vim.diagnostic.config({
  virtual_text = true,
  underline = true,
})

-- Treesitter

-- ---@class TreesitterParser
-- ---@field name string
-- ---@field ft? string
--
-- ---@type TreesitterParser[]
-- local treesitter_parsers = {
--   {
--     name = "lua",
--   },
-- }
--
-- for _, parser in ipairs(treesitter_parsers) do
--   local name = parser.name
--   local ft = parser.ft or parser.name
--
--   vim.treesitter.language.add(name)
--   vim.api.nvim_create_autocmd(
--     "FileType",
--     {
--       desc = "treesitter bvim " .. name,
--       pattern = ft,
--       callback = function(args)
--         vim.treesitter.start(args.buf, name)
--       end
--     }
--   )
-- end
