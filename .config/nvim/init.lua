--- vim API shortcuts
local execute = vim.api.nvim_command
local fn = vim.fn
local cmd = vim.cmd
local opt = vim.opt

--- Zany helper functions
function isModuleAvailable(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(name)
      if type(loader) == 'function' then
        package.preload[name] = loader
        return true
      end
    end
    return false
  end
end

-- Function to attach completion when setting up lsp
local on_attach = function(client)
  require('completion').on_attach(client)
end

--- Install Packer by default
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  cmd 'packadd packer.nvim'
end

-- Use :PackerCompile and then :PackerInstall when installing plugins
require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  -- use {'npxbr/gruvbox.nvim', requires = {"rktjmp/lush.nvim"}}
  use 'morhetz/gruvbox'
  use 'windwp/nvim-autopairs'
  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }
  -- Use :TSUpdate to update parsers
  -- Use :TSInstall to install specific parsers
  use 'nvim-treesitter/nvim-treesitter'
  use 'neovim/nvim-lspconfig'
  use 'alx741/vim-rustfmt'  -- Provides :Rustfmt and related commands
  -- use 'nvim-lua/lsp_extensions.nvim'
  -- TODO(Chris): Move to nvim-compe
  use 'nvim-lua/completion-nvim'
  -- use 'glepnir/lspsaga.nvim'
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    -- cmd = "Telescope"
  }
  use {'Superchig/vim-markdown'}
  use {'reedes/vim-pencil'}
  use {'kevinhwang91/nvim-bqf'}
  -- Provides (the most ubiquitous) readline bindings for Vim
  use {'tpope/vim-rsi'}

  -- use 'nvim-treesitter/nvim-treesitter-textobjects'
end)

if isModuleAvailable('nvim-treesitter.configs') then
  require('nvim-treesitter.configs').setup({
    ensure_installed = {"lua", "ruby", "python"},

    highlight = {
      enable = true,
      disable = {'markdown'}
    }
  })
end

require('nvim-autopairs').setup()

require('lualine').setup({
  sections = {
    -- The default for lualine_x is {'encoding', 'fileformat', 'filetype'}
    lualine_x = {
      {
        'diagnostics',
        sources = {'nvim_lsp'},
        symbols = {error = 'E', warn = 'W', info = 'I', hint = 'H'}
      },
      'encoding',
      'filetype'
    }
  }
})

-- This requires rust-analyzer to be installed separately
require('lspconfig').rust_analyzer.setup({ on_attach=on_attach })

-- Enable diagnostics
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    underline = true,
    virtual_text = true,
    signs = true,
    update_in_insert = true,
  }
)

-- https://github.com/nvim-telescope/telescope.nvim#customize-default-builtin-behavior
require('telescope').setup({
  defaults = {
    mappings = {
      i = {
        ['<C-g>'] = require('telescope.actions').close
      }
    }
  }
})

-- Disable Rust formatting on save
vim.g.rustfmt_on_save = 0

-- Enable Rust folding (though all folds are open by default)
-- This is from the ft_rust plugin
vim.g.rust_fold = 1

vim.g.vim_markdown_new_list_item_indent = 2
vim.g.vim_markdown_folding_style_pythonic = 1
vim.g.vim_markdown_folding_level = 6
-- Disable concealing of markdown syntax
vim.g.vim_markdown_conceal = 0

--- Options
-- Equivalent to set termguicolors
opt.termguicolors = true
-- Equivalent to set clipboard^=unnamedplus
opt.clipboard:prepend('unnamedplus')
-- opt.completeopt:remove('preview')
opt.ignorecase = true
opt.hlsearch = false
opt.number = true
opt.relativenumber = true
opt.inccommand = 'nosplit'
-- Set completeopt to have a better completion experience
-- :help completeopt
-- menuone: popup even when there's only one match
-- noinsert: Do not insert text until a selection is made
-- noselect: Do not select, force user to select one from the menu
opt.completeopt = 'menuone,noinsert,noselect'
-- Avoid showing extra messages when using completion
opt.shortmess:append('c')
opt.mouse = 'a'
opt.cursorline = true
-- opt.diffopt = 'internal,filler,closeoff'

--- Set keybindings
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map('n', '<C-s>', ':w<esc>')
map('i', '<C-s>', '<C-o>:w<esc>')

-- Make more convenient pane movement bindings
-- It's worth using :help nvim_set_keymap to check out options
-- like silent (which are just from vim's map commands anyways)
map('n', '<C-k>', '<C-w>k', {noremap = true, silent = true})
map('n', '<C-j>', '<C-w>j', {noremap = true, silent = true})
map('n', '<C-h>', '<C-w>h', {noremap = true, silent = true})
map('n', '<C-l>', '<C-w>l', {noremap = true, silent = true})

map('n', '<SPACE>', '<Nop>')
-- NOTE(Chris): We may need to move this to the beginning of the file
-- Equivalent to let mapleader = " "
vim.g.mapleader = ' '

-- Taken from https://github.com/nvim-telescope/telescope.nvim#usage
map('n', '<leader>ff', '<cmd>Telescope find_files<cr>')
map('n', '<leader>fg', '<cmd>Telescope live_grep<cr>')
map('n', '<leader>fb', '<cmd>Telescope buffers<cr>')
map('n', '<leader>fh', '<cmd>Telescope help_tags<cr>')

map('n', '<C-p>', '<cmd>Telescope find_files<cr>')

map('n', '<leader>ft', '<cmd>lua require("telescope.builtin").treesitter()<cr>')

-- LSP completion
cmd([[inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"]])
cmd([[inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"]])

-- Use <Tab> as trigger keys
map('i', '<Tab>', '<Plug>(completion_smart_tab)', {noremap = false})
map('i', '<S-Tab>', '<Plug>(completion_smart_s_tab)', {noremap = false})

map('n', '<leader>q', '<cmd>copen<CR>', {noremap = true, silent = true})
map('n', '<leader>n', '<cmd>cnext<CR>', {noremap = true, silent = true})
map('n', '<leader>p', '<cmd>cprev<CR>', {noremap = true, silent = true})

-- Code navigation shortcuts for rust
-- Note(Chris): Using execute in this way actually causes these autocmd's to be added every time
-- a rust buffer is entered, so we probably shouldn't use it at all, (just in case)
-- execute([[
-- autocmd FileType rust nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
-- ]])

-- -- There doesn't seem to be a way to make this non-focusable
cmd('autocmd FileType rust nnoremap <silent> gh <cmd>lua vim.lsp.buf.hover()<CR>')
cmd([[autocmd FileType rust nnoremap <silent> ge m'<cmd>lua vim.lsp.diagnostic.goto_next({enable_popup = false})<CR>]])
cmd([[autocmd FileType rust nnoremap <silent> gE m'<cmd>lua vim.lsp.diagnostic.goto_prev({enable_popup = false})<CR>]])
cmd('autocmd FileType rust nnoremap <silent> gs <cmd>lua vim.lsp.diagnostic.show_line_diagnostics({focusable = false})<CR>')
cmd('autocmd FileType rust nnoremap <silent> <F2>    <cmd>lua vim.lsp.buf.rename()<CR>')
-- <F24> seems to be what <S-F12> maps to with neovim + kitty/gnome-terminal
-- TODO(Chris): Implement previewing of references
cmd('autocmd FileType rust nnoremap <silent> <F24>   <cmd>lua vim.lsp.buf.references()<CR>')
-- cmd('autocmd FileType rust nnoremap <silent> <F24>   <cmd>lua require("telescope.builtin").lsp_references()<CR>')
cmd([[autocmd FileType rust nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>]])
-- -- If we set this to 'no' rather than 'number', it'll disable this entirely
cmd('autocmd FileType rust set signcolumn=number')

-- cmd([[autocmd FileType rust nnoremap <silent> gh    <cmd>lua require('lspsaga.hover').render_hover_doc()<CR>]])
-- cmd([[autocmd FileType rust nnoremap <silent> ge    <cmd>lua require('lspsaga.diagnostic').lsp_jump_diagnostic_next()<CR>]])
-- cmd([[autocmd FileType rust nnoremap <silent> gE    <cmd>lua require('lspsaga.diagnostic').lsp_jump_diagnostic_prev()<CR>]])
-- cmd([[autocmd FileType rust nnoremap <silent> gs    <cmd>lua require('lspsaga.diagnostic').show_line_diagnostics()<CR>]])
-- cmd([[autocmd FileType rust nnoremap <silent> <F2>  <cmd>lua require('lspsaga.rename').rename()<CR>]])
-- -- <F24> seems to be what <S-F12> maps to with neovim + kitty/gnome-terminal
-- cmd([[autocmd FileType rust nnoremap <silent> <F24> <cmd>lua require'lspsaga.provider'.lsp_finder()<CR>]])

-- This is a workaround until cabbrev has a direct Lua API equivalent
cmd('cabbrev tn tabnew')
cmd('nnoremap <S-k> gt')
cmd('nnoremap <S-j> gT')

cmd('cabbrev ev e ~/.config/nvim/init.lua')

cmd('cabbrev soi source ~/.config/nvim/init.lua')

cmd('cabbrev twospace set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')
cmd('cabbrev fulltab set tabstop=8 softtabstop=0 noexpandtab shiftwidth=8 nosmarttab')

cmd('autocmd Filetype markdown set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')
cmd('autocmd Filetype markdown set textwidth=78 colorcolumn=+0')

cmd('autocmd Filetype lua set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')

cmd('autocmd TextYankPost * lua vim.highlight.on_yank {on_visual = false}')

--- Set colorscheme
cmd('colorscheme gruvbox')

-- Set background to be transparent in rather contorted way
-- From https://www.reddit.com/r/neovim/comments/m0gyip/nvim_initlua_not_respecting_highlight_commands/
-- Perhaps I should file a bug report as to why simply using vim.cmd doesn't work?

function hi(group, opts)
  local c = "highlight " .. group
  for k, v in pairs(opts) do
    c = c .. " " .. k .. "=" .. v
  end
  vim.cmd(c)
end

function create_augroup(name, autocmds)
  cmd = vim.cmd
  cmd('augroup ' .. name)
  cmd('autocmd!')
  for _, autocmd in ipairs(autocmds) do
    cmd('autocmd ' .. table.concat(autocmd, ' '))
  end
  cmd('augroup END')
end

function HighlightNone()
  hi("Normal", {ctermbg = "NONE", guibg = "NONE"})
end

create_augroup("HighlightNone", {
  {"ColorScheme", "*", "lua HighlightNone()"}
})

-- -- This function and create_augroup call enable undercurl on diagnostics
function Undercurl()
  hi("LspDiagnosticsUnderlineWarning", {guifg="NONE", ctermfg="NONE", cterm="undercurl", gui="undercurl"})
  hi("LspDiagnosticsUnderlineError", {guifg="NONE", ctermfg="NONE", cterm="undercurl", gui="undercurl"})
end

create_augroup("Undercurl", {
  {"ColorScheme", "*", "lua Undercurl()"}
})

-- This command would enable undercurl for lsp underline diagnostics, but these
-- sorts of highlight commands don't work in init.lua by default.
-- cmd("hi LspDiagnosticsUnderlineWarning guifg=NONE ctermfg=NONE cterm=undercurl gui=undercurl")

-- Apparently this function is not implemented yet
-- vim.api.nvim_set_hl(0, "Normal", {guibg=nil; ctermbg = nil})
