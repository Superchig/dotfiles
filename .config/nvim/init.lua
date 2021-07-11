--- vim API shortcuts
local execute = vim.api.nvim_command
local fn = vim.fn
local cmd = vim.cmd
local opt = vim.opt

--- Install Packer by default
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  execute 'packadd packer.nvim'
end

require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- TODO(Chris): Consider replacing this with a Neovim-specific
  -- implementation, since this config file embraces Neovim in its
  -- entirety.
  use 'tpope/vim-commentary'

  use {'npxbr/gruvbox.nvim', requires = {"rktjmp/lush.nvim"}}

  use {'nvim-treesitter/nvim-treesitter', config = function()
    vim.cmd('TSUpdate')
  end}

  use {'ojroques/nvim-hardline'}

  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }

  use {'Superchig/vim-markdown', ft = 'markdown', config = function()
    vim.g.vim_markdown_new_list_item_indent = 2
    vim.g.vim_markdown_folding_style_pythonic = 1
    vim.g.vim_markdown_folding_level = 6
    -- Disable concealing of markdown syntax
    vim.g.vim_markdown_conceal = 0
  end}

  use {'reedes/vim-pencil', ft = {'', "markdown"}}

  -- use 'nvim-treesitter/nvim-treesitter-textobjects'
end)

require('nvim-treesitter.configs').setup({
  highlight = {
    enable = true,
    disable = "markdown"
  }
})

require('lualine').setup()

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
opt.completeopt = 'menuone,noselect'

--- Set keybindings
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map('n', '<C-s>', ':w<esc>')
map('i', '<C-s>', '<C-o>:w<esc>')

map('n', '<S-k>', 'gT')

-- This is a workaround until cabbrev has a direct Lua API equivalent
execute([[
cabbrev tn tabnew
]])

cmd('cabbrev ev e ~/.config/nvim/init.lua')

execute([[
cabbrev twospace set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
cabbrev fulltab set tabstop=8 softtabstop=0 noexpandtab shiftwidth=8 nosmarttab
]])

cmd('autocmd Filetype markdown set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')
cmd('autocmd Filetype markdown set textwidth=78 colorcolumn=+0')

cmd('autocmd Filetype lua set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')

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

-- Apparently this function is not implemented yet
-- vim.api.nvim_set_hl(0, "Normal", {guibg=nil; ctermbg = nil})
