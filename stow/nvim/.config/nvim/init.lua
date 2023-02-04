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

-- Use https://github.com/lewis6991/impatient.nvim
if isModuleAvailable('impatient') then
  require('impatient')
end

local bufmap = function(type, key, value)
  vim.api.nvim_buf_set_keymap(0,type,key,value,{noremap = true, silent = true});
end

-- Print contents of `tbl`, with indentation.
-- `indent` sets the initial level of indentation.
function tprint (tbl, indent)
  if not indent then indent = 0 end
  for k, v in pairs(tbl) do
    formatting = string.rep("  ", indent) .. k .. ": "
    if type(v) == "table" then
      print(formatting)
      tprint(v, indent+1)
    elseif type(v) == 'boolean' then
      print(formatting .. tostring(v))      
    elseif type(v) == 'function' then
      print(formatting .. '<lua function>')
    elseif type(v) == 'userdata' then
      print(formatting .. '<userdata value>')
    else
      print(formatting .. v)
    end
  end
end

-- Function to attach completion when setting up lsp
local on_attach = function(client)
  bufmap('n', 'gh', '<cmd>lua vim.lsp.buf.hover()<CR>')
  bufmap('n', 'ge', [[m'<cmd>lua vim.diagnostic.goto_next({enable_popup = false})<CR>]])
  bufmap('n', 'gE', [[m'<cmd>lua vim.diagnostic.goto_prev({enable_popup = false})<CR>]])
  bufmap('n', 'gs', '<cmd>lua vim.diagnostic.open_float({focusable = false})<CR>')
  bufmap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<CR>')
  bufmap('n', '<F24>', '<cmd>lua vim.lsp.buf.references()<CR>')
  bufmap('n', '<S-F12>', '<cmd>lua vim.lsp.buf.references()<CR>') -- For Windows/wezterm support
  bufmap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>')
  bufmap('n', 'gA', '<cmd>lua vim.lsp.buf.code_action()<CR>')
  cmd('setlocal signcolumn=number')

  bufmap('n','<leader>=', '<cmd>lua vim.lsp.buf.format({ async = true })<CR>')
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
  use {'wbthomason/packer.nvim'}
  use {'lewis6991/impatient.nvim'}
  use {'tpope/vim-commentary'}
  use {'tpope/vim-surround'}
  use {'npxbr/gruvbox.nvim', requires = {"rktjmp/lush.nvim"}}
  use {'shaunsingh/nord.nvim'}
  use {'shaunsingh/solarized.nvim'}
  use {'Mofiqul/dracula.nvim'}
  use {'Th3Whit3Wolf/one-nvim'}
  use {'ishan9299/modus-theme-vim'}
  -- use 'morhetz/gruvbox'
  use {'windwp/nvim-autopairs'}
  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }
  -- Use :TSUpdate to update parsers
  -- Use :TSInstall to install specific parsers
  use {'nvim-treesitter/nvim-treesitter'}
  use {'neovim/nvim-lspconfig'}
  use {'alx741/vim-rustfmt'}  -- Provides :Rustfmt and related commands

  use {'hrsh7th/cmp-nvim-lsp'}
  use {'hrsh7th/cmp-buffer'}
  use {'hrsh7th/cmp-path'}
  use {'hrsh7th/cmp-cmdline'}
  use {'hrsh7th/nvim-cmp'}
  use {'hrsh7th/vim-vsnip'}
  use {'hrsh7th/cmp-vsnip'}

  use {"ray-x/lsp_signature.nvim"}
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    -- cmd = "Telescope"
  }
  use {
    'nvim-telescope/telescope-ui-select.nvim',
    requires = {'nvim-telescope/telescope.nvim'}
  }
  use {'kevinhwang91/nvim-bqf'}
  -- Provides (the most ubiquitous) readline bindings for Vim
  use {'tpope/vim-rsi'}
  -- use 'nvim-treesitter/nvim-treesitter-textobjects'
  use {'mfussenegger/nvim-dap'}
  use {
    'rcarriga/nvim-dap-ui',
    requires = {'mfussenegger/nvim-dap'}
  }

  use {'simrat39/rust-tools.nvim', requires = {'nvim-lua/plenary.nvim'}}
  use {'Superchig/vim-markdown'}
  use {'dag/vim-fish'}
  use {'lervag/vimtex'}
  use {'ledger/vim-ledger'}

  use {
    'nvim-neorg/neorg',
    run = ":Neorg sync-parsers", -- This is the important bit!
    function()
      require('neorg').setup {
        load = {
          ["core.defaults"] = {}
        }
      }
    end,
  }
end)

if isModuleAvailable('nvim-treesitter.configs') then
  require('nvim-treesitter.configs').setup({
    ensure_installed = {
      "comment", -- Highlights comments with specific syntax
      "lua", "ruby", "python", "haskell", "go", "c", "latex", "rust", "jsonc",
      "org", "svelte", "typescript", "javascript", "css",
    },
    highlight = {
      enable = true,
      -- Use plasticboy/vim-markdown for highlighting, but Treesitter for
      -- code-folding
      disable = {'markdown'} ,
      -- Required for spellcheck, some LaTex highlights and code block highlights that do not have ts grammar
      additional_vim_regex_highlighting = {'org'},
    },
    textobjects = {
      select = {
        enable = true,
  
        -- Automatically jump forward to textobj, similar to targets.vim
        lookahead = true,
  
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ["af"] = "@function.outer",
          ["if"] = "@function.inner",
          ["ac"] = "@class.outer",
          ["ic"] = "@class.inner",
  
          -- Or you can define your own textobjects like this
          ["iF"] = {
            python = "(function_definition) @function",
            cpp = "(function_definition) @function",
            c = "(function_definition) @function",
            java = "(method_declaration) @function",
          },
        },
      },
      swap = {
        enable = true,
        swap_next = {
          ["<leader>a"] = "@parameter.inner",
        },
        swap_previous = {
          ["<leader>A"] = "@parameter.inner",
        },
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          ["]m"] = "@function.outer",
          ["]]"] = "@class.outer",
        },
        goto_next_end = {
          ["]M"] = "@function.outer",
          ["]["] = "@class.outer",
        },
        goto_previous_start = {
          ["[m"] = "@function.outer",
          ["[["] = "@class.outer",
        },
        goto_previous_end = {
          ["[M"] = "@function.outer",
          ["[]"] = "@class.outer",
        },
      },
    },
  })
end

require('lsp_signature').setup({
  bind = true,
  handler_opts = {
    border = "none"
  }
})

local cmp = require('cmp')

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  mapping = {
    ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
    ['<C-e>'] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'vsnip' }, -- For vsnip users.
    -- { name = 'luasnip' }, -- For luasnip users.
    -- { name = 'ultisnips' }, -- For ultisnips users.
    -- { name = 'snippy' }, -- For snippy users.
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- Prepare for lspconfig
local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

require('nvim-autopairs').setup()

require('lualine').setup({
  sections = {
    lualine_b = {
      'branch',
      {
        'diff',
        diff_color = {
          removed = { fg = 'orange' } -- This color is at least defined by gruvbox.nvim
        },
      },
      'diagnostics'
    },
  },
})

require('dapui').setup({
  icons = { expanded = "▾", collapsed = "▸" },
  mappings = {
    -- Use a table to apply multiple mappings
    expand = { "<CR>", "<2-LeftMouse>" },
    open = "o",
    remove = "d",
    edit = "e",
    repl = "r",
  },
  layouts = {
    {
      elements = {
        'scopes',
        'breakpoints',
        'stacks',
        'watches',
      },
      size = 40,
      position = 'left',
    }
  },
  {
    elements = {
      'repl',
      'console',
    },
    size = 10,
    position = 'bottom',
  },
  floating = {
    max_height = nil, -- These can be integers or a float between 0 and 1.
    max_width = nil, -- Floats will be treated as percentage of your screen.
    mappings = {
      close = { "q", "<Esc>" },
    },
  },
  windows = { indent = 1 },
})

require('rust-tools').setup({
  tools = {
    -- Disable inlay hints
    autoSetHints = false,
    -- Don't display hover actions in `gh` hover info
    hover_with_actions= false,
  },
  hover_actions = {
    border = "none",
  },
  server = {
    on_attach=on_attach,
    capabilities=capabilities,
    settings = {
      ["rust-analyzer"] = {
        -- https://users.rust-lang.org/t/how-to-use-clippy-in-vs-code-with-rust-analyzer/41881
        -- https://rust-analyzer.github.io/manual.html#nvim-lsp
        checkOnSave = {
          -- The default command is "check"
          command = 'clippy',
        },
      },
    },
  }
})

-- TODO(Chris): Put this repetitive lspconfig setup into a loop
-- This requires rust-analyzer to be installed separately
lspconfig = require('lspconfig')
lspconfig.hls.setup({
  on_attach=on_attach,
  capabilities=capabilities,
  root_dir=lspconfig.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", ".hlint.yaml"),
})
lspconfig.gopls.setup({
  on_attach=on_attach,
  capabilities=capabilities,
})
lspconfig.clangd.setup({
  on_attach=on_attach,
  capabilities=capabilities,
})
lspconfig.texlab.setup({
  on_attach=on_attach,
  capabilities=capabilities,
})
lspconfig.ocamllsp.setup({
  on_attach=on_attach,
  capabilities=capabilities,
})
lspconfig.zls.setup({
  on_attach=on_attach,
  capabilities=capabilities,
})
lspconfig.pyright.setup({
  on_attach=on_attach,
  capabilities=capabilities,
})
lspconfig.tsserver.setup({
  on_attach=on_attach,
  capabilities=capabilities,
})
lspconfig.svelte.setup({
  on_attach=on_attach,
  capabilities=capabilities,
})

local pid = vim.fn.getpid()
-- On linux/darwin if using a release build, otherwise under scripts/OmniSharp(.Core)(.cmd)
local omnisharp_bin = "/home/chiggie/Downloads/omnisharp/run"
-- on Windows
-- local omnisharp_bin = "/path/to/omnisharp/OmniSharp.exe"
lspconfig.omnisharp.setup({
  on_attach=on_attach,
  capabilities=capabilities,
  cmd = { omnisharp_bin, "--languageserver" , "--hostPID", tostring(pid) },
  root_dir = lspconfig.util.root_pattern("*.sln"),
})

-- Set up Idris 2 when loading relevant files using this hacky function and autocmd.
-- Normally, calling require('idris2').setup({}) leads to 220 extra ms in startup time
-- on an M.2 SSD machine.
function SetupIdris2()
  -- This also sets up the idris2 lsp
  require('idris2').setup({
    server={
      on_attach=on_attach,
      capabilities=capabilities,
    },
  })

  -- This essentially just calls what should already be called in the autocmd from
  -- the idris2 plugin.
  lspconfig["idris2_lsp"].manager.try_add()
end
cmd([[autocmd FileType idris2 ++once lua SetupIdris2()]])

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
  },
  extensions = {
    ["ui-select"] = {
      require("telescope.themes").get_dropdown()
    }
  },
})

-- To get ui-select loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require("telescope").load_extension("ui-select")

local dap = require('dap')
dap.adapters.go = function(callback, config)
  local stdout = vim.loop.new_pipe(false)
  local handle
  local pid_or_err
  local port = 38697
  local opts = {
    stdio = {nil, stdout},
    args = {"dap", "-l", "127.0.0.1:" .. port},
    detached = true
  }
  handle, pid_or_err = vim.loop.spawn("dlv", opts, function(code)
    stdout:close()
    handle:close()
    if code ~= 0 then
      print('dlv exited with code', code)
    end
  end)
  assert(handle, 'Error running dlv: ' .. tostring(pid_or_err))
  stdout:read_start(function(err, chunk)
    assert(not err, err)
    if chunk then
      vim.schedule(function()
        require('dap.repl').append(chunk)
      end)
    end
  end)
  -- Wait for delve to start
  vim.defer_fn(
  function()
    callback({type = "server", host = "127.0.0.1", port = port})
  end,
  100)
end
-- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
dap.configurations.go = {
  {
    type = "go",
    name = "Debug",
    request = "launch",
    program = "${file}"
  },
  {
    type = "go",
    name = "Debug test", -- configuration for debugging test files
    request = "launch",
    mode = "test",
    program = "${file}"
  },
  -- works with go.mod packages and sub packages 
  {
    type = "go",
    name = "Debug test (go.mod)",
    request = "launch",
    mode = "test",
    program = "./${relativeFileDirname}"
  } 
}
dap.adapters.lldb = {
  type = 'executable',
  command = '/usr/bin/lldb-vscode', -- adjust as needed
  name = "lldb"
}
-- TODO(Chris): Set up debugging config which allows you to attach via process name/id
dap.configurations.cpp = {
  {
    name = "Launch",
    type = "lldb",
    request = "launch",
    program = vim.fn.getcwd() .. '/target/debug/${workspaceFolderBasename}',
    -- program = function()
    --   return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    -- end,
    cwd = '${workspaceFolder}',
    stopOnEntry = false,
    args = {},

    -- if you change `runInTerminal` to true, you might need to change the yama/ptrace_scope setting:
    --
    --    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
    --
    -- Otherwise you might get the following error:
    --
    --    Error on launch: Failed to attach to the target process
    --
    -- But you should be aware of the implications:
    -- https://www.kernel.org/doc/html/latest/admin-guide/LSM/Yama.html
    runInTerminal = true,
  },
  {
    -- Debug adapter (lldb-vscode) specific items can be found in "Attaching Settings"
    -- https://marketplace.visualstudio.com/items?itemName=lanza.lldb-vscode
    -- TODO(Chris): Finish this
    name = "Attach",
    type = "lldb",
    request = "attach",
    program = vim.fn.getcwd() .. '/target/debug/${workspaceFolderBasename}',
    -- program = function()
    --   return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    -- end,
    cwd = '${workspaceFolder}',
    stopOnEntry = false,
    args = {},
    runInTerminal = false,
  },
}
dap.configurations.c = dap.configurations.cpp
dap.configurations.rust = dap.configurations.cpp

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

-- vim.o.foldmethod = 'expr'
-- vim.o.foldexpr = [[nvim_treesitter#foldexpr()]]

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
opt.shell = "/bin/bash"

--- Set keybindings
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- NOTE(Chris): We may need to move this to the beginning of the file
-- Equivalent to let mapleader = " "
map('n', '<SPACE>', '<Nop>')
vim.g.mapleader = ' '

map('n', '<C-s>', ':w<esc>')
map('i', '<C-s>', '<C-o>:w<esc>')

-- Make more convenient pane movement bindings
-- It's worth using :help nvim_set_keymap to check out options
-- like silent (which are just from vim's map commands anyways)
map('n', '<C-k>', '<C-w>k', {noremap = true, silent = true})
map('n', '<C-j>', '<C-w>j', {noremap = true, silent = true})
map('n', '<C-h>', '<C-w>h', {noremap = true, silent = true})
map('n', '<C-l>', '<C-w>l', {noremap = true, silent = true})

-- Rebind the "visual" mode keybindings to something more useful
map('n', 'Q', 'gwip')

-- Set up nvim-dap keybindings
cmd([[nnoremap <silent> <F5> :lua require'dap'.continue()<CR>]])
cmd([[nnoremap <silent> <F10> :lua require'dap'.step_over()<CR>]])
cmd([[nnoremap <silent> <F11> :lua require'dap'.step_into()<CR>]])
cmd([[nnoremap <silent> <F12> :lua require'dap'.step_out()<CR>]])
cmd([[nnoremap <silent> <leader>b :lua require'dap'.toggle_breakpoint()<CR>]])
cmd([[nnoremap <silent> <leader>B :lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>]])
cmd([[nnoremap <silent> <leader>lp :lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>]])
cmd([[nnoremap <silent> <leader>dr :lua require'dap'.repl.open()<CR>]])
cmd([[nnoremap <silent> <leader>dl :lua require'dap'.run_last()<CR>]])

-- Taken from https://github.com/nvim-telescope/telescope.nvim#usage
map('n', '<leader>ff', '<cmd>Telescope find_files<cr>')
map('n', '<leader>fg', '<cmd>Telescope live_grep<cr>')
map('n', '<leader>fb', '<cmd>Telescope buffers<cr>')
map('n', '<leader>fh', '<cmd>Telescope help_tags<cr>')
map('n', '<leader>fl', '<cmd>lua require("telescope.builtin").lsp_document_symbols()<cr>')
map('n', '<leader>fL', '<cmd>lua require("telescope.builtin").lsp_workspace_symbols()<cr>')
map('n', '<leader>fa', '<cmd>lua vim.lsp.buf.code_action()<cr>')

map('n', '<C-p>', '<cmd>Telescope find_files<cr>')

map('n', '<leader>ft', '<cmd>lua require("telescope.builtin").treesitter()<cr>')

map('n', '<leader>q', '<cmd>copen<CR>', {noremap = true, silent = true})
map('n', '<leader>n', '<cmd>cnext<CR>', {noremap = true, silent = true})
map('n', '<leader>p', '<cmd>cprev<CR>', {noremap = true, silent = true})

-- This is a workaround until cabbrev has a direct Lua API equivalent
cmd('cabbrev tn tabnew')
cmd('nnoremap <S-k> gt')
cmd('nnoremap <S-j> gT')
cmd('nnoremap <silent> gn :bn<CR>')
cmd('nnoremap <silent> gp :bp<CR>')

cmd('cabbrev ev e ~/.config/nvim/init.lua')

cmd('cabbrev soi source ~/.config/nvim/init.lua')

cmd('cabbrev twospace set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')
cmd('cabbrev fulltab set tabstop=8 softtabstop=0 noexpandtab shiftwidth=8 nosmarttab')

cmd('cabbrev Interesting vimgrep /^- .*interesting .*:/ %')

cmd('autocmd Filetype markdown set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')
cmd('autocmd Filetype markdown set textwidth=78 colorcolumn=+0')
-- cmd([[autocmd Filetype markdown setlocal spell]])
cmd([[autocmd Filetype markdown lua require('cmp').setup.buffer { enabled = false }]])

cmd('autocmd Filetype asciidoc set textwidth=80')

cmd('autocmd Filetype lua set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')

cmd('autocmd Filetype json set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')

cmd('autocmd Filetype haskell set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab')

cmd('autocmd Filetype go set tabstop=4 softtabstop=4 shiftwidth=4 smarttab')

cmd('autocmd TextYankPost * lua vim.highlight.on_yank {on_visual = false}')

cmd([[autocmd Filetype crontab set commentstring=#\ %s]])

cmd([[autocmd Filetype toml set commentstring=#\ %s]])
cmd([[autocmd Filetype toml set shiftwidth=2]])

cmd('autocmd Filetype cs set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab')

cmd('autocmd Filetype c set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab')
cmd('autocmd Filetype cpp set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab')

cmd([[autocmd Filetype cpp set commentstring=//\ %s]])
cmd([[autocmd Filetype c set commentstring=//\ %s]])

cmd([[autocmd Filetype sh set shiftwidth=2]])

cmd([[autocmd Filetype zsh set shiftwidth=2]])

cmd([[autocmd Filetype awk set shiftwidth=2]])

cmd([[autocmd Filetype sql set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab]])

cmd([[autocmd Filetype javascript set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab]])

-- Lambda calculus
cmd([[autocmd Bufread,BufNewFile *.lc set filetype=lc]])
cmd([[autocmd Filetype lc set commentstring=//\ %s]])
cmd([[autocmd Filetype lc set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab]])

-- cmd([[autocmd Filetype tex set textwidth=80 colorcolumn=+0]])
-- cmd([[autocmd Filetype tex set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab]])
--
cmd([[autocmd Filetype html set shiftwidth=4]])

-- Set the default empty buffer type to Markdown
cmd([[autocmd BufEnter {} set ft=markdown]])

cmd([[autocmd Filetype {} lua require('cmp').setup.buffer { enabled = false }]])

-- Configure vim-surround to use 'q' and 'Q' for LaTeX quotes
-- From https://vi.stackexchange.com/questions/5067/surround-plugin-and-tex-quotes
cmd([[autocmd FileType tex let b:surround_{char2nr("q")} = "`\r'"]])
cmd([[autocmd FileType tex let b:surround_{char2nr('Q')} = "``\r''"]])

-- cmd([[autocmd BufRead report.tex setlocal spell]])

cmd([[autocmd Filetype conf set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab]])

cmd([[autocmd Filetype ledger set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab]])

cmd([[autocmd Bufread,BufNewFile *.cf set filetype=lbnf]])
cmd([[autocmd Filetype lbnf set commentstring=--\ %s]])
cmd([[autocmd Filetype lbnf set comments+=b:--]])

cmd([[autocmd Filetype wast set tabstop=8 softtabstop=0 expandtab shiftwidth=1 smarttab]])
vim.opt.lispwords:append('module,import,export,func,call,block,loop,br_if')
-- cmd([[autocmd Filetype wast setlocal autoindent indentexpr=]])

cmd([[autocmd Bufread,BufNewFile *.chawk set filetype=awk]])
cmd([[autocmd Filetype pest set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab]])

cmd([[autocmd Filetype asciidoc set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab]])

--- Set colorscheme
if os.getenv('TERM') == 'foot' then
  cmd('colorscheme nord')
elseif vim.loop.os_uname().sysname == "Darwin" then
  vim.o.background = "light"
  cmd('colorscheme one-nvim')
else
  cmd('colorscheme gruvbox')
end

-- Neovide-specific configuration
-- https://github.com/neovide/neovide/issues/24
if vim.g.neovide then
  -- vim.g.neovide_transparency = 0.8
  if vim.loop.os_uname().sysname == "Linux" then
    vim.o.guifont = 'Iosevka Nerd Font:h12,Consolas:h12,Inconsolata:h12'
  else
    vim.o.guifont = 'Iosevka Term:h12,Consolas:h12,Inconsolata:h12'
  end
elseif (not vim.g.GuiLoaded or vim.g.GuiLoaded == 0) and vim.fn.has('macunix') == 0 then
  -- Make background transparent
  -- cmd([[highlight Normal guibg=NONE ctermbg=NONE]])
end

local function Ormolu()
  cmd('!ormolu --mode=inplace %')
end

cmd(':command! Ormolu lua Ormolu()')

-- This sets up abbreviated commands for idris2-nvim's interactive idris integration
-- https://github.com/ShinKage/idris2-nvim
cmd([[command! Es lua require('idris2.code_action').expr_search()]])
cmd([[command! Cs lua require('idris2.code_action').case_split()]])
cmd([[command! Gd lua require('idris2.code_action').generate_def()]])

-- Synctex function part of https://gist.github.com/vext01/16df5bd48019d451e078#gistcomment-2964552
cmd([[
function! Synctex()
    let vimura_param = " --synctex-forward " . line('.') . ":" . col('.') . ":" . expand('%:p') . " " . substitute(expand('%:p'),"tex$","pdf", "")
    call jobstart("vimura" . vimura_param)
    redraw!
endfunction
]])
-- Starting with Neovim 0.7, <CR> is different from <C-M>
cmd([[autocmd Filetype tex nnoremap <silent> <CR> :call Synctex()<cr>]])
cmd([[autocmd Filetype tex nnoremap <silent> <C-Space> :call Synctex()<cr>]])
-- cmd([[autocmd Filetype tex inoremap <silent> <C-M> <cmd>:call Synctex()<cr>]])

vim.g.vim_markdown_folding_disabled = 1

-- This can broadly replace the functionality of zM for Markdown files
function notes_headers(regex)
  regex = regex or '^# .*$'
  cmd([[vimgrep /]] .. regex .. [[/j %]])
  -- cmd([[vimgrep /^# .*$/j %]])
  -- The ivy theme is used to place the prompt at the top, reversing the
  -- result order
  vim.o.foldlevel = 0
  require('telescope.builtin').quickfix(require('telescope.themes').get_ivy())
end
-- cmd([[:command! NotesHeaders lua notes_headers()]])

if vim.g.vim_markdown_folding_disabled == 1 then
  -- Checking file name autocmd is from https://stackoverflow.com/questions/6009698/autocmd-check-filename-in-vim
  -- cmd([[autocmd BufRead,BufNewFile zoom_items.md nnoremap <silent> zM :lua notes_headers()<cr>]])
  cmd([[autocmd BufRead,BufNewFile zoom_items.md nnoremap zM :lua notes_headers()<cr>]])
  cmd([[autocmd BufRead,BufNewFile notes.md nnoremap zM :lua notes_headers('^#\\+ .*$')<cr>]])
end

