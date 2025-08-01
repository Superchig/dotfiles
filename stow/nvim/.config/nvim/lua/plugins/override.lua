return {
  -- The next three plugins modify the behavior of f and s
  { "ggandor/leap.nvim", enabled = false },
  { "ggandor/flit.nvim", enabled = false },
  { "folke/flash.nvim", enabled = false },
  -- I haven't taken the time to rebind this plugin to correspond to the
  -- vim-commentary mappings
  -- { "echasnovski/mini.surround", enabled = false },
  -- I've never gotten used to
  -- { "akinsho/bufferline.nvim", enabled = false },
  -- As of 2/4/2023, this doesn't work well with Neovide
  { "folke/noice.nvim", enabled = false },
  -- As of 2/4/2023, this doesn't support highlighting the author in TODO(author):
  -- https://github.com/folke/todo-comments.nvim/issues/10
  { "folke/todo-comments.nvim", enabled = false },
  -- { "RRethy/vim-illuminate", enabled = false },

  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      local keys = require("lazyvim.plugins.lsp.keymaps").get()

      keys[#keys + 1] = { "K", false }
      keys[#keys + 1] = { "J", false }

      opts.setup.tailwindcss = function(_, tailwindcss_opts)
        local default_filetypes = require("lspconfig").tailwindcss.config_def.default_config.filetypes
        tailwindcss_opts.filetypes = { "ruby" }
        vim.list_extend(tailwindcss_opts.filetypes, default_filetypes)

        tailwindcss_opts.settings = {
          tailwindCSS = {
            includeLanguages = {
              ruby = "html",
            },
            experimental = {
              classRegex = {
                [[class= "([^"]*)]],
                [[class: "([^"]*)]],
                [[class= '([^']*)]],
                [[class: '([^']*)]],
              },
            },
          },
        }
      end

      ---@diagnostic disable-next-line: undefined-field
      if vim.loop.os_uname().sysname == "Darwin" then
        opts.servers.clangd = {
          cmd = { "/opt/homebrew/opt/llvm@20/bin/clangd", "-header-insertion=never" },
        }
      else
        opts.servers.clangd = {
          cmd = { "clangd", "-header-insertion=never" },
        }
      end

      -- if require("mason-registry").is_installed("slang")
      opts.servers.slangd = {
        settings = {
          slang = {
            inlayHints = {
              deducedTypes = false,
              parameterNames = false,
            },
          },
        },
      }

      ---@type vim.lsp.ClientConfig
      opts.servers.zls = {
        cmd = { "zls" },
        mason = false,
        on_init = function(client)
          local function zls_auto_code_actions()
            -- Get all diagnostics for the current buffer
            local diagnostics_ = vim.diagnostic.get(0) or {}
            ---@type lsp.Diagnostic[]
            local diagnostics = vim.lsp.diagnostic.from(diagnostics_)

            local line, col = unpack(vim.api.nvim_win_get_cursor(0))

            ---@type lsp.CodeActionParams
            local params = {
              textDocument = vim.lsp.util.make_text_document_params(0),
              range = {
                start = { line = line, character = col },
                ["end"] = { line = line, character = col },
              },
              context = {
                diagnostics = diagnostics,
              },
            }

            ---@type table<integer, { error: (lsp.ResponseError)?, result: lsp.CodeAction[] }>?
            local clients_to_actions = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params)

            if clients_to_actions == nil then
              print("ZlsCodeActions could not obtain a list of code actions")
              return
            end

            local fix_all_available = false
            local organize_imports_available = false

            for client_id, actions in pairs(clients_to_actions) do
              if client_id ~= client.id then
                goto continue
              end

              if actions.error then
                print("Actions error: " .. vim.inspect(actions.error))
                goto continue
              end

              for _, action in ipairs(actions.result) do
                if action.kind == "source.fixAll" then
                  fix_all_available = true
                elseif action.kind == "source.organizeImports" then
                  organize_imports_available = true
                end

                if fix_all_available and organize_imports_available then
                  goto end_loops
                end
              end

              ::continue::
            end
            ::end_loops::

            if fix_all_available then
              vim.lsp.buf.code_action({
                context = {
                  diagnostics = diagnostics,
                  only = { "source.fixAll" },
                  triggerKind = 2, -- Automatic
                },
                apply = true,
              })
            end

            -- Reenable when this code action isn't always available
            -- if organize_imports_available then
            --   vim.lsp.buf.code_action({
            --     context = {
            --       diagnostics = diagnostics,
            --       only = { "source.organizeImports" },
            --       triggerKind = 2, -- Automatic
            --     },
            --     apply = true,
            --   })
            -- end
          end

          vim.api.nvim_create_autocmd("BufWritePre", {
            pattern = { "*.zig", "*.zon" },
            callback = function()
              zls_auto_code_actions()
            end,
          })
        end,
      }
    end,
  },

  {
    -- Compare to https://www.lazyvim.org/plugins/treesitter#nvim-treesitter
    "nvim-treesitter/nvim-treesitter",
    opts = {
      -- -- Disable highlighting for markdown, since we can just use Superchig/vim-markdown
      -- highlight = { disable = { "markdown" } },
      indent = { disable = { "ruby", "cpp", "d" } },
      -- ensure_installed = { "comment", "lua", "ruby", "python" },
    },
  },

  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "lua-language-server",
        "shellcheck",
        "shfmt",
      },
    },
  },

  {
    "snacks.nvim",
    opts = {
      scroll = { enabled = false },
      indent = {
        enabled = true,
        animate = {
          enabled = false,
        },
      },
    },
    config = function(_, opts)
      require("snacks").setup(opts)

      local orig_notify = vim.notify
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.notify = function(msg, level, notify_opts)
        if msg == "No information available" then
          return
        end

        orig_notify(msg, level, notify_opts)
      end
    end,
  },

  {
    "nvim-neo-tree/neo-tree.nvim",
    opts = {
      window = {
        mappings = {
          ["/"] = "none",
          ["?"] = "none",
          ["<leader>?"] = "show_help",
        },
      },
    },
  },
}
