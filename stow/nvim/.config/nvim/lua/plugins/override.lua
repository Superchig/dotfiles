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
      opts.servers["*"].keys = {
        {
          "<leader>cl",
          function()
            Snacks.picker.lsp_config()
          end,
          desc = "Lsp Info",
        },
        { "gd", vim.lsp.buf.definition, desc = "Goto Definition", has = "definition" },
        { "gr", vim.lsp.buf.references, desc = "References", nowait = true },
        { "gI", vim.lsp.buf.implementation, desc = "Goto Implementation" },
        { "gy", vim.lsp.buf.type_definition, desc = "Goto T[y]pe Definition" },
        { "gD", vim.lsp.buf.declaration, desc = "Goto Declaration" },
        {
          "K",
          "K",
          desc = "Use the default K command",
        },
        -- {
        --   "K",
        --   function()
        --     if vim.o.keywordprg == ":Man" then
        --       local smods = {}
        --       local width = vim.api.nvim_win_get_width(0)
        --       if width > 160 then
        --         smods.vertical = true
        --       end
        --
        --       require("man").open_page(0, smods, {})
        --     else
        --       vim.api.nvim_feedkeys("K", "n", true)
        --     end
        --   end,
        --   desc = "Open :Man page if applicable",
        -- },
        -- {
        --   "K",
        --   function()
        --     return vim.lsp.buf.hover()
        --   end,
        --   desc = "Hover",
        -- },
        {
          "gK",
          function()
            return vim.lsp.buf.signature_help()
          end,
          desc = "Signature Help",
          has = "signatureHelp",
        },
        {
          "<c-k>",
          function()
            return vim.lsp.buf.signature_help()
          end,
          mode = "i",
          desc = "Signature Help",
          has = "signatureHelp",
        },
        { "<leader>ca", vim.lsp.buf.code_action, desc = "Code Action", mode = { "n", "x" }, has = "codeAction" },
        { "<leader>cc", vim.lsp.codelens.run, desc = "Run Codelens", mode = { "n", "x" }, has = "codeLens" },
        {
          "<leader>cC",
          vim.lsp.codelens.refresh,
          desc = "Refresh & Display Codelens",
          mode = { "n" },
          has = "codeLens",
        },
        {
          "<leader>cR",
          function()
            Snacks.rename.rename_file()
          end,
          desc = "Rename File",
          mode = { "n" },
          has = { "workspace/didRenameFiles", "workspace/willRenameFiles" },
        },
        { "<leader>cr", vim.lsp.buf.rename, desc = "Rename", has = "rename" },
        { "<leader>cA", LazyVim.lsp.action.source, desc = "Source Action", has = "codeAction" },
        {
          "]]",
          function()
            Snacks.words.jump(vim.v.count1)
          end,
          has = "documentHighlight",
          desc = "Next Reference",
          enabled = function()
            return Snacks.words.is_enabled()
          end,
        },
        {
          "[[",
          function()
            Snacks.words.jump(-vim.v.count1)
          end,
          has = "documentHighlight",
          desc = "Prev Reference",
          enabled = function()
            return Snacks.words.is_enabled()
          end,
        },
        {
          "<a-n>",
          function()
            Snacks.words.jump(vim.v.count1, true)
          end,
          has = "documentHighlight",
          desc = "Next Reference",
          enabled = function()
            return Snacks.words.is_enabled()
          end,
        },
        {
          "<a-p>",
          function()
            Snacks.words.jump(-vim.v.count1, true)
          end,
          has = "documentHighlight",
          desc = "Prev Reference",
          enabled = function()
            return Snacks.words.is_enabled()
          end,
        },
      }

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
      opts.servers.omnisharp = {
        cmd = { "OmniSharp" },
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
      indent = { disable = { "ruby", "cpp", "d", "typst" } },
      -- ensure_installed = { "comment", "lua", "ruby", "python" },
    },
  },

  {
    "mason-org/mason.nvim",
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
      notifier = {
        enabled = false,
      },
    },
    config = function(_, opts)
      require("snacks").setup(opts)

      -- local orig_notify = vim.notify
      -- ---@diagnostic disable-next-line: duplicate-set-field
      -- vim.notify = function(msg, level, notify_opts)
      --   if msg == "No information available" then
      --     return
      --   end
      --
      --   orig_notify(msg, level, notify_opts)
      -- end
    end,
  },

  {
    "Saghen/blink.cmp",
    opts = {
      --- From https://www.reddit.com/r/neovim/comments/1hz43t8/help_disabling_blink_for_certain_file_types_in/
      enabled = function()
        return not vim.tbl_contains({ "norg", "org", "markdown" }, vim.bo.filetype) and vim.bo.buftype ~= "prompt"
      end,
    },
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

  {
    "stevearc/conform.nvim",
    opts = function()
      local opts = {
        default_format_opts = {
          timeout_ms = 3000,
          async = false, -- not recommended to change
          quiet = false, -- not recommended to change
          lsp_format = "fallback", -- not recommended to change
        },
        formatters_by_ft = {
          lua = { "stylua" },
          fish = { "fish_indent" },
          sh = { "shfmt" },
          cs = { "csharpier-client" },
        },
        formatters = {
          injected = { options = { ignore_errors = true } },
          ["csharpier-client"] = {
            command = "csharpier-client",
            args = { "--quiet", "$FILENAME" },
          },
          -- # Example of using dprint only when a dprint.json file is present
          -- dprint = {
          --   condition = function(ctx)
          --     return vim.fs.find({ "dprint.json" }, { path = ctx.filename, upward = true })[1]
          --   end,
          -- },
          --
          -- # Example of using shfmt with extra args
          -- shfmt = {
          --   prepend_args = { "-i", "2", "-ci" },
          -- },
        },
      }
      return opts
    end,
  },
}
