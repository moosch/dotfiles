return {
  {
    "folke/which-key.nvim",
    opts = {
      delay = 100,
    },
  },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = {
      toggle = {},
      image = {},
      indent = {
        only_scope = true,
        only_current = true,
      },
      win = {},
      notifier = {},
      statuscolumn = {},
      picker = {
        sources = {
          files = {
            cmd = "rg",
          },
        },
      },
    },
    keys = {
      {
        "<leader>p",
        function()
          Snacks.picker.files(require("search_history").opts("files"))
        end,
        desc = "Find Files",
      },
      {
        "<leader>o",
        function()
          for _, client in ipairs(vim.lsp.get_clients({ bufnr = 0 })) do
            if client:supports_method("textDocument/documentSymbol") then
              return Snacks.picker.lsp_symbols()
            end
          end
          Snacks.picker.treesitter()
        end,
        desc = "Outline (Document Symbols)",
      },
      {
        "<leader>b",
        function()
          local git_status = {}
          local root = vim.fs.root(0, ".git")
          if root then
            local out = vim.fn.systemlist({ "git", "-C", root, "status", "--porcelain=v1", "-uall" })
            if vim.v.shell_error == 0 then
              for _, line in ipairs(out) do
                local xy, file = line:match("^(..) (.+)$")
                if xy and file then
                  git_status[root .. "/" .. file] = xy
                end
              end
            end
          end

          Snacks.picker.buffers({
            format = function(item, picker)
              item.status = git_status[vim.api.nvim_buf_get_name(item.buf)]
              local git_hl
              if item.status then
                -- also sets item.filename_hl to the git status color
                git_hl = Snacks.picker.format.file_git_status(item, picker)
              end
              if vim.bo[item.buf].modified then
                item.filename_hl = "SnacksPickerBufModified" -- unsaved wins over git color
              end
              local ret = Snacks.picker.format.buffer(item, picker)
              if git_hl then
                vim.list_extend(ret, git_hl)
              end
              return ret
            end,
          })
        end,
        desc = "Buffers",
      },
      {
        "<leader>gl",
        function()
          Snacks.git.blame_line()
        end,
        desc = "Git Blame Diff",
      },
      {
        "<leader>n",
        function()
          Snacks.notifier.show_history()
        end,
        desc = "Notification History",
      },
      {
        "<leader>s",
        function()
          Snacks.picker.grep(require("search_history").opts("grep", {
            confirm = function(picker, item)
              -- Send every filtered result to the quickfix list, open it at the
              -- bottom, and jump to the item that was selected. Focus lands in
              -- the main window on that location; <C-w>j hops to the list, where
              -- <CR> opens any other result in the same main window.
              local idx = picker.list.cursor
              local items = picker:items()
              picker:close()
              if not item or #items == 0 then
                return
              end
              local qf = {}
              for _, it in ipairs(items) do
                qf[#qf + 1] = {
                  filename = Snacks.picker.util.path(it),
                  bufnr = it.buf,
                  lnum = it.pos and it.pos[1] or 1,
                  col = it.pos and it.pos[2] + 1 or 1,
                  end_lnum = it.end_pos and it.end_pos[1] or nil,
                  end_col = it.end_pos and it.end_pos[2] + 1 or nil,
                  text = it.line or it.comment or it.text,
                  valid = true,
                }
              end
              idx = math.min(math.max(idx, 1), #qf)
              vim.fn.setqflist({}, " ", {
                title = "Grep: " .. (picker:filter().search or picker:filter().pattern or ""),
                items = qf,
                idx = idx,
              })
              vim.cmd("botright copen")
              -- `:cc` from the quickfix window opens the entry in the previous
              -- (main) window and moves the cursor there.
              vim.cmd("cc " .. idx)
            end,
          }))
        end,
        desc = "Grep (rg)",
      },
    },
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
    cmd = "Neotree",
    keys = {
      {
        "<leader>t",
        function()
          vim.cmd("Neotree toggle float")
        end,
        desc = "Toggle Neotree",
      },
    },
    opts = {
      close_if_last_window = false,
      filesystem = {
        hijack_netrw_behavior = "disabled",
        follow_current_file = { enabled = true },
        filtered_items = {
          hide_dotfiles = false,
          hide_gitignored = false,
        },
      },
      window = {
        position = "float",
        popup = {
          size = { height = "80%", width = "40%" },
          position = "50%",
        },
      },
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      signs = {
        -- add = { text = "▎" },
        -- change = { text = "▎" },
        -- delete = { text = "▎" },
        -- topdelete = { text = "▎" },
        -- changedelete = { text = "▎" },
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "-" },
        topdelete = { text = "-" },
        changedelete = { text = "-" },
      },
      current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d> - <summary>",
    },
    keys = {
      {
        "<leader>gb",
        function()
          require("gitsigns").toggle_current_line_blame()
        end,
        desc = "Toggle Inline Blame",
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter").install({
        "elixir", "heex", "eex",
        "elm",
        "go", "gomod",
        "c", "cpp",
        "typescript", "tsx", "javascript",
        "zig", "odin",
        "bash", "lua", "vim", "vimdoc",
        "html", "css", "json", "yaml",
        "markdown", "markdown_inline",
        "hcl", "terraform",
      })
    end,
  },
  {
    "saghen/blink.cmp",
    dependencies = { "saghen/blink.lib", "rafamadriz/friendly-snippets" },
    version = "1.*",
    opts = {
      keymap = { preset = "enter" },
      completion = {
        documentation = { auto_show = true },
        -- the "enter" preset expects explicit accept; auto_insert previews the
        -- item into the buffer and its undo range goes stale as you keep typing
        list = { selection = { preselect = true, auto_insert = false } },
      },
      sources = { default = { "lsp", "path", "snippets", "buffer" } },
      fuzzy = { implementation = "prefer_rust_with_warning" },
    },
  },
  {
    "saattrupdan/pi-agent.nvim",
    config = function()
      require("pi-agent").setup({
        command = "pi",      -- command to run in the floating terminal
        width   = 0.8,       -- fraction of editor width
        height  = 0.8,       -- fraction of editor height
        border  = "rounded", -- any value accepted by nvim_open_win
        pane_gap = 1,         -- empty cells between split panes (0 disables)
        keymap  = "<C-,>",   -- toggle keymap (string or table of strings; set to false or "" to disable)
        abort_keymap = "<C-c>", -- terminal-mode keymap that aborts the current Pi run (string or table; set to false or "" to disable)
      })
    end,
  },
  {
    "milanglacier/minuet-ai.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("minuet").setup({
        provider = "openai_compatible",
        provider_options = {
          openai_compatible = {
            end_point = "http://192.168.68.100:1234/v1/chat/completions",
            model = "qwen3-coder",
            optional = {
              max_tokens = 256,
            }
          }
        },
        auto_trigger_ft = {},
        keymap = {
          accept = "<M-CR>", -- accept whole completion.
          accept_line = "<M-Tab>",
          prev = "<C-space>", -- Cycle to previous completion item or manually invoke completion.
          dismiss = "<Esc>",
        },
      })
    end
  },
  {
    -- Inline struct field sizes/offsets and sizeof totals for C, from clangd.
    "moosch/structlens.nvim",
    ft = "c",
    cmd = { "StructLens", "StructLensToggle", "StructLensMode" },
    opts = {},
  },

  {
    "moosch/ami.nvim",
    dependencies = {
      "moosch/ami-chat.nvim",              -- chat advisor panel.
      "moosch/ami-history.nvim",           -- branching history tree (multi-turn + :AmiTree).
      "moosch/ami-inline.nvim",            -- inline (FIM) completion.
      "moosch/ami-persona.nvim",           -- Adds a persona to chat responses.
      -- "moosch/ami-proactive.nvim",      -- Periodic, proactive suggestions in chat.
    },
    config = function()
      require("ami").setup({
        -- point ami at your LM Studio if it isn't on localhost:
        model = { endpoint = "http://192.168.68.150:1234/v1", model = "qwen3-coder" },
        extensions = {
          require("ami-chat"),
          require("ami-history"),
          require("ami-inline"),
          require("ami-persona"),
        },
        inline = { endpoint = "http://192.168.68.150:1234/v1", model = "deepseek-coder" },
      })
    end,
  },
}
-- 
