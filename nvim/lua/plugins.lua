return {
  {
    "folke/which-key.nvim",
    opts = {
      delay = 100,
    },
  },
  {
    "folke/snacks.nvim",
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
          Snacks.picker.files()
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
          Snacks.picker.buffers()
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
          Snacks.picker.grep()
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
            end_point = "http://<IP>:1234/v1/chat/completions",
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
}
-- 
