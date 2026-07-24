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
    opts = {
      signs = {
        add = { text = "▎" },
        change = { text = "▎" },
        delete = { text = "▎" },
        topdelete = { text = "▎" },
        changedelete = { text = "▎" },
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
      completion = { documentation = { auto_show = true } },
      sources = { default = { "lsp", "path", "snippets", "buffer" } },
      fuzzy = { implementation = "prefer_rust_with_warning" },
    },
  },
}
