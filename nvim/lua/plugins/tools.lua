return {
  -- Neogit: magit replacement
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    cmd = "Neogit",
    opts = {
      integrations = {
        telescope = true,
        diffview = true,
      },
      -- Use vim keybindings in the neogit buffer
      mappings = {
        status = {
          ["q"] = "Close",
        },
      },
    },
    keys = {
      { "<leader>gg", "<cmd>Neogit<cr>", desc = "Neogit (Magit)" },
      { "<leader>gc", "<cmd>Neogit commit<cr>", desc = "Neogit commit" },
      { "<leader>gp", "<cmd>Neogit push<cr>", desc = "Neogit push" },
      { "<leader>gl", "<cmd>Neogit log<cr>", desc = "Neogit log" },
    },
  },

  -- ToggleTerm: terminal management (replaces vterm/eshell)
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    opts = {
      size = function(term)
        if term.direction == "horizontal" then return 15
        elseif term.direction == "vertical" then return vim.o.columns * 0.4
        end
      end,
      open_mapping = [[<C-\>]],
      direction = "horizontal",
      shade_terminals = true,
      float_opts = { border = "curved" },
    },
    keys = {
      { "<C-\\>", desc = "Toggle terminal" },
      { "<leader>ot", "<cmd>ToggleTerm<cr>", desc = "Terminal (horizontal)" },
      { "<leader>oT", "<cmd>ToggleTerm direction=float<cr>", desc = "Terminal (float)" },
      {
        "<leader>oG",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          local lazygit = Terminal:new({
            cmd = "lazygit",
            direction = "float",
            float_opts = { border = "curved" },
            close_on_exit = true,
          })
          lazygit:toggle()
        end,
        desc = "Lazygit",
      },
    },
  },

  -- DAP: Python debugger (matching your dap-mode + dap-python)
  {
    "mfussenegger/nvim-dap-python",
    ft = "python",
    dependencies = { "mfussenegger/nvim-dap" },
    config = function()
      require("dap-python").setup("python3")
    end,
    keys = {
      { "-dt", function() require("dap-python").test_method() end, ft = "python", desc = "Debug test method" },
      { "-dc", function() require("dap-python").test_class() end, ft = "python", desc = "Debug test class" },
    },
  },

  -- Editorconfig is built into Neovim 0.9+ (no plugin needed)

  -- Conform: formatting (matching your format module)
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        python = { "black" },
        lua = { "stylua" },
        sh = { "shfmt" },
        json = { "prettier" },
        yaml = { "prettier" },
        markdown = { "prettier" },
      },
    },
  },

  -- Which-key: extra labels matching your doom leader prefixes
  {
    "folke/which-key.nvim",
    opts = {
      spec = {
        { "<leader>o", group = "open" },
        { "<leader>l", group = "llm/lang" },
        { "<leader>m", group = "multicursor" },
        { "<leader>+", group = "popup" },
      },
    },
  },
}
