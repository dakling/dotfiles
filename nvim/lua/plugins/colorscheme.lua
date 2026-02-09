return {
  -- Material palenight (closest to your doom-palenight)
  {
    "marko-cerovac/material.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      contrast = {
        sidebars = true,
        floating_windows = true,
      },
      plugins = {
        "gitsigns", "neo-tree", "telescope", "which-key",
        "nvim-cmp", "indent-blankline", "flash",
      },
    },
  },

  -- Set LazyVim colorscheme to material palenight
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = function()
        vim.g.material_style = "palenight"
        vim.cmd.colorscheme("material")
      end,
    },
  },
}
