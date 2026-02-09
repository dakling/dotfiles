return {
  -- vimtex: LaTeX support (replaces your AUCTeX + reftex + cdlatex + evil-tex setup)
  {
    "lervag/vimtex",
    lazy = false,
    init = function()
      -- Use pdf-tools equivalent: Skim on macOS, zathura on Linux
      if vim.fn.has("mac") == 1 then
        vim.g.vimtex_view_method = "skim"
      else
        vim.g.vimtex_view_method = "zathura"
      end
      vim.g.vimtex_compiler_method = "latexmk"
      vim.g.vimtex_compiler_latexmk = {
        options = {
          "-pdf",
          "-shell-escape",
          "-verbose",
          "-file-line-error",
          "-synctex=1",
          "-interaction=nonstopmode",
        },
      }
      -- Matching your reftex-cite-format style
      vim.g.vimtex_complete_close_braces = 1
    end,
    keys = {
      -- Localleader mappings matching your LaTeX config
      { "-a", "<cmd>VimtexCompile<cr>", ft = "tex", desc = "Compile (continuous)" },
      { "-b", "<cmd>VimtexCompileSS<cr>", ft = "tex", desc = "Compile (single shot)" },
      { "-v", "<cmd>VimtexView<cr>", ft = "tex", desc = "View PDF" },
      { "-e", "<cmd>VimtexErrors<cr>", ft = "tex", desc = "Errors" },
      { "-k", "<cmd>VimtexStop<cr>", ft = "tex", desc = "Stop compilation" },
      { "-t", "<cmd>VimtexTocToggle<cr>", ft = "tex", desc = "TOC" },
    },
  },

  -- Haskell: enhanced support (matching your haskell +lsp +tree-sitter)
  {
    "mrcjkb/haskell-tools.nvim",
    version = "^4",
    lazy = false,
    dependencies = { "nvim-lua/plenary.nvim" },
    ft = { "haskell", "lhaskell", "cabal", "cabalproject" },
  },

  -- Julia support
  -- Must NOT be lazy-loaded: it registers global BufEnter autocmds in ftdetect/
  {
    "JuliaEditorSupport/julia-vim",
    lazy = false,
    init = function()
      vim.g.latex_to_unicode_auto = 0
      vim.g.latex_to_unicode_tab = 0
      vim.g.latex_to_unicode_eager = 0
    end,
  },

  -- Lisp structural editing (matching your lispy/lispyville)
  {
    "julienvincent/nvim-paredit",
    ft = { "lisp", "scheme", "clojure", "fennel" },
    opts = {},
  },

  -- Conjure: Lisp REPL (partial replacement for sly)
  {
    "Olical/conjure",
    ft = { "lisp", "scheme", "clojure", "fennel", "python", "julia" },
    init = function()
      vim.g["conjure#mapping#prefix"] = "-"
      -- Disable conjure for filetypes where we don't want it
      vim.g["conjure#filetypes#override"] = { "lisp", "scheme", "clojure", "fennel" }
    end,
  },

  -- Python REPL keymaps (matching your python localleader config)
  {
    "benlubas/molten-nvim",
    version = "^1",
    ft = "python",
    build = ":UpdateRemotePlugins",
    init = function()
      vim.g.molten_output_win_max_height = 20
    end,
  },

  -- C/C++ additional config
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        -- Python (matching your pyright config)
        pyright = {},
        -- C/C++ (matching your cc +lsp)
        clangd = {},
        -- Shell (matching your sh +lsp)
        bashls = {},
        -- Haskell
        hls = {},
        -- YAML
        yamlls = {},
        -- JSON
        jsonls = {},
        -- LaTeX grammar (matching your lsp-ltex)
        ltex = {
          settings = {
            ltex = {
              language = "en-GB",
            },
          },
        },
      },
    },
  },

  -- Treesitter: ensure all your languages are installed
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "python", "haskell", "julia", "c", "cpp",
        "json", "yaml", "bash", "lua", "vim", "vimdoc",
        "markdown", "markdown_inline", "latex", "org",
        "html", "css", "commonlisp", "scheme",
      },
    },
  },
}
