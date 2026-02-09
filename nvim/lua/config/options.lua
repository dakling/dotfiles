-- Options matching your Doom Emacs preferences

local opt = vim.opt

-- Line numbers: you use absolute in doom
opt.number = true
opt.relativenumber = false

-- Leader keys: SPC as leader (LazyVim default), - as localleader (matching your doom config)
vim.g.mapleader = " "
vim.g.maplocalleader = "-"

-- Search: match your isearch config
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

-- Substitution: global by default (your evil-ex-substitute-global)
opt.gdefault = true

-- UI
opt.termguicolors = true
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.cursorline = true
opt.signcolumn = "yes"
opt.wrap = true
opt.linebreak = true
opt.showmode = false

-- Indentation
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.smartindent = true

-- Splits: open below and right
opt.splitbelow = true
opt.splitright = true

-- Undo: persistent undo (matching your doom undo +tree)
opt.undofile = true
opt.undolevels = 10000

-- Clipboard: sync with system
opt.clipboard = "unnamedplus"

-- Completion
opt.completeopt = "menu,menuone,noselect"

-- File handling
opt.swapfile = false
opt.backup = false

-- 24hr time in statusline
opt.timeoutlen = 300

-- Spell (matching your aspell + en_GB)
opt.spell = false -- toggle with <leader>us
opt.spelllang = { "en_gb" }

-- Fold
opt.foldmethod = "expr"
opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
opt.foldlevel = 99

-- Disable mouse (you had disable-mouse in packages)
-- Uncomment if you want:
-- opt.mouse = ""

-- Compilation scroll (matching your compilation-scroll-output)
opt.scrollback = 10000

-- Fill chars
opt.fillchars = { eob = "~" } -- matching vi-tilde-fringe

-- ── Neovide GUI settings ─────────────────────────────────────────────
if vim.g.neovide then
  -- Font (adjust family/size to taste)
  vim.o.guifont = "JetBrainsMono Nerd Font:h14"

  -- Cursor animation
  vim.g.neovide_cursor_animation_length = 0.08
  vim.g.neovide_cursor_trail_size = 0.4
  vim.g.neovide_cursor_vfx_mode = "railgun" -- options: railgun, torpedo, pixiedust, sonicboom, ripple, wireframe

  -- Smooth scrolling
  vim.g.neovide_scroll_animation_length = 0.2
  vim.g.neovide_scroll_animation_far_lines = 1

  -- Window
  vim.g.neovide_remember_window_size = true
  vim.g.neovide_fullscreen = false
  vim.g.neovide_padding_top = 4
  vim.g.neovide_padding_left = 8
  vim.g.neovide_padding_right = 8
  vim.g.neovide_padding_bottom = 4

  -- Transparency (1.0 = opaque)
  vim.g.neovide_transparency = 0.95

  -- macOS: use Option as Meta (matching your ns-alternate-modifier = meta)
  vim.g.neovide_input_macos_option_key_is_meta = "only_left"

  -- macOS: allow Cmd+C/V for clipboard
  vim.keymap.set({ "n", "v" }, "<D-c>", '"+y', { desc = "Copy" })
  vim.keymap.set({ "n", "v", "i" }, "<D-v>", function()
    vim.api.nvim_paste(vim.fn.getreg("+"), true, -1)
  end, { desc = "Paste" })

  -- Cmd+S to save (common macOS habit)
  vim.keymap.set({ "n", "i", "v" }, "<D-s>", "<cmd>w<cr><Esc>", { desc = "Save" })

  -- Cmd+= / Cmd+- to change font size
  vim.g.neovide_scale_factor = 1.0
  local change_scale = function(delta)
    vim.g.neovide_scale_factor = vim.g.neovide_scale_factor * delta
  end
  vim.keymap.set("n", "<D-=>", function() change_scale(1.1) end, { desc = "Zoom in" })
  vim.keymap.set("n", "<D-->", function() change_scale(1 / 1.1) end, { desc = "Zoom out" })
  vim.keymap.set("n", "<D-0>", function() vim.g.neovide_scale_factor = 1.0 end, { desc = "Reset zoom" })
end
