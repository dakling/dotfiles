-- Keymaps inspired by your Doom Emacs config
-- LazyVim already sets <Space> as leader with which-key

local map = vim.keymap.set

-- ── General ──────────────────────────────────────────────────────────
-- SPC SPC = command palette (like your "SPC SPC" → execute-extended-command)
map("n", "<leader><space>", "<cmd>Telescope commands<cr>", { desc = "Commands" })

-- Window navigation: match your C-s-{h,j,k,l} bindings
map("n", "<C-h>", "<C-w>h", { desc = "Go to left window" })
map("n", "<C-j>", "<C-w>j", { desc = "Go to lower window" })
map("n", "<C-k>", "<C-w>k", { desc = "Go to upper window" })
map("n", "<C-l>", "<C-w>l", { desc = "Go to right window" })

-- Window resize: match your s-M-{h,j,k,l} resize bindings
map("n", "<M-H>", "<cmd>vertical resize -2<cr>", { desc = "Decrease width" })
map("n", "<M-L>", "<cmd>vertical resize +2<cr>", { desc = "Increase width" })
map("n", "<M-J>", "<cmd>resize +2<cr>", { desc = "Increase height" })
map("n", "<M-K>", "<cmd>resize -2<cr>", { desc = "Decrease height" })

-- SPC w w = switch to last buffer (your evil-switch-to-windows-last-buffer)
map("n", "<leader>ww", "<cmd>e #<cr>", { desc = "Switch to last buffer" })
map("n", "<leader>w<Tab>", "<cmd>e #<cr>", { desc = "Switch to last buffer" })

-- SPC b b = buffer list (like your helm-mini)
map("n", "<leader>bb", "<cmd>Telescope buffers<cr>", { desc = "Buffers" })

-- ── Bookmarks ────────────────────────────────────────────────────────
map("n", "<leader>lm", "<cmd>Telescope marks<cr>", { desc = "Marks/Bookmarks" })

-- ── Eval (for Lua, replacing your SPC e r for eval-expression) ──────
map("n", "<leader>er", ":lua =", { desc = "Eval Lua expression" })

-- ── Open ─────────────────────────────────────────────────────────────
-- SPC o d = file explorer (like your dired-jump)
map("n", "<leader>od", "<cmd>Oil<cr>", { desc = "Open directory (Oil)" })
map("n", "<leader>oD", function()
  require("dap").continue()
end, { desc = "Debugger" })
map("n", "<leader>ot", "<cmd>ToggleTerm<cr>", { desc = "Terminal" })
map("n", "<leader>oT", "<cmd>ToggleTerm direction=float<cr>", { desc = "Terminal (float)" })
map("n", "<leader>og", "<cmd>Neogit<cr>", { desc = "Neogit (Magit)" })
map("n", "<leader>ol", "<cmd>Lazy<cr>", { desc = "Lazy plugin manager" })

-- ── Search (SPC s) ───────────────────────────────────────────────────
map("n", "<leader>sp", "<cmd>Telescope live_grep<cr>", { desc = "Grep project" })
map("n", "<leader>ss", "<cmd>Telescope current_buffer_fuzzy_find<cr>", { desc = "Search buffer" })
map("n", "<leader>sr", "<cmd>Telescope resume<cr>", { desc = "Resume last search" })
map("n", "<leader>sy", "<cmd>Telescope registers<cr>", { desc = "Yank history / registers" })

-- ── Kill ring (matching your M-p → helm-show-kill-ring) ──────────────
map("n", "<M-p>", "<cmd>Telescope registers<cr>", { desc = "Registers (kill ring)" })
map("n", "<M-y>", "<cmd>Telescope registers<cr>", { desc = "Registers (kill ring)" })

-- ── Go back (matching your gb → pop-tag-mark) ───────────────────────
map("n", "gb", "<C-o>", { desc = "Go back" })

-- ── Popup toggle (matching your SPC + +) ─────────────────────────────
map("n", "<leader>++", "<cmd>ToggleTerm<cr>", { desc = "Toggle terminal" })

-- ── Format ───────────────────────────────────────────────────────────
map("n", "<leader>cf", function()
  LazyVim.format({ force = true })
end, { desc = "Format" })

-- ── String inflection (matching your SPC c ~ prefix) ─────────────────
-- Handled by vim-abolish: crs (snake), crm (mixed), crc (camel), cru (upper), cr- (kebab)
-- Also g~ operator cycles via dial.nvim

-- ── Quit ─────────────────────────────────────────────────────────────
map("n", "<leader>qq", "<cmd>qa<cr>", { desc = "Quit all" })

-- ── Better escape ────────────────────────────────────────────────────
map("i", "jk", "<Esc>", { desc = "Escape" })
map("i", "kj", "<Esc>", { desc = "Escape" })

-- ── Terminal mode escape ─────────────────────────────────────────────
map("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- ── Visual mode: s for surround (matching your "v s" → evil-surround-region) ──
-- mini.surround handles this with sa (add), sd (delete), sr (replace)
