-- Keymaps inspired by your Doom Emacs config
-- LazyVim already sets <Space> as leader with which-key

local map = vim.keymap.set

-- Disable default `-` motion so it works as localleader
map("n", "-", "<Nop>")

-- ── Yanky cycling (override extra defaults [y / ]y) ─────────────────
map({ "n", "x" }, "<c-n>", "<Plug>(YankyNextEntry)", { desc = "Next yank history entry" })
map({ "n", "x" }, "<c-p>", "<Plug>(YankyPreviousEntry)", { desc = "Previous yank history entry" })

-- ── General ──────────────────────────────────────────────────────────
-- SPC SPC = command palette (like your "SPC SPC" → execute-extended-command)
map("n", "<leader><space>", "<cmd>Telescope commands<cr>", { desc = "Commands" })

-- -- smart-splits.nvim: seamless navigation/resizing across nvim windows + tmux panes
-- map("n", "<C-h>", function() require("smart-splits").move_cursor_left() end, { desc = "Move to left split" })
-- map("n", "<C-j>", function() require("smart-splits").move_cursor_down() end, { desc = "Move to below split" })
-- map("n", "<C-k>", function() require("smart-splits").move_cursor_up() end, { desc = "Move to above split" })
-- map("n", "<C-l>", function() require("smart-splits").move_cursor_right() end, { desc = "Move to right split" })
-- map("n", "<M-h>", function() require("smart-splits").resize_left() end, { desc = "Resize left" })
-- map("n", "<M-j>", function() require("smart-splits").resize_down() end, { desc = "Resize down" })
-- map("n", "<M-k>", function() require("smart-splits").resize_up() end, { desc = "Resize up" })
-- map("n", "<M-l>", function() require("smart-splits").resize_right() end, { desc = "Resize right" })
-- map("n", "<leader>wh", function() require("smart-splits").swap_buf_left() end, { desc = "Swap buffer left" })
-- map("n", "<leader>wj", function() require("smart-splits").swap_buf_down() end, { desc = "Swap buffer down" })
-- map("n", "<leader>wk", function() require("smart-splits").swap_buf_up() end, { desc = "Swap buffer up" })
-- map("n", "<leader>wl", function() require("smart-splits").swap_buf_right() end, { desc = "Swap buffer right" })

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
map("n", "<leader>oD", function()
	require("dap").continue()
end, { desc = "Debugger" })
map("n", "<leader>ot", "<cmd>ToggleTerm<cr>", { desc = "Terminal" })
map("n", "<leader>oT", "<cmd>ToggleTerm direction=float<cr>", { desc = "Terminal (float)" })
map("n", "<leader>og", "<cmd>Neogit<cr>", { desc = "Neogit (Magit)" })
map("n", "<leader>ol", "<cmd>Lazy<cr>", { desc = "Lazy plugin manager" })

map("n", "<leader>fs", "<cmd>w<cr>", { desc = "Save file" })

-- ── Search (SPC s) ───────────────────────────────────────────────────
map("n", "<leader>sp", "<cmd>Telescope live_grep<cr>", { desc = "Grep project" })
-- map("n", "<leader>ss", "<cmd>Telescope current_buffer_fuzzy_find<cr>", { desc = "Search buffer" })
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
-- Disable Flash's visual mode 's' mapping to allow mini.surround to work
vim.api.nvim_create_autocmd("User", {
	pattern = "VeryLazy",
	callback = function()
		vim.keymap.del("x", "s")
	end,
})

-- ── Obsidian vault (matching your SPC m prefix from Doom Emacs) ─────
-- SPC m m = meeting note
map("n", "<leader>mm", function()
	local title = vim.fn.input("Meeting title: ")
	if title == "" then
		return
	end
	local date = os.date("%Y-%m-%d")
	-- Create kebab-case slug
	local slug = title:lower():gsub("[^a-z0-9]+", "-"):gsub("%-$", "")
	local filename = string.format("meetings/%s-%s.md", date, slug)
	local vault = vim.fn.expand("~/obsidian-vault")
	local fullpath = vault .. "/" .. filename

	-- Create meetings/ directory if needed
	vim.fn.mkdir(vault .. "/meetings", "p")

	-- Create the file
	vim.cmd("edit " .. vim.fn.fnameescape(fullpath))
	if vim.api.nvim_buf_line_count(0) == 1 and vim.api.nvim_buf_get_lines(0, 0, -1, false)[1] == "" then
		local content = string.format(
			"---\ndate: %s\ntype: meeting\ntags: []\nattendees: []\n---\n\n# %s\n\n## Attendees\n-\n\n## Agenda\n-\n\n## Notes\n\n\n## Action Items\n- [ ]\n",
			date,
			title
		)
		vim.api.nvim_buf_set_lines(0, 0, -1, false, vim.split(content, "\n"))
		vim.cmd("write")
		-- Position cursor at first attendee slot
		vim.fn.search("## Attendees", "W")
		vim.cmd("normal! j0")
	end
end, { desc = "Meeting note" })

-- SPC m i = inbox capture (like obsidian-capture)
map("n", "<leader>mi", function()
	local vault = vim.fn.expand("~/obsidian-vault")
	local inbox_dir = vault .. "/inbox"
	vim.fn.mkdir(inbox_dir, "p")
	local filename = os.date("%Y-%m-%d-%H%M%S.md")
	local fullpath = inbox_dir .. "/" .. filename
	vim.cmd("edit " .. vim.fn.fnameescape(fullpath))
	if vim.api.nvim_buf_line_count(0) == 1 and vim.api.nvim_buf_get_lines(0, 0, -1, false)[1] == "" then
		vim.api.nvim_buf_set_lines(0, 0, -1, false, { "# " .. filename:gsub("%.md$", ""), "", "" })
		vim.cmd("write")
	end
	vim.cmd("startinsert")
end, { desc = "Inbox capture" })

-- SPC m j = jump to note (like obsidian-jump)
map("n", "<leader>mj", "<cmd>ObsidianQuickSwitch<cr>", { desc = "Jump to note" })

-- SPC m v = search vault (like obsidian-search)
map("n", "<leader>mv", "<cmd>ObsidianSearch<cr>", { desc = "Search vault" })

-- SPC m t = find by tag (like obsidian-find-tag)
map("n", "<leader>mt", "<cmd>ObsidianTags<cr>", { desc = "Find by tag" })

-- SPC m d = daily append (like your my/obsidian-daily-append)
map("n", "<leader>md", "<cmd>ObsidianToday<cr>", { desc = "Daily append" })

-- SPC m D = daily note (like obsidian-daily-note)
map("n", "<leader>mD", "<cmd>ObsidianToday<cr>", { desc = "Daily note" })

-- SPC m # = insert tag (like obsidian-insert-tag)
map("n", "<leader>m#", "<cmd>ObsidianTags<cr>", { desc = "Insert tag" })

-- SPC m b = backlinks (like obsidian-backlink-jump)
map("n", "<leader>mb", "<cmd>ObsidianBacklinks<cr>", { desc = "Backlinks" })

-- SPC m n = new note
map("n", "<leader>mn", "<cmd>ObsidianNew<cr>", { desc = "New note" })

-- SPC m l = follow link under cursor
map("n", "<leader>ml", "<cmd>ObsidianFollowLink<cr>", { desc = "Follow link" })

-- SPC m o = open in Obsidian app
map("n", "<leader>mo", "<cmd>ObsidianOpen<cr>", { desc = "Open in Obsidian" })

-- SPC m r = rename/note refactor
map("n", "<leader>mr", "<cmd>ObsidianRename<cr>", { desc = "Rename note" })

-- SPC m f = forward links
map("n", "<leader>mf", "<cmd>ObsidianLinks<cr>", { desc = "Forward links" })

-- SPC m g = toggle checkboxes
map("n", "<leader>mg", "<cmd>ObsidianToggleCheckbox<cr>", { desc = "Toggle checkbox" })

-- Which-key group label for Obsidian
vim.api.nvim_create_autocmd("User", {
	pattern = "VeryLazy",
	callback = function()
		local wk = require("which-key")
		wk.add({
			{ "<leader>m", group = "obsidian" },
		})
	end,
})
