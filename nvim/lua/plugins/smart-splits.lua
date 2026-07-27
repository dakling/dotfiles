return {
	{
		"mrjones2014/smart-splits.nvim",
		build = "./kitty/install-kittens.bash",
		-- Keep the existing tmux integration for tmux sessions.  Herdr gets its
		-- own bridge below because it has a different pane-control API.
		cond = function()
			return vim.env.HERDR_ENV ~= "1"
		end,
		lazy = false,
		config = function()
			require("smart-splits").setup({
				multiplexer_integration = "tmux",
			})

			local ss = require("smart-splits")
			vim.keymap.set("n", "<C-A-h>", ss.move_cursor_left, { desc = "Move to left window" })
			vim.keymap.set("n", "<C-A-l>", ss.move_cursor_right, { desc = "Move to right window" })
			vim.keymap.set("n", "<C-A-j>", ss.move_cursor_down, { desc = "Move to below window" })
			vim.keymap.set("n", "<C-A-k>", ss.move_cursor_up, { desc = "Move to above window" })
			vim.keymap.set("n", "<A-H>", ss.resize_left, { desc = "Resize left" })
			vim.keymap.set("n", "<A-L>", ss.resize_right, { desc = "Resize right" })
			vim.keymap.set("n", "<A-J>", ss.resize_down, { desc = "Resize down" })
			vim.keymap.set("n", "<A-K>", ss.resize_up, { desc = "Resize up" })
		end,
	},
	{
		"lmilojevicc/herdr-splits.nvim",
		cond = function()
			return vim.env.HERDR_ENV == "1"
		end,
		event = "VeryLazy",
		config = function()
			require("herdr-splits").setup({
				auto_sync_herdr = true,
				nav_at_edge = "stop",
				nav_keys = {
					left = "<C-A-h>",
					down = "<C-A-j>",
					up = "<C-A-k>",
					right = "<C-A-l>",
				},
				resize_keys = {
					left = "<A-H>",
					down = "<A-J>",
					up = "<A-K>",
					right = "<A-L>",
				},
			})
		end,
	},
}
