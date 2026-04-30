return {
	"mrjones2014/smart-splits.nvim",
	build = "./kitty/install-kittens.bash",
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
}
