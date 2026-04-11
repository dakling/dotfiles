return {
	"mrjones2014/smart-splits.nvim",
	build = "./kitty/install-kittens.bash",
	lazy = false,
	config = function()
		require("smart-splits").setup({
			multiplexer_integration = "tmux",
		})
	end,
	keys = {
		{
			"<C-A-h>",
			function()
				require("smart-splits").move_cursor_left()
			end,
			desc = "Move to left window",
		},
		{
			"<C-A-l>",
			function()
				require("smart-splits").move_cursor_right()
			end,
			desc = "Move to right window",
		},
		{
			"<C-A-j>",
			function()
				require("smart-splits").move_cursor_down()
			end,
			desc = "Move to below window",
		},
		{
			"<C-A-k>",
			function()
				require("smart-splits").move_cursor_up()
			end,
			desc = "Move to above window",
		},
		{
			"<A-H>",
			function()
				require("smart-splits").resize_left()
			end,
			desc = "Move to left window",
		},
		{
			"<A-L>",
			function()
				require("smart-splits").resize_right()
			end,
			desc = "Move to right window",
		},
		{
			"<A-J>",
			function()
				require("smart-splits").resize_down()
			end,
			desc = "Move to below window",
		},
		{
			"<A-K>",
			function()
				require("smart-splits").resize_up()
			end,
			desc = "Move to above window",
		},
	},
}
