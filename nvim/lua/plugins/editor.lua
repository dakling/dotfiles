return {
	-- Flash: replaces avy (your s/S → avy-goto-char-timer)
	{
		"folke/flash.nvim",
		opts = {
			modes = {
				char = { enabled = true },
				search = { enabled = false }, -- don't hijack /
			},
		},
		keys = {
			{ "s", mode = { "x" }, false }, -- Disable default flash mappings
			{ "S", mode = { "x" }, false },
			{
				"gs",
				mode = { "n", "x", "o" },
				function()
					require("flash").jump()
				end,
				desc = "Flash",
			},
			{
				"gS",
				mode = { "n", "x", "o" },
				function()
					require("flash").treesitter()
				end,
				desc = "Flash Treesitter",
			},
		},
	},

	{
		"nvim-mini/mini.surround",
		opts = {
			mappings = {
				add = "ys", -- Add surrounding in Normal mode
				change = "cs", -- Change surrounding
				delete = "ds", -- Delete surrounding
				-- Note: We'll handle visual mode mappings separately below
			},
		},
		-- Set up visual mode mappings for surround
		config = function(_, opts)
			require("mini.surround").setup(opts)

			-- Visual mode: s for adding surrounding (matching evil-surround-region)
			vim.keymap.set("x", "s", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { silent = true })

			-- Visual mode: S for adding surrounding on new lines
			vim.keymap.set("x", "S", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { silent = true })
		end,
	},

	-- vim-abolish: string inflection (matching your string-inflection setup)
	-- crs = snake_case, crm = MixedCase, crc = camelCase, cru = UPPER_CASE, cr- = kebab-case
	{ "tpope/vim-abolish" },

	-- Beacon: cursor flash on jump (matching your beacon config)
	{
		"rainbowhxch/beacon.nvim",
		event = "VeryLazy",
		opts = {
			enable = true,
			size = 40,
			fade = true,
			minimal_jump = 10,
		},
	},

	-- Telescope: configure to match your Helm habits
	{
		"nvim-telescope/telescope.nvim",
		opts = {
			defaults = {
				mappings = {
					i = {
						-- Match your helm M-j/k for navigation
						["<M-j>"] = "move_selection_next",
						["<M-k>"] = "move_selection_previous",
						["<M-l>"] = "select_default",
						["<M-h>"] = function(...)
							return require("telescope.actions").close(...)
						end,
						["<C-j>"] = "move_selection_next",
						["<C-k>"] = "move_selection_previous",
					},
					n = {
						["<M-j>"] = "move_selection_next",
						["<M-k>"] = "move_selection_previous",
						["q"] = "close",
					},
				},
			},
		},
	},

	-- Visual-multi: multiple cursors (matching your multiple-cursors module)
	{
		"mg979/vim-visual-multi",
		event = "VeryLazy",
		init = function()
			vim.g.VM_maps = {
				["Find Under"] = "<C-n>",
				["Find Subword Under"] = "<C-n>",
			}
		end,
	},

	-- Window picker (matching your window-select +numbers)
	{
		"s1n7ax/nvim-window-picker",
		event = "VeryLazy",
		opts = {
			hint = "floating-big-letter",
		},
		keys = {
			{
				"<leader>wp",
				function()
					local win = require("window-picker").pick_window()
					if win then
						vim.api.nvim_set_current_win(win)
					end
				end,
				desc = "Pick window",
			},
		},
	},

	-- Blink.cmp: Tab/S-Tab to cycle completions
	{
		"saghen/blink.cmp",
		opts = {
			keymap = {
				["<Tab>"] = { "select_and_accept", "select_next", "fallback" },
				["<CR>"] = {},
			},
		},
	},

	-- Diffview (enhanced diff, partial magit replacement for ediff)
	{
		"sindrets/diffview.nvim",
		cmd = { "DiffviewOpen", "DiffviewFileHistory" },
		keys = {
			{ "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diffview" },
			{ "<leader>gh", "<cmd>DiffviewFileHistory %<cr>", desc = "File history" },
			{ "<leader>gH", "<cmd>DiffviewFileHistory<cr>", desc = "Branch history" },
		},
	},
}
