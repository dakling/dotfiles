return {
	-- Obsidian vault integration (matching your Doom Emacs obsidian.el config)
	{
		"epwalsh/obsidian.nvim",
		version = "*", -- recommended, use latest release
		lazy = true,
		cmd = { "ObsidianToday", "ObsidianYesterday", "ObsidianTomorrow", "ObsidianNew", "ObsidianQuickSwitch", "ObsidianSearch", "ObsidianTags", "ObsidianBacklinks", "ObsidianFollowLink", "ObsidianRename", "ObsidianLinks", "ObsidianToggleCheckbox", "ObsidianOpen" },
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		opts = function()
			return {
				workspaces = {
					{
						name = "vault",
						path = "~/obsidian-vault",
					},
				},

				-- Completion settings (matching your corfu/cape tag completion)
				completion = {
					nvim_cmp = true,
					min_chars = 2,
				},

				-- Notes creation: put new notes in same directory as current buffer
				new_notes_location = "current_dir",

				-- Use wiki links (Obsidian default); can switch to "markdown" if preferred
				preferred_link_style = "wiki",

				-- Daily notes (matching your obsidian-daily-notes-directory)
				daily_notes = {
					folder = "_daily",
					date_format = "%Y-%m-%d",
					alias_format = "%B %-d, %Y",
					default_tags = { "daily-notes" },
				},

				-- Templates (matching your obsidian-templates-directory)
				templates = {
					folder = "_templates",
					date_format = "%Y-%m-%d",
					time_format = "%H:%M",
					substitutions = {},
				},

				-- Attachments settings
				attachments = {
					img_folder = "_attachments",
				},

				-- Picker config
				picker = {
					name = "telescope.nvim",
					note_mappings = {
						new = "<C-n>",
						insert_link = "<C-l>",
					},
					tag_mappings = {
						tag_note = "<C-x>",
						insert_tag = "<C-l>",
					},
				},

				-- Sort by latest modified
				sort_by = "modified",
				sort_reversed = true,

				-- UI enhancements (checkboxes, tags, etc.)
				ui = {
					enable = true,
					update_debounce = 200,
					checkboxes = {
						[" "] = { char = "☐", hl_group = "ObsidianTodo" },
						["x"] = { char = "✓", hl_group = "ObsidianDone" },
						[">"] = { char = ">", hl_group = "ObsidianRightArrow" },
						["~"] = { char = "~", hl_group = "ObsidianTilde" },
						["!"] = { char = "!", hl_group = "ObsidianImportant" },
					},
					bullets = { char = "•", hl_group = "ObsidianBullet" },
					tags = { hl_group = "ObsidianTag" },
					hl_groups = {
						ObsidianTodo = { bold = true, fg = "#f78c6c" },
						ObsidianDone = { bold = true, fg = "#89ddff" },
						ObsidianRightArrow = { bold = true, fg = "#f78c6c" },
						ObsidianTilde = { bold = true, fg = "#ff5370" },
						ObsidianImportant = { bold = true, fg = "#d73128" },
						ObsidianBullet = { bold = true, fg = "#89ddff" },
						ObsidianTag = { italic = true, fg = "#89ddff" },
					},
				},

				-- Disable default keymaps — we define our own in keymaps.lua
				mappings = {},
			}
		end,
	},
}
