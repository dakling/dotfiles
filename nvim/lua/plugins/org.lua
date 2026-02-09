return {
  -- Orgmode for Neovim: basic org support
  -- Covers: TODO keywords, basic capture, agenda, links
  -- Won't cover: roam, super-links, jupyter, pomodoro (keep Emacs for those)
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    opts = {
      -- Move orgmode prefix to <leader>n to avoid clashing with <leader>o (open)
      mappings = {
        prefix = "<Leader>n",
      },
      org_agenda_files = "~/org/**/*",
      org_default_notes_file = "~/org/notes.org",
      org_todo_keywords = { "TODO", "PROG", "|", "DONE", "BLOC", "KILL" },
      org_capture_templates = {
        t = {
          description = "Todo",
          template = "* TODO %?\n  %u\n  %a",
          target = "~/org/todo.org",
          headline = "Inbox",
        },
        n = {
          description = "Note",
          template = "* %u %?\n  %a",
          target = "~/org/notes.org",
          headline = "Inbox",
        },
        j = {
          description = "Journal",
          template = "* %U %?\n  %a",
          target = "~/org/journal.org",
          datetree = true,
        },
      },
      -- Matching your org settings
      org_id_link_to_org_use_id = true,
    },
    keys = {
      { "<leader>na", "<cmd>lua require('orgmode').action('agenda.prompt')<cr>", desc = "Org Agenda" },
      { "<leader>nc", "<cmd>lua require('orgmode').action('capture.prompt')<cr>", desc = "Org Capture" },
    },
  },

  -- Org bullets for nicer rendering
  {
    "akinsho/org-bullets.nvim",
    ft = "org",
    opts = {},
  },
}
