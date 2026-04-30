return {
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    config = function()
      require("orgmode").setup({
        org_agenda_files = "~/org/**/*",
        org_default_notes_file = "~/org/refile.org",
        org_log_into_drawer = "LOGBOOK",
        mappings = {
          org = {
            org_clock_in = "<localleader>xi",
            org_clock_out = "<localleader>xo",
            org_clock_cancel = "<localleader>xq",
            org_clock_goto = "<localleader>xj",
          },
        },
      })
    end,
  },
}
