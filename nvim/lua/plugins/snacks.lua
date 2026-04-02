return {
  {
    "folke/snacks.nvim",
    opts = function(_, opts)
      opts = opts or {}
      opts.picker = opts.picker or {}
      opts.picker.actions = opts.picker.actions or {}
      opts.picker.sources = opts.picker.sources or {}
      opts.picker.sources.files = opts.picker.sources.files or {}
      opts.picker.sources.files.win = opts.picker.sources.files.win or {}
      opts.picker.sources.files.win.input = opts.picker.sources.files.win.input or {}
      opts.picker.sources.files.win.input.keys = opts.picker.sources.files.win.input.keys or {}
      opts.picker.sources.files.win.list = opts.picker.sources.files.win.list or {}
      opts.picker.sources.files.win.list.keys = opts.picker.sources.files.win.list.keys or {}

      opts.picker.actions.cwd_up = function(picker)
        local cwd = picker:cwd()
        local parent = vim.fs.dirname(cwd)
        if parent and parent ~= "" and parent ~= cwd then
          picker:set_cwd(vim.fs.normalize(parent))
          picker:find()
        end
      end

      opts.picker.actions.cwd_home = function(picker)
        local home = vim.fs.normalize(vim.fn.expand("~"))
        if home and home ~= "" and picker:cwd() ~= home then
          picker:set_cwd(home)
          picker:find()
        end
      end

      -- Helm-like navigation in file picker (`<leader>ff`):
      -- Alt-j/k: down/up, Alt-l: confirm/open,
      -- Alt-h: parent directory, Alt-H: home directory.
      opts.picker.sources.files.win.input.keys["<a-j>"] = { "list_down", mode = { "i", "n" } }
      opts.picker.sources.files.win.input.keys["<a-k>"] = { "list_up", mode = { "i", "n" } }
      opts.picker.sources.files.win.input.keys["<a-l>"] = { "confirm", mode = { "i", "n" } }
      opts.picker.sources.files.win.input.keys["<c-h>"] = { "cwd_up", mode = { "i", "n" } }
      opts.picker.sources.files.win.input.keys["<c-l>"] = { "confirm", mode = { "i", "n" } }
      opts.picker.sources.files.win.input.keys["<a-h>"] = { "cwd_up", mode = { "i", "n" } }
      opts.picker.sources.files.win.input.keys["<a-H>"] = { "cwd_home", mode = { "i", "n" } }
      opts.picker.sources.files.win.list.keys["<a-j>"] = "list_down"
      opts.picker.sources.files.win.list.keys["<a-k>"] = "list_up"
      opts.picker.sources.files.win.list.keys["<a-l>"] = "confirm"
      opts.picker.sources.files.win.list.keys["<c-h>"] = "cwd_up"
      opts.picker.sources.files.win.list.keys["<c-l>"] = "confirm"
      opts.picker.sources.files.win.list.keys["<a-h>"] = "cwd_up"
      opts.picker.sources.files.win.list.keys["<a-H>"] = "cwd_home"
    end,
  },
}
