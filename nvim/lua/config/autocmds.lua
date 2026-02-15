-- Autocommands

local autocmd = vim.api.nvim_create_autocmd

-- Highlight on yank (like ophints in your doom config)
autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({ timeout = 200 })
  end,
})

-- Auto-resize splits when terminal is resized
autocmd("VimResized", {
  callback = function()
    vim.cmd("tabdo wincmd =")
  end,
})

-- Return to last edit position
autocmd("BufReadPost", {
  callback = function(event)
    local mark = vim.api.nvim_buf_get_mark(event.buf, '"')
    local lcount = vim.api.nvim_buf_line_count(event.buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Close some filetypes with q (matching doom popup behavior)
autocmd("FileType", {
  pattern = {
    "help", "man", "notify", "qf", "query", "spectre_panel",
    "startuptime", "tsplayground", "checkhealth", "neotest-output",
    "neotest-summary", "neotest-output-panel",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
  end,
})

-- Auto-enable spell in certain filetypes (matching your +everywhere spell config)
autocmd("FileType", {
  pattern = { "markdown", "org", "tex", "text", "gitcommit" },
  callback = function()
    vim.opt_local.spell = true
  end,
})

-- .m files → octave (matching your auto-mode-alist)
autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "*.m",
  callback = function()
    vim.bo.filetype = "octave"
  end,
})
