return {
  -- claudecode.nvim: proper Claude Code IDE integration
  -- Connects via WebSocket (same protocol as VS Code extension)
  -- Opens native Neovim diff views for proposed changes
  {
    "coder/claudecode.nvim",
    dependencies = { "folke/snacks.nvim" },
    config = true,
    opts = {
      -- Terminal
      terminal = {
        split_side = "right",
        split_width_percentage = 0.40,
        provider = "snacks",
        auto_close = true,
        -- Dynamically resolve project root each time Claude is opened
        cwd_provider = function()
          -- Get directory of the current buffer
          local buf = vim.api.nvim_get_current_buf()
          local bufname = vim.api.nvim_buf_get_name(buf)
          local dir = bufname ~= "" and vim.fn.fnamemodify(bufname, ":p:h") or vim.fn.getcwd()

          -- Try git root
          local git_root = vim.fn.systemlist("git -C " .. vim.fn.shellescape(dir) .. " rev-parse --show-toplevel")[1]
          if vim.v.shell_error == 0 and git_root and git_root ~= "" then
            return git_root
          end

          -- No git repo — use the file's directory
          return dir
        end,
      },
      -- Diff: open native Neovim diff views
      diff_opts = {
        auto_close_on_accept = true,
        vertical_split = true,
        open_in_current_tab = true,
      },
      -- Selection tracking (sends cursor/selection context to Claude)
      track_selection = true,
    },
    keys = {
      -- SPC o c to toggle Claude (matching your Emacs claude-code-ide-menu binding)
      { "<leader>oc", "<cmd>ClaudeCode<cr>", desc = "Claude Code" },
      { "<leader>oC", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Claude" },

      -- SPC a prefix for Claude actions
      { "<leader>ac", "<cmd>ClaudeCode<cr>", desc = "Toggle Claude" },
      { "<leader>af", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Claude" },
      { "<leader>ar", "<cmd>ClaudeCode --resume<cr>", desc = "Resume session" },
      { "<leader>aR", "<cmd>ClaudeCode --continue<cr>", desc = "Continue session" },
      { "<leader>am", "<cmd>ClaudeCodeSelectModel<cr>", desc = "Select model" },
      { "<leader>ab", "<cmd>ClaudeCodeAdd %<cr>", desc = "Add current buffer" },
      { "<leader>as", "<cmd>ClaudeCodeSend<cr>", mode = "v", desc = "Send selection to Claude" },

      -- Diff accept/reject
      { "<leader>aa", "<cmd>ClaudeCodeDiffAccept<cr>", desc = "Accept diff" },
      { "<leader>ad", "<cmd>ClaudeCodeDiffDeny<cr>", desc = "Deny diff" },

      -- Compose prompt in a vim buffer, then send to Claude
      {
        "<leader>ap",
        function()
          -- Find the Claude terminal buffer and channel
          local function get_claude_term()
            for _, b in ipairs(vim.api.nvim_list_bufs()) do
              local name = vim.api.nvim_buf_get_name(b)
              if name:match("[Cc]laude") and vim.bo[b].buftype == "terminal" then
                local chan = vim.bo[b].channel
                if chan and chan > 0 then return b, chan end
              end
            end
            return nil, nil
          end

          -- Grab existing input from the Claude terminal
          -- Claude Code layout:
          --   ─────────── (border)
          --   ❯ input     (prompt line + continuation lines)
          --   ─────────── (border)
          --   ⏵⏵ status   (status bar)
          -- Detect border lines (Box Drawing U+2500-U+259F chars only)
          -- In UTF-8: E2 [94-96] [80-BF]
          local function is_border_line(line)
            local cleaned = line:gsub("%z", ""):gsub("%s", "")
            if #cleaned < 6 then return false end -- at least 2 box-drawing chars (3 bytes each)
            -- No ASCII alphanumeric or punctuation → line is all Unicode symbols
            return not cleaned:match("[%a%d%p]")
          end

          local function is_status_line(line)
            return line:match("⏵") ~= nil
              or line:match("Context left") ~= nil
              or line:match("auto%-compact") ~= nil
              or line:match("accept edits") ~= nil
              or line:match("shift%+tab") ~= nil
          end

          local function grab_terminal_input(term_buf, chan)
            if not term_buf then return "" end
            local lines = vim.api.nvim_buf_get_lines(term_buf, 0, -1, false)

            -- Find the last ❯ prompt line (search anywhere in line, not just start)
            local prompt_idx = nil
            for i = #lines, 1, -1 do
              if lines[i]:match("❯") then
                prompt_idx = i
                break
              end
            end
            if not prompt_idx then return "" end

            -- Collect from ❯ line forward, stopping at border or status
            local input_lines = {}
            for i = prompt_idx, #lines do
              if is_border_line(lines[i]) or is_status_line(lines[i]) then break end
              table.insert(input_lines, lines[i])
            end

            if #input_lines == 0 then return "" end

            -- Strip everything up to and including ❯ from the first line
            input_lines[1] = input_lines[1]:gsub(".*❯ ?", "")

            -- Strip null chars and trailing whitespace
            for j, line in ipairs(input_lines) do
              input_lines[j] = line:gsub("%z", ""):gsub("%s+$", "")
            end

            -- Drop empty trailing lines
            while #input_lines > 0 and vim.trim(input_lines[#input_lines]) == "" do
              table.remove(input_lines)
            end

            local result = vim.trim(table.concat(input_lines, "\n"))
            if result ~= "" then
              -- Ctrl-C cancels the entire multi-line input
              vim.fn.chansend(chan, "\x03")
              vim.wait(200)
            end
            return result
          end

          local term_buf, chan = get_claude_term()
          local existing = grab_terminal_input(term_buf, chan)

          -- Open a scratch buffer at the bottom
          vim.cmd("botright 10new")
          local buf = vim.api.nvim_get_current_buf()
          vim.bo[buf].buftype = "nofile"
          vim.bo[buf].bufhidden = "wipe"
          vim.bo[buf].swapfile = false
          vim.bo[buf].filetype = "markdown"
          vim.api.nvim_buf_set_name(buf, "claude-prompt")

          -- Pre-fill with existing input
          if existing ~= "" then
            local existing_lines = vim.split(existing, "\n")
            vim.api.nvim_buf_set_lines(buf, 0, -1, false, existing_lines)
            -- Cursor at end
            vim.api.nvim_win_set_cursor(0, { #existing_lines, #existing_lines[#existing_lines] })
          end

          -- Send contents to Claude terminal on <CR> in normal mode
          vim.keymap.set("n", "<CR>", function()
            local send_lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
            local text = table.concat(send_lines, "\n")
            if text:match("^%s*$") then return end

            local _, send_chan = get_claude_term()
            if not send_chan then
              vim.notify("Claude terminal not found. Open it with <Space>oc first.", vim.log.levels.WARN)
              return
            end

            vim.fn.chansend(send_chan, text .. "\n")
            vim.cmd("bwipeout!")
          end, { buffer = buf, desc = "Send to Claude" })

          -- q to cancel
          vim.keymap.set("n", "q", "<cmd>bwipeout!<cr>", { buffer = buf, desc = "Cancel" })

          vim.cmd("startinsert!")
          vim.notify("Compose prompt (Enter to send, q to cancel)", vim.log.levels.INFO)
        end,
        desc = "Compose prompt",
      },
    },
  },

  -- which-key group label
  {
    "folke/which-key.nvim",
    opts = {
      spec = {
        { "<leader>a", group = "ai/claude" },
      },
    },
  },
}
