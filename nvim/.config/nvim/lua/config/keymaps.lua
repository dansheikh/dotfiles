local map = vim.keymap.set

-- Window Navigation
map("n", "<C-h>", "<C-w>h", { desc = "Move to Left Split" })
map("n", "<C-j>", "<C-w>j", { desc = "Move to Below Split" })
map("n", "<C-k>", "<C-w>k", { desc = "Move to Above Split" })
map("n", "<C-l>", "<C-w>l", { desc = "Move to Right Split" })

-- Buffer Navigation & Split-Safe Removal
map("n", "<S-h>", "<cmd>bprevious<cr>", { desc = "Previous Buffer" })
map("n", "<S-l>", "<cmd>bnext<cr>", { desc = "Next Buffer" })
map("n", "[b", "<cmd>bprevious<cr>", { desc = "Previous Buffer" })
map("n", "]b", "<cmd>bnext<cr>", { desc = "Next Buffer" })

map("n", "<leader>bd", function()
  if package.loaded["mini.bufremove"] then
    require("mini.bufremove").delete(0, false)
  else
    vim.cmd("bdelete")
  end
end, { desc = "Delete Buffer (Preserve Window)" })

map("n", "<leader>bD", function()
  if package.loaded["mini.bufremove"] then
    require("mini.bufremove").delete(0, true)
  else
    vim.cmd("bdelete!")
  end
end, { desc = "Force Delete Buffer" })

-- Tab Navigation & Management
map("n", "[t", "<cmd>tabprevious<cr>", { desc = "Previous Tab" })
map("n", "]t", "<cmd>tabnext<cr>", { desc = "Next Tab" })
map("n", "<leader><tab><tab>", "<cmd>tabnew<cr>", { desc = "New Tab" })
map("n", "<leader><tab>d", "<cmd>tabclose<cr>", { desc = "Close Tab" })
map("n", "<leader><tab>o", "<cmd>tabonly<cr>", { desc = "Close Other Tabs" })
map("n", "<leader><tab>f", "<cmd>tabfirst<cr>", { desc = "First Tab" })
map("n", "<leader><tab>l", "<cmd>tablast<cr>", { desc = "Last Tab" })

-- Fuzzy Finding (mini.pick)
map("n", "<leader><space>", function()
  require("mini.pick").builtin.buffers()
end, { desc = "Find Buffers" })
map("n", "<leader>bb", function()
  require("mini.pick").builtin.buffers()
end, { desc = "Find Buffers" })
map("n", "<leader>ff", function()
  require("mini.pick").builtin.files()
end, { desc = "Find Files" })
map("n", "<leader>fg", function()
  require("mini.pick").builtin.grep_live()
end, { desc = "Live Grep" })
map("n", "<leader>fh", function()
  require("mini.pick").builtin.help()
end, { desc = "Find Help" })

-- Keep Cursor Centered
map("n", "<C-d>", "<C-d>zz", { desc = "Scroll Down Centered" })
map("n", "<C-u>", "<C-u>zz", { desc = "Scroll Up Centered" })
map("n", "n", "nzzzv", { desc = "Next Search Match" })
map("n", "N", "Nzzzv", { desc = "Prev Search Match" })

-- Indent retention in Visual Mode
map("v", "<", "<gv", { desc = "Indent Left" })
map("v", ">", ">gv", { desc = "Indent Right" })

-- Clear search highlights
map("n", "<Esc>", "<cmd>nohlsearch<cr>", { desc = "Clear Search Highlights" })

-- Code Formatting (Conform)
map({ "n", "v" }, "<leader>cf", function()
  require("conform").format({ async = true, lsp_format = "fallback" })
end, { desc = "Format Buffer (Conform)" })
