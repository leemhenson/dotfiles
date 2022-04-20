local map = vim.api.nvim_set_keymap
local default_map_opts = {noremap = true, silent = true}

-- Clear search highlights
map('n', '<C-/>', ':nohl<CR>', default_map_opts)

-- Nvim-tree
map('n', '<C-p>', ':NvimTreeToggle<CR>', default_map_opts)

-- Telescope
map('n', '<leader>fb', ':Telescope buffers<CR>', default_map_opts)
map('n', '<leader>fc', ':Telescope commands<CR>', default_map_opts)
map('n', '<leader>ff', ':Telescope  find_files<CR>', default_map_opts)
map('n', '<leader>ft', ':Telescope file_browser<CR>', default_map_opts)
map('n', '<leader>ts', ':Telescope ', default_map_opts)

-- Hop
map('n', '<leader>s', ':HopChar1<CR>', default_map_opts)
