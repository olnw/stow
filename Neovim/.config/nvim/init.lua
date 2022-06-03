vim.opt.termguicolors = true
vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.expandtab = true

vim.cmd "syntax enable"
vim.cmd "filetype plugin indent on"
vim.cmd "colorscheme modus-vivendi"

require"nvim-treesitter.configs".setup {
  ensure_installed = "all",

  highlight = {
    enable = true,
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false
  },

  rainbow = {
    enable = true,
    extended_mode = nil,
    max_file_lines = nil
  }
}

-- Bootstrap packer.nvim
local fn = vim.fn
local install_path = fn.stdpath("data").."/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({"git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path})
end

return require("packer").startup(function(use)
  -- Packer can manage itself
  use "wbthomason/packer.nvim"

  use "ishan9299/modus-theme-vim"

  use "tpope/vim-repeat"

  use "guns/vim-sexp"

  use "tpope/vim-sexp-mappings-for-regular-people"

  use "vlime/vlime"

  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate"
  }
  
  use "p00f/nvim-ts-rainbow"

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require("packer").sync()
  end
end)
