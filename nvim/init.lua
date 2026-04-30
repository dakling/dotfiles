-- Fix luarocks.nvim vendor path (hardcoded.lua not generated during build)
local rocks_dir = vim.fn.stdpath("data") .. "/lazy/luarocks.nvim/.rocks"
package.path = package.path .. ";" .. rocks_dir .. "/share/lua/5.1/luarocks/vendor/?.lua"

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Leaders must be set before lazy.nvim loads plugins
vim.g.mapleader = " "
vim.g.maplocalleader = "-"

require("config.lazy")
