local fs = vim.fs
local fn = vim.fn

local _M = {}

_M.get_curr_char = function()
	local byte_index = vim.api.nvim_win_get_cursor(0)[2]
	return vim.api.nvim_get_current_line():sub(byte_index, byte_index)
end

_M.is_curr_char_space = function()
	return string.find(_M.get_curr_char(), "%s") ~= nil
end

_M.is_buffer_not_empty = function()
	return vim.fn.empty(vim.fn.expand("%:t")) == 1
end

_M.get_lua_modules = function(nvim_lua_dir, prefix)
	local lua_modules = {}
	for path, path_type in fs.dir(nvim_lua_dir) do
		if path_type == "file" and path:match("%.lua") then
			local module = path:gsub("%.lua", "")

			if prefix then
				module = prefix .. "." .. module
			end

			table.insert(lua_modules, module)
		elseif path_type == "directory" then
			local next_path = nvim_lua_dir .. "/" .. path
			local next_prefix = path
			if prefix then
				next_prefix = prefix .. "." .. next_prefix
			end

			local mods = _M.get_lua_modules(next_path, next_prefix)

			for _, v in ipairs(mods) do
				table.insert(lua_modules, v)
			end
		end
	end

	return lua_modules
end

_M.reload_lua = function()
	local HOME = fn.expand("$HOME")
	local nvim_lua_dir = HOME .. "/.config/nvim/lua"

	local modules = _M.get_lua_modules(nvim_lua_dir)

	for _, mod in pairs(modules) do
		package.loaded[mod] = nil
	end

	dofile(vim.env.MYVIMRC)
end

_M.reload_nvim = function()
	vim.cmd([[source "~/.config/nvim/init.lua"]], true)
	_M.reload_lua()
	vim.cmd("e")

	vim.notify("Reloadded Neovim", vim.log.levels.INFO)
end

_M.merge_table = function(t1, t2)
	for k, v in pairs(t2) do
		if type(v) == "table" then
			if type(t1[k] or false) == "table" then
				_M.merge_table(t1[k] or {}, t2[k] or {})
			else
				t1[k] = v
			end
		else
			t1[k] = v
		end
	end
	return t1
end

_M.t = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

_M.table_merge = function(t1, t2)
	for k, v in pairs(t2) do
		if type(v) == "table" then
			if type(t1[k] or false) == "table" then
				_M.table_merge(t1[k] or {}, t2[k] or {})
			else
				t1[k] = v
			end
		else
			t1[k] = v
		end
	end
	return t1
end

return _M
