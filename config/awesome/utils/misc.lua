local awful = require("awful")
local naughty = require("naughty")

local M = { placement = {} }

-- Layout
M.handle_master_count = function()
	local t = awful.screen.focused().selected_tag
	if t.layout.name == "mstab" then
		t.master_count = 0
	else
		t.master_count = 1
	end
end

-- Log
M.log = function(title, message)
	naughty.notification({
		urgency = "info",
		title = tostring(title),
		message = tostring(message),
	})
end

--
M.string_in_table = function(tbl, str)
	for i = 1, #tbl do
		if tbl[i] == str then
			return true
		end
	end
	return false
end

return M
