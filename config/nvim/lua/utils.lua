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

_M.reload_nvim = function()
    vim.notify("Reloading", vim.log.levels.INFO)
    vim.api.nvim_exec([[source "~/.config/nvim/init.lua"]], true)
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

_M.t = function(str) return
    vim.api.nvim_replace_termcodes(str, true, true, true) end

return _M
