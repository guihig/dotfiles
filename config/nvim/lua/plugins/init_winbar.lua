local web_devicons = require("nvim-web-devicons")
local fn = vim.fn

vim.api.nvim_set_hl(0, "WinFile", { fg = "#c678dd", bold = true })

local auto_cmd = {
    "DirChanged",
    "CursorMoved",
    "BufWinEnter",
    "BufFilePost",
    "InsertEnter",
    "BufWritePost"
}

local function set_winbar(winid)
    local file_name = fn.expand("%:p:t ")
    local file_path = fn.expand("%f")
    local file_type = fn.expand("%:e")
    local file_icon = web_devicons.get_icon(file_name, file_type,
                                            { default = true })
    file_icon = "%#DevIcon" .. file_type .. "#" .. file_icon .. " %*"

    local winbar = file_icon .. " " .. "%#WinFile#" .. file_path .. "%*"
    vim.wo[winid].winbar = winbar
end

vim.api.nvim_create_autocmd(auto_cmd, {
    callback = function()
        local disallowed_fts = {
            "TelescopePrompt",
            "rnvimr",
            "code-action-menu-warning-message",
            "code-action-menu-menu",
            "code-action-menu-diff",
            "code-action-menu-details",
            "none",
            " ",
            ""
        }

        if vim.tbl_contains(disallowed_fts, vim.bo.filetype) then
            vim.opt_local.winbar = nil
            return
        else
            local winid = vim.api.nvim_get_current_win()
            set_winbar(winid)
        end
    end
})
