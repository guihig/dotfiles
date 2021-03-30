local gl = require("galaxyline")
local gls = gl.section
gl.short_line_list = {"LuaTree", "vista", "dbui"}

local condition = require('galaxyline.condition')

vim.api.nvim_exec([[
  let configuration = sonokai#get_configuration()
  let g:palette = sonokai#get_palette(configuration.style)
]], true)

local utils = {}

function utils.has_width_gt(cols)
    -- Check if the windows width is greater than a given number of columns
    return vim.fn.winwidth(0) / 2 > cols
end

local colors = {
    bg = vim.g.palette.bg0[1],
    fg = vim.g.palette.fg[1],
    blue = vim.g.palette.blue[1],
    green = vim.g.palette.green[1],
    purple = vim.g.palette.purple[1],
    orange = vim.g.palette.orange[1],
    red = vim.g.palette.red[1],
    diff_red = vim.g.palette.diff_red[1],
    yellow = vim.g.palette.yellow[1],
    grey = vim.g.palette.grey[1],
    darkgrey = vim.g.palette.bg3[1]
}

local get_mode_color = function()
    local mode_colors = {
        [110] = colors.green,
        [105] = colors.blue,
        [99] = colors.green,
        [116] = colors.blue,
        [118] = colors.grey,
        [22] = colors.grey,
        [86] = colors.grey,
        [82] = colors.red,
        [115] = colors.red,
        [83] = colors.red
    }

    local mode_color = mode_colors[vim.fn.mode():byte()]
    if mode_color ~= nil then
        return mode_color
    else
        return colors.purple
    end
end

local white_space = function() return " " end

local file_readonly = function()
    if vim.bo.filetype == 'help' then return '' end
    if vim.bo.readonly == true then return '  ' end
    return ''
end

local get_current_file_name = function()
    local file = vim.fn.expand('%t')
    if vim.fn.empty(file) == 1 then return '' end
    if string.len(file_readonly()) ~= 0 then return file .. file_readonly() end
    if vim.bo.modifiable then
        if vim.bo.modified then return file .. ' [  ]' end
    end
    return file .. ' '
end

-- Left Sections
gls.left[1] = {
    leftRounded = {
        provider = function() return " " end,
        highlight = {colors.orange, colors.bg}
    }
}

gls.left[2] = {
    SiMode = {
        provider = function() return "   " end,
        highlight = {colors.bg, colors.orange}
    }
}

gls.left[3] = {
    ViMode = {
        provider = function()
            local aliases = {
                [110] = 'NORMAL',
                [105] = 'INSERT',
                [99] = 'COMMAND',
                [116] = 'TERMINAL',
                [118] = 'VISUAL',
                [22] = 'V-BLOCK',
                [86] = 'V-LINE',
                [82] = 'REPLACE',
                [115] = 'SELECT',
                [83] = 'S-LINE'
            }

            vim.api.nvim_command('hi GalaxyViMode guibg=' .. get_mode_color())
            local alias = aliases[vim.fn.mode():byte()]
            local mode
            if alias ~= nil then
                if utils.has_width_gt(35) then
                    mode = alias
                else
                    mode = alias:sub(1, 1)
                end
            else
                mode = vim.fn.mode():byte()
            end
            return '  ' .. mode .. ' '
        end,
        separator = " ",
        separator_highlight = {colors.darkgrey, colors.darkgrey},
        highlight = {colors.bg, colors.bg, "bold"}
    }
}

gls.left[4] = {
    FileIcon = {
        provider = "FileIcon",
        condition = condition.buffer_not_empty,
        highlight = {
            require("galaxyline.provider_fileinfo").get_file_icon_color,
            colors.darkgrey
        }
    }
}

gls.left[5] = {
    FileName = {
        provider = get_current_file_name,
        condition = condition.buffer_not_empty,
        highlight = {colors.fg, colors.darkgrey}
    }
}

gls.left[6] = {
    teech = {
        provider = function() return "" end,
        separator = " ",
        highlight = {colors.darkgrey, colors.bg}
    }
}

gls.left[7] = {
    DiffAdd = {
        provider = "DiffAdd",
        condition = condition.hide_in_width,
        icon = "   ",
        highlight = {colors.yellow}
    }
}

gls.left[8] = {
    DiffModified = {
        provider = "DiffModified",
        condition = condition.hide_in_width,
        icon = " ",
        highlight = {colors.orange}
    }
}

gls.left[9] = {
    DiffRemove = {
        provider = "DiffRemove",
        condition = condition.hide_in_width,
        icon = " ",
        highlight = {colors.red}
    }
}

gls.left[10] = {
    LeftEnd = {
        provider = function() return " " end,
        separator = " ",
        separator_highlight = {colors.darkgrey},
        highlight = {colors.darkgrey}
    }
}

-- Right Sections
gls.right[1] = {
    GitIcon = {
        provider = function() return "  " end,
        condition = require("galaxyline.provider_vcs").check_git_workspace,
        highlight = {colors.green}
    }
}

gls.right[2] = {
    GitBranch = {
        provider = "GitBranch",
        condition = require("galaxyline.provider_vcs").check_git_workspace,
        highlight = {colors.green}
    }
}

gls.right[3] = {
    right_LeftRounded = {
        provider = function() return "" end,
        separator = " ",
        separator_highlight = {colors.bg},
        highlight = {colors.fg}
    }
}

gls.right[4] = {
    PerCent = {
        provider = {white_space, "LinePercent"},
        highlight = {colors.bg, colors.fg}
    }
}

gls.right[5] = {
    rightRounded = {
        provider = function() return "" end,
        highlight = {colors.fg}
    }
}
