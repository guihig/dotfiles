local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")
local utils = require("utils")
local wgt = require("widgets")

-- Task list
local function task_list(s)
    local _task_list = awful.widget.tasklist {
        screen = s,
        filter = awful.widget.tasklist.filter.focused,
        style = { shape = gears.shape.octogon },
        layout = {
            spacing = 5,
            forced_num_rows = 1,
            layout = wibox.layout.grid.horizontal
        },
        widget_template = {
            {
                {
                    {
                        {
                            { id = "icon_role", widget = wibox.widget.imagebox },
                            top = 3,
                            bottom = 3,
                            left = 3,
                            right = 5,
                            widget = wibox.container.margin
                        },
                        {
                            { id = "text_role", widget = wibox.widget.textbox },
                            margins = 3,
                            widget = wibox.container.margin
                        },
                        layout = wibox.layout.fixed.horizontal
                    },
                    left = 5,
                    right = 5,
                    widget = wibox.container.margin
                },
                id = "background_role",
                widget = wibox.container.background
            },
            margins = 2,
            widget = wibox.container.margin
        }
    }

    return _task_list
end

-- Tag list
local function tag_list(s)
    local tag_list_buttons = gears.table.join( -- Left click
    awful.button({}, 1, function(t) t:view_only() end), -- Mod Left Click
    awful.button({ Modkey }, 1, function(t)
        if client.focus then client.focus:move_to_tag(t) end
    end), -- Right Click
    awful.button({}, 3, awful.tag.viewtoggle), -- Mod Right Click
    awful.button({ Modkey }, 3, function(t)
        if client.focus then client.focus:toggle_tag(t) end
    end))

    local _tag_list = awful.widget.taglist {
        screen = s,
        filter = awful.widget.taglist.filter.all,
        buttons = tag_list_buttons
    }

    return _tag_list
end

-- Layout box
local function layout_box(s)
    local layout_box_buttons = gears.table.join( --- Left click
    awful.button({}, 1, function()
        awful.layout.inc(1)
        utils.handle_master_count()
    end), --- Right click
    awful.button({}, 3, function()
        awful.layout.inc(-1)
        utils.handle_master_count()
    end), --- Scrolling
    awful.button({}, 4, function()
        awful.layout.inc(-1)
        utils.handle_master_count()
    end), awful.button({}, 5, function()
        awful.layout.inc(1)
        utils.handle_master_count()
    end))

    local _layout_box = awful.widget.layoutbox {
        screen = s,
        buttons = layout_box_buttons,
        forced_height = 16,
        forced_width = 16
    }

    return _layout_box
end

-- Systray
local function systray()
    local _systray = wibox.widget.systray()

    _systray.set_base_size(16)

    return _systray
end

-- Clock
local function clock()
    local cw = calendar_widget({
        placement = "top_right",
        start_sunday = true,
        radius = 8,
        -- with customized next/previous (see table above)
        previous_month_button = 1,
        next_month_button = 3
    })
    -- local icon = wibox.widget({
    --     text = "󰃰",
    --     align = "center",
    --     valign = "center",
    --     font = beautiful.icon_font,
    --     widget = wibox.widget.textbox
    -- })
    --
    -- icon:connect_signal("button::press",
    --                     function(self, _lx, _ly, button, _mods,
    --                              _find_widget_results)
    --     if button == 1 then cw.toggle() end
    -- end)

    local clock_text = wibox.widget({
        widget = wibox.widget.textclock,
        format = "%H:%M",
        valign = "center"
    })

    clock_text:connect_signal("button::press",
                              function(self, _lx, _ly, button, _mods,
                                       _find_widget_results)
        local short_format = "%H:%M"
        local long_format = "%d/%m/%Y %H:%M"

        if button == 1 then
            if self.format == long_format then
                self.format = short_format
            else
                self.format = long_format
            end
        end
    end)

    return wibox.widget({
        widget = wibox.container.background,
        bg = beautiful.bg_focus,
        shape = gears.shape.octogon,
        opacity = 0.8,
        {
            layout = wibox.layout.fixed.horizontal,
            widget = wibox.container.place,
            valign = "center",
            -- { widget = wibox.container.margin, left = 5, right = 5, icon },
            { widget = wibox.container.margin, right = 5, clock_text }
        }
    })
end

awful.screen.connect_for_each_screen(function(s)
    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Layout box
    s.mylayoutbox = layout_box(s)

    -- Tag list
    s.mytaglist = tag_list(s)

    -- Task list
    s.mytasklist = task_list(s)

    -- Systray
    s.mysystray = systray()

    -- Create the wibox
    s.mywibox = awful.wibar {
        position = "top",
        screen = s,
        widget = {
            layout = wibox.layout.align.horizontal,
            { -- Left
                layout = wibox.layout.fixed.horizontal,
                s.mytaglist,
                s.mypromptbox
            },
            s.mytasklist,
            { -- Right
                {
                    layout = wibox.layout.fixed.horizontal,
                    spacing = 5,
                    wgt.volume,
                    -- clock(),
                    s.mysystray
                },
                widget = wibox.container.margin,
                margins = 3
            }
        }
    }
end)
