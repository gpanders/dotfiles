local api = vim.api
local M = {}

local blockwise = "\22"
local edit_namespace = api.nvim_create_namespace("pi.edits")
local read_namespace = api.nvim_create_namespace("pi.reads")

local state = {
    buf = nil,
    job = nil,
    cwd = nil,
    source_win = nil,
    event_channel = nil,
    event_connecting = false,
    event_subscribed = false,
    event_remainder = "",
    tools = {},
    edit_snapshots = {},
    edited_buffers = {},
    read_buffers = {},
}

local socket_path = vim.fs.joinpath(vim.fn.stdpath("state"), "pi-" .. vim.fn.getpid() .. ".sock")
local extension_path = vim.fs.joinpath(vim.fn.stdpath("config"), "lua", "pi", "bridge.ts")
local present = require("present")

local function notify(message, level)
    vim.notify(message, level or vim.log.levels.ERROR, { title = "Pi" })
end

local function clear_highlights(buffers, namespace)
    for buf in pairs(buffers) do
        if api.nvim_buf_is_valid(buf) then
            api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
        end
    end
    return {}
end

local editor_tools = {}
local editor_tool_names = {}

local function load_editor_tools()
    local tools = {}
    for _, path in ipairs(api.nvim_get_runtime_file("tool/*.lua", true)) do
        local name = vim.fs.basename(path):sub(1, -5)
        if not name:match("^[a-z][a-z0-9_]*$") or #name > 64 then
            error("Invalid editor tool filename: " .. path, 0)
        end
        local definition = assert(loadfile(path))()
        if type(definition) ~= "table" then
            error(path .. ": expected a table", 0)
        end
        tools[name] = vim.tbl_deep_extend("force", tools[name] or {}, definition)
    end

    local names = vim.tbl_keys(tools)
    table.sort(names)
    for _, name in ipairs(names) do
        if type(tools[name].execute) ~= "function" then
            error("Editor tool has no execute function: " .. name, 0)
        end
        if tools[name].cleanup ~= nil and type(tools[name].cleanup) ~= "function" then
            error("Editor tool has an invalid cleanup function: " .. name, 0)
        end
    end
    editor_tools = tools
    editor_tool_names = names
end

local function list_editor_tools()
    load_editor_tools()
    local tools = {}
    for _, name in ipairs(editor_tool_names) do
        local tool = editor_tools[name]
        table.insert(tools, {
            name = name,
            label = tool.label,
            description = tool.description,
            parameters = tool.parameters,
            retrySafe = tool.retrySafe,
        })
    end
    return { tools = tools }
end

local function execute_editor_tool(request)
    if type(request.tool) ~= "string" or not editor_tools[request.tool] then
        error("Unknown editor tool: " .. tostring(request.tool), 0)
    end
    if type(request.arguments) ~= "table" then
        error("Editor tool arguments must be an object", 0)
    end
    if type(request.cwd) ~= "string" then
        error("Editor tool working directory must be a string", 0)
    end
    local result = editor_tools[request.tool].execute(request.arguments, request.cwd)
    if type(result) ~= "table" or type(result.text) ~= "string" then
        error("Editor tool returned an invalid result: " .. request.tool, 0)
    end
    return result
end

local function cleanup_editor_tools()
    for _, name in ipairs(editor_tool_names) do
        local cleanup = editor_tools[name].cleanup
        if cleanup then
            pcall(cleanup)
        end
    end
    editor_tools = {}
    editor_tool_names = {}
end

local function focus_terminal(win)
    api.nvim_set_current_win(win)
    vim.wo[win].statusline = " %{b:term_title} "
    vim.cmd("startinsert")
    return win
end

local function terminal_window()
    if state.job and (not state.buf or not api.nvim_buf_is_valid(state.buf)) then
        notify("Pi terminal is closing", vim.log.levels.WARN)
        return
    end
    if state.buf and api.nvim_buf_is_valid(state.buf) and not state.job then
        api.nvim_buf_delete(state.buf, { force = true })
        state.buf = nil
    end
    if state.buf then
        for _, win in ipairs(vim.fn.win_findbuf(state.buf)) do
            if api.nvim_win_is_valid(win) then
                return focus_terminal(win)
            end
        end
        vim.cmd("botright vsplit")
        api.nvim_win_set_buf(0, state.buf)
        return focus_terminal(api.nvim_get_current_win())
    end

    vim.cmd("botright vsplit")
    vim.cmd("enew")
    local win = api.nvim_get_current_win()
    state.buf = api.nvim_get_current_buf()
    state.cwd = vim.fn.getcwd()
    api.nvim_win_set_width(win, math.max(40, math.floor(vim.o.columns * 0.4)))

    vim.fn.delete(socket_path)
    local job = vim.fn.jobstart({ "pi", "--continue", "--tui-mode", "regular", "-e", extension_path }, {
        term = true,
        cwd = state.cwd,
        env = { PI_NVIM_SOCKET = socket_path, PI_DEBUG_REDRAW = "1" },
        on_exit = function(exited_job, code)
            vim.schedule(function()
                if state.job ~= exited_job then
                    return
                end
                state.job = nil
                state.cwd = nil
                if state.event_channel then
                    pcall(vim.fn.chanclose, state.event_channel)
                    state.event_channel = nil
                end
                state.event_connecting = false
                state.event_subscribed = false
                state.event_remainder = ""
                state.tools = {}
                state.edit_snapshots = {}
                cleanup_editor_tools()
                local buf = state.buf
                state.buf = nil
                if buf and api.nvim_buf_is_valid(buf) then
                    for _, terminal_win in ipairs(vim.fn.win_findbuf(buf)) do
                        pcall(api.nvim_win_close, terminal_win, true)
                    end
                    pcall(api.nvim_buf_delete, buf, { force = true })
                end
                vim.fn.delete(socket_path)
                if code ~= 0 then
                    notify("Pi exited with status " .. code)
                end
            end)
        end,
    })
    if job <= 0 then
        state.job = nil
        state.cwd = nil
        notify("Could not start Pi")
        return
    end

    state.job = job
    vim.bo[state.buf].bufhidden = "hide"
    vim.keymap.set("t", "<Esc>", "<Esc>", { buffer = state.buf })
    api.nvim_create_autocmd("TermLeave", {
        buffer = state.buf,
        callback = function(event)
            local current = api.nvim_get_current_win()
            if api.nvim_win_get_buf(current) == event.buf then
                api.nvim_win_set_cursor(current, { api.nvim_buf_line_count(event.buf), 0 })
            end
        end,
    })
    return focus_terminal(win)
end

local function source_file(bufnr)
    if not api.nvim_buf_is_valid(bufnr) or vim.bo[bufnr].buftype ~= "" then
        return nil, "the source must be a normal buffer"
    end
    if vim.bo[bufnr].modified then
        return nil, "save or discard source changes before using Pi"
    end

    local name = api.nvim_buf_get_name(bufnr)
    if name == "" then
        return nil, "the source buffer has no filename"
    end
    return vim.fs.abspath(name)
end

local function visual_selection(opts, bufnr)
    local kind = vim.fn.visualmode()
    if opts.range ~= 2 or (kind ~= "v" and kind ~= "V" and kind ~= blockwise) then
        return
    end

    local first = vim.fn.getpos("'<")
    local last = vim.fn.getpos("'>")
    local region = vim.fn.getregionpos(first, last, { type = kind })
    if #region == 0 or opts.line1 ~= region[1][1][2] or opts.line2 ~= region[#region][2][2] then
        return
    end

    return {
        lines = { start = math.min(first[2], last[2]), ["end"] = math.max(first[2], last[2]) },
        text = table.concat(vim.fn.getregion(first, last, { type = kind }), "\n"),
    }
end

local function capture_selection(opts, bufnr)
    if opts.range == 0 then
        return
    end

    local file, err = source_file(bufnr)
    if not file then
        return nil, err
    end
    local selection = visual_selection(opts, bufnr)
    if not selection then
        local first = math.min(opts.line1, opts.line2)
        local last = math.max(opts.line1, opts.line2)
        selection = {
            lines = { start = first, ["end"] = last },
            text = table.concat(api.nvim_buf_get_lines(bufnr, first - 1, last, false), "\n"),
        }
    end
    selection.file = file
    return selection
end

local function buffer_content(buf)
    local newline = ({ dos = "\r\n", mac = "\r", unix = "\n" })[vim.bo[buf].fileformat]
    local content = table.concat(api.nvim_buf_get_lines(buf, 0, -1, false), newline)
    if vim.bo[buf].endofline then
        content = content .. newline
    end
    if vim.bo[buf].bomb then
        content = "\239\187\191" .. content
    end
    return content
end

local function content_lines(content, revision)
    local has_bom = content:sub(1, 3) == "\239\187\191"
    if has_bom ~= revision.bomb then
        error("Pi edits cannot change the byte-order mark", 0)
    end
    if has_bom then
        content = content:sub(4)
    end

    if revision.fileformat == "dos" then
        local remainder = content:gsub("\r\n", "")
        if remainder:find("\r", 1, true) or remainder:find("\n", 1, true) then
            error("Pi edit changed the file line endings", 0)
        end
        content = content:gsub("\r\n", "\n")
    elseif revision.fileformat == "unix" then
        if content:find("\r", 1, true) then
            error("Pi edit changed the file line endings", 0)
        end
    else
        error("Unsupported file format: " .. tostring(revision.fileformat), 0)
    end

    local endofline = content:sub(-1) == "\n"
    if endofline ~= revision.endofline then
        error("Pi edits cannot change the final line ending", 0)
    end
    local lines = vim.split(content, "\n", { plain = true })
    if endofline then
        table.remove(lines)
    end
    if #lines == 0 then
        lines = { "" }
    end
    return lines
end

local function edit_changes(old_lines, new_lines, old_endofline, new_endofline)
    local old_text = table.concat(old_lines, "\n") .. (old_endofline and "\n" or "")
    local new_text = table.concat(new_lines, "\n") .. (new_endofline and "\n" or "")
    local changes = { added = {}, removed = {}, hunks = {} }
    for _, hunk in ipairs(vim.text.diff(old_text, new_text, { result_type = "indices" })) do
        local old_start, old_count, new_start, new_count = unpack(hunk)
        local at = math.max(1, new_start + (new_count == 0 and 1 or 0))
        table.insert(changes.hunks, {
            first = at,
            last = new_count == 0 and at or new_start + new_count - 1,
        })
        if new_count > 0 then
            table.insert(changes.added, { first = new_start, last = new_start + new_count - 1 })
        end
        if old_count > 0 then
            table.insert(changes.removed, {
                at = at,
                lines = vim.list_slice(old_lines, old_start, old_start + old_count - 1),
            })
        end
    end
    return changes
end

local function edit_read(request)
    if type(request.path) ~= "string" or type(request.toolCallId) ~= "string" then
        error("Invalid Neovim edit read request", 0)
    end
    local path = vim.fs.abspath(request.path)
    local buf = vim.fn.bufadd(path)
    vim.fn.bufload(buf)
    if not api.nvim_buf_is_valid(buf) or vim.bo[buf].buftype ~= "" then
        error("Pi can only edit normal file buffers", 0)
    end
    api.nvim_buf_call(buf, function()
        vim.cmd("silent checktime")
    end)
    local encoding = vim.bo[buf].fileencoding:lower()
    if encoding ~= "" and encoding ~= "utf-8" and encoding ~= "utf8" then
        error("Pi can only edit UTF-8 buffers", 0)
    end
    if vim.bo[buf].binary then
        error("Pi cannot edit binary buffers", 0)
    end
    if vim.bo[buf].fileformat == "mac" then
        error("Pi cannot edit Mac-format line endings", 0)
    end
    if vim.bo[buf].modified then
        error("Pi cannot edit a buffer with unsaved changes: " .. path, 0)
    end
    if not vim.bo[buf].modifiable or vim.bo[buf].readonly then
        error("Pi cannot edit a readonly buffer: " .. path, 0)
    end

    local revision = {
        buffer = buf,
        changedtick = api.nvim_buf_get_changedtick(buf),
        fileformat = vim.bo[buf].fileformat,
        fileencoding = vim.bo[buf].fileencoding,
        binary = vim.bo[buf].binary,
        bomb = vim.bo[buf].bomb,
        endofline = vim.bo[buf].endofline,
        fixendofline = vim.bo[buf].fixendofline,
    }
    local content = buffer_content(buf)
    if not vim.deep_equal(content_lines(content, revision), api.nvim_buf_get_lines(buf, 0, -1, false)) then
        error("Pi cannot represent this buffer without changing its bytes", 0)
    end
    state.edit_snapshots[request.toolCallId] = {
        path = path,
        content = content,
        lines = api.nvim_buf_get_lines(buf, 0, -1, false),
        revision = revision,
    }
    return { content = content, revision = revision }
end

local function disk_content(path)
    local fd, open_error = vim.uv.fs_open(path, "r", 438)
    if not fd then
        return nil, open_error
    end
    local stat, stat_error = vim.uv.fs_fstat(fd)
    if not stat then
        vim.uv.fs_close(fd)
        return nil, stat_error
    end
    local content, read_error = stat.size == 0 and "" or vim.uv.fs_read(fd, stat.size, 0)
    vim.uv.fs_close(fd)
    return content, read_error
end

local function restore_snapshot(buf, snapshot, undo_sequence)
    if not api.nvim_buf_is_valid(buf) then
        return
    end
    local ok = pcall(api.nvim_buf_call, buf, function()
        vim.cmd("silent undo " .. undo_sequence)
    end)
    vim.bo[buf].binary = snapshot.revision.binary
    vim.bo[buf].fileencoding = snapshot.revision.fileencoding
    vim.bo[buf].fileformat = snapshot.revision.fileformat
    vim.bo[buf].bomb = snapshot.revision.bomb
    vim.bo[buf].endofline = snapshot.revision.endofline
    vim.bo[buf].fixendofline = snapshot.revision.fixendofline
    if not ok or buffer_content(buf) ~= snapshot.content then
        error("Neovim could not restore the buffer after the failed write", 0)
    end
    vim.bo[buf].modified = false
end

local function edit_write(request)
    if type(request.path) ~= "string"
        or type(request.toolCallId) ~= "string"
        or type(request.content) ~= "string"
        or type(request.revision) ~= "table"
    then
        error("Invalid Neovim edit write request", 0)
    end

    local snapshot = state.edit_snapshots[request.toolCallId]
    if not snapshot or snapshot.path ~= vim.fs.abspath(request.path) then
        error("Neovim edit snapshot is missing", 0)
    end
    local revision = snapshot.revision
    if not vim.deep_equal(request.revision, revision) then
        error("Neovim edit snapshot does not match the write request", 0)
    end

    local buf = revision.buffer
    if not api.nvim_buf_is_valid(buf)
        or api.nvim_buf_get_name(buf) ~= snapshot.path
        or vim.bo[buf].buftype ~= ""
        or not vim.bo[buf].modifiable
        or vim.bo[buf].readonly
        or vim.bo[buf].modified
        or api.nvim_buf_get_changedtick(buf) ~= revision.changedtick
        or vim.bo[buf].fileformat ~= revision.fileformat
        or vim.bo[buf].fileencoding ~= revision.fileencoding
        or vim.bo[buf].binary ~= revision.binary
        or vim.bo[buf].bomb ~= revision.bomb
        or vim.bo[buf].endofline ~= revision.endofline
        or vim.bo[buf].fixendofline ~= revision.fixendofline
        or buffer_content(buf) ~= snapshot.content
    then
        error("Neovim buffer changed while Pi prepared the edit: " .. snapshot.path, 0)
    end
    local disk, disk_error = disk_content(snapshot.path)
    if disk ~= snapshot.content then
        error("File changed while Pi prepared the edit: " .. tostring(disk_error or snapshot.path), 0)
    end

    local lines = content_lines(request.content, revision)
    local undo_sequence = api.nvim_buf_call(buf, function()
        return vim.fn.undotree().seq_cur
    end)
    api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].fixendofline = false
    local write_ok, write_error = pcall(api.nvim_buf_call, buf, function()
        vim.cmd("silent write")
    end)
    if api.nvim_buf_is_valid(buf) then
        vim.bo[buf].fixendofline = revision.fixendofline
    end
    if not write_ok then
        local disk = disk_content(snapshot.path)
        if disk == snapshot.content then
            restore_snapshot(buf, snapshot, undo_sequence)
            error("Neovim did not save the Pi edit: " .. tostring(write_error), 0)
        end
        error("Neovim write failed after the file changed on disk. Do not retry this edit: " .. tostring(write_error), 0)
    end
    if not api.nvim_buf_is_valid(buf) or vim.bo[buf].modified then
        error("Neovim saved the Pi edit, but a write hook left unsaved changes. Do not retry this edit", 0)
    end

    local actual_lines = api.nvim_buf_get_lines(buf, 0, -1, false)
    local actual = buffer_content(buf)
    local disk, read_error = disk_content(snapshot.path)
    if disk ~= actual then
        error("Neovim saved the Pi edit, but the file changed again. Do not retry this edit: " .. tostring(read_error or snapshot.path), 0)
    end
    local changedtick = api.nvim_buf_get_changedtick(buf)
    local tool = state.tools[request.toolCallId]
    if tool then
        tool.changedtick = changedtick
        tool.modified = false
        tool.changes = edit_changes(snapshot.lines, actual_lines, revision.endofline, vim.bo[buf].endofline)
    end
    return { digest = vim.fn.sha256(actual), changedtick = changedtick }
end

local function handle_nvim_request(channel, request)
    if channel ~= state.event_channel then
        return
    end
    local handler = ({
        ["tools/list"] = list_editor_tools,
        ["tools/execute"] = execute_editor_tool,
        read = edit_read,
        write = edit_write,
    })[request.operation]
    local ok, result = pcall(handler or function()
        error("Unknown Neovim operation", 0)
    end, request)
    if type(request.toolCallId) == "string" and (request.operation == "write" or not ok) then
        state.edit_snapshots[request.toolCallId] = nil
    end
    if channel == state.event_channel then
        local response = ok and result or { error = tostring(result) }
        response.type = "nvim_response"
        response.requestId = request.requestId
        response.ok = ok
        vim.fn.chansend(channel, vim.json.encode(response) .. "\n")
    end
end

local function buffer_conflict(tool, buf)
    return tool.modified
        or vim.bo[buf].modified
        or (tool.changedtick and api.nvim_buf_get_changedtick(buf) ~= tool.changedtick)
end

local function prepare_buffer(tool)
    local buf = vim.fn.bufnr(tool.path)
    if buf < 0 or not api.nvim_buf_is_loaded(buf) then
        return true
    end
    if buffer_conflict(tool, buf) then
        notify("Pi changed " .. tool.path .. " while the buffer has unsaved changes", vim.log.levels.WARN)
        return false
    end
    api.nvim_buf_call(buf, function()
        vim.cmd("silent checktime")
    end)
    return true
end

local function show_read(tool, event)
    if not tool.show or event.isImage or not vim.uv.fs_stat(tool.path) then
        return
    end
    local buf = vim.fn.bufadd(tool.path)
    if buffer_conflict(tool, buf) then
        notify("Cannot show Pi read because the buffer changed", vim.log.levels.WARN)
        return
    end
    vim.fn.bufload(buf)
    api.nvim_buf_call(buf, function()
        vim.cmd("silent checktime")
    end)

    local line_count = api.nvim_buf_line_count(buf)
    local first = math.max(1, math.floor(tonumber(tool.args.offset) or 1))
    if first > line_count then
        return
    end
    local limit = tonumber(tool.args.limit)
    local truncation = event.details and event.details.truncation
    if type(truncation) == "table" and type(truncation.outputLines) == "number" then
        limit = limit and math.min(limit, truncation.outputLines) or truncation.outputLines
    end
    if limit and limit < 1 then
        return
    end
    local last = limit and math.min(line_count, first + math.floor(limit) - 1) or line_count
    vim.hl.range(buf, read_namespace, "Visual", { first - 1, 0 }, { last - 1, -1 }, {
        regtype = "V",
        inclusive = true,
    })
    state.read_buffers[buf] = true
    pcall(present.buffer, buf, {
        line = math.floor((first + last) / 2),
        anchor_win = tool.terminal_win,
        preferred_win = tool.preferred_win,
    })
end

local function show_edit(tool, event)
    if not prepare_buffer(tool) then
        return
    end
    local changes = tool.changes or { added = {}, removed = {}, hunks = {} }
    if #changes.added == 0 and #changes.removed == 0 then
        return
    end

    if not tool.show then
        return
    end
    local buf = vim.fn.bufadd(tool.path)
    vim.fn.bufload(buf)
    api.nvim_buf_call(buf, function()
        vim.cmd("silent checktime")
    end)

    local line_count = api.nvim_buf_line_count(buf)
    for _, hunk in ipairs(changes.hunks) do
        local first = math.max(1, math.min(hunk.first, line_count))
        local last = math.max(first, math.min(hunk.last, line_count))
        for line = first, last do
            local row = line - 1
            for _, mark in ipairs(api.nvim_buf_get_extmarks(
                buf,
                edit_namespace,
                { row, 0 },
                { row, -1 },
                { type = "sign" }
            )) do
                api.nvim_buf_del_extmark(buf, edit_namespace, mark[1])
            end
            api.nvim_buf_set_extmark(buf, edit_namespace, row, 0, {
                sign_text = "│",
                sign_hl_group = "Changed",
            })
        end
    end
    for _, range in ipairs(changes.added) do
        local first = math.max(1, math.min(range.first, line_count))
        local last = math.max(first, math.min(range.last, line_count))
        vim.hl.range(buf, edit_namespace, "Added", { first - 1, 0 }, { last - 1, -1 }, {
            regtype = "V",
            inclusive = true,
        })
    end
    for _, removed in ipairs(changes.removed) do
        local lines = {}
        for _, line in ipairs(removed.lines) do
            table.insert(lines, { { line, "Removed" } })
        end
        local at_end = removed.at > line_count
        api.nvim_buf_set_extmark(buf, edit_namespace, at_end and line_count - 1 or removed.at - 1, 0, {
            virt_lines = lines,
            virt_lines_above = not at_end,
            virt_lines_overflow = "scroll",
        })
    end
    state.edited_buffers[buf] = true

    local added = changes.added[1] and changes.added[1].first
    local removed = changes.removed[1] and changes.removed[1].at
    local first = added and removed and math.min(added, removed) or added or removed
    pcall(present.buffer, buf, {
        line = math.max(1, math.min(first, line_count)),
        anchor_win = tool.terminal_win,
        preferred_win = tool.preferred_win,
    })
end

local function bridge_event(event)
    if event.type == "subscribed" then
        state.event_subscribed = true
        return
    elseif event.type == "subscription_error" then
        state.event_subscribed = false
        notify(type(event.error) == "string" and event.error or "Pi rejected the editor tool catalog")
        return
    elseif event.type == "prompt_start" then
        state.edited_buffers = clear_highlights(state.edited_buffers, edit_namespace)
        if not event.external then
            state.source_win = nil
        end
    elseif event.type == "assistant_start" or event.type == "tool_start" then
        state.read_buffers = clear_highlights(state.read_buffers, read_namespace)
    elseif event.type == "agent_settled" then
        state.source_win = nil
    end

    if event.type == "tool_start" then
        local args = event.args or {}
        if (event.tool == "read" or event.tool == "edit" or event.tool == "write") and type(args.path) == "string" then
            local path = vim.fs.abspath(args.path)
            local buf = vim.fn.bufnr(path)
            local current = api.nvim_get_current_win()
            state.tools[event.id] = {
                name = event.tool,
                args = args,
                path = vim.fs.abspath(path),
                modified = buf >= 0 and vim.bo[buf].modified or false,
                changedtick = buf >= 0 and api.nvim_buf_is_loaded(buf) and api.nvim_buf_get_changedtick(buf) or nil,
                preferred_win = state.source_win,
                show = state.buf and api.nvim_win_get_buf(current) == state.buf,
                terminal_win = current,
            }
        end
    elseif event.type == "tool_end" then
        local tool = state.tools[event.id]
        state.tools[event.id] = nil
        state.edit_snapshots[event.id] = nil
        if not tool or event.isError then
            return
        end
        if type(event.changedtick) == "number" then
            tool.changedtick = event.changedtick
            tool.modified = false
        end
        if type(event.path) == "string" and vim.fs.abspath(event.path) ~= tool.path then
            tool.path = vim.fs.abspath(event.path)
            local buf = vim.fn.bufnr(tool.path)
            tool.modified = buf >= 0 and vim.bo[buf].modified or false
            tool.changedtick = nil
        end
        if tool.name == "read" then
            show_read(tool, event)
        elseif tool.name == "edit" then
            show_edit(tool, event)
        else
            prepare_buffer(tool)
        end
    end
end

local function read_events(channel, data)
    if state.event_channel ~= channel then
        return
    end
    if #data == 1 and data[1] == "" then
        state.event_channel = nil
        state.event_subscribed = false
        state.event_remainder = ""
        state.tools = {}
        state.edit_snapshots = {}
        cleanup_editor_tools()
        state.edited_buffers = clear_highlights(state.edited_buffers, edit_namespace)
        state.read_buffers = clear_highlights(state.read_buffers, read_namespace)
        if state.job then
            vim.defer_fn(M.connect_events, 100)
        end
        return
    end

    state.event_remainder = state.event_remainder .. table.concat(data, "\n")
    while true do
        local newline = state.event_remainder:find("\n", 1, true)
        if not newline then
            return
        end
        local line = state.event_remainder:sub(1, newline - 1)
        state.event_remainder = state.event_remainder:sub(newline + 1)
        local ok, event = pcall(vim.json.decode, line)
        if ok and type(event) == "table" then
            vim.schedule(function()
                if channel ~= state.event_channel then
                    return
                end
                if event.type == "nvim_request" then
                    handle_nvim_request(channel, event)
                else
                    bridge_event(event)
                end
            end)
        end
    end
end

function M.connect_events()
    if state.event_channel or state.event_connecting or not state.job then
        return
    end
    state.event_connecting = true
    local job = state.job

    local function connect(attempt)
        if state.job ~= job then
            state.event_connecting = false
            return
        end
        local ok, channel = pcall(vim.fn.sockconnect, "pipe", socket_path, { on_data = read_events })
        if ok and channel > 0 then
            state.event_channel = channel
            state.event_connecting = false
            state.event_subscribed = false
            vim.fn.chansend(channel, '{"type":"subscribe"}\n')
        elseif attempt == 100 then
            state.event_connecting = false
            notify("Could not subscribe to Pi tool events")
        else
            vim.defer_fn(function()
                connect(attempt + 1)
            end, 100)
        end
    end

    connect(1)
end

local function send_to_pi(message, job, attempt)
    if state.job ~= job then
        return
    end
    if not state.event_subscribed then
        if attempt == 100 then
            notify("Could not subscribe to Pi tool events")
            return
        end
        vim.defer_fn(function()
            send_to_pi(message, job, attempt + 1)
        end, 100)
        return
    end

    local response = ""
    local ok, channel = pcall(vim.fn.sockconnect, "pipe", socket_path, {
        on_data = function(_, data)
            response = response .. table.concat(data, "\n")
            while true do
                local newline = response:find("\n", 1, true)
                if not newline then
                    return
                end
                local line = response:sub(1, newline - 1)
                response = response:sub(newline + 1)
                local decoded, value = pcall(vim.json.decode, line)
                if decoded and type(value) == "table" and value.error then
                    notify(value.error)
                end
            end
        end,
    })
    if ok and channel > 0 then
        vim.fn.chansend(channel, vim.json.encode(message) .. "\n")
        return
    end
    if attempt == 100 then
        notify("Could not connect to the Pi bridge")
        return
    end
    vim.defer_fn(function()
        send_to_pi(message, job, attempt + 1)
    end, 100)
end

local function send(request, selection)
    local source_win = api.nvim_get_current_win()
    if not terminal_window() then
        return
    end
    state.source_win = source_win
    M.connect_events()
    send_to_pi({ type = "prompt", request = request, selection = selection }, state.job, 1)
end

function M.command(opts)
    local source_buf = api.nvim_get_current_buf()
    local selection, err = capture_selection(opts, source_buf)
    if err then
        notify(err, vim.log.levels.WARN)
        return
    end

    local request = opts.args or ""
    if vim.trim(request) ~= "" then
        send(request, selection)
    elseif not selection then
        if terminal_window() then
            M.connect_events()
        end
    else
        vim.ui.input({ prompt = "Pi: " }, function(input)
            if input and vim.trim(input) ~= "" then
                send(input, selection)
            end
        end)
    end
end

function M.cleanup()
    if state.event_channel then
        pcall(vim.fn.chanclose, state.event_channel)
        state.event_channel = nil
    end
    state.event_subscribed = false
    state.edit_snapshots = {}
    cleanup_editor_tools()
    state.edited_buffers = clear_highlights(state.edited_buffers, edit_namespace)
    state.read_buffers = clear_highlights(state.read_buffers, read_namespace)
    if state.job then
        vim.fn.jobstop(state.job)
        state.job = nil
    end
    vim.fn.delete(socket_path)
end

return M
