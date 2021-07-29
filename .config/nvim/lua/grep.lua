return function(l, cmdargs, mods)
    local expanded = vim.fn.expandcmd(cmdargs)
    local grepcmd
    if string.match(vim.o.grepprg, "%$%*") then
        grepcmd = string.gsub(vim.o.grepprg, "%$%*", expanded)
    else
        grepcmd = vim.o.grepprg .. " " .. expanded
    end

    local args = vim.split(grepcmd, " ")
    local cmd = table.remove(args, 1)

    local stdout = vim.loop.new_pipe(false)
    local chunks = {}

    vim.api.nvim_command("doautocmd QuickFixCmdPre " .. (l and "lgrep" or "grep"))

    vim.loop.spawn(cmd, {
        args = args,
        stdio = { nil, stdout, nil }
    }, vim.schedule_wrap(function()
        local lines = vim.split(vim.trim(table.concat(chunks)), "\n")
        local what = { title = grepcmd, efm = vim.o.grepformat, nr = "$", lines = lines }
        if l then
            vim.fn.setloclist(0, {}, " ", what)
        else
            vim.fn.setqflist({}, " ", what)
        end
        vim.api.nvim_command(mods .. " " .. (l and "lopen" or "copen"))
        vim.api.nvim_command("doautocmd QuickFixCmdPost " .. (l and "lgrep" or "grep"))
    end))

    vim.loop.read_start(stdout, function(err, data)
        assert(not err, err)
        if data then
            table.insert(chunks, data)
        end
    end)

    print(grepcmd)
end
