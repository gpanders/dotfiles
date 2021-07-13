return function(l, args, mods)
    local args = vim.fn.expandcmd(args)
    local grepcmd
    if string.match(vim.o.grepprg, "%$%*") then
        grepcmd = string.gsub(vim.o.grepprg, "%$%*", args)
    else
        grepcmd = vim.o.grepprg .. " " .. args
    end

    local opts = {
        stdout_buffered = true,
        stdin = "null",
        on_stdout = function(job, data, event)
            if data[#data] == "" then
                table.remove(data)
            end

            local what = { title = grepcmd, efm = vim.o.grepformat, nr = "$", lines = data }
            if l then
                vim.fn.setloclist(0, {}, " ", what)
            else
                vim.fn.setqflist({}, " ", what)
            end

            vim.cmd(mods .. " " .. (l and "lopen" or "copen"))
        end,
        on_exit = function()
            vim.cmd("doautocmd QuickFixCmdPost " .. (l and "lgrep" or "grep"))
        end,
    }

    vim.cmd("doautocmd QuickFixCmdPre " .. (l and "lgrep" or "grep"))
    vim.fn.jobstart(grepcmd, opts)
    print(grepcmd)
end
