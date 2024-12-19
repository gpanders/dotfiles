--- @class LspConfig : vim.lsp.ClientConfig
--- @field enabled boolean? If false, this LSP server is disabled.
--- @field filetype string|string[] Filetypes to enable this server for
--- @field autostart boolean? If true, automatically start the server
--- @field cmd string[] Command to start the server
--- @field root_dir (string|fun():string)? List of root markers
--- @field settings table? Optional settings forwarded to the server

--- @class LspOptions
--- @field autostart boolean? Set the global autostart value used by all servers
--- @field capabilities table? Optional capabilities
--- @field handlers table<string, function>? Optional handlers
--- @field on_init function? Optional init function
--- @field before_init function? Optional function to run before initialization

local M = {}

--- Mapping of server name to autocommand ID
local autocmds = {} ---@type table<string, integer>

--- Load an LSP configuration from the runtimepath
---
--- @param name string Name of the configuration to load
--- @return LspConfig? # LSP configuration table
function M.load(name)
    local paths = vim.api.nvim_get_runtime_file(string.format('lsp/%s.lua', name), true)
    if #paths == 0 then
        return nil
    end

    local config = {}

    for _, path in ipairs(paths) do
        local f = assert(loadfile(path))
        config = vim.tbl_deep_extend('force', config, f())
    end

    return config
end

--- Configure a single LSP server
---
--- @param server string Server name
--- @param opts LspOptions? Additional options
--- @return boolean # False if an error occurred
--- @return string? # Error message, if any
local function config(server, opts)
    opts = opts or {}

    local cfg = M.load(server)
    if not cfg then
        return false, string.format('No LSP configuration found for %s', server)
    end

    if cfg.enabled == false then
        return true, nil
    end

    local ft = cfg.filetype
    if not ft then
        return false,
            string.format(
                'Invalid LSP configuration for %s: missing required field "filetype"',
                server
            )
    end

    if type(ft) == 'string' then
        ft = { ft }
    end

    if type(ft) ~= 'table' then
        return false,
            string.format(
                'Invalid LSP configuration for %s: field "filetype" must be a string or table',
                server
            )
    end

    if autocmds[server] then
        vim.api.nvim_del_autocmd(autocmds[server])
    end

    local group = vim.api.nvim_create_augroup('nvim_lsp', {
        clear = false,
    })

    local id = vim.api.nvim_create_autocmd('FileType', {
        pattern = ft,
        group = group,
        callback = function()
            if vim.F.if_nil(cfg.autostart, opts.autostart, true) == false then
                return
            end

            local capabilities = vim.tbl_deep_extend(
                'force',
                vim.lsp.protocol.make_client_capabilities(),
                opts.capabilities or {}
            )

            local root_dir = cfg.root_dir
            if type(root_dir) == 'function' then
                root_dir = root_dir()
            end

            if not root_dir then
                root_dir = vim.uv.cwd()
            end

            vim.lsp.start(
                vim.tbl_deep_extend('keep', {
                    root_dir = root_dir,
                }, cfg, {
                    name = server,
                    before_init = opts.before_init,
                    on_init = opts.on_init,
                    capabilities = capabilities,
                    handlers = opts.handlers,
                }),
                {
                    silent = true,
                }
            )
        end,
    })

    autocmds[server] = id

    return true, nil
end

--- Configure the LSP subsystem
---
--- @param servers [string] A list of servers to configure
--- @param opts LspOptions? Additional options
function M.config(servers, opts)
    for _, server in ipairs(servers) do
        local ok, err = config(server, opts)
        if not ok and err then
            vim.notify(err, vim.log.levels.WARN)
        end
    end
end

return M
