local M = {}
local H = {}

M.gen_source = function(opts)
  opts = vim.tbl_extend('force', { debounce = 50, poll_interval = 250 }, opts or {})

  local attach = function(buf_id)
    if H.cache[buf_id] ~= nil then return false end
    if vim.fn.executable('jj') ~= 1 then return false end

    local path = H.get_buf_realpath(buf_id)
    if path == '' then return false end

    local root = vim.fs.root(path, '.jj')
    local rel_path = root and vim.fs.relpath(root, path)
    if rel_path == nil or rel_path == '' then return false end

    local repo = H.get_repo_path(root)
    if not H.is_dir(repo) or not H.is_dir(vim.fs.joinpath(root, '.jj', 'working_copy')) then return false end

    local workspace = H.workspaces[root]
    if workspace == nil then
      local timer = vim.uv.new_timer()
      if timer == nil then return false end
      workspace = { root = root, timer = timer, polls = {}, buffers = {} }
      H.workspaces[root] = workspace
      H.start_polling(workspace, repo, opts)
    end

    H.cache[buf_id] = { root = root, workspace = workspace }
    workspace.buffers[buf_id] = true
    H.set_ref_text(buf_id)
  end

  local detach = function(buf_id) H.clear_cache(buf_id) end

  return { name = 'jj', attach = attach, detach = detach }
end

H.cache = {}
H.workspaces = {}

H.get_buf_realpath = function(buf_id)
  local path = vim.api.nvim_buf_get_name(buf_id)
  if path == '' then return '' end
  return vim.fn.resolve(vim.fs.abspath(path))
end

H.is_dir = function(path)
  local stat = vim.uv.fs_stat(path)
  return stat ~= nil and stat.type == 'directory'
end

H.get_repo_path = function(root)
  local jj_path = vim.fs.joinpath(root, '.jj')
  local repo_path = vim.fs.joinpath(jj_path, 'repo')
  local stat = vim.uv.fs_stat(repo_path)
  if stat == nil or stat.type ~= 'file' then return repo_path end

  local file = io.open(repo_path, 'r')
  if file == nil then return repo_path end
  local target = file:read('*a') or ''
  file:close()
  if target == '' then return repo_path end

  local normalize_opts = { expand_env = false }
  if target:match('^[/\\]') or target:match('^%a:[/\\]') then return vim.fs.normalize(target, normalize_opts) end
  return vim.fs.normalize(vim.fs.joinpath(jj_path, target), normalize_opts)
end

H.start_polling = function(workspace, repo, opts)
  H.poll_path(workspace, vim.fs.joinpath(repo, 'op_heads', 'heads'), opts)
  H.poll_path(workspace, vim.fs.joinpath(workspace.root, '.jj', 'working_copy', 'checkout'), opts)
end

H.poll_path = function(workspace, path, opts)
  -- Polling avoids synchronous FSEvents shutdown on macOS.
  local poll = vim.uv.new_fs_poll()
  if poll == nil then return end

  local on_change = function()
    H.schedule_refresh(workspace, opts.debounce)
  end
  if not poll:start(path, opts.poll_interval, on_change) then
    poll:close()
    return
  end
  table.insert(workspace.polls, poll)
end

H.schedule_refresh = function(workspace, delay)
  if H.workspaces[workspace.root] ~= workspace then return end

  workspace.timer:stop()
  workspace.timer:start(delay, 0, vim.schedule_wrap(function()
    if H.workspaces[workspace.root] ~= workspace then return end
    for buf_id in pairs(workspace.buffers) do H.set_ref_text(buf_id) end
  end))
end

H.stop_job = function(cache)
  local job = cache.job
  cache.job = nil
  if job ~= nil then pcall(job.kill, job, 15) end
end

H.run_job = function(buf_id, cache, args, on_exit)
  H.stop_job(cache)

  local job
  job = vim.system(args, { cwd = cache.root, text = true }, vim.schedule_wrap(function(obj)
    if H.cache[buf_id] ~= cache or cache.job ~= job then return end
    cache.job = nil
    on_exit(obj)
  end))
  cache.job = job
end

H.file_args = function(revision, fileset)
  return {
    'jj',
    '--ignore-working-copy',
    '--no-pager',
    '--color=never',
    'file',
    'show',
    '-r',
    revision,
    '--template',
    '',
    '--',
    fileset,
  }
end

H.set_ref_text = function(buf_id)
  local cache = H.cache[buf_id]
  if cache == nil or not vim.api.nvim_buf_is_valid(buf_id) then return end

  local rel_path = vim.fs.relpath(cache.root, H.get_buf_realpath(buf_id))
  if rel_path == nil or rel_path == '' then return H.call_minidiff(buf_id, {}) end
  local fileset = 'root-file:' .. vim.json.encode(rel_path)

  H.run_job(buf_id, cache, H.file_args('@-', fileset), function(obj)
    if obj.code == 0 and obj.stdout ~= nil then return H.call_minidiff(buf_id, obj.stdout) end

    local stderr = obj.stderr or ''
    if stderr:find('resolved to more than one revision', 1, true) then
      return H.set_merged_ref_text(buf_id, cache, fileset)
    end
    H.call_minidiff(buf_id, stderr:find('No such path', 1, true) and '' or {})
  end)
end

H.set_merged_ref_text = function(buf_id, cache, fileset)
  local args = {
    'jj',
    '--ignore-working-copy',
    '--no-pager',
    '--color=never',
    'diff',
    '-r',
    '@',
    '--name-only',
    '--',
    fileset,
  }
  H.run_job(buf_id, cache, args, function(obj)
    if obj.code ~= 0 or obj.stdout == nil then return H.call_minidiff(buf_id, {}) end
    if obj.stdout ~= '' then return H.set_changed_merged_ref_text(buf_id, cache, fileset) end

    H.run_job(buf_id, cache, H.file_args('@', fileset), function(show_obj)
      if show_obj.code == 0 and show_obj.stdout ~= nil then return H.call_minidiff(buf_id, show_obj.stdout) end
      local stderr = show_obj.stderr or ''
      H.call_minidiff(buf_id, stderr:find('No such path', 1, true) and '' or {})
    end)
  end)
end

H.set_changed_merged_ref_text = function(buf_id, cache, fileset)
  local cat = vim.fn.exepath('cat')
  if cat == '' then return H.call_minidiff(buf_id, {}) end

  local args = {
    'jj',
    '--ignore-working-copy',
    '--no-pager',
    '--color=never',
    '--config',
    'merge-tools.mini-diff.program=' .. vim.json.encode(cat),
    '--config',
    'merge-tools.mini-diff.diff-args=["$left"]',
    '--config',
    'merge-tools.mini-diff.diff-invocation-mode="file-by-file"',
    'diff',
    '--tool',
    'mini-diff',
    '-r',
    '@',
    '--',
    fileset,
  }
  H.run_job(buf_id, cache, args, function(obj)
    H.call_minidiff(buf_id, obj.code == 0 and obj.stdout or {})
  end)
end

H.call_minidiff = function(buf_id, text)
  pcall(require('mini.diff').set_ref_text, buf_id, text)
end

H.clear_cache = function(buf_id)
  local cache = H.cache[buf_id]
  H.cache[buf_id] = nil
  if cache == nil then return end

  H.stop_job(cache)
  local workspace = cache.workspace
  workspace.buffers[buf_id] = nil
  if next(workspace.buffers) ~= nil then return end

  H.workspaces[workspace.root] = nil
  for _, poll in ipairs(workspace.polls) do
    pcall(poll.stop, poll)
    pcall(poll.close, poll)
  end
  pcall(workspace.timer.stop, workspace.timer)
  pcall(workspace.timer.close, workspace.timer)
end

vim.api.nvim_create_autocmd('VimLeavePre', {
  group = vim.api.nvim_create_augroup('MiniDiffSourceJj', { clear = true }),
  callback = function()
    for buf_id in pairs(H.cache) do H.clear_cache(buf_id) end
  end,
})

return M
