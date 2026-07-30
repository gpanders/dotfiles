local M = {}
local H = {}

M.gen_source = function(opts)
  opts = vim.tbl_extend('force', { debounce = 50 }, opts or {})

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

    local timer = vim.uv.new_timer()
    if timer == nil then return false end

    H.cache[buf_id] = { root = root, repo = repo, timer = timer, fs_events = {} }
    H.start_watchers(buf_id, opts)
    H.schedule_refresh(buf_id, 0)
  end

  local detach = function(buf_id) H.clear_cache(buf_id) end

  return { name = 'jj', attach = attach, detach = detach }
end

H.cache = {}

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

H.start_watchers = function(buf_id, opts)
  local cache = H.cache[buf_id]
  if cache == nil then return end

  H.watch_dir(buf_id, vim.fs.joinpath(cache.repo, 'op_heads', 'heads'), nil, opts)
  H.watch_dir(buf_id, vim.fs.joinpath(cache.root, '.jj', 'working_copy'), { checkout = true, tree_state = true }, opts)
end

H.watch_dir = function(buf_id, path, filenames, opts)
  local fs_event = vim.uv.new_fs_event()
  if fs_event == nil then return end

  local on_event = function(_, filename)
    if filenames ~= nil and filename ~= nil and not filenames[filename] then return end
    H.schedule_refresh(buf_id, opts.debounce)
  end
  if not fs_event:start(path, { recursive = false }, on_event) then
    fs_event:close()
    return
  end
  table.insert(H.cache[buf_id].fs_events, fs_event)
end

H.schedule_refresh = function(buf_id, delay)
  local cache = H.cache[buf_id]
  if cache == nil then return end

  cache.timer:stop()
  cache.timer:start(delay or 0, 0, vim.schedule_wrap(function() H.set_ref_text(buf_id) end))
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
  for _, fs_event in ipairs(cache.fs_events) do
    pcall(fs_event.stop, fs_event)
    pcall(fs_event.close, fs_event)
  end
  pcall(cache.timer.stop, cache.timer)
  pcall(cache.timer.close, cache.timer)
end

return M
