(local api vim.api)

(fn normal-window? [win ?tab]
  (and (= (type win) :number)
       (api.nvim_win_is_valid win)
       (or (not ?tab) (= (api.nvim_win_get_tabpage win) ?tab))
       (= (. (api.nvim_win_get_config win) :relative) "")))

(fn first-normal-window [wins ?tab]
  (var found nil)
  (each [_ win (ipairs wins) &until found]
    (when (normal-window? win ?tab)
      (set found win)))
  found)

(fn visible-window [buf ?preferred ?tab]
  (if (and (normal-window? ?preferred ?tab)
           (= (api.nvim_win_get_buf ?preferred) buf))
      ?preferred
      (first-normal-window (vim.fn.win_findbuf buf) ?tab)))

(fn replaceable-window? [win anchor tab]
  (and (normal-window? win tab)
       (not= win anchor)
       (let [buf (api.nvim_win_get_buf win)]
         (and (= (. vim.bo buf :buftype) "")
              (not (. vim.bo buf :modified))
              (not (. vim.wo win :winfixbuf))))))

(fn presentation-window [buf anchor preferred tab]
  (var win (visible-window buf preferred tab))
  (when (not win)
    (let [wins (api.nvim_tabpage_list_wins tab)
          candidates (icollect [_ candidate (ipairs wins) &into [preferred]] candidate)]
      (each [_ candidate (ipairs candidates) &until win]
        (when (replaceable-window? candidate anchor tab)
          (pcall api.nvim_win_set_buf candidate buf)
          (set win (visible-window buf candidate tab))))))
  (when (not win)
    (let [current (api.nvim_get_current_win)
          focused (pcall api.nvim_set_current_win anchor)]
      (when (and focused
                 (api.nvim_win_is_valid anchor)
                 (= (api.nvim_get_current_win) anchor)
                 (pcall vim.cmd "leftabove vsplit"))
        (let [candidate (api.nvim_get_current_win)]
          (pcall api.nvim_win_set_buf candidate buf)
          (set win (visible-window buf candidate tab))))
      (when (api.nvim_win_is_valid current)
        (pcall api.nvim_set_current_win current))))
  win)

(fn byte-column [buf line character]
  (let [text (. (api.nvim_buf_get_lines buf (- line 1) line false) 1)]
    (vim.str_byteindex text :utf-32 character true)))

(fn buffer [buf ?options]
  (vim.fn.bufload buf)
  (let [options (or ?options {})
        line-count (api.nvim_buf_line_count buf)
        line (or options.line 1)
        character (or options.character 0)]
    (assert (and (>= line 1) (<= line line-count))
            (.. "Present line is outside the buffer: " (tostring line)))
    (let [column (byte-column buf line character)
          current (api.nvim_get_current_win)
          current-tab (api.nvim_get_current_tabpage)
          requested-anchor options.anchor_win
          tab (if (normal-window? requested-anchor)
                  (api.nvim_win_get_tabpage requested-anchor)
                  current-tab)
          anchor (if (normal-window? requested-anchor tab)
                     requested-anchor
                     (if (normal-window? current tab)
                         current
                         (first-normal-window (api.nvim_tabpage_list_wins tab) tab)))]
      (tset vim.bo buf :buflisted true)
      (let [win (presentation-window buf anchor options.preferred_win tab)]
        (api.nvim_win_set_cursor win [line column])
        (api.nvim_win_call win #(vim.cmd "normal! zz"))
        {:buffer buf
         :window win
         :line line
         :column column}))))

(fn file [value ?options]
  (let [options (or ?options {})
        value (if (= (value:sub 1 1) "@") (value:sub 2) value)
        path (vim.fs.abspath value (and options.cwd {:cwd options.cwd}))
        stat (vim.uv.fs_stat path)]
    (assert (and stat (= stat.type :file)) (.. "Neovim cannot find file: " path))
    (let [buf (vim.fn.bufadd path)]
      (vim.fn.bufload buf)
      (assert (= (. vim.bo buf :buftype) "")
              "Neovim can only present normal file buffers")
      (when (not (. vim.bo buf :modified))
        (api.nvim_buf_call buf #(vim.cmd "silent checktime")))
      (let [result (buffer buf options)]
        (tset result :path path)
        result))))

{:buffer buffer
 :file file}
