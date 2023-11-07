(when (and (os.getenv :SSH_TTY) (not (os.getenv :TMUX)))
  (with-module [osc52 :vim.clipboard.osc52]
    (set vim.g.clipboard {:name "OSC 52"
                          :copy {:+ osc52.copy :* osc52.copy}
                          :paste {:+ osc52.paste :* osc52.paste}})))
