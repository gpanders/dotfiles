(with-module [pick :pick]
  (pick.setup {:source {:show pick.default_show}
               :mappings {:toggle_info "<C-/>"}})
  (keymap :n "<Space>f" pick.builtin.files)
  (keymap :n "<Space>b" pick.builtin.buffers)
  (keymap :n "<Space>/" pick.builtin.grep_live))
