(keymap :n "=" #((. (require "packer.display") "toggle_info")) {:buffer true})
(keymap :n "<CR>" #((. (require "packer.display") "diff")) {:buffer true})
