setlocal formatexpr=ft#csv#format()

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fex<'
