setlocal formatoptions+=wan
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:
setlocal spell

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fo< flp< spell<'
