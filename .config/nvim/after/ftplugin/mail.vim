setlocal formatoptions+=wan
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:
setlocal nonumber
setlocal spell

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fo< flp< nu< spell<'
