function! s:load()
    if !exists('*FugitiveGitDir') || !empty(FugitiveGitDir())
        lua require("gitsigns").setup()
        autocmd! my_gitsigns
        augroup! my_gitsigns
    end
endfunction

augroup my_gitsigns
    autocmd! BufNewFile,BufRead * call s:load()
augroup END
