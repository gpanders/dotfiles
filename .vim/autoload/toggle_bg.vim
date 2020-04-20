" See :h xterm-true-color
if !has('nvim')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

function! toggle_bg#toggle()
    if exists('g:toggle_bg_dark') && exists('g:toggle_bg_light')
        if !&termguicolors
            set termguicolors
        endif

        if g:colors_name ==# g:toggle_bg_dark
            execute 'colorscheme' g:toggle_bg_light
        else
            execute 'colorscheme' g:toggle_bg_dark
        endif
    else
        " If no colorschemes are defined then just toggle the bg setting
        if &background ==# 'dark'
            set background=light
        else
            set background=dark
        endif
    endif
endfunction

