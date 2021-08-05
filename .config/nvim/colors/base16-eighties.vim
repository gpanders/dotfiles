if v:vim_did_enter
    lua require("colors.base16-eighties")
else
    autocmd VimEnter * ++nested lua require("colors.base16-eighties")
end
