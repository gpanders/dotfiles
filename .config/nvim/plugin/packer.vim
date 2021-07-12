if exists(':PackerUpdate') == 2
    finish
endif

command PackerUpdate exec 'lua require("plugins")' | PackerUpdate
