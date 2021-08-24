setlocal errorformat=%EParse\ error\ in\ %f:%l,%ECompile\ error\ in\ %f:%l,%C%m,%-C%p^,%-G%.%#
setlocal makeprg=fennel\ --compile\ %
