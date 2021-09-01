setlocal errorformat=%C%[%^^]%#,%E%>Parse\ error\ in\ %f:%l,%E%>Compile\ error\ in\ %f:%l,%-Z%p^%.%#,%C%m,%-G*\ %.%#
setlocal makeprg=fennel\ --compile\ %
