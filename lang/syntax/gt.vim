" Vim syntax file
" Language: gt
" Add this to your vimrc "au bufreadpre,bufnewfile *.gt set ft=gt" 
" and put this file in ~/.vim/syntax/
" Author: Stanisław Bitner <https://github.com/Rentib>

if exists("b:current_syntax")
    finish
endif

syn case match

syn region gtComment start="/\*" end="\*/" contains=gtTodo
syn match gtComment "//.*$" contains=gtTodo
syn match gtComment "^\s*#.*$" contains=gtTodo

syn region gtString		start=+"+  skip=+\\\\\|\\"+  end=+"+
syn region gtString		start=+'+  skip=+\\\\\|\\'+  end=+'+
syn region gtString		start=+`+  skip=+\\\\\|\\`+  end=+`+

syn match gtNumber "[-+]\=\(\<\d[[:digit:]_]*L\=\>\|0[xX]\x[[:xdigit:]_]*\>\)"
syn match gtBoolean "\<true\|false\>"

syn match gtFunction "\<[a-zA-Z][a-zA-Z0-9_]*\s*("me=e-1

syn keyword gtConditional if else
syn keyword gtRepeat while for

syn match gtOperator "+"
syn match gtOperator "-"
syn match gtOperator "*"
syn match gtOperator "\/\(.*[\/\*]\)\@!"
syn match gtOperator "%"
syn match gtOperator "!"
syn match gtOperator "&&"
syn match gtOperator "||"
syn match gtOperator "=>"
syn match gtOperator ">"
syn match gtOperator ">="
syn match gtOperator "<"
syn match gtOperator "<="
syn match gtOperator "=="
syn match gtOperator "!="
syn match gtOperator "="
syn match gtOperator "&"

syn keyword gtKeyword return break continue var fn const

syn keyword gtType int bool char void

syn match gtIgnore ";"
syn match gtIgnore ","
syn match gtIgnore ":"
syn match gtIgnore "->"
syn match gtIgnore "\s"

command -nargs=+ HiLink hi def link <args>

HiLink gtComment   Comment
HiLink gtString    String
HiLink gtNumber    Number
HiLink gtBoolean   Boolean
HiLink gtConditional   Conditional
HiLink gtRepeat    Repeat
HiLink gtOperator  Operator
HiLink gtKeyword   Statement
HiLink gtType      Type
HiLink gtFunction  Function
HiLink gtIgnore    Ignore
HiLink gtTodo Todo

delcommand HiLink

let b:current_syntax = "gt"
