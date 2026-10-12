if exists("b:current_syntax")
  finish
endif

syntax match lamSymbol1 /\^\|λ\|\./
syntax match lamSymbol2 /<-\|{\|}/
syntax match lamSymbol3 /[\(\)]/
syntax match lamSymbol4 /[=;]/

syntax match lamIdentifier /[^\.<\(\)={}\^λ; \t\n\r]\+/

syntax keyword lamKeyword cps let in

syntax match lamNumber /\d\+/

syntax region lamComment start=/{-/ end=/-}/

syntax region lamString start=/"/ end=/"/ skip=/\\/


"highlight link lamSymbol1 Special
"highlight link lamSymbol2 Special
"highlight link lamSymbol3 Delimiter
"highlight link lamSymbol4 Delimiter

highlight lamSymbol1 cterm=bold ctermfg=blue
highlight lamSymbol2 cterm=bold ctermfg=blue
highlight lamSymbol3 cterm=bold ctermfg=blue
highlight lamSymbol4 cterm=bold ctermfg=blue

highlight lamIdentifier ctermfg=NONE
"highlight link lamIdentifier Function

highlight link lamKeyword Keyword
highlight link lamComment Comment
highlight link lamString String
highlight link lamNumber Number

let b:current_syntax = "lam"
