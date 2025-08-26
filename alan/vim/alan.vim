" VIM syntax highlighting for Alan
" Copyright (C) 2024  Andreas Stamos
" 
" This program is free software: you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
" 
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
" 
" You should have received a copy of the GNU General Public License
" along with this program.  If not, see <http://www.gnu.org/licenses/>.
"
"
"INSTRUCTIONS: copy this file to ~/.vim/syntax and write 'au BufRead,BufNewFile *.alan set filetype=alan' in ~/.vimrc


if exists("b:current_syntax")
    finish
endif

syn keyword alanStatement return while if else 
hi link alanStatement Statement

syn keyword alanKeyword byte int proc reference
hi link alanKeyword Keyword

syn match alanString "\".*\""
hi link alanString String

syn match alanChar "'.'"
hi link alanChar Character

syn match alanNumber "\d\+"
hi link alanNumber Number

syn keyword alanBoolean true false
hi link alanBoolean Boolean

syn keyword alanConditional if else
hi link alanConditional Conditional

syn keyword alanRepeat while
hi link alanRepeat Repeat

syn keyword alanType int byte proc
hi link alanType Type

syn keyword alanStorageClass reference

hi link alanStorageClass StorageClass

syn match alanOperator "==\|!=\|<=\|>=\|<\|>\|+\|-\|*\|/\|%\| and \| or \|&\||"
hi link alanOperator Operator

syn match alanComment "--.*$"
hi link alanComment Comment

syn region alanBlockComment start="(\*" end="\*)"
hi link alanBlockComment Comment

let b:current_syntax = "alan"

