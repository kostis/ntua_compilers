"=============================================================================
" File:          dana.vim
" Description:   Vim syntax file for Dana Language 
"=============================================================================

if exists("b:current_syntax")
  finish
endif


syn case ignore
syn sync lines=250

" Keywords
syn keyword danaBoolean	    true false
syn keyword danaConditional	if elif else 
syn keyword danaOperator	and not or 
syn keyword danaLoop	    loop
syn keyword danaStatement	begin end skip exit return break continue 
syn keyword danaType		int byte ref
syn keyword danaDefinition def decl var

" Operators
syn match   danaSymbolOperator  /+\|=\|−\|<>\|*\|<\|\/\|>\|%\|<=\|!\|>=\|&\|||/
syn match   danaSymbolOperator  ":="
syn keyword danaOperator	as is

" Delimiters
syn match danaDelimiter /[()\[\],]\|:\ze[^=]/

" Match any identifier except for statement keywords
syn match   danaIdentifier		"\<[a-zA-Z][a-zA-Z0-9_]*\>"


" Built-in library functions
syn keyword danaFunction writeInteger writeByte writeChar writeString readInteger readByte readChar readString extend shrink strlen strcmp strcpy strcat

" Numbers
syn match danaNumber /\<\d\+\>/

" Character literal: '\([^'"\\]\|\\\(n\|t\|r\|0\|\\\|'\|"\|x[0-9a-fA-F][0-9a-fA-F]\)\)'
syn match danaChar /'\([^'"\\]\|\\\(n\|t\|r\|0\|\\\|'\|"\|x[0-9a-fA-F][0-9a-fA-F]\)\)'/

" String literal: "([^'"\n\\]\|ESC)*"
syn match danaString /"\([^'"\n\\]\|\\\(n\|t\|r\|0\|\\\|'\|"\|x[0-9a-fA-F][0-9a-fA-F]\)\)*"/

" Comments
syn match danaComment /#.*$/
syn region danaComment start="(\*" end="\*)"

hi def link danaBoolean		    Boolean
hi def link danaChar            Character
hi def link danaComment		    Comment
hi def link danaConditional		Conditional
hi def link danaDefinition    Statement
hi def link danaDelimiter     Delimiter
hi def link danaFuncCall Function
hi def link danaFunction		Function
hi def link danaNumber		    Number
hi def link danaOperator		Operator
hi def link danaLoop		    Repeat
hi def link danaStatement		Statement
hi def link danaString		    String
hi def link danaSymbolOperator	danaOperator
hi def link danaType			Type

let b:current_syntax = "dana"
