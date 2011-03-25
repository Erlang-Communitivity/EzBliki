
Nonterminals 
	Wikitext
	BoldText
	ItalicText
	Paragraph.

Terminals
	'*'
	'atom'
	'var'
	.

Rootsymbol
	Wikitext.

Wikitext -> Wikitext atom : '$1' ++ $2. 
Wikitext -> Wikitext var : '$1' ++ $2. 
Wikitext -> atom : [{'$1'}].
Wikitext -> var : [{'$1'}].
Wikitext -> Wikitext BoldText : '$1' ++ '$2'.

BoldText -> '*' '*' Wikitext '*' '*' : [{boldstart}] ++ '$3' ++ [{boldend}].	
