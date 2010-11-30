capVowels :: Char -> Char
capVowels letter
	= case letter of
		'a' -> 'A'
		'e' -> 'E'
		'i' -> 'I'
		'o' -> 'O'
		'u' -> 'U'
		letter -> letter
		
deCapVowels :: Char -> Char
deCapVowels letter
	= case letter of
		'A' -> 'a'
		'E' -> 'e'
		'I' -> 'i'
		'O' -> 'o'
		'U' -> 'u'
		otherwise -> letter