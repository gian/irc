all:
	smlpeg irc-parser.peg
	echo "CM.make \"irc.cm\";" | sml

