@:header(natins)
#:if definitions > 0
@:integer(numrates, range)
@:integer(c4nrates, range)
@:double(c2floor, amount)
@:double(c2rate, amount)
@:double(ceiling, amount)
@:doublearray(rates, rate, MaxNatInsBands, numrates)
@:doublearray(bands, amount, MaxNatInsBands, numrates)
@:doublearray(c4rates, rate, MaxNatInsC4Bands, c4nrates)
@:doublearray(c4bands, amount, MaxNatInsC4Bands, c4nrates)
#:endif
@:footer(natins)
