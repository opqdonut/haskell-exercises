WEEKS=W1 W2 W3 W4 W5 W6 W7
OUTS=$(patsubst %,%.hs,$(WEEKS))
SOLS=$(patsubst %,%Sol.hs,$(WEEKS))

all: $(OUTS)

solutions: $(SOLS)

$(OUTS): %.hs: templ/%B.hs
	@echo "=> $@"
	@./Impl/strip 2 < $< > $@

$(SOLS): %Sol.hs: templ/%B.hs
	@echo "=> $@"
	@./Impl/strip 1 < $< > $@
