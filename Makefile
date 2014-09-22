WEEKS=W1 W2
OUTS=$(patsubst %,%.hs,$(WEEKS))
SOLS=$(patsubst %,%Sol.hs,$(WEEKS))

all: $(OUTS)

$(OUTS): %.hs: templ/%B.hs
	@echo "=> $@"
	@./Impl/strip 2 < $< > $@

$(SOLS): %Sol.hs: templ/%B.hs
	@echo "=> $@"
	@./Impl/strip 1 < $< > $@
