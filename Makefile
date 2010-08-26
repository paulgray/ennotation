SOURCES:=$(wildcard src/*.erl)
BEAMS:=$(patsubst src/%.erl, ebin/%.beam, $(SOURCES))

all: $(BEAMS)

clean:
	rm -f ebin/*beam

ebin/%.beam: src/%.erl
	@echo ERLC $<
	@erlc -I include -o ebin $<
