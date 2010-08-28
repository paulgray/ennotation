SOURCES:=$(wildcard src/*.erl)
BEAMS:=$(patsubst src/%.erl, ebin/%.beam, $(SOURCES))

TEST_SOURCES:=$(wildcard test/*.erl)
TEST_BEAMS:=$(patsubst test/%.erl, test_ebin/%.beam, $(TEST_SOURCES))

all: $(BEAMS)

clean:
	rm -f ebin/*beam

ebin/%.beam: src/%.erl
	@echo ERLC $<
	@erlc -pa ebin/ -I include -o ebin $<

test: $(TEST_BEAMS)

test_ebin/%.beam: test/%.erl
	@echo ERLC $<
	@erlc -pa ebin/ -I include -o test_ebin $< 

test_ebin:
	@echo MKDIR test_ebin
	@mkdir -p test_ebin

