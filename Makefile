SOURCES:=$(wildcard src/*.erl)
BEAMS:=$(patsubst src/%.erl, ebin/%.beam, $(SOURCES))

TEST_SOURCES:=$(wildcard test/*.erl)
TEST_BEAMS:=$(patsubst test/%.erl, test_ebin/%.beam, $(TEST_SOURCES))

all: $(BEAMS)

clean:
	@echo RM ebin/*ebin test_ebin/*ebin
	@rm -f ebin/*beam test_ebin/*ebin

ebin/%.beam: src/%.erl
	@echo ERLC $<
	@erlc -pa ebin/ -I include -o ebin $<

test: all $(TEST_BEAMS)

test_ebin/%.beam: test/%.erl
	@echo ERLC $<
	@erlc -pa ebin/ -I include -o test_ebin $< 

test_ebin:
	@echo MKDIR test_ebin
	@mkdir -p test_ebin

