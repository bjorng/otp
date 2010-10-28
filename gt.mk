
.PHONY: clean unit

ERLC ?= ./bin/erlc
EFLAGS += +debug_info +warn_exported_vars +warn_unused_vars +warn_unused_import #+warn_missing_spec

DIR=gt_working
SRC=$(wildcard $(DIR)/*.erl)
BIN=$(patsubst %.erl, %.beam, $(SRC))

all: $(BIN)

$(DIR)/%.beam: $(DIR)/%.erl
	$(ERLC) $(EFLAGS) -o $(dir $@) $<

%.beam: %.erl
	$(ERLC) $(EFLAGS) $<

unit: $(BIN)
	./unit $(BIN)

clean:
	$(RM) $(BIN)
