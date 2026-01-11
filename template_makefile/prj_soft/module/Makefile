EX_DIRS := $(wildcard ex_*)

.PHONY: all clean $(EX_DIRS)

all: $(EX_DIRS)

$(EX_DIRS):
	@$(MAKE) -C $@

clean:
	@for dir in $(EX_DIRS); do $(MAKE) -C $$dir clean; done
