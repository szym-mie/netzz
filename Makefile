CC = erlc
OUT = out
MODS = neten netpr netdv netpc netsw netrt tst
BEAMS = $(foreach f,$(MODS),$(OUT)/$(f).beam)

all: build

# pseudo-dumb target
$(OUT):
	mkdir $@

.PHONY: build
build: $(OUT) $(BEAMS)

$(BEAMS): $(OUT)/%.beam: src/%.erl
	$(CC) -o $(OUT) $<

.PHONY: run
run: build
	erl -pz $(OUT)

.PHONY: clean
clean:
	rm out/*.beam