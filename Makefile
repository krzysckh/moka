OL=ol
OFLAGS=-i third-party/robusta

.SUFFIXES: .scm .c

all: moka
run:
	$(OL) $(OFLAGS) -r moka.scm
.scm.c:
	$(OL) $(OFLAGS) -O1 -x c -o $@ $<
moka: moka.c
	$(CC) -O2 -static -o $@ $< -lsqlite3 -lm
clean:
	rm -f moka.c moka
