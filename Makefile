TRGT    = ycompmgr
CC      = gcc
LDFLAGS = -lm -lcairo -lX11 -lXcomposite -lXfixes -lXdamage -lXext
CFLAGS  = -std=gnu1x -O2 -fplan9-extensions
PREFIX  = ~/local/$(TRGT)

OBJS = $(patsubst %.c, %.o, $(wildcard *.c))
HDRS = $(wildcard *.h)

all: $(TRGT)

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c $< -o $@

$(TRGT): $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) -o $@

install: all
	install -m 0755 -D $(TRGT) $(PREFIX)/bin/$(TRGT)

uninstall:
	$(RM) $(PREFIX)/bin/$(TRGT)

clean:
	$(RM) *.o
	$(RM) $(TRGT)

.PHONY: all install uninstall clean
