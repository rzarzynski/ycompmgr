TRGT 	= ycompmgr
CC		= gcc
LIBS 	= -lm -lcairo -lX11 -lXcomposite -lXfixes -lXdamage -lXext
CFLAGS 	= -std=gnu1x -O2 -fplan9-extensions

OBJS = $(patsubst %.c, %.o, $(wildcard *.c))
HDRS = $(wildcard *.h)

default: $(TRGT)
all: default

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c $< -o $@

$(TRGT): $(OBJS)
	$(CC) $(OBJS) $(LIBS) -o $@

clean:
	-rm -f *.o
	-rm -f $(TRGT)

.PHONY: default all clean
