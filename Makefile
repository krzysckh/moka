OL=ol
OFLAGS=-i third-party/robusta

CFLAGS=-O2 -I/usr/local/include
LDFLAGS=-L/usr/local/lib -lsqlite3 -lm -lpthread

.SUFFIXES: .scm .c

all: moka static/beercss
run: static/beercss
	$(OL) $(OFLAGS) -r moka.scm
.scm.c:
	$(OL) $(OFLAGS) -O1 -x c -o $@ $<
moka: moka.c
	$(CC) $(CFLAGS) -static -o $@ $< $(LDFLAGS)
clean:
	rm -f moka.c moka
static/beercss:
	wget -O static/beercss.tgz "https://pub.krzysckh.org/beercss.tgz"
	( cd static && tar xvzf beercss.tgz )
auto-install: all
	useradd -k /var/empty -L daemon -d /var/moka -m -s /sbin/nologin _moka || echo "user already exists. that's okay"
	cp -v moka /var/moka/moka
	mkdir -p /var/moka/static/uploads
	cp -v static/app.js /var/moka/static/app.js
	[ -d /var/moka/static/beercss ] || cp -vr static/beercss /var/moka/static/beercss
	chown -R _moka:_moka /var/moka/
	cp -v moka.rc /etc/rc.d/moka
	chmod +x /etc/rc.d/moka
