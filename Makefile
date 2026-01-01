OL=ol
OFLAGS=-i third-party/robusta

CFLAGS=-O2 -I/usr/local/include
LDFLAGS=-L/usr/local/lib -lsqlite3 -lm -lpthread

STATIC_DOWNLOADS=static/beercss static/echarts.min.js

.SUFFIXES: .scm .c

all: moka $(STATIC_DOWNLOADS)
run: $(STATIC_DOWNLOADS)
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
static/echarts.min.js:
	wget -O static/echarts.min.js "https://pub.krzysckh.org/echarts6.min.js"
	( cd static && tar xvzf beercss.tgz )
auto-install: all
	useradd -k /var/empty -L daemon -d /var/moka -m -s /sbin/nologin _moka || echo "user already exists. that's okay"
	cp -v moka /var/moka/moka
	mkdir -p /var/moka/static/uploads
	cp -v static/*.js /var/moka/static/
	[ -d /var/moka/static/beercss ] || cp -vr static/beercss /var/moka/static/beercss
	chown -R _moka:_moka /var/moka/
	cp -v moka.rc /etc/rc.d/moka
	chmod +x /etc/rc.d/moka
