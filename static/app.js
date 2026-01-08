const render_lst = [];

const iota = to => [...new Array(to).keys()];

/* why in the name of someone important are some pieces of this code written in cps */

function E(t, kv) {
  const el = document.createElement(t);
  for (let k of Object.keys(kv || {})) {
    el[k] = kv[k];
  }
  return el;
}

function make_dialog(title, cont) {
  const dlg = document.createElement('dialog');
  const art = document.createElement('article');
  if (title != undefined) {
    const h5 = document.createElement('h5');
    h5.innerHTML = title;
    dlg.appendChild(h5);
  }
  dlg.appendChild(art);
  cont(art, dlg);
  return dlg;
}

// `((article (class . "no-padding border"))
//   ((img (class . "responsive medium") (src . ,(str (car it))) (loading . "lazy")))
//   ((div (class . "absolute bottom left right padding bottom-shadow white-text"))
//    (nav
//     ((button (class . "circle transparent") (type . "button") (onClick . ,(str "image_selected('" (str (cadr it)) "')")))
//      (i "add"))))))

function make_img(icont, bcont, title) {
  const el = document.createElement('article');
  el.classList = "no-padding border";

  const img = document.createElement('img');
  img.classList = "responsive medium";
  img.loading = "lazy";
  icont(img);
  el.appendChild(img);

  const div = document.createElement('div');
  div.classList = "absolute bottom left right padding bottom-shadow white-text";
  el.appendChild(div);

  const nav = document.createElement('nav');
  div.appendChild(nav);

  // const what = document.createElement('div');
  // what.classList = 'max';
  // nav.appendChild(what);

  if (title != undefined)
    nav.appendChild(E('p', {innerHTML: title}))

  const button = document.createElement('button');
  button.classList = "circle transparent";
  button.type = "button";
  bcont(button);
  nav.appendChild(button);

  const i = document.createElement('i');
  i.innerHTML = "check";
  button.appendChild(i);

  return el;
}

function enlarge_image(id) {
  const dlg = make_dialog(undefined, (body, dlg) => {
    body.appendChild(make_img(i => {
      i.src = `/uploads/${id}`;
      i.addEventListener('click', () => dlg.close());
      i.classList = "responsive max";
    }, b => {
      b.addEventListener('click', () => dlg.close());
    }))
  });
  dlg.classList = "large";
  document.body.appendChild(E('div', {classList: 'overlay blur'}));
  document.body.appendChild(dlg);
  dlg.showModal();
}

function load_relation(id, table) {
  const target = document.getElementById(id);
  const refresh = () => {
    const dlg = make_dialog(`wybierz ${table}`, body => {
      fetch(`/api/${table}`).then(r => r.json()).then(its => {
        for (let it of its.reverse()) {
          const click = () => {
            target.value = it['id'];
            dlg.close();
          };
          const el = make_img(i => {
            i.src = `/uploads/${it['image']}`;
            i.addEventListener('click', click);
          }, b => {
            b.addEventListener('click', click);
          }, it['name']);
          body.appendChild(el);
        }
      });
    });
    document.body.appendChild(dlg);
    dlg.showModal();
  }
  refresh();
}

function load_upload_id(id) {
  const it = document.getElementById(id);
  const refresh = () => {
    const dlg = make_dialog('wybierz obrazek', body => {
      fetch('/api/uploads').then(r => r.json()).then(ups => {
        const uploader = make_image_uploader(() => {
          dlg.close();
          refresh();
        });

        body.appendChild(uploader);

        for (let up of ups.reverse()) {
          const el = make_img(i => {
            i.src = '/' + up['location']
          }, b => {
            b.addEventListener('click', () => {
              it.value = up['id'];
              dlg.close();
            });
          });
          body.appendChild(el);
        }
      })
    });
    document.body.appendChild(dlg);
    dlg.showModal();
  }
  refresh();
}

function put_image(file, cont) {
  fetch('/new/upload', {
    method: 'PUT',
    headers: {
      'Content-type': 'application/octet-stream'
    },
    body: file
  }).then(() => cont());
}

function make_image_uploader(after_upload) {
  const el = E('span', {classList: 'row'});
  const label = E('label', {classList: 'field border label'});
  const btn = E('button', {classList: 'chip circle'});
  btn.appendChild(E('i', {innerHTML: 'upload'}));
  const input = E('input', {type: "file",
                            accept: "image/png image/jpg image/gif image/heic image/heic-sequence",
                            required: "true"});
  input.addEventListener('change', () => {
    put_image(input.files[0], after_upload);
  });

  btn.appendChild(input);
  el.appendChild(label);
  label.appendChild(btn);

  const labelp = E('label', {classList: 'field border label'});
  const rb = E('button', {classList: 'chip circle'});
  rb.appendChild(E('i', {innerHTML: 'refresh'}));
  rb.addEventListener('click', after_upload);
  labelp.appendChild(rb);
  el.appendChild(labelp);
  return el;
}

function date_to_day(d) {
  const dp = new Date(d);
  dp.setHours(0);
  dp.setMinutes(0);
  dp.setSeconds(0);
  return dp.getTime();
}

function make_with_thing(route, or) {
  return function(cont) {
    fetch(route).then(d => d.json()).then(vs => {
      const cache = [];
      vs.forEach(v => {
        cache[v.id] = v;
      });
      cont(id => cache[id] || (or || {}));
    });
  }
}

// data = [[timestamp, yval, ...] ...]
function dated_to_xys(data, fn, arg) {
  const lst = data.sort((a, b) => a[0] < b[0]).map(it => {
    it.date = new Date(it[0] * 1000)
    return it;
  });
  const days = lst.reduce((l, v) => {
    const t = date_to_day(v.date);
    if (l[t] == undefined)
      l[t] = [];
    l[t].push(v);
    return l;
  }, {});
  const xs = Object.keys(days).map(x => parseInt(x)).sort((a, b) => a > b);
  // const ys = xs.map(v => days[v].reduce(rfn, base || 0));
  const ys = xs.map(v => days[v][fn](...arg));

  return [xs, ys, days]
}

const with_coffee_data = make_with_thing('/api/coffees', {name: "unknown"});
const with_method_data = make_with_thing('/api/methods', {name: "unknown"});

function do_render(it) {
  render_lst.push((coffee_data, method_data) => {
    let el = document.getElementById(`chrt-${it}`);
    const chart = echarts.init(el, 'beer');
    /* we get adequate data via el->x-data */
    const data = JSON.parse(el.getAttribute('x-data'));

    switch (it) {
    case 'coffee-rating-distribution': {
      const xs = iota(11);
      const ys = new Array(11).fill(0)
      Object.keys(data).forEach(k => {
        const n = parseInt(k);
        if (!isNaN(n))
          ys[n] = data[k];
      });
      console.log(data);
      console.log(ys);
      const opt = {
        xAxis: {
          data: xs,
        },
        yAxis: {},
        series: [{
          type: 'bar',
          data: ys,
        }],
      };
      chart.setOption(opt);
    } break;
    case 'coffee-style-ratios': {
      const opt = {
        series: [{
          type: 'pie',
          data: Object.keys(data).map(k => { return { value: data[k],
                                                     name: method_data(k).name,
                                                     label: {
                                                       textStyle: {
                                                         color: 'white'
                                                       }}}})
        }],
      };
      chart.setOption(opt);
    } break;
    case 'bean-history': {
      // ob = [[timestamp, dose, name], ...]
      const [xs, ys, days] = dated_to_xys(data, 'reduce', [(a, b) => a+b[1], 0])
      const opt = {
        xAxis: {
          data: xs,
          axisLabel: {
            formatter: (v) => {
              return new Date(parseInt(v)).toLocaleDateString()
            }
          }
        },
        yAxis: {},
        series: [{
          type: 'line',
          data: ys,
        }],
        tooltip: {
          renderMode: "html",
          trigger: "axis",
          formatter: (param) => {
            const d = days[param[0].axisValue];
            const el = E('div', {classList: "padding no-border"});
            const ul = E('ul');
            el.appendChild(ul);
            d.sort((a, b) => a[0] > b[0]).forEach(it => {
              ul.appendChild(E('li', {innerHTML: `${coffee_data(it[2]).name} (${it[1]}g)`}));
            })
            return el;
          },
        },
      };
      chart.setOption(opt);
    } break;
    case 'rating-history': {
      const [xs, ysp, days] = dated_to_xys(data, 'valueOf', [])
      console.log(ysp)
      const ys = ysp.map(y => y.reduce((a, b) => a+b[1], 0)/y.length);
      console.log(ys)
      const opt = {
        xAxis: {
          data: xs,
          axisLabel: {
            formatter: (v) => {
              return new Date(parseInt(v)).toLocaleDateString()
            }
          }
        },
        yAxis: [{
          min: 0,
          max: 10
        }],
        series: [{
          type: 'line',
          data: ys,
        }],
      };
      chart.setOption(opt);
    } break;
    default:
      console.error(`unknown do_render query: ${it}`);
      break;
    }
  })
}

window.onload = () => {
  const st = window.getComputedStyle(document.body);
  echarts.registerTheme(
    'beer', {
      color: [
        st.getPropertyValue('--primary'),
        st.getPropertyValue('--on-primary'),
        st.getPropertyValue('--on-primary-container'),
        st.getPropertyValue('--secondary'),
        st.getPropertyValue('--on-secondary'),
        st.getPropertyValue('--on-secondary-container'),
        st.getPropertyValue('--teritary'),
        st.getPropertyValue('--on-teritary'),
        st.getPropertyValue('--on-teritary-container'),
      ],
  });

  with_coffee_data(coffee_data => {
    with_method_data(method_data => {
      render_lst.forEach(f => f(coffee_data, method_data))
    })
  });
}
