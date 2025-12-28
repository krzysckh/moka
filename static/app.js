let image_selected = _ => true;

function E(t, kv) {
  const el = document.createElement(t);
  for (let k of Object.keys(kv || {})) {
    el[k] = kv[k];
  }
  return el;
}

function make_dialog(title, cont) {
  const dlg = document.createElement('dialog');
  const h5 = document.createElement('h5');
  const art = document.createElement('article');
  h5.innerHTML = title;
  dlg.appendChild(h5);
  dlg.appendChild(art);
  cont(art);
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
