let image_selected = _ => true;

function load_upload_id(id) {
  const dlg = document.getElementById('choose_upload_dialog');
  const it = document.getElementById(id);
  console.log(it)
  dlg.show();
  image_selected = (img) => {
    console.log(img)
    it.value = img;
    dlg.close();
  }
}
