<app>
    <gloval-header></gloval-header>

    <app-tab data={STORE.state().pages}></app-tab>

    <div id="tab-contents">
        <readme ref="readme"></readme>
        <page-1 ref="page-1"></page-1>
        <page-2 ref="page-2"></page-2>
        <page-3 ref="page-3"></page-3>
    </div>

    <gloval-footer></gloval-footer>

    <script>
     this.displayTabContents = function () {
         let pages = this.refs;
         let data = STORE.state().pages;
         for (var code in data) {
             let page_data = data[code];
             let page_tag = pages[code];
             if (page_data.display)
                 // TODO: すでにマウントされている場合はマウントする必要ないな
                 riot.mount('#tab-contents', code, page_data);
             else
                 if(page_tag) pages[code].unmount();
         }
         // TODO: マウントに変更があった場合のみ update した方が良いね。
         this.update();
     };

     STORE.subscribe(function (action) { this.displayTabContents(); }.bind(this));
     this.on('mount', function () { this.displayTabContents(); }.bind(this));

     ACTIONS.loadDataPage1();
    </script>
</app>
