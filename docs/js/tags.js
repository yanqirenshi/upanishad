riot.tag2('app', '<gloval-header></gloval-header> <app-tab data="{STORE.state().pages}"></app-tab> <div id="tab-contents"> <readme ref="readme"></readme> <page-1 ref="page-1"></page-1> <page-2 ref="page-2"></page-2> <page-3 ref="page-3"></page-3> </div> <gloval-footer></gloval-footer>', '', '', function(opts) {
     this.displayTabContents = function () {
         let pages = this.refs;
         let data = STORE.state().pages;
         for (var code in data) {
             let page_data = data[code];
             let page_tag = pages[code];
             if (page_data.display)

                 riot.mount('#tab-contents', code, page_data);
             else
                 if(page_tag) pages[code].unmount();
         }

         this.update();
     };

     STORE.subscribe(function (action) { this.displayTabContents(); }.bind(this));
     this.on('mount', function () { this.displayTabContents(); }.bind(this));
});

riot.tag2('app-tab', '<div class="tabs"> <ul> <li each="{page , code in opts.data}" class="{page.display ? \'is-active\' : \'\'}"> <a code="{code}" onclick="{onClickTab}"> {page.label} </a> </li> </ul> </div>', '', '', function(opts) {
     this.onClickTab = function (e) {
         STORE.dispatch(
             ACTIONS.movePage(e.target.getAttribute('code'))
         );
     };
});

riot.tag2('gloval-footer', '<footer class="footer"> <div class="container"> <div class="content has-text-centered"> </div> </div> </footer>', '', '', function(opts) {
});

riot.tag2('gloval-header', '<section class="hero"> <div class="hero-body"> <div class="container"> <h1 class="title">Upanishad</h1> <h2 class="subtitle">Object Prevalence の Common Lips の実装です。</h2> </div> </div> </section>', 'gloval-header section.hero { background-color: #f5f5f5; }', '', function(opts) {
});

riot.tag2('package-upanishad', '', '', '', function(opts) {
     this.symbols = [
         '%id',
         'meme',
         'make-pool',
         'stop',
         'tx-create-meme',
         'tx-delete-meme',
         'find-meme',
         'get-meme'
     ];
});

riot.tag2('package-upanishad_memes', '', '', '', function(opts) {
     this.symbols = [
         'memes',
         'meme-class',
         'meme-list',
         '%id-index',
         'get-meme',
         'add-meme',
         'make-memes',
         'remove-meme'
     ];
});

riot.tag2('packages', '<package-upanishad> </package-upanishad>', '', '', function(opts) {
});

riot.tag2('page-1', '<section class="section"> <div class="container"> <h1 class="title">Page-1</h1> <h2 class="subtitle"> </h2> </div> </section>', '', '', function(opts) {
});

riot.tag2('page-2', '<section class="section"> <div class="container"> <h1 class="title">Page-2</h1> <h2 class="subtitle"> </h2> </div> </section>', '', '', function(opts) {
});

riot.tag2('page-3', '<section class="section"> <div class="container"> <h1 class="title">Page-3</h1> <h2 class="subtitle"> </h2> </div> </section>', '', '', function(opts) {
});

riot.tag2('readme-author', '<section class="section"> <div class="container"> <h1 class="title">Author</h1> </div> </section>', '', '', function(opts) {
});

riot.tag2('readme-description', '<section class="section"> <div class="container"> <h1 class="title">Description</h1> <div class="contents"> </div> </div> </section>', '', '', function(opts) {
});

riot.tag2('readme-installation', '<section class="section"> <div class="container"> <h1 class="title">Installation</h1> <div class="contents"> <p class="subtitle">飾りっけなし。</p> <pre><code>\n(ql:quickload :upanishad)\n(ql:quickload :upanishad-test)\n(upanishad-test:run!)\n                </code></pre> </div> </div> </section>', '', '', function(opts) {
});

riot.tag2('readme-license', '<section class="section"> <div class="container"> <h1 class="title">License</h1> </div> </section>', '', '', function(opts) {
});

riot.tag2('readme-requirement', '<section class="section"> <div class="container"> <h1 class="title">Requirement</h1> <p class="subtitle">ほぼ、まじりっけなし。</p> <table class="table is-bordered is-striped is-narrow is-fullwidth"> <thead> <tr> <th>libraly</th> <th>description</th> </tr> </thead> <tbody> <tr> <td><a href="https://common-lisp.net/project/alexandria/">alexandria</a></td> <td>あれ？ 使ってる？</td> </tr> <tr> <td><a href="#">cl-ppcre</a></td> <td></td> </tr> <tr> <td><a href="https://common-lisp.net/project/s-xml/">s-xml</a></td> <td>シリアライズ(XML)する時に利用。</td> </tr> <tr> <td><a href="https://github.com/svenvc/s-sysdeps">s-sysdeps</a></td> <td>マスタとスレーブでソケット通信するのに利用。</td> </tr> <tr> <td><a href="#">cl-fad</a></td> <td>ファイル/ディレクトリ操作</td> </tr> <tr> <td><a href="">s-serialization</a></td> <td>シリアライズの部分を自分で切り出したやつ。</td> </tr> </tbody> </table> </div> </section>', '', '', function(opts) {
});

riot.tag2('readme-usage', '<section class="section"> <div class="container"> <h1 class="title">Usage</h1> <div class="contents"> <p> データを保管する(snapshotと言う)ためのディレクトリを作成したら。。。 <pre>\n                        <code>\ncd ~/\nmkdir ~/up\n                        </code>\n                    </pre> </p> <p> あとは lisp でこんな感じ。 </p> <pre>\n                    <code>\n;; pool の開始 ※pool は db に脳内変換すると違和感がなくなります。\n(defvar *pool* (make-pool "~/up/"))\n(execute-transaction (tx-create-id-counter *pool*))\n    ：\n  ※いろいろな処理。 準備中\n    ：\n;; snapshot の実行。  ※snapshot は commit に脳内変換。\n(snapshot *test-system*)\n    ：\n  ※いろいろな処理。 準備中\n    ：\n;; restore の実行  ※restore は rollback に脳内変換。\n(restore *bank-system*)\n\n;; pool の停止\n(close-open-streams *test-system*)\n                    </code>\n                </pre> </div> </div> </section>', '', '', function(opts) {
});

riot.tag2('readme', '<readme-description></readme-description> <readme-installation> </readme-installation> <readme-usage> </readme-usage> <readme-requirement> </readme-requirement> <readme-author> </readme-author> <readme-license> </readme-license>', '', '', function(opts) {
});
